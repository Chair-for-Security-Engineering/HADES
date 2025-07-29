import spinal.core._
import spinal.core.internals._
import Gadgets._

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal._
import scala.util.Random._

/**
 * Class for Design Space Exploration
 */
class DesignSpaceExploration() extends GlobalConfigUser {

	type T = Template
	type options = mutable.Map[T, mutable.Map[String, Configurable]]

	//Either use the areaConfig for the current gadget, or the Default areaConfig if no gadget is specified
	var areaConfig =
		if (globalConfig.gadgetConfig.gadget != null) globalConfig.areaConfig(globalConfig.gadgetConfig.gadget.getName())
		else globalConfig.areaConfig("Default")

	var totalConfigurations = 0
	var foundConfigurations = 0

	val visitedTemplates = mutable.Map[String, T]()

	var rankedDesigns = mutable.Map[OptimizationTarget, ArrayBuffer[(BigDecimal, C)]]()
	globalConfig.optimizationTargets.foreach(opt => rankedDesigns += ((opt._1, ArrayBuffer[(BigDecimal, C)]())))

	val designs = mutable.Map[C, mutable.Map[OptimizationTarget, BigDecimal]]()

	val sortedDesigns = mutable.Map[OptimizationTarget, ArrayBuffer[(BigDecimal, C)]]()
	globalConfig.optimizationTargets.foreach(opt => sortedDesigns += ((opt._1, ArrayBuffer[(BigDecimal, C)]())))
	def clearSortedDesigns(): Unit = {
		sortedDesigns.foreach(sd => sd._2.clear())
	}

	var thresholdDesigns = ArrayBuffer[(C, mutable.Map[OptimizationTarget, BigDecimal])]()

	val exploredTemplates = ArrayBuffer[String]()

	val wellOrderedDesignAspects = mutable.Map[String, (String, Configurable)]()

	val successfulInterpolations = mutable.Map[(Int, Int), ArrayBuffer[(Configuration, BigDecimal)]]()

	val allDesignAspects = mutable.Map[String, Configurable]()

	//Set dryRun to true for the exploration
	GlobalConfig.setDryRun(true)

	/**
	 * Explore all designs with all gadgets
	 * @param c Toplevel template
	 * @param gen
	 */
	def exploreGadgets(c : T)(gen: => T): Unit = {

		AllGadgets.gadgets.foreach(g => {

			SpinalProgress(s"Currently exploring designs for Gadget ${g.getName()}")

			//If the gadget is not in the AreaConfig, raise a SpinalError
			if (!globalConfig.areaConfig.contains(g.getName())) SpinalError(s"No Area information for gadget ${g.getName()} found in the specified JSON file!")

			//Update the GlobalConfig
			g.updateConfig()

			//New Environment
			val environment = new Environment(GlobalConfig.config)(gen)
			environment.initialize()
			environment.top.updateConfig()

			//Explore all possible designs with the current gadget
			if (globalConfig.localOptimizationDepth == 0) environment.explorer.exploreAllDesigns(environment.top)
			else environment.explorer.exploreWithLocalOptimizations(environment.top)

			//Add the found and total configurations counter
			foundConfigurations += environment.explorer.foundConfigurations
			totalConfigurations += environment.explorer.totalConfigurations

			//Include the possible configurations
			designs ++= environment.explorer.designs

			globalConfig.optimizationTargets.foreach(opt => {
				rankedDesigns(opt._1) ++= environment.explorer.rankedDesigns(opt._1)
			})

			thresholdDesigns ++= environment.explorer.thresholdDesigns
		})
	}

	def localSearch(top : T, initSampleSize : Int, runs : Int): Unit = {
		SpinalProgress(s"Performing a local search with ${initSampleSize} initial samples and a total of ${runs} runs")

		val initialSampleSet = ArrayBuffer[(Configuration, BigDecimal)]()
		val orderedDesignAspects = mutable.Map[String, Configurable]()
		var bestConfig : (C, BigDecimal) = (null, BigDecimal(0))
		var currentGadget : MaskedGadget = HPC3Gadget

		currentGadget.updateConfig()
		areaConfig = globalConfig.areaConfig(globalConfig.gadgetConfig.gadget.getName())

		//Configuration extraction
		val configs = getDesignAspectsRecursively(top, Int.MaxValue)
		assert(initSampleSize <= configs.size, s"Initial sample size is larger than the number of possible configurations")

		for (runIndex <- 0 until runs) {
			SpinalProgress(s"Run ${runIndex+1} out of ${runs}")
			//Shuffle the configurations
			val shuffledConfigs = shuffle(configs)

			var numInstantiations = 0
			var configCounter = 0

			//Initial Sampling: Evaluate initSampleSize many configurations from the shuffled array and add them to the initial sample set
			SpinalProgress(s"Starting to sample and evaluate ${initSampleSize} (valid) configurations")
			while (numInstantiations < initSampleSize) {
				val cfg = shuffledConfigs(configCounter)
				configureOptionsRecursively(top, cfg)
				if (top.instantiateTemplate()) {
					val configuration = (cfg, GlobalConfig.config.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder)
					processResults(top, configuration)
					numInstantiations += 1
					initialSampleSet += ((cfg, designs(configuration)(ATP)))
				}
				top.removeNewTemplates()
				configCounter += 1
			}
			SpinalProgress(s"Successfully sampled and evaluated ${numInstantiations} out of ${configCounter} tried configurations")

			//Ordering of parameters
			SpinalProgress(s"Ordering the design aspects")
			allDesignAspects.foreach(da => if (!da._2.isInstanceOf[IntegerList]) {
				val groupedConfigs = mutable.Map[Any, ArrayBuffer[(Configuration, BigDecimal)]]()
				da._2.parameters.foreach(param => groupedConfigs += (param -> ArrayBuffer[(Configuration, BigDecimal)]()))

				val index = da._1.split("#")
				//For each design aspect, group the configurations by the chosen option from the design aspect
				initialSampleSet.foreach(sample => {
					if (sample._1.contains(index(0))) {
						if ((sample._1(index(0)).contains(index(1)))) {
							groupedConfigs((sample._1(index(0))) (index(1))) += sample
						}
					}
				})

				//Sort the grouped parameters according to their performance, from best to worst
				var sortedParams = ArrayBuffer[(Any, BigDecimal)]()
				groupedConfigs.foreach(param => {
					val sorted = param._2.sortWith(_._2 < _._2)
					val min = if (sorted.nonEmpty) sorted.head._2 else BigDecimal(-1)
					sortedParams += ((param._1, min))
				})

				sortedParams = sortedParams.sortWith(_._2 < _._2)
				//Move parameters with performance -1 (=> never sampled) to the back of the arrays
				while (sortedParams.head._2 == BigDecimal(-1)) {
					val head = sortedParams.head
					val tail = sortedParams.tail
					sortedParams = tail
					sortedParams += head
				}


				val configurables = ArrayBuffer[Any]()
				sortedParams.foreach(p => configurables += p._1)
				val conf = new Configurable {
					parameters = configurables.toList
				}
				orderedDesignAspects += (da._1 -> conf)
			} else {
				orderedDesignAspects += da
			})

			//Instantiate first config as the best config from the sampling
			SpinalProgress(s"Beginning optimization")
			var currentSolution = initialSampleSet.sortWith(_._2 < _._2).head
			var bestATP = designs(currentSolution._1, GlobalConfig.config.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder)(ATP)
			var change = true

			//Loop over all design aspects as long as a change happened in the previous iteration
			while (change) {
				change = false

				orderedDesignAspects.foreach(da => {
					val template = da._1.split("#")(0)
					val aspect = da._1.split("#")(1)
					//Get index of current aspect option
					val currentIndex = da._2.parameters.indexOf((currentSolution._1(template)) (aspect))
					//Go one position to the left
					if (currentIndex > 0) {
						val newSolution = replaceDesignAspect(currentSolution._1, da._1, da._2.parameters(currentIndex - 1))
						configureOptionsRecursively(top, newSolution)
						if (top.instantiateTemplate()) {
							val configuration = (newSolution, GlobalConfig.config.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder)
							processResults(top, configuration)
							val newBestATP = designs(configuration)(ATP)
							//Check if performance of new config is better than that of the old one. If yes, update config and best performance
							if (newBestATP < bestATP) {
								SpinalProgress(s"Changed ${da._1} from ${da._2.parameters(currentIndex)} to ${da._2.parameters(currentIndex - 1)}")
								currentSolution = (newSolution, newBestATP)
								change = true
								bestATP = newBestATP
							}
						}
						top.removeNewTemplates()
					}
					//Go one position to the right
					if (currentIndex < da._2.parameters.size - 1) {
						val newSolution = replaceDesignAspect(currentSolution._1, da._1, da._2.parameters(currentIndex + 1))
						configureOptionsRecursively(top, newSolution)
						if (top.instantiateTemplate()) {
							val configuration = (newSolution, GlobalConfig.config.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder)
							processResults(top, configuration)
							val newBestATP = designs(configuration)(ATP)
							//Check if performance of new config is better than that of the old one. If yes, update config and best performance
							if (newBestATP < bestATP) {
								SpinalProgress(s"Changed ${da._1} from ${da._2.parameters(currentIndex)} to ${da._2.parameters(currentIndex + 1)}")
								currentSolution = (newSolution, newBestATP)
								change = true
								bestATP = newBestATP
							}
						}
						top.removeNewTemplates()
					}
				})
			}

			if (bestConfig._1 == null) bestConfig = ((currentSolution._1, globalConfig.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder), bestATP)

			if (bestATP < bestConfig._2) bestConfig = ((currentSolution._1, globalConfig.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder), bestATP)

			if (globalConfig.outFile != null) {
				globalConfig.outFile.write(s"${bestATP}\n")
			}

			SpinalProgress(s"Optimization done. Evaluated a total of ${foundConfigurations} configurations.")
		}
	}

	def replaceDesignAspect(conf : Configuration, index : String, aspect : Any): Configuration = {
		val newConfig = mutable.Map[String, immutable.Map[String, Any]]()

		conf.foreach(t => newConfig += t)
		val template = index.split("#")(0)
		val asp = index.split("#")(1)

		newConfig(template) += (asp -> aspect)

		newConfig
	}

	/**
	 * Display the current process of DSE
	 */
	def displayProcess(): Unit = {
		if (globalConfig.exploreGadgets) {
			val gadgetName = globalConfig.gadgetConfig.gadget.getName()
			val gadgetIndex = AllGadgets.gadgets.indexOf(globalConfig.gadgetConfig.gadget)
			SpinalProgress(s"Current Gadget: ${gadgetName} (${gadgetIndex + 1} of ${AllGadgets.gadgets.size}) | ${foundConfigurations} found out of ${totalConfigurations}")
		} else {
			SpinalProgress(s"${foundConfigurations} found out of ${totalConfigurations}")
		}
	}

	def getOneDesignAspect(c : T, option : (String, Configurable)): ArrayBuffer[Configuration] = {
		val res = ArrayBuffer[Configuration]()
		option._2.parameters.foreach(par => {
			res += (mutable.Map(c.getFamilyName() -> immutable.Map(option._1 -> par)))
		})
		res
	}

	def mergeTwoMaps(m1 : ArrayBuffer[Configuration], m2 : ArrayBuffer[Configuration]): ArrayBuffer[Configuration] = {
		if (m1.isEmpty) return m2
		if (m2.isEmpty) return m1
		val res = ArrayBuffer[Configuration]()
		m1.foreach(m1Map => {
			m2.foreach(m2Map => {
				val newConf = mutable.Map[String, immutable.Map[String, Any]]()
				//2 cases: both have the same template => combine under this template name | different template => combine with different templates
				m1Map.foreach(m1Conf => {
					m2Map.foreach(m2Conf => {
						if (m1Conf._1 == m2Conf._1) {
							newConf += (m1Conf._1 -> (m1Conf._2 ++ m2Conf._2))
						} else {
							newConf += m1Conf
							newConf += m2Conf
						}
					})
				})
				res += mutable.Map(newConf.toSeq: _*)
				newConf.clear()
			})
		})
		res
	}

	/**
	 * Extract a list of all possible design configurations for the current template and its subtemplates
	 * @param c Current template
	 * @return List of design configurations
	 */
	def getDesignAspectsRecursively(c : T, depth : Int = 0): ArrayBuffer[Configuration] = {

		val options = c.getOptions()
		options.foreach(opt => if (opt._2.isInstanceOf[IntegerList]) {
			wellOrderedDesignAspects += (c.getFamilyName() -> opt)
		})
		options.foreach(opt => {
			if (!allDesignAspects.contains(s"${c.getFamilyName()}#${opt._1}")) {
				allDesignAspects += (s"${c.getFamilyName()}#${opt._1}" -> opt._2)
			}
		})
		exploredTemplates += c.getFamilyName()

		if (!visitedTemplates.contains(c.getFamilyName()) && options.nonEmpty) visitedTemplates += ((c.getFamilyName(), c))

		var da, designAspects = ArrayBuffer[Configuration]()
		options.foreach(opt => {
			val aspect = getOneDesignAspect(c, opt)
			da = mergeTwoMaps(da, aspect)
		})

		if (depth > 0) {
			if (da.nonEmpty) {
				da.foreach(opt => {
					c.configureTemplate(opt(c.getFamilyName()))
					var childOptions = ArrayBuffer[Configuration]()
					c.children.foreach(child => if (!child.asInstanceOf[T].hasDominantSibling()) {
						val newOptions = getDesignAspectsRecursively(child.asInstanceOf[T], depth-1)
						childOptions = mergeTwoMaps(childOptions, newOptions)
					})
					designAspects ++= mergeTwoMaps(ArrayBuffer(opt), childOptions)
					c.removeNewTemplates()
				})
			} else {
				var childOptions = ArrayBuffer[Configuration]()
				c.children.foreach(child => if (!child.asInstanceOf[T].hasDominantSibling()) {
					val newOptions = getDesignAspectsRecursively(child.asInstanceOf[T], depth-1)
					childOptions = mergeTwoMaps(childOptions, newOptions)
				})
				designAspects ++= mergeTwoMaps(designAspects, childOptions)
			}

			designAspects
		} else {
			da
		}
	}

	/**
	 * Explore templates with a cut after a predefined of template depth
	 * @param c Toplevel template
	 */
	def exploreWithLocalOptimizations(c: T): Unit = {

		SpinalProgress(s"Performing local optimizations with depth ${globalConfig.localOptimizationDepth}")
		SpinalProgress(s"Starting to fetch all configurations")
		//Get all options with maximum possible recursion depth
		val options = getDesignAspectsRecursively(c, Int.MaxValue)

		//Clear design and explored templates
		c.removeNewTemplates()
		exploredTemplates.clear()
		SpinalProgress(s"Found ${options.size} possible configurations")

		val sortedTemplates = ArrayBuffer[T]()
		val sortedTemplatesWithDepth = mutable.Map[Int, ArrayBuffer[T]]()
		val groupedTemplates = mutable.Map[Int, ArrayBuffer[T]]()
		var maxDepth = 0
		val bestConfigurations = mutable.Map[OptimizationTarget, (BigDecimal, C)]()

		//Find maximum depth of a configurable template from all templates that were visited during configuration extraction
		visitedTemplates.foreach(template => {
			//Get template depth, indicated by the number of '@' in its name
			val templateDepth = template._1.count(_ == '@')
			//Add template to the others with the same depth
			if (sortedTemplatesWithDepth.contains(templateDepth)) {
				sortedTemplatesWithDepth(templateDepth) += template._2
			} else {
				sortedTemplatesWithDepth += (templateDepth -> ArrayBuffer(template._2))
			}
			//Update the maximum depth for later
			if (templateDepth > maxDepth) maxDepth = templateDepth
		})

		//Sort the templates, starting with the one that is lowest in the hierarchy (has the highest depth)
		while (maxDepth >= 0) {
			visitedTemplates.foreach(template => {
				if (template._1.count(_ == '@') == maxDepth) sortedTemplates += template._2
			})
			maxDepth -= 1
		}

		//Add the toplevel template with depth 0 if it did not have any configuration options
		if (!sortedTemplates.contains(c)) {
			sortedTemplates += c
			sortedTemplatesWithDepth += (0 -> ArrayBuffer(c))
		}

		//Group templates into groups of size 'localOptimizationDepth' such that groups are always full from the top, i.e. only
		//the lowest group might have smaller depth than specified
		for (i <- 0 until sortedTemplatesWithDepth.size) {
			val newIndex = (i / globalConfig.localOptimizationDepth)
			if (!groupedTemplates.contains(newIndex)) {
				groupedTemplates += (newIndex -> ArrayBuffer[T]())
			}
			if (sortedTemplatesWithDepth.contains(i)) groupedTemplates(newIndex) ++= sortedTemplatesWithDepth(i)
		}

		//Go through all the groups, starting from the one that is lowest in the hierarchy
		var index = groupedTemplates.size-1
		while (index >= 0) {
			exploredTemplates.clear()
			//Go through all templates in the group, as long as the template has not yet been explored from its parent
			groupedTemplates(index).foreach(t => if (!exploredTemplates.contains(t.getFamilyName())) {
				designs.clear()
				t.removeNewTemplates()

				//Calculate the depth in the group and get all possible configurations for this template and possibly subtemplates
				val templateDepth = t.getFamilyName().count(_ == '@')
				val newDepth = globalConfig.localOptimizationDepth - (templateDepth % globalConfig.localOptimizationDepth) - 1

				val templateConfigs = getDesignAspectsRecursively(t, newDepth)

				//If configurations were found, go through all of them
				if (templateConfigs.nonEmpty) {
					templateConfigs.foreach(conf => {
						//If bestConfigurations is non-empty (i.e., there are already configurations from a previous template that need to be preserved,
						//loop through these best configurations
						if (bestConfigurations.nonEmpty) {
							globalConfig.optimizationTargets.foreach(opt => {
								//Combine existing configuration with current new one and analyse the performance
								//Performance numbers have to be added to the right list (i.e., if the existing performance optimized latency, the
								//numbers have to be added to the latency list as well
								val newConf = conf ++ bestConfigurations(opt._1)._2._1
								configureOptionsRecursively(t, newConf)
								if (t.instantiateTemplate()) {
									processResults(t, (newConf, globalConfig.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder), opt._1)
								}
								t.removeNewTemplates()
							})
						} else {
							//Otherwise just analyse the performance
							configureOptionsRecursively(t, conf)
							if (t.instantiateTemplate()) {
								processResults(t, (conf, globalConfig.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder))
							}
							t.removeNewTemplates()
						}
					})
				} else {
					//Otherwise, update the performance numbers with the existing configuration for the current template
					if (bestConfigurations.nonEmpty) {
						bestConfigurations.foreach(bestConf => {
							val newConf = bestConf._2._2._1
							configureOptionsRecursively(t, newConf)
							if (t.instantiateTemplate()) {
								processResults(t, (newConf, globalConfig.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder), bestConf._1)
							}
							t.removeNewTemplates()
						})
					} else {
						if (t.instantiateTemplate()) {
							processResults(t, (null, globalConfig.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder))
						}
						t.removeNewTemplates()
					}
				}

				//Sort the list of configurations and add the best configuration
				sortedDesigns.foreach(des => if (des._2(0)._2 != null) {
					val sorted = des._2.sortBy(_._1)
					bestConfigurations += ((des._1, sorted(0)))
				})

				clearSortedDesigns()
			})
			index -= 1
		}

		//Clear lists
		globalConfig.optimizationTargets.foreach(opt => {
			rankedDesigns(opt._1).clear()
			rankedDesigns(opt._1) += ((bestConfigurations(opt._1)._1, bestConfigurations(opt._1)._2))
		})
	}

	def exploreAllDesigns(c : T): Unit = {

	  SpinalProgress(s"Starting to fetch all configurations")
		val options = getDesignAspectsRecursively(c, Int.MaxValue)
		SpinalProgress(s"Found ${options.size} possible configurations")

		if (options.isEmpty) {
			totalConfigurations = 1
			if (c.instantiateTemplate()) {
				val conf = (null, GlobalConfig.config.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder)
				processResults(c, conf)
			}
			c.removeNewTemplates()
		}

		options.foreach(conf => {
			totalConfigurations += 1
			if ((totalConfigurations % 100) == 0) displayProcess()
			configureOptionsRecursively(c, conf)
			if (c.instantiateTemplate()) {
				val configuration = (conf, GlobalConfig.config.gadgetConfig.gadget, globalConfig.gadgetConfig.securityOrder)
				processResults(c, configuration)
			}
			c.removeNewTemplates()
		})
	}

	/**
	 * Process the results if the design could be instantiated. Evaluate the performance numbers and add config/performance to the corresponding Arrays
	 * @param c Toplevel Template
	 * @param conf Current design configuration
	 */
	def processResults(c : T, conf : C, opt : OptimizationTarget = null): Unit = {
		foundConfigurations += 1

		val andArea = c.andCount * areaConfig("AND")(GlobalConfig.config.gadgetConfig.securityOrder)
		val xorArea = c.xorCount * areaConfig("XOR")(GlobalConfig.config.gadgetConfig.securityOrder)
		val notArea = c.notCount * areaConfig("NOT")(GlobalConfig.config.gadgetConfig.securityOrder)
		val regArea = c.regCount * areaConfig("REG")(GlobalConfig.config.gadgetConfig.securityOrder)
		val estimatedArea = ((andArea + xorArea + notArea + regArea) / globalConfig.nandSize)

		val atp = c.latency * estimatedArea
		val atrp = atp * c.randomness

		val performanceMetrics = mutable.Map[OptimizationTarget, BigDecimal]()
		performanceMetrics += (Latency -> c.latency)
		performanceMetrics += (Area -> estimatedArea)
		performanceMetrics += (Randomness -> c.randomness)
		performanceMetrics += (ATP -> atp)
		performanceMetrics += (ATRP -> atrp)
		performanceMetrics += (Reload -> c.reload)

		if (opt == null) {
			globalConfig.optimizationTargets.foreach(opt => {
				rankedDesigns(opt._1) += ((performanceMetrics(opt._1), conf))
				sortedDesigns(opt._1) += ((performanceMetrics(opt._1), conf))
			})
		} else {
			rankedDesigns(opt) += ((performanceMetrics(opt), conf))
			sortedDesigns(opt) += ((performanceMetrics(opt), conf))
		}

		//Add optimization goal with respective performance to array of all designs
		designs += (conf -> mutable.Map[OptimizationTarget, BigDecimal]())
		performanceMetrics.foreach(pm => designs(conf) += pm)

		thresholdDesigns += ((conf, performanceMetrics))
	}

	/**
	 * Configure a design with a given configuration (globalConfig, localConfig, gadget)
	 * @param c Toplevel template
	 * @param conf Configuration
	 */
	def configure(c : T, conf : C): Unit = {
		//Update gadgetConfig
		if (conf._2 != null) conf._2.updateConfig()
		c.updateConfig()

		//Configure options
		configureOptionsRecursively(c, conf._1)
	}

	def configureOptionsRecursively(c : T, config : Configuration): Unit = {
		if (config == null) return
		if (config.contains(c.getFamilyName())) c.configureTemplate(config(c.getFamilyName()))
		else if (c.dominantSibling != null) {
			if (config.contains(c.dominantSibling.getFamilyName())) c.configureTemplate(config(c.dominantSibling.getFamilyName()))
			propagatePairs(c.dominantSibling, c)
		}

		c.children.foreach(child => configureOptionsRecursively(child.asInstanceOf[T], config))
	}


	/**
	 * Propagate the dominant sibling information through a design
	 * @param c The starting Template
	 */
	def propagateSiblingDominance(c : T): Unit = {
		val family = mutable.Map[T, ArrayBuffer[T]]()
		c.children.foreach(child => {
			//If the child is not a dominant sibling and it has no dominant sibling, we can continue with this child
			if (!(child.asInstanceOf[T].isDominantSibling || child.asInstanceOf[T].hasDominantSibling())) propagateSiblingDominance(child.asInstanceOf[T])
			//If the child is a dominant sibling, we add it as a new key to the family
			if (child.asInstanceOf[T].isDominantSibling) family += (child.asInstanceOf[T] -> ArrayBuffer[T]())
			//If the child has a dominant sibling, we add the child to the ArrayBuffer of its dominant sibling
			if (child.asInstanceOf[T].hasDominantSibling()) family(child.asInstanceOf[T].dominantSibling) += child.asInstanceOf[T]
		})

		//For each entry in the family, we propagate the information to all non-dominant siblings
		family.foreach(siblings => {
			siblings._2.foreach(slave => {
				val master = siblings._1
				propagatePairs(master, slave)
			})
		})
	}

	/**
	 * Propagate sibling information through 2 Templates, one dominant and one non-dominant sibling of the same generation
	 * @param master The dominant sibling
	 * @param slave The non-dominant sibling
	 */
	def propagatePairs(master : T, slave : T): Unit = {
		val numberOfChildren = master.children.size
		//Go through all children of the master (the slave has the exact same amount of children at the exact same position)
		for (i <- 0 until numberOfChildren) {
			//If the child of the master has a dominant sibling, the same child of the slave gets a reference to the master's child's dominant sibling
			if (master.children(i).asInstanceOf[T].hasDominantSibling()) {
				slave.children(i).asInstanceOf[T].dominantSibling = master.children(i).asInstanceOf[T].dominantSibling
				slave.children(i).asInstanceOf[T].isDominantSibling = false
			}
			//Otherwise, the slave child gets a reference to the master's child
			else {
				slave.children(i).asInstanceOf[T].dominantSibling = master.children(i).asInstanceOf[T]
				slave.children(i).asInstanceOf[T].isDominantSibling = false
			}
		}
	}
}
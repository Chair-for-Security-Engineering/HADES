import AdderModQPipelined.{FullyPipelined, NotPipelined}
import Gadgets._
import spinal.core._
import spinal.core.internals._
import spinal.core.sim._
import ujson.Value

import java.math.{MathContext, RoundingMode}
import java.nio.file.{Files, Paths}
import scala.collection.{immutable, mutable}
import scala.collection.mutable._


object HADES {
	def main(args: Array[String]): Unit = {
		//HADES("HADES")(new KoggeStoneAdder(32, true, true, false))().apply()
		//HADES("HADES")(new AdderModQ(3329))().apply()
		//HADES("HADES")(new AES(128))().apply()
		//HADES("HADES")(new CSubQ(3320))().apply()
		HADES("HADES")(new Keccak(64))().apply()
	}
}

case class HADES(configurationFile : String)(gen: => Template)(sim : Component => Unit = null) extends GlobalConfigUser {

	val areaConfig = mutable.Map[String, mutable.Map[String, ArrayBuffer[BigDecimal]]]()
	var nandSize = BigDecimal(0)
	val gadgetConfig = GadgetConfig()

	var exploreGadgets, exploreSecurity, enableThresholds, enableLocalSearch : Boolean = false

	var verbosity, topX, localSearchInitialSampleSize, localSearchRuns : Int = 0
	var taskExplore, taskGenerate, taskSimulate, separateFolders, fixedSeed : Boolean = false
	var exploreLatency, exploreArea, exploreRandomness, exploreReload, exploreATP, exploreATRP : Boolean = false
	var visEnable, visFull, gcEnable, scEnable, fiEnable, comEnable : Boolean = false
	var scOrder, fiOrder, comScaOrder, comFaultOrder, gcLatencyAnd, gcLatencyXor, gcLatencyNot, localOptimizationDepth : Int = 0
	var language, visPath, resetType, namePrefix : String = null
	var mode : SpinalMode = null
	val maximumSecurityOrder = 10

	val optimizationTargets = mutable.LinkedHashMap[OptimizationTarget, OptimizationTarget]()

	val thresholds = ArrayBuffer[(OptimizationTarget, BigDecimal)]()

	var possibleDesigns = mutable.Map[C, mutable.Map[OptimizationTarget, BigDecimal]]()
	var suitableDesigns = ArrayBuffer[mutable.Map[OptimizationTarget, ArrayBuffer[(BigDecimal, C)]]]()

	var config = Config()

	var toplevelName : String = ""
	var targetDirectory : String = ""


	def apply(): Unit = {
		//Read Configuration JSON-File
		readConfiguration(configurationFile)

		//Create config from parsed configuration
		config = Config(
			spinalConfig = if (taskSimulate || (mode == Verilog)) SpinalConfigDefault.verilog() else SpinalConfigDefault.vhdl(),
			simConfig = SpinalConfigDefault.sim(),
			gadgetConfig = gadgetConfig
		)
		config.exploreGadgets = exploreGadgets
		config.areaConfig = areaConfig
		config.nandSize = nandSize
		config.resetKind = if (resetType == "synchronous") SYNC else if (resetType == "asynchronous") ASYNC else BOOT
		config.topX = topX
		config.optimizationTargets = optimizationTargets
		config.localOptimizationDepth = localOptimizationDepth

		val design = new Environment(config)(gen)
		design.initialize()

		toplevelName = if (design.top.templateName != "") design.top.templateName else design.top.getClass.getName
		targetDirectory = config.spinalConfig.targetDirectory

		//Exploration
		if (taskExplore) {
			SpinalProgress(s"Starting to explore all possible designs for Template ${design.top.asInstanceOf[Component].getClass.getName}")

			if (exploreSecurity) {
				var belowThreshold = true
				var order = 1

				//Explore while new suitable designs were found in the previous run (otherwise, this and subsequent runs will also bring no new
				//suitable designs) and while the order is lower than the currently supported maximum sc-order (10)
				while (belowThreshold && (order <= maximumSecurityOrder)) {
					SpinalProgress(s"Exploring security order ${order}")

					//Add a new Map into the array
					suitableDesigns += mutable.Map[OptimizationTarget, ArrayBuffer[(BigDecimal, C)]]()
					optimizationTargets.foreach(opt => {
						suitableDesigns.last += (opt._1 -> ArrayBuffer[(BigDecimal, C)]())
					})

					//Update security order in the configuration and explore all designs for this order
					gadgetConfig.securityOrder = order
					design.explore()

					//Identify all suitable configurations
					findSuitableDesigns(design)

					//True if new suitable configs were found, otherwise false and abort
					belowThreshold = suitableDesigns.last.head._2.nonEmpty

					//Increment order and clear some arrays/maps
					possibleDesigns ++= design.explorer.designs
					order += 1
					design.explorer.designs.clear()
					design.explorer.thresholdDesigns.clear()
				}
			} else if (enableLocalSearch) {
				design.explorer.localSearch(design.top, localSearchInitialSampleSize, localSearchRuns)
				possibleDesigns = design.explorer.designs
			} else {
				design.explore()
				possibleDesigns = design.explorer.designs
			}

			SpinalProgress(s"${design.explorer.foundConfigurations} possible ${if (design.explorer.foundConfigurations == 1) "configuration" else "configurations"} out of ${design.explorer.totalConfigurations} total ${if (design.explorer.totalConfigurations == 1) "configuration" else "configurations"} ${if (design.explorer.foundConfigurations == 1) "was" else "were"} found")
		}

		//If security was explored before, suitable designs is non-empty and we do not need to perform another exploration with fixed order
		if (thresholds.nonEmpty && enableThresholds && suitableDesigns.isEmpty) {
			suitableDesigns += mutable.Map[OptimizationTarget, ArrayBuffer[(BigDecimal, C)]]()
			optimizationTargets.foreach(opt => {
				suitableDesigns.last += (opt._1 -> ArrayBuffer[(BigDecimal, C)]())
			})

			findSuitableDesigns(design)

			if (suitableDesigns.head(optimizationTargets.head._1).nonEmpty) {
				SpinalProgress(s"${suitableDesigns.head(optimizationTargets.head._1).size} designs below the specified thresholds were found.")
			} else {
				SpinalProgress(s"No suitable designs were found, please consider increasing the specified threshold(s).")
			}
		} else if (suitableDesigns.isEmpty) {
			suitableDesigns += design.explorer.rankedDesigns
		}

		//Sort designs according to the defined optimization targets
		optimizationTargets.foreach(opt => {
			processResults(opt._1, opt._2)
		})
	}

	def readConfiguration(file : String): Unit = {
		//Read JSON file
		val wd = os.pwd / "src" / "main" / "scala" / "Configuration"
		val jsonString = os.read(wd / s"${file}.json")
		val data = ujson.read(jsonString)

		//Read all data from the file, perform some sanity checks on the read data
		val general = data("General")
		val cellLibraryFile = general("Cell-library").str

		resetType = general("Reset-Type").str
		assert(resetType == "synchronous" || resetType == "asynchronous", s"Reset has to be either synchronous or synchronous")

		language = general("Mode").str.toLowerCase
		assert(language == "vhdl" || language == "verilog", s"Mode has to be either vhdl or verilog")
		mode = if (language == "vhdl") VHDL else Verilog

		verbosity = general("Verbose").num.toInt
		assert(Seq(0,1,2,3).contains(verbosity), s"Verbosity has to be in the range of 0 to 3")

		namePrefix = general("Name-Prefix").str

		separateFolders = general("Separate-Folders").bool

		fixedSeed = general("Fixed-Seed").bool

		localOptimizationDepth = general("Local-Optimization-Depth").num.toInt
		assert(localOptimizationDepth >= 0, s"Local optimization depth cannot be negative")

		val tasks = general("Tasks")
		taskExplore = tasks("Explore").bool
		if (taskExplore) assert(cellLibraryFile != "", s"Cell-Library file is necessary for exploration")
		taskGenerate = tasks("Generate").bool
		taskSimulate = tasks("Simulate").bool
		assert((taskExplore | taskGenerate | taskSimulate), s"One of the tasks has to be true")
		if (taskSimulate) assert(taskGenerate, s"For simulation, generation has to be true as well")
		assert(!(taskSimulate && (mode == VHDL)), s"Simulation currently NOT supported with VHDL, please change to Verilog for simulation")

		topX = general("Top-X").num.toInt
		assert((topX >= 1) || (topX == -1), s"Top-X has to be greater than 1 or equal to -1")

		exploreSecurity = general("Explore-Security").bool

		val localSearch = general("Local-Search")
		enableLocalSearch = localSearch("Enable").bool
		localSearchInitialSampleSize = localSearch("Initial-Sample-Size").num.toInt
		localSearchRuns = localSearch("Runs").num.toInt

		enableThresholds = general("Enable-Thresholds").bool
		val thresholdData = general("Thresholds").arr
		thresholdData.foreach(t => {
			thresholds += ((ConvertStringToOpt(t("Target").str), BigDecimal(t("Value").toString())))
		})
		assert(!(enableThresholds && (localOptimizationDepth > 0)), s"Thresholds and local optimizations currently do not work together")
		if (exploreSecurity) assert((enableThresholds && (thresholdData.nonEmpty)), s"At least one Threshold has to be given if security is explored")

		val optimizationBuffer = general("Optimization").arr
		val optimization = Map[String, Value]()
		val optimizationStrings = Seq("Area", "ATP", "ATRP", "Latency", "Randomness", "Reload")
		optimizationBuffer.foreach(opt => optimization += (opt.value.asInstanceOf[Map[String, Value]].head._1 -> opt.value.asInstanceOf[Map[String, Value]].head._2))

		optimizationStrings.foreach(optString => {
			val opt = optimization(optString)
			val optEnable = opt("Enable").bool
			val optTarget = ConvertStringToOpt(optString)
			val optSec = ConvertStringToOpt(opt("Secondary").str)

			if (optEnable) optimizationTargets += (optTarget -> optSec)
		})

		if (taskExplore && optimizationTargets.isEmpty) optimizationTargets += (Anything -> Anything)

		val visualization = general("Visualization")
		visEnable = visualization("Enable").bool
		visPath = visualization("Path").str
		visFull = visualization("Full").bool

		val gc = general("Gadget-Config")
		gcEnable = gc("Enable").bool
		gcLatencyAnd = gc("LatencyAND").num.toInt
		gcLatencyXor = gc("LatencyXOR").num.toInt
		gcLatencyNot = gc("LatencyNOT").num.toInt
		assert((gcLatencyAnd >= 0) && (gcLatencyXor >= 0) && (gcLatencyNot >= 0), s"Latency for gadgets has to be greater than 0")

		val sideChannel = data("Side-Channel")
		scEnable = sideChannel("Enable").bool
		scOrder = sideChannel("Order").num.toInt
		if (scEnable) assert(scOrder >= 1, s"Side-Channel order has to be greater than 1")

		val faultInjection = data("Fault-Injection")
		fiEnable = faultInjection("Enable").bool
		fiOrder = faultInjection("Order").num.toInt
		if (fiEnable) assert(fiOrder >= 1, s"Fault-Injection order has to be greater than 1")

		val combined = data("Combined")
		comEnable = combined("Enable").bool
		comScaOrder = combined("Sca-order").num.toInt
		comFaultOrder = combined("Fault-order").num.toInt
		if (comEnable) assert((comScaOrder + comFaultOrder) >= 1, s"At least one of the two orders has to be greater than 1")

		if (taskExplore || taskGenerate) {
			if (scEnable) assert((!fiEnable) && (!comEnable), s"Masking is active, no fault or combined gadgets possible")
			if (fiEnable) assert((!scEnable) && (!comEnable), s"Faults are active, no masking or combined gadgets possible")
			if (comEnable) assert((!scEnable) && (!fiEnable), s"Combined is active, no masking or fault gadgets possible")
			if (gcEnable) assert(!(scEnable | fiEnable | comEnable), s"Cannot use gadgets if gadgetConfig is being used")
			assert(gcEnable | scEnable | fiEnable | comEnable, message = s"Either the gadgetConfig or one of masking/fault/combined gadgets has to be used")
		}

		if (scEnable | fiEnable | comEnable) assert(!taskSimulate, s"Simulation with Gadgets is currently not supported")

		//Read in AreaConfig
		readAreaConfig(cellLibraryFile)

		//Change gadgetConfig if necessary
		if (gcEnable) {
			gadgetConfig.latencyAnd = gcLatencyAnd
			gadgetConfig.latencyXor = gcLatencyXor
			gadgetConfig.latencyNot = gcLatencyNot
			exploreGadgets = false
		}

		if (scEnable | fiEnable | comEnable) exploreGadgets = true

		if (scEnable) gadgetConfig.securityOrder = scOrder
		if (fiEnable) gadgetConfig.securityOrder = fiOrder

		//Give each optimization target a unique position using an integer
		var optIndex = 0
		optimizationTargets.foreach(opt => {
			opt._1.position = optIndex
			optIndex += 1
		})
	}

	def readAreaConfig(file : String): Unit = {
		val wd = os.pwd / "src" / "main" / "scala" / "Gadgets"
		val jsonString = os.read(wd / s"${file}.json")
		val data = ujson.read(jsonString)

		nandSize = BigDecimal(data("Nand-Size").num)
		val gadgets = data("Gadgets").arr
		gadgets.foreach(gadget => {
			val gadgetName = gadget("Name").str
			areaConfig += (gadgetName -> mutable.Map[String, ArrayBuffer[BigDecimal]]())
			for (gateType <- List("AND", "XOR", "NOT", "REG")) {
				val gate = gadget(gateType)
				areaConfig(gadgetName) += (gateType -> ArrayBuffer[BigDecimal]())
				val upperBound = if (gadgetName == "Default") 0 else 10
				for (i <- 0 to upperBound) {
					areaConfig(gadgetName)(gateType) += BigDecimal(gate(i.toString).num)
				}
			}
		})
	}

	/**
	 * Print the best results and generate/simulate the hardware files
	 * @param what String telling what is sorted for (e.g., area, latency, randomness)
	 */
	def processResults(what : OptimizationTarget, secondary : OptimizationTarget): Unit = {
		var sorted = ArrayBuffer[(BigDecimal, (Configuration, MaskedGadget, Int))]()
		//If security order is fixed, or only order 1 gives suitable configs, sort the only array
		if (suitableDesigns.size == 1) {
			sorted = suitableDesigns.head(what).sortBy(_._1)
		}	else {
			//If maximum possible security order can be achieved with given thresholds, sort last array
			if (suitableDesigns.last(what).nonEmpty) {
				sorted = suitableDesigns.last(what).sortBy(_._1)
			} else {
				//Otherwise, sort the second to last array (as the last will be empty)
				val index = suitableDesigns.size - 2
				sorted = suitableDesigns(index)(what).sortBy(_._1)
			}
		}
		sorted = sortBySecondary(sorted, secondary)
		if (topX != -1) sorted = sorted.take(topX)

		val numberOfDesigns = math.min(sorted.size, topX)

		if (exploreSecurity) SpinalProgress(s"Maximum possible security order: ${if (suitableDesigns.last(what).nonEmpty) suitableDesigns.size else suitableDesigns.size-1}")
		SpinalProgress(s"Top ${numberOfDesigns} configurations sorted by ${what} with secondary optimization target ${secondary}")
		val padding = "| %1$10s | %2$10s | %3$10s | %4$10s | %5$10s | %6$10s | %7$11s |"
		SpinalProgress(s"${padding.format("Latency", "Reload", "Area", "Randomness", "ATP", "ATRP", "Configuration")}")
		SpinalProgress(s"${"-" * (12*6+15+8)}")

		sorted.foreach(cg => {
			val perf = possibleDesigns(cg._2)
			SpinalProgress(s"${padding.format(perf(Latency), perf(Reload), perf(Area).toBigInt, perf(Randomness), PrettyPrint(perf(ATP).toBigInt), PrettyPrint(perf(ATRP).toBigInt), cg._2)}")
		})
		SpinalProgress(s"")

		val newName = if (namePrefix != "") s"${namePrefix}_Sec_${scOrder}_${what}" else s"${toplevelName}_Sec_${scOrder}_${what}"
		var i = 0

		if (taskGenerate || taskSimulate) {
			SpinalProgress(s"Starting generation and simulation of the designs")
			//Create VHDL or Verilog folder in Hardware if not yet there
			Files.createDirectories(Paths.get(targetDirectory))

			sorted.foreach(conf => {
				val perf = possibleDesigns(conf._2)
				val newNameWithSec = s"${newName}_${i}"
				val pathWithSec = s"${targetDirectory}/${newNameWithSec}"
				if (separateFolders) {
					//Create named subfolder and update SpinalConfig
					Files.createDirectories(Paths.get(pathWithSec))
					config.spinalConfig = SpinalConfig(mode = config.spinalConfig.mode, genVhdlPkg = true, targetDirectory = pathWithSec, bitVectorWidthMax = 2147483647)
				}
				config.spinalConfig = config.spinalConfig.copy(rtlHeader =
					s"""
          |HADES performance estimation:
          |${"-" * (12*6+15+8)}
					|${padding.format("Latency", "Reload", "Area", "Randomness", "ATP", "ATRP", "Configuration")}
					|${"-" * (12*6+15+8)}
          |${padding.format(perf(Latency), perf(Reload), perf(Area), perf(Randomness), PrettyPrint(perf(ATP).toBigInt), PrettyPrint(perf(ATRP).toBigInt), conf._2)}
					|""".stripMargin)
				val newDesign = new Environment(config)(gen)
				newDesign.initialize()
				newDesign.top.definitionName = newNameWithSec
				newDesign.explorer.configure(newDesign.top, conf._2)
				newDesign.instantiate()

				if (taskGenerate) {
					newDesign.execute()
					newDesign.finish()
				}

				if (taskSimulate) {
					SpinalProgress(s"Starting simulation with following configuration:")
					SpinalProgress(s"\tDesignAspects: ${conf._2._1}")
					SpinalProgress(s"\tGadget: ${conf._2._2}")

					newDesign.simConfig._workspaceName = null
					newDesign.simulate(fixedSeed, sim)
				}

				i += 1
			})
		}
	}

	/**
	 * Among all explored designs, find those that are below the specified thresholds
	 * @param environment Current Environment
	 */
	def findSuitableDesigns(environment : Environment[Template]): Unit = {
		environment.explorer.thresholdDesigns.foreach(des => {
			var belowThreshold = true
			//If one PM is above the corresponding threshold, set variable to false
			thresholds.foreach(t => {
				if (des._2(t._1) > t._2) belowThreshold = false
			})
			//Only add the configuration to the list of suitable designs if ALL PMs were below the thresholds
			if (belowThreshold) {
				optimizationTargets.foreach(opt => {
					suitableDesigns.last(opt._1) += ((des._2(opt._1), des._1))
				})
			}
		})
	}

	/**
	 * Sort a sorted list of configurations by a secondary optimization goal
	 * @param sorted List of configurations, sorted by a primary optimization goal
	 * @param secondary Secondary optimization goal
	 * @return List of configurations, sorted by the secondary optimization goal
	 */
	def sortBySecondary(sorted : ArrayBuffer[(BigDecimal, (Configuration, MaskedGadget, Int))], secondary : OptimizationTarget): ArrayBuffer[(BigDecimal, (Configuration, MaskedGadget, Int))] = {
		val secondarySorted = ArrayBuffer[(BigDecimal, (Configuration, MaskedGadget, Int))]()

		var currentIndex = 0

		//Sort by secondary until either the topX designs are found (if topX > 0) or all designs are sorted (topX = -1)
		while (((secondarySorted.size < scala.math.min(topX, sorted.size)) && (topX != -1)) || (secondarySorted.size < sorted.size)) {
			//Get minimal performance value
			val minPerformance = sorted(currentIndex)._1
			var tempSorted = ArrayBuffer[(BigDecimal, (Configuration, MaskedGadget, Int))]()
			//Look up performance of secondary optimization goal if primary optimization goal meets the minimal performance
			sorted.foreach(cg => if (cg._1 == minPerformance) {
				tempSorted += (((possibleDesigns(cg._2))(secondary), cg._2))
				currentIndex += 1
			})
			//Sort the batch (by the secondary goal) and append it to the array
			tempSorted = tempSorted.sortBy(_._1)
			secondarySorted ++= tempSorted
		}

		secondarySorted
	}
}
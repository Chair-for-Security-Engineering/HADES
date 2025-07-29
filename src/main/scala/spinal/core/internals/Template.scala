package spinal.core.internals

import spinal.core._
import Composability._

import scala.collection.mutable._
import scala.collection.{mutable, immutable}

/**
 * Template class
 */

abstract class Template() extends Component with BooleanGateUser with GlobalConfigUser {

	var templateName: String = ""

	var latency: Int = 0

	var randomness: Int = 0

	var reload: Int = 0

	var randomnessOptimization: Boolean = false

	var composability: ComposabilityNotion = NI

	var andCount: Int = 0

	var xorCount: Int = 0

	var notCount: Int = 0

	var regCount: Int = 0

	/**
	 * Map between all expressions of this template and their shared expressions (if it has any)
	 */
	val sharedSignals: HashMap[Expression, ArrayBuffer[Expression]] = HashMap()

	/**
	 * Map that contains all signals of this template with the information whether this signal has already been visited
	 * during sharing, and an array of all signals that write the current signal directly
	 */
	val previousSignals: mutable.LinkedHashMap[BaseType, (Boolean, ArrayBuffer[BaseType])] = mutable.LinkedHashMap()

	val statementsToRemove = ArrayBuffer[Statement]()

	/**
	 * Map between unmasked gates and their respective masked gadget
	 */
	val linkedGates = mutable.Map[Component, Component]()

	val bestConfigurations = mutable.Map[OptimizationTarget, (BigDecimal, C)]()

	var dominantSibling: Template = null
	var isDominantSibling: Boolean = false
	var numberOfSiblings: Int = 0

	def hasDominantSibling(): Boolean = (dominantSibling != null)

	var isNewChild : Boolean = false
	def setAsNewChild(): Unit = isNewChild = true

	//Quick access to the latencies of the masked gadgets
	var latencyXor = gadgetConfig.latencyXor
	var latencyAnd = gadgetConfig.latencyAnd
	var latencyNot = gadgetConfig.latencyNot

	//Clock and reset signal that are most likely used by all templates
	val control = new Bundle {
		val clk 	= in Bool()
		val reset = in Bool()
	}

	val cd = ClockDomain(clock = control.clk, reset = control.reset, config = ClockDomainConfig(resetKind = globalConfig.resetKind))

	//Use clk and reset for the ClockDomain and push it onto the stack
	ClockDomainStack.set(cd)

	/**
	 * Instantiate multiple sub-templates of the same type
	 * @param n Number of sub-templates to be instantiated
	 * @param gen Instantiation function of the sub-template (e.g., FullAdder() to instantiate a the FullAdder template
	 * @tparam T Component/Template
	 * @return An ArrayBuffer with the instantiated sub-templates
	 */
	def instantiateMultipleSubTemplates[T <: Template](n : Int)(gen: => T): ArrayBuffer[T] = {
		//Empty ArrayBuffer of type T
		val ret = ArrayBuffer[T]()
		var dominantSibling: Template = null
		for (i <- 0 until n) {
			//Create n sub-templates
			ret += gen
			//Set Name to className_i
			ret(i).setName(s"${ret(i).getClass.getName.toLowerCase()}_${i}", weak = true)

			//The first instantiated template is the "dominant sibling", which decides how the templates are configured
			//The other templates get a reference to the dominant sibling
			if (i == 0) {
				ret(i).isDominantSibling = true
				dominantSibling = ret(i)
			}
			else {
				ret(i).isDominantSibling = false
				ret(i).dominantSibling = dominantSibling
				if (GlobalConfig.dryRun) ret(0).numberOfSiblings += 1
			}
		}
		ret
	}

	final def instantiateTemplate(): Boolean = {
		//If template is a sibling: return immediately, performance prediction is handled by dominant sibling
		if (GlobalConfig.dryRun && this.dominantSibling != null) return true

		val ctx1 = DslScopeStack.set(this.dslBody)
		val ctx2 = ClockDomainStack.set(cd)

		//Reset Flags
		globalData.nodeAreNamed = false
		globalData.nodeAreInferringWidth = false
		globalData.nodeAreInferringEnumEncoding = false

		//Remove old Assignments before creating the new ones
		this.removeBody()

		val ret = instantiate()

		//If template is dominant sibling and not the toplevel template: add performance metrics of current template (and eventual siblings)
		//to those of the parent template
		if (isDominantSibling && parent != null) {
			parent.asInstanceOf[Template].andCount += andCount * (numberOfSiblings + 1)
			parent.asInstanceOf[Template].xorCount += xorCount * (numberOfSiblings + 1)
			parent.asInstanceOf[Template].notCount += notCount * (numberOfSiblings + 1)
			parent.asInstanceOf[Template].regCount += regCount * (numberOfSiblings + 1)
		} else if (parent != null) {
			parent.asInstanceOf[Template].andCount += andCount
			parent.asInstanceOf[Template].xorCount += xorCount
			parent.asInstanceOf[Template].notCount += notCount
			parent.asInstanceOf[Template].regCount += regCount
		}

		//Calculate the randomness for the current template based on its gate counts
		randomness = (andCount * gadgetConfig.randomnessAnd) + (xorCount * gadgetConfig.randomnessXor) + (notCount * gadgetConfig.randomnessNot)

		ctx1.restore()
		ctx2.restore()
    ret
	}

	def removeBody[T <: Template](): Unit = {
		dslBody.walkStatements(s => s match {
			case a: AssignmentStatement => {
				a.removeStatement()
			}
			case d: DeclarationStatement => {
				if (!(d.asInstanceOf[BaseType].isInput || d.asInstanceOf[BaseType].isOutput)) d.removeStatement()
			}
			case s: SwitchStatement => s.removeStatement()
			case w: WhenStatement => w.removeStatement()
			case _ =>
		})
	}

	def removeNewTemplates[T <: Template](): Unit = {
		this.randomness = 0
		this.andCount = 0
		this.xorCount = 0
		this.notCount = 0
		this.regCount = 0

		val templatesToRemove = ArrayBuffer[T]()
		children.foreach(child => child match {
			case t: T => {
				if (t.isNewChild) templatesToRemove += t
				t.removeNewTemplates()
			}
			case _ =>
		})
		templatesToRemove.foreach(t => {
			t.removeBody()
			children -= t
		})
		templatesToRemove.clear()
	}

  def instantiate(): Boolean

	def getOptions(): Map[String, Configurable] = Map()

	final def configureTemplate(options : immutable.Map[String, Any]): Unit = {
		val ctx1 = DslScopeStack.set(this.dslBody)
		val ctx2 = ClockDomainStack.set(cd)

		val oldChildren = this.children.toList

		configure(options)

		this.children.foreach(child => {
			if (!oldChildren.contains(child)) {
				child.asInstanceOf[Template].setAsNewChild()
			}
		})

		ctx1.restore()
		ctx2.restore()
	}

	def configure(options : immutable.Map[String, Any]): Unit = {}

	/**
	 * Get the full name of a Template with all its parents
	 * @return The family name of a Template
	 */
	def getFamilyName(): String = {
		if (this == null) return ""
		if (this.parent == null) this.getName()
		else s"${this.parent.asInstanceOf[Template].getFamilyName()}@${this.getName()}"
	}

	/**
	 * Update the latencies for the gadgets in the Template
	 */
	def updateConfig(): Unit = {
		latencyAnd = gadgetConfig.latencyAnd
		latencyXor = gadgetConfig.latencyXor
		latencyNot = gadgetConfig.latencyNot
		children.foreach(child => child.asInstanceOf[Template].updateConfig())
	}

	/**
	 * Connect the randomness ports of all templates together
	 * @tparam T
	 */
	def connectRandomnessPorts[T <: Template](): Unit = {
		val c = this

		val r = getIoFromName("io_r", c)
		var bitsUsed = 0

		c.children.foreach(child => {
			val childR = if (child.isInstanceOf[BlackBox]) getIoFromName("r", child) else getIoFromName("io_r", child)
			if (childR != null) {
				val d = new DataAssignmentStatement
				d.target = childR
				val bits = new BitsRangedAccessFixed
				bits.source = r.asInstanceOf[Expression with WidthProvider]
				bits.lo = bitsUsed
				val childRandomness = child match {
					case t: Template => t.randomness
					case b: BlackBox => childR.getBitsWidth
					case _ => 0
				}

				bits.hi = bitsUsed + childRandomness - 1
				d.source = bits
				c.dslBody.append(d)
				bitsUsed = bitsUsed + childRandomness
			}
		})

		c.children.foreach {
			case template: Template => template.connectRandomnessPorts()
			case _ =>
		}
	}

	/**
	 * Add a port for the randomness in a remplate
	 * @tparam T
	 */
	def addRandomnessPort[T <: Template](): Unit = {
		val c = this

		//Only add the port r is randomness of the template is > 0
		val r = if (c.randomness > 0) Bits(c.randomness bits) else null
		if (c.randomness > 0) {
			r.dir = in
			r.setName(s"io_r")
			r.parentScope = c.dslBody.head.parentScope
			c.ioSet += r
		}

		//Do the same for all children
		c.children.foreach(child => if (child.isInstanceOf[Template]) child.asInstanceOf[Template].addRandomnessPort())
	}

	def countGates[T <: Template](cumulative : Boolean = false): Unit = {
		val c = this

		if (!GlobalConfig.dryRun) {
			c.andCount = 0
			c.xorCount = 0
			c.notCount = 0

			c.children.foreach(child => {
				child match {
					case a: And => c.andCount += 1
					case x: Xor => c.xorCount += 1
					case n: Not => c.notCount += 1
					case t: Template => {
						t.countGates(true)
						c.andCount += t.andCount
						c.xorCount += t.xorCount
						c.notCount += t.notCount
					}
					case _ =>
				}
			})
		} else {
			c.children.foreach(child => {
				child match {
					case a: And =>
					case x: Xor =>
					case n: Not =>
					case t: Template => {
						t.countGates(true)
						c.andCount += t.andCount * (t.numberOfSiblings+1)
						c.xorCount += t.xorCount * (t.numberOfSiblings+1)
						c.notCount += t.notCount * (t.numberOfSiblings+1)
					}
					case _ =>
				}
			})
		}
	}

	def countRegisters[T <: Template](): Unit = {
		val c = this

		if (!GlobalConfig.dryRun) {
			c.regCount = 0

			c.dslBody.walkDeclarations(d => d match {
				case bt: BaseType => if (bt.isReg) c.regCount += bt.getBitsWidth
				case _ =>
			})

			c.children.foreach(child => child match {
				case u: BooleanGate =>
				case t: Template => {
					t.countRegisters()
					c.regCount += t.regCount
				}
				case _ =>
			})
		} else {
			c.dslBody.walkDeclarations(d => d match {
				case bt: BaseType => if (bt.isReg) {
					c.regCount += bt.getBitsWidth
				}
				case _ =>
			})

			c.children.foreach(child => child match {
				case t: Template => if (t.dominantSibling == null) {
					t.countRegisters()
					c.regCount += t.regCount * (t.numberOfSiblings+1)
				}
				case _ =>
			})
		}
	}

	/**
	 * Calculate the randomness demands of a template
	 * @tparam T
	 */
	def inferRandomness[T <: Template](): Unit = {
		val c = this

		//Randomness from gates in this template
		c.randomness = (c.andCount * gadgetConfig.randomnessAnd) +
		 (c.xorCount * gadgetConfig.randomnessXor) + (c.notCount * gadgetConfig.randomnessNot)

		//Calculate the randomness for the children
		c.children.foreach(child => if (!child.isInstanceOf[BlackBox]) child.asInstanceOf[Template].inferRandomness())
	}

	/**
	 * Remove all statements that are no longer used in the masked design (i.e., all that have a tag share)
	 * @tparam T
	 */
	def removeUnusedStatements[T <: Template](): Unit = {
		val c = this

		//Remove unshared declarations
		c.dslBody.walkDeclarations(d => if (d.asInstanceOf[BaseType].hasTag(share)) d.removeStatement)

		//Remove InitAssignments and DataAssignments if the target has the tag share
		c.dslBody.walkStatements(s => s match {
			case i: InitAssignmentStatement => {
				if (i.target.asInstanceOf[BaseType].hasTag(share)) i.removeStatement
			}
			case d: DataAssignmentStatement => d.target match {
				case bt: BaseType => if (bt.hasTag(share)) d.removeStatement
				case baf: BitAssignmentFixed => if (baf.out.hasTag(share)) d.removeStatement()
				case raf: RangedAssignmentFixed => if (raf.out.hasTag(share)) d.removeStatement()
				case baf: BitAssignmentFloating => if (baf.out.hasTag(share)) d.removeStatement()
				case _ => SpinalError("Missing case in removeUnusedStatements")
			}
			case _ =>
		})

		statementsToRemove.foreach(s => {
			s.removeStatement()
		})

		c.children.foreach(child => if (!child.isInstanceOf[BlackBox]) child.asInstanceOf[Template].removeUnusedStatements())
	}

	/**
	 * Duplicate all statements in a design
	 * @tparam T
	 */
	def duplicateAllStatements[T <: Component](): Unit = {
		val c = this

		//Set all io apart from control signals and fsm signals as shared
		c.ioSet.foreach(io => {
			if (io.getName().contains("io_") && !(io.hasTag(publicInput))) io.addTag(share)
		})

		//Propagate the sharing through the design
		SpinalProgress(s"Propagate Sharing")
		c.orderNodes()
		//Mark all inputs/outputs of gadgets as shared
		c.children.foreach(child => if (child.isInstanceOf[BooleanGate]) {
			child.ioSet.foreach(io => {
				if (io.getName() != "control_clk" && io.getName() != "control_reset") {
					io.addTag(share)
				}
			})
		})
		c.propagateSharing()

		//Replace all abstract Gates with their masked versions
		c.instantiateGadgets()

		//Duplicate Declarations
		SpinalProgress(s"Declarations")
		duplicateDeclarations(c)

		//Duplicate Assignments
		SpinalProgress(s"Assignments")
		duplicateAssignments(c)
	}

	/**
	 * Traverse towards the primary inputs of a signal to infer its sharing
	 * @param bt Current signal
	 * @return Sharing of current signal
	 */
	def traverseTowardsInputs(bt : BaseType): Boolean = {
		var isShared = false

		val set = if (bt.component.asInstanceOf[Template].previousSignals.contains(bt)) bt.component.asInstanceOf[Template].previousSignals else bt.component.parent.asInstanceOf[Template].previousSignals

		//Set this signal as visited
		set(bt) = (true, set(bt)._2)

		//If any of the previous signals is shared, this signal has to be shared as well
		set(bt)._2.foreach(prev => {
			if (prev.hasTag(share)) isShared = true
		})

		//Traverse further towards inputs only if signal is not yet to be shared
		if (!isShared) {
			set(bt)._2.foreach(prev => {
				val prevSet = if (prev.component.asInstanceOf[Template].previousSignals.contains(prev)) prev.component.asInstanceOf[Template].previousSignals else if (prev.component.parent != null) prev.component.parent.asInstanceOf[Template].previousSignals else mutable.Map[BaseType, (Boolean, ArrayBuffer[BaseType])]()
				if (!prevSet.contains(prev)) {
					//isShared ||= prev.hasTag(share)
				}	else if (!(prevSet(prev)._1)) {
					//If previous signal is unvisited, traverse towards its inputs and infer if it is shared, and update tag if necessary
					val prevIsShared = traverseTowardsInputs(prev)
					if (prevIsShared) {
						prev.addTag(share)
						isShared = true
					}
				}	else {
					//If previous signal was already visited, simply update sharing information
					isShared ||= prev.hasTag(share)
				}
			})
		}

		isShared
	}

	/**
	 * Propagate sharing information through the design
	 */
	def propagateSharing(): Unit = {
		val c = this

		//For all signals, infer whether one of its predecessors is shared and update tag accordingly
		previousSignals.foreach(sig => {
			val isShared = traverseTowardsInputs(sig._1)
			if (isShared) sig._1.addTag(share)
		})

		//Recursive for all children
		c.children.foreach(child => child.asInstanceOf[Template].propagateSharing())
	}

	/**
	 * Order the signals in a template
	 */
	def orderNodes(): Unit = {
		val c = this

		c.dslBody.walkStatements({
			case d: DataAssignmentStatement => {
				//Add signal to Map if it is not in it yet
				if (!previousSignals.contains(d.finalTarget)) previousSignals += (d.finalTarget -> (false, ArrayBuffer[BaseType]()))

				//Add all signals that write onto the current signals into its list of predecessors
				d.walkDrivingExpressions {
					case bt: BaseType => {
						//Add signal to list of predecessors
						if (!previousSignals(d.finalTarget)._2.contains(bt)) previousSignals(d.finalTarget)._2 += bt
					}
					case b => {
						b.walkExpression {
							case bt: BaseType => if (!previousSignals(d.finalTarget)._2.contains(bt)) previousSignals(d.finalTarget)._2 += bt
							case _ =>
						}
					}
				}
			}
			case _ =>
		})

		c.children.foreach(child => child.asInstanceOf[Template].orderNodes())
	}

	def printSharing(c : Template): Unit = {
		println(c, c.randomness)
		c.children.foreach(child => child match {
			case t: Template => t.printSharing(t)
			case b: BlackBox =>
			case _ =>
		})
	}

	/**
	 * Duplicate an Expression
	 * @param e
	 * @param i
	 * @return
	 */
	def duplicateExpression(e : Expression, i : Int): Expression = {
		val expr = e match {
			//BaseType: Return the sharedSignal at index i
			case bt: BaseType => {
				bt.component.asInstanceOf[Template].sharedSignals.get(bt) match {
					case Some(s) => s(i)
					case None => {
						if (bt.component.isInstanceOf[BooleanGate]) {
							val newGate = bt.component.parent.asInstanceOf[Template].linkedGates(bt.component)
							val newBt = getIoFromName(s"${bt.getName().last}", newGate).asInstanceOf[Bits]
							val baf = new BitsBitAccessFixed
							baf.source = newBt
							baf.bitId = i
							baf
						} else {
							if (i == 0) bt
							else bt match {
								case b: Bool => new BoolLiteral(false)
								case b: Bits => {
									val bl = new BitsLiteral
									bl.value = 0
									bl.bitCount = b.getBitsWidth
									bl
								}
								case u: UInt => {
									val ul = new UIntLiteral
									ul.value = 0
									ul.bitCount = u.getBitsWidth
									ul
								}
								case s: SInt => {
									val sl = new SIntLiteral
									sl.value = 0
									sl.bitCount = s.getBitsWidth
									sl
								}
								case _ => SpinalError(s"Missing Case in duplicateExpresson: ${bt.getClass}"); bt
							}
						}
					}
				}
			}

			case b: BitsBitAccessFixed => {
				val bitAccess = new BitsBitAccessFixed
				bitAccess.bitId = b.bitId
				bitAccess.source = this.duplicateExpression(b.source, i).asInstanceOf[Expression with WidthProvider]
				bitAccess
			}

			case b: BitsBitAccessFloating => {
				val bitAccess = new BitsBitAccessFloating
				bitAccess.source = this.duplicateExpression(b.source, i).asInstanceOf[Expression with WidthProvider]
				bitAccess.bitId = b.bitId
				bitAccess
			}

			case x: Operator.Bool.Xor => {
				val xor = new Operator.Bool.Xor
				xor.left  = this.duplicateExpression(x.left, i).asInstanceOf[xor.T]
				xor.right = this.duplicateExpression(x.right, i).asInstanceOf[xor.T]
				xor
			}

			case a: Operator.Bool.And => {
				val ex = new Operator.Bool.And
				ex.left = this.duplicateExpression(a.left, i).asInstanceOf[ex.T]
				ex.right = this.duplicateExpression(a.right, i).asInstanceOf[ex.T]
				ex
			}

			case c: Operator.Bits.Cat => {
				val cat = new Operator.Bits.Cat
				cat.left = this.duplicateExpression(c.left, i).asInstanceOf[cat.T]
				cat.right = this.duplicateExpression(c.right, i).asInstanceOf[cat.T]
				cat
			}

			case b: Operator.Bool.Not => {
				val not = new Operator.Bool.Not
				not.source = duplicateExpression(b.source, i).asInstanceOf[not.T]
				not
			}

			case b: BitAssignmentFixed => {
				val baf = new BitAssignmentFixed
				baf.out = duplicateExpression(b.out, i).asInstanceOf[BitVector]
				baf.bitId = b.bitId
				baf
			}

			case b: BitAssignmentFloating => {
				val baf = new BitAssignmentFloating
				baf.out = duplicateExpression(b.out, i).asInstanceOf[BitVector]
				baf.bitId = b.bitId
				baf
			}

			case r: RangedAssignmentFixed => {
				val raf = new RangedAssignmentFixed
				raf.out = duplicateExpression(r.out, i).asInstanceOf[BitVector]
				raf.hi = r.hi
				raf.lo = r.lo
				raf
			}

			case b: BitsRangedAccessFixed => {
				val baf = new BitsRangedAccessFixed
				baf.source = duplicateExpression(b.source, i).asInstanceOf[Expression with WidthProvider]
				baf.hi = b.hi
				baf.lo = b.lo
				baf
			}

			case u: UIntBitAccessFixed => {
				val uaf = new UIntBitAccessFixed
				uaf.source = duplicateExpression(u.source, i).asInstanceOf[BitVector]
				uaf.bitId = u.bitId
				uaf
			}

			case u: UIntRangedAccessFixed => {
				val uaf = new UIntRangedAccessFixed
				uaf.source = duplicateExpression(u.source, i).asInstanceOf[BitVector]
				uaf.hi = u.hi
				uaf.lo = u.lo
				uaf
			}

			case b: BoolLiteral => {
				val bl = if (i == 0) new BoolLiteral(b.value) else new BoolLiteral(false)
				bl
			}

			case b: BitsLiteral => {
				val bl = new BitsLiteral
				bl.value = if (i == 0) b.value else 0
				bl.bitCount = b.bitCount
				bl
			}

			case u: UIntLiteral => {
				val ul = new UIntLiteral
				ul.value = if (i == 0) u.value else 0
				ul.bitCount = u.bitCount
				ul
			}

			case s: SIntLiteral => {
				val sl = new SIntLiteral
				sl.value = if (i == 0) s.value else 0
				sl.bitCount = s.bitCount
				sl
			}

			case c: CastBoolToBits => {
				val cast = new CastBoolToBits
				cast.input = duplicateExpression(c.input, i).asInstanceOf[cast.T]
				cast.inferredWidth = c.inferredWidth
				cast
			}

			case c: CastBitsToUInt => {
				val cast = new CastBitsToUInt
				cast.input = duplicateExpression(c.input, i).asInstanceOf[cast.T]
				cast.inferredWidth = c.inferredWidth
				cast
			}

			case c: CastUIntToBits => {
				val cast = new CastUIntToBits
				cast.input = duplicateExpression(c.input, i).asInstanceOf[cast.T]
				cast.inferredWidth = c.inferredWidth
				cast
			}

			case s: CastSIntToBits => {
				val cast = new CastSIntToBits
				cast.input = duplicateExpression(s.input, i).asInstanceOf[cast.T]
				cast.inferredWidth = s.inferredWidth
				cast
			}

			case u: ResizeUInt => {
				val ru = new ResizeUInt
				ru.input = duplicateExpression(u.input, i).asInstanceOf[Expression with WidthProvider]
				ru.size = u.size
				ru
			}

			case b: MultiplexerBits => {
				val mux = new MultiplexerBits
				val inputs = ArrayBuffer[mux.T]()
				b.inputs.foreach(in => inputs += duplicateExpression(in, i).asInstanceOf[mux.T])
				mux.inferredWidth = b.inferredWidth
				mux.inputs = inputs
				mux.select = duplicateExpression(b.select, i).asInstanceOf[mux.T]
				mux
			}

			case m: MultiplexerUInt => {
				val mux = new MultiplexerUInt
				val inputs = ArrayBuffer[mux.T]()
				m.inputs.foreach(in => inputs += duplicateExpression(in, i).asInstanceOf[mux.T])
				mux.inferredWidth = m.inferredWidth
				mux.inputs = inputs
				mux.select = duplicateExpression(m.select, i).asInstanceOf[mux.T]
				mux
			}

			case e => SpinalError(s"Missing case in function duplicateExpression: ${e} ${e.getClass}"); null
		}
		expr
	}

	/**
	 * Duplicate an assignment statement
	 * @param c Current component
	 * @tparam T
	 */
	def duplicateAssignments[T <: Template](c : T): Unit = {
		//println("duplicateAssignments")

		c.dslBody.walkStatements(s => s match {
			//Only DataAssignmentStatements are relevant
			case d: DataAssignmentStatement => {
				val finalTarget = d.finalTarget

				if (finalTarget.component.isInstanceOf[BooleanGate] && (finalTarget.getName() == "control_clk")) {
					val newBt = getIoFromName("clk", d.component.asInstanceOf[Template].linkedGates(finalTarget.component))
					val newAssignment = DataAssignmentStatement(newBt, d.source)
					d.insertNext(newAssignment)
					d.removeStatement()
				} else if (finalTarget.component.isInstanceOf[BooleanGate] && (finalTarget.getName() == "control_reset")) {
					val newBt = getIoFromName("reset", d.component.asInstanceOf[Template].linkedGates(finalTarget.component))
					val newAssignment = DataAssignmentStatement(newBt, d.source)
					d.insertNext(newAssignment)
					d.removeStatement()
				} else if (finalTarget.component.isInstanceOf[BooleanGate] && !finalTarget.hasTag(share)) {
					for (i <- 0 until gadgetConfig.numShares) {
						var source: Expression = null
						var target: Expression = null

						//Target in a gate
						if (finalTarget.component.isInstanceOf[BooleanGate]) {
							val newGate = d.component.asInstanceOf[Template].linkedGates(finalTarget.component)
							val newIo = getIoFromName(finalTarget.getName().last.toString, newGate).asInstanceOf[Bits]
							val baf = new BitAssignmentFixed
							baf.out = newIo
							baf.bitId = i
							target = baf
						} else {
							target = c.duplicateExpression(d.target, i)
						}

						//Source in a gate
						source = c.duplicateExpression(d.source, i)

						val da = if (source == null) DataAssignmentStatement(target, d.source) else DataAssignmentStatement(target, source)
						d.insertNext(da)
					}
					d.removeStatement()
				} else if (finalTarget.hasTag(share)) {
					for (i <- 0 until gadgetConfig.numShares) {
						var source: Expression = null
						var target: Expression = null

						//Target in a gate
						if (finalTarget.component.isInstanceOf[BooleanGate]) {
							val newGate = d.component.asInstanceOf[Template].linkedGates(finalTarget.component)
							val newIo = getIoFromName(finalTarget.getName().last.toString, newGate).asInstanceOf[Bits]
							val baf = new BitAssignmentFixed
							baf.out = newIo
							baf.bitId = i
							target = baf
						} else {
							target = c.duplicateExpression(d.target, i)
						}

						//Source in a gate
						source = c.duplicateExpression(d.source, i)

						val da = if (source == null) DataAssignmentStatement(target, d.source) else DataAssignmentStatement(target, source)
						d.insertNext(da)
					}
				} else {
					d.source match {
						case bt: BaseType if (bt.component.isInstanceOf[BooleanGate]) => {
							var source: Expression = null
							for (i <- 0 until gadgetConfig.numShares) {
								val newGate = d.component.asInstanceOf[Template].linkedGates(bt.component)
								val newIo = getIoFromName("z", newGate).asInstanceOf[Bits]
								if (i == 0) {
									val baf = new BitsBitAccessFixed
									baf.source = newIo
									baf.bitId = i
									source = baf
								} else {
									val baf = new BitsBitAccessFixed
									baf.source = newIo
									baf.bitId = i
									val xor = new Operator.Bool.Xor
									xor.left = source.asInstanceOf[xor.T]
									xor.right = baf.asInstanceOf[xor.T]
									source = xor
								}
							}

							val da = DataAssignmentStatement(d.target, source)
							d.insertNext(da)
							d.removeStatement()
						}
						case _ =>
					}
				}
			}
			case _ =>
		})

		c.children.foreach(child => if (!child.isInstanceOf[Gadget]) duplicateAssignments(child.asInstanceOf[Template]))
	}

	/**
	 * Duplicate all signal declarations in a component
	 * @param c The component
	 * @tparam T
	 */
	def duplicateDeclarations[T <: Template](c : T): Unit = {

		c.dslBody.walkDeclarations(d => d match {
			case bt: BaseType if (bt.hasTag(share) && !(bt.component.isInstanceOf[BooleanGate])) => {

				//Add a new entry in sharedSignals
				c.sharedSignals += (bt.asInstanceOf[Expression] -> ArrayBuffer[Expression]())

				//Create numShares instances of the same data type
				for (i <- 0 until gadgetConfig.numShares) {
					val newBt = bt match {
						case b: Bool => if(b.isReg) Reg(Bool()) else Bool()
						case b: Bits => if(b.isReg) Reg(Bits(b.getBitsWidth bits)) else Bits(b.getBitsWidth bits)
						case u: UInt => if(u.isReg) Reg(UInt(u.getBitsWidth bits)) else UInt(u.getBitsWidth bits)
						case s: SInt => if(s.isReg) Reg(SInt(s.getBitsWidth bits)) else SInt(s.getBitsWidth bits)
					}

					//Set the name to oldname_i and set direction if necessary
					newBt.setName(s"${bt.getName()}_s${i}", true)
					newBt.clockDomain = bt.clockDomain
					if (bt.isInput || bt.isOutput) {
						newBt.dir = bt.dir
						c.ioSet += newBt
					}

					//Add the new signal to the hashmap of bt
					c.sharedSignals(bt) += newBt

					//Insert the new declaration
					d.insertNext(newBt)
				}
			}
			case _ =>
		})

		//Initialization value for registers
		c.dslBody.walkStatements(s => s match {
			case i: InitAssignmentStatement if (c.sharedSignals.contains(i.target)) => {
				for (j <- 0 until gadgetConfig.numShares) {
					val init = new InitAssignmentStatement
					init.target = c.sharedSignals(i.target)(j)
					init.source = i.source
					c.sharedSignals(i.target)(j).asInstanceOf[BaseType].dlcAppend(init)
					c.dslBody.append(init)
				}
			}
			case _ =>
		})

		//Recursion
		c.children.foreach(child => if (!child.isInstanceOf[Gadget]) duplicateDeclarations(child.asInstanceOf[Template]))
	}

	def printAllTemplates[T <: Component](): Unit = {
		val c = this
		println(c, c.andCount)
		c.children.foreach(child => {
			child match {
				case g: BooleanGate =>
				case t: Template => {
					t.printAllTemplates()
				}
				case _ =>
			}
		})
	}

	/**
	 * Get an IO-signal based on its name
	 * @param name Name of the signal
	 * @param c Current component
	 * @return The found IO
	 */
	def getIoFromName (name : String, c : Component): BaseType = {
		var ret : BaseType = null
		c.ioSet.foreach(io => {
			if (io.getName() == name) ret = io
		})
		ret
	}

	/**
	 * Create a new connection to a sub-component and delete the old connection
	 * @param c The current component
	 * @param childIO Old input/output
	 * @param newIO New input/output
	 * @param isInput Determine whether an input or output is replaced
	 */
	def connectNewTarget(c : Component, childIO : BaseType, newIO : BaseType, isInput : Boolean): Unit = {
		c.dslBody.walkStatements(s => s match {
			case d: DataAssignmentStatement => {
				//Input connection
				if (isInput) {
					d.target match {
						case bt: BaseType if bt == childIO => {
							val newAssignment = newIO match {
								case b: Bool => DataAssignmentStatement(newIO, d.source)
								case b: BitVector => {
									val baf = new BitAssignmentFixed
									baf.out = b
									baf.bitId = childIO.getName().split("""_""").last.toInt
									DataAssignmentStatement(baf, d.source)
								}
							}
							//New statement with new target (in new sub-component) and same source
							newAssignment.parentScope = d.parentScope
							//Insert new statement and remove the old one
							d.insertNext(newAssignment)
							d.removeStatementFromScope()
						}
						case _ =>
					}
				}
				//Output connection
				else {
					d.source match {
						case bt: BaseType if bt == childIO => {
							//New statement with new source (in new sub-component) and same target
							val newAssignment = newIO match {
								case b: Bool => DataAssignmentStatement(d.target, newIO)
								case b: BitVector => {
									val baf = new BitsBitAccessFixed
									baf.source = b
									baf.bitId = childIO.getName().split("""_""").last.toInt
									DataAssignmentStatement(d.target, baf)
								}
							}
							newAssignment.parentScope = d.parentScope
							//Insert new statement and remove the old one
							d.insertNext(newAssignment)
							d.removeStatementFromScope()
						}
						case _ =>
					}
				}
			}
			case _ =>
		})
	}

	/**
	 * Give a new component the same connections that the old one had
	 * @param c Current component whose sub-component are being replaced
	 * @param child Child that is replaced
	 * @param newGate Replacement
	 * @param numInputs Determine whether it is a 2-input oder 1-input (NOT) gate
	 */
	def connectComponent(c : Component, child : Component, newGate : Component, numInputs : Int = 2): Unit = {
		//Names of the inputs in the unmasked and the masked gate/gadget
		val oldInputs = ArrayBuffer("control_clk", "control_reset")
		val newInputs = ArrayBuffer[String]()

		if (newGate.isInstanceOf[Template]) {
			newInputs ++= List("control_clk", "control_reset")
		} else {
			newInputs ++= List("clk", "reset")
		}

		for (i <- 0 until gadgetConfig.numShares) {
			oldInputs += s"io_x_${i}"
			newInputs += s"x"
		}

		if (numInputs == 2) {
			for (i <- 0 until gadgetConfig.numShares) {
				oldInputs += s"io_y_${i}"
				newInputs += s"y"
			}
		}

		//Outputs
		val oldOutputs = ArrayBuffer[String]()
		val newOutputs = ArrayBuffer[String]()

		for (i <- 0 until gadgetConfig.numShares) {
			oldOutputs += s"io_z_${i}"
			newOutputs += s"z"
		}

		//New input connections
		for (i <- oldInputs.indices) {
			val childIO = getIoFromName(oldInputs(i), child)
			val newIO = getIoFromName(newInputs(i), newGate)
			connectNewTarget(c, childIO, newIO, true)
		}

		//New output connections
		for (i <- oldOutputs.indices) {
			val childIO = getIoFromName(oldOutputs(i), child)
			val newIO = getIoFromName(newOutputs(i), newGate)
			connectNewTarget(c, childIO, newIO, false)
		}
	}

	/**
	 * Instantiate the masked gadgets. All unmasked, abstract boolean gates are replaced by the boolean gadgets specified in the Config
	 * @tparam T
	 */
	def instantiateGadgets[T <: Component](): Unit = {

		val c = this

		val oldGates, newGates = ArrayBuffer[Component]()

		c.children.foreach(child => child match {
			//If child is one of the three gate types, it has to be replaced
			case u: BooleanGate => {
				var newGate: Component = null
				u match {
					case a: And => {
						newGate = gadgetConfig.gadget.And().asInstanceOf[BlackBox]
						newGate.definitionName = s"${gadgetConfig.gadget.getName()}And"
					}
					case x: Xor => {
						newGate = gadgetConfig.gadget.Xor().asInstanceOf[BlackBox]
						newGate.definitionName = s"${gadgetConfig.gadget.getName()}Xor"
					}
					case n: Not => {
						newGate = gadgetConfig.gadget.Not().asInstanceOf[BlackBox]
						newGate.definitionName = s"${gadgetConfig.gadget.getName()}Not"
					}
				}
				//Keep the name
				newGate.setName(s"${child.getName()}_Masked")
				newGate.parentScope = child.parentScope
				linkedGates += (child -> newGate)

				oldGates += child
				newGates += newGate
				ScopeProperty.get.remove(DslScopeStack.asInstanceOf[ScopeProperty[Any]])
			}
			case _ =>
		})

		//Remove old, unmasked gates and add new, masked gates to this component's children
		oldGates.foreach(gate => c.children -= gate)
		newGates.foreach(gate => c.children += gate)

		c.children.foreach(child => if (!child.isInstanceOf[Gadget]) child.asInstanceOf[Template].instantiateGadgets())
	}

	//Toy function that prints all signals of a comnponent
	def getAllSignals[T <: Component](): Component = {
		
		val c = this
		
		c.dslBody.walkDeclarations(d => println(d, d.asInstanceOf[BaseType].hasTag(share)))
		c.children.foreach {
			case t: Template => t.getAllSignals()
			case _ =>
		}
		this
	}

	//Toy function to add one new input signal for each input signal
	def duplicateInputs[T <: Component](): Component = {
		println("duplicateInputs")
		val c = this

		c.dslBody.walkStatements(s => s match {
			case d: DeclarationStatement => {
				d match {
					case bt: BaseType => {
						if (bt.isInput) {
							val newBt = bt.clone()
							newBt.setName(s"${bt.getName()}_0")
							newBt.dir = in

							c.ioSet += newBt
							d.insertNext(newBt)
						}
					}
					case _ =>
				}
			}
			case _ =>
		})

		c
	}
}

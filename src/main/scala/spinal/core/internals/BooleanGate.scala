package spinal.core.internals

import spinal.core._
import spinal.lib.Counter

import scala.collection.mutable.Map

/**
 * Base Gate Trait
 */
trait Gate extends GlobalConfigUser {
	val gadgetConfig = globalConfig.gadgetConfig
}

trait Gadget extends Gate
trait MaskedGadget {
	def getName(): String
	def updateConfig(): Unit
	def And(): Gadget
	def Xor(): Gadget
	def Not(): Gadget
}

trait BooleanGate extends Gate

/**
 * Functions to instantiate boolean gadgets and delays
 */
trait BooleanGateUser extends Gate {

	val dryRun = GlobalConfig.dryRun

	val dummyBool = spinal.core.Reg(spinal.core.Bool())
	val dummyBits = spinal.core.Reg(spinal.core.Bits(0 bits))
	val dummyUInt = spinal.core.Reg(spinal.core.UInt(0 bits))
	val dummySInt = spinal.core.Reg(spinal.core.SInt(0 bits))
	val dummyCounter = spinal.lib.Counter(1)

	def Bool(): Bool = {
		if (!GlobalConfig.dryRun) spinal.core.Bool()
		else dummyBool
	}

	def Bits(): Bits = {
		if (!GlobalConfig.dryRun) spinal.core.Bits()
		else dummyBits
	}

	def Bits(w : BitCount): Bits = {
		if (!GlobalConfig.dryRun) spinal.core.Bits(w)
		else dummyBits.setWidth(w.value)
	}

	def UInt(): UInt = {
		if (!GlobalConfig.dryRun) spinal.core.UInt()
		else dummyUInt
	}

	def UInt(w : BitCount): UInt = {
		if (!GlobalConfig.dryRun) spinal.core.UInt(w)
		else dummyUInt.setWidth(w.value)
	}

	def SInt(): SInt = {
		if (!GlobalConfig.dryRun) spinal.core.SInt()
		else dummySInt
	}

	def SInt(w : BitCount): SInt = {
		if (!GlobalConfig.dryRun) spinal.core.SInt(w)
		else dummySInt.setWidth(w.value)
	}

	def PubRegNext[T <: Data](next : T): T = {
		if (!GlobalConfig.dryRun) spinal.core.RegNext(next)
		else PubReg(next)
	}

	def PubReg[T <: Data](dataType : T): T = {
		if (!GlobalConfig.dryRun) spinal.core.Reg(dataType)
		else {
			dataType match {
				case b: Bool => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += 1
					dummyBool.asInstanceOf[T]
				}
				case b: Bits => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += b.fixedWidth
					dummyBits.asInstanceOf[T]
				}
				case s: SInt => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += s.fixedWidth
					dummySInt.asInstanceOf[T]
				}
				case u: UInt => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += u.fixedWidth
					dummyUInt.asInstanceOf[T]
				}
				case e: SpinalEnumCraft[SpinalEnum] => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += e.getBitsWidth
					spinal.core.Reg(dataType)
				}
				case _ =>	dummyBits.asInstanceOf[T]
			}
		}
	}

	def Reg[T <: Data](dataType: T): T = {
		if (!GlobalConfig.dryRun) spinal.core.Reg(dataType)
		else {
			dataType match {
				case b: Bool => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += gadgetConfig.numShares
					dummyBool.asInstanceOf[T]
				}
				case b: Bits => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += (b.fixedWidth) * gadgetConfig.numShares
					dummyBits.asInstanceOf[T]
				}
				case s: SInt => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += (s.fixedWidth) * gadgetConfig.numShares
					dummySInt.asInstanceOf[T]
				}
				case u: UInt => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += (u.fixedWidth) * gadgetConfig.numShares
					dummyUInt.asInstanceOf[T]
				}
				case e: SpinalEnumCraft[SpinalEnum] => {
					DslScopeStack.get.component.asInstanceOf[Template].regCount += (e.getBitsWidth) * gadgetConfig.numShares
					spinal.core.Reg(dataType)
				}
				case _ =>	dummyBits.asInstanceOf[T]
			}
		}
	}

	object Counter {
		def apply(stateCount : BigInt): Counter = {
			if (!GlobalConfig.dryRun) spinal.lib.Counter(stateCount)
			else {
				DslScopeStack.get.component.asInstanceOf[Template].regCount += log2Up(stateCount)
				dummyCounter
			}
		}

		def apply(start: BigInt,end: BigInt): Counter = {
			if (!GlobalConfig.dryRun) spinal.lib.Counter(start, end)
			else {
				DslScopeStack.get.component.asInstanceOf[Template].regCount += log2Up(end+1)
				dummyCounter
			}
		}
	}

	def RegNext[T <: Data](next : T, init : T = null.asInstanceOf[T]): T = {
		if (!GlobalConfig.dryRun) spinal.core.RegNext(next, init)
		else {
			Reg(next)
		}
	}

	def Xor(z : Bool, x : Bool, y : Bool): Unit = {
		if (!GlobalConfig.dryRun) {
			val xorGate = new Xor(gadgetConfig.latencyXor)
			xorGate.control.clk := x.component.asInstanceOf[Template].control.clk
			xorGate.control.reset := x.component.asInstanceOf[Template].control.reset
			xorGate.io.x := x
			xorGate.io.y := y
			z := xorGate.io.z
			val xorCount = DslScopeStack.get.component.asInstanceOf[Template].xorCount
			xorGate.setName(s"xor_${xorCount}", weak = true)
			DslScopeStack.get.component.asInstanceOf[Template].xorCount += 1
		} else {
			DslScopeStack.get.component.asInstanceOf[Template].xorCount += 1
		}
	}

	def Xor(x : Bool, y : Bool): Bool = {
		if (!GlobalConfig.dryRun) {
			val xorGate = new Xor(gadgetConfig.latencyXor)
			xorGate.control.clk := x.component.asInstanceOf[Template].control.clk
			xorGate.control.reset := x.component.asInstanceOf[Template].control.reset
			xorGate.io.x := x
			xorGate.io.y := y
			val xorCount = DslScopeStack.get.component.asInstanceOf[Template].xorCount
			//Set wire name to xor_{xorCount}
			xorGate.setName(s"xor_${xorCount}", weak = true)
			//Update xorCount of the Template
			DslScopeStack.get.component.asInstanceOf[Template].xorCount += 1
			xorGate.io.z
		} else {
			DslScopeStack.get.component.asInstanceOf[Template].xorCount += 1
			dummyBool
		}
	}

	def XorDelay(z : Bool, x : Bool): Unit = {
		Delay(z, x, gadgetConfig.latencyXor)
	}

	def XorDelay(x : Bool): Bool = {
		this.Delay(x, gadgetConfig.latencyXor)
	}

	def And(z : Bool, x : Bool, y : Bool): Unit = {
		if (!GlobalConfig.dryRun) {
			val andGate = new And(gadgetConfig.latencyAnd)
			andGate.control.clk := x.component.asInstanceOf[Template].control.clk
			andGate.control.reset := x.component.asInstanceOf[Template].control.reset
			andGate.io.x := x
			andGate.io.y := y
			z := andGate.io.z
			val andCount = DslScopeStack.get.component.asInstanceOf[Template].andCount
			andGate.setName(s"and_${andCount}", weak = true)
			DslScopeStack.get.component.asInstanceOf[Template].andCount += 1
		} else {
			DslScopeStack.get.component.asInstanceOf[Template].andCount += 1
		}
	}

	def And(x : Bool, y : Bool): Bool = {
		if (!GlobalConfig.dryRun) {
			val andGate = new And(gadgetConfig.latencyAnd)
			andGate.control.clk := x.component.asInstanceOf[Template].control.clk
			andGate.control.reset := x.component.asInstanceOf[Template].control.reset
			andGate.io.x := x
			andGate.io.y := y
			val andCount = DslScopeStack.get.component.asInstanceOf[Template].andCount
			//Set wire name to and_{andCount}
			andGate.setName(s"and_${andCount}", weak = true)
			//Update andCount of the Template
			DslScopeStack.get.component.asInstanceOf[Template].andCount += 1
			andGate.io.z
		} else {
			DslScopeStack.get.component.asInstanceOf[Template].andCount += 1
			dummyBool
		}
	}

	def AndDelay(z : Bool, x : Bool): Unit = {
		Delay(z, x, gadgetConfig.latencyAnd)
	}

	def AndDelay(x : Bool): Bool = {
		this.Delay(x, gadgetConfig.latencyAnd)
	}

	def Not(z : Bool, x : Bool): Unit = {
		if (!GlobalConfig.dryRun) {
			val notGate = new Not(gadgetConfig.latencyNot)
			notGate.control.clk := x.component.asInstanceOf[Template].control.clk
			notGate.control.reset := x.component.asInstanceOf[Template].control.reset
			notGate.io.x := x
			z := notGate.io.z
			val notCount = DslScopeStack.get.component.asInstanceOf[Template].notCount
			notGate.setName(s"not_${notCount}", weak = true)
			DslScopeStack.get.component.asInstanceOf[Template].notCount += 1
		} else {
			DslScopeStack.get.component.asInstanceOf[Template].notCount += 1
		}
	}

	def Not(x : Bool): Bool = {
		if (!GlobalConfig.dryRun) {
			val notGate = new Not(gadgetConfig.latencyNot)
			notGate.control.clk := x.component.asInstanceOf[Template].control.clk
			notGate.control.reset := x.component.asInstanceOf[Template].control.reset
			notGate.io.x := x
			val notCount = DslScopeStack.get.component.asInstanceOf[Template].notCount
			//Set wire name to not_{notCount}
			notGate.setName(s"not_${notCount}", weak = true)
			//Update notCount of the Template
			DslScopeStack.get.component.asInstanceOf[Template].notCount += 1
			notGate.io.z
		} else {
			DslScopeStack.get.component.asInstanceOf[Template].notCount += 1
			dummyBool
		}
	}

	def NotDelay(z : Bool, x : Bool): Unit = {
		Delay(z, x, gadgetConfig.latencyNot)
	}

	def NotDelay(x : Bool): Bool = {
		this.Delay(x, gadgetConfig.latencyNot)
	}

	def Delay(z : Bool, x : Bool, n : Int): Unit = {
		if (!GlobalConfig.dryRun) {
			z := Delay(x, n)
		} else {
			DslScopeStack.get.component.asInstanceOf[Template].regCount += n
		}
	}

	def Delay(x : Bool, n : Int): Bool = {
		//If n = 0, no register is instantiated and the wire is simply returned
		if (!GlobalConfig.dryRun) {
			if (n == 0) {
				x
			} else {
				//Else instantiate a vector of n elements and move data up one spot each cycle
				val delayGate = Vec(Reg(Bool()) init (False), n)
				if (x.getName() == "") delayGate.setName(s"reg", weak = true)
				else if (!x.getName().startsWith("reg")) delayGate.setName(s"reg_${x.getName()}", weak = true)
				else delayGate.setName(x.getName(), weak = true)
				//Increment the template's delay count by n
				DslScopeStack.get.component.asInstanceOf[Template].regCount += n * gadgetConfig.numShares
				delayGate(0) := x
				for (i <- 1 until n) delayGate(i) := delayGate(i - 1)
				delayGate(n - 1)
			}
		} else {
			if (n == 0) dummyBool
			else {
				DslScopeStack.get.component.asInstanceOf[Template].regCount += n * gadgetConfig.numShares
				dummyBool
			}
		}
	}

	def Delay(x : Bits, n : Int) : Bits = {
		if (!GlobalConfig.dryRun) {
			if (n == 0) x else {
				val delayRegs = Vec(Reg(Bits(x.fixedWidth bits)) init (0), n)
				if (x.getName() == "") delayRegs.setName(s"reg", weak = true)
				else if (!x.getName().startsWith("reg")) delayRegs.setName(s"reg_${x.getName()}", weak = true)
				else delayRegs.setName(x.getName(), weak = true)

				DslScopeStack.get.component.asInstanceOf[Template].regCount += x.fixedWidth * n * gadgetConfig.numShares
				delayRegs(0) := x
				for (i <- 1 until n) delayRegs(i) := delayRegs(i - 1)
				delayRegs.last
			}
		} else {
			if (n == 0) dummyBits else {
				DslScopeStack.get.component.asInstanceOf[Template].regCount += x.fixedWidth * n * gadgetConfig.numShares
				dummyBits
			}
		}
	}
}

/**
 * Unmasked Xor gadget
 */
class Xor(n : Int = 0) extends Template with BooleanGate {
	val io = new Bundle {
		val x = in  Bool()
		val y = in  Bool()
		val z = out Bool()
	}

  override def instantiate(): Boolean = true
  val z = io.x ^ io.y
  io.z := Delay(z, n)
}

/**
 * Unmasked And gadget
 */
class And(n : Int = 0) extends Template with BooleanGate {
	val io = new Bundle {
		val x = in  Bool()
		val y = in  Bool()
		val z = out Bool()
	}

  override def instantiate(): Boolean = true
	val z = io.x & io.y
	io.z := Delay(z, n)
}

/**
 * Unmasked Not gadget
 */
class Not(n : Int = 0) extends Template with BooleanGate {
	val io = new Bundle {
		val x = in  Bool()
		val z = out Bool()
	}

  override def instantiate(): Boolean = true
	val z = ~io.x
	io.z := Delay(z, n)
}

/**
 * Delay gadget
 * @param n Delay count, default = 0
 */
class Delay(n : Int = 0) extends Template with BooleanGate {
	val io = new Bundle {
		val x = in  Bool()
		val z = out Bool()
	}

	override def instantiate(): Boolean = true
	//If delay is 0, simply return input
	if (n == 0) io.z := io.x
	//otherwise, create a vector of n registers and move data up one spot each cycle, return value of final register
	else {
		val reg = Vec(Reg(Bool()) init(False), n)
		reg(0) := io.x
		for (i <- 1 until n) reg(i) := reg(i - 1)
		io.z := reg(n - 1)
	}
}
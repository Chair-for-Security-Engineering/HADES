import spinal.core._
import spinal.core.internals._
import spinal.lib.Counter

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.annotation.varargs

abstract class Adder extends Template {

  val width : Int
  val pipelined : Boolean
  val carryOut : Boolean
  val carryIn : Boolean

	//Input
	val io = new Bundle {
		val x 	= in  Bits(width bits)
		val y 	= in  Bits(width bits)
		val cIn = in  Bool()
		val s 	= out Bits(width+1 bits)
	}

	//Enable and done are only instantiated in a serial design
	val fsm = new Bundle {
		val enable 	= in  Bool()
		val ready 	= out Bool()
	}

  override def instantiate(): Boolean = false
}

abstract class AdderFactory extends TemplateFactory {
  type T <: Adder

  override def createTemplate(): T = ???
  def createTemplate(width : Int, pipelined : Boolean, carryOut : Boolean, carryIn : Boolean = false): Adder
  def getName(): String
}

case class SklanskyAdderFactory() extends AdderFactory {
  type T = SklanskyAdder

  override def createTemplate(width : Int, pipelined : Boolean, carryOut : Boolean, carryIn : Boolean): T = SklanskyAdder(width, pipelined, carryOut, carryIn)
  override def getName(): String = "SklanskyAdder"
}

case class KoggeStoneAdderFactory() extends AdderFactory {
  type T = KoggeStoneAdder

  override def createTemplate(width : Int, pipelined : Boolean, carryOut : Boolean, carryIn : Boolean): T = KoggeStoneAdder(width, pipelined, carryOut, carryIn)
  override def getName(): String = "KoggeStoneAdder"
}

case class RippleCarryAdderFactory() extends AdderFactory {
  type T = RippleCarryAdder

  override def createTemplate(width : Int, pipelined : Boolean, carryOut : Boolean, carryIn : Boolean): T = RippleCarryAdder(width, pipelined, carryOut, carryIn)
  override def getName(): String = "RippleCarryAdder"
}

object AllAdderFactories extends Configurable {
  parameters = List(
    SklanskyAdderFactory(),
    KoggeStoneAdderFactory(),
    RippleCarryAdderFactory()
  )
}

/**
 * Full Adder
 */
case class FullAdder(width : Int, pipelined : Boolean, carryOut : Boolean, carryIn : Boolean = true) extends Adder {

  this.io.s := 0

  override def instantiate(): Boolean = {
    fsm.ready := False
    val xPlusY, cIn, s, cInPlusXPlusY, xAndY, xAndYD = Bool()

    xPlusY := Xor(io.x(0), io.y(0))
    cIn := XorDelay(io.cIn)
    s := Xor(xPlusY, cIn)
    io.s(0) := AndDelay(s)

    cInPlusXPlusY := And(xPlusY, cIn)
    xAndY := And(io.x(0), io.y(0))
    xAndYD := XorDelay(xAndY)
    io.s(1) := Xor(cInPlusXPlusY, xAndYD)

    this.latency = 2*latencyXor + latencyAnd
    true
  }
}

/**
 * Ripple-Carry-Adder as a single Full-Adder
 */
case class RippleCarryAdder(width : Int = 32, pipelined : Boolean = true, carryOut : Boolean = true, carryIn : Boolean = true) extends Adder {

	//States for the StateMachine of the serial RCA
	object RCAState extends SpinalEnum {
		val sIdle, sInit, sExecute, sDone = newElement()
	}
	import RCAState._

  override def instantiate(): Boolean = {
    //Pipelined design
    var ret = true

    if (pipelined) {
      if (globalConfig.gadgetConfig.securityOrder == 0) return false
      val x, y, s = Vec(Bool(), width)

      //Instantiate width many FullAdders
      val fulladder = instantiateMultipleSubTemplates(width)(FullAdder(1, false, true))
      fulladder.foreach(ret &&= _.instantiateTemplate())

      for (i <- 0 until width) {
        //Delay x and y i times the FullAdder latency
        Delay(x(i), io.x(i), i*fulladder(i).latency)
        Delay(y(i), io.y(i), i*fulladder(i).latency)

        //Delay the results of each full-adder to make all result bits synchronous.
        //Final bit needs no delay, first bit gets (width-1)*FullAdder latency delays
        Delay(s(i), fulladder(i).io.s(0), (width-i-1)*fulladder(i).latency)
      }

      if (!GlobalConfig.dryRun) {
        fsm.ready := True
        for (i <- 0 until width) {
          //Connect clock and reset to all FullAdders
          fulladder(i).control.clk := control.clk
          fulladder(i).control.reset := control.reset

          fulladder(i).io.x(0) := x(i)
          fulladder(i).io.y(0) := y(i)

          //First FullAdder gets io.cIn as carry-in, the others get the previous carry-out
          if (i == 0) fulladder(i).io.cIn := io.cIn
          else fulladder(i).io.cIn := fulladder(i - 1).io.s(1)

          //Write delayed bits into result
          io.s(i) := s(i)
        }

        //The carry-out bit is the carry-out of the last FullAdder
        io.s(width) := fulladder(width - 1).io.s(1)
      }

      //Set the latency
      this.latency = width * fulladder(0).latency
    }
    //Serial design
    else {
      //Registers to store the inputs
      val x, y = Reg(Bits(width bits)) init(0)

      //Register for the result
      val s = Reg(Bits(width+1 bits)) init(0)

      //State for the state machine
      val state = PubReg(RCAState()) init(sIdle)
      state.setName(s"state", weak = true)

      val initializeData = Bool()
      val rotateOutput = Bool()
      val rotateInputs = Bool()

      //One Full-Adder for all calculations
      val fulladder = new FullAdder(1, false, true)
      ret &&= fulladder.instantiateTemplate()
      fulladder.io.x.addTag(share)
      val widthCounter = Counter(width)
      val adderCounter = Counter(0, fulladder.latency)

      if (!GlobalConfig.dryRun) {
        initializeData := False
        rotateOutput := False
        rotateInputs := False

        fulladder.control.clk := control.clk
        fulladder.control.reset := control.reset
        fulladder.io.x(0) := x(0)
        fulladder.io.y(0) := y(0)
        fulladder.io.cIn := s(width)

        //Carry-out of the FullAdder is always written into the leftmost bit of s
        s(width) := fulladder.io.s(1)

        //Write s to the output
        io.s := s
        fsm.ready := True

        //Initialize x and y registers with the two inputs, store the input carry in the leftmost bit of s
        when(initializeData) {
          for (i <- 0 until width) {
            x(i) := io.x(i)
            y(i) := io.y(i)
          }
          s(width) := io.cIn
        }

        //Rotate x and y one position to the right so that we can always take the rightmost bit as input for the FullAdder
        when(rotateInputs) {
          x := x.rotateRight(1)
          y := y.rotateRight(1)
        }

        //Rotate s and update with the output bit of the FullAdder
        when(rotateOutput) {
          s(width - 1) := fulladder.io.s(0)
          s(width - 2 downto 0) := s(width - 1 downto 1)
        }

        //State Machine
        switch(state) {
          //Idle/Reset state
          is(sIdle) {
            state := sIdle
            adderCounter.clear()
            widthCounter.clear()
            x := 0
            y := 0
            s := 0
            when(fsm.enable) {
              state := sInit
            }
          }

          //Initialization of registers with input data
          is(sInit) {
            fsm.ready := False
            initializeData := True
            state := sExecute
          }

          //Addition
          is(sExecute) {
            fsm.ready := False
            state := sExecute

            adderCounter.increment()
            when(adderCounter.willOverflow) {
              rotateOutput := True
            }

            when(adderCounter.willOverflow) {
              rotateInputs := True
              widthCounter.increment()
            }

            when(widthCounter.willOverflow) {
              state := sDone
            }
          }

          //Done state
          is(sDone) {
            state := sDone
            adderCounter.clear()
            widthCounter.clear()
            when(fsm.enable) {
              state := sInit
            }
          }
        }
      }

      //Set the latency of this template to width * (FullAdder latency) + 3 additional cycles
      this.latency = width * (fulladder.latency+1) + 2
    }
    ret
  }
}

/**
 * Preprocessing unit for Parallel-Prefix Adders
 */
case class PreprocessingUnit(pOnly : Boolean = false) extends Template {

	val io = new Bundle {
		val a = in  Bool()
		val b = in  Bool()
		val p = out Bool()
		val g = out Bool()
	}

  override def instantiate(): Boolean = {

    val p, g = Bool()
    //p = a ^ b, g = a & b
    p := Xor(io.a, io.b)
    if (!pOnly) {
      g := And(io.a, io.b)
    } else {
      if (!GlobalConfig.dryRun) g := False
    }

    //Delay outputs depending on which is larger, latencyAnd or latencyXor
    if (latencyAnd > latencyXor) {
      io.p := Delay(p, (latencyAnd-latencyXor))
      if (!GlobalConfig.dryRun) io.g := g
    } else if (latencyXor > latencyAnd) {
      if (!GlobalConfig.dryRun) io.p := p
      if (!pOnly) {
        io.g := Delay(g, latencyXor-latencyAnd)
      } else {
        if (!GlobalConfig.dryRun) io.g := False
      }
    } else {
      if (!GlobalConfig.dryRun) {
        io.p := p
        io.g := g
      }
    }

    //Latency is the maximum of latencyAnd and latencyXor
    this.latency = math.max(latencyAnd, latencyXor)
    true
  }
}

/**
 * Generate-Propagate Unit
 */
case class GeneratePropagate(gOnly : Boolean = false) extends Template {

	val io = new Bundle {
		val p 		= in  Bool()
		val pPrev = in  Bool()
		val g 		= in  Bool()
		val gPrev = in  Bool()
		val pOut  = out Bool()
		val gOut 	= out Bool()
	}

  override def instantiate(): Boolean = {
    //pOut := p & pPrev
    val p, pg, g = Bool()
    if (!gOnly) {
      p := And(io.p, io.pPrev)
      io.pOut := XorDelay(p)
    } else {
      if (!GlobalConfig.dryRun) io.pOut := False
    }

    pg := And(io.p, io.gPrev)
    g := AndDelay(io.g)
    io.gOut := Xor(g, pg)

    //Latency is one Xor and one And
    this.latency = latencyAnd + latencyXor
    true
  }
}

/**
 * Kogge-Stone Adder
 */
case class KoggeStoneAdder(width : Int, pipelined : Boolean = true, carryOut : Boolean = false, carryIn : Boolean = false) extends Adder {

	//Number necessary Generate-Propagate stages
	val numStages = log2Up(width)

  val co = if (carryOut) 0 else 1
	var pgLatency = 0
  var preprocessing0Latency = if (carryIn) scala.math.max(2 * latencyNot + 3 * latencyAnd, latencyXor) else 0

  override def instantiate(): Boolean = {
    var ret = true
    var k = 1

    //Generate-Propagate signals
    val p, g = Vec(Vec(Bool(), width), numStages+1)
    val pFinal = Vec(Bool(), width)

    //Generated carry-signals and result signal
    val c = Vec(Bool(), width)
    val s = Vec(Bool(), width+1)

    //Preprocessing Units
    val numPreprocessingUnits = if (carryIn) width - 2 else width - 1
    val preprocessingUnits = instantiateMultipleSubTemplates(numPreprocessingUnits)(PreprocessingUnit(pOnly = false))
    preprocessingUnits ++= instantiateMultipleSubTemplates(1)(PreprocessingUnit(pOnly = !carryOut))
    preprocessingUnits.foreach(ret &&= _.instantiateTemplate())

    io.s := 0
    fsm.ready := True

    if (pipelined) {

      //Feed preprocessor with input values, write outputs into p0 and g0
      if (!GlobalConfig.dryRun) {
        if (carryIn) {
          // The first p and g values are computed differently by taking the input carry into account
          p(0)(0) := Delay(Xor(io.x(0), io.y(0)), scala.math.max(2 * latencyNot + 3 * latencyAnd - latencyXor, 0))
          val g0a = And(io.x(0), io.cIn)
          val g0b = And(io.y(0), io.cIn)
          val g0c = And(io.x(0), io.y(0))
          g(0)(0) := Delay(Not(And(And(Not(g0a), Not(g0b)), AndDelay(Not(g0c)))), scala.math.max(latencyXor - (2 * latencyNot + 3 * latencyAnd), 0))
        }

        for (i <- 0 until numPreprocessingUnits + 1) {
          val idx = if (carryIn) i + 1 else i
          val requiredPreprocessingDelay = if (carryIn) scala.math.max(preprocessing0Latency - preprocessingUnits(0).latency, 0) else 0
          preprocessingUnits(i).control.clk := control.clk
          preprocessingUnits(i).control.reset := control.reset
          preprocessingUnits(i).io.a := io.x(idx)
          preprocessingUnits(i).io.b := io.y(idx)
          p(0)(idx) := Delay(preprocessingUnits(i).io.p, requiredPreprocessingDelay)
          g(0)(idx) := Delay(preprocessingUnits(i).io.g, requiredPreprocessingDelay)
        }
      }

      for (i <- 0 until numStages) {
        //Instantiate Generate-Propagate units (width-k many, where the first k do not need to compute p)
        val pg = instantiateMultipleSubTemplates(k)(GeneratePropagate(gOnly = true))
        pg ++= instantiateMultipleSubTemplates(width - 2*k - co)(GeneratePropagate(gOnly = false))
        pg.foreach(ret &&= _.instantiateTemplate())
        pgLatency = pg(0).latency

        //The rightmost k bits are simply delayed into the next stage
        for (j <- 0 until k) {
          p(i+1)(j) := Delay(p(i)(j), pg(0).latency)
          g(i+1)(j) := Delay(g(i)(j), pg(0).latency)
        }

        //The remaining bits get a Generate-Propagate unit that combines the current bit and the bit k positions to the right (for k = 1,2,4,8,...)
        if (!GlobalConfig.dryRun) {
          for (j <- k until width - co) {
            pg(j - k).control.clk := control.clk
            pg(j - k).control.reset := control.reset
            pg(j - k).io.p := p(i)(j)
            pg(j - k).io.pPrev := p(i)(j - k)
            pg(j - k).io.g := g(i)(j)
            pg(j - k).io.gPrev := g(i)(j - k)
            p(i + 1)(j) := pg(j - k).io.pOut
            g(i + 1)(j) := pg(j - k).io.gOut
          }
          for (j <- width - co until width) { // uppermost p,g set to zero if carryOut == true
            p(i + 1)(j) := False
            g(i + 1)(j) := False
          }
        }

        //Left shift of k by one position
        k = k << 1
      }

      //Delay the first p-signal through the numStages stages for the output calculation
      for (i <- 0 until width) pFinal(i) := Delay(p(0)(i), numStages*pgLatency)

      //Carry-bits are the last g-values
      //Output calculated as Xor of carry-bits and the first p-signal
      for (i <- 0 until width) {
        if (!GlobalConfig.dryRun) c(i) := g(numStages)(i)
        if (i == 0) {
          if (carryIn) {
            s(i) := Xor(pFinal(i), io.cIn)
          } else {
            s(i) := XorDelay(pFinal(i))
          }
        }
        else s(i) := Xor(pFinal(i), c(i-1))
      }

      //Leftmost bit of s is the last carry-bit
      if (carryOut) {
        s(width) := XorDelay(c(width-1))
      } else {
        if (!GlobalConfig.dryRun) s(width) := False
      }

      //Write the output
      if (!GlobalConfig.dryRun)  {
        for (i <- 0 until width + 1) {
          io.s(i) := s(i)
        }
      }

      //Latency: Preprocessing + Generate-Propagate stages + final XOR
      this.latency = scala.math.max(preprocessingUnits(0).latency, preprocessing0Latency) + numStages*pgLatency + latencyXor
      ret 
    } else {
      false
    }
  }
}

/**
 * Sklansky adder
 */
case class SklanskyAdder(width : Int, pipelined : Boolean = true, carryOut : Boolean = false, carryIn : Boolean = false) extends Adder {

	//Number necessary Generate-Propagate stages
	val numStages = log2Up(width)

  var k = width >> 1
  val co = if (carryOut) 0 else 1
	var pgLatency = 0
  var preprocessing0Latency = if (carryIn) scala.math.max(2 * latencyNot + 3 * latencyAnd, latencyXor) else 0

  override def instantiate(): Boolean = {
    //Generate-Propagate signals
    val p, g = Vec(Vec(Bool(), width), numStages+1)
    val pFinal = Vec(Bool(), width)

    //Generated carry-signals and result signal
    val c = Vec(Bool(), width)
    val s = Vec(Bool(), width+1)

    //Preprocessing Units
    var ret = true
    val numPreprocessingUnits = if (carryIn) width - 2 else width - 1
    val preprocessingUnits = instantiateMultipleSubTemplates(numPreprocessingUnits)(PreprocessingUnit(pOnly = false))
    preprocessingUnits ++= instantiateMultipleSubTemplates(1)(PreprocessingUnit(pOnly = !carryOut))
    preprocessingUnits.foreach(ret &&= _.instantiateTemplate())

    io.s := 0
    fsm.ready := True

    if (pipelined) {

      //Feed preprocessor with input values, write outputs into p0 and g0
      if (!GlobalConfig.dryRun) {
        if (carryIn) {
          // The first p and g values are computed differently by taking the input carry into account
          p(0)(0) := Delay(Xor(io.x(0), io.y(0)), scala.math.max(2 * latencyNot + 3 * latencyAnd - latencyXor, 0))
          val g0a = And(io.x(0), io.cIn)
          val g0b = And(io.y(0), io.cIn)
          val g0c = And(io.x(0), io.y(0))
          g(0)(0) := Delay(Not(And(And(Not(g0a), Not(g0b)), AndDelay(Not(g0c)))), scala.math.max(latencyXor - (2 * latencyNot + 3 * latencyAnd), 0))
        }

        for (i <- 0 until numPreprocessingUnits + 1) {
          val idx = if (carryIn) i + 1 else i
          val requiredPreprocessingDelay = if (carryIn) scala.math.max(preprocessing0Latency - preprocessingUnits(0).latency, 0) else 0
          preprocessingUnits(i).control.clk := control.clk
          preprocessingUnits(i).control.reset := control.reset
          preprocessingUnits(i).io.a := io.x(idx)
          preprocessingUnits(i).io.b := io.y(idx)
          p(0)(idx) := Delay(preprocessingUnits(i).io.p, requiredPreprocessingDelay)
          g(0)(idx) := Delay(preprocessingUnits(i).io.g, requiredPreprocessingDelay)
        }
      }

      for (i <- 0 until numStages) {
        val mod = 1 << (i + 1)
        val thresh = mod >> 1
        var cnt = 0

        //Instantiate Generate-Propagate units (k many, where the first thresh ones only need to compute g)
        val pg = instantiateMultipleSubTemplates(math.min(    thresh,      width - thresh))(GeneratePropagate(gOnly = true))
        if ((width%2) == 0) {
          pg ++= instantiateMultipleSubTemplates(math.max(k - thresh - co, 0             ))(GeneratePropagate(gOnly = false))
        } else {
          pg ++= instantiateMultipleSubTemplates(math.max(k - thresh,      0             ))(GeneratePropagate(gOnly = false))
        }
        pg.foreach(ret &&= _.instantiateTemplate())
        pgLatency = pg(0).latency

        for (j <- 0 until width-co) {
          //All bits that are < 2^i mod 2^(i+1) are delayed
          if ((j % mod) < thresh) {
            p(i+1)(j) := Delay(p(i)(j), pg(0).latency)
            g(i+1)(j) := Delay(g(i)(j), pg(0).latency)
          } else // All other bits have a new pg block
          {
            if (!GlobalConfig.dryRun) {
              val prev = (j / mod) * mod + thresh - 1
              pg(cnt).control.clk := control.clk
              pg(cnt).control.reset := control.reset
              pg(cnt).io.p := p(i)(j)
              pg(cnt).io.pPrev := p(i)(prev)
              pg(cnt).io.g := g(i)(j)
              pg(cnt).io.gPrev := g(i)(prev)
              p(i + 1)(j) := pg(cnt).io.pOut
              g(i + 1)(j) := pg(cnt).io.gOut
              cnt = cnt + 1
            }
          }
        }
        for (j <- width-co until width) { // uppermost p,g set to zero of delay if carryOut == true
          if ((j % mod) < thresh) {
            p(i+1)(j) := Delay(p(i)(j), pg(0).latency)
            g(i+1)(j) := Delay(g(i)(j), pg(0).latency)
          } else // All other bits have NO new pg block
          {
            if (!GlobalConfig.dryRun) {
              p(i + 1)(j) := False
              g(i + 1)(j) := False
            }
          }
        }
      }

      //Delay the first p-signal through the numStages stages for the output calculation
      for (i <- 0 until width) pFinal(i) := Delay(p(0)(i), numStages*pgLatency)

      //Carry-bits are the last g-values
      //Output calculated as Xor of carry-bits and the first p-signal
      for (i <- 0 until width) {
        if (!GlobalConfig.dryRun) c(i) := g(numStages)(i)
        if (i == 0) {
          if (carryIn) {
            s(i) := Xor(pFinal(i), io.cIn)
          } else {
            s(i) := XorDelay(pFinal(i))
          }
        }
        else s(i) := Xor(pFinal(i), c(i-1))
      }
      //Leftmost bit of s is the last carry-bit
      if (carryOut) {
        s(width) := XorDelay(c(width-1))
      } else {
        if (!GlobalConfig.dryRun) s(width) := False
      }

      //Write the output
      if (!GlobalConfig.dryRun) {
        for (i <- 0 until width + 1) io.s(i) := s(i)
      }

      //Latency: Preprocessing + Generate-Propagate stages + final XOR
      this.latency = scala.math.max(preprocessingUnits(0).latency, preprocessing0Latency) + numStages*pgLatency + latencyXor
      ret
    } else {
      false
    }
  }
}
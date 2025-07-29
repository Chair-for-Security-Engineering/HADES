import spinal.core._
import spinal.core.internals._

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

/**
 * Multiplexer for 2 1-bit inputs
 * s = 0 means the a is returned, s = 1 means b is returned
 */
case class Mux2() extends Template {

	val io = new Bundle {
		val a = in  Bool()
		val b = in  Bool()
		val s = in  Bool()
		val c = out Bool()
	}

  override def instantiate(): Boolean = {
    val aPlusB, s, abs, a = Bool()
    aPlusB := Xor(io.a, io.b)
    s := XorDelay(io.s)
    abs := And(aPlusB, s)
    a := Delay(io.a, latencyAnd + latencyXor)
    io.c := Xor(abs, a)

    this.latency = 2*latencyXor + latencyAnd
    true
  }
}

/**
 * Multiplexer for 2 n-bit values
 * @param width Width of the 2 values
 */
case class NMux(width : Int = 1) extends Template {

	val io = new Bundle {
		val a = in  Bits(width bits)
		val b = in  Bits(width bits)
		val s = in  Bool()
		val c = out Bits(width bits)
	}

  override def instantiate(): Boolean = {
    val mux2 = instantiateMultipleSubTemplates(width)(Mux2())
    mux2.foreach(_.instantiateTemplate())

    if (!GlobalConfig.dryRun) {
      for (i <- 0 until width) {
        mux2(i).control.clk := control.clk
        mux2(i).control.reset := control.reset
        mux2(i).io.a := io.a(i)
        mux2(i).io.b := io.b(i)
        mux2(i).io.s := io.s
        io.c(i) := mux2(i).io.c
      }
    }

    this.latency = mux2(0).latency
    true
  }
}

/**
 * Multiplexer to choose between an arbitrary amount of input
 * @param n Number of inputs
 */
case class MuxN(selSig : Map[Int,Int]) extends Template {

  val n = selSig.size
	val logn = if (n == 1) 1 else log2Up(n)

	val io = new Bundle {
		val a = in  Bits(n bits)
		val s = in  Bits(logn bits)
		val c = out Bool()
	}

  override def instantiate(): Boolean = {
    //No input (should not happen if used correctly)
    if (n == 0) {
      io.c := null
      this.latency = 0
      false
    }
    //One input, simply return it. Zero latency as no logic is done
    else if (n == 1) {
      if (!GlobalConfig.dryRun) io.c := io.a(0)
      this.latency = 0
      true
    }
    //Two inputs: Make use of the Mux2 class
    else if (n == 2) {
      val mux = Mux2()
      mux.instantiateTemplate()
      if (!GlobalConfig.dryRun) {
        mux.control := control
        mux.io.a := io.a(0)
        mux.io.b := io.a(1)
        mux.io.s := io.s(0)
        io.c := mux.io.c
      }
      this.latency = mux.latency
      true
    }
    //Three or more inputs: Split the data into a left and a right part. The left part is a full multiplexer tree with a power of 2 elements
    //The right part has the remaining elementes
    else {
      var leftElements = Map[Int, Int]()
      var rightElements = Map[Int, Int]()

      var lcnt = 0
      var rcnt = 0
      for ((el, idx) <- selSig) {
        if ((el & 1) == 0) {
          leftElements ++= Map((el>>1) -> lcnt)
          lcnt += 1
        } else {
          rightElements ++= Map((el>>1) -> rcnt)
          rcnt += 1
        }
      }
      val leftlogn = if(leftElements.size == 1) 1 else log2Up(leftElements.size)
      val rightlogn = if(rightElements.size == 1) 1 else log2Up(rightElements.size)

      //Full multiplexer tree that gets all but the leftmost select bits
      val leftMux = MuxN(leftElements)
      leftMux.instantiateTemplate()

      //Right multiplexer, between 1 and 2^n elements. Only forward the log(rightElements) rightmost bits of select
      val rightMux = MuxN(rightElements)
      rightMux.instantiateTemplate()

      //Final Mux chooses between the signal of the left and right Mux using the leftmost select bit
      val finalMux = Mux2()
      finalMux.instantiateTemplate()

      // assign correct inputs
      var cntl = 0
      var cntr = 0
      if (!GlobalConfig.dryRun) {
        leftMux.control := control
        leftMux.io.s := io.s(leftlogn downto 1)
        rightMux.control := control
        rightMux.io.s := io.s(rightlogn downto 1)
        finalMux.control := control
        for ((el, idx) <- selSig) {
          if ((el & 1) == 0) {
            leftMux.io.a(leftElements(el >> 1)) := io.a(idx)
          } else {
            rightMux.io.a(rightElements(el >> 1)) := io.a(idx)
          }
        }
      }

      if (rightMux.latency == leftMux.latency) {
        if (!GlobalConfig.dryRun) {
          finalMux.io.a := leftMux.io.c
          finalMux.io.b := rightMux.io.c
        }
      } else if (rightMux.latency < leftMux.latency) {
        if (!GlobalConfig.dryRun) finalMux.io.a := leftMux.io.c
        finalMux.io.b := Delay(rightMux.io.c, leftMux.latency - rightMux.latency)
      } else {
        if (!GlobalConfig.dryRun) finalMux.io.a := Delay(leftMux.io.c, rightMux.latency - leftMux.latency)
        finalMux.io.b := rightMux.io.c
      }
      finalMux.io.s := Delay(io.s(0), math.max(rightMux.latency, leftMux.latency))

      if (!GlobalConfig.dryRun) io.c := finalMux.io.c

      this.latency = math.max(rightMux.latency, leftMux.latency) + finalMux.latency
      true
    }
  }
}


case class NMuxN(width : Int = 1, selSig : Map[Int, Int]) extends Template {

  val n = selSig.size
	val logn = if (n == 1) 1 else log2Up(n)

	val io = new Bundle {
		val a = in  Vec(spinal.core.Bits(width bits), n)
		val s = in  Bits(logn bits)
		val c = out Bits(width bits)
	}

  override def instantiate(): Boolean = {

    var ret = true

    val mux = instantiateMultipleSubTemplates(width)(MuxN(selSig))
    mux.foreach(ret &&= _.instantiateTemplate())

    if (!GlobalConfig.dryRun) {
      for (i <- 0 until width) {
        mux(i).control := control
        for (j <- 0 until n) {
          mux(i).io.a(j) := io.a(j)(i)
        }
        mux(i).io.s := io.s
        io.c(i) := mux(i).io.c
      }
    }

    this.latency = mux(0).latency
    ret
  }
}

/**
  * Mux between -n and n (two's complement representation in log2Up(2*n+1) bits)
  *
  * @param n defines number of select inputs (2n+1; from -n to n)
  */
case class MuxCentered(n : Int) extends Template {

  if (n < 1) {
    throw new IllegalArgumentException("n must be positive.")
  }
  if (n > 3) {
    throw new NotImplementedError("n > 3 is not yet implemented.")
  }

  val numIn = 2*n
  val selWidth = log2Up(numIn+1)

  val io = new Bundle {
    val a = in  Bits(numIn bits)
    val s = in  Bits(selWidth bits)
    val c = out Bool()
  }

  override def instantiate(): Boolean = {
    var ret = false
    if (n == 1) {
      /* Index in a | s
       * -----------|---
       * 0          | 11
       * 1          | 01
       */
      ret = true
      io.c := And(
        Xor(
          And(
            Xor(io.a(1), io.a(0)), 
            XorDelay(io.s(1))
            ),
          AndDelay(XorDelay(io.a(0)))
        ), 
        XorDelay(XorDelay(AndDelay(io.s(0))))
        )
      this.latency = 2*latencyXor + 2*latencyAnd



    } else if (n == 2) {
      /* Index in a | s
       * -----------|---
       * 0          | 110 = 0
       * 1          | 111 = 1
       * 2          | 001 = 0
       * 3          | 010 = 0
       */
      ret = true
      val mux = ArrayBuffer[Mux2]()
      for (i <- 0 until 3) {
        mux += new Mux2()
        ret &&= mux.last.instantiateTemplate()
        mux.last.control := control
      }
      val selZero = Not(And(Not(io.s(0)), Not(io.s(1))))
      if (!GlobalConfig.dryRun) {
        mux(0).io.a := io.a(3) // +-2
        mux(0).io.b := io.a(0)
        mux(0).io.s := io.s(2)
        mux(1).io.a := io.a(2) // +-1
        mux(1).io.b := io.a(1)
        mux(1).io.s := io.s(2)
        mux(2).io.a := mux(0).io.c // +-2 vs +-1
        mux(2).io.b := mux(1).io.c
      }
      mux(2).io.s := Delay(io.s(0), mux(0).latency)
      io.c := And(mux(2).io.c, Delay(selZero, mux(0).latency + mux(2).latency - latencyAnd - 2*latencyNot))
      this.latency = mux(0).latency + mux(2).latency + latencyAnd



    } else if (n == 3) {
      /* Index in a | s
       * -----------|---
       * 0          | 101 (-3)
       * 1          | 110 (-2)
       * 2          | 111 (-1)
       * 3          | 001 ( 1)
       * 4          | 010 ( 2)
       * 5          | 011 ( 3)
       */
      ret = true
      val submux = new MuxCentered(2)
      ret &&= submux.instantiateTemplate()
      submux.control := control
      submux.io.a := io.a(4 downto 1)
      submux.io.s := io.s
      
      val pm3mux = new Mux2()
      ret &&= pm3mux.instantiateTemplate()
      pm3mux.control := control
      pm3mux.io.a := io.a(5)
      pm3mux.io.b := io.a(0)
      pm3mux.io.s := io.s(2)

      val ispm3 = And(XorDelay(io.s(0)), Xor(io.s(1), io.s(2)))
      val finalMux = new Mux2()
      ret &&= finalMux.instantiateTemplate()
      finalMux.control := control
      finalMux.io.a := submux.io.c
      finalMux.io.b := Delay(pm3mux.io.c, submux.latency - pm3mux.latency)
      finalMux.io.s := Delay(ispm3, submux.latency - latencyAnd - latencyXor)
      io.c := finalMux.io.c
      this.latency = submux.latency + finalMux.latency
    }
    ret
  }
}


case class NMuxCentered(width : Int = 1, n : Int) extends Template {

  val numIn = 2*n
  val selWidth = log2Up(numIn+1)

	val io = new Bundle {
		val a = in  Vec(spinal.core.Bits(width bits), numIn)
		val s = in  Bits(selWidth bits)
		val c = out Bits(width bits)
	}


  override def instantiate(): Boolean = {
    var ret = true

    val mux = instantiateMultipleSubTemplates(width)(MuxCentered(n))
    mux.foreach(ret &&= _.instantiateTemplate())

    if (!GlobalConfig.dryRun) {
      for (i <- 0 until width) {
        mux(i).control := control
        for (j <- 0 until numIn) {
          mux(i).io.a(j) := io.a(j)(i)
        }
        mux(i).io.s := io.s
        io.c(i) := mux(i).io.c
      }
    }

    this.latency = mux(0).latency
    ret
  }
}
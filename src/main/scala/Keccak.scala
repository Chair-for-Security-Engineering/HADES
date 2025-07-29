import spinal.core._
import spinal.core.internals._
import spinal.lib.Counter
import spinal.core.internals.Template

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

/**
 * Configurable Keccak template
 * @param w
 */
final case class Keccak(w : Int = 64) extends Template {

  if (w != 64) {
    throw new NotImplementedError("Only Keccak with w=64 is supported.")
  }

  val io = new Bundle {
    val stateIn  =  in Vec(Vec(spinal.core.Bits(w bits), 5), 5)
    val stateOut = out Vec(Vec(spinal.core.Bits(w bits), 5), 5)
  }

  val fsm = new Bundle {
    val enable =  in Bool()
    val ready  = out Bool()
  }

  //States for the StateMachine of Keccak
	object KeccakState extends SpinalEnum {
		val sIdle, sInit, sThetaRhoPi, sChiInit, sChi, sIota, sRepeat  = newElement()
	}
	import KeccakState._

  private var chiParallel : Int = 64

  override def getOptions(): mutable.Map[String, Configurable] = Map(
    "parallelism" -> IntegerList(List(1, 2, 4, 8, 16, 32, 64))
  )

  override def configure(options: Predef.Map[String, Any]): Unit = {
    this.chiParallel = options("parallelism").asInstanceOf[Int]
  }

  override def instantiate(): Boolean = {
    if (latencyXor > 0) { // else, the state machine would have to be adapted
      return false
    }

    val thetaOut, rhoIn, rhoOut, piIn, piOut, chiIn, chiOut, chiRotOut, iotaIn, iotaOut = Vec(Vec(Bits(w bits), 5), 5)
    val thetaC, thetaD = Vec(Bits(w bits), 5)

    val rCnt = Counter(0, 23)
    val chiCnt = Counter(0, math.ceil(w.toDouble / chiParallel.toDouble).toInt - 1)
    val chiLatency = latencyAnd + latencyXor + latencyNot + 1
    val chiPipeCnt = Counter(math.max(chiLatency-1, 1))
    val state = PubReg(KeccakState()) init (sIdle)

    if (!GlobalConfig.dryRun) io.stateOut := io.stateIn

    // theta
    for (x <- 0 until 5) {
      for (z <- 0 until w) {
        thetaC(x)(z) := Xor(Xor(Xor(io.stateIn(x)(0)(z), io.stateIn(x)(1)(z)), Xor(io.stateIn(x)(2)(z), io.stateIn(x)(3)(z))), XorDelay(io.stateIn(x)(4)(z)))
        thetaD(x)(z) := Xor(thetaC((5+x-1)%5)(z), thetaC((x+1)%5)((z+w-1)%w))
        for (y <- 0 until 5) {
          thetaOut(x)(y)(z) := Xor(XorDelay(XorDelay(XorDelay(XorDelay(io.stateIn(x)(y)(z))))), thetaD(x)(z))
        }
      }
    }

    if (!GlobalConfig.dryRun) {
      rhoIn := thetaOut

      // rho
      val shAmt = List(List(0, 1, 190, 28, 91), List(36, 300, 6, 55, 276), List(3, 10, 171, 153, 231), List(105, 45, 15, 21, 136), List(210, 66, 253, 120, 78))
      for (x <- 0 until 5) {
        for (y <- 0 until 5) {
          val thisShAmt = shAmt(y)(x)
          for (z <- 0 until w) {
            rhoOut(x)(y)((z + thisShAmt) % w) := rhoIn(x)(y)(z)
          }
        }
      }

      piIn := rhoOut

      // pi
      for (x <- 0 until 5) {
        for (y <- 0 until 5) {
          for (z <- 0 until w) {
            piOut(x)(y)(z) := piIn((x + 3 * y) % 5)(x)(z)
          }
        }
      }

      when(state === sThetaRhoPi) {
        io.stateOut := piOut
      }

      chiIn := io.stateIn
    }

    // chi, one slice at a time, rotate lanes
    for (z <- 0 until w) {
      for (x <- 0 until 5) {
        for (y <- 0 until 5) {
          val zOff = (z+chiParallel) % w
          if (z < chiParallel) {
            chiOut(x)(y)(z) := Xor(chiIn(x)(y)(zOff), And(Not(chiIn((x+1)%5)(y)(zOff)), chiIn((x+2)%5)(y)(zOff)))
          } else {
            if (!GlobalConfig.dryRun) chiOut(x)(y)(z) := chiIn(x)(y)(zOff)
          }
        }
      }
    }

    if (!GlobalConfig.dryRun) {
      when(state === sChi) {
        io.stateOut := chiOut
      }

      iotaIn := io.stateIn
    }

    // iota
    val rc = Vec(
      B"x0000000000000001", B"x0000000000008082", B"x800000000000808A", B"x8000000080008000",
      B"x000000000000808B", B"x0000000080000001", B"x8000000080008081", B"x8000000000008009",
      B"x000000000000008A", B"x0000000000000088", B"x0000000080008009", B"x000000008000000A",
      B"x000000008000808B", B"x800000000000008B", B"x8000000000008089", B"x8000000000008003",
      B"x8000000000008002", B"x8000000000000080", B"x000000000000800A", B"x800000008000000A",
      B"x8000000080008081", B"x8000000000008080", B"x0000000080000001", B"x8000000080008008"
    )

    for (x <- 0 until 5) {
      for (y <- 0 until 5) {
        for (z <- 0 until w) {
          if (x == 0 && y == 0) {
            iotaOut(x)(y)(z) := Xor(iotaIn(x)(y)(z), rc(rCnt)(z))
          } else {
            iotaOut(x)(y)(z) := XorDelay(iotaIn(x)(y)(z))
          }
        }
      }
    }

    if (!GlobalConfig.dryRun) {
      when(state === sIota) {
        io.stateOut := iotaOut
      }

      fsm.ready := False
      switch(state) {
        is(sIdle) {
          fsm.ready := True
          when(fsm.enable) {
            state := sInit
          }
        }

        is(sInit) {
          rCnt.clear()
          state := sThetaRhoPi
        }

        is(sThetaRhoPi) {
          chiCnt.clear()
          chiPipeCnt.clear()
          if (chiLatency > 1) {
            state := sChiInit
          } else {
            state := sChi
          }
        }

        is(sChiInit) {
          chiPipeCnt.increment()
          when(chiPipeCnt.willOverflow) {
            state := sChi
          }
        }

        is(sChi) {
          chiCnt.increment()
          chiPipeCnt.clear()
          when(chiCnt.willOverflow) {
            state := sIota
          } otherwise {
            if (chiLatency > 1) {
              state := sChiInit
            }
          }
        }

        is(sIota) {
          state := sRepeat
        }

        is(sRepeat) {
          rCnt.increment()
          when(rCnt.willOverflow) {
            state := sIdle
          } otherwise {
            state := sThetaRhoPi
          }
        }
      }
    }

    this.latency = 24 * (1600 / (chiParallel*25) * chiLatency + 3)
    true
  }
}

/**
 * Fixed configuration Keccak
 * @param w
 */
final case class KeccakStandalone(w : Int = 64) extends Template {

  val io = new Bundle {
    val stateIn  =  in Vec(Vec(spinal.core.Bits(w bits), 5), 5)
    val stateOut = out Vec(Vec(spinal.core.Bits(w bits), 5), 5)
  }

  val fsm = new Bundle {
    val enable =  in Bool()
    val valid  =  in Bool()
    val ready  = out Bool()
  }

  //States for the StateMachine of KeccakStandalone
	object KeccakStandaloneState extends SpinalEnum {
		val sIdle, sPermuteInit, sPermute  = newElement()
	}
	import KeccakStandaloneState._

  val keccak = new Keccak(w)

  override def instantiate(): Boolean = {
    val ret = keccak.instantiateTemplate()
    val keccakState = Vec(Vec(Reg(Bits(w bits)) init (0), 5), 5)
    val state = PubReg(KeccakStandaloneState()) init (sIdle)


    if (!GlobalConfig.dryRun) {
      fsm.ready := False
      keccak.fsm.enable := False
      keccak.control := control

      keccak.io.stateIn := keccakState
      when(state === sPermute || state === sPermuteInit) {
        keccakState := keccak.io.stateOut
      } elsewhen (fsm.valid) {
        keccakState := io.stateIn
      }

      io.stateOut := keccakState

      switch(state) {
        is(sIdle) {
          fsm.ready := True
          when(fsm.enable) {
            state := sPermuteInit
          }
        }

        is(sPermuteInit) {
          keccak.fsm.enable := True
          when(!keccak.fsm.ready) {
            state := sPermute
          }
        }

        is(sPermute) {
          when(keccak.fsm.ready) {
            state := sIdle
          }
        }
      }
    }

    this.latency = keccak.latency + 3
    ret
  }
}
import spinal.core._
import spinal.core.internals._
import spinal.lib.Counter

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.{immutable, mutable}

object AdderModQPipelined extends Enumeration {
  type AdderModQPipelinedType = Value

  val NotPipelined, FullyPipelined = Value
}

import AdderModQPipelined._

object AllAdderModQPipelined extends Configurable {
  parameters = List(NotPipelined, FullyPipelined)
}

class AdderModQ(val q : Int) extends Template {

  val width = log2Up(q)

  val io = new Bundle {
    val x   = in  Bits(width bits)
    val y   = in  Bits(width bits)
    val sum = out Bits(width bits)
  }

  val fsm = new Bundle {
    val enable =  in Bool()
    val ready  = out Bool()
  }

  //States for the StateMachine of the serial AdderModQ
	object AMQState extends SpinalEnum {
		val sIdle, sInit, sAdd, sCSubQInit, sCSubQ, sMux = newElement()
	}
	import AMQState._

  var pipelined : AdderModQPipelinedType = _
  var adderInstance: Adder = _
  var csubqInstance: CSubQ = _
  var mux : NMux = _

  override def getOptions(): mutable.Map[String, Configurable] = Map(
    ("pipelined" -> AllAdderModQPipelined),
    ("adder" -> AllAdderFactories)
  )

  override def configure(options: Predef.Map[String, Any]): Unit = {
    this.pipelined = options("pipelined").asInstanceOf[AdderModQPipelinedType]

    if (pipelined == NotPipelined) {
      this.mux = new NMux(width)
    } else if (pipelined == FullyPipelined) {
      this.csubqInstance = new CSubQ(q)
    }

    if (pipelined == NotPipelined) {
      this.adderInstance = options("adder").asInstanceOf[AdderFactory].createTemplate(width+1, false, true)
    } else {
      this.adderInstance = options("adder").asInstanceOf[AdderFactory].createTemplate(width, pipelined == FullyPipelined, true)
    }
  }

  override def instantiate() : Boolean = {
    var ret = true

    if (pipelined == FullyPipelined) {
      ret &&= adderInstance.instantiateTemplate()
      ret &&= csubqInstance.instantiateTemplate()
      if (!csubqInstance.adderInstance.pipelined) ret = false

      if (!GlobalConfig.dryRun) {
        adderInstance.fsm.enable := False
        adderInstance.control := control
        adderInstance.io.x := io.x
        adderInstance.io.y := io.y
        adderInstance.io.cIn := False

        csubqInstance.fsm.enable := False
        csubqInstance.control := control
        csubqInstance.io.x := adderInstance.io.s

        io.sum := csubqInstance.io.xsubq
        fsm.ready := True
      }

      this.latency = adderInstance.latency + csubqInstance.latency
    }

    if (this.pipelined == NotPipelined) {
      ret &&= mux.instantiateTemplate()
      ret &&= adderInstance.instantiateTemplate()

      val state = PubReg(AMQState()) init(sIdle)
      val inbuf = Reg(Bits(width bits))
      val muxCounter = Counter(0, mux.latency)

      if (!GlobalConfig.dryRun) {
        adderInstance.control := control
        adderInstance.io.cIn := False

        fsm.ready := False
        adderInstance.fsm.enable := False
        when(state === sInit || state === sIdle || state === sAdd) {
          adderInstance.io.x(width - 1 downto 0) := io.x
          adderInstance.io.y(width - 1 downto 0) := io.y
          adderInstance.io.x(width) := False
          adderInstance.io.y(width) := False
        }.otherwise {
          adderInstance.io.x := adderInstance.io.s(width downto 0)
          adderInstance.io.y := (S(-q, width + 1 bits)).asBits
        }

        when(state === sCSubQInit) {
          inbuf := adderInstance.io.s(width - 1 downto 0)
        }

        mux.io.a := inbuf
        mux.io.b := adderInstance.io.s(width - 1 downto 0)
        mux.io.s := !adderInstance.io.s(width)
        mux.control := control

        io.sum := mux.io.c

        switch(state) {
          is(sIdle) {
            fsm.ready := True
            when(fsm.enable && adderInstance.fsm.ready) {
              state := sInit
            }
          }

          is(sInit) {
            fsm.ready := True
            adderInstance.fsm.enable := True
            when(!adderInstance.fsm.ready) {
              state := sAdd
            }
          }

          is(sAdd) {
            when(adderInstance.fsm.ready) {
              state := sCSubQInit
            }
          }

          is(sCSubQInit) {
            adderInstance.fsm.enable := True
            when(!adderInstance.fsm.ready) {
              state := sCSubQ
            }
          }

          is(sCSubQ) {
            muxCounter.clear()
            when(adderInstance.fsm.ready) {
              state := sMux
            }
          }

          is(sMux) {
            muxCounter.increment()
            when(muxCounter.willOverflow) {
              state := sIdle
            }
          }
        }
      }

      this.latency = adderInstance.latency*2 + mux.latency + 4
    }

    ret
  }
}

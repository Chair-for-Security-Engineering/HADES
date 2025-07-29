import spinal.core._
import spinal.core.internals._
import spinal.lib.Counter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

class CSubQ(var q : Int) extends Template {
  var width = log2Up(q)

  val io = new Bundle {
    val x     = in  Bits(width+1 bits)
    val xsubq = out Bits(width   bits)
  }

  val fsm = new Bundle {
    val enable   =  in Bool()
    val ready    = out Bool()
  }

  //States for the StateMachine of the serial CSubQ
	object CSQState extends SpinalEnum {
		val sIdle, sInit, sSubtract, sMux, sDone = newElement()
	}
	import CSQState._

  private var pipelined : Boolean = _
  var adderInstance: Adder = _
  private val mux = NMux(width)

  override def getOptions(): mutable.Map[String, Configurable] = Map(
    ("adder" -> AllAdderFactories),
    ("pipelined" -> AllBooleans)
  )

  override def configure(options: Predef.Map[String, Any]): Unit = {
    this.pipelined = options("pipelined").asInstanceOf[Boolean]

    this.adderInstance = options("adder").asInstanceOf[AdderFactory].createTemplate(width+1, pipelined, true)
  }

  // 1. always subtract q
  // 2. delay the input around the adder that does the subtraction
  // 3. mux securely based on the carry-out of the adder
  
  override def instantiate(): Boolean = {
    var ret = true

    ret &&= mux.instantiateTemplate()
    ret &&= adderInstance.instantiateTemplate()

    if (!GlobalConfig.dryRun) {
      mux.control := control
      io.xsubq := mux.io.c

      adderInstance.control := control
      adderInstance.io.x := io.x
      adderInstance.io.cIn := False
      adderInstance.io.y := (S(-q, width + 1 bits)).asBits
    }

    if (!pipelined) {
      // on enable: write x to inbuf and adderInstance.io.x, enable adder.
      // Then wait for adder to finish, and mux between inbuf and the subtraction result.
      val inbuf = Reg(Bits(width bits))
      val state = PubReg(CSQState()) init(sIdle)
      val muxCounter = Counter(0, mux.latency)

      if (!GlobalConfig.dryRun) {
        val read = Bool()
        read := False
        adderInstance.fsm.enable := False

        when(read) {
          inbuf := io.x(width - 1 downto 0)
        }

        //State Machine
        switch(state) {
          //Idle/Reset state
          is(sIdle) {
            state := sIdle
            fsm.ready := adderInstance.fsm.ready
            when(fsm.enable && adderInstance.fsm.ready) {
              state := sInit
            }
          }

          //Initialization of registers with input data
          is(sInit) {
            fsm.ready := False
            read := True
            adderInstance.fsm.enable := True
            when(!adderInstance.fsm.ready) {
              state := sSubtract
            }
          }

          //Subtraction
          is(sSubtract) {
            fsm.ready := False
            state := sSubtract
            muxCounter.clear()

            when(adderInstance.fsm.ready) {
              state := sMux
            }
          }

          //Mux
          is(sMux) {
            fsm.ready := False
            state := sMux

            muxCounter.increment()
            when(muxCounter.willOverflow) {
              state := sDone
            }
          }

          //Done state
          is(sDone) {
            state := sIdle
            fsm.ready := True
            when(fsm.enable) {
              state := sInit
            }
          }
        }

        mux.io.a := adderInstance.io.s(width - 1 downto 0)
        mux.io.b := inbuf
        mux.io.s := adderInstance.io.s(width)
      }
      
      this.latency = adderInstance.latency + mux.latency + 2
    } else {
      if (!GlobalConfig.dryRun) {
        fsm.ready := True
        mux.io.a := adderInstance.io.s(width - 1 downto 0)
        mux.io.s := adderInstance.io.s(width)
      }
      for (i <- 0 until width) {
        mux.io.b(i) := Delay(io.x(i), adderInstance.latency)
      }

      this.latency = adderInstance.latency + mux.latency
    }
    ret
  }
}

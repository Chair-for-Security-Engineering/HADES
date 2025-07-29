import spinal.core._
import spinal.core.internals._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class AESMixColumns(mode: String = "encryption-only") extends Template {

  val supportedModes: List[String] = List("encryption-only", "decryption-only", "both")
  if (!supportedModes.contains(mode)) throw new IllegalArgumentException("Invalid mode selected.")

  val io = new Bundle {
    val columnIn           = in  Vec(spinal.core.Bits(8 bits), 4)
    val isInverseMixColumn = (mode == "both") generate (in  Bool())
    val columnOut          = out Vec(spinal.core.Bits(8 bits), 4)
    val invColumnOut       = (mode == "both") generate (out Vec(spinal.core.Bits(8 bits), 4))
  }

  if (mode == "both") io.isInverseMixColumn.addTag(publicInput)

  @Name
  private def mul2(a: Bits): Bits = {
    val ret = Bits(8 bits)

    ret(0) := XorDelay(a(7))
    ret(1) := Xor(a(0), a(7))
    ret(2) := XorDelay(a(1))
    ret(3) := Xor(a(2), a(7))
    ret(4) := Xor(a(3), a(7))
    ret(5) := XorDelay(a(4))
    ret(6) := XorDelay(a(5))
    ret(7) := XorDelay(a(6))

    ret
  }

  override def instantiate(): Boolean = {
    // Implements serial decomposition with byte-level resource sharing.
    // See: V. Fischer, M. Drutarovsky, P. Chodowiec and F. Gramain, "InvMixColumn decomposition and multilevel resource sharing in AES implementations," in IEEE Transactions on Very Large Scale Integration (VLSI) Systems, vol. 13, no. 8, pp. 989-992, Aug. 2005, doi: 10.1109/TVLSI.2005.853606.

    val mixColumnsOut = Vec(Bits(8 bits), 4)
    val invMixColumnsOut = Vec(Bits(8 bits), 4)

    val t0 = Bits(8 bits) // a0 ^ a1
    val t1 = Bits(8 bits) // a1 ^ a2
    val t2 = Bits(8 bits) // a2 ^ a3
    val t3 = Bits(8 bits) // a3 ^ a0
    val t4 = Bits(8 bits) // a0 ^ a1 ^ a2 ^ a3
    val t5 = Bits(8 bits) // xtime(a0 ^ a1)
    val t6 = Bits(8 bits) // xtime(a1 ^ a2)
    val t7 = Bits(8 bits) // xtime(a2 ^ a3)
    val t8 = Bits(8 bits) // xtime(a3 ^ a0)
    val t9 = Bits(8 bits) // mixColumnsOut(0) ^ mixColumnsOut(2)
    val t10 = Bits(8 bits) // mixColumnsOut(1) ^ mixColumnsOut(3)
    val t11 = Bits(8 bits) // xtime(xtime(t9))
    val t12 = Bits(8 bits) // xtime(xtime(t10))

    for (i <- 7 downto 0) {
      t0(i) := Xor(io.columnIn(0)(i), io.columnIn(1)(i))
      t1(i) := Xor(io.columnIn(1)(i), io.columnIn(2)(i))
      t2(i) := Xor(io.columnIn(2)(i), io.columnIn(3)(i))
      t3(i) := Xor(io.columnIn(3)(i), io.columnIn(0)(i))
      t4(i) := Xor(t0(i), t2(i))
    }

    t5 := mul2(t0)
    t6 := mul2(t1)
    t7 := mul2(t2)
    t8 := mul2(t3)

    for (i <- 7 downto 0) {
      mixColumnsOut(0)(i) := Xor(t5(i), XorDelay(Xor(XorDelay(XorDelay(io.columnIn(0)(i))), t4(i))))
      mixColumnsOut(1)(i) := Xor(t6(i), XorDelay(Xor(XorDelay(XorDelay(io.columnIn(1)(i))), t4(i))))
      mixColumnsOut(2)(i) := Xor(t7(i), XorDelay(Xor(XorDelay(XorDelay(io.columnIn(2)(i))), t4(i))))
      mixColumnsOut(3)(i) := Xor(t8(i), XorDelay(Xor(XorDelay(XorDelay(io.columnIn(3)(i))), t4(i))))
    }

    if (mode != "encryption-only") {
      for (i <- 7 downto 0) {
        t9(i) := Xor(mixColumnsOut(0)(i), mixColumnsOut(2)(i))
        t10(i) := Xor(mixColumnsOut(1)(i), mixColumnsOut(3)(i))
      }

      t11 := mul2(mul2(t9))
      t12 := mul2(mul2(t10))

      for (i <- 7 downto 0) {
        invMixColumnsOut(0)(i) := Xor(t11(i), XorDelay(XorDelay(mixColumnsOut(0)(i))))
        invMixColumnsOut(1)(i) := Xor(t12(i), XorDelay(XorDelay(mixColumnsOut(1)(i))))
        invMixColumnsOut(2)(i) := Xor(t11(i), XorDelay(XorDelay(mixColumnsOut(2)(i))))
        invMixColumnsOut(3)(i) := Xor(t12(i), XorDelay(XorDelay(mixColumnsOut(3)(i))))
      }
    }

    mode match {
      case "encryption-only" => io.columnOut := mixColumnsOut
      case "decryption-only" => io.columnOut := invMixColumnsOut
      case "both" => {
        io.columnOut := mixColumnsOut
        io.invColumnOut := invMixColumnsOut
      }
    }

    this.latency = 7 * latencyXor
    true
  }
}

case class AES(keySize : Int = 128, mode : String = "encryption-only") extends Template {
  this.templateName = f"AES${keySize}"

  // Detect invalid algorithm parameters
  val supportedModes: List[String] = List("encryption-only")
  if (!supportedModes.contains(mode)) {
    throw new IllegalArgumentException("Invalid mode selected.")
  }
  val supportedKeySizes: List[Int] = List(128, 192, 256)
  if (!supportedKeySizes.contains(keySize)) {
    throw new IllegalArgumentException("Invalid keysize selected.")
  }

  // ---------------------------------------------------
  // -- Define IO
  // ---------------------------------------------------
  val io = new Bundle {
    val key        =  in Bits (keySize bits)
    val dataIn     =  in Bits (128 bits)
    val dataOut    = out Bits (128 bits)
  }

  val fsm = new Bundle {
    val enable =  in Bool()
    val ready  = out Bool()
    val valid  = out Bool()
  }

  // ---------------------------------------------------
  // -- Define FSM states
  // ---------------------------------------------------
  object AESState extends SpinalEnum {
    val sIdle, sComputeRound, sComputeRoundProcessOutputs, sWaitKeySchedule = newElement()
  }
  import AESState._

  // ---------------------------------------------------
  // -- Define some constants
  // ---------------------------------------------------
  val roundCount = keySize match {
    case 128 => 10
    case 192 => 12
    case 256 => 14
  }

  // ---------------------------------------------------
  // -- Define sub-templates and configuration options
  // ---------------------------------------------------
  var keyScheduleInstance: AESKeySchedule = _
  var fullyPipelinedKeyScheduleInstance: AESFullyPipelinedKeySchedule = _

  var architecture: String = _

  var shareSboxes: Boolean = _
  var sboxPipelining: Boolean = _

  var roundSboxCount: Int = _
  var roundSboxType: AESSboxFactory = _
  var mixColumnsCount: Int = _
  var sboxInstances: ArrayBuffer[AESSbox] = ArrayBuffer()
  var mixColumnsInstances: ArrayBuffer[AESMixColumns] = ArrayBuffer()

  override def getOptions(): mutable.Map[String, Configurable] = mutable.Map(
    ("architecture" -> StringList(List("round-based", "fully-unrolled"))),
    ("shareSboxes" -> AllBooleans),
    ("roundSboxCount" -> IntegerList(List(0,1,2,4,8,16))),
    ("roundSboxType" -> AllSboxFactories),
    ("sboxPipelining" -> AllBooleans),
    ("mixColumnsParallelism" -> IntegerList(List(1,2,4)))
  )

  override def configure(options: Map[String, Any]): Unit = {
    architecture = options("architecture").asInstanceOf[String]
    val unrolledCount = architecture match {
      case "round-based" => 1
      case "fully-unrolled" => roundCount
    }

    // Get sbox options
    shareSboxes = options("shareSboxes").asInstanceOf[Boolean]
    roundSboxCount = options("roundSboxCount").asInstanceOf[Int]
    sboxPipelining = options("sboxPipelining").asInstanceOf[Boolean]
    roundSboxType = options("roundSboxType").asInstanceOf[AESSboxFactory]

    // Instantiate key schedule
    architecture match {
      case "round-based" =>
        keyScheduleInstance = AESKeySchedule(keySize = keySize, mode = mode, shareSboxes = shareSboxes)

      case "fully-unrolled" =>
        fullyPipelinedKeyScheduleInstance = AESFullyPipelinedKeySchedule(keySize, mode)
    }

    // Instantiate sboxes
    sboxInstances.clear()
    sboxInstances ++= instantiateMultipleSubTemplates(unrolledCount * roundSboxCount)(roundSboxType.createTemplate(pipelined = sboxPipelining))
    sboxInstances.foreach(_.io.sboxIn.addTag(share))
    sboxInstances.foreach(_.io.sboxOut.addTag(share))

    // Instantiate MixColumns instances
    mixColumnsCount = options("mixColumnsParallelism").asInstanceOf[Int]
    mixColumnsInstances.clear()
    mixColumnsInstances ++= instantiateMultipleSubTemplates(scala.math.min(unrolledCount, 15) * mixColumnsCount)(AESMixColumns())
    mixColumnsInstances.foreach(_.io.columnIn.addTag(share))
    mixColumnsInstances.foreach(_.io.columnOut.addTag(share))
    if (mode == "both") mixColumnsInstances.foreach(_.io.invColumnOut.addTag(share))

  }

  override def instantiate(): Boolean = {
    var ret = true

    // ---------------------------------------------------
    // -- Instantiate sub-templates
    // ---------------------------------------------------
    sboxInstances.foreach(ret &&= _.instantiateTemplate())
    mixColumnsInstances.foreach(ret &&= _.instantiateTemplate())

    // ---------------------------------------------------
    // -- Detect invalid configurations
    // ---------------------------------------------------

    // Due to the way MixColumns is currently handled, this implementation breaks if latencyXor > 0
    if (latencyXor > 1) throw new NotImplementedError("MixColumns handling does not currently support latencyXor > 0")

    // ---------------------------------------------------
    // -- Round-based AES
    // ---------------------------------------------------
    if (architecture == "round-based") {
      // Instantiate key schedule sub-template
      ret &&= keyScheduleInstance.instantiateTemplate()

      // ---------------------------------------------------
      // -- Detect invalid configurations
      // ---------------------------------------------------
      // If sbox sharing is enabled, no sbox instances should be instantiated within this template
      // and other parameters should match the sboxes as they are instantiated within the key schedule.
      if (shareSboxes && (roundSboxCount != 0 || sboxPipelining != keyScheduleInstance.sboxPipelining || roundSboxType != keyScheduleInstance.sboxType)) return false
      // If sbox sharing is disabled, the number of sboxes can't be 0
      if (!shareSboxes && roundSboxCount == 0) return false

      // Update number of available sbox instances if sbox sharing is enabled
      if (shareSboxes) roundSboxCount = keyScheduleInstance.sboxCount
      // Compute the number of required sbox iterations per AES round
      val sboxIterationsPerRound = 16 / roundSboxCount
      // Compute latency of a single AES round
      val sboxLatency = if (shareSboxes) keyScheduleInstance.sboxInstances(0).latency else sboxInstances(0).latency
      val roundLatency = if (!sboxPipelining) sboxIterationsPerRound * (sboxLatency + 1) else (sboxLatency + 1) + sboxIterationsPerRound - 1

      // The key schedule always has to be faster than or equally fast as the round function when sbox sharing is disabled.
      if (!shareSboxes && keyScheduleInstance.latency > roundLatency) return false

      // Having less MixColumn instances than ceil(sboxCount / 4) makes no sense. This would mean that multiple MixColumn iterations are required per
      // sbox iteration. Thus, the round computation wouldn't finish any quicker than if we were just using 4 * mixColumnCount many sboxes but area
      // usage is clearly higher.
      // ----------------------------
      // Having more MixColumn instances than ceil(sboxCount / 4) makes no sense. This would mean that multiple sbox iterations are required per MixColumns
      // iteration. Unless sboxCount < 4, this makes no sense since we could achieve the same latency using just ceil(sboxCount / 4) many MixColumns instances
      // which would reduce the area usage.
      if (mixColumnsCount != scala.math.ceil(roundSboxCount.toDouble / 4)) return false

      // Registers and other intermediate values
      val aesState = Reg(Bits(128 bits))

      val state = PubReg(AESState()) init(sIdle)
      val returnState = if (shareSboxes) PubReg(AESState()) init(sIdle) else null

      val keyScheduleEnable = if (shareSboxes) PubReg(Bool()) init(False) else null

      val roundCounter = Counter(0, roundCount - 1)
      val sboxIterationCounter = Counter(0, sboxIterationsPerRound - 1)
      val sboxLatencyCounter = Counter(0, sboxLatency)

      // Signals for representing intermediate results
      val shiftRowsOutput = Bits(128 bits)
      val subBytesOutput = Bits(128 bits)
      val mixColumnsOutput = Bits(128 bits)
      val addRoundKeyInput = Bits(128 bits)
      val addRoundKeyOutput = Bits(128 bits)

      val sboxInputs = Vec(Bits(8 bits), roundSboxCount)
      val sboxOutputs = Vec(Bits(8 bits), roundSboxCount)

      if (!GlobalConfig.dryRun) {
        // Connect control / static signals of sub-templates
        keyScheduleInstance.control := control
        keyScheduleInstance.io.key := io.key
        if (!shareSboxes) {
          sboxInstances.foreach(_.control := control)
          sboxInstances.foreach(_.io.isInverseSbox := False)
        }
        mixColumnsInstances.foreach(_.control := control)

        // ---------------------------------------------------
        // -- Wiring for individual AES round steps
        // ---------------------------------------------------

        // Perform ShiftRows step => Simple wiring onto ShiftRows output
        // ---------------------------------------------------
        for (i <- 0 until 4) { // Iterate over all output rows
          for (j <- 0 until 4) { // Iterate over all output columns
            shiftRowsOutput(127 - (mod(j - i, 4) * 4 + i) * 8 downto 120 - (mod(j - i, 4) * 4 + i) * 8) := aesState(127 - (j * 4 + i) * 8 downto 120 - (j * 4 + i) * 8)
          }
        }

        // Perform SubBytes step on ShiftRows output / AES state
        // ---------------------------------------------------
        if (shareSboxes) {
          // Connect to sbox instances in key schedule sub-template
          for (i <- 0 until roundSboxCount) {
            keyScheduleInstance.io.sboxIn(i) := sboxInputs(i)
            sboxOutputs(i) := keyScheduleInstance.io.sboxOut(i)
          }
          for (i <- roundSboxCount until 4) {
            keyScheduleInstance.io.sboxIn(i) := B"8'h00"
          }
        } else {
          // Connect to sbox instances in this template
          for (i <- 0 until roundSboxCount) {
            if (sboxPipelining) {
              // To avoid potential leakage caused by performing sbox computations on the same input value with different randomness,
              // we make sure that the complete sbox pipeline only contains the correct input once and otherwise only zeros.
              when (sboxLatencyCounter.value === 0) {
                sboxInstances(i).io.sboxIn := sboxInputs(i)
              } otherwise {
                sboxInstances(i).io.sboxIn := B"8'h0"
              }
            } else {
              sboxInstances(i).io.sboxIn := sboxInputs(i)
            }
            sboxOutputs(i) := sboxInstances(i).io.sboxOut
          }
        }

        // Handle static parts of the output
        for (i <- 0 until roundSboxCount) {
          val subBytesOutputStartIndex = 8 * (roundSboxCount - i) - 1
          subBytesOutput(subBytesOutputStartIndex downto subBytesOutputStartIndex - 7) := sboxOutputs(i)
        }

        // Handle non-static parts of the sbox IO
        if (roundSboxCount == 16) {
          for (i <- 0 until roundSboxCount) sboxInputs(i) := shiftRowsOutput(127 - 8 * i downto 120 - 8 * i)
        } else {
          var useShiftRowsAsInputCond: Bool = sboxIterationCounter.value === 0
          if (sboxPipelining && sboxIterationsPerRound > 1) useShiftRowsAsInputCond = state === sComputeRound && sboxLatencyCounter.value === 0

          when(useShiftRowsAsInputCond) {
            // In the first iteration, we take the sbox inputs from the shiftRowsOutput signal
            for (i <- 0 until roundSboxCount) sboxInputs(i) := shiftRowsOutput(127 - 8 * i downto 120 - 8 * i)
            subBytesOutput(127 downto 8 * roundSboxCount) := shiftRowsOutput(127 - 8 * roundSboxCount downto 0)
          } otherwise {
            // Take inputs from aesState since it already contains the shifted state by now
            for (i <- 0 until roundSboxCount) sboxInputs(i) := aesState(127 - 8 * i downto 120 - 8 * i)
            subBytesOutput(127 downto 8 * roundSboxCount) := aesState(127 - 8 * roundSboxCount downto 0)
          }
        }

        // Handle sbox pipelining
        if (sboxPipelining && sboxIterationsPerRound > 1) {
          // Rotate AES state in every clock cycle to pass new inputs to the sbox instances
          when (state === sComputeRound || state === sComputeRoundProcessOutputs) {
            aesState := aesState.rotateLeft(8 * roundSboxCount)
          }

          // Copy shiftRowsOutput into AES state register
          when (state === sComputeRound && sboxLatencyCounter.value === 0) {
            aesState := shiftRowsOutput.rotateLeft(8 * roundSboxCount)
          }
        }

        // Perform MixColumns step => Simple wiring from sbox outputs to MixColumns output. (We assume that MixColumns always has 0 latency)
        // ---------------------------------------------------
        // Set IO for MixColumns instances
        for (i <- 0 until mixColumnsCount) {
          for (j <- 0 until 4) {
            val mixColumnInputStartIdx = 32 * (mixColumnsCount - i) - 1 - 8 * j
            mixColumnsInstances(i).io.columnIn(j) := subBytesOutput(mixColumnInputStartIdx downto mixColumnInputStartIdx - 7)
            mixColumnsOutput(mixColumnInputStartIdx downto mixColumnInputStartIdx - 7) := mixColumnsInstances(i).io.columnOut(j)
          }
        }
        // Copy the rest of the state from the output of the sub bytes layer
        mixColumnsOutput(127 downto 32 * mixColumnsCount) := subBytesOutput(127 downto 32 * mixColumnsCount)

        // Perform AddRoundKey (skip MixColumns in the last round)
        // ---------------------------------------------------
        when (roundCounter.value === roundCounter.end) {
          addRoundKeyInput := subBytesOutput
        } otherwise {
          addRoundKeyInput := mixColumnsOutput
        }
        for (i <- 127 downto 0) addRoundKeyOutput(i) := Xor(addRoundKeyInput(i), keyScheduleInstance.io.roundkey(i))

        // ---------------------------------------------------
        // -- Data flow / AES state update
        // ---------------------------------------------------

        // Perform initial round key addition and thus also initialize the internal AES state
        when (state === sIdle && fsm.enable) {
          for (i <- 127 downto 0) aesState(i) := Xor(io.dataIn(i), keyScheduleInstance.io.roundkey(i))
        }

        // Update AES state with SubBytes / MixColumns results depending on if there are enough Sbox outputs available to apply MixColumns or not
        var subBytesStateUpdateCond : Bool = state === sComputeRound && sboxLatencyCounter.willOverflow
        if (sboxPipelining && sboxIterationsPerRound > 1) subBytesStateUpdateCond = state === sComputeRound && sboxLatencyCounter.willOverflow || state === sComputeRoundProcessOutputs

        when (subBytesStateUpdateCond) {
          roundSboxCount match {
            case 1 =>
              when (sboxIterationCounter.value.asBits(1 downto 0).asUInt === 3) {
                aesState := addRoundKeyInput
              } otherwise {
                aesState := subBytesOutput
              }

            case 2 =>
              when (sboxIterationCounter.value.asBits(0 downto 0).asUInt === 1) {
                aesState := addRoundKeyInput
              } otherwise {
                aesState := subBytesOutput
              }

            case _ =>
              // If 4 or more sbox instances are instantiated, MixColumns is applied in each iteration
              aesState := addRoundKeyInput
          }
        }

        // Update AES state with AddRoundKey result at the end of every round
        when (sboxIterationCounter.willOverflow) {
          aesState := addRoundKeyOutput
        }

        // ---------------------------------------------------
        // -- Output handling
        // ---------------------------------------------------
        io.dataOut := aesState

        // ---------------------------------------------------
        // -- FSM
        // ---------------------------------------------------
        fsm.valid := False
        fsm.ready := False

        // Key schedule control signals
        if (shareSboxes) {
          keyScheduleEnable := False
          keyScheduleInstance.fsm.enable := keyScheduleEnable
          keyScheduleInstance.fsm.externalSboxEnable := False
        } else {
          keyScheduleInstance.fsm.enable := False
        }
        keyScheduleInstance.fsm.reset := False

        switch (state) {
          is (sIdle) {
            fsm.valid := True
            fsm.ready := True
            keyScheduleInstance.fsm.reset := True

            when (fsm.enable) {
              // Enable key schedule to generate next round key
              keyScheduleInstance.fsm.reset := False
              if (!shareSboxes) keyScheduleInstance.fsm.enable := True
              if (!shareSboxes) state := sComputeRound

              if (shareSboxes) {
                returnState := sComputeRound
                state := sWaitKeySchedule
                keyScheduleEnable := True
              }

              fsm.valid := False
              fsm.ready := False
            }
          }

          is (sComputeRound) {
            if (shareSboxes) keyScheduleInstance.fsm.externalSboxEnable := True

            sboxLatencyCounter.increment()
            when (sboxLatencyCounter.willOverflow) {
              if (sboxPipelining && sboxIterationsPerRound > 1) state := sComputeRoundProcessOutputs
              sboxIterationCounter.increment()
              when (sboxIterationCounter.willOverflow) {
                // Start generating the next round key
                if (!shareSboxes) keyScheduleInstance.fsm.enable := True

                if (shareSboxes) {
                  returnState := sComputeRound
                  state := sWaitKeySchedule
                  keyScheduleEnable := True
                }

                roundCounter.increment()
                when (roundCounter.willOverflow) {
                  state := sIdle
                }
              }
            }
          }

          // This state is used if sboxPipelining is enabled. Once we are in this state, a new set of sbox outputs
          // will be ready in every clock cycle.
          if (sboxPipelining && sboxIterationsPerRound > 1) {
            is (sComputeRoundProcessOutputs) {
              if (shareSboxes) keyScheduleInstance.fsm.externalSboxEnable := True

              sboxIterationCounter.increment()
              when (sboxIterationCounter.willOverflow) {
                // Start generating the next round key
                if (!shareSboxes) keyScheduleInstance.fsm.enable := True
                if (!shareSboxes) state := sComputeRound

                if (shareSboxes) {
                  returnState := sComputeRound
                  state := sWaitKeySchedule
                  keyScheduleEnable := True
                }

                roundCounter.increment()
                when (roundCounter.willOverflow) {
                  state := sIdle
                }
              }
            }
          }

          if (shareSboxes) {
            is (sWaitKeySchedule) {
              keyScheduleInstance.fsm.externalSboxEnable := False

              // Wait for the key schedule update to finish and go back to computing the next AES round
              when (keyScheduleInstance.fsm.valid) {
                state := returnState
              }
            }
          }
        }
      }

      // Determine latency
      this.latency = 1 + roundCount * roundLatency
      if (shareSboxes) {
        this.latency += roundCount * (keyScheduleInstance.latency + 1)
      }
      this.reload = this.latency
    }

    // ---------------------------------------------------
    // -- Fully-unrolled AES
    // ---------------------------------------------------
    if (architecture == "fully-unrolled") {
      // Detect invalid configurations
      if (roundSboxCount < 16 || !sboxPipelining || mixColumnsCount < 4) return false

      // Instantiate key schedule sub-template
      ret &&= fullyPipelinedKeyScheduleInstance.instantiateTemplate()

      // Registers (only needed if the Sboxes contain no register stages)
      var roundIn: Vec[Bits] = null
      if (sboxInstances(0).latency == 0) roundIn = Vec(Reg(Bits(128 bits)), roundCount) else roundIn = Vec(Bits(128 bits), roundCount)

      // Other intermediate values
      val initAddRoundKeyOut = Bits(128 bits)

      val subBytesOut = Vec(Bits(128 bits), roundCount)
      val shiftRowsOut = Vec(Bits(128 bits), roundCount)
      val mixColumnsOut = Vec(Bits(128 bits), roundCount - 1)
      val roundOutputs = Vec(Bits(128 bits), roundCount - 1)

      val ciphertext = Bits(128 bits)

      if (!GlobalConfig.dryRun) {
        // Connect control inputs of all sub-templates
        sboxInstances.foreach(_.control := control)
        sboxInstances.foreach(_.io.isInverseSbox := False)
        mixColumnsInstances.foreach(_.control := control)
        fullyPipelinedKeyScheduleInstance.control := control

        // Set key schedule input
        fullyPipelinedKeyScheduleInstance.io.key := io.key

        // ---------------------------------------------------
        // -- Initial AddRoundKey
        // ---------------------------------------------------
        for (i <- 127 downto 0) initAddRoundKeyOut(i) := Xor(io.dataIn(i), io.key(keySize - 128 + i))

        // ---------------------------------------------------
        // -- roundCount many AES rounds
        // ---------------------------------------------------
        // Set round inputs
        roundIn(0) := initAddRoundKeyOut
        for (i <- 1 until roundCount) roundIn(i) := roundOutputs(i-1)

        // Perform round computation
        for (i <- 0 until roundCount) {
          // SubBytes:
          // ---------------------------------------------------
          for (j <- 0 until 16) {
            sboxInstances(16*i + j).io.sboxIn := roundIn(i)(8*j + 7 downto 8*j)
            subBytesOut(i)(8*j + 7 downto 8*j) := sboxInstances(16*i + j).io.sboxOut
          }

          // ShiftRows:
          // ---------------------------------------------------
          for (j <- 0 until 4) { // Iterate over all output rows
            for (k <- 0 until 4) { // Iterate over all output columns
              shiftRowsOut(i)(127 - (mod(k - j, 4) * 4 + j) * 8 downto 120 - (mod(k - j, 4) * 4 + j) * 8) := subBytesOut(i)(127 - (k * 4 + j) * 8 downto 120 - (k * 4 + j) * 8)
            }
          }

          // MixColumns:
          // ---------------------------------------------------
          if (i < roundCount - 1) {
            for (j <- 0 until 4) { // Iterate over all MixColumns instances
              for (k <- 0 until 4) { // Iterate over all 4 input bytes
                val mixColumnInputStartIdx = 32 * (mixColumnsCount - j) - 1 - 8 * k
                mixColumnsInstances(4*i + j).io.columnIn(k) := shiftRowsOut(i)(mixColumnInputStartIdx downto mixColumnInputStartIdx - 7)
                mixColumnsOut(i)(mixColumnInputStartIdx downto mixColumnInputStartIdx - 7) := mixColumnsInstances(4*i + j).io.columnOut(k)
              }
            }
          }

          // AddRoundKey:
          // ---------------------------------------------------
          for (j <- 127 downto 0) {
            if (i < roundCount - 1) {
              roundOutputs(i)(j) := Xor(mixColumnsOut(i)(j), fullyPipelinedKeyScheduleInstance.io.roundkeys(i)(j))
            } else {
              // Last round without MixColumns
              ciphertext(j) := Xor(shiftRowsOut(i)(j), fullyPipelinedKeyScheduleInstance.io.roundkeys(i)(j))
            }
          }
        }

        // Output handling
        // ---------------------------------------------------
        io.dataOut := ciphertext
        fsm.ready := True
        fsm.valid := True

      }

      // Determine latency
      this.latency = 1 + roundCount * scala.math.max(sboxInstances(0).latency, 1)

      this.reload = 1
    }

    ret
  }
}

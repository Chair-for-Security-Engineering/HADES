import spinal.core._
import spinal.core.internals._
import spinal.lib.Counter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// Regular AES key schedule. AES key is given as input and the roundkey output is updated by repeatedly pulsing the enable signal
case class AESKeySchedule(keySize : Int = 128, mode : String = "encryption-only", shareSboxes : Boolean = false) extends Template {

  // Detect invalid algorithm parameters
  val supportedModes: List[String] = List("encryption-only")
  if (!supportedModes.contains(mode)) throw new IllegalArgumentException("Invalid mode selected.")
  val supportedKeySizes: List[Int] = List(128, 192, 256)
  if (!supportedKeySizes.contains(keySize)) throw new IllegalArgumentException("Invalid keysize selected.")

  // ---------------------------------------------------
  // -- Define IO
  // ---------------------------------------------------
  val io = new Bundle {
    val key      =  in Bits(keySize bits)
    val sboxIn   = shareSboxes generate (in  Vec(spinal.core.Bits(8 bits), 4))
    val sboxOut  = shareSboxes generate (out Vec(spinal.core.Bits(8 bits), 4))
    val roundkey = out Bits(128 bits)
  }

  val fsm = new Bundle {
    val enable             =  in Bool()
    val externalSboxEnable = shareSboxes generate (in Bool()) // If set to high, the sboxIn signal is passed to the sbox instances
    val reset              =  in Bool()
    val valid              = out Bool()
  }

  // ---------------------------------------------------
  // -- Define FSM states
  // ---------------------------------------------------
  object AESKeyScheduleState extends SpinalEnum {
    val sIdle, sUpdate, sUpdateProcessOutputs = newElement()
  }
  import AESKeyScheduleState._

  // ---------------------------------------------------
  // -- Define sub-templates and configuration options
  // ---------------------------------------------------
  var sboxCount: Int = _
  var sboxType: AESSboxFactory = _
  var sboxPipelining: Boolean = _
  var sboxInstances: ArrayBuffer[AESSbox] = ArrayBuffer()

  override def getOptions(): mutable.Map[String, Configurable] = mutable.Map(
    ("keyScheduleSboxCount" -> IntegerList(List(1,2,4))),
    ("keyScheduleSboxType" -> AllSboxFactories),
    ("keyScheduleSboxPipelining" -> AllBooleans)
  )

  override def configure(options: Map[String, Any]): Unit = {
    // Instantiate Sbox instances if sharing with the parent AES template is disabled
    sboxCount = options("keyScheduleSboxCount").asInstanceOf[Int]
    sboxPipelining = options("keyScheduleSboxPipelining").asInstanceOf[Boolean]
    sboxType = options("keyScheduleSboxType").asInstanceOf[AESSboxFactory]

    sboxInstances.clear()
    sboxInstances ++= instantiateMultipleSubTemplates(sboxCount)(sboxType.createTemplate(pipelined = sboxPipelining))
    sboxInstances.foreach(_.io.sboxIn.addTag(share))
    sboxInstances.foreach(_.io.sboxOut.addTag(share))
  }

  // ---------------------------------------------------
  // -- Helper functions
  // ---------------------------------------------------
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
    var ret = true

    // Instantiate sub-templates
    sboxInstances.foreach(ret &&= _.instantiateTemplate())

    // Store the current FSM state
    val state = PubReg(AESKeyScheduleState()) init(sIdle)

    // Signals to handle sbox input MUXing if shareSboxes == 1
    val sboxKeyScheduleInput = Vec(Bits(8 bits), sboxCount)

    // Store the current round constant and compute the next round constant
    val rc = PubReg(Bits(8 bits)) init(B"8'00000001")
    val nextRc = Bits(8 bits)

    // Store the internal key state
    val keyState = Reg(Bits(keySize bits))
    // Wires corresponding to all 32-bit words in the next state
    val wordCount = scala.math.floorDiv(keySize, 32)
    val nextKeyStateWords = Vec(Bits(32 bits), wordCount)

    // Wires for intermediate values
    val w0UpperByteXorRC = Bits(8 bits)
    val rotWordIn = Bits(32 bits)
    val rotWordOut = Bits(32 bits)
    val w0XorGFunctionOut = Bits(32 bits)
    var w4XorHFunctionOut = if (keySize == 256) Bits(32 bits) else null
    var hFunctionIn = if (keySize == 256) Bits(32 bits) else null

    // Keep track of update progress
    val requiredSboxIterationsPerFunction = scala.math.ceil(4f / sboxCount).toInt
    val sboxIterationCounter = Counter(0, requiredSboxIterationsPerFunction - 1)
    val sboxLatencyCounter = Counter(0, sboxInstances(0).latency)

    val keyStateUpdateCond = Bool()

    // Number of different ways in which a round key is generated from the internal state.
    // - AES-128: This is equal to one, since each round key is composed of the full state
    // - AES-192: A round key either contains the first 16 byte of the current state,
    //            the last 16 byte of the current state or the last 8 byte of the previous state
    //            concatenated with the first 8 byte of the current state. Thus, this value equals 3.
    // - AES-256: A round key either contains the first 16 byte or the last 16 byte of the current state.
    //            Thus, this value equals 2
    val keyVariationCount = keySize match {
      case 128 => 1
      case 192 => 3
      case 256 => 2
    }

    var keyVariationCounter = if (keySize != 128) Counter(0, keyVariationCount - 1) else null

    // ---------------------------------------------------
    // -- Internal key state update
    // ---------------------------------------------------
    if (!GlobalConfig.dryRun) {
      sboxInstances.foreach(_.control := control)
      sboxInstances.foreach(_.io.isInverseSbox := False)

      // ---------------------------------------------------
      // Handle sbox sharing
      // ---------------------------------------------------
      if (shareSboxes) {
        // MUX between inputs
        when (state === sIdle && fsm.externalSboxEnable) {
          // Pass external input to the sboxes
          for (i <- 0 until sboxCount) sboxInstances(i).io.sboxIn := io.sboxIn(i)
        } otherwise {
          // Pass internal input to the sboxes
          for (i <- 0 until sboxCount) sboxInstances(i).io.sboxIn := sboxKeyScheduleInput(i)
        }

        // Pass sbox outputs to the outside
        for (i <- 0 until sboxCount) io.sboxOut(i) := sboxInstances(i).io.sboxOut
        for (i <- sboxCount until 4) io.sboxOut(i) := B"8'h00"

      } else {
        // If sbox sharing is disabled, we always connect the internal sbox inputs to the sbox instances
        for (i <- 0 until sboxCount) sboxInstances(i).io.sboxIn := sboxKeyScheduleInput(i)
      }

      // ---------------------------------------------------
      // -- Beginning of key schedule implementation
      // ---------------------------------------------------

      // Compute output of RotWord step
      rotWordIn := keyState(31 downto 0)
      rotWordOut := rotWordIn.rotateLeft(8)

      // Compute result of round constant addition and next round constant
      for (i <- 7 downto 0) w0UpperByteXorRC(i) := Xor(keyState(keySize-8+i), rc(i))
      nextRc := mul2(rc)

      // Set inputs for all sbox instances
      if (sboxCount == 4) {
        if (keySize == 256) {
          // For AES-256, inputs depend on whether we are updating the first or second half of the state
          when (keyVariationCounter.value === 0) {
            // Sbox inputs are equivalent to rotWord outputs
            for (i <- 0 until 4) sboxKeyScheduleInput(i) := rotWordOut((31 - 8*i) downto (24 - 8*i))
          } otherwise {
            // Sbox inputs are equivalent to the fourth-highest key state word
            for (i <- 0 until 4) sboxKeyScheduleInput(i) := keyState((159 - 8*i) downto (152 - 8*i))
          }
        } else {
          for (i <- 0 until 4) sboxKeyScheduleInput(i) := rotWordOut((31 - 8*i) downto (24 - 8*i))
        }
      } else {
        if (keySize == 256) {
          when (keyVariationCounter.value === 0) {
            // Sbox inputs are equivalent to rotWord outputs
            for (i <- 0 until sboxCount) sboxKeyScheduleInput(i) := rotWordOut((31 - 8*i) downto (24 - 8*i))
          } otherwise {
            // Sbox inputs are equivalent to the fourth-highest key state word
            for (i <- 0 until sboxCount) sboxKeyScheduleInput(i) := keyState((159 - 8*i) downto (152 - 8*i))
          }
        } else {
          for (i <- 0 until sboxCount) sboxKeyScheduleInput(i) := rotWordOut((31 - 8*i) downto (24 - 8*i))
        }
      }

      // Special handling for first sbox output onto which the round constant is XORed
      if (requiredSboxIterationsPerFunction > 1) {
        when (sboxIterationCounter.value === 0) {
          for (i <- 31 downto 24) w0XorGFunctionOut(i) := Xor(w0UpperByteXorRC(i-24), sboxInstances(0).io.sboxOut(i-24))
        } otherwise {
          for (i <- 31 downto 24) w0XorGFunctionOut(i) := Xor(keyState(keySize-32 + i), sboxInstances(0).io.sboxOut(i-24))
        }
      } else {
        for (i <- 31 downto 24) w0XorGFunctionOut(i) := Xor(w0UpperByteXorRC(i-24), sboxInstances(0).io.sboxOut(i-24))
      }

      // Use sbox outputs to generate the remaining bytes of the first word by XORing the results onto the previous key state
      for (i <- 1 until scala.math.min(sboxCount, 4)) {
        for (j <- 7 downto 0) {
          w0XorGFunctionOut(24 - 8*i + j) := Xor(keyState(keySize-8 - 8*i + j), sboxInstances(i).io.sboxOut(j))
        }
      }

      // Handle sbox outputs of second SubWord operation in AES-256 key schedule
      if (keySize == 256) {
        for (i <- 0 until sboxCount) {
          for (j <- 7 downto 0) {
            w4XorHFunctionOut(24 - 8*i + j) := Xor(keyState(120 - 8*i + j), sboxInstances(i).io.sboxOut(j))
          }
        }
      }

      if (sboxPipelining) {
        keyStateUpdateCond := (((state === sIdle && fsm.enable) || state === sUpdate) && sboxLatencyCounter.willOverflow) || state === sUpdateProcessOutputs
      } else {
        keyStateUpdateCond := ((state === sIdle && fsm.enable) || state === sUpdate) && sboxLatencyCounter.willOverflow
      }

      // Define h-function input for AES-256
      if (keySize == 256) hFunctionIn := keyState(159 downto 128)

      // If multiple sbox iterations are required, we insert the sbox results into the key state register and
      // rotate the last word (sbox input) and first word (sbox output) by 8 * sboxCount bits.
      if (requiredSboxIterationsPerFunction > 1) {
        // Copy the rest of the original word into the output
        if (sboxCount < 4) {
          w0XorGFunctionOut(31 - 8*sboxCount downto 0) := keyState(keySize-1 - 8*sboxCount downto keySize-32)
          if (keySize == 256) {
            w4XorHFunctionOut(31 - 8*sboxCount downto 0) := keyState(127 - 8*sboxCount downto 96)
          }
        }

        if (sboxPipelining && requiredSboxIterationsPerFunction > 1) {
          // If sbox pipelining is enabled, we rotate the part of the key state that forms the sbox input in every
          // clock cycle in order to feed a new input into each sbox instance.
          when ((state === sIdle && fsm.enable) || state === sUpdate || state === sUpdateProcessOutputs) {
            if (keySize == 128) {
              keyState(31 downto 0) := rotWordIn.rotateLeft(8*sboxCount)
            }

            if (keySize == 192) {
              when (keyVariationCounter.value === 0 || keyVariationCounter.value === 2) {
                keyState(31 downto 0) := rotWordIn.rotateLeft(8*sboxCount)
              }
            }

            if (keySize == 256) {
              when (keyVariationCounter.value === 0) {
                keyState(31 downto 0) := rotWordIn.rotateLeft(8*sboxCount)
              } otherwise {
                keyState(159 downto 128) := hFunctionIn.rotateLeft(8*sboxCount)
              }
            }
          }
        }

        // If sboxPipelining is enabled, the following block is only responsible for output handling
        when (keyStateUpdateCond) {
          if (keySize == 128) {
            keyState(keySize-1 downto keySize-32) := w0XorGFunctionOut.rotateLeft(8*sboxCount)
            if (!sboxPipelining || requiredSboxIterationsPerFunction == 1) keyState(31 downto 0) := rotWordIn.rotateLeft(8*sboxCount)
          }

          if (keySize == 192) {
            when (keyVariationCounter.value === 0 || keyVariationCounter.value === 2) {
              keyState(keySize-1 downto keySize-32) := w0XorGFunctionOut.rotateLeft(8*sboxCount)
              if (!sboxPipelining || requiredSboxIterationsPerFunction == 1) keyState(31 downto 0) := rotWordIn.rotateLeft(8*sboxCount)
            }
          }

          if (keySize == 256) {
            when (keyVariationCounter.value === 0) {
              keyState(keySize-1 downto keySize-32) := w0XorGFunctionOut.rotateLeft(8*sboxCount)
              if (!sboxPipelining || requiredSboxIterationsPerFunction == 1) keyState(31 downto 0) := rotWordIn.rotateLeft(8*sboxCount)
            } otherwise {
              keyState(127 downto 96) := w4XorHFunctionOut.rotateLeft(8*sboxCount)
              if (!sboxPipelining || requiredSboxIterationsPerFunction == 1) keyState(159 downto 128) := hFunctionIn.rotateLeft(8*sboxCount)
            }
          }
        }
      }

      // Compute next key state using previous results
      // ---------------------------------------------------

      // Highest word needs to be rotated if multiple sbox iterations are required since that is where the output of the sbox is stored.
      if (requiredSboxIterationsPerFunction > 1) {
        nextKeyStateWords(wordCount-1)(31 downto 0) := w0XorGFunctionOut.rotateLeft(8*sboxCount)
      } else {
        nextKeyStateWords(wordCount-1)(31 downto 0) := w0XorGFunctionOut
      }

      // Compute middle words via XOR with next higher word
      for (i <- wordCount - 2 downto 1) {
        // For AES-256, we have to include the second SubWords output in the corresponding nextKeyState word
        if (keySize == 256 && i == 3) {
          if (requiredSboxIterationsPerFunction > 1) {
            nextKeyStateWords(i)(31 downto 0) := w4XorHFunctionOut.rotateLeft(8*sboxCount)
          } else {
            nextKeyStateWords(i)(31 downto 0) := w4XorHFunctionOut
          }
        } else {
          for (j <- 31 downto 0) nextKeyStateWords(i)(j) := Xor(keyState(32*i+j), nextKeyStateWords(i+1)(j))
        }
      }

      // Lowest word needs to be rotated if multiple sbox iterations are required since that is where the input of the sbox is stored.
      if (requiredSboxIterationsPerFunction > 1) {
        if (keySize == 128) {
          for (i <- 31 downto 0) nextKeyStateWords(0)(i) := Xor(rotWordIn.rotateLeft(8*sboxCount)(i), nextKeyStateWords(1)(i))
        }

        if (keySize == 192) {
          when (keyVariationCounter.value === 0 || keyVariationCounter.value === 2) {
            for (i <- 31 downto 0) nextKeyStateWords(0)(i) := Xor(rotWordIn.rotateLeft(8*sboxCount)(i), nextKeyStateWords(1)(i))
          } otherwise {
            for (i <- 31 downto 0) nextKeyStateWords(0)(i) := Xor(rotWordIn(i), nextKeyStateWords(1)(i))
          }
        }

        if (keySize == 256) {
          for (i <- 31 downto 0) nextKeyStateWords(0)(i) := Xor(rotWordIn(i), nextKeyStateWords(1)(i))
        }
      } else {
        for (i <- 31 downto 0) nextKeyStateWords(0)(i) := Xor(keyState(i), nextKeyStateWords(1)(i))
      }

      // If we are not using AES-256, we are done at this point
      if (keySize == 128) {
        when(keyStateUpdateCond && sboxIterationCounter.willOverflow) {
          for (i <- wordCount - 1 downto 0) keyState(32 * i + 31 downto 32 * i) := nextKeyStateWords(i)
          rc := nextRc
        }
      }

      if (keySize == 192) {
        // Adapt nextKeyStateWords depending on the current keyVariationCounter value
        switch (keyVariationCounter.value) {
          is (0) {
            // We only used the first 4 words of the current key state to form the current round key. Thus, the last 2 words
            // are still going to be used in the next iteration. We thus only update the first 4 words of the round key.
            nextKeyStateWords(1) := keyState(63 downto 32)
            if (requiredSboxIterationsPerFunction > 1) {
              nextKeyStateWords(0) := rotWordIn.rotateLeft(8*sboxCount)
            } else {
              nextKeyStateWords(0) := keyState(31 downto 0)
            }
          }

          is (1) {
            // We used the last 2 words of the previous key state together with the first 2 words of the new state to form
            // the current round key. Since the previous update already computed the first 4 words of the new state,
            // we only need to perform a partial update that computes the final 2 words of the current state.
            nextKeyStateWords(2)(31 downto 0) := keyState(95 downto 64)
            nextKeyStateWords(3)(31 downto 0) := keyState(127 downto 96)
            nextKeyStateWords(4)(31 downto 0) := keyState(159 downto 128)
            nextKeyStateWords(5)(31 downto 0) := keyState(191 downto 160)
          }

          // is (2): We just used the last four words of the current state to form a round key. We thus need to update the entire state
        }

        // Perform the actual key state update
        when(keyStateUpdateCond && sboxIterationCounter.willOverflow) {
          for (i <- wordCount - 1 downto 0) keyState(32 * i + 31 downto 32 * i) := nextKeyStateWords(i)
          when (keyVariationCounter.value === 1 || keyVariationCounter.value === 2) {
            rc := nextRc
          }
        }
      }

      if (keySize == 256) {
        switch (keyVariationCounter.value) {
          is (0) {
            // We only update the first 4 words of the key state. The last four words stay the same
            nextKeyStateWords(3)(31 downto 0) := keyState(127 downto 96)
            nextKeyStateWords(2)(31 downto 0) := keyState(95 downto 64)
            nextKeyStateWords(1)(31 downto 0) := keyState(63 downto 32)
            if (requiredSboxIterationsPerFunction > 1) {
              nextKeyStateWords(0) := rotWordIn.rotateLeft(8*sboxCount)
            } else {
              nextKeyStateWords(0) := keyState(31 downto 0)
            }
          }

          is (1) {
            // We only update the last 4 words of the key state. The first four words stay the same
            nextKeyStateWords(7)(31 downto 0) := keyState(255 downto 224)
            nextKeyStateWords(6)(31 downto 0) := keyState(223 downto 192)
            nextKeyStateWords(5)(31 downto 0) := keyState(191 downto 160)
            if (requiredSboxIterationsPerFunction > 1) {
              nextKeyStateWords(4)(31 downto 0) := hFunctionIn.rotateLeft(8*sboxCount)
            } else {
              nextKeyStateWords(4)(31 downto 0) := keyState(159 downto 128)
            }
          }
        }

        // Perform the actual key state update
        when(keyStateUpdateCond && sboxIterationCounter.willOverflow) {
          for (i <- wordCount - 1 downto 0) keyState(32 * i + 31 downto 32 * i) := nextKeyStateWords(i)
          when (keyVariationCounter.value === 0) {
            rc := nextRc
          }
        }
      }

      // ---------------------------------------------------
      // -- Output handling
      // ---------------------------------------------------
      keySize match {
        case 128 => io.roundkey(127 downto 0) := keyState(127 downto 0)
        case 192 =>
          // Assignments for keyVariationCounter.value == 0 is added as a base case to avoid latches
          io.roundkey(127 downto 0) := keyState(191 downto 64)

          switch (keyVariationCounter.value) {
            is (1) {
              io.roundkey(127 downto 64) := keyState(63 downto 0)
              io.roundkey(63 downto 0) := keyState(191 downto 128)
            }

            is(2) {
              io.roundkey(127 downto 0) := keyState(127 downto 0)
            }
          }
        case 256 =>
          // Assignments for keyVariationCounter.value == 0 is added as a base case to avoid latches
          io.roundkey(127 downto 0) := keyState(255 downto 128)

          switch (keyVariationCounter.value) {
            is (1) {
              io.roundkey(127 downto 0) := keyState(127 downto 0)
            }
          }
      }

      // ---------------------------------------------------
      // -- FSM
      // ---------------------------------------------------
      fsm.valid := False

      switch (state) {
        is (sIdle) {
          fsm.valid := True
          when (fsm.enable) {
            fsm.valid := False
            state := sUpdate
            sboxLatencyCounter.increment()
            when (sboxLatencyCounter.willOverflow) {
              if (sboxPipelining && requiredSboxIterationsPerFunction > 1) state := sUpdateProcessOutputs
              sboxIterationCounter.increment()
              when (sboxIterationCounter.willOverflow) {
                state := sIdle

                if (keySize == 192 || keySize == 256) keyVariationCounter.increment()
              }
            }
          }
        }

        is (sUpdate) {
          sboxLatencyCounter.increment()
          when (sboxLatencyCounter.willOverflow) {
            if (sboxPipelining && requiredSboxIterationsPerFunction > 1) state := sUpdateProcessOutputs
            sboxIterationCounter.increment()
            when (sboxIterationCounter.willOverflow) {
              state := sIdle

              if (keySize == 192 || keySize == 256) keyVariationCounter.increment()
            }
          }
        }

        if (sboxPipelining && requiredSboxIterationsPerFunction > 1) {
          is (sUpdateProcessOutputs) {
            sboxIterationCounter.increment()
            when (sboxIterationCounter.willOverflow) {
              state := sIdle

              if (keySize == 192 || keySize == 256) keyVariationCounter.increment()
            }
          }
        }
      }

      // Handle reset
      when (fsm.reset) {
        state := sIdle
        fsm.valid := False
        // Load key into state register
        keyState := io.key
        // Reset round constant
        rc := B"8'00000001"
        // Reset counters
        sboxLatencyCounter.clear()
        sboxIterationCounter.clear()
        if (keySize != 128) keyVariationCounter.clear()
      }
    }

    // Specify latency and reload
    if (sboxPipelining) {
      this.latency = (sboxInstances(0).latency + 1) + (requiredSboxIterationsPerFunction - 1)
    } else {
      this.latency = requiredSboxIterationsPerFunction * (sboxInstances(0).latency + 1)
    }
    this.reload = this.latency

    ret
  }

}

// AES key schedule step for fully-pipelined configurations. Takes the current key state and computes the updated key state
case class AESFullyPipelinedKeyScheduleStep(keySize : Int = 128, mode : String = "encryption-only") extends Template {

  // Detect invalid algorithm parameters
  val supportedModes: List[String] = List("encryption-only")
  if (!supportedModes.contains(mode)) throw new IllegalArgumentException("Invalid mode selected.")
  val supportedKeySizes: List[Int] = List(128, 192, 256)
  if (!supportedKeySizes.contains(keySize)) throw new IllegalArgumentException("Invalid keysize selected.")

  // ---------------------------------------------------
  // -- Define IO
  // ---------------------------------------------------
  val io = new Bundle {
    val keyStateIn  =  in Bits(keySize bits)
    val rcon        =  in Bits(8 bits)
    val keyStateOut = out Bits(keySize bits)
  }

  // ---------------------------------------------------
  // -- Define sub-templates and configuration options
  // ---------------------------------------------------
  val sboxCount: Int = if (keySize == 256) 8 else 4
  var sboxInstances: ArrayBuffer[AESSbox] = ArrayBuffer()

  override def getOptions(): mutable.Map[String, Configurable] = mutable.Map(
    ("keyScheduleSboxType" -> AllSboxFactories)
  )

  override def configure(options: Map[String, Any]): Unit = {
    val sboxType = options("keyScheduleSboxType").asInstanceOf[AESSboxFactory]
    sboxInstances.clear()
    sboxInstances ++= instantiateMultipleSubTemplates(sboxCount)(sboxType.createTemplate(pipelined = true))
    sboxInstances.foreach(_.io.sboxIn.addTag(share))
    sboxInstances.foreach(_.io.sboxOut.addTag(share))
  }

  // ---------------------------------------------------
  // -- Logic Definition
  // ---------------------------------------------------
  override def instantiate(): Boolean = {
    var ret = true

    // Instantiate sub-templates
    sboxInstances.foreach(ret &&= _.instantiateTemplate())

    // Registers

    // AES-256 requires 2 consecutive sbox applications for its state update
    val keyStateInDelayRegs =
      if (sboxInstances(0).latency > 0) Vec(Reg(Bits(keySize bits)), if (keySize == 256) 2 * sboxInstances(0).latency else sboxInstances(0).latency)
      else null

    // Intermediate values
    val rotWordIn = Bits(32 bits)
    val rotWordOut = Bits(32 bits)
    val subWordOut = Bits(32 bits)
    val subWord2Out = if (keySize == 256) Bits(32 bits) else null

    // We need this vector to avoid false-positive combinatorial loop errors when computing the output state
    val nextKeyStateWords = Vec(Bits(32 bits), scala.math.floorDiv(keySize, 32))
    // If sbox latency is greater than 0, we need to delay the input key state accordingly
    val delayedKeyStateIn = Bits(keySize bits)
    val delayedKeyStateIn2 = if (keySize == 256) Bits(keySize bits) else null

    if (!GlobalConfig.dryRun) {
      // Connect sub-templates
      sboxInstances.foreach(_.control := control)
      sboxInstances.foreach(_.io.isInverseSbox := False)

      // Define common part of all AES key schedules
      // ---------------------------------------------------

      // Compute RotWord
      rotWordIn := io.keyStateIn(31 downto 0)
      rotWordOut := rotWordIn.rotateLeft(8)

      // Compute SubWord
      for (i <- 0 until 4) {
        sboxInstances(i).io.sboxIn := rotWordOut(8*i+7 downto 8*i)
        subWordOut(8*i+7 downto 8*i) := sboxInstances(i).io.sboxOut
      }

      // Delay some signals so that they arrive at the correct time
      if (sboxInstances(0).latency > 0) {
        // Directly add round constant onto current state
        for (i <- keySize-1 downto keySize-8) keyStateInDelayRegs(0)(i) := Xor(io.keyStateIn(i), io.rcon(i - (keySize-8)))
        for (i <- keySize-9 downto 0) keyStateInDelayRegs(0)(i) := io.keyStateIn(i)

        for (i <- 1 until keyStateInDelayRegs.length) keyStateInDelayRegs(i) := keyStateInDelayRegs(i-1)
        delayedKeyStateIn := keyStateInDelayRegs(sboxInstances(0).latency - 1)
        if (keySize == 256) {
          delayedKeyStateIn2 := keyStateInDelayRegs.last
          keyStateInDelayRegs(sboxInstances(0).latency)(255 downto 224) := nextKeyStateWords(0)
          keyStateInDelayRegs(sboxInstances(0).latency)(223 downto 192) := nextKeyStateWords(1)
          keyStateInDelayRegs(sboxInstances(0).latency)(191 downto 160) := nextKeyStateWords(2)
          keyStateInDelayRegs(sboxInstances(0).latency)(159 downto 128) := nextKeyStateWords(3)
        }

      } else {
        // Directly add round constant onto current state
        for (i <- keySize-1 downto keySize-8) delayedKeyStateIn(i) := Xor(io.keyStateIn(i), io.rcon(i - (keySize-8)))
        for (i <- keySize-9 downto 0) delayedKeyStateIn(i) := io.keyStateIn(i)
        if (keySize == 256) delayedKeyStateIn2 := delayedKeyStateIn
      }

      // Compute output word 0
      for (i <- 31 downto 0) nextKeyStateWords(0)(i) := Xor(delayedKeyStateIn(i + keySize - 32), subWordOut(i))

      // Compute output words 1-3
      for (i <- 1 until 4) {
        for (j <- 31 downto 0) nextKeyStateWords(i)(j) := Xor(delayedKeyStateIn(keySize - 32 * (i + 1) + j), nextKeyStateWords(i-1)(j))
      }

      // Define key size specific part of AES key schedule
      // ---------------------------------------------------
      keySize match {
        case 192 =>
          // Compute remaining two words just like the previous words
          for (i <- 4 until 6) {
            for (j <- 31 downto 0) nextKeyStateWords(i)(j) := Xor(delayedKeyStateIn(keySize - 32 * (i + 1) + j), nextKeyStateWords(i-1)(j))
          }

        case 256 =>
          // Compute SubWord2
          for (i <- 0 until 4) {
            sboxInstances(4+i).io.sboxIn := nextKeyStateWords(3)(8*i+7 downto 8*i)
            subWord2Out(8*i+7 downto 8*i) := sboxInstances(4+i).io.sboxOut
          }

          // Compute output word 4
          for (i <- 31 downto 0) nextKeyStateWords(4)(i) := Xor(delayedKeyStateIn2(i + 96), subWord2Out(i))

          // Compute remaining three output words
          for (i <- 5 until 8) {
            for (j <- 31 downto 0) nextKeyStateWords(i)(j) := Xor(delayedKeyStateIn2(keySize - 32 * (i + 1) + j), nextKeyStateWords(i-1)(j))
          }
        case _ =>
      }

      // Assign output
      // ---------------------------------------------------
      for (i <- 0 until scala.math.floorDiv(keySize, 32)) {
        io.keyStateOut(keySize-1 - 32*i downto keySize-32 - 32*i) := nextKeyStateWords(i)
      }

      // If AES-256 is used, we take the first 4 output words from the delay register
      if (keySize == 256 && sboxInstances(0).latency > 0) {
        io.keyStateOut(keySize-1 downto 128) := delayedKeyStateIn2(keySize-1 downto 128)
      }
    }

    this.latency = if (keySize == 256) 2 * sboxInstances(0).latency else sboxInstances(0).latency
    this.reload = 1

    ret
  }
}

// Fully-pipelined AES key schedule that computes all round keys
case class AESFullyPipelinedKeySchedule(keySize : Int = 128, mode : String = "encryption-only") extends Template {

  // Detect invalid algorithm parameters
  val supportedModes: List[String] = List("encryption-only")
  if (!supportedModes.contains(mode)) throw new IllegalArgumentException("Invalid mode selected.")
  val supportedKeySizes: List[Int] = List(128, 192, 256)
  if (!supportedKeySizes.contains(keySize)) throw new IllegalArgumentException("Invalid keysize selected.")

  // ---------------------------------------------------
  // -- Define some constants
  // ---------------------------------------------------
  val roundCount = keySize match {
    case 128 => 10
    case 192 => 12
    case 256 => 14
  }

  val keyScheduleStepCount = keySize match {
    case 128 => 10
    case 192 => 8
    case 256 => 7
  }

  // ---------------------------------------------------
  // -- Define IO
  // ---------------------------------------------------
  val io = new Bundle {
    val key       =  in Bits(keySize bits)
    val roundkeys = out Vec(spinal.core.Bits(128 bits), roundCount)
  }

  // ---------------------------------------------------
  // -- Define sub-templates and configuration options
  // ---------------------------------------------------
  val keyScheduleStepInstances = instantiateMultipleSubTemplates(keyScheduleStepCount)(AESFullyPipelinedKeyScheduleStep(keySize, mode))
  keyScheduleStepInstances.foreach(_.io.keyStateIn.addTag(share))
  keyScheduleStepInstances.foreach(_.io.keyStateOut.addTag(share))

  override def instantiate(): Boolean = {
    var ret = true

    // Instantiate sub-templates
    // ---------------------------------------------------
    keyScheduleStepInstances.foreach(ret &&= _.instantiateTemplate())

    // Registers
    // ---------------------------------------------------
    val keyScheduleStates = Vec(Reg(Bits(keySize bits)), roundCount)
    // The AES-192 key schedule generates 6 words per iteration. However, every round key only consists of 4 words.
    // There are thus some round keys that consist of 2 words from the previous state followed by 2 words from the
    // current state. In order to properly handle this, we also save the previous key state half for these rounds.
    // => We have to delay these values by that many cycles such that they arrive at the same time as the next
    //    key schedule step result.
    val previousKeyScheduleStateHalves = if (keySize == 192) Vec(Vec(Reg(Bits(64 bits)), scala.math.max(keyScheduleStepInstances(0).latency, 1)), 4) else null

    val keyScheduleStateDelayRegs = if (keySize == 192 || keySize == 256) {
      // If no key schedule state update is required in a specific cycle, we still have to delay the key state
      // by as many cycles as a regular key schedule update would take. Otherwise, the corresponding round key
      // would arrive too early.
      val regCount = keySize match {
        case 192 => 4
        case 256 => 7
        case _ => 0
      }
      if (keyScheduleStepInstances(0).latency > 1) {
        val regStages = if (keySize == 192) keyScheduleStepInstances(0).latency else keyScheduleStepInstances(0).latency - 1
        Vec(Vec(Reg(Bits(keySize bits)), regStages), regCount)
      } else {
        Vec(Vec(Bits(keySize bits), 1), regCount)
      }
    } else null

    val keyScheduleUpdatedStateDelayRegs = if (keySize == 192 || keySize == 256) {
      // The updated key state must arrive at least one cycle after starting the update.
      // Otherwise, timing of generated round keys is off. To achieve this, we insert
      // an additional register stage if each keyScheduleStepInstance does not inherently
      // contain any registers.
      val regCount = keySize match {
        case 192 => 8
        case 256 => 7
        case _ => 0
      }
      if (keyScheduleStepInstances(0).latency == 0) {
        Vec(Vec(Reg(Bits(keySize bits)), 1), regCount)
      } else {
        Vec(Vec(Bits(keySize bits), 1), regCount)
      }
    } else null


    // Other intermediate values
    // ---------------------------------------------------
    val nextKeyScheduleStates = Vec(Bits(keySize bits), roundCount)

    if (!GlobalConfig.dryRun) {
      keyScheduleStepInstances.foreach(_.control := control)

      // Set round constant inputs for key schedule step instances
      // Define round constants
      val rcons = Array(B"8'h01", B"8'h02", B"8'h04", B"8'h08", B"8'h10", B"8'h20", B"8'h40", B"8'h80", B"8'h1b", B"8'h36")
      for (i <- 0 until keyScheduleStepCount) keyScheduleStepInstances(i).io.rcon := rcons(i)

      // Wire key state delay regs
      if (keySize == 192 || keySize == 256) {
        for (i <- 0 until keyScheduleStateDelayRegs.length) {
          for (j <- 1 until keyScheduleStateDelayRegs(0).length) {
            keyScheduleStateDelayRegs(i)(j) := keyScheduleStateDelayRegs(i)(j-1)
          }
        }
      }

      // Handle key state updates
      // ---------------------------------------------------
      keySize match {
        case 128 =>
          // Key state updates are applied every cycle
          keyScheduleStepInstances(0).io.keyStateIn := io.key
          nextKeyScheduleStates(0) := keyScheduleStepInstances(0).io.keyStateOut
          for (i <- 1 until roundCount) {
            if (keyScheduleStepInstances(0).latency == 0) {
              keyScheduleStepInstances(i).io.keyStateIn := keyScheduleStates(i-1)
            } else {
              keyScheduleStepInstances(i).io.keyStateIn := nextKeyScheduleStates(i-1)
            }
            nextKeyScheduleStates(i) := keyScheduleStepInstances(i).io.keyStateOut
          }

        case 192 =>
          // Updates are only sometimes required
          var keyScheduleStateDelayRegsIdx = 0
          var keyScheduleStepInstanceIdx = 0
          for (i <- 0 until 12) {
            if (List(1,4,7,10).contains(i)) {
              // No updates are required
              val stateIn = if (keyScheduleStepInstances(0).latency == 0) keyScheduleStates else nextKeyScheduleStates
              keyScheduleStateDelayRegs(keyScheduleStateDelayRegsIdx)(0) := stateIn(i-1)
              nextKeyScheduleStates(i) := keyScheduleStateDelayRegs(keyScheduleStateDelayRegsIdx).last

              keyScheduleStateDelayRegsIdx += 1
            } else {
              // Key state updates are required
              if (i == 0) {
                keyScheduleStepInstances(0).io.keyStateIn := io.key
                keyScheduleUpdatedStateDelayRegs(0)(0) := keyScheduleStepInstances(0).io.keyStateOut
                nextKeyScheduleStates(0) := keyScheduleUpdatedStateDelayRegs(0).last
              } else {
                keyScheduleStepInstances(keyScheduleStepInstanceIdx).io.keyStateIn := nextKeyScheduleStates(i-1)
                keyScheduleUpdatedStateDelayRegs(keyScheduleStepInstanceIdx)(0) := keyScheduleStepInstances(keyScheduleStepInstanceIdx).io.keyStateOut
                nextKeyScheduleStates(i) := keyScheduleUpdatedStateDelayRegs(keyScheduleStepInstanceIdx).last
              }

              keyScheduleStepInstanceIdx += 1
            }
          }

        case 256 =>
          // Key state updates only need to be applied every second cycle
          keyScheduleStateDelayRegs(0)(0) := io.key
          nextKeyScheduleStates(0) := keyScheduleStateDelayRegs(0).last
          for (i <- 0 until scala.math.floorDiv(roundCount, 2)) {
            keyScheduleStepInstances(i).io.keyStateIn := nextKeyScheduleStates(2*i)
            keyScheduleUpdatedStateDelayRegs(i)(0) := keyScheduleStepInstances(i).io.keyStateOut
            nextKeyScheduleStates(2*i + 1) := keyScheduleUpdatedStateDelayRegs(i).last

            if (i < 6) {
              keyScheduleStateDelayRegs(i+1)(0) := keyScheduleStates(2*i + 1)
              nextKeyScheduleStates(2*i + 2) := keyScheduleStateDelayRegs(i+1).last
            }
          }
      }

      for (i <- 0 until roundCount) keyScheduleStates(i) := nextKeyScheduleStates(i)

      // Special previous key state handling for AES-192
      // ---------------------------------------------------
      if (keySize == 192) {
        previousKeyScheduleStateHalves(0)(0) := io.key(63 downto 0)
        previousKeyScheduleStateHalves(1)(0) := nextKeyScheduleStates(2)(63 downto 0)
        previousKeyScheduleStateHalves(2)(0) := nextKeyScheduleStates(5)(63 downto 0)
        previousKeyScheduleStateHalves(3)(0) := nextKeyScheduleStates(8)(63 downto 0)

        // Implement delay registers
        for (i <- 0 until 4) {
          for (j <- 1 until previousKeyScheduleStateHalves(0).length) {
            previousKeyScheduleStateHalves(i)(j) := previousKeyScheduleStateHalves(i)(j-1)
          }
        }
      }

      // Assign round keys
      // ---------------------------------------------------
      keySize match {
        case 128 =>
          for (i <- 0 until roundCount) {
            if (keyScheduleStepInstances(0).latency == 0) {
              io.roundkeys(i) := keyScheduleStates(i)
            } else {
              io.roundkeys(i) := nextKeyScheduleStates(i)
            }
          }
        case 192 =>
          for (i <- 0 until 4) {
            io.roundkeys(3*i)(127 downto 64) := previousKeyScheduleStateHalves(i).last
            io.roundkeys(3*i)( 63 downto  0) := nextKeyScheduleStates(3*i)(191 downto 128)

            io.roundkeys(3*i + 1)(127 downto  0) := nextKeyScheduleStates(3*i + 1)(127 downto   0)

            io.roundkeys(3*i + 2)(127 downto  0) := nextKeyScheduleStates(3*i + 2)(191 downto  64)
          }
        case 256 =>
          for (i <- 0 until roundCount) {
            if (mod(i, 2) == 0) {
              io.roundkeys(i) := keyScheduleStates(i)(127 downto   0)
            } else {
              io.roundkeys(i) := keyScheduleStates(i)(255 downto 128)
            }
          }
      }
    }

    this.latency = scala.math.max(keyScheduleStepInstances(0).latency, 1)
    this.reload = 1

    ret
  }
}

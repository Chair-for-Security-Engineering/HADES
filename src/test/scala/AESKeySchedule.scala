import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core.{Component, IntToBuilder, SpinalProgress}

class AESKeyScheduleTest extends AnyFunSuite {
  test("Testing AES-128 Key Schedule") {
    HADES("HADES")(AESKeySchedule(keySize = 128))(keyschedule128Sim).apply()
  }

  test("Testing AES-192 Key Schedule") {
    HADES("HADES")(AESKeySchedule(keySize = 192))(keyschedule192Sim).apply()
  }

  test("Testing AES-256 Key Schedule") {
    HADES("HADES")(AESKeySchedule(keySize = 256))(keyschedule256Sim).apply()
  }

  test("Testing AES-128 Key Schedule with sbox sharing") {
    HADES("HADES")(AESKeySchedule(keySize = 128, mode = "encryption-only", shareSboxes = true))(keySchedule128SboxSharingSim).apply()
  }

  def keyschedule128Sim(dut: Component): Unit = {
    val top = dut.asInstanceOf[AESKeySchedule]
    top.cd.forkStimulus(10)

    // Test vector taken from Appendix A of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
    val key = BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16)

    // These outputs correspond to all round keys
    val expectedOutputs = Array(
      BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16),
      BigInt("a0fafe1788542cb123a339392a6c7605", 16),
      BigInt("f2c295f27a96b9435935807a7359f67f", 16),
      BigInt("3d80477d4716fe3e1e237e446d7a883b", 16),
      BigInt("ef44a541a8525b7fb671253bdb0bad00", 16),
      BigInt("d4d1c6f87c839d87caf2b8bc11f915bc", 16),
      BigInt("6d88a37a110b3efddbf98641ca0093fd", 16),
      BigInt("4e54f70e5f5fc9f384a64fb24ea6dc4f", 16),
      BigInt("ead27321b58dbad2312bf5607f8d292f", 16),
      BigInt("ac7766f319fadc2128d12941575c006e", 16),
      BigInt("d014f9a8c9ee2589e13f0cc8b6630ca6", 16)
    )

    top.fsm.enable #= false

    // Load key
    top.io.key #= key
    top.fsm.reset #= true
    top.cd.waitSampling(1)
    top.fsm.reset #= false
    top.cd.waitSampling(1)

    // Wait for execution
    for (i <- expectedOutputs.indices) {
      assert(top.fsm.valid.toBoolean)
      assert(top.io.roundkey.toBigInt == expectedOutputs(i), f"RoundKey $i is incorrect")
      top.fsm.enable #= true
      top.cd.waitSampling(1)
      top.fsm.enable #= false
      top.cd.waitSampling(top.latency)
    }

    SpinalProgress(s"Tests passed")
  }

  def keyschedule192Sim(dut: Component): Unit = {
    val top = dut.asInstanceOf[AESKeySchedule]
    top.cd.forkStimulus(10)

    // Test vector taken from Appendix A of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
    val key = BigInt("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b", 16)

    // These outputs correspond to all round keys
    val expectedOutputs = Array(
      BigInt("8e73b0f7da0e6452c810f32b809079e5", 16),
      BigInt("62f8ead2522c6b7bfe0c91f72402f5a5", 16),
      BigInt("ec12068e6c827f6b0e7a95b95c56fec2", 16),
      BigInt("4db7b4bd69b5411885a74796e92538fd", 16),
      BigInt("e75fad44bb095386485af05721efb14f", 16),
      BigInt("a448f6d94d6dce24aa326360113b30e6", 16),
      BigInt("a25e7ed583b1cf9a27f939436a94f767", 16),
      BigInt("c0a69407d19da4e1ec1786eb6fa64971", 16),
      BigInt("485f703222cb8755e26d135233f0b7b3", 16),
      BigInt("40beeb282f18a2596747d26b458c553e", 16),
      BigInt("a7e1466c9411f1df821f750aad07d753", 16),
      BigInt("ca4005388fcc5006282d166abc3ce7b5", 16),
      BigInt("e98ba06f448c773c8ecc720401002202", 16)
    )

    top.fsm.enable #= false

    // Load key
    top.io.key #= key
    top.fsm.reset #= true
    top.cd.waitSampling(1)
    top.fsm.reset #= false
    top.cd.waitSampling(1)

    // Wait for execution
    for (i <- expectedOutputs.indices) {
      assert(top.fsm.valid.toBoolean)
      assert(top.io.roundkey.toBigInt == expectedOutputs(i), f"RoundKey $i is incorrect")
      top.fsm.enable #= true
      top.cd.waitSampling(1)
      top.fsm.enable #= false
      top.cd.waitSampling(top.latency)
    }

    SpinalProgress(s"Tests passed")
  }

  def keyschedule256Sim(dut: Component): Unit = {
    val top = dut.asInstanceOf[AESKeySchedule]
    top.cd.forkStimulus(10)

    // Test vector taken from Appendix A of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
    val key = BigInt("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4", 16)

    // These outputs correspond to all round keys
    val expectedOutputs = Array(
      BigInt("603deb1015ca71be2b73aef0857d7781", 16),
      BigInt("1f352c073b6108d72d9810a30914dff4", 16),
      BigInt("9ba354118e6925afa51a8b5f2067fcde", 16),
      BigInt("a8b09c1a93d194cdbe49846eb75d5b9a", 16),
      BigInt("d59aecb85bf3c917fee94248de8ebe96", 16),
      BigInt("b5a9328a2678a647983122292f6c79b3", 16),
      BigInt("812c81addadf48ba24360af2fab8b464", 16),
      BigInt("98c5bfc9bebd198e268c3ba709e04214", 16),
      BigInt("68007bacb2df331696e939e46c518d80", 16),
      BigInt("c814e20476a9fb8a5025c02d59c58239", 16),
      BigInt("de1369676ccc5a71fa2563959674ee15", 16),
      BigInt("5886ca5d2e2f31d77e0af1fa27cf73c3", 16),
      BigInt("749c47ab18501ddae2757e4f7401905a", 16),
      BigInt("cafaaae3e4d59b349adf6acebd10190d", 16),
      BigInt("fe4890d1e6188d0b046df344706c631e", 16)
    )

    top.fsm.enable #= false

    // Load key
    top.io.key #= key
    top.fsm.reset #= true
    top.cd.waitSampling(1)
    top.fsm.reset #= false
    top.cd.waitSampling(1)

    // Wait for execution
    for (i <- expectedOutputs.indices) {
      assert(top.fsm.valid.toBoolean)
      assert(top.io.roundkey.toBigInt == expectedOutputs(i), f"RoundKey $i is incorrect")
      top.fsm.enable #= true
      top.cd.waitSampling(1)
      top.fsm.enable #= false
      top.cd.waitSampling(top.latency)
    }

    SpinalProgress(s"Tests passed")
  }

  def keySchedule128SboxSharingSim(dut: Component): Unit ={
    val top = dut.asInstanceOf[AESKeySchedule]
    top.cd.forkStimulus(10)

    // Test vector taken from Appendix A of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
    val key = BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16)

    // These outputs correspond to all round keys
    val expectedOutputs = Array(
      BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16),
      BigInt("a0fafe1788542cb123a339392a6c7605", 16),
      BigInt("f2c295f27a96b9435935807a7359f67f", 16),
      BigInt("3d80477d4716fe3e1e237e446d7a883b", 16),
      BigInt("ef44a541a8525b7fb671253bdb0bad00", 16),
      BigInt("d4d1c6f87c839d87caf2b8bc11f915bc", 16),
      BigInt("6d88a37a110b3efddbf98641ca0093fd", 16),
      BigInt("4e54f70e5f5fc9f384a64fb24ea6dc4f", 16),
      BigInt("ead27321b58dbad2312bf5607f8d292f", 16),
      BigInt("ac7766f319fadc2128d12941575c006e", 16),
      BigInt("d014f9a8c9ee2589e13f0cc8b6630ca6", 16)
    )

    top.fsm.enable #= false
    top.fsm.externalSboxEnable #= false

    // Load key
    top.io.key #= key
    top.fsm.reset #= true
    top.cd.waitSampling(1)
    top.fsm.reset #= false
    top.cd.waitSampling(1)

    // Wait for execution
    for (i <- expectedOutputs.indices) {
      assert(top.fsm.valid.toBoolean)
      assert(top.io.roundkey.toBigInt == expectedOutputs(i), f"RoundKey $i is incorrect")
      top.fsm.enable #= true
      top.cd.waitSampling(1)
      top.fsm.enable #= false
      top.cd.waitSampling(top.latency)
    }

    SpinalProgress(s"Tests passed")
  }

}

class AESFullyPipelinedKeyScheduleStepTest extends AnyFunSuite {
  test("Testing AES-128 Key Schedule Step") {
    HADES("HADES")(AESFullyPipelinedKeyScheduleStep(keySize = 128))(keyScheduleStep128Sim).apply()
  }

  test("Testing AES-192 Key Schedule Step") {
    HADES("HADES")(AESFullyPipelinedKeyScheduleStep(keySize = 192))(keyScheduleStep192Sim).apply()
  }

  test("Testing AES-256 Key Schedule Step") {
    HADES("HADES")(AESFullyPipelinedKeyScheduleStep(keySize = 256))(keyScheduleStep256Sim).apply()
  }

  def keyScheduleStep128Sim(dut: Component): Unit = {
    val top = dut.asInstanceOf[AESFullyPipelinedKeyScheduleStep]
    top.cd.forkStimulus(10)

    val stateIn = BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16)
    val expected = BigInt("a0fafe1788542cb123a339392a6c7605", 16)

    top.io.keyStateIn #= 0
    top.io.rcon #= 0
    top.cd.waitSampling(1)
    top.io.keyStateIn #= stateIn
    top.io.rcon #= 1
    top.cd.waitSampling(1)
    top.io.keyStateIn #= 0
    top.io.rcon #= 0
    top.cd.waitSampling(top.latency)
    assert(top.io.keyStateOut.toBigInt == expected)
    SpinalProgress(s"Tests passed")
  }

  def keyScheduleStep192Sim(dut: Component): Unit = {
    val top = dut.asInstanceOf[AESFullyPipelinedKeyScheduleStep]
    top.cd.forkStimulus(10)

    val stateIn = BigInt("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b", 16)
    val expected = BigInt("fe0c91f72402f5a5ec12068e6c827f6b0e7a95b95c56fec2", 16)

    top.io.keyStateIn #= 0
    top.io.rcon #= 0
    top.cd.waitSampling(1)
    top.io.keyStateIn #= stateIn
    top.io.rcon #= 1
    top.cd.waitSampling(1)
    top.io.keyStateIn #= 0
    top.io.rcon #= 0
    top.cd.waitSampling(top.latency)
    assert(top.io.keyStateOut.toBigInt == expected)
    SpinalProgress(s"Tests passed")
  }

  def keyScheduleStep256Sim(dut: Component): Unit = {
    val top = dut.asInstanceOf[AESFullyPipelinedKeyScheduleStep]
    top.cd.forkStimulus(10)

    val stateIn = BigInt("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4", 16)
    val expected = BigInt("9ba354118e6925afa51a8b5f2067fcdea8b09c1a93d194cdbe49846eb75d5b9a", 16)

    top.io.keyStateIn #= 0
    top.io.rcon #= 0
    top.cd.waitSampling(1)
    top.io.keyStateIn #= stateIn
    top.io.rcon #= 1
    top.cd.waitSampling(1)
    top.io.keyStateIn #= 0
    top.io.rcon #= 0
    top.cd.waitSampling(top.latency)
    assert(top.io.keyStateOut.toBigInt == expected)
    SpinalProgress(s"Tests passed")
  }
}

class AESFullyPipelinedKeyScheduleTest extends AnyFunSuite {
  test("Testing AES-128 fully-pipelined key schedule") {
    HADES("HADES")(AESFullyPipelinedKeySchedule(keySize = 128))(keySchedule128Sim).apply()
  }

  test("Testing AES-192 fully-pipelined key schedule") {
    HADES("HADES")(AESFullyPipelinedKeySchedule(keySize = 192))(keySchedule192Sim).apply()
  }

  test("Testing AES-256 fully-pipelined key schedule") {
    HADES("HADES")(AESFullyPipelinedKeySchedule(keySize = 256))(keySchedule256Sim).apply()
  }

  def keySchedule128Sim(dut: Component): Unit = {
    val top = dut.asInstanceOf[AESFullyPipelinedKeySchedule]
    top.cd.forkStimulus(10)

    // Test vector taken from Appendix A of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
    val key = BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16)

    // These outputs correspond to all round keys
    val expected = Array(
      BigInt("a0fafe1788542cb123a339392a6c7605", 16),
      BigInt("f2c295f27a96b9435935807a7359f67f", 16),
      BigInt("3d80477d4716fe3e1e237e446d7a883b", 16),
      BigInt("ef44a541a8525b7fb671253bdb0bad00", 16),
      BigInt("d4d1c6f87c839d87caf2b8bc11f915bc", 16),
      BigInt("6d88a37a110b3efddbf98641ca0093fd", 16),
      BigInt("4e54f70e5f5fc9f384a64fb24ea6dc4f", 16),
      BigInt("ead27321b58dbad2312bf5607f8d292f", 16),
      BigInt("ac7766f319fadc2128d12941575c006e", 16),
      BigInt("d014f9a8c9ee2589e13f0cc8b6630ca6", 16)
    )

    top.io.key #= 0
    top.cd.waitSampling(1)
    top.io.key #= key
    top.cd.waitSampling(1)
    top.io.key #= 0
    top.cd.waitSampling(top.latency)
    for (i <- expected.indices) {
      assert(top.io.roundkeys(i).toBigInt == expected(i))
      top.cd.waitSampling(top.latency)
    }

    SpinalProgress(s"Tests passed")
  }

  def keySchedule192Sim(dut: Component): Unit = {
    val top = dut.asInstanceOf[AESFullyPipelinedKeySchedule]
    top.cd.forkStimulus(10)

    // Test vector taken from Appendix A of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
    val key = BigInt("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b", 16)

    // These outputs correspond to all round keys
    val expected = Array(
      BigInt("62f8ead2522c6b7bfe0c91f72402f5a5", 16),
      BigInt("ec12068e6c827f6b0e7a95b95c56fec2", 16),
      BigInt("4db7b4bd69b5411885a74796e92538fd", 16),
      BigInt("e75fad44bb095386485af05721efb14f", 16),
      BigInt("a448f6d94d6dce24aa326360113b30e6", 16),
      BigInt("a25e7ed583b1cf9a27f939436a94f767", 16),
      BigInt("c0a69407d19da4e1ec1786eb6fa64971", 16),
      BigInt("485f703222cb8755e26d135233f0b7b3", 16),
      BigInt("40beeb282f18a2596747d26b458c553e", 16),
      BigInt("a7e1466c9411f1df821f750aad07d753", 16),
      BigInt("ca4005388fcc5006282d166abc3ce7b5", 16),
      BigInt("e98ba06f448c773c8ecc720401002202", 16)
    )

    top.io.key #= 0
    top.cd.waitSampling(1)
    top.io.key #= key
    top.cd.waitSampling(1)
    top.io.key #= 0
    top.cd.waitSampling(top.latency)
    for (i <- expected.indices) {
      assert(top.io.roundkeys(i).toBigInt == expected(i))
      top.cd.waitSampling(top.latency)
    }

    SpinalProgress(s"Tests passed")
  }

  def keySchedule256Sim(dut: Component): Unit = {
    val top = dut.asInstanceOf[AESFullyPipelinedKeySchedule]
    top.cd.forkStimulus(10)

    // Test vector taken from Appendix A of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
    val key = BigInt("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4", 16)

    // These outputs correspond to all round keys
    val expected = Array(
      BigInt("1f352c073b6108d72d9810a30914dff4", 16),
      BigInt("9ba354118e6925afa51a8b5f2067fcde", 16),
      BigInt("a8b09c1a93d194cdbe49846eb75d5b9a", 16),
      BigInt("d59aecb85bf3c917fee94248de8ebe96", 16),
      BigInt("b5a9328a2678a647983122292f6c79b3", 16),
      BigInt("812c81addadf48ba24360af2fab8b464", 16),
      BigInt("98c5bfc9bebd198e268c3ba709e04214", 16),
      BigInt("68007bacb2df331696e939e46c518d80", 16),
      BigInt("c814e20476a9fb8a5025c02d59c58239", 16),
      BigInt("de1369676ccc5a71fa2563959674ee15", 16),
      BigInt("5886ca5d2e2f31d77e0af1fa27cf73c3", 16),
      BigInt("749c47ab18501ddae2757e4f7401905a", 16),
      BigInt("cafaaae3e4d59b349adf6acebd10190d", 16),
      BigInt("fe4890d1e6188d0b046df344706c631e", 16)
    )

    top.io.key #= 0
    top.cd.waitSampling(1)
    top.io.key #= key
    top.cd.waitSampling(1)
    top.io.key #= 0
    top.cd.waitSampling(top.latency)
    for (i <- expected.indices) {
      assert(top.io.roundkeys(i).toBigInt == expected(i))
      top.cd.waitSampling(top.latency)
    }

    SpinalProgress(s"Tests passed")
  }
}
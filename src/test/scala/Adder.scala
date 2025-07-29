import spinal.core._
import spinal.core.internals._
import spinal.core.sim._

import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

/**
 * Generic Test class for all Adders
 */
class AdderTest extends AnyFunSuite {

	//Sizes of adders to be tested
	val testSizes = Seq(4, 5, 8, 13, 32)

	var n: Int = 0

	for (n <- testSizes) {
		//n-bit adder with carry-out (change adder if needed)
		test (s"Sklansky: Testing ${n}-bit Adder with Carry-Out") {
			testNBits(n)(new SklanskyAdder(width = n, pipelined = true, carryOut = true))
		}

		//n-bit adder without carry-out (change adder if needed)
		test (s"Sklansky: Testing ${n}-bit Adder without Carry-Out") {
			testNBits(n)(new SklanskyAdder(width = n, pipelined = true, carryOut = false))
		}

		//n-bit adder with carry-out (change adder if needed)
		test (s"Kogge-Stone: Testing ${n}-bit Adder with Carry-Out") {
			testNBits(n)(new KoggeStoneAdder(width = n, pipelined = true, carryOut = true))
		}

		//n-bit adder without carry-out (change adder if needed)
		test (s"Kogge-Stone: Testing ${n}-bit Adder without Carry-Out") {
			testNBits(n)(new KoggeStoneAdder(width = n, pipelined = true, carryOut = false))
		}
	}

	def adderSim[T <: Adder](dut : Component): Unit = {
		val top = dut.asInstanceOf[T]

		top.cd.forkStimulus(10)

		//Number of tests, can be changed
		for (i <- 0 until 100) {
			val x, y = BigInt(n, Random)
			val result = if (top.carryOut) x+y else ((x+y) % (BigInt(1) << n))
			top.io.x #= x
			top.io.y #= y
			top.cd.waitSampling(top.latency + 1)
			assert(top.io.s.toBigInt == result)
		}

		SpinalProgress(s"Tests passed")
	}

	def testNBits[T <: Adder](n : Int = 32)(gen: => T): Unit = {
		this.n = n
		HADES("HADES")(gen)(adderSim).apply()
	}
}

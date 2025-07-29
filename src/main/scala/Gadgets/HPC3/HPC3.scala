package Gadgets.HPC3

import spinal.core._
import spinal.core.internals._

case class HPC3And() extends BlackBox with Gadget {

	addGeneric("d", gadgetConfig.securityOrder)
	addGeneric("pipeline", 1)

	val io = new Bundle {
		val clk 	= in  Bool()
		val reset = in  Bool()
		val x 		= in  Bits(gadgetConfig.numShares bits)
		val y 		= in  Bits(gadgetConfig.numShares bits)
		val r 		= in  Bits(gadgetConfig.randomnessAnd bits)
		val z 		= out Bits(gadgetConfig.numShares bits)
	}

	noIoPrefix()
	addRTLPath("src/main/scala/Gadgets/HPC3/HPC3And.vhd")
}

case class HPC3Xor() extends BlackBox with Gadget {

	addGeneric("d", gadgetConfig.securityOrder)
	addGeneric("pipeline", 1)

	val io = new Bundle {
		val clk 	= in  Bool()
		val reset = in  Bool()
		val x 		= in  Bits(gadgetConfig.numShares bits)
		val y 		= in  Bits(gadgetConfig.numShares bits)
		val z 		= out Bits(gadgetConfig.numShares bits)
	}

	noIoPrefix()
	addRTLPath("src/main/scala/Gadgets/HPC3/HPC3Xor.vhd")
}

case class HPC3Not() extends BlackBox with Gadget {

	addGeneric("d", gadgetConfig.securityOrder)
	addGeneric("pipeline", 1)

	val io = new Bundle {
		val clk 	= in  Bool()
		val reset = in  Bool()
		val x 		= in  Bits(gadgetConfig.numShares bits)
		val z 		= out Bits(gadgetConfig.numShares bits)
	}

	noIoPrefix()
	addRTLPath("src/main/scala/Gadgets/HPC3/HPC3Not.vhd")
}
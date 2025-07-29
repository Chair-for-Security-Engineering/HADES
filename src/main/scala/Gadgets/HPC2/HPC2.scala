package Gadgets.HPC2

import spinal.core._
import spinal.core.internals._

case class HPC2And() extends BlackBox with Gadget {

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
	addRTLPath("src/main/scala/Gadgets/HPC2/HPC2And.vhd")
}

case class HPC2Xor() extends BlackBox with Gadget {

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
	addRTLPath("src/main/scala/Gadgets/HPC2/HPC2Xor.vhd")
}

case class HPC2Not() extends BlackBox with Gadget {

	addGeneric("d", gadgetConfig.securityOrder)
	addGeneric("pipeline", 1)

	val io = new Bundle {
		val clk 	= in  Bool()
		val reset = in  Bool()
		val x 	= in  Bits(gadgetConfig.numShares bits)
		val z 	= out Bits(gadgetConfig.numShares bits)
	}

	noIoPrefix()
	addRTLPath("src/main/scala/Gadgets/HPC2/HPC2Not.vhd")
}
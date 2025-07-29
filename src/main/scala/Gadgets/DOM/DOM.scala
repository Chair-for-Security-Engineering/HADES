package Gadgets.DOM

import spinal.core._
import spinal.core.internals._

/**
 * DOMAnd Gadget as a BlackBox
 */
case class DOMAnd() extends BlackBox with Gadget {

	addGeneric("d", gadgetConfig.securityOrder)

	val io = new Bundle {
		val clk 	= in  Bool()
		val reset = in  Bool()
		val r 		= in Bits(gadgetConfig.randomnessAnd bits)
		val x = in  Bits(gadgetConfig.numShares bits)
		val y = in  Bits(gadgetConfig.numShares bits)
		val z = out Bits(gadgetConfig.numShares bits)
	}

	noIoPrefix()
	addRTLPath("src/main/scala/Gadgets/DOM/DOMAnd.vhd")
}

/**
 * DOMXor gadget as a BlackBox
 */
case class DOMXor() extends BlackBox with Gadget {

	addGeneric("d", gadgetConfig.securityOrder)

	val io = new Bundle {
		val clk 	= in  Bool()
		val reset = in  Bool()
		val r 		= null
		val x = in  Bits(gadgetConfig.numShares bits)
		val y = in  Bits(gadgetConfig.numShares bits)
		val z = out Bits(gadgetConfig.numShares bits)
	}

	noIoPrefix()
	addRTLPath("src/main/scala/Gadgets/DOM/DOMXor.vhd")
}

/**
 * DOMNot gadget as a BlackBox
 */
case class DOMNot() extends BlackBox with Gadget {

	addGeneric("d", gadgetConfig.securityOrder)

	val io = new Bundle {
		val clk 	= in  Bool()
		val reset = in  Bool()
		val r 		= null
		val x = in  Bits(gadgetConfig.numShares bits)
		val z = out Bits(gadgetConfig.numShares bits)
	}

	noIoPrefix()
	addRTLPath("src/main/scala/Gadgets/DOM/DOMNot.vhd")
}
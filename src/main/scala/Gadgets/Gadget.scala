package Gadgets

import spinal.core.internals._
import DOM._
import HPC2._
import HPC3._

object AllGadgets {
	val gadgets = List (
		//DOMGadget,
		HPC2Gadget,
		HPC3Gadget
	)
}

object HPC2Gadget extends MaskedGadget {

	override def getName(): String = "HPC2"

	override def updateConfig(): Unit = {
		val gadgetConfig = GlobalConfig.config.gadgetConfig

		gadgetConfig.numShares = gadgetConfig.securityOrder + 1

		gadgetConfig.latencyAnd = 2
		gadgetConfig.latencyXor = 0
		gadgetConfig.latencyNot = 0

		gadgetConfig.randomnessAnd = (gadgetConfig.securityOrder * (gadgetConfig.securityOrder + 1)) / 2
		gadgetConfig.randomnessXor = 0
		gadgetConfig.randomnessNot = 0

		gadgetConfig.gadget = this
	}

	override def And(): HPC2And = new HPC2And
	override def Xor(): HPC2Xor = new HPC2Xor
	override def Not(): HPC2Not = new HPC2Not
}



object HPC3Gadget extends MaskedGadget {

	override def getName(): String = "HPC3"

	override def updateConfig(): Unit = {
		val gadgetConfig = GlobalConfig.config.gadgetConfig

		gadgetConfig.numShares = gadgetConfig.securityOrder + 1

		gadgetConfig.latencyAnd = 1
		gadgetConfig.latencyXor = 0
		gadgetConfig.latencyNot = 0

		gadgetConfig.randomnessAnd = gadgetConfig.securityOrder * (gadgetConfig.securityOrder + 1)
		gadgetConfig.randomnessXor = 0
		gadgetConfig.randomnessNot = 0

		gadgetConfig.gadget = this
	}

	override def And(): HPC3And = new HPC3And
	override def Xor(): HPC3Xor = new HPC3Xor
	override def Not(): HPC3Not = new HPC3Not
}

object DOMGadget extends MaskedGadget {

	override def getName(): String = "DOM"

	override def updateConfig(): Unit = {
		val gadgetConfig = GlobalConfig.config.gadgetConfig

		gadgetConfig.numShares = gadgetConfig.securityOrder + 1

		gadgetConfig.latencyAnd = 1
		gadgetConfig.latencyXor = 0
		gadgetConfig.latencyNot = 0

		gadgetConfig.randomnessAnd = (gadgetConfig.securityOrder * (gadgetConfig.securityOrder + 1)) / 2
		gadgetConfig.randomnessXor = 0
		gadgetConfig.randomnessNot = 0

		gadgetConfig.gadget = this
	}

	override def And(): DOMAnd = new DOMAnd
	override def Xor(): DOMXor = new DOMXor
	override def Not(): DOMNot = new DOMNot
}
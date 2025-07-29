package spinal.core.internals

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Configuration
 * @param spinalConfig 	Spinal configuration
 * @param simConfig 		Config for simulation
 * @param gadgetConfig 	Config for the gadgets
 */
case class Config (var spinalConfig : SpinalConfig = SpinalConfigDefault.verilog(),
									 var simConfig : SpinalSimConfig = SpinalConfigDefault.sim(),
									 var gadgetConfig: GadgetConfig = GadgetConfig(),
									 var toplevelName : String = null
									){
	var exploreGadgets : Boolean = false
	var areaConfig = mutable.Map[String, mutable.Map[String, ArrayBuffer[BigDecimal]]]()
	var nandSize = BigDecimal(0)
	var resetKind : ResetKind = SYNC
	var topX : Int = 10
	var optimizationTargets : mutable.LinkedHashMap[OptimizationTarget, OptimizationTarget] = mutable.LinkedHashMap()
	var localOptimizationDepth : Int = 1
	var outFile : java.io.FileWriter = null
}

/**
 * SpinalConfigs for translation to VHDL and Verilog
 */
object SpinalConfigDefault {
	def vhdl(fileName : String = null): SpinalConfig = SpinalConfig (
		mode = VHDL,
		targetDirectory = "src/main/Hardware/VHDL",
		netlistFileName = if (fileName == null) null else s"${fileName}.vhd",
		bitVectorWidthMax = 2147483647
	)

	def verilog(fileName : String = null): SpinalConfig = SpinalConfig (
		mode = Verilog,
		targetDirectory = "src/main/Hardware/Verilog",
		netlistFileName = if (fileName == null) null else s"${fileName}.v",
		bitVectorWidthMax = 2147483647
	)

	def sim(workspaceName : String = null) = SpinalSimConfig(_workspaceName = workspaceName).withFstWave
}

/**
 * GadgetConfig describes masking order, latency of the 3 gadget types, randomness requirements, and used masked gadgets
 */
case class GadgetConfig (var securityOrder : Int = 0,
												 var latencyAnd : Int = 0,
												 var latencyXor : Int = 0,
												 var latencyNot : Int = 0,
												 var gadget: MaskedGadget = null
												) {
	var numShares : Int = 1
	var randomnessAnd : Int = 0
	var randomnessXor : Int = 0
	var randomnessNot : Int = 0
}

object GadgetConfig {
	def default: GadgetConfig = GadgetConfig()
}


/**
 * Optimization goals during DSE
 */
trait OptimizationTarget {
	var position : Int = 0
}

object Area extends OptimizationTarget {
	override def toString: String = "Area"
}

object ATP extends OptimizationTarget {
	override def toString: String = "Area_Time_Product"
}

object ATRP extends OptimizationTarget {
	override def toString: String = "Area_Time_Randomness_Product"
}

object Latency extends OptimizationTarget {
	override def toString: String = "Latency"
}

object Randomness extends OptimizationTarget {
	override def toString: String = "Randomness"
}

object Reload extends OptimizationTarget {
	override def toString: String = "Reload"
}

object Anything extends OptimizationTarget {
	override def toString: String = "Anything"
}
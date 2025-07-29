package spinal.core.internals

import scala.collection._
import scala.collection.mutable._

/**
 * Holds config data that is accessible for all classes
 */
object GlobalConfig {
	var config : Config = null

	//Return the current config
	def getConfig() = GlobalConfig.config

	//Update the current config
	def setConfig(cf : Config): Unit = {
		config = cf
	}

	var dryRun : Boolean = false
	def getDryRun(): Boolean = dryRun
	def setDryRun(dry : Boolean): Unit = {
		dryRun = dry
	}

	var areaConfig: mutable.Map[String, mutable.Map[String, ArrayBuffer[Double]]] = null
	def setAreaConfig(cf : mutable.Map[String, mutable.Map[String, ArrayBuffer[Double]]]): Unit = {
		areaConfig = cf
	}
	def getAreaConfig(): mutable.Map[String, mutable.Map[String, ArrayBuffer[Double]]] = areaConfig
}

/**
 * Trait that a class has to extend to be able to access the global config
 */
trait GlobalConfigUser {
	//Some useful type definitions
	//Configuration to configure Templates
	type Configuration = mutable.Map[String, immutable.Map[String, Any]]
	//GlobalConfig LocalConfig Gadget
	type C = (Configuration, MaskedGadget, Int)

	val globalConfig = GlobalConfig.getConfig()
	val globalDryRun = GlobalConfig.getDryRun()
}
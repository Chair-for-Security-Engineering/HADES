import spinal.core._
import spinal.core.internals._
import spinal.core.native.parentScope
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class Environment[T <: Template](config : Config)(gen: => T) {

	//Set the GlobalConfig to the current config
	GlobalConfig.setConfig(config)

	//Configs
	val spinalConfig = config.spinalConfig
	val simConfig = config.simConfig
	var gadgetConfig = config.gadgetConfig

	//PhaseContext that is needed for Spinal
	val phaseContext = new PhaseContext(spinalConfig)

	//Toplevel template (initially null), is set in the initialize() function and connected to phaseContext.toplevel
	var top : Template = null

	//Report for spinal
	val report = new SpinalReport[Component]()
	report.globalData = phaseContext.globalData

	//Design space exploration
	val explorer = new DesignSpaceExploration

	/**
	 * Initialization: Spinal initialization, loading of Config(s), gate counting, ...
	 */
	def initialize(): Unit = {
		//Initialization of the PhaseContext and creation of the toplevel module
		phaseContext.globalData.phaseContext = phaseContext
		if (spinalConfig.mode == VHDL) {
			phaseContext.globalData.anonymSignalPrefix = if (spinalConfig.anonymSignalPrefix == null) "zz" else spinalConfig.anonymSignalPrefix
		} else if (spinalConfig.mode == Verilog) {
			phaseContext.globalData.anonymSignalPrefix = if (spinalConfig.anonymSignalPrefix == null) "_zz" else spinalConfig.anonymSignalPrefix
		}

		val ctx = ClockDomainStack.set(ClockDomain.external(name = "", frequency = spinalConfig.defaultClockDomainFrequency))
		native //Avoid unconstructable during phase
		binarySequential
		binaryOneHot
		top = gen
		ctx.restore()

		phaseContext.checkGlobalData()

		//Update gadgetConfig
		if (gadgetConfig.gadget != null) gadgetConfig.gadget.updateConfig()

		//Naming of the toplevel given a configuration as string
		if (config.toplevelName != null && config.toplevelName.length < 255) top.definitionName = s"${top.getClass.getName}_${config.toplevelName}"
	}

	def explore(): Unit = {
		if (GlobalConfig.config.exploreGadgets) explorer.exploreGadgets(top)(gen)
		else {
			explorer.exploreAllDesigns(top)
		}
	}

	def instantiate(): Boolean = {
		//Set dryRun to false as we now want to instantiate all Registers... for the generation of VHDL and masking
		GlobalConfig.setDryRun(false)

		top.latency = 0
		top.randomness = 0
		top.andCount = 0
		top.xorCount = 0
		top.notCount = 0
		top.regCount = 0
		//Instantiate all internal logic of the Template for the first time
		val ret = top.instantiateTemplate()

		//Initialize all the phases
		val phases = ArrayBuffer[Phase]()

		phases ++= spinalConfig.transformationPhases
		phases ++= spinalConfig.memBlackBoxers
		if (spinalConfig.mode == VHDL && spinalConfig.onlyStdLogicVectorAtTopLevelIo) {
			phases += new PhaseStdLogicVectorAtTopLevelIo()
		}

		phases += new PhaseDeviceSpecifics(phaseContext)
		phases += new PhaseApplyIoDefault(phaseContext)

		phases += new PhaseNameNodesByReflection(phaseContext)
		phases += new PhaseCollectAndNameEnum(phaseContext)

		phases += new PhaseCheckIoBundle()
		phases += new PhaseCheckHierarchy()
		phases += new PhaseAnalog()
		phases += new PhaseNextifyReg()
		phases += new PhaseRemoveUselessStuff(false, false)
		phases += new PhaseRemoveIntermediateUnnameds(true)

		phases += new PhasePullClockDomains(phaseContext)

		if (spinalConfig.mode == VHDL) {
			phases += new PhaseInferEnumEncodings(phaseContext, e => e)
		} else if (spinalConfig.mode == Verilog) {
			phases += new PhaseInferEnumEncodings(phaseContext, e => if(e == `native`) binarySequential else e)
		}
		phases += new PhaseInferWidth(phaseContext)
		phases += new PhaseNormalizeNodeInputs(phaseContext)
		phases += new PhaseRemoveIntermediateUnnameds(false)
		phases += new PhaseSimplifyNodes(phaseContext)

		phases += new PhaseCompletSwitchCases()
		phases += new PhaseRemoveUselessStuff(true, true)
		phases += new PhaseRemoveIntermediateUnnameds(false)

		phases += new PhaseCheck_noLatchNoOverride(phaseContext)
		phases += new PhaseCheck_noRegisterAsLatch()
		phases += new PhaseCheckCombinationalLoops()
		phases += new PhaseCheckCrossClock()

		//Execute the phases if the design could be instantiated, otherwise continue
		if (ret) {
			for (phase <- phases) {
				phaseContext.doPhase(phase)
			}
		}

		ret
	}

	/**
	 * Execution phase: Gadget instantiation, composability notion, randomness optimization, optimization for area,
	 * latency, randomness, ...
	 */
	def execute(): Unit = {

		if (gadgetConfig.securityOrder > 0 && gadgetConfig.gadget != null) {
			top.duplicateAllStatements()

			//Remove now unused statements
			top.removeUnusedStatements()

			//Randomness
			top.inferRandomness()
			top.addRandomnessPort()
			top.connectRandomnessPorts()
		}
	}

	/**
	 * Finalization: Final phases, translation to VHDL/Verilog, report, ...
	 */
	def finish(): Unit = {
		SpinalProgress(s"Generating Hardware")

		val phases = ArrayBuffer[Phase]()

		phases += new PhaseDeviceSpecifics(phaseContext)
		phases += new PhaseApplyIoDefault(phaseContext)

		phases += new PhaseNameNodesByReflection(phaseContext)
		phases += new PhaseCollectAndNameEnum(phaseContext)

		phases += new PhaseCheckIoBundle()
		phases += new PhaseCheckHierarchy()
		phases += new PhaseAnalog()
		phases += new PhaseNextifyReg()

		phases += new PhaseRemoveIntermediateUnnameds(true)

		phases += new PhasePullClockDomains(phaseContext)

		if (spinalConfig.mode == VHDL) {
			phases += new PhaseInferEnumEncodings(phaseContext, e => e)
		} else if (spinalConfig.mode == Verilog) {
			phases += new PhaseInferEnumEncodings(phaseContext, e => if(e == `native`) binarySequential else e)
		}
		phases += new PhaseInferWidthNew(phaseContext)
		phases += new PhaseNormalizeNodeInputs(phaseContext)
		phases += new PhaseRemoveIntermediateUnnameds(false)
		phases += new PhaseSimplifyNodes(phaseContext)

		phases += new PhaseCompletSwitchCases()
		phases += new PhaseRemoveIntermediateUnnameds(false)

		phases += new PhasePropagateNames(phaseContext)
		phases += new PhaseRemoveLock(phaseContext)
		phases += new PhaseAllocateNames(phaseContext)
		phases += new PhaseDevice(phaseContext)

		if (spinalConfig.mode == VHDL) {
			phases += new PhaseVhdl(phaseContext, report)
		} else if (spinalConfig.mode == Verilog) {
			phases += new PhaseVerilog(phaseContext, report)
		}

		for (phase <- phases) {
			phaseContext.doPhase(phase)
		}

		phaseContext.checkGlobalData()

		report.toplevel = phaseContext.topLevel.asInstanceOf[T]

		//Add BB-Paths to report
		addBBPath(top)
	}

	/**
	 * Add a BlackBox path to the report
	 * @param c Current Component
	 */
	def addBBPath (c : Component): Unit = {
		c match {
			//If c is a BlackBox, add its RTLPath to the report
			case bb: BlackBox => {
				report.blackboxesSourcesPaths ++= bb.listRTLPath
			}
			//Otherwise, search its children for a BlackBox
			case _ => c.children.foreach(child => addBBPath(child))
		}
	}

	/**
	 * Simple simulation for the environment
	 * @param body Simulation body (signals, tests, ...)
	 */
	def simulate(fixedSeed : Boolean, body: Component => Unit): Unit = {
		val simCompiled = simConfig.compile(report)
		simCompiled.doSim(seed = if (fixedSeed) 42 else simCompiled.newSeed())(body)
	}

}

object Environment {
	def apply[T <: Template](config : Config)(gen: => T): Environment[T] = {
		val environment = new Environment(config = config)(gen)
		environment.initialize()
		val instantiated = environment.instantiate()
		if (instantiated) {
			environment.execute()
			environment.finish()
		}
		environment
	}
}
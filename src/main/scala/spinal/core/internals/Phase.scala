package spinal.core.internals

import spinal.core._

import scala.collection.mutable

/**
 * Remove the lock in the global scope
 * @param pc
 */
class PhaseRemoveLock(pc : PhaseContext) extends PhaseMisc {
	override def impl(pc: PhaseContext): Unit = {
		import pc._

		globalScope.lock = false
	}
}

/**
 * Phase that can be inserted in the backend to get an access to the internal data model at any desired point
 * @param pc
 */
class PhaseTest(pc : PhaseContext) extends PhaseMisc {
	override def impl(pc: PhaseContext): Unit = {
		import pc._

		//walkExpression(e => println(e))
		/*walkComponents(c => {
			c.dslBody.walkStatements(s => s.walkDrivingExpressions {
				case bt: BaseType => {
					println(s)
					println(bt, bt.component)
					println(bt.getComponents().head)
				}
				case _ =>
			})
		})*/
		/*walkDeclarations (d => d match {
			case e: Widthable => {
				println(d, e, e.inferredWidth, e.calcWidth, e.inferWidth)
				if(e.getWidth == 0 && e.isNamed) globalData.zeroWidths += (e.component -> e)
				//widthableCheck(e)
			}
			case _ =>
		})*/
	}
}

class PhaseInferWidthNew(pc: PhaseContext) extends PhaseMisc{

	override def impl(pc: PhaseContext): Unit = {
		import pc._
		globalData.nodeAreInferringWidth = true

		var iterationCounter = 0

		while (true) {
			var somethingChange = false

			//Infer width on all expressions
			//Use post-order traversal so that a parent node can get the widths of its children before inferring width,
			//which could help reducing the number of iterations
			walkExpressionPostorder {
				case e: DeclarationStatement =>
				case e: Widthable =>
					val hasChange = e.inferWidth
					somethingChange = somethingChange || hasChange
				case _ =>
			}

			//Infer width on all nameable expression (BitVector)
			walkDeclarations {
				case e: Widthable =>
					val hasChange = e.inferWidth
					somethingChange = somethingChange || hasChange
				case _ =>
			}

			//Check in the width inferation is done, then check it and generate errors
			if (!somethingChange || iterationCounter == 10000) {
				val errors = mutable.ArrayBuffer[String]()

				def widthableCheck(e: Widthable): Unit = {
					if (e.inferWidth) {
						//Don't care about Reg width inference
						errors += s"Can't infer width on ${e.getScalaLocationLong}"
					}

					if (e.widthWhenNotInferred != -1 &&
					 e.widthWhenNotInferred != e.getWidth) {
						errors += s"getWidth call result during elaboration differ from inferred width on\n${e.getScalaLocationLong}"
					}

					if(e.inferredWidth < 0){
						errors += s"Negative width on $e at ${e.getScalaLocationLong}"
					}

					if (e.inferredWidth > pc.config.bitVectorWidthMax) {
						errors += s"Way too big signal $e at ${e.getScalaLocationLong}"
					}
				}

				walkExpression {
					case e: DeclarationStatement =>
					case e: Widthable => widthableCheck(e)
					case e: WidthProvider =>
						if (e.getWidth < 0) {
							errors += s"Negative width on $e at ${e.getScalaLocationLong}"
						}
						if (e.getWidth > pc.config.bitVectorWidthMax) {
							errors += s"Way too big signal $e at ${e.getScalaLocationLong}"
						}
					case _ =>
				}
				walkDeclarations {
					case e: Widthable => {
						if(e.getWidth == 0 && e.isNamed) globalData.zeroWidths += (e.component -> e)
						widthableCheck(e)
					}
					case _ =>
				}

				if (errors.nonEmpty)
					SpinalError(errors)
				return
			}
			iterationCounter += 1
		}
	}
}
package spinal.core.internals

abstract class TemplateFactory {
	type T <: Template
	def createTemplate(): T
	def getName(): String
}
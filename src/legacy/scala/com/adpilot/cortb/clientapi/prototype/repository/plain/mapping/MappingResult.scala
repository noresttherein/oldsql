package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.MappingResult.AnyMappingResult

import scala.slick.jdbc.PositionedResult


trait MappingResult[M<:Mapping[E], E] extends AnyMappingResult[M, E] {
	def :\ [T](component :M#Component[T]) :MappingResult[component.type, T] //= pick(component)

	def apply[T](component :M#Component[T]) :T = (this :\ component).get

}



object MappingResult {
	def apply[M<:Mapping[E], E](mapping :M, res :PositionedResult) :MappingResult[mapping.type, E] =
		new MappingResultImpl[mapping.type, E](mapping, ColumnValues(mapping.selectable, ColumnValues.view(res)))
	
	def apply[M<:Mapping[E], E](mapping :M, values :ColumnValues) :MappingResult[mapping.type, E] =
		new MappingResultImpl[mapping.type, E](mapping, values)


	def apply[M<:Mapping[E], E](mapping :M)(implicit res :MappingResult[mapping.type, E]) :MappingResult[mapping.type, E] = res


//	implicit def valueOf[M<:Mapping[E], E](res :MappingResult[M, E]) = res.get
	
	
	
	trait AnyMappingResult[M<:AnyMapping, E] {

		def result :Option[E]

		def get :E = result.get

		def get[C<:M#Component[_]](component :C) :component.ResultType

		def pick[C<:M#Component[_]](component :C) :ComponentResult[C]


		def map [C<:M#Component[_]](component :M=>C) :ComponentResult[C] = this \ component

		def \ [C<:M#Component[_]](component :M=>C) :ComponentResult[C]

//		def :\ [T](component :M#Component[T]) :MappingResult[component.type, T] = pick(component)
	}

	object AnyMappingResult {
		implicit def valueOf[M<:Mapping[E], E](res :AnyMappingResult[M, E]) :E = res.get
	}

	
	
	trait ComponentResult[M<:AnyMapping] extends AnyMappingResult[M, M#ResultType] {
		def :\ (component :M#Component[_]) :ComponentResult[component.type]

		def apply(component :M#Component[_]) :component.ResultType = (this :\ component).get

//		override def get[C <: M#Component[_]](component: C): component.ResultType = (this :\ component).get
	}



	private abstract class BaseResultImpl[M<:AnyMapping, E] protected[MappingResult] (mapping :M, protected[MappingResult] val vals :ColumnValues)
		extends AnyMappingResult[M, E]
	{

		protected[MappingResult] def value :E

		def result :Option[E] =
			if (mapping.selectable.isEmpty) None
			else Some(value)


		def pick[C<:M#Component[_]](component: C): ComponentResult[C] =
			new ComponentResultImpl[C](component, valuesFor(component))

		override def get[C <: M#Component[_]](component: C): component.ResultType =
			component.assemble(mapping.valuesFor(component.asInstanceOf[mapping.Component[_]])(vals)).asInstanceOf[component.ResultType]

		override def \ [C<:M#Component[_]](component :M=>C) :ComponentResult[C] = {
			val comp = component(mapping)
			new ComponentResultImpl(comp, valuesFor(comp))
		}

		protected def valuesFor(c :M#Component[_]) :ColumnValues =
			mapping.valuesFor(c.asInstanceOf[mapping.Component[_]])(vals)

	}

	private class MappingResultImpl[M<:Mapping[E], E] protected[MappingResult] (mapping :M, res :ColumnValues)
		extends BaseResultImpl[M, E](mapping, res) with MappingResult[M, E]
	{
		protected[MappingResult] def value :E = mapping(vals)

		override def :\ [T](component: M#Component[T]): MappingResult[component.type, T] =
			new MappingResultImpl[component.type, T](component, valuesFor(component))
	}
	

	private class ComponentResultImpl[M<:AnyMapping] protected[MappingResult] (mapping :M, res :ColumnValues)
		extends BaseResultImpl[M, M#ResultType](mapping, res) with ComponentResult[M]
	{
		protected[MappingResult] def value :M#ResultType = mapping.assemble(vals)

		def :\ (component :M#Component[_]) :ComponentResult[component.type] =
			new ComponentResultImpl[component.type](component, valuesFor(component))

	}

}

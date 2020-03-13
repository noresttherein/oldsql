package com.hcore.ogre.mapping

import com.hcore.ogre.mapping.ComponentValues._
import com.hcore.ogre.morsels.InverseIndexSeq
import com.hcore.ogre.slang.options.extensions

import scala.slick.jdbc.PositionedResult

//implicits
import com.hcore.ogre.morsels.InverseIndexSeq.implicitIndexing
import extensions._


trait ColumnValues[M<:AnyMapping] extends ComponentValues[M] {
	@inline
	final def upcolumns[X>:M <:AnyMapping] :ColumnValues[X] = this.asInstanceOf[ColumnValues[X]]

	@inline
	final def downcolumns[X<:M] :ColumnValues[X] = this.asInstanceOf[ColumnValues[X]]

	@inline
	final protected[ColumnValues] def crosscolumns[X<:AnyMapping] :ColumnValues[X] = this.asInstanceOf[ColumnValues[X]]

}




object ColumnValues {

	type TypedValues[M<:Mapping[T], T] = ColumnValues[M]



	@inline
	def apply[M<:AnyMapping](mapping :M, res :PositionedResult) :ColumnValues[M] =
		generic[M](mapping)(res)



	@inline
	def Empty[M<:AnyMapping] :ColumnValues[M] = empty.crosscolumns[M]

	@inline
	def Empty[M<:AnyMapping](source : =>String) :ColumnValues[M] = new EmptyValues[M](source) with ColumnValues[M]

	@inline
	def Predefined[M<:AnyMapping](mapping :M, value :M#ResultType) :ColumnValues[M] =
		new SelectedDeassembledComponentValues[M](mapping, value, mapping.selectable) with ColumnValues[M]
	
	@inline
	def Predefined[M<:AnyMapping](mapping :M, value :M#ResultType, columns :Seq[M#Component[_]]) :ColumnValues[M] =
		new SelectedDeassembledComponentValues[M](mapping, value, columns.indexed) with ColumnValues[M]

	
//	@inline
//	def Lazy[M<:Mapping](mapping :M, value : =>M#ResultType) :ColumnValues[M] =
//		new LazyMappingValue[M](mapping, value)



	@inline
	def apply(mapping :AnyMapping) :ColumnValuesFactory[mapping.type] =
		new ColumnValuesFactory[mapping.type](mapping)

	@inline
	def generic[M<:AnyMapping](mapping :M) :ColumnValuesFactory[M] =
		new ColumnValuesFactory[M](mapping)




	final class ColumnValuesFactory[M<:AnyMapping](val mapping :M) extends AnyVal {
		@inline
		def apply(value :M#ResultType) :ColumnValues[M] = Predefined[M](mapping, value)

		@inline
		def apply(value :M#ResultType, components :Seq[M#Component[_]]) :ColumnValues[M] =
			new SelectedDeassembledComponentValues[M](mapping, value, components) with ColumnValues[M]


		def apply(values :M#Component[_]=>Option[_]) :ColumnValues[M] =
			new CustomComponentValues[M](mapping, values) with ColumnValues[M]

		@inline
		def apply(res :PositionedResult) :ColumnValues[M] =
			apply(res, mapping.selectable)

		@inline
		def apply(res :PositionedResult, offsets :M#Component[_]=>Option[Int]) :ColumnValues[M] =
			new PositionedResultComponentValues[M](mapping, res, offsets) with ColumnValues[M]

		@inline
		def apply(res :PositionedResult, columns :Seq[M#Component[_]]) :ColumnValues[M] = {
			val selectable = InverseIndexSeq(columns)
			val offsets = (c :M#Component[_]) => selectable.indexOf(c).providing(_>=0)
			new PositionedResultComponentValues[M](mapping, res, offsets) with ColumnValues[M]
		}

//		@inline
//		def Lazy(value : =>M#ResultType) :ComponentValues[M] = ComponentValues.Lazy[M](mapping, value)

		@inline
		def Empty :ColumnValues[M] = ColumnValues.Empty[M]

		@inline
		def Empty(source : =>String) :ColumnValues[M] = ColumnValues.Empty[M](source)
	}


	private val empty = new EmptyValues[AnyMapping] with ColumnValues[AnyMapping]
}

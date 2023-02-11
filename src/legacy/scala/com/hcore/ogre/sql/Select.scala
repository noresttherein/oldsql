package com.hcore.ogre.sql

import com.hcore.ogre.mapping.Mapping.MappingReadForm
import com.hcore.ogre.mapping.support.MappingAdapter.MappingImpostor
import com.hcore.ogre.mapping.{Mapping, AnyMapping}
import com.hcore.ogre.morsels.InverseIndexSeq
import com.hcore.ogre.sql.RowSource.{SelectSource, SubsourceOf, FirstTableOf}
import com.hcore.ogre.sql.SQLFormula.{SelectFormula, ComponentFormula}



//implicits
import InverseIndexSeq.implicitIndexing


trait Select[H] extends SQLStatement[RowCursor[H]] {

}





object Select {
	def apply[S<:SelectSource, T](select :SelectFrom[S, T]) :Select[T] = apply(select.asSelect)

	def apply[S<:SelectSource, T](select :SelectFormula[S, T]) :Select[T] = ???

	def *[S<:SelectSource](from :S) :Select[S#Row] = ???

	def first[S<:SelectSource, T<:AnyMapping](from :S)(implicit firstTable :T FirstTableOf S) :Select[T#ResultType] = ???

	def last[S<:SelectSource](from :S) :Select[S#LastMapping#ResultType] = ???




}
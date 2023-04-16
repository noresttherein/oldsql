package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.schema.{Mapping, Relation, Table}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.{AndBy, AndByParam, AndFrom, AndFromParam, By, ByParam, From, FromSome, GroupBy, GroupByClause, InnerJoin, Join, LeftJoin, NonParam, RightJoin, RowProduct, Subselect, WithParam}
import net.noresttherein.oldsql.sql.ParamClause.{ParamRelation, UnboundParam}
import net.noresttherein.oldsql.sql.RowProduct.As
import net.noresttherein.oldsql.sql.mechanics.GetTable.{ByAlias, ByIndex, ByParamAlias, ByParamIndex, ByParamType, BySubject, ByType, RelationEvidence}






class GetTableCheck {{

	class A[O] extends FormMapping[String, O]
	class B[O] extends FormMapping[String, O]
	class C[O] extends FormMapping[String, O]
	class D[O] extends FormMapping[String, O]
	class E[O] extends FormMapping[String, O]
	class M[O] extends FormMapping[Char, O]
	class N[O] extends FormMapping[Char, O]
	class X[O] extends FormMapping[Byte, O]
	class Y[O] extends FormMapping[Byte, O]
	class Z[O] extends FormMapping[Byte, O]

	val A :Relation[A] = Table("A", new A[Unit])
	val B :Relation[B] = Table("B", new B[Unit])
	val C :Relation[C] = Table("C", new C[Unit])
	val D :Relation[D] = Table("D", new D[Unit])
	val E :Relation[E] = Table("E", new E[Unit])

	type Test = (
		From[A] As "A" LeftJoin B As "B" WithParam Int As "Int" RightJoin C As "C" WithParam Long As "Long"
			Subselect D As "D" InnerJoin E As "E" WithParam Short As "Short" GroupBy D As "D" By E As "E"
			Subselect M As "M" InnerJoin N As "N"
			Subselect D As "D" RightJoin X As "X" LeftJoin Y As "Y" GroupBy Y As "Y" By X As "X" ByParam Byte As "Byte"
			Subselect Z As "Z"
	)
	type Generalized = (
		RowProduct NonParam A Join B WithParam Int Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	)

	class Expect[E <: RelationEvidence[Test, Generalized, K], K, M <: Mapping]

	implicit def Expect[E <: RelationEvidence[Test, Generalized, K], A, X[O] <: MappingAt[O], K, Y <: Mapping]
	                   (implicit get :E { type O = A; type M[O] = X[O] }, same :X[A] =:= Y) :Expect[E, K, Y] =
		new Expect

	def expect[E[F <: RowProduct, G <: RowProduct, X] <: RelationEvidence[F, G, X], K, M <: Mapping]
	          (implicit ev :Expect[E[Test, Generalized, K], K, M]) = ev

	def summon[T](implicit ev :T) :ev.type = ev


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByPositiveIndex ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByIndex.ByPositiveIndex.Return, 0, A[
		RowProduct AndFrom A Join B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 1, B[
		RowProduct AndFrom B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 2, UnboundParam[Int,
		RowProduct AndFromParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 3, C[
		RowProduct AndFrom C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 4, UnboundParam[Long,
		RowProduct AndFromParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 5, D[
		RowProduct AndBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 6, E[
		RowProduct AndBy E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 7, M[
		RowProduct AndFrom M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 8, N[
		RowProduct AndFrom N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 9, Y[
		RowProduct AndBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 10, X[
		RowProduct AndBy X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 11, UnboundParam[Byte,
		RowProduct AndByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 12, Z[RowProduct AndFrom Z]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByNegativeIndex ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByIndex.ByNegativeIndex.Return, -13, A[
		RowProduct AndFrom A Join B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -12, B[
		RowProduct AndFrom B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -11, UnboundParam[Int,
		RowProduct AndFromParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -10, C[
		RowProduct AndFrom C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -9, UnboundParam[Long,
		RowProduct AndFromParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -8, D[
		RowProduct AndBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -7, E[
		RowProduct AndBy E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -6, M[
		RowProduct AndFrom M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -5, N[
		RowProduct AndFrom N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -4, Y[
		RowProduct AndBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -3, X[
		RowProduct AndBy X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -2, UnboundParam[Byte,
		RowProduct AndByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -1, Z[RowProduct AndFrom Z]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByAlias ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByAlias.Return, "A", A[
		RowProduct AndFrom A Join B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "B", B[
		RowProduct AndFrom B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "Int", UnboundParam[Int,
		RowProduct AndFromParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "C", C[
		RowProduct AndFrom C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "Long", UnboundParam[Long,
		RowProduct AndFromParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "D", D[
		RowProduct AndBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "E", E[
		RowProduct AndBy E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "M", M[
		RowProduct AndFrom M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "N", N[
		RowProduct AndFrom N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "Y", Y[
		RowProduct AndBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "X", X[
		RowProduct AndBy X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "Byte", UnboundParam[Byte,
		RowProduct AndByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "Z", Z[RowProduct AndFrom Z]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByType ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByType.Return, A[Unit], A[
		RowProduct AndFrom A Join B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, B[Unit], B[
		RowProduct AndFrom B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, UnboundParam[Int, Unit], UnboundParam[Int,
		RowProduct AndFromParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, C[Unit], C[
		RowProduct AndFrom C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, UnboundParam[Long, Unit], UnboundParam[Long,
		RowProduct AndFromParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, D[Unit], D[
		RowProduct AndBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, E[Unit], E[
		RowProduct AndBy E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, M[Unit], M[
		RowProduct AndFrom M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, N[Unit], N[
		RowProduct AndFrom N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, Y[Unit], Y[
		RowProduct AndBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, X[Unit], X[
		RowProduct AndBy X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, UnboundParam[Byte, Unit], UnboundParam[Byte,
		RowProduct AndByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, Z[Unit], Z[RowProduct AndFrom Z]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ BySubject ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[BySubject.Return, Int, UnboundParam[Int,
		RowProduct AndFromParam Int Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[BySubject.Return, Long, UnboundParam[Long,
		RowProduct AndFromParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[BySubject.Return, String, E[
		RowProduct AndBy E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[BySubject.Return, Char, N[
		RowProduct AndFrom N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[BySubject.Return, Byte, Z[RowProduct AndFrom Z]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByPositiveParamIndex ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByParamIndex.ByPositiveParamIndex.Return, 0, UnboundParam[Int,
		RowProduct AndFromParam Int Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamIndex.ByPositiveParamIndex.Return, 1, UnboundParam[Long,
		RowProduct AndFromParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamIndex.ByPositiveParamIndex.Return, 3, UnboundParam[Byte,
		RowProduct AndBy ParamRelation[Byte]#Param
			Subselect Z
	]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByParamType ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByParamType.Return, Int, UnboundParam[Int,
		RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamType.Return, Long, UnboundParam[Long,
		RowProduct AndFromParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamType.Return, Byte, UnboundParam[Byte,
		RowProduct AndByParam Byte
			Subselect Z
	]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByParamAlias ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByParamAlias.Return, "Int", UnboundParam[Int,
		RowProduct AndFromParam Int Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamAlias.Return, "Long", UnboundParam[Long,
		RowProduct AndFromParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamAlias.Return, "Byte", UnboundParam[Byte,
		RowProduct AndByParam Byte
			Subselect Z
	]]

}}

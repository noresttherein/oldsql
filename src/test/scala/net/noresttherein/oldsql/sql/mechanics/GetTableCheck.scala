package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.schema.{Mapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.{AndFrom, By, From, FromSome, GroupBy, GroupByClause, InnerJoin, Join, LeftJoin, RightJoin, RowProduct, Subselect}
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, ParamRelation}
import net.noresttherein.oldsql.sql.mechanics.GetTable.{ByAlias, ByIndex, ByParamAlias, ByParamIndex, ByParamType, BySubject, ByType, RelationEvidence}
import net.noresttherein.oldsql.sql.GroupBy.AndBy
import net.noresttherein.oldsql.sql.GroupParam.ByParam
import net.noresttherein.oldsql.sql.JoinParam.WithParam
import net.noresttherein.oldsql.sql.RowProduct.As






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

	val A :Relation[A] = Relation("A", new A[()])
	val B :Relation[B] = Relation("B", new B[()])
	val C :Relation[C] = Relation("C", new C[()])
	val D :Relation[D] = Relation("D", new D[()])
	val E :Relation[E] = Relation("E", new E[()])

	type F = (
		From[A] As "A" LeftJoin B As "B" WithParam Int As "Int" RightJoin C As "C" WithParam Long As "Long"
			Subselect D As "D" InnerJoin E As "E" WithParam Short As "Short" GroupBy D As "D" By E As "E"
			Subselect M As "M" InnerJoin N As "N"
			Subselect D As "D" RightJoin X As "X" LeftJoin Y As "Y" GroupBy Y As "Y" By X As "X" ByParam Byte As "Byte"
			Subselect Z As "Z"
	)
	type U = (
		RowProduct AndFrom A Join B WithParam Int Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	)

	class Expect[E <: RelationEvidence[F, U, K], K, M <: Mapping]

	implicit def Expect[E <: RelationEvidence[F, U, K], A, M[O] <: MappingAt[O], K, R <: Mapping]
	                   (implicit get :E { type O = A; type T[X] = M[X] }, same :M[A] =:= R) :Expect[E, K, R] =
		new Expect

	def expect[E[F <: RowProduct, G <: RowProduct, X] <: RelationEvidence[F, G, X], K, M <: Mapping]
	          (implicit ev :Expect[E[F, U, K], K, M]) = ev

	
	
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
	expect[ByIndex.ByPositiveIndex.Return, 2, FromParam[Int,
		RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long
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
	expect[ByIndex.ByPositiveIndex.Return, 4, FromParam[Long,
		RowProduct AndFrom ParamRelation[Long]#Param
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 5, D[
		FromSome GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 6, E[
		GroupByClause AndBy E
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
		FromSome GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 10, X[
		GroupByClause AndBy X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByPositiveIndex.Return, 11, FromParam[Byte,
		GroupByClause AndBy ParamRelation[Byte]#Param
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
	expect[ByIndex.ByNegativeIndex.Return, -11, FromParam[Int,
		RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long
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
	expect[ByIndex.ByNegativeIndex.Return, -9, FromParam[Long,
		RowProduct AndFrom ParamRelation[Long]#Param
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -8, D[
		FromSome GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -7, E[
		GroupByClause AndBy E
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
		FromSome GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -3, X[
		GroupByClause AndBy X ByParam Byte
		Subselect Z
	]]
	expect[ByIndex.ByNegativeIndex.Return, -2, FromParam[Byte,
		GroupByClause AndBy ParamRelation[Byte]#Param
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
	expect[ByAlias.Return, "Int", FromParam[Int,
		RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long
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
	expect[ByAlias.Return, "Long", FromParam[Long,
		RowProduct AndFrom ParamRelation[Long]#Param
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "D", D[
		FromSome GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "E", E[
		GroupByClause AndBy E
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
		FromSome GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "X", X[
		GroupByClause AndBy X ByParam Byte
		Subselect Z
	]]
	expect[ByAlias.Return, "Byte", FromParam[Byte,
		GroupByClause AndBy ParamRelation[Byte]#Param
		Subselect Z
	]]
	expect[ByAlias.Return, "Z", Z[RowProduct AndFrom Z]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByType ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByType.Return, A[()], A[
		RowProduct AndFrom A Join B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, B[()], B[
		RowProduct AndFrom B WithParam Int Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, FromParam[Int, ()], FromParam[Int,
		RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, C[()], C[
		RowProduct AndFrom C WithParam Long
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, FromParam[Long, ()], FromParam[Long,
		RowProduct AndFrom ParamRelation[Long]#Param
		Subselect D Join E WithParam Short GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, D[()], D[
		FromSome GroupBy D By E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, E[()], E[
		GroupByClause AndBy E
		Subselect M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, M[()], M[
		RowProduct AndFrom M Join N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, N[()], N[
		RowProduct AndFrom N
		Subselect D Join X Join Y GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, Y[()], Y[
		FromSome GroupBy Y By X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, X[()], X[
		GroupByClause AndBy X ByParam Byte
		Subselect Z
	]]
	expect[ByType.Return, FromParam[Byte, ()], FromParam[Byte,
		GroupByClause AndBy ParamRelation[Byte]#Param
		Subselect Z
	]]
	expect[ByType.Return, Z[()], Z[RowProduct AndFrom Z]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ BySubject ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[BySubject.Return, Int, FromParam[Int,
		RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[BySubject.Return, Long, FromParam[Long,
		RowProduct AndFrom ParamRelation[Long]#Param
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[BySubject.Return, String, E[
		GroupByClause AndBy E
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

	expect[ByParamIndex.ByPositiveParamIndex.Return, 0, FromParam[Int,
		RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamIndex.ByPositiveParamIndex.Return, 1, FromParam[Long,
		RowProduct AndFrom ParamRelation[Long]#Param
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamIndex.ByPositiveParamIndex.Return, 3, FromParam[Byte,
		GroupByClause AndBy ParamRelation[Byte]#Param
			Subselect Z
	]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByParamType ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByParamType.Return, Int, FromParam[Int,
		RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamType.Return, Long, FromParam[Long,
		RowProduct AndFrom ParamRelation[Long]#Param
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamType.Return, Byte, FromParam[Byte,
		GroupByClause AndBy ParamRelation[Byte]#Param
			Subselect Z
	]]



	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ByParamAlias ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

	expect[ByParamAlias.Return, "Int", FromParam[Int,
		RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamAlias.Return, "Long", FromParam[Long,
		RowProduct AndFrom ParamRelation[Long]#Param
			Subselect D Join E WithParam Short GroupBy D By E
			Subselect M Join N
			Subselect D Join X Join Y GroupBy Y By X ByParam Byte
			Subselect Z
	]]
	expect[ByParamAlias.Return, "Byte", FromParam[Byte,
		GroupByClause AndBy ParamRelation[Byte]#Param
			Subselect Z
	]]


	//todo: ByLabel, ByParamName

//	expect[ByParamName.Return, "Int", FromParam[Int, RowProduct AndFrom ParamRelation[Int]#Param Join C WithParam Long Subselect D Join E]]
//	expect[ByParamName.Return, "Long", FromParam[Long, RowProduct AndFrom ParamRelation[Long]#Param Subselect D Join E]]

	/*
			val f = From(A) as "A" leftJoin B as "B" rightJoin C as "C" subselect D as "D" join E as "E"

			//	val a = f[A]; val b = f[B]; val c = f[C]; val d = f[D]; val e = f[E]
			//	val a = f(0); val b = f(1); val c = f(2); val d = f(3); val e = f(4)
			val a = f(-5); val b = f(-4); val c = f(-3); val d = f(-2); val e = f(-1)
			//	val a = f("A"); val b = f("B"); val c = f("C"); val d = f("D"); val e = f("E")
			//	val e = f.of[Int]
			a :A[RowProduct AndFrom A Join B Join C Subselect D Join E]
			b :B[RowProduct AndFrom B Join C Subselect D Join E]
			c :C[RowProduct AndFrom C Subselect D Join E]
			d :D[RowProduct AndFrom D Join E]
			e :E[RowProduct AndFrom E]

			//todo: make a method which creates a JoinParam _ As _ in one go.
			val params = From(A) as "A" param[Int] "p1" as "P1" join B as "B" param[Long] "p2" as "P2" join C as "C" param[String] "p3" as "P3"

			//	val params2 = From(A) ? [Int] "p1"

			//	def ?:[X] = ???
			//	def ?:[String :ValueOf, Int] = ???
			import net.noresttherein.oldsql.sql.UnboundParam.?:
			?:[Int]
			"param".?:[Int]
			?:["p1", Int]

			val params3 = From(A) param ?:[Int]
			//	From(A) param ?:["P2", Int]
			val p1 = params.?[Int]; val p2 = params.?[Long]; val p3 = params.?[String]
			//	val p1 = params ? 0; val p2 = params ? 1; val p3 = params ? 2
			//	val p1 = params ? -3; val p2 = params ? -2; val p3 = params ? -1

			p1 :FromParam[Int, RowProduct AndFrom FromParam.Of[Int]#P Join B JoinParam FromParam.Of[Long]#P Join C JoinParam FromParam.Of[String]#P]
			p2 :FromParam[Long, RowProduct AndFrom FromParam.Of[Long]#P Join C JoinParam FromParam.Of[String]#P]
			p3 :FromParam[String, RowProduct AndFrom FromParam.Of[String]#P]
		*/

}}
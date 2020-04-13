package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.abacus.{INT, Plus}
import net.noresttherein.oldsql.schema.{Mapping, RowSource, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, TypedMapping, MappingAlias, MappingOf}
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth
import net.noresttherein.oldsql.sql.FromClause.GetTableByPredicate.{ByMapping, ByOrigin, BySubject, ByTypeConstructor}
import net.noresttherein.oldsql.sql.MappingFormula.FromFormula
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula, Formula}
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple
import net.noresttherein.oldsql.sql.WithParam.{?:, ParamMapping, ParamSource}

import scala.annotation.implicitNotFound


trait FromClause {

	/** Type of the last mapping in this join (the rightmost one) if not empty. */
	type LastMapping <: Mapping

	/** Table alias proxy for the last table (or table-like expression) in this list as seen from some `FromClause`
	  * type `F` containing this instance in its 'tail'. In other words, this projects the type of the last element
	  * of this clause to an extending row source.
	  */
	type LastTable[-F <: FromClause] <: FromFormula[F, LastMapping]

	/** Last mapping in this source when treated as a list, if any. */
	def lastTable :LastTable[this.type] //:JoinedTable[this.type, _<:Mapping]



	/** Result types of all mappings in this source concatenated into a heterogeneous list.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `Join` (but unlike `::`), left associative.
	  */
	type Row <: Chain

	/** Create an sql tuple formula containing `TableFormula`s for all joined tables in their order of appearance.
	  * It will contain entries for all mappings in this source, including parameter mappings and mappings listed in this
	  * source's `Outer` prefix (if this source is a subselect source).
	  */
	def row :ChainTuple[this.type, Row]



	/** Type of the outer source if this source represents a subselect source created by `Outer.from()`.
	  * All 'real' `Join` subclasses have this type equal to the `Outer` of their left side, but `SubselectJoin`
	  * defines `Outer` as the type of its left side. Additionally, all 'real' joins implement `SubselectFrom[L#Outer]`
	  * and `SubselectJoin` implements `SubselectFrom[L]`. This means that for any concrete class `S &lt;: FromClause`
	  * with fully instantiated parameters (i.e. all tables in `S` and types of joins in it are known)
	  * value `(s :S) from t1 join t2 ... join t3` conforms to `SubselectFrom[S]`. This way we can statically express
	  * a dependency relationship between sources without resorting to implicit evidence.
	  */
	type Outer <: FromClause

	/** Return the outer source of this instance if it (or, recursively, any source in the left side of the join)
	  * was created by calling `outer.from()`. When viewed as a list of tables, outer constitutes the result of dropping
	  * all tables joined in this instance up until and including a table joined by a `.from()` call. If there's no
	  * `SubselectJoin` in the dynamic type definition of this source, meaning this is not a subselect source
	  * (''from'' clause of resulting selects will include all members of this source), `Dual` instance used
	  * as the table list terminator is returned.
	  * @return outer of the left side or just the left side if this instance is a `SubselectJoin`.
	  */
	def outer :Outer


	type Generalized <: FromClause

	def generalized :Generalized

	/** Number of mappings contained in this join (counting all their occurrences separately). */
	def size :Int


	def canEqual(that :Any) :Boolean = that.isInstanceOf[FromClause]

}






object FromClause {

	/** A `FromClause` representing the ''from'' clause of a subselect of a select with a ''from'' clause`S`.
	  * `S &lt;: SubselectFrom[F]` if and only if `S =:= F SubselectJoin M1 Join M2 ... Join MN` for some mappings
	  * `M1...MN` and both types are full clauses.  Sources conforming to `SubselectFrom[S]` can use
	  * all the mappings/tables which are a part of `S`, but they are not a part of any select formulas created
	  * from that source. This allows the use of nested select queries which depend on values from the ''from'' clause
	  * of the outer select. Rather counterintuitively, this type is contravariant rather than covariant.
	  * There are two reasons behind it: one, preventing any clause from being a subselect clause of a clause
	  * with an abstract prefix, ensuring that full mapping lists are compared, and two, treating all join
	  * kinds as equivalent for this purpose. Note that subselects may be nested to an arbitrary depth
	  * and only directly nested subselects of `S` conform to this type.
	  */
	type SubselectFrom[-F <: FromClause] = FromClause {
		type Outer >: F <: FromClause
	}


	/** A closed `FromClause`, which doesn't contain any parameters and is not a subclause of another clause.
	  * `S &lt;: SelectFrom` if and only if it is a concrete source (without any abstract types or `FromClause`
	  * occurrences in its definition) and doesn't contain any `WithParam` or `SubselectJoin`.
	  * The name stems from the fact that only such sources can be used to create independent select statements.
	  */
	type SelectFrom = FromClause {
		type Outer = FromClause
	}


	/** A `FromClause` without any tables specified, representing a single statement parameter `X`.
	  * This type encompasses all from clauses ending with a synthetic parameter mapping, not simply the empty ones.
	  */
	type FromParam[X, O] = FromClause WithParam (O ?: X)

//	trait FullFrom extends FromClause






	implicit class FromClauseMethods[F <: FromClause](private val self :F) extends AnyVal {
		def row(implicit row :RowOf[F, F#Row]) :SQLFormula[F, F#Row] = row(self)


		def join[T[O] <: MappingFrom[O]](table :RowSource[T])(implicit alias :UniqueAlias) :F InnerJoin T[alias.T] =
			(self :FromClause) match {
				case Dual => From(table).asInstanceOf[F InnerJoin T[alias.T]]
				case _ => InnerJoin(self, table)
			}

		//todo: maybe we should make From[T] extends all three join kinds?
		@inline
		def leftJoin[T[O] <: MappingFrom[O]](table :RowSource[T])(implicit alias :UniqueAlias) :F LeftJoin T[alias.T] =
			LeftJoin(self, table)

		@inline
		def leftJoin[T[O] <: MappingFrom[O], A]
		            (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :F LeftJoin T[A] =
			LeftJoin(self, RowSource(table))


		@inline
		def rightJoin[T[O] <: MappingFrom[O]](table :RowSource[T])(implicit alias :UniqueAlias) :F RightJoin T[alias.T] =
			RightJoin(self, table)

		@inline
		def rightJoin[T[O] <: MappingFrom[O], A]
		             (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :F RightJoin T[A] =
			RightJoin(self, RowSource(table))


		@inline
		def outerJoin[T[O] <: MappingFrom[O]](table :RowSource[T])(implicit alias :UniqueAlias) :F LeftJoin T[alias.T] =
			LeftJoin(self, table)

		@inline
		def outerJoin[T[O] <: MappingFrom[O], A]
		             (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :F LeftJoin T[A] =
			LeftJoin(self, RowSource(table))


		@inline
		def from[T[O] <: MappingFrom[O]](table :RowSource[T])(implicit alias :UniqueAlias) :F SubselectJoin T[alias.T] =
			SubselectJoin(self, table)

		@inline
		def from[T[O] <: MappingFrom[O], A]
		        (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :F SubselectJoin T[A] =
			SubselectJoin(self, RowSource(table))


		@inline
		def withParam[X :SQLForm](implicit alias :UniqueAlias) :F WithParam (alias.T ?: X) =
			WithParam(self, ParamSource[X]())

		@inline
		def withParam[X :SQLForm](name :String)(implicit alias :UniqueAlias) :F WithParam (alias.T ?: X) =
			WithParam(self, ParamSource[X](name))

		@inline
		def withParam[X, N](param :N ?: X) :F WithParam (N ?: X) =
			WithParam(self, param)






//		def where(condition :JoinedTables[F] => BooleanFormula[F]) :F
	}






	class JoinedTables[F <: FromClause](private val self :F) extends AnyVal {
		def table[M <: Mapping](implicit get :GetTableByPredicate[F, ByMapping, M]) :FromFormula[F, M] =
			get(self).asInstanceOf[FromFormula[F, M]]

		def of[E](implicit get :GetTableByPredicate[F, BySubject, E]) :FromFormula[F, get.T] = get(self)

		def apply[M[O] <: MappingFrom[O]](implicit get :GetTableByPredicate[F, ByTypeConstructor, M[Any]]) :FromFormula[F, get.T] =
			get(self)

		def apply[N <: String with Singleton](alias :N)(implicit get :GetTableByPredicate[F, ByOrigin, N]) :FromFormula[F, get.T] =
			get(self)

		def apply[N <: INT](n :N)(implicit get :GetTableByIndex[F, N]) :FromFormula[F, get.T] = get(self)

//		def param[N <: String with Singleton](name :N)(implicit get :GetTableByPredicate[F, ByOrigin, N])
	}



	@implicitNotFound("Cannot count the number of tables in ${F}. Either the FROM clause is not fully instantiated" +
		              "or ${N} is not the right number.")
	sealed class TableCount[-F <: FromClause, N <: INT] private ()

	object TableCount {
		implicit val DualCount = new TableCount[Dual, 0]

		implicit def moreTables[L <: FromClause, R <: Mapping, N <: INT, M <: INT]
		                       (implicit count :TableCount[L, N], plus :Plus[N, M]) :TableCount[L Join R, M] =
			count.asInstanceOf[TableCount[L Join R, M]]
	}



	@implicitNotFound("Cannot get table ${N} of ${F}.\n Either ${N} >= size, ${N} < -size " +
	                  "(where size is the number of tables in the FROM clause), or ${N} >= 0 and the size is not known " +
	                  "(the clause type starts with FromClause and not Dual/From.")
	sealed abstract class GetTableByIndex[-F <: FromClause, N <: INT] {
		type T <: Mapping
		def apply(from :F) :FromFormula[F, T]
	}

	object GetTableByIndex {

		implicit def last[M <: Mapping] :GetTableByIndex[FromClause Join M, -1] {type T = M} =
			new GetTableByIndex[FromClause Join M, -1] {
				type T = M
				override def apply(from :FromClause Join M) :FromFormula[FromClause Join M, M] = from.lastTable
			}

		implicit def previous[L <: FromClause, R <: Mapping, N <: INT, M <: INT]
		                     (implicit get :GetTableByIndex[L, M], minus :Plus[N, M])
				:GetTableByIndex[L Join R, N] { type T = get.T } =
			new GetTableByIndex[L Join R, N] {
				type T = get.T
				override def apply(from :L Join R) :FromFormula[L Join R, T] = get(from.left).asPartOf
			}

		implicit def last[F <: FromClause, M <: Mapping, N <: INT](implicit count :TableCount[F Join M, N])
				:GetTableByIndex[F Join M, N] { type T = M } =
			new GetTableByIndex[F Join M, N] {
				override type T = M
				override def apply(from :F Join M) = from.lastTable
			}

		implicit def previous[L <: FromClause, R <: Mapping, N <: INT](implicit get :GetTableByIndex[L, N])
				:GetTableByIndex[L Join R, N] { type T = get.T } =
			new GetTableByIndex[L Join R, N] {
				type T = get.T
				override def apply(from :L Join R) = get(from.left).asPartOf
			}

	}



	@implicitNotFound("Cannot find a mapping M satisfying $P[M, ${X}] in the clause ${F}.")
	sealed abstract class GetTableByPredicate[-F <: FromClause, P[_ <: Mapping, _], X] {
		type T <: Mapping
		def apply(from :F) :FromFormula[F, T]
	}

	object GetTableByPredicate {
		implicit def last[M <: Mapping, P[_ <: Mapping, _], X](implicit pred :P[M, X])
				:GetTableByPredicate[FromClause Join M, P, X] { type T = M } =
			new GetTableByPredicate[FromClause Join M, P, X] {
				type T = M
				override def apply(from :FromClause Join M) = from.lastTable
			}

		implicit def previous[L <: FromClause, R <: Mapping, P[_ <: Mapping, _], X]
		                     (implicit get :GetTableByPredicate[L, P, X])
				:GetTableByPredicate[L Join R, P, X] { type T = get.T } =
			new GetTableByPredicate[L Join R, P, X] {
				override type T = get.T
				override def apply(from :L Join R) = get(from.left).asPartOf
			}



		@implicitNotFound("Cannot find mapping ${M} with Origin type ${O}")
		final class ByOrigin[M <: Mapping, O] private[GetTableByPredicate]
		private[this] final val origin = new ByOrigin[Mapping, Any]

		implicit def ByOrigin[M <: MappingFrom[O], O] :ByOrigin[M, O] = origin.asInstanceOf[ByOrigin[M, O]]



		@implicitNotFound("Cannot find mapping ${M} with Subject type ${S}")
		final class BySubject[M <: Mapping, S] private[GetTableByPredicate]
		private[this] final val subject = new BySubject[Mapping, Any]

		implicit def BySubject[M <: MappingOf[S], S] :BySubject[M, S] = subject.asInstanceOf[BySubject[M, S]]



		@implicitNotFound("Cannot find mapping ${M} of type ${T}")
		final class ByMapping[M <: Mapping, T] private[GetTableByPredicate]
		private[this] final val mapping = new ByMapping[Mapping, Mapping]

		implicit def ByMapping[M <: Mapping] :ByMapping[M, M] = mapping.asInstanceOf[ByMapping[M, M]]



		@implicitNotFound("Cannot find mapping ${M} with same type constructor as ${T}")
		final class ByTypeConstructor[M <: Mapping, T] private[GetTableByPredicate]
		private[this] final val constructor = new ByTypeConstructor[Mapping, Any]

		implicit def ByTypeConstructor[M[O] <: MappingFrom[O], A] :ByTypeConstructor[M[A], M[Any]] =
			constructor.asInstanceOf[ByTypeConstructor[M[A], M[Any]]]

	}



/*
	@implicitNotFound("Cannot find a mapping with Origin type ${O} in the clause ${F}.")
	sealed abstract class GetTableByOrigin[-F <: FromClause, O] {
		type T <: MappingFrom[O]
		def apply(from :F) :FromFormula[F, T]
	}

	object GetTableByOrigin {
		implicit def last[M <: MappingFrom[O], O] :GetTableByOrigin[FromClause Join M, O] { type T = M } =
			new GetTableByOrigin[FromClause Join M, O] {
				type T = M
				override def apply(from :FromClause Join M) = from.lastTable
			}

		implicit def previous[L <: FromClause, R <: Mapping, O](implicit get: GetTableByOrigin[L, O])
				:GetTableByOrigin[Join[L, R], O] { type T = get.T } =
			new GetTableByOrigin[L Join R, O] {
				override type T = get.T
				override def apply(from :L Join R) = get(from.left).asPartOf
			}
	}



	@implicitNotFound("Cannot find a mapping with Subject type ${S} in the clause ${F}.")
	sealed abstract class GetTableBySubject[-F <: FromClause, S] {
		type T <: MappingOf[S]
		def apply(from :F) :FromFormula[F, T]
	}

	object GetTableBySubject {
		implicit def last[M <: MappingOf[S], S] :GetTableBySubject[FromClause Join M, S] { type T = M } =
			new GetTableBySubject[FromClause Join M, S] {
				type T = M
				override def apply(from :FromClause Join M) = from.lastTable
			}

		implicit def previous[L <: FromClause, R <: Mapping, S](implicit get :GetTableBySubject[L, S])
				:GetTableBySubject[L Join R, S] { type T = get.T } =
			new GetTableBySubject[L Join R, S] {
				override type T = get.T
				override def apply(from :L Join R) = get(from.left).asPartOf
			}
	}
*/



	abstract class RowOf[-F <: FromClause, R <: Chain] extends (F => ChainTuple[F, R])

	object RowOf {
		implicit val EmptyRow :RowOf[Dual, @~] = { _ => ChainTuple.EmptyChain }

		implicit def joinRow[L <: FromClause, R <: Mapping, S <: Chain]
		                    (implicit left :RowOf[L, S]) :RowOf[L Join R, S ~ R#Subject] =
			new RowOf[L Join R, S ~ R#Subject] {
				override def apply(join :L Join R) =
					left(join.left).asPartOf(join) ~ (join.lastTable :MappingFormula[FromClause Join R, R])
			}
	}





	/** Proof that the ''from'' clause `S` is an extension of the clause `F` / the clause `F` is a prefix
	  * of the clause of `S`. It means that `S &lt;: F Join T1 ... Join TN forSome { type T1 ... TN }`.
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a full clause and has a generalized prefix.
	  * The implication here is that this witness can be relied upon only in the context of the actual
	  * extension and is not to be treated as a generalized subtyping hierarchy.
	  */
	class ExtendedBy[F <: FromClause, -S <: FromClause] private(private[sql] val length :Int) extends AnyVal {
		def apply[T <: Mapping](table :FromFormula[F, T]) :FromFormula[S, T] =
			table.asInstanceOf[FromFormula[S, T]]

		 def apply[E[-R <: FromClause , X] <: SQLFormula[R, X], T](expression :E[F, T]) :E[S, T] =
			expression.asInstanceOf[E[S, T]]

		def apply[T <: Chain](expression :ChainTuple[F, T]) :ChainTuple[S, T] =
			expression.asInstanceOf[ChainTuple[S, T]]
//		def apply[T](expression :SQLFormula[F, T]) :SQLFormula[S, T] =
//			expression.asInstanceOf[SQLFormula[S, T]]
//
//		def apply[T](expression :ColumnFormula[F, T]) :ColumnFormula[S, T] =
//			expression.asInstanceOf[ColumnFormula[S, T]]
	}

	object ExtendedBy {
		private[this] val instance = new ExtendedBy[FromClause, FromClause](0)

		implicit def itself[F <: FromClause] :ExtendedBy[F, F] = instance.asInstanceOf[F ExtendedBy F]

		implicit def join[S <: FromClause, L <: FromClause, R <: Mapping]
		                 (implicit ev :S ExtendedBy L) :ExtendedBy[S, L Join R] =
			new ExtendedBy(ev.length + 1)
	}



	sealed abstract class UniqueAlias { type T }

	implicit def UniqueOrigin :UniqueAlias = new UniqueAlias { type T = this.type }
}

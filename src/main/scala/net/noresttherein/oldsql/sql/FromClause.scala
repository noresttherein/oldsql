package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.schema.{Mapping, RowSource, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component}
import net.noresttherein.oldsql.sql.FromClause.FromFormula
import net.noresttherein.oldsql.sql.SQLFormula.{ColumnFormula, Formula}
import net.noresttherein.oldsql.sql.SQLTuple.ChainFormula


trait FromClause {

	/** Type of the last mapping in this join (the rightmost one) if not empty. */
	type LastMapping[O] <: AnyComponent[O]

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
	def row :ChainFormula[this.type, Row]



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
	  * (FROM clause of resulting selects will include all members of this source), `Dual` instance used
	  * as the table list terminator is returned.
	  * @return outer of the left side or just the left side if this instance is a `SubselectJoin`.
	  */
	def outer :Outer



	/** Number of mappings contained in this join (counting all their occurrences separately). */
	def size :Int


	def canEqual(that :Any) :Boolean = that.isInstanceOf[FromClause]
}






object FromClause {

	/** A `FromClause` representing a subselect source of `S`.
	  * `S &lt;: SubselectFrom[R]` if and only if `S =:= R SubselectJoin M1 Join M2 ... Join MN` for some mappings
	  * `M1...MN`. Sources conforming to `SubselectFrom[S]` can use all the mappings/tables which are a part of `S`,
	  * but they are not a part of any select formulas created from that source. This allows the use of nested
	  * select queries which depend on values from the 'FROM' clause of the outer select. Note that subselects
	  * may be nested to an arbitrary depth and only directly nested subselects of `S` conform to this type.
	  */
	type SubselectFrom[-S <: FromClause] = FromClause {
		type Outer >: S <: FromClause
	}


	/** A closed `FromClause`, which doesn't contain any parameters and is not a subsource of another source.
	  * `S &lt;: SelectFrom` if and only if it is a concrete source (without any abstract types or `FromClause`
	  * occurrences in its definition) and doesn't contain any `WithParam` or `SubselectJoin`.
	  * The name stems from the fact that only such sources can be used to create independent select statements.
	  */
	type SelectFrom = FromClause {
		type Outer = FromClause
	}




	case class FromFormula[-F <: FromClause, M[O] <: AnyComponent[O]] private[sql] (from :RowSource[M], index :Int)
		extends MappingFormula[F, M]
	{
		val mapping :M[Any] = from[Any] //todo:

		override def readForm :SQLReadForm[Subject] = from[Any].selectForm

		override def applyTo[Y[+X]](matcher :SQLFormula.FormulaMatcher[F, Y]) :Y[Subject] = ???

		override def isGroundedIn(tables :Iterable[FromFormula[_, m forSome { type m[O] <: AnyComponent[O] }]]) :Boolean =
			tables.exists(_ == this)

		override def isomorphic(expression :Formula[_]) :Boolean = ???

		override private[oldsql] def equivalent(expression :Formula[_]) = ???
	}



	object FromFormula {
		trait FromMatcher[+F <: FromClause, +Y[X]] {
			def from[M[O] <: AnyComponent[O]](f :FromFormula[F, M]) :Y[M[Any]#Subject]
		}

		type MatchFrom[+F <: FromClause, +Y[X]] = FromMatcher[F, Y]

		type CaseFrom[+F <: FromClause, +Y[X]] = FromMatcher[F, Y]
	}


	/** Proof that the FROM clause `S` is an extension of the clause `F` / the clause `F` is a prefix the clause of `S`.
	  * It means that `S &lt;: F Join T1 ... Join TN forSome { type T1 ... TN }`.
	  */
	class ExtendedBy[F <: FromClause, -S <: FromClause] private() {
//		def apply[T <: Component[O, E], O, E](table :TableFormula[F, T, O, E]) :TableFormula[S, T, O, E] =
//			table.asInstanceOf[TableFormula[S, T, O, E]]

		def apply[T](expression :SQLFormula[F, T]) :SQLFormula[S, T] =
			expression.asInstanceOf[SQLFormula[S, T]]

		def apply[T](expression :ColumnFormula[F, T]) :ColumnFormula[S, T] =
			expression.asInstanceOf[ColumnFormula[S, T]]
	}

	object ExtendedBy {
		private[this] val instance = new ExtendedBy[FromClause, FromClause]

		implicit def itself[F <: FromClause] :ExtendedBy[F, F] = instance.asInstanceOf[F ExtendedBy F]

		implicit def join[S <: FromClause, L <: FromClause, R[O] <: AnyComponent[O]](implicit ev :S ExtendedBy L) :ExtendedBy[S, L Join R] =
			instance.asInstanceOf[S ExtendedBy (L Join R)]
	}



}

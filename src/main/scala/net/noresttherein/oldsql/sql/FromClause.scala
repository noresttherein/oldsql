package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.Component
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula
import net.noresttherein.oldsql.sql.SQLTuple.ChainFormula


trait FromClause {

	/** Result types of all mappings in this source concatenated into a heterogeneous list.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `Join` (but unlike `::`), left associative.
	  */
	type Row <: Chain

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



	/** Create an sql tuple formula containing `TableFormula`s for all joined tables in their order of appearance.
	  * It will contain entries for all mappings in this source, including parameter mappings and mappings listed in this
	  * source's `Outer` prefix (if this source is a subselect source).
	  */
//	def row :ChainFormula[this.type, Row]


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




	trait FromFormula[-F <: FromClause, M <: Mapping] extends SQLFormula[F, M#Subject] {
		type Subject = M#Subject
	}



	/** Proof that source S is an extension of source R / source R is a prefix source of S.
	  * It means that S<: R Join T1 ... Join TN forSome T1 ... TN.
	  */
	class ExtendedBy[R<:FromClause, -S<:FromClause] private() {
//		def apply[T <: Component[O, E], O, E](table :TableFormula[R, T, O, E]) :TableFormula[S, T, O, E] =
//			table.asInstanceOf[TableFormula[S, T, O, E]]

		def apply[T](expression :SQLFormula[R, T]) :SQLFormula[S, T] =
			expression.asInstanceOf[SQLFormula[S, T]]

		def apply[T](expression :ColumnFormula[R, T]) :ColumnFormula[S, T] =
			expression.asInstanceOf[ColumnFormula[S, T]]
	}

	object ExtendedBy {
		implicit def itself[R<:FromClause] :ExtendedBy[R, R] = new ExtendedBy[R, R]
		implicit def join[S<:FromClause, L<:FromClause, R<:Mapping](implicit ev :S ExtendedBy L) :ExtendedBy[S, L Join R] =
			new ExtendedBy[S, L Join R]
	}



}

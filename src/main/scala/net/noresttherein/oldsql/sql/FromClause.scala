package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.abacus.{Inc, INT}
import net.noresttherein.oldsql.morsels.Origin.{##, Rank}
import net.noresttherein.oldsql.schema.{bits, GenericMapping, Mapping, RowSource, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.RowSource.NamedSource
import net.noresttherein.oldsql.schema.bits.{ChainMapping, LabeledMapping}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.FromClause.GetTableByIndex.GetTableByNegativeIndex
import net.noresttherein.oldsql.sql.FromClause.GetTableByPredicate.{ByLabel, BySubject, ByTypeConstructor}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, JoinedTables}
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.AnyRelationIn
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula, Formula}
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple
import net.noresttherein.oldsql.sql.WithParam.{FromParam, ParamSource}

import scala.annotation.implicitNotFound





/** In its basic use, a `FromClause` is a representation of the ''FROM'' and ''WHERE'' clauses in an SQL ''SELECT''
  * statement, declaring the relations taking part in the query. More generally, it is the domain over which SQL
  * expressions (instances of the [[net.noresttherein.oldsql.sql.SQLFormula SQLFormula]] class hierarchy) are defined,
  * providing all non-constant values available to them. Consists of a a list of `Mapping`s, together with an optional
  * filter working on those mappings, especially any required join conditions. While the individual elements of a clause
  * are referred to often as tables for simplicity, they can not only be arbitrary relations such as other ''selects'',
  * but also synthetic artifacts such as query parameters. It is even possible to use arbitrary mapping components,
  * to be replaced at a later time with references to concrete tables, parameters or constants. As such, it
  * doesn't necessarily represent a valid fragment of a select from the application's schema.
  * In that case it's best thought as a signature containing declarations of terms (mappings and parameters)
  * over which SQL formulas can be defined; hence `SQLFormula` is parameterized with a `FromClause`.
  *
  * The mappings taking part in the query are encoded in the static type of the standard implementation, which
  * builds both the instance and its type by recursively applying the [[net.noresttherein.oldsql.sql.With With]]
  * class to a shorter clause, extending it by another relation. In that sense it is quite similar to general
  * purpose heterogeneous lists such as the ''shapeless'' `HList`, replacing its wide functionality with specific
  * application to this task. Just as a `HList` - and all other two-argument type constructors - it can and should
  * be written in the infix notation: `Dual Join Rangers Join Familiars Join Monsters`. Specialized classes for
  * all join kinds exist. Note that the whole `Join` class hierarchy is left-associative for more natural left to right
  * reading and writing. As they are also almost universally covariant regarding its left clause type parameter,
  * any prefix can be always substituted with the abstract `FromClause` supertype, balancing static type checking
  * with a degree of freedom and flexibility promoting code reuse.
  *
  * This trait is a bare bones common upper type, serving at the same time as a wildcard in type definitions meaning
  * 'an arbitrary number of other relations not pertinent under the circumstances at hand'. Most functionality
  * is defined in the `With` subclass representing actual non-empty clauses. Additionally, an implicit conversion
  * exists from any `F &lt;: FromClause` to
  * [[net.noresttherein.oldsql.sql.FromClause.FromClauseMethods FromClauseMethods[F] ]] which declares methods
  * for joining it with other relations and others which benefit from having a static, invariant self-type.
  * Individual relations can be most easily accessed through the
  * [[net.noresttherein.oldsql.sql.FromClause.JoinedTables JoinedTables]]
  * and [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]] classes exposed by the
  * `tables` and `formulas` methods defined by the implicit conversion.
  *
  * @see [[net.noresttherein.oldsql.sql.With With]]
  * @see [[net.noresttherein.oldsql.sql.Dual Dual]]
  * @see [[net.noresttherein.oldsql.sql.From From]]
  * @see [[net.noresttherein.oldsql.sql.Join Join]]
  * @author Marcin Mościcki
  */
trait FromClause {

	/** Type of the last mapping in this join (the rightmost one) if not empty. */
	type LastMapping[O] <: MappingFrom[O]

	/** Table alias proxy for the last table (or table-like expression) in this list as seen from some `FromClause`
	  * type `F` containing this instance in its 'tail'. In other words, this projects the type of the last element
	  * of this clause to an extending row source.
	  */
	type LastTable[F <: FromClause] <: JoinedRelation[F, LastMapping]

	/** The supertype of this instance containing only the last relation mapping joined with `FromClause` or `Dual`. */
	type FromLast >: this.type <: FromClause

	/** Last relation in this clause when treated as a list, if any. */
	def lastTable :LastTable[FromLast] //:JoinedTable[this.type, _<:Mapping]



	/** Self type of this clause used by some copy constructors present in the subclasses. */
	type This >: this.type <: FromLast //Generalized



	/** The type of this clause with all join kinds replaced with the root `With`. It forms the class of abstraction
	  * of clauses with the same relations (and in the same order), which are considered equivalent for most purposes.
	  */
	type Generalized <: FromLast

	/** This clause upcast to the generalized form in which all join kinds are replaced with `With`. */
	def generalized :Generalized



	/** Type of the outer source if this source represents a subselect source created by `Outer.subselect()`.
	  * All 'proper' `With` subclasses have this type equal to the `Outer` of their left side, but `Subselect`
	  * defines `Outer` as the type of its left side. Additionally, all 'proper' joins conform to `SubselectFrom[L#Outer]`
	  * and `L Subselect R` conforms to `SubselectFrom[L]`. This means that for any concrete class
	  * `S &lt;: FromClause` with fully instantiated parameters (i.e. all relations in `S` and types of joins in it
	  * are known) value `(s :S) from t1 join t2 ... join t3` conforms to `SubselectFrom[S]`. This way we can statically
	  * express a dependency relationship between ''from'' clauses without resorting to implicit evidence.
	  */
	type Outer <: FromClause

	/** Return the outer clause of this instance if it (or, recursively, any clause in the left side of the join)
	  * was created by calling `outer.subselect()`. When viewed as a list of relations, `outer` constitutes the result
	  * of dropping all relations joined in this instance up until and including a relation joined
	  * by a `.subselect()` call. If there's no `Subselect` in the dynamic type definition of this source, meaning
	  * this is not a subselect clause (''from'' clause of resulting selects will include all members of this clause),
	  * `Dual` instance used as the relation list terminator is returned.
	  * @return `outer` of the left side or just the left side if this instance is a `Subselect`.
	  */
	def outer :Outer



	/** Number of mappings contained in this join (counting all their occurrences separately). */
	def size :Int



	/** Subject types of all mappings in this clause, concatenated into a heterogeneous list.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `With` (but unlike `::`), left associative.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  */
	type Row <: Chain

	/** Create an SQL tuple formula containing `JoinedRelation` formulas for all joined elements in their order
	  * of appearance. It will contain entries for all mappings in this clause, including parameter mappings
	  * and mappings listed in this clause's `Outer` prefix (if this clause is a subselect clause).
	  */
	def row :ChainTuple[Generalized, Row] = row(ExtendedBy.itself)

	def row[E <: FromClause](stretch :Generalized ExtendedBy E) :ChainTuple[E, Row]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  */
	def tableStack :LazyList[AnyRelationIn[Generalized]] = tableStack(ExtendedBy.itself)

	def tableStack[E <: FromClause](stretch :Generalized ExtendedBy E) :LazyList[AnyRelationIn[E]]


	/** Subject types of all mappings in this clause following the `Outer` prefix, concatenated into a heterogeneous list.
	  * This amounts to the list of values mapped by the relations from the ''from'' clause of the most deeply nested
	  * subselect. If this clause doesn't represent a subselect, but a top-level query, it is the same as `Row`.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `With` (but unlike `::`), left associative.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#subselectRow]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Row]]
	  */
	type SubselectRow <: Chain

	/** Create an SQL tuple formula containing `JoinedRelation` formulas for all joined elements of the most deeply
	  * nested subselect clause, in their order of appearance. This includes all relations in this clause following
	  * the most recent `subselect` 'join', marking the first relation following the `Outer` clause prefix.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  */
	def subselectRow :ChainTuple[Generalized, SubselectRow] = subselectRow(ExtendedBy.itself)

	def subselectRow[E <: FromClause](stretch :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance, ending
	  * with the first relation following the `Outer` prefix. If this is not a subselect clause (no `Subselect` 'joins'
	  * are present in this clause and `Outer =:= FromClause`), all relations are included.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  */
	def subselectTableStack :LazyList[AnyRelationIn[Generalized]] = subselectTableStack(ExtendedBy.itself)

	def subselectTableStack[E <: FromClause](stretch :Generalized ExtendedBy E) :LazyList[AnyRelationIn[E]]





	/** Function type returning a join condition between the last relation in this clause and a following mapping `T`.
	  * Used as the parameter for [[net.noresttherein.oldsql.sql.With#on With.on]], it is declared here
	  * (on the 'left side' of a new join) to ensure that `on` won't be possible to call if the left side of
	  * a `With` clause is abstract or empty, i.e. that the clause contains at least two relations and their
	  * types are known.
	  */
	type JoinFilter[T[O] <: MappingFrom[O]] <:
		(JoinedRelation[FromLast With T, LastMapping], JoinedRelation[FromClause With T, T])
			=> BooleanFormula[FromLast With T]



/*
	def join[T[O] <: MappingFrom[O]](table :RowSource[T]) :This InnerJoin T =
		this match {
			case Dual() => From(table).asInstanceOf[This InnerJoin T]
			case _ => InnerJoin(this, table)
		}

	@inline def join[T[O] <: MappingFrom[O], A]
	                (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :This InnerJoin T =
		join(RowSource(table))


	@inline def leftJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :This LeftJoin T = LeftJoin(this, table)

	@inline def leftJoin[T[O] <: MappingFrom[O], A]
	                    (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :This LeftJoin T =
		LeftJoin(this, RowSource(table))


	@inline def rightJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :This RightJoin T = RightJoin(this, table)

	@inline def rightJoin[T[O] <: MappingFrom[O], A]
	                     (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :This RightJoin T =
		RightJoin(this, RowSource(table))


	@inline def outerJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :This LeftJoin T = LeftJoin(this, table)

	@inline def outerJoin[T[O] <: MappingFrom[O], A]
	                     (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :This LeftJoin T =
		LeftJoin(this, RowSource(table))


	@inline def subselect[T[O] <: MappingFrom[O]](table :RowSource[T]) :This Subselect T =
		Subselect(this, table)

	@inline def subselect[T[O] <: MappingFrom[O], A]
	                     (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :This Subselect T =
		Subselect(this, RowSource(table))



	@inline def param[X :SQLForm] :This WithParam X = WithParam(this, ParamSource[X]())

	@inline def param[X :SQLForm](name :String) :This WithParam X = WithParam(this, ParamSource[X](name))


*/
	def canEqual(that :Any) :Boolean = that.isInstanceOf[FromClause]


}






object FromClause {

	/** A `FromClause` representing the ''from'' clause of a subselect of a select with a ''from'' clause `F`.
	  * `S &lt;: SubselectFrom[F]` if and only if `S` has the form of `F Subselect M1 With M2 ... With MN`
	  * for some mappings `M1...MN` and both types are full clauses. Sources conforming to `SubselectFrom[F]` can use
	  * all the mappings/tables which are a part of `F`, but they are not a part of any select formulas created
	  * from that source. This allows the use of nested select queries which depend on values from the ''from'' clause
	  * of the outer select. Rather counterintuitively, this type is contravariant rather than covariant.
	  * There are two reasons behind it: one, preventing any clause from being a subselect clause of a clause
	  * with an abstract prefix, ensuring that full mapping lists are compared, and two, treating all join
	  * kinds as equivalent for this purpose. Note that subselects may be nested to an arbitrary depth
	  * and only directly nested subselects of `F` conform to this type.
	  */
	type SubselectFrom[-F <: FromClause] = FromClause {
		type Outer >: F <: FromClause
	}


	/** A full `FromClause`, which doesn't contain any parameters and is not a basis for a subselect of another clause.
	  * `S &lt;: OuterFrom` if and only if it is a concrete clause (without any abstract types or `FromClause`
	  * occurrences in its definition) and doesn't contain any `WithParam` or `Subselect`.
	  * The name stems from the fact that only such sources can be used to create independent select statements.
	  */
	type OuterFrom = FromClause {
		type Outer = FromClause
	}


//	/** A `FromClause` without any tables specified, representing a single statement parameter `X`.
//	  * This type encompasses all from clauses ending with a synthetic parameter mapping, not simply the empty ones.
//	  */
//	type ParamFrom[X, O] = FromClause WithParam X




	/** A wrapper type adapting the labeled mapping type `L @: M` to a form with a single-argument type constructor
	  * accepting the `Origin` type for use in `With` classes and other types accepting such a type constructor:
	  * `Dual With (Humans As "humans")#T` (where `Humans[O] &lt;: MappingFrom[O]`).
	  * @see [[bits.LabeledMapping.@:]]
	  */
	type As[M[O] <: MappingFrom[O], L <: Label] = { type T[O] = L @: M[O] }

	class AliasedSource[T[O] <: MappingFrom[O], A <: Label](source :RowSource[T], val alias :A)
		extends NamedSource[A, (T As A)#T]
	{
		override def name :A = alias

		override def apply[O] :A @: T[O] =
			(alias @: source[O].asInstanceOf[TypedMapping[Any, Any]]).asInstanceOf[A @: T[O]]
	}

	def AliasedSource[T[O] <: MappingFrom[O], A <: Label](source :RowSource[T], alias :A) :NamedSource[A, (T As A)#T] =
		new AliasedSource(source, alias)






//	@inline implicit def FromClauseMethods[F <: FromClause](from :F) :FromClauseMethods[F, from.type] =
//		new FromClauseMethods[F, from.type](from)


	/** Extension methods for `FromClause` classes which benefit from having a static, invariant self type.
	  * Most notable, this includes methods for joining it with other relations.
	  */
	implicit class FromClauseMethods[F <: FromClause](private val self :F) {

		@inline def tables :JoinedTables[F] = new JoinedTables[F](self)

		@inline def formulas :JoinedRelations[F] = new JoinedRelations[F](self)



		def join[T[O] <: MappingFrom[O]](table :RowSource[T]) :F InnerJoin T =
			(self :FromClause) match {
				case Dual() => From(table).asInstanceOf[F InnerJoin T]
				case _ => InnerJoin(self, table)
			}

		@inline def join[T[O] <: MappingFrom[O], A]
		                (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :F InnerJoin T =
			join(RowSource(table))

		@inline def join[C <: FromClause, R <: FromClause]
		                (other :C)(implicit joined :JoinAll.FirstInner.JoinClauses[F, C, R]) :R =
			joined(self, other)



		@inline def leftJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :F LeftJoin T = LeftJoin(self, table)

		@inline def leftJoin[T[O] <: MappingFrom[O], A]
		                    (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :F LeftJoin T =
			LeftJoin(self, RowSource(table))

		@inline def leftJoin[C <: FromClause, R <: FromClause]
		                    (other :C)(implicit joined :JoinAll.FirstLeft.JoinClauses[F, C, R]) :R =
			joined(self, other)



		@inline def rightJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :F RightJoin T = RightJoin(self, table)

		@inline def rightJoin[T[O] <: MappingFrom[O], A]
		                     (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :F RightJoin T =
			RightJoin(self, RowSource(table))

		@inline def rightJoin[C <: FromClause, R <: FromClause]
		                     (other :C)(implicit joined :JoinAll.FirstInner.JoinClauses[F, C, R]) :R =
			joined(self, other)



		@inline def outerJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :F LeftJoin T = LeftJoin(self, table)

		@inline def outerJoin[T[O] <: MappingFrom[O], A]
		                     (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :F LeftJoin T =
			LeftJoin(self, RowSource(table))

		@inline def outerJoin[C <: FromClause, R <: FromClause]
		                     (other :C)(implicit joined :JoinAll.FirstLeft.JoinClauses[F, C, R]) :R =
			joined(self, other)



		@inline def subselect[T[O] <: MappingFrom[O]](table :RowSource[T]) :F Subselect T =
			Subselect(self, table)

		@inline def subselect[T[O] <: MappingFrom[O], A]
		                     (table :T[A])(implicit alias :OriginProjection[T[A], A, T[Any], Any]) :F Subselect T =
			Subselect(self, RowSource(table))

		@inline def subselect[C <: FromClause, R <: FromClause]
		                     (other :C)(implicit joined :JoinAll.FirstSubselect.JoinClauses[F, C, R]) :R =
			joined(self, other)



		@inline def param[X :SQLForm] :F WithParam X = WithParam(self, ParamSource[X]())

		@inline def param[X :SQLForm](name :String) :F WithParam X = WithParam(self, ParamSource[X](name))

//		@inline def withParam[X, N](param :N ?: X) :F WithParam (N ?: X) =
//			WithParam(self, param)



//		@inline def where(filter :JoinedTables[S#Generalized] => BooleanFormula[S#Generalized]) :F#This =
//			self.filter(filter(new JoinedTables(self)))



	}






	implicit class JoinedTables[F <: FromClause](private val self :F) extends AnyVal {

		def of[E](implicit get :BySubject.Get[F, E]) :get.T[get.J] = get(self).mapping

		def apply[A <: String with Singleton](alias :A)(implicit get :ByLabel.Get[F, A]) :get.T[get.J] =
			get(self).mapping

		def apply[M[O] <: MappingFrom[O]](implicit get :ByTypeConstructor.Get[F, M]) :M[get.J] =
			get(self).mapping


		def apply[N <: INT](n :N)(implicit get :GetTableByIndex[F, N]) :get.T[get.J] = get(self).mapping


		def last(implicit get :GetTableByNegativeIndex[F, -1]) :get.T[get.J] = get(self).mapping

		def prev(implicit get :GetTableByNegativeIndex[F, -2]) :get.T[get.J] = get(self).mapping

//		def param[N <: String with Singleton](name :N)(implicit get :GetTableByPredicate[F, ByOrigin, N])

	}



	class JoinedRelations[F <: FromClause](private val self :F) extends AnyVal {

		def of[E](implicit get :BySubject.Get[F, E]) :JoinedRelation[get.J, get.T] = get(self)

		def apply[M[O] <: MappingFrom[O]](implicit get :ByTypeConstructor.Get[F, M]) :JoinedRelation[get.J, M] =
			get(self)

		def apply[A <: String with Singleton]
		         (alias :A)(implicit get :ByLabel.Get[F, A]) :JoinedRelation[get.J, get.T] =
			get(self)


		def apply[N <: INT](n :N)(implicit get :GetTableByIndex[F, N]) :JoinedRelation[get.J, get.T] =
			get(self)

		def last(implicit get :GetTableByNegativeIndex[F, -1]) :JoinedRelation[get.J, get.T] = get(self)

		def prev(implicit get :GetTableByNegativeIndex[F, -2]) :JoinedRelation[get.J, get.T] = get(self)

//		def param[N <: String with Singleton](name :N)(implicit get :GetTableByPredicate[F, ByOrigin, N])

	}






	@implicitNotFound("Can't calculate the size of the clause ${F}. Either the FROM clause is not fully instantiated " +
		              "or the expected number ${N} is incorrect. Missing implicit: FromClauseSize[${F}, ${N}].")
	class FromClauseSize[-F <: FromClause, N <: INT] private (val size :N) //extends AnyVal //AnyVal crashes scalac

	object FromClauseSize {
		implicit val DualCount :FromClauseSize[Dual, 0] = new FromClauseSize[Dual, 0](0)

		implicit def moreTables[L <: FromClause, R[O] <: MappingFrom[O], M <: INT, N <: INT]
		                       (implicit count :FromClauseSize[L, M], plus :Inc[M, N]) :FromClauseSize[L With R, N] =
			new FromClauseSize[L With R, N](plus.n)
	}



	@implicitNotFound("Failed counting tables in the clause ${F}. " + //AnyVal crashes scalac
	                  "Is ${N} the number of mappings listed by its definition? Missing implicit: TableCount[${F}, ${N}].")
	class TableCount[F <: FromClause, N <: INT] private[FromClause] (val count :N) //extends AnyVal//must be invariant

	sealed abstract class UnspecifiedTableCount {
		implicit val fromClauseHasZero :TableCount[FromClause, 0] = new TableCount[FromClause, 0](0)
	}

	object TableCount extends UnspecifiedTableCount {
		implicit val dualHasZero :TableCount[Dual, 0] = new TableCount[Dual, 0](0)

		implicit def forOneTableMore[F <: FromClause, T[O] <: MappingFrom[O], M <: INT, N <: INT]
		                            (implicit count :TableCount[F, M], inc :Inc[M, N]) :TableCount[F With T, N] =
			new TableCount[F With T, N](inc.n)
	}






	/** Implicit resolution of the `N`-th relation in the ''from'' clause `F`. This works both for positive numbers,
	  * indexed from zero and going from left to right (in which case `F` must be complete), and negative -
	  * indexed from `-1` and going from right to left (which is available always, but which index changes with
	  * joining new tables).
	  * @tparam F the input `FromClause`.
	  * @tparam N index of the desired relation as a literal `Int` type.
	  */
	@implicitNotFound("Cannot get ${N}-th relation of ${F}. \n" +
		              "Either ${N} >= size (where size is the number of relations in the FROM clause),\n" +
		              "or -${N} is greater the number of relations in the instantiated suffix of the FROM clause,\n" +
		              "or ${N} >= 0 and the size is not known (the clause type starts with FromClause and not Dual/From.")
	sealed abstract class GetTableByIndex[-F <: FromClause, N <: INT] {
		/** The ''negative'' index of the found relation: that's `-1` for the last relation in the clause
		  * and decreases going left. */
		type I <: INT
//		/** A unique origin type with the negative index of the relation encoded in it. */
		type O = ##[I]
		type J >: F <: FromClause

		/** The mapping type of the relation at index `N`. */
		type T[O] <: MappingFrom[O]

		val stretch :FromClause With T ExtendedBy J


		/** Getter for the relation from the input `FromClause`. */
		def apply(from :F) :JoinedRelation[J, T] = table(from).extend(stretch)

		def table(from :F) :JoinedRelation[FromClause With T, T]
	}


	object GetTableByIndex {

		@implicitNotFound("Can't get the relation at the (negative) index ${N} in ${F}. " +
			              "Either ${N} >= 0 or -${N} is greater than the number of known tables in the FROM clause.")
		sealed abstract class GetTableByNegativeIndex[-F <: FromClause, N <: INT] extends GetTableByIndex[F, N] {
			override type I = N
		}


		implicit def lastNegative[M[O] <: MappingFrom[O]]
				:GetTableByNegativeIndex[FromClause With M, -1] { type T[O] = M[O]; type J = FromClause With M } =
			new GetTableByNegativeIndex[FromClause With M, -1] {
				override type T[O] = M[O]
				type J = FromClause With M

				override val stretch = implicitly[J ExtendedBy J]

				override def table(from :FromClause With M) :JoinedRelation[FromClause With M, M] =
					from.lastTable
			}


		implicit def previousNegative[L <: FromClause, R[O] <: MappingFrom[O], N <: INT, M <: INT]
		                             (implicit minus :Inc[N, M], get :GetTableByNegativeIndex[L, M])
				:GetTableByNegativeIndex[L With R, N] { type T[O] = get.T[O]; type J = get.J With R } =
			new GetTableByNegativeIndex[L With R, N] {
				override type T[O] = get.T[O]
				override type J = get.J With R

				override val stretch = get.stretch.stretch[R]

				override def table(from :L With R) :JoinedRelation[FromClause With T, T] = get.table(from.left)
			}



		@implicitNotFound("Can't get relation at the (non-negative) index ${N} in ${F}. Either ${N} < 0, " +
		                  "or ${N} is greater or equal than the number of relations in the FROM clause, " +
		                  "or the exact number of relations in the FROM clause is not known.")
		sealed abstract class GetTableByPositiveIndex[-F <: FromClause, N <: INT, M <: INT] extends GetTableByIndex[F, N] {
			type I = M
		}

		implicit def lastPositive[L <: FromClause, R[O] <: MappingFrom[O], N <: INT]
		                         (implicit count :FromClauseSize[L, N])
				:GetTableByPositiveIndex[L With R, N, -1] { type T[O] = R[O]; type J = FromClause With R } =
			new GetTableByPositiveIndex[L With R, N, -1] {
				override type T[O] = R[O]
				override type J = FromClause With R

				override val stretch = implicitly[J ExtendedBy J]

				override def table(from :L With R) = from.lastTable
			}

		implicit def previousPositive[L <: FromClause, R[O] <: MappingFrom[O], N <: INT, I <: INT, J <: INT]
		                             (implicit get :GetTableByPositiveIndex[L, N, J], minus :Inc[I, J])
				:GetTableByPositiveIndex[L With R, N, I] { type T[O] = get.T[O]; type J = get.J With R } =
			new GetTableByPositiveIndex[L With R, N, I] {
				override type T[O] = get.T[O]
				override type J = get.J With R

				override val stretch = get.stretch.stretch[R]

				override def table(from :L With R) = get.table(from.left)
			}

	}



	/** Implicit resolution of retrieving a relation from a ''from'' clause where the `Mapping` type satisfies
	  * some predicate. The search starts with the last (rightmost) relation in the clause and goes backward,
	  * looking for the first relation with an implicit `Predicate[M, X]`.
	  */
	class GetTableByPredicate {

		/** Witnesses that mapping `M[Any]` (where `Any` is the `Origin` type) satisfies the sarch predicate
		  * and should be returned. Note that the predicate is invariant with the regards to a mapping, so
		  * it will not match subclasses of a matching mapping, but this can be controlled by the variance of the
		  * implicit value declaration to achieve that effect.
		  * @tparam M the type constructor of the mapping of the relation, accepting an arbitrary `Origin` type parameter.
		  * @tparam X the input key of the search: the type provided by the accessor, such as a label for a `LabeledMapping`.
		  */
		@implicitNotFound("Mapping ${M} does not match the key type ${X}.")
		final class Predicate[-M[A] <: MappingFrom[A], X]

		private[this] final val predicate = new Predicate[MappingFrom, Any]

		/** Subclasses can use this method as the implicit */
		protected def report[M[A] <: MappingFrom[A], X] :Predicate[M, X] =
			predicate.asInstanceOf[Predicate[M, X]]



		/** Implicit resolution of search for a mapping `M` in the `FromClause` `F` satisfying a `Predicate[M, X]`
		  * (that is, an `M` for which such an implicit value exists. The type of the mapping of the found relation
		  * is returned as the member type `T[O]`. In other words, an implicit value `found :Found[F, X, I]` witnesses
		  * that `found.T` is the mapping of the last relation (rightmost) in the clause `F` for which an implicit
		  * `Predicate[T, X]` exists, with `I` being the ''negative'' index of the mapping (starting with `-1`
		  * for the last mapping and decreasing).
		  */
		@implicitNotFound("Cannot find a mapping for key type ${X} in the clause ${F}.")
		sealed abstract class Found[-F <: FromClause, X, I <: INT] {
			/** The accessed `Mapping` type. */
			type T[O] <: MappingFrom[O]

			type J >: F <: FromClause

			/** The negative index of the relation. */
			def shift :I

			val stretch :FromClause With T ExtendedBy J

			/** Getter for the matching relation. */
			def apply(from :F) :JoinedRelation[J, T] = table(from).extend(stretch)

			def table(from :F) :JoinedRelation[FromClause With T, T]
		}

		implicit def last[M[O] <: MappingFrom[O], X](implicit pred :Predicate[M, X])
				:Found[FromClause With M, X, -1] { type T[O] = M[O]; type J = FromClause With M } =
			new Found[FromClause With M, X, -1] {
				override type T[O] = M[O]
				override type J = FromClause With M

				override val stretch = implicitly[J ExtendedBy J]

				override def shift = -1 : -1

				override def table(from :FromClause With M) =
					from.lastTable
			}

		implicit def previous[L <: FromClause, R[O] <: MappingFrom[O], X, I <: INT, J <: INT]
		                     (implicit get :Found[L, X, J], minus :Inc[I, J])
				:Found[L With R, X, I] { type T[O] = get.T[O]; type J = get.J With R } =
			new Found[L With R, X, I] {
				override type T[O] = get.T[O]
				override type J = get.J With R

				override val stretch = get.stretch.stretch[R]

				override val shift = minus.m

				override def table(from :L With R) = get.table(from.left)
			}



		/** An implicit result of the resolution derived from some implicit `Found[F, X, I]`. This duplication
		  * is required because the index `I` needs to be a type parameter for the type-level arithmetic to work
		  * and the accessor methods accepting `Get` do not declare 'output' types. Additionally, different
		  * accessors with the same signature but for different `GetTableByPredicate` instance will lead to conflicts
		  * after erasure, so subclasses introduce their own `Get` subclass to prevent it.
		  * @tparam F the input `FromClause` from which the relation is taken.
		  * @tparam X the 'key' type used to match the `Mapping` types of all relations in search for an implicit
		  *           `Predicate[M[Any], X]`.
		  */
		abstract class Get[-F <: FromClause, X] {
			/** The ''negative'' index of the accessed relation, starting with `-1` for the rightmost relation in `F`.*/
			type I <: INT
			/** A unique origin type with the negative index of the relation encoded in it. */
			type O = ##[I]

			/** The last mapping type matching `X` in `F`. */
			type T[O] <: MappingFrom[O]

			type J >: F <: FromClause

			/** The negative index of the found relation, starting with `-1` and decreasing from right to left. */
			def shift :I

			/** Returns the found relation from the input `FromClause`. */
			def apply(from :F) :JoinedRelation[J, T]
		}
	}



	object GetTableByPredicate {

		/** Accesses relations in a `FromClause` based on their `Subject` member type.
		  * Implicit `BySubject.Get[F, S]` returns the rightmost relation with the subject type `S`
		  */
		object BySubject extends GetTableByPredicate {
			implicit def satisfies[M[A] <: TypedMapping[S, A], S] :Predicate[M, S] = report[M, S]

			/** An implicit accessor object for the last relation in `F` with `Subject` type `S`.
			  * The type and index of the relation are returned as members `T[O]` and `I`/ `shift :I`. */
			@implicitNotFound("No relation with Subject type ${S} in the from clause ${F}:\n" +
			                  "no implicit value for BySubject.Found[${F}, ${S}].")
			sealed abstract class Get[-F <: FromClause, S] extends super.Get[F, S]

			implicit def Get[F <: FromClause, S, N <: INT]
			                (implicit found :Found[F, S, N] { type T[O] <: TypedMapping[S, O] })
					:Get[F, S] { type T[O] = found.T[O]; type J = found.J; type I = N } =
				new Get[F, S] {
					override type I = N
					override type J = found.J
					override type T[O] = found.T[O]

					override def shift = found.shift
					override def apply(from :F) :JoinedRelation[J, T] = found(from)
				}
		}



		/** Accesses relations in a `FromClause` based on their `Label` (that is, the `Label` type parameter
		  * of the `LabeledMapping` used for the relation. Implicit ByLabel.Get[F, L] returns the last relation
		  * with label `L` in `F`. */
		object ByLabel extends GetTableByPredicate {
			implicit def satisfies[M[A] <: LabeledMapping[L, _, A], L <: Label] :Predicate[M, L] = report[M, L]

			/** Accessor for the right-most relation in `F` with mapping conforming to `LabeledMapping[A, _, _]`. */
			@implicitNotFound("No relation with Alias type ${A} in the from clause ${F}:\n" +
			                  "no implicit value for ByLabel.Found[${F}, ${A}].")
			sealed abstract class Get[-F <: FromClause, A <: Label] extends super.Get[F, A] {
				type T[O] <: LabeledMapping[A, _, O]
			}

			implicit def Get[F <: FromClause, A <: Label, N <: INT]
			                (implicit found :Found[F, A, N] { type T[O] <: LabeledMapping[A, _, O] })
					:Get[F, A] { type T[O] = found.T[O]; type J = found.J; type I = N } =
				new Get[F, A] {
					override type I = N
					override type T[O] = found.T[O]
					override type J = found.J

					override def shift = found.shift
					override def apply(from :F) :JoinedRelation[J, T] = found(from)
				}
		}



		/** Accesses relations in a `FromClause` based on the type constructor for the mapping accepting
		  * the `Origin` type. The provided type constructor may be for a super type of the actual mapping.
		  * While the input to match accepts only a single parameter, it is possible to match mappings with multiple
		  * parameters as long as all of them are fixed by using a ''type lambda'' as the argument:
		  * `({ type M[O] = SomeMapping[X1, X2, ..., O] })#M`. */
		object ByTypeConstructor extends GetTableByPredicate {
			implicit def satisfies[M[A] <: MappingFrom[A]] :Predicate[M, M[Any]] = report[M, M[Any]]

			/** An implicit accessor object for the last relation in `F` with `Mapping` type `M[_]`.
			  * The type and index of the relation are returned as members `T[O]` and `I`/ `shift :I`. */
			@implicitNotFound("No relation with type constructor ${M} in the from clause ${F}:\n" +
			                  "no implicit value for ByTypeConstructor.Found[${F}, ${M}].")
			sealed abstract class Get[-F <: FromClause, M[O] <: MappingFrom[O]] {
				type I <: INT
				/** A unique origin type with the negative index of the relation encoded in it. */
				type O = ##[I]
				type T[O] = M[O]
				type J >: F <: FromClause

				def apply(from :F) :JoinedRelation[J, M]
				def shift :I
			}

			implicit def Get[F <: FromClause, M[O] <: MappingFrom[O], N <: INT]
			                (implicit found :Found[F, M[Any], N] { type T[O] = M[O] })
					:Get[F, M] { type J = found.J; type I = N } =
				new Get[F, M] {
					override type I = N
					override type J = found.J
					override def shift = found.shift
					override def apply(from :F) = found(from)
				}
		}

	}






	sealed abstract class JoinWithDual {
		type Joined[F <: FromClause, S <: FromClause, R <: FromClause]

		@inline implicit def joinDual[F <: FromClause] :Joined[F, Dual, F] = dual[F]

		protected def dual[F <: FromClause] :Joined[F, Dual, F]
	}



	class JoinAll[W[+L <: FromClause, R[O] <: MappingFrom[O]] <: Join[L, R]] private (start :Dual W MappingFrom)
		extends JoinWithDual
	{

		type Joined[F <: FromClause, S <: FromClause, R <: FromClause] = JoinClauses[F, S, R]

		@implicitNotFound("Can't join FROM clauses ${F} and ${S}. The second clause type must not be abstract. " +
			              "Missing implicit JoinAll[${F}, ${S}, ${R}].")
		abstract class JoinClauses[F <: FromClause, S <: FromClause, R <: FromClause] {
			def apply(first :F, second :S) :R
		}



		protected override def dual[F <: FromClause] :JoinClauses[F, Dual, F] =
			(first :F, _ :Dual) => first

		implicit def joinFirst[L <: FromClause, R[O] <: MappingFrom[O]]
				:JoinClauses[L, Dual With R, W[Dual, MappingFrom]#LikeJoin[L, R]] =
			(first :L, second :Dual With R) =>
				start.copy(first, second.right, second.condition.asInstanceOf[BooleanFormula[L With R]])

		implicit def joinNext[F <: FromClause, S <: FromClause, J <: L With R, L <: FromClause, R[O] <: MappingFrom[O],
		                      E <: FromClause]
		                     (implicit joinLeft :JoinClauses[F, L, E], conforms :Conforms[S, J, L With R])
				:JoinClauses[F, S, J#JoinRight[E]] =
			(first :F, second :S) => //we can simply cast the condition as it works on any clause with J as its suffix
				second.copy(joinLeft(first, second.left), second.condition.asInstanceOf[BooleanFormula[E With R]])

	}



	object JoinAll {
		private[this] val dummy = RowSource(ChainMapping[Any] :MappingFrom[Any])

		val FirstLeft = new JoinAll[LeftJoin](LeftJoin(Dual, dummy))
		val FirstRight = new JoinAll[RightJoin](RightJoin(Dual, dummy))
		val FirstInner = new JoinAll[InnerJoin](InnerJoin(Dual, dummy))
		val FirstSubselect = new JoinAll[Subselect](Subselect(Dual, dummy))
	}






	type ReverseTables[F <: FromClause, R <: FromClause] = ReversedClause[F, Dual, R]

	@implicitNotFound("Cannot reverse the order of mappings in the FROM clause {$F}: Either it is an incomplete type " +
	                  "or (${A}) With (${R}) is not the result. Missing implicit for ReversedClause[${F}, ${A}, ${R}].")
	class ReversedClause[F <: FromClause, A <: FromClause, R <: FromClause]

	object ReversedClause {
		private[this] val instance = new ReversedClause[Dual, Dual, Dual]

		implicit def reversedEnd[A <: FromClause] :ReversedClause[Dual, A, A] = instance.asInstanceOf[ReversedClause[Dual, A, A]]

		def reversed[F <: FromClause, T[O] <: MappingFrom[O], A <: FromClause, R <: FromClause]
		            (implicit reversed :ReversedClause[F, A With T, R]) :ReversedClause[F With T, A, R] =
			reversed.asInstanceOf[ReversedClause[F With T, A, R]]
	}






	abstract class FromExtension { suffix =>
		type F[P <: FromClause] <: FromClause
		def extension[P <: FromClause] :P ExtendedBy F[P]

		def stretch[T[O] <: MappingFrom[O]] :FromExtension { type F[P <: FromClause] = suffix.F[P With T] } =
			new FromExtension {
				override type F[P <: FromClause] = suffix.F[P With T]

				override def extension[P <: FromClause] =
					suffix.extension[P With T].stretchFront
			}
	}

	object FromExtension {
		def apply[R[O] <: MappingFrom[O]] :FromExtension { type F[P <: FromClause] = P With R } =
			new FromExtension {
				override type F[P <: FromClause] = P With R

				override def extension[P <: FromClause] = implicitly[P ExtendedBy (P With R)]
			}

	}



	/** Proof that the ''from'' clause `S` is an extension of the clause `F` / the clause `F` is a prefix
	  * of the clause of `S`. It means that `S &lt;: F With T1 ... With TN forSome { type T1 ... TN }`.
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a full clause and has a generalized prefix.
	  * The implication here is that this witness can be relied upon only in the context of the actual
	  * extension and is not to be treated as a generalized subtyping hierarchy.
	  */
	@implicitNotFound("FromClause ${F} is not a prefix of the clause ${S} (ignoring join kinds).")
	class ExtendedBy[F <: FromClause, -S <: FromClause] private (val length :Int) extends AnyVal {

		def stretch[R[O] <: MappingFrom[O]] :F ExtendedBy (S With R) = new ExtendedBy(length + 1)

		def stretchFront[L <: FromClause, R[O] <: MappingFrom[O]](implicit front :F <:< (L With R)) :L ExtendedBy S =
			new ExtendedBy(length + 1)
//		def apply[T[O] <: MappingFrom[O]](table :JoinedRelation[F, T]) :JoinedRelation[S, T] =
//			table.asInstanceOf[JoinedRelation[S, T]]
//
//		 def apply[E[-R <: FromClause , X] <: SQLFormula[R, X], T](expression :E[F, T]) :E[S, T] =
//			expression.asInstanceOf[E[S, T]]
//
//		def apply[T <: Chain](expression :ChainTuple[F, T]) :ChainTuple[S, T] =
//			expression.asInstanceOf[ChainTuple[S, T]]
//		def apply[T](expression :SQLFormula[F, T]) :SQLFormula[S, T] =
//			expression.asInstanceOf[SQLFormula[S, T]]
//
//		def apply[T](expression :ColumnFormula[F, T]) :ColumnFormula[S, T] =
//			expression.asInstanceOf[ColumnFormula[S, T]]
	}

	object ExtendedBy {
		private[this] val instance = new ExtendedBy[FromClause, FromClause](0)

		implicit def itself[F <: FromClause] :ExtendedBy[F, F] = instance.asInstanceOf[F ExtendedBy F]

		implicit def extend[S <: FromClause, L <: FromClause, R[A] <: MappingFrom[A]]
		                 (implicit ev :S ExtendedBy L) :S ExtendedBy (L With R) =
			new ExtendedBy(ev.length + 1)
	}





}



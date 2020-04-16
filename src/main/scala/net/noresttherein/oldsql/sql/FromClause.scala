package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.abacus.{INT, Plus}
import net.noresttherein.oldsql.schema.{GenericMapping, Mapping, RowSource, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAlias, MappingFrom, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.support.LabeledMapping
import net.noresttherein.oldsql.schema.support.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.RowSource.NamedSource
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.FromClause.GetTableByIndex.GetTableByNegativeIndex
import net.noresttherein.oldsql.sql.FromClause.GetTableByPredicate.{ByLabel, BySubject, ByTypeConstructor}
import net.noresttherein.oldsql.sql.FromClause.JoinedTables
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.AnyRelationIn
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula, Formula}
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple
import net.noresttherein.oldsql.sql.WithParam.{FromParam, ParamSource}

import scala.annotation.implicitNotFound





trait FromClause {

	/** Type of the last mapping in this join (the rightmost one) if not empty. */
	type LastMapping[O] <: MappingFrom[O]

	/** Table alias proxy for the last table (or table-like expression) in this list as seen from some `FromClause`
	  * type `F` containing this instance in its 'tail'. In other words, this projects the type of the last element
	  * of this clause to an extending row source.
	  */
	type LastTable[-F <: FromClause] <: JoinedRelation[F, LastMapping, _]

	type FromLast >: this.type <: FromClause

	/** Last mapping in this source when treated as a list, if any. */
	def lastTable :LastTable[FromLast] //:JoinedTable[this.type, _<:Mapping]



	/** Self type of this clause used by some copy constructors present in the subclasses. */
	type This >: this.type <: FromLast



	/** The type of this clause with all join kinds replaced with the root `With`. It forms the class of abstraction
	  * of clauses with the same relations (and in the same order), which are considered equivalent for most purposes.
	  */
	type Generalized <: FromClause

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
	  * by a `.subbselect()` call. If there's no `Subselect` in the dynamic type definition of this source, meaning
	  * this is not a subselect clause (''from'' clause of resulting selects will include all members of this clause),
	  * `Dual` instance used as the relation list terminator is returned.
	  * @return `outer` of the left side or just the left side if this instance is a `Subselect`.
	  */
	def outer :Outer



	/** Number of mappings contained in this join (counting all their occurrences separately). */
	def size :Int



//	def tables[U >: This] :JoinedTables[U] = new JoinedTables[U](this)

	/** Result types of all mappings in this source concatenated into a heterogeneous list.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `With` (but unlike `::`), left associative.
	  */
	type Row <: Chain

	/** Create an sql tuple formula containing `TableFormula`s for all joined tables in their order of appearance.
	  * It will contain entries for all mappings in this clause, including parameter mappings and mappings listed in this
	  * source's `Outer` prefix (if this source is a subselect source).
	  */
	def row :ChainTuple[this.type, Row]

	def tableStack :LazyList[AnyRelationIn[this.type]]


	type SubselectRow <: Chain

	def subselectRow :ChainTuple[this.type, SubselectRow]

	def subselectTableStack :LazyList[AnyRelationIn[this.type]]



	/** Function type returning a join condition between the last relation in this clause and a following mapping `T`.
	  * Used as the parameter for [[net.noresttherein.oldsql.sql.With#on With.on]], it is declared here
	  * (on the 'left side' of a new join) to ensure that `on` won't be possible to call if the left side of
	  * a `With` clause is abstract or empty, i.e. that the clause contains at least two relations and their
	  * types are known.
	  */
	type JoinFilter[T[O] <: MappingFrom[O]] <:
		(JoinedRelation[FromLast With T, LastMapping, _ <: -2], JoinedRelation[FromClause With T, T, _ <: -1])
			=> BooleanFormula[FromLast With T]

	private[sql] def filterJoined[T[O] <: MappingFrom[O]](filter :JoinFilter[T], next :FromClause With T)
			:next.JoinRight[This] =
		next.copy[This](this, filter( //todo: get rid of casts for origin
			lastTable.asInstanceOf[JoinedRelation[FromLast, LastMapping, -2]].asPartOf[FromLast, FromLast With T],
			next.lastTable.asInstanceOf[JoinedRelation[FromClause With T, T, -1]]
		))



/*
	def join[T[O] <: MappingFrom[O]](table :RowSource[T]) :This InnerJoin T =
		this match {
			case Dual() => From(table).asInstanceOf[This InnerJoin T]
			case _ => InnerJoin(this, table)
		}

	@inline def join[T[O] <: MappingFrom[O], A]
	                (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :This InnerJoin T =
		join(RowSource(table))


	@inline def leftJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :This LeftJoin T = LeftJoin(this, table)

	@inline def leftJoin[T[O] <: MappingFrom[O], A]
	                    (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :This LeftJoin T =
		LeftJoin(this, RowSource(table))


	@inline def rightJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :This RightJoin T = RightJoin(this, table)

	@inline def rightJoin[T[O] <: MappingFrom[O], A]
	                     (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :This RightJoin T =
		RightJoin(this, RowSource(table))


	@inline def outerJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :This LeftJoin T = LeftJoin(this, table)

	@inline def outerJoin[T[O] <: MappingFrom[O], A]
	                     (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :This LeftJoin T =
		LeftJoin(this, RowSource(table))


	@inline def subselect[T[O] <: MappingFrom[O]](table :RowSource[T]) :This Subselect T =
		Subselect(this, table)

	@inline def subselect[T[O] <: MappingFrom[O], A]
	                     (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :This Subselect T =
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
	  * @see [[net.noresttherein.oldsql.schema.support.LabeledMapping.@:]]
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



	implicit class FromClauseMethods[F <: FromClause](private val self :F) {

		@inline def tables :JoinedTables[F] = new JoinedTables[F](self)

//		def row(implicit row :RowOf[F, F#Row]) :ChainTuple[F, F#Row] = row(self)



		def join[T[O] <: MappingFrom[O]](table :RowSource[T]) :F InnerJoin T =
			(self :FromClause) match {
				case Dual() => From(table).asInstanceOf[F InnerJoin T]
				case _ => InnerJoin(self, table)
			}

		@inline def join[T[O] <: MappingFrom[O], A]
		                (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :F InnerJoin T =
			join(RowSource(table))


		@inline def leftJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :F LeftJoin T = LeftJoin(self, table)

		@inline def leftJoin[T[O] <: MappingFrom[O], A]
		                    (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :F LeftJoin T =
			LeftJoin(self, RowSource(table))


		@inline def rightJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :F RightJoin T = RightJoin(self, table)

		@inline def rightJoin[T[O] <: MappingFrom[O], A]
		                     (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :F RightJoin T =
			RightJoin(self, RowSource(table))


		@inline def outerJoin[T[O] <: MappingFrom[O]](table :RowSource[T]) :F LeftJoin T = LeftJoin(self, table)

		@inline def outerJoin[T[O] <: MappingFrom[O], A]
		                     (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :F LeftJoin T =
			LeftJoin(self, RowSource(table))


		@inline def subselect[T[O] <: MappingFrom[O]](table :RowSource[T]) :F Subselect T =
			Subselect(self, table)

		@inline def subselect[T[O] <: MappingFrom[O], A]
		                     (table :T[A])(implicit alias :MappingAlias[T[A], A, T[Any], Any]) :F Subselect T =
			Subselect(self, RowSource(table))



		@inline def param[X :SQLForm] :F WithParam X = WithParam(self, ParamSource[X]())

		@inline def param[X :SQLForm](name :String) :F WithParam X = WithParam(self, ParamSource[X](name))

//		@inline def withParam[X, N](param :N ?: X) :F WithParam (N ?: X) =
//			WithParam(self, param)



//		@inline def where(filter :JoinedTables[S#Generalized] => BooleanFormula[S#Generalized]) :F#This =
//			self.filter(filter(new JoinedTables(self)))



	}






	implicit class JoinedTables[F <: FromClause](private val self :F) extends AnyVal {

		def of[E](implicit get :BySubject.Get[F, E]) :get.T[_ <: get.I] = get(self).mapping

		def apply[A <: String with Singleton](alias :A)(implicit get :ByLabel.Get[F, A]) :get.T[_ <: get.I] =
			get(self).mapping

		def apply[M[O] <: MappingFrom[O]](implicit get :ByTypeConstructor.Get[F, M]) :M[_ <: get.I] =
			get(self).mapping


		def apply[N <: INT](n :N)(implicit get :GetTableByIndex[F, N]) :get.T[_ <: get.I] = get(self).mapping


		def last(implicit get :GetTableByNegativeIndex[F, -1]) :get.T[_ <: -1] = get(self).mapping

		def prev(implicit get :GetTableByNegativeIndex[F, -2]) :get.T[_ <: -2] = get(self).mapping

//		def param[N <: String with Singleton](name :N)(implicit get :GetTableByPredicate[F, ByOrigin, N])

	}



	class JoinedRelations[F <: FromClause](private val self :F) extends AnyVal {

		def of[E](implicit get :BySubject.Get[F, E]) :JoinedRelation[F, get.T, _ <: get.I] = get(self)

		def apply[M[O] <: MappingFrom[O]](implicit get :ByTypeConstructor.Get[F, M]) :JoinedRelation[F, M, _ <: get.I] =
			get(self)

		def apply[A <: String with Singleton]
		         (alias :A)(implicit get :ByLabel.Get[F, A]) :JoinedRelation[F, get.T, _ <: get.I] =
			get(self)


		def apply[N <: INT](n :N)(implicit get :GetTableByIndex[F, N]) :JoinedRelation[F, get.T, _ <: get.I] =
			get(self)


		def last(implicit get :GetTableByNegativeIndex[F, -1]) :JoinedRelation[F, get.T, _ <: -1] = get(self)

		def prev(implicit get :GetTableByNegativeIndex[F, -2]) :JoinedRelation[F, get.T, _ <: -2] = get(self)

//		def param[N <: String with Singleton](name :N)(implicit get :GetTableByPredicate[F, ByOrigin, N])

	}






	@implicitNotFound("Failed counting the tables in ${F}. Either the FROM clause is not fully instantiated " +
		              "or the expected number ${N} is incorrect.")
	sealed class TableCount[-F <: FromClause, N <: INT] private ()

	object TableCount {
		implicit val DualCount :TableCount[Dual, 0] = new TableCount[Dual, 0]

		implicit def moreTables[L <: FromClause, R[O] <: MappingFrom[O], N <: INT, M <: INT]
		                       (implicit count :TableCount[L, N], plus :Plus[N, M]) :TableCount[L With R, M] =
			count.asInstanceOf[TableCount[L With R, M]]
	}



	@implicitNotFound("Cannot get ${N}-th relation of ${F}. \n" +
		              "Either ${N} >= size (where size is the number of relations in the FROM clause),\n" +
		              "or -${N} is greater the number of relations in the instantiated suffix of the FROM clause,\n" +
		              "or ${N} >= 0 and the size is not known (the clause type starts with FromClause and not Dual/From.")
	sealed abstract class GetTableByIndex[-F <: FromClause, N <: INT] {
		type I <: INT
		type T[O] <: MappingFrom[O]
		def apply(from :F) :JoinedRelation[F, T, _ <: I]
	}


	object GetTableByIndex {

		@implicitNotFound("Can't get the relation at the (negative) index ${N} in ${F}. " +
			              "Either ${N} >= 0 or -${N} is greater than the number of known tables in the FROM clause.")
		sealed abstract class GetTableByNegativeIndex[-F <: FromClause, N <: INT] extends GetTableByIndex[F, N] {
			override type I = N
		}


		implicit def lastNegative[M[O] <: MappingFrom[O]]
				:GetTableByNegativeIndex[FromClause With M, -1] { type T[O] = M[O] } =
			new GetTableByNegativeIndex[FromClause With M, -1] {
				override type T[O] = M[O]

				override def apply(from :FromClause With M) :JoinedRelation[FromClause With M, M, _ <: -1] =
					from.lastTable.asInstanceOf[JoinedRelation[FromClause With M, M, -1]] //todo: fix Origin
			}


		implicit def previousNegative[L <: FromClause, R[O] <: MappingFrom[O], N <: INT, M <: INT]
		                             (implicit minus :Plus[N, M], get :GetTableByNegativeIndex[L, M])
				:GetTableByNegativeIndex[L With R, N] { type T[O] = get.T[O] } =
			new GetTableByNegativeIndex[L With R, N] {
				override type T[O] = get.T[O]

				override def apply(from :L With R) :JoinedRelation[L With R, T, _ <: N] =
					get(from.left).asInstanceOf[JoinedRelation[L, T, N]].asPartOf //todo: Origin
			}



		@implicitNotFound("Can't get relation at the (non-negative) index ${N} in ${F}. Either ${N} < 0, " +
		                  "or ${N} is greater or equal than the number of relations in the FROM clause, " +
		                  "or the exact number of relations in the FROM clause is not known.")
		sealed abstract class GetTableByPositiveIndex[-F <: FromClause, N <: INT, I <: INT] extends GetTableByIndex[F, N] {
			type I = INT
		}

		implicit def lastPositive[L <: FromClause, R[O] <: MappingFrom[O], N <: INT]
		                         (implicit count :TableCount[L, N])
				:GetTableByPositiveIndex[L With R, N, -1] { type T[O] = R[O] } =
			new GetTableByPositiveIndex[L With R, N, -1] {
				override type T[O] = R[O]

				override def apply(from :L With R) =
					from.lastTable.asInstanceOf[JoinedRelation[L With R, R, N]] //todo: Origin
			}

		implicit def previousPositive[L <: FromClause, R[O] <: MappingFrom[O], N <: INT, I <: INT, J <: INT]
		                             (implicit get :GetTableByPositiveIndex[L, N, J], minus :Plus[I, J])
				:GetTableByPositiveIndex[L With R, N, I] { type T[O] = get.T[O] } =
			new GetTableByPositiveIndex[L With R, N, I] {
				type T[O] = get.T[O]

				override def apply(from :L With R) =
					get(from.left).asInstanceOf[JoinedRelation[L, T, I]].asPartOf
			}

	}



	class GetTableByPredicate {

		@implicitNotFound("Mapping ${M} does not match the key type ${X}.")
		final class Predicate[M[A] <: MappingFrom[A], X]

		private[this] final val predicate = new Predicate[MappingFrom, Any]

		protected def report[M[A] <: MappingFrom[A], X] :Predicate[M, X] =
			predicate.asInstanceOf[Predicate[M, X]]



		@implicitNotFound("Cannot find a mapping for key type ${X} in the clause ${F}.")
		sealed abstract class Found[-F <: FromClause, X, I <: INT] {
			def offset :I

			type T[O] <: MappingFrom[O]
			def apply(from :F) :JoinedRelation[F, T, _ <: I]
		}

		implicit def last[M[O] <: MappingFrom[O], X](implicit pred :Predicate[M, X])
				:Found[FromClause With M, X, -1] { type T[O] = M[O] } =
			new Found[FromClause With M, X, -1] {
				override type T[O] = M[O]

				override def apply(from :FromClause With M) = //todo: Origin
					from.lastTable.asInstanceOf[JoinedRelation[FromClause With M, M, -1]]

				override def offset = -1 : -1
			}

		implicit def previous[L <: FromClause, R[O] <: MappingFrom[O], X, I <: INT, J <: INT]
		                     (implicit get :Found[L, X, J], minus :Plus[I, J])
				:Found[L With R, X, I] { type T[O] = get.T[O] } =
			new Found[L With R, X, I] {
				override type T[O] = get.T[O]

				override def apply(from :L With R) = //todo: Origin
					get(from.left).asInstanceOf[JoinedRelation[L, T, I]].asPartOf

				override val offset = minus.n
			}

		abstract class Get[-F <: FromClause, X] {
			type I <: INT
			type T[O] <: MappingFrom[O]

			def offset :I
			def apply(from :F) :JoinedRelation[F, T, _ <: I]
		}
	}



	object GetTableByPredicate {

		object BySubject extends GetTableByPredicate {
			implicit def satisfies[M[A] <: TypedMapping[S, A], S] :Predicate[M, S] = report[M, S]

			@implicitNotFound("No relation with Subject type ${S} in the from clause ${F}:\n" +
			                  "no implicit value for BySubject.Found[${F}, ${S}].")
			sealed abstract class Get[-F <: FromClause, S] extends super.Get[F, S]

			implicit def Get[F <: FromClause, S, N <: INT]
			                (implicit found :Found[F, S, N] { type T[O] <: TypedMapping[S, O] })
					:Get[F, S] { type T[O] = found.T[O]; type I = N } =
				new Get[F, S] {
					override type I = N
					override def offset = found.offset

					override type T[O] = found.T[O]
					override def apply(from :F) :JoinedRelation[F, T, _ <: I] = found(from)
				}
		}



		object ByLabel extends GetTableByPredicate {
			implicit def satisfies[M[A] <: LabeledMapping[L, _, A], L <: Label] :Predicate[M, L] = report[M, L]

			@implicitNotFound("No relation with Alias type ${A} in the from clause ${F}:\n" +
			                  "no implicit value for ByLabel.Found[${F}, ${A}].")
			sealed abstract class Get[-F <: FromClause, A <: Label] extends super.Get[F, A] {
				type T[O] <: LabeledMapping[A, _, O]
			}

			implicit def Get[F <: FromClause, A <: Label, N <: INT]
			                (implicit found :Found[F, A, N] { type T[O] <: LabeledMapping[A, _, O] })
					:Get[F, A] { type T[O] = found.T[O]; type I = N } =
				new Get[F, A] {
					override type I = N
					override def offset = found.offset

					override type T[O] = found.T[O]
					override def apply(from :F) :JoinedRelation[F, T, _ <: I] = found(from)
				}
		}



		object ByTypeConstructor extends GetTableByPredicate {
			implicit def satisfies[M[A] <: MappingFrom[A]] :Predicate[M, M[Any]] = report[M, M[Any]]

			@implicitNotFound("No relation with type constructor ${M} in the from clause ${F}:\n" +
			                  "no implicit value for ByTypeConstructor.Found[${F}, ${M}].")
			sealed abstract class Get[-F <: FromClause, M[O] <: MappingFrom[O]] {
				type I <: INT
				type T[O] = M[O]

				def apply(from :F) :JoinedRelation[F, M, _ <: I]
				def offset :I
			}

			implicit def Get[F <: FromClause, M[O] <: MappingFrom[O], N <: INT]
			                (implicit found :Found[F, M[Any], N] { type T[O] = M[O] }) :Get[F, M] { type I = N } =
				new Get[F, M] {
					override type I = N
					override def apply(from :F) = found(from)
					override def offset = found.offset
				}
		}

	}






	@implicitNotFound("${R} is not the row type for FromClause ${F}. The most common reason is partial definition " +
	                  "of the clause type (not starting with Dual or From)")
	abstract class RowOf[-F <: FromClause, R <: Chain] extends (F => ChainTuple[F, R])

	object RowOf {
		implicit val EmptyRow :RowOf[Dual, @~] = { _ => ChainTuple.EmptyChain }

		implicit def joinRow[L <: FromClause, R[A] <: TypedMapping[_, A], S <: Chain]
		                    (implicit left :RowOf[L, S]) :RowOf[L With R, S ~ R[_]#Subject] =
			new RowOf[L With R, S ~ R[_]#Subject] {
				override def apply(join :L With R) =
					left(join.left).asPartOf(join) ~ join.lastTable// :MappingFormula[FromClause With R, R])
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
	class ExtendedBy[F <: FromClause, -S <: FromClause] private(private[sql] val length :Int) extends AnyVal {
		def apply[T[O] <: MappingFrom[O], A](table :JoinedRelation[F, T, A]) :JoinedRelation[S, T, A] =
			table.asInstanceOf[JoinedRelation[S, T, A]]

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

		implicit def join[S <: FromClause, L <: FromClause, R[A] <: MappingFrom[A]]
		                 (implicit ev :S ExtendedBy L) :ExtendedBy[S, L With R] =
			new ExtendedBy(ev.length + 1)
	}



	sealed trait UniqueOrigin extends Any { type T }

	implicit def UniqueOrigin :UniqueOrigin = new UniqueOrigin { type T = UniqueOrigin }


}

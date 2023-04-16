package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Listing
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.schema.{Mapping, SQLForm, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.{BrokeredRelationshipMapping, DirectRelationshipMapping, IndexedMapping}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.AggregateFunction.{Avg, Count, Max, Min, StdDev, Sum, Var}
import net.noresttherein.oldsql.sql.FromClause.FromClauseTemplate
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.ParamClause.{NamedParamRelation, ParamRelation}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, JoinedMappings, NonEmptyRow, NonEmptyRowTemplate, PartOf, PrefixOf, RowProductTemplate}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{AggregateSQL, ChainTuple, ComponentSQL, EmptySQL, IndexedSQL, JoinedRelation, JoinedTable, LooseColumn, RelationSQL, SelectColumn}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.TableSQL.LastTable
import net.noresttherein.oldsql.sql.mechanics.{CanGroup, CanSelect, LastTableOf, RelationCount, RowProductVisitor, SpelledSQL, SQLNumber, SQLOrdering}
import net.noresttherein.oldsql.sql.mechanics.GetTable.ByIndex
import net.noresttherein.oldsql.sql.mechanics.LastTableOf.LastBound
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.BaseMappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.FromSome.FromSomeTemplate






/** Marker trait for all true ''from'' clauses, without a ''group by'' clause. Most implementations extend
  * its non-empty subtype, [[net.noresttherein.oldsql.sql.FromSome FromSome]].
  * @see [[net.noresttherein.oldsql.sql.GroupByClause]]
  */
trait FromClause extends RowProduct with FromClauseTemplate[FromClause] { thisClause =>

//	override type Last[O] <: JoinedTable[FromLast, O]

	/** The filter condition applied to all rows represented by this product relation.
	  * It is a conjunction of all join conditions added
	  * with [[net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate.on on]] and
	  * and [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] methods to all joins
	  * included in the [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]] section of this clause.
	  * Note that the final SQL doesn't have to use all, or any of these conditions, as the ''where'' clause,
	  * but instead opt to include them in ''on'' clauses with every join clause.
	  * @return `this.`[[net.noresttherein.oldsql.sql.FromClause.filter filter]]
	  */
	def whereClause :SingleBoolean[Generalized] = filter
//	def whereClause[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :GlobalBoolean[E] =
//		filter(target)
	override def filter :SingleBoolean[Generalized] = filter(generalized)
	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :SingleBoolean[E]


	override type AppliedParam <: FromClause
	override type GeneralizedParamless >: Paramless <: FromClause
	override type Paramless <: FromClause { type Params = @~ }
	override type BoundParamless >: Paramless <: FromClause { type Params = @~ }

	/** A join between this clause and the clause `F`. This type is the result of replacing `Dual` in `F` with `Self`.
	  * Non-empty clauses define it as `F#JoinedWith[Self, J]]`, while `Dual` defines it as `F` - the indirection
	  * enforced by the join type `J` (and `Join` subclasses) having `FromSome` as the upper bound of their left side.
	  */ //consider: renaming to MultiJoin, JoinMany
	type JoinWith[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L NonParam R, F <: RowProduct] <: RowProduct

	/** Joins the clause given as the parameter with this clause. If any of the clauses is empty, the other is
	  * returned. Otherwise the created clause contains this clause as its prefix, followed by all relations
	  * from `suffix` joined with the same join kinds. The first `From` pseudo join in suffix is replaced with
	  * the join type specified by the template parameter (`join.LikeJoin`); if the first relation in `suffix` is
	  * however joined with `Dual` using another join type (such as `JoinParam`), it is preserved. This is a dispatch
	  * call to [[net.noresttherein.oldsql.sql.RowProduct.joinedWith suffix.joinedWith(self, join)]]. This extra level
	  * of indirection is enforced by the upper bound of `FromSome` on the left type parameters in `Join` classes,
	  * while this method can be called also if this clause is empty. Additionally, the join kind to use between
	  * the last relation in this clause and the first relation in `suffix` can be specified as `Subselect`,
	  * while `joinedWith` allows only `Join` subclasses.
	  *
	  * It is a low level method and client code should prefer the eponymously named extension methods
	  * for individual join kinds defined in
	  * [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]]: `join`, `outerJoin`,
	  * `innerJoin`, `leftJoin`, `rightJoin`, `subselect`, as they have more friendly return types.
	  * @param suffix the clause with relations which should be added to this clause
	  * @param join a template instance to use as the factory for the join between the last relation in this clause
	  *             and the first relation in `suffix`.
	  */
	def joinWith[F <: FromSome](suffix :F, join :JoinLike.__ = InnerJoin.template) :JoinWith[join.LikeJoin, F]

/*
	/** A join between this clause and the relation for mapping `T`. For non empty clauses, this is simply `J[Self, T]`,
	  * but `Dual` defines it as `From[T]` instead.
	  */
	type Expand[+J[+L <: Self, R[O] <: T[O]] <: L AndFrom R, T[O] <: MappingAt[O]] <: Self AndFrom T

	/** Joins this clause with another relation `next` for mapping `T`.
	  * @param next   a relation to add to the clause.
	  * @param filter an additional filter condition for the ''where'' clause of the created clause; defaults to `True`.
	  * @param join   a template `JoinLike` instance used as a factory for the returned clause; defaults to `InnerJoin`.
	  * @return `From(next)` if this clause is empty and `join.likeJoin(self, next)` for non empty clauses.
	  */ //todo: currently unused, remove and free the name
	def expand[T[O] <: BaseMapping[S, O], S]
	    (next :Table[T], filter :SingleBoolean[Generalized AndFrom T] = True, join :JoinLike.__ = InnerJoin.template)
			:Expand[join.LikeJoin, T]

	//todo: used only in the factory method of AndFrom, which isn't really needed. We can free the name.
	/** Used to add any relation to any clause, creates the clause of a type depending on this clause:
	  * empty clauses return `From[T]`, while non empty clauses create `this.type InnerJoin T`.
	  */ //consider: renaming to andFrom/joinLike/joinWith
	protected[sql] def expand[T[O] <: BaseMapping[S, O], S, A <: Label]
	                         (right :LastTable[T, S], alias :Option[A],
	                          filter :SingleBoolean[Generalized NonParam T]) :this.type NonParam T As A
*/


	override def spell[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                     (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling.fromWhere(this)(context, params)


	protected override def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.fromClause(this)


	private[sql] override def concrete_RowProduct_subclass_must_extend_FromClause_or_GroupByClause(seal :Seal) :Unit = ()
}






object FromClause {

	/** A factory interface mixed providing a covariant upper bound of `F` on produced `RowProduct` instances.
	  * It works in the vein of `IterableOps` from the standard Scala library, allowing for better type
	  * inference of the return type by upcasting it to its most specific known concrete type.
	  * For instantiated types, it should always be preferred over member types such as `Copy`, which exist
	  * to allow declaring abstract types simply as `F <: RowProduct` (or `FromSome`, `GroupByClause`, etc.), instead
	  * of `F <: RowProduct with RowProductTemplate[F]`, making for much cleaner and shorter signatures.
	  */
	trait FromClauseTemplate[+F <: FromClause] extends RowProductTemplate[F] { this :F with FromClauseTemplate[F] =>
		override def fromClause :F = this
	}


	/** Extension methods performing most generic join between any ungrouped (i.e., pure) `RowProduct`,
	  * and other relations.
	  */
	implicit class FromClauseExtension[F <: FromClause](val thisClause :F) extends AnyVal {
//
//		/** Performs an inner join between this clause on the left side, and the table given as a `Table`
//		  * object on the right side. The real type of the result depends on the type of this clause:
//		  * for `Dual`, the result is `From[R]`, for non-empty clauses the result is `F InnerJoin R`.
//		  * The join condition can be subsequently specified using
//		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
//		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
//		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] method.
//		  * @param table a producer of the mapping for the relation.
//		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
//		  * @return `From[R]` if this clause is empty or `F InnerJoin R` otherwise.
//		  */
//		@inline def andFrom[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
//		            (table :Table[R])(implicit cast :BaseMappingSubject[R, T, S]) :F NonParam R =
//			AndFrom(thisClause, table)

		/** Performs a join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side. The join type between the clauses will be an `InnerJoin` if the dynamic
		  * type of the first join of the given clause is `From[_]`, otherwise the join type is preserved.
		  * If either of the clauses is empty, the other is returned unchanged.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */ //todo: we can move it to RowProductExtension if appendedTo would make subselects
		@inline def andFrom[R <: RowProduct](other :R) :other.JoinedWith[F, NonParam] = other.appendedTo(thisClause)
	}



	/** A `RowProduct` of a top level, independent ''select'' without a ''group by'' clause - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] or [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]
	  * joins (is not a ''from'' clause of a subselect of some other select). In order to conform naturally
	  * (rather than by refinement) to `OuterDiscreteForm`, the clause must be ''complete'', that is its static type
	  * must start with either [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]]
	  * - rather than a wildcard or abstract type hiding an unknown prefix - and its `Generalized` supertype
	  * must be known, meaning all join kinds should be narrowed down at least to
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]].
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedRow GeneralizedRow]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * This is an 'ungrouped' subtype of the more generic [[net.noresttherein.oldsql.sql.RowProduct.TopRow TopRow]].
	  * See [[net.noresttherein.oldsql.sql.FromSome.TopFromSome TopFromSome]] for a variant with a
	  * [[net.noresttherein.oldsql.sql.FromSome FromSome]] upper bound. An `TopFromClause` can still
	  * contain (unbound) parameters; [[net.noresttherein.oldsql.sql.FromClause.GroundFromClause GroundFromClause]]
	  * is the specialization of this type without any `JoinParam` 'joins'.
	  * @see [[net.noresttherein.oldsql.sql.FromClause FromClause]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.TopRow]]
	  */
	type TopFromClause = FromClause {
		type Implicit = RowProduct
	}

	/** A `RowProduct` without any [[net.noresttherein.oldsql.sql.Subselect Subselect]],
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]],
	  * or [[net.noresttherein.oldsql.sql.GroupBy, GroupBy]]/[[net.noresttherein.oldsql.sql.By By All]]
	  * joins. Representing a single ''from'' clause (and not one of a nested subselect), without a ''group by'' clause,
	  * it can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedRow GeneralizedRow]], but there is no actual subtype
	  * relationship between the two in the type system. A `GroundFromClause` will however always by a subtype of
	  * its more generic variants, [[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.TopFromClause TopFromClause]] (which groups all outer clauses,
	  * including those with unbound parameters), as well as
	  * [[net.noresttherein.oldsql.sql.RowProduct.ParamlessRow ParamlessRow]].
	  */
	type GroundFromClause = FromClause {
		type Implicit = RowProduct
		type Base = RowProduct
		type DefineBase[+I <: RowProduct] = I
		type Params = @~
	}

	/** An upper bound for all ''from'' clauses without a ''group by'' clause, which do not contain
	  * any [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'joins' in their concrete type. In order to prove this
	  * conformity the clause type must be complete and cannot contain [[net.noresttherein.oldsql.sql.AndFrom AndFrom]]
	  * joins (all joins in its static type must be at least as specific as their
	  * [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form).
	  * Note that `ParamlessFromClause &lt;: `[[net.noresttherein.oldsql.sql.RowProduct.ParamlessRow ParamlessRow]]
	  * and `ParamlessFromClause =:= `[[net.noresttherein.oldsql.sql.FromClause.ParameterizedFromClause ParameterizedFromClause]]`[@~]`.
	  */
	type ParamlessFromClause = FromClause {
		type Params = @~
	}

	/** An upper bound for all ''from'' clauses without a ''group by'' clause, which are known to contain at least one
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'join' in their static type. Note that a `FromClause`
	  * not conforming to this type does not mean that it indeed contains no parameters, as the information
	  * may have been lost by type abstraction. Note that
	  * `ParameterizedFromClause[P] &lt;: `[[net.noresttherein.oldsql.sql.RowProduct.ParameterizedRow ParameterizedRow]]`[P]`.
	  */
	type ParameterizedFromClause[P] = FromClause {
		type Params = P
	}




	/** A contradictory [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], which extends
	  * [[net.noresttherein.oldsql.sql.FromSome FromSome]] despite being empty.
	  * It is a subtype of [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]]`[`[[net.noresttherein.oldsql.collection.Chain.@~ @~]]`]`,
	  * used as the start of clauses consisting solely of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] joins
	  * and serves as a domain for parameterized SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]],
	  * particularly in DML [[net.noresttherein.oldsql.sql.DMLStatement statements]], such as
	  * [[net.noresttherein.oldsql.sql.Insert.syntax.InsertParam InsertParam]] or
	  * [[net.noresttherein.oldsql.sql.Call.InOutCallFunction InOutCallFunction]].
	  *
	  * This class breaks the contract `FromSome` and throws an [[UnsupportedOperationException]] from most methods,
	  * so it should not be used in client code as it is an implementation artifact.
	  */
	private[sql] class EmptyFromSome private[FromClause](override val filter :SingleBoolean[EmptyFromSome])
		extends FromSome
	{
		override type Last[F <: RowProduct] = Nothing
		override type LastMapping[O]        = Nothing
		override type FromLast              = EmptyFromSome
		override type Generalized           = EmptyFromSome
		override type Complete              = EmptyFromSome
		override type NoAlias               = EmptyFromSome
		override type Self                  = EmptyFromSome
		override type NoAliasCopy           = EmptyFromSome
		override type Copy                  = EmptyFromSome
		override type JoinFilter            = Nothing
		override type Alias                 = Nothing

		override def last :Nothing =
			throw new NoSuchElementException("Empty parameter domain NoParams contains no relations.")

		override def generalizedClass :Class[EmptyFromSome] = classOf[EmptyFromSome]
		override def selfClass        :Class[EmptyFromSome] = classOf[EmptyFromSome]

		override def lastAsIn[E <: FromSome](implicit expansion :FromLast PrefixOf E) :Nothing = last

		override def aliasOpt :Option[Nothing] = None

		override def aliased[A <: Label](alias :A) :Nothing =
			throw new UnsupportedOperationException("An empty parameter domain NoParams cannot have an AS clause.")

		override def filtered(condition :Nothing) :EmptyFromSome = this
		override def filtered[S >: Single <: Single](filter :SQLBoolean[Generalized, S]) :EmptyFromSome =
			if (filter == True) selfAsCopy
			else new EmptyFromSome(this.filter && filter)

		override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :SingleBoolean[E] =
			filter.basedOn(target)

		override type LastParam             = Nothing
		override type Params                = @~
		override type AppliedParam          = EmptyFromSome
		override type GeneralizedParamless  = EmptyFromSome
		override type Paramless             = EmptyFromSome
		override type DecoratedParamless[D <: BoundParamless] = D

		override def bind(param :LastParam) :AppliedParam = this
		override def bind(params :Params)   :Paramless    = this
		protected override def decoratedBind[D <: FromSome { type Params = @~ }]
		                                    (params :Params)(decorate :Paramless => D) :D = decorate(this)

		override def isParameterized :Boolean = false
		override def paramCount :Int = 0
		override def lastParamOffset :Int =
			throw new UnsupportedOperationException("No parameters in an empty clause NoParams.")

		override def fullSize :Int = 0
		override type FullRow = @~

		override def fullRow[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:ChainTuple[E, Single, FullRow] =
			EmptySQL

		override def fullTableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:LazyList[RelationSQL.from[E]#__] =
			LazyList.empty

		override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] = Nothing
		override type SelectedFrom[+P <: NonEmptyRow] = Nothing

		override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) :Nothing = selectedFrom(prefix)
		override def appendedTo[F <: FromClause](prefix :F) :Nothing = selectedFrom(prefix)
		override def selectedFrom[F <: RowProduct](prefix :F) :Nothing =
			throw new UnsupportedOperationException("NoParams cannot be joined with another RowProduct.")

		override def isExplicitParameterized :Boolean = false
		override def isValidSubselect        :Boolean = false

		override type Explicit = EmptyFromSome
		override type Inner    = EmptyFromSome
		override type Implicit = RowProduct
		override type Outer    = Dual
		override type Base     = Nothing
		override type DefineBase[+I <: Implicit] = Nothing

		override val outer :Dual = Dual
		override def base :Nothing =
			throw new UnsupportedOperationException("NoParams cannot be used as a FROM clause of a subselect.")

		override type Row = @~
		override type OuterRow = @~

		override def row[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:ChainTuple[E, Single, Row] =
			EmptySQL

		override def outerRow[E <: RowProduct](target :E)(implicit expansion :Implicit ExpandedBy E)
				:ChainTuple[E, Single, OuterRow] =
			EmptySQL

		override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:LazyList[RelationSQL.from[E]#__] =
			LazyList.empty

		override type AsSubselectOf[+F <: NonEmptyRow] = Nothing

		override def asSubselectOf[F <: NonEmptyRow](newOuter :F)(implicit expansion :Implicit ExpandedBy F) :Nothing =
			selectedFrom(newOuter)

		override def withClause :WithClause = WithClause.empty

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = 0

		protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Self])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			SpelledSQL(context)

		override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[@~] = spelling.newContext //SQLContext()

		override def parameterization :Parameterization[@~, Self] = Parameterization.paramless

		protected override def groupingSpellingContext[P](position :Int, context :SQLContext[P],
		                                                  params :Parameterization[P, Generalized])
				:Nothing =
			throw new NoSuchElementException(
				"NoParams does not contain a relation at position " + position +
				". Associated context was " + context + "."
			)

		override def homomorphic(that :RowProduct) :Boolean = that match {
			case other :EmptyFromSome => (this eq other) || (filter homomorphic other.filter)
			case _ => false
		}
		override def isomorphic(that :RowProduct) :Boolean = that match {
			case other :EmptyFromSome => (this eq other) || (filter isomorphic other.filter)
			case _ => false
		}
		override def identical(that :RowProduct) :Boolean = that match {
			case other :EmptyFromSome => (this eq other) || (filter identical other.filter)
			case _ => false
		}
		override def equals(that :Any) :Boolean = that match {
			case other :EmptyFromSome => (this eq other) || filter == other.filter
			case _ => false
		}
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[EmptyFromSome]
		override def hashCode :Int = filter.hashCode

		override def chunkedString :ChunkedString = ChunkedString.empty
		override def typeString(res :StringBuilder) :StringBuilder = res

	}

	private[sql] val EmptyFromSome = new EmptyFromSome(True)




	implicit def lastTableOf[U <: NonEmptyRow, M[O] <: MappingAt[O]]
			:LastTableOf[LastBound[JoinedTable.Of[M]#T, U, M]]
				{ type FromLast = U; type LastMapping[O] = M[O]; type Last[O <: RowProduct] = JoinedTable[O, M] } =
		LastBound.asInstanceOf[LastTableOf[LastBound[JoinedTable.Of[M]#T, U, M]] {
			type FromLast = U; type LastMapping[O] = M[O]; type Last[O <: RowProduct] = JoinedTable[O, M]
		}]
}







/** Common upper bound for all ''from'' clauses containing at least one relation, but no ''group by'' clause.
  * Extended by all [[net.noresttherein.oldsql.sql.FromClause FromClause]] implementations
  * other than [[net.noresttherein.oldsql.sql.Dual Dual]]. Most types do not do this directly however, but
  * through [[net.noresttherein.oldsql.sql.AndFrom AndFrom]], which is the base trait for recursively built
  * clauses by adding a [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] to a prefix
  * `RowProduct`.
  */
trait FromSome extends FromClause with NonEmptyRow with FromSomeTemplate[FromSome] { thisClause =>

	override type FromLast >: Generalized <: FromSome
	override type FromNext[E[+L <: FromSome] <: RowProduct] = E[FromLast]

	override type Generalized >: Complete <: FromSome {
		type LastMapping[O] = thisClause.LastMapping[O]
		type FromLast       = thisClause.FromLast
		type Generalized   <: thisClause.Generalized
		type Explicit      <: thisClause.Explicit
		type Implicit      <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
		type Base          <: thisClause.Base
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: FromSome {
		type LastMapping[O] = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast       = thisClause.FromLast
		type Generalized    = thisClause.Generalized
		type Complete       = thisClause.Complete
		type LastParam      = thisClause.LastParam
		type Params         = thisClause.Params
		type FullRow        = thisClause.FullRow
		type Explicit       = thisClause.Explicit
		type Implicit       = thisClause.Implicit
		type Base           = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row            = thisClause.Row
		type OuterRow       = thisClause.OuterRow
	}

	override type NoAlias >: Self <: FromSome {
		type LastMapping[O] = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast       = thisClause.FromLast
		type Generalized    = thisClause.Generalized
		type Complete       = thisClause.Complete
		type LastParam      = thisClause.LastParam
		type Params         = thisClause.Params
		type FullRow        = thisClause.FullRow
		type Explicit       = thisClause.Explicit
		type Implicit       = thisClause.Implicit
		type Base           = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row            = thisClause.Row
		type OuterRow       = thisClause.OuterRow
	}

	override type Self <: FromSome {
		type LastMapping[O] = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast       = thisClause.FromLast
		type Generalized    = thisClause.Generalized
		type Complete       = thisClause.Complete
		type LastParam      = thisClause.LastParam
		type Params         = thisClause.Params
		type FullRow        = thisClause.FullRow
		type Explicit       = thisClause.Explicit
		type Inner          = thisClause.Inner
		type Implicit       = thisClause.Implicit
		type Base           = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row            = thisClause.Row
		type OuterRow       = thisClause.OuterRow
	}


	override type FilterNext[E[+L <: FromSome] <: RowProduct Expanded T,
	                         S <: RowProduct Expanded T, G <: S, T[O] <: MappingAt[O]] =
		                    (JoinedRelation[E[FromLast], LastMapping], JoinedRelation[S, T]) => SingleBoolean[G]

	protected override def filterNext[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N])
			:next.Copy =
	{
		val condition = filter(last.asIn(next.generalizedExpansion[FromLast]), next.last)
		next.filtered(condition.anchor(next.generalized))
	}

	override type AppliedParam <: FromSome
	override type GeneralizedParamless >: Paramless <: FromSome
	override type Paramless <: BoundParamless
	override type BoundParamless = FromSome { type Params = @~ } //only because JoinParam requires FromSome on the left


	override type JoinWith[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L NonParam R, F <: RowProduct] =
		F#JoinedWith[Self, J]

	override def joinWith[F <: FromSome](suffix :F, join :JoinLike.__) :suffix.JoinedWith[Self, join.LikeJoin] =
		join.likeJoin(self, suffix)


/*
	override type Expand[+J[+L <: Self, R[O] <: T[O]] <: L AndFrom R, T[O] <: MappingAt[O]] = Self J T

	override def expand[T[O] <: BaseMapping[S, O], S]
	                   (next :Table[T], filter :SingleBoolean[Generalized AndFrom T], join :JoinLike.__)
			:join.LikeJoin[Self, T] =
		join.likeJoin[Self, T, S](self, next)(filter)

	protected[sql] def expand[T[O] <: BaseMapping[S, O], S, A <: Label]
	                   (right :LastTable[T, S], alias :Option[A], filter :SingleBoolean[Generalized NonParam T])
			:this.type NonParam T As A =
		InnerJoin[this.type, T, S, A](this, right, alias)(filter)
*/


	/** The upper bound for all `RowProduct` subtypes representing a ''group by'' clause grouping a clause
	  * with the same [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] type as this clause.
	  * This type is additionally extended to include the special [[net.noresttherein.oldsql.sql.Aggregated Aggregated]]
	  * clause, representing ''from'' clauses of SQL ''selects'' with an aggregate function in their ''select'' clause.
	  */
	type GeneralizedAggregate = AggregateClause {
		type GeneralizedDiscrete = thisClause.Generalized
	}

	/** The upper bound for all `RowProduct` subtypes representing a ''group by'' clause grouping this clause. */
	type Aggregate = AggregateClause {
		type GeneralizedDiscrete = thisClause.Generalized
		type Discrete = thisClause.Self
	}

}






object FromSome {

	trait FromSomeTemplate[+F <: FromSome] extends FromClauseTemplate[F] with NonEmptyRowTemplate[F, F] {
		this :F with FromSomeTemplate[F] =>

		/** Creates a `FromClause` of the same type as this one, but with its
		  * [[net.noresttherein.oldsql.sql.RowProduct.filter filter]] being a conjunction of this instance's filter
		  * and the given `SQLBoolean`. The filter becomes the ''where'' clause of an SQL ''select'' created
		  * from the returned instance.
		  * @return [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.filtered filtered]]`(filter)`.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where]]
		  */ //consider: renaming to filter and doing the same trick as with having, or make this an extension method
		override def where(filter :SingleBoolean[Generalized]) :F = and(filter)

		/** Apply a filter condition to this clause. The condition is combined using `&&` with `this.condition`
		  * and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
		  * The function's argument provides access to the mapping of this instance's relations.
		  * Each is parameterized with an [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]]
		  * type `O >: Generalized <: RowProduct` and implicitly convertible to
		  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[O, GlobalScope, M[_]#Subject]`
		  * and thus usable in any `SQLExpression[Generalized, _, _]`, which will make any Boolean expression built
		  * from them conform to the function's required return type, providing no external expression dependent
		  * on other relations are used. It suffices to call any `SQLExpression` method such as one of the comparison
		  * methods like [[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]].
		  * @param condition a function which accepts a
		  *                  [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] instance
		  *                  functioning as a facade to this clause, providing easy access to all its relations,
		  *                  and which returns an SQL expression for the new join/filter condition.
		  * @return a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */ //parameterized with F, not Self, as Self is a bound type unless all relations are aliased.
		override def where(condition :JoinedMappings[F] => SingleBoolean[Generalized]) :F = and(condition)
//			where(condition(this.mappings))

		/** Apply a filter condition to the last relation in this clause. The condition is combined using `&&` with
		  * `this.condition` and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
		  * It is equivalent to `this.and(mappings => condition(mappings.last))`.
		  * @param condition a function accepting the expression for the last relation in this clause and creating
		  *                  an additional SQL expression for the join condition.
		  * @return an `Expanded` instance of the same kind as this one, with the same left and right sides,
		  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
		  *         returned by the passed filter function.
		  */
		//consider: simply JoinedRelation as the function argument - may result in better type inference
		override def whereLast(condition :Last[FromLast] => SingleBoolean[FromLast]) :F =
			andLast(condition)
	}



	/** Extension methods for `FromSome` classes (non-empty ''from'' clauses) which benefit from having a static,
	  * invariant self type. These include methods for joining with other relations and clauses as well as
	  * select methods creating SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]] using
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions.
	  */
	implicit class FromSomeExtension[F <: FromSome](val thisClause :F) /*extends AnyVal*/ {
		import thisClause.{Base, Complete, FromLast, Generalized, Last, LastMapping, Self}

		/** Performs an inner join between this clause on the left side, and the table given as a `Table`
		  * object on the right side.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def join[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                (table :Table[R])(implicit cast :BaseMappingSubject[R, T, S]) :F InnerJoin R =
			InnerJoin(thisClause, table)

		/** Performs an inner join between this clause on the left side, and the table given as a `Table`
		  * object on the right side. The `String` literal with the name of the table taken from the table's type
		  * is used in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause, immediately added
		  * to the joined table. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def join[N <: Label, R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                (table :StaticTable[N, R])(implicit cast :BaseMappingSubject[R, T, S]) :F InnerJoin R As N =
			InnerJoin(thisClause, table)

		//todo: joining tables from an explicit with clause
//		def join[N <: Label](name :N)(implicit get :GetCTE.ByAlias[F, thisClause.Generalized N]) :F InnerJoin get.M As N =

		/** Performs an inner join between this clause on the left side, and a table referenced by a foreign key
		  * from one of its tables on the right side. The foreign key mapping returned by the passed function
		  * can represent both a true foreign key (linking 0-1 rows from the referenced table) and a foreign key inverse:
		  * a key in the last table (typically its primary key) referenced by a foreign key in the joined table.
		  * Returned join will already include a join condition matching the two keys from its last two tables.
		  * @param table  a function selecting a foreign key component from a mapping for a table joined in this clause.
		  * @param offset an implicit witness carrying the negative offset of the table with the reference component
		  *               selected by the function argument.
		  * @param cast   an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */ //todo: extension methods for BrokeredRelationshipMapping
		def join[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S, O >: Generalized <: FromSome]
		        (table :JoinedMappings[F] => DirectRelationshipMapping[R, _, O])
		        (implicit offset :RelationCount[O, _ <: Numeral], cast :BaseMappingSubject[R, T, S])
				:F InnerJoin R =
		{
			val key      = table(thisClause.mappings)
			val narrowed = InnerJoin[thisClause.type, Generalized, R, T, S](thisClause, key.table)
			val local    = key.key.toSQL[O, key.Key].expand(narrowed.generalized :O Join R)
			val remote   = key.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, key.Key]
			narrowed where local === remote
		}

		/** Performs an inner join between this clause on the left side, and a table referenced by a foreign key
		  * from its last table on the right side. The foreign key mapping returned by the passed function
		  * can represent both a true foreign key (linking 0-1 rows from the referenced table) and a foreign key inverse:
		  * a key in the last table (typically its primary key) referenced by a foreign key in the joined table.
		  * Returned join will already include a join condition matching the two keys from its last two tables.
		  * @param table a function selecting a foreign key component from the mapping of the last table
		  *              in this clause.
		  * @param cast  an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def joinLast[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		            (table :LastMapping[FromLast] => DirectRelationshipMapping[R, _, FromLast])
		            (implicit cast :BaseMappingSubject[R, T, S]) :F InnerJoin R =
		{
			val key = table(thisClause.last.relation.row[FromLast])
			val join = InnerJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(key.table))
			val local = (thisClause.last \ key.key).expand(join.generalized :FromLast Join T) //:GlobalSQL[FromLast Join T, key.Key]
			val remote = join.last \ key.target.withOrigin[RowProduct AndFrom T]
			cast.back.join(join where local === remote)
		}

		/** Performs an inner join between this clause and another table using a join table defined by a relationship
		  * component of one of the tables on the left side, adding two tables to this clause. The returned clause
		  * already includes a filtering condition for both foreign keys in the join table.
		  * @param table  a function selecting a ''many-to-many'' relationship component from a mapping joined in this clause.
		  * @param offset an implicit witness carrying the negative offset of the table with the reference component
		  *               selected by the function argument.
		  * @param cast1  an implicit witness helping with type inference of the subject type of the mapping type `J`.
		  * @param cast2  an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def joinBy[J[A] <: MappingAt[A], M[A] <: BaseMapping[E, A], E,
		           R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S, O >: Generalized <: FromSome]
		          (table :JoinedMappings[F] => BrokeredRelationshipMapping[J, R, S, O])
		          (implicit offset :RelationCount[O, _ <: Numeral],
		                    cast1 :BaseMappingSubject[J, M, E], cast2 :BaseMappingSubject[R, T, S])
				:F InnerJoin J InnerJoin R =
		{
			val rel     = table(thisClause.mappings)
			val first   = InnerJoin[thisClause.type, Generalized, J, M, E](thisClause, rel.joinTable)
			val local   = rel.key.toSQL[O, rel.Key].expand(first.generalized :O Join J)
			val back    = rel.firstKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.Key]
			val firstOn = first where local === back
			val second  = InnerJoin[firstOn.type, firstOn.Generalized, R, T, S](firstOn, rel.table)
			val forward = rel.secondKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.TargetKey]
			                           .expand(second.generalized :O Join J Join R)
			val remote  = rel.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, rel.TargetKey]
			second where forward === remote
		}

		/** Performs an inner join between the last table in this clause and another table, using a join table
		  * defined by a relationship component of the last table on the left side, adding two tables to this clause.
		  * The returned clause already includes a filtering condition for both foreign keys in the join table.
		  * @param table  a function selecting a ''many-to-many'' relationship component from a mapping
		  *               of the last table in this clause.
		  * @param cast1  an implicit witness helping with type inference of the subject type of the mapping type `J`.
		  * @param cast2  an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def joinLastBy[J[A] <: MappingAt[A], M[A] <: BaseMapping[E, A], E,
		               R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		              (table :LastMapping[FromLast] => BrokeredRelationshipMapping[J, R, S, FromLast])
		              (implicit cast1 :BaseMappingSubject[J, M, E], cast2 :BaseMappingSubject[R, T, S])
				:F InnerJoin J InnerJoin R =
		{
			val rel     = table(thisClause.last.relation[FromLast])
			val first   = InnerJoin[thisClause.type, Generalized, J, M, E](thisClause, rel.joinTable)
			val local   = (thisClause.last \ rel.key).expand(first.generalized :FromLast Join J)
			val back    = (first.last \ rel.firstKey.withOrigin[RowProduct AndFrom J])
			val firstOn = first where local === back
			val second  = InnerJoin[firstOn.type, firstOn.Generalized, R, T, S](firstOn, rel.table)
			val forward = rel.secondKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.TargetKey]
			                           .expand(second.generalized :FromLast Join J Join R)
			val remote  = rel.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, rel.TargetKey]
			second where forward === remote
		}

		/** Performs an inner join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def join[R <: RowProduct](other :R) :other.JoinedWith[F, InnerJoin] =
			other.joinedWith(thisClause, InnerJoin.template)


		/** Performs a symmetric outer join between this clause on the left side, and the table given as a `Table`
		  * object on the right side.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def outerJoin[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                     (table :Table[R])(implicit cast :BaseMappingSubject[R, T, S]) :F OuterJoin R =
			OuterJoin(thisClause, table)

		/** Performs a symmetric outer join between this clause on the left side, and the table given as a `Table`
		  * object on the right side. The `String` literal with the name of the table taken from the table's type
		  * is used in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause, immediately added
		  * to the joined table. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def outerJoin[N <: Label, R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                     (table :StaticTable[N, R])(implicit cast :BaseMappingSubject[R, T, S]) :F OuterJoin R As N =
			OuterJoin(thisClause, table)

		/** Performs a symmetric outer join between this clause on the left side, and a table referenced
		  * by a foreign key from one of its tables on the right side. The foreign key mapping returned
		  * by the passed function can represent both a true foreign key (linking 0-1 rows from the referenced table)
		  * and a foreign key inverse: a key in the last table (typically its primary key) referenced by a foreign key
		  * in the joined table. Returned join will already include a join condition matching the two keys
		  * from its last two tables.
		  * @param table a function selecting a foreign key component from a mapping for a table joined in this clause.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param offset an implicit witness carrying the negative offset of the table with the reference component
		  *               selected by the function argument.
		  */
		def outerJoin[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S, K, E >: Generalized <: FromSome]
		             (table :JoinedMappings[F] => DirectRelationshipMapping[R, _, E])
		             (implicit offset :RelationCount[E, _ <: Numeral], cast :BaseMappingSubject[R, T, S]) :F OuterJoin R =
		{
			val key      = table(thisClause.mappings)
			val narrowed = OuterJoin[thisClause.type, Generalized, R, T, S](thisClause, key.table)
			val local    = key.key.toSQL[E, key.Key].expand(narrowed.generalized :E Join R)
			val remote   = key.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, key.Key]
			narrowed where local === remote
		}

		/** Performs an outer join between this clause on the left side, and a table referenced by a foreign key
		  * from its last table on the right side. The foreign key mapping returned by the passed function
		  * can represent both a true foreign key (linking 0-1 rows from the referenced table) and a foreign key inverse:
		  * a key in the last table (typically its primary key) referenced by a foreign key in the joined table.
		  * Returned join will already include a join condition matching the two keys from its last two tables.
		  * @param foreignKey a function selecting a foreign key component from the mapping of the last table
		  *                   in this clause.
		  * @param cast       an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def outerJoinLast[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                 (foreignKey :LastMapping[FromLast] => DirectRelationshipMapping[R, _, FromLast])
		                 (implicit cast :BaseMappingSubject[R, T, S]) :F OuterJoin R =
		{
			val key = foreignKey(thisClause.last.relation.row[FromLast])
			val join = OuterJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(key.table))
			val local = (thisClause.last \ key.key).expand(join.generalized :FromLast Join T)
			val remote = join.last \ key.target.withOrigin[RowProduct AndFrom T]
			cast.back.join(join where local === remote)
		}

		/** Performs a left join between this clause and a join table, followed by a right join with the table
		  * referenced by the second key in the join table. The join table is defined by a relationship
		  * component of one of the tables on the left side. This method departs from the template of other
		  * outer join methods, as it does not include an [[net.noresttherein.oldsql.sql.OuterJoin OuterJoin]] at all.
		  * The spirit of an outer join is however preserved in that all rows from both this clause, and the target
		  * table will be present in the result.
		  * The returned clause already includes a filtering condition for both foreign keys in the join table.
		  * @param table  a function selecting a ''many-to-many'' relationship component from a mapping joined in this clause.
		  * @param offset an implicit witness carrying the negative offset of the table with the reference component
		  *               selected by the function argument.
		  * @param cast1  an implicit witness helping with type inference of the subject type of the mapping type `J`.
		  * @param cast2  an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def outerJoinBy[J[A] <: MappingAt[A], M[A] <: BaseMapping[E, A], E,
		                R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S, O >: Generalized <: FromSome]
		               (table :JoinedMappings[F] => BrokeredRelationshipMapping[J, R, S, O])
		               (implicit offset :RelationCount[O, _ <: Numeral],
		                         cast1 :BaseMappingSubject[J, M, E], cast2 :BaseMappingSubject[R, T, S])
				:F LeftJoin J RightJoin R =
		{
			val rel     = table(thisClause.mappings)
			val first   = LeftJoin[thisClause.type, Generalized, J, M, E](thisClause, rel.joinTable)
			val local   = rel.key.toSQL[O, rel.Key].expand(first.generalized :O Join J)
			val back    = rel.firstKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.Key]
			val firstOn = first where local === back
			val second  = RightJoin[firstOn.type, firstOn.Generalized, R, T, S](firstOn, rel.table)
			val forward = rel.secondKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.TargetKey]
			                           .expand(second.generalized :O Join J Join R)
			val remote  = rel.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, rel.TargetKey]
			second where forward === remote
		}

		/** Performs an inner join between the last table in this clause and a join table, followed by a right join
		  * with the table referenced by the second key in the join table. The join table is defined by a relationship
		  * component of one of the tables on the left side. This method departs from the template of other
		  * outer join methods, as it does not include an [[net.noresttherein.oldsql.sql.OuterJoin OuterJoin]] at all.
		  * The spirit of an outer join is however preserved in that all rows from both this clause, and the target
		  * table will be present in the result.
		  * The returned clause already includes a filtering condition for both foreign keys in the join table.
		  * @param table  a function selecting a ''many-to-many'' relationship component from a mapping
		  *               of the last table in this clause.
		  * @param cast1  an implicit witness helping with type inference of the subject type of the mapping type `J`.
		  * @param cast2  an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def outerJoinLastBy[J[A] <: MappingAt[A], M[A] <: BaseMapping[E, A], E,
		                    R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                   (table :LastMapping[FromLast] => BrokeredRelationshipMapping[J, R, S, FromLast])
		                   (implicit cast1 :BaseMappingSubject[J, M, E], cast2 :BaseMappingSubject[R, T, S])
				:F LeftJoin J RightJoin R =
		{
			val rel     = table(thisClause.last.relation[FromLast])
			val first   = LeftJoin[thisClause.type, Generalized, J, M, E](thisClause, rel.joinTable)
			val local   = (thisClause.last \ rel.key).expand(first.generalized :FromLast Join J)
			val back    = (first.last \ rel.firstKey.withOrigin[RowProduct AndFrom J])
			val firstOn = first where local === back
			val second  = RightJoin[firstOn.type, firstOn.Generalized, R, T, S](firstOn, rel.table)
			val forward = rel.secondKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.TargetKey]
			                           .expand(second.generalized :FromLast Join J Join R)
			val remote  = rel.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, rel.TargetKey]
			second where forward === remote
		}

		/** Performs a symmetric outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def outerJoin[R <: RowProduct](other :R) :other.JoinedWith[F, OuterJoin] =
			other.joinedWith(thisClause, OuterJoin.template)


		/** Performs a left outer join between this clause on the left side, and the table given as a `Table`
		  * object on the right side.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def leftJoin[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                    (table :Table[R])(implicit cast :BaseMappingSubject[R, T, S]) :F LeftJoin R =
			LeftJoin(thisClause, table)

		/** Performs a left outer join between this clause on the left side, and the table given as a `Table`
		  * object on the right side.The `String` literal with the name of the table taken from the table's type
		  * is used in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause, immediately added
		  * to the joined table. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def leftJoin[N <: Label, R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                    (table :StaticTable[N, R])(implicit cast :BaseMappingSubject[R, T, S]) :F LeftJoin R As N =
			LeftJoin(thisClause, table)

		/** Performs a left outer join between this clause on the left side, and a table referenced
		  * by a foreign key from one of its tables on the right side. The foreign key mapping returned
		  * by the passed function can represent both a true foreign key (linking 0-1 rows from the referenced table)
		  * and a foreign key inverse: a key in the last table (typically its primary key) referenced by a foreign key
		  * in the joined table. Returned join will already include a join condition matching the two keys
		  * from its last two tables.
		  * @param table a function selecting a foreign key component from a mapping for a table joined in this clause.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param offset an implicit witness carrying the negative offset of the table with the reference component
		  *               selected by the function argument.
		  */
		def leftJoin[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S, K, E >: Generalized <: FromSome]
		            (table :JoinedMappings[F] => DirectRelationshipMapping[R, _, E])
		            (implicit cast :BaseMappingSubject[R, T, S], offset :RelationCount[E, _ <: Numeral])
				:F LeftJoin R =
		{
			val key = table(thisClause.mappings)
			val join = LeftJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(key.table))
			val local = key.key.toSQL[E, key.Key].expand(join.generalized :E Join T)
			val remote = key.target.withOrigin[FromSome Join T].toSQL[FromSome Join T, key.Key]
			cast.back.join(join where local === remote)
		}

		/** Performs a left outer join between this clause on the left side, and a table referenced by a foreign key
		  * from its last table on the right side. The foreign key mapping returned by the passed function
		  * can represent both a true foreign key (linking 0-1 rows from the referenced table) and a foreign key inverse:
		  * a key in the last table (typically its primary key) referenced by a foreign key in the joined table.
		  * Returned join will already include a join condition matching the two keys from its last two tables.
		  * @param foreignKey a function selecting a foreign key component from the mapping of the last table
		  *                   in this clause.
		  * @param cast       an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def leftJoinLast[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                (foreignKey :LastMapping[FromLast] => DirectRelationshipMapping[R, _, FromLast])
		                (implicit cast :BaseMappingSubject[R, T, S]) :F LeftJoin R =
		{
			val key = foreignKey(thisClause.last.relation.row[FromLast])
			val join = LeftJoin[thisClause.type, Generalized, R, T, S](thisClause, key.table)
			val local = (thisClause.last \ key.key).expand(join.generalized :FromLast Join R)
			val remote = join.last \ key.target.withOrigin[RowProduct AndFrom R]
			join where local === remote
		}

		/** Performs a left outer join between this clause and another table using a join table defined
		  * by a relationship component of one of the tables on the left side, adding two tables to this clause.
		  * This method assumes that for every row in the join table, there is a row in the target table referenced
		  * by its second foreign key.
		  * The returned clause already includes a filtering condition for both foreign keys in the join table.
		  * @param table  a function selecting a ''many-to-many'' relationship component from a mapping joined in this clause.
		  * @param offset an implicit witness carrying the negative offset of the table with the reference component
		  *               selected by the function argument.
		  * @param cast1  an implicit witness helping with type inference of the subject type of the mapping type `J`.
		  * @param cast2  an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def leftJoinBy[J[A] <: MappingAt[A], M[A] <: BaseMapping[E, A], E,
		               R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S, O >: Generalized <: FromSome]
		              (table :JoinedMappings[F] => BrokeredRelationshipMapping[J, R, S, O])
		              (implicit offset :RelationCount[O, _ <: Numeral],
		                        cast1 :BaseMappingSubject[J, M, E], cast2 :BaseMappingSubject[R, T, S])
				:F LeftJoin J InnerJoin R =
		{
			val rel     = table(thisClause.mappings)
			val first   = LeftJoin[thisClause.type, Generalized, J, M, E](thisClause, rel.joinTable)
			val local   = rel.key.toSQL[O, rel.Key].expand(first.generalized :O Join J)
			val back    = rel.firstKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.Key]
			val firstOn = first where local === back
			val second  = InnerJoin[firstOn.type, firstOn.Generalized, R, T, S](firstOn, rel.table)
			val forward = rel.secondKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.TargetKey]
			                           .expand(second.generalized :O Join J Join R)
			val remote  = rel.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, rel.TargetKey]
			second where forward === remote
		}

		/** Performs a left outer join between the last table in this clause and another table, using a join table
		  * defined by a relationship component of the last table on the left side, adding two tables to this clause.
		  * This method assumes that for every row in the join table, there is a row in the target table referenced
		  * by its second foreign key.
		  * The returned clause already includes a filtering condition for both foreign keys in the join table.
		  * @param table  a function selecting a ''many-to-many'' relationship component from a mapping
		  *               of the last table in this clause.
		  * @param cast1  an implicit witness helping with type inference of the subject type of the mapping type `J`.
		  * @param cast2  an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def leftJoinLastBy[J[A] <: MappingAt[A], M[A] <: BaseMapping[E, A], E,
		                   R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                  (table :LastMapping[FromLast] => BrokeredRelationshipMapping[J, R, S, FromLast])
		                  (implicit cast1 :BaseMappingSubject[J, M, E], cast2 :BaseMappingSubject[R, T, S])
				:F LeftJoin J InnerJoin R =
		{
			val rel     = table(thisClause.last.relation[FromLast])
			val first   = LeftJoin[thisClause.type, Generalized, J, M, E](thisClause, rel.joinTable)
			val local   = (thisClause.last \ rel.key).expand(first.generalized :FromLast Join J)
			val back    = (first.last \ rel.firstKey.withOrigin[RowProduct AndFrom J])
			val firstOn = first where local === back
			val second  = InnerJoin[firstOn.type, firstOn.Generalized, R, T, S](firstOn, rel.table)
			val forward = rel.secondKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.TargetKey]
			                           .expand(second.generalized :FromLast Join J Join R)
			val remote  = rel.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, rel.TargetKey]
			second where forward === remote
		}

		/** Performs a left outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def leftJoin[R <: RowProduct](other :R) :other.JoinedWith[F, LeftJoin] =
			other.joinedWith(thisClause, LeftJoin.template)


		/** Performs a right outer join between this clause on the left side, and the table given as a `Table`
		  * object on the right side.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def rightJoin[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                     (table :Table[R])(implicit cast :BaseMappingSubject[R, T, S]) :F RightJoin R =
			RightJoin(thisClause, table)

		/** Performs a right outer join between this clause on the left side, and the table given as a `Table`
		  * object on the right side.The `String` literal with the name of the table taken from the table's type
		  * is used in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause, immediately added
		  * to the joined table. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def rightJoin[N <: Label, R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                     (table :StaticTable[N, R])(implicit cast :BaseMappingSubject[R, T, S]) :F RightJoin R As N =
			RightJoin(thisClause, table)

		/** Performs a right outer join between this clause on the left side, and a table referenced
		  * by a foreign key from one of its tables on the right side. The foreign key mapping returned
		  * by the passed function can represent both a true foreign key (linking 0-1 rows from the referenced table)
		  * and a foreign key inverse: a key in the last table (typically its primary key) referenced by a foreign key
		  * in the joined table. Returned join will already include a join condition matching the two keys
		  * from its last two tables.
		  * @param table a function selecting a foreign key component from a mapping for a table joined in this clause.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param offset an implicit witness carrying the negative offset of the table with the reference component
		  *               selected by the function argument.
		  */
		def rightJoin[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S, K, E >: Generalized <: FromSome]
		             (table :JoinedMappings[F] => DirectRelationshipMapping[R, _, E])
		             (implicit offset :RelationCount[E, _ <: Numeral], cast :BaseMappingSubject[R, T, S])
				:F RightJoin R =
		{
			val key      = table(thisClause.mappings)
			val narrowed = RightJoin[thisClause.type, Generalized, R, T, S](thisClause, key.table)
			val local    = key.key.toSQL[E, key.Key].expand(narrowed.generalized :E Join R)
			val remote   = key.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, key.Key]
			narrowed where local === remote
		}

		/** Performs a right outer join between this clause on the left side, and a table referenced by a foreign key
		  * from its last table on the right side. The foreign key mapping returned by the passed function
		  * can represent both a true foreign key (linking 0-1 rows from the referenced table) and a foreign key inverse:
		  * a key in the last table (typically its primary key) referenced by a foreign key in the joined table.
		  * Returned join will already include a join condition matching the two keys from its last two tables.
		  * @param foreignKey a function selecting a foreign key component from the mapping of the last table
		  *                   in this clause.
		  * @param cast       an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def rightJoinLast[R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                 (foreignKey :LastMapping[FromLast] => DirectRelationshipMapping[R, _, FromLast])
		                 (implicit cast :BaseMappingSubject[R, T, S]) :F RightJoin R =
		{
			val key = foreignKey(thisClause.last.relation.row[FromLast])
			val join = RightJoin[thisClause.type, Generalized, R, T, S](thisClause, key.table)
			val local = (thisClause.last \ key.key).expand(join.generalized :FromLast Join R)
			val remote = join.last \ key.target.withOrigin[RowProduct AndFrom R]
			join where local === remote
		}

		/** Performs a right outer join between this clause and another table using a join table defined
		  * by a relationship component of one of the tables on the left side, adding two tables to this clause.
		  * This method assumes that for every row in the join table, there is exactly one row in this clause
		  * (before filtering by its ''where'' clause), referenced by the first foreign key in the join table.
		  * The returned clause already includes a filtering condition for both foreign keys in the join table.
		  * @param table  a function selecting a ''many-to-many'' relationship component from a mapping joined in this clause.
		  * @param offset an implicit witness carrying the negative offset of the table with the reference component
		  *               selected by the function argument.
		  * @param cast1  an implicit witness helping with type inference of the subject type of the mapping type `J`.
		  * @param cast2  an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def rightJoinBy[J[A] <: MappingAt[A], M[A] <: BaseMapping[E, A], E,
		                R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S, O >: Generalized <: FromSome]
		               (table :JoinedMappings[F] => BrokeredRelationshipMapping[J, R, S, O])
		               (implicit offset :RelationCount[O, _ <: Numeral],
		                         cast1 :BaseMappingSubject[J, M, E], cast2 :BaseMappingSubject[R, T, S])
				:F InnerJoin J RightJoin R =
		{
			val rel     = table(thisClause.mappings)
			val first   = InnerJoin[thisClause.type, Generalized, J, M, E](thisClause, rel.joinTable)
			val local   = rel.key.toSQL[O, rel.Key].expand(first.generalized :O Join J)
			val back    = rel.firstKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.Key]
			val firstOn = first where local === back
			val second  = RightJoin[firstOn.type, firstOn.Generalized, R, T, S](firstOn, rel.table)
			val forward = rel.secondKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.TargetKey]
			                           .expand(second.generalized :O Join J Join R)
			val remote  = rel.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, rel.TargetKey]
			second where forward === remote
		}

		/** Performs a right outer join between the last table in this clause and another table, using a join table
		  * defined by a relationship component of the last table on the left side, adding two tables to this clause.
		  * This method assumes that for every row in the join table, there is exactly one row in the last table
		  * of this clause (before filtering by its ''where'' clause), referenced by the first foreign key
		  * in the join table.
		  * The returned clause already includes a filtering condition for both foreign keys in the join table.
		  * @param table  a function selecting a ''many-to-many'' relationship component from a mapping
		  *               of the last table in this clause.
		  * @param cast1  an implicit witness helping with type inference of the subject type of the mapping type `J`.
		  * @param cast2  an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		def rightJoinLastBy[J[A] <: MappingAt[A], M[A] <: BaseMapping[E, A], E,
		                    R[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		                   (table :LastMapping[FromLast] => BrokeredRelationshipMapping[J, R, S, FromLast])
		                   (implicit cast1 :BaseMappingSubject[J, M, E], cast2 :BaseMappingSubject[R, T, S])
				:F InnerJoin J RightJoin R =
		{
			val rel     = table(thisClause.last.relation[FromLast])
			val first   = InnerJoin[thisClause.type, Generalized, J, M, E](thisClause, rel.joinTable)
			val local   = (thisClause.last \ rel.key).expand(first.generalized :FromLast Join J)
			val back    = (first.last \ rel.firstKey.withOrigin[RowProduct AndFrom J])
			val firstOn = first where local === back
			val second  = RightJoin[firstOn.type, firstOn.Generalized, R, T, S](firstOn, rel.table)
			val forward = rel.secondKey.withOrigin[FromSome Join J].toSQL[FromSome Join J, rel.TargetKey]
			                           .expand(second.generalized :FromLast Join J Join R)
			val remote  = rel.target.withOrigin[FromSome Join R].toSQL[FromSome Join R, rel.TargetKey]
			second where forward === remote
		}

		/** Performs a right outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def rightJoin[R <: RowProduct](other :R) :other.JoinedWith[F, RightJoin] =
			other.joinedWith(thisClause, RightJoin.template)



		/** Performs a natural inner join between this clause on the left side, and the table given as a `Table`
		  * object on the right side. The join condition of the created instance will compare all columns of the last
		  * relation in this clause with columns with matching names from the given relation. If the column types
		  * (associated `ColumnForm` objects) of any of these column pairs differ, an `IllegalArgumentException`
		  * is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                       (table :Table[R])
		                       (implicit last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome },
		                                 cast :BaseMappingSubject[R, T, S])
				:F InnerJoin R =
			cast.back(InnerJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)

		/** Performs a natural inner join between this clause on the left side, and the table given as a `Table`
		  * object on the right side. The `String` literal with the name of the table taken from the table's type
		  * is used in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause, immediately added
		  * to the joined table. The join condition of the created instance will compare all columns of the last
		  * relation in this clause with columns with matching names from the given relation. If the column types
		  * (associated `ColumnForm` objects) of any of these column pairs differ, an `IllegalArgumentException`
		  * is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalJoin[N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                       (table :StaticTable[N, R])
		                       (implicit last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome },
		                                 cast :BaseMappingSubject[R, T, S])
				:F InnerJoin R As N =
			cast.back(InnerJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)
				.as(table.name)


		/** Performs a natural symmetric outer join between this clause on the left side, and the table given
		  * as a `Table` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalOuterJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :Table[R])
		                            (implicit last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome },
		                                      cast :BaseMappingSubject[R, T, S])
				:F OuterJoin R =
			cast.back(OuterJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)

		/** Performs a natural symmetric outer join between this clause on the left side, and the table given
		  * as a `Table` object on the right side. The `String` literal with the name of the table taken from
		  * the table's type is used in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause, immediately added
		  * to the joined table. The join condition of the created instance will compare all columns of the last
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalOuterJoin[N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :StaticTable[N, R])
		                            (implicit last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome },
		                                      cast :BaseMappingSubject[R, T, S])
				:F OuterJoin R As N =
			cast.back(OuterJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)
				.as(table.name)


		/** Performs a natural left outer join between this clause on the left side, and the table given
		  * as a `Table` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalLeftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                           (table :Table[R])
		                           (implicit last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome },
		                                     cast :BaseMappingSubject[R, T, S])
				:F LeftJoin R =
			cast.back(LeftJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)

		/** Performs a natural left outer join between this clause on the left side, and the table given
		  * as a `Table` object on the right side. The `String` literal with the name of the table taken from
		  * the table's type is used in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause, immediately added
		  * to the joined table. The join condition of the created instance will compare all columns of the last
		  * relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalLeftJoin[N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                           (table :StaticTable[N, R])
		                           (implicit last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome },
		                                     cast :BaseMappingSubject[R, T, S])
				:F LeftJoin R As N =
			cast.back(LeftJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)
				.as(table.name)


		/** Performs a natural right outer join between this clause on the left side, and the table given
		  * as a `Table` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalRightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :Table[R])
		                            (implicit last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome },
		                                      cast :BaseMappingSubject[R, T, S])
				:F RightJoin R =
			cast.back(RightJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)

		/** Performs a natural right outer join between this clause on the left side, and the table given
		  * as a `Table` object on the right side.The `String` literal with the name of the table taken from
		  * the table's type is used in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause, immediately added
		  * to the joined table. The join condition of the created instance will compare all columns of the last
		  * relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalRightJoin[N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :StaticTable[N, R])
		                            (implicit last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome },
		                                      cast :BaseMappingSubject[R, T, S])
				:F RightJoin R As N =
			cast.back(RightJoin[thisClause.type, Generalized, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)
				.as(table.name)


		private def naturalFilter[T[O] <: BaseMapping[_, O]]
		                         (tables :JoinedMappings[thisClause.type Join T])
		                         (implicit prev :ByIndex[F Join T, Generalized Join T, -2])
				:SingleBoolean[Generalized Join T] =
		{
			val firstTable = tables.prev
			val secondTable = tables.last

			val firstColumns = firstTable.columns.map(c => c.name -> c).toMap //todo: below - nondeterministic compilation
			val secondColumns = secondTable.columns.map(c => c.name -> (c :TypedColumn[_, RowProduct AndFrom T])).toMap
			val common = firstColumns.keySet & secondColumns.keySet

			val joins = common map { name =>

				val first = firstColumns(name).asInstanceOf[TypedColumn[Any, prev.O]]
				val second = secondColumns(name).asInstanceOf[TypedColumn[Any, RowProduct AndFrom T]]
				if (first.form != second.form)
					throw new IllegalArgumentException(
						s"Can't perform a natural join of $firstTable and $secondTable: " +
						s"columns $first and $second have different types (forms): ${first.form} and ${second.form}."
					)
				//todo: why explicit conversions are necessary here?
				LooseColumn(first, 1).toColumnSQL === LooseColumn(second, 0).toColumnSQL
			}
			(True[Generalized Join T] /: joins)(_ && _)
		}



		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Table` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Table[R])(implicit cast :BaseMappingSubject[R, T, S]) :F Subselect R =
			Subselect(thisClause, table)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The `String` literal type with name of the table, taken from the argument's type, is used for
		  * the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause added to the joined table.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Table` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :StaticTable[N, R])
		                     (implicit cast :BaseMappingSubject[R, T, S]) :F Subselect R As N =
			Subselect(thisClause, table)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `RowProduct`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] methods.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order). The clause cannot be empty to enforce that the `Subselect` join
		  *              is actually applied and that any relations joined later will be part of the new subselect
		  *              rather than the currently most deeply nested select.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  * @throws UnsupportedOperationException if `other` is empty or its first join is a `JoinParam`.
		  */
		@inline def subselect[R <: NonEmptyRow](other :R) :other.SelectedFrom[F] =
			other.selectedFrom(thisClause)



		/** Adds a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause to this ''from'' clause.
		  * @tparam E an expression used for grouping, for which
		  *           a [[net.noresttherein.oldsql.sql.mechanics.CanGroup CanGroup]]
		  *           type class exist. The [[net.noresttherein.oldsql.sql.mechanics.CanGroup$ companion]]
		  *           object contains definitions for:
		  *             - `M <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[_, O]`, having an implicit
		  *               [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] (which exists
		  *               for all subtypes of `BaseMapping` taking the `Origin` type as its last type parameter),
		  *             - components of relations:
		  *               [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[F, _]` and
		  *               [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL ColumnMappingSQL]]`[F, _]`,
		  *             - any single column expressions [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, _]`,
		  *             - base [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, _]`,
		  *           where type `F` is this clause, and `O` is its some supertype, with the origin relation
		  *           of the component expression being the first relation following a wildcard type (typically `FromSome`).
		  * @param expr a function accepting the facade to the this clause
		  *             [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], which provides
		  *             accessors to the mappings for all relations in this clause, and which returns
		  *             either a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] with a supertype
		  *             of this clause as its `Origin` type argument,
		  *             or an [[net.noresttherein.oldsql.sql.SQLExpression SQL expression]] based on this clause which
		  *             will be used as the grouping expression. The expression may be
		  *             a [[net.noresttherein.oldsql.sql.ColumnSQL single column]], but it doesn't have to,
		  *             in which case all columns of the expression will be inlined in the ''group by'' clause
		  *             in the order defined by its [[net.noresttherein.oldsql.schema.SQLReadForm form]].
		  *             If the returned value is a a mapping `M[O] <: MappingAt[O]` or
		  *             a [[net.noresttherein.oldsql.sql.ast.ComponentSQL component expression]]
		  *             for such a mapping, then the return type of the method will be
		  *             `G `[[net.noresttherein.oldsql.sql.By By]]` M`, allowing selecting of any
		  *             of its components/columns, just as with components of tables joined using
		  *             the [[net.noresttherein.oldsql.sql.Join Join]] classes (and through the same
		  *             mechanisms).
		  *             Note that the argument, `JoinedMappings[F]`, is parameterised not with this ''group by''
		  *             clause, but the discrete ''from'' clause underneath it.
		  * @param grouping a type class responsible for creating the returned ''group by'' clause, which defines
		  *                 the return type based on the type of the expression returned by the function passed
		  *                 as the first argument. See the `returns` section for a listing.
		  * @return a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] instance using this clause as its left side.
		  *         The mapping type on the right side will be the mapping for the expression `E` returned
		  *         by the passed function: if it is a
		  *         [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]], it is used directly after anchoring
		  *         to the relation based on its `Origin` type. In case of
		  *         [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] (including its column
		  *         subtype), the mapping is the mapping type parameter of the component expression.
		  *         Otherwise a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]
		  *         (or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] if `E` is
		  *         a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]) is used. In the latter case, the return type
		  *         is abbreviated as `F `[[net.noresttherein.oldsql.sql.GroupByVal GroupByVal]]` V`
		  *         (or `F `[[net.noresttherein.oldsql.sql.GroupByOne GroupByOne]]` V`), where `V` is the value type
		  *         of the expression `E`.
		  */
		def groupBy[E](expr :JoinedMappings[F] => E)(implicit grouping :CanGroup[Generalized, F, E]) :grouping.Result =
			grouping(thisClause, expr(thisClause.mappings))

		/** Adds a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause to this ''from'' clause.
		  * The grouping expression is based solely on the last relation in this clause, but
		  * @tparam E an expression used for grouping, for which
		  *           a [[net.noresttherein.oldsql.sql.mechanics.CanGroup CanGroup]] type class exist.
		  *           The [[net.noresttherein.oldsql.sql.mechanics.CanGroup$ companion]] object contains definitions for:
		  *             - `M <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[_, O]`, having an implicit
		  *               [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] (which exists
		  *               for all subtypes of `BaseMapping` taking the `Origin` type as its last type parameter),
		  *             - components of relations:
		  *               [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[F, _]` and
		  *               [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL ColumnMappingSQL]]`[F, _]`,
		  *             - any single column expressions [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, _]`,
		  *             - base [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, _]`,
		  *           where type `F` is this clause, and `O` is its some supertype, with the origin relation
		  *           of the component expression being the first relation following a wildcard type (typically `FromSome`).
		  * @param expr     a function accepting the last [[net.noresttherein.oldsql.sql.ast.JoinedRelation relation]]
		  *                 of the this clause, and which returns either
		  *                 a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] with a supertype of this clause
		  *                 as its `Origin` type argument, or an [[net.noresttherein.oldsql.sql.SQLExpression SQL expression]]
		  *                 based on this clause which will be used as the grouping expression. The expression may be
		  *                 a [[net.noresttherein.oldsql.sql.ColumnSQL single column]], but it doesn't have to,
		  *                 in which case all columns of the expression will be inlined in the ''group by'' clause
		  *                 in the order defined by its [[net.noresttherein.oldsql.schema.SQLReadForm form]].
		  *                 If the returned value is a a mapping `M[O] <: MappingAt[O]` or
		  *                 a [[net.noresttherein.oldsql.sql.ast.ComponentSQL component expression]]
		  *                 for such a mapping, then the return type of the method will be
		  *                 `F `[[net.noresttherein.oldsql.sql.GroupBy GroupBy]]` M`, allowing selecting of any
		  *                 of its components/columns, just as with components of tables joined using
		  *                 the [[net.noresttherein.oldsql.sql.Join Join]] classes (and through the same
		  *                 mechanisms).
		  * @param grouping a type class responsible for creating the returned ''group by'' clause, which defines
		  *                 the return type based on the type of the expression returned by the function passed
		  *                 as the first argument. See the `returns` section for a listing.
		  * @return a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] instance using this clause as its left side.
		  *         The mapping type on the right side will be the mapping for the expression `E` returned
		  *         by the passed function: if it is a
		  *         [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]], it is used directly after anchoring
		  *         to the relation based on its `Origin` type. In case of
		  *         [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] (including its column
		  *         subtype), the mapping is the mapping type parameter of the component expression.
		  *         Otherwise a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]
		  *         (or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] if `E` is
		  *         a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]) is used. In the latter case, the return type
		  *         is abbreviated as `F `[[net.noresttherein.oldsql.sql.GroupByVal GroupByVal]]` V`
		  *         (or `F `[[net.noresttherein.oldsql.sql.GroupByOne GroupByOne]]` V`), where `V` is the value type
		  *         of the expression `E`.
		  */
		def groupByLast[E](expr :Last[FromLast] => E)(implicit grouping :CanGroup[Generalized, F, E]) :grouping.Result =
			grouping(thisClause, expr(thisClause.last))

		/** Adds a ''group by'' clause to this ''from'' clause with all
		  * [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns of the given component.
		  * @param component a mapping for a component of one of the relations listed by this clause.
		  *                  It must be a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[S, O]`, where
		  *                  the `Origin` type `O` is a supertype of the `Generalized` type of this clause starting with
		  *                  `RowProduct AndFrom T`, where `T` is the mapping type for the relation this component
		  *                  comes from.
		  * @param typeParams used to instantiate the necessary `Subject` and `Origin` types `S`, `O` of the argument mapping.
		  * @param shift implicit evidence with the number of relations listed in the `Origin` type.
		  * @param projection a casting type class for `M` which provides its necessary type constructor accepting
		  *                   an `Origin` type.
		  */ //todo: this currently is not picked over the overload with ComponentSQL for some reason
		def groupBy[M <: Mapping, S, O <: RowProduct]
		           (component :M)
		           (implicit typeParams :M <:< MappingAt[O], belongs :Generalized <:< O,
		            shift :RelationCount.In[O], projection :OriginProjection[M, S])
				:F GroupBy projection.WithOrigin =
		{
			val relation = thisClause.fullTableStack(shift.offset).toRelationSQL.asInstanceOf[
				RelationSQL[Generalized, MappingOf[Any]#TypedProjection, Any, RowProduct AndFrom MappingOf[Any]#TypedProjection]
			]
			val expr = TypedComponentSQL(relation, projection[Generalized](component))(projection.isomorphism)
			GroupBy[thisClause.type, projection.WithOrigin, projection.WithOrigin, S](thisClause)(expr.asGrouping, True)
		}

		/** Adds a ''group by'' clause to this ''from'' clause with all
		  * [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns of the component from
		  * the given component expression based on this clause.
		  */
		def groupBy[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		           (component :ComponentSQL[Generalized, M])(implicit cast :BaseMappingSubject[M, T, S]) :F GroupBy M =
			GroupBy[thisClause.type, Generalized, M, T, S](thisClause, component)
//			GroupBy[thisClause.type, M, T, S](thisClause)(component.asGrouping, True)

		/** Adds a ''group by'' clause to this ''from'' clause with the given single column expression. */
		def groupBy[V](column :SingleColumn[Generalized, V]) :F GroupByOne V =
 			GroupByOne[thisClause.type, Generalized, V](thisClause, column)

		/** Adds a ''group by'' clause to this ''from'' clause with all member columns of the given expression.
		  * The expression is traversed structurally until a [[net.noresttherein.oldsql.sql.ColumnSQL column expression]]
		  * is encountered, which is then added to the ''group by'' clause of the generated SQL.
		  * Not all possible expressions are supported; the expression may consist of
		  *   - any single [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] (atomic SQL values),
		  *     in particular [[net.noresttherein.oldsql.sql.ast.ColumnTerm terms]],
		  *   - [[net.noresttherein.oldsql.sql.ast.ComponentSQL components]] (ranging from whole entities
		  *     to single columns),
		  *   - [[net.noresttherein.oldsql.sql.ast.AdapterSQL conversion]] nodes,
		  *   - any [[net.noresttherein.oldsql.sql.ast.CompositeSQL composites]] combining the above, in particular:
		  *   - [[net.noresttherein.oldsql.sql.ast.ChainTuple tuples]] and
		  *     [[net.noresttherein.oldsql.sql.ast.IndexedSQL indexed tuples]].
		  */
		def groupBy[V](expr :SingleSQL[Generalized, V]) :F GroupByVal V =
			GroupByVal[thisClause.type, Generalized, V](thisClause, expr)

		/** Adds a ''group by'' clause to this ''from'' clause with all member columns of the given expression. */
		def groupBy[V <: Listing](expr :IndexedSQL[Generalized, Single, V]) :F GroupBy IndexedMapping.of[V]#Mapping =
			GroupBy[thisClause.type, IndexedMapping.of[V]#M, IndexedMapping.of[V]#M, V](thisClause)(
				GroupingRelation[Generalized, IndexedMapping.of[V]#M, V](expr)
			)


		/** Wraps this clause in a special adapter allowing the use of
		  * [[net.noresttherein.oldsql.sql.AggregateFunction aggregate functions]] in its ''select'' clause.
		  * While available methods such as `count`, `sum`, `avg` etc. allow selecting of a single statistic
		  * directly from this instance, if results of more than one aggregate functions are required,
		  * they must be passed using the standard `select` methods defined in
		  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]:
		  * {{{
		  *     import AggregateFunction._
		  *     this.aggregate select { tables =>
		  *         val weapon = tables[Weapons]
		  *         Max(weapon.damage) ~ Min(weapon.damage) ~ Avg(weapon.damage)
		  *     }
		  * }}}
		  * Note that the general purpose [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]]
		  * method, which accepts a function of [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]],
		  * supports directly aggregate expressions based on the aggregation of this ''from'' clause.
		  */
		@inline def aggregate :Aggregated[F] = Aggregated(thisClause)



		/** Creates an aggregated SQL ''select'' with a single column and a single row, containing the result
		  * of applying the aggregate function of the argument to all rows from the ''from'' clause.
		  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
		  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
		  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
		  * the `Outer` clause).
		  * @return a `SelectColumn` subtype depending on whether this instance represents an outer select
		  *         ([[net.noresttherein.oldsql.sql.ast.SelectColumn.TopSelectColumn TopSelectColumn]])
		  *         or a subselect ([[net.noresttherein.oldsql.sql.ast.SelectColumn.SubselectColumn]])
		  */
		private[this] def select[V](selectClause :ColumnSQL[Aggregated[Generalized], Grouped, V])
		                           (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, V]])
				:result.Select =
			Aggregated(thisClause.complete) select selectClause
//			Aggregated[thisClause.type](thisClause :thisClause.type).select(selectClause)
//			thisClause.select(selectClause)(mechanics.CanSelect.selectAggregate[thisClause.Self, selectClause.type])


		/** Creates a single column SQL `select count(*) from` with this instance as the from clause.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
		  */
		def count(implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, Int]]) :result.Select =
			select(Count())

		/** Creates a single column SQL ''select'' counting all rows with non-null values in the given column.
		  * This translates to `select count(column) from this`.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
		  */
		def count(column :ColumnSQL[Generalized, Single, _])
		         (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, Int]]) :result.Select =
			select(Count(column))


		/** Creates a single column SQL ''select'' counting all rows with non-null values in the given column.
		  * This translates to `select count(column) from this`.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
		  */
		def count(column :JoinedMappings[F] => ColumnSQL[Generalized, Single, _])
		         (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, Int]]) :result.Select =
			select(Count(column(thisClause.mappings)))

		/** Creates a single column SQL ''select'' returning the smallest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]].
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a type with [[net.noresttherein.oldsql.sql.mechanics.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               and the actual ordering is not influenced by the type class.
		  * @return an SQL expression representing `select min(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Min]]
		  */
		def min[X](column :ColumnSQL[Generalized, Single, X])
		          (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, X]], number :SQLOrdering[X])
				:result.Select =
			select(Min(column))

		/** Creates a single column SQL ''select'' returning the smallest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]].
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a type with [[net.noresttherein.oldsql.sql.mechanics.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general
		  *               and the actual ordering is not influenced by the type class.
		  * @return SQL expression representing `select min(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Min]]
		  */
		def min[X](column :JoinedMappings[F] => ColumnSQL[Generalized, Single, X])
		          (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, X]], number :SQLOrdering[X])
				:result.Select =
			select(Min(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the largest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]].
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a type with [[net.noresttherein.oldsql.sql.mechanics.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  * @return an SQL expression representing `select max(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Max]]
		  */
		def max[X](column :ColumnSQL[Generalized, Single, X])
		          (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, X]], number :SQLOrdering[X])
				:result.Select =
			select(Max(column))

		/** Creates a single column SQL ''select'' returning the largest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]].
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a type with [[net.noresttherein.oldsql.sql.mechanics.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  * @return an SQL expression representing `select max(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Max]]
		  */
		def max[X](column :JoinedMappings[F] => ColumnSQL[Generalized, Single, X])
		          (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, X]], number :SQLOrdering[X])
				:result.Select =
			select(Max(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the sum of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select sum(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Sum]]
		  */
		def sum[X](column :ColumnSQL[Generalized, Single, X])
		          (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, X]], number :SQLNumber[X])
				:result.Select =
			select(Sum(column))

		/** Creates a single column SQL ''select'' returning the sum of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select sum(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Sum]]
		  */
		def sum[X](column :JoinedMappings[F] => ColumnSQL[Generalized, Single, X])
		          (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, X]], number :SQLNumber[X])
				:result.Select =
			select(Sum(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the average of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select avg(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Avg]]
		  */
		def avg[X](column :ColumnSQL[Generalized, Single, X])
		          (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, Avg.Result]], number :SQLNumber[X])
				:result.Select =
			select(Avg(column))

		/** Creates a single column SQL ''select'' returning the average of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select avg(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Var]]
		  */
		def avg[X](column :JoinedMappings[F] => ColumnSQL[Generalized, Single, X])
		          (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, Avg.Result]], number :SQLNumber[X])
				:result.Select =
			select(Avg(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the variance of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select var(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Var]]
		  */
		def variance[X](column :ColumnSQL[Generalized, Single, X])
		               (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, Var.Result]], number :SQLNumber[X])
				:result.Select =
			select(Var(column))

		/** Creates a single column SQL ''select'' returning the variance of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return a SQL expression representing `select var(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
		  */
		def variance[X](column :JoinedMappings[F] => ColumnSQL[Generalized, Single, X])
		               (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, Var.Result]], number :SQLNumber[X])
				:result.Select =
			select(Var(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the standard deviation of the gaussian approximation
		  * of the value distribution of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select stddev(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
		  */
		def stddev[X](column :ColumnSQL[Generalized, Single, X])
		             (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, StdDev.Result]], number :SQLNumber[X])
				:result.Select =
			select(StdDev(column))

		/** Creates a single column SQL ''select'' returning the standard deviation of the gaussian approximation
		  * of the value distribution of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select stddev(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
		  */
		def stddev[X](column :JoinedMappings[F] => ColumnSQL[Generalized, Single, X])
		             (implicit result :CanSelect[Aggregated[Complete], ColumnSQL[Aggregated[Generalized], Grouped, StdDev.Result]], number :SQLNumber[X])
				:result.Select =
			select(StdDev(column(thisClause.mappings)))
	}




	/** Extension methods for `TopFromSome` objects (''from'' clauses without any `Subselect`s which can serve
	  * as the basis for independent selects). It provides methods for introducing unbound parameters
	  * to the clause in the form of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'joins',
	  * which are available to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] in the same way
	  * as tables in `this` ''from'' clause. They do not add any entries to the generated SQL string's ''from'' clause,
	  * but instead each their usage (or their components, which represent some property or otherwise derivable value
	  * from the parameter type) is rendered as a separate statement parameter '?'.
	  */ //consider: making them available for any FromSome
	implicit class TopFromSomeExtension[F <: TopFromSome](private val thisClause :F) extends AnyVal {

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[X :SQLForm] :F WithParam X = JoinParam(thisClause, ParamRelation[X]())

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance. Non-inferable parameter type `X`
		  * can still be provided explicitly, even using the infix method call syntax:
		  * {{{
		  *     From(Hamsters) param[String] "name" on (_.name == _)
		  * }}}
		  * Note that the parameter name given here can be any `String`, not just literals, but is not used
		  * to add an `as` clause to the parameter mapping.
		  * @param name the suggested name for the parameter in the generated SQL, as specified by JDBC.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[X :SQLForm](name :String) :F WithParam X = JoinParam(thisClause, ParamRelation[X](name))

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  *
		  * The artificial pseudo relation [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]
		  * is best obtained using the [[net.noresttherein.oldsql.sql.?: ?:]] factory method
		  * defined in the [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] object:
		  * {{{
		  *     From(Hamsters) param ?:[String] on (_.name === _)
		  * }}}
		  * @param relation a pseudo relation dedicated to `ParamClause` joins, representing a future parameter
		  *                 of type `X`, which can be later accessed as any other mappings.
		  * @tparam X the parameter type.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[X](relation :ParamRelation[X]) :F WithParam X = JoinParam(thisClause, relation)

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  *
		  * The recommended ways of creating these relations are the
		  * [[net.noresttherein.oldsql.sql.?: ?:]] factory method in object from package `sql` and an extension method
		  * for `String` literals with the same name: [[net.noresttherein.oldsql.sql.method_?:.?: ?:]].
		  * {{{
		  *     def parameterize[N <: Label :ValueOf, X :SQLForm, F <: TopFromSome](from :F) :F WithParam X As N =
		  *         from param ?:[N, X]
		  *
		  *     From(Characters) param "XP".?:[Int] where { t => t[Characters].XP >= t.labeled["XP"] }
		  * }}}
		  * Importing the `?:` symbol imports at the same time the factory methods for named and unnamed parameter
		  * relations, the implicit conversion enriching `String` literals, and the container type
		  * with the type constructor for the type of `Mapping` used by `JoinParam`.
		  * @param relation a pseudo relation dedicated to `ParamClause` joins, representing a future parameter
		  *                 of type `X`, with its similarly synthetic `Mapping` being labeled with `N`,
		  *                 used at the same time for the suggested parameter name.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[N <: Label, X](relation :NamedParamRelation[N, X]) :F WithParam X As N =
			JoinParam(thisClause, relation)

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */ //the order of implicits is important to avoid double definition
		@inline def param[N <: Label, X](implicit form :SQLForm[X], name :ValueOf[N]) :F WithParam X As N =
			JoinParam(thisClause, (name.value :N) ?: form)
	}



	/** A refinement of the [[net.noresttherein.oldsql.sql.FromSome FromSome]] type, representing a top level,
	  * independent and non-empty ''from'' clause of a ''select'' without a ''group by'' clause - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] or [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]
	  * joins (is not a ''from'' clause of a subselect of some other select). In order to conform naturally
	  * (rather than by refinement) to `TopFromSome`, the clause must be ''complete'', that is its static type
	  * must start with either [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]]
	  * - rather than a wildcard or abstract type hiding an unknown prefix - and its `Generalized` supertype
	  * must be known, meaning all join kinds should be narrowed down at least to
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]].
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedRow GeneralizedRow]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * This is an 'ungrouped' subtype of the more generic [[net.noresttherein.oldsql.sql.RowProduct.TopRow TopRow]].
	  *  An `TopFromClause` can still contain (unbound) parameters;
	  *  [[net.noresttherein.oldsql.sql.FromClause.GroundFromClause GroundFromClause]] is the specialization of this type
	  *  without any `JoinParam` 'joins'.
	  */
	type TopFromSome = FromSome {
		type Implicit = RowProduct
	}

	/** A non empty `RowProduct` without any [[net.noresttherein.oldsql.sql.Subselect Subselect]],
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]],
	  * or [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]
	  * joins. Representing a single ''from'' clause (and not one of a nested subselect), without a ''group by'' clause,
	  * it can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedRow GeneralizedRow]], but there is no subtype
	  * relationship between the two in the type system. A `GroundFromSome` will however always by a subtype of
	  * its more generic variants, [[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]],
	  * [[net.noresttherein.oldsql.sql.FromClause.GroundFromClause GroundFromClause]] and
	  * [[net.noresttherein.oldsql.sql.FromSome.TopFromSome TopFromSome]] (which groups all outer clauses,
	  * including those with unbound parameters), as well as
	  * [[net.noresttherein.oldsql.sql.RowProduct.ParamlessRow ParamlessRow]].
	  */
	type GroundFromSome = FromSome {
		type Implicit = RowProduct
		type Base = RowProduct
		type DefineBase[+I <: RowProduct] = I
		type Params = @~
	}

//	private object GroundFromSomePrototype extends FromSome {
//
//	}

	/** An upper bound for all non-empty, 'true' ''from'' clauses which do not contain any
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'joins' in their concrete type. In order to prove
	  * this conformity the clause type must be complete and cannot contain
	  * [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] joins  (all joins in its static type must be at least
	  * as specific as their [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form).
	  * It is possible however to propagate this constraint to incomplete clauses simply
	  * by declaring them as `ParamlessFromSome Join X ...` instead of `FromSome Join X ...`.
	  * Note that `ParamlessFromSome &lt;: `[[net.noresttherein.oldsql.sql.RowProduct.ParamlessRow ParamlessRow]] and
	  * `ParamlessFromSome =:= `[[net.noresttherein.oldsql.sql.FromSome.ParameterizedFromSome ParameterizedFromSome]]`[@~]`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ParamlessFromClause]]
	  */
	type ParamlessFromSome = FromSome {
		type Params = @~
	}

	/** An upper bound for all non empty, 'true' ''from'' clauses which are known to contain at least one
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'join' in their static type. Note that a `FromSome`
	  * not conforming to this type does not mean that it indeed contains no parameters, as the information
	  * may have been lost by type abstraction. Note that
	  * `ParameterizedFromSome[P] &lt;: `[[net.noresttherein.oldsql.sql.RowProduct.ParameterizedRow ParameterizedRow]]`[P]`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ParameterizedFromClause]]
	  */
	type ParameterizedFromSome[P] = FromSome {
		type Params = P
	}

}




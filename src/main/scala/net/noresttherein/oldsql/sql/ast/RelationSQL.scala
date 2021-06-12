package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.collection.{Chain, Opt, Unique}
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.{ColumnMapping, MappingExtract, Relation, SQLForm}
import net.noresttherein.oldsql.schema.Relation.Table
import net.noresttherein.oldsql.sql.{AndFrom, ComponentSetter, Expanded, RowProduct, Select, SQLExpression}
import net.noresttherein.oldsql.sql.mechanics.{TableCount, TableOffset}
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, PartOf, PrefixOf, TopFrom}
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectMapping, TopSelectMapping}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.ast.SQLParameter
import net.noresttherein.oldsql.sql.ast.TableSQL.TableVisitor






//consider: unseal and 'seal' with a package private abstract method
trait JoinedRelation[F <: RowProduct, T[A] <: MappingAt[A]] extends ComponentSQL[F, T] {
	override type Origin = F
	override type Entity[A] = T[A]

	type Self = RelationSQL[F, M, T[F]#Subject, F] forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

	/** Offset of this relation in the clause `F`, counting ''from right to left'' and starting with 0. */
	def offset :Int
	def position :TableOffset[F, T] = new TableOffset(offset)

	override val mapping :T[F]
	override def origin :JoinedRelation[F, T] = this

	def includes :Unique[RefinedMapping[_, F]]
	def excludes :Unique[RefinedMapping[_, F]]

	override def alter(components :T[F] => ComponentSelection[_, F]*) :JoinedRelation[F, T]
	override def +-(components :T[F] => ComponentSelection[_, F]*) :JoinedRelation[F, T]

	override def include(components :Iterable[RefinedMapping[_, F]]) :JoinedRelation[F, T]
	override def exclude(components :Iterable[RefinedMapping[_, F]]) :JoinedRelation[F, T]
	override def include(components :T[F] => RefinedMapping[_, F]*) :JoinedRelation[F, T]
	override def exclude(components :T[F] => RefinedMapping[_, F]*) :JoinedRelation[F, T]
	override def +(component :T[F] => RefinedMapping[_, F]) :JoinedRelation[F, T]
	override def -(component :T[F] => RefinedMapping[_, F]) :JoinedRelation[F, T]
	override def default :JoinedRelation[F, T]

	def toRelationSQL :RelationSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

	/** Casts down this instance to the more strongly typed `RelationSQL`. The result type is an `Option`
	  * not because the cast may fail, but because the correct existential subtype cannot exist as a value
	  * (returning simply the existentially refined `RelationSQL` produces a compiler error about being
	  * unable to abstract over existential higher type `T`).
	  */
	def asRelationSQL :Option[RelationSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }]

	/** Casts this relation down to the more strongly typed `TableSQL` if it is a table (persistent or derived). */
	def asTableSQL :Option[TableSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }] = None


	/** A new `JoinedRelation` identical to this one, but in ''from'' clause `E` at offset `offset`. */
	override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :JoinedRelation[E, T]

	/** Simply returns the given relation. */
	override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :JoinedRelation[P, T] = relation

	/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
	def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedRelation[E[F], T]

	/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
	  * and returns a `JoinedRelation`. The `expand` method cannot be overriden here to return a `JoinedRelation`
	  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
	  */
	def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedRelation[G, T]


	/** Checks if this instance and the argument use the same [[net.noresttherein.oldsql.schema.Relation Relation]]
	  * and have the same offset. When comparing relations from the same
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], this attests that the two relations refer to
	  * the same alias in the ''from'' clause (or a grouping expression in the ''group by'' clause),
	  * ignoring `includes` and `excludes` lists which alter the column set for all components of this relation.
	  */
	def same(that :JoinedRelation.*) :Boolean = relation == that.relation && offset == that.offset

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedRelation.*]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case relation :JoinedRelation.* if (this canEqual relation) && (relation canEqual this) =>
			relation.offset == offset && relation.mapping == mapping &&
				relation.includes.toSet == includes.toSet && relation.excludes.toSet == excludes.toSet
		case _ => false
	}

	override def hashCode :Int = offset * 31 + mapping.hashCode

	override lazy val toString :String =
		if (includes.isEmpty && excludes.isEmpty)
			relation.refString + "#" + offset
		else
			(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(
				relation.refString + "#" + offset + "(", ",", ")"
			)


	private[sql] def concrete_JoinedRelation_subclass_must_extend_RelationSQL :Nothing
}




object JoinedRelation {

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Relation[MappingAt], Int)] =
		e match {
			case from :JoinedRelation.Typed[F, X] @unchecked => Got(from.relation -> from.offset)
			case _ => Lack
		}


	type * = JoinedRelation[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	type Last[M[O] <: MappingAt[O]] = JoinedRelation[RowProduct AndFrom M, M]

	type AnyIn[F <: RowProduct] = JoinedRelation[F, T] forSome { type T[O] <: MappingAt[O] }

	type Typed[F <: RowProduct, V] = JoinedRelation[F, T] forSome { type T[O] <: RefinedMapping[V, O] }

	type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedRelation[O, M] }
}






trait JoinedTable[F <: RowProduct, T[A] <: MappingAt[A]] extends JoinedRelation[F, T] {
	override def relation :Table[T] = toTableSQL.table //overriden in TableSQL, as this will cause a StackOverflowException
	def table :Table[T] = relation

	override def alter(components :T[F] => ComponentSelection[_, F]*) :JoinedTable[F, T]
	override def +-(components :T[F] => ComponentSelection[_, F]*) :JoinedTable[F, T]

	override def include(components :Iterable[RefinedMapping[_, F]]) :JoinedTable[F, T]
	override def exclude(components :Iterable[RefinedMapping[_, F]]) :JoinedTable[F, T]
	override def include(components :T[F] => RefinedMapping[_, F]*) :JoinedTable[F, T]
	override def exclude(components :T[F] => RefinedMapping[_, F]*) :JoinedTable[F, T]
	override def +(component :T[F] => RefinedMapping[_, F]) :JoinedTable[F, T]
	override def -(component :T[F] => RefinedMapping[_, F]) :JoinedTable[F, T]
	override def default :JoinedTable[F, T]

	def toTableSQL :TableSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

	/** Casts down this instance to the more strongly typed `RelationSQL`. The result type is an `Option`
	  * not because the cast may fail, but because the correct existential subtype cannot exist as a value
	  * (returning simply the existentially refined `RelationSQL` produces a compiler error about being
	  * unable to abstract over existential higher type `T`).
	  */
	def asTableSQL :Option[TableSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }]


	/** A new `JoinedTable` identical to this one, but in ''from'' clause `E` at offset `offset`. */
	override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :JoinedTable[E, T]

	/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
	def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedTable[E[F], T]

	/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
	  * and returns a `JoinedTable`. The `expand` method cannot be overriden here to return a `JoinedTable`
	  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
	  */
	def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedTable[G, T]

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedTable.*]

	private[sql] def concrete_JoinedTable_subclass_must_extend_TableSQL :Nothing
}




object JoinedTable {

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Table[MappingAt], Int)] =
		e match {
			case from :JoinedTable.Typed[F, X] @unchecked =>
				Got(from.relation -> from.offset)
			case _ => Lack
		}


	type * = JoinedTable[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	type Last[M[O] <: MappingAt[O]] = JoinedTable[RowProduct AndFrom M, M]

	type AnyIn[F <: RowProduct] = JoinedTable[F, T] forSome { type T[O] <: MappingAt[O] }

	type Typed[F <: RowProduct, V] = JoinedTable[F, T] forSome { type T[O] <: RefinedMapping[V, O] }

	type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedTable[O, M] }
}





//consider: why do we have includes and excludes here? Some problem with relation equality?
class RelationSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] protected
                 (override val relation :Relation[T], override val offset :Int,
                  override val includes :Unique[RefinedMapping[_, O]],
                  override val excludes :Unique[RefinedMapping[_, O]])
	extends JoinedRelation[O, T] with TypedComponentSQL[F, T, R, T, R, O]
{
	override val mapping :T[O] = relation[O]
	/** The export mapping used for the whole relation; note that it might not be `relation.export` due
	  * to includes and excludes on the particular instance of a component.
	  */
	override val export :RefinedMapping[R, O] = {
		if (includes.isEmpty && excludes.isEmpty) relation.export[O]
		else relation.export[O].apply(includes, excludes)
	}.asInstanceOf[RefinedMapping[R, O]]

	override val extract = MappingExtract.ident(export)

	override def projection :IsomorphicProjection[T, R, O] = OriginProjection.isomorphism

	override def groupingRelation :Relation[T] = relation

	override def origin :RelationSQL[O, T, R, O] = toRelationSQL

	@inline final override def toRelationSQL :RelationSQL[O, T, R, O] = this.asInstanceOf[RelationSQL[O, T, R, O]]

	override def asRelationSQL :Some[RelationSQL[O, T, R, O]] = Some(toRelationSQL)

	override def upcast :ComponentSQL[O, T] = this

	override def alter(components :T[O] => ComponentSelection[_, O]*) :RelationSQL[F, T, R, O] = {
		val newExcludes = components.view.map(_(mapping)).collect {
			case ExcludedComponent(c) => export.export(c)
		}.to(Unique)
		val newIncludes = components.view.map(_(mapping)).collect {
			case IncludedComponent(c) => export.export(c)
		}.to(Unique)
		new RelationSQL[F, T, R, O](relation, offset,
			(includes.view ++ newIncludes).filterNot(newExcludes.toSet).to(Unique),
			(excludes.view.filterNot(newIncludes.toSet) ++ newExcludes).to(Unique)
		)
	}

	override def +-(components :T[O] => ComponentSelection[_, O]*) :RelationSQL[F, T, R, O] =
		alter(components :_*)

	override def include(components :Iterable[RefinedMapping[_, O]]) :RelationSQL[O, T, R, O] =
		if (components.isEmpty) toRelationSQL
		else {
			val newIncludes = components.view.map(export.export(_)).to(Unique)
			new RelationSQL[O, T, R, O](
				relation, offset, includes ++ newIncludes, excludes.filterNot(newIncludes.toSet)
			)
		}

	override def exclude(components :Iterable[RefinedMapping[_, O]]) :RelationSQL[O, T, R, O] =
		if (components.isEmpty) toRelationSQL
		else {
			val newExcludes = components.view.map(export.export(_)).to(Unique)
			new RelationSQL[O, T, R, O](
				relation, offset, includes.filterNot(newExcludes.toSet), excludes ++ newExcludes
			)
		}

	override def include(components :T[O] => RefinedMapping[_, O]*) :RelationSQL[O, T, R, O] =
		include(components.view.map(_(mapping)))

	override def exclude(components :T[O] => RefinedMapping[_, O]*) :RelationSQL[O, T, R, O] =
		exclude(components.view.map(_(mapping)))

	override def +(component :T[O] => RefinedMapping[_, O]) :RelationSQL[O, T, R, O] =
		include(Unique.single(component(mapping)))

	override def -(component :T[O] => RefinedMapping[_, O]) :RelationSQL[O, T, R, O] =
		exclude(Unique.single(component(mapping)))

	override def default :RelationSQL[O, T, R, O] =
		if (includes.isEmpty && excludes.isEmpty) toRelationSQL
		else new RelationSQL[O, T, R, O](relation, offset, Unique.empty, Unique.empty)


	//these need to be overriden due to JoinedRelation's having wider bounds than the inherited implementations
	override def \[K <: MappingAt[O], X](component :K)(implicit project :OriginProjection[K, X])
			:TypedComponentSQL[O, T, R, project.WithOrigin, X, O] =
		TypedComponentSQL(origin, component)

	override def \[K <: MappingAt[O]]
	              (component :T[O] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.TypedResult[O, T, R, O] =
		factory(toRelationSQL, component(mapping))

	override def \[K <: ColumnMapping[_, O], X]
	              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
			:TypedColumnComponentSQL[O, T, R, project.WithOrigin, X, O] =
		TypedColumnComponentSQL(toRelationSQL, column)



	/** A new `RelationSQL` instance representing the same relation, but in ''from'' clause `E` at `offset`.
	  * The class of the created instance and all its fields except for `offset` are the same as in this instance.
	  */
	override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :RelationSQL[E, T, R, E] =
		new RelationSQL[E, T, R, E](relation, offset.tables,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		)

	/** Simply returns `relation.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.toRelationSQL toRelationSQL]]. */
	override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :RelationSQL[P, T, R, P] =
		relation.toRelationSQL.asInstanceOf[RelationSQL[P, T, R, P]]

	override def basedOn[U <: O, E <: RowProduct]
	                    (base :E)(implicit expansion :U PartOf E) :RelationSQL[E, T, R, _ >: E <: RowProduct] =
		new RelationSQL[E, T, R, E](relation, offset + expansion.lengthDiff,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		) //E is incorrect, but we lose this information anyway

	override def expand[U <: O, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
			:RelationSQL[E, T, R, _ >: E <: RowProduct] =
		new RelationSQL[E, T, R, E](relation, offset + ev.length,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		) //E is incorrect, but we lose this information anyway

	override def expand[E <: RowProduct](implicit expansion :O ExpandedBy E)
			:RelationSQL[E, T, R, _ >: E <: RowProduct] =
		new RelationSQL[E, T, R, E](relation, offset + expansion.length,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		)

	override def asIn[J[+L <: O] <: L Expanded T forSome { type T[A] <: MappingAt[A] }] :RelationSQL[J[F], T, R, J[O]] =
		new RelationSQL[J[F], T, R, J[O]](relation, offset + 1,
			includes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]],
			excludes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]]
		)

	override def asIn[E <: RowProduct](implicit expansion :O PrefixOf E) :RelationSQL[E, T, R, E] =
		new RelationSQL(relation, offset + expansion.lengthDiff,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		)


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ExpressionVisitor[O, Y]) :Y[GlobalScope, R] =
		visitor.relation(toRelationSQL)


	//the following methods are overriden due to a winder bound O inherited from invariant JoinedTable

	override def topSelectFrom[E <: O with GroundFrom](from :E) :TopSelectMapping[E, T, R] =
		SelectSQL(from, toRelationSQL)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
			:SubselectMapping[B, from.type, T, R] =
		SelectSQL.subselect(from, toRelationSQL)

	override def paramSelectFrom[P <: Chain, G <: O](from :TopFrom { type Generalized <: G; type Params = P })
			:SelectMapping[P, T] =
		Select(from)[T, R](toRelationSQL)


	override def :=[P <: RowProduct, Y, U](rvalue :SQLExpression[P, GlobalScope, Y])
	                                      (implicit promote :SQLTypeUnification[R, Y, U])
			:ComponentSetter[O, P, U] =
		ComponentSetter[O, T, R, P, Y, U](this, rvalue)(promote)

	override def :=[C <: MappingOf[R], E <: RowProduct, P <: RowProduct]
	               (component :C)(implicit cast :C <:< RefinedMapping[R, P],
	               subtype :SQLExpression[P, GlobalScope, R] <:< SQLExpression[E, GlobalScope, R],
	               project :OriginProjection[C, R], offset :TableCount[P, _ <: Numeral]) :ComponentSetter[O, E, R] =
		this := subtype(LooseComponent(component))

	override def :=[X, U](that :X)(implicit promote :SQLTypeUnification[R, X, U], form :SQLForm[X])
			:ComponentSetter[O, RowProduct, U] =
		this := SQLTerm(that)

	override def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[R, Y, U], form :SQLForm[Y])
			:ComponentSetter[O, RowProduct, U] =
		this := SQLParameter(rvalue)


	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case rel :RelationSQL.* @unchecked if rel canEqual this =>
			relation == rel.relation && includes.toSet == rel.includes.toSet && excludes.toSet == rel.excludes.toSet
		case _ => false
	}

	override def hashCode :Int = relation.hashCode

	override private[sql] def concrete_JoinedRelation_subclass_must_extend_RelationSQL :Nothing =
		throw new UnsupportedOperationException
}




object RelationSQL {

//		def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S]
//		         (table :Relation[T])(implicit offset :TableOffset[F, T, _ <: Numeral]) :RelationSQL[F, T, S, F] =
//			new RelationSQL[F, T, S, F](table, table[F], offset.tables)

	def apply[F <: RowProduct] :RelationSQLFactory[F] = new RelationSQLFactory[F] {}

	trait RelationSQLFactory[F <: RowProduct] extends Any {
		final def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		          (relation :Relation[M])
		          (implicit cast :InferTypeParams[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]],
		                    shift :TableOffset[F, T]) :RelationSQL[F, T, S, F] =
			new RelationSQL[F, T, S, F](cast(relation), shift.tables, Unique.empty, Unique.empty)
	}

	private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
	                      (table :Relation[T], index :Int) :RelationSQL[F, T, S, O] =
		new RelationSQL[F, T, S, O](table, index, Unique.empty, Unique.empty)

	private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
	                      (table :Relation[T], index :Int,
	                       includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]])
			:RelationSQL[F, T, S, O] =
		new RelationSQL[F, T, S, O](table, index, includes, excludes)


	def last[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	        (table :Relation[M])
	        (implicit cast :InferTypeParams[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:LastRelation[T, S] =
		new RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
			table, 0, Unique.empty, Unique.empty
		)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Relation[MappingAt], Int)] =
		e match {
			case from :RelationSQL.Typed[F, X] @unchecked =>
				Got(from.relation.asInstanceOf[Relation[MappingOf[X]#TypedProjection]] -> from.offset)
			case _ => Lack
		}


	type * = RelationSQL[F, T, X, O] forSome {
		type F <: RowProduct; type O >: F <: RowProduct
		type T[A] <: BaseMapping[X, A]; type X
	}

	type AnyIn[-F <: RowProduct] = RelationSQL[F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, A]; type R; type O >: F <: RowProduct
	}

	type Typed[-F <: RowProduct, R] = RelationSQL[F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, O]; type O >: F <: RowProduct
	}

	type LastRelation[T[A] <: BaseMapping[S, A], S] = RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T]

	def LastRelation[T[A] <: BaseMapping[S, A], S](from :Relation[T]) :LastRelation[T, S] =
		new RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
			from, 0, Unique.empty, Unique.empty
		)



	trait RelationVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends TableVisitor[F, Y] {
		def relation[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
		            (e :RelationSQL[F, T, R, O]) :Y[GlobalScope, R]
	}

	type MatchRelation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = RelationVisitor[F, Y]

	trait CaseRelation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends RelationVisitor[F, Y] {
		override def table[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
		                  (e :TableSQL[F, T, R, O]) :Y[GlobalScope, R] =
			relation(e)
	}
}






class TableSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] protected
              (override val relation :Table[T], override val offset :Int,
               override val includes :Unique[RefinedMapping[_, O]],
               override val excludes :Unique[RefinedMapping[_, O]])
	extends RelationSQL[F, T, R, O](relation, offset, includes, excludes)
	   with JoinedTable[O, T]
{
	@inline final override def toTableSQL :TableSQL[O, T, R, O] = this.asInstanceOf[TableSQL[O, T, R, O]]

	override def asTableSQL :Some[TableSQL[O, T, R, O]] = Some(toTableSQL)

	override def alter(components :T[O] => ComponentSelection[_, O]*) :TableSQL[F, T, R, O] = {
		val newExcludes = components.view.map(_(mapping)).collect {
			case ExcludedComponent(c) => export.export(c)
		}.to(Unique)
		val newIncludes = components.view.map(_(mapping)).collect {
			case IncludedComponent(c) => export.export(c)
		}.to(Unique)
		new TableSQL[F, T, R, O](relation, offset,
			(includes.view ++ newIncludes).filterNot(newExcludes.toSet).to(Unique),
			(excludes.view.filterNot(newIncludes.toSet) ++ newExcludes).to(Unique)
		)
	}

	override def +-(components :T[O] => ComponentSelection[_, O]*) :TableSQL[F, T, R, O] =
		alter(components :_*)

	override def include(components :Iterable[RefinedMapping[_, O]]) :TableSQL[O, T, R, O] =
		if (components.isEmpty) toTableSQL
		else {
			val newIncludes = components.view.map(export.export(_)).to(Unique)
			new TableSQL[O, T, R, O](
				relation, offset, includes ++ newIncludes, excludes.filterNot(newIncludes.toSet)
			)
		}

	override def exclude(components :Iterable[RefinedMapping[_, O]]) :TableSQL[O, T, R, O] =
		if (components.isEmpty) toTableSQL
		else {
			val newExcludes = components.view.map(export.export(_)).to(Unique)
			new TableSQL[O, T, R, O](
				relation, offset, includes.filterNot(newExcludes.toSet), excludes ++ newExcludes
			)
		}

	override def include(components :T[O] => RefinedMapping[_, O]*) :TableSQL[O, T, R, O] =
		include(components.view.map(_(mapping)))

	override def exclude(components :T[O] => RefinedMapping[_, O]*) :TableSQL[O, T, R, O] =
		exclude(components.view.map(_(mapping)))

	override def +(component :T[O] => RefinedMapping[_, O]) :TableSQL[O, T, R, O] =
		include(Unique.single(component(mapping)))

	override def -(component :T[O] => RefinedMapping[_, O]) :TableSQL[O, T, R, O] =
		exclude(Unique.single(component(mapping)))

	override def default :TableSQL[O, T, R, O] =
		if (includes.isEmpty && excludes.isEmpty) toTableSQL
		else new TableSQL[O, T, R, O](relation, offset, Unique.empty, Unique.empty)


	/** A new `TableSQL` instance representing the same relation, but in ''from'' clause `E` at `offset`.
	  * The class of the created instance and all its fields except for `offset` are the same as in this instance.
	  */
	override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :TableSQL[E, T, R, E] =
		new TableSQL[E, T, R, E](relation, offset.tables,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		)

	override def basedOn[U <: O, E <: RowProduct]
	                    (base :E)(implicit expansion :U PartOf E) :TableSQL[E, T, R, _ >: E <: RowProduct] =
		new TableSQL[E, T, R, E](relation, offset + expansion.lengthDiff,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		) //E is incorrect, but we lose this information anyway

	override def expand[U <: O, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
			:TableSQL[E, T, R, _ >: E <: RowProduct] =
		new TableSQL[E, T, R, E](relation, offset + ev.length,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		) //E is incorrect, but we lose this information anyway

	override def expand[E <: RowProduct](implicit expansion :O ExpandedBy E) :TableSQL[E, T, R, _ >: E <: RowProduct] =
		new TableSQL[E, T, R, E](relation, offset + expansion.length,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		)

	override def asIn[J[+L <: O] <: L Expanded T forSome { type T[A] <: MappingAt[A] }] :TableSQL[J[F], T, R, J[O]] =
		new TableSQL[J[F], T, R, J[O]](relation, offset + 1,
			includes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]],
			excludes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]]
		)

	override def asIn[E <: RowProduct](implicit expansion :O PrefixOf E) :TableSQL[E, T, R, E] =
		new TableSQL(relation, offset + expansion.lengthDiff,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		)


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ExpressionVisitor[O, Y]) :Y[GlobalScope, R] =
		visitor.relation(toTableSQL)

	override private[sql] def concrete_JoinedTable_subclass_must_extend_TableSQL :Nothing =
		throw new UnsupportedOperationException
}




object TableSQL {

	def apply[F <: RowProduct] :TableSQLFactory[F] = new TableSQLFactory[F] {}

	trait TableSQLFactory[F <: RowProduct] extends Any {
		final def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		               (table :Table[M])
		               (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]],
		                         offset :TableOffset[F, T]) :RelationSQL[F, T, S, F] =
			new TableSQL[F, T, S, F](cast(table), offset.tables, Unique.empty, Unique.empty)
	}

	private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
	                      (table :Table[T], index :Int) :RelationSQL[F, T, S, O] =
		new TableSQL[F, T, S, O](table, index, Unique.empty, Unique.empty)

	private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
	                      (table :Table[T], index :Int,
	                       includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]])
			:TableSQL[F, T, S, O] =
		new TableSQL[F, T, S, O](table, index, includes, excludes)


	def last[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	        (table :Table[M])
	        (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
			:LastTable[T, S] =
		new TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
			table, 0, Unique.empty, Unique.empty
		)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Table[MappingAt], Int)] =
		e match {
			case from :TableSQL.Typed[F, X] @unchecked =>
				Got(from.relation.asInstanceOf[Table[MappingOf[X]#TypedProjection]] -> from.offset)
			case _ => Lack
		}



	type * = TableSQL[F, T, X, O] forSome {
		type F <: RowProduct; type O >: F <: RowProduct
		type T[A] <: BaseMapping[X, A]; type X
	}

	type AnyIn[-F <: RowProduct] = TableSQL[F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, A]; type R; type O >: F <: RowProduct
	}

	type Typed[-F <: RowProduct, R] = TableSQL[F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, O]; type O >: F <: RowProduct
	}

	type LastTable[T[A] <: BaseMapping[S, A], S] = TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T]

	def LastTable[T[A] <: BaseMapping[S, A], S](from :Table[T]) :LastTable[T, S] =
		new TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
			from, 0, Unique.empty, Unique.empty
		)



	trait TableVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def table[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct](e :TableSQL[F, T, R, O]) :Y[GlobalScope, R]
	}

	type MatchTable[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = TableVisitor[F, Y]

	type CaseTable[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = TableVisitor[F, Y]
}




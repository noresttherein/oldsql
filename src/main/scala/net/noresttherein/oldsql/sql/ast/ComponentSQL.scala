package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{Chain, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, ColumnMappingExtract, ColumnReadForm, Mapping, MappingExtract, Relation, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.slang.classNameMethods
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, Select, SQLExpression, WithClause}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, PartOf, TopFrom}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLExpression.{*, ExpressionVisitor, GlobalScope, Lift, LocalScope}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.{ColumnComponentConversion, TypedColumnComponentSQL}
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.TypedColumnComponentSQL.{CaseColumnComponent, ColumnComponentVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.BaseColumnComponentConversion
import net.noresttherein.oldsql.sql.ast.ComponentLValueSQL.BaseComponentConversion
import net.noresttherein.oldsql.sql.ast.ComponentSQL.{ComponentConversion, TypedComponentSQL}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL.ProperComponent
import net.noresttherein.oldsql.sql.ast.RelationSQL.{CaseRelation, RelationVisitor}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectMapping, TopSelectMapping}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, SubselectColumnMapping, TopSelectColumnAs, TopSelectColumnMapping}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, TableCount, TableOffset}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






trait ComponentSQL[-F <: RowProduct, M[A] <: MappingAt[A]]
	extends MappingSQL[F, GlobalScope, M] with ComponentLValueSQL[F, M, M[Unit]#Subject]
{
	//consider: in quite a few places we create 'copies' of the expression using `table \ this.mapping`
	// this is not very OO as it will convert any custom subclass of this trait with default implementation
	/** The mapping type of the `SQLRelation` to which this component belongs. */
	type Entity[A] <: MappingAt[A]

	override def readForm :SQLReadForm[M[Unit]#Subject] = export.selectForm
	override def component :ComponentSQL[F, M] = this

	/** A pseudo relation adapting this expression for use in ''group by'' clauses
	  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and [[net.noresttherein.oldsql.sql.By By]].
	  */
	def groupingRelation :Relation[M]

	def entity :Entity[Origin] = origin.mapping
	def relation :Relation[Entity] = origin.relation
	def origin :JoinedRelation[Origin, Entity]

	def extract :MappingExtract[Entity[Origin]#Subject, M[Origin]#Subject, Origin]
	//fixme: currently export can be not a component of table.export if include/exclude is non empty;
	//  exposing it outside may in turn mean someone creates a ComponentSQL using *it* is the non-exported mapping.
	def export :RefinedMapping[M[Unit]#Subject, Origin]

	@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
	def alter(components :M[Origin] => ComponentSelection[_, Origin]*) :ComponentSQL[F, M]

	@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
	def +-(components :M[Origin] => ComponentSelection[_, Origin]*) :ComponentSQL[F, M]

	def include(components :Iterable[RefinedMapping[_, Origin]]) :ComponentSQL[F, M]

	@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
	def exclude(components :Iterable[RefinedMapping[_, Origin]]) :ComponentSQL[F, M]

	def include(components :M[Origin] => RefinedMapping[_, Origin]*) :ComponentSQL[F, M]

	@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
	def exclude(components :M[Origin] => RefinedMapping[_, Origin]*) :ComponentSQL[F, M]

	def +(component :M[Origin] => RefinedMapping[_, Origin]) :ComponentSQL[F, M]

	@throws[IllegalArgumentException]("if the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column.")
	def -(component :M[Origin] => RefinedMapping[_, Origin]) :ComponentSQL[F, M]

	def default :ComponentSQL[F, M]

	def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
			:ComponentSQL[F, project.WithOrigin]

	def \[K <: MappingAt[Origin]]
	     (component :M[Origin] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.Result[F]

	def \[K <: ColumnMapping[_, Origin], X]
	     (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
			:ColumnComponentSQL[F, project.WithOrigin, X]

	override def to[Y](implicit lift :Lift[M[Unit]#Subject, Y]) :ComponentLValueSQL[F, M, Y] =
		if (lift == Lift.self[Y]) this.asInstanceOf[ComponentLValueSQL[F, M, Y]]
		else ComponentConversion(this, lift)

	override def isAnchored = true
	override def anchor(from :F) :ComponentSQL[F, M] = this
//		override def component :ComponentSQL[F, M] = this


	/** An expression for the same component, but from the first known
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] of ''from'' clause `P`.
	  * Created expression is equal to this one, except for its `origin`, which is substituted with
	  * `origin.moveTo(offset)`.
	  * @param offset a proof that the first relation listed in ''from'' clause `P` is the same as the `origin`
	  *               relation of this component (or, more precisely, they use the same mapping type `Entity`),
	  *               carrying the offset of the new origin.
	  */
	def moveTo[P <: RowProduct](offset :TableOffset[P, Entity]) :ComponentSQL[P, M]

	/** An expression for the same component, but with the given relation used as its
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]].
	  */
	def graft[P <: RowProduct](relation :JoinedRelation[P, Entity]) :ComponentSQL[P, M]


	override def split(implicit op :OperationType) :Seq[ColumnSQL[F, GlobalScope, _]] =
		op.defaultColumns(export).map(origin \ _).toSeq

	override def columnCount(implicit spelling :SQLSpelling) :Int =
		spelling.scope.defaultColumns(export).size


	override def withClause :WithClause = origin.withClause

	protected override def reverseCollect[X](fun :PartialFunction[*, X], acc :List[X]) :List[X] =
		origin.reverseCollectForwarder(fun, super.reverseCollect(fun, acc))


	protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		origin.relation.spell(origin, export)(context, params)

	protected override def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
		origin.relation.inline(origin, export)(context, params)


	override def isomorphic(expression :SQLExpression.*) :Boolean = this == expression

	private[oldsql] override def equivalent(expression :SQLExpression.*) :Boolean = expression match {
		case self :AnyRef if self eq this => true
		case component :ComponentSQL[_, _] if component canEqual this =>
			relation == component.relation && mapping == component.mapping
		case _ => false
	}


	override def toString :String = origin.toString + "." + mapping.mappingName

	protected override def all_subclasses_of_ComponentLValueSQL_must_extend_ComponentSQL_or_LooseComponent :Nothing =
		throw new UnsupportedOperationException
}






object ComponentSQL { //support for AlteredMapping
	//fixme: again, buggy overload resolution picks this instead of the following one even when given a relation
	//  and the order of method declaration doesn't seem to have any effect in this case.
	def apply[F <: RowProduct, M <: BaseMapping[S, O], S, O >: F <: RowProduct]
	         (from :F, component :M)
	         (implicit offset :TableCount[O, _ <: Numeral], project :OriginProjection[M, S])
			:ComponentSQL[F, project.WithOrigin] =
	{
		val relation = from.fullTableStack(offset.offset).toRelationSQL
		                   .asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, O]]
		TypedComponentSQL(relation, component)
	}

	def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, K <: Mapping, V, O >: F <: RowProduct]
	         (from :RelationSQL[F, T, R, O], component :K)(implicit project :OriginProjection[K, V])
			:ComponentSQL[F, project.WithOrigin] =
		TypedComponentSQL(from, component)

	def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, C <: ColumnMapping[_, _], V, O >: F <: RowProduct]
	         (from :RelationSQL[F, T, R, O], column :C)
	         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: ColumnMapping[V, A] })
			:ColumnComponentSQL[F, project.WithOrigin, V] =
		TypedColumnComponentSQL(from, column)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
			:Opt[(RelationSQL[F, T, R, O], MappingExtract[R, X, O])
					forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: RowProduct }] =
		e match {
			case component :ComponentSQL.* @unchecked =>
				Got((component.origin, component.extract).asInstanceOf[
					(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], MappingExtract[Any, X, F])
				])
			case _ => Lack
		}


	type * = ComponentSQL[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }


	/** A conversion applying type promotion to a
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSLQ]]. Together with the component
	  * expression type itself and its [[net.noresttherein.oldsql.sql.ast.LooseComponent 'loose']]
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.LooseComponentConversion counterparts]],
	  * they form the close set of direct subtypes of
	  * [[net.noresttherein.oldsql.sql.ast.ComponentLValueSQL ComponentLValueSQL]]
	  * used as the ''l-value'' of of component assignment
	  * [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]], guaranteeing that the underlying
	  * expression is a component, one way or another. This class does not extend
	  * [[net.noresttherein.oldsql.sql.ast.ConversionSQL.PromotionConversion PromotionConversion]] only because
	  * the latter is a class, preventing suitable multiple inheritance by
	  * [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.ColumnComponentConversion ColumnComponentConversion]].
	  */
	sealed class ComponentConversion[-F <: RowProduct, M[O] <: MappingAt[O], V]
	                                (override val value :ComponentSQL[F, M], override val lift :Lift[M[Unit]#Subject, V])
		extends BaseComponentConversion[F, M, V](value, lift)
	{
		override def anchor(from :F) :ComponentConversion[F, M, V] = this

		override def to[Y](implicit lift :Lift[V, Y]) :ComponentLValueSQL[F, M, Y] =
			ComponentConversion(value, this.lift andThen lift)

		protected override def all_subclasses_of_ComponentLValueSQL_must_extend_ComponentSQL_or_LooseComponent :Nothing =
			throw new UnsupportedOperationException
	}


	object ComponentConversion {
		def apply[F <: RowProduct, M[O] <: MappingAt[O], V]
		         (component :ComponentSQL[F, M], lift :Lift[M[Unit]#Subject, V]) :ComponentConversion[F, M, V] =
			new ComponentConversion(component, lift)

		def unapply[F <: RowProduct, M[O] <: MappingAt[O], V](lvalue :ComponentLValueSQL[F, M, V])
				:Opt[(ComponentSQL[F, M], Lift[M[Unit]#Subject, V]) forSome { type s }] =
			lvalue match {
				case set :ComponentConversion[F, M, V] => Got(set.value, set.lift)
				case _ => Lack
			}

		type * = ComponentConversion[_ <: RowProduct, M, _] forSome { type M[O] <: MappingAt[O] }
	}



	trait Factory[M <: Mapping] {
		type Result[-F <: RowProduct] <: ComponentSQL[F, M] forSome { type M[O] <: MappingAt[O] }
		type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] <: Result[F]

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
		         (table :RelationSQL[F, T, R, O], mapping :M) :TypedResult[F, T, R, O]
	}


	sealed abstract class ComponentAbstractFactory {
		implicit def componentFactory[M <: MappingOf[S], S](implicit project :OriginProjection[M, S])
				:Factory[M] {
					type Result[-F <: RowProduct] = ComponentSQL[F, project.WithOrigin]
					type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] =
						TypedComponentSQL[F, T, R, project.WithOrigin, S, O]
				} =
			new Factory[M] {
				type Result[-F <: RowProduct] = ComponentSQL[F, project.WithOrigin]
				type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] =
					TypedComponentSQL[F, T, R, project.WithOrigin, S, O]

				override def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
				                  (table :RelationSQL[F, T, R, O], mapping :M) =
					(table \ mapping.withOrigin[O])(project.isomorphism)
			}
	}


	object Factory extends ComponentAbstractFactory {
		implicit def columnFactory[M <: ColumnMapping[S, _], S]
		             (implicit project :OriginProjection[M, S] { type WithOrigin[O] <: ColumnMapping[S, O] })
				:Factory[M] {
					type Result[-F <: RowProduct] = ColumnComponentSQL[F, project.WithOrigin, S]
					type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] =
						TypedColumnComponentSQL[F, T, R, project.WithOrigin, S, O]
				} =
			new Factory[M] {
				type Result[-F <: RowProduct] = ColumnComponentSQL[F, project.WithOrigin, S]
				type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] =
					TypedColumnComponentSQL[F, T, R, project.WithOrigin, S, O]

				override def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
				                  (table :RelationSQL[F, T, R, O], mapping :M) =
					(table \ mapping.withOrigin[O])(project.isomorphism)
			}
	}







	trait TypedComponentSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R,
	                        M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		extends ComponentSQL[F, M]
	{
		override type Origin = O
		override type Entity[A] = T[A]

		def projection :IsomorphicProjection[M, V, O]

		override def groupingRelation :Relation[M] = GroupingRelation[F, M, V, O](this)(projection)

		override def origin :RelationSQL[F, T, R, O]
		override def extract :MappingExtract[R, V, O]
		override def export :RefinedMapping[V, O] = extract.export


		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def alter(components :M[O] => ComponentSelection[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] = {
			val newExcludes = components.view.map(_(mapping)).collect { case ExcludedComponent(c) => c }
			val newIncludes = components.view.map(_(mapping)).collect { case IncludedComponent(c) => c }
			include(newIncludes).exclude(newExcludes)
		}

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def +-(components :M[O] => ComponentSelection[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] =
			alter(components :_*)

		override def include(components :Iterable[RefinedMapping[_, O]]) :TypedComponentSQL[F, T, R, M, V, O] =
		 	components.view.collect { case c if !export.contains(c) => c }.toList match {
			    case Nil => origin.include(components) \ mapping
				case comps => throw new IllegalArgumentException(
					s"Cannot include not belonging components $comps with mapping $mapping."
				)
			}

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def exclude(components :Iterable[RefinedMapping[_, O]]) :TypedComponentSQL[F, T, R, M, V, O] =
			components.view.collect { case c if !export.contains(c) => c }.toList match {
				case Nil => origin.exclude(components) \ mapping
				case comps => throw new IllegalArgumentException(
					s"Cannot exclude not belonging components $comps from mapping $mapping."
				)
			}

		override def include(components :M[O] => RefinedMapping[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] =
			include(components.map(_(mapping)))

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def exclude(components :M[O] => RefinedMapping[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] =
			exclude(components.map(_(mapping)))

		override def +(component :M[O] => RefinedMapping[_, O]) :TypedComponentSQL[F, T, R, M, V, O] =
			include(Unique.single(component(mapping)))

		@throws[IllegalArgumentException]("if the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column.")
		override def -(component :M[O] => RefinedMapping[_, O]) :TypedComponentSQL[F, T, R, M, V, O] =
			exclude(Unique.single(component(mapping)))

		override def default :TypedComponentSQL[F, T, R, M, V, O] = graft(origin.default)


		override def \[K <: MappingAt[O], X](component :K)(implicit project :OriginProjection[K, X])
				:TypedComponentSQL[F, T, R, project.WithOrigin, X, O] =
			origin \ component

		override def \[K <: MappingAt[O]]
		              (component :M[O] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.TypedResult[F, T, R, O] =
			factory(origin, component(mapping))

		override def \[K <: ColumnMapping[_, Origin], X]
		              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, X, O] =
			//we don't need to check if column==entity as a column always has itself as its column and among extractors.
			origin \ column



		override def moveTo[P <: RowProduct](offset :TableOffset[P, T]) :TypedComponentSQL[P, T, R, M, V, P] =
			graft(origin.moveTo(offset))

		override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :TypedComponentSQL[P, T, R, M, V, P] =
			relation.asInstanceOf[RelationSQL[P, T, R, P]] \ mapping.withOrigin[P]

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			graft(origin.expand(base).asInstanceOf[RelationSQL[E, T, R, E]])

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E)
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			graft(origin.expand[E].asInstanceOf[RelationSQL[E, T, R, E]])

		override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectMapping[E, M, V] =
			SelectSQL(from, this)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, this)

		override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
				:SelectMapping[P, M] =
			Select(from)[M, V](this)


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TypedComponentSQL.* @unchecked]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true

			case component :TypedComponentSQL.* @unchecked if canEqual(component) && component.canEqual(this) =>
				origin == component.origin && mapping == component.mapping
			case _ => false
		}

		override def hashCode :Int = origin.hashCode * 31 + mapping.hashCode
	}




	object TypedComponentSQL {

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, K <: Mapping, V, O >: F <: RowProduct]
		         (from :RelationSQL[F, T, R, O], component :K)(implicit project :OriginProjection[K, V])
				:TypedComponentSQL[F, T, R, project.WithOrigin, V, O] =
			component match {
				case column :ColumnMapping[V @unchecked, O @unchecked] =>
					TypedColumnComponentSQL(from, column)
						.asInstanceOf[TypedComponentSQL[F, T, R, project.WithOrigin, V, O]]
				case _ if component == from.mapping || component == from.export =>
					from.asInstanceOf[TypedComponentSQL[F, T, R, project.WithOrigin, V, O]]
				case _ =>
					new ProperComponent[F, T, R, project.WithOrigin, V, O](from, project(component))(project.isomorphism)
			}


		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[F, T, R, O], MappingExtract[R, X, O])
						forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: RowProduct }] =
			e match {
				case component :TypedComponentSQL.* @unchecked =>
					Got((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], MappingExtract[Any, X, F])
					])
				case _ => Lack
			}


		type * = TypedComponentSQL[F, T, R, M, V, O] forSome {
			type F <: RowProduct; type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type AnyIn[-F <: RowProduct] = TypedComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type Typed[-F <: RowProduct, V] = TypedComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]
		}


		private[ast] class ProperComponent[-F <: RowProduct, T[A] <: BaseMapping[R, A], R,
		                                   M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		                                  (override val origin :RelationSQL[F, T, R, O],
		                                   override val mapping :M[O])
		                                  (implicit override val projection :IsomorphicProjection[M, V, O])
			extends TypedComponentSQL[F, T, R, M, V, O]
		{
			override val extract :MappingExtract[R, V, O] = origin.export(mapping)
			override val readForm :SQLReadForm[V] = super.readForm

			//must be here because RelationSQL couldn't override a concrete definition due to extending invariant
			// JoinedRelation for some super type of F.
			protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
			                              (visitor :ExpressionVisitor[F, Y]) :Y[GlobalScope, V] =
				visitor.component(this)
		}


		trait ComponentVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends RelationVisitor[F, Y] with ColumnComponentVisitor[F, Y]
		{
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
			             (e :TypedComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		trait MatchComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ComponentVisitor[F, Y] with CaseRelation[F, Y] with CaseColumnComponent[F, Y]
		{
			override def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
			                      (e :TypedColumnComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V] =
				component[T, R, M, V, O](e :TypedComponentSQL[F, T, R, M, V, O])
		}

		trait CaseComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchComponent[F, Y] {
			override def relation[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
			                     (e :RelationSQL[F, T, R, O]) :Y[GlobalScope, R] =
				component(e)
		}
	}

}






trait ColumnComponentSQL[-F <: RowProduct, M[A] <: ColumnMapping[V, A], V]
	extends ComponentSQL[F, M] with ColumnSQL[F, GlobalScope, V] with ColumnLValueSQL[F, M, V]
{ //consider: a fishy thing is that this column may be excluded from an operation, breaking(?) ColumnSQL contract
	override def upcast :ColumnSQL[F, GlobalScope, Subject] = this
	override def component :ColumnComponentSQL[F, M, V] = this

	override def extract :ColumnMappingExtract[Entity[Origin]#Subject, V, Origin]
	override def export :ColumnMapping[V, Origin]

	override def readForm :ColumnReadForm[V] = export.selectForm match { //an alternative would be for the
		case select :ColumnReadForm[V @unchecked] => select //always the case unless the column has ExtraSelect
		case _ => throw new UnsupportedOperationException(  //being excluded doesn't affect being a column form
			s"No (Column)ReadForm for selecting column $this: most likely the column $export is not selectable."
		)
	}
	override def form :ColumnForm[V] = export.form

	//fixme: should return ComponentSQL as it might be empty
	override def include(components :Iterable[RefinedMapping[_, Origin]]) :ColumnComponentSQL[F, M, V]
	override def include(components :M[Origin] => RefinedMapping[_, Origin]*) :ColumnComponentSQL[F, M, V]
	override def +(component :M[Origin] => RefinedMapping[_, Origin]) :ColumnComponentSQL[F, M, V]
	override def default :ColumnComponentSQL[F, M, V]


	override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
		if (lift == Lift.self[V]) this.asInstanceOf[ColumnLValueSQL[F, M, Y]]
		else ColumnComponentConversion(this, lift)

	override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)

	override def anchor(from :F) :ColumnComponentSQL[F, M, V] = this

	override def moveTo[P <: RowProduct](offset :TableOffset[P, Entity]) :ColumnComponentSQL[P, M, V]

	override def graft[P <: RowProduct](relation :JoinedRelation[P, Entity]) :ColumnComponentSQL[P, M, V]

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
			:ColumnComponentSQL[E, M, V] =
		expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
			:ColumnComponentSQL[E, M, V] =
		expand[E]

	override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ColumnComponentSQL[E, M, V]

	override def selectFrom(from :F) :SelectColumnAs[from.Base, M, V] =
		if (from.isParameterized)
			throw new IllegalArgumentException(
				s"Cannot use a parameterized clause as a basis for select $this: $from."
			)
		else if (from.isSubselect) {
			subselectFrom(from.asInstanceOf[ExactSubselectOf[from.type, NonEmptyFrom]])
				.asInstanceOf[SelectColumnAs[from.Base, M, V]]
		} else
			topSelectFrom(from.asInstanceOf[F with GroundFrom])

	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectColumnAs[M, V] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
			:SubselectColumnAs[B, M, V] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:SelectMapping[P, M] =
		Select(from.self)[M, V](this)


	override def inParens[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		spelling(this :ColumnSQL[E, LocalScope, V])(context, params)

}




object ColumnComponentSQL {

	def apply[F <: RowProduct, M <: ColumnMapping[S, O], S, O >: F <: RowProduct]
	         (from :F, component :M)
	         (implicit offset :TableCount[O, _ <: Numeral],
	                   project :OriginProjection[M, S] { type WithOrigin[A] <: ColumnMapping[S, A] })
			:ColumnComponentSQL[F, project.WithOrigin, S] =
	{
		val relation = from.fullTableStack(offset.offset).toRelationSQL
			.asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, O]]
		TypedColumnComponentSQL(relation, component)
	}

	def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, C <: ColumnMapping[_, _], V, O >: F <: RowProduct]
	         (from :RelationSQL[F, T, R, O], column :C)
	         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: ColumnMapping[V, A] })
			:ColumnComponentSQL[F, project.WithOrigin, V] =
		ColumnComponentSQL(from, column)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
			:Opt[(RelationSQL[F, T, R, O], ColumnMappingExtract[R, X, O])
				forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: RowProduct }] =
		e match {
			case component :TypedColumnComponentSQL.* @unchecked =>
				Got((component.origin, component.extract).asInstanceOf[
					(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], ColumnMappingExtract[Any, X, F])
				])
			case _ => Lack
		}


	type * = ColumnComponentSQL[_ <: RowProduct, M, V] forSome { type V; type M[O] <: ColumnMapping[V, O] }


	/** A conversion applying a type promotion
	  * to a [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL ColumnComponentSQL]] instance
	  * in the process of the type unification with another SQL expression of a type automatically converted
	  * by the DBMS to/from the column type. Used in [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]]
	  * to represent setting of the value of a table column.
	  */
	class ColumnComponentConversion[-F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
	                               (override val value :ColumnComponentSQL[F, M, S], override val lift :Lift[S, V])
		extends ComponentConversion[F, M, V](value, lift) with BaseColumnComponentConversion[F, M, S, V]
	{
		override def component :ColumnComponentSQL[F, M, S] = value

		override def anchor(from :F) :ColumnComponentConversion[F, M, S, V] = this

		override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
			ColumnComponentConversion(value, this.lift andThen lift)
	}


	object ColumnComponentConversion {
		def apply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
		         (column :ColumnComponentSQL[F, M, S], lift :Lift[S, V]) :ColumnComponentConversion[F, M, S, V] =
			new ColumnComponentConversion(column, lift)


		def unapply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V](lvalue :ComponentLValueSQL[F, M, V])
				:Opt[(ColumnComponentSQL[F, M, S], Lift[S, V])] =
			lvalue match {
				case set :ColumnComponentConversion[F, M, S, V] @unchecked => Got((set.value, set.lift))
				case _ => Lack
			}

		def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, V])
				:Opt[(ColumnComponentSQL[F, m, s], Lift[s, V]) forSome { type m[O] <: ColumnMapping[s, O]; type s }] =
			e match {
				case set :ColumnComponentConversion[F, MappingOf[Any]#ColumnProjection, Any, V] @unchecked =>
					Got((set.value, set.lift))
				case _ => Lack
			}


		type * = ColumnComponentConversion[_ <: RowProduct, M, S, _] forSome { type M[O] <: ColumnMapping[S, O]; type S }
	}









	trait TypedColumnComponentSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R,
	                              M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		extends TypedComponentSQL[F, T, R, M, V, O] with ColumnComponentSQL[F, M, V]
	{
		override def export :ColumnMapping[V, O] = extract.export

		override def include(components :Iterable[RefinedMapping[_, O]]) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			this

		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def exclude(components :Iterable[RefinedMapping[_, O]]) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			if (components.isEmpty) this
			else
				throw new UnsupportedOperationException(
					s"A column expression $this cannot be excluded from itself: $components."
				)

		override def include(components :M[O] => RefinedMapping[_, O]*) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			this

		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def exclude(components :M[O] => RefinedMapping[_, O]*) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			if (components.isEmpty) this
			else exclude(components.view.map(_(mapping)))

		override def +(component :M[O] => RefinedMapping[_, O]) :TypedColumnComponentSQL[F, T, R, M, V, O] = this

		@throws[UnsupportedOperationException]("if this component is a column.")
		override def -(component :M[O] => RefinedMapping[_, O]) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			throw new UnsupportedOperationException(
				s"A column expression $this cannot be excluded from itself: ${component(mapping)}."
			)


		override def default :TypedColumnComponentSQL[F, T, R, M, V, O] = graft(origin.default)


		override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :TypedColumnComponentSQL[E, T, R, M, V, E] =
			graft(origin.moveTo(offset))

		override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :TypedColumnComponentSQL[P, T, R, M, V, P] =
			relation.asInstanceOf[RelationSQL[P, T, R, P]] \ mapping.withOrigin[P]

		override def basedOn[U <: F, E <: RowProduct]
		                    (base :E)(implicit expansion :U PartOf E)
				:TypedColumnComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ext :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:TypedColumnComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			graft(origin.expand(base))

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E)
				:TypedColumnComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			graft(origin.expand[E])

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[GlobalScope, V] =
			visitor.component(this)


		override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectColumnMapping[E, M, V] =
			SelectSQL(from, this)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectColumnMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, this) //todo: from.type is ugly!


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TypedColumnComponentSQL.* @unchecked]
	}




	object TypedColumnComponentSQL {

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, C <: ColumnMapping[_, _], V, O >: F <: RowProduct]
		         (from :RelationSQL[F, T, R, O], column :C)
		         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: ColumnMapping[V, A] })
				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, V, O] =
		{
			val cast = project[O](column)
			val export = from.export(cast).export
			val relation = if (from.includes.contains(export)) from else from + { _ => export }
			new ProperColumn[F, T, R, project.WithOrigin, V, O](relation, cast)(project.isomorphism)
		}


		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[F, T, R, O], ColumnMappingExtract[R, X, O])
						forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: RowProduct }] =
			e match {
				case component :TypedColumnComponentSQL.* @unchecked =>
					Got((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], ColumnMappingExtract[Any, X, F])
					])
				case _ => Lack
			}


		type * = TypedColumnComponentSQL[F, T, R, M, V, O] forSome {
			type F <: RowProduct; type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type AnyIn[-F <: RowProduct] = TypedColumnComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type Typed[-F <: RowProduct, V] = TypedColumnComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]
		}


		private class ProperColumn[-F <: RowProduct, T[A] <: BaseMapping[R, A], R,
		                           M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                          (relation :RelationSQL[F, T, R, O], column :M[O])
		                          (implicit project :IsomorphicProjection[M, V, O])
			extends ProperComponent[F, T, R, M, V, O](relation, column)
			   with TypedColumnComponentSQL[F, T, R, M, V, O]
		{
			override val extract :ColumnMappingExtract[R, V, O] = origin.export(mapping)
			override def export = extract.export

			//todo: sort out where the buff-related modifications take place to have consistent assembly semantics
			override val readForm = super.readForm
		}



		trait ColumnComponentVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
			             (e :TypedColumnComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		type MatchColumnComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentVisitor[F, Y]

		type CaseColumnComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentVisitor[F, Y]
	}

}

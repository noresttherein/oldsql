package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{Chain, Opt, Unique}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, ColumnReadForm, Mapping, SQLReadForm}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, Select, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, Lift, LocalScope}
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, PartOf, TopFrom}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.ColumnComponentConversion
import net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.BaseColumnComponentConversion
import net.noresttherein.oldsql.sql.ast.ComponentLValueSQL.BaseComponentConversion
import net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentConversion
import net.noresttherein.oldsql.sql.ast.LooseColumn.{LooseColumnConversion, LooseColumnVisitor}
import net.noresttherein.oldsql.sql.ast.LooseComponent.LooseComponentConversion
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectMapping, TopSelectMapping}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnMapping, TopSelectColumnMapping}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, TableCount}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






//All these classes would benefit with being parameterized with RefinedMapping and kin instead of BaseMapping,
//but a bug in the compiler makes subclasses of a subclass of MappingSQL not conform to MappingSQL
/** An expression evaluating to a component mapping of an undetermined at this point relation.
  * It can be used as a part of other expressions, but any expression containing loose components must
  * be [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] before actual SQL is generated.
  * This is done automatically for any expression used as a ''select'' clause in any of the factory methods in
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL$ SelectSQL]], as well as for all expressions created/passed
  * to [[net.noresttherein.oldsql.sql.RowProduct.where where]] method of
  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as well as its relatives.
  *
  * There is an implicit conversion from `M[F] for some M[O] <: BaseMapping[S, O]` to `LooseComponent[F, M, S]`,
  * providing that `F` is a `RowProduct` with an existing
  * [[net.noresttherein.oldsql.sql.mechanics.TableCount TableCount]]; it assumes that mapping `M[F]` is
  * a component of the first known relation in `F`. As `Origin` type is inherited from the parent mapping
  * by all its components, and all accessor methods of
  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] always return `T[F]`,
  * where `F` is a super type of the source `RowProduct` and `T` is the first known relation in `F`,
  * this holds true in the intended use case of lifting a component path to an `SQLExpression`.
  * Note however that `Origin` type can be set explicitly to any type, which would lead to
  * `NoSuchElementException` thrown when a `LooseComponent` is created with non-conforming `F` type.
  * Type safety is therefore guaranteed only for mappings obtained from official API, without manual
  * changing of the `Origin` type.
  *
  * This conversion can be triggered explicitly by invoking
  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.alter alter]],
  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.include include]], or
  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.exclude exclude]] method of this class
  * on a `Mapping`, which create a ''view'' of the mapping with certain subcomponents included/excluded from
  * the database operation the resulting expression will be used in.
  */
class LooseComponent[F <: RowProduct, M[A] <: BaseMapping[V, A], V] private[ast]
                    (override val mapping :M[F], val offset :Int,
                     val includes :Unique[RefinedMapping[_, F]], val excludes :Unique[RefinedMapping[_, F]])
                    (implicit val projection :IsomorphicProjection[M, V, F])
	extends MappingSQL[F, GlobalScope, M] with ComponentLValueSQL[F, M, V]
{
	def this(mapping :M[F], offset :Int)(implicit projection :IsomorphicProjection[M, V, F]) =
		this(mapping, offset, Unique.empty, Unique.empty)

	includes.find(!mapping.contains(_)) match {
		case Some(c) => throw new IllegalArgumentException(
			s"Included mapping $c is not a component of $mapping: " +
				s"cannot create LooseComponent($mapping, $offset, $includes, $excludes)."
		)
		case _ =>
	}
	excludes.find(!mapping.contains(_)) match {
		case Some(c) => throw new IllegalArgumentException(
			s"Excluded mapping $c is not a component of $mapping: " +
				s"cannot create LooseComponent($mapping, $offset, $includes, $excludes)."
		)
		case _ =>
	}

	override def readForm :SQLReadForm[V] = mapping.selectForm

	override def component :Nothing =
		throw new UnsupportedOperationException(
			s"An unanchored LooseComponent $this cannot be updated as its export version is unknown."
		)

	override type Origin = F

	/** Applies all given functions to this mapping and creates an SQL expression representing this mapping
	  * with the components wrapped in
	  * [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]] included
	  * and those wrapped in [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]]
	  * excluded. This is very similar to [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]
	  * method on any mapping, but takes a sequence of functions, instead of ready components. This makes it
	  * more convenient to call on return values of functions and methods, as the mapping `M` doesn't need
	  * to be assigned to an intermediate `val`. Example:
	  * {{{
	  *     Mages select _.alter(_.spellbook.+, _.familiar.-)
	  * }}}
	  *
	  * This method and [[net.noresttherein.oldsql.sql.ast.LooseComponent.+- +-]] method are equivalent.
	  */
	@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
	def alter(adjustments :M[F] => ComponentSelection[_, F]*) :LooseComponent[F, M, V] = {
		val newExcludes = adjustments.view.map(_(mapping)).collect {
			case ExcludedComponent(c) => mapping.export(c)
		}.to(Unique)
		val newIncludes = adjustments.view.map(_(mapping)).collect {
			case IncludedComponent(c) => mapping.export(c)
		}.to(Unique)
		new LooseComponent[F, M, V](mapping, offset,
			(includes.view ++ newIncludes).filterNot(newExcludes.toSet).to(Unique),
			(excludes.view.filterNot(newIncludes.toSet) ++ newExcludes).to(Unique)
		)(projection)
	}

	/** Applies all given functions to this mapping and creates an SQL expression representing this mapping
	  * with the components wrapped in
	  * [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]] included
	  * and those wrapped in [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]]
	  * excluded. This is very similar to [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]
	  * method on any mapping, but takes a sequence of functions, instead of ready components. This makes it
	  * more convenient to call on return values of functions and methods, as the mapping `M` doesn't need
	  * to be assigned to an intermediate `val`. Example:
	  * {{{
	  *     mage.alter(_.spellbook.+, _.familiar.-)
	  * }}}
	  *
	  * This method and [[net.noresttherein.oldsql.sql.ast.LooseComponent.alter alter]] are equivalent.
	  */
	@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
	def +-(adjustments :M[F] => ComponentSelection[_, F]*) :LooseComponent[F, M, V] =
		alter(adjustments:_*)

	/** Applies all given functions to this mapping and creates an SQL expression representing this mapping
	  * with returned components marked for being included in whatever database operation this expression
	  * is going to be used in. The [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] of the components
	  * are not validated for allowing them to be used in such an operation - as the operation type is not known
	  * at this point - which will result in the include directive being ignored.
	  *
	  * If any of the listed components is present on
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.excludes excludes]] list of this expression,
	  * it is removed.
	  */
	@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
	def include(components :M[F] => RefinedMapping[_, F]*) :LooseComponent[F, M, V] = {
		val newIncludes = components.view.map(_(mapping)).map(mapping.export(_)).to(Unique)
		new LooseComponent(mapping, offset, includes ++ newIncludes, excludes.filterNot(newIncludes.toSet))
	}

	/** Applies all given functions to this mapping and creates an SQL expression representing this mapping
	  * with returned components marked for being excluded from whatever database operation this expression
	  * is going to be used in. The [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] of the components
	  * are not validated for allowing them to be excluded from such an operation - as the operation type
	  * is not known at this point - which will result in the exclude directive being ignored.
	  *
	  * If any of the listed components is present on
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.includes includes]] list of this expression,
	  * it is removed.
	  */
	@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
	def exclude(components :M[F] => RefinedMapping[_, F]*) :LooseComponent[F, M, V] = {
		val newExcludes = components.view.map(_(mapping)).map(mapping.export(_)).to(Unique)
		new LooseComponent(mapping, offset, includes.filterNot(newExcludes.toSet), excludes ++ newExcludes)
	}

	/** Applies the given function to this mapping and creates an SQL expression representing this mapping
	  * with the returned component included by default in all permitted database operation types.
	  * This is essentially the same as
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.include include]]`(component)`,
	  * but provides syntax for adding components one by one, which is often preferable when only few components
	  * are include/excluded.
	  */
	def +(component :M[F] => RefinedMapping[_, F]) :LooseComponent[F, M, V] = {
		val cmp = mapping.export(component(mapping))
		new LooseComponent[F, M, V](mapping, offset, includes :+ cmp, excludes - cmp)
	}

	/** Applies the given function to this mapping and creates an SQL expression representing this mapping
	  * with the returned component excluded by default from all database operation types for which it is optional.
	  * This is essentially the same as
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.exclude exclude]]`(component)`,
	  * but provides syntax for adding components one by one, which is often preferable when only few components
	  * are include/excluded.
	  */
	@throws[IllegalArgumentException]("if the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column.")
	def -(component :M[F] => RefinedMapping[_, F]) :LooseComponent[F, M, V] = {
		val cmp = mapping.export(component(mapping))
		new LooseComponent[F, M, V](mapping, offset, includes - cmp, excludes :+ cmp)
	}

	/** This component with `includes` and `excludes` list cleared, making the export version of this mapping
	  * the mapping itself.
	  */
	def default :LooseComponent[F, M, V] =
		if (excludes.isEmpty && includes.isEmpty) this
		else new LooseComponent(mapping, offset)

	override def to[Y](implicit lift :Lift[V, Y]) :ComponentLValueSQL[F, M, Y] =
		if (lift == Lift.self[V]) this.asInstanceOf[ComponentLValueSQL[F, M, Y]]
		else LooseComponentConversion(this, lift)

	override def isAnchored = false
	override def isAnchored(from :F) = false

	override def anchor(from :F) :ComponentSQL[F, M] = {
		val relation = from.fullTableStack(offset).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
		relation.alter(includes, excludes) \ mapping
	}


	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :LooseComponent[E, M, V] =
		new LooseComponent[E, M, V](
			mapping.asInstanceOf[M[E]], offset + ext.lengthDiff,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
		)(projection.isomorphism[E])

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
			:LooseComponent[E, M, V] =
		new LooseComponent[E, M, V](mapping.asInstanceOf[M[E]], offset + ev.length,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]],
			excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(projection.isomorphism[E])

	override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :LooseComponent[E, M, V] =
		new LooseComponent[E, M, V](mapping.asInstanceOf[M[E]], offset + expansion.length,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]],
			excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(projection.isomorphism[E])


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ExpressionVisitor[F, Y]) :Y[GlobalScope, M[F]#Subject] =
		visitor.looseComponent(this)


	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectMapping[E, M, V] =
		SelectSQL(from, anchor(from))

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectMapping[B, from.type, M, V] =
		SelectSQL.subselect(from, anchor(from))

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:SelectMapping[P, M] =
		Select(from)[M, V](anchor(from.generalized))


	@throws[UnsupportedOperationException]("if this expression is not a ColumnSQL.")
	override def split(implicit scope :OperationType) :Seq[ColumnSQL[F, GlobalScope, _]] =
		throw new UnsupportedOperationException(
			"Not anchored component " + this + " has an undetermined number of effective columns."
		)

	override def columnCount(implicit spelling :SQLSpelling) :Int =
		throw new UnsupportedOperationException(
			"Not anchored component " + this + " has an undetermined number of effective columns."
		)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		throw new UnsupportedOperationException(
			"Not anchored component " + this + " cannot be spelled. This is likely a bug."
		)

	protected override def explodedSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		defaultSpelling(from, context, params)::Nil


	override def isomorphic(expression :SQLExpression.*) :Boolean = equals(expression)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[LooseComponent.* @unchecked]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true

		case free :LooseComponent.* @unchecked if free canEqual this =>
			free.mapping == mapping && free.offset == offset &&
				includes.toSet == free.includes.toSet && excludes.toSet == free.excludes.toSet

		case _ => false
	}

	override def hashCode :Int = mapping.hashCode * 31 + offset.hashCode


	override def toString :String = //not a lazy val as most likely we will never print this (outside of debugging)
		if (includes.isEmpty && excludes.isEmpty)
			"_#" + offset + "." + mapping.mappingName
		else {
			val alterations = includes.view.map("+" + _) ++ excludes.view.map("-" + _)
			alterations.mkString(s"_#$offset.${mapping.mappingName}(", ", ", ")")
		}

	protected override def all_subclasses_of_ComponentLValueSQL_must_extend_ComponentSQL_or_LooseComponent :Nothing =
		throw new UnsupportedOperationException
}






object LooseComponent {

	private[oldsql] def apply[F <: RowProduct, C <: Mapping, V]
	                         (mapping :C, offset :Int)
	                         (implicit cast :C <:< MappingAt[F], project :OriginProjection[C, V])
			:LooseComponent[F, project.WithOrigin, V] =
		project[F](mapping) match {
			case column :ColumnMapping[V @unchecked, F @unchecked] =>
				implicit val projection = OriginProjection.isomorphism[MappingOf[V]#ColumnProjection, V, F]
				new LooseColumn[F, MappingOf[V]#ColumnProjection, V](column, offset)
					.asInstanceOf[LooseComponent[F, project.WithOrigin, V]]
			case component =>
				new LooseComponent[F, project.WithOrigin, V](component, offset)(project.isomorphism)
		}

	def apply[F <: RowProduct, C <: Mapping, V]
	         (mapping :C) //TableCount, not TableOffset, because M is likely a component, not a table
	         (implicit cast :C <:< MappingAt[F], offset :TableCount[F, _ <: Numeral],
	                   project :OriginProjection[C, V])
			:LooseComponent[F, project.WithOrigin, V] =
			apply(mapping, offset.offset)


	def unapply[F <: RowProduct, X](expr :SQLExpression[F, _, X]) :Opt[(BaseMapping[X, _ >: F <: RowProduct], Int)] =
		expr match {
			case free: LooseComponent.Typed[F, X] @unchecked => Got(free.mapping -> free.offset)
			case _ => Lack
		}

	def unapply[F <: RowProduct, M[A] <: BaseMapping[X, A], X](expr :LooseComponent[F, M, X]) :Opt[(M[F], Int)] =
		Got(expr.mapping -> expr.offset)


	type * = LooseComponent[F, M, V] forSome {
		type F <: RowProduct; type M[A] <: BaseMapping[V, A]; type V
	}

	private type AnyIn[-F <: RowProduct] = LooseComponent[O, M, V] forSome {
		type O >: F <: RowProduct; type M[A] <: BaseMapping[V, A]; type V
	}

	private type Typed[-F <: RowProduct, V] = LooseComponent[O, M, V] forSome {
		type O >: F <: RowProduct; type M[A] <: BaseMapping[V, A]
	}



	/** A conversion applying type promotion to a
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]. Together with
	  * the wrapped type itself, anchored [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  * and its conversion
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentConversion ComponentConversion]]
	  * they form the close set of direct subtypes of (sealed)
	  * [[net.noresttherein.oldsql.sql.ast.ComponentLValueSQL ComponentLValueSQL]]
	  * used as the ''l-value'' of of component assignment
	  * [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]], guaranteeing that the underlying
	  * expression is a component, one way or another. This class does not extend
	  * [[net.noresttherein.oldsql.sql.ast.ConversionSQL.PromotionConversion PromotionConversion]] only because
	  * the latter is a class, preventing suitable multiple inheritance by
	  * [[net.noresttherein.oldsql.sql.ast.LooseColumn.LooseColumnConversion LooseColumnConversion]].
	  */
	sealed class LooseComponentConversion[F <: RowProduct, M[O] <: BaseMapping[S, O], S, V]
	                                     (override val value :LooseComponent[F, M, S], override val lift :Lift[S, V])
		extends BaseComponentConversion[F, M, V](value, lift)
	{
		override def component :ComponentSQL[F, M] =
			throw new UnsupportedOperationException(
				s"A not anchored LooseComponent $this cannot be updated as its export version is unknown."
			)

		override def to[Y](implicit lift :Lift[V, Y]) :ComponentLValueSQL[F, M, Y] =
			LooseComponentConversion(value, this.lift andThen lift)

		protected override def all_subclasses_of_ComponentLValueSQL_must_extend_ComponentSQL_or_LooseComponent :Nothing =
			throw new UnsupportedOperationException
	}


	object LooseComponentConversion {
		def apply[F <: RowProduct, M[O] <: BaseMapping[S, O], S, V]
		         (component :LooseComponent[F, M, S], lift :Lift[S, V]) :LooseComponentConversion[F, M, S, V] =
			new LooseComponentConversion(component, lift)

		def unapply[F <: RowProduct, M[O] <: BaseMapping[S, O], S, V](lvalue :ComponentLValueSQL[F, M, V])
				:Opt[(LooseComponent[_ >: F <: RowProduct, M, S], Lift[S, V])] =
			lvalue match {
				case set :LooseComponentConversion[_, M, S, V] @unchecked =>
					Got((set.value, set.lift))
				case _ => Lack
			}

		def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, V])
				:Opt[(LooseComponent[F, m, s], Lift[s, V]) forSome { type m[O] <: BaseMapping[s, O]; type s }] =
			e match {
				case set :LooseComponentConversion[F, MappingOf[Any]#ColumnProjection, Any, V] @unchecked =>
					Got((set.value, set.lift))
				case _ => Lack
			}


		type * = ComponentConversion[_ <: RowProduct, M, _] forSome { type M[O] <: MappingAt[O] }
	}



	trait LooseComponentVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends LooseColumnVisitor[F, Y] {
		def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[X, A], X]
		                  (e :LooseComponent[O, M, X]) :Y[GlobalScope, X]
	}

	trait MatchLooseComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends LooseComponentVisitor[F, Y] {
		override def looseComponent[O >: F <: RowProduct, M[A] <: ColumnMapping[V, A], V]
		                           (e :LooseColumn[O, M, V]) :Y[GlobalScope, V] =
			looseComponent(e :LooseComponent[O, M, V])
	}

	type CaseLooseComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = MatchLooseComponent[F, Y]
}






/** An expression evaluating to a column mapping of an undetermined at this point relation.
  * It can be used as a part of other expressions, but any expression containing loose components must
  * be [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] before actual SQL is generated.
  * This is done automatically for any expression used as a ''select'' clause in any of the factory methods in
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL$ SelectSQL]], as well as for all expressions created/passed
  * to [[net.noresttherein.oldsql.sql.RowProduct.where where]] method of
  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] and as its relatives.
  *
  * There is an implicit conversion from `C[O] <: `[[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[S, O]`
  * to `LooseColumn[O, C, S]`, providing `O <: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
  * See more about its details and guarantees in the documentation of the
  * [[net.noresttherein.oldsql.sql.ast.LooseComponent base class]] of this class.
  */
class LooseColumn[F <: RowProduct, M[A] <: ColumnMapping[V, A], V] private[ast]
                 (column :M[F], shift :Int,
                  incl :Unique[RefinedMapping[_, F]], excl :Unique[RefinedMapping[_, F]])
                 (implicit project :IsomorphicProjection[M, V, F])
	extends LooseComponent[F, M, V](column, shift, incl, excl) with ColumnLValueSQL[F, M, V]
{
	def this(column :M[F], shift :Int)(implicit project :IsomorphicProjection[M, V, F]) =
		this(column, shift, Unique.single(column), Unique.empty)

	//this should really use mapping.selectForm, bot it is not a ColumnForm due to possible buffs
	//doesn't really matter though, as this class is a placeholder and the form will never get used.
	override def readForm :ColumnReadForm[V] = mapping.form
	override def form :ColumnForm[V] = mapping.form

	//fixme: should return LooseComponent as it can be empty
	//fixme: Adapter column with subcolumns support
	private def include(components :Iterable[RefinedMapping[_, F]]) :LooseColumn[F, M, V] = this

	private def exclude(components :Iterable[RefinedMapping[_, F]]) :LooseColumn[F, M, V] =
		if (components.isEmpty) this
		else throw new UnsupportedOperationException(
			s"A column expression $this cannot be excluded from itself: $components."
		)

	override def include(components :M[F] => RefinedMapping[_, F]*) :LooseColumn[F, M, V] = this

	override def exclude(components :M[F] => RefinedMapping[_, F]*) :LooseColumn[F, M, V] =
		if (components.isEmpty) this
		else exclude(components.view.map(_(mapping)))

	/** Applies the given function to this column, which must return a column with the same ''export'' version
	  * as this column, and creates an SQL expression representing this column with the returned column included
	  * by default in all permitted database operation types. This is essentially the same as
	  * [[net.noresttherein.oldsql.sql.ast.LooseColumn.include include]]`(component)`,
	  * but provides syntax for adding components one by one, which is often preferable when only few components
	  * are include/excluded.
	  */
	override def +(component :M[F] => RefinedMapping[_, F]) :LooseColumn[F, M, V] = this

	/** Throws an [[UnsupportedOperationException]] - a column cannot be excluded from itself. */
	@throws[UnsupportedOperationException]("Always - column expressions cannot exclude their columns.")
	override def -(component :M[F] => RefinedMapping[_, F]) :LooseColumn[F, M, V] =
		throw new UnsupportedOperationException(
			s"A column expression $this cannot be excluded from itself: ${component(mapping)}."
		)

	override def default :LooseColumn[F, M, V] =
		if (includes.isEmpty && excludes.isEmpty) this
		else new LooseColumn(mapping, offset)

	override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
		if (lift == Lift.self[V]) this.asInstanceOf[ColumnLValueSQL[F, M, Y]]
		else LooseColumnConversion(this, lift)

	override def anchor(from :F) :ColumnComponentSQL[F, M, V] = {
		val relation = from.fullTableStack(offset).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
		relation.include(includes) \ mapping
	}

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :LooseColumn[E, M, V] =
		new LooseColumn[E, M, V](column.asInstanceOf[M[E]], offset + ext.lengthDiff,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(
			projection.isomorphism
		)

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
			:LooseColumn[E, M, V] =
		new LooseColumn[E, M, V](column.asInstanceOf[M[E]], offset + ev.length,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(
			projection.isomorphism
		)

	override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :LooseColumn[E, M, V] =
		new LooseColumn[E, M, V](column.asInstanceOf[M[E]], offset + expansion.length,
			includes.asInstanceOf[Unique[RefinedMapping[_, E]]],
			excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(projection.isomorphism
		)


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ColumnVisitor[F, Y]) :Y[GlobalScope, V] =
		visitor.looseComponent[F, M, V](this)

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

	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectColumnMapping[E, M, V] =
		SelectSQL(from, anchor(from))

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
			:SubselectColumnMapping[B, from.type, M, V] =
		SelectSQL.subselect(from, anchor(from))

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:SelectMapping[P, M] =
		Select(from)[M, V](anchor(from.generalized))

}






object LooseColumn {

	private[oldsql] def apply[F <: RowProduct, C <: ColumnMapping[_, _], V]
	                         (column :C, shift :Int)
	                         (implicit cast :C <:< ColumnMapping[V, F],
	                                   project :OriginProjection[C, V] { type WithOrigin[O] <: ColumnMapping[V, O] })
			:LooseColumn[F, project.WithOrigin, V] =
		new LooseColumn[F, project.WithOrigin, V](project[F](column), shift)(project.isomorphism)


	def apply[F <: RowProduct, C <: ColumnMapping[_, _], V]
	         (column :C) //TableCount, not TableOffset, because M is likely a component, not a table
	         (implicit cast :C <:< ColumnMapping[V, F], shift :TableCount[F, _ <: Numeral],
	                   project :OriginProjection[C, V] { type WithOrigin[O] <: ColumnMapping[V, O] })
			:LooseColumn[F, project.WithOrigin, V] =
		apply(column, shift.offset)


	def unapply[F <: RowProduct, X](expr :SQLExpression[F, _, X])
			:Opt[(ColumnMapping[X, _ >: F <: RowProduct], Int)] =
		expr match {
			case free: LooseColumn.Typed[F, X] @unchecked => Got(free.mapping -> free.offset)
			case _ => Lack
		}

	def unapply[F <: RowProduct, M[A] <: BaseMapping[X, A], X](expr :LooseComponent[F, M, X]) :Opt[(M[F], Int)] =
		(expr :LooseComponent.*) match {
			case _ :LooseColumn.* @unchecked => Got(expr.mapping -> expr.offset)
			case _ => Lack
		}


	type * = LooseColumn[_ <: RowProduct, M, V] forSome { type M[A] <: ColumnMapping[V, A]; type V }

	type AnyIn[-F <: RowProduct] = LooseColumn[O, M, V]
			forSome { type O >: F <: RowProduct; type M[A] <: ColumnMapping[V, A]; type V }

	type Typed[-F <: RowProduct, V] = LooseColumn[O, M, V]
			forSome { type O >: F <: RowProduct; type M[A] <: ColumnMapping[V, A] }


	/** A conversion applying a type promotion
	  * to a [[net.noresttherein.oldsql.sql.ast.LooseColumn LooseColumn]] instance
	  * in the process of the type unification with another SQL expression of a type automatically converted
	  * by the DBMS to/from the column type. Used in [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]]
	  * to represent setting of the value of a table column.
	  */
	class LooseColumnConversion[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
	                                    (override val value :LooseColumn[F, M, S], lift :Lift[S, V])
		extends LooseComponentConversion[F, M, S, V](value, lift) with BaseColumnComponentConversion[F, M, S, V]
	{
		override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
			LooseColumnConversion(value, this.lift andThen lift)
	}


	object LooseColumnConversion {
		def apply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
		         (column :LooseColumn[F, M, S], lift :Lift[S, V]) :LooseColumnConversion[F, M, S, V] =
			new LooseColumnConversion(column, lift)


		def unapply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V](lvalue :ComponentLValueSQL[F, M, V])
				:Opt[(LooseColumn[F, M, S], Lift[S, V])] =
			lvalue match {
				case set :LooseColumnConversion[F, M, S, V] @unchecked => Got((set.value, set.lift))
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



	trait LooseColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def looseComponent[O >: F <: RowProduct, M[A] <: ColumnMapping[V, A], V](e :LooseColumn[O, M, V])
		       :Y[GlobalScope, V]
	}

	type MatchLooseColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = LooseColumnVisitor[F, Y]

	type CaseLooseColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = LooseColumnVisitor[F, Y]
}





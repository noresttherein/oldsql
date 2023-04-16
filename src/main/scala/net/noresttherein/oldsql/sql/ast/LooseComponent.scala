package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{InseparableExpressionException, NoSuchComponentException, UndefinedShapeException}
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, ColumnReadForm, Mapping, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.sql.{Adjoin, ColumnSetter, ColumnSQL, RowProduct, RowShape, Seal, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, GroundRow, NonEmptyRow, PartOf, PrefixOf, SubselectOf, TopRow}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.ConvertedColumnLValue
import net.noresttherein.oldsql.sql.ast.LValueSQL.{ConvertedLValueSQL, LValueSQLTemplate, TypedLValueSQLTemplate}
import net.noresttherein.oldsql.sql.ast.LooseColumn.{AnyLooseColumnVisitor, SpecificLooseColumnVisitor}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.mechanics.{=~=, AlignableColumns, RelationCount, SpelledSQL, SQLConversion}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






//All these classes would benefit with being parameterized with TypedMapping and kin instead of BaseMapping,
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
  * [[net.noresttherein.oldsql.sql.mechanics.RelationCount TableCount]]; it assumes that mapping `M[F]` is
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
  * @tparam F $F
  * @tparam M $M
  * @tparam V $V
  * @define V    The [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type of the represented component
  *              and the value type of this expression.
  * @define this free component
  * @define Cons `LooseComponent`
  * @define link [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]
  */ //todo: rename to FreeComponent
class LooseComponent[F <: RowProduct, M[A] <: BaseMapping[V, A], V] private[ast]
                    (override val mapping :M[F], val offset :Int,
                     val includes :Unique[TypedMapping[_, F]], val excludes :Unique[TypedMapping[_, F]])
                    (implicit val projection :IsomorphicProjection[M, V, F])
	extends LValueSQL[F, M, V]
	   with TypedLValueSQLTemplate[F, M, V, ({ type E[f <: RowProduct] = LValueSQL[f, M, V] })#E, LValueSQL[F, M, V]]
	   with SelectableMappingSQL[F, M] //No LValueTemplate because anchoring produces a ComponentSQL.
//	   with LValueTemplate[F, M, V, ({ type E[f <: RowProduct] = LooseComponent[f, M, V] })#E, LooseComponent[F, M, V]]
{
	def this(mapping :M[F], offset :Int)(implicit projection :IsomorphicProjection[M, V, F]) =
		this(mapping, offset, Unique.empty, Unique.empty)

	includes.find(!mapping.contains(_)) match {
		case Some(c) => throw new IllegalArgumentException(
			s"Included mapping $c is not a component of $mapping: " +
				s"cannot create a LooseComponent($mapping, $offset, $includes, $excludes)."
		)
		case _ =>
	}
	excludes.find(!mapping.contains(_)) match {
		case Some(c) => throw new IllegalArgumentException(
			s"Excluded mapping $c is not a component of $mapping: " +
				s"cannot create a LooseComponent($mapping, $offset, $includes, $excludes)."
		)
		case _ =>
	}

	override def conversion :SQLConversion[V, V] = SQLConversion.toSelf
	override def selectForm :SQLReadForm[V] = mapping.selectForm
	protected override def form       :SQLForm[V] = selectForm <> mapping.writeForm(mapping.selectedByDefault)

	override def component :Nothing =
		throw new UnsupportedOperationException(
			"A non anchored LooseComponent " + this + " cannot be updated as its export version is unknown."
		)
	override val export   = mapping
	override val anchored = mapping

	override type Origin = F

	override def \[K <: MappingAt[F], X](component :K)(implicit project :OriginProjection[K, X])
			:LooseComponent[F, project.WithOrigin, X] { type Origin = F } =
	{
		val projected = component.withOrigin[F]
		if (!mapping.contains(projected))
			throw new NoSuchComponentException(mapping, projected)
		LooseComponent(component, offset,
			includes.view.map(_.original).filter(projected.contains).to(Unique),
			excludes.view.map(_.original).filter(projected.contains).to(Unique)
		)
	}

	override def \[K <: ColumnAt[F], X]
	              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
			:LooseColumn[F, project.WithOrigin, X] { type Origin = F } =
		if (!mapping.contains(column.withOrigin[F]))
			throw new NoSuchComponentException(mapping, column)
		else
			LooseColumn(column, offset)

	/** A component expression is ''default'' ''iff'' it does not alter the column set of the represented mapping `M`,
	  * i.e., [[net.noresttherein.oldsql.sql.ast.LooseComponent.includes includes]] and
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.excludes excludes]] lists are empty.
	  * A default `LooseComponent` can still become non-default
	  * after [[net.noresttherein.oldsql.sql.ast.LooseComponent.anchor anchoring]], but a non-default one
	  * will always remain as such.
	  */
	override def isDefault :Boolean = includes.isEmpty && excludes.isEmpty

	/** This component with `includes` and `excludes` list cleared, making the export version of this mapping
	  * the mapping itself.
	  */
	override def default :LooseComponent[F, M, V] =
		if (excludes.isEmpty && includes.isEmpty) this
		else new LooseComponent(mapping, offset)

	override def defaultWith(includes :Unique[TypedMapping[_, F]], excludes :Unique[TypedMapping[_, F]])
			:LooseComponent[F, M, V] =
		new LooseComponent(mapping, offset, includes.map(mapping.export(_)), excludes.map(mapping.export(_)))

	//commented out due to overloading clash unless all overloaded method variants are overriden
/*
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
	override def include(components :M[F] => TypedMapping[_, F]*) :LooseComponent[F, M, V] =
		super.include(components :_*)
	{
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
	override def exclude(components :M[F] => TypedMapping[_, F]*) :LooseComponent[F, M, V] =
		super.exclude(components:_*)
	{
		val newExcludes = components.view.map(_(mapping)).map(mapping.export(_)).to(Unique)
		new LooseComponent(mapping, offset, includes.filterNot(newExcludes.toSet), excludes ++ newExcludes)
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
	  *     Mages select _.alter(+_.spellbook, -_.familiar)
	  * }}}
	  *
	  * This method and [[net.noresttherein.oldsql.sql.ast.LooseComponent.+- +-]] method are equivalent.
	  */
	@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
	override def alter(adjustments :M[F] => ComponentSelection[_, F]*) :LooseComponent[F, M, V] =
		this +- adjustments
*/

	override def alter(includes :Iterable[TypedMapping[_, F]], excludes :Iterable[TypedMapping[_, F]])
			:LooseComponent[F, M, V] =
		new LooseComponent[F, M, V](mapping, offset,
			(this.includes.view ++ includes).filterNot(excludes.toSet).to(Unique),
			(this.excludes.view.filterNot(includes.toSet) ++ excludes).to(Unique)
		)(projection)

	/** Applies the given function to this mapping and creates an SQL expression representing this mapping
	  * with the returned component included by default in all permitted database operation types.
	  * This is essentially the same as
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent.include include]]`(component)`,
	  * but provides syntax for adding components one by one, which is often preferable when only few components
	  * are include/excluded.
	  */
	@throws[IllegalArgumentException]("if the given component is not a component of this mapping.")
	override def +(component :M[F] => TypedMapping[_, F]) :LooseComponent[F, M, V] = {
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
	@throws[IllegalArgumentException]("if the given component is not a component of this mapping.")
	@throws[UnsupportedOperationException]("if this component is a column.")
	override def -(component :M[F] => TypedMapping[_, F]) :LooseComponent[F, M, V] = {
		val cmp = mapping.export(component(mapping))
		new LooseComponent[F, M, V](mapping, offset, includes - cmp, excludes :+ cmp)
	}


	override def substitute[E <: F](substitutes :Iterable[ColumnSetter[FromLast, E]])
//			:MappingSQL[E, Single, M, Subject] =
			:MappingSQL[E, Single, M, Subject] { type Origin = F } =
		EditedLooseComponent(this)(substitutes.toSeq)


	protected override def convert[Y](conversion :SQLConversion[V, Y]) :LValueSQL[F, M, Y] =
		ConvertedLValueSQL[F, M, Y](this, conversion)

	override def isAnchored = false
	override def isAnchored(from :F) = false

	private def anchor[E <: RowProduct](from :E, offset :Int) :ComponentSQL[E, M] = {
		val relation = from.fullTableStack(offset).asInstanceOf[
			RelationSQL[E, MappingOf[Any]#TypedProjection, Any, RowProduct Adjoin MappingOf[Any]#TypedProjection]
		]
		relation.alter(includes.withOrigin[E], excludes.withOrigin[E]) \ mapping.withOrigin[E]
	}

	override def anchor(from :F) :ComponentSQL[F, M] = anchor(from, offset)

	@throws[UnsupportedOperationException]("Always.")
	override def basedOn[E <: RowProduct](implicit subtype :E <:< F) :Nothing =
		throw new UnsupportedOperationException("LooseComponent cannot be downcasted to another clause type.")

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :ComponentSQL[E, M] =
		anchor(base, offset + expansion.lengthDiff)

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
			:ComponentSQL[E, M] =
		anchor(base, offset + expansion.length)

	override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :LooseComponent[E, M, V] =
		new LooseComponent[E, M, V](mapping.asInstanceOf[M[E]], offset + expansion.length,
			includes.asInstanceOf[Unique[TypedMapping[_, E]]],
			excludes.asInstanceOf[Unique[TypedMapping[_, E]]])(projection.isomorphism[E])

	override def asIn[E <: RowProduct](implicit expansion :F PrefixOf E) :LooseComponent[E, M, V] =
		LooseComponent(mapping.withOrigin[E], offset + expansion.lengthDiff)

	override def asLast :LooseComponent[FromLast, M, V] = LooseComponent(mapping.withOrigin[FromLast], 0)


//	override def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, GlobalScope, V] =
//		throw new UnsupportedOperationException(
//			"A LooseComponent " + this + " has undefined columns and cannot be reordered."
//		)

	override type isSelectable = true

	override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E) :TopSelectAs[M] =
		anchor(from) topSelectFrom from

	override def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :SubselectAs[B, M] =
		anchor(from) subselectFrom[B] from

//	override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectAs[B, M] =
//		anchor(from) subselectFrom from
//
	override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
			:SelectMapping[P, M] =
		anchor(from) paramSelectFrom from



	override def canExclude(component :TypedColumn[_, F])(implicit spelling :SQLSpelling) :Boolean =
		throw new UndefinedShapeException("Not anchored component `" + this + "` has undefined effective column set.")

	override def canInclude(component :TypedColumn[_, F])(implicit spelling :SQLSpelling) :Boolean =
		isIncluded(component)

	override def isIncluded(component :TypedColumn[_, F])(implicit spelling :SQLSpelling) :Boolean =
		(component == mapping) || undefinedColumns

	override def columns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, F]] =
		spelling.scope.applicableColumns(mapping)

	override def defaultColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, F]] =
		undefinedColumns

	override def mandatoryColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, F]] =
		mapping.columns.filter(spelling.scope.isMandatory)

	override def columns(component :MappingAt[F])(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, F]] =
		spelling.scope.applicableColumns(mapping, component)

	override def defaultColumns(component :MappingAt[F])(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, F]] =
		component match {
			case column :TypedColumn[_, F] @unchecked => Unique.single(column)
			case _ => undefinedColumns
		}

	override def mandatoryColumns(component :MappingAt[F])(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, F]] =
		defaultColumns(component)

	@throws[UndefinedShapeException]("if this expression is not a LooseColumn.")
	protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :LValueSQL[F, M, V] =
		undefinedColumns

	@throws[InseparableExpressionException]("if this expression is not a ColumnSQL.")
	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnLValueSQL.from[F]#__] =
		throw new InseparableExpressionException(
			this, "Not anchored component `" + this + "` has an undetermined number of effective columns."
		)

	protected override def mappingForm(implicit spelling :SQLSpelling) :SQLForm[V] = undefinedColumns

//	@throws[UnsupportedOperationException]("if this expression is not a ColumnSQL.")
	@throws[UndefinedShapeException]("if this expression is not a LooseColumn.")
	protected override def shape(implicit spelling :SQLSpelling) :RowShape =
		undefinedColumns

	@throws[UndefinedShapeException]("if this expression is not a LooseColumn.")
//	@throws[UnsupportedOperationException]("if this expression is not a ColumnSQL.")
	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		throw new UndefinedShapeException(
			"Not anchored component `" + this + "` has an undetermined number of effective columns."
		)

	//note that the number of parameters doesn't need to be zero because the underlying relation might be a ParamSQL
	@throws[UndefinedShapeException]("if this expression is not a LooseColumn.")
	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		throw new UndefinedShapeException(
			"Not anchored component `" + this + "` has an undetermined number of JDBC parameters."
		)

	private def undefinedColumns =
		throw new UndefinedShapeException("Not anchored component `" + this + "` has an undefined effective column set.")

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		anchor(from).defaultSpelling(spelling)(from, context, params)

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		anchor(from).explodedSpelling(spelling, independent)(from, context, params)


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, M[F]#Subject] =
		visitor.looseComponent(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, V, Y]) :Y = visitor.looseComponent(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//	                             E >: LValueSQL[F_, M, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.looseComponent(this)

	override def isomorphic(expression :SQLExpression.__) :Boolean = equals(expression)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[LooseComponent.__ @unchecked]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true

		case free :LooseComponent.__ @unchecked if free canEqual this =>
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

	protected[sql] override
	def all_subclasses_of_LValueSQL_must_extend_TypedComponentSQL_or_LooseComponent(seal :Seal) :Unit = ()
}






object LooseComponent {

//	private[oldsql] def apply[F <: RowProduct, C <: Mapping, V]
//	                         (mapping :C, offset :Int)(implicit cast :C <:< MappingAt[F], project :OriginProjection[C, V])
//			:LooseComponent[F, project.WithOrigin, V] =
//		LooseComponent(mapping, offset, Unique.empty[TypedMapping[_, F]], Unique.empty[TypedMapping[_, F]])

	private[oldsql] def apply[F <: RowProduct, C <: Mapping, V]
	                         (mapping :C, offset :Int,
	                          includes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]],
	                          excludes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]])
	                         (implicit cast :C <:< MappingAt[F], project :OriginProjection[C, V])
			:LooseComponent[F, project.WithOrigin, V] =
		project[F](mapping) match {
			case column :BaseColumn[V, F] @unchecked =>
				implicit val projection = OriginProjection.isomorphism[MappingOf[V]#TypedColumnProjection, V, F]
				new LooseColumn[F, MappingOf[V]#TypedColumnProjection, V](column, offset)
					.asInstanceOf[LooseComponent[F, project.WithOrigin, V]]
			case component =>
				new LooseComponent[F, project.WithOrigin, V](component, offset, includes, excludes)(project.isomorphism)
		}

	def apply[F <: RowProduct, C <: Mapping, V]
	         (mapping :C) //TableCount, not TableOffset, because M is likely a component, not a table
	         (implicit cast :C <:< MappingAt[F], position :RelationCount[F, _ <: Numeral],
	          project :OriginProjection[C, V])
			:LooseComponent[F, project.WithOrigin, V] =
			apply(mapping, position.offset - 1)


	def unapply[F <: RowProduct, X](expr :SQLExpression[F, _, X]) :Opt[(BaseMapping[X, _ >: F <: RowProduct], Int)] =
		expr match {
			case free: LooseComponent.Typed[F, X] @unchecked => Got(free.mapping -> free.offset)
			case _ => Lack
		}

	def unapply[F <: RowProduct, M[A] <: BaseMapping[X, A], X](expr :LooseComponent[F, M, X]) :Opt[(M[F], Int)] =
		Got(expr.mapping -> expr.offset)


	type __ = LooseComponent[F, M, V] forSome {
		type F <: RowProduct; type M[A] <: BaseMapping[V, A]; type V
	}

//	private type AnyIn[-F <: RowProduct] = LooseComponent[O, M, V] forSome {
//		type O >: F <: RowProduct; type M[A] <: BaseMapping[V, A]; type V
//	}

	private type Typed[-F <: RowProduct, V] = LooseComponent[O, M, V] forSome {
		type O >: F <: RowProduct; type M[A] <: BaseMapping[V, A]
	}



	trait SpecificLooseComponentVisitor[+F <: RowProduct, V, +R] extends SpecificLooseColumnVisitor[F, V, R] {
		def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[V, A]](e :LooseComponent[O, M, V]) :R
	}
	trait MatchSpecificLooseComponent[+F <: RowProduct, V, +R] extends SpecificLooseComponentVisitor[F, V, R] {
		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A]](e :LooseColumn[O, M, V]) :R =
			looseComponent(e :LooseComponent[O, M, V])
	}
	type CaseSpecificLooseComponent[+F <: RowProduct, V, +R] = MatchSpecificLooseComponent[F, V, R]
//
//	trait LooseComponentVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends LooseColumnVisitor[F, Y]
//	{
//		def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[V, A], V]
//		                  (e :LooseComponent[O, M, V]) :Y[Single, V, LooseComponent[O, M, V]]
//	}
//	trait MatchLooseComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends LooseComponentVisitor[F, Y]
//	{
//		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
//		                        (e :LooseColumn[O, M, V]) :Y[Single, V, LooseColumn[O, M, V]] =
//			looseComponent(e)
//	}
//	type CaseLooseComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		MatchLooseComponent[F, Y]

	trait AnyLooseComponentVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyLooseColumnVisitor[F, Y]
	{
		def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[X, A], X]
		                  (e :LooseComponent[O, M, X]) :Y[Single, X]
	}
	trait MatchAnyLooseComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyLooseComponentVisitor[F, Y]
	{
		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
		                           (e :LooseColumn[O, M, V]) :Y[Single, V] =
			looseComponent(e :LooseComponent[O, M, V])
	}
	type CaseAnyLooseComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = MatchAnyLooseComponent[F, Y]
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
  * @tparam F $F
  * @tparam M $M
  * @tparam V $V
  * @define this column
  * @define Cons `LooseColumn`
  * @define link [[net.noresttherein.oldsql.sql.ast.LooseColumn LooseColumn]]
  */ //todo: rename to FreeColumn
class LooseColumn[F <: RowProduct, M[A] <: BaseColumn[V, A], V] private[ast]
                 (column :M[F], shift :Int,
                  incl :Unique[TypedMapping[_, F]], excl :Unique[TypedMapping[_, F]])
                 (implicit project :IsomorphicProjection[M, V, F])
	extends LooseComponent[F, M, V](column, shift, incl, excl) with ColumnLValueSQL[F, M, V]
	   with SelectableColumnMappingSQL[F, M, V]
//	   with LValueTemplate[F, M, V, ({ type E[f <: RowProduct] = LooseColumn[f, M, V] })#E, LooseColumn[F, M, V]]
{ self =>
	def this(column :M[F], shift :Int)(implicit project :IsomorphicProjection[M, V, F]) =
		this(column, shift, Unique.single(column), Unique.empty)


	override type Origin = F
	//this should really use mapping.selectForm, bot it is not a ColumnForm due to possible buffs
	//doesn't really matter though, as this class is a placeholder and the form will never get used.
	override def selectForm :ColumnReadForm[V] = mapping.form //fixme: we should use mapping.selectForm, see ColumnMappingSQL
	override def form :ColumnForm[V] = mapping.form

	protected override def setter[R <: RowProduct, Y](rvalue :ColumnSQL[R, Single, Y])(implicit compat :V =~= Y)
			:ColumnSetter[F, R] =
		ColumnSetter[F, M, V, R, compat.Unified](to(compat.left), denullify(rvalue).to(compat.right))


	override def default :LooseColumn[F, M, V] =
		if (includes.isEmpty && excludes.isEmpty) this
		else new LooseColumn(mapping, offset)

	override def defaultWith(includes :Unique[TypedMapping[_, F]], excludes :Unique[TypedMapping[_, F]])
			:LooseColumn[F, M, V] =
		exclude(excludes)

	override def include(components :Iterable[TypedMapping[_, F]]) :LooseColumn[F, M, V] = this
	override def include(components :M[F] => TypedMapping[_, F]*) :LooseColumn[F, M, V] = this

	override def exclude(components :Iterable[TypedMapping[_, F]]) :LooseColumn[F, M, V] =
		if (components.isEmpty) this
		else throw new UnsupportedOperationException(
			s"A column expression $this cannot be excluded from itself: $components."
		)
	override def exclude(components :M[F] => TypedMapping[_, F]*) :LooseColumn[F, M, V] =
		if (components.isEmpty) this
		else exclude(components.view.map(_(mapping)))

	override def alter(includes :Iterable[TypedMapping[_, F]], excludes :Iterable[TypedMapping[_, F]])
			:LooseColumn[F, M, V] =
		exclude(excludes)

	/** Applies the given function to this column, which must return a column with the same ''export'' version
	  * as this column, and creates an SQL expression representing this column with the returned column included
	  * by default in all permitted database operation types. This is essentially the same as
	  * [[net.noresttherein.oldsql.sql.ast.LooseColumn.include include]]`(component)`,
	  * but provides syntax for adding components one by one, which is often preferable when only few components
	  * are include/excluded.
	  */
	override def +(component :M[F] => TypedMapping[_, F]) :LooseColumn[F, M, V] = this

	/** Throws an [[UnsupportedOperationException]] - a column cannot be excluded from itself. */
	@throws[UnsupportedOperationException]("Always - column expressions cannot exclude their columns.")
	override def -(component :M[F] => TypedMapping[_, F]) :LooseColumn[F, M, V] =
		throw new UnsupportedOperationException(
			s"A column expression $this cannot be excluded from itself: ${component(mapping)}."
		)

	override def substitute[E <: F](substitutes :Iterable[ColumnSetter[FromLast, E]]) //:MappingSQL[E, Single, M, V] =
			:MappingSQL[E, Single, M, V] { type Origin = F } =
	{
		def reject() =
			throw new IllegalArgumentException(
				"Attempted to substitute in " + this + " a different column than " + mapping + ": " + substitutes + "."
			)
		//todo: use EditedColumnSQL; problem: it uses the Subject type, not the rvalue type as the value type
		if (substitutes.isEmpty)
			this
		else if (substitutes.sizeIs == 1)
			if (mapping.export(substitutes.head.lvalue.mapping.refine.withOrigin[F]) == mapping)
				EditedLooseComponent[F, E, M, V](this)(substitutes.toSeq)
			else
				reject()
		else if (substitutes.forall { sub => mapping.export(sub.lvalue.mapping.refine.withOrigin[F]) == mapping }) {
			val last = substitutes.last
			EditedLooseComponent[F, E, M, V](this)(last :: Nil)
		} else
			reject()
	}

	protected override def convert[Y](conversion :SQLConversion[V, Y]) :ColumnLValueSQL[F, M, Y] =
		ConvertedColumnLValue(this, conversion)

	private def anchor[E <: RowProduct](from :E, offset :Int) :GenericColumnComponentSQL[E, M, V] = {
		val relation = from.fullTableStack(offset).asInstanceOf[
			RelationSQL[E, MappingOf[Any]#TypedProjection, Any, RowProduct Adjoin MappingOf[Any]#TypedProjection]
		]
		implicit val projection = OriginProjection[M[F], V].isomorphism[E]
		(relation.include(includes.withOrigin[E]) \ mapping.withOrigin[E])(projection)
	}

	override def anchor(from :F) :GenericColumnComponentSQL[F, M, V] = anchor(from, offset)

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :GenericColumnComponentSQL[E, M, V] =
		anchor(base, offset + expansion.lengthDiff)

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
			:GenericColumnComponentSQL[E, M, V] =
		anchor(base, offset + expansion.length)

	override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :LooseColumn[E, M, V] =
		new LooseColumn[E, M, V](column.asInstanceOf[M[E]], offset + expansion.length,
			includes.asInstanceOf[Unique[TypedMapping[_, E]]],
			excludes.asInstanceOf[Unique[TypedMapping[_, E]]])(projection.isomorphism
		)

	override def asIn[E <: RowProduct](implicit expansion :F PrefixOf E) :LooseColumn[E, M, V] =
		LooseColumn(mapping.withOrigin[E], offset + expansion.lengthDiff)

	override def asLast :LooseColumn[FromLast, M, V] = LooseColumn(mapping.withOrigin[FromLast], 0)


	override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E) :TopSelectColumnAs[M, V] =
		anchor(from) topSelectFrom(from)
//		SelectSQL(from, anchor(from))

	override def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :SubselectColumnAs[B, M, V] =
		anchor(from) subselectFrom[B] from
//		SelectSQL.subselect[B, F with SubselectOf[B], M, V](from, anchor(from))
//
//	override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectColumnAs[B, M, V] =
//		SelectSQL.subselect(from, anchor(from))

	protected override def mappingForm(implicit spelling :SQLSpelling) :ColumnForm[V] = mapping.form

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnLValueSQL[F, M, _]] = PassedArray :+ this


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[Single, V] =
		visitor.looseColumn[F, M, V](this)

	protected override def visit[R](visitor :SpecificColumnVisitor[F, Single, V, R]) :R =
		visitor.looseColumn[F, M](this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//	                             E >: ColumnLValueSQL[F_, M, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.looseColumn(this)

//	protected override def visit[Y](visitor :ColumnVisitor[F, GlobalScope, V, Y]) :Y = visitor.looseColumn(this)

	protected[sql] override
	def all_subclasses_of_ColumnLValueSQL_must_extend_TypedColumnSQL_or_LooseColumn(seal :Seal) :Unit = ()
}






object LooseColumn {

	private[oldsql] def apply[F <: RowProduct, C <: ColumnMapping, V]
	                         (column :C, offset :Int)
	                         (implicit cast :C <:< ColumnAt[F],
	                                   project :OriginProjection[C, V] { type WithOrigin[O] <: BaseColumn[V, O] })
			:LooseColumn[F, project.WithOrigin, V] =
		new LooseColumn[F, project.WithOrigin, V](project[F](column), offset)(project.isomorphism)


	def apply[F <: RowProduct, C <: ColumnMapping, V]
	         (column :C) //TableCount, not TableOffset, because M is likely a component, not a table
	         (implicit cast :C <:< ColumnAt[F], position :RelationCount[F, _ <: Numeral],
	          project :OriginProjection[C, V] { type WithOrigin[O] <: BaseColumn[V, O] })
			:LooseColumn[F, project.WithOrigin, V] =
		apply(column, position.offset - 1)


	def unapply[F <: RowProduct, X](expr :SQLExpression[F, _, X])
			:Opt[(TypedColumn[X, _ >: F <: RowProduct], Int)] =
		expr match {
			case free: LooseColumn.Typed[F, X] @unchecked => Got(free.mapping -> free.offset)
			case _ => Lack
		}

	def unapply[F <: RowProduct, M[A] <: BaseMapping[X, A], X](expr :LooseComponent[F, M, X]) :Opt[(M[F], Int)] =
		(expr :LooseComponent.__) match {
			case _ :LooseColumn.__ @unchecked => Got(expr.mapping -> expr.offset)
			case _ => Lack
		}


	type __ = LooseColumn[_ <: RowProduct, M, V] forSome { type M[A] <: TypedColumn[V, A]; type V }

	type AnyIn[-F <: RowProduct] = LooseColumn[O, M, V]
			forSome { type O >: F <: RowProduct; type M[A] <: TypedColumn[V, A]; type V }

	type Typed[-F <: RowProduct, V] = LooseColumn[O, M, V]
			forSome { type O >: F <: RowProduct; type M[A] <: TypedColumn[V, A] }



	trait SpecificLooseColumnVisitor[+F <: RowProduct, V, +R] {
		def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A]](e :LooseColumn[O, M, V]) :R
	}
	type MatchSpecificLooseColumn[+F <: RowProduct, V, +R] = SpecificLooseColumnVisitor[F, V, R]
	type CaseSpecificLooseColumn[+F <: RowProduct, V, +R]  = SpecificLooseColumnVisitor[F, V, R]
//
//	trait LooseColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
//		               (e :LooseColumn[O, M, V]) :Y[Single, V, LooseColumn[O, M, V]]
//	}
//	type MatchLooseColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		LooseColumnVisitor[F, Y]
//	type CaseLooseColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		LooseColumnVisitor[F, Y]

	trait AnyLooseColumnVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V](e :LooseColumn[O, M, V]) :Y[Single, V]
	}
	type MatchAnyLooseColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyLooseColumnVisitor[F, Y]
	type CaseAnyLooseColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyLooseColumnVisitor[F, Y]
}


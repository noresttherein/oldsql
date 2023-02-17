package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ConvertingColumnTemplate, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertingTemplate, GroundingTemplate, Grouped, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.ConvertedColumnLValue.{AnyConvertedColumnLValueVisitor, CaseAnyConvertedColumnLValue, CaseSpecificConvertedColumnLValue, SpecificConvertedColumnLValueVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.{AnyColumnLValueVisitor, ConvertedColumnLValue, SpecificColumnLValueVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.{AnyColumnMappingVisitor, ConvertedColumnMappingSQL, SpecificColumnMappingVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.ConvertedColumnMappingSQL.{AnyConvertedColumnMappingVisitor, CaseAnyConvertedColumnMapping, CaseSpecificConvertedColumnMapping, SpecificConvertedColumnMappingVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingTerm.{AnyColumnMappingTermVisitor, CaseAnyColumnMappingTerm, CaseSpecificColumnMappingTerm, SpecificColumnMappingTermVisitor}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL.SpecificConvertedColumnVisitor
import net.noresttherein.oldsql.sql.ast.EditedColumnSQL.{AnyEditedColumnVisitor, CaseAnyEditedColumn, CaseSpecificEditedColumn, SpecificEditedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.EditedLValueSQL.{AnyEditedLValueVisitor, CaseAnyEditedLValue, CaseSpecificEditedLValue, SpecificEditedLValueVisitor}
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL.{AnyColumnComponentVisitor, CaseAnyColumnComponent, CaseSpecificColumnComponent, SpecificColumnComponentVisitor}
import net.noresttherein.oldsql.sql.ast.LooseColumn.{AnyLooseColumnVisitor, CaseAnyLooseColumn, CaseSpecificLooseColumn, SpecificLooseColumnVisitor}
import net.noresttherein.oldsql.sql.ast.LValueSQL.ConvertedLValueSQL.{AnyConvertedLValueVisitor, CaseAnyConvertedLValue, CaseSpecificConvertedLValue, SpecificConvertedLValueVisitor}
import net.noresttherein.oldsql.sql.ast.LValueSQL.{AnyLValueVisitor, CaseAnyLValue, CaseSpecificLValue, ConvertedLValueSQL, SpecificLValueVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ConvertedMappingSQL, MappingSQLTemplate}
import net.noresttherein.oldsql.sql.ast.MappingSQL.ConvertedMappingSQL.{AnyConvertedMappingVisitor, CaseAnyConvertedMapping, MatchSpecificConvertedMapping, SpecificConvertedMappingVisitor}
import net.noresttherein.oldsql.sql.ast.MappingTerm.{AnyMappingTermVisitor, CaseAnyMappingTerm, CaseSpecificMappingTerm, SpecificMappingTermVisitor}
import net.noresttherein.oldsql.sql.ast.SelectAsIdSQL.{AnySelectAsIdVisitor, CaseAnySelectAsId, CaseSpecificSelectAsId, SpecificSelectAsIdVisitor}
import net.noresttherein.oldsql.sql.mechanics.SQLConversion

//here be implicits
import net.noresttherein.oldsql.slang._




//consider: it would be useful to have a UnconvertedMappingSQL (or smth) case for reforming, which would fix V = M[Unit]#Subject
/** An SQL expression AST node represented by a mapping `M`. While `M` might be a subtype of
  * [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]], typically it is a component of some relation
  * from the ''from'' clause of an SQL select containing this expression. The value type of this expression may be
  * the mapping's [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type, but it can also be some type
  * derived from it, typically either a supertype of the subject, or some wider type to which it can be promoted.
  * This trait divides all implementing classes into two groups: those representing the subject directly,
  * extending either a [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]
  * or [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], and conversions of the former expressions,
  * extending [[net.noresttherein.oldsql.sql.ast.LValueSQL.ConvertedLValueSQL LValueConversion]].
  * This affects only the type of values seen by the application, but doesn't change the SQL to which the expression
  * is translated, which will be a list of columns of mapping `M`, either inlined or presented as a tuple,
  * depending on the context in which the expression is used in the ''select''.
  * If the expression is used literally as part of a ''select'' clause (either directly, or inside
  * a [[net.noresttherein.oldsql.sql.ast.InlineSQL tuple]]), default
  * [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns (those without a
  * [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]] buff) will be inlined in its place.
  * If used as part of a comparison in a ''where'' or ''having'' clause, the columns of the two expressions will
  * be compared ordinally.
  *
  * The exact set used in the final SQL cannot be determined based solely on this expression, as it depends
  * on the mapping instance used in the relation of the actual ''from'' clause of the spelled SQL ''select'',
  * rather than the instance presented here. Nevertheless, these are typically the same, and the column set
  * can be modified by various [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.exclude exclude]] methods
  * of the main subclass, [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]. See the documentation
  * of that trait and [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] for more details on the subject.
  * @tparam F $F
  * @tparam S $S
  * @tparam M $M
  * @tparam V $V
  * @author Marcin Mościcki
  */ //consider: make the mapping covariant
trait MappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[O] <: MappingAt[O], V]
	extends SQLExpression[F, S, V]
	   with ConvertingTemplate[F, S, V, MappingSQL.from[F]#rows[S]#meta[M]#E]
	   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = MappingSQL[f, S, M, V] })#E]
	   with MappingSQLTemplate[F, S, M, V, MappingSQL[F, S, M, V]]
{
	//consider: I don't like export and anchored being here, but they are needed for polymorphism in AlignedExpression
	val export   :TypedMapping[M[Unit]#Subject, Origin]

	val anchored :TypedMapping[M[Unit]#Subject, Origin]

	def conversion :SQLConversion[M[Unit]#Subject, V]
	/* They can't be type parameters because they are later refined Origin = this.mapping.Origin.
	 * As it is, almost every class in which the narrowing of these types would take place provides implementations
	 * of \ anyway (with the exception of MappingTerm and LValueConversionSQL), so they introduce no real benefit
	 * over specifying the return types of `\` explicitly, and the latter leads to arguably more useful type inference. */
//	type ComponentLike[C[A] <: BaseMapping[X, Origin], X] <: MappingSQL[F, S, C, X]
//	type ColumnLike[C[A] <: BaseColumn[X, Origin], X] <: ColumnMappingSQL[F, S, C, X] with ComponentLike[C, X]

	/** Creates an expression for the given subcomponent of this component. The type of the returned expression
	  *  - [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  *  or [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] - will depend on the type
	  *  of this expression.
	  *  @param component a component mapping of `this.`[[net.noresttherein.oldsql.sql.ast.LValueSQL.mapping mapping]].
	  */ //todo: verify that implementations work also for columns
	@throws[NoSuchComponentException]("if component is not a component of this.mapping.")
	def \[K <: MappingAt[Origin], X]
	     (component :K)(implicit project :OriginProjection[K, X]) :MappingSQL[F, S, project.WithOrigin, X]

	/** Creates an expression for the given column of this component.
	  * The type of the returned expression
	  *  - [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  *  or [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] - will depend on the type
	  *  of this expression.
	  *  @param column a column mapping of `this.`[[net.noresttherein.oldsql.sql.ast.LValueSQL.mapping mapping]].
	  */
	@throws[NoSuchComponentException]("if column is not a column of this.mapping.")
	def \[K <: ColumnAt[Origin], X]
	     (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
			:ColumnMappingSQL[F, S, project.WithOrigin, X]

	override def groundValue :Opt[V] = Lack

	override def isGround :Boolean = false

	protected override def convert[X](conversion :SQLConversion[V, X]) :MappingSQL[F, S, M, X] =
		ConvertedMappingSQL(this, conversion)

	//consider: ColumnMappingSQL return type is problematic:
	//  1. we need ColumnMappingTerm, ColumnMappingLiteral, BoundColumnMappingParam and EditedColumnSQL
	//  1. SelectIdAs has to return the id column as a virtual ColumnMapping
	//fixme: ColumnMappingSQL return type is limiting; we can't sensibly implement SelectMappingId and probably others.
	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[F]#rows[S]#__]

	private[oldsql] override def `->split`(spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[F]#rows[S]#__] =
		split(spelling)

	override def typeString :ChunkedString = mapping.mappingName
}




object MappingSQL {

	type __ = MappingSQL[_ <: RowProduct, _ >: Grouped <: Single, M, _] forSome { type M[O] <: MappingAt[O] }

	type from[-F <: RowProduct] = {
		type __ = MappingSQL[F, _ >: Grouped <: Single, M, _] forSome { type M[O] <: MappingAt[O] }
		type rows[-S >: Grouped <: Single] = {
			type __ = MappingSQL[F, S, M, _] forSome { type M[O] <: MappingAt[O] }
			type meta[M[A] <: MappingAt[A]] = {
				type __ = MappingSQL[F, S, M, _]
				type E[V] = MappingSQL[F, S, M, V]
				type of[V] = MappingSQL[F, S, M, V]
			}
			type of[V] = MappingSQL[F, S, M, V] forSome { type M[A] <: MappingAt[A] }
		}
	}


	//todo: update docs to use the defs
	/** Mixin trait for expressions of type `E` representing a mapping `M`.
	  * @tparam F    $F
	  * @tparam S    $S
	  * @tparam V    $V
	  * @tparam Same $Same
	  * @define M    A type constructor for the mapping whose column set is represented by this expression.
	  *              The type parameter is the mapping's [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
	  * @define this mapping
	  * @define Cons `MappingSQL`
	  * @define Same The self type of the expression mixing in this template, used to return an expression
	  *              of the same type from methods like
	  *              [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.anchor anchor]] or
	  *              [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.alter alter]].
	  * @define link [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]]
	  *
	  */
	trait MappingSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, M[O] <: MappingAt[O], V,
	                         +Same <: MappingSQL[F, S, M, V]]
	/* Does not extend GroundingTemplate because
	 *   1. we are invariant in F, but E[-f <: RowProduct] = E would need to be variant, and
	 *   2. JoinedRelation subtypes cannot extend GroundingTemplate, because those methods return a basic JoinedRelation.
	 * We might simply have JoinedRelationTemplate not extend this trait directly, as all methods defined here
	 * are overridden in JoinedRelationTemplate, but it is not true for ComponentSQLTemplate.
	 */
	{ self :Same with MappingSQLTemplate[F, S, M, V, Same] =>
		/** The [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type used by this expression's mapping. */
		type Origin

		/** The subject type of `this.mapping`. */
		type Subject = M[Unit]#Subject //todo: make mapping a val and this its member type.

		/** An instance of the mapping representing the value of this expression. */
		val mapping :M[Origin]

		/** A `MappingSQL` is ''default'' ''iff'' it does not alter the default column set of the represented mapping `M`. */
		def isDefault :Boolean

		/** An expression for the default version of this mapping, that is one without any alterations
		  * to its column set applied on the level of this expression. This does not take into account the possibility
		  * of an underlying relation having an altered view.
		  */
		def default :Same //= if (isDefault) this else defaultWith(Unique.empty, Unique.empty)


		/** Discards any non-standard view (included/excluded columns) from this mapping and resets them
		  * to those specified as arguments instead. Note that mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] govern if a column is applicable
		  * (or allowed) for a particular use, and include and exclude directives specified here are ignored
		  * if not legal in the final SQL/DML.
		  *
		  * Including a component has the effect of including all its subcomponents and columns, with the exception
		  * of those which are separately marked as [[net.noresttherein.oldsql.schema.Buff.Explicit explicit]].
		  * Excluding a component takes precedence over including any of its supercomponents (including itself).
		  *
		  * If this expression is not default, then the include and exclude lists are stacked, with directives
		  * specified here taking precedence over currently existing ones. This does not however have the same semantics
		  * as calling `mapping(includes1, excludes1)(includes2, excludes2)` -
		  * the [[net.noresttherein.oldsql.sql.ast.MappingSQL.anchored anchored]] mapping of the final result
		  * will most likely not recognize components of the `anchored` mapping of this (intermediate) expression.
		  * @param includes a list of components of
		  *                 `this.`[[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.mapping mapping]]
		  *                 which should be included in the SQL for this expression, if possible.
		  * @param excludes a list of components of
		  *                 `this.`[[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.mapping mapping]]
		  *                 which should not be included in the SQL for this expression, if possible.
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a proper component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the component list is non empty.")
		def defaultWith(includes : Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]]) :Same

		/** Includes the given subcomponents in the rendering of this mapping as SQL.
		  * This represents an alteration of the effective column set on a per expression basis; when formatting SQL,
		  * this change will be applied on top of the ''export'' mapping
		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
		  * column of this component will be included depends on the buffs of its effective version after these
		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
		  * is a name of a database operation type), this declaration will be ignored, producing no error,
		  * neither in this method, nor when the expression is formatted.
		  *
		  * Note that this method has no effect on columns, as the nominal mapping is always included,
		  * even if it is [[net.noresttherein.oldsql.schema.Buff.Explicit explicit]].
		  * @param components a collection of components of the nominal
		  *                   [[net.noresttherein.oldsql.sql.ast.MappingSQL.mapping mapping]] of this instance.
		  * @see [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.+]]
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a proper component of this mapping.")
		def include(components :Iterable[TypedMapping[_, Origin]]) :Same =
			alter(Unique.empty, components)

		/** Includes the given subcomponents in the rendering of this component as SQL.
		  * Note that this method has no effect on columns, as the nominal component is always considered included,
		  * even it is optional.
		  * @param components A sequence of functions selecting components of their arguments, given
		  *                   the nominal [[net.noresttherein.oldsql.sql.ast.MappingSQL.mapping mapping]]
		  *                   of this expression.
		  * @see [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.+]]
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a proper component of this mapping.")
		def include(components :M[Origin] => TypedMapping[_, Origin]*) :Same =
			include(components.view.map(_(mapping)))

		/** Excludes the given subcomponents from the rendering of this component as SQL.
		  * This represents an alteration of the effective column set on a per expression basis; when formatting SQL,
		  * this change will be applied on top of the ''export'' mapping
		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
		  * column of this component will be excluded depends on the buffs of its effective version after these
		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
		  * is a name of a database operation type), this declaration will be ignored, producing no error,
		  * neither in this method, nor when the expression is formatted.
		  *
		  * This method will throw an [[UnsupportedOperationException]] when called with a non-empty collection
		  * on a [[net.noresttherein.oldsql.sql.ast.ColumnMappingSQL column]] expression.
		  * @param components a collection of components
		  *                   of the [[net.noresttherein.oldsql.sql.ast.MappingSQL.mapping mapping]] of this instance.
		  * @see [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.-]]
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a proper component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the component list is non empty.")
		def exclude(components :Iterable[TypedMapping[_, Origin]]) :Same = alter(Unique.empty, components)

		/** Excludes the given subcomponents from the rendering of this component as SQL.
		  * This represents an alteration of the effective column set on a per expression basis; when formatting SQL,
		  * this change will be applied on top of the ''export'' mapping
		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
		  * column of this component will be excluded depends on the buffs of its effective version after these
		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
		  * is a name of a database operation type), this declaration will be ignored, producing no error,
		  * neither in this method, nor when the expression is formatted.
		  *
		  * This method will throw an [[UnsupportedOperationException]] when called with a non-empty collection
		  * on a [[net.noresttherein.oldsql.sql.ast.ColumnMappingSQL column]] expression.
		  * @param components A sequence of functions selecting components of their arguments, given
		  *                   the nominal [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]]
		  *                   of this expression.
		  * @see [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.-]]
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a proper component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the component list is non empty.")
		def exclude(components :M[Origin] => TypedMapping[_, Origin]*) :Same =
			exclude(components.view.map(_(mapping)))

		/** Creates a view of this mapping with certain columns or subcomponents specifically included or excluded.
		  * This represents an alteration of the effective column set on a per expression basis; when formatting SQL,
		  * this change will be applied on top of the ''export'' mapping
		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
		  * column of this component will be included depends on the buffs of its effective version after these
		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
		  * is a name of a database operation type), this declaration will be ignored, producing no error,
		  * neither in this method, nor when the expression is formatted.
		  *
		  * Both argument lists must contain only components of the nominal mapping `M`; passing any other mapping,
		  * may result in throwing an [[IllegalArgumentException]].
		  * @param includes a collection of subcomponents of `this.mapping` which should be included when formatting
		  *                 this object as SQL, if allowed in the specific context in which this expression is used.
		  * @param excludes a collection of subcomponents of `this.mapping` which should be excluded when formatting
		  *                 this object as SQL, if allowed in the specific context in which this expression is used.
		  * @see [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.+-]]
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the excludes list is non empty.")
		def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]]) :Same

		/** Creates a view of this component with certain columns or subcomponents specifically included or excluded.
          * This is very similar to [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]
		  * method on any mapping, but takes a sequence of functions, instead of ready components. This makes it
		  * more convenient to call on return values of functions and methods, as the mapping `M` doesn't need
		  * to be assigned to an intermediate `val`. Example:
		  * {{{
		  *     From(Mages) selectLast _.alter(+_.spellbook, -_.familiar)
		  * }}}
		  * This alteration of the column set is on a per expression level; when formatting SQL,
		  * this change will be applied on top of the ''export'' mapping
		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
		  * column of this component will be included depends on the buffs of its effective version after these
		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
		  * is a name of a database operation type), this declaration will be ignored, producing no error,
		  * neither in this method, nor when the expression is formatted.
		  *
		  * @param components a list of functions accepting the nominal
		  *                   [[net.noresttherein.oldsql.sql.ast.MappingSQL.mapping mapping]]
		  *                   of this instance and returning its component, wrapped in either
		  *                   [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]] or
		  *                   [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]].
		  *                   These can be created by methods [[net.noresttherein.oldsql.schema.Mapping.+ +]] and
		  *                   [[net.noresttherein.oldsql.schema.Mapping.- -]] (including their unary variants),
		  *                   defined on any mapping.
		  * @see [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.+-]]
		  */ //todo: verify the overload doesn't break type inference
		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		def alter(components :M[Origin] => ComponentSelection[_, Origin]*) :Same =
			this +- components


		/** Creates a view of this mapping with certain columns or components specifically included or excluded.
		  * Applies all given functions to this mapping and creates an SQL expression representing this mapping
		  * with the components wrapped in
		  * [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]] added to the included
		  * components in this instance, and those wrapped
		  * in [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]]
		  * to the excluded components. These changes are applied on the top of the current view of the component.
		  * This is very similar to [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]
		  * method on any mapping, but takes a sequence of functions, instead of ready components. This makes it
		  * more convenient to call on return values of functions and methods, as the mapping `M` doesn't need
		  * to be assigned to an intermediate `val`. Example:
		  * {{{
		  *     mage +- Seq(+_.spellbook, -_.familiar)
		  * }}}
		  *
		  * This method and [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.alter alter]]
		  * are equivalent.
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		def +-(components :Iterable[M[Origin] => ComponentSelection[_, Origin]]) :Same = {
			val newExcludes = components.view.map(_(mapping)).collect {
				case ExcludedComponent(c) => c
			}.to(Unique)
			val newIncludes = components.view.map(_(mapping)).collect {
				case IncludedComponent(c) => c
			}.to(Unique)
			alter(newIncludes, newExcludes)
		}

		/** Includes the given subcomponent in the rendering of this mapping as SQL.
		  * This represents an alteration of the effective column set on a per expression basis; when formatting SQL,
		  * this change will be applied on top of the ''export'' mapping
		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
		  * column of this component will be included depends on the buffs of its effective version after these
		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
		  * is a name of a database operation type), this declaration will be silently ignored, producing no error,
		  * neither in this method, nor when the expression is formatted.
		  *
		  * Note that this method has no effect on columns, as the nominal component is always considered included,
		  * even it is optional.
		  * @param component A function selecting a component of
		  *                  the nominal [[net.noresttherein.oldsql.sql.ast.MappingSQL.mapping mapping]]
		  *                  of this expression, given as its argument.
		  */
		@throws[IllegalArgumentException]("if the given component is not a component of this mapping.")
		def +(component :M[Origin] => TypedMapping[_, Origin]) :Same =
			include(Unique.single(component(mapping)))

		/** Excludes the given subcomponent from the rendering of this mapping as SQL.
		  * This represents an alteration of the effective column set on a per expression basis; when formatting SQL,
		  * this change will be applied on top of the ''export'' mapping
		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
		  * column of this component will be excluded depends on the buffs of its effective version after these
		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
		  * is a name of a database operation type), this declaration will be silently ignored, producing no error,
		  * neither in this method, nor when the expression is formatted.
		  *
		  * This method will throw an [[UnsupportedOperationException]] when called with a non-empty collection
		  * on a [[net.noresttherein.oldsql.sql.ast.ColumnMappingSQL column]] expression.
		  * @param component A function selecting a component of
		  *                  the nominal [[net.noresttherein.oldsql.sql.ast.MappingSQL.mapping mapping]]
		  *                  of this expression, given as its argument.
		  */
		@throws[IllegalArgumentException]("if the given component is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column.")
		def -(component :M[Origin] => TypedMapping[_, Origin]) :Same =
			exclude(Unique.single(component(mapping)))
	}




	/** Applies a Scalaland conversion [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation SQLTransformation]]`[X, Y]`
	  * to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] of another `MappingSQL[F, S, M, X]`,
	  * without effect on the generated SQL.
	  */
	trait ConvertedMappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[A]<: MappingAt[A], X, Y]
		extends ConvertedSQL[F, S, X, Y] with MappingSQL[F, S, M, Y]
	{ self =>
		/** The adapted mapping expression. Must be either a `lazy val` or a constructor parameter `val`
		  * (it is used in initialization of `MappingConversion` trait).
		  */
		override val value      :MappingSQL[F, S, M, X]
		override val mapping    :M[Origin] = value.mapping
		override val export     :TypedMapping[M[Unit]#Subject, Origin] = value.export
		override val anchored   :TypedMapping[M[Unit]#Subject, Origin] = value.anchored
//		override val adaptation :SQLConversion[X, Y]
		override def conversion :SQLConversion[M[Unit]#Subject, Y] = value.conversion andThen adaptation
		override type Origin = value.Origin

		@throws[NoSuchComponentException]("if component is not a component of this.mapping.")
		override def \[K <: MappingAt[Origin], T]
		              (component :K)(implicit project :OriginProjection[K, T]) :MappingSQL[F, S, project.WithOrigin, X] =
			value \ component

		@throws[NoSuchComponentException]("if column is not a column of this.mapping.")
		override def \[K <: ColumnAt[Origin], T]
		              (column :K)(implicit project :OriginProjection[K, T] { type WithOrigin[A] <: BaseColumn[T, A] })
				:ColumnMappingSQL[F, S, project.WithOrigin, X] =
			value \ column

		override def isDefault :Boolean = value.isDefault
		override def default :MappingSQL[F, S, M, Y] = reapply(value.default)

		override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
				:MappingSQL[F, S, M, Y] =
			reapply(value.defaultWith(includes, excludes))

		override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
				:MappingSQL[F, S, M, Y] =
			reapply(value.alter(includes, excludes))

		override def groundValue :Opt[Y]  = value.groundValue.map(adaptation.apply)
		override def isGround    :Boolean = value.isGround

		protected override def convert[Z](conversion :SQLConversion[Y, Z]) :MappingSQL[F, S, M, Z] =
			(this.adaptation andThen conversion)(value)

		override def anchor(from :F) :MappingSQL[F, S, M, Y] = reapply(value.anchor(from))

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :MappingSQL[E, S, M, Y] =
			reapply(value.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :MappingSQL[E, S, M, Y] =
			reapply(value.expand(base))

		private def reapply[E <: RowProduct, C >: Grouped <: Single]
		                   (e :MappingSQL[E, C, M, X]) :MappingSQL[E, C, M, Y] =
			if (e eq value) this.asInstanceOf[MappingSQL[E, C, M, Y]] else adaptation(e)

		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[F]#rows[S]#__] =
			spelling.split(value)

		protected def visit[R, E >: MappingSQL[F, S, M, Y]](visitor :SpecificExpressionVisitor[F, S, Y, R]) :R =
			visitor.convertedMapping(this)
//		protected override def visit[R](visitor :ExpressionVisitor[F, S, Y, R]) :R = visitor.convertedMapping(this)
		protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, R]) :R[S, Y] =
			visitor.convertedMapping(this)
	}


	object ConvertedMappingSQL {
		def apply[F <: RowProduct, S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y]
		         (value :MappingSQL[F, S, M, X], conversion :SQLConversion[X, Y]) :ConvertedMappingSQL[F, S, M, X, Y] =
			new Impl(value, conversion)

		def unapply[F <: RowProduct, S >: Grouped <: Single, V](e :SQLExpression[F, S, V])
				:Opt[(MappingSQL[F, S, M, T], SQLConversion[T, V]) forSome { type M[A] <: MappingAt[A]; type T }] =
			e match {
				case convert :ConvertedMappingSQL[F, S, MappingAt, Any, V] @unchecked =>
					Got((convert.value, convert.adaptation))
				case _ => Lack
			}


		private class Impl[-F <: RowProduct, -S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y]
		                  (override val value :MappingSQL[F, S, M, X], override val adaptation :SQLConversion[X, Y])
			extends ConvertedMappingSQL[F, S, M, X, Y]

		type __ = ConvertedMappingSQL[_ <: RowProduct, _ >: Grouped <: Single, M, _, _] forSome {
			type M[A] <: MappingAt[A]
		}


		trait SpecificConvertedMappingVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
			extends SpecificConvertedColumnMappingVisitor[F, S, V, Y] with SpecificConvertedLValueVisitor[F, V, Y]
		{
			def convertedMapping[M[A] <: MappingAt[A], X](e :ConvertedMappingSQL[F, S, M, X, V]) :Y
		}
		trait MatchSpecificConvertedMapping[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
			extends SpecificConvertedMappingVisitor[F, S, V, Y] with CaseSpecificConvertedLValue[F, V, Y]
	    {
		    override def convertedColumnMapping[M[A] <: ColumnAt[A], X]
		                                       (e :ConvertedColumnMappingSQL[F, S, M, X, V]) :Y = convertedMapping(e)
	    }
		trait CaseSpecificConvertedMapping[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
			extends MatchSpecificConvertedMapping[F, S, V, Y]
		{
			override def convertedLValue[M[O] <: MappingAt[O]](e :ConvertedLValueSQL[F, M, V]) :Y = convertedMapping(e)
		}
//
//
//		trait ConvertedMappingVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends ConvertedColumnMappingVisitor[F, Y] with ConvertedLValueVisitor[F, Y]
//		{
//			def convertedMapping[S >: Grouped <: Single, M[A] <: MappingAt[A], X, V]
//			                    (e :ConvertedMappingSQL[F, S, M, X, V]) :Y[S, V, ConvertedMappingSQL[F, S, M, X, V]]
//		}
//		trait MatchConvertedMapping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends ConvertedMappingVisitor[F, Y] with CaseConvertedLValue[F, Y]
//		{
//			override def convertedColumnMapping[S >: Grouped <: Single, M[A] <: ColumnAt[A], X, V]
//			                                   (e :ConvertedColumnMappingSQL[F, S, M, X, V])
//					:Y[S, V, ConvertedColumnMappingSQL[F, S, M, X, V]] =
//				convertedMapping(e)
//		}
//		trait CaseConvertedMapping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends MatchConvertedMapping[F, Y]
//		{
//			override def convertedLValue[M[O] <: MappingAt[O], V]
//			                            (e :ConvertedLValueSQL[F, M, V]) :Y[Single, V, ConvertedLValueSQL[F, M, V]] =
//				convertedMapping(e)
//		}


		trait AnyConvertedMappingVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
			extends AnyConvertedColumnMappingVisitor[F, R] with AnyConvertedLValueVisitor[F, R]
		{
			def convertedMapping[S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y]
			                    (e :ConvertedMappingSQL[F, S, M, X, Y]) :R[S, Y]
		}
		trait MatchAnyConvertedMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
			extends AnyConvertedMappingVisitor[F, R] with CaseAnyConvertedLValue[F, R]
	    {
		    override def convertedColumnMapping[S >: Grouped <: Single, M[A] <: ColumnAt[A], X, Y]
		                                       (e :ConvertedColumnMappingSQL[F, S, M, X, Y]) :R[S, Y] =
			    convertedMapping(e)
	    }
		trait CaseAnyConvertedMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
			extends MatchAnyConvertedMapping[F, R]
		{
			override def convertedLValue[M[O] <: MappingAt[O], Y](e :ConvertedLValueSQL[F, M, Y]) :R[Single, Y] =
				convertedMapping(e)
		}
	}




	trait SpecificMappingVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificColumnMappingVisitor[F, S, X, Y] with SpecificConvertedMappingVisitor[F, S, X, Y]
		   with SpecificLValueVisitor[F, X, Y] with SpecificEditedLValueVisitor[F, X, Y]
		   with SpecificMappingTermVisitor[X, Y] with SpecificSelectAsIdVisitor[F, X, Y]
	{
		def mapping[M[O] <: MappingAt[O]](e :MappingSQL[F, S, M, X]) :Y
	}
	trait MatchSpecificMapping[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends SpecificMappingVisitor[F, S, X, Y]
		with CaseSpecificLValue[F, X, Y] with CaseSpecificEditedLValue[F, X, Y]
		with MatchSpecificConvertedMapping[F, S, X, Y] with CaseSpecificMappingTerm[X, Y] with CaseSpecificSelectAsId[F, X, Y]
	{
		override def columnMapping[M[O] <: ColumnAt[O]](e :ColumnMappingSQL[F, S, M, X]) :Y = mapping(e)
		override def editedColumn[M[A] <: BaseColumn[X, A], V](e :EditedColumnSQL[F, M, X, V]) :Y = mapping(e)
	}
	trait CaseSpecificMapping[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends MatchSpecificMapping[F, S, X, Y] {
		override def editedLValue[M[A] <: BaseMapping[X, A]](e :EditedLValueSQL[F, M, X]) :Y = mapping(e)
		override def lvalue[M[O] <: MappingAt[O]](e :LValueSQL[F, M, X]) :Y = mapping(e)

		override def mappingTerm[M[A] <: BaseMapping[X, A]](e :MappingTerm[M, X]) :Y = mapping(e)
		override def convertedMapping[M[A] <: MappingAt[A], V](e :ConvertedMappingSQL[F, S, M, V, X]) :Y = mapping(e)

		override def selectAsId[M[O] <: MappingAt[O]](e :SelectAsIdSQL[F, M, X]) :Y = mapping(e)
	}
//
//
//	trait MappingVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ColumnMappingVisitor[F, Y] with LValueVisitor[F, Y] with ConvertedMappingVisitor[F, Y]
//		   with EditedLValueVisitor[F, Y] with MappingTermVisitor[Y] with SelectMappingIdVisitor[F, Y]
//	{
//		def mapping[S >: Grouped <: Single, M[A] <: MappingAt[A], V]
//		           (e :MappingSQL[F, S, M, V]) :Y[S, V, MappingSQL[F, S, M, V]]
//	}
//	trait MatchMapping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MappingVisitor[F, Y] with CaseConvertedMapping[F, Y]
//		   with CaseComponent[F, Y] with CaseLooseComponent[F, Y] with CaseEditedLValue[F, Y]
//		   with CaseMappingTerm[Y] with CaseSelectMappingId[F, Y]
//	{
//		override def columnMapping[S >: Grouped <: Single, M[A] <: ColumnAt[A], V]
//		                          (e :ColumnMappingSQL[F, S, M, V]) :Y[S, V, ColumnMappingSQL[F, S, M, V]] =
//			mapping(e)
//	}
//	trait CaseMapping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchMapping[F, Y]
//	{
//		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
//		                      (e :TypedComponentSQL[O, T, R, M, V, L])
//				:Y[Single, V, TypedComponentSQL[O, T, R, M, V, L]] =
//			mapping(e)
//
//		override def convertedMapping[S >: Grouped <: Single, M[A] <: MappingAt[A], X, V]
//		                             (e :ConvertedMappingSQL[F, S, M, X, V]) :Y[S, V, ConvertedMappingSQL[F, S, M, X, V]] =
//			mapping(e)
//
//		override def editedColumn[M[A] <: BaseMapping[V, A], V, X]
//		                         (e :EditedColumnSQL[F, M, V, X]) :Y[Single, V, EditedColumnSQL[F, M, V, X]] =
//			mapping(e)
//
//		override def editedLValue[M[A] <: BaseMapping[V, A], V]
//		                         (e :EditedLValueSQL[F, M, V]) :Y[Single, V, EditedLValueSQL[F, M, V]] =
//			mapping(e)
//
//		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[V, A], V]
//		                           (e :LooseComponent[O, M, V]) :Y[Single, V, LooseComponent[O, M, V]] =
//			mapping(e)
//
//		override def mappingTerm[M[A] <: BaseMapping[V, A], V]
//		                        (e :MappingTerm[M, V]) :Y[Single, V, MappingTerm[M, V]] = mapping(e)
//
//		override def selectMappingId[M[O] <: MappingAt[O], V]
//		                           (e :SelectMappingIdSQL[F, M, V]) :Y[Single, V, SelectMappingIdSQL[F, M, V]] =
//			mapping(e)
//	}


	trait AnyMappingVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyColumnMappingVisitor[F, R] with AnyConvertedMappingVisitor[F, R]
		   with AnyLValueVisitor[F, R] with AnyEditedLValueVisitor[F, R]
		   with AnyMappingTermVisitor[R] with AnySelectAsIdVisitor[F, R]
	{
		def mapping[S >: Grouped <: Single, M[O] <: MappingAt[O], V](e :MappingSQL[F, S, M, V]) :R[S, V]
	}
	trait MatchAnyMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends AnyMappingVisitor[F, R]
		with CaseAnyLValue[F, R] with CaseAnyEditedLValue[F, R]
		with CaseAnyConvertedMapping[F, R] with CaseAnyMappingTerm[R] with CaseAnySelectAsId[F, R]
	{
		override def columnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], V]
		                          (e :ColumnMappingSQL[F, S, M, V]) :R[S, V] =
			mapping(e)
	}
	trait CaseAnyMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends MatchAnyMapping[F, R] {
		override def lvalue[M[O] <: MappingAt[O], V](e :LValueSQL[F, M, V]) :R[Single, V] =
			mapping(e)

		override def editedLValue[M[A] <: BaseMapping[S, A], S](e :EditedLValueSQL[F, M, S]) :R[Single, S] =
			mapping(e)

		override def mappingTerm[M[A] <: BaseMapping[X, A], X](e :MappingTerm[M, X]) :R[Single, X] =
			mapping(e)

		override def convertedMapping[S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y]
		                              (e :ConvertedMappingSQL[F, S, M, X, Y]) :R[S, Y] =
			mapping(e)

		override def selectAsId[M[O] <: MappingAt[O], V](e :SelectAsIdSQL[F, M, V]) :R[Single, V] =
			mapping(e)
	}
}




/** An expression representing a single column of a table, or, more generally, another relation,
  * such as a ''group by'' expression. There is little need to reference this type in most situations,
  * as the mapping type `M` will almost always be specified
  * as a [[net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn TypedColumn]], and thus a simple
  * [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, S, V]` will likely serve as well.
  * It exists because many methods of [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]]
  * use `MappingSQL` in their return type, and in some cases the result should also be a column.
  *
  * @tparam F $F
  * @tparam S $S
  * @tparam M $M
  * @tparam V $V
  * @define this column
  * @define Cons `ColumnMappingSQL`
  * @define link [[net.noresttherein.oldsql.sql.ast.ColumnMappingSQL ColumnMappingSQL]]
  */
trait ColumnMappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[O] <: ColumnAt[O], V]
	extends MappingSQL[F, S, M, V] with ColumnSQL[F, S, V]
	   with ConvertingColumnTemplate[F, S, V, ColumnMappingSQL.from[F]#rows[S]#meta[M]#E]
	   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = ColumnMappingSQL[f, S, M, V] })#E]
	   with MappingSQLTemplate[F, S, M, V, ColumnMappingSQL[F, S, M, V]]
{
	override val export   :TypedColumn[M[Unit]#Subject, Origin]
	override val anchored :TypedColumn[M[Unit]#Subject, Origin]

	protected override def convert[X](conversion :SQLConversion[V, X]) :ColumnMappingSQL[F, S, M, X] =
		ConvertedColumnMappingSQL(this, conversion)

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL[F, S, M, _]] =
		PassedArray :+ this

	//fixme: Due to a compiler bug we don't have ColumnLValueSQL[F, M, V] <: ColumnSQL[F, GlobalScope, V].
	// In Scala 3 hopefully this will not be needed
	@inline private[oldsql] final def toColumnSQL :ColumnSQL[F, S, V] = this
}




object ColumnMappingSQL {

	type __ = ColumnMappingSQL[_ <: RowProduct, _ >: Grouped <: Single, M, _]
			forSome { type M[O] <: ColumnAt[O] }

	type from[-F <: RowProduct] = {
		type __ = ColumnMappingSQL[F, _ >: Grouped <: Single, M, _] forSome { type M[O] <: MappingAt[O] }
		type rows[-S >: Grouped <: Single] = {
			type __ = ColumnMappingSQL[F, S, M, _] forSome { type M[O] <: ColumnAt[O] }
			type meta[M[A] <: ColumnAt[A]] = {
				type __ = ColumnMappingSQL[F, S, M, _]
				type C[V] = ColumnMappingSQL[F, S, M, V]
				type E[V] = ColumnMappingSQL[F, S, M, V]
			}
		}
	}
//	type c[F <: RowProduct] = {
//		type cons[S >: Grouped <: Single, M[A] <: ColumnAt[A], V] = ColumnMappingSQL[F, S, M, V]
//		type c[S >: Grouped <: Single] = {
//			type __ = ColumnMappingSQL[F, S, M, _] forSome { type M[O] <: ColumnAt[O] }
//			type cons[M[A] <: ColumnAt[A], V] = ColumnMappingSQL[F, S, M, V]
//			type c[M[A] <: ColumnAt[A]] = {
//				type __ = ColumnMappingSQL[F, S, M, _]
//				type c[V] = ColumnMappingSQL[F, S, M, V]
//			}
//		}
//	}
//	type c[-F <: RowProduct] = {
//		type c[-S >: Grouped <: GlobalScope] = {
//			type c[M[A] <: MappingAt[A]] = {
//				type c[V] = MappingSQL[F, S, M, V]
//			}
//		}
//	}


	trait ConvertedColumnMappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[A]<: ColumnAt[A], X, Y]
		extends ConvertedMappingSQL[F, S, M, X, Y] with ConvertedColumnSQL[F, S, X, Y] with ColumnMappingSQL[F, S, M, Y]
	{
		override val value :ColumnMappingSQL[F, S, M, X]

		override val export   :TypedColumn[M[Unit]#Subject, Origin] = value.export
		override val anchored :TypedColumn[M[Unit]#Subject, Origin] = value.anchored

		override def default :ColumnMappingSQL[F, S, M, Y] = reapply(value.default)

		override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
				:ColumnMappingSQL[F, S, M, Y] =
			reapply(value.defaultWith(includes, excludes))

		override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
				:ColumnMappingSQL[F, S, M, Y] =
			reapply(value.alter(includes, excludes))

		protected override def convert[Z](conversion :SQLConversion[Y, Z]) :ColumnMappingSQL[F, S, M, Z] =
			(this.adaptation andThen conversion)(value)

		override def anchor(from :F) :ColumnMappingSQL[F, S, M, Y] = value.anchor(from) match {
			case same if same eq value => this
			case other => adaptation(other)
		}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :ColumnMappingSQL[E, S, M, Y] =
			reapply(value.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :ColumnMappingSQL[E, S, M, Y] =
			reapply(value.expand(base))

		private def reapply[E <: RowProduct, C >: Grouped <: Single]
		                   (e :ColumnMappingSQL[E, C, M, X]) :ColumnMappingSQL[E, C, M, Y] = adaptation(e)

		protected override def visit[R](visitor :SpecificColumnVisitor[F, S, Y, R]) :R = visitor.convertedColumnMapping(this)
		protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, Y] =
			visitor.convertedColumnMapping(this)
//
//		protected override def visit[F_ <: F, S_ >: Grouped <: S,
//		                             E >: ColumnMappingSQL[F_, S_, M, Y] <: SQLExpression[F_, S_, Y],
//		                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[F, R]) :R[S_, Y, E] =
//			visitor.convertedColumnMapping(this)
	}


	object ConvertedColumnMappingSQL {
		def apply[F <: RowProduct, S >: Grouped <: Single, M[A] <: ColumnAt[A], X, Y]
		         (value :ColumnMappingSQL[F, S, M, X], conversion :SQLConversion[X, Y])
				:ConvertedColumnMappingSQL[F, S, M, X, Y] =
			new Impl(value, conversion)

		def unapply[F <: RowProduct, S >: Grouped <: Single, Y](e :SQLExpression[F, S, Y])
				:Opt[(ColumnMappingSQL[F, S, M, X], SQLConversion[X, Y]) forSome { type M[A] <: ColumnAt[A]; type X }] =
			e match {
				case convert :ConvertedColumnMappingSQL[F, S, MappingOf[Any]#ColumnProjection, _, _] @unchecked =>
					Got((convert.value, convert.adaptation))
				case _ => Lack
			}


		type __ = ConvertedColumnMappingSQL[_ <: RowProduct, _ >: Grouped <: Single, M, _, _] forSome {
			type M[A] <: ColumnAt[A]
		}

		private class Impl[-F <: RowProduct, -S >: Grouped <: Single, M[A] <: ColumnAt[A], X, Y]
		              (override val value :ColumnMappingSQL[F, S, M, X], override val adaptation :SQLConversion[X, Y])
			extends ConvertedColumnMappingSQL[F, S, M, X, Y]


		trait SpecificConvertedColumnMappingVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
			extends SpecificConvertedColumnLValueVisitor[F, X, Y]
		{
			def convertedColumnMapping[M[A] <: ColumnAt[A], V](e :ConvertedColumnMappingSQL[F, S, M, V, X]) :Y
		}
		trait MatchSpecificConvertedColumnMapping[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
			extends SpecificConvertedColumnMappingVisitor[F, S, X, Y] with CaseSpecificConvertedColumnLValue[F, X, Y]

		trait CaseSpecificConvertedColumnMapping[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
			extends MatchSpecificConvertedColumnMapping[F, S, V, Y]
		{
			override def convertedColumnLValue[M[O] <: BaseColumn[X, O], X](e :ConvertedColumnLValue[F, M, X, V]) :Y =
				convertedColumnMapping(e)
		}
//
//
//		trait ConvertedColumnMappingVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends ConvertedColumnLValueVisitor[F, Y]
//		{
//			def convertedColumnMapping[S >: Grouped <: Single, M[A] <: ColumnAt[A], X, V]
//			                          (e :ConvertedColumnMappingSQL[F, S, M, X, V])
//					:Y[S, V, ConvertedColumnMappingSQL[F, S, M, X, V]]
//		}
//		trait MatchConvertedColumnMapping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends ConvertedColumnMappingVisitor[F, Y] with CaseConvertedColumnLValue[F, Y]
//
//		trait CaseConvertedColumnMapping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends MatchConvertedColumnMapping[F, Y]
//		{
//			override def convertedColumnLValue[M[O] <: BaseColumn[X, O], X, V](e :ConvertedColumnLValue[F, M, X, V])
//					:Y[Single, V, ConvertedColumnLValue[F, M, X, V]] =
//				convertedColumnMapping(e)
//		}


		trait AnyConvertedColumnMappingVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] {
			def convertedColumnMapping[S >: Grouped <: Single, M[A] <: ColumnAt[A], X, Y]
			                          (e :ConvertedColumnMappingSQL[F, S, M, X, Y]) :R[S, Y]
		}
		trait MatchAnyConvertedColumnMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
			extends AnyConvertedColumnMappingVisitor[F, R] with CaseAnyConvertedColumnLValue[F, R]

		trait CaseAnyConvertedColumnMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
			extends MatchAnyConvertedColumnMapping[F, R]
		{
			override def convertedColumnLValue[M[O] <: BaseColumn[X, O], X, Y]
			                                  (e :ConvertedColumnLValue[F, M, X, Y]) :R[Single, Y] =
				convertedColumnMapping(e)
		}
	}



	trait SpecificColumnMappingVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificColumnLValueVisitor[F, X, Y] with SpecificConvertedColumnMappingVisitor[F, S, X, Y]
		   with SpecificColumnMappingTermVisitor[X, Y] with SpecificEditedColumnVisitor[F, X, Y]
	{
		def columnMapping[M[O] <: ColumnAt[O]](e :ColumnMappingSQL[F, S, M, X]) :Y
	}
	trait MatchSpecificColumnMapping[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificColumnMappingVisitor[F, S, X, Y] with CaseSpecificConvertedColumnMapping[F, S, X, Y]
		   with CaseSpecificColumnComponent[F, X, Y] with CaseSpecificLooseColumn[F, X, Y]
           with CaseSpecificColumnMappingTerm[X, Y] with CaseSpecificEditedColumn[F, X, Y]

	trait CaseSpecificColumnMapping[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificColumnMapping[F, S, X, Y]
	{
		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[X, A], L <: RowProduct]
		                            (e :TypedColumnComponentSQL[O, T, R, M, X, L]) :Y =
			columnMapping(e)

		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[X, A]](e :LooseColumn[O, M, X]) :Y =
			columnMapping(e)

		override def convertedColumnMapping[M[O] <: ColumnAt[O], V](e :ConvertedColumnMappingSQL[F, S, M, V, X]) :Y =
			columnMapping(e)

		override def columnMappingTerm[M[A] <: BaseColumn[X, A]](e :ColumnMappingTerm[M, X]) :Y = columnMapping(e)

		override def editedColumn[M[A] <: BaseColumn[X, A], V](e :EditedColumnSQL[F, M, X, V]) :Y = columnMapping(e)
	}
//
//
//	trait ColumnMappingVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ColumnLValueVisitor[F, Y] with ConvertedColumnMappingVisitor[F, Y]
//		   with ColumnMappingTermVisitor[Y] with EditedColumnVisitor[F, Y]
//	{
//		def columnMapping[S >: Grouped <: Single, M[A] <: ColumnAt[A], V]
//		                 (e :ColumnMappingSQL[F, S, M, V]) :Y[S, V, ColumnMappingSQL[F, S, M, V]]
//	}
//	trait MatchColumnMapping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ColumnMappingVisitor[F, Y] with CaseConvertedColumnMapping[F, Y]
//		   with CaseColumnComponent[F, Y] with CaseLooseColumn[F, Y]
//		   with CaseColumnMappingTerm[Y] with CaseEditedColumn[F, Y]
//
//	trait CaseColumnMapping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchColumnMapping[F, Y]
//	{
//		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
//		                            (e :TypedColumnComponentSQL[O, T, R, M, V, L])
//				:Y[Single, V, TypedColumnComponentSQL[O, T, R, M, V, L]] =
//			columnMapping(e)
//
//		override def columnMappingTerm[M[A] <: BaseColumn[V, A], V]
//		                              (e :ColumnMappingTerm[M, V]) :Y[Single, V, ColumnMappingTerm[M, V]] =
//			columnMapping(e)
//
//		override def convertedColumnMapping[S >: Grouped <: Single, M[A] <: ColumnAt[A], X, V]
//		                                   (e :ConvertedColumnMappingSQL[F, S, M, X, V])
//				:Y[S, V, ConvertedColumnMappingSQL[F, S, M, X, V]] =
//			columnMapping(e)
//
//		override def editedColumn[M[A] <: BaseColumn[V, A], V, X]
//		                         (e :EditedColumnSQL[F, M, V, X]) :Y[Single, V, EditedColumnSQL[F, M, V, X]] =
//			columnMapping(e)
//
//		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
//		                        (e :LooseColumn[O, M, V]) :Y[Single, V, LooseColumn[O, M, V]] =
//			columnMapping(e)
//	}


	trait AnyColumnMappingVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyColumnLValueVisitor[F, R] with AnyConvertedColumnMappingVisitor[F, R]
		   with AnyColumnMappingTermVisitor[R] with AnyEditedColumnVisitor[F, R]
	{
		def columnMapping[S >: Grouped <: Single, M[A] <: ColumnAt[A], V]
		                 (e :ColumnMappingSQL[F, S, M, V]) :R[S, V]
	}
	trait MatchAnyColumnMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyColumnMappingVisitor[F, R] with CaseAnyConvertedColumnMapping[F, R]
		   with CaseAnyColumnComponent[F, R] with CaseAnyLooseColumn[F, R]
		   with CaseAnyColumnMappingTerm[R] with CaseAnyEditedColumn[F, R]

	trait CaseAnyColumnMapping[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyColumnMapping[F, Y]
	{
		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
		                            (e :TypedColumnComponentSQL[O, T, R, M, V, L]) :Y[Single, V] =
			columnMapping(e)

		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
		                        (e :LooseColumn[O, M, V]) :Y[Single, V] =
			columnMapping(e)

		override def convertedColumnMapping[S >: Grouped <: Single, M[O] <: BaseColumn[X, O], X, V]
		                                   (e :ConvertedColumnMappingSQL[F, S, M, X, V]) :Y[Single, V] =
			columnMapping(e)

		override def columnMappingTerm[M[O] <: BaseColumn[V, O], V](e :ColumnMappingTerm[M, V]) :Y[Single, V] =
			columnMapping(e)

		override def editedColumn[M[A] <: BaseColumn[X, A], X, V](e :EditedColumnSQL[F, M, X, V]) :Y[Single, X] =
			columnMapping(e)
	}

}

package net.noresttherein.oldsql.sql.ast

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, NoSuchComponentException}
import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, SQLForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, ColumnSetter, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ColumnConvertingTemplate, SpecificColumnVisitor, VariantColumnGroundingTemplate}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingOps, ConvertingTemplate, GroundingTemplate, Grouped, ReorderingTemplate, Single, SpecificExpressionVisitor, SpecificSQL, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.AdaptedSQL.AdaptedSQLTemplate
import net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.ConvertedColumnLValue.{AnyConvertedColumnLValueVisitor, CaseAnyConvertedColumnLValue, CaseSpecificConvertedColumnLValue, SpecificConvertedColumnLValueVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.{AnyColumnLValueVisitor, ConvertedColumnLValue, SpecificColumnLValueVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.{AnyColumnMappingVisitor, ConvertedColumnMappingSQL, SpecificColumnMappingVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.ConvertedColumnMappingSQL.{AnyConvertedColumnMappingVisitor, CaseAnyConvertedColumnMapping, CaseSpecificConvertedColumnMapping, SpecificConvertedColumnMappingVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingTerm.{AnyColumnMappingTermVisitor, CaseAnyColumnMappingTerm, CaseSpecificColumnMappingTerm, SpecificColumnMappingTermVisitor}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeTemplate
import net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL.SpecificConvertedColumnVisitor
import net.noresttherein.oldsql.sql.ast.ConvertedSQL.{ConvertedSQLTemplate, SpecificConvertedSQLTemplate}
import net.noresttherein.oldsql.sql.ast.EditedColumnSQL.{AnyEditedColumnVisitor, CaseAnyEditedColumn, CaseSpecificEditedColumn, SpecificEditedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.EditedLValueSQL.{AnyEditedLValueVisitor, CaseAnyEditedLValue, CaseSpecificEditedLValue, SpecificEditedLValueVisitor}
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL.{AnyColumnComponentVisitor, CaseAnyColumnComponent, CaseSpecificColumnComponent, SpecificColumnComponentVisitor}
import net.noresttherein.oldsql.sql.ast.LooseColumn.{AnyLooseColumnVisitor, CaseAnyLooseColumn, CaseSpecificLooseColumn, SpecificLooseColumnVisitor}
import net.noresttherein.oldsql.sql.ast.LValueSQL.ConvertedLValueSQL.{AnyConvertedLValueVisitor, CaseAnyConvertedLValue, CaseSpecificConvertedLValue, SpecificConvertedLValueVisitor}
import net.noresttherein.oldsql.sql.ast.LValueSQL.{AnyLValueVisitor, CaseAnyLValue, CaseSpecificLValue, ConvertedLValueSQL, SpecificLValueVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ConvertedMappingSQL, MappingSQLTemplate, RearrangedMappingSQL, TransformedMappingDefaults}
import net.noresttherein.oldsql.sql.ast.MappingSQL.ConvertedMappingSQL.{AnyConvertedMappingVisitor, CaseAnyConvertedMapping, ConvertedMappingSQLTemplate, MatchSpecificConvertedMapping, SpecificConvertedMappingVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.RearrangedMappingSQL.{AnyRearrangedMappingVisitor, CaseAnyRearrangedMapping, CaseSpecificRearrangedMapping, SpecificRearrangedMappingVisitor}
import net.noresttherein.oldsql.sql.ast.MappingTerm.{AnyMappingTermVisitor, CaseAnyMappingTerm, CaseSpecificMappingTerm, SpecificMappingTermVisitor}
import net.noresttherein.oldsql.sql.ast.RearrangedSQL.RearrangedSQLTemplate
import net.noresttherein.oldsql.sql.ast.SelectAsIdSQL.{AnySelectAsIdVisitor, CaseAnySelectAsId, CaseSpecificSelectAsId, SpecificSelectAsIdVisitor}
import net.noresttherein.oldsql.sql.ast.TransformedSQL.WrappedSQLTemplate
import net.noresttherein.oldsql.sql.mechanics.{AlignableColumns, SQLConversion, SQLTransformation, UnalignedSQL}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.MayReform
import net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Returning
import net.noresttherein.oldsql.sql.mechanics.UnalignedSQL.UnalignedSQLTemplate

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
  * The exact set used in the final SQL cannot be determined based solely on this expression (unless it is
  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingOps.isAnchored anchored]] in the ''from'' clause
  * of the SQL ''select'' it is used in), as it depends on the mapping instance used in the relation
  * of the actual ''from'' clause of the spelled SQL ''select'', rather than the instance presented here.
  * Nevertheless, these are typically the same, and the column set can be modified by various
  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.exclude exclude]] methods
  * of the main subclass, [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]. See the documentation
  * of that trait and [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] for more details on the subject.
  *
  * Note that in some rare cases, the expression may have additional columns, which are not based on a column
  * of [[net.noresttheerein.oldsq.sql.ast.MappingSQL.mapping mapping]]. For the purpose of spelling,
  * [[net.noresttherein.oldsql.sql.MappingSQL.split split]] method should be used.
  * @tparam F $F
  * @tparam S $S
  * @tparam M $M
  * @tparam V $V
  * @see [[net.noresttherein.oldsql.sql.ast.LValueSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.SelectAsIdSQL]]
  * @author Marcin Mościcki
  */ //consider: make the mapping covariant
trait MappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[O] <: MappingAt[O], V]
	extends SQLExpression[F, S, V]
//	   with ConvertingTemplate[F, S, V, MappingSQL.from[F]#rows[S]#meta[M]#E]
	   with ConvertingTemplate[F, S, V, ({ type E[v] = MappingSQL[F, S, M, v] })#E]
	   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = MappingSQL[f, S, M, V] })#E]
	   with ReorderingTemplate[F, S, V, MappingSQL[F, S, M, V]]
	   with MappingSQLTemplate[F, S, M, V, MappingSQL[F, S, M, V]]
{ self =>
	//consider: I don't like export and anchored being here, but they are needed for polymorphism in AlignedExpression
	val export   :TypedMapping[M[Unit]#Subject, Origin]

	//Todo: try to remove it; the only place we really need it is reforming of a MappingTerm by overriding its export version
	//  Alternatively, we can remove export and swap their names.
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
	     (component :K)(implicit project :OriginProjection[K, X])
			:MappingSQL[F, S, project.WithOrigin, X] { type Origin = self.Origin }

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
			:ColumnMappingSQL[F, S, project.WithOrigin, X] { type Origin = self.Origin }


	//Unusable on MappingSQL level, because we can't create ColumnLValueSQL for setter's lvalue.
	// Could still be made work if we introduced a superclass of ColumnSetter for this purpose.
//	/** Creates an SQL expression which substitutes certain columns of this component for arbitrary SQL expressions
//	  * of a compatible type. The [[net.noresttherein.oldsql.sql.ColumnSetter.lvalue l-values]] of the pairs must
//	  * be instances of [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL ColumnLValueSQL]] for column mappings
//	  * of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]] (including
//	  * columns of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]),
//	  * while the [[net.noresttherein.oldsql.sql.ColumnSetter.rvalue r-values]] must be expressions based on
//	  * the same clause as this expression (or its subtype). The columns will be automatically included in the result
//	  * using this instance's [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
//	  * method.
//	  *
//	  * The arguments of this method are split into the first substitution and the rest in order to avoid erasure
//	  * conflicts. All arguments are functions accepting the nominal mapping of this expression and pairing
//	  * one of its columns with an arbitrary [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] expression.
//	  * They work in a similar fashion to [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter alter]] family
//	  * of methods of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], but instead of just selecting
//	  * a column/component, they assign to it another expression. This can be done using extension method
//	  * [[net.noresttherein.oldsql.sql.ast.LValueSQL.:= :=]] available on the argument
//	  * through an implicit conversion:
//	  * {{{
//	  *     familiar.substitute(_.name := "Boo".?, _.species := "Hamster".?)
//	  * }}}
//	  */
//	//Fixme: if FromLast is undefined then there is no implicit conversion from Mapping to LooseComponent.
//	// The only thing stopping us is the bound on L type parameter in TypedComponentSQL
//	def substitute[E <: F](substitute :M[FromLast] => ColumnSetter[FromLast, E],
//	                       substitutes :M[FromLast] => ColumnSetter[FromLast, E]*) :MappingSQL[E, Single, M, V] =
//	{
//		val arg = mapping.castParam[FromLast]
//		this.substitute(substitutes.view.map(_(arg)).prepended(substitute(arg)) to ArraySeq)
//	}
//
//	/** Creates an SQL expression which substitutes certain columns of this component for arbitrary SQL expressions
//	  * of compatible type. The [[net.noresttherein.oldsql.sql.ColumnSetter.lvalue l-values]] of the pairs must
//	  * be instances of [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL ColumnLValueSQL]] for column mappings
//	  * of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]] (including
//	  * columns of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]),
//	  * while the [[net.noresttherein.oldsql.sql.ColumnSetter.rvalue r-values]] must be expressions based on
//	  * the same clause as this expression (or its subtype). The columns will be automatically included in the result
//	  * using this instance's [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
//	  * method.
//	  *
//	  * As [[net.noresttherein.oldsql.sql.ast.LValueSQL.FromLast FromLast]] type of this expression will not
//	  * be known in most contexts, `this.`[[net.noresttherein.oldsql.sql.ast.LValueSQL.asLast asLast]]
//	  * can be used to obtain a reference to a copy of this component expression grounded in `this.FromLast`.
//	  */
//	def substitute[E <: F](substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V]
//
//	def apply[E <: F](substitute :M[FromLast] => ColumnSetter[FromLast, E],
//	                  substitutes :M[FromLast] => ColumnSetter[FromLast, E]*) :MappingSQL[E, Single, M, V] =
//	{
//		val arg = mapping.castParam[FromLast]
//		this.substitute(substitutes.view.map(_(arg)).prepended(substitute(arg)) to PassedArray)
//	}
//
//	def apply[E <: F](substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V] =
//		substitute(substitutes)


	override def groundValue :Opt[V] = Lack
	override def isGround    :Boolean = false

	protected override def convert[X](conversion :SQLConversion[V, X]) :MappingSQL[F, S, M, X] =
		ConvertedMappingSQL(this, conversion)



	//consider: if we can change the type to TypedMapping[_, Origin]
	//Used during reforming because MappingTerm and ComponentSQL have different effective columns
	//todo: Try to remove it, it might not be used at all. But lets first have a robust aligning implementation.
	def canExclude(component :TypedColumn[_, Origin])(implicit spelling :SQLSpelling) :Boolean
	def canInclude(component :TypedColumn[_, Origin])(implicit spelling :SQLSpelling) :Boolean
	def isIncluded(component :TypedColumn[_, Origin])(implicit spelling :SQLSpelling) :Boolean

	/** List of mappings for all columns of the effective mapping (the one used in
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.defaultSpelling spelling]]) of this expression.
	  * For [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] it is
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.anchored anchored]]`.`[[net.noresttherein.oldsql.schema.Mapping.columns columns]],
	  * but for [[net.noresttherein.oldsql.sql.ast.MappingTerm MappingTerm]] it is
	  * [[net.noresttherein.oldsql.sql.ast.MappingTerm.export export]]`.columns`.
	  */
	def columns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]]
	def defaultColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]]
	def mandatoryColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]]

	def columns(component :MappingAt[Origin])(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]]
	def defaultColumns(component :MappingAt[Origin])(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]]
	def mandatoryColumns(component :MappingAt[Origin])(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]]

	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[V] = conversion(mappingForm)

	protected def mappingForm(implicit spelling :SQLSpelling) :SQLForm[M[Unit]#Subject]

	protected[sql] def `->mappingForm`(implicit spelling :SQLSpelling) :SQLForm[M[Unit]#Subject] = mappingForm


	protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :MappingSQL[F, S, M, V] =
		new RearrangedMappingSQL(this, reordering)
//		throw new NotImplementedError("MappingSQL.reform(reordering :Rearrangement) should have been overridden!")

	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		AlignableColumns(this, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int = mapping.columns.size

//	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[F]#rows[S]#__]
//
//	private[oldsql] override def `->split`(spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[F]#rows[S]#__] =
//		split(spelling)

	override def typeString :ChunkedString = mapping.mappingName
}




object MappingSQL {

	type __ = MappingSQL[_ <: RowProduct, _ >: Grouped <: Single, M, _] forSome { type M[O] <: MappingAt[O] }

	type from[F <: RowProduct] = {
		type __ = MappingSQL[F, _ >: Grouped <: Single, M, _] forSome { type M[O] <: MappingAt[O] }
		type rows[S >: Grouped <: Single] = {
			type __ = MappingSQL[F, S, M, _] forSome { type M[O] <: MappingAt[O] }
			type meta[M[A] <: MappingAt[A]] = {
				type __ = MappingSQL[F, S, M, _]
				type E[V] = MappingSQL[F, S, M, V]
				type of[V] = MappingSQL[F, S, M, V]
			}
			type of[V] = MappingSQL[F, S, M, V] forSome { type M[A] <: MappingAt[A] }
		}
	}
	type meta[M[A] <: MappingAt[A]] = {
		type apply[-F <: RowProduct, -S >: Grouped <: Single, V] = MappingSQL[F, S, M, V]
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

		/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] consisting solely of this component's
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]'s
		  * relation [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]].
		  * It joins the nominal mapping of the underlying relation
		  * ([[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.Entity Entity]],
		  * that is the type of `origin.mapping`, with `RowProduct` wildcard clause,
		  * using an [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] link proper for the `origin` relation
		  * ([[net.noresttherein.oldsql.sql.AndFrom AndFrom]] or [[net.noresttherein.oldsql.sql.AndBy AndBy]]).
		  * The definition of this type isn't normally visible to clients of this expression,
		  * but it is the exact prefix of `this.Origin` truncating it after the first relation.
		  * @see [[net.noresttherein.oldsql.sql.ast.LValueSQL.asLast asLast]]
		  */ //todo: we could try to enforce lower upper bound in TypedComponentSQL and in JoinedRelation
		type FromLast <: RowProduct

		//todo: add a mapping method to SQLExpression
		/** An instance of the mapping representing the value of this expression. */
		val mapping :M[Origin]

		/** Tells if the altering methods of this $Cons actually have any effect, or is the same instance always returned. */
		def isAlterable :Boolean = true

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




	/* Can't be a TransformedSQL (or TransformedSQLConvertingTemplate) because its premise kind of assumes
	 * that any expression, at least of an appropriate type, can be transformed in the same way.
	 * However, EditedLValueSQL cannot just implement reapply - it must override every method to delegate not only
	 * to the underlying lvalue, but also to substitutes. Unfortunate. However, if we manage to remove all those columns
	 * methods that need delegating to the underlying mapping, we might make it a TransformedSQL and simply implement
	 * that handful of required methods in EditedLValueSQL.
	 */
	private[ast] trait TransformedMappingDefaults[-F <: RowProduct, -S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y]
		extends MappingSQL[F, S, M, Y]
	{
		override type Origin   = value.Origin
		override type FromLast = value.FromLast

		/** The adapted mapping expression. Must be either a `lazy val` or a constructor parameter `val`
		  * (it is used in initialization of `MappingConversion` trait).
		  */
		val value :MappingSQL[F, S, M, X]
		override val mapping       :M[Origin] = value.mapping
		override val export        :TypedMapping[M[Unit]#Subject, Origin] = value.export
		override lazy val anchored :TypedMapping[M[Unit]#Subject, Origin] = value.anchored

		override def \[K <: MappingAt[Origin], V](component :K)(implicit project :OriginProjection[K, V])
				:MappingSQL[F, S, project.WithOrigin, V] { type Origin = value.Origin } =
			value \ component

		override def \[K <: ColumnAt[Origin], V]
		              (column :K)(implicit project :OriginProjection[K, V] { type WithOrigin[A] <: BaseColumn[V, A] })
				:ColumnMappingSQL[F, S, project.WithOrigin, V] { type Origin = value.Origin } =
			value \ column

		override def isDefault   :Boolean = value.isDefault
		override def isGround    :Boolean = value.isGround

		override def canExclude(component :TypedColumn[_, Origin])(implicit spelling :SQLSpelling) :Boolean =
			value.canExclude(component)

		override def canInclude(component :TypedColumn[_, Origin])(implicit spelling :SQLSpelling) :Boolean =
			value.canInclude(component)

		override def isIncluded(component :TypedColumn[_, Origin])(implicit spelling :SQLSpelling) :Boolean =
			value.isIncluded(component)

		override def columns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
			value.columns

		override def defaultColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
			value.defaultColumns

		override def mandatoryColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
			value.mandatoryColumns

		override def columns(component :MappingAt[Origin])
		                    (implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
			value.columns(component)

		override def defaultColumns(component :MappingAt[Origin])
		                           (implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
			value.defaultColumns(component)

		override def mandatoryColumns(component :MappingAt[Origin])
		                             (implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
			value.mandatoryColumns(component)


		protected override def mappingForm(implicit spelling :SQLSpelling) :SQLForm[M[Unit]#Subject] =
			value.`->mappingForm`
	}


	/* Some repetition could be extracted to a base TransformedMappingSQLTemplate trait,
	 * extending TransformedSQL and MappingSQL. Not feasible at the moment: first, MappingSQL mixes in
	 * ConvertingTemplate, and TransformedSQL implements reform returning SQLExpression.
	 * Second, it would have to be a template trait to be useful, because otherwise many classes would need to override
	 * these methods anyway, and we'd have to do it UnaryCompositeSQL way, that is having a reapply accepting
	 * SQLExpression (or MappingSQL, doesn't make a difference for ConvertedLValue).
	 * However, in Scala 3, with generic functions and trait constructors, we can pass in 'reapply' as a constructor
	 * argument and ignore variance we have to deal with it as a method. We could refactor UnaryCompositeTemplate
	 * and BinaryCompositeTemplate quite sweetly to have typed wrapped values.
	 */
/*
	trait TransformedMappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y]
		extends TransformedMappingDefaults[F, S, M, X, Y]
		   with TransformedSQL[F, S, X, Y]
//		   with TransformedSQLConvertingTemplate[F, S, X, Y, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, v] = MappingSQL[f, s, M, v] })#E]
	{
//		override type Origin   = value.Origin
//		override type FromLast = value.FromLast
		/** The adapted mapping expression. Must be either a `lazy val` or a constructor parameter `val`
		  * (it is used in initialization of `ConvertedMappingSQL` trait).
		  */
		override val value         :MappingSQL[F, S, M, X] //override clash
//		override val mapping       :M[Origin] = value.mapping
//		override val export        :TypedMapping[M[Unit]#Subject, Origin] = value.export
//		override lazy val anchored :TypedMapping[M[Unit]#Subject, Origin] = value.anchored
//
//		override def \[K <: MappingAt[Origin], V](component :K)(implicit project :OriginProjection[K, V])
//				:MappingSQL[F, S, project.WithOrigin, V] { type Origin = value.Origin } =
//			value \ component
//
//		override def \[K <: ColumnAt[Origin], V]
//		              (column :K)(implicit project :OriginProjection[K, V] { type WithOrigin[A] <: BaseColumn[V, A] })
//				:ColumnMappingSQL[F, S, project.WithOrigin, V] { type Origin = value.Origin } =
//			value \ column

//		override def substitute[E <: F]
//		                       (substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V] =
//			reapply(value.substitute(substitutes))

		override def isGround    :Boolean = value.isGround
		override def groundValue :Opt[Y]  = value.groundValue.map(transformation.apply)
		override def isDefault   :Boolean = value.isDefault
		override def default     :MappingSQL[F, S, M, Y] = reapply(value.default)

		override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
				:MappingSQL[F, S, M, Y] =
			reapply(value.defaultWith(includes, excludes))

		override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
				:MappingSQL[F, S, M, Y] =
			reapply(value.alter(includes, excludes))

		override def anchor(from :F) :MappingSQL[F, S, M, Y] = reapply(value.anchor(from))

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :MappingSQL[E, S, M, Y] =
			reapply(value.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :MappingSQL[E, S, M, Y] =
			reapply(value.expand(base))

		protected def reapply[E <: RowProduct, C >: Grouped <: Single](e :MappingSQL[E, C, M, X]) :MappingSQL[E, C, M, Y]

//		protected override def potentialColumns(permissions :Permissions)
//		                                       (implicit spelling :SQLSpelling) :AlignableColumns =
//			spelling.potentialColumns(value, permissions)
	}
*/


	/** Applies a Scalaland conversion [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation SQLTransformation]]`[X, Y]`
	  * to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] of another `MappingSQL[F, S, M, X]`,
	  * without effect on the generated SQL.
	  */
	trait ConvertedMappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[A]<: MappingAt[A], X, Y]
		extends TransformedMappingDefaults[F, S, M, X, Y]
		   with ConvertedSQL[F, S, X, Y]
		   with ConvertedMappingSQLTemplate[F, S, M, X, Y,
		                                    ({ type E[-f <: RowProduct, -s >: Grouped <: Single, v] = MappingSQL[f, s, M, v] })#E]
			//Not extending UnaryCompositeTemplate allows a safe reapply(SQLExpression) implementation
//		   with UnaryCompositeTemplate[F, S, X, Y,
//	                                   ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = MappingSQL[f, s, M, Y] })#E]
	{ self =>
//		override type Origin = value.Origin

//		/** The adapted mapping expression. Must be either a `lazy val` or a constructor parameter `val`
//		  * (it is used in initialization of `MappingConversion` trait).
//		  */
//		override val value :MappingSQL[F, S, M, X]
/*
		override val value      :MappingSQL[F, S, M, X]
		override val mapping    :M[Origin] = value.mapping
		override lazy val export     :TypedMapping[M[Unit]#Subject, Origin] = value.export
		override lazy val anchored   :TypedMapping[M[Unit]#Subject, Origin] = value.anchored
//		override val adaptation :SQLConversion[X, Y]
*/
		override def conversion :SQLConversion[M[Unit]#Subject, Y] = value.conversion andThen transformation

		@throws[NoSuchComponentException]("if component is not a component of this.mapping.")
		override def \[K <: MappingAt[Origin], T]
		              (component :K)(implicit project :OriginProjection[K, T])
				:MappingSQL[F, S, project.WithOrigin, T] { type Origin = value.Origin } =
			value \ component

		@throws[NoSuchComponentException]("if column is not a column of this.mapping.")
		override def \[K <: ColumnAt[Origin], T]
		              (column :K)(implicit project :OriginProjection[K, T] { type WithOrigin[A] <: BaseColumn[T, A] })
				:ColumnMappingSQL[F, S, project.WithOrigin, T] { type Origin = value.Origin } =
			value \ column
//
//		override def default   :MappingSQL[F, S, M, Y] = transformation(value.default)
//
//		override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
//				:MappingSQL[F, S, M, Y] =
//			transformation(value.defaultWith(includes, excludes))
//
//		override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
//				:MappingSQL[F, S, M, Y] =
//			transformation(value.alter(includes, excludes))

//		override def groundValue :Opt[Y]  = value.groundValue.map(transformation.apply)
//		override def isGround    :Boolean = value.isGround
//
//		protected override def convert[Z](conversion :SQLConversion[Y, Z]) :MappingSQL[F, S, M, Z] =
//			(this.transformation andThen conversion)(value)
//
//		override def anchor(from :F) :MappingSQL[F, S, M, Y] = transformation(value.anchor(from))
//
//		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :MappingSQL[E, S, M, Y] =
//			transformation(value.basedOn(base))
//
//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :MappingSQL[E, S, M, Y] =
//			transformation(value.expand(base))
//
//		private def reapply[E <: RowProduct, C >: Grouped <: Single](e :MappingSQL[E, C, M, X]) :MappingSQL[E, C, M, Y] =
//			if (e eq value) this.castFrom[MappingSQL[F, S, M, Y], MappingSQL[E, C, M, Y]] else transformation(e)
/*

		override def canExclude(component :TypedColumn[_, value.Origin])(implicit spelling :SQLSpelling) :Boolean =
			value.canExclude(component)

		override def canInclude(component :TypedColumn[_, value.Origin])(implicit spelling :SQLSpelling) :Boolean =
			value.canInclude(component)

		override def isIncluded(component :TypedColumn[_, value.Origin])(implicit spelling :SQLSpelling) :Boolean =
			value.isIncluded(component)

		override def columns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, value.Origin]] =
			value.columns

		override def defaultColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, value.Origin]] =
			value.defaultColumns

		override def mandatoryColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, value.Origin]] =
			value.mandatoryColumns

		protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[M[Unit]#Subject] =
			value.`->effectiveForm`

		protected override def potentialColumns(permissions :Permissions)
		                                       (implicit spelling :SQLSpelling) :AlignableColumns =
			spelling.potentialColumns(value, permissions)
*/

//		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[F]#rows[S]#__] =
//			spelling.split(value)
//		protected override def potentialColumns(permissions :Permissions)
//		                                       (implicit spelling :SQLSpelling) :AlignableColumns =
//			spelling.potentialColumns(value, permissions)

		protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, Y, R]) :R =
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
					Got((convert.value, convert.transformation))
				case _ => Lack
			}


		trait ConvertedMappingSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y,
		                                  +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
		                                        <: MappingSQL[f, s, M, v]
		                                           with MappingSQLTemplate[f, s, M, v, Same[f, s, v]]
		                                           with SpecificSQL[f, s, v, Same]]
//		                                           with ReorderingTemplate[f, s, v, Same[f, s, m, v]]]
			extends TransformedMappingDefaults[F, S, M, X, Y]
//			   with AdaptedSQLTemplate[F, S, X, Y, Same]
			   with SpecificConvertedSQLTemplate[F, S, X, Y, Same]
			   with VariantGroundingTemplate[F, S, Y, ({ type E[-f <: RowProduct] = Same[f, S, Y] })#E]
//			   with ReorderingTemplate[F, S, Y, Same[F, S, M, Y]]
		{ this :Same[F, S, Y] =>
//			type Origin = value.Origin
			override val value :Same[F, S, X] //override clash with TransformedMappingDefaults

			override def default :Same[F, S, Y] = if (isDefault) this else transformation(value.default)

			override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
					:Same[F, S, Y] =
				transformation(value.defaultWith(includes, excludes))

			override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
					:Same[F, S, Y] =
				transformation(value.alter(includes, excludes))

			protected override def realign(reordering :Rearrangement)
			                              (implicit spelling :SQLSpelling) :MappingSQL[F, S, M, Y] =
				if (reordering.isIdentity) this
				else transformation(spelling.realign(value, reordering))
		}


		private class Impl[-F <: RowProduct, -S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y]
		                  (override val value :MappingSQL[F, S, M, X], override val transformation :SQLConversion[X, Y])
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




	class RearrangedMappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[A] <: MappingAt[A], V]
	                          (override val value :MappingSQL[F, S, M, V], reordering :Rearrangement)
	                          (implicit spelling :SQLSpelling)
		extends RearrangedSQL[F, S, V](value, reordering)
		   with TransformedMappingDefaults[F, S, M, V, V]
		   with RearrangedSQLTemplate[F, S, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, v] = MappingSQL[f, s, M, v] })#E]
	{ self =>

		if (order.columnCount != value.columns.size)
			throw new IllegalArgumentException(
				"Expression `" + value + "` has " + value.columns.size +
					" applicable columns, but reordering " + order + " is defined for " + order.columnCount +
					" columns."
			)

		//		override type Origin = value.Origin
//		override val mapping :M[Origin] = value.mapping
//		override lazy val export :TypedMapping[M[Unit]#Subject, value.Origin] = value.export
//		override lazy val anchored :TypedMapping[M[Unit]#Subject, value.Origin] = value.anchored
		override def conversion :SQLConversion[M[Unit]#Subject, V] = value.conversion

		//adapt the reordering transformation to the component, to keep those columns
		override def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
				:MappingSQL[F, S, project.WithOrigin, X] { type Origin = value.Origin } =
			component match {
				case _ :ColumnMapping =>
					super.\(component)
				case _ =>
					val expr = value \ component
					val allColumns = value.columns
					val componentColumns = expr.columns
					if (componentColumns.isEmpty)
						RearrangedMappingSQL(expr, Rearrangement.empty)
					else try {
						val firstColumn = columns.minBy(allColumns.sureIndexOf(_))
						val lastColumn = columns.maxBy(allColumns.sureIndexOf(_))
						val min = allColumns.sureIndexOf(firstColumn)
						val max = allColumns.sureIndexOf(lastColumn)
						if (min - max + 1 != componentColumns.size)
							throw new IllegalArgumentException(
								"Columns " + componentColumns + " of component " + component + " of `" + value +
									"` do not form a consecutive sequence in all columns of " + value.mapping + ": " +
									allColumns + "."
							)
						try {
							val componentReordering = order.slice(min, max + 1)
							RearrangedMappingSQL(expr, componentReordering)
						} catch {
							case e :Exception =>
								throw new IllegalArgumentException(
									"Columns " + componentColumns + " of component " + component + " in `" + expr +
									"` do not form a consecutive sequence among columns " + allColumns +
										" of its parent component " + value.mapping + "(`" + value +
										"`) after reordering with " + order + ".",
									e
								)
						}
					} catch {
						case e :NoSuchElementException =>
							throw new NoSuchElementException(
								"Not all columns of expression `" + expr + "` for component " + component + " " +
									componentColumns + " are present among columns " + allColumns + " of `" + value +
									"` for its parent component " + value.mapping + ": " +
									componentColumns.filterNot(allColumns.contains(_)) + ".",
								e
							)
					}
		}

		override def \[K <: ColumnAt[Origin], X]
		              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
				:ColumnMappingSQL[F, S, project.WithOrigin, X] { type Origin = value.Origin } =
			super.\(column) //overloading rules...

		protected override def isUniversal = false
//		override def isAlterable = false
		override def isDefault :Boolean = false
		override def default   :MappingSQL[F, S, M, V] = value.default

		override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
				:MappingSQL[F, S, M, V] =
			value.defaultWith(includes, excludes)

		override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
				:MappingSQL[F, S, M, V] =
			value.alter(includes, excludes)

		protected override def potentialColumns(permissions :Permissions)
		                                       (implicit spelling :SQLSpelling) :AlignableColumns =
			super[RearrangedSQL].potentialColumns(permissions)

		protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
			super[RearrangedSQL].potentialColumnsCount

//		protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
//		                              (e :MappingSQL[E, C, M, V]) :MappingSQL[E, C, M, V] =
//			transformation(e)
//			if (e eq value) this.castFrom[MappingSQL[F, S, M, V], MappingSQL[E, C, M, V]] else transformation(e)

		protected override def decorate[E <: RowProduct, C >: Grouped <: Single, X]
		                               (e :SQLExpression[E, C, X]) :MappingSQL[E, C, M, X] =
			e match {
				case mapping :MappingSQL[E, C, M, X] @unchecked if mapping eq value =>
					this.castFrom[MappingSQL[F, S, M, V], MappingSQL[E, C, M, X]]
				case mapping :MappingSQL[E, C, M, X] @unchecked if mapping.mapping identical self.mapping =>
					spelling.realign(mapping, order)
				case _ =>
					throw new IllegalExpressionException(
						"Cannot recreate `" + this + "` for `" + e + "` - not a MappingSQL: " + e.className + "."
					)
			}

		protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :MappingSQL[F, S, M, V] =
			spelling.realign(value, reordering compose this.reordering)

		protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y =
			visitor.rearrangedMapping(this)

		protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] =
			visitor.rearrangedMapping(this)
	}


	object RearrangedMappingSQL {
		def apply[F <: RowProduct, S >: Grouped <: Single, M[A] <: MappingAt[A], V]
		         (e :MappingSQL[F, S, M, V], reordering :Rearrangement)
		         (implicit spelling :SQLSpelling)
				:RearrangedMappingSQL[F, S, M, V] { type Origin = e.Origin } =
			new RearrangedMappingSQL[F, S, M, V](e, reordering) {
				override val value :e.type = e
			}

		trait SpecificRearrangedMappingVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
			def rearrangedMapping[M[O] <: MappingAt[O]](e :RearrangedMappingSQL[F, S, M, X]) :Y
		}
		type MatchSpecificRearrangedMapping[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
			SpecificRearrangedMappingVisitor[F, S, X, Y]
		type CaseSpecificRearrangedMapping[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
			SpecificRearrangedMappingVisitor[F, S, X, Y]

		trait AnyRearrangedMappingVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] {
			def rearrangedMapping[S >: Grouped <: Single, M[O] <: MappingAt[O], V]
			                     (e :RearrangedMappingSQL[F, S, M, V]) :R[S, V]
		}
		type MatchAnyRearrangedMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] =
			AnyRearrangedMappingVisitor[F, R]
		type CaseAnyRearrangedMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] =
			AnyRearrangedMappingVisitor[F, R]
	}



	/** A wrapper created by [[net.noresttherein.oldsql.sql.ast.ParamSQL ParamSQL]] to mark their pseudo component
	  * parameter expressions for aligning by form exchange.
	  */
	class UnalignedMappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[A] <: MappingAt[A], V]
	                         (override val value :MappingSQL[F, S, M, V])
		extends UnalignedSQL[F, S, V](value)
           with TransformedMappingDefaults[F, S, M, V, V]
           with UnalignedSQLTemplate
                [F, S, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, v] = MappingSQL[f, s, M, v] })#E]
	{
		override def conversion :SQLConversion[M[Unit]#Subject, V] = value.conversion

		override def default :MappingSQL[F, S, M, V] = new UnalignedMappingSQL(value.default)

		override def defaultWith(includes :Unique[TypedMapping[_, value.Origin]],
		                         excludes :Unique[TypedMapping[_, value.Origin]]) :MappingSQL[F, S, M, V] =
			new UnalignedMappingSQL(value.defaultWith(includes, excludes))

		override def alter(includes :Iterable[TypedMapping[_, value.Origin]],
		                   excludes :Iterable[TypedMapping[_, value.Origin]]) :MappingSQL[F, S, M, V] =
			new UnalignedMappingSQL(value.alter(includes, excludes))

		protected override def decorate[E <: RowProduct, C >: Grouped <: Single, X]
		                               (e :SQLExpression[E, C, X]) :MappingSQL[E, C, M, X] =
			e match {
				case unaligned :UnalignedMappingSQL[E, C, M @unchecked, V] if unaligned.mapping identical mapping =>
					unaligned
				case value     :MappingSQL[E, C, M @unchecked, V] if value.mapping identical mapping =>
					new UnalignedMappingSQL(value)
				case _ =>
					throw new IllegalExpressionException(
						"Expected a MappingSQL, but got `" + e + "`: " + e.className + "."
					)
			}

	}


	type MappingSQLShape[M[O] <: MappingAt[O], V] = MappingSQL[Nothing, Grouped, M, V]

	trait SpecificMappingVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificColumnMappingVisitor[F, S, X, Y] with SpecificConvertedMappingVisitor[F, S, X, Y]
		   with SpecificLValueVisitor[F, X, Y] with SpecificEditedLValueVisitor[F, X, Y]
		   with SpecificMappingTermVisitor[X, Y] with SpecificRearrangedMappingVisitor[F, S, X, Y]
           with SpecificSelectAsIdVisitor[F, X, Y]
	{
		def mapping[M[O] <: MappingAt[O]](e :MappingSQL[F, S, M, X]) :Y
	}
	trait MatchSpecificMapping[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificMappingVisitor[F, S, X, Y] with CaseSpecificLValue[F, X, Y]
           with CaseSpecificEditedLValue[F, X, Y] with MatchSpecificConvertedMapping[F, S, X, Y]
           with CaseSpecificMappingTerm[X, Y] with CaseSpecificRearrangedMapping[F, S, X, Y]
           with CaseSpecificSelectAsId[F, X, Y]
	{
		override def columnMapping[M[O] <: ColumnAt[O]](e :ColumnMappingSQL[F, S, M, X]) :Y = mapping(e)
		override def editedColumn[M[A] <: BaseColumn[X, A], V](e :EditedColumnSQL[F, M, X, V]) :Y = columnMapping(e)
	}
	trait CaseSpecificMapping[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends MatchSpecificMapping[F, S, X, Y] {
		override def editedLValue[M[A] <: BaseMapping[X, A]](e :EditedLValueSQL[F, M, X]) :Y = mapping(e)
		override def lvalue[M[O] <: MappingAt[O]](e :LValueSQL[F, M, X]) :Y = mapping(e)

		override def mappingTerm[M[A] <: BaseMapping[X, A]](e :MappingTerm[M, X]) :Y = mapping(e)
		override def convertedMapping[M[A] <: MappingAt[A], V](e :ConvertedMappingSQL[F, S, M, V, X]) :Y = mapping(e)

		override def rearrangedMapping[M[O] <: MappingAt[O]](e :RearrangedMappingSQL[F, S, M, X]) :Y = mapping(e)
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
		extends AnyColumnMappingVisitor[F, R] with AnyConvertedMappingVisitor[F, R] with AnyLValueVisitor[F, R]
           with AnyEditedLValueVisitor[F, R] with AnyMappingTermVisitor[R] with AnyRearrangedMappingVisitor[F, R]
           with AnySelectAsIdVisitor[F, R]
	{
		def mapping[S >: Grouped <: Single, M[O] <: MappingAt[O], V](e :MappingSQL[F, S, M, V]) :R[S, V]
	}
	trait MatchAnyMapping[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends AnyMappingVisitor[F, R]
		with CaseAnyLValue[F, R] with CaseAnyEditedLValue[F, R] with CaseAnyConvertedMapping[F, R]
		with CaseAnyMappingTerm[R] with CaseAnyRearrangedMapping[F, R] with CaseAnySelectAsId[F, R]
	{
		override def columnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], V]
		                          (e :ColumnMappingSQL[F, S, M, V]) :R[S, V] =
			mapping(e)

		override def editedColumn[M[A] <: BaseColumn[V, A], V, X](e :EditedColumnSQL[F, M, V, X]) :R[Single, V] =
			columnMapping(e)
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

		override def rearrangedMapping[S >: Grouped <: Single, M[O] <: MappingAt[O], V]
		                              (e :RearrangedMappingSQL[F, S, M, V]) :R[S, V] = mapping(e)

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
	extends MappingSQL[F, S, M, V]
	   with ColumnSQL[F, S, V]
	   with ColumnConvertingTemplate[F, S, V, ColumnMappingSQL.from[F]#rows[S]#meta[M]#E]
	   with VariantColumnGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = ColumnMappingSQL[f, S, M, V] })#E]
//	   with ReorderingTemplate[F, S, V, ColumnMappingSQL[F, S, M, V]]
	   with MappingSQLTemplate[F, S, M, V, ColumnMappingSQL[F, S, M, V]]
{
	override val export   :TypedColumn[M[Unit]#Subject, Origin]
	override val anchored :TypedColumn[M[Unit]#Subject, Origin]

	protected override def convert[X](conversion :SQLConversion[V, X]) :ColumnMappingSQL[F, S, M, X] =
		ConvertedColumnMappingSQL(this, conversion)

	override def columns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] = Unique.single(mapping)
	override def defaultColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] = Unique.single(mapping)
	override def mandatoryColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
		Unique.single(mapping)

	override def columns(component :MappingAt[Origin])(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
		if (component == mapping)
			Unique(mapping)
		else
			throw new IllegalArgumentException(
				"Mapping " + component + " is not the mapping " + mapping + " of column `" + this + "`."
			)
	override def defaultColumns(component :MappingAt[Origin])
	                           (implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
		columns(component)

	override def mandatoryColumns(component :MappingAt[Origin])
	                             (implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
		columns(component)

	protected override def mappingForm(implicit spelling :SQLSpelling) :ColumnForm[M[Unit]#Subject] = anchored.form
	protected[sql] override def `->mappingForm`(implicit spelling :SQLSpelling) :ColumnForm[M[Unit]#Subject] =
		mappingForm

	protected override def effectiveForm(implicit spelling :SQLSpelling) :ColumnForm[V] = conversion(mappingForm)

	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		AlignableColumns(this, permissions)

//	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL[F, S, M, _]] =
//		PassedArray :+ this

	//fixme: Due to a compiler bug we don't have ColumnLValueSQL[F, M, V] <: ColumnSQL[F, GlobalScope, V].
	// In Scala 3 hopefully this will not be needed
	@inline private[oldsql] final def toColumnSQL :ColumnSQL[F, S, V] = this
}




object ColumnMappingSQL {

	type __ = ColumnMappingSQL[_ <: RowProduct, _ >: Grouped <: Single, M, _]
			forSome { type M[O] <: ColumnAt[O] }

	type from[F <: RowProduct] = {
		type __ = ColumnMappingSQL[F, _ >: Grouped <: Single, M, _] forSome { type M[O] <: MappingAt[O] }
		type rows[S >: Grouped <: Single] = {
			type __ = ColumnMappingSQL[F, S, M, _] forSome { type M[O] <: ColumnAt[O] }
			type meta[M[A] <: ColumnAt[A]] = {
				type __ = ColumnMappingSQL[F, S, M, _]
				type C[V] = ColumnMappingSQL[F, S, M, V]
				type E[V] = ColumnMappingSQL[F, S, M, V]
			}
		}
	}
	type meta[M[A] <: ColumnAt[A]] = {
		type apply[-F <: RowProduct, -S >: Grouped <: Single, V] = ColumnMappingSQL[F, S, M, V]
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
		extends ConvertedColumnSQL[F, S, X, Y]
		   with ColumnMappingSQL[F, S, M, Y]
		   with ConvertedMappingSQL[F, S, M, X, Y]
		   with ConvertedMappingSQLTemplate
                [F, S, M, X, Y, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, v] = ColumnMappingSQL[f, s, M, v] })#E]
	{
//		override val value :ColumnMappingSQL[F, S, M, X]

//		override val mapping  :M[Origin] = value.mapping
		override val export        :TypedColumn[M[Unit]#Subject, Origin] = value.export
		override lazy val anchored :TypedColumn[M[Unit]#Subject, Origin] = value.anchored //other mappings we inherit from TransformedMappingDefaults
//
//		override def default :ColumnMappingSQL[F, S, M, Y] = transformation(value.default)
//
//		override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
//				:ColumnMappingSQL[F, S, M, Y] =
//			transformation(value.defaultWith(includes, excludes))
//
//		override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
//				:ColumnMappingSQL[F, S, M, Y] =
//			transformation(value.alter(includes, excludes))

//		protected override def convert[Z](conversion :SQLConversion[Y, Z]) :ColumnMappingSQL[F, S, M, Z] =
//			(this.transformation andThen conversion)(value)
//
//		override def anchor(from :F) :ColumnMappingSQL[F, S, M, Y] = value.anchor(from) match {
//			case same if same eq value => this
//			case other => transformation(other)
//		}
//		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :ColumnMappingSQL[E, S, M, Y] =
//			reapply(value.basedOn(base))
//
//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :ColumnMappingSQL[E, S, M, Y] =
//			reapply(value.expand(base))
//
//		private def reapply[E <: RowProduct, C >: Grouped <: Single]
//		                   (e :ColumnMappingSQL[E, C, M, X]) :ColumnMappingSQL[E, C, M, Y] = transformation(e)

		//overridden because ColumnMappingSQL has a definition
//		protected override def potentialColumns(permissions :Permissions)
//		                                       (implicit spelling :SQLSpelling) :AlignableColumns =
//			spelling.potentialColumns(value, permissions)

		protected override def mappingForm(implicit spelling :SQLSpelling) :ColumnForm[M[Unit]#Subject] =
			anchored.form
//			spelling.mappingForm(value)

		protected override def realign(reordering :Rearrangement)
		                              (implicit spelling :SQLSpelling) :ColumnMappingSQL[F, S, M, Y] =
			super[ColumnMappingSQL].realign(reordering)

		protected override def visit[R](visitor :SpecificColumnVisitor[F, S, Y, R]) :R =
			visitor.convertedColumnMapping(this)
		
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
					Got((convert.value, convert.transformation))
				case _ => Lack
			}


		type __ = ConvertedColumnMappingSQL[_ <: RowProduct, _ >: Grouped <: Single, M, _, _] forSome {
			type M[A] <: ColumnAt[A]
		}

		private class Impl[-F <: RowProduct, -S >: Grouped <: Single, M[A] <: ColumnAt[A], X, Y]
		                  (override val value :ColumnMappingSQL[F, S, M, X],
		                   override val transformation :SQLConversion[X, Y])
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


	type ColumnMappingSQLShape[M[A] <: ColumnAt[A], V] = ColumnMappingSQL[Nothing, Grouped, M, V]



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

		override def convertedColumnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], X, V]
		                                   (e :ConvertedColumnMappingSQL[F, S, M, X, V]) :Y[S, V] =
			columnMapping(e)

		override def columnMappingTerm[M[O] <: BaseColumn[V, O], V](e :ColumnMappingTerm[M, V]) :Y[Single, V] =
			columnMapping(e)

		override def editedColumn[M[A] <: BaseColumn[X, A], X, V](e :EditedColumnSQL[F, M, X, V]) :Y[Single, X] =
			columnMapping(e)
	}

}

package net.noresttherein.oldsql.sql.ast

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{ColumnForm, SQLForm}
import net.noresttherein.oldsql.schema.ColumnMapping.ColumnAt
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.slang.castTypeParam
import net.noresttherein.oldsql.sql.{ColumnSetter, ColumnSQL, ComponentSetter, RowProduct, Seal, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ConvertingColumnTemplate, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingTemplate, GroundingTemplate, Grouped, Single, SingleRowSQLTemplate, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.ConvertedColumnLValue.{AnyConvertedColumnLValueVisitor, CaseAnyConvertedColumnLValue, CaseSpecificConvertedColumnLValue, SpecificConvertedColumnLValueVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.{AnyColumnLValueVisitor, ConvertedColumnLValue, SpecificColumnLValueVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.ConvertedColumnMappingSQL
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL.{AnyComponentVisitor, CaseAnyComponent, CaseSpecificComponent, SpecificComponentVisitor}
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL.{AnyColumnComponentVisitor, CaseAnyColumnComponent, CaseSpecificColumnComponent, SpecificColumnComponentVisitor}
import net.noresttherein.oldsql.sql.ast.LooseColumn.{AnyLooseColumnVisitor, CaseAnyLooseColumn, CaseSpecificLooseColumn, SpecificLooseColumnVisitor}
import net.noresttherein.oldsql.sql.ast.LooseComponent.{AnyLooseComponentVisitor, CaseAnyLooseComponent, CaseSpecificLooseComponent, SpecificLooseComponentVisitor}
import net.noresttherein.oldsql.sql.ast.LValueSQL.{ConvertedLValueSQL, LValueTemplate}
import net.noresttherein.oldsql.sql.ast.LValueSQL.ConvertedLValueSQL.{AnyConvertedLValueVisitor, CaseAnyConvertedLValue, CaseSpecificConvertedLValue, SpecificConvertedLValueVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ConvertedMappingSQL, MappingSQLTemplate}
import net.noresttherein.oldsql.sql.mechanics.{=~=, Reform, RelationCount, SQLConversion, SQLNumber, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount






/** An expression which can occur on the left side in the DSL for a ''set'' clause of an SQL ''update'',
  * representing a table component which value is set by an SQL ''insert'' or ''update'' statement.
  * It is a sealed trait with three distinct direct subtypes:
  *   1. [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], representing
  *      a direct use of a component, when the type of the ''r-value'' is the same as the subject type
  *      of the component;
  *   1. [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]], a non anchored
  *      (not linked to the owning table) component expression;
  *   1. [[net.noresttherein.oldsql.sql.ast.LValueSQL.ConvertedLValueSQL ConvertedLValueSQL]],
  *      which is a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]] subtype
  *      [[net.noresttherein.oldsql.sql.mechanics.SQLConversion converting]] the subject type of either
  *      a `ComponentSQL` or `LooseComponent` instance as a part of type unification with a ''r-value'' expression,
  *      originally of some different type.
  *
  * Additionally, there is a subtype for columns
  * [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL ColumnLValueSQL]],
  * with three direct subtypes mirroring the above.
  *
  * This type is used as part of [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]],
  * when creating DML ''update'' and ''insert'' statements as well as internally in preparation of an entity
  * before its saving. Note that, while representing a component, it is not
  * a [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]] subtype, as its value type may be different
  * than the mapping's `Subject`.
  * @tparam F $F
  * @tparam M $M. It is the type parameter of the underlying `ComponentSQL`.
  * @tparam V $V
  * @define V    the subject type of mapping `M` or a type to which it is promoted when unifying it
  *              with another expression.
  * @define this l-value
  * @define Cons `LValueSQL`
  * @define link [[net.noresttherein.oldsql.sql.ast.LValueSQL LValueSQL]]
  */
trait LValueSQL[-F <: RowProduct, M[O] <: MappingAt[O], V]
	extends MappingSQL[F, Single, M, V]
	   with ConvertingTemplate[F, Single, V, ({ type E[v] = LValueSQL[F, M, v] })#E]
	   with LValueTemplate[F, M, V, ({ type E[f <: RowProduct] = LValueSQL[f, M, V] })#E, LValueSQL[F, M, V]]
{ self =>
	/** The [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this instance's
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.mapping mapping]]. It is the most abstract supertype
	  * of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] this expression is based on which:
	  *   1. lists all the joins and join-like links
	  *      in their [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form;
	  *   1. Starts with a wildcard clause, followed by a join with a relation using this expression's mapping type.
	  *      This is the type returned by accessor methods
	  *      of [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]] (among others).
	  */ //docs copied from MappingSQL; added a lower bound
	override type Origin >: F <: RowProduct

	/** The underlying component expression (possibly this instance). */
	@throws[UnsupportedOperationException]("if the lvalue is not anchored (it is a LooseComponent).")
	def component  :ComponentSQL[F, M]
//	def conversion :SQLConversion[Subject, V]


	/** Creates an assignment object [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]]
	  * with the given expression as its ''r-value'' and this instance as its ''l-value'', used to set the value
	  * of this component as a part of an SQL ''insert'' or ''update''.
	  */ //consider: we should not allow to lift the left side I think. If anything, an explicit downcast API may be needed
	def :=[R <: RowProduct, Y](rvalue :SQLExpression[R, Single, Y])
	                          (implicit compat :V =~= Y) :ComponentSetter[F, R] =
		ComponentSetter(to(compat.left), compat.right(denullify(rvalue)))

	def :=[C <: MappingOf[V], R <: RowProduct, O <: RowProduct] //R and O are separate as R may be instantiated early from the expected type
	      (component :C)(implicit cast :C <:< TypedMapping[V, O], //todo: SQLTypeUnification
	                     subtype :SQLExpression[O, Single, V] <:< SQLExpression[R, Single, V],
	                     project :OriginProjection[C, V], offset :RelationCount[O, _ <: Numeral])
			:ComponentSetter[F, R] =
		this := subtype(LooseComponent(component))

	def :=[X](that :X)(implicit compat :V =~= X, form :SQLForm[X]) :ComponentSetter[F, RowProduct] =
		this := SQLTerm(that)

	//todo: we *could* get rid of the form param, as we will have one from the mapping.
	// We do not know however if we should use insert or update form, or something else (in EditedComponentSQL)
	// Moreover, in LooseComponent, we do not really have the final column set either.
	// Anchoring would have to be smarter and always reform. This conceivably could be done with a form with a zero/negative number of columns
	def :=?[Y](rvalue :Y)(implicit compat :V =~= Y, form :SQLForm[Y]) :ComponentSetter[F, RowProduct] =
		this := BoundParam(rvalue)

	/** Creates an expression for the given subcomponent of this component. The type of the returned expression
	  *  - [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  *  or [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] - will depend on the type
	  *  of this expression.
	  *  @param component a component mapping of `this.`[[net.noresttherein.oldsql.sql.ast.LValueSQL.mapping mapping]].
	  */
	@throws[NoSuchComponentException]("if component is not a component of this.mapping.")
	override def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
			:LValueSQL[F, project.WithOrigin, X] { type Origin = self.Origin }

	/** Creates an expression for the given column of this component.
	  * The type of the returned expression
	  *  - [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  *  or [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] - will depend on the type
	  *  of this expression.
	  *  @param column a column mapping of `this.`[[net.noresttherein.oldsql.sql.ast.LValueSQL.mapping mapping]].
	  */
	@throws[NoSuchComponentException]("if column is not a column of this.mapping.")
	override def \[K <: ColumnAt[Origin], X]
	     (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
			:ColumnLValueSQL[F, project.WithOrigin, X] { type Origin = self.Origin }


	/** Creates an SQL expression which substitutes certain columns of this component for arbitrary SQL expressions
	  * of compatible type. The [[net.noresttherein.oldsql.sql.ColumnSetter.lvalue l-values]] of the pairs must
	  * be instances of [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL ColumnLValueSQL]] for column mappings
	  * of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]] (including
	  * columns of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]),
	  * while the [[net.noresttherein.oldsql.sql.ColumnSetter.rvalue r-values]] must be expressions based on
	  * the same clause as this expression (or its subtype). The columns will be automatically included in the result
	  * using this instance's [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
	  * method.
	  *
	  * The arguments of this method are split into the first substitution and the rest in order to avoid erasure
	  * conflicts. All arguments are functions accepting the nominal mapping of this expression and pairing
	  * one of its columns with an arbitrary [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] expression.
	  * They work in a similar fashion to [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter alter]] family
	  * of methods of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], but instead of just selecting
	  * a column/component, they assign to it another expression. This can be done using extension method
	  * [[net.noresttherein.oldsql.sql.ast.LValueSQL.:= :=]] available on the argument
	  * through an implicit conversion:
	  * {{{
	  *     familiar.substitute(_.name := "Boo".?, _.species := "Hamster".?)
	  * }}}
	  */
	//Fixme: if FromLast is undefined then there is no implicit conversion from Mapping to LooseComponent.
	// The only thing stopping us is the bound on L type parameter in TypedComponentSQL
	def substitute[E <: F](substitute :M[FromLast] => ColumnSetter[FromLast, E],
	                       substitutes :M[FromLast] => ColumnSetter[FromLast, E]*) :MappingSQL[E, Single, M, V] =
	{
		val arg = mapping.castParam[FromLast]
		this.substitute(substitutes.view.map(_(arg)).prepended(substitute(arg)) to ArraySeq)
	}

	/** Creates an SQL expression which substitutes certain columns of this component for arbitrary SQL expressions
	  * of compatible type. The [[net.noresttherein.oldsql.sql.ColumnSetter.lvalue l-values]] of the pairs must
	  * be instances of [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL ColumnLValueSQL]] for column mappings
	  * of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]] (including
	  * columns of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]),
	  * while the [[net.noresttherein.oldsql.sql.ColumnSetter.rvalue r-values]] must be expressions based on
	  * the same clause as this expression (or its subtype). The columns will be automatically included in the result
	  * using this instance's [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
	  * method.
	  *
	  * As [[net.noresttherein.oldsql.sql.ast.LValueSQL.FromLast FromLast]] type of this expression will not
	  * be known in most contexts, `this.`[[net.noresttherein.oldsql.sql.ast.LValueSQL.asLast asLast]]
	  * can be used to obtain a reference to a copy of this component expression grounded in `this.FromLast`.
	  */
	def substitute[E <: F](substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V]

	def apply[E <: F](substitute :M[FromLast] => ColumnSetter[FromLast, E],
	                  substitutes :M[FromLast] => ColumnSetter[FromLast, E]*) :MappingSQL[E, Single, M, V] =
	{
		val arg = mapping.castParam[FromLast]
		this.substitute(substitutes.view.map(_(arg)).prepended(substitute(arg)) to ArraySeq)
	}

	def apply[E <: F](substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V] =
		substitute(substitutes)

	protected override def convert[X](conversion :SQLConversion[V, X]) :LValueSQL[F, M, X] =
		ConvertedLValueSQL(this, conversion)

	//consider moving this down, as the type itself does not implement selectFrom and is open for extension
	override type isSelectable = true

//	protected override def reformer[F1 <: F, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//	                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
//	                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//	                               (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
//	                                         spelling :SQLSpelling)
//			:SpecificExpressionVisitor
//			 [F2, S2, V2, (leftResult.SQLResult[F1, S1, LValueSQL[F1, M, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
//		new LValueReformer[F1, S1, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](other)(reform, passCount)
//
//	protected class LValueReformer[F1 <: F, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//	                               EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U,
//	                               LR[f <: RowProduct, s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U],
//	                               RR[f <: RowProduct, s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U]]
//	                              (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//	                              (implicit leftResult  :SQLTransformation[V, U]#Into[LR],
//	                                        rightResult :SQLTransformation[V2, U]#Into[RR], spelling :SQLSpelling)
//		extends BaseReformer

//	//all reform methods are overridden to narrow down the left result to ComponentLValueSQL
//	//this method should be overridden in subclasses
//	//never makes callbacks on other because there is no reformSpecific(other :ComponentLValueSQL)
//	protected override def reform[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                             (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(reform.LValue[F, M, U], SQLExpression[E, C, U]) =
//		if (passesAllowed > 0)
//			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
//		else
//			reformer[E, C, X, U](other)(reform, passesAllowed).apply(other)
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
//	                             (other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(reform.LValue[F, M, U], reform.LValue[E, C, U]) =
//		if (passesAllowed > 0) //dispatch to the variant specific for ComponentLValueSQL
//			other.reform(this)(reform.swap, passesAllowed - 1).swap
//		else
//			(leftResult(this), rightResult(other)) match {
//				case (l, r) if !leftResult.isDerived || !rightResult.isDerived => //either l or r aren't ConversionSQL
//					reform(l, r)
//				case (l, r) =>
//					reform.default(l, r)
//			}
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], U]
//	                       (other :ComponentSQL[E, C])(reform :Reform, passesAllowed :Int)
//	                       (implicit leftResult :Lift[V, U], rightResult :Lift[other.Subject, U], spelling :SQLSpelling)
//			:(reform.LValue[F, M, U], reform.LValue[E, C, U]) =
//		this.reform(other :LValueSQL[E, C, other.Subject])(reform, passesAllowed)
//
//	private[sql] override def `->reform`[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                          (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                          (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(reform.LValue[F, M, U], SQLExpression[E, C, U]) =
//		this.reform(other)(reform, passesAllowed)
//
//	private[sql] override def `->reform`[E <: RowProduct, C[O] <: MappingAt[O], X, U]
//	(other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
//	(implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(reform.LValue[F, M, U], reform.LValue[E, C, U]) =
//		this.reform(other)(reform, passesAllowed)
//
//	private[sql] override def `->reform`[E <: RowProduct, C[O] <: MappingAt[O], U]
//	                          (other :ComponentSQL[E, C])(reform :Reform, passesAllowed :Int)
//	                          (implicit leftResult :Lift[V, U], rightResult :Lift[other.Subject, U],
//	                           spelling :SQLSpelling)
//			:(reform.LValue[F, M, U], reform.LValue[E, C, U]) =
//		this.reform(other)(reform, passesAllowed)
//
//
//	protected override def reformer[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                               (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                               (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:SpecificExpressionVisitor[E, C, X, (reform.LValue[F, M, U], SQLExpression[E, C, U])] =
//		new SpecificLValueReformer[reform.LValue, E, C, X, U](other)(reform, passesAllowed)
//
//
//	protected class SpecificLValueReformer[LV[-G <: RowProduct, A[O] <: MappingAt[O], T] <: SQLExpression[G, Single, T],
//	                               E <: RowProduct, C >: Grouped <: Single, X, U]
//	                              (other :SQLExpression[E, C, X])(reform :ReformTo[LV], passesAllowed :Int)
//	                              (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//		extends CaseSpecificExpression[E, C, X, (LV[F, M, U], SQLExpression[E, C, U])]
//	{
//		protected def mayPass :Boolean = passesAllowed > 0
//
//		protected def pass(e :SQLExpression[E, C, X]) :(LV[F, M, U], SQLExpression[E, C, U]) =
//			//if passesAllowed > 0, then this is like in the reform method; otherwise e is a conversion, select, etc.
//			e.`->reform`(LValueSQL.this)(reform.swap, passesAllowed - 1).swap
//
//		protected def pass[N[O] <: MappingAt[O]](e :LValueSQL[E, N, X]) :(LV[F, M, U], LV[E, N, U]) =
//			e.`->reform`(LValueSQL.this)(reform.swap, passesAllowed - 1).swap
//
//		protected def fallback[G <: F](other :SQLExpression[E, C, X])
//		                              (left :LValueSQL[G, M, U] = leftResult(LValueSQL.this),
//		                               right :SQLExpression[E, C, U] = rightResult(other))
//				:(LV[G, M, U], SQLExpression[E, C, U]) =
//			if (!leftResult.isDerived || !rightResult.isDerived)
//				reform(left, right)
//			else if (mayPass) //this has been already checked by the standard flow, but lets keep it universal
//				pass(other)
//			else
//				reform.fallback(left, right)
//
//		override def expression(e :SQLExpression[E, C, X]) :(LV[F, M, U], SQLExpression[E, C, U]) =
//			if (mayPass) pass(e)
//			else fallback(e)()
//
//		override def component[O >: E <: RowProduct, T[A] <: BaseMapping[R, A], R, N[A] <: BaseMapping[X, A], L <: RowProduct]
//		                      (e :TypedComponentSQL[O, T, R, N, X, L]) :(LV[F, M, U], LV[E, N, U]) =
//			LValueSQL.this.reform(e)(reform, passesAllowed)
//
//		override def convertedLValue[N[O] <: MappingAt[O]](e :ConvertedLValueSQL[E, N, X])
//				:(LV[F, M, U], LV[E, N, U]) =
//			LValueSQL.this.reform(e)(reform, passesAllowed)
//	}


	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnLValueSQL.from[F]#__]

	private[oldsql] override def `->split`(spelling :SQLSpelling) :Seq[ColumnLValueSQL.from[F]#__] = split(spelling)


//	override def isomorphic(that :SQLExpression.*) :Boolean = that match {
//		case self :AnyRef if this eq self => true
//		case other :ComponentLValueSQL.* => mapping isomorphic other.mapping
////			(component.origin isomorphic other.component.origin) && (mapping isomorphic other.mapping)
//		case _ => false
//	}

	protected[sql] def all_subclasses_of_ComponentLValueSQL_must_extend_TypedComponentSQL_or_LooseComponent(seal :Seal) :Unit
}




object LValueSQL {

	type __ = LValueSQL[_ <: RowProduct, M, _] forSome { type M[O] <: MappingAt[O] }

	type from[-F <: RowProduct] = {
		type __ = LValueSQL[F, M, _] forSome { type M[O] <: MappingAt[O] }
//		type apply[M[O] <: MappingAt[O], V] = LValueSQL[F, M, V]
		type meta[M[O] <: MappingAt[O]] = {
			type __    = LValueSQL[F, M, _]
			type of[V] = LValueSQL[F, M, V]
			type E[V]  = LValueSQL[F, M, V]
		}
		type E[V] = LValueSQL[F, M, V] forSome { type M[O] <: MappingAt[O] }
		type of[V] = LValueSQL[F, M, V] forSome { type M[O] <: MappingAt[O] }
	}
//	type AnyIn[-F <: RowProduct] = LValueSQL[F, M, _] forSome { type M[O] <: MappingAt[O] }
//	type Cast[-F <: RowProduct, V] = LValueSQL[F, M, V] forSome { type M[O] <: MappingAt[O] }


	/** The bulk of [[net.noresttherein.oldsql.sql.ast.LValueSQL LValueSQL]] API, in particular all methods
	  * which return an `LValueSQL` of the same class.
	  */
	trait LValueTemplate[-F <: RowProduct, M[O] <: MappingAt[O], V, +Cons[f <: RowProduct] <: LValueSQL[f, M, V],
	                     +Same <: LValueSQL[F, M, V]]
		extends GroundingTemplate[F, Single, V, Cons, Same]
		   with MappingSQLTemplate[F, Single, M, V, Same]
	{ self :Same =>
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

		override def isSingleRow = true

		override def asSingleRow :Option[Same] = Some(this)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Cons[E] =
			expand(base)(expansion.asExpandedBy, implicitly[Single <:< Single])

		//We pass the call to the parameterless variant, so the result will not be anchored unless overridden.
		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single) :Cons[E] =
			expand[E]

		/** An overloaded parameterless variant of the standard [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]]
		  * method of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], taking advantage of the fact that
		  * a component expression does not depend on any relations other than the one included with it.
		  */ //cannot be in MappingSQL method because of, for example, EditedComponent
		def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :Cons[E]

		/** Grounds this component expression in a singleton clause
		  * [[net.noresttherein.oldsql.sql.ast.LValueSQL.FromLast FromLast]] consisting of `M` as its single
		  * relation joined with [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] wildcard clause with a join
		  * of type proper to the underlying [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]
		  * relation expression. This method is used in particular to create an l-value expression
		  * for a [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]]:
		  * {{{
		  *     val component = origin \ mapping
		  *     val setter = component.asLast := subject
		  * }}}
		  */
		def asLast :Cons[FromLast]
	}




	/** Base class for expressions applying conversions between types with the same database representation
	  * (or database types which are autoconverted between each other). It is a specialization
	  * of the universal [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]] which retains the information
	  * about the mapping type of the converted component and,
	  * like a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] itself, can be used as a left side
	  * of assignments `:=` defined in [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]].
	  */
	sealed class ConvertedLValueSQL[-F <: RowProduct, M[O] <: MappingAt[O], V] protected
	                               (override val value :LValueSQL[F, M, M[Unit]#Subject],
	                                override val adaptation :SQLConversion[M[Unit]#Subject, V])
		extends ConvertedMappingSQL[F, Single, M, M[Unit]#Subject, V] with LValueSQL[F, M, V]
	{
		override type FromLast  = value.FromLast
//		override type Entity[O] = value.Entity[O]
		override def component :ComponentSQL[F, M] = value.component

		override def \[K <: MappingAt[value.Origin], X](component :K)(implicit project :OriginProjection[K, X])
				:LValueSQL[F, project.WithOrigin, X] { type Origin = value.Origin } =
			value \ component

		override def \[K <: ColumnAt[value.Origin], X]
		              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
				:ColumnLValueSQL[F, project.WithOrigin, X] { type Origin = value.Origin } =
			value \ column


		override def groundValue :Opt[V] = Lack

		override def isDefault :Boolean = value.isDefault

		override def default :LValueSQL[F, M, V] = if (isDefault) this else adaptation(value.default)

		override def defaultWith(includes :Unique[TypedMapping[_, value.Origin]],
		                         excludes :Unique[TypedMapping[_, value.Origin]]) :LValueSQL[F, M, V] =
			adaptation(value.defaultWith(includes, excludes))

		override def alter(includes :Iterable[TypedMapping[_, value.Origin]],
		                   excludes :Iterable[TypedMapping[_, value.Origin]]) :LValueSQL[F, M, V] =
			adaptation(value.alter(includes, excludes))

		override def substitute[E <: F]
		                       (substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V] =
			adaptation(value.substitute(substitutes))

		protected override def convert[Y](conversion :SQLConversion[V, Y]) :LValueSQL[F, M, Y] =
			(this.adaptation andThen conversion)(value)

		override def anchor(from :F) :LValueSQL[F, M, V] =
			value.anchor(from) match {
				case same if same eq value => this
				case other => adaptation(other)
			}

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
				:LValueSQL[E, M, V] =
			adaptation(value.expand(base))

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :LValueSQL[E, M, V] =
			adaptation(value.expand[E])

		override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                    (e :ColumnSQL[E, C, M[Unit]#Subject]) :ColumnSQL[E, C, V] = adaptation(e)

		override def asLast :LValueSQL[FromLast, M, V] = adaptation(value.asLast)

//		override def reorder(permutation :IndexedSeq[Int]) :ComponentLValueSQL[F, M, V] =
//			if (permutation == permutation.indices) this
//			else value.reorder(permutation).to(lift)

		protected override def reform[F1 <: F, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.SQLResult[F1, S1, LValueSQL[F1, M, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
			if (this eq other)
				(leftResult(this), rightResult(other))
			else if (passCount.firstTime)
				super.reform(other)(reform, passCount)
			else
				reform(value, other)(adaptation andThen leftResult, rightResult, spelling)
//
//		protected override def reform[E <: RowProduct, C >: Grouped <: Single, X, U]
//		                             (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//		                             (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//				:(reform.LValue[F, M, U], SQLExpression[E, C, U]) =
//			if (passesAllowed >= 3)
//				super.reform(other)(reform, passesAllowed)
//			else {
//				implicit val compat = (conversion andThen leftResult) vs rightResult
//				reform(value, other)
//			}
//
//		protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
//		                             (other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
//		                             (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//				:(reform.LValue[F, M, U], reform.LValue[E, C, U]) =
//			if (passesAllowed >= 3)
//				other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
//			else {
//				implicit val compat = (conversion andThen leftResult) vs rightResult
//				reform(value, other)
//			}

		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnLValueSQL.from[F]#__] =
			spelling.split(value)

		protected override def visit[R](visitor :SpecificExpressionVisitor[F, Single, V, R]) :R =
			visitor.convertedLValue(this)

		protected override def visit[R[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyExpressionVisitor[F, R]) :R[Single, V] =
			visitor.convertedLValue(this)
//
//		protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//		                             E >: LValueSQL[F_, M, V] <: SQLExpression[F_, S_, V],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ExpressionVisitor[RowProduct, Y]) :Y[S_, V, E] =
//			visitor.convertedLValue(this)

		protected[sql] override
		def all_subclasses_of_ComponentLValueSQL_must_extend_TypedComponentSQL_or_LooseComponent(seal :Seal) :Unit = ()
	}


	object ConvertedLValueSQL {
		def apply[F <: RowProduct, M[O] <: BaseMapping[X, O], X, Y]
		         (component :LValueSQL[F, M, X], conversion :SQLConversion[X, Y]) :ConvertedLValueSQL[F, M, Y] =
			new ConvertedLValueSQL(component, conversion)

		def unapply[F <: RowProduct, M[O] <: MappingAt[O], V](lvalue :LValueSQL[F, M, V])
				:Opt[(LValueSQL[F, M, M[Unit]#Subject], SQLConversion[M[Unit]#Subject, V])] =
			lvalue match {
				case set :ConvertedLValueSQL[F, M, V] => Got(set.value, set.adaptation)
				case _ => Lack
			}

		trait SpecificConvertedLValueVisitor[+F <: RowProduct, X, +R] extends SpecificConvertedColumnLValueVisitor[F, X, R] {
			def convertedLValue[M[O] <: MappingAt[O]](e :ConvertedLValueSQL[F, M, X]) :R
		}
		trait MatchSpecificConvertedLValue[+F <: RowProduct, Y, +R] extends SpecificConvertedLValueVisitor[F, Y, R] {
			override def convertedColumnLValue[M[O] <: BaseColumn[X, O], X](e :ConvertedColumnLValue[F, M, X, Y]) :R =
				convertedLValue(e)
		}
		type CaseSpecificConvertedLValue[+F <: RowProduct, X, +R] = MatchSpecificConvertedLValue[F, X, R]
//
//
//		trait ConvertedLValueVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends ConvertedColumnLValueVisitor[F, Y]
//		{
//			def convertedLValue[M[O] <: MappingAt[O], V]
//			                   (e :ConvertedLValueSQL[F, M, V]) :Y[Single, V, ConvertedLValueSQL[F, M, V]]
//		}
//		trait MatchConvertedLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends ConvertedLValueVisitor[F, Y] with CaseConvertedColumnLValue[F, Y]
//		{
//			override def convertedColumnLValue[M[O] <: BaseColumn[X, O], X, V](e :ConvertedColumnLValue[F, M, X, V])
//					:Y[Single, V, ConvertedColumnLValue[F, M, X, V]] =
//				convertedLValue(e)
//		}
//		type CaseConvertedLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//			MatchConvertedLValue[F, Y]


		trait AnyConvertedLValueVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
			extends AnyConvertedColumnLValueVisitor[F, R]
		{
			def convertedLValue[M[O] <: MappingAt[O], V](e :ConvertedLValueSQL[F, M, V]) :R[Single, V]
		}
		trait MatchAnyConvertedLValue[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
			extends AnyConvertedLValueVisitor[F, R]
		{
			override def convertedColumnLValue[M[O] <: BaseColumn[S, O], S, X]
			                                  (e :ConvertedColumnLValue[F, M, S, X]) :R[Single, X] =
				convertedLValue(e)

		}
		type CaseAnyConvertedLValue[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] =
			MatchAnyConvertedLValue[F, R]
	}



	trait SpecificLValueVisitor[+F <: RowProduct, X, +Y]
		extends SpecificColumnLValueVisitor[F, X, Y] with SpecificConvertedLValueVisitor[F, X, Y]
		   with SpecificComponentVisitor[F, X, Y] with SpecificLooseComponentVisitor[F, X, Y]

	trait MatchSpecificLValue[+F <: RowProduct, X, +Y]
		extends SpecificLValueVisitor[F, X, Y] with CaseSpecificConvertedLValue[F, X, Y]
		   with CaseSpecificComponent[F, X, Y] with CaseSpecificLooseComponent[F, X, Y]
	{
		def lvalue[M[O] <: MappingAt[O]](e :LValueSQL[F, M, X]) :Y
	}

	trait CaseSpecificLValue[+F <: RowProduct, X, +Y] extends MatchSpecificLValue[F, X, Y] {
		override def convertedLValue[M[O] <: MappingAt[O]](e :ConvertedLValueSQL[F, M, X]) :Y = lvalue(e)

		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[X, A]](e :LooseComponent[O, M, X]) :Y =
			lvalue(e)

		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[X, A], L <: RowProduct]
		                      (e :TypedComponentSQL[O, T, R, M, X, L]) :Y =
			lvalue(e)
	}
//
//
//	trait LValueVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ColumnLValueVisitor[F, Y]
//		   with ComponentVisitor[F, Y] with LooseComponentVisitor[F, Y] with ConvertedLValueVisitor[F, Y]
//
//	trait MatchLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends LValueVisitor[F, Y] with CaseComponent[F, Y] with CaseLooseComponent[F, Y] with CaseConvertedLValue[F, Y]
//	{
//		def lvalue[M[O] <: MappingAt[O], V](e :LValueSQL[F, M, V]) :Y[Single, V, LValueSQL[F, M, V]]
//	}
//	trait CaseLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchLValue[F, Y]
//	{
//		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
//		                      (e :TypedComponentSQL[O, T, R, M, V, L])
//				:Y[Single, V, TypedComponentSQL[O, T, R, M, V, L]] =
//			lvalue(e)
//
//		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[V, A], V]
//		                           (e :LooseComponent[O, M, V]) :Y[Single, V, LooseComponent[O, M, V]] = lvalue(e)
//
//		override def convertedLValue[M[A] <: MappingAt[A], V]
//		                            (e :ConvertedLValueSQL[F, M, V]) :Y[Single, V, ConvertedLValueSQL[F, M, V]] =
//			lvalue(e)
//	}


	trait AnyLValueVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnLValueVisitor[F, Y] with AnyConvertedLValueVisitor[F, Y]
		   with AnyComponentVisitor[F, Y] with AnyLooseComponentVisitor[F, Y]

	trait MatchAnyLValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyLValueVisitor[F, Y] with CaseAnyConvertedLValue[F, Y]
		   with CaseAnyComponent[F, Y] with CaseAnyLooseComponent[F, Y]
	{
		def lvalue[M[O] <: MappingAt[O], V](e :LValueSQL[F, M, V]) :Y[Single, V]
	}

	trait CaseAnyLValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyLValue[F, Y]
	{
		override def convertedLValue[M[O] <: MappingAt[O], V](e :ConvertedLValueSQL[F, M, V]) :Y[Single, V] =
			lvalue(e)

		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[S, A], S]
		                           (e :LooseComponent[O, M, S]) :Y[Single, S] =
			lvalue(e)

		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[S, A], S, L <: RowProduct]
		                      (e :TypedComponentSQL[O, T, R, M, S, L]) :Y[Single, S] =
			lvalue(e)
	}
}






/** An expression which can occur on the left side in the DSL for a ''set'' clause of an SQL ''update'',
  * representing an updated column of a table. It is a sealed trait with three distinct direct subtypes:
  *   1. [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL ColumnMappingSQL]], representing
  *      a direct use of a column, when the type of the ''r-value'' is the same as the subject type
  *      of the column;
  *   1. [[net.noresttherein.oldsql.sql.ast.LooseColumn LooseColumn]], an non anchored (not linked
  *      to the owning table) table column expression;
  *   1. [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.ConvertedColumnLValue ConvertedColumnLValue]],
  *      which is a [[net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL ConvertedColumnSQL]]
  *      subtype [[net.noresttherein.oldsql.sql.mechanics.SQLConversion converting]] the subject type of a
  *      `ColumnMappingSQL` or `LooseColumn` instance as a part of type unification with a ''r-value'' expression,
  *      originally of some different type;
  *
  * This type is used as part of [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]],
  * when creating DML ''update'' and ''insert'' statements as well as internally in preparation of an entity
  * before its saving.
  * @tparam F $F
  * @tparam M $M. It is the type parameter of the underlying `ColumnMappingSQL`.
  * @tparam V $V
  * @define this column l-value
  * @define Cons `ColumnLValueSQL`
  * @define link [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL ColumnLValueSQL]]
  */
trait ColumnLValueSQL[-F <: RowProduct, M[O] <: BaseColumn[_, O], V]
	extends ColumnMappingSQL[F, Single, M, V] with LValueSQL[F, M, V]
	   with ConvertingColumnTemplate[F, Single, V, ({ type E[v] = ColumnLValueSQL[F, M, v] })#E]
	   with LValueTemplate[F, M, V, ({ type E[f <: RowProduct] = ColumnLValueSQL[f, M, V] })#E, ColumnLValueSQL[F, M, V]]
{ self =>
	override def component :GenericColumnComponentSQL[F, M, _]

	/** The form for the value type, which will be used for setting JDBC parameters if used as the right side
	  * in an assignment to this column expression.
	  */
	def form :ColumnForm[V] //we need it for :=, which accepts only SQLForm, but implementation requires a column form.

	/** Implementation of `:=` left for subclasses, extracted to a different method
	  * in order to avoid overloading problems. Impossible to implement here as we don't know the column subject type.
	  * @return `ColumnSetter(to(compat.left), denullify(rvalue).to(promote.right))`.
	  */
	protected def setter[R <: RowProduct, Y]
	                    (rvalue :ColumnSQL[R, Single, Y])(implicit compat :V =~= Y) :ColumnSetter[F, R]

	/** Creates an assignment object [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]]
	  * with the given expression as its ''r-value'' and this instance as its ''l-value'', used to set the value
	  * of this component as a part of an SQL ''insert'' or ''update''.
	  */
	def :=[R <: RowProduct, Y](rvalue :ColumnSQL[R, Single, Y])(implicit compat :V =~= Y) :ColumnSetter[F, R] =
		setter(rvalue)

	def :=[E <: F, O <: RowProduct] //E and O are separate as E may be instantiated early from the expected type
	      (component :BaseColumn[V, O]) //todo: variants for columns and literals for other assignment methods
	      (implicit subtype :ColumnSQL[O, Single, V] <:< ColumnSQL[E, Single, V],
	       offset :RelationCount[O, _ <: Numeral])
			:ColumnSetter[F, E] =
		this := subtype(LooseColumn(component).toColumnSQL)

	override def :=[X](that :X)(implicit compat :V =~= X, form :SQLForm[X]) :ComponentSetter[F, RowProduct] =
		to(compat.left) := SQLTerm(compat.right(that))(compat.left(this.form))

	//todo: we *could* get rid of the form param, as we will have one from the mapping.
	// We do not know however if we should use insert or update form, or something else (in EditedComponentSQL)
	// Moreover, in LooseComponent, we do not really have the final column set either.
	// Anchoring would have to be smarter and always reform. This conceivably could be done with a form with a zero/negative number of columns
	override def :=?[Y, U](rvalue :Y)(implicit compat :(V =~= Y)#As[U], form :ColumnForm[U])
			:ColumnSetter[F, RowProduct] =
		this := BoundColumnParam(rvalue)

	def +=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, Single, Y])
	      (implicit compat :(V =~= Y)#As[U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R] =
		to(compat.left) := expand[R] + denullify(rvalue)

	def +=?[Y, U](rvalue :Y)(implicit compat :(V =~= Y)#As[U], math :SQLNumber[U])
			:ColumnSetter[F, RowProduct] =
		this += BoundColumnParam(form)(rvalue)

	def -=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, Single, Y])
	      (implicit compat :(V =~= Y)#As[U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R] =
		to(compat.left) := expand[R] - denullify(rvalue)

	def -=?[Y, U](rvalue :Y)(implicit compat :(V =~= Y)#As[U], math :SQLNumber[U])
			:ColumnSetter[F, RowProduct] =
		this -= BoundColumnParam(form)(rvalue)

	def *=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, Single, Y])
	      (implicit compat :(V =~= Y)#As[U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R] =
		to(compat.left) := expand[R] * denullify(rvalue)

	def *=?[Y, U](rvalue :Y)(implicit compat :(V =~= Y)#As[U], math :SQLNumber[U])
			:ColumnSetter[F, RowProduct] =
		this *= BoundColumnParam(form)(rvalue)

	def /=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, Single, Y])
	      (implicit compat :(V =~= Y)#As[U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R] =
		to(compat.left) := expand[R] / denullify(rvalue)

	def /=?[Y, U](rvalue :Y)(implicit compat :(V =~= Y)#As[U], math :SQLNumber[U])
			:ColumnSetter[F, RowProduct] =
		this /= BoundColumnParam(form)(rvalue)

	def %=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, Single, Y])
	      (implicit compat :(V =~= Y)#As[U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R] =
		to(compat.left) := expand[R] % denullify(rvalue)

	def %=?[Y, U](rvalue :Y)(implicit compat :(V =~= Y)#As[U], math :SQLNumber[U])
			:ColumnSetter[F, RowProduct] =
		this %= BoundColumnParam(form)(rvalue)


	def &&=[R <: RowProduct, Y](rvalue :ColumnSQL[R, Single, Y])
	                           (implicit compat :(V =~= Y)#As[Boolean], expansion :F ExpandedBy R)
			:ColumnSetter[F, R] =
		to(compat.left) := expand[R].to(compat.left) && compat.right(rvalue)

	def &&=?[Y](rvalue :Y)(implicit compat :(V =~= Y)#As[Boolean]) :ColumnSetter[F, RowProduct] =
		this &&= BoundColumnParam(form)(rvalue)

	def ||=[R <: RowProduct, Y](rvalue :ColumnSQL[R, Single, Y])
	                           (implicit compat :(V =~= Y)#As[Boolean], expansion :F ExpandedBy R)
			:ColumnSetter[F, R] =
		to(compat.left) := expand[R].to(compat.left) || compat.right(rvalue)

	def ||=?[Y](rvalue :Y)(implicit compat :(V =~= Y)#As[Boolean]) :ColumnSetter[F, RowProduct] =
		this ||= BoundColumnParam(form)(rvalue)

//		def ^= [R <: RowProduct, Y](rvalue :ColumnSQL[R, GlobalScope, Y])
//		                  (implicit promote :SQLTypeUnification[V, Y, Boolean]) :ColumnSetter[F, R, Boolean] =
//			to(compat.left) := to(compat.left) ^ promote.right(rvalue)


	def ++=[R <: RowProduct, Y](rvalue :ColumnSQL[R, Single, Y])
	                           (implicit compat :(V =~= Y)#As[String], expansion :F ExpandedBy R)
			:ColumnSetter[F, R] =
		to(compat.left) := expand[R].to(compat.left) ++ compat.right(denullify(rvalue))

	def ++=?[Y](rvalue :Y)(implicit compat :(V =~= Y)#As[String]) :ColumnSetter[F, RowProduct] =
		this ++= BoundColumnParam(form)(rvalue)

//		override def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[V, Y, U], form :SQLForm[Y])
//				:ComponentSetter[F, RowProduct, U] =  //overriden for correct overloading
//			this := BoundParam(rvalue)


	protected override def convert[X](conversion :SQLConversion[V, X]) :ColumnLValueSQL[F, M, X] =
		ConvertedColumnLValue(this, conversion)

//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :PartOf[U, E]) :ColumnLValueSQL[E, M, V] =
//		expand[E]
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//			:ColumnLValueSQL[E, M, V] =
//		expand[E]
//
//	override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ColumnLValueSQL[E, M, V]
//
//	override def asLast :ColumnLValueSQL[FromLast, M, V]


	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnLValueSQL[F, M, _]] =
		PassedArray :+ (component :ColumnLValueSQL[F, M, _])

	protected[sql] def all_subclasses_of_ColumnLValueSQL_must_extend_TypedColumnSQL_or_LooseColumn(seal :Seal) :Unit
}



object ColumnLValueSQL {

	type __ = ColumnLValueSQL[_ <: RowProduct, M, _] forSome { type M[O] <: BaseColumn[_, O] }
	type AnyIn[-F <: RowProduct] = ColumnLValueSQL[F, M, _] forSome { type M[O] <: BaseColumn[_, O] }

	type from[-F <: RowProduct] = {
		type __ = ColumnLValueSQL[F, M, _] forSome { type M[O] <: BaseColumn[_, O] }
		type meta[M[O] <: BaseColumn[_, O]] = {
			type __ = ColumnLValueSQL[F, M, _]
			type of[V] = ColumnLValueSQL[F, M, V]
//			type C[V] = ColumnLValueSQL[F, M, V]
			type E[V] = ColumnLValueSQL[F, M, V]
		}
		type E[V] = ColumnLValueSQL[F, M, V] forSome { type M[O] <: BaseColumn[_, O] }
//		type C[V] = ColumnLValueSQL[F, M, V] forSome { type M[O] <: BaseColumn[_, O] }
	}


	class ConvertedColumnLValue[-F <: RowProduct, M[O] <: BaseColumn[S, O], S, V]
	                           (override val value :ColumnLValueSQL[F, M, S], override val adaptation :SQLConversion[S, V])
		extends ConvertedLValueSQL[F, M, V](value, adaptation)
		   with ConvertedColumnMappingSQL[F, Single, M, S, V] with ColumnLValueSQL[F, M, V]
	{
		override def component :GenericColumnComponentSQL[F, M, _] = value.component
		override def form      :ColumnForm[V] = adaptation(value.form)

		override def default :ColumnLValueSQL[F, M, V] = if (isDefault) this else adaptation(value.default)

		override def defaultWith(includes :Unique[TypedMapping[_, value.Origin]],
		                         excludes :Unique[TypedMapping[_, value.Origin]]) :ColumnLValueSQL[F, M, V] =
			adaptation(value.defaultWith(includes, excludes))

		override def alter(includes :Iterable[TypedMapping[_, value.Origin]],
		                   excludes :Iterable[TypedMapping[_, value.Origin]]) :ColumnLValueSQL[F, M, V] =
			adaptation(value.alter(includes, excludes))

		protected override def convert[Y](conversion :SQLConversion[V, Y]) :ColumnLValueSQL[F, M, Y] =
			(this.adaptation andThen conversion)(value)

		override def anchor(from :F) :ColumnLValueSQL[F, M, V] = value.anchor(from) match {
			case same if same eq value => this
			case other => adaptation(other)
		}

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ColumnLValueSQL[E, M, V] =
			adaptation(value.expand[E])

		override def asLast :ColumnLValueSQL[FromLast, M, V] = adaptation(value.asLast)

		protected override def setter[R <: RowProduct, Y](rvalue :ColumnSQL[R, Single, Y])
		                                                 (implicit compat :V =~= Y)
				:ColumnSetter[F, R] =
			ColumnSetter[F, M, S, R, compat.Unified](to(compat.left), compat.right(denullify(rvalue)))


		protected override def visit[R](visitor :SpecificColumnVisitor[F, Single, V, R]) :R =
			visitor.convertedColumnLValue(this)

		protected override def visit[R[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyColumnVisitor[F, R]) :R[Single, V] =
			visitor.convertedColumnLValue(this)
//
//		protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//		                             E >: ColumnLValueSQL[F_, M, V] <: SQLExpression[F_, S_, V],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[RowProduct, Y]) :Y[S_, V, E] =
//			visitor.convertedColumnLValue(this)
	}


	object ConvertedColumnLValue {
		def apply[F <: RowProduct, M[O] <: BaseColumn[X, O], X, Y]
		         (column :ColumnLValueSQL[F, M, X], conversion :SQLConversion[X, Y]) :ConvertedColumnLValue[F, M, X, Y] =
			new ConvertedColumnLValue(column, conversion)

		def unapply[F <: RowProduct, M[O] <: BaseColumn[S, O], S, V](lvalue :LValueSQL[F, M, V])
				:Opt[(ColumnLValueSQL[F, M, S], SQLConversion[S, V])] =
			lvalue match {
				case set :ConvertedColumnLValue[F, M, S, V] @unchecked => Got((set.value, set.adaptation))
				case _ => Lack
			}

		def unapply[F <: RowProduct, V](e :SQLExpression[F, Grouped, V])
				:Opt[(GenericColumnComponentSQL[F, m, s], SQLConversion[s, V]) forSome { type m[O] <: BaseColumn[s, O]; type s }] =
			e match {
				case set :ConvertedColumnLValue[F, MappingOf[Any]#TypedColumnProjection, Any, V] @unchecked =>
					Got((set.value, set.adaptation))
				case _ => Lack
			}

		trait SpecificConvertedColumnLValueVisitor[+F <: RowProduct, Y, +R] {
			def convertedColumnLValue[M[O] <: BaseColumn[X, O], X](e :ConvertedColumnLValue[F, M, X, Y]) :R
		}
		type MatchSpecificConvertedColumnLValue[+F <: RowProduct, X, +R] = SpecificConvertedColumnLValueVisitor[F, X, R]
		type CaseSpecificConvertedColumnLValue[+F <: RowProduct, X, +R] = SpecificConvertedColumnLValueVisitor[F, X, R]
//
//		trait ConvertedColumnLValueVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//			def convertedColumnLValue[M[O] <: BaseColumn[X, O], X, V]
//			                         (e :ConvertedColumnLValue[F, M, X, V])
//					:Y[Single, V, ConvertedColumnLValue[F, M, X, V]]
//		}
//		type MatchConvertedColumnLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//			ConvertedColumnLValueVisitor[F, Y]
//		type CaseConvertedColumnLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//			ConvertedColumnLValueVisitor[F, Y]
//
//		trait ConvertedColumnLValueVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//			def convertedColumnLValue[M[O] <: BaseColumn[X, O], X, V](e :ConvertedColumnLValue[F, M, X, V])
//					:Y[Single, V, ConvertedColumnLValue[F, M, X, V]]
//		}
//		type MatchConvertedColumnLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//			ConvertedColumnLValueVisitor[F, Y]
//		type CaseConvertedColumnLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//			ConvertedColumnLValueVisitor[F, Y]


		trait AnyConvertedColumnLValueVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] {
			def convertedColumnLValue[M[O] <: BaseColumn[X, O], X, Y]
			                          (e :ConvertedColumnLValue[F, M, X, Y]) :R[Single, Y]
		}
		type MatchAnyConvertedColumnLValue[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] =
			AnyConvertedColumnLValueVisitor[F, R]
		type CaseAnyConvertedColumnLValue[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] =
			AnyConvertedColumnLValueVisitor[F, R]
	}



	trait SpecificColumnLValueVisitor[+F <: RowProduct, X, +Y] extends SpecificConvertedColumnLValueVisitor[F, X, Y]
		with SpecificColumnComponentVisitor[F, X, Y] with SpecificLooseColumnVisitor[F, X, Y]

	trait MatchSpecificColumnLValue[+F <: RowProduct, X, +Y]
		extends SpecificColumnLValueVisitor[F, X, Y] with CaseSpecificConvertedColumnLValue[F, X, Y]
		   with CaseSpecificColumnComponent[F, X, Y] with CaseSpecificLooseColumn[F, X, Y]
	{
		def columnLValue[M[O] <: BaseColumn[_, O]](e :ColumnLValueSQL[F, M, X]) :Y
	}

	trait CaseSpecificColumnLValue[+F <: RowProduct, X, +Y] extends MatchSpecificColumnLValue[F, X, Y] {
		override def convertedColumnLValue[M[A] <: BaseColumn[V, A], V]
		                                   (e :ConvertedColumnLValue[F, M, V, X]) :Y = columnLValue(e)

		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[X, A], L <: RowProduct]
		                            (e :TypedColumnComponentSQL[O, T, R, M, X, L]) :Y =
			columnLValue(e)

		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[X, A]](e :LooseColumn[O, M, X]) :Y =
			columnLValue(e)
	}
//
//
//	trait ColumnLValueVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ColumnComponentVisitor[F, Y] with LooseColumnVisitor[F, Y] with ConvertedColumnLValueVisitor[F, Y]
//
//	trait MatchColumnLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ColumnLValueVisitor[F, Y]
//		   with CaseColumnComponent[F, Y] with CaseLooseColumn[F, Y] with CaseConvertedColumnLValue[F, Y]
//	{
//		def columnLValue[M[O] <: BaseColumn[_, O], V]
//		                (e :ColumnLValueSQL[F, M, V]) :Y[Single, V, ColumnLValueSQL[F, M, V]]
//	}
//	trait CaseColumnLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchColumnLValue[F, Y]
//	{
//		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
//		                            (e :TypedColumnComponentSQL[O, T, R, M, V, L])
//				:Y[Single, V, TypedColumnComponentSQL[O, T, R, M, V, L]] =
//			columnLValue(e)
//
//		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
//		                        (e :LooseColumn[O, M, V]) :Y[Single, V, LooseColumn[O, M, V]] = columnLValue(e)
//
//		override def convertedColumnLValue[M[A] <: BaseColumn[X, A], X, V]
//		                                  (e :ConvertedColumnLValue[F, M, X, V])
//				:Y[Single, V, ConvertedColumnLValue[F, M, X, V]] =
//			columnLValue(e)
//	}


	trait AnyColumnLValueVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyConvertedColumnLValueVisitor[F, Y]
		   with AnyColumnComponentVisitor[F, Y] with AnyLooseColumnVisitor[F, Y]

	trait MatchAnyColumnLValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnLValueVisitor[F, Y] with CaseAnyConvertedColumnLValue[F, Y]
		   with CaseAnyColumnComponent[F, Y] with CaseAnyLooseColumn[F, Y]
	{
	    def columnLValue[M[O] <: BaseColumn[_, O], V](e :ColumnLValueSQL[F, M, V]) :Y[Single, V]
	}

	trait CaseAnyColumnLValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyColumnLValue[F, Y]
    {
	    override def convertedColumnLValue[M[O] <: BaseColumn[X, O], X, V]
	                                       (e :ConvertedColumnLValue[F, M, X, V]) :Y[Single, V] =
		    columnLValue(e)

	    override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
	                                (e :TypedColumnComponentSQL[O, T, R, M, V, L]) :Y[Single, V] =
		    columnLValue(e)

	    override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
	                            (e :LooseColumn[O, M, V]) :Y[Single, V] =
		    columnLValue(e)
    }
}




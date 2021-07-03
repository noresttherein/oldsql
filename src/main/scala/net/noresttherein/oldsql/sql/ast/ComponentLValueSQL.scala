package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, SQLForm}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{ColumnSetter, ColumnSQL, ComponentSetter, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, GlobalSQL, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.ColumnComponentConversion
import net.noresttherein.oldsql.sql.ast.ComponentLValueSQL.BaseComponentConversion
import net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentConversion
import net.noresttherein.oldsql.sql.ast.ConversionSQL.{ColumnConversionSQL, ColumnPromotionConversion, PromotionConversion}
import net.noresttherein.oldsql.sql.ast.LooseColumn.LooseColumnConversion
import net.noresttherein.oldsql.sql.ast.LooseComponent.LooseComponentConversion
import net.noresttherein.oldsql.sql.ast.BoundParam
import net.noresttherein.oldsql.sql.mechanics.{SQLNumber, TableCount}






/** An expression which can occur on the left side in the DSL for a ''set'' clause of an SQL ''update'',
  * representing a table component which value is set by an SQL ''insert'' or ''update'' statement.
  * It is a sealed trait with four distinct direct 'case' subtypes:
  *   1. [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], representing
  *      a direct use of a component, when the type of the ''r-value'' is the same as the subject type
  *      of the component;
  *   1. [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentConversion ComponentConversion]],
  *      which is a [[net.noresttherein.oldsql.sql.ast.ConversionSQL ConversionSQL]] subtype
  *      [[net.noresttherein.oldsql.sql.SQLExpression.Lift promoting]] the subject type of a converted
  *      `ComponentSQL` instance as a part of type unification with a ''r-value'' expression, originally
  *      of some different type;
  *   1. [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]], an unanchored
  *      (not linked to the owning table) component expression;
  *   1. [[net.noresttherein.oldsql.sql.ast.LooseComponent.LooseComponentConversion LooseComponentConversion]],
  *      which promotes the type of the latter in the same manner as former `ComponentConversion`.
  *
  * Additionally, there is a subtype for columns
  * [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL ColumnLValueSQL]],
  * with four direct subtypes mirroring the above.
  *
  * This type is used as part of [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]],
  * when creating DML ''update'' and ''insert'' statements as well as internally in preparation of an entity
  * before its saving. Note that, while representing a component, it is not
  * a [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]] subtype, as its value type may be different
  * than the mapping's `Subject`.
  * @tparam F a ''from'' clause - list of relations/tables which provide columns used in this expression.
  * @tparam M the mapping type of the updated component, accepting the `Origin` type as a type argument.
  *           It is the type parameter of the underlying `ComponentSQL`.
  * @tparam V the subject type of mapping `M` or a type to which it is promoted when unifying it
  *           with another expression.
  */ //just rename it to LValueSQL
trait ComponentLValueSQL[-F <: RowProduct, M[O] <: MappingAt[O], V] extends SQLExpression[F, GlobalScope, V] {
	type Origin >: F <: RowProduct

	/** The underlying component expression (possibly this instance). */
	@throws[UnsupportedOperationException]("if the lvalue is not anchored (it is a LooseComponent).")
	def component :ComponentSQL[F, M]
//		def component :ComponentSQL[F, M]
	/** The mapping of the set component. */
	def mapping :M[Origin]


	override def isGlobal = true
	override def asGlobal :Option[GlobalSQL[F, V]] = Some(this)

	override def anchor(from :F) :ComponentLValueSQL[F, M, V]

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ComponentLValueSQL[E, M, V] =
		expand[E]

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
			:ComponentLValueSQL[E, M, V] =
		expand[E]

	def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ComponentLValueSQL[E, M, V]


	/** Creates an assignment object [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]]
	  * with the given expression as its ''r-value'' and this instance as its ''l-value'', used to set the value
	  * of this component as a part of an SQL ''insert'' or ''update''.
	  */
	def :=[R <: RowProduct, Y, U](rvalue :SQLExpression[R, GlobalScope, Y])
	                             (implicit promote :SQLTypeUnification[V, Y, U]) :ComponentSetter[F, R, U] =
		ComponentSetter(to(promote.left), promote.right(denullify(rvalue)))

	def :=[C <: MappingOf[V], R <: RowProduct, O <: RowProduct] //R and O are separate as R may be instantiated early from the expected type
	      (component :C)(implicit cast :C <:< RefinedMapping[V, O], //todo: SQLTypeUnification
	                     subtype :SQLExpression[O, GlobalScope, V] <:< SQLExpression[R, GlobalScope, V],
	                     project :OriginProjection[C, V], offset :TableCount[O, _ <: Numeral])
			:ComponentSetter[F, R, V] =
		this := subtype(LooseComponent(component))

	def :=[X, U](that :X)(implicit promote :SQLTypeUnification[V, X, U], form :SQLForm[X])
			:ComponentSetter[F, RowProduct, U] =
		this := SQLTerm(that)

	//todo: we *could* get rid of the form param, as we have one from the mapping. But we don't know if insert or update
	def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[V, Y, U], form :SQLForm[Y])
			:ComponentSetter[F, RowProduct, U] =
		this := BoundParam(rvalue)

	override def to[Y](implicit lift :Lift[V, Y]) :ComponentLValueSQL[F, M, Y] =
		throw new UnsupportedOperationException("This method should have been overriden by the subclass. This is a bug.")


	protected def all_subclasses_of_ComponentLValueSQL_must_extend_ComponentSQL_or_LooseComponent :Nothing
}




object ComponentLValueSQL {
	def apply[F <: RowProduct, M[O] <: MappingAt[O], V]
	         (component :ComponentSQL[F, M], lift :Lift[M[Unit]#Subject, V]) :ComponentConversion[F, M, V] =
		new ComponentConversion(component, lift)

	def apply[F <: RowProduct, M[O] <: BaseMapping[S, O], S, V]
	         (component :LooseComponent[F, M, S], lift :Lift[S, V]) :LooseComponentConversion[F, M, S, V] =
		new LooseComponentConversion(component, lift)


	type * = ComponentLValueSQL[_ <: RowProduct, M, _] forSome { type M[O] <: MappingAt[O] }


	abstract class BaseComponentConversion[-F <: RowProduct, M[O] <: MappingAt[O], V]
	                                      (override val value :ComponentLValueSQL[F, M, M[Unit]#Subject],
	                                       val lift :Lift[M[Unit]#Subject, V])
		extends ConversionSQL[F, GlobalScope, M[Unit]#Subject, V] with ComponentLValueSQL[F, M, V]
	{
		override type Origin = value.Origin
		override def mapping :M[Origin] = value.mapping
		override def component :ComponentSQL[F, M] = value.component

		override def convert(x :M[Unit]#Subject) :V = lift(x)
		override def groundValue :Opt[V] = Lack

		override def anchor(from :F) :ComponentLValueSQL[F, M, V] = value.anchor(from) match {
			case same if same eq value => this
			case other => other.to(lift)
		}

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ComponentLValueSQL[E, M, V] =
			value.expand[E].to(lift)

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (e :SQLExpression[E, C, M[Unit]#Subject]) :SQLExpression[E, C, V] =
			e match {
				case c :LooseComponent[_, MappingOf[M[Unit]#Subject]#TypedProjection, M[Unit]#Subject] @unchecked =>
					new LooseComponentConversion(c, lift)
				case c :ComponentSQL[F, MappingOf[M[Unit]#Subject]#Projection @unchecked] =>
					new ComponentConversion[E, MappingOf[M[Unit]#Subject]#Projection, V](c, lift)
				case e =>
					PromotionConversion(e, lift)
			}

	}
}





/** An expression which can occur on the left side in the DSL for a ''set'' clause of an SQL ''update'',
  * representing an updated column of a table. It is a sealed trait with four distinct direct 'case' subtypes:
  *   1. [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL ColumnComponentSQL]], representing
  *      a direct use of a column, when the type of the ''r-value'' is the same as the subject type
  *      of the column;
  *   1. [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.ColumnComponentConversion ColumnComponentConversion]],
  *      which is a [[net.noresttherein.oldsql.sql.ast.ConversionSQL.ColumnConversionSQL ColumnConversionSQL]]
  *      subtype [[net.noresttherein.oldsql.sql.SQLExpression.Lift promoting]] the subject type of a converted
  *      `ColumnComponentSQL` instance as a part of type unification with a ''r-value'' expression, originally
  *      of some different type;
  *   1. [[net.noresttherein.oldsql.sql.ast.LooseColumn LooseColumn]], an unanchored (not linked
  *      to the owning table) table column expression;
  *   1. [[net.noresttherein.oldsql.sql.ast.LooseColumn.LooseColumnConversion LooseColumnConversion]],
  *      which promotes the type of the latter expression in the same manner as former `ColumnComponentConversion`.
  *
  * This type is used as part of [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]],
  * when creating DML ''update'' and ''insert'' statements as well as internally in preparation of an entity
  * before its saving.
  * @tparam F a ''from'' clause - list of relations/tables which provide columns used in this expression.
  * @tparam M the mapping type of the updated component, accepting the `Origin` type as a type argument.
  *           It is the type parameter of the underlying `ColumnComponentSQL`.
  * @tparam V the subject type of column `M` or a type to which it is promoted when unifying it
  *           with another expression.
  */
trait ColumnLValueSQL[-F <: RowProduct, M[O] <: ColumnMapping[_, O], V]
	extends ColumnSQL[F, GlobalScope, V] with ComponentLValueSQL[F, M, V]
{
	override def component :ColumnComponentSQL[F, M, _]
	def form :ColumnForm[V]
	override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)

	override def anchor(from :F) :ColumnLValueSQL[F, M, V]


	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :PartOf[U, E]) :ColumnLValueSQL[E, M, V] =
		expand[E]

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
			:ColumnLValueSQL[E, M, V] =
		expand[E]

	def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ColumnLValueSQL[E, M, V]

//		override def :=[R <: RowProduct, Y, U](rvalue :SQLExpression[R, GlobalScope, Y]) //overriden for correct overloading
//		                                      (implicit promote :SQLTypeUnification[V, Y, U]) :ComponentSetter[F, R, U] =
//			ComponentSetter[F, M, R, U](to(promote.left), promote.right(rvalue))

	/** Creates an assignment object [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]]
	  * with the given expression as its ''r-value'' and this instance as its ''l-value'', used to set the value
	  * of this component as a part of an SQL ''insert'' or ''update''.
	  */
	def :=[R <: RowProduct, Y, U](rvalue :ColumnSQL[R, GlobalScope, Y])
	                             (implicit promote :SQLTypeUnification[V, Y, U]) :ColumnSetter[F, R, U] =
		ColumnSetter[F, M, R, U](to(promote.left), promote.right(denullify(rvalue)))

	def :=[E <: F, O <: RowProduct] //E and O are separate as E may be instantiated early from the expected type
	      (component :ColumnMapping[V, O]) //todo: variants for columns and literals for other assignment methods
	      (implicit subtype :ColumnSQL[O, GlobalScope, V] <:< ColumnSQL[E, GlobalScope, V],
	                offset :TableCount[O, _ <: Numeral])
			:ColumnSetter[F, E, V] =
		this := subtype(LooseColumn(component))

	override def :=[X, U](that :X)(implicit promote :SQLTypeUnification[V, X, U], form :SQLForm[X])
			:ComponentSetter[F, RowProduct, U] =
		to(promote.left) := SQLTerm(promote.right(that))(promote.left(this.form))

	def +=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, GlobalScope, Y])
	      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R, U] =
		to(promote.left) := expand[R] + denullify(rvalue)

	def -=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, GlobalScope, Y])
	      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R, U] =
		to(promote.left) := expand[R] - denullify(rvalue)

	def *=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, GlobalScope, Y])
	      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R, U] =
		to(promote.left) := expand[R] * denullify(rvalue)

	def /=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, GlobalScope, Y])
	      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R, U] =
		to(promote.left) := expand[R] / denullify(rvalue)

	def %=[R <: RowProduct, Y, U]
	      (rvalue :ColumnSQL[R, GlobalScope, Y])
	      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
			:ColumnSetter[F, R, U] =
		to(promote.left) := expand[R] % denullify(rvalue)


	def &&=[R <: RowProduct, Y](rvalue :ColumnSQL[R, GlobalScope, Y])
	                           (implicit promote :SQLTypeUnification[V, Y, Boolean], expansion :F ExpandedBy R)
			:ColumnSetter[F, R, Boolean] =
		to(promote.left) := expand[R].to(promote.left) && promote.right(rvalue)

	def ||=[R <: RowProduct, Y](rvalue :ColumnSQL[R, GlobalScope, Y])
	                           (implicit promote :SQLTypeUnification[V, Y, Boolean], expansion :F ExpandedBy R)
			:ColumnSetter[F, R, Boolean] =
		to(promote.left) := expand[R].to(promote.left) || promote.right(rvalue)

//		def ^= [R <: RowProduct, Y](rvalue :ColumnSQL[R, GlobalScope, Y])
//		                  (implicit promote :SQLTypeUnification[V, Y, Boolean]) :ColumnSetter[F, R, Boolean] =
//			to(promote.left) := to(promote.left) ^ promote.right(rvalue)


	def ++=[R <: RowProduct, Y](rvalue :ColumnSQL[R, GlobalScope, Y])
	                           (implicit promote :SQLTypeUnification[V, Y, String], expansion :F ExpandedBy R)
			:ColumnSetter[F, R, String] =
		to(promote.left) := expand[R].to(promote.left) ++ promote.right(denullify(rvalue))

//		override def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[V, Y, U], form :SQLForm[Y])
//				:ComponentSetter[F, RowProduct, U] =  //overriden for correct overloading
//			this := BoundParam(rvalue)

	def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[V, Y, U], form :ColumnForm[Y])
			:ColumnSetter[F, RowProduct, U] =
		this := BoundParam(rvalue)


	override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
		throw new UnsupportedOperationException("This method should have been overriden by the subclass. This is a bug.")

}


object ColumnLValueSQL {
	def apply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
	         (column :ColumnComponentSQL[F, M, S], lift :Lift[S, V]) :ColumnComponentConversion[F, M, S, V] =
		new ColumnComponentConversion(column, lift)

	def apply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
	         (column :LooseColumn[F, M, S], lift :Lift[S, V]) :LooseColumnConversion[F, M, S, V] =
		new LooseColumnConversion(column, lift)


	type * = ColumnLValueSQL[_ <: RowProduct, M, _] forSome { type M[O] <: ColumnMapping[_, O] }


	trait BaseColumnComponentConversion[-F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
		extends BaseComponentConversion[F, M, V] with ColumnConversionSQL[F, GlobalScope, S, V]
		   with ColumnLValueSQL[F, M, V]
	{
		override val value :ColumnLValueSQL[F, M, S] = null
		override def component :ColumnComponentSQL[F, M, _] = value.component
		override def form :ColumnForm[V] = lift(value.form)

		override def anchor(from :F) :ColumnLValueSQL[F, M, V] = value.anchor(from) match {
			case same if same eq value => this
			case other => other.to(lift)
		}

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ColumnLValueSQL[E, M, V] =
			value.expand[E].to(lift)

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope](e :ColumnSQL[E, C, S])
				:ColumnSQL[E, C, V] =
			e match {
				case c :LooseColumn[E, M, S] @unchecked =>
					new LooseColumnConversion[E, M, S, V](c, lift)
				case c :ColumnComponentSQL[F, M, S]  @unchecked =>
					new ColumnComponentConversion[F, M, S, V](c, lift)
				case e =>
					ColumnPromotionConversion(e, lift)
			}
	}
}




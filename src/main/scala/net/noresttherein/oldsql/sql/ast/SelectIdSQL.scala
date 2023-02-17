package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, MismatchedExpressionsException, MisalignedExpressionException}
import net.noresttherein.oldsql.pixies.RearrangedIndexing
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.{ColumnForm, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.ColumnAt
import net.noresttherein.oldsql.schema.bases.BaseColumn
import net.noresttherein.oldsql.slang.classNameMethods
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SingleSQL, SQLExpression}
import net.noresttherein.oldsql.sql.Query.QueryTemplate
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingTemplate, GroundingTemplate, Grouped, Single, SpecificExpressionVisitor, SQLShape, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.{GenericSQLDecoratorTemplate, UnaryCompositeSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate
import net.noresttherein.oldsql.sql.ast.SelectAsIdSQL.{AnySelectAsIdVisitor, SpecificSelectAsIdVisitor}
import net.noresttherein.oldsql.sql.mechanics.{Reform, SpelledSQL, SQLConversion, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate





/** A synthetic expression adding an extra column to a ''select'' clause expression with an identifier
  * of the select. Used within ''compound selects'', it allows the top level row form to identify
  * which member ''select'' does any row come from. Used solely during
  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.reformed reforming]] ''select'' clauses of members
  * of a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]/[[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]].
  */
sealed class SelectIdSQL[-F <: RowProduct, -S >: Grouped <: Single, V] protected
                        (val id :String, val columnName :String,
                         val idColumn :ColumnSQL[RowProduct, Single, String],
                         val selectClause :SQLExpression[F, S, V])
	extends UnaryCompositeSQL[F, S, V, V] with GenericSQLDecoratorTemplate[F, S, V, SelectIdSQL]
	   with SelectableSQL[F, S, V] with NonColumnSQL[F, S, V]
//	   with ConvertingTemplate[F, S, V, ({ type E[v] = SelectIdSQL[F, S, v] })#E]
//	   with GroundingTemplate[F, S, V ({ type E[f <: RowProduct] = SelectIdSQL[f, S, V] })#E]
{ self =>
	def this(id :String, columnName :String, selectClause :SQLExpression[F, S, V]) =
		this(id, columnName, ColumnLiteral(id) as columnName, selectClause)

	protected override val value :SQLExpression[F, S, V] = selectClause
//	protected override lazy val parts :Seq[SQLExpression[F, S, _]] = PassedArray.two(idColumn, selectClause)

	/** A form which reads the `selectClause` value starting from the second column (that is, `position+1`
	  * where `position` is the argument to `opt`/`apply`). The first id column is completely ignored.
	  */
	override def selectForm  :SQLReadForm[V] = {
		val form = selectClause.selectForm
		form.reorder(RearrangedIndexing.shift(1, form.columnCount))
	}

	protected override def decorate[E <: RowProduct, C >: Grouped <: Single, Y]
	                               (e :SQLExpression[E, C, Y]) :SelectIdSQL[E, C, Y] =
		e match {
			case lvalue :LValueSQL.from[E]#of[Y]  =>
				new SelectAsIdSQL(id, columnName, idColumn, lvalue)
			case _ =>
				new SelectIdSQL(id, columnName, idColumn, e)
		}

	//we can't really reform(selectClause, other) because we have no idea what kind of id to assign to reformed other
	protected override def reformer[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                   (implicit leftResult  :SQLTransformation[V, U],
                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.SQLResult[F1, S1, SelectIdSQL[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
	{
		if (!leftResult.isInstanceOf[SQLConversion[_, _]])
			throw new IllegalArgumentException(
				"A non-conversion " + leftResult + " passed for transforming the left result of reforming SelectIdSQL "
					+ self + " with " + other + "."
			)
		if (!rightResult.isInstanceOf[SQLConversion[_, _]])
			throw new IllegalArgumentException(
				"A non-conversion " + rightResult + " passed for transforming the right result of reforming SelectIdSQL "
					+ self + " with " + other + "."
			)
		new BaseReformer[F1, S1, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](
			other)(reform, passCount)(leftResult, rightResult, spelling
		) {
			override def selectId(e :SelectIdSQL[F2, S2, V2])  =
				reform(selectClause, e.selectClause)(
					//these casts may be possible to remove with a dedicated transformation implementation
					transformation andThen leftResult, e.transformation andThen rightResult, spelling
				).asInstanceOf[Result]
		}
	}

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
		idColumn +: spelling.split(selectClause)

	protected override def shape(implicit spelling :SQLSpelling) :RowShape =
		ColumnForm[String].sqlType +: spelling.shape(selectClause)

	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		spelling.columnCount(selectClause) + 1

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		spelling.explode(idColumn)(from, context, params) ++: spelling.explode(selectClause)(from, context, params)


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] = visitor.selectId(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y = visitor.selectId(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F, S, V] <: SQLExpression[F_, S_, V],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, V, E] =
//		visitor.selectId(this)

	override def equals(that :Any) :Boolean = that match {
		case any :AnyRef if this eq any => true
		case other :SelectIdSQL[_, _, _] if (other canEqual this) && (this canEqual other) =>
			id == other.id && columnName == other.columnName && selectClause == other.selectClause
		case _ => false
	}
//	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelectIdSQL[_, _, _]]
	override def hashCode :Int = (id.hashCode * 31 + columnName.hashCode) * 31 + selectClause.hashCode

	override def toString :String = "{" + idColumn + ", " + selectClause + "}"
}



object SelectIdSQL {
	def apply[F <: RowProduct, S >: Grouped <: Single, V]
	         (id :String, columnName :String, selectClause :SQLExpression[F, S, V]) :SelectIdSQL[F, S, V] =
		selectClause match {
			case comp :ComponentSQL[F, MappingOf[V]#Projection] @unchecked =>
				SelectAsIdSQL(id, columnName, comp)
			case _ =>
				new SelectIdSQL(id, columnName, selectClause)
		}

	def unapply[F <: RowProduct, S >: Grouped <: Single, V](e :SQLExpression[F, S, V])
			:Opt[(String, String, SQLExpression[F, S, V])] =
		e match {
			case select :SelectIdSQL[F, S, V] => Got(select.id, select.columnName, select.selectClause)
			case _ => Lack
		}

	trait SpecificSelectIdVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificSelectAsIdVisitor[F, X, Y]
	{
		def selectId(e :SelectIdSQL[F, S, X]) :Y
	}
	type MatchSpecificSelectId[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificSelectIdVisitor[F, S, X, Y]
	trait CaseSpecificSelectId[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends MatchSpecificSelectId[F, S, X, Y] {
		override def selectAsId[M[O] <: MappingAt[O]](e :SelectAsIdSQL[F, M, X]) :Y = selectId(e)
	}
//
//	trait SelectIdVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SelectMappingIdVisitor[F, R]
//	{
//		def selectId[S >: Grouped <: Single, V](e :SelectIdSQL[F, S, V]) :R[S, V, SelectIdSQL[F, S, V]]
//	}
//	type MatchSelectId[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		SelectIdVisitor[F, R]
//	trait CaseSelectId[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchSelectId[F, R]
//	{
//		override def selectMappingId[M[O] <: MappingAt[O], V]
//		                            (e :SelectMappingIdSQL[F, M, V]) :R[Single, V, SelectMappingIdSQL[F, M, V]] =
//			selectId(e)
//	}
//	type CaseSelectId[+F <: RowProduct, +R[-S >: Grouped <: GlobalScope, V, -e <: SQLExpression[F, S, V]]] =
//		SelectIdVisitor[F, R]

	trait AnySelectIdVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySelectAsIdVisitor[F, Y]
	{
		def selectId[S >: Grouped <: Single, V](e :SelectIdSQL[F, S, V]) :Y[S, V]
	}
	type MatchAnySelectId[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnySelectIdVisitor[F, Y]
	trait CaseAnySelectId[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnySelectId[F, Y] {
		override def selectAsId[M[O] <: MappingAt[O], V](e :SelectAsIdSQL[F, M, V]) :Y[Single, V] = selectId(e)
	}
} //consider: SelectListingIdSQL






//currently unused
//todo: make it accept any MappingSQL
final class SelectAsIdSQL[-F <: RowProduct, M[A] <: MappingAt[A], V] private[ast]
                         (override val id :String, override val columnName :String,
                          override val idColumn :ColumnSQL[RowProduct, Single, String],
                          override val selectClause :LValueSQL[F, M, V])
	extends SelectIdSQL[F, Single, V](id, columnName, idColumn, selectClause)
	   with MappingSQL[F, Single, M, V]
//	   with ConvertingTemplate[F, Single, V, ({ type E[v] = SelectAsIdSQL[F, M, v] })#E]
//	   with GroundingTemplate[F, Single, V, ({ type E[f <: RowProduct] = SelectAsIdSQL[f, M, V] })#E, SelectAsIdSQL[F, M, V]]
	   with GenericSQLDecoratorTemplate[F, Single, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, v] =
	                                                            SelectAsIdSQL[f, M, v] })#E]
	   with MappingSQLTemplate[F, Single, M, V, SelectAsIdSQL[F, M, V]]
{ self =>
	def this(id :String, columnName :String, selectClause :LValueSQL[F, M, V]) =
		this(id, columnName, ColumnLiteral(id) as columnName, selectClause)

	override type Origin    = selectClause.Origin
	override val mapping    :M[Origin] = selectClause.mapping
	override val export     :TypedMapping[M[Unit]#Subject, Origin] = selectClause.export
	override val anchored   :TypedMapping[M[Unit]#Subject, Origin] = selectClause.anchored
	override def conversion :SQLConversion[M[Unit]#Subject, V] = selectClause.conversion

	override def \[K <: MappingAt[selectClause.Origin], X](component :K)(implicit project :OriginProjection[K, X])
			:MappingSQL[F, Single, project.WithOrigin, X] =
		selectClause \ component

	override def \[K <: ColumnAt[selectClause.Origin], X]
	              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
			:ColumnMappingSQL[F, Single, project.WithOrigin, X] =
		selectClause \ column


	override def isDefault :Boolean = selectClause.isDefault
	override def default :SelectAsIdSQL[F, M, V] =
		if (isDefault) this else new SelectAsIdSQL(id, columnName, idColumn, selectClause.default)

	override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
			:SelectAsIdSQL[F, M, V] =
		new SelectAsIdSQL(id, columnName, idColumn, selectClause.defaultWith(includes, excludes))

	override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
			:SelectAsIdSQL[F, M, V] =
		new SelectAsIdSQL(id, columnName, idColumn, selectClause.alter(includes, excludes))
//
//	//to[Y] returns whatever the SQLTransformation wants and we can't guarantee to[Y] :SelectMappingIdSQL :(
//	override def convert[Y](conversion :SQLConversion[V, Y]) :SelectAsIdSQL[F, M, Y] =
//		if (conversion.isIdentity) this.asInstanceOf[SelectAsIdSQL[F, M, Y]]
//		else new SelectAsIdSQL(id, columnName, idColumn, conversion(selectClause))
//
//	override def anchor(from :F) :SelectAsIdSQL[F, M, V] = selectClause.anchor(from) match {
//		case same if same eq selectClause => this
//		case other => new SelectAsIdSQL(id, columnName, idColumn, other)
//	}
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :SelectAsIdSQL[E, M, V] =
//		new SelectAsIdSQL(id, columnName, idColumn, selectClause.basedOn(base))
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
//			:SelectAsIdSQL[E, M, V] =
//		new SelectAsIdSQL(id, columnName, idColumn, selectClause.expand(base))

	protected override def decorate[C <: RowProduct, A >: Grouped <: Single, X]
		                           (e :SQLExpression[C, A, X]) :SelectAsIdSQL[C, M, X] =
		e match {
			case this.value =>
				this.asInstanceOf[SelectAsIdSQL[C, M, X]]
			case lvalue :LValueSQL[C, M @unchecked, X] if lvalue.mapping == mapping =>
				new SelectAsIdSQL(id, columnName, idColumn, lvalue)
			case _ =>
				throw new IllegalArgumentException(
					"Cannot decorate " + e + " as a SelectAsIdSQL because it is not a LValueSQL for mapping " + mapping
						+ ". This is most likely a bug."
				)
		}

	protected override def reformer[F1 <: F, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                   (implicit leftResult  :SQLTransformation[V, U],
                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.SQLResult[F1, S1, SelectAsIdSQL[F1, M, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
	{
		if (!leftResult.isInstanceOf[SQLConversion[_, _]])
			throw new IllegalArgumentException(
				"A non-conversion " + leftResult + " passed for transforming the left result of reforming SelectAsIdSQL "
					+ self + " with " + other + "."
			)
		if (!rightResult.isInstanceOf[SQLConversion[_, _]])
			throw new IllegalArgumentException(
				"A non-conversion " + rightResult + " passed for transforming the right result of reforming SelectAsIdSQL "
					+ self + " with " + other + "."
			)
		new BaseReformer[F1, Single, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](
			other)(reform, passCount)(leftResult, rightResult, spelling
		) {
			override def selectId(e :SelectIdSQL[F2, S2, V2])  =
				reform(selectClause, e.selectClause)(
					//these casts may be possible to remove with a dedicated transformation implementation
					transformation andThen leftResult, e.transformation andThen rightResult, spelling
				).asInstanceOf[Result]
		}
	}


	//fixme: we must return the id column as ColumnMappingSQL - how???
	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[F]#rows[Single]#__] =
		spelling.split(selectClause)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, V] = visitor.selectAsId(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, V, Y]) :Y =
		visitor.selectAsId(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//	                             E >: SQLExpression[F, Single, V] <: SQLExpression[F_, S_, V],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, V, E] =
//		visitor.selectAsId(this)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelectAsIdSQL.__]
}



object SelectAsIdSQL {
	def apply[F <: RowProduct, M[O] <: MappingAt[O], V]
	         (id :String, columnName :String, selectClause :LValueSQL[F, M, V]) :SelectAsIdSQL[F, M, V] =
		new SelectAsIdSQL(id, columnName, selectClause)

	def unapply[F <: RowProduct, V](e :SQLExpression[F, _, V])
			:Opt[(String, String, LValueSQL[F, M, V] forSome { type M[O] <: MappingAt[O] })] =
		e match {
			case select :SelectAsIdSQL.Cast[F, V] =>
				Got(select.id, select.columnName, select.selectClause)
			case _ => Lack
		}

	type __ = SelectAsIdSQL[_, M, _] forSome { type M[O] <: MappingAt[O] }
	type AnyIn[-F <: RowProduct] = SelectAsIdSQL[F, M, _] forSome { type M[O] <: MappingAt[O] }
	type Cast[-F <: RowProduct, V] = SelectAsIdSQL[F, M, V] forSome { type M[O] <: MappingAt[O] }

	trait SpecificSelectAsIdVisitor[+F <: RowProduct, X, +Y] {
		def selectAsId[M[O] <: MappingAt[O]](e :SelectAsIdSQL[F, M, X]) :Y
	}
	type MatchSpecificSelectAsId[+F <: RowProduct, X, +Y] = SpecificSelectAsIdVisitor[F, X, Y]
	type CaseSpecificSelectAsId[+F <: RowProduct, X, +Y]  = SpecificSelectAsIdVisitor[F, X, Y]
//
//	trait SelectMappingIdVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def selectMappingId[M[O] <: MappingAt[O], V]
//		                   (e :SelectMappingIdSQL[F, M, V]) :R[Single, V, SelectMappingIdSQL[F, M, V]]
//	}
//	type MatchSelectMappingId[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		SelectMappingIdVisitor[F, R]
//	type CaseSelectMappingId[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		SelectMappingIdVisitor[F, R]

	trait AnySelectAsIdVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def selectAsId[M[O] <: MappingAt[O], V](e :SelectAsIdSQL[F, M, V]) :Y[Single, V]
	}
	type MatchAnySelectAsId[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnySelectAsIdVisitor[F, Y]
	type CaseAnySelectAsId[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]  = AnySelectAsIdVisitor[F, Y]
}






/** A pseudo expression serving as an [[net.noresttherein.oldsql.sql.SQLExpression.SQLShape SQLShape]]
  * of a ''compound select'' whose all member ''selects'' use [[net.noresttherein.oldsql.sql.ast.SelectIdSQL SelectIdSQL]]
  * as their ''select'' clauses, i.e. they prefix their actual ''select'' clause expressions with an addition literal
  * column with an id of the ''select'', allowing to match every row of this expression's query to the ''select''
  * it originated from, and thus use the proper [[net.noresttherein.oldsql.sql.SQLExpression.selectForm selectForm]].
  * The main purpose of this class is to define `this.selectForm` as a 'switch/decode' form,
  * reading the value of the discriminator column and using the form of the ''select'' with that id.
  *
  * Most transforming operations, in particular grounding/anchoring throw an [[UnsupportedOperationException]].
  * For this reason, this expression should only be used in final
  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] and
  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]] instances not exposed to the application,
  * just before their translation to an SQL statement.
  */ //what about reforming this shit?
//fixme: must also be reformable at least with other SelectIdSQL;
sealed trait CompoundSelectIdSQL[V]
	extends UnaryCompositeSQL[Nothing, Grouped, V, V]
	   with ConvertingTemplate[Nothing, Grouped, V, CompoundSelectIdSQL]
{ self =>
//	   with GenericSQLDecoratorTemplate[Nothing, Grouped, V, ({ type E})] {
//	def this(columnName :String, selectClause :SQLExpression[Nothing, Grouped, V]) =
//		this(columnName, SQLNull[String], selectClause)
	override val value :SelectIdSQL[Nothing, Grouped, V]
	def columnName     :String = value.columnName
//	def idColumn       :ColumnSQL[Nothing, Grouped, String]
	val selectClauses  :Seq[SelectIdSQL[Nothing, Grouped, V]]

	override lazy val selectForm :SQLReadForm[V] =
		SQLReadForm[String].when(
			selectClauses.map { selectClause => (selectClause.id, selectClause.selectClause.selectForm) } :_*		
		)

//	protected override def transform[X](conversion :SQLTransformation[V, X]) :SQLExpression[Nothing, Grouped, X] =
//		if (!conversion.isDerived || conversion.isIdentity) conversion[Nothing, Grouped](this) //this is most likely an error in the making
//		else CompoundSelectIdSQL(conversion(value), selectClauses.map(conversion.apply))

	protected override def convert[X](conversion :SQLConversion[V, X]) :CompoundSelectIdSQL[X] =
		if (conversion.isIdentity) conversion(toConvertibleSQL)
		else CompoundSelectIdSQL(conversion(value), selectClauses.map(conversion(_)))

	override def asSingleRow :Option[SingleSQL[Nothing, V]] = None
	override def groundValue :Opt[V] = Lack
	override def isGround :Boolean = false
	override def isAnchored :Boolean = true
	override def isAnchored(from :Nothing) :Boolean = true
	override def anchor(from :Nothing) :SQLExpression[Nothing, Grouped, V] = this

//	override def basedOn[U <: Nothing, E <: RowProduct]
//	                    (base :E)(implicit ext :U PartOf E) :SQLExpression[E, Grouped, V] =
//		throw new UnsupportedOperationException("CompoundSelectIdSQL " + this + " cannot be rebased to another clause.")
//
//	override def expand[U <: Nothing, E <: RowProduct]
//	                   (base :E)(implicit ext :U ExpandedBy E, global :GlobalScope <:< Grouped)
//			:SQLExpression[E, Grouped, V] =
//		throw new UnsupportedOperationException("CompoundSelectIdSQL " + this + " cannot be rebased to another clause.")

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (e :SQLExpression[E, C, V]) :SQLExpression[E, C, V] =
		if (e eq value)
			this.asInstanceOf[SQLExpression[E, C, V]]
		else
			throw new UnsupportedOperationException(
				"Cannot recreate a CompoundSelectIdSQL " + this + " for an arbitrary single select clause " + e + "."
			)

	protected override def reform[F1 <: Nothing, S1 >: Grouped <: Grouped, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                 (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
                                           spelling :SQLSpelling)
			:(leftResult.SQLResult[F1, S1, CompoundSelectIdSQL[U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
		if (passCount.firstTime)
			super.reform(other)(reform, passCount)
		else if (!leftResult.isInstanceOf[SQLConversion[_, _]])
			throw new IllegalArgumentException(
				"A non-conversion " + leftResult + " passed for transforming a CompoundSelectIdSQL " + self +
					" after reforming it with " + other + "."
			)
		else if (!rightResult.isInstanceOf[SQLConversion[_, _]])
			throw new IllegalArgumentException(
				"A non-conversion " + rightResult + " passed for transforming expression " + other +
					" after reforming with a CompoundSelectIdSQL " + self + "."
			)
		else if (reform.mayAlterRight) {
			val reformRight = reform.prohibitReformLeft
			val reformed = reformRight(value, other)._2
			if (!selectClauses.forall(reform.compatible(_, other)))
				throw new MisalignedExpressionException(this)
			(leftResult.convert(self), reformed)
		} else {
			val reformLeft = reform.prohibitReformRight
			val (shape, reformed) = reform(value, other)
			val reformedClauses = selectClauses.map { selectClause =>
				reformLeft(selectClause, reformed) match {
					case (selectId :SelectIdSQL[Nothing, Grouped, U], _) => selectId
					case (l, _) =>
						throw new IllegalExpressionException(l,
							"Failed to reform " + this + " with " + other +": a SelectIdSQL expression " +
								selectClause + " was reformed to a different type: " + l + " :" + l.className + "."
						)
				}
			}
			val left = CompoundSelectIdSQL(shape, reformedClauses)
			//cast is safe because we checked that leftResult is an SQLConversion, so its SQLResult is CompoundSelectIdSQL[U]
			(left.asInstanceOf[leftResult.SQLResult[F1, S1, CompoundSelectIdSQL[U]]], reformed)
		}

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[Nothing, Grouped, _]] =
		throw new UnsupportedOperationException("CompoundSelectIdSQL " + this + " cannot be split.")

	protected override def shape(implicit spelling :SQLSpelling) :RowShape = {
		val inSelect = spelling.inSelect
		try {
			selectClauses.view.map(inSelect.shape).reduce(_ | _)
		} catch {
			case e :IllegalArgumentException =>
				throw new IllegalStateException(
					selectClauses.view.map(inSelect.shape).mkString(
						"Incompatible shapes of member selects in " + selectClauses + ": ", ", ", "."
					), e
				)
		}
	}

	protected override def columnCount(implicit spelling :SQLSpelling) :Int = {
		val inSelect = spelling.inSelect
		val counts = selectClauses.iterator.map(inSelect.columnCount)
		val res = counts.next()
		if (counts.exists(_ != res))
			throw new IllegalStateException(
				selectClauses.mkString(
					"Select clauses with differing column counts used in a compound select:\n", ";\n", "."
				)
			)
		res
	}

	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		(0 /: selectClauses)(_ + spelling.sqlParamCount(_))

	protected override def defaultSpelling[P](from :Nothing, context :SQLContext[P], params :Parameterization[P, Nothing])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		throw new UnsupportedOperationException("CompoundSelectIdSQL " + this + " cannot be spelled. This is a bug.")

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :Nothing, context :SQLContext[P], params :Parameterization[P, Nothing])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		throw new UnsupportedOperationException("CompoundSelectIdSQL " + this + " cannot be spelled. This is a bug.")

	//if we add a visitor it probably shouldn't be extended by CompositeVisitor
	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[Nothing, Y]) :Y[Grouped, V] =
		throw new UnsupportedOperationException("CompoundSelectIdSQL(" + this + ").visit(" + visitor + ").")

	protected override def visit[Y](visitor :SpecificExpressionVisitor[Nothing, Grouped, V, Y]) :Y =
		throw new UnsupportedOperationException("CompoundSelectIdSQL(" + this + ").visit(" + visitor + ").")

	override def isomorphic(that: SQLExpression.__) :Boolean = ???

	override def toString :String = selectClauses.mkString("CompoundSelectId(", ", ", ")")
}



object CompoundSelectIdSQL {

	def apply[V](shape :SelectIdSQL[Nothing, Grouped, V], selectClauses :Seq[SelectIdSQL[Nothing, Grouped, V]])
			:CompoundSelectIdSQL[V] =
	{
		val main = shape
		val seq = selectClauses
		if (selectClauses.isEmpty)
			throw new IllegalArgumentException("Cannot create a CompoundSelectIdSQL for zero select clauses.")
		new CompoundSelectIdSQL[V] with RowShapeCache {
			override val value = main
			override val selectClauses = seq
		}
	}

	def apply[V](selectClauses: => Seq[SelectIdSQL[Nothing, Grouped, V]]) :CompoundSelectIdSQL[V] = {
		class LazyCompoundSelectIdSQL(clauses: => Seq[SelectIdSQL[Nothing, Grouped, V]])
			extends CompoundSelectIdSQL[V] with RowShapeCache
		{
			override lazy val value = selectClauses.head
			override lazy val selectClauses = clauses
		}
		new LazyCompoundSelectIdSQL(selectClauses)
	}

	def apply[V, Q[X] <: QueryTemplate[X, Q]]
	         (query :CompoundSelectTemplate[V, Q],
	          selectClauses :CompoundSelectTemplate[V, Q] => Seq[SelectIdSQL[Nothing, Grouped, V]])
			:CompoundSelectIdSQL[V] =
		apply(selectClauses(query))

	def apply[V, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[V, Q]) :CompoundSelectIdSQL[V] =
		CompoundSelectIdSQL[V, Q](query, selectClauses(_ :CompoundSelectTemplate[V, Q]))

	private[sql] def apply[V, Q[X] <: QueryTemplate[X, Q]](left :Q[V], right :Q[V]) :CompoundSelectIdSQL[V] =
		CompoundSelectIdSQL[V](try {
			(left.constituents.view ++ right.constituents).map(selectClause) to PassedArray
		} catch {
			case e :IllegalStateException =>
				throw new IllegalStateException(
					"Cannot create a form for queries `" + left + "` and `" + right + "`: " + e.getMessage, e
				)
		})

//	private[sql] def selectId[V](selectClause :SQLExpression[Nothing, Grouped, V]) :SelectIdSQL[Nothing, Grouped, V] =
//		selectClause match {
//			case id :SelectIdSQL[Nothing, Grouped, V] => id
//			case _ =>
//				throw new IllegalStateException("Select clause expression " + selectClause + " is not a SelectIdSQL")
//		}


	def unapply[V](e :SQLExpression[_, _, V]) :Opt[Seq[SelectIdSQL[Nothing, Grouped, V]]] =
		e match {
			case id :CompoundSelectIdSQL[V] => Got(id.selectClauses)
			case _ => Lack
		}


	private def selectClause[V, Q[X] <: QueryTemplate[X, Q]](select :Q[V]) :SelectIdSQL[Nothing, Grouped, V] =
		select.selectClause match {
			case id :SelectIdSQL[Nothing, Grouped, V] => id
			case other =>
				throw new IllegalStateException(
					"the select clause of `" + select + "` is not a SelectIdSQL: " + other.localClassName + "."
				)
		}

	private def selectClauses[V, Q[X] <: QueryTemplate[X, Q]]
	                         (query :CompoundSelectTemplate[V, Q]) :Seq[SelectIdSQL[Nothing, Grouped, V]] =
		try { query.constituents.map(selectClause(_)) } catch {
			case e :IllegalStateException =>
				throw new IllegalStateException(
					"Cannot create a form for compound select `" + query + "`: " + e.getMessage, e
				)
		}
}

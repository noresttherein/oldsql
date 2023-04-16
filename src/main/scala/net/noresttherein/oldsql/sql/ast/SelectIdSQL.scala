package net.noresttherein.oldsql.sql.ast

import java.sql.{JDBCType, PreparedStatement, ResultSet, SQLType}

import net.noresttherein.oldsql.collection.{Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, MisalignedExpressionException, MismatchedExpressionsException}
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.bases.BaseColumn
import net.noresttherein.oldsql.schema.SQLReadForm.AbstractSQLReadForm
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals
import net.noresttherein.oldsql.slang.{castTypeParam, classNameMethods, saferCasting}
import net.noresttherein.oldsql.sql.{ColumnSQL, GroundColumn, RowProduct, RowShape, SQLExpression, SingleSQL}
import net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate
import net.noresttherein.oldsql.sql.Query.QueryTemplate
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingOps, ConvertingTemplate, Grouped, ReorderingTemplate, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL
import net.noresttherein.oldsql.sql.ast.MappingSQL.{MappingSQLTemplate, TransformedMappingDefaults}
import net.noresttherein.oldsql.sql.ast.SelectAsIdSQL.{AnySelectAsIdVisitor, SelectAsIdTransformation, SpecificSelectAsIdVisitor}
import net.noresttherein.oldsql.sql.ast.SelectIdSQL.SelectIdTransformation
import net.noresttherein.oldsql.sql.ast.TransformedSQL.{ReshapedSQLTemplate, WrappedSQLTemplate}
import net.noresttherein.oldsql.sql.mechanics.{AlignableColumn, AlignableColumns, Reform, SQLConversion, SQLFormConversion, SQLTransformation, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLTransformation.{GenericTransformation, IndependentTransformation}
import net.noresttherein.oldsql.sql.mechanics.SQLValueConversion.SQLValueIdentity





/** A synthetic expression adding an extra column to a ''select'' clause expression with an identifier
  * of the select. Used within ''compound selects'', it allows the top level row form to identify
  * which member ''select'' does any row come from. Used solely during
  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.reformed reforming]] ''select'' clauses of members
  * of a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]/[[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]].
  */
sealed class SelectIdSQL[-F <: RowProduct, -S >: Grouped <: Single, V] protected
                        (val id :String, val columnName :String,
                         val idColumn :ColumnSQL[RowProduct, Single, String],
                         override val value :SQLExpression[F, S, V])
	extends TransformedSQL[F, S, V, V]
	   with NonColumnSQL[F, S, V]
	   with ConvertingTemplate[F, S, V, ({ type E[v] = SelectIdSQL[F, S, v] })#E]
	   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = SelectIdSQL[f, S, V] })#E]
	   with ReorderingTemplate[F, S, V, SelectIdSQL[F, S, V]]
	   with ReshapedSQLTemplate[F, S, V, V, SelectIdSQL]
	   with WrappedSQLTemplate[F, S, V, SelectIdSQL]
	   with SelectableSQL[F, S, V]
//	   with ConvertingTemplate[F, S, V, ({ type E[v] = SelectIdSQL[F, S, v] })#E]
//	   with GroundingTemplate[F, S, V ({ type E[f <: RowProduct] = SelectIdSQL[f, S, V] })#E]
{ self =>
	def this(id :String, columnName :String, selectClause :SQLExpression[F, S, V]) =
		this(id, columnName, ColumnLiteral(id) as columnName, selectClause)

	val selectClause :value.type = value
//	override val value :SQLExpression[F, S, V] = selectClause
	override def transformation :IndependentTransformation.Convertible[V, V, SelectIdSQL] =
		new SelectIdTransformation[V](id, columnName, idColumn)

//	protected override lazy val parts :Seq[SQLExpression[F, S, _]] = PassedArray.two(idColumn, selectClause)
//	override def isUniversal = true

//	protected override def decorate[E <: RowProduct, C >: Grouped <: Single, Y]
//	                               (e :SQLExpression[E, C, Y]) :SelectIdSQL[E, C, Y] =
//		e match {
//			case lvalue :LValueSQL.from[E]#of[Y]  =>
//				new SelectAsIdSQL(id, columnName, idColumn, lvalue)
//			case _ =>
//				new SelectIdSQL(id, columnName, idColumn, e)
//		}

	protected override def decorate[E <: RowProduct, C >: Grouped <: Single, X]
	                               (e :SQLExpression[E, C, X]) :SelectIdSQL[E, C, X] =
		e match {
			case same if same eq selectClause => this.castFrom[SelectIdSQL[F, S, V], SelectIdSQL[E, C, X]]
			case comp :LValueSQL[F, MappingAt, V] @unchecked =>
				SelectAsIdSQL(id, columnName, comp)
			case _ =>
				new SelectIdSQL(id, columnName, e)
		}

	//we can't really reform(selectClause, other) because we have no idea what kind of id to assign to reformed other
	protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                   (implicit leftResult  :SQLTransformation[V, U],
                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.Expression[F, S, SelectIdSQL[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])] =
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
		new BaseReformer[F2, S2, V2, EC2, U, leftResult.Expression, rightResult.Expression](
			other)(reform, passCount)(leftResult, rightResult, spelling
		) {
			override def selectId(e :SelectIdSQL[F2, S2, V2])  =
				reform(selectClause, e.selectClause)(
					//these casts may be possible to remove with a dedicated transformation implementation
					transformation andThen leftResult, e.transformation andThen rightResult, spelling
				).castFrom[
					(leftResult.Expression[F, S, SelectIdSQL[F, S, U]], rightResult.Expression[F2, S2, SQLExpression[F2, S2, U]]),
					(leftResult.Expression[F, S, SelectIdSQL[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])
				]//.asInstanceOf[Result]
		}
	}

	protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :SelectIdSQL[F, S, V] =
		if (reordering.isIdentity)
			this
		else {
			val valueProjection = selectClauseReordering(reordering)
			new SelectIdSQL(id, columnName, idColumn, spelling.realign(value, valueProjection))
		}

	protected def selectClauseReordering(reordering :Rearrangement)(implicit spelling :SQLSpelling) :Rearrangement =
		if (!reordering.isMapped(1) || reordering(1) != 1)
			throw new IllegalArgumentException(
				"Cannot reorder a compound select id column of `" + this + "` to other position than the first: " +
					reordering + "."
			)
		else
			new Rearrangement {
				override def columnCount = reordering.columnCount - 1
				override def underlyingColumnCount :Int = reordering.underlyingColumnCount - 1
				override def apply(index :Int) :Int = reordering(index + 1) - 1
				override def inverse(index :Int) :Int = reordering.inverse(index + 1) - 1
				override def exposed(index :Int) :Opt[Int] = reordering.exposed(index + 1).map(_ - 1)
				override def underlying(index :Int) :Opt[Int] = reordering.underlying(index + 1).map(_ - 1)
				override def isMapped(index :Int) :Boolean = reordering.isMapped(index + 1)
				override def isCovered(index :Int) :Boolean = reordering.isCovered(index + 1)
				override def toString = reordering.toString + ".project(2.." + reordering.columnCount + ")"
			}


	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		AlignableColumn.selectId(columnName, idColumn) +: spelling.potentialColumns(value, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
		spelling.potentialColumnsCount(value) + 1

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
		idColumn +: spelling.split(value)

	protected override def shape(implicit spelling :SQLSpelling) :RowShape =
		ColumnForm[String].sqlType +: spelling.shape(value)

	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		spelling.columnCount(value) + 1

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		spelling.explode(idColumn)(from, context, params) ++: spelling.explode(value)(from, context, params)


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
			case comp :LValueSQL[F, MappingAt, V] @unchecked =>
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


	private class SelectIdTransformation[V](override val id :String, val columnName :String, val column :GroundColumn[String])
		extends SelectIdFormTransformation[V](id) with GenericTransformation[V, V, SelectIdSQL]
	{
		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingOps[F, S, V, E]) :SelectIdSQL[F, S, V] =
			denullify(expr) match {
					case lvalue :LValueSQL[F @unchecked, MappingAt @unchecked, V @unchecked] =>
					new SelectAsIdSQL(id, columnName, column, lvalue)
				case _ =>
					new SelectIdSQL(id, columnName, column, expr)
			}

		override def toString :String = "SelectId(" + id + " as " + columnName + ")"
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




/*
private class SelectIdReadForm[+V](val form :SQLReadForm[V])
	extends AbstractSQLReadForm[V](form.columnCount + 1)(form.nulls)
{
	override val text = textOf(form).map("String::" + _)
	override def columnTypes :Seq[JDBCType] = JDBCType.VARCHAR +: form.columnTypes
	override def opt(res :ResultSet, position :Int) :Opt[V] = form.opt(res, position + 1)

	override def equals(that :Any) :Boolean = that match {
		case other :SelectIdReadForm[_] if other canEqual this => form == other.form
		case _ => false
	}
	override def hashCode :Int = form.hashCode
}

private case class SelectIdForm[V](val id :String, override val form :SQLForm[V])
	extends SelectIdReadForm[V](form) with SQLForm[V] with WriteFormLiterals[V]
{
	override def set(statement :PreparedStatement, position :Int, value :V) :Unit = {
		statement.setString(position,id)
		form.set(statement, position + 1, value)
	}
	override def setNull(statement :PreparedStatement, position :Int) :Unit = {
		statement.setNull(position, java.sql.Types.VARCHAR)
		form.setNull(statement, position + 1)
	}
	override def columnLiterals(value :V) = id +: form.columnLiterals(value)
	override def nullColumnLiterals = "null" +: form.nullColumnLiterals

	override def split = (ColumnWriteForm.const(id) :ColumnWriteForm[V]) +: form.split

	override def equals(that :Any) :Boolean = that match {
		case other :SelectIdForm[_] if other canEqual this => id == other.id && form == other.form
		case _ => false
	}
	override def hashCode :Int = id.hashCode * 31 + form.hashCode
}
*/

private abstract class SelectIdFormTransformation[V](val id :String)
	extends SQLFormConversion[V, V] with SQLValueIdentity[V]
{
//	override def apply(form :SQLReadForm[V]) :SQLReadForm[V] = new SelectIdReadForm(form)
//	override def apply(form :SQLForm[V]) :SQLForm[V] = new SelectIdForm(id, form)
	override def apply(form :SQLReadForm[V]) :SQLReadForm[V] =
		form.reorder(Rearrangement.shift(1, form.columnCount))
	override def apply(form :SQLForm[V]) :SQLForm[V] =
		form.reorder(Rearrangement.shift(1, form.columnCount))

	override def apply(form :ColumnReadForm[V]) :ColumnReadForm[V] =
		throw new UnsupportedOperationException(toString + " cannot transform a ColumnForm " + form + ".")

	override def apply(form :ColumnForm[V]) :ColumnForm[V] =
		throw new UnsupportedOperationException(toString + " cannot transform a ColumnForm " + form + ".")
}






//currently unused
//todo: make it accept any MappingSQL

final class SelectAsIdSQL[-F <: RowProduct, M[A] <: MappingAt[A], V] private[ast]
                         (override val id :String, override val columnName :String,
                          override val idColumn :ColumnSQL[RowProduct, Single, String],
                          override val value :LValueSQL[F, M, V])
	extends SelectIdSQL[F, Single, V](id, columnName, idColumn, value)
	   with TransformedMappingDefaults[F, Single, M, V, V]
	   with ConvertingTemplate[F, Single, V, ({ type E[v] = SelectAsIdSQL[F, M, v] })#E]
	   with VariantGroundingTemplate[F, Single, V, ({ type E[-f <: RowProduct] = SelectAsIdSQL[f, M, V] })#E]
	   with ReorderingTemplate[F, Single, V, SelectAsIdSQL[F, M, V]]
	   with ReshapedSQLTemplate[F, Single, V, V, SelectAsIdSQL.meta[M]#apply]
		//Unfortunately cannot extend it for SelectIdSQL, because MappingSQL mixes in ConvertingTemplate for MappingSQL.
	   with WrappedSQLTemplate[F, Single, V, SelectAsIdSQL.meta[M]#apply]
//	   with MappingSQLTemplate[F, Single, M, V, SelectAsIdSQL[F, M, V]]
{ self =>
	def this(id :String, columnName :String, selectClause :LValueSQL[F, M, V]) =
		this(id, columnName, ColumnLiteral(id) as columnName, selectClause)
/*
	override type Origin        = selectClause.Origin
	override val mapping        :M[Origin] = selectClause.mapping
	override val export         :TypedMapping[M[Unit]#Subject, Origin] = selectClause.export
	override val anchored       :TypedMapping[M[Unit]#Subject, Origin] = selectClause.anchored
*/
	override def conversion     :SQLConversion[M[Unit]#Subject, V] = value.conversion
	override def transformation :IndependentTransformation.Convertible[V, V, SelectAsIdSQL.meta[M]#apply] =
		new SelectAsIdTransformation[M, V](id, columnName, idColumn, selectClause.mapping.castParam[Unit])


	override def default :SelectAsIdSQL[F, M, V] =
		if (isDefault) this else new SelectAsIdSQL(id, columnName, idColumn, value.default)

	override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
			:SelectAsIdSQL[F, M, V] =
		new SelectAsIdSQL(id, columnName, idColumn, value.defaultWith(includes, excludes))

	override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
			:SelectAsIdSQL[F, M, V] =
		new SelectAsIdSQL(id, columnName, idColumn, value.alter(includes, excludes))

	//override the implementation from MappingSQL
	protected override def convert[X](conversion :SQLConversion[V, X]) :SelectAsIdSQL[F, M, X] =
		new SelectAsIdSQL(id, columnName, idColumn, conversion(selectClause))

//	protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :MappingSQL[E, C, M, V])
//			:MappingSQL[E, C, M, V] =
//		decorate(e)

//	protected override def decorate[E <: RowProduct, C >: Grouped <: Single, Y]
//                           (e :SQLExpression[E, C, Y]) :SelectAsIdSQL[E, M, Y] =
//		transformation(e)

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

	protected override def decorate[E <: RowProduct, C >: Grouped <: Single, X]
	                               (e :SQLExpression[E, C, X]) :SelectAsIdSQL[E, M, X] =
		e match {
			case same if same eq selectClause =>
				this.castFrom[SelectAsIdSQL[F, M, V], SelectAsIdSQL[E, M, X]]
			case lvalue :LValueSQL[F, M, V] @unchecked if lvalue.mapping identical mapping =>
				new SelectAsIdSQL(id, columnName, idColumn, lvalue)
			case _ => //todo: make SelectAsIdSQL accept any MappingSQL (by allowing it as a selectClause in SelectAs)
				throw new IllegalArgumentException(
					"Cannot wrap " + e + " in a SelectAsIdSQL because it is not a LValueSQL for mapping " +
						mapping + ". This is most likely a bug."
				)
		}


	protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                   (implicit leftResult  :SQLTransformation[V, U],
                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.Expression[F, Single, SelectAsIdSQL[F, M, U]], rightResult.Expression[F2, S2, EC2[U]])] =
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
		new BaseReformer[F2, S2, V2, EC2, U, leftResult.Expression, rightResult.Expression](
			other)(reform, passCount)(leftResult, rightResult, spelling
		) {
			override def selectId(e :SelectIdSQL[F2, S2, V2])  =
				reform(selectClause, e.selectClause)(
					//these casts may be possible to remove with a dedicated transformation implementation
					transformation andThen leftResult, e.transformation andThen rightResult, spelling
				).asInstanceOf[Result]
		}
	}

	protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :SelectAsIdSQL[F, M, V] = {
		val valueProjection = selectClauseReordering(reordering)
		spelling.realign(value, valueProjection) match {
			case lvalue :LValueSQL[F, M, V] =>
				new SelectAsIdSQL(id, columnName, idColumn, lvalue)
			case other => //todo: this exception should go away once we allow to SelectAs any MappingSQL
				throw new IllegalExpressionException(
					"Reordering of an LValueSQL `" + value + "` produced `" + other + "`: " + other.className + "."
				)
		}
	}

/*

	override def canExclude(component :TypedColumn[_, Origin])(implicit spelling :SQLSpelling) :Boolean =
		selectClause.canExclude(component)

	override def canInclude(component :TypedColumn[_, Origin])(implicit spelling :SQLSpelling) :Boolean =
		selectClause.canInclude(component)

	override def isIncluded(component :TypedColumn[_, Origin])(implicit spelling :SQLSpelling) :Boolean =
		selectClause.isIncluded(component)

	override def columns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
		selectClause.columns

	override def defaultColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
		selectClause.defaultColumns

	override def mandatoryColumns(implicit spelling :SQLSpelling) :Unique[TypedColumn[_, Origin]] =
		selectClause.mandatoryColumns

	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[M[Unit]#Subject] = {
		val selectClauseForm = spelling.effectiveForm(selectClause)
		selectClauseForm.reorder(Rearrangement.shift(1, selectClause.columnCount))
	}
	//		new SelectIdForm(id, selectClause.`->effectiveForm`)

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL.from[F]#rows[Single]#__] =
		idColumn +: spelling.split(selectClause)
*/

	protected override def potentialColumns(permissions :Permissions)(implicit spelling :SQLSpelling) :AlignableColumns =
		super[SelectIdSQL].potentialColumns(permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
		super[SelectIdSQL].potentialColumnsCount

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

	type meta[M[A] <: MappingAt[A]] = {
		type apply[-F <: RowProduct, -s >: Grouped <: Single, V] = SelectAsIdSQL[F, M, V]
	}
	type __ = SelectAsIdSQL[_, M, _] forSome { type M[O] <: MappingAt[O] }
	type AnyIn[-F <: RowProduct] = SelectAsIdSQL[F, M, _] forSome { type M[O] <: MappingAt[O] }
	type Cast[-F <: RowProduct, V] = SelectAsIdSQL[F, M, V] forSome { type M[O] <: MappingAt[O] }


	private class SelectAsIdTransformation[M[A] <: MappingAt[A], V]
	                                      (override val id :String, val columnName :String,
	                                       val column :GroundColumn[String], val mapping :M[Unit])
		extends SelectIdFormTransformation[V](id) with GenericTransformation[V, V, SelectAsIdSQL.meta[M]#apply]
	{
		override def isUniversal     :Boolean = false

		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingOps[F, S, V, E]) :SelectAsIdSQL[F, M, V] =
			denullify(expr) match {
				case lvalue :LValueSQL[F, M, V] @unchecked if lvalue.mapping identical mapping =>
					new SelectAsIdSQL(id, columnName, column, lvalue)
				case _ =>
					throw new IllegalArgumentException(
						"Cannot wrap " + expr + " in a SelectAsIdSQL because it is not a LValueSQL for mapping " +
							mapping + ". This is most likely a bug."
					)
			}

		override def toString :String = "SelectId(" + id + " as " + columnName + ")"
	}


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
	extends UnaryCompositeSQL[Nothing, Grouped, V, V] //extended only to throw exceptions from grounding methods
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

	protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                 (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
                                           spelling :SQLSpelling)
			:(leftResult.Expression[Nothing, Grouped, CompoundSelectIdSQL[U]], rightResult.Expression[F2, S2, EC2[U]]) =
	{
		if (this eq other)
			(leftResult[Nothing, Grouped, CompoundSelectIdSQL](this), rightResult(other))
		else if (passCount.firstTime)
			super.reform(other)(reform, passCount)
		else {
			(if (reform.mayAlterRight) try {
				val reformRight = reform.prohibitReformLeft
//				val reformed = reformSelectClause(value, reformRight)._2
				val reformed =
					reformRight[Nothing, Grouped, V, ({type E[v] = SelectIdSQL[Nothing, Grouped, v]})#E, F2, S2, V2, EC2, U](
						value, other
					)._2
				if (!selectClauses.forall(reform.compatible(_, other)))
					Lack
				else
					Got((leftResult[Nothing, Grouped, CompoundSelectIdSQL](this), reformed))
			} catch {
				case _ :MismatchedExpressionsException | _ :MisalignedExpressionException => Lack
			} else
				Lack
			).getOrElse(
				leftResult match {
					case conversion :SQLConversion[V, U] =>
						def reformSelectClause(selectClause :SelectIdSQL[Nothing, Grouped, V], reform :Reform) =
//								:(leftResult.Expression[Nothing, Grouped, SelectIdSQL[Nothing, Grouped, U]], rightResult.Expression[F2, S2, EC2[U]]) =
							reform[Nothing, Grouped, V, ({ type E[v] = SelectIdSQL[Nothing, Grouped, v] })#E, F2, S2, V2, EC2, U](
								selectClause, other
							)(conversion, rightResult, spelling)
						val reformLeft = reform.prohibitReformRight
						val (shape, reformed) = reformSelectClause(value, reformLeft)
						val reformedClauses = selectClauses.map { selectClause =>
							reformSelectClause(selectClause, reformLeft) match {
								case (selectId :SelectIdSQL[Nothing, Grouped, U], _) => selectId
								case (l, _) =>
									throw new MismatchedExpressionsException(l,
										"Failed to reform " + this + " with " + other +": a SelectIdSQL expression " +
											selectClause + " was reformed to a different type: " + l + " :" + l.className + "."
									)
							}
						}
						//cast is safe because we checked that leftResult is an SQLConversion, so its SQLResult is CompoundSelectIdSQL[U]
						val left = CompoundSelectIdSQL(shape, reformedClauses).castFrom[
							CompoundSelectIdSQL[U], leftResult.Expression[Nothing, Grouped, CompoundSelectIdSQL[U]]
						]
						(left, reformed)
					case _ =>
						throw new IllegalArgumentException(
							"A non-conversion " + leftResult + " passed for transforming a CompoundSelectIdSQL " + self +
								" after reforming it with " + other + "."
						)
				}
			)
		}
	}

	protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling)
			:CompoundSelectIdSQL[V] =
		CompoundSelectIdSQL(spelling.realign(value, reordering), selectClauses.map(spelling.realign(_, reordering)))

	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		AlignableColumn.selectId(columnName, value.idColumn) +: spelling.potentialColumns(value, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
		spelling.potentialColumnsCount(value) + 1

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

	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[V] =
		spelling.inSelect.effectiveForm(value) //should we make it a case form if the compound select is a union all?

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

	def apply[R](shape :SelectIdSQL[Nothing, Grouped, R], selectClauses :Seq[SelectIdSQL[Nothing, Grouped, R]])
			:CompoundSelectIdSQL[R] =
	{
		val main = shape
		val seq = selectClauses
		if (selectClauses.isEmpty)
			throw new IllegalArgumentException("Cannot create a CompoundSelectIdSQL for zero select clauses.")
		new CompoundSelectIdSQL[R] with RowShapeCache {
			override val value = main
			override val selectClauses = seq
		}
	}

	def apply[R](selectClauses: => Seq[SelectIdSQL[Nothing, Grouped, R]]) :CompoundSelectIdSQL[R] = {
		class LazyCompoundSelectIdSQL(clauses: => Seq[SelectIdSQL[Nothing, Grouped, R]])
			extends CompoundSelectIdSQL[R] with RowShapeCache
		{
			override lazy val value = selectClauses.head
			override lazy val selectClauses = clauses
		}
		new LazyCompoundSelectIdSQL(selectClauses)
	}

	def apply[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
	         (query :CompoundSelectTemplate[R, Q, Self],
	          selectClauses :CompoundSelectTemplate[R, Q, Self] => Seq[SelectIdSQL[Nothing, Grouped, R]])
			:CompoundSelectIdSQL[R] =
		apply(selectClauses(query))

	def apply[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]](query :CompoundSelectTemplate[R, Q, Self]) :CompoundSelectIdSQL[R] =
		CompoundSelectIdSQL[R, Q, Self](query, selectClauses[R, Q, Self](_ :CompoundSelectTemplate[R, Q, Self]))

	private[sql] def apply[R, Q[X] <: QueryTemplate[X, Q]]
	                      (left :QueryTemplate[R, Q], right :QueryTemplate[R, Q]) :CompoundSelectIdSQL[R] =
		CompoundSelectIdSQL[R](try {
			(left.constituents.view ++ right.constituents).map(selectClause[R, Q]) to PassedArray
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


	private def selectClause[V, Q[X] <: QueryTemplate[X, Q]]
	                        (select :QueryTemplate[V, Q]) :SelectIdSQL[Nothing, Grouped, V] =
		select.selectClause match {
			case id :SelectIdSQL[Nothing, Grouped, V] => id
			case other =>
				throw new IllegalStateException(
					"the select clause of `" + select + "` is not a SelectIdSQL: " + other.localClassName + "."
				)
		}

	private def selectClauses[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
	                         (query :CompoundSelectTemplate[R, Q, Self]) :Seq[SelectIdSQL[Nothing, Grouped, R]] =
		try { query.constituents.map(selectClause(_)) } catch {
			case e :IllegalStateException =>
				throw new IllegalStateException(
					"Cannot create a form for compound select `" + query + "`: " + e.getMessage, e
				)
		}
}



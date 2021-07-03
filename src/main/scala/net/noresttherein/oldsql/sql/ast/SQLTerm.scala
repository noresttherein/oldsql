package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{ConstSeq, Opt, ReversedList}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.InseparableExpressionException
import net.noresttherein.oldsql.morsels.generic.{Fixed, Self}
import net.noresttherein.oldsql.pixies.RecordingPreparedStatement
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.FormBasedFactory
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormBasedFactory
import net.noresttherein.oldsql.slang.IterableExtension
import net.noresttherein.oldsql.sql.{ColumnSQL, GlobalBoolean, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLBoolean.{False, True}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, GlobalSQL, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.SQLLiteral.{CaseLiteral, LiteralVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnLiteral.{CaseColumnLiteral, ColumnLiteralVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeNull.{CaseCompositeNull, CompositeNullVisitor}
import net.noresttherein.oldsql.sql.ast.SQLNull.{CaseNull, NullVisitor}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm.ColumnTermVisitor
import net.noresttherein.oldsql.sql.ast.SQLTerm.NativeColumnTerm.{CaseNativeColumnTerm, NativeColumnTermVisitor}
import net.noresttherein.oldsql.sql.ast.SQLTerm.NativeTerm.{CaseNative, NativeVisitor}
import net.noresttherein.oldsql.sql.ast.BoundParam.{CaseParameter, ParameterVisitor}
import net.noresttherein.oldsql.sql.ast.BoundParamColumn.{CaseParameterColumn, ParameterColumnVisitor}
import net.noresttherein.oldsql.sql.jdbc.PreparedStatementProxy
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLOrdering}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** Base type for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] implementations representing various
  * atomic terms. The atomicity is in regard to the SQL AST defined in this package, but not necessarily the generated
  * SQL, as instances for types `T` represented by multi column [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]s
  * are possible.
  * @see [[net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm]]
  */
trait SQLTerm[T] extends SQLExpression[RowProduct, GlobalScope, T] {
//	protected def groundValue :Option[T]
	override def isGlobal = true
	override def asGlobal :Option[GlobalSQL[RowProduct, T]] = Some(this)
	override def isAnchored = true
	override def isAnchored(from :RowProduct) = true
	override def anchor(from :RowProduct) :SQLTerm[T] = this

	override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SQLTerm[T] = this

	override def expand[U <: RowProduct, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :SQLTerm[T] =
		this

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//	                    (matcher :ExpressionVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
//		matcher.term(this)

	override def columnCount(implicit spelling :SQLSpelling) :Int = readForm.readColumns

	def sameAs(that :Any) :Boolean = canEqual(that)

	override def isomorphic(expression: SQLExpression.*): Boolean = expression match { //this == expression
		case self :AnyRef if self eq this => true
		case term :SQLTerm[_] if term sameAs this => term.groundValue == groundValue
		case _ => false
	}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLTerm[_]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case term :SQLTerm[_] if term.canEqual(this) =>
			term.groundValue == groundValue && term.readForm == readForm
		case _ => false
	}

	override def hashCode :Int = groundValue.hashCode * 31 + readForm.hashCode

}






object SQLTerm {
	def apply[T](literal :T)(implicit factory :TermFactory.Factory[T]) :factory.Res = TermFactory(literal)

	type Factory[T] = TermFactory.Factory[T]

	object TermFactory extends FormBasedFactory[Self, SQLTerm, ColumnTerm] {
		protected override def generalResult[T :SQLForm](arg :T) :SQLTerm[T] = arg match {
			case null => new CompositeNull[T]
			case _ => new SQLLiteral(arg)
		}

		protected override def specificResult[T :ColumnForm](arg :T) :ColumnTerm[T] = arg match {
			case null => new SQLNull[T]
			case _ => new ColumnLiteral(arg)
		}
	}


	trait ColumnTerm[T] extends SQLTerm[T] with ColumnSQL[RowProduct, GlobalScope, T] {
		override def asGlobal :Option[ColumnSQL[RowProduct, GlobalScope, T]] = Some(this)

		override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnTerm[T] =
			this

		override def expand[U <: RowProduct, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :ColumnTerm[T] =
			this

		override def anchor(from :RowProduct) :ColumnTerm[T] = this

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
//			matcher.term(this)

		override def inParens[P](from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
		                        (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			spelling(this)(from, context, params)
	}



	object ColumnTerm {

		trait ColumnTermVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ColumnLiteralVisitor[F, Y] with NullVisitor[F, Y] with ParameterColumnVisitor[F, Y]
			   with NativeColumnTermVisitor[F, Y]
		{
			def term[X](e :ColumnTerm[X]) :Y[GlobalScope, X]
		}

		trait MatchColumnTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnTermVisitor[F, Y]
			with CaseColumnLiteral[F, Y] with CaseNull[F, Y] with CaseParameterColumn[F, Y]
			with CaseNativeColumnTerm[F, Y]

		trait CaseColumnTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchColumnTerm[F, Y] {
			override def literal[X](e :ColumnLiteral[X]) :Y[GlobalScope, X] = term(e)
			override def param[X](e :BoundParamColumn[X]) :Y[GlobalScope, X] = term(e)
			override def sqlNull[X](e :SQLNull[X]) :Y[GlobalScope, X] = term(e)
			override def native[X](e :NativeColumnTerm[X]) :Y[GlobalScope, X] = term(e)
		}
	}


	implicit def nullColumn[T :ColumnForm](self :SQLNull.type) :SQLNull[T] = SQLNull[T]
	implicit def nullComposite[T :SQLForm](self :SQLNull.type) :CompositeNull[T] = CompositeNull[T]



	class NativeTerm[T](val sql :String)(implicit override val readForm :SQLReadForm[T]) extends SQLTerm[T] {
		override def groundValue :Opt[T] = Lack

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor: ExpressionVisitor[RowProduct, Y]): Y[GlobalScope, T] =
			visitor.native(this)

		override def split(implicit scope :OperationType) :Seq[ColumnSQL[RowProduct, GlobalScope, _]] =
			throw new InseparableExpressionException("Native term " + this + " cannot be split into columns.")

		protected override def defaultSpelling[P]
		                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
		                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			SpelledSQL(sql, context)

		protected override def explodedSpelling[P]
		                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
		                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
			readForm.readColumns match {
				case 0 => Nil
				case 1 => defaultSpelling(from, context, params)::Nil
				case n => throw new InseparableExpressionException(this)
			}

		override def isomorphic(expression :SQLExpression.*) :Boolean = expression match {
			case self :AnyRef if self eq this => true
			case native :NativeTerm[_] if sameAs(native) && (native sameAs this) => sql == native.sql
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case native :NativeTerm[_] if native canEqual this => sql == native.sql && readForm == native.readForm
			case _ => false
		}
		override def hashCode :Int = sql.hashCode * 31 + readForm.hashCode

		override def toString :String = sql
	}


	object NativeTerm extends ReadFormBasedFactory[Fixed[String]#T, NativeTerm, NativeColumnTerm] {

		def unapply(e :SQLExpression[Nothing, LocalScope, _]) :Opt[String] = e match {
			case native :NativeTerm[_] => Got(native.sql)
			case _ => Lack
		}

		protected override def generalResult[T :SQLReadForm](arg :String) :NativeTerm[T] =
			new NativeTerm[T](arg)

		protected override def specificResult[T :ColumnReadForm](arg :String) :NativeColumnTerm[T] =
			new NativeColumnTerm(arg)


		trait NativeVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends NativeColumnTermVisitor[F, Y]
		{
			def native[X](e :NativeTerm[X]) :Y[GlobalScope, X]
		}

		type MatchNative[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseNative[F, Y]

		trait CaseNative[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends NativeVisitor[F, Y] {
			override def native[X](e :NativeColumnTerm[X]) :Y[GlobalScope, X] = native(e :NativeTerm[X])
		}
	}




	class NativeColumnTerm[T](sql :String)(implicit override val readForm :ColumnReadForm[T])
		extends NativeTerm[T](sql) with ColumnTerm[T]
	{
		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ColumnVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
			visitor.native(this)
	}



	object NativeColumnTerm {
		def apply[T :ColumnReadForm](sql :String) :NativeColumnTerm[T] = new NativeColumnTerm[T](sql)

		def unapply(e :SQLExpression[Nothing, LocalScope, _]) :Opt[String] = e match {
			case native :NativeColumnTerm[_] => Got(native.sql)
			case _ => Lack
		}


		trait NativeColumnTermVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def native[X](e :NativeColumnTerm[X]) :Y[GlobalScope, X]
		}

		type MatchNativeColumnTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NativeColumnTermVisitor[F, Y]

		type CaseNativeColumnTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NativeColumnTermVisitor[F, Y]
	}




	trait TermVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnTermVisitor[F, Y]
		with LiteralVisitor[F, Y]  with CompositeNullVisitor[F, Y] with ParameterVisitor[F, Y] with NativeVisitor[F, Y]
	{
		def term[X](e :SQLTerm[X]) :Y[GlobalScope, X]
	}

	trait MatchTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends TermVisitor[F, Y]
		with CaseLiteral[F, Y]  with CaseCompositeNull[F, Y] with CaseParameter[F, Y] with CaseNative[F, Y]
	{
		override def term[X](e :ColumnTerm[X]) :Y[GlobalScope, X] = term(e :SQLTerm[X])
	}

	trait CaseTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchTerm[F, Y] {
		def param[X](e: BoundParam[X]): Y[GlobalScope, X] = term(e)

		def literal[X](e: SQLLiteral[X]): Y[GlobalScope, X] = term(e)

		def sqlNull[X](e: CompositeNull[X]): Y[GlobalScope, X] = term(e)

		def native[X](e: NativeTerm[X]): Y[GlobalScope, X] = term(e)
	}

}






class SQLLiteral[T](val value :T)(implicit val form :SQLForm[T]) extends SQLTerm[T] {
	//type of Unit rather than Any to prevent accidental use for other values
	val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const(value)(form.writer)
	override def readForm :SQLReadForm[T] = form.reader

	override def groundValue :Opt[T] = Got(value)

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor: ExpressionVisitor[RowProduct, Y]): Y[GlobalScope, T] =
		visitor.literal(this)

	override def split(implicit scope :OperationType) :Seq[ColumnSQL[RowProduct, GlobalScope, _]] =
		throw new InseparableExpressionException("Multi column SQL literal " + this + " cannot be currently split.")

	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		SpelledSQL(form.literal(value), context)

	protected override def explodedSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		readForm.readColumns match { //fixme: make form.inlineLiteral return a seq
			case 0 => Nil
			case 1 => defaultSpelling(from, context, params)::Nil
			case _ => throw new InseparableExpressionException(this)
		}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLLiteral[_]]
//		override def sameAs(that :Any) :Boolean = that.isInstanceOf[SQLLiteral[_]]

	override def toString :String = String.valueOf(value)
}



object SQLLiteral extends FormBasedFactory[Self, SQLLiteral, ColumnLiteral] {
	//consider: making SQLNull an SQLLiteral and handle null values gracefully

// 		def apply[T](value :T, form :SQLForm[T]) :SQLLiteral[T] = new SQLLiteral(value)(form)

	def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[T] = e match {
		case literal :SQLLiteral[T @unchecked] => Got(literal.value)
		case _ => Lack
	}

	protected override def generalResult[T :SQLForm](arg :T) :SQLLiteral[T] = new SQLLiteral(arg)
	protected override def specificResult[T :ColumnForm](arg :T) :ColumnLiteral[T] = new ColumnLiteral(arg)



	trait LiteralVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnLiteralVisitor[F, Y] {
		def literal[X](e :SQLLiteral[X]) :Y[GlobalScope, X]
	}

	type MatchLiteral[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseLiteral[F, Y]

	trait CaseLiteral[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends LiteralVisitor[F, Y] with CaseColumnLiteral[F, Y]
	{
		override def literal[X](f :ColumnLiteral[X]) :Y[GlobalScope, X] = literal(f :SQLLiteral[X])
	}

}






class ColumnLiteral[T](literal :T)(implicit override val form :ColumnForm[T])
	extends SQLLiteral[T](literal) with ColumnTerm[T]
{
	override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)(form.writer)
	override def readForm :ColumnReadForm[T] = form.reader

	override def &&[E <: RowProduct, O >: LocalScope <: GlobalScope]
	               (other: ColumnSQL[E, O, Boolean])(implicit ev :T =:= Boolean) :ColumnSQL[E, O, Boolean] =
		if (value) other else False

	override def ||[E <: RowProduct, O >: LocalScope <: GlobalScope]
	               (other: ColumnSQL[E, O, Boolean])(implicit ev :T =:= Boolean) :ColumnSQL[E, O, Boolean] =
		if (value) True else other

	override def unary_![E <: RowProduct, O >: LocalScope <: GlobalScope]
	                    (implicit ev :T =:= Boolean) :ColumnSQL[E, O, Boolean] =
		if (value) False else True

//		override def to[X](implicit lift :Lift[T, X]) :ColumnSQL[RowProduct, X] =
//			new ColumnLiteral[X](lift(value))(form.nullBimap(lift.apply)(lift.lower))

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ColumnVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
		visitor.literal(this)
}




object ColumnLiteral {
	def apply[T :ColumnForm](literal :T) :ColumnLiteral[T] = new ColumnLiteral[T](literal)

	def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[T] = e match {
		case literal :ColumnLiteral[T @unchecked] => Got(literal.value)
		case _ => Lack
	}


	trait ColumnLiteralVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def literal[X](f :ColumnLiteral[X]) :Y[GlobalScope, X]
	}

	trait MatchColumnLiteral[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnLiteralVisitor[F, Y]
	{
		def nonbool[X](e :ColumnLiteral[X]) :Y[GlobalScope, X]

		def bool(e :ColumnLiteral[Boolean]) :Y[GlobalScope, Boolean]

		override def literal[X](e: ColumnLiteral[X]): Y[GlobalScope, X] = e.value match {
			case _ :Boolean => bool(e.asInstanceOf[ColumnLiteral[Boolean]]).asInstanceOf[Y[GlobalScope, X]]
			case _ => nonbool(e)
		}
	}

	type CaseColumnLiteral[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnLiteralVisitor[F, Y]
}






class BoundParam[T](val value :T, val name :Option[String] = None)(implicit val form :SQLForm[T])
	extends SQLTerm[T]
{
	private[this] val constForm = SQLWriteForm.const[T](value)(form.writer)
	def writeForm            :SQLWriteForm[Unit] = constForm
	override def readForm    :SQLReadForm[T] = form.reader
	override def groundValue :Opt[T] = Got(value)

	private lazy val columns :Seq[BoundParamColumn[_]] = {
		val columnValues = new RecordingPreparedStatement(form.writtenColumns)
		form.set(columnValues, 1, value)
		form.split.mapWithIndex { (i, form) =>
			implicit val columnForm = form match {
				case f :ColumnForm[Any @unchecked] => f
				case other =>
					val readForm = ColumnReadForm.const(columnValues.parameterType(i + 1), columnValues(i + 1))
					other.asInstanceOf[ColumnWriteForm[Any]] <> readForm
			}
			BoundParamColumn(columnValues(i + 1), name.map(_ + "#" + i))
		}
	}


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor: ExpressionVisitor[RowProduct, Y]): Y[GlobalScope, T] =
		visitor.param(this)

	override def split(implicit scope :OperationType) :Seq[ColumnSQL[RowProduct, GlobalScope, _]] =
		try { columns } catch {
			case e :Exception => throw new InseparableExpressionException(
				"Failed to split multi column parameter " + this + ": " + e.getMessage + ".", e
			)
		}

	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		form.writtenColumns match {
			case 0 => SpelledSQL(context)
			case 1 => SpelledSQL("?", context, constForm)
			case n => SpelledSQL(form.param, context, constForm)
		}

	protected override def explodedSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
	try {
		constForm.split.map { SpelledSQL("?", context, _) }
	} catch { //fixme: this will work only if we don't reorder parameter expressions; two methods? seems too much for this case only.
		case e :UnsupportedOperationException => form.writtenColumns match {
			case 0 => Nil
			case n =>
				ReversedList.fill(n - 1)(SpelledSQL("?", context, SQLWriteForm.empty)) :+
					SpelledSQL("?", context, constForm)
		}
	}

//		override def sameAs(that :Any) :Boolean = that.isInstanceOf[BoundParam[_]]
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[BoundParam[_]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case p :BoundParam[_] if canEqual(p) && p.canEqual(this) =>
			name == p.name && value == p.value && form == p.form
		case _ => false
	}

	override def hashCode :Int =
		((if (value == null) 0 else value.hashCode) * 31 + name.hashCode) * 31 + form.hashCode


	override def toString :String = "?" + value
}



object BoundParam extends FormBasedFactory[Self, BoundParam, BoundParamColumn] {

	def apply[T](form :SQLForm[T])(value :T) :BoundParam[T] = new BoundParam(value)(form)

	def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[(T, Option[String])] = e match {
		case param :BoundParam[T @unchecked] => Got((param.value, param.name))
		case _ => Lack
	}

//		/** Provider of implicit converters `ParameterFactory[X]` from any Scala literal `X`
//		  * with an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]/[[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
//		  * to an [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]]`[X]`/[[net.noresttherein.oldsql.sql.ast.BoundParamColumn BoundParamColumn]]`[X]`
//		  */

	protected override def specificResult[X :ColumnForm](value :X) :BoundParamColumn[X] = BoundParamColumn(value)
	protected override def generalResult[X :SQLForm](value :X) :BoundParam[X] = BoundParam(value)

	/** A factory lifting any literal `X` into an [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]]`[X]`
	  * or its subclass [[net.noresttherein.oldsql.sql.ast.BoundParamColumn BoundParamColumn]]`[X]`,
	  * depending on whether an implicit [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
	  * or [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists.
	  */
	type ParameterFactory[X, P <: BoundParam[X]] = DedicatedFactory[X, X] { type Res = P }


	trait ParameterVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ParameterColumnVisitor[F, Y] {
		def param[X](e :BoundParam[X]) :Y[GlobalScope, X]
	}

	type MatchParameter[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseParameter[F, Y]

	trait CaseParameter[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ParameterVisitor[F, Y] with CaseParameterColumn[F, Y]
	{
		override def param[X](e :BoundParamColumn[X]) :Y[GlobalScope, X] = param(e :BoundParam[X])
	}

}




class BoundParamColumn[T](param :T, name :Option[String] = None)(implicit override val form :ColumnForm[T])
	extends BoundParam[T](param, name) with ColumnTerm[T]
{
	override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)(form.writer)
	override def readForm :ColumnReadForm[T] = form.reader
	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ColumnVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
		visitor.param(this)
}


object BoundParamColumn {
	def apply[T :ColumnForm](param :T, name :Option[String] = None) :BoundParamColumn[T] =
		new BoundParamColumn[T](param, name)

	def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[(T, Option[String])] = e match {
		case param :BoundParamColumn[T] => Got((param.value, param.name))
		case _ => Lack
	}


	trait ParameterColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def param[X](e :BoundParamColumn[X]) :Y[GlobalScope, X]
	}

	type MatchParameterColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ParameterColumnVisitor[F, Y]

	type CaseParameterColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ParameterColumnVisitor[F, Y]
}






class CompositeNull[T] private[ast](implicit val form :SQLForm[T]) extends SQLTerm[T] {
	def this(columnCount :Int) =
		this()(SQLReadForm.none[T](columnCount) <> SQLWriteForm.nulls(columnCount))

	val writeForm :SQLWriteForm[Unit] = SQLWriteForm.none[T](form.writer)
	override def readForm :SQLReadForm[T] = form.reader
	override def groundValue :Opt[T] = form.nulls.toOption
	override def isNull :GlobalBoolean[RowProduct] = True

	private lazy val isNullLiteralSeq = try {
		val literal = form.inlineNullLiteral
		val seq = ConstSeq("null", form.writtenColumns)
		(literal equalsIgnoreCase seq.mkString(", ")) || (literal equalsIgnoreCase seq.mkString(","))
	} catch {
		case e :Exception => false
	}

	private lazy val columns :Seq[SQLNull[_]] = form match {
		case col :ColumnForm[T] => SQLNull(col)::Nil
		case _ if form.writtenColumns == 0 => Nil
		case _ =>
			if (!isNullLiteralSeq)
				throw new InseparableExpressionException(
					"Cannot split composite null expression into separate columns: form's " + form +
						" null literal is not SQL NULL(s): " + form.inlineNullLiteral + "."
				)
			form.split.map { form =>
				val columnForm = form match {
					case f :ColumnForm[Any @unchecked] => f
					case other => //readForm doesn't matter, it won't be used, unless someone gets a reference for some other purpose
						other.asInstanceOf[ColumnWriteForm[Any]] <> ColumnReadForm.none()
				}
				SQLNull(columnForm)
			}
	}

	//consider: overriding and simplifying comparisons

	override def to[X](implicit lift :Lift[T, X]) :SQLExpression[RowProduct, GlobalScope, X] =
		new CompositeNull(form.writtenColumns)

	override def opt: GlobalSQL[RowProduct, Option[T]] = CompositeNull[Option[T]]


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ExpressionVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
		visitor.sqlNull(this)

	override def split(implicit scope :OperationType) :Seq[ColumnSQL[RowProduct, GlobalScope, _]] =
		try { columns } catch {
			case e :InseparableExpressionException => throw e
			case e :Exception =>
				throw new InseparableExpressionException(
					s"Cannot split composite null expression into separate columns: form $form cannot be split.", e
				)
		}


	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		SpelledSQL(form.nullLiteral, context)

	protected override def explodedSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		form.writtenColumns match {
			case 0 => Nil
			case 1 => SpelledSQL(form.inlineNullLiteral, context)::Nil
			case _ =>
				if (isNullLiteralSeq)
					ConstSeq(SpelledSQL(spelling.NULL, context), form.writtenColumns)
				else
					throw new InseparableExpressionException(
						"Cannot split composite null expression into separate columns: form's " + form +
							" null literal is not SQL NULL(s): " + form.inlineNullLiteral + "."
					)
		}


	override def sameAs(that :Any) :Boolean = that.isInstanceOf[CompositeNull[_]]

	override def toString :String =
		groundValue.filter(_ != null).map(v => v.toString + ":SQLNull") getOrElse form.nullLiteral
}




object CompositeNull {
	def apply[T :SQLForm] = new CompositeNull[T]

	def apply[T](columnCount :Int) = new CompositeNull[T](columnCount)

	def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
		case null => true
		case _ :CompositeNull[_] => true
		case SQLLiteral(null) => true
		case _ => false
	}


	implicit def nullComposite[T :SQLForm](self :CompositeNull.type) :CompositeNull[T] = CompositeNull[T]


	trait CompositeNullVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends NullVisitor[F, Y] {
		def sqlNull[X](e :CompositeNull[X]) :Y[GlobalScope, X]
	}

	type MatchCompositeNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseCompositeNull[F, Y]

	trait CaseCompositeNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends CompositeNullVisitor[F, Y] {
		override def sqlNull[X](e :SQLNull[X]) :Y[GlobalScope, X] = sqlNull(e :CompositeNull[X])
	}

}




class SQLNull[T] private[ast]
             (implicit override val form :ColumnForm[T] = ColumnReadForm.none[T]() <> ColumnWriteForm.nulls[T])
	extends CompositeNull[T] with ColumnTerm[T]
{
	override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.none[T](form.writer)
	override def readForm :ColumnReadForm[T] = form.reader

	override def <>[E <: RowProduct, O >: LocalScope <: GlobalScope, X, U]
	               (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[T, X, U])
			:ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]

	override def <=[E <: RowProduct, O >: LocalScope <: GlobalScope, X, U]
	               (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[T, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]

	override def < [E <: RowProduct, O >: LocalScope <: GlobalScope, X, U]
	              (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[T, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]

	override def >=[E <: RowProduct, O >: LocalScope <: GlobalScope, X, U]
	               (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[T, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]

	override def > [E <: RowProduct, O >: LocalScope <: GlobalScope, X, U]
	              (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[T, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]


	override def &&[E <: RowProduct, O >: LocalScope <: GlobalScope]
	               (other :ColumnSQL[E, O, Boolean])(implicit ev :T =:= Boolean) :ColumnSQL[E, O, Boolean] =
		cast

	override def ||[E <: RowProduct, O >: LocalScope <: GlobalScope]
	               (other :ColumnSQL[E, O, Boolean])(implicit ev :T =:= Boolean) :ColumnSQL[E, O, Boolean] =
		cast

	override def unary_![E <: RowProduct, O >: LocalScope <: GlobalScope]
	                    (implicit ev :T =:= Boolean) :ColumnSQL[E, O, Boolean] =
		cast

	override def to[X](implicit lift :Lift[T, X]) :ColumnSQL[RowProduct, GlobalScope, X] =
		new SQLNull[X]

	override def opt: ColumnSQL[RowProduct, GlobalScope, Option[T]] = SQLNull()

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ColumnVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
		visitor.sqlNull(this)


	override def toString :String =
		groundValue.filter(_ != null).map(_.toString + ":SQLNull") getOrElse "SQLNull"
}




object SQLNull extends FormBasedFactory[Fixed[Unit]#T, CompositeNull, SQLNull] {

	def apply[T](implicit factory :Factory[T]) :factory.Res = SQLNull[Unit, T](())

	def apply[T](form :ColumnForm[T] = ColumnReadForm.none[T]() <> ColumnWriteForm.nulls[T]) :SQLNull[T] =
		new SQLNull[T]()(form)

	def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
		case null => true
		case _ :SQLNull[_] => true
		case SQLLiteral(null) => true
		case _ => false
	}


	protected override def generalResult[T :SQLForm](arg :Unit) :CompositeNull[T] = new CompositeNull[T]
	protected override def specificResult[T :ColumnForm](arg :Unit) :SQLNull[T] = new SQLNull[T]

	private[oldsql] val boolean = SQLNull[Boolean]
	private[oldsql] val int = SQLNull[Int]
	private[oldsql] val long = SQLNull[Long]
	private[oldsql] val double = SQLNull[Double]
	private[oldsql] val string = SQLNull[String]

	trait NullVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def sqlNull[X](e :SQLNull[X]) :Y[GlobalScope, X]
	}

	type MatchNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NullVisitor[F, Y]

	type CaseNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NullVisitor[F, Y]

}




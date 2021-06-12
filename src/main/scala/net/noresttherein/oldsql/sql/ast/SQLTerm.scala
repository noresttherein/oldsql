package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{ConstSeq, Opt, ReversedList}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.InseparableExpressionException
import net.noresttherein.oldsql.morsels.generic.{Fixed, Self}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.FormBasedFactory
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormBasedFactory
import net.noresttherein.oldsql.sql.{ColumnSQL, GlobalBoolean, RowProduct, SQLBoolean, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, GlobalSQL, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.SQLLiteral.{CaseLiteral, False, LiteralVisitor, True}
import net.noresttherein.oldsql.sql.ast.ColumnLiteral.{CaseColumnLiteral, ColumnLiteralVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeNull.{CaseCompositeNull, CompositeNullVisitor}
import net.noresttherein.oldsql.sql.ast.SQLNull.{CaseNull, NullVisitor}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm.ColumnTermVisitor
import net.noresttherein.oldsql.sql.ast.SQLTerm.NativeColumnTerm.{CaseNativeColumnTerm, NativeColumnTermVisitor}
import net.noresttherein.oldsql.sql.ast.SQLTerm.NativeTerm.{CaseNative, NativeVisitor}
import net.noresttherein.oldsql.sql.ast.SQLParameter.{CaseParameter, ParameterVisitor}
import net.noresttherein.oldsql.sql.ast.SQLParameterColumn.{CaseParameterColumn, ParameterColumnVisitor}
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

		override def inParens[P, E <: RowProduct](context :SQLContext, params :Parameterization[P, E])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			spelling(this :ColumnSQL[E, GlobalScope, T])(context, params)
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
			override def param[X](e :SQLParameterColumn[X]) :Y[GlobalScope, X] = term(e)
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

		protected override def defaultSpelling[P, E <: RowProduct]
		                                      (context :SQLContext, params :Parameterization[P, E])
		                                      (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			SpelledSQL(sql, context, params)

		protected override def inlineSpelling[P, E <: RowProduct]
		                                     (context :SQLContext, params :Parameterization[P, E])
		                                     (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
			readForm.readColumns match {
				case 0 => Nil
				case 1 => defaultSpelling(context, params)::Nil
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
		def param[X](e: SQLParameter[X]): Y[GlobalScope, X] = term(e)

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

	override def split(implicit scope :OperationType) :Seq[ColumnSQL[RowProduct, GlobalScope, _]] = //fixme:
		throw new InseparableExpressionException("Multi column SQL literal " + this + " cannot be currently split.")

	protected override def defaultSpelling[P, E <: RowProduct]
	                                      (context :SQLContext, params :Parameterization[P, E])
	                                      (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		SpelledSQL(form.literal(value), context, params)

	protected override def inlineSpelling[P, E <: RowProduct]
	                                     (context :SQLContext, params :Parameterization[P, E])
	                                     (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
		readForm.readColumns match { //todo: make form.inlineLiteral return a seq
			case 1 => defaultSpelling(context, params)::Nil
			case n => throw new InseparableExpressionException(this)
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



	object True extends ColumnLiteral[Boolean](true) {
		/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
		def apply[F <: RowProduct] :GlobalBoolean[F] = this

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => v
			case _ => false
		}

		override def &&[E <: RowProduct, O >: LocalScope <: GlobalScope]
		               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = other

		override def ||[E <: RowProduct, O >: LocalScope <: GlobalScope]
		               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = this

		override def toString = "True"
	}


	object False extends ColumnLiteral[Boolean](false) {
		/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
		def apply[F <: RowProduct] :GlobalBoolean[F] = this

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => !v
			case _ => false
		}

		override def &&[E <: RowProduct, O >: LocalScope <: GlobalScope]
		               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = this

		override def ||[E <: RowProduct, O >: LocalScope <: GlobalScope]
		               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = other

		override def toString = "False"
	}




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






class SQLParameter[T](val value :T, val name :Option[String] = None)(implicit val form :SQLForm[T])
	extends SQLTerm[T]
{
	private[this] val constForm = SQLWriteForm.const[T](value)(form.writer)
	def writeForm :SQLWriteForm[Unit] = constForm
	override def readForm :SQLReadForm[T] = form.reader
	override def groundValue :Opt[T] = Got(value)


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor: ExpressionVisitor[RowProduct, Y]): Y[GlobalScope, T] =
		visitor.param(this)

	override def split(implicit scope :OperationType) :Seq[ColumnSQL[RowProduct, GlobalScope, _]] = //fixme:
		throw new InseparableExpressionException("Multi column parameter " + this + " cannot be currently split.")

	protected override def defaultSpelling[P, E <: RowProduct]
	                                      (context :SQLContext, params :Parameterization[P, E])
	                                      (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		constForm.writtenColumns match {
			case 0 => SpelledSQL(context, params)
			case 1 => SpelledSQL("?", context, params :+ constForm)
			case n => SpelledSQL(Iterator.fill(n)("?").mkString("(", ", ", ")"), context, params :+ constForm)
		}

	protected override def inlineSpelling[P, E <: RowProduct]
	                                     (context :SQLContext, params :Parameterization[P, E])
	                                     (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
	{
		val resultParams = params :+ constForm
		List.fill(constForm.writtenColumns)(SpelledSQL("?", context, resultParams))
	}

//		override def sameAs(that :Any) :Boolean = that.isInstanceOf[SQLParameter[_]]
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLParameter[_]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case p :SQLParameter[_] if canEqual(p) && p.canEqual(this) =>
			name == p.name && value == p.value && form == p.form
		case _ => false
	}

	override def hashCode :Int =
		((if (value == null) 0 else value.hashCode) * 31 + name.hashCode) * 31 + form.hashCode


	override def toString :String = "?" + value
}


object SQLParameter extends FormBasedFactory[Self, SQLParameter, SQLParameterColumn] {

	def apply[T](form :SQLForm[T])(value :T) :SQLParameter[T] = new SQLParameter(value)(form)

	def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[(T, Option[String])] = e match {
		case param :SQLParameter[T @unchecked] => Got((param.value, param.name))
		case _ => Lack
	}

//		/** Provider of implicit converters `ParameterFactory[X]` from any Scala literal `X`
//		  * with an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]/[[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
//		  * to an [[net.noresttherein.oldsql.sql.ast.SQLParameter SQLParameter]]`[X]`/[[net.noresttherein.oldsql.sql.ast.SQLParameterColumn SQLParameterColumn]]`[X]`
//		  */

	protected override def specificResult[X :ColumnForm](value :X) :SQLParameterColumn[X] = SQLParameterColumn(value)
	protected override def generalResult[X :SQLForm](value :X) :SQLParameter[X] = SQLParameter(value)

	/** A factory lifting any literal `X` into an [[net.noresttherein.oldsql.sql.ast.SQLParameter SQLParameter]]`[X]`
	  * or its subclass [[net.noresttherein.oldsql.sql.ast.SQLParameterColumn SQLParameterColumn]]`[X]`,
	  * depending on whether an implicit [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
	  * or [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists.
	  */
	type ParameterFactory[X, P <: SQLParameter[X]] = DedicatedFactory[X, X] { type Res = P }


	trait ParameterVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ParameterColumnVisitor[F, Y] {
		def param[X](e :SQLParameter[X]) :Y[GlobalScope, X]
	}

	type MatchParameter[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseParameter[F, Y]

	trait CaseParameter[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ParameterVisitor[F, Y] with CaseParameterColumn[F, Y]
	{
		override def param[X](e :SQLParameterColumn[X]) :Y[GlobalScope, X] = param(e :SQLParameter[X])
	}

}




class SQLParameterColumn[T](param :T, name :Option[String] = None)(implicit override val form :ColumnForm[T])
	extends SQLParameter[T](param, name) with ColumnTerm[T]
{
	override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)(form.writer)
	override def readForm :ColumnReadForm[T] = form.reader
	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ColumnVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
		visitor.param(this)
}


object SQLParameterColumn {
	def apply[T :ColumnForm](param :T, name :Option[String] = None) :SQLParameterColumn[T] =
		new SQLParameterColumn[T](param, name)

	def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[(T, Option[String])] = e match {
		case param :SQLParameterColumn[T] => Got((param.value, param.name))
		case _ => Lack
	}


	trait ParameterColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def param[X](e :SQLParameterColumn[X]) :Y[GlobalScope, X]
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

	//consider: overriding and simplifying comparisons

	override def to[X](implicit lift :Lift[T, X]) :SQLExpression[RowProduct, GlobalScope, X] =
		new CompositeNull(form.writtenColumns)

	override def opt: GlobalSQL[RowProduct, Option[T]] = CompositeNull[Option[T]]


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ExpressionVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
		visitor.sqlNull(this)

	override def split(implicit scope :OperationType) :Seq[ColumnSQL[RowProduct, GlobalScope, _]] = {
		groundValue.filter(_ != null).map { v => form match {
			case col :ColumnForm[T @unchecked] => ReversedList :+ SQLNull(col)
			case _ if form.writtenColumns == 0 => ReversedList.empty
//				case _ => form.split.map(SQLNull(_))
			case n => throw new InseparableExpressionException(
				s"Cannot render individual $n column strings for 'null' value $v :$form (${form.inlineLiteral(v)})."
			)
		}} getOrElse (form match {
			case col :ColumnForm[T @unchecked] => ReversedList :+ SQLNull(col)
			case _ if form.writtenColumns == 0 => ReversedList.empty
			case n =>
				throw new InseparableExpressionException(
					s"Cannot render individual $n column strings for null value of $form: ${form.inlineNullLiteral}."
				)
		})
	}


	protected override def defaultSpelling[P, E <: RowProduct]
	                                      (context :SQLContext, params :Parameterization[P, E])
	                                      (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
	{
		val literal = groundValue.filter(_ != null).map(v => form.literal(v)) getOrElse form.nullLiteral
		SpelledSQL(literal, context, params)
	}

	protected override def inlineSpelling[P, E <: RowProduct]
	                                     (context :SQLContext, params :Parameterization[P, E])
	                                     (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
	{
		val literals = groundValue.filter(_ != null).map { v => form.writtenColumns match {
			case 0 => Nil
			case 1 => form.inlineLiteral(v)::Nil //todo: refactor the form method to return a sequence
			case n => throw new InseparableExpressionException(
				s"Cannot render individual column strings for 'null' value $v :$form (${form.inlineLiteral(v)})."
			)
		}} getOrElse (form.writtenColumns match {
			case 0 => Nil
			case 1 => form.inlineNullLiteral::Nil
			case n if form.inlineNullLiteral.toLowerCase == Iterator.fill(n)("null").reduce(_ + ", " + _) =>
				ConstSeq("null", n)
			case n =>
				throw new InseparableExpressionException(
					s"Cannot render individual column strings for null value of $form: ${form.inlineNullLiteral}."
				)
		})
		literals.map(SpelledSQL(_, context, params))
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




package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.generic.{Const, Self}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.FormFunction
import net.noresttherein.oldsql.schema.SQLWriteForm.EmptyWriteForm
import net.noresttherein.oldsql.sql.{ast, ColumnSQL, GlobalBoolean, RowProduct, SQLBoolean, SQLExpression, SQLOrdering}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, GlobalSQL, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnLiteral.{CaseColumnLiteral, ColumnLiteralMatcher}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm.ColumnTermMatcher
import net.noresttherein.oldsql.sql.ast.SQLTerm.CompositeNull.{CaseCompositeNull, CompositeNullMatcher}
import net.noresttherein.oldsql.sql.ast.SQLTerm.NativeColumnTerm.{CaseNativeColumnTerm, NativeColumnTermMatcher}
import net.noresttherein.oldsql.sql.ast.SQLTerm.NativeTerm.{CaseNative, NativeMatcher}
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLLiteral.{CaseLiteral, LiteralMatcher}
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull.{CaseNull, NullMatcher}
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter.{CaseParameter, ParameterMatcher}
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameterColumn.{CaseParameterColumn, ParameterColumnMatcher}





/** Base type for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] implementations representing various
  * atomic terms. The atomicity is in regard to the SQL AST defined in this package, but not necessarily the generated
  * SQL, as instances for types `T` represented by multi column [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]s
  * are possible.
  * @see [[net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm]]
  */
trait SQLTerm[T] extends SQLExpression[RowProduct, GlobalScope, T] {
	protected def form :SQLForm[T]
	override def readForm :SQLReadForm[T] = form
	def writeForm :SQLWriteForm[Unit]

	protected def groundValue :Option[T]
	override def isGlobal = true
	override def asGlobal :Option[GlobalSQL[RowProduct, T]] = Some(this)
	override def isAnchored = true
	override def anchor(from :RowProduct) :SQLTerm[T] = this

	override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SQLTerm[T] = this

	override def expand[U <: RowProduct, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :SQLTerm[T] =
		this

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//	                    (matcher :ExpressionMatcher[RowProduct, Y]) :Y[GlobalScope, T] =
//		matcher.term(this)

	def sameAs(that :Any) :Boolean = canEqual(that)

	override def isomorphic(expression: SQLExpression.*): Boolean = expression match { //this == expression
		case self :AnyRef if self eq this => true
		case term :SQLTerm[_] if term sameAs this => term.groundValue == groundValue
		case _ => false
	}



	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case term :SQLTerm[_] if term.canEqual(this) =>
			term.groundValue == groundValue && term.form == form
		case _ => false
	}



	override def hashCode :Int = groundValue.hashCode * 31 + form.hashCode

}






object SQLTerm {

	trait ColumnTerm[T] extends SQLTerm[T] with ColumnSQL[RowProduct, GlobalScope, T] {
		override def form :ColumnForm[T]

		override def readForm :ColumnReadForm[T] = form

		override def asGlobal :Option[ColumnSQL[RowProduct, GlobalScope, T]] = Some(this)

		override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnTerm[T] =
			this

		override def expand[U <: RowProduct, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :ColumnTerm[T] =
			this

		override def anchor(from :RowProduct) :ColumnTerm[T] = this

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnMatcher[RowProduct, Y]) :Y[GlobalScope, T] =
//			matcher.term(this)
	}



	object ColumnTerm {

		trait ColumnTermMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ColumnLiteralMatcher[F, Y] with NullMatcher[F, Y] with ParameterColumnMatcher[F, Y]
			   with NativeColumnTermMatcher[F, Y]
		{
			def term[X](e :ColumnTerm[X]) :Y[GlobalScope, X]
		}

		trait MatchColumnTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnTermMatcher[F, Y]
			with CaseColumnLiteral[F, Y] with CaseNull[F, Y] with CaseParameterColumn[F, Y]
			with CaseNativeColumnTerm[F, Y]

		trait CaseColumnTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchColumnTerm[F, Y] {
			override def literal[X](e :ColumnLiteral[X]) :Y[GlobalScope, X] = term(e)
			override def param[X](e :SQLParameterColumn[X]) :Y[GlobalScope, X] = term(e)
			override def sqlNull[X](e :SQLNull[X]) :Y[GlobalScope, X] = term(e)
			override def native[X](e :NativeColumnTerm[X]) :Y[GlobalScope, X] = term(e)
		}
	}






	class SQLLiteral[T](val value :T)(implicit override val form :SQLForm[T]) extends SQLTerm[T] {
		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const(value)

		override def groundValue :Option[T] = Some(value)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher: ExpressionMatcher[RowProduct, Y]): Y[GlobalScope, T] =
			matcher.literal(this)

		override def sameAs(that :Any) :Boolean = that.isInstanceOf[SQLLiteral[_]]

		override def toString :String = String.valueOf(value)
	}



	object SQLLiteral extends FormFunction[Self, SQLLiteral, ColumnLiteral] {
		//consider: making SQLNull an SQLLiteral and handle null values gracefully
		//todo: sensible and consistent overloading of apply method so one can pass a form.

// 		def apply[T :SQLForm](value :T) :SQLLiteral[T] = new SQLLiteral(value)


		def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[T] = e match {
			case literal :SQLLiteral[T @unchecked] => Got(literal.value)
			case _ => Lack
		}


		protected override def multiColumn[T :SQLForm](arg :T) :SQLLiteral[T] = new SQLLiteral(arg)
		protected override def singleColumn[T :ColumnForm](arg :T) :ColumnLiteral[T] = new ColumnLiteral(arg)


		trait LiteralMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnLiteralMatcher[F, Y] {
			def literal[X](e :SQLLiteral[X]) :Y[GlobalScope, X]
		}

		type MatchLiteral[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseLiteral[F, Y]

		trait CaseLiteral[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends LiteralMatcher[F, Y] with CaseColumnLiteral[F, Y]
		{
			override def literal[X](f :ColumnLiteral[X]) :Y[GlobalScope, X] = literal(f :SQLLiteral[X])
		}

	}






	class ColumnLiteral[T](literal :T)(implicit override val form :ColumnForm[T])
		extends SQLLiteral[T](literal) with ColumnTerm[T]
	{
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

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

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[RowProduct, Y]) :Y[GlobalScope, T] =
			matcher.literal(this)

	}



	object ColumnLiteral {
		def apply[T :ColumnForm](literal :T) :ColumnLiteral[T] = new ColumnLiteral[T](literal)

		def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[T] = e match {
			case literal :ColumnLiteral[T @unchecked] => Got(literal.value)
			case _ => Lack
		}


		trait ColumnLiteralMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def literal[X](f :ColumnLiteral[X]) :Y[GlobalScope, X]
		}

		trait MatchColumnLiteral[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnLiteralMatcher[F, Y] {
			def nonbool[X](e :ColumnLiteral[X]) :Y[GlobalScope, X]

			def bool(e :ColumnLiteral[Boolean]) :Y[GlobalScope, Boolean]

			override def literal[X](e: ColumnLiteral[X]): Y[GlobalScope, X] = e.value match {
				case _ :Boolean => bool(e.asInstanceOf[ColumnLiteral[Boolean]]).asInstanceOf[Y[GlobalScope, X]]
				case _ => nonbool(e)
			}
		}

		type CaseColumnLiteral[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnLiteralMatcher[F, Y]
	}





	class SQLParameter[T](val value :T, val name :Option[String] = None)(implicit override val form :SQLForm[T])
		extends SQLTerm[T]
	{
		override def readForm :SQLReadForm[T] = form
		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const[T](value)
		override def groundValue :Option[T] = Some(value)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher: ExpressionMatcher[RowProduct, Y]): Y[GlobalScope, T] =
			matcher.param(this)


		override def sameAs(that :Any) :Boolean = that.isInstanceOf[SQLParameter[_]]

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



	object SQLParameter extends FormFunction[Self, SQLParameter, SQLParameterColumn] {

		def apply[T](form :SQLForm[T])(value :T) :SQLParameter[T] = new SQLParameter(value)(form)

		def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[(T, Option[String])] = e match {
			case param :SQLParameter[T @unchecked] => Got((param.value, param.name))
			case _ => Lack
		}


//		/** Provider of implicit converters `ParameterFactory[X]` from any Scala literal `X`
//		  * with an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]/[[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
//		  * to an [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter SQLParameter]]`[X]`/[[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameterColumn SQLParameterColumn]]`[X]`
//		  */

		protected override def singleColumn[X :ColumnForm](value :X) :SQLParameterColumn[X] = SQLParameterColumn(value)
		protected override def multiColumn[X :SQLForm](value :X) :SQLParameter[X] = SQLParameter(value)

		/** A factory lifting any literal `X` into an [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter SQLParameter]]`[X]`
		  * or its subclass [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameterColumn SQLParameterColumn]]`[X]`,
		  * depending on whether an implicit [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
		  * or [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists.
		  */
		type ParameterFactory[X, P <: SQLParameter[X]] = SpecificFactory[X, X] { type Res = P }


		trait ParameterMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ParameterColumnMatcher[F, Y] {
			def param[X](e :SQLParameter[X]) :Y[GlobalScope, X]
		}

		type MatchParameter[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseParameter[F, Y]

		trait CaseParameter[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ParameterMatcher[F, Y] with CaseParameterColumn[F, Y]
		{
			override def param[X](e :SQLParameterColumn[X]) :Y[GlobalScope, X] = param(e :SQLParameter[X])
		}

	}






	class SQLParameterColumn[T](param :T, name :Option[String] = None)(implicit override val form :ColumnForm[T])
		extends SQLParameter[T](param, name) with ColumnTerm[T]
	{
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[RowProduct, Y]) :Y[GlobalScope, T] =
			matcher.param(this)
	}



	object SQLParameterColumn {
		def apply[T :ColumnForm](param :T, name :Option[String] = None) :SQLParameterColumn[T] =
			new SQLParameterColumn[T](param, name)

		def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Opt[(T, Option[String])] = e match {
			case param :SQLParameterColumn[T] => Got((param.value, param.name))
			case _ => Lack
		}


		trait ParameterColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def param[X](e :SQLParameterColumn[X]) :Y[GlobalScope, X]
		}

		type MatchParameterColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ParameterColumnMatcher[F, Y]

		type CaseParameterColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ParameterColumnMatcher[F, Y]

	}






	class CompositeNull[T] private[SQLTerm](implicit override val form :SQLForm[T]) extends SQLTerm[T] {
		override def writeForm :SQLWriteForm[Unit] = SQLWriteForm.none[T]

		override def groundValue :Option[T] = form.nulls.toOption

		override def isNull :GlobalBoolean[RowProduct] = True

		//consider: overriding and simplifying comparisons

//		override def to[X](implicit lift :Lift[T, X]) :ColumnSQL[RowProduct, X] =
//		    SQLNull[X](ColumnForm[T].nullBimap(lift.apply)(lift.lower))

		override def opt: GlobalSQL[RowProduct, Option[T]] = CompositeNull[Option[T]]


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[RowProduct, Y]) :Y[GlobalScope, T] =
			matcher.sqlNull(this)


		override def sameAs(that :Any) :Boolean = that.isInstanceOf[CompositeNull[_]]

		override def toString :String =
			groundValue.filter(_ != null).map(v => form.literal(v) + ":SQLNull") getOrElse form.nullLiteral
	}



	object CompositeNull {
		def apply[T :SQLForm] = new CompositeNull[T]

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case null => true
			case _ :CompositeNull[_] => true
			case SQLLiteral(null) => true
			case _ => false
		}


		implicit def nullComposite[T :SQLForm](self :CompositeNull.type) :CompositeNull[T] = CompositeNull[T]


		trait CompositeNullMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends NullMatcher[F, Y] {
			def sqlNull[X](e :CompositeNull[X]) :Y[GlobalScope, X]
		}

		type MatchCompositeNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseCompositeNull[F, Y]

		trait CaseCompositeNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends CompositeNullMatcher[F, Y] {
			override def sqlNull[X](e :SQLNull[X]) :Y[GlobalScope, X] = sqlNull(e :CompositeNull[X])
		}

	}






	class SQLNull[T] private[SQLTerm](implicit override val form :ColumnForm[T])
		extends CompositeNull[T] with ColumnTerm[T]
	{
		override def writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.none[T]


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

		override def opt: ColumnSQL[RowProduct, GlobalScope, Option[T]] = SQLNull[Option[T]]

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[RowProduct, Y]) :Y[GlobalScope, T] =
			matcher.sqlNull(this)


		override def toString :String =
			groundValue.filter(_ != null).map(_.toString + ":SQLNull") getOrElse "SQLNull"
	}



	object SQLNull extends FormFunction[Const[Unit]#T, CompositeNull, SQLNull] {

		def apply[T](implicit factory :Factory[T]) :factory.Res = SQLNull[(), T](())

		def apply[T](form :ColumnForm[T]) :SQLNull[T] = new SQLNull[T]()(form)

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case null => true
			case _ :SQLNull[_] => true
			case SQLLiteral(null) => true
			case _ => false
		}


		protected override def multiColumn[T:SQLForm](arg :Unit) :CompositeNull[T] = new CompositeNull[T]
		protected override def singleColumn[T:ColumnForm](arg :Unit) :SQLNull[T] = new SQLNull[T]


		trait NullMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def sqlNull[X](e :SQLNull[X]) :Y[GlobalScope, X]
		}

		type MatchNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NullMatcher[F, Y]

		type CaseNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NullMatcher[F, Y]

	}


	implicit def nullColumn[T :ColumnForm](self :SQLNull.type) :SQLNull[T] = SQLNull[T]
	implicit def nullComposite[T :SQLForm](self :SQLNull.type) :CompositeNull[T] = CompositeNull[T]






	object True extends ColumnLiteral[Boolean](true) {
		/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
		def apply[F <: RowProduct] :GlobalBoolean[F] = this

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => v
			case _ => false
		}



		override def &&[E <: RowProduct, O >: LocalScope <: GlobalScope]
		               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean)
				:ColumnSQL[E, O, Boolean] =
			other

		override def ||[E <: RowProduct, O >: LocalScope <: GlobalScope]
		               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean)
				:ColumnSQL[E, O, Boolean] =
			this

		override def toString = "True"
	}



	object False extends ColumnLiteral[Boolean](false) {
		/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
		def apply[F <: RowProduct]() :GlobalBoolean[F] = this

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => !v
			case _ => false
		}


		override def &&[E <: RowProduct, O >: LocalScope <: GlobalScope]
		               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] =
			this

		override def ||[E <: RowProduct, O >: LocalScope <: GlobalScope]
		               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] =
			other

		override def toString = "False"
	}






	class NativeTerm[T](val sql :String)(implicit override val form :SQLForm[T]) extends SQLTerm[T] {
		override def readForm :SQLReadForm[T] = SQLForm[T]

		override def writeForm :SQLWriteForm[Unit] = new EmptyWriteForm[Unit] {
			override def literal(value :Unit) = NativeTerm.this.sql
			override def inlineLiteral(value :Unit) = NativeTerm.this.sql
			override def nullLiteral = SQLForm[T].nullLiteral
			override def inlineNullLiteral = SQLForm[T].inlineNullLiteral
			override def toString = NativeTerm.this.sql
		}

		override def groundValue :Option[T] = None

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher: ExpressionMatcher[RowProduct, Y]): Y[GlobalScope, T] =
			matcher.native(this)


		override def isomorphic(expression :SQLExpression.*) :Boolean = expression match {
			case self :AnyRef if self eq this => true
			case native :NativeTerm[_] if sameAs(native) && (native sameAs this) => sql == native.sql
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case native :NativeTerm[_] if native canEqual this => sql == native.sql && form == native.form
			case _ => false
		}

		override def hashCode :Int = sql.hashCode * 31 + form.hashCode


		override def toString :String = sql
	}



	object NativeTerm extends FormFunction[Const[String]#T, NativeTerm, NativeColumnTerm] {

		def unapply(e :SQLExpression[Nothing, LocalScope, _]) :Opt[String] = e match {
			case native :NativeTerm[_] => Got(native.sql)
			case _ => Lack
		}


		protected override def multiColumn[T :SQLForm](arg :String) :NativeTerm[T] =
			new NativeTerm[T](arg)

		protected override def singleColumn[T :ColumnForm](arg :String) :NativeColumnTerm[T] =
			new NativeColumnTerm(arg)


		trait NativeMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends NativeColumnTermMatcher[F, Y] {
			def native[X](e :NativeTerm[X]) :Y[GlobalScope, X]
		}

		type MatchNative[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseNative[F, Y]

		trait CaseNative[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends NativeMatcher[F, Y] {
			override def native[X](e :NativeColumnTerm[X]) :Y[GlobalScope, X] = native(e :NativeTerm[X])
		}

	}






	class NativeColumnTerm[T](sql :String)(implicit override val form :ColumnForm[T])
		extends NativeTerm[T](sql) with ColumnTerm[T]
	{
		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[RowProduct, Y]) :Y[GlobalScope, T] =
			matcher.native(this)
	}



	object NativeColumnTerm {
		def apply[T :ColumnForm](sql :String) :NativeColumnTerm[T] = new NativeColumnTerm[T](sql)

		def unapply(e :SQLExpression[Nothing, LocalScope, _]) :Opt[String] = e match {
			case native :NativeColumnTerm[_] => Got(native.sql)
			case _ => Lack
		}



		trait NativeColumnTermMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def native[X](e :NativeColumnTerm[X]) :Y[GlobalScope, X]
		}

		type MatchNativeColumnTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NativeColumnTermMatcher[F, Y]

		type CaseNativeColumnTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NativeColumnTermMatcher[F, Y]

	}






	trait TermMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnTermMatcher[F, Y]
		with LiteralMatcher[F, Y]  with CompositeNullMatcher[F, Y] with ParameterMatcher[F, Y] with NativeMatcher[F, Y]
	{
		def term[X](e :SQLTerm[X]) :Y[GlobalScope, X]
	}

	trait MatchTerm[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends TermMatcher[F, Y]
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


package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.EmptyWriteForm
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.SQLTerm.SQLParameterColumn.{CaseParameterColumn, ParameterColumnMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLParameter.{CaseParameter, ParameterMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnLiteral.{CaseColumnLiteral, ColumnLiteralMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.ColumnTermMatcher
import net.noresttherein.oldsql.sql.SQLTerm.CompositeNULL.{CaseCompositeNull, CompositeNullMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NativeColumnTerm.{CaseNativeColumnTerm, NativeColumnTermMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral.{CaseLiteral, LiteralMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NativeTerm.{CaseNative, NativeMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NULL.{CaseNull, NullMatcher}




trait SQLTerm[T] extends SQLExpression[FromClause, GlobalScope, T] {
	protected def form :SQLForm[T]

	override def readForm :SQLReadForm[T] = form

	def writeForm :SQLWriteForm[Unit]

	override def isGlobal = true
	override def asGlobal :Option[GlobalSQL[FromClause, T]] = Some(this)

	override def basedOn[U <: FromClause, E <: FromClause](base :E)(implicit ext :U PartOf E) :SQLTerm[T] = this

	override def extend[U <: FromClause, E <: FromClause]
	                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :SQLTerm[T] =
		this



	def sameAs(that :Any) :Boolean = canEqual(that)

	override def isomorphic(expression: SQLExpression.*): Boolean = expression match { //this == expression
		case self :AnyRef if self eq this => true
		case term :SQLTerm[_] if term sameAs this => term.freeValue == freeValue
		case _ => false
	}



	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case term :SQLTerm[_] if term.canEqual(this) =>
			term.freeValue == freeValue && term.form == form
		case _ => false
	}



	override def hashCode :Int = freeValue.hashCode * 31 + form.hashCode

}






object SQLTerm {

	trait ColumnTerm[T] extends SQLTerm[T] with ColumnSQL[FromClause, GlobalScope, T] {
		override def form :ColumnForm[T]

		override def readForm :ColumnReadForm[T] = form

		override def asGlobal :Option[ColumnSQL[FromClause, GlobalScope, T]] = Some(this)

		override def basedOn[U <: FromClause, E <: FromClause](base :E)(implicit ext :U PartOf E) :ColumnTerm[T] =
			this

		override def extend[U <: FromClause, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :ColumnTerm[T] =
			this
	}



	object ColumnTerm {

		trait ColumnTermMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ColumnLiteralMatcher[F, Y] with NullMatcher[F, Y] with ParameterColumnMatcher[F, Y]
			   with NativeColumnTermMatcher[F, Y]

		trait MatchColumnTerm[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnTermMatcher[F, Y]
			with CaseColumnLiteral[F, Y] with CaseNull[F, Y] with CaseParameterColumn[F, Y]
			with CaseNativeColumnTerm[F, Y]

		trait CaseColumnTerm[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchColumnTerm[F, Y] {
			def term[X](e :ColumnTerm[X]) :Y[GlobalScope, X]
			override def literal[X](e :ColumnLiteral[X]) :Y[GlobalScope, X] = term(e)
			override def param[X](e :SQLParameterColumn[X]) :Y[GlobalScope, X] = term(e)
			override def nullSQL[X](e :NULL[X]) :Y[GlobalScope, X] = term(e)
			override def native[X](e :NativeColumnTerm[X]) :Y[GlobalScope, X] = term(e)
		}
	}






	class SQLLiteral[T](val value :T)(implicit override val form :SQLForm[T]) extends SQLTerm[T] {

//		override def to[X](implicit lift :Lift[T, X]) :SQLExpression[FromClause, X] =
//			new SQLLiteral[X](lift(value))(form.nullBimap(lift.apply)(lift.lower))

		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const(value)

		override def freeValue :Option[T] = Some(value)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher: ExpressionMatcher[FromClause, Y]): Y[GlobalScope, T] =
			matcher.literal(this)

		override def sameAs(that :Any) :Boolean = that.isInstanceOf[SQLLiteral[_]]

		override def toString :String = String.valueOf(value)

	}



	object SQLLiteral {
		def apply[T :SQLForm](value :T) :SQLLiteral[T] = new SQLLiteral(value)

		def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Option[T] = e match {
			case literal :SQLLiteral[T @unchecked] => Some(literal.value)
			case _ => None
		}



		trait LiteralMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnLiteralMatcher[F, Y] {
			def literal[X](e :SQLLiteral[X]) :Y[GlobalScope, X]
		}

		type MatchLiteral[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseLiteral[F, Y]

		trait CaseLiteral[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends LiteralMatcher[F, Y] with CaseColumnLiteral[F, Y]
		{
			override def literal[X](f :ColumnLiteral[X]) :Y[GlobalScope, X] = literal(f :SQLLiteral[X])
		}

	}






	class ColumnLiteral[T](literal :T)(implicit override val form :ColumnForm[T])
		extends SQLLiteral[T](literal) with ColumnTerm[T]
	{
		override def &&[E <: FromClause, O >: LocalScope <: GlobalScope]
		               (other: ColumnSQL[E, O, Boolean])(implicit ev :T =:= Boolean) :ColumnSQL[E, O, Boolean] =
			if (value.asInstanceOf[Boolean]) other
			else cast[Boolean]

		override def ||[E <: FromClause, O >: LocalScope <: GlobalScope]
		               (other: ColumnSQL[E, O, Boolean])(implicit ev :T =:= Boolean) :ColumnSQL[E, O, Boolean] =
			if (value.asInstanceOf[Boolean]) cast[Boolean]
			else other

		override def unary_![E <: FromClause, O >: LocalScope <: GlobalScope]
		                    (implicit ev :T =:= Boolean) :ColumnSQL[E, O, Boolean] =
			if (value.asInstanceOf[Boolean]) False else True


		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

//		override def to[X](implicit lift :Lift[T, X]) :ColumnSQL[FromClause, X] =
//			new ColumnLiteral[X](lift(value))(form.nullBimap(lift.apply)(lift.lower))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[FromClause, Y]) :Y[GlobalScope, T] =
			matcher.literal(this)

	}



	object ColumnLiteral {
		def apply[T :ColumnForm](literal :T) :ColumnLiteral[T] = new ColumnLiteral[T](literal)

		def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Option[T] = e match {
			case literal :ColumnLiteral[T @unchecked] => Some(literal.value)
			case _ => None
		}


		trait ColumnLiteralMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def literal[X](f :ColumnLiteral[X]) :Y[GlobalScope, X]
		}

		trait MatchColumnLiteral[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnLiteralMatcher[F, Y] {
			def nonbool[X](e :ColumnLiteral[X]) :Y[GlobalScope, X]

			def bool(e :ColumnLiteral[Boolean]) :Y[GlobalScope, Boolean]

			override def literal[X](e: ColumnLiteral[X]): Y[GlobalScope, X] = e.value match {
				case _ :Boolean => bool(e.asInstanceOf[ColumnLiteral[Boolean]]).asInstanceOf[Y[GlobalScope, X]]
				case _ => nonbool(e)
			}
		}

		type CaseColumnLiteral[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnLiteralMatcher[F, Y]
	}





	class SQLParameter[T](val value :T, val name :Option[String] = None)(implicit override val form :SQLForm[T])
		extends SQLTerm[T]
	{
		override def readForm :SQLReadForm[T] = form
		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const[T](value)

		override def freeValue :Option[T] = Some(value)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher: ExpressionMatcher[FromClause, Y]): Y[GlobalScope, T] =
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



	object SQLParameter {
		def apply[T :SQLForm](value :T) :SQLParameter[T] = new SQLParameter(value)

		def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Option[(T, Option[String])] = e match {
			case param :SQLParameter[T @unchecked] => Some((param.value, param.name))
			case _ => None
		}


		trait ParameterMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ParameterColumnMatcher[F, Y] {
			def param[X](e :SQLParameter[X]) :Y[GlobalScope, X]
		}

		type MatchParameter[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseParameter[F, Y]

		trait CaseParameter[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ParameterMatcher[F, Y] with CaseParameterColumn[F, Y]
		{
			override def param[X](e :SQLParameterColumn[X]) :Y[GlobalScope, X] = param(e :SQLParameter[X])
		}

	}






	class SQLParameterColumn[T](param :T, name :Option[String] = None)(implicit override val form :ColumnForm[T])
		extends SQLParameter[T](param, name) with ColumnTerm[T]
	{
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[FromClause, Y]) :Y[GlobalScope, T] =
			matcher.param(this)
	}



	object SQLParameterColumn {
		def apply[T :ColumnForm](param :T, name :Option[String] = None) :SQLParameterColumn[T] =
			new SQLParameterColumn[T](param, name)

		def unapply[T](e :SQLExpression[Nothing, LocalScope, T]) :Option[(T, Option[String])] = e match {
			case param :SQLParameterColumn[T] => Some((param.value, param.name))
			case _ => None
		}


		trait ParameterColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def param[X](e :SQLParameterColumn[X]) :Y[GlobalScope, X]
		}

		type MatchParameterColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ParameterColumnMatcher[F, Y]

		type CaseParameterColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ParameterColumnMatcher[F, Y]

	}






	class CompositeNULL[T] private[SQLTerm] (implicit override val form :SQLForm[T]) extends SQLTerm[T] {

		override def writeForm :SQLWriteForm[Unit] = SQLWriteForm.none[T]

		override def freeValue :Option[T] = readForm.nulls.toOption

//		override def to[X](implicit lift :Lift[T, X]) :ColumnSQL[FromClause, X] =
//		    NULL[X](ColumnForm[T].nullBimap(lift.apply)(lift.lower))

		override def opt: GlobalSQL[FromClause, Option[T]] = CompositeNULL[Option[T]]


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[FromClause, Y]) :Y[GlobalScope, T] =
			matcher.nullSQL(this)


		override def sameAs(that :Any) :Boolean = that.isInstanceOf[CompositeNULL[_]]

		override def toString :String =
			freeValue.filter(_ != null).map(v => form.literal(v) + ":NULL") getOrElse form.nullLiteral
	}



	object CompositeNULL {
		def apply[T :SQLForm] = new CompositeNULL[T]

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case null => true
			case _ :CompositeNULL[_] => true
			case SQLLiteral(null) => true
			case _ => false
		}


		implicit def nullComposite[T :SQLForm](self :CompositeNULL.type) :CompositeNULL[T] = CompositeNULL[T]


		trait CompositeNullMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends NullMatcher[F, Y] {
			def nullSQL[X](e :CompositeNULL[X]) :Y[GlobalScope, X]
		}

		type MatchCompositeNull[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseCompositeNull[F, Y]

		trait CaseCompositeNull[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends CompositeNullMatcher[F, Y] {
			override def nullSQL[X](e :NULL[X]) :Y[GlobalScope, X] = nullSQL(e :CompositeNULL[X])
		}

	}






	class NULL[T] private[SQLTerm] (implicit override val form :ColumnForm[T])
		extends CompositeNULL[T] with ColumnTerm[T]
	{
		override def writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.none[T]

		override def opt: ColumnSQL[FromClause, GlobalScope, Option[T]] = NULL[Option[T]]

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[FromClause, Y]) :Y[GlobalScope, T] =
			matcher.nullSQL(this)


		override def toString :String =
			freeValue.filter(_ != null).map(_.toString + ":NULL") getOrElse "NULL"
	}



//	def NULL[T :ColumnForm] = new NULL[T]

	object NULL {
		def apply[T :ColumnForm] = new NULL[T]

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case null => true
			case _ :NULL[_] => true
			case SQLLiteral(null) => true
			case _ => false
		}


		implicit def nullColumn[T :ColumnForm](self :NULL.type) :NULL[T] = NULL[T]

		implicit def nullComposite[T :SQLForm](self :NULL.type) :CompositeNULL[T] = CompositeNULL[T]



		trait NullMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def nullSQL[X](e :NULL[X]) :Y[GlobalScope, X]
		}

		type MatchNull[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = NullMatcher[F, Y]

		type CaseNull[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = NullMatcher[F, Y]

	}






	object True extends ColumnLiteral[Boolean](true) {
		/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
		def apply[F <: FromClause]() :GlobalBoolean[F] = this

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => v
			case _ => false
		}



		override def &&[E <: FromClause, O >: LocalScope <: GlobalScope]
		               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean)
				:ColumnSQL[E, O, Boolean] =
			other

		override def ||[E <: FromClause, O >: LocalScope <: GlobalScope]
		               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean)
				:ColumnSQL[E, O, Boolean] =
			this

		override def toString = "True"
	}



	object False extends ColumnLiteral[Boolean](false) {
		/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
		def apply[F <: FromClause]() :GlobalBoolean[F] = this

		def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => !v
			case _ => false
		}


		override def &&[E <: FromClause, O >: LocalScope <: GlobalScope]
		               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] =
			this

		override def ||[E <: FromClause, O >: LocalScope <: GlobalScope]
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

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher: ExpressionMatcher[FromClause, Y]): Y[GlobalScope, T] =
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



	object NativeTerm {

		def apply[T :SQLForm](sql :String) :NativeTerm[T] = new NativeTerm[T](sql)

		def unapply(e :SQLExpression[Nothing, LocalScope, _]) :Option[String] = e match {
			case native :NativeTerm[_] => Some(native.sql)
			case _ => None
		}



		trait NativeMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends NativeColumnTermMatcher[F, Y] {
			def native[X](e :NativeTerm[X]) :Y[GlobalScope, X]
		}

		type MatchNative[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = CaseNative[F, Y]

		trait CaseNative[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends NativeMatcher[F, Y] {
			override def native[X](e :NativeColumnTerm[X]) :Y[GlobalScope, X] = native(e :NativeTerm[X])
		}

	}






	class NativeColumnTerm[T](sql :String)(implicit override val form :ColumnForm[T])
		extends NativeTerm[T](sql) with ColumnTerm[T]
	{
		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[FromClause, Y]) :Y[GlobalScope, T] =
			matcher.native(this)
	}



	object NativeColumnTerm {
		def apply[T :ColumnForm](sql :String) :NativeColumnTerm[T] = new NativeColumnTerm[T](sql)

		def unapply(e :SQLExpression[Nothing, LocalScope, _]) :Option[String] = e match {
			case native :NativeColumnTerm[_] => Some(native.sql)
			case _ => None
		}



		trait NativeColumnTermMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def native[X](e :NativeColumnTerm[X]) :Y[GlobalScope, X]
		}

		type MatchNativeColumnTerm[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = NativeColumnTermMatcher[F, Y]

		type CaseNativeColumnTerm[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = NativeColumnTermMatcher[F, Y]

	}






	trait SQLTermFactory[X, +T <: SQLTerm[X]] {
		def apply(value :X) :T
	}

	object SQLTermFactory {
		implicit def nullFactory[T:SQLForm] :SQLTermFactory[T, CompositeNULL[T]] =
			{ _ :T => CompositeNULL[T] }

		implicit def nullColumnFactory[T:ColumnForm] :SQLTermFactory[T, NULL[T]] =
			{ _ :T => NULL[T] }


		implicit def boundParameterFactory[T:SQLForm] :SQLTermFactory[T, SQLParameter[T]] =
			{ value :T => SQLParameter(value) }

		implicit def boundParameterColumnFactory[T:ColumnForm] :SQLTermFactory[T, SQLParameterColumn[T]] =
			{ value :T => SQLParameterColumn(value) }
	}



	trait TermMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnTermMatcher[F, Y]
		with LiteralMatcher[F, Y]  with CompositeNullMatcher[F, Y] with ParameterMatcher[F, Y] with NativeMatcher[F, Y]

	trait MatchTerm[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends CaseLiteral[F, Y]  with CaseCompositeNull[F, Y] with CaseParameter[F, Y] with CaseNative[F, Y]

	trait CaseTerm[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TermMatcher[F, Y] with MatchTerm[F, Y]
	{
		def term[X](e :SQLTerm[X]) :Y[GlobalScope, X]

		def param[X](e: SQLParameter[X]): Y[GlobalScope, X] = term(e)

		def literal[X](e: SQLLiteral[X]): Y[GlobalScope, X] = term(e)

		def nullSQL[X](e: CompositeNULL[X]): Y[GlobalScope, X] = term(e)

		def native[X](e: NativeTerm[X]): Y[GlobalScope, X] = term(e)

	}

}


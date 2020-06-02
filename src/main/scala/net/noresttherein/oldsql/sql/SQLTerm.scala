package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.EmptyWriteForm
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.SQLExpression.ExpressionMatcher
import net.noresttherein.oldsql.sql.SQLTerm.SQLParameterColumn.{CaseParameterColumn, ParameterColumnMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLParameter.{CaseParameter, ParameterMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnLiteral.{CaseColumnLiteral, ColumnLiteralMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.ColumnTermMatcher
import net.noresttherein.oldsql.sql.SQLTerm.CompositeNULL.{CaseCompositeNull, CompositeNullMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NativeColumnTerm.{CaseNativeColumnTerm, NativeColumnTermMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral.{CaseLiteral, LiteralMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NativeTerm.{CaseNative, NativeMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NULL.{CaseNull, NullMatcher}




trait SQLTerm[T] extends SQLExpression[FromClause, T] {
	protected def form :SQLForm[T]

	override def readForm :SQLReadForm[T] = form
	def writeForm :SQLWriteForm[Unit]

	override def stretch[U <: FromClause, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :SQLTerm[T] = this



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

	trait ColumnTerm[T] extends SQLTerm[T] with ColumnSQL[FromClause, T] {
		override def form :ColumnForm[T]

		override def readForm :ColumnReadForm[T] = form

		override def stretch[U <: FromClause, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :ColumnTerm[T] =
			this

	}



	object ColumnTerm {

		trait ColumnTermMatcher[+F <: FromClause, +Y[X]]
			extends ColumnLiteralMatcher[F, Y] with NullMatcher[F, Y] with ParameterColumnMatcher[F, Y]
			   with NativeColumnTermMatcher[F, Y]

		trait MatchColumnTerm[+F <: FromClause, +Y[X]] extends ColumnTermMatcher[F, Y]
			with CaseColumnLiteral[F, Y] with CaseNull[F, Y] with CaseParameterColumn[F, Y]
			with CaseNativeColumnTerm[F, Y]

		trait CaseColumnTerm[+F <: FromClause, +Y[X]] extends MatchColumnTerm[F, Y] {
			def term[X](e :ColumnTerm[X]) :Y[X]
			override def literal[X](e :ColumnLiteral[X]) :Y[X] = term(e)
			override def param[X](e :SQLParameterColumn[X]) :Y[X] = term(e)
			override def sqlNull[X](e :NULL[X]) :Y[X] = term(e)
			override def native[X](e :NativeColumnTerm[X]) :Y[X] = term(e)
		}
	}






	class SQLLiteral[T](val value :T)(implicit override val form :SQLForm[T]) extends SQLTerm[T] {

//		override def to[X](implicit lift :Lift[T, X]) :SQLExpression[FromClause, X] =
//			new SQLLiteral[X](lift(value))(form.bimapNull(lift.apply)(lift.lower))

		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const(value)

		override def freeValue = Some(value)


		override def applyTo[Y[_]](matcher: ExpressionMatcher[FromClause, Y]): Y[T] = matcher.literal(this)


		override def toString :String = String.valueOf(value)

	}



	object SQLLiteral {
		def apply[T :SQLForm](value :T) :SQLLiteral[T] = new SQLLiteral(value)

		def unapply[T](e :SQLExpression[Nothing, T]) :Option[T] = e match {
			case literal :SQLLiteral[T @unchecked] => Some(literal.value)
			case _ => None
		}



		trait LiteralMatcher[+F <: FromClause, +Y[X]] extends ColumnLiteralMatcher[F, Y] {
			def literal[X](e :SQLLiteral[X]) :Y[X]
		}

		type MatchLiteral[+F <: FromClause, +Y[X]] = CaseLiteral[F, Y]

		trait CaseLiteral[+F <: FromClause, +Y[X]] extends LiteralMatcher[F, Y] with CaseColumnLiteral[F, Y] {
			override def literal[X](f :ColumnLiteral[X]) :Y[X] = literal(f :SQLLiteral[X])
		}

	}






	class ColumnLiteral[T](literal :T)(implicit override val form :ColumnForm[T])
		extends SQLLiteral[T](literal) with ColumnTerm[T]
	{
		override def &&[S <: FromClause](other: SQLBoolean[S])
		                                (implicit ev: this.type <:< SQLBoolean[S]): SQLBoolean[S] =
			if (value.asInstanceOf[Boolean]) other
			else ev(this)

		override def ||[S <: FromClause](other: SQLBoolean[S])
		                                (implicit ev: this.type <:< SQLBoolean[S]): SQLBoolean[S] =
			if (value.asInstanceOf[Boolean]) ev(this)
			else other

		override def unary_![S <: FromClause](implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
			if (value.asInstanceOf[Boolean]) False else True


		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

//		override def to[X](implicit lift :Lift[T, X]) :ColumnSQL[FromClause, X] =
//			new ColumnLiteral[X](lift(value))(form.bimapNull(lift.apply)(lift.lower))

		override def applyTo[Y[_]](matcher :ColumnMatcher[FromClause, Y]) :Y[T] = matcher.literal(this)

	}



	object ColumnLiteral {
		def apply[T :ColumnForm](literal :T) :ColumnLiteral[T] = new ColumnLiteral[T](literal)

		def unapply[T](e :SQLExpression[Nothing, T]) :Option[T] = e match {
			case literal :ColumnLiteral[T @unchecked] => Some(literal.value)
			case _ => None
		}


		trait ColumnLiteralMatcher[+F <: FromClause, +Y[X]] {
			def literal[X](f :ColumnLiteral[X]) :Y[X]
		}

		trait MatchColumnLiteral[+F <: FromClause, +Y[X]] extends ColumnLiteralMatcher[F, Y] {
			def nonbool[X](e :ColumnLiteral[X]) :Y[X]

			def bool(e :ColumnLiteral[Boolean]) :Y[Boolean]

			override def literal[X](e: ColumnLiteral[X]): Y[X] = e.value match {
				case _ :Boolean => bool(e.asInstanceOf[ColumnLiteral[Boolean]]).asInstanceOf[Y[X]]
				case _ => nonbool(e)
			}
		}

		type CaseColumnLiteral[+F <: FromClause, +Y[X]] = ColumnLiteralMatcher[F, Y]
	}





	class SQLParameter[T](val value :T, val name :Option[String] = None)(implicit override val form :SQLForm[T])
		extends SQLTerm[T]
	{
		override def readForm :SQLReadForm[T] = form
		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const[T](value)

		override def freeValue = Some(value)


		override def applyTo[Y[_]](matcher: ExpressionMatcher[FromClause, Y]): Y[T] = matcher.param(this)


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

		def unapply[T](e :SQLExpression[Nothing, T]) :Option[(T, Option[String])] = e match {
			case param :SQLParameter[T @unchecked] => Some((param.value, param.name))
			case _ => None
		}

		trait ParameterMatcher[+F <: FromClause, +Y[X]] extends ParameterColumnMatcher[F, Y] {
			def param[X](e :SQLParameter[X]) :Y[X]
		}

		type MatchParameter[+F <: FromClause, +Y[X]] = CaseParameter[F, Y]

		trait CaseParameter[+F <: FromClause, +Y[X]]
			extends ParameterMatcher[F, Y] with CaseParameterColumn[F, Y]
		{
			override def param[X](e :SQLParameterColumn[X]) :Y[X] = param(e :SQLParameter[X])
		}

	}






	class SQLParameterColumn[T](param :T, name :Option[String] = None)(implicit override val form :ColumnForm[T])
		extends SQLParameter[T](param, name) with ColumnTerm[T]
	{
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

		override def applyTo[Y[_]](matcher :ColumnMatcher[FromClause, Y]) :Y[T] = matcher.param(this)
	}



	object SQLParameterColumn {
		def apply[T :ColumnForm](param :T, name :Option[String] = None) :SQLParameterColumn[T] =
			new SQLParameterColumn[T](param, name)

		def unapply[T](e :SQLExpression[Nothing, T]) :Option[(T, Option[String])] = e match {
			case param :SQLParameterColumn[T] => Some((param.value, param.name))
			case _ => None
		}


		trait ParameterColumnMatcher[+F <: FromClause, +Y[X]] {
			def param[X](e :SQLParameterColumn[X]) :Y[X]
		}

		type MatchParameterColumn[+F <: FromClause, +Y[X]] = ParameterColumnMatcher[F, Y]

		type CaseParameterColumn[+F <: FromClause, +Y[X]] = ParameterColumnMatcher[F, Y]

	}






	class CompositeNULL[T] private[SQLTerm](implicit override val form :SQLForm[T]) extends SQLTerm[T] {

		override def writeForm :SQLWriteForm[Unit] = SQLWriteForm.none[T]

		override def freeValue :Option[T] = readForm.nulls.toOption

//		override def to[X](implicit lift :Lift[T, X]) :ColumnSQL[FromClause, X] =
//		    NULL[X](ColumnForm[T].bimapNull(lift.apply)(lift.lower))

		override def opt: SQLExpression[FromClause, Option[T]] = CompositeNULL[Option[T]]


		override def applyTo[Y[_]](matcher :ExpressionMatcher[FromClause, Y]) :Y[T] = matcher.sqlNull(this)


		override def toString :String =
			freeValue.filter(_ != null).map(v => form.literal(v) + ":NULL") getOrElse form.nullLiteral
	}



	object CompositeNULL {
		def apply[T :SQLForm] = new CompositeNULL[T]

		def unapply(expression :SQLExpression[_, _]) :Boolean = expression match {
			case null => true
			case _ :CompositeNULL[_] => true
			case SQLLiteral(null) => true
			case _ => false
		}

		trait CompositeNullMatcher[+F <: FromClause, +Y[X]] extends NullMatcher[F, Y] {
			def sqlNull[X](e :CompositeNULL[X]) :Y[X]
		}

		type MatchCompositeNull[+F <: FromClause, +Y[X]] = CaseCompositeNull[F, Y]

		trait CaseCompositeNull[+F <: FromClause, +Y[X]] extends CompositeNullMatcher[F, Y] {
			override def sqlNull[X](e :NULL[X]) :Y[X] = sqlNull(e :CompositeNULL[X])
		}

	}






	class NULL[T] private[SQLTerm] (implicit override val form :ColumnForm[T])
		extends CompositeNULL[T] with ColumnTerm[T]
	{
		override def writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.none[T]

		override def opt: ColumnSQL[FromClause, Option[T]] = NULL[Option[T]]

		override def applyTo[Y[_]](matcher :ColumnMatcher[FromClause, Y]) :Y[T] = matcher.sqlNull(this)


		override def toString :String =
			freeValue.filter(_ != null).map(_ + ":NULL") getOrElse "NULL"
	}



//	def NULL[T :ColumnForm] = new NULL[T]

	object NULL {
		def apply[T :ColumnForm] = new NULL[T]

		def unapply(expression :SQLExpression[_, _]) :Boolean = expression match {
			case null => true
			case _ :NULL[_] => true
			case SQLLiteral(null) => true
			case _ => false
		}



		trait NullMatcher[+F <: FromClause, +Y[X]] {
			def sqlNull[X](e :NULL[X]) :Y[X]
		}

		type MatchNull[+F <: FromClause, +Y[X]] = NullMatcher[F, Y]

		type CaseNull[+F <: FromClause, +Y[X]] = NullMatcher[F, Y]

	}






	object True extends ColumnLiteral[Boolean](true) {
		def apply[F <: FromClause]() :SQLBoolean[F] = this

		def unapply(expression :SQLExpression[Nothing, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => v
			case _ => false
		}



		override def &&[S <: FromClause](other :SQLBoolean[S])
		                                (implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
			other

		override def ||[S <: FromClause](other :SQLBoolean[S])
		                                (implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
			this

		override def toString = "True"
	}



	object False extends ColumnLiteral[Boolean](false) {
		def apply[F <: FromClause]() :SQLBoolean[F] = this

		def unapply(expression :SQLExpression[Nothing, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => !v
			case _ => false
		}



		override def &&[S <: FromClause](other :SQLBoolean[S])
		                                (implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
			this

		override def ||[S <: FromClause](other :SQLBoolean[S])
		                                (implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
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

		override def applyTo[Y[_]](matcher: ExpressionMatcher[FromClause, Y]): Y[T] =
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

		def unapply(e :SQLExpression[_, _]) :Option[String] = e match {
			case native :NativeTerm[_] => Some(native.sql)
			case _ => None
		}



		trait NativeMatcher[+F <: FromClause, +Y[X]] extends NativeColumnTermMatcher[F, Y] {
			def native[X](e :NativeTerm[X]) :Y[X]
		}

		type MatchNative[+F <: FromClause, +Y[X]] = CaseNative[F, Y]

		trait CaseNative[+F <: FromClause, +Y[X]] extends NativeMatcher[F, Y] {
			override def native[X](e :NativeColumnTerm[X]) :Y[X] = native(e :NativeTerm[X])
		}

	}






	class NativeColumnTerm[T](sql :String)(implicit override val form :ColumnForm[T])
		extends NativeTerm[T](sql) with ColumnTerm[T]
	{
		override def applyTo[Y[_]](matcher :ColumnMatcher[FromClause, Y]) :Y[T] = matcher.native(this)
	}



	object NativeColumnTerm {
		def apply[T :ColumnForm](sql :String) :NativeColumnTerm[T] = new NativeColumnTerm[T](sql)

		def unapply(e :SQLExpression[Nothing, _]) :Option[String] = e match {
			case native :NativeColumnTerm[_] => Some(native.sql)
			case _ => None
		}



		trait NativeColumnTermMatcher[+F <: FromClause, +Y[X]] {
			def native[X](e :NativeColumnTerm[X]) :Y[X]
		}

		type MatchNativeColumnTerm[+F <: FromClause, +Y[X]] = NativeColumnTermMatcher[F, Y]

		type CaseNativeColumnTerm[+F <: FromClause, +Y[X]] = NativeColumnTermMatcher[F, Y]

	}






	trait TermMatcher[+S <: FromClause, +Y[X]] extends ColumnTermMatcher[S, Y]
		with LiteralMatcher[S, Y]  with CompositeNullMatcher[S, Y] with ParameterMatcher[S, Y] with NativeMatcher[S, Y]

	trait MatchTerm[+S <: FromClause, +Y[X]]
		extends CaseLiteral[S, Y]  with CaseCompositeNull[S, Y] with CaseParameter[S, Y] with CaseNative[S, Y]

	trait CaseTerm[+S <: FromClause, +Y[X]] extends TermMatcher[S, Y] with MatchTerm[S, Y] {
		def term[X](e :SQLTerm[X]) :Y[X]

		def param[X](e: SQLParameter[X]): Y[X] = term(e)

		def literal[X](e: SQLLiteral[X]): Y[X] = term(e)

		def sqlNull[X](e: CompositeNULL[X]): Y[X] = term(e)

		def native[X](e: NativeTerm[X]): Y[X] = term(e)

	}

}


package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.MappingFrom
import net.noresttherein.oldsql.schema.SQLWriteForm.EmptyWriteForm
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.ColumnFormulaMatcher
import net.noresttherein.oldsql.sql.SQLFormula.SQLTypePromotion.Lift
import net.noresttherein.oldsql.sql.SQLTerm.BoundParameterColumn.{BoundParameterColumnMatcher, CaseBoundParameterColumn}
import net.noresttherein.oldsql.sql.SQLTerm.BoundParameter
import net.noresttherein.oldsql.sql.SQLTerm.BoundParameter.{BoundParameterMatcher, CaseBoundParameter}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnLiteral.{CaseColumnLiteral, ColumnLiteralMatcher, MatchColumnLiteral}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.ColumnTermMatcher
import net.noresttherein.oldsql.sql.SQLTerm.NativeColumnTerm.{CaseNativeColumnTerm, NativeColumnTermMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral.{CaseLiteral, LiteralMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NativeTerm.{CaseNative, NativeMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NULL.{CaseNull, NullMatcher}




trait SQLTerm[T] extends SQLFormula[FromClause, T] {
	protected def form :SQLForm[T]

	override def readForm :SQLReadForm[T] = form
	def writeForm :SQLWriteForm[Unit]

	override def stretch[U <: FromClause, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :SQLTerm[T] = this

	override def isomorphic(expression: Formula[_]): Boolean = this == expression

	private[oldsql] override def equivalent(expression: Formula[_]): Boolean = this == expression

}






abstract class MultiColumnTerms {

	implicit class TermFormulas[T :SQLForm](term :T) {
		def ? = BoundParameter(term)
	}

}





object SQLTerm extends MultiColumnTerms {

	trait ColumnTerm[T] extends SQLTerm[T] with ColumnFormula[FromClause, T] {
		override def form :ColumnForm[T]

		override def readForm :ColumnReadForm[T] = form

		override def stretch[U <: FromClause, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :ColumnTerm[T] =
			this

	}



	object ColumnTerm {
		trait ColumnTermMatcher[+F <: FromClause, +Y[X]]
			extends ColumnLiteralMatcher[F, Y] with BoundParameterColumnMatcher[F, Y]
			   with NullMatcher[F, Y] with NativeColumnTermMatcher[F, Y]

		trait MatchColumnTerm[+F <: FromClause, +Y[X]] extends ColumnTermMatcher[F, Y]
			with CaseColumnLiteral[F, Y] with CaseBoundParameterColumn[F, Y]
			with CaseNull[F, Y] with CaseNativeColumnTerm[F, Y]

		trait CaseColumnTerm[+F <: FromClause, +Y[X]] extends MatchColumnTerm[F, Y] {
			def term[X](e :ColumnTerm[X]) :Y[X]
			override def literal[X](e :ColumnLiteral[X]) :Y[X] = term(e)
			override def param[X](e :BoundParameterColumn[X]) :Y[X] = term(e)
			override def sqlNull[X](e :NULL[X]) :Y[X] = term(e)
			override def native[X](e :NativeColumnTerm[X]) :Y[X] = term(e)
		}
	}






	class SQLLiteral[T](val value :T)(implicit override val form :SQLForm[T]) extends SQLTerm[T] {

		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const(value)

		override def freeValue = Some(value)

		override def &&[S <: FromClause](other: BooleanFormula[S])
		                                (implicit ev: this.type <:< BooleanFormula[S]): BooleanFormula[S] =
			if (value.asInstanceOf[Boolean]) other
			else ev(this)



		override def ||[S <: FromClause](other: BooleanFormula[S])
		                                (implicit ev: this.type <:< BooleanFormula[S]): BooleanFormula[S] =
			if (value.asInstanceOf[Boolean]) ev(this)
			else other

		override def to[X](implicit lift :Lift[T, X]) :SQLFormula[FromClause, X] =
			new SQLLiteral[X](lift(value))(form.bimapNull(lift.apply)(lift.lower))


		override def applyTo[Y[_]](matcher: FormulaMatcher[FromClause, Y]): Y[T] = matcher.literal(this)



		override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLLiteral[_]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :SQLLiteral[_] if canEqual(other) && other.canEqual(this) =>
				value == other.value && form == other.form
			case _ => false
		}

		override def hashCode :Int = (if (value == null) 0 else value.hashCode) * 31 + form.hashCode


		override def toString :String = String.valueOf(value)

	}



	object SQLLiteral {
		def apply[T :SQLForm](value :T) :SQLLiteral[T] = new SQLLiteral(value)

		def unapply[T](e :SQLFormula[Nothing, T]) :Option[T] = e match {
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
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

		override def to[X](implicit lift :Lift[T, X]) :ColumnFormula[FromClause, X] =
			new ColumnLiteral[X](lift(value))(form.bimapNull(lift.apply)(lift.lower))

		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[FromClause, Y]) :Y[T] = matcher.literal(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnLiteral[_]]

	}



	object ColumnLiteral {
		def apply[T :ColumnForm](literal :T) :ColumnLiteral[T] = new ColumnLiteral[T](literal)

		def unapply[T](e :SQLFormula[Nothing, T]) :Option[T] = e match {
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





	class BoundParameter[T](val value :T, val name :Option[String] = None)(implicit override val form :SQLForm[T])
		extends SQLTerm[T]
	{
		override def readForm :SQLReadForm[T] = form
		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const[T](value)


		override def to[X](implicit lift :Lift[T, X]) :BoundParameter[X] =
			new BoundParameter[X](lift(value))(form.bimapNull(lift.apply)(lift.lower))

		override def freeValue = Some(value)


		override def applyTo[Y[_]](matcher: FormulaMatcher[FromClause, Y]): Y[T] = matcher.param(this)


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[BoundParameter[_]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case p :BoundParameter[_] if canEqual(p) && p.canEqual(this) =>
				value == p.value && form == p.form
			case _ => false
		}

		override def hashCode :Int = (if (value == null) 0 else value.hashCode) * 31 + form.hashCode


		override def toString :String = "?" + value
	}



	object BoundParameter {
		def apply[T :SQLForm](value :T) :BoundParameter[T] = new BoundParameter(value)

		def unapply[T](e :SQLFormula[Nothing, T]) :Option[(T, Option[String])] = e match {
			case param :BoundParameter[T @unchecked] => Some((param.value, param.name))
			case _ => None
		}

		trait BoundParameterMatcher[+F <: FromClause, +Y[X]] extends BoundParameterColumnMatcher[F, Y] {
			def param[X](e :BoundParameter[X]) :Y[X]
		}

		type MatchBoundParameter[+F <: FromClause, +Y[X]] = CaseBoundParameter[F, Y]

		trait CaseBoundParameter[+F <: FromClause, +Y[X]]
			extends BoundParameterMatcher[F, Y] with CaseBoundParameterColumn[F, Y]
		{
			override def param[X](e :BoundParameterColumn[X]) :Y[X] = param(e :BoundParameter[X])
		}

	}



	class BoundParameterColumn[T](param :T, name :Option[String] = None)(implicit override val form :ColumnForm[T])
		extends BoundParameter[T](param, name) with ColumnTerm[T]
	{
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

		override def to[X](implicit lift :Lift[T, X]) :BoundParameterColumn[X] =
			new BoundParameterColumn[X](lift(value))(form.bimapNull(lift.apply)(lift.lower))


		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[FromClause, Y]) :Y[T] = matcher.param(this)
	}



	object BoundParameterColumn {
		def apply[T :ColumnForm](param :T, name :Option[String] = None) :BoundParameterColumn[T] =
			new BoundParameterColumn[T](param, name)

		def unapply[T](e :SQLFormula[Nothing, T]) :Option[(T, Option[String])] = e match {
			case param :BoundParameterColumn[T] => Some((param.value, param.name))
			case _ => None
		}


		trait BoundParameterColumnMatcher[+F <: FromClause, +Y[X]] {
			def param[X](e :BoundParameterColumn[X]) :Y[X]
		}

		type MatchBoundParameterColumn[+F <: FromClause, +Y[X]] = BoundParameterColumnMatcher[F, Y]

		type CaseBoundParameterColumn[+F <: FromClause, +Y[X]] = BoundParameterColumnMatcher[F, Y]

	}





	class NULL[T] private[SQLTerm](implicit override val form :ColumnForm[T]) extends ColumnTerm[T] {

		override def writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.none[T]

		override def freeValue :Option[T] = readForm.nulls.toOption

		override def to[X](implicit lift :Lift[T, X]) :ColumnFormula[FromClause, X] =
		    NULL[X](ColumnForm[T].bimapNull(lift.apply)(lift.lower))

		override def opt: ColumnFormula[FromClause, Option[T]] = NULL[Option[T]]



		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[FromClause, Y]) :Y[T] = matcher.sqlNull(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[NULL[_]]

		override def equals(that :Any) :Boolean = that match {
			case n :NULL[_] => freeValue == n.freeValue
			case _ => false
		}

		override def hashCode :Int = NULL.hashCode

		override def toString = "Null"
	}



	object NULL {
		def apply[T :ColumnForm] = new NULL[T]

		def unapply(expression :SQLFormula[_, _]) :Boolean = expression match {
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
		def apply[F <: FromClause]() :BooleanFormula[F] = this

		def unapply(expression :SQLFormula[Nothing, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => v
			case _ => false
		}



		override def &&[S <: FromClause](other :BooleanFormula[S])
		                                (implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			other

		override def ||[S <: FromClause](other :BooleanFormula[S])
		                                (implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			this

		override def toString = "True"
	}



	object False extends ColumnLiteral[Boolean](false) {
		def apply[F <: FromClause]() :BooleanFormula[F] = this

		def unapply(expression :SQLFormula[Nothing, _]) :Boolean = expression match {
			case SQLLiteral(v :Boolean) => !v
			case _ => false
		}



		override def &&[S <: FromClause](other :BooleanFormula[S])
		                                (implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			this

		override def ||[S <: FromClause](other :BooleanFormula[S])
		                                (implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			other

		override def toString = "False"
	}






	case class NativeTerm[T](sql :String)(implicit override val form :SQLForm[T]) extends SQLTerm[T] {
		override def readForm :SQLReadForm[T] = SQLForm[T]

		override def writeForm :SQLWriteForm[Unit] = new EmptyWriteForm[Unit] {
			override def literal(value :Unit) = NativeTerm.this.sql
			override def inlineLiteral(value :Unit) = NativeTerm.this.sql
			override def nullLiteral = SQLForm[T].nullLiteral
			override def inlineNullLiteral = SQLForm[T].inlineNullLiteral
			override def toString = NativeTerm.this.sql
		}

		override def applyTo[Y[_]](matcher: FormulaMatcher[FromClause, Y]): Y[T] =
			matcher.native(this)

		override def toString :String = sql
	}



	object NativeTerm {
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
		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[FromClause, Y]) :Y[T] = matcher.native(this)
	}



	object NativeColumnTerm {
		def apply[T :ColumnForm](sql :String) :NativeColumnTerm[T] = new NativeColumnTerm[T](sql)

		def unapply(e :SQLFormula[Nothing, _]) :Option[String] = e match {
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
		with LiteralMatcher[S, Y] with BoundParameterMatcher[S, Y] with NullMatcher[S, Y] with NativeMatcher[S, Y]

	trait MatchTerm[+S <: FromClause, +Y[X]]
		extends CaseLiteral[S, Y] with CaseBoundParameter[S, Y] with CaseNull[S, Y] with CaseNative[S, Y]

	trait CaseTerm[+S <: FromClause, +Y[X]] extends TermMatcher[S, Y] with MatchTerm[S, Y] {
		def term[X](e :SQLTerm[X]) :Y[X]

		def param[X](e: BoundParameter[X]): Y[X] = term(e)

		def literal[X](e: SQLLiteral[X]): Y[X] = term(e)

		def sqlNull[X](e: NULL[X]): Y[X] = term(e)

		def native[X](e: NativeTerm[X]): Y[X] = term(e)

	}

}


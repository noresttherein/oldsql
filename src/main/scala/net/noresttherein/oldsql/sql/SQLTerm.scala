package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, Mapping, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.AnyComponent
import net.noresttherein.oldsql.schema.SQLWriteForm.EmptyWriteForm
import net.noresttherein.oldsql.sql.FromClause.FromFormula
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.BoundParameter
import net.noresttherein.oldsql.sql.SQLTerm.BoundParameter.BoundParameterMatcher
import net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral.{CaseLiteral, LiteralMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NativeTerm.{CaseNative, NativeMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLNull.{CaseNull, NullMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLParameter.{CaseParameter, ParameterMatcher}




trait SQLTerm[+T] extends SQLFormula[FromClause, T] {

	def writeForm :SQLWriteForm[Unit]

	//	override def readForm :SQLReadForm[T] = SQLReadForm[T]


	override def isomorphic(expression: Formula[_]): Boolean = this == expression

	private[oldsql] override def equivalent(expression: Formula[_]): Boolean = this == expression

	override def isGroundedIn(tables: Iterable[FromFormula[_, m forSome { type m[O] <: AnyComponent[O] }]]): Boolean = freeValue.isDefined

//	override def get(values :RowValues[FromClause]) :Option[T] = freeValue
}






abstract class MultiColumnTerms {


	implicit class TermFormulas[T :SQLForm](val term :T) {
		def ? = BoundParameter(term)
	}
}





object SQLTerm extends MultiColumnTerms {

	trait ColumnTerm[+T] extends SQLTerm[T] with ColumnFormula[FromClause, T] {
		override def readForm :ColumnReadForm[T]
		override def writeForm :ColumnWriteForm[Unit]
	}






	case class SQLLiteral[+T](value :T)(implicit protected[this] val form :SQLForm[T]) extends SQLTerm[T] {

		override def readForm :SQLReadForm[T] = form
		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const(value)

		override def &&[S <: FromClause](other: BooleanFormula[S])
		                                (implicit ev: this.type <:< BooleanFormula[S]): BooleanFormula[S] =
			if (value.asInstanceOf[Boolean]) other
			else ev(this)

		override def ||[S <: FromClause](other: BooleanFormula[S])
		                                (implicit ev: this.type <:< BooleanFormula[S]): BooleanFormula[S] =
			if (value.asInstanceOf[Boolean]) ev(this)
			else other


		override def opt: SQLFormula[FromClause, Option[T]] = SQLLiteral(Option(value))

		override def freeValue = Some(value)


		override def applyTo[Y[+X]](matcher: FormulaMatcher[FromClause, Y]): Y[T] = matcher.literal(this)


		override def toString :String = String.valueOf(value)

	}



	object SQLLiteral {

		trait LiteralMatcher[+S <: FromClause, +Y[X]] {
			def literal[X](f :SQLLiteral[X]) :Y[X]
		}

		trait MatchLiteral[+S <: FromClause, +Y[X]] extends LiteralMatcher[S, Y] {
			def nonbool[X](f :SQLLiteral[X]) :Y[X]

			def bool(f :SQLLiteral[Boolean]) :Y[Boolean]

			override def literal[X](f: SQLLiteral[X]): Y[X] = f.value match {
				case _ :Boolean => bool(f.asInstanceOf[SQLLiteral[Boolean]]).asInstanceOf[Y[X]]
				case _ => nonbool(f)
			}
		}

		type CaseLiteral[+S <: FromClause, +Y[X]] = LiteralMatcher[S, Y]

	}



	class ColumnLiteral[+T](literal :T)(implicit protected[this] override val form :ColumnForm[T])
		extends SQLLiteral[T](literal) with ColumnTerm[T]
	{
		override def readForm :ColumnReadForm[T] = form
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnLiteral[_]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :ColumnLiteral[_] if other canEqual this => other.value == value && other.readForm == readForm
			case _ => false
		}
	}



	object ColumnLiteral {
		def apply[T :ColumnForm](literal :T) :ColumnLiteral[T] = new ColumnLiteral[T](literal)

		def unapply[T](e :SQLFormula[Nothing, T]) :Option[T] = e match {
			case col :ColumnLiteral[T] => Some(col.value)
			case _ => None
		}
	}






	trait SQLParameter[+T] extends SQLTerm[T]

	trait ColumnParameter[+T] extends SQLParameter[T] with ColumnTerm[T]



	object SQLParameter {

		type ParameterMatcher[+S <: FromClause, +Y[X]] = BoundParameterMatcher[S, Y]

		type MatchParameter[+S <: FromClause, +Y[X]] = ParameterMatcher[S, Y]

		type CaseParameter[+S <: FromClause, +Y[X]] = ParameterMatcher[S, Y]
	}



	case class BoundParameter[+T](value :T, name :Option[String] = None)(implicit protected[this] val form :SQLForm[T])
		extends SQLParameter[T]
	{
		override def readForm :SQLReadForm[T] = form
		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const[T](value)

		override def opt = BoundParameter(Option(value))

		override def freeValue = Some(value)


		override def applyTo[Y[+X]](matcher: FormulaMatcher[FromClause, Y]): Y[T] = matcher.param(this)


		//		override def canEqual(that :Any) :Boolean = that.isInstanceOf[BoundParameter[_]]
		//
		//		override def equals(that :Any) :Boolean = that match {
		//			case p :BoundParameter[_] =>
		//				(p eq this) || (p.canEqual(this) && p.value == value)
		//			case _ => false
		//		}
		//
		//		override def hashCode :Int = if (value == null) 0 else value.hashCode

		override def toString :String = "?" + value //if (value == null) "null" else "?"+value
	}



	object BoundParameter {

		trait BoundParameterMatcher[+S <: FromClause, +Y[X]] {
			def param[X](f :BoundParameter[X]) :Y[X]
		}

		type MatchBoundParameter[+S <: FromClause, +Y[X]] = BoundParameterMatcher[S, Y]

		type CaseBoundParameter[+S <: FromClause, +Y[X]] = BoundParameterMatcher[S, Y]

	}



	class BoundColumnParameter[+T](param :T, name :Option[String] = None)
	                              (implicit protected[this] override val form :ColumnForm[T])
		extends BoundParameter[T](param, name) with ColumnTerm[T]
	{
		override def readForm :ColumnReadForm[T] = form
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

		override def opt :BoundColumnParameter[Option[T]] = new BoundColumnParameter(Option(value))

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[BoundColumnParameter[_]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :BoundColumnParameter[_] if other canEqual this => other.value == value
			case _ => false
		}
	}



	object BoundColumnParameter {
		def apply[T :ColumnForm](param :T, name :Option[String] = None) :BoundColumnParameter[T] =
			new BoundColumnParameter[T](param, name)

		def unapply[T](e :SQLFormula[Nothing, T]) :Option[(T, Option[String])] = e match {
			case param :BoundColumnParameter[T] => Some(param.value -> param.name)
			case _ => None
		}

	}





	class SQLNull[+T :ColumnForm] private[SQLTerm] extends ColumnTerm[T] {
		override def writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const[T](readForm.nullValue)
		override def readForm :ColumnReadForm[T] = ColumnForm[T]

		override def freeValue = Some(readForm.nullValue)

		override def opt: ColumnFormula[FromClause, Option[T]] = SQLNull[Option[T]]



		override def applyTo[Y[+X]](matcher: FormulaMatcher[FromClause, Y]): Y[T] = matcher.sqlNull(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLNull[_]]

		override def equals(that :Any) :Boolean = that match {
			case n :SQLNull[_] => freeValue == n.freeValue
			case _ => false
		}

		override def hashCode :Int = SQLNull.hashCode

		override def toString = "Null"
	}



	object SQLNull {
		def apply[T :ColumnForm] = new SQLNull[T]

		def unapply(expression :SQLFormula[_, _]) :Boolean = expression match {
			case null => true
			case _ :SQLNull[_] => true
			case SQLLiteral(null) => true
			case _ => false
		}


		trait NullMatcher[+S <: FromClause, +Y[X]] {
			def sqlNull[X](f :SQLNull[X]) :Y[X]
		}

		type MatchNull[+S <: FromClause, +Y[X]] = NullMatcher[S, Y]

		type CaseNull[+S <: FromClause, +Y[X]] = NullMatcher[S, Y]

	}






	object True extends ColumnLiteral[Boolean](true) {
		def apply() :BooleanFormula[FromClause] = this

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
		def apply() :BooleanFormula[FromClause] = this

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






	case class NativeTerm[T :SQLForm](value :String) extends SQLTerm[T] {
		override def readForm :SQLReadForm[T] = SQLForm[T]

		override def writeForm :SQLWriteForm[Unit] = new EmptyWriteForm[Unit] {
			override def literal(value :Unit) = NativeTerm.this.value
			override def inlineLiteral(value :Unit) = NativeTerm.this.value
			override def nullLiteral = SQLForm[T].nullLiteral
			override def inlineNullLiteral = SQLForm[T].inlineNullLiteral
			override def toString = NativeTerm.this.value
		}

		override def applyTo[Y[+X]](matcher: FormulaMatcher[FromClause, Y]): Y[T] =
			matcher.native(this)

		override def toString :String = value
	}



	object NativeTerm {
		trait NativeMatcher[+S <: FromClause, +Y[X]] {
			def native[X](f :NativeTerm[X]) :Y[X]
		}

		type MatchNative[+S <: FromClause, +Y[X]] = NativeMatcher[S, Y]

		type CaseNative[+S <: FromClause, +Y[X]] = NativeMatcher[S, Y]

	}






	trait TermMatcher[+S <: FromClause, +Y[X]]
		extends LiteralMatcher[S, Y] with ParameterMatcher[S, Y] with NullMatcher[S, Y] with NativeMatcher[S, Y]

	trait MatchTerm[+S <: FromClause, +Y[X]]
		extends CaseLiteral[S, Y] with CaseParameter[S, Y] with CaseNull[S, Y] with CaseNative[S, Y]

	trait CaseTerm[+S <: FromClause, +Y[X]] extends TermMatcher[S, Y] with MatchTerm[S, Y] {
		def term[X](f :SQLTerm[X]) :Y[X]

		def param[X](f: BoundParameter[X]): Y[X] = term(f)

		def literal[X](f: SQLLiteral[X]): Y[X] = term(f)

		def sqlNull[X](f: SQLNull[X]): Y[X] = term(f)

		def native[X](f: NativeTerm[X]): Y[X] = term(f)

	}

}


package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, Mapping, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.MappingFrom
import net.noresttherein.oldsql.schema.SQLWriteForm.EmptyWriteForm
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.AnyJoinedRelation
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.ColumnFormulaMatcher
import net.noresttherein.oldsql.sql.SQLScribe.ColumnScribe
import net.noresttherein.oldsql.sql.SQLTerm.BoundColumnParameter.{BoundColumnParameterMatcher, CaseBoundColumnParameter}
import net.noresttherein.oldsql.sql.SQLTerm.BoundParameter
import net.noresttherein.oldsql.sql.SQLTerm.BoundParameter.{BoundParameterMatcher, CaseBoundParameter}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnLiteral.{CaseColumnLiteral, ColumnLiteralMatcher, MatchColumnLiteral}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.ColumnTermMatcher
import net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral.{CaseLiteral, LiteralMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NativeTerm.{CaseNative, NativeMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.NULL.{CaseNull, NullMatcher}




trait SQLTerm[T] extends SQLFormula[FromClause, T] {

	def writeForm :SQLWriteForm[Unit]

	//	override def readForm :SQLReadForm[T] = SQLReadForm[T]


	override def isomorphic(expression: Formula[_]): Boolean = this == expression

	private[oldsql] override def equivalent(expression: Formula[_]): Boolean = this == expression

	override def isGroundedIn(tables: Iterable[AnyJoinedRelation]): Boolean = freeValue.isDefined



//	override def get(values :RowValues[FromClause]) :Option[T] = freeValue
	override def stretch[M[O] <: MappingFrom[O]] :SQLTerm[T] = this

	override def stretch[U <: FromClause, S <: FromClause](implicit ev :U ExtendedBy S) :SQLTerm[T] = this

	override def stretch[U <: FromClause, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :SQLTerm[T] = this

}






abstract class MultiColumnTerms {

	implicit class TermFormulas[T :SQLForm](val term :T) {
		def ? = BoundParameter(term)
	}

}





object SQLTerm extends MultiColumnTerms {

	trait ColumnTerm[T] extends SQLTerm[T] with ColumnFormula[FromClause, T] {
		override def readForm :ColumnReadForm[T]
		override def writeForm :ColumnWriteForm[Unit]

		override def stretch[M[O] <: MappingFrom[O]] :ColumnTerm[T] = this

		override def stretch[U <: FromClause, S <: FromClause](implicit ev :U ExtendedBy S) :ColumnTerm[T] =
			this

		override def stretch[U <: FromClause, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :ColumnTerm[T] =
			this

	}



	object ColumnTerm {
		trait ColumnTermMatcher[+F <: FromClause, +Y[X]]
			extends ColumnLiteralMatcher[F, Y] with BoundColumnParameterMatcher[F, Y] with NullMatcher[F, Y]

		trait MatchColumnTerm[+F <: FromClause, +Y[X]] extends ColumnTermMatcher[F, Y]
			with CaseColumnLiteral[F, Y] with CaseBoundColumnParameter[F, Y] with CaseNull[F, Y]

		trait CaseColumnTerm[+F <: FromClause, +Y[X]] extends MatchColumnTerm[F, Y] {
			def term[X](e :ColumnTerm[X]) :Y[X]
			override def param[X](e :BoundColumnParameter[X]) :Y[X] = term(e)
			override def sqlNull[X](e :NULL[X]) :Y[X] = term(e)
			override def literal[X](e :ColumnLiteral[X]) :Y[X] = term(e)
		}
	}






	case class SQLLiteral[T](value :T)(implicit protected[this] val form :SQLForm[T]) extends SQLTerm[T] {

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


		override def applyTo[Y[X]](matcher: FormulaMatcher[FromClause, Y]): Y[T] = matcher.literal(this)


		override def toString :String = String.valueOf(value)

	}



	object SQLLiteral {

		trait LiteralMatcher[+S <: FromClause, +Y[X]] extends ColumnLiteralMatcher[S, Y] {
			def literal[X](e :SQLLiteral[X]) :Y[X]
		}

		trait MatchLiteral[+S <: FromClause, +Y[X]] extends LiteralMatcher[S, Y] with MatchColumnLiteral[S, Y]

		trait CaseLiteral[+S <: FromClause, +Y[X]] extends LiteralMatcher[S, Y] with CaseColumnLiteral[S, Y] {
			override def literal[X](f :ColumnLiteral[X]) :Y[X] = literal(f :SQLLiteral[X])
		}

	}



	class ColumnLiteral[T](literal :T)(implicit protected[this] override val form :ColumnForm[T])
		extends SQLLiteral[T](literal) with ColumnTerm[T]
	{
		override def readForm :ColumnReadForm[T] = form
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

		override def applyTo[Y[X]](matcher :ColumnFormulaMatcher[FromClause, Y]) :Y[T] = matcher.literal(this)

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

		trait ColumnLiteralMatcher[+S <: FromClause, +Y[X]] {
			def literal[X](f :ColumnLiteral[X]) :Y[X]
		}

		trait MatchColumnLiteral[+S <: FromClause, +Y[X]] extends ColumnLiteralMatcher[S, Y] {
			def nonbool[X](e :ColumnLiteral[X]) :Y[X]

			def bool(e :ColumnLiteral[Boolean]) :Y[Boolean]

			override def literal[X](e: ColumnLiteral[X]): Y[X] = e.value match {
				case _ :Boolean => bool(e.asInstanceOf[ColumnLiteral[Boolean]]).asInstanceOf[Y[X]]
				case _ => nonbool(e)
			}
		}

		type CaseColumnLiteral[+S <: FromClause, +Y[X]] = ColumnLiteralMatcher[S, Y]
	}





	case class BoundParameter[T](value :T, name :Option[String] = None)(implicit protected[this] val form :SQLForm[T])
		extends SQLTerm[T]
	{
		override def readForm :SQLReadForm[T] = form
		override val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const[T](value)

		override def opt = BoundParameter(Option(value))

		override def freeValue = Some(value)


		override def applyTo[Y[X]](matcher: FormulaMatcher[FromClause, Y]): Y[T] = matcher.param(this)


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

		trait BoundParameterMatcher[+S <: FromClause, +Y[X]] extends BoundColumnParameterMatcher[S, Y] {
			def param[X](e :BoundParameter[X]) :Y[X]
		}

		type MatchBoundParameter[+S <: FromClause, +Y[X]] = BoundParameterMatcher[S, Y]

		trait CaseBoundParameter[+S <: FromClause, +Y[X]]
			extends BoundParameterMatcher[S, Y] with CaseBoundColumnParameter[S, Y]
		{
			override def param[X](e :BoundColumnParameter[X]) :Y[X] = param(e :BoundParameter[X])
		}

	}



	class BoundColumnParameter[T](param :T, name :Option[String] = None)
	                             (implicit protected[this] override val form :ColumnForm[T])
		extends BoundParameter[T](param, name) with ColumnTerm[T]
	{
		override def readForm :ColumnReadForm[T] = form
		override val writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)

		override def opt :BoundColumnParameter[Option[T]] = new BoundColumnParameter(Option(value))


		override def applyTo[Y[X]](matcher :ColumnFormulaMatcher[FromClause, Y]) :Y[T] = matcher.param(this)

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


		trait BoundColumnParameterMatcher[+S <: FromClause, +Y[X]] {
			def param[X](e :BoundColumnParameter[X]) :Y[X]
		}

		type MatchBoundColumnParameter[+S <: FromClause, +Y[X]] = BoundColumnParameterMatcher[S, Y]

		type CaseBoundColumnParameter[+S <: FromClause, +Y[X]] = BoundColumnParameterMatcher[S, Y]

	}





	class NULL[T :ColumnForm] private[SQLTerm] extends ColumnTerm[T] {
		override def writeForm :ColumnWriteForm[Unit] = ColumnWriteForm.const[T](readForm.nullValue)
		override def readForm :ColumnReadForm[T] = ColumnForm[T]

		override def freeValue = Some(readForm.nullValue)

		override def opt: ColumnFormula[FromClause, Option[T]] = NULL[Option[T]]



		override def applyTo[Y[X]](matcher :ColumnFormulaMatcher[FromClause, Y]) :Y[T] = matcher.sqlNull(this)

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


		trait NullMatcher[+S <: FromClause, +Y[X]] {
			def sqlNull[X](e :NULL[X]) :Y[X]
		}

		type MatchNull[+S <: FromClause, +Y[X]] = NullMatcher[S, Y]

		type CaseNull[+S <: FromClause, +Y[X]] = NullMatcher[S, Y]

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






	case class NativeTerm[T :SQLForm](value :String) extends SQLTerm[T] {
		override def readForm :SQLReadForm[T] = SQLForm[T]

		override def writeForm :SQLWriteForm[Unit] = new EmptyWriteForm[Unit] {
			override def literal(value :Unit) = NativeTerm.this.value
			override def inlineLiteral(value :Unit) = NativeTerm.this.value
			override def nullLiteral = SQLForm[T].nullLiteral
			override def inlineNullLiteral = SQLForm[T].inlineNullLiteral
			override def toString = NativeTerm.this.value
		}

		override def applyTo[Y[X]](matcher: FormulaMatcher[FromClause, Y]): Y[T] =
			matcher.native(this)

		override def toString :String = value
	}



	object NativeTerm {
		trait NativeMatcher[+S <: FromClause, +Y[X]] {
			def native[X](e :NativeTerm[X]) :Y[X]
		}

		type MatchNative[+S <: FromClause, +Y[X]] = NativeMatcher[S, Y]

		type CaseNative[+S <: FromClause, +Y[X]] = NativeMatcher[S, Y]

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


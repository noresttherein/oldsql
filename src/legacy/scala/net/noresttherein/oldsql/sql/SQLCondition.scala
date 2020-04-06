package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.sql.SQLCondition.ComparisonFormula.{CaseComparison, ComparisonMatcher}
import net.noresttherein.oldsql.sql.SQLCondition.Equality.{CaseEquality, EqualityMatcher}
import net.noresttherein.oldsql.sql.SQLCondition.ExistsFormula.{CaseExists, ExistsMatcher}
import net.noresttherein.oldsql.sql.SQLCondition.In.{CaseIn, InMatcher}
import net.noresttherein.oldsql.sql.FromClause.RowValues
import net.noresttherein.oldsql.sql.SQLFormula.{ColumnFormula, CompositeFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLMapper.SQLRewriter






trait SQLCondition[-F <: FromClause] extends CompositeFormula[F, Boolean] with ColumnFormula[F, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]
}



object SQLCondition {
	//todo: uniform naming convention for classes.
	trait ComparisonFormula[-F <: FromClause, T] extends SQLCondition[F] {
		val left :SQLFormula[F, T]
		val right :SQLFormula[F, T]
		def symbol :String
	}



	object ComparisonFormula {

		type ComparisonMatcher[+F <: FromClause, +Y[X]] = EqualityMatcher[F, Y]

		type MatchComparison[+F <: FromClause, +Y[X]] = CaseEquality[F, Y]

		trait CaseComparison[+F <: FromClause, +Y[X]] extends MatchComparison[F, Y] {
			def comparison[X](f :ComparisonFormula[F, X]) :Y[Boolean]

			override def equality[X](f :Equality[F, X]) :Y[Boolean] = comparison(f)

		}
		//todo: ordering comparisons and like
	}



	case class Equality[-F <: FromClause, T](left :SQLFormula[F, T], right :SQLFormula[F, T])
		extends ComparisonFormula[F, T]
	{
		protected override def parts :Seq[SQLFormula[F, T]] = Seq(left, right)

		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield l == r

		override def get(values :RowValues[F]) :Option[Boolean] =
			for (l <- left.get(values); r <- right.get(values)) yield l == r

		def symbol = "="



		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Boolean] = matcher.equality(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]) = Equality(mapper(left), mapper(right))

		override def toString = s"$left == $right"
	}



	object Equality {
		trait EqualityMatcher[+F <: FromClause, +Y[X]] {
			def equality[X](f :Equality[F, X]) :Y[Boolean]
		}

		type MatchEquality[+F <: FromClause, +Y[X]] = EqualityMatcher[F, Y]

		type CaseEquality[+F <: FromClause, +Y[X]] = EqualityMatcher[F, Y]
	}








	case class ExistsFormula[-F <: FromClause, H](select :SelectFormula[F, _, H]) extends SQLCondition[F] {
		protected def parts :Seq[SelectFormula[F, _, H]] = select::Nil

		override def get(values: RowValues[F]): Option[Boolean] = None


		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Boolean] = matcher.exists(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]): SQLFormula[S, Boolean] = mapper(select) match {
			case sel :SelectFormula[_, _, _] => ExistsFormula(sel.asInstanceOf[SelectFormula[S, _, H]])
			case f => throw new IllegalArgumentException(s"Can't rewrite $this using $mapper because argument is not a select: $f")
		}

		override def toString = s"Exists($select)"
	}



	object ExistsFormula {
		trait ExistsMatcher[+F <: FromClause, +Y[X]] {
			def exists[X](f :ExistsFormula[F, X]) :Y[Boolean]
		}

		type MatchExists[+F <: FromClause, +Y[X]] = ExistsMatcher[F, Y]

		type CaseExists[+F <: FromClause, +Y[X]] = ExistsMatcher[F, Y]
	}



	case class In[-F <: FromClause, T](left :SQLFormula[F, T], right :SQLFormula[F, Seq[T]])
		extends SQLCondition[F]
	{
		protected override def parts :Seq[SQLFormula[F, _]] = Seq(left, right)

		override def get(values: RowValues[F]) :Option[Boolean] =
			for (l <- left.get(values); r <- right.get(values)) yield r.contains(l)

		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield r.contains(l)


		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Boolean] = matcher.in(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]): SQLFormula[S, Boolean] =
			In(mapper(left), mapper(right))



		private[oldsql] override def equivalent(expression: Formula[_]): Boolean = expression match {
			case l In r => (l equivalent left) && (r equivalent right)
			case _ => false
		}

		override def isomorphic(expression: Formula[_]): Boolean = expression match {
			case l In r => (l isomorphic left) && (r isomorphic right)
			case _ => false
		}

		override def toString = s"$left in $right"
	}



	object In {
		trait InMatcher[+F <: FromClause, +Y[X]] {
			def in[X](f :In[F, X]) :Y[Boolean]
		}

		type MatchIn[+F <: FromClause, +Y[X]] = InMatcher[F, Y]

		type CaseIn[+F <: FromClause, +Y[X]] = InMatcher[F, Y]
	}






	trait ConditionMatcher[+F <: FromClause, +Y[X]]
		extends InMatcher[F, Y] with ExistsMatcher[F, Y] with ComparisonMatcher[F, Y]

	trait MatchCondition[+F <: FromClause, +Y[X]]
		extends CaseIn[F, Y] with CaseExists[F, Y] with CaseComparison[F, Y]

	trait CaseCondition[+F <: FromClause, +Y[X]] extends ConditionMatcher[F, Y] with MatchCondition[F, Y] {
		def condition(f :SQLCondition[F]) :Y[Boolean]

		override def in[X](f :In[F, X]) :Y[Boolean] = condition(f)

		override def exists[X](f :ExistsFormula[F, X]) :Y[Boolean] = condition(f)

		override def comparison[X](f: ComparisonFormula[F, X]): Y[Boolean] = condition(f)

	}



}


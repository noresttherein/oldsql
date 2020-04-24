package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.SQLCondition.Comparison.{CaseComparison, ComparisonMatcher}
import net.noresttherein.oldsql.sql.SQLCondition.Equality.{CaseEquality, EqualityMatcher}
import net.noresttherein.oldsql.sql.SQLCondition.Inequality.InequalityMatcher
import net.noresttherein.oldsql.sql.SQLCondition.OrderComparison.OrderComparisonMatcher
import net.noresttherein.oldsql.sql.SQLFormula.{ColumnFormula, CompositeFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLMapper.SQLRewriter






trait SQLCondition[-F <: FromClause] extends CompositeFormula[F, Boolean] with ColumnFormula[F, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]
}






object SQLCondition {
	//todo: like, in, exists
	//todo: uniform naming convention for classes.

	trait Comparison[-F <: FromClause, T] extends SQLCondition[F] with CompositeFormula[F, Boolean] {
		val left :SQLFormula[F, T]
		val right :SQLFormula[F, T]
		def symbol :String

		protected override def parts :Seq[SQLFormula[F, T]] = left::right::Nil

		override def toString :String = left + " " + symbol + " " + right
	}



	object Comparison {

		final val LT = "<"
		final val LTE = "<="
		final val GT = ">"
		final val GTE = ">="
		final val EQ = "="
		final val NEQ = "<>"


		def apply[F <: FromClause, T :SQLOrdering]
		         (left :SQLFormula[F, T], symbol :String, right :SQLFormula[F, T]) :Comparison[F, T] =
			symbol match {
				case EQ => Equality(left, right)
				case NEQ =>Inequality(left, right)
				case _ => OrderComparison(left, symbol, right)
			}


		def unapply[F <: FromClause](f :SQLFormula[F, _]) :Option[(SQLFormula[F, T], String, SQLFormula[F, T])] forSome { type T } =
			f match {
				case compare :Comparison[F @unchecked, t] =>
					Some((compare.left, compare.symbol, compare.right))
			}



		trait ComparisonMatcher[+F <: FromClause, +Y[X]]
			extends OrderComparisonMatcher[F, Y] with EqualityMatcher[F, Y] with InequalityMatcher[F, Y]

		type MatchComparison[+F <: FromClause, +Y[X]] = ComparisonMatcher[F, Y]

		trait CaseComparison[+F <: FromClause, +Y[X]] extends MatchComparison[F, Y] {
			def comparison[X](f :Comparison[F, X]) :Y[Boolean]

			override def order[X](f :OrderComparison[F, X]) :Y[Boolean] = comparison(f)

			override def equality[X](f :Equality[F, X]) :Y[Boolean] = comparison(f)

			override def inequality[X](f :Inequality[F, X]) :Y[Boolean] = comparison(f)
		}

	}






	case class OrderComparison[-F <: FromClause, T]
	                          (ordering :SQLOrdering[T], left :SQLFormula[F, T], symbol :String, right :SQLFormula[F, T])
		extends Comparison[F, T]
	{
		import Comparison._

		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue)
				yield ordering.compare(l, r) match {
					case 0 => symbol == EQ || symbol == LTE || symbol == GTE
					case n if n < 0 => symbol == LT || symbol == LTE || symbol == NEQ
					case _ => symbol == GT || symbol == GTE || symbol == NEQ
				}

		override def map[S <: FromClause](mapper :SQLRewriter[F, S]) :SQLFormula[S, Boolean] =
			new OrderComparison(ordering, mapper(left), symbol, mapper(right))


		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :OrderComparison[S, T] =
			OrderComparison[S, T](ordering, left.stretch[U, S], symbol, right.stretch[U, S])

		override def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[Boolean] = matcher.order(this)
	}



	object OrderComparison {
		def apply[F <: FromClause, T](left :SQLFormula[F, T], symbol :String, right :SQLFormula[F, T])
		                             (implicit ordering :SQLOrdering[T]) =
			new OrderComparison(ordering, left, symbol, right)

		trait OrderComparisonMatcher[+F <: FromClause, +Y[X]] {
			def order[X](f :OrderComparison[F, X]) :Y[Boolean]
		}

		type MatchOrderComparison[+F <: FromClause, +Y[X]] = OrderComparisonMatcher[F, Y]

		type CaseOrderComparison[+F <: FromClause, +Y[X]] = OrderComparisonMatcher[F, Y]

	}






	case class Equality[-F <: FromClause, T](left :SQLFormula[F, T], right :SQLFormula[F, T])
		extends Comparison[F, T]
	{
		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield l == r

//		override def get(values :RowValues[F]) :Option[Boolean] =
//			for (l <- left.get(values); r <- right.get(values)) yield l == r

		override def symbol :String = Comparison.EQ



		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Boolean] = matcher.equality(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]) = Equality(mapper(left), mapper(right))

		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :Equality[S, T] =
			Equality(left.stretch[U, S], right.stretch[U, S])
	}



	object Equality {
		trait EqualityMatcher[+F <: FromClause, +Y[X]] {
			def equality[X](f :Equality[F, X]) :Y[Boolean]
		}

		type MatchEquality[+F <: FromClause, +Y[X]] = EqualityMatcher[F, Y]

		type CaseEquality[+F <: FromClause, +Y[X]] = EqualityMatcher[F, Y]
	}



	case class Inequality[-F <: FromClause, T](left :SQLFormula[F, T], right :SQLFormula[F, T])
		extends Comparison[F, T]
	{
		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield l != r

		//		override def get(values :RowValues[F]) :Option[Boolean] =
		//			for (l <- left.get(values); r <- right.get(values)) yield l == r

		override def symbol :String = Comparison.NEQ



		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Boolean] = matcher.inequality(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]) = Inequality(mapper(left), mapper(right))

		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :Inequality[S, T] =
			Inequality[S, T](left.stretch[U, S], right.stretch[U, S])
	}



	object Inequality {
		trait InequalityMatcher[+F <: FromClause, +Y[X]] {
			def inequality[X](f :Inequality[F, X]) :Y[Boolean]
		}

		type MatchInequality[+F <: FromClause, +Y[X]] = InequalityMatcher[F, Y]

		type CaseInequality[+F <: FromClause, +Y[X]] = InequalityMatcher[F, Y]
	}








/*
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
*/





/*
	trait ConditionMatcher[+F <: FromClause, +Y[X]]
		extends InMatcher[F, Y] with ExistsMatcher[F, Y] with ComparisonMatcher[F, Y]

	trait MatchCondition[+F <: FromClause, +Y[X]]
		extends CaseIn[F, Y] with CaseExists[F, Y] with CaseComparison[F, Y]

	trait CaseCondition[+F <: FromClause, +Y[X]] extends ConditionMatcher[F, Y] with MatchCondition[F, Y] {
		def condition(f :SQLCondition[F]) :Y[Boolean]

		override def in[X](f :In[F, X]) :Y[Boolean] = condition(f)

		override def exists[X](f :ExistsFormula[F, X]) :Y[Boolean] = condition(f)

		override def comparison[X](f: Comparison[F, X]): Y[Boolean] = condition(f)

	}
*/
	type ConditionMatcher[+F <: FromClause, +Y[X]] = ComparisonMatcher[F, Y]

	type MatchCondition[+F <: FromClause, +Y[X]] = CaseComparison[F, Y]

	trait CaseCondition[+F <: FromClause, +Y[X]] extends ConditionMatcher[F, Y] with MatchCondition[F, Y] {
		def condition(e :SQLCondition[F]) :Y[Boolean]

		override def comparison[X](e :Comparison[F, X]) :Y[Boolean] = condition(e)
	}



}


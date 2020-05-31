package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnExpressionMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ConditionSQL.ComparisonSQL.{CaseComparison, ComparisonMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.EqualitySQL.EqualityMatcher
import net.noresttherein.oldsql.sql.ConditionSQL.ExistsSQL.{CaseExists, ExistsMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.In.{CaseIn, InMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.InequalitySQL.InequalityMatcher
import net.noresttherein.oldsql.sql.ConditionSQL.LikeSQL.{CaseLike, LikeMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.OrderComparisonSQL.OrderComparisonMatcher
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL






trait ConditionSQL[-F <: FromClause] extends CompositeColumnSQL[F, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]
}






object ConditionSQL {
	//todo: uniform naming convention for classes.

	trait ComparisonSQL[-F <: FromClause, T] extends ConditionSQL[F] with CompositeSQL[F, Boolean] {
		val left :SQLExpression[F, T]
		val right :SQLExpression[F, T]
		def symbol :String

		protected override def parts :Seq[SQLExpression[F, T]] = left::right::Nil

		override def toString :String = left.toString + " " + symbol + " " + right
	}



	object ComparisonSQL {

		final val LT = "<"
		final val LTE = "<="
		final val GT = ">"
		final val GTE = ">="
		final val EQ = "="
		final val NEQ = "<>"


		def apply[F <: FromClause, T :SQLOrdering]
		         (left :SQLExpression[F, T], symbol :String, right :SQLExpression[F, T]) :ComparisonSQL[F, T] =
			symbol match {
				case EQ => EqualitySQL(left, right)
				case NEQ => InequalitySQL(left, right)
				case _ => OrderComparisonSQL(left, symbol, right)
			}


		def unapply[F <: FromClause](f :SQLExpression[F, _]) :Option[(SQLExpression[F, T], String, SQLExpression[F, T])] forSome { type T } =
			f match {
				case compare :ComparisonSQL[F @unchecked, t] =>
					Some((compare.left, compare.symbol, compare.right))
			}



		trait ComparisonMatcher[+F <: FromClause, +Y[X]]
			extends OrderComparisonMatcher[F, Y] with EqualityMatcher[F, Y] with InequalityMatcher[F, Y]

		type MatchComparison[+F <: FromClause, +Y[X]] = ComparisonMatcher[F, Y]

		trait CaseComparison[+F <: FromClause, +Y[X]] extends MatchComparison[F, Y] {
			def comparison[X](e :ComparisonSQL[F, X]) :Y[Boolean]

			override def order[X](e :OrderComparisonSQL[F, X]) :Y[Boolean] = comparison(e)

			override def equality[X](e :EqualitySQL[F, X]) :Y[Boolean] = comparison(e)

			override def inequality[X](e :InequalitySQL[F, X]) :Y[Boolean] = comparison(e)
		}

	}






	case class OrderComparisonSQL[-F <: FromClause, T]
	                          (ordering :SQLOrdering[T], left :SQLExpression[F, T], symbol :String, right :SQLExpression[F, T])
		extends ComparisonSQL[F, T]
	{
		import ComparisonSQL._

		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue)
				yield ordering.compare(l, r) match {
					case 0 => symbol == EQ || symbol == LTE || symbol == GTE
					case n if n < 0 => symbol == LT || symbol == LTE || symbol == NEQ
					case _ => symbol == GT || symbol == GTE || symbol == NEQ
				}


		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :SQLBoolean[S] =
			new OrderComparisonSQL(ordering, mapper(left), symbol, mapper(right))


		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[Boolean] =
			matcher.order(this)

	}



	object OrderComparisonSQL {
		def apply[F <: FromClause, T](left :SQLExpression[F, T], symbol :String, right :SQLExpression[F, T])
		                             (implicit ordering :SQLOrdering[T]) =
			new OrderComparisonSQL(ordering, left, symbol, right)

		trait OrderComparisonMatcher[+F <: FromClause, +Y[X]] {
			def order[X](e :OrderComparisonSQL[F, X]) :Y[Boolean]
		}

		type MatchOrderComparison[+F <: FromClause, +Y[X]] = OrderComparisonMatcher[F, Y]

		type CaseOrderComparison[+F <: FromClause, +Y[X]] = OrderComparisonMatcher[F, Y]

	}






	case class EqualitySQL[-F <: FromClause, T](left :SQLExpression[F, T], right :SQLExpression[F, T])
		extends ComparisonSQL[F, T]
	{
		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield l == r

		override def symbol :String = ComparisonSQL.EQ



		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = EqualitySQL(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[Boolean] = matcher.equality(this)

	}



	object EqualitySQL {
		trait EqualityMatcher[+F <: FromClause, +Y[X]] {
			def equality[X](e :EqualitySQL[F, X]) :Y[Boolean]
		}

		type MatchEquality[+F <: FromClause, +Y[X]] = EqualityMatcher[F, Y]

		type CaseEquality[+F <: FromClause, +Y[X]] = EqualityMatcher[F, Y]
	}



	case class InequalitySQL[-F <: FromClause, T](left :SQLExpression[F, T], right :SQLExpression[F, T])
		extends ComparisonSQL[F, T]
	{
		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield l != r


		override def symbol :String = ComparisonSQL.NEQ



		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = InequalitySQL(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[Boolean] = matcher.inequality(this)

	}



	object InequalitySQL {
		trait InequalityMatcher[+F <: FromClause, +Y[X]] {
			def inequality[X](e :InequalitySQL[F, X]) :Y[Boolean]
		}

		type MatchInequality[+F <: FromClause, +Y[X]] = InequalityMatcher[F, Y]

		type CaseInequality[+F <: FromClause, +Y[X]] = InequalityMatcher[F, Y]
	}






	case class LikeSQL[-F <: FromClause](expr :ColumnSQL[F, String], pattern :String) extends ConditionSQL[F] {
		protected override def parts :Seq[SQLExpression[F, String]]= expr::Nil

		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, Boolean] =
			LikeSQL(mapper(expr), pattern)

		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[Boolean] = matcher.like(this)

		override def toString = s"'$expr' like '$pattern'"
	}



	object LikeSQL {
		trait LikeMatcher[+F <: FromClause, +Y[X]] {
			def like(e :LikeSQL[F]) :Y[Boolean]
		}

		type MatchLike[+F <: FromClause, +Y[X]] = LikeMatcher[F, Y]

		type CaseLike[+F <: FromClause, +Y[X]] = LikeMatcher[F, Y]

	}






	case class ExistsSQL[-F <: FromClause, V, O](select :SelectSQL[F, V, O]) extends ConditionSQL[F] {
		protected override def parts :Seq[SQLExpression[F, _]] = select::Nil

		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, Boolean] =
			mapper(select) match {
				case sel :SelectSQL[S, V, _] => sel.exists
				case other =>
					throw new IllegalArgumentException(
						s"Can't map $this with $mapper because it didn't return a select expression: $other :${other.getClass}"
					)
			}

		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[Boolean] = matcher.exists(this)

		override def toString :String = s"EXISTS($select)"
	}



	object ExistsSQL {
		trait ExistsMatcher[+F <: FromClause, +Y[X]] {
			def exists[V, O](e :ExistsSQL[F, V, O]) :Y[Boolean]
		}

		type MatchExists[+F <: FromClause, +Y[X]] = ExistsMatcher[F, Y]

		type CaseExists[+F <: FromClause, +Y[X]] = ExistsMatcher[F, Y]
	}






	case class In[-F <: FromClause, V](left :ColumnSQL[F, V], right :SQLExpression[F, Seq[V]])
		extends ConditionSQL[F]
	{
		override protected def parts :Seq[SQLExpression[F, _]] = left::right::Nil

		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, Boolean] =
			In(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[Boolean] = matcher.in(this)

		override def toString :String = s"($left IN $right)"
	}



	object In {
		trait InMatcher[+F <: FromClause, +Y[X]] {
			def in[V](e :In[F, V]) :Y[Boolean]
		}

		type MatchIn[+F <: FromClause, +Y[X]] = InMatcher[F, Y]

		type CaseIn[+F <: FromClause, +Y[X]] = InMatcher[F, Y]
	}






	trait ConditionMatcher[+F <: FromClause, +Y[X]]
		extends ComparisonMatcher[F, Y] with LikeMatcher[F, Y] with ExistsMatcher[F, Y] with InMatcher[F, Y]

	trait MatchCondition[+F <: FromClause, +Y[X]]
		extends CaseComparison[F, Y] with CaseLike[F, Y] with CaseExists[F, Y] with CaseIn[F, Y]

	trait CaseCondition[+F <: FromClause, +Y[X]] extends ConditionMatcher[F, Y] with MatchCondition[F, Y] {
		def condition(e :ConditionSQL[F]) :Y[Boolean]

		override def comparison[X](e :ComparisonSQL[F, X]) :Y[Boolean] = condition(e)

		override def like(e :LikeSQL[F]) :Y[Boolean] = condition(e)

		override def in[V](e :In[F, V]) :Y[Boolean] = condition(e)

		override def exists[V, O](e :ExistsSQL[F, V, O]) :Y[Boolean] = condition(e)
	}



}


package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ConditionSQL.ComparisonSQL.{CaseComparison, Comparison, ComparisonMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.EqualitySQL.EqualityMatcher
import net.noresttherein.oldsql.sql.ConditionSQL.ExistsSQL.{CaseExists, ExistsMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.InSQL.{CaseIn, InMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.InequalitySQL.InequalityMatcher
import net.noresttherein.oldsql.sql.ConditionSQL.IsNULL.{CaseIsNull, IsNullMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.LikeSQL.{CaseLike, LikeMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.OrderComparisonSQL.OrderComparisonMatcher
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, GlobalScope, LocalScope}






trait ConditionSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope] extends CompositeColumnSQL[F, S, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]
}






object ConditionSQL {

	case class IsNULL[-F <: FromClause, -S >: LocalScope <: GlobalScope, T](expr :SQLExpression[F, S, T])
		extends ConditionSQL[F, S]
	{
		protected override def parts :Seq[SQLExpression[F, S, T]] = expr::Nil

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			new IsNULL(mapper(expr))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.isNull(this)

		override def toString :String = "(" + expr + "is null)"
	}



	object IsNULL {
		trait IsNullMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def isNull[S >: LocalScope <: GlobalScope, T](e :IsNULL[F, S, T]) :Y[S, Boolean]
		}

		type MatchIsNull[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = IsNullMatcher[F, Y]

		type CaseIsNull[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = IsNullMatcher[F, Y]
	}






	trait ComparisonSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, T] extends ConditionSQL[F, S] {
		val left :SQLExpression[F, S, T]
		val right :SQLExpression[F, S, T]
		def comparison :Comparison
		def symbol :String = comparison.symbol

		protected override def parts :Seq[SQLExpression[F, S, T]] = left::right::Nil

		override def sameAs(other :CompositeSQL[_, _, _]) :Boolean = other match {
			case cmp :ComparisonSQL[_, _, _] => cmp.comparison == comparison
			case _ => false
		}

		override def toString :String = left.toString + " " + comparison + " " + right
	}



	object ComparisonSQL {

		def apply[F <: FromClause, S >: LocalScope <: GlobalScope, T :SQLOrdering]
		         (left :SQLExpression[F, S, T], cmp :Comparison, right :SQLExpression[F, S, T]) :ComparisonSQL[F, S, T] =
			cmp match {
				case EQ => EqualitySQL(left, right)
				case NEQ => InequalitySQL(left, right)
				case _ => OrderComparisonSQL(left, cmp, right)
			}

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
				:Option[(SQLExpression[F, S, T], Comparison, SQLExpression[F, S, T]) forSome { type T }] =
			e match {
				case cmp :ComparisonSQL[F @unchecked, S @unchecked, t] => //fixme: ComparisonSQL
					Some((cmp.left, cmp.comparison, cmp.right))
				case _ => None
			}


		class Comparison private[ConditionSQL] (val symbol :String) extends AnyVal {
			def apply[F <: FromClause, S >: LocalScope <: GlobalScope, T :SQLOrdering]
			         (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T]) :ComparisonSQL[F, S, T] =
				ComparisonSQL(left, this, right)

			def unapply[F <: FromClause, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
					:Option[(SQLExpression[F, S, T], SQLExpression[F, S, T]) forSome { type T }] =
				e match {
					case compare :ComparisonSQL[F @unchecked, S @unchecked, t] if compare.comparison == this =>
						Some((compare.left, compare.right))
					case _ => None
				}
		}

		final val LT = new Comparison("<")
		final val LTE = new Comparison("<=")
		final val GT = new Comparison(">")
		final val GTE = new Comparison(">=")
		final val EQ = new Comparison("=")
		final val NEQ = new Comparison("<>")



		trait ComparisonMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends OrderComparisonMatcher[F, Y] with EqualityMatcher[F, Y] with InequalityMatcher[F, Y]

		type MatchComparison[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ComparisonMatcher[F, Y]

		trait CaseComparison[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchComparison[F, Y] {
			def comparison[S >: LocalScope <: GlobalScope, X](e :ComparisonSQL[F, S, X]) :Y[S, Boolean]

			override def equality[S >: LocalScope <: GlobalScope, X](e :EqualitySQL[F, S, X]) :Y[S, Boolean] =
				comparison(e)

			override def inequality[S >: LocalScope <: GlobalScope, X](e :InequalitySQL[F, S, X]) :Y[S, Boolean] =
				comparison(e)

			override def order[S >: LocalScope <: GlobalScope, X](e :OrderComparisonSQL[F, S, X]) :Y[S, Boolean] =
				comparison(e)
		}

	}






	class OrderComparisonSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, T]
	                        (override val left :SQLExpression[F, S, T], override val comparison :Comparison,
							 override val right :SQLExpression[F, S, T])
							(implicit val ordering :SQLOrdering[T])
		extends ComparisonSQL[F, S, T]
	{
		import ComparisonSQL._

		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue)
				yield ordering.compare(l, r) match {
					case 0 => comparison == EQ || comparison == LTE || comparison == GTE
					case n if n < 0 => comparison == LT || comparison == LTE || comparison == NEQ
					case _ => comparison == GT || comparison == GTE || comparison == NEQ
				}


		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			new OrderComparisonSQL[E, S, T](mapper(left), comparison, mapper(right))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.order[S, T](this)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :OrderComparisonSQL[_, _, _] if other canEqual this =>
				other.comparison == comparison && other.left == left && other.right == right && other.ordering == ordering
			case _ => false
		}

		override def hashCode :Int =
			((comparison.hashCode * 31 + left.hashCode) * 31 + right.hashCode) * 31 + ordering.hashCode

	}



	object OrderComparisonSQL {
		def apply[F <: FromClause, S >: LocalScope <: GlobalScope, T :SQLOrdering]
		         (left :SQLExpression[F, S, T], cmp :Comparison, right :SQLExpression[F, S, T]) =
			new OrderComparisonSQL(left, cmp, right)

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
			:Option[(SQLExpression[F, S, T], Comparison, SQLExpression[F, S, T]) forSome { type T }] =
			e match {
				case cmp :OrderComparisonSQL[F @unchecked, S @unchecked, t] =>
					Some((cmp.left, cmp.comparison, cmp.right))
				case _ => None
			}

		trait OrderComparisonMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def order[S >: LocalScope <: GlobalScope, X](e :OrderComparisonSQL[F, S, X]) :Y[S, Boolean]
		}

		type MatchOrderComparison[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = OrderComparisonMatcher[F, Y]

		type CaseOrderComparison[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = OrderComparisonMatcher[F, Y]

	}






	case class EqualitySQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, T]
	                      (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T])
		extends ComparisonSQL[F, S, T]
	{
		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield l == r

		override def comparison :Comparison = ComparisonSQL.EQ


		override def rephrase[E <: FromClause](mapper: SQLScribe[F, E]) :EqualitySQL[E, S, T] =
			EqualitySQL(mapper(left), mapper(right))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.equality(this)

	}



	object EqualitySQL {
		trait EqualityMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def equality[S >: LocalScope <: GlobalScope, X](e :EqualitySQL[F, S, X]) :Y[S, Boolean]
		}

		type MatchEquality[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = EqualityMatcher[F, Y]

		type CaseEquality[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = EqualityMatcher[F, Y]
	}



	case class InequalitySQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, T]
	                        (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T])
		extends ComparisonSQL[F, S, T]
	{
		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield l != r

		override def comparison :Comparison = ComparisonSQL.NEQ


		override def rephrase[E <: FromClause](mapper: SQLScribe[F, E]) :InequalitySQL[E, S, T] =
			InequalitySQL(mapper(left), mapper(right))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.inequality(this)

	}



	object InequalitySQL {
		trait InequalityMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def inequality[S >: LocalScope <: GlobalScope, X](e :InequalitySQL[F, S, X]) :Y[S, Boolean]
		}

		type MatchInequality[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = InequalityMatcher[F, Y]

		type CaseInequality[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = InequalityMatcher[F, Y]
	}





	//consider: pattern as an SQLExpression
	case class LikeSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope](expr :ColumnSQL[F, S, String], pattern :String)
		extends ConditionSQL[F, S]
	{
		protected override def parts :Seq[SQLExpression[F, S, String]]= expr::Nil

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			LikeSQL(mapper(expr), pattern)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.like(this)

		override def toString = s"'$expr' LIKE '$pattern'"
	}



	object LikeSQL {
		trait LikeMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def like[S >: LocalScope <: GlobalScope](e :LikeSQL[F, S]) :Y[S, Boolean]
		}

		type MatchLike[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = LikeMatcher[F, Y]

		type CaseLike[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = LikeMatcher[F, Y]

	}






	case class ExistsSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, V](select :SelectSQL[F, S, V, _])
		extends ConditionSQL[F, S]
	{
		protected override def parts :Seq[SQLExpression[F, S, _]] = select::Nil

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			mapper(select) match {
				case sel :SelectSQL[E, S, V, _] => sel.exists
				case other =>
					throw new IllegalArgumentException(
						s"Can't map $this with $mapper because it didn't return a select expression: $other :${other.getClass}"
					)
			}

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.exists(this)

		override def toString :String = s"EXISTS($select)"
	}



	object ExistsSQL {
		trait ExistsMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def exists[S >: LocalScope <: GlobalScope, V](e :ExistsSQL[F, S, V]) :Y[S, Boolean]
		}

		type MatchExists[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ExistsMatcher[F, Y]

		type CaseExists[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ExistsMatcher[F, Y]
	}






	case class InSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, V]
	                (left :ColumnSQL[F, S, V], right :SQLExpression[F, S, Seq[V]])
		extends ConditionSQL[F, S]
	{
		override protected def parts :Seq[SQLExpression[F, S, _]] = left::right::Nil

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			InSQL(mapper(left), mapper(right))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.in(this)

		override def toString :String = s"($left IN $right)"
	}



	object InSQL {
		trait InMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def in[S >: LocalScope <: GlobalScope, V](e :InSQL[F, S, V]) :Y[S, Boolean]
		}

		type MatchIn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = InMatcher[F, Y]

		type CaseIn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = InMatcher[F, Y]
	}






	trait ConditionMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends IsNullMatcher[F, Y] with ComparisonMatcher[F, Y] with LikeMatcher[F, Y]
		   with ExistsMatcher[F, Y] with InMatcher[F, Y]

	trait MatchCondition[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ConditionMatcher[F, Y]
		with CaseIsNull[F, Y] with CaseComparison[F, Y] with CaseLike[F, Y] with CaseExists[F, Y] with CaseIn[F, Y]

	trait CaseCondition[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchCondition[F, Y] {
		def condition[S >: LocalScope <: GlobalScope](e :ConditionSQL[F, S]) :Y[S, Boolean]

		override def comparison[S >: LocalScope <: GlobalScope, X](e :ComparisonSQL[F, S, X]) :Y[S, Boolean] =
			condition(e)

		override def isNull[S >: LocalScope <: GlobalScope, T](e :IsNULL[F, S, T]) :Y[S, Boolean] = condition(e)

		override def like[S >: LocalScope <: GlobalScope](e :LikeSQL[F, S]) :Y[S, Boolean] = condition(e)

		override def in[S >: LocalScope <: GlobalScope, V](e :InSQL[F, S, V]) :Y[S, Boolean] = condition(e)

		override def exists[S >: LocalScope <: GlobalScope, V](e :ExistsSQL[F, S, V]) :Y[S, Boolean] = condition(e)
	}


}


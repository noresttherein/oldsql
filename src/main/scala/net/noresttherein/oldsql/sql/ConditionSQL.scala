package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.UnaryColumnOperator
import net.noresttherein.oldsql.sql.ConditionSQL.ComparisonSQL.{CaseComparison, ComparisonMatcher, ComparisonOperator}
import net.noresttherein.oldsql.sql.ConditionSQL.EqualitySQL.EqualityMatcher
import net.noresttherein.oldsql.sql.ConditionSQL.ExistsSQL.{CaseExists, ExistsMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.InSQL.{CaseIn, InMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.InequalitySQL.InequalityMatcher
import net.noresttherein.oldsql.sql.ConditionSQL.IsNull.{CaseIsNull, IsNullMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.LikeSQL.{CaseLike, LikeMatcher}
import net.noresttherein.oldsql.sql.ConditionSQL.OrderComparisonSQL.OrderComparisonMatcher
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.{BinaryOperatorSQL, UnaryOperatorSQL}






trait ConditionSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope] extends CompositeColumnSQL[F, S, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
//		matcher.condition(this)
}






object ConditionSQL {

	case class IsNull[-F <: FromClause, -S >: LocalScope <: GlobalScope, T](value :SQLExpression[F, S, T])
		extends UnaryOperatorSQL[F, S, T, Boolean] with ConditionSQL[F, S]
	{
		override def anchor(from :F) :ColumnSQL[F, S, Boolean] = value.anchor(from) match {
			case same if same eq value => this
			case anchored => new IsNull(anchored)
		}

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			reapply(mapper(value))

		protected override def reapply[E <: FromClause, C >: LocalScope <: GlobalScope]
		                              (expr :SQLExpression[E, C, T]) :ColumnSQL[E, C, Boolean] =
			new IsNull(expr)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.isNull(this)

		override def toString :String = "(" + value + "is null)"
	}



	object IsNull {
		trait IsNullMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def isNull[S >: LocalScope <: GlobalScope, T](e :IsNull[F, S, T]) :Y[S, Boolean]
		}

		type MatchIsNull[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = IsNullMatcher[F, Y]

		type CaseIsNull[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = IsNullMatcher[F, Y]
	}






	trait ComparisonSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, T]
		extends BinaryOperatorSQL[F, S, T, Boolean] with ConditionSQL[F, S]
	{
		def comparison :ComparisonOperator
		def symbol :String = comparison.symbol

		override def anchor(from :F) :ColumnSQL[F, S, Boolean] = (left.anchor(from), right.anchor(from)) match {
			case (l, r) if (l eq left) && (r eq right) => this
			case (l, r) => reapply(l, r)
		}

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			reapply(mapper(left), mapper(right))

		protected override def reapply[E <: FromClause, C >: LocalScope <: GlobalScope]
		                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T]


//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
//			matcher.comparison(this)

		override def sameAs(that :CompositeSQL.*) :Boolean = that match {
			case cmp :ComparisonSQL[_, _, _] => cmp.comparison == comparison
			case _ => false
		}

		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :ComparisonSQL[_, _, _] if canEqual(other) && other.canEqual(this) =>
				comparison == other.comparison && left == other.left && right == other.right
			case _ => false
		}

		override def hashCode :Int = super[BinaryOperatorSQL].hashCode * 31 + comparison.hashCode

		override def toString :String = left.toString + " " + comparison + " " + right
	}



	object ComparisonSQL {

		def apply[F <: FromClause, S >: LocalScope <: GlobalScope, T :SQLOrdering]
		         (left :SQLExpression[F, S, T], cmp :ComparisonOperator, right :SQLExpression[F, S, T])
				:ComparisonSQL[F, S, T] =
			cmp match {
				case EQ => EqualitySQL(left, right)
				case NEQ => InequalitySQL(left, right)
				case _ => OrderComparisonSQL(left, cmp, right)
			}

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
				:Option[(SQLExpression[F, S, T], ComparisonOperator, SQLExpression[F, S, T]) forSome { type T }] =
			e match {
				case cmp :ComparisonSQL[F @unchecked, S @unchecked, t] => //fixme: ComparisonSQL
					Some((cmp.left, cmp.comparison, cmp.right))
				case _ => None
			}


		class ComparisonOperator private[ConditionSQL](val symbol :String) extends AnyVal {
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

		final val LT = new ComparisonOperator("<")
		final val LTE = new ComparisonOperator("<=")
		final val GT = new ComparisonOperator(">")
		final val GTE = new ComparisonOperator(">=")
		final val EQ = new ComparisonOperator("=")
		final val NEQ = new ComparisonOperator("<>")



		trait ComparisonMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends OrderComparisonMatcher[F, Y] with EqualityMatcher[F, Y] with InequalityMatcher[F, Y]
		{
			def comparison[S >: LocalScope <: GlobalScope, X](e :ComparisonSQL[F, S, X]) :Y[S, Boolean]
		}

		type MatchComparison[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ComparisonMatcher[F, Y]

		trait CaseComparison[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchComparison[F, Y] {

			override def equality[S >: LocalScope <: GlobalScope, X](e :EqualitySQL[F, S, X]) :Y[S, Boolean] =
				comparison(e)

			override def inequality[S >: LocalScope <: GlobalScope, X](e :InequalitySQL[F, S, X]) :Y[S, Boolean] =
				comparison(e)

			override def order[S >: LocalScope <: GlobalScope, X](e :OrderComparisonSQL[F, S, X]) :Y[S, Boolean] =
				comparison(e)
		}

	}






	class OrderComparisonSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, T]
	                        (override val left :SQLExpression[F, S, T], override val comparison :ComparisonOperator,
	                         override val right :SQLExpression[F, S, T])
	                        (implicit val ordering :SQLOrdering[T])
		extends ComparisonSQL[F, S, T]
	{
		protected override def reapply[E <: FromClause, C >: LocalScope <: GlobalScope]
		                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T] =
			new OrderComparisonSQL(left, comparison, right)


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
		         (left :SQLExpression[F, S, T], cmp :ComparisonOperator, right :SQLExpression[F, S, T]) =
			new OrderComparisonSQL(left, cmp, right)

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
				:Option[(SQLExpression[F, S, T], ComparisonOperator, SQLExpression[F, S, T]) forSome { type T }] =
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
		override def comparison :ComparisonOperator = ComparisonSQL.EQ

		protected override def reapply[E <: FromClause, C >: LocalScope <: GlobalScope]
		                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T] =
			new EqualitySQL(left, right)

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
		override def comparison :ComparisonOperator = ComparisonSQL.NEQ

		protected override def reapply[E <: FromClause, C >: LocalScope <: GlobalScope]
		                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T])
				:ComparisonSQL[E, C, T] =
			new InequalitySQL(left, right)

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
	case class LikeSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope]
	                  (value :ColumnSQL[F, S, String], pattern :String)
		extends UnaryColumnOperator[F, S, String, Boolean] with ConditionSQL[F, S]
	{
		protected override def reapply[E <: FromClause, C >: LocalScope <: GlobalScope]
		                              (e :ColumnSQL[E, C, String]) :ColumnSQL[E, C, Boolean] =
			LikeSQL(e, pattern)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.like(this)

		override def toString = s"'$value' LIKE '$pattern'"
	}



	object LikeSQL {
		trait LikeMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def like[S >: LocalScope <: GlobalScope](e :LikeSQL[F, S]) :Y[S, Boolean]
		}

		type MatchLike[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = LikeMatcher[F, Y]

		type CaseLike[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = LikeMatcher[F, Y]

	}






	case class ExistsSQL[-F <: FromClause, V](select :SQLExpression[F, GlobalScope, Rows[V]])
		extends ConditionSQL[F, GlobalScope]
	{
		protected override def parts :Seq[SQLExpression[F, GlobalScope, _]] = select::Nil

		override def isAnchored = true
		override def anchor(from :F) :SQLBoolean[F, GlobalScope] = this

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, GlobalScope, Boolean] =
			ExistsSQL(mapper(select))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Boolean] =
			matcher.exists(this)

		override def toString :String = s"EXISTS($select)"
	}



	object ExistsSQL {
		trait ExistsMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def exists[V](e :ExistsSQL[F, V]) :Y[GlobalScope, Boolean]
		}

		type MatchExists[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ExistsMatcher[F, Y]

		type CaseExists[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ExistsMatcher[F, Y]
	}






	case class InSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, V]
	                (left :ColumnSQL[F, S, V], right :SQLExpression[F, S, Seq[V]])
		extends ConditionSQL[F, S]
	{
		override protected def parts :Seq[SQLExpression[F, S, _]] = left::right::Nil

		override def isAnchored :Boolean = left.isAnchored && right.isAnchored

		override def anchor(from :F) :SQLBoolean[F, S] = (left.anchor(from), right.anchor(from)) match {
			case (l, r) if (l eq left) && (r eq right) => this
			case (l, r) => InSQL(l, r)
		}


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
	{
		def condition[S >: LocalScope <: GlobalScope](e :ConditionSQL[F, S]) :Y[S, Boolean]
	}

	trait MatchCondition[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ConditionMatcher[F, Y]
		with CaseIsNull[F, Y] with CaseComparison[F, Y] with CaseLike[F, Y] with CaseExists[F, Y] with CaseIn[F, Y]

	trait CaseCondition[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchCondition[F, Y] {

		override def comparison[S >: LocalScope <: GlobalScope, X](e :ComparisonSQL[F, S, X]) :Y[S, Boolean] =
			condition(e)

		override def isNull[S >: LocalScope <: GlobalScope, T](e :IsNull[F, S, T]) :Y[S, Boolean] = condition(e)

		override def like[S >: LocalScope <: GlobalScope](e :LikeSQL[F, S]) :Y[S, Boolean] = condition(e)

		override def in[S >: LocalScope <: GlobalScope, V](e :InSQL[F, S, V]) :Y[S, Boolean] = condition(e)

		override def exists[V](e :ExistsSQL[F, V]) :Y[GlobalScope, Boolean] = condition(e)
	}


}


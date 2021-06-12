package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLBoolean, SQLExpression, SQLString}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnVisitor, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.BinaryColumnOperator
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.{BinaryOperatorSQL, UnaryOperatorSQL}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.BetweenSQL.{BetweenVisitor, CaseBetween}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.ComparisonSQL.{CaseComparison, ComparisonVisitor, ComparisonOperator}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.EqualitySQL.EqualityVisitor
import net.noresttherein.oldsql.sql.ast.ConditionSQL.ExistsSQL.{CaseExists, ExistsVisitor}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.InequalitySQL.InequalityVisitor
import net.noresttherein.oldsql.sql.ast.ConditionSQL.InSQL.{CaseIn, InVisitor}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.IsNull.{CaseIsNull, IsNullVisitor}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.LikeSQL.{CaseLike, LikeVisitor}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.OrderComparisonSQL.OrderComparisonVisitor
import net.noresttherein.oldsql.sql.ast.QuerySQL.Rows
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLOrdering, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






trait ConditionSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope] extends CompositeColumnSQL[F, S, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnVisitor[F, Y]) :Y[S, Boolean] =
//		matcher.condition(this)
}






object ConditionSQL {

	case class IsNull[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T](value :SQLExpression[F, S, T])
		extends UnaryOperatorSQL[F, S, T, Boolean] with ConditionSQL[F, S]
	{
		override def groundValue :Opt[Boolean] = value.groundValue.map(_ == null)

		override def anchor(from :F) :ColumnSQL[F, S, Boolean] = value.anchor(from) match {
			case same if same eq value => this
			case anchored => new IsNull(anchored)
		}

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			reapply(mapper(value))

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (expr :SQLExpression[E, C, T]) :ColumnSQL[E, C, Boolean] =
			new IsNull(expr)

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
			visitor.isNull(this)

		//fixme: this should apply the condition to every column separately, but we don't have API which would expose them
		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			spelling(value :SQLExpression[E, S, T])(context, params) + " " +
				spelling.keyword("is") + " " + spelling.NULL

		override def toString :String = "(" + value + "is null)"
	}


	object IsNull {
		trait IsNullVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def isNull[S >: LocalScope <: GlobalScope, T](e :IsNull[F, S, T]) :Y[S, Boolean]
		}
		type MatchIsNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = IsNullVisitor[F, Y]
		type CaseIsNull[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = IsNullVisitor[F, Y]
	}




	trait ComparisonSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T]
		extends BinaryOperatorSQL[F, S, T, Boolean] with ConditionSQL[F, S]
	{
		def comparison :ComparisonOperator
		def symbol :String = comparison.symbol

		override def anchor(from :F) :ColumnSQL[F, S, Boolean] = (left.anchor(from), right.anchor(from)) match {
			case (l, r) if (l eq left) && (r eq right) => this
			case (l, r) => reapply(l, r)
		}

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			reapply(mapper(left), mapper(right))

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T]


//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnVisitor[F, Y]) :Y[S, Boolean] =
//			matcher.comparison(this)
		//fixme: column-based comparisons. Need to override === in ComponentSQL at least for literals
		//  this will be hard because the order of params can be changed if there are params in left and right
		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			spelling(left :SQLExpression[E, S, T])(context, params) + (" " + symbol + " ") +
				(spelling(right :SQLExpression[E, S, T])(_, _))


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

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, T :SQLOrdering]
		         (left :SQLExpression[F, S, T], cmp :ComparisonOperator, right :SQLExpression[F, S, T])
				:ComparisonSQL[F, S, T] =
			cmp match {
				case EQ => EqualitySQL(left, right)
				case NEQ => InequalitySQL(left, right)
				case _ => OrderComparisonSQL(left, cmp, right)
			}

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
				:Opt[(SQLExpression[F, S, T], ComparisonOperator, SQLExpression[F, S, T]) forSome { type T }] =
			e match {
				case cmp :ComparisonSQL[F @unchecked, S @unchecked, t] => //fixme: ComparisonSQL
					Got((cmp.left, cmp.comparison, cmp.right))
				case _ => Lack
			}


		class ComparisonOperator private[ConditionSQL](val symbol :String, cmpResult :Int, inverse :Boolean)
			extends Serializable
		{
			def apply[T](left :T, right :T)(implicit ordering :SQLOrdering[T]) :Boolean =
				(ordering.compare(left, right).sign == cmpResult) != inverse

			def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, T :SQLOrdering]
			         (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T]) :ComparisonSQL[F, S, T] =
				ComparisonSQL(left, this, right)

			def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
					:Opt[(SQLExpression[F, S, T], SQLExpression[F, S, T]) forSome { type T }] =
				e match {
					case compare :ComparisonSQL[F @unchecked, S @unchecked, t] if compare.comparison == this =>
						Got((compare.left, compare.right))
					case _ => Lack
				}
		}

		final val LT = new ComparisonOperator("<", -1, false)
		final val LTE = new ComparisonOperator("<=", 1, true)
		final val GT = new ComparisonOperator(">", 1, false)
		final val GTE = new ComparisonOperator(">=", -1, true)
		final val EQ = new ComparisonOperator("=", 0, false)
		final val NEQ = new ComparisonOperator("<>", 0, true)


		trait ComparisonVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends OrderComparisonVisitor[F, Y] with EqualityVisitor[F, Y] with InequalityVisitor[F, Y]
		{
			def comparison[S >: LocalScope <: GlobalScope, X](e :ComparisonSQL[F, S, X]) :Y[S, Boolean]
		}

		type MatchComparison[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ComparisonVisitor[F, Y]

		trait CaseComparison[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchComparison[F, Y] {

			override def equality[S >: LocalScope <: GlobalScope, X](e :EqualitySQL[F, S, X]) :Y[S, Boolean] =
				comparison(e)

			override def inequality[S >: LocalScope <: GlobalScope, X](e :InequalitySQL[F, S, X]) :Y[S, Boolean] =
				comparison(e)

			override def order[S >: LocalScope <: GlobalScope, X](e :OrderComparisonSQL[F, S, X]) :Y[S, Boolean] =
				comparison(e)
		}
	}




	class OrderComparisonSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T]
	                        (override val left :SQLExpression[F, S, T], override val comparison :ComparisonOperator,
	                         override val right :SQLExpression[F, S, T])
	                        (implicit val ordering :SQLOrdering[T])
		extends ComparisonSQL[F, S, T]
	{
		override def groundValue :Opt[Boolean] =
			for (l <- left.groundValue; r <- right.groundValue) yield comparison(l, r)

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T] =
			new OrderComparisonSQL(left, comparison, right)

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
			visitor.order[S, T](this)

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
		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, T :SQLOrdering]
		         (left :SQLExpression[F, S, T], cmp :ComparisonOperator, right :SQLExpression[F, S, T]) =
			new OrderComparisonSQL(left, cmp, right)

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
				:Opt[(SQLExpression[F, S, T], ComparisonOperator, SQLExpression[F, S, T]) forSome { type T }] =
			e match {
				case cmp :OrderComparisonSQL[F @unchecked, S @unchecked, t] =>
					Got((cmp.left, cmp.comparison, cmp.right))
				case _ => Lack
			}

		trait OrderComparisonVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def order[S >: LocalScope <: GlobalScope, X](e :OrderComparisonSQL[F, S, X]) :Y[S, Boolean]
		}
		type MatchOrderComparison[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = OrderComparisonVisitor[F, Y]
		type CaseOrderComparison[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = OrderComparisonVisitor[F, Y]
	}




	case class EqualitySQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T]
	                      (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T])
		extends ComparisonSQL[F, S, T]
	{
		override def comparison :ComparisonOperator = ComparisonSQL.EQ

		override def groundValue :Opt[Boolean] =
			for (l <- left.groundValue; r <- right.groundValue) yield l == r

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T] =
			new EqualitySQL(left, right)

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
			visitor.equality(this)
	}


	object EqualitySQL {
		trait EqualityVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def equality[S >: LocalScope <: GlobalScope, X](e :EqualitySQL[F, S, X]) :Y[S, Boolean]
		}
		type MatchEquality[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = EqualityVisitor[F, Y]
		type CaseEquality[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = EqualityVisitor[F, Y]
	}


	case class InequalitySQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T]
	                        (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T])
		extends ComparisonSQL[F, S, T]
	{
		override def comparison :ComparisonOperator = ComparisonSQL.NEQ

		override def groundValue :Opt[Boolean] =
			for (l <- left.groundValue; r <- right.groundValue) yield l != r

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T])
				:ComparisonSQL[E, C, T] =
			new InequalitySQL(left, right)

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
			visitor.inequality(this)
	}


	object InequalitySQL {
		trait InequalityVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def inequality[S >: LocalScope <: GlobalScope, X](e :InequalitySQL[F, S, X]) :Y[S, Boolean]
		}
		type MatchInequality[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = InequalityVisitor[F, Y]
		type CaseInequality[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = InequalityVisitor[F, Y]
	}




	case class BetweenSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V :SQLOrdering]
	                     (value :ColumnSQL[F, S, V], low :ColumnSQL[F, S, V], high :ColumnSQL[F, S, V])
		extends ConditionSQL[F, S]
	{
		protected override val parts :Seq[SQLExpression[F, S, _]] = value::low::high::Nil

		override def groundValue :Opt[Boolean] =
			for (v <- value.groundValue; l <- low.groundValue; h <- high.groundValue)
				yield SQLOrdering[V].compare(v, l) >= 0 && SQLOrdering[V].compare(v, h) <= 0

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			BetweenSQL(mapper(value), mapper(low), mapper(high))

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
			visitor.between(this)

		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			(value :ColumnSQL[E, S, V]).inParens(context, params) + (" " + spelling.BETWEEN + " ") +
				(spelling(low :ColumnSQL[E, S, V])(_, _)) + (" " + spelling.AND + " ") +
				(spelling(high :ColumnSQL[E, S, V])(_, _))

		override def toString :String = value.toString + " BETWEEN " + low + " AND " + high
	}


	object BetweenSQL {
		trait BetweenVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def between[S >: LocalScope <: GlobalScope, V](e :BetweenSQL[F, S, V]) :Y[S, Boolean]
		}
		type MatchBetween[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = BetweenVisitor[F, Y]
		type CaseBetween[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = BetweenVisitor[F, Y]
	}




	case class LikeSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope]
	                  (left :ColumnSQL[F, S, String], right :ColumnSQL[F, S, String])
		extends BinaryColumnOperator[F, S, String, Boolean] with ConditionSQL[F, S]
	{
		override def groundValue :Opt[Boolean] = Lack //todo:

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (left :SQLString[E, C], right :SQLString[E, C]) :SQLBoolean[E, C] =
			LikeSQL(left, right)

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
			visitor.like(this)

		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			left.inParens(context, params) + (" " + spelling.LIKE + " ") + (right.inParens(_, _))

		override def toString = s"$left LIKE $right"
	}



	object LikeSQL {
		trait LikeVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def like[S >: LocalScope <: GlobalScope](e :LikeSQL[F, S]) :Y[S, Boolean]
		}
		type MatchLike[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = LikeVisitor[F, Y]
		type CaseLike[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = LikeVisitor[F, Y]
	}




	case class ExistsSQL[-F <: RowProduct, V](select :SQLExpression[F, GlobalScope, Rows[V]])
		extends ConditionSQL[F, GlobalScope]
	{
		protected override def parts :Seq[SQLExpression[F, GlobalScope, _]] = select::Nil

		override def groundValue :Opt[Boolean] = select.groundValue.map(_.nonEmpty)

		override def isAnchored = true
		override def anchor(from :F) :SQLBoolean[F, GlobalScope] = this

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, GlobalScope, Boolean] =
			ExistsSQL(mapper(select))

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ColumnVisitor[F, Y]) :Y[GlobalScope, Boolean] =
			visitor.exists(this)

		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			(spelling.function("exists") + "(") +:
				(spelling(select :SQLExpression[E, GlobalScope, Rows[V]])(context, params) + ")")

		override def toString :String = s"EXISTS($select)"
	}


	object ExistsSQL {
		trait ExistsVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def exists[V](e :ExistsSQL[F, V]) :Y[GlobalScope, Boolean]
		}
		type MatchExists[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ExistsVisitor[F, Y]
		type CaseExists[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ExistsVisitor[F, Y]
	}




	case class InSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V]
	                (left :ColumnSQL[F, S, V], right :SQLExpression[F, S, Seq[V]])
		extends ConditionSQL[F, S]
	{
		protected override def parts :Seq[SQLExpression[F, S, _]] = left::right::Nil

		override def groundValue :Opt[Boolean] =
			for (l <- left.groundValue; r <- right.groundValue) yield r.contains(l)

		override def isAnchored :Boolean = left.isAnchored && right.isAnchored

		override def anchor(from :F) :SQLBoolean[F, S] = (left.anchor(from), right.anchor(from)) match {
			case (l, r) if (l eq left) && (r eq right) => this
			case (l, r) => InSQL(l, r)
		}

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
			InSQL(mapper(left), mapper(right))

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
			visitor.in(this)

		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			left.inParens(context, params) + (" " + spelling.keyword("IN") + " (") +
				(spelling(right :SQLExpression[E, S, Seq[V]])(_, _)) + ")"

		override def toString :String = s"($left IN $right)"
	}


	object InSQL {
		trait InVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def in[S >: LocalScope <: GlobalScope, V](e :InSQL[F, S, V]) :Y[S, Boolean]
		}
		type MatchIn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = InVisitor[F, Y]
		type CaseIn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = InVisitor[F, Y]
	}






	trait ConditionVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends IsNullVisitor[F, Y] with ComparisonVisitor[F, Y] with BetweenVisitor[F, Y] with LikeVisitor[F, Y]
		   with ExistsVisitor[F, Y] with InVisitor[F, Y]
	{
		def condition[S >: LocalScope <: GlobalScope](e :ConditionSQL[F, S]) :Y[S, Boolean]
	}

	trait MatchCondition[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ConditionVisitor[F, Y]
		with CaseIsNull[F, Y] with CaseComparison[F, Y] with CaseBetween[F, Y] with CaseLike[F, Y]
		with CaseExists[F, Y] with CaseIn[F, Y]

	trait CaseCondition[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchCondition[F, Y] {

		override def comparison[S >: LocalScope <: GlobalScope, X](e :ComparisonSQL[F, S, X]) :Y[S, Boolean] =
			condition(e)

		override def isNull[S >: LocalScope <: GlobalScope, T](e :IsNull[F, S, T]) :Y[S, Boolean] = condition(e)

		override def between[S >: LocalScope <: GlobalScope, V](e :BetweenSQL[F, S, V]) :Y[S, Boolean] = condition(e)

		override def like[S >: LocalScope <: GlobalScope](e :LikeSQL[F, S]) :Y[S, Boolean] = condition(e)

		override def in[S >: LocalScope <: GlobalScope, V](e :InSQL[F, S, V]) :Y[S, Boolean] = condition(e)

		override def exists[V](e :ExistsSQL[F, V]) :Y[GlobalScope, Boolean] = condition(e)
	}

}


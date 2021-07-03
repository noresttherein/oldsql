package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.ComparisonSQL.ComparisonOperator
import net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryOperatorSQL
import net.noresttherein.oldsql.sql.ast.EqualitySQL.EqualityVisitor
import net.noresttherein.oldsql.sql.ast.InequalitySQL.InequalityVisitor
import net.noresttherein.oldsql.sql.ast.OrderComparisonSQL.OrderComparisonVisitor
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLOrdering, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






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
	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val l = spelling(left)(from, context, params) + (" " + spelling.operator(symbol) + " ")
		l + spelling(right)(from, l.context, params)
	}


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


	class ComparisonOperator private[ComparisonSQL] (val symbol :String, cmpResult :Int, inverse :Boolean)
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




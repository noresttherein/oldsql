package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{MismatchedExpressionsException, MisspelledSQLException}
import net.noresttherein.oldsql.slang.mappingMethods
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ComparisonSQL.{EQ, GT, GTE, LT, LTE, NEQ, NULL_GT, NULL_GTE, NULL_LT, NULL_LTE, NULL_NEQ, OrderingOperator}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryCompositeSQL
import net.noresttherein.oldsql.sql.ast.EqualitySQL.{AnyEqualityVisitor, SpecificEqualityVisitor}
import net.noresttherein.oldsql.sql.ast.InequalitySQL.{AnyInequalityVisitor, SpecificInequalityVisitor}
import net.noresttherein.oldsql.sql.ast.NullEqualitySQL.{AnyNullEqualityVisitor, SpecificNullEqualityVisitor}
import net.noresttherein.oldsql.sql.ast.NullInequalitySQL.{AnyNullInequalityVisitor, SpecificNullInequalityVisitor}
import net.noresttherein.oldsql.sql.ast.NullOrderComparisonSQL.{AnyNullOrderComparisonVisitor, SpecificNullOrderComparisonVisitor}
import net.noresttherein.oldsql.sql.ast.OrderComparisonSQL.{AnyOrderComparisonVisitor, SpecificOrderComparisonVisitor}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLConversion, SQLOrdering, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






trait ComparisonSQL[-F <: RowProduct, -S >: Grouped <: Single, T]
	extends BinaryCompositeSQL[F, S, T, T, Boolean] with ConditionSQL[F, S]
{
	def operator :OrderingOperator
	def symbol :String = operator.symbol

	//overridden to return a ColumnSQL
	override def anchor(from :F) :ColumnSQL[F, S, Boolean] =
		(left.anchor(from), right.anchor(from)) match {
			case (l, r) if (l eq left) && (r eq right) => this
			case (l, r) => reapply(l, r)
		}

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
		reapply(mapper(left), mapper(right))

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T]


//		override def visit[Y[-_ >: Grouped <: GlobalScope, _]](matcher :AnyColumnVisitor[F, Y]) :Y[S, Boolean] =
//			matcher.comparison(this)
	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val spell = spelling.inOperand
		val l = spell(left)(from, context, params) + (" " + spelling.operator(symbol) + " ")
		l + spell(right)(from, l.context, params)
	}

	protected def columnSpelling[P](left :SpelledSQL[P], right :SpelledSQL[P])
	                               (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		columnSpelling(left, operator, right)

	protected def columnSpelling[P](left :SpelledSQL[P], operator :OrderingOperator, right :SpelledSQL[P])
	                               (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		left + (" " + spelling.operator(operator.symbol) + " ") + right

	protected def columnwiseSpelling[P](connective :String)
	                                   (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                   (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val cmpSpelling = spelling.inOperand
		val anchoredLeft = if (left.isAnchored(from)) left else left.anchor(from)
		val anchoredRight = if (right.isAnchored(from)) right else right.anchor(from)
		val (finalLeft, finalRight) =
			cmpSpelling.reform(anchoredLeft, anchoredRight)(SQLConversion.toSelf, SQLConversion.toSelf, cmpSpelling)
		val lefts = cmpSpelling.explode(left)(from, context, params)
		if (lefts.isEmpty)
			throw new MisspelledSQLException(
				"Spelling of expression " + left + " with " + spelling + " resulted in zero columns."
			)
		val rights = cmpSpelling.explode(right)(from, lefts.last.context, params)
//		val operatorSQL = " " + cmpSpelling.operator(symbol) + " "
		if (lefts.length == rights.length) {
			val columnComparisons = lefts.zipMap(rights)((l, r) => columnSpelling(l.inParens, r.inParens))
			(SpelledSQL.empty[P] /: columnComparisons) {
				(acc, colCmp) => if (acc.isEmpty) colCmp else acc + connective + colCmp
			}
		} else
			throw new MismatchedExpressionsException(
				"Failed to reform comparison " + this + " to compare matching column sets: final expressions " +
				finalLeft + " and " + finalRight + " resulted in columns " + lefts + " and " + rights + "."
			)
	}

	override def sameAs(that :CompositeSQL.__) :Boolean = that match {
		case cmp :ComparisonSQL[_, _, _] => cmp.operator == operator
		case _ => false
	}

	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :ComparisonSQL[_, _, _] if canEqual(other) && other.canEqual(this) =>
			operator == other.operator && left == other.left && right == other.right
		case _ => false
	}

	override def hashCode :Int = super[BinaryCompositeSQL].hashCode * 31 + operator.hashCode

	override def toString :String = left.toString + " " + operator + " " + right
}


object ComparisonSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, T :SQLOrdering]
	         (left :SQLExpression[F, S, T], cmp :OrderingOperator, right :SQLExpression[F, S, T])
			:ComparisonSQL[F, S, T] =
		cmp(left, right)

	def unapply[F <: RowProduct, S >: Grouped <: Single](e :SQLExpression[F, S, _])
			:Opt[(SQLExpression[F, S, T], OrderingOperator, SQLExpression[F, S, T]) forSome { type T }] =
		e match {
			case cmp :ComparisonSQL[F @unchecked, S @unchecked, t] =>
				Got((cmp.left, cmp.operator, cmp.right))
			case _ => Lack
		}


	class OrderingOperator protected (val symbol :String, cmpResult :Int, inverse :Boolean = false)
		extends Serializable
	{
		/** True for operators which treat all `null` values as equal. Created
		  * [[net.noresttherein.oldsql.sql.ast.ComparisonSQL ComparisonSQL]] includes additional conditions.
		  */
		def areNullsEqual :Boolean = false

		/** True for equality comparisons, including nullable equality
		  * [[net.noresttherein.oldsql.sql.ast.ComparisonSQL.EQ EQ]] and
		  * [[net.noresttherein.oldsql.sql.ast.ComparisonSQL.NULL_EQ NULL_EQ]]).
		  */
		def isEquality :Boolean = symbol == "="

		/** True for inequality comparisons, including nullable inequality
		  * [[net.noresttherein.oldsql.sql.ast.ComparisonSQL.NEQ NEQ]] and
		  * [[net.noresttherein.oldsql.sql.ast.ComparisonSQL.NULL_NEQ NULL_NEQ]]).
		  */
		def isInequality :Boolean = symbol == "<>"

		/** Strictly less than or strictly more than. Equality and inequality return `false`. */
		def isStrict    :Boolean = !inverse && cmpResult != 0 //inverse == false && cmpResult == 0 is inequality

		/** Less than or equal/greater than or equal. Equality and inequality return `false` */
		def isNonStrict :Boolean = inverse && cmpResult != 0

		/** Returns the strict variant of this operator. Strict orderings return themselves, non strict orderings
		  * drop the equality, while equality and inequality throw an [[UnsupportedOperationException]].
		  */
		def strict :OrderingOperator = symbol match {
			case "<" | ">" => this
			case "<=" => GT
			case ">=" => LT
			case _ =>
				throw new UnsupportedOperationException("Operator " + symbol + " does not have a strict version.")
		}

		/** Returns the non-strict variant of this operator. Strict orderings drop the equality sign,
		  * non strict orderings return themselves,
		  * while equality and inequality throw an [[UnsupportedOperationException]].
		  */
		def nonStrict :OrderingOperator = symbol match {
			case "<=" | ">=" => this
			case "<" => LTE
			case ">" => GTE
			case _ =>
				throw new UnsupportedOperationException("Operator " + symbol + " does not have a non strict version.")
		}

//		private def comparisonMatchAny :Int = cmpResult

		def apply[T](left :T, right :T)(implicit ordering :SQLOrdering[T]) :Boolean =
			(ordering.compare(left, right).sign == cmpResult) != inverse

		def apply[F <: RowProduct, S >: Grouped <: Single, T :SQLOrdering]
		         (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T]) :ComparisonSQL[F, S, T] =
			symbol match {
				case EQ.symbol  => EqualitySQL(denullify(left), denullify(right))
				case NEQ.symbol => InequalitySQL(denullify(left), denullify(right))
				case _ => OrderComparisonSQL(denullify(left), this, denullify(right))
			}


		def unapply[F <: RowProduct, S >: Grouped <: Single](e :SQLExpression[F, S, _])
				:Opt[(SQLExpression[F, S, T], SQLExpression[F, S, T]) forSome { type T }] =
			e match {
				case compare :ComparisonSQL[F @unchecked, S @unchecked, t] if compare.operator == this =>
					Got((compare.left, compare.right))
				case _ => Lack
			}

		def canEqual(other :Any) :Boolean = other.isInstanceOf[OrderingOperator]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :OrderingOperator if other canEqual this => symbol == other.symbol
			case _ => false
		}
		override def hashCode :Int = symbol.hashCode

		override def toString :String = symbol
	}

	private object OrderingOperator {
		def apply(symbol :String, cmpResult :Int, inverse :Boolean) :OrderingOperator =
			new OrderingOperator(symbol, cmpResult, inverse)
	}


	class NullableOrderingOperator protected (override val symbol :String, cmpResult :Int, inverse :Boolean)
		extends OrderingOperator(symbol, cmpResult, inverse)
	{
		override def areNullsEqual = true

		override def strict :OrderingOperator = symbol match {
			case "<" | ">" => this
			case "<=" => NULL_GT
			case ">=" => NULL_LT
			case _ =>
				throw new UnsupportedOperationException("Operator " + symbol + " does not have a strict version.")
		}
		override def nonStrict :OrderingOperator = symbol match {
			case "<=" | ">=" => this
			case "<" => NULL_LTE
			case ">" => NULL_GTE
			case _ =>
				throw new UnsupportedOperationException("Operator " + symbol + " does not have a non strict version.")
		}

		override def apply[T](left :T, right :T)(implicit ordering :SQLOrdering[T]) :Boolean =
			if (left == null & right == null) !isStrict || isEquality
			else if (left == null | right == null) false
			else super.apply(left, right)

		override def apply[F <: RowProduct, S >: Grouped <: Single, T :SQLOrdering]
		         (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T]) :ComparisonSQL[F, S, T] =
			symbol match {
				case EQ.symbol  => NullEqualitySQL(denullify(left), denullify(right))
				case NEQ.symbol => NullInequalitySQL(denullify(left), denullify(right))
				case _ => NullOrderComparisonSQL(denullify(left), this, denullify(right))
			}

		override def canEqual(other :Any) :Boolean = other.isInstanceOf[NullableOrderingOperator]
	}

	private object NullableOrderingOperator {
		def apply(symbol :String, cmpResult :Int, inverse :Boolean) :NullableOrderingOperator =
			new NullableOrderingOperator(symbol, cmpResult, inverse)
	}


	//consider: renaming them to <, <=, >, >=, ==, <>
	final val LT  = OrderingOperator("<", -1, false)
	final val LTE = OrderingOperator("<=", 1, true)
	final val GT  = OrderingOperator(">", 1, false)
	final val GTE = OrderingOperator(">=", -1, true)
	final val EQ  = OrderingOperator("=", 0, false)
	final val NEQ = OrderingOperator("<>", 0, true)
	//consider: special variants handling nulls

	final val NULL_LT  :OrderingOperator = NullableOrderingOperator("<", -1, false)
	final val NULL_LTE :OrderingOperator = NullableOrderingOperator("<=", 1, true)
	final val NULL_GT  :OrderingOperator = NullableOrderingOperator(">", 1, false)
	final val NULL_GTE :OrderingOperator = NullableOrderingOperator(">=", -1, true)
	final val NULL_EQ  :OrderingOperator = NullableOrderingOperator("=", 0, false)
	final val NULL_NEQ :OrderingOperator = NullableOrderingOperator("<>", 0, true)

	trait SpecificComparisonVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificOrderComparisonVisitor[F, S, X, Y] with SpecificNullOrderComparisonVisitor[F, S, X, Y]
		   with SpecificEqualityVisitor[F, S, X, Y] with SpecificNullEqualityVisitor[F, S, X, Y]
		   with SpecificInequalityVisitor[F, S, X, Y] with SpecificNullInequalityVisitor[F, S, X, Y]
	{
		def comparison[V](e :ComparisonSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificComparison[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificComparisonVisitor[F, S, X, Y]

	trait CaseSpecificComparison[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificComparison[F, S, X, Y]
	{
		override def equality[V](e :EqualitySQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y = comparison(e)
		override def inequality[V](e :InequalitySQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y = comparison(e)
		override def order[V](e :OrderComparisonSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y = comparison(e)
		override def nullEquality[V](e :NullEqualitySQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y = comparison(e)
		override def nullInequality[V](e :NullInequalitySQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y =
			comparison(e)
		override def nullOrder[V](e :NullOrderComparisonSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y =
			comparison(e)
	}
//
//
//	trait ComparisonVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends OrderComparisonVisitor[F, R] with EqualityVisitor[F, R] with InequalityVisitor[F, R]
//	{
//		def comparison[S >: Grouped <: Single, N]
//		              (e :ComparisonSQL[F, S, N]) :R[S, Boolean, ComparisonSQL[F, S, N]]
//	}
//	type MatchComparison[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		ComparisonVisitor[F, R]
//
//	trait CaseComparison[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends MatchComparison[F, R]
//	{
//		override def inequality[S >: Grouped <: Single, N]
//		                       (e :InequalitySQL[F, S, N]) :R[S, Boolean, InequalitySQL[F, S, N]] = comparison(e)
//
//		override def order[S >: Grouped <: Single, N]
//		                  (e :OrderComparisonSQL[F, S, N]) :R[S, Boolean, OrderComparisonSQL[F, S, N]] = comparison(e)
//
//		override def equality[S >: Grouped <: Single, N]
//		                     (e :EqualitySQL[F, S, N]) :R[S, Boolean, EqualitySQL[F, S, N]] = comparison(e)
//	}


	trait AnyComparisonVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyOrderComparisonVisitor[F, Y] with AnyNullOrderComparisonVisitor[F, Y]
		   with AnyEqualityVisitor[F, Y] with AnyNullEqualityVisitor[F, Y]
		   with AnyInequalityVisitor[F, Y] with AnyNullInequalityVisitor[F, Y]
	{
		def comparison[S >: Grouped <: Single, X](e :ComparisonSQL[F, S, X]) :Y[S, Boolean]
	}
	type MatchAnyComparison[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyComparisonVisitor[F, Y]

	trait CaseAnyComparison[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyComparison[F, Y] {
		override def equality[S >: Grouped <: Single, X](e :EqualitySQL[F, S, X]) :Y[S, Boolean] = comparison(e)
		override def inequality[S >: Grouped <: Single, X](e :InequalitySQL[F, S, X]) :Y[S, Boolean] = comparison(e)
		override def order[S >: Grouped <: Single, X](e :OrderComparisonSQL[F, S, X]) :Y[S, Boolean] = comparison(e)
		override def nullEquality[S >: Grouped <: Single, X](e :NullEqualitySQL[F, S, X]) :Y[S, Boolean] = comparison(e)
		override def nullInequality[S >: Grouped <: Single, X](e :NullInequalitySQL[F, S, X]) :Y[S, Boolean] =
			comparison(e)
		override def nullOrder[S >: Grouped <: Single, X](e :NullOrderComparisonSQL[F, S, X]) :Y[S, Boolean] =
			comparison(e)
	}
}




class OrderComparisonSQL[-F <: RowProduct, -S >: Grouped <: Single, T]
                        (override val left :SQLExpression[F, S, T], override val operator :OrderingOperator,
                         override val right :SQLExpression[F, S, T])
                        (implicit val ordering :SQLOrdering[T])
	extends ComparisonSQL[F, S, T]
{
	override def groundValue :Opt[Boolean] =
		for (l <- left.groundValue; r <- right.groundValue) yield operator(l, r)

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T] =
		new OrderComparisonSQL(left, operator, right)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		operator match {
			case LT | LTE | GT | GTE | NULL_LT | NULL_LTE | NULL_GT | NULL_GTE =>
				val cmpSpelling = spelling.inOperand
				val anchoredLeft = if (left.isAnchored(from)) left else left.anchor(from)
				val anchoredRight = if (right.isAnchored(from)) right else right.anchor(from)
				val (finalLeft, finalRight) =
					cmpSpelling.reform(anchoredLeft, anchoredRight)(
						SQLConversion.toSelf, SQLConversion.toSelf, cmpSpelling
					)
				val lefts = cmpSpelling.explode(left)(from, context, params)
				if (lefts.isEmpty) //or we could render as TRUE and produce a warning
					throw new MisspelledSQLException(
						"Spelling of expression " + left + " with " + spelling + " resulted in zero columns"
					)
				//we modify context out of order, but currently there is nothing that can go wrong
				val rights = cmpSpelling.explode(right)(from, lefts.last.context, params)
//				val eqSQL = " " + cmpSpelling.operator(EQ.symbol) + " "
//				val cmpSQL = " " + cmpSpelling.operator(operator.symbol) + " "
				if (lefts.length != rights.length)
					throw new MismatchedExpressionsException(
						"Failed to reform comparison " + this + " to compare matching column sets: final expressions " +
						finalLeft + " and " + finalRight + " resulted in columns " + lefts + " and " + rights + "."
					)
				val columnPairs = lefts zip rights
//				val prefixes = columnPairs.view.init.map { //conjunctions of equalities between first 0,...,n-1 pairs
//					case (l, r) => l.inParens + eqSQL + r.inParens
//				}.scanLeft(SpelledSQL.empty[P]) {
//					(prefix, pair) => prefix + cmpSpelling._AND_ + pair
//				}
				val prefixes = columnPairs.view.init.map {
					case (l, r) => columnSpelling(l.inParens, EQ, r.inParens)
				}.scanLeft(SpelledSQL.empty[P]) {
					(prefix, pair) => prefix + cmpSpelling._AND_ + pair
				}
				(prefixes zip columnPairs).map { case (eqs, (l, r)) => //slap inequality behind the following pair at the end
					if (eqs.isEmpty) columnSpelling(l.inParens, r.inParens)
					else eqs + cmpSpelling._AND_ + columnSpelling(l.inParens, r.inParens)
				}.reduce(_ + cmpSpelling._OR_ + _)
			case NEQ | NULL_NEQ =>
				columnwiseSpelling(spelling._OR_)(from, context, params)
			case _ => //EQ and anything I forget in the future
				columnwiseSpelling(spelling._AND_)(from, context, params)
		}


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.order[S, T](this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.order(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.order(this)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :OrderComparisonSQL[_, _, _] if other canEqual this =>
			other.operator == operator && other.left == left && other.right == right && other.ordering == ordering
		case _ => false
	}

	override def hashCode :Int =
		((operator.hashCode * 31 + left.hashCode) * 31 + right.hashCode) * 31 + ordering.hashCode
}


object OrderComparisonSQL {
	def apply[F <: RowProduct, S >: Grouped <: Single, T :SQLOrdering]
	         (left :SQLExpression[F, S, T], cmp :OrderingOperator, right :SQLExpression[F, S, T]) :ComparisonSQL[F, S, T] =
		new OrderComparisonSQL(left, cmp, right)

	def unapply[F <: RowProduct, S >: Grouped <: Single](e :SQLExpression[F, S, _])
			:Opt[(SQLExpression[F, S, T], OrderingOperator, SQLExpression[F, S, T]) forSome { type T }] =
		e match { //consider: maybe it shouldn't match NullOrderComparisonSQL?
			case cmp :OrderComparisonSQL[F @unchecked, S @unchecked, t] =>
				Got((cmp.left, cmp.operator, cmp.right))
			case _ => Lack
		}


	trait SpecificOrderComparisonVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def order[V](e :OrderComparisonSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificOrderComparison[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificOrderComparisonVisitor[F, S, X, Y]
	type CaseSpecificOrderComparison[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificOrderComparisonVisitor[F, S, X, Y]
//
//	trait OrderComparisonVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def order[S >: Grouped <: Single, N]
//		         (e :OrderComparisonSQL[F, S, N]) :R[S, Boolean, OrderComparisonSQL[F, S, N]]
//	}
//	type MatchOrderComparison[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		OrderComparisonVisitor[F, R]
//	type CaseOrderComparison[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		OrderComparisonVisitor[F, R]

	trait AnyOrderComparisonVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def order[S >: Grouped <: Single, X](e :OrderComparisonSQL[F, S, X]) :Y[S, Boolean]
	}
	type MatchAnyOrderComparison[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyOrderComparisonVisitor[F, Y]
	type CaseAnyOrderComparison[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyOrderComparisonVisitor[F, Y]
}




case class EqualitySQL[-F <: RowProduct, -S >: Grouped <: Single, T]
                      (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T])
	extends ComparisonSQL[F, S, T]
{
	override def operator :OrderingOperator = ComparisonSQL.EQ

	override def groundValue :Opt[Boolean] =
		for (l <- left.groundValue; r <- right.groundValue) yield l == r

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T] =
		new EqualitySQL(left, right)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		columnwiseSpelling(spelling._AND_)(from, context, params)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.equality(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.equality(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.equality(this)
}


object EqualitySQL {
	trait SpecificEqualityVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def equality[V](e :EqualitySQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificEquality[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificEqualityVisitor[F, S, X, Y]
	type CaseSpecificEquality[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificEqualityVisitor[F, S, X, Y]
//
//	trait EqualityVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def equality[S >: Grouped <: Single, N](e :EqualitySQL[F, S, N]) :R[S, Boolean, EqualitySQL[F, S, N]]
//	}
//	type MatchEqualaity[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		EqualityVisitor[F, R]
//	type CaseEqualaity[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		EqualityVisitor[F, R]

	trait AnyEqualityVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def equality[S >: Grouped <: Single, X](e :EqualitySQL[F, S, X]) :Y[S, Boolean]
	}
	type MatchAnyEquality[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyEqualityVisitor[F, Y]
	type CaseAnyEquality[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyEqualityVisitor[F, Y]
}




case class InequalitySQL[-F <: RowProduct, -S >: Grouped <: Single, T]
                        (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T])
	extends ComparisonSQL[F, S, T]
{
	override def operator :OrderingOperator = ComparisonSQL.NEQ

	override def groundValue :Opt[Boolean] =
		for (l <- left.groundValue; r <- right.groundValue) yield l != r

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T])
			:ComparisonSQL[E, C, T] =
		new InequalitySQL(left, right)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		columnwiseSpelling(spelling._OR_)(from, context, params)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.inequality(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.inequality(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.inequality(this)
}


object InequalitySQL {
	trait SpecificInequalityVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def inequality[V](e :InequalitySQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificInequality[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] = SpecificInequalityVisitor[F, S, V, Y]
	type CaseSpecificInequality[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] = SpecificInequalityVisitor[F, S, V, Y]
//
//	trait InequalityVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def inequality[S >: Grouped <: Single, N]
//		              (e :InequalitySQL[F, S, N]) :R[S, Boolean, InequalitySQL[F, S, N]]
//	}
//	type MatchInequality[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		InequalitlyVisitor[F, R]
//	type CaseInequality[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		InequalitlyVisitor[F, R]

	trait AnyInequalityVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def inequality[S >: Grouped <: Single, X](e :InequalitySQL[F, S, X]) :Y[S, Boolean]
	}
	type MatchAnyInequality[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyInequalityVisitor[F, Y]
	type CaseAnyInequality[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyInequalityVisitor[F, Y]
}




/** An SQL expression testing if two expressions are either equal or both `NULL`. */
case class NullEqualitySQL[-F <: RowProduct, -S >: Grouped <: Single, T]
                          (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T])
	extends ComparisonSQL[F, S, T]
{
	override def operator :OrderingOperator = ComparisonSQL.NULL_EQ

	override def groundValue :Opt[Boolean] =
		for (l <- left.groundValue; r <- right.groundValue) yield l == r

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T] =
		new EqualitySQL(left, right)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		columnwiseSpelling(spelling._AND_)(from, context, params)

	protected override def columnSpelling[P](left :SpelledSQL[P], right :SpelledSQL[P])
	                                        (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		NullEqualitySQL.spell(left, right)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.nullEquality(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.nullEquality(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.equality(this)
}


object NullEqualitySQL {

	private[ast] def spell[P](left :SpelledSQL[P], right :SpelledSQL[P])(implicit spelling :SQLSpelling) :SpelledSQL[P] =
		"(" +: (left + " = " + right + spelling._OR_ +
			left + (spelling._IS_ + spelling.NULL + spelling._AND_) + right + (spelling._IS_ + spelling.NULL + ")"))


	trait SpecificNullEqualityVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def nullEquality[V](e :NullEqualitySQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificNullEquality[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificNullEqualityVisitor[F, S, X, Y]
	type CaseSpecificNullEquality[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificNullEqualityVisitor[F, S, X, Y]
//
//	trait EqualityVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def equality[S >: Grouped <: Single, N](e :EqualitySQL[F, S, N]) :R[S, Boolean, EqualitySQL[F, S, N]]
//	}
//	type MatchEqualaity[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		EqualityVisitor[F, R]
//	type CaseEqualaity[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		EqualityVisitor[F, R]

	trait AnyNullEqualityVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def nullEquality[S >: Grouped <: Single, X](e :NullEqualitySQL[F, S, X]) :Y[S, Boolean]
	}
	type MatchAnyNullEquality[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyNullEqualityVisitor[F, Y]
	type CaseAnyNullEquality[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyNullEqualityVisitor[F, Y]
}




case class NullInequalitySQL[-F <: RowProduct, -S >: Grouped <: Single, T]
                            (left :SQLExpression[F, S, T], right :SQLExpression[F, S, T])
	extends ComparisonSQL[F, S, T]
{
	override def operator :OrderingOperator = ComparisonSQL.NULL_NEQ

	override def groundValue :Opt[Boolean] =
		for (l <- left.groundValue; r <- right.groundValue) yield l != r

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T])
			:ComparisonSQL[E, C, T] =
		new NullInequalitySQL(left, right)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		columnwiseSpelling(spelling._OR_)(from, context, params)

	protected override def columnSpelling[P](left :SpelledSQL[P], right :SpelledSQL[P])
	                                        (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		NullInequalitySQL.spell(left, right)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.nullInequality(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.nullInequality(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.inequality(this)
}


object NullInequalitySQL {
	private[ast] def spell[P](left :SpelledSQL[P], right :SpelledSQL[P])
	                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		"(" +: (left + " <> " + right + spelling._OR_ +
			left + (spelling._IS_ + spelling.NULL + spelling._AND_ +
				spelling.NOT + " ") + right + (spelling._IS_ + spelling.NULL + spelling._OR_ +
			spelling.NOT + " ") + left + (spelling._IS_ + spelling.NULL + spelling._AND_) +
				right + (spelling._IS_ + spelling.NULL + ")"))

	trait SpecificNullInequalityVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def nullInequality[V](e :NullInequalitySQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificNullInequality[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
		SpecificNullInequalityVisitor[F, S, V, Y]
	type CaseSpecificNullInequality[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
		SpecificNullInequalityVisitor[F, S, V, Y]
//
//	trait InequalityVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def inequality[S >: Grouped <: Single, N]
//		              (e :InequalitySQL[F, S, N]) :R[S, Boolean, InequalitySQL[F, S, N]]
//	}
//	type MatchInequality[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		InequalitlyVisitor[F, R]
//	type CaseInequality[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		InequalitlyVisitor[F, R]

	trait AnyNullInequalityVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def nullInequality[S >: Grouped <: Single, X](e :NullInequalitySQL[F, S, X]) :Y[S, Boolean]
	}
	type MatchAnyNullInequality[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyNullInequalityVisitor[F, Y]
	type CaseAnyNullInequality[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyNullInequalityVisitor[F, Y]
}




/** Compares two values, using alphabetic ordering of their column sets and treating two `null` values as equal.
  * Non strict less than/greater than comparisons always return false if any of the operands is null,
  * but strict operators treat two nulls as equal.
  *
  * Note that this class extends `OrderComparisonSQL`, but visitors delegate its case always directly
  * to [[net.noresttherein.oldsql.sql.ast.ComparisonSQL ComparisonSQL]].
  */
class NullOrderComparisonSQL[-F <: RowProduct, -S >: Grouped <: Single, T]
                            (override val left :SQLExpression[F, S, T], override val operator :OrderingOperator,
                             override val right :SQLExpression[F, S, T])
                            (implicit ordering :SQLOrdering[T])
	extends OrderComparisonSQL[F, S, T](left, operator, right)
{
	override def groundValue :Opt[Boolean] =
		for (l <- left.groundValue; r <- right.groundValue) yield
			if (operator.isStrict)  l != null & r != null && l != r && operator(l, r)
			else l == r || l != null & r != null && operator(l, r)

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, T], right :SQLExpression[E, C, T]) :ComparisonSQL[E, C, T] =
		new NullOrderComparisonSQL(left, operator, right)

	protected override def columnSpelling[P](left :SpelledSQL[P], operator :OrderingOperator, right :SpelledSQL[P])
	                                        (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if (operator.isEquality)
			NullEqualitySQL.spell(left, right)
		else if (operator.isInequality) //never occurs, but better safe than sorry
			NullInequalitySQL.spell(left, right)
		else if (operator.isStrict) //nulls will automatically compare unequal in SQL
			super.columnSpelling(left, operator, right)
		else
			(NullEqualitySQL.spell(left, right) + spelling._OR_ + super.columnSpelling(left, operator, right)).inParens

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.nullOrder(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.nullOrder(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.order(this)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :OrderComparisonSQL[_, _, _] if other canEqual this =>
			other.operator == operator && other.left == left && other.right == right && other.ordering == ordering
		case _ => false
	}

	override def hashCode :Int =
		((operator.hashCode * 31 + left.hashCode) * 31 + right.hashCode) * 31 + ordering.hashCode
}


object NullOrderComparisonSQL {
	def apply[F <: RowProduct, S >: Grouped <: Single, T :SQLOrdering]
	         (left :SQLExpression[F, S, T], cmp :OrderingOperator, right :SQLExpression[F, S, T]) :ComparisonSQL[F, S, T]=
		new NullOrderComparisonSQL(left, cmp, right)

	def unapply[F <: RowProduct, S >: Grouped <: Single](e :SQLExpression[F, S, _])
			:Opt[(SQLExpression[F, S, T], OrderingOperator, SQLExpression[F, S, T]) forSome { type T }] =
		e match {
			case cmp :NullOrderComparisonSQL[F @unchecked, S @unchecked, t] =>
				Got((cmp.left, cmp.operator, cmp.right))
			case _ => Lack
		}


	trait SpecificNullOrderComparisonVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def nullOrder[V](e :NullOrderComparisonSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificNullOrderComparison[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificNullOrderComparisonVisitor[F, S, X, Y]
	type CaseSpecificNullOrderComparison[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificNullOrderComparisonVisitor[F, S, X, Y]

	trait AnyNullOrderComparisonVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def nullOrder[S >: Grouped <: Single, X](e :NullOrderComparisonSQL[F, S, X]) :Y[S, Boolean]
	}
	type MatchAnyNullOrderComparison[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyNullOrderComparisonVisitor[F, Y]
	type CaseAnyNullOrderComparison[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyNullOrderComparisonVisitor[F, Y]
}







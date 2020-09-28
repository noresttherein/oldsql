package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.sql.ArithmeticSQL.BinaryOperationSQL.{BinaryOperation, BinaryOperationMatcher}
import net.noresttherein.oldsql.sql.ArithmeticSQL.UnaryOperationSQL.{UnaryOperation, UnaryOperationMatcher}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, GlobalScope, LocalScope}



/**
  * @author Marcin Mościcki
  */
trait ArithmeticSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, V] extends CompositeColumnSQL[F, S, V]




object ArithmeticSQL {

	class UnaryOperationSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, V]
	                       (val operation :UnaryOperation, val value :ColumnSQL[F, S, V])
	                       (implicit val arithmetic :SQLNumber[V])
		extends ArithmeticSQL[F, S, V]
	{
		override def readForm :ColumnReadForm[V] = value.readForm

		protected override def parts :Seq[ColumnSQL[F, S, V]] = value::Nil

		override def rephrase[E <: FromClause](scribe :SQLScribe[F, E]) :UnaryOperationSQL[E, S, V] =
			new UnaryOperationSQL(operation, scribe(value))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, V] =
			matcher.unaryArithmetic(this)

		override def sameAs(other :CompositeSQL[_, _, _]) :Boolean = other match {
			case op :UnaryOperationSQL[_, _, _] => op.operation == operation
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :UnaryOperationSQL[_, _, _] if other canEqual this =>
				other.operation == operation && other.value == value && other.arithmetic == arithmetic
			case _ => false
		}

		override def hashCode :Int = (operation.hashCode * 31 + value.hashCode) * 31 + arithmetic.hashCode

		override def toString :String = operation.toString + value
	}



	object UnaryOperationSQL {

		def apply[F <: FromClause, S >: LocalScope <: GlobalScope, V :SQLNumber]
		         (op :UnaryOperation, value :ColumnSQL[F, S, V]) :UnaryOperationSQL[F, S, V] =
			new UnaryOperationSQL(op, value)

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope, V]
		           (e :SQLExpression[F, S, V]) :Option[(UnaryOperation, ColumnSQL[F, S, V])] =
			e match {
				case op :UnaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] =>
					Some((op.operation, op.value))
				case _ => None
			}


		class UnaryOperation(val symbol :String) extends AnyVal {

			def apply[F <: FromClause, S >: LocalScope <: GlobalScope, V :SQLNumber]
			         (value :ColumnSQL[F, S, V]) :UnaryOperationSQL[F, S, V] =
				new UnaryOperationSQL(this, value)

			def unapply[F <: FromClause, S >: LocalScope <: GlobalScope, V]
			           (e :SQLExpression[F, S, V]) :Option[ColumnSQL[F, S, V]] =
				e match {
					case op :UnaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] if op.operation == this =>
						Some(op.value)
					case _ => None
				}
		}

		final val Neg = new UnaryOperation("-")


		trait UnaryOperationMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def unaryArithmetic[S >: LocalScope <: GlobalScope, V](e :UnaryOperationSQL[F, S, V]) :Y[S, V]
		}

		type MatchUnaryOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = UnaryOperationMatcher[F, Y]

		type CaseUnaryOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = UnaryOperationMatcher[F, Y]

	}






	class BinaryOperationSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, V]
	                  (val left :ColumnSQL[F, S, V], val operation :BinaryOperation, val right :ColumnSQL[F, S, V])
	                  (implicit val arithmetic :SQLNumber[V])
		extends ArithmeticSQL[F, S, V]
	{
		override def readForm :ColumnReadForm[V] = left.readForm

		protected override def parts :Seq[ColumnSQL[F, S, V]] = left::right::Nil

		override def rephrase[E <: FromClause](scribe :SQLScribe[F, E]) :BinaryOperationSQL[E, S, V] =
			new BinaryOperationSQL(scribe(left), operation, scribe(right))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, V] =
			matcher.binaryArithmetic(this)


		override def sameAs(other :CompositeSQL[_, _, _]) :Boolean = other match {
			case op :BinaryOperationSQL[_, _, _] => op.operation == operation
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case op :BinaryOperationSQL[_, _, _] if op canEqual this =>
				op.operation == operation && op.left == left && op.right == right && op.arithmetic == arithmetic
			case _ => false
		}

		override def hashCode :Int =
			((operation.hashCode * 31 + left.hashCode) * 31 + right.hashCode) * 31 + arithmetic.hashCode

		override def toString :String = "(" + left + " " + operation + " " + right + ")"

	}



	object BinaryOperationSQL {

		def apply[F <: FromClause, S >: LocalScope <: GlobalScope, V :SQLNumber]
		         (left :ColumnSQL[F, S, V], operation :BinaryOperation, right :ColumnSQL[F, S, V])
				:BinaryOperationSQL[F, S, V] =
			new BinaryOperationSQL(left, operation, right)

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V])
				:Option[(ColumnSQL[F, S, V], BinaryOperation, ColumnSQL[F, S, V])] =
			e match {
				case op :BinaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] =>
					Some((op.left, op.operation, op.right))
				case  _ => None
			}


		class BinaryOperation private[BinaryOperationSQL](val symbol :String) extends AnyVal {

			def apply[F <: FromClause, S >: LocalScope <: GlobalScope, V :SQLNumber]
			         (left :ColumnSQL[F, S, V], right :ColumnSQL[F, S, V]) :BinaryOperationSQL[F, S, V] =
				new BinaryOperationSQL(left, this, right)

			def unapply[F <: FromClause, S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V])
					:Option[(ColumnSQL[F, S, V], ColumnSQL[F, S, V])] =
				e match {
					case op :BinaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] if op.operation == this =>
						Some((op.left, op.right))
					case _ => None
				}
		}

		final val Plus = new BinaryOperation("+")
		final val Minus = new BinaryOperation("-")
		final val Times = new BinaryOperation("*")
		final val Divide = new BinaryOperation("/")
		final val Remainder = new BinaryOperation("%")


		trait BinaryOperationMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def binaryArithmetic[S >: LocalScope <: GlobalScope, V](e :BinaryOperationSQL[F, S, V]) :Y[S, V]
		}

		type MatchBinaryOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = BinaryOperationMatcher[F, Y]

//		trait MatchBinaryOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchBinaryOperation[F, Y] {
//			override def binaryArithmetic[S <: LocalScope, V](e :BinaryOperationSQL[F, S, V]) :Y[S, V] = e.operation match {
//				case Plus => plus(e.left, e.right)(e.arithmetic)
//				case Minus => minus(e.left, e.right)(e.arithmetic)
//				case Times => times(e.left, e.right)(e.arithmetic)
//				case Divide => divide(e.left, e.right)(e.arithmetic)
//				case Remainder => remainder(e.left, e.right)(e.arithmetic)
//				case error => throw new IllegalArgumentException("Unknown arithmetic operation: " + error)
//			}
//
//			def plus[S <: LocalScope, V :SQLNumber](left :ColumnSQL[F, S, V], right :ColumnSQL[F, S, V]) :Y[S, V]
//			def minus[S <: LocalScope, V :SQLNumber](left :ColumnSQL[F, S, V], right :ColumnSQL[F, S, V]) :Y[S, V]
//			def times[S <: LocalScope, V :SQLNumber](left :ColumnSQL[F, S, V], right :ColumnSQL[F, S, V]) :Y[S, V]
//			def divide[S <: LocalScope, V :SQLNumber](left :ColumnSQL[F, S, V], right :ColumnSQL[F, S, V]) :Y[S, V]
//			def remainder[S <: LocalScope, V :SQLNumber](left :ColumnSQL[F, S, V], right :ColumnSQL[F, S, V]) :Y[S, V]
//		}

		type CaseBinaryOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = BinaryOperationMatcher[F, Y]

	}






	trait ArithmeticMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends BinaryOperationMatcher[F, Y] with UnaryOperationMatcher[F, Y]

	type MatchArithmetic[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ArithmeticMatcher[F, Y]

	trait CaseArithmetic[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchArithmetic[F, Y] {
		def arithmetic[S >: LocalScope <: GlobalScope, V](e :ArithmeticSQL[F, S, V]) :Y[S, V]

		override def unaryArithmetic[S >: LocalScope <: GlobalScope, V](e :UnaryOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)

		override def binaryArithmetic[S >: LocalScope <: GlobalScope, V](e :BinaryOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)
	}
	
	
	
}

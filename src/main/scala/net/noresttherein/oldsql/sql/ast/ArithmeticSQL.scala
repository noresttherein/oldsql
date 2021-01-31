package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression, SQLNumber}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.{BinaryColumnOperator, UnaryColumnOperator}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.BinaryOperationSQL.{BinaryOperation, BinaryOperationMatcher}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.UnaryOperationSQL.{UnaryOperation, UnaryOperationMatcher}



/**
  * @author Marcin Mościcki
  */
trait ArithmeticSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V] extends CompositeColumnSQL[F, S, V] {
	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, V] =
		matcher.arithmetic(this)
}




object ArithmeticSQL {

	class UnaryOperationSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V]
	                       (val operation :UnaryOperation, override val value :ColumnSQL[F, S, V])
	                       (implicit val arithmetic :SQLNumber[V])
		extends UnaryColumnOperator[F, S, V, V] with ArithmeticSQL[F, S, V]
	{
		override def readForm :ColumnReadForm[V] = value.readForm

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
			new UnaryOperationSQL[E, C, V](operation, e)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, V] =
			matcher.unaryArithmetic(this)

		override def canEqual(that :Any) :Boolean = that match {
			case op :UnaryOperationSQL[_, _, _] => op.operation == operation
			case _ => false
		}

		override def hashCode :Int = (operation.hashCode * 31 + value.hashCode) * 31 + arithmetic.hashCode

		override def toString :String = operation.toString + value
	}



	object UnaryOperationSQL {

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, V :SQLNumber]
		         (op :UnaryOperation, value :ColumnSQL[F, S, V]) :UnaryOperationSQL[F, S, V] =
			new UnaryOperationSQL(op, value)

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, V]
		           (e :SQLExpression[F, S, V]) :Opt[(UnaryOperation, ColumnSQL[F, S, V])] =
			e match {
				case op :UnaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] =>
					Got((op.operation, op.value))
				case _ => Lack
			}


		class UnaryOperation(val symbol :String) extends AnyVal with Serializable {

			def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, V :SQLNumber]
			         (value :ColumnSQL[F, S, V]) :UnaryOperationSQL[F, S, V] =
				new UnaryOperationSQL(this, value)

			def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, V]
			           (e :SQLExpression[F, S, V]) :Opt[ColumnSQL[F, S, V]] =
				e match {
					case op :UnaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] if op.operation == this =>
						Got(op.value)
					case _ => Lack
				}
		}



		trait UnaryOperationMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def unaryArithmetic[S >: LocalScope <: GlobalScope, V](e :UnaryOperationSQL[F, S, V]) :Y[S, V]
		}

		type MatchUnaryOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = UnaryOperationMatcher[F, Y]

		type CaseUnaryOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = UnaryOperationMatcher[F, Y]

	}






	class BinaryOperationSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V]
	      (override val left :ColumnSQL[F, S, V], val operation :BinaryOperation, override val right :ColumnSQL[F, S, V])
	      (implicit val arithmetic :SQLNumber[V])
		extends BinaryColumnOperator[F, S, V, V] with ArithmeticSQL[F, S, V]
	{
		override def readForm :ColumnReadForm[V] = left.readForm

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (left :ColumnSQL[E, C, V], right :ColumnSQL[E, C, V]) :BinaryOperationSQL[E, C, V] =
			new BinaryOperationSQL(left, operation, right)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, V] =
			matcher.binaryArithmetic(this)


		override def canEqual(that :Any) :Boolean = that match {
			case op :BinaryOperationSQL[_, _, _] => op.operation == operation
			case _ => false
		}

		override def hashCode :Int =
			((operation.hashCode * 31 + left.hashCode) * 31 + right.hashCode) * 31 + arithmetic.hashCode

		override def toString :String = "(" + left + " " + operation + " " + right + ")"

	}



	object BinaryOperationSQL {

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, V :SQLNumber]
		         (left :ColumnSQL[F, S, V], operation :BinaryOperation, right :ColumnSQL[F, S, V])
				:BinaryOperationSQL[F, S, V] =
			new BinaryOperationSQL(left, operation, right)

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V])
				:Opt[(ColumnSQL[F, S, V], BinaryOperation, ColumnSQL[F, S, V])] =
			e match {
				case op :BinaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] =>
					Got((op.left, op.operation, op.right))
				case  _ => Lack
			}


		class BinaryOperation(val symbol :String) extends AnyVal with Serializable {

			def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, V :SQLNumber]
			         (left :ColumnSQL[F, S, V], right :ColumnSQL[F, S, V]) :BinaryOperationSQL[F, S, V] =
				new BinaryOperationSQL(left, this, right)

			def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V])
					:Opt[(ColumnSQL[F, S, V], ColumnSQL[F, S, V])] =
				e match {
					case op :BinaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] if op.operation == this =>
						Got((op.left, op.right))
					case _ => Lack
				}
		}



		trait BinaryOperationMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def binaryArithmetic[S >: LocalScope <: GlobalScope, V](e :BinaryOperationSQL[F, S, V]) :Y[S, V]
		}

		type MatchBinaryOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = BinaryOperationMatcher[F, Y]

//		trait MatchBinaryOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchBinaryOperation[F, Y] {
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

		type CaseBinaryOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = BinaryOperationMatcher[F, Y]

	}


	final val Neg = new UnaryOperation("-")
	final val Plus = new BinaryOperation("+")
	final val Minus = new BinaryOperation("-")
	final val Times = new BinaryOperation("*")
	final val Divide = new BinaryOperation("/")
	final val Remainder = new BinaryOperation("%")






	trait ArithmeticMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends BinaryOperationMatcher[F, Y] with UnaryOperationMatcher[F, Y]
	{
		def arithmetic[S >: LocalScope <: GlobalScope, V](e :ArithmeticSQL[F, S, V]) :Y[S, V]
	}

	type MatchArithmetic[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ArithmeticMatcher[F, Y]

	trait CaseArithmetic[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchArithmetic[F, Y] {
		override def unaryArithmetic[S >: LocalScope <: GlobalScope, V](e :UnaryOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)

		override def binaryArithmetic[S >: LocalScope <: GlobalScope, V](e :BinaryOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)
	}
	
	
	
}

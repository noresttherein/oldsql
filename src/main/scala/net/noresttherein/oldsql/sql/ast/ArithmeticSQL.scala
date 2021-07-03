package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.BinaryOperationSQL.{BinaryOperation, BinaryOperationVisitor}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.UnaryOperationSQL.{UnaryOperation, UnaryOperationVisitor}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.UnaryPostfixOperationSQL.{UnaryPostfixOperation, UnaryPostfixOperationVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL.{BinaryColumnOperator, UnaryColumnOperator}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLNumber}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLNumber.{SQLFraction, SQLInteger}



/**
  * @author Marcin Mościcki
  */
trait ArithmeticSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V] extends CompositeColumnSQL[F, S, V] {
	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, V] =
		visitor.arithmetic(this)
}




object ArithmeticSQL {

	class UnaryOperationSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V]
	                       (val operation :UnaryOperation, override val value :ColumnSQL[F, S, V])
	                       (implicit val arithmetic :SQLNumber[V])
		extends UnaryColumnOperator[F, S, V, V] with ArithmeticSQL[F, S, V]
	{
		override def readForm :ColumnReadForm[V] = value.readForm

		override def groundValue :Opt[V] = value.groundValue.flatMap(operation(_))

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
			new UnaryOperationSQL[E, C, V](operation, e)

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, V] =
			visitor.unaryArithmetic(this)

		protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			spelling.operator(operation.symbol) +: value.inParens(from, context, params)


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


		class UnaryOperation(val symbol :String) extends Serializable {
			def apply[V :SQLNumber](value :V) :Opt[V] = Lack

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


		trait UnaryOperationVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def unaryArithmetic[S >: LocalScope <: GlobalScope, V](e :UnaryOperationSQL[F, S, V]) :Y[S, V]
		}
		type MatchUnaryOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = UnaryOperationVisitor[F, Y]
		type CaseUnaryOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = UnaryOperationVisitor[F, Y]
	}




	class UnaryPostfixOperationSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V]
	                              (override val value :ColumnSQL[F, S, V], val operation :UnaryPostfixOperation)
	                              (implicit val arithmetic :SQLNumber[V])
		extends UnaryColumnOperator[F, S, V, V] with ArithmeticSQL[F, S, V]
	{
		override def readForm :ColumnReadForm[V] = value.readForm
		override def groundValue :Opt[V] = value.groundValue.flatMap(operation(_))

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
			new UnaryPostfixOperationSQL[E, C, V](e, operation)

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, V] =
			visitor.unaryPostfix(this)


		protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			value.inParens(from, context, params) + spelling.operator(operation.symbol)


		override def canEqual(that :Any) :Boolean = that match {
			case op :UnaryPostfixOperationSQL[_, _, _] => op.operation == operation
			case _ => false
		}

		override def hashCode :Int = (operation.hashCode * 31 + value.hashCode) * 31 + arithmetic.hashCode

		override def toString :String = operation.toString + value
	}



	object UnaryPostfixOperationSQL {

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, V :SQLNumber]
		         (op :UnaryPostfixOperation, value :ColumnSQL[F, S, V]) :UnaryPostfixOperationSQL[F, S, V] =
			new UnaryPostfixOperationSQL(value, op)

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, V]
		           (e :SQLExpression[F, S, V]) :Opt[(UnaryPostfixOperation, ColumnSQL[F, S, V])] =
			e match {
				case op :UnaryPostfixOperationSQL[F @unchecked, S @unchecked, V @unchecked] =>
					Got((op.operation, op.value))
				case _ => Lack
			}


		class UnaryPostfixOperation(val symbol :String) extends Serializable {
			def apply[V :SQLNumber](value :V) :Opt[V] = Lack

			def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, V :SQLNumber]
			         (value :ColumnSQL[F, S, V]) :UnaryPostfixOperationSQL[F, S, V] =
				new UnaryPostfixOperationSQL(value, this)

			def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, V]
			           (e :SQLExpression[F, S, V]) :Opt[ColumnSQL[F, S, V]] =
				e match {
					case op :UnaryPostfixOperationSQL[F @unchecked, S @unchecked, V @unchecked]
							if op.operation == this =>
						Got(op.value)
					case _ => Lack
				}
		}


		trait UnaryPostfixOperationVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def unaryPostfix[S >: LocalScope <: GlobalScope, V](e :UnaryPostfixOperationSQL[F, S, V]) :Y[S, V]
		}
		type MatchUnaryPostfixOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			UnaryPostfixOperationVisitor[F, Y]

		type CaseUnaryPostfixOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			UnaryPostfixOperationVisitor[F, Y]
	}






	class BinaryOperationSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V]
	      (override val left :ColumnSQL[F, S, V], val operation :BinaryOperation, override val right :ColumnSQL[F, S, V])
	      (implicit val arithmetic :SQLNumber[V])
		extends BinaryColumnOperator[F, S, V, V] with ArithmeticSQL[F, S, V]
	{
		override def readForm :ColumnReadForm[V] = left.readForm

		override def groundValue :Opt[V] =
			for (l <- left.groundValue; r <- right.groundValue; res <- operation(l, r)) yield res

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (left :ColumnSQL[E, C, V], right :ColumnSQL[E, C, V]) :BinaryOperationSQL[E, C, V] =
			new BinaryOperationSQL(left, operation, right)


		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, V] =
			visitor.binaryArithmetic(this)


		protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		{
			val l = left match {
				case BinaryOperationSQL(_, op, _) if op != operation || op != Plus && op != Times =>
					("(" +: spelling(left)(from, context, params)) + ")"
				case _ =>
					spelling(left)(from, context, params)
			}
			val r = right match {
				case BinaryOperationSQL(_, op, _) if op != operation || op != Plus && op != Times =>
					("(" +: spelling(right)(from, l.context, params)) + ")"
				case _ =>
					spelling(right)(from, l.context, params)
			}
			l +: (" " + spelling.operator(operation.symbol) + " ") +: r
		}


		override def canEqual(that :Any) :Boolean = that match {
			case op :BinaryOperationSQL[_, _, _] => op.operation == operation
			case _ => false
		}

		override def hashCode :Int =
			((operation.hashCode * 31 + left.hashCode) * 31 + right.hashCode) * 31 + arithmetic.hashCode

		override def toString :String = "(" + left + ") " + operation + " (" + right + ")"

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


		class BinaryOperation(val symbol :String) extends Serializable {
			def apply[V :SQLNumber](left :V, right :V) :Opt[V] = Lack

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



		trait BinaryOperationVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def binaryArithmetic[S >: LocalScope <: GlobalScope, V](e :BinaryOperationSQL[F, S, V]) :Y[S, V]
		}

		type MatchBinaryOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = BinaryOperationVisitor[F, Y]

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

		type CaseBinaryOperation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = BinaryOperationVisitor[F, Y]

	}


	final val Neg = new UnaryOperation("-") {
		override def apply[V](value :V)(implicit number :SQLNumber[V]) :Opt[V] = Got(number.negate(value))
	}
	final val Plus = new BinaryOperation("+") {
		override def apply[V:SQLNumber](left :V, right :V) = Got(SQLNumber[V].plus(left, right))
	}
	final val Minus = new BinaryOperation("-") {
		override def apply[V:SQLNumber](left :V, right :V) :Opt[V] = Got(SQLNumber[V].minus(left, right))
	}
	final val Times = new BinaryOperation("*") {
		override def apply[V:SQLNumber](left :V, right :V) :Opt[V] = Got(SQLNumber[V].times(left, right))
	}
	final val Divide = new BinaryOperation("/") {
		override def apply[V:SQLNumber](left :V, right :V) :Opt[V] = SQLNumber[V] match {
			case int :SQLInteger[V] => Got(int.quot(left, right))
			case frac :SQLFraction[V] => Got(frac.div(left, right))
			case _ => Lack
		}
	}
	final val Remainder = new BinaryOperation("%") {
		override def apply[V:SQLNumber](left :V, right :V) :Opt[V] = SQLNumber[V] match {
			case int :SQLInteger[V] => Got(int.rem(left, right))
			case frac :SQLFraction[V] => Got(frac.zero)
			case _ => Lack
		}
	}






	trait ArithmeticVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends BinaryOperationVisitor[F, Y] with UnaryOperationVisitor[F, Y] with UnaryPostfixOperationVisitor[F, Y]
	{
		def arithmetic[S >: LocalScope <: GlobalScope, V](e :ArithmeticSQL[F, S, V]) :Y[S, V]
	}

	type MatchArithmetic[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ArithmeticVisitor[F, Y]

	trait CaseArithmetic[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchArithmetic[F, Y] {
		override def unaryArithmetic[S >: LocalScope <: GlobalScope, V](e :UnaryOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)

		override def unaryPostfix[S >: LocalScope <: GlobalScope, V](e :UnaryPostfixOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)

		override def binaryArithmetic[S >: LocalScope <: GlobalScope, V](e :BinaryOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)
	}
	
	
	
}

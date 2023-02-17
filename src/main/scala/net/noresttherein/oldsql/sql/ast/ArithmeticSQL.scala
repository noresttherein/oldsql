package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{Single, Grouped}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.BinaryOperationSQL.{AnyBinaryOperationVisitor, BinaryOperation, SpecificBinaryOperationVisitor}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.UnaryOperationSQL.{AnyUnaryOperationVisitor, SpecificUnaryOperationVisitor, UnaryOperation}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.UnaryPostfixOperationSQL.{AnyUnaryPostfixOperationVisitor, SpecificUnaryPostfixOperationVisitor, UnaryPostfixOperation}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.{BinaryCompositeColumn, UnaryCompositeColumn}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLNumber}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLNumber.{SQLFraction, SQLInteger}






/**
  * @author Marcin Mościcki
  */
trait ArithmeticSQL[-F <: RowProduct, -S >: Grouped <: Single, V] extends CompositeColumnSQL[F, S, V] {
	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, Y]) :Y[S, V] =
		visitor.arithmetic(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Grouped,
//	                             E >: ColumnSQL[F_, S_, V] <: SQLExpression[F_, S_, V],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, V, E] =
//		visitor.arithmetic(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, V, Y]) :Y = visitor.arithmetic(this)
}




object ArithmeticSQL {

	class UnaryOperationSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
	                       (val operation :UnaryOperation, override val value :ColumnSQL[F, S, V])
	                       (implicit val arithmetic :SQLNumber[V])
		extends UnaryCompositeColumn[F, S, V, V] with ArithmeticSQL[F, S, V]
	{
		override def selectForm    :ColumnReadForm[V] = value.selectForm
		override def groundValue :Opt[V] = value.groundValue.flatMap(operation(_))

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                              (e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
			new UnaryOperationSQL[E, C, V](operation, e)

		protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			spelling.operator(operation.symbol) +: value.atomicSpelling(from, context, params)(spelling.inOperand)


		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, V] = visitor.unaryArithmetic(this)

		protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, V, Y]) :Y = visitor.unaryArithmetic(this)
//
//		protected override def visit[F_ <: F, S_ >: Grouped <: Grouped,
//		                             E >: ColumnSQL[F_, S_, V] <: SQLExpression[F_, S_, V],
//		                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[F, R]) :R[S_, V, E] =
//			visitor.unaryArithmetic(this)

		override def canEqual(that :Any) :Boolean = that match {
			case op :UnaryOperationSQL[_, _, _] => op.operation == operation
			case _ => false
		}
		override def hashCode :Int = (operation.hashCode * 31 + value.hashCode) * 31 + arithmetic.hashCode

		override def toString :String = operation.toString + value
	}


	object UnaryOperationSQL {

		def apply[F <: RowProduct, S >: Grouped <: Single, V :SQLNumber]
		         (op :UnaryOperation, value :ColumnSQL[F, S, V]) :UnaryOperationSQL[F, S, V] =
			new UnaryOperationSQL(op, value)

		def unapply[F <: RowProduct, S >: Grouped <: Single, V]
		           (e :SQLExpression[F, S, V]) :Opt[(UnaryOperation, ColumnSQL[F, S, V])] =
			e match {
				case op :UnaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] =>
					Got((op.operation, op.value))
				case _ => Lack
			}

		class UnaryOperation(val symbol :String) extends Serializable {
			def apply[V :SQLNumber](value :V) :Opt[V] = Lack

			def apply[F <: RowProduct, S >: Grouped <: Single, V :SQLNumber]
			         (value :ColumnSQL[F, S, V]) :UnaryOperationSQL[F, S, V] =
				new UnaryOperationSQL(this, value)

			def unapply[F <: RowProduct, S >: Grouped <: Single, V]
			           (e :SQLExpression[F, S, V]) :Opt[ColumnSQL[F, S, V]] =
				e match {
					case op :UnaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] if op.operation == this =>
						Got(op.value)
					case _ => Lack
				}
		}

		trait SpecificUnaryOperationVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
			def unaryArithmetic(e :UnaryOperationSQL[F, S, X]) :Y
		}
		type MatchSpecificUnaryOperation[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
			SpecificUnaryOperationVisitor[F, S, X, Y]
		type CaseSpcecificUnaryOperation[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
			SpecificUnaryOperationVisitor[F, S, X, Y]
//
//		trait UnaryOperationVisitor[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] {
//			def unaryArithmetic[S >: Grouped <: Single, V]
//			                   (e :UnaryOperationSQL[F, S, V]) :R[S, V, UnaryOperationSQL[F, S, V]]
//		}
//		type MatchUnaryOperation[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//			UnaryOperationVisitor[F, R]
//		type CaseUnaryOperation[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//			UnaryOperationVisitor[F, R]

		trait AnyUnaryOperationVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
			def unaryArithmetic[S >: Grouped <: Single, V](e :UnaryOperationSQL[F, S, V]) :Y[S, V]
		}
		type MatchAnyUnaryOperation[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
			AnyUnaryOperationVisitor[F, Y]
		type CaseAnyUnaryOperation[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
			AnyUnaryOperationVisitor[F, Y]
	}




	class UnaryPostfixOperationSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
	                              (override val value :ColumnSQL[F, S, V], val operation :UnaryPostfixOperation)
	                              (implicit val arithmetic :SQLNumber[V])
		extends UnaryCompositeColumn[F, S, V, V] with ArithmeticSQL[F, S, V]
	{
		override def selectForm :ColumnReadForm[V] = value.selectForm
		override def groundValue :Opt[V] = value.groundValue.flatMap(operation(_))

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                              (e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
			new UnaryPostfixOperationSQL[E, C, V](e, operation)


		protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			value.atomicSpelling(from, context, params)(spelling.inOperand) + spelling.operator(operation.symbol)


		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, V] = visitor.unaryPostfix(this)

		protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, V, Y]) :Y = visitor.unaryPostfix(this)
//
//		protected override def visit[F_ <: F, S_ >: Grouped <: Grouped,
//		                             E >: ColumnSQL[F_, S_, V] <: SQLExpression[F_, S_, V],
//		                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[F, R]) :R[S_, V, E] =
//			visitor.unaryPostfix(this)

		override def canEqual(that :Any) :Boolean = that match {
			case op :UnaryPostfixOperationSQL[_, _, _] => op.operation == operation
			case _ => false
		}
		override def hashCode :Int = (operation.hashCode * 31 + value.hashCode) * 31 + arithmetic.hashCode

		override def toString :String = operation.toString + value
	}


	object UnaryPostfixOperationSQL {

		def apply[F <: RowProduct, S >: Grouped <: Single, V :SQLNumber]
		         (op :UnaryPostfixOperation, value :ColumnSQL[F, S, V]) :UnaryPostfixOperationSQL[F, S, V] =
			new UnaryPostfixOperationSQL(value, op)

		def unapply[F <: RowProduct, S >: Grouped <: Single, V]
		           (e :SQLExpression[F, S, V]) :Opt[(UnaryPostfixOperation, ColumnSQL[F, S, V])] =
			e match {
				case op :UnaryPostfixOperationSQL[F @unchecked, S @unchecked, V @unchecked] =>
					Got((op.operation, op.value))
				case _ => Lack
			}

		class UnaryPostfixOperation(val symbol :String) extends Serializable {
			def apply[V :SQLNumber](value :V) :Opt[V] = Lack

			def apply[F <: RowProduct, S >: Grouped <: Single, V :SQLNumber]
			         (value :ColumnSQL[F, S, V]) :UnaryPostfixOperationSQL[F, S, V] =
				new UnaryPostfixOperationSQL(value, this)

			def unapply[F <: RowProduct, S >: Grouped <: Single, V]
			           (e :SQLExpression[F, S, V]) :Opt[ColumnSQL[F, S, V]] =
				e match {
					case op :UnaryPostfixOperationSQL[F @unchecked, S @unchecked, V @unchecked]
							if op.operation == this =>
						Got(op.value)
					case _ => Lack
				}
		}

		trait SpecificUnaryPostfixOperationVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
			def unaryPostfix(e :UnaryPostfixOperationSQL[F, S, X]) :Y
		}
		type MatchSpecificUnaryPostfixOperation[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
			SpecificUnaryPostfixOperationVisitor[F, S, X, Y]
		type CaseSpecificUnaryPostfixOperation[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
			SpecificUnaryPostfixOperationVisitor[F, S, X, Y]
//
//		trait UnaryPostfixOperationVisitor
//		      [+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]]
//		{
//			def unaryPostfix[S >: Grouped <: Single, V]
//			                (e :UnaryPostfixOperationSQL[F, S, V]) :R[S, V, UnaryPostfixOperationSQL[F, S, V]]
//		}
//		type MatchUnaryPostfixOperation[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//			UnaryPostfixOperationVisitor[F, R]
//		type CaseUnaryPostfixOperation[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//			UnaryPostfixOperationVisitor[F, R]

		trait AnyUnaryPostfixOperationVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
			def unaryPostfix[S >: Grouped <: Single, V](e :UnaryPostfixOperationSQL[F, S, V]) :Y[S, V]
		}
		type MatchAnyUnaryPostfixOperation[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
			AnyUnaryPostfixOperationVisitor[F, Y]
		type CaseAnyUnaryPostfixOperation[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
			AnyUnaryPostfixOperationVisitor[F, Y]
	}




	class BinaryOperationSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
	      (override val left :ColumnSQL[F, S, V], val operation :BinaryOperation, override val right :ColumnSQL[F, S, V])
	      (implicit val arithmetic :SQLNumber[V])
		extends BinaryCompositeColumn[F, S, V, V] with ArithmeticSQL[F, S, V]
	{
		override def selectForm :ColumnReadForm[V] = left.selectForm

		override def groundValue :Opt[V] =
			for (l <- left.groundValue; r <- right.groundValue; res <- operation(l, r)) yield res

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                              (left :ColumnSQL[E, C, V], right :ColumnSQL[E, C, V]) :BinaryOperationSQL[E, C, V] =
			new BinaryOperationSQL(left, operation, right)


		protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		{
			val spell = spelling.inOperand
			val l = left match {
				case BinaryOperationSQL(_, op, _) if op != operation || op != Plus && op != Times =>
					("(" +: spell(left)(from, context, params)) + ")"
				case _ =>
					spell(left)(from, context, params)
			}
			val r = right match {
				case BinaryOperationSQL(_, op, _) if op != operation || op != Plus && op != Times =>
					("(" +: spell(right)(from, l.context, params)) + ")"
				case _ =>
					spell(right)(from, l.context, params)
			}
			l +: (" " + spelling.operator(operation.symbol) + " ") +: r
		}


		protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, Y]) :Y[S, V] =
			visitor.binaryArithmetic(this)
//
//		protected override def visit[F_ <: F, S_ >: Grouped <: Grouped,
//		                             E >: ColumnSQL[F_, S_, V] <: SQLExpression[F_, S_, V],
//		                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[F, R]) :R[S_, V, E] =
//			visitor.binaryArithmetic(this)

		protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, V, Y]) :Y = visitor.binaryArithmetic(this)

		override def canEqual(that :Any) :Boolean = that match {
			case op :BinaryOperationSQL[_, _, _] => op.operation == operation
			case _ => false
		}
		override def hashCode :Int =
			((operation.hashCode * 31 + left.hashCode) * 31 + right.hashCode) * 31 + arithmetic.hashCode

		override def toString :String = "(" + left + ") " + operation + " (" + right + ")"
	}


	object BinaryOperationSQL {

		def apply[F <: RowProduct, S >: Grouped <: Single, V :SQLNumber]
		         (left :ColumnSQL[F, S, V], operation :BinaryOperation, right :ColumnSQL[F, S, V])
				:BinaryOperationSQL[F, S, V] =
			new BinaryOperationSQL(left, operation, right)

		def unapply[F <: RowProduct, S >: Grouped <: Single, V](e :SQLExpression[F, S, V])
				:Opt[(ColumnSQL[F, S, V], BinaryOperation, ColumnSQL[F, S, V])] =
			e match {
				case op :BinaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] =>
					Got((op.left, op.operation, op.right))
				case  _ => Lack
			}

		class BinaryOperation(val symbol :String) extends Serializable {
			def apply[V :SQLNumber](left :V, right :V) :Opt[V] = Lack

			def apply[F <: RowProduct, S >: Grouped <: Single, V :SQLNumber]
			         (left :ColumnSQL[F, S, V], right :ColumnSQL[F, S, V]) :BinaryOperationSQL[F, S, V] =
				new BinaryOperationSQL(left, this, right)

			def unapply[F <: RowProduct, S >: Grouped <: Single, V](e :SQLExpression[F, S, V])
					:Opt[(ColumnSQL[F, S, V], ColumnSQL[F, S, V])] =
				e match {
					case op :BinaryOperationSQL[F @unchecked, S @unchecked, V @unchecked] if op.operation == this =>
						Got((op.left, op.right))
					case _ => Lack
				}
		}

		trait SpecificBinaryOperationVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
			def binaryArithmetic(e :BinaryOperationSQL[F, S, X]) :Y
		}
		type MatchSpecificBinaryOperation[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
			SpecificBinaryOperationVisitor[F, S, X, Y]
		type CaseSpecificBinaryOperation[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
			SpecificBinaryOperationVisitor[F, S, X, Y]
//
//		trait BinaryOperationVisitor[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] {
//			def binaryArithmetic[S >: Grouped <: Single, V]
//			                    (e :BinaryOperationSQL[F, S, V]) :R[S, V, BinaryOperationSQL[F, S, V]]
//		}
//		type MatchBinaryOperation[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//			BinaryOperationVisitor[F, R]
//		type CaseBinaryOperation[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//			BinaryOperationVisitor[F, R]

		trait AnyBinaryOperationVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
			def binaryArithmetic[S >: Grouped <: Single, V](e :BinaryOperationSQL[F, S, V]) :Y[S, V]
		}
		type MatchAnyBinaryOperation[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyBinaryOperationVisitor[F, Y]
		type CaseAnyBinaryOperation[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyBinaryOperationVisitor[F, Y]
	}




	//consider: moving UnaryOperation and UnaryPostfixOperation to ArithmeticSQL
	final val Neg :UnaryOperation = new UnaryOperation("-") {
		override def apply[V](value :V)(implicit number :SQLNumber[V]) :Opt[V] = Got(number.negate(value))
		private def readResolve = Neg
	}
	final val Plus :BinaryOperation = new BinaryOperation("+") {
		override def apply[V:SQLNumber](left :V, right :V) = Got(SQLNumber[V].plus(left, right))
		private def readResolve = Plus
	}
	final val Minus :BinaryOperation = new BinaryOperation("-") {
		override def apply[V:SQLNumber](left :V, right :V) :Opt[V] = Got(SQLNumber[V].minus(left, right))
		private def readResolve = Minus
	}
	final val Times :BinaryOperation = new BinaryOperation("*") {
		override def apply[V:SQLNumber](left :V, right :V) :Opt[V] = Got(SQLNumber[V].times(left, right))
		private def readResolve = Times
	}
	final val Divide :BinaryOperation = new BinaryOperation("/") {
		override def apply[V:SQLNumber](left :V, right :V) :Opt[V] = SQLNumber[V] match {
			case int :SQLInteger[V] => Got(int.quot(left, right))
			case frac :SQLFraction[V] => Got(frac.div(left, right))
			case _ => Lack
		}
		private def readResolve = Divide
	}
	final val Remainder :BinaryOperation = new BinaryOperation("%") {
		override def apply[V:SQLNumber](left :V, right :V) :Opt[V] = SQLNumber[V] match {
			case int :SQLInteger[V] => Got(int.rem(left, right))
			case frac :SQLFraction[V] => Got(frac.zero)
			case _ => Lack
		}
		private def readResolve = Remainder
	}




	trait SpecificArithmeticVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificBinaryOperationVisitor[F, S, X, Y] with SpecificUnaryOperationVisitor[F, S, X, Y]
		   with SpecificUnaryPostfixOperationVisitor[F, S, X, Y]
	{
		def arithmetic(e :ArithmeticSQL[F, S, X]) :Y
	}
	type MatchSpecificArithmetic[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificArithmeticVisitor[F, S, X, Y]

	trait CaseSpecificArithmetic[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificArithmetic[F, S, X, Y]
	{
		override def unaryArithmetic(e :UnaryOperationSQL[F, S, X]) :Y = arithmetic(e)
		override def unaryPostfix(e :UnaryPostfixOperationSQL[F, S, X]) :Y = arithmetic(e)
		override def binaryArithmetic(e :BinaryOperationSQL[F, S, X]) :Y = arithmetic(e)
	}
//
//
//	trait ArithmeticVisitor[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]]
//		extends BinaryOperationVisitor[F, R] with UnaryOperationVisitor[F, R] with UnaryPostfixOperationVisitor[F, R]
//	{
//		def arithmetic[S >: Grouped <: Single, V](e :ArithmeticSQL[F, S, V]) :R[S, V, ArithmeticSQL[F, S, V]]
//	}
//	type MatchArithmetic[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//		ArithmeticVisitor[F, R]
//
//	trait CaseArithmetic[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]]
//		extends MatchArithmetic[F, R]
//		   with CaseBinaryOperation[F, R] with CaseUnaryOperation[F, R] with CaseUnaryPostfixOperation[F, R]
//	{
//		override def binaryArithmetic[S >: Grouped <: Single, V]
//		                             (e :BinaryOperationSQL[F, S, V]) :R[S, V, BinaryOperationSQL[F, S, V]] =
//			arithmetic(e)
//
//		override def unaryArithmetic[S >: Grouped <: Single, V]
//		                            (e :UnaryOperationSQL[F, S, V]) :R[S, V, UnaryOperationSQL[F, S, V]] =
//			arithmetic(e)
//
//		override def unaryPostfix[S >: Grouped <: Single, V]
//		                         (e :UnaryPostfixOperationSQL[F, S, V]) :R[S, V, UnaryPostfixOperationSQL[F, S, V]] =
//			arithmetic(e)
//	}


	trait AnyArithmeticVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyBinaryOperationVisitor[F, Y] with AnyUnaryOperationVisitor[F, Y]
		   with AnyUnaryPostfixOperationVisitor[F, Y]
	{
		def arithmetic[S >: Grouped <: Single, V](e :ArithmeticSQL[F, S, V]) :Y[S, V]
	}
	type MatchAnyArithmetic[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyArithmeticVisitor[F, Y]

	trait CaseAnyArithmetic[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyArithmetic[F, Y] {
		override def unaryArithmetic[S >: Grouped <: Single, V](e :UnaryOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)

		override def unaryPostfix[S >: Grouped <: Single, V](e :UnaryPostfixOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)

		override def binaryArithmetic[S >: Grouped <: Single, V](e :BinaryOperationSQL[F, S, V]) :Y[S, V] =
			arithmetic(e)
	}
}

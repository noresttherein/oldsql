package net.noresttherein.oldsql.sql.mechanics

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime}

import scala.annotation.nowarn
import scala.math.Numeric.IntIsIntegral
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.slang.classNameMethods
import net.noresttherein.oldsql.sql.mechanics.SQLOrdering.{MappedSQLOrdering, ReverseSQLOrdering}






/**
  * @author Marcin Mościcki
  */
sealed trait SQLOrdering[X] extends Ordering[X] with Serializable {
	def unmap[Y](lower :Y => X) :SQLOrdering[Y] = new MappedSQLOrdering(lower)(this)

	override def reverse :SQLOrdering[X] = new ReverseSQLOrdering[X](this)

	override def isReverseOf(other :Ordering[_]) :Boolean = other match {
		case rev :ReverseSQLOrdering[_] => rev.reverse == this
		case _ => false
	}

/*
	protected def defaultSpelling[F <: RowProduct, P]
	                             (left :SQLExpression[F, Grouped, X], operator :OrderingOperator,
	                              right :SQLExpression[F, Grouped, X])
	                             (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                             (implicit spelling :SQLSpelling) :SpelledSQL[P]

	final private[sql] def defaultSpelling[F <: RowProduct, P]
	                                      (spelling :SQLSpelling)
	                                      (left :SQLExpression[F, Grouped, X], operator :OrderingOperator,
	                                       right :SQLExpression[F, Grouped, X])
	                                      (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
		defaultSpelling(left, operator, right)(from, context, params)(spelling)
*/
}






object SQLOrdering { self =>

	@inline def apply[X :SQLOrdering] :SQLOrdering[X] = implicitly[SQLOrdering[X]]

	def by[X, Y :SQLOrdering](f :X => Y) :SQLOrdering[X] = SQLOrdering[Y].unmap(f)


	private class MappedSQLOrdering[X, Y](lower :Y => X)(implicit order :SQLOrdering[X]) extends SQLOrdering[Y] {
		override def compare(x :Y, y :Y) :Int = order.compare(lower(x), lower(y))

//		protected override def defaultSpelling[F <: RowProduct, P]
//		                       (left :SQLExpression[F, Grouped, Y], operator: OrderingOperator,
//		                        right :SQLExpression[F, Grouped, Y])
//		                       (from :F, context :SQLContext[P], params :Parameterization[P, F])
//		                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
//			spelling.compare(left.map(lower), operator, right.map(lower))(from, context, params)

		override def toString = "by[" + order + "]"
	}


	private class ReverseSQLOrdering[X](override val reverse :SQLOrdering[X]) extends SQLOrdering[X] {
		override def compare(x :X, y :X) :Int = reverse.compare(y, x)

		override def isReverseOf(other :Ordering[_]) :Boolean = other == reverse

//		protected override def defaultSpelling[F <: RowProduct, P]
//		                       (left :SQLExpression[F, Grouped, X], operator :OrderingOperator,
//		                        right :SQLExpression[F, Grouped, X])
//		                       (from :F, context :SQLContext[P], params :Parameterization[P, F])
//		                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
//			spelling.compare(right, operator, left)(from, context, params)(reverse)

		override def toString :String = reverse.toString + ".reverse"
	}

	private class ColumnOrdering[X :ClassTag](implicit order :Ordering[X]) extends SQLOrdering[X] {
		override def compare(x :X, y :X) :Int = order.compare(x, y)

/*
		protected override def defaultSpelling[F <: RowProduct, P]
		                       (left :SQLExpression[F, Grouped, X], operator :OrderingOperator,
		                        right :SQLExpression[F, Grouped, X])
		                       (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                       (implicit spelling :SQLSpelling) =
		{
			val cmpSpelling = spelling.inWhere
			val anchoredLeft = if (left.isAnchored(from)) left else left.anchor(from)
			val anchoredRight = if (right.isAnchored(from)) right else right.anchor(from)
			val (finalLeft, finalRight) =
				if (left.columnCount(cmpSpelling) == right.columnCount(cmpSpelling))
					(anchoredLeft, anchoredRight)
				else
					left.reform(right)(SQLTypeUnification.directly, cmpSpelling)
			val leftColumns = cmpSpelling.explode(finalLeft)(from, context, params)
			if (leftColumns.length != 1)
				throw new MismatchedExpressionsException(left, right,
					leftColumns.mkString("single column required, but ", ", ", " included.")
				)
			val rightColumns = cmpSpelling.explode(finalRight)(from, leftColumns.head.context, params)
			if (rightColumns.length != 1)
				throw new MismatchedExpressionsException(left, right,
					rightColumns.mkString("single column required, but ", ", ", " included.")
				)
			leftColumns.head + " " + spelling.operator(operator.symbol) + " " + rightColumns.head
		}
*/

		override def toString :String = "<=[" + implicitly[ClassTag[X]] + "]"
	}


/*
	trait LexicographicalOrdering[X] extends SQLOrdering[X] {
		protected[SQLOrdering] def splitSpelling[F <: RowProduct, P]
		                                        (left :SQLExpression[F, Grouped, X], operator :OrderingOperator,
		                                         right :SQLExpression[F, Grouped, X])
		                                        (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:(SpelledSQL[P], SpelledSQL[P], SpelledSQL[P])

		protected override def defaultSpelling[F <: RowProduct, P]
		                                      (left :SQLExpression[F, Grouped, X], operator :OrderingOperator,
		                                       right :SQLExpression[F, Grouped, X])
		                                      (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                      (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		{
			val (previous, nonStrict, strict) = splitSpelling(left, operator, right)(from, context, params)
			if (operator.isStrict) previous + strict else previous + nonStrict
		}
	}

	class ChainOrdering[Xs <: Chain, X](implicit prev :LexicographicalOrdering[Xs], val last :SQLOrdering[X])
		extends LexicographicalOrdering[Xs ~ X]
	{
		override def compare(x :Xs ~ X, y :Xs ~ X) :Int = prev.compare(x.init, y.init) match {
			case 0 => last.compare(x.last, y.last)
			case cmp => cmp
		}

		protected override def splitSpelling[F <: RowProduct, P]
		                       (left :SQLExpression[F, Grouped, Xs ~ X], operator :OrderingOperator,
		                        right :SQLExpression[F, Grouped, Xs ~ X])
		                       (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                       (implicit spelling :SQLSpelling) :(SpelledSQL[P], SpelledSQL[P], SpelledSQL[P]) =
		(left, right) match {
			case (ChainSQL(leftInit, leftLast), ChainSQL(rightInit, rightLast)) =>
					val (prefix, prevNonStrict, prevStrict) =
						prev.splitSpelling(leftInit, operator.nonStrict, rightInit)(from, context, params)
					val (lastNonStrict, lastStrict) =
						if (operator.isStrict)
							(spelling.compare(leftLast, operator.nonStrict, rightLast)(from, prefix.context, params)(last),
							spelling.compare(leftLast, operator, rightLast)(from, prefix.context, params)(last))
						else if (operator.isNonStrict)
							(spelling.compare(leftLast, operator, rightLast)(from, prefix.context, params)(last),
							spelling.compare(leftLast, operator.strict, rightLast)(from, prefix.context, params)(last))
						else {
							val cmp = spelling.compare(leftLast, operator, rightLast)(from, prefix.context, params)(last)
							(cmp, cmp)
						}
					(prefix)
			case _ =>
				throw new MismatchedExpressionsException(left, right,
					"cannot split the expressions into a prefix and the last element because " +
					"they are not ChainSQL but " + left.getClass.getName + + " and " + right.getClass.getName + "."

				)
		}

		override def toString :String = prev.toString + "~" + last.toString
	}

	implicit object EmptyTupleOrdering extends SQLOrdering[@~] {
		override def compare(x: @~, y: @~) :Int = 0

		protected override def defaultSpelling[F <: RowProduct, P]
		                       (left :SQLExpression[F, Grouped, @~], right :SQLExpression[F, Grouped, @~])
		                       (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			SpelledSQL(context)

		override def toString = "@~"
	}
*/

	//in the following declarations, we need to shadow the implicits
	implicit val OfInt            :SQLOrdering[Int]            = { @nowarn val OfInt            = null; new ColumnOrdering }
	implicit val OfLong           :SQLOrdering[Long]           = { @nowarn val OfLong           = null; new ColumnOrdering }
	implicit val OfShort          :SQLOrdering[Short]          = { @nowarn val OfShort          = null; new ColumnOrdering }
	implicit val OfByte           :SQLOrdering[Byte]           = { @nowarn val OfByte           = null; new ColumnOrdering }
	implicit val OfFloat          :SQLOrdering[Float]          = { @nowarn val OfFloat          = null; new ColumnOrdering }
	implicit val OfDouble         :SQLOrdering[Double]         = { @nowarn val OfDouble         = null; new ColumnOrdering }
	implicit val OfBigInt         :SQLOrdering[BigInt]         = { @nowarn val OfBigInt         = null; new ColumnOrdering }
	implicit val OfBigDecimal     :SQLOrdering[BigDecimal]     = { @nowarn val OfBigDecimal     = null; new ColumnOrdering }

	implicit val OfString         :SQLOrdering[String]         = { @nowarn val OfString         = null; new ColumnOrdering }

	implicit val OfLocalDate      :SQLOrdering[LocalDate]      = { @nowarn val OfLocalDate      = null; new ColumnOrdering }
	implicit val OfDateTime       :SQLOrdering[LocalDateTime]  = { @nowarn val OfDateTime       = null; new ColumnOrdering }
	implicit val OfLocalTime      :SQLOrdering[LocalTime]      = { @nowarn val OfLocalTime      = null; new ColumnOrdering }
	implicit val OfZonedDateTime  :SQLOrdering[ZonedDateTime]  = { @nowarn val OfZonedDateTime  = null; new ColumnOrdering }
	implicit val OfOffsetDateTime :SQLOrdering[OffsetDateTime] = { @nowarn val OfOffsetDateTime = null; new ColumnOrdering }
	implicit val OfOffsetTime     :SQLOrdering[OffsetTime]     = { @nowarn val OfOffsetTime     = null; new ColumnOrdering }
	implicit val OfInstant        :SQLOrdering[Instant]        = { @nowarn val OfInstant        = null; new ColumnOrdering }


	implicit def OfTuple[I <: Chain :SQLOrdering, L :SQLOrdering] :SQLOrdering[I ~ L] = new ColumnOrdering

	implicit val OfEmptyTuple :SQLOrdering[@~] = new ColumnOrdering
}






/**
  * @author Marcin Mościcki
  */
sealed trait SQLNumber[T] extends SQLOrdering[T] with Numeric[T]



object SQLNumber {
	@inline def apply[V](implicit num :SQLNumber[V]) :SQLNumber[V] = num

	//consider: making these two ColumnForms - if we don't need any extra features at least
	sealed trait SQLInteger[T] extends SQLNumber[T] with Integral[T]
	sealed trait SQLFraction[T] extends SQLNumber[T] with Fractional[T]

	private class AbstractNumber[T](implicit cmp :Numeric[T]) extends SQLNumber[T] {
		override def compare(x :T, y :T) = cmp.compare(x, y)
		override def plus(x :T, y :T) = cmp.plus(x, y)
		override def minus(x :T, y :T) = cmp.minus(x, y)
		override def times(x :T, y :T) = cmp.times(x, y)
		override def negate(x :T) = cmp.negate(x)
		override def fromInt(x :Int) = cmp.fromInt(x)
		override def parseString(str :String) = cmp.parseString(str)
		override def toInt(x :T) = cmp.toInt(x)
		override def toLong(x :T) = cmp.toLong(x)
		override def toFloat(x :T) = cmp.toFloat(x)
		override def toDouble(x :T) = cmp.toDouble(x)

		private def num :Numeric[T] = cmp

		override def equals(that :Any) :Boolean = that match {
			case num :AbstractNumber[_] => (this eq num) || num.num == this.num
			case _ => false
		}
		override def hashCode = cmp.hashCode
		override def toString = cmp.localClassName
	}


	private class Integer[T](implicit cmp :Integral[T]) extends AbstractNumber[T] with SQLInteger[T] {
		override def quot(x :T, y :T) = cmp.quot(x, y)
		override def rem(x :T, y :T) = cmp.rem(x, y)
	}

	private class Fraction[T](implicit cmp :Fractional[T]) extends AbstractNumber[T] with SQLFraction[T] {
		override def div(x :T, y :T) = cmp.div(x, y)
	}

	private def integer[T :Integral] :SQLInteger[T] = new Integer[T]
	private def fraction[T :Fractional] :SQLFraction[T] = new Fraction[T]

	implicit val SQLInt        = integer[Int]
	implicit val SQLLong       = integer[Long]
	implicit val SQLShort      = integer[Short]
	implicit val SQLByte       = integer[Byte]
	implicit val SQLDouble     = fraction[Double]
	implicit val SQLFloat      = fraction[Float]
	implicit val SQLBigInt     = integer[BigInt]
	implicit val SQLBigDecimal = fraction[BigDecimal]
//	implicit val SQLBigInteger = integer[java.math.BigInteger](Numeric.BigIntIsIntegral)
//	implicit val SQLJavaBigDecimal = fraction[java.math.BigDecimal]

}

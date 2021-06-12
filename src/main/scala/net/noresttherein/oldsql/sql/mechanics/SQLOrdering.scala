package net.noresttherein.oldsql.sql.mechanics

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime}

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
}






object SQLOrdering { self =>

	@inline def apply[X :SQLOrdering] :SQLOrdering[X] = implicitly[SQLOrdering[X]]

	def by[X, Y :SQLOrdering](f :X => Y) :SQLOrdering[X] = SQLOrdering[Y].unmap(f)


	private class MappedSQLOrdering[X, Y](lower :Y => X)(implicit order :SQLOrdering[X]) extends SQLOrdering[Y] {
		override def compare(x :Y, y :Y) :Int = order.compare(lower(x), lower(y))

		override def toString = "by[" + order + "]"
	}


	private class ReverseSQLOrdering[X](override val reverse :SQLOrdering[X]) extends SQLOrdering[X] {
		override def compare(x :X, y :X) :Int = reverse.compare(y, x)

		override def isReverseOf(other :Ordering[_]) :Boolean = other == reverse

		override def toString :String = reverse.toString + ".reverse"
	}

	protected class Adapter[X :ClassTag](implicit order :Ordering[X]) extends SQLOrdering[X] {
		override def compare(x :X, y :X) :Int = order.compare(x, y)

		override def toString :String = "<=[" + implicitly[ClassTag[X]] + "]"
	}


	implicit val OfInt            :SQLOrdering[Int]            = { val OfInt            = null; new Adapter }
	implicit val OfLong           :SQLOrdering[Long]           = { val OfLong           = null; new Adapter }
	implicit val OfShort          :SQLOrdering[Short]          = { val OfShort          = null; new Adapter }
	implicit val OfByte           :SQLOrdering[Byte]           = { val OfByte           = null; new Adapter }
	implicit val OfFloat          :SQLOrdering[Float]          = { val OfFloat          = null; new Adapter }
	implicit val OfDouble         :SQLOrdering[Double]         = { val OfDouble         = null; new Adapter }
	implicit val OfBigInt         :SQLOrdering[BigInt]         = { val OfBigInt         = null; new Adapter }
	implicit val OfBigDecimal     :SQLOrdering[BigDecimal]     = { val OfBigDecimal     = null; new Adapter }

	implicit val OfString         :SQLOrdering[String]         = { val OfString         = null; new Adapter }


	implicit val OfLocalDate      :SQLOrdering[LocalDate]      = { val OfLocalDate      = null; new Adapter }
	implicit val OfDateTime       :SQLOrdering[LocalDateTime]  = { val OfDateTime       = null; new Adapter }
	implicit val OfLocalTime      :SQLOrdering[LocalTime]      = { val OfLocalTime      = null; new Adapter }
	implicit val OfZonedDateTime  :SQLOrdering[ZonedDateTime]  = { val OfZonedDateTime  = null; new Adapter }
	implicit val OfOffsetDateTime :SQLOrdering[OffsetDateTime] = { val OfOffsetDateTime = null; new Adapter }
	implicit val OfOffsetTime     :SQLOrdering[OffsetTime]     = { val OfOffsetTime     = null; new Adapter }
	implicit val OfInstant        :SQLOrdering[Instant]        = { val OfInstant        = null; new Adapter }


	implicit def OfTuple[I <: Chain :SQLOrdering, L:SQLOrdering] :SQLOrdering[I ~ L] = new Adapter

	implicit val OfEmptyTuple :SQLOrdering[@~] = new Adapter
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

	implicit val SQLInt = integer[Int]
	implicit val SQLLong = integer[Long]
	implicit val SQLShort = integer[Short]
	implicit val SQLByte = integer[Byte]
	implicit val SQLDouble = fraction[Double]
	implicit val SQLFloat = fraction[Float]
	implicit val SQLBigInt = integer[BigInt]
	implicit val SQLBigDecimal = fraction[BigDecimal]
//	implicit val SQLBigInteger = integer[java.math.BigInteger](Numeric.BigIntIsIntegral)
//	implicit val SQLJavaBigDecimal = fraction[java.math.BigDecimal]

}

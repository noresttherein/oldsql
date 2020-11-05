package net.noresttherein.oldsql.sql

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime}

import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.sql.SQLOrdering.{MappedSQLOrdering, ReverseSQLOrdering}



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






object SQLOrdering {

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


	private class Adapter[X :ClassTag](implicit order :Ordering[X]) extends SQLOrdering[X] {
		override def compare(x :X, y :X) :Int = order.compare(x, y)

		override def toString :String = "<=[" + implicitly[ClassTag[X]] + "]"
	}



	implicit val OfInt :SQLOrdering[Int] = new Adapter
	implicit val OfLong :SQLOrdering[Long] = new Adapter
	implicit val OfShort :SQLOrdering[Short] = new Adapter
	implicit val OfByte :SQLOrdering[Byte] = new Adapter
	implicit val OfFloat :SQLOrdering[Float] = new Adapter
	implicit val OfDouble :SQLOrdering[Double] = new Adapter
	implicit val OfBigInt :SQLOrdering[BigInt] = new Adapter
	implicit val OfBigDecimal :SQLOrdering[BigDecimal] = new Adapter

	implicit val OfString :SQLOrdering[String] = new Adapter


	implicit val OfLocalDate :SQLOrdering[LocalDate] = new Adapter
	implicit val OfDateTime :SQLOrdering[LocalDateTime] = new Adapter
	implicit val OfLocalTime :SQLOrdering[LocalTime] = new Adapter
	implicit val OfZonedDateTime :SQLOrdering[ZonedDateTime] = new Adapter
	implicit val OfOffsetDateTime :SQLOrdering[OffsetDateTime] = new Adapter
	implicit val OfOffsetTime :SQLOrdering[OffsetTime] = new Adapter
	implicit val OfInstant :SQLOrdering[Instant] = new Adapter



	implicit def OfTuple[I <: Chain :SQLOrdering, L :SQLOrdering] :SQLOrdering[I ~ L] = new Adapter
	implicit val OfEmptyTuple :SQLOrdering[@~] = new Adapter

}








/**
  * @author Marcin Mościcki
  */
sealed trait SQLNumber[T] extends SQLOrdering[T]



object SQLNumber {
	//consider: making these two ColumnForms - if we don't need any extra features at least
	sealed trait SQLInteger[T] extends SQLNumber[T]
	sealed trait SQLFraction[T] extends SQLNumber[T]

	private class Integer[T](implicit cmp :Ordering[T]) extends SQLInteger[T] {
		override def compare(x :T, y :T) = cmp.compare(x, y)
	}

	private class Fraction[T](implicit cmp :Ordering[T]) extends SQLFraction[T] {
		override def compare(x :T, y :T) = cmp.compare(x, y)
	}

	private def integer[T :Ordering] :SQLInteger[T] = new Integer[T]
	private def fraction[T :Ordering] :SQLFraction[T] = new Fraction[T]

	implicit val SQLInt = integer[Int]
	implicit val SQLLong = integer[Long]
	implicit val SQLShort = integer[Short]
	implicit val SQLByte = integer[Byte]
	implicit val SQLDouble = fraction[Double]
	implicit val SQLFloat = fraction[Float]
	implicit val SQLBigInt = integer[BigInt]
	implicit val SQLBigDecimal = fraction[BigDecimal]
	implicit val SQLBigInteger = integer[java.math.BigInteger]
	implicit val SQLJavaBigDecimal = fraction[java.math.BigDecimal]

}

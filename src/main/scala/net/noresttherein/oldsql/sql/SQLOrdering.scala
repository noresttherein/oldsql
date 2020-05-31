package net.noresttherein.oldsql.sql

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime}

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
		case rev :ReverseSQLOrdering[_] => rev.order == this
		case _ => false
	}

}






object SQLOrdering {

	def apply[X :SQLOrdering] :SQLOrdering[X] = implicitly[SQLOrdering[X]]

	private class MappedSQLOrdering[X, Y](lower :Y => X)(implicit order :SQLOrdering[X]) extends SQLOrdering[Y] {
		override def compare(x :Y, y :Y) :Int = order.compare(lower(x), lower(y))
	}

	private class ReverseSQLOrdering[X](val order :SQLOrdering[X]) extends SQLOrdering[X] {
		override def compare(x :X, y :X) :Int = order.compare(y, x)
	}



	private class Adapter[X](implicit order :Ordering[X]) extends SQLOrdering[X] {
		override def compare(x :X, y :X) :Int = order.compare(x, y)
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






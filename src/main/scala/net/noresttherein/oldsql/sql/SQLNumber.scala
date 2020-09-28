package net.noresttherein.oldsql.sql

/**
  * @author Marcin Mościcki
  */
sealed trait SQLNumber[T] extends Serializable//extends SQLOrdering[T]

object SQLNumber {
	//consider: making these two ColumnForms - if we don't need any extra features at least
	sealed trait SQLInteger[T] extends SQLNumber[T]
	sealed trait SQLFraction[T] extends SQLNumber[T]

	private class Integer[T] extends SQLInteger[T]
	private class Fraction[T] extends SQLFraction[T]

	private def integer[T] :SQLInteger[T] = new Integer[T]
	private def fraction[T] :SQLFraction[T] = new Fraction[T]

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


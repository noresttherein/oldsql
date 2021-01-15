package net.noresttherein.oldsql.schema.bits

import java.{sql, time, util}
import java.time.{Clock, Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime, ZoneId, ZoneOffset}






/** A type class for various standard classes describing moments in time. Used by various timestamp-related buffs
  * in order to automatically provide current values. It covers all appropriate classes from the `java.time`
  * and `java.sql` packages, and if an implicit [[java.time.Clock]] is present, it will be used to generate 'current'
  * timestamps instead of the jvm time.
  * @author Marcin Mościcki
  */
final class Temporal[T](val now :()=>T)



sealed abstract class DefaultTemporalImplicits {

	implicit final val SQLDate = new Temporal(() => new sql.Date(System.currentTimeMillis))
	implicit final val SQLTime = new Temporal(() => new sql.Time(System.currentTimeMillis))
	implicit final val SQLTimestamp = new Temporal(() => new sql.Timestamp(System.currentTimeMillis))

	implicit final val JavaDate = new Temporal(() => new util.Date)

	implicit final val JavaInstant = new Temporal(() => Instant.now)
	implicit final val JavaZonedDateTime = new Temporal(() => ZonedDateTime.now)
	implicit final val JavaOffsetDateTime = new Temporal(() => OffsetDateTime.now)
	implicit final val JavaLocalDateTime = new Temporal(() => LocalDateTime.now)
	implicit final val JavaLocalDate = new Temporal(() => LocalDate.now)
	implicit final val JavaOffsetTime = new Temporal(() => OffsetTime.now)
	implicit final val JavaLocalTime = new Temporal(() => LocalTime.now)
}



object Temporal extends DefaultTemporalImplicits {
	def apply[T](implicit temporal :Temporal[T]) :Temporal[T] = temporal

	def apply[T](generator: =>T) :Temporal[T] = new Temporal(() => generator)

	implicit def sqlDate(implicit clock :Clock) = apply(new sql.Date(clock.millis))
	implicit def SQLTime(implicit clock :Clock) = apply(new sql.Time(clock.millis))
	implicit def sqlTimestamp(implicit clock :Clock) = apply(new sql.Timestamp(clock.millis))

	implicit def javaDate(implicit clock :Clock) = apply(new util.Date(clock.millis))

	implicit def javaInstant(implicit clock :Clock) = apply(Instant.now(clock))
	implicit def javaZonedDateTime(implicit clock :Clock) = apply(ZonedDateTime.now(clock))
	implicit def javaOffsetDateTime(implicit clock :Clock) = apply(OffsetDateTime.now(clock))
	implicit def javaLocalDateTime(implicit clock :Clock) = apply(LocalDateTime.now(clock))
	implicit def javaLocalDate(implicit clock :Clock) = apply(LocalDate.now(clock))
	implicit def javaOffsetTime(implicit clock :Clock) = apply(OffsetTime.now(clock))
	implicit def javaLocalTime(implicit clock :Clock) = apply(LocalTime.now(clock))

}






/** A light wrapper over [[java.time.ZoneId]] to be used as implicit indicating the time zone local to the used
  * database. This allows [[net.noresttherein.oldsql.schema.SQLForm forms]] which support it to convert temporal
  * objects with no associated time zone (such as [[java.time.LocalDate date]]) to timestamp-based data specific
  * to the implicit time zone, rather than using the local time zone of the virtual machine.
  */
case class DatabaseTimeZone(ZoneId :ZoneId)



object DatabaseTimeZone {
	val UTC = DatabaseTimeZone(ZoneOffset.UTC)
}

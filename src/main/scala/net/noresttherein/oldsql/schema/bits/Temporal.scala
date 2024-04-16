package net.noresttherein.oldsql.schema.bits

import java.sql.{Date, Time}
import java.{sql, util}
import java.time.{Clock, Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZoneId, ZoneOffset, ZonedDateTime}






/** A type class for various standard classes describing moments in time. Used by various timestamp-related buffs
  * in order to automatically provide current values. It covers all appropriate classes from the `java.time`
  * and `java.sql` packages, and if an implicit [[java.time.Clock]] is present, it will be used to generate 'current'
  * timestamps instead of the jvm time.
  * @author Marcin Mościcki
  */
final class Temporal[T](val now :() => T) extends Serializable



private[schema] sealed abstract class DefaultTemporalImplicits {

	implicit final val SQLDate :Temporal[Date] = new Temporal(() => new sql.Date(System.currentTimeMillis))
	implicit final val SQLTime :Temporal[Time] = new Temporal(() => new sql.Time(System.currentTimeMillis))
	implicit final val SQLTimestamp :Temporal[sql.Timestamp] =
		new Temporal(() => new sql.Timestamp(System.currentTimeMillis))

	implicit final val JavaDate           :Temporal[util.Date]      = new Temporal(() => new util.Date)

	implicit final val JavaInstant        :Temporal[Instant]        = new Temporal(() => Instant.now)
	implicit final val JavaZonedDateTime  :Temporal[ZonedDateTime]  = new Temporal(() => ZonedDateTime.now)
	implicit final val JavaOffsetDateTime :Temporal[OffsetDateTime] = new Temporal(() => OffsetDateTime.now)
	implicit final val JavaLocalDateTime  :Temporal[LocalDateTime]  = new Temporal(() => LocalDateTime.now)
	implicit final val JavaLocalDate      :Temporal[LocalDate]      = new Temporal(() => LocalDate.now)
	implicit final val JavaOffsetTime     :Temporal[OffsetTime]     = new Temporal(() => OffsetTime.now)
	implicit final val JavaLocalTime      :Temporal[LocalTime]      = new Temporal(() => LocalTime.now)
}



object Temporal extends DefaultTemporalImplicits {
	def apply[T](implicit temporal :Temporal[T]) :Temporal[T] = temporal

	def apply[T](generator: =>T) :Temporal[T] = new Temporal(() => generator)

	implicit def sqlDate(implicit clock :Clock) :Temporal[Date] = apply(new sql.Date(clock.millis))
	implicit def SQLTime(implicit clock :Clock) :Temporal[Time] = apply(new sql.Time(clock.millis))
	implicit def sqlTimestamp(implicit clock :Clock) :Temporal[sql.Timestamp] = apply(new sql.Timestamp(clock.millis))

	implicit def javaDate(implicit clock :Clock)           :Temporal[util.Date] = apply(new util.Date(clock.millis))

	implicit def javaInstant(implicit clock :Clock)        :Temporal[Instant] = apply(Instant.now(clock))
	implicit def javaZonedDateTime(implicit clock :Clock)  :Temporal[ZonedDateTime] = apply(ZonedDateTime.now(clock))
	implicit def javaOffsetDateTime(implicit clock :Clock) :Temporal[OffsetDateTime] = apply(OffsetDateTime.now(clock))
	implicit def javaLocalDateTime(implicit clock :Clock)  :Temporal[LocalDateTime] = apply(LocalDateTime.now(clock))
	implicit def javaLocalDate(implicit clock :Clock)      :Temporal[LocalDate] = apply(LocalDate.now(clock))
	implicit def javaOffsetTime(implicit clock :Clock)     :Temporal[OffsetTime] = apply(OffsetTime.now(clock))
	implicit def javaLocalTime(implicit clock :Clock)      :Temporal[LocalTime] = apply(LocalTime.now(clock))

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

package net.noresttherein.oldsql.schema.forms

import java.sql
import java.sql.Timestamp
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import net.noresttherein.oldsql.schema.ColumnForm






/** A trait extended (indirectly) by all forms, which brings into scope implicit definitions from
  * its [[net.noresttherein.oldsql.schema.forms.LocalTimeForms$ companion object]].
  */
//trait LocalTimeForms



/** Implicit definitions of [[net.noresttherein.oldsql.schema.ColumnForm forms]] for classes from the `java.time`
  * package which use the default time zones of the JVM and DBMS when interpreting epoch milliseconds as
  * date-like objects, especially those without a specified time zone ('Local' types).
  * These may be inappropriate if the time zones of the JVM and DBMS differ.
  * For this reason, implicit definitions from [[net.noresttherein.oldsql.schema.forms.TimeForms$ TimeForms]]
  * take precedence over these values.
  */
trait LocalTimeForms {
	import ColumnForm.map

	implicit val LocalDateForm :ColumnForm[LocalDate] = map[sql.Date, LocalDate]("LocalDate")(
		date => if (date == null) null else date.toLocalDate)(
		local => if (local == null) null else sql.Date.valueOf(local)
	)

	implicit val LocalTimeForm :ColumnForm[LocalTime] = map[sql.Time, LocalTime]("LocalTime")(
		time => if (time == null) null else time.toLocalTime)(
		local => if (local == null) null else sql.Time.valueOf(local),
	)

	implicit val LocalDateTimeForm :ColumnForm[LocalDateTime] = map[sql.Timestamp, LocalDateTime](
		"LocalDateTime")(
		time => if (time == null) null else time.toLocalDateTime)(
		local => if (local == null) null else sql.Timestamp.valueOf(local)
	)

}






/** A trait extended (indirectly) by all forms, which brings into scope implicit definitions from
  * its [[net.noresttherein.oldsql.schema.forms.TimeForms$ companion object]].
  */
//trait TimeForms extends LocalTimeForms



/**
  * @author Marcin Mościcki
  */
trait TimeForms extends LocalTimeForms {
	import ColumnForm.map

	implicit val InstantForm :ColumnForm[Instant] = map[sql.Timestamp, Instant](
		"Instant")(
		t => if (t == null) null else Instant.ofEpochMilli(t.getTime))(
		i => new Timestamp(i.toEpochMilli)
	)
	//todo: verify how these work when the timestamps differ


}



package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.SQLForm.{MappedSQLForm, NullValue}
import java.sql
import java.sql.Timestamp
import java.time.Instant

import net.noresttherein.oldsql.schema.ColumnForm.{DerivedColumnForm, MappedColumnForm}

/**
  * @author Marcin Mościcki
  */
object JavaSQLTypes {



	private def derive[S :ColumnForm, T :NullValue](map :S => T, unmap :T => S, name :String) :ColumnForm[T] =
		new DerivedColumnForm[S, T](map, unmap, name)

	implicit val InstantForm :ColumnForm[Instant] = derive[sql.Timestamp, Instant](
			t => if (t == null) null else Instant.ofEpochMilli(t.getTime),
			i => new Timestamp(i.toEpochMilli),
			"Instant"
	)



}

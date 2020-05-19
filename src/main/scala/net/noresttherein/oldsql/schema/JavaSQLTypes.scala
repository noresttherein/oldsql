package net.noresttherein.oldsql.schema

import java.sql
import java.sql.Timestamp
import java.time.Instant
import java.util.Optional

import net.noresttherein.oldsql.schema.ColumnForm.DerivedColumnForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue



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



	implicit def OptionalNullForm[T >: Null :SQLForm] :SQLForm[Optional[T]] =
		SQLForm[T].bimap[Optional[T]](Optional.ofNullable)(_.orElse(null))

	implicit def OptionalForm[T :SQLForm] :SQLForm[Optional[T]] =
		SQLForm[T].biflatMap(t => Some(Optional.ofNullable(t))) {
			case some if some.isPresent => Some(some.get)
			case _ => None
		}

}

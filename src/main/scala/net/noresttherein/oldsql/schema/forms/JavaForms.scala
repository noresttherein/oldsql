package net.noresttherein.oldsql.schema.forms

import java.lang.{Boolean => JBoolean, Byte => JByte, Character => JChar, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort}
import java.math.{BigDecimal => JBigDecimal}
import java.sql.{PreparedStatement, ResultSet}
import java.sql.JDBCType.{BIGINT, BOOLEAN, CHAR, DECIMAL, DOUBLE, FLOAT, INTEGER, SMALLINT, TINYINT}
import java.util.Optional

import net.noresttherein.oldsql.schema.SQLForm
import net.noresttherein.oldsql.schema.ColumnForm.NullableJDBCForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue






/** Trait mixed in by all forms solely to bring into the implicit search scope declarations from its companion object. */
//trait JavaForms extends TimeForms






/** Implicit `SQLForm` definitions for common Java classes, in particular 'competing' with standard Scala types,
  * such as boxed primitives in the `java.lang` package and `Optional`.  All these forms are in the implicit search
  * scope for all form classes, so, prefer to rely on implicit resolution by using `SQLForm[Xxx]` to access a form
  * for the type `Xxx` instead of explicit references to declarations in this object and others.
  * @author Marcin Mościcki
  */ //todo: date time forms
trait JavaForms extends TimeForms {

	private[this] implicit val NotNull = NullValue.NotNull



	implicit def OptionalNullForm[T >: Null :SQLForm] :SQLForm[Optional[T]] =
		SQLForm[T].bimap[Optional[T]](Optional.ofNullable)(_.orElse(null))

	implicit def OptionalForm[T :SQLForm] :SQLForm[Optional[T]] =
		SQLForm[T].biflatMap(t => Some(Optional.ofNullable(t))) {
			case some if some.isPresent => Some(some.get)
			case _ => None
		}






	implicit case object JavaBigDecimalForm extends NullableJDBCForm[JBigDecimal](DECIMAL) {

		override def set(statement :PreparedStatement, position :Int, value :JBigDecimal) :Unit =
			statement.setBigDecimal(position, value)

		protected override def read(res: ResultSet, column :Int) :JBigDecimal = res.getBigDecimal(column)

		override def toString = "JDECIMAL"
	}


	implicit case object JavaBooleanForm extends NullableJDBCForm[JBoolean](BOOLEAN) {

		override def set(statement :PreparedStatement, position :Int, value :JBoolean) :Unit =
			statement.setBoolean(position, value)

		protected override def read(res :ResultSet, column :Int) :JBoolean = {
			val bool = res.getBoolean(column)
			if (res.wasNull) null else bool
		}

		override def toString = "JBOOLEAN"
	}


	implicit case object JavaByteForm extends NullableJDBCForm[JByte](TINYINT) {

		override def set(statement :PreparedStatement, position :Int, value :JByte) :Unit =
			statement.setByte(position, value)

		protected override def read(res: ResultSet, column :Int) :JByte = {
			val byte = res.getByte(column)
			if (res.wasNull) null else byte
		}

		override def toString = "JBYTE"
	}


	implicit case object JavaCharForm extends NullableJDBCForm[JChar](CHAR) {
		override protected def read(res :ResultSet, position :Int) :JChar =
			res.getString(position) match {
				case null => null
				case s if s.length != 1 => throw new IllegalArgumentException(
					"Read '" + s + "' instead of a single Char from result set at index " + position
				)
				case c => c.charAt(0)
			}

		override def set(statement :PreparedStatement, position :Int, value :JChar) :Unit =
			statement.setString(position, if (value == null) null else String.valueOf(value))

		override def toString = "JCHAR"
	}


	implicit case object JavaDoubleForm extends NullableJDBCForm[JDouble](DOUBLE) {

		override def set(statement :PreparedStatement, position :Int, value :JDouble) :Unit =
			statement.setDouble(position, value)

		protected override def read(res: ResultSet, column :Int) :JDouble = {
			val double = res.getDouble(column)
			if (res.wasNull) null else double
		}

		override def toString = "JDOUBLE"
	}


	implicit case object JavaFloatForm extends NullableJDBCForm[JFloat](FLOAT) {

		override def set(statement :PreparedStatement, position :Int, value :JFloat) :Unit =
			statement.setFloat(position, value)

		protected override def read(res: ResultSet, column :Int) :JFloat = {
			val float = res.getFloat(column)
			if (res.wasNull) null else float
		}

		override def toString = "JFLOAT"
	}


	implicit case object JavaIntForm extends NullableJDBCForm[JInt](INTEGER) {

		override def set(statement :PreparedStatement, position :Int, value :JInt) :Unit =
			statement.setInt(position, value)

		protected override def read(res: ResultSet, column :Int) :JInt = {
			val int = res.getInt(column)
			if (res.wasNull) null else int
		}

		override def toString = "JINT"
	}


	implicit case object JavaLongForm extends NullableJDBCForm[JLong](BIGINT) {

		override def set(statement :PreparedStatement, position :Int, value :JLong) :Unit =
			statement.setLong(position, value)

		protected override def read(res: ResultSet, column :Int) :JLong = {
			val long = res.getLong(column)
			if (res.wasNull) null else long
		}

		override def toString = "JLONG"
	}


	implicit case object JavaShortForm extends NullableJDBCForm[JShort](SMALLINT) {

		override def set(statement :PreparedStatement, position :Int, value :JShort) :Unit =
			statement.setShort(position, value)

		protected override def read(res: ResultSet, column :Int) :JShort = {
			val short = res.getShort(column)
			if (res.wasNull) null else short
		}

		override def toString = "JSHORT"
	}

}

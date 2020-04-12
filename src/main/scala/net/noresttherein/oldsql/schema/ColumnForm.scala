package net.noresttherein.oldsql.schema

import java.sql.ResultSet

import net.noresttherein.oldsql.schema.ColumnForm.JDBCSQLType
import net.noresttherein.oldsql.schema.ColumnReadForm.{FlatMappedColumnReadForm, LazyColumnReadForm, MappedColumnReadForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.{LazyColumnWriteForm, OptionColumnWriteForm}
import net.noresttherein.oldsql.schema.ScalaForms.OptionForm
import net.noresttherein.oldsql.schema.SQLForm.{CombinedForm, FlatMappedSQLForm, LazyForm, MappedSQLForm, NullableForm, NullValue, OptionForm}



trait BaseColumnForm {
	/** The JDBC code for the underlying column type, as defined by constants in `java.sql.Types`. */
	def sqlType :JDBCSQLType
	
	override def toString :String = sqlType.toString
}



trait ColumnForm[T] extends SQLForm[T] with ColumnReadForm[T] with ColumnWriteForm[T] {

	override def bimap[X :NullValue](map :T => X)(unmap :X => T) :ColumnForm[X] = NullValue[X] match {
		case null =>
			ColumnForm.map[T, X](map)(unmap)(this, nulls.map(map))
		case nulls =>
			ColumnForm.map[T, X](map)(unmap)(this, nulls)
	}


	override def bimap[X](map :T => X, nullValue :X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(NullValue(nullValue))

	override def bimapNull[X](map :T => X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(nulls.map(map))


	override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] = NullValue[X] match {
		case null =>
			ColumnForm.flatMap[T, X](map)(unmap)(this, nulls.flatMap(map))
		case _ =>
			ColumnForm.flatMap(map)(unmap)(this, NullValue[X])
	}

	override def biflatMap[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :ColumnForm[X] =
		biflatMap(map)(unmap)(NullValue(nullValue))

	override def biflatMapNull[X](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		biflatMap(map)(unmap)(nulls.flatMap(map))

	override def asOpt :ColumnForm[Option[T]] = ColumnForm.OptionColumnForm(this)



	override def compatible(other: SQLForm[_]): Boolean = other match {
		case a :ColumnForm[_] => a.sqlType == sqlType
		case _ => false
	}


	
	override def toString :String = sqlType.toString
}






object ColumnForm {

	@inline def apply[X :ColumnForm] :ColumnForm[X] = implicitly[ColumnForm[X]]


	/** Creates a `ColumnForm` delegating all calls to the implicitly provided read and write forms. */
	def combine[T](implicit read :ColumnReadForm[T], write :ColumnWriteForm[T]) :ColumnForm[T] =
		if (read.sqlType != write.sqlType)
			throw new IllegalArgumentException(
				s"Can't combine column forms $read and $write with different underlying sql types: ${read.sqlType} != ${write.sqlType}."
			)
		else
			new CombinedForm[T](read, write) with ColumnForm[T] {
				override val sqlType = r.asInstanceOf[ColumnReadForm[T]].sqlType

				override def read(position :Int)(res :ResultSet) =
					r.asInstanceOf[ColumnReadForm[T]].friendRead(position)(res)
			}



	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None`/`nulls.value` when reading. 
	  */
	def nulls[T :ColumnWriteForm :NullValue] :ColumnForm[T] =
		combine(ColumnReadForm.nulls[T](ColumnWriteForm[T].sqlType), ColumnWriteForm.none[T])

	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None` when reading. All calls to `apply` will result in a `NullPointerException`. 
	  */
	def none[T :ColumnWriteForm] :ColumnForm[T] =
		combine(ColumnReadForm.none[T](ColumnWriteForm[T].sqlType), ColumnWriteForm.none[T])



	/** Creates a dummy form which always returns and writes the same value. */
	def opt[T :ColumnWriteForm :NullValue](value :Option[T]) :ColumnForm[T] =
		combine(ColumnReadForm.opt(ColumnWriteForm[T].sqlType, value), ColumnWriteForm.opt(value))

	/** Creates a dummy form which always returns and writes the same value. */
	def const[T :ColumnWriteForm](value :T) :ColumnForm[T] =
		combine(ColumnReadForm.const(ColumnWriteForm[T].sqlType, value), ColumnWriteForm.const(value))



	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def evalopt[T :ColumnWriteForm :NullValue](value: => Option[T]) :ColumnForm[T] =
		combine(ColumnReadForm.evalopt(ColumnWriteForm[T].sqlType, value), ColumnWriteForm.evalopt(value))

	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def eval[T :ColumnWriteForm](value: => T) :ColumnForm[T] =
		combine(ColumnReadForm.eval(ColumnWriteForm[T].sqlType, value), ColumnWriteForm.eval(value))



	def Lazy[T](delayed: => ColumnForm[T]) :ColumnForm[T] =
		new LazyForm[T](() => delayed) with LazyColumnReadForm[T] with LazyColumnWriteForm[T] with ColumnForm[T] {
			override def form :ColumnForm[T] = super[LazyForm].form.asInstanceOf[ColumnForm[T]]

			override def bimap[X :NullValue](map :T => X)(unmap :X => T) =
				if (isInitialized) form.bimap[X](map)(unmap)
				else Lazy(form.bimap[X](map)(unmap))

			override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) =
				if (isInitialized) form.biflatMap(map)(unmap)
				else Lazy(form.biflatMap[X](map)(unmap))
		}



	def map[S, T](map :S => T)(unmap :T => S)(implicit source :ColumnForm[S], nulls :NullValue[T] = null) :ColumnForm[T] =
		new MappedColumnForm[S, T](map, unmap)

	def flatMap[S :ColumnForm, T :NullValue](map :S => Option[T])(unmap :T => Option[S]) :ColumnForm[T] =
		new FlatMappedColumnForm[S, T](map, unmap)



	implicit def OptionColumnForm[T :ColumnForm] :ColumnForm[Option[T]] =
		new OptionForm[T] with OptionColumnWriteForm[T] with ColumnForm[Option[T]] {
			override val form = ColumnForm[T]
			override def read(position :Int)(res :ResultSet) :Option[T] = ColumnForm[T].opt(position)(res)
		}

	implicit def SomeColumnForm[T :ColumnForm] :ColumnForm[Some[T]] =
		ColumnForm[T].bimapNull(Some.apply)(_.get)






	class JDBCSQLType(val code :Int) extends AnyVal {
		import java.sql.Types //todo: binsearch this or something
		override def toString :String = code match {
			case Types.INTEGER => "INTEGER"
			case Types.SMALLINT => "SMALLINT"
			case Types.TINYINT => "TINYINT"
			case Types.BIGINT => "BIGINT"
			case Types.FLOAT => "FLOAT"
			case Types.DOUBLE => "DOUBLE"
			case Types.REAL => "REAL"
			case Types.DECIMAL => "DECIMAL"
			case Types.NUMERIC => "NUMERIC"
			case Types.BOOLEAN => "BOOLEAN"
			case Types.BIT => "BIT"
			case Types.CHAR => "CHAR"
			case Types.NCHAR => "NCHAR"
			case Types.VARCHAR => "VARCHAR"
			case Types.LONGVARCHAR => "LONGVARCHAR"
			case Types.NVARCHAR => "NVARCHAR"
			case Types.LONGNVARCHAR => "LONGNVARCHAR"
			case Types.VARBINARY => "VARBINARY"
			case Types.LONGVARBINARY => "LONGVARBINARY"
			case Types.DATE => "DATE"
			case Types.TIME => "TIME"
			case Types.TIMESTAMP => "TIMESTAMP"
			case Types.TIME_WITH_TIMEZONE => "TIME_WITH_TIMEZONE"
			case Types.TIMESTAMP_WITH_TIMEZONE => "TIMESTAMP_WITH_TIMEZONE"
			case Types.NULL => "NULL"
			case Types.BLOB => "BLOB"
			case Types.CLOB => "CLOB"
			case Types.NCLOB => "NCLOB"
			case Types.BINARY => "BINARY"
			case Types.SQLXML => "SQLXML"
			case Types.ROWID => "ROWID"
			case Types.ARRAY => "ARRAY"
			case Types.DATALINK => "DATALINK"
			case Types.DISTINCT => "DISTINCT"
			case Types.REF => "REF"
			case Types.REF_CURSOR => "REF_CURSOR"
			case Types.STRUCT => "STRUCT"
			case Types.JAVA_OBJECT => "JAVA_OBJECT"
			case Types.OTHER => "OTHER"
			case _ => "UNKNOWN"
		}
	}

	
	
	object JDBCSQLType {
		@inline implicit def fromInt(code :Int) = new JDBCSQLType(code)
		@inline implicit def toInt(code :JDBCSQLType) :Int = code.code
	}
	
	
	
	
	
	trait NullableColumnForm[T >: Null] extends ColumnForm[T] with NullableForm[T] {
		override def apply(position :Int)(res :ResultSet) :T = read(position)(res)
	}



	class MappedColumnForm[S :ColumnForm, T :NullValue](map :S => T, unmap :T => S)
		extends MappedSQLForm(map, unmap) with MappedColumnReadForm[S, T] with ColumnForm[T]

	class FlatMappedColumnForm[S :ColumnForm, T :NullValue](map :S => Option[T], unmap :T => Option[S])
		extends FlatMappedSQLForm(map, unmap) with FlatMappedColumnReadForm[S, T] with ColumnForm[T]



	class DerivedColumnForm[S :ColumnForm, T :NullValue](map :S => T, unmap :T => S, override val toString :String)
		extends MappedColumnForm[S, T](map, unmap)

}

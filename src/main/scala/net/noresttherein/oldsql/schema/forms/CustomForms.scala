package net.noresttherein.oldsql.schema.forms

import java.sql.{JDBCType, PreparedStatement, ResultSet}

import scala.collection.immutable.Seq

import net.noresttherein.oldsql.collection.{ConstSeq, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{NullValueException, OldSQLException}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnReadForm.DirectColumnReadForm
import net.noresttherein.oldsql.schema.ColumnWriteForm.{DirectColumnWriteForm, NonLiteralColumnWriteForm, SingletonColumnWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.{NotNullForm, NullValue}
import net.noresttherein.oldsql.schema.SQLReadForm.{AbstractSQLReadForm, NotNullReadForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.{AbstractSQLWriteForm, NonLiteralWriteForm, NotNullWriteForm, NullSafeWriteForm}
import net.noresttherein.oldsql.slang.classNameMethods






private[schema] class CustomSQLReadForm[+T](columns :Int, protected override val text :Opt[String] = Lack,
                                            protected val suffix :String = "")
                                           (reader :(ResultSet, Int) => T)
	extends AbstractSQLReadForm[T](columns, text)(NullValue.NotNull)
{
	protected def read = reader

	override def columnTypes :Seq[JDBCType] =
		throw new UnsupportedOperationException("Column types of form " + this + " are undefined.")

	override def apply(res :ResultSet, position :Int) :T = reader(res, position)
	override def opt(res :ResultSet, position :Int) :Opt[T] = Opt(reader(res, position))

	override def nullValue :T = throw new NullValueException("Null value not allowed for " + this + ".")
	override def guardedNullValue(res :ResultSet, position :Int) :T =
		throw new NullValueException(errorMessage(res, position))

	override def notNull :SQLReadForm[T] =
		new CustomSQLReadForm[T](columnCount, text, if (suffix.length == 0) ".notNull" else suffix + ".notNull")(reader)
			with NotNullCustomReadForm[T] with ComparableReadForm

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :CustomSQLReadForm[_] if other canEqual this =>
			columnCount == other.columnCount && read == other.read && nulls == other.nulls
		case _ => false
	}
	override def hashCode :Int = read.hashCode * 31 + nulls.hashCode

	private[schema] override lazy val cachedString =
		if (text.isDefined) text.get + ">" + suffix
		else "#" + columnCount + "@" + this.shortHashString + ">" + suffix
}


private[schema] trait NotNullCustomReadForm[+T] extends CustomSQLReadForm[T] with NotNullReadForm[T] {
	override val nulls :NullValue[T] = NullValue.NotNull
	//optimisation for the common case
	override def apply(res :ResultSet, position :Int) :T = {
		val t = read(res, position)
		if (t == null)
			throw new NullValueException(try {
				errorMessage(res, position)
			} catch {
				case e :Exception =>
					val thrown = new NullValueException("Null values not allowed for " + this + ".")
					thrown.addSuppressed(e)
					throw thrown
			})
		t
	}
}


private[schema] class CustomColumnReadForm[+T](override val sqlType :JDBCType,
                                               protected override val text :Opt[String] = Lack,
                                               protected override val suffix :String = "")
                                              (reader :(ResultSet, Int) => T)
	extends CustomSQLReadForm[T](1, text, suffix)(reader) with DirectColumnReadForm[T]
{
	protected override def get(res :ResultSet, position :Int) = reader(res, position)

	override def notNull :ColumnReadForm[T] =
		new CustomColumnReadForm[T](sqlType, text, if (suffix.length == 0) ".notNull" else suffix + ".notNull")(reader)
			with NotNullCustomReadForm[T] with ComparableReadForm

	private[schema] override lazy val cachedString =
		if (text.isDefined) text.get + ">" + suffix
		else sqlType.toString + "@" + this.shortHashString + ">" + suffix
}




private[schema] class CustomOptSQLReadForm[+T :NullValue](columns :Int, protected override val text :Opt[String] = Lack,
                                                          protected val suffix :String = "")
                                                         (reader :(ResultSet, Int) => Opt[T])
	extends AbstractSQLReadForm[T](columns, text)
{
	protected def read = reader
	override def columnTypes :Seq[JDBCType] =
		throw new UnsupportedOperationException("Custom form " + this + " has undefined column types.")

	override def opt(res :ResultSet, position :Int) :Opt[T] = reader(res, position)

	override def notNull :SQLReadForm[T] = {
		val notNullSuffix = if (suffix.length == 0) ".notNull" else suffix + ".notNull"
		new CustomOptSQLReadForm[T](columnCount, text, notNullSuffix)(reader)(NullValue.NotNull)
			with ComparableReadForm
		{
			override def notNull :this.type = this
		}
	}

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :CustomOptSQLReadForm[_] if other canEqual this =>
			columnCount == other.columnCount && read == other.read && nulls == other.nulls
		case _ => false
	}
	override def hashCode :Int = read.hashCode * 31 + nulls.hashCode

	private[schema] override lazy val cachedString =
		if (text.isDefined) text.get + ">" + suffix
		else "@" + this.shortHashString + ">" + suffix
}


private[schema] class CustomOptColumnReadForm[+T :NullValue]
                                             (override val sqlType :JDBCType,
                                              protected override val text :Opt[String] = Lack,
                                              protected override val suffix :String = "")
                                             (reader :(ResultSet, Int) => Opt[T])
	extends CustomOptSQLReadForm[T](1, text, suffix)(reader) with ColumnReadForm[T]
{
	override def notNull :ColumnReadForm[T] = {
		val notNullSuffix = if (suffix.length == 0) ".notNull" else suffix + ".notNull"
		new CustomOptColumnReadForm[T](sqlType, text, notNullSuffix)(reader)(NullValue.NotNull) with ComparableReadForm {
			override def notNull = this
		}
	}

	private[schema] override lazy val cachedString =
		if (text.isDefined) text.get + ">" + suffix
		else sqlType.toString + "@" + this.shortHashString + ">" + suffix
}






private[schema] trait CustomWriteForm[-T] extends NonLiteralWriteForm[T] {
	protected def write :(PreparedStatement, Int, T) => Unit

	override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
		write(statement, position, value)

	override def setNull(statement :PreparedStatement, position :Int) :Unit = {
		try {
			var i = columnCount
			while (i > 0) {
				i -= 1
				statement.setNull(position + i, JDBCType.NULL.getVendorTypeNumber)
			}
		} catch {
			case e :OldSQLException =>
				throw e.addInfo(toString + ": " + e.getMessage)
			case e :NullPointerException =>
				throw new NullPointerException(toString + " does not support null values.").initCause(e)
			case e :NoSuchElementException =>
				throw new NoSuchElementException(toString + ": " + e.getMessage).initCause(e)
		}
	}

	override def nullColumnLiterals :Seq[String] = ConstSeq("null", columnCount)

	override def split :Seq[ColumnWriteForm[T]] =
		throw new UnsupportedOperationException("Function adapter form " + this + " cannot be split into columns.")

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :CustomWriteForm[_] if other canEqual this =>
			columnCount == other.columnCount && write == other.write
	}
	override def hashCode :Int = write.hashCode
}


private[schema] class CustomSQLWriteForm[-T](columns :Int, protected override val text :Opt[String] = Lack,
                                             protected val suffix :String = "")
                                            (protected override val write :(PreparedStatement, Int, T) => Unit)
	extends AbstractSQLWriteForm[T](columns, text) with CustomWriteForm[T]
{
	override def columnTypes :Seq[JDBCType] =
		throw new UnsupportedOperationException("Column types of form " + this + " are undefined.")

	@inline protected final def composeSuffix(suffix :String) =
		if (this.suffix.length == 0) suffix else this.suffix + suffix

	override def nullSafe :SQLWriteForm[T] =
		new CustomSQLWriteForm[T](columnCount, text, composeSuffix(".nullSafe"))(write)
			with NullSafeWriteForm[T] with ComparableWriteForm
		{
			override def notNull :SQLWriteForm[T] = CustomSQLWriteForm.this.notNull
		}
	override def notNull :SQLWriteForm[T] =
		new CustomSQLWriteForm[T](columnCount, text, composeSuffix(".notNull"))(write)
			with NotNullWriteForm[T]
		{
			override def nullSafe :this.type = this
		}

	private[schema] override lazy val cachedString =
		if (text.isDefined) "<" + text.get + suffix
		else "<#" + columnCount + "@" + this.shortHashString + suffix
}


private[schema] class CustomColumnWriteForm[-T](override val sqlType :JDBCType,
                                                protected override val text :Opt[String] = Lack,
                                                protected override val suffix :String = "")
                                               (protected override val write :(PreparedStatement, Int, T) => Unit)
	extends CustomSQLWriteForm[T](1, text, suffix)(write)
	   with DirectColumnWriteForm[T] with NonLiteralColumnWriteForm[T] with SingletonColumnWriteForm[T]
{
	override def nullSafe :ColumnWriteForm[T] =
		new CustomColumnWriteForm[T](sqlType, text, ".nullSafe")(write)
			with NullSafeWriteForm[T] with ComparableWriteForm
		{
			override def notNull :ColumnWriteForm[T] = CustomColumnWriteForm.this.notNull
		}
	override def notNull :ColumnWriteForm[T] =
		new CustomColumnWriteForm[T](sqlType, text, ".notNull")(write) with NotNullWriteForm[T] {
			override def nullSafe :this.type = this
		}

	private[schema] override lazy val cachedString :String =
		if (text.isDefined) "<" + text.get + suffix
		else "<" + sqlType + "@" + this.shortHashString + suffix
}




private[schema] trait CustomOptWriteForm[-T] extends NonLiteralWriteForm[T] {
	protected def write :(PreparedStatement, Int, Opt[T]) => Unit

	override def setOpt(statement :PreparedStatement, position :Int, value :Opt[T]) :Unit =
		write(statement, position, value)

	override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
		write(statement, position, Opt(value))

	override def setNull(statement :PreparedStatement, position :Int) :Unit =
		write(statement, position, Lack)

	override def nullColumnLiterals :Seq[String] = ConstSeq("null", columnCount)

	override def split :Seq[ColumnWriteForm[T]] =
		throw new UnsupportedOperationException("Function adapter form " + this + " cannot be split into columns.")

	override def nullSafe :this.type = this

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :CustomOptWriteForm[_] if other canEqual this =>
			columnCount == other.columnCount && write == other.write
		case _ => false
	}
	override def hashCode :Int = columnCount.hashCode * 31 + write.hashCode

}


private[schema] class CustomOptSQLWriteForm[-T](columns :Int, protected override val text :Opt[String] = Lack,
                                                protected val suffix :String = "")
                                               (protected override val write :(PreparedStatement, Int, Opt[T]) => Unit)
	extends AbstractSQLWriteForm[T](columns, text) with CustomOptWriteForm[T]
{
	override def columnTypes :Seq[JDBCType] =
		throw new UnsupportedOperationException("Function adapter form " + this + " has undefined column types.")

	override def notNull :SQLWriteForm[T] =
		new CustomOptSQLWriteForm[T](columnCount, text, ".notNull")(write)
			with NotNullWriteForm[T] with ComparableWriteForm

	private[schema] override lazy val cachedString =
		if (text.isDefined) "<" + text.get + suffix
		else "<#" + columnCount + "@" + this.shortHashString + suffix
}


private[schema] class CustomOptColumnWriteForm[-T]
                      (override val sqlType :JDBCType,
                       protected override val text :Opt[String] = Lack, protected override val suffix :String = "")
                      (protected override val write :(PreparedStatement, Int, Opt[T]) => Unit)
	extends CustomOptSQLWriteForm[T](1, text, suffix)(write)
		with DirectColumnWriteForm[T] with NonLiteralColumnWriteForm[T] with SingletonColumnWriteForm[T]
{
	override def nullSafe :this.type = this
	override def notNull :ColumnWriteForm[T] =
		new CustomOptColumnWriteForm[T](sqlType, text, ".notNull")(write)
			with NotNullWriteForm[T] with ComparableWriteForm

	private[schema] override lazy val cachedString :String =
		if (text.isDefined) "<" + text.get else "<" + sqlType + "@" + this.shortHashString
}






private[schema] trait CustomFormEquality[T] extends AbstractSQLReadForm[T] with SQLForm[T] {
	protected def read  :(ResultSet, Int) => Any
	protected def write :(PreparedStatement, Int, Nothing) => Unit

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :CustomFormEquality[_] if other canEqual this =>
			read == other.read && write == other.write && nulls == other.nulls
		case _ => false
	}
	override def hashCode :Int = ((read.hashCode * 31) + write.hashCode) * 31 + nulls.hashCode

	private[schema] override lazy val cachedString =
		if (text.isDefined) "<" + text.get + ">" else "<#" + columnCount + "@" + this.shortHashString + ">"
}


private[schema] class CustomSQLForm[T](columns :Int, protected override val text :Opt[String] = Lack,
                                       protected override val suffix :String = "")
                                      (protected override val read :(ResultSet, Int) => T,
                                       protected override val write :(PreparedStatement, Int, T) => Unit)
	extends CustomSQLReadForm[T](columns, text, suffix)(read) with CustomWriteForm[T] with CustomFormEquality[T]
{
	override def nullSafe :SQLForm[T] =
		new CustomSQLForm[T](columnCount, text, ".nullSafe")(read, write)
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = CustomSQLForm.this.notNull
		}
	override def notNull :SQLForm[T] =
		new CustomSQLForm[T](columnCount, text, ".notNull")(read, write)
			with NotNullCustomReadForm[T] with NotNullForm[T] with ComparableReadForm with ComparableWriteForm
}


private[schema] class CustomColumnForm[T](override val sqlType :JDBCType, protected override val text :Opt[String] = Lack,
                                          protected override val suffix :String = "")
                                         (protected override val read :(ResultSet, Int) => T,
                                          protected override val write :(PreparedStatement, Int, T) => Unit)
	extends CustomColumnReadForm[T](sqlType, text, suffix)(read) with CustomWriteForm[T] with ColumnForm[T]
	   with DirectColumnWriteForm[T] with NonLiteralColumnWriteForm[T] with SingletonColumnWriteForm[T]
	   with CustomFormEquality[T]
{
	override def nullSafe :ColumnForm[T] =
		new CustomColumnForm[T](sqlType, text, ".nullSafe")(read, write)
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = CustomColumnForm.this.notNull
		}
	override def notNull :ColumnForm[T] =
		new CustomColumnForm(sqlType, text, ".notNull")(read, write)
			with NotNullCustomReadForm[T] with NotNullForm[T] with ComparableReadForm with ComparableWriteForm

	private[schema] override lazy val cachedString :String =
		if (text.isDefined) "<" + text.get + ">" else "<" + sqlType + "@" + this.shortHashString + ">"
}




private[schema] class CustomOptSQLForm[T :NullValue]
                                      (columns :Int, protected override val text :Opt[String] = Lack,
                                       protected override val suffix :String = "")
                                      (protected override val read :(ResultSet, Int) => Opt[T],
                                       protected override val write :(PreparedStatement, Int, Opt[T]) => Unit)
	extends CustomOptSQLReadForm[T](columns, text, suffix)(read) with CustomOptWriteForm[T] with CustomFormEquality[T]
{
	override def nullSafe :this.type = this
	override def notNull :SQLForm[T] =
		new CustomOptSQLForm[T](columnCount, text, ".notNull")(read, write)(NullValue.NotNull)
			with NotNullForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override val nulls = NullValue.NotNull
		}
}

private[schema] class CustomOptColumnForm[T :NullValue]
                                         (override val sqlType :JDBCType,
                                          protected override val text :Opt[String] = Lack,
                                          protected override val suffix :String = "")
                                         (protected override val read :(ResultSet, Int) => T,
                                          protected override val write :(PreparedStatement, Int, Opt[T]) => Unit)
	extends CustomColumnReadForm[T](sqlType, text, suffix)(read) with CustomOptWriteForm[T] with ColumnForm[T]
	   with DirectColumnWriteForm[T] with NonLiteralColumnWriteForm[T] with SingletonColumnWriteForm[T]
	   with CustomFormEquality[T]
{
	override def nullSafe :this.type = this
	override def notNull :ColumnForm[T] =
		new CustomOptColumnForm[T](sqlType, text, ".notNull")(read, write)(NullValue.NotNull)
			with NotNullForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override val nulls = NullValue.NotNull;
		}

	private[schema] override lazy val cachedString :String =
		if (text.isDefined) "<" + text.get + ">" else "<" + sqlType + "@" + this.shortHashString + ">"
}


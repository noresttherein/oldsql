package net.noresttherein.oldsql.schema.forms

import java.sql.{JDBCType, PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.{ConstSeq, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.{AbstractSQLReadForm, IgnoringReadForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormNullLiterals
import net.noresttherein.oldsql.slang.OptionGuardExtension




private[schema] class CaseSQLReadForm[D, +T](discriminator :SQLReadForm[D], cases :Map[D, SQLReadForm[T]],
                                             override val text :Opt[String] = Lack)
                                            (implicit override val nulls :NullValue[T] =
                                            cases.headOption.mapOrElse(_._2.nulls, NullValue.NotNull))
	extends AbstractSQLReadForm[T](discriminator.columnCount + cases.head._2.columnCount, text)
{   //consider: requiring that the forms are comparable with each other.
	if (cases.isEmpty)
		throw new IllegalArgumentException("No cases specified for a conditional form.")

	private[this] val discriminatorColumns = discriminator.columnCount
	override val isUniversal = discriminator.isUniversal && cases.forall(_._2.isUniversal)

	//consider: checking cases.forall(_._2 compatible cases.head._2)
	if (cases.exists(_._2.columnCount != columnCount - discriminatorColumns))
		throw new IllegalArgumentException("Cases differ in the numbers of read columns in " + this + ".")

	override lazy val columnTypes :Seq[JDBCType] =
		discriminator.columnTypes :++ {
			var i = cases.values.iterator
			var paragon :SQLReadForm[T] = null
			var types :Seq[JDBCType] = null
			while (i.hasNext)
				try { paragon = i.next(); types = paragon.columnTypes } catch {
					case _ :UnsupportedOperationException => paragon = null
				}
			if (types == null)
				throw new UnsupportedOperationException(
					"Neither of the case forms in " + this + " defines its column types."
				)
			i = cases.values.iterator
			var same = true
			while (i.hasNext && same) {
				val form = i.next()
				same = try {
					types == form.columnTypes
				} catch {
					case _ :UnsupportedOperationException => paragon comparable form
				}
			}
			if (!same)
				throw new UnsupportedOperationException("Incomparable case forms in " + this + ".")
			types
		}

	override def opt(res :ResultSet, position :Int) :Opt[T] =
		discriminator.opt(res, position) match {
			case Got(key) =>
				try { cases(key).opt(res, position + discriminatorColumns) } catch {
					case _ :NoSuchElementException => Lack
				}
			case _ => Lack
		}

	private def key = discriminator
	private def map = cases

	override def comparable(other :SQLReadForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :CaseSQLReadForm[D @unchecked, _] =>
			(columnCount == other.columnCount && (key comparable other.key) && map.keySet == other.map.keySet &&
				map.forall { case (k, v) => v comparable other.map(k) }
			) || super.comparable(other)
		case _ => super.comparable(other)
	}
	override def equals(that :Any) :Boolean = that match {
		case other :CaseSQLReadForm[_, _] => (this eq other) || key == other.key && map == other.map
		case _ => false
	}
	override def hashCode :Int = discriminator.hashCode * 31 + cases.hashCode

	override lazy val toString :String =
		if (text.isDefined)
			"<" + text.get
		else
			cases.iterator.map(entry => String.valueOf(entry._1) + "->" + entry._2).mkString(
				"When(" + discriminator + "){", ",", "}>"
			)
}






private[schema] class CaseSQLWriteForm[D, -T](choose: T => D, discriminator :SQLWriteForm[D], cases: Map[D, SQLWriteForm[T]],
                                              protected override val text :Opt[String] = Lack)
	extends SQLWriteForm[T] with WriteFormNullLiterals[T]
{   //consider: requiring that the forms are comparable with each other.
	if (cases.isEmpty)
		throw new IllegalArgumentException("No cases specified for a conditional form.")

	private[this] val discriminatorColumns = discriminator.columnCount
	override val columnCount :Int = discriminatorColumns + cases.head._2.columnCount
	override val isUniversal = discriminator.isUniversal && cases.forall(_._2.isUniversal)

	if (cases.exists(_._2.columnCount != columnCount - discriminator.columnCount))
		throw new IllegalArgumentException("Cases differ in the numbers of written columns in form " + this + ".")

	override lazy val columnTypes :Seq[JDBCType] =
		discriminator.columnTypes :++ {
			var i = cases.values.iterator
			var paragon :SQLWriteForm[T] = null
			var types :Seq[JDBCType] = null
			while (i.hasNext)
				try { paragon = i.next(); types = paragon.columnTypes } catch {
					case _ :UnsupportedOperationException => paragon = null
				}
			if (types == null)
				throw new UnsupportedOperationException(
					"Neither of the case forms in " + this + " defines its column types."
				)
			i = cases.values.iterator
			var same = true
			while (i.hasNext && same) {
				val form = i.next()
				same = try {
					types == form.columnTypes
				} catch {
					case _ :UnsupportedOperationException => paragon comparable form
				}
			}
			if (!same)
				throw new UnsupportedOperationException("Incomparable case forms in " + this + ".")
			types
		}

	override def set(statement :PreparedStatement, position :Int, value :T) :Unit = {
		val key = choose(value)
		discriminator.set(statement, position, key)
		try { cases(key).set(statement, position + discriminatorColumns, value) } catch {
			case _ :NoSuchElementException => setNull(statement, position)
		}
	}

	override def setNull(statement :PreparedStatement, position :Int) :Unit = {
		discriminator.setNull(statement, position)
		cases.head._2.setNull(statement, position + discriminatorColumns)
	}

	override def literal(value :T, inline :Boolean) =
		if (value == null)
			nullLiteral(inline)
		else {
			val key = choose(value)
			if (columnCount == discriminatorColumns)
				if (discriminatorColumns != 0)
					discriminator.literal(key, inline)
				else
					try { cases(key).literal(value, inline) } catch {
						case _ :NoSuchElementException if inline => ConstSeq("null", columnCount).mkString(", ")
						case _ :NoSuchElementException => ConstSeq("null", columnCount).mkString("(", ", ", ")")
					}
			else {
				val keyLiteral = discriminator.inlineLiteral(key)
				try {
					if (inline) keyLiteral + ", " + cases(key).inlineLiteral(value)
					else "(" + keyLiteral + ", " + cases(key).inlineLiteral(value) + ")"
				} catch {
					case _ :NoSuchElementException if inline =>
						ConstSeq("null", columnCount - discriminatorColumns).mkString(keyLiteral + ", ", ", ", "")
					case _ :NoSuchElementException =>
						ConstSeq("null", columnCount - discriminatorColumns).mkString("(" + keyLiteral + ", ", ", ", ")")
				}
			}
		}

	override lazy val nullLiteral       :String = newNullLiteral(false)
	override lazy val inlineNullLiteral :String = newNullLiteral(true)

	private def newNullLiteral(inline :Boolean) =
		if (columnCount == discriminatorColumns)
			if (discriminatorColumns != 0)
				discriminator.nullLiteral(inline)
			else
				cases.head._2.nullLiteral(inline)
		else {
			val keyLiteral = discriminator.inlineNullLiteral
			if (inline)
				ConstSeq("null", columnCount - discriminatorColumns).mkString(keyLiteral + ", ", ", ", "")
			else
				ConstSeq("null", columnCount - discriminatorColumns).mkString("(" + keyLiteral + ", ", ", ", ")")
		}

	override def columnLiterals(value :T) :Seq[String] = {
		val key = choose(value)
		try {
			discriminator.columnLiterals(key) ++ cases(key).columnLiterals(value)
		} catch {
			case _ :NoSuchElementException =>
				discriminator.columnLiterals(key) ++ ConstSeq("null", columnCount - discriminatorColumns)
		}
	}
	override lazy val nullColumnLiterals :Seq[String] =
		discriminator.nullColumnLiterals ++ cases.head._2.nullColumnLiterals

	override def split = throw new UnsupportedOperationException("Form " + this + " cannot be split.")


	private def key = discriminator
	private def map = cases

	override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :CaseSQLWriteForm[D @unchecked, _] =>
			(columnCount == other.columnCount && (key comparable other.key) && //map.keySet == other.map.keySet &&
				map.values.exists { form =>
					!form.isInstanceOf[IgnoringReadForm[_]] && other.map.values.exists { otherForm =>
						!otherForm.isInstanceOf[IgnoringReadForm[_]] && form.comparable(otherForm)
					}
				}
//				map.forall { case (k, v) => v comparable other.map(k) }
			) || super.comparable(other)
		case _ => super.comparable(other)
	}
	override def equals(that :Any) :Boolean = that match {
		case other :CaseSQLWriteForm[_, _] => (this eq other) || key == other.key && map == other.map
		case _ => false
	}
	override def hashCode :Int = discriminator.hashCode * 31 + cases.hashCode

	override lazy val toString :String =
		if (text.isDefined)
			text.get + ">"
		else
			cases.iterator.map(entry => String.valueOf(entry._1) + "->" + entry._2).mkString(
				"<When(" + discriminator + "){", ",", "}"
			)
}

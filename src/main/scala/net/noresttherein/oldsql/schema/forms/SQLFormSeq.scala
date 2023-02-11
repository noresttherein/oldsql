package net.noresttherein.oldsql.schema.forms

import java.sql.{JDBCType, PreparedStatement, ResultSet}

import scala.collection.immutable.{ArraySeq, Seq}

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLReadForm.CompositeReadForm
import net.noresttherein.oldsql.schema.SQLWriteForm.{AbstractSQLWriteForm, CompositeWriteForm}
import net.noresttherein.oldsql.slang.mappingMethods






private[schema] trait ReadFormSeq[+T] extends CompositeReadForm[Seq[T]] {
	protected override val forms :Seq[SQLReadForm[T]]

	override def opt(res :ResultSet, position :Int) :Opt[Seq[T]] = {
		var i = position + columnCount
		(forms :\ Option(List.empty[T])) { (form, acc) =>
			acc flatMap { tail =>
				i -= form.columnCount
				form.opt(res, i) match {
					case Got(head) => Some(head::tail)
					case _ => try {
						Some(form.nullValue::tail)
					} catch {
						case _ :Exception => None
					}
				}
			}
		}
	}
	override def apply(res :ResultSet, position :Int) = {
		var i = position
		forms.map { f => val elem = f(res, i); i += f.columnCount; elem }
	}
	private lazy val cachedNullValue = forms.map(_.nullValue)
	override def nullValue :Seq[T] = cachedNullValue

	override def comparable(other :SQLReadForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :ReadFormSeq[_] =>
			columnCount == other.columnCount && (
				(forms.view zip other.forms).forall { case (l, r) => l comparable r }
					|| super.comparable(other)
				)
		case _ => super.comparable(other)
	}

	override lazy val toString :String =
		if (text.isDefined) text.get + ">" else forms.iterator.map(_.toString).mkString("Seq(", ",", ")>")
}


private[schema] case class SQLReadFormSeq[+T](forms :Seq[SQLReadForm[T]],
                                              protected override val text :Opt[String] = Lack)
	extends ReadFormSeq[T]
{
	override val columnCount = super.columnCount
	override def notNull :SQLReadForm[Seq[T]] = new SQLReadFormSeq[T](forms.map(_.notNull))
}





private[schema] case class SQLWriteFormSeq[-T](protected override val forms :Seq[SQLWriteForm[T]],
                                               protected override val text :Opt[String] = Lack)
	extends CompositeWriteForm[Seq[T]]
{
	private def validateLength(value :Seq[T]) :Unit =
		if (value.length != forms.length)
			throw new IllegalArgumentException(
				s"Passed sequence's length differs from the number of forms: $value vs $forms"
			)

	override def set(statement :PreparedStatement, position :Int, value :Seq[T]) :Unit =
		if (value == null)
			setNull(statement, position)
		else {
			var i = position
			validateLength(value)
			value.iterator.zip(forms.iterator) foreach {
				case (x, form) => form.set(statement, i, x); i += form.columnCount
			}
		}

	override def literal(value :Seq[T], inline :Boolean) :String =
		if (value == null)
			nullLiteral(inline)
		else {
			validateLength(value)
			val literals = forms.zipMap(value) { (form, x) =>
				if (x == null) form.inlineNullLiteral else form.inlineLiteral(x)
			}
			if (inline) literals.mkString(", ")
			else literals.mkString("(", ", ", ")")
		}


	override def columnLiterals(value :Seq[T]) :Seq[String] =
		if (value == null)
			nullColumnLiterals
		else {
			validateLength(value)
			forms.zipFlatMap(value) { (form, x) =>
				if (x == null) form.nullColumnLiterals else form.columnLiterals(x)
			}
		}

	override lazy val nullColumnLiterals :Seq[String] = forms.flatMap(_.nullColumnLiterals)

	override def split :Seq[ColumnWriteForm[Seq[T]]] =
		forms.flatMapWithIndex { (f, i) =>
			f.split.map { cf => cf unmap { seq :Seq[T] => seq(i) } }
		}

	override def notNull :SQLWriteForm[Seq[T]] = new SQLWriteFormSeq[T](forms.map(_.notNull))

	override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :SQLWriteFormSeq[_] =>
			columnCount == other.columnCount && (
				(forms.view zip other.forms).forall { case (l, r) => l comparable r }
					|| super.comparable(other)
				)
		case _ => super.comparable(other)
	}

	override lazy val toString :String =
		if (text.isDefined) "<" + text.get else forms.iterator.map(_.toString).mkString("<Seq(", ",", ")")
}






private[schema] class SQLFormSeq[T](override val forms :Seq[SQLForm[T]],
                                    protected override val text :Opt[String] = Lack)
	extends SQLWriteFormSeq[T](forms, text) with ReadFormSeq[T] with SQLForm[Seq[T]]
{
	override def columnCount :Int = super[SQLWriteFormSeq].columnCount
	override def isUniversal :Boolean = super[SQLWriteFormSeq].isUniversal
	override def notNull :SQLForm[Seq[T]] = new SQLFormSeq[T](forms.map(_.notNull))
	override lazy val toString :String =
		if (text.isDefined) "<" + text.get + ">" else forms.iterator.map(_.toString).mkString("<Seq(", ",", ")>")
}


package net.noresttherein.oldsql.schema.forms

import java.sql.{JDBCType, PreparedStatement, ResultSet}

import scala.collection.immutable.{ArraySeq, Seq}

import net.noresttherein.oldsql.collection.{ConstSeq, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals
import net.noresttherein.oldsql.slang.mappingMethods






private[schema] trait RepeatedReadForm[+T] extends SQLReadForm[Seq[T]] {
	protected def form :SQLReadForm[T]
	protected def repeats :Int

	override def columnCount :Int = form.columnCount * repeats
	override lazy val columnTypes :Seq[JDBCType] = (0 until repeats).flatMap(_ => form.columnTypes)
	override def isUniversal = form.isUniversal

	override def opt(res :ResultSet, position :Int): Opt[Seq[T]] = Opt(apply(res, position))

	override def apply(res :ResultSet, position :Int): Seq[T] = {
		val f = form
		var i = position; val jump = form.columnCount
		val result = List.newBuilder[T]
		var countdown = repeats
		while (countdown > 0) f.opt(res, i) match {
			case Got(x) => result += x; countdown -= 1; i += jump
			case _ => countdown -= 1; i += jump
		}
		result.result()
	}

	override def nullValue: Seq[T] = Seq()

	override def comparable(other :SQLReadForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :RepeatedReadForm[_] if repeats == other.repeats => form comparable other.form
		case _ => super.comparable(other)
	}

	override lazy val toString :String =
		if (text.isDefined) text.get + ">" else "(" + repeats.toString + "*" + form + ")"
}


private[schema] case class RepeatedSQLReadForm[+T](form :SQLReadForm[T], repeats :Int) extends RepeatedReadForm[T] {
	def this(repeats :Int)(implicit form :SQLReadForm[T]) = this(form, repeats)

	override def notNull :SQLReadForm[Seq[T]] = new RepeatedSQLReadForm[T](form.notNull, repeats)

	override val columnCount = super.columnCount
}






private[schema] trait RepeatedWriteForm[-T] extends SQLWriteForm[Seq[T]] with WriteFormOptLiterals[Seq[T]] {
	protected def form :SQLWriteForm[T]
	protected def repeats :Int

	override def isUniversal :Boolean = form.isUniversal
	override def columnCount :Int = form.columnCount * repeats
	override lazy val columnTypes :Seq[JDBCType] = (0 until repeats).flatMap { _ => form.columnTypes }

	override def set(statement :PreparedStatement, position :Int, value :Seq[T]) :Unit =
		if (value == null)
			setNull(statement, position)
		else if (value.length > repeats)
			throw new IllegalArgumentException(
				s"Expected maximum $repeats values for form $this, got ${value.length}: $value."
			)
		else {
			val f = form
			var i = position; val jump = f.columnCount
			value foreach { x => f.set(statement, i, x); i += jump }
			val limit = columnCount
			while (i < limit) {
				f.setNull(statement, i); i += jump
			}
		}

	override def setNull(statement :PreparedStatement, position :Int) :Unit = {
		val f = form
		var i = position; val jump = f.columnCount; val limit = columnCount
		while (i < limit) {
			f.setNull(statement, i); i += jump
		}
	}

	override def optLiteral(value :Opt[Seq[T]], inline :Boolean): String =
		value match {
			case Got(seq) if seq ne null =>
				val length = seq.length
				if (length > repeats)
					throw new IllegalArgumentException(
						s"Expected maximum $repeats values for form $this, got ${seq.length}: $value."
					)
				else if (length == repeats)
					if (inline)
						seq.view.map(form.inlineLiteral).mkString(", ")
					else
						seq.view.map(form.inlineLiteral).mkString("(", ", ", ")")
				else {
					val literals = seq.view.map(form.inlineLiteral) ++
						ConstSeq(form.inlineNullLiteral, repeats - length).flatten
					if (inline) literals.mkString(", ")
					else literals.mkString("(", ", ", ")")
				}
			case _ if inline =>
				ConstSeq(form.inlineNullLiteral, repeats).mkString(", ")
			case _ =>
				ConstSeq(form.inlineNullLiteral, repeats).mkString("(", ", ", ")")
		}

	override def optColumnLiterals(value :Opt[Seq[T]]) :Seq[String] = value match {
		case Got(seq) if seq ne null =>
			val length = seq.length
			if (length > repeats)
				throw new IllegalArgumentException(
					s"Expected maximum $repeats values for form $this, got ${seq.length}: $value."
				)
			else if (length == repeats)
				seq.view.flatMap(form.columnLiterals) to ArraySeq
			else
				seq.view.flatMap(form.columnLiterals) ++
					ConstSeq(form.nullColumnLiterals, repeats - length).flatten to ArraySeq
		case _ =>
			ConstSeq(form.nullColumnLiterals, repeats).flatten
	}

	override def split = form.split.mapWithIndex {
		(f, i) => f optUnmap { seq :Seq[T] => if (seq.sizeIs > i) Some(seq(i)) else None }
	}

	override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :RepeatedWriteForm[_] if repeats == other.repeats => form comparable other.form
		case _ => super.comparable(other)
	}

	override lazy val toString :String =
		if (text.isDefined) "<" + text.get else "(" + form.toString + "*" + repeats + ")"
}


private[schema] case class RepeatedSQLWriteForm[-T](form :SQLWriteForm[T], repeats :Int,
                                                    protected override val text :Opt[String] = Lack)
	extends RepeatedWriteForm[T]
{
	def this(count :Int)(implicit form :SQLWriteForm[T]) = this(form, count)

	override def notNull :SQLWriteForm[Seq[T]] = new RepeatedSQLWriteForm[T](form.notNull, repeats)

	override val columnCount :Int = super.columnCount
}






private[schema] case class RepeatedSQLForm[T](form :SQLForm[T], repeats :Int, protected override val text :Opt[String] = Lack)
	extends SQLForm[Seq[T]] with RepeatedWriteForm[T] with RepeatedReadForm[T]
{
	def this(count :Int)(implicit form :SQLForm[T]) = this(form, count)
	override def notNull :SQLForm[Seq[T]] = new RepeatedSQLForm(form.notNull, repeats)

	override val columnCount = form.columnCount * repeats
	override lazy val columnTypes :Seq[JDBCType] = (0 until repeats).flatMap { _ => form.columnTypes }
	override lazy val toString :String = if (text.isDefined) "<" + text + ">" else "<" + repeats + "*" + form + ">"
}


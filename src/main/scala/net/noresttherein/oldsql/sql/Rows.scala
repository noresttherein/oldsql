package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.SQLForm.EmptyForm
import net.noresttherein.oldsql.schema.{SQLReadForm, SQLWriteForm}



/** The value type of `SelectSQL` instances with header (select clause) type `V`.
  * This indirection allows the use of a SQL select expression both as a sequence (for example, inside `exists`)
  * and as a single value (or rather, single row). Implicit conversions exist from `SQLExpression[F, Rows[V]]` to
  * both `SQLExpression[F, V]` and `SQLExpression[F, Seq[V]]`.
  */
trait Rows[+V] {
	def size :Int = seq.size
	def isEmpty :Boolean = seq.isEmpty
	def nonEmpty :Boolean = seq.nonEmpty

	def seq :Seq[V]
	def single :V
	def head :V
	def headOption :Option[V]
}



object Rows {
	def apply[E](items :E*) :Rows[E] =
		if (items.isEmpty || items.sizeIs > 1) MultipleRows(items)
		else new SingleRow(items.head)

	def single[E](item :E) :Rows[E] = new SingleRow(item)

//	def unapplySeq[E](rows :Rows[E]) :Seq[E] = rows.seq



	implicit def readForm[T :SQLReadForm] :SQLReadForm[Rows[T]] = SQLReadForm[T].map((t :T) => Rows(t))

	implicit def writeForm[T :SQLWriteForm] :SQLWriteForm[Rows[T]] =
		EmptyForm(throw new UnsupportedOperationException("SQLWriteForm[Rows]"))


	private case class MultipleRows[+E](seq :Seq[E]) extends Rows[E] {
		def single :E = seq match {
			case Seq(res) => res
			case _ => throw new IllegalStateException("Expected a single result from a Rows instance, got " + seq.size)
		}
		def head :E = seq.head
		def headOption :Option[E] = seq.headOption
	}

	private class SingleRow[E](override val single :E) extends Rows[E] {
		override def head = single
		override def headOption = Some(single)
		override def seq :Seq[E] = single::Nil
	}
}


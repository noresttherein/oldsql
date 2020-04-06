package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.SQLForm.EmptyForm
import net.noresttherein.oldsql.schema.{SQLReadForm, SQLWriteForm}


trait RowCursor[+E] {
	def size :Int = seq.size
	def isEmpty :Boolean = seq.isEmpty
	def nonEmpty :Boolean = seq.nonEmpty

	def seq :Seq[E]
	def single :E
	def head :E
	def headOption :Option[E]
}


object RowCursor {
	def apply[E](items :E*) :RowCursor[E] = Rows(items)


	implicit def readForm[T :SQLReadForm] :SQLReadForm[RowCursor[T]] = SQLReadForm[T].map((t :T) => RowCursor(t))

	implicit def writeForm[T :SQLWriteForm] :SQLWriteForm[RowCursor[T]] =
		EmptyForm(throw new UnsupportedOperationException("SQLWriteForm[RowCursor]")) //SQLWriteForm[T].imap(_.row)


	case class Rows[+E](seq :Seq[E]) extends RowCursor[E] {
		def single :E = seq match {
			case Seq(res) => res
			case _ => throw new IllegalStateException("Expected a single result from a row cursor, got " + seq.size)
		}
		def head :E = seq.head
		def headOption :Option[E] = seq.headOption
	}
}

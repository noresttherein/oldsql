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
	def apply[E](items :E*) :Rows[E] = Rows(items)


	implicit def readForm[T :SQLReadForm] :SQLReadForm[Rows[T]] = SQLReadForm[T].map((t :T) => Rows(t))

	implicit def writeForm[T :SQLWriteForm] :SQLWriteForm[Rows[T]] =
		EmptySQLForm(throw new UnsupportedOperationException("SQLWriteForm[Rows]")) //SQLWriteForm[T].imap(_.row)


	case class Rows[+E](seq :Seq[E]) extends Rows[E] {
		def single :E = seq match {
			case Seq(res) => res
			case _ => throw new IllegalStateException("Expected a single result from a row cursor, got " + seq.size)
		}
		def head :E = seq.head
		def headOption :Option[E] = seq.headOption
	}
}

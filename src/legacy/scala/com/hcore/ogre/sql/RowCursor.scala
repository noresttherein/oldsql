package com.hcore.ogre.sql

import com.hcore.ogre.sql.SQLForm.EmptyForm


trait RowCursor[E] {
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


	case class Rows[E](seq :Seq[E]) extends RowCursor[E] {
		def single = seq match {
			case Seq(res) => res
			case _ => throw new IllegalStateException(s"Expected a single result from a row cursor, got $seq")
		}
		def head = seq.head
		def headOption = seq.headOption
	}
}

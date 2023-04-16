package net.noresttherein.oldsql.schema.forms

import java.sql.{CallableStatement, JDBCType, PreparedStatement, ResultSet}

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.pixies.{RearrangedCallableStatement, Rearrangement, RearrangedPreparedStatement, RearrangedResultSet}
import net.noresttherein.oldsql.schema.{ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals






private[oldsql] trait ReorderedUnspecifiedForm extends UnspecifiedForm {
	protected def form :UnspecifiedForm
	protected[oldsql] def indexing :Rearrangement

	override def columnCount :Int = indexing.underlyingColumnCount

	override lazy val columnTypes :Seq[JDBCType] = indexing.reorder(form.columnTypes, JDBCType.NULL)
	override def isUniversal :Boolean = form.isUniversal && indexing.isSurjection
}



/** A form used to read values from a [[java.sql.ResultSet ResultSet]] where columns appear in different
  * order than than the one expected by `this.form`, possibly interspaced with ignored column sections.
  */ //not a ProxyReadForm because permutation makes the forms incompatible
private[oldsql] trait ReorderedReadForm[+T] extends SQLReadForm[T] with ReorderedUnspecifiedForm {
	protected override def form :SQLReadForm[T]

	override def opt(res :ResultSet, position :Int) :Opt[T] =
		form.opt(RearrangedResultSet(res, position, indexing), 1)

	override def apply(res :ResultSet, position :Int) :T =
		form(RearrangedResultSet(res, position, indexing), 1)

	override def register(call :CallableStatement, position :Int) :Unit =
		form.register(RearrangedCallableStatement(call, position, indexing), 1)

	override def nullValue :T = form.nullValue
	override def nulls :NullValue[T] = form.nulls

	override def notNull = form.notNull match {
		case same if same eq form => this
		case other => ReorderedReadForm(other, indexing)
	}

	override def comparable(other :SQLReadForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :ReorderedReadForm[_] =>
			indexing == other.indexing && (form comparable other.form) || super.comparable(other)
		case _ => super.comparable(other)
	}
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :ReorderedReadForm[_] if other canEqual this => form == other.form && indexing == other.indexing
		case _ => false
	}
	override def hashCode :Int = form.hashCode * 31 + indexing.hashCode

	override lazy val toString :String = form.toString + ".reorder(" + indexing + ")"
}



object ReorderedReadForm {
	/** An adapter to an implicit `SQLReadForm[T]` applying it to [[java.sql.ResultSet ResultSet]]s
	  * in which columns appear out of order expected by the form.
	  * It wraps the `ResultSet` given as the argument to [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]]
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] in a proxy mapping the `n`-th column read
	  * by `form` to column `indexing.`[[net.noresttherein.oldsql.pixies.Rearrangement.apply indexing]]`(n)`
	  * in the wrapped `ResultSet`, or a virtual column returning always `null`
	  * if `!indexing.`[[net.noresttherein.oldsql.pixies.Rearrangement.isMapped isMapped]]`(n)`.
	  * Reading from the new `ResultSet` always starts with the first column (and ends with `form.columnCount`-th column),
	  * regardless of the `position` argument given to the adapter form. If the `position` argument specifying the index
	  * of the first column to be read in the actual `ResultSet` is `x`, then the returned form will read columns
	  * `(0 until form.`[[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]]`).map(x - 1 + indexing(_))`.
	  * The [[net.noresttherein.oldsql.schema.SQLReadForm.columnCount column count]] of the reordered form equals
	  * `indexing.`[[net.noresttherein.oldsql.pixies.Rearrangement.underlyingColumnCount underlyingColumnCount]].
	  */
	def apply[T :SQLReadForm](indexing :Rearrangement) :SQLReadForm[T] =
		ReorderedReadForm(SQLReadForm[T], indexing)

	/** An adapter to an `SQLReadForm[T]` applying it to [[java.sql.ResultSet ResultSet]]s
	  * in which columns appear out of order expected by the form.
	  * It wraps the `ResultSet` given as the argument to [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]]
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] in a proxy mapping the `n`-th column read
	  * in the wrapped `ResultSet`, or a virtual column returning always `null`
	  * if `!indexing.`[[net.noresttherein.oldsql.pixies.Rearrangement.isMapped isMapped]]`(n)`.
	  * Reading from the new `ResultSet` always starts with the first column (and ends with `form.columnCount`-th column),
	  * regardless of the `position` argument given to the adapter form. If the `position` argument specifying the index
	  * of the first column to be read in the actual `ResultSet` is `x`, then the returned form will read columns
	  * `(0 until form.`[[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]]`).map(x - 1 + indexing(_))`.
	  * The [[net.noresttherein.oldsql.schema.SQLReadForm.columnCount column count]] of the reordered form equals
	  * `indexing.`[[net.noresttherein.oldsql.pixies.Rearrangement.underlyingColumnCount underlyingColumnCount]].
	  */
	def apply[T](form :SQLReadForm[T], indexing :Rearrangement) :SQLReadForm[T] = {
		if (form.columnCount != indexing.columnCount)
			throw new IllegalArgumentException(
				"Cannot map input columns for " + form + " with " + indexing + " because the form's column count " +
				form.columnCount + " does not match the output index range of " + indexing.columnCount + " columns."
			)
		class StandardReorderedReadForm(override val form :SQLReadForm[T], override val indexing :Rearrangement)
			extends ReorderedReadForm[T]
		new StandardReorderedReadForm(form, indexing)
	}

	/** An adapter to an implicit `SQLReadForm[T]` which skips certain columns from the [[java.sql.ResultSet ResultSet]]
	  * when reading. The adapted form will be given a proxy `ResultSet`, which 'cuts out' the sections specified
	  * by the argument, that is the `n`-th column of the actual `ResultSet` will either become 'hidden'
	  * if it falls into one of the gaps, or appear at index:
	  * {{{
	  *     n - gaps.scan((0, 0))((pair, gap) => (gap._1, gap._1 + gap._2 + pair._2 - pair._1))
	  *             .takeWhile(_._2 <= n).map(pair => pair._2 - pair._1).last
	  * }}}
	  * @param gaps Indices of skipped columns given as sections specified by `(n, length)` pairs,
	  *             where `1 <= n <= form.`[[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]]
	  *             is the `n`-th column read by the adapted form and `length` is the number of skipped
	  *             underlying columns directly preceding the underlying column corresponding to the `n`-th read column.
	  *             Must be sorted by start indices. Can be empty.
	  */
	def splice[T :SQLReadForm](gaps :IndexedSeq[(Int, Int)]) :SQLReadForm[T] =
		ReorderedReadForm.splice(SQLReadForm[T], gaps)

	/** An adapter to an `SQLReadForm[T]` which skips certain columns from the [[java.sql.ResultSet ResultSet]]
	  * when reading. The adapted form will be given a proxy `ResultSet`, which 'cuts out' the sections specified
	  * by the argument, that is the `n`-th column of the actual `ResultSet` will either become 'hidden'
	  * if it falls into one of the gaps, or appear at index:
	  * {{{
	  *     n - gaps.scan((0, 0))((pair, gap) => (gap._1, gap._1 + gap._2 + pair._2 - pair._1))
	  *             .takeWhile(_._2 <= n).map(pair => pair._2 - pair._1).last
	  * }}}
	  * @param gaps Indices of skipped columns given as sections specified by `(n, length)` pairs,
	  *             where `1 <= n <= form.`[[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]]
	  *             is the `n`-th column read by the adapted form and `length` is the number of skipped
	  *             underlying columns directly preceding the underlying column corresponding to the `n`-th read column.
	  *             Must be sorted by start indices. Can be empty.
	  */
	def splice[T](form :SQLReadForm[T], gaps :IndexedSeq[(Int, Int)]) :SQLReadForm[T] =
		ReorderedReadForm(form, Rearrangement.splice(gaps, form.columnCount))

	/** An adapter to an implicit `SQLReadForm[T]` which uses it to read from [[java.sql.ResultSet ResultSet]]s
	  * in which columns appear out of order. When reading, the adapted `form` is given as an argument
	  * a proxy `ResultSet` whose `n`-th column is mapped to the `position + permutation(n - 1)`-th column
	  * in the underlying result set passed to the form adapter, where `position` is the index of the first column
	  * given as an argument to the adapter form. In other words, the returned form's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] and [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]]
	  * will read columns `position + permutation(0), position + permutation(1),` etc.
	  * Note that shifts by one come from the fact that `ResultSet` column numbering
	  * starts with `1`.
	  * @param permutation A permutation vector of length equal to
	  *                    `form.`[[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]], where
	  *                    every value from range `[0, form.columnCount)` appears exactly once.
	  */
	def shuffle[T :SQLReadForm](permutation :IndexedSeq[Int]) :SQLReadForm[T] =
		ReorderedReadForm.shuffle(SQLReadForm[T], permutation)

	/** An adapter to an `SQLReadForm[T]` which uses it to read from [[java.sql.ResultSet ResultSet]]s
	  * in which columns appear out of order. When reading, the adapted `form` is given as an argument
	  * a proxy `ResultSet` whose `n`-th column is mapped to the `position + permutation(n - 1)`-th column
	  * in the underlying result set passed to the form adapter, where `position` is the index of the first column
	  * given as an argument to the adapter form. In other words, the returned form's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] and [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]]
	  * will read columns `position + permutation(0), position + permutation(1),` etc.
	  * Note that shifts by one come from the fact that `ResultSet` column numbering
	  * starts with `1`.
	  * @param permutation A permutation vector of length equal to
	  *                    `form.`[[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]], where
	  *                    every value from range `[0, form.columnCount)` appears exactly once.
	  */
	def shuffle[T](form :SQLReadForm[T], permutation :IndexedSeq[Int]) :SQLReadForm[T] =
		if (permutation.length != form.columnCount)
			throw new IllegalArgumentException(
				"Permutation length " + permutation.length + " differs from number of columns " + form.columnCount +
					" read by form " + form + ": " + permutation + "."
			)
		else
			ReorderedReadForm(form, Rearrangement.permutation(permutation))

	/** An adapter to an implicit `SQLReadForm[T]` which uses it to read from [[java.sql.ResultSet ResultSet]]s
	  * in which columns appear out of order. The adapted `form` is given as an argument a proxy `ResultSet`
	  * where the columns are mapped with `permutation`, that is
	  * `n`-th column in the new `ResultSet` will correspond to the real column at index `position + permutation(n - 1)`
	  * in the result set passed as the argument to the adapted form (shifts by one because JDBC columns and parameters
	  * are numbered starting with one).
	  * @param permutation a permutation vector of length equal to
	  *                    `form.`[[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]], where
	  *                    every value from range `[0, form.columnCount)` appears exactly once.
	  */
	def inverse[T :SQLReadForm](permutation :IndexedSeq[Int]) :SQLReadForm[T] =
		inverse(SQLReadForm[T], permutation)

	/** An adapter to an `SQLReadForm[T]` which uses it to read from [[java.sql.ResultSet ResultSet]]s
	  * in which columns appear out of order. The adapted `form` is given as an argument a proxy `ResultSet`
	  * where the columns are mapped with `permutation`, that is
	  * `n`-th column in the new `ResultSet` will correspond to the real column at index `position + permutation(n - 1)`
	  * in the result set passed as the argument to the adapted form (shifts by one because JDBC columns and parameters
	  * are numbered starting with one).
	  * @param permutation a permutation vector of length equal to
	  *                    `form.`[[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]], where
	  *                    every value from range `[0, form.columnCount)` appears exactly once.
	  */
	def inverse[T](form :SQLReadForm[T], permutation :IndexedSeq[Int]) :SQLReadForm[T] =
		if (permutation.length != form.columnCount)
			throw new IllegalArgumentException(
				"Permutation length " + permutation.length + " differs from number of columns " + form.columnCount +
					" read by form " + form + ": " + permutation + "."
			)
		else
	        ReorderedReadForm(form, Rearrangement.inverse(permutation))
}




/** A form used to set parameters of a [[java.sql.PreparedStatement PreparedStatement]] where they appear out
  * of order regarding the existing `this.form`.
  */
private[oldsql] trait ReorderedWriteForm[-T] extends WriteFormSeparateLiterals[T] with ReorderedUnspecifiedForm {
	protected override def form :SQLWriteForm[T]

	override def set(statement :PreparedStatement, position :Int, value :T) :Unit = {
		form.set(RearrangedPreparedStatement(statement, position, indexing), 1, value)
		setNulls(statement, position)
	}

	override def setNull(statement :PreparedStatement, position :Int) :Unit = {
		form.setNull(RearrangedPreparedStatement(statement, position, indexing), 1)
		setNulls(statement, position)
	}

	private final def setNulls(statement :PreparedStatement, position :Int) :Unit = {
		val idx = indexing
		if (!idx.isSurjection) {
			var i = idx.underlyingColumnCount
			while (i > 0) {
				if (!idx.isCovered(i))
					statement.setNull(position + i - 1, java.sql.Types.NULL)
				i -= 1
			}
		}
	}


	override def columnLiterals(value :T) :Seq[String] = indexing.reorder(form.columnLiterals(value), "null")
	override lazy val nullColumnLiterals :Seq[String] = indexing.reorder(form.nullColumnLiterals, "null")
	override lazy val split :Seq[ColumnWriteForm[T]] = indexing.reorder(form.split, ColumnWriteForm.nulls[T])

	override def notNull = form.notNull match {
		case same if same eq form => this
		case other => ReorderedWriteForm(other, indexing)
	}

	override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :ReorderedWriteForm[_] =>
			indexing == other.indexing && (form comparable other.form) || super.comparable(other)
		case _ => super.comparable(other)
	}
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :ReorderedWriteForm[_] if other canEqual this => form == other.form && indexing == other.indexing
		case _ => false
	}
	override def hashCode :Int = form.hashCode * 31 + indexing.hashCode

	override lazy val toString :String = form.toString + ".reorder(" + indexing + ")"
}


object ReorderedWriteForm {
	def apply[T :SQLWriteForm](indexing :Rearrangement) :SQLWriteForm[T] =
		ReorderedWriteForm(SQLWriteForm[T], indexing)

	def apply[T](form :SQLWriteForm[T], indexing :Rearrangement) :SQLWriteForm[T] = {
		if (indexing.columnCount != form.columnCount)
			throw new IllegalArgumentException(
				"Cannot map the indices of parameters set by " + form + " with " + indexing +
				" because they differ in the number of columns " + form.columnCount + " vs. " + indexing.columnCount + "."
			)
		val shuffle = indexing
		val f = form
		new ReorderedWriteForm[T] {
			override val form = f
			override val indexing = shuffle
		}
	}

	def splice[T :SQLWriteForm](gaps :IndexedSeq[(Int, Int)]) :SQLWriteForm[T] =
		ReorderedWriteForm.splice(SQLWriteForm[T], gaps)

	def splice[T](form :SQLWriteForm[T], gaps :IndexedSeq[(Int, Int)]) :SQLWriteForm[T] =
		ReorderedWriteForm(form, Rearrangement.splice(gaps, form.columnCount))

	def shuffle[T :SQLWriteForm](permutation :IndexedSeq[Int]) :SQLWriteForm[T] =
		ReorderedWriteForm.shuffle(SQLWriteForm[T], permutation)

	def shuffle[T](form :SQLWriteForm[T], permutation :IndexedSeq[Int]) :SQLWriteForm[T] =
		if (permutation.length != form.columnCount)
			throw new IllegalArgumentException(
				"Permutation length " + permutation.length + " differs from the number of columns " + form.columnCount +
				" written by form " + form + ": " + permutation + "."
			)
		else
			ReorderedWriteForm(form, Rearrangement.permutation(permutation))
}




private[oldsql] class ReorderedForm[T](override val form :SQLForm[T], override val indexing :Rearrangement)
	extends ReorderedReadForm[T] with ReorderedWriteForm[T] with SQLForm[T]
{
	override def notNull :SQLForm[T] = form.notNull match {
		case same if this eq same => this
		case other => ReorderedForm(other, indexing)
	}
}


object ReorderedForm {
	def apply[T :SQLForm](indexing :Rearrangement) :SQLForm[T] =
		ReorderedForm(SQLForm[T], indexing)

	def apply[T](form :SQLForm[T], indexing :Rearrangement) :SQLForm[T] =
		new ReorderedForm(form, indexing)

	def splice[T :SQLForm](gaps :IndexedSeq[(Int, Int)]) :SQLForm[T] =
		ReorderedForm.splice(SQLForm[T], gaps)

	def splice[T](form :SQLForm[T], gaps :IndexedSeq[(Int, Int)]) :SQLForm[T] =
		ReorderedForm(form, Rearrangement.splice(gaps, form.columnCount))

	def shuffle[T :SQLForm](permutation :IndexedSeq[Int]) :SQLForm[T] =
		ReorderedForm.shuffle(SQLForm[T], permutation)

	def shuffle[T](form :SQLForm[T], permutation :IndexedSeq[Int]) :SQLForm[T] =
		if (permutation.length != form.columnCount)
			throw new IllegalArgumentException(
				"Permutation length " + permutation.length + " differs from the number of columns " + form.columnCount +
				" in form " + form + ": " + permutation + "."
			)
		else
			ReorderedForm(form, Rearrangement.permutation(permutation))
}



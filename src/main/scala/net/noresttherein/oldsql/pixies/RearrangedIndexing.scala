package net.noresttherein.oldsql.pixies

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{Opt, PassedArray}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.pixies.RearrangedIndexing.ComposedIndexing






/** Column index translation strategy between the indexing exposed by a [[java.sql.ResultSet ResultSet]]
  * or a [[java.sql.PreparedStatement PreparedStatement]] adapter and the real indices in the underlying JDBC object.
  * Allows changing the order of columns/parameters and 'hiding' certain indices.
  * Its primary function is to allow reading of an object from a `ResultSet`
  * with a [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] assuming a different column order,
  * in particular a non consecutive subset of the full row of the result set. This is important especially
  * when a query returns rows mapping to different subclasses in the same class hierarchy, where every class
  * has its own form, ignorant of the existence of columns mapped to properties of other classes
  * (for example, in a class-per-table mapping scheme).
  *
  * A [[net.noresttherein.oldsql.schema.forms.ReorderedReadForm ReorderedReadForm]] proxy will wrap the argument
  * result set in a [[net.noresttherein.oldsql.pixies.RearrangedResultSet RearrangedResultSet]] and pass the wrapper
  * to the adapted form instead of the original. The same applies,
  * to a [[net.noresttherein.oldsql.schema.forms.ReorderedWriteForm ReorderedWriteForm]] adapter wrapping
  * argument statements in a [[net.noresttherein.oldsql.pixies.RearrangedPreparedStatement RearrangedPreparedStatement]].
  *
  * The primary direction of the translation, defined by method
  * [[net.noresttherein.oldsql.pixies.RearrangedIndexing!.apply apply]], happens from the index used by the adapted form
  * to the corresponding index in the underlying JDBC object, as handled by the database.
  * An inverse translation is implemented by method
  * [[net.noresttherein.oldsql.pixies.RearrangedIndexing!.inverse inverse]].
  */
trait RearrangedIndexing {
	/** Translation from the indices used by an adapted form, as exposed a [[java.sql.ResultSet ResultSet]] adapter,
	  * to the indexing of an underlying `ResultSet`.
	  * Remember that the numbering of `ResultSet` columns and [[java.sql.PreparedStatement PreparedStatement]]
	  * parameters starts with `1`, as it does in this trait, so, `apply(0)` is undefined.
	  *
	  * @param index An index of a column or parameter in the rearranged indexing implementing by this trait.
	  *              Valid values are
	  *              `1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]]`]`.
	  * @return a number from the `[1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]]`]` range.
	  */
	def apply(index :Int) :Int

	/** Returns an index of an underlying column/JDBC parameter corresponding to the given index seen by
	  * an adapted form, unless the column does not appear in the range of this instance,
	  * in which case `Lack` is returned.
	  * @param index an index in the `[1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]]`]` range.
	  * @return `if (`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.isMapped isMapped]]`(index)) Got(`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.apply apply]]`(index)) else Lack`,
	  *         but possibly in a more efficient manner.
	  */
	def get(index :Int) :Opt[Int]

	/** Translation from column indexing of a lower level, adapted
	  * [[java.sql.ResultSet ResultSet]]/[[java.sql.PreparedStatement PreparedStatement]], to an exposed indexing of
	  * a [[net.noresttherein.oldsql.pixies.RearrangedResultSet RearrangedResultSet]],
	  * [[net.noresttherein.oldsql.pixies.RearrangedPreparedStatement RearrangedPreparedStatement]], or other class
	  * using this indexing.
	  * Remember that the numbering of `ResultSet` columns and [[java.sql.PreparedStatement PreparedStatement]]
	  * parameters starts with `1`, as it does in this trait, so, `apply(0)` is undefined, while `apply(columnCount)`
	  * is the new index of the last column/parameter. Undefined is also the behaviour in case the given index
	  * identifies a column excluded from the exposed indexing; due to performance concerns, it may not cause
	  * an immediate exception, but result in a deferred [[ClassCastException]] when a value is used,
	  * or an [[java.sql.SQLException SQLException]] when the statement is executed.
	  * @param index An index of a column in the adapted `ResultSet` or parameter in the adapted `PreparedStatement`.
	  *              Valid values fall in the range
	  *              `1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]]`]`.
	  * @return a number from the `[1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]]`]` range.
	  */
	def inverse(index :Int) :Int

	/** Returns an index in the column/parameter range of
	  * `1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]], as used by an adapted
	  * form, corresponding to the actual read column/parameter `index` in an underlying JDBC object actually
	  * passed to the form adapter.
	  * @param index an index in the `[1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]]`]` range.
	  * @return `if (`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.isCovered isCovered]]`(index)) Got(`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.inverse inverse]]`(index)) else Lack`,
	  *         but possibly in a more efficient manner.
	  */
	def find(index :Int) :Opt[Int]

	/** Checks if the column at the given index in the underlying `ResultSet`/`PreparedStatement` features
	  * in the outer indexing defined by this translation.
	  * @param index a 1-based index of a column or parameter in the underlying JDBC object.
	  * @return `true` if [[net.noresttherein.oldsql.pixies.RearrangedIndexing.inverse inverse]]`(index)` will return
	  *         a valid index in the `[1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]]`]`
	  *         range.
	  * @see [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isMapped]]
	  */
	def isCovered(index :Int) :Boolean

	/** Checks if the column at the specified index has a corresponding column in the underlying
	  * `ResultSet`/`PreparedStatement`.
	  * @param index a 1-based index of a column or parameter in an JDBC object adapted with this translation.
	  * @return `true` if [[net.noresttherein.oldsql.pixies.RearrangedIndexing.apply apply]]`(index)` will return
	  *         a valid index in the `[1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumCount]]
	  * @see [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isCovered]]
	  */
	def isMapped(index :Int) :Boolean

	/** The number of columns exposed by a `ResultSet` or `PreparedStatement` reordered with this instance.
	  * This translation as a function is defined for indices `[1..columnCount]`.
	  * @see [[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount]]
	  */
	def columnCount :Int

	/** The range of indices of `ResultSet` columns or `PreparedStatement`'s parameters to which this translation
	  * maps the argument indices:
	  * {{{
	  *     (1 until columnCount).forall(i => !isMapped(i) || this(i) >= 1 && this(i) <= underlyingColumnCount)
	  * }}}
	  */
	def underlyingColumnCount :Int

	/** Given a sequence of length [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]],
	  * reorder it according to this mapping, returning a sequence
	  * of length [[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]],
	  * such that `n`-th element of the input sequence occurs at position
	  * [[net.noresttherein.oldsql.pixies.RearrangedIndexing.apply apply]]`(n)` in the result
	  * if [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isMapped isMapped]]`(n)`. Cells in the returned sequence
	  * with indices `n` such that `!`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.isCovered isCovered]]`(n)`
	  * are set to `missing` argument.
	  */
	def reorder[T](virtual :Seq[T], missing :T) :IndexedSeq[T] = {
		val xs = virtual match {
			case seq if seq.sizeIs <= 3 => seq
			case seq :IndexedSeq[T] => seq
			case _ => virtual to PassedArray
		}
		val underlying = new Array[AnyRef](underlyingColumnCount).asInstanceOf[Array[T]]
		var i = underlying.length
		while (i > 0) {
			if (isCovered(i))
				underlying(i - 1) = xs(inverse(i) - 1)
			else
				underlying(i - 1) = missing
			i -= 1
		}
		ArraySeq.unsafeWrapArray(underlying)
	}

	/** Given a sequence of length
	  *  [[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]],
	  * reorder it according to this mapping, returning a sequence
	  * of length [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]],
	  * such that `n`-th element of the input sequence occurs at position
	  * [[net.noresttherein.oldsql.pixies.RearrangedIndexing.inverse(index:Int) inverse]]`(n)` in the result
	  * if [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isCovered isCovered]]`(n)`.
	  * Cells in the returned sequence with indices `n` such that
	  * `!`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.isMapped isMapped]]`(n)` are set to `missing` argument.
	  */
	def inverse[T](underlying :Seq[T], missing :T) :IndexedSeq[T] = {
		val xs = underlying match {
			case seq if seq.sizeIs <= 3 => seq
			case seq :IndexedSeq[T] => seq
			case _ => underlying to PassedArray
		}
		val virtual = new Array[AnyRef](columnCount).asInstanceOf[Array[T]]
		var i = virtual.length
		while (i > 0) {
			if (isMapped(i))
				virtual(i - 1) = xs(apply(i) - 1)
			else
				virtual(i - 1) = missing
			i -= 1
		}
		ArraySeq.unsafeWrapArray(virtual)
	}


	/** If true, then [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]]` == `[[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]]
	  * and `(1 to columnCount).map(apply).sorted == (1 to columnCount)`.
	  */
	def isPermutation :Boolean =
		columnCount == underlyingColumnCount && {
			val present = new Array[Boolean](columnCount)
			var i = present.length
			while (i > 0 && isCovered(i)) {
				present(i) = true
				i -= 1
			}
			i == 0 && {
				i = present.length
				while (i > 0 && present(i))
					i -= 1
				i == 0
			}
		}

	/** A translation is an injection if for every column required by an adapted
	  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] there is an actual column in an underlying
	  * `ResultSet` (or every parameter set by an adapted [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]
	  * actually sets a real parameter on an underlying `PreparedStatement`. Reordering an `SQLReadForm`
	  * with an injection is safe, as all required values are provided. Reordering an `SQLWriteForm`
	  * with an injection may not be safe or correct, as some parameters of a used `PreparedStatement`
	  * will not be set by the form.
	  * Note that this is a departure
	  * from the standard definition of injection, as it does not require that every public index is mapped
	  * to a ''different'' underlying index. This is for efficiency reasons and because the condition is most likely
	  * to hold for all practical implementations.
	  * @return `(1 to columnCount).forall(isMapped)`.
	  * @see [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isSurjection isSurjection]]
	  */
	def isInjection :Boolean =
		columnCount <= underlyingColumnCount && {
			var i = columnCount
			while (i > 0 && isMapped(i))
				i -= 1
			i == 0
		}

	/** A translation is a surjection if every underlying index is mapped to some public index.
	  * The implication is that an [[net.noresttherein.oldsql.schema.SQLWriteForm write form]]
	  * will set all parameters of an underlying `PreparedStatement` in the range of
	  * `[1..`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount]]`]`, making this translation
	  * a safe reordering for write forms. Note that this is a departure from the standard definition of surjection,
	  * as it does not require that every underlying index corresponds to a single index as seen by an adapted form.
	  * This is for efficiency reasons, because the condition is most likely to hold for all practical implementations.
	  * @return `(1 to underlyingColumnCount).forall(isCovered)`.
	  * @see [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isInjection]]
	  */
	def isSurjection :Boolean =
		underlyingColumnCount >= columnCount && {
			var i = columnCount
			while (i > 0 && isMapped(i))
				i -= 1
			i == 0
		}

	/** Indexing being the composition of this indexing and the argument.
	  * It is defined as
	  * {{{
	  *     def apply(index :Int) = this(inner(index))
	  *     def inverse(index :Int) = inner.inverse(this.inverse(index))
	  * }}}
	  * @param inner a `RearrangedIndexing` with the same
	  *              [[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]]
	  *              as this instance and the same [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]]
	  *              as the argument.
	  */ //todo: tabulate it instead
	def compose(inner :RearrangedIndexing) :RearrangedIndexing =
		new ComposedIndexing(this, inner)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :RearrangedIndexing if other canEqual this =>
			columnCount == other.columnCount && underlyingColumnCount == other.underlyingColumnCount &&
				(1 to columnCount).forall {
					i => if (isMapped(i)) other.isMapped(i) && apply(i) == other(i) else !other.isMapped(i)
				} &&
				(1 to underlyingColumnCount).forall {
					i => if (isCovered(i)) other.isCovered(i) && inverse(i) == other.inverse(i) else !other.isCovered(i)
				}
		case _ => false
	}
	def canEqual(that :Any) :Boolean = that.isInstanceOf[RearrangedIndexing]
	override def hashCode :Int = //use a hash code compatible with IndexedSeq.hashCode
		(1 to columnCount).map(i => if (isMapped(i)) apply(i) else 0).hashCode * 31 +
			(1 to underlyingColumnCount).map(i => if (isCovered(i)) apply(i) else 0).hashCode
}




object RearrangedIndexing {
	/** Column indexing implementing a permutation given as a vector.
	  * The permutation is applied when translating the 'exposed' index, as seen by a form, to an index
	  * in a non-matching `ResultSet` or `PreparedStatement`. Conversely, the inverse is used
	  * by `RearrangedIndexing.`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.inverse inverse]],
	  * and maps the real indices of an argument [[java.sql.ResultSet ResultSet]]
	  * or a [[java.sql.PreparedStatement PreparedStatement]] to indices under which the former are visible
	  * to an adapted form.
	  *
	  * [[net.noresttherein.oldsql.pixies.RearrangedResultSet RearrangedResultSet]] proxy allows to read data
	  * from a `ResultSet` in which columns appear in a different order,
	  * with a standard [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] and set parameters of
	  * a `PreparedStatement` with a [[net.noresttherein.oldsql.schema.SQLWriteForm write]] expecting
	  * a different column order.
	  * @param permutation a vector of length equal to the number of columns in the result set, defining both
	  *                    [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]] and
	  *                    [[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]]
	  *                    of the returned indexing. It must contain each value from range `[0, permutation.length)`
	  *                    exactly once, in any order. The value at `n`-th position plus one specifies the position
	  *                    of a column of a [[net.noresttherein.oldsql.pixies.RearrangedResultSet RearrangedResultSet]]
	  *                    in an underlying `ResultSet` (or the index of `n`-th parameter of a
	  *                    [[net.noresttherein.oldsql.pixies.RearrangedPreparedStatement RearrangedPreparedStatement]]
	  *                    in the adapted `PreparedStatement`. 'Plus one' stems from the fact that columns
	  *                    in a result set and parameters of a prepared statement are numbered starting with one,
	  *                    while the permutation is zero-based. The exact equation is
	  *                    {{{
	  *                        underlyingIndex = permutation(exposedIndex - 1) + 1
	  *                    }}}
	  * @return an indexing translation with [[net.noresttherein.oldsql.pixies.RearrangedIndexing.apply apply]]
	  *         equivalent to `permutation` seen as a function.
	  */
	def permutation(permutation :IndexedSeq[Int]) :RearrangedIndexing =
		new PermutationIndexing(permutation)

	/** Column indexing implementing a permutation given as its inverse.
	  * The argument permutation is used to calculate the index under which a column of a [[java.sql.ResultSet ResultSet]]
	  * or a parameter of [[java.sql.PreparedStatement PreparedStatement]] will be seen by a form reordered
	  * with the created translation.
	  * for [[net.noresttherein.oldsql.pixies.RearrangedIndexing.inverse inverse]] method of the return indexing map.
	  * It is equivalent to calling
	  * `RearrangedIndexing.`[[net.noresttherein.oldsql.pixies.RearrangedIndexing.permutation permutation]]
	  * for the inverse of `permutation` argument.
	  * @param permutation a vector of length equal to the number of columns in the result set, defining both
	  *                    [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]] and
	  *                    [[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]]
	  *                    of the returned indexing. It must contain each value from range `[0, permutation.length)`
	  *                    exactly once, in any order. The value at `n`-th position plus one specifies the underlying
	  *                    index of the `(n+1)`-th column in
	  *                    a [[net.noresttherein.oldsql.pixies.RearrangedResultSet RearrangedResultSet]] or a
	  *                    [[net.noresttherein.oldsql.pixies.RearrangedPreparedStatement RearrangedPreparedStatement]]:
	  *                    {{{
	  *                        exposedIndex = permutation(underlyingIndex - 1) + 1
	  *                    }}}
	  */
	def inverse(permutation :IndexedSeq[Int]) :RearrangedIndexing = {
		val underlying = new Array[Int](permutation.length)
		try {
			var i = underlying.length
			while (i > 0) {
				i -= 1
				underlying(permutation(i)) = i
			}
			new PermutationIndexing(ArraySeq.unsafeWrapArray(underlying))
		} catch {
			case _ :IndexOutOfBoundsException =>
				throw new IllegalArgumentException("Invalid permutation: " + permutation + ".")
		}
	}

	/** Translation mapping the `n`-th column (of a mapping or form) to position `n + skippedColumns` in a `ResultSet`
	  * or `PreparedStatement`.
	  * @param skippedColumns the index of the first column to read from a `ResultSet` or parameter to set
	  *                       in a `PreparedStatement` minus one.
	  * @param `columnCount`  the [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]]
	  *                       of the returned indexing (its
	  *                       [[net.noresttherein.oldsql.pixies.RearrangedIndexing.underlyingColumnCount underlyingColumnCount]]
	  *                       will equal `columnCount + skippedColumns`).
	  */
	def shift(skippedColumns :Int, columnCount :Int) :RearrangedIndexing =
		new ShiftIndexing(skippedColumns, columnCount)

	def surjection(exposedIndex :IndexedSeq[Int], columnCount :Int = -1) :RearrangedIndexing =
		new SurjectionIndexing(exposedIndex, columnCount)

	def injection(underlyingIndex :IndexedSeq[Int], underlyingColumnCount :Int = -1) :RearrangedIndexing =
		new InjectionIndexing(underlyingIndex, underlyingColumnCount)

	/** Translation of a number range (columns read by an [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]
	  * or parameters set by an [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]) to a range with gaps
	  * in an actually used [[java.sql.ResultSet ResultSet]]/[[java.sql.PreparedStatement PreparedStatement]].
	  * Created `RearrangedIndexing` implements a monotonic function: `indexing(i) <= indexing(i + 1)`,
	  * where the difference `indexing(i) - i` equals `gaps.takeWhile(_._1 <= i).map(_._2).sum`.
	  * @param gaps  Indices of removed columns given as sections specified by `(startIndex, length)` pairs,
	  *              where `startIndex` is an exposed index and `length >= 0`
	  *              is a number of ''underlying'' following columns to skip following the index
	  *              corresponding to `startIndex`. Must be sorted by the first element of the pair. Can be empty.
	  * @param range The number of columns in the exposed index range. Must be `range >= gaps.last._1 - 1`.
	  */
	def splice(gaps :IndexedSeq[(Int, Int)], range :Int) :RearrangedIndexing = {
		val underlyingIndex = new Array[Int](range)
		val gapCount = gaps.length
		var nextGap = 0
		var x = 1 //an index in the domain (public)
		var y = 1 //a corresponding index in the counterdomain (underlying)
		while (x <= range) {
			while (nextGap < gapCount && gaps(nextGap)._1 <= x) { //if length == 0 then the following gap may start with the same index
				val (start, length) = gaps(nextGap)
				if (start < x)
					throw new IllegalArgumentException("Gaps are not sorted: " + gaps + ".")
				if (y < 0)
					throw new IllegalArgumentException("Negative gap length among skipped gaps " + gaps + ".")
				if (start == x) {
					y += length
					nextGap += 1
				}
			}
			underlyingIndex(x - 1) = y - 1
			x += 1
			y += 1
		}
		if (nextGap < gapCount)
			throw new IllegalArgumentException("Gaps outside of range " + range + ": " + gaps + ".")
		new InjectionIndexing(ArraySeq.unsafeWrapArray(underlyingIndex), range)
	}


	def isPermutation(permutation :IndexedSeq[Int]) :Boolean =
		permutation.indices == permutation.sorted


	private class PermutationIndexing(val permutation :IndexedSeq[Int]) extends RearrangedIndexing {
		private[this] val domainIndex = new Array[Int](permutation.length + 1)
		private[this] val rangeIndex  = new Array[Int](permutation.length + 1)

		{
			var i = permutation.length
			i = permutation.length
			while (i > 0) {
				val index = permutation(i - 1) + 1
				if (index <= 0 || index >= domainIndex.length || domainIndex(i) > 0)
					throw new IllegalArgumentException("Invalid permutation: " + permutation + ".")
				rangeIndex(i) = index
				domainIndex(index) = i
				i -= 1
			}
		}

		override val columnCount = permutation.length
		override def underlyingColumnCount :Int = columnCount

		override def apply(index :Int) = rangeIndex(index)
		override def get(index :Int) = Got(rangeIndex(index))
		override def inverse(index :Int) = domainIndex(index)
		override def find(index :Int) = Got(domainIndex(index))
		override def isCovered(index :Int) :Boolean = true
		override def isMapped(index :Int) :Boolean = true
		override def isPermutation :Boolean = true
		override def isInjection   :Boolean = true
		override def isSurjection  :Boolean = true

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :PermutationIndexing => permutation == other.permutation
			case _ => super.equals(that)
		}
		override def toString = permutation.mkString("Permutation(", ",", ")")
	}


	private class ShiftIndexing(shift :Int, length :Int) extends RearrangedIndexing {
		override def columnCount = length
		override def underlyingColumnCount :Int = shift + length

		override def apply(index :Int) = shift + index
		override def get(index :Int) = Got(shift + index)
		override def inverse(index :Int) = index - shift
		override def find(index :Int) = if (index > shift) Got(index - shift) else Lack

		override def isCovered(index :Int) = index > shift
		override def isMapped(index :Int) = true
		override def isInjection = true
		override def isSurjection = shift == 0

		private def skipped :Int = shift
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :ShiftIndexing => skipped == other.skipped && columnCount == other.columnCount
			case _ => super.equals(that)
		}
		override def toString = "Shift(" + shift + "+" + length + ")"
	}


	private def inverse(mapping :IndexedSeq[Int], copyTo :Array[Int], inverseCount :Int) :Array[Int] = {
		var x = mapping.length
		var max = -1 //the highest 0-based index in the underlying range
		while (x > 0) {
			x -= 1
			val y = mapping(x)
			if (y < 0)
				throw new IllegalArgumentException("Negative index in reordering " + mapping + ".")
			if (y > max)
				max = y
		}
		val res =
			if (inverseCount > max + 1)
				new Array[Int](inverseCount + 1)
			else if (inverseCount < 0)
				new Array[Int](max + 2) //+1 because max exists and +1 to convert to a 1-based index
			else
				throw new IllegalArgumentException(
					"Index mapping's " + mapping + " range exceeds the specified column count: " + inverseCount + "."
				)
		x = mapping.length
		while (x > 0) {
			val y = mapping(x - 1) + 1
			copyTo(x) = y
			res(y) = x
			x -= 1
		}
		res
	}

	private class InjectionIndexing(rangeIndex :IndexedSeq[Int], underlyingCount :Int) extends RearrangedIndexing {
		private[this] val underlyingIndex = new Array[Int](rangeIndex.length + 1)
		private[this] val publicIndex = RearrangedIndexing.inverse(rangeIndex, underlyingIndex, underlyingCount)
		override def columnCount = underlyingIndex.length - 1
		override def underlyingColumnCount = publicIndex.length - 1

		override def apply(index :Int)     = underlyingIndex(index)
		override def get(index :Int)       = Got(underlyingIndex(index))
		override def inverse(index :Int)   = publicIndex(index)
		override def find(index :Int)      = { val i = publicIndex(index); if (i > 0) Got(i) else Lack }
		override def isCovered(index :Int) = publicIndex(index) > 0
		override def isMapped(index :Int)  = true
		override def isInjection           = true

		override def toString = underlyingIndex.mkString("Injection(", ",", ")")
	}


	private class SurjectionIndexing(domainIndex :IndexedSeq[Int], count :Int) extends RearrangedIndexing {
		private[this] val publicIndex = new Array[Int](domainIndex.length + 1)
		private[this] val underlyingIndex = RearrangedIndexing.inverse(domainIndex, publicIndex, count)
		override def columnCount = underlyingIndex.length - 1
		override def underlyingColumnCount = publicIndex.length - 1

		override def apply(index :Int)     = underlyingIndex(index)
		override def get(index :Int)       = { val i = underlyingIndex(index); if (i > 0) Got(i) else Lack }
		override def inverse(index :Int)   = publicIndex(index)
		override def find(index :Int)      = Got(publicIndex(index))
		override def isCovered(index :Int) = true
		override def isMapped(index :Int)  = underlyingIndex(index) > 0
		override def isSurjection          = true

		override def toString = publicIndex.mkString("Surjection(", ",", ")")
	}



	/** Translation of a number range (column indices in a `ResultSet` to one with certain sections removed.
	  * @param gaps                  Indices of hidden columns given as sections specified by `(startIndex, length)`
	  *                              pairs. Must be sorted and contain valid indices without overlaps. Can be empty.
	  * @param underlyingColumnCount The number of columns in the adapted index range
	  *                              (of an original [[java.sql.ResultSet ResultSet]]).
	  */
/*
	private class SlicedIndexing(private val gaps :IndexedSeq[(Int, Int)], override val underlyingColumnCount :Int)
		extends RearrangedIndexing
	{
		//todo: change the implementation to tabulation
		private[this] val gapCount = gaps.length
		private[this] val firstGap = if (gapCount == 0) Int.MaxValue else gaps(0)._1
		/** Gap ends, i.e. `gaps.map { gap => gap._1 + gap._2 }`. */
		private[this] val ends   :Array[Int] = new Array[Int](gapCount)
		/** Accumulated gap length up to and including the n-th gap. */
		private[this] val shifts :Array[Int] = new Array[Int](gapCount)
		/** Public indices where cuts occurred; `cuts(i) + shifts(i + 1) == gaps(i)._1` */
		private[this] val cuts   :Array[Int] = new Array[Int](gapCount)

		{
			var i = 0; var shift = 0; var end = 0
			while (i < gapCount) {
				val (start, length) = gaps(i)
				if (length < 0)
					throw new IllegalArgumentException("Negative gap length: " + gaps + ".")
				if (start < end)
					throw new IllegalArgumentException("Overlapping gaps: " + gaps + ".")
				end = start + length
				ends(i) = end
				cuts(i) = start - shift
				shift += length
				shifts(i) = shift
				i += 1
			}
		}
		override val columnCount = underlyingColumnCount - shifts(gapCount - 1)


		private def precedingGaps(index :Int) = {
			var s = -1; var e = gapCount - 1 //let ends(-1) = Int.MinValue, ends(gapCount) = Int.MaxValue
			while (s < e) { //ends(s) <= index < ends(e + 1) && s <= e
				val i = (e + s + 1) / 2
				if (ends(i) <= index) s = i
				else e = i - 1
			} //s == e && s == max { i | ends(i) <= index }
			s
		}

		/** Translation from the indexing of adapted `ResultSet` to the exposed indexing without gaps. */
		override def apply(index :Int) :Int = precedingGaps(index) match {
			case -1 => index
			case n => index - shifts(n)
		}

		override def get(index :Int) :Opt[Int] = precedingGaps(index) match {
			case -1 => Lack
			case n => Got(n)
		}

		/** Translation from the exposed indexing (without certain columns) to the indexing of the adapted `ResultSet`. */
		override def inverse(index :Int) :Int =
			if (index < firstGap) //covers cases gapCount == 0 and index < 1
				index
			else {
				var s = 0; var e = gapCount - 1 //let cuts(-1) = Int.MinValue, cuts(gapCount) = Int.MaxValue
				while (s < e) { //cuts(s) < index <= cuts(e + 1)
					val i = (e + s + 1) / 2
					if (cuts(i) < index) s = i
					else e = i - 1
				} //s == e && s == max { i | cuts(i) < index }
				index + shifts(s)
			}

		override def isCovered(index :Int) :Boolean = precedingGaps(index) match {
			case -1 => true
			case n => index + shifts(n) < gaps(n)._1
		}
		override def isMapped(index :Int) :Boolean = true
		override def isPermutation :Boolean = gapCount == 0
		override def isSurjection  :Boolean = true
		override def isInjection   :Boolean = gapCount == 0


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :SlicedIndexing => gaps == other.gaps && underlyingColumnCount == other.underlyingColumnCount
			case _ => super.equals(that)
		}

		override def toString :String = gaps.mkString("Gaps(", ", ", ")")
	}
*/


	private case class ComposedIndexing(outer :RearrangedIndexing, inner :RearrangedIndexing)
		extends RearrangedIndexing
	{
		if (outer.columnCount != inner.underlyingColumnCount)
			throw new IllegalArgumentException(
				"Cannot compose " + outer + " with " + inner + " due to differing numbers of columns: outer input " +
					outer.columnCount + " vs. inner output " + inner.underlyingColumnCount + "."
			)

		override val columnCount           = inner.columnCount
		override val underlyingColumnCount = outer.underlyingColumnCount
		override def apply(index :Int) = outer(inner(index))
		override def get(index :Int) = inner.get(index) match {
			case Got(i) => outer.get(i)
			case lack   => lack
		}
		override def inverse(index :Int) = inner.inverse(outer.inverse(index))
		override def find(index :Int) = outer.find(index) match {
			case Got(i) => inner.find(i)
			case lack   => lack
		}
		override def isMapped(index :Int) :Boolean = inner.isMapped(index) && outer.isMapped(inner(index))
		override def isCovered(index :Int) :Boolean = outer.isCovered(index) && inner.isCovered(outer.inverse(index))
		override lazy val isPermutation :Boolean = outer.isPermutation && inner.isPermutation
		override lazy val isInjection   :Boolean = super.isInjection
		override lazy val isSurjection  :Boolean = super.isSurjection

		override def toString :String = "(" + outer.toString + " * " + inner + ")"
	}
}

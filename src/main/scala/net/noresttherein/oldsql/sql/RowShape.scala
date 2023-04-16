package net.noresttherein.oldsql.sql

import java.sql.JDBCType
import java.sql.JDBCType.NULL

import scala.collection.immutable.AbstractSeq

import net.noresttherein.oldsql.collection.{Opt, PassedArray}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.UndefinedShapeException
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm
import net.noresttherein.oldsql.sql.RowShape.{Indefinite, JDBCTypeExtensions, Nulls}






/** A light wrapper over a sequence of SQL types serving as an SQL relation descriptor both
  * for [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm forms]] and
  * SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]].
  * It is used to determine compatibility of different instances of the above classes on the SQL level,
  * ignoring completely the Scala types mapped to those columns. As [[java.sql.JDBCType JDBCType]] constants
  * do not map uniquely to database table column types, and because databases will generally widen or auto convert
  * related (and sometimes not so much) types between each other, forms/expressions having different shapes
  * can still be perfectly comparable in formatted SQL, for example as sides of an equation, or as the same column
  * in a union of two SQL ''selects'', this class introduces are subtyping relation of sorts,
  * where a type with a higher precision is considered a 'supertype'
  * (in terms of [[net.noresttherein.oldsql.sql.RowShape.<:< <:<]]) of a related type with a lower precision.
  * This allows to extend the interoperability of mappings with non matching column types.
  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.comparable]]
  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.comparable]]
  * @see [[net.noresttherein.oldsql.sql.SQLExpression.SQLShape]]
  * @author Marcin Mościcki
  */
@SerialVersionUID(1L)
class RowShape private (private val types :Seq[JDBCType]) extends AnyVal with Serializable {

	def isIndefinite :Boolean = types.isInstanceOf[Indefinite]
	def isNulls      :Boolean = types.isInstanceOf[Nulls] || !types.isInstanceOf[Indefinite] && types.forall(_ == NULL)
	def size         :Int = types.length
	def columnCount  :Int = types.length
	def columnTypes  :Opt[Seq[JDBCType]] = if (types.isInstanceOf[Indefinite]) Lack else Got(types)

	@throws[UndefinedShapeException]("If the shape is Indefinite")
	def toSeq        :Seq[JDBCType] =
		if (types.isInstanceOf[Indefinite])
			throw new UndefinedShapeException("RowShape.Indefinite(" + size + ")")
		else
			types

	def orNulls :RowShape = if (types.isInstanceOf[Indefinite]) Nulls(types.length) else this

	def map(f :Seq[JDBCType] => Seq[JDBCType]) :RowShape =
		if (types.isInstanceOf[Indefinite]) this
		else new RowShape(f(types))

	def flatMap(f :Seq[JDBCType] => RowShape) :RowShape =
		if (types.isInstanceOf[Indefinite]) this
		else f(types)

	/** Adds a new column of the given type after all existing columns.
	  * [[net.noresttherein.oldsql.sql.RowShape.Indefinite Indefinite]] instances simply increase their column count.
	  */
	def +(other :JDBCType) :RowShape =
		if (types.isInstanceOf[Indefinite]) Indefinite(types.length + 1)
		else new RowShape(types :+ other)

	/** Adds a new column of the given type before the existing columns.
	  * [[net.noresttherein.oldsql.sql.RowShape.Indefinite Indefinite]] instances simply increase their column count.
	  */
	def +:(other :JDBCType) :RowShape =
		if (types.isInstanceOf[Indefinite]) Indefinite(types.length + 1)
		else new RowShape(other +: types)

	/** Concatenates the columns of the two shapes. */
	def +(other :RowShape) :RowShape =
		if (types.isInstanceOf[Indefinite] || other.types.isInstanceOf[Indefinite])
			Indefinite(columnCount + other.columnCount)
		else
			new RowShape(types :++ other.types)


	def ||(other :RowShape) :RowShape =
		if (types.length != other.types.length)
			this | other //throw an exception
		else if (types.isInstanceOf[Indefinite])
			this
		else if (other.types.isInstanceOf[Indefinite])
			other
		else
			try this | other catch {
				case _ :IllegalArgumentException => Indefinite(types.length)
			}

	@throws[IllegalArgumentException](
		"if the column numbers in the shapes differ or there is a pair of columns with incomparable types.")
	def |(other :RowShape) :RowShape =  {
		def validateLengths() =
			if (types.length != other.size)
				throw new IllegalArgumentException(
					"Cannot union shapes " + this + " and " + other + ": different lengths (" +
						columnCount + " vs. " + other.columnCount + ")."
				)
		if (types.isInstanceOf[Nulls]) {
			validateLengths()
			other
		} else if (other.types.isInstanceOf[Nulls]) {
			validateLengths()
			this
		} else if (isIndefinite || other.isIndefinite)
			throw new IllegalArgumentException("Cannot union an unspecified shape: " + this + " || " + other + ".")
		else if (types.isInstanceOf[IndexedSeq[_]] && other.types.isInstanceOf[IndexedSeq[_]]) {
			validateLengths()
			val res = PassedArray.newBuilder[JDBCType]
			var i = types.length
			res.sizeHint(i)
			while (i > 0) {
				i -= 1
				res += types(i) | other.types(i)
			}
			new RowShape(res.result())
		} else {
			val i = types.iterator; val j = other.types.iterator
			val res = PassedArray.newBuilder[JDBCType]
			while (i.hasNext && j.hasNext)
				res += i.next() | j.next()
			if (i.hasNext || j.hasNext)
				validateLengths()
			new RowShape(res.result())
		}
	}


	def &&(other :RowShape) :RowShape =
		if (types.length != other.size)
			this & other //throw an exception
		else if (types.isInstanceOf[Indefinite])
			this
		else if (other.types.isInstanceOf[Indefinite])
			other
		else
			try this & other catch {
				case _ :IllegalArgumentException => Indefinite(types.length)
			}

	@throws[IllegalArgumentException](
		"if the column numbers in the shapes differ or there is a pair of columns with incomparable types.")
	def &(other :RowShape) :RowShape =  {
		def validateLengths() =
			if (types.length != other.size)
				throw new IllegalArgumentException(
					"Cannot intersect shapes " + this + " and " + other + ": different lengths (" +
						columnCount + " vs. " + other.columnCount + ")."
				)
		if (types.isInstanceOf[Nulls]) {
			validateLengths()
			this
		} else if (other.types.isInstanceOf[Nulls]) {
			validateLengths()
			other
		} else if (isIndefinite || other.isIndefinite)
			throw new IllegalArgumentException("Cannot intersect an unspecified shape: " + this + " || " + other + ".")
		else if (types.isInstanceOf[IndexedSeq[_]] && other.types.isInstanceOf[IndexedSeq[_]]) {
			validateLengths()
			val res = PassedArray.newBuilder[JDBCType]
			var i = types.length
			res.sizeHint(i)
			while (i > 0) {
				i -= 1
				res += types(i) & other.types(i)
			}
			new RowShape(res.result())
		} else {
			val i = types.iterator; val j = other.types.iterator
			val res = PassedArray.newBuilder[JDBCType]
			while (i.hasNext && j.hasNext)
				res += i.next() & j.next()
			if (i.hasNext || j.hasNext)
				validateLengths()
			new RowShape(res.result())
		}
	}


	/** Tests if both shapes list the same number of columns and their types are related.
	  * Related here means that either one of the column types can be widened without a loss of precision to the other,
	  * or there is a third type to which both can be converted this way.
	  * This condition is strictly looser than
	  * `this `[[net.noresttherein.oldsql.sql.RowShape.<:< <:<]]` other || other <:< this` because the conversion
	  * direction between the two shapes can be different for different column pairs.
	  */ //todo: replace most 'default' usages with form.comparable
	def <:>(other :RowShape) :Boolean = types match {
		case _ :Nulls =>
			types.length == other.types.length
		case _ if other.types.isInstanceOf[Nulls] =>
			types.length == other.types.length
		case _ :Indefinite =>
			false
		case _ if other.types.isInstanceOf[Indefinite] =>
			false
		case _ => other.types match {
			case _ :IndexedSeq[JDBCType] if other.types.isInstanceOf[IndexedSeq[_]] =>
				(types.length == other.types.length) && {
					var i = types.length - 1
					while (i >= 0 && types(i) <:> other.types(i))
						i -= 1
					i >= 0
				}
			case _ =>
				val i = types.iterator; val j = other.types.iterator
				var ok = true
				while (i.hasNext && j.hasNext && { ok = i.next() <:> j.next(); ok })
					{}
				!i.hasNext && !j.hasNext && ok
		}
	}

	def =:=(other :RowShape) :Boolean = types match {
		case _ :Indefinite =>
			false
		case _ if other.types.isInstanceOf[Indefinite] =>
			false
		case nulls :Nulls => other.types match {
			case alsoNulls :Nulls =>
				nulls.length == alsoNulls.length
			case _ =>
				nulls.length == other.types.length && other.types.forall(NULL =:= _)
		}
		case _ => other.types match {
			case nulls :Nulls =>
				types.length == nulls.length && types.forall(_ =:= NULL)
			case _ :IndexedSeq[JDBCType] if types.isInstanceOf[IndexedSeq[_]] =>
				(types.length == other.types.length) && {
					var i = types.length - 1
					while (i >= 0 && types(i) =:= other.types(i))
						i -= 1
					i >= 0
				}
			case _ =>
				val i = types.iterator; val j = other.types.iterator
				var same = true
				while (i.hasNext && j.hasNext && { same = i.next() =:= j.next(); same })
					{}
				same && !i.hasNext && !j.hasNext
		}
	}

	/** Tests if the shapes list the same number of columns and all column types in this shape can be converted
	  * without a loss of precision to the corresponding column type in the argument.
	  */
	def <:<(other :RowShape) :Boolean = types match {
		case nulls :Nulls =>
			nulls.length == other.types.length
		case _ :Indefinite =>
			false
		case _ if other.types.isInstanceOf[Indefinite] =>
			false
		case _ => other.types match {
			case nulls :Nulls =>
				types.length == nulls.length && types.forall(_ <:< NULL)
			case _ :IndexedSeq[JDBCType] if types.isInstanceOf[IndexedSeq[_]] =>
				(types.length == other.types.length) && {
					var i = types.length - 1
					while (i >= 0 && types(i) <:< other.types(i))
						i -= 1
					i >= 0
				}
			case _ =>
				val i = types.iterator; val j = other.types.iterator
				var conforms :Boolean = true
				while (i.hasNext && j.hasNext && { conforms = i.next() <:< j.next(); conforms })
					{}
				conforms && !i.hasNext && !j.hasNext
		}
	}


	override def toString :String = types match {
		case none :Indefinite => "[<-" + none.length + "->]"
		case _ => types.mkString("[", "|", "]")
	}
}






object RowShape {
	private[this] val Zero = new RowShape(PassedArray.empty)

	def apply() :RowShape = Zero

	def apply(columnCount :Int) :RowShape = Indefinite(columnCount)

	def apply(types :Seq[JDBCType]) :RowShape = new RowShape(types)

	def apply(first :JDBCType, rest :JDBCType*) :RowShape = new RowShape(first +: rest)

	def apply(form :UnspecifiedForm) :RowShape =
		try { new RowShape(form.columnTypes) } catch {
			case _ :UnsupportedOperationException => Indefinite(form.columnCount)
		}

	def unapply(shape :RowShape) :Opt[Seq[JDBCType]] = shape.columnTypes

	object Indefinite {
		def apply(columnCount :Int) :RowShape =
			if (columnCount < 0)
				throw new IllegalArgumentException("Negative column count: " + columnCount + ".")
			else if (columnCount < IndefiniteCacheSize)
				new RowShape(cache(columnCount))
			else
				new RowShape(new Indefinite(columnCount))

		def unapply(shape :RowShape) :Opt[Int] = shape.types match {
			case none :Indefinite => Got(none.length)
			case _ => Lack
		}

		private final val IndefiniteCacheSize = 64
		private[this] val cache = Array.tabulate(IndefiniteCacheSize)(new Indefinite(_))
	}

	@SerialVersionUID(1L)
	private class Indefinite(override val length :Int) extends AbstractSeq[JDBCType] with IndexedSeq[JDBCType] {
		override def apply(i :Int) = throw new UnsupportedOperationException
		override def equals(that :Any) :Boolean = that match {
			case other :Indefinite => (this eq other) || length == other.length
			case _ => false
		}
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Indefinite]
		override def hashCode :Int = length.hashCode
		override def toString = "Indefinite(" + length + ")"
	}


	object Nulls {
		def apply(columnCount :Int) :RowShape =
			if (columnCount < 0)
				throw new IllegalArgumentException("Negative SQL column count: " + columnCount + ".")
			else if (columnCount < NullsCacheSize)
				new RowShape(cache(columnCount))
			else
				new RowShape(new Nulls(columnCount))

		def unapply(shape :RowShape) :Opt[Int] = shape.types match {
			case nulls :Nulls => Got(nulls.length)
			case _ :Indefinite => Lack
			case _ if shape.types.forall(_ == NULL) => Got(shape.types.length)
			case _ => Lack
		}

		private final val NullsCacheSize = 32
		private[this] val cache = Array.tabulate(NullsCacheSize)(new Nulls(_))
	}

	@SerialVersionUID(1L)
	private class Nulls(override val length :Int) extends AbstractSeq[JDBCType] with Serializable {
		override def apply(i :Int) = NULL
		override def iterator = Iterator.fill(length)(NULL)
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :Nulls => length == other.length
			case _ => super.equals(that)
		}
//		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Nulls]
		override lazy val hashCode :Int = super.hashCode
		override def className = "Nulls"
		override def toString = "Nulls(" + length + ")"
	}


	implicit class JDBCTypeExtensions(private val tpe :JDBCType) extends AnyVal {
		def =:=(other :JDBCType) :Boolean = tpe == other
		def <:<(other :JDBCType) :Boolean = tpe == NULL || tpe == other //todo:
		def <:>(other :JDBCType) :Boolean = tpe == other || tpe == NULL || other == NULL

		def |(other :JDBCType) :JDBCType =
			if (this <:< other) other
			else if (other <:< tpe) tpe
			else if (tpe == NULL) other
			else if (other == NULL) tpe
			else
				throw new IllegalArgumentException("Incomparable SQL types: " + tpe + " and " + other)

		def &(other :JDBCType) :JDBCType =
			if (this <:< other) tpe
			else if (other <:< tpe) other
			else if (tpe == NULL) other
			else if (other == NULL) tpe
			else
				throw new IllegalArgumentException("Incomparable SQL types: " + tpe + " and " + other)
	}
}

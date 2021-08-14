package net.noresttherein.oldsql.model

import java.{lang => j}

import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.model.Restraint.Compares
import net.noresttherein.oldsql.model.Restrictive.{ArithmeticRestrictive, NegatedRestrictive}
import net.noresttherein.oldsql.model.types.ArithmeticSupport.MappedArithmeticSupport
import net.noresttherein.oldsql.model.types.LiteralSupport.MappedLiteral
import net.noresttherein.oldsql.model.types.OrderingSupport.MappedOrdering






/**
  * @author Marcin Mościcki
  */
object types {

	final val ValueTypes = new Specializable.Group(Byte, Short, Int, Long, Char, Float, Double, Boolean)

	abstract class <%<[S, T] extends (S => T) with Serializable

	abstract class =%=[S, T] extends (S<%<T) {
		def apply(left :S) :T = right(left)
		def right(left :S) :T
		def left(right :T) :S
	}

	object <%< {
//		@inline implicit final def symmetrical[S, T](implicit equiv: S=%=T) :T=%=S = adapt(equiv.left, equiv.right)

		implicit final val (byteUnboxing, byteBoxing) = adapt((_:j.Byte).byteValue, j.Byte.valueOf :Byte => j.Byte)
		implicit final val (shortUnboxing, shortBoxing) = adapt((_ :j.Short).shortValue, j.Short.valueOf :Short => j.Short)
		implicit final val (intUnboxing, intBoxing) = adapt((_ :j.Integer).intValue, j.Integer.valueOf :Int => j.Integer)
		implicit final val (longUnboxing, longBoxing) = adapt((_ :j.Long).longValue, j.Long.valueOf :Long => j.Long)
		implicit final val (floatUnboxing, floatBoxing) = adapt((_ :j.Float).floatValue, j.Float.valueOf :Float => j.Float)
		implicit final val (doubleUnboxing, doubleBoxing) = adapt((_ :j.Double).doubleValue, j.Double.valueOf :Double => j.Double)
		implicit final val (charUnboxing, charBoxing) = adapt((_ :j.Character).charValue, j.Character.valueOf :Char => j.Character)
		implicit final val (booleanUnboxing, booleanBoxing) = adapt((_ :j.Boolean).booleanValue, j.Boolean.valueOf :Boolean => j.Boolean)

		implicit final def nullToOption[X >: Null] :X =%= Option[X] = nullBoxing.asInstanceOf[X =%= Option[X]]
		implicit final def optionToNull[X >: Null] :Option[X] =%= X = nullUnboxing.asInstanceOf[Option[X] =%= X]
		private[this] final val (nullUnboxing, nullBoxing) = adapt((_ :Option[Null]).orNull, Option.apply :Null => Option[Null])

		implicit final val byte2Short = adapt((_ :Byte).toShort)
		implicit final val byte2Int = adapt((_ :Byte).toInt)
		implicit final val byte2Long = adapt((_ :Byte).toLong)
		implicit final val byte2Float = adapt((_ :Byte).toFloat)
		implicit final val byte2Double = adapt((_ :Byte).toDouble)
		implicit final val short2Int = adapt((_ :Short).toInt)
		implicit final val short2Long = adapt((_ :Short).toLong)
		implicit final val short2Float = adapt((_ :Short).toFloat)
		implicit final val short2Double = adapt((_ :Short).toDouble)
		implicit final val int2Long = adapt((_ :Int).toLong)
		implicit final val int2Float = adapt((_ :Int).toFloat)
		implicit final val int2Double = adapt((_ :Int).toDouble)
		implicit final val long2Float = adapt((_ :Long).toFloat)
		implicit final val long2Double = adapt((_ :Long).toDouble)
		implicit final val float2Double = adapt((_ :Float).toDouble)

		@inline private def adapt[S, T](right :S => T, left :T => S) :(S =%= T, T =%= S) =
			(new adapt(right, left), new adapt(left, right))

		private class adapt[S, T](r :S=>T, l :T=>S) extends (S =%= T) {
			override def right(left :S) :T = r(left)
			override def left(right :T) :S = l(right)
		}

		@inline private def adapt[S, T](cast :S=>T) :S<%<T = { s :S => cast(s) }
	}






	/** Type class for types which have a predefined translation to SQL expressions for atomic values. */
	sealed trait LiteralSupport[V] extends Serializable with implicits {
		@inline def imap[O](f :O => V) :LiteralSupport[O] = new MappedLiteral[V, O](f)(this)
	}



	object LiteralSupport {
		def map[X :LiteralSupport, Y](down :Y => X) :LiteralSupport[Y] = new MappedLiteral(down)

		private class MappedLiteral[X, Y](down :Y => X)(implicit literal :LiteralSupport[X])
			extends LiteralSupport[Y]

		private class StandardLiteral[V](override val toString :String) extends LiteralSupport[V]

		private def standard[V](name :String) :LiteralSupport[V] = new StandardLiteral[V](name)

		implicit val ForByte = standard[Byte]("Byte")
		implicit val ForShort = standard[Short]("Short")
		implicit val ForInt = standard[Int]("Int")
		implicit val ForLong = standard[Long]("Long")
		implicit val ForFloat = standard[Float]("Float")
		implicit val ForDouble = standard[Double]("Double")
		implicit val ForChar = standard[Char]("Char")
		implicit val ForBoolean = standard[Boolean]("Boolean")
		implicit val ForString = standard[String]("String")

		implicit def ForOption[T](some :LiteralSupport[T]) :LiteralSupport[Option[T]] =
			standard("Option[" + some + "]")

	}



	/** Type class for types which magnitude can be compared in SQL, not only in scala.
	  * Implicit values are provided for all standard numeric types and `String`.
	  */ //todo: this is just a stub
	trait OrderingSupport[V] extends LiteralSupport[V] with Ordering[V] {
		override def imap[O](down :O => V) :OrderingSupport[O] = new MappedOrdering[V, O](down)(this)
	}

	object OrderingSupport {
		@inline def apply[T :OrderingSupport] :OrderingSupport[T] = implicitly[OrderingSupport[T]]

		def unapply[T](restraint :Restraint[T]) :Opt[OrderingSupport[_]] = restraint match {
			case cmp :Compares[T, _] => Got(cmp.ordering)
			case _ => Lack
		}

		def map[X :OrderingSupport, Y](down :Y => X) :OrderingSupport[Y] = new MappedOrdering(down)


		implicit def numericOrdering[T :Numeric] :OrderingSupport[T] = {
			(x :T, y :T) => implicitly[Numeric[T]].compare(x, y)
		}

		implicit object OfEmptyChain extends OrderingSupport[@~] {
			override def compare(x: @~, y: @~) :Int = 0
		}

		implicit def OfChain[I <: Chain, L](implicit init :OrderingSupport[I], last :OrderingSupport[L])
				:OrderingSupport[I ~ L] =
			new ChainOrdering(init, last)


		implicit case object OfString extends OrderingSupport[String] {
			override def compare(x :String, y :String) :Int = x compare y

			def noCase :OrderingSupport[String] = OfStringNoCase
		}

		case object OfStringNoCase extends OrderingSupport[String] {
			override def compare(x :String, y :String) :Int = x compareToIgnoreCase y
		}


		private case class ChainOrdering[I <: Chain, L](init :OrderingSupport[I], last :OrderingSupport[L])
			extends OrderingSupport[I ~ L]
		{
			override def compare(x :I ~ L, y :I ~ L) = init.compare(x.init, y.init) match {
				case 0 => last.compare(x.last, y.last)
				case res => res
			}

			override def toString :String = init.toString + "~" + last.toString
		}

		private class MappedOrdering[X, Y](down :Y => X)(implicit ordering :OrderingSupport[X])
			extends OrderingSupport[Y]
		{
			override def compare(x :Y, y :Y) :Int = ordering.compare(down(x), down(y))

			override def toString = s"$ordering.map($down)"
		}
	}



	//todo: temporary stub
	sealed trait ArithmeticSupport[T] extends LiteralSupport[T] with Serializable {
		def plus(x :T, y :T) :T
		def minus(x :T, y :T) :T
		def times(x :T, y :T) :T
		def div(x :T, y :T) :T
		def rem(x :T, y :T) :T

		def negate(x :T) :T

		def map[O](lift :T => O)(lower :O => T) :ArithmeticSupport[O] =
			new MappedArithmeticSupport[T, O](lift, lower)(this)
	}

	object ArithmeticSupport {
		def apply[N :ArithmeticSupport] :ArithmeticSupport[N] = implicitly[ArithmeticSupport[N]]

		def unapply[T, N](restrictive :Restrictive[T, N]) :Opt[ArithmeticSupport[N]] = restrictive match {
			case op :ArithmeticRestrictive[T, N] => Got(op.arithmetic)
			case neg :NegatedRestrictive[T, N] => Got(neg.arithmetic)
			case _ => Lack
		}



		class MappedArithmeticSupport[X, Y](lift :X => Y, lower :Y => X)(implicit backing :ArithmeticSupport[X])
			extends ArithmeticSupport[Y]
		{
			override def plus(x :Y, y :Y) :Y = lift(backing.plus(lower(x), lower(y)))
			override def minus(x :Y, y :Y) :Y = lift(backing.minus(lower(x), lower(y)))
			override def times(x :Y, y :Y) :Y = lift(backing.times(lower(x), lower(y)))
			override def div(x :Y, y :Y) :Y = lift(backing.div(lower(x), lower(y)))
			override def rem(x :Y, y :Y) :Y = lift(backing.div(lower(x), lower(y)))
			override def negate(x :Y) :Y = lift(backing.negate(lower(x)))
		}

		implicit def integralArithmetic[T :Integral] :ArithmeticSupport[T] = new ArithmeticSupport[T] {
			private[this] val numeric = implicitly[Integral[T]]
			override def div(x :T, y :T) = numeric.quot(x, y)
			override def rem(x :T, y :T) = numeric.rem(x, y)
			override def plus(x :T, y :T) = numeric.plus(x, y)
			override def minus(x :T, y :T) = numeric.minus(x, y)
			override def times(x :T, y :T) = numeric.times(x, y)
			override def negate(x :T) = numeric.negate(x)
		}

		implicit def fractionalArithmetic[T :Fractional] :ArithmeticSupport[T] = new ArithmeticSupport[T] {
			private[this] val numeric = implicitly[Fractional[T]]
			override def div(x :T, y :T) = numeric.div(x, y)
			override def rem(x :T, y :T) = //todo: there is so much wrong about it, but its only a stub
				numeric.minus(numeric.div(x, y), numeric.fromInt((numeric.toLong(x) / numeric.toLong(y)).toInt))
			override def plus(x :T, y :T) = numeric.plus(x, y)
			override def minus(x :T, y :T) = numeric.minus(x, y)
			override def times(x :T, y :T) = numeric.times(x, y)
			override def negate(x :T) = numeric.negate(x)
		}

	}

}

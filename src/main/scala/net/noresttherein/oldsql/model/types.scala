package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.model.Restraint.Comparison
import net.noresttherein.oldsql.model.Restrictive.{ArithmeticRestrictive, NegatedRestrictive}


/**
  * @author Marcin Mościcki
  */
object types {


	/** Type class for types which can be translated to SQL expressions. */
	trait LiteralSupport[V] extends Serializable



	object LiteralSupport {

	}



	/** Type class for types which magnitude can be compared in SQL, not only in scala.
	  * Implicit values are provided for all standard numeric types and `String`.
	  */ //todo: this is just a stub
	trait OrderingSupport[V] extends LiteralSupport[V] with Ordering[V]

	object OrderingSupport {
		@inline def apply[T :OrderingSupport] :OrderingSupport[T] = implicitly[OrderingSupport[T]]

		def unapply[T](restraint :Restraint[T]) :Option[OrderingSupport[_]] = restraint match {
			case cmp :Comparison[T, _] => Some(cmp.ordering)
			case _ => None
		}

		//todo: stub
		implicit def numericOrdering[T :Numeric] :OrderingSupport[T] = {
			(x :T, y :T) => implicitly[Numeric[T]].compare(x, y)
		}

		//todo: case sensitive/insensitive
		implicit object OfString extends OrderingSupport[String] {
			override def compare(x :String, y :String) :Int = x compare y
		}
	}



	//todo: temporary stub
	trait ArithmeticSupport[T] extends Serializable {
		def plus(x :T, y :T) :T
		def minus(x :T, y :T) :T
		def times(x :T, y :T) :T
		def div(x :T, y :T) :T
		def rem(x :T, y :T) :T

		def negate(x :T) :T
	}

	object ArithmeticSupport {
		def apply[N :ArithmeticSupport] :ArithmeticSupport[N] = implicitly[ArithmeticSupport[N]]

		def unapply[T, N](restrictive :Restrictive[T, N]) :Option[ArithmeticSupport[N]] = restrictive match {
			case op :ArithmeticRestrictive[T, N] => Some(op.arithmetic)
			case neg :NegatedRestrictive[T, N] => Some(neg.arithmetic)
			case _ => None
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

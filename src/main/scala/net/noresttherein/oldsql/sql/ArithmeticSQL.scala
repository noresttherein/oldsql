package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.sql.ArithmeticSQL.Divide.{CaseDivide, DivideMatcher}
import net.noresttherein.oldsql.sql.ArithmeticSQL.Minus.{CaseMinus, MinusMatcher}
import net.noresttherein.oldsql.sql.ArithmeticSQL.Plus.{CasePlus, PlusMatcher}
import net.noresttherein.oldsql.sql.ArithmeticSQL.Remainder.{CaseRemainder, RemainderMatcher}
import net.noresttherein.oldsql.sql.ArithmeticSQL.Times.{CaseTimes, TimesMatcher}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL



/**
  * @author Marcin Mościcki
  */
trait ArithmeticSQL[-F <: FromClause, V] extends CompositeColumnSQL[F, V]






object ArithmeticSQL {

	sealed trait SQLArithmetic[T] extends Serializable//extends SQLOrdering[T]

	object SQLArithmetic {
		private class Adapter[T] extends SQLArithmetic[T]

		private def adapt[T] :SQLArithmetic[T] = new Adapter

		implicit val OfInt = adapt[Int]
		implicit val OfLong = adapt[Long]
		implicit val OfShort = adapt[Short]
		implicit val OfByte = adapt[Byte]
		implicit val OfDouble = adapt[Double]
		implicit val OfFloat = adapt[Float]
		implicit val OfBigInt = adapt[BigInt]
		implicit val OfBigDecimal = adapt[BigDecimal]
	}






	class Plus[-F <: FromClause, V](val left :ColumnSQL[F, V], val right :ColumnSQL[F, V])
	                               (implicit val arithmetic :SQLArithmetic[V])
		extends ArithmeticSQL[F, V]
	{
		override def readForm :ColumnReadForm[V] = left.readForm

		protected override def parts :Seq[ColumnSQL[F, V]] = left::right::Nil


		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, V] =
			Plus(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnSQL.ColumnMatcher[F, Y]) :Y[V] = matcher.plus(this)


		override def toString = "(" + left + " + " + right + ")"
	}



	object Plus {
		def apply[F <: FromClause, V :SQLArithmetic](left :ColumnSQL[F, V], right :ColumnSQL[F, V]) :Plus[F, V] =
			new Plus(left, right)

		def unapply[F <: FromClause, V](e :SQLExpression[F, V]) :Option[(ColumnSQL[F, V], ColumnSQL[F, V], SQLArithmetic[V])] =
			e match {
				case sum :Plus[F @unchecked, V @unchecked] => Some((sum.left, sum.right, sum.arithmetic))
				case  _ => None
			}

		trait PlusMatcher[+F <: FromClause, +Y[X]] {
			def plus[V](e :Plus[F, V]) :Y[V]
		}

		type MatchPlus[+F <: FromClause, +Y[X]] = PlusMatcher[F, Y]

		type CasePlus[+F <: FromClause, +Y[X]] = PlusMatcher[F, Y]
	}






	class Minus[-F <: FromClause, V](val left :ColumnSQL[F, V], val right :ColumnSQL[F, V])
	                               (implicit val arithmetic :SQLArithmetic[V])
		extends ArithmeticSQL[F, V]
	{
		override def readForm :ColumnReadForm[V] = left.readForm

		protected override def parts :Seq[ColumnSQL[F, V]] = left::right::Nil


		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, V] =
			Minus(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnSQL.ColumnMatcher[F, Y]) :Y[V] = matcher.minus(this)


		override def toString = "(" + left + " - " + right + ")"
	}



	object Minus {
		def apply[F <: FromClause, V :SQLArithmetic](left :ColumnSQL[F, V], right :ColumnSQL[F, V]) :Minus[F, V] =
			new Minus(left, right)

		def unapply[F <: FromClause, V](e :SQLExpression[F, V]) :Option[(ColumnSQL[F, V], ColumnSQL[F, V], SQLArithmetic[V])] =
			e match {
				case sum :Minus[F @unchecked, V @unchecked] => Some((sum.left, sum.right, sum.arithmetic))
				case  _ => None
			}

		trait MinusMatcher[+F <: FromClause, +Y[X]] {
			def minus[V](e :Minus[F, V]) :Y[V]
		}

		type MatchMinus[+F <: FromClause, +Y[X]] = MinusMatcher[F, Y]

		type CaseMinus[+F <: FromClause, +Y[X]] = MinusMatcher[F, Y]
	}






	class Times[-F <: FromClause, V](val left :ColumnSQL[F, V], val right :ColumnSQL[F, V])
	                                (implicit val arithmetic :SQLArithmetic[V])
		extends ArithmeticSQL[F, V]
	{
		override def readForm :ColumnReadForm[V] = left.readForm

		protected override def parts :Seq[ColumnSQL[F, V]] = left::right::Nil


		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, V] =
			Times(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnSQL.ColumnMatcher[F, Y]) :Y[V] = matcher.times(this)


		override def toString = "(" + left + " * " + right + ")"
	}



	object Times {
		def apply[F <: FromClause, V :SQLArithmetic](left :ColumnSQL[F, V], right :ColumnSQL[F, V]) :Times[F, V] =
			new Times(left, right)

		def unapply[F <: FromClause, V](e :SQLExpression[F, V]) :Option[(ColumnSQL[F, V], ColumnSQL[F, V], SQLArithmetic[V])] =
			e match {
				case sum :Times[F @unchecked, V @unchecked] => Some((sum.left, sum.right, sum.arithmetic))
				case  _ => None
			}

		trait TimesMatcher[+F <: FromClause, +Y[X]] {
			def times[V](e :Times[F, V]) :Y[V]
		}

		type MatchTimes[+F <: FromClause, +Y[X]] = TimesMatcher[F, Y]

		type CaseTimes[+F <: FromClause, +Y[X]] = TimesMatcher[F, Y]
	}






	class Divide[-F <: FromClause, V](val left :ColumnSQL[F, V], val right :ColumnSQL[F, V])
	                                (implicit val arithmetic :SQLArithmetic[V])
		extends ArithmeticSQL[F, V]
	{
		override def readForm :ColumnReadForm[V] = left.readForm

		protected override def parts :Seq[ColumnSQL[F, V]] = left::right::Nil


		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, V] =
			Divide(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnSQL.ColumnMatcher[F, Y]) :Y[V] = matcher.divide(this)


		override def toString = "(" + left + " / " + right + ")"
	}



	object Divide {
		def apply[F <: FromClause, V :SQLArithmetic](left :ColumnSQL[F, V], right :ColumnSQL[F, V]) :Divide[F, V] =
			new Divide(left, right)

		def unapply[F <: FromClause, V](e :SQLExpression[F, V]) :Option[(ColumnSQL[F, V], ColumnSQL[F, V], SQLArithmetic[V])] =
			e match {
				case sum :Divide[F @unchecked, V @unchecked] => Some((sum.left, sum.right, sum.arithmetic))
				case  _ => None
			}

		trait DivideMatcher[+F <: FromClause, +Y[X]] {
			def divide[V](e :Divide[F, V]) :Y[V]
		}

		type MatchDivide[+F <: FromClause, +Y[X]] = DivideMatcher[F, Y]

		type CaseDivide[+F <: FromClause, +Y[X]] = DivideMatcher[F, Y]
	}






	class Remainder[-F <: FromClause, V](val left :ColumnSQL[F, V], val right :ColumnSQL[F, V])
	                                (implicit val arithmetic :SQLArithmetic[V])
		extends ArithmeticSQL[F, V]
	{
		override def readForm :ColumnReadForm[V] = left.readForm

		protected override def parts :Seq[ColumnSQL[F, V]] = left::right::Nil


		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, V] =
			Remainder(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnSQL.ColumnMatcher[F, Y]) :Y[V] = matcher.remainder(this)


		override def toString = "(" + left + " % " + right + ")"
	}



	object Remainder {
		def apply[F <: FromClause, V :SQLArithmetic](left :ColumnSQL[F, V], right :ColumnSQL[F, V]) :Remainder[F, V] =
			new Remainder(left, right)

		def unapply[F <: FromClause, V](e :SQLExpression[F, V]) :Option[(ColumnSQL[F, V], ColumnSQL[F, V], SQLArithmetic[V])] =
			e match {
				case sum :Remainder[F @unchecked, V @unchecked] => Some((sum.left, sum.right, sum.arithmetic))
				case  _ => None
			}

		trait RemainderMatcher[+F <: FromClause, +Y[X]] {
			def remainder[V](e :Remainder[F, V]) :Y[V]
		}

		type MatchRemainder[+F <: FromClause, +Y[X]] = RemainderMatcher[F, Y]

		type CaseRemainder[+F <: FromClause, +Y[X]] = RemainderMatcher[F, Y]
	}






	trait ArithmeticMatcher[+F <: FromClause, +Y[X]]
		extends PlusMatcher[F, Y] with MinusMatcher[F, Y] with TimesMatcher[F, Y] with DivideMatcher[F, Y]
		   with RemainderMatcher[F, Y]

	type MatchArithmetic[+F <: FromClause, +Y[X]] = ArithmeticMatcher[F, Y]

	trait CaseArithmetic[+F <: FromClause, +Y[X]] extends MatchArithmetic[F, Y]
		with CasePlus[F, Y] with CaseMinus[F, Y] with CaseTimes[F, Y] with CaseDivide[F, Y] with CaseRemainder[F, Y]
	{
		def arithmetic[V](e :ArithmeticSQL[F, V]) :Y[V]
		
		override def plus[V](e :Plus[F, V]) :Y[V] = arithmetic(e)

		override def minus[V](e :Minus[F, V]) :Y[V] = arithmetic(e)
		
		override def times[V](e :Times[F, V]) :Y[V] = arithmetic(e)
		
		override def divide[V](e :Divide[F, V]) :Y[V] = arithmetic(e)

		override def remainder[V](e :Remainder[F, V]) :Y[V] = arithmetic(e)
	}
	
	
	
}

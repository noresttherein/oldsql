package net.noresttherein.oldsql.sql



import net.noresttherein.oldsql.schema.{ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.sql.SQLFormula.{ColumnFormula, CompositeFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLMapper.SQLRewriter
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.sql.AutoConversionFormula.PromotionConversion.{CasePromotion, PromotionMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.SQLTypePromotion.Lift



/**
  * @author Marcin Mościcki
  */
trait AutoConversionFormula[-F <: FromClause, S, T] extends CompositeFormula[F, T] {
	def expr :SQLFormula[F, S]

	def convert(s :S) :T

	override def parts :Seq[SQLFormula[F, S]] = expr::Nil

	override def readForm :SQLReadForm[T] = expr.readForm.mapNull(convert) //todo: NullValue?

	override def freeValue :Option[T] = expr.freeValue.map(convert)

//	override def get(values :RowValues[F]) :Option[T] = expr.get(values).map(convert)



	override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[T] = matcher.conversion(this)



	def sameAs(that :Formula[_]) :Boolean = canEqual(that)

	override def isomorphic(expression: Formula[_]): Boolean = expression match {
		case auto :AutoConversionFormula[_, _, _] if sameAs(auto) => expr isomorphic auto.expr
		case _ => false
	}

	private[oldsql] override def equivalent(expression: Formula[_]): Boolean = expression match {
		case auto :AutoConversionFormula[_, _, _] if sameAs(auto) => expr equivalent auto.expr
		case _ => false
	}

	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case f :AutoConversionFormula[_, _, _] => (f eq this) || (f sameAs this) && expr == f.expr
		case _ => false
	}

	override def hashCode :Int = expr.hashCode



	protected def name :String = this.unqualifiedClassName

	override def toString = s"$name($expr)"


}






object AutoConversionFormula {

	//	def apply[F <: FromClause, X, Y](f :SQLFormula[F, X])(map :X=>Y) :AutoConversionFormula[F, X, Y] =
	//		new CustomConversionFormula(f, map)



	//todo: use PromotionConversion
	/*
		case class OrNull[-F <: FromClause, T](expr :SQLFormula[F, T]) extends AutoConversionFormula[F, T, Option[T]] {

			override def convert(s: T): Option[T] = Option(s)

			override def name = "Option"

			override def map[S <: FromClause](mapper: SQLRewriter[F, S]) = OrNull(mapper(expr))
		}
	*/
	def OrNull[F <: FromClause, T](expr :SQLFormula[F, T]) :SQLFormula[F, Option[T]] =
		PromotionConversion(expr, Lift.option[T])

	def OrNull[F <: FromClause, T](expr :ColumnFormula[F, T]) :ColumnFormula[F, Option[T]] =
		new ColumnPromotionConversion[F, T, Option[T]](expr)


	class PromotionConversion[-F <: FromClause, T, U] protected[AutoConversionFormula]
			(val expr :SQLFormula[F, T])(implicit val lift :Lift[T, U])
		extends AutoConversionFormula[F, T, U]
	{
		override def convert(s :T) :U = lift(s)

		override def map[S <: FromClause](mapper :SQLRewriter[F, S]) :SQLFormula[S, U] =
			new PromotionConversion(mapper(expr))


		override def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[U] = matcher.promotion(this)

		override def name :String = lift.toString

		override def sameAs(that :Formula[_]) :Boolean = that match {
			case promo :PromotionConversion[_, _, _] => lift == promo.lift
			case _ => false
		}

		override def hashCode :Int = expr.hashCode * lift.hashCode
	}



	object PromotionConversion {
		def apply[F <: FromClause, X, Y](expr :SQLFormula[F, X], lift :Lift[X, Y]) :PromotionConversion[F, X, Y] =
			new PromotionConversion(expr)(lift)

		def unapply[F <: FromClause, Y](expr :SQLFormula[F, Y]) :Option[(SQLFormula[F, X], Lift[X, Y])] forSome { type X } =
			expr match {
				case promo :PromotionConversion[_, _, _] =>
					Some(promo.expr.asInstanceOf[SQLFormula[F, Any]] -> promo.lift.asInstanceOf[Lift[Any, Y]])
				case _ => None
			}



		implicit def promote[F <: FromClause, X, Y](expr :SQLFormula[F, X])(implicit lift :Lift[X, Y])
				:PromotionConversion[F, X, Y] =
			new PromotionConversion(expr)



		trait PromotionMatcher[+F <: FromClause, +Y[X]] {
			def promotion[T, U](promotion :PromotionConversion[F, T, U]) :Y[U]
		}

		type MatchPromotion[+F <: FromClause, +Y[X]] = PromotionMatcher[F, Y]

		type CasePromotion[+F <: FromClause, +Y[X]] = PromotionMatcher[F, Y]


	}



	class ColumnPromotionConversion[-F <: FromClause, T, U] private[AutoConversionFormula]
			(override val expr :ColumnFormula[F, T])(implicit lift :Lift[T, U])
		extends PromotionConversion[F, T, U](expr) with ColumnFormula[F, U]
	{
		override def readForm :ColumnReadForm[U] = expr.readForm.mapNull(convert) //todo: NullValue?

	}

	/*
		case class CustomConversionFormula[-F <: FromClause, X, Y](expr :SQLFormula[F, X], map :X=>Y)
			extends AutoConversionFormula[F, X, Y]
		{
			override def convert(s: X): Y = map(s)

			override def map[S <: FromClause](mapper: SQLRewriter[F, S]): SQLFormula[S, Y] =
				new CustomConversionFormula(mapper(expr), map)

			override def name = "Custom"
		}
	*/






	trait ConversionMatcher[+F <: FromClause, +Y[X]]
		extends PromotionMatcher[F, Y] //with SingleRowMatcher[F, Y] with MultipleRowsMatcher[F, Y]
	{
		def conversion[Z, X](f :AutoConversionFormula[F, Z, X]) :Y[X]
	}

	trait MatchConversion[+F <: FromClause, +Y[X]]
		extends ConversionMatcher[F, Y] with CasePromotion[F, Y] //with CaseRow[F, Y] with CaseRows[F, Y]

	trait CaseConversion[+F <: FromClause, +Y[X]] extends MatchConversion[F, Y] {
		def conversion[Z, X](f :AutoConversionFormula[F, Z, X]) :Y[X]

		override def promotion[T, U](promotion :PromotionConversion[F, T, U]) :Y[U] = conversion(promotion)

//		override def row[X](f :SelectAsRow[F, X]) :Y[X] = conversion(f)
//
//		override def rows[X](f :SelectAsRows[F, X]) :Y[Seq[X]] = conversion(f)

	}


}

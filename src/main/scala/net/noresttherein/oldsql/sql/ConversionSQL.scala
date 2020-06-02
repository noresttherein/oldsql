package net.noresttherein.oldsql.sql



import net.noresttherein.oldsql.schema.{ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher}
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ConversionSQL.ColumnPromotionConversion.{CaseColumnPromotion, ColumnPromotionMatcher}
import net.noresttherein.oldsql.sql.ConversionSQL.PromotionConversion.{CasePromotion, PromotionMatcher}
import net.noresttherein.oldsql.sql.FromClause.OuterFrom
import net.noresttherein.oldsql.sql.SelectSQL.{FreeSelectSQL, SubselectSQL}
import net.noresttherein.oldsql.sql.SQLExpression.SQLTypePromotion.Lift



/**
  * @author Marcin Mościcki
  */
trait ConversionSQL[-F <: FromClause, S, T] extends CompositeSQL[F, T] {
	def expr :SQLExpression[F, S]

	def convert(s :S) :T

	override def parts :Seq[SQLExpression[F, S]] = expr::Nil

	override def readForm :SQLReadForm[T] = expr.readForm.mapNull(convert) //consider: NullValue?

	override def freeValue :Option[T] = expr.freeValue.map(convert)


	override def selectFrom[G <: F with OuterFrom, O](from :G) :FreeSelectSQL[T, O] =
		SelectSQL(from, this)

	override def subselectFrom[G <: F, O](from :G) :SubselectSQL[from.Implicit, T, O] =
		SelectSQL.subselect[from.Implicit, from.type, S, T, O](from, this)


	override def applyTo[Y[_]](matcher: ExpressionMatcher[F, Y]): Y[T] = matcher.conversion(this)



	protected def name :String = this.unqualifiedClassName

	override def toString = s"$name($expr)"


}






object ConversionSQL {

	def OrNull[F <: FromClause, T](expr :SQLExpression[F, T]) :SQLExpression[F, Option[T]] =
		PromotionConversion(expr, Lift.option[T])

	def OrNull[F <: FromClause, T](expr :ColumnSQL[F, T]) :ColumnSQL[F, Option[T]] =
		new ColumnPromotionConversion[F, T, Option[T]](expr)



	trait ColumnConversionSQL[-F <: FromClause, T, U] extends ConversionSQL[F, T, U] with CompositeColumnSQL[F, U] {
		override def expr :ColumnSQL[F, T]

		override def parts :Seq[ColumnSQL[F, T]] = expr::Nil

		override def readForm :ColumnReadForm[U] = expr.readForm.mapNull(convert)

		override def applyTo[Y[_]](matcher :ColumnMatcher[F, Y]) :Y[U] = matcher.conversion(this)
	}



	private[sql] class MappedSQL[-F <: FromClause, T, U] protected[sql](val expr :SQLExpression[F, T])(f :T => U)
		extends ConversionSQL[F, T, U]
	{
		protected def conversion :T => U = f

		override def convert(s :T) :U = f(s)

		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :SQLExpression[S, U] =
			new MappedSQL(mapper(expr))(f)

		override def sameAs(other :CompositeSQL[Nothing, _]) :Boolean = other match {
			case conversion :MappedSQL[_, _, _] if conversion canEqual this =>
				this.conversion == conversion.conversion
			case _ => false
		}

		override def name = "~"
	}



	private[sql] class MappedColumnSQL[-F <: FromClause, T, U] protected[sql]
	(override val expr :ColumnSQL[F, T])(fun :T => U)
		extends MappedSQL[F, T, U](expr)(fun) with ColumnConversionSQL[F, T, U]
	{
		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnConversionSQL[S, T, U] =
			new MappedColumnSQL(mapper(expr))(fun)
	}





	class PromotionConversion[-F <: FromClause, T, U] protected[ConversionSQL]
	                         (val expr :SQLExpression[F, T])(implicit val lift :Lift[T, U])
		extends ConversionSQL[F, T, U]
	{
		override def convert(s :T) :U = lift(s)

		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :SQLExpression[S, U] =
			new PromotionConversion(mapper(expr))


		override def applyTo[Y[_]](matcher :ExpressionMatcher[F, Y]) :Y[U] = matcher.promotion(this)



		override def name :String = lift.toString

		override def sameAs(that :CompositeSQL[Nothing, _]) :Boolean = that match {
			case promo :PromotionConversion[_, _, _] => lift == promo.lift
			case _ => false
		}

		override def hashCode :Int = expr.hashCode * lift.hashCode
	}



	object PromotionConversion {

		def apply[F <: FromClause, X, Y](expr :SQLExpression[F, X], lift :Lift[X, Y]) :PromotionConversion[F, X, Y] =
			new PromotionConversion(expr)(lift)

		def unapply[F <: FromClause, Y](expr :SQLExpression[F, Y]) :Option[(SQLExpression[F, X], Lift[X, Y])] forSome { type X } =
			expr match {
				case promo :PromotionConversion[_, _, _] =>
					Some(promo.expr.asInstanceOf[SQLExpression[F, Any]] -> promo.lift.asInstanceOf[Lift[Any, Y]])
				case _ => None
			}



		trait PromotionMatcher[+F <: FromClause, +Y[X]] extends ColumnPromotionMatcher[F, Y] {
			def promotion[T, U](e :PromotionConversion[F, T, U]) :Y[U]
		}

		type MatchPromotion[+F <: FromClause, +Y[X]] = PromotionMatcher[F, Y]

		trait CasePromotion[+F <: FromClause, +Y[X]] extends PromotionMatcher[F, Y] {
			override def promotion[T, U](e :ColumnPromotionConversion[F, T, U]) :Y[U] =
				promotion(e :PromotionConversion[F, T, U])
		}


	}



	class ColumnPromotionConversion[-F <: FromClause, T, U] private[ConversionSQL]
	(override val expr :ColumnSQL[F, T])(implicit lift :Lift[T, U])
		extends PromotionConversion[F, T, U](expr) with ColumnConversionSQL[F, T, U]
	{
		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, U] =
			new ColumnPromotionConversion(mapper(expr))

		override def applyTo[Y[_]](matcher :ColumnMatcher[F, Y]) :Y[U] = matcher.promotion(this)
	}



	object ColumnPromotionConversion {

		def apply[F <: FromClause, X, Y](expr :ColumnSQL[F, X], lift :Lift[X, Y]) :ColumnPromotionConversion[F, X, Y] =
			new ColumnPromotionConversion(expr)(lift)

		def unapply[F <: FromClause, Y](expr :SQLExpression[F, Y]) :Option[(ColumnSQL[F, X], Lift[X, Y])] forSome { type X } =
			expr match {
				case promo :ColumnPromotionConversion[_, _, _] =>
					Some(promo.expr.asInstanceOf[ColumnSQL[F, Any]] -> promo.lift.asInstanceOf[Lift[Any, Y]])
				case _ => None
			}


		trait ColumnPromotionMatcher[+F <: FromClause, +Y[X]] {
			def promotion[T, U](e :ColumnPromotionConversion[F, T, U]) :Y[U]
		}

		type MatchColumnPromotion[+F <: FromClause, +Y[X]] = ColumnPromotionMatcher[F, Y]

		type CaseColumnPromotion[+F <: FromClause, +Y[X]] = ColumnPromotionMatcher[F, Y]
	}






	trait ColumnConversionMatcher[+F <: FromClause, +Y[X]] extends ColumnPromotionMatcher[F, Y] {
		def conversion[Z, X](e :ColumnConversionSQL[F, Z, X]) :Y[X]
	}

	type MatchColumnConversion[+F <: FromClause, +Y[X]] = ColumnConversionMatcher[F, Y]

	trait CaseColumnConversion[+F <: FromClause, +Y[X]]
		extends ColumnConversionMatcher[F, Y] with CaseColumnPromotion[F, Y]
	{
		override def promotion[T, U](e :ColumnPromotionConversion[F, T, U]) :Y[U] = conversion(e)
	}



	trait ConversionMatcher[+F <: FromClause, +Y[X]] extends PromotionMatcher[F, Y] {
		def conversion[Z, X](e :ConversionSQL[F, Z, X]) :Y[X]
	}

	trait MatchConversion[+F <: FromClause, +Y[X]]
		extends ConversionMatcher[F, Y] with CasePromotion[F, Y]

	trait CaseConversion[+F <: FromClause, +Y[X]] extends MatchConversion[F, Y] {
		override def promotion[T, U](e :PromotionConversion[F, T, U]) :Y[U] = conversion(e)
	}


}

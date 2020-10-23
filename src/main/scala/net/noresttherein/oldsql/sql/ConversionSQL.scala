package net.noresttherein.oldsql.sql



import net.noresttherein.oldsql.schema.{ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, Lift, LocalScope}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ConversionSQL.ColumnPromotionConversion.{CaseColumnPromotion, ColumnPromotionMatcher}
import net.noresttherein.oldsql.sql.ConversionSQL.PromotionConversion.{CasePromotion, PromotionMatcher}
import net.noresttherein.oldsql.sql.FromClause.FreeFrom
import net.noresttherein.oldsql.sql.SelectSQL.{FreeSelectSQL, SubselectSQL}

//here be implicits
import net.noresttherein.oldsql.slang._






/**
  * @author Marcin Mościcki
  */
trait ConversionSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, X, Y] extends CompositeSQL[F, S, Y] {
	def expr :SQLExpression[F, S, X]

	def convert(x :X) :Y

	override def parts :Seq[SQLExpression[F, S, X]] = expr::Nil

	override def readForm :SQLReadForm[Y] = expr.readForm.nullMap(convert) //consider: NullValue?

	override def freeValue :Option[Y] = expr.freeValue.map(convert)


	override def selectFrom[G <: F with FreeFrom](from :G) :FreeSelectSQL[Y, _] =
		SelectSQL(from, this)

	override def subselectFrom(from :F) :SubselectSQL[from.Base, Y, _] =
		SelectSQL.subselect[from.Base, from.type, X, Y, Any](from, this)


	override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher: ExpressionMatcher[F, R]): R[S, Y] =
		matcher.conversion(this)


	protected def name :String = this.unqualifiedClassName

	override def toString = s"$name($expr)"

}






object ConversionSQL {

	def OrNull[F <: FromClause, S >: LocalScope <: GlobalScope, T]
	          (expr :SQLExpression[F, S, T]) :SQLExpression[F, S, Option[T]] =
		PromotionConversion(expr, Lift.option[T])

	def OrNull[F <: FromClause, S >: LocalScope <: GlobalScope, T]
	          (expr :ColumnSQL[F, S, T]) :ColumnSQL[F, S, Option[T]] =
		new ColumnPromotionConversion[F, S, T, Option[T]](expr)



	trait ColumnConversionSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, X, Y]
		extends ConversionSQL[F, S, X, Y] with CompositeColumnSQL[F, S, Y]
	{
		override def expr :ColumnSQL[F, S, X]

		override def parts :Seq[ColumnSQL[F, S, X]] = expr::Nil

		override def readForm :ColumnReadForm[Y] = expr.readForm.nullMap(convert)

		override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, R]) :R[S, Y] =
			matcher.conversion(this)
	}



	private[sql] class MappedSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, X, Y] protected[sql]
	                            (override val expr :SQLExpression[F, S, X])(val conversion :X => Y)
		extends ConversionSQL[F, S, X, Y]
	{
		override def convert(s :X) :Y = conversion(s)

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :SQLExpression[E, S, Y] =
			new MappedSQL(mapper(expr))(conversion)

		override def sameAs(other :CompositeSQL[_, _, _]) :Boolean = other match {
			case conversion :MappedSQL[_, _, _, _] if conversion canEqual this =>
				this.conversion == conversion.conversion
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case conv :MappedSQL[_, _, _, _] if conv canEqual this =>
				conv.expr == expr && conv.conversion == conversion
		}

		override def hashCode :Int = expr.hashCode * 31 + conversion.hashCode

		override def name = "~"
	}



	private[sql] class MappedColumnSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, X, Y] protected[sql]
	                                  (override val expr :ColumnSQL[F, S, X])(fun :X => Y)
		extends MappedSQL[F, S, X, Y](expr)(fun) with ColumnConversionSQL[F, S, X, Y]
	{
		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnConversionSQL[E, S, X, Y] =
			new MappedColumnSQL(mapper(expr))(conversion)
	}





	class PromotionConversion[-F <: FromClause, -S >: LocalScope <: GlobalScope, X, Y] protected[ConversionSQL]
	                         (override val expr :SQLExpression[F, S, X])(implicit val lift :Lift[X, Y])
		extends ConversionSQL[F, S, X, Y]
	{
		override def convert(s :X) :Y = lift(s)

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :SQLExpression[E, S, Y] =
			new PromotionConversion(mapper(expr))


		override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, R]) :R[S, Y] =
			matcher.promotion(this)


		override def sameAs(that :CompositeSQL[_, _, _]) :Boolean = that match {
			case promo :PromotionConversion[_, _, _, _] => lift == promo.lift
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :PromotionConversion[_, _, _, _] if other canEqual this =>
				other.expr == expr && other.lift == lift
			case _ => false
		}

		override def hashCode :Int = expr.hashCode * lift.hashCode

		override def name :String = lift.toString
	}



	object PromotionConversion {

		def apply[F <: FromClause, S >: LocalScope <: GlobalScope, X, Y]
		         (expr :SQLExpression[F, S, X], lift :Lift[X, Y]) :PromotionConversion[F, S, X, Y] =
			new PromotionConversion(expr)(lift)

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope, Y](expr :SQLExpression[F, S, Y])
				:Option[(SQLExpression[F, S, X], Lift[X, Y])] forSome { type X } =
			expr match {
				case promo :PromotionConversion[_, _, _, _] =>
					Some(promo.expr.asInstanceOf[SQLExpression[F, S, Any]] -> promo.lift.asInstanceOf[Lift[Any, Y]])
				case _ => None
			}



		trait PromotionMatcher[+F <: FromClause, +R[-_ >: LocalScope <: GlobalScope, _]]
			extends ColumnPromotionMatcher[F, R]
		{
			def promotion[S >: LocalScope <: GlobalScope, X, Y](e :PromotionConversion[F, S, X, Y]) :R[S, Y]
		}

		type MatchPromotion[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = PromotionMatcher[F, Y]

		trait CasePromotion[+F <: FromClause, +R[-_ >: LocalScope <: GlobalScope, _]] extends PromotionMatcher[F, R] {
			override def promotion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnPromotionConversion[F, S, X, Y]) :R[S, Y] =
				promotion(e :PromotionConversion[F, S, X, Y])
		}


	}



	class ColumnPromotionConversion[-F <: FromClause, -S >: LocalScope <: GlobalScope, X, Y] private[ConversionSQL]
	                               (override val expr :ColumnSQL[F, S, X])(implicit lift :Lift[X, Y])
		extends PromotionConversion[F, S, X, Y](expr) with ColumnConversionSQL[F, S, X, Y]
	{
		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Y] =
			new ColumnPromotionConversion(mapper(expr))

		override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, R]) :R[S, Y] =
			matcher.promotion[S, X, Y](this)
	}



	object ColumnPromotionConversion {

		def apply[F <: FromClause, S >: LocalScope <: GlobalScope, X, Y]
		         (expr :ColumnSQL[F, S, X], lift :Lift[X, Y]) :ColumnPromotionConversion[F, S, X, Y] =
			new ColumnPromotionConversion(expr)(lift)

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope, Y](expr :SQLExpression[F, S, Y])
				:Option[(ColumnSQL[F, S, X], Lift[X, Y])] forSome { type X } =
			expr match {
				case promo :ColumnPromotionConversion[_, _, _, _] =>
					Some(promo.expr.asInstanceOf[ColumnSQL[F, S, Any]] -> promo.lift.asInstanceOf[Lift[Any, Y]])
				case _ => None
			}


		trait ColumnPromotionMatcher[+F <: FromClause, +R[-_ >: LocalScope <: GlobalScope, _]] {
			def promotion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnPromotionConversion[F, S, X, Y]) :R[S, Y]
		}

		type MatchColumnPromotion[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnPromotionMatcher[F, Y]

		type CaseColumnPromotion[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnPromotionMatcher[F, Y]
	}






	trait ColumnConversionMatcher[+F <: FromClause, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnPromotionMatcher[F, R]
	{
		def conversion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnConversionSQL[F, S, X, Y]) :R[S, Y]
	}

	type MatchColumnConversion[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnConversionMatcher[F, Y]

	trait CaseColumnConversion[+F <: FromClause, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnConversionMatcher[F, R] with CaseColumnPromotion[F, R]
	{
		override def promotion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnPromotionConversion[F, S, X, Y]) :R[S, Y] =
			conversion(e)
	}



	trait ConversionMatcher[+F <: FromClause, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnConversionMatcher[F, R] with PromotionMatcher[F, R]
	{
		def conversion[S >: LocalScope <: GlobalScope, X, Y](e :ConversionSQL[F, S, X, Y]) :R[S, Y]
	}

	trait MatchConversion[+F <: FromClause, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends ConversionMatcher[F, R] with CasePromotion[F, R]
	{
		override def conversion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnConversionSQL[F, S, X, Y]) :R[S, Y] =
			conversion(e :ConversionSQL[F, S, X, Y])
	}

	trait CaseConversion[+F <: FromClause, +R[-_ >: LocalScope <: GlobalScope, _]] extends MatchConversion[F, R] {

		override def promotion[S >: LocalScope <: GlobalScope, T, U](e :PromotionConversion[F, S, T, U]) :R[S, U] =
			conversion(e)
	}


}


package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, ParamSelect, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.UnaryColumnOperator
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, GroundFrom, NonEmptyFrom, TopFrom}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, Lift, LocalScope}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.UnaryOperatorSQL
import net.noresttherein.oldsql.sql.ast.ConversionSQL.ColumnPromotionConversion.{CaseColumnPromotion, ColumnPromotionMatcher}
import net.noresttherein.oldsql.sql.ast.ConversionSQL.PromotionConversion.{CasePromotion, PromotionMatcher}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}

//here be implicits
import net.noresttherein.oldsql.slang._






/**
  * @author Marcin Mościcki
  */
trait ConversionSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, Y] extends CompositeSQL[F, S, Y] {
	def value :SQLExpression[F, S, X]

	def convert(x :X) :Y

	override def parts :Seq[SQLExpression[F, S, X]] = value::Nil

	override def readForm :SQLReadForm[Y] = value.readForm.nullMap(convert) //consider: NullValue?


	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectSQL[Y] =
		SelectSQL(from, this)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectSQL[B, Y] =
		SelectSQL.subselect[B, from.type, X, Y](from, this)

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:ParamSelect[P, Y] =
		ParamSelect(from.self)[X, Y, ()](this)


	override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher: ExpressionMatcher[F, R]): R[S, Y] =
		matcher.conversion(this)


	protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		spelling(value :SQLExpression[E, S, X])(context, params)

	protected override def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		spelling.inline(value :SQLExpression[E, S, X])(context, params)


	protected def name :String = this.localClassName

	override def toString = s"$name($value)"

}






object ConversionSQL {

	def OrNull[F <: RowProduct, S >: LocalScope <: GlobalScope, T]
	          (expr :SQLExpression[F, S, T]) :SQLExpression[F, S, Option[T]] =
		PromotionConversion(expr, Lift.option[T])

	def OrNull[F <: RowProduct, S >: LocalScope <: GlobalScope, T]
	          (expr :ColumnSQL[F, S, T]) :ColumnSQL[F, S, Option[T]] =
		new ColumnPromotionConversion[F, S, T, Option[T]](expr)



	trait ColumnConversionSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, Y]
		extends ConversionSQL[F, S, X, Y] with CompositeColumnSQL[F, S, Y]
	{
		override def value :ColumnSQL[F, S, X]

		override def parts :Seq[ColumnSQL[F, S, X]] = value::Nil

		override def readForm :ColumnReadForm[Y] = value.readForm.nullMap(convert)

		override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, R]) :R[S, Y] =
			matcher.conversion(this)
	}



	private[sql] class MappedSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, Y] protected[sql]
	                            (override val value :SQLExpression[F, S, X])(val conversion :X => Y)
		extends UnaryOperatorSQL[F, S, X, Y] with ConversionSQL[F, S, X, Y]
	{
		override def convert(s :X) :Y = conversion(s)

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (e :SQLExpression[E, C, X]) :SQLExpression[E, C, Y] =
			new MappedSQL(e)(conversion)

		override def sameAs(other :CompositeSQL.*) :Boolean = other.isInstanceOf[MappedSQL[_, _, _, _]]

		override def canEqual(other :Any) :Boolean = other match {
			case conversion :MappedSQL[_, _, _, _] if conversion canEqual this =>
				this.conversion == conversion.conversion
			case _ => false
		}

		override def hashCode :Int = value.hashCode * 31 + conversion.hashCode

		override def name = "~"
	}



	private[sql] class MappedColumnSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, Y] protected[sql]
	                                  (override val value :ColumnSQL[F, S, X])(fun :X => Y)
		extends MappedSQL[F, S, X, Y](value)(fun) with UnaryColumnOperator[F, S, X, Y]
		   with ColumnConversionSQL[F, S, X, Y]
	{
		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope](e :ColumnSQL[E, C, X]) =
			new MappedColumnSQL(e)(fun)
	}





	class PromotionConversion[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, Y] protected[ConversionSQL]
	                         (override val value :SQLExpression[F, S, X])(implicit val lift :Lift[X, Y])
		extends UnaryOperatorSQL[F, S, X, Y] with ConversionSQL[F, S, X, Y]
	{
		override def convert(s :X) :Y = lift(s)

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (e :SQLExpression[E, C, X]) :SQLExpression[E, C, Y] =
			new PromotionConversion(e)


		override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, R]) :R[S, Y] =
			matcher.promotion(this)


		override def sameAs(that :CompositeSQL.*) :Boolean = that.isInstanceOf[PromotionConversion[_, _, _, _]]

		override def canEqual(that :Any) :Boolean = that match {
			case promo :PromotionConversion[_, _, _, _] => lift == promo.lift
			case _ => false
		}

		override def hashCode :Int = value.hashCode * lift.hashCode

		override def name :String = lift.toString
	}



	object PromotionConversion {

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X, Y]
		         (expr :SQLExpression[F, S, X], lift :Lift[X, Y]) :PromotionConversion[F, S, X, Y] =
			new PromotionConversion(expr)(lift)

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, Y](expr :SQLExpression[F, S, Y])
				:Opt[(SQLExpression[F, S, X], Lift[X, Y])] forSome { type X } =
			expr match {
				case promo :PromotionConversion[_, _, _, _] =>
					Got(promo.value.asInstanceOf[SQLExpression[F, S, Any]] -> promo.lift.asInstanceOf[Lift[Any, Y]])
				case _ => Lack
			}



		trait PromotionMatcher[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]]
			extends ColumnPromotionMatcher[F, R]
		{
			def promotion[S >: LocalScope <: GlobalScope, X, Y](e :PromotionConversion[F, S, X, Y]) :R[S, Y]
		}

		type MatchPromotion[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = PromotionMatcher[F, Y]

		trait CasePromotion[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] extends PromotionMatcher[F, R] {
			override def promotion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnPromotionConversion[F, S, X, Y]) :R[S, Y] =
				promotion(e :PromotionConversion[F, S, X, Y])
		}

	}



	class ColumnPromotionConversion[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, Y] private[ConversionSQL]
	                               (override val value :ColumnSQL[F, S, X])(implicit lift :Lift[X, Y])
		extends PromotionConversion[F, S, X, Y](value) with UnaryColumnOperator[F, S, X, Y]
		   with ColumnConversionSQL[F, S, X, Y]
	{
		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                              (e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] =
			new ColumnPromotionConversion(e)

		override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, R]) :R[S, Y] =
			matcher.promotion[S, X, Y](this)
	}



	object ColumnPromotionConversion {

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X, Y]
		         (expr :ColumnSQL[F, S, X], lift :Lift[X, Y]) :ColumnPromotionConversion[F, S, X, Y] =
			new ColumnPromotionConversion(expr)(lift)

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, Y](expr :SQLExpression[F, S, Y])
				:Opt[(ColumnSQL[F, S, X], Lift[X, Y])] forSome { type X } =
			expr match {
				case promo :ColumnPromotionConversion[_, _, _, _] =>
					Got(promo.value.asInstanceOf[ColumnSQL[F, S, Any]] -> promo.lift.asInstanceOf[Lift[Any, Y]])
				case _ => Lack
			}


		trait ColumnPromotionMatcher[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] {
			def promotion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnPromotionConversion[F, S, X, Y]) :R[S, Y]
		}

		type MatchColumnPromotion[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnPromotionMatcher[F, Y]

		type CaseColumnPromotion[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnPromotionMatcher[F, Y]
	}






	trait ColumnConversionMatcher[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnPromotionMatcher[F, R]
	{
		def conversion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnConversionSQL[F, S, X, Y]) :R[S, Y]
	}

	type MatchColumnConversion[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnConversionMatcher[F, Y]

	trait CaseColumnConversion[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnConversionMatcher[F, R] with CaseColumnPromotion[F, R]
	{
		override def promotion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnPromotionConversion[F, S, X, Y]) :R[S, Y] =
			conversion(e)
	}



	trait ConversionMatcher[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnConversionMatcher[F, R] with PromotionMatcher[F, R]
	{
		def conversion[S >: LocalScope <: GlobalScope, X, Y](e :ConversionSQL[F, S, X, Y]) :R[S, Y]
	}

	trait MatchConversion[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends ConversionMatcher[F, R] with CasePromotion[F, R]
	{
		override def conversion[S >: LocalScope <: GlobalScope, X, Y](e :ColumnConversionSQL[F, S, X, Y]) :R[S, Y] =
			conversion(e :ConversionSQL[F, S, X, Y])
	}

	trait CaseConversion[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] extends MatchConversion[F, R] {

		override def promotion[S >: LocalScope <: GlobalScope, T, U](e :PromotionConversion[F, S, T, U]) :R[S, U] =
			conversion(e)
	}


}


package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.SQLCondition.Comparison.{CaseComparison, ComparisonMatcher}
import net.noresttherein.oldsql.sql.SQLCondition.Equality.{CaseEquality, EqualityMatcher}
import net.noresttherein.oldsql.sql.SQLCondition.Exists.{CaseExists, ExistsMatcher}
import net.noresttherein.oldsql.sql.SQLCondition.In.{CaseIn, InMatcher}
import net.noresttherein.oldsql.sql.SQLCondition.Inequality.InequalityMatcher
import net.noresttherein.oldsql.sql.SQLCondition.OrderComparison.OrderComparisonMatcher
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula, CompositeColumnFormula, CompositeFormula}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.ColumnFormulaMatcher






trait SQLCondition[-F <: FromClause] extends CompositeColumnFormula[F, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]
}






object SQLCondition {
	//todo: uniform naming convention for classes.

	trait Comparison[-F <: FromClause, T] extends SQLCondition[F] with CompositeFormula[F, Boolean] {
		val left :SQLFormula[F, T]
		val right :SQLFormula[F, T]
		def symbol :String

		protected override def parts :Seq[SQLFormula[F, T]] = left::right::Nil

		override def toString :String = left.toString + " " + symbol + " " + right
	}



	object Comparison {

		final val LT = "<"
		final val LTE = "<="
		final val GT = ">"
		final val GTE = ">="
		final val EQ = "="
		final val NEQ = "<>"


		def apply[F <: FromClause, T :SQLOrdering]
		         (left :SQLFormula[F, T], symbol :String, right :SQLFormula[F, T]) :Comparison[F, T] =
			symbol match {
				case EQ => Equality(left, right)
				case NEQ => Inequality(left, right)
				case _ => OrderComparison(left, symbol, right)
			}


		def unapply[F <: FromClause](f :SQLFormula[F, _]) :Option[(SQLFormula[F, T], String, SQLFormula[F, T])] forSome { type T } =
			f match {
				case compare :Comparison[F @unchecked, t] =>
					Some((compare.left, compare.symbol, compare.right))
			}



		trait ComparisonMatcher[+F <: FromClause, +Y[X]]
			extends OrderComparisonMatcher[F, Y] with EqualityMatcher[F, Y] with InequalityMatcher[F, Y]

		type MatchComparison[+F <: FromClause, +Y[X]] = ComparisonMatcher[F, Y]

		trait CaseComparison[+F <: FromClause, +Y[X]] extends MatchComparison[F, Y] {
			def comparison[X](e :Comparison[F, X]) :Y[Boolean]

			override def order[X](e :OrderComparison[F, X]) :Y[Boolean] = comparison(e)

			override def equality[X](e :Equality[F, X]) :Y[Boolean] = comparison(e)

			override def inequality[X](e :Inequality[F, X]) :Y[Boolean] = comparison(e)
		}

	}






	case class OrderComparison[-F <: FromClause, T]
	                          (ordering :SQLOrdering[T], left :SQLFormula[F, T], symbol :String, right :SQLFormula[F, T])
		extends Comparison[F, T]
	{
		import Comparison._

		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue)
				yield ordering.compare(l, r) match {
					case 0 => symbol == EQ || symbol == LTE || symbol == GTE
					case n if n < 0 => symbol == LT || symbol == LTE || symbol == NEQ
					case _ => symbol == GT || symbol == GTE || symbol == NEQ
				}


		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :BooleanFormula[S] =
			new OrderComparison(ordering, mapper(left), symbol, mapper(right))


		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Boolean] =
			matcher.order(this)

	}



	object OrderComparison {
		def apply[F <: FromClause, T](left :SQLFormula[F, T], symbol :String, right :SQLFormula[F, T])
		                             (implicit ordering :SQLOrdering[T]) =
			new OrderComparison(ordering, left, symbol, right)

		trait OrderComparisonMatcher[+F <: FromClause, +Y[X]] {
			def order[X](e :OrderComparison[F, X]) :Y[Boolean]
		}

		type MatchOrderComparison[+F <: FromClause, +Y[X]] = OrderComparisonMatcher[F, Y]

		type CaseOrderComparison[+F <: FromClause, +Y[X]] = OrderComparisonMatcher[F, Y]

	}






	case class Equality[-F <: FromClause, T](left :SQLFormula[F, T], right :SQLFormula[F, T])
		extends Comparison[F, T]
	{
		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield l == r

//		override def get(values :RowValues[F]) :Option[Boolean] =
//			for (l <- left.get(values); r <- right.get(values)) yield l == r

		override def symbol :String = Comparison.EQ



		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = Equality(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Boolean] = matcher.equality(this)

	}



	object Equality {
		trait EqualityMatcher[+F <: FromClause, +Y[X]] {
			def equality[X](e :Equality[F, X]) :Y[Boolean]
		}

		type MatchEquality[+F <: FromClause, +Y[X]] = EqualityMatcher[F, Y]

		type CaseEquality[+F <: FromClause, +Y[X]] = EqualityMatcher[F, Y]
	}



	case class Inequality[-F <: FromClause, T](left :SQLFormula[F, T], right :SQLFormula[F, T])
		extends Comparison[F, T]
	{
		override def freeValue :Option[Boolean] =
			for (l <- left.freeValue; r <- right.freeValue) yield l != r

//		override def get(values :RowValues[F]) :Option[Boolean] =
//			for (l <- left.get(values); r <- right.get(values)) yield l == r

		override def symbol :String = Comparison.NEQ



		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = Inequality(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Boolean] = matcher.inequality(this)

	}



	object Inequality {
		trait InequalityMatcher[+F <: FromClause, +Y[X]] {
			def inequality[X](e :Inequality[F, X]) :Y[Boolean]
		}

		type MatchInequality[+F <: FromClause, +Y[X]] = InequalityMatcher[F, Y]

		type CaseInequality[+F <: FromClause, +Y[X]] = InequalityMatcher[F, Y]
	}






	case class Exists[-F <: FromClause, V, O](select :SelectFormula[F, V, O]) extends SQLCondition[F] {
		protected override def parts :Seq[SQLFormula[F, _]] = select::Nil

		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnFormula[S, Boolean] =
			mapper(select) match {
				case sel :SelectFormula[S, V, _] => sel.exists
				case other =>
					throw new IllegalArgumentException(
						s"Can't map $this with $mapper because it didn't return a select expression: $other :${other.getClass}"
					)
			}

		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Boolean] = matcher.exists(this)

		override def toString :String = s"EXISTS($select)"
	}



	object Exists {
		trait ExistsMatcher[+F <: FromClause, +Y[X]] {
			def exists[V, O](e :Exists[F, V, O]) :Y[Boolean]
		}

		type MatchExists[+F <: FromClause, +Y[X]] = ExistsMatcher[F, Y]

		type CaseExists[+F <: FromClause, +Y[X]] = ExistsMatcher[F, Y]
	}






	case class In[-F <: FromClause, V](left :ColumnFormula[F, V], right :SQLFormula[F, Seq[V]])
		extends SQLCondition[F]
	{
		override protected def parts :Seq[SQLFormula[F, _]] = left::right::Nil

		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnFormula[S, Boolean] =
			In(mapper(left), mapper(right))

		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Boolean] = matcher.in(this)

		override def toString :String = s"($left IN $right)"
	}



	object In {
		trait InMatcher[+F <: FromClause, +Y[X]] {
			def in[V](e :In[F, V]) :Y[Boolean]
		}

		type MatchIn[+F <: FromClause, +Y[X]] = InMatcher[F, Y]

		type CaseIn[+F <: FromClause, +Y[X]] = InMatcher[F, Y]
	}





/*
	trait ConditionMatcher[+F <: FromClause, +Y[X]]
		extends InMatcher[F, Y] with ExistsMatcher[F, Y] with ComparisonMatcher[F, Y]

	trait MatchCondition[+F <: FromClause, +Y[X]]
		extends CaseIn[F, Y] with CaseExists[F, Y] with CaseComparison[F, Y]

	trait CaseCondition[+F <: FromClause, +Y[X]] extends ConditionMatcher[F, Y] with MatchCondition[F, Y] {
		def condition(f :SQLCondition[F]) :Y[Boolean]

		override def in[X](f :In[F, X]) :Y[Boolean] = condition(f)

		override def exists[X](f :ExistsFormula[F, X]) :Y[Boolean] = condition(f)

		override def comparison[X](f: Comparison[F, X]): Y[Boolean] = condition(f)

	}
*/
	trait ConditionMatcher[+F <: FromClause, +Y[X]] extends ComparisonMatcher[F, Y]
		with ExistsMatcher[F, Y] with InMatcher[F, Y]

	trait MatchCondition[+F <: FromClause, +Y[X]] extends CaseComparison[F, Y]
		with CaseExists[F, Y] with CaseIn[F, Y]

	trait CaseCondition[+F <: FromClause, +Y[X]] extends ConditionMatcher[F, Y] with MatchCondition[F, Y] {
		def condition(e :SQLCondition[F]) :Y[Boolean]

		override def comparison[X](e :Comparison[F, X]) :Y[Boolean] = condition(e)

		override def in[V](e :In[F, V]) :Y[Boolean] = condition(e)

		override def exists[V, O](e :Exists[F, V, O]) :Y[Boolean] = condition(e)
	}



}


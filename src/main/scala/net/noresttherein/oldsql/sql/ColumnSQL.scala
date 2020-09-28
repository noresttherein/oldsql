package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, ColumnReadForm}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.AggregateSQL.{AggregateMatcher, CaseAggregate}
import net.noresttherein.oldsql.sql.ArithmeticSQL.{ArithmeticMatcher, CaseArithmetic}
import net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn.{AliasedColumnMatcher, CaseAliasedColumn}
import net.noresttherein.oldsql.sql.ColumnSQL.{AliasedColumn, ColumnMatcher}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.{CaseCompositeColumn, CompositeColumnMatcher}
import net.noresttherein.oldsql.sql.ConcatSQL.{CaseConcat, ConcatMatcher}
import net.noresttherein.oldsql.sql.ConversionSQL.{CaseColumnConversion, ColumnConversionMatcher, ColumnConversionSQL, ColumnPromotionConversion, MappedColumnSQL, OrNull}
import net.noresttherein.oldsql.sql.FromClause.{AggregateOf, ExtendedBy, FreeFrom, PartOf}
import net.noresttherein.oldsql.sql.LogicalSQL.{AND, CaseLogical, LogicalMatcher, NOT, OR}
import net.noresttherein.oldsql.sql.MappingSQL.ColumnComponentSQL.CaseColumnComponent
import net.noresttherein.oldsql.sql.MappingSQL.FreeColumn.CaseFreeColumn
import net.noresttherein.oldsql.sql.MappingSQL.{ColumnComponentSQL, FreeColumn, MappingColumnMatcher}
import net.noresttherein.oldsql.sql.SelectSQL.{CaseSelectColumn, FreeSelectColumn, SelectColumn, SelectColumnMatcher, SubselectColumn}
import net.noresttherein.oldsql.sql.ConditionSQL.{CaseCondition, ConditionMatcher, InSQL, LikeSQL}
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.{CaseColumnTerm, ColumnTermMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.{ColumnTerm, False, True}
import net.noresttherein.oldsql.sql.TupleSQL.IndexedChainTuple.IndexedColumn
import net.noresttherein.oldsql.sql.TupleSQL.SeqTuple




/** An `SQLExpression` which represents an SQL expression of a single, atomic value assignable to a column,
  * rather than a tuple.
  */
trait ColumnSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, V] extends SQLExpression[F, S, V] {
	override def readForm :ColumnReadForm[V]

	override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]]

	def as(alias :String) :ColumnSQL[F, S, V] = new AliasedColumn(this, alias)

	def @:[N <: Label](alias :N) :IndexedColumn[F, S, N, V] =
		new IndexedColumn(this, alias)

	def and[E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		AND(cast[Boolean]) and other

	def or [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		OR(cast[Boolean]) or other

	def && [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
			case True() => cast[Boolean]
			case False() => other
			case _ => this and other
		}

	def || [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean)
			:ColumnSQL[E, O, Boolean] =
		other match {
			case True() => other
			case False() => cast[Boolean]
			case _ => this or other
		}

	def unary_![E <: F, O >: LocalScope <: S](implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		NOT(cast[Boolean])



	def like(pattern :String)(implicit isString :V =:= String) :ColumnSQL[F, S, Boolean] =
		LikeSQL(cast[String], pattern)

	def +[E <: F, O >: LocalScope <: S]
	     (other :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
		ConcatSQL(cast[String]) + other

//todo: arithmetic


	def in[E <: F, O >: LocalScope <: S, X, U]
	      (that :SeqTuple[E, O, X])(implicit lift :SQLTypeUnification[V, X, U]) :ColumnSQL[E, O, Boolean] =
		new InSQL(lift.left(this), SeqTuple(that.parts.map(lift.right.apply[E, O])))



	override def cast[T](implicit ev :V =:= T) :ColumnSQL[F, S, T] =
		ev.substituteCo[({ type E[X] = ColumnSQL[F, S, X] })#E](this)

	override def to[X](implicit lift :Lift[V, X]) :ColumnSQL[F, S, X] =
		ColumnPromotionConversion(this, lift)

	override def opt :ColumnSQL[F, S, Option[V]] = OrNull(this)

	override def map[X](f :V => X) :ColumnSQL[F, S, X] = new MappedColumnSQL(this)(f)


	override def basedOn[E <: FromClause](implicit subtype :E <:< F) :ColumnSQL[E, S, V] =
		this.asInstanceOf[ColumnSQL[E, S, V]]

	override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :ColumnSQL[E, S, V]

	override def extend[U <: F, E <: FromClause]
	                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S) :ColumnSQL[E, S, V]


	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, V] =
		applyTo(matcher :ColumnMatcher[F, Y])

	def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, V]



	override def selectFrom[E <: F with FreeFrom, O](from :E) :FreeSelectColumn[V, O] =
		SelectSQL(from, this)

	override def subselectFrom[E <: F, O](from :E) :SubselectColumn[from.Base, V, O] =
		SelectSQL.subselect[from.Base, from.type, V, O](from, this)

}






object ColumnSQL {
	//todo: variant in uppercase
	implicit class ColumnSQLAggregateMethods[F <: FromSome, V](private val self :ColumnSQL[F, LocalScope, V])
		extends AnyVal
	{
		def count :AggregateSQL[F, F#GeneralizedAggregate, V, Int] = AggregateSQL.Count(self)

		def min(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateSQL.Min(self)

		def max(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateSQL.Max(self)

		def sum(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateSQL.Sum(self)

		def avg(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateSQL.Avg(self)

		def variance(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateSQL.Var(self)

		def stddev(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateSQL.StdDev(self)

	}



	type * = ColumnSQL[_ <: FromClause, _ >: LocalScope <: GlobalScope, _]

	type LocalColumnSQL[-F <: FromClause, V] = ColumnSQL[F, LocalScope, V]

	type GlobalColumnSQL[-F <: FromClause, V] = ColumnSQL[F, GlobalScope, V]


	class AliasedColumn[-F <: FromClause, -S >: LocalScope <: GlobalScope, V]
	                   (val column :ColumnSQL[F, S, V], val alias :String)
		extends CompositeColumnSQL[F, S, V]
	{
		protected override val parts :Seq[ColumnSQL[F, S, _]] = column::Nil

		override def readForm :ColumnReadForm[V] = column.readForm

		override def as(alias :String) :ColumnSQL[F, S, V] = new AliasedColumn(column, alias)

		override def @:[N <: Label](alias :N) :IndexedColumn[F, S, N, V] =
			new IndexedColumn(column, alias)

		override def isGlobal :Boolean = column.isGlobal

		override def asGlobal :Option[AliasedColumn[F, GlobalScope, V]] =
			if (column.isGlobal) Some(this.asInstanceOf[AliasedColumn[F, GlobalScope, V]])
			else None


		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] =
			new AliasedColumn(mapper(column), alias)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, V] =
			matcher.alias(this)

	}



	object AliasedColumn {
		trait AliasedColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def alias[S >: LocalScope <: GlobalScope, V](e :AliasedColumn[F, S, V]) :Y[S, V]
		}

		type MatchAliasedColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = AliasedColumnMatcher[F, Y]

		type CaseAliasedColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = AliasedColumnMatcher[F, Y]
	}



	trait CompositeColumnSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, X]
		extends CompositeSQL[F, S, X] with ColumnSQL[F, S, X]
	{
		override def asGlobal :Option[ColumnSQL[F, GlobalScope, X]] =
			if (isGlobal) Some(this.asInstanceOf[ColumnSQL[F, GlobalScope, X]])
			else None

		override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :ColumnSQL[E, S, X] =
			rephrase(SQLScribe.extend(base))

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S) :ColumnSQL[E, S, X] =
			rephrase(SQLScribe.extend(base))

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, X]
	}



	object CompositeColumnSQL {

		trait CompositeColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ConditionMatcher[F, Y] with LogicalMatcher[F, Y] with ArithmeticMatcher[F, Y]
			   with ConcatMatcher[F, Y] with ColumnConversionMatcher[F, Y] with AliasedColumnMatcher[F, Y]

		trait MatchCompositeColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompositeColumnMatcher[F, Y]
			   with CaseCondition[F, Y] with CaseLogical[F, Y] with CaseArithmetic[F, Y] with CaseColumnConversion[F, Y]
			   with CaseConcat[F, Y] with CaseAliasedColumn[F, Y]

		trait CaseCompositeColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchCompositeColumn[F, Y]
		{
			def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X]

			override def alias[S >: LocalScope <: GlobalScope, V](e :AliasedColumn[F, S, V]) :Y[S, V] = composite(e)

			override def arithmetic[S >: LocalScope <: GlobalScope, V](e :ArithmeticSQL[F, S, V]) :Y[S, V] =
				composite(e)

			override def concat[S >: LocalScope <: GlobalScope](e :ConcatSQL[F, S]) :Y[S, String] = composite(e)

			override def condition[S >: LocalScope <: GlobalScope](e :ConditionSQL[F, S]) :Y[S, Boolean] = composite(e)

			override def conversion[S >: LocalScope <: GlobalScope, Z, X](e :ColumnConversionSQL[F, S, Z, X]) :Y[S, X] =
				composite(e)

			override def logical[S >: LocalScope <: GlobalScope](e :LogicalSQL[F, S]) :Y[S, Boolean] = composite(e)
		}
	}






	trait ColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends AggregateMatcher[F, Y] with ColumnTermMatcher[F, Y] with CompositeColumnMatcher[F, Y]
		   with MappingColumnMatcher[F, Y] with SelectColumnMatcher[F, Y]

	trait MatchColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnMatcher[F, Y]
		with CaseAggregate[F, Y] with CaseColumnTerm[F, Y] with CaseCompositeColumn[F, Y]
		with CaseColumnComponent[F, Y] with CaseFreeColumn[F, Y] with CaseSelectColumn[F, Y]

	trait CaseColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchColumn[F, Y] {
		def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :Y[S, X]

		override def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[LocalScope, V] =
			column(e)

		override def *(e :ColumnSQL[FromClause, LocalScope, Nothing]) :Y[LocalScope, Nothing] =
			column[LocalScope, Nothing](e)

		override def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X] = column(e)

		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :Y[GlobalScope, V] =
			column(e)

		override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[O, M, V]) :Y[GlobalScope, V] =
			column(e)

		override def select[S >: LocalScope <: GlobalScope, V, O](e :SelectColumn[F, S, V, O]) :Y[S, Rows[V]] =
			column(e)

		override def term[X](e :ColumnTerm[X]) :Y[GlobalScope, X] = column(e)

	}

}

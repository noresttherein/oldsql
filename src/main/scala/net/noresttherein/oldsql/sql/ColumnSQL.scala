package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, ColumnReadForm}
import net.noresttherein.oldsql.sql.ArithmeticSQL.{ArithmeticMatcher, CaseArithmetic}
import net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn.{AliasedColumnMatcher, CaseAliasedColumn}
import net.noresttherein.oldsql.sql.ColumnSQL.{AliasedColumn, ColumnMatcher}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.{CaseCompositeColumn, CompositeColumnMatcher}
import net.noresttherein.oldsql.sql.ConcatSQL.{CaseConcat, ConcatMatcher}
import net.noresttherein.oldsql.sql.ConversionSQL.{CaseColumnConversion, ColumnConversionMatcher, ColumnConversionSQL, ColumnPromotionConversion, MappedColumnSQL, OrNull}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FreeFrom}
import net.noresttherein.oldsql.sql.LogicalSQL.{AND, CaseLogical, LogicalMatcher, NOT, OR}
import net.noresttherein.oldsql.sql.MappingSQL.ColumnComponentSQL.CaseColumnComponent
import net.noresttherein.oldsql.sql.MappingSQL.FreeColumn.CaseFreeColumn
import net.noresttherein.oldsql.sql.MappingSQL.{ColumnComponentSQL, FreeColumn, MappingColumnMatcher}
import net.noresttherein.oldsql.sql.SelectSQL.{CaseSelectColumn, FreeSelectColumn, SelectColumnMatcher, SubselectColumn}
import net.noresttherein.oldsql.sql.ConditionSQL.{CaseCondition, ConditionMatcher, In, LikeSQL}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, Lift, SQLTypePromotion}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.{CaseColumnTerm, ColumnTermMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.{False, True}
import net.noresttherein.oldsql.sql.TupleSQL.SeqTuple



/** An `SQLExpression` which represents an SQL expression of a single, atomic value assignable to a column,
  * rather than a tuple.
  */
trait ColumnSQL[-F <: FromClause, V] extends SQLExpression[F, V] {
	override def readForm :ColumnReadForm[V]



	def and[S <: F](other :SQLBoolean[S])(implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
		AND(ev(this)) and other

	def or [S <: F](other :SQLBoolean[S])(implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
		OR(ev(this)) or other

	def && [S <: F](other :SQLBoolean[S])(implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
		other match {
			case True() => ev(this)
			case False() => other
			case _ => this and other
		}

	def || [S <: F](other :SQLBoolean[S])(implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
		other match {
			case True() => other
			case False() => ev(this)
			case _ => this or other
		}

	def unary_![S <: F](implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] = NOT(ev(this))



	def like(pattern :String)(implicit isString :V =:= String) :SQLBoolean[F] =
		LikeSQL(isString.substituteCo[({ type E[X] = ColumnSQL[F, X] })#E](this), pattern)

	def +[S <: F](other :ColumnSQL[S, String])(implicit ev :V =:= String) :ColumnSQL[S, String] =
		ConcatSQL(ev.substituteCo[({ type E[X] = ColumnSQL[S, X] })#E](this)) + other

//todo: arithmetic


	def in [S <: F, X, U](that :SeqTuple[S, X])(implicit lift :SQLTypePromotion[V, X, U]) :SQLBoolean[S] =
		new In[S, U](lift.left(this), SeqTuple(that.parts.map(lift.right.apply[S])))


	override def to[X](implicit lift :Lift[V, X]) :ColumnSQL[F, X] =
		ColumnPromotionConversion(this, lift)

	override def opt :ColumnSQL[F, Option[V]] = OrNull(this)

	override def map[X](f :V => X) :ColumnSQL[F, X] = new MappedColumnSQL[F, V, X](this)(f)


	override def basedOn[E <: FromClause](implicit subtype :E <:< F) :ColumnSQL[E, V] =
		this.asInstanceOf[ColumnSQL[E, V]]

	override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :ColumnSQL[S, V]



	override def selectFrom[S <: F with FreeFrom, O](from :S) :FreeSelectColumn[V, O] =
		SelectSQL(from, this)

	override def subselectFrom[S <: F, O](from :S) :SubselectColumn[from.Base, V, O] =
		SelectSQL.subselect[from.Base, from.type, V, O](from, this)



	def as(alias :String) :ColumnSQL[F, V] = new AliasedColumn(this, alias)



	override def applyTo[Y[_]](matcher :ExpressionMatcher[F, Y]) :Y[V] =
		applyTo(matcher :ColumnMatcher[F, Y])

	def applyTo[Y[_]](matcher :ColumnMatcher[F, Y]) :Y[V]

}






object ColumnSQL {

	class AliasedColumn[-F <: FromClause, V](val column :ColumnSQL[F, V], val alias :String)
		extends CompositeColumnSQL[F, V]
	{
		protected override val parts = column::Nil

		override def readForm :ColumnReadForm[V] = column.readForm

		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, V] =
			new AliasedColumn(mapper(column), alias)

		override def applyTo[Y[X]](matcher :ColumnMatcher[F, Y]) :Y[V] = matcher.alias(this)

	}



	object AliasedColumn {
		trait AliasedColumnMatcher[+F <: FromClause, +Y[X]] {
			def alias[V](e :AliasedColumn[F, V]) :Y[V]
		}

		type MatchAliasedColumn[+F <: FromClause, +Y[X]] = AliasedColumnMatcher[F, Y]

		type CaseAliasedColumn[+F <: FromClause, +Y[X]] = AliasedColumnMatcher[F, Y]
	}



	trait CompositeColumnSQL[-F <: FromClause, X] extends CompositeSQL[F, X] with ColumnSQL[F, X] {
		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, X]

		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :ColumnSQL[S, X] =
			rephrase(SQLScribe.stretcher(base))
	}



	object CompositeColumnSQL {

		trait CompositeColumnMatcher[+F <: FromClause, +Y[X]]
			extends ConditionMatcher[F, Y] with LogicalMatcher[F, Y] with ArithmeticMatcher[F, Y]
			   with ConcatMatcher[F, Y] with ColumnConversionMatcher[F, Y] with AliasedColumnMatcher[F, Y]

		trait MatchCompositeColumn[+F <: FromClause, +Y[X]] extends CompositeColumnMatcher[F, Y]
			with CaseCondition[F, Y] with CaseLogical[F, Y] with CaseArithmetic[F, Y] with CaseColumnConversion[F, Y]
			with CaseConcat[F, Y] with CaseAliasedColumn[F, Y]

		trait CaseCompositeColumn[+F <: FromClause, +Y[X]] extends MatchCompositeColumn[F, Y] {
			def composite[X](e :CompositeColumnSQL[F, X]) :Y[X]

			override def arithmetic[V](e :ArithmeticSQL[F, V]) :Y[V] = composite(e)
			override def logical(e :LogicalSQL[F]) :Y[Boolean] = composite(e)
			override def conversion[Z, X](e :ColumnConversionSQL[F, Z, X]) :Y[X] = composite(e)
			override def concat(e :ConcatSQL[F]) :Y[String] = composite(e)
			override def condition(e :ConditionSQL[F]) :Y[Boolean] = composite(e)
			override def alias[V](e :AliasedColumn[F, V]) :Y[V] = composite(e)
		}
	}






	trait ColumnMatcher[+F <: FromClause, +Y[X]]
		extends ColumnTermMatcher[F, Y] with CompositeColumnMatcher[F, Y] with MappingColumnMatcher[F, Y]
			with SelectColumnMatcher[F, Y]

	trait MatchColumn[+F <: FromClause, +Y[X]] extends ColumnMatcher[F, Y]
		with CaseColumnTerm[F, Y] with CaseCompositeColumn[F, Y]
		with CaseColumnComponent[F, Y] with CaseFreeColumn[F, Y] with CaseSelectColumn[F, Y]

	trait CaseColumn[+F <: FromClause, +Y[X]] extends MatchColumn[F, Y] {
		def column[X](e :ColumnSQL[F, X]) :Y[X]

		override def composite[X](e :CompositeColumnSQL[F, X]) :Y[X] = column(e)

		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :Y[V] =
			column(e)

		override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[O, M, V]) :Y[V] =
			column(e)

		override def select[V, O](e :SelectSQL.SelectColumn[F, V, O]) :Y[Rows[V]] = column(e)

		override def term[X](e :SQLTerm.ColumnTerm[X]) :Y[X] = column(e)

	}

}

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
import net.noresttherein.oldsql.sql.MappingSQL.LooseColumnComponent.CaseLooseColumn
import net.noresttherein.oldsql.sql.MappingSQL.{ColumnComponentSQL, LooseColumnComponent, MappingColumnMatcher}
import net.noresttherein.oldsql.sql.SelectSQL.{CaseSelectColumn, FreeSelectColumn, SelectColumn, SelectColumnMatcher, SubselectColumn}
import net.noresttherein.oldsql.sql.ConditionSQL.{CaseCondition, ConditionMatcher, InSQL, LikeSQL}
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.{CaseColumnTerm, ColumnTermMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.{ColumnTerm, False, NULL, True}
import net.noresttherein.oldsql.sql.TupleSQL.IndexedChainTuple.IndexedColumn
import net.noresttherein.oldsql.sql.TupleSQL.SeqTuple




/** An [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] which can be represented in SQL by a single, atomic
  * value assignable to a column, rather than a tuple or a result set. Many methods from `SQLExpression`
  * are overriden in this trait in order to narrow their result type to `ColumnSQL`, but otherwise their semantics
  * remain the same.
  *
  * There is an implicit conversion from `ColumnSQL[F, _, V]` for any value type `V` having
  * the [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]] type class and
  * `F <: `[[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]] into
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLAggregateMethods ColumnQLAggregateMethods]] providing
  * extension methods for applying SQL aggregate functions such as
  * [[net.noresttherein.oldsql.sql.AggregateSQL.Count COUNT]] or [[net.noresttherein.oldsql.sql.AggregateSQL.Avg AVG]]
  * to this expression.
  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn]]
  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.GlobalColumn]]
  * @see [[net.noresttherein.oldsql.sql.SQLBoolean]]
  */
trait ColumnSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, V] extends SQLExpression[F, S, V] {
	override def readForm :ColumnReadForm[V]

	override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]]

	/** Provides a name to use in the 'as' clause appended to this expression, if it is used in a ''select'' clause. */
	def as(alias :String) :ColumnSQL[F, S, V] = new AliasedColumn(this, alias) //todo: precise semantics of name conflicts

	/** Attaches a label to this column in the form of a `String` literal type `N` for the purpose of indexing it
	  * inside an indexed SQL [[net.noresttherein.oldsql.sql.TupleSQL.IndexedChainTuple tuple]], allowing to access
	  * it later in a type safe manner. At the same time, the label is used as an alias for the column
	  * (replacing any previous alias) for the `as` clause appended to this column as in
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.as as]] method.
	  */
	def @:[N <: Label](alias :N) :IndexedColumn[F, S, N, V] =
		new IndexedColumn(this, alias)


	/** Joins this expression and another expression in a logical conjunction represented by the `AND` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return A `ColumnSQL` based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] being
	  *         the greatest lower bound of the bases of `this` and `that`,
	  *         and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]] of which is the intersection
	  *         of the scopes of the two expressions.
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.&&]] a variant of this method which recognizes constants
	  *      of `TRUE`, `FALSE` and `NULL`, reducing the result.
	  */
	def and[E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		AND(cast[Boolean]) and other

	/** Joins this expression and another expression in a logical disjunction represented by the `OR` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return A `ColumnSQL` based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] being
	  *         the greatest lower bound of the bases of `this` and `that`,
	  *         and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]] of which is the intersection
	  *         of the scopes of the two expressions.
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.||]] a variant of this method which recognizes constants
	  *      of `TRUE`, `FALSE` and `NULL`, reducing the result.
	  */
	def or [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		OR(cast[Boolean]) or other

	/** Joins this expression and another expression in a logical conjunction represented by the `AND` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`. Unlike
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.and and]], which is rendered explicitly as `this AND other`,
	  * this method recognizes the [[net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral literals]] `true`, `false`
	  * and `null`, reducing the result according to the rules of ternary logic.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.SQLTerm.NULL NULL]] literal,
	  *         than `NULL[Boolean]`. Otherwise a `ColumnSQL` based on
	  *         a [[net.noresttherein.oldsql.sql.FromClause FromClause]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def && [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
			case True() => cast[Boolean]
			case False() => other
			case NULL() => other
			case _ => this and other
		}

	/** Joins this expression and another expression in a logical disjunction represented by the `OR` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`. Unlike
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.or or]], which is rendered explicitly as `this OR other`,
	  * this method recognizes the [[net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral literals]] `true`, `false`
	  * and `null`, reducing the result according to the rules of ternary logic.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.SQLTerm.NULL NULL]] literal,
	  *         than `NULL[Boolean]`. Otherwise a `ColumnSQL` based on
	  *         a [[net.noresttherein.oldsql.sql.FromClause FromClause]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def || [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean)
			:ColumnSQL[E, O, Boolean] =
		other match {
			case True() => other
			case False() => cast[Boolean]
			case NULL() => other
			case _ => this or other
		}

	/** An SQL expression for `NOT this`. This method is available only if the value type of this expression is `Boolean`.
	  * If this expression is a [[net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral literal]] or
	  * [[net.noresttherein.oldsql.sql.SQLTerm.NULL NULL]], the result will be also a literal reduced according to
	  * ternary logic.
	  */
	def unary_![E <: F, O >: LocalScope <: S](implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		NOT(cast[Boolean])



	/** An SQL expression comparing this `String` expression with a pattern, given in the SQL format,
	  * using the `LIKE` operator.
	  */
	def like(pattern :String)(implicit isString :V =:= String) :ColumnSQL[F, S, Boolean] =
		LikeSQL(cast[String], pattern) //todo: why the pattern is a literal? Can't it be an arbitrary string?

	def ++[E <: F, O >: LocalScope <: S]
	      (other :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
		ConcatSQL(cast[String]) ++ other

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



	override def selectFrom[E <: F with FreeFrom](from :E) :FreeSelectColumn[V, _] =
		SelectSQL(from, this)

	override def subselectFrom(from :F) :SubselectColumn[from.Base, V, _] =
		SelectSQL.subselect[from.Base, from.type, V, Any](from, this)

}






object ColumnSQL {
	//todo: variant in uppercase
	/** Extension methods for [[net.noresttherein.oldsql.sql.ColumnSQL]] which apply standard SQL
	  * [[net.noresttherein.oldsql.sql.AggregateSQL.AggregateFunction aggregate]] functions to the enriched expression.
	  * Extracted out as it is restricted to expressions based on
	  * [[net.noresttherein.oldsql.sql.DiscreteFrom non-empty, not grouping]] `FromClause` subtypes and uses
	  * its [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome.GeneralizedAggregate GeneralizedAggregate]] member type
	  * as the base of the created expressions.
	  */
	implicit class ColumnSQLAggregateMethods[F <: FromSome, V](private val self :ColumnSQL[F, LocalScope, V])
		extends AnyVal
	{//fixme: remove abstract type projections
		/** Represents the SQL `COUNT(this)`.
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, Int]` based on clause
		  *         `G <: FromClause`, being the least upper bound of all aggregate expressions (`FromClause` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.FromClause.Generalized F#Generalized]].
		  */
		def count :AggregateSQL[F, F#GeneralizedAggregate, V, Int] = AggregateSQL.Count(self)

		/** Represents the SQL `MIN(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, V]` based on clause
		  *         `G <: FromClause`, being the least upper bound of all aggregate expressions (`FromClause` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.FromClause.Generalized F#Generalized]].
		  */
		def min(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateSQL.Min(self)

		/** Represents the SQL `MAX(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, V]` based on clause
		  *         `G <: FromClause`, being the least upper bound of all aggregate expressions (`FromClause` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.FromClause.Generalized F#Generalized]].
		  */
		def max(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateSQL.Max(self)

		/** Represents the SQL `SUM(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, V]` based on clause
		  *         `G <: FromClause`, being the least upper bound of all aggregate expressions (`FromClause` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.FromClause.Generalized F#Generalized]].
		  */
		def sum(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateSQL.Sum(self)

		/** Represents the SQL `AVG(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, BigDecimal]` based on clause
		  *         `G <: FromClause`, being the least upper bound of all aggregate expressions (`FromClause` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.FromClause.Generalized F#Generalized]].
		  */
		def avg(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateSQL.Avg(self)

		/** Represents the SQL `VAR(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, BigDecimal]` based on clause
		  *         `G <: FromClause`, being the least upper bound of all aggregate expressions (`FromClause` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.FromClause.Generalized F#Generalized]].
		  */
		def variance(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateSQL.Var(self)

		/** Represents the SQL `STDDEV(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, BigDecimal]` based on clause
		  *         `G <: FromClause`, being the least upper bound of all aggregate expressions (`FromClause` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.FromClause.Generalized F#Generalized]].
		  */
		def stddev(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateSQL.StdDev(self)

	}



	/** An upper bound of all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.FromClause ''from'' clause]] `F`.
	  * It restricts the application scope of the expression to the ''select'' and ''having'' clauses
	  * of an SQL ''select'' based on the clause `F`. It is not allowed in the ''where'' clause
	  * or within subselects of the select it is based on. This allows it to include aggregate expressions
	  * such as `count(*)` as its subtypes.
	  * This is a subtype of the [[net.noresttherein.oldsql.sql.SQLExpression.LocalSQL LocalSQL]]`[F, V]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope]]
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.GlobalColumn]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL]]
	  */
	type LocalColumn[-F <: FromClause, V] = ColumnSQL[F, LocalScope, V]

	/** An upper bound of all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.FromClause ''from'' clause]] `F`
	  * which can be used freely in the context of any SQL ''select'' based on `F` as well as any of its subselects.
	  * This is a subtype of the [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL GlobalSQL]]`[F, V]`.
	  * Most expression types derive from this type rather than
	  * its supertype [[net.noresttherein.oldsql.sql.SQLExpression.LocalSQL LocalSQL]], with the sole exception being
	  * [[net.noresttherein.oldsql.sql.AggregateSQL aggregate]] expressions.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn]]
	  */
	type GlobalColumn[-F <: FromClause, V] = ColumnSQL[F, GlobalScope, V]

	/** A type alias for SQL column [[net.noresttherein.oldsql.sql.ColumnSQL expressions]] independent of any relations
	  * in the FROM clause, that is applicable to any [[net.noresttherein.oldsql.sql.FromClause FromClause]].
	  * It is a subtype of [[net.noresttherein.oldsql.sql.SQLExpression.FreeExpression FreeExpression]]`[V]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */
	type FreeColumn[V] = ColumnSQL[FromClause, GlobalScope, V]

	/** An upper type bound of all `ColumnSQL[_, _, _]` instances. */
	type * = ColumnSQL[_ <: FromClause, _ >: LocalScope <: GlobalScope, _]



	/** A column with an alias given for use in the `as` clause. */
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
		def apply[F <: FromClause, S >: LocalScope <: GlobalScope, V]
		         (column :ColumnSQL[F, S, V], alias :String) :AliasedColumn[F, S, V] =
			new AliasedColumn[F, S, V](column, alias)

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope, V]
		           (expr :SQLExpression[F, S, V]) :Option[(ColumnSQL[F, S, V], String)] =
			expr match {
				case alias :AliasedColumn[F @unchecked, S @unchecked, V @unchecked] =>
					Some((alias.column, alias.alias))
				case _ =>
					None
			}


		trait AliasedColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def alias[S >: LocalScope <: GlobalScope, V](e :AliasedColumn[F, S, V]) :Y[S, V]
		}

		type MatchAliasedColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = AliasedColumnMatcher[F, Y]

		type CaseAliasedColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = AliasedColumnMatcher[F, Y]
	}



	/** Base type of [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] which are composed of other
	  * expressions. It is fully analogous to its supertype
	  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL CompositeSQL]], redefining the implementations
	  * of methods which needed narrowing of their result type to `ColumnSQL`.
	  */
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
		{
			def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X]
		}

		trait MatchOnlyCompositeColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompositeColumnMatcher[F, Y]
			   with CaseAliasedColumn[F, Y] with CaseArithmetic[F, Y] with CaseConcat[F, Y] with CaseCondition[F, Y]
			   with CaseLogical[F, Y]


		trait MatchCompositeColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchOnlyCompositeColumn[F, Y] with CaseColumnConversion[F, Y]

		trait CaseCompositeColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchCompositeColumn[F, Y]
		{
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
	{
		def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :Y[S, X]
	}

	trait MatchColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnMatcher[F, Y]
		with CaseAggregate[F, Y] with CaseColumnTerm[F, Y] with CaseCompositeColumn[F, Y]
		with CaseColumnComponent[F, Y] with CaseLooseColumn[F, Y] with CaseSelectColumn[F, Y]

	trait CaseColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchColumn[F, Y] {

		override def *(e :ColumnSQL[FromClause, LocalScope, Nothing]) :Y[LocalScope, Nothing] =
			column[LocalScope, Nothing](e)

		override def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[LocalScope, V] =
			column(e)

		override def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X] = column(e)

		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :Y[GlobalScope, V] =
			column(e)

		override def looseComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :LooseColumnComponent[O, M, V]) :Y[GlobalScope, V] =
			column(e)

		override def select[S >: LocalScope <: GlobalScope, V, O](e :SelectColumn[F, S, V, O]) :Y[S, Rows[V]] =
			column(e)

		override def term[X](e :ColumnTerm[X]) :Y[GlobalScope, X] = column(e)

	}

}

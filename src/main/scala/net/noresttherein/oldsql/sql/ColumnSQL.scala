package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, ColumnReadForm}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn.{AliasedColumnMatcher, CaseAliasedColumn}
import net.noresttherein.oldsql.sql.ColumnSQL.{AliasedColumn, ColumnMatcher}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.{CaseCompositeColumn, CompositeColumnMatcher}
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExtendedBy, GroundFrom, NonEmptyFrom, PartOf}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.{AggregateSQL, ArithmeticSQL, ConcatSQL, ConditionSQL, LogicalSQL, QuerySQL, SelectSQL, SQLTerm, TupleSQL}
import net.noresttherein.oldsql.sql.ast.AggregateSQL.{AggregateMatcher, CaseAggregate}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.{ArithmeticMatcher, CaseArithmetic}
import net.noresttherein.oldsql.sql.ast.ConcatSQL.{CaseConcat, ConcatMatcher}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.{CaseCondition, ConditionMatcher, InSQL, LikeSQL}
import net.noresttherein.oldsql.sql.ast.ConversionSQL.{CaseColumnConversion, ColumnConversionMatcher, ColumnConversionSQL, ColumnPromotionConversion, MappedColumnSQL, OrNull}
import net.noresttherein.oldsql.sql.ast.LogicalSQL.{AndSQL, CaseLogical, LogicalMatcher, NotSQL, OrSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{LooseColumn, MappingColumnMatcher, TypedColumnComponentSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn.CaseLooseColumn
import net.noresttherein.oldsql.sql.ast.MappingSQL.TypedColumnComponentSQL.CaseColumnComponent
import net.noresttherein.oldsql.sql.ast.QuerySQL.{CaseColumnQuery, ColumnQuery, ColumnQueryMatcher, Rows, SetColumnOperation}
import net.noresttherein.oldsql.sql.ast.QuerySQL.SetColumnOperation.{CaseSetColumnOperation, SetColumnOperationMatcher}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SelectColumn, SubselectColumn, TopSelectColumn}
import net.noresttherein.oldsql.sql.ast.SQLTerm.{ColumnTerm, False, SQLNull, True}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm.{CaseColumnTerm, ColumnTermMatcher}
import net.noresttherein.oldsql.sql.ast.TupleSQL.SeqTuple
import net.noresttherein.oldsql.sql.ast.TupleSQL.IndexedChainTuple.IndexedColumn
import net.noresttherein.oldsql.sql.mechanics.SQLScribe




/** An [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] which can be represented in SQL by a single, atomic
  * value assignable to a column, rather than a tuple or a result set. Many methods from `SQLExpression`
  * are overriden in this trait in order to narrow their result type to `ColumnSQL`, but otherwise their semantics
  * remain the same.
  *
  * There is an implicit conversion from `ColumnSQL[F, _, V]` for any value type `V` having
  * the [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]] type class and
  * `F <: `[[net.noresttherein.oldsql.sql.FromSome FromSome]] into
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLAggregateMethods ColumnQLAggregateMethods]] providing
  * extension methods for applying SQL aggregate functions such as
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count COUNT]] or [[net.noresttherein.oldsql.sql.AggregateFunction.Avg AVG]]
  * to this expression.
  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn]]
  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.GlobalColumn]]
  * @see [[net.noresttherein.oldsql.sql.SQLBoolean]]
  */
trait ColumnSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V] extends SQLExpression[F, S, V] {
	override def readForm :ColumnReadForm[V]

	override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]]


	/** Provides a name to use in the 'as' clause appended to this expression, if it is used in a ''select'' clause. */
	def as(alias :String) :ColumnSQL[F, S, V] = new AliasedColumn(this, alias) //todo: precise semantics of name conflicts

	/** Attaches a label to this column in the form of a `String` literal type `N` for the purpose of indexing it
	  * inside an indexed SQL [[net.noresttherein.oldsql.sql.ast.TupleSQL.IndexedChainTuple tuple]], allowing to access
	  * it later in a type safe manner. At the same time, the label is used as an alias for the column
	  * (replacing any previous alias) for the `as` clause appended to this column as in
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.as as]] method.
	  */
	def @:[N <: Label](alias :N) :IndexedColumn[F, S, N, V] =
		new IndexedColumn(this, alias)


	/** Joins this expression and another expression in a logical conjunction represented by the `AND` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return A `ColumnSQL` based on a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being
	  *         the greatest lower bound of the bases of `this` and `that`,
	  *         and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]] of which is the intersection
	  *         of the scopes of the two expressions.
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.&&]] a variant of this method which recognizes constants
	  *      of `TRUE`, `FALSE` and `NULL`, reducing the result.
	  */
	def and[E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		AndSQL(cast[Boolean]) and other

	/** Joins this expression and another expression in a logical disjunction represented by the `OR` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return A `ColumnSQL` based on a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being
	  *         the greatest lower bound of the bases of `this` and `that`,
	  *         and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]] of which is the intersection
	  *         of the scopes of the two expressions.
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.||]] a variant of this method which recognizes constants
	  *      of `TRUE`, `FALSE` and `NULL`, reducing the result.
	  */
	def or [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		OrSQL(cast[Boolean]) or other

	/** Joins this expression and another expression in a logical conjunction represented by the `AND` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`. Unlike
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.and and]], which is rendered explicitly as `this AND other`,
	  * this method recognizes the [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLLiteral literals]] `true`, `false`
	  * and `null`, reducing the result according to the rules of ternary logic.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise a `ColumnSQL` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def && [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
			case True() => cast[Boolean]
			case False() => other
			case SQLNull() => other
			case _ => this and other
		}

	/** Joins this expression and another expression in a logical disjunction represented by the `OR` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`. Unlike
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.or or]], which is rendered explicitly as `this OR other`,
	  * this method recognizes the [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLLiteral literals]] `true`, `false`
	  * and `null`, reducing the result according to the rules of ternary logic.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise a `ColumnSQL` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def || [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean)
			:ColumnSQL[E, O, Boolean] =
		other match {
			case True() => other
			case False() => cast[Boolean]
			case SQLNull() => other
			case _ => this or other
		}

	/** An SQL expression for `NOT this`. This method is available only if the value type of this expression is `Boolean`.
	  * If this expression is a [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLLiteral literal]] or
	  * [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]], the result will be also a literal reduced according to
	  * ternary logic.
	  */
	def unary_![E <: F, O >: LocalScope <: S](implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		NotSQL(cast[Boolean])



	/** An SQL expression comparing this `String` expression with a pattern, given in the SQL format,
	  * using the `LIKE` operator.
	  */
	def like(pattern :String)(implicit isString :V =:= String) :ColumnSQL[F, S, Boolean] =
		LikeSQL(cast[String], pattern) //todo: why the pattern is a literal? Can't it be an arbitrary string?

	/** Concatenates this string expression with another string.
	  * This method is available only for subtypes of `ColumnSQL[_, _, String]`.
	  */
	def ++[E <: F, O >: LocalScope <: S]
	      (that :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
		ConcatSQL(cast[String]) ++ that

	/** Concatenates this string expression with another string.
	  * This method is available only for subtypes of `ColumnSQL[_, _, String]`.
	  */
	def |[E <: F, O >: LocalScope <: S]
	     (that :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
		ConcatSQL(cast[String]) ++ that



	/** An SQL expression for minus this number. This method is available only for expressions of value type `V`
	  * with type class [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]].
	  */
	def unary_-(implicit number :SQLNumber[V]) :ColumnSQL[F, S, V] = ArithmeticSQL.Neg(this)

	/** An arithmetic sum of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]].
	  */
	def +[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Plus(types.left(this), types.right(that))

	/** An arithmetic subtraction of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]].
	  */
	def -[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Minus(types.left(this), types.right(that))

	/** An arithmetic multiplication of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]].
	  */
	def *[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Times(types.left(this), types.right(that))

	/** An arithmetic division of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]].
	  */
	def /[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Divide(types.left(this), types.right(that))

	/** An arithmetic division remainder of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]].
	  */
	def %[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Remainder(types.left(this), types.right(that))



	/** An SQL expression checking if this value is a member of the collection on the right side.
	  * @param that an SQL tuple expression: `(a0, a1, ... an)`.
	  */
	def in[E <: F, O >: LocalScope <: S, X, U]
	      (that :SeqTuple[E, O, X])(implicit lift :SQLTypeUnification[V, X, U]) :ColumnSQL[E, O, Boolean] =
		new InSQL(lift.left(this), SeqTuple(that.parts.map(lift.right.apply[E, O])))

	/** An SQL expression checking if this value is a member of the result set returned by the given query.
	  * @param that an SQL ''select'' or other expression returning a row cursor.
	  */
	def in[E <: F, X, U](that :QuerySQL[E, X])(implicit lift :SQLTypeUnification[V, X, U]) :ColumnSQL[E, S, Boolean] =
		if (lift.right == Lift.self[X])
			new InSQL(lift.left(this), that.rows.asInstanceOf[SQLExpression[E, GlobalScope, Seq[U]]])
		else
			new InSQL(lift.left(this), that.map(lift.right.apply(_ :X)).rows)



	override def cast[T](implicit ev :V =:= T) :ColumnSQL[F, S, T] =
		ev.substituteCo[({ type E[X] = ColumnSQL[F, S, X] })#E](this)

	override def to[X](implicit lift :Lift[V, X]) :ColumnSQL[F, S, X] =
		ColumnPromotionConversion(this, lift)

	override def opt :ColumnSQL[F, S, Option[V]] = OrNull(this)

	override def map[X](f :V => X) :ColumnSQL[F, S, X] = new MappedColumnSQL(this)(f)


	override def basedOn[E <: RowProduct](implicit subtype :E <:< F) :ColumnSQL[E, S, V] =
		this.asInstanceOf[ColumnSQL[E, S, V]]

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnSQL[E, S, V]

	override def extend[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S) :ColumnSQL[E, S, V]


	override def anchor(from :F) :ColumnSQL[F, S, V]


	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, V] =
		applyTo(matcher :ColumnMatcher[F, Y])

	def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, V]


	override def selectFrom(from :F) :SelectColumn[from.Base, V] =
		if (from.isParameterized)
			throw new IllegalArgumentException(
				s"Cannot use a parameterized clause as a basis for select $this: $from."
			)
		else if (from.isSubselect) {
			subselectFrom(from.asInstanceOf[ExactSubselectOf[from.type, NonEmptyFrom]])
				.asInstanceOf[SelectColumn[from.Base, V]]
		} else
			topSelectFrom(from.asInstanceOf[F with GroundFrom])

	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectColumn[V] =
		SelectSQL(from, this)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectColumn[B, V] =
		SelectSQL.subselect[B, from.type, V](from, this)

}






object ColumnSQL {
	//todo: variant in uppercase
	/** Extension methods for [[net.noresttherein.oldsql.sql.ColumnSQL]] which apply standard SQL
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL.AggregateFunction aggregate]] functions to the enriched expression.
	  * Extracted out as it is restricted to expressions based on
	  * [[net.noresttherein.oldsql.sql.FromClause non-empty, not grouping]] `RowProduct` subtypes and uses
	  * its [[net.noresttherein.oldsql.sql.FromSome.GeneralizedAggregate GeneralizedAggregate]] member type
	  * as the base of the created expressions.
	  */
	implicit class ColumnSQLAggregateMethods[F <: FromSome, V](private val self :ColumnSQL[F, LocalScope, V])
		extends AnyVal
	{//fixme: remove abstract type projections
		/** Represents the SQL `COUNT(this)`.
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, Int]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct.Generalized F#Generalized]].
		  */
		def count :AggregateSQL[F, F#GeneralizedAggregate, V, Int] = AggregateFunction.Count(self)

		/** Represents the SQL `MIN(this)`.
		  * @param ordering an implicit witness attesting that `V` is a comparable type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, V]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct.Generalized F#Generalized]].
		  */
		def min(implicit ordering :SQLOrdering[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateFunction.Min(self)

		/** Represents the SQL `MAX(this)`.
		  * @param ordering an implicit witness attesting that `V` is a comparable type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, V]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct.Generalized F#Generalized]].
		  */
		def max(implicit ordering :SQLOrdering[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateFunction.Max(self)

		/** Represents the SQL `SUM(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, V]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct.Generalized F#Generalized]].
		  */
		def sum(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateFunction.Sum(self)

		/** Represents the SQL `AVG(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, BigDecimal]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct.Generalized F#Generalized]].
		  */
		def avg(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateFunction.Avg(self)

		/** Represents the SQL `VAR(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, BigDecimal]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct.Generalized F#Generalized]].
		  */
		def variance(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateFunction.Var(self)

		/** Represents the SQL `STDDEV(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn LocalColumn]]`[G, BigDecimal]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct.Generalized F#Generalized]].
		  */
		def stddev(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateFunction.StdDev(self)

	}



	/** An upper bound of all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`.
	  * It restricts the application scope of the expression to the ''select'' and ''having'' clauses
	  * of an SQL ''select'' based on the clause `F`. It is not allowed in the ''where'' clause
	  * or within subselects of the select it is based on. This allows it to include aggregate expressions
	  * such as `count(*)` as its subtypes.
	  * This is a subtype of the [[net.noresttherein.oldsql.sql.SQLExpression.LocalSQL LocalSQL]]`[F, V]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope]]
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.GlobalColumn]]
	  * @see [[net.noresttherein.oldsql.sql.ast.AggregateSQL]]
	  */
	type LocalColumn[-F <: RowProduct, V] = ColumnSQL[F, LocalScope, V]

	/** An upper bound of all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`
	  * which can be used freely in the context of any SQL ''select'' based on `F` as well as any of its subselects.
	  * This is a subtype of the [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL GlobalSQL]]`[F, V]`.
	  * Most expression types derive from this type rather than
	  * its supertype [[net.noresttherein.oldsql.sql.SQLExpression.LocalSQL LocalSQL]], with the sole exception being
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn]]
	  */
	type GlobalColumn[-F <: RowProduct, V] = ColumnSQL[F, GlobalScope, V]

	/** A type alias for SQL column [[net.noresttherein.oldsql.sql.ColumnSQL expressions]] independent of any relations
	  * in the FROM clause, that is applicable to any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
	  * It is a subtype of [[net.noresttherein.oldsql.sql.SQLExpression.GroundExpression GroundExpression]]`[V]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */
	type TopColumn[V] = ColumnSQL[RowProduct, GlobalScope, V]

	/** An upper type bound of all `ColumnSQL[_, _, _]` instances. */
	type * = ColumnSQL[_ <: RowProduct, _ >: LocalScope <: GlobalScope, _]



	/** A column with an alias given for use in the `as` clause. */
	class AliasedColumn[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V]
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

		override def isAnchored :Boolean = column.isAnchored

		override def anchor(from :F) :AliasedColumn[F, S, V] = column.anchor(from) match {
			case same if same eq column => this
			case anchored => new AliasedColumn(anchored, alias)
		}

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] =
			new AliasedColumn(mapper(column), alias)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, V] =
			matcher.alias(this)

	}



	object AliasedColumn {
		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, V]
		         (column :ColumnSQL[F, S, V], alias :String) :AliasedColumn[F, S, V] =
			new AliasedColumn[F, S, V](column, alias)

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, V]
		           (expr :SQLExpression[F, S, V]) :Option[(ColumnSQL[F, S, V], String)] =
			expr match {
				case alias :AliasedColumn[F @unchecked, S @unchecked, V @unchecked] =>
					Some((alias.column, alias.alias))
				case _ =>
					None
			}


		trait AliasedColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def alias[S >: LocalScope <: GlobalScope, V](e :AliasedColumn[F, S, V]) :Y[S, V]
		}

		type MatchAliasedColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AliasedColumnMatcher[F, Y]

		type CaseAliasedColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AliasedColumnMatcher[F, Y]
	}



	/** Base type of [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] which are composed of other
	  * expressions. It is fully analogous to its supertype
	  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL CompositeSQL]], redefining the implementations
	  * of methods which needed narrowing of their result type to `ColumnSQL`.
	  */
	trait CompositeColumnSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X]
		extends CompositeSQL[F, S, X] with ColumnSQL[F, S, X]
	{
		override def asGlobal :Option[ColumnSQL[F, GlobalScope, X]] =
			if (isGlobal) Some(this.asInstanceOf[ColumnSQL[F, GlobalScope, X]])
			else None

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnSQL[E, S, X] =
			rephrase(SQLScribe.extend(base))

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S) :ColumnSQL[E, S, X] =
			rephrase(SQLScribe.extend(base))

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, X]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, X] =
//			matcher.composite(this)
	}



	object CompositeColumnSQL {

		/** Implementation-oriented base trait for `CompositeSQL` subclasses which consist of a single subexpression
		  * column with the same value type as this trait. It is unwise to reference this type directly and rely on any
		  * particular expression type extending it (other than in having the `value` subexpression property),
		  * as it is considered an implementation detail and is subject to change.
		  */
		trait UnaryColumnOperator[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, V]
			extends CompositeColumnSQL[F, S, V]
		{
			val value :ColumnSQL[F, S, X]
			protected override def parts :Seq[ColumnSQL[F, S, X]] = value::Nil

			override def isAnchored :Boolean = value.isAnchored

			override def anchor(form :F) :ColumnSQL[F, S, V] = value.anchor(form) match {
				case same if same eq value => this
				case anchored => reapply(anchored)
			}

			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] =
				reapply(mapper(value))

			protected def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
			                     (e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, V]


			override def contentsIsomorphic(other :CompositeSQL.*) :Boolean = other match {
				case unary :UnaryColumnOperator[_, _, _, _] => value isomorphic unary.value
				case _ => false
			}

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case unary :UnaryColumnOperator[_, _, _, _] if canEqual(unary) && unary.canEqual(this) =>
					value == unary.value
				case _ => false
			}

			override def hashCode :Int = value.hashCode
		}



		/** Implementation oriented base trait for `CompositeSQL` subclasses which consist of two column subexpressions
		  * with the same type (but possibly different than the value type of this trait).
		  * It is unwise to reference this type directly and rely on any
		  * particular expression type extending it (other than in having `left` and `right` subexpression properties),
		  * as it is considered an implementation detail and is subject to change.
		  */
		trait BinaryColumnOperator[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, V]
			extends CompositeColumnSQL[F, S, V]
		{
			val left :ColumnSQL[F, S, X]
			val right :ColumnSQL[F, S, X]

			protected override def parts :Seq[ColumnSQL[F, S, X]] = left::right::Nil

			override def isAnchored :Boolean = left.isAnchored && right.isAnchored

			override def anchor(from :F) :ColumnSQL[F, S, V] = (left.anchor(from), right.anchor(from)) match {
				case (l, r) if (l eq left) && (r eq right) => this
				case (l, r) => reapply(l, r)
			}

			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] =
				reapply(mapper(left), mapper(right))

			protected def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
			                     (left :ColumnSQL[E, C, X], right :ColumnSQL[E, C, X]) :ColumnSQL[E, C, V]


			override def contentsIsomorphic(other :CompositeSQL.*) :Boolean = other match {
				case e :BinaryColumnOperator[_, _, _, _] => (left isomorphic e.left) && (right isomorphic e.right)
				case _ => false
			}

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :BinaryColumnOperator[_, _, _, _] if canEqual(other) && other.canEqual(this) =>
					left == other.left && right == other.right
				case _ => false
			}

			override def hashCode :Int = left.hashCode * 31 + right.hashCode
		}

		trait CompositeColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends AliasedColumnMatcher[F, Y] with ArithmeticMatcher[F, Y] with ConcatMatcher[F, Y]
			   with ConditionMatcher[F, Y] with ColumnConversionMatcher[F, Y] with LogicalMatcher[F, Y]
			   with SetColumnOperationMatcher[F, Y]
		{
			def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X]
		}

		trait MatchOnlyCompositeColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompositeColumnMatcher[F, Y]
			   with CaseAliasedColumn[F, Y] with CaseArithmetic[F, Y] with CaseConcat[F, Y] with CaseCondition[F, Y]
			   with CaseLogical[F, Y]


		trait MatchCompositeColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchOnlyCompositeColumn[F, Y] with CaseColumnConversion[F, Y] with CaseSetColumnOperation[F, Y]

		trait CaseCompositeColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
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

			override def setOperation[V](e :SetColumnOperation[F, V]) :Y[GlobalScope, Rows[V]] =
				composite(e)
		}
	}






	trait ColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends AggregateMatcher[F, Y] with ColumnTermMatcher[F, Y] with CompositeColumnMatcher[F, Y]
		   with MappingColumnMatcher[F, Y] with ColumnQueryMatcher[F, Y]
	{
		def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :Y[S, X]
	}

	trait MatchColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnMatcher[F, Y]
		with CaseAggregate[F, Y] with CaseColumnTerm[F, Y] with CaseCompositeColumn[F, Y]
		with CaseColumnComponent[F, Y] with CaseLooseColumn[F, Y] with CaseColumnQuery[F, Y]

	trait CaseColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchColumn[F, Y] {

		override def *(e :ColumnSQL[RowProduct, LocalScope, Nothing]) :Y[LocalScope, Nothing] =
			column[LocalScope, Nothing](e)

		override def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[LocalScope, V] =
			column(e)

		override def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X] = column(e)

		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, E, M, V, O]) :Y[GlobalScope, V] =
			column(e)

		override def looseComponent[O >: F <: RowProduct, M[A] <: ColumnMapping[V, A], V]
		                          (e :LooseColumn[O, M, V]) :Y[GlobalScope, V] =
			column(e)

		override def query[V](e :ColumnQuery[F, V]) :Y[GlobalScope, Rows[V]] = column(e)

		override def term[X](e :ColumnTerm[X]) :Y[GlobalScope, X] = column(e)

	}

}

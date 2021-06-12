package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{Chain, Opt, ReversedList}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnReadForm}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.ColumnSQL.{AliasedColumn, ColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, PartOf, TopFrom}
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.SelectScope
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.StoredProcedure.Out
import net.noresttherein.oldsql.sql.ast.{denullify, AggregateSQL, AndSQL, ArithmeticSQL, BetweenSQL, ConcatSQL, InSQL, LikeSQL, LooseColumn, NotSQL, OrSQL, QuerySQL, SelectColumn, SelectSQL, SQLNull}
import net.noresttherein.oldsql.sql.ast.AggregateSQL.{AggregateVisitor, CaseAggregate}
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.TypedColumnComponentSQL.CaseColumnComponent
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL.{CaseCompositeColumn, CompositeColumnVisitor}
import net.noresttherein.oldsql.sql.ast.ConversionSQL.{ColumnPromotionConversion, MappedColumnSQL, OrNull}
import net.noresttherein.oldsql.sql.ast.MappingSQL.MappingColumnVisitor
import net.noresttherein.oldsql.sql.ast.LooseColumn.CaseLooseColumn
import net.noresttherein.oldsql.sql.ast.QuerySQL.{CaseColumnQuery, ColumnQuery, ColumnQueryVisitor, Rows}
import net.noresttherein.oldsql.sql.ast.SelectColumn.{SubselectColumn, TopSelectColumn}
import net.noresttherein.oldsql.sql.ast.SQLLiteral.{False, True}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm.{CaseColumnTerm, ColumnTermVisitor}
import net.noresttherein.oldsql.sql.ast.TupleSQL.SeqTuple
import net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL.ListingColumn
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLNumber, SQLOrdering, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** An [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] which can be represented in SQL by a single, atomic
  * value assignable to a column, rather than a tuple or a result set. Many methods from `SQLExpression`
  * are overriden in this trait in order to narrow their result type to `ColumnSQL`, but otherwise their semantics
  * remain the same.
  *
  * There is an implicit conversion from `ColumnSQL[F, _, V]` for any value type `V` having
  * the [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]] type class and
  * `F <: `[[net.noresttherein.oldsql.sql.FromSome FromSome]] into
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLExtension ColumnSQLExtension]] providing
  * extension methods for applying SQL aggregate functions such as
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count COUNT]]
  * or [[net.noresttherein.oldsql.sql.AggregateFunction.Avg AVG]]
  * to this expression.
  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.LocalColumn]]
  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.GlobalColumn]]
  * @see [[net.noresttherein.oldsql.sql.SQLBoolean]]
  */ //consider: renaming to DistinctSQL
trait ColumnSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V] extends SQLExpression[F, S, V] {
	override def readForm :ColumnReadForm[V]
//todo: priority (binding strength to avoid unnecessary usage of '(', ')'

	/** Provides a name to use in the 'as' clause appended to this expression, if it is used in a ''select'' clause. */
	def as(alias :String) :ColumnSQL[F, S, V] = new AliasedColumn(this, alias) //todo: precise semantics of name conflicts

	/** Attaches a label to this column in the form of a `String` literal type `N` for the purpose of indexing it
	  * inside an indexed SQL [[net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL tuple]], allowing to access
	  * it later in a type safe manner. At the same time, the label is used as an alias for the column
	  * (replacing any previous alias) for the `as` clause appended to this column as in
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.as as]] method.
	  */
	def @:[N <: Label](alias :N) :ListingColumn[F, S, N, V] =
		new ListingColumn(this, alias)

	//todo: remove these two
	/** Joins this expression and another expression in a logical conjunction represented by the `AND` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`.
	  * '''Note that alphanumeric method name leads to the lowest binding priority; consider using'''
	  * [[net.noresttherein.oldsql.sql.SQLExpression.&& &&]]''' as it is less bug prone'''.
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
		AndSQL(cast[Boolean]) and denullify(other)

	/** Joins this expression and another expression in a logical disjunction represented by the `OR` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`.
	  * '''Note that alphanumeric method name leads to the lowest binding priority; consider using'''
	  * [[net.noresttherein.oldsql.sql.SQLExpression.|| ||]]''' as it is less bug prone'''.
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
		OrSQL(cast[Boolean]) or denullify(other)

	/** Joins this expression and another expression in a logical conjunction represented by the `AND` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`. Unlike
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.and and]], which is rendered explicitly as `this AND other`,
	  * this method recognizes the [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] `true`, `false`
	  * and `null`, reducing the result according to the rules of ternary logic.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise a `ColumnSQL` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def && [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
			case null => SQLNull[Boolean]
			case SQLNull() => other
			case True => cast[Boolean]
			case False => other
			case _ => this and other
		}

	/** Joins this expression and another expression in a logical disjunction represented by the `OR` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`. Unlike
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.or or]], which is rendered explicitly as `this OR other`,
	  * this method recognizes the [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] `true`, `false`
	  * and `null`, reducing the result according to the rules of ternary logic.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise a `ColumnSQL` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def || [E <: F, O >: LocalScope <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean)
			:ColumnSQL[E, O, Boolean] =
		other match {
			case null => SQLNull[Boolean]
			case SQLNull() => other
			case True => other
			case False => cast[Boolean]
			case _ => this or other
		}
	//todo: xor
	/** An SQL expression for `NOT this`. This method is available only if the value type of this expression is `Boolean`.
	  * If this expression is a [[net.noresttherein.oldsql.sql.ast.SQLLiteral literal]] or
	  * [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]], the result will be also a literal reduced according to
	  * ternary logic.
	  */
	def unary_![E <: F, O >: LocalScope <: S](implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		NotSQL(cast[Boolean])


	//todo: methods which accept ColumnMapping and literals, as well as those creating bound parameters.
	/** Checks if the value of this expression falls between the given bounds using SQL ''between'' opertor. */
	def between[E <: F, O >: LocalScope <: S]
	           (bounds :(ColumnSQL[E, O, V], ColumnSQL[E, O, V]))(implicit ordering :SQLOrdering[V])
			:ColumnSQL[E, O, Boolean] =
		BetweenSQL(this, denullify(bounds._1), denullify(bounds._2))

	/** An SQL expression comparing this `String` expression with a pattern, given in the SQL format,
	  * using the `LIKE` operator.
	  */
	def like[E <: F, O >: LocalScope <: S]
	        (pattern :ColumnSQL[E, O, String])(implicit isString :V =:= String) :ColumnSQL[E, O, Boolean] =
		LikeSQL(cast[String], denullify(pattern))

	/** Concatenates this string expression with another string.
	  * This method is available only for subtypes of `ColumnSQL[_, _, String]`.
	  */
	def ++[E <: F, O >: LocalScope <: S]
	      (that :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
		ConcatSQL(cast[String]) ++ denullify(that)

	/** Prepends the given `String` expression to this string.
	  * This method is available only for subtypes of `ColumnSQL[_, _, String]` and is particularly useful
	  * when prepending a `String` value, rather than another expression, as it will force its implicit conversion
	  * (which [[net.noresttherein.oldsql.sql.ColumnSQL.++ ++]] would not).
	  */
	def ++:[E <: F, O >: LocalScope <: S]
	      (that :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
		denullify(that) ++: ConcatSQL(cast[String])

//	/** Concatenates this string expression with another string.
//	  * This method is available only for subtypes of `ColumnSQL[_, _, String]`.
//	  */
//	def |[E <: F, O >: LocalScope <: S]
//	     (that :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
//		ConcatSQL(cast[String]) ++ denullify(that)



	/** An SQL expression for minus this number. This method is available only for expressions of value type `V`
	  * with type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
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
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def +[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Plus(types.left(this), types.right(denullify(that)))

	/** An arithmetic subtraction of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def -[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Minus(types.left(this), types.right(denullify(that)))

	/** An arithmetic multiplication of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def *[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Times(types.left(this), types.right(denullify(that)))

	/** An arithmetic division of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def /[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Divide(types.left(this), types.right(denullify(that)))

	/** An arithmetic division remainder of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def %[E <: F, O >: LocalScope <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit types :SQLTypeUnification[V, X, Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Remainder(types.left(this), types.right(denullify(that)))



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

	override def out :ColumnSQL[F, S, Out[V]] = to[Out[V]]

	override def map[X](f :V => X) :ColumnSQL[F, S, X] = new MappedColumnSQL(this)(f)


	override def basedOn[E <: RowProduct](implicit subtype :E <:< F) :ColumnSQL[E, S, V] =
		this.asInstanceOf[ColumnSQL[E, S, V]]

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnSQL[E, S, V]

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :ColumnSQL[E, S, V]


	override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]]

	override def anchor(from :F) :ColumnSQL[F, S, V]

	override def bind[E <: F { type Params = Args }, Args](base :E, args :Args)
			:ColumnSQL[base.GeneralizedParamless, S, V] =
		SQLScribe.applyParams(base, base.bind(args) :base.GeneralizedParamless)(args)(this)

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ExpressionVisitor[F, Y]) :Y[S, V] =
		applyTo(visitor :ColumnVisitor[F, Y])

	protected def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, V]

	protected[sql] override def applyToForwarder[Y[-_ >: LocalScope <: GlobalScope, _]]
	                                            (visitor :ExpressionVisitor[F, Y]) :Y[S, V] =
		applyTo(visitor)

	protected[sql] final def applyToForwarder[Y[-_ >: LocalScope <: GlobalScope, _]]
	                                         (visitor :ColumnVisitor[F, Y]) :Y[S, V] =
		applyTo(visitor)

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

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:Select[P, V] =
		Select(from)(this)


	override def split(implicit op :OperationType) :Seq[ColumnSQL[F, S, _]] = ReversedList :+ this

	override def columnCount(implicit spelling :SQLSpelling) :Int = 1

	protected override def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
		defaultSpelling(context, params)::Nil

	/** Translates this expression into an SQL string according to the given format/dialect, adding parenthesis
	  * around the expression if necessary (the expression is not atomic and could be potentially split by operators
	  * of higher precedence). This is a polymorphic method: default implementation always adds parenthesis,
	  * unless the SQL returned by `spelling` already contains a pair; several subclasses override it simply
	  * to `spelling(this)(context, params)`.
	  */
	def inParens[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
	{
		val sql = spelling(this :ColumnSQL[E, S, V])(context, params)
		if (sql.sql.length >= 2 && sql.sql.head == '(' && sql.sql.last == ')') sql
		else "(" +: (sql + ")")
	}

}






object ColumnSQL {
	//todo: variant in uppercase
	/** Extension methods for [[net.noresttherein.oldsql.sql.ColumnSQL]] which apply standard SQL
	  * [[net.noresttherein.oldsql.sql.AggregateFunction aggregate]] functions to the enriched expression.
	  * Extracted out as it is restricted to expressions based on
	  * [[net.noresttherein.oldsql.sql.FromClause non-empty, not grouping]] `RowProduct` subtypes and uses
	  * its [[net.noresttherein.oldsql.sql.FromSome.GeneralizedAggregate GeneralizedAggregate]] member type
	  * as the base of the created expressions.
	  */
	implicit class ColumnSQLExtension[F <: FromSome, V](private val self :ColumnSQL[F, LocalScope, V]) extends AnyVal {
		//fixme: remove abstract type projections
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
	  */ //todo: rename to GroupColumn/DistinctGroupSQL; move to sql
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
	  */ //todo: rename to RowColumn/DistinctRowSQL
	type GlobalColumn[-F <: RowProduct, V] = ColumnSQL[F, GlobalScope, V]

	/** A type alias for SQL column [[net.noresttherein.oldsql.sql.ColumnSQL expressions]] independent of any relations
	  * in the FROM clause, that is applicable to any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
	  * It is a subtype of [[net.noresttherein.oldsql.sql.SQLExpression.GroundSQL GroundSQL]]`[V]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */ //todo: rename to GroundColumn/GroundDistinctSQL
	type TopColumn[V] = ColumnSQL[RowProduct, GlobalScope, V]

	/** An upper type bound of all `ColumnSQL[_, _, _]` instances. */
	type * = ColumnSQL[_ <: RowProduct, _ >: LocalScope <: GlobalScope, _]



	/** A column with an alias given for use in the `as` clause. The value given here is treated only as a suggestion:
	  * it will be ignored if the column appears in a position where `as` clause is illegal, and the alias
	  * can be changed when generating SQL to ensure uniqueness in the pertinent scope.
	  */
	class AliasedColumn[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V]
	                   (val column :ColumnSQL[F, S, V], val alias :String)
		extends CompositeColumnSQL[F, S, V]
	{
		protected override val parts :Seq[ColumnSQL[F, S, _]] = column::Nil

		override def readForm :ColumnReadForm[V] = column.readForm
		override def groundValue :Opt[V] = column.groundValue

		override def as(alias :String) :ColumnSQL[F, S, V] = new AliasedColumn(column, alias)

		override def @:[N <: Label](alias :N) :ListingColumn[F, S, N, V] =
			new ListingColumn(column, alias)

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

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, V] =
			visitor.alias(this)


		//this could conceivably be wrong if select returns it as a part of a composite column, but we are not there yet
		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		{
			val columnSQL = spelling(column :ColumnSQL[E, S, V])(context, params)
			if (spelling.scope == SelectScope) columnSQL else columnSQL + (" as " + alias)
		}

	}



	object AliasedColumn {
		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, V]
		         (column :ColumnSQL[F, S, V], alias :String) :AliasedColumn[F, S, V] =
			new AliasedColumn[F, S, V](column, alias)

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, V]
		           (expr :SQLExpression[F, S, V]) :Opt[(ColumnSQL[F, S, V], String)] =
			expr match {
				case alias :AliasedColumn[F @unchecked, S @unchecked, V @unchecked] =>
					Got((alias.column, alias.alias))
				case _ =>
					Lack
			}


		trait AliasedColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def alias[S >: LocalScope <: GlobalScope, V](e :AliasedColumn[F, S, V]) :Y[S, V]
		}

		type MatchAliasedColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AliasedColumnVisitor[F, Y]

		type CaseAliasedColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AliasedColumnVisitor[F, Y]
	}




	trait ColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends AggregateVisitor[F, Y] with ColumnQueryVisitor[F, Y] with ColumnTermVisitor[F, Y]
		   with CompositeColumnVisitor[F, Y] with MappingColumnVisitor[F, Y]
	{
		def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :Y[S, X]
	}

	trait MatchColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ColumnVisitor[F, Y]
		with CaseAggregate[F, Y] with CaseColumnQuery[F, Y] with CaseColumnTerm[F, Y] with CaseCompositeColumn[F, Y]
		with CaseColumnComponent[F, Y] with CaseLooseColumn[F, Y]

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

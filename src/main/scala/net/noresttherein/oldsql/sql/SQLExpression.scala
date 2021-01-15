package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, PartOf, TopFrom}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.{CaseComposite, CompositeMatcher}
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, GlobalSQL, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ColumnSQL.{AliasedColumn, ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.{CompositeColumnMatcher, MatchOnlyCompositeColumn}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{CaseQuery, CompoundSelectSQL, QueryMatcher, Rows}
import net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL.{CaseCompoundSelect, CompoundSelectMatcher}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.ast.{AggregateSQL, ArithmeticSQL, ConcatSQL, ConditionSQL, ConversionSQL, FunctionSQL, LogicalSQL, MappingSQL, QuerySQL, SelectSQL, SQLTerm, TupleSQL}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.{ComparisonSQL, EqualitySQL, InequalitySQL, IsNull}
import net.noresttherein.oldsql.sql.ast.ConversionSQL.{CaseConversion, ConversionMatcher, MappedSQL, PromotionConversion}
import net.noresttherein.oldsql.sql.ast.FunctionSQL.{CaseFunction, FunctionMatcher}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{CaseMapping, LooseComponent, MappingMatcher}
import net.noresttherein.oldsql.sql.ast.SQLTerm.{CaseTerm, SQLLiteral, SQLNull, SQLParameter, TermMatcher}
import net.noresttherein.oldsql.sql.ast.TupleSQL.{CaseTuple, ChainTuple, TupleMatcher}
import net.noresttherein.oldsql.sql.mechanics.{implicitSQLLiterals, SQLScribe, TableCount}

//here be implicits
import slang._





/** A representation of an SQL expression as an AST.
  * @tparam F a ''from'' clause - list of relations/tables which provide columns used in this expression.
  * @tparam S the ''scope'' defining where in an SQL statement this expression can be used.
  *           [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope local]] scope means it is restricted
  *           to the ''select'' and ''having'' clauses of SQL ''selects'' directly based on the clause `F`
  *           (or another ''from'' clause containing all relations present in
  *           the [[net.noresttherein.oldsql.sql.RowProduct.Explicit ''explicit'']] portion of `F`).
  *           [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope Global]] scope means that the expression
  *           can be used anywhere in a ''select'' based on `F`, as well as in any of its subselects
  *           - after rebasing with [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]].
  * @tparam V result type of the expression; may not necessarily be an SQL type, but a result type of some mapping.
  * @see [[net.noresttherein.oldsql.sql.ColumnSQL]]
  */
trait SQLExpression[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V] extends implicitSQLLiterals {
	import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL

	/** A form which will be used to read the values of this expression from an SQL [[java.sql.ResultSet ResultSet]]
	  * (if this expression is used as the ''select'' clause of a query).
	  */
	def readForm :SQLReadForm[V]

	/** An SQL expression testing if the value of this expression is `NULL` (by the database when executing the query).
	  * If this expression does not represent a single column, but a tuple/several inline columns, each individual
	  * column is tested for nullity.
	  */
	def isNull :ColumnSQL[F, S, Boolean] = IsNull(this)

	/** An SQL expression comparing the value of this expression with the given value.
	  * The argument value will be rendered as a parameter of the created [[java.sql.PreparedStatement PreparedStatement]],
	  * but must be nevertheless known at this place.
	  * @param value a value of any type `X` such that `V` and `X` can be automatically promoted by the database
	  *              to the same type `U` for the purpose of the comparison, and for which an implicit
	  *              [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @param form an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] used to set the value of the parameter
	  *             in the `PreparedStatement` using this expression.
	  * @see [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter]] the expression for ''bound'' SQL parameters.
	  * @see [[net.noresttherein.oldsql.sql.UnboundParam]] an ''unbound'' parameter introduced to the underlying `RowProduct`.
	  */ //we need SQLForm, not SQLReadForm because SQLParameter requires it, in case it's used in the select clause.
	def ==?[X, U](value :X)(implicit lift :SQLTypeUnification[V, X, U], form :SQLForm[X]) :ColumnSQL[F, S, Boolean] =
		this === value.?

	/** An SQL expression comparing the value of this expression with another expression for equality.
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]]
	  *         literal, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def ===[E <: F, O >: LocalScope <: S, X, U]
	       (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[V, X, U]) :ColumnSQL[E, O, Boolean] =
		that match {
			case null | SQLNull() => SQLNull[Boolean]
			case _ => EqualitySQL(lift.left(this), lift.right(that))
		}

	/** An SQL expression comparing if this expression equals a given value.
	  * @param that a scala value of a type which can be promoted to the same type `U` as the value type
	  *             of this expression. It will translate to an SQL literal.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def ===[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U]) :ColumnSQL[F, S, Boolean] =
		this === SQLLiteral(that)

	/** An SQL expression comparing the value of this expression with the value of a given relation component.
	  * @param component a mapping object for a table, component or column, with the same subject type as the value type
	  *                  of this expression. Its origin type must be the greatest upper bound of
	  *                  the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] it came from, which retains
	  *                  the table from which the mapping originated, and which is in
	  *                  its [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form. In other words,
	  *                  it is the source ''from'' clause type with all tables to the left of the table of mapping `M`
	  *                  replaced with an appropriate wildcard type and all join types upcast to their generalized form.
	  *                  Such instances can be obtained using various accessor methods of
	  *                  [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], which in turn
	  *                  is a common argument to functions creating SQL expressions and which can be always explicitly
	  *                  created with `from.`[[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.mappings mappings]].
	  * @param cast evidence that the given mapping has `E` as its `Origin` type, which helps the type inferer.
	  * @param project implicit type class for `M` which provides its type constructor accepting the origin type,
	  *                required for use in `RowProduct` subtypes.
	  * @param offset implicit evidence specifying the number of known tables in the argument's origin type `E`.
	  *               It equals the length of type `E` counted in joined relations, starting with either a wildcard type
	  *               or `From/Dual`.
	  * @return a Boolean expression comparing this expression with an SQL expression representing the given component.
	  */
	def ===[M <: MappingOf[V], E <: F]
	       (component :M)(implicit cast :M <:< RefinedMapping[V, E], project :OriginProjection[M, V],
	                               offset :TableCount[E, _ <: Numeral]) :ColumnSQL[E, S, Boolean] =
		this === LooseComponent(component)


	/** An SQL expression checking if the value of this expression and another expression are not equal.
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def <>[E <: F, O >: LocalScope <: S, X, U]
	      (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[V, X, U]) :ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => InequalitySQL(lift.left(this), lift.right(that))
		}

	def <>[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U]) :ColumnSQL[F, S, Boolean] =
		this <> SQLLiteral(that)

	/** An SQL expression comparing the value of this expression with another expression with `<=`.
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @param ordering a witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def <=[E <: F, O >: LocalScope <: S, X, U]
	      (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => ComparisonSQL(lift.left(this), ComparisonSQL.LTE, lift.right(that))
		}

	def <=[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this <= SQLLiteral(that)

	/** An SQL expression comparing the value of this expression with another expression with `<`.
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @param ordering a witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def <[E <: F, O >: LocalScope <: S, X, U]
	     (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => ComparisonSQL(lift.left(this), ComparisonSQL.LT, lift.right(that))
		}

	def <[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this === SQLLiteral(that)

	/** An SQL expression comparing the value of this expression with another expression with `>=`.
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @param ordering a witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def >=[E <: F, O >: LocalScope <: S, X, U]
	      (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => ComparisonSQL(lift.left(this), ComparisonSQL.GTE, lift.right(that))
		}

	def >=[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this >= SQLLiteral(that)

	/** An SQL expression comparing the value of this expression with another expression with `>`.
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @param ordering a witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def >[E <: F, O >: LocalScope <: S, X, U]
	     (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => ComparisonSQL(lift.left(this), ComparisonSQL.GT, lift.right(that))
		}

	def >[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this > SQLLiteral(that)


//todo: arithmetic

	/** Casts this expression to one with value type `T` based on implicit evidence. */
	def cast[T](implicit ev :V =:= T) :SQLExpression[F, S, T] =
		ev.substituteCo[({ type E[X] = SQLExpression[F, S, X] })#E](this)

	/** Lifts this expression to one of type `X`, without any effect on the actual generated SQL.
	  * Note that the expression will use the same `ColumnReadForm[V]` as this instance and only the result will
	  * be mapped into `X`, rather than switching to a different `getXxx` method of the JDBC `ResultSet`.
	  * It thus works the same way as [[net.noresttherein.oldsql.sql.SQLExpression.map map]], but takes advantage
	  * of an implicit `List` argument which can be compared with `equals`, making the resulting expression
	  * comparable with other instances of [[net.noresttherein.oldsql.sql.ast.ConversionSQL ConversionSQL]],
	  * which a custom function of `map` cannot be expected to facilitate.
	  */
	def to[X](implicit lift :Lift[V, X]) :SQLExpression[F, S, X] = PromotionConversion(this, lift)

	/** Lift this expression to one typed `Option[V]`, without any effect on the actual generated SQL.
	  * This is implemented simply as `this.`[[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[Option[V]]`. */
	def opt :SQLExpression[F, S, Option[V]] = to[Option[V]]

	/** Maps the read (selected) scala value of this expression, without any effect on the actual generated SQL. */
	def map[X](f :V => X) :SQLExpression[F, S, X] = new MappedSQL[F, S, V, X](this)(f)



	/** Upcasts this expression to the base ''from'' clause `E <: F`, using only implicit evidence about the subtype
	  * relation rather than explicit lower type bound (which would be an identity cast in Scala).
	  */
	def basedOn[E <: RowProduct](implicit subtype :E <:< F) :SQLExpression[E, S, V] =
		this.asInstanceOf[SQLExpression[E, S, V]]

	/** Treat this expression as an expression of a ''from'' clause containing this clause as its prefix.
	  * The expansion is limited only to clauses representing the same select as this clause - no
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] 'joins' can occur in `E` after `F`.
	  * This method is thus applicable to a strictly smaller set of ''from'' clauses than
	  * [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]], but is available for all expressions.
	  */
	def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SQLExpression[E, S, V]

	/** Treat this expression as an expression based on a ''from'' clause expanding (i.e. containing additional tables)
	  * the clause `F` this expression is based on. This method is available only for global expressions, i.e. those
	  * which can occur inside any subselect of a select with the ''from'' clause `F`. This method has thus a wider
	  * range of applicable ''from'' clauses than [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]],
	  * but is limited only to expressions conforming to `SQLExpression[F, GlobalScope, V]`.
	  */
	def expand[U <: F, E <: RowProduct]
	          (base :E)(implicit ext :U ExpandedBy E, global :GlobalScope <:< S) :SQLExpression[E, S, V]


	/** Answers if this expressions is applicable to the [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope global]]
	  * scope. If true, it can be safely cast to `SQLExpression[F, GlobalScope, V]`
	  * (and [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL GlobalSQL]]`[F, V]`).
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.asGlobal]]
	  */
	def isGlobal :Boolean = false

	/** Returns this expression in an option if it is a [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL global]]
	  * expression or `None` otherwise.
	  */
	def asGlobal :Option[GlobalSQL[F, V]]

	/** True, if this expression does not contain any
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent LooseComponent]] subexpression. Loose components
	  * are placeholder expression adapters of [[net.noresttherein.oldsql.schema.Mapping Mapping]] instances
	  * with a `RowProduct` subtype as their `Origin` type. They are not tied to any particular relation in `F`,
	  * which makes converting any enclosing expression into valid SQL impossible.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.anchor]]
	  */
	def isAnchored :Boolean

	/** Replaces all occurrences of [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent LooseComponent]]
	  * with a [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]] using the relation at
	  * the given position in `F`, (the first relation in its `RowProduct` type parameter) as its parent.
	  */
	def anchor(from :F) :SQLExpression[F, S, V]


	/** A visitor pattern callback in which this expression calls the method of `mather` most appropriate to its type. */
	def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, V]



	/** Creates an SQL ''select'' with this expression as its ''select'' clause. The ''from'' and ''where'' clauses
	  * are defined by this argument, while the ''select'' clause consists of all column sub-expressions constituting
	  * this expression.
	  * This method is supported only by chosen expression types, which have a well defined, unique SQL representation.
	  * These are mainly
	  * [[net.noresttherein.oldsql.sql.ast.TupleSQL TupleSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]]
	  * and all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subclasses.
	  * It is considered low level API exposed only to support potential extension by custom expression types
	  * and should not be used by the client code directly; prefer using
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] and its relatives instead.
	  *
	  * If `from.`[[net.noresttherein.oldsql.sql.RowProduct.isSubselect isSubselect]], then this method delegates
	  * to [[net.noresttherein.oldsql.sql.SQLExpression.subselectFrom subselectFrom]] and creates
	  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]]. Otherwise, it delegates to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.topSelectFrom topSelectFrom]] and creates
	  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]].
	  * If there are any unbound parameters inside the explicit portion of this clause,
	  * an `IllegalArgumentException` will be thrown. Additionally, the resulting ''select''
	  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
	  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
	  * the `Outer` clause).
	  * @param from any `RowProduct` instance conforming to the type this expression is based on, that is
	  *             containing all the relations listed in `F` in its suffix.
	  * @return a `SelectSQL` based on this instance's `Implicit` type (that is, embeddable only as an expression
	  *         in instances of this `this.Implicit`), using the given arbitrary expression as the ''select'' clause.
	  * @throws IllegalArgumentException if the `from` clause is parameterized (contains any `UnboundParam` joins).
	  * @throws UnsupportedOperationException if this expression cannot be used for a ''select'' clause.
	  */
	def selectFrom(from :F) :SelectSQL[from.Base, V] =
		if (from.isParameterized)
			throw new IllegalArgumentException(
				s"Cannot use a parameterized clause as a basis for select $this: $from."
			)
		else if (from.isSubselect) {
			subselectFrom(from.asInstanceOf[ExactSubselectOf[from.type, NonEmptyFrom]]).asInstanceOf[SelectSQL[from.Base, V]]
		} else
			topSelectFrom(from.asInstanceOf[F with GroundFrom])

	/** Creates a `SelectSQL` with this expression as the ''select'' clause and the given `from` clause.
	  * This method is supported only by a few expression types, mainly [[net.noresttherein.oldsql.sql.ast.TupleSQL TupleSQL]]
	  * and [[net.noresttherein.oldsql.sql.ast.MappingSQL.TypedComponentSQL TypedComponentSQL]] subclasses. It is considered
	  * low level API exposed only to support potential extension by custom expression types and should not be used
	  * by the client code directly; prefer using
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] and its relatives instead.
	  * @throws UnsupportedOperationException if this expression cannot be used as the complete ''select'' clause,
	  *                                       which is the default for all classes which do not override this method.
	  */
	def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectSQL[V] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

	/** Creates a subselect expression selecting this expression from the given `from` clause.
	  * This method is supported only by a few expression types, mainly [[net.noresttherein.oldsql.sql.ast.TupleSQL TupleSQL]]
	  * and [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]] subclasses. It is considered
	  * low level API exposed only to support potential extension by custom expression types and should not be used
	  * by the client code directly; prefer using
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] and its relatives instead.
	  * @throws UnsupportedOperationException if this expression cannot be used as the complete ''select'' clause,
	  *                                       which is the default for all classes which do not override this method.
	  */
	def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectSQL[B, V] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

	/** Creates an parameterized SQL ''select'' with this expression as its ''select'' clause.
	  * The ''from'' and ''where'' clauses are defined by this argument, while the ''select'' clause consists
	  * of all column sub-expressions constituting this expression.
	  * This method is supported only by chosen expression types, which have a well defined, unique SQL representation.
	  * These are mainly
	  * [[net.noresttherein.oldsql.sql.ast.TupleSQL TupleSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]]
	  * and all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subclasses.
	  * It is considered low level API exposed only to support potential extension by custom expression types
	  * and should not be used by the client code directly; prefer using
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] and its relatives instead.
	  *
	  * @param from a top-level `RowProduct` instance (i.e., not corresponding to a subselect) conforming to the type
	  *             this expression is based on, that is containing all the relations listed in `F` in its suffix.
	  * @return a `ParamSelect` parameterized with the unbound parameters in `from`.
	  * @throws UnsupportedOperationException if this expression cannot be used for a ''select'' clause.
	  */
	def paramSelectFrom[E <: F with TopFrom { type Params = P }, P <: Chain](from :E) :ParamSelect[P, V] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)



	/** List of [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter ''bound'']] parameters used by this expression,
	  * in the order in which they would appear in the rendered SQL.
	  */
	def parameters :Seq[SQLParameter[_]] = collect { case x :SQLParameter[_] => x }

	/** Applies the given partial function recursively to all subexpressions of this expression (in the preorder). */
	def collect[X](fun :PartialFunction[SQLExpression.*, X]) :Seq[X] = reverseCollect(fun, Nil).reverse

	/** Implementation target of [[net.noresttherein.oldsql.sql.SQLExpression.collect collect]], realising its
	  * contract in the reversed order - the tree is traversed in postorder, and the parts preprended
	  * to the accumulator `acc`.
	  * @return by default `fun.lift(this) ++: acc`.
	  */
	protected def reverseCollect[X](fun :PartialFunction[SQLExpression.*, X], acc :List[X]) :List[X] =
		fun.lift(this) ++: acc

	/** A bridge method exporting the `reverseCollect` method of other expressions to any subtype of this trait. */
	protected[this] final def reverseCollect[X](e :SQLExpression.*)
	                                           (fun :PartialFunction[SQLExpression.*, X], acc :List[X]) :List[X] =
		e.reverseCollect(fun, acc)

	protected[sql] final def bridgeReverseCollect[X](fun :PartialFunction[SQLExpression.*, X], acc :List[X]) :List[X] =
		reverseCollect(fun, acc)



	/** Tests if this expression is equal to the given one abstracting from possibly different sources.
	  * Basically, if both expressions would produce the same SQL they should be isomorphic.
	  */
	def isomorphic(expression :SQLExpression.*) :Boolean

	/** Tests if this expression would produce the same value as the given expression, abstracting from possibly
	  * different clauses. Similar to isomorphic, but generally disregards order of elements in composite expressions
	  * such as 'and', 'or', seq. Be warned that this method's primary use is for tests, and production code shouldn't
	  * depend on it.
	  */
	private[oldsql] def equivalent(expression :SQLExpression.*) :Boolean = isomorphic(expression)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLExpression.*]

}







object SQLExpression  {

	implicit class SQLExpressionChaining[F <: RowProduct, S >: LocalScope <: GlobalScope, T]
	                                    (private val self :SQLExpression[F, S, T])
		extends AnyVal
	{
		@inline def ~[O >: LocalScope <: S, H](head :SQLExpression[F, O, H]) :ChainTuple[F, O, @~ ~ T ~ H] =
			EmptyChain ~ self ~ head
	}



	/** Default scope of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], signifying that it can be used
	  * solely within the ''select'' and ''having'' clauses for the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
	  * serving as the base for the expression. Such expressions are illegal for subselects of the mentioned ''select'',
	  * that is it cannot be converted to another ''from'' clause `E` expanding the original clause `F`. This stands
	  * in contrast to the [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope GlobalScope]], which is a supertype
	  * of this type, and which permits such usage. Purely local expressions are reserved for SQL aggregate functions:
	  * `count(*)` of an SQL ''select'' cannot be used as a part of another ''select''.
	  * Note that type `SQLExpression` is contravariant in scope, meaning `SQLExpression[F, LocalScope, T]`
	  * is a supertype of `SQLExpression[F, GlobalScope, T]`, permitting the use of non-aggregate expressions
	  * in the local scope of a ''select'' as expected.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.LocalSQL]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PartOf]]
	  * @see [[net.noresttherein.oldsql.sql.ast.AggregateSQL]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateClause]]
	  */ //the reverse direction of inheritance is because type inference always picks the upper bound when instantiating free type variables.
	type LocalScope <: GlobalScope

	/** The type used as the ''scope'' type argument `S` of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
	  * instances which do not contain any SQL aggregate functions as their subexpressions,
	  * signifying that they can be converted to any [[net.noresttherein.oldsql.sql.RowProduct ''from'']] clause `E`
	  * expanding the clause `F` on which the expression is based, in particular within subselect expressions
	  * of the SQL ''select'' containing it.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PartOf]]
	  */
	type GlobalScope

	val GlobalScope = implicitly[GlobalScope <:< GlobalScope]

	/** An upper bound of all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`.
	  * It restricts the application scope of the expression to the ''select'' and ''having'' clauses
	  * of an SQL ''select'' based on the clause `F`. It is not allowed in the ''where'' clause
	  * or within subselects of the select it is based on. This allows it to include aggregate expressions
	  * such as `count(*)` as its subtypes.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.AggregateSQL]]
	  */
	type LocalSQL[-F <: RowProduct, V] = SQLExpression[F, LocalScope, V]

	/** An upper bound of all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`
	  * which can be used freely in the context of any SQL ''select'' based on `F` as well as any of its subselects.
	  * Most expression types derive from this type rather than
	  * its supertype [[net.noresttherein.oldsql.sql.SQLExpression.LocalSQL LocalSQL]], with the sole exception being
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.LocalSQL]]
	  */
	type GlobalSQL[-F <: RowProduct, V] = SQLExpression[F, GlobalScope, V]

	/** A type alias for [[net.noresttherein.oldsql.sql.SQLExpression SQL expressions]] independent of any relations
	  * in the FROM clause, that is applicable to any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */
	type GroundExpression[T] = SQLExpression[RowProduct, GlobalScope, T]


	/** An upper type bound of all `SQLExpression[_, _, _]` instances. */
	type * = SQLExpression[_ <: RowProduct, _ >: LocalScope <: GlobalScope, _]






	/** Base type of [[net.noresttherein.oldsql.sql.SQLExpression expressions]] which are composed of other expressions.
	  * The number of subexpressions can be fixed (including to a single one), or vary between instances.
	  * Likewise, the composite subtype may define the value type of included expressions, or be heterogeneous/agnostic.
	  * All constituting expressions must be applicable in the same context as this expression
	  * ([[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  * and the [[net.noresttherein.oldsql.sql.RowProduct base]] ''from'' clause), which excludes
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions.
	  * The assumption is however that their exact subtypes of the `SQLExpression` do not matter, aside from
	  * the mentioned restrictions on their type parameters, and that any of them can be replaced with another
	  * `SQLExpression` conforming to these bounds, and that the result will be a valid SQL expression which can
	  * serve as a substitute for this expression, again producing a valid expression.
	  *
	  * Such expressions are often treated the same way by
	  * [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionMatcher visitors]] of the `SQLExpression` type hierarchy,
	  * which recursively reapply themselves to the subexpression of this expression (possibly producing another
	  * `SQLExpression` by reapplying the same type of the composite expression to the mapped subexpressions.
	  * The additional methods defined here facilitate this behaviour as well as structural comparison
	  * for [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.equals equals]] and
	  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.isomorphic isomorphic]].
	  */
	trait CompositeSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V] extends SQLExpression[F, S, V] {
		protected def parts :Seq[SQLExpression[F, S, _]]

		def inOrder :Seq[SQLExpression[F, S, _]] = parts

		override def isGlobal :Boolean = parts.forall(_.isGlobal)

		override def asGlobal :Option[GlobalSQL[F, V]] =
			if (isGlobal) Some(this.asInstanceOf[GlobalSQL[F, V]])
			else None

		override def isAnchored :Boolean = parts.forall(_.isAnchored)

		/** Method used in the implementation of
		  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.basedOn basedOn]] and
		  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.expand expand]].
		  * It should apply `mapper` to all constituting subexpressions and reassemble them into another
		  * composite expression of the same type as this one.
		  */
		def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V]

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SQLExpression[E, S, V] =
			rephrase(SQLScribe.expand(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :SQLExpression[E, S, V] =
			rephrase(SQLScribe.expand(base))

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, V] =
//			matcher.composite(this)



		protected override def reverseCollect[X](fun: PartialFunction[SQLExpression.*, X], acc: List[X]): List[X] =
			(super.reverseCollect(fun, acc) /: inOrder)((collected, member) => member.reverseCollect(fun, collected))


		/** Defaults to [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.canEqual canEqual]]. */
		def sameAs(other :CompositeSQL.*) :Boolean = canEqual(other)

		private[oldsql] override def equivalent(expression: SQLExpression.*): Boolean = expression match {
			case e :CompositeSQL.* =>
				(e eq this) || e.sameAs(this) && this.sameAs(e) && contentsEquivalent(e)
			case _ => false
		}

		/** Compares this expression with another expression, which must be a `CompositeSQL` in order for the test
		  * to turn out positive in a way mirroring the implementation of
		  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.equals equals]].
		  * The difference is that [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.sameAs sameAs]] is used
		  * instead of [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.canEqual canEqual]],
		  * and the part lists are compared using their `isomorphic` methods instead of equality.
		  * Both the order and the number of subexpressions must still match this expression.
		  */
		override def isomorphic(expression :SQLExpression.*) :Boolean = expression match {
			case e :CompositeSQL.* =>
				(this eq e) || sameAs(e) && e.sameAs(this) && contentsIsomorphic(e)
			case _ => false
		}


		private[oldsql] def contentsEquivalent(other :CompositeSQL.*) :Boolean =
			parts.size == other.parts.size &&
				parts.forall(e => other.parts.exists(_ equivalent e)) &&
				other.parts.forall(e => parts.exists(_ equivalent e))

		/** Verifies that the [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.parts parts]] lists
		  * of both expressions are of the same length and that all matched pairs are
		  * [[net.noresttherein.oldsql.sql.SQLExpression.isomorphic isomorphic]].
		  */
		def contentsIsomorphic(other :CompositeSQL.*) :Boolean =
			parts.size == other.parts.size &&
				((parts zip other.parts) forall { case (left, right) => left isomorphic right })


		/** A method used to attain the symmetry of the equality relation on `CompositeSQL` instances.
		  * It is tested by the `equals` method in ''both'' directions before comparing the contents
		  * of compared expressions. The default expressions checks if both `this` and `that` are of the same class
		  * which should be satisfactory for most implementations.
		  */
		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		/** Symmetrically tests that both `this canEqual that` and `that canEqual this` as well as the part lists
		  * of both expressions are equal (including the ordering). This implementation is thus suitable for
		  * most subtypes of this type, requiring derived classes to implement only `canEqual`.
		  */
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case composite :CompositeSQL[_, _, _] if canEqual(composite) && composite.canEqual(this) =>
				parts == composite.parts
			case _ => false
		}

		override def hashCode :Int = parts.view.map(_.hashCode).reduce(_ * 31 + _)

	}



	object CompositeSQL { //todo: FunctionSQL, ProcedureSQL

		/** Implementation-oriented base trait for `CompositeSQL` subclasses which consist of a single subexpression.
		  * It is unwise to reference this type directly and rely on any
		  * particular expression type extending it (other than in having `left` and `right` subexpression properties),
		  * as it is considered an implementation detail and is subject to change.
		  */
		trait UnaryOperatorSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, V] extends CompositeSQL[F, S, V] {

			val value :SQLExpression[F, S, X]
			protected override def parts :Seq[SQLExpression[F, S, X]] = value::Nil

			override def isAnchored :Boolean = value.isAnchored

			override def anchor(from :F) :SQLExpression[F, S, V] = value.anchor(from) match {
				case same if same eq value => this
				case anchored => reapply(anchored)
			}

			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V] =
				reapply(mapper(value))

			protected def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
			                     (e :SQLExpression[E, C, X]) :SQLExpression[E, C, V]


			override def contentsIsomorphic(other :CompositeSQL.*) :Boolean = other match {
				case unary :UnaryOperatorSQL[_, _, _, _] => value isomorphic unary.value
				case _ => false
			}

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case unary :UnaryOperatorSQL[_, _, _, _] if canEqual(unary) && unary.canEqual(this) =>
					value == unary.value
				case _ => false
			}

			override def hashCode :Int = value.hashCode
		}



		/** Implementation oriented base trait for `CompositeSQL` subclasses which consist of two subexpressions
		  * with the same type (but possibly different than the value type of this trait).
		  * It is unwise to reference this type directly and rely on any
		  * particular expression type extending it (other than in having the `value` subexpression property),
		  * as it is considered an implementation detail and is subject to change.
		  */
		trait BinaryOperatorSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, V] extends CompositeSQL[F, S, V] {
			val left :SQLExpression[F, S, X]
			val right :SQLExpression[F, S, X]

			protected override def parts :Seq[SQLExpression[F, S, X]] = left::right::Nil

			override def isAnchored :Boolean = left.isAnchored && right.isAnchored

			override def anchor(from :F) :SQLExpression[F, S, V] = (left.anchor(from), right.anchor(from)) match {
				case (l, r) if (l eq left) && (r eq right) => this
				case (l, r) => reapply(l, r)
			}

			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V] =
				reapply(mapper(left), mapper(right))

			protected def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
			                     (left :SQLExpression[E, C, X], right :SQLExpression[E, C, X]) :SQLExpression[E, C, V]


			override def contentsIsomorphic(other :CompositeSQL.*) :Boolean = other match {
				case e :BinaryOperatorSQL[_, _, _, _] => (left isomorphic e.left) && (right isomorphic e.right)
				case _ => false
			}

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :BinaryOperatorSQL[_, _, _, _] if canEqual(other) && other.canEqual(this) =>
					left == other.left && right == other.right
				case _ => false
			}

			override def hashCode :Int = left.hashCode * 31 + right.hashCode
		}

		type * = CompositeSQL[_ <: RowProduct, _ >: LocalScope <: GlobalScope, _]

		trait CompositeMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompositeColumnMatcher[F, Y] with ConversionMatcher[F, Y] with FunctionMatcher[F, Y]
			   with CompoundSelectMatcher[F, Y] with TupleMatcher[F, Y]
		{
			def composite[S >: LocalScope <: GlobalScope, X](e :CompositeSQL[F, S, X]) :Y[S, X]
		}

		trait MatchComposite[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends CompositeMatcher[F, Y]
			with CaseConversion[F, Y] with CaseFunction[F, Y] with CaseCompoundSelect[F, Y] with CaseTuple[F, Y]
			with MatchOnlyCompositeColumn[F, Y]
		{
			override def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X] =
				composite(e :CompositeSQL[F, S, X])
		}

		/* There is a repetition with CaseCompositeColumn, but it can't be extended here, as mixing it in
		 * should always rewire the column columns through the column matcher hierarchy up to composite column*/
		trait CaseComposite[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchComposite[F, Y] {

			override def alias[S >: LocalScope <: GlobalScope, V](e :AliasedColumn[F, S, V]) :Y[S, V] = composite(e)

			override def arithmetic[S >: LocalScope <: GlobalScope, V](e :ArithmeticSQL[F, S, V]) :Y[S, V] =
				composite(e)

			override def concat[S >: LocalScope <: GlobalScope](e :ConcatSQL[F, S]) :Y[S, String] = composite(e)

			override def condition[S >: LocalScope <: GlobalScope](e :ConditionSQL[F, S]) :Y[S, Boolean] = composite(e)

			override def conversion[S >: LocalScope <: GlobalScope, Z, X](e :ConversionSQL[F, S, Z, X]) :Y[S, X] =
				composite(e)

			override def function[S >: LocalScope <: GlobalScope, X <: Chain, Z](e :FunctionSQL[F, S, X, Z]) :Y[S, Z] =
				composite(e)

			override def logical[S >: LocalScope <: GlobalScope](e :LogicalSQL[F, S]) :Y[S, Boolean] = composite(e)

			override def compoundSelect[V](e :CompoundSelectSQL[F, V]) :Y[GlobalScope, Rows[V]] = composite(e)

			override def tuple[S >: LocalScope <: GlobalScope, X](e :TupleSQL[F, S, X]) :Y[S, X] = composite(e)
		}

	}






	/** Attests that expressions of type `L` and `R` are compatible from the SQL point of view and
	  * can be directly compared in scala after lifting both sides to type `U`.
	  * This may mean for example that, for the purpose of generated SQL, we treat `Option[X]` and `X`
	  * as directly comparable: `SQLTypeUnification[Option[X], X, Option[X]]` or let us promote number types to
	  * a higher precision: `SQLTypeUnification[Int, Long, Long]`.
	  *
	  * @param left a function lifting both `SQLExpression[_, _, L]` and type `L` itself to a comparable type `U`.
	  * @param right a function lifting both `SQLExpression[_, _, R]` and type `R` itself to a comparable type `U`.
	  * @tparam L type of the left side of a comparison.
	  * @tparam R type of the right side of a comparison.
	  * @tparam U type to which both types are promoted in order to be directly comparable.
	  */
	@implicitNotFound("Types ${L} and ${R} are not interoperable in SQL (as type ${T}). " +
	                  "Missing implicit SQLTypeUnification[${L}, ${R}, ${T}] (required Lift[${L}, ${R}] or Lift[${R}, ${L}]).")
	class SQLTypeUnification[L, R, U](val left :Lift[L, U], val right :Lift[R, U]) {
		def swapped :SQLTypeUnification[R, L, U] = new SQLTypeUnification(right, left)
		override def toString = s"$left =~= $right"

		/** Converts the value of the left side to the value of the right side, if possible. */
		def l2r(l :L) :Option[R] = right.inverse(left(l))

		/** Converts the value of the right side to the value of the left side, if possible. */
		def r2l(r :R) :Option[L] = left.inverse(right(r))
	}



	sealed abstract class MirroredTypePromotion {
		implicit def mirror[L, R, T](implicit promotion :SQLTypeUnification[L, R, T]) :SQLTypeUnification[R, L, T] =
			promotion.swapped
	}

	object SQLTypeUnification extends MirroredTypePromotion {
		import Lift._

		implicit def directly[T] :SQLTypeUnification[T, T, T] =
			direct.asInstanceOf[SQLTypeUnification[T, T, T]]

		implicit def liftLeft[L, R](implicit lift :Lift[L, R]) :SQLTypeUnification[L, R, R] =
			new SQLTypeUnification(lift, self)

		implicit def liftRight[L, R](implicit lift :Lift[R, L]) :SQLTypeUnification[L, R, L] =
			new SQLTypeUnification(self, lift)


		private[this] val direct = new SQLTypeUnification[Any, Any, Any](Lift.self, Lift.self)

	}




	/** An implicit witness vouching that type `X` is a subtype of `Y` or can be automatically promoted to type `Y`.
	  * It is used to conflate more strict scala types having the same, more loose SQL representation.
	  */
	@implicitNotFound("Type ${X} cannot be used in place of type ${Y} in SQL. Missing implicit Lift[${X}, ${Y}].")
	abstract class Lift[X, Y] {
		def apply(value :X) :Y
		def inverse(value :Y) :Option[X] //todo: we need it for SetComponent, but it may fail or lose precision

		def lower(value :Y) :X = inverse(value) match {
			case Some(x) => x
			case _ => throw new IllegalArgumentException(
				s"Cannot convert $value :${value.localClassName} back with $this."
			)
		}

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](expr :SQLExpression[F, S, X]) :SQLExpression[F, S, Y] =
			expr.to(this)

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](expr :ColumnSQL[F, S, X]) :ColumnSQL[F, S, Y] =
			expr.to(this)

		protected def applyString(arg :String) :String

		override def toString :String = applyString("_")

	}



	object Lift { //todo: java type promotion
		implicit def self[T] :Lift[T, T] = ident.asInstanceOf[Lift[T, T]]
		implicit def option[T] :Lift[T, Option[T]] = opt.asInstanceOf[Lift[T, Option[T]]]
		//implicit def some[T] :Lift[Some[T], Option[T]] = new Supertype[Some[T], Option[T]]
		implicit def singleRow[T] :Lift[Rows[T], T] = selectRow.asInstanceOf[Lift[Rows[T], T]]
		implicit def rowSeq[T] :Lift[Rows[T], Seq[T]] = selectRows.asInstanceOf[Lift[Rows[T], Seq[T]]]

		def apply[X, Y](suffix :String, lift :X => Y, unlift :Y => X) :Lift[X, Y] = new Lift[X, Y] {
			override def apply(value :X) = lift(value)
			override def inverse(value :Y) = Some(unlift(value))
			override def lower(value :Y) = unlift(value)
			protected override def applyString(arg :String) = arg + suffix
		}


		class ComposedLift[X, Y, Z](prev :Lift[X, Y], next :Lift[Y, Z]) extends Lift[X, Z] {

			override def apply(value: X): Z = next(prev(value))

			override def inverse(value: Z): Option[X] = next.inverse(value).flatMap(prev.inverse)

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr: SQLExpression[F, S, X]) :SQLExpression[F, S, Z] =
				next(prev(expr))

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr :ColumnSQL[F, S, X]) :ColumnSQL[F, S, Z] =
				next(prev(expr))

			override def applyString(arg :String) :String = next.applyString(prev.applyString(arg))
		}

		private[this] val ident = new Lift[Any, Any] {
			override def apply(value: Any): Any = value
			override def inverse(value: Any): Option[Any] = Some(value)

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr: SQLExpression[F, S, Any]): SQLExpression[F, S, Any] = expr

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr: ColumnSQL[F, S, Any]) :ColumnSQL[F, S, Any] = expr

			override def applyString(arg :String) = arg
		}

		private[this] val opt = new Lift[Any, Option[Any]] {

			override def apply(value: Any): Option[Any] = Option(value)

			override def inverse(value: Option[Any]): Option[Any] = value

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr: SQLExpression[F, S, Any]): SQLExpression[F, S, Option[Any]] = expr.opt

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr :ColumnSQL[F, S, Any]) :ColumnSQL[F, S, Option[Any]] = expr.opt

			override def applyString(arg :String) :String = "Option[" + arg + "]"
		}

		private[this] val selectRow = new Lift[Rows[Any], Any] {
			override def apply(value: Rows[Any]): Any = value.head
			override def inverse(value: Any): Option[Rows[Any]] = Some(Rows(value))

			override def applyString(arg :String) = arg + ".head"
		}

		private[this] val selectRows = new Lift[Rows[Any], Seq[Any]] {
			override def apply(value: Rows[Any]): Seq[Any] = value.seq

			override def inverse(value: Seq[Any]): Option[Rows[Any]] = value match {
				case Seq(row) => Some(Rows(row))
				case _ => None
			}

			override def applyString(arg :String) = arg + ".toSeq"
		}


	}






	/** A visitor traversing the structure of an SQL expression over tables in `F` and producing a value of `Y[X]`,
	  * where `X` is the value type of the mapped expression. This interface declares a method for every concrete
	  * expression class defined in this package, which is invoked in double dispatch
	  * `apply(expression)`/`expression.applyTo(this)`. This makes for quite a large number of cases, making it
	  * a problem both for maintainers and implementors who may not need to handle every individual type directly.
	  * To alleviate the former, this interface is built by recursively extending partial traits defining methods
	  * for visiting all subclasses of a given expression class. They typically form a one-to-one relationship
	  * with expression classes, with each expression declaring in its companion object (or in the same scope
	  * for inner classes) its own matcher, extending the matchers of its subclasses. To alleviate the latter,
	  * two parallel trait families are introduced: `Case`''Expr'' and `Match`''Expr''. The former implements
	  * all visitor methods for all subclasses of the ''Expr''`SQL`/`SQL`''Expr''/''Expr''`Expression` class
	  * by delegating to the method for the base ''Expr'' class (introduced by the trait if the expression is abstract),
	  * catching all subclasses in a single case. The latter is similar, but doesn't introduce a new method
	  * for abstract expressions and leaves all methods for direct subclasses of ''Expression'' not implemented.
	  * The methods for ''their'' subclasses delegate as in the former example. This in turn has the effect of matching
	  * against a smallest set of classes covering all concrete subclasses of ''Expr''. `ColumnSQL` subtypes
	  * are a bit special, as they have their own hierarchy, parallel to that of plain `SQLExpression`s.
	  * Wherever an expression type, for example `SQLLiteral`, exists in both non-column and column versions,
	  * the non-column ''Expr''`Matcher` will expand the associated column ''Expr''`Matcher`. The `CaseLiteral` trait
	  * in our example will implement the callback for the column literal by delegating it to the method
	  * for the base literal. In this way, the standard delegation chain for any column expression always starts
	  * by delegating to the non-column method. It is however possible to change this by mixing in one or more
	  * of the `Match`''Expr'' or `Case`''Expr'' traits for particular column expression types after the general
	  * matcher traits. For example, `CaseExpression[F, Y] with CaseColumn[F, Y]` will change it
	  * to the opposite: all callbacks for column formulas reach the `column` callback for the root `ColumnSQL`
	  * and, only then, delegate to the `expression` method for `SQLExpression`. An exception to these rules exists
	  * in `MatchComposite`, which includes all methods of `MatchCompositeColumn`. By mixing in a selection
	  * of the above traits, one has a good degree of freedom in selecting which parts of the `SQLExpression` hierarchy
	  * are handled in detail and which are glossed over.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.MatchExpression]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.CaseExpression]]
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher]]
	  */
	trait ExpressionMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnMatcher[F, Y] with CompositeMatcher[F, Y] with MappingMatcher[F, Y]
		   with QueryMatcher[F, Y] with TermMatcher[F, Y]
	{
		def apply[S >: LocalScope <: GlobalScope, V](e: SQLExpression[F, S, V]): Y[S, V] = e.applyTo(this)

		def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) :Y[S, X]

		protected def unhandled(e :SQLExpression[F, _, _]) :Nothing =
			throw new IllegalArgumentException(s"Can't map expression $e :${e.getClass.getName} using $this")

		protected def unknown[E <: SQLExpression[F, _, _]](e :E, clazz :Class[E]) :Nothing =
			throw new IllegalArgumentException(s"Can't map expression $e :${e.getClass.getName} using $this - " +
				                               s"unexpected subclass of ${clazz.getName}")

		protected def unknown[E <: SQLExpression[F, _, _] :ClassTag](e :E) :Nothing =
			throw new IllegalArgumentException(s"Can't map expression $e :${e.getClass.getName} using $this - " +
			                                   s"unexpected subclass of ${implicitly[ClassTag[E]].runtimeClass.getName}")

		override def toString :String = this.localClassName
	}


	/** A `ExpressionMatcher` delegating all visitor calls to the methods specific to one of the direct `SQLExpression`
	  * subclasses: `term`, `composite`, `select`, `mapping`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionMatcher]]
	  */
	trait MatchExpression[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ExpressionMatcher[F, Y] with CaseComposite[F, Y] with CaseMapping[F, Y] with CaseQuery[F, Y]
		   with CaseTerm[F, Y]
	{
		override def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :Y[S, X] =
			expression(e :SQLExpression[F, S, X])
	}

	/** A not particularly useful `ExpressionMatcher` which delegates all the cases to the single `expression` method
	  * invoked for every subexpression (SQL AST node). Used as a base class when only few cases need special handling.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionMatcher]]
	  */
	trait CaseExpression[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ExpressionMatcher[F, Y] with MatchExpression[F, Y]
	{
		override def *(e :ColumnSQL[RowProduct, LocalScope, Nothing]) :Y[LocalScope, Nothing] =
			expression[LocalScope, Nothing](e :SQLExpression[RowProduct, LocalScope, Nothing])

		override def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[LocalScope, V] =
			expression(e :SQLExpression[F, LocalScope, V])

		override def composite[S >: LocalScope <: GlobalScope, X](e: CompositeSQL[F, S, X]): Y[S, X] =
			expression(e)

		override def mapping[S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]]
		                    (e :MappingSQL[F, S, M]) :Y[S, M[()]#Subject] =
			expression(e)

		override def query[V](e :QuerySQL[F, V]) :Y[GlobalScope, Rows[V]] = expression(e)

		override def term[X](e: SQLTerm[X]): Y[GlobalScope, X] = expression(e)
	}

	trait BaseExpressionMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends CaseExpression[F, Y] {
		override def expression[S >: LocalScope <: GlobalScope, X](e: SQLExpression[F, S, X]): Y[S, X] = unhandled(e)
	}


}


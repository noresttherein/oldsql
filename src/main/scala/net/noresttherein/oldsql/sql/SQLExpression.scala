package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.exceptions.InseparableExpressionException
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.Extractor.{Optional, Requisite}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.{slang, OperationType}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.RowProduct.{As, ExactSubselectOf, ExpandedBy, GroundFrom, JoinedMappings, NonEmptyFrom, ParameterizedWith, PartOf, TopFrom}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, GlobalSQL, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.SQLExpression.Lift.ComposedLift
import net.noresttherein.oldsql.sql.StoredProcedure.Out
import net.noresttherein.oldsql.sql.ast.{denullify, AggregateSQL, ChainSQL, ComparisonSQL, CompositeSQL, EqualitySQL, InequalitySQL, IsNull, LooseComponent, MappingSQL, QuerySQL, SelectSQL, SQLNull, SQLParameter, SQLTerm}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.{CaseComposite, CompositeVisitor}
import net.noresttherein.oldsql.sql.ast.ConversionSQL.{MappedSQL, PromotionConversion}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{CaseMapping, MappingVisitor}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{CaseQuery, QueryVisitor, Rows}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.SQLTerm.{CaseTerm, TermVisitor}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.mechanics.{implicitSQLLiterals, SpelledSQL, SQLOrdering, SQLScribe, TableCount}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}

//here be implicits
import slang._






/** A representation of an SQL expression as an abstract syntax tree, providing methods for constructing other
  * expressions mimicking SQL. It is an abstract type used as the root of a wide type hierarchy of classes dedicated
  * to various possible SQL expressions as well as some extensions. It can represent a single, atomic
  * [[net.noresttherein.oldsql.sql.ColumnSQL column]] expression, or multiple columns, including some
  * abstract expressions with flexible numbers of columns. Some subclasses may represent higher-level concepts
  * and be represented by more complex expressions in SQL; for example,
  * [[net.noresttherein.oldsql.sql.JoinedRelation JoinedRelation]] is an expression representing a whole
  * table from a ''select's'' ''from'' clause. Some expressions do not translate to unique final SQL for two reasons:
  *   1. How it is rendered may depend on the context, for example
  *      [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions can include different
  *      columns depending on the SQL statement or clause it is used in;
  *   1. Different DBMS may use different expressions to achieve the same effect (in particular common functions).
  *      Instead, before execution, a root [[net.noresttherein.oldsql.sql.ast.QuerySQL select]] expression
  *      or a containing [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] must be converted into
  *      an [[net.noresttherein.oldsql.sql.Incantation Incantation]] by
  *      a [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]] specific to the used DBMS. By default, this is done
  *      through an intermediate step of constructing a [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]]
  *      with the dialect's [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
  *
  * Every expression can depend on non-constant identifiers: columns of tables from its ''from'' clause
  * (as well as components of the corresponding mappings), columns and components of tables in ''from'' clauses of
  * any ''select'' expressions in which this expression is designed to be embedded, as well as
  * [[net.noresttherein.oldsql.sql.UnboundParam unbound]] query parameters. Their are represented jointly
  * by a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] (typically referred to simply as a ''from'' clause
  * for simplicity) called the ''domain'' of the expression. An expression is ''based'' on a ''from'' clause `F`,
  * if `F` is its domain - a subtype of its first parameter. This class is contravariant in `F`, meaning
  * it allows its usage with any `RowProduct` which uses subtypes of the join types in its type arguments, but,
  * more importantly, which substitutes an abstract prefix of `F`, such as `RowProduct` itself or its more specific
  * variants like `FromClause` and `FromSome`, with any of its subtypes, introducing additional joins and tables
  * to the domain, preceding those from `F`. For this reason, in most circumstances, expressions are based
  * on partially abstract types, containing only the last tables in the ''from'' clause used by the expression.
  * This allows their incremental creation without the knowledge of the full, final query, improving their reuse.
  *
  * An expression can be also easily adapted to ''from'' clauses which add extra tables/parameters to its domain
  * by the use of methods [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]] and
  * [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]] methods - see their documentation for specific
  * requirements.
  *
  * The level of a built-in type checking is a compromise between correctness, manageability and convenience.
  * Full type safety would allow only single-column expressions with an established, non-negotiable translation
  * to SQL distinct types. The following features are supported due to their usefulness (or simply impossibility of
  * their prevention), despite opening the door for throwing an exception at a later time, generating invalid SQL
  * or causing unintended behaviour:
  *   1. Expressions of custom types whose mapping to and from SQL types do not preserve the equality relation -
  *      a common case when two not ''literally'' identical compare equal in Scala because they are functionally
  *      equivalent from the point of view of the domain model (resulting in conditions evaluating differently in Scala
  *      than in the database);
  *   1. Non-strictness regarding the uniqueness of expression [[net.noresttherein.oldsql.schema.SQLForm forms]]:
  *      two expressions with the same value type and thus deemed comparable can use different mapping algorithms,
  *      creating a discrepancy, and in the extreme case errors at executions when a column comparison or assignment
  *      happens between types not automatically converted by the database.
  *   1. Expressions with the same value types but not equal forms - two instances created with the same,
  *      purely functional method will often compare unequal despite being exact clones of each other.
  *      A push for singleton forms would prevent their generic usage, in particular implicit forms assembled based
  *      on the presence of implicit forms for some other, 'lower' type(s). This includes the most common pattern
  *      for creating new forms by mapping another implicit form:
  *      {{{
  *          val figginForm = SQLForm.map(Figgin.apply)(_.length)
  *      }}}
  *   1. Multi-column expressions, especially for composite objects from the business domain such as ''Menu'',
  *      which can potentially have their columns reordered, leading to unexpected behaviours;
  *   1. As a consequence of the above, database tables represented by their
  *      [[net.noresttherein.oldsql.schema.Mapping Mapping]] (or mapped entity) types, rather than
  *      full column lists, which would increase all type names manyfold to the point of being completely unusable;
  *   1. A possibility to include or exclude certain columns from the mapped table on a per query basis -
  *      very useful especially in context of large data types such as `Blob` columns - which, in addition to reasons
  *      stated above, causes particular hurdles in compound ''selects'';
  *   1. Class inheritance, because it can cause lead to a subclass being 'truncated' when upcast to a superclass -
  *      similarly to assigning a stack variable from a pointer or a reference in C++, and connected to the reasons
  *      why Java and Scala equality is flawed.
  * While many of the above situations are easy to prevent, one should keep these issues in mind and remember
  * that an illusion of having arbitrary Scala types in the database comes with its risks.
  *
  * @tparam F a ''from'' clause - list of relations/tables which provide columns used in this expression.
  * @tparam S the ''scope'' defining where in an SQL statement this expression can be used.
  *           [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope local]] scope is used for various aggregate
  *           expressions and means it is restricted to the ''select'' and ''having'' clauses of SQL ''selects''
  *           directly based on the clause `F` (or another ''from'' clause containing all relations present in
  *           the [[net.noresttherein.oldsql.sql.RowProduct.Explicit ''explicit'']] portion of `F`).
  *           [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope Global]] scope means that the expression
  *           can be used anywhere in a ''select'' based on `F` (including the former), as well as in any
  *           of its subselects - after rebasing with [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]].
  * @tparam V result type of the expression; may not necessarily be an SQL type, but a result type of some mapping.
  * @see [[net.noresttherein.oldsql.sql.ColumnSQL]]
  */
trait SQLExpression[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V]
	extends Serializable with implicitSQLLiterals
{
	import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL

	/** Default form for reading of the values of this expression from an SQL [[java.sql.ResultSet ResultSet]]
	  * (if this expression is used as the ''select'' clause of a query). In case of multi column expressions,
	  * this might not represent exactly how it will be rendered in SQL.
	  * Primarily, [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions can vary
	  * the included column set based on the scope (SQL clause) and/or operation the expression is used in,
	  * such as different in an SQL ''insert'' and different in an SQL ''update''. Moreover, when comparing
	  * two expressions or otherwise using them in a context, where their column sets must be equal, in some cases
	  * (such as comparing a component with a value of a parameter), their column sets may be unified.
	  */
	def readForm :SQLReadForm[V]

	/** An SQL expression testing if the value of this expression is `NULL` (by the database when executing the query).
	  * If this expression does not represent a single column, but a tuple/several inline columns, each individual
	  * column is tested for nullity.
	  */
	def isNull :ColumnSQL[F, S, Boolean] = IsNull(this)

	/** An SQL expression comparing the value of this expression with the given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
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
	  * @see [[net.noresttherein.oldsql.sql.ast.SQLParameter]] the expression for ''bound'' SQL parameters.
	  * @see [[net.noresttherein.oldsql.sql.UnboundParam]] an ''unbound'' parameter introduced to the underlying `RowProduct`.
	  */ //we need SQLForm, not SQLReadForm because SQLParameter requires it, in case it's used in the select clause.
	def ==?[X, U](value :X)(implicit lift :SQLTypeUnification[V, X, U], form :SQLForm[X]) :ColumnSQL[F, S, Boolean] =
		this === value.? //todo: try to get rid of the form parameter

	/** An SQL expression comparing the value of this expression with another expression for equality.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]]
	  *         literal, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */ //todo: a type class to avoid this huge overloading repetition with all related methods
	def ===[E <: F, O >: LocalScope <: S, X, U]
	       (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[V, X, U]) :ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => EqualitySQL(lift.left(this), denullify(lift.right(that)))
		}

	/** An SQL expression comparing if this expression equals a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * The argument value will be rendered as a ''literal'' constant in the created
	  * [[java.sql.PreparedStatement PreparedStatement]], so this method should be used only if the built statement
	  * do not vary in this subexpression. Use [[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]] if `that` should
	  * become a [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]] parameter instead.
	  * @param that a scala value of a type which can be promoted to the same type `U` as the value type
	  *             of this expression. It will translate to an SQL literal.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]]
	  */
	def ===[X, U](that :X)(implicit lift :SQLTypeUnification[V, X, U], form :SQLForm[X]) :ColumnSQL[F, S, Boolean] =
		this === SQLTerm(that)

	/** An SQL expression comparing the value of this expression with the value of a given relation component.
	  * The component mapping is converted to an unanchored
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] and must
	  * be [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] before being translated into final SQL.
	  * This however happens automatically for every expression used to create
	  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expression.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
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
	  */ //todo: other comparisons do not have this variant
	def ===[M <: MappingOf[V], E <: F, O <: RowProduct] //E and O are separate as E may be instantiated early from the expected type
	       (component :M)(implicit cast :M <:< RefinedMapping[V, O],
	                               subtype :SQLExpression[O, GlobalScope, V] <:< SQLExpression[E, GlobalScope, V],
	                               project :OriginProjection[M, V], offset :TableCount[O, _ <: Numeral])
			:ColumnSQL[E, S, Boolean] =
		this === subtype(LooseComponent(component))


	/** An SQL expression checking if the value of this expression and another expression are not equal.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def <>[E <: F, O >: LocalScope <: S, X, U]
	      (that :SQLExpression[E, O, X])(implicit lift :SQLTypeUnification[V, X, U]) :ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => InequalitySQL(lift.left(this), denullify(lift.right(that)))
		}

	/** An SQL expression comparing if this expression is unequal to a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that a scala value of a type which can be promoted to the same type `U` as the value type
	  *             of this expression. It will translate to an SQL literal.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def <>[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U]) :ColumnSQL[F, S, Boolean] =
		this <> SQLTerm(that)

	/** An SQL expression comparing the value of this expression with another expression with `<=`.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @param ordering a witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
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
			case _ => ComparisonSQL(lift.left(this), ComparisonSQL.LTE, denullify(lift.right(that)))
		}

	/** An SQL expression comparing if this expression is less than or equal to a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that a scala value of a type which can be promoted to the same type `U` as the value type
	  *             of this expression. It will translate to an SQL literal.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def <=[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this <= SQLTerm(that)

	/** An SQL expression comparing the value of this expression with another expression with `<`.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @param ordering a witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
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
			case _ => ComparisonSQL(lift.left(this), ComparisonSQL.LT, denullify(lift.right(that)))
		}

	/** An SQL expression comparing if this expression is less than a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that a scala value of a type which can be promoted to the same type `U` as the value type
	  *             of this expression. It will translate to an SQL literal.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def <[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this < SQLTerm(that)

	/** An SQL expression comparing the value of this expression with another expression with `>=`.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @param ordering a witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
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
			case _ => ComparisonSQL(lift.left(this), ComparisonSQL.GTE, denullify(lift.right(that)))
		}

	/** An SQL expression comparing if this expression is greater than or equal to a given value.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that a scala value of a type which can be promoted to the same type `U` as the value type
	  *             of this expression. It will translate to an SQL literal.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def >=[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this >= SQLTerm(that)

	/** An SQL expression comparing the value of this expression with another expression with `>`.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that another expression, which value type is comparable in the database with the value type
	  *             of this expression.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @param ordering a witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
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
			case _ => ComparisonSQL(lift.left(this), ComparisonSQL.GT, denullify(lift.right(that)))
		}

	/** An SQL expression comparing if this expression is greater than a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that a scala value of a type which can be promoted to the same type `U` as the value type
	  *             of this expression. It will translate to an SQL literal.
	  * @param lift a witness to the fact that the values of the two expressions can be automatically promoted
	  *             to some unspecified type `U` for the purpose of the comparison by the database.
	  *             Implicit values provided in its companion object depend on the existence of
	  *             [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] evidence for `V -> U` and `X -> U`.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def >[X, U](that :X)(implicit form :SQLForm[X], lift :SQLTypeUnification[V, X, U], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this > SQLTerm(that)



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
	  * This is implemented simply as `this.`[[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[Option[V]]`.
	  */
	def opt :SQLExpression[F, S, Option[V]] = to[Option[V]]

	/** Pass this expression as an ''OUT'' parameter of a stored procedure.
	  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]] wrapping a parameter type in the parameter tuple
	  * type argument of [[net.noresttherein.oldsql.sql.StoredProcedure StoredProcedure]] signifies that its value
	  * can be retrieved by the application after the execution of the procedure. In a static context,
	  * passing an argument expression as `Out[X]` will include `X` in the values returned by created
	  * [[net.noresttherein.oldsql.sql.Call Call]] statements, while passing it simply as `X` will omit the parameter,
	  * treating it as if it was an ''IN'' parameter, declared by the procedure as `X`. The generated SQL is not changed
	  * by this operation. This is equivalent to [[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[Out[V]]`.
	  */
	def out :SQLExpression[F, S, Out[V]] = to[Out[V]]

	def chain :ChainTuple[F, S, @~ ~ V] = EmptyChain ~ this

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


	/** Answers if this expression is applicable to the [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope global]]
	  * scope. If true, it can be safely cast to `SQLExpression[F, GlobalScope, V]`
	  * (and [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL GlobalSQL]]`[F, V]`).
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.asGlobal]]
	  */
	def isGlobal :Boolean = false

	/** Returns this expression in an option if it is a [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL global]]
	  * expression or `None` otherwise.
	  */
	def asGlobal :Option[GlobalSQL[F, V]]

	def groundValue :Opt[V]

	/** True, if this expression does not contain any
	  * [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] subexpression. Loose components
	  * are placeholder expression adapters of [[net.noresttherein.oldsql.schema.Mapping Mapping]] instances
	  * with a `RowProduct` subtype as their `Origin` type. They are not tied to any particular relation in `F`,
	  * which makes converting any enclosing expression into valid SQL impossible.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.anchor]]
	  */
	def isAnchored :Boolean

	/** Replaces all occurrences of [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]
	  * with a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] using the relation at
	  * the given position in `F`, (the first relation in its `RowProduct` type parameter) as its parent.
	  */
	def anchor(from :F) :SQLExpression[F, S, V]

	/** Binds all occurrences of [[net.noresttherein.oldsql.sql.UnboundParam unbound]] parameters to the apprpriate
	  * value extracted from `args`, replacing them with [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]]
	  * parameters. The resulting expression is based on a `RowProduct` resulting from this expression's base by
	  * removing all occurrences of `UnboundParam` pseudo joins.
	  */ //todo: make this a virtual method
	def bind[E <: F { type Params = Args }, Args](base :E, args :Args) :SQLExpression[base.GeneralizedParamless, S, V] =
		SQLScribe.applyParams(base, base.bind(args) :base.GeneralizedParamless)(args)(this)



	/** Creates an SQL ''select'' with this expression as its ''select'' clause. The ''from'' and ''where'' clauses
	  * are defined by this argument, while the ''select'' clause consists of all column sub-expressions constituting
	  * this expression.
	  * This method is supported only by chosen expression types, which have a well defined, unique SQL representation.
	  * These are mainly
	  * [[net.noresttherein.oldsql.sql.ast.TupleSQL TupleSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
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
	  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]] subclasses. It is considered
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
	  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] subclasses. It is considered
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
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  * and all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subclasses.
	  * It is considered low level API exposed only to support potential extension by custom expression types
	  * and should not be used by the client code directly; prefer using
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] and its relatives instead.
	  *
	  * @param from a top-level `RowProduct` instance (i.e., not corresponding to a subselect) conforming to the type
	  *             this expression is based on, that is containing all the relations listed in `F` in its suffix.
	  * @return a `Select` parameterized with the unbound parameters in `from`.
	  * @throws UnsupportedOperationException if this expression cannot be used for a ''select'' clause.
	  */
	def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P }) :Select[P, V] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)


	@throws[InseparableExpressionException]("if the expression cannot be separated into individual columns, " +
	                                        "for example a multi-column SQL select.")
	def split(implicit scope :OperationType) :Seq[ColumnSQL[F, S, _]]


	/** A visitor pattern callback in which this expression calls the method of `mather` most appropriate to its type. */
	protected def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ExpressionVisitor[F, Y]) :Y[S, V]

	protected[sql] def applyToForwarder[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ExpressionVisitor[F, Y]) :Y[S, V] =
		applyTo(visitor)


	/** List of [[net.noresttherein.oldsql.sql.ast.SQLParameter ''bound'']] parameters used by this expression,
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

	protected[sql] final def reverseCollectForwarder[X](fun :PartialFunction[SQLExpression.*, X], acc :List[X]) :List[X] =
		reverseCollect(fun, acc)



//	private[sql] final def spell[P](spelling :SQLSpelling, inline :Boolean = false)(context :SQLContext[P])
//	                               (implicit params :Parameterization[P, F]) :SpelledSQL[P] =
//		if (inline) inlineSpelling(context)(params, spelling) else defaultSpelling(context)(params, spelling)
//
//	protected def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E], inline :Boolean = false)
//	                                        (implicit spelling :SQLSpelling) :SpelledSQL[P, E]

	/** Total number of columns in this expression, after inlining, as would be rendered in SQL in the given
	  * SQL dialect.
	  */
	def columnCount(implicit spelling :SQLSpelling = StandardSQL.spelling) :Int

	/** Translates this expression object into an annotated SQL string `SpelledSQL[P, E]`. This is a fallback method
	  * used by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] if no non-standard representation
	  * is required by the used DBMS. This method should not be exposed, as the clients of this class should
	  * always use the `apply` method of `SQLSpelling` as the SQL formatter.
	  */
	protected def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                        (implicit spelling :SQLSpelling) :SpelledSQL[P, E]

	/** Translates this expression object into an annotated SQL string `SpelledSQL[P, E]`. This is the fallback method
	  * used by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] if no non-standard representation
	  * is required for the used DBMS. This is similar to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]], but multi-column expressions,
	  * such as tuples and mapping components, are not surrounded in a pair of parenthesis (and all their subexpressions
	  * are likewise inlined to the same level).
	  */
	@throws[InseparableExpressionException]("if the expression cannot be separated into individual column strings, " +
	                                        "for example a multi-column SQL select.")
	protected def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]]

	private[sql] final def defaultSpelling[P, E <: F]
	                       (spelling :SQLSpelling)(implicit context :SQLContext, params :Parameterization[P, E])
			:SpelledSQL[P, E] =
		defaultSpelling(context, params)(spelling)

	private[sql] final def inlineSpelling[P, E <: F]
	                       (spelling :SQLSpelling)(implicit context :SQLContext, params :Parameterization[P, E])
			:Seq[SpelledSQL[P, E]] =
		inlineSpelling(context, params)(spelling)


	/** Tests if this expression and the other expression are equal. It functions the same way as `equals`,
	  * but in case of component expressions, only the indices of the relations are compared, rather than the whole
	  * mapping.
	  */
//	def same[E <: F](that :SQLExpression[E, LocalScope, _]) :Boolean

	/** Tests if this expression is equal to the given one abstracting from possibly different sources.
	  * Basically, if both expressions would produce the same SQL they should be isomorphic.
	  */
	def isomorphic(expression :SQLExpression.*) :Boolean

	/** Tests if this expression would produce the same value as the given expression, abstracting from possibly
	  * different clauses. Similar to isomorphic, but generally disregards order of elements in composite expressions
	  * such as 'and', 'or', seq. Be warned that this method's primary use is for tests, and production code should not
	  * depend on it. No guarantees are given and the contract is best-effort only.
	  */
	private[oldsql] def equivalent(expression :SQLExpression.*) :Boolean = isomorphic(expression)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLExpression.*]

	//todo: chunkedString
}






object SQLExpression {

	/** An SQL [[net.noresttherein.oldsql.sql.ast.SQLLiteral literal]] expression of the given value.
	  * This methods requires [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] type class to be present for `T`.
	  * If, additionally, the type class is also [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]],
	  * then the expression will be a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]].
	  */
	@inline def apply[T](literal :T)(implicit factory :SQLTerm.Factory[T]) :factory.Res = factory(literal)


	implicit class ChainSQLExpressionExtension[F <: RowProduct, S >: LocalScope <: GlobalScope, T <: Chain]
	                                          (private val self :SQLExpression[F, S, T])
		extends AnyVal
	{
		/** Expands this chain expression by appending another expression. */
		@inline def ~[O >: LocalScope <: S, H](head :SQLExpression[F, O, H]) :SQLExpression[F, O, T ~ H] =
			ChainSQL(self, head)
	}



	/** Starts building an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] by declaring its
	  * [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters in a sequence of chained calls, starting
	  * with parameter `P`. The returned object supports (by extension methods) additional calls without parameter lists
	  * of [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[N<:Label,P]* [N,P] ]]
	  * (for named parameters) and [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[P]* [P] ]]
	  * (for anonymous parameters), as well as a factory method accepting a constructor function
	  * of [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F]` creating the target expression
	  * based on `F`, where `F` is the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] containing
	  * pseudo joins for all declared parameters. The parameters are available through this argument facade the same
	  * way as in [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method of
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]. Note that expressions created in this way cannot depend
	  * on any tables: the only bottom terms possible are [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]]
	  * and unbound parameters as well as [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]].
	  * This is useful particularly for tuple expressions representing sequences of parameters.
	  * {{{
	  *     SQLExpression.using[String][Int] { params => params.of[String].chained ~ params.of[Int] }
	  * }}}
	  * @return an object implicitly convertible to a
	  *         [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams SQLExpressionParams]]`[FromSome WithParam P, @~ ~ P]`.
	  * @tparam P the type of the first parameter of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
	  *           with the unbound parameters used by the future SQL expression.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams]]
	  */
	def using[P] = new SQLExpressionParamDecl[FromSome, @~, P](From.template)

	/** Starts building an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] by declaring its
	  * [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters in a sequence of chained calls, starting
	  * with parameter `P` of name `N` (where `N` is a `String` literal type). The returned object
	  * supports (by extension methods) additional calls without parameter lists of
	  * [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[N<:Label,P]* [N,P] ]]
	  * (for named parameters) and [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[P]* [P] ]]
	  * (for anonymous parameters), as well as a factory method accepting a constructor function
	  * of [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F]` creating the target expression
	  * based on `F`, where `F` is the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] containing
	  * pseudo joins for all declared parameters.The parameters are available through this argument facade the same
	  * way as in [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method of
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]. Note that expressions created in this way cannot depend
	  * on any tables: the only bottom terms possible are [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]]
	  * and unbound parameters as well as [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]].
	  * This is useful particularly for tuple expressions representing sequences of parameters.
	  * {{{
	  *     SQLExpression.using["damage", Int]["damageType", String] {
	  *         params => params("damage").chained ~ params("damageType")
	  *     }
	  * }}}
	  * @return an object implicitly convertible to a
	  *         [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams SQLExpressionParams]]`[FromSome WithParam P As N, @~ ~ P]`.
	  * @tparam P the type of the first parameter of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
	  *           with the unbound parameters used by the future SQL expression.
	  * @tparam N a `String` literal type with the name of the parameter, which can be used to access it
	  *           from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] for the whole
	  *           domain when creating the SQL expression with the arguments for the procedure.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams]]
	  */
	def using[N <: Label, P] = new SQLExpressionNamedParamDecl[FromSome, @~, N, P](From.template)


	/** An intermediate object adding a new [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P`
	  * to the `F &lt;: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]] domain serving as a base
	  * for a new [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], implicitly convertible to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams SQLExpressionParams]]`[F WithParam P, Ps ~ P]`.
	  * It has the following (extension) methods for adding additional parameters and creating an expression using them:
	  *   1. [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[P]* this]]`[Q]` -
	  *      adds a new parameter of type `Q` to the expression's domain: `F WithParam P WithParam Q`;
	  *   1. [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[N<:Label,P]* this]]`["Q", Q]` - adds
	  *      a new parameter named "Q" and of type `Q` to the expression's domain: `F WithParam P WithParam Q As "Q"`;
	  *   1. [[[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[E<:SQLExpression[F,LocalScope,_]](args:JoinedMappings[F]=>E)* this]]]`(params => `''expr''`)` -
	  *      creates a new expression using a constructor function accepting
	  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P]`.
	  *
	  * This class exists as Scala 2 would not allow chaining of methods with explicit type parameters
	  * but only implicit parameter lists.
	  * @tparam F  a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting ''solely'' of
	  *            [[net.noresttherein.oldsql.sql.WithParam WithParam]] expressions - each possibly with an
	  *            [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause. It is the ''from'' clause on which
	  *            created expressions are based.
	  * @tparam Ps a chain consisting of types of all unbound parameters of `F`, forming the parameters of the
	  *            created SQL expression. Any wildcard prefix type of `F` is ignored and only unbound parameters
	  *            following it are included in `Ps`, which always starts with `@~`.
	  * @tparam P  the type of the new parameter.
	  */
	final class SQLExpressionParamDecl[F <: FromSome, Ps <: Chain, P] private[SQLExpression]
	                                  (private val domain :F ParameterizedWith Ps)

	object SQLExpressionParamDecl {
		implicit def domain[F <: FromSome, Ps <: Chain, P :SQLForm](param :SQLExpressionParamDecl[F, Ps, P])
				:SQLExpressionParams[F WithParam P, Ps ~ P] =
			new SQLExpressionParams(JoinParam(param.domain, ?:[P]))
	}

	/** An intermediate object adding a new [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P`
	  * with alias `N` to the `F &lt;: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]] domain serving as a base
	  * for a new [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], implicitly convertible to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams SQLExpressionParams]]`[F WithParam P, Ps ~ P]`.
	  * It has the following (extension) methods for adding additional parameters and creating an expression using them:
	  *   1. [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[P]* this]]`[Q]` -
	  *      adds a new parameter of type `Q` to the expression's domain: `F WithParam P As N WithParam Q`;
	  *   1. [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[N<:Label,P]* this]]`["Q", Q]` - adds
	  *      a new parameter named "Q" and of type `Q` to the expression's domain:
	  *      `F WithParam P As N WithParam Q As "Q"`;
	  *   1. [[[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[E<:SQLExpression[F,LocalScope,_]](args:JoinedMappings[F]=>E)* this]]]`(params => `''expr''`)` -
	  *      creates a new expression using a constructor function accepting
	  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P As N]`.
	  *
	  * This class exists as Scala 2 would not allow chaining of methods with explicit type parameters
	  * but only implicit parameter lists.
	  * @tparam F  a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting ''solely'' of
	  *            [[net.noresttherein.oldsql.sql.WithParam WithParam]] expressions - each possibly with an
	  *            [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause. It is the ''from'' clause on which
	  *            created expressions are based.
	  * @tparam Ps a chain consisting of types of all unbound parameters of `F`, forming the parameters of the
	  *            created SQL expression. Any wildcard prefix type of `F` is ignored and only unbound parameters
	  *            following it are included in `Ps`, which always starts with `@~`.
	  * @tparam N  a `String` literal with the name of the new parameter.
	  * @tparam P  the type of the new parameter.
	  */
	final class SQLExpressionNamedParamDecl[F <: FromSome, Ps <: Chain, N <: Label, P] private[SQLExpression]
	                                       (private val domain :F ParameterizedWith Ps)

	object SQLExpressionNamedParamDecl {
		implicit def domain[F <: FromSome, Ps <: Chain, N <: Label :ValueOf, P :SQLForm]
		                   (param :SQLExpressionNamedParamDecl[F, Ps, N, P])
				:SQLExpressionParams[F WithParam P As N, Ps ~ P] =
			new SQLExpressionParams(JoinParam(param.domain, ?:[N, P]))
	}

	/** A factory of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] based on `F` and using its
	  * [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters `Ps`, as a function of
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F]`.
	  * It also allows to introduce additional parameters by chaining parameterless calls of
	  * [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[P]* this]]`[P]` and
	  * [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[N<:Label,P]* this]]`[N, P]`
	  * behaving identically to the counterpart methods `using` of the `SQLExpression` object.
	  * @tparam F  a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting ''solely'' of
	  *            [[net.noresttherein.oldsql.sql.WithParam WithParam]] expressions - each possibly with an
	  *            [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause. It is the ''from'' clause on which
	  *            created expressions are based.
	  * @tparam Ps a chain consisting of types of all unbound parameters of `F`, forming the parameters of the
	  *            created SQL expression. Any wildcard prefix type of `F` is ignored and only unbound parameters
	  *            following it are included in `Ps`, which always starts with `@~`.
	  */
	class SQLExpressionParams[F <: FromSome, Ps <: Chain](domain :F ParameterizedWith Ps) {
		/** Add another [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P` to the parameters
		  * of the created [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
		  * @return an object implicitly convertible to a
		  *         [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams SQLExpressionParams]]`[F WithParam P, Ps ~ P]`.
		  * @tparam P the type of the - currently last - parameter
		  *           of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] with the unbound parameters
		  *           used by the future SQL expression.
		  */
		def apply[P] :SQLExpressionParamDecl[F, Ps, P] = new SQLExpressionParamDecl[F, Ps, P](domain)

		/** Add another [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P` named `N` to the parameters
		  * of the created [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
		  * @return an object implicitly convertible to a
		  *         [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams SQLExpressionParams]]`[F WithParam P As N, Ps ~ P]`.
		  * @tparam P the type of the - currently last - parameter
		  *           of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] with the unbound parameters
		  *           used by the future SQL expression.
		  * @tparam N a `String` literal type with the name of the parameter, which can be used to access it
		  *           from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] for the whole
		  *           domain when creating the SQL expression with the arguments for the procedure.
		  */
		def apply[N <: Label, P] :SQLExpressionNamedParamDecl[F, Ps, N, P] =
			new SQLExpressionNamedParamDecl[F, Ps, N, P](domain)

		/** Creates an SQL expression of any type, using the unbound parameters accessible as pseudo relations
		  * from the facade to the domain `F` with all previously declared unbound parameters.
		  */
		def apply[E <: SQLExpression[F, LocalScope, _]](args :JoinedMappings[F] => E) :E =
			args(new JoinedMappings(domain))
	}




	/** Phantom type used to denote where an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] parameterized
	  * with it can be used. It is an alias for [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope GlobalScope]]
	  * which specifies that the expression can be used both in all SQL ''selects'' based on
	  * the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] type it is parameterized with it, as well
	  * as any of its subselects (dependent selects) by expanding it with method
	  * [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]] (and similar). Its subtype,
	  * [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope LocalScope]], marks expressions which can be used
	  * only in the 'same' ''select'' - that is the one based on the `RowProduct` type it is parameterized with,
	  * as well as on any ''from'' clauses expanding that type without crossing
	  * a [[net.noresttherein.oldsql.sql.Subselect Subselect]] boundary, by method
	  * [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]] (and its overloads).
	  */
	type ExpressionScope = GlobalScope

	/** Default scope of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], signifying that it can be used
	  * solely within the ''select'' and ''having'' clauses for the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
	  * serving as the domain of the expression. Such expressions are illegal for subselects of the mentioned ''select'',
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
	type LocalScope <: GlobalScope //consider: renaming to Aggregate/Group/GroupScope

	/** A phantom type used as the ''scope'' type argument `S`
	  * of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] instances which do not contain any SQL aggregate
	  * functions as their subexpressions, signifying that they can be converted
	  * to any [[net.noresttherein.oldsql.sql.RowProduct ''from'']] clause `E` expanding the clause `F`
	  * on which the expression is based, in particular within subselect expressions of the SQL ''select'' containing it.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PartOf]]
	  */
	type GlobalScope //consider: renaming to Discrete/RowScope

	private[oldsql] val GlobalScope = implicitly[GlobalScope <:< GlobalScope]

	/** An upper bound of all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`.
	  * It restricts the application scope of the expression to the ''select'' and ''having'' clauses
	  * of an SQL ''select'' based on the clause `F`. It is not allowed in the ''where'' clause
	  * or within subselects of the select it is based on. This allows it to include aggregate expressions
	  * such as `count(*)` as its subtypes.
	  * @see [[net
*      .noresttherein.oldsql.sql.SQLExpression.LocalScope]] @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.AggregateSQL]]
	  */ //todo: move it to package sql; rename to AggregateSQL/GroupSQL
	type LocalSQL[-F <: RowProduct, V] = SQLExpression[F, LocalScope, V]

	/** An upper bound of all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`
	  * which can be used freely in the context of any SQL ''select'' based on `F` as well as any of its subselects.
	  * Most expression types derive from this type rather than
	  * its supertype [[net.noresttherein.oldsql.sql.SQLExpression.LocalSQL LocalSQL]], with the sole exception being
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.LocalSQL]]
	  */ //todo: move to package sql; rename to DiscreteSQL/RowSQL
	type GlobalSQL[-F <: RowProduct, V] = SQLExpression[F, GlobalScope, V]

	/** A type alias for [[net.noresttherein.oldsql.sql.SQLExpression SQL expressions]] independent of any relations
	  * in the FROM clause, that is applicable to any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */ //todo: rename to GroundSQL/TopSQL
	type GroundSQL[T] = SQLExpression[RowProduct, GlobalScope, T]


	/** An upper type bound of all `SQLExpression[_, _, _]` instances, including proper bounds of the type parameters
	  * in its definition. Most subclasses of `SQLExpression` define their own `*` types, which are particularly
	  * useful for heavily parameterized types as well as those parameterized with type constructors (needing
	  * existential types).
	  */
	type * = SQLExpression[_ <: RowProduct, _ >: LocalScope <: GlobalScope, _]


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
	@implicitNotFound("Types ${L} and ${R} are not interoperable in SQL (as type ${U}). " +
	                  "Missing implicit SQLTypeUnification[${L}, ${R}, ${U}] (required Lift[${L}, ${R}] or Lift[${R}, ${L}]).")
	class SQLTypeUnification[L, R, U](val left :Lift[L, U], val right :Lift[R, U]) extends Serializable {
		def swapped :SQLTypeUnification[R, L, U] = new SQLTypeUnification(right, left)
		override def toString = s"$left =~= $right"

		/** Converts the value of the left side to the value of the right side, if possible. */
		def l2r(l :L) :Option[R] = right.inverse(left(l))

		/** Converts the value of the right side to the value of the left side, if possible. */
		def r2l(r :R) :Option[L] = left.inverse(right(r))
	}



	sealed abstract class Level2SQLTypeUnificationImplicits {
		implicit def mirror[L, R, T](implicit promotion :SQLTypeUnification[L, R, T]) :SQLTypeUnification[R, L, T] =
			promotion.swapped
	}
	sealed abstract class Level1SQLTypeUnificationImplicits {
		import Lift._
		implicit def liftLeft[L, R](implicit lift :Lift[L, R]) :SQLTypeUnification[L, R, R] =
			new SQLTypeUnification(lift, self)

		implicit def liftRight[L, R](implicit lift :Lift[R, L]) :SQLTypeUnification[L, R, L] =
			new SQLTypeUnification(self, lift)
	}
	object SQLTypeUnification extends Level1SQLTypeUnificationImplicits {

		implicit def directly[T] :SQLTypeUnification[T, T, T] =
			direct.asInstanceOf[SQLTypeUnification[T, T, T]]

		private[this] val direct = new SQLTypeUnification[Any, Any, Any](Lift.self, Lift.self)
	}




	/** An implicit witness vouching that type `X` is a subtype of `Y` or can be automatically promoted to type `Y`.
	  * It is used to convert types of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] without affecting
	  * the SQL output, conflating more strict scala types having the same, more loose SQL representation
	  * (or automatically converted in the database).
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.to]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification]]
	  */
	@implicitNotFound("Type ${X} cannot be used in place of type ${Y} in SQL. Missing implicit Lift[${X}, ${Y}].")
	abstract class Lift[X, Y] extends Serializable {
		def apply(value :X) :Y
		def inverse(value :Y) :Option[X] //todo: we need it for SetComponent, but it may fail or lose precision

		def lower(value :Y) :X = inverse(value) match {
			case Some(x) => x
			case _ => throw new IllegalArgumentException(
				s"Cannot convert $value :${value.localClassName} back with $this."
			)
		}
		def apply(form :SQLReadForm[X]) :SQLReadForm[Y] = form.nullMap(apply)
		def apply(form :ColumnReadForm[X]) :ColumnReadForm[Y] = form.nullMap(apply)
		def apply(form :SQLForm[X]) :SQLForm[Y] = form.nullAs(Requisite(apply(_:X)))(Optional(inverse))
		def apply(form :ColumnForm[X]) :ColumnForm[Y] = form.nullAs(Requisite(apply))(Optional(inverse))

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](expr :SQLExpression[F, S, X]) :SQLExpression[F, S, Y] =
			if (expr == null) null else expr.to(this)

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](expr :ColumnSQL[F, S, X]) :ColumnSQL[F, S, Y] =
			if (expr == null) null else expr.to(this)


		def compose[W](first :Lift[W, X]) :Lift[W, Y] =
			if (first == Lift.self[X]) first andThen this
			else new ComposedLift[W, X, Y](first, this)

		def andThen[Z](second :Lift[Y, Z]) :Lift[X, Z] =
			if (second == Lift.self[Y]) second compose this
			else new ComposedLift[X, Y, Z](this, second)

		protected def applyString(arg :String) :String

		override def toString :String = applyString("_")

	}



	object Lift {
		implicit def self[T] :Lift[T, T] = ident.asInstanceOf[Lift[T, T]]
		implicit def option[T] :Lift[T, Option[T]] = opt.asInstanceOf[Lift[T, Option[T]]]
		//implicit def some[T] :Lift[Some[T], Option[T]] = new Supertype[Some[T], Option[T]]
		implicit def out[T] :Lift[T, Out[T]] = outParam.asInstanceOf[Lift[T, Out[T]]]
		implicit def param[T] :Lift[Out[T], T] = inParam.asInstanceOf[Lift[Out[T], T]]
		implicit def singleRow[T] :Lift[Rows[T], T] = selectRow.asInstanceOf[Lift[Rows[T], T]]
		implicit def rowSeq[T] :Lift[Rows[T], Seq[T]] = selectRows.asInstanceOf[Lift[Rows[T], Seq[T]]]

		def apply[X, Y](suffix :String, lift :X => Y, unlift :Y => X) :Lift[X, Y] = new Lift[X, Y] {
			override def apply(value :X) = lift(value)
			override def inverse(value :Y) = Some(unlift(value))
			override def lower(value :Y) = unlift(value)
			protected override def applyString(arg :String) = arg + suffix
		}

		trait BidirectionalLift[X, Y] extends Lift[X, Y] {
			override def apply(form :SQLForm[X]) :SQLForm[Y] = form.nullBimap(apply)(lower)
			override def apply(form :ColumnForm[X]) :ColumnForm[Y] = form.nullBimap(apply)(lower)
			override def inverse(value :Y) :Option[X] = Some(lower(value))
		}

		implicit object Byte2Short extends BidirectionalLift[Byte, Short] {
			override def apply(value :Byte) :Short = value
			override def lower(value :Short) :Byte = value.toByte
			protected override def applyString(arg :String) :String = arg + ".toShort"
		}
		implicit object Byte2Int extends BidirectionalLift[Byte, Int] {
			override def apply(value :Byte) :Int = value
			override def lower(value :Int) :Byte = value.toByte
			protected override def applyString(arg :String) :String = arg + ".toInt"
		}
		implicit object Byte2Long extends BidirectionalLift[Byte, Long] {
			override def apply(value :Byte) :Long = value
			override def lower(value :Long) :Byte = value.toByte
			protected override def applyString(arg :String) :String = arg + ".toLong"
		}
		implicit object Byte2Float extends BidirectionalLift[Byte, Float] {
			override def apply(value :Byte) :Float = value
			override def lower(value :Float) :Byte = value.toByte
			protected override def applyString(arg :String) :String = arg + ".toFloat"
		}
		implicit object Byte2Double extends BidirectionalLift[Byte, Double] {
			override def apply(value :Byte) :Double = value
			override def inverse(value :Double) :Option[Byte] = Some(value.toByte)
			override def lower(value :Double) :Byte = value.toByte
			protected override def applyString(arg :String) :String = arg + ".toDouble"
		}

		implicit object Short2Int extends BidirectionalLift[Short, Int] {
			override def apply(value :Short) :Int = value
			override def lower(value :Int) :Short = value.toShort
			protected override def applyString(arg :String) :String = arg + ".toInt"
		}
		implicit object Short2Long extends BidirectionalLift[Short, Long] {
			override def apply(value :Short) :Long = value
			override def lower(value :Long) :Short = value.toShort
			protected override def applyString(arg :String) :String = arg + ".toLong"
		}
		implicit object Short2Float extends BidirectionalLift[Short, Float] {
			override def apply(value :Short) :Float = value
			override def lower(value :Float) :Short = value.toShort
			protected override def applyString(arg :String) :String = arg + ".toFloat"
		}
		implicit object Short2Double extends BidirectionalLift[Short, Double] {
			override def apply(value :Short) :Double = value
			override def lower(value :Double) :Short = value.toShort
			protected override def applyString(arg :String) :String = arg + ".toDouble"
		}

		implicit object Int2Long extends BidirectionalLift[Int, Long] {
			override def apply(value :Int) :Long = value
			override def lower(value :Long) :Int = value.toInt
			protected override def applyString(arg :String) :String = arg + ".toLong"
		}
		implicit object Int2Double extends BidirectionalLift[Int, Double] {
			override def apply(value :Int) :Double = value
			override def lower(value :Double) :Int = value.toInt
			protected override def applyString(arg :String) :String = arg + ".toDouble"
		}

		implicit object Char2Int extends BidirectionalLift[Char, Int] {
			override def apply(value :Char) :Int = value
			override def lower(value :Int) :Char = value.toChar
			protected override def applyString(arg :String) :String = arg + ".toInt"
		}

		implicit object FloatToDouble extends BidirectionalLift[Float, Double] {
			override def apply(value :Float) :Double = value
			override def lower(value :Double) :Float = value.toFloat
			protected override def applyString(arg :String) :String = arg + ".toFloat"
		}



		class ComposedLift[X, Y, Z](prev :Lift[X, Y], next :Lift[Y, Z]) extends Lift[X, Z] {
			override def apply(value: X): Z = next(prev(value))
			override def inverse(value: Z): Option[X] = next.inverse(value).flatMap(prev.inverse)

			override def apply(form :SQLReadForm[X]) :SQLReadForm[Z] = next(prev(form))
			override def apply(form :ColumnReadForm[X]) :ColumnReadForm[Z] = next(prev(form))
			override def apply(form :SQLForm[X]) :SQLForm[Z] = next(prev(form))
			override def apply(form :ColumnForm[X]) :ColumnForm[Z] = next(prev(form))

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr: SQLExpression[F, S, X]) :SQLExpression[F, S, Z] =
				next(prev(expr))

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr :ColumnSQL[F, S, X]) :ColumnSQL[F, S, Z] =
				next(prev(expr))

			override def applyString(arg :String) :String = next.applyString(prev.applyString(arg))
		}

		private[this] val ident = new BidirectionalLift[Any, Any] {
			override def apply(value: Any): Any = value
			override def lower(value :Any) = value

			override def apply(form :SQLReadForm[Any]) :SQLReadForm[Any] = form
			override def apply(form :ColumnReadForm[Any]) :ColumnReadForm[Any] = form
			override def apply(form :SQLForm[Any]) :SQLForm[Any] = form
			override def apply(form :ColumnForm[Any]) :ColumnForm[Any] = form

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr: SQLExpression[F, S, Any]): SQLExpression[F, S, Any] = expr

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr: ColumnSQL[F, S, Any]) :ColumnSQL[F, S, Any] = expr

			override def compose[W](first :Lift[W, Any]) = first
			override def andThen[Z](second :Lift[Any, Z]) = second

			override def applyString(arg :String) = arg
		}

		private[this] val opt = new Lift[Any, Option[Any]] {
			override def apply(value: Any): Option[Any] = Option(value)
			override def inverse(value: Option[Any]): Option[Any] = value

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr: SQLExpression[F, S, Any]): SQLExpression[F, S, Option[Any]] =
				if (expr == null) null else expr.opt

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (expr :ColumnSQL[F, S, Any]) :ColumnSQL[F, S, Option[Any]] =
				if (expr == null) null else expr.opt

			override def applyString(arg :String) :String = "Option[" + arg + "]"
		}

		private[this] val selectRow = new BidirectionalLift[Rows[Any], Any] {
			override def apply(value: Rows[Any]): Any = value.head
			override def lower(value: Any): Rows[Any] = Rows(value)
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

		private[this] val outParam = new BidirectionalLift[Any, Out[Any]] {
			override def apply(value: Any): Out[Any] = Out(value)
			override def lower(value :Out[Any]) = value.param
			override def applyString(arg :String) = "Out[" + arg + "]"
		}

		private[this] val inParam = new BidirectionalLift[Out[Any], Any] {
			override def apply(value :Out[Any]) :Any = value.param
			override def lower(value :Any) = Out(value)
			override def applyString(arg :String) :String = arg + ".param"
		}

	}






	/** A generic function translating SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] to values of
	  * some type `Y[S, V]` dependant on the expression's value type. Most notably, it is implemented
	  * by the visitor interface [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionVisitor ExpressionVisitor]] -
	  * this trait was extracted to allow hiding the multitude of visitor methods from public interfaces
	  * of implementing classes, if needed.
 	  * @tparam F the `RowProduct` subtype on which all translated expressions are based.
	  * @tparam Y the type constructor of the return type, accepting the expression's
	  *           [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope scope]] and value types.
	  */
	trait ExpressionMapper[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def apply[S >: LocalScope <: GlobalScope, V](e: SQLExpression[F, S, V]): Y[S, V]
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
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor]]
	  */ //todo: rename to visitor
	trait ExpressionVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ExpressionMapper[F, Y] with ColumnVisitor[F, Y] with CompositeVisitor[F, Y] with MappingVisitor[F, Y]
		   with QueryVisitor[F, Y] with TermVisitor[F, Y]
	{
		override def apply[S >: LocalScope <: GlobalScope, V](e: SQLExpression[F, S, V]): Y[S, V] =
			e.applyTo(this)

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


	/** A `ExpressionVisitor` delegating all visitor calls to the methods specific to one of the direct `SQLExpression`
	  * subclasses: `term`, `composite`, `select`, `mapping`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionVisitor]]
	  */
	trait MatchExpression[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ExpressionVisitor[F, Y] with CaseComposite[F, Y] with CaseMapping[F, Y] with CaseQuery[F, Y]
		   with CaseTerm[F, Y]
	{
		override def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :Y[S, X] =
			expression(e :SQLExpression[F, S, X])
	}

	/** A not particularly useful `ExpressionVisitor` which delegates all the cases to the single `expression` method
	  * invoked for every subexpression (SQL AST node). Used as a base class when only few cases need special handling.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionVisitor]]
	  */
	trait CaseExpression[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ExpressionVisitor[F, Y] with MatchExpression[F, Y]
	{
		override def *(e :ColumnSQL[RowProduct, LocalScope, Nothing]) :Y[LocalScope, Nothing] =
			expression[LocalScope, Nothing](e :SQLExpression[RowProduct, LocalScope, Nothing])

		override def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[LocalScope, V] =
			expression(e :SQLExpression[F, LocalScope, V])

		override def composite[S >: LocalScope <: GlobalScope, X](e: CompositeSQL[F, S, X]): Y[S, X] =
			expression(e)

		override def mapping[S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]]
		                    (e :MappingSQL[F, S, M]) :Y[S, M[Unit]#Subject] =
			expression(e)

		override def query[V](e :QuerySQL[F, V]) :Y[GlobalScope, Rows[V]] = expression(e)

		override def term[X](e: SQLTerm[X]): Y[GlobalScope, X] = expression(e)
	}

	trait BaseExpressionVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends CaseExpression[F, Y] {
		override def expression[S >: LocalScope <: GlobalScope, X](e: SQLExpression[F, S, X]): Y[S, X] = unhandled(e)
	}


}


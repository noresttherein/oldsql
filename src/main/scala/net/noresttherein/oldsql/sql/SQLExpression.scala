package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound
import scala.reflect.{classTag, ClassTag}

import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.collection.{Chain, Listing, Opt}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, InseparableExpressionException, InvalidSQLException, MisalignedExpressionException, MismatchedExpressionsException, OldSQLException, UndefinedShapeException}
import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.ColumnAt
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{As, Complete, ExpandedBy, GroundRow, JoinedMappings, NonEmptyRow, NoParams, ParamsRow, PartOf, SubselectOf, TopRow}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertingTemplate, Grouped, Single, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.StoredProcedure.Out
import net.noresttherein.oldsql.sql.ast.{denullify, AdaptedSQL, AggregateSQL, ChainSQL, ChainTuple, ColumnLValueSQL, ColumnMappingSQL, ComparisonSQL, CompositeSQL, ConvertedSQL, EqualitySQL, HasNulls, HasRowShape, InequalitySQL, IsNull, LabeledSQL, LooseComponent, LValueSQL, MappingSQL, NullEqualitySQL, NullInequalitySQL, QuerySQL, SelectableSQL, SelectSQL, SeqSQL, SQLNull, SQLTerm}
import net.noresttherein.oldsql.sql.ast.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL.{CaseAnyComponent, CaseSpecificComponent}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.{AnyCompositeVisitor, CaseAnyComposite, CaseSpecificComposite, SpecificCompositeVisitor}
import net.noresttherein.oldsql.sql.ast.LabeledSQL.EmptyListing
import net.noresttherein.oldsql.sql.ast.LooseComponent.{CaseAnyLooseComponent, CaseSpecificLooseComponent}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{AnyMappingVisitor, SpecificMappingVisitor}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{AnyQueryVisitor, MatchAnyQuery, MatchSpecificQuery, Rows, SpecificQueryVisitor}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.SQLTerm.{AnyTermVisitor, CaseAnyTerm, CaseSpecificTerm, SpecificTermVisitor}
import net.noresttherein.oldsql.sql.mechanics.{implicitSQLLiterals, CanSelect, Interoperable, Reform, RelationCount, SpelledSQL, SQLAdaptation, SQLConversion, SQLOrdering, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.CanSelect.{CanSelectDef, CanSelectDirect}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}

//here be implicits
import slang._





/** A representation of an SQL expression as an abstract syntax tree, providing methods for constructing other
  * expressions mimicking SQL. It is an abstract type used as the root of a wide type hierarchy of classes dedicated
  * to various possible SQL expressions as well as some extensions. It can represent a single, atomic
  * [[net.noresttherein.oldsql.sql.ColumnSQL column]] expression, or multiple columns, including some
  * abstract expressions with flexible numbers of columns. Some subclasses may represent higher-level concepts
  * and be represented by more complex expressions in SQL; for example,
  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] is an expression representing a whole
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
  * [[net.noresttherein.oldsql.sql.ParamClause unbound]] query parameters. Their are represented jointly
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
  * by the use of methods [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.expand expand]] and
  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.basedOn basedOn]] methods -
  * see their documentation for specific requirements.
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
  *      full column lists, which would increase all type names manifold to the point of being completely unusable;
  *   1. A possibility to include or exclude certain columns from the mapped table on a per query basis -
  *      very useful especially in context of large data types such as `Blob` columns - which, in addition to reasons
  *      stated above, causes particular hurdles in compound ''selects'';
  *   1. Class inheritance, because it can cause lead to a subclass being 'truncated' when upcast to a superclass -
  *      similarly to assigning a stack variable from a pointer or a reference in C++, and connected to the reasons
  *      why Java and Scala equality is flawed.
  * While many of the above situations are easy to prevent, one should keep these issues in mind and remember
  * that an illusion of having arbitrary Scala types in the database comes with its risks.
  * @tparam F $F
  * @tparam S $S
  * @tparam V $V
  * @see [[net.noresttherein.oldsql.sql.ColumnSQL]]
  * @define Thing `SQLExpression`
  * @define thing expression
  */ //todo: rename all `from` arguments to `clause`; rename 'base of', 'based on', to 'domain of', 'grounded in'
trait SQLExpression[-F <: RowProduct, -S >: Grouped <: Single, V]
	extends ConvertingTemplate[F, S, V, SQLExpression.from[F]#rows[S]#E]
	   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = SQLExpression[f, S, V] })#E]
	   with HasRowShape with Serializable with implicitSQLLiterals
{
	import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL

	/** Default form for reading of the values of this expression from an SQL [[java.sql.ResultSet ResultSet]]
	  * (if this expression is used as the ''select'' clause of a query). In case of multi column expressions,
	  * this might not represent exactly how it will be rendered in SQL.
	  * Primarily, this represents the usage of the expression in a query, rather than other DML statements:
	  * the same instance can be formatted differently when used in different scopes (SQL clauses), for example
	  * as an assigned value in an SQL ''insert'' or ''update'', or even in the ''group by'' clause.
	  *
	  * However, even in the scope of a ''select'' clause, the column set of the final SQL may be inconsistent
	  * with this form because [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions are semantically
	  * placeholders for components from the actual ''from'' clause of the formatted query/DML,
	  * with their [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
	  * and [[net.noresttherein.oldsql.schema.Relation Relation]] being substituted before formatting for instances
	  * at the same positions in the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] instance
	  * passed to [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]] as the `from` argument.
	  * Discrepancies most commonly occur as a result
	  * of [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.alter altering]] the source table.
	  * Moreover, when comparing two expressions or otherwise using them in a context, where their column sets
	  * must be equal, in some cases (such as comparing a component with a value of a parameter), their column sets
	  * may be adapted to fit the form of another expression.
	  */
	//todo: consider migrating it to SQLForm. This would allow literals/params to always be reformed,
	// give access to 'null literals', not require forms when comparing with literals, etc.
	// problems: form for what? filter? insert? update? how will we upcast it, which currently is relatively safe?
	// ConversionSQL is lossy (obviously), ComponentSQL may not even allow the same columns, function calls
	def selectForm :SQLReadForm[V]

	/** An SQL expression testing if the value of this expression is `NULL` (by the database when executing the query).
	  * If this expression does not represent a single column, but a tuple/several inline columns, each individual
	  * column is tested for nullity.
	  */
	def isNull :ColumnSQL[F, S, Boolean] = IsNull(this)

	/** Same as [[net.noresttherein.oldsql.sql.SQLExpression.isNull isNull]], but multi column expressions
	  * check if ''any'' of their columns are null, not ''all'' of them.
	  */
	def hasNulls :ColumnSQL[F, S, Boolean] = HasNulls(this)

	/** An SQL expression comparing the value of this expression with the given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * The argument value will be rendered as a parameter of the created [[java.sql.PreparedStatement PreparedStatement]],
	  * but must be nevertheless known at this place.
	  * @param value A value of any type `X` such that `V` and `X` can be automatically promoted by the database
	  *              to the same type `U` for the purpose of the comparison, and for which an implicit
	  *              [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists.
	  * @param unify A witness to the fact that the values of the two expressions can be automatically promoted
	  *              to some unspecified type `U` for the purpose of the comparison by the database.
	  *              Implicit values provided in its companion object depend on the existence of
	  *              [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]]
	  *              evidence for `V -> U` and `X -> U`.
	  * @param form  An [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] used to set the value of the parameter
	  *              in the `PreparedStatement` using this expression.
	  * @see [[net.noresttherein.oldsql.sql.ast.BoundParam]] the expression for ''bound'' SQL parameters.
	  * @see [[net.noresttherein.oldsql.sql.ParamClause]] an ''unbound'' parameter introduced to the underlying `RowProduct`.
	  */ //todo: a type class to avoid this huge overloading repetition with all related methods
	def ==?[X](value :X)(implicit unify :Interoperable[V, X], form :SQLForm[X]) :ColumnSQL[F, S, Boolean] =
		this === value.? //todo: try to get rid of the form parameter

	//consider: if we get rid of the null case and return EqualitySQL, then we can have an implicit conversion
	// from EqualitySQL to Boolean implemented as left equals right. Then maybe renaming this method to ==
	// wouldn't be a bad idea.
	/** An SQL expression comparing the value of this expression with another expression for equality.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that  Another expression, which value type is comparable in the database with the value type
	  *              of this expression.
	  * @param unify A witness to the fact that the values of the two expressions can be automatically promoted
	  *              to some unspecified type `U` for the purpose of the comparison by the database.
	  *              Implicit values provided in its companion object depend on the existence of
	  *              [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *              for `V -> U` and `X -> U`.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]]
	  *         literal, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def ===[E <: F, O >: Grouped <: S, X]
	       (that :SQLExpression[E, O, X])(implicit unify :Interoperable[V, X]) :ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => EqualitySQL(unify.left(this), denullify(unify.right(that)))
		}

	/** An SQL expression comparing if this expression equals a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * The argument value will be rendered as a ''literal'' constant in the created
	  * [[java.sql.PreparedStatement PreparedStatement]], so this method should be used only if the built statement
	  * do not vary in this subexpression. Use [[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]] if `that` should
	  * become a [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameter instead.
	  * @param that   a Scala value of a type which can be promoted to the same type `U` as the value type
	  *               of this expression. It will translate to an SQL literal.
	  * @param unify a witness to the fact that the values of the two expressions can be automatically promoted
	  *               to some unspecified type `U` for the purpose of the comparison by the database.
	  *               Implicit values provided in its companion object depend on the existence of
	  *               [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *               for `V -> U` and `X -> U`.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]]
	  */
	def ===[X](that :X)(implicit unify :Interoperable[V, X], form :SQLForm[X]) :ColumnSQL[F, S, Boolean] =
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
	  *                  its [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form. In other words,
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
	       (component :M)(implicit cast :M <:< TypedMapping[V, O],
	                      subtype :SQLExpression[O, Single, V] <:< SQLExpression[E, Single, V],
	                      project :OriginProjection[M, V], offset :RelationCount[O, _ <: Numeral])
			:ColumnSQL[E, S, Boolean] =
		this === subtype(LooseComponent(component))


	/** An SQL expression checking if the value of this expression and another expression are not equal.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that  Another expression, which value type is comparable in the database with the value type
	  *              of this expression.
	  * @param unify A witness to the fact that the values of the two expressions can be automatically promoted
	  *              to some unspecified type `U` for the purpose of the comparison by the database.
	  *              Implicit values provided in its companion object depend on the existence of
	  *              [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *              for `V -> U` and `X -> U`.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def <>[E <: F, O >: Grouped <: S, X]
	      (that :SQLExpression[E, O, X])(implicit unify :Interoperable[V, X]) :ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => InequalitySQL(unify.left(this), denullify(unify.right(that)))
		}

	/** An SQL expression comparing if this expression is unequal to a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that  A Scala value of a type which can be promoted to the same type `U` as the value type
	  *              of this expression. It will translate to an SQL literal.
	  * @param unify A witness to the fact that the values of the two expressions can be automatically promoted
	  *              to some unspecified type `U` for the purpose of the comparison by the database.
	  *              Implicit values provided in its companion object depend on the existence of
	  *              [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *              for `V -> U` and `X -> U`.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def <>[X](that :X)(implicit unify :Interoperable[V, X], form :SQLForm[X]) :ColumnSQL[F, S, Boolean] =
		this <> SQLTerm(that)

	/** An SQL expression comparing the value of this expression with another expression with `<=`.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that     Another expression, which value type is comparable in the database with the value type
	  *                 of this expression.
	  * @param unify    A witness to the fact that the values of the two expressions can be automatically promoted
	  *                 to some unspecified type `U` for the purpose of the comparison by the database.
	  *                 Implicit values provided in its companion object depend on the existence of
	  *                 [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *                 for `V -> U` and `X -> U`.
	  * @param ordering A witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def <=[E <: F, O >: Grouped <: S, X, U]
	      (that :SQLExpression[E, O, X])
	      (implicit unify :Interoperable[V, X] { type Unified = U }, ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => ComparisonSQL(unify.left(this), ComparisonSQL.LTE, denullify(unify.right(that)))
		}

	/** An SQL expression comparing if this expression is less than or equal to a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that     A Scala value of a type which can be promoted to the same type `U` as the value type
	  *                 of this expression. It will translate to an SQL literal.
	  * @param unify    A witness to the fact that the values of the two expressions can be automatically promoted
	  *                 to some unspecified type `U` for the purpose of the comparison by the database.
	  *                 Implicit values provided in its companion object depend on the existence of
	  *                 [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *                 for `V -> U` and `X -> U`.
	  * @param ordering A witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def <=[X, U](that :X)
	            (implicit unify :Interoperable[V, X] { type Unified = U }, form :SQLForm[X], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this <= SQLTerm(that)

	/** An SQL expression comparing the value of this expression with another expression with `<`.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that     Another expression, which value type is comparable in the database with the value type
	  *                 of this expression.
	  * @param unify    A witness to the fact that the values of the two expressions can be automatically promoted
	  *                 to some unspecified type `U` for the purpose of the comparison by the database.
	  *                 Implicit values provided in its companion object depend on the existence of
	  *                 [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *                 for `V -> U` and `X -> U`.
	  * @param ordering A witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def <[E <: F, O >: Grouped <: S, X, U]
	     (that :SQLExpression[E, O, X])
	     (implicit unify :Interoperable[V, X] { type Unified = U }, ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => ComparisonSQL(unify.left(this), ComparisonSQL.LT, denullify(unify.right(that)))
		}

	/** An SQL expression comparing if this expression is less than a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that     A Scala value of a type which can be promoted to the same type `U` as the value type
	  *                 of this expression. It will translate to an SQL literal.
	  * @param unify    A witness to the fact that the values of the two expressions can be automatically promoted
	  *                 to some unspecified type `U` for the purpose of the comparison by the database.
	  *                 Implicit values provided in its companion object depend on the existence of
	  *                 [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *                 for `V -> U` and `X -> U`.
	  * @param ordering A witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def <[X, U](that :X)
	           (implicit form :SQLForm[X], unify :Interoperable[V, X] { type Unified = U }, ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this < SQLTerm(that)

	/** An SQL expression comparing the value of this expression with another expression with `>=`.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that     Another expression, which value type is comparable in the database with the value type
	  *                 of this expression.
	  * @param unify    A witness to the fact that the values of the two expressions can be automatically promoted
	  *                 to some unspecified type `U` for the purpose of the comparison by the database.
	  *                 Implicit values provided in its companion object depend on the existence of
	  *                 [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *                 for `V -> U` and `X -> U`.
	  * @param ordering A witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def >=[E <: F, O >: Grouped <: S, X, U]
	      (that :SQLExpression[E, O, X])
	      (implicit unify :Interoperable[V, X] { type Unified = U }, ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => ComparisonSQL(unify.left(this), ComparisonSQL.GTE, denullify(unify.right(that)))
		}

	/** An SQL expression comparing if this expression is greater than or equal to a given value.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that     A Scala value of a type which can be promoted to the same type `U` as the value type
	  *                 of this expression. It will translate to an SQL literal.
	  * @param unify    A witness to the fact that the values of the two expressions can be automatically promoted
	  *                 to some unspecified type `U` for the purpose of the comparison by the database.
	  *                 Implicit values provided in its companion object depend on the existence of
	  *                 [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *                 for `V -> U` and `X -> U`.
	  * @param ordering A witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def >=[X, U](that :X)
	            (implicit unify :Interoperable[V, X] { type Unified = U }, form :SQLForm[X], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this >= SQLTerm(that)

	/** An SQL expression comparing the value of this expression with another expression with `>`.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that     Another expression, which value type is comparable in the database with the value type
	  *                 of this expression.
	  * @param unify    A witness to the fact that the values of the two expressions can be automatically promoted
	  *                 to some unspecified type `U` for the purpose of the comparison by the database.
	  *                 Implicit values provided in its companion object depend on the existence of
	  *                 [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *                 for `V -> U` and `X -> U`.
	  * @param ordering A witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def >[E <: F, O >: Grouped <: S, X, U]
	     (that :SQLExpression[E, O, X])
	     (implicit unify :Interoperable[V, X] { type Unified = U }, ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		that match {
			case SQLNull() => SQLNull[Boolean]
			case _ => ComparisonSQL(unify.left(this), ComparisonSQL.GT, denullify(unify.right(that)))
		}

	/** An SQL expression comparing if this expression is greater than a given value. Multi-column expressions
	  * will be compared either as tuples, or as a logical conjunction of comparisons for individual columns.
	  * If the other expression cannot be represented by a compatible column set with this expression (or vice versa),
	  * an exception will be thrown. This can happen as the result of this call, but typically will be delayed until
	  * rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that     A Scala value of a type which can be promoted to the same type `U` as the value type
	  *                 of this expression. It will translate to an SQL literal.
	  * @param unify    A witness to the fact that the values of the two expressions can be automatically promoted
	  *                 to some unspecified type `U` for the purpose of the comparison by the database.
	  *                 Implicit values provided in its companion object depend on the existence of
	  *                 [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *                 for `V -> U` and `X -> U`.
	  * @param ordering A witness to the fact that the magnitudes of the type to which both operands are promoted to
	  *                 are comparable in SQL. Implicit values exist for built in database types, they can also be
	  *                 used to inject the relation on a preexisting type to an arbitrary Scala type.
	  * @return If either `this` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal
	  *         oor `that == null`, than `SQLNull[Boolean]`. Otherwise an `SQLExpression` based on
	  *         the same [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as this expression.
	  */
	def >[X, U](that :X)
	           (implicit unify :Interoperable[V, X] { type Unified = U }, form :SQLForm[X], ordering :SQLOrdering[U])
			:ColumnSQL[F, S, Boolean] =
		this > SQLTerm(that)

	/** An SQL expression comparing the value of this expression with another expression for equality, treating
	  * `NULL` values as equal.
	  * Multi-column expressions will be compared either as tuples, or as a logical conjunction of comparisons
	  * for individual columns. If the other expression cannot be represented by a compatible column set
	  * with this expression (or vice versa), an exception will be thrown. This can happen as the result of this call,
	  * but typically will be delayed until rendering by an
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]/[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * @param that  Another expression, whose value type is comparable in the database with the value type
	  *              of this expression.
	  * @param unify A witness to the fact that the values of the two expressions can be automatically promoted
	  *              to some unspecified type `U` for the purpose of the comparison by the database.
	  *              Implicit values provided in its companion object depend on the existence of
	  *              [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]] evidence
	  *              for `V -> U` and `X -> U`.
	  * @return An `SQLExpression` equivalent to `this === that || this.isNull && that.isNull`
	  */
	def nullEq[E <: F, O >: Grouped <: S, X]
	          (that :SQLExpression[E, O, X])(implicit unify :V Interoperable X) :SQLBoolean[F, S] =
		NullEqualitySQL(unify.left(this), unify.right(denullify(that)))

	def nullNeq[E <: F, O >: Grouped <: S, X]
	           (that :SQLExpression[E, O, X])(implicit unify :V Interoperable X) :SQLBoolean[F, S] =
		NullInequalitySQL(unify.left(this), unify.right(denullify(that)))

	/** An SQL expression for singleton [[net.noresttherein.oldsql.collection.Chain Chain]] (tuple)
	  * containing this expression
	  */
	def chain :ChainTuple[F, S, @~ ~ V] = EmptyChain ~ this

	protected override def convert[X](conversion :SQLConversion[V, X]) :SQLExpression[F, S, X] =
		if (conversion.isIdentity) conversion(this)
		else ConvertedSQL(this, conversion)


	/** Binds all occurrences of [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters to the appropriate
	  * value extracted from `args`, replacing them with [[net.noresttherein.oldsql.sql.ast.BoundParam bound]]
	  * parameters. The resulting expression is based on a `RowProduct` resulting from this expression's base by
	  * removing all occurrences of `ParamClause` pseudo joins.
	  */ //todo: make this a virtual method and return the same type; move to SQLExpression.GroundingTemplate
	def bind[E <: F { type Params = Args }, Args](base :E, args :Args) :SQLExpression[base.GeneralizedParamless, S, V] =
		SQLScribe.applyParams(base, base.bind(args) :base.GeneralizedParamless)(args)(this)

	/** A marker type set to `true` by subclasses which override
	  * [[net.noresttherein.oldsql.sql.SQLExpression.selectFrom selectFrom]],
	  * [[net.noresttherein.oldsql.sql.SQLExpression.topSelectFrom topSelectFrom]],
	  * [[net.noresttherein.oldsql.sql.SQLExpression.subselectFrom subselectFrom]] and
	  * [[net.noresttherein.oldsql.sql.SQLExpression.paramSelectFrom paramSelectFrom]],
	  * creating a ''select'' instance with it as the ''select'' clause.
	  * It is depended upon by [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]] type class,
	  * allowing the expression to be used as an argument to one of
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] methods
	  * of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
	  * Note that this flag ''does not'' fully guarantee that these methods will succeed: for example,
	  * [[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]] is selectable itself, but if it contains
	  * any non-selectable expressions as its elements, an exception will be thrown.
	  * Conversely, some expressions are not nominally selectable, like multi column
	  * [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]], but in many cases will actually be accepted
	  * as arguments by default [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] implementations.
	  */ //todo: make all expressions selectable, with exceptions never escaping their intended usage scope
	type isSelectable <: Boolean

	/** Creates an SQL ''select'' expression with this expression as its ''select'' clause. The ''from'' and ''where''
	  * clauses are defined by this argument, while the ''select'' clause consists of all column sub-expressions
	  * comprising this expression.
	  * This method is supported only by a subset of expression types, which have a well defined,
	  * unique SQL representation. These are mainly
	  * [[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  * and all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subclasses.
	  * It is lower level API exposed to support potential extension by custom expression types and ease writing generic
	  * code. Application code should prefer using
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] and its relatives instead.
	  *
	  * In general, a class providing its own implementation of this, and other SQL ''select'' factory methods
	  * will also provide appropriate [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]] type class
	  * in its companion object. For simplicity, three base `trait`s are introduced, extending which will provide
	  * required implicits: [[net.noresttherein.oldsql.sql.ast.SelectableSQL SelectableSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.SelectableMappingSQL SelectableMappingSQL]]
	  * and [[net.noresttherein.oldsql.sql.ast.SelectableColumnMappingSQL SelectableColumnMappingSQL]],
	  * which use [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.SelectAs SelectAs]] and
	  * [[net.noresttherein.oldsql.sql.ast.SelectColumnAs SelectColumnAs]] as the return types, respectively.
	  * Note that implementations of this and other 'select' methods in those traits are still stubs,
	  * throwing an [[UnsupportedOperationException]] and need to be overriden. The companion objects of those traits
	  * introduce the required implicit definitions, which will be 'inherited' by any extending classes. Their presence
	  * allows to use the aforementioned `select` extension method
	  * of [[net.noresttherein.oldsql.sql.RowProduct, RowProduct]], which unifies the interface and - in principle -
	  * guarantees that an exception is a valid ''select'' clause.
	  *
	  * If `from.`[[net.noresttherein.oldsql.sql.RowProduct.isSubselect isSubselect]], then this method delegates
	  * to [[net.noresttherein.oldsql.sql.SQLExpression.subselectFrom subselectFrom]] and creates
	  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]]. Otherwise, it delegates to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.topSelectFrom topSelectFrom]] and creates
	  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]].
	  * If there are any unbound parameters inside the [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]]
	  * section of this clause, an [[IllegalArgumentException]] will be thrown. Additionally, the resulting ''select''
	  * will be based on type `Nothing` (rather than `from.`[[net.noresttherein.oldsql.sql.RowProduct.Implicit Implicit]]),
	  * making it statically impossible to use as a part of any other
	  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside the `Outer` clause).
	  * @param from any `RowProduct` instance conforming to the type this expression is based on, that is
	  *             containing all the relations listed in `F` in its suffix.
	  * @return a `SelectSQL` based on this instance's `Implicit` type (that is, embeddable only as an expression
	  *         in instances of this `this.Implicit`), using the given arbitrary expression as the ''select'' clause.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.paramSelectFrom paramSelectFrom]]
	  * @see [[net.noresttherein.oldsql.sql.ast.SelectableSQL]]
	  */ //consider: a common base trait for Select and SelectSQL
	@throws[IllegalArgumentException]("if the `from` clause is parameterized (contains any `ParamClause` joins).")
	@throws[UnsupportedOperationException]("if this expression cannot be used as a ''select'' clause.")
	def selectFrom(from :F) :SelectSQL[from.Base, V] =
		if (from.isParameterized)
			throw new IllegalArgumentException(
				s"Cannot use a parameterized clause as a basis for select $this: $from."
			)
		else if (from.isSubselect) {
			subselectFrom[NonEmptyRow](from.asInstanceOf[F with SubselectOf[NonEmptyRow]]).asInstanceOf[SelectSQL[from.Base, V]]
		} else
			topSelectFrom(from.asInstanceOf[Complete[F with GroundRow]])

	/** Creates an SQL ''select'' object extending both [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]
	  * (a ''select'' expression) and [[net.noresttherein.oldsql.sql.Select Select]] (a ''select'' statement),
	  * with this expression as the ''select'' clause and the given `from` clause.
	  * This method is supported only by a few expression types,
	  * mainly [[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]]
	  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]] subclasses.
	  * It is lower level API exposed to support potential extension by custom expression types and ease of writing
	  * generic code. Application code should prefer using
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] and its relatives instead.
	  * The latter is available only if a [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]] type class
	  * is present for the expression type, which restricts its arguments only to selectable expressions
	  * and specifies the appropriate return type.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.selectFrom]]
	  */
	//not type Complete = E because it is impossible to define a type alias defining the fixed point of Complete transformation.
	@throws[UnsupportedOperationException]("if this expression cannot be used as a complete ''select'' clause, " +
	                                       "which is default for all classes which do not override this method.")
	def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E) :TopSelectSQL[V] =
		SelectSQL(from, this)
//		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")

	/** Creates an SQL expression for a dependent ''select'', using this expression as its ''select'' clause
	  * and the argument as its ''from'' and ''where'' clauses (as well as, optionally, ''group by'' and ''having'').
	  * This method is supported only by a few expression types,
	  * mainly [[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]]
	  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] subclasses. It is lower level API
	  * exposed to support potential extension by custom expression types and to facilitate writing of generic code.
	  * Application code should prefer using
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] and its relatives instead.
	  * The latter is available only if a [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]] type class
	  * is present for the expression type, which restricts its arguments only to selectable expressions
	  * and specifies the appropriate return type.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.selectFrom]]
	  */ //we really want to use SubselectOf but it is volatile
	@throws[UnsupportedOperationException]("if this expression cannot be used as a complete ''select'' clause, " +
	                                       "which is default for all classes which do not override this method.")
	def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :SubselectSQL[B, V] =
		SelectSQL.subselect[B, F with SubselectOf[B], V](from, this)

//	def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectSQL[B, V] =
//		SelectSQL.subselect[B, from.type, V](from, this)
//		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")

	def newSubselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :SubselectSQL[B, V] = {
		implicitly[(F { type Implicit = Base; type Base >: B <: RowProduct; type DefineBase[+I <: RowProduct] = I }) <:< SubselectOf[B]]
		SelectSQL.subselect[B, F with SubselectOf[B], V](from, this)
	}

	/** Creates an parameterized SQL ''select'' statement with this expression as its ''select'' clause.
	  * The ''from'' and ''where'' clauses are defined by this argument, while the ''select'' clause consists
	  * of all column sub-expressions constituting this expression.
	  * This method is supported only by a subset of expression types, which have a well defined,
	  * unique SQL representation. These are mainly
	  * [[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  * and all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subclasses.
	  * In general, classes which provide [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]]
	  * implicit definitions (either themselves or by extending
	  * [[net.noresttherein.oldsql.sql.ast.SelectableSQL SelectableSQL]] or one of its subtypes),
	  * will not throw an exception from this method.
	  * It is lower level API exposed to support potential extension by custom expression types and facilitate
	  * writing of generic code; application code should prefer using
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] and its relatives instead.
	  *
	  * @param from a top-level `RowProduct` instance (i.e., not corresponding to a subselect) conforming to the type
	  *             this expression is based on, that is containing all the relations listed in `F` in its suffix.
	  * @return a `Select` parameterized with the unbound parameters in `from`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.selectFrom]]
	  */
	@throws[UnsupportedOperationException]("if this expression cannot be used for a ''select'' clause.")
	def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E) :Select[P, V] =
		Select(from)(this)
//		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")

	//we can't really reorder any expression here as the column count is unspecified
//	def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, S, V] =
//		if (permutation == permutation.indices) this
//		else new RearrangedSQL(this, RearrangedIndexing.permutation(permutation))


	/** A collection of all [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]] referenced
	  * by this expression (within a ''from'' clause of some nested ''select'' subexpression). It is an 'outer
	  * with clause', because they must be declared by some outer ''select'', rather than within this expression
	  * (which is possible only for [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]).
	  * This outer ''select'' may be unspecified (for [[net.noresttherein.oldsql.sql.WithClause.outer outer]] tables
	  * in this clause), or the most nested ''select'' with this expression as a subterm
	  * (for tables listed by [[net.noresttherein.oldsql.sql.WithClause.local local]] property of this clause).
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL Composite]] expression's ''with'' clause is always the sum
	  * of ''with'' clauses of its subexpressions.
	  * @see [[net.noresttherein.oldsql.sql.Query.QueryTemplate.withClause]]
	  */
	def outerWithClause :WithClause = WithClause.empty

	//todo: adding ctes manually to the local section of the with clause
//	def withLocal(cte :CommonTableExpression.*) :SQLExpression[F, S, V] = withLocal(cte.withClause)
//	def withLocal(withClause :WithClause) :SQLExpression[F, S, V]
//	def `with`(cte :CommonTableExpression.*) :SQLExpression[F, S, V] = `with`(cte.withClause)
//	def `with`(withClause :WithClause) :SQLExpression[F, S, V]


	/** Attempts to align this expression with the argument by adjusting the columns of either or both expressions
	  * o the same shape. `SQLExpression` type hierarchy allows expressing more general concepts than actual
	  * SQL and several subclasses, most notably [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  * and, by transition, any composite expression having them as subexpressions, can be rendered with differing
	  * numbers of columns. In the case of the former, this depends on
	  * the [[net.noresttherein.oldsql.schema.Relation Relation]] and
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] present at its corresponding position
	  * in the final ''from'' clause of a formatted SQL ''select''. Others,
	  * like [[net.noresttherein.oldsql.sql.ast.SQLTerm terms]],
	  * depend on an arbitrary [[net.noresttherein.oldsql.schema.SQLForm form]], and the expression can be formatted
	  * differently if a different form is given as the argument.
	  *
	  * As several possible strategies for such a unification are conceivable, and both different
	  * [[net.noresttherein.oldsql.sql.Select.SelectOperator set operations]] in compound ''selects''
	  * and comparison expressions between multi-column expressions require varying approaches,
	  * the whole functionality is encapsulated in [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] strategy,
	  * which, rather than this method, serves as the entry point for the algorithm. However, standard use cases
	  * do not really require alternate handling of expression types other than `ComponentSQL`, but still
	  * need to work for any pair of expressions. For this reason, this method recursively traverses
	  * both this expression and the argument, applying the given `reform` strategy to corresponding subexpression pairs.
	  * In this way, all [[net.noresttherein.oldsql.sql.ast.CompositeSQL composite]] expression implementations
	  * can provide their own deconstruction and alignment with other compatible instances,
	  * to which standard `Reform` implementations can delegate (including the overloaded variants of this method),
	  * in order to be able to focus their implementation solely on the `ComponentSQL` vs `ComponentSQL` case.
	  *
	  * The `reform` argument, through properties `mayExcludeLeft`, `mayIncludeLeft`, `mayReorderLeft`, `mayAddNullLeft`,
	  * `mayReformLeft`, `mayExcludeRight`, `mayIncludeRight`, `mayReorderRight`, `mayAddNullRight`, `mayReformRight`
	  * specifies what kind of reforming actions are allowed on both expressions. If an implementation performs
	  * any reforming directly, without delegating to the strategy, it must abide to the above constraints.
	  * `Reform` defines also methods which modify those flags, which allows to further restrict allowed operations.
	  *
	  * While, with the help of a [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor ExpressionVisitor]],
	  * any expression can in principle handle its pairing with any other expression,
	  * it is often more logical or convenient to place the implementation in one of the classes rather than the other,
	  * as, for example, it is undesirable for other expression types to handle their pairing with some corner cases
	  * like [[net.noresttherein.oldsql.sql.ast.NativeTerm NativeTerm]]
	  * or [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]], which require approaches differing from other terms.
	  * For this reason, and to improve the robustness of the procedure against custom or future extensions,
	  * this method implements multi-dispatch and is allowed to delegate to the same method
	  * (or one of its more specific variants) on the argument, reversing the responsibility.
	  * In order to prevent infinite recursion, such delegation, without introducing extra information
	  * (that is, calling a more specific method) is allowed only `passesAllowed` number of times.
	  * This counter is set to `4` when `Reform` delegates to this method, which gives both expressions potentially
	  * two chances to handle reforming themselves. The default implementation ''always'' delegates
	  * to the other expression if `passesAllowed` is greater than zero, and the recommended, approach even
	  * for expression types for which a specific alignment strategies exist, is to still delegate as long as
	  * `passesAllowed > 1` (that is, the other expression can delegate back to them if it doesn't know how to handle
	  * reforming of this particular pairing) and use the default implementation reluctantly rather than eagerly
	  * in order to allow potential custom or more specific implementations to override that behaviour
	  * with something more appropriate.
	  *
	  * With each such delegation, `passesAllowed` is reduced by one,
	  * and if it reaches zero, the called expression must either reform itself to the best of its ability,
	  * or throw a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]].
	  * An exception are specially treated classes
	  * [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]]
	  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], which can delegate from this method
	  * - if `passesAllowed > 0` - in a standard double dispatch mutual discovery to the specific,
	  * overloaded method variants for those expression types on the argument, without reducing the counter.
	  * If the argument is verified to be either
	  * a [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]]
	  * or a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], this method may also,
	  * directly or indirectly, delegate to the overloaded variant on this instance specific to the argument type.
	  *
	  * If the control cannot be passed to the argument, or an extending class doesn't wish to do so and overrides
	  * this method, the standard approach is to use the [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]]
	  * visitor defined by this instance, which should handle all expression types whose columns can be adjusted
	  * or somehow linked to the columns of this expression. See the documentation of that method for information
	  * about the constraints applying to the visitor.
	  *
	  * This scheme provides a greater degree of freedom than a simple double dispatch, as not only it allows
	  * both instances to discover the type of the other expression and make it possible to place the implementation
	  * in the class where it is more convenient, but also provide a default implementation without forcing it over
	  * the other expression, all while using the existing visitor framework.
	  *
	  * The exact behaviour will depend on individual implementations, but the standard policy is:
	  *   1. Any expression with a full read/write [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] can reform
	  *      an argument of [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]] by replacing its form.
	  *   1. Tuple-like types aligning structurally with one another on Scala level reform their corresponding
	  *      element pairs.
	  *   1. [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] tries to reform the two
	  *      [[net.noresttherein.oldsql.sql.Select.SelectTemplate.selectClause select]] clauses
	  *      if the other expression is also a `SelectSQL`, or reform its ''select'' clause with the whole
	  *      of the other expression otherwise. This is primarily used between parts of a
	  *      [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL compound selects]], but also
	  *      in ''insert ... select'' statements.
	  *   1. Component expressions using the same [[net.noresttherein.oldsql.schema.Mapping Mapping]] type
	  *      can [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.alter alter]] their column sets
	  *      by including/excluding their optional columns. How far this process will go to unify the shapes
	  *      of the expression depends on the `Reform` strategy specific to a given context of use, with an attempt
	  *      to strike a balance between flexibility and predictable results.
	  *
	  * Note however that the final form of an expression is unspecified until it becomes
	  * [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] in the ''from'' clause of the formatted
	  * SQL query, substituting all component expressions for their definitive versions. For this reason there is
	  * little use for this method outside the spelling process, or when the anchoring condition is otherwise
	  * satisfied. In particular, two expressions reformed to the same shape for one SQL statement type
	  * (defined by the [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope SpellingScope]]) can have different
	  * column sets when used in another statement or another clause. Furthermore, subclasses are allowed to return
	  * specialized implementations (other than those constructed normally), which are created with the spelling process
	  * in mind and might behave in an unexpected fashion in other use cases.
	  *
	  * This method is protected in order not to clutter the public interface with implementation API,
	  * so it cannot be delegated to by subclasses (for other instances). Instead, they should either
	  * call `super.reform` (which will delegate to `other` immediately if `passesAllowed > 1`, and indirectly
	  * by the default [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]] visitor if `passesAllowed > 0`.
	  * Alternatively, when changing one or both of the paired expressions, overriding methods should instead
	  * always delegate to `reform`.
	  *
	  * @param other          Another expression of a compatible value type (as defined by `SQLTypeUnification`),
	  *                       which requires aligning with this one either for the purpose of column-wise comparison,
	  *                       or in order to assign values to particular columns
	  *                       within a [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]].
	  * @param reform         a strategy governing the whole process, delegated to for all subexpressions
	  *                       of this expression, instead of this method. It is in particular responsible for
	  *                       defining the semantics of the bottom case of aligning two component expressions.
	  * @param passesAllowed  A counter specifying how many times the control can be passed between `this` and `other`
	  *                       by calling `other.reform`, without reducing the problem (changing the expression pairing
	  *                       to subexpressions). If `passesAllowed > 0`, implementations
	  *                       should do so before throwing an exception, but can first attempt to reform
	  *                       both expressions themselves if they so wish. It is always set to `4`
	  *                       ([[net.noresttherein.oldsql.sql.mechanics.Reform Reform]]`.`[[net.noresttherein.oldsql.sql.mechanics.Reform.MaxPasses MaxPasses]]
	  *                       when `reform` delegates to this method, and each delegation call reduces it by `1`,
	  *                       unless the called method is strictly more specific.
	  * @param leftResult     a conversion
	  *                       (possibly [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]])
	  *                       adapting this expression to a more generic type `U`, common with the other expression.
	  * @param rightResult    a conversion
	  *                       (possibly [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]])
	  *                       adapting the other expression to a more generic type `U`, common with this expression.
	  * @param spelling       A strategy used to format both expressions as SQL which initiated the reforming.
	  *                       It provides [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
	  *                       methods as well as
	  *                       the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]/context
	  *                       in which the expressions are used, which defines the default column sets of
	  *                       component expressions.
	  */ //the type unification to U is necessary when we change the form of an SQLTerm to that of the other expression
/*
	//consider: should columns be reformed? currently they have no special treatment.
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	protected def reform[E <: RowProduct, C >: Grouped <: Single, X, U]
	                    (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
	                    (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], SQLExpression[E, C, U]) =
		if (passesAllowed > 0)
			other.reform(this)(reform.swap, passesAllowed - 1).swap
		else
			reformer(other)(reform, passesAllowed).apply(other)


	/** Attempts to align this expression with the argument by adjusting the columns of either or both
	  * of the expressions to the same shape, preserving the argument's status of being a `ComponentLValueSQL`.
	  * This is particularly important for [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]],
	  * when assigning values to table components as a part of an ''insert'' or ''update'' statement,
	  * but can be also used in the same capacity as the most generic overloaded method variant for `SQLExpression`,
	  * whenever the second expression is a `ComponentLValueSQL`. It serves also a third role as a double dispatch
	  * target for the base `SQLExpression` variant, when `this` expression is a `ComponentLValueSQL`.
	  * This offsets the limitation of the [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]] visitor
	  * used by the former, which cannot preserve the mapping type parameter of the argument.
	  * As a final use case, the default [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]]
	  * ([[net.noresttherein.oldsql.sql.SQLExpression.BaseReformer BaseReformer]]) of this instance delegates
	  * to this method if the visited expression is a (non-`ComponentSQL`) `ComponentLValueSQL`.
	  *
	  * As in the method variant for `SQlExpression`, the default implementation passes the control over to the
	  * appropriate `reform` method of the argument, depending on this expression's type, as long as `passesAllowed > 0`.
	  * Otherwise, if the column counts, as defined by `spelling`, of the converted expressions indeed differ,
	  * it attempts to reform (using the `reform` strategy) both expressions after conversion, as long as at least one
	  * of the conversions is not [[net.noresttherein.oldsql.sql.SQLExpression.Lift.isDerived standard]] - in order
	  * to prevent infinite recursion caused by the standard
	  * [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]] implementations delegating back to
	  * the original expression(s).
	  * If the above fails, which it will in most cases,
	  * it throws a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]].
	  *
	  * Minding the risk of an infinite recursion, this method
	  *   1. cannot delegate to the overloaded variant for `SQLExpression` on `this` or `other` using `this`
	  *      as an argument;
	  *   1. cannot delegate to the [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]] visitor;
	  *   1. can delegate to the same method on `other` if `passesAllowed > 0`, reducing it by `1`
	  *      (on a `ComponentLValueSQl`);
	  *   1. can delegate to the overloaded variant for [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  *      on other if `this` is a `ComponentSQL` and `passesAllowed > 0`, without reducing the counter.
	  *
	  * The `reform` argument, through properties `mayExcludeLeft`, `mayIncludeLeft`, `mayReorderLeft`, `mayAddNullLeft`,
	  * `mayReformLeft`, `mayExcludeRight`, `mayIncludeRight`, `mayReorderRight`, `mayAddNullRight`, `mayReformRight`
	  * specifies what kind of reforming actions are allowed on both expressions. If an implementation performs
	  * any reforming directly, without delegating to the strategy, it must abide to the above constraints.
	  * `Reform` defines also methods which modify those flags, which allows to further restrict allowed operations.
	  *
	  * Note however that the final form of an expression is unspecified until it becomes
	  * [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] in the ''from'' clause of the formatted
	  * SQL query, substituting all component expressions for their definitive versions. For this reason there is
	  * little use for this method outside the spelling process, or when the anchoring condition is otherwise
	  * satisfied. In particular, two expressions reformed to the same shape for one SQL statement type
	  * (defined by the [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope SpellingScope]]) can have different
	  * column sets when used in another statement or another clause. Furthermore, subclasses are allowed to return
	  * specialized implementations (other than those constructed normally), which are created with the spelling process
	  * in mind and might behave in an unexpected fashion in other use cases.
	  *
	  * This method is protected in order not to clutter the public interface with implementation API,
	  * so it cannot be delegated to by subclasses (for other instances). Instead, they should either
	  * call `super.reform`, which will delegate to `other` immediately if `passesAllowed > 0`.
	  * Alternatively, when changing one or both of the paired expressions, overriding methods should instead
	  * always delegate to `reform`.
	  *
	  * @param other          Another expression of a compatible value type (as defined by `SQLTypeUnification`),
	  *                       which requires aligning with this one either for the purpose of column-wise comparison,
	  *                       or in order to assign values to particular columns
	  *                       within a [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]].
	  * @param reform         a strategy governing the whole process, delegated to for all subexpressions
	  *                       of this expression, instead of a method of `SQLExpression` directly. It is in particular
	  *                       responsible for defining the semantics of the bottom case of aligning
	  *                       two component expressions.
	  * @param passesAllowed  A counter specifying how many times the control can be passed between `this` and `other`
	  *                       by calling `other.reform`, without reducing the problem (changing the expression pairing
	  *                       to subexpressions). If `passesAllowed > 0`, implementations
	  *                       should do so before throwing an exception, but can first attempt to reform
	  *                       both expressions themselves if they so wish. It is always set to `4`
	  *                       ([[net.noresttherein.oldsql.sql.mechanics.Reform Reform]]`.`[[net.noresttherein.oldsql.sql.mechanics.Reform.MaxPasses MaxPasses]]
	  *                       when `reform` delegates to this method, and each delegation call reduces it by `1`,
	  *                       unless the called method is strictly more specific.
	  * @param leftResult     a conversion (possibly [[net.noresttherein.oldsql.sql.SQLExpression.Lift.self identity]]
	  *                       adapting this expression to a more generic type `U`, common with the other expression.
	  * @param rightResult    a conversion (possibly [[net.noresttherein.oldsql.sql.SQLExpression.Lift.self identity]]
	  *                       adapting the other expression to a more generic type `U`, common with this expression.
	  * @param spelling       A strategy used to format both expressions as SQL which initiated the reforming.
	  *                       It provides [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
	  *                       methods as well as
	  *                       the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]/context
	  *                       in which the expressions are used, which defines the default column sets of
	  *                       component expressions.
	  */ //todo: replace all comparisons spelling.shape(this) <:> spelling.shape(other) with calls to reform.fallback
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	protected def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
	                    (other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
	                    (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], reform.LValue[E, C, U]) =
		if (passesAllowed > 0)
			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
		else
			(leftResult(this), rightResult(other)) match {
				case (l, r) if !leftResult.isDerived || !rightResult.isDerived => //either l or r aren't ConversionSQL
					reform(l, r)
				case (l, r) =>
					reform.fallback(l, r)
			}

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	protected def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
	                    (other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
	                    (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], reform.LValue[E, C, U]) =
		if (passesAllowed > 0)
			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
		else
			(leftResult(this), rightResult(other)) match {
				case (l, r) if !leftResult.isDerived || !rightResult.isDerived => //either l or r aren't ConversionSQL
					reform(l, r)
				case (l, r) =>
					reform.fallback(l, r)
			}

	/** Attempts to adjust the columns of either or both of this expression and the argument to the same shape,
	  * preserving the argument's status of being a `ComponentSQL`.
	  * This is particularly important for [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]],
	  * when assigning values to table components as a part of an ''insert'' or ''update'' statement,
	  * but can be also used in the same capacity as the most generic overloaded method variant for `SQLExpression`,
	  * whenever the second expression is a `ComponentSQL`. It serves also a third role as a double dispatch
	  * target for the base `SQLExpression` variant, when `this` expression is a `ComponentSQL`.
	  * This offsets the limitation of the [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]] visitor
	  * used by the former, which cannot preserve the mapping type parameter of the argument.
	  * As the final use case, the default [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]]
	  * ([[net.noresttherein.oldsql.sql.SQLExpression.BaseReformer BaseReformer]]) of this instance delegates
	  * to this method if the visited expression is a `ComponentSQL`.
	  *
	  * By default, this method simply delegates to the overloaded variant for the supertype
	  * of [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]]
	  * on this expression. However, as `ComponentSQL` is the bottom case which is typically handled separately,
	  * while the remaining implementations of `ComponentLValueSQL` either do not support reforming at all
	  * ([[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] and its conversions), or are
	  * conversions which are better served treated the same way as other conversions, by resorting to unify
	  * the underlying `ComponentSQL`, it is useful to have this variant declared as the sole implementation point
	  * for this special case.
	  *
	  * Minding the risk of infinite recursion, this method
	  *   1. cannot delegate to the overloaded variant for `SQLExpression` on `this` or `other`
	  *      using `this` as an argument;
	  *   1. cannot delegate to the [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]] visitor;
	  *   1. can always delegate to the overloaded variant for `ComponentLValueSQL` with the same arguments;
	  *   1. can delegate to the same method on `other` if `passesAllowed > 0`, reducing it by `1`.
	  *
	  * The `reform` argument, through properties `mayExcludeLeft`, `mayIncludeLeft`, `mayReorderLeft`, `mayAddNullLeft`,
	  * `mayReformLeft`, `mayExcludeRight`, `mayIncludeRight`, `mayReorderRight`, `mayAddNullRight`, `mayReformRight`
	  * specifies what kind of reforming actions are allowed on both expressions. If an implementation performs
	  * any reforming directly, without delegating to the strategy, it must abide to the above constraints.
	  * `Reform` defines also methods which modify those flags, which allows to further restrict allowed operations.
	  *
	  * Note however that the final form of an expression is unspecified until it becomes
	  * [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] in the ''from'' clause of the formatted
	  * SQL query, substituting all component expressions for their definitive versions. For this reason there is
	  * little use for this method outside the spelling process, or when the anchoring condition is otherwise
	  * satisfied. In particular, two expressions reformed to the same shape for one SQL statement type
	  * (defined by the [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope SpellingScope]]) can have different
	  * column sets when used in another statement or another clause. Furthermore, subclasses are allowed to return
	  * specialized implementations (other than those constructed normally), which are created with the spelling process
	  * in mind and might behave in an unexpected fashion in other use cases.
	  *
	  * This method is protected in order to not clutter the public interface with implementation API,
	  * so it cannot be delegated to by subclasses (for other instances). Instead, they should either
	  * call `super.reform`, which will delegate to `other` immediately if `passesAllowed > 0`.
	  * Alternatively, when changing one or both of the paired expressions, overriding methods should instead
	  * always delegate to `reform`.
	  *
	  * @param other          Another expression of a compatible value type (as defined by `SQLTypeUnification`),
	  *                       which requires aligning with this one either for the purpose of column-wise comparison,
	  *                       or in order to assign values to particular columns
	  *                       within a [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]].
	  * @param reform         a strategy governing the whole process, delegated to for all subexpressions
	  *                       of this expression, instead of a method of `SQLExpression` directly. It is in particular
	  *                       responsible for defining the semantics of the bottom case of aligning
	  *                       two component expressions.
	  * @param passesAllowed  A counter specifying how many times the control can be passed between `this` and `other`
	  *                       by calling `other.reform`, without reducing the problem (changing the expression pairing
	  *                       to subexpressions). If `passesAllowed > 0`, implementations
	  *                       should do so before throwing an exception, but can first attempt to reform
	  *                       both expressions themselves if they so wish. It is always set to `4`
	  *                       ([[net.noresttherein.oldsql.sql.mechanics.Reform Reform]]`.`[[net.noresttherein.oldsql.sql.mechanics.Reform.MaxPasses MaxPasses]]
	  *                       when `reform` delegates to this method, and each delegation call reduces it by `1`,
	  *                       unless the called method is strictly more specific.
	  * @param leftResult     a conversion (possibly [[net.noresttherein.oldsql.sql.SQLExpression.Lift.self identity]]
	  *                       adapting this expression to a more generic type `U`, common with the other expression.
	  * @param rightResult    a conversion (possibly [[net.noresttherein.oldsql.sql.SQLExpression.Lift.self identity]]
	  *                       adapting the other expression to a more generic type `U`, common with this expression.
	  * @param spelling       A strategy used to format both expressions as SQL which initiated the reforming.
	  *                       It provides [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
	  *                       methods as well as
	  *                       the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]/context
	  *                       in which the expressions are used, which defines the default column sets of
	  *                       component expressions.
	  */
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	protected def reform[E <: RowProduct, C[O] <: MappingAt[O], U]
	                    (other :ComponentSQL[E, C])(reform :Reform, passesAllowed :Int)
	                    (implicit leftResult :Lift[V, U], rightResult :Lift[other.Subject, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], reform.LValue[E, C, U]) =
		this.reform(other :LValueSQL[E, C, other.Subject])(reform, passesAllowed)
*/


/*
	private[sql] def `->reform`[E <: RowProduct, C >: Grouped <: GlobalScope, X, U]
	                 (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
	                 (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[X, U], spelling :SQLSpelling)
			:(leftResult.SQLResult[F, S], rightResult.SQLResult[E, C]) =
		this.reform(other)(reform, passesAllowed)

	private[sql] def `->reform`[C <: RowProduct, A >: Grouped <: GlobalScope, X, E[v] <: SQLExpression[C, A, v], U]
	                           (other :ConvertibleSQL[C, A, X, E])(reform :Reform, passesAllowed :Int)
	                           (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[X, U],
	                                     spelling :SQLSpelling)
			:(leftResult.SQLResult[F, S], rightResult.SQLResult[C, A, E[U]]) =
		this.reform(other)(reform, passesAllowed)
*/

/*

	private[sql] def `->reform`[E <: RowProduct, C >: Grouped <: GlobalScope, X, U]
	                 (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
	                 (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], SQLExpression[E, C, U]) =
		this.reform(other)(reform, passesAllowed)

	private[sql] def `->reform`[E <: RowProduct, C[O] <: MappingAt[O], X, U]
	                           (other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
	                           (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], reform.LValue[E, C, U]) =
		this.reform(other)(reform, passesAllowed)

	private[sql] def `->reform`[E <: RowProduct, C[O] <: MappingAt[O], U]
	                 (other :ComponentSQL[E, C])(reform :Reform, passesAllowed :Int)
	                 (implicit leftResult :Lift[V, U], rightResult :Lift[other.Subject, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], reform.LValue[E, C, U]) =
		this.reform(other)(reform, passesAllowed)
*/



	//todo: after Scala 3 expression refactor make this a method of all expressions, not only ComponentSQL
//	def asGrouping :Relation[MappingOf[V]#Projection]

	/** Splits this SQL expression into individual [[net.noresttherein.oldsql.sql.ColumnSQL column]] expressions
	  * (of ''distinct'' types). Note that both the number of columns and expressions themselves can change
	  * when the expression is [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]]
	  * during the spelling process. This method should thus be used only in contexts where that anchoring is assured.
	  * The returned columns are guaranteed to be in number equal to the number of columns rendered by this expression's
	  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]], and their own `defaultSpelling`
	  * methods are consistent with the former.
	  *
	  * In guarantee this consistency with any spelling strategy, this method is protected,
	  * and `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.split split]]`(expression)` should
	  * be used instead.
	  */
	@throws[InseparableExpressionException]("if the expression cannot be separated into individual columns, " +
	                                        "for example a multi-column SQL select.")
	protected def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]]
	//todo: rename to `->split`
	private[oldsql] def `->split`(spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] = split(spelling)

	/** JDBC types of all columns of this expression, after inlining, as they would be read  by the application
	  * if this expression was used as a ''select'' clause. This list must be consistent with the SQL generated
	  * by [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]] with the same `spelling`
	  * argument. Some expression types, in particular [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  * and any composite expression containing them as subexpression, can have different column sets, depending
	  * on the context in which they are used (SQL operation type or usage type, represented
	  * by `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]),
	  * as well as the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] instance used as the ''from'' clause
	  * of the SQL ''select'' containing this expression (or a synthetic instance used with ''insert'' and ''update'').
	  * This value is thus valid only for the specified `spelling` instance, and only if this expression
	  * is already [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] in the ''from'' clause instance
	  * with which it is going to be used. For this reason, the method is of little use outside of the spelling process.
	  *
	  * Because the result is closely tied to the way the expression is formatted as SQL, which is governed
	  * by a [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] used, the method is protected
	  * and in all circumstances `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.shape shape]]
	  * should be used instead in order to guarantee consistency.
	  *
	  * Note however that if the expression is used in a context in which its column types must match another expression
	  * (comparisons, assignments and in ''select'' clauses of SQL ''selects'' forming a ''compound select''),
	  * the expression can be [[net.noresttherein.oldsql.sql.SQLExpression.reform reformed]] during spelling
	  * in order to achieve a unified shape.
	  */
	@throws[UndefinedShapeException]("if this expression is not anchored.")
	@throws[MisalignedExpressionException]("if the expression's internal state is inconsistent and has no definite column shape.")
	protected override def shape(implicit spelling :SQLSpelling) :RowShape

	/** Total number of columns in this expression, after inlining, as would be rendered in SQL
	  * by [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]] and/or
	  * [[net.noresttherein.oldsql.sql.SQLExpression.explodedSpelling explodedSpelling]] in the given
	  * SQL dialect. The number will be accurate for an expression used in the context of a ''from'' clause `f`
	  * only if [[net.noresttherein.oldsql.sql.SQLExpression.isAnchored(from:F) isAnchored]]`(f)`.
	  * Note however that in certain contexts, such as when comparing values of two expressions,
	  * supporting expressions can be [[net.noresttherein.oldsql.sql.SQLExpression.reform reformed]] to a different
	  * column count in order to match the columns of another expression.
	  *
	  * It is the value returned by
	  * `SQLSpelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
	  * if no non-standard spelling for this expression is needed; the latter should be always used instead
	  * of this method in order to always produce results consistent with the SQL for this expression,
	  * as formatted by `spelling`.
	  *
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.split]]
	  */
//	@throws[UnsupportedOperationException]("if this expression is not anchored.") //consider: switching IllegalStateException to IllegalSQLException
//	@throws[IllegalStateException]("if the expression's internal state is inconsistent and no definite column count exists.")
	@throws[UndefinedShapeException]("if this expression is not anchored.")
	@throws[MisalignedExpressionException]("if the expression's internal state is inconsistent and has no definite column shape.")
	protected def columnCount(implicit spelling :SQLSpelling) :Int


	/** Number of JDBC parameters in the SQL for this clause. This counts all individual columns
	  * of both embedded bound and unbound parameters and should equal the number of '?' JDBC placeholders
	  * in the formatted SQL returned by [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]].
	  * Note that the result will be accurate regarding the formatted SQL only if this expression
	  * is [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]]
	  * in the ''from'' clause equal to the one passed as the `from` argument
	  * to [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]] (or a related method).
	  * Furthermore, just as `defaultSpelling`, it is reflective only of the standard/default SQL and not necessarily
	  * of the actual value when formatted with `spelling`;
	  * `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.sqlParamCount sqlParamCount]] should always be
	  * used instead of this method in all contexts other than the body of the latter method.
	  */ //we can't take a `from` argument because this method exists also in Relation (without info about type F)
//	@throws[UnsupportedOperationException]("if this expression is not anchored.")
	@throws[UndefinedShapeException]("if this expression is not anchored.")
//	@throws[MisalignedExpressionException]("if the expression's internal state is inconsistent and has no definite column shape.")
	protected def sqlParamCount(implicit spelling :SQLSpelling) :Int
	private[oldsql] final def `->sqlParamCount`(spelling :SQLSpelling) :Int = sqlParamCount(spelling)

	/** Translates this expression object into an annotated SQL string `SpelledSQL[P, E]`. This is a fallback method
	  * used by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] if no non-standard representation
	  * is required by the used DBMS. This method should not be exposed, as the clients of this class should
	  * always use the `apply` method of `SQLSpelling` as the SQL formatter.
	  */
	@throws[InvalidSQLException]("if the expression cannot be formatted as a valid SQL or is unsupported " +
	                             "by the spelling strategy")
	protected def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                (implicit spelling :SQLSpelling) :SpelledSQL[P]

	/** Translates this expression object into an annotated SQL string `SpelledSQL[P, E]`. This is the fallback method
	  * used by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] if no non-standard representation
	  * is required for the used DBMS. This is similar to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]], but returns SQL individually
	  * for all member columns (including, recursively, all columns of subexpressions). Apart from the use cases
	  * which require inserting specific SQL between the columns (for example, column-wise comparisons of components),
	  * it is also used for expressions used in locations which require a flat structure - in particular
	  * ''select'' and ''group by'' clauses.
	  */ //consider: rename to columnSpelling or splitSpelling
	@throws[InseparableExpressionException]("if the expression cannot be separated into individual column strings, " +
	                                        "for example a multi-column SQL select.")
	@throws[InvalidSQLException]("if the expression cannot be formatted as a valid SQL or is unsupported " +
	                             "by the spelling strategy")
	//todo: define and review if each column is an atomic expression (in '(' and ')')
	protected def explodedSpelling[P](independent :Boolean)
	                                 (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                 (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]]

	private[sql] final def defaultSpelling[P](spelling :SQLSpelling)
	                                         (from :F, context :SQLContext[P], params :Parameterization[P, F])
			:SpelledSQL[P] =
		defaultSpelling(from, context, params)(spelling)

	private[sql] final def explodedSpelling[P](spelling :SQLSpelling, independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
			:Seq[SpelledSQL[P]] =
		explodedSpelling(independent)(from, context, params)(spelling)



	/** Checks if the two expressions have the same SQL signature, that is consist of the same number
	  * of columns and of the same (database) type. This assumes both are grounded in clauses using the exact same
	  * [[net.noresttherein.oldsql.schema.Relation relations]] as the instances referenced by the expressions (if any),
	  * that is that the spelling process would not replace included
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions with other instances due to either
	  * the mappings being altered or altogether different tables being used.
	  * @return `this.`[[net.noresttherein.oldsql.sql.SQLExpression.selectForm]]` `[[net.noresttherein.oldsql.schema.SQLReadForm.comparable comparable]]` that.selectForm`.
	  *  */
	def comparable(that :SQLExpression.__) :Boolean = selectForm comparable that.selectForm //todo: type compatibility

	/** Checks if the two expressions have the same value type and the same SQL signature
	  * (as defined by [[net.noresttherein.oldsql.sql.SQLExpression.comparable comparable]]).
	  * This implies that, assuming them being grounded in the same ''from'' clause (or at least the clause
	  * type parameter of one being a supertype of another), substituting one for another will yield valid SQL -
	  * both the formatted `String` from the point of view of the DBMS, and as an `SQLExpression`.
	  */
	def compatible(that :SQLExpression.__) :Boolean = ???

	/** Tests if this expression and the other expression are equal. It functions the same way as `equals`,
	  * but in case of component expressions, only the indices of the relations are compared, rather than the whole
	  * mapping.
	  */
//	def same[E <: F](that :SQLExpression[E, Grouped, _]) :Boolean
//four equalities:
// equals - all mappings and relations used are equal; param relation uses form equality
// identical - structural equality using Mapping.identical
// isomorphic - identical table mappings, grouping expressions isomorphic
// homomorphic (same domain) - same relation indices, ignores includes/excludes
	/** Checks if this expression is equal to the given one, abstracting from possibly different relations
	  * and column composition of featured [[net.noresttherein.oldsql.sql.ast.ComponentSQL component expressions]].
	  * This is a weaker variant of [[net.noresttherein.oldsql.sql.SQLExpression.isomorphic isomorphism]],
	  * which ignores alterations made to components. It reverses the implication of the latter:
	  * if substituting one expression for another ''could'' result in the same final SQL, then they are homomorphic.
	  */
	def homomorphic(that :SQLExpression.__) :Boolean = this isomorphic that //todo: homomorphism

	//fixme: flawed - different relations, or even mapping instances, can mean different column sets.
	// Param/grouping mapping equality can be overridden on JoinedParam/JoinedGrouping level.
	// if we want to allow isomorphism (or homomorphism, etc.) between different mappings, we must enforce expression
	// is of the same domain type.
	/** Tests if this expression is equal to the given one, abstracting from possibly different relations in the domain.
	  * Two expressions are isomorphic if they can be [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]]
	  * in the same ''from'' clause, and for every such clause, the anchored expressions are identical.
	  * This check defines the de facto complete equivalency from the point of view of the application, taking into
	  * account the placeholder nature of a [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
	  * (and a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]). Note that this means that
	  * for [[net.noresttherein.oldsql.sql.ast.JoinedGrouping grouping expressions]], only their type/shape
	  * is taken into account, and the entirety of the expression is ignored. Furthermore, this implies that
	  * any [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.alter alterations]]
	  * made to the column composition of a component must be the same in both expressions.
	  *
	  * Substituting a subexpression for an isomorphic one will have no effect on the generated SQL for the whole
	  * query/statement.
	  */
	def isomorphic(that :SQLExpression.__) :Boolean

	/** Tests if this expression would produce the same value as the given expression, abstracting from possibly
	  * different clauses. Similar to isomorphic, but generally disregards order of elements in composite expressions
	  * such as 'and', 'or', seq. Be warned that this method's primary use is for tests, and production code should not
	  * depend on it. No guarantees are given and the contract is best-effort only.
	  */ //todo: base this
	private[oldsql] def equivalent(that :SQLExpression.__) :Boolean = isomorphic(that)

	/** Compares the two expressions for structural equality: two instances created by executing the same,
	  * pure, functional code twice should always compare equal. This represents functional equivalency: the two
	  * instances are interchangeable in contexts which do not share state with either of them
	  * (that is, in-memory object graphs of the 'inspecting' code and the expressions are disjoint).
	  * This relation is strictly looser than equality as defined by `equals`: while for most expression types
	  * the two methods will yield the same results (and in fact, the default implementation of this method
	  * delegates to `equals`), the primary difference lies in [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]]
	  * expressions: they compare equal only if their [[net.noresttherein.oldsql.schema.Relation relations]] are equal.
	  * The latter however almost universally implement equals as ''referential'' equality, i.e. any object
	  * is equal only to itself. For database tables, this is largely irrelevant, as their instances are typically
	  * static, unique for the whole application, and two expressions for the same table will compare equal.
	  * However, [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]] instances are always
	  * unique to a particular parameter in the ''from'' clause and thus recreating the same expression will result
	  * in an expression unequal to the original.
	  *
	  * Like other mappings, [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] parameter mapping
	  * will recognize only the components belonging to that instance - expressions derived from the value
	  * of that particular parameter - which will make the two expressions incompatible in the sense that their parts
	  * are ''not'' interchangeable, with attempts to combine the parameter relation of one expression
	  * with subcomponents of another, of the same type and in the same position,
	  * resulting in an [[IllegalArgumentException]] (typically) being thrown. This can happen in particular
	  * when a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] instance is being modified and the expressions
	  * used by its ''where''/''having'' etc. clauses are rewritten. However, when the expressions are used as wholes,
	  * they can be safely combined to form larger expressions as long as the result is used in a purely additive
	  * manner.
	  */
	def identical(that :SQLExpression.__) :Boolean = equals(that)  //todo: deep equality

//	def compareWith(that :SQLExpression.*)(cmp :(JoinedRelation.*, JoinedRelation.*) => Boolean) :Boolean

	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLExpression.__]

	/** '''Throws `UnsupportedOperationException`'''. The intent of writing `expr1 == expr2` is unclear:
	  * did the programmer mean to compare the two scala objects, or to create an SQL expression comparing
	  * the two expressions? Use explicitly `equals` for the former and `===` (or `nulleq`) for the latter.
	  */
//	def ==(other :Any) :Any =
//		throw new UnsupportedOperationException(
//			"Expression expr1 == expr2 is unclear: was the intent object equality, or an SQL expression comparing " +
//				"the two expressions? Use equals, sameAs for the former and === for the latter."
//		)

	//todo: verify all concrete classes implement toString and equals
	//todo: chunkedString
//	def dumpString :ChunkedString
	def typeString :ChunkedString = selectForm.toString
}


/**
  * @define GroundingDoc A mixin trait used by [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
  *                      and its subclasses to specify that the result of methods rebasing this expression
  *                      to another `clause` shares a base trait `Expr` with this expression. In other words,
  *                      extending this trait declares that the type constructor of the expression is preserved
  *                      by those methods.
  *
  *                      This is independent of preserving the type constructor when the expression is converted by a
  *                      [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]], which is in turn enforced
  *                      by mixing in [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]].
  * @define F            A ''from'' clause in which this expression is grounded - a list of relations/tables which provide
  *                      columns used in this expression.
  * @define S            The ''scope'' specifying the multiplicity of rows used by this expression and, as the result,
  *                      where in an SQL statement this expression can be used.
  *                        - [[net.noresttherein.oldsql.sql.SQLExpression.Grouped Grouped]] scope specifies
  *                          that the value of the expression depends on any number of rows, and is used for expressions
  *                          which ''might'' contain SQL aggregate functions as their subexpression. If it is indeed
  *                          the case, then this expression is grounded in (i.e., its `F` type parameter is a subtype of)
  *                          an [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]], and cannot refer to
  *                          individual rows of relations included in the clause's
  *                          [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]] part (the actual
  *                          ''from'' clause). Grouped expressions are restricted to ''select'' and ''having'' clauses
  *                          and cannot be used in dependent ''selects'' of an SQL ''select'' using `F`
  *                          as its ''from'' clause (including, most likely, a ''group by'' clause).
  *                        - [[net.noresttherein.oldsql.sql.SQLExpression.Single Single]] scope specifies
  *                          that the expression refers to individual rows in the [[java.sql.ResultSet ResultSet]]
  *                          and do not'contain any aggregate functions depending on rows in `F` (it might still contain
  *                          a dependent ''select'' with a ''group by'' clause). Single row expressions can be used
  *                          anywhere in a ''select'' grounded in clause `F`, and may be
  *                          [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.expand expanded]] to clauses
  *                          of direct and indirect dependent ''selects'' of the former. A single row expression
  *                          is at the same time a grouped expression:
  *                          `SQLExpression[F, Single, V] <:< SQLExpression[F, Grouped, V]`
  *                          for any types `F <: RowProduct` and `V`.
  * @define V            The result type of the expression; not necessarily an SQL type, but potentially any type for which
  *                      an [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] exists (likely a result of mapping
  *                      a form for a type naturally supported by JDBC).
  */
object SQLExpression {

	/** An SQL [[net.noresttherein.oldsql.sql.ast.SQLLiteral literal]] expression of the given value.
	  * This methods requires [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] type class to be present for `T`.
	  * If, additionally, the type class is also [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]],
	  * then the expression will be a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]].
	  */
	@inline def apply[T](literal :T)(implicit factory :SQLTerm.Factory[T]) :factory.Res = factory(literal)


	/** A template interface for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] similar to
	  * [[scala.collection.IterableOps IterableOps]] type hierarchy from standard Scala library.
	  * It is parameterized with a supertype of extending expression, as well as its type constructor.
	  * Extended by `SQLExpression`, and then directly by its subclasses which wish to provide more specific
	  * return types of several methods than `SQLExpression` - in particular, regular value type conversion
	  * [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.convert convert]].
	  * @tparam F  $F
	  * @tparam S  $S
	  * @tparam V  $V
	  * @tparam EC A type constructor for some supertype of this expression which is preserved by value conversions:
	  *            applying an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]]`[V, Y]`
	  *            to this expression returns an `EC[Y]`.
	  */
	trait ConvertingTemplate[-F <: RowProduct, -S >: Grouped <: Single, V, +EC[v] <: ConvertibleSQL[F, S, v, EC]] {
		self :EC[V] with ConvertingTemplate[F, S, V, EC] =>

		/** An identity conversion designed to improve type inference where a `ConvertibleSQL` is expected. */
		@inline final def toConvertibleSQL :EC[V] = this

		/** Converts this expression to an expression of value type being a supertype of this expression's value type.
		  * This conversion naturally is not, in general, reversible. Therefore care should be taken in order
		  * to ensure that the resulting expression occurs only in covariant position (i.e, it's value might be selected,
		  * but it cannot be written).
		  */
		@throws[UnsupportedOperationException]("if this expression is a BoundParam.")
		def upcast[X](implicit ev :V <:< X, tag :ClassTag[V]) :EC[X] = to(SQLConversion.supertype[V, X])

		/** Represents this expression as one of a, narrower in some sense, type `X`.
		  * This is a reverse of promoting an expression to greater precision/a wider type, as allowed by
		  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation SQLTransformation]]
		  * and implemented by `this.`[[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[X]`. Where the latter
		  * represented a 'sure' conversion which can never fail, this one forces the value to a type of a lesser precision,
		  * or even possibly one which can no longer hold it, such as `Option[X]#get`. As with the latter however,
		  * the conversion is applied only on the level of Scala types and does not alter the underlying SQL.
		  */
//		def downcast[X](implicit conversion :SQLTransformation[X, V]) :SQLExpression[F, S, X] =
//			to(conversion.inverse)

//		  *   1. the whole expression is replaced with a new, independent one created by `conversion(this)`, or
		/** Converts this expression to one of value type `Y`, without any effect on the actual generated SQL.
		  * This conversion may take two forms:
		  *   1. the expression is wrapped in a generic [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]]
		  *      which stores the converted expression and the used conversion, or,
		  *   1. if `conversion :`[[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]],
		  *      this expression will be wrapped in a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]]
		  *      by delegating to `this.`[[net.noresttherein.oldsql.sql.SQLExpression.convert convert]].
		  *      Subclasses extending directly `ConvertingTemplate` can narrow down the result type
		  *      template use a specialized subclass of `ConvertedSQL` in order to preserve their type constructor,
		  *      but the principle remains the same.
		  *
		  * Because this method should stay consistent with `conversion(this)`, which may create custom wrappers
		  * over this expression, or even replacing it with a different expression, this method
		  * should not be generally overridden save for narrowing the returned type to
		  * `conversion.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.SQLResult ConvertedSQL]]`[F, S, Expr[Y]]`.
		  * Note however that this declaration uses simply `SQLExpression[F, S, Y]`, not the former or `Expr[Y]`
		  * as the return type, and such narrowing must be done explicitly by subclasses.
		  *
		  * This method starts a double dispatch with `SQLTransformation`: `SQLConversion[V, Y]` instances
		  * will delegate to [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.convert convert]],
		  * while other implementations return
		  * `this.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.adapt adapt]]`(conversion)`
		  * by default. In general, it is those two methods which should be overridden instead of this one.
		  */ //consider: renaming to cast
		def to[X](implicit conversion :SQLAdaptation[V, X]) :conversion.SQLResult[F, S, EC[X]] = conversion(this)

		/** Default delegation target of `conversion(this)`, invoked in double dispatch starting
		  * with `this.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.to to]]`[Y]`.
		  * It is implemented by [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
		  * and [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] to return a generic
		  * [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]]/[[net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL AdaptedColumnSQL]],
		  * but can be overridden by sticky expression wrappers to remain 'on the top' by delegating the transformation
		  * to a wrapped subexpression. This method rarely needs overriding, as the main purpose
		  * of extending a `SQLTransformation` rather than its subtype
		  * [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]] is to control the type and character
		  * of the returned expression.
		  * @see [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.convert convert]]
		  */
		protected def adapt[X](conversion :SQLAdaptation[V, X]) :SQLExpression[F, S, X] =
			AdaptedSQL(this, conversion)

		/** A straightforward delegate to protected
		  * `this.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.adapt adapt]]
		  * making it possible to call by `SQLTransformation` or `SQLSpelling`.
		  */
		protected[sql] def `->adapt`[X](conversion :SQLAdaptation[V, X]) :SQLExpression[F, S, X] =
			adapt(conversion)

		/** Converts this expression to one with a value type `Y` by wrapping it in a
		  * [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]] or its subclass.
		  * This conversion has no effect on the SQL generated from this expression and happens only on the level
		  * of the Scala type system. It is intended to cover cases such as widening of an `Int` to `Long`,
		  * or making an `SQLExpression[F, S, V]` compatible with `SQLExpression[F, S, Option[V]]`.
		  * The expression will use the same [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[V]`
		  * as this instance and only the result will be mapped into `Y`, rather than switching
		  * to a different `getXxx` method of the JDBC `ResultSet`. It thus works the same way as
		  * [[net.noresttherein.oldsql.sql.SQLExpression.map map]], but takes advantage of the `conversion` argument
		  * which can be compared with `equals`, making the resulting expression comparable with other instances
		  * of [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]], which mapping with an arbitrary
		  * function `V => Y` cannot accomplish. Additionally, `SQLTransformation` instances
		  * represent conversions which may be reversible to a degree (perhaps with a loss of precision).
		  *
		  * This is a double dispatch method called by `SQLConversion` when it is used as an implicit argument of
		  * `this.`[[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[Y]`, and covers the case when the conversion
		  * is applied only to the value type, rather then the whole expression.
		  */
		protected def convert[X](conversion :SQLConversion[V, X]) :EC[X]

		/** A straightforward delegate to protected
		  * `this.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.transform transform]]
		  * making it possible to call by `SQLTransformation` or `SQLSpelling`.
		  */
		protected[sql] def `->convert`[Y](conversion :SQLConversion[V, Y]) :EC[Y] =
			(this :ConvertingTemplate[F, S, V, EC]).convert(conversion)

		/** Convert this expression to one typed `Option[V]`, without any effect on the actual generated SQL.
		  * This is implemented simply as `this.`[[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[Option[V]]`.
		  */
		def opt :EC[Option[V]] = convert(SQLConversion.toOption) //don't use to[Option[V]] as it causes infinite recursion

		/** Pass this expression as an ''OUT'' parameter of a stored procedure.
		  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]] wrapping a parameter type in the parameter tuple
		  * type argument of [[net.noresttherein.oldsql.sql.StoredProcedure StoredProcedure]] signifies that its value
		  * can be retrieved by the application after the execution of the procedure. In a static context,
		  * passing an argument expression as `Out[X]` will include `X` in the values returned by created
		  * [[net.noresttherein.oldsql.sql.Call Call]] statements, while passing it simply as `X` will omit the parameter,
		  * treating it as if it was an ''IN'' parameter, declared by the procedure as `X`. The generated SQL is not changed
		  * by this operation. This is equivalent to [[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[Out[V]]`.
		  */
		def out :EC[Out[V]] = convert(SQLConversion.toOut) //don't use to[Out[V]] as it causes infinite recursion

		/** Maps the read (selected) scala value of this expression, without any effect on the actual generated SQL. */
		def map[Y](f :V => Y) :EC[Y] =
			(this :ConvertingTemplate[F, S, V, EC]).convert(SQLConversion(".map", f))


		//fixme: docs out of date
		/** Attempts to align this expression with the argument by adjusting the columns of either or both expressions
		  * o the same shape. `SQLExpression` type hierarchy allows expressing more general concepts than actual
		  * SQL and several subclasses, most notably [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
		  * and, by transition, any composite expression having them as subexpressions, can be rendered with differing
		  * numbers of columns. In the case of the former, this depends on
		  * the [[net.noresttherein.oldsql.schema.Relation Relation]] and
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] present at its corresponding position
		  * in the final ''from'' clause of a formatted SQL ''select''. Others,
		  * like [[net.noresttherein.oldsql.sql.ast.SQLTerm terms]],
		  * depend on an arbitrary [[net.noresttherein.oldsql.schema.SQLForm form]], and the expression can be formatted
		  * differently if a different form is given as the argument.
		  *
		  * As several possible strategies for such a unification are conceivable, and both different
		  * [[net.noresttherein.oldsql.sql.Select.SelectOperator set operations]] in compound ''selects''
		  * and comparison expressions between multi-column expressions require varying approaches,
		  * the whole functionality is encapsulated in [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] strategy,
		  * which, rather than this method, serves as the entry point for the algorithm. However, standard use cases
		  * do not really require alternate handling of expression types other than `ComponentSQL`, but still
		  * need to work for any pair of expressions. For this reason, this method recursively traverses
		  * both this expression and the argument, applying the given `reform` strategy to corresponding subexpression pairs.
		  * In this way, all [[net.noresttherein.oldsql.sql.ast.CompositeSQL composite]] expression implementations
		  * can provide their own deconstruction and alignment with other compatible instances,
		  * to which standard `Reform` implementations can delegate (including the overloaded variants of this method),
		  * in order to be able to focus their implementation solely on the `ComponentSQL` vs `ComponentSQL` case.
		  *
		  * The `reform` argument, through properties `mayExcludeLeft`, `mayIncludeLeft`, `mayReorderLeft`, `mayAddNullLeft`,
		  * `mayReformLeft`, `mayExcludeRight`, `mayIncludeRight`, `mayReorderRight`, `mayAddNullRight`, `mayReformRight`
		  * specifies what kind of reforming actions are allowed on both expressions. If an implementation performs
		  * any reforming directly, without delegating to the strategy, it must abide to the above constraints.
		  * `Reform` defines also methods which modify those flags, which allows to further restrict allowed operations.
		  *
		  * While, with the help of a [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor ExpressionVisitor]],
		  * any expression can in principle handle its pairing with any other expression,
		  * it is often more logical or convenient to place the implementation in one of the classes rather than the other,
		  * as, for example, it is undesirable for other expression types to handle their pairing with some corner cases
		  * like [[net.noresttherein.oldsql.sql.ast.NativeTerm NativeTerm]]
		  * or [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]], which require approaches differing from other terms.
		  * For this reason, and to improve the robustness of the procedure against custom or future extensions,
		  * this method implements multi-dispatch and is allowed to delegate to the same method
		  * (or one of its more specific variants) on the argument, reversing the responsibility.
		  * In order to prevent infinite recursion, such delegation, without introducing extra information
		  * (that is, calling a more specific method) is allowed only `passesAllowed` number of times.
		  * This counter is set to `4` when `Reform` delegates to this method, which gives both expressions potentially
		  * two chances to handle reforming themselves. The default implementation ''always'' delegates
		  * to the other expression if `passesAllowed` is greater than zero, and the recommended, approach even
		  * for expression types for which a specific alignment strategies exist, is to still delegate as long as
		  * `passesAllowed > 1` (that is, the other expression can delegate back to them if it doesn't know how to handle
		  * reforming of this particular pairing) and use the default implementation reluctantly rather than eagerly
		  * in order to allow potential custom or more specific implementations to override that behaviour
		  * with something more appropriate.
		  *
		  * With each such delegation, `passesAllowed` is reduced by one,
		  * and if it reaches zero, the called expression must either reform itself to the best of its ability,
		  * or throw a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]].
		  * An exception are specially treated classes
		  * [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]]
		  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], which can delegate from this method
		  * - if `passesAllowed > 0` - in a standard double dispatch mutual discovery to the specific,
		  * overloaded method variants for those expression types on the argument, without reducing the counter.
		  * If the argument is verified to be either
		  * a [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]]
		  * or a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], this method may also,
		  * directly or indirectly, delegate to the overloaded variant on this instance specific to the argument type.
		  *
		  * If the control cannot be passed to the argument, or an extending class doesn't wish to do so and overrides
		  * this method, the standard approach is to use the [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]]
		  * visitor defined by this instance, which should handle all expression types whose columns can be adjusted
		  * or somehow linked to the columns of this expression. See the documentation of that method for information
		  * about the constraints applying to the visitor.
		  *
		  * This scheme provides a greater degree of freedom than a simple double dispatch, as not only it allows
		  * both instances to discover the type of the other expression and make it possible to place the implementation
		  * in the class where it is more convenient, but also provide a default implementation without forcing it over
		  * the other expression, all while using the existing visitor framework.
		  *
		  * The exact behaviour will depend on individual implementations, but the standard policy is:
		  *   1. Any expression with a full read/write [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] can reform
		  *      an argument of [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]] by replacing its form.
		  *   1. Tuple-like types aligning structurally with one another on Scala level reform their corresponding
		  *      element pairs.
		  *   1. [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] tries to reform the two
		  *      [[net.noresttherein.oldsql.sql.Select.SelectTemplate.selectClause select]] clauses
		  *      if the other expression is also a `SelectSQL`, or reform its ''select'' clause with the whole
		  *      of the other expression otherwise. This is primarily used between parts of a
		  *      [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL compound selects]], but also
		  *      in ''insert ... select'' statements.
		  *   1. Component expressions using the same [[net.noresttherein.oldsql.schema.Mapping Mapping]] type
		  *      can [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.alter alter]] their column sets
		  *      by including/excluding their optional columns. How far this process will go to unify the shapes
		  *      of the expression depends on the `Reform` strategy specific to a given context of use, with an attempt
		  *      to strike a balance between flexibility and predictable results.
		  *
		  * Note however that the final form of an expression is unspecified until it becomes
		  * [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] in the ''from'' clause of the formatted
		  * SQL query, substituting all component expressions for their definitive versions. For this reason there is
		  * little use for this method outside the spelling process, or when the anchoring condition is otherwise
		  * satisfied. In particular, two expressions reformed to the same shape for one SQL statement type
		  * (defined by the [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope SpellingScope]]) can have different
		  * column sets when used in another statement or another clause. Furthermore, subclasses are allowed to return
		  * specialized implementations (other than those constructed normally), which are created with the spelling process
		  * in mind and might behave in an unexpected fashion in other use cases.
		  *
		  * This method is protected in order not to clutter the public interface with implementation API,
		  * so it cannot be delegated to by subclasses (for other instances). Instead, they should either
		  * call `super.reform` (which will delegate to `other` immediately if `passesAllowed > 1`, and indirectly
		  * by the default [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]] visitor if `passesAllowed > 0`.
		  * Alternatively, when changing one or both of the paired expressions, overriding methods should instead
		  * always delegate to `reform`.
		  *
		  * @param other       another expression, located in an opposed position to this expression in formatted SQL.
		  *                    We use a conjunction type to specify the most specific form of
		  *                    [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]]
		  *                    mixed in by the expression, which defines the type to which it is reformed, assuming
		  *                    `rightResult` is an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]].
		  * @param reform      a strategy governing the whole process, delegated to for all subexpressions
		  *                    of this expression, instead of this method. It is in particular responsible for
		  *                    defining the semantics of the bottom case of aligning two component expressions.
		  * @param passCount   A counter specifying how many times the control has passed between `this` and `other`
		  *                    by calling `other.reform`, without reducing the problem (changing the expression pairing
		  *                    to subexpressions). Every next call to `reform` must increase the counter: `passCount.++`.
		  *                    If `!passCount.mayPass`, the reformer cannot call
		  *                    [[net.noresttherein.oldsql.sql.SQLExpression.BaseReformer.pass pass]] or
		  *                    `other.reform(this)(reform, _)`, it ''must'' either perform reforming itself,
		  *                    or delegate to [[net.noresttherein.oldsql.sql.SQLExpression.BaseReformer.fallback fallback]]
		  *                    (`reform.`[[net.noresttherein.oldsql.sql.mechanics.Reform.fallback fallback]]).
		  *                    As a guidline, normal expressions should not attempt reforming immediately, but wait
		  *                    at least for the other expression to also have seen the pair. This can be checked with
		  *                    `passCount.`[[net.noresttherein.oldsql.sql.mechanics.Reform.PassCount.otherHasPassed otherHasPassed]].
		  * @param leftResult  a conversion (possibly
		  *                    [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]]
		  *                    adapting this expression to a more generic, or posessing higher precision, type `U`,
		  *                    common with the other expression.
		  * @param rightResult a conversion (possibly
		  *                    [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]]
		  *                    adapting this expression to a more generic, or posessing higher precision, type `U`,
		  *                    common with the other expression.
		  * @param spelling    A strategy used to format both expressions as SQL which initiated the reforming.
		  *                    It provides [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
		  *                    methods as well as
		  *                    the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]/context
		  *                    in which the expressions are used, which defines the default column sets of
		  *                    component expressions.
		  *///the type unification to U is necessary when we change the form of an SQLTerm to that of the other expression
		@throws[MismatchedExpressionsException]("if the two expressions cannot be aligned")
		@throws[MisalignedExpressionException]("if one of the expressions, or its subexpressions " +
		                                       "has an internally inconsistent shape, which cannot be reformed")
		@throws[UnsupportedOperationException]("if either of the expressions contains a LooseComponent")
		protected def reform[F1 <: F, S1 >: Grouped <: S,
		                     F2 <: RowProduct, S2 >: Grouped <: Single, V2, EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                    (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                    (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
		                              spelling :SQLSpelling)
				:(leftResult.SQLResult[F1, S1, EC[U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
			if (this eq other) //todo: add this check in other places
				(leftResult(this), rightResult(other))
			else if (passCount.firstTime)
				other.reform[F2, S2, F1, S1, V, EC, U](this)(reform.swap, passCount.++).swap
			else
				reformer[F1, S1, F2, S2, V2, EC2, U](other)(reform, passCount).apply(other)

		protected[sql] def `->reform`[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.SQLResult[F1, S1, EC[U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
			this.reform[F1, S1, F2, S2, V2, EC2, U](other)(reform, passCount)

		/** Invokes protected method
		  * `other.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.reform reform]]`(this)(reform.swap, passCount.++).swap`
 		  */
		@throws[MismatchedExpressionsException]("if the two expressions cannot be aligned")
		@throws[MisalignedExpressionException]("if one of the expressions, or its subexpressions " +
		                                       "has an internally inconsistent shape, which cannot be reformed")
		@throws[UnsupportedOperationException]("if either of the expressions contains a LooseComponent")
		@throws[IllegalArgumentException]("if the pass count limit has been reached (!passCount.mayPass)")
		protected def passReform[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                         EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                        (other :ConvertingTemplate[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                        (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
		                                  spelling :SQLSpelling)
				:(leftResult.SQLResult[F1, S1, EC[U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
			if (passCount.mayPass)
				other.reform[F2, S2, F1, S1, V, EC, U](this)(reform.swap, passCount.++).swap
			else
				throw new IllegalArgumentException(
					"Cannot pass reform " + reform + " from " + this + " to " + other + ": reached pass limit " +
						passCount + "."
				)

		/* An alternate implementation could use a dedicated Reformer class which doesn't accept any constructor
		 * parameters, returning instead a function accepting
		 * (Reform, passCount, leftResult, rightResult, SQLSpelling). This would allow `reformer` to be a constant,
		 * and multiple dispatch could happen between the reformers directly, without `reform` methods
		 * in SQLExpression. Expressions implemented as private classes implementing a public interface
		 * could even extend `Reformer` directly, rather than being wrapped in another object.
		 * This reduces the overhead as well as API clutter in SQLExpression. The downside are
		 *   1. the function returned/implemented by the visitor is generic, as it depends on the type of the other
		 *      expression. This means that either the reformer must be generic, or we need generic functions of Scala 3.
		 *      In the former case, the reformer would either have to be created each time when an expression pair
		 *      is reformed, or we would have to cast the same instance each time `expr.reformer` is called.
		 *   1. The function returned by the reformer takes multiple arguments, so each dispatch method
		 *      of the visitor becomes much more verbose comparing to passing all those arguments once to its constructor.
		 *   1. It requires each expression with any reforming capability to provide an extra class, rather than
		 *      implementing it directly within `SQLExpression.reform`.
		 */
		/** A visitor adjusting the columns of this expression when matched with another `SQLExpression[E, C, X]`
		  * (unified with this expression to the same type of `U`). It is used by method
		  * [[net.noresttherein.oldsql.sql.SQLExpression.reform[E<:RowProduct,C>:Grouped<:GlobalScope,X,U]*, reform]]
		  * if it cannot or wishes not to pass the responsibility to `other`.
		  *
		  * On the level of `SQLExpression`, no actual reforming is really possible. For this reason,
		  * the default implementation of [[net.noresttherein.oldsql.sql.SQLExpression.BaseReformer BaseReformer]]
		  * overrides only methods for [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
		  * and [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]], delegating them
		  * to the appropriate `reform` method of this instance. However, if `passesAllowed > 0`, the reformer
		  * can still delegate to `other.reform`, which the default implementation does for all other expression types.
		  *
		  * The exact behaviour will depend on individual implementations, but the standard policy is:
		  *   1. Any expression with a full read/write [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] can reform
		  *      an argument of [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]].
		  *   1. Tuple-like types aligning structurally with one another reform their corresponding element pairs.
		  *   1. [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] tries to reform the two ''select'' clauses
		  *      if the other expression is also a `SelectSQL`, or reform their ''select'' clause with the whole
		  *      of the other expression otherwise. Selects cannot be split into individual column expressions however,
		  *      and ''select'' clause unification in compound selects is another process, so this feature is mostly
		  *      related to ''insert ... select'' statements and similar.
		  *   1. Conversions reform the converted expression and the argument, simply stacking themselves with `leftResult`.
		  *   1. Component expressions using the same nominal [[net.noresttherein.oldsql.schema.Mapping Mapping]] type
		  *      can [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.alter alter]] their column sets
		  *      by including/excluding their optional columns. This case is explicitly handled by the `reform` strategy -
		  *      see documentation on provided implementations for specifics.
		  *   1. If other attempts fail (including by throwing an `Exception`) and `mayPass` is `true`,
		  *      then pass the responsibility to the other expression. Unlike with `reform` methods,
		  *      the reformer should first try to handle any case itself, and only pass control as a fallback,
		  *      but this is not a strict requirement.
		  *
		  * Typically, classes which support changing of their shape override this method to return a subclass
		  * of [[net.noresttherein.oldsql.sql.SQLExpression.BaseReformer BaseReformer]] (or an altogether different
		  * [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor ExpressionVisitor]]
		  * and override the method specific to this expression in order to compare themselves structurally
		  * with another expression of the same type.
		  *
		  * This method should not be called by other instances.
		  * @param other       another expression, located in an opposed position to this expression in formatted SQL.
		  *                    We use a conjunction type to specify the most specific form of
		  *                    [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]]
		  *                    mixed in by the expression, which defines the type to which it is reformed, assuming
		  *                    `rightResult` is an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]].
		  * @param reform      a strategy governing the whole process, delegated to for all subexpressions
		  *                    of this expression, instead of a method of `SQLExpression` directly. It is in particular
		  *                    responsible for defining the semantics of the bottom case of aligning
		  *                    two component expressions.
		  * @param passCount   A counter specifying how many times the control has passed between `this` and `other`
		  *                    by calling `other.reform`, without reducing the problem (changing the expression pairing
		  *                    to subexpressions). Every next call to `reform` must increase the counter: `passCount.++`.
		  *                    If `!passCount.mayPass`, the reformer cannot call
		  *                    [[net.noresttherein.oldsql.sql.SQLExpression.BaseReformer.pass pass]] or
		  *                    `other.reform(this)(reform, _)`, it ''must'' either perform reforming itself,
		  *                    or delegate to [[net.noresttherein.oldsql.sql.SQLExpression.BaseReformer.fallback fallback]]
		  *                    (`reform.`[[net.noresttherein.oldsql.sql.mechanics.Reform.fallback fallback]]).
		  *                    As a guidline, normal expressions should not attempt reforming immediately, but wait
		  *                    at least for the other expression to also have seen the pair. This can be checked with
		  *                    `passCount.`[[net.noresttherein.oldsql.sql.mechanics.Reform.PassCount.otherHasPassed otherHasPassed]].
		  * @param leftResult  a conversion (possibly
		  *                    [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]]
		  *                    adapting this expression to a more generic, or posessing higher precision, type `U`,
		  *                    common with the other expression.
		  * @param rightResult a conversion (possibly
		  *                    [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]]
		  *                    adapting this expression to a more generic, or posessing higher precision, type `U`,
		  *                    common with the other expression.
		  * @param spelling    A strategy used to format both expressions as SQL which initiated the reforming.
		  *                    It provides [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
		  *                    methods as well as
		  *                    the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]/context
		  *                    in which the expressions are used, which defines the default column sets of
		  *                    component expressions.
		  * @return a [[net.noresttherein.oldsql.sql.SQLExpression.BaseReformer]] created with the provided arguments.
		  */ //fixme: docs out of date
		protected def reformer[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                       EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                      (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                      (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
		                                spelling :SQLSpelling)
				:SpecificExpressionVisitor
				 [F2, S2, V2, (leftResult.SQLResult[F1, S1, EC[U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
			new BaseReformer[F1, S1, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](
				other)(reform, passCount)(leftResult, rightResult, spelling)

		/* The problem with this implementation is that within a method for a particular expression class,
		 * we have e :ConvertibleSQL[F2, S2, V2, EC3] - not EC2. This means that if we do something to e
		 * and obtain another expression e2 :EC3[V2], then rightResult(e2) is SQLResult[F2, S2, EC3[V2]] -
		 * not SQLResultF2, S2, EC2[V2]]! We would have to enforce EC3[v] <: EC2[v], but,
		 * while such a visitor is possible, it could not delegate to cases for superclasses,
		 * unless we use reform.prohibitReformRight and always return reformRight(other),
		 * or the superclass is also <: ConvertibleSQL[F2, S2, V2, EC2]. This is fortunately the case
		 * for most expressions, simply because they convert only to SQLExpression[F2, S2, v],
		 * but the whole visitor implementation would be heavily dedicated to this single use case
		 * and likely methods for all classes extending ConvertingTemplate would have to remain abstract.
		 * I don't have the strength to do it, as the whole mechanism may be refactored if we require
		 * that aligned expressions share the same value type (rather than rely on SQLConversions to make it happen).
		 *
		 * For now we just force cast the result, we have only to remember that we can only do it if the class
		 * for the handled case doesn't have any subclasses which mix in ConvertingTemplate with a narrower EC type,
		 * unless we handle those cases, too - without delegating to the case for the more generic class.
		 * Currently only MappingSQL and LValueSQL implement their own conversions, and maybe some terms.
		 */
		/** Implements the strategy of adjusting the column sets of this expression and some other `SQLExpression[E, C, X]`,
		  * unified to a common wider type `U`. This visitor is used by method
		  * [[net.noresttherein.oldsql.sql.SQLExpression.reform reform]], if it may not, or chooses not to,
		  * pass the responsibility to the other expression. At this level, with no specific information about
		  * the form of this expression, the handled cases simply delegate the calls for some expression types
		  * to other methods. The handled cases include:
		  *   1. [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] - delegates to `this.reform(e)`
		  *      (the overloaded variant specific to `ComponentSQL`).
		  *   1. [[net.noresttherein.oldsql.sql.ast.LValueSQL.ConvertedLValueSQL ConvertedLValueSQL]] -
		  *      delegates to `this.reform(e)` (the overloaded variant for
		  *      [[net.noresttherein.oldsql.sql.ast.LValueSQL LValueSQL]])
		  *   1. [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] -
		  *      delegates to `e.reform(this, ...,. mayPass = false)`.
		  *   1. [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]] -
		  *      delegates to `e.reform(this, ..., mayPass = false)`.
		  *   1. [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] - if the column counts of both expressions
		  *      after conversions are equal, simply returns the converted expressions; otherwise, if any of the conversions
		  *      produces a non [[net.noresttherein.oldsql.sql.SQLExpression.Lift.isDerived derived]] expression,
		  *      then the method delegates to `reform(leftResult(this), rightResult(other))`. In all other cases,
		  *      a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
		  *      is thrown.
		  *
		  * This strategy provides sensible defaults, assuring this visitor fares no worse than the specialized `reform`
		  * methods of `SQLExpression`, and covers expression types with forms directly derived from other expression.
		  * Subclasses of `SQLExpression` can extend this class with additional cases, returning the specialized visitor
		  * from method [[net.noresttherein.oldsql.sql.SQLExpression.reformer reformer]].
		  */
		protected class BaseReformer[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                             EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U,
		                             LR[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U],
		                             RR[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U]]
		                            (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                            (implicit leftResult  :SQLTransformation[V, U]#Into[LR],
		                                      rightResult :SQLTransformation[V2, U]#Into[RR], spelling :SQLSpelling)
			extends CaseSpecificExpression[F2, S2, V2, (LR[F1, S1, EC[U]], RR[F2, S2, EC2[U]])]
		{
			protected type Argument       = ConvertibleSQL[F2, S2, V2, EC2]
			protected type Left           = LR[F1, S1, EC[U]]
			protected type Right          = RR[F2, S2, EC2[U]]
			protected type Result         = (LR[F1, S1, EC[U]], RR[F2, S2, EC2[U]])
			protected type LeftResult[X]  = SQLTransformation[X, U]#Into[LR]
			protected type RightResult[X] = SQLTransformation[X, U]#Into[RR]
			@inline protected final def left    :LR[F1, S1, EC[U]]  = leftResult(self :ConvertibleSQL[F1, S1, V, EC])
			@inline protected final def right   :RR[F2, S2, EC2[U]] = rightResult(other)
			@inline protected final def mayPass :Boolean = passCount.mayPass

			/** Invokes the `reform` method of `other`. */
			protected def pass :(LR[F1, S1, EC[U]], RR[F2, S2, EC2[U]]) = {
				assert(passCount.mayPass,
					"Exceeded the limit " + passCount + " for passing reforming between expressions " + self +
						" and " + other + "."
				)
				other.`->reform`[F2, S2, F1, S1, V, EC, U](self)(reform.swap, passCount.++).swap
			}

			/** Passes the control to `other`, unless the pass limit has been reached, in which case calls `reform.fallback(self, other)` */
			final protected def fallback :(LR[F1, S1, EC[U]], RR[F2, S2, EC2[U]]) =
				if (mayPass)
					pass
				else
					reform.fallback(self, other)(leftResult, rightResult, spelling)

			final protected def fallbackGuard(msg: => String) :Result =
				try { fallback } catch {
					case e :OldSQLException => throw e.addInfo(msg)
				}
			final protected def fallbackAfter(e :Exception) :Result =
				try { fallback } catch {
					case e2 :OldSQLException =>
						e.addSuppressed(e2)
						throw e
				}

			@inline final def asIs :Result = (leftResult(self :ConvertibleSQL[F1, S1, V, EC]), rightResult(other))

			protected def thisExpression :EC[V] = self

			override def expression(e :SQLExpression[F2, S2, V2]) :(LR[F1, S1, EC[U]], RR[F2, S2, EC2[U]]) = fallback
		}

		//todo: move to SQLExpression
		/** A visitor pattern callback in which this expression calls the method of `visitor` most appropriate to its type. */
		protected def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y

		protected[sql] def `->visit`[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y = visit(visitor)

		//consider: renaming to dispatch; a value parameter specifying if S is local or global
		/** A visitor pattern callback in which this expression calls the method of `visitor` most appropriate to its type. */
		protected def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, Y]) :Y[S, V]

		protected[sql] def `->visit`[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] = visit(visitor)
	}


	/** An alias for `SQLExpression[F, S, V]` using a type constructor `EC[V]`. */
	type ConvertibleSQL[-F <: RowProduct, -S >: Grouped <: Single, V,
	                    +EC[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, EC]] =
		SQLExpression[F, S, V] with ConvertingTemplate[F, S, V, EC]


	/** $GroundingDoc
	  *
	  * This trait is contravariant in `F` as it is the case for most expressions, but type constructor `EC`
	  * is invariant in `F` to allow classes invariant in `F` to also extend this trait.
	  * In order to declare method [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.anchor anchor]],
	  * a forth type parameter `Expr` is introduced, which, for concrete classes, is expected to e `Expr =:= EC[F]`,
	  * because of impossibility of returning `EC[F]`. For convenience, two type aliases are introduced
	  * which get rid of this type parameter by unifying the variance of the expression and `EC`:
	  *   - [[net.noresttherein.oldsql.sql.SQLExpression.VariantGroundingTemplate VariantGroundingTemplate]], and
	  *   - [[net.noresttherein.oldsql.sql.SQLExpression.InvariantGroundingTemplate InvariantGroundingTemplate]].
	  * @tparam F    $F
	  * @tparam S    $S
	  * @tparam V    $V
	  * @tparam Cons $GroundedInCons
	  * @tparam Same $Same
	  * @define Same The self type of this expression, normally a subtype of `EC[F]`, which condition is impossible
	  *              to enforce here. Used as the return type
	  *              of [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.anchor anchor]].
	  * @define GroundedInCons A type constructor for a supertype of this expression: `this.type <:< Expr[F]`.
	  */
	trait GroundingTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                        +Cons[f <: RowProduct] <: SQLExpression[f, S, V], +Same <: SQLExpression[F, S, V]]
	{
		//These two are declared here despite generic return type because we often override it in extending template classes..
		/** Answers if this expression is applicable to the [[net.noresttherein.oldsql.sql.SQLExpression.Single single row]]
		  * scope. If true, it can be safely cast to `SQLExpression[F, Single, V]`
		  * (and [[net.noresttherein.oldsql.sql.SingleSQL SingleSQL]]`[F, V]`).
		  * @see [[net.noresttherein.oldsql.sql.SQLExpression.asSingleRow]]
		  */
		def isSingleRow :Boolean = false

		/** Returns this expression in an option if it is a [[net.noresttherein.oldsql.sql.SingleSQL single row]]
		  * expression or `None` otherwise.
		  */
		def asSingleRow :Option[SingleSQL[F, V]]

		/** An expression is ''anchored'' if it does not contain
		  * a [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] as a subexpression. Loose components
		  * are placeholder expression adapters of [[net.noresttherein.oldsql.schema.Mapping Mapping]] instances
		  * with a `RowProduct` subtype as their `Origin` type. They are not tied to any particular relation in `F`,
		  * which makes converting any enclosing expression into valid SQL impossible.
		  * @see [[net.noresttherein.oldsql.sql.SQLExpression.anchor]]
		  * @see [[net.noresttherein.oldsql.sql.SQLExpression.isAnchored(from:F)*]]
		  */ //consider: flipping it to isFree in order to avoid conflict with isGround
		def isAnchored :Boolean

		/** An expression is ''anchored in `clause :F`'' if it doesn't contain a
		  * [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] as a subexpression (`this.isAnchored`)
		  * and all [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] subexpressions have as their
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relations]] instances equal to those
		  * present in `clause` at the corresponding positions:
		  *   1. An [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]] (a literal or a parameter) is always anchored
		  *      in any clause;
		  *   1. A [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] is not anchored in any clause;
		  *   1. A [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] `c` is anchored in `clause`
		  *      ''ìff'' `c.relation == clause.fullTableStack(c.origin.index).relation`;
		  *   1. A [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] `s` is anchored in `clause` ''iff''
		  *      `s.from.outer == clause`;
		  *   1. An [[net.noresttherein.oldsql.sql.ast.AggregateSQL AggregateSQL]] `g`is anchored in `clause` ''iff''
		  *      `clause.isInstanceOf[AggregateClause]` and `g.`[[net.noresttherein.oldsql.sql.ast.AggregateSQL.arg arg]]
		  *      is anchored in `clause.fromClause`;
		  *   1. A [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]] is anchored in `clause` ''iff''
		  *      all its parts are anchored in it.
		  *
		  * While any expression anchored in some clause could be translated to SQL based on their carried information,
		  * an expression used in a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] is always rendered
		  * in the context of relations listed by its ''from'' clause, rather than the one the expression is anchored in.
		  * While the types will always match, different relations can possibly use the same mapping, and any table can have
		  * its column set altered - such differences are not reflected in the type of the expression, but
		  * affect the form of the generated SQL.
		  */
		def isAnchored(from :F) :Boolean

		/** Replaces all occurrences of [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]
		  * with a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] using the relation at
		  * the given position in `F`, (the first relation in its `RowProduct` type parameter) as its parent.
		  * Additionally, all `ComponentSQL` subexpressions which are not anchored in `from` (or a clause derived
		  * from `from` according to the semantics defined by the context for expressions not based on `F`, such
		  * as [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions), are anchored in `from`
		  * by replacing their [[net.noresttherein.oldsql.schema.Relation Relation]] for one at the same offset
		  * in `from` (or an appropriate derived clause), but keeping
		  * any [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.alter alterations]]
		  * made on the expression level.
		  * Any alterations made on the relation [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.alter level]]
		  * are replaced with those defined by the new relation.
		  */ //consider: renaming to ground
		def anchor(from :F) :Same

		/** The value of this expression, if it can be computed without an access to the database. */
		//Opt is inconsistent with asGlobal :Option[_]
		def groundValue :Opt[V] //todo: rename to eval; //todo: functions with in-application implementations (concatenation, current_date, etc.)

		/** An expression is a ''ground'' expression if it does not reference any tables in its ''from'' clause
		  * type parameters (or grouping relations, etc.). Such expressions need not to be constant, or computable
		  * by the application, as they may call stored procedures or include ground queries as their subterms.
		  * This property is `true` ''iff'' this expression can be safely cast to `SQLExpression[RowProduct, S, V]`.
		  * Being a ground expression implies being
		  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.isAnchored anchored]].
		  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.asGround asGround]]
		  */
		def isGround :Boolean

		/** Returns this expression in [[Some]] if it can be safely cast up to `SQLExpression[RowProduct, S, V]]`,
		  * i.e. it is [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.isGround asGround]].
		  */
		def asGround :Option[Cons[RowProduct]] =
			if (isGround) Some(this.asInstanceOf[Cons[RowProduct]]) else None

		//def anchor is not here first because Expr is invariant (because JoinedRelation), and second
		//  because JoinedTable, etc. are anchored to JoinedRelation, not themselves.

		/** Upcasts this expression to the base ''from'' clause `E <: F`, using only implicit evidence about the subtype
		  * relation rather than explicit lower type bound (which would be an identity cast in Scala).
		  *///consider: renaming to from/groundIn
		def basedOn[E <: RowProduct](implicit subtype :E <:< F) :Cons[E] = this.asInstanceOf[Cons[E]] //cast can be eliminated with sugar

		//todo: this isn't that different from anchor, as the result will be anchored in `base`. I say we rename both to groundIn
		/** Treat this expression as an expression of a ''from'' clause containing this clause as its prefix.
		  * The expansion is limited only to clauses representing the same select as this clause - no
		  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] 'joins' can occur in `E` after `F`.
		  * This method is thus applicable to a strictly smaller set of ''from'' clauses than
		  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.expand expand]],
		  * but is available for all expressions.
		  *///todo: return this if ext.length == 0
		def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Cons[E]

		/** Treat this expression as an expression based on a ''from'' clause expanding (i.e. containing additional tables)
		  * the clause `F` this expression is based on. This method is available only for global expressions, i.e. those
		  * which can occur inside any subselect of a select with the ''from'' clause `F`. This method has thus a wider
		  * range of applicable ''from'' clauses than
		  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate.basedOn basedOn]],
		  * but is limited only to expressions conforming to `SQLExpression[F, GlobalScope, V]`.
		  */
		def expand[U <: F, E <: RowProduct]
		          (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Cons[E]
	}


	/** $GroundingDoc
	  *
	  * Both this template and type constructor `Expr` are contravariant in their `F` type parameters,
	  * so this trait can only be used for those subclasses of `SQLExpression` which are also variant in `F`.
	  * For expression types invariant in `F`,
	  * see [[net.noresttherein.oldsql.sql.SQLExpression.InvariantGroundingTemplate InvariantGroundingTemplate]].
	  * @tparam F $F
	  * @tparam S $S
	  * @tparam V $V
	  * @tparam E $GroundedInCons
	  */
	type VariantGroundingTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                              +E[-f <: RowProduct] <: SQLExpression[f, S, V]] =
		GroundingTemplate[F, S, V, E, E[F]]


	/** $GroundingDoc
	  *
	  * Both this template and type constructor `Expr` are invariant in their `F` type parameters,
	  * so this trait can only be used for those subclasses of `SQLExpression` which are also invariant in `F`.
	  * For expression types variant in `F`,
	  * see [[net.noresttherein.oldsql.sql.SQLExpression.VariantGroundingTemplate VariantGroundingTemplate]].
	  * @tparam F $F
	  * @tparam S $S
	  * @tparam V $V
	  * @tparam E $GroundedInCons
	  */
	type InvariantGroundingTemplate[F <: RowProduct, -S >: Grouped <: Single, V,
	                                +E[f <: RowProduct] <: SQLExpression[f, S, V]] =
		GroundingTemplate[F, S, V, E, E[F]]


	//f in Expr must be contravariant for asGlobal, so not usable for JoinedRelation and other invariant types
	/** Convenience mix-in trait for SQL expressions with [[net.noresttherein.oldsql.sql.SQLExpression.Single Single]]
	  * [[net.noresttherein.oldsql.sql.SQLExpression.RowScope RowScope]].
	  * It preserves self type `Expr` given as the last type parameter by
	  * [[net.noresttherein.oldsql.sql.SQLExpression.SingleRowSQLTemplate.basedOn basedOn]] and
	  * [[net.noresttherein.oldsql.sql.SQLExpression.SingleRowSQLTemplate.expand expand]] methods.
	  * @tparam F    $F
	  * @tparam V    $V
	  * @tparam Expr $GroundedInCons
	  */
	trait SingleRowSQLTemplate[-F <: RowProduct, V, +Expr[-f <: RowProduct] <: SQLExpression[f, Single, V]]
		extends VariantGroundingTemplate[F, Single, V, Expr]
	{ this :Expr[F] =>
		override def isSingleRow = true
		override def asSingleRow :Option[Expr[F]] = Some(this)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Expr[E] =
			expand(base)(expansion.asExpandedBy, implicitly[Single <:< Single])
	}


	/** A mixin trait for ''ground'' expressions, i.e. those grounded in
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] itself.
	  * Implements all grounding methods.
	  */
	trait GroundSQLTemplate[V, +Same <: SQLExpression[RowProduct, Single, V]]
		extends SingleRowSQLTemplate[RowProduct, V, ({ type E[-_ <: RowProduct] = Same })#E]
	{ this :Same =>
		override def isGround   = true
		override def isAnchored = true
		override def isAnchored(from :RowProduct) = true
		override def anchor(from :RowProduct) :Same = this

		override def expand[U <: RowProduct, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single) :Same =
			this
	}


	implicit class ChainSQLExpressionExtension[F <: RowProduct, S >: Grouped <: Single, T <: Chain]
	                                          (private val self :SQLExpression[F, S, T])
		extends AnyVal
	{
		/** Expands this chain expression by appending another expression. */
		@inline def ~[O >: Grouped <: S, H](next :SQLExpression[F, O, H]) :SQLExpression[F, O, T ~ H] =
			self match {
				case tuple :ChainTuple[F, S, T] => tuple ~ next
				case _ => ChainSQL(self, next)
			}
	}


	@inline
	implicit def fromConvertingTemplate[F <: RowProduct, S >: Grouped <: Single, V, E <: SQLExpression[F, S, V]]
	                                   (e :ConvertingTemplate[F, S, V, SQLExpression.from[F]#rows[S]#E]) :E =
		e.toConvertibleSQL

	implicit def fromSQLExpressionSeq[F <: RowProduct, S >: Grouped <: Single, T]
	                                 (expressions :Seq[SQLExpression[F, S, T]]) :SeqSQL[F, S, T] =
		SeqSQL(expressions :_*)

	implicit def fromSQLExpressionChain[F <: RowProduct, S >: Grouped <: Single, E <: Chain, I <: Chain, L]
	                                   (expressions :E ~ SQLExpression[F, S, L])(implicit init :E => ChainTuple[F, S, I])
			:ChainTuple[F, S, I ~ L] =
		init(expressions.init) ~ expressions.last

	implicit def fromEmptyChain(empty: @~) :ChainTuple[RowProduct, Single, @~] = EmptyChain

	implicit def fromSQLExpressionListing //todo: either the same for LabeledValueSQL, or make ColumnSQL a LabeledValueSQL
	             [F <: RowProduct, S >: Grouped <: Single, E <: Listing, I <: Listing, K <: Label :ValueOf, L]
	             (expressions :E |~ (K :~ ColumnSQL[F, S, L]))(implicit init :E => LabeledSQL[F, S, I])
			:LabeledSQL[F, S, I |~ (K :~ L)] =
		init(expressions.init) |~ expressions.last

	implicit def fromEmptyListing(empty: @~) :LabeledSQL[RowProduct, Single, @~] = EmptyListing



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
	  * on any tables: the only bottom terms possible are [[net.noresttherein.oldsql.sql.ast.BoundParam bound]]
	  * and unbound parameters as well as [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]].
	  * This is useful particularly for tuple expressions representing sequences of parameters.
	  * {{{
	  *     SQLExpression.using[String][Int] { params => params.of[String].chained ~ params.of[Int] }
	  * }}}
	  * Alternatively, the expression for a tuple simply listing all parameters is returned by
	  * extension method [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.params params]]:
	  * {{{
	  *     val params = SQLExpression.using[String][Int][Long].params
	  *     params :ChainTuple[NoParams WithParam String WithParam Int WithParam Long, GlobalScope, @~ ~String~Int~Long]
	  * }}}
	  * @return an object implicitly convertible to a
	  *         [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams SQLExpressionParams]]`[FromSome WithParam P, @~ ~ P]`.
	  * @tparam P the type of the first parameter of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
	  *           with the unbound parameters used by the future SQL expression.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams]]
	  */ //consider: renaming to param
	def using[P] = new SQLExpressionParamDecl[NoParams, @~, P](NoParams, EmptyChain)

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
	  * on any tables: the only bottom terms possible are [[net.noresttherein.oldsql.sql.ast.BoundParam bound]]
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
	def using[N <: Label, P] = new SQLExpressionNamedParamDecl[NoParams, @~, N, P](NoParams, EmptyChain)


	/** An intermediate object adding a new [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P`
	  * to the `F &lt;: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]] domain serving as a base
	  * for a new [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], implicitly convertible to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams SQLExpressionParams]]`[F WithParam P, Ps ~ P]`.
	  * It has the following (extension) methods for adding additional parameters and creating an expression using them:
	  *   1. [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[P]* this]]`[Q]` -
	  *      adds a new parameter of type `Q` to the expression's domain: `F WithParam P WithParam Q`;
	  *   1. [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[N<:Label,P]* this]]`["Q", Q]` - adds
	  *      a new parameter named "Q" and of type `Q` to the expression's domain: `F WithParam P WithParam Q As "Q"`;
	  *   1. [[[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[E<:SQLExpression[F,Grouped,_]](args:JoinedMappings[F]=>E)* this]]]`(params => `''expr''`)` -
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
	final class SQLExpressionParamDecl[F <: ParamsRow[Ps], Ps <: Chain, P] private[SQLExpression]
	                                  (private val domain :F { type Generalized >: F <: RowProduct; type Self <: F },
	                                   private val params :ChainTuple[F, Single, Ps])

	object SQLExpressionParamDecl {
		implicit def domain[F <: ParamsRow[Ps], Ps <: Chain, P :SQLForm](param :SQLExpressionParamDecl[F, Ps, P])
				:SQLExpressionParams[F WithParam P, Ps ~ P] =
		{
			val domain = JoinParam(param.domain, ?:[P])
			new SQLExpressionParams(domain, param.params.expand[F, F WithParam P](domain) ~ domain.last)
		}
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
	  *   1. [[[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams.apply[E<:SQLExpression[F,Grouped,_]](args:JoinedMappings[F]=>E)* this]]]`(params => `''expr''`)` -
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
	final class SQLExpressionNamedParamDecl[F <: ParamsRow[Ps], Ps <: Chain, N <: Label, P] private[SQLExpression]
	                                       (private val domain :F { type Generalized >: F <: RowProduct; type Self <: F },
	                                        private val params :ChainTuple[F, Single, Ps])

	object SQLExpressionNamedParamDecl {
		implicit def domain[F <: ParamsRow[Ps], Ps <: Chain, N <: Label :ValueOf, P :SQLForm]
		                   (param :SQLExpressionNamedParamDecl[F, Ps, N, P])
				:SQLExpressionParams[F WithParam P As N, Ps ~ P] =
		{
			val domain = JoinParam(param.domain, ?:[N, P])
			new SQLExpressionParams(domain, param.params.basedOn[F, F WithParam P](domain) ~ domain.last)
		}
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
	class SQLExpressionParams[F <: ParamsRow[Ps], Ps <: Chain](
		domain :F { type Generalized >: F <: RowProduct; type Self <: F },
		/** A tuple of parameter expressions for every [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameter
		  * in clause `F`. Can be used as an argument for the simplest procedure call with the JDBC format of
		  * `call `''proc''`(?, ?, ..., ?)`.
		  */
		val params :ChainTuple[F, Single, Ps])
	{
		/** Add another [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P` to the parameters
		  * of the created [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
		  * @return an object implicitly convertible to a
		  *         [[net.noresttherein.oldsql.sql.SQLExpression.SQLExpressionParams SQLExpressionParams]]`[F WithParam P, Ps ~ P]`.
		  * @tparam P the type of the - currently last - parameter
		  *           of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] with the unbound parameters
		  *           used by the future SQL expression.
		  */
		def apply[P] :SQLExpressionParamDecl[F, Ps, P] = new SQLExpressionParamDecl[F, Ps, P](domain, params)

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
			new SQLExpressionNamedParamDecl[F, Ps, N, P](domain, params)

		/** Creates an SQL expression of any type, using the unbound parameters accessible as pseudo relations
		  * from the facade to the domain `F` with all previously declared unbound parameters.
		  */
		def apply[E <: SQLExpression[F, Grouped, _]](args :JoinedMappings[F] => E) :E =
			args(new JoinedMappings(domain))
	}




	/** Phantom type used to denote where an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] parameterized
	  * with it can be used. It is an alias for [[net.noresttherein.oldsql.sql.SQLExpression.Single Single]],
	  * which specifies that the expression can be used both in all SQL ''selects'' based on
	  * the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] type it is parameterized with it, as well
	  * as any of its subselects (dependent selects) by expanding it with method
	  * [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]] (and similar). Its subtype,
	  * [[net.noresttherein.oldsql.sql.SQLExpression.Grouped Grouped]], marks expressions which can be used
	  * only in the 'same' ''select'' - that is the one based on the `RowProduct` type it is parameterized with,
	  * as well as on any ''from'' clauses expanding that type without crossing
	  * a [[net.noresttherein.oldsql.sql.Subselect Subselect]] boundary, by method
	  * [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]] (and its overloads).
	  * Additionally, expressions of `Grouped` cannot be used in a ''where'' clause.
	  */
	type RowScope = Single //consider: renaming to SQLArity/SQLVisibility

	/** Default scope of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], signifying that it can be used
	  * solely within the ''select'' and ''having'' clauses for the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
	  * serving as the domain of the expression. Such expressions are illegal for subselects of the mentioned ''select'',
	  * that is they cannot be converted to another ''from'' clause `E` expanding the original clause `F`. This stands
	  * in contrast to the [[net.noresttherein.oldsql.sql.SQLExpression.Single Single]] scope, which is a supertype
	  * of this type, and which permits such usage. Purely local expressions are reserved for SQL aggregate functions:
	  * `count(*)` of an SQL ''select'' cannot be used as a part of another ''select''.
	  *
	  * Note that type `SQLExpression` is contravariant in scope, meaning `SQLExpression[F, Grouped, T]`
	  * is a supertype of `SQLExpression[F, Single, T]`, permitting the use of non-aggregate expressions
	  * in the local scope of a ''select'' as expected. Furthermore, if an `SQLExpression[F, Grouped, T]`,
	  * if actually contains an aggregate function (and not within a subselect),
	  * implies that `F <: `[[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]].
	  * @see [[net.noresttherein.oldsql.sql.GroupedSQL]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PartOf]]
	  * @see [[net.noresttherein.oldsql.sql.ast.AggregateSQL]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateClause]]
	  */ //reverse direction of subtyping is because type inference always picks the upper bound when instantiating free type variables.
	type Grouped <: Single //consider: renaming to Groups/Rows/RowGroup/Grouped/Multi/Group/Aggregate/GroupArity/Group/GroupScope/GroupDomain/Private; problem: AggregateSQL exists, GroupSQL is to close to GroupingSQL

	/** A phantom type used as the ''scope'' (second) type argument `S`
	  * of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] instances which do not contain any SQL aggregate
	  * functions as their subexpressions. Only such expressions can be used within a ''where'' clause
	  * of a [[net.noresttherein.oldsql.sql.FromClause FromClause]]. They can also be converted,
	  * using method [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]],
	  * to any [[net.noresttherein.oldsql.sql.RowProduct ''from'']] clause `E` expanding the clause `F`
	  * on which the expression is based, in particular ''subselect'' clauses.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.Grouped]]
	  * @see [[net.noresttherein.oldsql.sql.SingleSQL]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PartOf]]
	  */
	type Single //consider: renaming to Distinct/Rows/Row/SingleRow/Ungrouped/Individual/Discrete/RowArity/RowScope/RowDomain/Public (remember about GlobalBoolean etc.); problem: DistinctSQL may be better used for ColumnSQL

	//move it out somewhere to not cause binary incompatibility issues
	@inline private[oldsql] def IsSingle :Single <:< Single = implicitly[Single <:< Single]



	/** An upper type bound of all `SQLExpression[_, _, _]` instances, including proper bounds of the type parameters
	  * in its definition. Most subclasses of `SQLExpression` define their own `*` types, which are particularly
	  * useful for heavily parameterized types as well as those parameterized with type constructors (needing
	  * existential types).
	  */ //todo: in Scala 3 replace all types Xxx.* with AnyXxx
	//consider: Should we use Nothing or _ <: RowProduct? Matching against SQLExpression[Nothing, Grouped, _]
	//  may cause warnings
	type __ = SQLExpression[_ <: RowProduct, _ >: Grouped <: Single, _]
//	type * = SQLExpression[Nothing, Grouped, _]

	/** A curried type constructor for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
	  * and [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]].
	  */
	type from[-F <: RowProduct] = {
		type __ = SQLExpression[F, _ >: Grouped <: Single, _]
		type apply[-S >: Grouped <: Single, V] = SQLExpression[F, S, V]
		type rows[-S >: Grouped <: Single] = {
			type __ = SQLExpression[F, S, _]
//			type apply[V] = SQLExpression[F, S, V]
			type value[V] = SQLExpression[F, S, V]
			type E[V] = SQLExpression[F, S, V]
			type C[V] = ColumnSQL[F, S, V]
			type Expression[V] = SQLExpression[F, S, V]
			type Column[V]     = ColumnSQL[F, S, V]
		}
	}



	/** A type alias of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] used in cases
	  * when the actual value of the expression does not matter, but only its type.
	  * It is closely related to both [[net.noresttherein.oldsql.sql.RowShape RowShape]]
	  * and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], but enforces an additional level of compatibility
	  * between matching expressions, both statically and dynamically. The latter involves two expressions sharing
	  * not also the value type, but also additional information, such as column aliases, for example.
	  * The exact degree of similarity at which two expressions are considered to have the same layout is however
	  * undefined; in particular, replacing all `ColumnSQL` subexpressions
	  * with an [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] will, in most cases,
	  * be considered of a matching layout.
	  * @see [[net.noresttherein.oldsql.sql.RowShape]]
	  */
	type SQLShape[V] = SQLExpression[Nothing, Grouped, V]

/*
	object SQLShape {
		def apply[X, Y, Z](left :SQLLayout[X], right :SQLLayout[Y])
		                  (implicit unify :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) :SQLShape[Z] =
			(left, right) match {
				case (l :ComponentLValueSQL.Cast[Nothing, X] @unchecked, _) => l.to(unify.left)
				case (_, r :ComponentLValueSQL.Cast[Nothing, Y] @unchecked) => r.to(unify.right)
				case (l :MappingSQL.Cast[Nothing, X] @unchecked, _)         => l.to(unify.left)
				case (_, r :MappingSQL.Cast[Nothing, Y] @unchecked)         => r.to(unify.right)
				case (l :ColumnSQL[Nothing, _, X] @unchecked, _)            => l.to(unify.left)
				case (_, r :ColumnSQL[Nothing, _, Y] @unchecked)            => r.to(unify.right)
				case (l :CompositeSQL[Nothing, _, X] @unchecked, _)         => l.to(unify.left)
				case (_, r :CompositeSQL[Nothing, _, Y] @unchecked)         => r.to(unify.right)
				case (l :AlignedExpression[Nothing, _, X] @unchecked,
				      r :AlignedExpression[Nothing, _, V]@unchecked) =>
					new AlignedExpression(mergeLayouts(l.value, r.value), l.alignment)
				case (l :AlignedExpression[Nothing, _, V]@unchecked, _) =>
					new AlignedExpression(mergeLayouts(l.value, right), l.alignment)
				case (_, r :AlignedExpression[Nothing, _, V]@unchecked) =>
					new AlignedExpression(mergeLayouts(left, r.value), r.alignment)
				case _ => left.to(unify.left)
			}

		(left, right) match {
				case (_ :ComponentLValueSQL.*, _) => left.to(unify.left)
				case (_, _ :ComponentLValueSQL.*) => right.to(unify.right)
				case (_ :MappingSQL.*, _) => left.to(unify.left)
				case (_, _ :MappingSQL.*) => right.to(unify.right)
				case (_ :ColumnTerm[_], _ :ColumnSQL[_, _, _]) => right.to(unify.right)
				case (_ :ColumnSQL[_, _, _], _ :ColumnSQL[_, _, _]) => left.to(unify.left)
				case (QuerySQL(query, equiv), _) =>
					def lift[A](equiv :X =:= Rows[A]) = {
						val ident = equiv.substituteContra[({ type L[V] = Lift[Rows[A], V] })#L](Lift.self)
						Lift.toRow[A] andThen ident andThen unify.left vs unify.right
					}
					SQLLayout(query.selectClause, right)(lift(equiv), spelling)
				case (_, _ :QuerySQL[_, _]) => SQLLayout(right, left)(unify.swap, spelling)
				case (_ :SQLTerm[_], _) => right.to(unify.right)
				case (_, _ :SQLTerm[_]) => left.to(unify.left)
				case (l :CompositeSQL[_, _, X], r :CompositeSQL[_, _, Y]) =>
					???
				case _ => ???
			}
	}
*/


	//should we enforce E <: SQLExpression?
	implicit def canSelectAggregate[F <: FromSome, E, S](implicit result :CanSelectDirect[Aggregated[F], E, S])
			:CanSelect[F, E] { type Select = result.Select } =
		new CanSelect[F, E] {
			override type Select = result.Select
			override def apply(from :F, expr :E) = result(from.aggregate, expr)
		}
	//temporarily, as it erroneously conflicts with canTopSelect
	implicit def canParamSelect[F <: TopRow { type Complete = C; type Params = P },
	                            C <: TopRow { type Complete = C; type Params = P }, P, E, V, O >: C <: RowProduct]
	                           (implicit selectClauseType :E <:< SelectableSQL[O, Grouped, V],
	                            fromClauseType :F <:< TopRow { type Complete = C; type Params = P })
			:CanSelectDirect[F, E, Select[P, V]] =
		new CanSelectDef[F, E, Select[P, V]] {
			override def apply(from :F, expr :E) = expr paramSelectFrom from.complete
		}




//	trait ExpressionVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, EC[v] <: SQLExpression[F, S, v], -E <: EC[V]]]
//		extends ColumnVisitor[F, Y] with CompositeVisitor[F, Y] with MappingVisitor[F, Y]
//		   with QueryVisitor[F, Y] with TermVisitor[Y]
//	{
////		def apply(e :SQLExpression[F, S, X]) :Y[SQLExpression[F, S, X]] = e.visit(this)
//		def apply[S >: Grouped <: Single, V, Expr <: Cons[V], Cons[v] <: SQLExpression[F, S, v]]
//		         (e :ConvertingTemplate[F, S, V, Cons, Expr]) :Y[S, V, Cons, Expr] =
//			e.visit(this)
//
//		def expression[S >: Grouped <: Single, V](e :SQLExpression[F, S, V]) :Y[S, V, SQLExpression[F, S, V]]
//
//		protected def unhandled(e :SQLExpression[F, Grouped, _]) :Nothing =
//			throw new IllegalArgumentException(s"Can't map expression $e :${e.className} using $this.")
//
//		protected def unknown[E <: SQLExpression[F, Grouped, _]](e :E, clazz :Class[E]) :Nothing =
//			throw new IllegalArgumentException(
//				s"Can't map expression $e :${e.className} using $this - unknown subclass of ${clazz.getName}."
//			)
//
//		protected def unknown[E <: SQLExpression[F, Grouped, _] :ClassTag](e :E) :Nothing =
//			unknown(e, classTag[E].runtimeClass.castParam[E])
//
//		override def toString :String = this.localClassName
//	}
//
//	//supertraits MatchXxx must not extend any CaseYyyColumn
//	trait MatchExpression[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[F, S, v], -E <: EC[V]]]
//		extends ExpressionVisitor[F, Y] with CaseComposite[F, Y]
//		   with CaseComponent[F, Y] with CaseLooseComponent[F, Y] with MatchQuery[F, Y] with CaseTerm[Y]
//	{
//		override def column[S >: Grouped <: Single, V]
//		                   (e :ColumnSQL[F, S, V]) :Y[S, V, ColumnSQL[F, S, V]] = expression(e :SQLExpression[F, S, V])
//		//override because we don't extend MatchMapping/CaseMapping so that the LValueConversion case be grouped under CompositeSQL
//		override def columnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], V]
//		                          (e :ColumnMappingSQL[F, S, M, V]) :Y[S, V, ColumnMappingSQL[F, S, M, V]] = mapping(e)
//
//		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
//		                      (e :TypedComponentSQL[O, T, E, M, V, L])
//				:Y[Single, V, TypedComponentSQL[O, T, E, M, V, L]] =
//			mapping(e)
//
//		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[V, A], V]
//		                           (e :LooseComponent[O, M, V]) :Y[Single, V, LooseComponent[O, M, V]] =
//			mapping(e)
//
//		//handled as a composite expression
////		override def modifiedComponent[M[A] <: BaseMapping[X, A]](e :EditedComponentSQL[F, M, X]) :Y =
////			mapping(e)
//
//		//implemented here because we don't extend CaseQuery because it would override implementation from CaseComposite
//		override def select[R](e :SelectSQL[F, R]) :Y[Single, Rows[R], SelectSQL[F, R]] = query(e)
//	}
//
//	trait CaseExpression[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[F, S, v], -E <: EC[V]]]
//		extends MatchExpression[F, Y]
//	{
//		override def *(e :ColumnSQL[RowProduct, Grouped, Nothing])
//				:Y[Grouped, Nothing, ColumnSQL[RowProduct, Grouped, Nothing]] =
//			expression(e)
//
//		override def aggregate[D <: FromSome, X, V]
//		                      (e :AggregateSQL[D, F, X, V]) :Y[Grouped, V, AggregateSQL[D, F, X, V]] = expression(e)
//
//		override def composite[S >: Grouped <: Single, V]
//		                      (e :CompositeSQL[F, S, V]) :Y[S, V, CompositeSQL[F, S, V]] = expression(e)
//
//		override def mapping[S >: Grouped <: Single, M[O] <: MappingAt[O], V]
//		                    (e :MappingSQL[F, S, M, V]) :Y[S, V, MappingSQL[F, S, M, V]] =
//			expression(e)
//
//		override def query[R](e :QuerySQL[F, R]) :Y[Single, Rows[R], QuerySQL[F, R]] =
//			expression(e)
//
//		override def term[V](e :SQLTerm[V]) :Y[Single, V, SQLTerm[V]] = expression(e)
//	}
//
//	trait BaseExpressionVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CaseExpression[F, Y]
//	{
//		override def expression[S >: Grouped <: Single, V]
//		                       (e :SQLExpression[F, S, V]) :Y[S, V, SQLExpression[F, S, V]] = unhandled(e)
//	}
//
//
//
	/** A visitor specific to [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, X]`, returning
	  * a value of type `Y`, parameterized with the type of the visited expression specified as the type argument
	  * given to its most specific
	  * [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]] supertype.
	  * In most use cases, the return type `Y` is the same, regardless of the argument expression type, in which
	  * case type `[-_] Y` (or `({ type Res[-_] = Y })#Res`) is given as the last type argument.
	  *
	  * This is a less flexible alternative
	  * to [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor AnyExpressionVisitor]], as an instance
	  * is dedicated to a particular expression type and cannot be easily reused, typically requiring creation
	  * of another visitor if any subexpressions of the visited expression exist. Fixing the value type however allows
	  * to pass parameters to the constructor of an implementing class, whose correlation with the visited expression
	  * type will be preserved inside the callback methods, or implement algorithms which are limited to certain
	  * value types (for example, implementing Boolean logic or arithmetic), which is not possible with the former.
	  *
	  * This interface declares a method for every concrete
	  * expression class defined in this package, as well as the main (rather than mixin) abstract `trait`s open
	  * for extension. These methods are invoked in double dispatch
	  * [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor.apply apply]]`(expr)`/`expr.`[[net.noresttherein.oldsql.sql.SQLExpression.visit visit]]`(this)`.
	  * This makes for a large number of cases, making it
	  * a problem both for maintainers and implementors, who may not need to handle every individual type directly.
	  * To alleviate the former, this interface is built by recursively extending partial traits defining methods
	  * for visiting all subclasses of a given expression class. They typically form a one-to-one relationship
	  * with expression classes, with each expression declaring in its companion object (or in the same scope
	  * for inner classes) its own visitor, extending the visitor of its subclasses, essentially reversing
	  * the inheritance hierarchy of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
	  * Every public expression class/trait declares a visitor which is then extended by visitors
	  * for all direct superclasses of the expression.
	  *
	  * To alleviate the latter, two parallel trait families are introduced: `Case`''Expr'' and `Match`''Expr''.
	  * The former implements all visitor methods for all subclasses of an expression type (typically named
	  * ''Expr''`SQL`, `SQL`''Expr'' or ''Expr''`Expression`) by delegating to the method for the base ''Expr'' class
	  * (sometimes introduced by the trait if the expression is abstract), catching all subclasses in a single case.
	  * The latter is similar, but leaves all methods for direct subclasses of ''Expr'' not implemented;
	  * the methods for ''their'' subclasses delegate as in the former example. This in turn has the effect of matching
	  * against a smallest set of classes covering all concrete subclasses of ''Expr''.
	  *
	  * This scheme is partially motivated by multiple inheritance of expression types, in particular
	  * single column expressions extending both [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]
	  * and the multi-column variant, but other cases also exist.
	  * Multiple inheritance causes the cases for some of `Case`''Xxx'' traits overlap;
	  * [[net.noresttherein.oldsql.sql.SQLExpression.CaseSpecificExpression CaseExpression]] extends only those following
	  * the primary line of inheritance, but mixing in a `Case`-visitor one for another expression can change
	  * the delegation direction. For example, `CaseExpression` extends
	  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral.CaseSpecificLiteral CaseLiteral]],
	  * but not [[net.noresttherein.oldsql.sql.ast.MappingSQL.CaseSpecificMapping CaseMapping]], so, by default,
	  * [[net.noresttherein.oldsql.sql.ast.MappingLiteral MappingLiteral]] which extends both
	  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral SQLLiteral]] and
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]], will result in calling method `literal`
	  * of the visitor. Mixing in also `CaseMapping` after this trait will however divert the call to method `mapping`.
	  * As this trait is covariant in its return type, it is possible to parameterize mixin traits with a subtype
	  * of the type argument to `ExpressionVisitor`. In particular, mixing in
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.CaseSpecificColumn CaseColumn]] after `CaseExpression` will result
	  * in all column expressions eventually invoking method
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.SpecificColumnVisitor.column column]] on the visitor, returning
	  * a specialized instance (for example, a `ColumnSQL` for an argument `ColumnSQL` instead of
	  * more generic `SQLExpression`).
	  *
	  * Minor deviations from these rules exist, with an intent of flattening the hierarchy where an intermediate
	  * base class or trait is considered more an implementation mixin than a new expression type, for example
	  * `MatchComposite` includes all methods of `MatchCompositeColumn`.
	  *
	  * By mixing in a selection of the above traits, one has a good degree of freedom in selecting which parts
	  * of the `SQLExpression` hierarchy are handled in detail and which are glossed over.
	  * @tparam F the domain type of the visited expression (its first type parameter).
	  * @tparam S the scope of the visited expression (its second type parameter).
	  * @tparam V the value type of the visited expression (its third type parameter).
	  * @tparam R The return type of this visitor, applied to the type of the visited expression. More specifically,
	  *           [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]]`[F, S, X, Expr]`
	  *           returns `Y[Expr]` from its [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.visit visit]]
	  *           method. The type is contravariant in its type parameter to allow delegating calls to cases
	  *           for super types of an expression. Unfortunately, a bound of `SQLExpression[F, S, X]` cannot be
	  *           imposed on its type parameter, as it would make impossible to implement cases for
	  *           [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]] or any other expression using a constrained
	  *           or composite value type.
	  *
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.MatchSpecificExpression]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.CaseSpecificExpression]]
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.SpecificColumnVisitor]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor]]
	  */  //todo: rename to SpecificExpressionVisitor
	trait SpecificExpressionVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +R]
		extends SpecificColumnVisitor[F, S, V, R] with SpecificCompositeVisitor[F, S, V, R]
		   with SpecificMappingVisitor[F, S, V, R] with SpecificQueryVisitor[F, V, R] with SpecificTermVisitor[V, R]
	{
		def apply(e :SQLExpression[F, S, V]) :R = e.visit(this)

		def expression(e :SQLExpression[F, S, V]) :R

		protected def unhandled(e :SQLExpression[F, S, _]) :Nothing =
			throw new IllegalArgumentException("Can't map expression " + e + " :" + classNameMethods(e).className + " using " + this + ".")

		protected def unknown[E <: SQLExpression[F, S, _]](e :E, clazz :Class[E]) :Nothing =
			throw new IllegalArgumentException(
				s"Can't map expression $e :${e.className} using $this - unknown subclass of ${clazz.getName}."
			)

		protected def unknown[E <: SQLExpression[F, S, _] :ClassTag](e :E) :Nothing =
			unknown(e, classTag[E].runtimeClass.castParam[E])

		override def toString :String = this.localClassName
	}

	/** A `ExpressionVisitor` delegating all visitor calls to the methods specific to one of the direct `SQLExpression`
	  * subclasses: `term`, `composite`, `query`, `mapping`.
	  * @tparam F the domain type of the visited expression (its first type parameter).
	  * @tparam S the scope of the visited expression (its second type parameter).
	  * @tparam V the value type of the visited expression (its third type parameter).
	  * @tparam Y the return type of this visitor.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor]]
	  */
	trait MatchSpecificExpression[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificExpressionVisitor[F, S, V, Y] with CaseSpecificComposite[F, S, V, Y]
		   with CaseSpecificComponent[F, V, Y] with CaseSpecificLooseComponent[F, V, Y]
		   with MatchSpecificQuery[F, V, Y] with CaseSpecificTerm[V, Y]
	{
		override def column(e :ColumnSQL[F, S, V]) :Y = expression(e :SQLExpression[F, S, V])
		//overridden because we don't extend MatchMapping/CaseMapping so that LValueConversion and EditedLValueSQL cases
		// be grouped under CompositeSQL
		override def columnMapping[M[O] <: ColumnAt[O]](e :ColumnMappingSQL[F, S, M, V]) :Y = mapping(e)

		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], L <: RowProduct]
		                      (e :TypedComponentSQL[O, T, R, M, V, L]) :Y =
			mapping(e)

		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[V, A]](e :LooseComponent[O, M, V]) :Y =
			mapping(e)

		//handled as a composite expression
//		override def modifiedComponent[M[A] <: BaseMapping[X, A]](e :EditedComponentSQL[F, M, X]) :Y =
//			mapping(e)

		//implemented here because we don't extend CaseQuery because it would override implementation from CaseComposite
		override def select[R](e :SelectSQL[F, R])(implicit isRows :V =:= Rows[R]) :Y = query(e)
	}

	/** An `ExpressionVisitor` which delegates all the cases to the single `expression` method
	  * invoked eventually for every subexpression (SQL AST node). Used as a base class when only few cases
	  * need special handling.
	  * @tparam F the domain type of the visited expression (its first type parameter).
	  * @tparam S the scope of the visited expression (its second type parameter).
	  * @tparam V the value type of the visited expression (its third type parameter).
	  * @tparam R the return type of this visitor.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor]]
	  */
	trait CaseSpecificExpression[+F <: RowProduct, +S >: Grouped <: Single, V, +R]
		extends MatchSpecificExpression[F, S, V, R]
	{
		override def *(e :ColumnSQL[RowProduct, Grouped, Nothing])
		              (implicit conforms :ColumnSQL[RowProduct, Grouped, Nothing] <:< ColumnSQL[RowProduct, S, V]) :R =
			expression(e)

		override def aggregate[D <: FromSome, E >: F <: RowProduct, X]
		                      (e :AggregateSQL[D, E, X, V])
		                      (implicit conforms :AggregateSQL[D, E, X, V] <:< ColumnSQL[E, S, V]) :R =
			expression(e)

		override def composite(e :CompositeSQL[F, S, V]) :R = expression(e)
		override def mapping[M[O] <: MappingAt[O]](e :MappingSQL[F, S, M, V]) :R = expression(e)
		override def query[H](e :QuerySQL[F, H])(implicit isRows :V =:= Rows[H]) :R =
			expression(isRows.substituteContra(e))
		override def term(e :SQLTerm[V]) :R = expression(e)
	}

	trait BaseSpecificExpressionVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends CaseSpecificExpression[F, S, X, Y]
	{
		override def expression(e :SQLExpression[F, S, X]) :Y = unhandled(e)
	}


	/** A generic visitor for the class hierarchy of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]s
	  * over tables in clause `F`, which produces a value of `Y[S, X]`, where `X` is the value type
	  * of the mapped expression and `S` is its [[net.noresttherein.oldsql.sql.SQLExpression.RowScope scope]]
	  * type parameter. The generic nature of this trait - the fact that its methods accept expressions
	  * of any value type - make it particularly suited for recursive traversal of the expression structure,
	  * such as in [[net.noresttherein.oldsql.sql.mechanics.SQLScribe SQLScribe]] implementations.
	  * It is different from [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor ExpressionVisitor]]
	  * in that it accepts expressions of an arbitrary value type and scope, which allows its reuse and, in particular,
	  * application to subexpressions of the visited expression. The drawback is that some operations
	  * cannot be expressed in such a generic manner, especially in the absence of other parameters correlated
	  * with the expression's value type, hence the existence of both traits.
	  *
	  * This interface declares a method for every concrete
	  * expression class defined in this package, as well as the main (rather than mixin) abstract `trait`s.
	  * These methods are invoked in double dispatch
	  * [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor.apply apply]]`(expr)`/`expr.`[[net.noresttherein.oldsql.sql.SQLExpression.visit visit]]`(this)`.
	  * This makes for a large number of cases, making it
	  * a problem both for maintainers and implementors, who may not need to handle every individual type directly.
	  * To alleviate the former, this interface is built by recursively extending partial traits defining methods
	  * for visiting all subclasses of a given expression class. They typically form a one-to-one relationship
	  * with expression classes, with each expression declaring in its companion object (or in the same scope
	  * for inner classes) its own matcher, extending the matchers of its subclasses, essentially reversing
	  * the inheritance hierarchy of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
	  * Every public expression class/trait declares a visitor which is then extended by visitors
	  * for all direct superclasses of the expression.
	  *
	  * To alleviate the latter, two parallel trait families are introduced: `CaseAny`''Expr'' and `MatchAny`''Expr''.
	  * The former implements all visitor methods for all subclasses of an expression type (typically named
	  * ''Expr''`SQL`, `SQL`''Expr'' or ''Expr''`Expression`) by delegating to the method for the base ''Expr'' class
	  * (sometimes introduced by the trait if the expression is abstract), catching all subclasses in a single case.
	  * The latter is similar, but leaves all methods for direct subclasses of ''Expr'' not implemented;
	  * the methods for ''their'' subclasses delegate as in the former example. This in turn has the effect of matching
	  * against a smallest set of classes covering all concrete subclasses of ''Expr''.
	  *
	  * This scheme is partially motivated by multiple inheritance of expression types, in particular
	  * single column expressions extending both [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]
	  * and the multi-column variant, but other cases also exist.
	  * Multiple inheritance causes the cases for some of `CaseAny`''Xxx'' traits overlap;
	  * [[net.noresttherein.oldsql.sql.SQLExpression.CaseAnyExpression CaseAnyExpression]] extends only those following
	  * the primary line of inheritance, but mixing in a `CaseAny`-visitor one for another expression can change
	  * the delegation direction. For example, `CaseAnyExpression` extends
	  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral.CaseAnyLiteral CaseAnyLiteral]],
	  * but not [[net.noresttherein.oldsql.sql.ast.MappingSQL.CaseAnyMapping CaseAnyMapping]], so, by default,
	  * [[net.noresttherein.oldsql.sql.ast.MappingLiteral MappingLiteral]] which extends both
	  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral SQLLiteral]] and
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]], will result in calling method `literal`
	  * of the visitor. Mixing in also `CaseAnyMapping` after this trait will however divert the call to method `mapping`.
	  * As this trait is covariant in its return type `R`, it is possible to parameterize mixin traits with a subtype
	  * of the type argument to `AnyExpressionVisitor`. In particular, mixing in
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.CaseAnyColumn CaseAnyColumn]] after `CaseAnyExpression` will result
	  * in all column expressions eventually invoking method
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.AnyColumnVisitor.column column]] on the visitor, returning
	  * a specialized instance (for example, a `ColumnSQL` for an argument `ColumnSQL` instead of
	  * more generic `SQLExpression`).
	  *
	  * Minor deviations from these rules exist, with an intent of flattening the hierarchy where an intermediate
	  * base class or trait is considered more an implementation mixin than a new expression type, for example
	  * `MatchAnyComposite` includes all methods of `MatchAnyCompositeColumn`.
	  *
	  * By mixing in a selection of the above traits, one has a good degree of freedom in selecting which parts
	  * of the `SQLExpression` hierarchy are handled in detail and which are glossed over.
	  * @tparam F the domain type of the visited expression (its first type parameter).
	  * @tparam Y the type returned by this visitor; its first type parameter is the scope (second type parameter)
	  *           of the visited expression, while the second type parameter is the value type of the visitee.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.MatchAnyExpression]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.CaseAnyExpression]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.BaseAnyExpressionVisitor]]
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.AnyColumnVisitor]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLScribe]]
	  */ //todo: rename to ExpressionVisitor once the latter is renamed to SpecificExpressionVisitor
	trait AnyExpressionVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnVisitor[F, Y] with AnyCompositeVisitor[F, Y] with AnyMappingVisitor[F, Y]
		   with AnyQueryVisitor[F, Y] with AnyTermVisitor[Y]
	{
		def apply[S >: Grouped <: Single, V](e: SQLExpression[F, S, V]): Y[S, V] =
			e.visit(this)

		def expression[S >: Grouped <: Single, X](e :SQLExpression[F, S, X]) :Y[S, X]

		protected def unhandled(e :SQLExpression[F, _, _]) :Nothing =
			throw new IllegalExpressionException(s"Can't map expression $e :${e.getClass.getName} using $this.")

		protected def unknown[E <: SQLExpression[F, _, _]](e :E, clazz :Class[E]) :Nothing =
			throw new IllegalExpressionException(s"Can't map expression $e :${e.getClass.getName} using $this - " +
				                                 s"unexpected subclass of ${clazz.getName}.")

		protected def unknown[E <: SQLExpression[F, _, _] :ClassTag](e :E) :Nothing =
			throw new IllegalExpressionException(s"Can't map expression $e :${e.getClass.getName} using $this - " +
			                                     s"unexpected subclass of ${implicitly[ClassTag[E]].runtimeClass.getName}.")

		override def toString :String = this.localClassName
	}

	/** A `AnyExpressionVisitor` delegating all visitor calls to the methods specific to one of the direct `SQLExpression`
	  * subclasses: `term`, `composite`, `query`, `mapping`.
	  * @tparam F the domain type of the visited expression (its first type parameter).
	  * @tparam Y the type returned by this visitor; its first type parameter is the scope (second type parameter)
	  *           of the visited expression, while the second type parameter is the value type of the visitee.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor]]
	  */
	trait MatchAnyExpression[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnyExpressionVisitor[F, Y]
		with CaseAnyComposite[F, Y] with CaseAnyComponent[F, Y] with CaseAnyLooseComponent[F, Y]
		with MatchAnyQuery[F, Y] with CaseAnyTerm[Y]
	{
		override def column[S >: Grouped <: Single, X](e :ColumnSQL[F, S, X]) :Y[S, X] =
			expression(e :SQLExpression[F, S, X])
		//override because we don't extend MatchMapping/CaseMapping so that the LValueConversion case be grouped under CompositeSQL
		override def columnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], V]
		                          (e :ColumnMappingSQL[F, S, M, V]) :Y[S, V] =
			mapping(e)

		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
		                      (e :TypedComponentSQL[O, T, R, M, V, L]) :Y[Single, V] =
			mapping(e)

		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[V, A], V]
		                           (e :LooseComponent[O, M, V]) :Y[Single, V] =
			mapping(e)

//		override def modifiedComponent[M[A] <: BaseMapping[X, A], X](e :EditedComponentSQL[F, M, X]) :Y[GlobalScope, X] =
//			mapping(e)

		//overridden here because we don't extend CaseAnyQuery because it would override implementation from CaseAnyComposite
		override def select[V](e :SelectSQL[F, V]) :Y[Single, Rows[V]] = query(e)
	}

	/** An `AnyExpressionVisitor` which delegates all the cases to the single `expression` method
	  * invoked for every subexpression (SQL AST node). Used as a base class when only few cases need special handling.
	  * @tparam F the domain type of the visited expression (its first type parameter).
	  * @tparam Y the type returned by this visitor; its first type parameter is the scope (second type parameter)
	  *           of the visited expression, while the second type parameter is the value type of the visitee.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor]]
	  */
	trait CaseAnyExpression[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyExpression[F, Y] {
		override def *(e :ColumnSQL[RowProduct, Grouped, Nothing]) :Y[Grouped, Nothing] =
			expression[Grouped, Nothing](e :SQLExpression[RowProduct, Grouped, Nothing])

		override def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[Grouped, V] =
			expression(e :SQLExpression[F, Grouped, V])

		override def composite[S >: Grouped <: Single, X](e: CompositeSQL[F, S, X]): Y[S, X] =
			expression(e)

		override def mapping[S >: Grouped <: Single, M[O] <: MappingAt[O], V]
		                    (e :MappingSQL[F, S, M, V]) :Y[S, V] =
			expression(e)

		override def query[V](e :QuerySQL[F, V]) :Y[Single, Rows[V]] = expression(e)
		override def term[X](e: SQLTerm[X]): Y[Single, X] = expression(e)
	}

	trait BaseAnyExpressionVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends CaseAnyExpression[F, Y]
	{
		override def expression[S >: Grouped <: Single, X](e: SQLExpression[F, S, X]): Y[S, X] = unhandled(e)
	}

	object AnyExpressionVisitor {
		type Result[F <: RowProduct] = {
			type E[-S >: Grouped <: Single, V] = SQLExpression[F, S, V]
			type C[-S >: Grouped <: Single, V] = ColumnSQL[F, S, V]
			type Expression[-S >: Grouped <: Single, V] = SQLExpression[F, S, V]
			type Column[-S >: Grouped <: Single, V] = ColumnSQL[F, S, V]
			type LValue[M[O] <: MappingAt[O], V] = LValueSQL[F, M, V]
			type ColumnLValue[M[O] <: BaseColumn[_, O], V] = ColumnLValueSQL[F, M, V]
		}
	}

}


package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.{Opt, PassedArray}
import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ColumnConvertingOps, ColumnConvertingTemplate, ColumnGroundingOps, SpecificColumnVisitor, VariantColumnGroundingTemplate}
import net.noresttherein.oldsql.sql.RowProduct.{Complete, GroundRow, NonEmptyRow, SubselectOf, TopRow}
import net.noresttherein.oldsql.sql.SQLBoolean.{False, True}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingTemplate, GroundingTemplate, Grouped, ReorderingTemplate, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.{denullify, AdaptedColumnSQL, AggregateSQL, AliasedSQL, AndSQL, ArithmeticSQL, BetweenSQL, ColumnMappingSQL, ColumnQuery, ColumnTerm, CompositeColumnSQL, ConcatSQL, ConvertedColumnSQL, IndexedSQL, InSQL, LikeSQL, LooseColumn, NotSQL, OrSQL, QuerySQL, SelectableSQL, SelectColumn, SelectSQL, SeqSQL, SQLNull, XorSQL}
import net.noresttherein.oldsql.sql.ast.AggregateSQL.{AnyAggregateVisitor, CaseAnyAggregate, CaseSpecificAggregate, SpecificAggregateVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.{AnyColumnMappingVisitor, SpecificColumnMappingVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnQuery.{AnyColumnQueryVisitor, CaseAnyColumnQuery, CaseSpecificColumnQuery, SpecificColumnQueryVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnTerm.{AnyColumnTermVisitor, CaseAnyColumnTerm, CaseSpecificColumnTerm, SpecificColumnTermVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.{AnyCompositeColumnVisitor, CaseAnyCompositeColumn, CaseSpecificCompositeColumn, SpecificCompositeColumnVisitor}
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL.{CaseAnyColumnComponent, CaseSpecificColumnComponent}
import net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledColumnSQL
import net.noresttherein.oldsql.sql.ast.LooseColumn.{CaseAnyLooseColumn, CaseSpecificLooseColumn}
import net.noresttherein.oldsql.sql.ast.QuerySQL.Rows
import net.noresttherein.oldsql.sql.ast.SelectColumn.{SubselectColumn, TopSelectColumn}
import net.noresttherein.oldsql.sql.mechanics.{=~=, AlignableColumns, Interoperable, Reform, SpelledSQL, SQLAdaptation, SQLConversion, SQLNumber, SQLOrdering, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.CanSelect.{CanSelectDef, CanSelectDirect}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions






//todo: rename to SQLColumn, use ColumnSQL for ColumnComponentSQL, and ColumnComponentSQL for GenericColumnComponentSQL.
/** An [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] which can be represented in SQL by a single, atomic
  * value assignable to a column, rather than a tuple or a result set. Many methods from `SQLExpression`
  * are overridden in this trait in order to narrow their result type to `ColumnSQL`, but otherwise their semantics
  * remain the same.
  *
  * There is an implicit conversion from `ColumnSQL[F, _, V]` for any value type `V` having
  * the [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]] type class and
  * `F <: `[[net.noresttherein.oldsql.sql.FromSome FromSome]] into
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLExtension ColumnSQLExtension]] providing extension methods
  * for applying SQL aggregate functions such as [[net.noresttherein.oldsql.sql.AggregateFunction.Count COUNT]]
  * or [[net.noresttherein.oldsql.sql.AggregateFunction.Avg AVG]] to this expression.
  * @tparam F $F
  * @tparam S $S
  * @tparam V $V
  * @see [[net.noresttherein.oldsql.sql.GroupedColumn]]
  * @see [[net.noresttherein.oldsql.sql.SingleColumn]]
  * @see [[net.noresttherein.oldsql.sql.SQLBoolean]]
  */ //consider: renaming to DistinctSQL
trait ColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
	extends SelectableSQL[F, S, V] //consider: not extending ColumnConvertingTemplate to allow reforming to multi columns
//	   with LabeledColumnValueSQL[F, S, V]
	   with ColumnConvertingOps[F, S, V, SQLExpression.from[F]#rows[S]#C]
	   with ColumnGroundingOps[F, S, V, ({ type E[-f <: RowProduct] = ColumnSQL[f, S, V] })#E, ColumnSQL[F, S, V]]
{
//todo: priority (binding strength to avoid unnecessary usage of '(', ')'

	/** Provides a name to use in the 'as' clause appended to this expression, if it is used in a ''select'' clause. */
	def as(alias :String) :ColumnSQL[F, S, V] = new AliasedSQL(this, alias)

	/** Attaches a label to this column in the form of a `String` literal type `N` for the purpose of indexing it
	  * inside an indexed SQL [[net.noresttherein.oldsql.sql.ast.IndexedSQL tuple]], allowing to access
	  * it later in a type safe manner. At the same time, the label is used as an alias for the column
	  * (replacing any previous alias) for the `as` clause appended to this column as in
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.as as]] method.
	  */
	def @:[N <: Label](alias :N) :LabeledColumnSQL[F, S, N, V] =
		LabeledColumnSQL(alias, this)

	/** Joins this expression and another expression in a logical conjunction represented by the `AND` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`.
	  * It recognizes [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] `true`, `false`
	  * and `null`, reducing the result according to the rules of ternary logic.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise a `ColumnSQL` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def && [E <: F, O >: Grouped <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
			case SQLNull() => other
			case True => to[Boolean]
			case False => other
			case _ => AndSQL(to[Boolean]) && denullify(other)
		}

	/** Joins this expression and another expression in a logical disjunction represented by the `OR` operator in SQL.
	  * This method is available only if the value type of this expression is `Boolean`.
	  * It recognizes [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] `true`, `false`
	  * and `null`, reducing the result according to the rules of ternary logic.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise a `ColumnSQL` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def || [E <: F, O >: Grouped <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
			case SQLNull() => other
			case True => other
			case False => to[Boolean]
			case _ => OrSQL(to[Boolean]) || denullify(other)
		}

	/** Joins this expression and another expression in a logical exclusive disjunction represented by the `XOR`
	  * operator in SQL. This method is available only if the value type of this expression is `Boolean`.
	  * It recognizes [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] `true`, `false`
	  * and `null`, reducing the result according to the rules of ternary logic.
	  * @param other any other SQL expression with `Boolean` value type, to use as the right (second) operand.
	  * @return if either `this` or `that` is the SQL [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]] literal,
	  *         than `SQLNull[Boolean]`. Otherwise a `ColumnSQL` based on
	  *         a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] being the greatest lower bound of the bases
	  *         of `this` and `that`, and the [[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
	  *         of which is the intersection of the scopes of the two expressions.
	  */
	def ^  [E<: F, O >: Grouped <: S]
	       (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
			case SQLNull() => SQLNull[Boolean]
			case _ => XorSQL(to[Boolean]) ^ denullify(other)
		}

	/** An SQL expression for `NOT this`. This method is available only if the value type of this expression is `Boolean`.
	  * If this expression is a [[net.noresttherein.oldsql.sql.ast.SQLLiteral literal]] or
	  * [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]], the result will be also a literal reduced according to
	  * ternary logic.
	  */
	def unary_![E <: F, O >: Grouped <: S](implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		NotSQL(to[Boolean])


	//todo: methods which accept TypedColumn and literals, as well as those creating bound parameters.
	/** Checks if the value of this expression falls between the given bounds using SQL ''between'' opertor. */
	def between[E <: F, O >: Grouped <: S]
	           (bounds :(ColumnSQL[E, O, V], ColumnSQL[E, O, V]))(implicit ordering :SQLOrdering[V])
			:ColumnSQL[E, O, Boolean] =
		BetweenSQL(this, denullify(bounds._1), denullify(bounds._2))

	/** An SQL expression comparing this `String` expression with a pattern, given in the SQL format,
	  * using the `LIKE` operator.
	  */
	def like[E <: F, O >: Grouped <: S]
	        (pattern :ColumnSQL[E, O, String])(implicit isString :V =:= String) :ColumnSQL[E, O, Boolean] =
		LikeSQL(to[String], denullify(pattern))

	/** Concatenates this string expression with another string.
	  * This method is available only for subtypes of `ColumnSQL[_, _, String]`.
	  */
	def ++[E <: F, O >: Grouped <: S]
	      (that :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
		ConcatSQL(to[String]) ++ denullify(that)

	/** Prepends the given `String` expression to this string.
	  * This method is available only for subtypes of `ColumnSQL[_, _, String]` and is particularly useful
	  * when prepending a `String` value, rather than another expression, as it will force its implicit conversion
	  * (which [[net.noresttherein.oldsql.sql.ColumnSQL.++ ++]] would not).
	  */
	def ++:[E <: F, O >: Grouped <: S]
	      (that :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
		denullify(that) ++: ConcatSQL(to[String])

//	/** Concatenates this string expression with another string.
//	  * This method is available only for subtypes of `ColumnSQL[_, _, String]`.
//	  */
//	def |[E <: F, O >: Grouped <: S]
//	     (that :ColumnSQL[E, O, String])(implicit ev :V =:= String) :ColumnSQL[E, O, String] =
//		ConcatSQL(to[String]) ++ denullify(that)



	/** An SQL expression for minus this number. This method is available only for expressions of value type `V`
	  * with type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def unary_-(implicit number :SQLNumber[V]) :ColumnSQL[F, S, V] = ArithmeticSQL.Neg(this)

	/** An arithmetic sum of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.mechanics.=~= =~=]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def +[E <: F, O >: Grouped <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit unify :Interoperable[V, X]#As[Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Plus(unify.left(this), unify.right(denullify(that)))

	/** An arithmetic subtraction of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.mechanics.=~= =~=]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def -[E <: F, O >: Grouped <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit unify :Interoperable[V, X]#As[Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Minus(unify.left(this), unify.right(denullify(that)))

	/** An arithmetic multiplication of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.mechanics.=~= =~=]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def *[E <: F, O >: Grouped <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit unify :(V =~= X)#As[Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Times(unify.left(this), unify.right(denullify(that)))

	/** An arithmetic division of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.mechanics.=~= =~=]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def /[E <: F, O >: Grouped <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit unify :(V =~= X)#As[Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Divide(unify.left(this), unify.right(denullify(that)))

	/** An arithmetic division remainder of two numbers.
	  * @param that a compatible numeric expression. 'Compatible' here means that both can be represented by the same
	  *             SQL type in the database, marked by type class
	  *             [[net.noresttherein.oldsql.sql.mechanics.=~= =~=]].
	  *             Implicit values of it exist for types which weakly conform in Java to this type, or to which
	  *             this type weakly conforms. Additionally, `Option[T]` is interoperable with `T`
	  *             and [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[T]` (the result type of ''select''
	  *             expressions) is interoperable with both `T` and `Seq[T]`. The unification type to which both
	  *             expressions are promoted must have type class [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]].
	  */
	def %[E <: F, O >: Grouped <: S, X, Y]
	     (that :ColumnSQL[E, O, X])(implicit unify :(V =~= X)#As[Y], number :SQLNumber[Y])
			:ColumnSQL[E, O, Y] =
		ArithmeticSQL.Remainder(unify.left(this), unify.right(denullify(that)))



	/** An SQL expression checking if this value is a member of the collection on the right side.
	  * @param that an SQL tuple expression: `(a0, a1, ... an)`.
	  */
	def in[E <: F, O >: Grouped <: S, X]
	      (that :SeqSQL[E, O, X])(implicit unify :V =~= X) :ColumnSQL[E, O, Boolean] =
		new InSQL(unify.left(this), SeqSQL(that.toSeq.map(unify.right(_)) :_*))

	/** An SQL expression checking if this value is a member of the result set returned by the given query.
	  * @param that an SQL ''select'' or other expression returning a row cursor.
	  */
	def in[E <: F, X](that :QuerySQL[E, X])(implicit unify :V =~= X) :ColumnSQL[E, S, Boolean] =
		if (unify.right.isIdentity)
			new InSQL(unify.left(this), that.rows.asInstanceOf[SQLExpression[E, Single, Seq[unify.Unified]]])
		else
			new InSQL(unify.left(this), that.mapRows(unify.right.apply(_ :X)).rows)


	/** True for special purpose `ColumnSQL` implementations, whose type constructor is preserved by some or most
	  * of the methods.
	  * @return [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnConvertingOps.isColumnConvertingSpecific isColumnConvertingSpecific]]` || `[[net.noresttherein.oldsql.sql.ColumnSQL.ColumnGroundingOps.isColumnGroundingSpecific isColumnGroundingSpecific]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.isSpecific]]
	  */
	def isSpecificColumn :Boolean = isColumnConvertingSpecific || isColumnGroundingSpecific

	protected override def convert[Y](conversion :SQLConversion[V, Y]) :ColumnSQL[F, S, Y] =
		if (conversion.isIdentity) conversion(this)
		else ConvertedColumnSQL(this, conversion)

	override def asSingleRow :Option[ColumnSQL[F, Single, V]]
//
//	override def asGround :Option[ColumnSQL[RowProduct, S, V]] =
//		if (isGround) Some(this.asInstanceOf[ColumnSQL[RowProduct, S, V]]) else None
//
//	override def anchor(from :F) :ColumnSQL[F, S, V]

//	override def basedOn[E <: RowProduct](implicit subtype :E <:< F) :ColumnSQL[E, S, V] =
//		this.asInstanceOf[ColumnSQL[E, S, V]]
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnSQL[E, S, V]
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :ColumnSQL[E, S, V]


	override def bind[E <: F { type Params = Args }, Args](base :E, args :Args)
			:ColumnSQL[base.GeneralizedParamless, S, V] =
		SQLScribe.applyParams(base, base.bind(args) :base.GeneralizedParamless)(args)(this)


	override type isSelectable = true
//	@deprecated(ImplementationWarning + " Use directly SelectColumnSQL instead.", InitialVersion)
//	type SelectColumnResult[-E <: RowProduct] = SelectColumn[E, V]
//	@deprecated(ImplementationWarning + " Use directly SubselectColumnSQL instead.", InitialVersion)
//	type SubselectColumnResult[-E <: RowProduct] = SubselectColumn[E, V]

	//can't extend SelectableSQLTemplate to receive it for free because any class extending this type
	// with a higher type as the value type will result in an infinitary class graph
	override def selectFrom(from :F) :SelectColumn[from.Base, V] =
		if (from.isParameterized)
			throw new IllegalArgumentException(
				s"Cannot use a parameterized clause as a basis for select $this: $from."
			)
		else if (from.isSubselect) {
			subselectFrom[NonEmptyRow](from.asInstanceOf[F with SubselectOf[NonEmptyRow]]).asInstanceOf[SelectColumn[from.Base, V]]
		} else
			topSelectFrom(from.asInstanceOf[Complete[F with GroundRow]])

	override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E) :TopSelectColumn[V] =
		SelectSQL(from, this)

//	override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectColumn[B, V] =
//		SelectSQL.subselect[B, from.type, V](from, this)
	override def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :SubselectColumn[B, V] =
		SelectSQL.subselect[B, F with SubselectOf[B], V](from, this)

	override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E) :Select[P, V] =
		Select(from)(this)

//	private[sql] override def rephraseAsLabeledValue[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] =
//		mapper(this)
//	override def reorder(permutation :IndexedSeq[Int]) :ColumnSQL[F, S, V] =
//		if (permutation.length != 1 || permutation.head != 0)
//			throw new IllegalArgumentException(
//				"The only valid reordering permutation of a single column is Seq(0): " + permutation + ")."
//			)
//		else
//			this

	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		AlignableColumns(this, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int = 1

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] = PassedArray :+ this
	protected override def shape(implicit spelling :SQLSpelling) :RowShape = effectiveForm.shape
	protected override def columnCount(implicit spelling :SQLSpelling) :Int = 1
	protected override def effectiveForm(implicit spelling :SQLSpelling) :ColumnForm[V]

	protected[sql] override def `->effectiveForm`(implicit spelling :SQLSpelling) :ColumnForm[V] = effectiveForm

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		PassedArray.single(defaultSpelling(from, context, params))

	/** Translates this expression into an SQL string according to the given format/dialect, adding parenthesis
	  * around the expression if necessary (the expression is not atomic and could be potentially split by operators
	  * of higher precedence). This is a polymorphic method: default implementation always adds parenthesis,
	  * unless the SQL returned by `spelling` already contains a pair; several subclasses override it simply
	  * to `spelling(this)(context, params)`.
	  *
	  * This method cannot be called from within `defaultSpelling` or `inlineSpelling` (for the same object).
	  */
	protected[sql] def atomicSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                    (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(this)(from, context, params).inParens


	/** Forwards the call to the overloaded method variant passing visitor
	  * as a [[net.noresttherein.oldsql.sql.ColumnSQL.SpecificColumnVisitor ColumnVisitor]].
	  */
	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, V, R]) :R =
		visit[R](visitor :SpecificColumnVisitor[F, S, V, R])

	protected def visit[R](visitor :SpecificColumnVisitor[F, S, V, R]) :R
	protected[sql] final def `->visit`[Y](visitor :SpecificColumnVisitor[F, S, V, Y]) :Y = visit(visitor)

	/** A visitor pattern callback in which this expression calls the method of `visitor` most appropriate to its type.
	  * This overloaded variant exists so that visitor implementations
	  * such as [[net.noresttherein.oldsql.sql.mechanics.SQLScribe.AbstractSQLScribe AbstractSQLScribe]] can mix in
	  * a [[net.noresttherein.oldsql.sql.ColumnSQL.AnyColumnVisitor AnyColumnVisitor]] for a more specific type `Y`
	  * than the type parameter to its [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor ExpressionVisitor]]
	  * base type. In the stated example, visiting a `ColumnSQL` will produce another `ColumnSQL` instance,
	  * while non-column expressions return a `SQLExpression`.
	  */
	protected def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, Y]) :Y[S, V]

	/** Forwards the call to the overloaded method variant passing visitor
	  * as a [[net.noresttherein.oldsql.sql.ColumnSQL.AnyColumnVisitor AnyColumnVisitor]].
	  */
	protected override def visit[R[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, R]) :R[S, V] =
		visit(visitor :AnyColumnVisitor[F, R])

	protected[sql] final def `->visit`[Y[-_ >: Grouped <: Single, _]]
	                                  (visitor :AnyColumnVisitor[F, Y]) :Y[S, V] = visit(visitor)
	//	override def typeString :ChunkedString = selectForm.toString
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
	implicit class ColumnSQLExtension[F <: FromSome, V](private val self :ColumnSQL[F, Grouped, V]) extends AnyVal {
		//fixme: remove abstract type projections
		/** Represents SQL `COUNT(this)`.
		  * @return a [[net.noresttherein.oldsql.sql.GroupedColumn LocalColumn]]`[G, Int]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct!.Generalized F#Generalized]].
		  */
		def count :AggregateSQL[F, F#GeneralizedAggregate, V, Int] = AggregateFunction.Count(self)

		/** Represents the SQL `MIN(this)`.
		  * @param ordering an implicit witness attesting that `V` is a comparable type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.GroupedColumn LocalColumn]]`[G, V]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct!.Generalized F#Generalized]].
		  */
		def min(implicit ordering :SQLOrdering[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateFunction.Min(self)

		/** Represents the SQL `MAX(this)`.
		  * @param ordering an implicit witness attesting that `V` is a comparable type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.GroupedColumn LocalColumn]]`[G, V]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct!.Generalized F#Generalized]].
		  */
		def max(implicit ordering :SQLOrdering[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateFunction.Max(self)

		/** Represents the SQL `SUM(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.GroupedColumn LocalColumn]]`[G, V]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct!.Generalized F#Generalized]].
		  */
		def sum(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, V] =
			AggregateFunction.Sum(self)

		/** Represents the SQL `AVG(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.GroupedColumn LocalColumn]]`[G, BigDecimal]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct!.Generalized F#Generalized]].
		  */
		def avg(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateFunction.Avg(self)

		/** Represents the SQL `VAR(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.GroupedColumn LocalColumn]]`[G, BigDecimal]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct!.Generalized F#Generalized]].
		  */
		def variance(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateFunction.Var(self)

		/** Represents the SQL `STDDEV(this)`.
		  * @param isNumber an implicit witness attesting that `V` is a numeric type (or at least the type of the
		  *                 SQL rendered from this instance is numeric in the database).
		  * @return a [[net.noresttherein.oldsql.sql.GroupedColumn LocalColumn]]`[G, BigDecimal]` based on clause
		  *         `G <: RowProduct`, being the least upper bound of all aggregate expressions (`RowProduct` subtypes
		  *         including a ''group by'' clause or [[net.noresttherein.oldsql.sql.Aggregated aggregated]]
		  *         ''selects'', that is subtypes of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  *         such that their [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
		  *         type is equal to [[net.noresttherein.oldsql.sql.RowProduct!.Generalized F#Generalized]].
		  */
		def stddev(implicit isNumber :SQLNumber[V]) :AggregateSQL[F, F#GeneralizedAggregate, V, BigDecimal] =
			AggregateFunction.StdDev(self)

	}



	sealed trait ColumnGroundingOps[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                +Cons[f <: RowProduct] <: ColumnSQL[f, S, V], +Same <: ColumnSQL[F, S, V]]
		extends GroundingTemplate[F, S, V, Cons, Same]
			//consider: not extending ReorderingTemplate. If we know our type, then there is no need to reorder.
		   with ReorderingTemplate[F, S, V, Same]
	{ this :Same with ColumnGroundingOps[F, S, V, Cons, Same] =>
		/** Similar to [[net.noresttherein.oldsql.sql.SQLExpression.GroundingOps.isGroundingSpecific isGroundingSpecific]],
		  * but, given that the latter is implied by simply being a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]],
		  * marks only columns extending
		  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnGroundingTemplate ColumnGroundingTemplate]],
		  * that is implementations which preserve their type by grounding methods in more specificity
		  * than only `ColumnSQL` itself.
		  */
		def isColumnGroundingSpecific :Boolean = false

		protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :Same =
			if (reordering.columnCount != 1 || reordering.underlyingColumnCount != 1 || !reordering.isMapped(1))
				throw new IllegalArgumentException(
					"Cannot reorder `" + this + "`: " + reordering + " is not a single-column identity index translation"
				)
			else
				this
	}
//
//
//	/** Allows template/ops traits to inherit declarations from
//	  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnGroundingOps ColumnGroundingOps]] without extending
//	  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnGroundingTemplate ColumnGroundingTemplate]].
//	  * This is useful because it makes it possible to inherit those definitions without making the extending class
//	  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnGroundingOps.isColumnGroundingSpecific grounding specific]]
//	  * as long as it does not extend `ColumnGroundingTemplate` itself.
//	  */
//	trait DefaultColumnGroundingOps[-F <: RowProduct, -S >: Grouped <: Single, V]
//		extends ColumnGroundingOps[F, S, V, ({ type E[f <: RowProduct] = ColumnSQL[f, S, V] })#E, ColumnSQL[F, S, V]]
//	{ this :ColumnSQL[F, S, V] => }


	trait ColumnGroundingTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                              +Cons[f <: RowProduct] <: ColumnSQL[f, S, V], +Same <: ColumnSQL[F, S, V]]
		extends ColumnGroundingOps[F, S, V, Cons, Same]
	{ this :Same with ColumnGroundingTemplate[F, S, V, Cons, Same] =>
		final override def isColumnGroundingSpecific = true
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
	type VariantColumnGroundingTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                    +E[-f <: RowProduct] <: ColumnSQL[f, S, V]] =
		ColumnGroundingTemplate[F, S, V, E, E[F]]

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
	type InvariantColumnGroundingTemplate[F <: RowProduct, -S >: Grouped <: Single, V,
	                                      +E[f <: RowProduct] <: ColumnSQL[f, S, V]] =
		ColumnGroundingTemplate[F, S, V, E, E[F]]



	/**
	  * @tparam F  $F
	  * @tparam S  $S
	  * @tparam V  $V
	  * @tparam EC $ConvertedCons
	  */
	sealed trait ColumnConvertingOps[-F <: RowProduct, -S >: Grouped <: Single, V, +EC[v] <: ConvertibleColumn[F, S, v, EC]]
		extends ConvertingTemplate[F, S, V, EC]
	{ this :EC[V] with ColumnConvertingOps[F, S, V, EC] =>

		/** Similar to
		  *  [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.isConvertingSpecific isConvertingSpecific]],
		  * but returns `true` only if this expression mixes in
		  * a [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnConvertingTemplate ColumnConvertingTemplate]], presumably
		  * parameterized with a proper subtype of [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]].
		  */
		def isColumnConvertingSpecific :Boolean = false

		override def to[X](implicit conversion :SQLConversion[V, X]) :EC[X] = //conversion.Expression[F, S, EC[X]] =
			conversion(this)

		protected override def adapt[X](conversion :SQLAdaptation[V, X]) :ColumnSQL[F, S, X] =
			AdaptedColumnSQL(this, conversion)

		protected[sql] override def `->adapt`[X](conversion :SQLAdaptation[V, X]) :ColumnSQL[F, S, X] =
			adapt(conversion)

		override def selectForm :ColumnReadForm[V] //consider: Opt[ColumnForm[V]]
		override def universalForm :Opt[ColumnForm[V]] = Lack

		//If/when we properly introduce SQLTransformation.ColumnResult this method should return leftResult.ColumnResult
		// This will make it possible to implement AdaptedColumnSQL without returning a ColumnSQL for any SQLExpression
		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.Expression[F, S, EC[U]], rightResult.Expression[F2, S2, EC2[U]]) =
			if (this eq other)
				(leftResult(this), rightResult.convert(other))
			else if (passCount.firstTime)
				super.reform(other)(reform, passCount)
			else
				other match {
					case column :ColumnSQL[F2, S2, V2] if reform.compatible(this, column) =>
						(leftResult(this), rightResult(other))
					case _ =>
						/* This trait will cause problems if mixed in to classes with reform implementations
						 * returning SQLExpression and it is well, because we don't want to override them
						 * by accident in cases like ConvertedColumnSQL. We have ColumnReformingOverride for that.
						 */
						super.reform(other)(reform, passCount)
				}

/*
		protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                               (implicit leftResult  :SQLTransformation[V, U],
		                                         rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
				:SpecificExpressionVisitor[F2, S2, V2, (leftResult.Expression[F, S, EC[U]],
		                                                rightResult.Expression[F2, S2, EC2[U]])] =
			new BaseReformer[F2, S2, V2, EC2, U, leftResult.Expression, rightResult.Expression](other)(reform, passCount)
*/
/*

		protected override def reform(reordering :Rearrangement)(implicit spelling :SQLSpelling) :EC[V] =
			if (reordering.columnCount != 1)
				throw new IllegalArgumentException(
					"Cannot reorder a column expression `" + this + "` with a Rearrangement for " +
						reordering.columnCount + " columns: " + reordering
				)
			else if (reordering.underlyingColumnCount != 1)
				throw new IllegalArgumentException(
					"Cannot reorder a column expression `" + this + "` into " + reordering.underlyingColumnCount +
						" columns: " + reordering
				)
			else
				this
*/

		//It's unclear if we even need the special subcase for ColumnVisitor because base ExpressionVisitor
		// is parameterized with the expression type, so we can enforce column => column transformation.
		/** A visitor pattern callback in which this expression calls the method of `visitor` most appropriate to its type.
		  * This overloaded variant exists so that visitor implementations can mix in
		  * a [[net.noresttherein.oldsql.sql.ColumnSQL.SpecificColumnVisitor ColumnVisitor]] for a more specific result type `Y`
		  * than the type parameter to its [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor ExpressionVisitor]]
		  * base type.
		  */
//		protected override def visit[F_ <: F, S_ >: Grouped <: S, C[v] >: EC[v] <: SQLExpression[F_, S_, v], T >: E <: C[V],
//		                             Y[-s >: Grouped <: Single, v, -c[vv] <: SQLExpression[F_, s, vv], -e <: c[v]]]
//		                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, C, T] =
//			visit[F_, S_, C, T, Y](visitor :ColumnVisitor[F, Y])
//
//		protected def visit[F_ <: F, S_ >: Grouped <: S, C[v] >: EC[v] <: SQLExpression[F_, S_, v], T >: E <: C[V],
//		                    Y[-s >: Grouped <: Single, v, -c[vv] <: SQLExpression[F_, s, vv], -e <: c[v]]]
//		                   (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, C, T]
//
//		protected[sql] final def `->visit`[F_ <: F, S_ >: Grouped <: S, C[v] >: EC[v] <: SQLExpression[F_, S_, v], T >: E <: C[V],
//		                                   Y[-s >: Grouped <: Single, v, -c[vv] <: SQLExpression[F_, s, vv], -e <: c[v]]]
//		                                  (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, C, T] =
//			visit[F_, S_, C, T, Y](visitor)

	}


	/**
	  * @tparam F  $F
	  * @tparam S  $S
	  * @tparam V  $V
	  * @tparam EC $ConvertedCons
	  */
	trait ColumnConvertingTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                               +EC[v] <: ColumnSQL[F, S, v] with ColumnConvertingOps[F, S, v, EC]]
		extends ColumnConvertingOps[F, S, V, EC]
	{ this :EC[V] with ColumnConvertingOps[F, S, V, EC] =>
		final override def isColumnConvertingSpecific = true
	}


	type ConvertibleColumn[-F <: RowProduct, -S >: Grouped <: Single, V,
	                       +EC[v] <: ColumnSQL[F, S, v] with ColumnConvertingOps[F, S, v, EC]] =
		ColumnSQL[F, S, V] with ColumnConvertingOps[F, S, V, EC]

	/** A type alias for a column expression `Same[F, S, V]`, such that type constructor `Same` is preserved by
	  * conversions, reforming, and grounding.
	  */
	type SpecificColumn[-F <: RowProduct, -S >: Grouped <: Single, V,
	                     +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                           <: ColumnSQL[f, s, v] with ColumnConvertingOps[f, s, v, ({ type E[A] = Same[f, s, A] })#E]
	                              with ColumnGroundingOps[f, s, v, ({ type E[A <: RowProduct] = Same[A, s, v] })#E, Same[f, s, v]]
	] =
		ColumnSQL[F, S, V] with ColumnConvertingOps[F, S, V, ({ type E[v] = Same[F, S, v] })#E]
		                   with ColumnGroundingOps[F, S, V, ({ type E[f <: RowProduct] = Same[f, S, V] })#E, Same[F, S, V]]


	trait ColumnReformingDefaults[-F <: RowProduct, -S >: Grouped <: Single, V, +EC[v] <: ConvertibleColumn[F, S, v, EC]]
		extends ConvertingTemplate[F, S, V, EC] //for super
	{ self :EC[V] with ColumnConvertingOps[F, S, V, EC] =>

		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.Expression[F, S, EC[U]], rightResult.Expression[F2, S2, EC2[U]]) =
			if (this eq other)
				(leftResult(this), rightResult(other))
			else if (passCount.firstTime)
				super[ConvertingTemplate].reform(other)(reform, passCount)
			else
				other match {
					case column :ColumnSQL[F2, S2, V2] if reform.compatible(this, column) =>
						(leftResult(this), rightResult(other))
					case _ =>
						super[ConvertingTemplate].reform(other)(reform, passCount)
				}

		protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                               (implicit leftResult  :SQLTransformation[V, U],
		                                         rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
				:SpecificExpressionVisitor[F2, S2, V2, (leftResult.Expression[F, S, EC[U]],
		                                                rightResult.Expression[F2, S2, EC2[U]])] =
			super[ConvertingTemplate].reformer(other)(reform, passCount)
	}




	implicit def canSelect[F <: RowProduct, B <: RowProduct, E, V, O <: RowProduct]
	                      (implicit selectClauseType :E <:< ColumnSQL[O, Grouped, V],
	                       fromClauseType :F <:< O { type Base = B; type DefineBase[+I <: RowProduct] = I })
			:CanSelectDirect[F, E, SelectColumn[B, V]] =
		new CanSelectDef[F, E, SelectColumn[B, V]] {
			override def apply(from :F, expr :E) =  expr selectFrom from
		}

	implicit def canTopSelect[F <: GroundRow { type Complete = C }, C <: GroundRow { type Complete = C },
	                          E, V, O >: C <: RowProduct]
	                         (implicit selectClauseType :E <:< ColumnSQL[O, Grouped, V],
	                          fromClauseType :F <:< GroundRow { type Complete = C })
			:CanSelectDirect[F, E, TopSelectColumn[V]] =
		new CanSelectDef[F, E, TopSelectColumn[V]] {
			override def apply(from :F, expr :E) = expr topSelectFrom from.complete
		}

	implicit def canSubselect[F <: RowProduct { type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I },
	                          B <: NonEmptyRow, E, V, O <: RowProduct]
	                         (implicit selectClauseType :E <:< ColumnSQL[O, Grouped, V],
	                          fromClauseType :F <:< O {
		                                   type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I
	                                   }
	                         )
			:CanSelectDirect[F, E, SubselectColumn[B, V]] =
		new CanSelectDef[F, E, SubselectColumn[B, V]] {
			override def apply(from :F, expr :E) = expr subselectFrom from
		}
	//paramSelect moved to SelectableSQL because not recognized as a supertype of topSelect


	/** An upper type bound of all `ColumnSQL[_, _, _]` instances. */
	type __ = ColumnSQL[_ <: RowProduct, _ >: Grouped <: Single, _]

	/** A curried type constructor for [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]. */
	type from[F <: RowProduct] = {
		type __ = ColumnSQL[F, _ >: Grouped <: Single, _]
		type rows[S >: Grouped <: Single] = {
			type __ = ColumnSQL[F, S, _]
//			type C[V] = ColumnSQL[F, S, V]
			type value[V] = ColumnSQL[F, S, V]
			type E[V] = ColumnSQL[F, S, V]
		}
	}
/*
	type c[F <: RowProduct] = {
		type apply[-S >: Grouped, V] = ColumnSQL[F, S, V]

		type c[S >: Grouped <: Single] = {
			type __ = ColumnSQL[F, S, _]
			type apply[V] = ColumnSQL[F, S, V]
		}
	}
*/


	/** A type alias for [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] used in cases
	  * when the actual value of the expression does not matter, but only its type.
	  * It is closely related to both [[net.noresttherein.oldsql.sql.RowShape RowShape]]
	  * and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], but used as a sort of an exemplar
	  * for all other instances sharing the same features, but resulting in a different SQL.
	  * The exact degree of similarity at which two `ColumnSQL` instances are considered to have the same layout
	  * is unspecified; in particular, all may be represented by an [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]].
	  */
	type ColumnShape[V] = ColumnSQL[Nothing, Grouped, V]


	trait SpecificColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +R]
		extends SpecificAggregateVisitor[F, S, V, R] with SpecificColumnQueryVisitor[F, V, R]
		   with SpecificColumnTermVisitor[V, R] with SpecificCompositeColumnVisitor[F, S, V, R]
		   with SpecificColumnMappingVisitor[F, S, V, R]
	{
		def apply(e :ColumnSQL[F, S, V]) :R = e.visit(this)

		def column(e :ColumnSQL[F, S, V]) :R
	}

	trait MatchSpecificColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificColumnVisitor[F, S, X, Y] with CaseSpecificAggregate[F, S, X, Y]
		   with CaseSpecificColumnQuery[F, X, Y] with CaseSpecificColumnTerm[X, Y]
		   with CaseSpecificCompositeColumn[F, S, X, Y] with CaseSpecificColumnComponent[F, X, Y]
		   with CaseSpecificLooseColumn[F, X, Y]
	{   //these irregular overrides are needed because we don't extend CaseColumnMapping as it bundles also
		// mapping conversion expressions under its case, and we'd rather have them by default under ConversionSQL
		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[X, A], L <: RowProduct]
		                            (e :TypedColumnComponentSQL[O, T, R, M, X, L]) :Y =
			columnMapping(e)

		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[X, A]](e :LooseColumn[O, M, X]) :Y =
			columnMapping(e)
	}

	/** A mixin for [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor ExpressionVisitor]]
	  * which overrides the methods for all types of column expressions so that they delegate to their supertype
	  * in the [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] inheritance hierarchy,
	  * rather than to the case for their corresponding non-column expression supertype.
	  * For example, [[net.noresttherein.oldsql.sql.SQLExpression.CaseSpecificExpression CaseExpression]] mixes in
	  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral.CaseSpecificLiteral CaseLiteral]], which delegates
	  * [[net.noresttherein.oldsql.sql.ast.ColumnLiteral.SpecificColumnLiteralVisitor.columnLiteral columnLiteral]]
	  * to [[net.noresttherein.oldsql.sql.ast.SQLLiteral.SpecificLiteralVisitor.literal literal]].
	  * On the other hand, this trait delegates the former
	  * to [[net.noresttherein.oldsql.sql.ast.ColumnTerm.SpecificColumnTermVisitor.columnTerm columnTerm]].
	  *
	  * As both `ExpressionVisitor` and this trait are covariant in their return type, this trait
	  * (and thus, indirectly, [[net.noresttherein.oldsql.sql.ColumnSQL.SpecificColumnVisitor ColumnVisitor]])
	  * can be mixed in with a narrowed result type, in order to return specialized instances for column expression
	  * arguments.
	  */
	trait CaseSpecificColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends MatchSpecificColumn[F, S, X, Y] {
		override def *(e :ColumnSQL[RowProduct, Grouped, Nothing])
		              (implicit conforms :ColumnSQL[RowProduct, Grouped, Nothing] <:< ColumnSQL[RowProduct, S, X]) :Y =
			column(e)

		override def aggregate[D <: FromSome, E >: F <: RowProduct, V]
		                      (e :AggregateSQL[D, E, V, X])
		                      (implicit conforms :AggregateSQL[D, E, V, X] <:< ColumnSQL[E, S, X]) :Y =
			column(conforms(e))

		override def compositeColumn(e :CompositeColumnSQL[F, S, X]) :Y = column(e)
		override def columnTerm(e :ColumnTerm[X]) :Y = column(e)

		override def columnQuery[R](e :ColumnQuery[F, R])(implicit isRows :X =:= Rows[R]) :Y =
			column(isRows.substituteContra(e :ColumnSQL[F, S, Rows[R]]))

		override def columnMapping[M[O] <: TypedColumn[_, O]](e :ColumnMappingSQL[F, S, M, X]) :Y = column(e)
	}
//
//
//	trait ColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[F, S, v], -E <: EC[V]]]
//		extends AggregateVisitor[F, Y] with ColumnMappingVisitor[F, Y] with ColumnQueryVisitor[F, Y]
//	       with ColumnTermVisitor[Y] with CompositeColumnVisitor[F, Y]
//	{
//		def apply[S >: Grouped <: Single, V, E <: ColumnSQL[F, S, V] with ColumnSQLTemplate[F, S, V, E]]
//		         (e :ColumnSQLTemplate[F, S, V, E]) :Y[S, V, E] =
//			e.visit(this)
//
//		def column[S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) :Y[S, V, ColumnSQL[F, S, V]]
//	}
//
//	trait MatchColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[F, S, v], -E <: EC[V]]]
//		extends ColumnVisitor[F, Y] with CaseAggregate[F, Y] with CaseColumnQuery[F, Y] with CaseColumnTerm[Y]
//		   with CaseCompositeColumn[F, Y] with CaseColumnComponent[F, Y] with CaseLooseColumn[F, Y]
//	{
//		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseColumn[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
//		                            (e :TypedColumnComponentSQL[O, T, R, M, V, L])
//				:Y[Single, V, TypedColumnComponentSQL[O, T, R, M, V, L]] =
//			columnMapping(e)
//
//		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
//		                        (e :LooseColumn[O, M, V]) :Y[Single, V, LooseColumn[O, M, V]] = columnMapping(e)
//	}
//
//	trait CaseColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[F, S, v], -E <: EC[V]]]
//		extends MatchColumn[F, Y]
//	{
//		override def *(e :ColumnSQL[RowProduct, Grouped, Nothing])
//				:Y[Grouped, Nothing, ColumnSQL[RowProduct, Grouped, Nothing]] =
//			column(e)
//
//		override def aggregate[U <: FromSome, X, V]
//		                      (e :AggregateSQL[U, F, X, V]) :Y[Grouped, V, AggregateSQL[U, F, X, V]] =
//			column(e)
//
//		override def columnMapping[S >: Grouped <: Single, M[A] <: ColumnAt[A], V]
//		                          (e :ColumnMappingSQL[F, S, M, V]) :Y[S, V, ColumnMappingSQL[F, S, M, V]] = column(e)
//
//		override def columnQuery[R](e :ColumnQuery[F, R]) :Y[Single, Rows[R], ColumnQuery[F, R]] = column(e)
//		override def columnTerm[V](e :ColumnTerm[V]) :Y[Single, V, ColumnTerm[V]] = column(e)
//
//		override def compositeColumn[S >: Grouped <: Single, V]
//		                            (e :CompositeColumnSQL[F, S, V]) :Y[S, V, CompositeColumnSQL[F, S, V]] = column(e)
//	}


	trait AnyColumnVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyAggregateVisitor[F, Y] with AnyColumnQueryVisitor[F, Y] with AnyColumnTermVisitor[Y]
		   with AnyCompositeColumnVisitor[F, Y] with AnyColumnMappingVisitor[F, Y]
	{
		def apply[S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) :Y[S, V] = e.visit(this)
		def column[S >: Grouped <: Single, X](e :ColumnSQL[F, S, X]) :Y[S, X]
	}

	trait MatchAnyColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnyColumnVisitor[F, Y]
		with CaseAnyAggregate[F, Y] with CaseAnyColumnQuery[F, Y] with CaseAnyColumnTerm[Y]
		with CaseAnyCompositeColumn[F, Y] with CaseAnyColumnComponent[F, Y] with CaseAnyLooseColumn[F, Y]
	{   //these irregular overrides are needed because we don't extend CaseAnyColumnMapping as it bundles also
		// mapping conversion expressions under its case, and we'd rather have them by default under ConversionSQL
		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
		                            (e :TypedColumnComponentSQL[O, T, E, M, V, L]) :Y[Single, V] =
			columnMapping(e)

		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
		                        (e :LooseColumn[O, M, V]) :Y[Single, V] =
			columnMapping(e)
	}

	/** A mixin for [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor AnyExpressionVisitor]]
	  * which overrides the methods for all types of column expressions so that they delegate to their supertype
	  * in the [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] inheritance hierarchy,
	  * rather than to the case for their corresponding non-column expression supertype.
	  * For example, [[net.noresttherein.oldsql.sql.SQLExpression.CaseAnyExpression CaseAnyExpression]] mixes in
	  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral.CaseAnyLiteral CaseAnyLiteral]], which delegates
	  * [[net.noresttherein.oldsql.sql.ast.ColumnLiteral.AnyColumnLiteralVisitor.columnLiteral columnLiteral]]
	  * to [[net.noresttherein.oldsql.sql.ast.SQLLiteral.AnyLiteralVisitor.literal literal]].
	  * On the other hand, this trait delegates the former
	  * to [[net.noresttherein.oldsql.sql.ast.ColumnTerm.AnyColumnTermVisitor.columnTerm columnTerm]].
	  *
	  * As both `AnyExpressionVisitor` and this trait are covariant in their return type, this trait
	  * (and thus, indirectly, [[net.noresttherein.oldsql.sql.ColumnSQL.AnyColumnVisitor AnyColumnVisitor]])
	  * can be mixed in with a narrowed result type. For example, this is taken advantage of
	  * by [[net.noresttherein.oldsql.sql.mechanics.SQLScribe SQLScribe]], which returns a `ColumnSQL`
	  * instead of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] if the argument was a column expression.
	  */
	trait CaseAnyColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyColumn[F, Y] {
		override def *(e :ColumnSQL[RowProduct, Grouped, Nothing]) :Y[Grouped, Nothing] =
			column[Grouped, Nothing](e)

		override def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[Grouped, V] =
			column(e)

		override def compositeColumn[S >: Grouped <: Single, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X] =
			column(e)

		override def columnQuery[V](e :ColumnQuery[F, V]) :Y[Single, Rows[V]] = column(e)
		override def columnTerm[X](e :ColumnTerm[X]) :Y[Single, X] = column(e)

		override def columnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], V]
		                          (e :ColumnMappingSQL[F, S, M, V]) :Y[S, V] =
			column(e)
	}

}

package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Chain, Listing, PassedArray}
import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.exceptions.IllegalExpressionException
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.slang.saferCasting
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SingleSQL, SQLExpression, WithClause}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, DefaultGroundingOps, Grouped, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL.{AnyAdaptedColumnVisitor, CaseAnyAdaptedColumn, CaseSpecificAdaptedColumn, SpecificAdaptedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.AliasedSQL.{AnyAliasedExpressionVisitor, CaseAnyAliasedExpression, CaseSpecificAliasedExpression, SpecificAliasedExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.{AnyArithmeticVisitor, CaseAnyArithmetic, CaseSpecificArithmetic, SpecificArithmeticVisitor}
import net.noresttherein.oldsql.sql.ast.ChainSQL.{AnyChainVisitor, MatchAnyChain, MatchSpecificChain, SpecificChainVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.{AnyCompositeColumnVisitor, MatchAnyCompositeColumnOnly, MatchSpecificCompositeColumnOnly, SpecificCompositeColumnVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeSQLTemplate
import net.noresttherein.oldsql.sql.ast.CompoundSelectColumn.{AnyCompoundSelectColumnVisitor, CaseAnyCompoundSelectColumn, CaseSpecificCompoundSelectColumn, SpecificCompoundSelectColumnVisitor}
import net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.{AnyCompoundSelectVisitor, CaseAnyCompoundSelect, CaseSpecificCompoundSelect, SpecificCompoundSelectVisitor}
import net.noresttherein.oldsql.sql.ast.ConcatSQL.{AnyConcatVisitor, CaseAnyConcat, CaseSpecificConcat, SpecificConcatVisitor}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.{AnyConditionVisitor, CaseAnyCondition, CaseSpecificCondition, SpecificConditionVisitor}
import net.noresttherein.oldsql.sql.ast.EditedColumnSQL.{AnyEditedColumnVisitor, CaseAnyEditedColumn, CaseSpecificEditedColumn, SpecificEditedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.EditedLValueSQL.{AnyEditedLValueVisitor, CaseAnyEditedLValue, CaseSpecificEditedLValue, SpecificEditedLValueVisitor}
import net.noresttherein.oldsql.sql.ast.FunctionColumnSQL.{AnyFunctionColumnVisitor, CaseAnyFunctionColumn, CaseSpecificFunctionColumn, SpecificFunctionColumnVisitor}
import net.noresttherein.oldsql.sql.ast.FunctionSQL.{AnyFunctionVisitor, CaseAnyFunction, CaseSpecificFunction, SpecificFunctionVisitor}
import net.noresttherein.oldsql.sql.ast.QuerySQL.Rows
import net.noresttherein.oldsql.sql.ast.RearrangedSQL.{AnyRearrangedVisitor, CaseAnyRearranged, CaseSpecificRearranged, SpecificRearrangedVisitor}
import net.noresttherein.oldsql.sql.ast.SelectIdSQL.{AnySelectIdVisitor, CaseAnySelectId, CaseSpecificSelectId, SpecificSelectIdVisitor}
import net.noresttherein.oldsql.sql.ast.InlineSQL.{AnyInlineVisitor, CaseAnyInline, CaseSpecificInline, SpecificInlineVisitor}
import net.noresttherein.oldsql.sql.ast.LabeledSQL.{AnyLabeledVisitor, MatchAnyLabeled, MatchSpecificLabeled, SpecificLabeledVisitor}
import net.noresttherein.oldsql.sql.ast.TransformedSQL.{AnyTransformedVisitor, CaseAnyTransformed, CaseSpecificTransformed, SpecificTransformedVisitor}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** Base type of [[net.noresttherein.oldsql.sql.SQLExpression expressions]] which are composed of other expressions.
  * The number of subexpressions can be fixed (including to a single one), or vary between instances.
  * Likewise, the composite subtype may define the value type of included expressions, or be heterogeneous/agnostic.
  * All constituting expressions must be applicable in the same context as this expression
  * ([[net.noresttherein.oldsql.sql.SQLExpression.Grouped scope]]
  * and the [[net.noresttherein.oldsql.sql.RowProduct base]] ''from'' clause), which excludes
  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions.
  * The assumption is however that their exact subtypes of the `SQLExpression` do not matter, aside from
  * the mentioned restrictions on their type parameters, and that any of them can be replaced with another
  * `SQLExpression` conforming to these bounds, and that the result will be a valid SQL expression which can
  * serve as a substitute for this expression, again producing a valid expression. Exactly how the expression
  * is composed, and what role its subexpressions play in it, is undefined. However, two of its subtypes,
  * [[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]] and
  * [[net.noresttherein.oldsql.sql.ast.AdaptedSQL DecoratorSQL]] specify explicitly that their columns
  * are formed directly from the columns of their subexpressions, with the latter being completely equivalent
  * from the SQL point of view with its single wrapped subexpression, while the former inlines its subexpressions
  * to form its SQL representation.
  *
  * Such expressions are often treated the same way by
  * [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor visitors]] of the `SQLExpression` type hierarchy,
  * which recursively reapply themselves to the subexpression of this expression (possibly producing another
  * `SQLExpression` by reapplying the same type of the composite expression to the mapped subexpressions.
  * The additional methods defined here facilitate this behaviour as well as structural comparison
  * for [[net.noresttherein.oldsql.sql.ast.CompositeSQL.equals equals]] and
  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.isomorphic isomorphic]].
  */
trait CompositeSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
	extends SQLExpression[F, S, V]
	   with CompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = SQLExpression[f, S, V] })#E]
{

//	override def anchor(from :F) :SQLExpression[F, S, V] =
//		if (isAnchored(from)) this else rephrase(SQLScribe.anchor(from))
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :SQLExpression[E, S, V] =
//		rephrase(SQLScribe.rebase(base))
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :SQLExpression[E, S, V] =
//		rephrase(SQLScribe.expand(base))
//
//	/** Method used in the implementation of
//	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.basedOn basedOn]],
//	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.expand expand]] and other methods recreating the same expression
//	  * for a different domain clause.
//	  * It should apply `mapper` to all constituting subexpressions and reassemble them into another
//	  * composite expression of the same type as this one.
//	  */
//	@throws[IllegalExpressionException]("if no equivalent expression can be constructed from reformed subexpressions.")
//	def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V]

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, R]) :R[S, V] =
		visitor.composite(this)

	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, V, R]) :R = visitor.composite(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F_, S_, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.composite(this)


	private[oldsql] override def equivalent(expression: SQLExpression.__): Boolean = expression match {
		case e :CompositeSQL.__ => //fixme:  we can't ignore order for all expressions!
			(e eq this) || e.sameAs(this) && this.sameAs(e) && contentsEquivalent(e)
		case _ => false
	}

	/** Compares this expression with another expression, which must be a `CompositeSQL` in order for the test
	  * to turn out positive in a way mirroring the implementation of
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.equals equals]].
	  * The difference is that [[net.noresttherein.oldsql.sql.ast.CompositeSQL.sameAs sameAs]] is used
	  * instead of [[net.noresttherein.oldsql.sql.ast.CompositeSQL.canEqual canEqual]],
	  * and the part lists are compared using their `isomorphic` methods instead of equality.
	  * Both the order and the number of subexpressions must still match this expression.
	  */
	override def isomorphic(expression :SQLExpression.__) :Boolean = expression match {
		case e :CompositeSQL.__ =>
			(this eq e) || sameAs(e) && e.sameAs(this) && contentsIsomorphic(e)
		case _ => false
	}

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



object CompositeSQL {

	type __ = CompositeSQL[_ <: RowProduct, _ >: Grouped <: Single, _]

	//These templates do not extend SQLExpression because they are mixed in to traits extending ConvertingTemplate
	// (with a narrowed type parameter), and declaration of convert clashes with the definition in SQLExpression
	trait CompositeSQLOps[-F <: RowProduct, -S >: Grouped <: Single, V,
	                      +Same[-f <: RowProduct] <: SQLExpression[f, S, V]]
		extends DefaultGroundingOps[F, S, V]
	{ this :Same[F] =>
		/** Subexpressions of this expression. */
		protected def parts :Seq[SQLExpression[F, S, _]]

		/** Constituent subexpressions ([[net.noresttherein.oldsql.sql.ast.CompositeSQL.parts parts]]) of this expression,
		  * from left to right, as they would appear in the generated SQL.
		  */
		def inOrder :Seq[SQLExpression[F, S, _]] = parts

		/** Tells if all columns of [[net.noresttherein.oldsql.sql.ast.CompositeSQL.parts parts]] subexpressions
		  * have corresponding columns in the [[net.noresttherein.oldsql.sql.SQLExpression.split split]] column list
		  * of this expression. The order in which they occur is unspecified and this expression may introduce additional
		  * columns, not listed among `parts`' columns.
		  */
		def isShapeComposite     :Boolean = false //consider: a better name. isColumnComposite? isColumnSeq? isColumnConcat? isInline?

		/** Tells if the columns listed by [[net.noresttherein.oldsql.sql.SQLExpression.split split]] correspond one to one
		  * to columns [[net.noresttherein.oldsql.sql.ast.CompositeSQL.inOrder inOrder]]`.flatMap(_.split)`.
		  * The columns need not be the same instances, but spelling of this composite should be consistent
		  * with spelling of individual parts.
		  */
		def isInline             :Boolean = false //isFlatMap? isColumnFlatMap?
		override def isGround    :Boolean = parts.forall(_.isGround)
		override def isSingleRow :Boolean = parts.forall(_.isSingleRow)
		override def asSingleRow :Option[SingleSQL[F, V]] =
			if (isSingleRow) Some(this.asInstanceOf[SingleSQL[F, V]])
			else None

		//consider: moving this up, because we'd like to set anchorClause on other instances and a bound of CompositeSQL is inconvenient
		@transient @volatile private var knownIfAnchored = false
		@transient @volatile private var anchored_? :Boolean = _
		@transient @volatile private[this] var anchorClause :F = _

		override def isAnchored :Boolean = {
			if (!knownIfAnchored) {
				anchored_? = checkIfAnchored
				knownIfAnchored = true
			}
			anchored_?
		}
		private[ast] def checkIfAnchored :Boolean = parts.forall(_.isAnchored)

		override def isAnchored(from :F) :Boolean =
			(anchorClause eq from) || {
				val res = checkIfAnchored(from)
				if (res)
					anchorClause = from
				res
			}
		private[ast] def checkIfAnchored(from :F) :Boolean = parts.forall(_.isAnchored(from))
		private[ast] def anchoredIn :RowProduct = anchorClause
		private[ast] def anchoredIn_=(from :F) :Unit = anchorClause = from

		override def anchor(from :F) :Same[F] =
			if (isAnchored(from)) this
			else rephrase(SQLScribe.anchor(from))
//			else {
//				val res = rephrase(SQLScribe.anchor(from))
//				res.anchoredIn = from
//				res
//			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Same[E] =
			rephrase(SQLScribe.expand(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Same[E] =
			rephrase(SQLScribe.expand(base))

		/** Method used in the implementation of
		  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.basedOn basedOn]],
		  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.expand expand]] and other methods recreating
		  * the same expression for a different domain clause.
		  * It should apply `mapper` to all constituting subexpressions and reassemble them into another
		  * composite expression of the same type as this one.
		  */
		@throws[IllegalExpressionException]("if no equivalent expression can be constructed from reformed subexpressions.")
		def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :Same[E]

		override def outerWithClause :WithClause = parts.view.map(_.outerWithClause).reduce(_ ++ _)

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
			(0 /: parts)(_ + spelling.sqlParamCount(_))

		private[oldsql] def contentsEquivalent(other :CompositeSQL.__) :Boolean =
			parts.size == other.parts.size &&
				parts.forall(e => other.parts.exists(_ equivalent e)) &&
				other.parts.forall(e => parts.exists(_ equivalent e))

		/** Verifies that the [[net.noresttherein.oldsql.sql.ast.CompositeSQL.parts parts]] lists
		  * of both expressions are of the same length and that all matched pairs are
		  * [[net.noresttherein.oldsql.sql.SQLExpression.isomorphic isomorphic]].
		  */
		def contentsIsomorphic(other :CompositeSQL.__) :Boolean =
			parts.size == other.parts.size &&
				((parts zip other.parts) forall { case (left, right) => left isomorphic right })

		/** Defaults to [[net.noresttherein.oldsql.sql.ast.CompositeSQL.canEqual canEqual]]. */
		def sameAs(other :CompositeSQL.__) :Boolean = canEqual(other)

		/** A method used to attain the symmetry of the equality relation on `CompositeSQL` instances.
		  * It is tested by the `equals` method in ''both'' directions before comparing the contents
		  * of compared expressions. The default expressions checks if both `this` and `that` are of the same class
		  * which should be satisfactory for most implementations.
		  */
		override def canEqual(that :Any) :Boolean = that.getClass == getClass
	}


	trait CompositeSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                           +Same[-f <: RowProduct] <: SQLExpression[f, S, V]]
		extends VariantGroundingTemplate[F, S, V, Same] with CompositeSQLOps[F, S, V, Same]
	{ this :Same[F] => }


	/** Implementation-oriented base trait for `CompositeSQL` subclasses which consist of a single subexpression.
	  * It does not carry any additional meaning or offer new functionality, it simply provides more efficient
	  * implementations for methods inherited from `CompositeSQL`. Clients should not make assumptions
	  * that the exact semantics of methods implemented in subclasses match those defined here.
	  * @see [[net.noresttherein.oldsql.sql.ast.AdaptedSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryCompositeTemplate]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn]]
	  */ //todo: template Expr[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, Expr[v]] with GroundingTemplate[F, S, v, Expr]
	trait UnaryCompositeSQL[-F <: RowProduct, -S >: Grouped <: Single, X, V]
		extends CompositeSQL[F, S, V]
		   with UnaryCompositeTemplate[F, S, X, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = SQLExpression[f, s, V] })#E]


	/** A template mixin trait for subclasses of
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL UnaryCompositeSQL]]
	  * which can wrap SQL expressions of any type (with any type arguments).
	  * It preserves type constructor `Ex` of the subclass when grounding it in other clauses.
	  * It delegates all methods declared in
	  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]] to the wrapped
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeTemplate.value value]],
	  * and [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeTemplate.reapply reapplies]]
	  * this type of wrapper to the result.
	  * @see [[net.noresttherein.oldsql.sql.ast.TransformedSQL.WrappedSQLTemplate]]
	  */
	trait UnaryCompositeTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, V,
	                             +Same[-f <: RowProduct, -s >: Grouped <: Single] <: SQLExpression[f, s, V]]
		//Does not extend VariantGroundingTemplate in order to not have to be grounding specific
		// if a subclass extends only GroundingOps[_, SQLExpression]
		extends CompositeSQLOps[F, S, V, ({ type E[-f  <: RowProduct] = SQLExpression[f, S, V] })#E]
//		   with VariantGroundingTemplate[F, S, V, ({ type T[-f <: RowProduct] = Same[f, S] })#T]
	{ this :Same[F, S] =>
		//todo: rename to underlying (and in UnaryCompositeColumn, too)
		protected def value :SQLExpression[F, S, X]
		protected override def parts :Seq[SQLExpression[F, S, _]] = PassedArray.one(value)

		override def isSingleRow :Boolean = value.isSingleRow
		override def isGround    :Boolean = value.isGround
		private[sql] override def checkIfAnchored  :Boolean = value.isAnchored
		private[sql] override def checkIfAnchored(from :F) :Boolean = value.isAnchored(from)

		override def asSingleRow :Option[Same[F, Single]] =
			if (isSingleRow) Some(this.castFrom[Same[F, S], Same[F, Single]]) else None

		override def anchor(from :F) :Same[F, S] =
			if (isAnchored(from))
				this
			else value.anchor(from) match {
				case same if same eq value => this
				case other => reapply(other)
			}
//			else value.anchor(from) match {
//				case same if same eq value => anchoredIn = from; this
//				case anchored => val res = reapply(anchored); res.anchoredIn = from; res
//			}
	    override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Same[E, S] =
		    if (expansion.lengthDiff == 0) this.castFrom[Same[F, S], Same[E, S]]
		    else reapply(value.basedOn(base))

	    override def expand[U <: F, E <: RowProduct]
	                       (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Same[E, S] =
		    if (expansion.length == 0) this.castFrom[Same[F, S], Same[E, S]]
		    else reapply(value.expand(base))

		//Not Same[E, S] because SQLScribe gives no guarantees for the types of the returned expressions
		// other than ColumnSQL, so we want to reserve an option for subclasses to return a value of a supertype here.
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V] = {
			val v = mapper(value)
			if (v eq value) this.castFrom[Same[F, S], SQLExpression[E, S, V]]
			else reapply(v)
		}

		/* We need C >: Grouped <: GlobalScope because S would be in a covariant position in e :(
		 * Scala 3 trait parameter of SQLExpression[E, S, X] => SQLExpression[E, C, V] would solve this.
		 * However, currently only TransformedSQL.conversion relies on the ability to wrap expressions of any scope.
		 * We might move this method to TransformedSQL and leave here only one accepting SQLExpression[E, S, X].
		 * protected def reapply[E <: RowProduct, C >: Grouped <: S](e :SQLExpression[E, C, X]) :Ex[E, C]
		 */
		protected def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, X]) :Same[E, C]


		override def outerWithClause :WithClause = value.outerWithClause

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(value)


		override def contentsIsomorphic(other :CompositeSQL.__) :Boolean = other match {
			case unary :UnaryCompositeSQL[_, _, _, _] @unchecked => value isomorphic unary.value
			case _ => false
		}
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case unary :UnaryCompositeTemplate[_, _, _, _, Same] @unchecked
				if canEqual(unary) && unary.canEqual(this)
			=>
				value == unary.value
			case _ => false
		}
		override def hashCode :Int = value.hashCode
	}



	trait BinaryCompositeSQL[-F <: RowProduct, -S >: Grouped <: Single, L, R, V]
		extends CompositeSQL[F, S, V]
		   with BinaryCompositeTemplate[F, S, L, R, V,
		                               ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = SQLExpression[f, s, V] })#E]

	/** An implementation oriented base trait for `CompositeSQL` subclasses which consist of two subexpressions.
	  * It does not carry any additional meaning or offer new functionality, it simply provides more efficient
	  * implementations for methods inherited from `CompositeSQL`.
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.BinaryCompositeColumnTemplate]]
	  * @see [[net.noresttherein.oldsql.sql.ast.InlineSQL.AbstractInlineBinaryCompositeSQL]]
	  */
	trait BinaryCompositeTemplate[-F <: RowProduct, -S >: Grouped <: Single, L, R, V,
	                              +Same[-f <: RowProduct, -s >: Grouped <: Single] <: SQLExpression[f, s, V]]
		extends CompositeSQLOps[F, S, V, ({ type E[-f <: RowProduct] = SQLExpression[f, S, V] })#E]
//		   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = Same[f, S] })#E]
	{ this :Same[F, S] =>
		//Consider: going an extra mile and making left and right GroundingTemplate, so that reapply accepts only
		// those types as arguments. We still may fail with SQLScribe/rephrase, though.
		protected def left  :SQLExpression[F, S, L]
		protected def right :SQLExpression[F, S, R]

		protected override def parts :Seq[SQLExpression[F, S, _]] = PassedArray.two(left, right)

		//reversed check order so that ChainEntry and similar first check the last item, and only then the prefix
		override def isSingleRow :Boolean = right.isSingleRow && left.isSingleRow
		override def isGround    :Boolean = right.isGround && left.isGround
		private[ast] override def checkIfAnchored :Boolean = right.isAnchored && left.isAnchored
		private[ast] override def checkIfAnchored(from :F) :Boolean = right.isAnchored(from) && left.isAnchored(from)

		override def asSingleRow :Option[Same[F, Single]] =
			if (isSingleRow) Some(this.castFrom[Same[F, S], Same[F, Single]]) else None

		override def anchor(from :F) :Same[F, S] =
			if (isAnchored(from))
				this
			else
				(left.anchor(from), right.anchor(from)) match {
					case (l, r) if (l eq left) && (r eq right) => this
					case (l, r) => reapply(l, r)
				}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Same[E, S] =
			if (expansion.lengthDiff == 0) this.asInstanceOf[Same[E, S]]
			else reapply(left.basedOn(base), right.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Same[E, S] =
			if (expansion.length == 0) this.asInstanceOf[Same[E, S]]
			else reapply(left.expand(base), right.expand(base))

		//Not Same[E, S] because SQLScribe gives no guarantees for the types of the returned expressions
		// other than ColumnSQL, so we want to reserve an option for subclasses to return a value of a supertype here.
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V] =
			(mapper(left), mapper(right)) match  {
				case (l, r) if (l eq left) & (r eq right) => this.castFrom[Same[F, S], SQLExpression[E, S, V]]
				case (l, r) => reapply(l, r)
			}

		/* I tried to make the template monstrous by parameterizing left and right expression types
		 * with GroundingTemplate bounds, so that reapply called after expand and other methods there would receive
		 * the same expression types, but rephrase will produce vanilla SQLExpression/ColumnSQL
		 * and need casting down anyway. We can't even implement rephrase here for convenience,
		 * because without ClassTags we can't cast down the results to the specific left/right types.
		 * At best we could catch class cast exceptions, but that's far from perfect,
		 * as they might come from somewhere deeper in the expression. Anyway, the type would have to be invariant,
		 * which is inconvenient. This, and annoying Scope type parameter can be solved with trait parameters.
		 */
		protected def reapply[E <: RowProduct, C >: Grouped <: Single]
		                     (left :SQLExpression[E, C, L], right :SQLExpression[E, C, R]) :Same[E, C]


		override def outerWithClause :WithClause = left.outerWithClause ++ right.outerWithClause

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
			spelling.sqlParamCount(left) + spelling.sqlParamCount(right)

		override def contentsIsomorphic(other :CompositeSQL.__) :Boolean = other match {
			case e :BinaryCompositeTemplate[_, _, _, _, _, Same @unchecked] =>
				(left isomorphic e.left) && (right isomorphic e.right)
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :BinaryCompositeTemplate[_, _, _, _, _, Same @unchecked]
			if canEqual(other) && other.canEqual(this) =>
				left == other.left && right == other.right
			case _ => false
		}

		override def hashCode :Int = left.hashCode * 31 + right.hashCode
	}





	trait SpecificCompositeVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificCompositeColumnVisitor[F, S, X, Y] with SpecificChainVisitor[F, S, X, Y]
		   with SpecificCompoundSelectVisitor[F, X, Y] with SpecificEditedLValueVisitor[F, X, Y]
		   with SpecificFunctionVisitor[F, S, X, Y] with SpecificInlineVisitor[F, S, X, Y]
		   with SpecificLabeledVisitor[F, S, X, Y] with SpecificRearrangedVisitor[F, S, X, Y]
		   with SpecificSelectIdVisitor[F, S, X, Y] with SpecificTransformedVisitor[F, S, X, Y]
   {
	   def composite(e :CompositeSQL[F, S, X]) :Y
   }
	//bundles RecordSQL and ChainTuple under InlineSQL, not LabeledSQL and ChainSQL
	trait MatchSpecificComposite[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificCompositeVisitor[F, S, X, Y] with MatchSpecificCompositeColumnOnly[F, S, X, Y]
		   with MatchSpecificChain[F, S, X, Y] with CaseSpecificCompoundSelect[F, X, Y]
		   with CaseSpecificEditedLValue[F, X, Y] with CaseSpecificFunction[F, S, X, Y]
		   with CaseSpecificInline[F, S, X, Y] with MatchSpecificLabeled[F, S, X, Y]
		   with CaseSpecificRearranged[F, S, X, Y] with CaseSpecificSelectId[F, S, X, Y]
		   with CaseSpecificTransformed[F, S, X, Y]
	{
		override def compositeColumn(e :CompositeColumnSQL[F, S, X]) :Y = composite(e)
	}

	trait CaseSpecificComposite[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificComposite[F, S, X, Y]
	{
		override def transformed[V](e :TransformedSQL[F, S, V, X]) :Y = composite(e)
		override def alias(e :AliasedSQL[F, S, X]) :Y = compositeColumn(e)
		override def arithmetic(e :ArithmeticSQL[F, S, X]) :Y = compositeColumn(e)
		override def chain[I <: Chain, L](e :ChainSQL[F, S, I, L])(implicit isChain :X =:= (I ~ L)) :Y =
			composite(isChain.substituteContra(e :CompositeSQL[F, S, I ~ L]))

		override def compoundSelect[R](e :CompoundSelectSQL[F, R])(implicit isRows :X =:= Rows[R]) :Y =
			composite(isRows.substituteContra(e :CompositeSQL[F, S, Rows[R]]))

		override def concat(e :ConcatSQL[F, S])(implicit isString :X =:= String) :Y =
			compositeColumn(isString.substituteContra(e :CompositeColumnSQL[F, S, String]))

		override def condition(e :ConditionSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y =
			compositeColumn(isBoolean.substituteContra(e :CompositeColumnSQL[F, S, Boolean]))

		override def editedColumn[M[O] <: BaseColumn[X, O], V](e :EditedColumnSQL[F, M, X, V]) :Y = composite(e)
		override def editedLValue[M[A] <: BaseMapping[X, A]](e :EditedLValueSQL[F, M, X]) :Y = composite(e)

		override def function[A <: Chain](e :FunctionSQL[F, S, A, X]) :Y = composite(e)
		override def inline(e :InlineSQL[F, S, X]) :Y = composite(e)

		override def labeled[I <: Listing, K <: Label, L](e :LabeledSQL[F, S, I, K, L])
		                                                 (implicit isListing :X =:= (I |~ (K :~ L))) :Y =
			composite(isListing.substituteContra(e :CompositeSQL[F, S, I |~ (K :~ L)]))

		override def rearranged(e :RearrangedSQL[F, S, X]) :Y = composite(e)
		override def selectId(e :SelectIdSQL[F, S, X]) :Y = composite(e)
	}
//
//
//	trait CompositeVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompositeColumnVisitor[F, Y] with AdaptedVisitor[F, Y] with ChainVisitor[F, Y]
//		   with CompoundSelectVisitor[F, Y] with EditedLValueVisitor[F, Y] with FunctionVisitor[F, Y]
//		   with InlineVisitor[F, Y] with RearrangedVisitor[F, Y] with SelectIdVisitor[F, Y]
//	{
//	   def composite[S >: Grouped <: Single, V](e :CompositeSQL[F, S, V]) :Y[S, V, CompositeSQL[F, S, V]]
//	}
//
//	trait MatchComposite[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompositeVisitor[F, Y] with MatchCompositeColumnOnly[F, Y]
//		   with CaseAdapted[F, Y] with CaseCompoundSelect[F, Y] with MatchChain[F, Y]
//		   with CaseEditedLValue[F, Y] with CaseFunction[F, Y]
//		   with CaseInline[F, Y] with CaseRearranged[F, Y] with CaseSelectId[F, Y]
//	{
//		override def compositeColumn[S >: Grouped <: Single, V]
//		                            (e :CompositeColumnSQL[F, S, V]) :Y[S, V, CompositeColumnSQL[F, S, V]] =
//			composite(e)
//	}
//
//	trait CaseComposite[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchComposite[F, Y]
//	{
//		override def alias[S >: Grouped <: Single, V]
//		                  (e :AliasedSQL[F, S, V]) :Y[S, V, AliasedSQL[F, S, V]] = compositeColumn(e)
//
//		override def arithmetic[S >: Grouped <: Single, V]
//		                       (e :ArithmeticSQL[F, S, V]) :Y[S, V, ArithmeticSQL[F, S, V]] = compositeColumn(e)
//
//		override def concat[S >: Grouped <: Single](e :ConcatSQL[F, S]) :Y[S, String, ConcatSQL[F, S]] =
//			compositeColumn(e)
//
//		override def condition[S >: Grouped <: Single](e :ConditionSQL[F, S]) :Y[S, Boolean, ConditionSQL[F, S]] =
//			compositeColumn(e)
//
//		override def logical[S >: Grouped <: Single]
//		                    (e :LogicalSQL[F, S]) :Y[S, Boolean, LogicalSQL[F, S]] = compositeColumn(e)
//
//		override def editedColumn[M[O] <: BaseColumn[X, O], X, V](e :EditedColumnSQL[F, M, X, V]) :Y[Single, X] =
//			composite(e)
////
////		override def transformed[S >: Grouped <: GlobalScope, X, V]
////		                        (e :TransformedSQL[F, S, X, V]) :Y[S, X, TransformedSQL[F, S, X, V]] =
////			composite(e)
//
//		override def adapted[S >: Grouped <: Single, X, V]
//		                    (e :AdaptedSQL[F, S, X, V]) :Y[S, V] = composite(e)
//
//		override def chain[S >: Grouped <: Single, I <: Chain, L]
//		                  (e :ChainSQL[F, S, I, L]) :Y[S, I ~ L, ChainSQL[F, S, I, L]] = composite(e)
//
//		override def compoundSelect[V](e :CompoundSelectSQL[F, V]) :Y[Single, Rows[V], CompoundSelectSQL[F, V]] =
//			composite(e)
//
//		override def editedLValue[M[A] <: BaseMapping[V, A], V]
//		                         (e :EditedLValueSQL[F, M, V]) :Y[Single, V, EditedLValueSQL[F, M, V]] =
//			composite(e)
//
//		override def function[S >: Grouped <: Single, X <: Chain, V]
//		                     (e :FunctionSQL[F, S, X, V]) :Y[S, V, FunctionSQL[F, S, X, V]] =
//			composite(e)
//
//		override def inline[S >: Grouped <: Single, V](e :InlineSQL[F, S, V]) :Y[S, V, InlineSQL[F, S, V]] =
//			composite(e)
//
//		override def rearranged[S >: Grouped <: Single, V]
//		                       (e :RearrangedSQL[F, S, V]) :Y[S, V, RearrangedSQL[F, S, V]] = composite(e)
//
//		override def selectId[S >: Grouped <: Single, V](e :SelectIdSQL[F, S, V]) :Y[S, V, SelectIdSQL[F, S, V]] =
//			composite(e)
//	}


	trait AnyCompositeVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompositeColumnVisitor[F, Y] with AnyChainVisitor[F, Y] with AnyCompoundSelectVisitor[F, Y]
		   with AnyEditedLValueVisitor[F, Y] with AnyFunctionVisitor[F, Y] with AnyInlineVisitor[F, Y]
		   with AnyLabeledVisitor[F, Y] with AnyRearrangedVisitor[F, Y] with AnySelectIdVisitor[F, Y]
		   with AnyTransformedVisitor[F, Y]
	{
		def composite[S >: Grouped <: Single, X](e :CompositeSQL[F, S, X]) :Y[S, X]
	}

	trait MatchAnyComposite[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompositeVisitor[F, Y] with MatchAnyCompositeColumnOnly[F, Y]
		   with MatchAnyChain[F, Y] with CaseAnyCompoundSelect[F, Y] with CaseAnyInline[F, Y]
		   with CaseAnyFunction[F, Y] with CaseAnyEditedLValue[F, Y] with MatchAnyLabeled[F, Y]
		   with CaseAnyRearranged[F, Y] with CaseAnySelectId[F, Y] with CaseAnyTransformed[F, Y]
	{
		override def compositeColumn[S >: Grouped <: Single, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X] =
			composite(e :CompositeSQL[F, S, X])
	}

	/* There is a repetition with CaseAnyCompositeColumn, but it can't be extended here, as mixing it in
	 * should always rewire the column columns through the column matcher hierarchy up to composite column*/
	trait CaseAnyComposite[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyComposite[F, Y] {
		override def transformed[S >: Grouped <: Single, Z, X](e :TransformedSQL[F, S, Z, X]) :Y[S, X] = composite(e)
		override def alias[S >: Grouped <: Single, V](e :AliasedSQL[F, S, V]) :Y[S, V] = compositeColumn(e)
		override def arithmetic[S >: Grouped <: Single, V](e :ArithmeticSQL[F, S, V]) :Y[S, V] =
			compositeColumn(e)

		override def chain[S >: Grouped <: Single, I <: Chain, L](e :ChainSQL[F, S, I, L]) :Y[S, I ~ L] =
			composite(e)

		override def compoundSelect[V](e :CompoundSelectSQL[F, V]) :Y[Single, Rows[V]] = composite(e)
		override def concat[S >: Grouped <: Single](e :ConcatSQL[F, S]) :Y[S, String] = compositeColumn(e)
		override def condition[S >: Grouped <: Single](e :ConditionSQL[F, S]) :Y[S, Boolean] =
			compositeColumn(e)

		override def editedColumn[M[O] <: BaseColumn[X, O], X, V](e :EditedColumnSQL[F, M, X, V]) :Y[Single, X] =
			composite(e)
		override def editedLValue[M[A] <: BaseMapping[V, A], V](e :EditedLValueSQL[F, M, V]) :Y[Single, V] =
			composite(e)

		override def function[S >: Grouped <: Single, X <: Chain, Z](e :FunctionSQL[F, S, X, Z]) :Y[S, Z] =
			composite(e)

		override def inline[S >: Grouped <: Single, X](e :InlineSQL[F, S, X]) :Y[S, X] = composite(e)
		override def labeled[S >: Grouped <: Single, I <: Listing, K <: Label, L]
		                    (e :LabeledSQL[F, S, I, K, L]) :Y[S, I |~ (K :~ L)] = composite(e)

		override def rearranged[S >: Grouped <: Single, V](e :RearrangedSQL[F, S, V]) :Y[S, V] = composite(e)

		override def selectId[S >: Grouped <: Single, V](e :SelectIdSQL[F, S, V]) :Y[S, V] =
			composite(e)
	}

}






/** Base type of [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] which are composed of other
  * expressions. It is fully analogous to its supertype
  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]], redefining the implementations
  * of methods which needed narrowing of their result type to `ColumnSQL`.
  */ //todo: rename to CompositeColumn; move to ast
trait CompositeColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
	extends CompositeSQL[F, S, V]
	   with ColumnSQL[F, S, V]
	   with CompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = ColumnSQL[f, S, V] })#E]
{
	override def asSingleRow :Option[ColumnSQL[F, Single, V]] =
		if (isSingleRow) Some(this.asInstanceOf[ColumnSQL[F, Single, V]])
		else None

//	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V]

//	override def anchor(from :F) :ColumnSQL[F, S, V] =
//		if (isAnchored(from)) this else rephrase(SQLScribe.anchor(from))
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :ColumnSQL[E, S, V] =
//		rephrase(SQLScribe.expand(base))
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :ColumnSQL[E, S, V] =
//		rephrase(SQLScribe.expand(base))
//
//	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V]

	protected[sql] override def atomicSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                             (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		("(" +: spelling(this)(from, context, params)) + ")"

	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, V, R]) :R = visitor.compositeColumn(this)
	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, Y]) :Y[S, V] =
			visitor.compositeColumn(this)
}



object CompositeColumnSQL {

	/** Implementation-oriented base trait for `CompositeColumnSQL` subclasses which consist of
	  * a single subexpression column with the same value type as this trait. It is not conceptually different
	  * from `CompositeColumnSQL`, it simply offers more efficient implementations of several methods.
	  * For this reason it should be used only to derive subclasses, not as a value type. It is also
	  * similar to [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL UnaryCompositeSQL]], but
	  * the wrapped expression is a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] and thus copy constructor
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumnTemplate.reapply reapply]]
	  * also accepts a `ColumnSQL`: implementing
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL.reapply reapply]] of `UnaryCompositeSQL`
	  * would be in most cases impossible, as it provides a more generic argument of `SQLExpression`.
	  * @see [[net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.BinaryCompositeColumn]]
	  */
	//we could easily implement both this and UnaryCompositeSQL as a template class, but then we would not be able to
	// extend both (different type arguments for an invariant type of the wrapped expression).
	trait UnaryCompositeColumn[-F <: RowProduct, -S >: Grouped <: Single, X, V]
		extends CompositeColumnSQL[F, S, V]
		   with UnaryCompositeColumnTemplate[F, S, X, V,
		                                     ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = ColumnSQL[f, s, V] })#E]

	/** A template mixin classes extending
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn UnaryCompositeColumn]],
	  * whose type constructor is preserved by grounding methods. It delegates all methods declared in
	  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]] to the wrapped
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumnTemplate.value value]],
	  * and [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumnTemplate.reapply reapplies]]
	  * this type of wrapper to the result.
	  */
	trait UnaryCompositeColumnTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, V,
	                                   +Same[-f <: RowProduct, -s >: Grouped <: Single] <: ColumnSQL[f, s, V]]
		//Does not extend VariantColumnGroundingTemplate in order to not have to be a grounding specific column
		// if a subclass extends only ColumnGroundingOps[_, SQLExpression]
		extends CompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = ColumnSQL[f, S, V] })#E]
//		   with VariantColumnGroundingTemplate[F, S, V, ({ type T[-f <: RowProduct] = Same[f, S] })#T]
	{ this :Same[F, S] =>
		//todo: rename to underlying (and in UnaryCompositeColumn, too)
		protected def value :ColumnSQL[F, S, X]
		protected override def parts :Seq[ColumnSQL[F, S, _]] = PassedArray.one(value)

		override def isSingleRow :Boolean = value.isSingleRow
		override def isGround    :Boolean = value.isGround
		private[sql] override def checkIfAnchored :Boolean = value.isAnchored
		private[sql] override def checkIfAnchored(from :F) :Boolean = value.isAnchored(from)


		override def asSingleRow :Option[Same[F, Single]] =
			if (isSingleRow) Some(this.castFrom[Same[F, S], Same[F, Single]]) else None

		override def anchor(from :F) :Same[F, S] =
			if (isAnchored(from))
				this
			else value.anchor(from) match {
				case same if same eq value => this
				case anchored => reapply(anchored)
			}
	    override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Same[E, S] =
		    if (expansion.lengthDiff == 0) this.asInstanceOf[Same[E, S]]
		    else reapply(value.basedOn(base))

	    override def expand[U <: F, E <: RowProduct]
	                       (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Same[E, S] =
		    if (expansion.length == 0) this.asInstanceOf[Same[E, S]]
		    else reapply(value.expand(base))

		//Not Same[E, S] because SQLScribe gives no guarantees for the types of the returned expressions
		// other than ColumnSQL, so we want to reserve an option for subclasses to return a value of a supertype here.
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] = {
			val sql = mapper(value)
			if (sql eq value) this.asInstanceOf[Same[E, S]]
			else reapply(sql)
		}

		//We need C >: Grouped <: GlobalScope because S would be in a covariant position in e :(
		// Scala 3 trait parameter of SQLExpression[E, S, X] => SQLExpression[E, C, V] would solve this.
		// However, currently only TransformedSQL.conversion relies on the ability to wrap expressions of any scope.
		// We might move this method to TransformedSQL and leave here only one accepting SQLExpression[E, S, X].
		protected def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :Same[E, C]

		override def outerWithClause :WithClause = value.outerWithClause

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(value)

		override def contentsIsomorphic(other :CompositeSQL.__) :Boolean = other match {
			case unary :UnaryCompositeColumn[_, _, _, _] => value isomorphic unary.value
			case _ => false
		}
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case unary :UnaryCompositeColumn[_, _, _, _] @unchecked if canEqual(unary) && unary.canEqual(this) =>
				value == unary.value
			case _ => false
		}
		override def hashCode :Int = value.hashCode
	}


/*
	trait GenericColumnDecoratorTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                     +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                        <: ConvertibleColumn[f, s, v, ({ type E[X] = Same[f, s, X] })#E]]
		extends ColumnConvertingTemplate[F, S, V, ({ type E[v] = Same[F, S, v] })#E]
		   with UnaryCompositeColumnTemplate[F, S, V, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, V] })#E]
	{ self :Same[F, S, V] =>
		protected def decoration[E[-f <: RowProduct, -s >: Grouped <: Single, v] >: Same[f, s, v] <: ConvertibleColumn[f, s, v, ({ type T[X] = E[f, s, X] })#T]]
				:WrappingTransformation.Convertible[V, V, E] =
			new WrappingTransformationTemplate[V, V, E] with SQLFormIdentity[V] {
				override protected def wrap[f <: RowProduct, s >: Grouped <: Single](expr :SQLExpression[f, s, V]) =
					expr match {
						case column :ColumnSQL[f, s, V] => decorate(column)
						case _ => throw new IllegalExpressionException(
							"Cannot decorate a non column expression " + expr + ": " + expr.className + " with " +
								this.className + "."
						)
					}
			}

		override def asSingleRow :Option[Same[F, Single, V]] =
			if (isSingleRow) Some(this.asInstanceOf[Same[F, Single, V]]) else None

		protected override def convert[Y](conversion :SQLConversion[V, Y]) :Same[F, S, Y] =
			decorate(conversion(value))

		protected override def reapply[E <: RowProduct, C >: Grouped <: S](e :ColumnSQL[E, C, V]) :Same[E, C, V] =
			decorate(e)

		protected def decorate[E <: RowProduct, C >: Grouped <: Single, X](e :ColumnSQL[E, C, X]) :Same[E, C, X]

		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.Expression[F, S, Same[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
			if (other eq this)
				(leftResult(this), rightResult(other))
			else if (passCount.firstTime)
				super.reform(other)(reform, passCount)
			else
				reform(value, other)(decoration andThen leftResult, rightResult, spelling)
	}
*/




	trait BinaryCompositeColumn[-F <: RowProduct, -S >: Grouped <: Single, X, V]
		extends CompositeColumnSQL[F, S, V]
		   with BinaryCompositeColumnTemplate[F, S, X, X, V,
		                                      ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = ColumnSQL[f, s, V] })#E]

	/** Implementation oriented base trait for `CompositeColumnSQL` subclasses which consist of two column
	  * subexpressions. It is not conceptually different from `CompositeColumnSQL`, it simply offers more efficient
	  * implementations of several methods. For this reason it should only be used to derive subclasses, and not
	  * as a value type. It is also similar to
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryCompositeTemplate BinaryCompositeSQL]], but
	  * the wrapped expression is a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] and thus copy constructor
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.BinaryCompositeColumn.reapply reapply]]
	  * also accepts a `ColumnSQL`: implementing
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryCompositeTemplate.reapply reapply]] of `BinaryCompositeSQL`
	  * would be in most cases impossible, as it provides more generic arguments of `SQLExpression`.
	  */
	trait BinaryCompositeColumnTemplate[-F <: RowProduct, -S >: Grouped <: Single, L, R, V,
	                                    +Same[-f <: RowProduct, -s >: Grouped <: Single] <: ColumnSQL[f, s, V]]
		//Does not extend VariantColumnGroundingTemplate in order to not have to be grounding specific
		// if a subclass extends only ColumnGroundingOps[_, SQLExpression]
		extends CompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = ColumnSQL[f, S, V] })#E]
//		   with VariantColumnGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = Same[f, S] })#E]
	{ this :Same[F, S] =>
		protected def left  :ColumnSQL[F, S, L]
		protected def right :ColumnSQL[F, S, R]

		protected override def parts :Seq[ColumnSQL[F, S, _]] = PassedArray.two(left, right)

		override def isSingleRow :Boolean = right.isSingleRow && left.isSingleRow
		override def isGround    :Boolean = right.isGround && left.isGround
		private[ast] override def checkIfAnchored :Boolean = right.isAnchored && left.isAnchored
		private[ast] override def checkIfAnchored(from :F) :Boolean = right.isAnchored(from) && left.isAnchored(from)

		override def asSingleRow :Option[Same[F, Single]] =
			if (isSingleRow) Some(this.castFrom[Same[F, S], Same[F, Single]]) else None

		override def anchor(from :F) :Same[F, S] =
			if (isAnchored(from))
				this
			else (left.anchor(from), right.anchor(from)) match {
				case (l, r) if (l eq left) && (r eq right) => this
				case (l, r) => reapply(l, r)
			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Same[E, S] =
			if (expansion.lengthDiff == 0) this.asInstanceOf[Same[E, S]]
			else reapply(left.basedOn(base), right.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Same[E, S] =
			if (expansion.length == 0) this.asInstanceOf[Same[E, S]]
			else reapply(left.expand(base), right.expand(base))

		//Not Same[E, S] because SQLScribe gives no guarantees for the types of the returned expressions
		// other than ColumnSQL, so we want to reserve an option for subclasses to return a value of a supertype here.
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] = {
			val l = mapper(left)
			val r = mapper(right)
			if ((l eq left) & (r eq right)) this.asInstanceOf[Same[E, S]]
			else reapply(l, r)
		}

		protected def reapply[E <: RowProduct, C >: Grouped <: Single]
		                     (left :ColumnSQL[E, C, L], right :ColumnSQL[E, C, R]) :Same[E, C]

		override def outerWithClause :WithClause = left.outerWithClause ++ right.outerWithClause

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
			spelling.sqlParamCount(spelling.sqlParamCount(left)) + spelling.sqlParamCount(right)

		override def contentsIsomorphic(other :CompositeSQL.__) :Boolean = other match {
			case e :BinaryCompositeColumn[_, _, _, _] @unchecked => (left isomorphic e.left) && (right isomorphic e.right)
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :BinaryCompositeColumn[_, _, _, _] @unchecked if canEqual(other) && other.canEqual(this) =>
				left == other.left && right == other.right
			case _ => false
		}

		override def hashCode :Int = left.hashCode * 31 + right.hashCode
	}



	trait SpecificCompositeColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificAdaptedColumnVisitor[F, S, X, Y] with SpecificAliasedExpressionVisitor[F, S, X, Y]
		   with SpecificArithmeticVisitor[F, S, X, Y] with SpecificConcatVisitor[F, S, X, Y]
		   with SpecificConditionVisitor[F, S, X, Y] with SpecificCompoundSelectColumnVisitor[F, X, Y]
		   with SpecificEditedColumnVisitor[F, X, Y] with SpecificFunctionColumnVisitor[F, S, X, Y]
	{
		def compositeColumn(e :CompositeColumnSQL[F, S, X]) :Y
	}

	/** A mixin for [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor SpecificExpressionVisitor]]
	  *  which lists all direct subtypes of `CompositeColumnSQL` which do not have a multi-column counterpart
	  * (are single column by definition).
	  */
	trait MatchSpecificCompositeColumnOnly[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificCompositeColumnVisitor[F, S, X, Y]
		   with CaseSpecificAliasedExpression[F, S, X, Y] with CaseSpecificArithmetic[F, S, X, Y]
		   with CaseSpecificConcat[F, S, X, Y] with CaseSpecificEditedColumn[F, X, Y]
		   with CaseSpecificCondition[F, S, X, Y]

	trait MatchSpecificCompositeColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificCompositeColumnOnly[F, S, X, Y] with CaseSpecificAdaptedColumn[F, S, X, Y]
		   with CaseSpecificCompoundSelectColumn[F, X, Y] with CaseSpecificFunctionColumn[F, S, X, Y]

	trait CaseSpecificCompositeColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificCompositeColumn[F, S, X, Y]
	{
		override def adaptedColumn[V](e :AdaptedColumnSQL[F, S, V, X]) :Y = compositeColumn(e)
		override def alias(e :AliasedSQL[F, S, X]) :Y = compositeColumn(e)
		override def arithmetic(e :ArithmeticSQL[F, S, X]) :Y = compositeColumn(e)
		override def concat(e :ConcatSQL[F, S])(implicit isString :X =:= String) :Y =
			compositeColumn(isString.substituteContra(e :CompositeColumnSQL[F, S, String]))

		override def condition(e :ConditionSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y =
			compositeColumn(isBoolean.substituteContra(e :CompositeColumnSQL[F, S, Boolean]))

		override def compoundSelectColumn[R](e :CompoundSelectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y =
			compositeColumn(isRows.substituteContra(e :CompositeColumnSQL[F, S, Rows[R]]))

		override def functionColumn[V <: Chain](e :FunctionColumnSQL[F, S, V, X]) :Y = compositeColumn(e)

		override def editedColumn[M[A] <: BaseColumn[X, A], V](e :EditedColumnSQL[F, M, X, V]) :Y =
			compositeColumn(e)
	}
//
//
//	trait CompositeColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends AdaptedColumnVisitor[F, Y] with AliasedExpressionVisitor[F, Y] with ArithmeticVisitor[F, Y]
//		   with ConcatVisitor[F, Y] with ConditionVisitor[F, Y] with CompoundSelectColumnVisitor[F, Y]
//		   with EditedColumnVisitor[F, Y] with FunctionColumnVisitor[F, Y] with LogicalVisitor[F, Y]
//	{
//		def compositeColumn[S >: Grouped <: Single, V]
//		                   (e :CompositeColumnSQL[F, S, V]) :Y[S, V, CompositeColumnSQL[F, S, V]]
//	}
//
//	/** A mixin for [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionVisitor ExpressionVisitor]] which
//	  * lists all direct subtypes of `CompositeColumnSQL` which do not have a multi-column counterpart
//	  * (are single column by definition).
//	  */
//	trait MatchCompositeColumnOnly[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompositeColumnVisitor[F, Y]
//		   with CaseAliasedExpression[F, Y] with CaseArithmetic[F, Y] with CaseConcat[F, Y]
//		   with CaseEditedColumn[F, Y] with CaseCondition[F, Y] with CaseLogical[F, Y]
//
//	trait MatchCompositeColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CaseAdaptedColumn[F, Y] with MatchCompositeColumnOnly[F, Y] with CaseCompoundSelectColumn[F, Y]
//		   with CaseFunctionColumn[F, Y]
//
//	trait CaseCompositeColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchCompositeColumn[F, Y]
//	{
//		override def adaptedColumn[S >: Grouped <: Single, X, V]
//		                          (e :AdaptedColumnSQL[F, S, X, V]) :Y[S, V, AdaptedColumnSQL[F, S, X, V]] =
//			compositeColumn(e)
//
//		override def alias[S >: Grouped <: Single, V]
//		                  (e :AliasedSQL[F, S, V]) :Y[S, V, AliasedSQL[F, S, V]] =
//			compositeColumn(e)
//
//		override def arithmetic[S >: Grouped <: Single, V]
//		                       (e :ArithmeticSQL[F, S, V]) :Y[S, V, ArithmeticSQL[F, S, V]] = compositeColumn(e)
//
//		override def condition[S >: Grouped <: Single]
//		                      (e :ConditionSQL[F, S]) :Y[S, Boolean, ConditionSQL[F, S]] = compositeColumn(e)
//
//		override def compoundSelectColumn[R](e :CompoundSelectColumn[F, R])
//				:Y[Single, Rows[R], CompoundSelectColumn[F, R]] =
//			compositeColumn(e)
//
//		override def concat[S >: Grouped <: Single](e :ConcatSQL[F, S]) :Y[S, String, ConcatSQL[F, S]] =
//			compositeColumn(e)
//
//		override def editedColumn[M[A] <: BaseColumn[V, A], V, X]
//		                         (e :EditedColumnSQL[F, M, V, X]) :Y[Single, V, EditedColumnSQL[F, M, X, V]] =
//			compositeColumn(e)
//
//		override def functionColumn[S >: Grouped <: Single, X <: Chain, V]
//		                           (e :FunctionColumnSQL[F, S, X, V]) :Y[S, V, FunctionColumnSQL[F, S, X, V]] =
//			compositeColumn(e)
//
//		override def logical[S >: Grouped <: Single](e :LogicalSQL[F, S]) :Y[S, Boolean, LogicalSQL[F, S]] =
//			compositeColumn(e)
//	}



	trait AnyCompositeColumnVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyAdaptedColumnVisitor[F, Y] with AnyAliasedExpressionVisitor[F, Y] with AnyArithmeticVisitor[F, Y]
		   with AnyConcatVisitor[F, Y] with AnyConditionVisitor[F, Y] with AnyCompoundSelectColumnVisitor[F, Y]
		   with AnyEditedColumnVisitor[F, Y] with AnyFunctionColumnVisitor[F, Y]
	{
		def compositeColumn[S >: Grouped <: Single, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X]
	}

	/** An [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor ExpressionVisitor]] supertype
	  * introducing to it methods for all
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL CompositeColumnSQL]] subtypes
	  * which do not have a non-column equivalent (such as those implementing `Boolean`
	  * [[net.noresttherein.oldsql.sql.ast.LogicalSQL logic]]). This trait is mixed as an exception into
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.MatchAnyComposite MatchAnyComposite]] to handle
	  * these cases individually, rather than having them all lumped in together into
	  * the [[net.noresttherein.oldsql.sql.ast.CompositeSQL.AnyCompositeVisitor.composite composite]] case
	  * as `CompositeSQL`, as they would by the standard rules.
	  */
	trait MatchAnyCompositeColumnOnly[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompositeColumnVisitor[F, Y]
		   with CaseAnyAliasedExpression[F, Y] with CaseAnyArithmetic[F, Y] with CaseAnyConcat[F, Y]
		   with CaseAnyCondition[F, Y] with CaseAnyEditedColumn[F, Y]

	trait MatchAnyCompositeColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyCompositeColumnOnly[F, Y] with CaseAnyAdaptedColumn[F, Y]
		   with CaseAnyCompoundSelectColumn[F, Y] with CaseAnyFunctionColumn[F, Y]

	trait CaseAnyCompositeColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyCompositeColumn[F, Y]
	{
		override def adaptedColumn[S >: Grouped <: Single, Z, X](e :AdaptedColumnSQL[F, S, Z, X]) :Y[S, X] =
			compositeColumn(e)

		override def alias[S >: Grouped <: Single, V](e :AliasedSQL[F, S, V]) :Y[S, V] =
			compositeColumn(e)

		override def arithmetic[S >: Grouped <: Single, V](e :ArithmeticSQL[F, S, V]) :Y[S, V] =
			compositeColumn(e)

		override def concat[S >: Grouped <: Single](e :ConcatSQL[F, S]) :Y[S, String] =
			compositeColumn(e)

		override def condition[S >: Grouped <: Single](e :ConditionSQL[F, S]) :Y[S, Boolean] =
			compositeColumn(e)

		override def compoundSelectColumn[V](e :CompoundSelectColumn[F, V]) :Y[Single, Rows[V]] =
			compositeColumn(e)

		override def functionColumn[S >: Grouped <: Single, X <: Chain, Z]
		                           (e :FunctionColumnSQL[F, S, X, Z]) :Y[S, Z] = compositeColumn(e)

		override def editedColumn[M[A] <: BaseColumn[X, A], X, V](e :EditedColumnSQL[F, M, X, V]) :Y[Single, X] =
			compositeColumn(e)
	}
}

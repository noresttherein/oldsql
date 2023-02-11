package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Chain, Opt, PassedArray}
import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.exceptions.IllegalExpressionException
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.slang.classNameMethods
import net.noresttherein.oldsql.sql.{ColumnSQL, Query, RowProduct, SingleSQL, SQLExpression, WithClause}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ConvertibleColumn, ConvertingColumnTemplate, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingTemplate, Grouped, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL.{AnyAdaptedColumnVisitor, CaseAnyAdaptedColumn, CaseSpecificAdaptedColumn, SpecificAdaptedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.AdaptedSQL.{AnyAdaptedVisitor, CaseAnyAdapted, CaseSpecificAdapted, MatchAnyAdapted, MatchSpecificAdapted, SpecificAdaptedVisitor}
import net.noresttherein.oldsql.sql.ast.AliasedSQL.{AnyAliasedExpressionVisitor, CaseAnyAliasedExpression, CaseSpecificAliasedExpression, SpecificAliasedExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.{AnyArithmeticVisitor, CaseAnyArithmetic, CaseSpecificArithmetic, SpecificArithmeticVisitor}
import net.noresttherein.oldsql.sql.ast.ChainSQL.{AnyChainVisitor, MatchAnyChain, MatchSpecificChain, SpecificChainVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.{AnyCompositeColumnVisitor, MatchAnyCompositeColumnOnly, MatchSpecificCompositeColumnOnly, SpecificCompositeColumnVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.GroundingCompositeSQLTemplate
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
import net.noresttherein.oldsql.sql.mechanics.{ComposedTransformation, SpelledSQL, SQLConversion, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLTransformation.SQLDecoration






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
	   with GroundingCompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = SQLExpression[f, S, V] })#E]
{
	/** Subexpressions of this expression. */
	protected def parts :Seq[SQLExpression[F, S, _]]

	/** Constituent subexpressions ([[net.noresttherein.oldsql.sql.ast.CompositeSQL.parts parts]]) of this expression,
	  * from left to right, as they would appear in the generated SQL.
	  */
	def inOrder :Seq[SQLExpression[F, S, _]] = parts

//	override def layout   :SQLExpression[Nothing, S, V] = rephrase(SQLScribe.layout)
	/** Tells if all columns of [[net.noresttherein.oldsql.sql.ast.CompositeSQL.parts parts]] subexpressions
	  * have corresponding columns in the [[net.noresttherein.oldsql.sql.SQLExpression.split split]] column list
	  * of this expression. The order in which they occur is unspecified and this expression may introduce additional
	  * columns, not listed among `parts`' columns.
	  */
	def isShapeComposite  :Boolean = false //consider: a better name. isColumnComposite? isColumnSeq? isColumnConcat? isInline?

	/** Tells if the columns listed by [[net.noresttherein.oldsql.sql.SQLExpression.split split]] correspond one to one
	  * to columns [[net.noresttherein.oldsql.sql.ast.CompositeSQL.inOrder inOrder]]`.flatMap(_.split)`.
	  * The columns need not be the same instances, but spelling of this composite should be consistent
	  * with spelling of individual parts.
	  */
	def isInline          :Boolean = false //isFlatMap? isColumnFlatMap?
	override def isGround :Boolean = parts.forall(_.isGround)
	override def isSingleRow :Boolean = parts.forall(_.isSingleRow)
	override def asSingleRow :Option[SingleSQL[F, V]] =
		if (isSingleRow) Some(this.asInstanceOf[SingleSQL[F, V]])
		else None

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


	override def outerWithClause :WithClause = parts.view.map(_.outerWithClause).reduce(_ ++ _)

	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		(0 /: parts)(_ + spelling.sqlParamCount(_))


	/** Defaults to [[net.noresttherein.oldsql.sql.ast.CompositeSQL.canEqual canEqual]]. */
	def sameAs(other :CompositeSQL.__) :Boolean = canEqual(other)

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



object CompositeSQL {

	type __ = CompositeSQL[_ <: RowProduct, _ >: Grouped <: Single, _]

	trait GroundingCompositeSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                    +Cons[-f <: RowProduct] <: SQLExpression[f, S, V]]
		extends VariantGroundingTemplate[F, S, V, Cons]
	{ this :Cons[F] =>
		override def anchor(from :F) :Cons[F] =
			if (isAnchored(from)) this else rephrase(SQLScribe.anchor(from))

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Cons[E] =
			rephrase(SQLScribe.expand(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Cons[E] =
			rephrase(SQLScribe.expand(base))

		/** Method used in the implementation of
		  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.basedOn basedOn]],
		  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.expand expand]] and other methods recreating
		  * the same expression for a different domain clause.
		  * It should apply `mapper` to all constituting subexpressions and reassemble them into another
		  * composite expression of the same type as this one.
		  */
		@throws[IllegalExpressionException]("if no equivalent expression can be constructed from reformed subexpressions.")
		def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :Cons[E]
	}


 	/** Implementation-oriented base trait for `CompositeSQL` subclasses which consist of a single subexpression.
	  * It does not carry any additional meaning or offer new functionality, it simply provides more efficient
	  * implementations for methods inherited from `CompositeSQL`. Clients should not make assumptions
	  * that the exact semantics of methods implemented in subclasses match those defined here.
	  * @see [[net.noresttherein.oldsql.sql.ast.AdaptedSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeSQL.GenericSQLDecoratorTemplate]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryCompositeSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn]]
	  */ //todo: template Expr[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, Expr[v]] with GroundingTemplate[F, S, v, Expr]
	trait UnaryCompositeSQL[-F <: RowProduct, -S >: Grouped <: Single, X, V]
	    extends CompositeSQL[F, S, V]
	       with GroundingUnaryComposite[F, S, X, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = SQLExpression[f, s, V] })#E]
    {
		protected override def parts :Seq[SQLExpression[F, S, _]] = PassedArray.one(value)

		override def isSingleRow :Boolean = value.isSingleRow
		override def isGround    :Boolean = value.isGround
		private[sql] override def checkIfAnchored  :Boolean = value.isAnchored
		private[sql] override def checkIfAnchored(from :F) :Boolean = value.isAnchored(from)

		override def outerWithClause :WithClause = value.outerWithClause

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(value)


		override def contentsIsomorphic(other :CompositeSQL.__) :Boolean = other match {
			case unary :UnaryCompositeSQL[_, _, _, _] => value isomorphic unary.value
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case unary :UnaryCompositeSQL[_, _, _, _] if canEqual(unary) && unary.canEqual(this) =>
				value == unary.value
			case _ => false
		}
		override def hashCode :Int = value.hashCode
	}


	/** A template mixin trait for subclasses of
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL UnaryCompositeSQL]]
	  * which can wrap SQL expressions of any type (with any type arguments).
	  * It preserves type constructor `Ex` of the subclass when grounding it in other clauses.
	  * It delegates all methods declared in
	  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]] to the wrapped
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.GroundingUnaryComposite.value value]],
	  * and [[net.noresttherein.oldsql.sql.ast.CompositeSQL.GroundingUnaryComposite.reapply reapplies]]
	  * this type of wrapper to the result.
	  */
	trait GroundingUnaryComposite[-F <: RowProduct, -S >: Grouped <: Single, X, V,
	                              +Ex[-f <: RowProduct, -s >: Grouped <: Single] <: SQLExpression[f, s, V]]
		extends GroundingCompositeSQLTemplate[F, S, V, ({ type T[-f <: RowProduct] = Ex[f, S] })#T]
	{ this :Ex[F, S] =>
		//todo: rename to underlying (and in UnaryCompositeColumn, too)
		protected def value :SQLExpression[F, S, X]

		override def anchor(from :F) :Ex[F, S] = value.anchor(from) match {
			case same if same eq value => this
			case anchored => reapply(anchored)
		}

	    override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Ex[E, S] =
		    reapply(value.basedOn(base))

	    override def expand[U <: F, E <: RowProduct]
	                       (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Ex[E, S] =
		    reapply(value.expand(base))

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :Ex[E, S] =
			reapply(mapper(value))

		//We need C >: Grouped <: GlobalScope because S would be in a covariant position in e :(
		// Scala 3 trait parameter of SQLExpression[E, S, X] => SQLExpression[E, C, V] would solve this.
		// However, currently only TransformedSQL.conversion relies on the ability to wrap expressions of any scope.
		// We might move this method to TransformedSQL and leave here only one accepting SQLExpression[E, S, X].
		protected def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, X]) :Ex[E, C]
	}


	/** A template mixin trait for subclasses of
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL UnaryCompositeSQL]],
	  * which can wrap a column expression of any type (with any type arguments). The type constructor `Ex`
	  * of the subclass is preserved when converting its value
	  * with an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]] or grounding it in other clauses.
	  * The decorator delegates methods from
	  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]]
	  * and [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]] to the same method
	  * of the underlying expression, wrapping the result in an instance of the same class as this expression.
	  * Note that this means in particular that the result of converting this expression isn't (most likely)
	  * are [[net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL ConvertedColumnSQL]]. Instead, its underlying expression
	  * is a `ConvertedSQL` wrapping directly `this.value`.
	  */
	trait GenericSQLDecoratorTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                  +Ex[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                     <: ConvertibleSQL[f, s, v, ({ type T[X] = Ex[f, s, X] })#T]]
		extends ConvertingTemplate[F, S, V, ({ type E[v] = Ex[F, S, v] })#E]
		   with GroundingUnaryComposite[F, S, V, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Ex[f, s, V] })#E]
	{ self :Ex[F, S, V] =>

		def transformation :SQLTransformation[V, V] = new Decoration

		private class Decoration extends SQLDecoration[V] { decoration =>
			override type BaseResult[f <: RowProduct, s >: Grouped <: Single] = Ex[f, s, V]
			override type SQLResult[f <: RowProduct, s >: Grouped <: Single, +_ <: SQLExpression[f, s, V]] = Ex[f, s, V]
			override type QueryResult[P, +Q <: Query[P, V]] = Query[P, V]

			override def apply[f <: RowProduct, s >: Grouped <: Single, e[v] <: ConvertibleSQL[f, s, v, e]]
			                  (expr :ConvertibleSQL[f, s, V, e]) :Ex[f, s, V] =
				reapply(expr)

			/* Todo: see if we can enforce in composition SQLResult = second.SQLResult[f, s, Ex[f, s, Z]].
			 * We need it for reforming, but the problem is that it conflicts with
			 * CompositionTransformation.second.SQLResult[f, s, E] <: CompositionTransformation.SQLResult[f, s, E]
			 * for *all* E, not just for `Ex` - so while we can likely implement a dedicated transformation
			 * which has the proper SQLResult, it will not be a CompositonTransformation, and in other places we match
			 *  against it, extracting the two constituents, and relying heavily on the composition property above.
			 */
/*
			override def andThen[Z](next :SQLTransformation[V, Z]) :SQLTransformation[V, Z] {
						type BaseResult[f <: RowProduct, s >: Grouped <: Single] = next.BaseResult[f, s]
						type SQLResult[f <: RowProduct, s >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
							next.SQLResult[f, s, Ex[f, s, Z]]
					} =
				next match {
					case conversion :SQLConversion[V, Z] if conversion.isIdentity =>
						this.asInstanceOf[SQLTransformation[V, Z] {
							type BaseResult[f <: RowProduct, s >: Grouped <: Single] = next.BaseResult[f, s]
							type SQLResult[f <: RowProduct, s >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
								next.SQLResult[f, s, Ex[f, s, Z]]
						}]
					case _ =>
						new ComposedTransformation[V, V, Z] {
							override type BaseResult[f <: RowProduct, s >: Grouped <: Single] = next.BaseResult[f, s]
							override type SQLResult[f <: RowProduct, s >: Grouped <: Single, +E <: SQLExpression[f, s, Z]] =
								next.SQLResult[f, s, Ex[f, s, Z]]
							override val first  :decoration.type = decoration
							override val second :next.type = next

						}
				}
*/

			override def applyString(arg :String) :String = self.localClassName + "(" + arg + ")"
		}

		override def groundValue :Opt[V] = value.groundValue

		override def asSingleRow :Option[Ex[F, Single, V]] =
			if (isSingleRow) Some(this.asInstanceOf[Ex[F, Single, V]]) else None

		protected override def convert[Y](conversion :SQLConversion[V, Y]) :Ex[F, S, Y] =
			decorate(conversion(value))

		//not sure if we may do it safely
//		protected override def adapt[X](conversion :SQLAdaptation[V, X]) :EC[F, S, X] =
//			decorate(conversion(value))

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, V]) :Ex[E, C, V] =
			decorate(e)

		protected def decorate[E <: RowProduct, C >: Grouped <: Single, X](e :SQLExpression[E, C, X]) :Ex[E, C, X]
	}




	/** An implementation oriented base trait for `CompositeSQL` subclasses which consist of two subexpressions.
	  * It does not carry any additional meaning or offer new functionality, it simply provides more efficient
	  * implementations for methods inherited from `CompositeSQL`.
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn]]
	  */
	trait BinaryCompositeSQL[-F <: RowProduct, -S >: Grouped <: Single, L, R, V] extends CompositeSQL[F, S, V] {
		protected def left :SQLExpression[F, S, L]
		protected def right :SQLExpression[F, S, R]

		protected override def parts :Seq[SQLExpression[F, S, _]] = PassedArray.two(left, right)

		//reversed check order so that ChainEntry and similar first check the last item, and only then the prefix
		override def isSingleRow :Boolean = right.isSingleRow && left.isSingleRow
		override def isGround    :Boolean = right.isGround && left.isGround
		private[ast] override def checkIfAnchored :Boolean = right.isAnchored && left.isAnchored
		private[ast] override def checkIfAnchored(from :F) :Boolean = right.isAnchored(from) && left.isAnchored(from)

		override def anchor(from :F) :SQLExpression[F, S, V] =
			(left.anchor(from), right.anchor(from)) match {
				case (l, r) if (l eq left) && (r eq right) => this
				case (l, r) => reapply(l, r)
			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :SQLExpression[E, S, V] =
			reapply(left.basedOn(base), right.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :SQLExpression[E, S, V] =
			reapply(left.expand(base), right.expand(base))

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V] =
			reapply(mapper(left), mapper(right))

		protected def reapply[E <: RowProduct, C >: Grouped <: Single]
		                     (left :SQLExpression[E, C, L], right :SQLExpression[E, C, R]) :SQLExpression[E, C, V]


		override def outerWithClause :WithClause = left.outerWithClause ++ right.outerWithClause

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
			spelling.sqlParamCount(left) + spelling.sqlParamCount(right)

		override def contentsIsomorphic(other :CompositeSQL.__) :Boolean = other match {
			case e :BinaryCompositeSQL[_, _, _, _, _] => (left isomorphic e.left) && (right isomorphic e.right)
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :BinaryCompositeSQL[_, _, _, _, _] if canEqual(other) && other.canEqual(this) =>
				left == other.left && right == other.right
			case _ => false
		}

		override def hashCode :Int = left.hashCode * 31 + right.hashCode
	}




	trait SpecificCompositeVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificCompositeColumnVisitor[F, S, X, Y] with SpecificAdaptedVisitor[F, S, X, Y]
		   with SpecificChainVisitor[F, S, X, Y] with SpecificCompoundSelectVisitor[F, X, Y]
		   with SpecificEditedLValueVisitor[F, X, Y] with SpecificFunctionVisitor[F, S, X, Y]
		   with SpecificInlineVisitor[F, S, X, Y] with SpecificRearrangedVisitor[F, S, X, Y]
		   with SpecificSelectIdVisitor[F, S, X, Y]
   {
	   def composite(e :CompositeSQL[F, S, X]) :Y
   }

	trait MatchSpecificComposite[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificCompositeVisitor[F, S, X, Y] with MatchSpecificCompositeColumnOnly[F, S, X, Y]
		   with CaseSpecificAdapted[F, S, X, Y] with MatchSpecificChain[F, S, X, Y]
		   with CaseSpecificCompoundSelect[F, X, Y] with CaseSpecificEditedLValue[F, X, Y]
		   with CaseSpecificFunction[F, S, X, Y] with CaseSpecificInline[F, S, X, Y]
		   with CaseSpecificRearranged[F, S, X, Y] with CaseSpecificSelectId[F, S, X, Y]
	{
		override def compositeColumn(e :CompositeColumnSQL[F, S, X]) :Y = composite(e)
	}

	trait CaseSpecificComposite[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificComposite[F, S, X, Y]
	{
		override def adapted[V](e :AdaptedSQL[F, S, V, X]) :Y = composite(e)
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

		override def function[A <: Chain](e :FunctionSQL[F, S, A, X]) :Y = composite(e)
		override def inline(e :InlineSQL[F, S, X]) :Y = composite(e)

		override def editedColumn[M[O] <: BaseColumn[X, O], V](e :EditedColumnSQL[F, M, X, V]) :Y = composite(e)
		override def editedLValue[M[A] <: BaseMapping[X, A]](e :EditedLValueSQL[F, M, X]) :Y = composite(e)

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
		extends AnyCompositeColumnVisitor[F, Y] with AnyAdaptedVisitor[F, Y] with AnyChainVisitor[F, Y]
		   with AnyCompoundSelectVisitor[F, Y] with AnyEditedLValueVisitor[F, Y] with AnyFunctionVisitor[F, Y]
		   with AnyInlineVisitor[F, Y] with AnyRearrangedVisitor[F, Y] with AnySelectIdVisitor[F, Y]
	{
		def composite[S >: Grouped <: Single, X](e :CompositeSQL[F, S, X]) :Y[S, X]
	}

	trait MatchAnyComposite[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompositeVisitor[F, Y] with MatchAnyCompositeColumnOnly[F, Y]
		   with CaseAnyAdapted[F, Y] with MatchAnyChain[F, Y] with CaseAnyCompoundSelect[F, Y] with CaseAnyInline[F, Y]
		   with CaseAnyFunction[F, Y] with CaseAnyEditedLValue[F, Y] with CaseAnyRearranged[F, Y]
		   with CaseAnySelectId[F, Y]
	{
		override def compositeColumn[S >: Grouped <: Single, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X] =
			composite(e :CompositeSQL[F, S, X])
	}

	/* There is a repetition with CaseAnyCompositeColumn, but it can't be extended here, as mixing it in
	 * should always rewire the column columns through the column matcher hierarchy up to composite column*/
	trait CaseAnyComposite[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyComposite[F, Y] {
		override def adapted[S >: Grouped <: Single, Z, X](e :AdaptedSQL[F, S, Z, X]) :Y[S, X] = composite(e)
		override def alias[S >: Grouped <: Single, V](e :AliasedSQL[F, S, V]) :Y[S, V] = compositeColumn(e)
		override def arithmetic[S >: Grouped <: Single, V](e :ArithmeticSQL[F, S, V]) :Y[S, V] =
			compositeColumn(e)

		override def chain[S >: Grouped <: Single, I <: Chain, L](e :ChainSQL[F, S, I, L]) :Y[S, I ~ L] =
			composite(e)

		override def compoundSelect[V](e :CompoundSelectSQL[F, V]) :Y[Single, Rows[V]] = composite(e)
		override def concat[S >: Grouped <: Single](e :ConcatSQL[F, S]) :Y[S, String] = compositeColumn(e)
		override def condition[S >: Grouped <: Single](e :ConditionSQL[F, S]) :Y[S, Boolean] =
			compositeColumn(e)

		override def function[S >: Grouped <: Single, X <: Chain, Z](e :FunctionSQL[F, S, X, Z]) :Y[S, Z] =
			composite(e)

		override def inline[S >: Grouped <: Single, X](e :InlineSQL[F, S, X]) :Y[S, X] = composite(e)

		override def editedColumn[M[O] <: BaseColumn[X, O], X, V](e :EditedColumnSQL[F, M, X, V]) :Y[Single, X] =
			composite(e)
		override def editedLValue[M[A] <: BaseMapping[V, A], V](e :EditedLValueSQL[F, M, V]) :Y[Single, V] =
			composite(e)

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
	extends CompositeSQL[F, S, V] with ColumnSQL[F, S, V]
	   with GroundingCompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = ColumnSQL[f, S, V] })#E]
{
	override def asSingleRow :Option[ColumnSQL[F, Single, V]] =
		if (isSingleRow) Some(this.asInstanceOf[ColumnSQL[F, Single, V]])
		else None

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

	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, V, R]) :R = visitor.compositeColumn(this)
	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, Y]) :Y[S, V] =
			visitor.compositeColumn(this)

	protected[sql] override def atomicSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                             (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		("(" +: spelling(this)(from, context, params)) + ")"
}



object CompositeColumnSQL {

	/** Implementation-oriented base trait for `CompositeColumnSQL` subclasses which consist of
	  * a single subexpression column with the same value type as this trait. It is not conceptually different
	  * from `CompositeColumnSQL`, it simply offers more efficient implementations of several methods.
	  * For this reason it should be used only to derive subclasses, not as a value type. It is also
	  * similar to [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL UnaryCompositeSQL]], but
	  * the wrapped expression is a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] and thus copy constructor
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.GroundingUnaryCompositeColumn.reapply reapply]]
	  * also accepts a `ColumnSQL`: implementing
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL.reapply reapply]] of `UnaryCompositeSQL`
	  * would be in most cases impossible, as it provides a more generic argument of `SQLExpression`.
	  * @see [[net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL DecoratorColumn]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.GenericColumnDecoratorTemplate]]
	  * @see [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.BinaryCompositeColumn]]
	  */
	//we could easily implement both this and UnaryCompositeSQL as a template class, but then we would not be able to
	// extend both (different type arguments for an invariant type of the wrapped expression).
	trait UnaryCompositeColumn[-F <: RowProduct, -S >: Grouped <: Single, X, V]
		extends CompositeColumnSQL[F, S, V]
		   with GroundingUnaryCompositeColumn[F, S, X, V,
		                                      ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = SQLExpression[f, s, V] })#E]
	{
		protected override def parts :Seq[ColumnSQL[F, S, _]] = PassedArray.one(value)

		override def isSingleRow :Boolean = value.isSingleRow
		override def isGround    :Boolean = value.isGround
		private[sql] override def checkIfAnchored :Boolean = value.isAnchored
		private[sql] override def checkIfAnchored(from :F) :Boolean = value.isAnchored(from)

		override def outerWithClause :WithClause = value.outerWithClause

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(value)

		override def contentsIsomorphic(other :CompositeSQL.__) :Boolean = other match {
			case unary :UnaryCompositeColumn[_, _, _, _] => value isomorphic unary.value
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case unary :UnaryCompositeColumn[_, _, _, _] if canEqual(unary) && unary.canEqual(this) =>
				value == unary.value
			case _ => false
		}

		override def hashCode :Int = value.hashCode
	}


	/** A template mixin classes extending
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn UnaryCompositeColumn]],
	  * whose type constructor is preserved by grounding methods. It delegates all methods declared in
	  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]] to the wrapped
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.GroundingUnaryCompositeColumn.value value]],
	  * and [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.GroundingUnaryCompositeColumn.reapply reapplies]]
	  * this type of wrapper to the result.
	  */
	trait GroundingUnaryCompositeColumn[-F <: RowProduct, -S >: Grouped <: Single, X, V,
	                                    +Ex[-f <: RowProduct, -s >: Grouped <: Single] <: ColumnSQL[f, s, V]]
		extends GroundingCompositeSQLTemplate[F, S, V, ({ type T[-f <: RowProduct] = Ex[f, S] })#T]
	{ this :Ex[F, S] =>
		//todo: rename to underlying (and in UnaryCompositeColumn, too)
		protected def value :ColumnSQL[F, S, X]

		override def anchor(from :F) :Ex[F, S] = value.anchor(from) match {
			case same if same eq value => this
			case anchored => reapply(anchored)
		}

	    override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Ex[E, S] =
		    reapply(value.basedOn(base))

	    override def expand[U <: F, E <: RowProduct]
	                       (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Ex[E, S] =
		    reapply(value.expand(base))

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :Ex[E, S] =
			reapply(mapper(value))

		//We need C >: Grouped <: GlobalScope because S would be in a covariant position in e :(
		// Scala 3 trait parameter of SQLExpression[E, S, X] => SQLExpression[E, C, V] would solve this.
		// However, currently only TransformedSQL.conversion relies on the ability to wrap expressions of any scope.
		// We might move this method to TransformedSQL and leave here only one accepting SQLExpression[E, S, X].
		protected def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :Ex[E, C]
	}


	/** A template mixin trait for subclasses of
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn UnaryCompositeColumn]],
	  * which can wrap a column expression of any type (with any type arguments). The type constructor `Ex`
	  * of the subclass is preserved when converting its value
	  * with an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]] or grounding it in other clauses.
	  * The decorator delegates methods from
	  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]]
	  * and [[net.noresttherein.oldsql.sql.ColumnSQL.ConvertingColumnTemplate ConvertingColumnTemplate]] to the same method
	  * of the underlying expression, wrapping the result in an instance of the same class as this expression.
	  * Note that this means in particular that the result of converting this expression isn't (most likely)
	  * are [[net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL ConvertedColumnSQL]]. Instead, its underlying expression
	  * is a `ConvertedSQL` wrapping directly `this.value`.
	  *
	  * This trait is almost an exact clone of
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.GenericSQLDecoratorTemplate GenericSQLDecoratorTemplate]],
	  * but method [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.GenericColumnDecoratorTemplate.decorate decorate]]
	  * accepts only [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] instances, rather than any expressions.
	  */
	trait GenericColumnDecoratorTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                     +Ex[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                        <: ConvertibleColumn[f, s, v, ({ type E[X] = Ex[f, s, X] })#E]]
		extends ConvertingColumnTemplate[F, S, V, ({ type E[v] = Ex[F, S, v] })#E]
		   with GroundingUnaryCompositeColumn[F, S, V, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Ex[f, s, V] })#E]
	{ this :Ex[F, S, V] =>
		override def asSingleRow :Option[Ex[F, Single, V]] =
			if (isSingleRow) Some(this.asInstanceOf[Ex[F, Single, V]]) else None

		protected override def convert[Y](conversion :SQLConversion[V, Y]) :Ex[F, S, Y] =
			reapply(conversion(value))

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, V]) :Ex[E, C, V] =
			decorate(e)

		protected def decorate[E <: RowProduct, C >: Grouped <: Single, X](e :ColumnSQL[E, C, X]) :Ex[E, C, X]
	}




	/** Implementation oriented base trait for `CompositeColumnSQL` subclasses which consist of two column
	  * subexpressions. It is not conceptually different from `CompositeColumnSQL`, it simply offers more efficient
	  * implementations of several methods. For this reason it should only be used to derive subclasses, and not
	  * as a value type. It is also similar to
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryCompositeSQL BinaryCompositeSQL]], but
	  * the wrapped expression is a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] and thus copy constructor
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.BinaryCompositeColumn.reapply reapply]]
	  * also accepts a `ColumnSQL`: implementing
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryCompositeSQL.reapply reapply]] of `BinaryCompositeSQL`
	  * would be in most cases impossible, as it provides more generic arguments of `SQLExpression`.
	  */
	trait BinaryCompositeColumn[-F <: RowProduct, -S >: Grouped <: Single, X, V]
		extends CompositeColumnSQL[F, S, V]
	{
		protected def left  :ColumnSQL[F, S, X]
		protected def right :ColumnSQL[F, S, X]

		protected override def parts :Seq[ColumnSQL[F, S, X]] = PassedArray.two(left, right)

		override def isSingleRow :Boolean = right.isSingleRow && left.isSingleRow
		override def isGround    :Boolean = right.isGround && left.isGround
		private[ast] override def checkIfAnchored :Boolean = right.isAnchored && left.isAnchored
		private[ast] override def checkIfAnchored(from :F) :Boolean = right.isAnchored(from) && left.isAnchored(from)

		override def anchor(from :F) :ColumnSQL[F, S, V] =
			(left.anchor(from), right.anchor(from)) match {
				case (l, r) if (l eq left) && (r eq right) => this
				case (l, r) => reapply(l, r)
			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :ColumnSQL[E, S, V] =
			reapply(left.basedOn(base), right.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :ColumnSQL[E, S, V] =
			reapply(left.expand(base), right.expand(base))

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] =
			reapply(mapper(left), mapper(right))

		protected def reapply[E <: RowProduct, C >: Grouped <: Single]
		                     (left :ColumnSQL[E, C, X], right :ColumnSQL[E, C, X]) :ColumnSQL[E, C, V]

		override def outerWithClause :WithClause = left.outerWithClause ++ right.outerWithClause

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
			spelling.sqlParamCount(spelling.sqlParamCount(left)) + spelling.sqlParamCount(right)

		override def contentsIsomorphic(other :CompositeSQL.__) :Boolean = other match {
			case e :BinaryCompositeColumn[_, _, _, _] => (left isomorphic e.left) && (right isomorphic e.right)
			case _ => false
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :BinaryCompositeColumn[_, _, _, _] if canEqual(other) && other.canEqual(this) =>
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
		extends MatchSpecificCompositeColumnOnly[F, S, X, Y]
		   with CaseSpecificAdaptedColumn[F, S, X, Y] with CaseSpecificCompoundSelectColumn[F, X, Y]
		   with CaseSpecificFunctionColumn[F, S, X, Y]

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
		extends MatchAnyCompositeColumnOnly[F, Y]
		   with CaseAnyAdaptedColumn[F, Y] with CaseAnyCompoundSelectColumn[F, Y] with CaseAnyFunctionColumn[F, Y]

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

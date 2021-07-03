package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression, WithClause}
import net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn
import net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn.{AliasedColumnVisitor, CaseAliasedColumn}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.ast.ArithmeticSQL.{ArithmeticVisitor, CaseArithmetic}
import net.noresttherein.oldsql.sql.ast.ChainSQL.{CaseChain, ChainVisitor}
import net.noresttherein.oldsql.sql.ast.CompoundSelectColumn.{CaseCompoundSelectColumn, CompoundSelectColumnVisitor}
import net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.{CaseCompoundSelect, CompoundSelectVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL.{CompositeColumnVisitor, MatchOnlyCompositeColumn}
import net.noresttherein.oldsql.sql.ast.ConcatSQL.{CaseConcat, ConcatVisitor}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.{CaseCondition, ConditionVisitor}
import net.noresttherein.oldsql.sql.ast.ConversionSQL.{CaseColumnConversion, CaseConversion, ColumnConversionSQL, ColumnConversionVisitor, ConversionVisitor}
import net.noresttherein.oldsql.sql.ast.FunctionSQL.{CaseFunction, FunctionColumnSQL, FunctionVisitor}
import net.noresttherein.oldsql.sql.ast.FunctionSQL.FunctionColumnSQL.{CaseFunctionColumn, FunctionColumnVisitor}
import net.noresttherein.oldsql.sql.ast.LogicalSQL.{CaseLogical, LogicalVisitor}
import net.noresttherein.oldsql.sql.ast.QuerySQL.Rows
import net.noresttherein.oldsql.sql.ast.TupleSQL.{CaseTuple, TupleVisitor}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation






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
  * [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionVisitor visitors]] of the `SQLExpression` type hierarchy,
  * which recursively reapply themselves to the subexpression of this expression (possibly producing another
  * `SQLExpression` by reapplying the same type of the composite expression to the mapped subexpressions.
  * The additional methods defined here facilitate this behaviour as well as structural comparison
  * for [[net.noresttherein.oldsql.sql.ast.CompositeSQL.equals equals]] and
  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.isomorphic isomorphic]].
  */ //todo: move it up to package sql.ast
trait CompositeSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, V] extends SQLExpression[F, S, V] {
	protected def parts :Seq[SQLExpression[F, S, _]]

	def inOrder :Seq[SQLExpression[F, S, _]] = parts

	/** Method used in the implementation of
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.basedOn basedOn]] and
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.expand expand]].
	  * It should apply `mapper` to all constituting subexpressions and reassemble them into another
	  * composite expression of the same type as this one.
	  */
	def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V]

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SQLExpression[E, S, V] =
		rephrase(SQLScribe.expand(base))

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :SQLExpression[E, S, V] =
		rephrase(SQLScribe.expand(base))

	override def isGlobal :Boolean = parts.forall(_.isGlobal)

	override def asGlobal :Option[GlobalSQL[F, V]] =
		if (isGlobal) Some(this.asInstanceOf[GlobalSQL[F, V]])
		else None

	//fixme: concurrency semantics verification; review overrides in subclasses
	private var knownIfAnchored = false
	private var anchored_? :Boolean = _

	override def isAnchored :Boolean = {
		if (!knownIfAnchored) {
			anchored_? = checkIfAnchored
			knownIfAnchored = true
		}
		anchored_?
	}
	protected[sql] def checkIfAnchored :Boolean = parts.forall(_.isAnchored)

	override def isAnchored(from :F) :Boolean = parts.forall(_.isAnchored(from))

	override def anchor(from :F) :SQLExpression[F, S, V] =
		if (isAnchored(from)) this else rephrase(SQLScribe.anchor(from))


//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionVisitor[F, Y]) :Y[S, V] =
//			matcher.composite(this)

	override def withClause :WithClause = parts.view.map(_.withClause).reduce(_ ++ _)

	protected override def reverseCollect[X](fun: PartialFunction[SQLExpression.*, X], acc: List[X]): List[X] =
		(super.reverseCollect(fun, acc) /: inOrder) {
			(collected, member) => member.reverseCollectForwarder(fun, collected)
		}


	/** Defaults to [[net.noresttherein.oldsql.sql.ast.CompositeSQL.canEqual canEqual]]. */
	def sameAs(other :CompositeSQL.*) :Boolean = canEqual(other)

	private[oldsql] override def equivalent(expression: SQLExpression.*): Boolean = expression match {
		case e :CompositeSQL.* =>
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
	override def isomorphic(expression :SQLExpression.*) :Boolean = expression match {
		case e :CompositeSQL.* =>
			(this eq e) || sameAs(e) && e.sameAs(this) && contentsIsomorphic(e)
		case _ => false
	}


	private[oldsql] def contentsEquivalent(other :CompositeSQL.*) :Boolean =
		parts.size == other.parts.size &&
			parts.forall(e => other.parts.exists(_ equivalent e)) &&
			other.parts.forall(e => parts.exists(_ equivalent e))

	/** Verifies that the [[net.noresttherein.oldsql.sql.ast.CompositeSQL.parts parts]] lists
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



object CompositeSQL {

	type * = CompositeSQL[_ <: RowProduct, _ >: LocalScope <: GlobalScope, _]


	/** Implementation-oriented base trait for `CompositeSQL` subclasses which consist of a single subexpression.
	  * It is unwise to reference this type directly and rely on any
	  * particular expression type extending it (other than in having `left` and `right` subexpression properties),
	  * as it is considered an implementation detail and is subject to change.
	  */
	trait UnaryOperatorSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, V] extends CompositeSQL[F, S, V] {

		val value :SQLExpression[F, S, X]
		protected override def parts :Seq[SQLExpression[F, S, X]] = value::Nil

		override def isGlobal :Boolean = value.isGlobal

		override def isAnchored :Boolean = value.isAnchored
		override def isAnchored(from :F) :Boolean = value.isAnchored(from)

		override def anchor(from :F) :SQLExpression[F, S, V] = value.anchor(from) match {
			case same if same eq value => this
			case anchored => reapply(anchored)
		}

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V] =
			reapply(mapper(value))

		protected def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                     (e :SQLExpression[E, C, X]) :SQLExpression[E, C, V]

		override def split(implicit scope :OperationType) :Seq[ColumnSQL[F, S, _]] = value.split
//				throw new InseparableExpressionException("This multi column expression " + this + " cannot be split.")

		override def withClause :WithClause = value.withClause

		override def columnCount(implicit spelling :SQLSpelling) :Int = value.columnCount


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

		override def isGlobal :Boolean = left.isGlobal && right.isGlobal
		override def isAnchored :Boolean = left.isAnchored && right.isAnchored
		override def isAnchored(from :F) :Boolean = left.isAnchored(from) && right.isAnchored(from)

		override def anchor(from :F) :SQLExpression[F, S, V] = (left.anchor(from), right.anchor(from)) match {
			case (l, r) if (l eq left) && (r eq right) => this
			case (l, r) => reapply(l, r)
		}

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, V] =
			reapply(mapper(left), mapper(right))

		protected def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
		                     (left :SQLExpression[E, C, X], right :SQLExpression[E, C, X]) :SQLExpression[E, C, V]

		override def withClause :WithClause = left.withClause ++ right.withClause

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




	/** Base type of [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] which are composed of other
	  * expressions. It is fully analogous to its supertype
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]], redefining the implementations
	  * of methods which needed narrowing of their result type to `ColumnSQL`.
	  */
	trait CompositeColumnSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X]
		extends CompositeSQL[F, S, X] with ColumnSQL[F, S, X]
	{
		override def asGlobal :Option[ColumnSQL[F, GlobalScope, X]] =
			if (isGlobal) Some(this.asInstanceOf[ColumnSQL[F, GlobalScope, X]])
			else None

		override def anchor(from :F) :ColumnSQL[F, S, X] = rephrase(SQLScribe.anchor(from))

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, X]

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnSQL[E, S, X] =
			rephrase(SQLScribe.expand(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :ColumnSQL[E, S, X] =
			rephrase(SQLScribe.expand(base))

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnVisitor[F, Y]) :Y[S, X] =
//			matcher.composite(this)

		override def inParens[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                        (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			("(" +: defaultSpelling(from, context, params)) + ")"
	}



	object CompositeColumnSQL {

		/** Implementation-oriented base trait for `CompositeSQL` subclasses which consist of a single subexpression
		  * column with the same value type as this trait. It is unwise to reference this type directly and rely on any
		  * particular expression type expanding it (other than in having the `value` subexpression property),
		  * as it is considered an implementation detail and is subject to change.
		  */
		trait UnaryColumnOperator[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, V]
			extends CompositeColumnSQL[F, S, V]
		{
			val value :ColumnSQL[F, S, X]
			protected override def parts :Seq[ColumnSQL[F, S, X]] = value::Nil

			override def isGlobal :Boolean = value.isGlobal
			override def isAnchored :Boolean = value.isAnchored
			override def isAnchored(from :F) :Boolean = value.isAnchored(from)

			override def anchor(form :F) :ColumnSQL[F, S, V] = value.anchor(form) match {
				case same if same eq value => this
				case anchored => reapply(anchored)
			}

			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] =
				reapply(mapper(value))

			protected def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
			                     (e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, V]

			override def withClause :WithClause = value.withClause


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
		  * particular expression type expanding it (other than in having `left` and `right` subexpression properties),
		  * as it is considered an implementation detail and is subject to change.
		  */
		trait BinaryColumnOperator[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, V]
			extends CompositeColumnSQL[F, S, V]
		{
			val left :ColumnSQL[F, S, X]
			val right :ColumnSQL[F, S, X]

			protected override def parts :Seq[ColumnSQL[F, S, X]] = left::right::Nil

			override def isGlobal :Boolean = left.isGlobal && right.isGlobal
			override def isAnchored :Boolean = left.isAnchored && right.isAnchored
			override def isAnchored(from :F) :Boolean = left.isAnchored(from) && right.isAnchored(from)

			override def anchor(from :F) :ColumnSQL[F, S, V] = (left.anchor(from), right.anchor(from)) match {
				case (l, r) if (l eq left) && (r eq right) => this
				case (l, r) => reapply(l, r)
			}

			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, V] =
				reapply(mapper(left), mapper(right))

			protected def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
			                     (left :ColumnSQL[E, C, X], right :ColumnSQL[E, C, X]) :ColumnSQL[E, C, V]

			override def withClause :WithClause = left.withClause ++ right.withClause

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

		trait CompositeColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends AliasedColumnVisitor[F, Y] with ArithmeticVisitor[F, Y] with ColumnConversionVisitor[F, Y]
			   with ConcatVisitor[F, Y] with ConditionVisitor[F, Y] with CompoundSelectColumnVisitor[F, Y]
			   with FunctionColumnVisitor[F, Y] with LogicalVisitor[F, Y]
		{
			def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :Y[S, X]
		}

		trait MatchOnlyCompositeColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompositeColumnVisitor[F, Y]
			   with CaseAliasedColumn[F, Y] with CaseArithmetic[F, Y] with CaseConcat[F, Y] with CaseCondition[F, Y]
			   with CaseLogical[F, Y]


		trait MatchCompositeColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchOnlyCompositeColumn[F, Y] with CaseColumnConversion[F, Y] with CaseCompoundSelectColumn[F, Y]
			   with CaseFunctionColumn[F, Y]

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

			override def function[S >: LocalScope <: GlobalScope, X <: Chain, Z](e :FunctionColumnSQL[F, S, X, Z]) :Y[S, Z] =
				composite(e)

			override def logical[S >: LocalScope <: GlobalScope](e :LogicalSQL[F, S]) :Y[S, Boolean] = composite(e)

			override def compoundSelect[V](e :CompoundSelectColumn[F, V]) :Y[GlobalScope, Rows[V]] =
				composite(e)
		}
	}




	trait CompositeVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends CompositeColumnVisitor[F, Y] with ConversionVisitor[F, Y] with FunctionVisitor[F, Y]
		   with CompoundSelectVisitor[F, Y] with TupleVisitor[F, Y] with ChainVisitor[F, Y]
	{
		def composite[S >: LocalScope <: GlobalScope, X](e :CompositeSQL[F, S, X]) :Y[S, X]
	}

	trait MatchComposite[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends CompositeVisitor[F, Y]
		with CaseChain[F, Y] with CaseConversion[F, Y] with CaseCompoundSelect[F, Y] with CaseFunction[F, Y]
		with CaseTuple[F, Y] with MatchOnlyCompositeColumn[F, Y]
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

		override def chain[S >: LocalScope <: GlobalScope, I <: Chain, L](e :ChainSQL[F, S, I, L]) :Y[S, I ~ L] =
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

package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.Bug
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLBoolean, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLBoolean.{False, True}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}
import net.noresttherein.oldsql.sql.ast.AndSQL.{AnyAndVisitor, CaseAnyAnd, CaseSpecificAnd, SpecificAndVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn
import net.noresttherein.oldsql.sql.ast.NotSQL.{AnyNotVisitor, CaseAnyNot, CaseSpecificNot, SpecificNotVisitor}
import net.noresttherein.oldsql.sql.ast.OrSQL.{AnyOrVisitor, CaseAnyOr, CaseSpecificOr, SpecificOrVisitor}
import net.noresttherein.oldsql.sql.ast.XorSQL.{AnyXorVisitor, CaseAnyXor, CaseSpecificXor, SpecificXorVisitor}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** Base trait for SQL expressions implementing Boolean algebra. */
trait LogicalSQL[-F <: RowProduct, -S >: Grouped <: Single] extends ConditionSQL[F, S] {
	override def selectForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]

//	override def visit[Y[-_ >: Grouped <: GlobalScope, _]](matcher :AnyColumnVisitor[F, Y]) :Y[S, Boolean] =
//		matcher.logical(this)
}






object LogicalSQL {
	trait SpecificLogicalVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificNotVisitor[F, S, X, Y] with SpecificAndVisitor[F, S, X, Y]
		   with SpecificOrVisitor[F, S, X, Y] with SpecificXorVisitor[F, S, X, Y]
	{
		def logical(e :LogicalSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y
	}
	trait MatchSpecificLogical[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificLogicalVisitor[F, S, X, Y]
		   with CaseSpecificNot[F, S, X, Y] with CaseSpecificAnd[F, S, X, Y]
		   with CaseSpecificOr[F, S, X, Y] with CaseSpecificXor[F, S, X, Y]

	trait CaseSpecificLogical[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificLogical[F, S, X, Y]
	{
		override def not(e :NotSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y = logical(e)
		override def and(e :AndSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y = logical(e)
		override def or(e :OrSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y = logical(e)
		override def xor(e :XorSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y = logical(e)
	}
//
//
//	trait LogicalVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends NotVisitor[F, R] with AndVisitor[F, R] with OrVisitor[F, R] with XorVisitor[F, R]
//	{
//		def logical[S >: Grouped <: Single](e :LogicalSQL[F, S]) :R[S, Boolean, LogicalSQL[F, S]]
//	}
//	trait MatchLogical[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends LogicalVisitor[F, R] with CaseNot[F, R] with CaseAnd[F, R] with CaseOr[F, R] with CaseXor[F, R]
//
//	trait CaseLogical[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends MatchLogical[F, R]
//	{
//		override def not[S >: Grouped <: Single](e :NotSQL[F, S]) :R[S, Boolean, NotSQL[F, S]] = logical(e)
//		override def and[S >: Grouped <: Single](e :AndSQL[F, S]) :R[S, Boolean, AndSQL[F, S]] = logical(e)
//		override def or[S >: Grouped <: Single](e :OrSQL[F, S]) :R[S, Boolean, OrSQL[F, S]] = logical(e)
//		override def xor[S >: Grouped <: Single](e :XorSQL[F, S]) :R[S, Boolean, XorSQL[F, S]] = logical(e)
//	}


	trait AnyLogicalVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyNotVisitor[F, Y] with AnyAndVisitor[F, Y] with AnyOrVisitor[F, Y] with AnyXorVisitor[F, Y]
	{
		def logical[S >: Grouped <: Single](e :LogicalSQL[F, S]) :Y[S, Boolean]
	}

	trait MatchAnyLogical[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnyLogicalVisitor[F, Y]
		with CaseAnyNot[F, Y] with CaseAnyAnd[F, Y] with CaseAnyOr[F, Y] with CaseAnyXor[F, Y]

	trait CaseAnyLogical[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyLogical[F, Y] {
		override def not[S >: Grouped <: Single](e :NotSQL[F, S]) :Y[S, Boolean] = logical(e)
		override def and[S >: Grouped <: Single](e :AndSQL[F, S]) :Y[S, Boolean] = logical(e)
		override def or[S >: Grouped <: Single](e :OrSQL[F, S]) :Y[S, Boolean] = logical(e)
		override def xor[S >: Grouped <: Single](e :XorSQL[F, S]) :Y[S, Boolean] = logical(e)
	}

}






final case class NotSQL[-F <: RowProduct, -S >: Grouped <: Single](value :ColumnSQL[F, S, Boolean])
	extends UnaryCompositeColumn[F, S, Boolean, Boolean] with LogicalSQL[F, S]
{
	override def groundValue :Opt[Boolean] = value.groundValue.map(!_)

	override def unary_![E <: F, O >: Grouped <: S](implicit ev :Boolean =:= Boolean) :ColumnSQL[E, O, Boolean] =
		value

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLBoolean[E, C]) :SQLBoolean[E, C] =
		NotSQL(e)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				(spelling.NOT + " ") +: value.atomicSpelling(from, context, params)

	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] =
		visitor.not(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.not(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.not(this)

	override def toString :String = "NOT " + value
}



object NotSQL {
	trait SpecificNotVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def not(e :NotSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificNot[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificNotVisitor[F, S, X, Y]
	type CaseSpecificNot[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificNotVisitor[F, S, X, Y]
//
//	trait NotVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def not[S >: Grouped <: Single](e :NotSQL[F, S]) :R[S, Boolean, NotSQL[F, S]]
//	}
//	type MatchNot[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		NotVisitor[F, R]
//	type CaseNot[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		NotVisitor[F, R]

	trait AnyNotVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def not[S >: Grouped <: Single](e :NotSQL[F, S]) :Y[S, Boolean]
	}
	type MatchAnyNot[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyNotVisitor[F, Y]
	type CaseAnyNot[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyNotVisitor[F, Y]
}





private[ast] trait BinaryLogicalOperatorSQL[-F <: RowProduct, -S  >: Grouped <: Single]
	extends LogicalSQL[F, S]
{
	protected override def parts :List[ColumnSQL[F, S, Boolean]]
	@inline final def conditions :Seq[ColumnSQL[F, S, Boolean]] = inOrder
	override def inOrder :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse


	override def groundValue :Opt[Boolean] = (Opt(false) /: parts.view.map(_.groundValue)) {
		case (Got(l), Got(r)) => Got(apply(l, r))
		case _ => Lack
	}
	protected def apply(left :Boolean, right :Boolean) :Boolean

	override def anchor(from :F) :SQLBoolean[F, S] =
		if (isAnchored(from)) this
		else reapply(parts.map(_.anchor(from)))

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :ColumnSQL[E, S, Boolean] =
		reapply(parts.map(_.basedOn(base)))

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :ColumnSQL[E, S, Boolean] =
		reapply(parts.map(_.expand(base)))

	protected override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
		reapply(parts.map(mapper(_)))

	protected def reapply[E <: RowProduct, A >: Grouped <: S]
	                     (conditions :List[ColumnSQL[E, A, Boolean]]) :ColumnSQL[E, A, Boolean]


	protected def symbol(implicit spelling :SQLSpelling) :String

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val spell = spelling.inOperand
		parts.scanLeft(SpelledSQL(spelling.TRUE, context)) { //we pass the context in the reverse direction, but it's ok
			(sql, bool) => bool.atomicSpelling(from, sql.context, params)(spell)
		} match {
			case Seq(empty) => empty
			case Seq(_, t @ _*) => t.reduce {
				(_2, _1) => SpelledSQL(_1.sql +: (" " + symbol + " ") +: _2.sql, _1.setter + _2.setter, _1.context)
			}
			case _ => throw Bug("empty scanLeft result for " + parts + ".")
		}
	}
}




sealed class AndSQL[-F <: RowProduct, -S >: Grouped <: Single] private
                   (protected override val parts :List[ColumnSQL[F, S, Boolean]])
	extends BinaryLogicalOperatorSQL[F, S]
{
	override def &&[E <: F, O >: Grouped <: S]
	               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean)
			:ColumnSQL[E, O, Boolean] =
		other match {
			case True => this
			case False => other
			case and :AndSQL[E, O] => ((this :ColumnSQL[E, O, Boolean]) /: and.inOrder)(_ && _)
			case _ if parts.contains(other) => this
			case _ => new AndSQL(other :: parts)
		}

	override protected def apply(left :Boolean, right :Boolean) :Boolean = left && right

	override protected def reapply[E <: RowProduct, A >: Grouped <: S]
	                              (conditions :List[ColumnSQL[E, A, Boolean]]) :ColumnSQL[E, A, Boolean] =
		new AndSQL(conditions)

	protected override def symbol(implicit spelling :SQLSpelling) :String = spelling.AND

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] =
		visitor.and(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.and(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.and(this)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case and :AndSQL[_, _] if and canEqual this => and.parts == parts
		case _ => false
	}

	override def hashCode :Int = parts.hashCode

	override def toString :String = parts.reverse.mkString("(", " AND ", ")")
}



object AndSQL {
	def apply[F <: RowProduct, S >: Grouped <: Single](conditions :ColumnSQL[F, S, Boolean]*) :AndSQL[F, S] =
		new AndSQL(conditions.toList.reverse) {
			override val inOrder = conditions
		}

	def unapplySeq[F <: RowProduct, S >: Grouped <: Single](e :AndSQL[F, S]) :Seq[ColumnSQL[F, S, Boolean]] =
		e.conditions

	def unapply[F <: RowProduct, S >: Grouped <: Single](e :SQLExpression[F, S, _])
			:Opt[Seq[ColumnSQL[F, S, Boolean]]] =
		e match {
			case and :AndSQL[F, S] => Got(and.conditions)
			case _ => Lack
		}


	trait SpecificAndVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def and(e :AndSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificAnd[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificAndVisitor[F, S, X, Y]
	type CaseSpecificAnd[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificAndVisitor[F, S, X, Y]
//
//	trait AndVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def and[S >: Grouped <: Single](e :AndSQL[F, S]) :R[S, Boolean, AndSQL[F, S]]
//	}
//	type MatchAnd[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		AndVisitor[F, R]
//	type CaseAnd[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		AndVisitor[F, R]

	trait AnyAndVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def and[S >: Grouped <: Single](e :AndSQL[F, S]) :Y[S, Boolean]
	}
	type MatchAnyAnd[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyAndVisitor[F, Y]
	type CaseAnyAnd[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyAndVisitor[F, Y]
}






sealed class OrSQL[-F <: RowProduct, -S >: Grouped <: Single] private
                  (protected override val parts :List[ColumnSQL[F, S, Boolean]])
	extends BinaryLogicalOperatorSQL[F, S]
{
	override def ||[E <: F, O >: Grouped <: S]
	               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
			case True => other
			case False => this
			case or :OrSQL[E, O] => ((this :ColumnSQL[E, O, Boolean]) /: or.inOrder)(_ || _)
			case _ if parts contains other => this
			case _ => new OrSQL(other :: parts)
		}

	override protected def apply(left :Boolean, right :Boolean) :Boolean = left || right

	override protected def reapply[E <: RowProduct, A >: Grouped <: S]
	                              (conditions :List[ColumnSQL[E, A, Boolean]]) :ColumnSQL[E, A, Boolean] =
		new OrSQL(conditions)

	protected override def symbol(implicit spelling :SQLSpelling) :String = spelling.OR


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.or(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.or(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.or(this)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case or :OrSQL[_, _] if or canEqual this => or.parts == parts
		case _ => false
	}

	override def hashCode :Int = parts.hashCode

	override def toString :String = parts.reverse.mkString("(", ") OR (", ")")
}



object OrSQL {
	def apply[F <: RowProduct, S >: Grouped <: Single](conditions :ColumnSQL[F, S, Boolean]*) :OrSQL[F, S] =
		new OrSQL(conditions.view.reverse.toList) {
			override val inOrder = conditions
		}

	def unapplySeq[F <: RowProduct, S >: Grouped <: Single](or :OrSQL[F, S]) :Seq[ColumnSQL[F, S, Boolean]] =
		or.conditions

	def unapply[F <: RowProduct, S >: Grouped <: Single](e :SQLExpression[F, S, _])
			:Opt[Seq[ColumnSQL[F, S, Boolean]]] =
		e match {
			case or :OrSQL[F, S] => Got(or.conditions)
			case _ => Lack
		}

	trait SpecificOrVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def or(e :OrSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificOr[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificOrVisitor[F, S, X, Y]
	type CaseSpecificOr[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificOrVisitor[F, S, X, Y]
//
//	trait OrVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def or[S >: Grouped <: Single](e :OrSQL[F, S]) :R[S, Boolean, OrSQL[F, S]]
//	}
//	type MatchOr[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		OrVisitor[F, R]
//	type CaseOr[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		OrVisitor[F, R]

	trait AnyOrVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def or[S >: Grouped <: Single](e :OrSQL[F, S]) :Y[S, Boolean]
	}
	type MatchAnyOr[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyOrVisitor[F, Y]
	type CaseAnyOr[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyOrVisitor[F, Y]
}






sealed class XorSQL[-F <: RowProduct, -S >: Grouped <: Single] private
                   (protected override val parts :List[ColumnSQL[F, S, Boolean]])
	extends BinaryLogicalOperatorSQL[F, S]
{
	override def ^[E <: F, O >: Grouped <: S]
	               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
//			case True => other
//			case False => this
			case xor :XorSQL[E, O] => new XorSQL(xor.parts ::: parts)
			case _ if parts contains other => this
			case _ => new XorSQL(other :: parts)
		}

	override protected def apply(left :Boolean, right :Boolean) :Boolean = left ^ right

	override protected def reapply[E <: RowProduct, A >: Grouped <: S]
	                              (conditions :List[ColumnSQL[E, A, Boolean]]) :ColumnSQL[E, A, Boolean] =
		new XorSQL(conditions)

	protected override def symbol(implicit spelling :SQLSpelling) :String = spelling.XOR

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.xor(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.xor(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.xor(this)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case xor :XorSQL[_, _] if xor canEqual this => xor.parts == parts
		case _ => false
	}

	override def hashCode :Int = parts.hashCode

	override def toString :String = parts.reverse.mkString("(", ") XOR (", ")")
}



object XorSQL {
	def apply[F <: RowProduct, S >: Grouped <: Single](conditions :ColumnSQL[F, S, Boolean]*) :XorSQL[F, S] =
		new XorSQL(conditions.view.reverse.toList) {
			override val inOrder = conditions
		}

	def unapplySeq[F <: RowProduct, S >: Grouped <: Single](or :XorSQL[F, S]) :Seq[ColumnSQL[F, S, Boolean]] =
		or.conditions

	def unapply[F <: RowProduct, S >: Grouped <: Single](e :SQLExpression[F, S, _])
			:Opt[Seq[ColumnSQL[F, S, Boolean]]] =
		e match {
			case or :XorSQL[F, S] => Got(or.conditions)
			case _ => Lack
		}

	trait SpecificXorVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def xor(e :XorSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificXor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificXorVisitor[F, S, X, Y]
	type CaseSpecificXor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificXorVisitor[F, S, X, Y]
//
//	trait XorVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def xor[S >: Grouped <: Single](e :XorSQL[F, S]) :R[S, Boolean, XorSQL[F, S]]
//	}
//	type MatchXor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		XorVisitor[F, R]
//	type CaseXor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		XorVisitor[F, R]

	trait AnyXorVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def xor[S >: Grouped <: Single](e :XorSQL[F, S]) :Y[S, Boolean]
	}
	type MatchAnyXor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyXorVisitor[F, Y]
	type CaseAnyXor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyXorVisitor[F, Y]
}




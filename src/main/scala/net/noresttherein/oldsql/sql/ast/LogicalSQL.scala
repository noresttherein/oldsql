package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLBoolean, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.AndSQL.{AndVisitor, CaseAnd}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL.UnaryColumnOperator
import net.noresttherein.oldsql.sql.ast.NotSQL.{CaseNot, NotVisitor}
import net.noresttherein.oldsql.sql.ast.OrSQL.{CaseOr, OrVisitor}
import net.noresttherein.oldsql.sql.ast.SQLLiteral.{False, True}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** Base trait for SQL formulas implementing Boolean algebra. */
trait LogicalSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope] extends CompositeColumnSQL[F, S, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnVisitor[F, Y]) :Y[S, Boolean] =
//		matcher.logical(this)
}






object LogicalSQL {

	trait LogicalVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends NotVisitor[F, Y] with AndVisitor[F, Y] with OrVisitor[F, Y]
	{
		def logical[S >: LocalScope <: GlobalScope](e :LogicalSQL[F, S]) :Y[S, Boolean]
	}

	trait MatchLogical[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends LogicalVisitor[F, Y] with CaseNot[F, Y] with CaseAnd[F, Y] with CaseOr[F, Y]

	trait CaseLogical[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchLogical[F, Y] {
		override def not[S >: LocalScope <: GlobalScope](e :NotSQL[F, S]) :Y[S, Boolean] = logical(e)

		override def and[S >: LocalScope <: GlobalScope](e :AndSQL[F, S]) :Y[S, Boolean] = logical(e)

		override def or[S >: LocalScope <: GlobalScope](e :OrSQL[F, S]) :Y[S, Boolean] = logical(e)

	}

}






final case class NotSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope](value :ColumnSQL[F, S, Boolean])
	extends UnaryColumnOperator[F, S, Boolean, Boolean] with LogicalSQL[F, S]
{
	override def unary_![E <: F, O >: LocalScope <: S](implicit ev :Boolean =:= Boolean) :ColumnSQL[E, O, Boolean] =
		value

	override def groundValue :Opt[Boolean] = value.groundValue.map(!_)

	protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope](e :SQLBoolean[E, C]) :SQLBoolean[E, C] =
		NotSQL(e)

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
		visitor.not(this)

	protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
				(spelling.NOT + " ") +: value.inParens(context, params)

	override def toString :String = "NOT " + value
}



object NotSQL {
	trait NotVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def not[S >: LocalScope <: GlobalScope](e :NotSQL[F, S]) :Y[S, Boolean]
	}

	type MatchNot[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NotVisitor[F, Y]

	type CaseNot[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NotVisitor[F, Y]
}






sealed class AndSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope] private
                   (protected override val parts :List[ColumnSQL[F, S, Boolean]])
	extends LogicalSQL[F, S]
{
	@inline final def conditions :Seq[ColumnSQL[F, S, Boolean]] = inOrder

	override def inOrder :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse

	override def groundValue :Opt[Boolean] = (Got(false) /: parts.view.map(_.groundValue)) {
		case (Got(l), Got(r)) => Got(l || r)
		case _ => Lack
	}

	override def and[E <: F, O >: LocalScope <: S]
	             (other: ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean): AndSQL[E, O] =
		other match {
			case and :AndSQL[E, O] => new AndSQL(and.parts ::: parts)
			case _ => new AndSQL(other :: parts)
		}

	override def &&[E <: F, O >: LocalScope <: S]
	               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean)
			:ColumnSQL[E, O, Boolean] =
		other match {
			case True => this
			case False => other
			case and :AndSQL[E, O] => ((this :ColumnSQL[E, O, Boolean]) /: and.inOrder)(_ && _)
			case _ if parts.contains(other) => this
			case _ => new AndSQL(other :: parts)
		}


	override def anchor(from :F) :SQLBoolean[F, S] = new AndSQL(parts.map(_.anchor(from)))

	override def rephrase[E <: RowProduct](mapper: SQLScribe[F, E]) = new AndSQL(parts.map(mapper(_)))

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
		visitor.and(this)


	protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		parts.scanLeft(SpelledSQL(spelling.TRUE, context, params)) {
			(sql, bool) => bool.inParens(sql.context, sql.params)
		} match {
			case Seq(empty) => empty
			case Seq(_, t @ _*) => t.reduce { (_2, _1) => _1.sql +: (" " + spelling.AND + " ") +: _2 }
			case _ => throw new IllegalStateException("empty scanLeft result")
		}


	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case and :AndSQL[_, _] if and canEqual this => and.parts == parts
		case _ => false
	}

	override def hashCode :Int = parts.hashCode

	override def toString :String = parts.reverse.mkString("(", " AND ", ")")
}



object AndSQL {
	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](conditions :ColumnSQL[F, S, Boolean]*) :AndSQL[F, S] =
		new AndSQL(conditions.toList.reverse) {
			override val inOrder = conditions
		}

	def unapplySeq[F <: RowProduct, S >: LocalScope <: GlobalScope](e :AndSQL[F, S]) :Seq[ColumnSQL[F, S, Boolean]] =
		e.conditions

	def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
			:Opt[Seq[ColumnSQL[F, S, Boolean]]] =
		e match {
			case and :AndSQL[F, S] => Got(and.conditions)
			case _ => Lack
		}



	trait AndVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def and[S >: LocalScope <: GlobalScope](e :AndSQL[F, S]) :Y[S, Boolean]
	}

	type MatchAnd[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AndVisitor[F, Y]

	type CaseAnd[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AndVisitor[F, Y]
}






sealed class OrSQL[-F <: RowProduct, S >: LocalScope <: GlobalScope] private
                  (protected override val parts :List[ColumnSQL[F, S, Boolean]])
	extends LogicalSQL[F, S]
{
	@inline final def conditions :Seq[ColumnSQL[F, S, Boolean]] = inOrder

	override def inOrder :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse

	override def groundValue :Opt[Boolean] = (Got(false) /: parts.view.map(_.groundValue)) {
		case (Got(l), Got(r)) => Got(l || r)
		case _ => Lack
	}

	override def or[E <: F, O >: LocalScope <: S]
	               (other: ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean): OrSQL[E, O] =
		other match {
			case or :OrSQL[E, O] => new OrSQL(or.parts ::: parts)
			case _ => new OrSQL(other :: parts)
		}

	override def ||[E <: F, O >: LocalScope <: S]
	               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :ColumnSQL[E, O, Boolean] =
		other match {
			case True => other
			case False => this
			case or :OrSQL[E, O] => ((this :ColumnSQL[E, O, Boolean]) /: or.inOrder)(_ || _)
			case _ if parts contains other => this
			case _ => new OrSQL(other :: parts)
		}

	override def anchor(from :F) :SQLBoolean[F, S] = new OrSQL(parts.map(_.anchor(from)))

	override def rephrase[E <: RowProduct](mapper: SQLScribe[F, E]) = new OrSQL(parts.map(mapper(_)))

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, Boolean] =
		visitor.or(this)


	protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		parts.scanLeft(SpelledSQL(spelling.TRUE, context, params)) {
			(sql, bool) => bool.inParens(sql.context, sql.params)
		} match {
			case Seq(empty) => empty
			case Seq(_, t @ _*) => t.reduce { (_2, _1) => _1.sql +: (" " + spelling.OR + " ") +: _2 }
			case _ => throw new IllegalStateException("empty scanLeft result")
		}


	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case or :OrSQL[_, _] if or canEqual this => or.parts == parts
		case _ => false
	}

	override def hashCode :Int = parts.hashCode

	override def toString :String = parts.reverse.mkString("(", ") OR (", ")")
}



object OrSQL {
	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](conditions :ColumnSQL[F, S, Boolean]*) :OrSQL[F, S] =
		new OrSQL(conditions.view.reverse.toList) {
			override val inOrder = conditions
		}

	def unapplySeq[F <: RowProduct, S >: LocalScope <: GlobalScope](or :OrSQL[F, S]) :Seq[ColumnSQL[F, S, Boolean]] =
		or.conditions

	def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
			:Opt[Seq[ColumnSQL[F, S, Boolean]]] =
		e match {
			case or :OrSQL[F, S] => Got(or.conditions)
			case _ => Lack
		}

	trait OrVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def or[S >: LocalScope <: GlobalScope](e :OrSQL[F, S]) :Y[S, Boolean]
	}

	type MatchOr[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = OrVisitor[F, Y]

	type CaseOr[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = OrVisitor[F, Y]
}




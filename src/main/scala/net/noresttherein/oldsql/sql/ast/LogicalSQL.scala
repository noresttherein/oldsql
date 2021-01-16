package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLBoolean, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.UnaryColumnOperator
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.LogicalSQL.AndSQL.{AndMatcher, CaseAnd}
import net.noresttherein.oldsql.sql.ast.LogicalSQL.NotSQL.{CaseNot, NotMatcher}
import net.noresttherein.oldsql.sql.ast.LogicalSQL.OrSQL.{CaseOr, OrMatcher}
import net.noresttherein.oldsql.sql.ast.SQLTerm.{False, True}
import net.noresttherein.oldsql.sql.mechanics.SQLScribe



/** Base trait for SQL formulas implementing Boolean algebra. */
trait LogicalSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope] extends CompositeColumnSQL[F, S, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
//		matcher.logical(this)
}






object LogicalSQL {

	case class NotSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope](value :ColumnSQL[F, S, Boolean])
		extends UnaryColumnOperator[F, S, Boolean, Boolean] with LogicalSQL[F, S]
	{
		override def unary_![E <: F, O >: LocalScope <: S](implicit ev :Boolean =:= Boolean) :ColumnSQL[E, O, Boolean] =
			value

		protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope](e :SQLBoolean[E, C]) :SQLBoolean[E, C] =
			NotSQL(e)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.not(this)

		override def toString :String = "NOT " + value
	}



	object NotSQL {
		trait NotMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def not[S >: LocalScope <: GlobalScope](e :NotSQL[F, S]) :Y[S, Boolean]
		}

		type MatchNot[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NotMatcher[F, Y]

		type CaseNot[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = NotMatcher[F, Y]
	}






	class AndSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope] private
	            (protected override val parts :List[ColumnSQL[F, S, Boolean]])
		extends LogicalSQL[F, S]
	{
		def conditions :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse

		override def inOrder :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse

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
				case True() => this
				case False() => other
				case and :AndSQL[E, O] => ((this :ColumnSQL[E, O, Boolean]) /: and.inOrder)((acc, cond) => acc && cond)
				case _ if parts.contains(other) => this
				case _ => new AndSQL(other :: parts)
			}


		override def anchor(from :F) :SQLBoolean[F, S] = new AndSQL(parts.map(_.anchor(from)))

		override def rephrase[E <: RowProduct](mapper: SQLScribe[F, E]) = new AndSQL(parts.map(mapper(_)))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.and(this)


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case and :AndSQL[_, _] if and canEqual this => and.parts == parts
			case _ => false
		}

		override def hashCode :Int = parts.hashCode

		override def toString :String = parts.reverse.mkString("(", " AND ", ")")
	}



	object AndSQL {
		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](parts :ColumnSQL[F, S, Boolean]*) :AndSQL[F, S] =
			new AndSQL(parts.toList.reverse)

		def unapplySeq[F <: RowProduct, S >: LocalScope <: GlobalScope](e :AndSQL[F, S]) :Seq[ColumnSQL[F, S, Boolean]] =
			e.conditions

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
				:Opt[Seq[ColumnSQL[F, S, Boolean]]] =
			e match {
				case and :AndSQL[F, S] => Got(and.conditions)
				case _ => Lack
			}



		trait AndMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def and[S >: LocalScope <: GlobalScope](e :AndSQL[F, S]) :Y[S, Boolean]
		}

		type MatchAnd[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AndMatcher[F, Y]

		type CaseAnd[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AndMatcher[F, Y]
	}






	class OrSQL[-F <: RowProduct, S >: LocalScope <: GlobalScope] private
	           (protected override val parts :List[ColumnSQL[F, S, Boolean]])
		extends LogicalSQL[F, S]
	{
		def conditions :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse

		override def inOrder :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse


		override def or[E <: F, O >: LocalScope <: S]
		               (other: ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean): OrSQL[E, O] =
			other match {
				case or :OrSQL[E, O] => new OrSQL(or.parts ::: parts)
				case _ => new OrSQL(other :: parts)
			}

		override def ||[E <: F, O >: LocalScope <: S]
		               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :ColumnSQL[E, O, Boolean] =
			other match {
				case True() => other
				case False() => this
				case or :OrSQL[E, O] => ((this :ColumnSQL[E, O, Boolean]) /: or.inOrder)((acc, cond) => acc || cond)
				case _ if parts contains other => this
				case _ => new OrSQL(other :: parts)
			}

		override def anchor(from :F) :SQLBoolean[F, S] = new OrSQL(parts.map(_.anchor(from)))

		override def rephrase[E <: RowProduct](mapper: SQLScribe[F, E]) = new OrSQL(parts.map(mapper(_)))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.or(this)


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
			new OrSQL(conditions.toList.reverse)

		def unapplySeq[F <: RowProduct, S >: LocalScope <: GlobalScope](or :OrSQL[F, S]) :Seq[ColumnSQL[F, S, Boolean]] =
			or.conditions

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
				:Opt[Seq[ColumnSQL[F, S, Boolean]]] =
			e match {
				case or :OrSQL[F, S] => Got(or.conditions)
				case _ => Lack
			}

		trait OrMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def or[S >: LocalScope <: GlobalScope](e :OrSQL[F, S]) :Y[S, Boolean]
		}

		type MatchOr[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = OrMatcher[F, Y]

		type CaseOr[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = OrMatcher[F, Y]
	}







	trait LogicalMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends NotMatcher[F, Y] with AndMatcher[F, Y] with OrMatcher[F, Y]
	{
		def logical[S >: LocalScope <: GlobalScope](e :LogicalSQL[F, S]) :Y[S, Boolean]
	}

	trait MatchLogical[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends LogicalMatcher[F, Y] with CaseNot[F, Y] with CaseAnd[F, Y] with CaseOr[F, Y]

	trait CaseLogical[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchLogical[F, Y] {
		override def not[S >: LocalScope <: GlobalScope](e :NotSQL[F, S]) :Y[S, Boolean] = logical(e)

		override def and[S >: LocalScope <: GlobalScope](e :AndSQL[F, S]) :Y[S, Boolean] = logical(e)

		override def or[S >: LocalScope <: GlobalScope](e :OrSQL[F, S]) :Y[S, Boolean] = logical(e)

	}



}




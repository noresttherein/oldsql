package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.LogicalSQL.AND.{ANDMatcher, CaseAND}
import net.noresttherein.oldsql.sql.LogicalSQL.NOT.{CaseNOT, NOTMatcher}
import net.noresttherein.oldsql.sql.LogicalSQL.OR.{CaseOR, ORMatcher}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLTerm.{False, True}



/** Base trait for SQL formulas implementing Boolean algebra. */
trait LogicalSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope] extends CompositeColumnSQL[F, S, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]

}






object LogicalSQL {

	case class NOT[-F <: FromClause, -S >: LocalScope <: GlobalScope](expression :ColumnSQL[F, S, Boolean])
		extends LogicalSQL[F, S]
	{

		override def parts: Seq[SQLExpression[F, S, _]] = expression::Nil

//		override def get(values: RowValues[F]): Option[Boolean] = expression.get(values).map(!_)

		override def freeValue: Option[Boolean] = expression.freeValue.map(!_)


		override def unary_![E <: F, O >: LocalScope <: S]
		                    (implicit ev :this.type <:< ColumnSQL[E, O, Boolean]) :ColumnSQL[E, O, Boolean] =
			expression


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.not(this)

		override def rephrase[E <: FromClause](mapper: SQLScribe[F, E]) :NOT[E, S] = NOT(mapper(expression))

		override def toString :String = "NOT " + expression
	}



	object NOT {
		trait NOTMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def not[S >: LocalScope <: GlobalScope](e :NOT[F, S]) :Y[S, Boolean]
		}

		type MatchNOT[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = NOTMatcher[F, Y]

		type CaseNOT[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = NOTMatcher[F, Y]

	}






	class AND[-F <: FromClause, -S >: LocalScope <: GlobalScope] private
	         (protected override val parts :List[ColumnSQL[F, S, Boolean]])
		extends LogicalSQL[F, S]
	{
		def conditions :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse

		override def inOrder :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse

//		override def get(values: RowValues[F]): Option[Boolean] = (Option(true) /: parts) {
//			case (acc, e) => for (v1 <- acc; v2 <- e.get(values)) yield v1 && v2
//		}

		override def freeValue :Option[Boolean] = (Option(true) /: parts) {
			case (acc, e) => for (v1 <- acc; v2 <- e.freeValue) yield v1 && v2
		}

		override def and[E <: F, O >: LocalScope <: S]
		             (other: ColumnSQL[E, O, Boolean])(implicit ev: this.type <:< ColumnSQL[E, O, Boolean]): AND[E, O] =
			other match {
				case and :AND[E, O] => new AND(and.parts ::: parts)
				case _ => new AND(other :: parts)
			}

		override def &&[E <: F, O >: LocalScope <: S]
		               (other :ColumnSQL[E, O, Boolean])(implicit ev: this.type <:< ColumnSQL[E, O, Boolean])
				:ColumnSQL[E, O, Boolean] =
			other match {
				case True() => this
				case False() => other
				case and :AND[E, O] => ((this :ColumnSQL[E, O, Boolean]) /: and.inOrder)((acc, cond) => acc && cond)
				case _ if parts.contains(other) => this
				case _ => new AND(other :: parts)
			}



		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] =
			matcher.and(this)

		override def rephrase[E <: FromClause](mapper: SQLScribe[F, E]) = new AND(parts.map(mapper(_)))


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case and :AND[_, _] if and canEqual this => and.parts == parts
			case _ => false
		}

		override def hashCode :Int = parts.hashCode

		override def toString :String = parts.reverse.mkString("(", " AND ", ")")
	}



	object AND {
		def apply[F <: FromClause, S >: LocalScope <: GlobalScope](parts :ColumnSQL[F, S, Boolean]*) :AND[F, S] =
			new AND(parts.toList.reverse)

		def unapplySeq[F <: FromClause, S >: LocalScope <: GlobalScope](e :AND[F, S]) :Seq[ColumnSQL[F, S, Boolean]] = e.conditions

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
				:Option[Seq[ColumnSQL[F, S, Boolean]]] =
			e match {
				case and :AND[F, S] => Some(and.conditions)
				case _ => None
			}



		trait ANDMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def and[S >: LocalScope <: GlobalScope](e :AND[F, S]) :Y[S, Boolean]
		}

		type MatchAND[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ANDMatcher[F, Y]

		type CaseAND[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ANDMatcher[F, Y]
	}






	class OR[-F <: FromClause, S >: LocalScope <: GlobalScope] private
	        (protected override val parts :List[ColumnSQL[F, S, Boolean]])
		extends LogicalSQL[F, S]
	{
		def conditions :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse

		override def inOrder :Seq[ColumnSQL[F, S, Boolean]] = parts.reverse


//		override def get(values: RowValues[F]): Option[Boolean] = (Option(false) /: parts) {
//			case (acc, e) => for (v1 <- acc; v2 <- e.get(values)) yield v1 || v2
//		}

		override def freeValue :Option[Boolean] = (Option(false) /: parts) {
			case (acc, e) => for (v1 <- acc; v2 <- e.freeValue) yield v1 || v2
		}


		override def or[E <: F, O >: LocalScope <: S]
		               (other: ColumnSQL[E, O, Boolean])(implicit ev: this.type <:< ColumnSQL[E, O, Boolean]): OR[E, O] =
			other match {
				case or :OR[E, O] => new OR(or.parts ::: parts)
				case _ => new OR(other :: parts)
			}

		override def ||[E <: F, O >: LocalScope <: S]
		               (other :ColumnSQL[E, O, Boolean])(implicit ev :this.type <:< ColumnSQL[E, O, Boolean])
				:ColumnSQL[E, O, Boolean] =
			other match {
				case True() => other
				case False() => this
				case or :OR[E, O] => ((this :ColumnSQL[E, O, Boolean]) /: or.inOrder)((acc, cond) => acc || cond)
				case _ if parts contains other => this
				case _ => new OR(other :: parts)
			}


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, Boolean] = matcher.or(this)

		override def rephrase[E <: FromClause](mapper: SQLScribe[F, E]) = new OR(parts.map(mapper(_)))


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case or :OR[_, _] if or canEqual this => or.parts == parts
			case _ => false
		}

		override def hashCode :Int = parts.hashCode

		override def toString :String = parts.reverse.mkString("(", ") OR (", ")")
	}



	object OR {

		def apply[F <: FromClause, S >: LocalScope <: GlobalScope](conditions :ColumnSQL[F, S, Boolean]*) :OR[F, S] =
			new OR(conditions.toList.reverse)

		def unapplySeq[F <: FromClause, S >: LocalScope <: GlobalScope](or :OR[F, S]) :Seq[ColumnSQL[F, S, Boolean]] =
			or.conditions

		def unapply[F <: FromClause, S >: LocalScope <: GlobalScope](e :SQLExpression[F, S, _])
				:Option[Seq[ColumnSQL[F, S, Boolean]]] =
			e match {
				case or :OR[F, S] => Some(or.conditions)
				case _ => None
			}

		trait ORMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def or[S >: LocalScope <: GlobalScope](e :OR[F, S]) :Y[S, Boolean]
		}

		type MatchOR[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ORMatcher[F, Y]

		type CaseOR[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ORMatcher[F, Y]
	}







	trait LogicalMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends NOTMatcher[F, Y] with ANDMatcher[F, Y] with ORMatcher[F, Y]

	trait MatchLogical[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends LogicalMatcher[F, Y] with CaseNOT[F, Y] with CaseAND[F, Y] with CaseOR[F, Y]

	trait CaseLogical[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchLogical[F, Y] {
		def logical[S >: LocalScope <: GlobalScope](e :LogicalSQL[F, S]) :Y[S, Boolean]

		override def not[S >: LocalScope <: GlobalScope](e :NOT[F, S]) :Y[S, Boolean] = logical(e)

		override def and[S >: LocalScope <: GlobalScope](e :AND[F, S]) :Y[S, Boolean] = logical(e)

		override def or[S >: LocalScope <: GlobalScope](e :OR[F, S]) :Y[S, Boolean] = logical(e)

	}



}




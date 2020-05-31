package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnExpressionMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.LogicalSQL.AND.{ANDMatcher, CaseAND}
import net.noresttherein.oldsql.sql.LogicalSQL.NOT.{CaseNot, NotMatcher}
import net.noresttherein.oldsql.sql.LogicalSQL.OR.{CaseOR, ORMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.{False, True}



/** Base trait for SQL formulas implementing Boolean algebra. */
trait LogicalSQL[-F <: FromClause] extends CompositeColumnSQL[F, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]

}






object LogicalSQL {

	case class NOT[-F <: FromClause](formula :SQLBoolean[F]) extends LogicalSQL[F] {

		override def parts: Seq[SQLExpression[F, _]] = formula::Nil

//		override def get(values: RowValues[F]): Option[Boolean] = expression.get(values).map(!_)

		override def freeValue: Option[Boolean] = formula.freeValue.map(!_)


		override def unary_![S <: F](implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
			formula


		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[Boolean] = matcher.not(this)

		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = NOT(mapper(formula))

		override def toString = "NOT " + formula
	}



	object NOT {
		trait NotMatcher[+F <: FromClause, +Y[X]] {
			def not(e :NOT[F]) :Y[Boolean]
		}

		type MatchNot[+F <: FromClause, +Y[X]] = NotMatcher[F, Y]

		type CaseNot[+F <: FromClause, +Y[X]] = NotMatcher[F, Y]

	}






	case class AND[-F <: FromClause] private(protected val parts :List[SQLBoolean[F]]) extends LogicalSQL[F] {
		def conditions :Seq[SQLBoolean[F]] = parts.reverse

		override def inOrder :Seq[SQLBoolean[F]] = parts.reverse

//		override def get(values: RowValues[F]): Option[Boolean] = (Option(true) /: parts) {
//			case (acc, e) => for (v1 <- acc; v2 <- e.get(values)) yield v1 && v2
//		}

		override def freeValue :Option[Boolean] = (Option(true) /: parts) {
			case (acc, e) => for (v1 <- acc; v2 <- e.freeValue) yield v1 && v2
		}

		override def and[S <: F](other: SQLBoolean[S])
		                        (implicit ev: this.type <:< SQLBoolean[S]): AND[S] =
			other match {
				case and :AND[S] => new AND(and.parts ::: parts)
				case _ => new AND(other :: parts)
			}

		override def &&[S <: F](other :SQLBoolean[S])
		                       (implicit ev: this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
			other match {
				case True() => this
				case False() => other
				case and :AND[S] => ((this :SQLBoolean[S]) /: and.inOrder)((acc, cond) => acc && cond)
				case _ if parts.contains(other) => this
				case _ => new AND(other :: parts)
			}



		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[Boolean] = matcher.and(this)

		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = new AND(parts.map(mapper(_)))



		override def toString :String = parts.reverse.mkString("(", " AND ", ")")
	}



	object AND {
		def apply[F <: FromClause](parts :SQLBoolean[F]*) :AND[F] =
			new AND(parts.toList.reverse)

		def unapply[F <: FromClause](sql :SQLBoolean[F]) :Option[Seq[SQLBoolean[F]]] = sql match {
			case and :AND[F] => Some(and.conditions)
			case _ => None
		}



		trait ANDMatcher[+F <: FromClause, +Y[X]] {
			def and(e :AND[F]) :Y[Boolean]
		}

		type MatchAND[+F <: FromClause, +Y[X]] = ANDMatcher[F, Y]

		type CaseAND[+F <: FromClause, +Y[X]] = ANDMatcher[F, Y]
	}






	case class OR[-F <: FromClause] private(protected val parts :List[SQLBoolean[F]]) extends LogicalSQL[F] {
		def conditions :Seq[SQLBoolean[F]] = parts.reverse

		override def inOrder :Seq[SQLBoolean[F]] = parts.reverse


//		override def get(values: RowValues[F]): Option[Boolean] = (Option(false) /: parts) {
//			case (acc, e) => for (v1 <- acc; v2 <- e.get(values)) yield v1 || v2
//		}

		override def freeValue :Option[Boolean] = (Option(false) /: parts) {
			case (acc, e) => for (v1 <- acc; v2 <- e.freeValue) yield v1 || v2
		}


		override def or[S <: F](other: SQLBoolean[S])
		                       (implicit ev: this.type <:< SQLBoolean[S]): OR[S] =
			other match {
				case or :OR[S] => new OR(or.parts ::: parts)
				case _ => new OR(other :: parts)
			}

		override def ||[S <: F](other :SQLBoolean[S])
		                       (implicit ev :this.type <:< SQLBoolean[S]) :SQLBoolean[S] =
			other match {
				case True() => other
				case False() => this
				case or :OR[S] => ((this :SQLBoolean[S]) /: or.inOrder)((acc, cond) => acc || cond)
				case _ if parts contains other => this
				case _ => new OR(other :: parts)
			}



		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[Boolean] = matcher.or(this)

		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = new OR(parts.map(mapper(_)))



		override def toString :String = parts.reverse.mkString("(", ") OR (", ")")
	}



	object OR {

		def apply[F <: FromClause](conditions :SQLBoolean[F]*) :OR[F] = new OR(conditions.toList.reverse)

		trait ORMatcher[+F <: FromClause, +Y[X]] {
			def or(e :OR[F]) :Y[Boolean]
		}

		type MatchOR[+F <: FromClause, +Y[X]] = ORMatcher[F, Y]

		type CaseOR[+F <: FromClause, +Y[X]] = ORMatcher[F, Y]
	}







	trait LogicalMatcher[+F <: FromClause, +Y[X]] extends NotMatcher[F, Y] with ANDMatcher[F, Y] with ORMatcher[F, Y]

	trait MatchLogical[+F <: FromClause, +Y[X]] extends CaseNot[F, Y] with CaseAND[F, Y] with CaseOR[F, Y]

	trait CaseLogical[+F <: FromClause, +Y[X]] extends LogicalMatcher[F, Y] with MatchLogical[F, Y] {
		def logical(e :LogicalSQL[F]) :Y[Boolean]

		override def not(e :NOT[F]) :Y[Boolean] = logical(e)

		override def and(e :AND[F]) :Y[Boolean] = logical(e)

		override def or(e :OR[F]) :Y[Boolean] = logical(e)

	}



}




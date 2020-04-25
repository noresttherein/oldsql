package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.LogicalFormula.AND.{ANDMatcher, CaseAND}
import net.noresttherein.oldsql.sql.LogicalFormula.NOT.{CaseNOT, NOTMatcher}
import net.noresttherein.oldsql.sql.LogicalFormula.OR.{CaseOR, ORMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula, CompositeColumnFormula, CompositeFormula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.ColumnFormulaMatcher
import net.noresttherein.oldsql.sql.SQLMapper.SQLRewriter
import net.noresttherein.oldsql.sql.SQLTerm.{False, True}



/** Base trait for SQL formulas implementing Boolean algebra. */
trait LogicalFormula[-F <: FromClause] extends CompositeColumnFormula[F, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]


	override def applyTo[Y[X]](matcher :FormulaMatcher[F, Y]) :Y[Boolean] =
		applyTo(matcher :ColumnFormulaMatcher[F, Y])

}






object LogicalFormula {

	case class NOT[-F <: FromClause](formula :BooleanFormula[F]) extends LogicalFormula[F] {

		override def parts: Seq[SQLFormula[F, _]] = formula::Nil

//		override def get(values: RowValues[F]): Option[Boolean] = formula.get(values).map(!_)

		override def freeValue: Option[Boolean] = formula.freeValue.map(!_)


		override def unary_![S <: F](implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			formula


		override def applyTo[Y[X]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Boolean] = matcher.not(this)

		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = NOT(mapper(formula))

		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :BooleanFormula[S] =
			NOT(formula.stretch[U, S])
	}



	object NOT {
		trait NOTMatcher[+F <: FromClause, +Y[X]] {
			def not(e :NOT[F]) :Y[Boolean]
		}

		type MatchNOT[+F <: FromClause, +Y[X]] = NOTMatcher[F, Y]

		type CaseNOT[+F <: FromClause, +Y[X]] = NOTMatcher[F, Y]

	}






	case class AND[-F <: FromClause] private(protected val parts :List[BooleanFormula[F]]) extends LogicalFormula[F] {
		def conditions :Seq[BooleanFormula[F]] = parts.reverse

		override def inOrder :Seq[BooleanFormula[F]] = parts.reverse

//		override def get(values: RowValues[F]): Option[Boolean] = (Option(true) /: parts) {
//			case (acc, e) => for (v1 <- acc; v2 <- e.get(values)) yield v1 && v2
//		}

		override def freeValue :Option[Boolean] = (Option(true) /: parts) {
			case (acc, e) => for (v1 <- acc; v2 <- e.freeValue) yield v1 && v2
		}

		override def and[S <: F](other: BooleanFormula[S])
		                        (implicit ev: this.type <:< BooleanFormula[S]): AND[S] =
			other match {
				case and :AND[S] => new AND(and.parts ::: parts)
				case _ => new AND(other :: parts)
			}

		override def &&[S <: F](other :BooleanFormula[S])
		                       (implicit ev: this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			other match {
				case True() => this
				case False() => other
				case and :AND[S] => ((this :BooleanFormula[S]) /: and.inOrder)((acc, cond) => acc && cond)
				case _ if parts.contains(other) => this
				case _ => new AND(other :: parts)
			}



		override def applyTo[Y[X]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Boolean] = matcher.and(this)

		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = new AND(parts.map(mapper(_)))


		override def stretch[U <: F, S <: FromClause](implicit ev :ExtendedBy[U, S]) :BooleanFormula[S] =
			AND(parts.map(_.stretch[U, S]))

		override def toString :String = parts.reverse.mkString("(", " and ", ")")
	}



	object AND {
		def apply[F <: FromClause](parts :BooleanFormula[F]*) :AND[F] =
			new AND(parts.toList.reverse)

		def unapply[F <: FromClause](sql :BooleanFormula[F]) :Option[Seq[BooleanFormula[F]]] = sql match {
			case and :AND[F] => Some(and.conditions)
			case _ => None
		}



		trait ANDMatcher[+F <: FromClause, +Y[X]] {
			def and(e :AND[F]) :Y[Boolean]
		}

		type MatchAND[+F <: FromClause, +Y[X]] = ANDMatcher[F, Y]

		type CaseAND[+F <: FromClause, +Y[X]] = ANDMatcher[F, Y]
	}






	case class OR[-F <: FromClause] private(protected val parts :List[BooleanFormula[F]]) extends LogicalFormula[F] {
		def conditions :Seq[BooleanFormula[F]] = parts.reverse

		protected override def inOrder :Seq[BooleanFormula[F]] = parts.reverse


//		override def get(values: RowValues[F]): Option[Boolean] = (Option(false) /: parts) {
//			case (acc, e) => for (v1 <- acc; v2 <- e.get(values)) yield v1 || v2
//		}

		override def freeValue :Option[Boolean] = (Option(false) /: parts) {
			case (acc, e) => for (v1 <- acc; v2 <- e.freeValue) yield v1 || v2
		}


		override def or[S <: F](other: BooleanFormula[S])
		                       (implicit ev: this.type <:< BooleanFormula[S]): OR[S] =
			other match {
				case or :OR[S] => new OR(or.parts ::: parts)
				case _ => new OR(other :: parts)
			}

		override def ||[S <: F](other :BooleanFormula[S])
		                       (implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			other match {
				case True() => other
				case False() => this
				case or :OR[S] => ((this :BooleanFormula[S]) /: or.inOrder)((acc, cond) => acc || cond)
				case _ if parts contains other => this
				case _ => new OR(other :: parts)
			}



		override def applyTo[Y[X]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Boolean] = matcher.or(this)

		override def map[S <: FromClause](mapper: SQLScribe[F, S]) = new OR(parts.map(mapper(_)))


		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :BooleanFormula[S] =
			OR(parts.map(_.stretch[U, S]))

		override def toString :String = parts.reverse.mkString("(", ") or (", ")")
	}



	object OR {

		def apply[F <: FromClause](conditions :BooleanFormula[F]*) :OR[F] = new OR(conditions.toList.reverse)

		trait ORMatcher[+F <: FromClause, +Y[X]] {
			def or(e :OR[F]) :Y[Boolean]
		}

		type MatchOR[+F <: FromClause, +Y[X]] = ORMatcher[F, Y]

		type CaseOR[+F <: FromClause, +Y[X]] = ORMatcher[F, Y]
	}







	trait LogicalMatcher[+F <: FromClause, +Y[X]] extends NOTMatcher[F, Y] with ANDMatcher[F, Y] with ORMatcher[F, Y]

	trait MatchLogical[+F <: FromClause, +Y[X]] extends CaseNOT[F, Y] with CaseAND[F, Y] with CaseOR[F, Y]

	trait CaseLogical[+F <: FromClause, +Y[X]] extends LogicalMatcher[F, Y] with MatchLogical[F, Y] {
		def logical(e :LogicalFormula[F]) :Y[Boolean]

		override def not(e :NOT[F]) :Y[Boolean] = logical(e)

		override def and(e :AND[F]) :Y[Boolean] = logical(e)

		override def or(e :OR[F]) :Y[Boolean] = logical(e)

	}



}




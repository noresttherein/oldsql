package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.sql.LogicalFormula.And.{AndMatcher, CaseAnd}
import net.noresttherein.oldsql.sql.LogicalFormula.NotFormula.{CaseNot, NotMatcher}
import net.noresttherein.oldsql.sql.LogicalFormula.Or.{CaseOr, OrMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula, CompositeFormula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLMapper.SQLRewriter
import net.noresttherein.oldsql.sql.SQLTerm.{False, True}



/** Base trait for SQL formulas implementing Boolean algebra. */
trait LogicalFormula[-F <: FromClause] extends CompositeFormula[F, Boolean] with ColumnFormula[F, Boolean] {
	override def readForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]
}






object LogicalFormula {

	//todo: rename to Not
	case class NotFormula[-F <: FromClause](formula :BooleanFormula[F]) extends LogicalFormula[F] {

		override def parts: Seq[SQLFormula[F, _]] = formula::Nil

//		override def get(values: RowValues[F]): Option[Boolean] = formula.get(values).map(!_)

		override def freeValue: Option[Boolean] = formula.freeValue.map(!_)


		override def unary_![S <: F](implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			formula


		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Boolean] = matcher.not(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]) = //todo: casts are bad 
			NotFormula(mapper(formula).asInstanceOf[BooleanFormula[S]])
	}



	object NotFormula {
		trait NotMatcher[+F <: FromClause, +Y[X]] {
			def not(f :NotFormula[F]) :Y[Boolean]
		}

		type MatchNot[+F <: FromClause, +Y[X]] = NotMatcher[F, Y]

		type CaseNot[+F <: FromClause, +Y[X]] = NotMatcher[F, Y]

	}






	case class And[-F <: FromClause] private(protected val parts :List[BooleanFormula[F]]) extends LogicalFormula[F] {
		def conditions :Seq[BooleanFormula[F]] = parts.reverse

		override def inOrder :Seq[BooleanFormula[F]] = parts.reverse

//		override def get(values: RowValues[F]): Option[Boolean] = (Option(true) /: parts) {
//			case (acc, e) => for (v1 <- acc; v2 <- e.get(values)) yield v1 && v2
//		}

		override def freeValue :Option[Boolean] = (Option(true) /: parts) {
			case (acc, e) => for (v1 <- acc; v2 <- e.freeValue) yield v1 && v2
		}

		override def and[S <: F](other: BooleanFormula[S])
		                        (implicit ev: this.type <:< BooleanFormula[S]): And[S] =
			other match {
				case and :And[S] => new And(and.parts ::: parts)
				case _ => new And(other :: parts)
			}

		override def &&[S <: F](other :BooleanFormula[S])
		                       (implicit ev: this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			other match {
				case True() => this
				case False() => other
				case and :And[S] => ((this :BooleanFormula[S]) /: and.inOrder)((acc, cond) => acc && cond)
				case _ if parts.contains(other) => this
				case _ => new And(other :: parts)
			}



		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Boolean] = matcher.and(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]) = //todo: casting is bad
			new And(parts.map(mapper(_).asInstanceOf[BooleanFormula[S]]))


		override def toString :String = parts.reverse.mkString("(", " and ", ")")
	}



	object And {
		def apply[F <: FromClause](parts :BooleanFormula[F]*) :And[F] =
			new And(parts.toList.reverse)

		def unapply[F <: FromClause](sql :BooleanFormula[F]) :Option[Seq[BooleanFormula[F]]] = sql match {
			case and :And[F] => Some(and.conditions)
			case _ => None
		}



		trait AndMatcher[+F <: FromClause, +Y[X]] {
			def and(f :And[F]) :Y[Boolean]
		}

		type MatchAnd[+F <: FromClause, +Y[X]] = AndMatcher[F, Y]

		type CaseAnd[+F <: FromClause, +Y[X]] = AndMatcher[F, Y]
	}






	case class Or[-F <: FromClause] private(protected val parts :List[BooleanFormula[F]]) extends LogicalFormula[F] {
		def conditions :Seq[BooleanFormula[F]] = parts.reverse

		protected override def inOrder :Seq[BooleanFormula[F]] = parts.reverse


//		override def get(values: RowValues[F]): Option[Boolean] = (Option(false) /: parts) {
//			case (acc, e) => for (v1 <- acc; v2 <- e.get(values)) yield v1 || v2
//		}

		override def freeValue :Option[Boolean] = (Option(false) /: parts) {
			case (acc, e) => for (v1 <- acc; v2 <- e.freeValue) yield v1 || v2
		}


		override def or[S <: F](other: BooleanFormula[S])
		                       (implicit ev: this.type <:< BooleanFormula[S]): Or[S] =
			other match {
				case or :Or[S] => new Or(or.parts ::: parts)
				case _ => new Or(other :: parts)
			}

		override def ||[S <: F](other :BooleanFormula[S])
		                       (implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
			other match {
				case True() => other
				case False() => this
				case or :Or[S] => ((this :BooleanFormula[S]) /: or.inOrder)((acc, cond) => acc || cond)
				case _ if parts contains other => this
				case _ => new Or(other :: parts)
			}



		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Boolean] = matcher.or(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]) = //todo: casts are bad!
			new Or(parts.map(mapper(_).asInstanceOf[BooleanFormula[S]]))

		override def toString :String = parts.reverse.mkString("(", ") or (", ")")
	}



	object Or {

		def apply[F <: FromClause](conditions :BooleanFormula[F]*) :Or[F] = new Or(conditions.toList.reverse)

		trait OrMatcher[+F <: FromClause, +Y[X]] {
			def or(f :Or[F]) :Y[Boolean]
		}

		type MatchOr[+F <: FromClause, +Y[X]] = OrMatcher[F, Y]

		type CaseOr[+F <: FromClause, +Y[X]] = OrMatcher[F, Y]
	}







	trait LogicalMatcher[+F <: FromClause, +Y[X]] extends NotMatcher[F, Y] with AndMatcher[F, Y] with OrMatcher[F, Y]

	trait MatchLogical[+F <: FromClause, +Y[X]] extends CaseNot[F, Y] with CaseAnd[F, Y] with CaseOr[F, Y]

	trait CaseLogical[+F <: FromClause, +Y[X]] extends LogicalMatcher[F, Y] with MatchLogical[F, Y] {
		def logical(f :LogicalFormula[F]) :Y[Boolean]

		override def not(f :NotFormula[F]) :Y[Boolean] = logical(f)

		override def and(f :And[F]) :Y[Boolean] = logical(f)

		override def or(f :Or[F]) :Y[Boolean] = logical(f)

	}



}




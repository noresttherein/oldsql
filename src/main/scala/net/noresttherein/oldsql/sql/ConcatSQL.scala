package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}



/**
  * @author Marcin Mościcki
  */
class ConcatSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope] private (protected override val parts :List[ColumnSQL[F, S, String]])
	extends CompositeColumnSQL[F, S, String]
{
	override def readForm :ColumnReadForm[String] = implicitly[ColumnForm[String]]

	override def inOrder :Seq[ColumnSQL[F, S, String]] = parts.reverse


	override def +[E <: F, O >: LocalScope <: S]
	              (other :ColumnSQL[E, O, String])(implicit ev :String =:= String) :ColumnSQL[E, O, String] =
		other match {
			case concat :ConcatSQL[E @unchecked, O @unchecked] => new ConcatSQL(concat.parts ::: parts)
			case _ => new ConcatSQL(other :: parts)
		}

	override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, String] =
		new ConcatSQL(parts.map(mapper.apply))

	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[S, String] =
		matcher.concat(this)


	override def toString :String = parts.view.reverse.map("'" + _ + "'").mkString(" + ")
}






object ConcatSQL {

	def apply[F <: FromClause, S >: LocalScope <: GlobalScope](parts :ColumnSQL[F, S, String]*) :ConcatSQL[F, S] =
		new ConcatSQL(parts.toList.reverse)

	def unapply[F <: FromClause, S >: LocalScope <: GlobalScope]
	           (e :SQLExpression[F, S, _]) :Option[Seq[ColumnSQL[F, S, String]]] =
		e match {
			case concat :ConcatSQL[F @unchecked, S @unchecked] => Some(concat.inOrder)
			case _ => None
		}


	trait ConcatMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def concat[S >: LocalScope <: GlobalScope](e :ConcatSQL[F, S]) :Y[S, String]
	}

	type MatchConcat[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ConcatMatcher[F, Y]

	type CaseConcat[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ConcatMatcher[F, Y]

}


package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnExpressionMatcher, CompositeColumnSQL}



/**
  * @author Marcin Mościcki
  */
class ConcatSQL[-F <: FromClause] private (protected override val parts :List[ColumnSQL[F, String]])
	extends CompositeColumnSQL[F, String]
{
	override def inOrder :Seq[ColumnSQL[F, String]] = parts.reverse


	override def +[S <: F](other :ColumnSQL[S, String])(implicit ev :String =:= String) :ColumnSQL[S, String] =
		other match {
			case concat :ConcatSQL[S @unchecked] => new ConcatSQL(concat.parts ::: parts)
			case _ => new ConcatSQL(other :: parts)
		}

	override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnSQL[S, String] =
		new ConcatSQL(parts.map(mapper.apply))

	override def readForm :ColumnReadForm[String] = implicitly[ColumnForm[String]]

	override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[String] = matcher.concat(this)


	override def toString :String = parts.view.reverse.map("'" + _ + "'").mkString(" + ")
}





object ConcatSQL {

	def apply[F <: FromClause](parts :ColumnSQL[F, String]*) :ConcatSQL[F] =
		new ConcatSQL(parts.toList.reverse)

	def unapply[F <: FromClause](e :SQLExpression[F, _]) :Option[Seq[ColumnSQL[F, String]]] = e match {
		case concat :ConcatSQL[F @unchecked] => Some(concat.inOrder)
		case _ => None
	}


	trait ConcatMatcher[+F <: FromClause, +Y[X]] {
		def concat(e :ConcatSQL[F]) :Y[String]
	}

	type MatchConcat[+F <: FromClause, +Y[X]] = ConcatMatcher[F, Y]

	type CaseConcat[+F <: FromClause, +Y[X]] = ConcatMatcher[F, Y]

}

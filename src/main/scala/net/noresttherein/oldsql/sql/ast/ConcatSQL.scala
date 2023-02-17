package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.Bug
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{Single, Grouped}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/**
  * @author Marcin Mościcki
  */ //consider: perhaps it would be better as a pseudo list itself, with only first, second?
class ConcatSQL[-F <: RowProduct, -S >: Grouped <: Single] private
               (protected override val parts :List[ColumnSQL[F, S, String]])
	extends CompositeColumnSQL[F, S, String]
{
	override def selectForm :ColumnReadForm[String] = implicitly[ColumnForm[String]]

	override def inOrder :Seq[ColumnSQL[F, S, String]] = parts.reverse


	override def ++[E <: F, O >: Grouped <: S]
	               (that :ColumnSQL[E, O, String])(implicit ev :String =:= String) :ColumnSQL[E, O, String] =
		that match {
			case concat :ConcatSQL[E @unchecked, O @unchecked] => new ConcatSQL(concat.parts ::: parts)
			case _ => new ConcatSQL(that :: parts)
		}

	override def ++:[E <: F, O >: Grouped <: S]
	                (that :ColumnSQL[E, O, String])(implicit ev :String =:= String) :ColumnSQL[E, O, String] =
		that match {
			case concat :ConcatSQL[E @unchecked, O @unchecked] => new ConcatSQL(parts ::: concat.parts)
			case _ => new ConcatSQL(parts :+ that)
		}

	override def groundValue :Opt[String] =
		(Opt(new StringBuilder) /: parts.reverseIterator.map(_.groundValue)) {
			case (Got(builder), Got(part)) => Got(builder ++= part)
			case _ => Lack
		}.map(_.result())

	override def anchor(from :F) :ColumnSQL[F, S, String] =
		if (isAnchored) this else new ConcatSQL(parts.map(_.anchor(from)))

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, String] =
		new ConcatSQL(parts.map(mapper.apply))


	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val spell = spelling.inOperand
		parts.scanLeft(SpelledSQL("''", context)) { //reverse order!
			(sql, col) => col.atomicSpelling(from, sql.context, params)(spell)
		} match {
			case Seq(empty) => empty  //reverse the order while reducing
			case Seq(_, tail @ _*) => tail.reduce {
				(_2, _1) => SpelledSQL(_1.sql +: spelling._CONCAT_ +: _2.sql, _1.setter + _2.setter, _1.context)
			}
			case _ => throw Bug("empty scanLeft result for " + parts + ".")
		}
	}


	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, Y]) :Y[S, String] =
		visitor.concat(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, String, Y]) :Y = visitor.concat(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, String] <: SQLExpression[F_, S_, String],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, String, E] =
//		visitor.concat(this)


	override def toString :String = parts.view.reverse.map("'" + _ + "'").mkString(" + ")
}






object ConcatSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single](parts :ColumnSQL[F, S, String]*) :ConcatSQL[F, S] =
		new ConcatSQL(parts.toList.reverse)

	def unapply[F <: RowProduct, S >: Grouped <: Single]
	           (e :SQLExpression[F, S, _]) :Opt[Seq[ColumnSQL[F, S, String]]] =
		e match {
			case concat :ConcatSQL[F @unchecked, S @unchecked] => Got(concat.inOrder)
			case _ => Lack
		}


	trait SpecificConcatVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def concat(e :ConcatSQL[F, S])(implicit isString :X =:= String) :Y
	}
	type MatchSpecificConcat[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificConcatVisitor[F, S, X, Y]
	type CaseSpecificConcat[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificConcatVisitor[F, S, X, Y]
//
//	trait ConcatVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def concat[S >: Grouped <: Single](e :ConcatSQL[F, S]) :R[S, String, ConcatSQL[F, S]]
//	}
//	type MatchConcat[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		ConcatVisitor[F, R]
//	type CaseConcat[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		ConcatVisitor[F, R]

	trait AnyConcatVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def concat[S >: Grouped <: Single](e :ConcatSQL[F, S]) :Y[S, String]
	}
	type MatchAnyConcat[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyConcatVisitor[F, Y]
	type CaseAnyConcat[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyConcatVisitor[F, Y]

}


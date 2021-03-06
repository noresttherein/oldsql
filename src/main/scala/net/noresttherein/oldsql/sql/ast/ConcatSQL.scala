package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/**
  * @author Marcin Mościcki
  */
class ConcatSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope] private
               (protected override val parts :List[ColumnSQL[F, S, String]])
	extends CompositeColumnSQL[F, S, String]
{
	override def readForm :ColumnReadForm[String] = implicitly[ColumnForm[String]]

	override def groundValue :Opt[String] =
		(Opt(new StringBuilder) /: parts.reverseIterator.map(_.groundValue)) {
			case (Got(builder), Got(part)) => Got(builder ++= part)
			case _ => Lack
		}.map(_.result())


	override def inOrder :Seq[ColumnSQL[F, S, String]] = parts.reverse


	override def ++[E <: F, O >: LocalScope <: S]
	               (that :ColumnSQL[E, O, String])(implicit ev :String =:= String) :ColumnSQL[E, O, String] =
		that match {
			case concat :ConcatSQL[E @unchecked, O @unchecked] => new ConcatSQL(concat.parts ::: parts)
			case _ => new ConcatSQL(that :: parts)
		}

	override def ++:[E <: F, O >: LocalScope <: S]
	                (that :ColumnSQL[E, O, String])(implicit ev :String =:= String) :ColumnSQL[E, O, String] =
		that match {
			case concat :ConcatSQL[E @unchecked, O @unchecked] => new ConcatSQL(parts ::: concat.parts)
			case _ => new ConcatSQL(parts :+ that)
		}

	override def anchor(from :F) :ColumnSQL[F, S, String] = new ConcatSQL(parts.map(_.anchor(from)))

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, String] =
		new ConcatSQL(parts.map(mapper.apply))

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[S, String] =
		visitor.concat(this)


	protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		parts.scanLeft(SpelledSQL("''", context, params)) { //reverse order!
			(sql, col) => col.inParens(sql.context, sql.params)
		} match {
			case Seq(empty) => empty  //reverse the order while reducing
			case Seq(_, tail @ _*) => tail.reduce { (_2, _1) => _2 + (" " + spelling.CONCAT + " ") + _1.sql }
			case _ => throw new IllegalStateException("empty scanLeft result")
		}




	override def toString :String = parts.view.reverse.map("'" + _ + "'").mkString(" + ")
}






object ConcatSQL {

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](parts :ColumnSQL[F, S, String]*) :ConcatSQL[F, S] =
		new ConcatSQL(parts.toList.reverse)

	def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope]
	           (e :SQLExpression[F, S, _]) :Opt[Seq[ColumnSQL[F, S, String]]] =
		e match {
			case concat :ConcatSQL[F @unchecked, S @unchecked] => Got(concat.inOrder)
			case _ => Lack
		}


	trait ConcatVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def concat[S >: LocalScope <: GlobalScope](e :ConcatSQL[F, S]) :Y[S, String]
	}

	type MatchConcat[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ConcatVisitor[F, Y]

	type CaseConcat[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ConcatVisitor[F, Y]

}


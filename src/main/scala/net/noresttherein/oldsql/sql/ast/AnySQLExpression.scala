package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}




/** An implementation artifact used to change expressions of arbitrary types into basic
  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]'s, or, more precisely, into expressions
  * which [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.convert convert]] into basic `SQLExpression`s.
  * It's present as a subexpression in another expression is a proof that the adapted expression can be swapped out
  * with any other `SQLExpression[F, S, V]`. This is important in particular when reforming/aligning multiple
  * expressions at the same time (like ''select'' clauses of all member ''selects'' of a ''compound select'').
  * Any generic transformation working on the expression tree can introduce another decorator in its place
  * (or substitute `value` with the decorator wrapping said `value`).
  * @author Marcin Mościcki
  */
class AnySQLExpression[-F <: RowProduct, -S >: Grouped <: Single, V](override val value :SQLExpression[F, S, V])
	extends DecoratedSQL[F, S, V]
{
	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, V]) :SQLExpression[E, C, V] =
		new AnySQLExpression(e)

	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
		new AnyColumnSQL(e)

	override def toString :String = value.toString
}



class AnyColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, V](override val value :ColumnSQL[F, S, V])
	extends AnySQLExpression[F, S, V](value) with DecoratedColumnSQL[F, S, V]

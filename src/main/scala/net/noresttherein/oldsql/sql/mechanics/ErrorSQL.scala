package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.schema.SQLReadForm
import net.noresttherein.oldsql.sql.ast.AbstractGlobalSQL
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression, RowShape}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, SpecificExpressionVisitor, Single, Grouped}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}




/** A pseudo SQL expression which can take place of any other non-column expression but cannot be translated
  * into SQL and, by default, even used in any normal manner. It implements all abstract methods
  * by throwing an [[UnsupportedOperationException]].
  */ //we already have ErrorTerm
/*
private[sql] trait ErrorSQL[T] extends AbstractGlobalSQL[RowProduct, T] {
	override def selectForm :SQLReadForm[T] =
		throw new UnsupportedOperationException("No definite form for " + this + ".")

	override def groundValue :Opt[T] = Lack

	override def isGround   :Boolean = throw new UnsupportedOperationException(toString + ".isGround")
	override def isAnchored :Boolean= throw new UnsupportedOperationException(toString + ".isAnchored")
	override def isAnchored(from :RowProduct) :Boolean =
		throw new UnsupportedOperationException("Cannot anchor " + this + " with different RowProduct types.")
	override def anchor(from :RowProduct) :SQLExpression[RowProduct, Single, T] =
		throw new UnsupportedOperationException("Cannot anchor " + this + " with different RowProduct types.")

	override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E)
			:SQLExpression[E, Single, T] =
		throw new UnsupportedOperationException(
			"Cannot rebase " + this + " to " + base + " because of different RowProduct types."
		)
	override def expand[U <: RowProduct, E <: RowProduct]
	                   (base :E)(implicit ext :U ExpandedBy E, global :Single <:< Single)
			:SQLExpression[E, Single, T] =
		throw new UnsupportedOperationException(
			"Cannot expand " + this + " to " + base + " because of different RowProduct types."
		)


	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[RowProduct, Single, _]] =
		throw new UnsupportedOperationException(toString + " cannot be split.")

	protected override def shape(implicit spelling :SQLSpelling) :RowShape =
		throw new UnsupportedOperationException(toString + " has an undefined shape.")

	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		throw new UnsupportedOperationException(toString + " has an undefined number of columns.")

	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		throw new UnsupportedOperationException("Number of SQL parameters is undefined for " + this + ".")

	protected override def defaultSpelling[P](from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                                         (implicit spelling :SQLSpelling) :Nothing =
		throw new UnsupportedOperationException(
			toString + " is not a real SQLExpression and cannot be spelled. This is a bug."
		)

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		defaultSpelling(from, context, params)


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[RowProduct, Y]) :Y[Single, T] =
		throw new UnsupportedOperationException("Attempted to visit a pseudo expression " + this + " with " + visitor + ".")

	protected override def visit[Y](visitor :ExpressionVisitor[RowProduct, Single, T, Y]) :Y =
		throw new UnsupportedOperationException("Attempted to visit a pseudo expression " + this + " with " + visitor + ".")


	override def isomorphic(that :SQLExpression.*) = false

}
*/

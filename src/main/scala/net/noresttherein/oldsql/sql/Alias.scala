package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.DecoratedFrom.FromSomeDecorator
import net.noresttherein.oldsql.sql.DecoratedFrom.FromSomeDecorator.FromSomeDecoratorComposition
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.SQLTerm.True






/** Introduces an alias name to the last relation in the wrapped clause, translating to an `as` keyword
  * in the underlying SQL.
  */
class Alias[+F <: FromSome, N <: Label](override val clause :F, val alias :N) extends FromSome with FromSomeDecorator[F] {

	override type GeneralizedClause[+G <: FromSome] = G Alias N
	override type WithClause[+G <: FromSome] = G Alias N
	override type This = clause.This Alias N

	override def withClause[G <: FromSome](from :G) :G Alias N = Alias(from, alias)

	override def where(filter :GlobalBoolean[Generalized]) :This =
		withClause(clause where filter.asInstanceOf[GlobalBoolean[clause.Generalized]]) //todo: eliminate the cast


	override val outer :Outer = clause.outer



	override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.alias(this)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Alias[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case as :Alias[_, _] if as canEqual this => clause == as.clause && alias == as.alias
		case _ => false
	}

	override def hashCode :Int = clause.hashCode * 31 + alias.hashCode


	override def toString :String = clause match {
		case join :AndFrom.* if join.condition != True => //inject the 'as' alias before the condition to reduce confusion.
			join.left.toString + " " + join.name + " " + join.right + " as " + alias + " on " + join.condition
		case _ => clause.toString + " as " + alias
	}
}






object Alias {
	def apply[F <: FromSome, N <: Label](from :F, name :N) :F Alias N =
		new Alias(from, name)

	def unapply(clause :FromClause) :Option[(FromSome, Label)] = clause match {
		case alias :Alias[_, _] => Some((alias.clause, alias.alias))
		case _ => None
	}

	def unapply[F <: FromClause](clause :DecoratedFrom[F]) :Option[(F, Label)] = clause match {
		case alias :Alias[_, _] => Some((clause.clause, alias.alias))
		case _ => None
	}

	def unapply[F <: FromSome, N <: Label](alias :Alias[F, N]) :Some[(F, N)] =
		Some((alias.clause, alias.alias))

	type * = Alias[_ <: FromSome, _ <: Label]

	type WithLabel[A <: Label] = { type F[+C <: FromSome] = C Alias A }
	type WithClause[C <: FromSome] = { type F[A <: Label] = C Alias A }


	implicit def aliasComposition[F <: FromSome, A <: Label]
			:FromSomeDecoratorComposition[F Alias A, F, WithLabel[A]#F] =
		composition.asInstanceOf[FromSomeDecoratorComposition[F Alias A, F, WithLabel[A]#F]]

	private[this] val composition = new FromSomeDecoratorComposition[Alias[FromSome, ""], FromSome, WithLabel[""]#F]

}

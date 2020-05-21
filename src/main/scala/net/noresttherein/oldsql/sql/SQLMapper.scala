package net.noresttherein.oldsql.sql


import scala.reflect.ClassTag

import net.noresttherein.oldsql.slang._

trait SQLMapper[+F <: FromClause, +Y[X]] {
	def apply[X](e :SQLFormula[F, X]) :Y[X]

	def unhandled(e :SQLFormula[F, _]) :Nothing =
		throw new IllegalArgumentException(s"Can't map formula $e :${e.getClass.getName} using $this")

	def unknown[E <: SQLFormula[F, _]](e :E, clazz :Class[E]) :Nothing =
		throw new IllegalArgumentException(s"Can't map formula $e :${e.getClass.getName} using $this - " +
			s"unexpected subclass of ${clazz.getName}")

	def unknown[E <: SQLFormula[F, _] :ClassTag](e :E) :Nothing =
		throw new IllegalArgumentException(s"Can't map formula $e :${e.getClass.getName} using $this - " +
			s"unexpected subclass of ${implicitly[ClassTag[E]].runtimeClass.getName}")



	override def toString = this.unqualifiedClassName
}






object SQLMapper {
//	type SQLMatcher[+S <: FromClause, +Y[+X]] = FormulaMatcher[S, Y]


	type FixedResult[Y] = { type T[X] = Y }


//	trait SQLReducer[+S <: FromClause, +Y] extends SQLMatcher[S, SQLMapper.FixedResult[Y]#T] {
//		def apply[X](f :SQLFormula[S, X]) :Y
//	}




}


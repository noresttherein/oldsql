package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.sql.SQLFormula.{ColumnFormula, FormulaMatcher}

import scala.reflect.ClassTag


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
}






object SQLMapper {
//	type SQLMatcher[+S <: FromClause, +Y[+X]] = FormulaMatcher[S, Y]


	type FixedResult[Y] = { type T[X] = Y }

//	trait SQLReducer[+S <: FromClause, +Y] extends SQLMatcher[S, SQLMapper.FixedResult[Y]#T] {
//		def apply[X](f :SQLFormula[S, X]) :Y
//	}




	type OptionResult[Y[+X]] = { type T[+X] = Option[Y[X]] }

	type SQLExtractor[+S <: FromClause, Y[+X]] = FormulaMatcher[S, OptionResult[Y]#T]


	type FormulaResult[S <: FromClause] = { type T[X] = SQLFormula[S, X] }
	type ColumnResult[S <: FromClause] = { type T[X] = ColumnFormula[S, X] }

//	type SQLRewriter[+F <: FromClause, T <: FromClause] = SQLMapper[F, FormulaResult[T]#T]
	trait SQLRewriter[+F <: FromClause, T <: FromClause] extends SQLMapper[F, FormulaResult[T]#T] {
		def apply[X](e :SQLFormula[F, X]) :SQLFormula[T, X]

		def apply[X](e :ColumnFormula[F, X]) :ColumnFormula[T, X]
	}

}


package com.hcore.ogre.sql

import com.hcore.ogre.mapping.AnyMapping
import com.hcore.ogre.sql.SQLFormula.SelectFormula.{SelectAsRows, SelectAsRow}
import com.hcore.ogre.sql.SQLFormula._
import shapeless.HList

import scala.reflect.ClassTag

trait SQLMapper[+S<:RowSource, +Y[X]] {
	def apply[X](f :SQLFormula[S, X]) :Y[X]

	def unhandled(f :SQLFormula[S, _]) :Nothing =
		throw new IllegalArgumentException(s"Can't map formula $f :${f.getClass.getName} using $this")
	
	def unknown[F<:SQLFormula[S, _]](f :F, clazz :Class[F]) :Nothing =
		throw new IllegalArgumentException(s"Can't map formula $f :${f.getClass.getName} using $this - unexpected subclass of ${clazz.getName}")

	def unknown[F <:SQLFormula[S, _] :ClassTag](f :F) :Nothing =
		throw new IllegalArgumentException(s"Can't map formula $f :${f.getClass.getName} using $this - unexpected subclass of ${implicitly[ClassTag[F]].runtimeClass.getName}")
}

//trait SQLMatcher[+S<:RowSource, +Y[X]] extends SQLMapper[S, Y] with FormulaMatcher[S, Y]

//trait SQLMatcher[+S<:RowSource, +Y[X]] extends SQLMapper[S, Y] {
//	override def apply[X](f: SQLFormula[S, X]): Y[X] = f.map(this)
//
//	protected[sql] def matchFormula[X](f: SQLFormula[S, X]): Y[X] //= unhandled(f)
//
//	protected[sql] def matchTerm[X](f :TermFormula[X]) :Y[X] //= matchFormula(f)
//
//	protected[sql] def matchLiteral[X](f :Literal[X]) :Y[X] //= matchTerm(f)
//
//	protected[sql] def matchBoolean(f :Literal[Boolean]) :Y[Boolean] //= matchLiteral(f)
//
//
//	protected[sql] def matchParameter[X](f :ParameterFormula[X]) :Y[X] //= matchTerm(f)
//
//	protected[sql] def matchBoundParameter[X](f :BoundParameter[X]) :Y[X] //= matchParameter(f)
//
//
//	protected[sql] def matchNull[X](f :SQLFormula.Null[X]) :Y[X] //= matchTerm(f)
//
//
//	protected[sql] def matchNative[X](f :NativeTerm[X]) :Y[X] //= matchTerm(f)
//
//
//
//	protected[sql] def matchComposite[X](f :CompositeFormula[S, X]) :Y[X] //= matchFormula(f)
//
//	protected[sql] def matchConversion[X, Z](f :AutoConversionFormula[S, Z, X]) :Y[X] //= matchComposite(f)
//
//
//	protected[sql] def matchTuple[X](f :SQLTuple[S, X]) :Y[X] //= matchComposite(f)
//
//	protected[sql] def matchHList[X<:HList](f :HListFormula[S, X]) :Y[X] //= matchTuple(f)
//
//
//	protected[sql] def matchSeq[X](f :SeqFormula[S, X]) :Y[Seq[X]] //= matchComposite(f)
//
//
//
//	protected[sql] def matchSelect[X](f :SelectFormula[S, X]) :Y[RowCursor[X]] //= matchFormula(f)
//
//
//	protected[sql] def matchRow[X](f :SelectAsRow[S, X]) :Y[X] //= matchConversion(f)
//
//	protected[sql] def matchRows[X](f :SelectAsRows[S, X]) :Y[Seq[X]] //= matchConversion(f)
//
//
//	protected[sql] def matchCondition(f :SQLCondition[S]) :Y[Boolean] //= matchFormula(f)
//
//	protected[sql] def matchExists[X](f :ExistsFormula[S, X]) :Y[Boolean] //= matchCondition(f)
//
//	protected[sql] def matchIn[X](f :In[S, X]) :Y[Boolean] //= matchCondition(f)
//
//
//
//	protected[sql] def matchPath[T<:AnyMapping, C<:AnyMapping](f :PathFormula[S, T, C]) :Y[C#ResultType] //= matchFormula(f)
//
//	protected[sql] def matchComponent[T<:AnyMapping, C<:AnyMapping](f :ComponentFormula[S, T, C]) :Y[C#ResultType] //= matchPath(f)
//
//
//	protected[sql] def matchComparison[X](f :ComparisonFormula[S, X]) :Y[Boolean] //= matchCondition(f)
//
//	protected[sql] def matchEquality[X](f :Equality[S, X]) :Y[Boolean] //= matchComparison(f)
//
//
//
//	protected[sql] def matchLogical[X](f :LogicalFormula[S]) :Y[Boolean] = matchComposite(f)
//
//	protected[sql] def matchNot(f :NotFormula[S]) :Y[Boolean] = matchLogical(f)
//
//	protected[sql] def matchAnd(f :And[S]) :Y[Boolean] = matchLogical(f)
//
//	protected[sql] def matchOr(f :Or[S]) :Y[Boolean] = matchLogical(f)
//
//}






object SQLMapper {
	type SQLMatcher[+S<:RowSource, +Y[X]] = FormulaMatcher[S, Y]
	
	
	type FixedResult[Y] = { type T[X] = Y }

	trait SQLReducer[+S<:RowSource, +Y] extends SQLMatcher[S, SQLMapper.FixedResult[Y]#T] {
		def apply[X](f :SQLFormula[S, X]) :Y
	}
	
	
	
	
	type OptionResult[Y[X]] = { type T[X] = Option[Y[X]] }

	type SQLExtractor[+S<:RowSource, Y[X]] = SQLMatcher[S, OptionResult[Y]#T]


	type FormulaResult[S<:RowSource] = { type T[X] = SQLFormula[S, X] }
	
	type SQLRewriter[+F<:RowSource, T<:RowSource] = SQLMapper[F, FormulaResult[T]#T]

	
}

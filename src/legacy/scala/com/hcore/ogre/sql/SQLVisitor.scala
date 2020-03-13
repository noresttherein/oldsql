package com.hcore.ogre.sql


/*
import com.hcore.ogre.mapping.Mapping
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.sql.SQLFormula.SelectFormula.{SelectAsRows, SelectAsRow}
import com.hcore.ogre.sql.SQLFormula._
import com.hcore.ogre.sql.SQLVisitor.Fixed
import shapeless.HList


//implicits
import SaferCasts._



/** Visitor pattern transforming an SQLExpression for source F into an SQLExpression based on source T. */
trait GenericSQLVisitor[+F<:RowSource, +R[T]] {

	def apply[X](expression :SQLFormula[F, X]) :R[X] = check(expression) {
		case e :HListFormula[_, _] =>
			tuple(e.asInstanceOf[HListFormula[F, HList]]).asInstanceOf[R[X]]
		case e :SeqFormula[_, _] =>
			seq(e.asInstanceOf[SeqFormula[F, Seq[_]]]).asInstanceOf[R[X]]
		case e :OrNull[_, _] =>
			option(e.asInstanceOf[OrNull[F, _]]).asInstanceOf[R[X]]
		case e:LogicalFormula[_] =>
			logical(e.downcast).asInstanceOf[R[X]]
		case e: Equality[_, _] =>
			comparison(e.downcast).asInstanceOf[R[X]]
		case e :In[_, _] =>
			in(e.downcast).asInstanceOf[R[X]]
		case e :ExistsFormula[_, _] =>
			exists(e.downcast).asInstanceOf[R[X]]
		case e :SelectAsRow[_, _] =>
			row(e.downcast).asInstanceOf[R[X]]
		case e :SelectAsRows[_, _] =>
			rows(e.downcast).asInstanceOf[R[X]]
		case e :SelectFormula[_, _] =>
			select(e.downcast).asInstanceOf[R[X]]
		case e :PathFormula[_, _, _] =>
			path(e.asInstanceOf[PathFormula[F, _<:Mapping, _<:Mapping]]).asInstanceOf[R[X]]
		case e :TermFormula[_] =>
			term(e.crosstyped[X])

	}
	
	def term[X](e :TermFormula[X]) :R[X]

	def option[X](e :OrNull[F, X]) :R[Option[X]]

	def tuple[X<:HList](e :HListFormula[F, X]) :R[X]

	def seq[X](e :SeqFormula[F, X]) :R[Seq[X]]


	def logical(e :LogicalFormula[F]) :R[Boolean] = check(e) {
		case a:And[_] => and(a.asInstanceOf[And[F]])
		case o:Or[_] => or(o.asInstanceOf[Or[F]])
		case n:NotFormula[_] => not(n.asInstanceOf[NotFormula[F]])
	}

	def and(e :And[F]) :R[Boolean]

	def or(e :Or[F]) :R[Boolean]

	def not(e :NotFormula[F]) :R[Boolean]
	
	def comparison(e :BooleanFormula[F]) :R[Boolean] = check(e) {
		case e: Equality[_, _] => equality(e.downcast)
	}
	
	def equality[X](e: Equality[F, X]) :R[Boolean]

	def in[X](e :In[F, X]) :R[Boolean]

	def path[M<:Mapping, C<:Mapping](e :PathFormula[F, M, C]) :R[C#ResultType]

	def select[H](e :SelectFormula[F, H]) :R[RowCursor[H]]

	def exists[H](e :ExistsFormula[F, H]) :R[Boolean]
	
	def row[H](e :SelectAsRow[F, H]) :R[H]
	
	def rows[H](e :SelectAsRows[F, H]) :R[Seq[H]]

	
	protected[this] def check[X](expression :SQLFormula[F, X])(fun :PartialFunction[SQLFormula[_<:F, X], R[X]]) :R[X] =
		fun.applyOrElse(expression, (_:Any) => throw new IllegalArgumentException(s"Unrecognized expression type: $expression :${expression.getClass.getName}"))
	
	
}


trait GenericSQLTermVisitor[+S<:RowSource, +R[T]] extends GenericSQLVisitor[S, R] {
	def term[X](e :TermFormula[X]) :R[X] = check(e) {
		case l :Literal[_] => literal(l.crosstyped[X])
//		case l :UnboundParameter[_] => parameter(l.crosstyped[X])
		case l :BoundParameter[_] => parameter(l.crosstyped[X])
		case l :NativeTerm[_] => native(l.crosstyped[X])
		case l :Null[_] => sqlNull(l.crosstyped[X])
	}

	def literal[X](e :Literal[X]) :R[X] = check(e) {
		case True() | False() =>
			bool(e.crosstyped[Boolean]).asInstanceOf[R[X]]
		case _ => nonbool(e)
	}

	def parameter[X](e :BoundParameter[X]) :R[X]

//	def parameter[X](e :UnboundParameter[X]) :R[X]

	def native[X](e :NativeTerm[X]) :R[X]

	def sqlNull[X](e :Null[X]) :R[X]

	def bool(e :Literal[Boolean]) :R[Boolean]

	def nonbool[X](e :Literal[X]) :R[X]

}

trait SQLVisitor[+S<:RowSource, +R] extends GenericSQLVisitor[S, Fixed[R]#Result]

//trait SQLReducer[+S<:RowSource, +R] extends SQLVisitor[S, R] {
//
//
//	override def option[X](e: OrNull[S, X]) = composite(e)
//
//	override def tuple[X <: HList](e: HListFormula[S, X]) = composite(e)
//
//	override def seq[X](e: SeqFormula[S, X]) = composite(e)
//
//	override def and(e: And[S]) = composite(e)
//
//	override def or(e: Or[S]) = composite(e)
//
//	override def not(e: NotFormula[S]) = composite(e)
//
//
//
//	override def equality[X](e: Equality[S, X]) = composite(e)
//
//	override def in[X](e: In[S, X]) = composite(e)
//
//
//	override def exists[H](e: ExistsFormula[S, H]) = apply(e.select)
//
//	override def row[H](e: SelectAsRow[S, H]) = apply(e.select)
//
//	override def rows[H](e: SelectAsRows[S, H]) = apply(e.select)
//
//	protected def composite[T](e :CompositeFormula[S, T]) :R = reduce(e.parts.map(apply(_)) :_*)
//
//	protected[this] def reduce(results :R*) :R
//
//
//}


trait SQLTermVisitor[+S<:RowSource, +R] extends SQLVisitor[S, R] with GenericSQLTermVisitor[S, Fixed[R]#Result]


object SQLVisitor {
	type Fixed[T] = { type Result[X] = T }

	trait GenericActualSQLVisitor[+S<:RowSource, +R[T]] extends GenericSQLVisitor[S, R] {
		override def path[M <: Mapping, C <: Mapping](e: PathFormula[S, M, C]): R[C#ResultType] =
			e.ifSubclass[ComponentFormula[S, M, C]].orElse(component)(abstractPath(e))

		def component[M <: Mapping, C <: Mapping](e: ComponentFormula[S, M, C]): R[C#ResultType]

		def abstractPath[M <: Mapping, C <: Mapping](e: PathFormula[S, M, C]): R[C#ResultType] =
			throw new IllegalArgumentException(s"Expected an actual sql expression but got abstract path $e")
	}

	trait ActualSQLVisitor[+S<:RowSource, +R] extends GenericActualSQLVisitor[S, Fixed[R]#Result] with SQLTermVisitor[S, R]
}



*/
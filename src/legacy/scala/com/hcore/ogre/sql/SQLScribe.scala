package com.hcore.ogre.sql

import com.hcore.ogre.mapping.{Mapping, AnyMapping, ComponentPath}
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.sql.RowSource.{SelectSource, SubsourceOf, TableFormula}
import com.hcore.ogre.sql.SQLFormula.CompositeFormula.CaseComposite
import com.hcore.ogre.sql.SQLFormula.PathFormula.{MatchPath, CasePath}
import com.hcore.ogre.sql.SQLFormula.SelectFormula._
import com.hcore.ogre.sql.SQLFormula.TermFormula.CaseTerm
import com.hcore.ogre.sql.SQLFormula._
import com.hcore.ogre.sql.SQLMapper.{FormulaResult => Res, SQLRewriter, SQLMatcher}
import shapeless.HList


//implicits
import SaferCasts._


//trait FormulaRewriter[F<:RowSource, T<:RowSource] extends SQLMatcher[F, Res[T]]

/** Visitor pattern transforming an SQLExpression for source F into an SQLExpression based on source T. */
abstract class SQLScribe[F<:RowSource, T<:RowSource](protected val from :F, protected val to :T)
//	extends GenericSQLVisitor[F, ({ type R[X] = SQLFormula[T, X] })#R]
	extends SQLMatcher[F, Res[T]#T] with CaseComposite[F, Res[T]#T] with CaseTerm[F, Res[T]#T] with MatchSelect[F, Res[T]#T]
{ scribe =>


	override def apply[X](f: SQLFormula[F, X]): SQLFormula[T, X] = super.apply(f)

	override def term[X](e: TermFormula[X]): SQLFormula[T, X] = e


	override def composite[X](f: CompositeFormula[F, X]) :SQLFormula[T, X] = f.map(this)

//	override def tuple[X<:HList](e :HListFormula[F, X]) :SQLFormula[T, X] = tupleMember(e)

//	override def seq[X](e :SeqFormula[F, X]) :SQLFormula[T, Seq[X]] =
//		SeqFormula(e.expressions.map(apply(_)))


//	override def option[X](e: OrNull[F, X]): SQLFormula[T, Option[X]] = OrNull(apply(e.expr))

//	protected def tupleMember[X<:HList](e :HListFormula[F, X]) :HListFormula[T, X] = e match {
//		case SQLHNil =>
//			SQLHNil.asInstanceOf[HListFormula[T, X]]
//		case HListHead(head, tail) =>
//			HListHead(apply(head.asInstanceOf[SQLFormula[F, Any]]), tupleMember(tail.asInstanceOf[HListFormula[F, HList]])).asInstanceOf[HListFormula[T, X]]
//	}


//	override def and(e :And[F]) :SQLFormula[T, Boolean] =
//		And(e.conditions.map(apply(_)))
//
//	override def or(e :Or[F]) :SQLFormula[T, Boolean] =
//		Or(e.conditions.map(apply(_)))
//
//	override def not(e: NotFormula[F]): SQLFormula[T, Boolean] = !apply(e.formula)
//
//	override def equality[X](e: Equality[F, X]) :BooleanFormula[T] =
//		apply(e.left) === apply(e.right)
//
//	override def in[X](e :In[F, X]) :BooleanFormula[T] =
//		In(apply(e.left), apply(e.right))
//
//
//	override def row[H](e: SelectAsRow[F, H]): SQLFormula[T, H] = select(e.select).single
//
//	override def rows[H](e: SelectAsRows[F, H]): SQLFormula[T, Seq[H]] = select(e.select).rows
//
//	override def exists[H](e: ExistsFormula[F, H]): SQLFormula[T, Boolean] =
//		select(e.select).exists

//	override def select[H](e :SelectFormula[F, H]) :SelectFormula[T, H] = e match {
//		case s :GroundedSelectFormula[_] =>
//			select(s.asInstanceOf[GroundedSelectFormula[H]])
//		case s :SubselectFormula[_, _, _] =>
//			subselect(s.asInstanceOf[SubselectFormula[F, SubsourceOf[F], H]])
//	}

	override def select[H](e :GroundedSelectFormula[H]) :SelectFormula[T, H] = e

	private def subselect[R<:SubsourceOf[F], H](e :SubselectFormula[F, R, H]) :SelectFormula[T, H] = {
		val sf = e.asInstanceOf[SubselectFormula[F, SubsourceOf[F], H]]
		val transplanted = sf.source.transplant(to, this.asInstanceOf[SQLScribe[sf.source.Parent, T]])
		val header = SQLScribe.subselect[F, SubsourceOf[F], T, transplanted.type, H](sf.header, sf.source, transplanted, this)
		SelectFormula.subselect[T, transplanted.type, H](to, transplanted :transplanted.type, header)
	}

	override def subselect[X](f: SelectFormula[F, X]): SelectFormula[T, X] =
		subselect(f.asInstanceOf[SubselectFormula[F, SubsourceOf[F], X]])
}




/** Rewrites sql expressions using given term substitution functions. */
object SQLScribe {

	/** Rewrite the given expression grounded in source F into an expression grounded in source T substituting
	  * all source-dependent expressions (i.e. PathExpressions) for values returned by the provided function.
	  * @param expression expression to rewrite
	  * @param from source in which the given expression is grounded
	  * @param to source in which the result expression should be grounded
	  * @param mapper substitution function returning an expression that should take place of the argument in the result expression.
	  *               Returned expression must be of the same type (i.e C#ResultType) as argument expression.
	  * @tparam F source source type
	  * @tparam T target source type
	  */
//	def apply[F<:RowSource, T<:RowSource](expression :BooleanFormula[F], from :F, to :T)(mapper :PathFormula[F, _<:Mapping, _<:Mapping]=>SQLFormula[T, _]) :BooleanFormula[T] =
//		translate[F, T, Boolean](expression, from, to)(mapper)

	
//	def apply[F<:RowSource, T<:RowSource](from :F, to :T)(mapper :PathFormula[F, _<:Mapping, _<:Mapping]=>PathFormula[T, _<:Mapping, _<:Mapping]) :SQLFormula[T, Boolean] =
//		translate(from)
	
	def apply[F<:RowSource, T<:RowSource, X](expression :SQLFormula[F, X], from :F, to :T)(mapper :PathFormula[F, _<:AnyMapping, _<:AnyMapping]=>SQLFormula[T, _]) :SQLFormula[T, X] =
		translate[F, T, X](expression, from, to)(mapper)

	/** Rewrite the given expression grounded in source from into an expression grounded in source to substituting
	  * all source-dependent expressions (PathExpression instances) for values returned by the provided function.
	  */
	def apply[T](from :RowSource, to :RowSource)(expression :SQLFormula[from.type, T])(mapper :PathFormula[from.type, _<:AnyMapping, _<:AnyMapping]=>SQLFormula[to.type, _]) :SQLFormula[to.type, T] =
		translate[from.type, to.type, T](expression, from, to)(mapper)





	def translate[F<:RowSource, T<:RowSource, X](expression :SQLFormula[F, X], from :F, to :T)(mapper :PathFormula[F, _<:AnyMapping, _<:AnyMapping]=>SQLFormula[T, _]) :SQLFormula[T, X] =
		expression.applyTo[Res[T]#T](new AbstractSQLScribe[F, T](from, to) {
			override def path[M <: AnyMapping, C <: AnyMapping](e: PathFormula[F, M, C]): SQLFormula[T, C#ResultType] =
				mapper(e).asInstanceOf[SQLFormula[T, C#ResultType]]
		})


	/** Rewrite the given expression grounded in source F into an expression grounded in source T by substituting the tables in
	  * all PathExpression instances to the last returned by the provided function. Can be used to 'copy' an expression into
	  * another, isomorphic row source.
	  * @param expression source expression
	  * @param from source in which the given expression is grounded
	  * @param to source in which the result expression should be grounded
	  * @param mapper function returning a last that should take the part of the argument last in the result expression.
	  *               The mapping of the argument and return value have to be equal, or an exception will be thrown.
	  */
	def replant[F<:RowSource, T<:RowSource, X](expression :SQLFormula[F, X], from :F, to :T)(mapper :TableFormula[F, _<:AnyMapping]=>TableFormula[T, _<:AnyMapping]) :SQLFormula[T, X] =
		expression.applyTo[Res[T]#T](new Replanter[F, T](from, to) {
			override def map[M <: AnyMapping](table: TableFormula[F, M]): TableFormula[T, M] = mapper(table).asInstanceOf[TableFormula[T, M]]
		})

	/** Rewrite a formula grounded in source S representing a subselect in source F of parent scribe
	  * to subselect source R of target source X of parent scribe.
 	  * @param expr formula to be rewritten from source from :S to source to :R
	  * @param from base source of the argument formula representing a subselect source nested in source F
	  * @param to target row source of rewritten formula.
	  * @param scribe scribe used to rewrite formulas grounded in original source F
	  * @tparam F source which is the 'parent' source of subselect source S, i.e. S.Parent = F,
	  *           representing the fact that source S implicitly imports tables present in source F
	  * @tparam S base source of the argument formula representing a source for a subselect nested in source F,
	  *           i.e. S <:< F SubselectJoin ... (where ... is any sequence of joined mappings)
	  * @tparam T parent source of the target subselect source, isomorphic with F (i.e. containing the same tables in the same order as F)
	  * @tparam R target source in which rewritten formula will be grounded, isomorphic with source S (representing the same last list)
	  * @tparam X value type of the rewritten formula
	  * @return a formula isomorphic with argument formula, where all references to tables from S are replaced
	  *         to references to a last in R at the corresponding index.
	  */
	def subselect[F<:RowSource, S<:SubsourceOf[F], T<:RowSource, R<:SubsourceOf[T], X](
			expr :SQLFormula[S, X], from :S, to :R, scribe :SQLScribe[F, T]) :SQLFormula[R, X] =
	{
		val tables = to.all.toIndexedSeq
		translate(expr, from, to) {
			case p if p.table.index < from.parent.size =>
				scribe(p.asInstanceOf[SQLFormula[F, Any]]).asInstanceOf[SQLFormula[R, Any]]
			case p @ PathFormula(table, path) =>
				val mapped = tables(table.index - from.size + to.size).asAny
				if (mapped.mapping != table.mapping)
					throw new IllegalStateException(s"Failed to translate $p of subselect $from while rewriting to $to. Expected last for $table, got $mapped")
				PathFormula(mapped, path.unchecked)
		}
	}

//	trait ActualSQLScribe[F<:RowSource, T<:RowSource] extends SQLScribe[F, T] with GenericActualSQLVisitor[F, ({ type R[X] = SQLFormula[T, X] })#R]
	abstract class AbstractSQLScribe[F<:RowSource, T<:RowSource](from :F, to :T) extends SQLScribe[F, T](from, to) with CasePath[F, Res[T]#T]

	abstract class ConcreteSQLScribe[F<:RowSource, T<:RowSource](from :F, to :T) extends SQLScribe[F, T](from, to) with MatchPath[F, Res[T]#T] {

		override def path[M <: AnyMapping, C <: AnyMapping](f: PathFormula[F, M, C]): SQLFormula[T, C#ResultType] =
			throw new IllegalArgumentException(s"can't rewrite expression containing abstract path $f :${f.getClass.getName} with $this")
	}

	abstract class Replanter[F<:RowSource, T<:RowSource](from :F, to :T) extends AbstractSQLScribe[F, T](from, to) {
		override def path[M <: AnyMapping, C <: AnyMapping](e: PathFormula[F, M, C]): SQLFormula[T, C#ResultType] = {
			val mapped = map(e.table)
			if (mapped.mapping!=e.table.mapping)
				throw new AssertionError(s"error while mapping $e: result last mapping $mapped doesn't equal input mapping ${e.table}.")
			PathFormula[T, M, C](mapped, e.path)
		}

		def map[M<:AnyMapping](table :TableFormula[F, M]) :TableFormula[T, M]

	}


}
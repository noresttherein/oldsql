package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.sql.RowProduct.{SubselectFrom, TableFormula}
import net.noresttherein.oldsql.sql.SelectFormula.MatchSelect
import net.noresttherein.oldsql.sql.SQLFormula.CompositeFormula.CaseComposite
import net.noresttherein.oldsql.sql.SQLFormula.CompositeFormula
import net.noresttherein.oldsql.sql.SQLMapper.{SQLMatcher, FormulaResult => Res}
import net.noresttherein.oldsql.sql.SQLTerm.CaseTerm
import net.noresttherein.oldsql.sql.ast.SQLTerm


/** Visitor pattern transforming an `SQLFormula` over source `F` into an `SQLFormula` over source `T`. */
abstract class SQLScribe[F <: RowProduct, T <: RowProduct](protected val from :F, protected val to :T)
//	extends GenericSQLVisitor[F, ({ type R[X] = SQLFormula[T, X] })#R]
	extends SQLMatcher[F, Res[T]#T] with CaseComposite[F, Res[T]#T] with CaseTerm[F, Res[T]#T] with MatchSelect[F, Res[T]#T]
{ scribe =>


	override def apply[X](f: SQLFormula[F, X]): SQLFormula[T, X] = super.apply(f)

	override def term[X](e: SQLTerm[X]): SQLFormula[T, X] = e


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
	//		case s :FreeSelectFormula[_] =>
	//			select(s.asInstanceOf[FreeSelectFormula[H]])
	//		case s :SubselectFormula[_, _, _] =>
	//			subselect(s.asInstanceOf[SubselectFormula[F, AsSubselectOf[F], H]])
	//	}

	override def select[H](e :GroundedSelectFormula[H]) :SelectFormula[T, H] = e

	private def subselect[R<:SubselectFrom[F], H](e :SubselectFormula[F, R, H]) :SelectFormula[T, H] = {
		val sf = e.asInstanceOf[SubselectFormula[F, SubselectFrom[F], H]]
		val transplanted = sf.source.transplant(to, this.asInstanceOf[mechanics.SQLScribe[sf.source.Parent, T]])
		val header = SQLScribe.subselect[F, SubselectFrom[F], T, transplanted.type, H](sf.header, sf.source, transplanted, this)
		SelectFormula.subselect[T, transplanted.type, H](to, transplanted :transplanted.type, header)
	}

	override def subselect[X](f: SelectFormula[F, X]): SelectFormula[T, X] =
		subselect(f.asInstanceOf[SubselectFormula[F, SubselectFrom[F], X]])
}




/** Rewrites sql expressions using given term substitution functions. */
object SQLScribe {

	/** Rewrite the given expression grounded in source F into an expression grounded in source T substituting
	  * all source-dependent expressions (i.e. PathExpressions) for values returned by the provided function.
	  * @param expression expression to rewrite
	  * @param from source in which the given expression is grounded
	  * @param to source in which the result expression should be grounded
	  * @param mapper substitution function returning an expression that should take place of the argument in the result expression.
	  *               Returned expression must be of the same type (i.e C#Subject) as argument expression.
	  * @tparam F source source type
	  * @tparam T target source type
	  */
	//	def apply[F<:RowProduct, T<:RowProduct](expression :BooleanFormula[F], from :F, to :T)(mapper :PathFormula[F, _<:Mapping, _<:Mapping]=>SQLFormula[T, _]) :BooleanFormula[T] =
	//		translate[F, T, Boolean](expression, from, to)(mapper)


	//	def apply[F<:RowProduct, T<:RowProduct](from :F, to :T)(mapper :PathFormula[F, _<:Mapping, _<:Mapping]=>PathFormula[T, _<:Mapping, _<:Mapping]) :SQLFormula[T, Boolean] =
	//		translate(from)

	def apply[F<:RowProduct, T<:RowProduct, X](expression :SQLFormula[F, X], from :F, to :T)(mapper :PathFormula[F, _<:Mapping, _<:Mapping]=>SQLFormula[T, _]) :SQLFormula[T, X] =
		translate[F, T, X](expression, from, to)(mapper)

	/** Rewrite the given expression grounded in source from into an expression grounded in source to substituting
	  * all source-dependent expressions (PathExpression instances) for values returned by the provided function.
	  */
	def apply[T](from :RowProduct, to :RowProduct)(expression :SQLFormula[from.type, T])(mapper :PathFormula[from.type, _<:Mapping, _<:Mapping]=>SQLFormula[to.type, _]) :SQLFormula[to.type, T] =
		translate[from.type, to.type, T](expression, from, to)(mapper)





	def translate[F<:RowProduct, T<:RowProduct, X](expression :SQLFormula[F, X], from :F, to :T)(mapper :PathFormula[F, _<:Mapping, _<:Mapping]=>SQLFormula[T, _]) :SQLFormula[T, X] =
		expression.applyTo[Res[T]#T](new AbstractSQLScribe[F, T](from, to) {
			override def path[M <: Mapping, C <: Mapping](e: PathFormula[F, M, C]): SQLFormula[T, C#Subject] =
				mapper(e).asInstanceOf[SQLFormula[T, C#Subject]]
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
	def replant[F<:RowProduct, T<:RowProduct, X](expression :SQLFormula[F, X], from :F, to :T)(mapper :TableFormula[F, _<:Mapping]=>TableFormula[T, _<:Mapping]) :SQLFormula[T, X] =
		expression.applyTo[Res[T]#T](new Replanter[F, T](from, to) {
			override def map[M <: Mapping](table: TableFormula[F, M]): TableFormula[T, M] = mapper(table).asInstanceOf[TableFormula[T, M]]
		})

	/** Rewrite a formula grounded in source S representing a subselect in source F of parent scribe
	  * to subselect source R of target source X of parent scribe.
	  * @param expr formula to be rewritten from source from :S to source to :R
	  * @param from base source of the argument formula representing a subselect source nested in source F
	  * @param to target row source of rewritten formula.
	  * @param scribe scribe used to rewrite formulas grounded in original source F
	  * @tparam F source which is the 'outer' source of subselect source S, i.e. S.Outer = F,
	  *           representing the fact that source S implicitly imports tables present in source F
	  * @tparam S base source of the argument formula representing a source for a subselect nested in source F,
	  *           i.e. S <:< F SubselectJoin ... (where ... is any sequence of joined mappings)
	  * @tparam T outer source of the target subselect source, isomorphic with F (i.e. containing the same tables in the same order as F)
	  * @tparam R target source in which rewritten formula will be grounded, isomorphic with source S (representing the same last list)
	  * @tparam X value type of the rewritten formula
	  * @return a formula isomorphic with argument formula, where all references to tables from S are replaced
	  *         to references to a last in R at the corresponding index.
	  */
	def subselect[F<:RowProduct, S<:SubselectFrom[F], T<:RowProduct, R<:SubselectFrom[T], X](
		                                                                                  expr :SQLFormula[S, X], from :S, to :R, scribe :mechanics.SQLScribe[F, T]) :SQLFormula[R, X] =
	{
		val tables = to.all.toIndexedSeq
		translate(expr, from, to) {
			case p if p.table.index < from.outer.size =>
				scribe(p.asInstanceOf[SQLFormula[F, Any]]).asInstanceOf[SQLFormula[R, Any]]
			case p @ PathFormula(table, path) =>
				val mapped = tables(table.index - from.size + to.size).asAny
				if (mapped.mapping != table.mapping)
					throw new IllegalStateException(s"Failed to translate $p of subselect $from while rewriting to $to. Expected last for $table, got $mapped")
				PathFormula(mapped, path.unchecked)
		}
	}

	//	trait ActualSQLScribe[F<:RowProduct, T<:RowProduct] extends SQLScribe[F, T] with GenericActualSQLVisitor[F, ({ type R[X] = SQLFormula[T, X] })#R]
	abstract class AbstractSQLScribe[F<:RowProduct, T<:RowProduct](from :F, to :T) extends mechanics.SQLScribe[F, T](from, to) with CasePath[F, Res[T]#T]

	abstract class ConcreteSQLScribe[F<:RowProduct, T<:RowProduct](from :F, to :T) extends mechanics.SQLScribe[F, T](from, to) with MatchPath[F, Res[T]#T] {

		override def path[M <: Mapping, C <: Mapping](f: PathFormula[F, M, C]): SQLFormula[T, C#Subject] =
			throw new IllegalArgumentException(s"can't rewrite expression containing abstract path $f :${f.getClass.getName} with $this")
	}

	abstract class Replanter[F<:RowProduct, T<:RowProduct](from :F, to :T) extends AbstractSQLScribe[F, T](from, to) {
		override def path[M <: Mapping, C <: Mapping](e: PathFormula[F, M, C]): SQLFormula[T, C#Subject] = {
			val mapped = map(e.table)
			if (mapped.mapping!=e.table.mapping)
				throw new AssertionError(s"error while mapping $e: result last mapping $mapped doesn't equal input mapping ${e.table}.")
			PathFormula[T, M, C](mapped, e.path)
		}

		def map[M<:Mapping](table :TableFormula[F, M]) :TableFormula[T, M]

	}


}


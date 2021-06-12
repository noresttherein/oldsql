package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, GroundFrom, NonEmptyFrom, TopFrom}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.TypedColumnComponentSQL.ColumnComponentVisitor
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL.{CaseComponent, ComponentVisitor}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.LooseColumn.LooseColumnVisitor
import net.noresttherein.oldsql.sql.ast.LooseComponent.{CaseLooseComponent, LooseComponentVisitor}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}

//here be implicits
import net.noresttherein.oldsql.slang._






/** An SQL expression AST node represented by a mapping `M`. While `M` might be a subtype of
  * [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]], typically it is a component of some relation
  * from the ''from'' clause of an SQL select containing this expression. The value type of this expression is defined
  * as the mapped [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type, as seen by the application,
  * but will be represented in actual generated SQL as a tuple containing all columns of the mapping `M`.
  * If the expression is used literally as part of a ''select'' clause (either directly, or inside
  * a [[net.noresttherein.oldsql.sql.ast.TupleSQL tuple]]), default
  * [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns (those without a
  * [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]] buff) will be inlined in its place.
  * If used as part of a comparison in a ''where'' or ''having'' clause, the columns of the two expressions will
  * be compared ordinally.
  * @author Marcin Mościcki
  */
trait MappingSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]]
	extends SQLExpression[F, S, M[Unit]#Subject]
{
	type Origin >: F <: RowProduct
	type Subject = M[Unit]#Subject

//	override def readForm :SQLReadForm[Subject] //= mapping.withOrigin[Any].selectForm

	def mapping :M[Origin]

	/** Returns `this` upcast to an `SQLExpression`. This method exists because many expressions,
	  * such as `table \ component`, producing some subtype of a `MappingSQL` in a place where
	  * its supertype `SQLExpression[F, S, Subject]` is expected, will confuse the compiler and make type inference fail.
	  * While simply splitting the above into a `val` assignment and its access would solve the issue, calling
	  * `(table \ component).upcast` is the most concise way of separating the expression creation with the type
	  * inference and the returned value.
	  */
	def upcast :SQLExpression[F, S, Subject] = this

	override def groundValue :Opt[Subject] = Lack

	override def anchor(from :F) :MappingSQL[F, GlobalScope, M]

	override def selectFrom(from :F) :SelectAs[from.Base, M] =
		if (from.isParameterized)
			throw new IllegalArgumentException(
				s"Cannot use a parameterized clause as a basis for select $this: $from."
			)
		else if (from.isSubselect) {
			subselectFrom(from.asInstanceOf[ExactSubselectOf[from.type, NonEmptyFrom]])
				.asInstanceOf[SelectAs[from.Base, M]]
		} else
			topSelectFrom(from.asInstanceOf[F with GroundFrom])

	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectAs[M] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectAs[B, M] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:SelectMapping[P, M] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

}






object MappingSQL {

	trait MappingColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnComponentVisitor[F, Y] with LooseColumnVisitor[F, Y]

	trait MappingVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MappingColumnVisitor[F, Y] with ComponentVisitor[F, Y] with LooseComponentVisitor[F, Y]
	{
		def mapping[S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]](e :MappingSQL[F, S, M]) :Y[S, M[Unit]#Subject]
	}

	trait MatchMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MappingVisitor[F, Y]
		with CaseComponent[F, Y] with CaseLooseComponent[F, Y]

	trait CaseMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchMapping[F, Y] {

		override def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V] =
			mapping(e)

		override def looseComponent[J >: F <: RowProduct, M[A] <: BaseMapping[X, A], X]
		                          (e :LooseComponent[J, M, X]) :Y[GlobalScope, X] =
			mapping(e)
	}

}



package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bases.BaseColumn
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, Select, SQLExpression}
import net.noresttherein.oldsql.sql.RowProduct.{Complete, GroundRow, NonEmptyRow, SubselectOf, TopRow}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}
import net.noresttherein.oldsql.sql.ast.SelectableSQL.SelectableSQLTemplate
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.mechanics.CanSelect
import net.noresttherein.oldsql.sql.mechanics.CanSelect.{CanSelectDef, CanSelectDirect}









/** An base trait for SQL expression types which can be used as ''select'' clauses.
  * All concrete subclasses are required to provide valid implementation of methods
  * [[net.noresttherein.oldsql.sql.SQLExpression.topSelectFrom topSelectFrom]],
  * [[net.noresttherein.oldsql.sql.SQLExpression.subselectFrom subselectFrom]] and
  * [[net.noresttherein.oldsql.sql.SQLExpression.paramSelectFrom paramSelectFrom]], although the implementations
  * provided here are stubs frowing an [[UnsupportedOperationException]].
  *
  * The companion object to this trait introduces [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]]
  * type classes allowing to use any `SelectableSQL` as an argument to
  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] extension methods.
  *
  * Two additional base traits are provided, which narrow down return types to
  * [[net.noresttherein.oldsql.sql.ast.SelectAs SelectAs]]
  * and [[net.noresttherein.oldsql.sql.ast.SelectColumnAs SelectColumnAs]].
  * @tparam F the domain ''from'' clause in which this expression is grounded.
  * @tparam S the scope marker specifying if the expression contains aggregate functions.
  * @tparam V the value type of this expression.
  * @see [[net.noresttherein.oldsql.sql.ast.SelectableMappingSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.SelectableColumnMappingSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.SelectableSQL.NonSelectableSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.SelectableSQL.SelectableSQLTemplate]]
  */ //it's important that it does not extend SelectableSQLTemplate to be safe for extension with higher type arguments
//todo: remove it; make QuerySQL selectable by moving it to the from clause and selecting individual columns and
//  throw an exception for FunctionSQL (non FunctionColumnSQL)
trait SelectableSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
	extends SQLExpression[F, S, V]
{
	override type isSelectable = true

	//todo: factory methods should ideally return a SelectableSQL, as PromotionsQL is selectable, too
//	override def map[X](f :V => X) :SelectableSQL[F, S, X] = ???
//	override def to[X](implicit lift :SQLExpression.Lift[V, X]) :SelectableSQL[F, S, X] = ???

	override def topSelectFrom[E <: F with GroundRow { type Complete  <: E }](from :E) :TopSelectSQL[V] =
		SelectSQL(from, this)

	override def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :SubselectSQL[B, V] =
		SelectSQL.subselect[B, F with SubselectOf[B], V](from, this)
//
//	override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectSQL[B, V] =
//		SelectSQL.subselect(from, this)

	override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E) :Select[P, V] =
		Select(from)(this)
}



object SelectableSQL {
//	private[sql] trait ExpressionSelectMethods[-F <: RowProduct, V] {
//		def selectFrom(from :F) :Any
//		def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E) :Any
//		def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :Any
//		def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E) :Any
//	}

	/** A mixin trait for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] which
	  * narrows down the return types of [[net.noresttherein.oldsql.sql.SQLExpression.selectFrom selectFrom]],
	  * [[net.noresttherein.oldsql.sql.SQLExpression.topSelectFrom topSelectFrom]] and
	  * [[net.noresttherein.oldsql.sql.SQLExpression.subselectFrom subselectFrom]] to its type parameter `Sel[_]`
	  * and implements `selectFrom` by delegating to the appropriate method from the other two.
	  *
	  * This trait should not be extended by generic expressions open to arbitrary extension by application classes:
	  * any generic expression `E[F, V] <: SelectableSQL[F, V, Sel]` which mixes it in cannot be later extended
	  * by a type `S[F] <: E[F, V]`, where `V` resolves to a higher type, as it would result
	  * in a non finitary class graph (infinite subtype relation graph from expanding the type definition).
	  * @tparam F the domain ''from'' clause in which this expression is grounded.
	  * @tparam V the value type of this expression.
	  * @see [[net.noresttherein.oldsql.sql.ast.SelectableSQL]]
	  */
	private[sql] trait SelectableSQLTemplate[-F <: RowProduct, V, +Sel[-A <: RowProduct] <: SelectSQL[A, V]] {
		this :SQLExpression[F, Grouped, V] =>

		override def selectFrom(from :F) :Sel[from.Base] =
			if (from.isParameterized)
				throw new IllegalArgumentException(
					s"Cannot use a parameterized clause as a basis for select $this: $from."
				)
			else if (from.isSubselect) {
				subselectFrom[NonEmptyRow](from.asInstanceOf[F with SubselectOf[NonEmptyRow]]).asInstanceOf[Sel[from.Base]]
			} else
				(this :SelectableSQLTemplate[F, V, Sel]) topSelectFrom from.asInstanceOf[Complete[F with GroundRow]]

		override def topSelectFrom[E <: F with GroundRow { type Complete  <: E }](from :E) :Sel[RowProduct] =
			throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")

		override def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :Sel[B] =
			throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")
//		override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :Sel[B] =
//			throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")
	}

	/** A mixin trait for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] which overrides methods
	  * `selectFrom`, `topSelectFrom`, `subselectFrom`, `paramSelectFrom` with a return type of `Nothing`.
	  */
	trait NonSelectableSQL[-F <: RowProduct, V]
		extends SelectableSQLTemplate[F, V, CanSelect.NoType]
	{ this :SQLExpression[F, Grouped, V] => }



	implicit def canSelect[F <: RowProduct, B <: RowProduct, E, V, O <: RowProduct]
	                      (implicit selectClauseType :E <:< SelectableSQL[O, Grouped, V],
	                       fromClauseType :F <:< O { type Base = B; type DefineBase[+I <: RowProduct] = I })
			:CanSelectDirect[F, E, SelectSQL[B, V]] =
		new CanSelectDef[F, E, SelectSQL[B, V]] {
			override def apply(from :F, expr :E) = expr selectFrom from
		}

	implicit def canTopSelect[F <: GroundRow { type Complete = C }, C <: GroundRow { type Complete = C },
	                          E, V, O >: C <: RowProduct]
	                         (implicit selectClauseType :E <:< SelectableSQL[O, Grouped, V],
	                          fromClauseType :F <:< GroundRow { type Complete = C })
			:CanSelectDirect[F, E, TopSelectSQL[V]] =
		new CanSelectDef[F, E, TopSelectSQL[V]] {
			override def apply(from :F, expr :E) = expr topSelectFrom from.complete
		}

	implicit def canSubselect[F <: RowProduct { type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I },
	                          B <: NonEmptyRow, E, V, O <: RowProduct]
	                         (implicit selectClauseType :E <:< SelectableSQL[O, Grouped, V],
	                          fromClauseType :F <:< O {
		                                   type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I
	                                   }
	                         )
			:CanSelectDirect[F, E, SubselectSQL[B, V]] =
		new CanSelectDef[F, E, SubselectSQL[B, V]] {
			override def apply(from :F, expr :E) = expr subselectFrom from
		}

	implicit def canParamSelectColumn[F <: TopRow { type Complete = C; type Params = P },
	                                  C <: TopRow { type Complete = C; type Params = P }, P, E, V, O >: C <: RowProduct]
	                                 (implicit selectClauseType :E <:< ColumnSQL[O, Grouped, V],
	                                  fromClauseType :F <:< TopRow { type Complete = C; type Params = P })
			:CanSelectDirect[F, E, Select[P, V]] =
		new CanSelectDef[F, E, Select[P, V]] {
			override def apply(from :F, expr :E) = expr paramSelectFrom from.complete
		}

	implicit def canParamSelectMapping[F <: TopRow { type Complete = C; type Params = P },
	                                   C <: TopRow { type Complete = C; type Params = P },
	                                   P, M[A] <: MappingAt[A], E, O >: C <: RowProduct]
	                                  (implicit selectClauseType :E <:< SelectableMappingSQL[O, M],
	                                   fromClauseType :F <:< TopRow { type Complete = C; type Params = P })
			:CanSelectDirect[F, E, SelectMapping[P, M]] =
		new CanSelectDef[F, E, SelectMapping[P, M]] {
			override def apply(from :F, expr :E) = expr paramSelectFrom from.complete
		}

}






/** Base trait for included [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]] subtypes,
  * which are used as ''select'' clauses by [[net.noresttherein.oldsql.sql.Select.SelectMapping SelectMapping]],
  * [[net.noresttherein.oldsql.sql.ast.SelectAs SelectAs]] and its subclasses.
  * Overriden `topSelectFrom`, `subselectFrom`, `paramSelectFrom` methods throw an [[UnsupportedOperationException]]
  * and must be implemented by subclasses. They simply narrow down the returned types for declarations
  * in interface classes. As [[net.noresttherein.oldsql.sql.ast.SelectableSQL SelectableSQL]],
  * its companion object provides definitions of [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]]
  * type classes for this type, allowing its subtypes to be used as arguments to
  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] factory extension methods
  * of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
  */
trait SelectableMappingSQL[-F <: RowProduct, M[A] <: MappingAt[A]]
	extends SelectableSQL[F, Grouped, M[Unit]#Subject]
	   with SelectableSQLTemplate[F, M[Unit]#Subject, CanSelect.as[M]#from]
{
//	@deprecated(ImplementationWarning + " Use directly SelectAs instead.", InitialVersion)
//	type SelectAsResult[-E <: RowProduct] = SelectAs[E, M]
//	@deprecated(ImplementationWarning + " Use directly SubselectAs instead.", InitialVersion)
//	type SubselectAsResult[-E <: RowProduct] = SubselectAs[E, M]
//	@deprecated(ImplementationWarning + " Use directly SelectMapping instead.", InitialVersion)
//	type SelectMappingResult[P] = SelectMapping[P, M]


//	override def selectFrom(from :F) :SelectAsResult[from.Base] = super.selectFrom(from) //overrdiden for the type alias in the return type

	override def topSelectFrom[E <: F with GroundRow { type Complete  <: E }](from :E) :TopSelectAs[M] =
		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")

//	override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectAs[B, M] =
//		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")
	override def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :SubselectAs[B, M] =
		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")


	override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
			:SelectMapping[P, M] =
		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")
}



object SelectableMappingSQL {

	implicit def canSelect[F <: RowProduct, B <: RowProduct, M[A] <: MappingAt[A], E, O <: RowProduct]
	                      (implicit selectClauseType :E <:< SelectableMappingSQL[O, M],
	                       fromClauseType :F <:< O { type Base = B; type DefineBase[+I <: RowProduct] = I })
			:CanSelectDirect[F, E, SelectAs[B, M]] =
		new CanSelectDef[F, E, SelectAs[B, M]] {
			override def apply(from :F, expr :E) = expr selectFrom from
		}

	implicit def canTopSelect[F <: GroundRow { type Complete = C }, C <: GroundRow { type Complete = C },
	                          M[A] <: MappingAt[A], E, O >: C <: RowProduct]
	                         (implicit selectClauseType :E <:< SelectableMappingSQL[O, M],
	                          fromClauseType :F <:< GroundRow { type Complete = C })
			:CanSelectDirect[F, E, TopSelectAs[M]] =
		new CanSelectDef[F, E, TopSelectAs[M]] {
			override def apply(from :F, expr :E) = expr topSelectFrom from.complete
		}

	implicit def canSubselect[F <: RowProduct { type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I },
	                          B <: NonEmptyRow, M[A] <: MappingAt[A], E, V, O <: RowProduct]
	                         (implicit selectClauseType :E <:< SelectableMappingSQL[O, M],
	                          fromClauseType :F <:< O {
		                                   type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I
	                                   }
	                         )
			:CanSelectDirect[F, E, SubselectAs[B, M]] =
		new CanSelectDef[F, E, SubselectAs[B, M]] {
			override def apply(from :F, expr :E) = expr subselectFrom from
		}

	implicit def canParamSelect[F <: TopRow { type Complete = C; type Params = P },
	                            C <: TopRow { type Complete = C; type Params = P },
	                            P, M[A] <: BaseColumn[V, A], E, V, O >: C <: RowProduct]
	                           (implicit selectClauseType :E <:< SelectableColumnMappingSQL[O, M, V],
	                            fromClauseType :F <:< TopRow { type Complete = C; type Params = P })
			:CanSelectDirect[F, E, SelectMapping[P, M]] =
		new CanSelectDef[F, E, SelectMapping[P, M]] {
			override def apply(from :F, expr :E) = expr paramSelectFrom from.complete
		}
}






/** Base trait for included [[net.noresttherein.oldsql.sql.ast.ColumnMappingSQL ColumnMappingSQL]] column expressions,
  * which are used as ''select'' clauses by [[net.noresttherein.oldsql.sql.Select.SelectMapping SelectMapping]],
  * [[net.noresttherein.oldsql.sql.ast.SelectColumnAs SelectColumnAs]] and its subclasses.
  * Overriden `topSelectFrom`, `subselectFrom`, `paramSelectFrom` methods throw an [[UnsupportedOperationException]]
  * and must be implemented by subclasses. They simply narrow down the returned types for declarations
  * in interface classes. As [[net.noresttherein.oldsql.sql.ast.SelectableSQL SelectableSQL]],
  * its companion object provides definitions of [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]]
  * type classes for this type, allowing its subtypes to be used as arguments to
  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]] factory extension methods
  * of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
  */
trait SelectableColumnMappingSQL[-F <: RowProduct, M[A] <: BaseColumn[V, A], V]
	extends ColumnSQL[F, Grouped, M[Unit]#Subject]
	   with SelectableMappingSQL[F, M] with SelectableSQLTemplate[F, V, CanSelect.column[V]#as[M]#from]
{
//	@deprecated(ImplementationWarning + " Use directly SelectColumnAs instead.", InitialVersion)
//	type SelectColumnAsResult[-E <: RowProduct] = SelectColumnAs[E, M, V]
//	@deprecated(ImplementationWarning + " Use directly SubselectColumnAs instead.", InitialVersion)
//	type SubselectColumnAsResult[-E <: RowProduct] = SubselectColumnAs[E, M, V]

	//overriden because typically `SelectableSQLTemplate` is mixed in before `ColumnSQL` in a `SelectableMappingSQL`
	override def selectFrom(from :F) :SelectColumnAs[from.Base, M, V] =
		super[SelectableSQLTemplate].selectFrom(from) //overriden for the type alias in the return type

	override def topSelectFrom[E <: F with GroundRow { type Complete  <: E }](from :E) :TopSelectColumnAs[M, V] =
		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")

	override def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :SubselectColumnAs[B, M, V] =
		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")

//	override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectColumnAs[B, M, V] =
//		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")
//
	override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
			:SelectMapping[P, M] =
		throw new UnsupportedOperationException("Expression " + this + " cannot be used as a select clause.")
}



object SelectableColumnMappingSQL {

	implicit def canSelect[F <: RowProduct, B <: RowProduct, M[A] <: BaseColumn[V, A], E, V, O <: RowProduct]
	                      (implicit selectClauseType :E <:< SelectableColumnMappingSQL[O, M, V],
	                       fromClauseType :F <:< O { type Base = B; type DefineBase[+I <: RowProduct] = I })
			:CanSelectDirect[F, E, SelectColumnAs[B, M, V]] =
		new CanSelectDef[F, E, SelectColumnAs[B, M, V]] {
			override def apply(from :F, expr :E) = expr selectFrom from
		}

	implicit def canTopSelect[F <: GroundRow { type Complete = C }, C <: GroundRow { type Complete = C },
	                          M[A] <: BaseColumn[V, A], E, V, O >: C <: RowProduct]
	                         (implicit selectClauseType :E <:< SelectableColumnMappingSQL[O, M, V],
	                          fromClauseType :F <:< GroundRow { type Complete = C })
			:CanSelectDirect[F, E, TopSelectColumnAs[M, V]] =
		new CanSelectDef[F, E, TopSelectColumnAs[M, V]] {
			override def apply(from :F, expr :E) = expr topSelectFrom from.complete
		}

	implicit def canSubselect[F <: RowProduct { type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I },
	                          B <: NonEmptyRow, M[A] <: BaseColumn[V, A], E, V, O <: RowProduct]
	                         (implicit selectClauseType :E <:< SelectableColumnMappingSQL[O, M, V],
	                          fromClauseType :F <:< O {
		                                   type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I
	                                   }
	                         )
			:CanSelectDirect[F, E, SubselectColumnAs[B, M, V]] =
		new CanSelectDef[F, E, SubselectColumnAs[B, M, V]] {
			override def apply(from :F, expr :E) = expr subselectFrom from
		}

}


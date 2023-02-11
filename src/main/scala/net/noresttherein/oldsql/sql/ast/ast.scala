package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.schema.ColumnMappingExtract
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.MappingOf
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{Single, Grouped}
import net.noresttherein.oldsql.sql.mechanics.RelationCount






//todo: All/Any expressions: 1 = All (select ...)
//todo: common functions
//todo: CaseSQL
package object ast {
//	type SelectAs[F <: RowProduct, M[O] <: MappingAt[O]] = TypedSelectSQL[F, M, M[Unit]#Subject]
//	object SelectAs {
//		type TopSelectAs[M[O] <: MappingAt[O]] = TypedTopSelectSQL[M, M[Unit]#Subject]
//		type SubselectAs[F <: RowProduct, M[O] <: MappingAt[O]] = TypedSubselectSQL[F, M, M[Unit]#Subject]
//	}
//
//	type SelectColumnAs[F <: RowProduct, M[O] <: TypedColumn[V, O], V] = TypedSelectColumn[F, M, V]
//	object SelectColumnAs {
//		type TopSelectColumnAs[M[O] <: TypedColumn[V, O], V] = TypedTopSelectColumn[M, V]
//		type SubselectColumnAs[F <: RowProduct, M[O] <: TypedColumn[V, O], V] = TypedSubselectColumn[F, M, V]
//	}

	//todo: SQLExpressionFactory - an abstract factory to allow plugging in implementations
	type ColumnComponentSQL[-F <: RowProduct, S] = GenericColumnComponentSQL[F, MappingOf[S]#TypedColumnProjection, S]

	object ColumnComponentSQL {

		def apply[F <: RowProduct, S]
		         (from :F, column :TypedColumn[S, F])(implicit offset :RelationCount.In[F])
				:ColumnComponentSQL[F, S] =
			GenericColumnComponentSQL[F, TypedColumn[S, F], S](from, column)

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, S, L <: RowProduct Adjoin T]
		         (from :RelationSQL[F, T, R, L], column :TypedColumn[S, F]) :ColumnComponentSQL[F, S] =
			GenericColumnComponentSQL(from, column)

		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[O, T, R, L], ColumnMappingExtract[R, X, O]) forSome {
					type T[O] <: BaseMapping[R, O]; type R; type L <: RowProduct; type O >: F <: RowProduct
				}] =
			GenericColumnComponentSQL.unapply(e)

		type __ = ColumnComponentSQL[_ <: RowProduct, _]
	}



	def denullify[F <: RowProduct, S >: Grouped <: Single, V](e :SQLExpression[F, S, V]) :SQLExpression[F, S, V] =
		if (e == null) MultiNull[V](1) else e

	def denullify[F <: RowProduct, S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) :ColumnSQL[F, S, V] =
		if (e == null) SQLNull[V]() else e


}




package ast {

	import net.noresttherein.oldsql.exceptions.{MisalignedExpressionException, UndefinedShapeException}

	/** @define Thing expression
	  * @define thing expression
	  */
	private[sql] trait HasRowShape {
		/** The column types of all columns in this $Thing.
		  * It is dependent on the spelling strategy and consistent with spelling.
		  * [[net.noresttherein.oldsql.sql.Query Query]]`.`[[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]].
		  */
		@throws[MisalignedExpressionException]("if the expression is internally inconsistent and must be reformed " +
		                                       "to attain a unified shape.")
		@throws[UndefinedShapeException]("if this expression or its subexpression has no definite shape " +
		                                 "(for example, if it includes a LooseComponent).")
		protected def shape(implicit spelling :SQLSpelling) :RowShape

		private[sql] def `->shape`(spelling :SQLSpelling) :RowShape = shape(spelling)

		/** Number of columns returned by the query, consistent with the formatting by
		  * [[net.noresttherein.oldsql.sql.Query Query]]`.`[[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]]/[[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]].
		  */
		@throws[MisalignedExpressionException]("if the expression is internally inconsistent and must be reformed " +
		                                       "to attain a unified shape.")
		@throws[UndefinedShapeException]("if this expression or its subexpression has no definite shape " +
		                                 "(for example, if it includes a LooseComponent).")
		protected def columnCount(implicit spelling :SQLSpelling) :Int

		private[sql] def `->columnCount`(spelling :SQLSpelling) :Int = columnCount(spelling)
	}


	private[sql] trait RowShapeCache extends HasRowShape {
		@volatile @transient private var _shape               :RowShape    = _
		@volatile @transient private var _shapeSpelling       :SQLSpelling = _
		@volatile @transient private var _columnCount         :Int         = _
		@volatile @transient private var _columnCountSpelling :SQLSpelling = _

		protected abstract override def shape(implicit spelling :SQLSpelling) :RowShape =
			if (_shapeSpelling == spelling)
				_shape
			else {
				val res = super.shape
				if (_shapeSpelling == null) synchronized {
					if (_shapeSpelling == null) {
						_shape = res
						_shapeSpelling = spelling
					}
				}
				res
			}

		protected abstract override def columnCount(implicit spelling :SQLSpelling) :Int =
			if (_columnCountSpelling == spelling)
				_columnCount
			else if (_shapeSpelling == spelling)
				_shape.size
			else {
				val res = super.columnCount
				if (_columnCountSpelling == null) synchronized {
					if (_columnCountSpelling == null) {
						_columnCount = res
						_columnCountSpelling = spelling
					}
				}
				res
			}
	}

}
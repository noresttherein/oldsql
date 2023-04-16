package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.schema.ColumnMappingExtract
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.MappingOf
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnConvertingOps, ColumnConvertingTemplate, ConvertibleColumn}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, ConvertingOps, ConvertingTemplate, Grouped, Single}
import net.noresttherein.oldsql.exceptions.{MisalignedExpressionException, UndefinedShapeException}
import net.noresttherein.oldsql.sql.mechanics.{AlignableColumns, RelationCount}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions


//todo: All/Any expressions: 1 = All (select ...)
//todo: common functions
//todo: CaseSQL
//todo: a custom composite class ObjectSQL(TupleSQL[_, _, T], T => V, V => T) extends SQL[_, _, V]
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

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, S, L <: RowProduct]
		         (from :RelationSQL[F, T, R, L], column :TypedColumn[S, F]) :ColumnComponentSQL[F, S] =
			GenericColumnComponentSQL(from, column)

		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[O, T, R, L], ColumnMappingExtract[R, X, O]) forSome {
					type T[O] <: BaseMapping[R, O]; type R; type L <: RowProduct; type O >: F <: RowProduct
				}] =
			GenericColumnComponentSQL.unapply(e)

		type __ = ColumnComponentSQL[_ <: RowProduct, _]
	}



	@inline def denullify[F <: RowProduct, S >: Grouped <: Single, V](e :SQLExpression[F, S, V]) :SQLExpression[F, S, V] =
		if (e == null) MultiNull[V](1) else e

	@inline def denullify[F <: RowProduct, S >: Grouped <: Single, V, EC[v] <: ConvertibleSQL[F, S, v, EC]]
	             (e :ConvertingOps[F, S, V, EC]) :SQLExpression[F, S, V] =
		if (e == null) MultiNull[V](1) else e.toConvertibleSQL

	@inline def denullify[F <: RowProduct, S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) :ColumnSQL[F, S, V] =
		if (e == null) SQLNull[V]() else e

	@inline def denullify[F <: RowProduct, S >: Grouped <: Single, V, EC[v] <: ConvertibleColumn[F, S, v, EC]]
	             (e :ColumnConvertingOps[F, S, V, EC]) :ColumnSQL[F, S, V] =
		if (e == null) SQLNull[V]() else e.toConvertibleSQL


}




package ast {

	/** @define This expression
	  * @define this expression
	  */
	private[sql] trait HasRowShape {
		/** The column types of all columns in this $This.
		  * It is dependent on the spelling strategy and consistent with spelling.
		  * [[net.noresttherein.oldsql.sql.Query Query]]`.`[[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]].
		  */
		@throws[MisalignedExpressionException]("if the expression is internally inconsistent and must be reformed " +
		                                       "to attain a unified shape.")
		@throws[UndefinedShapeException]("if this expression or its subexpression has no definite shape " +
		                                 "(for example, if it includes a LooseComponent).")
		protected def shape(implicit spelling :SQLSpelling) :RowShape

		private[sql] def `->shape`(spelling :SQLSpelling) :RowShape = shape(spelling)

		/** Number of columns returned by the $this, consistent with the formatting by
		  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]]/[[net.noresttherein.oldsql.sql.Query Query]]`.`[[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]].
		  */
		@throws[MisalignedExpressionException]("if the expression is internally inconsistent and must be reformed " +
		                                       "to attain a unified shape.")
		@throws[UndefinedShapeException]("if this expression or its subexpression has no definite shape " +
		                                 "(for example, if it includes a LooseComponent).")
		protected def columnCount(implicit spelling :SQLSpelling) :Int

		private[sql] def `->columnCount`(spelling :SQLSpelling) :Int = columnCount(spelling)

		/** Lists all potential columns of this $this (including those which are excluded by default)
		  * as [[net.noresttherein.oldsql.sql.mechanics.AlignableColumn AlignableColumn]] instances,
		  * together with [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions restrictions]]
		  * on what kind of modifications [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingOps.reform reforming]]
		  * process can apply to them. Unlike [[net.noresttherein.oldsql.sql.SQLExpression.split split]],
		  * returned columns do not need to be actual columns of this expression, and may be even grounded
		  * in another [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]; they are not used directly
		  * for expression transformation or the spelling purposes, but in an attempt to simultaneously unify
		  * multiple expressions (''select'' clauses within
		  * a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]).
		  *
		  * Not all, or even any, actual columns of the expression in its current form need to actually be included:
		  * the purpose of `AlignableColumn` is being compared with instances returned by other expressions,
		  * in order to align like with like, and determine the final
		  * [[net.noresttherein.oldsql.sql.ast.HasRowShape.shape shape]] of the whole query. If columns of an expression
		  * cannot be compared in any meaningful way with individual columns of other expressions, returning a non empty
		  * collection here would result in them being included
		  * in the [[net.noresttherein.oldsql.sql.mechanics.Alignment Alignment]] not aligned with any other columns,
		  * which clearly defeats the purpose of the process. Those expressions return
		  * [[net.noresttherein.oldsql.sql.mechanics.AlignableColumns AlignableColumns]]`.`[[net.noresttherein.oldsql.sql.mechanics.AlignableColumns.none none]]
		  * here, and do not attempt later to [[net.noresttherein.oldsql.SQLExpression.ReorderingTemplate.realign realign]]
		  * themselves by column reordering. Instead, they can accept the shape of aligned expressions as a whole.
		  * For other expressions, in particular [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]],
		  * the following relationship holds:
		  * {{{
		  *     this.split = this.potentialColumns(MayReform).columns.filter(_.isDefault).map(_.column)
		  * }}}
		  * In other words, while they may be interspersed with columns excluded by default (i.e., missing from `split`),
		  * default columns in the result must appear in the order consistent with
		  * the [[net.noresttherein.oldsql.sql.ast.HasRowShape.shape shape]] of this expression.
		  *
		  * This method should not throw an exception if the expression is inseparable (like
		  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]], for example), but instead return columns of a matching
		  * value type and specific class, so they can be matched with potential columns of other expressions using
		  * [[net.noresttherein.oldsql.sql.mechanics.AlignableColumn AlignableColumn]]`.`[[net.noresttherein.oldsql.sql.mechanics.AlignableColumn.corresponds corresponds]].
		  */
		protected def potentialColumns(permissions :Permissions)(implicit spelling :SQLSpelling) :AlignableColumns

		private[sql] def `->potentialColumns`(permissions :Permissions)(spelling :SQLSpelling) :AlignableColumns =
			potentialColumns(permissions)(spelling)

		protected def potentialColumnsCount(implicit spelling :SQLSpelling) :Int

		private[sql] def `->potentialColumnsCount`(implicit spelling :SQLSpelling) :Int = potentialColumnsCount
	}


	private[sql] trait RowShapeCache extends HasRowShape {
		@volatile @transient private var _shape                         :RowShape    = _
		@volatile @transient private var _shapeSpelling                 :SQLSpelling = _
		@volatile @transient private var _columnCount                   :Int         = _
		@volatile @transient private var _columnCountSpelling           :SQLSpelling = _
		@volatile @transient private var _potentialColumns              :AlignableColumns = _
		@volatile @transient private var _potentialColumnsSpelling      :SQLSpelling = _
		@volatile @transient private var _potentialColumnsCount         :Int = _
		@volatile @transient private var _potentialColumnsCountSpelling :SQLSpelling = _

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

		protected abstract override def potentialColumns(permissions :Permissions)
		                                                (implicit spelling :SQLSpelling) :AlignableColumns =
			if (_potentialColumnsSpelling == spelling)
				_potentialColumns
			else {
				val res = super.potentialColumns(permissions)
				if (_potentialColumnsSpelling == null) synchronized {
					if (_potentialColumnsSpelling == null) {
						_potentialColumns = res
						_potentialColumnsSpelling = spelling
					}
				}
				res
			}

		protected abstract override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
			if (_potentialColumnsCountSpelling == spelling)
				_potentialColumnsCount
			else if (_potentialColumnsSpelling == spelling)
				_potentialColumns.columns.length
			else {
				val res = super.potentialColumnsCount
				if (_potentialColumnsSpelling == null) synchronized {
					if (_potentialColumnsCountSpelling == null) {
						_potentialColumnsCount = res
						_potentialColumnsCountSpelling = spelling
					}
				}
				res
			}
	}

}
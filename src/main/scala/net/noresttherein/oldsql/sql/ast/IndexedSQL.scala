package net.noresttherein.oldsql.sql.ast

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.{Chain, Listing, Opt, Unique}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{Bug, IllegalExpressionException, MismatchedExpressionsException}
import net.noresttherein.oldsql.morsels.generic
import net.noresttherein.oldsql.morsels.generic.=>:
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.bits.{IndexedMapping, LabelPath}
import net.noresttherein.oldsql.schema.bits.LabelPath.{~/, Label, LabelPathPrefix}
import net.noresttherein.oldsql.slang.{cast2TypeParams, castTypeParam, classNameMethods, mappingMethods, saferCasting, SeqExtension}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, Select, SQLExpression, TypedListingColumnSQLMapping, TypedListingSQLMapping}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ColumnConvertingTemplate, SpecificColumnVisitor, VariantColumnGroundingTemplate}
import net.noresttherein.oldsql.sql.RowProduct.{GroundRow, NonEmptyRow, SubselectOf, TopRow}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, Grouped, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumnTemplate
import net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryCompositeTemplate
import net.noresttherein.oldsql.sql.ast.DecoratedColumnSQL.FloatingDecoratedColumnTemplate
import net.noresttherein.oldsql.sql.ast.IndexedSQL.{splitIndexTransformation, IndexItem, LabeledColumnSQL, LabeledValueSQL, MatchSpecificIndexed}
import net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledColumnSQL.{AnyLabeledColumnVisitor, SpecificLabeledColumnVisitor}
import net.noresttherein.oldsql.sql.ast.RecordSQL.{UnsealedRecordSQL, ProjectRecord, RecordField, RecordItem, RecordPath, RecordSQLTemplate, ShuffleRecord}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.mechanics.{sql_=>, AlignableColumn, AlignableColumns, Reform, SQLConversion, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.MayExclude
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.{RecordConversion, Upcast}






//does not extend ChainTuple because NonEmptyIndexedSQL doesn't extend ChainEntry and it would cause problems
// in pattern matching a ChainTuple
/** A variant of [[net.noresttherein.oldsql.sql.ast.ChainTuple ChainTuple]] which maps
  * to a [[net.noresttherein.oldsql.collection.Listing Listing]] - a `Chain` subtype with key-value pairs
  * in the form of `K :~ V` as its only elements.
  * The indexing has no special effect on the generated SQL other than
  * [[net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledColumnSQL LabeledColumnSQL]] being a subclass of
  * [[net.noresttherein.oldsql.sql.ast.AliasedSQL AliasedColumn]]. Instead, it allows referencing
  * the columns in a type safe way when mapping results of a select, if this expression appears
  * in the ''select'' clause. Additionally, a ''select'' clause consisting solely of this type can be matched
  * to any [[net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping IndexedSchemaMapping]] with a component chain
  * containing exactly the same key-value pairs as the `T` type parameter, regardless of their order.
  * Non-column components of the mapping must in that case recursively match a nested indexed tuple paired
  * with the same key type in this `LiteralChain` as the component's label.
  * An `IndexedSQL` can consist only of expressions implementing `LabeledValueSQL` - a sealed type
  * extended only by [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]
  * (including [[net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledColumnSQL LabeledColumnSQL]]) and this trait.
  * This ensures that every column of the whole expression is assigned a unique key,
  * which however may be a sequence of `Label`s, rather than a single one, if this expression contains
  * another indexed tuples as its subexpressions.
  * Note that, in order to provide unambiguous cases when pattern matching, this type does not extend the standard
  * `ChainTuple`.
  * @define This `IndexedSQL`
  * @define this labeled record
  */
sealed trait IndexedSQL[-F <: RowProduct, -S >: Grouped <: Single, V <: Listing]
	extends UnsealedRecordSQL[F, S, V] //a package private trait allowing to extend sealed RecordSQL
	   with LabeledValueSQL[F, S, V]
	   with SelectableMappingSQL[F, IndexedMapping.of[V]#Mapping]
//	   with ReorderingTemplate[F, S, V, IndexedSQL[F, S, V]]
	   with RecordSQLTemplate[F, S, V, LabeledValueSQL, IndexedSQL]
{
	override type LabeledItem[-F1 <: RowProduct, -S1 >: Grouped <: Single, K <: Label, X] <: IndexItem[F1, S1, K, X]

//	override def items :Seq[LabeledItem[F, S, _ <: Label, _]]

	override def listingMapping[O](columnPrefix :String) :TypedListingSQLMapping[F, S, V, O] =
		TypedListingSQLMapping(this, columnPrefix)

	//override clashes between RecordSQL and LabeledValueSQL
//	override def init[I <: Listing](implicit nonEmpty: V <:< (I |~ Listing.Item)) :IndexedSQL[F, S, I] =
//		this.castFrom[IndexedSQL[F, S, V], NonEmptyIndexedSQL[F, S, I, Label, Any]].init

//	override def last[L](implicit nonEmpty: V <:< (Listing |~ (Label :~ L))) :LabeledValueSQL[F, S, L] =
//		this.castFrom[IndexedSQL[F, S, V], NonEmptyIndexedSQL[F, S, Listing, Label, L]].last


	//If we somehow managed to ensure LabeledColumnSQL cannot be made for a Listing subtype,
	// then we could narrow down the return type to either IndexedSQL or LabeledColumnSQL (or LabeledValueSQL if unknown)
//	def last[L](implicit nonEmpty: V <:< (Listing |~ (Label :~ L))) :LabeledValueSQL[F, S, L] =
//		this.castFrom[LabeledValueSQL[F, S, V], NonEmptyIndexedSQL[F, S, Listing, Label, L]].right

//	override def lastKey[K <: Label](implicit nonEmpty: V <:< (Listing |~ (K :~ Any))) :K =
//		this.castFrom[IndexedSQL[F, S, V], NonEmptyIndexedSQL[F, S, Listing, K, Any]].key

	//override clash
//	override def lastItem[K <: Label, L](implicit notEmpty :V <:< (Listing |~ (K :~ L))) :LabeledItem[F, S, K, L] =
//		this.castFrom[IndexedSQL[F, S, V], NonEmptyIndexedSQL[F, S, Listing, K, L]].item

	//technically not necessary to compile, but otherwise overloading can't pick between LabeledValueSQL.apply and RecordSQL.apply
	override def apply[K <: Label](key :K)(implicit get :RecordField[V, K]) :LabeledValueSQL[F, S, get.Value] = get(this)
//	def apply[K <: Label](key :K)(implicit get :RecordPath[V, K])    :LabeledValueSQL[F, S, get.Value] = get(this)
	def apply[P](path :LabelPath[P])(implicit get :RecordPath[V, P]) :LabeledValueSQL[F, S, get.Value] = get(this)


	def |~[E <: F, O >: Grouped <: S, K <: Label, L]
	      (entry :(K, LabeledValueSQL[E, O, L])) :IndexedSQL[E, O, V |~ (K :~ L)] =
		(this |~ :~[K](entry._2))(new ValueOf(entry._1))

	def |~[E <: F, O >: Grouped <: S, K <: Label :ValueOf, L]
	      (entry :K :~ LabeledValueSQL[E, O, L]) :IndexedSQL[E, O, V |~ (K :~ L)] =
		entry.value match {
			case column :LabeledColumnSQL[E, O, _, L] if column.alias == valueOf[K] =>
				new NonEmptyIndexedSQL(this, column)
			case column :LabeledColumnSQL[E, O, _, L] =>
//				super.|~(:~[K](column)) //throws an exception
				throw new IllegalArgumentException(
					"Cannot add a LabeledColumnSQL `" + column + "` to record `" + this +
						"` under a new label '" + valueOf[K] + "'."
				)
			case value =>
				new NonEmptyIndexedSQL(this, value)
		}

	def |~[E <: F, O >: Grouped <: S, K <: Label :ValueOf, L]
	      (entry :K :~ ColumnSQL[E, O, L]) :IndexedSQL[E, O, V |~ (K :~ L)] =
		entry.value match {
			case column :LabeledColumnSQL[E, O, _, L] if column.alias == valueOf[K] =>
				new NonEmptyIndexedSQL(this, column)
			case column :LabeledColumnSQL[E, O, _, L] =>
//				super.|~(:~[K](column)) //throws an exception
				throw new IllegalArgumentException(
					"Cannot add a LabeledColumnSQL `" + column + "` to record `" + this +
						"` under a new label '" + valueOf[K] + "'."
				)
			case column =>
				new NonEmptyIndexedSQL(this, valueOf[K] @: column)
		}

	def |~[E <: F, O >: Grouped <: S, K <: Label, L]
	      (entry :IndexItem[E, O, K, L]) :IndexedSQL[E, O, V |~ (K :~ L)] =
		(this |~ :~[K](entry.value))(new ValueOf(entry.key))


	override def |~[E <: F, O >: Grouped <: S, K <: Label, L]
	               (entry :RecordItem[E, O, K, L]) :RecordSQL[E, O, V |~ (K :~ L)] =
		entry.value match {
			case labeled :LabeledValueSQL[E, O, L] => (this |~ :~[K](labeled))(new ValueOf(entry.key))
			case column :ColumnSQL[E, O, L] => (this |~ :~[K](column))(new ValueOf(entry.key))
			case _ => super.|~(entry)
		}

	override def |~[E <: F, O >: Grouped <: S, K <: Label, L]
	               (entry :LabeledColumnSQL[E, O, K, L]) :IndexedSQL[E, O, V |~ (K :~ L)] =
		new NonEmptyIndexedSQL(this, entry.alias, entry)

	override def |~[E <: F, O >: Grouped <: S, K <: Label :ValueOf, L]
	               (entry :K :~ SQLExpression[E, O, L]) :RecordSQL[E, O, V |~ (K :~ L)] =
		entry.value match {
			case value :LabeledValueSQL[E, O, L] => this |~ :~[K](value)
			case column :ColumnSQL[E, O, L] => this |~ :~[K](column)
			case _ => super.|~(entry)
		}



	/** A list of paths to all, direct and indirect record columns contained in this expression, in their order
	  * of appearance from left to right. The result is the key set of
	  * `this.`[[net.noresttherein.oldsql.sql.ast.IndexedSQL.columns columns]].
	  */
	def paths :Seq[LabelPath[_]]
	/*{
		def rec(path :LabelPathPrefix, e :IndexedSQL[F, S, _], acc :List[LabelPath[_]]) :List[LabelPath[_]] =
			e match {
				case entry :NonEmptyIndexedSQL[F, S, _, _, _] =>
					def dfs(path :LabelPath[_], e :LabeledValueSQL[F, S, _], res :List[LabelPath[_]]) :List[LabelPath[_]] =
						e match {
							case record :IndexedSQL[F, S, _] => rec(path, record, res)
							case _ => path::res
						}
					val down = path / entry.lastKey
					rec(path, entry.init, dfs(down, entry.last, acc))
				case _ => acc
			}
		rec(~/, this, Nil)
	}*/

	/** Map of all full paths in this expression, that is paths starting with a key in this record
	  * and ending with a direct or indirect record column. The key set of the result equals
	  * `this.`[[net.noresttherein.oldsql.sql.ast.IndexedSQL.paths paths]]`.toSet`.
	  * @see [[net.noresttherein.oldsql.sql.ast.IndexedSQL.paths paths]]
	  * @see [[net.noresttherein.oldsql.sql.ast.IndexedSQL.index pathValues]]
	  * @see [[net.noresttherein.oldsql.sql.ast.IndexedSQL.toMap toMap]]
	  */
	def columns :Map[LabelPath[_], LabeledColumnSQL[F, S, _ <: Label, _]]
/*
	{
		type Res = Map[LabelPath[_], ColumnSQL[F, S, _]]
		def rec(path :LabelPathPrefix, e :IndexedSQL[F, S, _ <: Listing], acc :Res) :Res =
			e match {
				case entry :NonEmptyIndexedSQL[F, S, _, _, _] =>
					def dfs(path :LabelPath[_], e :LabeledValueSQL[F, S, _], res :Res) :Res =
						e match {
							case tuple :IndexedSQL[F, S, _] => rec(path, tuple, res)
							case column :ColumnSQL[F, S, _] => res.updated(path, column)
						}
					rec(path, entry.init, dfs(path / entry.lastKey, entry.last, acc))
				case _ => acc
			}
		rec(~/, this, SeqMap.empty)
	}
*/

	/** Map of all, direct and indirect record entries under this record, indexed by their paths,
	  * starting with a key from this record.
	  * @see [[net.noresttherein.oldsql.sql.ast.IndexedSQL.columns columns]]
	  */
	def index :Map[LabelPath[_], LabeledValueSQL[F, S, _]]
	/*{
		type Res = Map[LabelPath[_], LabeledValueSQL[F, S, _]]
		def rec(path :LabelPathPrefix, e :IndexedSQL[F, S, _], acc :Res) :Res =
			e match {
				case entry :NonEmptyIndexedSQL[F, S, _, _, _] =>
					def dfs(path :LabelPath[_], e :LabeledValueSQL[F, S, _], res :Res) :Res =
						e match {
							case column :LabeledColumnSQL[F, S, l, _] => res.updated(path, column)
							case tuple :IndexedSQL[F, S, _] => rec(path, tuple, acc.updated(path, tuple))
						}
					rec(path, entry.init, dfs(path / entry.lastKey, entry.last, acc))
				case _ => acc
			}
		rec(~/, this, SeqMap.empty)
	}*/


//
//	//these methods are overridden in NonEmptyIndexedSQL, but we need to also implement them here
//	// because CompositeSQL implements them and we narrow the return type.
//	override def anchor(from :F) :IndexedSQL[F, S, V] = rephrase(SQLScribe.anchor(from))
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :IndexedSQL[E, S, V] =
//		rephrase(SQLScribe.expand(base))
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :IndexedSQL[E, S, V] =
//		rephrase(SQLScribe.expand(base))
//
	//overridden to narrow down the result type and because of a clash between LabeledValueSQL and CompositeSQL
//	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :IndexedSQL[E, S, V]

	override def nullSQL :IndexedSQL[RowProduct, Single, V]

	override def reorder[K <: Chain](implicit order :ShuffleRecord[V, K]) :IndexedSQL[F, S, order.Out] = order(this)
	override def reorder[K <: Chain](keys :K)(implicit order :ShuffleRecord[V, K]) :IndexedSQL[F, S, order.Out] = order(this)

	override def project[K <: Chain](implicit project :ProjectRecord[V, K]) :IndexedSQL[F, S, project.Out] = project(this)
	override def project[K <: Chain](keys :K)(implicit project :ProjectRecord[V, K]) :IndexedSQL[F, S, project.Out] =
		project(this)

	//todo: conjunction and disjunction of two expressions (one each, or ordered and non ordered?)
//	def reform[K <: Listing](implicit reform :RecordReform[V, K]) :IndexedSQL[F, S, reform.Out] = reform(this)
//	def reform[K <: Listing]()

	/** Creates a $This expression with its values recursively reordered to follow the order of their paths
	  * specified by the argument. The argument must equal
	  * `record.`[[net.noresttherein.oldsql.sql.ast.IndexedSQL.paths paths]] for some `record :IndexedSQL[_, _, _]`:
	  * paths for very direct and indirect subrecord must form a consecutive subsequence.
	  * @param paths `this.paths` reordered in a grouped manner: for any path prefix length (in keys),
	  *              all paths with equal prefixes of that length form a consecutive subsequence in the argument.
	  * @return a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]] wrapping the reordered record expression.
	  */
	def reorder(paths :Seq[LabelPath[_]]) :SQLExpression[F, S, V]

	/** Omits from this expression columns whose paths are not listed in `paths`, dropping whole subrecords
	  * if all their columns are omitted. The remaining columns are reordered to follow the order defined by `paths`.
	  * The argument must equal
	  * `record.`[[net.noresttherein.oldsql.sql.ast.IndexedSQL.paths paths]] for some `record :IndexedSQL[_, _, _]`:
	  * paths for very direct and indirect subrecord must form a consecutive subsequence.
	  * @param paths a subset of `this.paths`; for any path prefix length (in keys),
	  *              all paths with equal prefixes of that length must form a consecutive subsequence in the argument.
	  * @return a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]] wrapping the reordered record expression.
	  */
	def project(paths :Seq[LabelPath[_]]) :SQLExpression[F, S, V]

	/** Modifies this record by excluding certain columns, adding new ones and/or changing their order.
	  * @param paths a sequence of paths of the new record. If a given path exists in this expression,
	  *              it is carried over with its value to the new expression, and the second element of the pair
	  *              is ignored. Otherwise, a new (possibly indirect) record element is created of the value equal
	  *              to the second pair element.
	  */
	def reform[E <: F, A >: Grouped <: S](paths :Seq[(LabelPath[_], LabeledValueSQL[E, A, _])]) :SQLExpression[E, A, V]


	override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E)
			:TopSelectAs[IndexedMapping.of[V]#Mapping] =
		SelectSQL[E, V](from, this)

	override def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B]) :SubselectAs[B, IndexedMapping.of[V]#Mapping] =
		SelectSQL.subselect[B, F with SubselectOf[B], V](from, this)
//	override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectAs[B, IndexedMapping.of[V]#Mapping] =
//		SelectSQL.subselect[B, F ProperSubselectOf B, V](from, this)

	override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
			:SelectMapping[P, IndexedMapping.of[V]#Mapping] =
		Select(from)(this)

//		def reorder[I <: Listing](implicit up :I SublistingOf T, down :T SublistingOf I) :IndexedSQL[F, S, I] =
//			up(this)
//
//		def reform[I <: Listing](implicit up :I SublistingOf T) :IndexedSQL[F, S, I] = up(this)

//	protected override def potentialColumns(permissions :Permissions)(implicit spelling :SQLSpelling) :AlignableColumns =
//		AlignableColumns(this, permissions)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] =
		visitor.indexed(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y = visitor.indexed(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F, S, V] <: SQLExpression[F_, S_, V],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, V, E] =
//		visitor.labeled(this)

	override def toString :String = {
		def rec(e :IndexedSQL[_, _, _], res :StringBuilder) :StringBuilder = e match {
			case tuple :NonEmptyIndexedSQL[_, _, _, _, _] =>
				rec(tuple.init, res) ++= " |~ " ++= tuple.lastKey ++= ":~"
				tuple.last match {
					case record :IndexedSQL[_, _, _] => rec(record, res += '(') += ')'
					case column :LabeledColumnSQL[_, _, _, _] => res ++= column.value.toString
				}
			case _ => res ++= "@~"
		}
		rec(this, new StringBuilder).toString
	}

}



object IndexedSQL {
	def apply() :IndexedSQL[RowProduct, Single, @~] = EmptySQL

	def apply[F <: RowProduct, S >: Grouped <: Single, K <: Label, T]
	         (item :LabeledColumnSQL[F, S, K, T]) :IndexedSQL[F, S, @~ |~ (K :~ T)] =
		EmptySQL |~ item

	def apply[F <: RowProduct, S >: Grouped <: Single, K <: Label :ValueOf, T]
	         (item :K :~ ColumnSQL[F, S, T]) :IndexedSQL[F, S, @~ |~ (K :~ T)] =
		EmptySQL |~ item

	def apply[F <: RowProduct, S >: Grouped <: Single, K <: Label, T]
	         (item :IndexItem[F, S, K, T]) :IndexedSQL[F, S, @~ |~ (K :~ T)] =
		IndexedSQL(item.key, item.value)

	def apply[F <: RowProduct, S >: Grouped <: Single, K <: Label, T]
	         (key :K, value :ColumnSQL[F, S, T]) :IndexedSQL[F, S, @~ |~ (K :~ T)] =
		//a minor inconsistency: this will silently drop a preexisting label from a LabeledColumnSQL
		IndexedSQL(value.@:[K](key))

	def apply[F <: RowProduct, S >: Grouped <: Single, K <: Label, T]
	         (key :K, value :LabeledValueSQL[F, S, T]) :IndexedSQL[F, S, @~ |~ (K :~ T)] =
		(EmptySQL |~ :~[K](value))(new ValueOf(key))



	final class IndexItem[-F <: RowProduct, -S >: Grouped <: Single, K <: Label, X]
	                     (override val index :Int, override val key :K, override val value :LabeledValueSQL[F, S, X])
		extends RecordItem[F, S, K, X](index, key, value)



	type __ = IndexedSQL[_ <: RowProduct, _ >: Grouped <: Single, _]

	/** A type alias of [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]] used in cases
	  * when the actual value of the expression does not matter, but only its type and, in particular, structure.
	  * It is closely related to both [[net.noresttherein.oldsql.sql.RowShape RowShape]]
	  * and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], but enforces an additional level of compatibility
	  * coming from the `Listing` type, especially its keys, in order to prevent accidental shape matching
	  * of otherwise incompatible expressions.
	  */
	type LabeledShape[V <: Listing] = IndexedSQL[Nothing, Grouped, V]

//	/** [[net.noresttherein.oldsql.sql.ast.EmptySQL EmptySQL]] as an instance of `IndexedSQL`. */
//	val EmptyIndex :IndexedSQL[RowProduct, Single, @~] = EmptySQL


	/** An SQL expression which may occur as a subexpression of
	  * a [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]], which associates with it -
	  * the value of the SQL expression - a unique [[net.noresttherein.oldsql.schema.bits.LabelPath.Label Label]]
	  * identifying it among the elements of the tuple. It has only two subclasses: `IndexedSQL` itself and
	  * [[net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledColumnSQL ListingColumn]] - a decorator
	  * of any column expression with an associated label used as the leaf in the expression tree of the root
	  * `IndexedSQL`. Thus, every column in such an expression is uniquely identified
	  * by a [[net.noresttherein.oldsql.schema.bits.LabelPath LabelPath]] following the labels of all enclosing
	  * components. In contexts where an SQL expression is adapted
	  * to a [[net.noresttherein.oldsql.schema.Mapping Mapping]], that is in ''group by'' expressions and
	  * in ''select'' clauses, this expression is represented
	  * by a [[net.noresttherein.oldsql.sql.TypedListingSQLMapping TypedListingSQLMapping]] (or just its supertype
	  * [[net.noresttherein.oldsql.sql.ListingSQLMapping ListingSQLMapping]], which allows access to its individual
	  * components representing subexpressions of this expression by the use
	  * of a [[net.noresttherein.oldsql.schema.bits.LabelPath LabelPath]] listing labels of all enclosing
	  * subexpressions on the path to a particular subexpression. This allows safer use of the value
	  * of this expression than relying on hard-coded positioning information in a non indexed tuple expressions.
	  * Additionally, queries returning this expression type can be safely combined in ''compound selects''
	  * trough [[net.noresttherein.oldsql.sql.Select.SelectOperator set operators]] such as
	  * [[net.noresttherein.oldsql.sql.Select.Union union]] or [[net.noresttherein.oldsql.sql.Select.Minus minus]].
	  */
	//I remember I tried to make ColumnSQL extend LabeledValueSQL, but it was for some reason problematic.
	// All I remember now is that ListingColumnSQLMapping requires, as a ColumnMapping, a name, which only
	// LabeledColumnSQL can provide. This however seems like a solvable problem.
	//Todo: we should try disallow using a Listing type as the value type for a LabeledColumnSQL.
	// This will be possible in Scala 3 with match types.
	// We'll then be able to safely cast a LabeledValueSQL to IndexedSQL if V <: Listing
	sealed trait LabeledValueSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
		extends SQLExpression[F, S, V] //consider: a ConvertedLabeledValueSQL would help with reordering
//		   with CompositeGroundingTemplate
		   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = LabeledValueSQL[f, S, V] })#E]
//		   with ReorderingTemplate[F, S, V, LabeledValueSQL[F, S, V]]
	{
		//Todo: add a mapping method to SQLExpression. A serious problem is that MappingSQL also defines mapping,
		// but as a val, of constant Origin. Maybe we should just drop type parameter O in the signature,
		// returning an instance of unknown Origin type, and use withOrigin directly.
		// It's not like it is a widely used functionality.
		def listingMapping[O] :TypedListingSQLMapping[F, S, V, O] = listingMapping[O]("")
		def listingMapping[O](columnPrefix :String) :TypedListingSQLMapping[F, S, V, O]

//		private[ast] def paths   :Seq[LabelPath[_]] = Nil
//		private[ast] def columns :Map[LabelPath[_], ColumnSQL[F, S, _]] = Map.empty
//		private[ast] def index   :Map[LabelPath[_], LabeledValueSQL[F, S, _]] = Map.empty
		//we have an implicit conversion which makes the following available
//		def init[I <: Listing](implicit nonEmpty: V <:< (I |~ Listing.Item)) :IndexedSQL[F, S, I] =
//			this.castFrom[LabeledValueSQL[F, S, V], NonEmptyIndexedSQL[F, S, I, Label, Any]].init
//
//		//If we somehow managed to ensure LabeledColumnSQL cannot be made for a Listing subtype,
//		// then we could narrow down the return type to either IndexedSQL or LabeledColumnSQL (or LabeledValueSQL if unknown)
//		def last[L](implicit nonEmpty: V <:< (Listing |~ (Label :~ L))) :LabeledValueSQL[F, S, L] =
//			this.castFrom[LabeledValueSQL[F, S, V], NonEmptyIndexedSQL[F, S, Listing, Label, L]].last
//
//		def lastKey[K <: Label](implicit nonEmpty: V <:< (Listing |~ (K :~ Any))) :K =
//			this.castFrom[LabeledValueSQL[F, S, V], NonEmptyIndexedSQL[F, S, Listing, K, Any]].key
//
//		//Note that here this method returns only the base IndexItem, while in IndexedSQL it must return this.LabeledItem
//		def lastItem[K <: Label, L](implicit notEmpty :V <:< (Listing |~ (K :~ L))) :IndexItem[F, S, K, L]
//
//		def apply[K <: Label](key :K)(implicit get :RecordPath[V, K])    :LabeledValueSQL[F, S, get.Value] = get(this)
//		def apply[P](path :LabelPath[P])(implicit get :RecordPath[V, P]) :LabeledValueSQL[F, S, get.Value] = get(this)

		override def asSingleRow :Option[LabeledValueSQL[F, Single, V]]

//		def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :LabeledValueSQL[E, S, V]
		/** Similar to
		  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]]`.`[[net.noresttherein.oldsql.sql.ast.CompositeSQL.rephrase rephrase]],
		  * and delegates to the latter in case of an [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]],
		  * but a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] simply applies `mapper` to itself
		  * (which it couldn't do inside `rephrase`, as it would cause an infinite recursion).
		  */
		private[sql] def rephraseAsLabeledValue[E <: RowProduct](mapper :SQLScribe[F, E]) :LabeledValueSQL[E, S, V]

		def nullSQL   :LabeledValueSQL[RowProduct, Single, V] //= MultiNull(selectForm)

		@throws[NullPointerException](
			"if the value type of any of the columns does not accept nulls or they are disallowed by their form.")
		def nullValue :Opt[V] = selectForm.nulls.opt
	}


	object LabeledValueSQL {
		//exists so that we can use |~ on a value in a IndexedSQL.
		implicit def IndexedSQLValue[F <: RowProduct, S >: Grouped <: Single, V <: Listing]
		                            (e :LabeledValueSQL[F, S, V]) :IndexedSQL[F, S, V] =
			e match {
				case record :IndexedSQL[F, S, V] => record
				case column :LabeledColumnSQL[F, S, _, V] =>
					throw new IllegalArgumentException("Cannot convert a LabeledColumnSQL " + column + " to an IndexedSQL.")
			}


		type __ = LabeledValueSQL[_, _, _]
//
//		/** Allows [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] to extend sealed
//		  * [[net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledValueSQL LabeledValueSQL]].
//		  */ //extends ColumnConvertingTemplate so we have access to it as a ColumnSQL through toConvertibleSQL
//		private[sql] trait LabeledColumnValueSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
//			extends LabeledValueSQL[F, S, V] with ColumnConvertingTemplate[F, S, V, ({ type E[v] = ColumnSQL[F, S, v] })#E]
//		{ this :ColumnSQL[F, S, V] => }


		trait SpecificLabeledValueVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
			extends SpecificIndexedVisitor[F, S, V, Y] with SpecificLabeledColumnVisitor[F, S, V, Y]

		trait MatchSpecificLabeledValue[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
			extends SpecificLabeledValueVisitor[F, S, V, Y] //with SpecificColumnVisitor[F, S, V, Y]

		trait CaseSpecificLabeledValue[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
			extends MatchSpecificLabeledValue[F, S, V, Y]
		{
			def labeledValue(e :LabeledValueSQL[F, S, V]) :Y

			override def indexed[X <: Listing](e :IndexedSQL[F, S, X])(implicit isListing :V =:= X) :Y =
				labeledValue(isListing.substituteContra[({ type E[v] = LabeledValueSQL[F, S, v] })#E](e))

			override def labeledColumn[N <: Label](e :LabeledColumnSQL[F, S, N, V]) :Y = labeledValue(e)
//			override def column(e :ColumnSQL[F, S, V]) :Y = labeledValue(e)
		}


		trait AnyLabeledValueVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
			extends AnyIndexedVisitor[F, Y] with AnyLabeledColumnVisitor[F, Y]

		trait MatchAnyLabeledValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
			extends AnyLabeledValueVisitor[F, Y] //with AnyColumnVisitor[F, Y]

		trait CaseAnyLabeledValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
			extends MatchAnyLabeledValue[F, Y]
		{
			def labeledValue[S >: Grouped <: Single, V](e :LabeledValueSQL[F, S, V]) :Y[S, V]

			override def indexed[S >: Grouped <: Single, V <: Listing](e :IndexedSQL[F, S, V]) :Y[S, V] = labeledValue(e)
			override def labeledColumn[S >: Grouped <: Single, N <: Label, V](e :LabeledColumnSQL[F, S, N, V]) :Y[S, V] =
				labeledValue(e)

//			override def column[S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) :Y[S, V] = labeledValue(e)
		}
	}


	/** A bottom expression in a [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]] - a single column expression
	  * with an associated key value `alias`. Converting a `LabeledColumnSQL` always produces another `LabeledColumnSQL`,
	  * wrapping converted underlying column of the former - the result is not
	  * a [[net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL ConvertedColumnSQL]].
	  */
	final class LabeledColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, N <: Label, V] private[IndexedSQL]
	                            (override val value :ColumnSQL[F, S, V], override val alias :N)
		extends AliasedSQL[F, S, V](value, alias)
		        with LabeledValueSQL[F, S, V]
		        with SelectableColumnMappingSQL[F, IndexedMapping.of[V]#Column, V]
		        with UnaryCompositeColumnTemplate[F, S, V, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] =
		                                                                              LabeledColumnSQL[f, s, N, V] })#E]
		        with VariantColumnGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = LabeledColumnSQL[f, S, N, V] })#E]
		        with FloatingDecoratedColumnTemplate[F, S, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, v] =
		                                                                              LabeledColumnSQL[f, s, N, v] })#E]
	{
		if (value.isInstanceOf[LabeledColumnSQL[_, _, _, _]])
			throw new IllegalArgumentException(
				"Cannot wrap a LabeledColumnSQL " + value + " in another labeled column '" + alias + "'."
			)
//
//		override def lastItem[K <: Label, L](implicit notEmpty :V <:< (Listing |~ (K :~ L))) :IndexItem[F, S, K, L] =
//			throw new UnsupportedOperationException(
//				"LabeledColumnSQL.lastItem for `" + this + "`. This call should not have been possible."
//			)

		override def listingMapping[O] :TypedListingColumnSQLMapping[F, S, N, V, O] =
			TypedListingColumnSQLMapping(this, alias)

		override def listingMapping[O](columnPrefix :String) :TypedListingColumnSQLMapping[F, S, N, V, O] =
			TypedListingColumnSQLMapping(this, columnPrefix)

		override def nullSQL   = new LabeledColumnSQL[RowProduct, Single, N, V](SQLNull(value.selectForm), alias)
		//Consider: we might want to differentiate from 'default value' (functional) and true 'null' value
		// (i.e, something that will map to all nulls), because LabeledColumnSQL currently uses form.nulls.nullValue
		// to generate null columns when aligning column sets of two expressions.
		override def nullValue :Opt[V] = value.selectForm.nulls.opt //what if it's NonNull?
//			override def layout :LabeledColumnSQL[Nothing, S, N, V] = new LabeledColumnSQL(value.layout, alias)

		private[sql] override def rephraseAsLabeledValue[E <: RowProduct](mapper :SQLScribe[F, E])
				:LabeledColumnSQL[E, S, _ <: Label, V] =
			mapper(this) match {
				case labeled :LabeledColumnSQL[E, S, _, V] => labeled
				case column => alias @: column
			}

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :LabeledColumnSQL[E, S, N, V] =
			mapper(value) match {
				case same if same eq value =>
					this.castFrom[LabeledColumnSQL[F, S, N, V], LabeledColumnSQL[E, S, N, V]]
				case column => alias @: column //unlike the constructor, will unwrap an LabeledColumnSQL
			}

		//made public because the def in GenericColumnDecoratorTemplate is protected, but public in AdaptedSQL
		override def reapply[E <: RowProduct, A >: Grouped <: Single]
		                    (e :ColumnSQL[E, A, V]) :LabeledColumnSQL[E, A, N, V] =
			decorate(e)

		//This will throw an exception if e is a LabeledColumnSQL with a different label. I don't know what's best.
		protected override def decorate[E <: RowProduct, A >: Grouped <: Single, X]
		                               (e :ColumnSQL[E, A, X]) :LabeledColumnSQL[E, A, N, X] =
			new LabeledColumnSQL[E, A, N, X](e, alias)

		override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E)
				:TopSelectColumnAs[IndexedMapping.of[V]#Column, V] =
			SelectSQL(from, this)

//		override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B)
//				:SubselectColumnAs[B, IndexedMapping.of[V]#Column, V] =
//			SelectSQL.subselect[B, F ProperSubselectOf B, N, V](from, this)
		override def subselectFrom[B <: NonEmptyRow](from :F with SubselectOf[B])
				:SubselectColumnAs[B, IndexedMapping.of[V]#Column, V] =
			SelectSQL.subselect[B, F with SubselectOf[B], N, V](from, this)

		override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
				:SelectMapping[P, IndexedMapping.of[V]#Column] =
			Select(from)[N, V](this)

		protected override def potentialColumns(permissions :Permissions)
		                                       (implicit spelling :SQLSpelling) :AlignableColumns =
			AlignableColumns(AlignableColumn(this), permissions - MayExclude)
//
//		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
//		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//		                             (implicit leftResult  :SQLTransformation[V, U],
//		                                       rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
//				:(leftResult.Expression[F, S, LabeledColumnSQL[F, S, N, U]], rightResult.Expression[F2, S2, EC2[U]]) =
//			if (other eq this)
//				(leftResult(this), rightResult(other))
//			else //A column must reform to a column, so prohibiting reforming of the wrapped value prohibits only form swapping.
//				passReform(other)(reform.prohibitReformLeft, passCount)

		override def visit[R](visitor :SpecificColumnVisitor[F, S, V, R]) :R = visitor.labeledColumn(this)
		override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, V] =
			visitor.labeledColumn(this)
	}



	object LabeledColumnSQL {
		def apply[A <: Label, F <: RowProduct, S >: Grouped <: Single, V]
		         (label :A, column :ColumnSQL[F, S, V]) :LabeledColumnSQL[F, S, A, V] =
			new LabeledColumnSQL(column, label)

		def unapply[F <: RowProduct, S >: Grouped <: Single, V]
		           (e :SQLExpression[F, S, V]) :Opt[(_ <: Label, ColumnSQL[F, S, V])] =
			e match {
				case col :LabeledColumnSQL[F, S, l, V] => Got((col.alias, col.value))
				case _ => Lack
			}


		trait SpecificLabeledColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] {
			def labeledColumn[N <: Label](e :LabeledColumnSQL[F, S, N, V]) :Y
		}
		type MatchSpecificLabeledColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
			SpecificLabeledColumnVisitor[F, S, V, Y]
		type CaseSpecificLabeledColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
			SpecificLabeledColumnVisitor[F, S, V, Y]

		trait AnyLabeledColumnVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
			def labeledColumn[S >: Grouped <: Single, N <: Label, V](e :LabeledColumnSQL[F, S, N, V]) :Y[S, V]
		}
		type MatchAnyLabeledColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyLabeledColumnVisitor[F, Y]
		type CaseAnyLabeledColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]  = AnyLabeledColumnVisitor[F, Y]
	}


	//It's a class because there is an erasure conflict in EmptySQL between ChainTuple.{init,last} and RecordSQL
	private[ast] trait UnsealedEmptyIndexedSQL extends IndexedSQL[RowProduct, Single, @~]




	//attempt to recreate a IndexedSQL by casting down the expressions obtained
	// by reforming init and last
	private[ast] def attempt[F <: RowProduct, S >: Grouped <: Single, I <: Listing, K <: Label, L]
	           (i :SQLExpression[F, S, I], key :K, l :SQLExpression[F, S, L]) :Opt[IndexedSQL[F, S, I |~ (K :~ L)]] =
		i match {
			case listing :IndexedSQL[F, S, I] => l match {
				case value :LabeledValueSQL[F, S, L] =>
					Got((listing |~ :~[K](value))(new ValueOf(key)))
				case value :ColumnSQL[F, S, L] =>
					Got((listing |~ :~[K](value))(new ValueOf(key)))
				case _ => Lack
//					throw new MismatchedExpressionsException(NonEmptyIndexedSQL.this, e,
//						"Reformed last expression " + l + " is neither a ColumnSQL nor a LabeledValueSQL."
//					)
			}
			case _ => Lack
//				throw new MismatchedExpressionsException(NonEmptyIndexedSQL.this, e,
//					"received a non-IndexedSQL expression " + i +
//						": " + i.getClass.getName + " as a partial result."
//				)
		}

	private[ast] type SplitListing[XI <: Listing, K <: Label, XL, YI <: Listing, YL, Z,
	                               R[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, Z]]
	                                 <: SQLExpression[f, s, Z]] =
		(XI sql_=> YI, XL sql_=> YL, SQLTransformation.Into[YI |~ (K :~ YL), Z, R])

	/** Splits a transformation of a non-empty chain into transformations for its `init` and `last`.
	  * If the method returns `Got`, then the last conversion is actually identity.
	  */
	private[ast] def splitIndexTransformation[XI <: Listing, K <: Label, XL, Z]
	                                           (conversion :SQLTransformation[XI |~ (K :~ XL), Z])
			:Opt[SplitListing[XI, K, XL, _ <: Listing, _, Z, conversion.Expression]] =
//			:Opt[(XI sql_=> YI, XL sql_=> YL, SQLTransformation.Returning[YI |~ (K :~ YL), Z, conversion.Expression])
//				forSome { type YI <: Listing; type YL }
//			] =
	{
		type Result[Y] = SQLTransformation.Into[Y, Z, conversion.Expression]
		type Composed[Y] = (SQLConversion[XI |~ (K :~ XL), Y], Result[Y])
//		def result[YI, YL](init :XI sql_=> YI, last :XL sql_=> YL, post :Result[YI |~ (K :~ YL)])
//				:Opt[(XI sql_=> YI, XL sql_=> YL, SQLTransformation[YI |~ (K :~ YL), Z]#Into[conversion.Expression])] =
//			Got((init, last, post))
		conversion match {
			case _ if conversion.isIdentity =>
//				result(SQLConversion.toSelf[XI], SQLConversion.toSelf[XL], conversion)
				Got((SQLConversion.toSelf[XI], SQLConversion.toSelf[XL],
					conversion :SQLTransformation.Into[XI |~ (K :~ XL), Z, conversion.Expression]
				))
			case _ :Upcast[XI |~ (K :~ XL), Z] =>
//				result(upcastListing.castParams[XI, XI], upcastAny.castParams[XL, XL], conversion)
				Got((upcastListing.castParams[XI, XI], upcastAny.castParams[XL, XL],
					conversion :SQLTransformation.Into[XI |~ (K :~ XL), Z, conversion.Expression]
				))
//				Got((SQLConversion.toSelf[XI], SQLConversion.toSelf[XL], conversion))
			case chain :RecordConversion[xi, xl, yi, yl, k] =>
				Got((chain.init, chain.last, SQLConversion.toSelf.asInstanceOf[Result[yi |~ (k :~ yl)]]))
			//if conversion is composed of only ConvertChain, we can split each and compose each item individually
			case _ => SQLTransformation.Composition.WithConversion.unapply(conversion) match {
				//a way of ensuring the intermediate existential types match and we can recompose
				case composed :Opt[Composed[y]] if composed.isDefined =>
					type Y = y
					val first  :SQLConversion[XI |~ (K :~ XL), Y] = composed.get._1
					val second :Result[Y] = composed.get._2
					type SplitFirst[YI <: Listing, YL] =
						(XI sql_=> YI, XL sql_=> YL, SQLTransformation.Into[YI |~ (K :~ YL), Y, first.Expression])
//					type SplitFirst[YI <: Listing, YL] =
//						(XI sql_=> YI, XL sql_=> YL, SQLTransformation.Returning[YI |~ (K :~ YL), Y, first.Expression])
					splitIndexTransformation(first) match {
						case splitFirst :Opt[SplitFirst[yi, yl]] if splitFirst.isDefined =>
							type YI = yi; type YL = yl
//							val (firstInit, firstLast, firstPost) = splitFirst.get
							val firstInit  :XI sql_=> YI = splitFirst.get._1
							val firstLast  :XL sql_=> YL = splitFirst.get._2
							val firstWhole :SQLTransformation[YI |~ (K :~ YL), Y] = splitFirst.get._3
//							type SplitSecond[ZI <: Listing, ZL] = (
//								YI sql_=> ZI, YL sql_=> ZL, SQLTransformation[ZI |~ (K :~ ZL), Z]#Into[second.Expression]
//							)
							type SplitSecond[ZI <: Listing, ZL] = SplitListing[YI, K, YL, ZI, ZL, Z, second.Expression]
							firstWhole match {
								case conversion :SQLConversion[YI |~ (K :~ YL), Y] if conversion.isIdentity =>
									splitIndexTransformation(conversion andThen second) match {
										case splitSecond :Opt[SplitSecond[zi, zl]] if splitSecond.isDefined =>
											val secondInit  :YI sql_=> zi = splitSecond.get._1
											val secondLast  :YL sql_=> zl = splitSecond.get._2
											val secondWhole :Result[zi |~ (K :~ zl)] = splitSecond.get._3
											val initResult     = firstInit andThen secondInit
											val lastResult     = firstLast andThen secondLast
		//									Got((initResult, lastResult, combinedResult))
											type ZI = Listing
											Got((initResult.castParam2[ZI], lastResult, secondWhole.castParam[ZI |~ (K :~ zl)]))
//											result(initResult.castParam2[ZI], lastResult, secondWhole.castParam[ZI |~ (K :~ zl)])
										case _ =>
											//The compiler doesn't know that SQLResult is preserved during composition
											// because the functions were already composed in conversion to start with.
											val whole = (firstWhole andThen second).asInstanceOf[Result[YI |~ (K:~YL)]]
											Got((firstInit, firstLast, whole))
//											result(firstInit, firstLast, whole)
									}
										/* Technically, if firstWhole is Upcast, then the second split might still
										 * succeed. We can't just reuse the nice type checking code above, because
										 * firstWhole andThen second would cause an infinite recursion, so we'd
										 * need to cast the input type of second, and to a Listing in order to
										 * call splitListingTransformation again, which we can't know if is true.
										 * The current implementation will handle it, because the presence
										 * of an Upcast makes the whole conversions irreversible anyway,
										 * so spliting second :Upcast into init and last Upcast doesn't change
										 * much for us. All those types in this function are more documentation
										 * than anything else.
										 */
//								case conversion :SQLConversion[YI |~ (K:~YL), Y] if conversion.isUpcast =>
								case _ =>
									val whole = (firstWhole andThen second).asInstanceOf[Result[YI |~ (K :~ YL)]]
									Got((firstInit, firstLast, whole))
//									result(firstInit, firstLast, whole)
							}
//							assert(splitFirst.get._3.isIdentity, splitFirst.get._3 + " is not identity")
						case _ => Lack
					}
				case _ => Lack
			}
		}
	}

	private val upcastListing = SQLConversion.supertype[Listing, Listing]
	private val upcastAny     = SQLConversion.supertype[Any, Any]




	trait SpecificIndexedVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] { //extends SpecificEmptyVisitor[X, Y] {
		def indexed[V <: Listing](e :IndexedSQL[F, S, V])(implicit isListing :X =:= V) :Y
	}
	trait MatchSpecificIndexed[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificIndexedVisitor[F, S, X, Y]
	{
		def emptyIndexed(implicit ev :X =:= @~) :Y
		def indexedItem[I <: Listing, K <: Label, L]
		               (e :IndexedSQL[F, S, I |~ (K :~ L)])(implicit isListing :X =:= (I |~ (K :~ L))) :Y

		override def indexed[V <: Listing](e :IndexedSQL[F, S, V])(implicit isListing :X =:= V) :Y =
			e match {
				case tuple :NonEmptyIndexedSQL[F, S, i, k, l] =>
					indexedItem(tuple)(isListing.asInstanceOf[X =:= (i |~ (k :~ l))])
				case _ => emptyIndexed(isListing.castParam2[@~])
			}
	}
	type CaseSpecificIndexed[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificIndexedVisitor[F, S, X, Y]
//
//
//	trait LabeledVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def labeled[S >: Grouped <: Single, T <: Listing](e :IndexedSQL[F, S, T]) :R[S, T, IndexedSQL[F, S, T]]
//	}
//	trait MatchLabeled[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends LabeledVisitor[F, R]
//	{
//		def labeledItem[S >: Grouped <: Single, I <: Listing, K <: Label, L]
//		               (e :IndexedSQL[F, S, I |~ (K :~ L)]) :R[S, I ~ (K :~ L), IndexedSQL[F, S, I |~ (K :~ L)]]
//
//		def emptyLabeled :R[Single, @~, IndexedSQL[RowProduct, Single, @~]]
//
//		override def labeled[S >: Grouped <: Single, T <: Listing]
//		                    (e :IndexedSQL[F, S, T]) :R[S, T, IndexedSQL[F, S, T]] =
//			(e match {
//				case tuple :NonEmptyIndexedSQL[F, S, i, k, l] => labeledItem(tuple)
//				case _ => emptyLabeled
//			}).asInstanceOf[R[S, T, IndexedSQL[F, S, T]]]
//	}
//	type CaseLabeled[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		LabeledVisitor[F, R]


	trait AnyIndexedVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {//extends AnyEmptyVisitor[Y] {
		def indexed[S >: Grouped <: Single, V <: Listing](e :IndexedSQL[F, S, V]) :Y[S, V]
	}

	trait MatchAnyIndexed[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyIndexedVisitor[F, Y]
	{
		def emptyIndexed :Y[Single, @~]

        def indexedItem[S >: Grouped <: Single, I <: Listing, K <: Label, L]
		               (e :IndexedSQL[F, S, I |~ (K :~ L)]) :Y[S, I |~ (K :~ L)]

		override def indexed[S >: Grouped <: Single, V <: Listing](e :IndexedSQL[F, S, V]) :Y[S, V] =
			(e match {
				case tuple :NonEmptyIndexedSQL[F, S, i, k, l] => indexedItem[S, i, k, l](tuple)
				case _ => emptyIndexed
			}).asInstanceOf[Y[S, V]]
	}
	type CaseAnyIndexed[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyIndexedVisitor[F, Y]
}






private final class NonEmptyIndexedSQL[-F <: RowProduct, -S >: Grouped <: Single, I <: Listing, K <: Label, L]
                                      (override val init :IndexedSQL[F, S, I], override val lastKey :K,
                                       override val last :LabeledValueSQL[F, S, L])
//	extends NonEmptyRecordTemplate[F, S, I, K, L, LabeledValueSQL, IndexedSQL](init, lastKey, last)
	extends UnsealedLabeledSQL[F, S, I, K, L](init, lastKey, last)
	   with NonEmptyRecordTemplate[F, S, I, K, L, LabeledValueSQL, IndexedSQL]
	   with IndexedSQL[F, S, I |~ (K :~ L)]
	   with RowShapeCache
{ self =>
	def this(left :IndexedSQL[F, S, I], right :LabeledValueSQL[F, S, L])(implicit key :ValueOf[K]) =
		this(left, key.value ,right)

	override type LabeledItem[-F1 <: RowProduct, -S1 >: Grouped <: Single, K1 <: Label, X] = IndexItem[F1, S1, K1, X]

	override def right :SQLExpression[F, S, L] = last //LabeledSQL wrapped it in NonSpecificSQL, we must unwrap it
	override val item = new IndexItem(init.size, lastKey, last)
//	override val items :IndexedSeq[Item[F, S, _]] = init.items :+ item

//	override lazy val items   :IndexedSeq[Item[F, S, _]] = init.items :+ item
	override lazy val paths   :Seq[LabelPath[_]] =
		dfs(init.paths)((s, p, _) => s :+ p)((s, _, _) => s)
	override lazy val columns :Map[LabelPath[_], LabeledColumnSQL[F, S, _ <: Label, _]] =
		dfs(init.columns)(_.updated(_, _))((s, _, _) => s)
	override lazy val index   :Map[LabelPath[_], LabeledValueSQL[F, S, _]] =
		dfs(init.index)(_.updated(_, _))(_.updated(_, _))

	//this recursive implementation somewhat assume that paths is a PassedArray or at least a Vector as it uses :+
	private[this] def dfs[T, E](acc :T, prefix :LabelPath[_] = ~/(lastKey), e :LabeledValueSQL[F, S, _] = last)
	                           (onColumn :(T, LabelPath[_], LabeledColumnSQL[F, S, _ <: Label, _]) => T)
	                           (onRecord :(T, LabelPath[_], LabeledValueSQL[F, S, _]) => T) :T =
		(e: @unchecked) match {
			case indexed :NonEmptyIndexedSQL[F, S, _, _, _] =>
				def rec(e :NonEmptyIndexedSQL[F, S, _, _, _], res :T) :T =
					e.init match {
						case record :NonEmptyIndexedSQL[F, S, _, _, _] =>
							val init = onRecord(rec(record, res), prefix, record)
							dfs(init, prefix / record.lastKey, record.last)(onColumn)(onRecord)
						case _ => //EmptySQL
							res
					}
				rec(indexed, acc)
			case column :LabeledColumnSQL[F, S, _, _] =>
				onColumn(acc, prefix, column)
		}

//	override def construct(items :({type T[X] = LabeledItem[F, S, _ <: Label, X]})#T =>: generic.Self) :I |~ (K :~ L) =
//		init.construct(items) |~ :~[K](items(item))

	override def nullSQL :IndexedSQL[RowProduct, Single, I |~ (K :~ L)] =
		init.nullSQL |~ :~[K](last.nullSQL)

	override def nullValue :Opt[I |~ (K :~ L)] = (init.nullValue, last.nullValue) match {
		case (Got(init), Got(last)) => Got(init |~ :~[K](last))
		case _ => Lack
	}

	private[sql] override def rephraseAsLabeledValue[E <: RowProduct](mapper :SQLScribe[F, E])
			:LabeledValueSQL[E, S, I |~ (K :~ L)] =
		rephrase(mapper)

	//This short-circuits the call, resulting in no-callbacks for the prefixes. Lets call it a feature.
	// The override is needed because the return type is different in IndexedSQL and LabeledSQL
	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :NonEmptyIndexedSQL[E, S, I, K, L] =
		(init.rephrase(mapper), last.rephraseAsLabeledValue(mapper)) match {
			case (i, l) if (i eq init) && (l eq last) =>
				this.castFrom[NonEmptyIndexedSQL[F, S, I, K, L], NonEmptyIndexedSQL[E, S, I, K, L]]
			case (i, l) => new NonEmptyIndexedSQL(i, lastKey, l)
		}

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, I], right :SQLExpression[E, C, L])
			:NonEmptyIndexedSQL[E, C, I, K, L] =
		(left, right) match {
			case _ if (left eq this.left) && (right eq this.right) =>
				this.castFrom[NonEmptyIndexedSQL[F, S, I, K, L], NonEmptyIndexedSQL[E, C, I, K, L]]
			case (index :IndexedSQL[E, C, I], labeled :LabeledValueSQL[E, C, L]) =>
				new NonEmptyIndexedSQL(index, lastKey, labeled)
			case _ =>
				throw new IllegalExpressionException(
					"Cannot recreate an IndexedSQL `" + this + "` with expressions `" + left + "` and `" + right +
						"` (" + left.className + " |~ " + right.className + ")"
				)
		}
//	protected override def empty = EmptySQL
//
//	protected override def cons[E <: RowProduct, C >: Grouped <: Single, I1 <: Listing, K1 <: Label, L1]
//	                           (init :IndexedSQL[E, C, I1], key :K1, last :LabeledValueSQL[E, C, L1]) =
//		(init |~ :~[K1](last))(new ValueOf[K1](key))


	override def reorder(paths :Seq[LabelPath[_]]) :SQLExpression[F, S, I |~ (K :~ L)] =
		if (this.paths == paths)
			this
		else {
			val includedInThis = this.index.keySet
			if (!paths.forall(includedInThis))
				throw new IllegalArgumentException(
					"Record reordering " + paths + " contains paths not existing in " + this + " " + this.paths + "."
				)
			val coveredPaths = paths.view.map(_.toList) to Set
			@tailrec def coversPath(path :List[Label]) :Boolean =
				coveredPaths(path) || path.nonEmpty && coversPath(path.tail)

			if (!this.paths.view.map(_.toList).forall(coversPath))
				throw new IllegalArgumentException(
					"Record reordering " + paths + " of expression " + this + " does not include all columns: " +
						paths.filterNot(this.columns.contains) + "."
				)
			reform("reorder", paths.map(path => (path, index(path))))
		}

	override def project(paths :Seq[LabelPath[_]]) :SQLExpression[F, S, I |~ (K :~ L)] =
		if (this.paths == paths)
			this
		else {
			val includedInThis = this.index.keySet
			if (!paths.forall(includedInThis))
				throw new IllegalArgumentException(
					"Record projection " + paths + " contains paths not existing in " + this + " " + this.paths + "."
				)
			reform("project", paths.map(path => (path, index(path))))
		}

	override def reform[E <: F, A >: Grouped <: S]
	                   (paths :Seq[(LabelPath[_], LabeledValueSQL[E, A, _])]) :SQLExpression[E, A, I |~ (K :~ L)] =
		reform("reform", paths)

	private def reform[E <: F, A >: Grouped <: S](name :String, paths :Seq[(LabelPath[_], LabeledValueSQL[E, A, _])])
			:SQLExpression[E, A, I |~ (K :~ L)] =
	{
		if (paths.isEmpty)
			throw new IllegalArgumentException("Cannot project record " + this + " to an empty column set.")
		else if (this.paths.corresponds(paths)(_ == _._1))
			this
		else {
			type Index = List[(String, Either[LabeledValueSQL[E, A, _], List[Any]])]
			/* A function converting between recursive Listing types with the structure represented by the arguments.
			 * Both list arguments have a recursive structure, with second elements of the pair having the same type
			 * as the argument lists. Each list element represents an element of either the argument listing,
			 * or the returned listing. The first pair elements in the entries are top level keys in a listing,
			 * while the last elements are either placeholder expressions providing a default (null) value,
			 * for when a corresponding entry in the other record does not exist, or a list of the same type
			 * as the argument list, containing recursive structure of the record field under the associated key.
			 *
			 * If a value is needed for a path not present in srcKeys, then the Either provided
			 * in dstKeys under the same path must be a Left, and expr.nullValue is used (not selectForm.nullValue) -
			 * this is the case for all columns, as well as completely omitted values.
			 * If an entry in dstKeys contains a Right, then src keys must use Right for the same key,
			 * and the value in the argument src for this key must be also a Listing.
			 */
			def convert(//the index for src argument: tails of the paths in the paths argument grouped by the first path element
                        srcKeys :List[(String, Either[LabeledValueSQL[E, A, _], List[Any]])],
			            //the index of the built record: each element represents a LabeledValueSQL under this IndexedSQL
			            dstKeys :List[(String, Either[LabeledValueSQL[E, A, _], List[Any]])]
			           )(src :Listing) :Listing =
			{
				val entries = src.toSeq
				assert(srcKeys.length == entries.length,
					"The number of elements in the argument record " + src +
						" does not match the number of elements in the pre-built index " + srcKeys
				)
				val topValues = srcKeys.view.zipMap(entries) {
					case ((key, subtree), entry) => (key, (entry.value, subtree))
				}.toMap
				((@~ :Listing) /: dstKeys) { case (acc, (key, subtree)) =>
					subtree match {
						//We either use the field from the argument verbatim, or a null value from the provided expr
						case Left(expr) => topValues.get(key) match {
							case Some((value, _)) => acc |~ :~[key.type](value)
							case _ => acc |~ :~[key.type](expr.nullValue)
						}
						case Right(dst :Index @unchecked) =>
							topValues.get(key) match {
								case Some((listing :Listing, Right(index :Index @unchecked))) =>
									acc |~ :~[key.type](convert(index, dst)(listing))
								case Some((value, index)) =>
									throw Bug(s"Expected a Listing and an index list of entries, got ($value, $index).")
								case _ =>
									throw new AssertionError(
										"No field " + key + " in the mapped record " + src + "."
									)
							}
					}
				}
			}

			/* Recursively converts a record, starting with this expression, to an index of all direct and indirect
			 * values, both columns and other records. The returned List is of the same length as this.keys,
			 * and contains the latter as the first elements in the list entries. The second elements
			 * are this.toMap(key).selectForm.nulls, while the third is either an empty list, if value.toMap(key)
			 * is a column, or a recursively built index for the latter.
			 */
			def buildDstIndex(value :IndexedSQL[F, S, _]) :List[(String, Either[LabeledValueSQL[E, A, _], List[Any]])] =
				value.keys.view.map { key =>
					value.toMap(key) match {
						case column :LabeledColumnSQL[F, S, _, _] => (key, Left(column))
						case value :IndexedSQL[F, S, _] => (key, Right(buildDstIndex(value)))
					}
				}.toList

			/* Converts the argument paths as srcPaths to a recursive tree index: the first element of the pair
			 * is the key to a node, the second element is NullValue.NotNull, used for paths not present
			 * in this record, and the third element is a list with subnodes, of the same type as the return type.
			 */
			def buildSrcIndex(srcPaths :List[(List[String], LabeledValueSQL[E, A, _])],
			                  //path prefix to the current level
				              prefix :LabelPathPrefix,
			                  //the head of the last non empty element in srcPaths
			                  First :String = null,
			                  //tails of preceding elements in srcPath which had First as their head
			                  current :ListBuffer[(List[String], LabeledValueSQL[E, A, _])] =
			                    ListBuffer.empty[(List[String], LabeledValueSQL[E, A, _])],
			                  //result accumulator
			                  res :ListBuffer[(String, Either[LabeledValueSQL[E, A, _], List[_]])] =
			                    ListBuffer.empty[(String, Either[LabeledValueSQL[E, A, _], List[_]])])
					:List[(String, Either[LabeledValueSQL[E, A, _], List[_]])] =
				//invariant: First == null iff current.isEmpty
			{
				type Entry = (List[String], LabeledValueSQL[E, A, _])
				/* Fetches the expression under the given path in this expression, or returns alternatives
				 * if the path is not present in this expression. */
				def pathEntry(path :LabelPath[_], alternative :LabeledValueSQL[E, A, _]) :LabeledValueSQL[E, A, _] =
					this.index.get(path) match {
						case Some(value) => value
						case _ => alternative match {
							case column :LabeledColumnSQL[E, A, _, _] if column.alias != path.last =>
								throw new IllegalArgumentException(
									"Cannot introduce column " + column + " into " + this + " under path " + path +
									" because column's alias does not match the last element of the path."
								)
							case _ =>
								alternative
						}
					}
				srcPaths match {
					//First recursion step on this level, srcPaths contains a single, empty, path.
					case (Nil, alternative)::Nil if current.isEmpty =>
						assert(prefix.isInstanceOf[LabelPath[_]],
							"An empty path prefix when projecting " + this + " to " + paths + "."
						)
						val path = prefix.asInstanceOf[LabelPath[_]]
						(path.last, Left(pathEntry(path, alternative)))::Nil
					//an empty 'top level' path included with other paths in original srcDst
					case (Nil, _)::_ =>
						throw new IllegalArgumentException(
							"Paths " + paths + " include " + prefix +
								" which is either a duplicate or a prefix or of another path."
						)
					//Also the first recursion step, there are non empty paths under prefix
					case (key::path, alternative)::tail if current.isEmpty =>
						buildSrcIndex(tail, prefix, key, current += ((path, alternative)), res)
					//The first label in the current path is the same as in the previous one
					case (First::path, alternative)::tail =>
						buildSrcIndex(tail, prefix, First, current += ((path, alternative)), res)
					//The current path diverges from the previous one on this level
					case (key::path, alternative)::tail =>
						if (res.lastIndexWhere(_._1 == key) >= 0)
							throw new IllegalArgumentException(s"Paths $paths are not grouped by their keys.")
						//recursively build the index for the finished node
						val lastValuePath = prefix / First
						val lastIndex = buildSrcIndex(current.toList, lastValuePath)
						val last = (First, Right(lastIndex))
						//and now continue the recursion on the same level
						buildSrcIndex(tail, prefix, key, ListBuffer.empty[Entry] += ((path, alternative)), res += last)
					case Nil =>
						res.toList
				}
			}
			def buildNewExpr(keys :List[(String, Either[LabeledValueSQL[E, A, _], List[_]])], acc :IndexedSQL[E, A, _])
					:IndexedSQL[E, A, _] =
				keys match {
					case (key, index)::t => index match {
						case Left(expr) =>
							buildNewExpr(t, acc |~ ((key :Label, expr)))
						case Right(tree :Index @unchecked) =>
							buildNewExpr(t, acc |~ ((key :Label, buildNewExpr(tree, EmptySQL))))
					}
					case _ => acc
				}
			val pathList = paths.view.map { case (path, placeholder) => (path.toList, placeholder) }.toList
			val srcIndex = buildSrcIndex(pathList, ~/)
			val dstIndex = buildDstIndex(this)
			val result = buildNewExpr(srcIndex, EmptySQL).castFrom[
				IndexedSQL[E, A, _], IndexedSQL[E, A, Listing]
			]
			val conversionName = this.paths.mkString("." + name + "(", ", ", ")")
			val conversion = SQLConversion(
				conversionName, convert(srcIndex, dstIndex), convert(dstIndex, srcIndex)
			).castParam2[I |~ (K :~ L)]
			conversion(result)
		}
	}


	protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                               (implicit leftResult  :SQLTransformation[I |~ (K :~ L), U],
	                                         rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])] =
		new BaseReformer[F2, S2, V2, EC2, U, leftResult.Expression, rightResult.Expression](
			other)(reform, passCount)(leftResult, rightResult, spelling
		) with MatchSpecificIndexed[F2, S2, V2, (leftResult.Expression[F, S, SQLExpression[F, S, U]],
		                                         rightResult.Expression[F2, S2, EC2[U]])]
		{
			private def forceNullValue(x: @~) = nullValue match {
				case Got(e) => e
				case _ => throw new UnsupportedOperationException(
					keys.mkString(
						"Cannot convert EmptySQL to Listing(", ",", ") because the matched expression " +
							self + " does not define a null value."
					)
				)
			}

			override def multiNull(e :MultiNull[V2]) =
				if (reform.mayReformRight ||
					reform.mayAddNullRight && rightResult.isValueIdentity && e.form.columnCount < selectForm.columnCount)
				{
					val rightNull = MultiNull(effectiveForm)
					(this.left, rightResult(rightNull.asInstanceOf[EC2[V2]]))
				} else
					fallback

			override def empty(implicit isEmpty :V2 =:= @~) =
				if (passCount.secondTime && reform.mayAddNullRight) {
					val nullConversion =
						SQLConversion.opt("to[@~]", (_:(I |~ (K :~ L))) => @~ : @~, (_: @~) => nullValue)
					val toV2 = isEmpty.flip.liftCo[SQLConversion.from[I |~ (K :~ L)]#to](nullConversion)
					(this.left, rightResult(toV2(nullSQL.asInstanceOf[EC2[I |~ (K :~ L)]])))
				} else if (passCount.secondTime && reform.mayExcludeLeft) {
					/* Two options here: either we fail fast in case of lack of nullValue as an early warning,
					 * or we hope that the method never gets called. Ideally we would report a warning
					 * in the latter case, but we do not have SQLContext here. Perhaps we will just log
					 * it here and now ourselves. */
					val nullConversion = SQLConversion.opt(
						keys.mkString(".to[", ",", "]"), forceNullValue, (_:(I |~ (K :~ L))) => Got(@~)
					)
					(leftResult(nullConversion(EmptySQL)), this.right)
				} else
					fallback

			//we could also reform with a LabeledColumnSQL, too
			override def indexed[V <: Listing](e :IndexedSQL[F2, S2, V])(implicit isListing :V2 =:= V)
					:(leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
				if (keys == e.keys)
					super.indexed(e)
				else {
					//Attempt to reorder both or either expression, either excluding or adding null columns,
					// but only if the column forms match exactly on shared columns, because we have no information
					// on how - if at all - I |~ (K :~ L) and V2 are related.
					val leftColumns = self.columns
					val rightColumns = e.columns
					val subsetOfRight = leftColumns.forall {
						case (path, col) => rightColumns.get(path).exists(_.selectForm == col.selectForm)
					}
					val leftPaths  = leftColumns.keySet
					val rightPaths = rightColumns.keySet
					val subsetOfLeft =
						if (subsetOfRight)
							leftPaths == rightPaths
						else
							rightColumns.forall {
								case (path, col) => rightColumns.get(path).exists(_.selectForm == col.selectForm)
							}
					def subseqOfRight = {
						val otherPaths = e.paths to Unique
						paths.isSortedBy(otherPaths.indexOf)
					}
					def subseqOfLeft = {
						val paths = self.paths to Unique
						e.paths.isSortedBy(paths.indexOf)
					}
					if (subsetOfRight && subsetOfLeft) //same structure, different order
						if (reform.mayReorderLeft) {
							val reordered = reorder(e.paths)
							(leftResult(reordered), rightResult(other))
						} else if (reform.mayReorderRight) {
							val reordered = e.reorder(paths).asInstanceOf[Argument]
							(leftResult(self), rightResult(reordered))
						} else
							fallback
					else if (subsetOfRight)
						if (reform.mayExcludeRight) //use the left path set, it is a subset the right one
							if  (reform.mayReorderRight || subseqOfRight) {
								val reordered = e.project(paths).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
								(leftResult(self), rightResult(reordered))
							} else if (reform.mayReorderLeft) {
								val order = e.paths.filterNot(leftColumns.keySet)
								val right = e.project(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
								(leftResult(reorder(order)), rightResult(right))
							} else
								fallback
						else if (reform.mayAddNullLeft) //use the right path set, it is a superset of the left one
							if (reform.mayReorderLeft || subseqOfRight) {
								val order = e.paths.map(path => (path, rightColumns(path).nullSQL))
								(leftResult(self.reform(order)), rightResult(other))
							} else if (reform.mayReorderRight) {
								val order = paths ++ e.paths.filterNot(leftPaths)
								val left  = self.reform(order.map(path => (path, rightColumns(path).nullSQL)))
								val right = e.project(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
								(leftResult(left), rightResult(right))
							} else
								fallback
						else
							fallback
					else if (subsetOfLeft) //symmetrical to the above, but extracting as a method would actually take more code
						if (reform.mayExcludeLeft) //use the right path set, it is a subset of the left one
							if (reform.mayReorderLeft || subseqOfLeft)
								(leftResult(project(e.paths)), rightResult(other))
							else if (reform.mayReorderRight) {
								val order = paths.filterNot(rightColumns.keySet)
								val right = e.reorder(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
								(leftResult(project(order)), rightResult(right))
							} else
								fallback
						else if (reform.mayAddNullRight) //use the left path set, it a superset of the right one
							if (reform.mayReorderRight || subseqOfLeft) {
								val order = paths.map(path => (path, leftColumns(path).nullSQL))
								val right = e.reform(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
								(leftResult(self), rightResult(right))
							} else if (reform.mayReorderLeft) {
								val order = e.paths ++ paths.filterNot(rightPaths)
								val right = e.reform(order.map(path => (path, leftColumns(path).nullSQL)))
									.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
								(leftResult(project(order)), rightResult(right))
							} else
								fallback
						else
							fallback
					else {
						val sharedInLeftOrder  = paths.filter(rightPaths)
						val sharedInRightOrder = e.paths.filter(leftPaths)
						if (reform.mayExcludeLeft)
							if (reform.mayExcludeRight) { //user the shared path set, projecting both sides
								if (reform.mayReorderLeft || sharedInLeftOrder == sharedInRightOrder) {
									val left  = self.project(sharedInRightOrder)
									val right = e.project(sharedInRightOrder).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(left), rightResult(right))
								} else if (reform.mayReorderRight) {
									val left  = self.project(sharedInLeftOrder)
									val right = e.project(sharedInLeftOrder).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(left), rightResult(right))
								} else
									fallback
							} else if (reform.mayAddNullLeft) { //use the right path set
								if (reform.mayReorderLeft || sharedInLeftOrder == sharedInRightOrder) {
									val left  = self.reform(e.paths.map { path => (path, rightColumns(path).nullSQL) })
									(leftResult(left), rightResult(other))
								} else if (reform.mayReorderRight) {
									val order = sharedInLeftOrder ++ e.paths.filterNot(leftPaths)
									val left = self.reform(order.map { path => (path, rightColumns(path).nullSQL) })
									val right = e.reorder(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(left), rightResult(right))
								} else
									fallback
							} else
								fallback
						else if (reform.mayAddNullRight)
							if (reform.mayExcludeRight) { //use the left path set
								if (reform.mayReorderRight || sharedInLeftOrder == sharedInRightOrder) {
									val right = e.reform(paths.map { path => (path, leftColumns(path).nullSQL) })
									(leftResult(self), rightResult(right.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
								} else if (reform.mayReorderLeft) {
									val order = e.paths.filter(leftPaths) ++ paths.filterNot(rightPaths)
									val left  = self.reorder(order)
									val right = e.reform(order.map { path => (path, leftColumns(path).nullSQL) })
									(leftResult(left), rightResult(right.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
								} else
									fallback
							} else if (reform.mayAddNullLeft)  //use the superset of both path sets
								if (reform.mayReorderLeft || sharedInLeftOrder == sharedInRightOrder) {
									val order =
										e.paths.map { path => (path, rightColumns(path).nullSQL) } ++
											paths.filterNot(rightPaths).map { path => (path, leftColumns(path).nullSQL) }
									val right = e.reform(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(self.reform(order)), rightResult(right))
								} else if (reform.mayReorderRight) {
									val order =
										paths.map { path => (path, leftColumns(path).nullSQL) } ++
											e.paths.filterNot(leftPaths).map { path => (path, rightColumns(path).nullSQL) }
									val right = e.reform(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(self.reform(order)), rightResult(right))
								} else
									fallback
							else
								fallback
						else
							fallback
					}
				}

			override def emptyIndexed(implicit ev :V2 =:= @~) =
				if (reform.mayAddNullRight) {
					val forceEmpty = SQLConversion(" => @~", (_ :I |~ (K :~ L)) => @~)
					(leftResult(self), //this cast can be removed by delegating this case to EmptyIndex
					 rightResult(forceEmpty(nullSQL).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
				} else if (reform.mayExcludeLeft)
					try {
//							val nullValue = NonEmptyIndexedSQL.this.nullValue
//							val forceNull = SQLConversion(" => (" + nullValue + ")", forceNullValue)
//							val left = forceNull(nullSQL :ConvertibleSQL[RowProduct, Single, @~, GroundSQL])
						(leftResult(nullSQL), rightResult(other))
					} catch {
						case e1 :NullPointerException => try {
							fallback
						} catch {
							case e2 :MismatchedExpressionsException =>
								e2.addSuppressed(e1)
								throw e2
						}
					}
				else
					fallback

			override def indexedItem[I2 <: Listing, K2 <: Label, L2]
			                        (e :IndexedSQL[F2, S2, I2 |~ (K2 :~ L2)])
			                        (implicit isListing :V2 =:= (I2 |~ (K2 :~ L2))) =
			{
				implicit val listingResult = isListing.substituteCo[RightResult](rightResult)

				//Attempt to recreate a IndexedSQL by casting down the expressions obtained
				// by reforming init and last
				def listing[G <: RowProduct, D >: Grouped <: Single, W <: Listing, A <: Label, Z]
				           (i :SQLExpression[G, D, W], key :A, l :SQLExpression[G, D, Z]) =
					IndexedSQL.attempt(i, key, l) match {
						case Lack if !i.isInstanceOf[IndexedSQL[_, _, _]] =>
							throw new MismatchedExpressionsException(NonEmptyIndexedSQL.this, e,
								"received a non-IndexedSQL expression " + i +
									": " + i.getClass.getName + " as a partial result."
							)
						case Lack =>
							throw new MismatchedExpressionsException(NonEmptyIndexedSQL.this, e,
								"Reformed last expression " + l + " is neither a ColumnSQL nor a LabeledValueSQL."
							)
						case Got(res) => res
					}
				def isUpcast(conversion :SQLTransformation[_, _]) =
					conversion.isIdentity || conversion.isInstanceOf[Upcast[_, _]]

				(splitIndexTransformation[I, K, L, U](leftResult),
					splitIndexTransformation[I2, K2, L2, U](listingResult)
				) match {
					case (Lack, _) =>
						throw new MismatchedExpressionsException(
							self, other, "unsupported left conversion type " + leftResult + "."
						)
					case (_, Lack) =>
						throw new MismatchedExpressionsException(
							self, other, "unsupported right conversion type " + rightResult + "."
						)
					//init and last are converted separately, no conversion applied on top of init |~ key :~ last
//						case (left :Opt[LeftSplit[Listing, Any]]@unchecked, right :Opt[RightSplit[Listing, Any]]@unchecked) =>
//	//					if left.get._3.isIdentity && right.get._3.isIdentity =>
////							implicit val (leftInitResult, leftLastResult, leftPost) = left.get
////							implicit val (rightInitResult, rightLastResult, rightPost) = right.get
//							val leftInitResult  = left.get._1
//							val leftLastResult  = left.get._2
//							val leftPost        = left.get._3
//							val rightInitResult = right.get._1
//							val rightLastResult = right.get._2
//							val rightPost       = right.get._3
					case (Got(leftInitResult :(I sql_=> Listing) @unchecked, leftLastResult :(L sql_=> Any) @unchecked,
					      leftPost :SQLTransformation.Into[Listing |~ (K :~ Any), U, leftResult.Expression] @unchecked),
					      Got(rightInitResult :(I2 sql_=> Listing) @unchecked, rightLastResult :(L2 sql_=> Any) @unchecked,
					      rightPost :SQLTransformation.Into[Listing |~ (K2 :~ Any), U, rightResult.Expression]@unchecked))
					/* This check approximates the condition that the input types of leftPost and rightPost are the same.
					 * 1. We know the outputs are equal, so if the conversions are also equal, we assume the same for inputs.
					 * 2. We know both expressions before conversions have Listing as value types, so if both
					 *    conversions are upcasts/identities, then the conversions can be split into components
					 *    for init and last. Function splitListingTransformation has already helpfully wrapped
					 *    the expressions in suitable upcast conversions, which will at least generate more
					 *    informative errors if we fail to convert value from one expression to the type in the other.
					 * This is all with fingers crossed, but there is much more usefulness in the ability to
					 * unify two records than providing strict guarantees about Scala type safety,
					 * because if the only thing we need to do is convert it to SQL, without interchanging values
					 * with the application, then it all doesn't matter anyway.
					 */
					if leftPost == rightPost || isUpcast(leftPost) && isUpcast(rightPost) =>
						val (leftLast, rightLast) = reform(self.last, e.last)(
							leftLastResult, rightLastResult, spelling
						)
						val (leftInit, rightInit) = reform(self.init, e.init)(
							leftInitResult, rightInitResult, spelling
						)
						val leftReformed  = listing(leftInit, lastKey :K, leftLast)
						val rightReformed = listing(rightInit, e.lastKey :K2, rightLast).castFrom[
							IndexedSQL[F2, S2, Listing |~ (K2 :~ Any)],
							ConvertibleSQL[F2, S2, Listing |~ (K2 :~ Any), EC2]
						]
						(leftPost.convert(leftReformed), rightPost.convert(rightReformed))
				}
			}
		}
}

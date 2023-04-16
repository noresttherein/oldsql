package net.noresttherein.oldsql.sql.ast


import scala.annotation.{implicitAmbiguous, implicitNotFound, tailrec}

import net.noresttherein.oldsql.collection.Chain.{@~, ChainLength, ~}
import net.noresttherein.oldsql.collection.{Chain, ConstSeq, Listing, Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, InseparableExpressionException, UndefinedShapeException}
import net.noresttherein.oldsql.morsels
import net.noresttherein.oldsql.morsels.{Extractor, generic, inversePermutation}
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.generic.=>:
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.bits.LabelPath
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm}
import net.noresttherein.oldsql.slang.{cast2TypeParams, castTypeParam, classNameMethods, downcastTypeParam, mappingMethods, saferCasting}
import net.noresttherein.oldsql.sql.{RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, GroundingOps, Grouped, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.{BinaryCompositeSQL, BinaryCompositeTemplate, CompositeSQLTemplate}
import net.noresttherein.oldsql.sql.ast.EmptySQL.{AnyEmptyVisitor, SpecificEmptyVisitor}
import net.noresttherein.oldsql.sql.ast.IndexedSQL.{AnyIndexedVisitor, CaseAnyIndexed, CaseSpecificIndexed, LabeledColumnSQL, LabeledValueSQL, SpecificIndexedVisitor}
import net.noresttherein.oldsql.sql.ast.InlineSQL.{AbstractInlineBinaryCompositeSQL, InlineItem}
import net.noresttherein.oldsql.sql.ast.RecordSQL.{CaseAnyRecord, CaseSpecificRecord, ProjectRecord, RecordField, RecordItem, RecordSQLTemplate, ShuffleRecord, SplitRecord}
import net.noresttherein.oldsql.sql.mechanics.{AlignableColumns, SQLConversion, SQLScribe, SQLTransformation, sql_=>}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.MayReform
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.{RecordConversion, RecordReordering, Upcast}




sealed class LabeledSQL[-F <: RowProduct, -S >: Grouped <: Single, I <: Listing, K <: Label, L] protected[ast]
                       (val init :SQLExpression[F, S, I], val last :SQLExpression[F, S, L])
                       (implicit keyLiteral :ValueOf[K])
	extends AbstractInlineBinaryCompositeSQL[F, S, I, L, I |~ (K :~ L)]
	//this one is not strictly needed here, but we want it mixed in before BinaryCompositeTemplate in NonEmptyRecordSQL, NonEmptyIndexedSQL
	   with CompositeSQLTemplate[F, S, I |~ (K :~ L), ({ type E[-f <: RowProduct] = SQLExpression[f, S, I |~ (K :~ L)] })#E]
	   with BinaryCompositeSQL[F, S, I, L, I |~ (K :~ L)]
//	   with BinaryCompositeTemplate[F, S, I, L, I |~ (K :~ L),
//	                                ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = LabeledSQL[f, s, I, K, L]})#E]
	   with SelectableSQL[F, S, I |~ (K :~ L)]
{
	protected[ast] def this(left :SQLExpression[F, S, I], key :K, right :SQLExpression[F, S, L]) =
		this(left, right)(new ValueOf(key))

	def lastKey :K = valueOf[K]

	//todo: review where do we want to use NonSpecificSQL and apply them surgically
	protected override def left  :SQLExpression[F, S, I] = init
	protected override def right :SQLExpression[F, S, L] = NonSpecificSQL(last)

	override lazy val selectForm    :SQLReadForm[I |~ (K :~ L)] = init.selectForm |~ :~[K](last.selectForm)
	override lazy val universalForm :Opt[SQLForm[I |~ (K :~ L)]] =
		for (l <- init.universalForm; r <- last.universalForm) yield l |~ :~[K](r)

/*
	protected override def reform(reordering :Rearrangement)
	                             (implicit spelling :SQLSpelling) :SQLExpression[F, S, I |~ (K :~ L)] =
	{
		val lastColumns = spelling.potentialColumns(last, MayReform).columns.length
		val initColumns = reordering.columnCount - lastColumns
		try {
			//Hopes to reform left and right independently, without changing label order on this level
			val (initOrder, lastOrder) = reordering.splitAt(initColumns + 1)
			LabeledSQL(spelling.reorder(init, initOrder), lastKey, spelling.reorder(last, lastOrder))
		} catch {
			case e :Exception =>
				def error(e2 :Exception) = {
					val e3 = new IllegalArgumentException(
						"Cannot reorder `" + this + "` according to " + reordering + ": " + e.getMessage, e
					)
					e3.addSuppressed(e2)
					e3
				}
				try {
					split //make sure we can spell each column independently, so exploded spelling is possible
					new RearrangedSQL(this, reordering)
				} catch {
					case e2 :InseparableExpressionException => throw error(e2)
					case e2 :UndefinedShapeException => throw error(e2)
				}
		}
	}
*/

	protected override def leftValue(value :I |~ (K :~ L)) :I = value.init
	protected override def rightValue(value :I |~ (K :~ L)) :L = value.last.value
	protected override def value(left :I, right :L) :I |~ (K :~ L) = left |~ (lastKey :K) :~ right

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, I], right :SQLExpression[E, C, L])
			:SQLExpression[E, C, I |~ (K :~ L)] =
		LabeledSQL(left, lastKey, right)

	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[I |~ (K :~ L)] =
		spelling.effectiveForm(init) |~ (lastKey :K) :~ spelling.effectiveForm(last)


	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, I |~ (K :~ L), R]) :R =
		visitor.labeled(this)

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, R]) :R[S, I |~ (K :~ L)] =
		visitor.labeled(this)


	override def toString :String = left.toString + " |~ (" + lastKey + ":~" + last + ")"
}




object LabeledSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, I <: Listing, K <: Label, L]
	         (init :SQLExpression[F, S, I], key :K, last :SQLExpression[F, S, L]) :LabeledSQL[F, S, I, K, L] =
		LabeledSQL[F, S, I, K, L](init, :~[K](last))(new ValueOf(key))

	def apply[F <: RowProduct, S >: Grouped <: Single, I <: Listing, K <: Label :ValueOf, L]
	         (init :SQLExpression[F, S, I], entry :K :~ SQLExpression[F, S, L]) :LabeledSQL[F, S, I, K, L] =
	{
		entry.value match {
			case column :LabeledColumnSQL[F, S, _, L] if column.alias != valueOf[K] =>
				throw new IllegalArgumentException(
					"Cannot add a LabeledColumnSQL `" + column + "` to record `" + this +
						"` under a new label '" + valueOf[K] + "'."
				)
			case _ =>
		}
		init match {
			case index  :IndexedSQL[F, S, I] => entry.value match {
				case value :LabeledValueSQL[F, S, L] => new NonEmptyIndexedSQL(index, value)
				case value => new NonEmptyRecordSQL(index, value)
			}
			case record :RecordSQL[F, S, I] =>
				new NonEmptyRecordSQL(record, entry.value)
			case _ =>
				new LabeledSQL[F, S, I, K, L](init, entry.value) with RowShapeCache
//				new LabeledSQL[F, S, I, K, L](init, entry.value)
//					with InlineBinaryCompositeSQL[F, S, I, L, I |~ (K :~ L)] with RowShapeCache
		}
	}

	type __ = LabeledSQL[_ <: RowProduct, _ >: Grouped <: Single, _ <: Listing, _ <: Label, _]



	private[ast] type SplitListing[XI <: Listing, K <: Label, XL, YI <: Listing, YL, Z,
	                               R[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, Z]]
	                                 <: SQLExpression[f, s, Z]] =
		(XI sql_=> YI, XL sql_=> YL, SQLTransformation.Into[YI |~ (K :~ YL), Z, R])

	/** Splits a transformation of a non-empty chain into transformations for its `init` and `last`.
	  * If the method returns `Got`, then the last conversion is actually identity.
	  */
	private[ast] def splitListingTransformation[XI <: Listing, K <: Label, XL, Z]
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
					splitListingTransformation(first) match {
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
									splitListingTransformation(conversion andThen second) match {
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



	trait SpecificLabeledVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends SpecificEmptyVisitor[X, Y] {
		def labeled[I <: Listing, K <: Label, L](e :LabeledSQL[F, S, I, K, L])
		                                        (implicit isListing :X =:= (I |~ (K :~ L))) :Y
	}
	//or should match extend CaseSpecificRecord?
	type MatchSpecificLabeled[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificLabeledVisitor[F, S, X, Y]

	trait CaseSpecificLabeled[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificLabeled[F, S, X, Y] with CaseSpecificRecord[F, S, X, Y]
	{
		override def record[V <: Listing](e :RecordSQL[F, S, V])(implicit isListing :X =:= V) :Y =
			e match {
				case labeled :LabeledSQL[F, S, i, k, l] => this.labeled(labeled)
				case EmptySQL => empty(isListing.asInstanceOf[X =:= @~])
			}
	}

	trait AnyLabeledVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnyEmptyVisitor[Y] {
		//todo: make a common mixin CaseAnyXxx with MatchAnyRecord
		def labeled[S >: Grouped <: Single, I <: Listing, K <: Label, L]
		           (e :LabeledSQL[F, S, I, K, L]) :Y[S, I |~ (K :~ L)]
	}
	type MatchAnyLabeled[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyLabeledVisitor[F, Y]

	trait CaseAnyLabeled[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyLabeled[F, Y] with CaseAnyRecord[F, Y] //extends a protected trait
	{
//		override def emptyLabeled :Y[Single, @~] = empty
		override def record[S >: Grouped <: Single, X <: Listing](e :RecordSQL[F, S, X]) :Y[S, X] =
			(e match {
				case labeled :LabeledSQL[F, S, i, k, l] => this.labeled(labeled)
				case EmptySQL => empty
			}).castFrom[Y[S, _], Y[S, X]]
	}

}






/** A record is an expression with [[net.noresttherein.oldsql.collection.Listing Listing]] value type,
  * allowing access to subexpressions associated with particular labels.
  * @tparam F $F
  * @tparam S $S
  * @tparam V $V
  * @define This `RecordSQL`
  * @define this record
  */
sealed trait RecordSQL[-F <: RowProduct, -S >: Grouped <: Single, V <: Listing]
	extends InlineSQL[F, S, V]
	   with RecordSQLTemplate[F, S, V, SQLExpression, RecordSQL]
//	   with ReorderingTemplate[F, S, V, RecordSQL[F, S, V]]
//	   with SelectableMappingSQL[F, IndexedMapping.of[V]#Mapping]
{
	override def apply[K <: Label](key :K)(implicit get :RecordField[V, K]) :SQLExpression[F, S, get.Value] = get(this)

	def |~[E <: F, O >: Grouped <: S, K <: Label, L]
	       (entry :LabeledColumnSQL[E, O, K, L]) :RecordSQL[E, O, V |~ (K :~ L)] =
		new NonEmptyRecordSQL(this, entry.alias, entry.value)

	def |~[E <: F, O >: Grouped <: S, K <: Label :ValueOf, L]
	      (entry :K :~ SQLExpression[E, O, L]) :RecordSQL[E, O, V |~ (K :~ L)] =
		entry.value match {
			case column :LabeledColumnSQL[E, O, _, L] if column.alias == valueOf[K] =>
				new NonEmptyRecordSQL(this, entry.value)
			case column :LabeledColumnSQL[E, O, _, L] =>
				throw new IllegalArgumentException(
					"Cannot add a LabeledColumnSQL `" + column + "` to record `" + this +
						"` under a new label '" + valueOf[K] + "'."
				)
			case value =>
				new NonEmptyRecordSQL(this, value)
		}

	def |~[E <: F, O >: Grouped <: S, K <: Label, L]
	      (entry :(K, SQLExpression[E, O, L])) :RecordSQL[E, O, V |~ (K :~ L)] =
		(this |~ :~[K](entry._2))(new ValueOf(entry._1))

	def |~[E <: F, O >: Grouped <: S, K <: Label, L](entry :RecordItem[E, O, K, L]) :RecordSQL[E, O, V |~ (K :~ L)] =
		new NonEmptyRecordSQL(this, entry.key, entry.value)

//	def nullSQL   :RecordSQL[RowProduct, Single, V]
//	def nullValue :Opt[V]

	override def reorder[K <: Chain](implicit order :ShuffleRecord[V, K]) :RecordSQL[F, S, order.Out] = order(this)
	override def reorder[K <: Chain](keys :K)(implicit order :ShuffleRecord[V, K]) :RecordSQL[F, S, order.Out] =
		order(this)

	override def project[K <: Chain](implicit project :ProjectRecord[V, K]) :RecordSQL[F, S, project.Out] = project(this)
	override def project[K <: Chain](keys :K)(implicit project :ProjectRecord[V, K]) :RecordSQL[F, S, project.Out] =
		project(this)

	//todo: conjunction and disjunction of two expressions (one each, or ordered and non ordered?)
//	def reform[K <: Listing](implicit reform :RecordReform[V, K]) :IndexedSQL[F, S, reform.Out] = reform(this)
//	def reform[K <: Listing]()

/*
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
*/


	protected override def potentialColumns(permissions :Permissions)(implicit spelling :SQLSpelling) :AlignableColumns =
		AlignableColumns(this, permissions)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] =
		visitor.record(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y = visitor.record(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F, S, V] <: SQLExpression[F_, S_, V],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, V, E] =
//		visitor.labeled(this)

	override def toString :String = {
		def rec(e :RecordSQL[_, _, _], res :StringBuilder) :StringBuilder = e match {
			case tuple :NonEmptyRecordTemplate[_, _, _, _, _, SQLExpression, RecordSQL] =>
				rec(tuple.init, res) ++= " |~ " ++= tuple.lastKey ++= ":~"
				tuple.last match {
					case r :RecordSQL[_, _, _] => rec(r, res += '(') += ')'
					case t :ChainSQL[_, _, _, _] => res += '(' ++= t.toString += ')'
					case other => res ++= other.toString
				}
			case _ => res ++= "@~"
		}
		rec(this, new StringBuilder).toString
	}

}




//does not extend ChainTuple because LabeledEntry doesn't extend ChainEntry and it would cause problems
// in pattern matching a ChainTuple
/** A variant of [[net.noresttherein.oldsql.sql.ast.ChainTuple ChainTuple]] which maps
  * to a [[net.noresttherein.oldsql.collection.Listing Listing]] - a `Chain` subtype with key-value pairs
  * in the form of `K :~ V` as its only elements. The keys exist only on the type level in the `T` type parameter;
  * Member SQL expressions are not of the `K :~ V` type, but rather directly the value type `V`.
  * The indexing has no special effect on the generated SQL other than `ListingColumn` being a subclass of
  * [[net.noresttherein.oldsql.sql.ast.AliasedSQL AliasedColumn]]. Instead, it allows referencing
  * the columns in a type safe way when mapping results of a select, if this expression appears
  * in the ''select'' clause. Additionally, a ''select'' clause consisting solely of this type can be matched
  * to any [[net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping IndexedSchemaMapping]] with a component chain
  * containing exactly the same key-value pairs as the `T` type parameter, regardless of their order.
  * Non-column components of the mapping must in that case recursively match a nested indexed tuple paired
  * with the same key type in this `LiteralChain` as the component's label.
  * An `IndexedSQL` can consist only of expressions implementing `LabeledValueSQL` - a sealed type
  * extended only by [[net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledColumnSQL LabeledColumnSQL]]
  * and this trait. This ensures that every column of the whole expression is assigned a unique key,
  * which however may be a sequence of `Label`s, rather than a single one, if this expression contains
  * another indexed tuples as its subexpressions.
  * Note that, in order to provide unambiguous cases when pattern matching, this type does not extend the standard
  * `ChainTuple`.
  * @define This `RecordSQL`
  * @define this record
  */
object RecordSQL {
	def apply() :RecordSQL[RowProduct, Single, @~] = EmptySQL

	def apply[F <: RowProduct, S >: Grouped <: Single, K <: Label, T]
	         (item :LabeledColumnSQL[F, S, K, T]) :RecordSQL[F, S, @~ |~ (K :~ T)] =
		IndexedSQL(item)

	def apply[F <: RowProduct, S >: Grouped <: Single, K <: Label :ValueOf, T]
	         (item :K :~ SQLExpression[F, S, T]) :RecordSQL[F, S, @~ |~ (K :~ T)] =
		EmptySQL |~ item

	def apply[F <: RowProduct, S >: Grouped <: Single, K <: Label, T]
	         (item :RecordItem[F, S, K, T]) :RecordSQL[F, S, @~ |~ (K :~ T)] =
		RecordSQL(item.key, item.value)

	def apply[F <: RowProduct, S >: Grouped <: Single, K <: Label, T]
	         (key :K, value :SQLExpression[F, S, T]) :RecordSQL[F, S, @~ |~ (K :~ T)] =
		(EmptySQL |~ :~[K](value))(new ValueOf(key))


	/** [[net.noresttherein.oldsql.sql.ast.InlineSQL.Item Item]] type used by
	  * [[net.noresttherein.oldsql.sql.ast.RecordSQL RecordSQL]] and a base type for
	  * [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]]`.`[[net.noresttherein.oldsql.sql.ast.IndexedSQL.IndexItem Item]].
	  */
	class RecordItem[-F <: RowProduct, -S >: Grouped <: Single, K <: Label, X]
	                (override val index :Int, val key :K, override val value :SQLExpression[F, S, X])
		extends InlineItem[F, S, X]
	{
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :RecordItem[_, _, _, _] if other canEqual this =>
				index == other.index && key == other.key && value == other.value
			case _ => false
		}
		override lazy val hashCode :Int = (index.hashCode * 31 + key.hashCode) * 31 + value.hashCode
		override def toString :String = "(#" + index + ")"  + key + ": " + value
	}

	type __ = RecordSQL[_ <: RowProduct, _ >: Grouped <: Single, _]

	/** A type alias of [[net.noresttherein.oldsql.sql.ast.RecordSQL RecordSQL]] used in cases
	  * when the actual value of the expression does not matter, but only its type and, in particular, structure.
	  * It is closely related to both [[net.noresttherein.oldsql.sql.RowShape RowShape]]
	  * and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], but enforces an additional level of compatibility
	  * coming from the `Listing` type, especially its keys, in order to prevent accidental shape matching
	  * of otherwise incompatible expressions.
	  */
	type RecordShape[V <: Listing] = RecordSQL[Nothing, Grouped, V]

//	final val EmptyRecord :RecordSQL[RowProduct, Single, @~] = EmptySQL



	/** A template trait for [[net.noresttherein.oldsql.sql.ast.RecordSQL RecordSQL]] and its subtype,
	  * [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]].
	  */
	trait RecordSQLTemplate
	      [-F <: RowProduct, -S >: Grouped <: Single, V <: Listing,
           +Val[-f <: RowProduct, -s >: Grouped <: Single, v]
                <: SQLExpression[f, s, v] with GroundingOps[f, s, v, ({ type E[-E <: RowProduct] = Val[E, s, v] })#E, Val[f, s, v]],
           +Same[-f <: RowProduct, -s >: Grouped <: Single, v <: Listing]
           //We don't need RecordSQL bound, but it makes RecordSQLTemplate[F, S, V, SQLExpression, RecordSQL] a bound.
                 <: RecordSQL[f, s, v] with RecordSQLTemplate[f, s, v, Val, Same]]
		extends InlineSQL[F, S, V]
		   with CompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = Same[f, S, V] })#E]
		   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = Same[f, S, V] })#E]
	{ this :Same[F, S, V] with RecordSQLTemplate[F, S, V, Val, Same] =>

		type LastKey <: Label
		type LastValue
		override type Item[-E <: RowProduct, -D >: Grouped <: Single, X] = LabeledItem[E, D, _ <: Label, X]
		type LabeledItem[-E <: RowProduct, -D >: Grouped <: Single, K <: Label, X] <: RecordItem[E, D, K, X]

		def init(implicit nonEmpty :SplitRecord[V]) :Same[F, S, nonEmpty.Init] =
			this.castFrom[Same[F, S, V], NonEmptyRecordTemplate[F, S, nonEmpty.Init, Label, Any, Val, Same]].init

		def last(implicit nonEmpty :SplitRecord[V]) :Val[F, S, nonEmpty.Value] =
			this.castFrom[Same[F, S, V], NonEmptyRecordTemplate[F, S, Listing, Label, nonEmpty.Value, Val, Same]].last

		def lastKey(implicit nonEmpty :SplitRecord[V]) :nonEmpty.Key =
			this.castFrom[Same[F, S, V], NonEmptyRecordTemplate[F, S, Listing, nonEmpty.Key, Any, Val, Same]].lastKey

		def lastItem(implicit nonEmpty :SplitRecord[V]) :LabeledItem[F, S, nonEmpty.Key, nonEmpty.Value]
//			this.castFrom[Same[F, S, V], NonEmptyRecordTemplate[F, S, Listing, K, L, Val, Same]].item

		//Note that this doesn't override the method in LabeledValueSQL, as it uses RecordField, not RecordPath
		// (we can accept a RecordSQL as an argument only if we know V <: Listing, which is not the case in LabeledValueSQL).
		def apply[K <: Label](key :K)(implicit get :RecordField[V, K]) :Val[F, S, get.Value] //= get(this)


		def |~[E <: F, O >: Grouped <: S, K <: Label, L]
		      (entry :LabeledColumnSQL[E, O, K, L]) :Same[E, O, V |~ (K :~ L)]

		/* It is tempting to have a polymorphic append with this.LabeledItem, which is possible to declare here, but
		 *   1. actual method definitions could clash;
		 *   2. the method couldn't be supported by EmptySQL
		 *   3. init.LabeledItem is not the same as this.LabeledItem, so we can't append this.LabeledItem to init,
		 *      which would be the most important use case.
		 */
//		def |~[E <: F, O >: Grouped <: S, K <: Label, L](entry :LabeledItem[E, O, K, L]) :Same[E, O, V |~ (K :~ L)]

		override def toSeq :Seq[Val[F, S, _]] =
			throw new NotImplementedError("RecordSQLTemplate.toSeq should be overridden.")

		//we could make it Unique to ensure uniqueness and allow searching for position
		/** A list of (direct) keys/entry labels in this $this, in order from left to right. */
		def keys :IndexedSeq[Label] = {
			@tailrec def rec(e :RecordSQL[F, S, _], acc :PassedArray[Label] = PassedArray.empty) :IndexedSeq[Label] =
				e match {
					case entry :NonEmptyRecordTemplate[F, S, _, _, _, SQLExpression, RecordSQL] =>
						rec(entry.init, entry.lastKey +: acc)
					case _ => acc
				}
			rec(this)
		}

//		override def toSeq   :Seq[Val[F, S, _]] = parts
		//lets not change inOrder behaviour from LabeledSQL
//		override def inOrder :Seq[Val[F, S, _]] = parts
//		protected override def parts :Seq[Val[F, S, _]]
/*
		{
			@tailrec def rec(e :Same[F, S, _], acc :PassedArray[Val[F, S, _]] = PassedArray.empty) :Seq[Val[F, S, _]] =
				e match {
					case entry :NonEmptyRecordTemplate[F, S, _, _, _, Val, Same] @unchecked =>
						rec(entry.init, entry.last +: acc)
					case _ => acc
				}
			rec(this)
		}
*/
		/** Map of entries in this $this: keys mapped to their values. The key set of the result equals
		  * `this.`[[net.noresttherein.oldsql.sql.ast.RecordSQL.RecordSQLTemplate.keys keys]]`.toSet`.
		  * @see [[net.noresttherein.oldsql.sql.ast.InlineSQL.toSeq toSeq]]
		  * @see [[net.noresttherein.oldsql.sql.ast.IndexedSQL.index IndexedSQL.index]]
		  */
		def toMap :Map[String, Val[F, S, _]]
/*
		{
			@tailrec def rec(e :Same[F, S, _], acc :Map[String, Val[F, S, _]]) :Map[String, Val[F, S, _]] =
				e match {
					case entry :NonEmptyRecordTemplate[F, S, _, _, _, Val, Same] @unchecked =>
						if (acc.contains(entry.key))
							throw new IllegalStateException("Duplicate keys in expression " + this + ".")
						rec(entry.init, acc.updated(entry.key, entry.last))
					case _ => acc
				}
			rec(this, Map.empty)
		}
*/


		override def asSingleRow :Option[Same[F, Single, V]] =
			if (isSingleRow) Some(this.castFrom[Same[F, S, V], Same[F, Single, V]])
			else None

//		def nullSQL   :RecordSQL[RowProduct, Single, V]
//		def nullValue :Opt[V]

		//todo: try to implement them here
		def reorder[K <: Chain](implicit order :ShuffleRecord[V, K]) :Same[F, S, order.Out] //= order(this)
		def reorder[K <: Chain](keys :K)(implicit order :ShuffleRecord[V, K]) :Same[F, S, order.Out] //= order(this)

		def project[K <: Chain](implicit project :ProjectRecord[V, K]) :Same[F, S, project.Out] //= project(this)
		def project[K <: Chain](keys :K)(implicit project :ProjectRecord[V, K]) :Same[F, S, project.Out] //=
//			project(this)

		@throws[IllegalArgumentException]("if keys is not a permutation of numbers [0..this.items.length).")
		override def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, S, V] =
			if (permutation == permutation.indices)
				this
			else if (permutation != permutation.sorted)
				throw new IllegalArgumentException(
					"Cannot reorder record `" + this + "` according to permutation " + permutation +
						" because it is of a wrong size: expected " + size + ", but got " + permutation.size + "."
				)
			else {
				val inverse = inversePermutation(permutation)
				val items = this.items
				val ourKeys = items.view.map(_.key :Label) to Unique
				val newKeys = inverse.view.map(ourKeys.apply) to Unique
				trustedReorder(ourKeys, newKeys)
			}

		/** Creates a $This expression by reordering the items of this listing to match the argument key sequence,
		  * and wraps it in a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]] which maps
		  * the new order back to the value type of this listing.
		  */ //this loses any null columns squeezed in
		@throws[IllegalArgumentException]("if keys is not a permutation of this listing's keys")
		def reorder(keys :Unique[String]) :SQLExpression[F, S, V] =
			if (items.view.map(_.key) == keys)
				this
			else if (items.length != keys.size)
				throw new IllegalArgumentException(
					"There are " + items.length + " items in record " + this + ", but the key permutation given " +
						keys + " has " + keys.size + " elements."
				)
			else {
				val newKeys = keys.downcastParam[Label]
				val ourKeys = items.view.map(_.key :Label).to(Unique)
				if (ourKeys.size != items.length)
					throw new IllegalStateException(
						"Cannot reorder record " + this + " to key order " + keys + " because it contains key duplicates."
					)
				if (ourKeys.toSet != keys.toSet)
					throw new IllegalArgumentException(
						"Cannot reorder record " + this + " to key order " + keys + " because the keys do not match."
					)
				trustedReorder(ourKeys, newKeys)
			}

		private def trustedReorder(ourKeys :Unique[Label], newKeys :Unique[Label]) :SQLExpression[F, S, V] = {
			def reorder(itemNo :Int) :RecordSQL[F, S, _ <: Listing] =
				if (itemNo < 0)
					RecordSQL()
				else {
					val init = reorder(itemNo - 1)
					val key = newKeys(itemNo)
					(init |~ :~[key.type](toMap(key)))(new ValueOf(key))
				}
			val reordered = reorder(ourKeys.size - 1).castFrom[RecordSQL[F, S, _], RecordSQL[F, S, Listing]]
			val convert = RecordReordering[Listing, Chain, V, Chain](
				ProjectRecord.unsafe(newKeys, ourKeys), ProjectRecord.unsafe(ourKeys, newKeys)
			)
			convert(reordered)
		}

		override def rebuild[E <: RowProduct, A >: Grouped <: Single]
	                        (items :({ type T[X] = Item[F, S, X] })#T =>: SQLExpression.from[E]#rows[A]#E)
				:RecordSQL[E, A, V]
	}


	/** Allows [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]] to extend sealed `RecordSQL`. */
	//todo: replace it with a sealing method pattern, as it makes matching EmptySQL | LabeledSQL non exhaustive
	private[ast] trait UnsealedRecordSQL[-F <: RowProduct, -S >: Grouped <: Single, V <: Listing]
		extends RecordSQL[F, S, V]



	class SplitRecord[V <: Listing] private extends Serializable {
		type Init <: Listing
		type Value
		type Key <: Label
	}
	object SplitRecord {
		implicit def nonEmpty[I <: Listing, K <: Label, L]
				:SplitRecord[I |~ (K :~ L)] { type Init = I; type Key = K; type Value = L } =
			prototype.asInstanceOf[SplitRecord[I |~ (K :~ L)] { type Init = I; type Key = K; type Value = L }]
		private[this] val prototype = new SplitRecord[Listing]
	}


	//There is a lot of duplication between RecordSQL and IndexedSQL methods, but its not as simple as only
	// substituting argument and return types from RecordSQL to IndexedSQL, but various accessors and factory methods
	// return/accept SQLException vs LabeledValueSQL.
	/** Implicit evidence used to project and reorder a [[net.noresttherein.oldsql.sql.ast.IndexedSQL record]]
	  * expression to another record, containing the specified fields.
	  * It witnesses that [[net.noresttherein.oldsql.sql.ast.RecordSQL.TransformRecord.Out Out]]
	  * is the result of taking fields at [[net.noresttherein.oldsql.schema.bits.LabelPath.Label keys]]
	  * (or [[net.noresttherein.oldsql.schema.bits.LabelPath key paths]] listed in `Ks` from a record of type `X`.
	  * @tparam X  $X
	  * @tparam Ks $Ks
	  * @define X  the input record.
	  * @define Ks a tuple of [[net.noresttherein.oldsql.schema.bits.LabelPath.Label Label]]s
	  *            or [[net.noresttherein.oldsql.schema.bits.LabelPath LabelPath]]s specifying the fields
	  *            of the input record, in case of the latter indirect fields of some its subrecord,
	  *            which should be taken to form the output record.
	  *  */
	trait TransformRecord[X <: Listing, Ks <: Chain] extends Serializable {
		type Out <: Listing
//		type OutSQL[-F <: RowProduct, -S >: Grouped <: Single] <: LabeledValueSQL[F, S, Out]
		def apply(record :X) :Out
//		def inverse(record :Out) :X
//		def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) :OutSQL[F, S]
		def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X])  :RecordSQL[F, S, Out]
		def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) :IndexedSQL[F, S, Out]
//		def inverse[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, Out]) :IndexedSQL[F, S, X]
	}


	/** Evidence that `Ks` is a tuple containing keys of all fields of record `X`, in any order. */
	@implicitNotFound("${Ks} does not form a complete list of keys in ${X}.")
	sealed abstract class ShuffleRecord[X <: Listing, Ks <: Chain] extends TransformRecord[X, Ks]

	object ShuffleRecord {
		implicit def reorder[X <: Listing, Ks <: Chain, N <: Numeral]
		                    (implicit project :ProjectRecord[X, Ks], inSize :ChainLength[X, N], outSize :ChainLength[Ks, N])
//		implicit def reorder[X <: Listing, XKs <: Chain, Y <: Listing, YKs <: Chain]
//		                    (implicit project :ProjectRecord[X, YKs] { type Out = Y }, keys :ToKeyChain[X, XKs],
//		                     back :ProjectRecord[Y, XKs])
				:ShuffleRecord[X, Ks] { type Out = project.Out } =
			new ShuffleRecord[X, Ks] {
				override type Out = project.Out
//				override type OutSQL[-F <: RowProduct, -S >: Grouped <: Single] = select.OutSQL[F, S]
				override def apply(record :X) = project(record)
//				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) =
//					select(record)
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X]) = project(record)
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) = project(record)

				override def toString = (project.toString(new StringBuilder ++= "RecordShuffle(") += ')').result()
			}
	}


	/** Implicit evidence that `this.`[[net.noresttherein.oldsql.sql.ast.RecordSQL.TransformRecord.Out Out]]
	  * is a projection of input record `X` to fields listed in `Ks`.
	  * @tparam X  $X
      * @tparam Ks $Ks
	  */
	@implicitNotFound("${Ks} contains keys not present in ${X} (or at least one of the types is abstract).")
	@implicitAmbiguous("${Ks} contain duplicate keys or at least one of the keys appears in ${X} more than once.")
	sealed abstract class ProjectRecord[X <: Listing, Ks <: Chain] extends TransformRecord[X, Ks] { outer =>
		type To[Res <: Listing] = ProjectRecord[X, Ks] { type Out = Res }
		override type Out <: Listing
//		override type OutSQL[-F <: RowProduct, -S >: Grouped <: Single] = IndexedSQL[F, S, Out]
		def size :Int

		def andThen[C <: Chain](projection :ProjectRecord[Out, C]) :ProjectRecord[X, C] { type Out = projection.Out } =
			new ProjectRecord[X, C] {
				override type Out = projection.Out
//				override type OutSQL[-F <: RowProduct, -S >: Grouped <: Single] = projection.OutSQL[F, S]
				override def size = projection.size

				override def apply(record :X) :Out = projection(outer(record))
//				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) =
//					projection(subRecord(record))

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X]) =
					projection(outer(record))

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) =
					projection(outer(record))

				override def toString(res :StringBuilder) = projection.toString(res)
			}

		protected[RecordSQL] def toString(res :StringBuilder) :StringBuilder
		override def toString :String = (toString(new StringBuilder ++= "RecordProjection(") += ')').result()
	}

	private[RecordSQL] sealed abstract class OutOfOrderProjections {
		implicit def lastValueAtKey[X <: Listing, Ks <: Chain, K <: Label]
		                           (implicit item :RecordField[X, K], preceding :ProjectRecord[X, Ks])
		        :ProjectRecord[X, Ks ~ K] { type Out = preceding.Out |~ (K :~ item.Value) } =
			new ProjectRecord[X, Ks ~ K] {
				override type Out = preceding.Out |~ (K :~ item.Value)
				override def size = preceding.size + 1
				override def apply(record :X) = preceding(record) |~ :~[K](item(record))

//				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) =
//					(preceding(record) |~ :~[K](item(record)))(new ValueOf[K](item.path(record)))

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X]) =
					(preceding(record) |~ :~[K](item(record)))(new ValueOf[K](item.key(record)))

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) =
					(preceding(record) |~ :~[K](item(record)))(new ValueOf[K](item.key(record)))

				override def toString(res :StringBuilder) = preceding.size match {
					case 0 => res += '0'
					case n => res += ',' ++= n.toString
				}
			}
	}

	object ProjectRecord extends OutOfOrderProjections {
		type SubRecordOf[Y <: Listing, X <: Listing] = ProjectRecord[X, _] { type Out = Y }

		implicit def emptyProjection[X <: Listing] :ProjectRecord[X, @~] { type Out = @~ } =
			new ProjectRecord[X, @~] {
				override type Out = @~
				override def size = 0
				override def apply(record :X): @~ = @~
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X]) = EmptySQL
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) = EmptySQL
//				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) =
//					EmptyIndex

				override def toString(res :StringBuilder) :StringBuilder = res
			}
		implicit def includeLast[X <: Listing, Ks <: Chain, K <: Label, V](implicit preceding :ProjectRecord[X, Ks])
				:ProjectRecord[X |~ (K :~ V), Ks ~ K] { type Out = preceding.Out |~ (K :~ V) } =
			new ProjectRecord[X |~ (K :~ V), Ks ~ K] {
				override type Out = preceding.Out |~ (K :~ V)
				override def size = preceding.size + 1
				override def apply(record :X |~ (K :~ V)) =
					preceding(record.init) |~ :~[K](record.last.value)
//
//				override def apply[F <: RowProduct, S >: Grouped <: Single]
//				                  (record :LabeledValueSQL[F, S, X |~ (K :~ V)]) :IndexedSQL[F, S, Out] =
//					(preceding(record.init) |~ :~[K](record.last))(new ValueOf[K](record.lastKey))

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X |~ (K :~ V)]) =
					(preceding(record.init) |~ :~[K](record.last))(new ValueOf[K](record.lastKey))

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X |~ (K :~ V)]) =
					(preceding(record.init) |~ :~[K](record.last))(new ValueOf[K](record.lastKey))

				override def toString(res :StringBuilder) :StringBuilder = preceding.toString(res)
			}
		implicit def excludeLast[X <: Listing, Ks <: Chain, K <: Label, V](implicit preceding :ProjectRecord[X, Ks])
				:ProjectRecord[X |~ (K :~ V), Ks ~ K] { type Out = preceding.Out } =
			new ProjectRecord[X |~ (K :~ V), Ks ~ K] {
				override type Out = preceding.Out
				override def size = preceding.size
				override def apply(record :X |~ (K :~ V)) = preceding(record.init)
//				override def apply[F <: RowProduct, S >: Grouped <: Single]
//				                  (record :LabeledValueSQL[F, S, X |~ (K :~ V)]) = preceding(record.init)

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X |~ (K :~ V)]) =
					preceding(record.init)

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X |~ (K :~ V)]) =
					preceding(record.init)

				override def toString(res :StringBuilder) = preceding.size match {
					case 0 => res += '0'
					case n => res += ',' ++= n.toString
				}
			}
		private[ast] def unsafe[X <: Listing, Ks <: Chain, Y <: Listing]
		                       (superOrder :Unique[String], subOrder :Unique[String]) :ProjectRecord[X, Ks] { type Out = Y } =
			new UntypedProjectRecord[X, Ks, Y](superOrder, subOrder)

		private class UntypedProjectRecord[X <: Listing, Ks <: Chain, Y <: Listing]
		                                  (val oldOrder :Unique[String], val newOrder :Unique[String])
			extends ProjectRecord[X, Ks]
		{
			type Out = Y
			override def size = oldOrder.size
			override def apply(index :X) :Y = {
				val entries = index.toIndexedSeq
				def rec(i :Int) :Listing =
					if (i < 0)
						@~
					else {
						val init = rec(i - 1)
						val key = newOrder(i)
						val entry = entries(oldOrder.sureIndexOf(key))
						init |~ entry
					}
				rec(newOrder.size - 1).asInstanceOf[Y]
			}

/*
			override def apply[F <: RowProduct, S >: Grouped <: Single](listing :LabeledValueSQL[F, S, X]) = {
				val items = listing.castFrom[LabeledValueSQL[F, S, X], IndexedSQL[F, S, X]].toMap
//				((EmptyIndex :IndexedSQL[F, S, _ <: Listing]) /: newOrder) {
//					(res, key) => res |~ (key :Label, items(key)) :IndexedSQL[F, S, _ <: Listing]
//				}.castFrom[IndexedSQL[F, S, _ <: Listing], IndexedSQL[F, S, Y]]
				var res :IndexedSQL[F, S, _] = EmptyIndex
				val i = newOrder.iterator
				while (i.hasNext) {
					val key = i.next()
					res = res |~ (key :Label, items(key))
				}
				res.castFrom[IndexedSQL[F, S, _], IndexedSQL[F, S, Y]]
			}
*/

			override def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X]) = {
				val items = record.castFrom[RecordSQL[F, S, X], RecordSQL[F, S, X]].toMap
				var res :RecordSQL[F, S, _] = EmptySQL
				val i = newOrder.iterator
				while (i.hasNext) {
					val key = i.next()
					res = res |~ (key :Label, items(key))
				}
				res.castFrom[RecordSQL[F, S, _], RecordSQL[F, S, Y]]
			}

			override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) = {
				val items = record.castFrom[IndexedSQL[F, S, X], IndexedSQL[F, S, X]].toMap
				var res :IndexedSQL[F, S, _] = EmptySQL
				val i = newOrder.iterator
				while (i.hasNext) {
					val key = i.next()
					res = res |~ (key :Label, items(key))
				}
				res.castFrom[IndexedSQL[F, S, _], IndexedSQL[F, S, Y]]
			}

//			override def inverse[F <: RowProduct, S >: Grouped <: Single](listing :IndexedSQL[F, S, X]) = {
//				val items = listing.toMap
//				(EmptyIndex.asInstanceOf[IndexedSQL[F, S, Listing]] /: oldOrder) { (res, key) =>
//					(items.get(key) match {
//						case Some(value) => res |~ (key, value)
//						case _ => res |~ LabeledColumnSQL(key, SQLNull[Any]())
//					}).asInstanceOf[IndexedSQL[F, S, Listing]]
//				}.asInstanceOf[IndexedSQL[F, S, Y]]
//			}
//
//			override def compose[O <: Listing](other :Y SublistingOf O) :X SublistingOf O = other match {
//				case untyped :UntypedSublistingOf[Y, O] =>
//					if (untyped.newOrder != oldOrder)
//						throw new IllegalArgumentException(
//							"Cannot compose (" + this + ") with (" + untyped + "): output key order " +
//								"of the second conversion does not match the input key order of the first one."
//						)
//					new UntypedSublistingOf(newOrder, untyped.oldOrder)
//				case _ => super.compose(other)
//			}
//
			override def andThen[C <: Chain](projection :ProjectRecord[Y, C])
					:ProjectRecord[X, C] { type Out = projection.Out } =
				projection match {
					case untyped :UntypedProjectRecord[_, _, _] =>
						new UntypedProjectRecord[X, C, projection.Out](oldOrder, untyped.newOrder)
					case _ =>
						super.andThen(projection)
				}

			override def toString(res :StringBuilder) =
				if (newOrder.isEmpty)
					res
				else {
					res append newOrder.head
					val i = newOrder.iterator
					i.next().foreach { res append ',' append _.toString }
					res
				}
		}
	}
//
//
//	@implicitNotFound("Keys ${Ks} do not appear in ${X} in the same order (or contain absent keys). " +
//	                  "Did you want to use SubRecord[${X}, ${Ks}] instead?")
//	@implicitAmbiguous("Either ${Ks} or ${X} contain duplicate keys.")
//	sealed abstract class RecordProjection[X <: Listing, Ks <: Chain] extends TransformRecord[X, Ks] {
//		override type Out <: Listing
//		override type OutSQL[-F <: RowProduct, -S >: Grouped <: Single] = IndexedSQL[F, S, X]
//	}
//
//	object RecordProjection {
//		implicit def emptyProjection[X <: Listing] :RecordProjection[X, @~] { type Out = @~ } =
//			new RecordProjection[X, @~] {
//				override type Out = @~
//				override def apply(record :X) = @~
//				override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) = EmptyIndex
//			}
//		implicit def includeLast[X <: Listing, Ks <: Chain, K <: Label, V](implicit preceding :RecordProjection[X, Ks])
//				:RecordProjection[X |~ (K :~ V), Ks ~ K] { type Out = preceding.Out |~ (K :~ V) } =
//			new RecordProjection[X |~ (K :~ V), Ks ~ K] {
//				override type Out = preceding.Out |~ (K :~ V)
//				override def apply(record :X |~ (K :~ V)) =
//					preceding(record.init) |~ :~[K](record.last.value)
//
//				override def apply[F <: RowProduct, S >: Grouped <: Single]
//				                  (record :IndexedSQL[F, S, X |~ (K :~ V)]) :IndexedSQL[F, S, Out] =
//					(preceding(record.init) |~ :~[K](record.last))(new ValueOf[K](record.lastKey))
//			}
//		implicit def excludeLast[X <: Listing, Ks <: Chain, K <: Label, V](implicit preceding :RecordProjection[X, Ks])
//				:RecordProjection[X |~ (K :~ V), Ks ~ K] { type Out = preceding.Out } =
//			new RecordProjection[X |~ (K :~ V), Ks ~ K] {
//				override type Out = preceding.Out
//				override def apply(record :X |~ (K :~ V)) = preceding(record.init)
//				override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X |~ (K :~ V)]) =
//					preceding(record.init)
//			}
//	}


	trait RecordUnification[X <: Listing, Y <: Listing] extends Serializable {
		type Out <: Listing
		type Keys <: Chain
		val left  :TransformRecord[X, Keys] { type Out = RecordUnification.this.Out }
		val right :TransformRecord[Y, Keys] { type Out = RecordUnification.this.Out }
	}

/*
	sealed abstract class RecordSum[X <: Listing, Y <: Listing] {
		type Out <: Listing
		type Keys <: Chain
		val left  :TransformRecord[X, Keys] { type Out = RecordSum.this.Out }
		val right :TransformRecord[Y, Keys] { type Out = RecordSum.this.Out }
	}

	private[RecordSQL] sealed abstract class SequentialRecordSum {
	}
	private[RecordSQL] sealed abstract class SubRecordSum {
		implicit def toRight[X <: Listing, Y <: Listing, Ks <: Chain]
		                    (implicit keys :RecordKeys[Y] { type Out = Ks }, subRecord :SubRecord[X, Ks] { type Out = Y })
				:RecordSum[X, Y] { type Out = Y; type Keys = Ks } =
			new RecordSum[X, Y] {
				override type Out  = Y
				override type Keys = Ks
				override val left  = subRecord
				override val right = identity.asInstanceOf[TransformRecord[Y, Ks] { type Out = Y }]
			}
		implicit def toLeft[X <: Listing, Y <: Listing, Ks <: Chain]
		                   (implicit keys :RecordKeys[X] { type Out = Ks }, subRecord :SubRecord[Y, Ks] { type Out = X })
				:RecordSum[X, Y] { type Out = X; type Keys = Ks } =
			new RecordSum[X, Y] {
				override type Out  = X
				override type Keys = Ks
				override val left  = identity.asInstanceOf[TransformRecord[X, Ks] { type Out = X }]
				override val right = subRecord
			}
		private val identity = new TransformRecord[Listing, Chain] {
			override type Out = Listing
			override def apply(record :Listing) :Listing = record
			override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, Listing]) = record
			override def toString = "TransformRecord(<identity>)"
		}
	}
	object RecordSum extends SubRecordSum {
		implicit def same[X <: Listing](implicit keys :RecordKeys[X])
				:RecordSum[X, X] { type Out = X; type Keys = keys.Out } =
			new RecordSum[X, X] {
				override type Out = X
				override type Keys = keys.Out
				override val left = new TransformRecord[X, Keys] {
					override type Out = X
					override def apply(record :X) = record
					override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) = record
				}
				override val right = left
			}
	}
*/



	@implicitNotFound("Path ${P} does not appear in ${X}.")
	@implicitAmbiguous("Multiple entries with path ${P} present in ${X}.")
	sealed abstract class RecordPath[X, P] extends Serializable {
		type Value
		type Updated[T] <: Listing
		def posFromTheRight :Int
		def isTopLevel :Boolean
		def apply(record :X) :Value
		def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) :LabeledValueSQL[F, S, Value]
		def path[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) :P

		def set(record :X, value :Value) :X

		def set[F <: RowProduct, S >: Grouped <: Single]
		       (record :LabeledValueSQL[F, S, X], value :LabeledValueSQL[F, S, Value]) :LabeledValueSQL[F, S, X]

		protected def evidencePath :String
		override def toString :String = "RecordPath(" + evidencePath + ")"
	}

	@implicitNotFound("Record ${X} does not have field {K}.")
	@implicitAmbiguous("Multiple fields ${K} present in ${X}.")
	sealed abstract class RecordField[X <: Listing, K] extends RecordPath[X, K] {
		override def isTopLevel :Boolean = true

		def key[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X]) :K

		def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X]) :SQLExpression[F, S, Value]
		def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) :LabeledValueSQL[F, S, Value] =
			apply(record :LabeledValueSQL[F, S, X])

		def set[F <: RowProduct, S >: Grouped <: Single]
		       (record :RecordSQL[F, S, X], value :SQLExpression[F, S, Value]) :RecordSQL[F, S, X]

		def set[F <: RowProduct, S >: Grouped <: Single] //overloading ambigouity with RecordSQL
		       (record :IndexedSQL[F, S, X], value :LabeledValueSQL[F, S, Value]) :IndexedSQL[F, S, X]

		protected override def evidencePath :String = "-" + posFromTheRight
	}

	object RecordPath {
		implicit def lastItem[X <: Listing, K <: Label, V]
				:RecordField[X |~ (K :~ V), K] { type Value = V; type Updated[T] = X |~ (K :~ T) } =
			new RecordField[X |~ (K :~ V), K] {
				override type Value = V
				override type Updated[T] = X |~ (K :~ T)
				override def posFromTheRight = 0

				override def apply(record :X |~ (K :~ V)) :V = record.last.value

				override def apply[F <: RowProduct, S >: Grouped <: Single]
				                  (record :LabeledValueSQL[F, S, X |~ (K :~ V)]) :LabeledValueSQL[F, S, V] =
					record.last

				override def apply[F <: RowProduct, S >: Grouped <: Single]
				                  (record :RecordSQL[F, S, X |~ (K :~ V)]) :SQLExpression[F, S, V] =
					record.last

				override def path[F <: RowProduct, S >: Grouped <: Single]
				                 (record :LabeledValueSQL[F, S, X |~ (K :~ V)]) :K =
					record.lastKey

				override def key[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X |~ (K :~ V)]) :K =
					record.lastKey

				override def set(record :X |~ (K :~ V), value :V) :X |~ (K :~ V) =
					record.init |~ :~[K](record.last.value)

				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :LabeledValueSQL[F, S, X |~ (K :~ V)], value :LabeledValueSQL[F, S, V]) =
					(record.init |~ :~[K](record.last))(new ValueOf(record.lastKey))

				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :RecordSQL[F, S, X |~ (K :~ V)], value :SQLExpression[F, S, V]) =
					(record.init |~ :~[K](record.last))(new ValueOf(record.lastKey))

				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :IndexedSQL[F, S, X |~ (K :~ V)], value :LabeledValueSQL[F, S, V]) =
					(record.init |~ :~[K](record.last))(new ValueOf(record.lastKey))
			}

		//todo: I don't know if Scala can infer these types.
		implicit def previousItem[X <: Listing, K <: Label, A <: Label, B](implicit preceding :RecordField[X, K])
				:RecordField[X |~ (A :~ B), K] {
					type Value = preceding.Value; type Updated[T] = preceding.Updated[T] |~ (A :~ B)
				} =
			new RecordField[X |~ (A :~ B), K] {
				override type Value = preceding.Value
				override type Updated[T] = preceding.Updated[T] |~ (A :~ B)
				override def posFromTheRight = preceding.posFromTheRight + 1

				override def apply(record :X |~ (A :~ B)) :Value = preceding(record.init)
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X |~ (A :~ B)]) =
					preceding(record.init)

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X |~ (A :~ B)]) =
					preceding(record.init)

				override def path[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X |~ (A :~ B)]) :K =
					preceding.path(record.init)

				override def key[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X |~ (A :~ B)]) :K =
					preceding.key(record.init)

				override def set(record :X |~ (A :~ B), value :preceding.Value) :X |~ (A :~ B) =
					preceding.set(record.init, value) |~ record.last

				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :LabeledValueSQL[F, S, X |~ (A :~ B)], value :LabeledValueSQL[F, S, Value]) =
					preceding.set(record.init, value) |~ record.lastItem

				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :RecordSQL[F, S, X |~ (A :~ B)], value :SQLExpression[F, S, Value]) =
					preceding.set(record.init, value) |~ record.lastItem

				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :IndexedSQL[F, S, X |~ (A :~ B)], value :LabeledValueSQL[F, S, Value]) =
					preceding.set(record.init, value) |~ record.lastItem
			}

		implicit def nestedItem[X <: Listing, P <: LabelPath[P], H <: Label, T, V <: Listing]
		                       (implicit split :LabelPath.Split[P] { type First = H; type Suffix = T },
		                        topLevel :RecordPath[X, H] { type Value = V }, nested :RecordPath[V, T])
				:RecordPath[X, P] {
					type Value = nested.Value; type Updated[A] = topLevel.Updated[nested.Updated[A]]
				} =
			new RecordPath[X, P] {
				override type Value = nested.Value
				override type Updated[A] = topLevel.Updated[nested.Updated[A]]
				override def posFromTheRight = topLevel.posFromTheRight
				override def isTopLevel = false

				override def apply(record :X) :Value = nested(topLevel(record))
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) =
					nested(topLevel(record))

				override def path[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) =
					split.join(topLevel.path(record), nested.path(topLevel(record))).path

				override def set(record :X, value :Value) = topLevel.set(record, nested.set(topLevel(record), value))
				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :LabeledValueSQL[F, S, X], value :LabeledValueSQL[F, S, Value]) =
					topLevel.set(record, nested.set(topLevel(record), value))

				override def evidencePath = topLevel.evidencePath + "/" + nested.evidencePath
			}
	}


	@implicitNotFound("Cannot determine the full list of keys for ${X} - is the type partially abstract?")
	sealed abstract class RecordKeys[X <: Listing] {
		type Out <: Chain
		def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X]) :Out
	}

	object RecordKeys {
		implicit val emptyRecordKeys :RecordKeys[@~] { type Out = @~ } =
			new RecordKeys[@~] {
				override type Out = @~
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, @~]) = @~
			}
		implicit def nonEmptyRecordKeys[X <: Listing, K <: Label](implicit preceding :RecordKeys[X])
				:RecordKeys[X |~ (K :~ Any)] { type Out = preceding.Out ~ K } =
			new RecordKeys[X |~ (K :~ Any)] {
				override type Out = preceding.Out ~ K
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledValueSQL[F, S, X |~ (K :~ Any)]) =
					preceding(record.init) ~ record.lastKey
			}
	}




	//attempt to recreate a RecordSQL by casting down the expressions obtained by reforming init and last
	private[ast] def attempt[F <: RowProduct, S >: Grouped <: Single, I <: Listing, K <: Label, L]
	                 (i :SQLExpression[F, S, I], key :K, l :SQLExpression[F, S, L]) :Opt[RecordSQL[F, S, I |~ (K :~ L)]] =
		i match {
			case record :RecordSQL[F, S, I] => Got((record |~ :~[K](l))(new ValueOf(key)))
			case _ => Lack
		}



	trait SpecificRecordVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificIndexedVisitor[F, S, X, Y]
	{
		def record[V <: Listing](e :RecordSQL[F, S, V])(implicit isListing :X =:= V) :Y
	}
	trait MatchSpecificRecord[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificRecordVisitor[F, S, X, Y] with CaseSpecificIndexed[F, S, X, Y]
	{
		def emptyRecord(implicit ev :X =:= @~) :Y
		def recordItem[I <: Listing, K <: Label, L]
		               (e :RecordSQL[F, S, I |~ (K :~ L)])(implicit isListing :X =:= (I |~ (K :~ L))) :Y

		override def record[V <: Listing](e :RecordSQL[F, S, V])(implicit isListing :X =:= V) :Y =
			e match {
				case tuple :NonEmptyRecordTemplate[F, S, i, k, l, SQLExpression, RecordSQL] =>
					recordItem(tuple)(isListing.asInstanceOf[X =:= (i |~ (k :~ l))])
				case _ => emptyRecord(isListing.castParam2[@~])
			}
		override def indexed[V <: Listing](e :IndexedSQL[F, S, V])(implicit isListing :X =:= V) :Y = record(e)
	}
	trait CaseSpecificRecord[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificRecordVisitor[F, S, X, Y] with CaseSpecificIndexed[F, S, X, Y]
	{
		override def indexed[V <: Listing](e :IndexedSQL[F, S, V])(implicit isListing :X =:= V) :Y = record(e)
	}


	trait AnyRecordVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnyIndexedVisitor[F, Y] {
		def record[S >: Grouped <: Single, V <: Listing](e :RecordSQL[F, S, V]) :Y[S, V]
	}

	trait MatchAnyRecord[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyRecordVisitor[F, Y] with CaseAnyIndexed[F, Y]
	{
		def emptyRecord :Y[Single, @~]
        def recordItem[S >: Grouped <: Single, I <: Listing, K <: Label, L]
		               (e :RecordSQL[F, S, I |~ (K :~ L)]) :Y[S, I |~ (K :~ L)]

		override def record[S >: Grouped <: Single, V <: Listing](e :RecordSQL[F, S, V]) :Y[S, V] =
			(e match {
				case tuple :NonEmptyRecordTemplate[F, S, i, k, l, SQLExpression, RecordSQL] =>
					recordItem(tuple)
				case _ =>
					emptyRecord
			}).castFrom[Y[S, _], Y[S, V]]

		override def indexed[S >: Grouped <: Single, V <: Listing](e :IndexedSQL[F, S, V]) :Y[S, V] = record(e)
	}
	trait CaseAnyRecord[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyRecordVisitor[F, Y] with CaseAnyIndexed[F, Y]
	{
		override def indexed[S >: Grouped <: Single, V <: Listing](e :IndexedSQL[F, S, V]) :Y[S, V] = record(e)
	}

}




/* The problem with padding a RecordSQL with null columns is that it would be lost whenever items/toSeq is used
 * to somehow represent all columns of the record, which would lose all padding. An alternative is to pad
 * record elements instead. However, in that case either LabeledValueSQL would have to have a new subtype,
 * and that subtype would have to be public, or we wouldn't be able to pad individual columns
 * (because ColumnSQL extends ReorderingTemplate, returning a ColumnSQL from reform(Rearrangement).
 * We should preserve the IndexedSQL type by the latter method, in order to preserve a SelectAs for an IndexedSQL.
 */
/*
private trait NullPaddedRecordSQLTemplate
              [-F <: RowProduct, -S >: Grouped <: Single, V <: Listing,
               +Val[-f <: RowProduct, -s >: Grouped <: Single, v]
                   <: SQLExpression[f, s, v] with GroundingOps[f, s, v, ({ type E[-E <: RowProduct] = Val[E, s, v] })#E, Val[f, s, v]],
               +Same[-f <: RowProduct, -s >: Grouped <: Single, v <: Listing]
                    <: RecordSQL[f, s, v] with RecordSQLTemplate[f, s, v, Val, Same]]
	extends BinaryCompositeTemplate[F, S, V, Null, V,
	                                ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, V] })#E]
	   with RecordSQLTemplate[F, S, V, Val, Same]
{ self :Same[F, S, V] with NullPaddedRecordSQLTemplate[F, S, V, Val, Same] =>
	override type LastKey   = left.LastKey
	override type LastValue = left.LastValue
	override type LabeledItem[-f <: RowProduct, -s >: Grouped <: Single, k <: Label, v] = left.LabeledItem[f, s, k, v]

	override val left  :Same[F, S, V]
//	override val right
//	def item                  :LabeledItem[F, S, LastKey, LastValue] =
	override def size  :Int = init.size //items.last.index + 1
	override def keys  :IndexedSeq[Label] = init.keys
	override def items :IndexedSeq[Item[F, S, _]] = init.items
	override def toMap :Map[String, Val[F, S, _]] = init.toMap
	override def toSeq :Seq[Val[F, S, _]] = init.toSeq
//	override lazy val inOrder :Seq[SQLExpression[F, S, _]] = PassedArray.two(left, right)
//	protected override lazy val parts :Seq[Val[F, S, _]] = init.in

	override def lastItem(implicit nonEmpty :SplitRecord[V]):LabeledItem[F, S, nonEmpty.Key, nonEmpty.Value] =
		init.lastItem

	override def groundValue :Opt[V] = left.groundValue

	override def extracts[E <: F, A >: Grouped <: S]
			:Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[V]#to, _]] =
		left.extracts

	override def construct(items :({ type T[X] = Item[F, S, X] })#T =>: generic.Self) :V =
		left.construct(items)

	override def rebuild[E <: RowProduct, A >: Grouped <: Single]
	                    (items :({type T[X] = Item[F, S, X]})#T =>: SQLExpression.from[E]#rows[A]#E):RecordSQL[E, A, V] =
		init.rebuild(items)

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :Same[E, S, V] = left.rephrase(mapper)

	override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                    (left :SQLExpression[E, C, V], right :SQLExpression[E, C, Null]) :RecordSQL[E, C, V] =
		???

	protected override def reform(reordering :Rearrangement)
	                             (implicit spelling :SQLSpelling) :SQLExpression[F, S, V] =
		left.reform(reordering)

	protected override def split(implicit spelling: SQLSpelling) :Seq[ColumnSQL[F, S, _]] = spelling.split(left)

	protected override def potentialColumns(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
}
*/






/* We could implement the whole GroundingTemplate suite of methods in a type safe manner, but we'll inherit working
 * implementations from LabeledSQL, so why bother? This remains a template trait only for RecordSQLTemplate
 * to be able to cast itself down and return the correct Val/Same type. Of course, we could just cast twice:
 * this.asInstanceOf[LabeledSQL[F, S, _, _, _]].last.asInstanceOf[LabeledValueSQL[F, S, _]]
 */
private trait NonEmptyRecordTemplate
              [-F <: RowProduct, -S >: Grouped <: Single, I <: Listing, K <: Label, L,
               +Val[-f <: RowProduct, -s >: Grouped <: Single, v]
                   <: SQLExpression[f, s, v] with GroundingOps[f, s, v, ({ type E[-E <: RowProduct] = Val[E, s, v] })#E, Val[f, s, v]],
               +Same[-f <: RowProduct, -s >: Grouped <: Single, v <: Listing]
                    <: RecordSQL[f, s, v] with RecordSQLTemplate[f, s, v, Val, Same]]
	extends BinaryCompositeTemplate[F, S, I, L, I |~ (K :~ L),
	                                ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, I |~ (K :~ L)] })#E]
	   with RecordSQLTemplate[F, S, I |~ (K :~ L), Val, Same]
//private class NonEmptyRecordTemplate
//              [-F <: RowProduct, -S >: Grouped <: Single, I <: Listing, K <: Label, L,
//               +Val[-f <: RowProduct, -s >: Grouped <: Single, v]
//                   <: SQLExpression[f, s, v] with VariantGroundingTemplate[f, s, v, ({ type E[-E <: RowProduct] = Val[E, s, v] })#E],
//               +Same[-f <: RowProduct, -s >: Grouped <: Single, v <: Listing]
//                    <: RecordSQL[f, s, v] with RecordSQLTemplate[f, s, v, Val, Same]]
//              (override val init :Same[F, S, I], override val lastKey :K, override val last :Val[F, S, L])
//	extends LabeledSQL[F, S, I, K, L](init, lastKey, last)
//	   with RecordSQL[F, S, I |~ (K :~ L)]
//	   with BinaryCompositeTemplate[F, S, I, L, I |~ (K :~ L),
//	                                ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, I |~ (K :~ L)] })#E]
//	   with RecordSQLTemplate[F, S, I |~ (K :~ L), Val, Same]
{ self :Same[F, S, I |~ (K :~ L)] with NonEmptyRecordTemplate[F, S, I, K, L, Val, Same] =>

//	@inline final def self :Same[F, S, I |~ (K :~ L)] = this
	//For this check we need one of the collections eagerly initialized. I am not sure which is best.
	if (init.keys.contains(lastKey))
		throw new IllegalArgumentException("Duplicate key given in `" + init + " |~ '" + lastKey + "' :~ `" + last + "`.")

	if (lastKey.isEmpty)
		throw new IllegalArgumentException("Empty key given in `" + init + " |~ '' :~ `" + last + "`.")

	override type LabeledItem[-E <: RowProduct, -D >: Grouped <: Single, L <: Label, X] >: init.LabeledItem[E, D, L, X]
	                          <: RecordItem[E, D, L, X]

	implicit def keyLiteral :ValueOf[K] = new ValueOf(lastKey)
	def lastKey :K
	val init    :Same[F, S, I]
	val last    :Val[F, S, L]
	val item    :LabeledItem[F, S, K, L]
	override def size         :Int = keys.length //items.last.index + 1
	override val keys         :IndexedSeq[Label] = init.keys :+ lastKey
	override lazy val items   :IndexedSeq[Item[F, S, _]] = (init.items :IndexedSeq[Item[F, S, _]]) :+ item
	override lazy val toMap   :Map[String, Val[F, S, _]] = init.toMap.updated(lastKey, last)
	override lazy val toSeq   :Seq[Val[F, S, _]] = init.toSeq :+ last
//	override lazy val inOrder :Seq[SQLExpression[F, S, _]] = PassedArray.two(init, last)
//	protected override lazy val parts :Seq[Val[F, S, _]] = init.toSeq :+ last

	override def lastItem(implicit nonEmpty :SplitRecord[I |~ (K :~ L)])
			:LabeledItem[F, S, nonEmpty.Key, nonEmpty.Value] =
		item.castFrom[LabeledItem[F, S, K, L], LabeledItem[F, S, nonEmpty.Key, nonEmpty.Value]]
//
//	override lazy val universalForm :Opt[SQLForm[I |~ (K :~ L)]] =
//		for (iform <- init.universalForm; lform <- last.universalForm)
//			yield (iform |~ :~[K](lform))(new ValueOf[K](key))
//
//	override lazy val selectForm :SQLReadForm[I |~ (K :~ L)] =
//		(init.selectForm |~ :~[K](last.selectForm))(new ValueOf[K](key))

	override def extracts[E <: F, A >: Grouped <: S]
			:Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[I |~ (K :~ L)]#to, _]] =
		_extracts.asInstanceOf[Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[I |~ (K :~ L)]#to, _]]]

	private[this] lazy val _extracts
		:Seq[Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I |~ (K :~ L)]#to, _]] =
	{
		def map[X](entry :Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I]#to, X]) =
			Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I |~ (K :~ L)]#to, X](
				entry._1, entry._2 compose Chain.init[I] _
			)
		init.extracts.map {
			entry :Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I]#to, _] => map(entry)
		} :+ Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I |~ (K :~ L)]#to, L](
			last, { tuple :(I |~ (K :~ L)) => tuple.last.value }
		)
	}

	override def construct(items :({ type T[X] = Item[F, S, X] })#T =>: generic.Self) :I |~ (K :~ L) =
		init.construct(items) |~ :~[K](items(item))

	override def rebuild[E <: RowProduct, A >: Grouped <: Single]
	                    (items :({type T[X] = Item[F, S, X]})#T =>: SQLExpression.from[E]#rows[A]#E)
			:RecordSQL[E, A, I |~ (K :~ L)] =
		init.rebuild(items) |~ :~[K](items(item))



//	override def groundValue :Opt[I |~ (K :~ L)] =
//		for {i <- init.groundValue; l <- last.groundValue} yield i |~ :~[K](l)
//
//	override def isSingleRow :Boolean = last.isSingleRow && init.isSingleRow
//
//	private[ast] override def checkIfAnchored :Boolean = last.isAnchored && init.isAnchored
//	private[ast] override def checkIfAnchored(from :F) = last.isAnchored(from) && init.isAnchored(from)
//
//	override def anchor(from :F) :Same[F, S, I |~ (K :~ L)] =
//		(init.anchor(from), last.anchor(from)) match {
//			case (i, l) if (i eq init) && (l eq last) => this
//			case (i, l) => cons(i, lastKey, l)
//		}
//
//	override def basedOn[U <: F, E <: RowProduct]
//	                    (base :E)(implicit expansion :U PartOf E) :Same[E, S, I |~ (K :~ L)] =
//		cons(init.basedOn(base), lastKey, last.basedOn(base))
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S)
//			:Same[E, S, I |~ (K :~ L)] =
//		cons(init.expand(base), lastKey, last.expand(base))


//	override def reapply[E <: RowProduct, C >: Grouped <: Single]
//	                    (left :SQLExpression[E, C, I], right :SQLExpression[E, C, L]) :Same[E, C, I |~ (K :~ L)] =
//
	//This short-circuits the call, resulting in no-callbacks for the prefixes. Lets call it a feature.
	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :Same[E, S, I |~ (K :~ L)] =
		reapply(init.rephrase(mapper), mapper(last))


/*
	override def reorder(keys :Unique[String]) :SQLExpression[F, S, I |~ (K :~ L)] = {
		if (items.view.map(_.key) == keys)
			this
		else if (items.length != keys.size)
			throw new IllegalArgumentException(
				"There are " + items.length + " items in record " + this + ", but the key permutation given " +
					keys + " is counts " + keys.size + " elements."
			)
		else {
			val newKeys = keys.downcastParam[Label]
			val ourKeys = items.view.map(_.key :Label).to(Unique)
			if (ourKeys.size != items.length)
				throw new IllegalStateException(
					"Cannot reorder record " + this + " to key order " + keys + " because it contains key duplicates."
				)
			if (ourKeys.toSet != keys.toSet)
				throw new IllegalArgumentException(
					"Cannot reorder record " + this + " to key order " + keys + " because the keys do not match."
				)
			def reorder(itemNo :Int) :RecordSQL[F, S, _ <: Listing] =
				if (itemNo < 0)
					RecordSQL()
//					empty
				else {
					val init = reorder(itemNo - 1)
					val key = keys(itemNo)
//					init |~ key :~ toMap(key)
					(init |~ :~[key.type](toMap(key)))(new ValueOf(key))
//					cons(init, key, toMap(key))
				}
//			val reordered = reorder(ourKeys.size - 1).castFrom[Same[F, S, _], Same[F, S, Listing]]
			val reordered = reorder(ourKeys.size - 1).castFrom[RecordSQL[F, S, _], RecordSQL[F, S, Listing]]
			val convert = RecordReordering[Listing, Chain, I |~ (K :~ L), Chain](
				ProjectRecord.unsafe(newKeys, ourKeys), ProjectRecord.unsafe(ourKeys, newKeys)
			)
			convert(reordered)
		}
	}
*/
	//todo: implement reform!

	/* Todo: remove it, inherited method should be sufficient. Also, allow an IndexedSQL to be padded with null columns.
	 * The latter is very problematic, because suddenly potentialColumns of all items do not add up
	 * to all alignable columns of this record. However, without it, or a third LabeledValueSQL,
	 * we can't preserve the IndexedSQL type after padding. On the other hand, we still do not preserve it
	 * when reordering the items (which also may happen in this method), so perhaps we might just
	 * create RearrangedIndexedSQL as a third option after EmptySQL and NonEmptyIndexedSQL.
	 * On the other hand, wouldn't it defeat the whole idea of InlineSQL?
	 * Maybe we should simply create a supertrait for IndexedSQL, which does not extend InlineSQL,
	 * with a sole purpose of creating an SQLMapping for the expression.
	 * I also think that we should do the same (including a special SQLMapping) for RecordSQL. We won't know
	 * if a component with a given label is a ColumnMapping, but it still seems to have genuine use.
	 */
	protected override def realign(reordering :Rearrangement)
	                              (implicit spelling :SQLSpelling) :SQLExpression[F, S, I |~ (K :~ L)] =
		if (reordering.isIdentity)
			this
		else {
			val lastColumns = spelling.potentialColumnsCount(last)
			val initColumns = reordering.columnCount - lastColumns
			try {
				//Hopes to reform left and right independently, without changing label order on this level
				val (initOrder, lastOrder) = reordering.splitAt(initColumns + 1)
				if (lastOrder.columnCount == 1 && lastOrder.underlyingColumnCount != 1) {
					//todo: pad with null columns on this level to prevent an attempt to map a column to multiple columns
				}
				if (initOrder.columnCount == 1 && initOrder.underlyingColumnCount != 1) {
					//todo: pad with null columns on this level to prevent an attempt to map a column to multiple columns
				}
				reapply(spelling.realign(left, initOrder), spelling.realign(right, lastOrder))
			} catch {
				case e1 :Exception => //second attempt: see if reordering shuffles complete expressions.
					//fixme: this does not work with NullPaddingRecordSQL, and we need NullPaddingIndexedSQL
					// because it becomes SelectAs, and the mapping type should be preserved.
					//current start and end index of every element in the record
					val spans = toSeq.scanLeft((0, 1)) { case ((_, start), part) =>
						(start, start + spelling.potentialColumns(part, MayReform).columns.length)
					}
					//range of column indices in the new order of each element of this record
					val newSpans = spans.map { case (start, end) =>
						val min = (start until end).view.map {
							i => if (reordering.isMapped(i)) reordering(i) else Int.MaxValue
						}.min
						val max = (start until end).view.map {
							i => if (reordering.isMapped(i)) reordering(i) else Int.MinValue
						}.max
						(min, max + 1)
					}.toIndexedSeq
					//indices of current parts after reordering
					val newOrder = (0 until size).sortBy(newSpans(_)._1)//.map(keys) to Unique
					//check if there are no overlaps
					val overlaps = newOrder.exists { i =>
						i != 0 && newSpans(i - 1)._2 > newSpans(i)._1
					}
					def fallback(e :Exception) = {
						def error(e2 :Exception) = {
							val e3 = new IllegalArgumentException(
								"Cannot reorder `" + this + "` according to " + reordering + ": " + e2.getMessage, e2
							)
							e3.addSuppressed(e1)
							if (e != null)
								e3.addSuppressed(e)
							e3
						}
						try {
							split //make sure we can spell each column independently, so exploded spelling is possible
							RearrangedSQL(this, reordering)
						} catch {
							case e2 :IllegalArgumentException => throw error(e2)
							case e2 :InseparableExpressionException => throw error(e2)
							case e2 :UndefinedShapeException => throw error(e2)
						}
					}
					if (!overlaps) { //try to reorder each part independently, and then reorder the parts themselves
						val partReorderings = spans.map { case (start, end) =>
							val indices = (start until end).map { i =>
								if (reordering.isMapped(i)) i - start else -1
							}
							Rearrangement(indices)
						}
						//individual reorderings above use ranges [min,max) possibly omitting gap regions in reordering
						val hasGaps =
							newSpans(newOrder.head)._1 > 1 ||
								newSpans(newOrder.last)._2 <= reordering.underlyingColumnCount ||
								newOrder.exists { i => i != 0 && newSpans(i - 1)._2 < newSpans(i)._1 }
						try {
							type OwnItem[X] = Item[F, S, X]
							val reordered = rebuild(
								new (OwnItem =>: SQLExpression.from[F]#rows[S]#E) {
									override def apply[T](x :Item[F, S, T]) :SQLExpression[F, S, T] =
										spelling.realign(x.value, partReorderings(x.index))
								}
							)
							val res = reordered.reorder(newOrder.map(keys) to Unique)
							//If there are gaps between spans in newSpans, then res needs to be spliced with null columns
							// An alternative would be to force element expressions to include them, but this is impossible
							// for columns.
							if (!hasGaps)
								res
							else {
								//todo: NullPaddedRecordSQL
								//list every index which is mapped
								val identityWithGaps =
									newOrder.flatMap { oldPartNo =>
										val (from, until) = newSpans(oldPartNo)
										from until until
									}
								val nullPadding = Rearrangement.injection(identityWithGaps)
								RearrangedSQL(res, nullPadding)
							}
						} catch {
							case e :IllegalArgumentException => fallback(e)
							case e :InseparableExpressionException => fallback(e)
							case e :UndefinedShapeException => fallback(e)
						}
					} else
						fallback(null)
			}
		}

/*
	protected override def reform(reordering :Rearrangement)
	                             (implicit spelling :SQLSpelling) :SQLExpression[F, S, I |~ (K :~ L)] =
	{
		val lastColumns = spelling.potentialColumns(last, MayReform).columns.length
//		val lastColumns = paths.length
		val initColumns = reordering.columnCount - lastColumns
		try {
			val (initOrder, lastOrder) = reordering.splitAt(initColumns + 1)
			LabeledSQL(init.`->reform`(initOrder), lastKey, last.`->reform`(lastOrder))
		} catch {
			case e :Exception =>
				try {
					split //if we can print individual columns
					new RearrangedSQL(this, reordering)
				} catch {
					case _ :Exception =>
						throw new IllegalArgumentException(
							"Cannot reorder `" + this + "` according to " + reordering + ": " + e.getMessage, e
						)
				}
		}
	}
*/

//	protected def empty :Same[RowProduct, Single, @~]
//	protected def cons[E <: RowProduct, C >: Grouped <: Single, I1 <: Listing, K1 <: Label, L1]
//	                  (init :Same[E, C, I1], key :K1, last :Val[E, C, L1]) :Same[E, C, I1 |~ (K1 :~ L1)]


	//
//	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[I |~ (K :~ L)] =
//		spelling.effectiveForm(init) |~ :~[K](spelling.effectiveForm(last))

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :NonEmptyRecordTemplate[_, _, _, _, _, Val, Same] @unchecked if other.canEqual(this) =>
			last == other.last && lastKey == other.lastKey && init == other.init
		case _ => false
	}
	override def hashCode :Int = (init.hashCode * 31 + lastKey.hashCode) * 31 + last.hashCode
}






private final class NonEmptyRecordSQL[-F <: RowProduct, -S >: Grouped <: Single, I <: Listing, K <: Label, L]
                                     (override val init :RecordSQL[F, S, I], override val lastKey :K,
                                      override val last :SQLExpression[F, S, L])
	extends LabeledSQL[F, S, I, K, L](init, lastKey, last)
	   with NonEmptyRecordTemplate[F, S, I, K, L, SQLExpression, RecordSQL]
//	extends NonEmptyRecordTemplate[F, S, I, K, L, SQLExpression, RecordSQL](init, lastKey, last)
	   with RecordSQL[F, S, I |~ (K :~ L)]
	   with RowShapeCache
{ self =>
	def this(left :RecordSQL[F, S, I], right :SQLExpression[F, S, L])(implicit key :ValueOf[K]) =
		this(left, key.value, right)

	override type LabeledItem[-F1 <: RowProduct, -S1 >: Grouped <: Single, K1 <: Label, X] = RecordItem[F1, S1, K1, X]
	override val item = new RecordItem(init.size, lastKey, right)
//	override val items :IndexedSeq[Item[F, S, _]] = init.items :+ item

//	override def construct(items :({ type T[X] = RecordItem[F, S, _ <: Label, X] })#T =>: generic.Self) :I |~ (K :~ L) =
//		init.construct(items) |~ :~[K](items(item))


	//This short-circuits the call, resulting in no-callbacks for the prefixes. Lets call it a feature.
	// The override is needed because the return type is different in RecordSQL and LabeledSQL
//	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :RecordSQL[E, S, I |~ (K :~ L)] =
//		reapply(init.rephrase(mapper), mapper(last))
/*
		(init.rephrase(mapper), mapper(last)) match {
			case (i, l) if (i eq init) && (l eq last) =>
				this.castFrom[NonEmptyRecordSQL[F, S, I, K, L], NonEmptyRecordSQL[E, S, I, K, L]]
			case (index :IndexedSQL[E, S, I], labeled :LabeledValueSQL[E, S, L]) =>
				new NonEmptyIndexedSQL(index, lastKey, labeled)
			case (init, last) =>
				new NonEmptyRecordSQL(init, lastKey, last)
		}
*/

	override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                    (left :SQLExpression[E, C, I], right :SQLExpression[E, C, L]) :RecordSQL[E, C, I |~ (K :~ L)] =
		(left, right) match {
			case _ if (left eq init) && (right eq last) =>
				this.castFrom[NonEmptyRecordSQL[F, S, I, K, L], NonEmptyRecordSQL[E, C, I, K, L]]
			case (index :IndexedSQL[E, C, I], labeled :LabeledValueSQL[E, C, L]) =>
				new NonEmptyIndexedSQL(index, lastKey, labeled)
			case (record :RecordSQL[E, C, I], last) =>
				new NonEmptyRecordSQL(record, lastKey, last)
			case _ => throw new IllegalExpressionException(
				"Cannot recreate a RecordSQL with a non record expression `" + left + "`: " + left.className +
					" and `" + right + "` (started from `" + this + "`)."
			)
		}

//	protected override def empty = EmptySQL
//	protected override def cons[E <: RowProduct, C >: Grouped <: Single, I1 <: Listing, K1 <: Label, L1]
//	                           (init :RecordSQL[E, C, I1], key :K1, last :SQLExpression[E, C, L1])
//			:RecordSQL[E, C, I1 |~ (K1 :~ L1)] =
//		(init |~ :~[K](last))(new ValueOf[K1](key))

	//fixme: implement RecordSQL reforming
//		protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//		                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
//		                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//		                               (implicit leftResult  :SQLTransformation[I |~ (K :~ L), U],
//		                                         rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
//				:SpecificExpressionVisitor
//				 [F2, S2, V2, (leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])] =
//			new BaseReformer[F2, S2, V2, EC2, U, leftResult.Expression, rightResult.Expression](
//				other)(reform, passCount)(leftResult, rightResult, spelling
//			) with MatchSpecificRecord[F2, S2, V2, (leftResult.Expression[F, S, SQLExpression[F, S, U]],
//			                                         rightResult.Expression[F2, S2, EC2[U]])]
//			{
//				override def multiNull(e :MultiNull[V2]) =
//					if (reform.mayReformRight ||
//						reform.mayAddNullRight && rightResult.isValueIdentity && e.form.columnCount < selectForm.columnCount)
//					{
//						val rightNull = MultiNull(effectiveForm)
//						(this.left, rightResult(rightNull.asInstanceOf[EC2[V2]]))
//					} else
//						fallback
//
//				override def empty(implicit isEmpty :V2 =:= @~) =
//					if (passCount.secondTime && reform.mayAddNullRight) {
//						val nullConversion =
//							SQLConversion.opt("to[@~]", (_:(I |~ (K :~ L))) => @~ : @~, (_: @~) => nullValue)
//						val toV2 = isEmpty.flip.liftCo[SQLConversion.from[I |~ (K :~ L)]#to](nullConversion)
//						(this.left, rightResult(toV2(nullSQL.asInstanceOf[EC2[I |~ (K :~ L)]])))
//					} else if (passCount.secondTime && reform.mayExcludeLeft) {
//						/* Two options here: either we fail fast in case of lack of nullValue as an early warning,
//						 * or we hope that the method never gets called. Ideally we would report a warning
//						 * in the latter case, but we do not have SQLContext here. Perhaps we will just log
//						 * it here and now ourselves. */
//						val nullConversion = SQLConversion.opt(
//							keys.mkString(".to[", ",", "]"), forceNullValue, (_:(I |~ (K :~ L))) => Got(@~)
//						)
//						(leftResult(nullConversion(EmptyIndex)), this.right)
//					} else
//						fallback
//
//				//we could also reform with a LabeledColumnSQL, too
//				override def labeled[V <: Listing](e :IndexedSQL[F2, S2, V])(implicit isListing :V2 =:= V)
//						:(leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
//					if (keys == e.keys)
//						super.labeled(e)
//					else {
//						//Attempt to reorder both or either expression, either excluding or adding null columns,
//						// but only if the column forms match exactly on shared columns, because we have no information
//						// on how - if at all - I |~ (K :~ L) and V2 are related.
//						val leftColumns = self.columns
//						val rightColumns = e.columns
//						val subsetOfRight = leftColumns.forall {
//							case (path, col) => rightColumns.get(path).exists(_.selectForm == col.selectForm)
//						}
//						val leftPaths  = leftColumns.keySet
//						val rightPaths = rightColumns.keySet
//						val subsetOfLeft =
//							if (subsetOfRight)
//								leftPaths == rightPaths
//							else
//								rightColumns.forall {
//									case (path, col) => rightColumns.get(path).exists(_.selectForm == col.selectForm)
//								}
//						def subseqOfRight = {
//							val otherPaths = e.paths to Unique
//							paths.isSortedBy(otherPaths.indexOf)
//						}
//						def subseqOfLeft = {
//							val paths = self.paths to Unique
//							e.paths.isSortedBy(paths.indexOf)
//						}
//						if (subsetOfRight && subsetOfLeft) //same structure, different order
//							if (reform.mayReorderLeft) {
//								val reordered = reorder(e.paths)
//								(leftResult(reordered), rightResult(other))
//							} else if (reform.mayReorderRight) {
//								val reordered = e.reorder(paths).asInstanceOf[Argument]
//								(leftResult(self), rightResult(reordered))
//							} else
//								fallback
//						else if (subsetOfRight)
//							if (reform.mayExcludeRight) //use the left path set, it is a subset the right one
//								if  (reform.mayReorderRight || subseqOfRight) {
//									val reordered = e.project(paths).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//									(leftResult(self), rightResult(reordered))
//								} else if (reform.mayReorderLeft) {
//									val order = e.paths.filterNot(leftColumns.keySet)
//									val right = e.project(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//									(leftResult(reorder(order)), rightResult(right))
//								} else
//									fallback
//							else if (reform.mayAddNullLeft) //use the right path set, it is a superset of the left one
//								if (reform.mayReorderLeft || subseqOfRight) {
//									val order = e.paths.map(path => (path, rightColumns(path).nullSQL))
//									(leftResult(self.reform(order)), rightResult(other))
//								} else if (reform.mayReorderRight) {
//									val order = paths ++ e.paths.filterNot(leftPaths)
//									val left  = self.reform(order.map(path => (path, rightColumns(path).nullSQL)))
//									val right = e.project(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//									(leftResult(left), rightResult(right))
//								} else
//									fallback
//							else
//								fallback
//						else if (subsetOfLeft) //symmetrical to the above, but extracting as a method would actually take more code
//							if (reform.mayExcludeLeft) //use the right path set, it is a subset of the left one
//								if (reform.mayReorderLeft || subseqOfLeft)
//									(leftResult(project(e.paths)), rightResult(other))
//								else if (reform.mayReorderRight) {
//									val order = paths.filterNot(rightColumns.keySet)
//									val right = e.reorder(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//									(leftResult(project(order)), rightResult(right))
//								} else
//									fallback
//							else if (reform.mayAddNullRight) //use the left path set, it a superset of the right one
//								if (reform.mayReorderRight || subseqOfLeft) {
//									val order = paths.map(path => (path, leftColumns(path).nullSQL))
//									val right = e.reform(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//									(leftResult(self), rightResult(right))
//								} else if (reform.mayReorderLeft) {
//									val order = e.paths ++ paths.filterNot(rightPaths)
//									val right = e.reform(order.map(path => (path, leftColumns(path).nullSQL)))
//										.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//									(leftResult(project(order)), rightResult(right))
//								} else
//									fallback
//							else
//								fallback
//						else {
//							val sharedInLeftOrder  = paths.filter(rightPaths)
//							val sharedInRightOrder = e.paths.filter(leftPaths)
//							if (reform.mayExcludeLeft)
//								if (reform.mayExcludeRight) { //user the shared path set, projecting both sides
//									if (reform.mayReorderLeft || sharedInLeftOrder == sharedInRightOrder) {
//										val left  = self.project(sharedInRightOrder)
//										val right = e.project(sharedInRightOrder).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//										(leftResult(left), rightResult(right))
//									} else if (reform.mayReorderRight) {
//										val left  = self.project(sharedInLeftOrder)
//										val right = e.project(sharedInLeftOrder).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//										(leftResult(left), rightResult(right))
//									} else
//										fallback
//								} else if (reform.mayAddNullLeft) { //use the right path set
//									if (reform.mayReorderLeft || sharedInLeftOrder == sharedInRightOrder) {
//										val left  = self.reform(e.paths.map { path => (path, rightColumns(path).nullSQL) })
//										(leftResult(left), rightResult(other))
//									} else if (reform.mayReorderRight) {
//										val order = sharedInLeftOrder ++ e.paths.filterNot(leftPaths)
//										val left = self.reform(order.map { path => (path, rightColumns(path).nullSQL) })
//										val right = e.reorder(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//										(leftResult(left), rightResult(right))
//									} else
//										fallback
//								} else
//									fallback
//							else if (reform.mayAddNullRight)
//								if (reform.mayExcludeRight) { //use the left path set
//									if (reform.mayReorderRight || sharedInLeftOrder == sharedInRightOrder) {
//										val right = e.reform(paths.map { path => (path, leftColumns(path).nullSQL) })
//										(leftResult(self), rightResult(right.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
//									} else if (reform.mayReorderLeft) {
//										val order = e.paths.filter(leftPaths) ++ paths.filterNot(rightPaths)
//										val left  = self.reorder(order)
//										val right = e.reform(order.map { path => (path, leftColumns(path).nullSQL) })
//										(leftResult(left), rightResult(right.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
//									} else
//										fallback
//								} else if (reform.mayAddNullLeft)  //use the superset of both path sets
//									if (reform.mayReorderLeft || sharedInLeftOrder == sharedInRightOrder) {
//										val order =
//											e.paths.map { path => (path, rightColumns(path).nullSQL) } ++
//												paths.filterNot(rightPaths).map { path => (path, leftColumns(path).nullSQL) }
//										val right = e.reform(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//										(leftResult(self.reform(order)), rightResult(right))
//									} else if (reform.mayReorderRight) {
//										val order =
//											paths.map { path => (path, leftColumns(path).nullSQL) } ++
//												e.paths.filterNot(leftPaths).map { path => (path, rightColumns(path).nullSQL) }
//										val right = e.reform(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//										(leftResult(self.reform(order)), rightResult(right))
//									} else
//										fallback
//								else
//									fallback
//							else
//								fallback
//						}
//					}
//
//				override def emptyLabeled(implicit ev :V2 =:= @~) =
//					if (reform.mayAddNullRight) {
//						val forceEmpty = SQLConversion(" => @~", (_ :I |~ (K :~ L)) => @~)
//						(leftResult(self), //this cast can be removed by delegating this case to EmptyIndex
//						 rightResult(forceEmpty(nullSQL).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
//					} else if (reform.mayExcludeLeft)
//						try {
////							val nullValue = LabeledEntry.this.nullValue
////							val forceNull = SQLConversion(" => (" + nullValue + ")", forceNullValue)
////							val left = forceNull(nullSQL :ConvertibleSQL[RowProduct, Single, @~, GroundSQL])
//							(leftResult(nullSQL), rightResult(other))
//						} catch {
//							case e1 :NullPointerException => try {
//								fallback
//							} catch {
//								case e2 :MismatchedExpressionsException =>
//									e2.addSuppressed(e1)
//									throw e2
//							}
//						}
//					else
//						fallback
//
//				override def labeledItem[I2 <: Listing, K2 <: Label, L2]
//				                        (e :IndexedSQL[F2, S2, I2 |~ (K2 :~ L2)])
//				                        (implicit isListing :V2 =:= (I2 |~ (K2 :~ L2))) =
//				{
//					implicit val listingResult = isListing.substituteCo[RightResult](rightResult)
//
//					//Attempt to recreate a IndexedSQL by casting down the expressions obtained
//					// by reforming init and last
//					def listing[G <: RowProduct, D >: Grouped <: Single, W <: Listing, A <: Label, Z]
//					           (i :SQLExpression[G, D, W], key :A, l :SQLExpression[G, D, Z]) =
//						attempt(i, key, l) match {
//							case Lack if !i.isInstanceOf[IndexedSQL[_, _, _]] =>
//								throw new MismatchedExpressionsException(self, e,
//									"received a non-IndexedSQL expression " + i +
//										": " + i.getClass.getName + " as a partial result."
//								)
//							case Lack =>
//								throw new MismatchedExpressionsException(self, e,
//									"Reformed last expression " + l + " is neither a ColumnSQL nor a LabeledValueSQL."
//								)
//							case Got(res) => res
//						}
//					def isUpcast(conversion :SQLTransformation[_, _]) =
//						conversion.isIdentity || conversion.isInstanceOf[Upcast[_, _]]
//
//					(splitRecordTransformation[I, K, L, U](leftResult),
//						splitRecordTransformation[I2, K2, L2, U](listingResult)
//					) match {
//						case (Lack, _) =>
//							throw new MismatchedExpressionsException(
//								self, other, "unsupported left conversion type " + leftResult + "."
//							)
//						case (_, Lack) =>
//							throw new MismatchedExpressionsException(
//								self, other, "unsupported right conversion type " + rightResult + "."
//							)
//						//init and last are converted separately, no conversion applied on top of init |~ key :~ last
//	//						case (left :Opt[LeftSplit[Listing, Any]]@unchecked, right :Opt[RightSplit[Listing, Any]]@unchecked) =>
//	//	//					if left.get._3.isIdentity && right.get._3.isIdentity =>
//	////							implicit val (leftInitResult, leftLastResult, leftPost) = left.get
//	////							implicit val (rightInitResult, rightLastResult, rightPost) = right.get
//	//							val leftInitResult  = left.get._1
//	//							val leftLastResult  = left.get._2
//	//							val leftPost        = left.get._3
//	//							val rightInitResult = right.get._1
//	//							val rightLastResult = right.get._2
//	//							val rightPost       = right.get._3
//						case (Got(leftInitResult :(I sql_=> Listing) @unchecked, leftLastResult :(L sql_=> Any) @unchecked,
//						      leftPost :SQLTransformation.Into[Listing |~ (K :~ Any), U, leftResult.Expression] @unchecked),
//						      Got(rightInitResult :(I2 sql_=> Listing) @unchecked, rightLastResult :(L2 sql_=> Any) @unchecked,
//						      rightPost :SQLTransformation.Into[Listing |~ (K2 :~ Any), U, rightResult.Expression]@unchecked))
//						/* This check approximates the condition that the input types of leftPost and rightPost are the same.
//						 * 1. We know the outputs are equal, so if the conversions are also equal, we assume the same for inputs.
//						 * 2. We know both expressions before conversions have Listing as value types, so if both
//						 *    conversions are upcasts/identities, then the conversions can be split into components
//						 *    for init and last. Function splitListingTransformation has already helpfully wrapped
//						 *    the expressions in suitable upcast conversions, which will at least generate more
//						 *    informative errors if we fail to convert value from one expression to the type in the other.
//						 * This is all with fingers crossed, but there is much more usefulness in the ability to
//						 * unify two records than providing strict guarantees about Scala type safety,
//						 * because if the only thing we need to do is convert it to SQL, without interchanging values
//						 * with the application, then it all doesn't matter anyway.
//						 */
//						if leftPost == rightPost || isUpcast(leftPost) && isUpcast(rightPost) =>
//							val (leftLast, rightLast) = reform(self.last, e.last)(
//								leftLastResult, rightLastResult, spelling
//							)
//							val (leftInit, rightInit) = reform(self.init, e.init)(
//								leftInitResult, rightInitResult, spelling
//							)
//							val leftReformed  = listing(leftInit, key, leftLast)
//							val rightReformed = listing(rightInit, e.lastKey :K2, rightLast).castFrom[
//								IndexedSQL[F2, S2, Listing |~ (K2 :~ Any)],
//								ConvertibleSQL[F2, S2, Listing |~ (K2 :~ Any), EC2]
//							]
//							(leftPost.convert(leftReformed), rightPost.convert(rightReformed))
//					}
//				}
//			}
}




private[ast] class UnsealedLabeledSQL[-F <: RowProduct, -S >: Grouped <: Single, I <: Listing, K <: Label, L]
                                     (init :SQLExpression[F, S, I], key :K, last :SQLExpression[F, S, L])
	extends LabeledSQL[F, S, I, K, L](init, key, last)

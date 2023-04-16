package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Chain, Listing, Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{InseparableExpressionException, MismatchedExpressionsException, UndefinedShapeException}
import net.noresttherein.oldsql.morsels.{Extractor, generic, inversePermutation}
import net.noresttherein.oldsql.morsels.generic.=>:
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.bits.LabelPath
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.slang.{cast2TypeParams, saferCasting}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, Grouped, ReorderingTemplate, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.ChainSQL.{CaseSpecificChain, splitChainTransformation, upcastListing}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.{BinaryCompositeTemplate, CompositeSQLTemplate}
import net.noresttherein.oldsql.sql.ast.ChainTuple.{CaseAnyChainTuple, CaseSpecificChainTuple}
import net.noresttherein.oldsql.sql.ast.EmptySQL.{AnyEmptyVisitor, SpecificEmptyVisitor}
import net.noresttherein.oldsql.sql.ast.InlineSQL.{AbstractInlineBinaryCompositeSQL, InlineItem}
import net.noresttherein.oldsql.sql.ast.IndexedSQL.{LabeledColumnSQL, LabeledValueSQL, MatchSpecificIndexed, UnsealedEmptyIndexedSQL}
import net.noresttherein.oldsql.sql.ast.LabeledSQL.splitListingTransformation
import net.noresttherein.oldsql.sql.ast.RecordSQL.{RecordField, RecordPath, SplitRecord}
import net.noresttherein.oldsql.sql.mechanics.{AlignableColumns, Reform, ReformPermissions, SQLConversion, SQLScribe, SQLTransformation, SpelledSQL, sql_=>}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.{MayReform, MayReorder}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.{ChainConversion, ChainReordering, Upcast}






/** An SQL expression for a non empty [[net.noresttherein.oldsql.collection.Chain Chain]].
  * It is a composite of [[net.noresttherein.oldsql.sql.ast.ChainSQL.init init]], an expression for all
  * elements of the chain except for the last, and [[net.noresttherein.oldsql.sql.ast.ChainSQL.last last]],
  * an expression for the last element in the chain.
  *
  * This class differs from the extending [[net.noresttherein.oldsql.sql.ast.ChainTuple ChainTuple]]
  * in that it allows any `SQLExpression` for the initial elements of the chain, rather than
  * a [[net.noresttherein.oldsql.sql.ast.ChainTuple ChainTuple]] as the former. Another difference is that
  * the latter may represent an [[net.noresttherein.oldsql.sql.ast.EmptySQL empty record]], while this class is,
  * by its nature, non empty.
  *
  * @author Marcin Mościcki
  */
//todo: rename to TupleExtensionSQL
sealed class ChainSQL[-F <: RowProduct, -S >: Grouped <: Single, I <: Chain, L] protected
                     (val init :SQLExpression[F, S, I], val last :SQLExpression[F, S, L])
	extends AbstractInlineBinaryCompositeSQL[F, S, I, L, I ~ L]
	   with BinaryCompositeTemplate[F, S, I, L, I ~ L,
	                                ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = ChainSQL[f, s, I, L]})#E]
	   with VariantGroundingTemplate[F, S, I ~ L, ({ type E[-f <: RowProduct] = ChainSQL[f, S, I, L] })#E]
	   with SelectableSQL[F, S, I ~ L]
{ self =>
	//todo: review where do we want to use NonSpecificSQL and apply them surgically
	protected override def left  :SQLExpression[F, S, I] = init
	protected override def right :SQLExpression[F, S, L] = NonSpecificSQL(last)

/*
	protected override def reform(reordering :Rearrangement)
	                             (implicit spelling :SQLSpelling) :ChainSQL[F, S, I, L] =
	{
		val lastColumns = spelling.potentialColumns(last, MayReform).columns.length
		val initColumns = reordering.columnCount - lastColumns
		try {
			val (initOrder, lastOrder) = reordering.splitAt(initColumns + 1)
			ChainSQL(spelling.reorder(init, initOrder), spelling.reorder(last, lastOrder))
		} catch {
			case e :Exception =>
				throw new IllegalArgumentException(
					"Cannot reorder `" + this + "` according to " + reordering + ": " + e.getMessage, e
				)
		}
	}
*/

	protected override def leftValue(value :I ~ L) :I = value.init
	protected override def rightValue(value :I ~ L) :L = value.last
	protected override def value(left :I, right :L) :I ~ L = left ~ right

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, I], right :SQLExpression[E, C, L]) :ChainSQL[E, C, I, L] =
		ChainSQL(left, right)

	override lazy val selectForm    :SQLReadForm[I ~ L] = init.selectForm ~ last.selectForm
	override lazy val universalForm :Opt[SQLForm[I ~ L]] =
		for (l <- init.universalForm; r <- last.universalForm) yield l ~ r


	protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                               (implicit leftResult :SQLTransformation[I ~ L, U], rightResult :SQLTransformation[V2, U],
	                                         spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])] =
	{
		type Reformed = (leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])
		new BaseReformer[F2, S2, V2, EC2, U, leftResult.Expression, rightResult.Expression](
		                 other)(reform, passCount)(leftResult, rightResult, spelling
			) with CaseSpecificChain[F2, S2, V2, Reformed] with MatchSpecificIndexed[F2, S2, V2, Reformed]
		{
			private def isUpcast(conversion :SQLTransformation[_, _]) =
				conversion.isIdentity || conversion.isInstanceOf[Upcast[_, _]]

			override def empty(implicit isEmpty :V2 =:= @~) =
				throw new MismatchedExpressionsException("Tuple " + ChainSQL.this + " matched with an empty tuple.")

			override def multiNull(e :MultiNull[V2]) =
				if (reform.mayReformRight ||
					reform.mayAddNullRight && rightResult.isValueIdentity && e.form.columnCount < selectForm.columnCount)
				{
					val rightNull = MultiNull(SQLWriteForm.nulls(selectForm.columnCount) <> selectForm)
					(this.left, rightResult(rightNull.castFrom[MultiNull[I ~ L], ConvertibleSQL[F2, S2, V2, EC2]]))
				} else
					fallback

			override def chain[I2 <: Chain, L2](e :ChainSQL[F2, S2, I2, L2])(implicit isChain :V2 =:= (I2 ~ L2)) = {
//				type RightResult[A] = SQLTransformation.Into[A, U, rightResult.Expression]
				implicit val chainResult = isChain.substituteCo[RightResult](rightResult)

				//no sense in passing control if we fail, because ChainSQL is effectively sealed and other will try to do the same
				(splitChainTransformation(leftResult), splitChainTransformation(chainResult)) match {
					case (Lack, _) =>
						throw new MismatchedExpressionsException(
							ChainSQL.this, other, "unsupported left conversion type " + leftResult + "."
						)
					case (_, Lack) =>
						throw new MismatchedExpressionsException(
							ChainSQL.this, other, "unsupported right conversion type " + rightResult + "."
						)
					//init and last are converted separately, no conversion applied on top of init~last
//					case (left :Opt[LeftSplit[Chain, Any]] @unchecked, right :Opt[RightSplit[Chain, Any]] @unchecked) =>
					case (Got(
							leftInitResult :(I sql_=> Chain) @unchecked,
							leftLastResult :(L sql_=> Any) @unchecked,
							leftPost       :LeftResult[Chain ~ Any] @unchecked
						), Got(
							rightInitResult :(I2 sql_=> Chain) @unchecked,
							rightLastResult :(L2 sql_=> Any @unchecked),
							rightPost       :RightResult[Chain ~ Any] @unchecked
					)) if (leftPost == rightPost || isUpcast(leftPost) && isUpcast(rightPost)) =>
						/* The equality check approximates that leftPost and rightPost have the same input type, so
						 * leftInitResult/rightInitResult and leftLastResult/rightLastResult have the same output types.
						 * OTH if unification of this and other is through upcasting, we can always unify
						 * this.init with e.init and this.last with e.last by applying upcasting
						 * on top of whatever conversions they have.
						 */
						val (leftInit, rightInit) = reform(self.init, e.init)(leftInitResult, rightInitResult, spelling)
						val (leftLast, rightLast) = reform(self.last, e.last)(leftLastResult, rightLastResult, spelling)
						val leftReformed = leftInit ~ leftLast
						val rightReformed = (rightInit ~ rightLast).castFrom[
							SQLExpression[F2, S2, Chain ~ Any], ConvertibleSQL[F2, S2, Chain ~ Any, EC2]
						]
						(leftPost(leftReformed), rightPost(rightReformed))
				}

			}

			override def emptyIndexed(implicit ev :V2 =:= @~) = empty

			override def indexedItem[I2 <: Listing, K <: Label, L2]
			                        (e :IndexedSQL[F2, S2, I2 |~ (K :~ L2)])
			                        (implicit isListing :V2 =:= (I2 |~ (K :~ L2))) =
			{
				type RightResult[A] = SQLTransformation.Into[A, U, rightResult.Expression]
				implicit val listingResult = isListing.substituteCo[RightResult](rightResult)

				type LeftSplit[YI <: Chain, YL] =
					(I sql_=> YI, L sql_=> YL, SQLTransformation.Returning[YI ~ YL, U, leftResult.Expression])
				type RightSplit[YI <: Listing, YL] =
					(I2 sql_=> YI, L2 sql_=> YL, SQLTransformation.Returning[YI |~ (K :~ YL), U, rightResult.Expression])
				(splitChainTransformation(leftResult) :Opt[LeftSplit[_, _]],
					splitListingTransformation(listingResult) :Opt[RightSplit[_, _]]
				) match {
					case (Lack, _) =>
						throw new MismatchedExpressionsException(
							ChainSQL.this, other, "unsupported left conversion type " + leftResult + "."
						)
					case (_, Lack) =>
						throw new MismatchedExpressionsException(
							ChainSQL.this, other, "unsupported right conversion type " + rightResult + "."
						)
					case (left :Opt[LeftSplit[Chain, Any]] @unchecked, right :Opt[RightSplit[Listing, Any]] @unchecked)
					if left.isDefined && right.isDefined && isUpcast(left.get._3) && isUpcast(right.get._3) &&
						right.get._1.isIdentity => //the last condition is so we can cast reformed rightInit to Listing

						implicit val (leftInitResult, leftLastResult, leftPost) = left.get
						implicit val (rightInitResult, rightLastResult, rightPost) = right.get
						val (leftLast, rightLast) = reform(self.last, e.last)(leftLastResult, rightLastResult, spelling)
						val (leftInit, rightInit) = reform(self.init, e.init)(
							leftInitResult, rightInitResult andThen upcastListing, spelling
						)
						val leftReformed = leftInit ~ leftLast
						val rightRebuildAttempt = IndexedSQL.attempt[F2, S2, I2, K, Any](
							//the cast is safe-ish because rightInitResult.isIdentity
							rightInit.asInstanceOf[SQLExpression[F2, S2, I2]], e.lastKey, rightLast
						)
						val rightReformed = rightRebuildAttempt match {
							case Lack if !rightInit.isInstanceOf[IndexedSQL[_, _, _]] =>
								throw new MismatchedExpressionsException(thisExpression, e,
									"received a non-IndexedSQL expression " + rightInit +
										": " + rightInit.getClass.getName + " as a partial result."
								)
							case Lack =>
								throw new MismatchedExpressionsException(thisExpression, e,
									"Reformed last expression " + rightLast +
										" is neither a ColumnSQL nor a LabeledValueSQL."
								)
							case Got(res) =>
								res.castFrom[
									IndexedSQL[F2, S2, I2 |~ (K :~ Any)], ConvertibleSQL[F2, S2, Listing |~ (K :~ Any), EC2]
								]
						}
						(leftPost(leftReformed), rightPost[F2, S2, EC2](rightReformed))
				}
			}
		}
	}

	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[I ~ L] =
		spelling.effectiveForm(init) ~ spelling.effectiveForm(last)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, I ~ L] =
		visitor.chain(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, I ~ L, Y]) :Y = visitor.chain(this)


	override def sameAs(other :CompositeSQL.__) :Boolean = other.isInstanceOf[ChainSQL[_, _, _, _]]

	override def toString :String = inOrder.mkString("(", " ~ ", ")")
}



object ChainSQL {

//	/** [[net.noresttherein.oldsql.sql.ast.EmptySQL EmptySQL]] as an instance of `SQLExpression`. */
//	val EmptyChain :SQLExpression[RowProduct, Single, @~] = ChainTuple.EmptyChain

	def apply[F <: RowProduct, S >: Grouped <: Single, I <: Chain, L]
	         (init :SQLExpression[F, S, I], last :SQLExpression[F, S, L]) :ChainSQL[F, S, I, L] =
		init match {
			case tuple :ChainTuple[F, S, I] =>
				new NonEmptyChainTuple(tuple, last)
			case _ =>
				new ChainSQL(init, last) with RowShapeCache
		}
//
//	private class ChainSQLImpl[-F <: RowProduct, -S >: Grouped <: Single, I <: Chain, L]
//	                          (init :SQLExpression[F, S, I], last :SQLExpression[F, S, L])
//		extends ChainSQL(init, last) with InlineBinaryCompositeSQL[F, S, I, L, I ~ L] with RowShapeCache

	type __ = ChainSQL[_ <: RowProduct, _ >: Grouped <: Single, _ <: Chain, _]


	/** Splits a transformation of a non-empty chain into transformations for its `init` and `last`.
	  * If the method returns `Got`, then the third conversion is actually an identity.
	  */
	private[ast] def splitChainTransformation[XI <: Chain, XL, Z](conversion :SQLTransformation[XI ~ XL, Z])
			:Opt[(XI sql_=> YI, XL sql_=> YL, SQLTransformation.Returning[YI ~ YL, Z, conversion.Expression])
				forSome { type YI <: Chain; type YL }
			] =
	{
		type Result[Y] = SQLTransformation.Into[Y, Z, conversion.Expression]
		type Composed[Y] = (SQLConversion[XI ~ XL, Y], Result[Y])
		conversion match {
			case _ if conversion.isIdentity =>
				Got((SQLConversion.toSelf[XI], SQLConversion.toSelf[XL], conversion :Result[XI ~ XL]))
			case _ :Upcast[XI ~ XL, Z] =>
				Got((upcastChain.castParams[XI, XI], upcastAny.castParams[XL, XL], conversion :Result[XI ~ XL]))
//				Got((SQLConversion.toSelf[XI], SQLConversion.toSelf[XL], conversion :conversion.type))
			case chain :ChainConversion[XI, XL, yi, yl] =>
				Got((chain.init, chain.last, SQLConversion.toSelf.asInstanceOf[Result[yi ~ yl]]))
			//if conversion is composed of only ChainConversion, we can split each and compose each item individually
			case _ => SQLTransformation.Composition.WithConversion.unapply(conversion) match {
				//a way of ensuring the intermediate existential types match and we can recompose
				case composed :Opt[Composed[y]] if composed.isDefined =>
					type Y = y
					type SplitFirst[YI <: Chain, YL, Y] = (XI sql_=> YI, XL sql_=> YL, SQLTransformation[YI ~ YL, Y])
					splitChainTransformation(composed.get._1 :SQLTransformation[XI ~ XL, Y]) match {
						case splitFirst :Opt[SplitFirst[yi, yl, Y]] if splitFirst.isDefined =>
							type YI = yi; type YL = yl
							type SplitSecond[ZI <: Chain, ZL] = (YI sql_=> ZI, YL sql_=> ZL, Result[ZI ~ ZL])
							val firstInit :XI sql_=> YI = splitFirst.get._1
							val firstLast :XL sql_=> YL = splitFirst.get._2
							val firstWhole :SQLTransformation[YI ~ YL, Y] = splitFirst.get._3
							firstWhole match {
								case conversion :SQLConversion[YI ~ YL, Y] if conversion.isIdentity =>
									splitChainTransformation(firstWhole andThen composed.get._2) match {
										case splitSecond :Opt[SplitSecond[zi, zl]] if splitSecond.isDefined =>
											val initResult     = (splitFirst.get._1 :XI sql_=> YI) andThen splitSecond.get._1
											val lastResult     = (splitFirst.get._2 :XL sql_=> YL) andThen splitSecond.get._2
											val combinedResult = splitSecond.get._3
											Got((initResult, lastResult, combinedResult))
										case _ =>
											//The compiler doesn't know that SQLResult is preserved  during composition
											// because the functions were already composed in conversion to start with.
											val whole = (firstWhole andThen composed.get._2).asInstanceOf[Result[YI ~ YL]]
											Got((firstInit, firstLast, whole))
									}
								case _ =>
									//The compiler doesn't know that SQLResult is preserved  during composition
									// because the functions were already composed in conversion to start with.
									val whole = (firstWhole andThen composed.get._2).asInstanceOf[Result[YI ~ YL]]
									Got((firstInit, firstLast, whole))
							}
						case _ => Lack
					}
				case _ => Lack
			}
		}
	}

	//used in reforming
	private val upcastChain   = SQLConversion.supertype[Chain, Chain]
	private val upcastListing = SQLConversion.supertype[Listing, Chain]
	private val upcastAny     = SQLConversion.supertype[Any, Any]

	trait SpecificChainVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends SpecificEmptyVisitor[X, Y] {
		def chain[I <: Chain, L](e :ChainSQL[F, S, I, L])(implicit isChain :X =:= (I ~ L)) :Y
	}
	//or should match extend CaseSpecificChainTuple?
	type MatchSpecificChain[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificChainVisitor[F, S, X, Y]

	trait CaseSpecificChain[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificChain[F, S, X, Y] with CaseSpecificChainTuple[F, S, X, Y]
	{
		override def chainTuple[C <: Chain](e :ChainTuple[F, S, C])(implicit isChain :X =:= C) :Y =
			e match {
				case chain :ChainSQL[F, S, i, l] => this.chain(chain)
				case EmptySQL => empty(isChain.asInstanceOf[X =:= @~])
			}
	}
//
//
//	trait ChainVisitor[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] {
//		def chain[S >: Grouped <: Single, I <: Chain, L](e :ChainSQL[F, S, I, L]) :R[S, I ~ L, ChainSQL[F, S, I, L]]
//	}
//	type MatchChain[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//		ChainVisitor[F, R]
//	type CaseChain[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//		ChainVisitor[F, R] with ChainTuple.CaseChain[F, R] //extends a protected trait

	trait AnyChainVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnyEmptyVisitor[Y] {
		//todo: make a common mixin CaseAnyXxx with MatchAnyChainTuple
		def chain[S >: Grouped <: Single, I <: Chain, L](e :ChainSQL[F, S, I, L]) :Y[S, I ~ L]
	}
	type MatchAnyChain[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyChainVisitor[F, Y]

	trait CaseAnyChain[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyChain[F, Y] with CaseAnyChainTuple[F, Y] //extends a protected trait
	{
//		override def emptyChain :Y[Single, @~] = empty
		override def chainTuple[S >: Grouped <: Single, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] =
			(e match {
				case chain :ChainSQL[F, S, i, l] => this.chain(chain)
				case EmptySQL => empty
			}).asInstanceOf[Y[S, X]]
	}
}






/** A tuple expression implementation with flexible length, mapping nominally
  * to a [[net.noresttherein.oldsql.collection.Chain Chain]] subtype containing the member expressions.
  */ //todo: a conversion from tuples of SQLExpressions
//todo: rename to TupleSQL
sealed trait ChainTuple[-F <: RowProduct, -S >: Grouped <: Single, V <: Chain]
	extends InlineSQL[F, S, V] //does not extend GroundingCompositeSQL because it causes clash with ChainSQL in ChainEntry
//	   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = ChainTuple[f, S, V] })#E]
	   with CompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = ChainTuple[f, S, V] })#E]
	   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = ChainTuple[f, S, V] })#E]
//	   with ReorderingTemplate[F, S, V, ChainTuple[F, S, V]]
{
//	override type Item[-E <: RowProduct, -D >: Grouped <: Single, X] = InlineItem[E, D, X]

	/* These methods are implemented by casting here because in ChainEntry identifiers init and last
	 * conflict between these methods and fields inherited from ChainSQL.
	 */
	def init[I <: Chain, L](implicit nonEmpty :V =:= (I ~ L)) :ChainTuple[F, S, I] =
		this.asInstanceOf[NonEmptyChainTuple[F, S, I, _]].init //defined here because in ChainEntry would conflict with ChainSQL.init

	def last[I <: Chain, L](implicit nonEmpty :V =:= (I ~ L)) :SQLExpression[F, S, L] =
		this.asInstanceOf[NonEmptyChainTuple[F, S, _, L]].last

	def toChainSQL[I <: Chain, L](implicit nonEmpty :V =:= (I ~ L)) :ChainSQL[F, S, I, L] =
		this match {
			case entry :NonEmptyChainTuple[F @unchecked, S @unchecked, I @unchecked, L @unchecked] => entry
			case _ => throw new UnsupportedOperationException("Cannot convert an empty ChainTuple to ChainSQL.")
		}

	def ~[E <: F, O >: Grouped <: S, H](head :SQLExpression[E, O, H]) :ChainTuple[E, O, V ~ H] =
		new NonEmptyChainTuple(this, head)

	//ChainEntry.parts is just init and last expressions
//	protected override def parts :Seq[SQLExpression[F, S, _]] = {
//		@tailrec def rec(e :ChainTuple[F, S, _], acc :List[SQLExpression[F, S, _]] = Nil)
//				:Seq[SQLExpression[F, S, _]] =
//			e match {
//				case ChainEntry(tail, head) => rec(tail, head::acc)
//				case _ => acc
//			}
//		rec(this)
//	}

	override def asSingleRow :Option[ChainTuple[F, Single, V]] =
		if (isSingleRow) Some(this.asInstanceOf[ChainTuple[F, Single, V]])
		else None

//	//The following methods are overriden in ChainEntry and EmptyChain, so aren't really used,
//	// but we need to override them here because CompositeSQL implements them.
//	override def anchor(from :F) :ChainTuple[F, S, V] = rephrase(SQLScribe.anchor(from))
//
//	override def basedOn[U <: F, E <: RowProduct]
//	                    (base :E)(implicit expansion :U PartOf E) :ChainTuple[E, S, V] =
//		rephrase(SQLScribe.expand(base))
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :ChainTuple[E, S, V] =
//		rephrase(SQLScribe.expand(base))
//
//	//overriden to narrow down the result type
//	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ChainTuple[E, S, V]

	override def rebuild[E <: RowProduct, A >: Grouped <: Single]
	                    (items :({ type T[X] = Item[F, S, X] })#T =>: SQLExpression.from[E]#rows[A]#E)
			:ChainTuple[E, A, V]

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] = visitor.chainTuple(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y = visitor.chainTuple(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F, S, Seq[V]] <: SQLExpression[F_, S_, V],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, V, E] =
//		visitor.chainTuple(this)

//	override def toString :String = inOrder.mkString("(", " ~ ", ")")
}



object ChainTuple {

	def apply() :ChainTuple[RowProduct, Single, @~] = EmptySQL

	def apply[F <: RowProduct, S >: Grouped <: Single, T](e :SQLExpression[F, S, T]) :ChainTuple[F, S, @~ ~ T] =
		new NonEmptyChainTuple(EmptySQL, e)

//	/** [[net.noresttherein.oldsql.sql.ast.EmptySQL EmptySQL]] as an instance of `ChainTuple`. */
//	val EmptyChain :ChainTuple[RowProduct, Single, @~] = EmptySQL


	trait SpecificChainTupleVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
//		extends SpecificEmptyVisitor[X, Y]
	{
		def chainTuple[C <: Chain](e :ChainTuple[F, S, C])(implicit isChain :X =:= C) :Y
	}
	trait MatchSpecificChainTuple[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificChainTupleVisitor[F, S, X, Y]
	{
		def emptyChain(implicit ev :X =:= @~) :Y
		def chainItem[I <: Chain, L](init :ChainTuple[F, S, I], last :InlineItem[F, S, L])
		                            (implicit isChain :X =:= (I ~ L)) :Y

		override def chainTuple[C <: Chain](e :ChainTuple[F, S, C])(implicit isChain :X =:= C) :Y =
			e match {
				case tuple :NonEmptyChainTuple[F, S, i, l] =>
					chainItem(tuple.init, tuple.item)(isChain.asInstanceOf[X =:= (i ~ l)])
				case _ => emptyChain(isChain.castParam2[@~])
			}
	}
	type CaseSpecificChainTuple[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificChainTupleVisitor[F, S, X, Y]

//
//
//	trait ChainTupleVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def chainTuple[S >: Grouped <: Single, V <: Chain](e :ChainTuple[F, S, V]) :R[S, V, ChainTuple[F, S, V]]
//	}
//	trait MatchChainTuple[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends ChainTupleVisitor[F, R] with EmptyChainVisitor[R]
//	{
//		def chainItem[S >: Grouped <: Single, I <: Chain, L]
//		             (e :ChainTuple[F, S, I ~ L]) :R[S, I ~ L, ChainTuple[F, S, I ~ L]]
//
//		override def chainTuple[S >: Grouped <: Single, V <: Chain]
//		                       (e :ChainTuple[F, S, V]) :R[S, V, ChainTuple[F, S, V]] =
//			(e match {
//				case tuple :ChainEntry[F, S, i, l] => chainItem(tuple)
//				case _ => emptyChain
//			}).asInstanceOf[R[S, V, ChainTuple[F, S, V]]]
//	}
//	type CaseChainTuple[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		ChainTupleVisitor[F, R]


	trait AnyChainTupleVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] { //extends AnyEmptyVisitor[Y] {
		def chainTuple[S >: Grouped <: Single, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X]
	}
	trait MatchAnyChainTuple[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyChainTupleVisitor[F, Y]
	{
		def emptyChain :Y[Single, @~]
		def tupleItem[S >: Grouped <: Single, I <: Chain, L]
		             (init :ChainTuple[F, S, I], last :InlineItem[F, S, L]) :Y[S, I ~ L]

		override def chainTuple[S >: Grouped <: Single, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] =
			(e match {
				case tuple :NonEmptyChainTuple[F, S, i, l] => tupleItem(tuple.init, tuple.item)
				case _ => emptyChain
			}).asInstanceOf[Y[S, X]]
	}
	//Or we could extend MatchAnyChainTuple for uniformity. The result is however the same, including ChainSQL visitors,
	// unless someone decides to mix in both MatchAnyChainTuple and CaseAnyChainTuple/CaseAnyChain
	type CaseAnyChainTuple[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyChainTupleVisitor[F, Y]
}




//Does not extend BinaryCompositeTemplate, so that reapply/rephrase can return ChainSQL for non ChainTuple arguments
private final class NonEmptyChainTuple[-F <: RowProduct, -S >: Grouped <: Single, I <: Chain, L]
                                      (override val init :ChainTuple[F, S, I], override val last :SQLExpression[F, S, L])
	extends ChainSQL[F, S, I, L](init, last)
	   with ChainTuple[F, S, I ~ L] with RowShapeCache
	   with VariantGroundingTemplate[F, S, I ~ L, ({ type E[-f <: RowProduct] =
	                                                         ChainSQL[f, S, I, L] with ChainTuple[f, S, I ~ L] })#E]
//		   with BinaryCompositeTemplate[F, S, I, L, I ~ L, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] =
//		                                                           ChainSQL[f, S, I, L] with ChainTuple[f, s, I ~ L] })]
//		   with GroundingCompositeSQLTemplate[F, S, I ~ L, ({ type E[-f <: RowProduct] =
//		                                                         ChainSQL[f, S, I, L] with ChainTuple[f, S, I ~ L] })#E]
//		   with SpecificBinaryCompositeSQL[F, S, I, ({ type E[-f <: RowProduct] = ChainTuple[f, S, I] })#E,
//		                                         L, ({ type E[-f <: RowProduct] = SQLExpression[f, S, L] })#E,
//		                                         I ~ L, ({ type E[-f <: RowProduct] = ChainEntry[f, S, I, L] })#E]
{
	override type Item[-E <: RowProduct, -D >: Grouped <: Single, X] = InlineItem[E, D, X]

	override def size         :Int = item.index + 1
	val item = InlineItem(init.size, last)
	override lazy val items   :IndexedSeq[Item[F, S, _]] = init.items :+ item
	override lazy val toSeq   :Seq[SQLExpression[F, S, _]] = init.toSeq :+ last
	/* this.parts = Seq(init, last) (inherited from ChainSQL).
	 * This breaks the contract of inOrder being sorted parts, but this is a private class
	 * and parts is a protected method, so cannot be called from outside code.
	 */
//	protected override lazy val parts :Seq[SQLExpression[F, S, _]] = init.toSeq :+ last

	override def extracts[E <: F, A >: Grouped <: S]
			:Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[I ~ L]#to, _]] =
		_extracts.asInstanceOf[Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[I ~ L]#to, _]]]

	private[this] lazy val _extracts :Seq[Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I ~ L]#to, _]] = {
		def map[X](entry :Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I]#to, X]) =
			Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I ~ L]#to, X](
				entry._1, entry._2 compose Chain.init[I] _
			)
		init.extracts.map {
			(entry :Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I]#to, _]) => map(entry)
		} :+ Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I ~ L]#to, L](last, Chain.last[L] _)
	}

	override def construct(items :({ type T[X] = InlineItem[F, S, X] })#T =>: generic.Self) :I ~ L =
		init.construct(items) ~ items(item)

	override def rebuild[E <: RowProduct, A >: Grouped <: Single]
                        (items :({ type T[X] = Item[F, S, X] })#T =>: SQLExpression.from[E]#rows[A]#E)
			:ChainTuple[E, A, I ~ L] =
		init.rebuild(items) ~ items(item)

	override def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, S, I ~ L] =
		if (permutation == permutation.indices)
			this
		else if (permutation != permutation.sorted)
			throw new IllegalArgumentException(
				"Cannot reorder record `" + this + "` according to permutation " + permutation +
					" because it is of a wrong size: expected " + size + ", but got " + permutation.size + "."
			)
		else {
			val inverse = inversePermutation(permutation)
			val values = toSeq
			def reorder(itemNo :Int) :ChainTuple[F, S, _ <: Chain] =
				if (itemNo < 0)
					ChainTuple()
				else {
					val init = reorder(itemNo - 1)
					init ~ values(inverse(itemNo))
				}
			val reordered = reorder(size - 1).castFrom[ChainTuple[F, S, _], ChainTuple[F, S, Chain]]
			val convert = new ChainReordering[Chain, I ~ L](permutation)
			convert(reordered)
		}

	override def asSingleRow :Option[NonEmptyChainTuple[F, Single, I, L]] =
		if (isSingleRow) Some(this.castFrom[NonEmptyChainTuple[F, S, I, L], NonEmptyChainTuple[F, Single, I, L]])
		else None

	override def anchor(from :F) :NonEmptyChainTuple[F, S, I, L] =
		if (isAnchored)
			this
		else (init.anchor(from), last.anchor(from)) match {
			case (i, l) if (i eq init) && (l eq last) => this
			case (i, l) => new NonEmptyChainTuple(i, l)
		}

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :NonEmptyChainTuple[E, S, I, L] =
		new NonEmptyChainTuple(init.basedOn(base), last.basedOn(base))

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :NonEmptyChainTuple[E, S, I, L] =
		new NonEmptyChainTuple(init.expand(base), last.expand(base))


	/* This short-circuits the call, resulting in no-callbacks for the prefixes. In particular,
	 * a scribe gets only a single chance to handle a ChainTuple and if it defers to rephrase,
	 * it will built another tuple. Lets call it a feature. The override is necessary because the return type
	 * is different in ChainSQL and ChainTuple
	 */
	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :NonEmptyChainTuple[E, S, I, L] =
		new NonEmptyChainTuple(init.rephrase(mapper), mapper(last))

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, I], right :SQLExpression[E, C, L]) :ChainSQL[E, C, I, L] =
		left match {
			case tuple :ChainTuple[E, C, I] => new NonEmptyChainTuple(tuple, right)
			case _ => ChainSQL(left, right)
		}

//			protected override def visit[Y[-_ >: Grouped <: GlobalScope, _]](visitor :AnyExpressionVisitor[F, Y]) =
//				visitor.tupleItem(init, lastItem)
//
//			protected override def visit[Y](visitor :ExpressionVisitor[F, S, I ~ L, Y]) =
//				visitor.tupleItem(init, lastItem)

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if this eq self => true
		case other :NonEmptyChainTuple[_, _, _, _] if other.canEqual(this) =>
			last == other.last && init == other.init
		case _ => false
	}
	override def hashCode :Int = init.hashCode * 31 + last.hashCode
}






sealed abstract class EmptySQL
	extends NativeTerm[@~]("")
	   with UnsealedEmptyIndexedSQL
	   with ChainTuple[RowProduct, Single, @~]
	   with CompositeSQLTemplate[RowProduct, Single, @~, ({ type E[-f <: RowProduct] = EmptySQL })#E]
	   with VariantGroundingTemplate[RowProduct, Single, @~, ({ type E[-f <: RowProduct] = EmptySQL })#E]
	   with ReorderingTemplate[RowProduct, Single, @~, EmptySQL]
{
	override type LastKey   = Nothing
	override type LastValue = Nothing
	override type LabeledItem[-F <: RowProduct, -S >: Grouped <: Single, N <: Label, V] = Nothing
	protected override def parts :Seq[Nothing] = PassedArray.empty
	override def toSeq   :Seq[Nothing] = PassedArray.empty
	override def inOrder :Seq[Nothing] = PassedArray.empty
	override def items   :IndexedSeq[Nothing] = PassedArray.empty
	override def keys    :IndexedSeq[Label] = PassedArray.empty
	override def paths   :Seq[LabelPath[_]] = PassedArray.empty
	override def index   :Map[LabelPath[_], LabeledValueSQL[RowProduct, Single, _]] = Map.empty
	override def columns :Map[LabelPath[_], LabeledColumnSQL[RowProduct, Single, _ <: Label, _]] = Map.empty
	override def toMap   :Map[String, LabeledValueSQL[RowProduct, Single, _]] = Map.empty
	override def size    = 0

	override def lastItem(implicit notEmpty :SplitRecord[@~]) :Nothing =
		throw new NoSuchElementException("EmptySQL.lastItem: do not cheat the type system!")

	override def extracts[E <: RowProduct, A >: Grouped <: Single]
			:Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[@~]#to, _]] =
		PassedArray.empty

	override def isSingleRow = true
	override val asSingleRow :Option[this.type] = Some(this)
	override val groundValue :Opt[@~] = Got(@~)
	override def isGround    = true
	override def asGround    :Option[this.type] = asSingleRow
	override def isAnchored  = true
	override def isAnchored(from :RowProduct) = true
	override def anchor(from :RowProduct) :this.type = this

	override def apply[K <: Label](key :K)(implicit get :RecordField[@~, K]) =
		throw new UnsupportedOperationException("EmptySQL does not have elements: " + key + ".")

	override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :EmptySQL = this

	override def expand[U <: RowProduct, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single) :EmptySQL = this

	override def rephrase[E <: RowProduct](mapper :SQLScribe[RowProduct, E]) :EmptySQL = this

	//IndexedSQL methods

	override def construct(items :({ type T[X] = Item[RowProduct, Single, X] })#T =>: generic.Self): @~ = @~
	override def rebuild[E <: RowProduct, A >: Grouped <: Single]
	                    (items :({type T[X] = Item[RowProduct, Single, X]})#T =>: SQLExpression.from[E]#rows[A]#E)
			:EmptySQL =
		this

	private[sql] override def rephraseAsLabeledValue[E <: RowProduct](mapper :SQLScribe[RowProduct, E]) = this

	override def nullSQL   :IndexedSQL[RowProduct, Single, @~] = this
	override def nullValue : Opt[@~] = Got(@~)

	override def reorder(permutation :IndexedSeq[Int]) :SQLExpression[RowProduct, Single, @~] =
		if (permutation.isEmpty)
			this
		else
			throw new IllegalArgumentException(
				"Cannot reorder an empty record to a non-empty permutation: " + permutation + "."
			)

	override def reorder(keys :Unique[String]) :SQLExpression[RowProduct, Single, @~] =
		if (keys.isEmpty)
			this
		else
			throw new IllegalArgumentException(
				"Cannot reorder an empty record to a non-empty key order: " + keys + "."
			)
	override def reorder(paths :Seq[LabelPath[_]]) :SQLExpression[RowProduct, Single, @~] =
		if (paths.isEmpty)
			this
		else
			throw new IllegalArgumentException("Attempted to reorder an empty record to " + paths + ".")

	override def project(paths :Seq[LabelPath[_]]) :SQLExpression[RowProduct, Single, @~] = reorder(paths)

	override def reform[E <: RowProduct, A >: Grouped <: Single]
	                   (paths :Seq[(LabelPath[_], LabeledValueSQL[E, A, _])]) :SQLExpression[E, A, @~] =
	{
		def reformed(suffixes :Seq[(List[String], LabeledValueSQL[E, A, _])])
				:LabeledValueSQL[E, A, _] =
		{
			val lastIndices = (Map.empty[String, Int] /: suffixes.zipWithIndex) {
				case (lastIndex, ((key::_, _), i)) =>
					if (lastIndex.get(key).exists(_ != i - 1))
						throw new IllegalArgumentException(
							"Cannot project an empty record to " + paths + " because the paths are not grouped by leading keys."
						)
						lastIndex.updated(key, i)
			}
			val keyOrder = lastIndices.toIndexedSeq.sortBy(_._2).map(_._1)
			val fields = suffixes.groupBy(_._1.head)//.view.mapValues { case (path, expr) => (path.tail, expr) }

			val orderedFields = keyOrder.view.map(key => (key, fields(key))).toList
			def expand(fields :List[(String, Seq[(List[String], LabeledValueSQL[E, A, _])])], acc :IndexedSQL[E, A, _])
					:IndexedSQL[E, A, _] =
				fields match {
					case (key, paths)::t =>
						val topLevel = paths.filter(_._1.isEmpty)
						if (topLevel.sizeIs >= 1 && paths.sizeIs > 1)
							throw new IllegalArgumentException("Duplicate paths or a prefix path present in " + paths + ".")
						if (topLevel.sizeIs == 1)
							expand(t, (acc |~ :~[key.type](paths.head._2))(new ValueOf(key)))
						else {
							val pathTails = paths.map { case (path, placeholder) => (path.tail, placeholder) }
							expand(t, (acc  |~ :~[key.type](reformed(pathTails)))(new ValueOf(key)))
						}
					case _ => acc
				}
			expand(orderedFields, EmptySQL)
//			((EmptySQL :IndexedSQL[E, A, _]) /: orderedFields) { (acc, field) =>
//				val (key, paths) = field
//				val topLevel = paths.filter(_._1.isEmpty)
//				if (topLevel.sizeIs >= 1 && paths.sizeIs > 1)
//					throw new IllegalArgumentException("Duplicate paths or a prefix path present in " + paths + ".")
//				val expanded :IndexedSQL[E, A, _] =
//					if (topLevel.sizeIs == 1)
//						(acc |~ :~[key.type](paths.head._2))(new ValueOf(key))
//					else {
//						val pathTails = paths.map { case (path, placeholder) => (path.tail, placeholder) }
//						(acc  |~ :~[key.type](reformed(pathTails)))(new ValueOf(key))
//					}
//				expanded
//			}
		}
		val expr = reformed(paths.map { case (path, placeholder) => (path.toList, placeholder) })
			.asInstanceOf[LabeledValueSQL[E, A, Listing]]
		val conversion = SQLConversion.opt[Listing, @~](".return(@~)", (_ :Listing) => @~, (_: @~) => expr.nullValue)
		conversion(expr)
	}

	protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :this.type =
		//todo: null columns padding if underlyingColumnCount > 0
		if (reordering.columnCount != 0 || reordering.underlyingColumnCount != 0)
			throw new IllegalArgumentException("Cannot reorder EmptySQL with non empty reordering " + reordering + ".")
		else
			this

	protected override def potentialColumns(permissions :Permissions)(implicit spelling :SQLSpelling) :AlignableColumns =
		AlignableColumns.none(this)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int = 0

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[RowProduct, Single, _]] =
		PassedArray.empty

	protected override def shape(implicit spelling :SQLSpelling) :RowShape = RowShape()
	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[@~] = SQLForm[@~]
	protected override def columnCount(implicit spelling :SQLSpelling) :Int = 0

	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		SpelledSQL(context)

	protected override def explodedSpelling[P]
	                       (independent :Boolean)
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		PassedArray.empty


//		private[ast] trait EmptyChainVisitor[+R[-S >: Grouped <: Single, V, -E <: SQLExpression[RowProduct, S, V]]] {
//			def emptyChain :R[Single, @~, ChainTuple[RowProduct, Single, @~]]
//		}

	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[RowProduct, Y]) :Y[Single, @~] =
		visitor.empty

	protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, @~, Y]) :Y =
		visitor.empty

}




object EmptySQL extends EmptySQL {
	trait SpecificEmptyVisitor[X, +Y] {
		def empty(implicit isEmpty :X =:= @~) :Y
	}
	trait AnyEmptyVisitor[+Y[-_ >: Grouped <: Single, _]] {
		def empty :Y[Single, @~]
	}
}
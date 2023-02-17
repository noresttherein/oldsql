package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Chain, Listing, Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.MismatchedExpressionsException
import net.noresttherein.oldsql.morsels.{generic, Extractor}
import net.noresttherein.oldsql.morsels.generic.=>:
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.bits.LabelPath
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.slang.{cast2TypeParams, cast3TypeParams}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ChainSQL.{splitChainTransformation, upcastListing, CaseSpecificChain}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.{BinaryCompositeSQL, GroundingCompositeSQLTemplate}
import net.noresttherein.oldsql.sql.ast.ChainTuple.{CaseAnyChainTuple, CaseSpecificChainTuple, ChainEntry}
import net.noresttherein.oldsql.sql.ast.EmptySQL.{AnyEmptyVisitor, SpecificEmptyVisitor}
import net.noresttherein.oldsql.sql.ast.InlineSQL.InlineItem
import net.noresttherein.oldsql.sql.ast.LabeledSQL.{splitListingTransformation, EmptyLabeled, LabeledValueSQL, MatchSpecificLabeled}
import net.noresttherein.oldsql.sql.mechanics.{sql_=>, Reform, SpelledSQL, SQLConversion, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.{ConvertChain, Upcast}






//todo: create a similar class for Listing
/** An SQL expression for a non empty [[net.noresttherein.oldsql.collection.Chain Chain]].
  * It is a composite of [[net.noresttherein.oldsql.sql.ast.ChainSQL.init init]], an expression for all
  * elements of the chain except for the last, and [[net.noresttherein.oldsql.sql.ast.ChainSQL.last last]],
  * an expression for the last element in the chain.
  *
  * This class differs from the extending [[net.noresttherein.oldsql.sql.ast.ChainTuple.ChainEntry ChainEntry]]
  * in that it allows any `SQLExpression` for the initial elements of the chain, rather than
  * a [[net.noresttherein.oldsql.sql.ast.ChainTuple ChainTuple]] as the former.
  *
  * @author Marcin Mościcki
  */ //the constructor is private so that the class is effectively sealed.
case class ChainSQL[-F <: RowProduct, -S >: Grouped <: Single, I <: Chain, L] private[sql]
                   (init :SQLExpression[F, S, I], last :SQLExpression[F, S, L])
	extends BinaryCompositeSQL[F, S, I, L, I ~ L] with SelectableSQL[F, S, I ~ L]
{ self =>
	override lazy val selectForm :SQLReadForm[I ~ L] = (init.selectForm, last.selectForm) match {
		case (i :SQLForm[I @unchecked], l :SQLForm[L @unchecked]) => i ~ l
		case _ => init.selectForm ~ last.selectForm
	}

	protected override def left  :SQLExpression[F, S, I] = init
	protected override def right :SQLExpression[F, S, L] = last

	override def groundValue :Opt[I ~ L] = for { i <- init.groundValue; l <- last.groundValue } yield i ~ l

	override def anchor(from :F) :ChainSQL[F, S, I, L] =
		(init.anchor(from), last.anchor(from)) match {
			case (i, l) if (i eq init) && (l eq last) => this
			case (i, l) => new ChainSQL(i, l)
		}

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, I], right :SQLExpression[E, C, L]) :ChainSQL[E, C, I, L] =
		ChainSQL(left, right)


	protected override def reformer[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                               (implicit leftResult :SQLTransformation[I ~ L, U], rightResult :SQLTransformation[V2, U],
	                                         spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
	{
		type Reformed = (leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]])
		new BaseReformer[F1, S1, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](
		                 other)(reform, passCount)(leftResult, rightResult, spelling)
			with CaseSpecificChain[F2, S2, V2, Reformed] with MatchSpecificLabeled[F2, S2, V2, Reformed]
		{
			private def isUpcast(conversion :SQLTransformation[_, _]) =
				conversion.isIdentity || conversion.isInstanceOf[Upcast[_, _]]

			override def empty(implicit isEmpty :V2 =:= @~) =
				throw new MismatchedExpressionsException("Tuple " + ChainSQL.this + " matched with an empty tuple.")

			override def multiNull(e :MultiNull[V2]) =
				if (reform.mayReformRight ||
					reform.mayAddNullRight && rightResult.isDecorator && e.form.columnCount < selectForm.columnCount)
				{
					val rightNull = MultiNull(selectForm <> SQLWriteForm.nulls(selectForm.columnCount))
					(left, rightResult(rightNull.asInstanceOf[EC2[V2]]))
				} else
					fallback

			override def chain[I2 <: Chain, L2](e :ChainSQL[F2, S2, I2, L2])(implicit isChain :V2 =:= (I2 ~ L2)) = {
				type RightResult[A] = SQLTransformation[A, U]#Into[rightResult.SQLResult]
				implicit val chainResult = isChain.substituteCo[RightResult](rightResult)

				type LeftSplit[YI <: Chain, YL] =
					(I sql_=> YI, L sql_=> YL, SQLTransformation[YI ~ YL, U]#Into[leftResult.SQLResult])
				type RightSplit[YI <: Chain, YL] =
					(I2 sql_=> YI, L2 sql_=> YL, SQLTransformation[YI ~ YL, U]#Into[rightResult.SQLResult])
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
					case (
						Got(leftInitResult :(I sql_=> Chain) @unchecked, leftLastResult :(L sql_=> Any) @unchecked, leftPost :LeftResult[Chain ~ Any] @unchecked),
						Got(rightInitResult :(I2 sql_=> Chain) @unchecked, rightLastResult :(L2 sql_=> Any @unchecked), rightPost :LeftResult[Chain ~ Any] @unchecked)
					) if (leftPost == rightPost || isUpcast(leftPost) && isUpcast(rightPost)) =>
						/* The equality check approximates that leftPost and rightPost have the same input type, so
						 * leftInitResult/rightInitResult and leftLastResult/rightLastResult have the same output types.
						 * OTH if unification of this and other is through upcasting, we can always unify
						 * this.init with e.init and this.last with e.last by applying upcasting
						 * on top of whatever conversions they have.
						 */
						val (leftInit, rightInit) = reform(self.init, e.init)(leftInitResult, rightInitResult, spelling)
						val (leftLast, rightLast) = reform(self.last, e.last)(leftLastResult, rightLastResult, spelling)
						val leftReformed = leftInit ~ leftLast
						val rightReformed = rightInit ~ rightLast
						(leftPost(leftReformed), rightPost(rightReformed))
				}

			}

			override def labeledItem[I2 <: Listing, K <: Label, L2]
			                        (e :LabeledSQL[F2, S2, I2 |~ (K :~ L2)])
			                        (implicit isListing :V2 =:= (I2 |~ (K :~ L2))) =
			{
				type RightResult[A] = SQLTransformation[A, U]#Into[rightResult.SQLResult]
				implicit val listingResult = isListing.substituteCo[RightResult](rightResult)

				type LeftSplit[YI <: Chain, YL] =
					(I sql_=> YI, L sql_=> YL, SQLTransformation[YI ~ YL, U]#Into[leftResult.SQLResult])
				type RightSplit[YI <: Listing, YL] =
					(I2 sql_=> YI, L2 sql_=> YL, SQLTransformation[YI |~ (K :~ YL), U]#Into[rightResult.SQLResult])
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
						val rightRebuildAttempt = LabeledSQL.attempt[F2, S2, I2, K, Any](
							//the cast is safe-ish because rightInitResult.isIdentity
							rightInit.asInstanceOf[SQLExpression[F2, S2, I2]], e.lastKey, rightLast
						)
						val rightReformed = rightRebuildAttempt match {
							case Lack if !rightInit.isInstanceOf[LabeledSQL[_, _, _]] =>
								throw new MismatchedExpressionsException(thisExpression, e,
									"received a non-LabeledSQL expression " + rightInit +
										": " + rightInit.getClass.getName + " as a partial result."
								)
							case Lack =>
								throw new MismatchedExpressionsException(thisExpression, e,
									"Reformed last expression " + rightLast +
										" is neither a ColumnSQL nor a LabeledValueSQL."
								)
							case Got(res) => res

						}
						(leftPost(leftReformed), rightPost[F2, S2, EC2](rightReformed.asInstanceOf[EC2[V2]]))
				}
			}
		}
	}

//
//	protected override def reformer[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                               (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                               (implicit leftResult :Lift[I ~ L, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:SpecificExpressionVisitor[E, C, X, (SQLExpression[F, S, U], SQLExpression[E, C, U])] =
//		new BaseReformer(other)(reform, passesAllowed)
//			with CaseSpecificChain[E, C, X, (SQLExpression[F, S, U], SQLExpression[E, C, U])]
//		{
//			override def emptyChain =
//				throw new MismatchedExpressionsException("Tuple " + ChainSQL.this + " matched with an empty tuple.")
//
//			//handles also ChainTuple because we extended CaseChain; should we handle also LabeledSQL?
//			override def chain[OI <: Chain, OL](e :ChainSQL[E, C, OI, OL])(implicit isChain :X =:= (OI ~ OL)) =
//				if (!leftResult.isDerived || !rightResult.isDerived)
//					reform(leftResult(ChainSQL.this), rightResult(other))
//				else {
//					implicit val rightRes = isChain.substituteCo[({ type T[A] = Lift[A, U] })#T](rightResult)
//					type Split[T <: Chain, H, Z] = (Lift[T, Chain], Lift[H, Any], Lift[Chain ~ Any, Z])
//					def split[T <: Chain, H, Z](lift :Lift[T ~ H, Z]) :Split[T, H, Z] =
//						lift match {
//							case _ if lift.isIdentity =>
//								(Lift.self, Lift.self, Lift.self).asInstanceOf[Split[T, H, Z]]
//							case chain :LiftChain[xi, xl, yi, yl] =>
//								(chain.init, chain.last, Lift.self).asInstanceOf[Split[T, H, Z]]
//							case composed :ComposedLift[T ~ H, y, Z] =>
//								val (init, last, combined) = split(composed.first)
//								(init, last, combined andThen composed.second)
//							case _ =>
//								throw new MismatchedExpressionsException(
//									ChainSQL.this, e, "unsupported lift type " + lift + "."
//								)
//						}
//					var liftOk = false
//					try {
//						val (leftInit, leftLast, leftPost) = split(leftResult)
//						val (rightInit, rightLast, rightPost) = split(rightRes)
//						liftOk = true
//						implicit val initCompat = leftInit vs rightInit
//						implicit val lastCompat = leftLast vs rightLast
//						val (thisInit, otherInit) = reform(init, e.init)
//						val (thisLast, otherLast) = reform(last, e.last)
//						val thisReformed  = thisInit ~ thisLast
//						val otherReformed = otherInit ~ otherLast
//						(leftPost(thisReformed), rightPost(otherReformed)) //these should be ok
//					} catch {
//						case ex :Exception if !liftOk && (mayPass || !leftResult.isDerived || !rightResult.isDerived) =>
//							ex match {
//								case _ :MismatchedExpressionsException | _ :UnsupportedOperationException => try {
//									fallback
//								} catch {
//									case e :Exception => e.addSuppressed(ex); throw e
//								}
//								case _ => throw ex
//							}
//					}
//				}
//		}

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
		spelling.split(init) :++ spelling.split(last)

	protected override def shape(implicit spelling :SQLSpelling) :RowShape =
		spelling.shape(init) + spelling.shape(last)

	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		spelling.columnCount(init) + spelling.columnCount(last)

	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		spelling.sqlParamCount(init) + spelling.sqlParamCount(last)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		( "(" +: explodedSpelling(false)(from, context, params).reduce(_ +: ", " +: _)) + ")"

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
	{
		val first = spelling.explode(init, independent)(from, context, params)
		val second = first match {
			case Seq() => spelling.explode(last, independent)(from, context, params)
			case columns =>
				val prev = columns.last
				spelling.explode(last, independent)(from, prev.context, params)
		}
		first :++ second
	}


//	protected override def visit[F_ <: F, S_ >: Grouped <: Grouped,
//	                             E >: ColumnSQL[F_, S_, I ~ L] <: SQLExpression[F_, S_, I ~ L],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, I ~ L, E] =
//		visitor.chain(this)
//
	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, I ~ L] =
		visitor.chain(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, I ~ L, Y]) :Y = visitor.chain(this)


	override def sameAs(other :CompositeSQL.__) :Boolean = other.isInstanceOf[ChainSQL[_, _, _, _]]


	override def toString :String = inOrder.mkString("(", " ~ ", ")")
}



object ChainSQL {

	/** [[net.noresttherein.oldsql.sql.ast.EmptySQL EmptySQL]] as an instance of `SQLExpression`. */
	val EmptyChain :SQLExpression[RowProduct, Single, @~] = ChainTuple.EmptyChain

	def apply[F <: RowProduct, S >: Grouped <: Single, I <: Chain, L]
	         (init :SQLExpression[F, S, I], last :SQLExpression[F, S, L]) :ChainSQL[F, S, I, L] =
		new ChainSQL(init, last) with RowShapeCache


	/** Splits a transformation of a non-empty chain into transformations for its `init` and `last`.
	  * If the method returns `Got`, then the third conversion is actually an identity.
	  */
	private[ast] def splitChainTransformation[XI <: Chain, XL, Z](conversion :SQLTransformation[XI ~ XL, Z])
			:Opt[(XI sql_=> YI, XL sql_=> YL, SQLTransformation[YI ~ YL, Z]#Into[conversion.SQLResult]) forSome {
				type YI <: Chain; type YL
			}] =
	{
		type Result[Y] = SQLTransformation[Y, Z]#Into[conversion.SQLResult]
		type Composed[Y] = (SQLTransformation[XI ~ XL, Y], Result[Y])
		conversion match {
			case _ if conversion.isIdentity =>
				Got((SQLConversion.toSelf[XI], SQLConversion.toSelf[XL], conversion :Result[XI ~ XL]))
			case _ :Upcast[XI ~ XL, Z] =>
				Got((upcastChain.castParams[XI, XI], upcastAny.castParams[XL, XL], conversion :Result[XI ~ XL]))
//				Got((SQLConversion.toSelf[XI], SQLConversion.toSelf[XL], conversion :conversion.type))
			case chain :ConvertChain[XI, XL, yi, yl] =>
				Got((chain.init, chain.last, SQLConversion.toSelf.asInstanceOf[Result[yi ~ yl]]))
			//if conversion is composed of only ConvertChain, we can split each and compose each item individually
			case _ => SQLTransformation.Composition.unapply(conversion) match {
				//a way of ensuring the intermediate existential types match and we can recompose
				case composed :Opt[Composed[y]] if composed.isDefined =>
					type Y = y
					type SplitFirst[YI <: Chain, YL, Y] = (XI sql_=> YI, XL sql_=> YL, SQLTransformation[YI ~ YL, Y])
					splitChainTransformation(composed.get._1 :SQLTransformation[XI ~ XL, Y]) match {
						case splitFirst :Opt[SplitFirst[yi, yl, Y]] if splitFirst.isDefined =>
							type YI = yi; type YL = yl
							type SplitSecond[ZI, ZL] = (YI sql_=> ZI, YL sql_=> ZL, Result[ZI ~ ZL])
							//if there was need, we could drop this assert, but we'd need to check in reformer
							// if the last transformations for both expressions are equal
//							assert(splitFirst.get._3.isIdentity, splitFirst.get._3.toString + " is not identity")
							splitChainTransformation(composed.get._2.asInstanceOf[Result[YI ~ YL]]) match {
								case splitSecond :Opt[SplitSecond[zi, zl]] if splitSecond.isDefined =>
									val initResult     = (splitFirst.get._1 :XI sql_=> YI) andThen splitSecond.get._1
									val lastResult     = (splitFirst.get._2 :XL sql_=> YL) andThen splitSecond.get._2
									val combinedResult = splitSecond.get._3
									Got((initResult, lastResult, combinedResult))
								case _ => Lack
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
				case ChainTuple.EmptyChain => empty(isChain.asInstanceOf[X =:= @~])
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
				case ChainTuple.EmptyChain => empty
			}).asInstanceOf[Y[S, X]]
	}
}






/** A tuple expression implementation with flexible length, mapping nominally
  * to a [[net.noresttherein.oldsql.collection.Chain Chain]] subtype containing the member expressions.
  */ //todo: a conversion from tuples of SQLExpressions
sealed trait ChainTuple[-F <: RowProduct, -S >: Grouped <: Single, V <: Chain]
	extends InlineSQL[F, S, V]
	   with GroundingCompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = ChainTuple[f, S, V] })#E]
{
//	override type Item[-E <: RowProduct, -D >: Grouped <: Single, X] = InlineItem[E, D, X]

	/* These methods are implemented by casting here because in ChainEntry identifiers init and last
	 * conflict between these methods and fields inherited from ChainSQL.
	 */
	def init[I <: Chain](implicit nonEmpty :V <:< I ~ Any) :ChainTuple[F, S, I] =
		this.asInstanceOf[ChainEntry[F, S, I, _]].init //defined here because in ChainEntry would conflict with ChainSQL.init

	def last[L](implicit nonEmpty :V <:< Chain ~ L) :SQLExpression[F, S, L] =
		this.asInstanceOf[ChainEntry[F, S, _, L]].last

	def toChainSQL[I <: Chain, L](implicit nonEmpty :V =:= (I ~ L)) :ChainSQL[F, S, I, L] =
		this match {
			case entry :ChainEntry[F @unchecked, S @unchecked, I @unchecked, L @unchecked] => entry
			case _ => throw new UnsupportedOperationException("Cannot convert an empty ChainTuple to ChainSQL.")
		}

	def ~[E <: F, O >: Grouped <: S, H](head :SQLExpression[E, O, H]) :ChainTuple[E, O, V ~ H] =
		new ChainEntry(this, head)

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


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] = visitor.chainTuple(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y = visitor.chainTuple(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F, S, Seq[V]] <: SQLExpression[F_, S_, V],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, V, E] =
//		visitor.chainTuple(this)

	override def toString :String = inOrder.mkString("(", " ~ ", ")")
}



object ChainTuple {

	def apply() :ChainTuple[RowProduct, Single, @~] = EmptyChain

	def apply[F <: RowProduct, S >: Grouped <: Single, T](e :SQLExpression[F, S, T]) :ChainTuple[F, S, @~ ~ T] =
		new ChainEntry(EmptyChain, e)

	/** [[net.noresttherein.oldsql.sql.ast.EmptySQL EmptySQL]] as an instance of `ChainTuple`. */
	val EmptyChain :ChainTuple[RowProduct, Single, @~] = EmptySQL


	private class ChainEntry[-F <: RowProduct, -S >: Grouped <: Single, I <: Chain, L]
	                        (override val init :ChainTuple[F, S, I], override val last :SQLExpression[F, S, L])
		extends ChainSQL[F, S, I, L](init, last) with ChainTuple[F, S, I ~ L] with RowShapeCache
	{
		override type Item[-E <: RowProduct, -D >: Grouped <: Single, X] = InlineItem[E, D, X]

		override def size         :Int = item.index + 1
		val item = InlineItem(init.size, last)
		override lazy val items   :Seq[Item[F, S, _]] = init.items :+ item
		/* this.parts = Seq(init, last) (inherited from ChainSQL).
		 * This breaks the contract of inOrder being sorted parts, but this is a private class
		 * and parts is a protected method, so cannot be called from outside code.
		 */
		override lazy val inOrder :Seq[SQLExpression[F, S, _]] = init.inOrder :+ last

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

		override def anchor(from :F) :ChainEntry[F, S, I, L] =
			(init.anchor(from), last.anchor(from)) match {
				case (i, l) if (i eq init) && (l eq last) => this
				case (i, l) => new ChainEntry(i, l)
			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :ChainTuple[E, S, I ~ L] =
			init.basedOn(base) ~ last.basedOn(base)

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :ChainTuple[E, S, I ~ L] =
			init.expand(base) ~ last.expand(base)

		//this short-circuits the call, resulting in no-callbacks for the prefixes. Lets call it a feature.
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ChainTuple[E, S, I ~ L] =
			init.rephrase(mapper) ~ mapper(last)

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                              (left :SQLExpression[E, C, I], right :SQLExpression[E, C, L]) :ChainSQL[E, C, I, L] =
			left match {
				case tuple :ChainTuple[E, C, I] => new ChainEntry(tuple, right)
				case _ => ChainSQL(left, right)
			}

//			protected override def visit[Y[-_ >: Grouped <: GlobalScope, _]](visitor :AnyExpressionVisitor[F, Y]) =
//				visitor.tupleItem(init, lastItem)
//
//			protected override def visit[Y](visitor :ExpressionVisitor[F, S, I ~ L, Y]) =
//				visitor.tupleItem(init, lastItem)

		override def equals(that :Any) :Boolean = that match {
			case self  :AnyRef if this eq self => true
			case other :ChainEntry[_, _, _, _] if other.canEqual(this) =>
				last == other.last && init == other.init
			case _ => false
		}
		override def hashCode :Int = init.hashCode * 31 + last.hashCode
	}


	private object ChainEntry {
		def apply[F <: RowProduct, S >: Grouped <: Single, I <: Chain, L]
		         (init :ChainTuple[F, S, I], last :SQLExpression[F, S, L]) :ChainEntry[F, S, I, L] =
			new ChainEntry(init, last)

		def unapply[F <: RowProduct, S >: Grouped <: Single, C]
		           (e :SQLExpression[F, S, C]) :Option[(ChainTuple[F, S, _ <: Chain], SQLExpression[F, S, _])] =
			e match {
				case entry :ChainEntry[F, S, _, _] => Some((entry.init, entry.last))
				case _ => None
			}

		def unapply[F <: RowProduct, S >: Grouped <: Single, I <: Chain, L]
		           (e :ChainTuple[F, S, I ~ L]) :Option[(ChainTuple[F, S, I], SQLExpression[F, S, L])] =
			e match {
				case entry :ChainEntry[F, S, I @unchecked, L @unchecked] => Some((entry.init, entry.last))
				case _ => None
			}
	}


//	private[ast] sealed trait EmptyChainSQL extends NativeTerm[@~] with InlineSQL[RowProduct, Single, @~] {
//		override def size    = 0
//		override def toSeq   :Seq[Nothing] = PassedArray.empty
//		override def inOrder :Seq[Nothing] = PassedArray.empty
//		override def items   :Seq[Nothing] = PassedArray.empty
//		protected override def parts :Seq[Nothing] = PassedArray.empty
//
//		override def extracts[E <: RowProduct, A >: Grouped <: Single]
//				:Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[@~]#to, _]] =
//			PassedArray.empty
//
//		override def construct(items :({ type T[X] = Item[RowProduct, Single, X] })#T =>: generic.Self): @~ = @~
//
//		override def isSingleRow = true
//		override val asSingleRow :Option[this.type] = Some(this)
//		override val groundValue :Opt[@~] = Got(@~)
//		override def isGround    = true
//		override def asGround    :Option[this.type] = asSingleRow
//		override def isAnchored  = true
//		override def isAnchored(from :RowProduct) = true
//		override def anchor(from :RowProduct) :this.type = this
//
//		override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :this.type = this
//
//		override def expand[U <: RowProduct, E <: RowProduct]
//		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single) :this.type = this
//
//		override def rephrase[E <: RowProduct](mapper :SQLScribe[RowProduct, E]) :this.type = this
//
//		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[RowProduct, Single, _]] =
//			PassedArray.empty
//
//		protected override def shape(implicit spelling :SQLSpelling) :RowShape = RowShape()
//		protected override def columnCount(implicit spelling :SQLSpelling) :Int = 0
//
//		protected override def defaultSpelling[P]
//		                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
//		                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
//			SpelledSQL(context)
//
//		protected override def explodedSpelling[P]
//		                       (independent :Boolean)
//		                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
//		                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
//			PassedArray.empty
//	}


	trait SpecificChainTupleVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
//		extends SpecificEmptyVisitor[X, Y]
	{
		def chainTuple[C <: Chain](e :ChainTuple[F, S, C])(implicit isChain :X =:= C) :Y
	}
	trait MatchSpecificChainTuple[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificChainTupleVisitor[F, S, X, Y]
	{
		def emptyChain :Y
		def chainItem[I <: Chain, L](init :ChainTuple[F, S, I], last :InlineItem[F, S, L])
		                            (implicit isChain :X =:= (I ~ L)) :Y

		override def chainTuple[C <: Chain](e :ChainTuple[F, S, C])(implicit isChain :X =:= C) :Y =
			e match {
				case tuple :ChainEntry[F, S, i, l] =>
					chainItem(tuple.init, tuple.item)(isChain.asInstanceOf[X =:= (i ~ l)])
				case _ => emptyChain
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
				case tuple :ChainEntry[F, S, i, l] => tupleItem(tuple.init, tuple.item)
				case _ => emptyChain
			}).asInstanceOf[Y[S, X]]
	}
	//Or we could extend MatchAnyChainTuple for uniformity. The result is however the same, including ChainSQL visitors,
	// unless someone decides to mix in both MatchAnyChainTuple and CaseAnyChainTuple/CaseAnyChain
	type CaseAnyChainTuple[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyChainTupleVisitor[F, Y]
}




object EmptySQL
	extends NativeTerm[@~]("") with ChainTuple[RowProduct, Single, @~] with EmptyLabeled
{
	override def size    = 0
	override def toSeq   :Seq[Nothing] = PassedArray.empty
	override def inOrder :Seq[Nothing] = PassedArray.empty
	override def items   :Seq[Nothing] = PassedArray.empty
	protected override def parts :Seq[Nothing] = PassedArray.empty

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

	override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :this.type = this

	override def expand[U <: RowProduct, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single) :this.type = this

	override def rephrase[E <: RowProduct](mapper :SQLScribe[RowProduct, E]) :this.type = this

	//LabeledSQL methods

	override def construct(items :({ type T[X] = Item[RowProduct, Single, X] })#T =>: generic.Self): @~ = @~

	override def nullSQL   :LabeledSQL[RowProduct, Single, @~] = this
	override def nullValue : Opt[@~] = Got(@~)

	override def keys :Seq[Label] = PassedArray.empty

	override def reorder(keys :Unique[String]) :SQLExpression[RowProduct, Single, @~] =
		if (keys.isEmpty)
			this
		else
			throw new IllegalArgumentException(
				"Cannot reorder an empty listing to a non-empty key order: " + keys + "."
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

			val orderedFields = keyOrder.map(key => (key, fields(key)))
			((EmptySQL :LabeledSQL[E, A, _ <: Listing]) /: orderedFields) { (acc, field) =>
				val (key, paths) = field
				val topLevel = paths.filter(_._1.isEmpty)
				if (topLevel.sizeIs >= 1 && paths.sizeIs > 1)
					throw new IllegalArgumentException("Duplicate paths or a prefix path present in " + paths + ".")
				val expanded =
					if (topLevel.sizeIs == 1)
						(acc |~ :~[key.type](paths.head._2))(new ValueOf(key))
					else {
						val pathTails = paths.map { case (path, placeholder) => (path.tail, placeholder) }
						(acc  |~ :~[key.type](reformed(pathTails)))(new ValueOf(key))
					}
				expanded
			}
		}
		val expr = reformed(paths.map { case (path, placeholder) => (path.toList, placeholder) })
			.asInstanceOf[LabeledValueSQL[E, A, Listing]]
		val conversion = SQLConversion.opt[Listing, @~](".return(@~)", (_ :Listing) => @~, (_: @~) => expr.nullValue)
		conversion(expr)
	}

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[RowProduct, Single, _]] =
		PassedArray.empty

	protected override def shape(implicit spelling :SQLSpelling) :RowShape = RowShape()
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

	trait SpecificEmptyVisitor[X, +Y] {
		def empty(implicit isEmpty :X =:= @~) :Y
	}
	trait AnyEmptyVisitor[+Y[-_ >: Grouped <: Single, _]] {
		def empty :Y[Single, @~]
	}
}

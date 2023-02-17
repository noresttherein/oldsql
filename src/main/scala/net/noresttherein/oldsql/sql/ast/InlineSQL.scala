package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Chain, Listing, Opt, PassedArray}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.MismatchedExpressionsException
import net.noresttherein.oldsql.morsels.{generic, Extractor}
import net.noresttherein.oldsql.morsels.generic.{=>:, Self}
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm}
import net.noresttherein.oldsql.slang.{cast2TypeParams, mappingMethods}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ChainTuple.{AnyChainTupleVisitor, CaseAnyChainTuple, CaseSpecificChainTuple, SpecificChainTupleVisitor}
import net.noresttherein.oldsql.sql.ast.EmptySQL.SpecificEmptyVisitor
import net.noresttherein.oldsql.sql.ast.InlineSQL.InlineItem
import net.noresttherein.oldsql.sql.ast.LabeledSQL.{AnyLabeledVisitor, CaseAnyLabeled, CaseSpecificLabeled, SpecificLabeledVisitor}
import net.noresttherein.oldsql.sql.ast.SeqSQL.{AnySeqVisitor, CaseAnySeq, CaseSpecificSeq, SpecificSeqVisitor}
import net.noresttherein.oldsql.sql.mechanics.{Reform, SpelledSQL, SQLConversion, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.{ConvertSeq, Upcast}






private[sql] trait NonColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, V] extends SQLExpression[F, S, V] {
	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	( "(" +: spelling.inlineSpelling(this)(from, context, params)) + ")"
}



/** `InlineSQL` represents a pseudo SQL expression consisting of several individual expressions one after another.
  * It is a class of [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]] expressions whose column lists
  * are the concatenated column lists of their [[net.noresttherein.oldsql.sql.ast.CompositeSQL.inOrder parts]]
  * (direct subexpressions). More specifically,
  * [[net.noresttherein.oldsql.sql.SQLExpression.explodedSpelling explodedSpelling]] of any `InlineSQL` implementation
  * is equivalent to [[net.noresttherein.oldsql.sql.ast.CompositeSQL.inOrder inOrder]]`.flatMap(spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.explode explode]]`)`,
  * save for some internal details. In its literal form it is written as `(col1, col2, ..., colN)` but,
  * in most contexts, the surrounding parenthesis are omitted. The member expressions of this tuple
  * need not to be instances of `ColumnSQL`, but any multi-column expressions are inlined in the resulting `SQL`.
  *
  * Note that the value type of this expression can be any Scala type, not necessarily tuple-like.
  * It allows for implementations with various sequence-like or tuple-like value types.
  */
trait InlineSQL[-F <: RowProduct, -S >: Grouped <: Single, T]
	extends CompositeSQL[F, S, T] with SelectableSQL[F, S, T] //could extend NonColumnSQL
{
	type Item[-E <: RowProduct, -D >: Grouped <: Single, X] <: InlineItem[E, D, X]

	def size  :Int
	def toSeq :Seq[SQLExpression[F, S, _]] = inOrder
	def items :Seq[Item[F, S, _]]
	def extracts[E <: F, A >: Grouped <: S] :Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[T]#to, _]]
	def construct(items :({ type T[X] = Item[F, S, X] })#T =>: generic.Self) :T

	override def isShapeComposite :Boolean = true
	override def isInline         :Boolean = true

	protected override def shape(implicit spelling :SQLSpelling) :RowShape =
		(RowShape() /: parts)(_ + spelling.shape(_))

	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		(0 /: parts)(_ + spelling.columnCount(_))

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		explodedSpelling(false)(from, context, params) match {
			case Seq() => SpelledSQL("()", context)
			case columns => ( "(" +: columns.reduce(_ +: ", " +: _)) + ")"
		}

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		inOrder.foldLeft(List.empty[SpelledSQL[P]]) {
			(parts, item) => //we must assemble left to right for params, but the result is reversed due to prepending
				val ctx = if (parts.isEmpty) context else parts.head.context
				spelling.explode(item, independent)(from, ctx, params).toList reverse_::: parts
		}.reverse


	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, Y]) :Y[S, T] =
		visitor.inline(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, T, Y]) :Y = visitor.inline(this)

	override def sameAs(other :CompositeSQL.__) :Boolean = other.isInstanceOf[InlineSQL[_, _, _]]
}






object InlineSQL {
	/** A wrapper expression type holding a top-level tuple element together with its index
	  * (zero-based, counting left to right). Used to assemble an instance of the value type
	  * of a [[net.noresttherein.oldsql.sql.ast.InlineSQL]].
	  */
	trait InlineItem[-F <: RowProduct, -S >: Grouped <: Single, T] extends Serializable {
		def index :Int
		def value :SQLExpression[F, S, T]

		def canEqual(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case item :InlineItem[_, _, _] => //comparing whole expressions is slow, but otherwise we have no type safety
				index == item.index && hashCode == item.hashCode && value == item.value
			case _ => false
		}
		override lazy val hashCode :Int = value.hashCode * 31 + index.hashCode

		override def toString :String = "#" + index + "(" + value + ")"
	}

	object InlineItem {
		def apply[F <: RowProduct, S >: Grouped <: Single, T]
		         (index :Int, value :SQLExpression[F, S, T]) :InlineItem[F, S, T] =
			new Impl(index, value)

//		def unapply[F <: RowProduct, S >: Grouped <: GlobalScope, T]
//		           (e :SQLExpression[F, S, T]) :Opt[(Int, SQLExpression[F, S, T])] =
//			e match {
//				case item :TupleItem[F, S, T] => Got((item.index, item.value))
//				case _ => Lack
//			}

		private class Impl[-F <: RowProduct, -S >: Grouped <: Single, T]
		                  (override val index :Int, override val value :SQLExpression[F, S, T])
			extends InlineItem[F, S, T]

//		type Of[F <: RowProduct, S >: Grouped <: GlobalScope] = {
//			type T[X] = TupleItem[F, S, X]
//		}
//		type c[F <: RowProduct] = {
//			type c[S >: Grouped <: GlobalScope] = {
//				type e[X] = TupleItem[F, S, X]
//			}
//		}
	}


	//todo: true scala tuple expressions
	def unapply[F <: RowProduct, S >: Grouped <: Single](expr: SQLExpression[F, S, _])
			:Opt[Seq[SQLExpression[F, S, _]]] =
		expr match {
			case t: InlineSQL[_, _, _] => Got(t.inOrder.asInstanceOf[Seq[SQLExpression[F, S, _]]])
			case _ => Lack
		}


	trait SpecificInlineVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificChainTupleVisitor[F, S, X, Y] with SpecificLabeledVisitor[F, S, X, Y]
		   with SpecificSeqVisitor[F, S, X, Y] //with SpecificEmptyVisitor[X, Y]
	{
		def inline(e :InlineSQL[F, S, X]) :Y
	}

	trait MatchSpecificInline[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends SpecificInlineVisitor[F, S, X, Y]
		with CaseSpecificChainTuple[F, S, X, Y] with CaseSpecificLabeled[F, S, X, Y] with CaseSpecificSeq[F, S, X, Y]

	trait CaseSpecificInline[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends MatchSpecificInline[F, S, X, Y] {
		override def chainTuple[C <: Chain](e :ChainTuple[F, S, C])(implicit isChain :X =:= C) :Y =
			inline(isChain.substituteContra(e :InlineSQL[F, S, C]))

		override def labeled[V <: Listing](e :LabeledSQL[F, S, V])(implicit isListing :X =:= V) :Y =
			inline(isListing.substituteContra(e :InlineSQL[F, S, V]))

		override def seq[V](e :SeqSQL[F, S, V])(implicit isSeq :X =:= Seq[V]) :Y =
			inline(isSeq.substituteContra(e :InlineSQL[F, S, Seq[V]]))
	}
//
//
//	trait InlineVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends ChainTupleVisitor[F, R] with LabeledVisitor[F, R] with SeqVisitor[F, R]
//	{
//		def inline[S >: Grouped <: Single, V](e :InlineSQL[F, S, V]) :R[S, V, InlineSQL[F, S, V]]
//	}
//	trait MatchInline[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends InlineVisitor[F, R] with CaseChainTuple[F, R] with CaseLabeled[F, R] with CaseSeq[F, R]
//
//	trait CaseInline[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends MatchInline[F, R]
//	{
//		override def chainTuple[S >: Grouped <: Single, V <: Chain]
//		                       (e :ChainTuple[F, S, V]) :R[S, V, ChainTuple[F, S, V]] = inline(e)
//
//		override def labeled[S >: Grouped <: Single, T <: Listing]
//		                    (e :LabeledSQL[F, S, T]) :R[S, T, LabeledSQL[F, S, T]] = inline(e)
//
//		override def seq[S >: Grouped <: Single, V]
//		                (e :SeqSQL[F, S, V]) :R[S, Seq[V], SeqSQL[F, S, V]] = inline(e)
//	}


	trait AnyInlineVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyChainTupleVisitor[F, Y] with AnyLabeledVisitor[F, Y] with AnySeqVisitor[F, Y]
	{
		def inline[S >: Grouped <: Single, X](e: InlineSQL[F, S, X]): Y[S, X]
	}

	trait MatchAnyInline[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyInlineVisitor[F, Y] with CaseAnyChainTuple[F, Y] with CaseAnyLabeled[F, Y] with CaseAnySeq[F, Y]

	trait CaseAnyInline[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyInlineVisitor[F, Y] with MatchAnyInline[F, Y]
	{
		override def chainTuple[S >: Grouped <: Single, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] = inline(e)
		override def labeled[S >: Grouped <: Single, X <: Listing](e :LabeledSQL[F, S, X]) :Y[S, X] = inline(e)
		override def seq[S >: Grouped <: Single, X](e: SeqSQL[F, S, X]): Y[S, Seq[X]] = inline(e)
	}
}






trait SeqSQL[-F <: RowProduct, -S >: Grouped <: Single, T] extends InlineSQL[F, S, Seq[T]] {
	protected override def parts :Seq[SQLExpression[F, S, T]]
	override def toSeq   :Seq[SQLExpression[F, S, T]] = parts
	override def inOrder :Seq[SQLExpression[F, S, T]] = parts
	override def items   :Seq[Item[F, S, T]]
	override def size    :Int = parts.length

	override def selectForm :SQLReadForm[Seq[T]] = SQLReadForm.seq(inOrder.map(_.selectForm))

	override def extracts[E <: F, A >: Grouped <: S]
			:Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[Seq[T]]#to, _]] =
		_extracts.asInstanceOf[Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[Seq[T]]#to, _]]]

	private[this] lazy val _extracts :Seq[Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[Seq[T]]#to, _]] =
		parts.view.zipWithIndex.map { case (item, i) =>
			Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[Seq[T]]#to, T](item, (seq :Seq[T]) => seq(i))
		}.to(Seq)

	override def construct(items :({ type T[X] = Item[F, S, X] })#T =>: generic.Self) :Seq[T] =
		this.items.map(items(_))


	override def groundValue :Opt[Seq[T]] = inOrder.flatMap(_.groundValue.toOption) match {
		case res if res.length == inOrder.length => Got(res)
		case _ => Lack
	}

	override def anchor(from :F) :SeqSQL[F, S, T] = SeqSQL(parts.map(_.anchor(from)) :_*)

	override def rephrase[E <: RowProduct](mapper: SQLScribe[F, E]) :SeqSQL[E, S, T] =
		SeqSQL(inOrder.map(mapper(_)) :_*)

	protected override def reformer[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                               (implicit leftResult :SQLTransformation[Seq[T], U], rightResult :SQLTransformation[V2, U],
	                                         spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
		new BaseReformer[F1, S1, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](
			other)(reform, passCount)(leftResult, rightResult, spelling
		) {

			override def seq[I](e :SeqSQL[F2, S2, I])(implicit isSeq :V2 =:= Seq[I]) = {
				implicit val rightSeqRes =
					isSeq.substituteCo[({ type T[B] = SQLTransformation[B, U]#Into[rightResult.SQLResult] })#T](rightResult)

				type SplitResult[X, Y, Z, Res[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, Z]]] =
					(SQLConversion[X, Y], SQLTransformation[Seq[Y], Z]#Into[Res])

				/** Splits a conversion for a `SeqSQL` into a conversion for the individual elements and one applied
				  * to the whole reformed expression. */
				def splitSeqConversion[X, Z](conversion :SQLTransformation[Seq[X], Z])
						:Opt[SplitResult[X, _, Z, conversion.SQLResult]] =
//						:Opt[(SQLConversion[X, Y], SQLTransformation[Seq[Y], Z]#Into[conversion.SQLResult]) forSome { type Y }] =
				{
					type End[A] = SQLTransformation[A, Z]#Into[conversion.SQLResult]
					//this function exists because type checker can't handle existentials involved in the return type.
					def result[Y](item :SQLConversion[X, Y], seq :End[Seq[Y]]) :Opt[(SQLConversion[X, Y], End[Seq[Y]])] =
						Got((item, seq))
					conversion match {
						case seq :ConvertSeq[X, y] =>
//						case ConvertSeq(item, _, )
							result(seq.item, SQLConversion.toSelf.asInstanceOf[End[Seq[y]]])
						case _ if conversion.isIdentity => //possible really only at the start of recursion
							result(SQLConversion.toSelf[X], conversion)
						case _ if conversion.isUpcast =>
							result(SQLConversion.toSelf[X], conversion)
						case _ if conversion == SQLConversion.toRows =>
							result(SQLConversion.toSelf[X], conversion.asInstanceOf[End[Seq[X]]])
//									case _ if lift == Lift.toRows => //unlikely
//										Got((Lift.self, lift).asInstanceOf[(Lift[X, Any], Lift[Seq[Any], U])])
						case _ =>
							//The weird unapply use is to be able to instantiate type y, as otherwise Scala won't
							// track the compatibility of the first and second component in conversion
							type Composed[Y] = (SQLTransformation[Seq[X], Y], End[Y])
							SQLTransformation.Composition.unapply(conversion) match {
								case composed :Opt[Composed[y]] if composed.isDefined =>
									type Y = y
									val first  :SQLTransformation[Seq[X], Y] = composed.get._1
									val second :End[Y] = composed.get._2
									type SplitFirst[A, B, C] = SplitResult[A, B, C, first.SQLResult]
									type SplitSecond[A, B]   = SplitResult[A, B, Z, second.SQLResult]
									splitSeqConversion[X, Y](first) match {
										case firstSplit :Opt[SplitFirst[X, a, Y]] if firstSplit.isDefined =>
											type A = a
											val itemInFirst :SQLConversion[X, A] = firstSplit.get._1//.castParam2[Y]
											(firstSplit.get._2 :SQLTransformation[Seq[A], Y]) match {
												//we 'cast' to SQLConversion in order to statically compose it with second
												case identity :SQLConversion[Seq[A], Y] if identity.isIdentity =>
													//If the whole seq conversion in the composed._1 is identity, it is possible
													// that second still includes a ConvertSeq with an item conversion
													splitSeqConversion(identity andThen second) match {
														case secondSplit :Opt[SplitSecond[A, b]] if secondSplit.isDefined =>
															val itemPart :SQLConversion[A, b] = secondSplit.get._1
															val seqPart  :End[Seq[b]] = secondSplit.get._2
															Got((itemInFirst andThen itemPart, seqPart))
														case _ => //Ok, so first contained the whole items part
															result(itemInFirst, identity andThen second)
													}
												/* Theoretically, if firstSplit.get._2 is upcast, then second split
												 * can still succeed, although we would have to forgo the nice
												 * type safety of identity andThen second because that leads
												 * to an infinite recursion. We would also have to cast the input type
												 * of second to a Seq[_] in order to call ourselves again, and we don't
												 * know if it is true. It doesn't matter, because we don't rely
												 * on that fact in the actual implementation other than providing
												 * the correct output type, but in the end we compose everything again
												 * the same way anyway. Probably not worth it, it would be such
												 * a corner case when what we primarily are interested in are just
												 * cases of identity and SeqConversion.
												 */
												case seqInFirst =>
													//composition doesn't type check because seqInFirst isn't a SQLConversion
													val seqPart = (seqInFirst andThen second).asInstanceOf[End[Seq[A]]]
													result(itemInFirst, seqPart)
											}
										//if first doesn't have an items part, no need to try splitting second
										case _ => Lack
									}
								case _ => Lack
							}
					}
				}
				//Aaand we are back to actually structurally comparing this and e, losing all that precious type safety from earlier.
				type SecondLeft[X]  = SQLTransformation[Seq[X], U]#Into[leftResult.SQLResult]
				type SecondRight[X] = SQLTransformation[Seq[X], U]#Into[rightResult.SQLResult]
				(splitSeqConversion(leftResult), splitSeqConversion(rightSeqRes)) match {
					case (Got((leftItems :SQLConversion[T, Any @unchecked], leftRes :SecondLeft[Any] @unchecked)),
					      Got((rightItems :SQLConversion[I, Any @unchecked], rightRes :SecondRight[Any] @unchecked)))
						if leftRes == rightRes || leftRes.isUpcast && rightRes.isUpcast
					=>
						/* We can unify leftItems and rightItems only if they convert to the same type.
						 * Without using TypeTags in every SQLConversion, this is of course impossible to check.
						 * We require thus that leftRes equals rightRes, hoping that - because their result types
						 * are statically checked to be the same - their argument types also are.
						 * It is possible to create implementations which violate this postulate,
						 * but it holds for all actual implementations we use. In practice this means that
						 * both conversions are most likely identity, anyway.
						 */
						val these = parts; val those = e.parts
						if (these.length != those.length)
							throw new MismatchedExpressionsException(
								SeqSQL.this, e, "tuples have different lengths."
							)
						/* It's a pity we need to recreate the whole expressions
						 * even if they are compatible, but validating with reform.prohibitAll
						 * will still create intermediate result tuples, so this is not that much
						 * on top of that. We can't create a recursive validate(...) :Unit because
						 * we'd need to duplicate all reform methods in expressions.
						 */
						val reformed = (these zipMap those)(reform(_, _)(leftItems, rightItems, spelling))
						(leftRes(SeqSQL(reformed.map(_._1) :_*)), rightRes(SeqSQL(reformed.map(_._2) :_*)))
					case (Got((_, leftRes)), Got((_, rightRes))) =>
						fallbackGuard(
							s"SQLConversion instances for Seq[T] $leftResult, $rightResult" +
							s" end with incompatible conversions for whole sequences $leftRes and $rightRes."
						)
					case (Got((_, _)), _) =>
						fallbackGuard(
							s"unsupported type conversion for the right Seq expression: $rightResult."
						)
					case (_, Got((_, _))) =>
						fallbackGuard(
							s"unsupported type conversion for the left Seq expression: $leftResult."
						)
					case _ =>
						fallbackGuard(
							"unsupported type conversions for a pair of SeqSQL: (" +
								leftResult  + ", " + rightResult + ")."
						)
				}
			}
		}

//	protected override def reformer[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                               (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                               (implicit leftResult :Lift[Seq[T], U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:SpecificExpressionVisitor[E, C, X, (SQLExpression[F, S, U], SQLExpression[E, C, U])] =
//		new BaseReformer[E, C, X, U](other)(reform, passesAllowed) {
//
//			override def seq[V](e :SeqSQL[E, C, V])(implicit isSeq :X =:= Seq[V]) =
//				if (!leftResult.isDerived || !rightResult.isDerived)
//					this.reform(leftResult(SeqSQL.this), rightResult(other))
//				else {
//					implicit val rightSeqRes = isSeq.liftCo[({ type T[B] = Lift[B, U] })#T](rightResult)
//
//					/** Splits a lift for a SeqSQL into a lift for the individual elements and one applied
//					  * to the whole reformed expression. */
//					def splitSeqLift[A, B](lift :Lift[Seq[A], B]) :Opt[(Lift[A, Any], Lift[Seq[Any], B])] =
//						lift match {
//							case seq :SeqLift[_, _] =>
//								Got((seq.item, Lift.self).asInstanceOf[(Lift[A, Any], Lift[Seq[Any], B])])
//							case _ if lift.isIdentity => //possible really only at the start of recursion
//								Got((Lift.self, Lift.self).asInstanceOf[(Lift[A, Any], Lift[Seq[Any], B])])
////									case _ if lift == Lift.toRows => //unlikely
////										Got((Lift.self, lift).asInstanceOf[(Lift[X, Any], Lift[Seq[Any], U])])
//							case composed :ComposedLift[Seq[A], o, B] =>
//								splitSeqLift[A, o](composed.first) match {
//									case Got((item, seq)) if seq.isIdentity => //composed.second <:< Lift[Seq[A], U]
//										splitSeqLift(composed.second.asInstanceOf[Lift[Seq[Any], B]]) match {
//											case Got((secondItem, seq)) =>
//												Got((item andThen secondItem, seq))
//											case _ => Lack
//										}
//									case Got((item, seq)) =>
//										Got((item, seq andThen composed.second))
//									case _ => Lack
//								}
//							case _ => Lack //unknown Lift whose application would create a ConversionSQL wrapping this expression
//						}
//					(splitSeqLift(leftResult), splitSeqLift(rightSeqRes)) match {
//						case (Got((leftItems, leftRes)), Got((rightItems, rightRes))) if leftRes == rightRes =>
//							/* The unification below type checks only because we cast both lefTItems and rightItems
//							 * to Lift[V/X, Any]. This would be dangerous in general, because we might deeper down assign
//							 * an incompatible SQLForm to a term (among other problems). We require thus that the lifts
//							 * applied to the whole selects are equal (essentially implying their being identities)
//							 * as a sort of guarantee that the value types of the first pair are compatible
//							 * (because Lift is invariant). This isn't much of a restriction, as there is no reason
//							 * for anyone to create an expression with Seq value type which isn't a tuple.
//							 */
//							implicit val itemCompat = SQLTypeUnification(leftItems, rightItems)
//							val these = parts; val those = e.parts
//							if (these.length != those.length)
//								throw new MismatchedExpressionsException(
//									SeqSQL.this, e, "tuples have different lengths."
//								)
//							/*	It's a pity we need to recreate the whole expression
//							 *  even if they are compatible, but validating with reform.prohibitAll
//							 *  will still create intermediate result tuples, so this is not that much
//							 *  on top of that. We can't create a recursive validate(...) :Unit because
//							 *  we'd need to duplicate all reform methods in expressions, which use apply.
//							 */
//							val reformed = (these zipMap those)(reform(_, _))
//							(SeqSQL(reformed.map(_._1) :_*).to(leftRes),
//								SeqSQL(reformed.map(_._2) :_*).to(rightRes))
//						case (Got((_, leftRes)), Got((_, rightRes))) =>
//							fallbackGuard(
//								"incompatible Lift types " + leftResult + ", " + rightResult +
//								" ending with " + leftRes + " and " + rightRes + "."
//							)
//						case (Got((_, _)), _) =>
//							fallbackGuard(
//								s"unsupported type conversion for the right Seq expression: $rightResult."
//							)
//						case (_, Got((_, _))) =>
//							fallbackGuard(
//								s"unsupported type conversion for the left Seq expression: $leftResult."
//							)
//						case _ =>
//							fallbackGuard(
//								"unsupported type conversions for a SeqSQL pair: (" +
//									leftResult  + ", " + rightResult + ")."
//							)
//					}
//				}
//		}


	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
		parts.view.flatMap(spelling.split(_)).to(PassedArray)


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor: AnyExpressionVisitor[F, Y]): Y[S, Seq[T]] =
		visitor.seq(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, Seq[T], Y]) :Y = visitor.seq(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F, S, Seq[T]] <: SQLExpression[F_, S_, Seq[T]],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, Seq[T], E] =
//		visitor.seq(this)

	override def toString :String = inOrder.mkString("Seq(", ", ", ")")
}



object SeqSQL {
	def apply[F <: RowProduct, S >: Grouped <: Single, T](parts :SQLExpression[F, S, T]*) :SeqSQL[F, S, T] =
		new Impl(parts)

	def unapply[F <: RowProduct, S >: Grouped <: Single, T]
	           (e :SQLExpression[F, S, _]) :Opt[Seq[SQLExpression[F, S, _]]] =
		e match {
			case tuple :SeqSQL[F, S, _] => Got(tuple.inOrder)
			case _ => Lack
		}

	implicit def literalSeq[T :SQLForm](items :Seq[T]) :SeqSQL[RowProduct, Single, T] =
		new Impl(items.map(SQLTerm(_)))

	implicit def expressionSeq[F <: RowProduct, S >: Grouped <: Single, T]
	                          (items :Seq[SQLExpression[F, S, T]]) :SeqSQL[F, S, T] =
		new Impl(items)


	private case class Impl[F <: RowProduct, S >: Grouped <: Single, T]
	                       (override val parts :Seq[SQLExpression[F, S, T]])
		extends SeqSQL[F, S, T]
	{
		override type Item[-E <: RowProduct, -D >: Grouped <: Single, X] = InlineItem[E, D, X]
		override lazy val items = parts.zipWithIndex map { case (expr, i) => InlineItem(i, expr) }
	}


	trait SpecificSeqVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def seq[V](e :SeqSQL[F, S, V])(implicit isSeq :X =:= Seq[V]) :Y
	}
	type MatchSpecificSeq[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificSeqVisitor[F, S, X, Y]
	type CaseSpecificSeq[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificSeqVisitor[F, S, X, Y]
//
//	trait SeqVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def seq[S >: Grouped <: Single, V](e :SeqSQL[F, S, V]) :R[S, Seq[V], SeqSQL[F, S, V]]
//	}
//	type MatchSeq[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		SeqVisitor[F, R]
//	type CaseSeq[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		SeqVisitor[F, R]

	trait AnySeqVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def seq[S >: Grouped <: Single, X](e :SeqSQL[F, S, X]) :Y[S, Seq[X]]
	}
	type MatchAnySeq[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnySeqVisitor[F, Y]
	type CaseAnySeq[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnySeqVisitor[F, Y]
}



package net.noresttherein.oldsql.sql.ast

import java.sql.{PreparedStatement, ResultSet}
import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{Chain, Listing, NaturalMap, Opt, PassedArray}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{InseparableExpressionException, MismatchedExpressionsException, UndefinedShapeException}
import net.noresttherein.oldsql.morsels.{Extractor, generic}
import net.noresttherein.oldsql.morsels.generic.{=>:, Self}
import net.noresttherein.oldsql.morsels.Extractor.from
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.{AbstractSQLReadForm, CompositeReadForm, ReadFormNullValue}
import net.noresttherein.oldsql.schema.SQLWriteForm.CompositeWriteForm
import net.noresttherein.oldsql.slang.{cast2TypeParams, castTypeParam, classNameMethods, mappingMethods, saferCasting}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ChainTuple.{AnyChainTupleVisitor, CaseAnyChainTuple, CaseSpecificChainTuple, SpecificChainTupleVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.BinaryCompositeSQL
import net.noresttherein.oldsql.sql.ast.EmptySQL.SpecificEmptyVisitor
import net.noresttherein.oldsql.sql.ast.InlineSQL.InlineItem
import net.noresttherein.oldsql.sql.ast.IndexedSQL.{AnyIndexedVisitor, CaseAnyIndexed, CaseSpecificIndexed, SpecificIndexedVisitor}
import net.noresttherein.oldsql.sql.ast.RecordSQL.{AnyRecordVisitor, CaseAnyRecord, CaseSpecificRecord, SpecificRecordVisitor}
import net.noresttherein.oldsql.sql.ast.SeqSQL.{AnySeqVisitor, CaseAnySeq, CaseSpecificSeq, SpecificSeqVisitor}
import net.noresttherein.oldsql.sql.mechanics.{AlignableColumns, Reform, ReformPermissions, SQLConversion, SQLScribe, SQLTransformation, SpelledSQL, sql_=>}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.{MayReform, MayReorder}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.{SeqConversion, Upcast}






private[sql] trait NonColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, V] extends SQLExpression[F, S, V] {
	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if (spelling.isInline)
			spelling.inlineSpelling(this)(from, context, params)
		else
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
  * It allows for implementations with various sequence-like or tuple-like value types, at the same time providing
  * a shared interface allowing to modify those expressions in a polymorphic manner. Each implementation defines its own
  * [[net.noresttherein.oldsql.sql.ast.InlineSQL.Item Item]]` <: `[[net.noresttherein.oldsql.sql.ast.InlineSQL.InlineItem InlineItem]]
  * type, which carry top-level elements of this expression and identify their position by an index.
  * Method [[net.noresttherein.oldsql.sql.ast.InlineSQL.items items]] allows to access
  */ //todo: MappingRecord parameterized by a Chain/Listing of Mapping types
trait InlineSQL[-F <: RowProduct, -S >: Grouped <: Single, T]
	extends CompositeSQL[F, S, T] with SelectableSQL[F, S, T] //could extend NonColumnSQL
{
	private[this] type OwnItem[V] = Item[F, S, V]
	type Item[-E <: RowProduct, -D >: Grouped <: Single, X] <: InlineItem[E, D, X]

	def size  :Int
	def toSeq :Seq[SQLExpression[F, S, _]] = inOrder
	def items :IndexedSeq[Item[F, S, _]]
	def extracts[E <: F, A >: Grouped <: S] :Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[T]#to, _]]

	/** Creates a value of this expression from values
	  * for its individual [[net.noresttherein.oldsql.sql.ast.InlineSQL.items items]].
	  */
	def construct(items :({ type T[X] = Item[F, S, X] })#T =>: generic.Self) :T

	/** Recreates this expression by mapping each individual element into an expression of the same value type.
	  * Note that the exact type of the returned expression may be different from this one if any of the individual
	  * expressions returned by the argument function are incompatible with this type,
	  * as in [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]].
	  */
	def rebuild[E <: RowProduct, A >: Grouped <: Single]
	           (items :({ type T[X] = Item[F, S, X] })#T =>: SQLExpression.from[E]#rows[A]#E) :InlineSQL[E, A, T]

	//consider: inversing the encoding of permutation, so that reordering based on it is done permutation.map(input)
	@throws[IllegalArgumentException]("if keys is not a permutation of numbers [0, items.length).")
	def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, S, T]

	override def isShapeComposite :Boolean = true
	override def isInline         :Boolean = true


	protected override def realign(reordering :Rearrangement)
	                              (implicit spelling :SQLSpelling) :SQLExpression[F, S, T] =
		if (reordering.isIdentity)
			this
		else {
			//fixme: padding with null columns
			val items = this.items
			try {
				//attempt to split reordering into consecutive instances for each item in this expression
				val (reorderings, empty) = ((PassedArray.empty[Rearrangement], reordering) /: items) {
					case ((reorderings, order), item) =>
						val columnCount = spelling.potentialColumnsCount(item.value)
						val (h, t) = order.splitAt(columnCount + 1)
						(reorderings :+ h, t)
				}
				assert(empty.columnCount != 0 || empty.underlyingColumnCount != 0,
					"Splitting " + reordering + " for item expressions of `" + this + "` into " + reorderings +
					" left a non-empty remainder from " + reorderings + ": " + empty + "."
				)
				//todo: one way in which it may fail is if three consecutive items are columns and reordering
				// inserts null columns in between them, as ColumnSQL.realign must return a ColumnSQL.
				type OwnItem[X] = Item[F, S, X]
				rebuild(new (OwnItem =>: SQLExpression.from[F]#rows[S]#E) {
					override def apply[X](x :OwnItem[X]) :SQLExpression[F, S, X] =
						spelling.realign(x.value, reorderings(x.index))
				})
			} catch {
				case e1 :Exception => //a second attempt: see if reordering shuffles complete expressions.
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
					//check if there are no overlaps in images of mapped ranges
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
						//Todo: try first to widen the spans so they cover the whole underlying range,
						// and make the null columns (gaps) a responsibility of item expressions.
						// This must attempt to avoid adding gaps to single-column spans if the item is a ColumnSQL,
						// as it will not succeed.
						try {
							type OwnItem[X] = Item[F, S, X]
							val reordered = rebuild(
								new (OwnItem =>: SQLExpression.from[F]#rows[S]#E) {
									override def apply[X](x :Item[F, S, X]) :SQLExpression[F, S, X] =
										spelling.realign(x.value, partReorderings(x.index))
								}
							)
							val res = reordered.reorder(newOrder)
							//If there are gaps between spans in newSpans, then res needs to be spliced with null columns
							// An alternative would be to force element expressions to include them, but this is impossible
							// for columns.
							if (!hasGaps)
								res
							else {
								//todo: NullPaddedRecordSQL
								//list every column index which is mapped
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


	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		AlignableColumns(this, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
		(0 /: toSeq)(_ + spelling.potentialColumnsCount(_))

	protected override def shape(implicit spelling :SQLSpelling) :RowShape =
		(RowShape() /: parts)(_ + spelling.shape(_))

	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[T] = {
		def writeForm[X](extract :Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[T]#to, X]) :SQLWriteForm[T] =
			spelling.effectiveForm(extract._1).writer compose extract._2
		val forms = extracts[F, S].view.map(writeForm(_)) to PassedArray
		val writer = forms.reduce(_ + _)

		val reader = { //todo: extract a proper SQLReadForm class, with equality, and use it also for selectForm
			def itemReader[X](item :OwnItem[X], offset :Int) = {
				val form = spelling.effectiveForm(item.value)
				def read(rs :ResultSet, position :Int) = form(rs, position + offset)
				Assoc[OwnItem, ({type F[A] = (ResultSet, Int) => A})#F, X](item, read)
			}
			val offsets = forms.view.scanLeft(0)(_ + _.columnCount).toArray
			val readers = items.zipMap(offsets)(itemReader(_, _)) to NaturalMap

			new ReadFormNullValue[T] {
				override val nulls = NullValue.NotNull
				override val columnCount = offsets(offsets.length - 1)
				override def columnTypes = writer.columnTypes

				override def opt(res :ResultSet, position :Int) =
					try Got(
						construct(new (({ type T[V] = Item[F, S, V] })#T =>: generic.Self) {
							override def apply[X](x :Item[F, S, X]) :X = readers(x)(res, position)
						})
					) catch {
						case _ :NullPointerException | _ :NoSuchElementException => Lack
					}

				override lazy val text = Got {
					forms.view.map(f => textOf(f) getOrElse f.toString).mkString(
						InlineSQL.this.localClassName + "(", ", ", ")"
					)
				}
				override lazy val toString = text.get + '>'
			}
		}
		reader <> writer
	}


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
//	trait InlineItemTemplate[-F <: RowProduct, -S >: Grouped <: Single, T,
//	                         +Val[-f <: RowProduct, -s >: Grouped <: Single, X] <: SQLExpression[f, s, X],
//	                         +Item[-f <: RowProduct, -s >: Grouped <: Single, t] <: InlineItemTemplate[f, s, t, Val, Item]]
//	{
//		/** A zero-based index of this item in the owning expression's
//		  * [[net.noresttherein.oldsql.sql.ast.InlineSQL.items items]] sequence.
//		  */
//		def index :Int
//
//		/** The expression at [[net.noresttherein.oldsql.sql.ast.InlineSQL.InlineItem.index index]]
//		  * in the owning `InlineSQL`.
//		  */
//		def value :Val[F, S, T]
//	}


	/** A wrapper holding a top-level tuple element of an [[net.noresttherein.oldsql.sql.ast.InlineSQL]]
	  * together with its index (zero-based, counting left to right). Various `InlineSQL` implementations
	  * use specific subtypes of this trait, which can be used to assemble another instance of the same kind
	  * in a polymorphic manner.
	  * @see [[net.noresttherein.oldsql.sql.ast.InlineSQL.construct]]
	  */
	trait InlineItem[-F <: RowProduct, -S >: Grouped <: Single, T] extends Serializable {
		/** A zero-based index of this item in the owning expression's
		  * [[net.noresttherein.oldsql.sql.ast.InlineSQL.items items]] sequence.
		  */
		def index :Int

		/** The expression at [[net.noresttherein.oldsql.sql.ast.InlineSQL.InlineItem.index index]]
		  * in the owning `InlineSQL`.
		  */
		def value :SQLExpression[F, S, T]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case item :InlineItem[_, _, _] if item canEqual this => //comparing whole expressions is slow, but otherwise we have no type safety
				index == item.index && hashCode == item.hashCode && value == item.value
			case _ => false
		}
		def canEqual(that :Any) :Boolean = that.getClass == getClass //that.isInstanceOf[InlineItem[_, _, _]]
		override lazy val hashCode :Int = value.hashCode * 31 + index.hashCode

		override def toString :String = "#" + index + ": " + value
	}

	object InlineItem {
		def apply[F <: RowProduct, S >: Grouped <: Single, T]
		         (index :Int, value :SQLExpression[F, S, T]) :InlineItem[F, S, T] =
			new Impl(index, value)

		private class Impl[-F <: RowProduct, -S >: Grouped <: Single, T]
		                  (override val index :Int, override val value :SQLExpression[F, S, T])
			extends InlineItem[F, S, T]
	}


	//todo: true scala tuple expressions
	def unapply[F <: RowProduct, S >: Grouped <: Single](expr: SQLExpression[F, S, _])
			:Opt[Seq[SQLExpression[F, S, _]]] =
		expr match {
			case t: InlineSQL[F, S, _] => Got(t.inOrder)
			case _ => Lack
		}


	/** A base trait for SQL expressions constituting of two expressions, whose values is rendered one after another,
	  * i.e., the columns of the former are a concatenation of columns of `left` and `right`.
	  * It is an inlining of the two expression in the sense of rendered SQL, but it doesn't implement
	  * [[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]] itself. This is so subclasses may either define
	  * [[net.noresttherein.oldsql.sql.ast.InlineSQL.Item Item]] type as they see fit,
	  * extend [[net.noresttherein.oldsql.sql.ast.InlineSQL.InlineBinaryCompositeSQL]] for a default, two element
	  * [[net.noresttherein.oldsql.sql.ast.InlineSQL.items items]] list, or do not extend `InlineSQL` at all.
	  */
	//We need it to extract common implementations from ChainSQL and LabeledSQL, but it can't implement InlineSQL
	// because subclasses of those go from two parts to a fully inlined part list, which could lead to bugs.
	// This decision can be revised if we implement more abstract code dealing with InlineSQL
	trait AbstractInlineBinaryCompositeSQL[-F <: RowProduct, -S >: Grouped <: Single, L, R, V]
		extends BinaryCompositeSQL[F, S, L, R, V] with NonColumnSQL[F, S, V]
	{ expr =>
		override def groundValue :Opt[V] = for { l <- left.groundValue; r <- right.groundValue } yield value(l, r)

		override def selectForm    :SQLReadForm[V] = new ReadForm(left.selectForm, right.selectForm)
		override def universalForm :Opt[SQLForm[V]] =
			for (lf <- left.universalForm; rf <- right.universalForm) yield compositeForm(lf, rf)

		protected def value(left :L, right :R) :V
		protected def leftValue(value :V) :L
		protected def rightValue(value :V) :R

		protected override def realign(reordering :Rearrangement)
		                              (implicit spelling :SQLSpelling) :SQLExpression[F, S, V] =
		if (reordering.isIdentity)
			this
		else
			try {
				val lastColumns = spelling.potentialColumns(right, MayReform).columns.length
				val initColumns = reordering.columnCount - lastColumns
				//Hopes to reform left and right independently, without changing label order on this level
				val (initOrder, lastOrder) = reordering.splitAt(initColumns + 1)
				if (lastOrder.columnCount == 1 && lastOrder.underlyingColumnCount != 1) {
					//todo: pad with null columns on this level to prevent mapping a column to multiple columns
				}
				if (initOrder.columnCount == 1 && initOrder.underlyingColumnCount != 1) {
					//todo: pad with null columns on this level to prevent mapping a column to multiple columns
				}
				reapply(spelling.realign(left, initOrder), spelling.realign(right, lastOrder))
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
						RearrangedSQL(this, reordering)
					} catch {
						case e2 :InseparableExpressionException => throw error(e2)
						case e2 :UndefinedShapeException => throw error(e2)
					}
			}

		protected override def potentialColumns(permissions :Permissions)
		                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		{
			val noReorder = permissions - MayReorder
			val alignableColumns =
				spelling.potentialColumns(left, noReorder).columns ++ spelling.potentialColumns(right, noReorder).columns
			new AlignableColumns(this, noReorder, alignableColumns)
		}

		protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
			spelling.potentialColumnsCount(left) + spelling.potentialColumnsCount(right)

		protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[V] =
			compositeForm(spelling.effectiveForm(left), spelling.effectiveForm(right))

		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
			spelling.split(left) :++ spelling.split(right)

		protected override def shape(implicit spelling :SQLSpelling) :RowShape =
			spelling.shape(left) + spelling.shape(right)

		protected override def columnCount(implicit spelling :SQLSpelling) :Int =
			spelling.columnCount(left) + spelling.columnCount(right)


		protected override def explodedSpelling[P](independent :Boolean)
		                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		{
			val first = spelling.explode(left, independent)(from, context, params)
			val second = first match {
				case Seq() => spelling.explode(right, independent)(from, context, params)
				case columns =>
					val prev = columns.last
					spelling.explode(left, independent)(from, prev.context, params)
			}
			first :++ second
		}

		private def compositeForm(left :SQLForm[L], right :SQLForm[R]) :SQLForm[V] = {
			val reader = new ReadForm(left, right)
			val writer = (left unmap leftValue) + (right unmap rightValue)
			val text = for (l <- left.`->text`; r <- right.`->text`) yield l + "," + r
			SQLForm(text getOrElse "(" + left + "," + right + ")", reader, writer)
			reader <> writer
		}

		private class ReadForm(left :SQLReadForm[L], right :SQLReadForm[R])
			extends ReadFormNullValue[V]
		{
			override val columnCount = left.columnCount + right.columnCount
			override val columnTypes = left.columnTypes ++ right.columnTypes
			override def nulls = NullValue.NotNull

			override def opt(res :ResultSet, position :Int) :Opt[V] =
				for (l <- left.opt(res, position); r <- right.opt(res, position + left.columnCount))
					yield expr.value(l, r)

			override def apply(res :ResultSet, position :Int) :V =
				expr.value(left(res, position), right(res, position + left.columnCount))

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if this eq self => true
				case other :AbstractInlineBinaryCompositeSQL[_, _, _, _, _]#ReadForm if other canEqual this =>
					expression == other.expression
				case _ => false
			}
			override def hashCode :Int = expr.hashCode
			private def expression = expr
			override lazy val toString = "(" + expr + ").selectForm"
		}
	}


	trait InlineBinaryCompositeSQL[-F <: RowProduct, -S >: Grouped <: Single, L, R, V]
		extends AbstractInlineBinaryCompositeSQL[F, S, L, R, V] with InlineSQL[F, S, V]
	{
		override type Item[-E <: RowProduct, -A >: Grouped <: Single,  X] = InlineItem[E, A, X]

		override def size :Int = 2

		private[this] val item1 = InlineItem(0, left)
		private[this] val item2 = InlineItem(1, right)
		override val items :IndexedSeq[InlineItem[F, S, _]] = PassedArray.two(item1, item2)

		override def extracts[E <: F, A >: Grouped <: S]
				:Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[V]#to, _]] =
			PassedArray.two(
				Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[V]#to, L](left, Extractor.req(leftValue)),
				Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[V]#to, R](right, Extractor.req(rightValue))
			)

		override def construct(items :({ type T[X] = Item[F, S, X] })#T =>: generic.Self) :V =
			value(items(item1), items(item2))

		override def rebuild[E <: RowProduct, A >: Grouped <: Single]
		                    (items :({type T[X] = Item[F, S, X]})#T =>: SQLExpression.from[E]#rows[A]#E) :InlineSQL[E, A, V] =
			reapply(items(item1), items(item2))

		override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                    (left :SQLExpression[E, C, L], right :SQLExpression[E, C, R]) :InlineSQL[E, C, V]
	}



	trait SpecificInlineVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificChainTupleVisitor[F, S, X, Y] with SpecificRecordVisitor[F, S, X, Y]
		   with SpecificSeqVisitor[F, S, X, Y] //with SpecificEmptyVisitor[X, Y]
	{
		def inline(e :InlineSQL[F, S, X]) :Y
	}

	trait MatchSpecificInline[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends SpecificInlineVisitor[F, S, X, Y]
		with CaseSpecificChainTuple[F, S, X, Y] with CaseSpecificRecord[F, S, X, Y] with CaseSpecificSeq[F, S, X, Y]

	trait CaseSpecificInline[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] extends MatchSpecificInline[F, S, X, Y] {
		override def chainTuple[C <: Chain](e :ChainTuple[F, S, C])(implicit isChain :X =:= C) :Y =
			inline(isChain.substituteContra(e :InlineSQL[F, S, C]))

		override def record[V <: Listing](e :RecordSQL[F, S, V])(implicit isListing :X =:= V) :Y =
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
//		                    (e :IndexedSQL[F, S, T]) :R[S, T, IndexedSQL[F, S, T]] = inline(e)
//
//		override def seq[S >: Grouped <: Single, V]
//		                (e :SeqSQL[F, S, V]) :R[S, Seq[V], SeqSQL[F, S, V]] = inline(e)
//	}


	trait AnyInlineVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyChainTupleVisitor[F, Y] with AnyRecordVisitor[F, Y] with AnySeqVisitor[F, Y]
	{
		def inline[S >: Grouped <: Single, X](e: InlineSQL[F, S, X]): Y[S, X]
	}

	trait MatchAnyInline[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyInlineVisitor[F, Y] with CaseAnyChainTuple[F, Y] with CaseAnyRecord[F, Y] with CaseAnySeq[F, Y]

	trait CaseAnyInline[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyInlineVisitor[F, Y] with MatchAnyInline[F, Y]
	{
		override def chainTuple[S >: Grouped <: Single, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] = inline(e)
		override def record[S >: Grouped <: Single, X <: Listing](e :RecordSQL[F, S, X]) :Y[S, X] = inline(e)
		override def seq[S >: Grouped <: Single, X](e: SeqSQL[F, S, X]): Y[S, Seq[X]] = inline(e)
	}
}






trait SeqSQL[-F <: RowProduct, -S >: Grouped <: Single, T] extends InlineSQL[F, S, Seq[T]] {
	protected override def parts :Seq[SQLExpression[F, S, T]]
	override def toSeq   :Seq[SQLExpression[F, S, T]] = parts
	override def inOrder :Seq[SQLExpression[F, S, T]] = parts
	override def items   :IndexedSeq[Item[F, S, T]]
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

	override def rebuild[E <: RowProduct, A >: Grouped <: Single]
                        (items :({ type T[X] = Item[F, S, X] })#T =>: SQLExpression.from[E]#rows[A]#E) :SeqSQL[E, A, T] =
		SeqSQL(this.items.map(items(_)) :_*)


	override def groundValue :Opt[Seq[T]] = inOrder.flatMap(_.groundValue.toOption) match {
		case res if res.length == inOrder.length => Got(res)
		case _ => Lack
	}

	override def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, S, Seq[T]] =
		if (permutation.length != parts.length || permutation.sorted != permutation.indices)
			throw new IllegalArgumentException(
				"Cannot reorder `" + this + "`: " + permutation + " is not a valid permutation of size " + size + "."
			)
		else {
			val items = this.items
			val reordered = new Array[SQLExpression[F, S, T]](permutation.length)
			var i = permutation.length
			while (i > 0) {
				i -= 1
				reordered(permutation(i)) = items(i).value
			}
			SeqSQL(ArraySeq.unsafeWrapArray(reordered) :_*)
		}

	override def anchor(from :F) :SeqSQL[F, S, T] = SeqSQL(parts.map(_.anchor(from)) :_*)

	override def rephrase[E <: RowProduct](mapper: SQLScribe[F, E]) :SeqSQL[E, S, T] =
		SeqSQL(inOrder.map(mapper(_)) :_*)

/*
	protected override def realign(reordering :Rearrangement)
	                              (implicit spelling :SQLSpelling) :SQLExpression[F, S, Seq[T]] =
		if (reordering.isIdentity)
			this
		else {
			val reordered = ((PassedArray.empty[SQLExpression[F, S, T]], reordering, 0) /: toSeq) {
				case ((expressions, order, i), e) =>
					val columnCount = spelling.potentialColumnsCount(e)
					val (h, t) =
						try order.splitAt(columnCount + 1) catch {
							case ex :Exception =>
								throw new IllegalArgumentException(
									"Cannot reorder `" + this + "` according to " + reordering +
										" because  cannot separate from " + order + " reordering of " + columnCount +
										" columns for the " + i + "-th expression `" + e + "`: " + ex.getMessage, ex
								)
						}
					(expressions :+ e.`->realign`(h), t, i + 1)
			}
			SeqSQL(reordered._1 :_*)
		}
*/

	protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                               (implicit leftResult :SQLTransformation[Seq[T], U], rightResult :SQLTransformation[V2, U],
	                                         spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])] =
		new BaseReformer[F2, S2, V2, EC2, U, leftResult.Expression, rightResult.Expression](
			other)(reform, passCount)(leftResult, rightResult, spelling
		) {
			override def seq[I](e :SeqSQL[F2, S2, I])(implicit isSeq :V2 =:= Seq[I]) = {
				implicit val rightSeqRes =
					isSeq.substituteCo[({ type T[B] = SQLTransformation[B, U]#Into[rightResult.Expression] })#T](rightResult)

				type SplitResult[X, Y, Z, Res[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, Z]] <: SQLExpression[f, s, Z]] =
					(SQLConversion[X, Y], SQLTransformation.Returning[Seq[Y], Z, Res])

				/** Splits a conversion for a `SeqSQL` into a conversion for the individual elements and one applied
				  * to the whole reformed expression. */
				def splitSeqConversion[X, Z](conversion :SQLTransformation[Seq[X], Z])
						:Opt[SplitResult[X, _, Z, conversion.Expression]] =
//						:Opt[(SQLConversion[X, Y], SQLTransformation[Seq[Y], Z]#Into[conversion.SQLResult]) forSome { type Y }] =
				{
					type End[A] = SQLTransformation.Returning[A, Z, conversion.Expression]
					//this function exists because type checker can't handle existentials involved in the return type.
					def result[Y](item :SQLConversion[X, Y], seq :End[Seq[Y]]) :Opt[(SQLConversion[X, Y], End[Seq[Y]])] =
						Got((item, seq))
					conversion match {
						case seq :SeqConversion[X, y] =>
//						case SeqConversion(item, _, )
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
							type Composed[Y] = (Seq[X] sql_=> Y, SQLTransformation.Into[Y, Z, conversion.Expression])
							SQLTransformation.Composition.WithConversion.unapply(conversion) match {
								case composed :Opt[Composed[y]] if composed.isDefined =>
									type Y = y
									val first  :SQLTransformation[Seq[X], Y] = composed.get._1
									val second :End[Y] = composed.get._2
//									type SplitFirst[A, B, C] = (SQLConversion[A, B], SQLTransformation[Seq[B], C])
									type SplitFirst[A, B] = (SQLConversion[A, B], SQLTransformation.Returning[Seq[B], Y, first.Expression])
									type SplitSecond[A, B]   = (SQLConversion[A, B], SQLTransformation.Returning[Seq[B], Z, second.Expression])
//									type SplitFirst[A, B, C] = SplitResult[A, B, C, first.Expression]
//									type SplitSecond[A, B]   = SplitResult[A, B, Z, second.Expression]
									splitSeqConversion[X, Y](first) match {
										case firstSplit :Opt[SplitFirst[X, a]] if firstSplit.isDefined =>
											type A = a
											val itemInFirst :SQLConversion[X, A] = firstSplit.get._1//.castParam2[Y]
											(firstSplit.get._2 :SQLTransformation[Seq[A], Y]) match {
												//we 'cast' to SQLConversion in order to statically compose it with second
												case identity :SQLConversion[Seq[A], Y] if identity.isIdentity =>
													//If the whole seq conversion in the composed._1 is identity, it is possible
													// that second still includes a SeqConversion with an item conversion
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
				type SecondLeft[X]  = SQLTransformation.Into[Seq[X], U, leftResult.Expression]
				type SecondRight[X] = SQLTransformation.Into[Seq[X], U, rightResult.Expression]
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
						val these = parts.map(NonSpecificSQL(_))
						val those = e.parts.map(NonSpecificSQL(_))
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
						val left = SeqSQL(reformed.map(_._1) :_*)
						val right = SeqSQL(reformed.map(_._2) :_*).castFrom[
							SeqSQL[F2, S2, Any], ConvertibleSQL[F2, S2, Seq[Any], EC2]
						]
						(leftRes(left), rightRes(right))
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

	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[Seq[T]] =
		SQLForm.seq(parts.map(spelling.effectiveForm(_)))

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
		new Impl(parts.toIndexedSeq)

	def unapply[F <: RowProduct, S >: Grouped <: Single, T]
	           (e :SQLExpression[F, S, _]) :Opt[Seq[SQLExpression[F, S, _]]] =
		e match {
			case tuple :SeqSQL[F, S, _] => Got(tuple.inOrder)
			case _ => Lack
		}

	implicit def literalSeq[T :SQLForm](items :Seq[T]) :SeqSQL[RowProduct, Single, T] =
		new Impl(items.view.map(SQLTerm(_)).toIndexedSeq)

	implicit def expressionSeq[F <: RowProduct, S >: Grouped <: Single, T]
	                          (items :Seq[SQLExpression[F, S, T]]) :SeqSQL[F, S, T] =
		new Impl(items.toIndexedSeq)


	private case class Impl[F <: RowProduct, S >: Grouped <: Single, T]
	                       (override val parts :IndexedSeq[SQLExpression[F, S, T]])
		extends SeqSQL[F, S, T]
	{
		override type Item[-E <: RowProduct, -D >: Grouped <: Single, X] = InlineItem[E, D, X]
		override lazy val items = parts.mapWithIndex { (expr, i) => InlineItem(i, expr) }
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


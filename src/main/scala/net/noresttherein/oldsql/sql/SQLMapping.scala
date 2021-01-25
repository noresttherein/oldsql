package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.collection.{Chain, Listing, NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.morsels.generic.{=#>, Self}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema.{composeExtracts, filterColumnExtracts, Buff, Buffs, ColumnForm, ColumnMapping, ColumnReadForm, ColumnWriteForm, GenericExtract, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, ReadOnly}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseMapping, ExportMapping, LazyMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.bits.LabelPath
import net.noresttherein.oldsql.schema.bits.LabelPath./
import net.noresttherein.oldsql.sql.ColumnSQL.{AliasedColumn, CaseColumn}
import net.noresttherein.oldsql.sql.ListingSQLMapping.GetListingComponent
import net.noresttherein.oldsql.sql.SQLExpression.{BaseExpressionMatcher, CaseExpression, ExpressionMatcher, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.UnboundParam.UnboundParamSQL
import net.noresttherein.oldsql.sql.ast.ConversionSQL
import net.noresttherein.oldsql.sql.ast.ConversionSQL.PromotionConversion
import net.noresttherein.oldsql.sql.ast.MappingSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter
import net.noresttherein.oldsql.sql.ast.TupleSQL.{ChainTuple, ListingSQL, SeqTuple}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.MatchChain
import net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL.{ListingColumn, ListingValueSQL, MatchListing}






/** A synthetic adapter of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, O]` to a
  * [[net.noresttherein.oldsql.schema.Mapping Mapping]]. Used in ''select'' and ''group by'' clauses,
  * so that [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]
  * can be homomorphic with [[net.noresttherein.oldsql.sql.Join Join]]s and to allow
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expressions to be used as
  * [[net.noresttherein.oldsql.schema.Relation Relation]]s in the ''from'' clauses of other SQL selects.
  * This class is dedicated to non-component expressions; subclasses of
  * [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]] should be used directly for component expressions.
  * Not all possible expressions are supported; the expression may consist of
  *   - any single [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] (atomic SQL values),
  *     in particular [[net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm terms]],
  *   - [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL components]] (ranging from whole entities
  *     to single columns),
  *   - [[net.noresttherein.oldsql.sql.ast.ConversionSQL conversion]] nodes,
  *   - any [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL composites]] combining the above, in particular:
  *   - [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuples]] and
  *     [[ast.TupleSQL.ListingSQL indexed tuples]].
  *
  * The expression is traversed recursively descending to leaf expressions:
  *   - all column expressions are used directly;
  *   - component expressions are treated as tuples of their columns and their columns are adapted to columns
  *     of this expression,
  *
  * This mapping doesn't contain any non-column components.
  *
  * @tparam F the ''from'' clause serving as the basis of the adapted expression;
  * @tparam S the scope of the expression: [[net.noresttherein.oldsql.sql.SQLExpression.LocalScope local]] for
  *           expressions which can occur only as part of the most nested SQL select based on `F` in its
  *           ''group by'' or ''select'' clause, and [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope global]]
  *           for expressions which can occur anywhere in a SQL select from `F` or its dependent selects.
  * @tparam X the value type of this expression
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this mapping.
  *
  * @see [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]
  * @author Marcin Mościcki
  */
trait SQLMapping[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, O] extends BaseMapping[X, O] {

	val expr :SQLExpression[F, S, X]
	def format :String = ??? //todo:

	override val buffs :Buffs[X] = Buffs(this, SQLMapping.buffList.asInstanceOf[Seq[Buff[X]]]:_*)

	override def writtenValues[T](op :WriteOperationType, subject :X) :ComponentValues[X, O] = ColumnValues.empty
	override def writtenValues[T](op :WriteOperationType, subject :X, collector :ComponentValuesBuilder[T, O]) :Unit =
		()


	override def components :Unique[SQLMapping[F, S, _, O]]
	override def subcomponents :Unique[SQLMapping[F, S, _, O]]

	override def columns(op :OperationType) :Unique[ColumnSQLMapping[F, S, _, O]] = op match {
		case SELECT => selectable
		case FILTER => filterable
		case INSERT => insertable
		case UPDATE => updatable
	}

	override def columns :Unique[ColumnSQLMapping[F, S, _, O]]
	override def selectable :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoSelect)
	override def filterable :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoFilter)
	override def insertable :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoInsert)
	override def updatable :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoUpdate)
	override def autoInserted :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWith(AutoInsert)
	override def autoUpdated :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWith(AutoUpdate)
	override def selectedByDefault :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoSelectByDefault)
	override def filteredByDefault :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoFilterByDefault)
	override def insertedByDefault :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoInsertByDefault)
	override def updatedByDefault :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoUpdateByDefault)

	override def columnsWith(buff :BuffType) :Unique[ColumnSQLMapping[F, S, _, O]] =
		columns.filter(buff.active)

	override def columnsWithout(buff :BuffType) :Unique[ColumnSQLMapping[F, S, _, O]] =
		columns.filter(buff.inactive)

	override def selectForm :SQLReadForm[X] = expr.readForm

	override def writeForm(op :WriteOperationType) :SQLWriteForm[X] = SQLWriteForm.empty

	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[X] =
		if (components.isEmpty)
			SQLWriteForm.empty
		else
			throw new IllegalArgumentException(s"Can't $op $components: expression column $expr does not support write.")

}






object SQLMapping {
	private val buffList = ReadOnly::NoFilter::Nil

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X, O]
	         (expression :SQLExpression[F, S, X]) :SQLMapping[F, S, X, O] =
		expression match {
			case column :ColumnSQL[F @unchecked, S @unchecked, X @unchecked] =>
				ColumnSQLMapping(column)
			case _ =>
				new NonColumnSQLMapping(expression)
		}

	def unapply[X](mapping :RefinedMapping[X, _]) :Opt[SQLExpression[_, LocalScope, X]] =
		mapping match {
			case expr :SQLMapping[_, LocalScope @unchecked, X @unchecked, _] => Got(expr.expr)
			case _ => Lack
		}





	type Project[F <: RowProduct, S >: LocalScope <: GlobalScope, X] = {
		type Expression[O] = SQLMapping[F, S, X, O]
		type Column[O] = ColumnSQLMapping[F, S, X, O]
		type IndexedExpression[O] = ListingSQLMapping[F, S, X, O]
	}



	class NonColumnSQLMapping[F <: RowProduct, S >: LocalScope <: GlobalScope, X, O]
	                         (override val expr :SQLExpression[F, S, X])
		extends SQLMapping[F, S, X, O] with LazyMapping[X, O]
	{ outer =>
		protected type ExpressionColumn[V] = ColumnSQLMapping[F, S, V, O]


		private type ExpressionExtract[V] = {
			type E[T] = GenericExtract[ColumnSQLMapping[F, S, T, O], V, T, O]
		}
		private type Extractors[-_ >: LocalScope <: GlobalScope, V] =
			Seq[Assoc[ExpressionColumn, ExpressionExtract[V]#E, _]]


		/** Traverses the `expr` AST in the steps, stopping recursion when a `ColumnSQL` is encountered.
		  * Returns a flat list of all found column expressions with their `Extract`s.
		  */
		private class ExtractsCollector extends ExpressionMatcher[F, Extractors]
			with CaseExpression[F, Extractors] with CaseColumn[F, Extractors] with MatchChain[F, Extractors]
			with MatchListing[F, Extractors]
		{
			private[this] var names :Set[String] = Set("") //used column names, disallowing ""

			@inline private def composeColumnExtractAssoc[W, P, T]
			                    (extractor : W =?> P)(entry :Assoc[ExpressionColumn, ExpressionExtract[P]#E, T])
					:Assoc[ExpressionColumn, ExpressionExtract[W]#E, T] =
				Assoc[ExpressionColumn, ExpressionExtract[W]#E, T](entry._1, entry._2 compose extractor)


			override def column[C >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, C, V]) :Extractors[C, V] = {
				val column = ColumnSQLMapping[F, S, V, O](e.asInstanceOf[ColumnSQL[F, S, V]], nameFor(e))
				Assoc[ExpressionColumn, ExpressionExtract[V]#E, V](column, GenericExtract.ident(column))::Nil
			}

			override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, G >: F <: RowProduct]
			                      (e :TypedComponentSQL[F, T, E, M, V, G]) :Extractors[GlobalScope, V] =
			{
				val relation = e.origin
				val mapping = e.export

				def extractAssoc[C](column :ColumnMapping[C, G]) = {
					val expr = e.origin \ column
					val selected = ColumnSQLMapping[F, S, C, O](e.origin \ column, nameFor(expr))
					val componentExtract = mapping(column)
					val selectExtract = GenericExtract(selected)(componentExtract)
					Assoc[ExpressionColumn, ExpressionExtract[V]#E, C](selected, selectExtract)
				}
				mapping.columns.view.map(relation.export(_).export).filter(NoSelectByDefault.inactive)
					.map(extractAssoc(_)).toList
			}

			override def conversion[C >: LocalScope <: GlobalScope, T, U](e :ConversionSQL[F, C, T, U]) = {
				val extracts = apply(e.value)
				extracts.map(composeColumnExtractAssoc(Extractor.none :U =?> T)(_))
			}

			override def promotion[C >: LocalScope <: GlobalScope, T, U](e :PromotionConversion[F, C, T, U]) = {
				val extracts = apply(e.value)
				extracts.map(composeColumnExtractAssoc(Extractor(e.lift.inverse))(_))
			}

			override def seq[C >: LocalScope <: GlobalScope, V](e :SeqTuple[F, C, V]) = {
				e.inOrder.view.zipWithIndex.flatMap { entry :(SQLExpression[F, C, V], Int) =>
					apply(entry._1).map(composeColumnExtractAssoc((seq :Seq[V]) => seq(entry._2))(_))}.toList
			}

			override def chainHead[C >: LocalScope <: GlobalScope, T <: Chain, H]
			                      (tail :ChainTuple[F, C, T], head :SQLExpression[F, C, H]) =
			{
				val tailExs = apply(tail).map(composeColumnExtractAssoc(Chain.init[T] _)(_))
				val headExs = apply(head).map(composeColumnExtractAssoc(Chain.last[H] _)(_))
				headExs ++: tailExs
			}

			override def listingEntry[C >: LocalScope <: GlobalScope, I <: Listing, K <: Label :ValueOf, L]
			                         (init :ListingSQL[F, C, I], last :ListingValueSQL[F, C, L]) = {
				val tailExs = apply(init).map(composeColumnExtractAssoc(Chain.init[I] _)(_))
				val headExs = apply(last).map(composeColumnExtractAssoc((_:(I |~ (K :~ L))).last.value)(_))
				headExs ++: tailExs
			}

			override def emptyChain = Nil

			override def expression[C >: LocalScope <: GlobalScope, V](e :SQLExpression[F, C, V]) = unhandled(e)

			override def unhandled(e :SQLExpression[F, _, _]) :Nothing =
				throw new IllegalArgumentException(
					s"SQLExpression $e cannot be used in a SelectSQL select clause expression (as part of $outer)."
				)


			private def nameFor(f :ColumnSQL[F, LocalScope, _]) :String = {
				val name :String = f match {
					case UnboundParamSQL(param, extract, _) => //first as it would match the following pattern, too
						if (extract.isIdentity && !names(param.name)) param.name
						else param.name + "_" + columns.size

					case TypedComponentSQL(_, MappingExtract(_, _, component)) =>
						val name :String = component match {
							case column :ColumnMapping[_, _] => column.name //this is the almost sure case
							case label @: _ => label //more as a safeguard against refactors than anything else
							case _ => ""
						}
						if (!names(name)) name else name + "_" + columns.size

					case SQLParameter(_, Some(name)) => //unlikely to appear in this position
						if (!names(name)) name else name + "_" + columns.size

					case _ => "_" + columns.size
				}
				names = names + name
				name
			}

		}



		private type Assembler[-_ >: LocalScope <: GlobalScope, T] = Pieces => Option[T]

		/** Visitor traversing the represented expression and composing a function used for implementation
		  * of the `assemble` method.
		  */
		private class AssemblerComposer extends ExpressionMatcher[F, Assembler]
			with CaseExpression[F, Assembler] with CaseColumn[F, Assembler] with MatchChain[F, Assembler]
			with MatchListing[F, Assembler]
		{
			/** The stack is in the exact order of individual column appearance, as returned by `ExtractsCollector`. */
			private[this] var columnStack = columns.toList

			override def column[C >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, C, V]) :Pieces => Option[V] = {
				val column = columnStack.head.asInstanceOf[ExpressionColumn[V]]
				columnStack = columnStack.tail
				pieces => pieces.get(column).toOption
			}

			override def component[T[B] <: BaseMapping[E, B], E, M[B] <: BaseMapping[V, B], V, A >: F <: RowProduct]
			                      (e :TypedComponentSQL[F, T, E, M, V, A]) =
			{
				val table = e.entity
				val component = e.export
				val exported = component.columns.view.map(table.export(_)).filter(NoSelectByDefault.inactive).toList
				val count = exported.length
				val (selected, tail) = columnStack.splitAt(count)
				columnStack = tail

				def expressionColumn[C](alias :(component.Column[_], ExpressionColumn[C])) =
					Assoc[component.Component, ExpressionColumn, C](alias._1.asInstanceOf[component.Column[C]], alias._2)

				val exportAliasing = NaturalMap(exported.zip(selected).map(expressionColumn(_)) :_*)

				def selectedColumn[C](extract :Assoc[component.Column, component.ColumnExtract, C]) =
					Assoc[component.Component, ExpressionColumn, C](
						extract._1, columnExtracts(exportAliasing(extract._2.export)).export
					)
				val aliasing = component.columnExtracts.map(selectedColumn(_))

				{ pieces :Pieces =>
					val values = ComponentValues[V, A](new (MappingAt[A]#Component =#> Self) {
						override def apply[C](x :RefinedMapping[C, A]) :C =
							aliasing.getOrElse[ExpressionColumn, C](x, null) match {
								case null => null.asInstanceOf[C]
								case column => pieces.get(column) match {
									case Got(res) => res
									case _ => null.asInstanceOf[C]
								}
							}
						})
					component.optionally(values).toOption
				}
			}

			override def conversion[C >: LocalScope <: GlobalScope, T, U](e :ConversionSQL[F, C, T, U]) = {
				val base = e.value.applyTo(this) //important to have this as a constant
				pieces => base(pieces).map(e.convert)
			}

			override def emptyChain = { val res = Some(@~); _ => res }

			override def chainHead[C >: LocalScope <: GlobalScope, T <: Chain, H]
			                      (tail :ChainTuple[F, C, T], head :SQLExpression[F, C, H]) =
			{
				val tl = apply(tail)
				val hd = apply(head)
				pieces => for (t <- tl(pieces); h <- hd(pieces)) yield t ~ h
			}

			override def listingEntry[C >: LocalScope <: GlobalScope, I <: Listing, K <: Label :ValueOf, L]
			                         (init :ListingSQL[F, C, I], last :ListingValueSQL[F, C, L]) =
			{
				val tl = apply(init)
				val hd = apply(last)
				pieces => for (t <- tl(pieces); h <- hd(pieces)) yield t |~ :~[K](h)
			}

			override def seq[C >: LocalScope <: GlobalScope, V](e :SeqTuple[F, C, V]) = {
				val assemblers = e.inOrder.map(apply).reverse
				val acc = Option(List.empty[V])
				pieces => (acc /: assemblers) {
					(acc, assembler) => for (t <- acc; h <- assembler(pieces)) yield h::t
				}
			}

			override def expression[C >: LocalScope <: GlobalScope, V](e :SQLExpression[F, C, V]) =
				throw new IllegalArgumentException(
					s"SQLExpression $e cannot be used in a SelectSQL select clause expression (as part of $outer)."
				)

		}



		override val (columnExtracts, columns) = {
			val extracts = (new ExtractsCollector)(expr)
			NaturalMap((extracts :Seq[Assoc[Column, ExpressionExtract[X]#E, _]]) :_*) -> Unique(extracts.map(_._1) :_*)
		}

		override val extracts :ExtractMap = columnExtracts.asInstanceOf[ExtractMap]

		override def components :Unique[SQLMapping[F, S, _, O]] = columns
		override def subcomponents :Unique[SQLMapping[F, S, _, O]] = columns
		override def selectable :Unique[ExpressionColumn[_]] = columns
		override def filterable :Unique[ExpressionColumn[_]] = columns
		override def insertable :Unique[ExpressionColumn[_]] = Unique.empty
		override def updatable :Unique[ExpressionColumn[_]] = Unique.empty
		override def autoInserted :Unique[ExpressionColumn[_]] = Unique.empty
		override def autoUpdated :Unique[ExpressionColumn[_]] = Unique.empty
		override def selectedByDefault :Unique[ExpressionColumn[_]] = columns
		override def filteredByDefault :Unique[ExpressionColumn[_]] = columns
		override def insertedByDefault :Unique[ExpressionColumn[_]] = Unique.empty
		override def updatedByDefault :Unique[ExpressionColumn[_]] = Unique.empty

		override val selectForm :SQLReadForm[X] = expr.readForm

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[X] =
			if (columns == components)
				selectForm
			else //consider: theoretically, we could allow components to contain columns out of order, but is it worth it?
				throw new IllegalArgumentException(
					"SQLMapping offers selectForm only for full the column set.\n" +
					s"Asked for: $components;\nhave    : $columns."
				)


		private val assembler = (new AssemblerComposer)(expr)

		override def assemble(pieces: Pieces): Opt[X] = assembler(pieces)


		override def mappingName = "SQL"

		override def toString :String = expr.toString
	}


}






/** A synthetic adapter of an [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, S, O]` to a
  * [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]. Used in ''select'' and ''group by'' clauses,
  * so that [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]
  * can be homomorphic with [[net.noresttherein.oldsql.sql.Join Join]]s and to allow
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expressions to be used as
  * [[net.noresttherein.oldsql.schema.Relation Relation]]s in the ''from'' clauses of other SQL selects.
  */
trait ColumnSQLMapping[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, O]
	extends ColumnMapping[X, O] with SQLMapping[F, S, X, O]
{
	override val expr :ColumnSQL[F, S, X]

	override def name :String = expr match {
		case AliasedColumn(_, alias) => alias

		case UnboundParamSQL(param, _, _) => param.name //first as it would match the following pattern, too

		case TypedComponentSQL(_, MappingExtract(_, _, component)) =>
			component match {
				case column :ColumnMapping[_, _] => column.name
				case label @: _ => label
				case _ => "result"
			}

		case SQLParameter(_, Some(name)) => name //unlikely to appear in this position

		case _ => "result" //todo: should it be an alias, or sql?
	}

	override def format :String = name

	override def components :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override def subcomponents :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override val columns :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.single(this)
	override def selectable :Unique[ColumnSQLMapping[F, S, X, O]] = columns
	override def filterable :Unique[ColumnSQLMapping[F, S, X, O]] = columns
	override def insertable :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override def updatable :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override def autoInserted :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override def autoUpdated :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override def selectedByDefault :Unique[ColumnSQLMapping[F, S, X, O]] = selectable
	override def filteredByDefault :Unique[ColumnSQLMapping[F, S, X, O]] = filterable
	override def insertedByDefault :Unique[ColumnSQLMapping[F, S, X, O]] = insertable
	override def updatedByDefault :Unique[ColumnSQLMapping[F, S, X, O]] = updatable

	override def form :ColumnForm[X] = expr.readForm match {
		case rw :ColumnForm[X @unchecked] => rw
		case r => r <> ColumnWriteForm.unsupported(r.sqlType)(
			s"expression column $expr does not support write."
		)

	}

	override def selectForm :ColumnReadForm[X] = expr.readForm

}






object ColumnSQLMapping {

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X, O]
	         (column :ColumnSQL[F, S, X], alias :String = null) :ColumnSQLMapping[F, S, X, O] =
		new ColumnSQLMapping[F, S, X, O] {
			override val expr = column
			override val name :String = if (alias != null) alias else super.name
			override val form = super.form
			override val selectForm :ColumnReadForm[X] = expr.readForm
		}




	def unapply[X, O](mapping :MappingOf[X]) :Opt[(ColumnSQL[_, LocalScope, X], String)] =
		mapping match {
			case col :ColumnSQLMapping[_, LocalScope @unchecked, X @unchecked, O @unchecked] =>
				Got((col.expr, col.name))
			case _ => Lack
		}

}






//todo: move to schema; this trait defines nothing new; we could just as well define future indexing as extension methods
sealed trait IndexedMapping[S, O] extends BaseMapping[S, O] {
	def apply[N <: Label](label :N)(implicit get :GetListingComponent[S, N]) :IndexedMapping[get.Value, O]

	def apply[P](path :LabelPath[P])(implicit get :GetListingComponent[S, P]) :IndexedMapping[get.Value, O]

	override def components :Unique[IndexedMapping[_, O]]
	override def subcomponents :Unique[IndexedMapping[_, O]]


	override def columns(op :OperationType) :Unique[IndexedColumnMapping[_, O]] = op match {
		case SELECT => selectable
		case FILTER => filterable
		case INSERT => insertable
		case UPDATE => updatable
	}

	override def columns :Unique[IndexedColumnMapping[_, O]]
	override def selectable :Unique[IndexedColumnMapping[_, O]] = columnsWithout(NoSelect)
	override def filterable :Unique[IndexedColumnMapping[_, O]] = columnsWithout(NoFilter)
	override def insertable :Unique[IndexedColumnMapping[_, O]] = columnsWithout(NoInsert)
	override def autoInserted :Unique[IndexedColumnMapping[_, O]] = columnsWith(AutoInsert)
	override def updatable :Unique[IndexedColumnMapping[_, O]] = columnsWithout(NoUpdate)
	override def autoUpdated :Unique[IndexedColumnMapping[_, O]] = columnsWith(AutoUpdate)
	override def selectedByDefault :Unique[IndexedColumnMapping[_, O]] = columnsWithout(NoSelectByDefault)
	override def filteredByDefault :Unique[IndexedColumnMapping[_, O]] = columnsWithout(NoFilterByDefault)
	override def insertedByDefault :Unique[IndexedColumnMapping[_, O]] = columnsWithout(NoInsertByDefault)
	override def updatedByDefault :Unique[IndexedColumnMapping[_, O]] = columnsWithout(NoUpdateByDefault)

	override def columnsWith(buff :BuffType) :Unique[IndexedColumnMapping[_, O]] =
		columns.filter(buff.active)

	override def columnsWithout(buff :BuffType) :Unique[IndexedColumnMapping[_, O]] =
		columns.filter(buff.inactive)
}






sealed trait IndexedColumnMapping[S, O] extends IndexedMapping[S, O] with ColumnMapping[S, O] {
	override def components :Unique[IndexedColumnMapping[S, O]] = Unique.empty
	override def subcomponents :Unique[IndexedColumnMapping[S, O]] = Unique.empty
	override def columns :Unique[IndexedColumnMapping[S, O]] = Unique.single(this)
	override def selectable :Unique[IndexedColumnMapping[S, O]] = columns
	override def filterable :Unique[IndexedColumnMapping[S, O]] = columns
	override def insertable :Unique[IndexedColumnMapping[S, O]] = Unique.empty
	override def updatable :Unique[IndexedColumnMapping[S, O]] = Unique.empty
	override def autoInserted :Unique[IndexedColumnMapping[S, O]] = Unique.empty
	override def autoUpdated :Unique[IndexedColumnMapping[S, O]] = Unique.empty
	override def selectedByDefault :Unique[IndexedColumnMapping[S, O]] = columns
	override def filteredByDefault :Unique[IndexedColumnMapping[S, O]] = columns
	override def insertedByDefault :Unique[IndexedColumnMapping[S, O]] = Unique.empty
	override def updatedByDefault :Unique[IndexedColumnMapping[S, O]] = Unique.empty

	override def apply[N <: Label](label :N)(implicit get :GetListingComponent[S, N]) :IndexedMapping[get.Value, O] =
		this.asInstanceOf[IndexedColumnMapping[get.Value, O]]

	override def apply[P](path :LabelPath[P])(implicit get :GetListingComponent[S, P]) :IndexedMapping[get.Value, O] =
		this.asInstanceOf[IndexedColumnMapping[get.Value, O]]
}






object IndexedMapping {
	type Of[S] = { //consider: renaming projections to simply P and C
		type Projection[O] = IndexedMapping[S, O]
		type Column[O] = IndexedColumnMapping[S, O]
	}
}






trait ListingSQLMapping[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, O]
	extends SQLMapping[F, S, X, O] with IndexedMapping[X, O]
{
	override val expr :ListingValueSQL[F, S, X]

	override def apply[N <: Label](label :N)
	                              (implicit get :GetListingComponent[X, N]) :ListingSQLMapping[F, S, get.Value, O]

	override def apply[P](path :LabelPath[P])
	                     (implicit get :GetListingComponent[X, P]) :ListingSQLMapping[F, S, get.Value, O]


	override def components :Unique[ListingSQLMapping[F, S, _, O]]
	override def subcomponents :Unique[ListingSQLMapping[F, S, _, O]]

	override def columns(op :OperationType) :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] = op match {
		case SELECT => selectable
		case FILTER => filterable
		case INSERT => insertable
		case UPDATE => updatable
	}

	override def columns :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]]
	override def selectable :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWithout(NoSelect)
	override def filterable :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWithout(NoFilter)
	override def insertable :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWithout(NoInsert)
	override def updatable :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWithout(NoUpdate)
	override def autoInserted :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWith(AutoInsert)
	override def autoUpdated :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWith(AutoUpdate)

	override def selectedByDefault :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columnsWithout(NoSelectByDefault)

	override def filteredByDefault :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columnsWithout(NoFilterByDefault)

	override def insertedByDefault :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columnsWithout(NoInsertByDefault)

	override def updatedByDefault :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columnsWithout(NoUpdateByDefault)

	override def columnsWith(buff :BuffType) :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columns.filter(buff.active)

	override def columnsWithout(buff :BuffType) :Unique[ListingColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columns.filter(buff.inactive)
}






object ListingSQLMapping {
	type ListingSQLExtract[-F <: RowProduct, -S >: LocalScope <: GlobalScope, -X, Y, O] =
		GenericExtract[ListingSQLMapping[F, S, Y, O], X, Y, O]

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Listing, O](expr :ListingSQL[F, S, X])
			:ListingSQLMapping[F, S, X, O] =
		new ListingTupleMapping(expr)



	private class ListingTupleMapping[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Listing, O]
	                                 (override val expr :ListingSQL[F, S, X])
		extends ListingSQLMapping[F, S, X, O] with ExportMapping
	{ outer =>
		private type Expression[V] = SQLExpression[F, LocalScope, V]
		private type ColumnExpression[N <: Label, V] = ListingColumnSQLMapping[F, S, N, V, O]
		private type Subcomponent[V] = ListingSQLMapping[F, S, V, O]

		private def subcomponent[T](e :ListingValueSQL[F, S, T]) :Assoc[Expression, Subcomponent, T] =
			Assoc[Expression, Subcomponent, T](e, e.mapping[O])

		private[this] val subexpressions = NaturalMap(expr.toSeq.map(subcomponent(_)):_*)
		private[this] val index = expr.toMap.map { case (label, e) => (label, subexpressions(e)) }

		private def at(path :Seq[String]) :ListingSQLMapping[F, S, _, O] = path match {
			case hd +: tail =>
				if (tail.isEmpty) index(hd)
				else index(hd).asInstanceOf[ListingTupleMapping[F, S, _, O]].at(tail)
			case _ =>
				this
		}

		override def apply[N <: Label](label :N)(implicit get :GetListingComponent[X, N]) :ListingSQLMapping[F, S, get.Value, O] =
			index(label).asInstanceOf[ListingSQLMapping[F, S, get.Value, O]]

		override def apply[P](path :LabelPath[P])(implicit get :GetListingComponent[X, P]) :ListingSQLMapping[F, S, get.Value, O] =
			at(path.toSeq).asInstanceOf[ListingSQLMapping[F, S, get.Value, O]]



		private type Extracts[-C >: LocalScope <: GlobalScope, V] = List[ListingSQLExtract[F, C, V, _, O]]

		private class ComponentsCollector extends BaseExpressionMatcher[F, Extracts] with MatchListing[F, Extracts] {

			override def listingEntry[C >: LocalScope <: GlobalScope, I <: Listing, K <: Label :ValueOf, L]
			                         (init :ListingSQL[F, C, I], last :ListingValueSQL[F, C, L]) =
			{
				val extracts = apply(init).map(_ compose Chain.init[I] _) :Extracts[C, I |~ (K :~ L)]
				val extract = GenericExtract.req(subexpressions(last).asInstanceOf[ListingSQLMapping[F, C, L, O]]) {
					(_ :(I |~ (K :~ L))).last.value
				}
				extract::extracts
			}

			override def emptyChain = Nil
		}

		override val (components, subcomponents, extracts) = {
			val collected = (new ComponentsCollector)(expr)
			def assoc[V](extract :ListingSQLExtract[F, S, X, V, O]) =
				Assoc[Component, Extract, V](extract.export, extract)
			val all = collected.flatMap { composeExtracts(_) } ++ collected.map(assoc(_))
			val map = NaturalMap((all :Seq[Assoc[Component, Extract, _]]) :_*)
			val comps = Unique(collected.map(_.export):_*)
			val subs = Unique(collected.flatMap { e => e.export +: e.export.subcomponents } :_*)
			(comps, subs, map)
		}

		override val columns = components.flatMap(_.columns)
		override val columnExtracts = filterColumnExtracts(this)(extracts)

		override def selectable :Unique[ColumnExpression[_ <: Label, _]] = columns
		override def filterable :Unique[ColumnExpression[_ <: Label, _]] = columns
		override def insertable :Unique[ColumnExpression[_ <: Label, _]] = Unique.empty
		override def updatable :Unique[ColumnExpression[_ <: Label, _]] = Unique.empty
		override def autoInserted :Unique[ColumnExpression[_ <: Label, _]] = Unique.empty
		override def autoUpdated :Unique[ColumnExpression[_ <: Label, _]] = Unique.empty
		override def selectedByDefault :Unique[ColumnExpression[_ <: Label, _]] = selectable
		override def filteredByDefault :Unique[ColumnExpression[_ <: Label, _]] = filterable
		override def insertedByDefault :Unique[ColumnExpression[_ <: Label, _]] = insertable
		override def updatedByDefault :Unique[ColumnExpression[_ <: Label, _]] = updatable

		private val columnMap = columns.view.map { c => (c.name, c) }.toMap

		override def columnNamed(name :String) :Column[_] = columnMap.getOrElse(name, null) match {
			case null => throw new NoSuchComponentException("No column '" + name + "' in " + this + ".")
			case res => res
		}

		private type Assembler[-_ >: LocalScope <: GlobalScope, T] = Pieces => Opt[T]

		private class AssemblerComposer extends BaseExpressionMatcher[F, Assembler] with MatchListing[F, Assembler] {
			override def emptyChain = { val res = Got(@~); _ => res }

			override def listingEntry[C >: LocalScope <: GlobalScope, I <: Listing, K <: Label :ValueOf, L]
			                         (init :ListingSQL[F, C, I], last :ListingValueSQL[F, C, L]) :Assembler[C, I |~ (K :~ L)] =
			{
				val tl = apply(init)
				val hd = subexpressions(last)
				pieces => for (t <- tl(pieces); h <- pieces.get(hd)) yield t |~ :~[K](h)
			}
		}

		private[this] val assembler = (new AssemblerComposer)(expr)

		override def assemble(pieces :Pieces) = assembler(pieces)


	}






	class GetListingComponent[X, N] private[ListingSQLMapping] {
		type Value
	}

	object GetListingComponent {
		def last[I <: Listing, K, V, N <: Label] :GetListingComponent[I |~ (K :~ V), N] { type Value = V } =
			instance.asInstanceOf[GetListingComponent[I |~ (K :~ V), N] { type Value = V }]

		def previous[I <: Listing, K, V, P](implicit prefix :GetListingComponent[I, P])
				:GetListingComponent[I |~ (K :~ P), P] { type Value = prefix.Value } =
			instance.asInstanceOf[GetListingComponent[I |~ (K :~ P), P] { type Value = prefix.Value }]

		def nested[I <: Listing, T, P, N <: Label]
		          (implicit prefix :GetListingComponent[I, P] { type Value = T }, last :GetListingComponent[T, N])
				:GetListingComponent[I, P / N] { type Value = last.Value } =
			instance.asInstanceOf[GetListingComponent[I, P / N] { type Value = last.Value }]

		private[this] val instance = new GetListingComponent[Any, Any] {}
	}

}






trait ListingColumnSQLMapping[-F <: RowProduct, -S >: LocalScope <: GlobalScope, N <: Label, X, O]
	extends ColumnSQLMapping[F, S, X, O] with ListingSQLMapping[F, S, X, O] with IndexedColumnMapping[X, O]
{
	override val expr :ListingColumn[F, S, N, X]

	override def components :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def subcomponents :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override val columns :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = Unique.single(this)
	override def selectable :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = columns
	override def filterable :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = columns
	override def insertable :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def updatable :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def autoInserted :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def autoUpdated :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def selectedByDefault :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = columns
	override def filteredByDefault :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = columns
	override def insertedByDefault :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def updatedByDefault :Unique[ListingColumnSQLMapping[F, S, N, X, O]] = Unique.empty

	override def apply[K <: Label](label :K)(implicit get :GetListingComponent[X, K]) :ListingSQLMapping[F, S, get.Value, O] =
		this.asInstanceOf[ListingSQLMapping[F, S, get.Value, O]] //these shouldn't be possible to call, but to guard against a future refactor

	override def apply[P](path :LabelPath[P])(implicit get :GetListingComponent[X, P]) :ListingSQLMapping[F, S, get.Value, O] =
		this.asInstanceOf[ListingSQLMapping[F, S, get.Value, O]]
}






object ListingColumnSQLMapping {

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, N <: Label, X, O]
	         (column :ListingColumn[F, S, N, X])
			:ListingColumnSQLMapping[F, S, N, X, O] =
		new ListingColumnSQLMapping[F, S, N, X, O] {
			override val expr = column
			override val name :String = column.alias
			override val form = super.form
			override val selectForm :ColumnReadForm[X] = expr.readForm
		}


	def unapply[X, O](mapping :MappingOf[X]) :Opt[ListingColumn[_ <: RowProduct, LocalScope, _ <: Label, X]] =
		mapping match {
			case indexed :ListingColumnSQLMapping[f, LocalScope @unchecked, n, X @unchecked, _] =>
				Got(indexed.expr)
			case _ => Lack
		}

	type Column[F <: RowProduct, S >: LocalScope <: GlobalScope, N <: Label, X] = {
		type Projection[O] = ListingColumnSQLMapping[F, S, N, X, O] //consider: renaming to P
	}

}


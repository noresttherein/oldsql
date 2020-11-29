package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.{Chain, IndexedChain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.generic.=#>
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.schema.{composeExtracts, filterColumnExtracts, Buff, ColumnForm, ColumnMapping, ColumnReadForm, ColumnWriteForm, ComponentValues, GenericExtract, MappingExtract, Relation, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, ReadOnly}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.sql.ColumnSQL.{AliasedColumn, CaseColumn}
import net.noresttherein.oldsql.sql.SQLExpression.{BaseExpressionMatcher, CaseExpression, ExpressionMatcher, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.UnboundParam.UnboundParamSQL
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.schema.ComponentValues.{ColumnValues, ComponentValuesBuilder}
import net.noresttherein.oldsql.schema.bases.{BaseMapping, LazyMapping}
import net.noresttherein.oldsql.sql.ast.{ConversionSQL, SelectSQL}
import net.noresttherein.oldsql.sql.ast.ConversionSQL.PromotionConversion
import net.noresttherein.oldsql.sql.ast.MappingSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter
import net.noresttherein.oldsql.sql.ast.TupleSQL.{ChainTuple, IndexedChainTuple, SeqTuple}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.MatchChain
import net.noresttherein.oldsql.sql.ast.TupleSQL.IndexedChainTuple.{IndexedColumn, IndexedSQLExpression, MatchIndexedChain}
import net.noresttherein.oldsql.sql.IndexedSQLMapping.GetIndexedExpressionComponent






/** A synthetic adapter of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, O]` to a
  * [[net.noresttherein.oldsql.schema.Mapping Mapping]]. Used in ''select'' and ''group by'' clauses,
  * so that [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]
  * can be homomorphic with [[net.noresttherein.oldsql.sql.Join Join]]s and to allow
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expressions to be used as
  * [[net.noresttherein.oldsql.schema.Relation Relation]]s in the ''from'' clauses of other SQL selects.
  * This class is dedicated to non-component expressions; subclasses of
  * [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]] should be used directly.
  * Not all possible expressions are supported; the expression may consist of
  *   - any single [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] (atomic SQL values),
  *     in particular [[net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm terms]],
  *   - [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL components]] (ranging from whole entities
  *     to single columns),
  *   - [[ConversionSQL conversion]] nodes,
  *   - any [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL composites]] combining the above, in particular:
  *   - [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuples]] and
  *     [[ast.TupleSQL.IndexedChainTuple indexed tuples]].
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

	override def buffs :Seq[Buff[X]] = SQLMapping.buffList.asInstanceOf[Seq[Buff[X]]]

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
	override def updatable :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoUpdate)
	override def autoUpdated :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWith(AutoUpdate)
	override def insertable :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoInsert)
	override def autoInserted :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWith(AutoInsert)
	override def selectedByDefault :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoSelectByDefault)
	override def filteredByDefault :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoFilterByDefault)
	override def updatedByDefault :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoUpdateByDefault)
	override def insertedByDefault :Unique[ColumnSQLMapping[F, S, _, O]] = columnsWithout(NoInsertByDefault)

	override def columnsWith(buff :BuffType) :Unique[ColumnSQLMapping[F, S, _, O]] =
		columns.filter(buff.enabled)

	override def columnsWithout(buff :BuffType) :Unique[ColumnSQLMapping[F, S, _, O]] =
		columns.filter(buff.disabled)

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

	def unapply[X](mapping :RefinedMapping[X, _]) :Option[SQLExpression[_, LocalScope, X]] =
		mapping match {
			case expr :SQLMapping[_, LocalScope @unchecked, X @unchecked, _] =>
				Some(expr.expr)
			case _ => None
		}





	type Project[F <: RowProduct, S >: LocalScope <: GlobalScope, X] = {
		type Expression[O] = SQLMapping[F, S, X, O]
		type Column[O] = ColumnSQLMapping[F, S, X, O]
		type IndexedExpression[O] = IndexedSQLMapping[F, S, X, O]
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
			with MatchIndexedChain[F, Extractors]
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
				mapping.columns.view.map(relation.export(_).export).filter(NoSelectByDefault.disabled)
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

			override def indexedChainHead[C >: LocalScope <: GlobalScope, I <: IndexedChain, K <: Label :ValueOf, L]
			                             (init :IndexedChainTuple[F, C, I], last :IndexedSQLExpression[F, C, L]) = {
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
			with MatchIndexedChain[F, Assembler]
		{
			/** The stack is in the exact order of individual column appearance, as returned by `ExtractsCollector`. */
			private[this] var columnStack = columns.toList

			override def column[C >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, C, V]) :Pieces => Option[V] = {
				val column = columnStack.head.asInstanceOf[ExpressionColumn[V]]
				columnStack = columnStack.tail
				pieces => pieces.get(column)
			}

			override def component[T[B] <: BaseMapping[E, B], E, M[B] <: BaseMapping[V, B], V, A >: F <: RowProduct]
			                      (e :TypedComponentSQL[F, T, E, M, V, A]) =
			{
				val table = e.entity
				val component = e.mapping //fixme: this doesn't take into account include/exclude
				val exported = component.columns.view.map(table.export(_)).filter(NoSelectByDefault.disabled).toList
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
					val values = ComponentValues[V, A](new (MappingAt[A]#Component =#> Option) {
						override def apply[C](x :RefinedMapping[C, A]) = aliasing.get(x) match {
							case Some(column) => pieces.get(column)
							case _ => None
						}
					})
					component.optionally(values)
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

			override def indexedChainHead[C >: LocalScope <: GlobalScope, I <: IndexedChain, K <: Label :ValueOf, L]
			                             (init :IndexedChainTuple[F, C, I], last :IndexedSQLExpression[F, C, L]) =
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
		override def updatable :Unique[ExpressionColumn[_]] = Unique.empty
		override def autoUpdated :Unique[ExpressionColumn[_]] = Unique.empty
		override def insertable :Unique[ExpressionColumn[_]] = Unique.empty
		override def autoInserted :Unique[ExpressionColumn[_]] = Unique.empty
		override def selectedByDefault :Unique[ExpressionColumn[_]] = columns
		override def filteredByDefault :Unique[ExpressionColumn[_]] = columns
		override def updatedByDefault :Unique[ExpressionColumn[_]] = Unique.empty
		override def insertedByDefault :Unique[ExpressionColumn[_]] = Unique.empty

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

		override def assemble(pieces: Pieces): Option[X] = assembler(pieces)


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


	override def columns :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.single(this)
	override def selectable :Unique[ColumnSQLMapping[F, S, X, O]] = columns
	override def filterable :Unique[ColumnSQLMapping[F, S, X, O]] = columns
	override def updatable :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override def autoUpdated :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override def insertable :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override def autoInserted :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.empty
	override def selectedByDefault :Unique[ColumnSQLMapping[F, S, X, O]] = selectable
	override def filteredByDefault :Unique[ColumnSQLMapping[F, S, X, O]] = filterable
	override def updatedByDefault :Unique[ColumnSQLMapping[F, S, X, O]] = updatable
	override def insertedByDefault :Unique[ColumnSQLMapping[F, S, X, O]] = insertable

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
			override val columns :Unique[ColumnSQLMapping[F, S, X, O]] = Unique.single(this)
			override val form = super.form
			override val selectForm :ColumnReadForm[X] = expr.readForm
		}




	def unapply[X, O](mapping :MappingOf[X]) :Option[(ColumnSQL[_, LocalScope, X], String)] =
		mapping match {
			case col :ColumnSQLMapping[_, LocalScope @unchecked, X @unchecked, O @unchecked] =>
				Some((col.expr, col.name))
			case _ => None
		}

}






sealed trait IndexedMapping[S, O] extends BaseMapping[S, O] {
	override def components :Unique[IndexedMapping[_, O]]
	override def subcomponents :Unique[IndexedMapping[_, O]]
}





trait IndexedSQLMapping[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, O]
	extends SQLMapping[F, S, X, O] with IndexedMapping[X, O]
{
	override val expr :IndexedSQLExpression[F, S, X]

	def apply[N <: Label](label :N)(implicit get :GetIndexedExpressionComponent[F, S, X, N]) :get.Component[O] =
		get(this, label)


	override def components :Unique[IndexedSQLMapping[F, S, _, O]]
	override def subcomponents :Unique[IndexedSQLMapping[F, S, _, O]]

	override def columns(op :OperationType) :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] = op match {
		case SELECT => selectable
		case FILTER => filterable
		case INSERT => insertable
		case UPDATE => updatable
	}

	override def columns :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]]
	override def selectable :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWithout(NoSelect)
	override def filterable :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWithout(NoFilter)
	override def updatable :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWithout(NoUpdate)
	override def autoUpdated :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWith(AutoUpdate)
	override def insertable :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWithout(NoInsert)
	override def autoInserted :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] = columnsWith(AutoInsert)

	override def selectedByDefault :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columnsWithout(NoSelectByDefault)

	override def filteredByDefault :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columnsWithout(NoFilterByDefault)

	override def updatedByDefault :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columnsWithout(NoUpdateByDefault)

	override def insertedByDefault :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columnsWithout(NoInsertByDefault)

	override def columnsWith(buff :BuffType) :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columns.filter(buff.enabled)

	override def columnsWithout(buff :BuffType) :Unique[IndexedColumnSQLMapping[F, S, _ <: Label, _, O]] =
		columns.filter(buff.disabled)
}






object IndexedSQLMapping {
	type IndexedSQLExtract[-F <: RowProduct, -S >: LocalScope <: GlobalScope, -X, Y, O] =
		GenericExtract[IndexedSQLMapping[F, S, Y, O], X, Y, O]

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: IndexedChain, O](expr :IndexedChainTuple[F, S, X])
			:IndexedSQLMapping[F, S, X, O] =
		new IndexedTupleMapping(expr)



	trait GetIndexedExpressionComponent[+F <: RowProduct, +S >: LocalScope <: GlobalScope, X, N <: Label] {
		type Component[O] <: SQLMapping[_, LocalScope, _, O]
		def apply[O](mapping :IndexedSQLMapping[F, S, X, O], label :N) :Component[O]
	}



	private class IndexedTupleMapping[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: IndexedChain, O]
	                                 (override val expr :IndexedChainTuple[F, S, X])
		extends IndexedSQLMapping[F, S, X, O]
	{ outer =>

		private type Expression[V] = IndexedSQLMapping[F, S, V, O]
		private type ColumnExpression[N <: Label, V] = IndexedColumnSQLMapping[F, S, N, V, O]
		private type IndexedExtract[W, P] = GenericExtract[IndexedSQLMapping[F, S, P, O], W, P, O]
		private type MyExtract[V] = IndexedExtract[X, V]

		private type Extracts[-C >: LocalScope <: GlobalScope, V] = List[IndexedSQLExtract[F, C, V, _, O]]


		private class ComponentsCollector extends BaseExpressionMatcher[F, Extracts] with MatchIndexedChain[F, Extracts] {

			override def indexedChainHead[C >: LocalScope <: GlobalScope, I <: IndexedChain, K <: Label :ValueOf, L]
			                             (init :IndexedChainTuple[F, C, I], last :IndexedSQLExpression[F, C, L]) =
			{
				val extracts = apply(init).map(_ compose Chain.init[I] _) :Extracts[C, I |~ (K :~ L)]
				val extract = GenericExtract.req(last.mapping[O])((_:(I |~ (K :~ L))).last.value)
				extract::extracts
			}

			override def emptyChain = Nil
		}

		private type Assembler[-_ >: LocalScope <: GlobalScope, T] = Pieces => Option[T]

		private class AssemblerComposer extends BaseExpressionMatcher[F, Assembler] with MatchIndexedChain[F, Assembler] {
			override def emptyChain = { val res = Some(@~); _ => res }

			override def indexedChainHead[C >: LocalScope <: GlobalScope, I <: IndexedChain, K <: Label :ValueOf, L]
			             (init :IndexedChainTuple[F, C, I], last :IndexedSQLExpression[F, C, L]) =
			{
				val tl = apply(init)
				val hd = apply(last)
				pieces => for (t <- tl(pieces); h <- hd(pieces)) yield t |~ :~[K](h)
			}
		}

		override val (components, subcomponents, extracts) = {
			val collected = (new ComponentsCollector)(expr)
			def assoc[V](extract :IndexedSQLExtract[F, S, X, V, O]) =
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
		override def updatable :Unique[ColumnExpression[_ <: Label, _]] = Unique.empty
		override def insertable :Unique[ColumnExpression[_ <: Label, _]] = Unique.empty
		override def autoUpdated :Unique[ColumnExpression[_ <: Label, _]] = Unique.empty
		override def autoInserted :Unique[ColumnExpression[_ <: Label, _]] = Unique.empty
		override def selectedByDefault :Unique[ColumnExpression[_ <: Label, _]] = selectable
		override def filteredByDefault :Unique[ColumnExpression[_ <: Label, _]] = filterable
		override def updatedByDefault :Unique[ColumnExpression[_ <: Label, _]] = updatable
		override def insertedByDefault :Unique[ColumnExpression[_ <: Label, _]] = insertable


		private[this] val assembler = (new AssemblerComposer)(expr)

		override def assemble(pieces :Pieces) = assembler(pieces)
	}

}






trait IndexedColumnSQLMapping[-F <: RowProduct, -S >: LocalScope <: GlobalScope, N <: Label, X, O]
	extends ColumnSQLMapping[F, S, X, O] with IndexedSQLMapping[F, S, X, O]
{
	override val expr :IndexedColumn[F, S, N, X]

	override def columns :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = Unique.single(this)
	override def selectable :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = columns
	override def filterable :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = columns
	override def updatable :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def autoUpdated :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def insertable :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def autoInserted :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def selectedByDefault :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = columns
	override def filteredByDefault :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = columns
	override def updatedByDefault :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = Unique.empty
	override def insertedByDefault :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = Unique.empty
}






object IndexedColumnSQLMapping {

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, N <: Label, X, O]
	         (column :IndexedColumn[F, S, N, X])
			:IndexedColumnSQLMapping[F, S, N, X, O] =
		new IndexedColumnSQLMapping[F, S, N, X, O] {
			override val expr = column
			override val name :String = column.alias
			override val columns :Unique[IndexedColumnSQLMapping[F, S, N, X, O]] = Unique.single(this)
			override val form = super.form
			override val selectForm :ColumnReadForm[X] = expr.readForm
		}


	def unapply[X, O](mapping :MappingOf[X]) :Option[IndexedColumn[_ <: RowProduct, LocalScope, _ <: Label, X]] =
		mapping match {
			case indexed :IndexedColumnSQLMapping[f, LocalScope @unchecked, n, X @unchecked, _] => Some(indexed.expr)
			case _ => None
		}

	type Column[F <: RowProduct, S >: LocalScope <: GlobalScope, N <: Label, X] = {
		type Projection[O] = IndexedColumnSQLMapping[F, S, N, X, O]
	}

}


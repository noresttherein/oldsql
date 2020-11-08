package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.{Chain, IndexedChain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.generic.=#>
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.{schema, OperationType}
import net.noresttherein.oldsql.schema.{BaseMapping, Buff, ColumnExtract, ColumnForm, ColumnMapping, ColumnMappingExtract, ColumnWriteForm, ComponentValues, MappingExtract, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.StableColumn
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, NoInsert, NoQuery, NoSelect, NoSelectByDefault, NoUpdate, ReadOnly}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.support.LazyMapping
import net.noresttherein.oldsql.sql.ColumnSQL.{AliasedColumn, CaseColumn}
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, ExpressionMatcher, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.UnboundParam.UnboundParamSQL
import net.noresttherein.oldsql.OperationType.{INSERT, QUERY, SELECT, UPDATE}
import net.noresttherein.oldsql.sql.ast.{ConversionSQL, MappingSQL, SelectSQL, SQLTerm, TupleSQL}
import net.noresttherein.oldsql.sql.ast.ConversionSQL.PromotionConversion
import net.noresttherein.oldsql.sql.ast.MappingSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter
import net.noresttherein.oldsql.sql.ast.TupleSQL.{ChainTuple, IndexedChainTuple, SeqTuple}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.MatchChain
import net.noresttherein.oldsql.sql.ast.TupleSQL.IndexedChainTuple.{IndexedSQLExpression, MatchIndexedChain}


/** A synthetic adapter of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, O]` to a
  * [[net.noresttherein.oldsql.schema.Mapping Mapping]]. Used in ''select'' and ''group by'' clauses,
  * so that [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]
  * can be homomorphic with [[net.noresttherein.oldsql.sql.Join Join]]s and to allow
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expressions to be used as
  * [[net.noresttherein.oldsql.schema.Relation Relation]]s in the ''from'' clauses of other SQL selects.
  * This class is dedicated to non-component expressions; subclasses of
  * [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]] should be used directly.
  * Not all possible expressions are supported; the expression may consist of
  *   - [[net.noresttherein.oldsql.sql.ast.SQLTerm terms]], todo: these not, unless we the SQLForm will list column names
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
  * @see [[net.noresttherein.oldsql.sql.ExpressionColumnMapping ExpressionColumnMapping]]
  * @author Marcin Mościcki
  */
trait ExpressionMapping[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, O] extends BaseMapping[X, O] {
	
	val expr :SQLExpression[F, S, X]

	def format :String = ??? //todo:

	override def buffs :Seq[Buff[X]] = ExpressionMapping.buffList.asInstanceOf[Seq[Buff[X]]]

	override def columns(op :OperationType) :Unique[ExpressionColumnMapping[F, S, _, O]] = op match {
		case SELECT => selectable
		case QUERY => queryable
		case INSERT => insertable
		case UPDATE => updatable
	}

	override def columns :Unique[ExpressionColumnMapping[F, S, _, O]]
	override def selectable :Unique[ExpressionColumnMapping[F, S, _, O]] = columnsWithout(NoSelect)
	override def queryable :Unique[ExpressionColumnMapping[F, S, _, O]] = columnsWithout(NoQuery)
	override def updatable :Unique[ExpressionColumnMapping[F, S, _, O]] = columnsWithout(NoUpdate)
	override def autoUpdated :Unique[ExpressionColumnMapping[F, S, _, O]] = columnsWith(AutoUpdate)
	override def insertable :Unique[ExpressionColumnMapping[F, S, _, O]] = columnsWithout(NoInsert)
	override def autoInserted :Unique[ExpressionColumnMapping[F, S, _, O]] = columnsWith(AutoInsert)

	override def columnsWith(buff :BuffType) :Unique[ExpressionColumnMapping[F, S, _, O]] =
		columns.filter(buff.enabled)	

	override def columnsWithout(buff :BuffType) :Unique[ExpressionColumnMapping[F, S, _, O]] =
		columns.filter(buff.disabled)
}






object ExpressionMapping {
	private val buffList = ReadOnly::Nil

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X, O]
	         (expression :SQLExpression[F, S, X]) :ExpressionMapping[F, S, X, O] =
		expression match {
			case column :ColumnSQL[F @unchecked, S @unchecked, X @unchecked] =>
				ExpressionColumnMapping(column)
			case _ =>
				new NonColumnExpressionMapping(expression)
		}

	def unapply[X](mapping :RefinedMapping[X, _]) :Option[SQLExpression[_, LocalScope, X]] =
		mapping match {
			case expr :ExpressionMapping[_, LocalScope @unchecked, X @unchecked, _] =>
				Some(expr.expr)
			case _ => None
		}





	type Expression[F <: RowProduct, S >: LocalScope <: GlobalScope, X] = {
		type Projection[O] = ExpressionMapping[F, S, X, O]
		type ColumnProjection[O] = ExpressionColumnMapping[F, S, X, O]
	}



	class NonColumnExpressionMapping[F <: RowProduct, S >: LocalScope <: GlobalScope, X, O]
	                                (override val expr :SQLExpression[F, S, X])
		extends ExpressionMapping[F, S, X, O] with LazyMapping[X, O]
	{ outer =>
		private type ExpressionColumn[V] = ExpressionColumnMapping[F, S, V, O]

		private type Assembler[-_ >: LocalScope <: GlobalScope, T] = Pieces => Option[T]

		/** Visitor traversing the represented expression and composing a function used for implementation
		  * of the `assemble` method, while listing all columns of the expression as `ColumnMapping`s at the same time
		  */
		private class AssemblerComposer extends ExpressionMatcher[F, Assembler]
			with CaseExpression[F, Assembler] with CaseColumn[F, Assembler] with MatchChain[F, Assembler]
			with MatchIndexedChain[F, Assembler]
		{
			/** All columns appearing in `ExpressionMapping.this.expr`, in the reverse order. */
			var columns :List[ExpressionColumn[_]] = Nil
			private[this] var names :Set[String] = Set("") //used column names, disallowing ""

			override def column[C >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, C, V]) :Pieces => Option[V] = {
				val column = ExpressionColumnMapping[F, S, V, O](e.asInstanceOf[ColumnSQL[F, S, V]], nameFor(e))
				columns = column::columns
				pieces => pieces.get(column)
			}

			override def component[T[B] <: BaseMapping[E, B], E, M[B] <: BaseMapping[V, B], V, A >: F <: RowProduct]
			                      (e :TypedComponentSQL[F, T, E, M, V, A]) =
			{
				val table = e.entity

				def expressionColumn[C](column :ColumnMapping[C, A]) :Assoc[table.Component, ExpressionColumn, C] = {
					val expr = e.origin \ column
					val selected = ExpressionColumnMapping[F, S, C, O](e.origin \ column, nameFor(expr))
					Assoc[table.Component, ExpressionColumn, C](column, selected)
				}
				val mapping = e.mapping
				val columnMappings = mapping.selectable.view.map(table.export(_)).filter(NoSelectByDefault.disabled)
					.map(expressionColumn(_)).toList
				columns = columnMappings.map(_._2) reverse_::: columns
				val aliases = NaturalMap(columnMappings :_*)

				{ pieces :Pieces =>
					val values = ComponentValues[V, A](new (MappingAt[A]#Component =#> Option) {
						override def apply[C](x :RefinedMapping[C, A]) = aliases.get(x) match {
							case Some(column) => pieces.get(column)
							case _ => None
						}
					})
					e.mapping.optionally(values)
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
					s"SQLExpression $e cannot be used in a SelectSQL header expression."
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
							case _ => component.sqlName getOrElse ""
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




		private type Extractors[-_ >: LocalScope <: GlobalScope, V] =
			Seq[Assoc[Column, ({ type E[T] = ColumnMappingExtract[V, T, O]})#E, _]]


		/** Traverses again the `expr` AST in the steps of `AssemblerComposer`, creating `Extract`s for every column. */
		private class ExtractsCollector extends ExpressionMatcher[F, Extractors]
			with CaseExpression[F, Extractors] with CaseColumn[F, Extractors] with MatchChain[F, Extractors]
			with MatchIndexedChain[F, Extractors]
		{
			/** The stack is in the exact order of individual column appearance, as returned by the `AssemblerComposer`. */
			private[this] var columnStack = outer.inlined.toList

			override def column[C >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, C, V]) :Extractors[C, V] = {
				val column = columnStack.head.asInstanceOf[ExpressionColumn[V]]
				columnStack = columnStack.tail
				type ColumnEx[T] = ColumnMappingExtract[V, T, O]
				Assoc[Column, ColumnEx, V](column, ColumnExtract.ident(column))::Nil
			}

			override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, G >: F <: RowProduct]
			                      (e :TypedComponentSQL[F, T, E, M, V, G]) :Extractors[GlobalScope, V] =
			{
				val table = e.entity
				val component = e.mapping
				val componentColumns = component.selectable.toSeq.map(table.export(_)).filter(NoSelectByDefault.disabled)
				val count = componentColumns.size
				val (selectColumns, tail) = columnStack.splitAt(count)
				columnStack = tail
				type ColumnEx[C] = ColumnMappingExtract[V, C, O]

				def extract[C](selectColumn :ExpressionColumn[C], componentColumn :component.Column[_]) = {
					val compatible = componentColumn.asInstanceOf[component.Column[C]]
					val componentExtract = component(compatible)
					val selectExtract = ColumnExtract(selectColumn)(componentExtract)
					Assoc[Column, ColumnEx, C](selectColumn, selectExtract)
				}

				selectColumns.zip(componentColumns).map { case (selCol, compCol) => extract(selCol, compCol) }
			}

			override def conversion[C >: LocalScope <: GlobalScope, T, U](e :ConversionSQL[F, C, T, U]) = {
				val extracts = apply(e.value)
				extracts.map(schema.composeColumnExtractAssoc(outer, Extractor.none :U =?> T)(_))
			}

			override def promotion[C >: LocalScope <: GlobalScope, T, U](e :PromotionConversion[F, C, T, U]) = {
				val extracts = apply(e.value)
				extracts.map(schema.composeColumnExtractAssoc(outer, Extractor(e.lift.inverse))(_))
			}

			override def seq[C >: LocalScope <: GlobalScope, V](e :SeqTuple[F, C, V]) = {
				e.inOrder.view.zipWithIndex.flatMap { entry :(SQLExpression[F, C, V], Int) =>
					apply(entry._1).map(
						schema.composeColumnExtractAssoc(outer, (seq :Seq[V]) => seq(entry._2))(_)
					)
				}.toList
			}

			override def chainHead[C >: LocalScope <: GlobalScope, T <: Chain, H]
			                      (tail :ChainTuple[F, C, T], head :SQLExpression[F, C, H]) =
			{
				val tailExs = apply(tail).map(schema.composeColumnExtractAssoc(outer, Chain.init[T] _)(_))
				val headExs = apply(head).map(schema.composeColumnExtractAssoc(outer, Chain.last[H] _)(_))
				headExs ++: tailExs
			}

			override def indexedChainHead[C >: LocalScope <: GlobalScope, I <: IndexedChain, K <: Label :ValueOf, L]
			                             (init :IndexedChainTuple[F, C, I], last :IndexedSQLExpression[F, C, L]) = {
				val tailExs = apply(init).map(schema.composeColumnExtractAssoc(outer, Chain.init[I] _)(_))
				val headExs = apply(last).map(schema.composeColumnExtractAssoc(outer, (_:(I |~ (K :~ L))).last.value)(_))
				headExs ++: tailExs
			}

			override def emptyChain = Nil


			override def expression[C >: LocalScope <: GlobalScope, V](e :SQLExpression[F, C, V]) = unhandled(e)
		}



		override def assemble(pieces: Pieces): Option[X] = assembler(pieces)


		private val (assembler, inlined) = {
			val ac = new AssemblerComposer
			ac(expr) -> ac.columns.reverse
		}

		override val columns :Unique[ExpressionColumn[_]] = Unique(inlined :_*)
		override def selectable :Unique[ExpressionColumn[_]] = columns
		override def queryable :Unique[ExpressionColumn[_]] = columns
		override def updatable :Unique[ExpressionColumn[_]] = Unique.empty
		override def autoUpdated :Unique[ExpressionColumn[_]] = Unique.empty
		override def insertable :Unique[ExpressionColumn[_]] = Unique.empty
		override def autoInserted :Unique[ExpressionColumn[_]] = Unique.empty
		override def components :Unique[Component[_]] = columns
		override def subcomponents :Unique[Component[_]] = columns

		override val columnExtracts :ColumnExtractMap =
			NaturalMap.Lazy((new ExtractsCollector)(expr) :Seq[Assoc[Column, ColumnExtract, _]])

		override val extracts :ExtractMap = columnExtracts.asInstanceOf[ExtractMap]

		override val selectForm :SQLReadForm[X] = expr.readForm

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[X] =
			if (columns contentsEqual components) selectForm
			else
				throw new IllegalArgumentException(
					"ExpressionMapping offers selectForm only for full the column set.\n" +
					s"Asked for: $components;\nhave    : $columns."
				)


		override def mappingName = "SQL"

		override def toString :String = expr.toString
	}






//	class IndexedExpressionMapping[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, O]
}






/** A synthetic adapter of an [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, S, O]` to a
  * [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]. Used in ''select'' and ''group by'' clauses,
  * so that [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]
  * can be homomorphic with [[net.noresttherein.oldsql.sql.Join Join]]s and to allow
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expressions to be used as
  * [[net.noresttherein.oldsql.schema.Relation Relation]]s in the ''from'' clauses of other SQL selects.
  */
trait ExpressionColumnMapping[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X, O]
	extends ExpressionMapping[F, S, X, O] with ColumnMapping[X, O]
{
	override val expr :ColumnSQL[F, S, X]

	override def format :String = name

	override def columns :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.single(this)
	override def selectable :Unique[ExpressionColumnMapping[F, S, X, O]] = columns
	override def queryable :Unique[ExpressionColumnMapping[F, S, X, O]] = columns
	override def updatable :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.empty
	override def autoUpdated :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.empty
	override def insertable :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.empty
	override def autoInserted :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.empty
}






object ExpressionColumnMapping {

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X, O]
	         (column :ColumnSQL[F, S, X], alias :String = null) :ExpressionColumnMapping[F, S, X, O] =
		new ExpressionColumnMapping[F, S, X, O] with StableColumn[X, O] {
			override val expr = column

			override val columns :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.single(this)
			override val selectable :Unique[ExpressionColumnMapping[F, S, X, O]] = columns
			override val queryable :Unique[ExpressionColumnMapping[F, S, X, O]] = columns
			override val updatable :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.empty
			override val autoUpdated :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.empty
			override val insertable :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.empty
			override val autoInserted :Unique[ExpressionColumnMapping[F, S, X, O]] = Unique.empty

			override val form = column.readForm match {
				case rw :ColumnForm[X @unchecked] => rw

				case r => r <> ColumnWriteForm.unsupported(
					s"expression column $expr does not support write.", r.sqlType
				)
			}

			override val name :String = expr match {
				case _ if alias != null => alias

				case AliasedColumn(_, alias) => alias

				case UnboundParamSQL(param, _, _) => param.name //first as it would match the following pattern, too

				case TypedComponentSQL(_, MappingExtract(_, _, component)) =>
					component match {
						case column :ColumnMapping[_, _] => column.name
						case label @: _ => label
						case _ => component.sqlName getOrElse "result"
					}

				case SQLParameter(_, Some(name)) => name //unlikely to appear in this position

				case _ => "result" //todo: should it be an alias, or sql?
			}

		}



	def unapply[X, O](mapping :RefinedMapping[X, O]) :Option[(ColumnSQL[_, LocalScope, X], String)] =
		mapping match {
			case col :ExpressionColumnMapping[_, LocalScope @unchecked, X @unchecked, O @unchecked] =>
				Some((col.expr, col.name))
			case _ => None
		}



	type Expression[F <: RowProduct, S >: LocalScope <: GlobalScope, X] = {
		type Projection[O] = ExpressionColumnMapping[F, S, X, O]
	}

}
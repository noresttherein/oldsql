package net.noresttherein.oldsql.sql

import scala.annotation.nowarn
import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.{Chain, Listing, NaturalMap, Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{Bug, InseparableExpressionException, NoSuchComponentException}
import net.noresttherein.oldsql.morsels.generic.{=>:, Self}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.pixies.MockResultSet
import net.noresttherein.oldsql.schema.{Buffs, ColumnExtract, ColumnForm, ColumnMapping, ColumnReadForm, ColumnWriteForm, Mapping, MappingExtract, RelVar, SpecificExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{BuffType, ReadOnly}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnMappingTemplate, OptimizedColumnTemplate, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, MappingTemplate, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping, ExportMapping}
import net.noresttherein.oldsql.schema.bases.ExportMapping.ExportMappingTemplate
import net.noresttherein.oldsql.schema.bases.LazyMapping.LazyMappingTemplate
import net.noresttherein.oldsql.schema.bits.{IndexedColumnMapping, IndexedMapping, LabelPath}
import net.noresttherein.oldsql.schema.bits.IndexedMapping.GetListingComponent
import net.noresttherein.oldsql.schema.bits.LabeledMapping.@:
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.support.AliasedColumn
import net.noresttherein.oldsql.slang.mappingMethods
import net.noresttherein.oldsql.sql.ColumnSQL.CaseAnyColumn
import net.noresttherein.oldsql.sql.ParamClause.UnboundParamSQL
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, BaseAnyExpressionVisitor, CaseAnyExpression, Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{AdaptedColumnSQL, AdaptedSQL, AliasedSQL, BoundParam, ChainSQL, ComponentSQL, ConvertedSQL, DecoratedSQL, EditedComponentSQL, InlineSQL, LabeledSQL, MappingTerm, SelectIdSQL, SQLTerm}
import net.noresttherein.oldsql.sql.ast.ColumnMappingTerm.CaseAnyColumnMappingTerm
import net.noresttherein.oldsql.sql.ast.ColumnTerm.ErrorColumnTerm
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.LabeledSQL.{LabeledColumnSQL, LabeledItem, LabeledValueSQL, MatchAnyLabeled}
import net.noresttherein.oldsql.sql.ast.MappingTerm.CaseAnyMappingTerm
import net.noresttherein.oldsql.sql.mechanics.Equivalent






//todo: move to mechanics
/** A synthetic adapter of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[Domain, S, O]`
  * to a [[net.noresttherein.oldsql.schema.Mapping Mapping]]. Used in ''select'' and ''group by'' clauses,
  * so that [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]
  * can be homomorphic with [[net.noresttherein.oldsql.sql.Join Join]]s and to allow
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expressions to be used as
  * [[net.noresttherein.oldsql.schema.Relation Relation]]s in ''from'' clauses of other SQL selects.
  * This class is dedicated to non-component expressions; subclasses of
  * [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]] should be used directly for component expressions.
  * Not all possible expressions are supported; the expression may consist of
  *   - any single [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] (distinct SQL types),
  *     in particular [[net.noresttherein.oldsql.sql.ast.ColumnTerm terms]],
  *   - [[net.noresttherein.oldsql.sql.ast.ComponentSQL components]] (ranging from whole entities
  *     to single columns),
  *   - component expressions with certain columns substituted with arbitrary column expressions
  *     ([[net.noresttherein.oldsql.sql.ast.EditedComponentSQL EditedComponentSQL]]),
  *   - [[net.noresttherein.oldsql.sql.ast.AdaptedSQL conversion]] nodes for legal expressions,
  *   - SQL tuples (both [[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]]
  *     and [[net.noresttherein.oldsql.sql.ast.ChainSQL ChainSQL]]) of various Scala types,
  *   - [[net.noresttherein.oldsql.sql.ast.LabeledSQL LabeledSQL]] expressions use instead
  *     [[net.noresttherein.oldsql.sql.ListingSQLMapping ListingSQLMapping]], a subtype of this type,
  *   - all multi column expressions which support [[net.noresttherein.oldsql.sql.SQLExpression.split splitting]] -
  *     this in particular includes [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]]
  *     and [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]].
  *
  * Other types may appear as subexpressions, as long as they do not contribute columns to this mapping - for example,
  * as sides of a comparison. The expression is traversed recursively descending to leaf expressions:
  *   - all column expressions are used directly;
  *   - tuples and tuple-like values are inlined, with their columns (including those of their subexpressions)
  *   promoted to the columns of this mapping;
  *   - component expressions are treated as tuples of their columns, which are adapted to columns of this expression.
  *
  * Column names in this mapping may be unrelated to the original column names of mappings from component subexpressions
  * of `this.`[[net.noresttherein.oldsql.sql.SQLMapping.expr expr]]. They are guaranteed however to be unique,
  * valid identifiers and can be used as aliases given in ''as'' clauses inside a ''select'' clause. They are not
  * valid identifiers themselves, but should be treated as alias candidates for ''as'' clauses if this mapping
  * is used as a ''select'' clause of a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]], or for debugging
  * purposes. Note that, as column names of [[net.noresttherein.oldsql.schema.Table tables]] must be valid,
  * it means that, if a `SelectSQL` using this mapping is promoted to a table expression, the spelling process
  * must honor these names, using them as aliases for their corresponding columns, or make sure to track all references
  * to this mapping in order to guarantee correctness.
  *
  * The standard implementation of this mapping, created by its companion object,
  * doesn't contain any non-column components.
  *
  * @tparam S the scope of the expression: [[net.noresttherein.oldsql.sql.SQLExpression.Grouped local]] for
  *           expressions which can occur only as part of the most nested SQL select based on `F` in its
  *           ''group by'' or ''select'' clause, and [[net.noresttherein.oldsql.sql.SQLExpression.Single global]]
  *           for expressions which can occur anywhere in a SQL select from `F` or its dependent selects.
  * @tparam V the value type of this expression (typically some kind of a tuple-like type).
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this mapping.
  *
  * @see [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]
  * @see [[net.noresttherein.oldsql.sql.TypedSQLMapping TypedSQLMapping]]
  * @see [[net.noresttherein.oldsql.sql.ListingSQLMapping ListingSQLMapping]]
  * @author Marcin Mościcki
  */
//todo: add a factory method creating a mapping of a proper type to SQLExpression.
// This could be combined with defining member types for proper SelectSQL/Select subtype.
//todo: SQLMapping should be pseudo covariant in its subject; this will help in select clause/grouping
// and guarantee no one uses write operations. This in turn requires getting rid of the API dependency on BaseMapping
//consider: renaming to ExpressionMapping
trait SQLMapping[V, O]
	extends BaseMapping[V, O] with MappingTemplate[SQLMapping, ColumnSQLMapping]
{
	type Domain <: RowProduct
	type Scope >: Grouped <: Single
	val expr :SQLExpression[Domain, Scope, V]

	override def buffs :Buffs[V] = SQLMapping.defaultBuffs[V]

	override def writtenValues[T](op :WriteOperationView, subject :V) :ComponentValues[V, O] = ColumnValues.empty
	override def writtenValues[T](op :WriteOperationView, subject :V, collector :ComponentValuesBuilder[T, O]) :Unit =
		()

	def apply(column :Int) :ColumnSQLMapping[_, O] = columns(column)

	def indexOf(column :Column[_]) :Int = columns.indexOf(column)


	override def selectForm :SQLReadForm[V] = expr.selectForm

	protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[V] = SQLWriteForm.empty

	protected override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[V] =
		if (components.isEmpty)
			SQLWriteForm.empty
		else
			throw new IllegalArgumentException(s"Can't $op $components: expression mapping $expr does not support write.")


	override def homomorphic(other :Mapping) :Boolean = other match {
		case same if same eq this => true
		case e :SQLMapping[_, _] => expr compatible e.expr
		case _ => false
	}
	override def uniHomomorphic(that :Mapping) :Boolean = homomorphic(that)

	override def isomorphic(other :Mapping) :Boolean = other match {
		case same if same eq this => true //todo: a proper isomorphism implementation
		case e :SQLMapping[_, _] => expr compatible e.expr
		case _ => false
	}
	override def uniIsomorphic(that :Mapping) :Boolean = isomorphic(that)

	override def identical(other :Mapping) :Boolean = other match {
		case same if same eq this => true
		case e :SQLMapping[_, _] => expr identical e.expr
		case _ => false
	}

	override def mappingName :String = expr.toString
	override def toString :String = expr.toString
}




object SQLMapping {
	private val buffList = Buffs(ReadOnly[Unit])
	private[sql] def defaultBuffs[V] = buffList.asInstanceOf[Buffs[V]]

	def apply[F <: RowProduct, S >: Grouped <: Single, T, O]
	         (expression :SQLExpression[F, S, T], view :OperationView)
			:SQLMapping[T, O] { type Domain >: F <: RowProduct; type Scope >: S <: Single } =
		TypedSQLMapping(expression, view)

	def unapply[T](mapping :TypedMapping[T, _]) :Opt[SQLExpression[_, Grouped, T]] =
		mapping match {
			case expr :SQLMapping[T @unchecked, _] => Got(expr.expr)
			case _ => Lack
		}


	type SQLExtract[-X, Y, O] = SpecificExtract[SQLMapping[Y, O], X, Y, O]

	type __ = SQLMapping[_, _]

	type c[V] = {
		type apply[O]    = SQLMapping[V, O]
		type project[O] = SQLMapping[V, O]
	}
}






/** A synthetic adapter of an [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, S, O]` to a
  * [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]. Used in ''select'' and ''group by'' clauses,
  * so that [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]
  * can be homomorphic with [[net.noresttherein.oldsql.sql.Join Join]]s and to allow
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expressions to be used as
  * [[net.noresttherein.oldsql.schema.Relation Relation]]s in the ''from'' clauses of other SQL selects.
  *
  * The name of such a column is an identifier by which it can be referenced in SQL - needed currently
  * only for columns returned by a subselect. It corresponds to the alias given in SQL ''as'' clause,
  * and for selected table columns, it might be different than their original name. For mappings of columns
  * which are never referenced as values - any expressions appearing in the ''group by'' clause need to be
  * repeated verbatim in the ''select'' and ''having'' clauses if needed - it might be an empty `String` or some
  * other placeholder value, although this is the last resort if no 'natural' name for the expression exists.
  * These names are however guaranteed unique among instances being columns of a larger
  * [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]].
  * Note that the Scala object of this mapping ''can'' be freely reused: any references to it in the final SQL
  * will be rendered either by-name, or by-value, as listed above.
  *
  * @tparam V the value type of this expression.
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this mapping.
  * @see [[net.noresttherein.oldsql.sql.TypedColumnSQLMapping]]
  * @see [[net.noresttherein.oldsql.sql.ListingColumnSQLMapping]]
  */
trait ColumnSQLMapping[V, O]
	extends SQLMapping[V, O] with BaseColumn[V, O] with ColumnMappingTemplate[ColumnSQLMapping]
{
	override val expr :ColumnSQL[Domain, Scope, V]

	override def name :String = expr match {
		case AliasedSQL(_, alias) => alias
		case UnboundParamSQL(param, _, _) => param.name //first as it would match the following pattern, too
		case ComponentSQL(_, MappingExtract(_, _, component)) =>
			component match {
				case AliasedColumn(_ @unchecked, alias) => alias
				case column :ColumnMapping => column.name
				case label @: _ => label
				case _ => ""
			}
		case BoundParam(_, Some(name)) => name //unlikely to appear in this position
		case _ => ""
	}

	override def indexOf(column :Column[_]) :Int =
		if (column == this) 0
		else -1 //throw new NoSuchComponentException("Column " + column + " is not a part of column " + this + ".")

	override def apply(column :Int) :ColumnSQLMapping[_, O] =
		if (column != 0)
			throw new IndexOutOfBoundsException(
				column.toString + ": mapping for column expression " + this + " has only a single column."
			)
		else this

	//casting down a covariant type to invariant is wrong in general, but ColumnSQL is invariant and here we assume
	//  that the form is produced 'internally', rather than being an independent instance assigned to it
	override def form :ColumnForm[V] = expr.selectForm match {
		case rw :ColumnForm[V @unchecked] => rw
		case r => r <> ColumnWriteForm.unsupported(r.sqlType)(
			s"expression column $expr does not support write."
		)
	}

	override def selectForm :ColumnReadForm[V] = expr.selectForm

	override def toString :String = expr.toString + " as " + name
}




object ColumnSQLMapping {

	def apply[F <: RowProduct, S >: Grouped <: Single, T, O]
	         (expr :ColumnSQL[F, S, T], alias :String = null, buffs :Buffs[T] = SQLMapping.defaultBuffs[T])
			:ColumnSQLMapping[T, O] { type Domain >: F <: RowProduct; type Scope >: S <: Single } =
		TypedColumnSQLMapping(expr, alias, buffs)

	def unapply[T, O](mapping :MappingOf[T]) :Opt[(ColumnSQL[_, Grouped, T], String)] =
		mapping match {
			case col :ColumnSQLMapping[T @unchecked, O @unchecked] =>
				Got((col.expr, col.name))
			case _ => Lack
		}


	type ColumnSQLExtract[X, Y, O] = SpecificExtract[ColumnSQLMapping[Y, O], X, Y, O]

	type __ = ColumnSQLMapping[_, _]

	type c[V] = {
		type apply[O]    = ColumnSQLMapping[V, O]
		type project[O] = ColumnSQLMapping[V, O]
	}
}






/** An [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]] adapting
  * a [[net.noresttherein.oldsql.sql.ast.LabeledSQL LabeledSQL]] expression for use
  * in [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]] clauses
  * or as a ''select'' clause of a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] - and potentially
  * as a mapping of a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]]
  * to which it can be adapted.
  *
  * Every element of this tuple has an attached [[net.noresttherein.oldsql.schema.bits.LabelPath.Label Labell]]
  * uniquely identifying it within the tuple. As the only types of expressions allowed as elements of such a tuple
  * are columns and other `LabeledSQL` expressions, this means that every column of this mapping
  * has a unique [[net.noresttherein.oldsql.schema.bits.LabelPath LabelPath]] associated with it.
  * It can be used to access said column of this mapping, but they also add type safety when a `SelectSQL`
  * using this mapping is combined with another in a [[net.noresttherein.oldsql.sql.Select.SelectOperator set operations]],
  * or when matching the columns to columns of an existing
  * [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]].
  *
  * @tparam V the value type of this expression - either a [[net.noresttherein.oldsql.collection.Listing Listing]],
  *           or a distinct SQL type for [[net.noresttherein.oldsql.sql.ListingColumnSQLMapping column mappings]].
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this mapping.
  *
  * @see [[net.noresttherein.oldsql.sql.ListingColumnSQLMapping ListingColumnSQLMapping]]
  * @see [[net.noresttherein.oldsql.sql.TypedListingSQLMapping TypedListingSQLMapping]]
  */
trait ListingSQLMapping[V, O]
	extends SQLMapping[V, O] with IndexedMapping[V, O] with MappingTemplate[ListingSQLMapping, ListingSQLMapping.column]
{
	override val expr :LabeledValueSQL[Domain, Scope, V]

	override def apply[N <: Label](label :N)
	                              (implicit get :GetListingComponent[V, N]) :ListingSQLMapping[get.Value, O]

	override def apply[P](path :LabelPath[P])
	                     (implicit get :GetListingComponent[V, P]) :ListingSQLMapping[get.Value, O]

	override def columnNamed(name :String) :ListingColumnSQLMapping[_ <: Label, _, O] with Column[_] //scalac bug...
}



object ListingSQLMapping {

	//we don't need an OperationView because LabeledSQL cannot contain non-column components
	def apply[F <: RowProduct, S >: Grouped <: Single, T <: Listing, O](expr :LabeledSQL[F, S, T])
			:ListingSQLMapping[T, O] { type Domain >: F <: RowProduct; type Scope >: S <: Single } =
		TypedListingSQLMapping(expr)


	type ListingSQLExtract[-X, Y, O] = SpecificExtract[ListingSQLMapping[Y, O], X, Y, O]

	type __ = ListingSQLMapping[_, _]

	type c[V] = {
		type apply[O]    = ListingSQLMapping[V, O]
		type project[O] = ListingSQLMapping[V, O]
	}
	type column[V, O] = ListingColumnSQLMapping[_ <: Label, V, O]
}






/** A single-column [[net.noresttherein.oldsql.sql.ListingSQLMapping ListingSQLMapping]] associated
  * with a [[net.noresttherein.oldsql.schema.bits.LabelPath.Label Label]]. It doubles also as the type
  * of all columns of `ListingSQLMapping`s for whole [[net.noresttherein.oldsql.collection.Listing Listing]] values.
  *
  * @tparam V the value type of this column.
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this column.
  *
  * @see [[net.noresttherein.oldsql.sql.TypedListingSQLMapping TypedListingSQLMapping]]
  */
trait ListingColumnSQLMapping[N <: Label, V, O]
	extends ColumnSQLMapping[V, O] with ListingSQLMapping[V, O] with IndexedColumnMapping[V, O]
	   with ColumnMappingTemplate[ListingColumnSQLMapping.c[N]#Column]
{
	override type Origin = O //todo: make Mapping extend MappingPrototype and remove these overrides
	override type Subject = V
	override val expr :LabeledColumnSQL[Domain, Scope, N, V]

	override def apply[K <: Label](label :K)(implicit get :GetListingComponent[V, K]) :ListingSQLMapping[get.Value, O] =
		this.asInstanceOf[ListingSQLMapping[get.Value, O]] //these shouldn't be possible to call, but to guard against a future refactor

	override def apply[P](path :LabelPath[P])(implicit get :GetListingComponent[V, P]) :ListingSQLMapping[get.Value, O] =
		this.asInstanceOf[ListingSQLMapping[get.Value, O]]
}



object ListingColumnSQLMapping {

	def apply[F <: RowProduct, S >: Grouped <: Single, N <: Label, V, O]
	         (column :LabeledColumnSQL[F, S, N, V], buffs :Buffs[V] = SQLMapping.defaultBuffs[V])
			:ListingColumnSQLMapping[N, V, O] { type Domain >: F <: RowProduct; type Scope >: S <: Single } =
		TypedListingColumnSQLMapping(column, column.alias, buffs)

	def unapply[V](mapping :MappingOf[V]) :Opt[LabeledColumnSQL[_ <: RowProduct, Grouped, _ <: Label, V]] =
		mapping match {
			case indexed :ListingColumnSQLMapping[n, V @unchecked, _] =>
				Got(indexed.expr)
			case _ => Lack
		}


	type ListingColumnSQLExtract[N <: Label, X, Y, O] = SpecificExtract[ListingColumnSQLMapping[N, Y, O], X, Y, O]

	type __ = ListingColumnSQLMapping[_ <: Label, _, _]

	type c[N <: Label] = {
		type __ = ListingColumnSQLMapping[N, _, _]
		type apply[V, O]   = ListingColumnSQLMapping[N, V, O]
		type Column[V, O] = ListingColumnSQLMapping[N, V, O]
		type c[V] = {
			type __ = ListingColumnSQLMapping[N, V, _]
			type apply[O]    = ListingColumnSQLMapping[N, V, O]
			type project[O] = ListingColumnSQLMapping[N, V, O]
		}
	}
}






/** A specialization of [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]] defining a ''lower'' bound
  * for its [[net.noresttherein.oldsql.sql.TypedSQLMapping.Domain Domain]] type. Note that because
  * SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] are contravariant in the domain clause type,
  * this means that that a lower bound on `Domain` places an upper bound on the underlying
  * [[net.noresttherein.oldsql.sql.SQLMapping.expr expression]]. The column type of this mapping is also
  * narrowed down to [[net.noresttherein.oldsql.sql.TypedColumnSQLMapping TypedColumnSQLMapping]].
  *
  * The separation from `SQLMapping` is dictated by the need to preserve the mapping type when
  * a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]] clause is anchored
  * or otherwise converted to another clause type, which is impossible with clause type `F` listed
  * in this type's signature.
  * @tparam F the ''from'' clause serving as the domain of the adapted expression;
  * @tparam S the scope of the expression: [[net.noresttherein.oldsql.sql.SQLExpression.Grouped local]] for
  *           expressions which can occur only as part of the most nested SQL select based on `F` in its
  *           ''group by'' or ''select'' clause, and [[net.noresttherein.oldsql.sql.SQLExpression.Single global]]
  *           for expressions which can occur anywhere in a SQL select from `F` or its dependent selects.
  * @tparam V the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type of this expression -
  *           a [[net.noresttherein.oldsql.collection.Listing Listing]] subtype, or any distinct SQL type
  *           for individual columns.
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this mapping.
  * @see [[net.noresttherein.oldsql.sql.TypedColumnSQLMapping]]
  * @see [[net.noresttherein.oldsql.sql.TypedListingSQLMapping]]
  */ //consider: all 'Typed' subtypes could be replaced with type aliases, no?
trait TypedSQLMapping[-F <: RowProduct, -S >: Grouped <: Single, V, O] extends SQLMapping[V, O]
	with MappingTemplate[TypedSQLMapping.c[F]#c[S]#Mapping, TypedColumnSQLMapping.c[F]#c[S]#M]
{
	override type Domain >: F <: RowProduct
	override type Scope >: S <: Single

	override def apply(column :Int) :TypedColumnSQLMapping[F, S, _, O] = columns(column)
}






object TypedSQLMapping {
	//todo: make multi-column terms components
	def apply[F <: RowProduct, S >: Grouped <: Single, T, O]
	         (expression :SQLExpression[F, S, T], view :OperationView) :TypedSQLMapping[F, S, T, O] =
		expression match {
			case column :ColumnSQL[F @unchecked, S @unchecked, T @unchecked] =>
				TypedColumnSQLMapping(column)
			case _ =>
				new NonColumnSQLMapping(expression, view)
		}

	def unapply[T](mapping :TypedMapping[T, _]) :Opt[SQLExpression[_, Grouped, T]] =
		mapping match {
			case expr :TypedSQLMapping[_, Grouped @unchecked, T @unchecked, _] => Got(expr.expr)
			case _ => Lack
		}

	type TypedSQLExtract[F <: RowProduct, S >: Grouped <: Single, -X, Y, O] =
		SpecificExtract[TypedSQLMapping[F, S, Y, O], X, Y, O]

	type __ = TypedSQLMapping[_ <: RowProduct, _ >: Grouped <: Single, _, _]
	type GlobalTypedSQLMapping[-F <: RowProduct, V, O] = TypedSQLMapping[F, Single, V, O]
	type LocalTypedSQLMapping[-F <: RowProduct, V, O] = TypedSQLMapping[F, Grouped, V, O]

	//todo: we could use $ instead of c or apply; other good identifiers are t, T, C (looks like subset) and _1, _2, etc.
	type c[F <: RowProduct] = {
		type __ = TypedSQLMapping[F, _ >: Grouped <: Single, _, _]
		type apply[-S >: Grouped <: Single, V, O] = TypedSQLMapping[F, S, V, O]
		type c[S >: Grouped <: Single] = {
			type __ = TypedSQLMapping[F, S, _, _]
			type apply[V, O]   = TypedSQLMapping[F, S, V, O]
			type M[V, O]       = TypedSQLMapping[F, S, V, O]
			type Mapping[V, O] = TypedSQLMapping[F, S, V, O]
			type Column[V, O]  = TypedColumnSQLMapping[F, S, V, O]
			type c[V] = {
				type apply[O]   = TypedSQLMapping[F, S, V, O]
				type project[O] = TypedSQLMapping[F, S, V, O]
			}
		}
	}



	//todo: NamingConvention, NamingStrategy. They should abstract over text case and `` quotes, leaving these to SQLSpelling
	private class NonColumnSQLMapping[F <: RowProduct, S >: Grouped <: Single, X, O]
	                                 (override val expr :SQLExpression[F, S, X], val view :OperationView)
		extends TypedSQLMapping[F, S, X, O] with ExportMapping
		   with LazyMappingTemplate[c[F]#c[S]#Mapping, c[F]#c[S]#Column, X, O]
		   with ExportMappingTemplate[c[F]#c[S]#Mapping, c[F]#c[S]#Column]
	{ outer =>
		override type Domain = F
		override type Scope  = S

		type ExpressionComponent[V] = TypedSQLMapping[F, S, V, O]
		type ExpressionColumn[V] = TypedColumnSQLMapping[F, S, V, O]
		type SQLExtract[T] = SpecificExtract[TypedSQLMapping[F, S, T, O], X, T, O]
		type ColumnSQLExtract[T] = SpecificExtract[TypedColumnSQLMapping[F, S, T, O], X, T, O]

		private type Extractors[-_ >: Grouped <: Single, V] =
			Seq[Assoc[ExpressionColumn, Extractor.from[V]#to, _]]


		/** Traverses the `expr` AST, stopping recursion when a `ColumnSQL` is encountered.
		  * Returns a flat list of all found column expressions with their `Extract`s.
		  */
		private class ExtractsCollector extends AnyExpressionVisitor[F, Extractors]
			with CaseAnyExpression[F, Extractors] with CaseAnyMappingTerm[Extractors]
			with CaseAnyColumn[F, Extractors] with CaseAnyColumnMappingTerm[Extractors]
		{
			private[this] var names :Set[String] = Set("", "?") //used column names, disallowing "" and "?"
			private[this] var columnCount = 0

			@inline private def composeColumnExtractAssoc[W, P, T]
			                    (extractor : W =?> P)(entry :Assoc[ExpressionColumn, Extractor.from[P]#to, T])
					:Assoc[ExpressionColumn, Extractor.from[W]#to, T] =
				Assoc[ExpressionColumn, Extractor.from[W]#to, T](entry._1, entry._2 compose extractor)

			override def column[C >: Grouped <: Single, V](e :ColumnSQL[F, C, V]) :Extractors[C, V] = {
				val rightlyScoped = e.asInstanceOf[ColumnSQL[F, S, V]]
				val column = TypedColumnSQLMapping[F, S, V, O](rightlyScoped, nameFor(e), Buffs.empty)
				PassedArray.single(Assoc[ExpressionColumn, Extractor.from[V]#to, V](column, Extractor.ident))
			}

			override def alias[C >: Grouped <: Single, V](e :AliasedSQL[F, C, V]) :Extractors[C, V] =
				e.value match {
					case component :TypedColumnComponentSQL.__ @unchecked =>
						val buffs = component.anchored.buffs.asInstanceOf[Buffs[V]]
						val rightlyScoped = e.asInstanceOf[ColumnSQL[F, S, V]]
						val column = TypedColumnSQLMapping[F, S, V, O](rightlyScoped, nameFor(e), buffs)
						PassedArray.single(Assoc[ExpressionColumn, Extractor.from[V]#to, V](column, Extractor.ident))
					case _ =>
						column(e)
				}

			override def columnComponent[G >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
			                            (e :TypedColumnComponentSQL[G, T, E, M, V, L]) :Extractors[Single, V] =
			{
				val column = e.anchored.withOrigin[G]
				val export = TypedColumnSQLMapping[F, S, V, O](e.toColumnSQL, nameFor(e.toColumnSQL), column.buffs)
				PassedArray.single(Assoc[ExpressionColumn, Extractor.from[V]#to, V](export, Extractor.ident))
			}

			override def component[G >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
			                      (e :TypedComponentSQL[G, T, E, M, V, L]) :Extractors[Single, V] =
			{   //any changes must be reflected also in the AssemblerComposer to retain consistency
				val row = e.origin.anchored
				val component = e.anchored

				def extractAssoc[C](column :TypedColumn[C, G]) = {
					val expr = (e.origin \ column).toColumnSQL
					val export = TypedColumnSQLMapping[F, S, C, O](expr, nameFor(expr), column.buffs)
					Assoc[ExpressionColumn, Extractor.from[V]#to, C](export, component(column))
				}
				//we cannot simply filter columns of e.mapping because they may be in a different order
				// than in e.anchored, and we must use the exact same columns that e.origin.relation uses during spelling.
				//val columns = e.mapping.columns.view.filter(c => view.NonDefault.inactive(row.export(c)))
				val columns = e.origin.mapping.counterpartColumns(row, row.defaultColumns(view, component))
				columns.map(extractAssoc(_)) to PassedArray.iterableFactory
			}

			override def mappingTerm[M[A] <: BaseMapping[V, A], V](e :MappingTerm[M, V]) = {
				def extractAssoc[C](column :TypedColumn[C, e.Origin]) :Assoc[ExpressionColumn, Extractor.from[V]#to, _] = {
					val expr = (e \ column).toColumnSQL
					val export = TypedColumnSQLMapping[F, S, C, O](expr, nameFor(expr), column.buffs)
					Assoc[ExpressionColumn, Extractor.from[V]#to, C](export, e.export(column))
				}
				val columns = view.defaultColumns(e.export)
				columns.map(extractAssoc(_)) to PassedArray.iterableFactory
			}


			override def editedComponent[M[A] <: BaseMapping[V, A], V](e :EditedComponentSQL[F, M, V]) = {
				val row = e.component.origin.anchored
				val component = e.component.anchored

				//columns of e.component.mapping, not e.component.origin.anchored as we need extracts from the component
				val columns = e.mapping.columns.view.filter(c => view.NonDefault.inactive(row.export(c)))
				columns.map { column =>
					def extractAssoc[C](column :TypedColumn[C, e.Origin])
							:Assoc[ExpressionColumn, Extractor.from[V]#to, _] =
					{
						val default = e.component.origin \ column
						val name = nameFor(default)
						e.anchoredSubstitutes.get(default.anchored) match {
							case Some(setter :(ColumnSetter[e.FromLast, F] { type Subject = C }) @unchecked) =>
								val getter = component(column) andThen ((c :C) => setter.lvalue.conversion(c))
								//buffs.cascade would be safer
								//val buffs = column.buffs.bimap(setter.lvalue.lift(_), setter.lvalue.lift.lower)
								val buffs = default.anchored.buffs.unsafeBimap(
									Extractor.req(setter.lvalue.conversion(_:C)), Extractor.opt(setter.lvalue.conversion.unapply)
								)
								val export = TypedColumnSQLMapping[F, S, setter.Value, O](setter.rvalue, name, buffs)
								Assoc[ExpressionColumn, Extractor.from[V]#to, setter.Value](export, getter)
							case _ =>
								val export = TypedColumnSQLMapping[F, S, C, O](default, name, default.anchored.buffs)
								Assoc[ExpressionColumn, Extractor.from[V]#to, C](export, component(column))
						}
					}
					extractAssoc(column)
				} to PassedArray.iterableFactory
			}

			//we could instead use all decorators, but their convert is protected and we need it in AssemblerComposer
//			override def adapted[C >: Grouped <: Single, T, U](e :AdaptedSQL[F, C, T, U]) = {
//				val extracts = apply(e.value)
//				extracts.map(composeColumnExtractAssoc(Extractor.none :U =?> T)(_))
//			}

			override def decorated[C >: Grouped <: Single, V](e :DecoratedSQL[F, C, V]) :Extractors[C, V] =
				apply(e.value)

			override def adapted[C >: Grouped <: Single, T, U](e :AdaptedSQL[F, C, T, U]) = {
				val extracts = apply(e.value)
				val extractor =
					if (e.adaptation.isReversible) Extractor.req(e.adaptation.inverse)
					else Extractor(e.adaptation.unapply(_ :U))
				e.adaptation match {
					case bi :Equivalent[T, U] => Extractor.req(bi.inverse)
					case lift => Extractor(lift.unapply)
				}
				extracts.map(composeColumnExtractAssoc(extractor)(_))
			}

			override def selectId[C >: Grouped <: Single, V](e :SelectIdSQL[F, C, V]) :Extractors[S, V] = {
				val idColumn = TypedColumnSQLMapping[F, S, String, O](e.idColumn, nameFor(e.idColumn), Buffs.empty)
				Assoc[ExpressionColumn, Extractor.from[V]#to, String](idColumn, Extractor.none) +: apply(e.selectClause)
			}

			override def inline[C >: Grouped <: Single, T](e :InlineSQL[F, C, T]) :Extractors[C, T] =
				e.extracts.flatMap { entry :Assoc[SQLExpression.from[F]#rows[C]#E, Extractor.from[T]#to, _] =>
					def map[A](entry :Assoc[SQLExpression.from[F]#rows[C]#E, Extractor.from[T]#to, A]) :Extractors[C, T] =
						apply(entry._1).map(composeColumnExtractAssoc(entry._2)(_))
					map(entry)
				} //to Seq.iterableFactory

			//this is not a tuple!
			override def chain[C >: Grouped <: Single, I <: Chain, L](e :ChainSQL[F, C, I, L]) =
				(apply(e.init).view.map(composeColumnExtractAssoc(Chain.init[I] :(I ~L) => I)(_)) :++
					apply(e.last).view.map(composeColumnExtractAssoc(Chain.last[L] :(I ~ L) => L)(_))
				) to PassedArray.iterableFactory


			//we can make this work only if we are sure no one is actually using column.expr, but always depend on this.expr
			override def term[T](e :SQLTerm[T]) :Extractors[Single, T] = {
				try { expression(e) } catch {
					case _ :UnsupportedOperationException | _ :InseparableExpressionException =>
						(0 until e.selectForm.columnCount).map { i =>
							val termName = indexedName("term")
							val columnName = indexedName(termName + "_")
							val column = TypedColumnSQLMapping[RowProduct, S, Null, O](
								ErrorColumnTerm[Null]("Column #" + i + " of a non splittable term " + e),
								columnName
							)
							val extract = SpecificExtract.none(column)
							Assoc[ExpressionColumn, Extractor.from[T]#to, Null](column, extract)
						}
				}
			}

			//Technically, we don't even need the individual column expressions and could (almost) just as well
			// create mock columns in the right number, but split will validate that an expression can be inlined.
			override def expression[C >: Grouped <: Single, T](e :SQLExpression[F, C, T]) =
				try { //fixme: this can cause discrepancies with spelling
					StandardSQL.spelling.in(view.spellingScope).split(e).flatMap {
						col => apply(col).view.map(composeColumnExtractAssoc(Extractor.none :T =?> Nothing)(_))
					}
				} catch {
					case e :InseparableExpressionException =>
						throw e.addInfo("Failed to create a synthetic mapping for expression " + expr + ".")
					case e :UnsupportedOperationException =>
						throw new UnsupportedOperationException(
							"Failed to create a synthetic mapping for expression " + expr + ": " + e.getMessage
						)
				}

			override def unhandled(e :SQLExpression[F, _, _]) :Nothing =
				throw new IllegalArgumentException(
					s"SQLExpression $e cannot be used in a SelectSQL select clause expression (as part of $outer)."
				)


			private def indexedName(name :String) :String = {
				var i = 1; var alias = name + "1"
				while (names(alias)) {
					i += 1; alias = name + i
				}
				names += alias
				alias
			}
			private def uniqueNameLike(name :String) :String =
				if (!names(name)) {
					names += name; name
				} else
					indexedName(name)

			private def nameFor(e :ColumnSQL[F, Grouped, _]) :String = {
				columnCount += 1
				val name :String = e match {
					case AliasedSQL(_, alias) => uniqueNameLike(alias)
					case UnboundParamSQL(param, extract, idx) => //first as it would match the following pattern, too
						val sanitized =
							if (param.name.contains('?')) param.name.replace("?", "")
							else param.name
						val paramName = if (sanitized.length == 0) "p" + idx else sanitized
						if (extract.isIdentity)
							if (!names(paramName)) paramName else indexedName(paramName + "_")
						else { //todo: use the extract name
							indexedName(paramName + "_")
						}
					case TypedColumnComponentSQL(origin, ColumnExtract(_, _, column)) =>
						val actual = origin.anchored.withOrigin[Unit].export(column.withOrigin[Unit])
						val name :String = actual match {
							case AliasedColumn(_, alias) => alias
							//todo: incorporate the extract name
							case _ => origin.relation match { //the almost certain case
								case table :RelVar[MappingAt] => table.name + "_" + column.name
								case _ => column.name
							}
						}
						uniqueNameLike(name)

					case AdaptedColumnSQL(col) => nameFor(col)

//					case col :AdaptedColumnSQL[F, Grouped, _, _] => nameFor(col.value)
//					case col :UnaryCompositeColumn[F, Grouped, _, _] => nameFor(col.value)

					case BoundParam(_, Some(name)) => //unlikely to appear in this position
						uniqueNameLike(name)

					case _ => indexedName("_")
				}
//				names += name
				names += "_" + columnCount //indexedName("_") will default to column number
				name
			}
		}


		private type Assembler[-_ >: Grouped <: Single, T] = Pieces => Option[T]

		/** Visitor traversing the represented expression and composing a function used for implementation
		  * of the `assemble` method. It depends on the `columns` property of the outer mapping and assumes
		  * they are in the order in which they will be visited by this instance. Every case must thus mirror
		  * the corresponding case in `ExtractsCollector`, dropping the same number of columns that was added
		  * by that visitor.
		  */
		private class AssemblerComposer extends AnyExpressionVisitor[F, Assembler]
			with CaseAnyExpression[F, Assembler] with CaseAnyMappingTerm[Assembler]
			with CaseAnyColumn[F, Assembler] with CaseAnyColumnMappingTerm[Assembler]
		{
			/** The stack is in the exact order of individual column appearance, as returned by `ExtractsCollector`. */
			private[this] var columnStack :List[TypedColumnSQLMapping[F, S, _, O]] = columns.toList

			override def column[C >: Grouped <: Single, V](e :ColumnSQL[F, C, V]) :Pieces => Option[V] = {
				val column = columnStack.head.asInstanceOf[ExpressionColumn[V]]
				columnStack = columnStack.tail
				pieces => pieces.get(column).toOption
			}

			override def component[A >: F <: RowProduct, T[B] <: BaseMapping[E, B], E, M[B] <: BaseMapping[V, B], V, L <: RowProduct]
			                      (e :TypedComponentSQL[A, T, E, M, V, L]) =
			{   //chosen column set must be exactly the same as in ExtractsCollector
				val component = e.anchored
				val exported = e.origin.anchored.defaultColumns(view, component)
				val count = exported.size
				val (selected, tail) = columnStack.splitAt(count)
				columnStack = tail
				componentAssembler(component, exported, selected)
			}


			override def mappingTerm[M[A] <: BaseMapping[V, A], V](e :MappingTerm[M, V]) = {
				//chosen column set must be exactly the same as in ExtractsCollector
				val comp = e.export
				val exported = comp.defaultColumns(view)
				val count = exported.size
				val (selected, tail) = columnStack.splitAt(count)
				columnStack = tail
				componentAssembler(comp, exported, selected)
			}

			private def componentAssembler[V, A](comp :TypedMapping[V, A], columns :Unique[TypedColumn[_, A]],
			                                     sqlColumns :Seq[TypedColumnSQLMapping[F, S, _, O]]) =
			{
				//map selected export columns of component to columns of this at positions in this.columns consistent
				// with ExtractsCollector's output
				def expressionColumn[C](_1 :TypedColumn[_, comp.Origin], _2 :TypedColumnSQLMapping[F, S, C, O]) = {
					assert(_1.form == _2.form,
							"Column's " + _1.toString + " (of " + comp + ") type " + _1.form +
							" does not match the type of corresponding column " + _2 + " of " + this + ": " +
							_2.form + ". This is an inconsistency bug in traversing expression " + expr + "."
					)
					Assoc[comp.Component, ExpressionColumn, C](_1.asInstanceOf[comp.Column[C]], _2)
				}
				val exportAliasing = NaturalMap.from(columns.view.zipMap(sqlColumns) {
					(column, sqlColumn) => expressionColumn(column, sqlColumn)
				})

				//map all columns of component to extracts of corresponding columns of this, if they exist.
				def selectedColumn[C](extract :Assoc[comp.Column, comp.ColumnExtract, C]) =
					exportAliasing.getOrElse(extract._2.export, null :ExpressionColumn[C]) match {
						case null => None
						case col => Some(
							Assoc[comp.Component, ExpressionColumn, C](extract._1, columnExtracts(col).export)
						)
					}
				val aliasing = comp.columnExtracts.flatMap(selectedColumn(_))

				{ pieces :Pieces =>
					val values = ColumnValues[V, comp.Origin](new (MappingAt[comp.Origin]#Column =>: Self) {
						override def apply[C](x :TypedColumn[C, comp.Origin]) :C =
							aliasing.getOrElse[ExpressionColumn, C](x, null) match {
								case null => null.asInstanceOf[C]
								case column => pieces.get(column) match {
									case Got(res) => res
									case _ => null.asInstanceOf[C]
								}
							}
						})
					comp.optionally(values).toOption
				}
			}


			override def editedComponent[M[A] <: BaseMapping[V, A], V](e :EditedComponentSQL[F, M, V]) = {
				val row = e.component.origin.anchored
				val component = e.component.anchored
				val columns = //a bit of a roundabout way in order to use the exact same code as ExtractCollector
					e.component.mapping.columns.view.map(row.export(_)).filter(view.NonDefault.inactive) to PassedArray
//				val exported = e.component.origin.anchored.defaultColumns(view, component)
				val count = columns.size
				val (selected, tail) = columnStack.splitAt(count)
				columnStack = tail

				type Getter[T] = Pieces => T

				//map default export columns of row to getters selecting the values of the columns of this mapping
				// representing them
				val anchoredGetters = columns.view.zip(selected).map { case (column, sqlColumn) =>
					def anchoredGetter[A, B](_1 :TypedColumn[A, e.Origin], _2 :TypedColumnSQLMapping[F, S, B, O])
							:Assoc[row.Column, Getter, _] =
					{
						e.anchoredSubstitutes.get(_1) match {
							case Some(setter) => //column substituted for an expression, possibly of a different type B
								val lift =
									setter.asInstanceOf[
										ColumnSetter[e.FromLast, F] { type Subject = A; type Value = B }
									].lvalue.conversion
								val getter = (pieces :Pieces) =>
									pieces.get(_2) match {
										case Got(b) => lift.unapply(b) match {
											case Got(a) => a
											case _ => null.asInstanceOf[A]
										}
										case _ => null.asInstanceOf[A]
									}
								Assoc[row.Column, Getter, A](_1, getter)
							case _ if _1.form == _2.form => //no substitution for column _1, meaning _2.expr is a ColumnMappingSQL for _1
								val getter = (pieces :Pieces) =>
									(pieces.get(_2) match {
										case Got(b) => b
										case _ => null
									}).asInstanceOf[A]
								Assoc[row.Column, Getter, A](_1, getter)
							case _ =>
								throw Bug(
									"Column's " + _1.toString + " (of " + component + ") type " + _1.form +
										" does not match the type of corresponding column " + _2 + " of " + this + ": " +
										_2.form + ". This is an inconsistency bug in traversing expression " + expr + "."
								)
						}
					}
					anchoredGetter(column, sqlColumn)
				} to NaturalMap

				//map all columns of component to extracts of corresponding columns of this, if they exist.
				val columnGetters = component.columnExtracts.flatMap { assoc =>
					def columnGetter[C](extract :Assoc[component.Column, component.ColumnExtract, C]) = {
						val export = row.export(extract._1)
						anchoredGetters.get(export).map(
							getter => Assoc[component.Column, Getter, C](extract._1, getter)
						)
					}
					columnGetter(assoc)
				} withDefault new (component.Column =>: Getter) {
					override def apply[C](key :component.Column[C]) = (_ :Pieces) => null.asInstanceOf[C]
				}

				{ pieces :Pieces =>
					val values = ColumnValues[V, e.Origin](new (MappingAt[e.Origin]#Column =>: Self) {
						override def apply[C](x :TypedColumn[C, e.Origin]) :C = columnGetters(x)(pieces)
					})
					component.optionally(values).toOption
				}
			}


			override def adapted[C >: Grouped <: Single, T, U](e :AdaptedSQL[F, C, T, U]) = {
				val base = apply(e.value) //important to have this as a constant
				pieces => base(pieces).map(e.adaptation)
			}

			override def decorated[C >: Grouped <: Single, V](e :DecoratedSQL[F, C, V]) :Assembler[C, V] = {
				val base = apply(e.value)
				pieces => base(pieces)
			}


			override def selectId[C >: Grouped <: Single, V](e :SelectIdSQL[F, C, V]) :Assembler[C, V] = {
				columnStack = columnStack.tail
				apply(e.selectClause)
			}

			//in Scala 3, it would be good to provide direct, faster implementation for proper tuple types
			override def inline[C >: Grouped <: Single, T](e :InlineSQL[F, C, T]) = {
				val getters = e.items.view.map { item :e.Item[F, C, _] =>
					def assembler[V](item :e.Item[F, C, V]) =
						Assoc[({ type T[A] = e.Item[F, C, A] })#T, ({ type T[A] = Pieces => Option[A] })#T, V](
							item, apply(item.value)
						)
					assembler(item)
				} to NaturalMap

				pieces =>
					val getter = new (({ type T[A] = e.Item[F, C, A] })#T =>: Self) {
						override def apply[A](x :e.Item[F, C, A]) = getters(x)(pieces) match {
							case Some(a) => a
							case _ => throw new NoSuchElementException
						}
					}
					try {
						Some(e.construct(getter))
					} catch { case _ :NoSuchElementException => None }
			}
			//not a tuple!
			override def chain[C >: Grouped <: Single, I <: Chain, L](e :ChainSQL[F, C, I, L]) = {
				val init = apply(e.init)
				val last = apply(e.last)
				pieces :Pieces => for (i <- init(pieces); l <- last(pieces)) yield i ~ l
			}

			override def expression[C >: Grouped <: Single, T](e :SQLExpression[F, C, T]) = {
				val form = e.selectForm
				val (expressionColumns, rest) = columnStack.splitAt(form.columnCount)
				columnStack = rest
				//these are not column labels, so its technically wrong, but likely column names in the select
				// are the same and we don't use them anyway.
				val columnNames = expressionColumns.view.map(_.name).zipWithIndex.toMap
				val getters = expressionColumns.view.map { col => (pieces :Pieces) => pieces(col) } to ArraySeq
				val mockResultSet = (pieces :Pieces) => new MockResultSet(getters.map(_(pieces)), columnNames)

				pieces => form.opt(mockResultSet(pieces), 0)
			}
		}


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[X] =
			if (columns == components)
				selectForm
			else //consider: theoretically, we could allow components to contain columns out of order, but is it worth it?
				throw new IllegalArgumentException(
					"SQLMapping offers selectForm only for the full column set.\n" +
						s"Asked for: $components;\nhave    : $columns."
				)


		override lazy val (extracts :NaturalMap[Component, SQLExtract],
		                   columnExtracts :NaturalMap[Column, ColumnSQLExtract],
		                   columns) =
		{
			val extractors = (new ExtractsCollector)(expr)
			val extracts = extractors.view.map { entry =>
				def f[T](entry :Assoc[ExpressionColumn, Extractor.from[X]#to, T]) = {
					val column = entry._1; val extract = entry._2
					val buffs = column.buffs +/: this.buffs.unsafeCascade(extract)
					val buffed = TypedColumnSQLMapping[F, S, T, O](column.expr, column.name, buffs)
					Assoc[ExpressionColumn, ColumnSQLExtract, T](buffed, SpecificExtract(buffed)(extract))
				}
				f(entry)
			} to ArraySeq
//			val ofComponents = NaturalMap.from(extracts :Iterable[Assoc[Component, SQLExtract, _]])
//			val ofColumns = ofComponents.asInstanceOf[NaturalMap[Column, ColumnSQLExtract]]
			val ofColumns = NaturalMap.from(extracts :Iterable[Assoc[Column, ColumnSQLExtract, _]])
			val ofComponents = ofColumns.asInstanceOf[NaturalMap[Component, SQLExtract]]
			val columns = Unique.from[TypedColumnSQLMapping[F, S, _, O]](extracts.view.map(_._1))
			(ofComponents, ofColumns, columns)
		}

		override def components :Unique[TypedSQLMapping[F, S, _, O]] = columns

		override def columnsWith(buff :BuffType) :Unique[ExpressionColumn[_]] = columnsWith(this, buff)
		override def columnsWithout(buff :BuffType) :Unique[ExpressionColumn[_]] = columnsWithout(this, buff)

		override def columnsWith(component :Component[_], buff :BuffType) :Unique[TypedColumnSQLMapping[F, S, _, O]] = {
			val all = export(component).columns
			all.count(buff.active) match {
				case 0 => Unique.empty
				case n if n == all.size => all
				case _ => all.filter(buff.active)
			}
		}
		override def columnsWithout(component :Component[_], buff :BuffType) :Unique[TypedColumnSQLMapping[F, S, _, O]] = {
			val all = export(component).columns
			all.count(buff.inactive) match {
				case 0 => Unique.empty
				case n if n == all.size => all
				case _ => all.filter(buff.inactive)
			}
		}


		private lazy val assembler = (new AssemblerComposer)(expr) //depends on columns, so must be after them

		override def assemble(pieces: Pieces): Opt[X] = assembler(pieces)


		override def export[T](component :Component[T]) :TypedSQLMapping[F, S, T, O] = component match {
			case comp :TypedSQLMapping[F @unchecked, S @unchecked, T @unchecked, O @unchecked] => comp
			case _ =>
				throw new IllegalArgumentException(
					s"Mapping $component of ${component.getClass} passed as a component of $this is not an SQLMapping!"
				)
		}

		override def export[T](column :Column[T]) :TypedColumnSQLMapping[F, S, T, O] = column match {
			case comp :TypedColumnSQLMapping[F @unchecked, S @unchecked, T @unchecked, O @unchecked] => comp
			case _ =>
				throw new IllegalArgumentException(
					s"Mapping $column of ${column.getClass} passed as a column of $this is not a ColumnSQLMapping!"
				)
		}


		override def mappingName = "SQL"
		override def toString :String = expr.toString
	}

}






/** A specialization of [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]], exposing as a mapping
  * an arbitrary column expression [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, S, V]`, which
  * defines a lower bound `F` on the [[net.noresttherein.oldsql.sql.TypedSQLMapping.Domain Domain]] clause
  * of the wrapped [[net.noresttherein.oldsql.sql.ColumnSQLMapping.expr expression]].
  * @tparam F the ''from'' clause serving as the domain of the adapted expression;
  * @tparam S the scope of the expression: [[net.noresttherein.oldsql.sql.SQLExpression.Grouped local]] for
  *           expressions which can occur only as part of the most nested SQL select based on `F` in its
  *           ''group by'' or ''select'' clause, and [[net.noresttherein.oldsql.sql.SQLExpression.Single global]]
  *           for expressions which can occur anywhere in a SQL select from `F` or its dependent selects.
  * @tparam V the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type of this column.
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this mapping.
  */
trait TypedColumnSQLMapping[-F <: RowProduct, -S >: Grouped <: Single, V, O]
	extends TypedSQLMapping[F, S, V, O] with ColumnSQLMapping[V, O]
	   with ColumnMappingTemplate[TypedSQLMapping.c[F]#c[S]#Column]
{
	override def apply(column :Int) :TypedColumnSQLMapping[F, S, _, O] =
		if (column != 0)
			throw new IndexOutOfBoundsException(
				column.toString + ": mapping for column expression " + this + " has only a single column."
			)
		else this
}




object TypedColumnSQLMapping {

	def apply[F <: RowProduct, S >: Grouped <: Single, T, O]
	         (column :ColumnSQL[F, S, T], alias :String = null, buffs :Buffs[T] = SQLMapping.defaultBuffs[T])
			:TypedColumnSQLMapping[F, S, T, O] =
		new StandardColumnSQLMapping[F, S, T, O](column, buffs, alias)

	def unapply[T, O](mapping :MappingOf[T]) :Opt[(ColumnSQL[_, Grouped, T], String)] =
		mapping match {
			case col :TypedColumnSQLMapping[_, Grouped @unchecked, T @unchecked, O @unchecked] =>
				Got((col.expr, col.name))
			case _ => Lack
		}


	type TypedColumnSQLExtract[F <: RowProduct, S >: Grouped <: Single, X, Y, O] =
		SpecificExtract[TypedColumnSQLMapping[F, S, Y, O], X, Y, O]

	type __ = TypedColumnSQLMapping[Nothing, Grouped, _, _]
	type GlobalTypedSQLColumn[-F <: RowProduct, S, O] = TypedColumnSQLMapping[F, Single, S, O]
	type LocalTypedSQLColumn[-F <: RowProduct, S, O]  = TypedColumnSQLMapping[F, Grouped, S, O]

	type c[F <: RowProduct] = {
		type __ = TypedColumnSQLMapping[F, _ >: Grouped <: Single, _, _]
		type apply[S >: Grouped <: Single, V, O] = TypedColumnSQLMapping[F, S, V, O]
		type c[S >: Grouped <: Single] = {
			type __            = TypedColumnSQLMapping[F, S, _, _]
			type apply[V, O]   = TypedColumnSQLMapping[F, S, V, O]
			type M[V, O]       = TypedColumnSQLMapping[F, S, V, O]
			type Mapping[V, O] = TypedColumnSQLMapping[F, S, V, O]
			type c[V] = {
				type __ = TypedColumnSQLMapping[F, S, V, _]
				type apply[O]   = TypedColumnSQLMapping[F, S, V, O]
				type project[O] = TypedColumnSQLMapping[F, S, V, O]
			}
		}
	}

	private class StandardColumnSQLMapping[F <: RowProduct, S >: Grouped <: Single, T, O]
	              (override val expr :ColumnSQL[F, S, T], override val buffs :Buffs[T], alias :String = null)
		extends TypedColumnSQLMapping[F, S, T, O] with OptimizedColumnTemplate[TypedSQLMapping.c[F]#c[S]#Column, T, O]
	{
		override type Domain = F
		override type Scope  = S
		override val name :String = if (alias != null) alias else super.name
		override val form = super.form
		override val selectForm :ColumnReadForm[T] = expr.selectForm
		override val extracts = super.extracts
		override val columnExtracts = super.columnExtracts
	}
}






trait TypedListingSQLMapping[-F <: RowProduct, -S >: Grouped <: Single, V, O]
	extends TypedSQLMapping[F, S, V, O] with ListingSQLMapping[V, O]
	   with MappingTemplate[TypedListingSQLMapping.c[F]#c[S]#Mapping, TypedListingSQLMapping.c[F]#c[S]#Column]
{
	override def apply[N <: Label](label :N)
	                              (implicit get :GetListingComponent[V, N]) :TypedListingSQLMapping[F, S, get.Value, O]

	override def apply[P](path :LabelPath[P])
	                     (implicit get :GetListingComponent[V, P]) :TypedListingSQLMapping[F, S, get.Value, O]

	override def columnNamed(name :String) :TypedListingColumnSQLMapping[F, S, _ <: Label, _, O] with Column[_] //scalac bug...
}



object TypedListingSQLMapping {

	def apply[F <: RowProduct, S >: Grouped <: Single, T <: Listing, O](expr :LabeledSQL[F, S, T])
			:TypedListingSQLMapping[F, S, T, O] =
		new ListingTupleMapping(expr)

	def apply[F <: RowProduct, S >: Grouped <: Single, T <: Listing, O]
	         (expr :LabeledSQL[F, S, T], columnPrefix :String) :TypedListingSQLMapping[F, S, T, O] =
		new ListingTupleMapping(columnPrefix, expr)


	type TypedListingSQLExtract[-F <: RowProduct, -S >: Grouped <: Single, -X, Y, O] =
		SpecificExtract[TypedListingSQLMapping[F, S, Y, O], X, Y, O]

	type TypedListingColumnSQLExtract[-F <: RowProduct, -S >: Grouped <: Single, N <: Label, -X, Y, O] =
		SpecificExtract[TypedListingColumnSQLMapping[F, S, N, Y, O], X, Y, O]

	type c[F <: RowProduct] = {
		type __ = TypedListingSQLMapping[F, _ >: Grouped <: Single, _, _]
		type apply[-S >: Grouped <: Single, V, O] = TypedListingSQLMapping[F, S, V, O]

		type c[S >: Grouped <: Single] = {
			type __ = TypedListingSQLMapping[F, S, _, _]
			type apply[T, O]   = TypedListingSQLMapping[F, S, T, O]
			type M[T, O]       = TypedListingSQLMapping[F, S, T, O]
			type Mapping[T, O] = TypedListingSQLMapping[F, S, T, O]
			type Column[T, O ] = TypedListingColumnSQLMapping[F, S, _ <: Label, T, O]

			type c[V] = {
				type __ = TypedListingSQLMapping[F, S, V, _]
				type apply[O]   = TypedListingSQLMapping[F, S, V, O]
				type project[O] = TypedListingSQLMapping[F, S, V, O]
			}
		}
	}



	//todo: NamingConvention, NamingStrategy
	private class ListingTupleMapping[F <: RowProduct, S >: Grouped <: Single, V <: Listing, O]
	                                 (columnPrefix :String, override val expr :LabeledSQL[F, S, V])
		extends TypedListingSQLMapping[F, S, V, O]
		   with ExportMapping with ExportMappingTemplate[c[F]#c[S]#Mapping, c[F]#c[S]#Column]
	{ outer =>
		def this(expr :LabeledSQL[F, S, V]) = this("", expr)

		override type Domain = F
		override type Scope  = S
		type Item[T] = LabeledItem[F, Grouped, _ <: Label, T]
		type Expression[T] = SQLExpression[F, Grouped, T]
		type ExpressionComponent[T] = TypedListingSQLMapping[F, S, T, O]
		type ExpressionColumn[N <: Label, T] = TypedListingColumnSQLMapping[F, S, N, T, O]
		type SQLExtract[T] = SpecificExtract[TypedListingSQLMapping[F, S, T, O], V, T, O]
		type ColumnSQLExtract[T] = SpecificExtract[TypedListingColumnSQLMapping[F, S, _ <: Label, T, O], V, T, O]


		/** Maps SQL expressions to `ListingSQLMapping` instances wrapping them. */
		private[this] val subexpressions = {
			@inline def itemToComponent[K <: Label, T](e :LabeledItem[F, S, K, T]) = {
				val prefix = if (columnPrefix.length == 0) e.key else columnPrefix + "_" + e.key
				Assoc[Item, ExpressionComponent, T](e, e.value.mapping[O](prefix))
			}
			expr.items.view.map(itemToComponent(_ :LabeledItem[F, S, _ <: Label, _])) to NaturalMap
		}
		private[this] val index = subexpressions.view.map { case Assoc(item, comp) => (item.key :String, comp) }.toMap
		if (index.size != subexpressions.size)
			throw new IllegalArgumentException(
				subexpressions.groupBy(_._1.key).view.filter(_._2.sizeIs > 1).keys.mkString(
					"Listing expression " + expr + " contains duplicate keys: ", ", ", "."
				)
			)

		private def at(path :Seq[String]) :TypedListingSQLMapping[F, S, _, O] = path match {
			case hd +: tail =>
				if (tail.isEmpty) index(hd)
				else index(hd).asInstanceOf[ListingTupleMapping[F, S, _, O]].at(tail)
			case _ =>
				this
		}

		private type Assembler[-_ >: Grouped <: Single, T] = Pieces => Opt[T]

		private class AssemblerComposer
			extends BaseAnyExpressionVisitor[F, Assembler] with MatchAnyLabeled[F, Assembler]
		{
			override def labeledItem[C >: Grouped <: Single, I <: Listing, K <: Label, L]
			             (e :LabeledSQL[F, C, I |~ (K :~ L)]) :Assembler[C, I |~ (K :~ L)] =
			{
				val tl = apply(e.init)
				val hd = subexpressions(e.lastItem)
				pieces => for (t <- tl(pieces); h <- pieces.get(hd)) yield t |~ :~[K](h)
			}
			override def emptyLabeled = { val res = Got(@~); _ => res }
		}
		private[this] val assembler = (new AssemblerComposer)(expr)

		override def assemble(pieces :Pieces) = assembler(pieces)



		private type Extracts[-C >: Grouped <: Single, T] = List[TypedListingSQLExtract[F, C, T, _, O]]

		private class ComponentsCollector
			extends BaseAnyExpressionVisitor[F, Extracts] with MatchAnyLabeled[F, Extracts]
		{
			override def labeledItem[C >: Grouped <: Single, I <: Listing, K <: Label, L]
			             (e :LabeledSQL[F, C, I |~ (K :~ L)]) =
			{
				val extracts = apply(e.init).map(_ compose Chain.init[I] _) :Extracts[C, I |~ (K :~ L)]
				val last = subexpressions(e.lastItem).asInstanceOf[TypedListingSQLMapping[F, C, L, O]]
				val extract = SpecificExtract.req(last) {
					(_ :(I |~ (K :~ L))).last.value
				}
				extract::extracts
			}
			override def emptyLabeled = Nil
		}

		override val (components, subcomponents, extracts) = {
			val collected = (new ComponentsCollector)(expr).reverse
			@inline def extractEntry[T](extract :TypedListingSQLExtract[F, S, V, T, O]) =
				Assoc[Component, SQLExtract, T](extract.export, extract)

			@nowarn type SubExtract[X] = { type E[Y] = SpecificExtract[TypedListingSQLMapping[F, S, Y, O], X, Y, O] }
			@inline def compose[X, Y](extract :TypedListingSQLExtract[F, S, V, X, O])
			                         (subcomponent :Assoc[Component, SubExtract[X]#E, Y]) =
				Assoc[Component, SQLExtract, Y](subcomponent._1, subcomponent._2 compose extract)

			def composeExtracts[T](extract :TypedListingSQLExtract[F, S, V, T, O]) =
				extract.export.extracts.map(compose(extract)(_))

			val all = collected.flatMap { composeExtracts(_) } ++
				collected.map(extractEntry(_))
			val map = NaturalMap(all :_*)
			val comps = collected.view.map(_.export) to Unique
			val subs = collected.view.flatMap { e => e.export +: e.export.subcomponents } to Unique
			(comps, subs, map)
		}

		override val columnExtracts = extracts.flatMap { assoc =>
			if (assoc._1.isInstanceOf[TypedColumn[_, _]])
				Some(assoc.asInstanceOf[Assoc[Column, ColumnSQLExtract, _]])
			else None
		} //filterColumnExtracts(this)(extracts)

		override val columns = components.flatMap(_.columns)
		private val columnMap = columns.view.map { c => (c.name, c) }.toMap

		override def columnsWith(buff :BuffType) :Unique[ExpressionColumn[_ <: Label, _]] =
			columnsWith(this, buff)

		override def columnsWithout(buff :BuffType) :Unique[ExpressionColumn[_ <: Label, _]] =
			columnsWithout(this, buff)

		override def columnsWith(component :Component[_], buff :BuffType) :Unique[ExpressionColumn[_ <: Label, _]] = {
			val exported = export(component).columns //just to cast
			exported.count(buff.active) match {
				case 0 => Unique.empty
				case all if all == exported.size => exported
				case _ => exported.filter(buff.active)
			}
		}
		override def columnsWithout(component :Component[_], buff :BuffType) :Unique[ExpressionColumn[_ <: Label, _]] = {
			val exported = export(component).columns
			exported.count(buff.inactive) match {
				case 0 => Unique.empty
				case all if all == exported.size => exported
				case _ => exported.filter(buff.inactive)
			}
		}


		override def apply[N <: Label](label :N)(implicit get :GetListingComponent[V, N]) :TypedListingSQLMapping[F, S, get.Value, O] =
			index(label).asInstanceOf[TypedListingSQLMapping[F, S, get.Value, O]]

		override def apply[P](path :LabelPath[P])(implicit get :GetListingComponent[V, P]) :TypedListingSQLMapping[F, S, get.Value, O] =
			at(path.toSeq).asInstanceOf[TypedListingSQLMapping[F, S, get.Value, O]]

		override def export[T](component :Component[T]) :TypedListingSQLMapping[F, S, T, O] = component match {
			case indexed :TypedListingSQLMapping[F, S, T, O] @unchecked => indexed
			case _ =>
				throw new NoSuchComponentException(
					s"Component $component :${component.getClass.getName} is not a ListingSQLMapping and subcomponent of $this."
				)
		}
		override def export[T](column :Column[T]) :TypedListingColumnSQLMapping[F, S, _ <: Label, T, O] = column match {
			case indexed :TypedListingColumnSQLMapping[F, S, _, T, O] @unchecked => indexed
			case _ =>
				throw new NoSuchComponentException(
					s"Column $column :${column.getClass.getName} is not a ListingColumnSQLMapping and column of $this."
				)
		}

		override def columnNamed(name :String) :TypedListingColumnSQLMapping[F, S, _ <: Label, _, O] with Column[_] = //scalac bug...
			columnMap.getOrElse(name, null) match {
				case null => throw new NoSuchComponentException("No column '" + name + "' in " + this + ".")
				case res => res
			}

	}

}







trait TypedListingColumnSQLMapping[-F <: RowProduct, -S >: Grouped <: Single, N <: Label, V, O]
	extends TypedColumnSQLMapping[F, S, V, O] with ListingColumnSQLMapping[N, V, O]
	   with TypedListingSQLMapping[F, S, V, O] with ColumnMappingTemplate[TypedListingColumnSQLMapping.c[F]#c[S]#c[N]#Column]
{
	override def apply[K <: Label]
	                  (label :K)(implicit get :GetListingComponent[V, K]) :TypedListingSQLMapping[F, S, get.Value, O] =
		this.asInstanceOf[TypedListingSQLMapping[F, S, get.Value, O]] //these shouldn't be possible to call, but to guard against a future refactor

	override def apply[P](path :LabelPath[P])
	                     (implicit get :GetListingComponent[V, P]) :TypedListingSQLMapping[F, S, get.Value, O] =
		this.asInstanceOf[TypedListingSQLMapping[F, S, get.Value, O]]
}




object TypedListingColumnSQLMapping {

	def apply[F <: RowProduct, S >: Grouped <: Single, N <: Label, V, O]
	         (column :LabeledColumnSQL[F, S, N, V], name :String, buffs :Buffs[V] = SQLMapping.defaultBuffs[V])
			:TypedListingColumnSQLMapping[F, S, N, V, O] =
		new StandardListingColumnSQLMapping(column, name, buffs)

	def apply[F <: RowProduct, S >: Grouped <: Single, N <: Label, V, O]
	         (column :LabeledColumnSQL[F, S, N, V])
			:TypedListingColumnSQLMapping[F, S, N, V, O] =
		new StandardListingColumnSQLMapping(column, column.alias, SQLMapping.defaultBuffs[V])


	def unapply[V](mapping :MappingOf[V]) :Opt[LabeledColumnSQL[_ <: RowProduct, Grouped, _ <: Label, V]] =
		mapping match {
			case indexed :TypedListingColumnSQLMapping[f, Grouped @unchecked, n, V @unchecked, _] =>
				Got(indexed.expr)
			case _ => Lack
		}

	type TypedListingColumnSQLExtract[-F <: RowProduct, -S >: Grouped <: Single, N <: Label, -X, Y, O] =
		SpecificExtract[TypedListingColumnSQLMapping[F, S, N, Y, O], X, Y, O]

	type __ = TypedListingColumnSQLMapping[_ <: RowProduct, _ >: Grouped <: Single, _ <: Label, _, _]
	type c[F <: RowProduct] = {
		type __ = TypedListingColumnSQLMapping[F, _ >: Grouped <: Single, _ <: Label, _, _]
		type apply[-S >: Grouped <: Single, N <: Label, V, O] = TypedListingColumnSQLMapping[F, S, N, V, O]

		type c[S >: Grouped <: Single] = {
			type __ = TypedListingColumnSQLMapping[F, S, _ <: Label, _, _]
			type apply[N <: Label, V, O] = TypedListingColumnSQLMapping[F, S, N, V, O]
			type Column[V, O] = TypedListingColumnSQLMapping[F, S, _ <: Label, V, O]

			type c[N <: Label] = {
				type __ = TypedListingColumnSQLMapping[F, S, N, _, _]
				type apply[V, O]  = TypedListingColumnSQLMapping[F, S, N, V, O]
				type Column[V, O] = TypedListingColumnSQLMapping[F, S, N, V, O]

				type c[V] = {
					type __ = TypedListingColumnSQLMapping[F, S, N, V, _]
					type apply[O]   = TypedListingColumnSQLMapping[F, S, N, V, O]
					type project[O] = TypedListingColumnSQLMapping[F, S, N, V, O]
				}
			}
		}
	}



	private class StandardListingColumnSQLMapping[F <: RowProduct, S >: Grouped <: Single, N <: Label, T, O]
	              (override val expr :LabeledColumnSQL[F, S, N, T], override val name :String, override val buffs :Buffs[T])
		extends TypedListingColumnSQLMapping[F, S, N, T, O]
			with OptimizedColumnTemplate[TypedListingColumnSQLMapping.c[F]#c[S]#c[N]#Column, T, O]
	{
		def this(expr :LabeledColumnSQL[F, S, N, T], buffs :Buffs[T]) = this(expr, expr.alias, buffs)
		override type Domain = F
		override type Scope  = S
		override val form = super.form
		override val selectForm :ColumnReadForm[T] = expr.selectForm
	}

}


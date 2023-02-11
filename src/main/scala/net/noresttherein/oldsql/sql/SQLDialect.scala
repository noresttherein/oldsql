package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.OperationView.{FilterView, GroupByView, InsertView, SelectView, UpdateView, WriteOperationView}
import net.noresttherein.oldsql.collection.{Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.exceptions.{rethrow, InseparableExpressionException, InvalidSQLException, MisalignedExpressionException, NoSuchComponentException, OldSQLException, UndefinedShapeException}
import net.noresttherein.oldsql.morsels.{Decorable, Stateless, TextCase}
import net.noresttherein.oldsql.morsels.Decorable.{AbstractDecorable, Decorator}
import net.noresttherein.oldsql.morsels.TextCase.LowerCase
import net.noresttherein.oldsql.schema.{Relation, RelVar, SQLForm, SQLReadForm, SQLWriteForm, Table}
import net.noresttherein.oldsql.schema.Buff.{ExtraWhere, Ignored, NoSelect, OptionalSelect, Virtual, WherePreset}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingReadForm, MappingTemplate, TypedMapping}
import net.noresttherein.oldsql.schema.Table.Aliased
import net.noresttherein.oldsql.schema.Relation.NamedRelation
import net.noresttherein.oldsql.schema.support.AliasedColumn
import net.noresttherein.oldsql.slang.{innerClassNameOf, mappingMethods, OptionGuardExtension}
import net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.Incantation.Cantrip
import net.noresttherein.oldsql.sql.Query.QueryTemplate
import net.noresttherein.oldsql.sql.Select.{Intersect, Minus, SelectOperator, SelectTemplate, Union, UnionAll}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.{DefaultSpelling, SpellingScope, SQLSpelling}
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.{CallScope, FromScope, GroupByScope, HavingScope, InsertScope, OperandScope, QueryScope, SelectScope, TopScope, UpdateScope, WhereScope, WithScope}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.{DelegatedSpellingConstants, GroupingSpellingContext, QuerySpelling, RedactedSpelling, SpellingConstants}
import net.noresttherein.oldsql.sql.SQLExpression.{CaseAnyExpression, Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{BoundColumnParam, ColumnLValueSQL, ColumnMappingSQL, CompoundSelectSQL, JoinedRelation, JoinedTable, LValueSQL, MappingSQL, QuerySQL}
import net.noresttherein.oldsql.sql.ast.ComparisonSQL.OrderingOperator
import net.noresttherein.oldsql.sql.mechanics.{QueryReform, Reform, SpelledSQL, SQLOrdering}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}




/** A high level bridge between the SQL/DML DSL of this package and the JDBC API. It is responsible for translating
  * and formatting various SQL statements and queries into SQL strings understandable by the DBMS in use.
  * Return objects wrap [[java.sql.PreparedStatement PreparedStatement]] instances, handling setting of parameters,
  * reading and mapping the result as well as all related functions such as batching of same statement types.
  *
  * The default implementation is object [[net.noresttherein.oldsql.sql.StandardSQL$ StandardSQL]],
  * extending [[net.noresttherein.oldsql.sql.StandardSQL StandardSQL]] class, which implements a subset of
  * the SQL standard understood by most databases. Most situations can be served by this dialect, especially
  * that it uses double dispatch to delegate the actual SQL generation to the classes being its abstract representation
  * in a polymorphic, object-orienting manner, so language extensions can be performed by custom implementations
  * of the appropriate expression class. Some DBMS or requirements for special handling, such as setting particular
  * statement properties, handling of more advanced or proprietary JDBC features cannot be however served
  * by the existing API and can be introduced globally by providing an `SQLDialect` tuned to the application's
  * or DBMS needs. In particular, any changes to the SQL itself, such as formatting, comments, query hints or
  * rendering of existing expression classes will be most convenient to implement in this place.
  *
  * This is a high-level class with a narrow API serving as a facade to the complex task of SQL rendering.
  * It concerns itself mainly with the creation of executable [[net.noresttherein.oldsql.sql.Incantation Incantation]]
  * instances, including controlling [[java.sql.PreparedStatement PreparedStatement]] parameters and properties,
  * and setting up the handling of the execution results. The task of actual SQL formatting is,
  * by the [[net.noresttherein.oldsql.sql.StandardSQL! default]] implementation, delegated
  * to [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] which translates any class in the package
  * representing some database, SQL, DML or DDL element, to an intermediate
  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]]. Any changes required in that scope will be
  * easier to implement on that level, by providing a custom spelling for an otherwise unmodified `StandardSQL`.
  * @see [[net.noresttherein.oldsql.sql.StandardSQL]]
  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling]]
  * @author Marcin Mościcki
  */ //consider: maybe this should go into the mechanics package?
trait SQLDialect {
	def apply[X, R, Y](query :Query[X, R])(implicit result :StatementResult[R, Y]) :Incantation[X, Y]
	def apply[R, Y](query :QuerySQL[RowProduct, R])(implicit result :StatementResult[R, Y]) :Cantrip[Y]
//	def apply[X <: Chain, Y](call :FunctionSQL[RowProduct, GlobalScope, X, Y]) :Cantrip[Y]
	//todo: double dispatch to handle Call, InsertReturning
	def apply[X, Y](statement :DMLStatement[X, Y]) :Incantation[X, Y]
	def apply[X, Y](statement :Call[X, Y]) :Incantation[X, Y]
//	def apply[Y](statement :GroundDMLStatement[Y]) :Cantrip[Y] = apply(statement :DMLStatement[(), Y])
//	def apply[X](call :CallProcedure[X]) :Incantation[X, ()]
//	def apply[X, Y](call :CallFunction[X, Y]) :Incantation[X, Y]
}




object SQLDialect {

	/** A grammatical fragment of an SQL expression or statement, in particular used to represent a logical scope
	  * of a clause such as ''from'' or ''where'' clauses of an SQL ''select''. Used in the process of spelling
	  * SQL expressions, affecting mainly the subset of table columns used when inlining component-level expressions.
	  * This is done by default by delegating to one of the standard column lists dedicated to individual
	  * database operations (''select'', ''insert'', ''update'' and ''filter'' - for the ''where'' clause).
	  *
	  * The companion object contains constants for all scopes recognized by the standard
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy, as well as a factory method
	  * creating a scope for a given [[net.noresttherein.oldsql.OperationView OperationView]].
	  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]
	  */ //todo: move SQLSpelling to mechanics and SpellingScope to SQLSpelling
	trait SpellingScope extends Serializable {
		def view :OperationView =
			throw new UnsupportedOperationException("No OperationView associated with spelling scope " + this + ".")

		//fixme: we need to handle ExtraXxx/CustomXxx columns.
		/** All columns, direct or indirect, of the given mapping which are applicable to this operation type.
		  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
		  * the [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] buff.
		  * @see [[net.noresttherein.oldsql.OperationView.defaultColumns]]
		  */
		def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                     (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]]

		/** Export, from the point of view of `mapping`, columns of `component`, which are applicable to this
		  * operation type. These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]]
		  * list ''without'' a [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] buff after exporting.
		  * @see [[net.noresttherein.oldsql.OperationView.defaultColumns]]
		  */
		def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                     (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O]) :Unique[Col[_, O]]

		/** Columns, both direct and indirect, of the given mapping which are used by default in this operation type.
		  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
		  * the [[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]] buff. Any `NonDefault` buff on `mapping`
		  * itself is ignored.
		  * @see [[net.noresttherein.oldsql.OperationView.applicableColumns]]
		  */
		def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                  (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]]

		/** Export, from the point of view of `mapping`, columns of `component`, which are used by default in this
		  * operation type. These are all columns from `component.`[[net.noresttherein.oldsql.schema.Mapping.columns columns]]
		  * list ''without'' [[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]] buff after exporting.
		  * However, if `component` itself - in its export version for `root` - has a `NonDefault` buff,
		  * it will be ignored by this method, as if it was included in `root` by calling `root(component.+)`.
		  * @see [[net.noresttherein.oldsql.OperationView.applicableColumns]]
		  */
		def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                  (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O]) :Unique[Col[_, O]]

		def mandatoryColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                    (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
			defaultColumns(mapping).filter(isMandatory)

		def mandatoryColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                    (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
				:Unique[Col[_, O]] =
			defaultColumns(root, component).filter(isMandatory)

		/** A default column is a column which should be included in the SQL for an owning component in this scope. */
		def isDefault(column :TypedColumn[_, _]) :Boolean

		/** A prohibited column is one which can never be included in this scope as a part of the SQL
		  * for an owning component.
		  */
		def isProhibited(column :TypedColumn[_, _]) :Boolean

		/** A mandatory column is a non optional column, that is one which must be included always
		  * in the SQL for an owning component, and cannot be excluded.
		  * Implies [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.isDefault isDefault]]`(column)`.
		  */
		def isMandatory(column :TypedColumn[_, _]) :Boolean

		/** An inline scope is one in which all composite expressions (tuples and multi column components)
		  * are rendered in SQL as a comma separated list of their columns.
		  */
		def isInline :Boolean

		/** An SQL write form of the given mapping in this scope, consisting of all
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.isDefault default]] columns of the mapping.
		  * It is consistent
		  * with `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]].
		  * Note that scopes which do not correspond to any write operations may include columns which
		  * cannot be normally updated. This form should be used only for query parameters of the mapping's subject
		  * type, and not for actual database entities mapped by the mapping.
		  */
		def writeForm[S, O](mapping :TypedMapping[S, O]) :SQLWriteForm[S]

		/** An SQL write form of `component` of `root`, consisting of all columns of `component` whose
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] versions in `root` are used in this scope
		  * by default. The form includes the exact same columns as
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]]`(root, component)`,
		  * in the same order.
		  * Note that scopes which do not correspond to any write operations may include columns which
		  * cannot be normally updated. This form should be used only for query parameters of the mapping's subject
		  * type, and not for actual database entities mapped by the mapping.
		  */
		def writeForm[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLWriteForm[S]

		/** An SQL write form of `mapping`, which includes exactly the specified components.
		  * Each component is represented by an inlined list of columns which,
		  * in their [[net.noresttherein.oldsql.schema.Mapping.export export]] versions for `mapping`,
		  * are [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.isDefault default]] in this scope.
		  *
		  * Note that this may omit non-null (or otherwise required) columns, while including non-updatable ones,
		  * if this scope does not correspond to a write operation
		  * (for example, for [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.SelectScope SelectScope]]).
		  */
		def writeForm[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) :SQLWriteForm[S]

		/** An SQL write form of `component` of entity `root`, which includes its specified subcomponents.
		  * Each subcomponent is represented by an inlined list of columns which,
		  * in their [[net.noresttherein.oldsql.schema.Mapping.export export]] versions for `root`,
		  * are [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.isDefault default]] in this scope.
		  *
		  * Note that this may omit non-null (or otherwise required) columns, while including non-updatable ones,
		  * if this scope does not correspond to a write operation
		  * (for example, for [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.SelectScope SelectScope]]).
		  */
		def writeForm[S, O](root :MappingAt[O], component :TypedMapping[S, O],
		                    subcomponents :Unique[TypedMapping[_, O]]) :SQLWriteForm[S]

		/** A form used by terms for the subject type of `mapping`, which change their effective
		  * column set and [[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.form form]] based on the spelling scope
		  * specific to their place of use. The form will be based on column set
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]]`(mapping)`.
		  * For [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.SelectScope SelectScope]], the writer component
		  * of the form will use `mapping.selectableByDefault`, regardless of their buffs and applicability
		  * to other scopes. Other scopes use `mapping`'s write form specific to the scope as their writer component
		  * and an error form as a reader component, as this part of the form is unused.
		  */
		def termForm[S, O](mapping :TypedMapping[S, O]) :SQLForm[S]

		/** A form used for literals or query/statement parameters of type `S`, occurring in a position aligned
          * with a `ComponentSQL` for `component`. This includes comparisons in the form of `component == ?`
		  * and unions of SQL ''selects'' in which `component` present in one of the ''select'' clauses corresponds
		  * to a bound or unbound parameter of type `S` in another. The form consists
		  * of `this.defaultColumns(root, component)`, ''regardless of their buffs''. For example, `InsertScope`
		  * will use all columns without `NoInsertByDefault`, even if some of them are `NoSelect`,
		  * or if any columns without `OptionalSelect` are missing. Similarly, `SelectScope` will use
		  * all `selectableByDefault` columns of `component` (as specified by `root`), even if some of them
		  * are never used by `InsertView`, `UpdateView` or `FilterView`.
		  *
		  * This isn't really a problem in the stated scenario: parameters are not normally placed inside a ''select''
		  * clause, so the default columns will correspond to one of the 'write' views and the 'read' side
		  * of the returned form will remain unused, making moot the fact that some columns required to construct `S`
		  * are missing. In the unlikely case that a parameter value is indeed among selected columns,
		  * it should still be set correctly, as all components (and thus, columns) are required to have an associated
		  * extract. Even if that extract never produces a value, or doesn't for a particular `s :S`,
		  * the form will simply set that column to `null`.
		  */
		def termForm[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLForm[S]

		def canEqual(that :Any) :Boolean = that.isInstanceOf[SpellingScope]

		def isomorphic(that :SpellingScope) :Boolean
	}


	object SpellingScope {

		private class OperationViewScope(override val view :OperationView, override val toString :String,
		                                 override val isInline :Boolean = false)
			extends SpellingScope
		{
			override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
                                          (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
				view.applicableColumns(mapping)

			override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
                                          (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
					:Unique[Col[_, O]] =
				view.applicableColumns(root, component)

			override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
			                           (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
				view.defaultColumns(mapping)

			override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
			                           (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
					:Unique[Col[_, O]] =
				view.defaultColumns(root, component) //++ view.columns(root, component).filter(view.Preset.active)

			override def mandatoryColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
			                             (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
				view.mandatoryColumns(mapping)

			override def mandatoryColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
			                             (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
					:Unique[Col[_, O]] =
				view.mandatoryColumns(root, component)

			override def isDefault(column :TypedColumn[_, _])    :Boolean = view.NonDefault.inactive(column)
			override def isProhibited(column :TypedColumn[_, _]) :Boolean = view.Prohibited.active(column)
			override def isMandatory(column :TypedColumn[_, _])  :Boolean = view.Optional.inactive(column)


			override def writeForm[S, O](mapping :TypedMapping[S, O]) :SQLWriteForm[S] =
				mapping.writeForm(defaultColumns(mapping))

			override def writeForm[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLWriteForm[S] =
				root.writeForm(component, defaultColumns(root, component))

			override def writeForm[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]])
					:SQLWriteForm[S] =
				mapping.writeForm(components.flatMap(defaultColumns(mapping, _)))

			override def writeForm[S, O](root :MappingAt[O], component :TypedMapping[S, O],
			                             subcomponents :Unique[TypedMapping[_, O]]) :SQLWriteForm[S] =
				root.writeForm(component, subcomponents.flatMap(defaultColumns(root, _)))

			override def termForm[S, O](mapping :TypedMapping[S, O]) :SQLForm[S] = {
				val defaults = defaultColumns(mapping)
				val read  = mapping.selectForm(defaults)
				val write = mapping.writeForm(defaults)
				read <> write
			}

			override def termForm[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLForm[S] = {
				val export = root.export(component)
				val columns = defaultColumns(root, export)
				val selectable = root.selectable(export)
				val read =
					if (selectable.forall(c => OptionalSelect.active(c) || columns.contains(c)))
						root.selectForm(export, columns)
					else
						SQLReadForm.error(columns.size, "ERROR[" + root.mappingName + "]>") {
							throw new UnsupportedOperationException(
								"A term for component " + export + " of " + root + " in scope " + view +
								" cannot be used in a select clause because the appropriate column set " + columns +
								" does not contain all mandatory columns: " +
								selectable.filter(c => OptionalSelect.inactive(c) && !columns.contains(c)) + "."
							)
						}
				val write = root.writeForm(export, columns)
				read <> write
			}


			override def isomorphic(that :SpellingScope) :Boolean = that match {
				case other :OperationViewScope =>
					(other eq this) || (other canEqual this) && view == other.view
				case _ => false
			}

			override def equals(that :Any) :Boolean = that match {
				case other :OperationViewScope =>
					(this eq other) || other.canEqual(this) && view == other.view && toString == other.toString
				case _ => false
			}
			override def hashCode :Int = toString.hashCode
		}


		private class WriteOperationScope(override val view :WriteOperationView, override val toString :String,
		                                  override val isInline :Boolean = false)
			extends OperationViewScope(view, toString)
		{
			override def writeForm[S, O](mapping :TypedMapping[S, O]) :SQLWriteForm[S] = mapping.writeForm(view)

			override def writeForm[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLWriteForm[S] =
				root.writeForm(view, component)

			override def writeForm[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) =
				mapping.writeForm(view, components)

			override def writeForm[S, O](root :MappingAt[O], component :TypedMapping[S, O],
			                             subcomponents :Unique[TypedMapping[_, O]]) :SQLWriteForm[S] =
				root.writeForm(view, component, subcomponents)

			override def termForm[S, O](mapping :TypedMapping[S, O]) :SQLForm[S] = {
				val read  = new MappingReadForm[S, O](mapping, defaultColumns(mapping), _.form)
				val write = mapping.writeForm(view)
				read <> write
			}

			override def termForm[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLForm[S] = {
				val export = root.export(component)
				val columns = defaultColumns(root, export)
				val read  = new MappingReadForm[S, O](root, export, columns, _.form) //use _.form to force columnCount = 1
				val write = root.writeForm(view, export)
				read <> write
			}
		}


		private object TopLevel extends SpellingScope {
			override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
                                          (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
				throw new UnsupportedOperationException(
					"TopScope does not specify a concrete column set (for " + mapping + "). " + TopScopeError
				)
			override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
                                          (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
					:Unique[Col[_, O]] =
				throw new UnsupportedOperationException(
					"TopScope does not specify a concrete column set (for " + component + " of " + root + "). " +
					TopScopeError
				)
			override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
			                           (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
				applicableColumns(mapping)

			override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
			                           (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
					:Unique[Col[_, O]] =
				applicableColumns(root, component)

			override def isDefault(column :TypedColumn[_, _]) = false
			override def isProhibited(column :TypedColumn[_, _]) = false
			override def isMandatory(column :TypedColumn[_, _]) = false
			override def isInline = false

			override def writeForm[S, O](mapping :TypedMapping[S, O]) =
				throw new UnsupportedOperationException(
					"TopScope does not correspond to a concrete write form (for " + mapping + "). " + TopScopeError
				)
			override def writeForm[S, O](root :MappingAt[O], component :TypedMapping[S, O]) =
				throw new UnsupportedOperationException(
					"TopScope does not correspond to a concrete write form (for " + component + " of " + root + "). " +
					TopScopeError
				)
			override def writeForm[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) =
				throw new UnsupportedOperationException(
					"TopScope does not correspond to a concrete write form (for " + mapping + ": " + components + "). " +
					TopScopeError
				)
			override def writeForm[S, O](root :MappingAt[O], component :TypedMapping[S, O],
			                             subcomponents :Unique[TypedMapping[_, O]]) =
				throw new UnsupportedOperationException(
					"TopScope does not correspond to a concrete write form (for " + component + " of " + root +": " +
					subcomponents + "). " + TopScopeError
				)
			override def termForm[S, O](mapping :TypedMapping[S, O]) =
				throw new UnsupportedOperationException(
					"TopScope does not correspond to a concrete term form (for " + mapping + ")." + TopScopeError
				)
			override def termForm[S, O](root :MappingAt[O], component :TypedMapping[S, O]) =
				throw new UnsupportedOperationException(
					"TopScope does not correspond to a concrete term form (for " + component + " of " + root + "). " +
					TopScopeError
				)
			private final val TopScopeError =
				"The scope should have been changed with the start of the spelling of the top statement. This is a bug."

			override def isomorphic(that :SpellingScope) = this == that

			override def toString :String = "TopScope"
		}




		def apply(view :OperationView, name :String) :SpellingScope = view match {
			case write :WriteOperationView => new WriteOperationScope(write, name)
			case _ => new OperationViewScope(view, name)
		}

//		val OperandScope :SpellingScope = DefaultScope
		/** Scope for spelling top level statements. It is changed by the statement to the appropriate one
		  * during spelling. Most methods throw an [[UnsupportedOperationException]].
		  * It exists to allow various spelling methods check if the spelled query is a top level one
		  * and alter behaviour based on that. */
		val TopScope     :SpellingScope = TopLevel
		/** Scope for spelling expressions being a part of other expressions, such as sides of an equality operator.
		  * Uses 'filterable' columns.
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.WhereScope WhereScope]]. */
		val OperandScope :SpellingScope = new WriteOperationScope(FilterView, "OPERAND")
		/** Scope used for spelling subqueries in a ''compound select''. Uses 'selectable' columns. */
		val QueryScope   :SpellingScope = new OperationViewScope(SelectView, "SUBQUERY", true)
		/** Scope used for spelling ''with'' clauses. Uses 'selectable' columns. */
		val WithScope    :SpellingScope = new OperationViewScope(SelectView, "WITH")
		/** Scope used for spelling ''select'' clauses. Uses 'selectable' columns. */
		val SelectScope  :SpellingScope = new OperationViewScope(SelectView, "SELECT", true)
		/** Scope used for spelling ''from'' clauses. Uses 'filterable' columns.*/
		val FromScope    :SpellingScope = new WriteOperationScope(FilterView, "FROM")
		/** Scope used for spelling ''where'' clauses. Uses 'filterable' columns. */
		val WhereScope   :SpellingScope = new WriteOperationScope(FilterView, "WHERE")
		/** Scope used for spelling expressions within ''group by'' clauses. Uses 'filterable' columns. */
		val GroupByScope :SpellingScope = new WriteOperationScope(GroupByView, "GROUP BY", true) //this may need to be a special implementation
		/** Scope used for spelling expressions within ''having'' clauses. Uses 'filterable' columns. */
		val HavingScope  :SpellingScope = new WriteOperationScope(FilterView, "HAVING")
		/** Scope used for spelling ''insert'' statements. Uses 'insertable' columns. */
		val InsertScope  :SpellingScope = new WriteOperationScope(InsertView, "INSERT", true)
		/** Scope used for spelling ''update'' statements. Uses 'updatable' columns.  */
		val UpdateScope  :SpellingScope = new WriteOperationScope(UpdateView, "UPDATE")
		/** Scope used for spelling stored procedure calls. Uses 'filterable' columns. */
		val CallScope    :SpellingScope = new WriteOperationScope(FilterView, "CALL", true) //fixme: a proper view for the use as procedure parameters
	}




	/** A strategy class used to render all classes representing some SQL/DML/DDL syntax features or database objects
	  * as SQL (or DML/DDL, appropriately). It provides high-level `spell` methods for all classes which represent
	  * whole statements of various types, serving as entry points for converting the arguments to their
	  * intermediate [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] representation.
	  * The implementations of these entry point methods in this class universally delegate back
	  * to the rendered object's appropriate method such as
	  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]], which are responsible
	  * for assembling their SQL representation from SQL fragments for their components created using this instance.
	  * The callbacks for these [[net.noresttherein.oldsql.sql.SQLExpression subexpressions]] and other constructs
	  * which do not exist by themselves use overloaded `apply` methods (where the scope is evident) or methods named
	  * after the element(s) they print and have signatures accepting a preexisting context of the larger statement.
	  * In cases where the logic of translating the SQL objects is not trivial, they often provide additional methods,
	  * such as [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.reform reform]]
	  * or [[net.noresttherein.oldsql.schema.Table Table]]`.`[[net.noresttherein.oldsql.schema.Table.spell spell]],
	  * with forwarders declared in this class. This allows those class hierarchies to remain open for extensions
	  * for vendor-specific features and to keep this class simple and focused on decisions directly related to spelling,
	  * while still retaining the control of how the returned SQL will look like.
	  *
	  * The formatting happens recursively, by dividing any composite expressions/syntactic constructs into
	  * subexpressions, formatting them in a mostly sequential, left-to-right manner, and concatenating the results.
	  * The devil is in the context passing and combining of the meta data of the concatenated `SpelledSQL` fragments:
	  * expressions refer to table [[net.noresttherein.oldsql.sql.ast.ComponentSQL components]]
	  * and [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters, which must be properly resolved into,
	  * respectively, their actual aliases in the SQL and getter functions of the parameter(s) of the complete,
	  * final query/statement. The scope with visible symbols changes neither in a top-down, nor left-to-right fashion,
	  * requiring a look-ahead and look-aside (in particular ''select'' clauses depend on the tables
	  * in their ''from'' clauses), so the process requires performing some things out of order and passing
	  * of information in both directions.
	  *
	  * This trait itself takes a hands-off approach, implementing only methods which:
	  *   1. delegate back to the formatted object's appropriate `defaultSpelling` equivalent and satisfying themselves
	  *      with the default format defined by the element,
	  *   1. are under tight constraints coming from the contracts of collaborating classes and leave little freedom
	  *      for implementation,
	  *   1. handle very small syntactical elements such as keywords with obvious implementations.
	  * As the result, they would rarely need overriding for functional reasons.
	  *
	  * The default implementation is [[net.noresttherein.oldsql.sql.SQLDialect.DefaultSpelling DefaultSpelling]],
	  * which expands on the above with some more arbitrary implementations and - in some cases - overrides them
	  * for efficiency.
	  *
	  * Custom implementations, regardless if derived from `DefaultSpelling` or this trait directly, are likely
	  * to benefit from implementing or including components for the ''visitor'' traits for separate type hierarchies
	  * if they desire to override the behaviour on a more detailed scale, down to case-by-case for each single class
	  * provided by the framework. They come with mix-in traits implementing cases of any branch by delegating them
	  * step-by-step to their common super type, allowing overriding on arbitrary levels. Consult their documentation
	  * to learn more about composing tailored visitor implementations.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor]]
	  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.RowProductVisitor]]
	  */ //todo: prettifier - methods which return eol (or not) and indent (or not); preprocessor transformation
	trait SQLSpelling extends Decorable[SQLSpelling] { //spell methods are kind of redundant given TopScope, but at least take no extra params

		/** Decorates this instance with a proxy `SQlSpelling` in a 'sticky' (recursion-aware) manner.
		  * A naive wrapper implementation would be lost after it delegated to the wrapped instance, as the latter
		  * would pass itself, rather than its wrapper, when recursively calling
		  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]] or any other method
		  * on the subexpression(s) of the spelled expression. For this reason, an implementation supporting
		  * this functionality, must be aware of the existence of its wrapper or, in general, simply what the
		  * effective strategy, which should be passed to spelled expressions, is. This requires a bidirectional
		  * relationship in which both instances reference one another. Rather than use a mutable variable, we opt here
		  * for an inversion of responsibility, with the ''wrapped'' (this) spelling being the primary owner.
		  *
		  * This is implemented by creating a copy `s` of this instance, with
		  * its [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.self self]] property set to
		  * `redactor(s)`, and returning `s.self` (i.e, the instance created by the argument function).
		  * @param redactor a constructor function creating a proxy `SQlSpelling` accepting the wrapped instance
		  *                 as the argument.
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.RedactedSpelling]]
		  */
		def redact(redactor :SQLSpelling => SQLSpelling) :SQLSpelling = decorate(redactor)


		/** Renders as a full textual DML an abstract representation of any supported database operation.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @return a formatted DML string with an [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *         all bound and unbound parameters present in the statement.
		  */
		def spell[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P] =
			defaultSpelling(statement)

		/** Renders as a stand alone SQL query a parameterized query. Will add a prefix with a ''with'' clause
		  * containing the definitions of all
		  * [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]] referenced by the query,
		  * regardless of whether within a dependent ''select'' or not. Delegates to `this(query)(context)`
		  * for rendering query proper, where `context` is the instance returned in the rendered ''with'' clause.
		  * Unless this or the delegated method are overriden, the call eventually invoke
		  * `query.`[[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]].
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @return a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *         all bound and unbound parameters present in the query.
		  */
		@throws[InvalidSQLException]("if the query cannot be formatted as SQL because of illegal internal state " +
		                             "or a subexpression not supported by this spelling dialect.")
		def spell[P, V](query :Query[P, V]) :SpelledSQL[P] =
			top(query)(newContext)

		/** Renders as a stand alone SQL query a top-level, parameterless ''select'' expression. Will add a prefix
		  * with a ''with'' clause containing the definitions of all
		  * [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]] referenced by the query,
		  * regardless of whether within a dependent ''select'' or not. Delegates to
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply apply]]`(query)(`[[net.noresttherein.oldsql.sql.Dual Dual]]`, context, `[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]]`.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.paramless paramless]]`)`,
		  * where `context` is the instance returned from spelling `query.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.withClause withClause]].
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @return a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *         all bound and unbound parameters present in the query.
		  */
		@throws[InvalidSQLException]("if the expression cannot be formatted as SQL because of illegal internal state " +
		                             "or its type is not supported by this spelling dialect.")
		def spell[V](query :QuerySQL[RowProduct, V]) :SpelledSQL[@~] =
			top(query)(Dual, newContext, Parameterization.paramless)


		/** Renders as SQL an optional ''with'' clause preceding a query or a statement. If the passed clause
		  * does not contain any [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]],
		  * an empty string will be returned.
		  */
		@throws[InvalidSQLException]("if a common table expression in the clause cannot be formatted as SQL because of " +
		                             "illegal internal state or its type is not supported by this spelling dialect.")
		def apply[P](ctes :WithClause)(context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P] =
			if (ctes.isEmpty)
				SpelledSQL(context)
			else {  //todo: WithParam (new, not the JoinParam)
				val init = SpelledSQL(WITH_, context)
				val spelling = inWith
				(init /: ctes) { (sql, cte) =>
					val name = inWith.name(cte)(sql.context)
					val decl =
						if (sql eq init) name + _AS_ + "("
						else (", " +: name) + _AS_ + "("
					sql + decl + spelling(cte.query)(Dual, name.context, params) + ")"
				}
			}

		/** Renders as SQL a parameterized query expression for use as a part of a larger SQL/DML expression -
		  * either as a dependent (nested) ''select'' or in a compound ''select''. This is analogous to main
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply[P,F<:RowProduct,V](e:SQLExpression[F,Grouped,V])* apply]]
		  * method for SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] and the control flow follows
		  * the same pattern; the method delegates to
		  * `query.`[[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]] by means of bridge method
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.defaultSpelling[P](e:Query[P,_])* defaultSpelling]]`(query)(context)`.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam V      the nominal value type of the formatted expression.
		  * @param query   the formatted query as an abstract object.
		  * @param context the list of tables in the outer ''select'' for the use of `query` as a dependent ''select'',
		  *                or an empty context if the argument is to be used as a stand-alone query.
		  *                `Query` instances do not depend on any external tables and all tables used by them
		  *                are invisible outside their scope, so it is included unchanged in the returned `SpelledSQL`.
		  *                The parameter is declared for consistency with other methods and to facilitate uniform
		  *                passing of the context when formatting subsequent fragments of a larger query.
		  * @return        a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]]
		  *                setting all bound and unbound parameters present in the query.
		  */ //consider: removing context parameter
		@throws[InvalidSQLException]("if the query cannot be formatted as SQL because of illegal internal state " +
		                             "or a subexpression not supported by this spelling dialect.")
		def apply[P, V](query :Query[P, V])(context :SQLContext[P]) :SpelledSQL[P] =
			defaultSpelling(query)(context)

		/** Renders as SQL an abstract representation of any SQL expression, for use within a larger SQL/DML expression.
		  * Defaults to `e.`[[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]]`(this)(context, params)`
		  * (using bridge method
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.defaultSpelling[P,F<:RowProduct](e:SQLExpression[F,Grouped,_])* defaultSpelling]]`(this)(context, params)`).
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam F      the `RowProduct` on which `e` is based, listing all tables in the ''from'' clause
		  *                of the containing expression as well as columns in its ''group by'' clause (if present).
		  * @tparam V      the nominal value type of the formatted expression.
		  * @param e       the formatted SQL (sub)expression as an abstract syntax tree.
		  * @param from    the ''from'' clause of the (most nested) SQL ''select'' of which the formatted expression
		  *                is a part.
		  * @param context the list of tables in scope of the formatted expression; this includes all tables
		  *                in the ''from'' clause of the encompassing expression (even if under ''group by'' clause)
		  *                as well as any tables in the outer ''select''s and unbound parameters, in the order
		  *                consistent with their appearance in `F`. It is purely an 'in' parameter, included unchanged
		  *                in the returned `SpelledSQL`, as only [[net.noresttherein.oldsql.sql.FromClause FromClause]]
		  *                (including synthetic unbound parameters) can introduce new table aliases to scope.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters of the formatted SQL. It provides getter functions `P => X` for all parameters `X`
		  *                in the scope of the formatted expression, given
		  *                the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL.
		  *                Note that this means that rendering of a ''select'' clause requires the parameterization
		  *                created by the rendering of its ''from'' clause.
		  * @return        a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *                all bound and unbound parameters present in the query.
		  */
		@throws[InvalidSQLException]("if the expression cannot be formatted as SQL because of illegal internal state " +
		                             "or its type is not supported by this spelling dialect.")
		def apply[P, F <: RowProduct, V]
                 (e :SQLExpression[F, Grouped, V])
                 (from :F, context :SQLContext[P], params: Parameterization[P, F]) :SpelledSQL[P] =
			defaultSpelling(e)(from, context, params)

		/** Recursively splits an expression into its constituting columns, to be rendered as a part
		  * of a comma-separated list, for example when inlining a complex Scala type to individual parameters
		  * for a procedure call or when formatting a ''select'' clause. Not all expressions can be split this way,
		  * with the most common case being ''select'' expressions returning multiple columns - in that case
		  * an exception is thrown, although this should not occur in practice when the method is called in the process
		  * of formatting a valid top-level query/statement.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam F      the `RowProduct` on which `e` is based, listing all tables in the ''from'' clause
		  *                of the containing expression as well as columns in its ''group by'' clause (if present).
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @param e       the formatted SQL (sub)expression as an abstract syntax tree.
		  * @param from    the ''from'' clause of the (most nested) SQL ''select'' of which the formatted expression
		  *                is a part.
		  * @param context the list of tables in scope of the split expression; this includes all tables
		  *                in the ''from'' clause of the encompassing expression (even if under ''group by'' clause)
		  *                as well as any tables in the outer ''select''s and unbound parameters, in the order
		  *                consistent with their appearance in `F`. It is purely an 'in' parameter, included unchanged
		  *                in the returned `SpelledSQL`, as only [[net.noresttherein.oldsql.sql.FromClause FromClause]]
		  *                (including synthetic unbound parameters) can introduce new table aliases to scope.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters of the formatted SQL. It provides getter functions `P => X` for all parameters `X`
		  *                in the scope of the formatted expression, given
		  *                the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL.
		  *                Note that this means that rendering of a ''select'' clause requires the parameterization
		  *                created by the rendering of its ''from'' clause.
		  * @return        a list of formatted SQL strings for every column in this expression in the exact same order.
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]]
		  *                included in the last `SpelledSQL` of the list represent the complete state,
		  *                taking into account all previous columns and is the same as if the expression was formatted
		  *                using [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply[P,F<:RowProduct,V](e:SQLExpression[F,Grouped,V])* apply]]
		  *                method.
		  */
		@throws[InseparableExpressionException]("if the expression cannot be separated into individual column strings, " +
		                                        "for example a multi-column SQL select.")
		@throws[InvalidSQLException]("if any of the columns cannot be formatted as SQL because of illegal " +
		                             "internal state or its type is not supported by this spelling dialect.")
		def explode[P, F <: RowProduct, V]
		           (e :SQLExpression[F, Grouped, V], independent :Boolean = true)
		           (from :F, context :SQLContext[P], params :Parameterization[P, F]) :Seq[SpelledSQL[P]] =
			explodedSpelling(e, independent)(from, context, params)


		/** Renders as SQL a call to an aggregate function (including any modifying clauses such as ''distinct'').
		  * This method is called from
		  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL.defaultSpelling AggregateSQL.defaultSpelling]] and
		  * immediately dispatches to [[net.noresttherein.oldsql.sql.AggregateFunction.defaultSpelling AggregateFunction.defaultSpelling]].
		  * @tparam P       the type of the parameter or parameters of the complete query expression containing
		  *                 the enclosing expression; typically, it will be
		  *                 `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                 do not exist.
		  * @tparam F       the 'ungrouped' `FromClause` representing the namespace available
		  *                 to the aggregated expression (rather than the one on which the containing
		  *                 [[net.noresttherein.oldsql.sql.ast.AggregateSQL AggregateSQL]] is based).
		  * @param function the aggregate function invoked for a group of rows.
		  * @param arg      the expression in the scope of a single row passed as the argument to the function.
		  * @param from     the ''from'' clause of the (most nested) SQL ''select'' of which the formatted expression
		  *                 is a part.
		  * @param context  the spelling context with table aliases indexed consistently with `F`.
		  * @param params   Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                 parameters of the formatted SQL. It provides getter functions `P => X` for all parameters `X`
		  *                 in the scope of the formatted expression, given
		  *                 the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                 of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                 representing them. These getters are in turn necessary to adapt the forms of individual
		  *                 expression to the type `P` of the parameter(s) of the final SQL.
		  *                 Note that this means that rendering of a ''select'' clause requires the parameterization
		  *                 created by the rendering of its ''from'' clause.
		  * @return         the formatted SQL for `function(arg)` (pseudocode), with the same
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]]
		  *                 (or at least an instance with unmodified relation indexing) and
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]]
		  *                 for all parameters used by `arg`.
		  */ //consider: removing it and short wiring from AggregateSQL to AggregateFunction - seems like an exception made for no reason
		@throws[InvalidSQLException]("if the arguments cannot be formatted as SQL because of illegal internal state " +
		                             "or a type not supported by this spelling dialect.")
		def apply[P, F <: RowProduct](function :AggregateFunction, distinct :Boolean)
		                             (arg :ColumnSQL[F, Grouped, _])
		                             (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
			function.defaultSpelling(self)(arg, distinct)(from, context, params)

		/** Renders as SQL a call to a standard SQL function (or a custom stored function). This method is called from
		  * [[net.noresttherein.oldsql.sql.ast.FunctionSQL.defaultSpelling FunctionSQL.defaultSpelling]] and
		  * immediately dispatches to [[net.noresttherein.oldsql.sql.StoredFunction.defaultSpelling StoredFunction.defaultSpelling]].
		  * @tparam P       the type of the parameter or parameters of the complete query expression containing the
		  *                 enclosing expression; typically, it will be
		  *                 `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                 do not exist.
		  * @tparam F       the `RowProduct` on which `args` is based, listing all tables in the ''from'' clause
		  *                 of the containing expression as well as columns in its ''group by'' clause (if present).
		  * @tparam X       a chain listing the types of all formal parameters of `function`.
		  * @tparam Y       the return type of the function and the value type of the whole formatted SQL expression.
		  * @param function the called function.
		  * @param args     a tuple expression containing expressions for all arguments of `function` as elements.
		  * @param from     the ''RowProduct'' instance serving as the domain for the arguments, in particular including
		  *                 the definitions of all unbound parameters.
		  * @param context  the spelling context with table aliases indexed consistently with `F`.
		  * @param params   Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                 parameters of the formatted SQL. It provides getter functions `P => X` for all parameters `X`
		  *                 in the scope of the formatted expression, given
		  *                 the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                 of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                 representing them. These getters are in turn necessary to adapt the forms of individual
		  *                 expression to the type `P` of the parameter(s) of the final SQL.
		  *                 Note that this means that rendering of a ''select'' clause requires the parameterization
		  *                 created by the rendering of its ''from'' clause.
		  * @return         the formatted SQL for `function(args)` (pseudocode), with the same
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]]
		  *                 (or at least an instance with unmodified relation indexing) and
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]]
		  *                 setting all parameters used by `args` by the use of `params`.
		  */
		@throws[InvalidSQLException]("if the arguments cannot be formatted as SQL because of illegal internal state " +
		                             "or a type not supported by this spelling dialect.")
		def apply[P, F <: RowProduct, X, Y](function :SQLExecutable[X, Y])(args :SQLExpression[F, Grouped, X])
		                                   (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			function.defaultSpelling(self)(args)(from, context, params)


		/** Renders as SQL the most nested ''from'' clause of the passed relation product.
		  * If `from` conforms to [[net.noresttherein.oldsql.sql.FromClause FromClause]]
		  * (is not aggregated), this will print the full ''from'' clause, including any join conditions if
		  * rendered using the 'join ... on ...' syntax, but without a ''where'' clause, instead collecting remaining
		  * filter condition in the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]]
		  * returned with `SpelledSQL`. If `from` is a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]],
		  * returned SQL will contain full ''from'', ''where'' and ''group by'' clauses, but without a ''having''
		  * clause. [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] ''from'' clauses
		  * (without a ''group by'' clause) are formatted the same as unaggregated.
		  *
		  * The default implementation delegates
		  * to `from.`[[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling defaultSpelling]].
		  * That method spells the rightmost relation (table/''group by'' expression),
		  * appending it recursively to SQL for its left side, as formatted by this method,
		  * stopping when [[net.noresttherein.oldsql.sql.Dual Dual]] or
		  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] are reached, marking the begin of the rendered
		  * ''from'' clause.
		  *
		  * This method is not the entry point to the recursion, which starts with either
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.fromWhere fromWhere]] or
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.groupByHaving groupByHaving]], depending on the type
		  * of the relation product used; the proper one between the above is picked by the polymorphic method
		  * [[net.noresttherein.oldsql.sql.RowProduct.spell RowProduct.spell]]. The latter should always be used
		  * when formatting 'whole' objects (i.e., those referenced by [[net.noresttherein.oldsql.sql.Select Select]]
		  * expressions.
		  *
		  * Defaults to `from.`[[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling defaultSpelling]].
		  * @param context the namespace containing the aliases of all tables in scope of `from` (but not `from` itself)
		  *                due to belonging to an outer ''select'', or a fresh instance if the formatted ''select''
		  *                is the top-level expression is a top select.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters of the formatted SQL, including those in `from` itself.
		  *                It provides getter functions `P => X` for all parameters `X` in the scope of the formatted
		  *                expression, given the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL. This will likely be
		  *                `from.`[[net.noresttherein.oldsql.sql.RowProduct.parameterization parameterization]],
		  *                either itself or mapped/composed.
		  * @return a formatted SQL string in a chunked form with rendered and concatenated all of the ''from'',
		  *         ''where'', ''group by'', ''having'' clauses which are present in this instance - possibly none
		  *         and an empty string for [[net.noresttherein.oldsql.sql.Dual Dual]].
		  *         The [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context spelling context]] included
		  *         appends to `context` argument the aliases for all tables present in the ''from'' clause,
		  *         as well as empty placeholders for [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters.
		  *         The [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]] included contains
		  *         a [[net.noresttherein.oldsql.schema.SQLWriteForm write form]] for all formatted expressions
		  *         (but not necessarily for all ''join/where/having'' conditions present in the instance).
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.fromWhere]]
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.groupByHaving]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.spell]]
		  */
		@throws[InvalidSQLException]("if the clause cannot be formatted as SQL because of illegal internal state " +
		                             "or either its type or one of embedded expressions is not supported " +
		                             "by this spelling dialect.")
		def apply[P](from :RowProduct)
		            (context :SQLContext[P], params :Parameterization[P, from.Generalized]) :SpelledSQL[P] =
			defaultSpelling(from)(context, params)

		/** Renders as an SQL fragment full ''from'' and ''where'' clauses of a ''select''. The process recursively calls
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply(from:RowProduct)* apply(from)(context, params)]]
		  * (which by default will delegate to
		  * [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling RowProduct.defaultSpelling]]) and simultaneously
		  * appends ''join'' clauses (or simply lists the tables) and builds
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]] for the rest
		  * of the SQL ''select'' to which `from` belongs. Recursion stops and backtracks when either
		  * [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.Subselect Subselect]]
		  * is encountered, marking the start of the ''from'' clause of the most nested ''select''. From that point,
		  * table aliases and join conditions are added to `context`.
		  * This method is invoked from [[net.noresttherein.oldsql.sql.RowProduct.spell RowProduct.spell]]
		  * and the latter should be preferred as the entry point.
		  * @param from    the ''from'' clause to format.
		  * @param context the namespace containing the aliases of all tables in scope of `from` (but not `from` itself)
		  *                due to belonging to an outer ''select'', or a fresh instance if the formatted ''select''
		  *                is the top-level expression is a top select.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters of the formatted SQL, including those in `from` itself.
		  *                It provides getter functions `P => X` for all parameters `X` in the scope of the formatted
		  *                expression, given the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL. This will likely be
		  *                `from.`[[net.noresttherein.oldsql.sql.RowProduct.parameterization parameterization]],
		  *                either itself or mapped/composed.
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.groupByHaving]]
		  */
		@throws[InvalidSQLException]("if the clause cannot be formatted as SQL because of illegal internal state " +
		                             "or either its type or one of embedded expressions is not supported " +
		                             "by this spelling dialect.")
		def fromWhere[P](from :FromClause)
		                (context :SQLContext[P], params :Parameterization[P, from.Generalized]) :SpelledSQL[P] =
		{
			val spelledFrom = inFrom(from)(context, params)
			val whereClause = spelledFrom.context.whereClause ++ extraWhereConditions(from)(spelledFrom.context, params)
			if (whereClause.isEmpty)
				spelledFrom
			else {
				val whereSQL = whereClause.reduce(_ + _AND_ + _)
				val sql = spelledFrom.sql + _WHERE_ + whereSQL.sql
				val newContext = spelledFrom.context.copy(whereClause = PassedArray.empty)
				SpelledSQL(sql, spelledFrom.setter + whereSQL.setter, newContext)
			}
		}

		/** Extra conditions for the ''where'' clause of `from` coming from `WherePreset` and `ExtraWhere` buffs
		  * on columns of its tables.
		  */
		private def extraWhereConditions[P](from :FromClause)
		                                   (context :SQLContext[P], params :Parameterization[P, from.Generalized]) =
			from.tableStack.view.flatMapWith(context) { (origin, ctx) =>
				def where[O >: from.Generalized <: RowProduct, T[A] <: MappingAt[A]](origin :JoinedRelation[O, T]) =
					origin.mapping.columns.view.flatMapWith(ctx) { (column, ctx) =>
						def condition[V](column :TypedColumn[V, O]) = {
							val anchored = (origin.anchored :TypedMapping[_, O]).export(column)
							WherePreset.Value(anchored).map { preset =>
								val expr = (origin \ column).toColumnSQL === BoundColumnParam(preset)(anchored.form)
								apply(expr)(from.generalized, ctx, params)
							} orElse ExtraWhere.unapply(anchored).map { buff =>
								val expr = (origin \ column).toColumnSQL === buff.expr
								apply(expr)(from.generalized, ctx, params)
							}
						}
						val columnCondition = condition(column)
						(columnCondition, columnCondition.mapOrElse(_.context, ctx))
					} to PassedArray
				val originConditions = where(origin)
				val nextCtx = if (originConditions.nonEmpty) originConditions.last.context else ctx
				(originConditions, nextCtx)
			} to PassedArray

		/** Renders as an SQL fragment full ''from'', ''where'', ''group by'' and ''having'' clauses of a ''select''.
		  * This method is invoked from [[net.noresttherein.oldsql.sql.RowProduct.spell RowProduct.spell]]
		  * and the latter should be preferred as the entry point. The process recursively calls
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply(from:RowProduct)* apply(from)(context, params)]]
		  * (which by default will delegate to
		  * [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling RowProduct.defaultSpelling]]) and simultaneously
		  * appends ''join'' clauses (or simply lists the tables) and builds `SQLContext` for the rest
		  * of the SQL ''select'' to which `from` belongs. When the recursion encounters the
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] instance, its `defaultSpelling` calls
		  * [[net.noresttherein.oldsql.sql.FromClause.spell spell]] again on the grouped clause, which forwards the call
		  * to [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.fromWhere fromWhere]] method of this instance.
		  * @param from    the ''from'' clause with a ''group by'' clause to format.
		  * @param context the namespace containing the aliases of all tables in scope of `from` (but not `from` itself)
		  *                due to belonging to an outer ''select'', or a fresh instance if the formatted ''select''
		  *                is the top-level expression is a top select.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters of the formatted SQL, including those in `from` itself.
		  *                It provides getter functions `P => X` for all parameters `X` in the scope of the formatted
		  *                expression, given the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL. This will likely be
		  *                `from.`[[net.noresttherein.oldsql.sql.RowProduct.parameterization parameterization]],
		  *                either itself or mapped/composed.
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.fromWhere]]
		  */
		@throws[InvalidSQLException]("if the clause cannot be formatted as SQL because of illegal internal state " +
		                             "or either its type or one of embedded expressions is not supported " +
		                             "by this spelling dialect.")
		def groupByHaving[P](from :GroupByClause)
		                    (context :SQLContext[P], params :Parameterization[P, from.Generalized]) :SpelledSQL[P] =
		{
			val spelledGroupBy = inGroupBy(from)(context, params)
			if (from.filter == True) spelledGroupBy
			else spelledGroupBy + _HAVING_ + inHaving(from.filter)(from.generalized, spelledGroupBy.context, params)
		}

		/** SQL fragment for an empty ''from'' clause. Defaults to an empty string, leading to rendering a ''select''
		  * without a ''from'' clause.
		  */
		def emptyFrom[P](context :SQLContext[P]) :SpelledSQL[P] = SpelledSQL(context)

		/** Creates an SQL fragment adding a single table to a non-empty ''from'' clause.
		  * This can happen both using a ''join'' clause (and optionally an accompanying ''on'' clause),
		  * and as simple comma-separated concatenation.
		  * If [[net.noresttherein.oldsql.sql.Join.condition join.condition]] is not used in the the returned SQL,
		  * it is added to the pending
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereClause whereClause]]
		  * in the `SQLContext` included in the returned SQL.
		  * @param join     The ''from'' clause whose last join should be spelled.
		  * @param joinKind An SQL join kind (i.e., `"left join"`, `"outer join"`, etc.).
		  * @param context  A namespace with aliases of all tables in `L`, indexed consistently with their
		  *                 [[net.noresttherein.oldsql.sql.ast.JoinedRelation.position positions]].
		  * @param params   Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                 parameters in `L`. It provides getter functions `P => X` for all parameters `X` declared by
		  *                 `join.left`, given the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                 of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                 representing them. These getters are in turn necessary to adapt the forms of individual
		  *                 expression to the type `P` of the parameter(s) of the final SQL. As this method is called
		  *                 recursively for ever shorter prefixes of the complete ''from'' clause, instances are
		  *                 typically obtained through 'shrinking' methods of `RowProduct` directly containing `join`.
		  */
		@throws[InvalidSQLException]("if the clause cannot be formatted as SQL because of illegal internal state " +
		                             "or either its type or one of embedded expressions is not supported " +
		                             "by this spelling dialect.")
		def join[P, L <: FromSome, R[O] <: MappingAt[O]]
		        (join :L Join R, joinKind :String)(context :SQLContext[P], params :Parameterization[P, join.Generalized])
					:SpelledSQL[P]

		/** Root entry point for the spelling of tables in a ''from'' clause.
		  * Called from [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.join join]], it delegates directly to
		  * `from.`[[net.noresttherein.oldsql.sql.NonParam.last last]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedTable.spell spell]],
		  * which by default delegates back to one of `this.table` methods.
		  * @param from     The ''from'' clause whose last table should be spelled.
		  * @param context  A namespace with aliases of all tables in `L`, indexed consistently with their
		  *                 [[net.noresttherein.oldsql.sql.ast.JoinedRelation.position positions]].
		  * @param params   Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                 parameters in `L`. It provides getter functions `P => X` for all parameters `X` declared by
		  *                 `join.left`, given the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                 of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                 representing them. These getters are in turn necessary to adapt the forms of individual
		  *                 expression to the type `P` of the parameter(s) of the final SQL. As this method is called
		  *                 recursively for ever shorter prefixes of the complete ''from'' clause, instances are
		  *                 typically obtained through 'shrinking' methods of `RowProduct` directly containing `join`.
		  * @return SQL representation of the last table in the given ''from'' clause.
		  */
		@throws[InvalidSQLException]("if the table cannot be formatted as SQL because of illegal internal state " +
		                             "or a subexpression not supported by this spelling dialect.")
		def last[P, L <: RowProduct, T[A] <: MappingAt[A]] //todo: merge param groups in Scala 3
		        (from :L NonParam T)(context :SQLContext[P], params :Parameterization[P, from.Generalized])
				:SpelledSQL[P] =
			from.last.spell(self)(from.generalized, context, params)

		/** Formats a reference to single table (including ''derived tables'', that is ''select'' expressions)
		  * included in a ''from'' clause of a ''select''. This overloaded method variant is called when no alias
		  * for the table has been explicitly specified by the application and one may need to be picked
		  * in order to ensure uniqueness of aliases for all tables listed in `context`.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing
		  *                the formatted ''from'' clause; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam M      the type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]]
		  *                used to map individual rows of `table`.
		  * @param table   a relation permissible in the ''from'' clause.
		  * @param from    a `RowProduct` containing all tables preceding `table` in the ''from'' clause
		  *                of the formatted SQL ''select'', including any tables inherited from outer ''selects''
		  *                in case of dependent ''selects''.
		  * @param context spelling context containing aliased of all preceding relations in `from` clause,
		  *                as well as aliases of tables from enclosing ''selects'' which remain in scope
		  *                for dependent ''selects''.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters in `from`.
		  *                It provides getter functions `P => X` for all parameters `X` in the scope of the formatted
		  *                expression, given the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL. This will likely be
		  *                `from.`[[net.noresttherein.oldsql.sql.RowProduct.parameterization parameterization]],
		  *                either itself or mapped/composed.
		  * @return        in the most common cases of actual tables and stored views
		  *                ([[net.noresttherein.oldsql.schema.RelVar RelVar]] instances), a string containing
		  *                the table/view name together with an optional alias. Included
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] contains
		  *                an additional entry in respect to `context`, while
		  *                the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]] sets any
		  *                bound and unbound parameters used by the table if it is
		  *                a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]],
		  *                and is empty for base tables.
		  */
		@throws[InvalidSQLException]("if the argument is a table expression with an illegal internal state " +
		                             "or a subexpression not supported by this spelling dialect.")
		def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M])(from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P]

		/** Formats a single table (including ''derived tables'', that is ''select'' expressions) included in a ''from''
		  * clause of a ''select''. This overloaded method variant is called when an alias for the table
		  * has been explicitly specified by the application. If the alias is already used by another table in `context`,
		  * it may be necessary to assign a unique one - the handling of this case is given up to subclasses.
		  * The exception is an empty string, which has the special meaning that no alias should be assigned
		  * (and references to columns of this table should not be qualified with the table name).
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing
		  *                the formatted ''from'' clause; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam M      the type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]]
		  *                used to map individual rows of `table`.
		  * @param from    a `RowProduct` containing all tables preceding `table` in the ''from'' clause
		  *                of the formatted SQL ''select'', including any tables inherited from outer ''selects''
		  *                in case of dependent ''selects''.
		  * @param context spelling context containing aliased of all preceding relations in `from` clause,
		  *                as well as aliases of tables from enclosing ''selects'' which remain in scope
		  *                for dependent ''selects''.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters in `from`.
		  *                It provides getter functions `P => X` for all parameters `X` in the scope of the formatted
		  *                expression, given the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL. This will likely be
		  *                `from.`[[net.noresttherein.oldsql.sql.RowProduct.parameterization parameterization]],
		  *                either itself or mapped/composed.
		  * @return        in the most common cases of actual tables and stored views
		  *                ([[net.noresttherein.oldsql.schema.RelVar RelVar]] instances), a string containing
		  *                the table/view name together with an optional alias. Included
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] contains
		  *                an additional entry in respect to `context`, while
		  *                the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]] sets any
		  *                bound and unbound parameters used by the table if it is
		  *                a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]],
		  *                and is empty for base tables.
		  */
		@throws[InvalidSQLException]("if the argument is a table expression with an illegal internal state " +
		                             "or a subexpression not supported by this spelling dialect.")
		def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M], alias :String)
		         (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P]

		/** Formats a single table (including ''derived tables'', that is ''select'' expressions) included in a ''from''
		  * clause of a ''select''. This overloaded method variant is equivalent
		  * to `this.table(table, alias.get)(from, context, params)` if `alias` is defined,
		  * and to `this.table(table)(from, context, params)` otherwise. Each way, the `SQLContext` included
		  * in the returned `SpelledSQL` will contain an additional entry with a unique ''non-empty alias'' used
		  * by the expressions to refer to this particular occurrence of `table`.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing
		  *                the formatted ''from'' clause; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam M      the type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]]
		  *                used to map individual rows of `table`.
		  * @param table   a relation permissible in the ''from'' clause.
		  * @param alias   proposed alias for the table.
		  * @param from    a `RowProduct` containing all tables preceding `table` in the ''from'' clause
		  *                of the formatted SQL ''select'', including any tables inherited from outer ''selects''
		  *                in case of dependent ''selects''.
		  * @param context spelling context containing aliased of all preceding relations in `from` clause,
		  *                as well as aliases of tables from enclosing ''selects'' which remain in scope
		  *                for dependent ''selects''.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters in `from`.
		  *                It provides getter functions `P => X` for all parameters `X` in the scope of the formatted
		  *                expression, given the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL. This will likely be
		  *                `from.`[[net.noresttherein.oldsql.sql.RowProduct.parameterization parameterization]],
		  *                either itself or mapped/composed.
		  * @return        in the most common cases of actual tables and stored views
		  *                ([[net.noresttherein.oldsql.schema.RelVar RelVar]] instances), a string containing
		  *                the table/view name together with an optional alias. Included
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] contains
		  *                an additional entry in respect to `context`, while
		  *                the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]] sets any
		  *                bound and unbound parameters used by the table if it is
		  *                a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]],
		  *                and is empty for base tables.
		  */
		@throws[InvalidSQLException]("if the argument is a table expression with an illegal internal state " +
		                             "or a subexpression not supported by this spelling dialect.")
		def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M], alias :Option[String])
		         (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
			alias.mapOrElse(this.table(table, _)(from, context, params), this.table(table)(from, context, params))

		/** Updates the spelling context with an alias used in an ''as'' clause for the given table's occurrence
		  * in a ''from'' clause. The alias will be prepended to property
		  * parameter and can be accessed as `context(0)`. It must be the value prepended to property
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.aliasStack aliasStack]] of the `context`
		  * by method `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.table table]]`(table, alias)`
		  * and is extracted in order to avoid potential spelling of the whole
		  * [[net.noresttherein.oldsql.schema.Table.TableExpression table expression]].
		  * @param table the last table in ''from'' clause `from`.
		  * @param alias optional suggestion for the alias. If `None`, an alias will be generated by this spelling
		  *              strategy in conjunction with `context`, potentially taking into account the `table` instance.
		  *              It does not enforce the lack of an ''as'' clause for this table; in order to force the use
		  *              of unqualified column names, pass `Some("")`.
		  * @return a unique alias for the given table or an empty `String` if the table would not have an ''as'' clause
		  *         inside the given ''from'' clause. It equals
		  *         `this.table(table, alias)(from, context, from.parameterization).context(0)`.
		  */
		def alias[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M], alias :Option[String])(from :F, context :SQLContext[P]) :String

		/** Spells a grouping expression from a ''group by'' clause exposed as a pseudo relation.
		  * @param grouping a pseudo relation representing a grouping expression, that is an expression adding
		  *                 one or more columns to the ''group by'' clause of the formatted SQL ''select''. The use of
		  *                 [[net.noresttherein.oldsql.schema.Relation Relation]] interface allows the use of
		  *                 `grouping.`[[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation.expr expr]]
		  *                 (or its subexpression) in the ''select'' and ''having'' clauses of the complete ''select''
		  *                 through [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] interface,
		  *                 the same way as if it was a table/table component.
		  * @param from     a `RowProduct` containing all relations preceding `grouping` in the ''from'' clause
		  *                 of the formatted SQL ''select'', including any tables inherited from outer ''selects''
		  *                 in case of dependent ''selects''.
		  * @param context  spelling context containing aliased of all preceding relations in `from` clause,
		  *                 as well as aliases of tables from enclosing ''selects'' which remain in scope
		  *                 for dependent ''selects''. Grouping expressions (instances of
		  *                 [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]]) do not have any real
		  *                 aliases, as the expression is always copied verbatim when used, rather than referenced.
		  *                 Instead, it contains a counter of such expressions in `from` in order to retain consistent
		  *                 indexing with aliased relations of `from`.
		  * @param params   Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                 parameters in `from`.
		  *                 It provides getter functions `P => X` for all parameters `X` in the scope of the formatted
		  *                 expression, given the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                 of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                 representing them. These getters are in turn necessary to adapt the forms of individual
		  *                 expression to the type `P` of the parameter(s) of the final SQL. This will likely be
		  *                 `from.`[[net.noresttherein.oldsql.sql.RowProduct.parameterization parameterization]],
		  *                 either itself or mapped/composed.
		  * @tparam P       the type of the parameter or parameters of the complete query expression containing
		  *                 the formatted ''from'' clause; typically, it will be
		  *                 `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                 do not exist.
		  * @tparam F       the `RowProduct` listing all preceding tables in the ''from'' clause as well as tables
		  *                 inherited from enclosing ''select''(s) and remaining in the scope of the formatted ''select''.
		  */
		@throws[InvalidSQLException]("if the expression cannot be formatted as SQL because of illegal internal state " +
		                             "or its type is not supported by this spelling dialect.")
		def grouping[P, F <: FromSome, M[O] <: MappingAt[O]]
		            (grouping :GroupingRelation[F, M])
		            (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
			grouping.defaultSpelling(self)(from, context, params)


		/** Formats a column expression inside a ''select'' clause, together with an optional alias.
		  * The name of the column ''must'' be used as the alias, unless the name derived from the expression
		  * `column.`[[net.noresttherein.oldsql.sql.ColumnSQLMapping.expr expr]] is the same (in practice,
		  * if `column.expr` is a [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL GenericColumnComponentSQL]]
		  * for a column of a name equal to `column.name`). This `SQLSpelling` can however affect 'cosmetic' details
		  * of the spelling, such as capitalization, quoting, etc.
		  * @param column  a column from the [[net.noresttherein.oldsql.sql.Select.SelectTemplate.columns columns]] list
		  *                of the formatted
		  *                [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]/[[net.noresttherein.oldsql.sql.Select Select]]
		  * @param from    the ''from'' clause of the SQL ''select'' to whose ''select'' clause `column` belongs.
		  * @param context the list of tables in scope of the column expression; this includes all tables
		  *                in `from`.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters of the formatted SQL. It provides getter functions `P => X` for all parameters `X`
		  *                in the scope of the formatted expression, given
		  *                the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL.
		  *                Note that this means that rendering of a ''select'' clause requires the parameterization
		  *                created by the rendering of its ''from'' clause.
		  */
		@throws[InvalidSQLException]("if the column is a grouping expression with an illegal internal state " +
		                             "or a subexpression not supported by this spelling dialect.")
		def select[P, F <: RowProduct, V, O]
		          (column :TypedColumnSQLMapping[F, Grouped, V, O])
		          (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
			inSelect.inline(column.expr)(from, context, params) + _AS_ + column.name

		/** Renders as SQL a reference to column `column` of the relation `table` in the ''from'' clause
		  * of the formatted expression. This method terminates the recursion and simply returns the column name
		  * qualified with the alias for the given `JoinedTable`, as specified by `context` - unless the alias
		  * is an empty string, in which case the column name will not be qualified.
		  * If the active [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]] is
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.SelectScope SelectScope]], the column may
		  * optionally be rendered with an ''as'' clause. If the column is
		  * an [[net.noresttherein.oldsql.schema.support.AliasedColumn AliasedColumn]], its carried alias will be used,
		  * unless not unique. Alternatively, the alias may be generated by the passed `SQLContext`.
		  * The `table` argument
		  * must refer to a [[net.noresttherein.oldsql.schema.Table Table]] relation (in official SQL
		  * terminology this includes ''stored views'' and ''select'' expressions as well as ''base tables'') -
		  * if it is some kind of synthetic pseudo relation existing only in the framework, an exception will be thrown.
		  * @param table   an occurrence of a table in ''from'' clause `from`, containing the column. It must be
		  *                the exact relation present at `table.index` in `from` (i.e., it must be anchored in `from`).
		  * @param column  the formatted column; it must be an export column of the mapping `table.anchored`.
		  * @param from    the ''from'' clause of the (most nested) SQL ''select'' of which the formatted expression
		  *                is a part, containing `table`
		  *                (possibly as a part of its [[net.noresttherein.oldsql.sql.RowProduct.outer outer]] clause).
		  * @param context the list of tables in scope of the column expression; this includes all tables
		  *                in the ''from'' clause of the encompassing expression (even if under ''group by'' clause)
		  *                as well as any tables in the outer ''select''s and unbound parameters, in the order
		  *                consistent with their appearance in `F`. It is purely an 'in' parameter, included unchanged
		  *                in the returned `SpelledSQL`, as only [[net.noresttherein.oldsql.sql.FromClause FromClause]]
		  *                (including synthetic unbound parameters) can introduce new table aliases to scope.
		  * @param params  Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                parameters of the formatted SQL. It provides getter functions `P => X` for all parameters `X`
		  *                in the scope of the formatted expression, given
		  *                the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL.
		  *                Note that this means that rendering of a ''select'' clause requires the parameterization
		  *                created by the rendering of its ''from'' clause.
		  */
		@throws[IllegalArgumentException]("if the relation at the given index is synthetic and does not correspond " +
		                                  "to an actual table in the FROM clause.")
		@throws[IndexOutOfBoundsException]("if the context doesn't correspond to from clause F.")
		@throws[NoSuchComponentException]("if the column does not come from the mapping table.anchored.")
		def column[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
		          (table :JoinedTable[O, T], column :TypedColumn[_, O])
		          (from :F, context :SQLContext[P], params: Parameterization[P, F]) :SpelledSQL[P] =
			if (scope == SelectScope)
				scope.view.Extra.get(column) match {
					case Got(buff) =>
						apply(buff.expr)(from, context, params) //consider: an alias?
					case _ =>
						defaultSpelling(table, column)(from, context, params)
				}
			else
				defaultSpelling(table, column)(from, context, params)


		/** The name (alias) to use for a ''common table expression'' inside a ''with'' clause. */
		def name[P](cte :CommonTableExpression[MappingAt])(context :SQLContext[P]) :SpelledSQL[P] =
			context.nameOf(cte) match {
				case Some(name) => SpelledSQL(name, context)
				case _ =>
					val template = if (cte.isAnonymous) "cte" else cte.name
					var name = template
					var i = 0
					while (context.tableNames.values.exists(_ == name)) {
						name = template + "_" + i
						i += 1
					}
					if (!cte.isAnonymous && name == cte.name)
						SpelledSQL(name, context)
					else
						SpelledSQL(name, context.name(cte, name))
			}

		/** Dedicated names for operators combining two queries
		  * into a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL compound select]].
		  */
		def apply(operator :SelectOperator) :String = operator match {
			case Union => UNION
			case UnionAll => UNION_ALL
			case Intersect => INTERSECT
			case Minus => MINUS
			case _ => this.operator(operator.name)
		}

		/** Formats the given SQL literal. `SQLSpelling` returns the argument `sql` here,
		  * but subclasses can potentially change capitalization. This method is used by
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.TRUE TRUE]],
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.FALSE FALSE]],
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.NULL NULL]] and similar.
		  */
		def literal(sql :String) :String = sql

		/** Formats the given SQL operator (this involves arithmetic operators, but also
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.BETWEEN BETWEEN]] and
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.LIKE LIKE]], and subclasses can change letter case)
		  */
		def operator(sql :String) :String = sql

		/** Formats the given SQL function name. It is used both for standard SQL functions and custom stored functions.
		  * By default this method returns the argument as-is, but subclasses can change letter case, quote
		  * or even qualify the identifier.
		  */
		def function(sql :String) :String = sql

		/** Formats the given SQL keyword. This method is used for all constants such as ''insert'', ''update'',
		  * ''from'', and all such uppercase properties of this trait are initialized with it.
		  */
		def keyword(sql :String) :String = sql

		//keywords, constants, operators and standard functions

//		def isFormatting :Boolean  = false
//		def formatted :SQLSpelling = if (isFormatting) self else redact(new FormattedSpelling(_))
//		def formatted(indent :String) :SQLSpelling =
//			if (isFormatting)
//				throw new IllegalStateException("Cannot change indent for formatted spelling " + this + ".")
//			else redact(new FormattedSpelling(_, indent))
//
//		def >>  :SQLSpelling = this
//		def <<  :SQLSpelling = this

		def eol :String = ""
		def ` ` :String = ""

		def NULL         :String = literal("null")
		def TRUE         :String = literal("true")
		def FALSE        :String = literal("false")
		def CONCAT       :String = operator("+")
		def LIKE         :String = operator("like")
		def BETWEEN      :String = operator("between")
		def NOT          :String = operator("not")
		def AND          :String = operator("and")
		def OR           :String = operator("or")
		def XOR          :String = operator("xor")
		def UNION        :String = operator("union")
		def UNION_ALL    :String = operator("union all")
		def INTERSECT    :String = operator("intersect")
		def MINUS        :String = operator("minus")
		def CASE         :String = keyword("case")
		def WHEN         :String = keyword("when")
		def THEN         :String = keyword("then")
		def ELSE         :String = keyword("else")
		def END          :String = keyword("end")
		def WITH         :String = keyword("with")
		def SELECT       :String = keyword("select")
		def DISTINCT     :String = keyword("distinct")
		def FROM         :String = keyword("from")
		def WHERE        :String = keyword("where")
		def GROUP_BY     :String = keyword("group by")
		def HAVING       :String = keyword("having")
		def AS           :String = keyword("as")
		def INNER_JOIN   :String = keyword("join")
		def OUTER_JOIN   :String = keyword("outer join")
		def LEFT_JOIN    :String = keyword("left join")
		def RIGHT_JOIN   :String = keyword("right join")
		def ON           :String = keyword("on")
		def INSERT       :String = keyword("insert")
		def INTO         :String = keyword("into")
		def VALUES       :String = keyword("values")
		def UPDATE       :String = keyword("update")
		def SET          :String = keyword("set")
		def MERGE        :String = keyword("merge")
		def DELETE       :String = keyword("delete")

		def _NULL_       :String = " " + NULL + " "
		def _TRUE_       :String = " " + TRUE + " "
		def _FALSE_      :String = " " + FALSE + " "
		def _CONCAT_     :String = " " + CONCAT + " "
		def _LIKE_       :String = " " + LIKE + " "
		def _BETWEEN_    :String = " " + BETWEEN + " "
		def _NOT_        :String = " " + NOT + " "
		def _AND_        :String = " " + AND + " "
		def _OR_         :String = " " + OR + " "
		def _XOR_        :String = " " + XOR + " "
		def _UNION_      :String = " " + UNION + " "
		def _UNION_ALL_  :String = " " + UNION_ALL + " "
		def _INTERSECT_  :String = " " + INTERSECT + " "
		def _MINUS_      :String = " " + MINUS + " "
		def _SELECT_     :String = " " + SELECT + " "
		def _FROM_       :String = " " + FROM + " "
		def _WHERE_      :String = " " + WHERE + " "
		def _GROUP_BY_   :String = " " + GROUP_BY + " "
		def _HAVING_     :String = " " + HAVING + " "
		def _AS_         :String = " " + AS + " "
		def _INNER_JOIN_ :String = " " + INNER_JOIN + " "
		def _OUTER_JOIN_ :String = " " + OUTER_JOIN + " "
		def _LEFT_JOIN_  :String = " " + LEFT_JOIN + " "
		def _RIGHT_JOIN_ :String = " " + RIGHT_JOIN + " "
		def _ON_         :String = " " + ON + " "
		def _INTO_       :String = " " + INTO + " "
		def _VALUES_     :String = " " + VALUES + " "
		def _SET_        :String = " " + SET + " "
		def CASE_        :String = keyword("case") + " "
		def _WHEN_       :String = " " + keyword("when") + " "
		def _THEN_       :String = " " + keyword("then") + " "
		def _ELSE_       :String = " " + keyword("else") + " "
		def _END         :String = " " + keyword("end")
		def WITH_        :String = WITH + " "
		def SELECT_      :String = SELECT + " "
		def INSERT_      :String = INSERT + " "
		def UPDATE_      :String = UPDATE + " "
		def MERGE_       :String = MERGE + " "
		def DELETE_      :String = DELETE + " "

		/** The spelling strategy passed as the argument to all callback methods in the formatted classes.
		  * Defaults to `this`, but subclasses supporting entangled adapters (reapplied to any spelling instance
		  * returned for specific scopes) will override it with the applied adapter.
		  */
		override def self :SQLSpelling = this

		/** Switches spelling into an inline mode, in which multi-column expressions are never rendered as tuples,
		  * but as comma-separated columns (distinct expressions) without any delimiters. It is used when formatting
		  * tuple expressions of ''select'' clauses and function call arguments, but possibly also others.
		  * An ''inline'' speller should delegate to
		  * [[net.noresttherein.oldsql.sql.SQLExpression.explodedSpelling explodedSpelling]] rather than
		  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]] methods and be equivalent
		  * to using [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.explode this.explode]].
		  */
		def inline    :SQLSpelling

		/** If true, spelling of multi column expressions does not attempt to wrap them as a tuple,
		  * but simply list one after another, separated with ','.
		  */
		def isInline :Boolean

		/** The spelling scope specifying what kind of statement is being currently formatted (''insert'', ''update'',
		  * ''select'', etc) and what clause, with some additional provision for other spelling contexts. T
		  * his affects primarily which columns will be included by default
		  * for [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions, but can have
		  * also other effects. The scope is changed with [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.in in]]
		  * method or dedicated properties as the formatted expression/statement is recursively inspected.
		  */
		def scope :SpellingScope

		/** Switches the context of the spelling to the given clause. */
		def in(scope :SpellingScope) :SQLSpelling

		/** Initial spelling instance for top-level statements. */
		def top       :SQLSpelling = in(TopScope)
		/** Spelling instance to use for rendering ''with'' clauses. */
		def inWith    :SQLSpelling = in(WithScope)
		/** Spelling instance to use for rendering ''select'' clauses. */
		def inSelect  :SQLSpelling = in(SelectScope)
		/** Spelling instance to use for rendering (proper) ''from'' clauses. */
		def inFrom    :SQLSpelling = in(FromScope)
		/** Spelling instance to use for rendering ''where'' clauses. */
		def inWhere   :SQLSpelling = in(WhereScope)
		/** Spelling instance to use for rendering ''group by'' clauses. */
		def inGroupBy :SQLSpelling = in(GroupByScope)
		/** Spelling instance to use for rendering ''having'' clauses. */
		def inHaving  :SQLSpelling = in(HavingScope)
		/** Spelling instance to use for rendering ''values'', ''set'' and other dedicated clauses of ''insert'' statements. */
		def inInsert  :SQLSpelling = in(InsertScope)
		/** Spelling instance to use for rendering ''set'' clauses of SQL ''update'' statements.*/
		def inUpdate  :SQLSpelling = in(UpdateScope)
		/** Spelling instance to use for rendering arguments of calls to stored procedures and functions.*/
		def inCall    :SQLSpelling = in(CallScope)
		/** Spelling instance to use for rendering subexpressions in contexts such as operands in arithmetic operators.
		  * It is never used for expressions appearing on the top level, such as in a ''select'' clause,
		  * or assigned directly to database table columns in ''insert'' and ''update'' statements.
		  */
		def inOperand :SQLSpelling = in(OperandScope)

		protected def inQuery   :SQLSpelling = in(QueryScope)

		/** Spelling instance to use for the given `CompoundSelect` instance. This not only switches
		  * the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]
		  * to [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.QueryScope QueryScope]],
		  * but returns an instance whose [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]
		  * should be used for unifying the both sides of the ''compound select''.
		  * Left and right subqueries of a ''compound select'' should instead use
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.inLeft inLeft]] or
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.inRight inRight]], respectively.
		  */
//		def inQuery[P, R](query :CompoundSelect[P, R]) :SQLSpelling =
//			withReform(queryReform.subreform(query)(this))

		/** Spelling instance to use for the given `CompoundSelectSQL` instance.
		  * This not only switches the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]
		  * to [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.QueryScope QueryScope]],
		  * but returns an instance whose [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]
		  * should be used for unifying the both sides of the ''compound select''.
		  * Left and right subqueries of a ''compound select'' should instead use
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.inLeft inLeft]] or
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.inRight inRight]], respectively.
		  */
//		def inQuery[F <: RowProduct, R](query :CompoundSelectSQL[F, R]) :SQLSpelling =
//			withReform(queryReform.subreform(query)(this))

		/** Spelling instance to use for the left side of the given `CompoundSelect`.
		  * This sets the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]
		  * to [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.QueryScope QueryScope]],
		  * but returns an instance whose [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]
		  * has appropriate permissions for the left subquery. This method should be called on a spelling instance
		  * returned by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.inQuery[P,R]* inQuery]],
		  * or the spelling may have an improper query reform.
		  */
		def inLeft[P, R](query :CompoundSelect[P, R])  :SQLSpelling =
			withReform(queryReform.left(query)(self))

		/** Spelling instance to use for the right side of the given `CompoundSelect`.
		  * This sets the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]
		  * to [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.QueryScope QueryScope]],
		  * but returns an instance whose [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]
		  * has appropriate permissions for the left subquery. This method should be called on a spelling instance
		  * returned by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.inQuery[P,R]* inQuery]],
		  * or the spelling may have an improper query reform.
		  */
		def inRight[P, R](query :CompoundSelect[P, R]) :SQLSpelling =
			withReform(queryReform.right(query)(self))

		/** Spelling instance to use for the left side of the given `CompoundSelectSQL`.
		  * This sets the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]
		  * to [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.QueryScope QueryScope]],
		  * but returns an instance whose [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]
		  * has appropriate permissions for the left sub query. This method should be called on a spelling instance
		  * returned by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.inQuery[F<:RowProduct,R]* inQuery]],
		  * or the spelling may have an improper query reform.
		  */
		def inLeft[F <: RowProduct, R](query :CompoundSelectSQL[F, R])  :SQLSpelling =
			withReform(queryReform.left(query)(self))

		/** Spelling instance to use for the right side of the given `CompoundSelectSQL`.
		  * This sets the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]
		  * to [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.QueryScope QueryScope]],
		  * but returns an instance whose [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]
		  * has appropriate permissions for the left sub query. This method should be called on a spelling instance
		  * returned by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.inQuery[F<:RowProduct,R]* inQuery]],
		  * or the spelling may have an improper query reform.
		  */
		def inRight[F <: RowProduct, R](query :CompoundSelectSQL[F, R]) :SQLSpelling =
			withReform(queryReform.left(query)(self))
//
//		def inQuery[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q]) :SQLSpelling = {
//			val reform = queryReform.subreform(query)
//			if (scope == QueryScope && reform == queryReform) self
//			else decorate(QuerySpelling(reform))
//		}

//		/** Spelling used for the  */
//		def inLeft[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q]) :SQLSpelling = {
//			val reform = if (scope == TopScope) query.topReform else query.defaultReform
//			decorate(QuerySpelling(reform.left))
//		}
//
//		def inRight[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q]) :SQLSpelling = {
//			val reform = if (scope == TopScope) query.topReform else query.defaultReform
//			decorate(QuerySpelling(reform.right))
//		}

		/** [[net.noresttherein.oldsql.morsels.Decorable.decorate Decorates]] this instance in a proxy
		  * which substitutes [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]
		  * for the argument `reform` and switches the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]
		  * to [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.QueryScope QueryScope]].
		  */
		protected def withReform(reform :QueryReform) :SQLSpelling =
			if (scope == QueryScope && queryReform == reform) self
			else decorate(QuerySpelling(reform))

		/** The strategy used to unify the column sets between both sides of comparison expressions. */
		def reform :Reform = setterReform

		/** The strategy used to unify the column sets of left and right sides
		  * of a [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]].
		  */
		def setterReform :Reform = Reform.ReformDefaults

		/** The strategy used to unify select clauses of member selects in a compound select. */
		def queryReform  :QueryReform = QueryReform.bottomUp

		//fixme: outdated docs
		/** A reforming strategy used to unify the ''select'' clauses of member selects of an SQL ''compound select''.
		  * By default, [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]]`.`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.subreform subreform]]
		  * redirects here.
		  *
		  * The standard implementation returns
		  * `query.`[[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.topReform topReform]]
		  * if `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]] is
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.TopScope TopScope]], or
		  * `query.`[[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.defaultReform defaultReform]]
		  * otherwise.
		  * @param query a ''compound select'': either a [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]
		  *              or a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] which needs
		  *              unification of the ''select'' clauses of member ''selects''.
		  */
//		def compoundReform[V, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[V, Q]) :QueryReform =
//			//a sensible alternative would be query.operator.permit(reform)
//			if (scope == TopScope) query.topReform else query.defaultReform


		/** A hot spot allowing subclasses to inject specialized `SQLContext` implementations. */
		def newContext :SQLContext[Any] = SQLContext()


		//forwarding methods.
		//consider: rethrow will happen at each node of the expression tree. Do we really want it?
		/** The types of columns in the rows returned by this query.
		  * Forwards to `Query.`[[net.noresttherein.oldsql.sql.Query.shape shape]].
		  * Must stay consistent with spelled SQL for `query`.
		  */
		@throws[UndefinedShapeException]("if this expression is not anchored.")
		@throws[MisalignedExpressionException]("if the expression's internal state is inconsistent and has no definite column shape.")
		def shape(query :Query.__) :RowShape = query.`->shape`(self)

		/** The underlying column types of the argument expression, specific to this spelling's
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]].
		  * Forwards to `SQLExpression.`[[net.noresttherein.oldsql.sql.SQLExpression.shape shape]].
		  * Must stay consistent with spelled SQL for `expression`, providing the expression is anchored
		  * in the ''from'' clause given for spelling.
		  */
		//todo: shape(sql :SQLLayout[_]) :RowShape
		@throws[UndefinedShapeException]("if this expression is not anchored.")
		@throws[MisalignedExpressionException]("if the expression's internal state is inconsistent and has no definite column shape.")
		def shape(expression :SQLExpression.__) :RowShape = expression.`->shape`(self)

		/** Splits this SQL expression into individual [[net.noresttherein.oldsql.sql.ColumnSQL column]] expressions
		  * (of ''distinct'' types). This method will return
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]] columns, such that
		  * {{{
		  *     split(expression).map(apply(_)(from, ctx, params).toString) == explode(expression)(from, ctx, params).map(_.toString).
		  * }}}
		  * as long as `expression.`[[net.noresttherein.oldsql.sql.SQLExpression.isAnchored isAnchored]]`(from)`.
		  *
		  * Note that both the number of columns and expressions themselves can change
		  * when the expression is [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]]
		  * during the spelling process. This method should thus be used only in contexts where that anchoring is assured.
		  *
		  * The default implementation delegates to expression's [[net.noresttherein.oldsql.sql.SQLExpression.split split]]
		  * method.
		  */
		@throws[InseparableExpressionException]("if the expression cannot be separated into individual columns, " +
		                                        "for example a multi-column SQL select.")
		def split[F <: RowProduct, S >: Grouped <: Single, V]
		         (e :SQLExpression[F, S, V]) :Seq[ColumnSQL[F, S, _]] =
			rethrow { e.`->split`(self) } {
				"Failed to split expression " + e + " into individual columns in " + self.scope + "."
			}

		/** Splits the given l-value expression into column expressions for individual columns of mapping `M`,
		  * default in this spelling's current scope. This split is consistent with
		  * the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.explode exploded]] spelling of the expression.
		  * In the default implementation, the underlying column mappings of the returned expressions
		  * are the same as returned by
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]`.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]]`(e.anchored)`.
		  */
		@throws[InseparableExpressionException]("if the expression cannot be separated into individual columns, " +
		                                        "for example a multi-column SQL select.")
		def split[F <: RowProduct, M[O] <: MappingAt[O], V](e :LValueSQL[F, M, V]) :Seq[ColumnLValueSQL.from[F]#__] =
			rethrow { e.`->split`(self) } {
				"Failed to split component expression " + e + " into individual columns in " + self.scope + "."
			}

		/** Splits the given mapping expression into column expressions for individual columns of mapping `M`,
		  * default in this spelling's current scope. This split is consistent with
		  * the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.explode exploded]] spelling of the expression.
		  * In the default implementation, the underlying column mappings of the returned expressions
		  * are the same as returned by
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]`.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]]`(e.anchored)`.
		  */
		@throws[InseparableExpressionException]("if the expression cannot be separated into individual columns, " +
		                                        "for example a multi-column SQL select.")
		def split[F <: RowProduct, S >: Grouped <: Single, M[O] <: MappingAt[O], V]
		         (e :MappingSQL[F, S, M, V]) :Seq[ColumnMappingSQL.from[F]#rows[S]#__] =
			rethrow { e.`->split`(self) } {
				"Failed to split component expression " + e + " into individual columns in " + self.scope + "."
			}

		/** Number of columns in the given query, consistent with SQL returned
		  * by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.spell spell]]`(query)`.
		  * Defaults to `query.`[[net.noresttherein.oldsql.sql.SQLExpression.columnCount columnCount]]`(this)`,
		  * but any implementation overriding the former method should also override this one.
		  */
		@throws[UndefinedShapeException]("if this expression is not anchored.")
		@throws[MisalignedExpressionException]("if the expression's internal state is inconsistent and has no definite column shape.")
		def columnCount(query :Query.__) :Int =
			rethrow { query.`->columnCount`(self) } {
				"Failed to count the columns of query " + query + "."
			}

		/** Number of columns in the given expression, consistent with SQL returned by `apply(expression)`
		  * and [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.explode explode]]`(expression)`.
		  * Defaults to `expression.`[[net.noresttherein.oldsql.sql.SQLExpression.columnCount columnCount]]`(this)`,
		  * but any implementation overriding the former methods should also override this one.
		  * The value will be indicative of `expression` embedded in an SQL statement only if the argument
		  * is [[net.noresttherein.oldsql.sql.SQLExpression.isAnchored anchored]] in the ''from'' clause of the query.
		  * In particular, this method can throw an [[UnsupportedOperationException]] if the expression
		  * is not anchored at all (contains a [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]
		  * as a subexpression).
		  */
		//todo: review all reforming - we are always quitting if column counts are equal, regardless if columns are equal
		//todo: columnCount(sql :SQLLayout[_]) :Int
//		@throws[UnsupportedOperationException]("if this expression is not anchored.")
//		@throws[IllegalStateException]("if the expression's internal state is inconsistent and no definite column count exists.")
		@throws[UndefinedShapeException]("if this expression is not anchored.")
		@throws[MisalignedExpressionException]("if the expression's internal state is inconsistent and has no definite column shape.")
		def columnCount(expression :SQLExpression.__) :Int =
			rethrow { expression.`->columnCount`(self) } {
				"Failed to count the columns of expression " + expression + " used in " + self.scope + "."
			}

//		def columnTypes(expression :SQLExpression.*) :Opt[Seq[JDBCType]] =
//			rethrow { expression.`->columnTypes`(self) } {
//				"Failed to determine the column types of expression " + expression + " used in " + self.scope + "."
//			}

		/** Number of all individual JDBC parameters in the given expression when formatted as SQL,
		  * coming from both bound and unbound parameters. This assumes the expression is anchored
		  * in the ''from'' clause of the formatted ''select''.
		  * @return defaults to `expression.`[[net.noresttherein.oldsql.sql.SQLExpression.sqlParamCount sqlParamCount]].
		  */
		@throws[UnsupportedOperationException]("if this expression is not anchored.")
		def sqlParamCount(expression :SQLExpression.__) :Int =
			rethrow { expression.`->sqlParamCount`(self) } {
				"Failed to count SQL parameters in expression " + expression + " used in " + self.scope + "."
			}

		/** Number of all individual JDBC parameters in all expressions being a part of the given clause,
		  * coming from both bound and unbound parameters.
		  * @return defaults to `expression.`[[net.noresttherein.oldsql.sql.SQLExpression.sqlParamCount sqlParamCount]].
		  */
		def sqlParamCount(from :RowProduct) :Int =
			rethrow { from.`->sqlParamCount`(self) } {
				"Failed to count SQL parameters in clause " + from + "."
			}

		/** Number of all individual JDBC parameters in the declaration of the given relation. */
		def sqlParamCount(relation :Relation[MappingAt]) :Int =
			rethrow { relation.`->sqlParamCount`(self) } {
				"Failed to count SQL parameters in relation " + relation + " used in " + self.scope + "."
			}

		/** A forwarder method to
		  * [[net.noresttherein.oldsql.sql.RowProduct.groupingSpellingContext RowProduct.groupingSpellingContext]].
		  */
		@inline protected final def groupingSpellingContext[P]
		                            (from :RowProduct)
		                            (position :Int, context :SQLContext[P], params :Parameterization[P, from.Generalized])
				:GroupingSpellingContext[P] =
			rethrow { from.`->groupingSpellingContext`(position, context, params) } {
				"Failed to spell grouping expression #" + position + " in " + from +
					"; context = " + context + ", params = " + params + "."
			}



		/** A forwarder method to [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P] =
			rethrow { statement.`->defaultSpelling`(self) } {
				"Failed to spell statement " + statement + "."
			}

		/** A forwarder method to [[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P](e :Query[P, _])(context :SQLContext[P]) :SpelledSQL[P] =
			rethrow { e.defaultSpelling(self)(context) } {
				"Failed to spell query " + e + "."
			}

		/** A forwarder method to [[net.noresttherein.oldsql.sql.SQLExecutable.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P, F <: RowProduct, X, Y]
		                                           (f :SQLExecutable[X, Y])(args :SQLExpression[F, Grouped, X])
		                                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			rethrow { f.defaultSpelling(self)(args)(from, context, params) } {
				"Failed to spell function/procedure call " + f + "(" + args + ") in " + self.scope +
					". Spelling parameters: " + (from, context, params) + "."
			}

		/** A forwarder method to [[net.noresttherein.oldsql.sql.SQLExecutable.paramSpelling paramSpelling]]. */
		@inline protected final def paramSpelling[X, Y](f :SQLExecutable[X, Y]) :SpelledSQL[X] =
			rethrow { f.`->paramSpelling`(self) } {
				"Failed to spell parameterized call of function/procedure " + f + " in " + self.scope + "."
			}

		/** A forwarder method to [[net.noresttherein.oldsql.sql.AggregateFunction.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P, F <: RowProduct]
		                            (f :AggregateFunction)(arg :ColumnSQL[F, Grouped, _], distinct :Boolean = false)
		                            (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			rethrow { f.defaultSpelling(self)(arg, distinct)(from, context, params) } {
				"Failed to spell aggregate function call " + f + (if (distinct) "(distinct " else "(" ) + arg +
					") in " + self.scope + ". Spelling parameters: " + (from, context, params) + "."
			}

		/** A forwarder method to [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P, F <: RowProduct]
		                                           (e :SQLExpression[F, Grouped, _])
		                                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			rethrow { e.defaultSpelling(self)(from, context, params) } {
				"Failed to spell expression " + e + " in " + self.scope + "; parameters: " + (from, context, params) + "."
			}

		/** Invokes [[net.noresttherein.oldsql.sql.SQLExpression.explodedSpelling e.explodedSpelling]],
		  * concatenating the fragments. */
		final def inlineSpelling[P, F <: RowProduct](e :SQLExpression[F, Grouped, _])
		                                            (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			explodedSpelling(e, false)(from, context, params) match {
				case Seq() => SpelledSQL(context)
				case columns => columns.reduce(_ + ", " + _)
			}

		/** A forwarder method to [[net.noresttherein.oldsql.sql.SQLExpression.explodedSpelling explodedSpelling]]. */
		@inline protected final def explodedSpelling[P, F <: RowProduct]
		                                            (e :SQLExpression[F, Grouped, _], independent :Boolean = true)
		                                            (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:Seq[SpelledSQL[P]] =
			rethrow { e.explodedSpelling(self, independent)(from, context, params) } {
				"Failed to spell exploded expression " + e + " in " + self.scope + "; parameters: " +
					(from, context, params) + "."
			}

		/** A forwarder method to [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P](from :RowProduct) //this is called from apply(RowProduct) as the only action, maybe we could remove it?
		                                              (context :SQLContext[P],
		                                               params :Parameterization[P, from.Generalized])
				:SpelledSQL[P] =
			rethrow { from.defaultSpelling(self)(context, params) } {
				"Failed to spell clause " + from + "; parameters: " + (context, params) + "."
			}

		@inline protected final def defaultSpelling[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                                           (table :Table[M])
		                                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			rethrow { table.defaultSpelling(self)(from, context, params) } {
				"Failed to spell an occurrence of table " + table + "; parameters: " + (from, context, params) + "."
			}

		@inline protected final def defaultSpelling[P, F <: FromSome, M[O] <: MappingAt[O]]
		                                           (grouping :GroupingRelation[F, M])
		                                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			rethrow { grouping.defaultSpelling(self)(from, context, params) } {
				"Failed to spell a grouping expression " + grouping + "; parameters: " + (from, context, params) + "."
			}


		@inline protected final def defaultSpelling[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
		                                           (table :JoinedTable[O, T], column :TypedColumn[_, O])
		                                           (from :F, context :SQLContext[P], params: Parameterization[P, F])
				:SpelledSQL[P] =
			rethrow { table.table.defaultSpellingOf(self)(table, column)(from, context, params) } {
				"Failed to spell column " + column + " of table " + table + " (" + table.table +
					"); parameters: " + (from, context, params) + "."
			}
/*
		private[sql] final def defaultSpellingOf[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                                        (component :ComponentSQL[F, M], inline :Boolean = false)
		                                        (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] = ???
			component.defaultSpellingOf(this)(component, inline)(from, context, params)

		private[sql] final def explodedSpellingOf[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                                         (spelling :SQLSpelling)
		                                         (component :ComponentSQL[F, M])
		                                         (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:Seq[SpelledSQL[P]] = ???
//			explodedSpellingOf(component)(from, context, params)(spelling)
*/

		override def toString :String = innerClassNameOf(this)
	}



	object SQLSpelling {
		private val default = new DefaultSpelling(SelectScope)

		def apply() :SQLSpelling = default

		def apply(scope :SpellingScope) :SQLSpelling = new DefaultSpelling(scope)


		/** A base visitor class for the [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] class hierarchy
		  * for implementations which need to override the spelling of a particular expression type.
		  * Invokes [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]]
		  * (or [[net.noresttherein.oldsql.sql.SQLExpression.explodedSpelling inlineSpelling]]) on any expression,
		  * but the method for any node in the inheritance tree can be overriden to handle all its subclasses.
		  */
		class ExpressionSpelling[P, F <: RowProduct](spelling :SQLSpelling, inline :Boolean = false)
		                                            (from :F, context :SQLContext[P])
		                                            (implicit params :Parameterization[P, F])
			extends CaseAnyExpression[F, ({type T[-S >: Grouped <: Single, V] = SpelledSQL[P] })#T]
		{
			override def expression[S >: Grouped <: Single, X](e :SQLExpression[F, S, X]) :SpelledSQL[P] =
				if (inline) inlineSpelling(e)(context) else defaultSpelling(e)(context)

			protected def defaultSpelling(e :SQLExpression[F, Grouped, _])(context :SQLContext[P]) :SpelledSQL[P] =
				e.defaultSpelling(spelling)(from, context, params)

			protected def inlineSpelling(e :SQLExpression[F, Grouped, _])(context :SQLContext[P]) :SpelledSQL[P] =
				explodedSpelling(e)(context) match {
					case Seq() => SpelledSQL("", context)
					case columns => columns.reduce(_ +: ", " +: _)
				}

			protected def explodedSpelling(e :SQLExpression[F, Grouped, _])(context :SQLContext[P]) :Seq[SpelledSQL[P]] =
				e.explodedSpelling(spelling, false)(from, context, params)
		}


		//todo: move this somewhere else, either next to SQLContext and Parameterization or GroupByClause
		trait GroupingSpellingContext[P] {
			type Ungrouped <: FromSome
			type Grouping[O] <: MappingAt[O]

			val grouping :GroupingRelation[Ungrouped, Grouping]
			val from     :Ungrouped
			val context  :SQLContext[P]
			val params   :Parameterization[P, Ungrouped]
		}

		object GroupingSpellingContext {
			def apply[P, F <: FromSome, M[O] <: MappingAt[O]]
			         (grouping :GroupingRelation[F, M])(from :F, context :SQLContext[P], params :Parameterization[P, F])
					:GroupingSpellingContext[P] =
				new Impl(grouping)(from, context, params)

			private class Impl[P, F <: FromSome, M[O] <: MappingAt[O]]
			                  (val grouping :GroupingRelation[F, M])
			                  (val from :F, val context :SQLContext[P], val params :Parameterization[P, F])
				extends GroupingSpellingContext[P]
			{
				type Ungrouped = F
				type Grouping[O] = M[O]
			}
		}



		trait SpellingDefaults extends SQLSpelling {
			override def NULL         :String = literal("null")
			override def TRUE         :String = literal("true")
			override def FALSE        :String = literal("false")
			override def CONCAT       :String = operator("+")
			override def LIKE         :String = operator("like")
			override def BETWEEN      :String = operator("between")
			override def NOT          :String = operator("not")
			override def AND          :String = operator("and")
			override def OR           :String = operator("or")
			override def XOR          :String = operator("xor")
			override def UNION        :String = operator("union")
			override def UNION_ALL    :String = operator("union all")
			override def INTERSECT    :String = operator("intersect")
			override def MINUS        :String = operator("minus")
			override def CASE         :String = keyword("case")
			override def WHEN         :String = keyword("when")
			override def THEN         :String = keyword("then")
			override def ELSE         :String = keyword("else")
			override def END          :String = keyword("end")
			override def WITH         :String = keyword("with")
			override def SELECT       :String = keyword("select")
			override def DISTINCT     :String = keyword("distinct")
			override def FROM         :String = keyword("from")
			override def WHERE        :String = keyword("where")
			override def GROUP_BY     :String = keyword("group by")
			override def HAVING       :String = keyword("having")
			override def AS           :String = keyword("as")
			override def INNER_JOIN   :String = keyword("join")
			override def OUTER_JOIN   :String = keyword("outer join")
			override def LEFT_JOIN    :String = keyword("left join")
			override def RIGHT_JOIN   :String = keyword("right join")
			override def ON           :String = keyword("on")
			override def INSERT       :String = keyword("insert")
			override def INTO         :String = keyword("into")
			override def VALUES       :String = keyword("values")
			override def UPDATE       :String = keyword("update")
			override def SET          :String = keyword("set")
			override def MERGE        :String = keyword("merge")
			override def DELETE       :String = keyword("delete")

			override def _NULL_       :String = " " + NULL + " "
			override def _TRUE_       :String = " " + TRUE + " "
			override def _FALSE_      :String = " " + FALSE + " "
			override def _CONCAT_     :String = " " + CONCAT + " "
			override def _LIKE_       :String = " " + LIKE + " "
			override def _BETWEEN_    :String = " " + BETWEEN + " "
			override def _NOT_        :String = " " + NOT + " "
			override def _AND_        :String = " " + AND + " "
			override def _OR_         :String = " " + OR + " "
			override def _XOR_        :String = " " + XOR + " "
			override def _UNION_      :String = " " + UNION + " "
			override def _UNION_ALL_  :String = " " + UNION_ALL + " "
			override def _INTERSECT_  :String = " " + INTERSECT + " "
			override def _MINUS_      :String = " " + MINUS + " "
			override def _SELECT_     :String = " " + SELECT + " "
			override def _FROM_       :String = " " + FROM + " "
			override def _WHERE_      :String = " " + WHERE + " "
			override def _GROUP_BY_   :String = " " + GROUP_BY + " "
			override def _HAVING_     :String = " " + HAVING + " "
			override def _AS_         :String = " " + AS + " "
			override def _INNER_JOIN_ :String = " " + INNER_JOIN + " "
			override def _OUTER_JOIN_ :String = " " + OUTER_JOIN + " "
			override def _LEFT_JOIN_  :String = " " + LEFT_JOIN + " "
			override def _RIGHT_JOIN_ :String = " " + RIGHT_JOIN + " "
			override def _ON_         :String = " " + ON + " "
			override def _INTO_       :String = " " + INTO + " "
			override def _VALUES_     :String = " " + VALUES + " "
			override def _SET_        :String = " " + SET + " "
			override def CASE_        :String = keyword("case") + " "
			override def _WHEN_       :String = " " + keyword("when") + " "
			override def _THEN_       :String = " " + keyword("then") + " "
			override def _ELSE_       :String = " " + keyword("else") + " "
			override def _END         :String = " " + keyword("end")
			override def WITH_        :String = WITH + " "
			override def SELECT_      :String = SELECT + " "
			override def INSERT_      :String = INSERT + " "
			override def UPDATE_      :String = UPDATE + " "
			override def MERGE_       :String = MERGE + " "
			override def DELETE_      :String = DELETE + " "

		}


		trait SpellingConstants extends SQLSpelling {
			override val NULL         :String = literal("null")
			override val TRUE         :String = literal("true")
			override val FALSE        :String = literal("false")
			override val CONCAT       :String = operator("+")
			override val LIKE         :String = operator("like")
			override val BETWEEN      :String = operator("between")
			override val NOT          :String = operator("not")
			override val AND          :String = operator("and")
			override val OR           :String = operator("or")
			override val XOR          :String = operator("xor")
			override val UNION        :String = operator("union")
			override val UNION_ALL    :String = operator("union all")
			override val INTERSECT    :String = operator("intersect")
			override val MINUS        :String = operator("minus")
			override val CASE         :String = keyword("case")
			override val WHEN         :String = keyword("when")
			override val THEN         :String = keyword("then")
			override val ELSE         :String = keyword("else")
			override val END          :String = keyword("end")
			override val WITH         :String = keyword("with")
			override val SELECT       :String = keyword("select")
			override val DISTINCT     :String = keyword("distinct")
			override val FROM         :String = keyword("from")
			override val WHERE        :String = keyword("where")
			override val GROUP_BY     :String = keyword("group by")
			override val HAVING       :String = keyword("having")
			override val AS           :String = keyword("as")
			override val INNER_JOIN   :String = keyword("join")
			override val OUTER_JOIN   :String = keyword("outer join")
			override val LEFT_JOIN    :String = keyword("left join")
			override val RIGHT_JOIN   :String = keyword("right join")
			override val ON           :String = keyword("on")
			override val INSERT       :String = keyword("insert")
			override val INTO         :String = keyword("into")
			override val VALUES       :String = keyword("values")
			override val UPDATE       :String = keyword("update")
			override val SET          :String = keyword("set")
			override val MERGE        :String = keyword("merge")
			override val DELETE       :String = keyword("delete")

			override val _NULL_       :String = " " + NULL + " "
			override val _TRUE_       :String = " " + TRUE + " "
			override val _FALSE_      :String = " " + FALSE + " "
			override val _CONCAT_     :String = " " + CONCAT + " "
			override val _LIKE_       :String = " " + LIKE + " "
			override val _BETWEEN_    :String = " " + BETWEEN + " "
			override val _NOT_        :String = " " + NOT + " "
			override val _AND_        :String = " " + AND + " "
			override val _OR_         :String = " " + OR + " "
			override val _XOR_        :String = " " + XOR + " "
			override val _UNION_      :String = " " + UNION + " "
			override val _UNION_ALL_  :String = " " + UNION_ALL + " "
			override val _INTERSECT_  :String = " " + INTERSECT + " "
			override val _MINUS_      :String = " " + MINUS + " "
			override val CASE_        :String = CASE + " "
			override val _WHEN_       :String = " " + WHEN + " "
			override val _THEN_       :String = " " + THEN + " "
			override val _ELSE_       :String = " " + ELSE + " "
			override val _END         :String = " " + END
			override val _SELECT_     :String = " " + SELECT + " "
			override val _FROM_       :String = " " + FROM + " "
			override val _WHERE_      :String = " " + WHERE + " "
			override val _GROUP_BY_   :String = " " + GROUP_BY + " "
			override val _HAVING_     :String = " " + HAVING + " "
			override val _AS_         :String = " " + AS + " "
			override val _INNER_JOIN_ :String = " " + INNER_JOIN + " "
			override val _OUTER_JOIN_ :String = " " + OUTER_JOIN + " "
			override val _LEFT_JOIN_  :String = " " + LEFT_JOIN + " "
			override val _RIGHT_JOIN_ :String = " " + RIGHT_JOIN + " "
			override val _ON_         :String = " " + ON + " "
			override val _INTO_       :String = " " + INTO + " "
			override val _VALUES_     :String = " " + VALUES + " "
			override val _SET_        :String = " " + SET + " "
			override val WITH_        :String = WITH + " "
			override val SELECT_      :String = SELECT + " "
			override val INSERT_      :String = INSERT + " "
			override val UPDATE_      :String = UPDATE + " "
			override val MERGE_       :String = MERGE + " "
			override val DELETE_      :String = DELETE + " "
		}


		trait DelegatedSpellingConstants extends SQLSpelling {
			protected val decorated :SQLSpelling

			override def NULL         :String = decorated.NULL
			override def TRUE         :String = decorated.TRUE
			override def FALSE        :String = decorated.FALSE
			override def CONCAT       :String = decorated.CONCAT
			override def LIKE         :String = decorated.LIKE
			override def BETWEEN      :String = decorated.BETWEEN
			override def NOT          :String = decorated.NOT
			override def AND          :String = decorated.AND
			override def OR           :String = decorated.OR
			override def XOR          :String = decorated.XOR
			override def UNION        :String = decorated.UNION
			override def UNION_ALL    :String = decorated.UNION_ALL
			override def INTERSECT    :String = decorated.INTERSECT
			override def MINUS        :String = decorated.MINUS
			override def CASE         :String = decorated.CASE
			override def WHEN         :String = decorated.WHEN
			override def THEN         :String = decorated.THEN
			override def ELSE         :String = decorated.ELSE
			override def END          :String = decorated.END
			override def WITH         :String = decorated.WITH
			override def SELECT       :String = decorated.SELECT
			override def DISTINCT     :String = decorated.DISTINCT
			override def FROM         :String = decorated.FROM
			override def WHERE        :String = decorated.WHERE
			override def GROUP_BY     :String = decorated.GROUP_BY
			override def HAVING       :String = decorated.HAVING
			override def AS           :String = decorated.AS
			override def INNER_JOIN   :String = decorated.INNER_JOIN
			override def OUTER_JOIN   :String = decorated.OUTER_JOIN
			override def LEFT_JOIN    :String = decorated.LEFT_JOIN
			override def RIGHT_JOIN   :String = decorated.RIGHT_JOIN
			override def ON           :String = decorated.ON
			override def INSERT       :String = decorated.INSERT
			override def INTO         :String = decorated.INTO
			override def VALUES       :String = decorated.VALUES
			override def UPDATE       :String = decorated.UPDATE
			override def SET          :String = decorated.SET
			override def MERGE        :String = decorated.MERGE
			override def DELETE       :String = decorated.DELETE

			override def _NULL_       :String =  decorated._NULL_
			override def _TRUE_       :String =  decorated._TRUE_
			override def _FALSE_      :String =  decorated._FALSE_
			override def _CONCAT_     :String =  decorated._CONCAT_
			override def _LIKE_       :String =  decorated._LIKE_
			override def _BETWEEN_    :String =  decorated._BETWEEN_
			override def _NOT_        :String =  decorated._NOT_
			override def _AND_        :String =  decorated._AND_
			override def _OR_         :String =  decorated._OR_
			override def _XOR_        :String =  decorated._XOR_
			override def _UNION_      :String =  decorated._UNION_
			override def _UNION_ALL_  :String =  decorated._UNION_ALL_
			override def _INTERSECT_  :String =  decorated._INTERSECT_
			override def _MINUS_      :String =  decorated._MINUS_
			override def _SELECT_     :String =  decorated._SELECT_
			override def _FROM_       :String =  decorated._FROM_
			override def _WHERE_      :String =  decorated._WHERE_
			override def _GROUP_BY_   :String =  decorated._GROUP_BY_
			override def _HAVING_     :String =  decorated._HAVING_
			override def _AS_         :String =  decorated._AS_
			override def _INNER_JOIN_ :String =  decorated._INNER_JOIN_
			override def _OUTER_JOIN_ :String =  decorated._OUTER_JOIN_
			override def _LEFT_JOIN_  :String =  decorated._LEFT_JOIN_
			override def _RIGHT_JOIN_ :String =  decorated._RIGHT_JOIN_
			override def _ON_         :String =  decorated._ON_
			override def _INTO_       :String =  decorated._INTO_
			override def _VALUES_     :String =  decorated._VALUES_
			override def _SET_        :String =  decorated._SET_
			override def CASE_        :String =  decorated.CASE_
			override def _WHEN_       :String =  decorated._WHEN_
			override def _THEN_       :String =  decorated._THEN_
			override def _ELSE_       :String =  decorated._ELSE_
			override def _END         :String =  decorated._END
			override def WITH_        :String =  decorated.WITH_
			override def SELECT_      :String =  decorated.SELECT_
			override def INSERT_      :String =  decorated.INSERT_
			override def UPDATE_      :String =  decorated.UPDATE_
			override def MERGE_       :String =  decorated.MERGE_
			override def DELETE_      :String =  decorated.DELETE_
		}



		abstract class RedactedSpelling(override val scope :SpellingScope, override val isInline :Boolean)
		                               (protected val redactor :SQLSpelling => SQLSpelling)
			extends AbstractDecorable[SQLSpelling](redactor) with SQLSpelling
		{
			override val self :SQLSpelling = redactor(this)

			override def inline :SQLSpelling =
				if (self.isInline) self else copy(scope, true, redactor).self

			override def in(scope :SpellingScope) :SQLSpelling =
				if (scope == self.scope && self.isInline == scope.isInline) self
				else copy(scope, scope.isInline, redactor).self

			protected override def redecorate(decorators :SQLSpelling => SQLSpelling) :SQLSpelling =
				copy(scope, isInline, decorators).self

			protected def copy(scope :SpellingScope, inline :Boolean, wrap :SQLSpelling => SQLSpelling) :SQLSpelling
		}



		//consider: renaming to SpellingEditor
		abstract class SpellingRedactor(author :SQLSpelling)
			extends DelegatedSpellingConstants with Decorator[SQLSpelling]
		{
		    protected override val decorated :SQLSpelling = author

//			override def redact(redactor :SQLSpelling => SQLSpelling) :SQLSpelling = author.redact(redactor)

			override def spell[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P] = author.spell(statement)
			override def spell[P, V](query :Query[P, V]) :SpelledSQL[P] = author.spell(query)
			override def spell[V](query :QuerySQL[RowProduct, V]) :SpelledSQL[@~] = author.spell(query)

			override def apply[P](ctes :WithClause)
			                     (context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P] =
				author(ctes)(context, params)

			override def apply[P, V](query :Query[P, V])(context :SQLContext[P]) :SpelledSQL[P] = author(query)(context)

			override def apply[P, F <: RowProduct, V]
			                  (e :SQLExpression[F, Grouped, V])
			                  (from :F, context :SQLContext[P], params: Parameterization[P, F]) :SpelledSQL[P] =
				author(e)(from, context, params)

			override def explode[P, F <: RowProduct, V]
			                    (e :SQLExpression[F, Grouped, V], independent :Boolean = true)
			                    (from :F, context :SQLContext[P], params :Parameterization[P, F]) :Seq[SpelledSQL[P]] =
				author.explode(e, independent)(from, context, params)

			override def apply[P, F <: RowProduct]
			                  (function :AggregateFunction, distinct :Boolean)
			                  (arg :ColumnSQL[F, Grouped, _])
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author(function, distinct)(arg)(from, context, params)

			override def apply[P, F <: RowProduct, X, Y]
			                  (function :SQLExecutable[X, Y])(args :SQLExpression[F, Grouped, X])
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author(function)(args)(from, context, params)


			override def apply[P](from :RowProduct)
			                     (context :SQLContext[P], params :Parameterization[P, from.Generalized]) :SpelledSQL[P] =
				author(from)(context, params)

			override def fromWhere[P](from :FromClause)
			                         (context :SQLContext[P], params :Parameterization[P, from.Generalized]) :SpelledSQL[P] =
				author.fromWhere(from)(context, params)

			override def groupByHaving[P](from :GroupByClause)
			                             (context :SQLContext[P], params :Parameterization[P, from.Generalized])
					:SpelledSQL[P] =
				author.groupByHaving(from)(context, params)

			override def emptyFrom[P](context :SQLContext[P]) :SpelledSQL[P] = author.emptyFrom(context)

			override def join[P, L <: FromSome, R[O] <: MappingAt[O]]
			                 (join :L Join R, joinKind :String)
			                 (context :SQLContext[P], params :Parameterization[P, join.Generalized]) :SpelledSQL[P] =
				author.join(join, joinKind)(context, params)

			override def last[P, L <: RowProduct, T[A] <: MappingAt[A]]
			                 (from :L NonParam T)
			                 (context :SQLContext[P], params :Parameterization[P, from.Generalized]) :SpelledSQL[P] =
				author.last(from)(context, params)

			override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
			                  (table :Table[M])(from :F, context :SQLContext[P], params :Parameterization[P, F])
					:SpelledSQL[P] =
				author.table(table)(from, context, params)

			override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
			                  (table :Table[M], alias :String)
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author.table(table, alias)(from, context, params)

			override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
			                  (table :Table[M], alias :Option[String])
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author.table(table, alias)(from, context, params)

			override def alias[P, F <: RowProduct, M[O] <: MappingAt[O]]
			                  (table :Table[M], alias :Option[String])(from :F, context :SQLContext[P]) :String =
				author.alias(table, alias)(from, context)

			override def grouping[P, F <: FromSome, M[O] <: MappingAt[O]]
			                     (grouping :GroupingRelation[F, M])
			                     (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author.grouping(grouping)(from, context, params)


			override def select[P, F <: RowProduct, V, O]
			                   (column :TypedColumnSQLMapping[F, Grouped, V, O])
			                   (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author.select(column)(from, context, params)

			override def column[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
			                   (table :JoinedTable[O, T], column :TypedColumn[_, O])
			                   (from :F, context :SQLContext[P], params: Parameterization[P, F]) :SpelledSQL[P] =
				author.column(table, column)(from, context, params)

			override def name[P](cte :CommonTableExpression[MappingAt])(context :SQLContext[P]) :SpelledSQL[P] =
				author.name(cte)(context)

			override def apply(operator :SelectOperator) :String = operator match {
				case Union => UNION
				case UnionAll => UNION_ALL
				case Intersect => INTERSECT
				case Minus => MINUS
				case _ => self.operator(operator.name)
			}

			override def literal(sql :String) :String = author.literal(sql)
			override def operator(sql :String) :String = author.operator(sql)
			override def function(sql :String) :String = author.function(sql)
			override def keyword(sql :String) :String = author.keyword(sql)

			override def isInline :Boolean = author.isInline
			override def scope :SpellingScope = author.scope

			override def in(scope :SpellingScope) :SQLSpelling = author.in(scope)

//			override def self      :SQLSpelling = author.self
			override def inline    :SQLSpelling = author.inline
			override def top       :SQLSpelling = author.top
			override def inWith    :SQLSpelling = author.inWith
			override def inSelect  :SQLSpelling = author.inSelect
			override def inFrom    :SQLSpelling = author.inFrom
			override def inWhere   :SQLSpelling = author.inWhere
			override def inGroupBy :SQLSpelling = author.inGroupBy
			override def inHaving  :SQLSpelling = author.inHaving
			override def inInsert  :SQLSpelling = author.inInsert
			override def inUpdate  :SQLSpelling = author.inUpdate
			override def inCall    :SQLSpelling = author.inCall
			override def inOperand :SQLSpelling = author.inOperand
			override def inQuery   :SQLSpelling = author.inQuery

//			override def inQuery[P, R](query :CompoundSelect[P, R]) :SQLSpelling = author.inQuery(query)
//			override def inQuery[F <: RowProduct, R](query :CompoundSelectSQL[F, R]) :SQLSpelling = author.inQuery(query)
			override def inLeft[P, R](query :CompoundSelect[P, R]) :SQLSpelling = author.inLeft(query)
			override def inRight[P, R](query :CompoundSelect[P, R]) :SQLSpelling = author.inRight(query)
			override def inLeft[F <: RowProduct, R](query :CompoundSelectSQL[F, R]) :SQLSpelling = author.inLeft(query)
			override def inRight[F <: RowProduct, R](query :CompoundSelectSQL[F, R]) :SQLSpelling = author.inRight(query)

//			override def inLeft[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q]) :SQLSpelling =
//				author.inLeft(query)
//
//			override def inRight[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q]) :SQLSpelling =
//				author.inRight(query)

/*
			override def isFormatting :Boolean = author.isFormatting
			override def formatted(indent :String) :SQLSpelling =
				if (isFormatting) author.formatted(indent).decorate(decorate)
				else author.decorate(new FormattedSpelling(_, indent))

			override def >> :SQLSpelling = if (isFormatting) author.>>.decorate(decorate) else self
			override def << :SQLSpelling = if (isFormatting) author.<<.decorate(decorate) else self
			override def eol :String = author.eol
			override def ` ` :String = author.` `
*/


			override def reform       :Reform = author.reform
			override def setterReform :Reform = author.setterReform
			override def queryReform  :QueryReform = author.queryReform
//			override def queryReform[V, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[V, Q]) :Reform =
//				author.queryReform(query)

			override def newContext :SQLContext[Any] = author.newContext

			override def split[F <: RowProduct, S >: Grouped <: Single, V]
			                  (e :SQLExpression[F, S, V]) :Seq[ColumnSQL[F, S, _]] =
				author.split(e)

			override def split[F <: RowProduct, M[O] <: MappingAt[O], V]
			                  (e :LValueSQL[F, M, V]) :Seq[ColumnLValueSQL.from[F]#__] = author.split(e)

			override def shape(query :Query.__) :RowShape = author.shape(query)
			override def shape(expression :SQLExpression.__) :RowShape = author.shape(expression)
			override def columnCount(query :Query.__) :Int = author.columnCount(query)
			override def columnCount(expression :SQLExpression.__) :Int = author.columnCount(expression)

			override def sqlParamCount(expression :SQLExpression.__) :Int = author.sqlParamCount(expression)
			override def sqlParamCount(from :RowProduct) :Int = author.sqlParamCount(from)
			override def sqlParamCount(relation :Relation[MappingAt]) :Int = author.sqlParamCount(relation)
		}


/*
		class FormattedSpelling(author :SQLSpelling, indent :String = "  ", indentLevel :Int = 0)
			extends SpellingRedactor(author)
		{
			private[this] val currentIndent = indent * indentLevel
			override def copy(author :SQLSpelling) :SQLSpelling = new FormattedSpelling(author, indent, indentLevel)

			override def isFormatting = true
			override def formatted :SQLSpelling = self
			override def formatted(indent :String) :SQLSpelling =
				if (indent == this.indent) this else new FormattedSpelling(author, indent, indentLevel)
			def >>(decorators :SQLSpelling => SQLSpelling) =
				author.recreate(new FormattedSpelling(_, indent, indentLevel - 1) andThen decorators)
			override def >> :SQLSpelling = author, indent, indentLevel + 1)
			override def << :SQLSpelling = new FormattedSpelling(author, indent, indentLevel - 1)

			override def eol :String = "\n"
			override def ` ` :String = currentIndent
		}
*/

		def QuerySpelling(queryReform :QueryReform)(spelling :SQLSpelling) :SQLSpelling =
			if (spelling.scope == QueryScope && spelling.queryReform == queryReform)
				spelling
			else spelling match {
				case reforming :QuerySpelling =>
					new QuerySpelling(reforming.decorated, queryReform)
				case _ =>
					new QuerySpelling(spelling, queryReform)
			}

		class QuerySpelling(override val decorated :SQLSpelling, override val queryReform :QueryReform)
			extends SpellingRedactor(decorated) with SQLSpelling //for super access
		{
			protected override def decorate(decorated :SQLSpelling) :SQLSpelling =
				QuerySpelling(queryReform)(decorated)

			override def scope   :SpellingScope = QueryScope

			//consider: should we wrap decorated.in(scope) in another decorator to preserve expression reform?
			override def in(scope :SpellingScope) :SQLSpelling = scope match {
				case QueryScope => self
				case _ => decorated.in(scope)
			}
//			override def inQuery :SQLSpelling = self


//			override def inQuery[P, R](query :CompoundSelect[P, R]) :SQLSpelling = super[SQLSpelling].inQuery(query)
//			override def inQuery[F <: RowProduct, R](query :CompoundSelectSQL[F, R]) :SQLSpelling =
//				super[SQLSpelling].inQuery(query)

			override def inLeft[P, R](query :CompoundSelect[P, R]) :SQLSpelling = super[SQLSpelling].inLeft(query)
			override def inRight[P, R](query :CompoundSelect[P, R]) :SQLSpelling = super[SQLSpelling].inRight(query)

			override def inLeft[F <: RowProduct, R](query :CompoundSelectSQL[F, R]) :SQLSpelling =
				super[SQLSpelling].inLeft(query)

			override def inRight[F <: RowProduct, R](query :CompoundSelectSQL[F, R]) :SQLSpelling =
				super[SQLSpelling].inRight(query)

			protected override def withReform(reform :QueryReform) :SQLSpelling =
				if (queryReform == reform) self
				else if (self eq this) decorated.undecorate(QuerySpelling(reform))
				else decorate(QuerySpelling(reform) _)
//
//			override def inLeft[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q]) :SQLSpelling =
//				if (self == this) decorated.undecorate(QuerySpelling(queryReform.left(query)(this)))
//				else super[SQLSpelling].inLeft(query)
//
//			override def inRight[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q]) :SQLSpelling =
//				if (self == this) decorated.undecorate(QuerySpelling(queryReform.right(query)(this)))
//				else super[SQLSpelling].inRight(query)

			override def undecoratedEquals(that :SQLSpelling) :Boolean = that match {
				case reforming :QuerySpelling if reforming undecoratedCanEqual this =>
					decorated == reforming.decorated && queryReform == reforming.queryReform
				case _ => false
			}

			override def toString :String = "QuerySpelling(" + decorated + "," + queryReform + ")"
		}


	}




	/** An SQL renderer which delegates to `defaultSpelling` methods of
	  * [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling DMLStatement]],
	  * [[net.noresttherein.oldsql.sql.Query.defaultSpelling Query]],
	  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling SQLException]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling RowProduct]],
	  * [[net.noresttherein.oldsql.schema.Table.defaultSpelling Table]] and others, doing little by itself
	  * that is not required by the contract of [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * One added feature is ensuring table aliases are unique, by treating `alias` parameters to
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.table table]] methods as proposed, rather than required
	  * values. All tables are always aliased, using a generated name if none is given, unless an empty string
	  * is explicitly passed as the alias. An additional cosmetic function is applying preferred capitalization
	  * to various syntactical elements.
	  * @param literals  The capitalization to use for literal expressions (defaults to `LowerCase`).
	  * @param operators The capitalization to use for operators (infix functions such as 'and', defaults to `LowerCase`).
	  * @param functions The capitalization to use for function and procedure names (defaults to `LowerCase`).
	  * @param keywords  The capitalization to use for keywords (defaults to `LowerCase`).
	  * @param aliases   The capitalization to use for aliases given to tables and columns (defaults to `LowerCase`).
	  * @param scope     The syntactic context of expression arguments corresponding to an SQL clause
	  *                  (''select'' clause, ''where'' clause, etc.).
	  * @param isInline  Should multi-column expressions be rendered as individual comma-separated columns rather
	  *                  than tuples or stand-alone expressions? This is used to flatten the tree structure of
	  *                  SQL expressions and format `SQLExpression` instances representing SQL fragments larger
	  *                  than a single expression, for example a whole or fragment of ''select'' or ''group by'' clause.
	  */
	class DefaultSpelling(literals :TextCase = LowerCase, operators :TextCase = LowerCase,
	                      functions :TextCase = LowerCase, keywords :TextCase = LowerCase,
	                      aliases :TextCase = LowerCase)
	                     (override val scope :SpellingScope, override val isInline :Boolean = false,
	                      protected override val redactor :SQLSpelling => SQLSpelling = identity[SQLSpelling] _)
		extends RedactedSpelling(scope, isInline)(redactor)
	{
		def this(scope :SpellingScope, textCase :TextCase, isInline :Boolean, redactor :SQLSpelling => SQLSpelling) =
			this(textCase, textCase, textCase, textCase, textCase)(scope, isInline, redactor)

		def this(scope :SpellingScope, textCase :TextCase, isInline :Boolean) =
			this(scope, textCase, isInline, identity[SQLSpelling] _)

		def this(scope :SpellingScope, isInline :Boolean, redactor :SQLSpelling => SQLSpelling) =
			this(scope, LowerCase, isInline, redactor)

		def this(scope :SpellingScope, redactor :SQLSpelling => SQLSpelling) = this(scope, scope.isInline, redactor)
		def this(scope :SpellingScope, isInline :Boolean) = this(scope, LowerCase, isInline)
		def this(scope :SpellingScope) = this(scope, LowerCase, scope.isInline)


		protected override def copy(scope :SpellingScope, inline :Boolean, wrap :SQLSpelling => SQLSpelling) :SQLSpelling =
			new DefaultSpelling(literals, operators, functions, keywords, aliases)(scope, inline, wrap)
				with DelegatedSpellingConstants
			{
				override val decorated = DefaultSpelling.this
			}

		def newLine :String = "" //todo: multi line formatting, if possible
		def indent(depth :Int) :String = ""

		override def apply[P, F <: RowProduct, V]
                          (e :SQLExpression[F, Grouped, V])
                          (from :F, context :SQLContext[P], params: Parameterization[P, F]) :SpelledSQL[P] =
			if (isInline) inlineSpelling(e)(from, context, params) else defaultSpelling(e)(from, context, params)


		override def join[P, L <: FromSome, R[O] <: MappingAt[O]]
		                 (join :L Join R, joinKind :String)
		                 (context :SQLContext[P], params :Parameterization[P, join.Generalized]) :SpelledSQL[P] =
		{
			val left = apply(join.left :join.left.type)(context, params.left)
			val right = last(join)(left.context, params)
			val sql = left.sql + (" " + joinKind + " ") + right.sql
			val joined = SpelledSQL(sql, left.setter + right.setter, right.context)
			if (join.condition == True)
				joined
			else if (useJoinOnClause)
				joined + _ON_ + inWhere(join.condition)(join.generalized, joined.context, params)
			else {
				val context = joined.context && inWhere(join.condition)(join.generalized, joined.context, params)
				SpelledSQL(joined.sql, joined.setter, context)
			}
		}

		protected def useJoinOnClause :Boolean = true
		protected val indexedTableAliasRoot   :String = aliases("table")
		protected val arbitraryTableAliasRoot :String = aliases("relation")

		override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M])(from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			(table :Relation[M]) match {
				case t :NamedRelation[M] => self.table(table, t.name)(from, context, params)
				case _ =>
					//we could try to check if the select returns a single entity to provide a more informative alias
					val default = indexedTableAliasRoot + context.aliasStack.length
					if (!context.contains(default)) self.table(table, default)(from, context, params)
					else self.table(table, arbitraryTableAliasRoot)(from, context, params)
			}

		override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M], alias :String)
		                  (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
		{
			val sql = defaultSpelling(table)(from, context, params)
			if (alias.length == 0)
				SpelledSQL(sql.sql, sql.setter, context.join(""))
			else {
				val unique =
					if (!context.contains(alias)) alias
					else context.newAlias(alias)
				SpelledSQL(sql.sql + (_AS_ + unique), sql.setter, context.join(unique))
			}
		}
		//overriden due to Scala's stupid overloading rules
		override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M], alias :Option[String])
		                  (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			alias.mapOrElse(self.table(table, _)(from, context, params), self.table(table)(from, context, params))

		override def alias[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M], alias :Option[String])(from :F, context :SQLContext[P]) :String =
			alias match {
				case Some("") => ""
				case Some(name) => context.newAlias(name)
				case _ => (table :Relation[M]) match {
					case t :NamedRelation[_] if t.name.length == 0 => ""
					case t :NamedRelation[M] => context.newAlias(t.name)
					case _ =>
						//we could try to check if the select returns a single entity to provide a more informative alias
						val default = indexedTableAliasRoot + context.aliasStack.length
						if (!context.contains(default)) default
						else context.newAlias(arbitraryTableAliasRoot)
				}
			}


		override def literal(sql :String) :String = literals(sql)
		override def operator(sql :String) :String = operators(sql)
		override def function(sql :String) :String = functions(sql)
		override def keyword(sql :String) :String = keyword(sql)

		override def toString :String =
			"DefaultSpelling(" + scope + (if (isInline && !scope.isInline) ", inline)" else ")")
	}


	object DefaultSpelling {
		def apply(literals :TextCase = LowerCase, operators :TextCase = LowerCase,
		          functions :TextCase = LowerCase, keywords :TextCase = LowerCase, aliases :TextCase = LowerCase)
		         (scope :SpellingScope, isInline :Boolean = false,
		          redactor :SQLSpelling => SQLSpelling = identity[SQLSpelling]) :DefaultSpelling =
			new DefaultSpelling(literals, operators, functions, keywords, aliases)(scope, isInline, redactor)
				with SpellingConstants
	}

	//todo: WYSIWYG (no clever SQL transformations) and Strict (no/limited reforms) modes.
}






/** Implementation of the SQL standard formatting all SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
  * and DML [[net.noresttherein.oldsql.sql.DMLStatement statements]] using
  * a [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy of its
  * [[net.noresttherein.oldsql.sql.StandardSQL!.spelling spelling]] property. Unless overriden, all formatting methods
  * delegate the generation of SQL to the formatted objects themselves in double dispatch, most notably to
  * methods [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling SQLExpression.defaultSpelling]]
  * and [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling DMLStatement.defaultSpelling]].
  * The [[net.noresttherein.oldsql.sql.SQLDialect$ companion]] object to this class extends it, serving
  * as the default dialect used by all methods unless an implicit instance is present.
  * @see [[net.noresttherein.oldsql.sql.SQLDialect.DefaultSpelling DefaultSpelling]]
  */
class StandardSQL extends SQLDialect {

	/** The effective spelling strategy used by this dialect. All other methods are implemented by delegating
	  * to the appropriate method of this strategy. Standard implementation returns a cached
	  * `new `[[net.noresttherein.oldsql.sql.SQLDialect.DefaultSpelling DefaultSpelling]]([[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.TopScope TopScope]])
	  * but subclasses are not restricted to always returning the same instance when overriding this method.
	  */
	def spelling :SQLSpelling = StandardSQL.spellTop

	override def apply[R, Y](query :QuerySQL[RowProduct, R])(implicit result :StatementResult[R, Y]) :Cantrip[Y] =
		chant(spelling.spell(query).compose { _ :Any => @~ }, result)

	override def apply[X, R, Y](query :Query[X, R])(implicit result :StatementResult[R, Y]) :Incantation[X, Y] =
		chant(spelling.spell(query), result)
//
//	override def apply[X <: Chain, Y](call :FunctionSQL[RowProduct, GlobalScope, X, Y]) :Cantrip[Y] =
//		chant(spelling.spell(call).compose { _ :Any => @~ }, StatementResult.SingleResult(call.readForm))

	override def apply[X, Y](statement :DMLStatement[X, Y]) :Incantation[X, Y] =  //todo: CallableStatement
		chant(spelling.spell(statement), statement.result)

	override def apply[X, Y](statement :Call[X, Y]) :Incantation[X, Y] = {
		val sql = spelling.spell(statement)
		Incantation.call(sql.sql.toString)(sql.setter, statement.result)
	}

	/** The default factory method creating incantations from complete SQL/DML of the translated statement
	  * and a strategy for reading the required values from the [[java.sql.PreparedStatement PreparedStatement]]
	  * result(s). It is the delegation target of all `apply` methods in this dialect except for
	  * [[net.noresttherein.oldsql.sql.Call Call]] statements which require a specific incantation implementation
	  * creating a [[java.sql.CallableStatement CallableStatement]] instead of regular `PreparedStatement`
	  * in order to read (optional) ''OUT'' parameters.
	  */
	protected def chant[X, Y](sql :SpelledSQL[X], result :StatementResult[Nothing, Y]) :Incantation[X, Y] =
		Incantation(sql.sql.toString)(sql.setter, result)

//	private[this] val action = new StatementVisitor[Incantation] with CaseAnyStatement[Incantation] {
//		override def statement[X, Y](stmt :DMLStatement[X, Y]) = StandardSQL.this(stmt)
//	}
}



/** The default formatter of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] and
  * DML [[net.noresttherein.oldsql.sql.DMLStatement statements]], converting them
  * to [[net.noresttherein.oldsql.sql.Incantation incantations]] executing
  * [[java.sql.PreparedStatement PreparedStatement]]s for their generated textual representations.
  * Generated SQL conforms to the standard for maximum portability and, on principle, defers the translation of
  * each expression and syntactic element to the appropriate class representing it in the abstract syntax tree,
  * most notably methods [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling SQLExpression.defaultSpelling]]
  * and [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling DMLStatement.defaultSpelling]], so minor
  * specific modifications can be performed by extending individual classes in the framework.
  *
  * This is ''not'' an implicit value; instead methods accepting - typically implicitly - a dialect provide
  * it as a default value used if no implicit instance is available.
  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]]
  * @see [[net.noresttherein.oldsql.morsels.witness.Maybe Maybe]]
  */
case object StandardSQL extends StandardSQL {
	private class StandardSpelling(override val scope :SpellingScope, override val isInline :Boolean,
	                               redact :SQLSpelling => SQLSpelling = identity[SQLSpelling])
		extends DefaultSpelling(scope, isInline, redact)
	{
		def this(scope :SpellingScope) = this(scope, scope.isInline)

		override val inline    = if (isInline) self             else copy(scope, true, redactor).self
		override def top       = if (self eq this) spellTop     else copy(TopScope)
		override def inWith    = if (self eq this) spellWith    else copy(WithScope)
		override def inSelect  = if (self eq this) spellSelect  else copy(SelectScope)
		override def inFrom    = if (self eq this) spellFrom    else copy(FromScope)
		override def inWhere   = if (self eq this) spellWhere   else copy(WhereScope)
		override def inGroupBy = if (self eq this) spellGroupBy else copy(GroupByScope)
		override def inHaving  = if (self eq this) spellHaving  else copy(HavingScope)
		override def inInsert  = if (self eq this) spellInsert  else copy(InsertScope)
		override def inUpdate  = if (self eq this) spellUpdate  else copy(UpdateScope)
		override def inCall    = if (self eq this) spellCall    else copy(CallScope)
		override def inOperand = if (self eq this) spellOperand else copy(OperandScope)
		override def inQuery   = if (self eq this) spellQuery   else copy(QueryScope)

		override def in(scope :SpellingScope) :SQLSpelling = scope match {
			case _ if !(self eq this) => copy(scope)
			case TopScope     => top
			case WithScope    => inWith
			case SelectScope  => inSelect
			case FromScope    => inFrom
			case WhereScope   => inWhere
			case GroupByScope => inGroupBy
			case HavingScope  => inHaving
			case InsertScope  => inInsert
			case UpdateScope  => inUpdate
			case CallScope    => inCall
			case OperandScope => inOperand
			case QueryScope   => spellQuery
			case _ => copy(scope)
		}
		override val newContext :SQLContext[Any] = SQLContext()

		private def copy(scope :SpellingScope) :SQLSpelling = copy(scope, scope.isInline, redactor).self

		protected override def copy(scope :SpellingScope, inline :Boolean, wrap :SQLSpelling => SQLSpelling) :SQLSpelling =
			new StandardSpelling(scope, inline, wrap) with DelegatedSpellingConstants {
				override val decorated = StandardSpelling.this
			}

		override def undecoratedEquals(that :SQLSpelling) :Boolean = that match {
			case other :StandardSpelling => (this eq other) || scope == other.scope && isInline == other.isInline
			case _ => false
		}
		override def undecoratedHashCode :Int = (getClass.hashCode * 31 + scope.hashCode) * 31 + isInline.hashCode

		override def toString :String =
			"StandardSpelling(" + scope + (if (isInline && !scope.isInline) ", inline)" else ")")
	}

	private class PresetSpelling(scope :SpellingScope) 
		extends StandardSpelling(scope, scope.isInline, identity) with SpellingConstants
	
	private val spellTop     = new PresetSpelling(TopScope)
	private val spellQuery   = new PresetSpelling(QueryScope)
	private val spellWith    = new PresetSpelling(WithScope)
	private val spellSelect  = new PresetSpelling(SelectScope)
	private val spellFrom    = new PresetSpelling(FromScope)
	private val spellWhere   = new PresetSpelling(WhereScope)
	private val spellGroupBy = new PresetSpelling(GroupByScope)
	private val spellHaving  = new PresetSpelling(HavingScope)
	private val spellInsert  = new PresetSpelling(InsertScope)
	private val spellUpdate  = new PresetSpelling(UpdateScope)
	private val spellCall    = new PresetSpelling(CallScope)
	private val spellOperand = new PresetSpelling(OperandScope)
}


package com.hcore.ogre.sql

import com.hcore.ogre.mapping.ComponentPath.\**\
import com.hcore.ogre.mapping.Mapping.ColumnFilter.{ForUpdate, ForSelect}
import com.hcore.ogre.mapping.Mapping.{TypedMapping, MappingWriteForm}
import com.hcore.ogre.mapping.MappingMorphism.ValueMorphism
import com.hcore.ogre.mapping.bits.MappingMatchers.{SingleColumn, AsColumn, EmptyColumns}
import com.hcore.ogre.mapping._
import com.hcore.ogre.morsels.InverseIndexSeq
import com.hcore.ogre.slang.matching.Unapply
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.RowSource._
import com.hcore.ogre.sql.SetComponent.{UpdateForm, UpdateComponent}
import com.hcore.ogre.sql.SQLFormula._
import com.hcore.ogre.sql.SQLForm.{EmptyForm, NoneForm, UnitForm}
import com.hcore.ogre.sql.SQLStatement.ParameterizedStatement
import com.hcore.ogre.sql.SQLWriteForm.ProxySQLWriteForm
import com.hcore.ogre.sql.Update.{UpdateCount, UpdateTable}


import scala.collection.breakOut

//implicits
import extensions._
import InverseIndexSeq.implicitIndexing


trait UpdateTemplate[X, Y] extends SQLStatementTemplate[X, Y] {
	def table :AnyMapping
	def columns :Seq[Mapping[_]]


	override def apply(param :X) :SQLStatement[Y] = new UpdateTable[X, Y](this, param)
}


trait UpdateEntityTemplate[X] extends UpdateTemplate[X, UpdateCount] {
	def table :Mapping[X]
}


/** A builder of update SQLStatement instances as well as their factories (SQLTemplate instances).
  * Statements and templates are built by chaining calls providing additional information about parts of final statement,
  * for example:
  * <code>(Update table Targets on (_.id==_.id))(target)</code> - creates an 'update by id' template and then a statement for a particular entity, or
  * <code>Update(Targets) where (_.distance<=100) set (_.legal := true)</code> - creates an update by specifying the filter and set clause directly.
  */
object Update {

	type UpdateCount = Int
	type Setter[T<:AnyMapping, X] = UpdateComponent[From[T] WithParam X, T, _<:AnyMapping, _, _]


	/** Create a builder of update statements for the table specified by the given table.
	  * Returned builder provides methods allowing to specify the 'set' and 'where' clauses of the statement and creates
	  * an SQLStatement directly, omitting the template (parameterized factory of updates) step.
	  * @param table mapping representing updated table, which sqlName is equal to the qualified name of the table
	  * @tparam E mapped entity type
	  * @return an update builder which allows creating an update statement by chained calls.
	  */
	def apply[E](table: Mapping[E]): UpdateBuilder[table.type, E] =
		new UpdateBuilder[table.type, E](table)


	/** Create a builder of update templates (parameterized SQLStatement factories for update statements) for the given table.
	  * Returned builder provides methods allowing to specify the 'set', 'where' and 'returning' clauses of the statement as well
	  * as change the parameter type of the template.
	  * @param table mapping representing the updated table, which sqlname is equal to the qualified name of the updated table.
	  * @tparam E mapped entity type and default parameter type of the template.
	  * @return a builder of 'update <code>table</code>' templates which can create the template by chained method calls.
	  */
	def table[E](table :Mapping[E]) :UpdateTemplateBuilder[table.type, E] =
		new UpdateTemplateBuilder[table.type, E](table)

	/** Create a builder of update statements updating all rows in the table (with empty where clause).
	  * Equivalent to Update(table).all
	  * @param table mapping representing updated table, which sqlName is equal to the qualified name of the table
	  * @tparam E mapped entity type
	  * @return an update builder which allows creating an update statement by chained calls.
	  */
	def all[E](table :Mapping[E]) :UpdateWhereBuilder[table.type, E] = Update(table).all

	/** Create a builder of update templates parameterized with type X. All expressions being a part of created updates will
	  * be grounded in From[T] WithParam X, and thus both assigned values and filter selecting rows for update can use
	  * both columns (or components) of table T and a value of parameter X.
	  * @tparam X
	  * @return
	  */
	def by[X] :ParameterizedUpdateTemplateBuilder[X] = parameterizer.asInstanceOf[ParameterizedUpdateTemplateBuilder[X]]

	/** Update template builder which by default will create templates parameterized with type X */
	class ParameterizedUpdateTemplateBuilder[X] {

		/** Create a template builder for update statements for the given table. Equivalent to (Update table table).by[X].
		  * @param table mapping for the updated table with sqlName equal to table's fully qualified name
		  * @tparam E mapped row (entity) type of the udpated table
		  * @return a template builder which can be used to create update templates for the given table accepting parameter X
		  */
		def table[E](table :Mapping[E]) :WithParamUpdateTemplateBuilder[table.type, E, X] = (Update table table).by[X]
	}
	private[this] val parameterizer = new ParameterizedUpdateTemplateBuilder[Nothing]




	/** First step of chained build process of parameterless update statements - requires either defining the filter condition
	  * specifying which rows to update, or explicitly requesting an update of all rows to limit accidental 'update all rows'
	  * statements. Update templates created by this builder will have all their expressions
	  * (contained in 'set', 'where' and 'returning' clauses) grounded in From[T] row source, meaning they won't rely on any external
	  * parameters or tables. While the finally produced statement can contain sql parameters, their values have to be known during
	  * creation of the particular SQLFormula and have to be represented by a BoundParameter instance.
	  * @param table table being updated
	  * @tparam T mapping type representing the updated table
	  * @tparam E mapped row (entity) type
	  */
	class UpdateBuilder[T<:Mapping[E], E](val table :T) extends AnyVal {

		/** Create a direct builder of update statements for updating rows specified by the given condition.
		  * All update statements created by the returned builder will have their 'where' clause equal to the result of
		  * the passed filter function applied to the updated table.
		  * @param condition A function returning a boolean formula where all expressions referring to columns of T use
		  *                  the TableFormula instance passed as the argument.
		  * @return a builder of 'update <code>table</code> set ... where <code>condition</code>' statements
		  */
		def where(condition :TableFormula[From[T], T]=>BooleanFormula[From[T]]) :UpdateWhereBuilder[T, E] =
			new UpdateWhereBuilder(From(table) where (t => condition(t.last)))

		/** Create a builder of update statemetns udpating all rows in the given table.
		  * @return a builder class allowing to specify updated columns by chained method calls.
		  */
		def all :UpdateWhereBuilder[T, E] =
			where(_ => True())
	}



	/** Second step of chained direct build process of update statements, allowing to specify the columns of the table
	  * to be updated and their new values.
	  * @param source a RowSource consisting of the single updated table.
	  * @tparam T mapping type of the updated table
	  * @tparam E mapped row (entity) type
	  */
	class UpdateWhereBuilder[T<:Mapping[E], E](val source :From[T]) extends AnyVal {
		//todo: returning

		/** Specify components of the table T to be updated and their assigned values.
		  * each assignment is given as an ComponentUpdate instance returned by an argument function when applied to
		  * the TableFormula for the updated table and represents a pair of (left side, right side).
		  * An ComponentUpdate instance can be obtained by calling <code>comp := value</code>, where comp is a component formula representing the left side
		  * (which has to specify a component of the table given as the argument to the function) and value is any sql formula sql compatible type
		  * (for example Option[X] if type comp is X) representing the right side. While the expression on the left side of the assignment can be any component,
		  * not necessarily a column, the formula on the right side would then have to be either statically known.
		  * Assigning a formula depending on columns (or components) of the updated table to a non-column component will result in an exception,
		  * possibly later in the build process.
		  * @param assignments functions creating ComponentUpdates specifying the columns to be included in the set clause of the update
		  * @return an update <code>table<code> set c1=v1, ..., cn=vn where <code>condition</code>
		  *         where (c1,...,cn) are all updatable columns of the left sides of the given assignments,
		  *         and (v1,...,vn) are formulas returning values for a particular column from a formula on the right side of the originating assignment.
		  */
		def set(assignments :(TableFormula[From[T], T]=>UpdateComponent[From[T], T, _<:AnyMapping, _, _])*) :UpdateTable[Unit, UpdateCount] =
			(template /: assignments)((t, setter) => t.set(setter.asInstanceOf[TableFormula[From[T], T]=>UpdateComponent[From[T], T, AnyMapping, Any, Any]]))()

		/** Specifies the components of the table T to be updated with values taken from the given ComponentValues instance.
		  * Returns an update statement using row filter specified when creating this instance and including all columns
		  * of the given components in the 'set' clause.
		  * @param components components components (possibly of more than one column) to be updated.
		  *                   If a component consists of more than one column, all columns specified as updatable by default on this component
		  *                   will be included in the updated column list.
		  * @param values ComponentValues[T] which should contain (or be able to assemble) values for all specified components
		  * @return an update <code>table<code> set c1=v1, ..., cn=vn where <code>condition</code>
		  *         where (c1,...,cn) = components.flatMap(_.updatable) and vi = values(ci)
		  */
		def set(components :Seq[T#Component[_]], values :ComponentValues[T]) :UpdateTable[Unit, UpdateCount] =
			(template /: components){
				case (update, column) =>
					val col = column.asInstanceOf[T#Component[Any]]
					update.set(col, values.get(col))
			}()

		/** Create an update statement updating all columns of table T which are updatable by default with values
		  * contained in the specified ComponentValues.
		  */
		def set(values :ComponentValues[T]) :UpdateTable[Unit, UpdateCount] =
			set(source.last.mapping.updatable, values)


		/** Create an update statement updating all columns of table T which are updatable by default with values obtained
		  * from the value
		  * @param value value mapped to the updated table
		  */
		def set(value :E) :UpdateTable[Unit, UpdateCount] = set(ComponentValues.Predefined[T](source.mapping, value))


		private def template[X, V, C <: AnyMapping]: NoParamUpdateWhereTemplate[T, E] =
			new NoParamUpdateWhereTemplate[T, E](source.withParam[Unit], Seq())

	}




	/** A builder class of update statements for a particular table, which allows to specify the row filter selecting
	  * the rows to be updated. By default the parameter type of the created template (and thus also the row filter)
	  * is assumed to be the type mapped by the table T - E, but this can be changed by calling 'by[X]' before any other methods.
	  * @param table mapping for  the table being updated with sqlName equal to fully qualified table name
	  * @tparam T type of the mapping of the updated table
	  * @tparam E mapped row (entity) type of the updated table and default parameter (input) type of the created statements
	  */
	class UpdateTemplateBuilder[T <: Mapping[E], E](val table: T) extends AnyVal {

		/** Create a builder of update templates for the given table using the specified type as its parameter.
		  * @tparam X parameter type of created template, which has to contain both
		  *           the values required to update the columns of the table, and any values needed by the row filter.
		  * @return builder of 'update <code>table</code>' templates using X as parameter.
		  */
		def by[X]: WithParamUpdateTemplateBuilder[T, E, X] = new WithParamUpdateTemplateBuilder(table)

		/** Create an 'update <code>table</code> where <code>filter</code>' template, where filter is the result of applying the argument function
		  * to a row source representing updated table and parameter entity E.
		  * Returned template will by default create update statements where the 'set' clause contains all columns of the given table which
		  * are updatable by default. This can be changed by calling 'set' on this template,
		  * which will override default values and start creating a new set clause from scratch.
		  * @param condition a function creating the boolean condition to be used as the row filter in the 'where' clause of the created update statements.
		  * @return a template of 'update <code>table</code> where <code>condition</code> set c1=v1,...,cn=vn',
		  *         where c1,...,cn are all updatable columns of the given table and their values are obtained from the value E of the whole mapped row by
		  *         a querying the table mapping for a value for the given column.
		  */
		def where(condition: RowTables[From[T] WithParam E] => BooleanFormula[From[T] WithParam E]): DefaultUpdateWhereEntityTemplate[T, E] =
			new DefaultUpdateWhereEntityTemplate[T, E](table, condition)

		/** An alternative to this.where which takes a function accepting two arguments: the updated table and an artificial table representing template parameter E.
		  *
		  * @param condition a function creating the booolean condition to be used as the row filter in the 'where' clause of the created update statements.
		  * @return a template of 'update <code>table</code> where <code>condition</code> set c1=v1,...,cn=vn',
		  *         where c1,...,cn are all updatable columns of the given table and their values are obtained from the value E of the whole mapped row by
		  *         a querying the table mapping for a value for the given column.
		  */
		def on(condition: (TableFormula[From[T] WithParam E, T], RowSourceParam[From[T] WithParam E, E]) => BooleanFormula[From[T] WithParam E]): DefaultUpdateWhereEntityTemplate[T, E] =
			where(t => condition(t.table[T], t.param[E]))

		/** Create a template for updating all rows in the table (i.e. statements without a 'where' clause) to the single given value E.
		  * This most probably is not what you want unless the table consists of a single row.
		  * @return a template for 'update <code>table</code> set c1=v1,...,cn=vn'
		  *         where c1,...,cn are all updatable columns of the given table and their values are obtained from the value E of the whole mapped row by
		  *         a querying the table mapping for a value for the given column.
		  */
		def all: DefaultUpdateWhereEntityTemplate[T, E] =
			where(_ => True())
	}







	/** A builder class for update templates taking X as their parameter, which allows to specify the row filter selecting
	  * the rows to be updated. The parameter type X is a source for both the values of updated columns, and the row selector
	  * of the 'where' clause of produced statements. All formulas in the resulting update sql have to be grounded in
	  * a row source From[T] WithParam[X] containing two 'tables' - actual updated table T and artificial parameter table of value X.
	  *
	  * @param table mapping for  the table being updated with sqlName equal to fully qualified table name
	  * @tparam T type of the mapping of the updated table
	  * @tparam E mapped row (entity) type of the updated table and default parameter (input) type of the created statements
	  * @tparam X input type of this template.
	  */
	class WithParamUpdateTemplateBuilder[T <: Mapping[E], E, X](val table: T) extends AnyVal {

		/** Create an 'update <code>table</code> where <code>filter</code>' template, where filter is the result of applying the argument function
		  * to a row source representing updated table and parameter entity E.
		  * Returned template doesn't contain a 'set' clause and is therefore invalid.
		  * This can be changed by invoking its 'set' methods.
		  * @param condition a function creating the boolean condition to be used as the row filter in the 'where' clause of the created update statements.
		  * @return a template for 'update <code>table</code> where <code>condition</code>' statements, where set clause can be specified
		  *         by chained calls to 'setTo' methods.
		  */
		def where(condition: RowTables[From[T] WithParam X] => BooleanFormula[From[T] WithParam X]): UpdateWhereTemplate[T, E, X] =
			new UpdateWhereTemplate[T, E, X](table, condition)

		/** An alternative to this.where which takes a function accepting two arguments: the updated table and an artificial table representing template parameter X.
		  *
		  * @param condition a function creating the booolean condition to be used as the row filter in the 'where' clause of the created update statements.
		  * @return a template of 'update <code>table</code> where <code>condition</code>' which can create templates with non-empty
		  *         set clauses by calling its 'set' methods.
		  */
		def on(condition: (TableFormula[From[T] WithParam X, T], RowSourceParam[From[T] WithParam X, X]) => BooleanFormula[From[T] WithParam X]): UpdateWhereTemplate[T, E, X] =
			where(t => condition(t.table[T], t.param[X]))

		/** Create a template for updating all rows in the table (i.e. statements without a 'where' clause) to the same value obtained from X.
		  * This most probably is not what you want unless the table consists of a single row.
		  * @return a template for 'update <code>table</code>' which allows to specify the set clause by chained calls to its 'set' methods.
		  */
		def all: UpdateWhereTemplate[T, E, X] =
			where(_ => True())
	}





	/** Parameterless template for update statements of table T.
	  * A no-argument apply() method will create an SQLStatement
	  * 'update <code>table</code> set <code>s1, ..., sn</code> where <code>filter</code>'
	  * where table and filter are specified by source, and (s1, ..., sn) = setters.
	  * The 'set' clause can be expended by subsequent calls to 'set' methods of this instance,
	  * which will return a template with the new setters appended to the setters of this instance.
	  * @param source source combining the updated table and updated row selector is its filter condition
	  * @param setters ComponentUpdate instances specifying which components/columns of the table should be included
	  *                in the 'set' clause and what their values should be.
	  * @tparam T mapping type of the updated table
	  * @tparam E mapped row (entity) type of the updated table
	  */
	class NoParamUpdateWhereTemplate[T<:Mapping[E], E](
			val source :From[T] WithParam Unit, protected val setters :Seq[Setter[T, Unit]])
		extends UpdateTemplate[Unit, UpdateCount] with AbstractUpdateTableTemplate[T, E, Unit, UpdateCount]
	{
		/** Create an update template with an empty 'set' clause, useful only as a base for appending setters. */
		def this(table :T, condition :TableFormula[From[T], T] => BooleanFormula[From[T]]) =
			this(filterTable[T, Unit](table, t => condition(t.source.left.last)), Seq())

		import com.hcore.ogre.sql.SQLFormula.TermFormulas

		/** Create a parameterless update template for the same table and where clause, and the set clause
		  * containing the given assignment prepended to this instance setter list. The assignment can be either to a column,
		  * or a multi-column component, in which case it will result in individual items for all columns of that component which are updatable by default.
		  * In that case, assigned value has to be statically known (not dependant on columns of T) or an exception will occur (possibly at a later point).
		  * @param assignment a function creating an sql assignment based on the given table by calling c := value,
		  *                   where c is a ComponentFormula of the argument of the function, and value is an sql formula
		  *                   of sql-compatible type with the value type of the component c
		  * @tparam C type of the component (a column or multi-column) to include in the set list.
		  * @tparam V type of the right side of assignment
		  * @tparam X 'lifted' type after auto converting left and right sides of the assignment to the same scala type.
		  * @return a parameterless update template expanding this template by adding
		  *         col_i = val_i for all updatable columns col_i of the passed assignment
		  *         and values val_i are sql formulas equivalent to getting the value of the given column from the formula on the right side of the given assignment.
		  */
		def set[C<:AnyMapping, V, X](assignment :TableFormula[From[T], T]=>UpdateComponent[From[T], T, C, V, X]) :NoParamUpdateWhereTemplate[T, E] =
			new NoParamUpdateWhereTemplate(source, setter(assignment) +: setters)

		/** Create a parameterless update template for the same table and where clause as this instance, and the set clause
		  * containing all columns of the given component, assigned the values obtained from the provided component value, prepended
		  * before this instance setters. If the component is not a column, all columns specified by its updatable list will be listed
		  * in the created template.
		  * @param component a component of the updated table to be updated.
		  * @param value a value for the updated component - None will result in setting null for all updatable columns.
		  * @tparam C type of the mapping representing the updated component
		  * @tparam X mapped type of the component
		  * @return a parameterless update template expanding this template by adding
		  *         <code>component.col1=v1, ..., component.coln=vn</code> to the set clause of this template.
		  */
		def set[C<:T#Component[X], X](component :C, value :Option[X]) :NoParamUpdateWhereTemplate[T, E] =
			set(t => (t :\ component) := BoundParameter(value)(component.updateForm.asOpt && component.selectForm.asOpt))

		/** Create an sql update statement in the form of 'update <code>table</code> set <code>s1, ..., sn</code> where <code>filter</code>',
		  * where table and filter are specified by source, and (s1, ..., sn) = setters
		  */
		def apply() :UpdateTable[Unit, UpdateCount] = new UpdateTable(this, ())


		/** Create a paramerless update statement based on this template, but returning the value of the given table component as the result.
		  * Created template will have a 'returning (c1, ..., cn)' clause, where c1,...,cn are columns of the specified component
		  * @param component a component which value (possibly generated/updated by the database) should be returned by
		  *                  statements created by the returned template
		  * @tparam Y mapped type of the component
		  * @return an sql for 'update <code>source.last</code> where <code>source.filter</code> set <code>setters</code> returning (c1,..,cn)'
		  *         where c1,...cn are all selectable by default columns of the given component.
		  */
		def returning[Y](component :T#Component[Y]) :SQLStatement[Y] =
			returning(_ :\ component)

		/** Create a parameterless update statement based on this template, but returning the value of the given formula as the result.
		  * Created template will have a 'returning <code>formula</code>' clause, where formula is the result of transforming
		  * the value created by the argument function into sql in the same way as it would in a select statement header
		  * (using selectable columns of all components included in the formula).
		  * @param expression a function which creates an sql formula to be returned, which may refer to the new values of the updated row
		  *                   by creating an appropriate ComponentFormula from the argument table.
		  * @tparam Y the result type of the 'returning' expression and the return type of the created statement.
		  * @return an sql for 'update <code>source.last</code> where <code>source.filter</code> set <code>setters</code> returning <code>expression(source.last)</code>'
		  */
		def returning[Y](expression :TableFormula[From[T], T] => SQLFormula[From[T], Y]) :SQLStatement[Y] =
			returning(expression(source.left.last))

		/** Create a parameterless update statement based on this template, but returning the value of the given formula as the result.
		  * Created template will have a 'returning <code>formula</code>' clause, where formula is the result of transforming
		  * the value created by the argument function into sql in the same way as it would in a select statement header
		  * (using selectable columns of all components included in the formula).
		  * @param expression an sql formula grounded in this instance's source (i.e. where all referenced components of table T are grounded in this.source.last)
		  *                   to be used as the 'returning' clause of the created statement.
		  * @return an sql for 'update <code>source.last</code> where <code>source.filter</code> set <code>setters</code> returning <code>expression</code>'
		  */
		def returning[Y](expression :SQLFormula[From[T], Y]) :SQLStatement[Y] =
			(new UpdateWhereReturningTemplate[T, E, Unit, Y](source, setters, expression.asPartOf[From[T], From[T] WithParam Unit](source)))(())


		protected def setter[C<:AnyMapping, V, X](assignment :TableFormula[From[T], T]=>UpdateComponent[From[T], T, C, V, X]) :Setter[T, Unit] = {
			val ass = assignment(source.left.last)
			implicit val equalizer = ass.lift
//			val extended = ass.component.table.asPartOf[From[T], From[T] WithParam Unit](source) \\ ass.component.path

			ass.path =: ass.value.asPartOf[From[T], From[T] WithParam Unit](source)
		}

		override val input = super.input //SQLWriteForm.chain(setterForms ++: whereForms)
		override val output: SQLReadForm[UpdateCount] = SQLForm[UpdateCount]
		override val sql = updateSQL
	}
	
	
	
	
	/** A template for sql updates in the form of 'update <code>table</code> where <code>filter</code> set <code>assignments</code>',
	  * where table and filter are specified by this.source, and assignments is a sequence of ComponentUpdate instances
	  * grounded in this.source, specifying the components of the updated table to be set and their values as formulas grounded in
	  * the updated table and this template's parameter X.
	  * @param source source comprised of the updated table and an artificial 'table' representing template's parameter X
	  * @param assignments a sequence of ComponentUpdate instances created by c := v, where c is a ComponentFormula[From[T] WithParam X, T, C]
	  *                    and value v is an SQLFormula[From[T] WithParam X, V] for any type V which is sql-compatible with the mapped type of component C on the left side.
	  * @tparam T mapping type of the updated table
	  * @tparam E mapped row (entity) type of the updated table
	  * @tparam X template parameter type, providing the values for updated columns and input for the 'where' clause.
	  */
	class UpdateWhereTemplate[T <: Mapping[E], E, X] protected (
			val source: From[T] WithParam X,
			protected val assignments: Seq[Setter[T, X]])
		extends UpdateTemplate[X, UpdateCount] with AbstractUpdateTableTemplate[T, E, X, UpdateCount]
	{
		def this(table: T, condition: RowTables[From[T] WithParam X] => BooleanFormula[From[T] WithParam X]) =
			this(filterTable(table, condition), Seq())

		/** A list of setters to be included in the created statements' set clauses.
		  * By default it is equal to this.assignments, but overriding it can be used to provide 'default' set clause different from (usually empty) this.assignments.
		  * @return setters for components to be updated by created statements.
		  */
		override def setters = assignments

		override val table = source.left.right

		/** Create an update template for the same source (table and row filter) as this instance
		  * with the 'set' clause containing all setters specified by this.assignments and the result of the passed function.
		  * The setter specified as the argument can be for a column or a multi-column component, but in the latter case,
		  * its value should not depend on any column values of T (i.e. be either statically known at this point or dependent solely on template parameter X).
		  * @param assignment a function creating a set clause items by invoking c := v for some c :ComponentFormula[From[T] WithParam X, T, C]
		  *                   and v :SQLFormula[From[T] WithParam X, V] where V is sql-compatible with C#ResultType.
		  * @return an update template for 'update <code>table</code> where <code>filter</code> set <code>assignment(source), this.setters</code>'.
		  */
		def setTo(assignment: RowTables[From[T] WithParam X] => UpdateComponent[From[T] WithParam X, T, _<:AnyMapping, _, _]): UpdateWhereTemplate[T, E, X] =
			copy(assignment(source.asTables[From[T] WithParam X]) +: assignments)

		/** Create an update template equivalent to this template, but returning the value of the given component as its result.
		  * Produced statements will have a 'returning (c1, ..., cn) where c1,...,cn are selectable columns of component.
		  * @param component a component which value should be returned, based on the values of the row after udpate
		  *                  (probably containing values updated/created by the database)
		  * @tparam Y mapped type of the component and return type of created statements
		  * @return an update template for statements with a returning clause containing all selectable columns of component
		  */
		def returning[Y](component :T#Component[Y]) :UpdateWhereReturningTemplate[T, E, X, Y] =
			returning(_[T] :\ component)

		/** Create an update template equivalent to this template, but returning the value of the given component as its result.
		  * Produced statements will have a 'returning expr' clause where expr is obtained by creating an sql out of value created by expression as it would occur
		  * in a select statement header (i.e using selectable columns of all referenced components).
		  * @param expression a function, which based on the tables in this source (where _.last is the artificial table for parameter X and _.prev is table T),
		  *                   creates an expression which value should be returned by the update statement.
		  * @tparam Y return type of the created update statements and the type of the formula in 'returning' clause.
		  * @return an update template for statements with a returning clause consisting of the expression created by the argument function.
		  */
		def returning[Y](expression :RowTables[From[T] WithParam X] => SQLFormula[From[T] WithParam X, Y]) :UpdateWhereReturningTemplate[T, E, X, Y] =
			returning(expression(source.asTables[From[T] WithParam X]))

		/** Create an update template equivalent to this template, but returning the value of the given component as its result.
		  * Produces statements will have a 'returning expr' clause where expr is obtained by creating an sql out of passed expression as it would occur
		  * in a select statement header (i.e using selectable columns of all referenced components).
		  * @param expression an sql formula grounded in this.source (where source.last is the artificial table for parameter X and source.prev is updated table T),
		  *                   to be specified as the 'returning' clause of created update statements.
		  * @tparam Y return type of the created update statements and the type of the formula in 'returning' clause.
		  * @return an update template for statements with a returning clause consisting of the expression created by the argument function.
		  */
		def returning[Y](expression :SQLFormula[From[T] WithParam X, Y]) :UpdateWhereReturningTemplate[T, E, X, Y] =
			new UpdateWhereReturningTemplate(source, setters, expression)


		protected def copy(assignments: Seq[UpdateComponent[From[T] WithParam X, T, _<:AnyMapping, _, _]]): UpdateWhereTemplate[T, E, X] =
			new UpdateWhereTemplate[T, E, X](source, assignments)


		override val input = super.input //SQLWriteForm.chain(setterForms ++: whereForms)
		override val output = SQLForm[UpdateCount]
		override val sql = updateSQL
	}

	
	
	

	/** A template for update statements which take as their parameter the mapped type of the updated table.
	  * Will create statements in the form of 'update <code>table</code> where <code>filter</code> set <code>assignments</code>',
	  * where table and filter are specified by this.source, and assignments is a non-empty sequence of ComponentUpdate instances
	  * grounded in this.source, specifying the components of the updated table to be set and their values as formulas grounded in
	  * the updated table and this template's parameter X.
	  * @param src source consisting of updated table and template parameter E
	  * @param asses assignments to be used as the 'set' clause of created udpate statemetns.
	  * @tparam T mapping type of the updated table
	  * @tparam E mapped row (entity) type of the updated table
	  */
	class UpdateWhereEntityTemplate[T <: Mapping[E], E] protected(
			src: From[T] WithParam E, asses: Seq[Setter[T, E]])
		extends UpdateWhereTemplate[T, E, E](src, asses) with UpdateEntityTemplate[E]
	{
		/** Create an <b>invalid</b> template with empty set clause to be used as a basis for actual templates by calling its 'set' or 'setTo' methods.
		  * @param table updated table
		  * @param condition filter condition to be passed to created row source and used as the 'where' clause of created statements
		  */
		def this(table: T, condition: RowTables[From[T] WithParam E] => BooleanFormula[From[T] WithParam E]) =
			this(filterTable(table, condition), Seq())

		override def setTo(assignment: RowTables[From[T] WithParam E] => UpdateComponent[From[T] WithParam E, T, _<:AnyMapping, _, _]): UpdateWhereEntityTemplate[T, E] =
			copy(assignment(source.asTables[From[T] WithParam E]) +: assignments)

		/** Create a template based on this instance, with the set clause containing assignments for all updatable by default
		  * columns of passed components and all setters passed explicitly before. Note that the new set clause will be a super set
		  * of this template's set clause only if it is not a 'default' instance, i.e. was created by explicitly calling 'set' or 'setTo'
		  * methods. The values for updated columns will be obtained by querying the table mapping for a value for the specific component
		  * from parameter value E (mapped type of T).
		  * @param components a list of components (either columns or multi-column components) to be included in the set clause.
		  * @return an update template with the same table and 'where' clause as this instance, and containing setters
		  *         specified by this.assignments prepended by all updatable columns of all components in the argument list.
		  */
		def set(components: Iterable[T#Component[_]]): UpdateWhereEntityTemplate[T, E] = {
			val t = source.asTables[From[T] WithParam E]
			val assignments = components.map { comp =>
				val path = (table \\ comp.asInstanceOf[table.Component[Any]]).cast[T, T#Component[Any]]
				implicit val form = path.end.selectForm && path.end.updateForm
				t[T] \\ path := t.?[E](path.pick)
			}(breakOut) :Seq[Setter[T, E]]
			copy(assignments ++: this.assignments)
		}

		/** Create a template based on this instance, with the set clause containing assignments for all updatable by default
		  * columns of passed components and all setters passed explicitly before. Note that the new set clause will be a super set
		  * of this template's set clause only if it is not a 'default' instance, i.e. was created by explicitly calling 'set' or 'setTo'
		  * methods. The values for updated columns will be obtained by querying the table mapping for a value for the specific component
		  * from parameter value E (mapped type of T).
		  * @param components a list of functions returning components of the updated table
		  *                   (either columns or multi-column components) to be included in the set clause.
		  * @return an update template with the same table and 'where' clause as this instance, and containing setters
		  *         specified by this.assignments prepended by all updatable columns of all components in the argument list.
		  */
		def set(components :(T=>T#AnyComponent)*) :UpdateWhereEntityTemplate[T, E] =
			set(components.map(_(table)))

		/** Create a template based on this instance, with the set clause containing assignments for all updatable by default
		  * columns of passed component and all setters passed explicitly before. Note that the new set clause will be a super set
		  * of this template's set clause only if it is not a 'default' instance, i.e. was created by explicitly calling 'set' or 'setTo'
		  * methods. The values for updated columns will be obtained by querying the table mapping for a value for the specific column
		  * from parameter value E (mapped type of T).
		  * @param component a component which columns should be included in the set clause
		  * @return an update template with the same table and 'where' clause as this instance, and containing setters
		  *         specified by this.assignments prepended by all updatable columns of the argument component
		  */
		def set(component: T#Component[_]): UpdateWhereEntityTemplate[T, E] =
			set(Seq(component))


		override protected def copy(assignments: Seq[UpdateComponent[From[T] WithParam E, T, _<:AnyMapping, _, _]]): UpdateWhereEntityTemplate[T, E] =
			new UpdateWhereEntityTemplate[T, E](source, assignments)
	}


	/** A 'default' update template which takes as its parameter type mapped to the updated table and including
	  * all updatable columns of the table in the set clause of created update statements.
	  * Note that calling any 'set' or 'setTo' methods on this instance will start creating the set clause from scratch and
	  * templates created that way will not include the columns present in this instance's set clause.
	  * @param src row source consisting of the updated table and template parameter E with a filter condition to be used as
	  *            the where clause of created statements
	  */
	class DefaultUpdateWhereEntityTemplate[T <:Mapping[E], E] protected (src:From[T] WithParam E)
		extends UpdateWhereEntityTemplate[T, E](src, Seq())
	{
		/** Create a new 'default' update template for statements updating all updatable columns of T and where clause
		  * given by the sql formula created by the second argument
		  * @param table updated table
		  * @param condition function creating the filter condition grounded in this.source to be used as the 'where' clause.
		  */
		def this(table :T, condition  :RowTables[From[T] WithParam E] => BooleanFormula[From[T] WithParam E]) =
			this(filterTable(table, condition))

		override def setters = table.updatable.map(columnSetter(_))

		def columnSetter[V](column :T#Component[V]) :Setter[T, E] = {
			val t = source.asTables[Source]
			val path = ComponentPath(table, column)
			implicit val form = path.end.selectForm && path.end.updateForm
			t[T] \\ path := t.?[E](path.pick)
		}
	}


	/** A template for update statements with a 'returning' clause.
	  * @param source source consisting of updated table, template parameter and row filter to be used as the 'where' clause.
	  * @param setters setters for updated components to be included in the 'set' clause
	  * @param returnExpression sql formula, based on the new values of updated rows (with possibly columns modified by the database),
	  *                         which value should be returned by created update statements
	  * @tparam T mapping type of updated table
	  * @tparam E mapped row (entity) type of updated table
	  * @tparam X template parameter type
	  * @tparam Y template return type
	  */
	class UpdateWhereReturningTemplate[T <: Mapping[E], E, X, Y](
			val source :From[T] WithParam X,
			val setters :Seq[Setter[T, X]],
			returnExpression :SQLFormula[From[T] WithParam X, Y])
		extends AbstractUpdateTableTemplate[T, E, X, Y]
	{
		protected override def returning = Some(returnExpression)

		override val input = super.input
		override val output: SQLReadForm[Y] = returnExpression.readForm
		override def sql = updateSQL
	}


	/** Base trait for update templates, responsible for creating the sql as well as input and output forms based on the
	  * values to be used for 'set', 'where', and 'returning' clauses specified by subclasses.
	  * @tparam T mapping type of updated table
	  * @tparam E mapped row (entity) type of updated table
	  * @tparam X template parameter type
	  * @tparam Y template return type
	  */
	trait AbstractUpdateTableTemplate[T<:Mapping[E], E, X, Y] extends UpdateTemplate[X, Y] {
		protected type Source = From[T] WithParam X
		val source :From[T] WithParam X

		def table = source.left.right

		protected def tableName = table.sqlName getOrElse {
			throw new IllegalArgumentException(s"Cannot create update $table - no sqlName specified for $source")
		}
		protected def alias = SQLPrinter.alias(source.prev, Set())

		protected def noAlias = Map[TableFormula[source.type, _], String](source.prev -> "")
		protected def aliases = Map[TableFormula[source.type, _], String](source.prev -> alias)

		protected def setters :Seq[Setter[T, X]]

		
		protected def setterForm[C<:AnyMapping, RV, V](setter :UpdateComponent[Source, T, C, RV, V]) :Seq[UpdateForm[Source, T, X]] =
			SetComponent.UpdateForms(source, setter)

		protected def setterForms :Seq[UpdateForm[Source, T, X]] = setters.flatMap(setterForm(_) :Seq[UpdateForm[Source, T, X]])

		def columns = setterForms.flatMap(_.columns)

		protected def returning :Option[SQLFormula[From[T] WithParam X, Y]] = None

		protected def returningForms :Seq[SQLWriteForm[X]] = {
			val ParamForm = source.tables.param[X].ParamForm
			returning.toSeq.flatMap(_.collect {
				case BoundParameter(v, form) => SQLWriteForm.const(v)(form.asInstanceOf[SQLWriteForm[Any]])
				case ParamForm(form) => form
			})
		}

		protected def fromClause = alias.providing(_.length>0).map(tableName+" "+_) getOrElse tableName
		
		protected def updateClause = setterForms.map(_.updateSQL).mkString(" set ", ", ", "")

		protected def whereClause = source.filter match {
			case True() => ""
			case e => " where "+SQLPrinter(e, aliases)
		}

		protected def whereForms :Seq[SQLWriteForm[X]] = {
			val ParamForm = source.tables.param[X].ParamForm
			source.filter.collect {
				case BoundParameter(v, form) => SQLWriteForm.const(v)(form.asInstanceOf[SQLWriteForm[Any]])
				case ParamForm(form) => form
			}
		}


		protected def returningClause = returning.map(" returning "+SQLPrinter(_, noAlias, ForSelect)) getOrElse ""

		def updateSQL = s"update $fromClause$updateClause$whereClause$returningClause"

		def sql = updateSQL

		def input: SQLWriteForm[X] = SQLWriteForm.chain(setterForms ++: whereForms ++: returningForms)
	}




	class UpdateTable[X, Y](override val template: UpdateTemplate[X, Y], value: X)
		extends ParameterizedStatement[X, Y](template, value)
	{
		def table = template.table

		override def toString = s"$template value $param"
	}


	private def filterTable[T <: AnyMapping, X](table: T, condition: RowTables[From[T] WithParam X] => BooleanFormula[From[T] WithParam X]) = {
		val source = From(table).withParam[X]
		source where condition(source.asTables[From[T] WithParam X])
	}


}

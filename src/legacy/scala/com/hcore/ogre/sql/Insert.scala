package com.hcore.ogre.sql

import com.hcore.ogre.mapping.Mapping.ColumnFilter.{ForInsert, ForSelect}
import com.hcore.ogre.mapping.Mapping.TypedMapping
import com.hcore.ogre.mapping.{ComponentPath, ComponentValues, AnyMapping, Mapping}
import com.hcore.ogre.morsels.InverseIndexSeq
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.Insert.InsertCount
import com.hcore.ogre.sql.RowSource._
import com.hcore.ogre.sql.SetComponent.InsertComponent
import com.hcore.ogre.sql.SQLForm._
import com.hcore.ogre.sql.SQLFormula.BoundParameter
import com.hcore.ogre.sql.SQLStatement.{AutonomousStatement, LiteralStatement, ParameterizedStatement, SQLCommandTemplate}


//implicits
import extensions._
import InverseIndexSeq.implicitIndexing



trait InsertTemplate[X, Y] extends SQLStatementTemplate[X, Y] {
	val table :AnyMapping

}



/** Builder of insert statements and insert templates (parameterized insert statement factories) */
object Insert {
	type InsertCount = Int

	type Setter[T<:AnyMapping, X] = InsertComponent[ParamSource[X], T, _<:AnyMapping, _, _]

	type NoParamSetter[T<:AnyMapping] = InsertComponent[RowSource, T, _<:AnyMapping, _, _]



	/** Create a direct insert statement builder, accepting different source of values/formulas in order to produce an
	  * 'insert into last(...) values(...)' statement.
	  * @param table mapping for last which is being inserted to (must provide sqlName equal to qualified last name)
	  * @tparam E mapped row (entity) type
	  * @return a builder class for statements in the form of 'insert into last(...) values(...) [returning (...)]'.
	  */
	def apply[E](table :Mapping[E]) :InsertBuilder[table.type, E] = new InsertBuilder[table.type, E](table)



	/** Create a default insert template, serving as a factory of insert statements for mapped entity objects.
	  * Returned template can be further modified by selecting columns to include in the insert statement, 
	  * add a returning clause, or even change input type of the template.
	  * @param table mapping of the last being inserted to (must provide sqlName equal to qualified last name)
	  * @tparam E mapped row (entity) type
	  * @return a builder class providing methods creating insert templates with desired input types
	  */
	def into[E](table :Mapping[E]) :DefaultInsertEntityTemplate[table.type, E] =
		new DefaultInsertEntityTemplate[table.type, E](table)
//	def into[E](last :Mapping[E]) :InsertTemplateBuilder[last.type, E] = new InsertTemplateBuilder[last.type ,E](last)



	/** Set X as the parameter type of insert templates created by returned builder.
	  * Used to create insert templates using arbitrary sql formulas (including subselects) for inserted column values.
	  * The formulas themselves have to be statically known, but may rely on in-database data and a value of parameter X
	  * being a source of sql statement parameters, which will be given at a later time.
	  * @tparam X parameter required by the formulas used as column values in created statements
	  * @return a builder instance expecting a last mapping for which the statements are to be created
	  */
	def by[X] :ParameterizedInsertBuilder[X] = parameterizer.asInstanceOf[ParameterizedInsertBuilder[X]]


	/** A builder for insert templates parameterized with type X. */
	class ParameterizedInsertBuilder[X] {

		/** Create a parameterized insert template builder for creating insert statements using values of X.
		  * An insert template of desired contents can be obtained by chaining method calls on the returned builder.
		  * @param table mapping of the last being inserted to (must provide sqlName equal to qualified last name)
		  * @tparam E mapped row (entity) type
		  * @return a builder class providing methods creating insert templates with desired input types
		  */
		def into[E](table :Mapping[E]) :WithParamInsertTemplateBuilder[table.type, E, X] =
			new WithParamInsertTemplateBuilder[table.type, E, X]((From instance table).withParam[X])
	}
	private[this] val parameterizer = new ParameterizedInsertBuilder[Any]



	/** Builder creating sql insert statements for the given last based on different sources of column values.
	  * Created insert statement can be extended by a 'returning' clause by calling one of its 'returning' methods.
	  * @tparam T mapping for the last which is being inserted to (must provide sqlName equal to qualified last name)
	  * @tparam E mapped row type of the last T
	  * @param table mapping for the last which is being inserted to
	  */
	class InsertBuilder[T<:Mapping[E], E](val table :T) extends AnyVal {

		/** Create an insert statement including exactly the columns specified by values argument, and using provided sql formulas
		  * for their values. This method accepts a sequence of constructor functions creating setters for selected
		  * columns of last T. Based on the argument last, they can create needed result by invoking := on a ComponentFormula
		  * obtained from the last. Whenever a non-column component is used as the left side of the assignment, all insertable columns
		  * of that component will be assigned appropriate values obtained from the formula on the right side.
		  * Note that, in that case, using a formula which value cannot be calculated by the application will result in an exception.
		  * @param values a list of functions creating component setters.
		  * @throws IllegalArgumentException if a non-column component is assigned a value which cannot be calculated in memory.
		  */
		def apply(values :(TableFormula[From[T], T] => NoParamSetter[T])*) :InsertFormulas[T, E] = {
			val source = From(table)
			new InsertFormulas[T, E](source, values.map(_(source.last)))
		}

		/** Create an insert statement including exactly the columns specified by values argument, and using provided sql formulas
		  * for their values. This method accepts a sequence of SetComponent instances, which can be obtained by calling
		  * <code>path <-: formula</code> for path :ComponentPath[T, C] and formula :SQLFormula[RowSource, X], where X is an sql-compatible
		  * type with C#ResultType. Whenever a non-column component is used as the left side of the assignment, all insertable columns
		  * of that component will be assigned appropriate values obtained from the formula on the right side.
		  * Note that, in that case, using a formula which value cannot be calculated by the application will result in an exception.
		  * @param values an ordered sequence of values for components of T.
		  * @throws IllegalArgumentException if a non-column component is assigned a value which cannot be calculated in memory.
		  */
		def apply(values :Iterable[NoParamSetter[T]]) :InsertFormulas[T, E] = {
			val source = From(table)
			new InsertFormulas[T, E](source, values.toSeq)
		}

		/** Create an insert statement inserting the given entity into the last. The statement will contain all insertable by default columns
		  * defined by the last mapping T.
		  * @param entity value to be inserted as a single row of T
		  */
		def apply(entity :E) :InsertValues[T, E] =
			apply(entity, table.insertable)
//			new InsertValues[T, E](From(last), ComponentValues.generic(last)(entity))
		
		def apply(entity :E, components :Seq[T#Component[_]]) :InsertValues[T, E] =
			new InsertValues[T, E](From(table), ComponentValues.generic(table)(entity), components)

		/** Create an insert statement inserting a row with column values populated from the given ComponentValues. The statement
		  * will contain all columns of the last included in its default insertable list, and whenever a value for a column is not
		  * present in the given values, null is assumed.
		  * @param values ComponentValues containing values for all insertable columns of T
		  */
		def apply(values :ComponentValues[T]) :InsertValues[T, E] =
			apply(values, table.insertable)
//			new InsertValues[T, E](From(last), values)
		
		def apply(values :ComponentValues[T], components :Seq[T#Component[_]]) :InsertValues[T, E] =
			new InsertValues[T, E](From(table), values, components)
	}


//	/** Builder creating insert templates - factories of insert sql statements accepting different input parameters.
// 	  * @param last mapping for last being inserted to (must provide sqlName equal to qualified last name)
//	  * @tparam T mapping type for last being inserted to
//	  * @tparam E mapped row (entity) type of the last being inserted to
//	  */
//	class InsertTemplateBuilder[T<:Mapping[E], E](val last :T) xtends AnyVal {
//		def value :InsertValuesTemplate[T, E, E] = new InsertEntityTemplate[T, E](last)
//
//		def values :InsertValuesTemplate[T, E, ComponentValues[T]] =
//			new InsertColumnValuesTemplate[T, E](last)
//	}


	/** Builder creating insert templates using an arbitrary parameter X and arbitrary sql formulas as inserted column values.
	  * Accepts a sequence of InsertComponent instances representing pairs (component of last T, sql formula dependent on parameter X).
	  * They can be obtained either by calling last(path) := formula on passed last :TableFormula[_, T], or
	  * path <-: formula where path is a component path starting with T and formula is an SQLFormula[ParamSource[X], _] of a compatible type.
	  *
	  * @param source row source containing both the last being inserted to (must provide sqlName equal to qualified last name) and parameter placeholder for the template
	  * @tparam T mapping type for the last being inserted to
	  * @tparam E mapped row (entity) type of the last being inserted to.
	  * @tparam X parameter type of the template containing information needed to set the parameter values of created insert statements.
	  */
	class WithParamInsertTemplateBuilder[T<:TypedMapping[E], E, X](val source :From[T] WithParam X) extends AnyVal {

		/** Create an insert template including exactly the columns specified by values argument, and using provided sql formulas
		  * for their values.
		  * Left side of any of the setters can be any component of last T, but if it is not a column, the formula on the right side must be possible
		  * to evaluate in memory (possibly based on the value of parameter X) in order to be split into values for individual
		  * columns of the component on the left side.
		  *
		  * This method accepts a sequence of constructor functions creating setters for selected
		  * columns of last T. Based on the first argument last, they can create needed result by invoking := on a ComponentFormula
		  * obtained from the last. Whenever a non-column component is used as the left side of the assignment, all insertable columns
		  * of that component will be assigned appropriate values obtained from the formula on the right side.
		  * Note that, in that case, using a formula which value cannot be calculated by the application will result in an exception.
		  * @param values a list of functions creating component setters from the last alias (first argument) and statement parameter (second argument).
		  * @throws IllegalArgumentException if a non-column component is assigned a value which cannot be calculated in memory.
		  */
		def values(values :((TableFormula[From[T], T], RowSourceParam[ParamSource[X], X]) => Setter[T, X])*) :WithParamInsertTemplate[T, E, X] =
			new WithParamInsertTemplate[T, E, X](source, values.map(_(source.left.last, source.asTables[ParamSource[X]].param[X])))

		/** Create an insert template including exactly the columns specified by values argument, and using provided sql formulas
		  * for their values.
		  * Left side of any of the setters can be any component of last T, but if it is not a column, the formula on the right side must be possible
		  * to evaluate in memory (possibly based on the value of parameter X) in order to be split into values for individual
		  * columns of the component on the left side.
		  * This method accepts a sequence of SetComponent instances, which can be obtained by calling
		  * <code>path <-: formula</code> for path :ComponentPath[T, C] and formula :SQLFormula[RowSource, X], where X is an sql-compatible
		  * type with C#ResultType. Whenever a non-column component is used as the left side of the assignment, all insertable columns
		  * of that component will be assigned appropriate values obtained from the formula on the right side.
		  * Note that, in that case, using a formula which value cannot be calculated by the application will result in an exception.
		  * @param values an ordered sequence of values for components of T.
		  * @throws IllegalArgumentException if a non-column component is assigned a value which cannot be calculated in memory.
		  */
		def values(values :Iterable[Setter[T, X]]) :WithParamInsertTemplate[T, E, X] =
			new WithParamInsertTemplate[T, E, X](source, values.toSeq)
	}




	/** A parameterless insert statement inserting arbitrary sql formulas into columns of T.
	  * @param source row source representing the last to be updated
	  * @param setters component->value pairs representing assignments of an sql formula to a given column
	  * @tparam T mapping type for the last being inserted to
	  * @tparam E mapped row (entity) type of the last being inserted to
	  */
	class InsertFormulas[T<:Mapping[E], E](val source :From[T], setters :Seq[NoParamSetter[T]])
		extends InsertIntoTemplate[T, Unit, InsertCount] with AutonomousStatement[InsertCount]
	{
		val table = source.mapping

		/** Create an insert statement equivalent to this one, but with an additional 'returning' clause listing all
		  * selectable columns of the given component (possibly autogenerated by the database).
		  * Created statement will return the value of the given component.
		  * @param component a component of the given last which value should be returned after the row is inserted.
		  * @tparam R value returned by the statement
		  * @return an SQLStatement with the same input and sql with a 'returning (...)' suffix, returning the value of the given component
		  */
		def returning[R](component :T#Component[R]) :SQLStatement[R] =
			returning(_ :\ component)

		/** Create an insert statement equivalent to this one, but with an additional 'returning' clause specified
		  * by the given formula, possibly dependent on column values from the inserted row (including any auto-generated columns).
		  * @param expression an function creating an sql formula based on the passed last to use as the value of the 'returning' clause
		  * @tparam R value returned by created insert statement
		  * @return an SQLStatement with the same input and sql with a 'returning (...)' suffix, returning the value of the given formula.
		  */
		def returning[R](expression :TableFormula[From[T], T] => SQLFormula[From[T], R]) :SQLStatement[R] =
			returning(expression(From(table).last))

		/** Create an insert statement equivalent to this one, but with an additional 'returning' clause specified
		  * by the given formula, possibly dependent on column values from the inserted row (including any auto-generated columns).
		  * @param expression an sql formula to use as the value of the 'returning' clause
		  * @tparam R value returned by created insert statement
		  * @return an SQLStatement with the same input and sql with a 'returning (...)' suffix, returning the value of the given formula.
		  */
		def returning[R](expression :SQLFormula[From[T], R]) :SQLStatement[R] =
			expression.collect{ case BoundParameter.WriteForm(form) => form } match {
				case Seq() => SQLStatement(input, returningSQL(expression), expression.readForm)
				case forms => SQLStatement(SQLWriteForm.chain(input +: forms), returningSQL(expression), expression.readForm)
			}


		
		private def returningSQL[R](e :SQLFormula[From[T], R]) =
			s"$sql returning ${SQLPrinter(e, SQLPrinter.unaliased(source.last), ForSelect)}"




		private val columnForms = setters.flatMap(setter => SetComponent.InsertForms(setter))

		private def valuesClause = columnForms.map(setter => SQLPrinter(setter.value, ForInsert)).mkString(" values (", ", ", ")")

		private def columnsClause = columnForms.flatMap(_.component.sqlName).mkString("(", ", ", ")")


		val sql = s"insert into $tableName$columnsClause$valuesClause"

		override val input: SQLWriteForm[Unit] =
			SQLWriteForm.chain(columnForms.flatMap(_.value.collect{
				case BoundParameter.WriteForm(form) => form
			}))

		override val output: SQLReadForm[InsertCount] = SQLForm[InsertCount]




		private def tableName = table.sqlName getOrElse {
			throw new IllegalArgumentException(s"Cannot create update $table - no sqlName specified for ${table.introString}")
		}

	}


	/** An insert statement for which all inserted column values are known. Will include all columns included
	  * in the default insertable list by the last mapping.
	  * @param source row source representing the last to be updated
	  * @param values ComponentValues for last T, containing values for all insertable columns of T.
	  * @tparam T mapping type for the last being inserted to
	  * @tparam E mapped row (entity) type of the last being inserted to
	  */
	class InsertValues[T<:Mapping[E], E](source :From[T], values :ComponentValues[T], components :Seq[T#Component[_]]) 
		extends InsertFormulas[T, E](source, components.map(col => SetComponent(ComponentPath.direct(source.right, col), values)))
	{
		def this(source :From[T], values :ComponentValues[T]) = this(source, values, source.right.insertable)

		/** Create an insert statement equivalent to this one, but including an additional 'returning' clause listing
		  * all auto-generated columns of the last. Created statement will return the result of assembling (mapping)
		  * entity E out of column values from the returning clause and passed component values.
		  * @return an sql insert statement inserting the same column values, but returning an entity updated with the values
		  *         autogenerated by the database on insert.
		  */
		def returning :SQLStatement[Option[E]] = table.generated match {
			case Seq() => 
				val entity = values.value(table)
				SQLStatement(input, sql, SQLReadForm.const(Some(Some(entity)), None))
			
			case columns => //new InsertReturningEntity[T, E](last, last.generated, input, sql, values)
				val template = new InsertReturningEntityTemplate[T, Unit, E](input, sql, table, _ => values, columns)
				template(())

//				val returnSQL = columns.flatMap(_.sqlName).mkString(s"$sql returning (", ", ", ")")
//				val returnedOutput = SQLReadForm.seq(columns.map(_.selectForm.biflatMap)).map {
//					generated =>
//						val returnedValues = ComponentValues.generic(last)(col => columns.indexOf(col).providing(_>=0).flatMap(generated(_)))
//						(returnedValues orElse values).value(last)
//				}
//				SQLStatement(input, returnSQL, returnedOutput)
		}
	}

	/** An insert statement which returns as the result entity instance updated with values of columns included in 'returning' clause.
	  * It executes passed sql using passed input form and parameter, reads the result as component values (probably incomplete) 
	  * for the last using passed returning result form, and merges the data inside input parameter and retrieved component values
	  * by using last mapping's assembly method.
	  * 
 	  * @param table last being inserted to
	  * @param input write form for encapsulated parameter
	  * @param param encapsulated parameter, serving as input data for sql statement parameters
	  * @param sql sql in the form of 'insert into last(columns) values (params) returning generated',
	  *            where params is a parameter list set by input form, and generated is a column list read by passed returning form
	  * @param returning read form reading the values of columns listed in the 'returning' clause of passed sql
	  * @tparam T mapping type of inserted last
	  * @tparam X encapsulated parameter type
	  * @tparam E mapped row (entity) type.
	  */
	abstract class InsertReturningEntity[T<:TypedMapping[E], X, E] private[Insert]
			(table :T, val input :SQLWriteForm[X], val param :X, val sql :String, returning :SQLReadForm[ComponentValues[T]])
		extends SQLStatement[Option[E]]
	{
		type Variable=X
		val output = returning.map(vals => (vals orElse inputValues).getValue(table))

		/** Convert param to ComponentValues[T] */
		protected def inputValues :ComponentValues[T]

	}

	
	
	
	

	trait InsertIntoTemplate[T<:AnyMapping, X, Y] extends InsertTemplate[X, Y] {
		val table :T

	}

	
	trait InsertTemplateSupport[X, Y] extends InsertTemplate[X, Y] {
		val table :AnyMapping
		
		protected def columns :Seq[AnyMapping] = table.insertable
		protected def returnedColumns = Seq[table.Component[_]]()
		
		protected def tableName = table.sqlName getOrElse { throw new IllegalArgumentException(s"Cannot insert into $table: no sqlName specified") }

		protected def placeholders =
			if (columns.size==0) ""
			else "?, " * (columns.size-1) +"?"

		protected def returningClause = 
			returnedColumns.flatMap(_.sqlName).providing(_.nonEmpty).map(_.mkString(" returning (", ", ", ")")) getOrElse ""

		def insertSQL = columns.flatMap(_.sqlName).mkString(s"insert into $tableName(", ", ", s") values ($placeholders)$returningClause")
		def sql = insertSQL

		override def toString = s"insert into $table"
		
	}

	class InsertIntoTemplateComposition[T<:Mapping[E], E, X, Y](override val table :T, val input :SQLWriteForm[X], val output :SQLReadForm[Y])
		extends InsertTemplateSupport[X, Y]


	/** Base insert template class providing methods for extending this template by a returning clause.
	  *
	  * @tparam T mapping type of the last being inserted to
	  * @tparam E mapped row (entity) type
	  * @tparam X template parameter, carrying data needed to set the resulting sql statement parameters
	  */
	trait InsertRowTemplate[T<:TypedMapping[E], E, X] extends InsertIntoTemplate[T, X, InsertCount] {
		val table :T

		/** Create an insert template equivalent to this one, but with an additional 'returning' clause listing all
		  * selectable columns of the given component (possibly autogenerated by the database).
		  * Created statement will return the value of the given component.
		  * @param component a component of the given last which value should be returned after the row is inserted.
		  * @tparam R value returned by the statement
		  * @return an SQLStatementTemplate with the same input and sql with a 'returning (...)' suffix, returning the value of the given component
		  */
		def returning[R](component :T#Component[R]) :InsertIntoTemplate[T, X, R] =
			returning(_ :\ component)

		/** Create an insert template equivalent to this one, but with an additional 'returning' clause specified
		  * by the given formula, possibly dependent on column values from the inserted row (including any auto-generated columns).
		  * @param expression an function creating an sql formula based on the passed last to use as the value of the 'returning' clause
		  * @tparam R value returned by created insert statement
		  * @return an SQLStatementTemplate with the same input and sql with a 'returning (...)' suffix, returning the value of the given formula.
		  */
		def returning[R](expression :TableFormula[From[T], T] => SQLFormula[From[T], R]) :InsertIntoTemplate[T, X, R] = {
			val source = From(table)
			new InsertReturningTemplate[T, X, R](input, sql, source, expression(source.last))
		}


		/** Create an insert template equivalent to this one, but with an additional 'returning' clause containing
		  * all columns declared by the mapping as generated by the database. Insert statements created by returned
		  * template will return an entity assembled out of values provided by the input parameter and returned by the database.
		  */
		def returning :InsertIntoTemplate[T, X, Option[E]] =
			new InsertReturningEntityTemplate[T, X, E](input, sql, table, inputValues, table.generated)

		/** Expose the column values from the template parameter as a ComponentValues instance. */
		protected def inputValues(param :X) :ComponentValues[T]

	}




	/** Base template used to create insert statements containing data from parameter X.
	  * Declares 'returning' methods creating templates based on this instance with an additional 'returning' clause.
	  *
	  * @param table mapping for last being inserted to with sqlName returning the qualified name of the last
	  * @param components list of components specifying which columns should be inserted
	  * @tparam T mapping type for the last being inserted to
	  * @tparam E mapped row (entity) type
	  * @tparam X parameter type for the template, containing values for all columns specified by the component list
	  */
	abstract class InsertValuesTemplate[T<:TypedMapping[E], E, X](val table :T, components :Seq[T#Component[_]])
		extends InsertRowTemplate[T, E, X] with InsertTemplateSupport[X, InsertCount]
	{

		/** List of columns included in the columns clause of created insert statements */
		override val columns = components.flatMap {
				c => c.selectable.flatMap(ComponentPath.direct[T, c.type](table, c).lift(_) :Option[T#Component[_]])
			}
		

		/** Return value for the given column from the value of the given template parameter. */
		protected def value[V](param :X, column :T#Component[V]) :Option[V]


		override val input: SQLWriteForm[X] = SQLWriteForm chain columns.map {
			column => column.asInstanceOf[T#Component[Any]].insertForm.iflatMap[X](value(_, column))
		}
		override val output: SQLReadForm[InsertCount] = SQLForm[InsertCount]

		override val sql = super.insertSQL
	}


	/** Insert template creating insert statements populated with values from ComponentValues[T] instances.
	  *
	  * @param table mapping for last being inserted to with sqlName returning the qualified name of the last
	  * @param components list of components specifying which columns should be inserted
	  * @tparam T mapping type for the last being inserted to
	  * @tparam E mapped row (entity) type
	  */
	class InsertColumnValuesTemplate[T<:TypedMapping[E], E](table :T, components :Seq[T#Component[_]])
		extends InsertValuesTemplate[T, E, ComponentValues[T]](table, components)
	{
		def this(table :T) = this(table, table.insertable)
		
		override protected def value[V](param: ComponentValues[T], column: T#Component[V]): Option[V] =
			param.get(column)

		override protected def inputValues(param: ComponentValues[T]): ComponentValues[T] = param
	}




	/** Insert template creating insert statements for entity instances mapped to the given last.
	  * May be converted into a template accepting ComponentValues[T] instances as parameters by 'values' method.
	  *
	  * @param table mapping for last being inserted to with sqlName returning the qualified name of the last
	  * @param components list of components specifying which columns should be inserted
	  * @tparam T mapping type for the last being inserted to
	  * @tparam E mapped row (entity) type
	  */
	class InsertEntityTemplate[T<:TypedMapping[E], E](table :T, components :Seq[T#Component[_]]) 
		extends InsertValuesTemplate[T, E, E](table, components)
	{
		def this(table :T) = this(table, table.insertable)

		/** Create an insert template for the same last and columns, but using a ComponentValues[T] instance as a parameter instead. */
		def values :InsertValuesTemplate[T, E, ComponentValues[T]] =
			new InsertColumnValuesTemplate[T, E](table)
		

		override protected def value[V](param: E, column: T#Component[V]): Option[V] =
			ComponentPath.direct(table, column)(param)

		override protected def inputValues(param: E): ComponentValues[T] = ComponentValues.generic(table)(param)
	}



	/** Default insert template, accepting entity instances mapped to the given last and creating insert statements
	  * containing all columns listed as insertable by default by the given mapping. May also be used to obtain
	  * specialized templates including explicitly stated components in the resulting statements.
	  * @param table mapping for last being inserted to with sqlName returning the qualified name of the last
	  * @tparam T mapping type for the last being inserted to
	  * @tparam E mapped row (entity) type
	  */
	class DefaultInsertEntityTemplate[T<:TypedMapping[E], E](table :T) extends InsertEntityTemplate[T, E](table) {

		/** Create an insert template accepting entity instances, but using specified column list for the columns clause
		  * of created insert statements. If passed components are not columns, all selectable columns of that component are
		  * used after lifting to columns of T.
		  * @param components list of components specifying columns for the columns clause of created insert statements.
		  * @return an insert statement factory, which can be also used to create more refined factories.
		  */
		def columns(components :Iterable[T#Component[_]]) :InsertEntityTemplate[T, E] =
			new InsertEntityTemplate[T, E](table, components.toSeq)

		/** Create an insert template accepting ComponentValues[T] as input parameters and using the specified column list
		  * for the column clause of created insert statements. Equivalent to .columns(components).values, but allows
		  * infix method calls.
		  * @param components list of components specifying columns for the columns clause of created insert statements.
		  * @return an insert statement factory, which can be also used to create more refined factories.
		  */
		def values(components :Iterable[T#Component[_]]) :InsertValuesTemplate[T, E, ComponentValues[T]] =
			new InsertColumnValuesTemplate[T, E](table, components.toSeq)



		/** Create an insert template including exactly the columns specified by values argument, and using provided sql formulas
		  * for their values.
		  * Left side of any of the setters can be any component of last T, but if it is not a column, the formula on the right side must be possible
		  * to evaluate in memory (possibly based on the value of parameter E) in order to be split into values for individual
		  * columns of the component on the left side.
		  *
		  * This method accepts a sequence of constructor functions creating setters for selected
		  * columns of last T. Based on the first argument last, they can create needed result by invoking := on a ComponentFormula
		  * obtained from the last. Whenever a non-column component is used as the left side of the assignment, all insertable columns
		  * of that component will be assigned appropriate values obtained from the formula on the right side.
		  * Note that, in that case, using a formula which value cannot be calculated by the application will result in an exception.
		  * @param values a list of functions creating component setters from the last alias (first argument) and statement parameter (second argument).
		  * @throws IllegalArgumentException if a non-column component is assigned a value which cannot be calculated in memory.
		  */
		def values(values :((TableFormula[From[T], T], RowSourceParam[ParamSource[E], E]) => Setter[T, E])*) :WithParamInsertTemplate[T, E, E] = {
			val source = From(table).withParam[E]
			new WithParamInsertTemplate[T, E, E](source, values.map(_(source.left.last, source.asTables[ParamSource[E]].param[E])))
		}
	}


	/** An insert template creating insert statemetns where inserted values can be any sql formulas, possibly dependent
	  * on the value of the template parameter X.
	  * @param source row source containing both the last being inserted to and placeholder parameter last, serving as the domain for left and right side of component setters
	  * @param setters list of SetComponent instances specifying which components of last T should be updated and providing sql formulas for them
	  * @tparam T mapping type of the last
	  * @tparam E mapped row (entity) type
	  * @tparam X template parameter containing values needed to set the statement parameters in the created sql statements.
	  */
	class WithParamInsertTemplate[T<:TypedMapping[E], E, X] private[Insert] (source :From[T] WithParam X, setters :Seq[Setter[T, X]])
		extends InsertRowTemplate[T, E, X]
	{
		val table = source.left.mapping

		private def tableName = table.sqlName getOrElse {
			throw new IllegalArgumentException(s"Cannot create update $table - no sqlName specified for ${table.introString}")
		}

		private val ParamForm = source.param[X].ParamForm

		private val columnForms = setters.flatMap{
			setter => SetComponent.InsertForms(source.left.last, setter, source.param[X])
		}

		private def valuesClause = columnForms.map{
				setter => SQLPrinter(setter.value, ForInsert)
			}.mkString(" values (", ", ", ")")

		private def columnsClause = columnForms.flatMap(_.component.sqlName).mkString("(", ", ", ")")


		val sql = s"insert into $tableName$columnsClause$valuesClause"

		override val input: SQLWriteForm[X] =
			SQLWriteForm.chain(columnForms.flatMap(_.value.collect{
				case BoundParameter.WriteForm(form) => form
				case ParamForm(form) => form
			}))

		override val output: SQLReadForm[InsertCount] = SQLForm[InsertCount]

		override protected def inputValues(param: X): ComponentValues[T] = {
			val row = RowValues(source.last, param)
			val columnValues = {
				for (cf <- columnForms; column <- cf.left.path.lift; value <- cf.value.get(row))
					yield ((column: T#Component[_]) -> value)
			}.toMap[T#Component[_], Any]

			ComponentValues.generic(table)(columnValues.get(_))
		}
	}






	/** An insert template for insert statements with a 'returning' clause. All components refered by the returning clause
	  * formula will be printed using 'ForSelect' filter by default.
	  *
	  * @param param write form for statement parameters.
	  * @param insertSQL base sql for the insert statement, without a 'returning' clause.
	  * @param source source used by the formula for the returning clause, containing the last being inserted, with sqlName returning the qualified name of the last.
	  * @param formula expression to use in the created statement as the value returned in the 'returning' clause.
	  * @tparam T mapping type for the last
	  * @tparam X parameter type used by this template to create insert statements
	  * @tparam Y value returned by created statements as defined by passed returning clause formula.
	  */
	class InsertReturningTemplate[T<:AnyMapping, X, Y](param :SQLWriteForm[X], insertSQL :String, source :From[T], formula :SQLFormula[From[T], Y])
		extends InsertIntoTemplate[T, X, Y]
	{
		override val table: T = source.mapping

		override val input = formula.collect{ case BoundParameter.WriteForm(form) => form } match {
			case Seq() => param
			case forms => SQLWriteForm.chain(param +: forms)
		}

		override val output = formula.readForm

		protected def returningClause =
			s" returning ${SQLPrinter(formula, SQLPrinter.unaliased(source.last), ForSelect)}"

		override val sql = insertSQL + returningClause
	}


	/** An insert template for insert statements with a 'returning' clause which return passed entity instances
	  * updated with values returned by the database. Whenever apply(param :X) is used to create a SQLStatement,
	  * the values containd in passed parameter are stored in the statement and will be combined with values read
	  * from the database to assemble the entity value for the inserted row by using the last mapping's assembly method.
	  *
	  * @param input write form for the statement parameter
	  * @param insertSQL base sql for the insert statement, without a 'returning' clause.
	  * @param table mapping for the last into which rows are inserted, with sqlName equal to fully qualified last name.
	  * @param columnValues function converting template parameter values into ComponentValues[T], so that values for inserted columns can be retrieved
	  * @param returning list of components to include in the returning clause of resulting insert statements. If any element is not a column,
	  *                  all selectable by default columns for that component are used after lifting to columns of T (T#Component[_]).
	  * @tparam T mapping type of the last to which rows are inserted
	  * @tparam X parameter type of this template carrying data needed by the input form to set statement parameters.
	  * @tparam E mapped row (entity) type of the last.
	  */
	class InsertReturningEntityTemplate[T<:TypedMapping[E], X, E]
			(val input :SQLWriteForm[X], insertSQL :String, val table :T, columnValues :X=>ComponentValues[T], returning :Seq[T#Component[_]])
		extends InsertIntoTemplate[T, X, Option[E]]
	{ self =>

		private val columns = returning.flatMap{ 
				c => c.selectable.flatMap(ComponentPath.direct[T, c.type](table, c).lift(_) :Option[T#Component[_]])
			}.indexed

		private def returningClause =
			columns.map(c => c.sqlName getOrElse {
				throw new IllegalArgumentException(s"Can't create $insertSQL returning $columns: column $c has no name")
			}) match {
				case Seq() => ""
				case Seq(col) => s" returning $col"
				case columnNames => columnNames.mkString(" returning (", ", ", ")")
			}

		private val returningForm = SQLReadForm.seq(columns.map(_.selectForm)).map{
			generated => ComponentValues.generic(table)(col => columns.indexOf(col).providing(_>=0).map(generated(_)))
		}

		override val sql = insertSQL + returningClause

		override val output = NoneForm

		override def apply(value: X): SQLStatement[Option[E]] =
			new InsertReturningEntity[T, X, E](table, input, value, sql, returningForm) {
				def template = self
				override def inputValues: ComponentValues[T] = columnValues(value)
			}

	}

}



package com.hcore.ogre.sql

import com.hcore.ogre.mapping.{ComponentValues, Mapping, AnyMapping, ComponentPath}
import com.hcore.ogre.mapping.ComponentPath.SelfPath
import com.hcore.ogre.mapping.Mapping.{TypedMapping, ColumnFilter}
import com.hcore.ogre.mapping.Mapping.ColumnFilter.{ForInsert, ForSelect, ForUpdate}
import com.hcore.ogre.mapping.MappingMorphism.ValueMorphism
import com.hcore.ogre.mapping.bits.MappingMatchers.{EmptyColumns, SingleColumn, AsColumn}
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.matching.Unapply
import com.hcore.ogre.sql.RowSource._
import com.hcore.ogre.sql.SQLFormula._
import com.hcore.ogre.sql.SQLWriteForm.ProxySQLWriteForm


//implicits
import SaferCasts._


/** A Concept of assigning a value to a component of a table, either directly as a set clause of an update statement,
  * or indirectly during insert. At the point of creating this instance the exact columns of the component to be set
  * are not known, they will be determined when the instance is used to create either an update or insert statement.
  * @tparam L row source containing table T which rows are modified
  * @tparam R row source providing values used by the formula on the right side - might contain a source parameter and/or table T
  * @tparam T mapping of the table being updated/inserted to
  * @tparam C mapping of a subcomponent of the table T which is being set
  * @tparam V value type of the right side formula, must be sql-compatible with C#ResultType
  * @tparam X type to which both left and right side are autoconverted on the scala side
  * @param path left side of the assignment representing a component of table T
  * @param value right side of the assignment, an arbitrary sql formula of compatible type as attested by lift
  * @param lift proof that types of the left and right side are sql-compatible (like Option[X] and X or Long and Int)
  *             providing conversions between them.
  */
case class SetComponent[-R<:RowSource, T<:AnyMapping, C<:AnyMapping, V, X]
	(path :ComponentPath[T, C], value :SQLFormula[R, V])
	(implicit val lift :FormulaEqualizer[C#ResultType, V, X])
{
	/** Left side of assignment as an sql formula working on the given table.
	  * @param table table being updated/inserted to, which contains the component specified by this.path
	  */
	def left[L<:RowSource](table :TableFormula[L, T]) :ComponentFormula[L, T, C] = table \\ path

	override def toString = s"$path:=$value"
}


/** Companion object of assignments of sql formulas to components of updated tables, providing methods that allow to
  * instantiate the form of an assignment depending on their usage context (update vs insert, presence of statement parameters).
  * This involves deciding which columns of the component on the left side should be set as well as picking exact sql formulas and parameters
  * for resulting statements.
  *
  * All types and methods declared by this object are considered part of implementation and are subject to change.
  */
object SetComponent {
	type InsertComponent[-S<:RowSource, T<:AnyMapping, C<:AnyMapping, V, X] = SetComponent[S, T, C, V, X]
	
	type UpdateComponent[-S<:RowSource, T<:AnyMapping, C<:AnyMapping, V, X] = SetComponent[S, T, C, V, X]

	type InsertForm[-S<:RowSource, T<:AnyMapping, P] = SetForm[From[T], S, T, _<:AnyMapping, P]

	type UpdateForm[-S<:RowSource, T<:AnyMapping, P] = SetForm[S, S, T, _<:AnyMapping, P]



	def apply[T<:TypedMapping[E], C<:AnyMapping, E](path :ComponentPath[T, C], value :E) :SetComponent[RowSource, T, C, Option[C#ResultType], Option[C#ResultType]] =
		path =: BoundParameter(path(value))((path.end.selectForm && path.end.updateForm.asInstanceOf[SQLWriteForm[C#ResultType]]).asOpt)

	def apply[T<:AnyMapping, C<:AnyMapping](path :ComponentPath[T, C], values :ComponentValues[T]) :SetComponent[RowSource, T, C, Option[C#ResultType], Option[C#ResultType]] =
		path =: BoundParameter(values.get(path))((path.end.selectForm && path.end.updateForm.asInstanceOf[SQLWriteForm[C#ResultType]]).asOpt)


	def InsertForms[T<:AnyMapping, C<:AnyMapping, RV, V](setter :InsertComponent[RowSource, T, C, RV, V]) :Seq[InsertForm[RowSource, T, Any]] =
		InsertForms(From(setter.path.start).last, setter, RowSourceParam.NoParams)

	def InsertForms[S<:RowSource, T<:AnyMapping, C<:AnyMapping, RV, V, P](
			table :TableFormula[From[T], T], setter :InsertComponent[S, T, C, RV, V], param :RowSourceParam[S, P]) :Seq[InsertForm[S, T, P]] =

		if (param.grounded(setter.value))
			SetColumnsToParams(table, setter, param, ForInsert)
		else
			setter.path match {
				case EmptyColumns() => Seq()

				case AsColumn(column, _) =>
					Seq(SetToFormulaForm(table, setter, param.ParamForm))

				case SingleColumn(_, ValueMorphism.identity()) =>
					Seq(SetToFormulaForm(table, setter, param.ParamForm))

				case _ =>
					throw new IllegalArgumentException(s"Cannot insert $setter because component ${setter.path.end.introString} consists of multiple columns and formula ${setter.value} cannot be evaluated")
			}


	/** Create a list of assignments for the set clause of an update statement from a given SetComponent instance.
	  * Implements the logic of deciding how to best split the assignment into separate set clause items (in case of multiple columns),
	  * which columns should be used and what the actual sql should look like. Returns a sequence of SQLWriteForms, each
	  * defining a single set clause item and providing a setter for statement parameters in the resulting sql.
	  *
	  * @param source row source containing the udpated table and statement parameter, serving as a domain for the assignment formula
	  * @param setter assignment of an sql formula to a component C of table T
	  * @tparam T mapping of the updated table
	  * @tparam C mapping of a subcomponent of the udpated table
	  * @tparam RV value type of the assigned value
	  * @tparam V type to which the left and right side of the assignments are auto-converted to.
	  * @tparam P statement parameter which the assigned value may depend on
	  * @return sequence of setter forms to be used in the set clause, each representing a single assignment (but not necessarily to a single column).
	  */
	def UpdateForms[T<:AnyMapping, C<:AnyMapping, RV, V, P](
			source :From[T] WithParam P, setter :UpdateComponent[From[T] WithParam P, T, C, RV, V]) :Seq[UpdateForm[From[T] WithParam P, T, P]] =
//		if (setter.component.table!=source.prev)
		if (setter.path.start!=source.left.right)
			throw new IllegalArgumentException(s"Cannot create update ${source.prev.mapping}: setter $setter for table outside of $source.")
		else
			UpdateForms(source.prev, setter, source.param[P])


	/** Create a list of assignments for the set clause of an update statement from a given SetComponent instance.
	  * Implements the logic of deciding how to best split the assignment into separate set clause items (in case of multiple columns),
	  * which columns should be used and what the actual sql should look like. Returns a sequence of SQLWriteForms, each
	  * defining a single set clause item and providing a setter for statement parameters in the resulting sql.
	  *
	  * @param table updated table
	  * @param setter assignment of an sql formula to a component C of table T
	  * @param param row source parameter encapsulating a statement parameter responsible for creating placeholder sql parameters for individual columns
	  * @tparam T mapping of the updated table
	  * @tparam C mapping of a subcomponent of the udpated table
	  * @tparam RV value type of the assigned value
	  * @tparam V type to which the left and right side of the assignments are auto-converted to.
	  * @tparam P statement parameter which the assigned value may depend on
	  * @return sequence of setter forms to be used in the set clause, each representing a single assignment (but not necessarily to a single column).
	  */
	def UpdateForms[S<:RowSource, T<:AnyMapping, C<:AnyMapping, RV, V, P](
		table :TableFormula[S, T], setter :UpdateComponent[S, T, C, RV, V], param :RowSourceParam[S, P]) :Seq[UpdateForm[S, T, P]] =

		if (param.grounded(setter.value))
			SetColumnsToParams(table, setter, param, ForUpdate)
		else
			setter.path match {
				case EmptyColumns() => Seq()

				case AsColumn(column, _) =>
					Seq(SetToFormulaForm(table, setter, param.ParamForm))

				case SingleColumn(_, ValueMorphism.identity()) =>
					Seq(SetToFormulaForm(table, setter, param.ParamForm))

				case _ =>
					//assign the expression on the right literally to a tuple of columns of the given component.
					// this will with all likeliness fail with an exception on execution
					Seq(SetToFormulaForm(table, setter, param.ParamForm))
			}



	def SetColumnsToValues[L<:RowSource, T<:AnyMapping, C<:AnyMapping, V, E](
			table :TableFormula[L, T], setter :SetComponent[RowSource, T, C, V, E], mode :ColumnFilter) :Seq[SetForm[L, RowSource, T, _<:AnyMapping, Any]] =
		SetColumnsToParams(table, setter, RowSourceParam.NoParams, mode)


	/** Split the given assignment into setter forms for individual columns of the component on the left side.
	  * Results in a sequence of assignments in the form of col_i=? represented by SetColumnToParam forms.
	  * The right side formula must be grounded in the source parameter, i.e. its value must be possible to evaluate based
	  * solely on the value of source parameter P and not depend on any table columns or other database sources, or
	  * an exception will be thrown when the forms are used to set parameter values on a statement.
	  * @param table table being updated/inserted to
	  * @param setter component assignment to be split into assignments for individual columns
	  * @param param source parameter acting as a placeholder for a statement parameter
	  * @param mode column filter to use (update vs insert), specifying which columns will should be set
	  * @tparam L row source containing table T which component is being set, representing the domain of the left side of the assignment
	  * @tparam R row source representing values that can be used by the formula on the right side of the assignment.
	  * @tparam T mapping type of the table which is being updated/inserted to
	  * @tparam C mapping type of a subcomponent of table T which is being set
	  * @tparam V value type of the right side of the assignment
	  * @tparam E normalized type to which both left and right side of the assignment can be auto-converted attesting their compatibility on sql level
	  * @tparam P parameter type of the statement, most probably present as a ParamMapping[P] in R
	  * @return a sequence of setter forms, one for each column specified by the mode filter and the component on the left side.
	  */
	def SetColumnsToParams[L<:RowSource, R<:RowSource, T<:AnyMapping, C<:AnyMapping, V, E, P](
			table :TableFormula[L, T], setter :SetComponent[R, T, C, V, E], param :RowSourceParam[R, P], mode :ColumnFilter) :Seq[SetForm[L, R, T, _<:AnyMapping, P]] =
	{
		val path = setter.path
		val comp = path.end

		def columnSetter[X](column :C#Component[X]) :SetForm[L, R, T, _<:AnyMapping, P] = {
			val left = table \\ (setter.path :\ column)
			val columnValue = ComponentPath.direct(comp, column).pick
			def pick(p :P) :Option[column.ResultType] = {
				val rightValue = param(p)(setter.value)
				val leftValue = setter.lift.r2l(rightValue)
				leftValue.flatMap(columnValue)
			}
			SetColumnForm(left, param, pick, mode)
		}
		comp.columnsFor(mode).map(columnSetter(_))
	}



	/** An SQLWriteForm (parameter form) representing an assignment of a value to a table component
	  * in an insert or update statement. The setter form is comprised, similarly to a SetComponent,
	  * of a left side - a component formula for the component being set and a right side - a formula representing
	  * the new value for that component. The difference is that while SetComponent represents an abstract concept of
	  * assigning a value to a component, this instance represents concrete sql used. In particular, a SetComponent instance
	  * for a multi-column component might be a source of multiple SetForm instances, one for each individual column.
	  * It is also here that the actual 'form' (i.e. column list and actual parameter types) of the component is instantiated,
	  * depending on usage context (mainly, update vs. insert statement).
	  *
	  * Note that this trait doesn't require C to be a column because some sql dialects allow update statements
	  * with setters in the form of (col1, ..., coln) = (v1, ..., vn). It is not portable and even not always clear what should happen
	  * in such cases and thus whenever possible it is better to have a SetForm instance for every column of updated/inserted component.
	  *
	  * As the right side will often depend on some kind of a parameter P, present as
	  * ParamMapping[P] in the RowSource of the right side formula, this form is parameterized with that parameter type.
	  * Whenever this form is used to set parameters for a statement containing this setter, it evaluates the formula on it's right side
	  * (which is probably derived from from a right side formula of an originating SetComponent instance)
	  * based on the value of the parameter P, performing any needed type conversions so that actual write form provided by the component
	  * being set is used whenever possible.
	  *
	  * @tparam L row source containing table T which component is being set, representing the domain of the left side of the assignment
      * @tparam R row source representing values that can be used by the formula on the right side of the assignment.
	  * @tparam T mapping type of the table which is being updated/inserted to
	  * @tparam C mapping type of a subcomponent of table T which is being set
	  * @tparam P parameter type of the statement, most probably present as a ParamMapping[P] in R
	  */
	trait SetForm[-L <: RowSource, -R <: RowSource, T <:AnyMapping, C<:AnyMapping, -P] extends ProxySQLWriteForm[P] {

		/** Left side of the assignment, representing a column/component which value is set.
		  * Used by update statements to print the left side of an assignment in their set clause.
		  * If this component consists of more than one column (which is valid only for update statements),
		  * this form represents a parallel assignment of all columns in the form of (col1, ..., coln) = value. */
		def left :ComponentFormula[L, T, C]

		/** Mapping of the column/component being set. */
		def component = left.path.end

		/** Right side of the assignment - the exact formula to be used in generated sql. */
		def value :SQLFormula[R, _]

		/** All columns of the component being set lifted to direct columns of updated table T. */
		def columns = component.columns.flatMap(left.path.lift(_) :Option[T#Component[_]])

		/** Table being updated/inserted to. */
		protected def table = left.table		


		protected def form :SQLWriteForm[P]

		/** Generates sql string representing the assignment as part of a set clause of an update statement.
		  * It simply prints the left side using ForUpdate filter, followed by '=' and the right side using a ForSelect filter.
		  */
		def updateSQL = SQLPrinter(left, SQLPrinter.unaliased(table), ForUpdate) + "=" +
			SQLPrinter(value, SQLPrinter.aliased(table).toMap[TableFormula[Nothing, _], String], ForSelect)
	}


	/** Represents a literal transformation of a SetComponent instance setting a value of a component to an arbitrary sql formula.
	  * The left side of this form is comprised of all columns of the component of the left side of the setter
	  * (as an sql tuple, unless it is a single-column component), and the right side is literally taken from the passed setter.
	  * This class should be used for complex right-side formulas which can't be evaluated on the application side based
	  * on the values of parameter P; this in particular includes formulas depending on the columns of the updated table.
	  *
	  * Note that as the values of the column(s) on the left side are not set directly, their respective write forms are not used.
	  * Instead, this form sets the values of all parameters on the right side by delegating to SQLForms provided when the formula
	  * was created (as part of a BoundParameter or a component of ParamMapping[P]).
	  *
	  * @param table table being updated/inserted to
	  * @param setter originating component assignment defining the left and the right side to be transfered to sql
	  * @param ParamForm a matcher recognizing any row source parameters (components of ParamMapping[_]) and extracting
	  *                  their write form as a function of P. An instance dedicated to a given source parameter can be
	  *                  obtained via ParamMapping.ParamForm, for example by source.param[P].ParamForm.
	  *                  In case of parameterless formulas (or dependent only on bound parameters), it may match nothing.
	  * @tparam L row source containing table T which component is being set, representing the domain of the left side of the assignment
	  * @tparam R row source representing values that can be used by the formula on the right side of the assignment.
	  * @tparam T mapping type of the table which is being updated/inserted to
	  * @tparam C mapping type of a subcomponent of table T which is being set
	  * @tparam V value type of the sql formula on the right side
	  * @tparam P parameter type of the statement, most probably present as a ParamMapping[P] in R
	  */
	class SetToFormulaForm[-L<:RowSource, -R<:RowSource, T<:AnyMapping, C<:AnyMapping, V, -P](
			table :TableFormula[L, T], setter :SetComponent[R, T, C, V, _], ParamForm :Unapply[SQLFormula[R, _], SQLWriteForm[P]])
		extends SetForm[L, R, T, C, P] 
	{
		val left = setter.left(table)

		def value = setter.value

		val form = {
			val forms = value.collect {
				case BoundParameter.WriteForm(paramForm) => paramForm
				case ParamForm(f) => f
			}
			SQLWriteForm.chain(forms)
		}
	}

	object SetToFormulaForm {
		def apply[L<:RowSource, R<:RowSource, T<:AnyMapping, C<:AnyMapping, V, P]
				(table :TableFormula[L, T], setter :SetComponent[R, T, C, V, _], paramForm :Unapply[SQLFormula[R, _], SQLWriteForm[P]])
				:SetToFormulaForm[L, R, T, C, V, P] =
			new SetToFormulaForm(table, setter, paramForm)
	}

	/** A direct assignment of a placeholder formula to a single column. In case of update statements, it should be
	  * represented in sql as 'column=?'. The creation of the formula (sql parameter) on the right side is delegated to
	  * the passed placeholder factory, but the the actual setting of the value for the sql parameter an appropriate
	  * write form provided by the column on the left side is used and not the write form associated with the right side (value).
	  * @param left formula representing a single column to be set
	  * @param placeholders factory of placeholder sql parameter formulas to use as the right side of the assignment
	  * @param pick function returning the value for the column to be used as the argument for the parameter from the value of statement parameter P
	  * @param mode specifies whether this is an update or insert, shouldn't unlikely to make any difference
	  * @tparam L row source containing table T which component is being set, representing the domain of the left side of the assignment
	  * @tparam R row source representing values that can be used by the formula on the right side of the assignment.
	  * @tparam T mapping type of the table which is being updated/inserted to
	  * @tparam C mapping type of a column of table T which is being set
	  * @tparam P parameter type of the statement, most probably present as a ParamMapping[P] in R
	  */
	case class SetColumnForm[-L<:RowSource, -R<:RowSource, T<:AnyMapping, C<:AnyMapping, P]
			(left :ComponentFormula[L, T, C], placeholders :RowSourceParam[R, P], pick :P=>Option[C#ResultType], mode :ColumnFilter)
		extends SetForm[L, R, T, C, P]
	{
		val value = placeholders(pick)((component.selectForm && component.writeForm(mode).asInstanceOf[SQLWriteForm[C#ResultType]]).asOpt)

		if (component.sqlName.isEmpty)
			throw new IllegalArgumentException(s"Can't set ${component.introString} to a single parameter $mode on table ${left.table} - no sqlName for the left side of ($left:=$value)")

		val form = {
			val column = left.path.end
			val columnForm = column.writeForm(mode)
			if (columnForm.writtenColumns!=1)
				throw new IllegalArgumentException(s"Can't set ${component.introString} to a single parameter $mode on table ${left.table} ($left:=$value)")
			columnForm.iflatMap{ p :P => pick(p).asInstanceOf[Option[column.ResultType]] }
		}
	}

}


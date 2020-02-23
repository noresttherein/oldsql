package com.hcore.ogre.sql

import com.hcore.ogre.mapping.Mapping.TypedMapping
import com.hcore.ogre.mapping.{Mapping, AnyMapping}
import com.hcore.ogre.sql.Delete.DeleteCount
import com.hcore.ogre.sql.RowSource._
import com.hcore.ogre.sql.SQLFormula.{True, BooleanFormula}
import com.hcore.ogre.sql.SQLForm.UnitForm
import com.hcore.ogre.sql.SQLStatement.AutonomousStatement


trait DeleteTemplate[X] extends SQLStatementTemplate[X, DeleteCount] {
	def table :AnyMapping
	def output = SQLForm[Int]

}



/** A factory for delete statements and delete templates (parameterized delete statement factories). */
object Delete {
	type DeleteCount = Int

	/** Return a builder of parameterless delete statements. Statements created by this builder may be limited to selected rows,
	  * but the where clause relies solely on columns of the deleted table without any external parameters.
	  * @param table mapping representing table from which rows are to be deleted, which sqlName is equal to table's fully qualified name
	  * @return a builder which allows to specify a condition selecting rows to be deleted.
	  */
	def apply(table :AnyMapping) :ParameterlessDeleteBuilder[table.type] = new ParameterlessDeleteBuilder[table.type](table)


	/** Return a builder of parameterized delete templates. Returned instance by default accepts the parameter of the type E
	  * mapped to the given table, but it can be changed by invoking its 'by' method.
	  * @param table mapping representing table from which rows are to be deleted, which sqlName is equal to table's fully qualified name
	  * @return a builder which allows to specify a condition selecting rows to be deleted.
	  */
	def from[E](table :Mapping[E]) :DeleteTemplateBuilder[table.type, E] = new DeleteTemplateBuilder[table.type, E](table)


	/** Create a delete statement deleting all rows from the given table.
	  * @param table mapping representing table from which rows are to be deleted, which sqlName is equal to table's fully qualified name
	  * @return a literal delete statement - a statement which is its own template.
	  */
	def all(table :AnyMapping) :AutonomousStatement[DeleteCount] = Delete(table).all

	/** Return a builder of delete templates parameterized with type X.
	  * Created builder can be used in the same way as Delete object, but its from method will create builders
	  * parameterized with type X rather than the mapped row type of the table as is the default.
	  * @tparam X parameter type used by the 'where' clause of created delete statements.
	  * @return a builder accepting a table from which rows should be deleted.
	  */
	def by[X] :ParameterizedDeleteTemplateBuilder[X] = deleter.asInstanceOf[ParameterizedDeleteTemplateBuilder[X]]


	/** Delete template builder creating templates parameterized with type X. It accepts a table :T to be deleted from
	  * and will create another builder accepting a boolean formula grounded in From[T] WithParam X to use as the where clause
	  * of the created statements
	  * @tparam X input type of created delete templates.
	  */
	class ParameterizedDeleteTemplateBuilder[X] {
		def from[E](table :Mapping[E]) :DeleteTemplateBuilder[table.type, X] =
			new DeleteTemplateBuilder[table.type, X](table)
	}
	private[this] val deleter = new ParameterizedDeleteTemplateBuilder[Nothing]


	/** Delete statement and template builder not relying on any external input. Created statements are at the same time
	  * their own templates with Unit input type.
	  * @param table mapping representing table from which rows are to be deleted, which sqlName is equal to table's fully qualified name
	  * @tparam T type of the mapping for the table deleted from
	  */
	class ParameterlessDeleteBuilder[T<:AnyMapping](val table :T) extends AnyVal {
		def where(condition :TableFormula[From[T], T] => BooleanFormula[From[T]]) :AutonomousStatement[DeleteCount] =
			new ParameterlessDeleteWhere[T](table, condition)
		
		def all :AutonomousStatement[DeleteCount] = new ParameterlessDeleteWhere[T](table, _ => True)
			
	}

	/** Delete template builder accepting parameters X as input for created delete statements
	  * and allowing to specify which rows should be deleted. In order to create a DeleteFromTemplate[T, X],
	  * it needs a filtering condition grounded in row source From[T] WithParam X - dependant on any columns of the deleted table
	  * and/or values obtainable from the parameter X exposed as an artificial parameter 'table'.
	  *
	  * @param table mapping representing table from which rows are to be deleted, which sqlName is equal to table's fully qualified name
	  * @tparam T type of the mapping for the table deleted from
	  * @tparam X type of the parameters
	  */
	class DeleteTemplateBuilder[T<:AnyMapping, X](val table :T) extends AnyVal {

		/** Create a builder parameterized with the given type P. This completely overrides the parameter X set in this instance.
		  * @tparam P parameter type used by the created builder to create a 'where' condition.
		  * @return a delete template builder for the same table as this instance, but accepting conditions using parameter P instead of X.
		  */
		def by[P] :DeleteTemplateBuilder[T, P] = new DeleteTemplateBuilder[T, P](table)

		/** Create a delete template for statements 'delete from <code>table</code> where <code>filter</code>', where filter
		  * is the the formula created by passed function.
		  * @param condition a function creating an sql formula for the 'where' clause, using any columns/components
		  *                  of this.table as well as the template parameter exposed as parts of row source From[T] WithParam X.
		  * @return a delete template accepting values of type X and creating delete statements for rows specified by the given condition.
		  */
		def where(condition :RowTables[From[T] WithParam X]=>BooleanFormula[From[T] WithParam X]) :DeleteFromTemplate[T, X] =
			new DeleteWhereTemplate[T, X](table, condition)

		/** An alternative to 'where' method, creates a delete template accepting X as its argument.
		  * This variant expects a function accepting two arguments - the table deleted from and an artificial 'table' exposing the template parameter.
		  * @param condition a function creating an sql formula for the 'where' clause, using any columns/components
		  *                  of the table deleted from as ComponentFormula instances obtained by its first argument,
		  *                  and the value of the template parameter exposed as 'components' of artificial placeholder table given by its second argument.
		  * @return a delete template accepting values of type X and creating delete statements for rows specified by the given condition.
		  */
		def on(condition :(TableFormula[From[T] WithParam X, T], RowSourceParam[From[T] WithParam X, X]) => BooleanFormula[From[T] WithParam X]) :DeleteFromTemplate[T, X] =
			new DeleteWhereTemplate[T, X](table, t => condition(t.table[T], t.param[X]))
	}



	trait DeleteFromTemplate[T<:AnyMapping, X] extends DeleteTemplate[X] {
		def table :T

		protected def tableName = table.sqlName getOrElse { throw new IllegalArgumentException(s"Can't delete from $table: no sqlName specified.") }
	}


	/** An autonomous delete statement, that is a statement with Unit input type which is its own template.
 	  * @param source source wrapping both the table deleted from and the filter condition selecting rows to be deleted.
	  * @tparam T mapping type of the table deleted from
	  */
	class ParameterlessDeleteWhere[T<:AnyMapping] private (private val source :From[T]) 
		extends DeleteFromTemplate[T, Unit] with AutonomousStatement[DeleteCount] 
	{
		def this(table :T, condition :TableFormula[From[T], T] => BooleanFormula[From[T]]) =
			this({val source = From(table); source where condition(source.last) })
		
		def table = source.right
		
		override val sql = source.filter match {
			case True() => s"delete from $tableName"
			case e => s"delete from $tableName where ${SQLPrinter(e)}"
		}

		override val input = SQLWriteForm.chain(source.filter.boundParameters.map(_.writeForm))

//		override val output = super[DeleteFromTemplate].output
	}


	/** A delete template accepting values of type X to create delete statements.
	  * @param source a row source combining the table being deleted from, a placeholder for the parameter and the condition for the 'where' clause depending on both.
	  * @tparam T mapping type of the table deleted from
	  * @tparam X input type of this template
	  */
	class DeleteWhereTemplate[T<:AnyMapping, X] private (private val source :From[T] WithParam X)
		extends DeleteFromTemplate[T, X]
	{

		def this(table :T, condition :(RowTables[From[T] WithParam X]) => BooleanFormula[From[T] WithParam X]) =
			this({
				val source = From(table).withParam[X]
				source where condition(source)
			})

		def table = source.prev.mapping

		val sql = source.filter match {
			case True() => s"delete from $tableName"
			case e => s"delete from $tableName where ${SQLPrinter(e)}"
		}

		val input = {
			val Param = source.right
			val params = source.filter.collect { case Param(pick, form) => (pick, form :SQLWriteForm[Nothing]) }
			SQLWriteForm.seq[Nothing](params.map(_._2)).asInstanceOf[SQLWriteForm[Any]].imap((x:X) => params.map(_._1(x)))
		}


	}
}

package net.noresttherein.oldsql.sql

import scala.collection.immutable.{ArraySeq, LinearSeq}
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.Extractor.Optional
import net.noresttherein.oldsql.schema.{RelVar, SQLForm}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.Delete.implementation.{GroundDelete, ParamDelete}
import net.noresttherein.oldsql.sql.Delete.syntax.{DeleteAll, DeleteFacade, DeleteMany, DeleteOne, DeleteParam, GroundDeleteOneFactory, GroundMultiDeleteFactory}
import net.noresttherein.oldsql.sql.DML.{BoundDML, ComposedDML, DMLAPI, GroundDML, RepeatedDML}
import net.noresttherein.oldsql.sql.DMLStatement.{AlteredResultStatement, BoundStatement, ComposedStatement, DMLStatementAPI, StatementResult, StatementVisitor}
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UpdateCount
import net.noresttherein.oldsql.sql.ParamClause.{NamedParamRelation, ParamRelation, UnboundParam}
import net.noresttherein.oldsql.sql.SQLBoolean.{False, True}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.TableStatement.{GroundWhereClauseFactory, WhereAllClauseFactory, WhereAnyClauseFactory, WhereClauseFactory}
import net.noresttherein.oldsql.sql.ast.OrSQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.{BaseMappingSubject, MappingSubject}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization






/** An SQL ''delete'' statement or statements, possibly executed multiple times.
  * @tparam Args The types of the argument(s) accepted by this DML and the executable `Incantation` resulting from it.
  *              Multiple arguments are often passed as a [[net.noresttherein.oldsql.collection.Chain Chain]].
  *              A common case is the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] type
  *              of the domain [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] on which the ''where''
  *              condition [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] is based.
  * @tparam M    The type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]] type for the rows
  *              of the associated table, accepting the mapping's
  *              [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
  * @tparam Res  The type of the value returned by the resulting `Incantation`. In the most common case,
  *              this will be the number of deleted rows (or ''numbers'' of deleted rows in case of batched statements).
  * @see [[net.noresttherein.oldsql.sql.Delete]]
  */ //todo: rename to Deletes
trait DeleteDML[-Args, M[O] <: MappingAt[O], +Res]
	extends TableDML[Args, M, Res] with DMLAPI[Args, Res, Delete.from[M]#DML]
{
	override def compose[X](f :X => Args) :DeleteDML[X, M, Res] =
		new ComposedDML.Base[X, Args, Res, Delete.from[M]#DML](this, f)
			with DeleteDML[X, M, Res] with DerivedDML[X, Res]
			with ComposedDML[X, Args, Res] with ComposedDML.Impl[X, Args, Res, Delete.from[M]#DML]

	override def bind(args :Args) :DeleteDML[Unit, M, Res] =
		new BoundDML.Base[Args, Res, Delete.from[M]#DML](this, args)
			with DeleteDML[Any, M, Res] with DerivedDML[Any, Res]
			with BoundDML[Args, Res] with BoundDML.Impl[Args, Res, Delete.from[M]#DML]
}




/** An SQL ''delete'' statement represented as an abstract object, before its rendering as actual SQL.
  * Instances can be created using its [[net.noresttherein.oldsql.sql.Delete$ companion]] object as a factory.
  * Standard implementations, with the exception of adapters listed in
  * [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] documentation, extend from one of the two subtypes:
  *   1. [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete ParamDelete]] for parameterized statements, and
  *   1. [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete GroundDelete]] for statements independent
  *      of external values.
  *      Referencing any subtypes of `Delete`, other than use of
  *      [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete ParamDelete]]
  *      and [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete GroundDelete]] as base classes
  *      for custom implementations, is in general discouraged. They are public due to the API chainable methods they expose
  *      for creating varied `Delete` statements, but are implementation-specific artefacts which may change in the future
  *      and their additional API is restricted only to creating new instances of similar statements, rather than
  *      their inner details. The recommended approach is to use the factory expressions as a whole, upcasting their results
  *      to `Delete`/[[net.noresttherein.oldsql.sql.DeleteDML DeleteDMDL]] when complete. In cases where the expression
  *      must be created in a dynamic, generic way, create the condition
  *      [[net.noresttherein.oldsql.sql.SingleBoolean GlobalBoolean]] argument (or condition function argument as declared by
  *      [[net.noresttherein.oldsql.sql.TableStatement.WhereClauseFactory.where where]],
  *      [[net.noresttherein.oldsql.sql.TableStatement.WhereAllClauseFactory.and and]] and
  *      [[net.noresttherein.oldsql.sql.TableStatement.WhereAnyClauseFactory.or or]]) and pass it as a whole.
  *      The syntax for the statements is more stable than the methods by which it is achieved.
  * @tparam Args The types of the argument(s) accepted by this statement and the executable `Incantation` resulting
  *              from it. Multiple arguments are often passed as a [[net.noresttherein.oldsql.collection.Chain Chain]].
  *              A common case is the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] type
  *              of the domain [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] on which the ''where''
  *              condition [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] is based.
  * @tparam M    The type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]] type for the rows
  *              of the associated table, accepting the mapping's
  *              [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
  * @tparam Res  The type of the value returned by the resulting `Incantation`. In the most common case,
  *              this will be the number of deleted rows (or ''numbers'' of deleted rows in case of batched statements).
  */
trait Delete[-Args, M[O] <: MappingAt[O], +Res]
	extends DeleteDML[Args, M, Res] with TableStatement[Args, M, Res] with DMLStatementAPI[Args, Res, Delete.from[M]#Stmt]
{
	protected override def returns[Y](result :StatementResult[Nothing, Y]) :Delete[Args, M, Y] =
		new AlteredResultStatement.Base[Args, Y, Delete.from[M]#Stmt](this, result)
			with Delete[Args, M, Y] with DerivedDML[Args, Y]
			with AlteredResultStatement[Args, Y] with AlteredResultStatement.Impl[Args, Y, Delete.from[M]#Stmt]

	override def compose[X](f :X => Args) :Delete[X, M, Res] =
		new ComposedDML.Base[X, Args, Res, Delete.from[M]#Stmt](this, f)
			with Delete[X, M, Res] with DerivedDML[X, Res]
			with ComposedStatement[X, Args, Res] with ComposedStatement.Impl[X, Args, Res, Delete.from[M]#Stmt]

	override def bind(args :Args) :Delete[Unit, M, Res] =
		new BoundDML.Base[Args, Res, Delete.from[M]#Stmt](this, args)
			with Delete[Any, M, Res] with DerivedDML[Any, Res]
			with BoundStatement[Args, Res] with BoundStatement.Impl[Args, Res, Delete.from[M]#Stmt]

	override def batch :DeleteDML[Seq[Args], M, Seq[Res]] =
		new RepeatedDML.Base[Args, Res, Delete.from[M]#Stmt](this)
			with DeleteDML[Seq[Args], M, Seq[Res]] with DerivedDML[Seq[Args], Seq[Res]] with RepeatedDML[Args, Res]

	protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] = visitor.delete(this)
}




/** A factory of [[net.noresttherein.oldsql.sql.Delete Delete]] statements (and batches).
  * The statements are created by chaining method calls in a syntax closely following SQL, but expanding it
  * to incorporate most common use cases, especially working with mapped entity types as parameters.
  * The result will fall into one of the following, not mutually exclusive types:
  *   1. parameterless statements extending
  *      [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete GroundDelete]],
  *      which carry all required information for their execution
  *      (possibly as [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters)
  *   1. statements parameterized with arbitrary types
  *      extending [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete ParamDelete]],
  *   1. statements dedicated to removing individual entities, specified either at their creation (parameterless),
  *      or when the statement is executed (parameterized with the entity type),
  *   1. batches (instances of [[net.noresttherein.oldsql.sql.DeleteDML DeleteDML]] rather than `Delete` itself).
  *
  * The main methods of this object are:
  *   1. `Delete `[[net.noresttherein.oldsql.sql.Delete.from from]]` table` - constructs parameterless statements,
  *      with the ''where'' clause defined solely in terms of available values - literals
  *      or [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters.
  *   1. `Delete `[[net.noresttherein.oldsql.sql.Delete.one one]]` table` - statements parameterized with
  *      the entity type mapped to `table`.
  *   1. `Delete `[[net.noresttherein.oldsql.sql.Delete.many many]]` table` - batches parameterized with
  *      a collection of entities from the table, deleting all rows matching any of those entities.
  *   1. `Delete`[[net.noresttherein.oldsql.sql.Delete.apply[M[O* (table)]] - statements and batches
  *      with varied parameter types and nature:
  *      1. Parameterized with the entity type, same as with `Delete one table`;
  *      1. Parameterized with arbitrary types, which can be used in the ''where'' clause the same way
  *         as in a custom ''where'' clause given to `Delete one table where ...`;
  *      1. Parameterless statements and batches deleting giving entities;
  *      1. Parameterized Statements with a ''where'' clause consisting of the same condition, repeated
  *         several times for subsequent elements of the input collection.
  *   1. `Delete`[[net.noresttherein.oldsql.sql.Delete.apply[S](entity:S)* (entity*)]] - overloaded methods
  *      deleting all rows matching any of the input entities as single statements.
  *
  * The following syntax is supported:
  * {{{
  * Delete from Dragons where
  *     (_.race ==? "Blue Dragon" || _.race ==? "Red Dragon")  :Delete[(), Dragons, Int]
  * Delete from Dragons where { t =>
  *     (t.race === "Blue Dragon".? || t.race === "Red Dragon".?)
  *         && t.level > 20.?
  * }                                                          :Delete[(), Dragons, Int]
  * Delete from Dragons where (_.name ==? "Saladrex") or
  *     (_.name ==? "Thaxll'sillyia")
  * Delete from Dragons where (_.strength > _.intelligence)    :Delete[(), Dragons, Int]
  * Delete from Dragons where condition                        :Delete[(), Dragons, Int]
  * Delete from Dragons /* deletes all rows */                 :Delete[(), Dragons, Int] //consider: or Delete[Dragon, Dragons, Int] ???
  * Delete all Dragons  /* same, but more explicit */          :Delete[(), Dragons, Int]
  *
  * Delete(firkraag) from Dragons                              :Delete[(), Dragons, Int]
  * Delete(firkraag, saladrex) from Dragons                    :Delete[(), Dragons, Int]
  * Delete(dragons) from Dragons                               :Delete[(), Dragons, Int]
  *
  * Delete(Dragons)                                            :Delete[Dragon, Dragons, Int]
  * Delete(Dragons) where (_.level > _(_.level))               :Delete[Dragon, Dragons, Int]
  * Delete(Dragons)[String] where (_.race === _)               :Delete[String, Dragons, Int] //fixme: this doesn't work due to an implicit
  * Delete(Dragons)[Dragon] where (_.name === _(_.name))       :Delete[Dragon, Dragons, Int]
  * Delete(Dragons)[Dragon] where {
  *     ps => ps.table.race === ps.param(_.race)
  * }                                                          :Delete[Dragon, Dragons, Int]
  *
  * Delete(Dragons) * 6                                        :Delete[Seq[Dragon], Dragons, Int]
  * Delete(Dragons) * 6 where (_.name === _(_.name))           :Delete[Seq[Dragon], Dragons, Int]
  * Delete(Dragons)[String] * 6 where (_.name === _)           :Delete[Seq[String], Dragons, Int]
  * Delete(Dragons)(firkraag)                                  :Delete[(), Dragons, Int]
  * Delete(Dragons)(firkraag, saladrex)                        :DeleteDML[(), Dragons, Seq[Int]]
  * Delete(Dragons)(dragons)                                   :DeleteDML[(), Dragons, Seq[Int]]
  *
  * Delete one Dragons                                         :Delete[Dragon, Dragons, Int]
  * Delete one Dragons where (_.name === _(_.name))            :Delete[Dragon, Dragons, Int]
  * Delete many Dragons                                        :DeleteDML[Seq[Dragon], Dragons, Seq[Int]]
  * Delete many Dragons where {
  *     (t, p) => t.race === p(_.race) && t.name === p(_.name)
  * }                                                          :DeleteDML[Seq[Dragon], Dragons, Seq[Int]]
  *
  * Delete.using[String] from Dragons where (_.race === _)     :Delete[String, Dragons, Int]
  * Delete.using[String] from Dragons where { //same as above
  *     s => s.table.race == s.param
  * }                                                          :Delete[String, Dragons, Int]
  * Delete.using[String] from Dragons where expression         //a value of the same type as expressions above
  * Delete.using[String] many Dragons ...                      :Deletes[Seq[String], Dragons, Seq[Int]]
  * }}}
  *
  * The above list is not exhaustive for practical reasons and various combinations of the presented examples
  * are also possible. Simple expressions where given here for ''where'' conditions, but any Boolean
  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] can be provided in the same manner as with
  * the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] clause in
  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
  *
  * Consult the documentation of the individual leading factory methods for more details.
  * @author Marcin Mościcki
  */
object Delete { //todo: multiple unbound parameters with aliases, growing base FromClause.
	/** A curried type constructor for [[net.noresttherein.oldsql.sql.DeleteDML DeleteDML]] and
	  * [[net.noresttherein.oldsql.sql.Delete Delete]]. */
	type from[M[O] <: MappingAt[O]] = {
		type DML[-X, +Y] = DeleteDML[X, M, Y]
		type Stmt[-X, +Y] = Delete[X, M, Y]
	}
	//todo: remove code duplication
	//todo: multiple parameters and named parameters
	/** Initiates building of [[net.noresttherein.oldsql.sql.Delete Delete]] statements parameterized with `X`. */
	def using[X :SQLForm] :DeleteParam[X] = new DeleteParam(ParamRelation[X]())

	/** Initiates building of [[net.noresttherein.oldsql.sql.Delete Delete]] statements parameterized with `X`. */
	def using[X](param :ParamRelation[X]) :DeleteParam[X] = new DeleteParam(param)

//	def using[N <: Label :ValueOf, X :SQLForm] = ???
//	def using[N <: Label, X](param :NamedParamRelation[N, X]) = ???


	/** Creates a [[net.noresttherein.oldsql.sql.Delete Delete]]`[(), T, Int]` statement deleting all rows in
	  * the given table, which can be narrowed by its `where` methods specifying the filter condition.
	  * The returned object provides the following `where` methods:
	  *   1. `Delete from table `[[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:GlobalBoolean* where]]` condition` -
	  *      provides an arbitrary Boolean SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] as the filter;
	  *   1. `Delete from table `[[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]]` { mapping => condition }` -
	  *      provides the filter condition as a constructor function accepting mapping `M[From[M]]`,
	  *      which can be used to access any its components, all of which are implicitly convertible
	  *      to SQL component [[net.noresttherein.oldsql.sql.ast.ComponentSQL expressions]] and can be parts
	  *      of larger expressions;
	  *   1. `Delete from table `[[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:(M* where]]` { (mapping, mapping) => condition }` -
	  *      provides the filter condition as a constructor function accepting two copies of mapping `M[From[M]]`;
	  *      the arguments are the exact same mapping, but this doubling allows to create conditions which references
	  *      it twice (for example, by referencing two of its columns) in the shortened syntax:
	  *      {{{
	  *          Delete from Dragons where (_.strength > _.intelligence)
	  *      }}}
	  *      Variants of the above method for arities up to 10 are defined.
	  *
	  * All statements created starting with this method are parameterless:
	  * the filter expression must not depend on [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters,
	  * but use only [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] and
	  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]].
	  * See `Delete.`[[net.noresttherein.oldsql.sql.Delete.by by]]`[X] `[[net.noresttherein.oldsql.sql.Delete.syntax.DeleteParam.from from]]` table`
	  * and `Delete`[[net.noresttherein.oldsql.sql.Delete.apply[M[O* (table)]]
	  * for a generic way to create statements with parameters to be specified at execution time.
	  *
	  * The list of columns included by default when comparing whole components can be modified
	  * using `table`'s [[net.noresttherein.oldsql.schema.Relation.apply(components* apply]] method:
	  * {{{
	  *     Delete from Dragons(_.level.-) where (_ ===? Dragon("Firkraag", "Red Dragon", 23))
	  * }}}
	  * @tparam M    the mapping type of the rows in the affected table.
	  * @tparam T    the same mapping type as `M`,
	  *              but as a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] subtype - a separate
	  *              type argument for automatic type inference of the mapped type `S`
	  * @tparam S    the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type of the table's mapping `T`,
	  *              representing its every row.
	  * @param table the table with rows to delete. This allows persistent views without any checking if they
	  *              support delete.
	  * @return      a ''delete'' statement without a ''where'' clause, but providing `where` methods for adding one.
	  *              The statement returns the number of deleted rows.
	  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory]] -
	  *      definition place of `where` methods.
	  * @see [[net.noresttherein.oldsql.sql.Delete.by]] - factory method kick-starting the creation of parameterized
	  *      statements.
	  */
	def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	        (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :DeleteAll[S, T] =
		new DeleteAll[S, T](reveal(table))

	/** A ''delete'' statement deleting all rows from `table`. This is exactly equivalent to
	  * `Delete `[[net.noresttherein.oldsql.sql.Delete.from from]]` table`, but is self documenting
	  * due to specifying the intent explicitly.
	  * @return a parameterless [[net.noresttherein.oldsql.sql.Delete]]`[(), T, Int]` without a ''where'' clause.
	  *         The statement returns the number of deleted rows.
	  * @see [[net.noresttherein.oldsql.sql.Delete.from]]
	  * @see [[net.noresttherein.oldsql.sql.Delete.apply[M[O]<:MappingAt[O],T[O]<:BaseMapping[S,O],S](table:RelVar[M])*]]
	  */
	def all[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	       (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :Delete[Unit, T, Int] =
		new DeleteAll[S, T](reveal(table))

	/** Creates a ''delete'' statement deleting an entity from the given table. The statement is parameterized
	  * with the entity type `S` (the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type of the mapping
	  * used for table rows), which will be specified when the [[net.noresttherein.oldsql.sql.Incantation Incantation]]
	  * created from the returned [[net.noresttherein.oldsql.sql.Delete Delete]]`[S, T, Int]` is executed:
	  * {{{
	  *     val delete = Delete one Dragons
	  *     val command = delete.chant
	  *     command(Dragon("Firkraag", "Red Dragon", "chaotic evil", 23))
	  *     command(Dragon("Saladrex", "Red Dragon", "neutral evil", 25))
	  * }}}
	  * The filter condition in the ''where'' clause of the created statement compares all columns of `table`
	  * which are declared as filterable -
	  * the [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] list of the mapping
	  * associated with `table` with their corresponding properties of the argument entity, as extracted by
	  * table's mapping `M`. This means that, contrary to the method name, potentially any number of rows
	  * can be deleted as the result of executing this statement: both none and more than one.
	  *
	  * The list of columns included by default when comparing whole components can be modified
	  * using `table`'s [[net.noresttherein.oldsql.schema.Relation.apply(components* apply]] method:
	  * {{{
	  *     Delete one Dragons(_.level.-) //will not include the 'level' column in the filter condition
	  * }}}
	  *
	  * The ''where'' clause can be also supplanted by invoking
	  * [[net.noresttherein.oldsql.sql.TableStatement.WhereClauseFactory.where where]] method on the result.
	  * In that case the given condition will completely replace the default one, and this method simply
	  * specifies the parameter type used by the ''where'' clause as the table mapping's subject type:
	  * {{{
	  *     Delete one Dragons where (_.name === _(_.name))
	  * }}}
	  * In the above expression, the first placeholder parameter `_` refers to
	  * `Dragons.`[[net.noresttherein.oldsql.schema.Relation.row row]] mapping, while the second to
	  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[Dragon, _]` synthetic mapping for
	  * the statement parameter. The latter allows to use any properties of the parameter as virtual 'components'
	  * of the mapping by providing the getter function. Hence, the first `_.name` here refers to a column
	  * of the table mapping `Dragons`, while the second to a property of mapped entity `Dragon`
	  * (the statement parameter). Equality operator [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]]
	  * is a method of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], forcing an implicit
	  * conversion from a mapping object to an expression object. This will happen also with other standard operators,
	  * such as [[net.noresttherein.oldsql.sql.SQLExpression.> >]] or [[net.noresttherein.oldsql.sql.ColumnSQL.+ +]].
	  *
	  * The ''where'' clause can also be split for convenience into several expressions, joined with
	  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere.or or]]/[[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll.and and]],
	  * which allows the use of `_` placeholders due to referencing the arguments only once. Both `where`
	  * and `and`/`or` methods provide also overloaded version accepting functions of the table mapping only,
	  * allowing use of the shortened lambda syntax also for expressions not dependent on the statement parameter `S`.
	  * The following expressions are equivalent:
	  * {{{
	  *     Delete one Dragons where (_.name === _(_.name)) and (_.race === "Red " ++: _(_.race))
	  *     Delete one Dragons where { (t, p) => t.name === p(_.name) && t.race === "Red " ++: p(_.race) }
	  * }}}
	  * @tparam M    the mapping of the rows in the affected table.
	  * @tparam T    the same mapping as `M`, but as a subtype of `BaseMapping`, as a separate argument for the purpose
	  *              of automatic type inference.
	  * @tparam S    the `Subject` type of `T` and `M` - the Scala type to which all rows in `table` are mapped
	  * @param table the table with deleted entities.
	  * @return      an SQL statement parameterized with the entity type `S` which returns the number of deleted rows.
	  * @see [[net.noresttherein.oldsql.sql.Delete.many]]
	  * @see [[net.noresttherein.oldsql.sql.Delete.apply[S](entity:S)* Delete(entity)]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalFilter]]
	  */
	def one[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	       (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :DeleteOne[S, T] =
		new DeleteOne[S, T](reveal(table))

	/** Creates a parameterized ''delete'' statement batch deleting any number of entities from the given table.
	  * The created [[net.noresttherein.oldsql.sql.DeleteDML DeleteDML]] is parameterized with type `Seq[S]`
	  * (where entity type `S` is the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type of the mapping
	  * used for table rows), to be specified when the
	  * [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[Seq[S], T, Seq[Int]]` based on it is executed:
	  * {{{
	  *     val delete = Delete many Dragons
	  *     val command = delete.chant
	  *     command(Seq(
	  *         Dragon("Firkraag", "Red Dragon", "chaotic evil", 23),
	  *         Dragon("Saladrex", "Red Dragon", "neutral evil", 25)
	  *     ))
	  * }}}
	  * The returned sequence contains the number of deleted matching rows for every entity in the input collection.
	  * The filter condition in the ''where'' clause of the created statement compares all columns of `table`
	  * which are declared as filterable -
	  * the [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] list of the mapping
	  * associated with `table`. All rows matching any of the argument entities are deleted, possibly multiple
	  * for a single entity if its projection to the filter columns above is not unique.
	  *
	  * The list of columns included by default when comparing whole components can be modified
	  * using `table`'s [[net.noresttherein.oldsql.schema.Relation.apply(components* apply]] method:
	  * {{{
	  *     Delete many Dragons(_.level.-)
	  * }}}
	  *
	  * This method is functionally equivalent to
	  * `(Delete.`[[net.noresttherein.oldsql.sql.Delete.one one]]` table).`[[net.noresttherein.oldsql.sql.Delete.batch batch]],
	  * but adds the possibility of supplanting the ''where'' clause using
	  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteMany.where where]] method of the result,
	  * which uses the same entity type as the statement parameter provided as an argument to the constructor function:
	  * {{{
	  *     Delete many Dragons where (_.race === "Red " ++: _(_.race)) or (_.race === "Blue " ++: _(_.race))
	  * }}}
	  * The signatures of available `where` methods,
	  * as well as [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhere.or or]]
	  * and [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhereAll.and and]] which can follow them,
	  * are exactly the same as in `Delete one table`. In any expression, `Delete one` can be substituted
	  * for `Delete many`, and the result of the latter will be a batch of the result of the former.
	  *
	  * Note that the parameter type of the built statement remains `S`/`Seq[S]`; in order to provide an arbitrary type,
	  * use `Delete`[[net.noresttherein.oldsql.sql.Delete.apply[M* (table)]][[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.apply[X:SQLForm] [X] ]].
	  * @tparam M    the mapping of the rows in the affected table.
	  * @tparam T    the same mapping as `M`, but as a subtype of `BaseMapping`, as a separate argument for the purpose
	  *              of automatic type inference.
	  * @tparam S    the `Subject` type of `T` and `M` - the Scala type to which all rows in `table` are mapped
	  * @param table the table with deleted entities.
	  * @return      a DML statement batch parameterized with the sequence of mapped entities `Seq[S]`,
	  *              which returns the number of deleted rows for every `S` in the input sequence.
	  * @see [[net.noresttherein.oldsql.sql.Delete.one]]
	  * @see [[net.noresttherein.oldsql.sql.Delete.apply[S](entities:Seq[S])*]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalFilter]]
	  */
	def many[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	        (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :DeleteMany[S, T] =
		new DeleteMany[S, T](reveal(table))


	/** Creates a ''delete'' statement parameterized with entities `S` to delete from the given table,
	  * which also serves as a factory for various other [[net.noresttherein.oldsql.sql.Delete Delete]] statements,
	  * parameterized both with the entity type `S` and arbitrary types.
	  *
	  * '''Returned statement'''
	  *
	  * The returned `Delete[S, M, Int]` will translate to an [[net.noresttherein.oldsql.sql.Incantation Incantation]]
	  * accepting at execution time arguments of the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
	  * of the mapping used for rows of `table` argument, and returning the number of rows matching the given entity,.
	  * which were deleted as the result of the execution of the incantation. The filter condition in the ''where''
	  * clause of the created statement compares all columns of `table` which are declared as filterable -
	  * the [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] list of the mapping
	  * associated with `table`. All matching rows are deleted, possibly multiple if the projection of the argument
	  * to the filter columns above is not unique.
	  * {{{
	  * val delete = Delete(Dragons)
	  * val command = delete.chant
	  * val deletedCount = command(Dragon("Firkraag", "Red Dragon", "chaotic evil", 23))
	  * val firkraagDead = deleteCount > 0
	  * }}}
	  * The list of columns included by default when comparing whole components can be modified
	  * using `table`'s [[net.noresttherein.oldsql.schema.Relation.apply(components* apply]] method:
	  * {{{
	  *     Delete(Dragons(_.level.-, _.race.-)) //will not include columns 'level' and 'race' in the where clause
	  * }}}
	  * The statement created by this method is essentially equivalent
	  * to `Delete `[[net.noresttherein.oldsql.sql.Delete.one one]]` Dragons`, but provides additional factory methods.
	  *
	  * '''Bound (ground) batches'''
	  *
	  * The result can be immediately [[net.noresttherein.oldsql.sql.DML.BoundDML bound]] by following this call with
	  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.apply(entity:S) apply]]`(entity)`, creating
	  * a parameterless statement. An overloaded
	  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.apply(entities:Seq[S])* variant]] of the method
	  * accepts a sequence of entities, as well as a variant with a variable argument list, allowing to specify
	  * the deleted entities individually:
	  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.apply(first:S* apply]]`(firkraag, saladrex)`.
	  * Both create a [[net.noresttherein.oldsql.sql.DeleteDML DeleteDML]]`[(), Int, Seq[Int]]` returning a sequence
	  * of update counts as the direct result of this method:
	  * {{{
	  * val batch1 = Delete(Dragons)(firkraag, saladrex)       //a vararg variant
	  * val batch2 = Delete(Dragons)(Seq(firkraag, saladrex))  //same effect, convenient for existing collections
	  * }}}
	  * These are equivalent to
	  * `Delete(Dragons).`[[net.noresttherein.oldsql.sql.Delete.batch batch]]`.`[[net.noresttherein.oldsql.sql.DeleteDML.bind bind]]`(dragons)`.
	  *
	  * '''Delete where'''
	  *
	  * The returned object can be also used as a factory of different ''delete'' statements for the same table
	  * and parameterized with the entity type `S`, by providing their ''where'' clause to its
	  * [[net.noresttherein.oldsql.sql.TableStatement.WhereClauseFactory.where(condition:(M* where]] method:
	  * all of the following expressions create a `Delete[Dragon, Dragons, Int]`:
	  * {{{
	  * Delete(Dragons) where (_.level > _.(_.level))                   //condition as a function of (Dragons, Dragon)
	  * Delete(Dragons) where (_.level > 10) and (_.race === _(_.race)) //condition as a function of Dragons
	  * Delete(Dragons) where { (table, param) =>
	  *     table.name === param(_.name) && table.race === param(_.race) && table.level === param(_.level)
	  * }
	  * }}}
	  * The argument function of the methods above is of type
	  * `(M[F], `[[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[S, F]) => `[[net.noresttherein.oldsql.sql.SingleBoolean GlobalBoolean]]`[S]`,
	  * providing access to both the components of the table (the first argument) and the statement parameter
	  * (the second argument) - in the examples, `Dragon` -
	  * as a [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameter expression. In the example, it is used
	  * to create SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] for its various properties
	  * by providing their getter function
	  * to its [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.apply[T](pick:P=>T)* apply]] method.
	  *
	  * The ''where'' clause - and the created ''delete'' statement - can also be parameterized by providing
	  * a type parameter ''before'' `where`:
	  * {{{
	  * Delete(Dragons)[String] where { _.name === _ } //a function of (Dragons[_], UnboundParam[String, _])
	  * Delete(Dragons)[String] where { (t, p) => t.name === p.name || t.race === p.race }
	  * Delete(Dragons)[Int] where (_.level > _) or (_.level > 20)
	  * }}}
	  * The type application returns a [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory factory]]
	  * with `where` methods analogical to the one described earlier, only using a different type of their parameter.
	  *
	  * '''Multi delete'''
	  *
	  * Following this call with [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.* *]]` number` returns
	  * a factory with the same API as the `where` method described earlier, but repeating the provided filter condition
	  * a fixed number of times. The same goes for [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory.* *]]
	  * call following the type application specifying another parameter type:
	  * {{{
	  *     Delete(Dragons) * 8 where (_.name === _(_.name))
	  *     val dragonHunting = Delete(Dragons)[String] * 8 where (_.name === _)
	  *     val hunt = dragonHunting.chant
	  *     hunt(Seq(
	  *         "Abazigal", "Adalon", "Draconis", "Firkraag",
	  *         "Nizidramanii'yt", "Morentherene", "Saladrex", "Thaxll'sillyia"
	  *     ))
	  * }}}
	  * Created statements accept sequences with the specified type parameter as their element type, of length
	  * up to the preset number of repetitions. All rows which match any of the input elements are deleted.
	  * Note that these are statements, ''not'' [[net.noresttherein.oldsql.sql.Delete.batch batches]],
	  * and in most circumstances a batch will yield a better performance; the use of these methods should be restricted
	  * to small and often occurring collection sizes.
	  * @tparam M    the mapping of the rows in the affected table.
	  * @tparam T    the same mapping as `M`, but as a subtype of `BaseMapping`, as a separate argument for the purpose
	  *              of automatic type inference.
	  * @tparam S    the `Subject` type of `T` and `M` - the Scala type to which all rows in `table` are mapped
	  * @param table the table with deleted entities.
	  * @return      a DML statement batch equivalent in functionality to its supertype
	  *              `DeleteDML[Seq[S], Dragons, Seq[Int]]`, but providing additional factory methods.
	  * @see [[net.noresttherein.oldsql.sql.Delete.one]]
	  * @see [[net.noresttherein.oldsql.sql.Delete.apply[S](entities:Seq[S])*]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalFilter]]
	  */
	def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :DeleteFacade[S, T] =
		new DeleteFacade[S, T](reveal(table))


	/** Starts building a parameterless SQL statement deleting the given entity from a table. Returned object defines
	  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundMultiDeleteFactory.from from]] method
	  * accepting a table which maps to the entity type `S`.
	  * The final result is a [[net.noresttherein.oldsql.sql.Delete Delete]]`[(), M, Int]` with a ''where'' clause
	  * matching all columns of the table which do not have the
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]] buff with their corresponding values
	  * in `entity` argument. The complete expression has the form of:
	  * {{{
	  *     Delete(firkraag) from Dragons
	  * }}}
	  * There are overloaded variants of this method accepting multiple entities, both as varargs and a collection.
	  * @return a factory of parameterless ''delete'' statements deleting all rows matching the given entity
	  *         and returning the number of deleted rows.
	  * @see [[net.noresttherein.oldsql.sql.Delete.apply[S](entities:Seq[S])* Delete(entities)]]
	  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.*]]
	  */
	def apply[S](entity :S) :GroundDeleteOneFactory[S] =
		new GroundDeleteOneFactory(entity)

	/** Starts building a parameterless SQL statement deleting the given entities from a table.
	  * Returned object defines [[net.noresttherein.oldsql.sql.Delete.syntax.GroundMultiDeleteFactory.from from]] method
	  * accepting a table which maps to the entity type `S`. The ''where'' clause of the created statement
	  * is a logical disjunction of a conjunction comparing the columns in
	  * `table.row.`[[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]]
	  * with their corresponding values in an argument entity `S`, repeated for every argument entity.
	  * The statement parameters for each repetition of the matching condition are set to the properties of subsequent
	  * entities. The complete expression has the form of:
	  * {{{
	  *     Delete(Dragon("Thaxll'sillyia", "Shadow Dragon", 23), Dragon("Firkraag", "Dragon", 23)) from Dragons
	  * }}}
	  * Note that, unlike
	  *
	  * `Delete(Dragons).`[[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.apply(first:S* (first, second, rest:_*)]]
	  *
	  * the returned object is a single `Delete` statement,
	  * rather than a batch [[net.noresttherein.oldsql.sql.DeleteDML DeleteDML]]. The latter should be preferred
	  * in most cases, unless the number of entities is small, as this method can potentially produce very long
	  * SQL statements and be a source of unnecessarily numerous different statements, which can negatively affect
	  * caching in the driver and the database.
	  *
	  * There also is an overloaded variant of this method accepting a single entity as well as a non-vararg version
	  * accepting a `Seq[S]`, which will be more useful if the deleted entities are known beforehand
	  * rather than listed individually.
	  * @return a factory of parameterless ''delete'' statements deleting all rows matching any of the given entities
	  *         and returning the total number of deleted rows.
	  * @see [[net.noresttherein.oldsql.sql.Delete.apply[S](entities:Seq[S])* Delete(first +: second +: rest)]]
	  * @see [[net.noresttherein.oldsql.sql.Delete.apply[S](entity:S)* Delete(entity)]]
	  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.*]]
	  */
	def apply[S](first :S, second :S, rest :S*) :GroundMultiDeleteFactory[S] = rest match {
		case _ :LinearSeq[S] =>
			new GroundMultiDeleteFactory(first +: second +: rest)
		case _ =>
			val b = ArraySeq.newBuilder(ClassTag[S](classOf[Any]))
			b sizeHint rest.length + 2
			new GroundMultiDeleteFactory((b += first += second ++= rest).result())
	}

	/** Starts building a parameterless SQL statement deleting the given entities from a table. Returned object defines
	  *  [[net.noresttherein.oldsql.sql.Delete.syntax.GroundMultiDeleteFactory.from from]] method
	  * accepting a table which maps to the entity type `S`. The ''where'' clause of the created statement
	  * is a logical disjunction of a conjunction comparing the columns in
	  * `table.row.`[[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]]
	  * with their corresponding values in an argument entity `S`, repeated for every argument entity.
	  * The statement parameters for each repetition of the matching condition are set to the properties of subsequent
	  * elements of the input collection. The complete expression has the form of:
	  * {{{
	  * val dragons = Seq(Dragon("Nizidramanii'ytt", "Shadow Dragon", 25), Dragon("Firkraag", "Red Dragon", 23))
	  * Delete(dragons) from Dragons
	  * }}}
	  * Note that, unlike
	  * `Delete(Dragons).`[[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.apply(first:S* (first, second, rest:_*)]]
	  * the returned object is a single `Delete` statement,
	  * rather than a batch [[net.noresttherein.oldsql.sql.DeleteDML DeleteDML]]. The latter should be preferred
	  * in most cases, unless the number of entities is small, as this method can potentially produce very long
	  * SQL statements and be a source of unnecessarily numerous different statements, which can negatively affect
	  * caching in the driver and the database.
	  *
	  * There also is an overloaded variant of this method accepting a single entity as well as a vararg version,
	  * which allow listing the deleted entities individually, without explicitly putting them in a collection first.
	  * @return a factory of parameterless ''delete'' statements deleting all rows matching any of the given entities
	  *         and returning the number of deleted rows.
	  * @see [[net.noresttherein.oldsql.sql.Delete.apply[S](first:S,second:S* Delete(first, second, ...)]]
	  * @see [[net.noresttherein.oldsql.sql.Delete.apply[S](entity:S)* Delete(entity)]]
	  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.*]]
	  */
	def apply[S](entities :Seq[S]) :GroundMultiDeleteFactory[S] =
		new GroundMultiDeleteFactory(entities)




	/** Namespace for various factory classes and some [[net.noresttherein.oldsql.sql.Delete Delete]] implementations
	  * which are part of the public interface, but are unlikely to be referenced explicitly by the application.
	  * They implement an SQL-like DSL, allowing the creation of ''delete'' statements of various kinds through
	  * method chaining. The classes provided here provide additional APIs for that purpose only and are not intended
	  * to be treated in any special way outside of this context. They are not extensible an extracted here
	  * from `Delete` object in order to leave it with a cleaner interface and more obvious entry points.
	  */
	object syntax {
		/** A factory of [[net.noresttherein.oldsql.sql.Delete Delete]] statements with a single
		  * [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter used in its ''where'' clause.
		  * It accepts the required parameters as separate arguments to chained methods in expressions similar
		  * to real SQL:
		  * {{{
		  *     Delete from <table> where <condition>
		  * }}}
		  * @tparam X the parameter type of the created ''delete'' statement which can be used by the expressions in
		  *           its ''where'' clause.
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteParam.from]]
		  */
		class DeleteParam[X] private[Delete] (private val param :ParamRelation[X]) extends AnyVal {
			/** Starts building a [[net.noresttherein.oldsql.sql.Delete Delete]]`[X, M, Int]` deleting rows
			  * from the given table.
			  * @tparam M    the mapping type of the rows in the affected table.
			  * @tparam T    the same mapping type as `M`,
			  *              but as a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] subtype -
			  *              a separate type argument for automatic type inference of the mapped type `S`
			  * @tparam S    the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
			  *              of the table's mapping `T`, representing its every row.
			  * @param table the table with rows to delete. This allows persistent views without any checking if they
			  *              support delete.
			  * @return      a next stage factory providing methods for adding a ''where'' clause to the statement.
			  */
			def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			        (table :RelVar[M])(implicit reveal :BaseMappingSubject[M, T, S])
					:DeleteWhereFactory[X, M] =
				new DeleteWhereFactory[X, M](From(table) param param)

			/** Starts building a [[net.noresttherein.oldsql.sql.Delete Delete]]`[X, M, Int]` deleting rows
			  * from the given table. This is exactly equivalent
			  * to [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteParam.from from]],
			  * but uses a function application syntax, bringing the expression closer to the SQL statement
			  * it implements.
			  * @tparam M    the mapping type of the rows in the affected table.
			  * @tparam T    the same mapping type as `M`,
			  *              but as a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] subtype -
			  *              a separate type argument for automatic type inference of the mapped type `S`
			  * @tparam S    the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
			  *              of the table's mapping `T`, representing its every row.
			  * @param table the table with rows to delete. This allows persistent views without any checking if they
			  *              support delete.
			  * @return      a next stage factory providing methods for adding a ''where'' clause to the statement.
			  */
			def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			         (table :RelVar[M])(implicit reveal :BaseMappingSubject[M, T, S])
					:DeleteWhereFactory[X, M] =
				new DeleteWhereFactory[X, M](From(table) param param)

			/** Starts building a [[net.noresttherein.oldsql.sql.DeleteDML DeleteDML]]`[Seq[X], M, Seq[Int]]` batch
			  * deleting rows from the given table. On execution, they send in a single call the batched statement
			  * together with all parameter sets of the [[java.sql.PreparedStatement PreparedStatement]] extracted
			  * from subsequent elements of their input collection, and return the numbers of deleted rows for
			  * every input argument. The ''where'' clause of the built statement can be provided using
			  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhereFactory.where(condition:(M* where]] method
			  * of the returned factory.
			  * @tparam M    the mapping type of the rows in the affected table.
			  * @tparam T    the same mapping type as `M`,
			  *              but as a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] subtype -
			  *              a separate type argument for automatic type inference of the mapped type `S`
			  * @tparam S    the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
			  *              of the table's mapping `T`, representing its every row.
			  * @param table the table with rows to delete. This allows persistent views without any checking if they
			  *              support delete.
			  * @return      a next stage factory providing methods for adding a ''where'' clause to the statement.
			  */
			def many[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			        (table :RelVar[M])(implicit reveal :BaseMappingSubject[M, T, S])
					:DeleteManyWhereFactory[X, M] =
				new DeleteManyWhereFactory[X, M](From(table) param param)
		}



		/** A factory of [[net.noresttherein.oldsql.sql.Delete]]`[Arg, M, Int]` statements, allowing to provide
		  * arbitrary SQL Boolean [[net.noresttherein.oldsql.sql.SingleBoolean expressions]]
		  * to their `where` and `whereOld` methods. $methodsInfo
		  * @tparam Arg     the type of the statement parameter, joined by
		  *                 [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] with the modified table to form the domain
		  *                 [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`
		  *                 serving as the domain for the filter condition expressions.
		  * @tparam S       the entity type ([[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  *                 of the mapping of the rows in the affected table).
		  * @tparam M       the mapping type of individual rows in the affected table.
		  * @define Del     `DeleteWhereAll[Arg, S, M]`
		  * @define DelLink [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll DeleteWhereAll]]`[Arg, S, M]`
		  */
		class DeleteWhereFactory[Arg, M[O] <: MappingAt[O]] private[Delete]
		                        (protected override val whereDomain :From[M] WithParam Arg)
			extends AnyVal with WhereClauseFactory[Arg, M, DeleteWhereAll[Arg, M]]
		{
			@inline protected override def table :RelVar[M] = whereDomain.left.table.asInstanceOf[RelVar[M]]

			protected override def where(condition :SingleBoolean[From[M] WithParam Arg]) :DeleteWhereAll[Arg, M] =
				new DeleteWhereAll(table, condition, whereDomain)

			/** Creates a factory of statements which contain `max` copies of the same condition
			  * in their ''where'' clause, joined in a logical alternative. The API of the factory mirrors this one,
			  * including [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhere.or or]] and
			  * [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhereAll.and and]] methods available
			  * on results of its [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhereFactory.where where]]
			  * methods. While the ''where'' clause is created as in the case of this object's `where` method,
			  * in terms of a single parameter `Arg`, created statements are parameterized with `Seq[Arg]`,
			  * and each repetition of the provided condition in the ''where'' clause has the JDBC parameters
			  * set to values extracted from subsequent elements of the input collection.
			  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhere]]
			  */
			def *(max :Int) :MultiDeleteWhereFactory[Arg, M] =
				new MultiDeleteWhereFactory[Arg, M](table, whereDomain, max)
		}


		/** An implementation of the parameterized ''delete'' statement `ParamDelete`, relying
		  * on a single [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `Arg` in its ''where'' clause,
		  * to be provided when an [[net.noresttherein.oldsql.sql.Incantation Incantation]] created for this statement
		  * is executed. The result of the execution is the number of deleted rows.
		  * The condition for the ''where'' clause can be supplied as an argument to
		  * `Delete(table) `[[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory.where where]]` (...)`,
		  * but can also be a logical conjunction of several conditions coming from chaining
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere.and and]] calls and passing several
		  * condition expressions:
		  * {{{
		  *     Delete(Dragons) where (_.race === "Red " ++ _.(_.race)) or (_.race === "Blue " ++ _.(_.race))
		  *     Delete(Dragons)[(String, String)] where (_.name === _.(_._1)) or (_.name === _.(_._2))
		  * }}}
		  * $methodsInfo
		  * @tparam Arg the type of the only parameter of the statement, in the sense that its ''where'' condition
		  *             is an expression based on [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
		  *             [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
		  * @tparam S   the entity type mapped to the table from which the rows are deleted.
		  * @tparam M   the type of the mapping used for rows of the table affected by this statement.
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.*]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.by]]
		  * @define Or           `DeleteWhere[Arg, S, M]`
		  * @define OrLink       [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere DeleteWhere]]
		  * @define AndLink      [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll DeleteWhereAll]]
		  * @define Del          `DeleteWhere[Arg, M, Int]`
		  * @define DelLink      [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere DeleteWhere]]`[Arg, M, Int]`
		  */ //the condition must use the exact ParamRelation from domain, or problems arise.
		sealed class DeleteWhere[Arg, M[O] <: MappingAt[O]] private[Delete]
		                        (override val table :RelVar[M],
		                         override val condition :SingleBoolean[From[M] WithParam Arg],
		                         protected override val domain :From[M] WithParam Arg)
			extends ParamDelete[Arg, M] with WhereAnyClauseFactory[Arg, M, DeleteWhere[Arg, M]]
		{
			protected override def whereDomain :From[M] WithParam Arg = domain

			override def or(condition :SingleBoolean[From[M] WithParam Arg]) :DeleteWhere[Arg, M] =
				new DeleteWhere[Arg, M](table, this.condition || condition.anchor(domain), domain)
		}


		/** The default Implementation of the parameterized ''delete'' statement `ParamDelete`,
		  * with a ''where'' clause condition which can be a conjunction of several expressions passed to chained
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll.and and]] calls.
		  * Calling [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere.or or]] ends the conjunction,
		  * returning a [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere DeleteWhere]] which allows only
		  * `or` calls, due to difficulty to realise proper operator precedence by method chaining and possibility
		  * for confusion. More complex expression should be simply given using the standard Scala lambda syntax,
		  * without `_` placeholders.
		  *
		  * All constituent expressions in the ''where'' clause may rely on a single
		  * [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `Arg`,
		  * to be provided when an [[net.noresttherein.oldsql.sql.Incantation Incantation]] created for this statement
		  * is executed. The result of the execution is the number of deleted rows.
		  *
		  * An instance of this class is created by all
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory.where where]] methods of parameterized
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteAll statements]] and their
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory factories]]:
		  * {{{
		  *     Delete(Dragons) where (_.name === _.(_.name)) and (_.race === _.(_.race))
		  *     Delete(Dragons)[(String, String)] where (_.name === _.(_._1)) and (_.race === _.(_._2))
		  * }}}
		  * $methodsInfo
		  * @tparam Arg the type of the only parameter of the statement, in the sense that its ''where'' condition
		  *             is an expression based on [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
		  *             [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
		  * @tparam S   the entity type mapped to the table from which the rows are deleted.
		  * @tparam M   the type of the mapping used for rows of the table affected by this statement.
		  * @define And     `DeleteWhereAll`
		  * @define Del     `DeleteWhereAll[Arg, M, Int]`
		  * @define DelLink [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll DeleteWhereAll]]`[Arg, M, Int]`
		  */ //the condition must use the exact ParamRelation from domain, or problems arise.
		final class DeleteWhereAll[Arg, M[O] <: MappingAt[O]] private[Delete]
		                          (override val table :RelVar[M],
		                           override val condition :SingleBoolean[From[M] WithParam Arg],
		                           override val domain :From[M] WithParam Arg)
			extends DeleteWhere[Arg, M](table, condition, domain)
			   with WhereAllClauseFactory[Arg, M, DeleteWhere[Arg, M], DeleteWhereAll[Arg, M]]
		{
			override def and(condition :SingleBoolean[From[M] WithParam Arg]) :DeleteWhereAll[Arg, M] =
				new DeleteWhereAll[Arg, M](table, this.condition && condition.anchor(domain), domain)
		}



		/** A factory creating [[net.noresttherein.oldsql.sql.Delete Delete]]`[Seq[Arg], T, Int]` statements
		  * which delete rows matching at least one of up to `max` arguments `Arg`.
		  * It defines `where` and `whereOld` methods which accept filter conditions for the created statements.
		  * $methodsInfo
		  * @tparam Arg     the element type of the input sequence, used as the only parameter of the statement,
		  *                 in the sense that its ''where'' condition is an expression based on
		  *                 [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
		  *                 [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
		  * @tparam S       the entity type - the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type
		  *                 of mapping `M` used for rows in `table`.
		  * @tparam M       the mapping used by `table`.
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhere]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.*]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.apply[M*]]
		  * @define Args         `Seq[Arg]`
		  * @define Del          `MultiDeleteWhereAll[Arg, S, M]`
		  * @define DelLink      [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhereAll MultiDeleteWhereAll]]`[Arg, S, M]`
		  * @define delResult    deleting all rows in the table for which the condition evaluates to `true` for
		  *                      at least one element of the input, returning their number without specifying
		  *                      which parameters where matched.
		  * @define examplesInfo The following expressions are fully equivalent:
		  * {{{
		  * Delete(Characters) * 5 whereOld (_.isMagicUser && _.magicSchool ==? "conjurer") and (_.level >= _(_.level))
		  * Delete(Characters) * 5 where { (t, p) =>
		  *     t.isMagicUser && t.magicSchool =? "conjurer" && t.level >= p.level
		  * }
		  * }}}
		  */
		sealed class MultiDeleteWhereFactory[Arg, M[O] <: MappingAt[O]] private[Delete]
		                                    (override val table :RelVar[M],
		                                     protected override val whereDomain :From[M] WithParam Arg, val max :Int)
			extends WhereClauseFactory[Arg, M, MultiDeleteWhereAll[Arg, M]]
		{
			protected def domain :From[M] WithParam Seq[Arg] =
				whereDomain.left.param[Seq[Arg]](whereDomain.right.row.form * max)

			protected override def where(condition :SingleBoolean[From[M] WithParam Arg]) :MultiDeleteWhereAll[Arg, M] =
				new MultiDeleteWhereAll[Arg, M](table, domain, whereDomain, condition, max)
		}


		/** Shared implementation of [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteEntity MultiDeleteEntity]]
		  * and its variants. It includes the full implementation of `ParamDelete` trait, except for
		  * value properties - subclasses add only `where`, `and`, `or` methods, as appropriate.
		  * @tparam Arg     the element type of the input sequence, used as the only parameter of the statement,
		  *                 in the sense that its ''where'' condition is an expression based on
		  *                 [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
		  *                 [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
		  * @tparam S       the entity type - the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type
		  *                 of mapping `M` used for rows in `table`.
		  * @tparam M       the mapping used by `table`.
		  */
		private[Delete] sealed trait MultiDelete[Arg, M[O] <: MappingAt[O]] extends ParamDelete[Seq[Arg], M] {
			protected val whereDomain :From[M] WithParam Arg
			protected val single :SingleBoolean[From[M] WithParam Arg]
			val max :Int

			if (max <= 0)
				throw new IllegalArgumentException(
					"Cannot repeat condition " + single + " for " + table + " a non positive number of times: "+ max + "."
				)

			override lazy val condition :SingleBoolean[From[M] WithParam Seq[Arg]] =
				TableStatement.repeatedWhere[Arg, M](domain, whereDomain, single, max)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Seq[Arg]] =
				if (max == 0)
					Delete.spell(this)(False)(domain, Parameterization.paramless)
				else {
					val part = Delete.spell(this)(single)(whereDomain, whereDomain.parameterization)
					val form = (0 until max).view.map {
						i => part.setter.compose(
							Optional[Seq[Arg], @~ ~ Arg] { args => if (args.sizeIs > i) Got(@~ ~ args(i)) else Lack }
						)
					}.toList.reduce(_ + _)
					SpelledSQL(part.sql * max, form, part.context.reset())
				}

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :MultiDelete[_, _] =>
					(other canEqual this) && other.table == table && other.single == single && other.max == max
				case other :ParamDelete[_, _] =>
					(other canEqual this) && other.hashCode == hashCode &&
						other.table == table && other.condition == condition
				case _ => false
			}
		}


		/** A parameterized ''delete'' statement which contains `max` repetitions of the same condition matching
		  * elements of the input collection, specified at the execution time, with filterable columns of `table`.
		  * The repetitions are joined in a logical disjunction and are themselves logical conjunctions of tests
		  * for individual columns listed
		  * by [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] property of mapping `M`.
		  * The parameters for every equality test are extracted by the mapping from subsequent elements
		  * of the input collection given at the execution time. The input collection can have fewer elements
		  * than specified, in which case extra repetitions are simply made redundant, but if its size exceeds `max`
		  * an [[IllegalArgumentException]] will be thrown. The statement returns the total number of deleted rows,
		  * without specifying which of the input entities they matched. Instances can be created using method
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.* DeleteFacade.*]]
		  * as in the following expression:
		  * {{{
		  *     Delete(Dragons) * 5
		  * }}}
		  * The ''where'' clause of the above expression can be replaced with an arbitrary
		  * SQL Boolean [[net.noresttherein.oldsql.sql.SingleBoolean expression]] by `where` and `whereOld` methods
		  * of this instance, which create new `Delete` statements using the same parameter type `S`. $methodsInfo
		  * @tparam S       the entity type - the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type
		  *                 of mapping `M` used for rows in `table`.
		  * @tparam M       the mapping used by `table`.
		  * @see [[net.noresttherein.oldsql.sql.Delete.apply[M*]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.many]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteMany]]
		  * @define Args    `Seq[S]`
		  * @define Arg     `S`
		  * @define Del     `MultiDeleteWhereAll[S, S, M]`
		  * @define DelLink [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhereAll MultiDeleteWhereAll]]`[S, S, M]`
		  */ //consider: a better, more universal name - or swap with its supertype
		final class MultiDeleteEntity[S, M[O] <: BaseMapping[S, O]] private[Delete]
		                             (override val table :RelVar[M],
		                              protected override val whereDomain :From[M] WithParam S, override val max :Int)
			extends MultiDeleteWhereFactory[S, M](table, TableStatement.whereDomain(table), max)
			   with MultiDelete[S, M]
		{
			protected override val domain :From[M] WithParam Seq[S] = super.domain
			protected override val single :SingleBoolean[From[M] WithParam S] =
				TableStatement.whereEntity(table, whereDomain)

//			val condition :GlobalBoolean[From[M] WithParam Seq[S]] = repeats match {
//				case n if n < 0 =>
//					throw new IllegalArgumentException(
//						"Negative number of alternative condition sets " + repeats + " given to MultiDeleteEntity(" + table + ")."
//					)
//				case 0 => False
//				case n =>
//					type F = From[M] WithParam Seq[S]
//					val mapping = table.export[F].asInstanceOf[TypedMapping[S, F]]
//					val params = domain.right.apply[F]
//					//todo: probably should use primary key - or assume entity mappins buff everything else with NoFilterByDefault
//					OrSQL((0 until n).map { i =>
//						(True[F] /: mapping.filteredByDefault) { (cond, col) =>
//							def filter[T](column :TypedColumn[T, F]) :GlobalBoolean[F] = {
//								implicit val form = column.form
//								val columnExtract = mapping(column)
//								val paramColumn = params.comp(Extractor.Optional[Seq[S], T] { ps =>
//									if (ps.sizeIs > i) Got(columnExtract(ps(i))) else Lack
//								})
//								column.anchor(domain) === paramColumn.anchor(domain)
//							}
//							cond && filter(col)
//						}
//					} :_*)
//			}
//
//			protected override def visit[R[-X, +Y]](mapper :StatementVisitor[R]) :R[Seq[S], Int] =
//				mapper.deleteAny(this)

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :MultiDeleteEntity[_, _] if other canEqual this => table == other.table && max == other.max
				case _ => super.equals(that)
			}
			protected override def initToString :String = "Delete(?*" + max + ") from " + table
		}


		/** A parameterized ''delete'' statement which contains `max` repetitions of the same condition, joined
		  * in a logical disjunction. Its ''where'' clause can be modified by adding another condition
		  * to the disjunction using its [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhere.or or]] and
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhere.orSelf orSelf]] methods.
		  * $methodsInfo
		  *
		  * $repeatInfo
		  * @tparam Arg     the element type of the input sequence, used as the only parameter of the statement,
		  *                 in the sense that its ''where'' condition is an expression based on
		  *                 [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
		  *                 [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
		  * @tparam S       the entity type - the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type
		  *                 of mapping `M` used for rows in `table`.
		  * @tparam M       the mapping used by `table`.
		  * @see [[net.noresttherein.oldsql.sql.Delete.apply[M*]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteEntity]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.many]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteMany]]
		  * @define Args         `Seq[Arg]`
		  * @define Or           `MultiDeleteWhere`
		  * @define OrLink       [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhere MultiDeleteWhere]]
		  * @define AndLink      [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhereAll MultiDeleteWhereAll]]
		  * @define Del          `MultiDeleteWhere[Arg, S, M]`
		  * @define DelLink      [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhere MultiDeleteWhere]]`[Arg, S, M]`
		  * @define delResult    deleting all rows in the table for which the condition evaluates to `true` for
		  *                      at least one element of the input, returning their number without specifying
		  *                      which parameters where matched.
		  * @define repeatInfo   The values of JDBC parameters used in each occurrence of the repeated condition
		  *                      are set from subsequent elements of the input collection `Seq[Arg]` using
		  *                      an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[Arg]`. The input collection
		  *                      can have fewer elements than specified, in which case extra repetitions are simply
		  *                      made redundant, but if its size exceeds `max` an [[IllegalArgumentException]]
		  *                      will be thrown. The statement returns the total number of deleted rows, without
		  *                      specifying which of the input elements they matched. Instances can be created
		  *                      starting with methods
		  *                      [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.* DeleteFacade.*]] and
		  *                      [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory.* DeleteWhereFactory.*]]
		  *                      as in the following expressions:
		  * {{{
		  *     Delete(Dragons) * 5 where (_.level === _(_.level))
		  *     Delete(Dragons)[String] * 5 where (_.level === _)
		  * }}}
		  * @define examplesInfo The following expressions are fully equivalent:
		  * {{{
		  * Delete(Dragons)[Int] * 5 where (_.level === _) or (_.level >= 10 || _.strength > 23 && _.armorClass < 0)
		  * Delete(Dragons)[Int] * 5 where { (t, p) =>
		  *     t.level === p || t.level >= 10 || t.strength > 23 && t.armorClass < 0
		  * }
		  * }}}
		  */
		sealed class MultiDeleteWhere[Arg, M[O] <: MappingAt[O]] private[Delete]
		                             (override val table :RelVar[M],
		                              protected override val domain :From[M] WithParam Seq[Arg],
		                              protected override val whereDomain :From[M] WithParam Arg,
		                              protected override val single :SingleBoolean[From[M] WithParam Arg],
		                              override val max :Int)
			extends MultiDelete[Arg, M] with WhereAnyClauseFactory[Arg, M, MultiDeleteWhere[Arg, M]]
		{
			protected override def or(condition :SingleBoolean[From[M] WithParam Arg]) :MultiDeleteWhere[Arg, M] =
				new MultiDeleteWhere[Arg, M](
					table, domain, whereDomain, single || condition.anchor(whereDomain), max
				)
		}


		/** A parameterized ''delete'' statement which contains `max` repetitions of the same condition, joined
		  * in a logical disjunction. The condition itself is either the one passed to
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteEntity.where MultiDeleteEntity.where]]/[[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhereFactory.where MultiDeleteWhereFactory.where]]
		  * in order to create an instance, or its logical conjunction with condition arguments specified in subsequent
		  * calls to [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhereAll.and and]] method of other instances
		  * of this class. Its ''where'' clause can be further modified by adding another condition
		  * to the disjunction using its [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhereAll.and and]] and
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhereAll.andSelf andSelf]] methods.
		  * $methodsInfo
		  *
		  * $repeatInfo
		  * @tparam Arg     the element type of the input sequence, used as the only parameter of the statement,
		  *                 in the sense that its ''where'' condition is an expression based on
		  *                 [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
		  *                 [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
		  * @tparam S       the entity type - the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type
		  *                 of mapping `M` used for rows in `table`.
		  * @tparam M       the mapping used by `table`.
		  * @see [[net.noresttherein.oldsql.sql.Delete.apply[M*]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteEntity]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.many]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteMany]]
		  * @define Args         `Seq[Arg]`
		  * @define And          `MultiDeleteWhere`
		  * @define Del          `MultiDeleteWhere[Arg, S, M]`
		  * @define DelLink      [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteWhere MultiDeleteWhere]]`[Arg, S, M]`
		  * @define examplesInfo The following expressions are fully equivalent:
		  * {{{
		  * Delete(Dragons)[Int] * 5 where (_.level === _) or (_.level >= 10 || _.strength > 23 && _.armorClass < 0)
		  * Delete(Dragons)[Int] * 5 where { (t, p) =>
		  *     t.level === p || t.level >= 10 || t.strength > 23 && t.armorClass < 0
		  * }
		  * }}}
		  */
		final class MultiDeleteWhereAll[Arg, M[O] <: MappingAt[O]] private[Delete]
		                               (override val table :RelVar[M],
		                                protected override val domain :From[M] WithParam Seq[Arg],
		                                protected override val whereDomain :From[M] WithParam Arg,
		                                protected override val single :SingleBoolean[From[M] WithParam Arg],
		                                override val max :Int)
			extends MultiDeleteWhere[Arg, M](table, domain, whereDomain, single, max)
			   with WhereAllClauseFactory[Arg, M, MultiDeleteWhere[Arg, M], MultiDeleteWhereAll[Arg, M]]
		{
			protected override def and(condition :SingleBoolean[WithParam[From[M], Arg]]) :MultiDeleteWhereAll[Arg, M] =
				new MultiDeleteWhereAll[Arg, M](
					table, domain, whereDomain, single && condition.anchor(whereDomain), max
				)
		}



		/** A parameterized ''delete'' statement accepting an entity `S` being
		  * the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type of the mapping for the given table
		  * and deleting all matching rows. The ''where'' clause tests all
		  * [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] columns of mapping `M`
		  * for equality with JDBC parameters, set to values extracted from an argument entity using the mapping
		  * when the [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[S, Int]`
		  * created from this statement is executed. While its primary use case is deletion of single rows,
		  * any number can be deleted as the result if the projection to filterable columns is not unique.
		  * The statement (or rather, the incantation created from it) returns the number of rows actually deleted.
		  *
		  * An instance of this class is created by expression `Delete one table`.
		  * That expression can be fallowed with a call
		  * to [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteOne.where where]] method on
		  * the returned (this) object, which allows to supplant completely the default ''where'' clause with
		  * another SQL Boolean [[net.noresttherein.oldsql.sql.SingleBoolean expression]] as a function
		  * of the table mapping `M` and parameter mapping for the entity type `S`:
		  * {{{
		  *     Delete one Dragons where (_.name === _(_.name)) and (_.race === "Red " ++: _(_.race))
		  * }}}
		  * In the above expression, the first placeholder parameter `_` refers to
		  * `Dragons.`[[net.noresttherein.oldsql.schema.Relation.row row]] mapping, while the second to
		  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[Dragon, _]` synthetic mapping for
		  * the statement parameter. The latter allows to use any properties of the parameter as virtual 'components'
		  * of the mapping by providing the getter function. Hence, the first `_.name` here refers to a column
		  * of the table mapping `Dragons`, while the second to a property of mapped entity `Dragon`
		  * (the statement parameter). Equality operator [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]]
		  * is a method of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], forcing an implicit
		  * conversion from a mapping object to an expression object of the same subject/value type. This will happen
		  * also with other standard operators, such as [[net.noresttherein.oldsql.sql.SQLExpression.> >]]
		  * or [[net.noresttherein.oldsql.sql.ColumnSQL.+ +]].
		  *
		  * As pictured, the ''where'' clause can be split for convenience into several expressions, joined with
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere.or or]]/[[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll.and and]],
		  * which allows the use of `_` placeholders due to referencing the arguments only once. Both `where`
		  * and `and`/`or` methods provide also overloaded version accepting functions of the table mapping only,
		  * allowing use of the shortened lambda syntax also for expressions not dependent on the statement parameter `S`.
		  * The above expression is completely equivalent to
		  * {{{
		  *     Delete one Dragons where { (t, p) => t.name === p(_.name) && t.race === "Red " ++: p(_.race) }
		  * }}}
		  * @tparam S       the entity type - the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type
		  *                 of mapping `M` used for rows in `table`.
		  * @tparam M       the mapping used by `table`.
		  * @define Args    `S`
		  * @define Arg     `S`
		  * @define Del     `DeleteWhereAll[S, S, M]`
		  * @define DelLink [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll DeleteWhereAll]]`[S, S, M]`
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll]]
		  *///fixme: null columns should be excluded from matching.
		sealed class DeleteOne[S, M[O] <: BaseMapping[S, O]] private[Delete](override val table :RelVar[M])
			extends ParamDelete[S, M] with WhereClauseFactory[S, M, DeleteWhereAll[S, M]]
		{
			protected override val domain :From[M] WithParam S = TableStatement.whereDomain[S, M](table)
			protected override def whereDomain :From[M] WithParam S = domain

			protected override def where(condition :SingleBoolean[From[M] WithParam S]) :DeleteWhereAll[S, M] =
				new DeleteWhereAll[S, M](table, condition, domain)

			override lazy val condition :SingleBoolean[From[M] WithParam S] =
				TableStatement.whereEntity[S, M](table, domain)

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :DeleteOne[_, _] => other.table == table
				case other :ParamDelete[_, _] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && condition == other.condition
				case _ => false
			}
			protected override def initToString :String = "Delete(?) from " + table
		}


		/** A parameterized ''delete'' statement accepting an entity `S` being
		  * the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type of the mapping for the given table
		  * and deleting all matching rows. The ''where'' clause tests all
		  * [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] columns of mapping `M`
		  * for equality with JDBC parameters, set to values extracted from an argument entity using the mapping
		  * when the [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[S, Int]`
		  * created from this statement is executed. While its primary use case is deletion of single rows,
		  * any number can be deleted as the result if the projection to filterable columns is not unique.
		  * The statement (or rather, the incantation created from it) returns the number of rows actually deleted.
		  *
		  * At the same time, for syntactical reasons, this class defines several factory methods for various
		  * other ''delete'' statements for the same table:
		  *   1. [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.where where]] -
		  *      a statement of the same type which uses the provided Boolean
		  *      [[net.noresttherein.oldsql.sql.SingleBoolean condition]] as its ''where'' clause, replacing
		  *      the default one based on the mapping `M`:
		  *      {{{
		  *          Delete(Dragons) where (_.name === _(_.name)) and (_.race === "Red " ++: _(_.race))
		  *      }}}
		  *      $methodsInfo
		  *   1. [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.apply[X:SQLForm]* apply]]`[X]`/ [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.using using]]`[X]` -
		  *      a [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory factory]] of statements
		  *      parameterized with arbitrary types, providing the exact same `where` method as this instance,
		  *      but exposing a statement parameter of the given type rather than the entity type `S`:
		  *      {{{
		  *          Delete(Dragons)[String] where (_.race === _)
		  *      }}}
		  *   1. Parameterless statements deleting given entities - essentially a shortened syntax for
		  *      [[net.noresttherein.oldsql.sql.Delete.batch batch]] and [[net.noresttherein.oldsql.sql.Delete.bind bind]]:
		  *      {{{
		  *          Delete(Dragons)(firkraag)            //Delete(Dragons).bind(firkraag)
		  *          Delete(Dragons)(firkraag, saladrex)  //Delete(Dragons).batch.bind(firkraag::saldrex::Nil)
		  *          Delete(Dragons)(dragons)            //Delete(Dragons).batch.bind(dragons)
		  *      }}}
		  *   1. [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.* Multi-delete]] statements containing
		  *      a given (parameterized) condition repeated several times and joined in a logical disjunction.
		  *      They accept a `Seq[P]` as input and set the parameters within each occurrence of the repeated condition
		  *      based on the values of subsequent input parameters. It is most useful when deleting multiple
		  *      entities, but can be defined with any ''where'' clause condition and for any parameter type:
		  *      {{{
		  *          Delete(Dragons) * 2 //"Delete dragons where (name=? and race=?) or (name=? and race=?)"
		  *          Delete(Dragons)[String] * 6 where (_.name === _)
		  *      }}}
		  *      Note the use of `_` placeholders: first for the table mapping `M[_]`, and second for
		  *      the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam parameter mapping]]. The statements returned
		  *      by all `where` allow appending additional conditions in the same way, joining them with either ''and''
		  *      or ''or'', as specified, to allow repeated use of this shortened syntax.3
		  */
		final class DeleteFacade[S, M[O] <: BaseMapping[S, O]] private[Delete](override val table :RelVar[M])
			extends DeleteOne[S, M](table)
		{
			/** Creates a parameterless DML statement deleting all rows matching the given entity.
			  * The ''where'' clause of the returned instance will be the same as this one, if the JDBC parameters
			  * initialized to values extracted from `entity` by the mapping `M` of `table`.
			  * Same as [[net.noresttherein.oldsql.sql.Delete.bind bind]].
			  */
			def apply(entity :S) :Delete[Unit, M, Int] = bind(entity)

			/** Creates a parameterless DML batch deleting all rows matching any of the given entities.
			  * The batch will execute this statement for all of the listed entities in a single database call.
			  * This is a simple shortcut for
			  * `this.`[[net.noresttherein.oldsql.sql.Delete.batch batch]]`.`[[net.noresttherein.oldsql.sql.Delete.bind bind]]`(first +: second +: rest)`
			  * @return A DML batch returning a sequence of length equal to the number of given arguments,
			  *         with each element being the number of rows which matched the corresponding argument of this method.
			  */
			def apply(first :S, second :S, rest :S*) :DeleteDML[Unit, M, Seq[Int]] = rest match {
				case _ :LinearSeq[_] =>
					apply(first +: second +: rest)
				case _ =>
					val b = ArraySeq.newBuilder(ClassTag[S](classOf[Any]))
					b sizeHint rest.length + 2
					apply((b += first += second ++= rest).result())
			}

			/** Creates a parameterless DML batch deleting all rows matching any of the given entities.
			  * The batch will execute this statement for all of the listed entities in a single database call.
			  * This is a simple shortcut for
			  * `this.`[[net.noresttherein.oldsql.sql.Delete.batch batch]]`.`[[net.noresttherein.oldsql.sql.Delete.bind bind]]`(entities)`
			  * @return A DML batch returning a sequence of length equal to the number of given arguments,
			  *         with each element being the number of rows which matched the corresponding entity
			  *         in the argument sequence.
			  */
			def apply(entities :Seq[S]) :DeleteDML[Unit, M, Seq[Int]] = batch.bind(entities)

			/** Creates a DML statement deleting a predefined number of entities at once. It deletes all rows in `table`
			  * given as an argument to method `Delete`[[net.noresttherein.oldsql.sql.Delete.apply[M* (table)]]
			  * which match one of up to `max` entities `S` mapping to the rows of `table`. The ''where'' clause
			  * of the created statement is a logical disjunction of `max` repetitions of a conjunction comparing
			  * the columns in `table.row.`[[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]]
			  * with their corresponding values in an argument entity `S`. The statement parameters of each repetition
			  * of the matching condition are set at execution time to the subsequent elements of argument `Seq[S]`.
			  * If the argument sequence has fewer elements than the required number of parameters, the missing parameters
			  * are set to `null`, which will cause the whole (single) conjunction to evaluate to `null` in ternary logic,
			  * having the same effect as if it was not present. If the argument sequence has more than `max` elements,
			  * an [[IllegalArgumentException]] will be thrown. The result of the method is an instance
			  * of [[net.noresttherein.oldsql.sql.Delete Delete]]`[Seq[Dragon], Dragons, Int]`,
			  * with a ''where'' clause containing the matching condition for `Dragon` repeated `5` times,
			  * joined in a logical alternative. It can be optionally followed
			  * with [[net.noresttherein.oldsql.sql.Delete.syntax.MultiDeleteEntity.where where]] method of the result
			  * to provide a different filter condition:
			  * {{{
			  *     Delete(Dragons) * 5 where (_.name === _(_.name))
			  * }}}
			  * Alternatively, the list of compared columns can be modified using `table`'s
			  * [[net.noresttherein.oldsql.schema.Relation.apply(components* apply]] method:
			  * {{{
			  *     Delete(Dragons(_.race.-, _.level.-)) * 5
			  * }}}
			  * The ''where'' clause of the SQL of the above expression will consist of a single condition `name = ?`
			  * (assuming column 'name' is on the `filteredByDefault` list).
			  *
			  * Note the difference from the DML ''batch'' `Delete `[[net.noresttherein.oldsql.sql.Delete.many many]]` Dragons`,
			  * which doesn't impose a limit on the deleted entities and executes a single
			  * `Delete `[[net.noresttherein.oldsql.sql.Delete.one one]]` Dragons` repeatedly for all arguments
			  * in a single database round trip. In most cases, it is preferable to this method due to higher generality,
			  * and smaller (and unique) resulting SQL statement; it also returns the numbers of deleted rows
			  * individually for every argument entity, rather than a total as in a statement created
			  * by the returned factory.
			  * @param max the maximum number of entities possible to delete by the created statement. It should be
			  *            of moderate magnitude in order to avoid generation of unnecessarily large SQL statements.
			  */
			def *(max :Int) :MultiDeleteEntity[S, M] = new MultiDeleteEntity(table, domain, max)

			/** A factory of statements parameterized with type `X`, using arbitrary
			  * [[net.noresttherein.oldsql.sql.SingleBoolean Boolean]] expressions as filter conditions.
			  * The returned factory provides the same
			  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory.where where]] methods as
			  * this instance, but using `X` as the [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameter
			  * including in the
			  * [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` X`
			  * domain of the used [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
			  */
			def apply[X :SQLForm] :DeleteWhereFactory[X, M] = new DeleteWhereFactory[X, M](From(table).param[X])

			/** A factory of statements parameterized with type `X`, using arbitrary
			  * [[net.noresttherein.oldsql.sql.SingleBoolean Boolean]] expressions as filter conditions.
			  * The returned factory provides the same
			  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory.where where]] methods as
			  * this instance, but using `X` as the [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameter
			  * including in the
			  * [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` X`
			  * domain of the used [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
			  */
			def using[X :SQLForm] :DeleteWhereFactory[X, M] = new DeleteWhereFactory[X, M](From(table).param[X])

			/** A factory of statements parameterized with type `X`, using arbitrary
			  * [[net.noresttherein.oldsql.sql.SingleBoolean Boolean]] expressions as filter conditions.
			  * The returned factory provides the same
			  * [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory.where where]] methods as
			  * this instance, but using `X` as the [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameter
			  * including in the
			  * [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` X`
			  * domain of the used [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
			  */
			def using[X](param :ParamRelation[X]) :DeleteWhereFactory[X, M] =
				new DeleteWhereFactory[X, M](From(table) param param)

			//todo: aliased where domain
			def using[N <: Label, X](param :NamedParamRelation[N, X]) :DeleteWhereFactory[X, M] =
				new DeleteWhereFactory[X, M](From(table) param param)
		}



		/** A factory of [[net.noresttherein.oldsql.sql.DeleteDML DeleteDML]]`[Seq[Arg], M, Seq[Int]]` batches executing
		  * a single parameterized statement [[net.noresttherein.oldsql.sql.Delete Delete]]`[Arg, S, M]` for multiple
		  * arguments in a single execution of the created [[net.noresttherein.oldsql.sql.Incantation Incantation]].
		  *
		  * The ''where'' clause of the created instances can be set to arbitrary
		  * [[net.noresttherein.oldsql.sql.SingleBoolean GlobalBoolean]]`[From[M] WithParam Arg]`
		  * using `where` and `whereOld` methods, as in the case of single statements. $methodsInfo
		  * @tparam Arg          the type of the statement parameter, joined by
		  *                      [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] with the modified table to form the domain
		  *                      [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`
		  *                      serving as the domain for the filter condition expressions.
		  * @tparam S            the entity type ([[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  *                      of the mapping of the rows in the affected table).
		  * @tparam M            the mapping type of individual rows in the affected table.
		  * @define Args         `Seq[Arg]`
		  * @define Del          `DeleteManyWhereAll[Arg, S, M]`
		  * @define DelLink      [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhereAll DeleteManyWhereAll]]`[Arg, S, M]`
		  * @define delResult    deleting all rows in the table for which `condition` evaluates to `true` for any
		  *                      of the elements in input, returning the number of all matched rows for every argument.
		  */
		class DeleteManyWhereFactory[Arg, M[O] <: MappingAt[O]] private[Delete]
		                            (protected override val whereDomain :From[M] WithParam Arg)
			extends AnyVal with WhereClauseFactory[Arg, M, DeleteManyWhereAll[Arg, M]]
		{
			@inline protected override def table :RelVar[M] = whereDomain.left.table.asInstanceOf[RelVar[M]]

			protected override def where(condition :SingleBoolean[From[M] WithParam Arg]) :DeleteManyWhereAll[Arg, M] =
				new DeleteManyWhereAll(new DeleteWhereAll(table, condition, whereDomain))
		}


		/** A ''delete'' statement batch deleting all rows matching subsequent elements of the input collection `Seq[S]`
		  * specified at execution time, by repeated execution of an SQL ''delete'' for a single entity.
		  * The executed statement is [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteOne DeleteOne]],
		  * with multiple parameter sets combined in a batched [[java.sql.PreparedStatement PreparedStatement]].
		  *
		  * The ''where'' clause of this instance can be supplanted with an arbitrary
		  * SQL Boolean [[net.noresttherein.oldsql.sql.SingleBoolean expression]] using `where` and `whereOld` methods
		  * just as with a single statement `Delete one ...`. $methodsInfo
		  *
		  * The parameter type of the created batches always remains `Seq[S]` (and `S` for the batched statement,
		  * which defines the parameter type accepted by argument functions to `where` methods);
		  * if you wish to provide an arbitrary type different than the entity mapped to this table, use either
		  *   - `Delete.`[[net.noresttherein.oldsql.sql.Delete.by by]]`[X] `[[net.noresttherein.oldsql.sql.Delete.syntax.DeleteParam.many many]]` table `[[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhereFactory.where where]]` ...`, or
		  *   - [[net.noresttherein.oldsql.sql.Delete.apply[M* Delete(table)]][[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.apply[X:SQLForm] [X] ]]` `[[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereFactory.where where]]` ...`
		  *     and call [[net.noresttherein.oldsql.sql.Delete.batch batch]] on the created statement.
		  * @tparam S       the entity type - the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type
		  *                 of mapping `M` used for rows in `table`.
		  * @tparam M       the mapping used by `table`.
		  * @see [[net.noresttherein.oldsql.sql.Delete.many]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.apply[M* Delete(table)]]
		  * @define Args      `Seq[S]`
		  * @define Arg       `S`
		  * @define Del       `DeleteManyWhereAll[S, S, M]`
		  * @define DelLink   [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhereAll DeleteManyWhereAll]]`[S, S, M]`
		  * @define delResult deleting all rows in the table for which `condition` evaluates to `true` for any
		  *                   of the elements in input, returning the number of all matched rows for every entity.
		  */
		final class DeleteMany[S, M[O] <: BaseMapping[S, O]] private[Delete] (override val table :RelVar[M])
			extends DeleteDML[Seq[S], M, Seq[Int]] with RepeatedDML[S, Int]
			   with WhereClauseFactory[S, M, DeleteManyWhereAll[S, M]]
		{
			private[this] val one = new DeleteOne[S, M](table)
			override def dml :Delete[S, M, Int] = one

			protected override def whereDomain :From[M] WithParam S = one.`->whereDomain`

			protected override def where(condition :SingleBoolean[WithParam[From[M], S]]) :DeleteManyWhereAll[S, M] =
				new DeleteManyWhereAll[S, M](one.`->where`(condition))
//
//			/** Creates a ''delete'' DML batch deleting rows matching given entities `S`, using the provided
//			  * condition for its ''where'' clause in place of the default one of this instance.
//			  * The argument function is a constructor of the ''where'' clause, accepting the mappings `M` of the rows
//			  * in the affected table and [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]
//			  * exposing the parameter `S`. Both mappings are implicitly convertible to SQL expressions representing
//			  * all columns of the selected components. The conversion will happen if one
//			  * of the methods of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] is called on a mapping,
//			  * with an equality test [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]] being the most common case,
//			  * followed by ordering comparisons. The parameter mapping can be similarly converted
//			  * to an expression `SQLExpression[O, GlobalScope, S]`. It is a simple variant of the overloaded method
//			  * accepting a function of [[net.noresttherein.oldsql.sql.DMLStatement.DMLScope DMLScope]]
//			  * instead, which makes for more concise expressions if the filter condition depends on a single property
//			  * (component) of the affected table and the parameter, leveraging Scala's shortened lambda syntax.
//			  * Note that complete components can be compared in this way, not only single column properties,
//			  * which will translate to pair-wise comparisons of individual columns (those without buff
//			  * [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]]) of the component.
//			  *
//			  * The mapping type `UnboundParam` does not define any components or columns as properties and is treated
//			  * as a composite type based on an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[S]` obtained from
//			  * the table's mapping. Its properties can be accessed by exposing them as components or columns
//			  * of the parameter mapping, using methods
//			  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.apply[X<:P=>T,T](pick:X)* apply]],
//			  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.opt[T:SQLForm](pick:P=>Option[T])* opt]],
//			  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.col[T:ColumnForm](pick:P=>T)* col]] and
//			  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.optcol[T:ColumnForm](pick:P=>Option[T])* optcol]],
//			  * which create a synthetic [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]]
//			  * expression for the parameter property, the usages of which will translate to additional JDBC parameters
//			  * initialized with values derived from `S`:
//			  * {{{
//			  *     Delete many Dragons where _.race === _(_.race)
//			  * }}}
//			  * @return an SQL batch parameterized with `Seq[S]` which deletes all rows in the table which match
//			  *         the given condition, parameterized with subsequent elements of the sequence.
//			  *         The batch returns the number of deleted rows for every entity in the input.
//			  */
//			def where(condition :(M[From[M] WithParam S], UnboundParam[S, RowProduct AndFrom ParamRelation[S]#Param])
//			                     => GlobalBoolean[From[M] WithParam S]) :DeleteDML[Seq[S], M, Seq[Int]] =
//				dml.where(condition).batch
//
//			def where(condition :M[From[M] WithParam S] => GlobalBoolean[From[M] WithParam S])
//					:DeleteDML[Seq[S], M, Seq[Int]] =
//				dml.where(condition).batch

			//commented out until equality of parameter component expressions is fixed.
//			/** Creates a ''delete'' DML batch deleting rows matching given entities `S`, using the provided
//			  * condition for its ''where'' clause in place of the default one of this instance.
//			  * The argument is a [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
//			  * which depends (only on) the affected table and the statement parameter. This overloaded method variant
//			  * is useful in more abstract code, where the expression is created by some other part of the application.
//			  * If the filter condition is known at the point of calling this method, its overloaded siblings accepting
//			  * constructor functions provide a more convenient way of creating the expression starting from
//			  * the expressions for the table row and statement parameter.
//			  * @return an SQL batch parameterized with `Seq[S]` which deletes all rows in the table which match
//			  *         the given condition, parameterized with subsequent elements of the sequence.
//			  *         The batch returns the number of deleted rows for every entity in the input.
//			  */
//			def where(condition :GlobalBoolean[From[M] WithParam S]) :DeleteDML[Seq[S], M, Seq[Int]] =
//				dml.where(condition).batch
		}


		/** $generalInfo
		  * This class however, like [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere DeleteWhere]]
		  * of its `dml` property, allows further expansion of the ''where'' clause into a logical disjunction.
		  * $methodsInfo
		  *
		  * @tparam Arg the type of the only parameter of the statement, in the sense that its ''where'' condition
		  *             is an expression based on [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
		  *             [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
		  * @tparam S   the entity type - the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type
		  *             of mapping `M` used for rows in `table`.
		  * @tparam M   the mapping used by `table`.
		  * @define Args         `Seq[Arg]`
		  * @define Arg          `Arg`
		  * @define Or           `DeleteManyWhere`
		  * @define OrLink       [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhere DeleteManyWhere]]
		  * @define AndLink      [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhereAll DeleteManyWhereAll]]
		  * @define Del          `DeleteManyWhere[Arg, S, M]`
		  * @define DelLink      [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhere DeleteManyWhere]]`[Arg, S, M]`
		  * @define delResult    deleting all rows in the table for which `condition` evaluates to `true` for any
		  *                      of the elements in input, returning the number of all matched rows for every argument.
		  * @define generalInfo A batch of a ''delete'' statement using an arbitrary SQL Boolean
		  *                     [[net.noresttherein.oldsql.sql.SingleBoolean expression]] as its ''where'' clause.
		  *                     Instances can be created using
		  *                     [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteMany.where where]] method
		  *                     of [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteMany DeleteMany]]
		  *                     by the following expression:
		  *                      {{{
		  *                          Delete many Dragons where (_.name === _(_.name))
		  *                      }}}
		  *                     The above expression is functionally equivalent to:
		  *                      {{{
		  *                          (Delete one Dragons where (_.name === _(_.name))).batch
		  *                      }}}
		  *                     In fact, in any `Delete one ...` expression
		  *                     [[net.noresttherein.oldsql.sql.Delete.one one]] method call can be replaced with
		  *                     [[net.noresttherein.oldsql.sql.Delete.many many]], and the result of the latter
		  *                     will create the same [[net.noresttherein.oldsql.sql.Incantation Incantation]]
		  *                     as a [[net.noresttherein.oldsql.sql.Delete.batch batch]] of the former.
		  * @define examplesInfo The following expressions are fully equivalent:
		  * {{{
		  * Delete many Characters where (_.characterClass === _(_.characterClass)) orSelf
		  *     (_.isMagicUser && _.level >= 9 && _.magicSchool ==? "conjurer")
		  * Delete many Characters where { (t, p) =>
		  *     t.characterClass === p(_.characterClass) || t.isMagicUser && t.level >= 9 && t.magicSchool =? "conjurer"
		  * }
		  * }}}
		  */
		sealed class DeleteManyWhere[Arg, M[O] <: MappingAt[O]] private[Delete]
		                            (override val dml :DeleteWhere[Arg, M])
			extends DeleteDML[Seq[Arg], M, Seq[Int]] with RepeatedDML[Arg, Int]
			   with WhereAnyClauseFactory[Arg, M, DeleteManyWhere[Arg, M]]
		{
			override val table :RelVar[M] = dml.table
			protected override def whereDomain :WithParam[From[M], Arg] = dml.`->whereDomain`

			protected override def or(condition :SingleBoolean[From[M] WithParam Arg]) :DeleteManyWhere[Arg, M] =
				new DeleteManyWhere(dml.or(condition))
		}


		/** $generalInfo
		  * This class however, like [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere DeleteWhere]]
		  * of its `dml` property, allows further expansion of the ''where'' clause into a logical disjunction.
		  * $methodsInfo
		  *
		  * @tparam Arg the type of the only parameter of the statement, in the sense that its ''where'' condition
		  *             is an expression based on [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
		  *             [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
		  * @tparam S   the entity type - the [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type
		  *             of mapping `M` used for rows in `table`.
		  * @tparam M   the mapping used by `table`.
		  * @define And          `DeleteManyWhereAll`
		  * @define Del          `DeleteManyWhereAll[Arg, S, M]`
		  * @define DelLink      [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteManyWhere DeleteManyWhereAll]]`[Arg, S, M]`
		  * @define examplesInfo The following expressions are fully equivalent:
		  * {{{
		  *     Delete many Characters where (_.race === _(_.race)) andSelf
		  *         (_.isMagicUser && _.level >= 9 && _.magicSchool ==? "conjurer")
		  *     Delete many Characters where { (t, p) =>
		  *         t.race === p(_.race) && t.isMagicUser && t.level >= 9 && t.magicSchool =? "conjurer"
		  *     }
		  * }}}
		  */
		final class DeleteManyWhereAll[Arg, M[O] <: MappingAt[O]] private[Delete]
		                              (override val dml :DeleteWhereAll[Arg, M])
			extends DeleteManyWhere[Arg, M](dml)
			   with WhereAllClauseFactory[Arg, M, DeleteManyWhere[Arg, M], DeleteManyWhereAll[Arg, M]]
		{
			protected override def and(condition :SingleBoolean[From[M] WithParam Arg]) :DeleteManyWhereAll[Arg, M] =
				new DeleteManyWhereAll[Arg, M](dml.and(condition))
		}






		/** Interface with methods for creating parameterless [[net.noresttherein.oldsql.sql.Delete Delete]]`[(), M, Int]`
		  * statements using a provided SQL Boolean [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] as their
		  * ''where'' clause. The condition can be passed either directly as a value, or be a result
		  * of a constructor function accepting the table mapping `M[From[M]]`. These are passed
		  * to overloaded `where` methods, accepting functions of arity varying from 1 to 10,
		  * with identical copies of the table mapping `M[From[M]]` of `this.table` assigned to all arguments,
		  * and returning a [[net.noresttherein.oldsql.sql.SingleBoolean GlobalBoolean]]`[From[M]]`. The mapping given
		  * as the arguments, as well as any of its components, are implicitly convertible
		  * to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] of the appropriate value type and thus
		  * can be used directly in larger SQL expressions forming the whole condition. Repeated declarations
		  * of the same parameter allows to use placeholder `_` expression for the mapping of the whole row
		  * as many times as needed, without resorting to the full lambda expression syntax with explicit declaration
		  * of parameters. The `_` placeholder here always refers to the table mapping, as created statements
		  * are all parameterless:
		  * {{{
		  *     Delete from where (_.fireResistance > _.coldResistance || _.magicResistance >= 25.?)
		  * }}}
		  * The return type of all `where` methods is generic `Delete[(), M, Int]`, which does not
		  * support following the expression with [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll.and and]]
		  * or [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere.or or]] methods; this is unnecessary
		  * here, as the overloaded `where` allows to the reference the table the required number of times
		  * by placeholder `_` parameters, rather than just once as in parameterized statements, where
		  * `where`, `and` and `or` accept a function of two arguments: the table mapping and the parameter mapping,
		  * which requires named lambda parameters (and thus the longer syntax) to reference any of them
		  * more than once - hence the availability of `and`/`or` methods.
		  * @see [[net.noresttherein.oldsql.sql.Delete.from]] - `Delete from table` creates
		  *      a [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteAll DeleteAll]] statement extending this trait.
		  */
		trait GroundDeleteWhereFactory[S, M[O] <: BaseMapping[S, O]]
			extends GroundWhereClauseFactory[M, Delete[Unit, M, Int]]
		{
			@inline protected final override def domain :From[M] = From(table)
		}
///*		type GroundDeleteWhereFactory[S, M[O] <: BaseMapping[S, O]] = GroundWhereClauseFactory[M, Delete[(), M, Int]]
//		sealed trait GroundDeleteWhereFactory[S, M[O] <: BaseMapping[S, O]] {
//			protected val table :RelVar[M]
//			protected def domain :From[M] = From(table)
//
//			//consider: this could take any valid (parameterized) FromClause in theory with a help of a type class
//			/** Creates a parameterless ''delete'' statement working on this table
//			  * and using `condition` as its ''where'' clause.
//			  * This is the basic variant of the overloaded method `where`; other methods allow defining the condition
//			  * as a function of one or more references to the mapping `M` instance of the table.
//			  */
//			def where(condition :GlobalBoolean[From[M]]) :Delete[(), M, Int] =
//				new GroundDeleteWhere[S, M](table, condition.anchor(domain))
//
//			//From[M] makes including of subselects more difficult
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as an argument the mapping `M` of rows
//			  * from the affected table; any of its components and subcomponents, exposed as its properties
//			  * (from columns to the whole row represented by `M` itself), are implicitly convertible
//			  * to an SQL expression with the value type equal
//			  * to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type of the converted component.
//			  * This allows its direct use in comparisons defined as methods
//			  * of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], such as
//			  *
//			  * `component `[[net.noresttherein.oldsql.sql.SQLExpression.=== ===]]` param.?`,
//			  *
//			  * `component `[[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]]` param`,
//			  *
//			  * `component `[[net.noresttherein.oldsql.sql.SQLExpression.< <]]` param`,
//			  *
//			  * and arithmetic expressions for distinct types:
//			  *
//			  * `column `[[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL.+]]` 42`.
//			  *
//			  * Any such composite expressions can be used as parts of the returned condition.
//			  * As the created expression doesn't declare [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters,
//			  * values of all expressions other than those referring to the affected table must be known - either
//			  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] or
//			  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters. The latter can be created
//			  * using extension method [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL.? ?]]
//			  * available after importing [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL]]
//			  * (it is also present in packages [[net.noresttherein.oldsql.sql.lowercase sql.lowercase]]
//			  * and [[net.noresttherein.oldsql.sql.uppercase]]). Alternatively, any argument of type `V`
//			  * given to a comparison operator method ending with `?` is automatically promoted to a `BoundParam[V]`
//			  * internally.
//			  *
//			  * In order to avoid the hassle of providing the argument function in its full literal syntax with
//			  * an explicit argument, overloaded variants of this method exist which accept the same argument mapping
//			  * two or more times; this allows to reference columns of the deleted rows multiple times in a single
//			  * expression. The following expressions are fully equivalent:
//			  * {{{
//			  *     Delete from Dragons where { dragon => dragon.strength > dragon.intelligence }
//			  *     Delete from Dragons where (_.strength > _.intelligence)
//			  * }}}
//			  * Overloaded variants exist for other argument arities, offering flexibility in how the expression
//			  * refers to the table rows.
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :M[From[M]] => GlobalBoolean[From[M]]) :Delete[(), M, Int] =
//				where(condition(table.row))
//
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as arguments two copies the mapping `M` of rows from
//			  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
//			  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
//			  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
//			  * of the converted component. This allows their use directly as subexpressions in comparison tests,
//			  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
//			  * as the argument for both of the parameters; this repetition does not provide any additional information
//			  * or functionality, but instead allows to refer to the table twice in the same expression using
//			  * placeholder `_`: `where (_.fireResistance > _.coldResistance)`. Overloaded variants exist
//			  * for other argument arities, offering flexibility in how the expression refers to the table rows.
//			  * For additional details, see the documentation of the method variant for a single argument function:
//			  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :(M[From[M]], M[From[M]]) => GlobalBoolean[From[M]]) :Delete[(), M, Int] =
//				where(condition(table.row, table.row))
//
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as arguments three copies the mapping `M` of rows from
//			  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
//			  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
//			  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
//			  * of the converted component. This allows their use directly as subexpressions in comparison tests,
//			  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
//			  * as the argument for all of the parameters; this repetition does not provide any additional information
//			  * or functionality, but instead allows to refer to the table three times in the same expression
//			  * using placeholder `_`:
//			  * {{{
//			  *     where (_.isMagicUser && _.level >= 9 && _.magicSchool ==? "conjurer")
//			  * }}}
//			  * Overloaded variants exist for other argument arities, offering flexibility
//			  * in how the expression refers to the table rows.
//			  * For additional details, see the documentation of the method variant for a single argument function:
//			  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :(M[From[M]], M[From[M]], M[From[M]]) => GlobalBoolean[From[M]]) :Delete[(), M, Int] = {
//				val mapping = table[From[M]]
//				where(condition(mapping, mapping, mapping))
//			}
//
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as arguments four copies the mapping `M` of rows from
//			  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
//			  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
//			  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
//			  * of the converted component. This allows their use directly as subexpressions in comparison tests,
//			  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
//			  * as the argument for all of the parameters; this repetition does not provide any additional information
//			  * or functionality, but instead allows to refer to the table four times in the same expression
//			  * using placeholder `_`:
//			  * {{{
//			  *     where (_.meleeWeapon.damage + _.strengthBonus > _.rangedWeapon.damage + _.dexterityBonus)
//			  * }}}
//			  * Overloaded variants exist for other argument arities, offering flexibility
//			  * in how the expression refers to the table rows.
//			  * For additional details, see the documentation of the method variant for a single argument function:
//			  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => GlobalBoolean[From[M]])
//					:Delete[(), M, Int] =
//			{
//				val mapping = table[From[M]]
//				where(condition(mapping, mapping, mapping, mapping))
//			}
//
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as arguments five copies the mapping `M` of rows from
//			  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
//			  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
//			  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
//			  * of the converted component. This allows their use directly as subexpressions in comparison tests,
//			  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
//			  * as the argument for all of the parameters; this repetition does not provide any additional information
//			  * or functionality, but instead allows to refer to the table multiple times in the same expression
//			  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
//			  * in how the expression refers to the table rows.
//			  * For additional details, see the documentation of the method variant for a single argument function:
//			  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => GlobalBoolean[From[M]])
//					:Delete[(), M, Int] =
//			{
//				val mapping = table[From[M]]
//				where(condition(mapping, mapping, mapping, mapping, mapping))
//			}
//
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as arguments six copies the mapping `M` of rows from
//			  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
//			  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
//			  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
//			  * of the converted component. This allows their use directly as subexpressions in comparison tests,
//			  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
//			  * as the argument for all of the parameters; this repetition does not provide any additional information
//			  * or functionality, but instead allows to refer to the table multiple times in the same expression
//			  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
//			  * in how the expression refers to the table rows.
//			  * For additional details, see the documentation of the method variant for a single argument function:
//			  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]])
//			                     => GlobalBoolean[From[M]])
//					:Delete[(), M, Int] =
//			{
//				val mapping = table[From[M]]
//				where(condition(mapping, mapping, mapping, mapping, mapping, mapping))
//			}
//
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as arguments seven copies the mapping `M` of rows from
//			  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
//			  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
//			  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
//			  * of the converted component. This allows their use directly as subexpressions in comparison tests,
//			  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
//			  * as the argument for all of the parameters; this repetition does not provide any additional information
//			  * or functionality, but instead allows to refer to the table multiple times in the same expression
//			  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
//			  * in how the expression refers to the table rows.
//			  * For additional details, see the documentation of the method variant for a single argument function:
//			  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]])
//			                     => GlobalBoolean[From[M]])
//					:Delete[(), M, Int] =
//			{
//				val mapping = table[From[M]]
//				where(condition(mapping, mapping, mapping, mapping, mapping, mapping, mapping))
//			}
//
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as arguments eight copies the mapping `M` of rows from
//			  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
//			  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
//			  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
//			  * of the converted component. This allows their use directly as subexpressions in comparison tests,
//			  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
//			  * as the argument for all of the parameters; this repetition does not provide any additional information
//			  * or functionality, but instead allows to refer to the table multiple times in the same expression
//			  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
//			  * in how the expression refers to the table rows.
//			  * For additional details, see the documentation of the method variant for a single argument function:
//			  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
//			                      M[From[M]]) => GlobalBoolean[From[M]])
//					:Delete[(), M, Int] =
//			{
//				val mapping = table[From[M]]
//				where(condition(mapping, mapping, mapping, mapping, mapping, mapping, mapping, mapping))
//			}
//
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as arguments nine copies the mapping `M` of rows from
//			  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
//			  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
//			  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
//			  * of the converted component. This allows their use directly as subexpressions in comparison tests,
//			  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
//			  * as the argument for all of the parameters; this repetition does not provide any additional information
//			  * or functionality, but instead allows to refer to the table multiple times in the same expression
//			  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
//			  * in how the expression refers to the table rows.
//			  * For additional details, see the documentation of the method variant for a single argument function:
//			  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
//			                      M[From[M]], M[From[M]]) => GlobalBoolean[From[M]])
//					:Delete[(), M, Int] =
//			{
//				val mapping = table[From[M]]
//				where(condition(mapping, mapping, mapping, mapping, mapping, mapping, mapping, mapping, mapping))
//			}
//
//			/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
//			  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
//			  * of its ''where'' clause. The function accepts as arguments ten copies the mapping `M` of rows from
//			  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
//			  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
//			  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
//			  * of the converted component. This allows their use directly as subexpressions in comparison tests,
//			  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
//			  * as the argument for all of the parameters; this repetition does not provide any additional information
//			  * or functionality, but instead allows to refer to the table multiple times in the same expression
//			  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
//			  * in how the expression refers to the table rows.
//			  * For additional details, see the documentation of the method variant for a single argument function:
//			  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
//			  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
//			  *         evaluates to `true` (take note of ternary logic and `null` values).
//			  */
//			def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
//			                      M[From[M]], M[From[M]], M[From[M]]) => GlobalBoolean[From[M]])
//					:Delete[(), M, Int] =
//			{
//				val mapping = table[From[M]]
//				where(condition(mapping, mapping, mapping, mapping, mapping, mapping, mapping, mapping, mapping, mapping))
//			}
//		}


		/** A parameterless ''delete'' statement without a ''where'' clause, deleting all rows from a table.
		  * As a `GroundDelete` subclass, it is completely equivalent
		  * to a [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete GroundDelete]] statement
		  * with [[net.noresttherein.oldsql.sql.SQLBoolean.True True]]
		  * as its [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete.condition condition]].
		  * It provides however methods for narrowing the set of deleted rows by specifying
		  * an SQL Boolean [[net.noresttherein.oldsql.sql.SingleBoolean expression]] for the ''where'' clause.
		  *
		  * This has a form of overloaded `where` methods, accepting functions of arity varying from 1 to 10
		  * with identical copies of the table mapping `M[From[M]]` of `this.table` assigned to all arguments,
		  * and returning a [[net.noresttherein.oldsql.sql.SingleBoolean GlobalBoolean]]`[From[M]]`. The mappings
		  * given as the arguments, as well as any of its components, are implicitly convertible
		  * to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] of the appropriate value type and thus
		  * can be used directly in larger SQL expressions forming the whole condition. Repeated declarations
		  * of the same parameter allows to use placeholder `_` expression for the mapping of the whole row
		  * as many times as needed, without resorting to the full lambda expression syntax with explicit declaration
		  * of parameters. Note that all created statements are parameterless and thus `_` placeholder always refers
		  * to the table mapping `M`.
		  *
		  * An instance of this class can be created by expressions
		  * {{{
		  *     Delete from table
		  *     Delete all table
		  * }}}
		  * The latter returns it as a generic `Delete[(), M, Int]`, hiding the factory methods.
		  */
		final class DeleteAll[S, M[O] <: BaseMapping[S, O]] private[Delete] (override val table :RelVar[M])
			extends GroundDelete[M] with GroundDML.Impl[Int] with GroundDeleteWhereFactory[S, M]
		{
			override def where(condition :SingleBoolean[From[M]]) :Delete[Unit, M, Int] =
				new GroundDeleteWhere[S, M](table, condition)

			/** Equals [[net.noresttherein.oldsql.sql.SQLBoolean.True True]]. **/
			override val condition :SingleBoolean[From[M]] = True

			protected override def initToString :String = "Delete " + table
		}


		/** Standard implementation of arbitrary ''delete'' statements which have the values
		  * of any [[net.noresttherein.oldsql.sql.ast.BoundParam parameters]] embedded, not needing
		  * any external data to create and execute a [[java.sql.PreparedStatement PreparedStatement]]
		  * for the DML they represent. The [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[(), Int]`
		  * created by this statement will return the number of deleted rows.
		  *
		  * Instances of this class can be created by expression
		  * {{{
		  *     Delete from table where (...)
		  * }}}
		  */
		private final class GroundDeleteWhere[S, M[O] <: BaseMapping[S, O]] private[Delete]
		                    (override val table :RelVar[M], override val condition :SingleBoolean[From[M]])
			extends GroundDelete[M] with GroundDML.Impl[Int]
		{
			protected override def domain :From[M] = From(table)
		}




		/** A factory of SQL statements deleting all rows matching any of the entities in a predefined collection.
		  * Its [[net.noresttherein.oldsql.sql.Delete.syntax.GroundMultiDeleteFactory.from from]] method accepts
		  * a table with rows mapping to entity type `S` of the elements of the input collection, and creates
		  * a statement which deletes them all in one go. For every entity in input, the statement's ''where'' clause
		  * contains a Boolean [[net.noresttherein.oldsql.sql.ColumnSQL expression]] comparing the columns of the table
		  * listed in [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] collection
		  * of the associated mapping with their corresponding values, as extracted by the mapping; all repetitions
		  * of these matching conditions are joined together in a logical disjunction (`or`).
		  *
		  * An instance of this class is created with overloaded `apply` methods of the enclosing
		  * [[net.noresttherein.oldsql.sql.Delete$ Delete]] object:
		  * {{{
		  *     Delete(firkraag) from Dragons
		  *     Delete(firkraag, saladrex) from Dragons
		  *     Delete(dragons) from Dragons
		  * }}}
		  * The list of compared columns can be modified
		  * using `table`'s [[net.noresttherein.oldsql.schema.Relation.apply(components* apply]] method:
		  * {{{
		  *     Delete(Dragon("Saladrex", "Dragon", 25)) from Dragons(_.level.-, _.race.-)
		  * }}}
		  * The ''where'' clause of the SQL of the above expression will consist of a single condition `name = ?`
		  * (assuming column 'name' is on the `filteredByDefault` list).
		  *
		  * Note that, unless the input collection is small and of a magnitude occurring repeatedly,
		  * deleting multiple entities will most likely be more efficient as a batch
		  * of a [[net.noresttherein.oldsql.sql.Delete Delete]]`[Dragon, Dragons, Int]` statement:
		  * {{{
		  *     val batch = Delete(Dragons)(entities).chant
		  *     batch()
		  *     val delete = Delete(Dragons).chant
		  *     delete(entities)
		  * }}}
		  * @see [[net.noresttherein.oldsql.sql.Delete.many]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteMany]]
		  */
		class GroundMultiDeleteFactory[S] private[Delete](private val entities :Seq[S]) extends AnyVal {
			/** Accepts a database table and creates a single [[net.noresttherein.oldsql.sql.Delete Delete]]`[(), T, Int]`
			  * statement deleting all the entities given as the preceding arguments as a single operation.
			  * The ''where'' clause of the created statement will be a logical alternative between matching conditions
			  * for every deleted value, identical except for the values of used JDBC statement parameters, extracted
			  * by the table's mapping from each individual entity. They test for equality between
			  * all filterable columns of `table` (members of
			  * `table.row.`[[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]])
			  * and their corresponding properties in the mapped subject type `S`. This list of columns can be modified
			  * using `table`'s [[net.noresttherein.oldsql.schema.Relation.apply(components* apply]] method:
			  * {{{
			  *     Delete(saladrex, firkraag, abazigal) from Dragons(_.level.-, _.race.-)
			  * }}}
			  * The ''where'' clause of the SQL of the above expression will exclude columns 'level' and 'race'
			  * from the filter condition.
			  *
			  * Because the SQL text for every such statement depends on the size of the input collection,
			  * it might result in problematically large statements as well as flooding the statement cache
			  * of the JDBC driver and the database with numerous variants of this statement for different arities.
			  * The general use case of deleting any number of entities is better saved by a batch of a statement
			  * deleting a single entity:
			  * {{{
			  *     Delete(Dragons)(entities)
			  * }}}
			  * @return a single DML statement returning the number of rows which matched any of the input entities
			  *         and were deleted as the result.
			  * @see [[net.noresttherein.oldsql.sql.Delete.many]]
			  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteMany]]
			  */
			def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O]]
			        (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :Delete[Unit, T, Int] =
				if (entities.isEmpty)
					throw new IllegalArgumentException(
						"An empty collection of entities given to Delete(values) from " + table +"."
					)
				else if (entities.sizeIs == 1)
					new GroundDeleteOne[S, T](reveal(table), entities.head)
				else
					new GroundMultiDelete[S, T](reveal(table), entities)
		}


		/** A ''delete'' statement which contains matching conditions for every entity in the input collection,
		  * joined in a logical disjunction. The conditions are identical conjunctions of tests for individual columns
		  * listed by [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] property
		  * of mapping `M`, comparing them with statement parameters set to values extracted by the mapping
		  * from subsequent elements of the input collection. The statement returns the total number of deleted rows,
		  * without specifying which of the input entities they matched.
		  *
		  * Instances can be created by expressions:
		  * {{{
		  *     Delete(firkraag, saladrex) from Dragons
		  *     Delete(Seq(firkraag, saladrex)) from Dragons
		  * }}}
		  * @tparam S the entity type mapping to the rows of `table`.
		  * @tparam M the mapping of the rows of `table`.
		  * @see [[net.noresttherein.oldsql.sql.Delete.apply[S](entities:Seq[S])* Delete(values)]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteFacade.apply(entities)*]]
		  */
		private final class GroundMultiDelete[S, M[O] <: BaseMapping[S, O]] private[Delete]
		                                     (override val table :RelVar[M], val values :Seq[S])
			extends GroundDelete[M] with GroundDML.Impl[Int]
		{
			protected override val domain = From(table)
			override val condition :SingleBoolean[From[M]] =
				OrSQL(values.map(TableStatement.whereEntity(domain, _)) :_*)

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GroundMultiDelete[_, _] =>
					hashCode == other.hashCode && table == table && values == other.values
				case other :GroundDelete[_] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && condition == other.condition
				case _ => false
			}
			protected override def initToString :String = values.mkString("Delete(", ", ", ") from " + table)
		}



		/** A factory of [[net.noresttherein.oldsql.sql.Delete Delete]]`[(), T, Int]` statements deleting
		  * the specified entity from the table given as the argument to its
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteOneFactory.from from]] method.
		  */
		class GroundDeleteOneFactory[S] private[Delete] (private val entity :S) extends AnyVal {
			/** A parameterless DML statement deleting all rows matching the given entity. The ''where'' clause
			  * of the created statement is a logical conjunction comparing the columns in
			  * `table.`[[net.noresttherein.oldsql.schema.Relation.export export]]`.`[[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]]
			  * with their corresponding values in an argument entity `S`. This list of columns can be modified
			  * using `table`'s [[net.noresttherein.oldsql.schema.Relation.apply(components* apply]] method:
			  * {{{
			  *     Delete(Dragon("Saladrex", "Dragon", 25)) from Dragons(_.level.-, _.race.-)
			  * }}}
			  * The ''where'' clause of the SQL of the above expression will consist of a single condition `name = ?`
			  * (assuming column 'name' is on the `filteredByDefault` list). The statement will return the number
			  * of matched - and deleted - rows.
			  * @param table the table with deleted rows.
			  */
			def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O]]
			        (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :Delete[Unit, T, Int] =
				new GroundDeleteOne[S, T](reveal(table), entity)
		}


		/** A ''delete'' statement deleting all rows in `table` which match the given entity `value` of the
		  * [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type of the table's mapping `M`.
		  * The ''where'' clause of the statement contains equality tests for all columns of mapping `M`
		  * listed by its [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] property,
		  * joined in a logical conjunction. The result is the number of deleted rows, which can be any number
		  * from 0 to more than one, if the projection of the table to the filterable columns is not unique.
		  * An instance of this class can be created by expression
		  * {{{
		  *     Delete(value) from table
		  * }}}
		  * This list of columns can be modified
		  * using `table`'s [[net.noresttherein.oldsql.schema.Relation.apply(components* apply]] method:
		  * {{{
		  *     Delete(Dragon("Saladrex", "Dragon", 25)) from Dragons(_.level.-, _.race.-)
		  * }}}
		  * The ''where'' clause of the SQL of the above expression will consist of a single condition `name = ?`
		  * (assuming column 'name' is on the `filteredByDefault` list).
		  */ //fixme: null columns should be excluded from matching.
		private final class GroundDeleteOne[S, M[O] <: BaseMapping[S, O]] private[Delete]
		                                   (override val table :RelVar[M], val value :S)
			extends GroundDelete[M] with GroundDML.Impl[Int]
		{
			override val domain :From[M] = From(table)
			override val condition :SingleBoolean[From[M]] = TableStatement.whereEntity(table, value)

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GroundDeleteOne[_, _] => table == table && value == other.value
				case other :GroundDelete[_] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && condition == other.condition
				case _ => false
			}
			protected override def initToString :String = "Delete(" + value + ") from " + table
		}
	}






	/** The implementation interfaces serving as bases for virtually all [[net.noresttherein.oldsql.sql.Delete Delete]]
	  * statement implementations. They are public for two reasons: to facilitate the creation of custom implementations
	  * consistent with the standard ones (and thus seamlessly interoperable with them) and as a means for inspecting
	  * the statement structure in code responsible for added functionality or special handling, in particular
	  * custom [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] implementations.
	  * They are not intended for general use, and client code whose purpose is only to execute them should
	  * use the super type `Delete` instead of implementation-specific classes.
	  */
	object implementation {
		/** Base trait of ''delete'' statement implementations accepting argument `Arg` used
		  * as an [[net.noresttherein.oldsql.sql.WithParam WithParam]] unbound parameter in its `condition` property
		  * used for the ''where'' clause of this statement. The result is always
		  * the [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UpdateCount number of deleted rows]].
		  * It is an implementation-oriented interface and should be only used either as a framework for new `Delete`
		  * implementations, or when in lower level inspecting code, such as a custom
		  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]].
		  *
		  * The implementation is centered around
		  * [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete.condition condition]] property,
		  * which is an SQL Boolean [[net.noresttherein.oldsql.sql.ColumnSQL expression]] used as the ''where'' clause
		  * of this statement. The expression is parameterized with
		  * [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`,
		  * which exposes both the [[net.noresttherein.oldsql.sql.TableStatement.table table]] from which the rows
		  * are deleted and the statement parameter `Arg` as members of a virtual
		  * [[net.noresttherein.oldsql.sql.FromClause from clause]] - composite expressions which can be used to access
		  * their components (such as the columns of the table or a property of an `Arg` instance), and be used
		  * in larger expressions comprising the fragments of the ''where'' clause.
		  *
		  * It defines equality in terms of the `condition` property only - essentially as the equality of both
		  * the generated SQL and the parameter types (due to [[net.noresttherein.oldsql.schema.SQLForm forms]] included
		  * in the expressions) - and subclasses should also equal instances of any other subclass with the same ''where''
		  * clause if possible.
		  *
		  * The statement fixes the [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete.result result]]
		  * of execution to [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UpdateCount UpdateCount]] -
		  * returning the number of deleted rows. If the number might be too large for the `Int` type, it can be changed
		  * by [[net.noresttherein.oldsql.sql.DMLStatement.largeUpdateCount largeUpdateCount]] method
		  * returning an adapter [[net.noresttherein.oldsql.sql.Delete Delete]]`[(), M, Long]` statement.
		  * @see [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete]]
		  */
		trait ParamDelete[Args, M[O] <: MappingAt[O]] extends Delete[Args, M, Int] { outer =>
			/** The [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] used as the type parameter
			  * by the [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete.condition condition]] expression
			  * of the ''where'' clause of this statement.
			  */
			type Domain = From[M] WithParam Args

			/** An instance of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] that this instance's
			  * [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete.condition condition]] expression
			  * is parameterized with, used to create said `condition`.
			  */
			protected val domain :From[M] WithParam Args

			/** The ''where'' clause of this statement. **/
			val condition :SingleBoolean[From[M] WithParam Args]

			/** Returns [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UpdateCount UpdateCount]]. **/
			override def result :StatementResult[Nothing, Int] = UpdateCount

	//		override def bind(args :Arg) :Delete[(), M, Int] =
	//			new BoundStatement[Arg, Int](this, args) with Delete[Any, M, Int] with GroundDelete[M] {
	//				override val table = outer.table
	//				override val condition = outer.condition.bind(outer.domain, @~ ~ args)
	//			}
			@inline final protected[Delete]
			def eval(condition :(M[From[M] WithParam Args], UnboundParam[Args, WithParam.Last[Args]])
			                    => SingleBoolean[From[M] WithParam Args]) :SingleBoolean[From[M] WithParam Args] =
				condition(table.row[From[M] WithParam Args], domain.last.mapping).anchor(domain)

			@inline final protected[Delete]
			def eval(condition :M[From[M] WithParam Args] => SingleBoolean[From[M] WithParam Args])
					:SingleBoolean[From[M] WithParam Args] =
				condition(table.row).anchor(domain)

			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Int] =
				visitor.paramDelete(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args] =
				Delete.spell(this)(condition)(domain, domain.parameterization) compose { @~ ~ _ }

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamDelete[_, MappingAt @unchecked]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :ParamDelete[_, _] if canEqual(other) && other.canEqual(this) =>
					hashCode == other.hashCode && table == other.table && other.condition == condition
				case _ => false
			}
			protected override def initHashCode :Int = table.hashCode * 31 + condition.hashCode
			protected override def initToString :String = "Delete " + table + " where " + condition
		}


		/** Base trait of parameterless ''delete'' statement implementations with an SQL Boolean
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] used as its ''where'' clause.
		  * It is an implementation-oriented interface and should be only used either as a framework for new `Delete`
		  * implementations, or when in lower level inspecting code, such as a custom
		  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]].
		  *
		  * The implementation is centered around [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete.condition condition]]
		  * property, which is an SQL Boolean [[net.noresttherein.oldsql.sql.ColumnSQL expression]] dependent
		  * only on the table with deleted rows (parameterized with [[net.noresttherein.oldsql.sql.From From]]`[M]`) -
		  * all terms must either be [[net.noresttherein.oldsql.sql.ast.ComponentSQL components]]
		  * of its mapping `M` (in particular table columns), [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]],
		  * or [[net.noresttherein.oldsql.sql.ast.BoundParam parameters]] with already provided values.
		  *
		  * Aside from a different [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete.Domain Domain]] type
		  * of the condition, the implementation is otherwise very similar
		  * to [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete ParamDelete]].
		  *
		  * It defines equality in terms of the `table` and `condition` properties only - essentially as the equality
		  * of both the generated SQL and the parameter types (due to [[net.noresttherein.oldsql.schema.SQLForm forms]]
		  * included in the expressions) - and subclasses should also equal instances of any other subclass with the same
		  * ''where'' clause if possible.
		  *
		  * The statement sets the [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete.result result]]
		  * of execution to [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UpdateCount UpdateCount]] -
		  * returning the number of deleted rows. If the number might be too large for the `Int` type, it can be changed
		  * by [[net.noresttherein.oldsql.sql.DMLStatement.largeUpdateCount largeUpdateCount]] method
		  * returning an adapter [[net.noresttherein.oldsql.sql.Delete Delete]]`[(), M, Long]` statement.
		  */
		trait GroundDelete[M[O] <: MappingAt[O]] extends Delete[Any, M, Int] {
			/** The [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] used as the type parameter
			  * by the [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete.condition condition]] expression
			  * of the ''where'' clause of this statement.
			  */
			protected type Domain = From[M]
			protected def domain :From[M] //= From(table)

			/** The ''where'' clause of this statement. **/
			val condition :SingleBoolean[From[M]]

			/** Returns [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UpdateCount UpdateCount]]. **/
			override def result :StatementResult[Nothing, Int] = UpdateCount

			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Any, Int] =
				visitor.groundDelete(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Any] =
				Delete.spell(this)(condition)(domain, Parameterization.paramless) compose { _ :Any => @~ }

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroundDelete[MappingAt @unchecked]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GroundDelete[_] if canEqual(other) && (other canEqual this) =>
					hashCode == other.hashCode && table == other.table && condition == other.condition
				case _ => false
			}
			protected override def initHashCode :Int = table.hashCode * 31 + condition.hashCode
			protected override def initToString :String = "Delete " + table + " where " + condition
		}




		/** Fragment of the [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] visitor
		  * covering the cases of all ''delete'' statements. This includes empty methods for every existing type
		  * in the hierarchy, abstract or concrete.
		  * Note that instances of standard adapter classes,
		  * such as [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement BoundStatement]], which extend also
		  * [[net.noresttherein.oldsql.sql.Delete Delete]], will not be handled by any of these methods, but rather
		  * into the more generic ones declared by `StatementVisitor` itself to better reflect their nature -
		  * in their case, conforming to `Delete` serves only a declarative purpose.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Arg` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  * @see [[net.noresttherein.oldsql.sql.Delete.implementation.MatchDelete]]
		  * @see [[net.noresttherein.oldsql.sql.Delete.implementation.CaseDelete]]
		  */
		trait DeleteVisitor[R[-X, +Y]] {
			def paramDelete[X, M[O] <: MappingAt[O]](stmt :ParamDelete[X, M])               :R[X, Int]
			def groundDelete[M[O] <: MappingAt[O]](stmt :GroundDelete[M])                   :R[Any, Int]
			def delete[X, M[O] <: MappingAt[O], Y](stmt :Delete[X, M, Y])                   :R[X, Y]
		}

		type MatchDelete[R[-X, +Y]] = DeleteVisitor[R]
//		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]]
//		  * ''visitors'' of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It implements
//		  * all methods of [[net.noresttherein.oldsql.sql.Delete.implementation.DeleteVisitor DeleteVisitor]]
//		  * for concrete [[net.noresttherein.oldsql.sql.Delete Delete]] implementations by delegating them
//		  * to the methods for their base traits, leaving unimplemented only the cases for
//		  * [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete GroundDelete]],
//		  * [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete ParamDelete]] and `Delete` itself
//		  * (for custom extensions not derived from any of the existing implementations).
//		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
//		  *           (the `Arg` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
//		  *           and its return type (the `Res` argument of the visited statement).
//		  * @see [[net.noresttherein.oldsql.sql.Delete.implementation.CaseDelete]]
//		  */
//		trait MatchDelete[R[-X, +Y]] extends DeleteVisitor[R] {
//			override def deleteWhere[X, S, M[O] <: BaseMapping[S, O]](stmt :DeleteWhere[X, S, M]) = paramDelete(stmt)
//			override def deleteOne[S, M[O] <: BaseMapping[S, O]](stmt :DeleteOne[S, M])           = paramDelete(stmt)
//			override def deleteAny[S, M[O] <: BaseMapping[S, O]](stmt :MultiDeleteEntity[S, M])           = paramDelete(stmt)
//
//			override def deleteWhere[S, M[O] <: BaseMapping[S, O]](stmt :GroundDeleteWhere[S, M]) = groundDelete(stmt)
//			override def deleteAll[S, M[O] <: BaseMapping[S, O]](stmt :DeleteAll[S, M])           = groundDelete(stmt)
//			override def deleteOne[S, M[O] <: BaseMapping[S, O]](stmt :GroundDeleteOne[S, M])     = groundDelete(stmt)
//			override def deleteAny[S, M[O] <: BaseMapping[S, O]](stmt :GroundCombinedDelete[S, M])     = groundDelete(stmt)
//		}

		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]]
		  * ''visitors'' of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It expands on
		  * [[net.noresttherein.oldsql.sql.Delete.implementation.MatchDelete MatchDelete]] by further delegating
		  * the remaining open cases to the method for [[net.noresttherein.oldsql.sql.Delete Delete]] trait itself.
		  * CaseAnys for concrete subclasses dispatch still to their immediate base type, making the delegation
		  * a multi-step affair and allowing to override on the chosen level.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Arg` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  */
		trait CaseDelete[R[-X, +Y]] extends MatchDelete[R] {
			override def paramDelete[X, M[O] <: MappingAt[O]](stmt :ParamDelete[X, M]) :R[X, Int]  = delete(stmt)
			override def groundDelete[M[O] <: MappingAt[O]](stmt :GroundDelete[M])     :R[Any, Int] = delete(stmt)
		}

	}




	private[sql] def spell[X, M[O] <: MappingAt[O], F <: RowProduct]
	                      (self :Delete[Nothing, M, Any])(condition :SingleBoolean[F])
	                      (domain :F, params :Parameterization[X, F])(implicit spelling :SQLSpelling) :SpelledSQL[X] =
	{
		val table = self.table
		val context = spelling.newContext
		val delete = (spelling.DELETE + " ") +: (spelling.table(table, "")(domain, context, params))
		if (condition == True) delete
		else delete + (" " + spelling.WHERE + " ") + (spelling(condition)(domain, _, params))
	}

}



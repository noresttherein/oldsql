package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.OperationView.UpdateView
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.{Chain, PassedArray, ReversedList}
import net.noresttherein.oldsql.exceptions.{MismatchedExpressionsException, MisspelledSQLException}
import net.noresttherein.oldsql.schema.{RelVar, SQLForm, Table}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.SingleSQL
import net.noresttherein.oldsql.sql.DML.{BoundDML, ComposedDML, DMLAPI, GroundDML, RepeatedDML}
import net.noresttherein.oldsql.sql.DMLStatement.{AlteredResultStatement, BoundStatement, ComposedStatement, DMLStatementAPI, StatementResult, StatementVisitor}
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult.{UpdateCount, UpdatedEntities}
import net.noresttherein.oldsql.sql.ParamClause.UnboundParam
import net.noresttherein.oldsql.sql.Returning.implementation.{AbstractReturningEntities, GenericBatchReturningEntities, ReturningProperTuple, ReturningTupleSeqTemplate, ReturningTupleSingleton, ReturningTuplesTemplate}
import net.noresttherein.oldsql.sql.Returning.syntax.{BatchReturningEntitiesClause, BatchReturningTuplesClause, EntitiesBatch, EntitiesStatement, EntityStatement, EntityStatementsTemplate, GenericReturningEntitiesClause, GroundBatchReturningEntitiesClause, GroundBatchReturningTuplesClause, GroundEntitiesBatch, GroundRowsBatch, ReturningEntitiesClause, ReturningEntitiesClauses, ReturningEntityClause, ReturningTupleClause, ReturningTuplesClause, ReturningTuplesClauses, ReturningTuplesClausesTemplate, RowsBatch, RowsStatement, RowStatement}
import net.noresttherein.oldsql.sql.RowProduct.ParameterizedRow
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.TableStatement.{seqAt, GroundSetClauseFactory, GroundSupplantClauseFactory, GroundWhereClauseFactory, ReturningClauseFactory, SetClauseFactory, SetParamScope, SupplantClauseFactory, WhereAllClauseFactory, WhereAnyClauseFactory, WhereClauseFactory}
import net.noresttherein.oldsql.sql.Update.implementation.{updateDomain, DefaultEntityUpdate, DefaultGroundEntitiesUpdate, DefaultGroundEntityUpdate, DefaultGroundUpdate, DefaultUpdate, DefaultUpdateReturningEntityWhereSeed, DefaultUpdateUpdatingEntity, GroundUpdate, ParamUpdate, UpdateReturning, UpdatesReturning}
import net.noresttherein.oldsql.sql.Update.syntax.{DefaultUpdateReturningEntity, DefaultUpdateUpdatingOne, EntitiesUpdate, EntityUpdate, EntityUpdateWhere, EntityUpdateWhereAll, EntityUpdateWhereSeed, GenericUpdateFactory, GroundEntitiesUpdate, GroundEntitiesUpdateWhereSeed, GroundEntityMultiUpdateFactory, GroundEntityUpdate, GroundEntityUpdateWhereSeed, GroundRowUpdateWhereSeed, GroundUpdateAllFactory, GroundUpdateFactory, GroundUpdateOneFactory, RowUpdate, RowUpdateWhereAll, RowUpdateWhereSeed, UpdateFacade, UpdateMany, UpdateOne, UpdateParam, UpdateUpdatingOne}
import net.noresttherein.oldsql.sql.ast.{BoundParam, OrSQL}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.MappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.SQLExpression.Single

//here be implicits
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL





//todo: rename to Updates
trait UpdateDML[-Args, M[O] <: MappingAt[O], +Res]
	extends TableDML[Args, M, Res] with DMLAPI[Args, Res, Update.table[M]#DML]
{
	override def compose[X](f :X => Args) :UpdateDML[X, M, Res] =
		new ComposedDML.Base[X, Args, Res, Update.table[M]#DML](this, f)
			with UpdateDML[X, M, Res] with DerivedDML[X, Res]
			with ComposedDML[X, Args, Res] with ComposedDML.Impl[X, Args, Res, Update.table[M]#DML]

	override def bind(args :Args) :UpdateDML[Unit, M, Res] =
		new BoundDML.Base[Args, Res, Update.table[M]#DML](this, args)
			with UpdateDML[Any, M, Res] with DerivedDML[Any, Res]
			with BoundDML[Args, Res] with BoundDML.Impl[Args, Res, Update.table[M]#DML]
}




trait Update[-Args, M[O] <: MappingAt[O], +Res]
	extends UpdateDML[Args, M, Res] with TableStatement[Args, M, Res]
		with DMLStatementAPI[Args, Res, Update.table[M]#Stmt]
{
	protected override def returns[Y](result :StatementResult[Nothing, Y]) :Update[Args, M, Y] =
		new AlteredResultStatement.Base[Args, Y, Update.table[M]#Stmt](this, result)
			with Update[Args, M, Y] with DerivedDML[Args, Y]
			with AlteredResultStatement[Args, Y] with AlteredResultStatement.Impl[Args, Y, Update.table[M]#Stmt]

	override def compose[X](f :X => Args) :Update[X, M, Res] =
		new ComposedDML.Base[X, Args, Res, Update.table[M]#Stmt](this, f)
			with Update[X, M, Res] with DerivedDML[X, Res]
			with ComposedStatement[X, Args, Res] with ComposedStatement.Impl[X, Args, Res, Update.table[M]#Stmt]

	override def bind(args :Args) :Update[Unit, M, Res] =
		new BoundDML.Base[Args, Res, Update.table[M]#Stmt](this, args)
			with Update[Any, M, Res] with DerivedDML[Any, Res]
			with BoundStatement[Args, Res] with BoundStatement.Impl[Args, Res, Update.table[M]#Stmt]

	override def batch :UpdateDML[Seq[Args], M, Seq[Res]] =
		new RepeatedDML.Base[Args, Res, Update.table[M]#Stmt](this)
			with UpdateDML[Seq[Args], M, Seq[Res]] with DerivedDML[Seq[Args], Seq[Res]] with RepeatedDML[Args, Res]

	protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] = visitor.update(this)
}




/**
  * {{{
  * Update table Dragons set (_.level += 1) where (_.race ==? "Dragon")    :Update[(), Dragons, Int]
  * Update table Dragons set (_.race := "Red Dragon".?) set
  *     (_.level := 23.?) where (_.name === "Firkraag".?)                  :Update[(), Dragons, Int]
  * Update table Dragons set
  *     (_.strength := _.intelligence) where (_.strength < _.intelligence) :Update[(), Dragons, Int]
  * Update table Dragons set
  *     Seq( _.strength += 1, _.intelligence += 1) where (_.level < 10)    :Update[(), Dragons, Int]
  * Update table Dragons(_.race.-) ... //the column/component set of the table can be altered
  *
  * Update all Dragons set (_.level := _.level + 1)                        :Update[(), Dragons, Int]
  *
  * Update(firkraag) in Dragons                                            :Update[(), Dragons, Int]
  * Update(firkraag) in Dragons(_.race.-)                                  :Update[(), Dragons, Int]
  * Update(firkraag) in Dragons set
  *     (_.level += 1) set (_.race := "Red " ++ _.race)                    :Update[(), Dragons, Int]
  * Update(firkraag) in Dragons supplant (_.level := 25)                   :Update[(), Dragons, Int]
  * Update(firkraag) in Dragons where (_.race like "%Dragon")              :Update[(), Dragons, Int]
  * Update(firkraag, saladrex) in Dragons set (_.level += 1)               :Update[(), Dragons, Int]
  *
  * Update(Dragons)                                                        :Update[Dragon, Dragons, Int]
  * Update(Dragons) set (_.race := _(_.race)) set (_.level += 1)           :Update[Dragon, Dragons, Int]
  * Update(Dragons).set (_.race := _(_.race), _.level := _(_.level)) where
  *     (_.name === _(_.name))                                             :Update[Dragon, Dragons, Int]
  * Update(Dragons).supplant (_.level :=? 20) where
  *     (_.name === _(_.name)) or (_.race === _(_.race))                   :Update[Dragon, Dragons, Int]
  *
  * Update(Dragons)[Int] set (_.strength += _) whereSelf
  *     (_.intelligence > _.strength)                                      :Update[Int, Dragons, Int]
  * Update(Dragons)[String] set (_.level += 1) update
  *     (_.race := "Red " ++ _.race) where (_.name === _)                  :Update[String, Dragons, Int]
  * Update(Dragons)[(String, String, Int)].set
  *     (_.race := _(_._2), _.level := _(_._3)) where (_.name === _(_._1)) :Update[(String, String, Int), Dragons, Int]
  *
  * Update(Dragons) * 6 set/supplant/update ... //same as Update(Dragons) ..., but where clause is repeated
  * Update(Dragons)[String] * 6 set/update ...  //same as Update(Dragons)[String] ..., but where clause is repeated
  *
  * Update(Dragons)(firkraag)                                              :Update[(), Dragons, Int]
  * Update(Dragons)(firkraag, saladrex)                                    :UpdateDML[(), Dragons, Seq[Int]]
  * Update(Dragons)(dragons)                                               :UpdateDML[(), Dragons, Seq[Int]]
  *
  * Update one Dragons                                                     :Update[Dragon, Dragons, Int]
  * Update one Dragons set (_.race := _(_.race))                           :Update[Dragon, Dragons, Int]
  * Update one Dragons where
  *     (_.race === _(_.race)) and (_.name === _(_.name))                  :Update[Dragon, Dragons, Int]
  * Update one Dragons supplant (_.level := 23)                            :Update[Dragon, Dragons, Int]
  * Update one Dragons supplant
  *     (_.level := _(_.level) + 1) where (_.name like _(_.name))          :Update[Dragon, Dragons, Int]
  * Update many Dragons                                                    :Update[Seq[Dragon], Dragons, Seq[Int]]
  * Update many Dragons ... /* same as Update one ..., but creates batches */
  *
  * Update returning Dragons ...                                           :Update[Dragon, Dragons, Dragon]
  *
  * Update.by[String] table Dragons set ... where ....                     :Update[String, Dragons, Int]
  * Update.by[String] all Dragons set ...                                  :Update[String, Dragons, Int]
  * Update.by[String] * 10 table Dragons set ...                           :Update[Seq[String], Dragons, Int]
  * }}}
  *
  * @see [[net.noresttherein.oldsql.sql.ComponentSetter]]
  * @see [[net.noresttherein.oldsql.sql.ast.LValueSQL.:=]]
  * @see [[net.noresttherein.oldsql.sql.ast.LValueSQL.:=?]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.+=]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.-=]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.*=]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL./=]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.%=]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.&&=]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.||=]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.++=]]
  * @author Marcin Mościcki
  */
object Update {
	type table[M[O] <: MappingAt[O]] = {
		type DML[-X, +Y] = UpdateDML[X, M, Y]
		type Stmt[-X, +Y] = Update[X, M, Y]
	}
	//todo: make either this, or Update(table) by ... accept non-implicit form
	def by[X] :UpdateParam[X] = new UpdateParam[X] {}

	def table[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S] //this could potentially be UpdateMany or UpdateOne
	         (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :GroundUpdateFactory[S, T] =
		new GroundUpdateFactory[S, T](From(reveal(table)))

	def all[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	       (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :GroundUpdateAllFactory[S, T] =
		new GroundUpdateAllFactory[S, T](From(reveal(table)))

	def returning[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	             (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :UpdateUpdatingOne[S, T] =
	{
		val one = this.one(table)
		val mapping = one.table.export[From[T]]
		val keys = mapping.autoUpdated.to(Seq)
		val columns = keys.view.map(_.name).to(Seq)
		new DefaultUpdateUpdatingOne[S, T](one, keys, columns)
	}

	def one[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	       (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :UpdateOne[S, T] =
		new UpdateOne[S, T](reveal(table))

	def many[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	        (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :UpdateMany[S, T] =
		new UpdateMany[S, T](reveal(table))

	def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :UpdateFacade[S, T] =
		new UpdateFacade[S, T](reveal(table))


	def apply[S](entity :S) :GroundUpdateOneFactory[S] =
		new GroundUpdateOneFactory(entity)

	def apply[S](first :S, second :S, rest :S*) :GroundEntityMultiUpdateFactory[S] =
		new GroundEntityMultiUpdateFactory(first +: second +: rest) //todo: efficient concatenation

	def apply[S](entities :Seq[S]) :GroundEntityMultiUpdateFactory[S] =
		new GroundEntityMultiUpdateFactory(entities)




	object syntax {

		sealed trait RowUpdate[-Args, M[O] <: MappingAt[O], +Res]
			extends Update[Args, M, Res] with RowStatement[Args, M, Res]
			   with ReturningClauseFactory[M, ({ type U[X] = UpdateReturningKey[Args, M, @~ ~ X, X] })#U]
		{
			override def returning[T](key :M[From[M]] => TypedMapping[T, From[M]])
					:UpdateReturningKey[Args, M, @~ ~ T, T] =
				new UpdateComponentReturning[Args, M, T](this, key(table.row))
		}

		sealed trait RowUpdates[-Args, M[O] <: MappingAt[O], +Res]
			extends UpdateDML[Args, M, Res] //with GenericRowStatements[Args, M, Res, Seq]
			   with ReturningClauseFactory[M, ({ type U[X] = UpdatesReturningKeys[Args, M, @~ ~ X, X] })#U]

		sealed trait RowsUpdate[-Args, M[O] <: MappingAt[O], +Res]
			extends Update[Args, M, Res] with RowUpdates[Args, M, Res] with RowsStatement[Args, M, Res]
			   with ReturningClauseFactory[M, ({ type U[X] = UpdateReturningKeys[Args, M, @~ ~ X, X] })#U]
		{
			override def returning[T](key :M[From[M]] => TypedMapping[T, From[M]])
					:UpdateReturningKeys[Args, M, @~ ~ T, T] =
				new DefaultUpdateReturningKeys[Args, M, @~ ~ T, T](
					new UpdateComponentReturning[Args, M, T](this, key(table.row))
				)
		}

		sealed trait BatchUpdate[-Args, M[O] <: MappingAt[O], +Res]
			extends UpdateDML[Seq[Args], M, Seq[Res]] with RowsBatch[Args, M, Res]
			   with ReturningClauseFactory[M, ({ type U[X] = UpdateBatchReturningKeys[Args, M, @~ ~ X, X] })#U]
		{
			override def returning[T](key :M[From[M]] => TypedMapping[T, From[M]])
					:UpdateBatchReturningKeys[Args, M, @~ ~ T, T] =
				new DefaultUpdateBatchReturningKeys[Args, M, @~ ~ T, T](dml returning key)

			protected def dml :RowUpdate[Args, M, Res]
		}

		sealed trait GroundBatchUpdate[Args, M[O] <: MappingAt[O], +Res]
			extends UpdateDML[Any, M, Seq[Res]] with GroundRowsBatch[Args, M, Res]
			with ReturningClauseFactory[M, ({ type U[X] = GroundUpdateBatchReturningKeys[Args, M, @~ ~ X, X] })#U]
		{
			override def returning[T](key :M[From[M]] => TypedMapping[T, From[M]])
					:GroundUpdateBatchReturningKeys[Args, M, @~ ~ T, T] =
				new DefaultGroundUpdateBatchReturningKeys[Args, M, @~ ~ T, T](dml returning key, args)

			protected def dml :BatchUpdate[Args, M, Res]
			protected def args :Seq[Args]
		}


		sealed trait EntityUpdateTemplate[-Args, S, M[O] <: BaseMapping[S, O], +Res, +U]
			extends Update[Args, M, Res] with EntityStatementsTemplate[Args, M, Res, U]
		{
			override def updatingKeys(keys :Seq[TypedMapping[_, From[M]]]) :U =
				updating(keys, Returning.implementation.columnNames(table, keys))

			protected def updating(components :Seq[TypedMapping[_, From[M]]], columnNames :Seq[String]) :U

			private[sql] def exportMapping = table.export.asInstanceOf[TypedMapping[S, From[M]]]
		}

		sealed trait EntityUpdate[-Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends RowUpdate[Args, M, Res] with EntityStatement[Args, S, M, Res]
			   with EntityUpdateTemplate[Args, S, M, Res, UpdateReturningEntity[Args, S, M]]
		{
			protected override def updating(components :Seq[TypedMapping[_, From[M]]], columnNames :Seq[String])
					:UpdateReturningEntity[Args, S, M] =
				new DefaultUpdateReturningEntity[Args, S, M](this, components, columnNames, updatedResult(columnNames))

			protected def updatedResult(columns :Seq[String]) :StatementResult[S, S]
		}

		sealed trait ParamEntityUpdate[S, M[O] <: BaseMapping[S, O], +Res] extends EntityUpdate[S, S, M, Res] {
			protected override def updatedResult(columns :Seq[String]) :StatementResult[S, S] =
				UpdatedEntities.Single(exportMapping, columns)
		}

		sealed trait GroundEntityUpdate[S, M[O] <: BaseMapping[S, O], +Res] extends EntityUpdate[Any, S, M, Res] {
			protected override def updatedResult(columns :Seq[String]) :StatementResult[S, S] =
				UpdatedEntities.Single(value, exportMapping, columns)
			protected def value :S
		}

		sealed trait EntityUpdates[-Args, S, M[O] <: BaseMapping[S, O], +Res] extends RowUpdates[Args, M, Res]
			with EntityStatementsTemplate[Args, M, Res, UpdatesReturningEntities[Args, S, M]]

		sealed trait EntitiesUpdate[-Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends RowsUpdate[Args, M, Res] with EntityUpdates[Args, S, M, Res]
			   with EntitiesStatement[Args, S, M, Res]
			   with EntityUpdateTemplate[Args, S, M, Res, UpdateReturningEntities[Args, S, M]]
		{
			protected override def updating(components :Seq[TypedMapping[_, From[M]]], columnNames :Seq[String])
					:UpdateReturningEntities[Args, S, M] =
				new DefaultUpdateReturningEntities[Args, S, M](
					this, components, columnNames, updatedResult(columnNames)
				)
			protected def updatedResult(columns :Seq[String]) :StatementResult[S, Seq[S]]
		}

		sealed trait ParamEntitiesUpdate[S, M[O] <: BaseMapping[S, O], +Res] extends EntitiesUpdate[Seq[S], S, M, Res] {
			protected override def updatedResult(columns :Seq[String]) :StatementResult[S, Seq[S]] =
				UpdatedEntities(exportMapping, columns, Seq)
		}

		sealed trait GroundEntitiesUpdate[S, M[O] <: BaseMapping[S, O], +Res] extends EntitiesUpdate[Any, S, M, Res] {
			protected override def updatedResult(columns :Seq[String]) :StatementResult[S, Seq[S]] =
				UpdatedEntities(values, exportMapping, columns, Seq)
			protected def values :Seq[S]
		}

		sealed trait BatchEntityUpdate[S, M[O] <: BaseMapping[S, O], +Res]
			extends EntitiesBatch[S, S, M, Res] with BatchUpdate[S, M, Res] with EntityUpdates[Seq[S], S, M, Seq[Res]]
			   with EntityStatementsTemplate[Seq[S], M, Seq[Res], UpdateBatchReturningEntities[S, M]]
		{
			override def updatingKeys(keys :Seq[TypedMapping[_, From[M]]]) :UpdateBatchReturningEntities[S, M] =
				new DefaultUpdateBatchReturningEntities[S, M](dml updatingKeys keys)

			protected def dml :EntityUpdate[S, S, M, Res]
		}

		sealed trait GroundBatchEntityUpdate[S, M[O] <: BaseMapping[S, O], +Res]
			extends GroundEntitiesBatch[S, S, M, Res] with GroundBatchUpdate[S, M, Res]
			   with EntityUpdates[Any, S, M, Seq[Res]]
			   with EntityStatementsTemplate[Any, M, Seq[Res], GroundUpdateBatchReturningEntities[S, M]]
		{
			override def updatingKeys(keys :Seq[TypedMapping[_, From[M]]]) :GroundUpdateBatchReturningEntities[S, M] =
				new DefaultGroundUpdateBatchReturningEntities[S, M](dml updatingKeys keys, args)

			protected def dml :BatchEntityUpdate[S, M, Res]
			protected def args :Seq[S]
		}



		/** An interface of an ''update'' statement providing factory methods for modifying its ''set'' clause.
		  * Whether the new [[net.noresttherein.oldsql.sql.ComponentSetter setters]] will supplant or supplement
		  * the existing set depends on the implementation - consult the documentation of the factory method
		  * which created this instance.
		  */ //factories in all these must be before Update, as table there is protected and public in Update
		sealed trait UpdateSeed[Args, M[O] <: MappingAt[O], +Res]
			extends GenericUpdateFactory[Args, M, UpdateSeed[Args, M, Res]] with Update[Args, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying its ''set'' clause.
		  * Whether the new [[net.noresttherein.oldsql.sql.ComponentSetter setters]] will supplant or supplement
		  * the existing set depends on the implementation - consult the documentation of the factory method
		  * which created this instance. This instance - and those created with the aforementioned factory methods
		  * is a [[net.noresttherein.oldsql.sql.Update.syntax.RowUpdate RowUpdate]], meaning it also offers
		  * [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]] method,
		  * which allows listing the components with automatically generated keys which should be returned
		  * by the update.
		  */
		sealed trait RowUpdateSeed[Args, M[O] <: MappingAt[O], +Res]
			extends GenericUpdateFactory[Args, M, RowUpdateSeed[Args, M, Res]]
			   with UpdateSeed[Args, M, Res] with RowUpdate[Args, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying its ''set'' clause.
		  * Whether the new [[net.noresttherein.oldsql.sql.ComponentSetter setters]] will supplant or supplement
		  * the existing set depends on the implementation - consult the documentation of the factory method
		  * which created this instance. This instance - and those created with the aforementioned factory methods
		  * is a [[net.noresttherein.oldsql.sql.Update.syntax.EntityUpdate EntityUpdate]], meaning it also offers
		  * [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]]
		  * and [[net.noresttherein.oldsql.sql.TableStatement.UpdatingClauseFactory.updating updated]] methods,
		  * which allow listing the components with automatically generated keys which should be returned
		  * by the update.
		  */
		sealed trait EntityUpdateSeed[Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericUpdateFactory[Args, M, EntityUpdateSeed[Args, S, M, Res]]
			   with RowUpdateSeed[Args, M, Res] with EntityUpdate[Args, S, M, Res] //ParamEntityUpdate[Args, M, Int]

		/** An interface of an ''update'' statement providing factory methods for modifying
		  * its ''set'' and ''where'' clauses. The new condition, if provided, will replace the current one (typically
		  * the 'natural' update of all updatable columns in the table, as defined by its mapping, but may depend on
		  * the implementation); whether the new [[net.noresttherein.oldsql.sql.ComponentSetter setters]]
		  * will supplant or supplement the existing set depends on the implementation - consult the documentation
		  * of the factory method which created this instance.
		  */
		sealed trait UpdateWhereSeed[Args, M[O] <: MappingAt[O], +Res]
			extends GenericUpdateFactory[Args, M, UpdateWhereSeed[Args, M, Res]]
			   with WhereClauseFactory[Args, M, UpdateWhereAll[Args, M, Res]]
			   with UpdateSeed[Args, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying
		  * its ''set'' and ''where'' clauses. The new condition, if provided, will replace the current one (typically
		  * the 'natural' update of all updatable columns in the table, as defined by its mapping, but may depend on
		  * the implementation); whether the new [[net.noresttherein.oldsql.sql.ComponentSetter setters]]
		  * will supplant or supplement the existing set depends on the implementation - consult the documentation
		  * of the factory method which created this instance. This instance - and those created with the aforementioned
		  * factory methods is a [[net.noresttherein.oldsql.sql.Update.syntax.RowUpdate RowUpdate]], meaning
		  * it also offers [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]]
		  * method, which allows listing the components with automatically generated keys which should be returned
		  * by the update.
		  */
		sealed trait RowUpdateWhereSeed[Args, M[O] <: MappingAt[O], +Res]
			extends GenericUpdateFactory[Args, M, RowUpdateWhereSeed[Args, M, Res]]
			   with WhereClauseFactory[Args, M, RowUpdateWhereAll[Args, M, Res]]
			   with RowUpdateSeed[Args, M, Res] with UpdateWhereSeed[Args, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying
		  * its ''set'' and ''where'' clauses. The new condition, if provided, will replace the current one (typically
		  * the 'natural' update of all updatable columns in the table, as defined by its mapping, but may depend on
		  * the implementation); whether the new [[net.noresttherein.oldsql.sql.ComponentSetter setters]]
		  * will supplant or supplement the existing set depends on the implementation - consult the documentation
		  * of the factory method which created this instance. This instance - and those created with the aforementioned
		  * factory methods is a [[net.noresttherein.oldsql.sql.Update.syntax.EntityUpdate EntityUpdate]], meaning
		  * it also offers [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]]
		  * and [[net.noresttherein.oldsql.sql.TableStatement.UpdatingClauseFactory.updating updated]] methods,
		  * which allow listing the components with automatically generated keys which should be returned
		  * by the update.
		  */
		sealed trait EntityUpdateWhereSeed[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericUpdateFactory[S, M, EntityUpdateWhereSeed[S, M, Res]]
			   with WhereClauseFactory[S, M, EntityUpdateWhereAll[S, M, Res]]
			   with RowUpdateWhereSeed[S, M, Res] with ParamEntityUpdate[S, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying ''where'' clause
		  * by adding additional, alternative (to be joined in a logical disjunction with the preexisting condition)
		  * filter conditions.
		  */
		sealed trait UpdateWhere[Args, M[O] <: MappingAt[O], +Res]
			extends WhereAnyClauseFactory[Args, M, UpdateWhere[Args, M, Res]] with Update[Args, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying ''where'' clause
		  * by adding additional, alternative (to be joined in a logical disjunction with the preexisting condition)
		  * filter conditions. This instance - and those created with the aforementioned factory methods
		  * is a [[net.noresttherein.oldsql.sql.Update.syntax.RowUpdate RowUpdate]], meaning it also offers
		  * [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]] method,
		  * which allows listing the components with automatically generated keys which should be returned
		  * by the update.
		  */
		sealed trait RowUpdateWhere[Args, M[O] <: MappingAt[O], +Res]
			extends WhereAnyClauseFactory[Args, M, RowUpdateWhere[Args, M, Res]]
			   with UpdateWhere[Args, M, Res] with RowUpdate[Args, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying ''where'' clause
		  * by adding additional, alternative (to be joined in a logical disjunction with the preexisting condition)
		  * filter conditions. This instance - and those created with the aforementioned factory methods
		  * is a [[net.noresttherein.oldsql.sql.Update.syntax.EntityUpdate EntityUpdate]], meaning it also offers
		  * [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]]
		  * and [[net.noresttherein.oldsql.sql.TableStatement.UpdatingClauseFactory.updating updated]] methods,
		  * which allow listing the components with automatically generated keys which should be returned
		  * by the update.
		  */
		sealed trait EntityUpdateWhere[S, M[O] <: BaseMapping[S, O], +Res]
			extends WhereAnyClauseFactory[S, M, EntityUpdateWhere[S, M, Res]]
			   with RowUpdateWhere[S, M, Res] with ParamEntityUpdate[S, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying ''where'' clause
		  * by adding additional filter conditions, to be joined in a logical disjunction with the preexisting
		  * expression.
		  */
		sealed trait UpdateWhereAll[Args, M[O] <: MappingAt[O], +Res]
			extends WhereAllClauseFactory[Args, M, UpdateWhere[Args, M, Res], UpdateWhereAll[Args, M, Res]]
			   with UpdateWhere[Args, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying ''where'' clause
		  * by adding additional filter conditions, to be joined in a logical disjunction with the preexisting
		  * expression. This instance - and those created with the aforementioned factory methods
		  * is a [[net.noresttherein.oldsql.sql.Update.syntax.RowUpdate RowUpdate]], meaning it also offers
		  * [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]] method
		  * which allows listing the components with automatically generated keys which should be returned
		  * by the update.
		  */
		sealed trait RowUpdateWhereAll[Args, M[O] <: MappingAt[O], +Res]
			extends WhereAllClauseFactory[Args, M, RowUpdateWhere[Args, M, Res], RowUpdateWhereAll[Args, M, Res]]
			   with RowUpdateWhere[Args, M, Res] with UpdateWhereAll[Args, M, Res]

		/** An interface of an ''update'' statement providing factory methods for modifying ''where'' clause
		  * by adding additional filter conditions, to be joined in a logical disjunction with the preexisting
		  * expression. This instance - and those created with the aforementioned factory methods
		  * is a [[net.noresttherein.oldsql.sql.Update.syntax.EntityUpdate EntityUpdate]], meaning it also offers
		  * [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]]
		  * and [[net.noresttherein.oldsql.sql.TableStatement.UpdatingClauseFactory.updating updated]] methods,
		  * which allow listing the components with automatically generated keys which should be returned
		  * by the update.
		  */
		sealed trait EntityUpdateWhereAll[S, M[O] <: BaseMapping[S, O], +Res]
			extends WhereAllClauseFactory[S, M, EntityUpdateWhere[S, M, Res], EntityUpdateWhereAll[S, M, Res]]
			   with RowUpdateWhereAll[S, M, Res] with EntityUpdateWhere[S, M, Res]




		sealed trait UpdateReturningKey[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends UpdateReturning[Args, M, Res] with ReturningTupleClause[Args, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Res, Update[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: UpdateReturningKey[Args, M, X, X]
		}

		private trait UpdateReturningKeyExpansion[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends UpdateReturningKey[Args, M, Keys, Res]
		{
			override type tuple[X <: Chain] = UpdateReturningKey[Args, M, X, X]

			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) :UpdateReturningKey[Args, M, Keys~T, Keys~T] =
				//don't export it for better debugging, columns will be exported anyway and only they really matter
				new UpdateComponentsReturning[Args, M, Keys, T](this, key(table.row))
		}

		private class UpdateComponentReturning[Args, M[O] <: MappingAt[O], K]
		                                      (override val statement :Update[Args, M, Any],
		                                       override val key :TypedMapping[K, From[M]])
			extends ReturningTupleSingleton[Args, M, K](statement, key)
			   with UpdateReturningKeyExpansion[Args, M, @~ ~ K, K]

		private class UpdateComponentsReturning[Args, M[O] <: MappingAt[O], Keys <: Chain, K]
		                                       (override val init :UpdateReturningKey[Args, M, Keys, Any],
		                                        override val key :TypedMapping[K, From[M]])
			extends ReturningProperTuple[Args, M, Keys, K](init, key)
			   with UpdateReturningKeyExpansion[Args, M, Keys ~ K, Keys ~ K]
		{
			override val statement = init.statement
		}


		sealed trait UpdatesReturningKeys[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends UpdatesReturning[Args, M, Seq[Res]] with ReturningTuplesClauses[Args, M, Keys, Res, Seq]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Seq[Res], Update[Nothing, M, Any]]
		{
			override type tuple[X <: Chain] <: UpdatesReturningKeys[Args, M, X, X]
		}

		sealed trait UpdateReturningKeys[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends UpdateReturning[Args, M, Seq[Res]] with UpdatesReturningKeys[Args, M, Keys, Res]
			   with ReturningTuplesClause[Args, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Seq[Res], Update[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: UpdateReturningKeys[Args, M, X, X]
		}

		private class DefaultUpdateReturningKeys[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
		                                        (override val dml :UpdateReturningKey[Args, M, Keys, Res])
			extends ReturningTuplesTemplate[Args, M, Keys, Res, Update[Args, M, Any]]
			   with UpdateReturningKeys[Args, M, Keys, Res]
		{
			override type tuple[X <: Chain] = UpdateReturningKeys[Args, M, X, X]
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultUpdateReturningKeys[Args, M, Keys ~ T, Keys ~ T](dml x key)
		}


		sealed trait UpdateBatchReturningKeys[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends UpdatesReturningKeys[Seq[Args], M, Keys, Res] with BatchReturningTuplesClause[Args, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Seq[Args], M, Keys, Seq[Res], Update[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: UpdateBatchReturningKeys[Args, M, X, X]
		}

		private class DefaultUpdateBatchReturningKeys[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
		                                             (override val dml :UpdateReturningKey[Args, M, Keys, Res])
			extends RepeatedDML[Args, Res] with ReturningTupleSeqTemplate[Seq[Args], M, Keys, Res, Update[Args, M, Any]]
			   with UpdateBatchReturningKeys[Args, M, Keys, Res]
		{
			override type tuple[X <: Chain] = UpdateBatchReturningKeys[Args, M, X, X]
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultUpdateBatchReturningKeys[Args, M, Keys ~ T, Keys ~ T](dml x key)
		}

		sealed trait GroundUpdateBatchReturningKeys[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends UpdatesReturningKeys[Any, M, Keys, Res] with GroundBatchReturningTuplesClause[Args, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Any, M, Keys, Seq[Res], Update[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: GroundUpdateBatchReturningKeys[Args, M, X, X]
		}

		private class DefaultGroundUpdateBatchReturningKeys[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
		              (override val dml :UpdateBatchReturningKeys[Args, M, Keys, Res], override val args :Seq[Args])
			extends BoundDML[Seq[Args], Seq[Res]] with ReturningTupleSeqTemplate[Any, M, Keys, Res, Update[Args, M, Any]]
			   with GroundUpdateBatchReturningKeys[Args, M, Keys, Res]
			   with BoundDML.Impl[Seq[Args], Seq[Res], Update.table[M]#DML]
		{
			override type tuple[X <: Chain] = GroundUpdateBatchReturningKeys[Args, M, X, X]
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultGroundUpdateBatchReturningKeys[Args, M, Keys ~ T, Keys ~ T](dml x key, args)
		}




		sealed trait UpdateReturningEntity[-Args, S, M[O] <: BaseMapping[S, O]]
			extends UpdateReturning[Args, M, S] with ReturningEntityClause[Args, S, M]
			   with GenericReturningEntitiesClause[Args, S, M, S, Update[Args, M, Any], UpdateReturningEntity[Args, S, M]]

		private[Update] class DefaultUpdateReturningEntity[Args, S, M[O] <: BaseMapping[S, O]]
		                                                  (override val statement :Update[Args, M, Any],
		                                                   override val keys :Seq[TypedMapping[_, From[M]]],
		                                                   override val columnNames :Seq[String],
		                                                   override val result :StatementResult[S, S])
			extends AbstractReturningEntities[Args, S, M, S](statement, keys, columnNames, result)
			   with UpdateReturningEntity[Args, S, M]
		{
			def this(statement :Update[Args, M, Any], keys :Seq[TypedMapping[_, From[M]]], result :StatementResult[S, S]) =
				this(statement, keys, Returning.implementation.columnNames(statement.table, keys), result)

			override type x[T] = UpdateReturningEntity[Args, S, M]

			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) = {
				val comp = key(table.row)
				val columns = columnNames :++ Returning.implementation.columnNames(table, comp)
				new DefaultUpdateReturningEntity[Args, S, M](statement, keys :+ comp, columns, result)
			}
			override def toReturningEntities :UpdateReturningEntities[Args, S, M] =
				new DefaultUpdateReturningEntities[Args, S, M](statement, keys, columnNames, result.batch(1))
		}


		sealed trait UpdatesReturningEntities[-Args, S, M[O] <: BaseMapping[S, O]]
			extends UpdatesReturning[Args, M, Seq[S]] with ReturningEntitiesClauses[Args, S, M, Seq[S]]
			   with GenericReturningEntitiesClause[Args, S, M, Seq[S], Update[Nothing, M, Any],
			                                       UpdatesReturningEntities[Args, S, M]]

		sealed trait UpdateReturningEntities[-Args, S, M[O] <: BaseMapping[S, O]]
			extends UpdateReturning[Args, M, Seq[S]] with ReturningEntitiesClause[Args, S, M]
			   with UpdatesReturningEntities[Args, S, M]
			   with GenericReturningEntitiesClause[Args, S, M, Seq[S],
			                                       Update[Args, M, Any], UpdateReturningEntities[Args, S, M]]

		private class DefaultUpdateReturningEntities[Args, S, M[O] <: BaseMapping[S, O]]
		              (override val statement :Update[Args, M, Any], override val keys :Seq[TypedMapping[_, From[M]]],
		               override val columnNames :Seq[String], override val result :StatementResult[S, Seq[S]])
			extends AbstractReturningEntities[Args, S, M, Seq[S]](statement, keys, columnNames, result)
			   with UpdateReturningEntities[Args, S, M]
		{
			override type x[T] = UpdateReturningEntities[Args, S, M]

			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) = {
				val comp = key(table.row)
				val columns = columnNames :++ Returning.implementation.columnNames(table, comp)
				new DefaultUpdateReturningEntities[Args, S, M](statement, keys :+ comp, columns, result)
			}
		}

		sealed trait UpdateBatchReturningEntities[S, M[O] <: BaseMapping[S, O]]
			extends UpdatesReturningEntities[Seq[S], S, M] with BatchReturningEntitiesClause[S, S, M]
			   with GenericReturningEntitiesClause[Seq[S], S, M, Seq[S], Update[S, M, Any],
			                                       UpdateBatchReturningEntities[S, M]]

		private class DefaultUpdateBatchReturningEntities[S, M[O] <: BaseMapping[S, O]]
		                                                 (override val dml :UpdateReturningEntity[S, S, M])
			extends UpdateBatchReturningEntities[S, M] with RepeatedDML[S, S]
			   with GenericBatchReturningEntities[Seq[S], S, M, Seq[S], Update[S, M, Any],
			                                      UpdateBatchReturningEntities[S, M]]
		{
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultUpdateBatchReturningEntities[S, M](dml x key)
		}


		sealed trait GroundUpdateBatchReturningEntities[S, M[O] <: BaseMapping[S, O]]
			extends UpdatesReturningEntities[Any, S, M] with GroundBatchReturningEntitiesClause[S, M]
			   with GenericReturningEntitiesClause[Any, S, M, Seq[S], Update[S, M, Any],
			                                       GroundUpdateBatchReturningEntities[S, M]]

		private class DefaultGroundUpdateBatchReturningEntities[S, M[O] <: BaseMapping[S, O]]
		              (override val dml :UpdateBatchReturningEntities[S, M], override val args :Seq[S])
			extends GroundUpdateBatchReturningEntities[S, M] with BoundDML[Seq[S], Seq[S]]
			   with BoundDML.Impl[Seq[S], Seq[S], Update.table[M]#DML]
			   with GenericBatchReturningEntities[Any, S, M, Seq[S], Update[S, M, Any],
			                                      GroundUpdateBatchReturningEntities[S, M]]
		{
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultGroundUpdateBatchReturningEntities[S, M](dml x key, args)
		}




		sealed trait UpdateParam[Arg] extends Any {
			def table[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			         (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S], form :SQLForm[Arg])
					:UpdateFactory[Arg, S, T] =
			{
				val t = reveal(table)
				val domain = From(t).param[Arg]
				new UpdateFactory[Arg, S, T](domain, domain, t)
			}

			def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			         (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S], form :SQLForm[Arg])
					:UpdateFactory[Arg, S, T] =
				this.table(table)

			def all[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			       (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S], form :SQLForm[Arg])
					:UpdateAllFactory[Arg, S, T] =
				new UpdateAllFactory[Arg, S, T](From(reveal(table)).param[Arg])

//			def *(max :Int) = new CombinedUpdateParam[Arg](max)
		}

//		class CombinedUpdateParam[Arg] private[Update](private val max :Int) extends AnyVal {
//			def table[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
//			         (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S], form :SQLForm[Arg])
//					:MultiUpdateFactory[Arg, S, T] =
//			{
//				val domain = From(reveal(table)).param[Arg]
//				new MultiUpdateFactory[Arg, S, T](domain, domain, reveal(table), max)
//			}
//		}



		sealed trait GenericUpdateFactory[Arg, M[O] <: MappingAt[O], +Res]
			extends Any with SetClauseFactory[Arg, M, From[M] WithParam Arg, Res]
		{   //consider: removing this, and moving all InsertUpdateFactory.set methods to SetClauseFactory
			def setAll(setters :(M[From[M]], M[SetDomain], UnboundParam.Last[Arg])
			                    => Seq[From[M] := SetDomain]) :Res =
			{
				val base = setDomain
				setAll(setters(table.row, table.row, base.last.mapping).map(_.anchor(base.left, base)))
			}

			def set(setter :(M[From[M]], M[SetDomain], UnboundParam.Last[Arg])
	                        => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				set(setter(table.row, table.row, base.last.mapping).anchor(base.left, base))
			}


			def merge(setter :(M[From[M]], SetParamScope[Arg, M]) => From[M] := SetDomain) :Res = {
				val base = setDomain
				set(setter(table.row, new SetParamScope(base)))
			}

			def merge(setter :(M[From[M]], SetParamScope[Arg, M], SetParamScope[Arg, M])
			                  => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val facade = new SetParamScope(base)
				set(setter(table.row, facade, facade))
			}

			def merge(setter :(M[From[M]], SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M])
			                  => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val facade = new SetParamScope(base)
				set(setter(table.row, facade, facade, facade))
			}

			def merge(setter :(M[From[M]], SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M],
			                   SetParamScope[Arg, M])
			                  => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val facade = new SetParamScope(base)
				set(setter(table.row, facade, facade, facade, facade))
			}

			def merge(setter :(M[From[M]], SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M],
			                   SetParamScope[Arg, M], SetParamScope[Arg, M])
			                  => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val facade = new SetParamScope(base)
				set(setter(table.row, facade, facade, facade, facade, facade))
			}

			def merge(setter :(M[From[M]], SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M],
			                   SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M])
			                  => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val facade = new SetParamScope(base)
				set(setter(table.row, facade, facade, facade, facade, facade, facade))
			}

			def merge(setter :(M[From[M]], SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M],
			                   SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M],
			                   SetParamScope[Arg, M])
			                  => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val facade = new SetParamScope(base)
				set(setter(table.row, facade, facade, facade, facade, facade, facade, facade))
			}

			def merge(setter :(M[From[M]], SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M],
			                   SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M],
			                   SetParamScope[Arg, M], SetParamScope[Arg, M])
			                  => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val facade = new SetParamScope(base)
				set(setter(table.row, facade, facade, facade, facade, facade, facade, facade, facade))
			}

			def merge(setter :(M[From[M]], SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M],
			                   SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M],
			                   SetParamScope[Arg, M], SetParamScope[Arg, M], SetParamScope[Arg, M])
			                  => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val facade = new SetParamScope(base)
				set(setter(table.row, facade, facade, facade, facade, facade, facade, facade, facade, facade))
			}

			def merge(setters :Seq[(M[From[M]], SetParamScope[Arg, M]) => From[M] := SetDomain]) :Res = {
				val base = setDomain
				val row = table.row[From[M]]
				val facade = new SetParamScope(base)
				setAll(setters.map(_(row, facade).anchor(base.left, base)))
			}


			def mergeAll(setters :(M[From[M]], SetParamScope[Arg, M]) => Seq[From[M] := SetDomain]) :Res = {
				val base = setDomain
				setAll(setters(table.row, new SetParamScope(base)).map(_.anchor(base.left, base)))
			}


			def updateAll(setter :(M[From[M]], M[SetDomain]) => Seq[From[M] := SetDomain]) :Res = {
				val base = setDomain
				setAll(setter(table.row, table.row).map(_.anchor(base.left, base)))
			}

			def update(setter :M[From[M]] => From[M] := RowProduct) :Res = {
				val base = setDomain.left
				set(setter(table.row[From[M]]).anchor(base, base))
			}

			def update(setter :(M[From[M]], M[SetDomain]) => (From[M] := SetDomain)) :Res = {
				val base = setDomain
				set(setter(table.row, table.row).anchor(base.left, base))
			}

			def update(setter :(M[From[M]], M[SetDomain], M[SetDomain]) => (From[M] := SetDomain)) :Res = {
				val base = setDomain
				val row = table.row[SetDomain]
				set(setter(table.row, row, row).anchor(base.left, base))
			}

			def update(setter :(M[From[M]], M[SetDomain], M[SetDomain], M[SetDomain])
			                   => (From[M] := SetDomain)) :Res =
			{
				val base = setDomain
				val row = table.row[SetDomain]
				set(setter(table.row, row, row, row).anchor(base.left, base))
			}

			def update(setter :(M[From[M]], M[SetDomain], M[SetDomain], M[SetDomain], M[SetDomain])
			                   => (From[M] := SetDomain)) :Res =
			{
				val base = setDomain
				val row = table.row[SetDomain]
				set(setter(table.row, row, row, row, row).anchor(base.left, base))
			}

			def update(setter :(M[From[M]], M[SetDomain], M[SetDomain], M[SetDomain], M[SetDomain],
			                    M[SetDomain])
			                   => (From[M] := SetDomain)) :Res =
			{
				val base = setDomain
				val row = table.row[SetDomain]
				set(setter(table.row, row, row, row, row, row).anchor(base.left, base))
			}

			def update(setter :(M[From[M]], M[SetDomain], M[SetDomain], M[SetDomain], M[SetDomain],
			                    M[SetDomain], M[SetDomain])
			                   => (From[M] := SetDomain)) :Res =
			{
				val base = setDomain
				val row = table.row[SetDomain]
				set(setter(table.row, row, row, row, row, row, row).anchor(base.left, base))
			}

			def update(setter :(M[From[M]], M[SetDomain], M[SetDomain], M[SetDomain], M[SetDomain],
			                    M[SetDomain], M[SetDomain], M[SetDomain])
			                   => (From[M] := SetDomain)) :Res =
			{
				val base = setDomain
				val row = table.row[SetDomain]
				set(setter(table.row, row, row, row, row, row, row, row).anchor(base.left, base))
			}

			def update(setter :(M[From[M]], M[SetDomain], M[SetDomain], M[SetDomain], M[SetDomain],
			                    M[SetDomain], M[SetDomain], M[SetDomain], M[SetDomain])
			                   => (From[M] := SetDomain)) :Res =
			{
				val base = setDomain
				val row = table.row[SetDomain]
				set(setter(table.row, row, row, row, row, row, row, row, row).anchor(base.left, base))
			}

			def update(setter :(M[From[M]], M[SetDomain], M[SetDomain], M[SetDomain], M[SetDomain],
			                    M[SetDomain], M[SetDomain], M[SetDomain], M[SetDomain], M[SetDomain])
			                   => (From[M] := SetDomain)) :Res =
			{
				val base = setDomain
				val row = table.row[SetDomain]
				set(setter(table.row, row, row, row, row, row, row, row, row, row).anchor(base.left, base))
			}
		}


		sealed trait GenericSupplantedUpdateFactory[Arg, M[O] <: MappingAt[O], +Res]
			extends Any with SupplantClauseFactory[Arg, M, From[M] WithParam Arg, Res]
		{
			def supplantAll(setters :(M[From[M]], M[SetDomain], UnboundParam.Last[Arg])
				=> Seq[From[M] := SetDomain]) :Res =
			{
				val base = setDomain
				supplantAll(setters(table.row, table.row, base.last.mapping).map(_.anchor(base.left, base)))
			}

			def supplant(update :(M[From[M]], M[SetDomain], UnboundParam.Last[Arg]) => From[M] := SetDomain) :Res = {
				val base = setDomain
				supplant(update(table.row, table.row, base.last.mapping).anchor(base.left, base))
			}
			//consider: supplantUpdate, supplantMerge - how to name those?
		}



		final class UpdateFactory[Arg, S, M[O] <: BaseMapping[S, O]] private[Update]
		                         (protected override val setDomain :From[M] WithParam Arg,
		                          protected val whereDomain :From[M] WithParam Arg,
		                          protected override val table :RelVar[M])
			extends GenericUpdateFactory[Arg, M, UpdateSetClause[Arg, S, M]]
		{
			protected override def setAll(updates :Seq[From[M] := (From[M] WithParam Arg)]) :UpdateSetClause[Arg, S, M] =
				new UpdateSetClause[Arg, S, M](setDomain, whereDomain, table, updates)

			def *(max :Int) :MultiUpdateFactory[Arg, S, M] =
				new MultiUpdateFactory[Arg, S, M](setDomain, whereDomain, table, max)
		}


		sealed class UpdateSetClause[Arg, S, M[O] <: BaseMapping[S, O]] private[Update]
		                            (protected override val setDomain :From[M] WithParam Arg,
		                             protected override val whereDomain :From[M] WithParam Arg,
		                             protected override val table :RelVar[M],
		                             protected val setters :Seq[From[M] := (From[M] WithParam Arg)])
			extends GenericUpdateFactory[Arg, M, UpdateSetClause[Arg, S, M]]
			   with WhereClauseFactory[Arg, M, RowUpdateWhere[Arg, M, Int]]
		{
			protected override def setAll(updates :Seq[From[M] := (From[M] WithParam Arg)])
					:UpdateSetClause[Arg, S, M] =
				new UpdateSetClause[Arg, S, M](setDomain, whereDomain, table, setters :++ updates)

			protected override def set(update :From[M] := (From[M] WithParam Arg)) :UpdateSetClause[Arg, S, M] =
				new UpdateSetClause[Arg, S, M](setDomain, whereDomain, table, setters :+ update)

			protected override def where(condition :SingleBoolean[From[M] WithParam Arg]) :RowUpdateWhere[Arg, M, Int] =
				new DefaultUpdate[Arg, S, M](setDomain, whereDomain, table, setters, condition)
		}



		class UpdateAllFactory[Arg, S, M[O] <: BaseMapping[S, O]] private[Update]
		                      (protected override val setDomain :From[M] WithParam Arg)
			extends AnyVal with GenericUpdateFactory[Arg, M, RowUpdateSeed[Arg, M, Int]]
		{
			protected override def table :RelVar[M] = setDomain.left.table.castFrom[Table[M], RelVar[M]]

			protected override def setAll(updates :Seq[From[M] := (From[M] WithParam Arg)]) :RowUpdateSeed[Arg, M, Int] =
				new DefaultUpdate[Arg, S, M](setDomain, setDomain, table, updates, True)
		}



//todo:
/*
		class UpdateBatchFactory[Arg, S, M[O] <: BaseMapping[S, O]] private[Insert]
		                        (protected override val setDomain :From[M] WithParam Arg)
			extends AnyVal with GenericUpdateFactory[Arg, M, BatchUpdateSetClause[Arg, M, Int]]
		{
			protected override def table :RelVar[M] = setDomain.left.table.castFrom[Table[M], RelVar[M]]

			protected override def setAll(setters :Seq[From[M] := JoinParam.Last[Arg]]) :BatchUpdateSetClause[Arg, M] =
				new BatchUpdateSetClause[Arg, S, M](setDomain, table, setters)
		}
*/



		sealed class MultiUpdateFactory[Arg, S, M[O] <: BaseMapping[S, O]] private[Update]
		                               (protected override val setDomain :From[M] WithParam Arg,
		                                protected val whereDomain :From[M] WithParam Arg,
		                                protected override val table :RelVar[M], val max :Int)
			extends GenericUpdateFactory[Arg, M, MultiUpdateSetClause[Arg, S, M]]
		{
			protected override def setAll(setters :Seq[From[M] := (From[M] WithParam Arg)])
					:MultiUpdateSetClause[Arg, S, M] =
				new MultiUpdateSetClause[Arg, S, M](setDomain, whereDomain, table, setters, max)
		}

		sealed class EntityMultiUpdateFactory[S, M[O] <: BaseMapping[S, O]] private[Update]
		                                     (protected override val setDomain :From[M] WithParam S,
		                                      protected val whereDomain :From[M] WithParam S,
		                                      protected override val table :RelVar[M], val max :Int)
			extends GenericUpdateFactory[S, M, EntityMultiUpdateSetClause[S, M]]
		{
			protected override def setAll(setters :Seq[From[M] := (From[M] WithParam S)])
					:EntityMultiUpdateSetClause[S, M] =
				new EntityMultiUpdateSetClause[S, M](setDomain, whereDomain, table, setters, max)
		}


		sealed class MultiUpdateSetClause[Arg, S, M[O] <: BaseMapping[S, O]] private[Update]
		                                 (protected override val setDomain :From[M] WithParam Arg,
		                                  protected override val whereDomain :From[M] WithParam Arg,
		                                  protected override val table :RelVar[M],
		                                  protected val setters :Seq[From[M] := (From[M] WithParam Arg)],
		                                  override val max :Int)
			extends MultiUpdateFactory[Arg, S, M](setDomain, whereDomain, table, max)
			   with WhereClauseFactory[Arg, M, MultiUpdateWhereAll[Arg, S, M]]
		{
			protected override def setAll(setters :Seq[From[M] := (From[M] WithParam Arg)])
					:MultiUpdateSetClause[Arg, S, M] =
				new MultiUpdateSetClause[Arg, S, M](setDomain, whereDomain, table, this.setters :++ setters, max)

			protected override def set(setter :From[M] := (From[M] WithParam Arg))
					:MultiUpdateSetClause[Arg, S, M] =
				new MultiUpdateSetClause[Arg, S, M](setDomain, whereDomain, table, this.setters :+ setter, max)

			protected override def where(condition :SingleBoolean[From[M] WithParam Arg])
					:MultiUpdateWhereAll[Arg, S, M] =
			{
				implicit val form = setDomain.last.mapping.form * max
				new MultiUpdateWhereAll[Arg, S, M](
					setDomain.left.param[Seq[Arg]](form), setDomain, whereDomain, table, setters, condition, max
				)
			}
		}

		final class EntityMultiUpdateSetClause[S, M[O] <: BaseMapping[S, O]] private[Update]
		                                      (protected override val setDomain :From[M] WithParam S,
		                                       protected override val whereDomain :From[M] WithParam S,
		                                       protected override val table :RelVar[M],
		                                       protected override val setters :Seq[From[M] := (From[M] WithParam S)],
		                                       override val max :Int)
			extends MultiUpdateSetClause[S, S, M](setDomain, whereDomain, table, setters, max)
			   with WhereClauseFactory[S, M, EntityMultiUpdateWhereAll[S, M]]
		{
			protected override def setAll(setters :Seq[From[M] := (From[M] WithParam S)])
					:EntityMultiUpdateSetClause[S, M] =
				new EntityMultiUpdateSetClause(setDomain, whereDomain, table, this.setters :++ setters, max)

			protected override def set(setter :From[M] := (From[M] WithParam S))
					:EntityMultiUpdateSetClause[S, M] =
				new EntityMultiUpdateSetClause(setDomain, whereDomain, table, this.setters :+ setter, max)

			protected override def where(condition :SingleBoolean[From[M] WithParam S])
					:EntityMultiUpdateWhereAll[S, M] =
			{
				implicit val form = setDomain.last.mapping.form * max
				new EntityMultiUpdateWhereAll[S, M](
					setDomain.left.param[Seq[S]](form), setDomain, whereDomain, table, setters, condition, max
				)
			}
		}


		sealed class MultiUpdateWhere[Arg, S, M[O] <: BaseMapping[S, O]] private[Update]
		                             (protected override val domain :From[M] WithParam Seq[Arg],
		                              protected val setDomain :From[M] WithParam Arg,
		                              protected override val whereDomain :From[M] WithParam Arg,
		                              override val table :RelVar[M],
		                              protected val setOne :Seq[From[M] := (From[M] WithParam Arg)],
		                              protected val whereOne :SingleBoolean[From[M] WithParam Arg], val max :Int)
			extends ParamUpdate[Seq[Arg], M] with WhereAnyClauseFactory[Arg, M, MultiUpdateWhere[Arg, S, M]]
		{
			protected override def or(condition :SingleBoolean[From[M] WithParam Arg]) :MultiUpdateWhere[Arg, S, M] =
				new MultiUpdateWhere[Arg, S, M](
					domain, setDomain, whereDomain, table, setOne, whereOne || condition, max
				)

			override lazy val setters :Seq[From[M] := (From[M] WithParam Seq[Arg])] =
				(0 until max).flatMap { i =>
					val scribe = SQLScribe.replaceParam(
						setDomain, domain, setDomain.last.toRelationSQL, domain.last.toRelationSQL)(seqAt(i)
					)
					setOne map { (setter :From[M] := (From[M] WithParam Arg)) =>
						setter.lvalue := scribe[Single, setter.Value](setter.rvalue)
					}
				}

			override lazy val condition :SingleBoolean[From[M] WithParam Seq[Arg]] =
				TableStatement.repeatedWhere[Arg, S, M](domain, whereDomain, whereOne, max)
		}

		sealed trait EntityMultiUpdateWhere[S, M[O] <: BaseMapping[S, O]]
			extends MultiUpdateWhere[S, S, M] with WhereAnyClauseFactory[S, M, EntityMultiUpdateWhere[S, M]]
		{
			protected override def or(condition :SingleBoolean[From[M] WithParam S]) :EntityMultiUpdateWhere[S, M] =
				new MultiUpdateWhere[S, S, M](
					domain, setDomain, whereDomain, table, setOne, whereOne || condition, max
				) with EntityMultiUpdateWhere[S, M]
		}


		sealed class MultiUpdateWhereAll[Arg, S, M[O] <: BaseMapping[S, O]] private[Update]
		                                (protected override val domain :From[M] WithParam Seq[Arg],
		                                 protected override val setDomain :From[M] WithParam Arg,
		                                 protected override val whereDomain :From[M] WithParam Arg,
		                                 override val table :RelVar[M],
		                                 protected override val setOne :Seq[From[M] := (From[M] WithParam Arg)],
		                                 protected override val whereOne :SingleBoolean[From[M] WithParam Arg],
		                                 override val max :Int)
			extends MultiUpdateWhere[Arg, S, M](domain, setDomain, whereDomain, table, setOne, whereOne, max)
			   with WhereAllClauseFactory[Arg, M, MultiUpdateWhere[Arg, S, M], MultiUpdateWhereAll[Arg, S, M]]
		{
			protected override def and(condition :SingleBoolean[From[M] WithParam Arg])
					:MultiUpdateWhereAll[Arg, S, M] =
				new MultiUpdateWhereAll[Arg, S, M](
					domain, setDomain, whereDomain, table, setOne, this.whereOne && condition, max
				)
		}

		final class EntityMultiUpdateWhereAll[S, M[O] <: BaseMapping[S, O]] private[Update]
				                             (protected override val domain :From[M] WithParam Seq[S],
				                              protected override val setDomain :From[M] WithParam S,
				                              protected override val whereDomain :From[M] WithParam S,
				                              override val table :RelVar[M],
				                              protected override val setOne :Seq[From[M] := (From[M] WithParam S)],
				                              protected override val whereOne :SingleBoolean[From[M] WithParam S],
				                              override val max :Int)
			extends MultiUpdateWhereAll[S, S, M](domain, setDomain, whereDomain, table, setOne, whereOne, max)
			   with EntityMultiUpdateWhere[S, M]
			   with WhereAllClauseFactory[S, M, EntityMultiUpdateWhere[S, M], EntityMultiUpdateWhereAll[S, M]]
		{
			protected override def and(condition :SingleBoolean[WithParam[From[M], S]])
					:EntityMultiUpdateWhereAll[S, M] =
				new EntityMultiUpdateWhereAll[S, M](
					domain, setDomain, whereDomain, table, setOne, this.whereOne && condition, max
				)
		}




		//this is very confusing that calling set ignores the setters on this instance
		sealed class UpdateOne[S, M[O] <: BaseMapping[S, O]] private[Update] (override val table :RelVar[M])
			extends ParamUpdate[S, M] with EntityUpdateWhereSeed[S, M, Int] with ParamEntityUpdate[S, M, Int]
			   with GenericSupplantedUpdateFactory[S, M, EntityUpdateWhereSeed[S, M, Int]]
			   with GenericUpdateFactory[S, M, EntityUpdateWhereSeed[S, M, Int]]
			   with WhereClauseFactory[S, M, EntityUpdateWhereAll[S, M, Int]]
		{
			protected override def setAll(updates :Seq[From[M] := (From[M] WithParam S)])
					:EntityUpdateWhereSeed[S, M, Int] =
				new DefaultEntityUpdate[S, M](domain, whereDomain, table, updates, condition)

			protected override def supplantAll(updates :Seq[From[M] := (From[M] WithParam S)])
					:EntityUpdateWhereSeed[S, M, Int] =
				new SupplantedUpdateOne[S, M](domain, whereDomain, table, updates, condition)

			protected override def where(condition :SingleBoolean[From[M] WithParam S]) :EntityUpdateWhereAll[S, M, Int] =
				new DefaultEntityUpdate[S, M](domain, whereDomain, table, setters, condition)

			//would be better as lazy val, but update to other classes would be necessary
			protected override def setDomain   :From[M] WithParam S = domain
			protected override val whereDomain :From[M] WithParam S = TableStatement.whereDomain(table)
			protected override val domain      :From[M] WithParam S = updateDomain(whereDomain.left)

			override val setters   :Seq[From[M] := (From[M] WithParam S)] =
				PassedArray :+ (table.row[From[M]] := domain.last)
			override val condition :SingleBoolean[From[M] WithParam S] =
				TableStatement.whereEntity(table, whereDomain)

			override def bind(value :S) :Update[Unit, M, Int] = new GroundUpdateOne(table, value)

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :UpdateOne[_, MappingAt @unchecked] => table == other.table
				case other :ParamUpdate[_, _] if other canEqual this =>
					hashCode == other.hashCode && table == other.table &&
						condition == other.condition && setters == other.setters
				case _ => false
			}
			protected override def initToString :String = "Update(?) " + table
		}


		private class SupplantedUpdateOne[S, M[O] <: BaseMapping[S, O]] private[Update]
		                                        (protected override val domain :From[M] WithParam S,
		                                         protected override val whereDomain :From[M] WithParam S,
		                                         override val table :RelVar[M],
		                                         protected val overrides :Seq[From[M] := (From[M] WithParam S)],
		                                         override val condition :SingleBoolean[From[M] WithParam S])
			extends ParamUpdate[S, M] with EntityUpdateWhereSeed[S, M, Int]
		{
			protected override def setDomain :From[M] WithParam S = domain

			protected override def setAll(updates :Seq[From[M] := (From[M] WithParam S)]) :EntityUpdateWhereSeed[S, M, Int] =
				new SupplantedUpdateOne[S, M](domain, whereDomain, table, overrides :++ updates, condition)

			protected override def set(update :From[M] := (From[M] WithParam S)) =
				new SupplantedUpdateOne[S, M](domain, whereDomain, table, overrides :+ update, condition)

			protected override def where(condition :SingleBoolean[From[M] WithParam S]) :EntityUpdateWhereAll[S, M, Int] =
				new DefaultEntityUpdate[S, M](domain, whereDomain, table, setters, condition)

			override lazy val setters :Seq[From[M] := (From[M] WithParam S)] =
				//todo: delay the split into columns until defaultSpelling to use spelling.setterReform
				TableStatement.supplant[S, M, From[M] WithParam S](
					UpdateView, table, domain, ColumnSetter.updates(domain), overrides
				)
		}


		final class UpdateFacade[S, M[O] <: BaseMapping[S, O]] private[Update] (override val table :RelVar[M])
			extends UpdateOne[S, M](table)
		{
			def set(first  :(M[From[M]], UnboundParam.Last[S]) => From[M] := (From[M] WithParam S),
			        second :(M[From[M]], UnboundParam.Last[S]) => From[M] := (From[M] WithParam S),
			        rest   :(M[From[M]], UnboundParam.Last[S]) => From[M] := (From[M] WithParam S)*)
					:EntityUpdateWhereSeed[S, M, Int] =
			{
				val row = table.row[From[M]]
				val param = domain.last.mapping
				setAll((first +: second +: rest).map(_(row, param).anchor(domain.left, domain)))
			}

			def apply(entity :S) :Update[Unit, M, Int] = new GroundUpdateOne[S, M](table, entity)

			def apply(first :S, second :S, rest :S*) :UpdateDML[Unit, M, Seq[Int]] =
				apply(first +: second +: rest)

			def apply(entities :Seq[S]) :UpdateDML[Unit, M, Seq[Int]] =
				new UpdateOne[S, M](table).batch.bind(entities)

			def *(max :Int) :EntityMultiUpdateFactory[S, M] =
				new EntityMultiUpdateFactory[S, M](domain, whereDomain, table, max)

			def apply[X :SQLForm] :UpdateFactory[X, S, M] = by[X]

			def by[X :SQLForm] :UpdateFactory[X, S, M] = {
				val domain = this.domain.left.param[X]
				new UpdateFactory[X, S, M](domain, domain, table)
			}
		}




		sealed trait UpdateUpdatingOne[S, M[O] <: BaseMapping[S, O]]
			extends SupplantClauseFactory[S, M, From[M] WithParam S, UpdateWhereSeed[S, M, S]]
			   with UpdateWhereSeed[S, M, S] with UpdateReturningEntity[S, S, M]

		private[Update] class DefaultUpdateUpdatingOne[S, M[O] <: BaseMapping[S, O]] private[Update]
		                                              (override val statement :UpdateOne[S, M],
		                                               override val keys :Seq[TypedColumn[_, From[M]]],
		                                               override val columnNames :Seq[String])
			extends DefaultUpdateReturningEntity[S, S, M](
				statement, keys, columnNames,
				UpdatedEntities.Single(statement.table.export[Unit].asInstanceOf[TypedMapping[S, _]], columnNames)
			) with UpdateUpdatingOne[S, M]
		{
			def this(statement :UpdateOne[S, M]) =
				this(statement, statement.table.export[From[M]].autoUpdated.toSeq :Seq[TypedColumn[_, From[M]]],
					statement.table.export[From[M]].autoUpdated.view.map(_.name).to(Seq))

			protected override def setAll(setters :Seq[From[M] := (From[M] WithParam S)]) :UpdateWhereSeed[S, M, S] = {
				val update = new DefaultEntityUpdate[S, M](setDomain, whereDomain, table, setters, condition)
				new DefaultUpdateUpdatingEntity[S, M](update, keys, columnNames, result)
			}
			protected override def supplantAll(setters :Seq[From[M] := (From[M] WithParam S)]) :UpdateWhereSeed[S, M, S] =
				new DefaultUpdateReturningEntityWhereSeed[S, M](
					statement `->supplantAll` setters, keys, columnNames, result
				)

			protected override def where(condition :SingleBoolean[From[M] WithParam S]) :EntityUpdateWhereAll[S, M, S] = {
				val update = new DefaultEntityUpdate[S, M](setDomain, whereDomain, table, setters, condition)
				new DefaultUpdateUpdatingEntity[S, M](update, keys, columnNames, result)
			}

			protected override def setDomain   :From[M] WithParam S                   = statement.`->setDomain`
			protected override def whereDomain :From[M] WithParam S                   = statement.`->whereDomain`
			private def setters                :Seq[From[M] := (From[M] WithParam S)] = statement.setters
			private def condition              :SingleBoolean[From[M] WithParam S]    = statement.condition
		}




		final class UpdateMany[S, M[O] <: BaseMapping[S, O]] private[Update] (override val table :RelVar[M])
			extends BatchEntityUpdate[S, M, Int] with RepeatedDML[S, Int]
			   with GenericSupplantedUpdateFactory[S, M, UpdateManySet[S, M]]
			   with GenericUpdateFactory[S, M, UpdateManySet[S, M]]
			   with WhereClauseFactory[S, M, UpdateManyWhereAll[S, M]]
		{
			protected override def supplantAll(setters :Seq[From[M] := (From[M] WithParam S)]) :UpdateManySet[S, M] =
				new UpdateManySet[S, M](one `->supplantAll` setters)

			protected override def supplant(setter :From[M] := (From[M] WithParam S)) :UpdateManySet[S, M] =
				new UpdateManySet[S, M](one `->supplant` setter)

			protected override def setAll(setters :Seq[From[M] := (From[M] WithParam S)]) :UpdateManySet[S, M] =
				new UpdateManySet[S, M](one `->setAll` setters)

			protected override def set(setter :From[M] := (From[M] WithParam S)) :UpdateManySet[S, M] =
				new UpdateManySet[S, M](one `->set` setter)

			protected override def where(condition :SingleBoolean[From[M] WithParam S]) :UpdateManyWhereAll[S, M] =
				new UpdateManyWhereAll[S, M](one `->where` condition)

			private[this] val one = new UpdateOne[S, M](table)
			override def dml                   :EntityUpdate[S, S, M, Int] = one
			protected override def setDomain   :From[M] WithParam S        = one.`->setDomain`
			protected override def whereDomain :From[M] WithParam S        = one.`->whereDomain`
		}


		final class UpdateManySet[S, M[O] <: BaseMapping[S, O]] private[Update]
		                         (override val dml :EntityUpdateWhereSeed[S, M, Int])
			extends BatchUpdate[S, M, Int] with RepeatedDML[S, Int]
			   with GenericUpdateFactory[S, M, UpdateManySet[S, M]]
			   with WhereClauseFactory[S, M, UpdateManyWhereAll[S, M]]
		{
			protected override def setAll(setters :Seq[From[M] := (From[M] WithParam S)]) :UpdateManySet[S, M] =
				new UpdateManySet[S, M](dml.`->setAll`(setters))

			protected override def set(setter :From[M] := (From[M] WithParam S)) :UpdateManySet[S, M] =
				new UpdateManySet[S, M](dml.`->set`(setter))

			protected override def where(condition :SingleBoolean[From[M] WithParam S]) :UpdateManyWhereAll[S, M] =
				new UpdateManyWhereAll[S, M](dml.`->where`(condition))

			override val table :RelVar[M] = dml.table
			protected override def whereDomain :From[M] WithParam S = dml.`->whereDomain`
			protected override def setDomain :From[M] WithParam S = dml.`->setDomain`
		}


		sealed class UpdateManyWhere[S, M[O] <: BaseMapping[S, O]] private[Update]
		                            (override val dml :EntityUpdateWhere[S, M, Int])
			extends BatchEntityUpdate[S, M, Int] with RepeatedDML[S, Int]
			   with WhereAnyClauseFactory[S, M, UpdateManyWhere[S, M]]
		{
			override val table :RelVar[M] = dml.table
			protected override def whereDomain :From[M] WithParam S = dml.`->whereDomain`

			protected override def or(condition :SingleBoolean[From[M] WithParam S]) :UpdateManyWhere[S, M] =
				new UpdateManyWhere[S, M](dml `->or` condition)
		}


		final class UpdateManyWhereAll[S, M[O] <: BaseMapping[S, O]] private[Update]
		                              (override val dml :EntityUpdateWhereAll[S, M, Int])
			extends UpdateManyWhere[S, M](dml)
			   with WhereAllClauseFactory[S, M, UpdateManyWhere[S, M], UpdateManyWhereAll[S, M]]
		{
			protected override def and(condition :SingleBoolean[From[M] WithParam S]) :UpdateManyWhereAll[S, M] =
				new UpdateManyWhereAll[S, M](dml `->and` condition)
		}




		sealed trait GroundUpdateSeed[M[O] <: MappingAt[O], +Res]
			extends GenericGroundUpdateFactory[M, GroundUpdateSeed[M, Res]]
			   with Update[Unit, M, Res]

		sealed trait GroundRowUpdateSeed[M[O] <: MappingAt[O], +Res]
			extends GenericGroundUpdateFactory[M, GroundRowUpdateSeed[M, Res]]
			   with GroundUpdateSeed[M, Res] with RowUpdate[Unit, M, Res]

		sealed trait GroundEntityUpdateSeed[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericGroundUpdateFactory[M, GroundEntityUpdateSeed[S, M, Res]]
			   with GroundRowUpdateSeed[M, Res] with EntityUpdate[Unit, S, M, Res]

		sealed trait GroundRowsUpdateSeed[M[O] <: MappingAt[O], +Res]
			extends GenericGroundUpdateFactory[M, GroundRowsUpdateSeed[M, Res]]
			   with GroundUpdateSeed[M, Res] with RowsUpdate[Unit, M, Res]

		sealed trait GroundEntitiesUpdateSeed[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericGroundUpdateFactory[M, GroundEntitiesUpdateSeed[S, M, Res]]
			   with GroundRowsUpdateSeed[M, Res] with EntitiesUpdate[Unit, S, M, Res]

		sealed trait GroundUpdateWhereSeed[M[O] <: MappingAt[O], +Res]
			extends GenericGroundUpdateFactory[M, GroundUpdateWhereSeed[M, Res]]
			   with GroundWhereClauseFactory[M, Update[Unit, M, Res]]
			   with GroundUpdateSeed[M, Res]

		sealed trait GroundRowUpdateWhereSeed[M[O] <: MappingAt[O], +Res]
			extends GenericGroundUpdateFactory[M, GroundRowUpdateWhereSeed[M, Res]]
			   with GroundWhereClauseFactory[M, RowUpdate[Unit, M, Res]]
			   with GroundUpdateWhereSeed[M, Res] with GroundRowUpdateSeed[M, Res]

		sealed trait GroundEntityUpdateWhereSeed[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericGroundUpdateFactory[M, GroundEntityUpdateWhereSeed[S, M, Res]]
			   with GroundWhereClauseFactory[M, EntityUpdate[Unit, S, M, Res]]
			   with GroundRowUpdateWhereSeed[M, Res] with GroundEntityUpdateSeed[S, M, Res]

		sealed trait GroundRowsUpdateWhereSeed[M[O] <: MappingAt[O], +Res]
			extends GenericGroundUpdateFactory[M, GroundRowsUpdateWhereSeed[M, Res]]
			   with GroundWhereClauseFactory[M, RowsUpdate[Unit, M, Res]]
			   with GroundUpdateWhereSeed[M, Res] with GroundRowsUpdateSeed[M, Res]

		sealed trait GroundEntitiesUpdateWhereSeed[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericGroundUpdateFactory[M, GroundEntitiesUpdateWhereSeed[S, M, Res]]
			   with GroundWhereClauseFactory[M, EntitiesUpdate[Unit, S, M, Res]]
			   with GroundRowsUpdateWhereSeed[M, Res] with GroundEntitiesUpdateSeed[S, M, Res]


		type GenericGroundUpdateFactory[M[O] <: MappingAt[O], +U] = GroundSetClauseFactory[M, From[M], U]

		type GenericGroundSupplantedUpdateFactory[M[O] <: MappingAt[O], +Res] =
			GroundSupplantClauseFactory[M, From[M], Res]


		final class GroundUpdateFactory[S, M[O] <: BaseMapping[S, O]] private[Update]
		                               (protected override val domain :From[M])
			extends GroundSetClauseFactory[M, From[M], GroundUpdateSetClause[S, M]]
		{
			@inline protected override def setDomain :From[M] = domain
			@inline protected override def table :RelVar[M] = domain.table.castFrom[Table[M], RelVar[M]]

			override def setAll(setters :Seq[From[M] := From[M]]) :GroundUpdateSetClause[S, M] =
				new GroundUpdateSetClause[S, M](domain, table, setters)
		}


		final class GroundUpdateSetClause[S, M[O] <: BaseMapping[S, O]] private[Update]
		                                 (protected override val domain :From[M],
		                                  protected override val table :RelVar[M],
		                                  protected val setters :Seq[From[M] := From[M]])
			extends GroundSetClauseFactory[M, From[M], GroundUpdateSetClause[S, M]]
			   with GroundWhereClauseFactory[M, RowUpdate[Unit, M, Int]]
		{
			protected override def setDomain :From[M] = domain

			override def setAll(setters :Seq[From[M] := From[M]]) :GroundUpdateSetClause[S, M] =
				new GroundUpdateSetClause[S, M](domain, table, this.setters :++ setters)

			override def set(setter :From[M] := From[M]) :GroundUpdateSetClause[S, M] =
				new GroundUpdateSetClause[S, M](domain, table, this.setters :+ setter)

			override def where(condition :SingleBoolean[From[M]]) :RowUpdate[Unit, M, Int] =
				new DefaultGroundUpdate[S, M](domain, table, setters, condition)
		}


		final class GroundUpdateAllFactory[S, M[O] <: BaseMapping[S, O]] private[Update]
		                                  (protected override val domain :From[M])
			extends AnyVal with GroundSetClauseFactory[M, From[M], GroundRowUpdateSeed[M, Int]]
		{
			protected final override def table     :RelVar[M] = domain.table.asInstanceOf[RelVar[M]]
			protected final override def setDomain :From[M]   = domain

			override def setAll(setters :Seq[From[M] := From[M]]) :GroundRowUpdateSeed[M, Int] =
				new DefaultGroundUpdate[S, M](domain, table, setters, True)
			//todo: override the rest of the methods to avoid boxing
			//uncomment in Scala 3 and add docs (in intermediate implicit parameter list in method creating this object)
//			def apply(entities :S*) :UpdateDML[(), M, Seq[Int]] = GroundUpdateMany[S, M](table, entities)
		}




		class GroundUpdateOneFactory[S] private[Update] (private val entity :S) extends AnyVal {
			def in[M[O] <: BaseMapping[S, O]](table :RelVar[M]) :GroundUpdateOne[S, M] =
				new GroundUpdateOne(table, entity)
		}


		final class GroundUpdateOne[S, M[O] <: BaseMapping[S, O]] private[Update]
		                           (protected override val domain :From[M], override val table :RelVar[M],
		                            override val value :S)
			extends GroundUpdate[M] with GroundEntityUpdate[S, M, Int] with GroundDML.Impl[Int]
			   with GroundSetClauseFactory[M, From[M], GroundEntityUpdateSeed[S, M, Int]]
			   with GenericGroundSupplantedUpdateFactory[M, GroundEntityUpdateSeed[S, M, Int]]
			   with GroundWhereClauseFactory[M, EntityUpdate[Unit, S, M, Int]]
		{
			private[Update] def this(table :RelVar[M], value :S) = this(From(table), table, value)

			override def setAll(setters :Seq[From[M] := From[M]]) :GroundEntityUpdateSeed[S, M, Int] =
				new DefaultGroundEntityUpdate[S, M](domain, table, value, setters, condition)

			override def supplantAll(setters :Seq[From[M] := From[M]]) :GroundEntityUpdateSeed[S, M, Int] =
				new GroundSupplantedUpdateOne[S, M](domain, table, value, setters, condition)

			override def where(condition :SingleBoolean[From[M]]) :EntityUpdate[Unit, S, M, Int] =
				new DefaultGroundEntityUpdate(domain, table, value, setters, condition)


			protected override def setDomain :From[M] = domain

			override lazy val setters :Seq[From[M] := From[M]] = {
				implicit val updateForm = table[From[M]].selectForm <> table[From[M]].updateForm
				PassedArray :+ (domain.last := BoundParam(value))
			}

			override lazy val condition :SingleBoolean[From[M]] = {
				implicit val filterForm = table[From[M]].selectForm <> table[From[M]].filterForm
				domain.last === BoundParam(value)
			}


			override def equals(that :Any) :Boolean = that match {
				case other :AnyRef if other eq this => true
				case other :GroundUpdateOne[_, MappingAt @unchecked] =>
					table == other.table && value == other.value
				case other :GroundUpdate[_] if other canEqual this =>
					hashCode == other.hashCode && table == other.table &&
						condition == other.condition && setters == other.setters
				case _ => false
			}
			protected override def initToString :String = "Update(" + value + ") in " + table
		}


		private class GroundSupplantedUpdateOne[S, M[O] <: BaseMapping[S, O]] private[Update]
		                                       (override val domain :From[M], override val table :RelVar[M],
		                                        override val value :S, val overrides :Seq[From[M] := From[M]],
		                                        override val condition :SingleBoolean[From[M]])
			extends GroundUpdate[M] with GroundEntityUpdateWhereSeed[S, M, Int] with GroundEntityUpdate[S, M, Int]
			   with GroundDML.Impl[Int]
		{
			override def setAll(updates :Seq[From[M] := From[M]]) =
				new GroundSupplantedUpdateOne(domain, table, value, overrides :++ updates, condition)

			override def set(update :From[M] := From[M]) =
				new GroundSupplantedUpdateOne(domain, table, value, overrides :+ update, condition)

			override def where(condition :SingleBoolean[From[M]]) :EntityUpdate[Unit, S, M, Int] =
				new DefaultGroundEntityUpdate[S, M](domain, table, value, setters, condition)

			protected override def setDomain :From[M] = domain
			override lazy val setters :Seq[From[M] := From[M]] =
			//todo: delay the split into columns until defaultSpelling to use spelling.setterReform
			TableStatement.supplant[S, M, From[M]](
					UpdateView, table, setDomain, ColumnSetter.updates(table, value), overrides
				)
		}




		class GroundEntityMultiUpdateFactory[S] private[Update](private val entities :Seq[S]) extends AnyVal {
			def in[M[O] <: BaseMapping[S, O]](table :RelVar[M]) :GroundEntityMultiUpdateSetClause[S, M] =
				new GroundEntityMultiUpdateSetClause[S, M](From(table), table, entities)
		}

		final class GroundEntityMultiUpdateSetClause[S, M[O] <: BaseMapping[S, O]] private[Update]
		                                            (protected override val domain :From[M],
		                                             protected override val table :RelVar[M],
		                                             protected val values :Seq[S])
			extends GenericGroundUpdateFactory[M, GroundEntitiesUpdateSeed[S, M, Int]]
			   with GroundSupplantClauseFactory[M, From[M], GroundEntitiesUpdateSeed[S, M, Int]]
		{
			override def setAll(setters :Seq[From[M] := From[M]]) :GroundEntitiesUpdateSeed[S, M, Int] =
				new DefaultGroundEntitiesUpdate(domain, table, values, setters, condition)

			override def supplantAll(setters :Seq[From[M] := From[M]]) :GroundEntitiesUpdateSeed[S, M, Int] =
				new GroundSupplantedMultiUpdate(domain, table, values, setters, condition)

			protected override def setDomain :From[M] = domain
			private def condition = OrSQL(values.map(TableStatement.whereEntity(domain, _)) :_*)
		}


		private class GroundSupplantedMultiUpdate[S, M[O] <: BaseMapping[S, O]] private[Update]
		              (protected override val domain :From[M], override val table :RelVar[M],
		               override val values :Seq[S], overrides :Seq[From[M] := From[M]],
		               override val condition :SingleBoolean[From[M]])
			extends GroundUpdate[M] with GroundEntitiesUpdateSeed[S, M, Int] with GroundEntitiesUpdate[S, M, Int]
			   with GroundDML.Impl[Int]
		{
			override def setAll(setters :Seq[From[M] := From[M]]) =
				new GroundSupplantedMultiUpdate[S, M](domain, table, values, overrides :++ setters, condition)

			override def set(setter :From[M] := From[M]) =
				new GroundSupplantedMultiUpdate[S, M](domain, table, values, overrides :+ setter, condition)

			protected override def setDomain :From[M] = domain

			override lazy val setters :Seq[From[M] := From[M]] =
				values flatMap { value =>
					//todo: delay the split into columns until defaultSpelling to use spelling.setterReform
					TableStatement.supplant[S, M, From[M]](
						UpdateView, table, setDomain, ColumnSetter.updates(table, value), overrides
					)
				}
		}

	}







	object implementation {
		trait UpdatesReturning[-Args, M[O] <: MappingAt[O], +Res]
			extends UpdateDML[Args, M, Res] with StatementsReturning[Args, M, Res]

		trait UpdateReturning[-Args, M[O] <: MappingAt[O], +Res]
			extends Update[Args, M, Res] with Returning[Args, M, Res] with UpdatesReturning[Args, M, Res]


		trait ParamUpdate[Arg, M[O] <: MappingAt[O]] extends Update[Arg, M, Int] { outer =>
			protected type Domain = From[M] WithParam Arg
			protected def domain :From[M] WithParam Arg
			def setters :Seq[From[M] := (From[M] WithParam Arg)]
			val condition :SingleBoolean[From[M] WithParam Arg]
			override def result :StatementResult[Nothing, Int] = UpdateCount

//			override def bind(args :Args) :Update[(), M, Int] =
//				new BoundStatement[Args, Int](this, args) with Update[Any, M, Int] with GroundUpdate[M] {
//					override val table = outer.table
//					override val setters = Update.bind(outer.domain, outer.setters, args)
//					override val condition = outer.condition.bind(outer.domain, @~ ~ args)
//				}
			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Arg, Int] =
				visitor.paramUpdate(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Arg] = {
				val ctx = spelling.newContext.join("").param("") //the only param, the alias will be unique
				implementation.spell[M, Domain, @~ ~ Arg](this)(setters, condition)(
					domain.left, domain, ctx, domain.parameterization
				).compose { @~ ~ _ }
			}

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamUpdate[_, MappingAt @unchecked]]
			//fixme: unbound parameter expressions do not equal other expressions for the same parameter type
			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :ParamUpdate[_, MappingAt @unchecked] if (other canEqual this) && canEqual(other) =>
					hashCode == other.hashCode && table == other.table &&
						condition == other.condition && setters == other.setters
				case _ => false
			}
			protected override def initHashCode :Int = (table.hashCode * 31 + condition.hashCode) * 31 + setters.hashCode

			protected override def initToString :String =
				setters.mkString("Update " + table + " set ", ", ", " where " + condition)
		}


		private[Update] final class DefaultUpdate[Arg, S, M[O] <: BaseMapping[S, O]]
		                            (protected override val domain :From[M] WithParam Arg,
		                             protected override val whereDomain :From[M] WithParam Arg,
		                             override val table :RelVar[M],
		                             override val setters :Seq[From[M] := (From[M] WithParam Arg)],
		                             override val condition :SingleBoolean[From[M] WithParam Arg])
			extends ParamUpdate[Arg, M] with RowUpdateWhereSeed[Arg, M, Int] with RowUpdateWhereAll[Arg, M, Int]
		{
			override def setDomain :From[M] WithParam Arg = domain

			protected override def setAll(updates :Seq[From[M] := (From[M] WithParam Arg)]) =
				new DefaultUpdate[Arg, S, M](domain, whereDomain, table, setters :++ updates, condition)

			protected override def set(update :From[M] := (From[M] WithParam Arg)) =
				new DefaultUpdate[Arg, S, M](domain, whereDomain, table, setters :+ update, condition)

			protected override def where(condition :SingleBoolean[From[M] WithParam Arg]) =
				new DefaultUpdate[Arg, S, M](domain, whereDomain, table, setters, condition)

			protected override def or(condition :SingleBoolean[From[M] WithParam Arg]) =
				new DefaultUpdate[Arg, S, M](domain, whereDomain, table, setters, this.condition || condition)

			protected override def and(condition :SingleBoolean[From[M] WithParam Arg]) =
				new DefaultUpdate[Arg, S, M](domain, whereDomain, table, setters, this.condition && condition)
		}

		private[Update] final class DefaultEntityUpdate[S, M[O] <: BaseMapping[S, O]]
		                            (protected override val setDomain   :From[M] WithParam S,
		                             protected override val whereDomain :From[M] WithParam S,
		                             override val table                 :RelVar[M],
		                             override val setters               :Seq[From[M] := (From[M] WithParam S)],
		                             override val condition             :SingleBoolean[From[M] WithParam S])
			extends ParamUpdate[S, M] with EntityUpdateWhereSeed[S, M, Int] with EntityUpdateWhereAll[S, M, Int]
			   with GenericUpdateFactory[S, M, DefaultEntityUpdate[S, M]]
			   with WhereClauseFactory[S, M, DefaultEntityUpdate[S, M]]
			   with WhereAllClauseFactory[S, M, DefaultEntityUpdate[S, M], DefaultEntityUpdate[S, M]]
		{
			override def setAll(updates :Seq[From[M] := (From[M] WithParam S)]) =
				new DefaultEntityUpdate[S, M](domain, whereDomain, table, setters :++ updates, condition)

			override def set(update :From[M] := (From[M] WithParam S)) =
				new DefaultEntityUpdate[S, M](domain, whereDomain, table, setters :+ update, condition)

			override def where(condition :SingleBoolean[From[M] WithParam S]) =
				new DefaultEntityUpdate[S, M](domain, whereDomain, table, setters, condition)

			override def or(condition :SingleBoolean[From[M] WithParam S]) =
				new DefaultEntityUpdate[S, M](domain, whereDomain, table, setters, this.condition || condition)

			override def and(condition :SingleBoolean[From[M] WithParam S]) =
				new DefaultEntityUpdate[S, M](domain, whereDomain, table, setters, this.condition && condition)

			override def domain :From[M] WithParam S = setDomain
		}


		private[Update] final class DefaultUpdateUpdatingEntity[S, M[O] <: BaseMapping[S, O]]
		                            (override val statement :DefaultEntityUpdate[S, M],
		                             override val keys :Seq[TypedMapping[_, From[M]]],
		                             override val columnNames :Seq[String],
		                             override val result :StatementResult[S, S])
			extends DefaultUpdateReturningEntity[S, S, M](statement, keys, columnNames, result)
			   with EntityUpdateWhereSeed[S, M, S] with EntityUpdateWhereAll[S, M, S]
			   with WhereAllClauseFactory[S, M, DefaultUpdateUpdatingEntity[S, M], DefaultUpdateUpdatingEntity[S, M]]
		{
			def this(statement :DefaultEntityUpdate[S, M], keys :Seq[TypedMapping[_, From[M]]], columns :Seq[String]) =
				this(statement, keys, columns, {
					val mapping = statement.table.export[Unit].asInstanceOf[TypedMapping[S, Unit]]
					UpdatedEntities.Single(mapping, columns)
				})

			def this(statement :DefaultEntityUpdate[S, M]) =
				this(statement, statement.table.export[From[M]].autoUpdated.toSeq,
					statement.table.export[Unit].autoUpdated.view.map(_.name).to(Seq))

			protected override  def setAll(setters :Seq[From[M] := (From[M] WithParam S)]) =
				new DefaultUpdateUpdatingEntity[S, M](statement `->setAll` setters, keys, columnNames, result)

			protected override def where(condition :SingleBoolean[From[M] WithParam S]) =
				new DefaultUpdateUpdatingEntity[S, M](statement `->where` condition, keys, columnNames, result)

			protected override def and(condition :SingleBoolean[From[M] WithParam S]) =
				new DefaultUpdateUpdatingEntity[S, M](statement `->and` condition, keys, columnNames, result)

			protected override def or(condition :SingleBoolean[From[M] WithParam S]) =
				new DefaultUpdateUpdatingEntity[S, M](statement `->or` condition, keys, columnNames, result)

			protected override def setDomain   = statement.`->setDomain`
			protected override def whereDomain = statement.`->whereDomain`
		}

		private[Update] class DefaultUpdateReturningEntityWhereSeed[S, M[O] <: BaseMapping[S, O]]
		                      (override val statement :EntityUpdateWhereSeed[S, M, Any],
		                       override val keys :Seq[TypedMapping[_, From[M]]],
		                       override val columnNames :Seq[String],
		                       override val result :StatementResult[S, S])
			extends DefaultUpdateReturningEntity[S, S, M](statement, keys, columnNames, result)
			   with EntityUpdateWhereSeed[S, M, S]
		{
			protected override def setAll(setters :Seq[From[M] := (From[M] WithParam S)]) :EntityUpdateWhereSeed[S, M, S] =
				new DefaultUpdateReturningEntityWhereSeed(statement `->setAll` setters, keys, columnNames, result)

			protected override def set(setter :From[M] := (From[M] WithParam S)) =
				new DefaultUpdateReturningEntityWhereSeed(statement `->set` setter, keys, columnNames, result)

			protected override def where(condition :SingleBoolean[WithParam[From[M], S]]) :EntityUpdateWhereAll[S, M, S] =
				new DefaultUpdateReturningEntityWhereAll(statement `->where` condition, keys, columnNames, result)

			protected override def whereDomain :From[M] WithParam S = statement.`->whereDomain`
			protected override def setDomain   :From[M] WithParam S = statement.`->setDomain`
		}

		private[Update] class DefaultUpdateReturningEntityWhere[S, M[O] <: BaseMapping[S, O]]
		                      (override val statement :EntityUpdateWhere[S, M, Any],
		                       override val keys :Seq[TypedMapping[_, From[M]]],
		                       override val columnNames :Seq[String],
		                       override val result :StatementResult[S, S])
			extends DefaultUpdateReturningEntity[S, S, M](statement, keys, columnNames, result)
			   with EntityUpdateWhere[S, M, S]
		{
			protected override def or(condition :SingleBoolean[From[M] WithParam S]) =
				new DefaultUpdateReturningEntityWhere(statement `->or` condition, keys, columnNames, result)

			protected override def whereDomain :From[M] WithParam S = statement.`->whereDomain`
		}

		private[Update] class DefaultUpdateReturningEntityWhereAll[S, M[O] <: BaseMapping[S, O]]
		                      (override val statement :EntityUpdateWhereAll[S, M, Any],
		                       override val keys :Seq[TypedMapping[_, From[M]]],
		                       override val columnNames :Seq[String],
		                       override val result :StatementResult[S, S])
			extends DefaultUpdateReturningEntityWhere[S, M](statement, keys, columnNames, result)
			   with EntityUpdateWhereAll[S, M, S]
		{
			protected override def and(condition :SingleBoolean[From[M] WithParam S]) =
				new DefaultUpdateReturningEntityWhereAll(statement `->and` condition, keys, columnNames, result)
		}




		trait GroundUpdate[M[O] <: MappingAt[O]] extends Update[Any, M, Int] {
			protected type Domain = From[M]
			protected val domain :Domain
			def setters :Seq[From[M] := From[M]]
			val condition :SingleBoolean[From[M]]
			override def result :StatementResult[Nothing, Int] = UpdateCount

			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Any, Int] =
				visitor.groundUpdate(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Any] =
				implementation.spell[M, From[M], @~](this)(setters, condition)(
					domain, domain, spelling.newContext.join(""), Parameterization.paramless
				).compose { _ => @~ }

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroundUpdate[MappingAt @unchecked]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GroundUpdate[MappingAt @unchecked] if (other canEqual this) && canEqual(other) =>
					hashCode == other.hashCode && table == other.table &&
						condition == other.condition && setters == other.setters
				case _ => false
			}
			protected override def initHashCode :Int =
				(table.hashCode * 31 + condition.hashCode) * 31 + setters.hashCode

			protected override def initToString :String =
				setters.mkString("Update " + table + " set ", ", ", " where " + condition)
		}


		private[Update] class DefaultGroundUpdate[S, M[O] <: BaseMapping[S, O]]
		                                         (override val domain :From[M], override val table :RelVar[M],
		                                          override val setters :Seq[From[M] := From[M]],
		                                          override val condition :SingleBoolean[From[M]])
			extends GroundUpdate[M] with RowUpdate[Any, M, Int]
			   with GroundDML.Impl[Int] with GroundRowUpdateWhereSeed[M, Int]
		{
			override def setAll(updates :Seq[From[M] := From[M]]) :GroundRowUpdateWhereSeed[M, Int] =
				new DefaultGroundUpdate[S, M](domain, table, updates, condition)

			override def set(update :From[M] := From[M]) :GroundRowUpdateWhereSeed[M, Int] =
				new DefaultGroundUpdate[S, M](domain, table, setters :+ update, condition)

			override def where(condition :SingleBoolean[From[M]]) :RowUpdate[Unit, M, Int] =
				new DefaultGroundUpdate[S, M](domain, table, setters, condition)

			protected override def setDomain :From[M] = domain
		}

		private[Update] class DefaultGroundEntityUpdate[S, M[O] <: BaseMapping[S, O]]
		                                               (override val domain :From[M], override val table :RelVar[M],
		                                                override val value :S,
		                                                override val setters :Seq[From[M] := From[M]],
		                                                override val condition :SingleBoolean[From[M]])
			extends GroundUpdate[M] with GroundEntityUpdate[S, M, Int]
			   with GroundDML.Impl[Int] with GroundEntityUpdateWhereSeed[S, M, Int]
		{
			override def setAll(updates :Seq[From[M] := From[M]]) :GroundEntityUpdateWhereSeed[S, M, Int] =
				new DefaultGroundEntityUpdate[S, M](domain, table, value, updates, condition)

			override def set(update :From[M] := From[M]) :GroundEntityUpdateWhereSeed[S, M, Int] =
				new DefaultGroundEntityUpdate[S, M](domain, table, value, setters :+ update, condition)

			override def where(condition :SingleBoolean[From[M]]) :EntityUpdate[Unit, S, M, Int] =
				new DefaultGroundEntityUpdate[S, M](domain, table, value, setters, condition)

			protected override def setDomain :From[M] = domain
		}


		private[Update] class DefaultGroundEntitiesUpdate[S, M[O] <: BaseMapping[S, O]]
		                                                 (override val domain :From[M], override val table :RelVar[M],
		                                                  override val values :Seq[S],
		                                                  override val setters :Seq[From[M] := From[M]],
		                                                  override val condition :SingleBoolean[From[M]])
			extends GroundUpdate[M] with GroundEntitiesUpdateWhereSeed[S, M, Int] with GroundEntitiesUpdate[S, M, Int]
			   with GroundDML.Impl[Int]
		{
			override def setAll(updates :Seq[From[M] := From[M]]) :GroundEntitiesUpdateWhereSeed[S, M, Int] =
				new DefaultGroundEntitiesUpdate[S, M](domain, table, values, updates, condition)

			override def set(update :From[M] := From[M]) :GroundEntitiesUpdateWhereSeed[S, M, Int] =
				new DefaultGroundEntitiesUpdate[S, M](domain, table, values, setters :+ update, condition)

			override def where(condition :SingleBoolean[From[M]]) :EntitiesUpdate[Unit, S, M, Int] =
				new DefaultGroundEntitiesUpdate[S, M](domain, table, values, setters, condition)

			protected override def setDomain :From[M] = domain
		}




		/** Fragment of the [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] visitor
		  * covering the cases of all ''update'' statements. This includes empty methods for every existing type
		  * in the hierarchy, abstract or concrete.
		  * Note that instances of standard adapter classes,
		  * such as [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement BoundStatement]], which extend also
		  * [[net.noresttherein.oldsql.sql.Update Update]], will not be handled by any of these methods, but rather
		  * into the more generic ones declared by `StatementVisitor` itself to better reflect their nature -
		  * in their case, conforming to `Update` serves only a declarative purpose.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  * @see [[net.noresttherein.oldsql.sql.Update.implementation.MatchUpdate]]
		  * @see [[net.noresttherein.oldsql.sql.Update.implementation.CaseUpdate]]
		  */
		trait UpdateVisitor[R[-X, +Y]] {
			def paramUpdate[X, M[O] <: MappingAt[O]](stmt :ParamUpdate[X, M])            :R[X, Int]
			def groundUpdate[M[O] <: MappingAt[O]](stmt :GroundUpdate[M])                :R[Any, Int]
			def update[X, M[O] <: MappingAt[O], Y](stmt :Update[X, M, Y])                :R[X, Y]
		}

		type MatchUpdate[R[-X, +Y]] = UpdateVisitor[R]
//		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitors''
//		  * of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It implements all methods
//		  * of [[net.noresttherein.oldsql.sql.Update.implementation.UpdateVisitor UpdateVisitor]] for concrete
//		  * [[net.noresttherein.oldsql.sql.Update Update]] implementations by delegating them to the methods for their
//		  * base traits, leaving unimplemented only the cases for
//		  * [[net.noresttherein.oldsql.sql.Update.implementation.GroundUpdate GroundUpdate]],
//		  * [[net.noresttherein.oldsql.sql.Update.implementation.ParamUpdate ParamUpdate]] and `Delete` itself (for custom extensions
//		  * not derived from any of the existing implementations).
//		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
//		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
//		  *           and its return type (the `Res` argument of the visited statement).
//		  * @see [[net.noresttherein.oldsql.sql.Insert.CaseAnyInsert]]
//		  */
//		trait MatchAnyUpdate[R[-X, +Y]] extends UpdateVisitor[R] {
//			override def updateOne[S, M[O] <: BaseMapping[S, O]](stmt :UpdateOne[S, M])           = paramUpdate(stmt)
//			override def updateAll[X, S, M[O] <: BaseMapping[S, O]](stmt :UpdateAll[X, S, M])     = paramUpdate(stmt)
//			override def updateWhere[X, S, M[O] <: BaseMapping[S, O]](stmt :RowUpdateWhere[X, S, M]) = paramUpdate(stmt)
//
//			override def updateOne[S, M[O] <: BaseMapping[S, O]](stmt :GroundUpdateOne[S, M])     = groundUpdate(stmt)
//			override def updateAll[S, M[O] <: BaseMapping[S, O]](stmt :GroundUpdateAll[S, M])     = groundUpdate(stmt)
//			override def updateWhere[S, M[O] <: BaseMapping[S, O]](stmt :GroundUpdateWhere[S, M]) = groundUpdate(stmt)
//		}

		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitors''
		  * of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It expands on
		  * [[net.noresttherein.oldsql.sql.Update.implementation.MatchUpdate MatchAnyUpdate]] by further delegating the remaining open cases
		  * to the method for [[net.noresttherein.oldsql.sql.Update Update]] trait itself. CaseAnys for concrete subclasses
		  * dispatch still to their immediate base type, making the delegation a multi-step affair and allowing to override
		  * on the chosen level.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  */
		trait CaseUpdate[R[-X, +Y]] extends MatchUpdate[R] {
			override def paramUpdate[X, M[O] <: MappingAt[O]](stmt :ParamUpdate[X, M]) :R[X, Int]   = update(stmt)
			override def groundUpdate[M[O] <: MappingAt[O]](stmt :GroundUpdate[M])     :R[Any, Int] = update(stmt)
		}
	
	
	

		private[Update] def updateDomain[S, M[O] <: BaseMapping[S, O]](from :From[M]) :From[M] WithParam S = {
			val mapping = from.table.export.asInstanceOf[TypedMapping[S, Unit]]
			from.param[S](mapping.selectForm <> mapping.updateForm)
		}
		
		private[Update] def updateDomain[S, M[O] <: BaseMapping[S, O]](table :RelVar[M]) :From[M] WithParam S = {
			val mapping = table.export.asInstanceOf[TypedMapping[S, Unit]]
			From(table).param[S](mapping.selectForm <> mapping.updateForm)
		}

		private[Update] def bind[M[O] <: MappingAt[O], X]
		                        (base :From[M] WithParam X, setters :Seq[From[M] := (From[M] WithParam X)], arg :X)
				:Seq[From[M] := From[M]] =
			setters.map { (update :From[M] := (From[M] WithParam X)) =>
				update.lvalue := (update.rvalue.bind(base, @~ ~ arg) :SingleSQL[base.GeneralizedParamless, update.Value])
			}

	
		//consider: making the validations earlier, at class creation
		private[Update] def spell[M[O] <: MappingAt[O], R <: ParameterizedRow[Xs], Xs]
		                         (self :UpdateDML[Nothing, M, Any])
		                         (setters :Seq[From[M] := R], condition :GroupedBoolean[R])
		                         (setDomain :From[M], whereDomain :R, context :SQLContext[Xs], params :Parameterization[Xs, R])
		                         (implicit spelling :SQLSpelling) :SpelledSQL[Xs] =
		{
			//todo: use UpdatePreset, CustomUpdate, ExtraUpdate
			val spell = spelling.inUpdate
			val tableSQL = spell.table(self.table, "")(Dual, spelling.newContext, Parameterization.paramless)
			val columnSetters = setters.flatMap { update => //todo: eliminate duplicates as per supplant
				implicit val spelling = spell //shadow the argument
				val ComponentSetter(left, right) = update.reform
				val lefts = spell.explode(left, false)(setDomain, tableSQL.context, Parameterization.paramless)
				val rights = spell.explode(right, false)(whereDomain, context, params)
				if (lefts.size != rights.size)
					throw new MismatchedExpressionsException(
						s"Illegal DML '$this': cannot set component ${update.lvalue} of ${self.table} " +
						s"to ${update.rvalue} due to differing numbers of columns for both sides:\n$lefts\nvs\n$rights."
					)
				lefts.iterator zip rights.iterator map {
					case (l, r) => l +: " = " +: r
				} to List
			}
			if (columnSetters.isEmpty)
				throw new MisspelledSQLException(s"Illegal DML '$this': an empty SET clause.")
			val set = columnSetters.reduce(_ + ", " + _)
			val where = spelling.inWhere(condition)(whereDomain, set.context, params)
			spell.UPDATE_ +: tableSQL.sql +: spell._SET_ +: set +: spell._WHERE_ +: where
		}
	}
	
}




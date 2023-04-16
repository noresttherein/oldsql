package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.OperationView.InsertView
import net.noresttherein.oldsql.collection.{Chain, PassedArray}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.exceptions.{MismatchedExpressionsException, MisspelledSQLException}
import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.morsels.Extractor.Requisite
import net.noresttherein.oldsql.schema.{RelVar, SQLForm, Table}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.forms.ChainForm
import net.noresttherein.oldsql.sql.DML.{BoundDML, ComposedDML, DMLAPI, GroundDML, RepeatedDML}
import net.noresttherein.oldsql.sql.DMLStatement.{AlteredResultStatement, BoundStatement, ComposedStatement, DMLStatementAPI, StatementResult, StatementVisitor}
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult.{UpdateCount, UpdatedEntities}
import net.noresttherein.oldsql.sql.Insert.implementation.{DefaultBatchInsert, DefaultEntityInsert, DefaultGroundInsert, DefaultInsert, DefaultInsertUpdatingEntity, GroundInsert, InsertReturning, InsertsReturning, ParamInsert}
import net.noresttherein.oldsql.sql.Insert.syntax.{BatchInsertSet, DefaultInsertReturningEntity, DefaultInsertUpdatingOne, EntityInsertSet, GroundInsertFactory, GroundInsertOneFactory, GroundInsertSelect, GroundInsertSetRow, GroundMultiInsertFactory, InsertFacade, InsertMany, InsertOne, InsertParam, InsertReturningEntities, InsertReturningEntity, InsertReturningKey, InsertReturningKeys, InsertSelect, InsertSet, InsertUpdatingOne, ParamEntityInsert, RowInsert, RowInsertSet}
import net.noresttherein.oldsql.sql.ParamClause.{NamedParamRelation, ParamRelation, UnboundParam}
import net.noresttherein.oldsql.sql.Returning.implementation.{AbstractReturningEntities, GenericBatchReturningEntities, ReturningProperTuple, ReturningTupleSeqTemplate, ReturningTupleSingleton, ReturningTuplesTemplate}
import net.noresttherein.oldsql.sql.Returning.syntax.{BatchReturningEntitiesClause, BatchReturningTuplesClause, EntitiesBatch, EntitiesStatement, EntityStatement, EntityStatementsTemplate, GenericReturningEntitiesClause, GroundBatchReturningEntitiesClause, GroundBatchReturningTuplesClause, GroundEntitiesBatch, GroundRowsBatch, ReturningEntitiesClause, ReturningEntitiesClauses, ReturningEntityClause, ReturningTupleClause, ReturningTuplesClause, ReturningTuplesClauses, ReturningTuplesClausesTemplate, RowStatement, RowsBatch, RowsStatement}
import net.noresttherein.oldsql.sql.RowProduct.ParamlessRow
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.TableStatement.{GroundSetClauseFactory, GroundSupplantClauseFactory, ReturningClauseFactory, SetClauseFactory, SupplantClauseFactory, seqAt}
import net.noresttherein.oldsql.sql.ast.{BoundParam, QuerySQL}
import net.noresttherein.oldsql.sql.mechanics.{SQLScribe, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.{BaseMappingSubject, MappingSubject}
import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}

//here be implicits
import net.noresttherein.oldsql.slang._





//todo: rename to Inserts
trait InsertDML[-Args, M[O] <: MappingAt[O], +Res]
	extends TableDML[Args, M, Res] with DMLAPI[Args, Res, Insert.into[M]#DML]
{
	override def compose[X](f :X => Args) :InsertDML[X, M, Res] =
		new ComposedDML.Base[X, Args, Res, Insert.into[M]#DML](this, f)
			with InsertDML[X, M, Res] with DerivedDML[X, Res]
			with ComposedDML[X, Args, Res] with ComposedDML.Impl[X, Args, Res, Insert.into[M]#DML]

	override def bind(args :Args) :InsertDML[Unit, M, Res] =
		new BoundDML.Base[Args, Res, Insert.into[M]#DML](this, args)
			with InsertDML[Any, M, Res] with DerivedDML[Any, Res]
			with BoundDML[Args, Res] with BoundDML.Impl[Args, Res, Insert.into[M]#DML]
}



trait Insert[-Args, M[O] <: MappingAt[O], +Res]
	extends InsertDML[Args, M, Res] with TableStatement[Args, M, Res]
		with DMLStatementAPI[Args, Res, Insert.into[M]#Stmt]
{
	protected override def returns[Y](result :StatementResult[Nothing, Y]) :Insert[Args, M, Y] =
		new AlteredResultStatement.Base[Args, Y, Insert.into[M]#Stmt](this, result)
			with Insert[Args, M, Y] with DerivedDML[Args, Y]
			with AlteredResultStatement[Args, Y] with AlteredResultStatement.Impl[Args, Y, Insert.into[M]#Stmt]

	override def compose[X](f :X => Args) :Insert[X, M, Res] =
		new ComposedDML.Base[X, Args, Res, Insert.into[M]#Stmt](this, f)
			with Insert[X, M, Res] with DerivedDML[X, Res]
			with ComposedStatement[X, Args, Res] with ComposedStatement.Impl[X, Args, Res, Insert.into[M]#Stmt]

	override def bind(args :Args) :Insert[Unit, M, Res] =
		new BoundDML.Base[Args, Res, Insert.into[M]#Stmt](this, args)
			with Insert[Any, M, Res] with DerivedDML[Any, Res]
			with BoundStatement[Args, Res] with BoundStatement.Impl[Args, Res, Insert.into[M]#Stmt]

	override def batch :InsertDML[Seq[Args], M, Seq[Res]] =
		new RepeatedDML.Base[Args, Res, Insert.into[M]#Stmt](this)
			with InsertDML[Seq[Args], M, Seq[Res]] with DerivedDML[Seq[Args], Seq[Res]] with RepeatedDML[Args, Res]

	protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] = visitor.insert(this)
}




/**
  * {{{
  * Insert into Dragons set
  *     (_.race := "Dragon".?) set (_.name := "Firkraag".?)
  *         set (_.level := 23)                                            :Insert[(), Dragons, Int]
  * Insert into Dragons set
  *     (_.race :=? "Dragon") set (_.name :=? "Firkraag") returning (_.pk) :Insert[(), Dragons, PK]
  * Insert into Dragons set Seq(
  *     _.race :=? "Dragon", _.name :=? "Firkraag", _.level :=? 23
  * )                                                                      :Insert(), Dragons, Int]
  * Insert into Dragons values (_.race) x (_.name) x (_.level)             :Insert[@~ ~String~String~Int, Dragons, Int]
  * Insert into Dragons                                                    :Insert[Dragon, Dragons, Int]
  * Insert into Dragons(_.level.-)                                         :Insert[Dragon, Dragons, Int]
  *
  * Insert(firkraag) into Dragons                                          :Insert[(), Dragons, Int]
  * Insert(firkraag) into Dragons(_.level.-)                               :Insert[(), Dragons, Int]
  * Insert(firkraag) into Dragons returning (_.pk)                         :Insert[(), Dragons, PK]
  * Insert(firkraag) into Dragons supplant
  *     (_.race :=? "Red Dragon") updating (_.pk)                          :Insert[(), Dragons, Dragon]
  * Insert(firkraag, saladrex) into Dragons                                :Insert[(), Dragons, Int]
  * Insert(firkraag, saladrex) into Dragons returning (_.pk)               :Insert[(), Dragons, Seq[PK]]
  * Insert(dragons) into Dragons                                           :Insert[(), Dragons, Int]
  * Insert(dragons) into Dragons updating (_.pk)                           :Insert[(), Dragons, Seq[Dragon]]
  *
  * Insert(Dragons) * 5                                                    :Insert[Seq[Dragon], Dragons, Int]
  * Insert(Dragons)[(String, String)] * 5 set
  *     (_.race := _(_._1)) set (_.name := _(_.name)) set (_.level := 0)   :Insert[Seq[(String, String), Dragons, Int]
  *
  * Insert(Dragons)                                                        :Insert[Dragon, Dragons, Int]
  * Insert(Dragons) returning (_.pk) x (_.createDate)                      :Insert[Dragon, Dragons, @~ ~PK~Instant]
  * Insert(Dragons) updating (_.pk)                                        :Insert[Dragon, Dragons, Dragon]
  * Insert(Dragons) set (_.name := _(_.name)) set (_.race := "Dragon")     :Insert[Dragon, Dragons, Int]
  * Insert(Dragons).set (_.race := _(_.race), _.name := _(_.name))         :Insert[Dragon, Dragons, Int]
  *
  * Insert(Dragons)[(String, String, Int)].set (
  *     _.race := _(_._1), _.name := _(_._2), _level := _(_._3)
  * )                                                                      :Insert[Dragon, Dragons, Int]
  * Insert(Dragons)[(String, String, Int)].setAll { (t, p) =>
  *     Seq(t.race := p(_._1), t.name := p(_._2), t.level := p(_._3)
  * }                                                                      :Insert[Dragon, Dragons, Int]
  * Insert(Dragons)(firkraag, saladrex)                                    :InsertDML[(), Dragons, Seq[Int]]
  * Insert(Dragons)(firkraag, saladrex) returning (_.pk)                   :InsertDML[(), Dragons, Seq[PK]]
  * Insert(Dragons)(dragons)                                               :InsertDML[(), Dragons, Seq[Int]]
  * Insert(Dragons)(dragons).updating                                       :InsertDML[(), Dragons, Seq[Dragon]]
  * Insert(Dragons)(dragons).updating(_.pk, _.createDate)                   :InsertDML[(), Dragons, Seq[Dragon]]
  *
  * Insert one Dragons                                                     :Insert[Dragon, Dragons, Int]
  * Insert one Dragons set
  *     (_.name) set (_.race := "Red " + _(_.race)) set (_.level := 1)     :Insert[Dragon, Dragons, Int] //todo: this requires BaseMapping and ComponentSetter to have a common supertype (until Scala 3)
  * Insert one Dragons supplant
  *     (_.race := "Red " + _(_.race) set (_.level := 23)                  :Insert[Dragon, Dragons, Int]
  * Insert one Dragons setAll { (t, p) =>
  *     Seq(t.name := p(_.name), t.race := p(_.race))
  * }                                                                      :Insert[Dragon, Dragons, Int]
  * Insert one Dragons setAll setters                                      :Insert[Dragon, Dragons, Int]
  *
  * Insert many Dragons                                                    :InsertDML[Seq[Dragon], Dragons, Seq[Int]]
  * Insert many Dragons ... /** any clause possible for Insert one **/     :InsertDML[Seq[Dragon], Dragons, Seq[_]]
  * Insert many Dragons values (_.race) x (_.name) x (_.level)             :InsertDML[Seq[@~ ~String~String~Int], Dragons, Seq[Int]]
  *
  * Insert returning Dragons                                               :Insert[Dragon, Dragons, Dragon]
  * Insert returning Dragons ... /**any clause possible for Insert one**/  :Insert[Dragon, Dragons, Dragon]
  *
  * Insert.using[(String, String)] into Rangers set Seq(_.name := _._1, _.familiar := _._2) :Insert[(String, String), Rangers]
  * }}}
  */ //todo: Insert into table select (...) - not worth documenting before a more flexible implementation exists
object Insert {
	type into[M[O] <: MappingAt[O]] = {
		type DML[-X, +Y] = InsertDML[X, M, Y]
		type Stmt[-X, +Y] = Insert[X, M, Y]
//		type value[X] = { type returning[Y] = Insert[X, M, Y] }
	}
	//todo: multiple parameters, named parameters
	def using[X :SQLForm] :InsertParam[X] = new InsertParam(ParamRelation[X]())

	def using[X](param :ParamRelation[X]) :InsertParam[X] = new InsertParam(param)

//	def using[N <: Label :ValueOf, X :SQLForm] = ???
//	def using[N <: Label, X](param :NamedParamRelation[N, X]) = ???

	def into[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	        (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :GroundInsertFactory[S, T] =
		new GroundInsertFactory[S, T](From(reveal(table)))

//	def all[S](entities :Seq[S]) :GroundInsertGivenFactory[S] = new GroundInsertGivenFactory(entities)

	def one[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	       (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :InsertOne[S, T] =
		new InsertOne[S, T](reveal(table))

	def many[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	        (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :InsertMany[S, T] =
		new InsertMany[S, T](one(table))

	def returning[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	             (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :InsertUpdatingOne[S, T] =
	{
		val one = this.one(table)
		val mapping = one.table.export[From[T]]
		val keys = mapping.autoInserted.to(Seq)
		val columns = keys.view.map(_.name).to(Seq)
		new DefaultInsertUpdatingOne[S, T](one, keys, columns)
	}

	def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (table :RelVar[M])(implicit reveal :MappingSubject[M, T, S]) :InsertFacade[S, T] =
		new InsertFacade[S, T](reveal(table))


	def apply[S](entity :S) :GroundInsertOneFactory[S] = new GroundInsertOneFactory(entity)

	def apply[S](first :S, second :S, rest :S*) :GroundMultiInsertFactory[S] = Insert(first +: second +: rest)

	def apply[S](entities :Seq[S]) :GroundMultiInsertFactory[S] = new GroundMultiInsertFactory[S](entities)





	/** Multiple concrete [[net.noresttherein.oldsql.sql.InsertDML InsertDML]]
	  * and [[net.noresttherein.oldsql.sql.Insert Insert]] implementations as well as base traits
	  * which do not implement any methods of those interfaces, but serve as categorization of various ways
	  * of execution of ''insert'' statements. These are public as they all add new factory methods expanding
	  * the DSL syntax for building more complex statements, in particular various variants of
	  * [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]] clause.
	  * These types are not really expected to be used directly as types of any class fields,
	  * but rather as return types of factory methods, with only their methods being invoked in a chained call.
	  * Before use, they are normally upcasted simply to `Insert` or `InsertDML`.
	  */
	object syntax {
		/** A generic base trait for `Insert`s parameterized with `Args` which insert a single row into a table.
		  * It adds [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]] clause
		  * which allows to specify a component of `M` which should be returned to the application by the DBMS driver.
		  */
		sealed trait RowInsert[-Args, M[O] <: MappingAt[O], +Res]
			extends Insert[Args, M, Res] with RowStatement[Args, M, Res]
			   with ReturningClauseFactory[M, ({ type U[X] = InsertReturningKey[Args, M, @~ ~ X, X] })#U]
		{
			override def returning[T](key :M[From[M]] => TypedMapping[T, From[M]])
					:InsertReturningKey[Args, M, @~ ~ T, T] =
				new InsertComponentReturning[Args, M, T](this, key(table.row))
		}

		/** A base trait for `Insert` statements which potentially add more than a single row to the table.
		  * This encompasses both:
		  *   1. [[net.noresttherein.oldsql.sql.Insert.syntax.BatchInsert BatchInsert]] batches, which
		  *      execute the same statement for multiple parameter sets, and
		  *   1. [[net.noresttherein.oldsql.sql.Insert.syntax.RowsInsert RowsInsert]] - single insert statements
		  *      which insert multiple rows, both parameterized and ground.
		  *
		  * It introduces a [[net.noresttherein.oldsql.sql.TableStatement.ReturningClauseFactory.returning returning]]
		  * clause specifying a component of the rows which should be returned for every inserted row by the DBMS driver
		  * together in a sequence.
		  */
		sealed trait RowInserts[-Args, M[O] <: MappingAt[O], +Res]
			extends InsertDML[Args, M, Res] //with GenericRowStatements[Args, M, Res, Seq]
			   with ReturningClauseFactory[M, ({ type U[X] = InsertsReturningKeys[Args, M, @~ ~ X, X] })#U]

		/** A base trait for statements which insert multiple rows with each execution
		  * (as opposed to a [[net.noresttherein.oldsql.sql.Insert.syntax.BatchInsert batches]]), such as
		  * [[net.noresttherein.oldsql.sql.Insert.syntax.InsertSelect InsertSelect]] and
		  * [[net.noresttherein.oldsql.sql.Insert.syntax.GroundInsertSelect]] which insert rows returned
		  * by a specified SQL ''select'',
		  * or [[net.noresttherein.oldsql.sql.Insert.syntax.EntityMultiInsert EntityMultiInsert]]
		  * and [[net.noresttherein.oldsql.sql.Insert.syntax.GroundMultiInsert GroundMultiInsert]],
		  * which explicitly specified data for several rows.
		  */
		sealed trait RowsInsert[-Args, M[O] <: MappingAt[O], +Res]
			extends Insert[Args, M, Res] with RowInserts[Args, M, Res] with RowsStatement[Args, M, Res]
			   with ReturningClauseFactory[M, ({ type U[X] = InsertReturningKeys[Args, M, @~ ~ X, X] })#U]
		{
			override def returning[T](key :M[From[M]] => TypedMapping[T, From[M]])
					:InsertReturningKeys[Args, M, @~ ~ T, T] =
				new DefaultInsertReturningKeys[Args, M, @~ ~ T, T](
					new InsertComponentReturning[Args, M, T](this, key(table.row))
				)
		}

		/** A base trait for parameterized ''insert'' batches, that is parameterized
		  * [[net.noresttherein.oldsql.sql.Insert Insert]] statements executed once for every parameter set `Args`
		  * in the batch's argument sequence.
		  */
		sealed trait BatchInsert[-Args, M[O] <: MappingAt[O], +Res]
				extends InsertDML[Seq[Args], M, Seq[Res]] with RowsBatch[Args, M, Res]
//			extends RowInserts[Seq[Args], M, Seq[Res]] with RowsBatch[Args, M, Res]
			   with ReturningClauseFactory[M, ({ type U[X] = InsertBatchReturningKeys[Args, M, @~ ~ X, X] })#U]
		{
			override def returning[T](key :M[From[M]] => TypedMapping[T, From[M]])
					:InsertBatchReturningKeys[Args, M, @~ ~ T, T] =
				new DefaultInsertBatchReturningKeys[Args, M, @~ ~ T, T](dml returning key)

			protected def dml :RowInsert[Args, M, Res]
		}

		/** A base trait for parameterless ''insert'' batches, that is parameterized
		  * [[net.noresttherein.oldsql.sql.Insert Insert]] statements coupled with multiple parameter sets `Seq[Args]`.
		  */
		sealed trait GroundBatchInsert[Args, M[O] <: MappingAt[O], +Res]
				extends InsertDML[Any, M, Seq[Res]] with GroundRowsBatch[Args, M, Res]
//			extends RowInserts[Any, M, Seq[Res]] with GroundRowsBatch[Args, M, Res]
			   with ReturningClauseFactory[M, ({ type U[X] = GroundInsertBatchReturningKeys[Args, M, @~ ~ X, X] })#U]
		{
			override def returning[T](key :M[From[M]] => TypedMapping[T, From[M]])
					:GroundInsertBatchReturningKeys[Args, M, @~ ~ T, T] =
				new DefaultGroundInsertBatchReturningKeys[Args, M, @~ ~ T, T](dml returning key, args)

			protected def dml :BatchInsert[Args, M, Res]
			protected def args :Seq[Args]
		}


		/** A generic interface for `Insert` statements which insert whole entities, that is objects mapped
		  * to the table by its mapping `M`.
		  * Introduces [[net.noresttherein.oldsql.sql.TableStatement.UpdatingClauseFactory.updating updating]]
		  * and [[net.noresttherein.oldsql.sql.TableStatement.UpdatingClauseFactory.updatingKeys updatingKeys]]
		  * clauses which allow to specify the database generated keys of the table which should be returned by the call;
		  * these values are then used to set the corresponding properties of the inserted arguments,
		  * returning the updated entities.
		  * @tparam Args the complete parameter type of the statement (including `Any` for
		  *              [[net.noresttherein.oldsql.sql.Insert.implementation.GroundInsert GroundInsert]]).
		  * @tparam S    the entity type, that is the subject type of the table's mapping `M`.
		  * @tparam M    the mapping type for the rows of the table.
		  * @tparam Res  the complete return type of the statement.
		  * @tparam U    an `Insert` statement with an `updating` clause added, that is returning entities updated
		  *              with autogenerated keys.
		  */
		sealed trait EntityInsertTemplate[-Args, S, M[O] <: BaseMapping[S, O], +Res, +U]
			extends Insert[Args, M, Res] with EntityStatementsTemplate[Args, M, Res, U]
		{
			override def updatingKeys(keys :Seq[TypedMapping[_, From[M]]]) :U =
				updating(keys, Returning.implementation.columnNames(table, keys))

			protected def updating(components :Seq[TypedMapping[_, From[M]]], columnNames :Seq[String]) :U

			private[sql] def exportMapping = table.export.asInstanceOf[TypedMapping[S, From[M]]]
		}

		/** A base trait for [[net.noresttherein.oldsql.sql.Insert Insert]] statements inserting a single entity,
		  * that is a value of the table's mapping's [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type.
		  * This encompasses both [[net.noresttherein.oldsql.sql.Insert.syntax.ParamEntityInsert parameterized]]
		  * and [[net.noresttherein.oldsql.sql.Insert.syntax.GroundEntityInsert parameterless]] variants.
		  */
		sealed trait EntityInsert[-Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends RowInsert[Args, M, Res] with EntityStatement[Args, S, M, Res]
			   with EntityInsertTemplate[Args, S, M, Res, InsertReturningEntity[Args, S, M]]
		{
			protected override def updating(components :Seq[TypedMapping[_, From[M]]], columnNames :Seq[String])
					:InsertReturningEntity[Args, S, M] =
				new DefaultInsertReturningEntity[Args, S, M](
					this, components, columnNames, updatedResult(columnNames)
				)

			protected def updatedResult(columns :Seq[String]) :StatementResult[S, S]
		}

		/** The type of standard [[net.noresttherein.oldsql.sql.Insert Insert]] statements parameterized
		  * with and inserting a single entity (the value mapped to a table row).
		  */
		sealed trait ParamEntityInsert[S, M[O] <: BaseMapping[S, O], +Res] extends EntityInsert[S, S, M, Res] {
			protected override def updatedResult(columns :Seq[String]) :StatementResult[S, S] =
				UpdatedEntities.Single(exportMapping, columns)
		}

		/** The type of a simple parameterless [[net.noresttherein.oldsql.sql.Insert Insert]] statement
		  * (that is, carrying with it the inserted value) inserting a single entity (the value mapped to a table row).
		  */
		sealed trait GroundEntityInsert[S, M[O] <: BaseMapping[S, O], +Res] extends EntityInsert[Any, S, M, Res] {
			protected override def updatedResult(columns :Seq[String]) :StatementResult[S, S] =
				UpdatedEntities.Single(value, exportMapping, columns)
			protected def value :S
		}

		/** Base trait for ''insert'' [[net.noresttherein.oldsql.sql.Insert statements]]
		  * and [[net.noresttherein.oldsql.sql.InsertDML batches]] which insert multiple entities (rows initialized
		  * with values of the table's mapping's [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type).
		  * This includes:
		  *   1. [[net.noresttherein.oldsql.sql.Insert.syntax.BatchEntityInsert BatchEntityInsert]] -
		  *      a parameterized insert statement for a single row, but executed repeatedly for each entity
		  *      in the argument sequence as a part of a single JDBC batch;
		  *   1. [[net.noresttherein.oldsql.sql.Insert.syntax.GroundBatchEntityInsert GroundBatchEntityInsert]] -
		  *      a parameterless `Insert` statement inserting a single row, but executed multiple times,
		  *      once for each of the bundled entity values;
		  *   1. [[net.noresttherein.oldsql.sql.Insert.syntax.EntitiesInsert EntitiesInsert]] -
		  *      a single `Insert` statement - either
		  *      [[net.noresttherein.oldsql.sql.Insert.syntax.ParamEntitiesInsert parameterized]] or
		  *      [[net.noresttherein.oldsql.sql.Insert.syntax.GroundEntitiesInsert parameterless]] - inserting
		  *      multiple entities, typically explicitly specified as several rows in a ''values'' clause.
		  */
		sealed trait EntityInserts[-Args, S, M[O] <: BaseMapping[S, O], +Res] extends RowInserts[Args, M, Res]
			with EntityStatementsTemplate[Args, M, Res, InsertsReturningEntities[Args, S, M]]

		sealed trait EntitiesInsert[-Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends RowsInsert[Args, M, Res] with EntityInserts[Args, S, M, Res]
			   with EntitiesStatement[Args, S, M, Res]
			   with EntityInsertTemplate[Args, S, M, Res, InsertReturningEntities[Args, S, M]]
		{
			protected override def updating(components :Seq[TypedMapping[_, From[M]]], columnNames :Seq[String])
					:InsertReturningEntities[Args, S, M] =
				new DefaultInsertReturningEntities[Args, S, M](
					this, components, columnNames, updatedResult(columnNames)
				)
			protected def updatedResult(columns :Seq[String]) :StatementResult[S, Seq[S]]
		}

		sealed trait ParamEntitiesInsert[S, M[O] <: BaseMapping[S, O], +Res] extends EntitiesInsert[Seq[S], S, M, Res] {
			protected override def updatedResult(columns :Seq[String]) :StatementResult[S, Seq[S]] =
				UpdatedEntities(exportMapping, columns, Seq)
		}

		sealed trait GroundEntitiesInsert[S, M[O] <: BaseMapping[S, O], +Res] extends EntitiesInsert[Any, S, M, Res] {
			protected override def updatedResult(columns :Seq[String]) :StatementResult[S, Seq[S]] =
				UpdatedEntities(values, exportMapping, columns, Seq)
			protected def values :Seq[S]
		}

		sealed trait BatchEntityInsert[S, M[O] <: BaseMapping[S, O], +Res]
			extends EntitiesBatch[S, S, M, Res] with BatchInsert[S, M, Res] with EntityInserts[Seq[S], S, M, Seq[Res]]
			   with EntityStatementsTemplate[Seq[S], M, Seq[Res], InsertBatchReturningEntities[S, M]]
		{
			override def updatingKeys(keys :Seq[TypedMapping[_, From[M]]]) :InsertBatchReturningEntities[S, M] =
				new DefaultInsertBatchReturningEntities[S, M](dml updatingKeys keys)

			protected def dml :EntityInsert[S, S, M, Res]
		}

		sealed trait GroundBatchEntityInsert[S, M[O] <: BaseMapping[S, O], +Res]
			extends GroundEntitiesBatch[S, S, M, Res] with GroundBatchInsert[S, M, Res]
			   with EntityInserts[Any, S, M, Seq[Res]]
			   with EntityStatementsTemplate[Any, M, Seq[Res], GroundInsertBatchReturningEntities[S, M]]
		{
			override def updatingKeys(keys :Seq[TypedMapping[_, From[M]]]) :GroundInsertBatchReturningEntities[S, M] =
				new DefaultGroundInsertBatchReturningEntities[S, M](dml updatingKeys keys, args)

			protected def dml :BatchEntityInsert[S, M, Res]
			protected def args :Seq[S]
		}




		sealed trait InsertReturningKey[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends InsertReturning[Args, M, Res] with ReturningTupleClause[Args, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Res, Insert[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: InsertReturningKey[Args, M, X, X]

//			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
//				visitor.insertReturningKey(this)
		}

		private trait InsertReturningKeyExpansion[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends InsertReturningKey[Args, M, Keys, Res]
		{
			override type tuple[X <: Chain] = InsertReturningKey[Args, M, X, X]

			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) :InsertReturningKey[Args, M, Keys~T, Keys~T] =
				//don't export it for better debugging, columns will be exported anyway and only they really matter
				new InsertComponentsReturning[Args, M, Keys, T](this, key(table.row))
		}

		private class InsertComponentReturning[Args, M[O] <: MappingAt[O], K]
		                                      (override val statement :Insert[Args, M, Any],
		                                       override val key :TypedMapping[K, From[M]])
			extends ReturningTupleSingleton[Args, M, K](statement, key)
			   with InsertReturningKeyExpansion[Args, M, @~ ~ K, K]

		private class InsertComponentsReturning[Args, M[O] <: MappingAt[O], Keys <: Chain, K]
		                                       (override val init :InsertReturningKey[Args, M, Keys, Any],
		                                        override val key :TypedMapping[K, From[M]])
			extends ReturningProperTuple[Args, M, Keys, K](init, key)
			   with InsertReturningKeyExpansion[Args, M, Keys ~ K, Keys ~ K]
		{
			override val statement = init.statement
		}


		sealed trait InsertsReturningKeys[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends InsertsReturning[Args, M, Seq[Res]] with ReturningTuplesClauses[Args, M, Keys, Res, Seq]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Seq[Res], Insert[Nothing, M, Any]]
		{
			override type tuple[X <: Chain] <: InsertsReturningKeys[Args, M, X, X]

//			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
//				visitor.insertReturningKeys(this)
		}

		sealed trait InsertReturningKeys[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends InsertReturning[Args, M, Seq[Res]] with InsertsReturningKeys[Args, M, Keys, Res]
			   with ReturningTuplesClause[Args, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Seq[Res], Insert[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: InsertReturningKeys[Args, M, X, X]
		}

		private class DefaultInsertReturningKeys[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
		                                        (override val dml :InsertReturningKey[Args, M, Keys, Res])
			extends ReturningTuplesTemplate[Args, M, Keys, Res, Insert[Args, M, Any]]
			   with InsertReturningKeys[Args, M, Keys, Res]
		{
			override type tuple[X <: Chain] = InsertReturningKeys[Args, M, X, X]
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultInsertReturningKeys[Args, M, Keys ~ T, Keys ~ T](dml x key)
		}


		sealed trait InsertBatchReturningKeys[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends InsertsReturningKeys[Seq[Args], M, Keys, Res] with BatchReturningTuplesClause[Args, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Seq[Args], M, Keys, Seq[Res], Insert[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: InsertBatchReturningKeys[Args, M, X, X]
		}

		private class DefaultInsertBatchReturningKeys[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
		                                             (override val dml :InsertReturningKey[Args, M, Keys, Res])
			extends RepeatedDML[Args, Res] with ReturningTupleSeqTemplate[Seq[Args], M, Keys, Res, Insert[Args, M, Any]]
			   with InsertBatchReturningKeys[Args, M, Keys, Res]
		{
			override type tuple[X <: Chain] = InsertBatchReturningKeys[Args, M, X, X]
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultInsertBatchReturningKeys[Args, M, Keys ~ T, Keys ~ T](dml x key)
		}

		sealed trait GroundInsertBatchReturningKeys[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends InsertsReturningKeys[Any, M, Keys, Res] with GroundBatchReturningTuplesClause[Args, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Any, M, Keys, Seq[Res], Insert[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: GroundInsertBatchReturningKeys[Args, M, X, X]
		}

		private class DefaultGroundInsertBatchReturningKeys[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
		              (override val dml :InsertBatchReturningKeys[Args, M, Keys, Res], override val args :Seq[Args])
			extends BoundDML[Seq[Args], Seq[Res]] with ReturningTupleSeqTemplate[Any, M, Keys, Res, Insert[Args, M, Any]]
			   with GroundInsertBatchReturningKeys[Args, M, Keys, Res]
			   with BoundDML.Impl[Seq[Args], Seq[Res], Insert.into[M]#DML]
		{
			override type tuple[X <: Chain] = GroundInsertBatchReturningKeys[Args, M, X, X]
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultGroundInsertBatchReturningKeys[Args, M, Keys ~ T, Keys ~ T](dml x key, args)
		}




		sealed trait InsertReturningEntity[-Args, S, M[O] <: BaseMapping[S, O]]
			extends InsertReturning[Args, M, S] with ReturningEntityClause[Args, S, M]
			   with GenericReturningEntitiesClause[Args, S, M, S, Insert[Args, M, Any], InsertReturningEntity[Args, S, M]]
		{
//			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, S] =
//				visitor.insertReturningEntity(this)
		}

		private[Insert] class DefaultInsertReturningEntity[Args, S, M[O] <: BaseMapping[S, O]]
		                                                  (override val statement :Insert[Args, M, Any],
		                                                   override val keys :Seq[TypedMapping[_, From[M]]],
		                                                   override val columnNames :Seq[String],
		                                                   override val result :StatementResult[S, S])
			extends AbstractReturningEntities[Args, S, M, S](statement, keys, columnNames, result)
			   with InsertReturningEntity[Args, S, M]
		{
			def this(statement :Insert[Args, M, Any], keys :Seq[TypedMapping[_, From[M]]], result :StatementResult[S, S]) =
				this(statement, keys, Returning.implementation.columnNames(statement.table, keys), result)

			override type x[T] = InsertReturningEntity[Args, S, M]

			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) = {
				val comp = key(table.row)
				val columns = columnNames :++ Returning.implementation.columnNames(table, comp)
				new DefaultInsertReturningEntity[Args, S, M](statement, keys :+ comp, columns, result)
			}
			override def toReturningEntities :InsertReturningEntities[Args, S, M] =
				new DefaultInsertReturningEntities[Args, S, M](statement, keys, columnNames, result.batch(1))
		}


		sealed trait InsertsReturningEntities[-Args, S, M[O] <: BaseMapping[S, O]]
			extends InsertsReturning[Args, M, Seq[S]] with ReturningEntitiesClauses[Args, S, M, Seq[S]]
			   with GenericReturningEntitiesClause[Args, S, M, Seq[S], Insert[Nothing, M, Any],
			                                       InsertsReturningEntities[Args, S, M]]

		sealed trait InsertReturningEntities[-Args, S, M[O] <: BaseMapping[S, O]]
			extends InsertReturning[Args, M, Seq[S]] with ReturningEntitiesClause[Args, S, M]
			   with InsertsReturningEntities[Args, S, M]
			   with GenericReturningEntitiesClause[Args, S, M, Seq[S],
			                                       Insert[Args, M, Any], InsertReturningEntities[Args, S, M]]
		{
//			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Seq[S]] =
//				visitor.insertReturningEntities(this)
		}

		private class DefaultInsertReturningEntities[Args, S, M[O] <: BaseMapping[S, O]]
		              (override val statement :Insert[Args, M, Any], override val keys :Seq[TypedMapping[_, From[M]]],
		               override val columnNames :Seq[String], override val result :StatementResult[S, Seq[S]])
			extends AbstractReturningEntities[Args, S, M, Seq[S]](statement, keys, columnNames, result)
			   with InsertReturningEntities[Args, S, M]
		{
			override type x[T] = InsertReturningEntities[Args, S, M]

			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) = {
				val comp = key(table.row)
				val columns = columnNames :++ Returning.implementation.columnNames(table, comp)
				new DefaultInsertReturningEntities[Args, S, M](statement, keys :+ comp, columns, result)
			}
		}

		sealed trait InsertBatchReturningEntities[S, M[O] <: BaseMapping[S, O]]
			extends InsertsReturningEntities[Seq[S], S, M] with BatchReturningEntitiesClause[S, S, M]
			   with GenericReturningEntitiesClause[Seq[S], S, M, Seq[S], Insert[S, M, Any],
			                                       InsertBatchReturningEntities[S, M]]

		private class DefaultInsertBatchReturningEntities[S, M[O] <: BaseMapping[S, O]]
		                                                 (override val dml :InsertReturningEntity[S, S, M])
			extends InsertBatchReturningEntities[S, M] with RepeatedDML[S, S]
			   with GenericBatchReturningEntities[Seq[S], S, M, Seq[S], Insert[S, M, Any],
			                                      InsertBatchReturningEntities[S, M]]
		{
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultInsertBatchReturningEntities[S, M](dml x key)
		}


		sealed trait GroundInsertBatchReturningEntities[S, M[O] <: BaseMapping[S, O]]
			extends InsertsReturningEntities[Any, S, M] with GroundBatchReturningEntitiesClause[S, M]
			   with GenericReturningEntitiesClause[Any, S, M, Seq[S], Insert[S, M, Any],
			                                       GroundInsertBatchReturningEntities[S, M]]

		private class DefaultGroundInsertBatchReturningEntities[S, M[O] <: BaseMapping[S, O]]
		              (override val dml :InsertBatchReturningEntities[S, M], override val args :Seq[S])
			extends GroundInsertBatchReturningEntities[S, M] with BoundDML[Seq[S], Seq[S]]
			   with BoundDML.Impl[Seq[S], Seq[S], Insert.into[M]#DML]
			   with GenericBatchReturningEntities[Any, S, M, Seq[S], Insert[S, M, Any],
			                                      GroundInsertBatchReturningEntities[S, M]]
		{
			override def x[T](key :M[From[M]] => TypedMapping[T, From[M]]) =
				new DefaultGroundInsertBatchReturningEntities[S, M](dml x key, args)
		}



		/** The DSL factory interface of [[net.noresttherein.oldsql.sql.Insert Insert]] statements
		  * providing [[net.noresttherein.oldsql.sql.Insert.syntax.GenericInsertFactory.set set]] and
		  * [[net.noresttherein.oldsql.sql.Insert.syntax.GenericInsertFactory.setAll setAll]] methods
		  * for specifying additional component/inserted value pairs.
		  * The difference from the generic `Insert` is purely cosmetic, restricted to the creation process.
		  * It provides no additional features and is not handled in any special way.
		  */
		trait InsertSet[Arg, M[O] <: MappingAt[O], +Res]
			extends GenericInsertFactory[Arg, M, InsertSet[Arg, M, Res]] with Insert[Arg, M, Res]

		//InsertDML mixed after factories for proper override order of table
		trait RowInsertSet[Arg, M[O] <: MappingAt[O], +Res]
			extends GenericInsertFactory[Arg, M, RowInsertSet[Arg, M, Res]]
			   with InsertSet[Arg, M, Res] with RowInsert[Arg, M, Res]

		trait EntityInsertSet[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericInsertFactory[S, M, EntityInsertSet[S, M, Res]]
			   with RowInsertSet[S, M, Res] with EntityInsert[S, S, M, Res]

		trait RowsInsertSet[Arg, M[O] <: MappingAt[O], +Res]
			extends GenericInsertFactory[Arg, M, RowsInsertSet[Arg, M, Res]]
			   with RowsInsert[Seq[Arg], M, Res]

		trait EntitiesInsertSet[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericInsertFactory[S, M, EntitiesInsertSet[S, M, Res]]
				with RowsInsertSet[S, M, Res] with EntitiesInsert[Seq[S], S, M, Res]

		trait BatchInsertSet[Arg, M[O] <: MappingAt[O], +Res]
			extends GenericInsertFactory[Arg, M, BatchInsertSet[Arg, M, Res]] with BatchInsert[Arg, M, Res]

		trait BatchEntityInsertSet[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericInsertFactory[S, M, BatchEntityInsertSet[S, M, Res]]
			   with BatchInsertSet[S, M, Res] with BatchEntityInsert[S, M, Res]


		trait GenericInsertFactory[Arg, M[O] <: MappingAt[O], +Res]
			extends Any with SetClauseFactory[Arg, M, WithParam.Last[Arg], Res]
		{
			def setAll(setters :(M[From[M]], UnboundParam.Last[Arg]) => Seq[From[M] := SetDomain]) :Res = {
				val base = setDomain
				setAll(setters(table.row, base.last.mapping).map(_.anchor(base.left, base)))
			}

			//overridden because overload resolution ...
//			override def set(setter :(M[From[M]], UnboundParam.Last[Arg]) => From[M] := SetDomain) :Res = {
//				val base = setDomain
//				set(setter(table.row, base.last.mapping).anchor(base.left, base))
//			}

			def set(setter :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg]) => From[M] := SetDomain) :Res = {
				val base = setDomain
				val param = base.last.mapping
				set(setter(table.row, param, param).anchor(base.left, base))
			}			

			def set(setter :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg], UnboundParam.Last[Arg]) 
			                => From[M] := SetDomain) :Res = 
			{
				val base = setDomain
				val param = base.last.mapping
				set(setter(table.row, param, param, param).anchor(base.left, base))
			}			

			def set(setter :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg], UnboundParam.Last[Arg], 
				                         UnboundParam.Last[Arg])
			                => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val param = base.last.mapping
				set(setter(table.row, param, param, param, param).anchor(base.left, base))
			}			

			def set(setter :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg], UnboundParam.Last[Arg], 
				                         UnboundParam.Last[Arg], UnboundParam.Last[Arg])
			                => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val param = base.last.mapping
				set(setter(table.row, param, param, param, param, param).anchor(base.left, base))
			}

			//consider: up to 9*UnboundParam.Last[Arg]

			//doesn't make much sense to overload it further, as all setters must have the same arity
			def set(first  :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg]) => From[M] := SetDomain,
			        second :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg]) => From[M] := SetDomain,
			        rest   :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg]) => From[M] := SetDomain*) :Res =
			{
				val base = setDomain
				val row = table.row[From[M]]
				val param = base.last.mapping
				setAll(
					first(row, param, param).anchor(base.left, base) +: second(row, param, param).anchor(base.left, base)
						+: rest.map(_(row, param, param).anchor(base.left, base))
				)
			}

		}

		sealed trait GenericSupplantedInsertFactory[Arg, M[O] <: MappingAt[O], +Res]
			extends Any with SupplantClauseFactory[Arg, M, WithParam.Last[Arg], Res]
		{
			def supplantAll(setters :(M[From[M]], UnboundParam.Last[Arg]) => Seq[From[M] := SetDomain]) :Res = {
				val base = setDomain
				supplantAll(setters(table.row, base.last.mapping).map(_.anchor(base.left, base)))
			}

			def supplant(setter :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg]) => From[M] := SetDomain)
					:Res =
			{
				val base = setDomain
				val param = base.last.mapping
				supplant(setter(table.row, param, param).anchor(base.left, base))
			}

			def supplant(setter :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg], UnboundParam.Last[Arg])
			                     => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val param = base.last.mapping
				supplant(setter(table.row, param, param, param).anchor(base.left, base))
			}

			def supplant(setter :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg], UnboundParam.Last[Arg],
			                                  UnboundParam.Last[Arg])
			                     => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val param = base.last.mapping
				supplant(setter(table.row, param, param, param, param).anchor(base.left, base))
			}

			def supplant(setter :(M[From[M]], UnboundParam.Last[Arg], UnboundParam.Last[Arg], UnboundParam.Last[Arg],
			                                  UnboundParam.Last[Arg], UnboundParam.Last[Arg])
			                     => From[M] := SetDomain) :Res =
			{
				val base = setDomain
				val param = base.last.mapping
				supplant(setter(table.row, param, param, param, param, param).anchor(base.left, base))
			}
			//consider: up to 9*UnboundParam.Last[Arg]
		}




		class InsertParam[X] private[Insert] (private val param :ParamRelation[X]) extends AnyVal {
			def into[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			        (table :RelVar[M])(implicit reveal :BaseMappingSubject[M, T, S], form :SQLForm[X])
					:InsertFactory[X, M] =
				new InsertFactory[X, M](From(table).param[X])

			def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			         (table :RelVar[M])(implicit reveal :BaseMappingSubject[M, T, S], form :SQLForm[X])
					:InsertFactory[X, M] =
				new InsertFactory[X, M](From(table).param[X])

			def many[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			        (table :RelVar[M])(implicit reveal :BaseMappingSubject[M, T, S], form :SQLForm[X])
					:InsertBatchFactory[X, M] =
				new InsertBatchFactory(From(table).param[X])
		}


		class InsertFactory[Arg, M[O] <: MappingAt[O]] private[Insert]
		                   (protected override val setDomain :From[M] WithParam Arg)
			extends /*AnyVal with */GenericInsertFactory[Arg, M, RowInsertSet[Arg, M, Int]]
		{
			protected override def table :RelVar[M] = setDomain.left.table.castFrom[Table[M], RelVar[M]]

			protected override def setAll(setters :Seq[From[M] := WithParam.Last[Arg]]) :RowInsertSet[Arg, M, Int] =
				new DefaultInsert[Arg, M](setDomain, table, setters)

			def *(max :Int) :MultiInsertFactory[Arg, M] =
				new MultiInsertFactory[Arg, M](setDomain, table, max)
		}

		class InsertBatchFactory[Arg, M[O] <: MappingAt[O]] private[Insert]
		                        (protected override val setDomain :From[M] WithParam Arg)
			extends AnyVal with GenericInsertFactory[Arg, M, BatchInsertSet[Arg, M, Int]]
		{
			protected override def table :RelVar[M] = setDomain.left.table.castFrom[Table[M], RelVar[M]]

			protected override def setAll(setters :Seq[From[M] := WithParam.Last[Arg]]) :BatchInsertSet[Arg, M, Int] =
				new DefaultBatchInsert[Arg, M](setDomain, table, setters)
		}




		sealed trait InsertValues[Arg, Args <: Chain, M[O] <: MappingAt[O]] extends RowInsert[Arg, M, Int] {
			def x[T](component :M[From[M]] => TypedMapping[T, From[M]]) :InsertValues[Args ~ T, Args ~ T, M]

			override def result :StatementResult[Arg, Int]       = UpdateCount
			def components      :Seq[TypedMapping[_, From[M]]]
//			def argsForm        :SQLWriteForm[Args]
			def argsForm        :SQLForm[Args]
			def argForm         :SQLForm[Arg]
			def domain          :From[M] WithParam Arg

			val setters :Seq[From[M] := WithParam.Last[Arg]]
			def setters[Xs <: Chain](domain :WithParam.Last[Xs], get :Xs => Args) :Seq[From[M] := WithParam.Last[Xs]]

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Arg] = {
				//todo: use InsertPreset, CustomInsert, ExtraInsert
				val comps = components
				val prefix = spelling.INSERT + spelling._INTO_
				val tableName = spelling.table(table, "")(Dual, spelling.newContext, Parameterization.paramless)
				val columnNames = comps.iterator.flatMap(_.insertedByDefault)
				                       .mkString("(", ", ", ") " + spelling.VALUES)
				val params = {
					val res = new StringBuilder
					res += '('
					comps foreach { component =>
						(res /: component.insertedByDefault.size)(_ ++= "?, ")
					}
					if (res.length == 0)
						throw new IllegalArgumentException(
							components.mkString("Cannot insert components ", ", ", " into " + table +
							                    ": empty insertedByDefault column lists")
						)
					res.delete(res.length - 2, res.length).append(')').toString
				}
				val sql = (prefix +: tableName.sql) + columnNames + params
				SpelledSQL[Arg](sql, argForm, tableName.context)
			}

			private[Insert] def export[T](component :M[From[M]] => TypedMapping[T, From[M]])
					:TypedMapping[T, From[M]] =
				table.export[From[M]].export(component(table.row))

			private[Insert] def validate(component :TypedMapping[_, From[M]]) :Unit =
				if (component.insertedByDefault.size != component.insertForm.columnCount)
					throw new IllegalArgumentException(
						"Cannot insert " + table + "(" + component + ") because the number of component's "
							+ "insertedByDefault columns (" + component.insertedByDefault.size + ") does not equal "
							+ "the number of columns written by its insertForm (" + component.insertForm.columnCount
							+ "): " + component.insertForm + " vs. " + component.insertedByDefault + "."
					)


			override def canEqual(that :Any) :Boolean = that.isInstanceOf[InsertValues[_, _, MappingAt @unchecked]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :InsertValues[_, _, _] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && components == other.components
				case _ => false
			}
			protected override def initHashCode :Int = table.hashCode * 31 + components.hashCode
			protected override def initToString :String = components.mkString("Insert into " + table + "(", ", ", ")")
		}


		private class InsertValue[Arg, M[O] <: MappingAt[O]]
		                         (from :From[M], override val table :RelVar[M], param :TypedMapping[Arg, From[M]])
			extends ParamInsert[Arg, M] with InsertValues[Arg, @~ ~ Arg, M]
		{   //todo * n; apply(entities)
			override def x[T](component :M[From[M]] => TypedMapping[T, From[M]])
					:InsertValues[@~ ~ Arg ~ T, @~ ~ Arg ~ T, M] =
				new InsertValueChain[@~ ~ Arg, T, M](this, from, component(table.row))

			val component                 :TypedMapping[Arg, From[M]]    = table.export[From[M]].export(param)
			override val components       :Seq[TypedMapping[_, From[M]]] = PassedArray :+ component
//			override val argsForm      :SQLWriteForm[@~ ~ Arg]          = ChainForm ~ component.insertForm
			implicit override val argForm :SQLForm[Arg]                    = component.insertForm <> component.selectForm
			override val argsForm         :SQLForm[@~ ~ Arg]               = ChainForm ~ argForm
			override val domain           :From[M] WithParam Arg           = from.param[Arg]

			override lazy val setters :Seq[From[M] := WithParam.Last[Arg]] =
				(component.anchor(domain.left) := domain.last) :: Nil

			override def setters[Xs <: Chain](domain :WithParam.Last[Xs], get :Xs => @~ ~ Arg)
					:Seq[From[M] := WithParam.Last[Xs]] =
			{
				val scribe = SQLScribe.replaceParam(
						this.domain, domain, this.domain.last.toRelationSQL, domain.last.toRelationSQL
					)(Requisite { xs :Xs => get(xs).last })
				(component.anchor(this.domain.left) := scribe(this.domain.last))::Nil
			}

			validate(component)

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :InsertValue[_, _] => other.table == table && other.component == component
				case other :InsertValues[_, _, _] if other canEqual this =>
					other.hashCode == hashCode && other.table == table && other.components == components
				case _ => false
			}
			protected override def initToString :String = "Insert into " + table + "(" + component + ")"
		}


		private class InsertValueChain[Args <: Chain, Arg, M[O] <: MappingAt[O]]
		                              (private val prefix :InsertValues[_, Args, M], from :From[M],
		                               param :TypedMapping[Arg, From[M]])
			extends ParamInsert[Args ~ Arg, M] with InsertValues[Args ~ Arg, Args ~ Arg, M]
		{
			override def x[T](component :M[From[M]] => TypedMapping[T, From[M]])
					:InsertValues[Args ~ Arg ~ T, Args ~ Arg ~ T, M] =
				new InsertValueChain[Args ~ Arg, T, M](this, from, component(table.row))

			override val table      :RelVar[M]                       = prefix.table
			val component           :TypedMapping[Arg, From[M]]    = table.export[From[M]].export(param)
			override val components :Seq[TypedMapping[_, From[M]]] = prefix.components :+ component
			implicit val lastForm   :SQLForm[Arg]                    = (component.insertForm <> component.selectForm)
			override val argForm    :SQLForm[Args ~ Arg]             = prefix.argsForm ~ lastForm
			implicit override def argsForm :SQLForm[Args ~ Arg]      = argForm

			override lazy val domain  :From[M] WithParam (Args ~ Arg)  = from.param[Args ~ Arg]

			override lazy val setters :Seq[From[M] := WithParam.Last[Args ~ Arg]] = {
				val setter = component.anchor(domain.left) := domain.last.mapping(_.last).anchor(domain)
				(setter +: prefix.setters[Args ~ Arg](domain, Chain.init)).reverse
			}

			override def setters[Xs <: Chain](domain :WithParam.Last[Xs], get :Xs => Args ~ Arg)
					:Seq[From[M] := WithParam.Last[Xs]] =
			{ //todo: casting is needed because origins used by inserts are not the generalized types required by \
				val paramDomain = this.domain :WithParam.Last[Args ~ Arg]
				val scribe = SQLScribe.replaceParam(
						paramDomain, domain, this.domain.last.toRelationSQL, domain.last.toRelationSQL
					)(get(_))
				val left = this.domain.left.last \ component.withOrigin[From.Last[M]]
				val right = this.domain.last \ this.domain.last.mapping(_.last)
				val setter = left := scribe(right)//.anchor(domain)
				setter +: prefix.setters[Xs](domain, get andThen Chain.init)
			}

			validate(component)

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :InsertValueChain[_, _, _] => component == other.component && prefix == other.prefix
				case other :InsertValues[_, _, _] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && components == other.components
				case _ => false
			}
		}


		final class InsertManyValues[Arg, Args <: Chain, M[O] <: MappingAt[O]] private[Insert]
		                            (override val dml :InsertValues[Arg, Args, M])
			extends BatchInsert[Arg, M, Int] with RepeatedDML[Arg, Int]
		{
			def x[T](component :M[From[M]] => TypedMapping[T, From[M]]) :InsertManyValues[Args ~ T, Args ~ T, M] =
				new InsertManyValues[Args ~ T, Args ~ T, M](dml x component)

			override val table :RelVar[M] = dml.table
		}




		final class MultiInsertFactory[Arg, M[O] <: MappingAt[O]] private[Insert]
		                              (protected override val setDomain :From[M] WithParam Arg,
		                               protected override val table :RelVar[M], val max :Int)
			extends GenericInsertFactory[Arg, M, RowsInsertSet[Arg, M, Int]]
		{
			protected override def setAll(setters :Seq[From[M] := WithParam.Last[Arg]]) :RowsInsertSet[Arg, M, Int] = {
				val domain = setDomain.left.param(setDomain.last.mapping.form * max)
				new MultiInsertSet[Arg, M](domain, setDomain, table, setters, max)
			}
		}


		final class EntityMultiInsert[S, M[O] <: BaseMapping[S, O]] private[Insert]
		            (override val setDomain :From[M] WithParam S, override val table :RelVar[M], val max :Int)
			extends ParamInsert[Seq[S], M] with EntitiesInsertSet[S, M, Int] with ParamEntitiesInsert[S, M, Int]
			   with SupplantClauseFactory[S, M, WithParam.Last[S], EntitiesInsertSet[S, M, Int]]
		{
			protected override def setAll(setters :Seq[From[M] := WithParam.Last[S]]) :EntitiesInsertSet[S, M, Int] =
				new EntityMultiInsertSet[S, M](domain, setDomain, table, setters, max)

			protected override def supplantAll(setters :Seq[From[M] := WithParam.Last[S]])
					:EntitiesInsertSet[S, M, Int] =
				new SupplantedMultiInsert[S, M](domain, setDomain, table, setters, max)

			protected override val domain :From[M] WithParam Seq[S] =
				From(table).param(setDomain.last.mapping.form * max)

			override def setters :Seq[From[M] := WithParam.Last[Seq[S]]] = {
				val export = table.export.asInstanceOf[TypedMapping[S, Unit]]
				implicit val paramForm = export.insertForm <> export.selectForm
				(0 until max).map { i =>
					domain.left.last := domain.last \ domain.last.mapping.comp(seqAt(i))
				}
			}
		}


		private class MultiInsertSet[Arg, M[O] <: MappingAt[O]] private[Insert]
		                            (protected override val domain :From[M] WithParam Seq[Arg],
		                             protected override val setDomain :From[M] WithParam Arg,
		                             override val table :RelVar[M],
		                             val setOne :Seq[From[M] := WithParam.Last[Arg]], val max :Int)
			extends ParamInsert[Seq[Arg], M] with RowsInsertSet[Arg, M, Int]
		{
			protected override def setAll(setters :Seq[From[M] := WithParam.Last[Arg]]) :MultiInsertSet[Arg, M] =
				new MultiInsertSet[Arg, M](domain, setDomain, table, setOne :++ setters, max)

			protected override def set(setter :From[M] := WithParam.Last[Arg]) :MultiInsertSet[Arg, M] =
				new MultiInsertSet[Arg, M](domain, setDomain, table, setOne :+ setter, max)

			//todo: this is copy&paste from MultiUpdateWhere, if we started using WithParam.Last instead of WithParam.FromLast
			// we could generalize and extract it
			override lazy val setters :Seq[From[M] := WithParam.Last[Seq[Arg]]] =
				(0 until max).flatMap { i =>
					val scribe = SQLScribe.replaceParam(
						setDomain, domain :WithParam.Last[Seq[Arg]],
						setDomain.last.toRelationSQL, domain.last.toRelationSQL)(seqAt(i))
					setOne map { set => set.lvalue := scribe(set.rvalue) }
				}
		}


		private class EntityMultiInsertSet[S, M[O] <: BaseMapping[S, O]] private[Insert]
		              (protected override val domain :From[M] WithParam Seq[S],
		               protected override val setDomain :From[M] WithParam S, override val table :RelVar[M],
		               override val setOne :Seq[From[M] := WithParam.Last[S]], override val max :Int)
			extends MultiInsertSet[S, M](domain, setDomain, table, setOne, max)
				with EntitiesInsertSet[S, M, Int] with ParamEntitiesInsert[S, M, Int]
		{
			protected override def setAll(setters :Seq[From[M] := WithParam.Last[S]]) :EntityMultiInsertSet[S, M] =
				new EntityMultiInsertSet[S, M](domain, setDomain, table, setOne :++ setters, max)

			protected override def set(setter :From[M] := WithParam.Last[S]) :EntityMultiInsertSet[S, M] =
				new EntityMultiInsertSet[S, M](domain, setDomain, table, setOne, max)
		}


		private class SupplantedMultiInsert[S, M[O] <: BaseMapping[S, O]] private[Insert]
		              (protected override val domain :From[M] WithParam Seq[S],
		               protected override val setDomain :From[M] WithParam S, override val table :RelVar[M],
		               val overrides :Seq[From[M] := WithParam.Last[S]], val max :Int)
			extends ParamInsert[Seq[S], M] with EntitiesInsertSet[S, M, Int] with ParamEntitiesInsert[S, M, Int]
		{
			protected override def setAll(setters :Seq[From[M] := WithParam.Last[S]]) =
				new SupplantedMultiInsert[S, M](domain, setDomain, table, overrides :++ setters, max)

			protected override def set(setter :From[M] := WithParam.Last[S]) =
				new SupplantedMultiInsert[S, M](domain, setDomain, table, overrides :+ setter, max)

			override lazy val setters :Seq[From[M] := WithParam.Last[Seq[S]]] = {
				//todo: delay the split into columns until spelling, so we use the reform defined by SQLSpelling
				val singleSet = TableStatement.supplant[S, M, WithParam.Last[S]](
					InsertView, table, setDomain, ColumnSetter.inserts(setDomain), overrides
				)
				max.enumerate { i =>
					val scribe = SQLScribe.replaceParam(
							setDomain, domain :WithParam.Last[Seq[S]],
							setDomain.last.toRelationSQL, domain.last.toRelationSQL
						)(Requisite { args :Seq[S] => //this could be an optional extractor, but we don't want to set nulls
							if (args.sizeIs > i) args(i)
							else throw new IllegalArgumentException(
								s"Cannot insert ${args.length} elements with multi-insert of $max rows $this: $args."
							)
						})
					singleSet.view.map { (set :From[M] := WithParam.Last[S]) => set.lvalue := scribe(set.rvalue) }
				}.flatten
			}
		}




		sealed class InsertOne[S, M[O] <: BaseMapping[S, O]] private[Insert] (override val table :RelVar[M])
			 extends ParamInsert[S, M] with EntityInsertSet[S, M, Int] with ParamEntityInsert[S, M, Int]
			    with GenericSupplantedInsertFactory[S, M, EntityInsertSet[S, M, Int]]
		{
			protected override def supplantAll(setters :Seq[From[M] := WithParam.Last[S]]) :EntityInsertSet[S, M, Int] =
				new SupplantedInsertOne[S, M](domain, table, setters)

			protected override def setAll(setters :Seq[From[M] := WithParam.Last[S]]) :EntityInsertSet[S, M, Int] =
				new DefaultEntityInsert[S, M](domain, table, setters)

			protected override def setDomain :From[M] WithParam S = domain
			protected override val domain :From[M] WithParam S = implementation.insertDomain(table)
			override val setters :Seq[From[M] := WithParam.Last[S]] =
				PassedArray :+ (table[From[M]] := domain.last)

//			override def bind(arg :S) :Insert[(), M, Int] = new GroundInsertOne(table, arg)

//		    override def batch :InsertDML[Seq[S], M, Seq[Int]] = InsertAll.InsertMany(table)

//			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[S, Int] =
//				visitor.insertOne(this)

			//fixme: parameter component expressions from parameter relations created with the same constructors aren't equal
			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :InsertOne[_, _] => table == other.table
				case other :ParamInsert[_, _] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && setters == other.setters
				case _ => false
			}
			protected override def initHashCode :Int = table.hashCode * 31 + setters.hashCode
			protected override def initToString :String = "Insert(?) into " + table
		}


		private class SupplantedInsertOne[S, M[O] <: BaseMapping[S, O]]
		              (protected override val domain :From[M] WithParam S, override val table :RelVar[M],
		               val overrides :Seq[From[M] := WithParam.Last[S]])
			extends ParamInsert[S, M] with EntityInsertSet[S, M, Int] with ParamEntityInsert[S, M, Int]
		{
			protected override def setAll(setters :Seq[From[M] := WithParam.Last[S]]) =
				new SupplantedInsertOne[S, M](domain, table, overrides :++ setters)

			protected override def set(setter :From[M] := WithParam.Last[S]) =
				new SupplantedInsertOne[S, M](domain, table, overrides :+ setter)

			protected override def setDomain :From[M] WithParam S = domain
			override lazy val setters =
				//todo: delay the split into columns until spelling, so we use the reform defined by SQLSpelling
				TableStatement.supplant[S, M, WithParam.Last[S]](
					InsertView, table, setDomain, ColumnSetter.inserts(domain), overrides
				)
		}


		final class InsertFacade[S, M[O] <: BaseMapping[S, O]] private[Insert] (override val table :RelVar[M])
			extends InsertOne[S, M](table)
		{
			def apply(entity :S) :EntityInsert[Unit, S, M, Int] = new GroundInsertOne[S, M](table, entity)

			def apply(first :S, second :S, rest :S*) :EntityInserts[Unit, S, M, Seq[Int]] = apply(first +: second +: rest)

			def apply(entities :Seq[S]) :EntityInserts[Unit, S, M, Seq[Int]] =
				new GroundInsertSetMany[S, M](domain.left, table, entities)

			def *(max :Int) :EntityMultiInsert[S, M] = new EntityMultiInsert[S, M](domain, table, max)

			def apply[X :SQLForm] :InsertFactory[X, M] =
				new InsertFactory[X, M](domain.left.param[X])

			def using[X :SQLForm] :InsertFactory[X, M] =
				new InsertFactory[X, M](domain.left.param[X])

			def using[X](param :ParamRelation[X]) :InsertFactory[X, M] =
				new InsertFactory[X, M](domain.left param param)

			//todo: aliased domain
			def using[N <: Label, X](param :NamedParamRelation[N, X]) :InsertFactory[X, M] =
				new InsertFactory[X, M](domain.left param param)
		}




		sealed trait InsertUpdatingOne[S, M[O] <: BaseMapping[S, O]]
			extends SupplantClauseFactory[S, M, WithParam.Last[S], InsertSet[S, M, S]]
			   with InsertSet[S, M, S] with InsertReturningEntity[S, S, M]

		private[Insert] class DefaultInsertUpdatingOne[S, M[O] <: BaseMapping[S, O]] private[Insert]
		                                              (override val statement :InsertOne[S, M],
		                                               override val keys :Seq[TypedColumn[_, From[M]]],
		                                               override val columnNames :Seq[String])
			extends DefaultInsertReturningEntity[S, S, M](
				statement, keys, columnNames,
				UpdatedEntities.Single(statement.table.export[Unit].asInstanceOf[TypedMapping[S, _]], columnNames)
			) with InsertUpdatingOne[S, M]
		{
			def this(statement :InsertOne[S, M]) =
				this(statement, statement.table.export[From[M]].autoInserted.toSeq :Seq[TypedColumn[_, From[M]]],
					statement.table.export[From[M]].autoInserted.view.map(_.name).to(Seq))

			protected override def setAll(setters :Seq[From[M] := WithParam.Last[S]]) :InsertSet[S, M, S] = {
				val insert = new DefaultEntityInsert[S, M](setDomain, table, setters)
				new DefaultInsertUpdatingEntity[S, M](insert, keys, columnNames, result)
			}
			protected override def supplantAll(setters :Seq[From[M] := WithParam.Last[S]]) :InsertSet[S, M, S] =
				new DefaultInsertUpdatingEntity[S, M](
					statement `->supplantAll` setters, keys, columnNames, result
				)

			protected override def setDomain :From[M] WithParam S = statement.`->setDomain`
		}




		final class InsertMany[S, M[O] <: BaseMapping[S, O]] private[Insert] (override val dml :InsertOne[S, M])
			extends BatchEntityInsertSet[S, M, Int] with RepeatedDML[S, Int]
			   with GenericSupplantedInsertFactory[S, M, BatchEntityInsertSet[S, M, Int]]
		{
			protected override def setAll(setters :Seq[From[M] := WithParam.Last[S]]) :BatchEntityInsertSet[S, M, Int] =
				new InsertSetMany[S, M](dml `->setAll` setters)

			protected override def supplantAll(setters :Seq[From[M] := WithParam.Last[S]]) :BatchEntityInsertSet[S, M, Int] =
				new InsertSetMany[S, M](dml `->supplantAll` setters)

			def values[T](component :M[From[M]] => TypedMapping[T, From[M]]) :InsertManyValues[T, @~ ~ T, M] =
				new InsertManyValues[T, @~ ~ T, M](new InsertValue[T, M](setDomain.left, table, component(table.row)))

			protected override def setDomain :From[M] WithParam S = dml.`->setDomain`
			override val table :RelVar[M] = dml.table
		}


		private class InsertSetMany[S, M[O] <: BaseMapping[S, O]](override val dml :EntityInsertSet[S, M, Int])
			extends BatchEntityInsertSet[S, M, Int] with RepeatedDML[S, Int]
		{
			protected override def setAll(setters :Seq[From[M] := WithParam.Last[S]]) =
				new InsertSetMany[S, M](dml `->setAll` setters)

			override val table = dml.table
			protected override def setDomain = dml.`->setDomain`
		}




		//todo: this is *extremely* limiting; need an 'insert select' on column/component level
		sealed trait InsertSelectFactory[S, M[O] <: BaseMapping[S, O]] extends Any {
			protected def table :RelVar[M]

			def select(select :QuerySQL[RowProduct, S]) :GroundInsertSelect[S, M] =
				new GroundInsertSelect[S, M](table, select)

			def select[X](select :Query[X, S]) :InsertSelect[X, S, M] =
				new InsertSelect[X, S, M](table, select)
		}


		final class InsertSelect[Args, S, M[O] <: TypedMapping[S, O]] private[Insert]
		                        (override val table :RelVar[M], val select :Query[Args, S])
			extends RowsInsert[Args, M, Int]
		{
			override def result :StatementResult[Nothing, Int] = UpdateCount

			override def bind(arg :Args) :Insert[Any, M, Int] = new GroundInsertSelect(table, select.bind(arg))

			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Int] =
				visitor.insertSelect[Args, S, M](this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args] = {
				//todo: this is extremely simplistic, no reconciliation or validation
				val mapping = table.export[Unit]
				val columns = select.export[Unit].selectedByDefault
				columns.foreach { column =>
					try { //we can't really compare forms, as the one in the selectClause may be a combined r+w form
						mapping.columnNamed(column.name)
					} catch {
						case e :NoSuchElementException =>
							throw new MisspelledSQLException(
								s"Invalid DML '$this': no column named ${column.name} in the target table.", e
							)
					}
				}
				val preamble = spelling.INSERT + spelling._INTO_
				val tableSQL = spelling.table(table, "")(Dual, spelling.newContext, Dual.parameterization)
				val columnsString = columns.view.map(_.name).mkString("(", ", ", ") ")
				preamble +: tableSQL.sql +: columnsString +: spelling.spell(select)
			}

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[InsertSelect[_, _, MappingAt @unchecked]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :InsertSelect[_, _, _] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && select == other.select
				case _ => false
			}
			protected override def initHashCode :Int = table.hashCode * 31 + select.hashCode
			protected override def initToString :String = "Insert into " + table + " " + select
		}






		trait GroundInsertSet[M[O] <: MappingAt[O], +Res]
			extends GenericGroundInsertFactory[M, GroundInsertSet[M, Res]] with Insert[Any, M, Res]

		trait GroundInsertSetRow[M[O] <: MappingAt[O], +Res]
			extends GenericGroundInsertFactory[M, GroundInsertSetRow[M, Res]]
			   with GroundInsertSet[M, Res] with RowInsert[Any, M, Res]

		trait GroundInsertSetEntity[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericGroundInsertFactory[M, GroundInsertSetEntity[S, M, Res]]
			   with GroundInsertSetRow[M, Res] with GroundEntityInsert[S, M, Res]

		trait GroundInsertSetRows[M[O] <: MappingAt[O], +Res]
			extends GenericGroundInsertFactory[M, GroundInsertSetRows[M, Res]]
			   with GroundInsertSet[M, Res] with RowsInsert[Any, M, Res]

		trait GroundInsertSetEntities[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericGroundInsertFactory[M, GroundInsertSetEntities[S, M, Res]]
			   with GroundInsertSetRows[M, Res] with GroundEntitiesInsert[S, M, Res]

		trait GroundInsertSetBatch[Args, M[O] <: MappingAt[O], +Res]
			extends GenericGroundInsertFactory[M, GroundInsertSetBatch[Args, M, Res]]
			   with GroundBatchInsert[Args, M, Res]

		trait GroundInsertSetEntityBatch[S, M[O] <: BaseMapping[S, O], +Res]
			extends GenericGroundInsertFactory[M, GroundInsertSetEntityBatch[S, M, Res]]
			   with GroundBatchEntityInsert[S, M, Res] with GroundInsertSetBatch[S, M, Res]


		trait GenericGroundInsertFactory[M[O] <: MappingAt[O], +Res]
			extends Any with GroundSetClauseFactory[M, RowProduct, Res]
		{
			protected override def setDomain :RowProduct = Dual
		}

		final class GroundInsertFactory[S, M[O] <: BaseMapping[S, O]] private[Insert]
		                               (protected override val domain :From[M])
			extends AnyVal with InsertSelectFactory[S, M]
			   with GenericGroundInsertFactory[M, GroundInsertSetRow[M, Int]]
		{
			protected override def table :RelVar[M] = domain.table.castFrom[Table[M], RelVar[M]]

			override def setAll(setters :Seq[From[M] := RowProduct]) :GroundInsertSetRow[M, Int] =
				new DefaultGroundInsert[S, M](domain, table, setters)

			def values[T](component :M[From[M]] => TypedMapping[T, From[M]]) :InsertValues[T, @~ ~ T, M] =
				new InsertValue[T, M](domain, table, table.export[From[M]].export(component(table.row)))
		}



		class GroundMultiInsertFactory[S] private[Insert](private val values :Seq[S]) extends AnyVal {
			def into[M[O] <: BaseMapping[S, O]](table :RelVar[M]) :GroundMultiInsert[S, M] =
				new GroundMultiInsert[S, M](table, values)
		}


		final class GroundMultiInsert[S, M[O] <: BaseMapping[S, O]] private[Insert]
		                             (override val table :RelVar[M], override val values :Seq[S])
			extends GroundEntitiesInsert[S, M, Int] with GroundDML.Impl[Int]
			   with GroundSupplantClauseFactory[M, RowProduct, GroundInsertSetEntities[S, M, Int]]
		{
			override def supplantAll(setters :Seq[From[M] := RowProduct]) :GroundInsertSetEntities[S, M, Int] =
				new GroundSupplantedMultiInsert[S, M](domain, table, values, setters)

			if (table.export.insertedByDefault.isEmpty)
				throw new IllegalArgumentException(
					"Cannot insert into " + table + ": export mapping " + table.export
						+ " has an empty insertedByDefault column list."
				)
			if (values.isEmpty)
				throw new IllegalArgumentException("No values to insert into " + table + ".")

			protected override val domain :From[M] = From(table)
			protected override def setDomain :RowProduct = Dual

			override def result :StatementResult[Nothing, Int] = UpdateCount

/*
			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Any, RowProduct] = {
				val inInsert = spelling.inInsert
				val columns = table.export[()].insertedByDefault
				val prefix = spelling.INSERT + ' ' + spelling.INTO + ' '
				val noParams = Parameterization.paramless[From[M]]
				val tableName = spelling.table[Any, From[M], M](table, "")(SQLContext(), noParams)
				val columnNames = columns.iterator.map(_.name).mkString("(", ", ", ") " + spelling.VALUES)
				val preamble = (prefix +: tableName) + columnNames
				val valueSets = values.iterator map { value => ColumnSetter.inserts(table, value) }
				val columnCount = columns.size
				((preamble :SpelledSQL[Any, RowProduct]) /: valueSets) { (sql, setters) =>
					val start = if (sql eq preamble) sql else sql + ", "
					start + (implementation.spellValues(this, columnCount)(setters)(_, _)(inInsert))
				}
			}
*/

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Any] = {
				//todo: use InsertPreset, CustomInsert, ExtraInsert
				val columns = table.export[Unit].insertedByDefault
				val prefix = spelling.INSERT + spelling._INTO_
				val tableName = spelling.table(table, "")(Dual, spelling.newContext, Parameterization.paramless)
				val columnNames = columns.iterator.map(_.name).mkString("(", ", ", ") " + spelling.VALUES)
				val preamble = (prefix +: tableName) + columnNames
				val mapping = table.row[Unit]
				(preamble /: values) { (acc, value) =>
					val start = if (acc eq preamble) acc else acc + ", "
					(start /: columns) { (s, col) =>
						val separator = if (s eq acc) s + '(' else s + ", "
						def addParam[T](column :TypedColumn[T, Unit]) =
							mapping(column).opt(value) match {
								case Got(x) => separator +
									(spelling(BoundParam(column.form, x))(Dual, _, Parameterization.paramless))
								case _ => separator + spelling.NULL
							}
						addParam(col)
					} + ')'
				}
			}

			override def equals(that :Any) :Boolean = that match {
				case other :GroundMultiInsert[_, _] =>
					(other eq this) || other.hashCode == hashCode && other.table == table && other.values == values
				case _ => false
			}
			protected override def initHashCode :Int = table.hashCode * 31 + values.hashCode
			protected override def initToString :String = values.mkString("Insert(", ", ", ") into " + table)
		}


		private class GroundSupplantedMultiInsert[S, M[O] <: BaseMapping[S, O]]
		              (protected override val domain :From[M], override val table :RelVar[M],
		               override val values :Seq[S], val overrides :Seq[From[M] := RowProduct])
			extends GroundInsertSetEntities[S, M, Int] with GroundDML.Impl[Int]
		{
			override def setAll(setters :Seq[From[M] := RowProduct]) =
				new GroundSupplantedMultiInsert[S, M](domain, table, values, overrides :++ setters)

			override def set(setter :From[M] := RowProduct) =
				new GroundSupplantedMultiInsert[S, M](domain, table, values, overrides :+ setter)

			if (values.isEmpty)
				throw new IllegalArgumentException(
					"No values to insert into " + table + " (given overrides " + overrides + ")."
				)

			protected override def setDomain :From[M] = domain
			override def result = UpdateCount

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Any] = {
				//todo: use InsertPreset, CustomInsert, ExtraInsert
				val inInsert = spelling.inInsert
				val valueSets = values map { value =>
					TableStatement.supplant[S, M, RowProduct](
						InsertView, table, Dual, ColumnSetter.inserts(table, value), overrides
					)
				}
				val columnCount = valueSets.head.length
				val preamble = SpelledSQL(
					implementation.spellInsertColumns(this)(valueSets.head)(inInsert), spelling.newContext
				)
				((preamble :SpelledSQL[Any]) /: valueSets) { (sql, setters) =>
					val start = if (sql eq preamble) sql else sql + ", "
					start + (implementation.spellValues[Any, M, RowProduct](
						this, columnCount)(setters)(domain, _, Parameterization.paramless)(inInsert)
					)
				}
			}

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GroundSupplantedMultiInsert[_, _] =>
					other.hashCode == hashCode && other.table == table &&
						other.values == values && other.overrides == overrides
				case _ => false
			}
			protected override def initHashCode :Int = (table.hashCode * 31 + values.hashCode) * 31 + overrides.hashCode
			protected override def initToString :String =
				values.mkString("Insert(", ", ", ") into " + table) + overrides.mkString(" supplant ", ", ", "")
		}



		class GroundInsertOneFactory[S] private[Insert] (private val value :S) extends AnyVal {
			def into[M[O] <: BaseMapping[S, O]](table :RelVar[M]) :GroundInsertOne[S, M] =
				new GroundInsertOne[S, M](table, value)
		}


		final class GroundInsertOne[S, M[O] <: BaseMapping[S, O]] private[Insert]
		                           (override val table :RelVar[M], override val value :S)
			extends GroundInsert[M] with GroundDML.Impl[Int] with GroundEntityInsert[S, M, Int]
			   with GroundSupplantClauseFactory[M, RowProduct, GroundInsertSetEntity[S, M, Int]]
		{
			override def supplantAll(setters :Seq[From[M] := RowProduct]) :GroundInsertSetEntity[S, M, Int] =
				new GroundInsertSetOne[S, M](domain, table, value, PassedArray.empty)

			protected override lazy val domain :From[M] = From(table)
			protected override def setDomain :RowProduct = Dual

			override val setters :Seq[From[M] := RowProduct] = {
				val mapping = table.export.asInstanceOf[TypedMapping[S, Unit]]
				implicit val insertForm = mapping.selectForm <> mapping.insertForm
				PassedArray :+ (table[From[M]] := value.?)
			}

//			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Any, Int] =
//				visitor.insertOne[S, M](this)

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GroundInsertOne[_, _] => other.table == table && other.value == value
				case other :GroundInsert[_] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && setters == other.setters
				case _ => false
			}
			protected override def initToString :String = "Insert(" + value + ") into " + table
		}


		private final class GroundInsertSetOne[S, M[O] <: BaseMapping[S, O]]
		                                      (protected override val domain :From[M], override val table :RelVar[M],
		                                       override val value :S, val overrides :Seq[From[M] := RowProduct])
			extends GroundInsert[M] with GroundInsertSetEntity[S, M, Int] with GroundDML.Impl[Int]
		{
			override def setAll(setters :Seq[From[M] := RowProduct]) =
				new GroundInsertSetOne[S, M](domain, table, value, overrides :++ setters)

			override def set(setter :From[M] := RowProduct) =
				new GroundInsertSetOne[S, M](domain, table, value, overrides :+ setter)

			override lazy val setters :Seq[From[M] := RowProduct] =
				//todo: delay the split into columns until spelling, so we use the reform defined by SQLSpelling
				TableStatement.supplant[S, M, RowProduct](
					InsertView, table, Dual, ColumnSetter.inserts(table, value), overrides
				)

			protected override def initToString :String =
				"Insert(" + value + ") into " + table + " supplant " + overrides
		}




		//todo: this doesn't need to be BatchEntityInsertSet, EntityBatch is enough - if at all, with supplant
		private class GroundInsertSetMany[S, M[O] <: BaseMapping[S, O]]
		                                 (override val domain :From[M],
		                                  override val dml :BatchEntityInsert[S, M, Int], override val args :Seq[S])
			extends GroundInsertSetEntityBatch[S, M, Int]
			   with BoundDML[Seq[S], Seq[Int]] with BoundDML.Impl[Seq[S], Seq[Int], Insert.into[M]#DML]
		{
			def this(domain :From[M], table :RelVar[M], values :Seq[S]) =
				this(domain, new InsertSetMany[S, M](new InsertOne[S, M](table)), values)

			def this(table :RelVar[M], values :Seq[S]) = this(From(table), table, values)

			override def setAll(setters :Seq[From[M] := RowProduct]) =
				new GroundInsertSetMany[S, M](domain, dml, args)

			override val table = dml.table
		}




		final class GroundInsertSelect[S, M[O] <: TypedMapping[S, O]] private[Insert]
		                              (override val table :RelVar[M], val select :QuerySQL[RowProduct, S])
			extends RowsInsert[Any, M, Int] with GroundDML.Impl[Int]
		{
			override def result :StatementResult[Nothing, Int] = UpdateCount

			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Any, Int] =
				visitor.insertSelect[S, M](this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Any] = {
				val mapping = table.export[Unit]
				val columns = select.export[Unit].selectedByDefault
				columns.foreach { column =>
					try {
						mapping.columnNamed(column.name)
					} catch {
						case e :NoSuchElementException =>
							throw new MisspelledSQLException(
								s"Invalid DML '$this': no column named ${column.name} in the target table.", e
							)
					}
				}
				val preamble = spelling.INSERT + spelling._INTO_
				val tableSQL = spelling.table(table, "")(Dual, spelling.newContext, Dual.parameterization)
				val columnsString = columns.view.map(_.name).mkString("(", ", ", ") ")
				(preamble +: tableSQL.sql +: columnsString +: spelling.spell(select)) compose { _ => @~ }
			}

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroundInsertSelect[_, MappingAt @unchecked]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GroundInsertSelect[_, _] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && select == other.select
				case _ => false
			}
			protected override def initHashCode :Int = table.hashCode * 31 + select.hashCode
			protected override def initToString :String = "Insert into " + table + " " + select
		}
	}




	object implementation {
		/** Base trait for ''insert'' statements and batches returning either autogenerated keys,
		  * or the argument entity/entities updated with the generated keys received from the DBMS.
		  */
		trait InsertsReturning[-Args, M[O] <: MappingAt[O], +Res]
			extends InsertDML[Args, M, Res] with StatementsReturning[Args, M, Res]

		/** Base trait for single ''insert'' statements returning either autogenerated keys,
		  * or the argument entity/entities updated with the generated keys received from the DBMS.
		  */
		trait InsertReturning[-Args, M[O] <: MappingAt[O], +Res]
			extends Insert[Args, M, Res] with Returning[Args, M, Res] with InsertsReturning[Args, M, Res]
//		{
//			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
//				visitor.insertReturning(this)
//		}


		/** A base trait for ''insert'' statements for a table with mapping `M`, using a single parameter `Args`
		  * and returning the number of inserted rows. It provides the full needed implementation, leaving
		  * for subclasses to only specify
		  * [[net.noresttherein.oldsql.sql.Insert.implementation.ParamInsert.setters setter expressions]]
		  * for inserted columns and the target
		  * [[net.noresttherein.oldsql.sql.Insert.implementation.ParamInsert.table table]].
		  */
		trait ParamInsert[Args, M[O] <: MappingAt[O]] extends Insert[Args, M, Int] {

			override def result :StatementResult[Nothing, Int] = UpdateCount
			def setters :Seq[From[M] := WithParam.Last[Args]]
			protected def domain :From[M] WithParam Args

			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Int] =
				visitor.paramInsert(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args] =
				implementation.spell[@~ ~ Args, M, From[M] WithParam Args](this)(setters)(
					domain, spelling.newContext.param(""), domain.parameterization //"" is safe only because it is the only param
				) compose { @~ ~ _ }

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamInsert[_, MappingAt @unchecked]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :ParamInsert[_, _] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && setters == other.setters
				case _ => false
			}
			protected override def initHashCode :Int = table.hashCode * 31 + setters.hashCode

			protected override def initToString :String = setters.mkString("Insert into " + table + " (", ", ", ")")
		}


		private[Insert] class DefaultInsert[Arg, M[O] <: MappingAt[O]]
		                      (protected override val domain :From[M] WithParam Arg, override val table :RelVar[M],
		                       override val setters :Seq[From[M] := WithParam.Last[Arg]])
			extends ParamInsert[Arg, M] with RowInsertSet[Arg, M, Int]
		{
			protected override def setDomain :From[M] WithParam Arg = domain

			protected override def setAll(setters :Seq[From[M] := WithParam.Last[Arg]]) =
				new DefaultInsert[Arg, M](domain, table, this.setters :++ setters)

			protected override def set(setter :From[M] := WithParam.Last[Arg]) =
				new DefaultInsert[Arg, M](domain, table, this.setters :+ setter)
		}

		private[Insert] class DefaultEntityInsert[S, M[O] <: BaseMapping[S, O]]
		                      (protected override val domain :From[M] WithParam S, override val table :RelVar[M],
		                       override val setters :Seq[From[M] := WithParam.Last[S]])
			extends ParamInsert[S, M] with EntityInsertSet[S, M, Int] with ParamEntityInsert[S, M, Int]
		{
			protected override def setAll(setters :Seq[From[M] := WithParam.Last[S]]) =
				new DefaultEntityInsert[S, M](domain, table, this.setters :++ setters)

			protected override def set(setter :From[M] := WithParam.Last[S]) =
				new DefaultEntityInsert[S, M](domain, table, this.setters :+ setter)

			protected override def setDomain :From[M] WithParam S = domain
		}




		/** A base trait for parameterless ''insert'' statements (with any JDBC parameters appearing as
		  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]]), which returns the number
		  * of inserted rows. Provides full implementation for SQL generation and requires subclasses
		  * to only specify [[net.noresttherein.oldsql.sql.Insert.implementation.GroundInsert.setters setter expressions]]
		  * for inserted columns as well as the target
		  * [[net.noresttherein.oldsql.sql.Insert.implementation.GroundInsert.table table]].
		  */
		trait GroundInsert[M[O] <: MappingAt[O]] extends Insert[Any, M, Int] {

			override def result :StatementResult[Nothing, Int] = UpdateCount
			def setters :Seq[From[M] := RowProduct]

			protected override def visit[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Any, Int] =
				visitor.groundInsert(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Any] =
				implementation.spell[@~, M, ParamlessRow](this)(setters)(
					Dual, spelling.newContext, Parameterization.paramless
				).compose { _ => @~ }

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroundInsert[MappingAt @unchecked]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GroundInsert[_] if other canEqual this =>
					hashCode == other.hashCode && table == other.table && setters == other.setters
				case _ => false
			}
			protected override def initHashCode :Int = table.hashCode * 31 + setters.hashCode

			protected override def initToString :String = setters.mkString("Insert into " + table + " (", ", ", ")")
		}


		private[Insert] class DefaultGroundInsert[S, M[O] <: BaseMapping[S, O]]
		                                         (protected override val domain :From[M], override val table :RelVar[M],
		                                          override val setters :Seq[From[M] := RowProduct])
			extends GroundInsert[M] with GroundDML.Impl[Int] with RowInsert[Any, M, Int] with GroundInsertSetRow[M, Int]
		{
			override def setAll(setters :Seq[From[M] := RowProduct]) :DefaultGroundInsert[S, M] =
				new DefaultGroundInsert(domain, table, this.setters :++ setters)

			override def set(setter :From[M] := RowProduct) :DefaultGroundInsert[S, M] =
				new DefaultGroundInsert(domain, table, this.setters :+ setter)
		}



		private[Insert] final class DefaultInsertUpdatingEntity[S, M[O] <: BaseMapping[S, O]]
		                            (override val statement :InsertSet[S, M, Any],
		                             override val keys :Seq[TypedMapping[_, From[M]]],
		                             override val columnNames :Seq[String],
		                             override val result :StatementResult[S, S])
			extends DefaultInsertReturningEntity[S, S, M](statement, keys, columnNames, result)
			   with InsertSet[S, M, S]
		{
			def this(statement :DefaultEntityInsert[S, M], keys :Seq[TypedMapping[_, From[M]]], columns :Seq[String]) =
				this(statement, keys, columns, {
					val mapping = statement.table.export[Unit].asInstanceOf[TypedMapping[S, Unit]]
					UpdatedEntities.Single(mapping, columns)
				})

			def this(statement :DefaultEntityInsert[S, M]) =
				this(statement, statement.table.export[From[M]].autoInserted.toSeq,
					statement.table.export[Unit].autoInserted.view.map(_.name).to(Seq))

			protected override  def setAll(setters :Seq[From[M] := WithParam.Last[S]]) =
				new DefaultInsertUpdatingEntity[S, M](statement `->setAll` setters, keys, columnNames, result)

			protected override def setDomain = statement.`->setDomain`
		}




		private[Insert] class DefaultBatchInsert[Args, M[O] <: MappingAt[O]]
		                                        (override val dml :RowInsertSet[Args, M, Int])
			extends BatchInsertSet[Args, M, Int] with RepeatedDML[Args, Int]
		{
			def this(domain :From[M] WithParam Args, table :RelVar[M], setters :Seq[From[M] := WithParam.Last[Args]]) =
				this(new DefaultInsert[Args, M](domain, table, setters))

			protected override def setAll(setters :Seq[From[M] := WithParam.Last[Args]]) :BatchInsertSet[Args, M, Int] =
				new DefaultBatchInsert[Args, M](dml `->setAll` setters)

			override val table = dml.table
			protected override def setDomain = dml.`->setDomain`
		}




		/** Fragment of the [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] visitor
		  * covering the cases of all ''insert'' statements (subclasses of [[net.noresttherein.oldsql.sql.Insert Insert]],
		  * not just [[net.noresttherein.oldsql.sql.InsertDML InsertDML]]) as they are more generic DML rather than
		  * single-execution statements). This includes empty methods for every existing type in the hierarchy,
		  * abstract or concrete. Note that instances of standard adapter classes,
		  * such as [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement BoundStatement]], which extend also
		  * [[net.noresttherein.oldsql.sql.Insert Insert]], will not be handled by any of these methods, but rather
		  * into the more generic ones declared by `StatementVisitor` itself to better reflect their nature -
		  * in their case, conforming to `Insert` serves only a declarative purpose.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  * @see [[net.noresttherein.oldsql.sql.Insert.implementation.MatchInsert]]
		  * @see [[net.noresttherein.oldsql.sql.Insert.implementation.CaseInsert]]
		  */
		trait InsertVisitor[R[-X, +Y]] /*extends AnyInsertReturningVisitor[R]*/ {
//			def insertOne[S, M[O] <: BaseMapping[S, O]](stmt :InsertOne[S, M])                      :R[S, Int]
//			def insertOne[S, M[O] <: BaseMapping[S, O]](stmt :GroundInsertOne[S, M])                :R[Any, Int]
//			def insertValues[X, M[O] <: MappingAt[O]](stmt :InsertValues[X, M])                     :R[X, Int]
//			def insertValues[M[O] <: MappingAt[O]](stmt :GroundInsertValues[M])                     :R[Any, Int]

			def paramInsert[X, M[O] <: MappingAt[O]](stmt :ParamInsert[X, M])                       :R[X, Int]
			def groundInsert[M[O] <: MappingAt[O]](stmt :GroundInsert[M])                           :R[Any, Int]

			def insertSelect[X, S, M[O] <: TypedMapping[S, O]](stmt :InsertSelect[X, S, M])       :R[X, Int]
			def insertSelect[S, M[O] <: TypedMapping[S, O]](stmt :GroundInsertSelect[S, M])       :R[Any, Int]

//			def insertReturningNone[X, M[O] <: MappingAt[O], Y](stmt :InsertReturningNone[X, M, Y]) :R[X, Y]
//			def insertReturningOne[X, M[O] <: MappingAt[O], K](stmt :InsertReturningOne[X, M, K])   :R[X, K]
//			def insertReturningChain[X, M[O] <: MappingAt[O], K, G <: Chain]
//			                        (stmt :InsertReturningChain[X, M, K, G])                        :R[X, G ~ K]

			def insert[X, M[O] <: MappingAt[O], Y](stmt :Insert[X, M, Y])                           :R[X, Y]
		}

		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitors''
		  * of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It implements all methods
		  * of [[net.noresttherein.oldsql.sql.Insert.implementation.InsertVisitor InsertVisitor]] for concrete
		  * [[net.noresttherein.oldsql.sql.Insert Insert]] implementations by delegating them to the methods for their
		  * base traits, leaving unimplemented only the cases for direct `Insert` subtypes
		  * [[net.noresttherein.oldsql.sql.Insert.implementation.GroundInsert GroundInsert]],
		  * [[net.noresttherein.oldsql.sql.Insert.implementation.ParamInsert ParamInsert]],
		  * [[net.noresttherein.oldsql.sql.Insert.syntax.InsertSelect InsertSelect]],
		  * [[net.noresttherein.oldsql.sql.Insert.syntax.GroundInsertSelect GroundInsertSelect]],
		  * [[net.noresttherein.oldsql.sql.Insert.syntax.InsertReturning InsertReturning]] and `Insert` itself
		  * (for custom extensions not derived from any of the existing implementations).
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  * @see [[net.noresttherein.oldsql.sql.Insert.implementation.CaseInsert]]
		  */
		trait MatchInsert[R[-X, +Y]] extends InsertVisitor[R] /*with CaseAnyInsertReturning[R]*/ {
//			override def insertOne[S, M[O] <: BaseMapping[S, O]](stmt :InsertOne[S, M])       :R[S, Int] =
//				paramInsert(stmt)
//			override def insertValues[X, M[O] <: MappingAt[O]](stmt :InsertValues[X, M])      :R[X, Int] =
//				paramInsert(stmt)
//
//			override def insertOne[S, M[O] <: BaseMapping[S, O]](stmt :GroundInsertOne[S, M]) :R[Any, Int] =
//				groundInsert(stmt)
//			override def insertValues[M[O] <: MappingAt[O]](stmt :GroundInsertValues[M])      :R[Any, Int] =
//				groundInsert(stmt)
		}

		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitors''
		  * of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It expands on
		  * [[net.noresttherein.oldsql.sql.Insert.implementation.MatchInsert MatchAnyInsert]] by further delegating
		  * the remaining open cases to the method for [[net.noresttherein.oldsql.sql.Insert Insert]] trait itself.
		  * CaseAnys for concrete subclasses dispatch still to their immediate base type, making the delegation
		  * a multi-step affair and allowing to override on the chosen level.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  */
		trait CaseInsert[R[-X, +Y]] extends MatchInsert[R] {
			override def paramInsert[X, M[O] <: MappingAt[O]](stmt :ParamInsert[X, M]) :R[X, Int]   = insert(stmt)
			override def groundInsert[M[O] <: MappingAt[O]](stmt :GroundInsert[M])     :R[Any, Int] = insert(stmt)

			override def insertSelect[X, S, M[O] <: TypedMapping[S, O]](stmt :InsertSelect[X, S, M]) :R[X, Int] =
				insert(stmt)
			override def insertSelect[S, M[O] <: TypedMapping[S, O]](stmt :GroundInsertSelect[S, M]) :R[Any, Int] =
				insert(stmt)

//			override def insertReturning[X, M[O] <: MappingAt[O], Y]
//			                            (stmt :InsertReturning[X, M, Y]) :R[X, Y] =
//				insert(stmt)
		}

/*
		trait AnyInsertReturningVisitor[R[-X, +Y]] {
			def insertReturningKey[X, M[O] <: MappingAt[O], K <: Chain, Y]
			                      (stmt :InsertReturningKey[X, M, K, Y])                                         :R[X, Y]
			def insertReturningKeys[X, M[O] <: MappingAt[O], K <: Chain, Y]
			                       (stmt :InsertReturningKeys[X, M, K, Y])                                       :R[X, Seq[Y]]

			def insertReturningEntity[X, S, M[O] <: BaseMapping[S, O]](stmt :InsertReturningEntity[X, S, M])     :R[X, S]
			def insertReturningEntities[X, S, M[O] <: BaseMapping[S, O]](stmt :InsertReturningEntities[X, S, M]) :R[X, Seq[S]]

			def insertReturning[X, M[O] <: MappingAt[O], Y](stmt :InsertReturning[X, M, Y])                      :R[X, Y]
		}
*/

		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitor''
		  * delegating cases for all individual implementations
		  * of [[net.noresttherein.oldsql.sql.Insert.syntax.InsertReturningKey InsertReturningKey]], dedicated to different
		  * composite type with the generated keys, to the one for `InsertReturningKey`.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  * @see [[net.noresttherein.oldsql.sql.Insert.implementation.CaseAnyInsertReturning]]
		  * @see [[net.noresttherein.oldsql.sql.Insert.implementation.MatchInsert]]
		  */
//		trait MatchAnyInsertReturning[R[-X, +Y]] extends InsertVisitor[R] {
//			override def insertReturningNone[X, M[O] <: MappingAt[O], Y](stmt :InsertReturningNone[X, M, Y]) :R[X, Y] =
//				InsertReturningKey(stmt)
//			override def insertReturningOne[X, M[O] <: MappingAt[O], K](stmt :InsertReturningOne[X, M, K]) :R[X, K] =
//				InsertReturningKey(stmt)
//			override def insertReturningChain[X, M[O] <: MappingAt[O], K, G <: Chain](stmt :InsertReturningChain[X, M, K, G]) =
//				InsertReturningKey(stmt)
//		}

/*
		trait CaseAnyInsertReturning[R[-X, +Y]] extends InsertVisitor[R] {
			override def insertReturningKey[X, M[O] <: MappingAt[O], K <: Chain, Y](stmt :InsertReturningKey[X, M, K, Y]) =
				insertReturning(stmt)
			override def insertReturningKeys[X, M[O] <: MappingAt[O], K <: Chain, Y](stmt :InsertReturningKeys[X, M, K, Y]) =
				insertReturning(stmt)

			override def insertReturningEntity[X, S, M[O] <: BaseMapping[S, O]](stmt :InsertReturningEntity[X, S, M]) =
				insertReturning(stmt)
			override def insertReturningEntities[X, S, M[O] <: BaseMapping[S, O]](stmt :InsertReturningEntities[X, S, M]) =
				insertReturning(stmt)
		}
*/




		private[Insert] def insertDomain[S, M[O] <: BaseMapping[S, O]](table :Table[M]) :From[M] WithParam S = {
			val mapping = table.export.asInstanceOf[TypedMapping[S, Unit]]
			From(table).param[S](mapping.selectForm <> mapping.insertForm)
		}

		private[Insert] def bind[M[O] <: MappingAt[O], X]
		                        (base :From[M] WithParam X, setters :Seq[From[M] := WithParam.Last[X]], arg :X)
				:Seq[From[M] := RowProduct] =
		{
			val binder = SQLScribe.applyParam(base, base.left :RowProduct, arg)
			setters.map { (update :From[M] := (RowProduct AndFrom ParamRelation[X]#Param)) =>
				update.lvalue := binder(update.rvalue)
			}
		}


		//consider: making the validations earlier, at class creation
		private[Insert] def spell[X, M[O] <: MappingAt[O], F <: RowProduct { type Params <: X }]
		                         (self :Insert[Nothing, M, Any])(setters :Seq[From[M] := F])
		                         (domain :F, context :SQLContext[X], params :Parameterization[X, F])
		                         (implicit spelling :SQLSpelling) :SpelledSQL[X] =
		{
			//todo: use InsertPreset, CustomInsert, ExtraInsert
			val spell = spelling.inInsert
			val reformed = setters.map(_.reform(spell)) //consider: eliminating the duplicates
			val columnCount = (0 /: reformed) { (count, update) =>
				count + spell.columnCount(update.lvalue)
			}
			val preamble = spellInsertColumns[X, M, F](self)(reformed)(spell)
			val valuesSQL = spellValues[X, M, F](self, columnCount)(reformed)(domain, context, params)(spell)
			preamble +: valuesSQL
		}

		private[Insert] def spellInsertColumns[X, M[O] <: MappingAt[O], F <: RowProduct { type Params <: X }]
		                                      (self :Insert[Nothing, M, Any])(setters :Seq[From[M] := F])
		                                      (implicit spelling :SQLSpelling) :ChunkedString =
		{
			val mapping = self.table.export[From[M]]
			val preamble = ChunkedString(spelling.INSERT + spelling._INTO_)
			val tableSQL = spelling.table(self.table, "")(Dual, spelling.newContext, Dual.parameterization)

			val columns = setters.flatMap { update =>
				val lcolumns = spelling.scope.defaultColumns(update.lvalue.component.anchored)
				val rightColumnCount = spelling.columnCount(update.rvalue)
				//setters are already reformed and most likely columns, so this is more of an assertion than a full test
				if (lcolumns.size != rightColumnCount)
					throw new MismatchedExpressionsException(
						s"Illegal DML '$self' - differing number of columns between the left and right " +
						s"sides of assignment $update: $lcolumns (${lcolumns.size}) vs. $rightColumnCount."
					)
				lcolumns.view.map { col :TypedColumn[_, _] => mapping.export(col.withOrigin[From[M]]) }
			}
			if (columns.isEmpty)
				throw new MisspelledSQLException(s"Illegal DML '$self': cannot insert zero columns.")
			val columnsString = columns.iterator.map(_.name).mkString("(", ", ", ") " + spelling.VALUES + " ")
			preamble + tableSQL.sql + columnsString
		}

		private[Insert] def spellValues[X, M[O] <: MappingAt[O], F <: RowProduct { type Params <: X }]
		                               (self :Insert[Nothing, M, Any], columnCount :Int)(setters :Seq[From[M] := F])
		                               (domain :F, context :SQLContext[X], params :Parameterization[X, F])
		                               (implicit spelling :SQLSpelling) :SpelledSQL[X] =
		{
			val values = setters.flatMap {
				update => spelling.explode(update.rvalue, false)(domain, context, params)
			}
			if (values.isEmpty)
				throw new MisspelledSQLException(s"Illegal DML '$self': cannot insert zero columns.")
			if (values.length != columnCount) //this is more of an assertion, as we've reformed the setters earlier.
				throw new MisspelledSQLException(
					s"Illegal DML '$self': number of values $values differs from the expected column number $columnCount."
				)
			"(" +: (values.reduce(_ + ", " + _) + ")")
		}
	}

}


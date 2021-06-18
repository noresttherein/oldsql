package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.{Chain, ReversedList}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.generic.Self
import net.noresttherein.oldsql.schema.{RelVar, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.forms.{ChainForm, ChainReadForm}
import net.noresttherein.oldsql.schema.SQLReadForm
import net.noresttherein.oldsql.sql.DML.{BoundDML, RepeatedDML}
import net.noresttherein.oldsql.sql.DMLStatement.{StatementResult, StatementVisitor}
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult.{GeneratedKeys, UpdatedEntities}
import net.noresttherein.oldsql.sql.Returning.syntax.{BatchReturningEntitiesClause, BatchReturningTuplesClause, GenericReturningEntitiesClause, GroundBatchReturningEntitiesClause, GroundBatchReturningTuplesClause, ReturningClause, ReturningEntitiesClause, ReturningEntitiesClauses, ReturningEntityClause, ReturningTupleClause, ReturningTuplesClause, ReturningTuplesClausesTemplate}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.TableStatement.{ReturningClauseExpansion, ReturningClauseFactory, UpdatingClauseFactory}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.Returning.implementation.{DefaultBatchReturningEntities, DefaultBatchReturningTuples, DefaultGroundBatchReturningEntities, DefaultGroundBatchReturningTuples, DefaultReturningEntities, DefaultReturningEntity, DefaultReturningTuples, ReturningTupleClauseExpansion, ReturningTupleSingleton}






/**
  * @author Marcin Mościcki
  */
trait StatementsReturning[-Args, M[O] <: MappingAt[O], +Res] extends TableDML[Args, M, Res] {
	def statement   :TableStatement[Nothing, M, Any]
	def columnNames :Seq[String]
	def keys        :Seq[RefinedMapping[_, From[M]]]

	override def canEqual(that :Any) :Boolean =
		that.isInstanceOf[StatementsReturning[_, MappingAt @unchecked, _]]
	//			that.isInstanceOf[ReturningTuple[_, MappingAt @unchecked, _, _]]
}






trait Returning[-Args, M[O] <: MappingAt[O], +Res]
	extends TableStatement[Args, M, Res] with StatementsReturning[Args, M, Res]
{
	override def statement :TableStatement[Nothing, M, Any]

	protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] = ??? //todo:

//		protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args, RowProduct] =
//			spelling.spell(statement)
//
	protected override def doChant(implicit dialect :SQLDialect) :Incantation[Args, Res] =
		dialect(this) //fixme: must use Incantation.returning

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Returning[_, MappingAt @unchecked, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :Returning[_, _, _] if canEqual(that) && other.canEqual(this) =>
			hashCode == other.hashCode &&
				statement == other.statement && columnNames == other.columnNames && result == other.result
		case _ => false
	}
	protected override def initHashCode :Int = statement.hashCode * 31 + columnNames.hashCode//result.hashCode
}






object Returning {

	trait StatementsReturningTuples[-Args, M[O] <: MappingAt[O], +Keys <: Chain, +Res]
		extends StatementsReturning[Args, M, Res]

	trait StatementsReturningTupleSeq[-Args, M[O] <: MappingAt[O], +Keys <: Chain, +Res]
		extends StatementsReturningTuples[Args, M, Keys, Seq[Res]]

	trait ReturningQuantifiedTuples[-Args, M[O] <: MappingAt[O], +Keys <: Chain, +Res, Q[+_]] //or StatementReturningTuples?
		extends Returning[Args, M, Q[Res]] with StatementsReturningTuples[Args, M, Keys, Q[Res]]
	{
		def tupleForm :ChainReadForm[Keys]
		def list[E >: Res](entities :Q[E]) :Seq[E]
		def toReturningTuples :ReturningTuples[Args, M, Keys, Res]

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Q[Res]] =
			visitor.tuples(this)

		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[ReturningQuantifiedTuples[_, M @unchecked, _, _, Q @unchecked]]
	}

	trait ReturningTuple[-Args, M[O] <: MappingAt[O], +Keys <: Chain, +Res]
		extends ReturningQuantifiedTuples[Args, M, Keys, Res, Self]
	{
		override def list[E >: Res](entities :E) :Seq[E] = entities::Nil

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] = visitor.tuple(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ReturningTuple[_, M @unchecked, _, _]]

		protected override def initToString :String =
			columnNames.mkString(statement.toString + " returning (", ", ", ")")
	}

	trait ReturningTuples[-Args, M[O] <: MappingAt[O], +Keys <: Chain, +Res]
		extends ReturningQuantifiedTuples[Args, M, Keys, Res, Seq] with StatementsReturningTupleSeq[Args, M, Keys, Res]
	{
		override def list[E >: Res](entities :Seq[E]) :Seq[E] = entities
		override def toReturningTuples :this.type = this

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Seq[Res]] = visitor.tuples(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ReturningTuples[_, M @unchecked, _, _]]

		protected override def initToString :String =
			columnNames.mkString(statement.toString + " returning (", ", ", ")*")
	}

	trait BatchReturningTuples[-Args, M[O] <: MappingAt[O], +Keys <: Chain, +Res]
		extends StatementsReturningTupleSeq[Seq[Args], M, Keys, Res]

	trait GroundBatchReturningTuples[M[O] <: MappingAt[O], +Keys <: Chain, +Res]
		extends StatementsReturningTupleSeq[Any, M, Keys, Res]




	trait StatementsReturningEntities[-Args, S, M[O] <: BaseMapping[S, O], +Res]
		extends StatementsReturning[Args, M, Res]

	trait StatementsReturningEntitySeq[-Args, S, M[O] <: BaseMapping[S, O]]
		extends StatementsReturningEntities[Args, S, M, Seq[S]]

	trait ReturningQuantifiedEntities[-Args, S, M[O] <: BaseMapping[S, O], Q[+_]]
		extends Returning[Args, M, Q[S]] with StatementsReturningEntities[Args, S, M, Q[S]]
	{
		def list(entities :Q[S]) :Seq[S]
		def toReturningEntities :ReturningEntities[Args, S, M]

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Q[S]] = visitor.entities(this)
	}

	trait ReturningEntity[-Args, S, M[O] <: BaseMapping[S, O]] extends ReturningQuantifiedEntities[Args, S, M, Self] {

		override def list(entities :S) :Seq[S] = entities::Nil

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, S] = visitor.entity(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ReturningEntity[_, S @unchecked, M @unchecked]]
		protected override def initToString :String =
			columnNames.mkString(statement.toString + " updating (", ", ", ")")
	}

	trait ReturningEntities[-Args, S, M[O] <: BaseMapping[S, O]]
		extends ReturningQuantifiedEntities[Args, S, M, Seq] with StatementsReturningEntitySeq[Args, S, M]
	{
		override def list(entities :Seq[S]) :Seq[S] = entities
		override def toReturningEntities :this.type = this

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Seq[S]] = visitor.entities(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ReturningEntities[_, S @unchecked, M @unchecked]]
		protected override def initToString :String =
			columnNames.mkString(statement.toString + " updating (", ", ", ")*")
	}

	trait BatchReturningEntities[-Args, S, M[O] <: BaseMapping[S, O]]
		extends StatementsReturningEntitySeq[Seq[Args], S, M]

	trait GroundBatchReturningEntities[S, M[O] <: BaseMapping[S, O]]
		extends StatementsReturningEntitySeq[Any, S, M]




	object syntax {

		trait GenericRowStatements[-Args, M[O] <: MappingAt[O], +Res, +Q[+_]] extends TableDML[Args, M, Res]
			with ReturningClauseFactory[M, ({ type C[X] = ReturningTuplesClauses[Args, M, @~ ~ X, X, Q] })#C]

		trait RowStatement[-Args, M[O] <: MappingAt[O], +Res]
			extends TableStatement[Args, M, Res] with GenericRowStatements[Args, M, Res, Self]
			   with ReturningClauseFactory[M, ({ type C[X] = ReturningTupleClause[Args, M, @~ ~ X, X] })#C]
		{
			override def returning[T](key :M[From[M]] => RefinedMapping[T, From[M]])
					:ReturningTupleClause[Args, M, @~ ~ T, T] =
				ReturningTupleClause[Args, M, T](this)(key)
		}

		trait RowsStatement[-Args, M[O] <: MappingAt[O], +Res]
			extends TableStatement[Args, M, Res] with GenericRowStatements[Args, M, Res, Seq]
			   with ReturningClauseFactory[M, ({ type C[X] = ReturningTuplesClause[Args, M, @~ ~ X, X] })#C]
		{
			override def returning[T](key :M[From[M]] => RefinedMapping[T, From[M]])
					:ReturningTuplesClause[Args, M, @~ ~ T, T] =
				new DefaultReturningTuples[Args, M, @~ ~ T, T](
					ReturningTupleClause[Args, M, T](this)(key)
				)
		}

		trait RowsBatch[-Args, M[O] <: MappingAt[O], +Res] extends GenericRowStatements[Seq[Args], M, Seq[Res], Seq]
			with ReturningClauseFactory[M, ({ type C[X] = BatchReturningTuplesClause[Args, M, @~ ~ X, X] })#C]
		{
			override def returning[T](key :M[From[M]] => RefinedMapping[T, From[M]])
					:BatchReturningTuplesClause[Args, M, @~ ~ T, T] =
				new DefaultBatchReturningTuples[Args, M, @~ ~ T, T](ReturningTupleClause[Args, M, T](dml)(key))

			protected def dml :TableStatement[Args, M, Any]
		}

		trait GroundRowsBatch[Args, M[O] <: MappingAt[O], +Res] extends GenericRowStatements[Any, M, Seq[Res], Seq]
			with ReturningClauseFactory[M, ({ type C[X] = GroundBatchReturningTuplesClause[Args, M, @~ ~ X, X] })#C]
		{
			override def returning[T](key :M[From[M]] => RefinedMapping[T, From[M]])
					:GroundBatchReturningTuplesClause[Args, M, @~ ~ T, T] =
				new DefaultGroundBatchReturningTuples[Args, M, @~ ~ T, T](dml returning key, args)

			protected def dml  :RowsBatch[Args, M, Res]
			protected def args :Seq[Args]
		}



		trait EntityStatementsTemplate[-Args, M[O] <: MappingAt[O], +Res, +U]
			extends TableDML[Args, M, Res] with UpdatingClauseFactory[M, U]
		{
			def updating :U = updatingKeys(table.export[From[M]].autoUpdated.toSeq)

			override def updating(keys :Seq[M[From[M]] => RefinedMapping[_, From[M]]]) :U =
				updatingKeys(keys.map(_(table.row)))
		}

		trait GenericEntityStatements[-Args, S, M[O] <: BaseMapping[S, O], +Res, +Q[+_]]
			extends GenericRowStatements[Args, M, Res, Q]
				with EntityStatementsTemplate[Args, M, Res, ReturningEntitiesClauses[Args, S, M, Q[S]]]

		trait EntityStatement[-Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends RowStatement[Args, M, Res] with GenericEntityStatements[Args, S, M, Res, Self]
			   with EntityStatementsTemplate[Args, M, Res, ReturningEntityClause[Args, S, M]]
		{
			override def updatingKeys(keys :Seq[RefinedMapping[_, From[M]]]) :ReturningEntityClause[Args, S, M] =
				new DefaultReturningEntity[Args, S, M](
					this, keys, updatedResult(implementation.columnNames(table, keys))
				)
			protected def updatedResult(columnNames :Seq[String]) :StatementResult[S, S]
		}

		trait EntitiesStatement[-Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends RowsStatement[Args, M, Res] with GenericEntityStatements[Args, S, M, Res, Seq]
			   with EntityStatementsTemplate[Args, M, Res, ReturningEntitiesClause[Args, S, M]]
		{
			override def updatingKeys(keys :Seq[RefinedMapping[_, From[M]]]) :ReturningEntitiesClause[Args, S, M] =
				new DefaultReturningEntities[Args, S, M](
					this, keys, updatedResult(implementation.columnNames(table, keys))
				)
			protected def updatedResult(columnNames :Seq[String]) :StatementResult[S, Seq[S]]
		}

		trait EntitiesBatch[-Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends RowsBatch[Args, M, Res] with GenericEntityStatements[Seq[Args], S, M, Seq[Res], Seq]
				with EntityStatementsTemplate[Seq[Args], M, Seq[Res], BatchReturningEntitiesClause[Args, S, M]]
		{
			override def updatingKeys(keys :Seq[RefinedMapping[_, From[M]]]) :BatchReturningEntitiesClause[Args, S, M] =
				new DefaultBatchReturningEntities[Args, S, M](
					new DefaultReturningEntity[Args, S, M](
						dml, keys, updatedResult(implementation.columnNames(table, keys))
					)
				)

			protected def dml :TableStatement[Args, M, Any]

			protected def updatedResult(columnNames :Seq[String]) :StatementResult[S, S] =
				UpdatedEntities.Single(table.export.asInstanceOf[RefinedMapping[S, _]], columnNames)
		}

		trait GroundEntitiesBatch[Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends GroundRowsBatch[Args, M, Res] with GenericEntityStatements[Any, S, M, Seq[Res], Seq]
			   with EntityStatementsTemplate[Any, M, Seq[Res], GroundBatchReturningEntitiesClause[S, M]]
		{
			override def updatingKeys(keys :Seq[RefinedMapping[_, From[M]]]) :GroundBatchReturningEntitiesClause[S, M] =
				new DefaultGroundBatchReturningEntities[Args, S, M](dml updatingKeys keys, args)

			protected override def dml :EntitiesBatch[Args, S, M, Res]
		}



		def ReturningTupleClause[Args, M[O] <: MappingAt[O], Res](statement :TableStatement[Args, M, Any])
		                                                         (key :M[From[M]] => RefinedMapping[Res, From[M]])
				:ReturningTupleClause[Args, M, @~ ~ Res, Res] =
			ReturningTupleClause(statement, key(statement.table.row))

		def ReturningTupleClause[Args, M[O] <: MappingAt[O], Res]
		                        (statement :TableStatement[Args, M, Any], key :RefinedMapping[Res, From[M]])
				:ReturningTupleClause[Args, M, @~ ~ Res, Res] =
			new ReturningTupleSingleton[Args, M, Res](statement, key)
				with ReturningTupleClauseExpansion[Args, M, @~ ~ Res, Res]




		trait StatementsReturningClauses[-Args, M[O] <: MappingAt[O], +Res]
			extends StatementsReturning[Args, M, Res] with ReturningClauseExpansion[M]

		trait ReturningClauses[-Args, M[O] <: MappingAt[O], +Res] extends StatementsReturningClauses[Args, M, Res] {
			override type x[T] <: ReturningClauses[Args, M, Res]
		}

		trait ReturningClause[-Args, M[O] <: MappingAt[O], +Res]
			extends Returning[Args, M, Res] with ReturningClauses[Args, M, Res]
			   with StatementsReturningClauses[Args, M, Res]
		{
			override type x[T] <: ReturningClause[Args, M, Res]
		}



		trait ReturningTuplesClausesTemplate[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res,
		                                     +Stmt <: TableStatement[Nothing, M, Any]]
			extends StatementsReturningTuples[Args, M, Keys, Res] with StatementsReturningClauses[Args, M, Res]
		{
			type tuple[_ <: Chain]
			override type x[T] = tuple[Keys ~ T]
			override def statement :Stmt
		}

		trait ReturningTuplesClauses[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res, +Q[+_]]
			extends ReturningTuplesClausesTemplate[Args, M, Keys, Q[Res], TableStatement[Nothing, M, Any]]
		{
			override type tuple[X <: Chain] <: ReturningTuplesClauses[Args, M, X, X, Q]
		}

		trait ReturningTupleSeqClauses[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends StatementsReturningTupleSeq[Args, M, Keys, Res] with ReturningTuplesClauses[Args, M, Keys, Res, Seq]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Seq[Res], TableStatement[Nothing, M, Any]]
		{
			override type tuple[X <: Chain] <: ReturningTupleSeqClauses[Args, M, X, X]
		}

		trait ReturningQuantifiedTuplesClause[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res, Q[+_]]
			extends ReturningQuantifiedTuples[Args, M, Keys, Res, Q] with ReturningTuplesClauses[Args, M, Keys, Res, Q]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Q[Res], TableStatement[Nothing, M, Any]]
		{
			override type tuple[X <: Chain] <: ReturningQuantifiedTuplesClause[Args, M, X, X, Q]
		}

		trait ReturningTupleClause[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends ReturningTuple[Args, M, Keys, Res] with ReturningQuantifiedTuplesClause[Args, M, Keys, Res, Self]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Res, TableStatement[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: ReturningTupleClause[Args, M, X, X]

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args, RowProduct] =
				spelling.spell(statement)
		}

		trait ReturningTuplesClause[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends ReturningTuples[Args, M, Keys, Res] with ReturningQuantifiedTuplesClause[Args, M, Keys, Res, Seq]
			   with ReturningTupleSeqClauses[Args, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Args, M, Keys, Seq[Res], TableStatement[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: ReturningTuplesClause[Args, M, X, X]

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args, RowProduct] =
				spelling.spell(statement)
		}

		trait BatchReturningTuplesClause[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends BatchReturningTuples[Args, M, Keys, Res] with ReturningTupleSeqClauses[Seq[Args], M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Seq[Args], M, Keys, Seq[Res], TableStatement[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: BatchReturningTuplesClause[Args, M, X, X]
		}

		trait GroundBatchReturningTuplesClause[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends GroundBatchReturningTuples[M, Keys, Res] with ReturningTupleSeqClauses[Any, M, Keys, Res]
			   with ReturningTuplesClausesTemplate[Any, M, Keys, Seq[Res], TableStatement[Args, M, Any]]
		{
			override type tuple[X <: Chain] <: GroundBatchReturningTuplesClause[Args, M, X, X]
		}



		trait GenericReturningEntitiesClause[-Args, S, M[O] <: BaseMapping[S, O], +Res,
		                                     +Stmt <: TableStatement[Nothing, M, Any], +C]
			extends StatementsReturningClauses[Args, M, Res]
		{
			override type x[T] <: C
			override def statement :Stmt
		}

		trait ReturningEntitiesClauses[-Args, S, M[O] <: BaseMapping[S, O], +Res]
			extends StatementsReturningEntities[Args, S, M, Res] with ReturningClauses[Args, M, Res]
			   with GenericReturningEntitiesClause[Args, S, M, Res, TableStatement[Nothing, M, Any],
			                                       ReturningEntitiesClauses[Args, S, M, Res]]

		trait ReturningEntitySeqClauses[-Args, S, M[O] <: BaseMapping[S, O]]
			extends StatementsReturningEntitySeq[Args, S, M] with ReturningEntitiesClauses[Args, S, M, Seq[S]]
			   with GenericReturningEntitiesClause[Args, S, M, Seq[S], TableStatement[Nothing, M, Any],
			                                       ReturningEntitySeqClauses[Args, S, M]]

		trait ReturningQuantifiedEntitiesClause[-Args, S, M[O] <: BaseMapping[S, O], Q[+_]]
			extends ReturningQuantifiedEntities[Args, S, M, Q] with ReturningEntitiesClauses[Args, S, M, Q[S]]
			   with GenericReturningEntitiesClause[Args, S, M, Q[S], TableStatement[Nothing, M, Any],
			                                       ReturningQuantifiedEntitiesClause[Args, S, M, Q]]

		trait ReturningEntityClause[-Args, S, M[O] <: BaseMapping[S, O]]
			extends ReturningEntity[Args, S, M] with ReturningClause[Args, M, S]
			   with ReturningQuantifiedEntitiesClause[Args, S, M, Self]
			   with GenericReturningEntitiesClause[Args, S, M, S,
			                                       TableStatement[Args, M, Any], ReturningEntityClause[Args, S, M]]


		trait ReturningEntitiesClause[-Args, S, M[O] <: BaseMapping[S, O]]
			extends ReturningEntities[Args, S, M] with ReturningClause[Args, M, Seq[S]]
			   with ReturningQuantifiedEntitiesClause[Args, S, M, Seq] with ReturningEntitySeqClauses[Args, S, M]
			   with GenericReturningEntitiesClause[Args, S, M, Seq[S],
			                                       TableStatement[Args, M, Any], ReturningEntitiesClause[Args, S, M]]

		trait BatchReturningEntitiesClause[-Args, S, M[O] <: BaseMapping[S, O]]
			extends BatchReturningEntities[Args, S, M] with ReturningEntitySeqClauses[Seq[Args], S, M] //with ReturningClauses[Seq[Args], M, Seq[S]]
			   with GenericReturningEntitiesClause[Seq[Args], S, M, Seq[S], TableStatement[Args, M, Any],
			                                       BatchReturningEntitiesClause[Args, S, M]]

		trait GroundBatchReturningEntitiesClause[S, M[O] <: BaseMapping[S, O]]
			extends GroundBatchReturningEntities[S, M] with ReturningEntitySeqClauses[Any, S, M] //with ReturningClauses[Any, M, Seq[S]]
			   with GenericReturningEntitiesClause[Any, S, M, Seq[S], TableStatement[Nothing, M, Any],
			                                       GroundBatchReturningEntitiesClause[S, M]]
	}




	object implementation {

		private[sql] trait ReturningTupleClauseExpansion[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
			extends ReturningTupleClause[Args, M, Keys, Res]
		{
			override type tuple[X <: Chain] = ReturningTupleClause[Args, M, X, X]

			override def x[T](key :M[From[M]] => RefinedMapping[T, From[M]]) :ReturningTupleClause[Args, M, Keys ~ T, Keys ~ T] =
				new ReturningProperTuple[Args, M, Keys, T](this, key)
					with ReturningTupleClauseExpansion[Args, M, Keys ~ T, Keys ~ T]
		}


		private[sql] abstract class AbstractReturningTuple[Args, M[O] <: MappingAt[O], Keys <: Chain, K, +Res]
		                            (override val statement :TableStatement[Args, M, Any],
		                             val key :RefinedMapping[K, From[M]], val form :SQLReadForm[Res])
			extends ReturningTupleClause[Args, M, Keys ~ K, Res]
		{
			override val table       :RelVar[M]                     = statement.table
			override lazy val result :StatementResult[Nothing, Res] = GeneratedKeys.Single(form)
			//consider: not making this check and taking a hands-off, user knows better approach
//  			if (returned.autoInserted.isEmpty)
//  				throw new IllegalArgumentException(
//  					returned.columns.iterator.map(_.buffString).mkString(
//  						"Cannot return component " + returned + " from " + insert +
//  							" - no columns with AutoGen buff present:", ", ", "."
//  					)
//  				)
//  			if (returned.selectedByDefault.exists(AutoGen.inactive))
//  				throw new IllegalArgumentException(
//  					"Cannot return component " + returned + " from " + insert + " - columns without AutoGen present: " +
//  						returned.selectedByDefault.filter(AutoGen.inactive) + "."
//  				)

			override def toReturningTuples :ReturningTuples[Args, M, Keys ~ K, Res] =
				new DefaultReturningTuples[Args, M, Keys ~ K, Res](this)
		}

		//todo: public factory methods
		private[sql] abstract class ReturningTupleSingleton[Args, M[O] <: MappingAt[O], K] private
		                                                   (override val statement :TableStatement[Args, M, Any],
		                                                    override val key :RefinedMapping[K, From[M]],
		                                                    override val tupleForm :ChainReadForm[@~ ~ K])
			extends AbstractReturningTuple[Args, M, @~, K, K](statement, key, key.selectForm)
		{
			private[sql] def this(statement :TableStatement[Args, M, Any], key :RefinedMapping[K, From[M]]) =
				this(statement, key, ChainForm ~ key.selectForm)

			private[sql] def this(statement :TableStatement[Args, M, Any], key :M[From[M]] => RefinedMapping[K, From[M]]) =
				this(statement, statement.table.export[From[M]].export(key(statement.table.row)))

			override def keys        :Seq[RefinedMapping[_, From[M]]] = ReversedList :+ key
			override val columnNames :Seq[String] =  //consider: should we check if the exported column is still autoIns?
				table.export[From[M]].selectedByDefault(key).iterator.map(_.name).to(ReversedList)

//          protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, K] =
//          		visitor.insertReturningOne(this)
		}

		private[sql] abstract class ReturningProperTuple[Args, M[O] <: MappingAt[O], Keys <: Chain, K] private
		                                                (val init :ReturningTupleClause[Args, M, Keys, Any],
		                                                 override val key :RefinedMapping[K, From[M]],
		                                                 override val tupleForm :ChainReadForm[Keys ~ K])
			extends AbstractReturningTuple[Args, M, Keys, K, Keys ~ K](
				init.statement, key, init.tupleForm ~ key.selectForm
			)
		{
			private[sql] def this(init :ReturningTupleClause[Args, M, Keys, Any], key :RefinedMapping[K, From[M]]) =
				this(init, key, init.tupleForm ~ key.selectForm)

			private[sql] def this(init :ReturningTupleClause[Args, M, Keys, Any], key :M[From[M]] => RefinedMapping[K, From[M]]) =
				this(init, init.table.export[From[M]].export(key(init.table.row)))

			override def keys        :Seq[RefinedMapping[_, From[M]]] = init.keys :+ key
			override val columnNames :Seq[String] =
				init.columnNames :++ table.export[From[M]].selectedByDefault(key).iterator.map(_.name).to(Seq)
//          protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Gen ~ K] =
//          	visitor.insertReturningChain(this)
		}




		private[sql] trait ReturningTupleSeqTemplate[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res,
		                                             +Stmt <: TableStatement[Nothing, M, Any]]
			extends ReturningTuplesClausesTemplate[Args, M, Keys, Seq[Res], Stmt]
		{
			protected val dml         :ReturningTuplesClausesTemplate[Nothing, M, Keys, Any, Stmt]
			override  def statement   :Stmt                            = dml.statement
			override  def columnNames :Seq[String]                     = dml.columnNames
			override  def keys        :Seq[RefinedMapping[_, From[M]]] = dml.keys
			override  val table       :RelVar[M]                       = dml.table

			override def canEqual(that :Any) :Boolean = that.getClass == getClass
			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :ReturningTupleSeqTemplate[_, M @unchecked, _, _, _] =>
					canEqual(other) && other.canEqual(this) && other.dml == dml
				case _ => false
			}
		}

		private[sql] trait ReturningTuplesTemplate[-Args, M[O] <: MappingAt[O], Keys <: Chain, +Res,
		                                           +Stmt <: TableStatement[Args, M, Any]]
			extends ReturningTuplesClause[Args, M, Keys, Res] with ReturningTupleSeqTemplate[Args, M, Keys, Res, Stmt]
		{
			protected override val dml :ReturningTupleClause[Args, M, Keys, Res] 
			                            with ReturningTuplesClausesTemplate[Args, M, Keys, Any, Stmt]
			override val result        :StatementResult[Nothing, Seq[Res]]       = dml.result.batch
			override def tupleForm     :ChainReadForm[Keys]                      = dml.tupleForm
			def one                    :ReturningTupleClause[Args, M, Keys, Res] = dml

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ReturningTuplesClause[_, M @unchecked, _, _]]
		}

		private[sql] class DefaultReturningTuples[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
		                                         (override val dml :ReturningTupleClause[Args, M, Keys, Res])
			extends ReturningTuplesTemplate[Args, M, Keys, Res, TableStatement[Args, M, Any]]
		{
			override type tuple[X <: Chain] = ReturningTuplesClause[Args, M, X, X]
			override def x[T](key :M[From[M]] => RefinedMapping[T, From[M]])
					:ReturningTuplesClause[Args, M, Keys ~ T, Keys ~ T] =
				new DefaultReturningTuples[Args, M, Keys ~ T, Keys ~ T](dml x key)
		}

		private[sql] class DefaultBatchReturningTuples[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
		                                              (override val dml :ReturningTupleClause[Args, M, Keys, Res])
			extends BatchReturningTuplesClause[Args, M, Keys, Res] with RepeatedDML[Args, Res]
			   with ReturningTupleSeqTemplate[Seq[Args], M, Keys, Res, TableStatement[Args, M, Any]]
		{
			override type tuple[X <: Chain] = BatchReturningTuplesClause[Args, M, X, X]
			override def x[T](key :M[From[M]] => RefinedMapping[T, From[M]])
					:BatchReturningTuplesClause[Args, M, Keys ~ T, Keys ~ T] =
				new DefaultBatchReturningTuples[Args, M, Keys ~ T, Keys ~ T](dml x key)

			override def canEqual(that :Any) :Boolean =
				that.isInstanceOf[BatchReturningTuples[_, M @unchecked, _, _]]
		}

		private[sql] class DefaultGroundBatchReturningTuples[Args, M[O] <: MappingAt[O], Keys <: Chain, +Res]
		                   (override val dml :BatchReturningTuplesClause[Args, M, Keys, Res],
		                    override val args :Seq[Args])
			extends GroundBatchReturningTuplesClause[Args, M, Keys, Res] with BoundDML[Seq[Args], Seq[Res]]
			   with BoundDML.Impl[Seq[Args], Seq[Res], TableDML.Of[M]#DML]
			   with ReturningTupleSeqTemplate[Any, M, Keys, Res, TableStatement[Args, M, Any]]
		{
			override type tuple[X <: Chain] = GroundBatchReturningTuplesClause[Args, M, X, X]
			override def x[T](key :M[From[M]] => RefinedMapping[T, From[M]])
					:GroundBatchReturningTuplesClause[Args, M, Keys ~ T, Keys ~ T] =
				new DefaultGroundBatchReturningTuples[Args, M, Keys ~ T, Keys ~ T](dml x key, args)

			override def canEqual(that :Any) :Boolean =
				that.isInstanceOf[GroundBatchReturningTuples[M @unchecked, _, _]]
		}




		private[sql] abstract class AbstractReturningEntities[-Args, S, M[O] <: BaseMapping[S, O], +Res]
                                                             (override val statement :TableStatement[Args, M, Any],
                                                              override val keys :Seq[RefinedMapping[_, From[M]]],
                                                              override val columnNames :Seq[String],
                                                              override val result :StatementResult[S, Res])
			extends ReturningClause[Args, M, Res] //with ReturningEntitiesClauses[Args, S, M, Res]
		{
			def this(statement :TableStatement[Args, M, Any], keys :Seq[RefinedMapping[_, From[M]]],
			         result :StatementResult[S, Res]) =
				this(statement, keys, implementation.columnNames(statement.table, keys), result)

			def this(statement :TableStatement[Args, M, Any], key :RefinedMapping[_, From[M]],
			         result :StatementResult[S, Res]) =
				this(statement, ReversedList :+ key, implementation.columnNames(statement.table, key), result)

			def this(statement :TableStatement[Args, M, Any], key :M[From[M]] => RefinedMapping[_, From[M]],
			         result :StatementResult[S, Res]) =
				this(statement, key(statement.table.row), result)

			override val table :RelVar[M] = statement.table

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args, RowProduct] =
				spelling.spell(statement)
		}

		private[sql] class DefaultReturningEntity[Args, S, M[O] <: BaseMapping[S, O]]
		                                         (override val statement :TableStatement[Args, M, Any],
		                                          override val keys :Seq[RefinedMapping[_, From[M]]],
		                                          override val columnNames :Seq[String],
		                                          override val result :StatementResult[S, S])
			extends AbstractReturningEntities[Args, S, M, S](statement, keys, columnNames, result)
			   with ReturningEntityClause[Args, S, M]
		{
			def this(statement :TableStatement[Args, M, Any], keys :Seq[RefinedMapping[_, From[M]]],
			         result :StatementResult[S, S]) =
				this(statement, keys,implementation.columnNames[M](statement.table, keys), result)

			def this(statement :TableStatement[Args, M, Any], key :RefinedMapping[_, From[M]],
			         result :StatementResult[S, S]) =
				this(statement, ReversedList :+ key, implementation.columnNames(statement.table, key), result)

			def this(statement :TableStatement[Args, M, Any], key :M[From[M]] => RefinedMapping[_, From[M]],
			         result :StatementResult[S, S]) =
				this(statement, key(statement.table.row), result)

			override type x[T] = ReturningEntityClause[Args, S, M]

			override def x[T](key :M[From[M]] => RefinedMapping[T, From[M]]) :ReturningEntityClause[Args, S, M] = {
				val comp = key(table.row)
				new DefaultReturningEntity[Args, S, M](statement, keys :+ comp,
					columnNames :++ table.export[From[M]].selectedByDefault(comp).iterator.map(_.toString),
					result
				)
			}
			override def toReturningEntities :ReturningEntitiesClause[Args, S, M] =
				new DefaultReturningEntities[Args, S, M](statement, keys, columnNames, result.batch(1))

		}

		private[sql] class DefaultReturningEntities[Args, S, M[O] <: BaseMapping[S, O]]
		                                           (override val statement :TableStatement[Args, M, Any],
		                                            override val keys :Seq[RefinedMapping[_, From[M]]],
		                                            override val columnNames :Seq[String],
		                                            override val result :StatementResult[S, Seq[S]])
			extends AbstractReturningEntities[Args, S, M, Seq[S]](statement, keys, columnNames, result)
			   with ReturningEntitiesClause[Args, S, M]
		{
			def this(statement :TableStatement[Args, M, Any], keys :Seq[RefinedMapping[_, From[M]]],
			         result :StatementResult[S, Seq[S]]) =
				this(statement, keys,implementation.columnNames[M](statement.table, keys), result)

			def this(statement :TableStatement[Args, M, Any], key :RefinedMapping[_, From[M]],
			         result :StatementResult[S, Seq[S]]) =
				this(statement, ReversedList :+ key, implementation.columnNames(statement.table, key), result)

			def this(statement :TableStatement[Args, M, Any], key :M[From[M]] => RefinedMapping[_, From[M]],
			         result :StatementResult[S, Seq[S]]) =
				this(statement, key(statement.table.row), result)


			override type x[T] = ReturningEntitiesClause[Args, S, M]

			override def x[T](key :M[From[M]] => RefinedMapping[T, From[M]]) :ReturningEntitiesClause[Args, S, M] = {
				val comp = key(table.row)
				new DefaultReturningEntities[Args, S, M](statement, keys :+ comp,
					columnNames :++ table.export[From[M]].selectedByDefault(comp).iterator.map(_.toString),
					result
				)
			}
		}



		private[sql] trait GenericBatchReturningEntities[-Args, S, M[O] <: BaseMapping[S, O], +Res,
		                                                 +Stmt <: TableStatement[Nothing, M, Any], C]
			extends GenericReturningEntitiesClause[Args, S, M, Res, Stmt, C]
		{
			protected val dml         :GenericReturningEntitiesClause[Nothing, S, M, Any, Stmt, Any]
			override  def statement   :Stmt                            = dml.statement
			override  def columnNames :Seq[String]                     = dml.columnNames
			override  def keys        :Seq[RefinedMapping[_, From[M]]] = dml.keys
			override  val table       :RelVar[M]                       = dml.table
			override type x[T] = C
		}

		private[sql] class DefaultBatchReturningEntities[Args, S, M[O] <: BaseMapping[S, O]]
		                                                (override val dml :ReturningEntityClause[Args, S, M])
			extends BatchReturningEntitiesClause[Args, S, M] with RepeatedDML[Args, S]
			   with GenericBatchReturningEntities[Seq[Args], S, M, Seq[S], TableStatement[Args, M, Any],
			                                      BatchReturningEntitiesClause[Args, S, M]]
		{
			override def x[T](key :M[From[M]] => RefinedMapping[T, From[M]]) :BatchReturningEntitiesClause[Args, S, M] =
				new DefaultBatchReturningEntities[Args, S, M](dml x key)

			override def canEqual(that :Any) :Boolean =
				that.isInstanceOf[BatchReturningEntities[_, S @unchecked, M @unchecked]]
		}

		private[sql] class DefaultGroundBatchReturningEntities[Args, S, M[O] <: BaseMapping[S, O]] private[sql]
		                   (override val dml :ReturningEntitiesClauses[Seq[Args], S, M, Seq[S]],
		                    override val args :Seq[Args])
			extends GroundBatchReturningEntitiesClause[S, M] with BoundDML[Seq[Args], Seq[S]]
			   with BoundDML.Impl[Seq[Args], Seq[S], TableDML.Of[M]#DML]
			   with GenericBatchReturningEntities[Any, S, M, Seq[S], TableStatement[Nothing, M, Any],
			                                      GroundBatchReturningEntitiesClause[S, M]]
		{
			override def x[T](key :M[From[M]] => RefinedMapping[T, From[M]]) :GroundBatchReturningEntitiesClause[S, M] =
				new DefaultGroundBatchReturningEntities[Args, S, M](dml x key, args)

			override def canEqual(that :Any) :Boolean =
				that.isInstanceOf[GroundBatchReturningEntities[S @unchecked, M @unchecked]]
		}




		trait ReturningVisitor[R[-X, +Y]] {
			def returning[X, M[O] <: MappingAt[O], Y](stmt :Returning[X, M, Y])                     :R[X, Y]

			def tuples[X, M[O] <: MappingAt[O], Ks <: Chain, Y, Q[+_]]
			          (stmt :ReturningQuantifiedTuples[X, M, Ks, Y, Q])                             :R[X, Q[Y]]

			def entities[X, S, M[O] <: BaseMapping[S, O], Q[+_]]
			            (stmt :ReturningQuantifiedEntities[X, S, M, Q])                             :R[X, Q[S]]

			def tuple[X, M[O] <: MappingAt[O], Ks <: Chain, Y](stmt :ReturningTuple[X, M, Ks, Y])   :R[X, Y]
			def tuples[X, M[O] <: MappingAt[O], Ks <: Chain, Y](stmt :ReturningTuples[X, M, Ks, Y]) :R[X, Seq[Y]]
			def entity[X, S, M[O] <: BaseMapping[S, O]](stmt :ReturningEntity[X, S, M])             :R[X, S]
			def entities[X, S, M[O] <: BaseMapping[S, O]](stmt :ReturningEntities[X, S, M])         :R[X, Seq[S]]
		}

		trait MatchReturning[R[-X, +Y]] extends ReturningVisitor[R] {
			override def tuple[X, M[O] <: MappingAt[O], Ks <: Chain, Y](stmt :ReturningTuple[X, M, Ks, Y])   :R[X, Y] =
				tuples(stmt :ReturningQuantifiedTuples[X, M, Ks, Y, Self])

			override def tuples[X, M[O] <: MappingAt[O], Ks <: Chain, Y](stmt :ReturningTuples[X, M, Ks, Y]) :R[X, Seq[Y]] =
				tuples(stmt :ReturningQuantifiedTuples[X, M, Ks, Y, Seq])

			override def entity[X, S, M[O] <: BaseMapping[S, O]](stmt :ReturningEntity[X, S, M])             :R[X, S] =
				entities(stmt :ReturningQuantifiedEntities[X, S, M, Self])

			override def entities[X, S, M[O] <: BaseMapping[S, O]](stmt :ReturningEntities[X, S, M])         :R[X, Seq[S]] =
				entities(stmt :ReturningQuantifiedEntities[X, S, M, Seq])
		}

		trait CaseReturning[R[-X, +Y]] extends MatchReturning[R] {
			override def tuples[X, M[O] <: MappingAt[O], Ks <: Chain, Y, Q[+_]]
			                   (stmt :ReturningQuantifiedTuples[X, M, Ks, Y, Q]) :R[X, Q[Y]] =
				returning(stmt)

			override def entities[X, S, M[O] <: BaseMapping[S, O], Q[+_]]
			                     (stmt :ReturningQuantifiedEntities[X, S, M, Q]) :R[X, Q[S]] =
				returning(stmt)
		}

//		trait CaseReturningTuples[R[-X, +Y]] extends ReturningVisitor[R] with InsertReturningVisitor[R]
//		trait CaseReturningEntities[R[-X, +Y]] extends ReturningVisitor[R] with InssertReturningVisitor[R]



		private[sql] def columnNames[M[O] <: MappingAt[O]]
		                            (table :Table[M], key :RefinedMapping[_, From[M]]) :Seq[String] =
			table.export[From[M]].selectedByDefault(key).view.map(_.toString).to(ReversedList)

		private[sql] def columnNames[M[O] <: MappingAt[O]]
		                            (table :Table[M], keys :Seq[RefinedMapping[_, From[M]]]) :Seq[String] =
		{
			val root = table.export[From[M]]
			keys.view.flatMap(root.selectedByDefault(_).map(_.name)).to(Seq)
		}

	}


}
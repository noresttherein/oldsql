package net.noresttherein.oldsql.sql

import java.sql.PreparedStatement

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.pixies.CachedUpdateCountStatement
import net.noresttherein.oldsql.schema.{SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.Incantation.{BoundIncantation, ComposedIncantation, MappedIncantation, RepeatedIncantation}






/**
  * @author Marcin Mościcki
  */ //or Conjuration?
trait Incantation[-Args, +Res] extends Serializable {
	def apply(args :Args)(implicit tx :SQLTransaction) :Res

	def map[X](f :Res => X) :Incantation[Args, X] = new MappedIncantation(this, f)

	def compose[X](f :X => Args) :Incantation[X, Res] = new ComposedIncantation(this, f)

	def bind(args :Args) :Incantation[Unit, Res] = new BoundIncantation(this, args)

	def batch :Incantation[Seq[Args], Seq[Res]] = new RepeatedIncantation(this)
}






object Incantation {

	def apply[Args, Res](sql :String)(implicit form :SQLWriteForm[Args], result :StatementResult[Nothing, Res])
			:Incantation[Args, Res] =
		new ExecutePreparedStatement(sql)

	def call[Args, Res](sql :String)(implicit form :SQLWriteForm[Args], result :StatementResult[Nothing, Res])
			:Incantation[Args, Res] =
		new ExecuteCallableStatement(sql)

	def returning[Args :SQLWriteForm, Res :SQLReadForm](sql :String, columnNames :Seq[String]) :Incantation[Args, Res] =
		new ReturnGeneratedKeys[Args, Res](sql, columnNames)

	implicit class Incantation0Extension[Y](private val self :Cantrip[Y]) extends AnyVal {
		@inline def apply()(implicit tx :SQLTransaction) :Y = self(())
	}
	implicit class Incantation1Extension[A, Y](private val self :Incantation[@~ ~A, Y]) extends AnyVal {
		@inline def apply(a :A)(implicit tx :SQLTransaction) :Y = self(@~ ~a)
	}
	implicit class Incantation2Extension[A, B, Y](private val self :Incantation[@~ ~A~B, Y]) extends AnyVal {
		@inline def apply(a :A, b :B)(implicit tx :SQLTransaction) :Y = self(@~ ~a~b)
	}
	implicit class Incantation3Extension[A, B, C, Y](private val self :Incantation[@~ ~A~B~C, Y]) extends AnyVal {
		@inline def apply(a :A, b :B, c :C)(implicit tx :SQLTransaction) :Y = self(@~ ~a~b~c)
	}
	implicit class Incantation4Extension[A, B, C, D, Y](private val self :Incantation[@~ ~A~B~C~D, Y]) extends AnyVal {
		@inline def apply(a :A, b :B, c :C, d :D)(implicit tx :SQLTransaction) :Y = self(@~ ~a~b~c~d)
	}
	implicit class Incantation5Extension[A, B, C, D, E, Y](private val self :Incantation[@~ ~A~B~C~D~E, Y])
		extends AnyVal
	{
		@inline def apply(a :A, b :B, c :C, d :D, e :E)(implicit tx :SQLTransaction) :Y = self(@~ ~a~b~c~d~e)
	}
	implicit class Incantation6Extension[A, B, C, D, E, F, Y](private val self :Incantation[@~ ~A~B~C~D~E~F, Y])
		extends AnyVal
	{
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F)(implicit tx :SQLTransaction) :Y = self(@~ ~a~b~c~d~e~f)
	}
	implicit class Incantation7Extension[A, B, C, D, E, F, G, Y](private val self :Incantation[@~ ~A~B~C~D~E~F~G, Y])
		extends AnyVal
	{
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G)(implicit tx :SQLTransaction) :Y =
			self(@~ ~a~b~c~d~e~f~g)
	}
	implicit class Incantation8Extension[A, B, C, D, E, F, G, H, Y]
	                                    (private val self :Incantation[@~ ~A~B~C~D~E~F~G~H, Y]) extends AnyVal
	{
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H)(implicit tx :SQLTransaction) :Y =
			self(@~ ~a~b~c~d~e~f~g~h)
	}
	implicit class Incantation9Extension[A, B, C, D, E, F, G, H, I, Y]
	                                    (private val self :Incantation[@~ ~A~B~C~D~E~F~G~H~I, Y]) extends AnyVal
	{
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I)(implicit tx :SQLTransaction) :Y =
			self(@~ ~a~b~c~d~e~f~g~h~i)
	}
	implicit class Incantation10Extension[A, B, C, D, E, F, G, H, I, J, Y]
	                                     (private val self :Incantation[@~ ~A~B~C~D~E~F~G~H~I~J, Y]) extends AnyVal
	{
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J)(implicit tx :SQLTransaction) :Y =
			self(@~ ~a~b~c~d~e~f~g~h~i~j)
	}


	import Chain.ToChainConverter

	type Cantrip[+T] = Incantation[Unit, T]

	type Incantation0[+T] = Cantrip[T]


	case object NoOp extends Incantation[Any, Int] {
		override def apply(args :Any)(implicit tx :SQLTransaction) :Int = 0
	}


	trait BatchedIncantation[-Args, +Res] extends Incantation[Args, Res] {
		def prepare(implicit tx :SQLTransaction) :PreparedStatement
		def addBatch(statement :PreparedStatement, args :Args) :Unit
		def batchResult(statement :PreparedStatement, args :Args) :Res

		override def apply(args :Args)(implicit tx :SQLTransaction) :Res = {
			val stmt = prepare
			try {
				addBatch(stmt, args)
				stmt.execute()
				val res = batchResult(stmt, args)
				stmt.close()
				res
			} catch {
				case e :Exception =>
					try { stmt.close() }
					catch { case suppressed :Exception => e.addSuppressed(suppressed) }
					throw e
			}
		}

		override def map[X](f :Res => X) :Incantation[Args, X] = new MappedBatchedIncantation(this, f)
		override def compose[X](f :X => Args) :Incantation[X, Res] = new ComposedBatchedIncantation(this, f)
		override def bind(args :Args) :Incantation[Unit, Res] = new BoundBatchedIncantation(this, args)
		override def batch :Incantation[Seq[Args], Seq[Res]] = new IncantationBatch(this)
	}


	private trait LazyBatchedIncantation[-Args, +Res] extends BatchedIncantation[Args, Res] {
		override def apply(args :Args)(implicit tx :SQLTransaction) :Res = {
			val stmt = prepare
			try {
				addBatch(stmt, args)
				stmt.execute()
				val res = batchResult(stmt, args)
				res
			} catch {
				case e :Exception =>
					try { stmt.close() }
					catch { case suppressed :Exception => e.addSuppressed(suppressed) }
					throw e
			}
		}

		override def map[X](f :Res => X) :Incantation[Args, X] =
			new MappedBatchedIncantation(this, f) with LazyBatchedIncantation[Args, X]

		override def compose[X](f :X => Args) :Incantation[X, Res] =
			new ComposedBatchedIncantation(this, f) with LazyBatchedIncantation[X, Res]

		override def bind(args :Args) :Incantation[Any, Res] =
			new BoundBatchedIncantation(this, args) with LazyBatchedIncantation[Any, Res]

		override def batch :Incantation[Seq[Args], Seq[Res]] =
			new IncantationBatch(this) with LazyBatchedIncantation[Seq[Args], Seq[Res]]
	}


	private class ExecutePreparedStatement[-Args, +Res](protected val sql :String)
	                                                   (implicit setter :SQLWriteForm[Args],
	                                                    protected val result :StatementResult[Nothing, Res])
		extends BatchedIncantation[Args, Res]
	{
		override def prepare(implicit tx :SQLTransaction) = {
			val statement = tx.prepareStatement(sql)
			result.prepare(statement)
			statement
		}

		override def addBatch(statement :PreparedStatement, args :Args) :Unit = {
			setter.set(statement, 1, args)
			statement.addBatch()
		}

		override def batchResult(statement :PreparedStatement, args :Args) :Res =
			result(new CachedUpdateCountStatement(statement))

		override def apply(args :Args)(implicit tx :SQLTransaction) :Res = {
			val stmt = prepare
			try {
				setter.set(stmt, 1, args)
				stmt.execute()
				val res = result(stmt)
				stmt.close()
				res
			} catch {
				case e :Exception =>
					try { stmt.close() }
					catch { case suppressed :Exception => e.addSuppressed(suppressed) }
					throw e
			}
		}

		override def batch :Incantation[Seq[Args], Seq[Res]] =
			new IncantationBatch[Args, Res](this) {
				override def batchResult(statement :PreparedStatement, args :Seq[Args]) :Seq[Res] = {
					val repeat = result.batch(ArraySeq, args.size)(ClassTag[Res](classOf[Any]))
					repeat(statement)
				}
			}

		override def equals(that :Any) :Boolean = that match {
			case other :ExecutePreparedStatement[_, _] =>
				(other eq this) || other.getClass == getClass && other.sql == sql && other.result == result
			case _ => false
		}

		override def hashCode :Int = sql.hashCode * 31 + result.hashCode

		override def toString :String = "Incantation[" + setter + "," + result +"](" + sql + ")"
	}


	private class ReturnGeneratedKeys[-Args, +Res](protected override val sql :String,
	                                               protected val generatedColumns :Seq[String])
	                                              (implicit setter :SQLWriteForm[Args], getter :SQLReadForm[Res])
		extends ExecutePreparedStatement[Args, Res](sql)(setter, StatementResult.GeneratedKeys.Single)
	{
		private[this] val keys = generatedColumns.toArray

		override def prepare(implicit tx :SQLTransaction) :PreparedStatement = {
			val stmt = tx.connection.prepareStatement(sql, keys)
			result.prepare(stmt)
			stmt
		}
	}


	private class ExecuteCallableStatement[-Args, +Res](protected override val sql :String)
	                                                   (implicit setter :SQLWriteForm[Args],
	                                                    result :StatementResult[Nothing, Res])
		extends ExecutePreparedStatement[Args, Res](sql)
	{
		override def prepare(implicit tx :SQLTransaction) = {
			val statement = tx.prepareCall(sql)
			result.prepare(statement)
			statement
		}
	}


	private trait LazyExecutePreparedStatement[-Args, +Res] extends ExecutePreparedStatement[Args, Res] {
		override def apply(args :Args)(implicit tx :SQLTransaction) :Res = {
			val stmt = prepare
			try {
//				setter.set(stmt, 1, args)
				addBatch(stmt, args)
				stmt.execute()
				val res = result(stmt)
				res
			} catch {
				case e :Exception =>
					try { stmt.close() }
					catch { case suppressed :Exception => e.addSuppressed(suppressed) }
					throw e
			}
		}
	}


	private case class IncantationBatch[Args, +Res](incantation :BatchedIncantation[Args, Res])
		extends BatchedIncantation[Seq[Args], Seq[Res]]
	{
		override def prepare(implicit tx :SQLTransaction) :PreparedStatement = incantation.prepare

		override def addBatch(statement :PreparedStatement, args :Seq[Args]) :Unit =
			args.foreach(incantation.addBatch(statement, _))

		override def batchResult(statement :PreparedStatement, args :Seq[Args]) :Seq[Res] =
			args.view.map(incantation.batchResult(statement, _)).to(
				ArraySeq.evidenceIterableFactory(ClassTag[Res](classOf[Any]))
			)

		override def apply(args :Seq[Args])(implicit tx :SQLTransaction) :Seq[Res] =
			if (args.isEmpty) Nil
			else super.apply(args)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[IncantationBatch[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case other :IncantationBatch[_, _] => (other eq this) || other.incantation == incantation
			case _ => false
		}

		override def toString :String = "Batch(" + incantation + ")"
	}


	private class BoundBatchedIncantation[Args, +Res]
	                                     (override val incantation :BatchedIncantation[Args, Res], params :Args)
		extends BoundIncantation(incantation, params) with BatchedIncantation[Any, Res]
	{
		override def prepare(implicit tx :SQLTransaction) :PreparedStatement = incantation.prepare

		override def addBatch(statement :PreparedStatement, args :Any) :Unit =
			incantation.addBatch(statement, this.args)

		override def batchResult(statement :PreparedStatement, args :Any) :Res =
			incantation.batchResult(statement, this.args)

		override def bind(args :Any) :Incantation[Any, Res] = this
		override def compose[X](f :X => Any) :Incantation[X, Res] = this

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[BoundBatchedIncantation[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case other :BoundBatchedIncantation[_, _] =>
				(other eq this) || other.incantation == incantation && other.args == args
			case _ => false
		}
	}


	private class ComposedBatchedIncantation[-Args, X, +Res]
	                                        (override val incantation :BatchedIncantation[X, Res], f :Args => X)
		extends ComposedIncantation[Args, X, Res](incantation, f) with BatchedIncantation[Args, Res]
	{
		override def prepare(implicit tx :SQLTransaction) :PreparedStatement = incantation.prepare

		override def addBatch(statement :PreparedStatement, args :Args) :Unit =
			incantation.addBatch(statement, argmap(args))

		override def batchResult(statement :PreparedStatement, args :Args) :Res =
			incantation.batchResult(statement, argmap(args))

		override def apply(args :Args)(implicit tx :SQLTransaction) = super[ComposedIncantation].apply(args)

		override def compose[Z](f :Z => Args) :Incantation[Z, Res] =
			new ComposedBatchedIncantation(incantation, argmap compose f)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposedBatchedIncantation[_, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case other :ComposedBatchedIncantation[_, _, _] =>
				(other eq this) || other.incantation == incantation && other.argmap == argmap
			case _ => false
		}
	}


	private class MappedBatchedIncantation[-Args, Y, +Res]
	                                      (override val incantation :BatchedIncantation[Args, Y], f :Y => Res)
		extends MappedIncantation[Args, Y, Res](incantation, f) with BatchedIncantation[Args, Res]
	{
		override def prepare(implicit tx :SQLTransaction) = incantation.prepare

		override def addBatch(statement :PreparedStatement, args :Args) :Unit =
			incantation.addBatch(statement, args)

		override def batchResult(statement :PreparedStatement, args :Args) =
			resmap(incantation.batchResult(statement, args))

		override def map[X](f :Res => X) :Incantation[Args, X] =
			new MappedBatchedIncantation(incantation, resmap andThen f)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[MappedBatchedIncantation[_, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :MappedBatchedIncantation[_, _, _] if other canEqual this =>
				other.incantation == incantation && other.resmap == resmap
			case _ => false
		}
	}


	private[sql] case class BoundIncantation[Args, +Res](incantation :Incantation[Args, Res], args :Args)
		extends Incantation[Any, Res]
	{
		override def apply(args :Any)(implicit tx :SQLTransaction) :Res = incantation(this.args)

		override def bind(args :Any) :Incantation[Any, Res] = this
		override def compose[X](f :X => Any) :Incantation[X, Res] = this

		override def toString :String = "Bound(" + incantation + ")(" + args + ")"
	}


	private[sql] case class RepeatedIncantation[-Args, +Res](incantation :Incantation[Args, Res])
		extends Incantation[Iterable[Args], Seq[Res]]
	{
		override def apply(args :Iterable[Args])(implicit tx :SQLTransaction) :Seq[Res] =
			args.view.map(incantation(_)).to(Seq)

		override def toString = incantation.toString + "*"
	}


	private[sql] case class ComposedIncantation[-X, Y, +Res](incantation :Incantation[Y, Res], argmap :X => Y)
		extends Incantation[X, Res]
	{
		override def apply(args :X)(implicit tx :SQLTransaction) :Res = incantation(argmap(args))

		override def compose[Z](f :Z => X) :Incantation[Z, Res] =
			new ComposedIncantation(incantation, argmap compose f)

		override def toString :String = "Composed(" + incantation + ")"
	}


	private[sql] case class MappedIncantation[-Args, Y, +Res](incantation :Incantation[Args, Y], resmap :Y => Res)
		extends Incantation[Args, Res]
	{
		override def apply(args :Args)(implicit tx :SQLTransaction) :Res = resmap(incantation(args))

		override def map[X](f :Res => X) :Incantation[Args, X] =
			new MappedIncantation(incantation, resmap andThen f)

		override def toString :String = "Mapped(" + incantation + ")"
	}

}

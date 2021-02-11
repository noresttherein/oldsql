package net.noresttherein.oldsql.sql

import java.sql.{PreparedStatement, ResultSet}

import scala.collection.{Factory, IterableFactory}

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.exceptions.IllegalResultArityException
import net.noresttherein.oldsql.schema.{SQLReadForm, SQLWriteForm}






/**
  * @author Marcin Mościcki
  */
trait SQLStatement[-Args <: Chain, +Res] extends Serializable {
	def apply(args :Args)(implicit tx :SQLTransaction) :Res
}






object SQLStatement {

	private class SingleCommand[+Res](sql :String, setter :SQLWriteForm[()])(implicit result :StatementResult[Any, Res])
		extends SQLCommand[Res]
	{
		override def apply(args: @~)(implicit tx :SQLTransaction) = {
			val stmt = tx.prepareStatement(sql)
			try {
				setter.set(stmt, 0, ())
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

		override def toString = s"$result{$sql}$setter"
	}



	trait StatementResult[-E, +Res] extends Serializable {
		def apply(statement :PreparedStatement) :Res
	}

	object StatementResult {
		object UpdateCount extends StatementResult[Any, Int] {
			override def apply(statement :PreparedStatement) :Int = statement.getUpdateCount
		}
		object LargeUpdateCount extends StatementResult[Any, Long] {
			override def apply(statement :PreparedStatement) :Long = statement.getLargeUpdateCount
		}

		private class SingleResult[E](implicit form :SQLReadForm[E]) extends StatementResult[E, E] {
			override def apply(statement :PreparedStatement) :E = {
				val rs = statement.getResultSet
				if (!rs.next())
					throw new IllegalResultArityException("Excepted one result, got zero.")
				val res = form(rs, 0)
				if (rs.next())
					throw new IllegalResultArityException(s"Expected one result, got $res, ${form(rs, 0)}, ...")
				res
			}
		}

		private class OptionResult[E](implicit form :SQLReadForm[E]) extends StatementResult[E, Option[E]] {
			override def apply(statement :PreparedStatement) = {
				val rs = statement.getResultSet
				val res = if (rs.next()) Some(form(rs, 0)) else None
				if (rs.next())
					throw new IllegalResultArityException(s"Expected at most one result, got: ${res.get}, ${form(rs, 0)}, ...")
				res
			}
		}

		private class CollectionResult[E, C](factory :Factory[E, C])(implicit form :SQLReadForm[E])
			extends StatementResult[E, C]
		{
			override def apply(statement :PreparedStatement) = {
				val rs = statement.getResultSet
				val builder = factory.newBuilder
				while (rs.next()) {
					builder += form(rs, 0)
				}
				builder.result()
			}
		}

		private class LazyResult[E](implicit form :SQLReadForm[E]) extends StatementResult[E, LazyList[E]] {
			override def apply(statement :PreparedStatement) :LazyList[E] = {
				def result(rs :ResultSet = statement.getResultSet) :LazyList[E] =
					if (rs.next()) form(rs, 0) #:: result(rs) else LazyList.empty[E]
				result()
			}
		}

		private class IteratorResult[E](implicit form :SQLReadForm[E]) extends StatementResult[E, Iterator[E]] {
			override def apply(statement :PreparedStatement) :Iterator[E] = new Iterator[E] {
				private[this] val rs = statement.getResultSet
				private[this] var hasMore = rs.next()
				override def hasNext = hasMore

				override def next() :E = { val res = form(rs, 0); hasMore = rs.next(); res }
			}
		}

		def apply[E :SQLReadForm, C](factory :Factory[E, C]) :StatementResult[E, C] = new CollectionResult(factory)
		def apply[E :SQLReadForm, C[_]](factory :IterableFactory[C]) :StatementResult[E, C[E]] =
			new CollectionResult[E, C[E]](factory)

		implicit def SingleResult[E :SQLReadForm] :StatementResult[E, E] = new SingleResult[E]
		implicit def OptionResult[E :SQLReadForm] :StatementResult[E, Option[E]] = new OptionResult[E]
		implicit def IterableResult[E, C](implicit form :SQLReadForm[E], factory :Factory[E, C]) :StatementResult[E, C] =
			new CollectionResult[E, C](factory)
		implicit def LazyResult[E :SQLReadForm] :StatementResult[E, LazyList[E]] = new LazyResult[E]
		implicit def IteratorResult[E :SQLReadForm] :StatementResult[E, Iterator[E]] = new IteratorResult[E]
	}



	import Chain.ToChainConverter

	type SQLStatement0[+T] = SQLCommand[T]

	trait SQLStatement1[-A, +T] extends SQLStatement[@~ ~A, T] {
		def apply(arg :A)(implicit tx :SQLTransaction) :T = apply(@~ ~ arg)
		def apply(args :Tuple1[A])(implicit tx :SQLTransaction) :T = apply(args.toChain)
	}

	trait SQLStatement2[-A, -B, +T] extends SQLStatement[@~ ~A~B, T] {
		def apply(a :A, b :B)(implicit tx :SQLTransaction) :T = apply(@~ ~a~b)
		def apply(args :(A, B))(implicit tx :SQLTransaction) :T = apply(args.toChain)
	}

	trait SQLStatement3[-A, -B, -C, +T] extends SQLStatement[@~ ~A~B~C, T] {
		def apply(a :A, b :B, c :C)(implicit tx :SQLTransaction) :T = apply(@~ ~a~b~c)
		def apply(args :(A, B, C))(implicit tx :SQLTransaction) :T = apply(args.toChain)
	}

}






trait SQLCommand[+T] extends SQLStatement[@~, T] {
	def apply()(implicit tx :SQLTransaction) :T = apply(@~)
}



object SQLCommand {

}

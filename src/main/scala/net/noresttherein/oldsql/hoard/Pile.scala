package net.noresttherein.oldsql.hoard

import net.noresttherein.oldsql.hoard.Hoard.EntityNotFoundException
import net.noresttherein.oldsql.model
import net.noresttherein.oldsql.model.{ComposedOf, Kin, PrimaryKey, Restraint, Restrictive}
import net.noresttherein.oldsql.model.ComposedOf.{DecomposableTo, ExtractAs}
import net.noresttherein.oldsql.model.Restraint.True
import net.noresttherein.oldsql.model.PropertyPath

import scala.collection.immutable.Seq
import scala.reflect.runtime.universe.TypeTag




/**
  * @author Marcin Mościcki
  */
trait Pile[T] { pile =>
	type PK
	type Session <: Hoard.Session

	def newSession() :Session
	
	def inSession[X](block :Session => X) :X = { //todo: logging
		val session = newSession()
		try {
			block(session)
		} catch {
			case e :Exception => try {
				session.close()
				throw e
			} finally {throw e }
		}
	}



	def apply(pk :PK)(implicit session :Session = newSession()) :T = get(pk) match {
		case Some(found) => found
		case _ => throw new EntityNotFoundException("Entity not found: " + this + " for key " + pk)
	}

	def get(pk :PK)(implicit session :Session = newSession()) :Option[T]

	def load[C](kin :Kin[C])(implicit composite :C ComposedOf T, session :Session = newSession()) :C

	def one(restraint :Restraint[T], fetch :PropertyPath[T, Any]*)(implicit session :Session = newSession()) :T =
		opt(restraint, fetch :_*) match {
			case Some(found) => found
			case _ => throw new EntityNotFoundException("Entity not found: " + this + " for " + restraint)
		}

	def opt(restraint :Restraint[T], fetch :PropertyPath[T, Any]*)(implicit session :Session = newSession()) :Option[T] =
		as[Option[T]](restraint, fetch :_*)

	def all(restraint :Restraint[T], fetch :PropertyPath[T, Any]*)(implicit session :Session = newSession()) :Seq[T] =
		as[Seq[T]](restraint, fetch :_*)

	def limit(restraint :Restraint[T], max :Int, fetch :PropertyPath[T, Any]*)(implicit session :Session = newSession()) :Seq[T]

	def as[C](restraint :Restraint[T], fetch :PropertyPath[T, Any]*)
	         (implicit composite :C ComposedOf T, session :Session = newSession()) :C

	def in[C[X]](restraint :Restraint[T], fetch :PropertyPath[T, Any]*)
	            (implicit composite :C[T] ComposedOf T, session :Session = newSession()) :C[T] =
		as[C[T]](restraint, fetch :_*)



	def where[P](property :PropertyPath[T, P]) :QueryPropertyBinder[P] = new QueryPropertyBinder(True, property)



	def save(entity :T)(implicit session :Session = newSession()) :T
	def insert(entity :T)(implicit session :Session = newSession()) :T
	def update(entity :T)(implicit session :Session = newSession()) :T

	def save(entities :Seq[T])(implicit session :Session) :Seq[T] =
		entities.map(save(_))

	def insert(entities :Seq[T])(implicit session :Session) :Seq[T] =
		entities.map(insert(_))

	def update(entities :Seq[T])(implicit session :Session) :Seq[T] =
		entities.map(update(_))



	protected def deleteBy(pk :PK)(implicit session :Session) :Unit
	protected def delete(entity :T)(implicit session :Session) :Unit
	protected def delete(entities :Iterable[T])(implicit session :Session) :Unit
	protected def delete(filter :Restraint[T])(implicit session :Session) :Unit
	protected def delete[C](kin :Kin[C])(implicit composite :C ComposedOf T, session :Session) :Unit



	def +=(entity :T)(implicit session :Session = newSession()) :T = save(entity)
	def ++=(entities :Seq[T])(implicit session :Session = newSession()) :Seq[T] = save(entities)
	def -=(entity :T)(implicit session :Session = newSession()) :Unit = delete(entity)
	def --=(entities :T)(implicit session :Session = newSession()) :Unit = delete(entities)



	import model.implicits._

	class QueryPropertyBinder[P](filter :Restraint[T], property :Restrictive[T, P]) {
		def ===(value :P) :QueryBuilder = new QueryBuilder(filter && property === value)

		def is(value :P) :QueryBuilder =
			if (value == null) isNull
			else this === value

		def isNull :QueryBuilder = new QueryBuilder(filter && property.isNull)

		def in(values :P*) :QueryBuilder = new QueryBuilder(filter && (property in values))
		def in(values :Iterable[P]) :QueryBuilder = new QueryBuilder(filter && (property in values))
		def in[C](collection :Restrictive[T, C])(implicit decompose :C ExtractAs P) :QueryBuilder =
			new QueryBuilder(filter && (property in collection))

	}



	class QueryBuilder(filter :Restraint[T], fetch :Seq[PropertyPath[T, Any]] = Nil) {

		def &&(condition :Restraint[T]) :QueryBuilder = new QueryBuilder(filter && condition, fetch)

		def ||(condition :Restraint[T]) :QueryBuilder = new QueryBuilder(filter || condition, fetch)

		def fetch(kin :PropertyPath[T, Any]*) :QueryBuilder =
			if (fetch.isEmpty) new QueryBuilder(filter, kin)
			else new QueryBuilder(filter, fetch ++: kin)

		def fetch(kin :Iterable[T => Any])(implicit tag :TypeTag[T]) :QueryBuilder =
			fetch(kin.map(PropertyPath[T](_)).toSeq :_*)

		def one(implicit transaction :Session = newSession()) :T = pile.one(filter, fetch :_*)
		def all(implicit transaction :Session = newSession()) :Seq[T] = pile.all(filter, fetch :_*)
		def opt(implicit transaction :Session = newSession()) :Option[T] = pile.opt(filter, fetch :_*)
	}


}






object Pile {
/*
	@inline implicit def PileOperations[T](pile :Pile[T]) :PileOperations[T, pile.PK] = new PileOperations(pile)


	class PileOperations[T, PK] private[Pile] (private val pile :Pile[T])(implicit session :pile.Session = pile.newSession()) {

	}

	class Deletions[T, PK] private[Pile] (private val pile :Pile[T])(implicit session :pile.Session) {
//		@inline def by(pk :PK) :Unit = pile.delete(pk)
		@inline def apply(entity :T) :Unit = pile.delete(entity)
		@inline def apply(entities :Iterable[T]) :Unit = pile.delete(entities) 
		@inline def apply(filter :Restraint[T]) :Unit = pile.delete(filter)
		@inline def apply[C](kin :Kin[C])(implicit composite :C ComposedOf T) :Unit = pile.delete(kin)
	}
*/
}



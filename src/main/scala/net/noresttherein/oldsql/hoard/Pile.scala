package net.noresttherein.oldsql.hoard

import net.noresttherein.oldsql.model
import net.noresttherein.oldsql.model.{ComposedOf, Kin, PrimaryKey, Restraint, Restrictive}
import net.noresttherein.oldsql.model.ComposedOf.{DecomposableTo, ExtractAs}
import net.noresttherein.oldsql.model.Restraint.True
import net.noresttherein.oldsql.model.Restrictive.TranslableTerm
import net.noresttherein.oldsql.morsels.PropertyPath

import scala.collection.immutable.Seq
import scala.collection.breakOut
import scala.reflect.runtime.universe.TypeTag

/**
  * @author Marcin Mościcki
  */
trait Pile[T] { pile =>
	type Transaction

	def newTransaction() :Transaction

	def apply[PK](pk :PK)(implicit identity :PrimaryKey[T, PK], transaction :Transaction) :T

	def apply[C](kin :Kin[C])(implicit composite :C ComposedOf T, transaction :Transaction = newTransaction()) :C

	def get(restraint :Restraint[T], fetch :PropertyPath[T, Any]*)(implicit transaction :Transaction = newTransaction()) :T
	def all(restraint :Restraint[T], fetch :PropertyPath[T, Any]*)(implicit transaction :Transaction = newTransaction()) :Seq[T]
	def ?(restraint :Restraint[T], fetch :PropertyPath[T, Any]*)(implicit transaction :Transaction = newTransaction()) :Option[T]

	def where[P](property :PropertyPath[T, P]) :QueryPropertyBinder[P] = new QueryPropertyBinder(True, property)


	def save(entity :T) :T
	def insert(entity :T) :T
	def update(entity :T) :T


	import model.implicits._

	class QueryPropertyBinder[P](filter :Restraint[T], property :Restrictive[T, P]) {
		def ===(value :P) :QueryBuilder = new QueryBuilder(filter && property === value)

		def is(value :P) :QueryBuilder =
			if (value == null) isNull
			else this === value

		def isNull :QueryBuilder = new QueryBuilder(filter && property.isNull)

		def in(values :P*) :QueryBuilder = new QueryBuilder(filter && (property in values)))
		def in(values :Iterable[P]) :QueryBuilder = new QueryBuilder(filter && (property in values))
		def in[C](collection :Restrictive[T, C])(implicit decompose :C ExtractAs P) :QueryBuilder =
			new QueryBuilder(filter && (property in collection))

	}



	class QueryBuilder(filter :Restraint[T], fetch :collection.Seq[PropertyPath[T, Any]] = Nil) {

		def &&(condition :Restraint[T]) :QueryBuilder = new QueryBuilder(filter && condition, fetch)

		def ||(condition :Restraint[T]) :QueryBuilder = new QueryBuilder(filter || condition, fetch)

		def fetch(kin :PropertyPath[T, Any]*) :QueryBuilder =
			if (fetch.isEmpty) new QueryBuilder(filter, kin)
			else new QueryBuilder(filter, fetch ++: kin)

		def fetch(kin :Iterable[T => Any])(implicit tag :TypeTag[T]) :QueryBuilder =
			fetch(kin.map(PropertyPath[T](_))(breakOut) :_*)

		def get(implicit transaction :Transaction = newTransaction()) :T = pile.get(filter, fetch :_*)
		def all(implicit transaction :Transaction = newTransaction()) :Seq[T] = pile.all(filter, fetch :_*)
		def ?(implicit transaction :Transaction = newTransaction()) :Option[T] = pile.?(filter, fetch :_*)
	}


}

package net.noresttherein.oldsql.haul

import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.Relation.Table






trait FutureValues[+E] {
	def apply[K](component :MappingOf[K], key :K) :E
	def unique[K](component :MappingOf[K], key :K) :Option[E]
	def all[K](component :MappingOf[K], key :K) :Iterable[E]
}



object FutureValues {
	object Empty extends FutureValues[Nothing] {
		override def apply[K](component :MappingOf[K], key :K) :Nothing =
			throw new NoSuchElementException(s"No value for key $component=$key in $this")

		override def unique[K](component :MappingOf[K], key :K) :Option[Nothing] = None

		override def all[K](component :MappingOf[K], key :K) :Iterable[Nothing] = Nil
	}
}



trait TableCache[E] extends FutureValues[E] {
	def table :Table[MappingOf[E]#Projection]
	def indices :Iterable[TableIndex[_, E]]
	//a non-empty index for a component can only be provided if the query will load all values with referenced keys:
	//otherwise some reference may resolve to non-empty, but incomplete value set.
	//Similarly, we must always take care whenever loading a 'many' side of a relationship so that they are not
	//created as empty collections if the referenced table is not included in the query/queries at all.
	def apply[K](component :MappingOf[K]) :TableIndex[K, E]

	def += (value :E) :Unit
}



object TableCache {
	trait EntityTableCache[PK, E] extends TableCache[E] {
		def pk :TableIndex[PK, E]
	}



	class PredefinedTableCache[S](val table :Table[MappingOf[S]#Projection], override val indices :Seq[TableIndex[_, S]])
		extends TableCache[S]
	{
		private[this] val idxs = indices.toArray
		private[this] val componentIndex =
			Map[Mapping, TableIndex[_, S]](indices.map { idx => (idx.component, idx)} :_*)

		override def apply[T](component :MappingOf[T]) :TableIndex[T, S] = {
			val res = componentIndex.getOrElse(component, null).asInstanceOf[TableIndex[T, S]]
			if (res == null)
				throw new NoSuchElementException(s"No index for component $component of $table.")
			res
		}

		override def apply[K](component :MappingOf[K], key :K) :S = apply(component)(key)

		override def unique[K](component :MappingOf[K], key :K) :Option[S] = apply(component).unique(key)

		override def all[K](component :MappingOf[K], key :K) :Iterable[S] = apply(component).all(key)

		override def +=(value :S) :Unit = {
			var i = idxs.length - 1
			while (i >= 0) {
				idxs(i) += value
				i -= 1
			}
		}

		override def toString :String = indices.view.map(_.component).mkString("Cache(" + table +": ", ", ",  ")")
	}

}






/**
  * @author Marcin Mościcki
  */
trait HaulCache {
	def apply[S](table :Table[MappingOf[S]#Projection]) :TableCache[S]
}



object HaulCache {

	class PredefinedHaulCache(tables :Seq[TableCache[_]]) extends HaulCache {
		private val caches = Map[Table[MappingAt], TableCache[_]](tables.map { t => t.table -> t } :_*)

		override def apply[S](table :Table[MappingOf[S]#Projection]) :TableCache[S] = {
			val res = caches.getOrElse(table, null)
			if (res == null)
				throw new NoSuchElementException(s"No predefined table cache for $table in $this.")
			res.asInstanceOf[TableCache[S]]
		}

		override def toString :String =
			tables.view.map { t => t.indices.view.map(_.component).mkString("(" + t + ": ", ", ", ")") }
				.mkString("HaulCache(", ", ", ")")
	}
}

package net.noresttherein.oldsql.haul

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{Mapping, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}






trait FutureValues[+E] {
	def apply[K](component :MappingOf[K], key :K) :Opt[E]
//	def unique[K](component :MappingOf[K], key :K) :Opt[E]
	def all[K](component :MappingOf[K], key :K) :Opt[Iterable[E]]
}



object FutureValues {
	object Empty extends FutureValues[Nothing] {
		override def apply[K](component :MappingOf[K], key :K) :Opt[Nothing] = Lack
		override def all[K](component :MappingOf[K], key :K) :Opt[Iterable[Nothing]] = Lack
	}
}



trait TableCache[E] extends FutureValues[E] {
	def table :Table[MappingOf[E]#Projection]
	def indices :Iterable[TableIndex[_, E]]
	//a non-empty index for a component can only be provided if the query will load all values with referenced keys:
	//otherwise some reference may resolve to a non-empty, but incomplete value set.
	//Similarly, we must always take care whenever loading a 'many' side of a relationship so that they are not
	//created as empty collections if the referenced table is not included in the query/queries at all.
	def apply[K](component :MappingOf[K]) :Opt[TableIndex[K, E]]

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
			Map[Mapping, TableIndex[_, S]](indices.map { idx => (idx.component, idx) } :_*)

		override def apply[T](component :MappingOf[T]) :Opt[TableIndex[T, S]] =
			Opt(componentIndex.getOrElse(component, null).asInstanceOf[TableIndex[T, S]])

		override def apply[K](component :MappingOf[K], key :K) :Opt[S] = {
			val idx = apply(component)
			if (idx.isEmpty) Lack else idx.get.unique(key)
		}

		override def all[K](component :MappingOf[K], key :K) :Opt[Iterable[S]] = {
			val idx = apply(component)
			if (idx.isEmpty) Lack else Got(idx.get.all(key))
		}

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
	def apply[S](table :Table[MappingOf[S]#Projection]) :Opt[TableCache[S]]
}



object HaulCache {
	object NoCache extends HaulCache {
		override def apply[S](table :Table[MappingOf[S]#Projection]) :Opt[TableCache[S]] = Lack
	}

	class SmallHaulCache(tables :Iterable[TableCache[_]]) extends HaulCache {
		private[this] val caches = tables.toArray

		override def apply[S](table :Table[MappingOf[S]#Projection]) :Opt[TableCache[S]] = {
			var i = caches.length - 1
			while (i >= 0) {
				val cache = caches(i)
				if (cache.table == table)
					return Got(cache.asInstanceOf[TableCache[S]])
				i -= 1
			}
			Lack
		}

		override def toString :String =
			tables.view.map { t => t.indices.view.map(_.component).mkString("(" + t + ": ", ", ", ")") }
				.mkString("HaulCache(", ", ", ")")
	}


	class PredefinedHaulCache(tables :Seq[TableCache[_]]) extends HaulCache {
		private val caches = Map[Table[MappingAt], TableCache[_]](tables.map { t => t.table -> t } :_*)

		override def apply[S](table :Table[MappingOf[S]#Projection]) :Opt[TableCache[S]] =
			Opt(caches.getOrElse(table, null).asInstanceOf[TableCache[S]])

		override def toString :String =
			tables.view.map { t => t.indices.view.map(_.component).mkString("(" + t + ": ", ", ", ")") }
				.mkString("HaulCache(", ", ", ")")
	}
}

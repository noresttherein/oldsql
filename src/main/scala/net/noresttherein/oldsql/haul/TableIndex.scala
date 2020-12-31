package net.noresttherein.oldsql.haul

import scala.collection.mutable

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.Table






/**
  * @author Marcin Mościcki
  */

trait TableIndex[K, E] {
	def table :Table[MappingOf[E]#Projection]
	def component[O] :RefinedMapping[K, O]
	def property :E =?> K

	def apply(key :K) :E
	def unique(key :K) :Option[E]
	def all(key :K) :Iterable[E]

	def +=(value :E) :Unit
	def add(key :K, value :E) :Unit
}



object TableIndex {

	class UniqueTableIndex[K, E](override val table :Table[MappingOf[E]#Projection], keyMapping :MappingOf[K])
		extends TableIndex[K, E]
	{
		private[this] val idx = mutable.Map.empty[K, E]
		private[this] val extract = table[this.type](keyMapping.asInstanceOf[RefinedMapping[K, this.type]])

		override def component[O] :RefinedMapping[K, O] = keyMapping.asInstanceOf[RefinedMapping[K, O]]
		protected def index :mutable.Map[K, E] = idx
		override def property :E =?> K = extract

		override def apply(key :K) :E = {
			val res = idx.getOrElse(key, null.asInstanceOf[E])
			if (res == null)
				throw new NoSuchElementException(s"No entity for key $key in $this.")
			res
		}

		override def unique(key :K) :Option[E] = idx.get(key)

		override def all(key :K) :Set[E] = {
			val res = idx.getOrElse(key, null.asInstanceOf[E])
			if (res == null) Set.empty[E] else Set.empty[E] + res
		}

		override def +=(value :E) :Unit =
			if (value != null)
				extract.get(value) match {
					case Some(key) if key != null => idx.update(key, value)
					case _ =>
				}

		override def add(key :K, value :E) :Unit =
			if (key != null)
				if (value == null)
					throw new NullPointerException(s"Attempted to add null value with key $key to index $this.")
				else idx.addOne(key, value)

		override def toString :String = "Unique(" + table + " on " + keyMapping + ")"
	}



	class OptionalUniqueTableIndex[K, E](override val table :Table[MappingOf[E]#Projection],
	                                     keyMapping :MappingOf[Option[K]])
		extends UniqueTableIndex[Option[K], E](table, keyMapping)
	{
		private[this] val extract = table[this.type](keyMapping.asInstanceOf[RefinedMapping[Option[K], this.type]])

		override def +=(value :E) :Unit =
			if (value != null)
				extract.get(value) match {
					case Some(key) if key != null && !(key eq None) => index.update(key, value)
					case _ =>
				}

		override def add(key :Option[K], value :E) :Unit =
			if (key != null && !(key eq None))
				if (value == null)
					throw new NullPointerException(s"Attempted to add null value with key $key to index $this.")
				else index.addOne(key, value)

		override def toString :String = "Opt" + super.toString
	}



	class TableMultiIndex[K, E](override val table :Table[MappingOf[E]#Projection], keyMapping :MappingOf[K])
		extends TableIndex[K, E]
	{
		private[this] val idx = mutable.Map.empty[K, mutable.Set[E]]
		private[this] val extract = table[this.type](keyMapping.asInstanceOf[RefinedMapping[K, this.type]])

		override def component[O] :RefinedMapping[K, O] = keyMapping.asInstanceOf[RefinedMapping[K, O]]
		protected def index :mutable.Map[K, mutable.Set[E]] = idx
		override def property :E =?> K = extract

		override def apply(key :K) :E = {         //we don't have to check if key==null because default mutable.Map
			val res = idx.getOrElse(key, null)  //handles null keys gracefully and we never add null keys
			if (res == null || res.isEmpty)
				throw new NoSuchElementException(s"No entity for key $key in $this.")
			if (res.size > 1)
				throw new IllegalStateException(s"Non unique results for key $key in $this.")
			res.head
		}

		override def unique(key :K) :Option[E] = {
			val res = idx.getOrElse(key, null)
			if (res == null || res.isEmpty) None
			else if (res.size > 1)
				throw new IllegalStateException(s"Non unique results for key $key in $this.")
			res.headOption
		}

		override def all(key :K) :collection.Set[E] = {
			val res = idx.getOrElse(key, null)
			if (res == null) Set.empty[E] else res
		}

		override def +=(value :E) :Unit =
			if (value != null)
				extract.get(value) match {
					case Some(key) if key != null =>
						var res = idx.getOrElse(key, null)
						if (res == null) {
							res = mutable.Set.empty[E]
							idx.update(key, res)
						}
						res += value
					case _ =>
				}

		override def add(key :K, value :E) :Unit =
			if (key != null) {
				if (value == null)
					throw new NullPointerException(s"Attempted to add a null value with key $key to index $this.")
				var res = idx.getOrElse(key, null)
				if (res == null) {
					res = mutable.Set.empty[E]
					idx.update(key, res)
				}
				res += value
			}


		override def toString :String = "Index(" + table + " on " + keyMapping + ")"
	}



	class OptionalTableMultiIndex[K, E](override val table :Table[MappingOf[E]#Projection],
	                                    keyMapping :MappingOf[Option[K]])
		extends TableMultiIndex[Option[K], E](table, keyMapping)
	{
		private[this] val extract = table[this.type](keyMapping.asInstanceOf[RefinedMapping[Option[K], this.type]])

		override def +=(value :E) :Unit =
			if (value != null)
				extract.get(value) match {
					case Some(key) if key != null && !(key eq None) =>
						var res = index.getOrElse(key, null)
						if (res == null) {
							res = mutable.Set.empty[E]
							index.update(key, res)
						}
						res += value
					case _ =>
				}

		override def add(key :Option[K], value :E) :Unit =
			if (key != null && !(key eq None)) {
				if (value == null)
					throw new NullPointerException(s"Attempted to add a null value with key $key to index $this.")
				var res = index.getOrElse(key, null)
				if (res == null) {
					res = mutable.Set.empty[E]
					index.update(key, res)
				}
				res += value
			}

		override def toString = "Opt" + super.toString
	}

}
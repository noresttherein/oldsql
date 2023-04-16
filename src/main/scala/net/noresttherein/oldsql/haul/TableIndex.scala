package net.noresttherein.oldsql.haul

import scala.collection.mutable

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Table
import net.noresttherein.oldsql.schema.Mapping.{MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.MappingExtract






/**
  * @author Marcin Mościcki
  */
trait TableIndex[K, E] {
	def table :Table[MappingOf[E]#Projection]
	def component[O] :TypedMapping[K, O]
	def property :E =?> K

	def apply(key :K) :E
	def unique(key :K) :Opt[E]
	def all(key :K) :Iterable[E]

	def +=(value :E) :Unit
	def add(key :K, value :E) :Unit

	def add[O](value :E, pieces :ComponentValues[E, O]) :Unit = pieces.get(component[O]) match {
		case Got(key) => add(key, value)
		case _ => this += value
	}
}



object TableIndex {

	class UniqueTableIndex[K, E](override val table :Table[MappingOf[E]#Projection], keyMapping :MappingOf[K])
		extends TableIndex[K, E]
	{
		private[this] val idx = mutable.Map.empty[K, E]
		private[this] val extract = table.row[this.type](keyMapping.asInstanceOf[TypedMapping[K, this.type]])

		override def component[O] :TypedMapping[K, O] = keyMapping.asInstanceOf[TypedMapping[K, O]]
		protected def index :mutable.Map[K, E] = idx
		override def property :E =?> K = extract

		override def apply(key :K) :E = {
			val res = idx.getOrElse(key, null.asInstanceOf[E])
			if (res == null)
				throw new NoSuchElementException(s"No entity for key $key in $this.")
			res
		}

		override def unique(key :K) :Opt[E] =
			Opt(idx.getOrElse(key, null.asInstanceOf[E]))

		override def all(key :K) :Iterable[E] =
			idx.getOrElse(key, null.asInstanceOf[E])::Nil

		override def +=(value :E) :Unit =
			if (value != null)
				extract.opt(value) match {
					case Got(key) if key != null => idx.update(key, value)
					case _ =>
				}

		override def add(key :K, value :E) :Unit =
			if (key != null)
				if (value == null)
					throw new NullPointerException(s"Attempted to add null value with key $key to index $this.")
				else idx.update(key, value)

		override def add[O](value :E, pieces :ComponentValues[E, O]) :Unit =
			pieces.get(extract.asInstanceOf[MappingExtract[E, K, O]]) match {
				case Got(k) => add(k, value)
				case _ =>
			}

		override def toString :String = "Unique(" + table + " on " + keyMapping + ")"
	}



	class OptionalUniqueTableIndex[K, E](override val table :Table[MappingOf[E]#Projection],
	                                     keyMapping :MappingOf[Option[K]])
		extends UniqueTableIndex[Option[K], E](table, keyMapping)
	{
		private[this] val extract = table.row[this.type](keyMapping.asInstanceOf[TypedMapping[Option[K], this.type]])

		override def +=(value :E) :Unit =
			if (value != null)
				extract.opt(value) match {
					case Got(key) if key != null && !(key eq None) => index.update(key, value)
					case _ =>
				}

		override def add(key :Option[K], value :E) :Unit =
			if (key != null && !(key eq None))
				if (value == null)
					throw new NullPointerException(s"Attempted to add null value with key $key to index $this.")
				else index.update(key, value)

		override def toString :String = "Opt" + super.toString
	}



	class TableMultiIndex[K, E](override val table :Table[MappingOf[E]#Projection], keyMapping :MappingOf[K])
		extends TableIndex[K, E]
	{
		private[this] val idx = mutable.Map.empty[K, mutable.Set[E]]
		private[this] val extract = table.row[this.type](keyMapping.asInstanceOf[TypedMapping[K, this.type]])

		override def component[O] :TypedMapping[K, O] = keyMapping.asInstanceOf[TypedMapping[K, O]]
		protected def cache :mutable.Map[K, mutable.Set[E]] = idx
		override def property :E =?> K = extract

		override def apply(key :K) :E = {         //we don't have to check if key==null because default mutable.Map
			val res = idx.getOrElse(key, null)  //handles null keys gracefully and we never add null keys
			if (res == null || res.isEmpty)
				throw new NoSuchElementException(s"No entity for key $key in $this.")
			if (res.size > 1)
				throw new IllegalStateException(s"Non unique results for key $key in $this.")
			res.head
		}

		override def unique(key :K) :Opt[E] = {
			val res = idx.getOrElse(key, null)
			if (res == null || res.isEmpty) Lack
			else if (res.size > 1)
				throw new IllegalStateException(s"Non unique results for key $key in $this.")
			else Got(res.head)
		}

		override def all(key :K) :collection.Set[E] = {
			val res = idx.getOrElse(key, null)
			if (res == null) Set.empty[E] else res
		}

		override def +=(value :E) :Unit =
			if (value != null)
				extract.opt(value) match {
					case Got(key) if key != null =>
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

		override def add[O](value :E, pieces :ComponentValues[E, O]) :Unit =
			pieces.get(extract.asInstanceOf[MappingExtract[E, K, O]]) match {
				case Got(k) => add(k, value)
				case _ =>
			}


		override def toString :String = "Index(" + table + " on " + keyMapping + ")"
	}



	class OptionalTableMultiIndex[K, E](override val table :Table[MappingOf[E]#Projection],
	                                    keyMapping :MappingOf[Option[K]])
		extends TableMultiIndex[Option[K], E](table, keyMapping)
	{
		private[this] val extract = table.row[this.type](keyMapping.asInstanceOf[TypedMapping[Option[K], this.type]])

		override def +=(value :E) :Unit =
			if (value != null)
				extract.opt(value) match {
					case Got(key) if key != null && !(key eq None) =>
						var res = cache.getOrElse(key, null)
						if (res == null) {
							res = mutable.Set.empty[E]
							cache.update(key, res)
						}
						res += value
					case _ =>
				}

		override def add(key :Option[K], value :E) :Unit =
			if (key != null && !(key eq None)) {
				if (value == null)
					throw new NullPointerException(s"Attempted to add a null value with key $key to index $this.")
				var res = cache.getOrElse(key, null)
				if (res == null) {
					res = mutable.Set.empty[E]
					cache.update(key, res)
				}
				res += value
			}

		override def toString :String = "Opt" + super.toString
	}



	class OrderedTableIndex[I, K, E](override val table :Table[MappingOf[E]#Projection],
	                                 index :MappingOf[I], keyMapping :MappingOf[K])
	                                (implicit ordering :Ordering[I])
		extends TableIndex[K, E]
	{
		implicit private[this] val entryOrdering :Ordering[(I, E)] = Ordering.by { e :(I, E) => e._1 }
		private[this] val indexProperty = table.row[this.type](index.asInstanceOf[TypedMapping[I, this.type]])
		private[this] val extract = table.row[this.type](keyMapping.asInstanceOf[TypedMapping[K, this.type]])
		private[this] val idx = mutable.Map.empty[K, mutable.ListBuffer[(I, E)]]

		override def component[O] :TypedMapping[K, O] = keyMapping.asInstanceOf[TypedMapping[K, O]]
		protected def cache :mutable.Map[K, mutable.ListBuffer[(I, E)]] = idx
		override def property :E =?> K = extract

		override def apply(key :K) :E = {         //we don't have to check if key==null because default mutable.Map
			val res = idx.getOrElse(key, null)  //handles null keys gracefully and we never add null keys
			if (res == null || res.isEmpty)
				throw new NoSuchElementException(s"No entity for key $key in $this.")
			if (res.size > 1)
				throw new IllegalStateException(s"Non unique results for key $key in $this.")
			res.head._2
		}

		override def unique(key :K) :Opt[E] = {
			val res = idx.getOrElse(key, null)
			if (res == null || res.isEmpty) Lack
			else if (res.size > 1)
				throw new IllegalStateException(s"Non unique results for key $key in $this.")
			else Got(res.head._2)
		}

		override def all(key :K) :Iterable[E] = {
			val res = idx.getOrElse(key, null)
			if (res == null) Nil else res.sorted.view.map(_._2)
		}

		override def +=(value :E) :Unit =
			if (value != null)
				indexProperty.opt(value) match {
					case Got(i) if i != null => extract.opt(value) match {
						case Got(key) if key != null =>
							var res = idx.getOrElse(key, null)
							if (res == null) {
								res = mutable.ListBuffer.empty[(I, E)]
								idx.update(key, res)
							}
							res += ((i, value))
						case _ =>
					}
					case _ =>
				}

		override def add(key :K, value :E) :Unit =
			if (key != null) {
				if (value == null)
					throw new NullPointerException(s"Attempted to add a null value with key $key to index $this.")
				indexProperty.opt(value) match {
					case Got(i) if i != null =>
						var res = idx.getOrElse(key, null)
						if (res == null) {
							res = mutable.ListBuffer.empty[(I, E)]
							idx.update(key, res)
						}
						res += ((i, value))
					case _ =>
				}
			}

		override def add[O](value :E, pieces :ComponentValues[E, O]) :Unit =
			pieces.get(indexProperty.asInstanceOf[MappingExtract[E, I, O]]) match {
				case Got(i) if i != null =>
					pieces.get(extract.asInstanceOf[MappingExtract[E, K, O]]) match {
						case Got(key) if key != null =>
							var res = idx.getOrElse(key, null)
							if (res == null) {
								res = mutable.ListBuffer.empty[(I, E)]
								idx.update(key, res)
							}
							res += ((i, value))
						case _ =>
					}
				case _ =>
			}


		override def toString :String = "Index(" + table + " on " + keyMapping + " #" + index +")"
	}

}
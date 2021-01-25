package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.haul.{TableCache, TableIndex}
import net.noresttherein.oldsql.haul.TableCache.EntityTableCache
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy
import net.noresttherein.oldsql.schema.MappingExtract






/** A stateful proxy to another mapping which maintains a collection of indices which map the values
  * of chosen components of the backing mapping to subjects sharing those values.
  * Unlike normal mappings, which are static - typically created during application startup -
  * and are shared by all threads performing actions on the schema, this mapping is created
  * specifically for mapping a single `ResultSet` or a single ''haul''.
  */
class IndexedMapping[S, O](protected override val backer :RefinedMapping[S, O], indices :Iterable[TableIndex[_, S]])
	extends DirectProxy[S, O]
{
	def this(table :TableCache[S]) = this(table.table[O], table.indices)

	private[this] val fastIndices = indices.toArray

	override def optionally(pieces :Pieces) :Opt[S] = pieces.get(backer) match {
		case res @ Got(value) =>
			var i = fastIndices.length - 1
			while (i >= 0) {
				fastIndices(i) += value
				i -= 1
			}
			res
		case none => none
	}

	override def assemble(pieces :Pieces) :Opt[S] = Lack

	override def toString :String = "Indexed(" + backer + ")"
}






/** A stateful proxy to another mapping which maintains a cache of all previously read values
  * and performs aliasing, always returning the same entity instance from the cache for the same primary key.
  * Additionally, like [[net.noresttherein.oldsql.schema.support.IndexedMapping IndexedMapping]], it maintains
  * a collection of indices which map the values of chosen components of the backing mapping
  * to subjects sharing those values in the same manner as with the primary key cache.
  * Unlike normal mappings, which are static - typically created during application startup -
  * and are shared by all threads performing actions on the schema, this mapping is created
  * specifically for mapping a single `ResultSet` or a single ''haul''.
  */
class IndexedEntityMapping[K, S, O] private (protected override val backer :RefinedMapping[S, O],
                                             pk :TableIndex[K, S], indices :Iterable[TableIndex[_, S]])
	extends DirectProxy[S, O]
{
	def this(table :EntityTableCache[K, S]) = this(table.table[O], table.pk, table.indices)

	private[this] val extract = MappingExtract(pk.component[O])(pk.property)
	private[this] val fastIndices = indices.toArray

	override def optionally(pieces :Pieces) :Opt[S] = pieces.get(extract) match {
		case Got(k) => pk.unique(k) match {
			case res if res.isDefined => res
			case _ => pieces.get(backer) match {
				case res @ Got(s) => put(k, s); res
				case none => none
			}
		}
		case _ => pieces.get(backer) match {
			case res @ Got(s) => extract.opt(s) match {
				case Got(k) => put(k, s); res
				case _ => res
			}
			case none => none
		}
	}

	override def assemble(pieces :Pieces) :Opt[S] = Lack

	private def put(key :K, value :S) :Unit = {
		pk.add(key, value)
		var i = fastIndices.length - 1
		while (i >= 0) {
			fastIndices(i) += value
			i -= 1
		}
	}

	override def toString :String =
		indices.view.map(_.component).mkString("Indexed(" + backer + "#" + pk.component + "; ", ", ",  ")")
}






object IndexedMapping {

	def apply[S, O](cache :TableCache[S]) :IndexedMapping[S, O] = new IndexedMapping[S, O](cache)
	def apply[K, S, O](cache :EntityTableCache[K, S]) :IndexedEntityMapping[K, S, O] = new IndexedEntityMapping(cache)

}









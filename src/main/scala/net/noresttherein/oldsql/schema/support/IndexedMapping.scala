package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.haul.{TableCache, TableIndex}
import net.noresttherein.oldsql.haul.TableCache.EntityTableCache
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy
import net.noresttherein.oldsql.schema.MappingExtract






class IndexedMapping[S, O](protected override val backer :RefinedMapping[S, O], indices :Iterable[TableIndex[_, S]])
	extends DirectProxy[S, O]
{
	def this(table :TableCache[S]) = this(table.table[O], table.indices)

	private[this] val fastIndices = indices.toArray

	override def optionally(pieces :Pieces) :Option[S] = pieces.get(backer) match {
		case res @ Some(value) =>
			var i = fastIndices.length - 1
			while (i >= 0) {
				fastIndices(i) += value
				i -= 1
			}
			res
		case none => none
	}

	override def assemble(pieces :Pieces) :Option[S] = None //backer.assemble(pieces)

	override def toString :String = "Indexed(" + backer + ")"
}






/**
  * @author Marcin Mościcki
  */
class IndexedEntityMapping[K, S, O] private (protected override val backer :RefinedMapping[S, O],
                                             pk :TableIndex[K, S], indices :Iterable[TableIndex[_, S]])
	extends DirectProxy[S, O]
{
	def this(table :EntityTableCache[K, S]) = this(table.table[O], table.pk, table.indices)

	private[this] val extract = MappingExtract(pk.component[O])(pk.property)
	private[this] val fastIndices = indices.toArray

	override def optionally(pieces :Pieces) :Option[S] = pieces.get(extract) match {
		case Some(k) =>	pk.unique(k) match {
			case res @ Some(_) => res
			case _ => pieces.get(backer) match {
				case res @ Some(s) => put(k, s); res
				case none => none
			}
		}
		case _ => pieces.get(backer) match {
			case res @ Some(s) => extract.get(s) match {
				case Some(k) => put(k, s); res
				case _ => res
			}
			case none => none
		}
	}

	override def assemble(pieces :Pieces) :Option[S] = None //backer.assemble(pieces)

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









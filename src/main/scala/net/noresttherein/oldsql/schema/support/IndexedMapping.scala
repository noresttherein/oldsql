package net.noresttherein.oldsql.schema.support

import scala.collection.mutable

import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter
import net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy
import net.noresttherein.oldsql.schema.{MappingExtract, PrimaryKeyOf}
import net.noresttherein.oldsql.schema.support.IndexedMapping.MappingIndex






/**
  * @author Marcin Mościcki
  */
class IndexedMapping[S, K, O] private (protected override val backer :RefinedMapping[S, O], pk :MappingIndex[S, K, O],
                                       indices :Iterable[MappingIndex[S, _, O]])
	extends ShallowProxy[S, O] with DelegateAdapter[RefinedMapping[S, O], S, O]
{
	def this(pk :MappingIndex[S, K, O], indices :Iterable[MappingIndex[S, _, O]] = Nil) = this(pk.mapping, pk, indices)
	def this(pk :MappingIndex[S, K, O]) = this(pk.mapping, pk, Nil)

	private[this] val extract = pk.property
	private[this] val index = pk.values
	private[this] val fastIndices = indices.toArray

	override def optionally(pieces :Pieces) :Option[S] = pieces.get(extract) match {
		case Some(k) =>	index.get(k) match {
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

	private def put(key :K, value :S) :Unit = {
		index.put(key, value)
		var i = fastIndices.length - 1
		while (i >= 0) {
			fastIndices(i) += value
			i -= 1
		}
	}

	override def toString :String = "Indexed(" + backer + ")#" + pk.property.export
}






object IndexedMapping {

	def apply[S, K, O](pk :MappingIndex[S, K, O], indices :MappingIndex[S, _, O]*) :IndexedMapping[S, K, O] =
		new IndexedMapping[S, K, O](pk, indices)

	def apply[M[X] <: RefinedMapping[S, X], S, K, O](mapping :M[O])(implicit key :PrimaryKeyOf[M] { type Key = K })
			:IndexedMapping[S, K, O] =
		new IndexedMapping[S, K, O](new MappingIndex[S, K, O](mapping, key(mapping)))



	class MappingIndex[S, K, O](
		val mapping :RefinedMapping[S, O], key :RefinedMapping[K, O], index :mutable.Map[K, S] = mutable.Map.empty[K, S]
	){
		private[this] val extract = mapping(key)
		def property :MappingExtract[S, K, O] = extract
		def values :mutable.Map[K, S] = index

		def += (value :S) :Unit = extract.get(value) match {
			case Some(key) => index.put(key, value)
			case _ =>
		}

		def get(key :K) :Option[S] = index.get(key)

		def apply(key :K) :S = index(key)

		override def toString :String =
			index.view.map {
				case (key, value) => key.toString + "->" + value
			}.mkString(mapping.mappingName + "#" + key + "(", ", ", ")")
	}
}




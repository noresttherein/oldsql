package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.ties.MappingReference.{MappingReferenceFactory, GenericMappingReferenceFactory}
import com.hcore.ogre.mapping._
import com.hcore.ogre.mapping.ComponentValues.ComponentValuesProxy
import com.hcore.ogre.mapping.MappingPath.\~\
import com.hcore.ogre.mapping.ties.ReferenceResolver.AdaptedReferenceResolver
import com.hcore.ogre.model.Reference
import com.hcore.ogre.model.Reference.GenericReferenceFactory
import com.hcore.ogre.morsels.InverseIndexSeq
import com.hcore.ogre.slang.options.extensions

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.slick.jdbc.PositionedResult

//implicits
import extensions._
import InverseIndexSeq.implicitIndexing


/** An observer which tracks objects mapped as a result of a query and serves as a factory for references in those objects.
  * If the target of the reference is present in the results, the reference created will be a full reference.
  * Note that:
  * 1) references returned by this instance will be usually lazy in one form and another to allow forward
  * and circular references, so their value shouldn't be accessed before the mapping of the whole result is over,
  * and 2) the mapping/resolve process may span several related queries, not necessarily a single select.
  */
trait ReferenceResolver { self =>

	/** Notification callback informing this instance that a value of the given was present in the result set. */
	def note[M<:AnyMapping](mapping :M, value :M#ResultType) :Unit


	/** Create a reference based on the given key, using the given factory, and data in this instance to provide the optional value.
 	  * @param key the value used by the given factory to identify referenced entities.
	  * @param factory reference factory which 'delayed' method will be called with the given
	  * @tparam K type of the key - identification data permamently associated with the source (referencing) object, which identifies the target object(s)
	  * @tparam E referenced entity (non-collection type)
	  * @tparam T content type of the created reference, based on E (generally such that T ConsistsOf E)
	  * @tparam R particular reference type.
	  * @return Some(reference), either full or empty, possibly not determined at this point if the target entity is tracked, and None if it isn't and passed factory is not recognized.
	  */
	def apply[K, E, T, R<:Reference[T]](key :K, factory :GenericReferenceFactory[K, E, T, R]) :Option[R]


	/** Create a resolver which will first delegate to this instance, and if no reference could be created,
	  * will fallback to the resolver given as the argument.
	  */
	def orElse(other :ReferenceResolver) :ReferenceResolver = new ReferenceResolver {

		override def note[M<:AnyMapping](mapping: M, value: M#ResultType): Unit = {
			self.note(mapping, value)
			other.note(mapping, value)
		}

		override def apply[K, E, T, R <: Reference[T]](key :K, factory: GenericReferenceFactory[K, E, T, R]): Option[R] =
			self(key, factory) orElse other(key, factory)
	}


	def adapt(mapArgs :AnyMapping=>Option[AnyMapping]) :ReferenceResolver =
		new AdaptedReferenceResolver(this, mapArgs)

}






object ReferenceResolver {
	def apply(trackedRefs :Seq[GenericMappingReferenceFactory[_, _, _, _, _<:AnyMapping]]) :ReferenceResolver =
		new MappingReferenceResolver(trackedRefs)


	def apply[K, E, T, R<:Reference[T]](res :PositionedResult)(key :K, factory :GenericReferenceFactory[K, E, T, R]) :Option[R] =
		unapply(res).flatMap(_(key, factory))


	def apply[M<:AnyMapping, K, E, T, R<:Reference[T]](values :ComponentValues[M])(key :K, factory :GenericReferenceFactory[K, E, T, R]) :Option[R] =
		unapply(values).flatMap(_(key, factory))


	def unapply(res :PositionedResult) :Option[ReferenceResolver] = res match {
		case r :ReferencingPositionedResult => Some(r.resolver)
		case _ => None
	}

	def unapply(values :ComponentValues[_]) :Option[ReferenceResolver] = values match {
		case r :ReferencingComponentValues[_] => Some(r.resolver)
		case _ => None
	}


	class MappingReferenceResolver(referencers :Seq[GenericMappingReferenceFactory[_, _, _, _, _<:AnyMapping]]) extends ReferenceResolver {
		import MappingReferenceResolver._

		private val referencedByMapping = referencers.groupBy(_.target).map {
			case (target, refs) => target ->
				new ItemsForMapping[target.ResultType](refs.asInstanceOf[Seq[GenericMappingReferenceFactory[_, target.ResultType, _, _, target.type]]])
		}.toMap[AnyMapping, ItemsForMapping[_]]

		override def note[M<:AnyMapping](mapping: M, value: M#ResultType): Unit =
			referencedByMapping.get(mapping).foreach {
				_.asInstanceOf[ItemsForMapping[M#ResultType]] += value
			}


		override def apply[K, E, T, R <: Reference[T]](key :K, factory: GenericReferenceFactory[K, E, T, R]): Option[R] =
			factory match {
				case MappingReferenceFactory(m) =>
					referencedByMapping.get(m.target).flatMap(_.asInstanceOf[ItemsForMapping[E]].get(key, factory))
				case _ => None
			}


	}

	object MappingReferenceResolver {

		private class ItemsForMapping[E](references: Seq[GenericReferenceFactory[_, E, _, _]]) {
			val byReference = references.map { ref =>
				val cast = ref.asInstanceOf[GenericReferenceFactory[Any, E, Any, Reference[Any]]]
				cast -> new ItemsForReference(cast)
			}.toMap[GenericReferenceFactory[_, E, _, _], ItemsForReference[_, E, _, _]]

			def +=(item: E): Unit = byReference.foreach {
				case (_, items) => items += item
			}

			def get[K, C, R <: Reference[C]](key: K, ref: GenericReferenceFactory[K, E, C, R]): Option[R] =
				byReference.get(ref).map(_.asInstanceOf[ItemsForReference[K, E, C, R]].get(key))

//			private def forReference[K, T, R <: Reference[T]](ref: GenericMappingReferenceFactory[K, E, T, R, _<:Mapping[E]]) =
//				new ItemsForReference[K, E, T, R](ref)
		}


		private class ItemsForReference[K, E, T, R <: Reference[T]](val reference: GenericReferenceFactory[K, E, T, R]) {
			val byKey = mutable.Map[K, ListBuffer[E]]()
			private var exported = false

			def +=(item: E): Unit = reference.keyFor(item).foreach { key =>
				byKey.getOrElseUpdate(key, ListBuffer[E]()) += item
			}.unlessThenThrow(exported, s"Cannot enlist another item $item for $reference: items already exported")

			def items(key: K): Iterable[E] = {
				exported = true
				byKey.getOrElse(key, Seq())
			}

			def get(key: K): R = reference.delayed(key, reference.items(items(key)))
		}

	}



	private class AdaptedReferenceResolver(target :ReferenceResolver, argMapper :AnyMapping=>Option[AnyMapping]) extends ReferenceResolver {
		override def note[M<:AnyMapping](mapping: M, value: M#ResultType): Unit =
			argMapper(mapping).foreach{ mapping => target.note(mapping, value) }

		override def apply[K, E, T, R <: Reference[T]](key: K, factory: GenericReferenceFactory[K, E, T, R]): Option[R] =
			target(key, factory)

		override def adapt(mapArgs :AnyMapping=>Option[AnyMapping]) :ReferenceResolver =
			new AdaptedReferenceResolver(target, mapArgs(_).flatMap(argMapper))
	}









	class ReferencingPositionedResult(res :PositionedResult, val resolver :ReferenceResolver, offset :Int=0)
		extends PositionedResultView(res, offset)
	{
		override def offset(idx: Int): ReferencingPositionedResult = new ReferencingPositionedResult(result, resolver, pos+idx)
	}


	/** ComponentValues proxy which notifies the given resolver about each value returned by 'apply()' and 'get'.
	  * Note that these are the top level methods that delegate directly to the underlying mapping, not the accessor methods
	  * used by the mapping in the assembly, as caching a result of 'result' might end with incorrect values, because the
	  * mapping may chose to modify or disregard the value returned by ComponentValues. For this reason, it is important
	  * to use apply() or get of this instance to map the results of a query.
	  */
	class ReferencingComponentValues[M<:AnyMapping](target :M, backing :ComponentValues[M], val resolver :ReferenceResolver)
		extends ComponentValuesProxy[M](target, backing)
	{

		override def value(root: M): M#ResultType = note(super.value(root))

		override def getValue(root: M): Option[M#ResultType] = super.getValue(root).map(note)

		private def note(value :M#ResultType) :M#ResultType = {
			resolver.note[M](mapping, value)
			value
		}

//		override def proxy[X <: Mapping](target: X, result: ComponentValues[X], map: (X#Component[_]) => Option[M#Component[_]]): ComponentValues[X] = {
//			val components = target.subcomponents.indexed
//			val adapter = (m :Mapping) =>
//				if (components.contains(m)) map(m.asInstanceOf[X#Component[_]])
//				else Some(m)
//			new ReferencingComponentValues[X](target, result, resolver.adapt(adapter))
//		}
		//		def proxy[X<:Mapping](target :X, result :ComponentValues[X], map :X#Component[_]=>Option[M#Component[_]]) :ComponentValues[X]
		override def proxy[X <: AnyMapping](target: X, values: ComponentValues[X], morphism: MappingMorphism[M, X]): ComponentValues[X] =
			new ReferencingComponentValues[X](target, values, resolver)
	}

}
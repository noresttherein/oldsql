package com.hcore.ogre.mapping

import com.hcore.ogre.mapping.ComponentPath.TypedComponentPath
import com.hcore.ogre.mapping.MappingPath.\~\
import com.hcore.ogre.mapping.ties.Pathfinder
import com.hcore.ogre.mapping.ties.Pathfinder.SingleRootPathfinder
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.slang.options.extensions

import scala.reflect.runtime.universe.TypeTag


//implicits
import extensions._


/** Index of all subcomponents inside mapping M by the function returning the value for that component. */
trait ComponentIndex[M<:AnyMapping{ type ResultType=X }, X] extends SingleRootPathfinder[M, X, Any] {
	def mapping :M

	override def apply(property :PropertyChain[X, Any]) :ComponentPath[_>:M <:M,  _<:M#Component[_]] =
		get(property) getOrElse {
			throw new NoSuchElementException(s"Can't find component for $property in $this")
		}

	override def apply(property :X=>Any) :ComponentPath[_ >:M <:M, _<:M#Component[_]] =
		get(property) getOrElse {
			throw new NoSuchElementException(s"Can't find component for $property in $this")
		}

	def get[S<:AnyMapping, T<:AnyMapping](path :MappingPath[S, T]) :Option[PropertyChain[X, _]]


	def get(property :PropertyChain[X, Any]) :Option[ComponentPath[M, _<:M#Component[_]]]

	def get(property :X=>Any) :Option[ComponentPath[M, _<:M#Component[_]]]

	def component(property :PropertyChain[X, Any]) :Option[M#Component[_]] =
		get(property).flatMap[M#Component[_]](_.lift)

	def component(property :X=>Any) :Option[M#Component[_]] =
		get(property).flatMap[M#Component[_]](_.lift)

	def mapping(property :PropertyChain[X, Any]) :Option[Mapping[_]] = get(property).map(_.end)

	def mapping(property :X=>Any) :Option[Mapping[_]] = get(property).map(_.end)


}




object ComponentIndex {

	trait MappingIndex {
		def mappings :Iterable[AnyMapping]

		def apply(mapping :AnyMapping) :ComponentIndex[mapping.type, mapping.ResultType] =
			get(mapping) getOrElse {
				throw new NoSuchElementException(s"No component index found for $mapping")
			}

		def get(mapping :AnyMapping) :Option[ComponentIndex[mapping.type, mapping.ResultType]]
	}




	def apply[T :TypeTag](mapping :Mapping[T], ignoreErrors :Boolean=true) :ComponentIndex[mapping.type, T] =
		new ReflectedComponentIndex[mapping.type, T](mapping, ignoreErrors)
//		reflect(mapping, ignoreErrors)


	def reflect[T :TypeTag](mapping :Mapping[T], ignoreErrors :Boolean=true) :ReflectedMappingIndex =
		new ReflectedMappingIndex(
			Seq(mapping -> new ReflectedComponentIndex[mapping.type ,T](mapping, ignoreErrors)).toMap,
			ignoreErrors
		)


	class ReflectedComponentIndex[M<:AnyMapping{ type ResultType=X }, X :TypeTag](val mapping :M, ignoreErrors :Boolean=true)
		extends ComponentIndex[M, X] //with PartTimePathfinder[X, Any]
	{

		private val index =
			mapping.subcomponents.flatMap { comp =>
				val path = (mapping \\ comp).asInstanceOf[TypedComponentPath[M, M#Component[Any], Any]]
				if (ignoreErrors)
					path.surepick.flatMap(PropertyChain.maybe(_)).map(_ -> path)
				else
					path.surepick.map(PropertyChain(_) -> path)
			}.toMap[PropertyChain[X, Any], TypedComponentPath[M, M#Component[Any], Any]] //.asInstanceOf[Iterable[(PropertyPath[X, Any], TypedPath[M, M#Component[Any], Any])]].toMap //[PropertyPath[X, _], TypedPath[M, _<:M#Component[_], _]]

		private val reverseIndex = index.map { case (property, path) => (path, property) }.toMap[MappingPath[_, _], PropertyChain[X, Any]]

		override def get(property: PropertyChain[X, Any]): Option[ComponentPath[M, _ <: M#Component[_]]] =
			index.get(property :PropertyChain[X, Any]) //.asInstanceOf[Option[TypedComponentPath[M, _ <: M#Component[X], X]]]


		override def get(property: X => Any): Option[ComponentPath[M, _ <: M#Component[_]]] =
			get(PropertyChain(property))

		def get[S<:AnyMapping, T<:AnyMapping](path :MappingPath[S, T]) :Option[PropertyChain[X, _]] = reverseIndex.get(path)


		def consume[Y](property: PropertyChain[X, Y]): Option[(MappingPath[M, _ <: AnyMapping], Option[PropertyChain[_, Y]])] =
			index.get(property).map((_, None)) orElse {
				val prefixes = index.flatMap { case (prop, path) => property.drop(prop).map(suffix => (path, Some(suffix))) }
				prefixes.minBy(_._2.getValue.name.length).providing(prefixes.nonEmpty)
			}

		override def consume[S <: AnyMapping, N <: AnyMapping, Y <: Any](path: MappingPath[S, N], property: PropertyChain[X, Y]): Option[(MappingPath[S, _ <: AnyMapping], Option[PropertyChain[_, Y]])] =
			consume(property).flatMap { case (consumed, suffix) => path.concat(consumed).map((_, suffix)) }

		override def follow[S <: AnyMapping, N <: AnyMapping, Y](
				path: MappingPath[S, N], property: PropertyChain[X, Y], continuation: Pathfinder[Nothing, Y]) =
			consume(property).flatMap { case (comp, tail) =>
				val prefix = path.asInstanceOf[S\~\M] ++ comp
				tail.map(prop => continuation.follow(prefix, prop)) getOrElse Some(prefix)
			}



		override def toString = index.keys.mkString(s"Properties[$mapping]{", ",", "}")
	}





	class ReflectedMappingIndex(val indices :Map[AnyMapping, ComponentIndex[_, _]], ignoreErrors :Boolean=true)
		extends MappingIndex
	{

		def mappings = indices.keys


		override def get(mapping: AnyMapping): Option[ComponentIndex[mapping.type, mapping.ResultType]] =
			indices.get(mapping).asInstanceOf[Option[ComponentIndex[mapping.type, mapping.ResultType]]]




		def and[X :TypeTag](mapping :Mapping[X]) :ReflectedMappingIndex =
			new ReflectedMappingIndex(
				indices + ((mapping :AnyMapping, new ReflectedComponentIndex[mapping.type, X](mapping, ignoreErrors))),
				ignoreErrors)
	}











	abstract class DeassembledComponentIndex[M<:AnyMapping{ type ResultType=T }, T](val mapping :M) extends ComponentIndex[M, T] {

	}

}

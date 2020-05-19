package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.Mapping.TypedMapping
import com.hcore.ogre.mapping.MappingPath.\~\
import com.hcore.ogre.mapping.{ComponentPath, AnyMapping, MappingPath}
import com.hcore.ogre.model.Reference


/** A marker interface identifying some mapping by a contained path. It is used by Pathfinders as a means for discovery
  * what mapping does a particular function refer to. In general, given some function p: X=>Y working on types in the domain model,
  * a mapping : M<:Mapping[X], we would like to discover what mapping (component, other last) does the function refer to.
  * One way of doing it is to call it on a mock instance obtained from mapping M and investigate the result:
  * if the target mapping (and all intermediate from X to Y) support this feature, they will pass the starting path
  * originating in the root mock for X, appending to it any passed components/joined tables, hopfully eventually
  * returning a value containing the full path through all the mappings reflected by p. A pathfinder than can match
  * the result p(mapping.scoutValue), and if it implements the scout interface, return the path contained in it.
  *
  * If you implement scoutValue method on mappings so that they always recursively return Scout instances whenever possible,
  * it will be possible to map a function of chained property calls, such as (_:Person).homeAdress.inhabitants,
  * to path through all the mappings responsible for intermediate and final results, for example People\Addresses\People.
  */
trait Scout {
	/** Path to the mapping representing this instance. In most cases implementations will be artificial mock subclasses
	  * of business model (or helper component) classes, created solely for the purpose of path discovery.
	  */
	def path :MappingPath[_, _]
	
	override def toString = s"Scout($path)"
}


/** Pattern matching extractor investigating a value returned by some function, assumed to represent
  * a (transitive) property of some business entity, when called on a mock instance created for the process
  * of this discovery. If the matched value is a Scout, it will return the path associated with it.
  */
object Scout {

	/** Create a new scout containing the given path. As the instance returned doesn't implement any extra interfaces,
	  * it will probably be useless for virtually all applications.
	  */
	def create[S<:AnyMapping, T<:AnyMapping](walkedPath :MappingPath[S, T]) :Scout = new GenericScout(walkedPath)


	/** Attempt to extend the given path by applying the given function to a mock value created by its end mapping and investigating
	  * the returned value. If it is a scout instance, and the path associated with it starts where the argument path ends, it will
	  * be returned as Some. In all other cases (no scoutValue on the mapping, not a scout, or due to some programming error the paths
	  * cannot be extended, will return None in a miserable failure. All exceptions thrown by by the function are propagated - be warned!
	  * @param path a path through already resolved mappings, which end is the starting point from which we send our scout.
	  * @param incognita a 'walk' through mapped domain objects, assumingly constituting of chained property calls on the result type of the path's target.
	  * @tparam S root mapping of the path being resolved, irrelevant to this function.
	  * @tparam T mapping from which the discovery process should start
	  * @tparam X domain model type mapped by mapping T and at the same time the owner of the property returned by incognita.
	  * @tparam Y return value of the function, and hopefully the ResultType of the mapping found by the Scout, but we have no way of enforcing it.
	  */
	def apply[S<:AnyMapping, T<:TypedMapping[X], X, Y](path :MappingPath[S, T], incognita :X=>Y) :Option[MappingPath[S, _<:AnyMapping]] =
		path.end.scoutValue.map(incognita) match {
			case Some(Scout(discovered)) => path.concat(discovered)
			case _ => None
		}


	/** Attempt to plot a path through mappings underlying the chained property described by incognita.
	  * Will call incognita on mapping.scoutValue (if its Some(_)) and check if the returned value is a Scout containing a path
	  * which starts with the given mapping. If so, the path's end is assumed to be the mapping responsible for type Y,
	  * in the context of this function. If incognita is a chained property, returned path should contain all intermediate
	  * mappings such as components or joined tables.
	  */
	def apply[T<:TypedMapping[X], X, Y](mapping :T, incognita :X=>Y) :Option[MappingPath[T, _<:AnyMapping]] =
		apply(ComponentPath.self(mapping), incognita)


	/** Checks if scoutValue is implemented on the given mapping to return a path. If scoutValue returns Some(Scout)
	  *  (or another object from which a scout could be retrieved recursively by unapply) and the returned path
	  *  starts with the given mapping, it is returned in Some.
	  */
	def apply[T<:AnyMapping](mapping :T) :Option[MappingPath[T, _<:AnyMapping]] =
		mapping.scoutValue match {
			case Scout(path) if path.start==mapping => Some(path.cast[T, AnyMapping])
			case _ => None
		}

	/** Attempts to extend the given path by investigating its end's mapping scout value.
	  * If it contains a path that can be contatenated to the given path,
	  * the extended (or not, if Scout containd a self path) path is returned.
	  */
	def apply[S<:AnyMapping, T<:AnyMapping](path :MappingPath[S, T]) :Option[MappingPath[S, _<:AnyMapping]] =
		path.end.scoutValue match {
			case Scout(result) => path.concat(result)
			case _ => None
		}



	/** Checks if result is an instance of Scout and returns its path if so.
	  * It will also match a Reference[Scout], Some[Scout], and Iterable[Scout] (with a single element).
	  * Return path is assumed to contain mappings representing the objects walked since the time the path was lost.
	  */
	def unapply(result :Any) :Option[MappingPath[_<:AnyMapping, _<:AnyMapping]] = {
		result match {
			case scout: Scout =>
				Some(scout.path.cast[AnyMapping, AnyMapping])
			case Reference.Full(o) =>
				unapply(o)
			case Some(o) =>
				unapply(o)
			case col :Iterable[_] if col.size==1 =>
				unapply(col.head)
			case _ => None
		}
	}

	/** Checks if the second element in the pair is a Scout instance containing a path which starts where the first pair element
	  * ends. Used to map a function 'walking' through properties of domain model objects into mappings behind those objects.
	  * @param scouting a pair, where the first element contains a path up to currently discovered mapping, and a second is an object returned
	  *                 by some function when applied to a mock value of the target mapping of that path.
	  * @tparam S root mapping of the followed path, irrevelant for this function;
	  * @tparam T mapping where the path was 'lost' and the track needs to be picked up by 'sending a scout', i.e.
	  *           applying a function to a mock value of this mapping and hoping to receive a Scout plotting the path as the result.
	  * @return an option containing the extended path, if the second element was a Scout and its path could had been appended to the end of the first element.
	  */
	def unapply[S<:AnyMapping, T<:AnyMapping](scouting :(MappingPath[S, T], Any)) :Option[MappingPath[S, _<:AnyMapping]] =
		scouting._2 match {
			case Scout(path) => scouting._1.concat(path.cast[AnyMapping, AnyMapping])
			case _ => None
		}

	/** A convenience base class which can be used as a scout instance. */
	class GenericScout[S<:AnyMapping, T<:AnyMapping](val path :MappingPath[S, T]) extends Scout
}

package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.ComponentIndex.{ReflectedComponentIndex, MappingIndex}
import com.hcore.ogre.mapping.ComponentPath.{SelfPath, TypedComponentPath}
import com.hcore.ogre.mapping.{Mapping, ComponentPath, AnyMapping, MappingPath}
import com.hcore.ogre.mapping.MappingPath.{\~\, TypedPath}
import com.hcore.ogre.model.NavigableReference
import com.hcore.ogre.morsels.necromancy.PropertyChain.===>
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.options.extensions

import scala.reflect.runtime.universe.{TypeTag, Type, typeOf}

//implicits
import extensions._
import SaferCasts._


/** Maps a function representing a property of a mapped entity class (or a PropertyPath for such a function) into a mapping path representing that property.
  * The path might represent both a component/column of a mapped table, which can be used in a filter or explicitly selected in case
  * of columns which are not included in the select header by default, or another table/component, in which case it will contain link elements
  * representing the required joins/additional selects.
  * This class is designed to work with NavigableReference and the 'fetch' function it provides in order to specify relationships to be loaded together with the root entity.
  * @tparam X lower bound type for the the function argument specifying what entities are recognized by this pathfinder
  * @tparam Y lower bound type for the function value of handled property types - for example, a pathfinder implementation may work only for functions returning Reference[_]
  */
trait Pathfinder[+X, -Y] {

	def apply(fun :X=>Y) :MappingPath[_<:AnyMapping, _<:AnyMapping] = get(fun) getOrElse {
		throw new IllegalArgumentException(s"Unable to find a mapping path for property $fun using $this")
	}

	def apply(property :PropertyChain[X, Y]) :MappingPath[_<:AnyMapping, _<:AnyMapping] = get(property) getOrElse {
		throw new IllegalArgumentException(s"Unable to find a mapping path for property $property using $this")
	}

	/** Try to find a path represented by this property. Be warned that implementation might not support this method if it
	  * requires a PropertyPath and one cannot be created due to lack of type information for the argument. For this reason, prefer
	  * get(PropertyPath[X,Y]) if possible, or use it as a fallback.
	  * Will generally return None on failure, but situations recognized as programming errors might result in throwing an exception.
	  * @param fun a function returning a value of a property we want to find the mapping for, for example _.favouriteHat or _.favouritePub.address.street
	  */
	def get(fun :X=>Y) :Option[MappingPath[_<:AnyMapping, _<:AnyMapping]]

	/** Try to find a path represented by this property. Will generally return None if the function cannot be matched into a property,
	  * but some cases (for example, passing a function of not supported type) might throw an exception, which would indicate a programming error somewhere,
	  * not necessarly in this method.
	  * @param property property chain for the entity property we want to find the mapping for, for example _.favouriteHat or _.favouritePub.address.street
	  * @return
	  */
	def get(property :PropertyChain[X, Y]) :Option[MappingPath[_<:AnyMapping, _<:AnyMapping]]

	/** Reverse index of supported paths and their corresponding property chains. Implementations with statically known all accepted property chains
	  * may provide an inverse mapping to avoid expensive creation of the property chain. In generall the following should always be true:
	  * <code>get(path).forall(fun => get(fun)==Some(path))</code>
	  * @param path a path handled by this pathfinder, for example representing a comopnent of an underlying table
	  * @return property chain returning the value of the given component.
	  */
	def get[S<:AnyMapping, T<:AnyMapping](path :MappingPath[S, T]) :Option[PropertyChain[_, _]]


	/** Continue to resolve a property into a path, appending to the given path prefix and starting resolving from property.
	  * This method exists for convenience of pathfinder implementations supporting partial resolution, where a pathfinder might
	  * resolve part of the argument property and pass on the call to another instance in a tail call. For example, given a function:
	  * (_:Member).personal.favourites.pub.address, a pathfinder might resolve the prefix _.personal.favourites.pub into a foreign key for a table,
	  * and call follow(Members :\ PersonalInfo :\ Favourites :\ pub, _.address) on a pathfinder responsible for the pubs table.
	  * Default implementation is just <code>get(property).map(path ++ _)</code> and overriding methods should preserve this equivalence.
	  * @param prefix already resolved path of the prefix function - any mappings resolved by this instance should be appended to this path.
	  * @param remainder the 'tail' of originally passed argument function which remains to be resolved.
	  * @return mapping path being a concatenation of argument path prefix and resolved path specified by the argument property chain or None
	  *         if the property chain doesn't correspond to any path known to this instance.
	  * @see #consume
	  */
	def follow[S<:AnyMapping, M<:AnyMapping](prefix :MappingPath[S, M], remainder :PropertyChain[X, Y]) :Option[MappingPath[S, _<:AnyMapping]] =
		get(remainder).map(prefix ++ _.asInstanceOf[M\~\AnyMapping])

	/** Continue to resolve a property into a path, appending to the given path prefix and starting resolving from property.
	  * This method exists for convenience of pathfinder implementations supporting partial resolution, where a pathfinder might
	  * resolve part of the argument property and pass on the call to another instance in a tail call. For example, given a function:
	  * (_:Member).personal.favourites.pub.address, a pathfinder might resolve the prefix _.personal.favourites.pub into a foreign key for a table,
	  * and call follow(Members :\ PersonalInfo :\ Favourites :\ pub, _.address) on a pathfinder responsible for the pubs table. Additional third argument
	  * is a callback pathfinder whose .follow(prefix, remainder) is to be called if and only if this instance was able to partially resolve the argument property.
	  * @param prefix already resolved path of the prefix function - any mappings resolved by this instance should be appended to this path.
	  * @param remainder the 'tail' of originally passed argument function which remains to be resolved.
	  * @param successor a pathfinder which can pick up resolving any remainder of the argument property which couldn't be resolved by this instance.
	  * @return mapping path being a concatenation of argument path prefix and resolved path specified by the argument property chain, either by this instance
	  *         or the passed successor, or None if either this instance couldn't resolve any prefix of the remainder, or successor returned None
	  * @see #consume
	  */
	def follow[S<:AnyMapping, M<:AnyMapping, Z<:Y](prefix :MappingPath[S, M], remainder :PropertyChain[X, Z], successor :Pathfinder[Nothing, Z]) :Option[MappingPath[S, _<:AnyMapping]] =
		get(remainder).map(prefix ++ _.asInstanceOf[M\~\AnyMapping])

	/** Similar to follow, this method will try to resolve as much of the given property as it can, but instead of forwarding the resolution of the remainder, will return the partial result.
	  * Implementations should always consume as much as possible, so that any further call to consume with the values directly returned by a previous call would return None, meaning
	  * the remainder can't be resolved by this pathfinder. If the implementation cannot resolve any prefix of the argument propperty it should return None, but any partial success should
	  * be returned wrapped in Some. Please note that some implementations might not support partial resolution, and thus will return Some(_,_) if and only if get(_) would return Some(_).
	  * @param prefix the prefix path containing the already resolved part of a consumed (dropped) prefix of the original property chain.
	  * @param remainder property chain representing the resolved property with the prefix already resolved into 'prefix' dropped.
	  * @return None if the property is not recognized at all, or a pair denoting the state after partial resolution, consisting of the resolved prefix
	  *         (of which the argument path is a strict prefix) and optional unresolved calls from the remainder. If the first argument represents the final result of the resolution, it should
	  *         be None rather than Some(SelfPath(_)).
	  */
	def consume[S<:AnyMapping, M<:AnyMapping, Z<:Y](prefix :MappingPath[S, M], remainder :PropertyChain[X, Z]) :Option[(MappingPath[S, _<:AnyMapping], Option[PropertyChain[_, Z]])] =
		get(remainder).map(suffix => (prefix ++ suffix.asInstanceOf[M\~\AnyMapping], None))

}



object Pathfinder {


	
	def guild[Y](joiner :Pathfinder[Nothing, Y]) :PathfinderGuild[Y] = new PathfinderGuild[Y](joiner)

	def guild :PathfinderGuild[Any] = guild[Any](new ReflectedScoutPathfinder)
	
	
	abstract class SingleRootPathfinder[M<:AnyMapping, X, -Y] extends Pathfinder[X, Y] {
		def mapping :M
//		override def apply(fun :X=>Y) :MappingPath[_, _]
		override def get(fun: (X) => Y): Option[MappingPath[M, _ <: AnyMapping]]

		override def get(property: PropertyChain[X, Y]): Option[MappingPath[M, _ <: AnyMapping]] 
	}


	/** A Container of pathfinders dedicated to individual mappings and mediator between them.
	  * Intended as a single pathfinder for a whole mapped schema, determines what mapping (table) resolved properties
	  * are associated with by a member mapping, and will always use the same pathfinder for the same mapping (determined either
	  * directly as the end of a path argument or by the argument type of the property). Apart from being just the router,
	  * if the target pathfinder supports partial resolution (provides non-trivial consume&follow implementations),
	  * after partially resolving a property within a single table it will try to resolve the remainder as a foreign reference to another mapping
	  * using a specially designated pathfinder for that task provided in the constructor.
	  *
	  * So, a process of resolution of <code>(_:Person).personal.favourites.pub.address.street</code> might look similar to the following:
	  * <verbatim>
	  *     1. Determine that Person class corresponds to People mapping and lookup a pathfinder for People;
	  *     2. call people.consume(_) or people.follow(_) with this as successor, hopefully resolving _.personal.favourites.pub as a component of People;
	  *     3. call joiner.consume(_), which would append an appropriate foreign key path element ending with Pubs mapping to the already resolved path,
	  *        without consuming any part of the remaining property,
	  *     4. resolve the end mapping of freshly extended path to a pathfinder pubs;
	  *     5. call pubs.follow(People\Personal\Favourites\Pub\~Pubs, _.address.street, this) (or a corresponding consume() variant),
	  *        which should resolve address.street as a nested column of Pubs, finally returning People\Personal\Favourites\Pub\~Pubs\Addresses\street.
	  *  </verbatim>
	  *  @param tables root mappings recognized by this instance indexed by corresponding reflected type of the mapped entity; all properties 'definedFor' this type (or a subtype)
	  *         will be assumed to refer to the associated mapping
	  *  @param byTable pathfinders for all mappings defined as values of tables; all resolution of paths starting with that mapping
	  *                 (as determined either by property.definedFor type or the end of the path prefix) will be routed to the associated pathfinder
	  *  @param joiner a special pathfinder to be called after partial resolution result from a table pathfinder, responsible for mapping table relationships into path elements.
	  */
	class PathfinderGuild[Y](val tables :Map[Type, AnyMapping], val byTable :Map[AnyMapping, Pathfinder[_, Y]], val joiner :Pathfinder[Nothing, Y])
		extends Pathfinder[Nothing, Y]
	{
		def this(joiner :Pathfinder[Nothing, Y]) = this(Map(), Map(), joiner)

		override def get(fun: (Nothing) => Y): Option[MappingPath[_ <: AnyMapping, _ <: AnyMapping]] = None

		override def get(property: Nothing ===> Y): Option[MappingPath[_ <: AnyMapping, _ <: AnyMapping]] =
			for {
				table <- root(property.definedFor)
				pathfinder <- byTable.get(table)
				path <- pathfinder.asInstanceOf[Pathfinder[Nothing, Y]].follow(ComponentPath.self(table), property, this)
			} yield path

		def get[S<:AnyMapping, T<:AnyMapping](path :MappingPath[S, T]) :Option[PropertyChain[_, _]] = None

		override def follow[S <: AnyMapping, M <: AnyMapping](path: MappingPath[S, M], property: Nothing ===> Y): Option[MappingPath[S, _ <: AnyMapping]] =
			follow(path, property, this)

		override def follow[S <: AnyMapping, M <: AnyMapping, Z <: Y](path: MappingPath[S, M], property: Nothing===>Z, successor: Pathfinder[Nothing, Z]): Option[MappingPath[S, _ <: AnyMapping]] =
			byTable.get(path.end).flatMap {
				pathfinder => pathfinder.asInstanceOf[Pathfinder[Nothing, Y]].follow(path, property, successor)
			} orElse
				joiner.consume(path, property).flatMap {
					case (prefix, None) => Some(prefix)
					case (prefix, Some(suffix)) =>
//						follow(prefix, suffix, successo)
						successor.follow(prefix, suffix) //orElse
//							prefix.prefixesDesc.toStream.map()
				}


		override def consume[S<:AnyMapping, M<:AnyMapping, Z <: Y](path :MappingPath[S, M], property: Nothing ===> Z): Option[(MappingPath[S, _ <: AnyMapping], Option[_===>Z])] =
			(byTable.get(path.end).flatMap { //let's check if the path ends at one of the tables we have a pathfinder for
					pathfinder => pathfinder.asInstanceOf[Pathfinder[Nothing, Y]].consume(path, property)
				} orElse //maybe it's an inter-table join then?
					joiner.consume(path, property).map { //yes!
						case (consumed, Some(tail)) => //let's see if we can continue from here
							consume(consumed, tail) orElse { //most probably the join ends with a table
								for { //but it might end with a table component
									split <- consumed.splitsReversed.toStream.takeWhile(_.prefix.length>=path.length)
									components <- byTable.get(split.suffix.start) //suffix probably represents a component path in a table
									suffixProperty <- components.get(split.suffix) //lets get the property for the target component and try consuming from the table mapping
									(component, tail) <- components.consume(split.prefix, suffixProperty.asInstanceOf[Any===>Nothing] andThen tail.asInstanceOf[Nothing===>Z])
								} yield (component, tail)
							}.headOption getOrElse (consumed, Some(tail)) //can't do anything else? return the result obtained from the joiner
						case finalResult => finalResult //(result, None) - consumed the whole property
					}
			).map {
				case (partial, Some(remaining)) =>
					consume(partial, remaining) getOrElse (partial, Some(remaining))
				case finalResult => finalResult
			}

		private def get(argType :Type) :Option[Pathfinder[_, Y]] =
			root(argType).flatMap(byTable.get)
		
		private def root(argType :Type) :Option[AnyMapping] =
			tables.get(argType) orElse tables.flatMap {
				case (tpe, mapping) => mapping.providing(argType <:< tpe)
			}.providingOrElse(_.size <= 1,
		        throw new IllegalArgumentException(s"PathfinderGuild.get: several pathfinders match source type $argType")
			).headOption


		/** Add a new pathfinder to the guild, returning a new guild without modifying this one */
		def enlist[X :TypeTag, Z<:Y](pathfinder :SingleRootPathfinder[_<:Mapping[X], X, Y]) :PathfinderGuild[Z] =
			enlist(pathfinder.mapping, pathfinder)

		/** Add a new pathfinder to the guild, returning a new guild without modifying this one */
		def enlist[X :TypeTag, Z<:Y](mapping :AnyMapping, pathfinder :Pathfinder[X, Z]) :PathfinderGuild[Z] =
			if (tables.contains(typeOf[X]) || byTable.contains(mapping))
				throw new IllegalArgumentException(s"Attempt to enlist a pathfinder for an already handled type/mapping ${typeOf[X]}/${mapping} (${tables.get(typeOf[X])}: $pathfinder")
			else
				new PathfinderGuild[Z](tables + (typeOf[X]->mapping), byTable + (mapping->pathfinder), joiner)

		/** Add a new pathfinder to the guild, returning a new guild without modifying this one */
		def enlist[X :TypeTag](mapping :Mapping[X]) :PathfinderGuild[Y] =
			enlist(new ReflectedComponentIndex[Mapping[X], X](mapping))
	}


	/** Resolution of inter-table relationships. Will inspect a mock result of the end mapping of already resolved path, and
	  * if it contains join information (i.e, it is a MappingReference), will append an appropriate JoinPath to the path prefix
	  * and consume any 'accessor' methods resulting from calling 'fetch' (via a NavigableReference) on the reference associated with
	  * the path's end mapping from the start of the property chain.
	  */
	class ReflectedScoutPathfinder extends Pathfinder[Nothing, Any] {
		import NavigableReference._
		private val getters = Set[PropertyChain[_, _]](SingletonFetchProperty, CompositeFetchProperty, ReferenceGetProperty, OptionReferenceGetProperty, IterableReferenceGetProperty)

		override def get(fun: (Nothing) => Any): Option[MappingPath[_ <: AnyMapping, _ <: AnyMapping]] = None

		override def get(property: Nothing===>Any): Option[MappingPath[_ <: AnyMapping, _ <: AnyMapping]] = None

		def get[S<:AnyMapping, T<:AnyMapping](path :MappingPath[S, T]) :Option[PropertyChain[_, _]] = None

		override def consume[S <: AnyMapping, M <: AnyMapping, Z <: Any](path: MappingPath[S, M], property: Nothing===> Z): Option[(MappingPath[S, _ <: AnyMapping], Option[_ ===> Z])] =
			path.end.scoutValue match {
				case Some(Scout(join)) =>
					if (getters(property))
						path.concat(join).map((_, None))
					else
						path.concat(join).flatMap{ prefix =>
							getters.toStream.map(property.drop(_)).collectFirst { case Some(suffix) => (prefix, Some(suffix)) }
						}
				case _ => None
			}
	}


	/** Dead end - doesn't resolve anything, always returning None or throwing NoSuchElementException if it is not possible. */
	object BlindPathfinder extends Pathfinder[Nothing, Any] {

		override def follow[S <: AnyMapping, M <: AnyMapping, Z](path: MappingPath[S, M], property: Nothing ===> Z, continuation: Pathfinder[Nothing, Z]) =
			None

		override def get(property: Nothing ===> Any): Option[MappingPath[_ <: AnyMapping, _ <: AnyMapping]] = None

		override def get(fun: (Nothing) => Any) = None

		def get[S<:AnyMapping, T<:AnyMapping](path :MappingPath[S, T]) :Option[PropertyChain[_, _]] = None

		override def apply(fun: (Nothing) => Any) =
			throw new NoSuchElementException("BlindPathfinder")
	}





}






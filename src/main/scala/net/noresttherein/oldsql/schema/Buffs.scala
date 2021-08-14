package net.noresttherein.oldsql.schema

import scala.annotation.tailrec
import scala.collection.immutable.{ArraySeq, Iterable, LinearSeq}
import scala.collection.{Factory, IterableFactory, IterableOps}
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.BuffMappingFailureException
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Buff.{BuffType, TypedBuffType}
import net.noresttherein.oldsql.schema.Buffs.{BuffsFactory, BuffsZipper, DeclaredBuffs, EmptyDeclaration, HierarchicalBuffs, Tag}
import net.noresttherein.oldsql.schema.Mapping.MappingOf






/** A multi-level collection of [[net.noresttherein.oldsql.schema.Buff buffs]] attached to a component or column.
  * It is a stack of individual declarations, where every declaration consists of a buff collection
  * [[net.noresttherein.oldsql.schema.Buffs.front front]] of any size. Aside from the buffs, each declaration
  * is associated with a unique [[net.noresttherein.oldsql.schema.Buffs.tag tag]], which represents the source/location
  * of the declaration (that is, the mapping class). No information about the source itself is available and this tag
  * is used only in comparisons do determine if two buff collections of different `Buffs` instances originate
  * from the same original declaration. This is because all elements of this linked list following the first one,
  * starting with the [[net.noresttherein.oldsql.schema.Buffs.inherited inherited]] reference, and ending with
  * an empty `Buffs` instance, represent buff declarations made in some supercomponents of the component from which
  * came the first declaration. They are not however taken from those mappings directly, forming a tree of sorts,
  * but are adapted in order to change the buff type from `Buff[S]` of some composite type `S` to `Buff[T]`
  * of some its property `T`; other buffs may not be inherited at all, leading to otherwise untraceable changes.
  *
  * When viewed as a single collection, all declarations are flattened into a single sequence. Individual buff
  * collections are not declared as sequences in order to potentially allow optimisations or variance in behaviour
  * such as detection of duplicates, but the order of their elements is important and consistent between all program
  * runs. This is because relative order of buffs in the collection is important for the purpose of determining
  * if and what buff of a given [[net.noresttherein.oldsql.schema.Buff.BuffType BuffType]] is present, including
  * shadowing of following competing buffs and stopping searches by earlier occurrences of conflicting buffs.
  *
  * The elements of the stack do not have to represent a consecutive path from the root table mapping to a component:
  * any mapping can clear its buffs collection, rejecting inherited declarations, leading to broken chains.
  * Additionally, consecutive empty elements are elided: an empty declaration can typically be encountered only
  * in two places: a synthetic terminator of the list corresponding to no actual mapping, or the top/first link,
  * corresponding to a mapping which does not declare any buffs of its own. The latter stems from an important invariant
  * mandating that the tag of the first declaration of a `Buffs` instance being a value of
  * [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] property of [[net.noresttherein.oldsql.schema.Mapping Mapping]],
  * which is necessary to match derived declarations with the originating mapping. On the internal positions
  * of the stack, this is largely irrelevant and compactness motivated by efficiency takes priority.
  *
  * All collection operations which return another `Buffs` instance retain its structure, with the exception of
  * removing empty declarations from the list. In particular, the first declaration can also be removed from the stack
  * as the result of `filter` or `flatMap`, potentially affecting the above mentioned invariant - if the result is
  * intended to be used by the 'same' mapping, check for this specifically. Where not possible, a generic
  * `Iterable[Buff[T]]` is returned instead.
  *
  * The buffs from the first - 'most recent' - declaration are available
  * as [[net.noresttherein.oldsql.schema.Buffs.front front]] property of this trait, but also a dedicated
  * [[net.noresttherein.oldsql.schema.Buffs.DeclaredBuffs DeclaredBuffs]] view over a `Buffs` instance which
  * limits its operations only to the elements of `front` collection, and which works similarly
  * to [[scala.collection.ArrayOps ArrayOps]] in that all transforming methods return a modified `Buffs` instance
  * (with the same `inherited` suffix as the original), rather than another `DeclaredBuffs` or the produced collection
  * of buffs. It is available as [[net.noresttherein.oldsql.schema.Buffs.declared declared]] property and makes
  * modifying the top declaration easier in a single chained expression.
  *
  * @see [[net.noresttherein.oldsql.schema.Buff]]
  * @see [[net.noresttherein.oldsql.schema.Buffs.DeclaredBuffs]]
  * @see [[net.noresttherein.oldsql.schema.Buffs.Tag]]
  * @author Marcin Mościcki
  */ //todo: reverse order - make later buffs override earlier. This changes prepend to append and +/: to /
trait Buffs[T] extends Iterable[Buff[T]] with IterableOps[Buff[T], Iterable, Buffs[T]] with Serializable {
	/** A zipper over this list of buff declarations: a sort of an immutable, bidirectional iterator which allows
	  * applying changes to this instance at its current position (declaration).
	  */
	def zipper :BuffsZipper[T] = new BuffsZipper(this)

	/** A value unique to this declaration, preserved by the transformation and cascading processes and identifying
	  * the originating mapping for future comparison with other `Buffs` instances. It is often the `Mapping` instance
	  * which declared this buff collection; note that [[net.noresttherein.oldsql.schema.Buffs.inherited inherited]]
	  * buffs contain their own tags (likely the enclosing components of the component declaring this instance).
	  */
	def tag :Tag

	/** Buff declarations inherited from supercomponents of the component owning this instance. This is a linked list
	  * ending with an empty `Buffs` instance (one where both [[net.noresttherein.oldsql.schema.Buffs.front front]]
	  * and [[net.noresttherein.oldsql.schema.Buffs.inherited inherited]] are empty).
	  */
	def inherited :Buffs[T]

	/** The buffs from the declaration of this instance. Can be empty. The order of the buffs in the collection
	  * is significant, consistent with the order in which they have been provided to the factory method,
	  * same between different runs and preserved by transformation methods. The type of the collection used
	  * may vary, with [[net.noresttherein.oldsql.collection.Unique Unique]]
	  * and [[java.util.LinkedHashSet LinkedHashSet]] being possible alternatives to [[Seq]] types.
	  */ //todo: rename to top
	def front :Iterable[Buff[T]]

	/** A view of this instance as its [[net.noresttherein.oldsql.schema.Buffs.front front]] collection of buffs,
	  * but which produces other `Buffs[T]` instances by transplanting unmodified
	  * [[net.noresttherein.oldsql.schema.Buffs.inherited inherited]] suffixes to the result of any collection
	  * transformation method.
	  */
	@inline final def declared :DeclaredBuffs[T] = new DeclaredBuffs[T](this)

	/** Searches for a buff of the given type in this collection, by testing every element in one declaration after
	  * another until either [[net.noresttherein.oldsql.schema.Buff.BuffType.get buff.get]] method matches a buff,
	  * or a [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradicting]] buff is encountered,
	  * in which case the search is aborted.
	  */
	def apply(buff :BuffType) :Opt[Buff[T]]

	/** Searches for a buff of the given type in this collection, by testing every element in one declaration after
	  * another until either [[net.noresttherein.oldsql.schema.Buff.TypedBuffType.unapply buff.unapply]] method matches a buff,
	  * or a [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradicting]] buff is encountered,
	  * in which case the search is aborted.
	  */
	def apply[B[X] <: Buff[X]](buff :TypedBuffType[B]) :Opt[B[T]]

	/** Creates a new `Buffs` stack with this instance as result's
	  * [[net.noresttherein.oldsql.schema.Buffs.inherited inherited]] property and the given buffs as its first
	  * declaration. The [[net.noresttherein.oldsql.schema.Buffs.tag tag]] of the declaration will be some new, unique
	  * but unspecified value.
	  * @return `declare(Tag(), buffs: _*)`.
	  */
	def declare(buffs :Buff[T]*) :Buffs[T] = declare(Tag(), buffs :_*)

	/** Creates a new `Buffs` stack with this instance as result's
	  * [[net.noresttherein.oldsql.schema.Buffs.inherited inherited]] property and the given buffs as its first
	  * declaration. The [[net.noresttherein.oldsql.schema.Buffs.tag tag]] of the declaration will carry use
	  * the given mapping both when printing and for equality checks.
	  * @return `declare(Tag(source), buffs: _*)`.
	  */
	def declare(source :MappingOf[T], buffs :Buff[T]*) :Buffs[T] = declare(Tag(source), buffs :_*)

	/** Creates a new `Buffs` stack with this instance as result's
	  * [[net.noresttherein.oldsql.schema.Buffs.inherited inherited]] property and the given buffs as its first
	  * declaration identified by the given tag. This is useful when copying or modifying declarations and wanting
	  * to preserve their identity.
	  */
	def declare(source :Tag, buffs :Buff[T]*) :Buffs[T] =
		if (front.isEmpty)
			if (inherited.isEmpty) Buffs(source, buffs :_*)
			else inherited.declare(source, buffs :_*)
		else if (buffs.isEmpty)
			new EmptyDeclaration[T](source, this)
		else
			new HierarchicalBuffs[T](source, buffs, this)

	/** A `Buffs` collection equal to this one, but with the [[net.noresttherein.oldsql.schema.Buffs.tag tag]]
	  * of the first declaration being based on this mapping.
	  * @return `redeclare(Tag(source))`.
	  */
	def redeclare(source :MappingOf[T]) :Buffs[T] = redeclare(Tag(source))

	/** A `Buffs` collection equal to this one, but with the given [[net.noresttherein.oldsql.schema.Buffs.tag tag]]
	  * for its first declaration.
	  * @return `inherited.declare(source, front.toSeq: _*)`.
	  */
	def redeclare(source :Tag) :Buffs[T] =
		if (source == this.tag) this else inherited.declare(source, front.toSeq :_*)

	/** A new `Buffs` instance based on this one, but with `buffs` as its
	  * [[net.noresttherein.oldsql.schema.Buffs.front front]] property.
	  * The [[net.noresttherein.oldsql.schema.Buffs.tag tag]] of the first declaration will remain unchanged.
	  * @return `reset(tag, buffs: _*)`.
	  */
	def reset(buffs :Buff[T]*) :Buffs[T] = reset(tag, buffs :_*)

	/** Swaps the first declaration of this collection to one identified by a
	  * [[net.noresttherein.oldsql.schema.Buffs.Tag Tag]] based on `source` mapping, and consisting of the given `buffs`.
	  * @return `reset(Tag(source), buffs: _*)`.
	  */
	def reset(source :MappingOf[T], buffs :Buff[T]*) :Buffs[T] = reset(Tag(source), buffs :_*)

	/** Swaps the first declaration of this collection to one identified by tag `source` and consisting
	  * of the given `buffs`.
	  * @return `inherited.declare(source, buffs: _*)`.
	  */
	def reset(source :Tag, buffs :Buff[T]*) :Buffs[T] = inherited.declare(source, buffs :_*)

	/** Prepends the given buff ''to the first declaration'' of this collection. Created `Buffs` will have `buff`
	  * as the first element of their [[net.noresttherein.oldsql.schema.Buffs.front front]] property.
	  * @return `reset(buff +: front.toSeq: _*)`.
	  */ //consider: removing it and leaving only in DeclaredBuffs
	def +:(buff :Buff[T]) :Buffs[T] = reset(buff +: front.toSeq :_*)

	/** Prepends the given buffs ''to the first declaration'' of this collection. Created `Buffs`'
	  * [[net.noresttherein.oldsql.schema.Buffs.front front]] property will consist of `buffs` followed by `this.front`.
	  * @return `reset(buffs ++: front.toSeq: _*)`.
	  */
	def ++:(buffs :Iterable[Buff[T]]) :Buffs[T] = reset(buffs ++: front.toSeq :_*)

	/** Appends this collection to the given one. Created buffs will have all the declarations of `buffs`
	  * (with the same tags) followed by the declarations of this collection.
	  */ //todo: better name, this is awful;
	def +/:(buffs :Buffs[T]) :Buffs[T] =
		if (isEmpty) buffs
		else if (buffs.isEmpty) declare(buffs.tag)
		else (buffs.inherited +/: this).declare(buffs.tag, buffs.front.toSeq :_*)


	/** Performs bidirectional mapping of all buffs in this collection using functions which will fail with
	  * a [[net.noresttherein.oldsql.exceptions.BuffMappingFailureException BuffMappingFailureException]]
	  * if one of the extractors doesn't yield a value when required. The error may occur when this method
	  * is executed, when the resulting `Buffs` collection is inspected, or at an undetermined moment in the future
	  * when an an extractor which doesn't produce a value for its argument. The mapping happens using
	  * [[net.noresttherein.oldsql.schema.Buff.bimap Buff.bimap]]; this is not considered cascading and all buffs
	  * of this collection will be mapped. Returned collection will have the same structure (consist of declarations
	  * with the same tags and buff types).
	  * This method forwards to `this.bimap` after converting both `map` and `unmap` to true functions.
	  * @see [[net.noresttherein.oldsql.schema.Buffs.bimap]]
	  */
	def unsafeBimap[S](map :T =?> S, unmap :S =?> T) :Buffs[S] = this.map(bimapBuff(map, unmap))

	/** Performs a bidirectional mapping of all buffs in this collection using the given function pair.
	  * The mapping happens using the buffs' [[net.noresttherein.oldsql.schema.Buff.bimap bimap]] methods: providing
	  * two functions allows to map [[net.noresttherein.oldsql.schema.Buff.AuditBuff AuditBuff]]s by sandwiching
	  * their functions between the pair. This is not considered cascading: all buffs of this collection will
	  * have a counterpart in the returned instance, organized into the same declarations in the same order.
	  */
	def bimap[S](map :T => S, unmap :S => T) :Buffs[S] = this.map(_.bimap(map, unmap))

	protected def bimapBuff[S](map :T =?> S, unmap :S =?> T)(buff :Buff[T]) :Buff[S] =
		buff.bimap(
			map.requisite getOrElse {
				t :T => map.opt(t) getOrElse {
					throw new BuffMappingFailureException(
//							s"Failed mapping buff $buff from $tag: could not map value $t."
						tag.source match {
							case Some(mapping) =>
								s"Failed mapping $mapping: cannot derive buff $buff value from $t."
							case _ =>
								s"Failed mapping buff $buff $tag: could not map value $t."
						}
					)
				}
			},
			unmap.requisite getOrElse {
				s :S => unmap.opt(s) getOrElse {
					throw new BuffMappingFailureException(
//							s"Failed mapping buff $buff from $tag: could not unmap value $s."
						tag.source match {
							case Some(mapping) =>
								s"Failed mapping $mapping: cannot derive buff $buff value from $s."
							case _ =>
								s"Failed mapping buff $buff $tag: could not unmap value $s."
						}
					)
				}
			}
		)

	/** Adapts the buffs of this collection to subject type `S` derivable from `T`. Created `Buffs` will have
	  * a structure homomorphic with this one: individual buffs which do not
	  * [[net.noresttherein.oldsql.schema.Buff.cascades cascade]] may be missing - in particular all
	  * [[net.noresttherein.oldsql.schema.Buff.AuditBuff audit buffs]] - and declarations which become empty
	  * as the result will be elided, but all mapped buffs remain in the same order and
	  * the [[net.noresttherein.oldsql.schema.Buffs.tag tags]] those declarations which remain are preserved.
	  * @return `flatMap(_.cascade(f))`.
	  */
	def cascade[S](f :T => S) :Buffs[S] = flatMap(_.cascade(f))

	/** Adapts the buffs of this collection to subject type `S` derivable from `T` with a function obtained from
	  * the extractor. Created `Buffs` will have a structure homomorphic with this one: individual buffs which do not
	  * [[net.noresttherein.oldsql.schema.Buff.cascades cascade]] may be missing - in particular all
	  * [[net.noresttherein.oldsql.schema.Buff.AuditBuff audit buffs]] - and declarations which become empty
	  * as the result will be elided, but all mapped buffs remain in the same order and
	  * the [[net.noresttherein.oldsql.schema.Buffs.tag tags]] those declarations which remain are preserved.
	  *
	  * This variant may fail with
	  * a [[net.noresttherein.oldsql.exceptions.BuffMappingFailureException BuffMappingFailureException]]
	  * if the given extractor is undefined for a required argument. This might happen when executing this method,
	  * but if buffs with values from by-name expressions are present, it may also be at an unpredictable point
	  * in the future, when an unsupported value of `T` is encountered.
	  */
	def unsafeCascade[S](f :T =?> S) :Buffs[S] = f.requisite match {
		case Got(g) => cascade(g)
		case _ => flatMap { _.cascadeGuard(f) }
	}



	override def filter(f :Buff[T] => Boolean) :Buffs[T] = filterMatching(f, true)
	override def filterNot(f :Buff[T] => Boolean) :Buffs[T] = filterMatching(f, false)
	protected def filterMatching(f :Buff[T] => Boolean, value :Boolean) :Buffs[T] =
		(newSpecificBuilder ++= (if (value) iterator.filter(f) else iterator.filterNot(f))).result()

	/** Maps this collection to a `Buffs` instance for another subject type. The returned instance will have the same
	  * structure - same number and order of declarations, with the same tags, and same number of buffs in the order
	  * in which they were mapped - as this one, but the mapping function can possibly change both buff types and kinds.
	  */
	def map[S](f :Buff[T] => Buff[S]) :Buffs[S]

	/** Maps this collection to a `Buffs` instance for another subject type. Mapping happens individually for each
	  * element of this list and the returned instance will have the same declarations (with the same tags), but
	  * their contents can arbitrarily differ and empty declarations will be removed. Note that it is thus possible
	  * that the first declaration will have a different [[net.noresttherein.oldsql.schema.Buffs.tag tag]] than this one.
	  */
	def flatMap[S](f :Buff[T] => IterableOnce[Buff[S]]) :Buffs[S]

	/** Transforms this collection to a different subject type by selectively mapping its contents.
	  * Mapping happens individually for each element of this list and the returned instance will have the same
	  * declarations (with the same tags), preserving both their order and that of buffs within them, but some buffs
	  * will not have their counterparts and emptied declarations will be removed. Note that it is thus possible
	  * that the first declaration will have a different [[net.noresttherein.oldsql.schema.Buffs.tag tag]] than this one.
	  */
	def collect[S](f :PartialFunction[Buff[T], Buff[S]]) :Buffs[S] =
		flatMap(f.andThen(Option(_)).applyOrElse(_, (_ :Any) => None))


	override def knownSize :Int = size
	override def isEmpty :Boolean = size == 0

	override def empty :Buffs[T] = Buffs.empty(tag)

	protected override def fromSpecific(coll :IterableOnce[Buff[T]]) :Buffs[T] =
		new BuffsFactory[T](tag).fromSpecific(coll)

	protected override def newSpecificBuilder :Builder[Buff[T], Buffs[T]] = Buffs.newBuilder(tag)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[Buffs[_]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case buffs :Buffs[_] if buffs canEqual this =>
			tag == buffs.tag && size == buffs.size &&
				(front == buffs.front) && (inherited == buffs.inherited)
		case _ => false
	}

	def sameElements[E >: Buff[T]](that :IterableOnce[E]) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case buffs :Buffs[_] => size == buffs.size && (iterator sameElements buffs.iterator)
		case _ => that.knownSize match {
			case n if n >= 0 => size == n && (iterator sameElements that.iterator)
			case _ => iterator sameElements that.iterator
		}
	}

	override lazy val hashCode :Int = iterator.hashCode

	protected override def className :String = "Buffs"
}






/** Base class only used to bring the implicit conversion
  * [[net.noresttherein.oldsql.schema.ImplicitBuffs.toBuffFactory toBuffFactory]] to scope for
  * the [[net.noresttherein.oldsql.schema.Buffs$ Buffs]] companion object.
  */
sealed abstract class ImplicitBuffs

object ImplicitBuffs {
	/** Converts the [[net.noresttherein.oldsql.schema.Buffs$ Buffs]] companion object to a [[Factory]]
	  * for `Buffs` of any type parameter. This allows its use with [[Iterable.to to]] method of standard collections,
	  * as long as the element type is `Buffs[T]`:
	  * {{{
	  *     Seq(NoSelect[Int]).to(Buffs)
	  * }}}
	  */
	@inline implicit def toBuffFactory[T](buffs :Buffs.type) :Factory[Buff[T], Buffs[T]] = Buffs.factory[T]
}



object Buffs extends ImplicitBuffs {

	def apply[T](source :Tag, buffs :Buff[T]*) :Buffs[T] =
		if (buffs.isEmpty) new EmptyBuffs(source)
		else if (buffs.sizeIs <= 1) new BuffsSingleton[T](source, buffs.head)
		else new HierarchicalBuffs[T](source, buffs.toArray[Buff[T]])

	def apply[T](source :MappingOf[T], buffs :Buff[T]*) :Buffs[T] = apply(Tag(source), buffs :_*)
	def apply[T](buffs :Buff[T]*) :Buffs[T] = apply(Tag(), buffs :_*)

	def single[T](source :Tag, buff :Buff[T]) :Buffs[T] = new BuffsSingleton[T](source, buff)
	def single[T](source :MappingOf[T], buff :Buff[T]) :Buffs[T] = new BuffsSingleton[T](Tag(source), buff)
	def single[T](buff :Buff[T]) :Buffs[T] = new BuffsSingleton[T](Tag(), buff)

	def empty[T](source :Tag) :Buffs[T] = new EmptyBuffs[T](source)
	def empty[T](source :MappingOf[T]) :Buffs[T] = new EmptyBuffs[T](Tag(source))
	def empty[T] :Buffs[T] = emptyInstance.asInstanceOf[Buffs[T]]

	private[this] val emptyInstance = new EmptyBuffs[Any](Tag())


	@inline def unapply[T](buffs :collection.Iterable[Buff[T]]) :Opt[(Iterable[Buff[T]], Buffs[T])] =
		buffs match {
			case matched :Buffs[T] => Got(matched.front, matched.inherited)
			case _ => Lack
		}

	def newBuilder[T](source :Tag) :Builder[Buff[T], Buffs[T]] =
		Array.newBuilder[Buff[T]].mapResult { buffs => buffs.length match {
			case 0 => empty[T](source)
			case 1 => single[T](source, buffs(0))
			case _ => new HierarchicalBuffs[T](source, buffs)
		}}

	def newBuilder[T] :Builder[Buff[T], Buffs[T]] = newBuilder(Tag())

	def factory[T](source :Tag) :Factory[Buff[T], Buffs[T]] = new BuffsFactory[T](source)

	def factory[T] :Factory[Buff[T], Buffs[T]] = new BuffsFactory[T](Tag())

	private class BuffsFactory[T](val source :Tag) extends AnyVal with Factory[Buff[T], Buffs[T]] {
		override def fromSpecific(it :IterableOnce[Buff[T]]) :Buffs[T] = it match {
			case buffs :Buffs[T] if buffs.tag == source => buffs
			case buffs :Buffs[T] => buffs.redeclare(source)
			case iter :Iterable[Buff[T]] => new HierarchicalBuffs[T](source, iter.toArray[Buff[T]])
			case _ => new HierarchicalBuffs[T](source, it.iterator.toArray[Buff[T]])
		}
		override def newBuilder = Buffs.newBuilder[T](source)
	}



	/** A tag attached to every declaration in [[net.noresttherein.oldsql.schema.Buffs Buffs]] in order to allow
	  * its identification. It represents the source of declaration and t is often wrapping
	  * the [[net.noresttherein.oldsql.schema.Mapping mapping]] where the buffs were declared, although if
	  * not specified when creating a `Buffs` instance, a new, unique `Tag` will be created for this purpose.
	  * The tag given as [[net.noresttherein.oldsql.schema.Buffs.tag tag]] property of `Buffs` is preserved
	  * through all mapping and similar operations, so the mapping may have any subject type, unrelated to
	  * `Buff`'s type parameter. Typically, if a component declares non-empty
	  * [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]], it will include
	  * [[net.noresttherein.oldsql.schema.Buffs.cascade cascaded]] buffs from its directly enclosing `Mapping`,
	  * or the first enclosing `Mapping` with non-empty [[net.noresttherein.oldsql.schema.Buffs.declared declared]]
	  * buff list, as its [[net.noresttherein.oldsql.schema.Buffs.inherited inherited]] buffs.
	  *
	  * Tags are serializable, but just as with mappings, they do not preserve equality after deserialization
	  */
	class Tag extends Serializable {
		def source :Option[Mapping] = None
		override def toString :String = "@" + System.identityHashCode(this)
	}

	object Tag {
		/** A unique, anonymous tag. */
		def apply() :Tag = new Tag

		/** A unique tag using the given by-name expression in its `toString`. This debug information may be useful
		  * when cascading involves unsure extractors and may fail at a later point. This tag still uses
		  * referential equality, each call to this method, even with the same constant argument, will yield
		  * a unique instance.
		  */
		def apply(location: => String) :Tag = new Tag {
			override def toString :String = "@" + location
		}

		/** A unique tag specifying the given mapping as its declaration place. An ideal argument for a `Tag`
		  * constructor, it will be used whenever this tag will get printed, including exception messages.
		  * It is particularly useful when optional components result in unsafe cascading of buffs from the original
		  * declaration to an inherited one. This implementation defines equality as equality of the wrapped mappings:
		  * all tags created with the same argument will be equal to each other. Note however that `Mapping` uses
		  * referential equality and does not preserve identity after serialization and deserialization.
		  */
		def apply(mapping :Mapping) :Tag = new Tag {
			override val source = Some(mapping)
			override def equals(that :Any) :Boolean = that match {
				case tag :Tag => (tag eq this) || source == tag.source
			}
			override def hashCode = mapping.hashCode
			override def toString = "@" + source.get
		}

		def unapply(buffs :Buffs[_]) :Opt[Tag] = Got(buffs.tag)
	}



	/** A view of the top/first declaration of a [[net.noresttherein.oldsql.schema.Buffs Buffs]] collection.
	  * All collection methods operate only on the [[net.noresttherein.oldsql.schema.Buffs.front front]] property:
	  * methods returning a standard collection, searching for items, etc, are limited to the buffs in that declaration.
	  * While this class is [[IterableOnce]], the specific collection type of extended [[IterableOps]] is `Buffs[T]`.
	  * All non-conversion methods return `Buffs[T]` instead of `DeclaredBuffs[T]`, simply stacking all processed buffs
	  * on the inherited suffix `Buffs`, which remains unchanged, as is
	  * the [[net.noresttherein.oldsql.schema.Buffs.DeclaredBuffs.source source]] of this declaration (and in returned
	  * `Buffs`). This class is only a light syntactic wrapper over `Buffs`, intended to make the operations on
	  * its first declaration easier, and not as a stand alone collection. Using it in that aspect (i.e. through
	  * polymorphism of `IterableOnce` interface) may lead to inconsistencies or surprising results due to this duality.
	  */
	class DeclaredBuffs[T](val all :Buffs[T])
		extends AnyVal with IterableOnce[Buff[T]] with IterableOps[Buff[T], Iterable, Buffs[T]] with Serializable
	{
		import all.front

		@inline def source :Tag = all.tag
		def apply(buff :BuffType) :Opt[Buff[T]] = buff.get(front)
		def apply[B[X] <: Buff[X]](buff :TypedBuffType[B]) :Opt[B[T]] = buff.unapply(front)

		@inline private def reset(buffs :scala.Iterable[Buff[T]]) :Buffs[T] = all.reset(buffs.toSeq :_*)

		def +:(buff :Buff[T]) :Buffs[T] = reset(buff +: front.toSeq)
		def ++:(buffs :Iterable[Buff[T]]) :Buffs[T] = reset(front match {
			case list :LinearSeq[Buff[T]] => buffs ++: list
			case _ => buffs ++ front
		})

		override def filter(f :Buff[T] => Boolean) :Buffs[T] = reset(front.view.filter(f))
		override def filterNot(f :Buff[T] => Boolean) :Buffs[T] = reset(front.view.filterNot(f))
		def map(f :Buff[T] => Buff[T]) :Buffs[T] = reset(front.map(f))
		def flatMap(f :Buff[T] => IterableOnce[Buff[T]]) :Buffs[T] = reset(front.flatMap(f))
		def collect(f :PartialFunction[Buff[T], Buff[T]]) :Buffs[T] = reset(front.collect(f))

		override def foreach[U](f :Buff[T] => U) :Unit = front.foreach(f)
		override def iterator :Iterator[Buff[T]] = front.iterator


		@inline override def toIterable :Iterable[Buff[T]] = front
		@inline override def toSet[U >: Buff[T]] :Set[U] = front.toSet
		@inline override def toSeq :Seq[Buff[T]] = front.toSeq
		@inline override def toIndexedSeq :IndexedSeq[Buff[T]] = front.toIndexedSeq
		@inline override def toVector :Vector[Buff[T]] = front.toVector
		@inline override def toList :List[Buff[T]] = front.toList
		@inline override def toArray[U >: Buff[T] :ClassTag] :Array[U] = front.toArray[U]
		@inline protected override def coll :Buffs[T] = all
		@inline def seq :Seq[Buff[T]] = front.toSeq

		override def knownSize :Int = all.knownSize
		override def size :Int = all.size
		override def isEmpty :Boolean = all.isEmpty

		protected override def fromSpecific(coll :IterableOnce[Buff[T]]) :Buffs[T] =
			coll match {
				case iterable :Iterable[Buff[T]] if iterable.isEmpty =>
					new EmptyDeclaration[T](all.tag, all.inherited)
				case iterable :Iterable[Buff[T]] =>
					new HierarchicalBuffs[T](all.tag, iterable.toArray[Buff[T]], all.inherited)
				case iterator :Iterator[Buff[T]] if !iterator.hasNext =>
					new EmptyDeclaration[T](all.tag, all.inherited)
				case _ =>
					new HierarchicalBuffs[T](all.tag, coll.iterator.toArray[Buff[T]], all.inherited)
			}

		protected override def newSpecificBuilder :Builder[Buff[T], Buffs[T]] =
			Array.newBuilder[Buff[T]].mapResult { buffs => buffs.length match {
				case 0 => new EmptyDeclaration[T](this.coll.tag, this.coll.inherited)
				//				case 1 => Buffs.single[T](this.coll.tag, buffs(0))
				case _ => new HierarchicalBuffs[T](this.coll.tag, buffs, this.coll.inherited)
			}}

		override def iterableFactory :IterableFactory[Iterable] = Iterable
	}



	/** A zipper over a [[net.noresttherein.oldsql.schema.Buffs Buffs]], treating them as a stack of declarations.
	  * It works similarly to a bidirectional iterator, allowing modifications and using property
	  * [[net.noresttherein.oldsql.schema.Buffs.inherited inherited]] of `Buffs` to move deeper down the stack
	  * (right in the notation) and keeping internally a stack of 'skipped' prefix of the buffs collection.
	  */
	final class BuffsZipper[T] private (private val prefix :Opt[BuffsZipper[T]], val stack :Buffs[T])
		extends Serializable
	{
		def this(buffs :Buffs[T]) = this(Lack, buffs)

		/** Is the underlying `Buffs` instance empty? */
		def isEmpty :Boolean = stack.isEmpty && prefix.isEmpty

		/** Does this zipper point to the first element of the underlying `Buffs`? If so, then `this.stack` represents
		  * the whole underlying `Buffs`. */
		def isTop :Boolean = prefix.isEmpty

		/** Does this zipper point after the last element of the underlying `Buffs`? If so, then `this.stack.isEmpty`. */
		def isBottom :Boolean = stack.isEmpty

		/** Current declaration's [[net.noresttherein.oldsql.schema.Buffs.tag tag]]. */
		def source :Tag = stack.tag

		/** Buffs from the current declaration. */
		def top :Iterable[Buff[T]] = stack.front

		/** The underlying `Buff` collection, with all changes applied. */
		def buffs :Buffs[T] = |<<.stack

		/** Skip to the previous declaration. After this operation `stack.inherited` will equal current `stack`. */
		@throws[NoSuchElementException]("if the zipper points to the start of the collection.")
		def << :BuffsZipper[T] = prefix match {
			case Got(zip) if stack eq zip.stack.inherited => zip
			case Got(zip) => new BuffsZipper(zip.prefix, stack.declare(zip.stack.tag, zip.stack.front.toSeq :_*))
			case _ => throw new NoSuchElementException("Buffs zipper end.")
		}

		/** Skip to the inherited declarations of current `Buffs`.
		  * After this operation `stack` will equal current `stack.inherited`.
		  */
		@throws[NoSuchElementException]("if the zipper points to the end of the collection.")
		def >> :BuffsZipper[T] =
			if (buffs.isEmpty)
				throw new NoSuchElementException("Buffs zipper end.")
			else new BuffsZipper(Got(this), stack.inherited)

		/** Skip to the end of the collection. After this operation `stack` will be empty. */
		@tailrec def >>| :BuffsZipper[T] = if (isBottom) this else >>.>>|

		/** Skip to the start of the collection. After this operation the zipper will point to the first element
		  * of the collection and `stack` will become the underlying `Buffs` instance with all changes applied. */
		@tailrec def |<< :BuffsZipper[T] = if (isTop) this else <<.|<<

		/** Skip to the following declaration with the given [[net.noresttherein.oldsql.schema.Buffs.tag tag]]
		  * or the bottom of the stack, if none such exists. */
		@tailrec def :>>(tag :Tag) :BuffsZipper[T] = if (stack.tag == tag || isBottom) this else >> :>> tag

		/** Skip to the preceding declaration with the given [[net.noresttherein.oldsql.schema.Buffs.tag tag]]
		  * or the top of the stack, if none such exists. */
		@tailrec def <<:(tag :Tag) :BuffsZipper[T] = if (stack.tag == tag || isTop) this else tag <<: this

		/** Finds the remains of `buffs` in this `Buffs`' suffix. This assumes that the underlying `Buffs`
		  * are derived from the argument, i.e. they are the result of applying a series
		  * of [[net.noresttherein.oldsql.schema.Buffs.cascade cascade]] operations on the argument.
		  * This method is used to identify inherited, cascaded buffs from a mapping in the buffs of one
		  * of its components. The procedure is a heuristic, because cascading can result in elided empty declarations,
		  * and relies on the uniqueness of tags for individual declarations. It first tries to find
		  * a following declaration with the same [[net.noresttherein.oldsql.schema.Buffs.tag tag]]
		  * as the first declaration in `buffs`; if that fails, it searches for the second declaration's tag
		  * and so on. When the method returns, the zipper points to the declaration in the underlying `Buffs`,
		  * at or after the current zipper position, with `tag` same as the first declaration in `buffs`
		  * which has a corresponding declaration following this zipper. If no declarations in `buffs` are preserved
		  * in the underlying `Buffs`, the zipper will point to the bottom of the stack.
		  */
		@tailrec def locate(buffs :Buffs[_]) :BuffsZipper[T] =
			if (buffs.isEmpty) >>|
			else {
				val first = this :>> buffs.tag
				if (first.source == buffs.tag) first
				else locate(buffs.inherited)
			}


		/** Remove the current declaration from the represented `Buffs`.
		  * The zipper will point to the `inherited` element of the current `Buffs` instance. */
		@throws[NoSuchElementException]("if the zipper points to the end of the collection (stack is empty).")
		def del :BuffsZipper[T] =
			if (stack.isEmpty)
				throw new NoSuchElementException("Buffs zipper end.")
			else new BuffsZipper(prefix, stack.inherited)


		/** Add a new declaration before the current element. This invokes
		  * `stack.`[[net.noresttherein.oldsql.schema.Buffs.declare declare]] with the given arguments and moves
		  * the zipper to point to the created declaration. */
		def push(tag :Tag, buffs :Buff[T]*) :BuffsZipper[T] =
			new BuffsZipper(prefix, stack.declare(tag, buffs :_*))

		/** Add a new declaration before the current element. This invokes
		  * `stack.`[[net.noresttherein.oldsql.schema.Buffs.declare declare]] with the given arguments and moves
		  * the zipper to point to the created declaration. */
		def push(buffs :Buff[T]*) :BuffsZipper[T] =
			new BuffsZipper(prefix, stack.declare(buffs :_*))

		/** Replaces the `front` buff list of the `Buffs` instance currently on top of `stack`, without changing
		  * its [[net.noresttherein.oldsql.schema.Buffs.tag tag]].
		  * After the operation the zipper position remains the same, pointing to the new buff list.
		  */
		def set(buffs :Buff[T]*) :BuffsZipper[T] =
			new BuffsZipper(prefix, stack.inherited.declare(stack.tag, buffs :_*))

		/** Replaces the whole suffix (all declarations starting from the current one to the outermost one).
		  * After the operation `stack` will equal `buffs`, while the prefix remains unchanged. */
		def replace(buffs :Buffs[T]) :BuffsZipper[T] = new BuffsZipper(prefix, buffs)
	}



	private class EmptyBuffs[T](override val tag :Tag) extends Buffs[T] {
		override def front :Iterable[Nothing] = Nil
		override def inherited :Buffs[T] = this

		override def apply(buff :BuffType) :Opt[Buff[T]] = Lack
		override def apply[B[X] <: Buff[X]](buff :TypedBuffType[B]) :Opt[B[T]] = Lack

		override def +:(buff :Buff[T]) :Buffs[T] = Buffs.single(tag, buff)
		override def ++:(buffs :Iterable[Buff[T]]) :Buffs[T] = new BuffsFactory[T](tag).fromSpecific(buffs)
		override def +/:(buffs :Buffs[T]) :Buffs[T] = buffs

		override def iterator :Iterator[Buff[T]] = Iterator.empty
		override def foreach[U](f :Buff[T] => U) :Unit = ()
		protected override def filterMatching(f :Buff[T] => Boolean, value :Boolean) :Buffs[T] = this
		override def map[S](f :Buff[T] => Buff[S]) :Buffs[S] = this.asInstanceOf[EmptyBuffs[S]]
		override def flatMap[S](f :Buff[T] => IterableOnce[Buff[S]]) :Buffs[S] = this.asInstanceOf[EmptyBuffs[S]]
		override def collect[S](f :PartialFunction[Buff[T], Buff[S]]) :Buffs[S] = this.asInstanceOf[EmptyBuffs[S]]

		override def size = 0
		override def empty :Buffs[T] = this

		override def sameElements[E >: Buff[T]](that :IterableOnce[E]) :Boolean = that match {
			case iter :Iterable[_] => iter.isEmpty
			case _ => that.iterator.isEmpty
		}
	}



	private class BuffsSingleton[T](override val tag :Tag, buff :Buff[T]) extends Buffs[T] {
		override val front = buff::Nil
		override def inherited = Buffs.empty

		override def apply(buff :BuffType) :Opt[Buff[T]] = buff.get(this.buff)
		override def apply[B[X] <: Buff[X]](buff :TypedBuffType[B]) = buff.unapply(this.buff)

		override def +:(buff :Buff[T]) :Buffs[T] = new HierarchicalBuffs[T](tag, {
			val arr = new Array[Buff[T]](2)
			arr(0) = buff; arr(1) = this.buff
			arr
		})

		override def ++:(buffs :Iterable[Buff[T]]) =
			if (buffs.isEmpty) this
			else buffs.knownSize match {
				case n if n > 0 =>
					val arr = new Array[Buff[T]](1 + n)
					buffs.copyToArray(arr)
					arr(n) = buff
					new HierarchicalBuffs[T](tag, arr)
				case _ =>
					new HierarchicalBuffs[T](tag, (Array.newBuilder[Buff[T]] ++= buffs += buff).result())
			}

		override def iterator = Iterator.single(buff)
		override def foreach[U](f :Buff[T] => U) :Unit = f(buff)

		protected override def filterMatching(f :Buff[T] => Boolean, value :Boolean) :Buffs[T] =
			if (f(buff) == value) this else empty

		override def map[S](f :Buff[T] => Buff[S]) = new BuffsSingleton(tag, f(buff))
		override def flatMap[S](f :Buff[T] => IterableOnce[Buff[S]]) = factory[S](tag).fromSpecific(f(buff))
		override def collect[S](f :PartialFunction[Buff[T], Buff[S]]) :Buffs[S] =
			f.andThen(new BuffsSingleton(tag, _)).applyOrElse(buff, (_ :Any) => Buffs.empty[S](tag))

		override def size = 1
	}



	private class EmptyDeclaration[T](override val tag :Tag, override val inherited :Buffs[T]) extends Buffs[T] {
		override def front :Iterable[Nothing] = Nil

		override def apply(buff :BuffType) = inherited.apply(buff)
		override def apply[B[X] <: Buff[X]](buff :TypedBuffType[B]) = inherited.apply(buff)


		override def +:(buff :Buff[T]) :Buffs[T] =
			new HierarchicalBuffs[T](tag, { val a = new Array[Buff[T]](1); a(0) = buff; a }, inherited)
		override def ++:(buffs :Iterable[Buff[T]]) :Buffs[T] = inherited.declare(tag, buffs.toSeq :_*)
		override def +/:(buffs :Buffs[T]) = buffs +/: inherited

		override def iterator = inherited.iterator
		override def foreach[U](f :Buff[T] => U) :Unit = inherited foreach f
		protected override def filterMatching(f :Buff[T] => Boolean, value :Boolean) =
			inherited.filterMatching(f, value)
		override def map[S](f :Buff[T] => Buff[S]) = inherited.map(f)
		override def flatMap[S](f :Buff[T] => IterableOnce[Buff[S]]) = inherited.flatMap(f)
		override def collect[S](f :PartialFunction[Buff[T], Buff[S]]) = inherited.collect(f)

		override def size = inherited.size
	}




	private class HierarchicalBuffs[T] private[Buffs] (override val tag :Tag, buffs :Array[Buff[T]],
	                                                   override val inherited :Buffs[T] = Buffs.empty[T])
		extends Buffs[T]
	{
		def this(source :Tag, buffs :Iterable[Buff[T]], inherited :Option[Buffs[T]]) =
			this(source, buffs.toArray, inherited getOrElse Buffs.empty[T])

		def this(source :Tag, buffs :Iterable[Buff[T]], inherited :Buffs[T]) =
			this(source, buffs.toArray, inherited)

		def this(source :Tag, buffs :Iterable[Buff[T]]) = this(source, buffs.toArray, Buffs.empty[T])

		private[Buffs] def array :Array[Buff[T]] = buffs
		override val front :Seq[Buff[T]] = ArraySeq.unsafeWrapArray(buffs)
		override val size :Int = buffs.length + inherited.size

		override def apply(buff :BuffType) :Opt[Buff[T]] = {
			var i = 0; val len = buffs.length
			while (i < len) {
				val b = buffs(i)
				if (b.buffType contradicts buff)
					return Lack
				val res = buff.get(buffs(i))
				if (res.isDefined)
					return res
				i += 1
			}
			if (buff.cascades) inherited.apply(buff)
			else Lack
		}

		override def apply[B[X] <: Buff[X]](buff :TypedBuffType[B]) :Opt[B[T]] = {
			var i = 0; val len = buffs.length
			while (i < len) {
				val b = buffs(i)
				if (b.buffType contradicts buff)
					return Lack
				val res = buff.unapply(buffs(i))
				if (res.isDefined)
					return res
				i += 1
			}
			if (buff.cascades) inherited.apply(buff)
			else Lack
		}


		override def +:(buff :Buff[T]) :Buffs[T] =
			new HierarchicalBuffs[T](tag, {
				val arr = new Array[Buff[T]](1 + this.buffs.length)
				arr(0) = buff
				System.arraycopy(this.buffs, 0, arr, 1, this.buffs.length)
				arr
			}, inherited)

		override def ++:(buffs :Iterable[Buff[T]]) :Buffs[T] =
			if (buffs.isEmpty) this
			else buffs.knownSize match {
				case n if n > 0 =>
					val arr = new Array[Buff[T]](n + this.buffs.length)
					buffs.copyToArray(arr)
					System.arraycopy(this.buffs, 0, arr, n, this.buffs.length)
					new HierarchicalBuffs[T](tag, arr, inherited)
				case _ =>
					new HierarchicalBuffs[T](tag, front.view.prependedAll(buffs).toArray[Buff[T]])
			}


		protected override def filterMatching(f :Buff[T] => Boolean, value :Boolean) :Buffs[T] = {
			val res = new Array[Buff[T]](buffs.length)
			var count = 0; var i = 0; val len = buffs.length
			while (i < len) {
				val b = buffs(i)
				if (f(b) == value) {
					res(count) = b; count += 1
				}
				i += 1
			}
			if (count == 0)
			inherited.filterMatching(f, value)
			else {
				val newBuffs = if (count < buffs.length) res.take(count) else buffs
				val bottom = inherited.filterMatching(f, value)
				if ((newBuffs eq buffs) && (bottom eq inherited)) this
				else new HierarchicalBuffs(tag, newBuffs, inherited.filterMatching(f, value))
			}
		}

		override def map[S](f :Buff[T] => Buff[S]) :Buffs[S] =
			new HierarchicalBuffs(tag, buffs.map(f), inherited.map(f))

		override def flatMap[S](f :Buff[T] => IterableOnce[Buff[S]]) :Buffs[S] = {
			val prefix = buffs.flatMap(f)
			val suffix = inherited.flatMap(f)
			if (prefix.length == 0) suffix
			else new HierarchicalBuffs(tag, prefix, suffix)
		}

		override def collect[S](f :PartialFunction[Buff[T], Buff[S]]) :Buffs[S] = {
			val prefix = buffs.collect(f)
			val suffix = inherited.collect(f)
			if (prefix.length == 0) suffix
			else new HierarchicalBuffs[S](tag, prefix, suffix)
		}
		override def iterator :Iterator[Buff[T]] =
			if (inherited.isEmpty) front.iterator
			else front.iterator ++ inherited.iterator

		override def foreach[U](f :Buff[T] => U) :Unit = {
			buffs foreach f
			inherited foreach f
		}


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :HierarchicalBuffs[_] if other canEqual this =>
				tag == other.tag &&
					java.util.Arrays.equals(array.asInstanceOf[Array[AnyRef]], other.array.asInstanceOf[Array[AnyRef]]) &&
					inherited == other.inherited
			case other :Buffs[_] if other canEqual this =>
				tag == other.tag && front == other.front && inherited == other.inherited
			case _ => false
		}

		override lazy val hashCode :Int = buffs.hashCode * 31 + inherited.hashCode

		override def toString :String = {
			@tailrec def rec(buffs :Buffs[_], res :StringBuilder) :String =
				if (buffs.isEmpty) (res += ')').result()
				else {
					if (buffs.front.nonEmpty) {
						res ++= buffs.front.mkString(", ")
						if (buffs.inherited.nonEmpty)
							res ++= "| "
					}
					rec(buffs.inherited, res)
				}
			rec(this, new StringBuilder ++= "Buffs(")
		}
	}

}


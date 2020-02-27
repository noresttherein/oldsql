package net.noresttherein.oldsql.model

import scala.collection.breakOut
import net.noresttherein.oldsql.model.ComposedOf.{ComposableFrom, DecomposableTo}
import net.noresttherein.oldsql.model.Kin.{GenericKinFactory, KinComposer, KinFactory, Present}
import net.noresttherein.oldsql.model.Kin.Lazy.LazyKin
import net.noresttherein.oldsql.model.MappedKin.{CompositeMapper, KinMapper}
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.slang._




/** A composite kin is a kin for type `T`, which can be constructed from and deconstructed to
  * a collection of values of type `E`. It is used primarily to represent collection types of entities in a database,
  * but can be adapted to represent higher-level data-structures such as trees or graphs that can be serialized
  * into many rows (or at least in theory, even tables). It is expected that the specification on how to obtain
  * a value for a composite reference limits itself to type `E`, but any actual details are left for subclasses.
  *
  * This class no longer is covariant in regard to `T`, so you can't safely cast a `Kin[T]` to `CompositeKin[T, _]`,
  * even if the underlying class is a `CompositeKin[_, _]`, but should use `CompositeKin[_&lt;:T, _]` instead!
  *
  * @tparam T composite value type of this reference
  * @tparam E element type - `T` can be mapped into an `Iterable[E]` and back, possibly with some restriction on the number
  *           of results or their values.
  */
trait CompositeKin[+T, E] extends Kin[T] {
//	type Item = E
	def composer :ComposableFrom[T, E]
	def decomposer :DecomposableTo[_<:T, E]
	def decompose :Option[Iterable[E]]

	def items :ComposedOf[_<:T, E]

	def mapItems[X, XS](map :KinMapper[E, X])(implicit as :XS ComposedOf X) :Kin[XS]

	override def canEqual(that: Any): Boolean = that.isInstanceOf[CompositeKin[_, _]]
}






/** Operations on references for some composite type C which value can be represented as some sort of collection or composite
  * of items of another type T. Best examples are of course all subtypes of Iterable[T], but also Option[T] as well as
  * identity representation to a super type. The motivation is to be able to accept and return instances of Kin[C],
  * where composite value C can be mapped to a collection of E, which themselves can be mapped to individual rows in a table. 
  *
  */
object CompositeKin {

	def unapply[T](kin :Kin[T]) :Option[CompositeKin[T, _]] =
		kin.asSubclass[CompositeKin[T, _]]


//	def apply[T](restraint :Restraint[T])

	def apply[T](single :Kin[T]) :CompositeKin[T, T] = new SingletonKinWrapper(single)





	trait AbstractCompositeKin[T, E] extends CompositeKin[T, E] {
		def items :T ComposedOf E
		def composer :T ComposableFrom E = items.composer
		def decomposer :T DecomposableTo E = items.decomposer
		def decompose :Option[Iterable[E]] = toOpt.map(items(_))
//		def decompose = items(get)

		def mapItems[X, XS](map :KinMapper[E, X])(implicit as :XS ComposedOf X) :Kin[XS] =
			this.map(CompositeMapper[T, E, X, XS](map)(items, as))

		override def toString :String =
			if (isEmpty) "Absent[" + items + "]" else "Present(" + get + ")"
	}

	class BaseCompositeKin[T, E](implicit val items :T ComposedOf E) extends AbstractCompositeKin[T, E] {
		override def toOpt: Option[T] = None
	}

	private class KinDecorator[T, E](private val kin :Kin[T])(implicit val items :T ComposedOf E)
		extends AbstractCompositeKin[T, E]
	{
		override def get :T = kin.get
		override def toOpt :Option[T] = kin.toOpt

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[KinDecorator[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case wrap :KinDecorator[_, _] if wrap canEqual this => wrap.kin == kin && wrap.items == items
			case _ => false
		}

		override def hashCode :Int = kin.hashCode * 31 + items.hashCode

		override def toString :String = kin + "[" + items + "]"
	}

	private case class SingletonKinWrapper[T](single :Kin[T]) extends BaseCompositeKin[T, T] {
		override def isEmpty :Boolean = single.isEmpty
		override def get :T = single.get
		override def toOpt :Option[T] = single.toOpt
		override def toString = s"($single)[1]"
	}




	/** A representation of a `Kin` for a collection `KC ` of `Kin[C]`, where `C` is a collection of `E`,
	  * as `Kin` for a composite `T` of `E`. Intuitively, this is used to implement ''many-to-many'' relationships,
	  * with the inner `Kin` mapping to rows in an intermediate table. It can be either completely absent (if `key`
	  * is absent), contain a collection `KC ` of absent kin, or present kin. This `Kin` is present ''iff'' 
	  * `key` is present and all kin inside it are also present.
	  * @tparam T exposed collection type with elements of type `E`.
	  * @tparam KC a collection type with `Kin[C]` as its elements, representing references to rows in the relationship table.
	  * @tparam C a type composed of `E` (typically simply `E`) representing the reference from the relationship table
	  *           to the referenced table with `E`.
	  */
	trait FlattenedKin[+T, KC, C, E] extends CompositeKin[T, E] {

		/** A `Kin` for a collection of other `Kin` for type `C` composed of `E`. May be both full and empty. */
		def key :Kin[KC]

		/** Decomposition of the top-level collection to references associated with individual rows in the relationship table.*/
		def links :KC DecomposableTo Kin[C]

		/** Decomposition of the nested composite type representing rows in the relationship table into referenced entities from
		  * the related table.
		  */
		def targets :C DecomposableTo E

		override lazy val toOpt :Option[T] =
			key.toOpt.map(links(_)).flatMap { references =>
				val values = references.flatMap(_.toOpt)
				values.size == references.size ifTrue composer(values.flatMap(targets(_)))
			}

		override def toString :String = key.toString
	}


	object FlattenedKin {

		/** Starts a multi-step build process for `FlattenedKin` instances designed to make the most out of type inference. */
		@inline def apply[E] :FlattenedKinBuilder[E] = new FlattenedKinBuilder[E] {}

		/** The most generic factory method for flattened kin. */
		def apply[T, KC, C, E](kin :Kin[KC], links :KC DecomposableTo Kin[C], targets :C DecomposableTo E)
		                      (implicit compose :T ComposedOf E)
				:FlattenedKin[T, KC, C, E] =
			new KinLinks[T, KC, C, E](kin)(compose, links, targets)

		def apply[T, L[X] <: Iterable[X], C, E](kin :L[Kin[C]], elements :C DecomposableTo E)(implicit compose :T ComposedOf E) :FlattenedKin[T, L[Kin[C]], C, E] =
			new KinLinks[T, L[Kin[C]], C, E](Present(kin))(compose, DecomposableTo[L[Kin[C]], Kin[C]], elements)

		/** The most useful variant of the `FlattenedKin` factory method, assuming that each nested `Kin` points to
		  * exactly one entity in the referenced table.
		  */
		def apply[T, L[X] <: Iterable[X], E](kin :L[Kin[E]])(implicit compose :T ComposedOf E) :FlattenedKin[T, L[Kin[E]], E, E] =
			new KinLinks[T, L[Kin[E]], E, E](Present(kin))

		/** Creates a present `FlattenedKin` instance out of the actual content collection. */
		def apply[T, E](values :T)(implicit compose :T ComposedOf E) :FlattenedKin[T, Seq[Kin[E]], E, E] =
			new KinLinks[T, Seq[Kin[E]], E, E](Present(compose.decomposer(values).map(Present(_))(breakOut)))



		def unapply[T, E](kin :Kin[T])(implicit composition :T ComposedOf E)
				:Option[(Kin[KC], KC DecomposableTo Kin[C], C DecomposableTo E) forSome { type KC; type C }] =
			kin match {
				case r :FlattenedKin[_, _, _, _] if r.decomposer compatibleWith composition.decomposer =>
					val cast = r.asInstanceOf[FlattenedKin[T, Any, Any, E]]
					Some(cast.key, cast.links, cast.targets)
				case _ => None
			}


		trait FlattenedKinBuilder[E] extends Any { //todo:
//			def apply[KC, C](kin :Kin[KC])(implicit links :KC DecomposableTo Kin[C], targets :C DecomposableTo E) :Kin[C] =
//				new KinLinks(kin)()
		}



		private case class KinLinks[T, KC, C, E]
		                           (key :Kin[KC])
		                           (implicit ev :T ComposedOf E, val links :KC DecomposableTo Kin[C], val targets :C DecomposableTo E)
			extends BaseCompositeKin[T, E] with FlattenedKin[T, KC, C, E]
		{
			override def toString :String = key.toString
		}
	}



	object PluralKin {
		def apply[X, E](kin :Seq[Kin[E]], as :X ComposedOf E) :CompositeKin[X, E] =
			FlattenedKin(kin, DecomposableTo.Subclass[E]())(as)

		def apply[E](kin :Seq[Kin[E]]) :CompositeKin[Seq[E], E] =
			FlattenedKin(kin, DecomposableTo.Subclass[E]())(ComposedOf[Seq[E], E])


	}



	class FlattenedKinFactory[HK, HC, TK, TC, E, T](
			head :KinFactory[HK, Kin[TC], HC],
			tail :KinFactory[TK, E, TC])(implicit val result :T ComposedOf E)
		extends KinFactory[Kin[HC], E, T]
	{ factory =>
		override def present(value: T): Kin[T] = Present(value)

		override def absent(key: Kin[HC]): Kin[T] =
			FlattenedKin[T, HC, TC, E](key, head.result.decomposer, tail.result.decomposer)

		override def keyFor(item: E): Option[Kin[HC]] =
			tail.keyFor(item).map(tail.absent).flatMap(head.keyFor).map(head.absent)

		override def keyOf[F >: Kin[T] <: Kin[T]](ref: F): Option[Kin[HC]] =
			ref match {
				case FlattenedKin(khc @ head(_, _), _, _) =>
					Some(khc)
				case Present(result.decomposer(values)) =>
					tail.forItems(values).flatMap(tailValue => head.forItems(Seq(tailValue))) orElse {
						val tailValues = values.map(Seq(_)).map(tail.forItems)
						tailValues.collect{ case Some(v) => v }.providing(_.size == tailValues.size).flatMap(head.forItems)
					}
				case _ => None
			}

		override def delayed(keyRef: Kin[HC], value: => T): Kin[T] =
			new LazyKin[T](()=>Some(value)) with AbstractCompositeKin[T, E] with FlattenedKin[T, HC, TC, E] {
				override def items = factory.result
				override def links = head.result.decomposer
				override def targets = tail.result.decomposer
				override def key = keyRef

			}

		override def as[Y](implicit composition: ComposedOf[Y, E]): KinFactory[Kin[HC], E, Y] =
			new FlattenedKinFactory(head, tail)(composition)

		override def equivalencyToken: Any = (head.equivalencyToken, tail.equivalencyToken)


	}



}

package net.noresttherein.oldsql.model




/** A convenient class to use as primary keys. It introduces a distinction between 'persistent' and 'transient'
  * keys, which reflects the state of the identified entity. A persistent key is a key of an entity loaded from
  * the repository (previously assigned) and wraps some kind of a unique value. A transient key is a key created
  * by the application for fresh entities; if the keys for the entity are assigned by the databaase (or repository itself),
  * it is essentially a dummy value using referential equality instead of the equality of the wrapped key: each new
  * instance is unique. This allows to declare the primary key of an entity as a non-null, non-option type ''and''
  * use the key equality to distinguish between entities abstracting over their persistent/transient state.
  * If keys are assigned by the application before storing the value, a transient key wrapping the value of the appropriate
  * type can be used. Such a key will equal also persistent instances of the same value, but still provide a handy
  * distinction between persistent and transient entity, which can in many cases save a round-trip call to the database
  * otherwise needed to verify if the saved entity (in particular, in cascade saves) should be inserted or updated.
  * @tparam T the type of the identified entity.
  * @see [[net.noresttherein.oldsql.model.PK.Key Key]]
  * @see [[net.noresttherein.oldsql.model.PK.TransientPK TransientPK]]
  * @see [[net.noresttherein.oldsql.model.PK.AssignedPK, AssignedPK]]
  * @author Marcin Mościcki
  */
trait PK[T] extends Any {
	def isPersistent :Boolean = !isTransient
	def isTransient :Boolean = false
	def canEqual(that :Any) :Boolean = that.isInstanceOf[PK[_]]
}


/** A factory for primary keys. */
object PK {

	/** Creates a unique transient key marking a target entity `T` as transient. */
	@inline def apply[T]() :TransientPK[T] = new TransientPK[T]

	/** Creates a key assigned to an entity by an application (transient) backed by the given value. */
	@inline def apply[T, @specialized(Int, Long) K](key :K) :AssignedPK[T, K] = new AssignedPK[T, K](key)



	/** A base trait for primary keys backed by value of type `K`. It can be either transient or persistent,
	  * but will equal another instance regardless of its state. Equality can be affected only by the 'public'
	  * type if the trait is extended to provide unique primary key classes to unique entities (or provide other
	  * functionality).
	  * @tparam T the type of the identified entity.
	  */
	trait Key[T, @specialized(Int, Long) K] extends PK[T] {
		/** The value backing this key */
		val key :K

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Key[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case pk :Key[_, _] => (pk eq this) || pk.canEqual(this) && pk.key == this.key
			case _ => false
		}

		override def toString :String = "PK#" + key
	}

	object Key {
		def unapply[T, K](key :Key[T, K]) :Some[K] = Some(key.key)
	}




	/** A persistent primary key backed by the value of `K`. Instances of this class are created by mappings
	  * when an entity is loaded from storage.
	  * @tparam T the type of the identified entity.
	  * @tparam K the type of the value of the key.
	  */
	class PersistentPK[T, @specialized(Int, Long) K](val key :K) extends Key[T, K] {
		override def isTransient = false
	}

	object PersistentPK {
		def apply[T, @specialized(Int, Long) K](key :K) :PersistentPK[T, K] = new PersistentPK[T, K](key)

		def unapply[T](pk :PK[T]) :Boolean = pk.isPersistent
	}



	/** A base class for transient primary keys. Keys of this class are assigned to entities created
	  * by the application before persisting them. This instances of this class itself don't equal any other instances,
	  * although subclasses can change this behaviour.
	  * @see [[net.noresttherein.oldsql.model.PK.AssignedPK AssignedPK]]
	  */
	class TransientPK[T] extends PK[T] {
		override def isTransient = true

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TransientPK[_]]

		override def equals(that :Any) :Boolean = that match {
			case ref :AnyRef => ref eq this
			case _ => false
		}

		override def hashCode :Int = System.identityHashCode(this)

		override def toString :String = "PK~" + hashCode.toHexString
	}

	object TransientPK {
		/** Creates a unique transient key marking a target entity `T` as transient. */
		@inline def apply[T]() :TransientPK[T] = new TransientPK[T]

		/** Creates a key assigned to an entity by an application (transient) backed by the given value. */
		@inline def apply[T, @specialized(Int, Long) K](key :K) :AssignedPK[T, K] = new AssignedPK[T, K](key)

		def unapply[T](key :PK[T]) :Boolean = key.isTransient
	}



	/** A value of primary key for an entity `T` assigned to it by the application before storing. */
	class AssignedPK[T, @specialized(Int, Long) K](val key :K) extends TransientPK[T] with Key[T, K] {
		override def toString :String = "PK~" + key
	}


	object AssignedPK {
		/** Creates a key assigned to an entity by an application (transient) backed by the given value. */
		def apply[T, @specialized(Int, Long) K](key :K) :AssignedPK[T, K] = new AssignedPK[T, K](key)

		def unapply[T, K](pk :Key[T, K]) :Option[K] = if (pk.isTransient) Some(pk.key) else None
	}


}
package com.hcore.ogre.mapping.ties


import com.hcore.ogre.mapping.ComponentPath.{TypedComponentPath, SelfPath, MappedComponent}
import com.hcore.ogre.mapping.Mapping.{MappingExtension, ReferenceContext}
import com.hcore.ogre.mapping.MappingMorphism.ValueMorphism
import com.hcore.ogre.mapping.support.MappingAdapter.{DirectMappingAdapter, MappingSubstitute}
import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.mapping.ties.MappingReference.MappingReferenceFactory.LazyMappingReferenceFactory
import com.hcore.ogre.mapping.ties.MappingReference.{MappingReferenceFactory, GenericMappingReferenceFactory}
import com.hcore.ogre.mapping._
import com.hcore.ogre.model.ComposedOf.{DecomposableTo}
import com.hcore.ogre.model.Reference.Lazy.LazyReferenceFactory
import com.hcore.ogre.model.Reference._
import com.hcore.ogre.model.Restriction.Opressor
import com.hcore.ogre.model.Restriction.Restrictive.{Literal, Property}
import com.hcore.ogre.model.{Restriction, Reference, ComposedOf, NavigableReference}
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.SQLFormula.{ComponentFormula, FormulaEqualizer}
import com.hcore.ogre.sql.{From, Join, RowSource, SQLReadForm}

import scala.collection.Set

import scala.reflect.runtime.universe.TypeTag

//implicits
import extensions._





/** A generic skeleton implementation for mappings creating Reference instances to other tables or parts of other tables.
  * Works as a wrapper for a foreign key component which contains all data required to perform a join between
  * the owner table and the foreign table. The actual implementation of the join is left out for subclasses, with
  * this instance simply creating a correct MappingPath linking the two tables and exporting it via 'scout' method
  * for discovery by pathfinders. Note that we will refer here to the key mapped by this instance as foreign key and
  * the referenced key in the other table as target key (usually primary key), but  because of symmetrical nature
  * of relational databases it might be actually the other way round, the 'foreign key' component of this mapping being
  * a primary key of the table (or rather, probably a sym link mapping for the primary key component) and the target key
  * a real foreign key in the database sense.
  * @tparam FK scala type of the foreign key, representing the local data used to match records in foreign table TM.
  * @tparam FKM mapping responsible for the foreign key type which is being boxed by this adapter inside produced references.
  * @tparam TM table mapping for the target of the join. Note that it is not necessarily the actual target mapping for referenced type!
  * @tparam E referenced element type being an individual record contained in produced references.
  * @tparam EM target component in the foreign table responsible for mapping instances of E which will be used to produce referenced composite type X.
  * @tparam TK the key in the other table and the key type for produced references, identifying subsets of records E.
  * @tparam TKM component in the target table responsible for mapping the referenced key, usually the primary key component.
  * @tparam X scala type contained by the reference, a composite type consisting of items of type E
  * @tparam R type of the mapped references, the result type of this mapping.
  */
trait GenericJoinComponentReferenceMapping[FK, FKM<:Mapping[FK], TM<:AnyMapping, TK, TKM<:Mapping[TK], E, EM<:Mapping[E], X, R<:Reference[X]]
	extends DirectMappingAdapter[R, FK, FKM] with ReferenceMapping[TK, E, X, R]
{
	/** Factory of reference values used to create reference R instances based on the key and/or Iterable[E],
	  * as well as producing a key for storing out of instances of R
	  */
	val referenceFactory :GenericMappingReferenceFactory[TK, E, X, R, EM]

	/** A shorthand for adaptee to be used in queries when it would be more convenient to refer to the foreign key
	  * by its actual type, rather then boxed in a reference R by this instance.
	  */
	val key :FKM

	/** Convert the local foreign key into a referenced target key. Note that sometimes datatypes used in sql joins
	  * don't match exactly, or the scala types exported by mappings are different despite using the same record type
	  * in the database (for example, Option[Int] and Int). This function should be defined for all values in
	  * its domain, as an existance of a foreign key should certainly imply existance of a referenced value!
	  */
	protected def toTargetKey :FK=>TK

	/** Convert the value of the key retrieved from a referenced instance into a value that can be stored
	  * in this table by the foreign key component TKM. This function is called when saving an instance of Reference R
	  * pointing to instances of E using key TK; if the conversion is impossible recovery is probably not possible, so
	  * exceptions here will be generally propagated.
	  */
	protected def toForeignKey :TK=>FK

	/** Path in the target table pointing to the component mapping the values for referenced keys. */
	protected def targetKey :ComponentPath[TM, TKM]

	/** Path in the target table pointing to the component mapping the actual referenced values.
	  * In most cases, it will be just a self path for the whole table).
	  */
	protected def target :ComponentPath[TM, EM]

	/** Actual implementations will return the join between the target table, this component and potentially
	  * other tables which take no part in mapping the result values, but can be used to narrow down the results,
	  * such as dictionary tables. Note two things: first, the join is not between two tables, but actually between
	  * this component and the target table and will be 'planted' in the owning table before performing actual queries.
	  * This is to avoid the dependency of components on the owning mappings. Second, the order of the join is reversed
	  * for easy access to this mapping: first come any optional tables, then the target table, and this component last.
	  * This means that it will likely appear in the reversed form in final queries, and if any outer joins are present here,
	  * they'll be swapped into their opposites to preserve the semantics.
	  * @return
	  */
	def join :RowSource Join TM Join this.type


	/** Path from this component to the referenced mapping, starting with a JoinPath with target table and followed by the
	  * path to the target component in that table.
	  */
	def joinPath :MappingPath[this.type, EM] =
		JoinPath[this.type, TM](join, arity, referencedOwner) ++ target

	/** Arity of the join, defaulting to the one provided by composition stored in the referenceFactory */
	def arity = referenceFactory.items.arity

	/** A function used as the 'pick' function for the join path, which would retrieve the value mapped
	  * by the whole target table (rather than just the target component) from a reference. This will generally
	  * be impossible, so default implementation will just return None for all arguments.
	  */
	def referencedOwner :R => Option[TM#ResultType] = _ => None

	/** A function which retrieves a single referenced value E out of a reference R if possible. Foreign keys pointing
	  * to at most one record in the target table and to the whole table record rather than just a component may
	  * use it as an implementation of referencedOwner.
	  * @return
	  */
	def referencedValue :R => Option[E] = referenceFactory.items.decomposition match {
		case DecomposableTo.Subclass() => (_:R).toOpt.asInstanceOf[Option[E]]
		case DecomposableTo.Optional() => (_:R).toOpt.flatMap(_.asInstanceOf[Option[E]])
		case DecomposableTo.Iterable() => (_:R).toOpt.flatMap(_.asInstanceOf[Iterable[E]].providing(_.size==1).map(_.head))
		case _ => (_:R) => None
	}


	object morphism extends DirectMorphism {
		val value = ValueMorphism[R, FK](referenceFactory.keyOf(_).map(toForeignKey))
	}

	override def selectForm: SQLReadForm[R] = adaptee.selectForm.map(fk => referenceFactory.empty(toTargetKey(fk)))

	/** Delegates to mapping factory in order to produce a scouting reference, i.e. a mock instance used
	  * by the pathfinders to introspect the mappings based on mapped domain objects.
	  * @see com.hcore.ogre.mapping.Scout
	  */
	override def scoutValue(ctx: ReferenceContext[this.type]): Option[R] =
		Some(referenceFactory.scout(ctx \\ joinPath))

	/** Use the provided key mapping TKM and associated toTargetKey mapping function to produce the value of the key
	  * to be stored inside mapped references.
	  */
	override protected def referenceKey(values: Values): Option[TK] =
		values.get(adaptee).map(toTargetKey)




	override def canEqual(that :Any) = that.isInstanceOf[GenericJoinComponentReferenceMapping[_, _, _, _, _, _, _, _, _]]

	override def equals(that :Any) = that match {
		case r:GenericJoinComponentReferenceMapping[_, _, _, _, _, _, _, _, _] =>
			(this eq r) || (r.canEqual(that) && r.key==key && r.referenceFactory==referenceFactory && r.join==join)
		case _ => false
	}

	override def hashCode = (key, referenceFactory).hashCode

	override def toString = s"$referenceFactory[$join]"
}




/** A specialization of generic reference mappings based on stored joins which refer to the whole records in the target table
  * (as it will generally be the case).
  */
trait GenericJoinReferenceMapping[FK, FKM<:Mapping[FK], TK, TKM<:Mapping[TK], E, EM<:Mapping[E], X, R<:Reference[X]]
	extends GenericJoinComponentReferenceMapping[FK, FKM, EM, TK, TKM, E, EM, X, R]
{
	override def target = ComponentPath.self(targetKey.start)
	override def referencedOwner = referencedValue

	override def joinPath = JoinPath[this.type, EM](join, arity, referencedValue)
}




/** A mapping for R<:Reference[X] based on equality comparison between a key of type K stored locally and mapped by mapping FKM,
  * and a (asummingly compatible column-wise) key in the target table mapped by component TKM. Produced join simply
  * joins the target table with this mapping, comparing the values of components responsible for their respective keys.
  * @tparam K scala type of the key compared in both tables. Note that if the actual mappings compared are of incompatible types,
  *           they should be first mapped via ComponentPath.map.
  * @tparam FKM mapping responsible for the key which is being boxed by this adapter inside produced references.
  * @tparam TM table mapping for the target of the join. Note that it is not necessarily the actual target mapping for referenced type!
  * @tparam TKM component in the target table responsible for mapping the referenced key, usually the primary key component.
  * @tparam E referenced element type being an individual record contained in produced references.
  * @tparam EM target component in the foreign table responsible for mapping instances of E which will be used to produce referenced composite type X.
  * @tparam X scala type contained by the reference, a composite type consisting of items of type E
  * @tparam R type of the mapped references, the result type of this mapping.
  */
trait GenericComponentReferenceKeyMapping[K, FKM<:Mapping[K], TM<:AnyMapping, TKM<:Mapping[K], E, EM<:Mapping[E], X, R<:Reference[X]]
	extends GenericJoinComponentReferenceMapping[K, FKM, TM, K, TKM, E, EM, X, R]
{ self =>
	protected def toTargetKey = identity[K] _
	protected def toForeignKey = identity[K] _

	def targetKey :ComponentPath[TM, TKM]
	def target :ComponentPath[TM, EM]


	def join = From(targetKey.start).join[this.type](this) on (_ \\ targetKey === _ :\ key)


	override def toString = s"($key<=>$target($targetKey))"
	
}




/** A mapping for R<:Reference[X] based on equality comparison between a key of type K stored locally and mapped by mapping FKM,
  * and a (asummingly compatible column-wise) key in the target table mapped by component TKM. Produced join simply
  * joins the target table with this mapping, comparing the values of components responsible for their respective keys.
  * @param lazyKey mapping for the local key value to be included as a component of this mapping.
  *            For foreign keys, this might be any mapping, for inverse foreign keys it will usually be a sym link to primary key mapping of the owner table.
  * @param lazyTargetKey expression evaluating to a path starting at the target table and pointing to the mapping of the key to be compared with key
  * @param lazyTarget expression evaluating to a path starting at the target table and pointing to the mapping responsible for the element type E of mapped Reference R
  * @param referenceFactory factory of reference values used to create reference R instances based on the key and/or Iterable[E],
  *                         as well as producing a key for storing out of instances of R
  * @tparam K scala type of the key compared in both tables. Note that if the actual mappings compared are of incompatible types,
  *           they should be first mapped via ComponentPath.map.
  * @tparam FKM mapping responsible for the key which is being boxed by this adapter inside produced references.
  * @tparam TM table mapping for the target of the join. Note that it is not necessarily the actual target mapping for referenced type!
  * @tparam TKM component in the target table responsible for mapping the referenced key, usually the primary key component.
  * @tparam E referenced element type being an individual record contained in produced references.
  * @tparam EM target component in the foreign table responsible for mapping instances of E which will be used to produce referenced composite type X.
  * @tparam X scala type contained by the reference, a composite type consisting of items of type E
  * @tparam R type of the mapped references, the result type of this mapping.
  */
class BaseGenericComponentReferenceKeyMapping[K, FKM<:Mapping[K], TM<:AnyMapping, TKM<:Mapping[K], E, EM<:Mapping[E], X, R<:Reference[X]]
		(lazyKey : =>FKM, lazyTargetKey : =>ComponentPath[TM, TKM], lazyTarget : =>ComponentPath[TM, EM],
        val referenceFactory :GenericMappingReferenceFactory[K, E, X, R, EM])
	extends GenericComponentReferenceKeyMapping[K, FKM, TM, TKM, E, EM, X, R]
{ self =>
	lazy val key = lazyKey
	protected lazy val adaptee = key
	lazy val targetKey = lazyTargetKey
	lazy val target = lazyTarget

}



trait GenericReferenceKeyMapping[K, FKM<:Mapping[K], TKM<:Mapping[K], E, EM<:Mapping[E], X, R<:Reference[X]]
	extends GenericComponentReferenceKeyMapping[K, FKM, EM, TKM, E, EM, X, R]
	with GenericJoinReferenceMapping[K, FKM, K, TKM, E, EM, X, R]
{

	override def toString = s"($key<=>$targetKey)"
}



/** A mapping for reference R<:Reference[X :(_ ComposedOf E)] joining on key equality between this table's key,
  * as given by 'key' argument, and target table key mapping. This is a specialized producing refering to the whole
  * target table (the element type of R is the ResultType of the table mapping).
  */
class BaseGenericReferenceKeyMapping[K, FKM<:Mapping[K], TKM<:Mapping[K], E, EM<:Mapping[E], X, R<:Reference[X]]
		(lazyKey : =>FKM, lazyTargetKey : =>ComponentPath[EM, TKM],
        referenceFactory :GenericMappingReferenceFactory[K, E, X, R, EM])
	extends BaseGenericComponentReferenceKeyMapping[K, FKM, EM, TKM, E, EM, X, R](
		lazyKey, lazyTargetKey, ComponentPath.self(lazyTargetKey.start), referenceFactory)
//	with GenericJoinReferenceMapping[K, FKM, K, TKM, E, EM, X, R]
	with GenericReferenceKeyMapping[K, FKM, TKM, E, EM, X, R]
{
//	override lazy val target = super[GenericJoinReferenceMapping].target
	override lazy val target = super[GenericReferenceKeyMapping].target
}



trait GenericForeignKeyReferenceMapping[K, FKM<:Mapping[K], TKM<:Mapping[K], E, EM<:Mapping[E], X, R<:Reference[X]] 
	extends GenericReferenceKeyMapping[K, FKM, TKM, E, EM, X, R]
{ self =>
	/** Create a mapping representing the inversed side of this relationship that can be used as a comopnent of the target table's mapping.
	  * This instance simply creates a mirror instance of this same class with roles swapped, using local key as the target key.
	  * @param foreignKey the path in the owning table pointing to this mapping.
	  * @param ownerPath the path to the component of the owning table which will be the element type of created references
	  * @param reference factory which will create the references
	  * @tparam O element type of mapped references (i.e Y ConsistsOf O)
	  * @tparam OM type of mapping for reference items
	  * @tparam OT mapping of the table owning this key reference mapping and the component which is the target of the created reference mapping
	  * @tparam Y composite result type of the reference RY<:Reference[Y] such that Y ConsistsOf O
	  * @tparam RY static type of references created by the produced mapping, as given by the passed factory
	  */
	def inverse[O, OM<:Mapping[O], OT<:AnyMapping, Y, RY<:Reference[Y]](
			foreignKey :ComponentPath[OT, this.type], ownerPath :ComponentPath[OT, OM],
			reference :GenericMappingReferenceFactory[K, O, Y, RY, OM])
	:BaseGenericComponentReferenceKeyMapping[K, TKM, OT, FKM, O, OM, Y, RY] =
		new BaseGenericComponentReferenceKeyMapping(targetKey.end, foreignKey :+ key, ownerPath, reference)


	/** Create a mapping representing the inverse side of this relationship that can be used as a component of the target table's mapping.
	  * This is a specialized, most typical variant where the whole table owning this mapping is the target of the inverse reference.
	  */
	def inverse[O, OM<:Mapping[O], Y, RY<:Reference[Y]](
			foreignKey :ComponentPath[OM, this.type], reference :GenericMappingReferenceFactory[K, O, Y, RY, OM])
	:BaseGenericReferenceKeyMapping[K, TKM, FKM, O, OM, Y, RY] =
		new BaseGenericReferenceKeyMapping(targetKey.end, foreignKey :+ key, reference)


	class ForeignKeyInverter[Y] {
		def apply[O, OM<:Mapping[O], OT<:AnyMapping, RY<:Reference[Y]](
				foreignKey :ComponentPath[OT, self.type], ownerPath :ComponentPath[OT, OM],
				reference :GenericMappingReferenceFactory[K, O, Y, RY, OM])
		:BaseGenericComponentReferenceKeyMapping[K, TKM, OT, FKM, O, OM, Y, RY] =
			new BaseGenericComponentReferenceKeyMapping(targetKey.end, foreignKey :+ key, ownerPath, reference)

		def apply[O, OM<:Mapping[O], RY<:Reference[Y]](
			                                              foreignKey :ComponentPath[OM, self.type], reference :GenericMappingReferenceFactory[K, O, Y, RY, OM])
		:BaseGenericReferenceKeyMapping[K, TKM, FKM, O, OM, Y, RY] =
			new BaseGenericReferenceKeyMapping(targetKey.end, foreignKey :+ key, reference)
	}

	def inverseAs[Y] = new ForeignKeyInverter[Y]
	
}


trait ForeignKeyReferenceMapping[K, FKM<:Mapping[K], TKM<:Mapping[K], E, EM<:Mapping[E], X]
	extends GenericForeignKeyReferenceMapping[K, FKM, TKM, E, EM, X, Reference[X]]
{ self =>
	import ForeignKeyReferenceMapping._

//	val referenceFactory = MappingReferenceFactory.Lazy(KeyReferenceFactory[K, E, EM](targetKey)).as[X]
	def inverse[O, OM<:Mapping[O], KM<:Mapping[K], Y](mapping : =>KM,
			foreignKey : =>ComponentPath[OM, self.type])(implicit composedOf :Y ComposedOf O, tag :TypeTag[O])
	:BaseForeignKeyReferenceMapping[K, KM, FKM, O, OM, Y] =
		new BaseForeignKeyReferenceMapping[K, KM, FKM, O, OM, Y](mapping, foreignKey :+ key,
			MappingReferenceFactory.Lazy(new ForeignKeyInverseReferenceFactory[K, O, Y, OM, X](foreignKey, referenceFactory)))

	class ForeignKeyInverter[O, Y] extends super.ForeignKeyInverter[Y] {
		def apply[OM<:Mapping[O], KM<:Mapping[K]](mapping : =>KM,
				foreignKey : =>ComponentPath[OM, self.type])(implicit composedOf :Y ComposedOf O, tag :TypeTag[O])
		:BaseForeignKeyReferenceMapping[K, KM, FKM, O, OM, Y] =
			new BaseForeignKeyReferenceMapping[K, KM, FKM, O, OM, Y](mapping, foreignKey :+ key,
				MappingReferenceFactory.Lazy(new ForeignKeyInverseReferenceFactory[K, O, Y, OM, X](foreignKey, referenceFactory)))

		//			inverse(foreignKey)
	}

	class ForeignKeyInverseTypist[O] {
		def in[C[V]] = new ForeignKeyInverter[O, C[O]]
		def as[C] = new ForeignKeyInverter[O, C]
	}

	override def inverseAs[Y] = new ForeignKeyInverter[Y, Y]

	def inverse[O] = new ForeignKeyInverseTypist[O]

}



class BaseForeignKeyReferenceMapping[K, FKM<:Mapping[K], TKM<:Mapping[K], E, EM<:Mapping[E], X](
		lazyKey : =>FKM, lazyTargetKey : =>ComponentPath[EM, TKM], factory :MappingReferenceFactory[K, E, X, EM])(
		implicit composed :X ComposedOf E, tag :TypeTag[E])
	extends BaseGenericReferenceKeyMapping[K, FKM, TKM, E, EM, X, Reference[X]](lazyKey, lazyTargetKey, factory)
	with ForeignKeyReferenceMapping[K, FKM, TKM, E, EM, X]
{ self =>
	import ForeignKeyReferenceMapping.{KeyReferenceFactory, ForeignKeyInverseReferenceFactory}

	def this(key : =>FKM, lazyTargetKey : =>ComponentPath[EM, TKM])(implicit composed :X ComposedOf E, tag :TypeTag[E]) =
		this(key, lazyTargetKey,  MappingReferenceFactory.Lazy(ForeignKeyReferenceMapping.KeyReferenceFactory[K, E, EM](lazyTargetKey)).as[X])

}



//trait ForeignKeyInverseReferenceMapping[K, KM<:Mapping[K], FKM<:Mapping[K], E, EM<:Mapping[E], X] {
//	import ForeignKeyReferenceMapping._
//
//	val original :GenericForeignKeyReferenceMapping[K, FKM, KM, _, _, _, _]
//	val referenceFactory = MappingReferenceFactory.Lazy(new ForeignKeyInverseReferenceFactory[K, E, X, EM, X](foreignKey, original.referenceFactory))
//}




class BaseForeignKeyInverseReferenceMapping[K, FKM<:Mapping[K], TKM<:Mapping[K], E, EM<:Mapping[E], X](
		key : =>FKM, lazyTargetKey : =>ComponentPath[EM, TKM], factory :MappingReferenceFactory[K, E, X, EM])(
		implicit composed :X ComposedOf E, tag :TypeTag[E])
	extends BaseForeignKeyReferenceMapping[K, FKM, TKM, E, EM, X](key, lazyTargetKey, factory)
{
	import ForeignKeyReferenceMapping.{KeyReferenceFactory, ForeignKeyInverseReferenceFactory}

	def this(key : =>FKM, lazyTargetKey : =>ComponentPath[EM, TKM])(implicit composed :X ComposedOf E, tag :TypeTag[E]) =
		this(key, lazyTargetKey,  MappingReferenceFactory.Lazy(ForeignKeyReferenceMapping.KeyReferenceFactory[K, E, EM](lazyTargetKey)).as[X])

}







object ForeignKeyReferenceMapping {

	def apply[K, FKM<:Mapping[K], TKM<:Mapping[K], E, EM<:Mapping[E], X](
			key :FKM, lazyTargetKey : =>ComponentPath[EM, TKM])(implicit composed :X ComposedOf E, tag :TypeTag[E])
	:BaseForeignKeyReferenceMapping[K, FKM, TKM, E, EM, X] =
		new BaseForeignKeyReferenceMapping[K, FKM, TKM, E, EM, X](key, lazyTargetKey)

	


	import com.hcore.ogre.model.Restriction.Restrictive

	
	
	def KeyReferenceFactory[K, E :TypeTag, M<:Mapping[E]](targetKey :ComponentPath[M, _<:Mapping[K]]) :MappingReferenceFactory[K, E, E, M] =
		targetKey.surepick.flatMap(PropertyChain.maybe[E, K](_)) match {
			case Some(property) => MappingReferenceFactory[K, E, E, M](targetKey.start, property ==?)
			case None => throw new IllegalArgumentException(s"Couldn't create a PropertyChain for target key $targetKey.")
		}


	class ForeignKeyInverseReferenceFactory[K, E :TypeTag, X, EM<:Mapping[E], T](
			foreignKey :ComponentPath[EM, _<:Mapping[Reference[T]]], original :ReferenceFactory[K, _, T])(
			implicit val items :X ComposedOf E)
		extends MappingReferenceFactory[K, E, X, EM]
	{
		val refKeyedFactory = KeyReferenceFactory[Reference[T], E, EM](foreignKey).as[X]

		override def target: EM = foreignKey.start

		override def delayed(key: K, value: => X): Reference[X] = Lazy(value)

		override def full(value: X): Reference[X] = Full(value)

		override def empty(key: K): Reference[X] = refKeyedFactory.empty(original.empty(key))


		override def keyFor(item: E): Option[K] = refKeyedFactory.keyFor(item).flatMap(original.keyOf)

		override def keyOf[F >: Reference[X] <: Reference[X]](ref: F): Option[K] =
			refKeyedFactory.keyOf(ref).flatMap(original.keyOf)



		override def scout(ctx: ReferenceContext[EM]): Reference[X] = refKeyedFactory.scout(ctx)

		override def as[Y](implicit composition: ComposedOf[Y, E]): GenericMappingReferenceFactory[K, E, Y, Reference[Y], EM] =
			new ForeignKeyInverseReferenceFactory[K, E, Y, EM, T](foreignKey, original)

		override def equivalencyToken: Any = (foreignKey, original.equivalencyToken)

		override def toString = s"$refKeyedFactory(key)"
	}
}



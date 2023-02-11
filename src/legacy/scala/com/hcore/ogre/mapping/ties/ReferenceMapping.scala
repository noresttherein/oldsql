package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.ComponentPath.{SelfPath, MappedComponent}
import com.hcore.ogre.mapping.Mapping.{MappingExtension, ReferenceContext}
import com.hcore.ogre.mapping.MappingMorphism.ValueMorphism
import com.hcore.ogre.mapping.support.MappingAdapter.{DirectMappingAdapter, MappingSubstitute}
import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.mapping.ties.MappingReference.GenericMappingReferenceFactory
import com.hcore.ogre.mapping._
import com.hcore.ogre.model.ComposedOf.DecomposableTo
import com.hcore.ogre.model.Reference._
import com.hcore.ogre.model.{Reference, ComposedOf, NavigableReference}
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.SQLFormula.FormulaEqualizer
import com.hcore.ogre.sql.{RowSource, SQLReadForm}

//implicits
import extensions._



trait ReferenceMapping[K, E, X, R<:Reference[X]] extends Mapping[R] {
	val referenceFactory :GenericReferenceFactory[K, E, X, R]

	override def assemble(values: Values): Option[R] = values match {
		case ReferenceResolver(resolver) =>
			for (key <- referenceKey(values))
				yield resolver(key, referenceFactory) getOrElse referenceFactory.empty(key)
		case _ =>
			referenceKey(values).map(referenceFactory.empty)
	}


	protected def referenceKey(values :Values) :Option[K]

	override def scoutValue(ctx: ReferenceContext[this.type]): Option[R] = NavigableReference.from(referenceFactory)
}




trait StaticReferenceMapping[K, E, X, R<:Reference[X]] extends StaticMapping[R] with ReferenceMapping[K, E, X, R] {
	final override protected def construct(implicit res: Values): R =
		throw new UnsupportedOperationException(s"StaticReferenceMapping.construct() called on $this: this call should never happen")

	final override protected def referenceKey(values: Values): Option[K] =
		constructKey(values).providing(isDefined(values))

	protected def constructKey(implicit values :Values) :K
}



object ReferenceMapping {

	def apply[K, E, T, R<:Reference[T]](references :GenericReferenceFactory[K, E, T, R], keyMapping :Mapping[K]) :Mapping[R] =
		new ReferenceMappingAdapter(keyMapping, references, identity[K], identity[K])

	def apply[FK, K, E, T, R<:Reference[T]](references :GenericReferenceFactory[K, E, T, R], keyMapping :Mapping[FK], referencedKey :FK=>K, foreignKey :K=>FK) :Mapping[R] =
		new ReferenceMappingAdapter(keyMapping, references, referencedKey, foreignKey)



	
	
	
	
	
	
	
	


	class ReferenceMappingAdapter[FK, K, E, T, R<:Reference[T]](
			val adaptee :Mapping[FK], val referenceFactory :GenericReferenceFactory[K, E, T, R], targetKey :FK=>K, foreignKey :K=>FK)
		extends MappingSubstitute[R, FK, Mapping[FK]] with ReferenceMapping[K, E, T, R]
	{


		override val modifiers: Seq[MappingExtension[R]] =
			adaptee.modifiers.map(_.map(v => referenceFactory.empty(targetKey(v))))


		object morphism extends SubstituteMorphism {
			override val value = ValueMorphism[R, FK](referenceFactory.keyOf(_).map(foreignKey))
		}

		override protected def referenceKey(values: Values): Option[K] =
			adaptee.assemble(values.morph(morphism)).map(targetKey)



//		override def mockValue: Option[R] =
//			NavigableReference.from(referenceFactory) orElse adaptedMapping.mockValue.map(v=>referenceFactory.empty(targetKey(v)))

		override def selectForm: SQLReadForm[R] = adaptee.selectForm.map(fk => referenceFactory.empty(targetKey(fk)))

		override def scoutValue(ctx :ReferenceContext[this.type]): Option[R] =
			NavigableReference.from(referenceFactory) orElse adaptee.scoutValue.map(v=>referenceFactory.empty(targetKey(v)))


		override def toString = s"$adaptee references $referenceFactory"
	}


/*

	class EmptyReferenceType[FK :ColumnType, K, E, T, R >:Null <:Reference[T]] (
			protected val references :TypedReferenceFactory[K, E, T, R], protected val referencedKey :FK=>K, foreignKey :K=>FK)
		extends ColumnType[R]
	{
		protected val keyType = ColumnType[FK]

		override val Setter = SetParameter[R]{ (ref, params) =>
			references.keyOf(ref) match {
				case Some(key) =>
					keyType.Setter(foreignKey(key), params)
				case None =>
					throw new IllegalArgumentException(s"Can't save reference column value $ref: no key")
			}
		}

		override val Getter = GetResult[R] { res =>
			references.empty(referencedKey(keyType.Getter(res)))
		}


		override def nullValue: R = null
	}



	class ReferenceType[FK :ColumnType, K, E, T, R >:Null <:Reference[T]] (target :TypedReferenceFactory[K, E, T, R], toPK :FK=>K, toFK :K=>FK)
		extends EmptyReferenceType[FK, K, E ,T, R](target, toPK, toFK)
	{
		override val Getter = GetResult[R] { res =>
			val key = referencedKey(keyType.Getter(res))
			ReferenceResolver(res)(key, references) getOrElse references.empty(key)
		}
	}


	object ReferenceType {
		def apply[K :ColumnType, E, T, R >:Null <:Reference[T]](target :TypedReferenceFactory[K, E, T, R]) :ColumnType[R] =
			new ReferenceType[K, K, E, T, R](target, identity[K], identity[K])

		def apply[FK :ColumnType, K, E, T, R>:Null <:Reference[T]](target :TypedReferenceFactory[K, E, T, R], targetKey :FK=>K, foreignKey :K=>FK) :ColumnType[R] =
			new ReferenceType[FK, K, E, T, R](target, targetKey, foreignKey)

		def empty[K :ColumnType, E, T, R >:Null <:Reference[T]](target :TypedReferenceFactory[K, E, T, R]) :ColumnType[R] =
			new EmptyReferenceType[K, K, E, T, R](target, identity[K], identity[K])

		def empty[FK :ColumnType, K, E, T, R>:Null <:Reference[T]](target :TypedReferenceFactory[K, E, T, R], targetKey :FK=>K, foreignKey :K=>FK) :ColumnType[R] =
			new EmptyReferenceType[FK, K, E, T, R](target, targetKey, foreignKey)

	}

*/



//	class JoinReferenceMapping[]()



}

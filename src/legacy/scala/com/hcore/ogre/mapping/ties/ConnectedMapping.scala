package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.ComponentPath.TypedComponentPath
import com.hcore.ogre.mapping._
import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.mapping.ties.ForeignKeyReferenceMapping.ForeignKeyInverseReferenceFactory
import com.hcore.ogre.mapping.ties.MappingReference.MappingReferenceFactory
import com.hcore.ogre.model.{ComposedOf, Reference}

import scala.reflect.runtime.universe.TypeTag


@deprecated
trait ConnectedMapping[E] extends StaticMapping[E] { table =>

//	class ForeignKeyComponent[E]
	
//	def foreignKey[TM<:Mapping[X], X, KM<:Mapping[K], K :ColumnType](name :String, pick :E=>Reference[X], target : =>ComponentPath[TM, KM]) =
//		foreignKey(pick)(name, target)


	def foreignKey[X](pick :E=>Reference[X]) :ForeignKeyBuilder[X] = new ForeignKeyBuilder[X](pick)
	
	
	class ForeignKeyBuilder[X](val property :E=>Reference[X])  {
		def apply[FK<:Mapping[K], K, T<:Mapping[X], TK<:Mapping[K]](localKey :FK, targetKey : =>ComponentPath[T, TK])(implicit targetTag :TypeTag[X])
				:ForeignKeyComponent[K, FK, TK, T, X] =
			new ForeignKeyComponent[K, FK, TK, T, X](property, localKey, targetKey)

		def apply[T<:Mapping[X], K](name :String, targetKey : =>ComponentPath[T, _<:Mapping[K]])(implicit keyType :ColumnType[K], targetTag :TypeTag[X])
				:ForeignKeyComponent[K, TypedColumn[K], _<:Mapping[K], T, X] =
			apply(TypedColumn(name), targetKey)
	}


	def inverse[O, C](
			pick :E=>Reference[C], foreignKey : ComponentPath[_<:Mapping[O], _<:ForeignKeyReferenceMapping[_, _, _, E, this.type, _]])(
			implicit items :C ComposedOf O, targetTag :TypeTag[O]) :Component[Reference[C]] =
	{
		val fk = foreignKey.end.asInstanceOf[ForeignKeyReferenceMapping[Any, Mapping[Any], Mapping[Any], E, this.type, Any]]
		embed(pick, fk.inverse[O].as[C].apply(
			symLink(foreignKey.end.targetKey.asInstanceOf[TypedComponentPath[this.type, Mapping[Any], Any]]),
			foreignKey.cast[Mapping[O], fk.type]
		))
	}
//	def inverse[O, C](pick :E=>Reference[C], foreignKey :ComponentPath[_<:Mapping[O], _<:ForeignKeyReferenceMapping[_, _, _, E, this.type, _]])(
//	                 implicit items :C ComposedOf O, targetTag :TypeTag[O]) :Component[Reference[C]] = ???
//		new ForeignKeyInverseComponent(pick, foreignKey)

/*
	class InverseReferenceBuilder[T<:Mapping[X], K, FKM<:Mapping[K], KM<:Mapping[K], X, O](foreignKey :ComponentPath[T, _<:ForeignKeyReferenceMapping[K, FKM, KM, E, this.type, X]]) {
		def as[C](implicit as :C ComposedOf O) :Component[Reference[C]] = //foreignKey.end.inverseAs[C](symLink(id), Liberals \\ Liberals.nemesis)
			new BaseForeignKeyReferenceMapping[K, KM, FKM, X, this.type, C](foreignKey.end.targetKey, foreignKey :+ foreignKey.end.key,
					MappingReferenceFactory.Lazy(new ForeignKeyInverseReferenceFactory[K, O, C, T, X](foreignKey, foreignKey.end.referenceFactory))) 
				with Component[T] 
			{
				override def pick: (E) => Option[T] = foreignKey.end.targetKey

				override def surepick: Option[(E) => T] = ???
			}
			
	}
*/



	class ForeignKeyComponent[K, FKM<:Mapping[K], TKM<:Mapping[K], TM<:Mapping[X], X :TypeTag](value :E=>Reference[X], key :FKM, target : =>ComponentPath[TM, TKM])
		extends BaseForeignKeyReferenceMapping[K, FKM, TKM, X, TM, X](key, target) with MandatoryComponent[Reference[X]]
	{
		override protected def selector: (E) => Reference[X] = value
	}
	
//	class ForeignKeyInverseComponent[T<:Mapping[O], K, FKM<:Mapping[K], KM<:Mapping[K], X, O, C](
//			value :E=>Reference[C], foreignKey :ComponentPath[T, _<:ForeignKeyReferenceMapping[K, FKM, KM, E, this.type, X]])(implicit ev :C ComposedOf O, targetTag :TypeTag[O])
//		extends BaseGenericReferenceKeyMapping[K, KM, FKM, O, T, C, Reference[C]](
//			symLink(foreignKey.end.targetKey.asInstanceOf[TypedComponentPath[this.type, KM, K]]),
//			foreignKey :+ foreignKey.end.key,
//			MappingReferenceFactory.Lazy(new ForeignKeyInverseReferenceFactory[K, O, C, T, X](foreignKey, foreignKey.end.referenceFactory))
//		) with MandatoryComponent[Reference[C]]
//	{
//		override protected def extract: (E) => Reference[C] = value
//	}

}

package com.adpilot.cortb.clientapi.prototype.repository.entities.meta

//object DomainModel extends BaseDomainTypes


trait DomainModel { domain =>
//	import self.{BaseTypes=>Base}
	import BaseTypes._
//	import DomainModel.Source
//	type Source[D<:DomainModel]
//	type Source = DomainModel.Source[this.type]
	type Source
//	type Store[E]

//	type CommonBaseClass
	type EntityModel[E] <: BaseEntityModel[E]
	type IndividualModel[E, I] <: BaseIndividualModel[E, I] with EntityModel[E]
	type EntityAttribute[E, T, +M<:BaseEntityModel[E]] <: BaseEntityAttribute[E, T, M]
	type EntityProperty[E, T, +M<:BaseEntityModel[E]] <: BaseEntityProperty[E, T, M] with EntityAttribute[E, T, M]
	type MutableEntityProperty[E, T, +M<:BaseEntityModel[E]] <: BaseMutableEntityProperty[E, T, M] with EntityProperty[E, T, M]
	type ReferableEntityAttribute[E, T, +M<:BaseEntityModel[E]] <: BaseReferableEntityAttribute[E, T, M] with EntityAttribute[E, T, M]
	type UniqueEntityProperty[E, T, +M<:BaseEntityModel[E]] <: BaseUniqueEntityProperty[E, T, M] with ReferableEntityAttribute[E, T, M] with EntityProperty[E, T, M]
	
	type EntityAttributeValue[E, T, +A <: BaseEntityAttribute[E, T, _]] <: BaseEntityAttributeValue[E, T, A]

//	type EntityRelationAttribute[E, C, +M<:BaseEntityModel[E], R <: EntityRelationSide[E]] <: BaseEntityRelationAttribute[E, C, M, R] with EntityAttribute[E, C, M]
//	type EntityRelationProperty[E, C, +M<:BaseEntityModel[E], R <: EntityRelationSide[E]] <: BaseEntityRelationProperty[E, C, M, R] with EntityRelationAttribute[E, C, M, R] with EntityProperty[E, C, M]
//	type EntityReferenceAttribute[E, C, +M<:BaseEntityModel[E], R] <: BaseEntityReferenceAttribute[E, C, M, R] with EntityRelationAttribute[E, C, M, BinaryEntityRelation[E, R]]
//	type EntityReferenceProperty[E, C, +M<:BaseEntityModel[E], R] <: BaseEntityReferenceProperty[E, C, M, R] with EntityRelationProperty[E, C, M, BinaryEntityRelation[E, R]]

	type MultiEntityRelation <: BaseMultiEntityRelation
	type EntityRelationSide[E] <: BaseEntityRelationSide[E]
	type BinaryEntityRelation[L, R] <: BaseBinaryEntityRelation[L, R] with MultiEntityRelation with EntityRelationSide[L]


	implicit def EntityExtension[E](entity :E)(implicit model :EntityModel[E]) = new model.EntityExtension(entity)



	object BaseTypes {
		trait BaseEntityModel[E] { //this :EntityModel[E] =>
			type Attribute[T] = EntityAttribute[E, T, this.type]
			type Property[T] = EntityProperty[E, T, this.type]
			type MutableProperty[T] = MutableEntityProperty[E, T, this.type]
			type Mutation[T] = EntityAttributeValue[E, T, MutableEntityProperty[E, T,  _>:this.type]]

			type ReferableAttribute[T] = ReferableEntityAttribute[E, T, this.type]
			type UniqueProperty[T] = UniqueEntityProperty[E, T, this.type]

//			type RelationAttribute[C, R<:EntityRelationSide[E]] = EntityRelationAttribute[E, C, this.type, R]
//			type RelationProperty[C, R<:EntityRelationSide[E]] = EntityRelationProperty[E, C, this.type, R]

			def attributes :Seq[Attribute[_]]
			def properties :Seq[Property[_]]

			def mutate(entity: E, mutations: Mutation[_]*): E



			class EntityExtension(val entity :E) {
				def apply[T, A<:Attribute[T]](attribute :A)(implicit source :Source) :T = attribute.get(entity)
			}
		}


		trait BaseIndividualModel[E, I] extends BaseEntityModel[E] { //this :IndividualModel[E, I] =>
			type IdentityType = I
			val Identity :MutableProperty[IdentityType]
		}

		
		trait BaseEntityAttribute[E, T, +M<:BaseEntityModel[E]] { //this :EntityAttribute[E, T] =>
			def := (value: T) :EntityAttributeValue[E, T, this.type]
			def get(entity :E)(implicit source :Source) :T //= FetchEntityAttribute(entity, this)
		}

		//trait VaryingEntityAssociate[E, T] extends EntityAssociate[E, T]


		trait BaseEntityProperty[E, T, +M<:BaseEntityModel[E]] extends BaseEntityAttribute[E, T, M] { //this :EntityProperty[E, T] =>
			def apply(entity: E): T
		}


		trait BaseMutableEntityProperty[E, T, +M<:BaseEntityModel[E]] extends BaseEntityProperty[E, T, M] { //this :MutableEntityProperty[E, T] =>
			def update(entity: E, value: T): E
		}
		
		trait BaseReferableEntityAttribute[E, T, +M<:BaseEntityModel[E]] extends BaseEntityAttribute[E, T, M]
		
		trait BaseUniqueEntityProperty[E, T, +M<:BaseEntityModel[E]] extends BaseEntityProperty[E, T, M] with BaseReferableEntityAttribute[E, T, M]


		
		
		trait BaseEntityAttributeValue[E, T, +A <: BaseEntityAttribute[E, T, _]] {
			def attribute: A

			def value: T
		}




		trait BaseEntityRelationAttribute[E, C, +M<:BaseEntityModel[E], R <: EntityRelationSide[E]] extends BaseEntityAttribute[E, C, M] {
//			this :EntityRelationAttribute[E, R, C] =>
		}


		trait BaseEntityRelationProperty[E, C, +M<:BaseEntityModel[E], R <: EntityRelationSide[E]] extends BaseEntityRelationAttribute[E, C, M, R] with BaseEntityProperty[E, C, M] {
//			this :EntityRelationProperty[E, R, C] =>
		}

		trait BaseEntityReferenceAttribute[E, C, +M<:BaseEntityModel[E], R]
			extends BaseEntityRelationAttribute[E, C, M, BinaryEntityRelation[E, R]]

		trait BaseEntityReferenceProperty[E, C, +M<:BaseEntityModel[E], R]
			extends BaseEntityReferenceAttribute[E, C, M, R] with BaseEntityRelationProperty[E, C, M, BinaryEntityRelation[E, R]]



		trait BaseMultiEntityRelation {
			type Side[E] = EntityRelationSide[E]

			def sides: Seq[Side[_]]
		}

		trait BaseEntityRelationSide[E] {
			def relation: MultiEntityRelation
			def source: EntityModel[E]
			def others: Seq[EntityRelationSide[_]]
		}


		trait BaseBinaryEntityRelation[L, R] extends BaseMultiEntityRelation with BaseEntityRelationSide[L] {
			self :MultiEntityRelation with EntityRelationSide[L] =>

			def relation = this

			def sides = Seq(self :EntityRelationSide[L], inverse)

			def others = Seq(inverse)

			def source: EntityModel[L] = left

			def target: EntityModel[R] = right

			def left: EntityModel[L]

			def right: EntityModel[R]

			def inverse: BinaryEntityRelation[R, L]
		}
			
	}


	def FetchEntityAttribute[E, T, M<:EntityModel[E]](entity :E, attribute :EntityAttribute[E, T, M])(implicit model :M, source :Source) :T

}




object DomainModel {
//	class Source[M<:DomainModel] protected[DomainModel] (val domain :M)
}


package com.adpilot.cortb.clientapi.prototype.repository.entities.meta


//object DomainModel extends BaseDomainTypes


trait DomainModelOld {
//	import self.{BaseTypes=>Base}
	import BaseTypes._
	
	type Source

//	type Store[E]

	type EntityModel[E] <: BaseEntityModel[E]
	type IndividualModel[E, I] <: BaseIndividualModel[E, I] with EntityModel[E]
	type EntityAttribute[E, T] <: BaseEntityAttribute[E, T]
	type EntityProperty[E, T] <: BaseEntityProperty[E, T] with EntityAttribute[E, T]
	type MutableEntityProperty[E, T] <: BaseMutableEntityProperty[E, T] with EntityProperty[E, T]
	
	type EntityAttributeValue[E, T, +A <: BaseEntityAttribute[E, T]] <: BaseEntityAttributeValue[E, T, A]

	type MultiEntityRelation <: BaseMultiEntityRelation
	type EntityRelationSide[E] <: BaseEntityRelationSide[E]
	type BinaryEntityRelation[L, R] <: BaseBinaryEntityRelation[L, R] with MultiEntityRelation with EntityRelationSide[L]

	type EntityRelationAttribute[E, R <: EntityRelationSide[E], C] <: BaseEntityRelationAttribute[E, R, C] with EntityAttribute[E, C]
	type EntityRelationProperty[E, R <: EntityRelationSide[E], C] <: BaseEntityRelationProperty[E, R, C] with EntityRelationAttribute[E, R, C] with EntityProperty[E, C]

	implicit def EntityExtension[E](entity :E)(implicit model :EntityModel[E]) = new model.EntityExtension(entity)

	object BaseTypes {
		trait BaseEntityModel[E] { //this :EntityModel[E] =>
			type Attribute[T] <: EntityAttribute[E, T]
			type Property[T] <: EntityProperty[E, T]

			type MutableProperty[T] <: MutableEntityProperty[E, T]
			type Mutation[T] = BaseEntityAttributeValue[E, T, MutableProperty[T]]

			//	trait Attribute[T] extends EntityAttribute[E, T]
			//	trait MutableAttribute[T] extends Attribute[T]

			def attributes :Seq[Attribute[_]]
			def properties :Seq[Property[_]]
//			def references: Seq[Reference[_, _]]


			def mutate(entity: E, changes: Mutation[_]*): E
//			def mutate(entity :E, changes :EntityAttributeValue[E, T, MutableProperty[T]]*)

			class EntityExtension(val entity :E) {
				def apply[T, A<:Attribute[T]](attribute :A)(implicit source :Source) :T = attribute.get(entity)
			}
		}


		trait BaseIndividualModel[E, I] extends BaseEntityModel[E] { //this :IndividualModel[E, I] =>
			type IdentityType = I
			val Identity :MutableProperty[IdentityType]
		}

		
		trait BaseEntityAttribute[E, T] { //this :EntityAttribute[E, T] =>
			def := (value: T) :EntityAttributeValue[E, T, this.type]
			def get(entity :E)(implicit source :Source) :T
		}

		//trait VaryingEntityAssociate[E, T] extends EntityAssociate[E, T]


		trait BaseEntityProperty[E, T] extends BaseEntityAttribute[E, T] { //this :EntityProperty[E, T] =>
			def apply(entity: E): T
		}


		trait BaseMutableEntityProperty[E, T] extends BaseEntityProperty[E, T] { //this :MutableEntityProperty[E, T] =>
			def update(entity: E, value: T): E
		}


		trait BaseEntityAttributeValue[E, T, +A <: BaseEntityAttribute[E, T]] {
			def attribute: A

			def value: T
		}




		trait BaseEntityRelationAttribute[E, R <: EntityRelationSide[E], C] extends BaseEntityAttribute[E, C] {
//			this :EntityRelationAttribute[E, R, C] =>
		}


		trait BaseEntityRelationProperty[E, R <: EntityRelationSide[E], C] extends BaseEntityRelationAttribute[E, R, C] with BaseEntityProperty[E, C] {
//			this :EntityRelationProperty[E, R, C] =>
		}





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

//			def sides = Seq(self :EntityRelationSide[L], inverse)
			def others = Seq(inverse)

			def source: EntityModel[L] = left

			def target: EntityModel[R] = right

			def left: EntityModel[L]

			def right: EntityModel[R]

			def inverse: BinaryEntityRelation[R, L]
		}
			
	}


}





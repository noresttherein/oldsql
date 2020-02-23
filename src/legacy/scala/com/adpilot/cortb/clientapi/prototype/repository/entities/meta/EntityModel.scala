package com.adpilot.cortb.clientapi.prototype.repository.entities.meta


class BaseDomainTypes {


	trait EntityModel[E] {
//		type Dependency[T] = EntityRelationProperty[E, T, _]
//		type Reference[T, C] = EntityRelationProperty[E, T, C]


		type Attribute[T] <: EntityAttribute[E, T]
		type Property[T] <: EntityProperty[E, T]

		type MutableProperty[T] <: MutableEntityProperty[E, T]
		type Mutation[T] = AttributeValue[E, T, MutableProperty[T]]

		//	trait Attribute[T] extends EntityAttribute[E, T]
		//	trait MutableAttribute[T] extends Attribute[T]

		def attributes: Seq[Attribute[_]]

//		def references: Seq[Reference[_, _]]


		def mutate(entity: E, mutations: Mutation[_]*): E
	}


	trait IndividualModel[E, I] extends EntityModel[E] {
		type IdentityType = I

		val Identity: Attribute[IdentityType]
	}


	trait EntityAttribute[E, T] {
		def :=(value: T): AttributeValue[E, T, this.type]
	}

	//trait VaryingEntityAssociate[E, T] extends EntityAssociate[E, T]


	trait EntityProperty[E, T] extends EntityAttribute[E, T] {
		def apply(entity: E): T
	}


	trait MutableEntityProperty[E, T] extends EntityProperty[E, T] {
		def update(entity: E, value: T): E
	}


	trait AttributeValue[E, T, +A <: EntityAttribute[E, T]] {
		def attribute: A

		def value: T
	}


	trait EntityRelationAttribute[E, R <: EntityRelationSide[E], C] extends EntityAttribute[E, C]


	trait EntityRelationProperty[E, R <: EntityRelationSide[E], C] extends EntityRelationAttribute[E, R, C] with EntityProperty[E, C]


	trait MultiEntityRelation {
		type Side[E] = EntityRelationSide[E]

		def sides: Seq[Side[_]]
	}

	trait EntityRelationSide[E] {
		def relation: MultiEntityRelation

		def source: EntityModel[E]

		def others: Seq[EntityRelationSide[_]]
	}


	trait BinaryEntityRelation[L, R] extends MultiEntityRelation with EntityRelationSide[L] {
		def sides = Seq(this, inverse)

		def others = Seq(inverse)

		def source: EntityModel[L] = left

		def target: EntityModel[R] = right

		def left: EntityModel[L]

		def right: EntityModel[R]

		def inverse: BinaryEntityRelation[R, L]
	}

}
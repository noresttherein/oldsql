package com.adpilot.cortb.clientapi.prototype.repository.entities.meta


trait BaseDomainModel extends DomainModel {
	import BaseTypes._

	type EntityAttribute[E, T, +M<:BaseEntityModel[E]] = BaseEntityAttribute[E, T, M]
	type EntityProperty[E, T, +M<:BaseEntityModel[E]] = BaseEntityProperty[E, T, M]
	type MutableEntityProperty[E, T, +M<:BaseEntityModel[E]] = BaseMutableEntityProperty[E, T, M]
	type ReferableEntityAttribute[E, T, +M<:BaseEntityModel[E]] = BaseReferableEntityAttribute[E, T, M]
	type UniqueEntityProperty[E, T, +M<:BaseEntityModel[E]] = BaseUniqueEntityProperty[E, T, M]

	type EntityAttributeValue[E, T, +A <: BaseEntityAttribute[E, T, _]] = BaseEntityAttributeValue[E, T, A]

	type MultiEntityRelation = BaseMultiEntityRelation
	type EntityRelationSide[E] = BaseEntityRelationSide[E]
	type BinaryEntityRelation[L, R] = BaseBinaryEntityRelation[L, R]

	type EntityRelationAttribute[E, C, +M<:BaseEntityModel[E], R <: EntityRelationSide[E]] = BaseEntityRelationAttribute[E, C, M, R]
	type EntityRelationProperty[E, C, +M<:BaseEntityModel[E], R <: EntityRelationSide[E]] = BaseEntityRelationProperty[E, C, M, R]

}

//object BaseDomainModel extends BaseDomainModel
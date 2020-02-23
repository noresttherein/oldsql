package com.adpilot.cortb.clientapi.prototype.repository.entities.meta

trait DomainProxy[+D <: DomainModel] extends DomainModel { domain =>
	import BaseTypes._

	val ImplementationModel :D
//	implicit val proxiedDomain :D

//	type EntityModel[E] <: EntityProxy[E]
//	type IndividualModel[E, I] <: BaseIndividualModel[E, I] with EntityModel[E]
//	type EntityAttribute[E, T, +M<:BaseEntityModel[E]] <:
//		BaseEntityAttribute[E, T, M]
//
//	type EntityProperty[E, T, +M<:BaseEntityModel[E]] <:
//		BaseEntityProperty[E, T, M] with EntityAttribute[E, T, M]
//
//	type MutableEntityProperty[E, T, +M<:BaseEntityModel[E]]  <:
//		BaseMutableEntityProperty[E, T, M] with EntityProperty[E, T, M]
//
//	type ReferableEntityAttribute[E, T, +M<:BaseEntityModel[E]] <:
//		BaseReferableEntityAttribute[E, T, M] with EntityAttribute[E, T, M]
//
//	type UniqueEntityProperty[E, T, +M<:BaseEntityModel[E]] <:
//		BaseUniqueEntityProperty[E, T, M] with ReferableEntityAttribute[E, T, M] with EntityProperty[E, T, M]


//	type EntityAttributeValue[E, T, +A <: BaseEntityAttribute[E, T, _]] <:
//		BaseEntityAttributeValue[E, T, A]




//	type EntityRelationAttribute[E, C, +M<:BaseEntityModel[E], R <: EntityRelationSide[E]] <:
//		BaseEntityRelationAttribute[E, C, M, R] with EntityAttribute[E, C, M]

//	type EntityRelationProperty[E, C, +M<:BaseEntityModel[E], R <: EntityRelationSide[E]] <:
//		BaseEntityRelationProperty[E, C, M, R] with EntityRelationAttribute[E, C, M, R] with EntityProperty[E, C, M]



//	type MultiEntityRelation <: BaseMultiEntityRelation

//	type EntityRelationSide[E] <: BaseEntityRelationSide[E]

//	type BinaryEntityRelation[L, R] <:
//		BaseBinaryEntityRelation[L, R] with MultiEntityRelation with EntityRelationSide[L]



	def EntityModel[E](entity :ImplementationModel.EntityModel[E]) :EntityModel[E] =
		new EntityModel[E] { val ProxiedEntityModel = entity }
	
	def IndividualModel[E, I](individual :ImplementationModel.IndividualModel[E, I]) :IndividualModel[E, I] =
		new IndividualModel[E, I](individual)



	def MultiEntityRelation(relation :ImplementationModel.MultiEntityRelation) :MultiEntityRelation = ???

	def EntityRelationSide[E](relation :MultiEntityRelation, side :ImplementationModel.EntityRelationSide[E]) :EntityRelationSide[E] = ???

	def BinaryEntityRelation[L, R](relation :ImplementationModel.BinaryEntityRelation[L, R]) :BinaryEntityRelation[L, R] = ???

//	def EntityAttribute[E, T, M<:EntityModel[E]](entity :M)(attr :entity.ImplementationModel.Attribute[T]) :EntityAttribute[E, T, M] = ???

//	def EntityProperty[E, T, M<:EntityModel[E]](entity :M)(prop :entity.ImplementationModel.Property[T]) :EntityProperty[E, T, M] = ???

//	def MutableEntityProperty[E, T, M<:EntityModel[E]](entity :M)(prop :entity.ImplementationModel.MutableProperty)

//	trait ProxyType[T] {
//		val ImplementationModel :T
//	}
	
	trait EntityModel[E] extends BaseEntityModel[E]  {
		val ProxiedEntityModel :ImplementationModel.EntityModel[E]

		protected def Attribute[T](attr :ImplementationModel.EntityModel[E]#Attribute[T]) :Attribute[T] =
			new EntityAttributeImpl[E, T, this.type](attr)

		protected def Property[T](prop :ImplementationModel.EntityModel[E]#Property[T]) :Property[T] = ???
		protected def MutableProperty[T](prop :ImplementationModel.MutableEntityProperty[E, T, domain.ImplementationModel.EntityModel[E]]) :MutableProperty[T] =
			new MutableEntityProperty[E, T, this.type](prop)

		protected def ReferableAttribute[T](attr :ImplementationModel.EntityModel[E]#ReferableAttribute[T]) :ReferableAttribute[T] = ???
		protected def UniqueProperty[T](prop :ImplementationModel.EntityModel[E]#UniqueProperty[T]) :UniqueProperty[T] = ???



		override def attributes: Seq[Attribute[_]] = ProxiedEntityModel.attributes.map(attr => Attribute(attr))

		override def properties: Seq[Property[_]] = ProxiedEntityModel.properties.map(prop => Property(prop))

		override def mutate(entity: E, mutations: Mutation[_]*): E =
			ProxiedEntityModel.mutate(entity, mutations.map(m => m.ImplementationModel.asInstanceOf[ProxiedEntityModel.Mutation[_]]):_*)
	}
	
	class IndividualModel[E, I](val ProxiedIndividualModel :ImplementationModel.IndividualModel[E, I])
		extends EntityModel[E] with BaseIndividualModel[E, I]
	{
		val ProxiedEntityModel = ProxiedIndividualModel :ImplementationModel.EntityModel[E]
		val Identity = MutableProperty[IdentityType](this.ProxiedIndividualModel.Identity)
		
	}


	case class EntityAttributeValue[E, T, +A <: BaseEntityAttribute[E, T, _]](attribute :A, value :T, ImplementationModel :domain.ImplementationModel.EntityAttributeValue[E, T, _ <: domain.ImplementationModel.EntityAttribute[E, T, _]])
		extends BaseEntityAttributeValue[E, T, A]


	trait EntityAttribute[E, T, +M<:BaseEntityModel[E]]
		extends BaseEntityAttribute[E, T, M] //with ProxyType[D#EntityAttribute[E, T, D#EntityModel[E]]]
	{
		val ProxiedAttribute :ImplementationModel.EntityAttribute[E, T, ImplementationModel.EntityModel[E]]

		override def :=(value: T): EntityAttributeValue[E, T, this.type] = EntityAttributeValue(this, value, ProxiedAttribute := value)

		override def get(entity: E)(implicit source: Source): T = ??? //ImplementationModel.get(entity)(source)
//			FetchEntityAttribute[E, T, M](entity, this)(???, source)
	}

	class EntityAttributeImpl[E, T, +M<:BaseEntityModel[E]](val ProxiedAttribute :ImplementationModel.EntityAttribute[E, T, ImplementationModel.EntityModel[E]]) extends EntityAttribute[E, T, M]


	trait EntityProperty[E, T, +M<:BaseEntityModel[E]]
		extends EntityAttribute[E, T, M] with BaseEntityProperty[E, T, M]
	{
		val ProxiedProperty :ImplementationModel.EntityProperty[E, T, ImplementationModel.EntityModel[E]]

		override def apply(entity: E): T = ProxiedProperty(entity)
	}

	class EntityPropertyImpl[E, T, +M<:BaseEntityModel[E]](val ProxiedProperty :ImplementationModel.EntityProperty[E, T, ImplementationModel.EntityModel[E]])
		extends EntityAttributeImpl[E, T, M](ProxiedProperty) with EntityProperty[E, T, M]


	class MutableEntityProperty[E, T, +M<:BaseEntityModel[E]](override val ProxiedProperty :ImplementationModel.MutableEntityProperty[E, T, ImplementationModel.EntityModel[E]])
		extends EntityProperty[E, T, M] with BaseMutableEntityProperty[E, T, M]
	{
		val ProxiedAttribute = ProxiedProperty :ImplementationModel.EntityAttribute[E, T, ImplementationModel.EntityModel[E]]

		override def update(entity: E, value: T): E = ProxiedProperty.update(entity, value)
	}

	class ReferableEntityAttribute[E, T, +M<:BaseEntityModel[E]](val ProxiedReferableAttribute :ImplementationModel.ReferableEntityAttribute[E, T, ImplementationModel.EntityModel[E]])
		extends EntityAttributeImpl[E, T, M](ProxiedReferableAttribute) with BaseReferableEntityAttribute[E, T, M]

	class UniqueEntityProperty[E, T, +M<:BaseEntityModel[E]](override val ProxiedProperty :ImplementationModel.UniqueEntityProperty[E, T, ImplementationModel.EntityModel[E]])
		extends ReferableEntityAttribute[E, T, M](ProxiedProperty) with BaseUniqueEntityProperty[E, T, M] with EntityProperty[E, T, M]



//	class MultiEntityRelation(val ImplementationModel :ImplementationModel.MultiEntityRelation) extends BaseMultiEntityRelation {
//		val sides: Seq[Side[_]] = ImplementationModel.sides.map(EntityRelationSide(this, _))
//	}
	trait MultiEntityRelation extends BaseMultiEntityRelation {
		val ProxiedMultiEntityRelation :ImplementationModel.MultiEntityRelation
		lazy val sides: Seq[Side[_]] = ProxiedMultiEntityRelation.sides.map(EntityRelationSide(this, _))
	}

	trait EntityRelationSide[E] //(val relation :MultiEntityRelation, val ImplementationModel :ImplementationModel.EntityRelationSide[E])
		extends BaseEntityRelationSide[E]
	{
		val ProxiedRelationSide :ImplementationModel.EntityRelationSide[E]

		override def source: EntityModel[E] = EntityModel(ProxiedRelationSide.source) //todo: pass it as an argument?

		override def others: Seq[EntityRelationSide[_]] = ProxiedRelationSide.others.map(side => EntityRelationSide(relation, side))
	}

	class BinaryEntityRelation[L, R](val ProxiedBinaryRelation :ImplementationModel.BinaryEntityRelation[L, R])
		extends EntityRelationSide[L] with MultiEntityRelation with BaseBinaryEntityRelation[L, R]
	{

		val ProxiedMultiEntityRelation = ProxiedBinaryRelation :ImplementationModel.MultiEntityRelation
		val ProxiedRelationSide = ProxiedBinaryRelation :ImplementationModel.EntityRelationSide[L]

		override def left: EntityModel[L] = EntityModel(ProxiedBinaryRelation.left)

		override def right: EntityModel[R] = EntityModel(ProxiedBinaryRelation.right)

		override def inverse: BinaryEntityRelation[R, L] = BinaryEntityRelation(ProxiedBinaryRelation.inverse)


		override def source = left

		override def target = right

		override def others = super[BaseBinaryEntityRelation].others

		override lazy val sides = super[BaseBinaryEntityRelation].sides
	}

//	case class EntityAttributeProxy[E, T, +M<:BaseEntityModel[E]](ImplementationModel :ImplementationModel.EntityAttribute[E, T, ImplementationModel.EntityModel[E]]) extends EntityAttribute[E, T, M]


//	type Source = SourceProxy

	def FetchEntityAttribute[E, T, M <: EntityModel[E]](entity: E, attribute: EntityAttribute[E, T, M])(implicit model: M, source: Source): T =
		ImplementationModel.FetchEntityAttribute(entity, attribute.ProxiedAttribute)(???, source.ImplementationModel)
//		source.asInstanceOf[SourceProxy].fetch(entity, attribute)
//		ImplementationModel.FetchEntityAttribute[E, T, ImplementationModel.EntityModel[E]](entity, attribute.ImplementationModel)(model.ImplementationModel, source)

	class Source(val ImplementationModel :domain.ImplementationModel.Source) { //extends DomainModel.Source[domain.type](domain) {
//		def fetch[E, T, M<:EntityModel[E]](entity :E, attr :EntityAttribute[E, T, M]) :T =
//			domain.ImplementationModel.FetchEntityAttribute(entity, attr.ImplementationModel)(???, ImplementationModel)
	}

	implicit def Source(implicit source :ImplementationModel.Source) :Source = new Source(source)

}




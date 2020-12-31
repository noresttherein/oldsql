package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}






trait EntityMappingTemplate[+C[A] <: RefinedMapping[PK, A], PK, S, O] extends BaseMapping[S, O] {
	val pk :C[O]
}



object EntityMappingTemplate {
	implicit def primaryKeyOf[M[A] <: EntityMappingTemplate[C, PK, S, A], C[A] <: RefinedMapping[PK, A], PK, S]
			:PrimaryKeyOf[M] { type Key = PK; type PKMapping[O] = C[O] } =
		new PrimaryKeyOf[M] {
			override type Key = PK
			override type PKMapping[O] = C[O]

			override def apply[O](entity :M[O]) = entity.pk
		}
}



/**
  * @author Marcin Mościcki
  */
trait EntityMapping[PK, S, O] extends EntityMappingTemplate[MappingOf[PK]#Projection, PK, S, O] {
	val pk :Component[PK]
}

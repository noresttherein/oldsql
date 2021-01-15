package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}






trait EntityMappingTemplate[+C[A] <: RefinedMapping[PK, A], PK, S, O] extends BaseMapping[S, O] {
	val pk :C[O]
}



object EntityMappingTemplate {
	implicit def primaryKeyOf[C[A] <: RefinedMapping[PK, A], PK, S]
			:PrimaryKeyOf[({ type M[O] = EntityMappingTemplate[C, PK, S, O] })#M]
				{ type Key = PK; type PKMapping[O] = C[O] } =
		new PrimaryKeyOf[({ type M[O] = EntityMappingTemplate[C, PK, S, O] })#M] {
			override type Key = PK
			override type PKMapping[O] = C[O]

			override def apply[O](entity :EntityMappingTemplate[C, PK, S, O]) = entity.pk
		}
}



/**
  * @author Marcin Mościcki
  */
trait EntityMapping[PK, S, O] extends EntityMappingTemplate[MappingOf[PK]#Projection, PK, S, O] {
	val pk :Component[PK]
}

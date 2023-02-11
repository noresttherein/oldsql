package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingOf, TypedMapping}






trait GenericEntityMapping[+C[A] <: TypedMapping[PK, A], PK, S, O] extends BaseMapping[S, O] {
	val pk :C[O]
}



object GenericEntityMapping {
	implicit def primaryKeyOf[C[A] <: TypedMapping[PK, A], PK, S]
			:PrimaryKeyOf[({ type M[O] = GenericEntityMapping[C, PK, S, O] })#M]
				{ type Key = PK; type PKMapping[O] = C[O] } =
		new PrimaryKeyOf[({ type M[O] = GenericEntityMapping[C, PK, S, O] })#M] {
			override type Key = PK
			override type PKMapping[O] = C[O]

			override def apply[O](entity :GenericEntityMapping[C, PK, S, O]) = entity.pk
		}
}



/**
  * @author Marcin Mościcki
  */
trait EntityMapping[PK, S, O] extends GenericEntityMapping[MappingOf[PK]#Projection, PK, S, O] {
	val pk :Component[PK]
}

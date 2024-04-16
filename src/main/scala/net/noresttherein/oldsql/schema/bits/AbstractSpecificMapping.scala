package net.noresttherein.oldsql.schema.bits

/*
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bases.StableMapping.StableMappingTemplate




abstract class AbstractSpecificMapping
               [+Comp[T, Q] <: TypedMapping[T, Q], +Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], S, O]
               (override val components     :Unique[Comp[_, O]],
                override val subcomponents  :Unique[Comp[_, O]],
                override val columns        :Unique[Col[_, O]],
                override val extracts       :BaseMapping[S, O]#like[Comp]#ExtractMap,
                override val columnExtracts :BaseMapping[S, O]#like[Col]#ColumnExtractMap)
	extends BaseMapping[S, O] with StableMappingTemplate[Comp, Col]
{
	def this(components     :Unique[Comp[_, O]],
	         extracts       :BaseMapping[S, O]#like[Comp]#ExtractMap,
	         columnExtracts :BaseMapping[S, O]#like[Comp]#ColumnExtractMap) =
		this(
			components,
			Unique from extracts.view.map(_._1),
			Unique from columnExtracts.view.map(_._2.export),
			extracts,
			columnExtracts
		)
}
*/
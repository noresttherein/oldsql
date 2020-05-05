package net.noresttherein.oldsql

import net.noresttherein.oldsql.collection.NaturalMap
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, Requisite}
import net.noresttherein.oldsql.schema.Buff.BuffMappingFailureException
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.MappingExtract.GenericMappingExtract
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.slang._

/**
  * @author Marcin Mościcki
  */
package object schema {

	/** A `MappingExtract` describes the parent-child relationship between a mapping and its component.
	  * It serves three functions:
	  *   - provides a means of extracting the value of the component from the value of the parent;
	  *   - retrieves the value of a component from `ComponentValues`;
	  *   - provides the canonical, 'export' version of the component, that is the version with any wholesale
	  *     modifications declared in the parent mapping (or some other mapping on the path to the subcomponent),
	  *     applied to the original version of the component. This includes buffs and column prefix declarations
	  *     defined for all subcomponents of a mapping.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.apply[T](TypedMapping[T] ]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]]
	  */
	type MappingExtract[-S, T, O] = GenericMappingExtract[TypedMapping[T, O], S, T, O]

	type ColumnMappingExtract[S, T, O] = GenericMappingExtract[ColumnMapping[T, O], S, T, O]






	private[schema] def mapForm[S, X](form :ColumnForm[S])(map :S =?> X, unmap :X =?> S)
	                                 (implicit nulls :NullValue[X] = null) :ColumnForm[X] =
		(map, unmap) match {
			case (Requisite(there), Requisite(back)) =>
				if (nulls == null) form.bimapNull(there)(back)
				else form.bimap(there)(back)
			case _ =>
				if (nulls == null) form.biflatMapNull(map.optional)(unmap.optional)
				else form.biflatMap(map.optional)(unmap.optional)
		} //todo: implement form mapping with extractors.



	private[schema] def mapBuffs[S, X](mapping :MappingOf[S])(map :S =?> X, unmap :X =?> S) :Seq[Buff[X]] =
		mapping.buffs.map { buff =>
			buff.bimap(
				map.requisite getOrElse {
					val flatMap = map.optional
					s :S => flatMap(s) getOrElse {
						throw new BuffMappingFailureException(
							s"Failed mapping of $mapping: could not derive the value for buff $buff from $s."
						)
					}
				},
				unmap.requisite getOrElse {
					val flatUnmap = unmap.optional
					x :X => flatUnmap(x) getOrElse {
						throw new BuffMappingFailureException(
							s"Failed mapping of $mapping: could not unmap value $x as part of the buff $buff."
						)
					}
				}
			)
		}



	private[schema] def flatMapBuffs[S, X](mapping :MappingOf[S])
	                                      (map :S => Option[X], unmap :X => Option[S]) :Seq[Buff[X]] =
		mapping.buffs.map { buff => buff.bimap(
			s => map(s) getOrElse {
				throw new BuffMappingFailureException(s"Failed mapping buff $buff of mapping $mapping: no value returned for $s by $map.")
			},
			x => unmap(x) getOrElse {
				throw new BuffMappingFailureException(s"Failed reverse mapping buff $buff of mapping $mapping: no value returned for $x by $unmap")
			}
		)}


	private[schema] def cascadeBuffs[S, X](mapping :MappingOf[S])(map :S =?> X) :Seq[Buff[X]] =
		cascadeBuffs[S, X](mapping.buffs, mapping.toString)(map)

	private[schema] def cascadeBuffs[S, X](buffs :Seq[Buff[S]], owner: =>String)(map :S =?> X) :Seq[Buff[X]] =
		buffs.flatMap(buff => buff.cascade(map.requisite getOrElse {
			s :S => map.get(s) getOrElse {
				throw new BuffMappingFailureException(
					s"Failed cascading buff $buff from $owner: no value returned for $s by $map")
			}
		}))






	private[schema] def selectColumnExtracts[S, O]
	                    (mapping :TypedMapping[S, O])
	                    (extracts :NaturalMap[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract])
			:NaturalMap[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract] =
		selectColumnExtracts(mapping.toString)(extracts)


	private[schema] def selectColumnExtracts[S, O]
	                    (mapping: => String)
	                    (extracts :NaturalMap[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract])
			:NaturalMap[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract] =
	{
		def castToColumn[T](entry :Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T]) = 
			entry._1 match {
				case column :ColumnMapping[T @unchecked, O @unchecked] =>
					Some(Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](
						column, entry._2.asInstanceOf[ColumnMappingExtract[S, T, O]]
					))
				case _ => None
			}
		
		extracts.flatMap(castToColumn(_))
	}



//	@inline def extractAssoc[S, T, O]
//	                        (mapping :TypedMapping[S, O], comp :TypedMapping[T, O], extract :MappingExtract[S, T, O])
//			:Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T] =
//		Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T](comp, extract)
//
//	@inline def extractAssoc[S, T, O]
//	                        (mapping :TypedMapping[S, O], col :ColumnMapping[T, O], extract :ColumnExtract[S, T, O])
//			:Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T] =
//		Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](col, extract)

	@inline private[schema] def composeExtracts[S, X, T, O]
	                            (extracts :TypedMapping[X, O]#ExtractMap, extractor :MappingExtract[S, X, O])
			:TypedMapping[S, O]#ExtractMap =
		extracts.map(composeExtractAssoc(extractor)(_))

	@inline private[schema] def composeExtracts[S, X, T, O](mapping :TypedMapping[X, O], extractor :S =?> X)
			:TypedMapping[S, O]#ExtractMap =
		mapping.extracts.map(composeExtractAssoc(mapping, extractor)(_))



	@inline private[schema] def composeColumnExtracts[S, X, T, O]
	                            (extracts :TypedMapping[X, O]#ColumnExtractMap, extractor :MappingExtract[S, X, O])
			:TypedMapping[S, O]#ColumnExtractMap =
		extracts.map(composeColumnExtractAssoc(extractor)(_))

	@inline private[schema] def composeColumnExtracts[S, X, T, O](mapping :TypedMapping[X, O], extractor :S =?> X)
			:TypedMapping[S, O]#ColumnExtractMap =
		mapping.columnExtracts.map(composeColumnExtractAssoc(mapping, extractor)(_))



	@inline private[schema] def composeExtractAssoc[S, X, T, O]
	                            (extractor :MappingExtract[S, X, O])
	                            (entry :Assoc[TypedMapping[X, O]#Component, TypedMapping[X, O]#Extract, T])
			:Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T] =
		Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T](entry._1, entry._2 compose extractor)

	@inline private[schema] def composeExtractAssoc[S, X, T, O](mapping :MappingFrom[O], extractor :S =?> X)
	                            (entry :Assoc[TypedMapping[X, O]#Component, TypedMapping[X, O]#Extract, T])
			:Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T] =
		Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T](entry._1, entry._2 compose extractor)

	@inline private[schema] def composeExtractAssoc[S, X, T, O](mapping :MappingFrom[O], f : S => X)
	                            (entry :Assoc[TypedMapping[X, O]#Component, TypedMapping[X, O]#Extract, T])
			:Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T] =
		Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T](entry._1, entry._2 compose f)



	@inline private[schema] def composeColumnExtractAssoc[S, X, T, O]
	                            (extractor :MappingExtract[S, X, O])
	                            (entry :Assoc[TypedMapping[X, O]#Column, TypedMapping[X, O]#ColumnExtract, T])
			:Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T] =
		Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose extractor)

	@inline private[schema] def composeColumnExtractAssoc[S, X, T, O]
	                            (mapping :MappingFrom[O], extractor : S =?> X)
	                            (entry :Assoc[TypedMapping[X, O]#Column, TypedMapping[X, O]#ColumnExtract, T])
	:Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T] =
		Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose extractor)

	@inline private[schema] def composeColumnExtractAssoc[S, X, T, O]
	                            (mapping :MappingFrom[O], f : S => X)
	                            (entry :Assoc[TypedMapping[X, O]#Column, TypedMapping[X, O]#ColumnExtract, T])
	:Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T] =
		Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose f)




	private[schema] def prefixOption(prefix :String) :Option[String] = prefix.length > 0 ifTrue prefix

}

package net.noresttherein.oldsql

import net.noresttherein.oldsql.collection.NaturalMap
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.exceptions.BuffMappingFailureException
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}


//here be implicits
import net.noresttherein.oldsql.slang._




/**
  * @author Marcin Mościcki
  */
package object schema {

	/** A `MappingExtract` describes the parent-child relationship between a mapping and its component.
	  * It serves three functions:
	  *   - provides a means of extracting the value of the component from the value of the parent;
	  *   - retrieves the value of a component from `ComponentValues`;
	  *   - provides the operative, 'export' version of the component, that is the version with any modifications,
	  *     declared in the parent mapping (or some other mapping on the path to the subcomponent),
	  *     applied to the original version of the component. This includes additional buffs and column prefix
	  *     declarations defined for all subcomponents of a mapping.
	  * As with its [[net.noresttherein.oldsql.morsels.Extractor Extractor]] super type, the primary extractor function
	  * returns an `Option`, thus allowing situations where a value for a component does not exist in a given
	  * subject value `S` (and forcing using code to take this into account). This is conceptually different from
	  * a property as an `Option` (or similar), which should still be generally declared as a
	  * [[net.noresttherein.oldsql.schema.MappingExtract$.req requisite]] (mandatory) property of type `Option[T]`.
	  * The optional nature is reserved primarily for situations where a value is not present for technical reasons,
	  * but might in theory exist in the logical model. The primary example of this is data which was not loaded
	  * with the main subject, for example a BLOB property. It might still be declared as an `Option`
	  * in the entity class, but (assuming it is declared as ''not null'' in the database) in this case it signifies
	  * the value not being present only for this particular instance, rather than the modeled logical entity.
	  * An extract returning `None` in this case (rather than `Some(None)`) can be taken to mean that the column
	  * should not be included in the update for example.
	  *
	  * This is a type alias for a template class parameterized with the component mapping type, specified as
	  * the most generic [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping RefinedMapping[T, O] ]].
	  * @see [[net.noresttherein.oldsql.schema.GenericExtract]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.apply[T](RefinedMapping[T]) ]]
	  * @see [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]]
	  * @tparam S the subject type of the parent mapping.
	  * @tparam T the subject type of the component mapping.
	  * @tparam O the origin type of the parent and child mappings.
	  */
	type MappingExtract[-S, T, O] = GenericExtract[RefinedMapping[T, O], S, T, O]

	/** A `MappingExtract` for a column with subject type `T` of a parent mapping with subject type `S` and origin
	  * type `O`.
	  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
	  * @see [[net.noresttherein.oldsql.schema.GenericExtract]]
	  */
	type ColumnMappingExtract[S, T, O] = GenericExtract[ColumnMapping[T, O], S, T, O]






	private[schema] def mapBuffs[S, X](mapping :MappingOf[S])(map :S =?> X, unmap :X =?> S) :Buffs[X] =
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
	                                      (map :S => Option[X], unmap :X => Option[S]) :Buffs[X] =
		mapping.buffs.map { buff => buff.bimap[X](
			s => map(s) getOrElse {
				throw new BuffMappingFailureException(
					s"Failed mapping buff $buff of mapping $mapping: no value returned for $s by $map."
				)
			},
			x => unmap(x) getOrElse {
				throw new BuffMappingFailureException(
					s"Failed reverse mapping buff $buff of mapping $mapping: no value returned for $x by $unmap"
				)
			}
		)}


	private[schema] def cascadeBuffs[S, X](mapping :MappingOf[S])(map :S =?> X) :Buffs[X] =
		cascadeBuffs[S, X](mapping.buffs, mapping.toString)(map)

	private[schema] def cascadeBuffs[S, X](buffs :Buffs[S], owner: => String)(map :S =?> X) :Buffs[X] =
		buffs.flatMap(buff =>
			buff.cascade(map.requisite getOrElse {
				s :S => map.opt(s) getOrElse {
					throw new BuffMappingFailureException(
						s"Failed cascading buff $buff from $owner: no value returned for $s by $map"
					)
				}
			})
		)






	private[oldsql] def filterColumnExtracts[S, O]
	                    (mapping :RefinedMapping[S, O])
	                    (extracts :NaturalMap[RefinedMapping[S, O]#Component, RefinedMapping[S, O]#Extract])
			:NaturalMap[RefinedMapping[S, O]#Column, RefinedMapping[S, O]#ColumnExtract] =
		filterColumnExtracts(mapping.toString)(extracts)


	private[oldsql] def filterColumnExtracts[S, O]
	                    (mapping: => String)
	                    (extracts :NaturalMap[RefinedMapping[S, O]#Component, RefinedMapping[S, O]#Extract])
			:NaturalMap[RefinedMapping[S, O]#Column, RefinedMapping[S, O]#ColumnExtract] =
	{
		def castToColumn[T](entry :Assoc[RefinedMapping[S, O]#Component, RefinedMapping[S, O]#Extract, T]) =
			entry._1 match {
				case column :ColumnMapping[T @unchecked, O @unchecked] =>
					Some(Assoc[RefinedMapping[S, O]#Column, RefinedMapping[S, O]#ColumnExtract, T](
						column, entry._2.asInstanceOf[ColumnMappingExtract[S, T, O]]
					))
				case _ => None
			}
		
		extracts.flatMap(castToColumn(_))
	}



	private[oldsql] def composeExtracts[S, X, O](extract :MappingExtract[S, X, O])
			:RefinedMapping[S, O]#ExtractMap =
		composeExtracts(extract.export, extract)

	private[oldsql] def composeExtracts[S, X, O]
	                    (extracts :RefinedMapping[X, O]#ExtractMap, extract :MappingExtract[S, X, O])
			:RefinedMapping[S, O]#ExtractMap =
		extracts.map(composeExtractAssoc(extract)(_))

	private[oldsql] def composeExtracts[S, X, O](mapping :RefinedMapping[X, O], extract :S =?> X)
			:RefinedMapping[S, O]#ExtractMap =
		mapping.extracts.map(composeExtractAssoc(mapping, extract)(_))



	private[oldsql] def composeColumnExtracts[S, X, O](extract :MappingExtract[S, X, O])
			:RefinedMapping[S, O]#ColumnExtractMap =
		composeColumnExtracts(extract.export, extract)

	private[oldsql] def composeColumnExtracts[S, X, O]
	                    (extracts :RefinedMapping[X, O]#ColumnExtractMap, extractor :MappingExtract[S, X, O])
			:RefinedMapping[S, O]#ColumnExtractMap =
		extracts.map(composeColumnExtractAssoc(extractor)(_))

	private[oldsql] def composeColumnExtracts[S, X, O](mapping :RefinedMapping[X, O], extractor :S =?> X)
			:RefinedMapping[S, O]#ColumnExtractMap =
		mapping.columnExtracts.map(composeColumnExtractAssoc(mapping, extractor)(_))



	@inline private[oldsql] def composeExtractAssoc[S, X, T, O]
	                            (extractor :MappingExtract[S, X, O])
	                            (entry :Assoc[RefinedMapping[X, O]#Component, RefinedMapping[X, O]#Extract, T])
			:Assoc[RefinedMapping[S, O]#Component, RefinedMapping[S, O]#Extract, T] =
		Assoc[RefinedMapping[S, O]#Component, RefinedMapping[S, O]#Extract, T](entry._1, entry._2 compose extractor)

	@inline private[oldsql] def composeExtractAssoc[S, X, T, O](mapping :RefinedMapping[X, O], extractor :S =?> X)
	                            (entry :Assoc[RefinedMapping[X, O]#Component, RefinedMapping[X, O]#Extract, T])
			:Assoc[RefinedMapping[S, O]#Component, RefinedMapping[S, O]#Extract, T] =
		Assoc[RefinedMapping[S, O]#Component, RefinedMapping[S, O]#Extract, T](entry._1, entry._2 compose extractor)

	@inline private[oldsql] def composeExtractAssoc[S, X, T, O](mapping :MappingAt[O], f : S => X)
	                            (entry :Assoc[RefinedMapping[X, O]#Component, RefinedMapping[X, O]#Extract, T])
			:Assoc[RefinedMapping[S, O]#Component, RefinedMapping[S, O]#Extract, T] =
		Assoc[RefinedMapping[S, O]#Component, RefinedMapping[S, O]#Extract, T](entry._1, entry._2 compose f)



	@inline private[oldsql] def composeColumnExtractAssoc[S, X, T, O]
	                            (extractor :MappingExtract[S, X, O])
	                            (entry :Assoc[RefinedMapping[X, O]#Column, RefinedMapping[X, O]#ColumnExtract, T])
			:Assoc[RefinedMapping[S, O]#Column, RefinedMapping[S, O]#ColumnExtract, T] =
		Assoc[RefinedMapping[S, O]#Column, RefinedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose extractor)

	@inline private[oldsql] def composeColumnExtractAssoc[S, X, T, O]
	                            (mapping :MappingAt[O], extractor : S =?> X)
	                            (entry :Assoc[RefinedMapping[X, O]#Column, RefinedMapping[X, O]#ColumnExtract, T])
			:Assoc[RefinedMapping[S, O]#Column, RefinedMapping[S, O]#ColumnExtract, T] =
		Assoc[RefinedMapping[S, O]#Column, RefinedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose extractor)

	@inline private[oldsql] def composeColumnExtractAssoc[S, X, T, O]
	                            (mapping :MappingAt[O], f : S => X)
	                            (entry :Assoc[RefinedMapping[X, O]#Column, RefinedMapping[X, O]#ColumnExtract, T])
			:Assoc[RefinedMapping[S, O]#Column, RefinedMapping[S, O]#ColumnExtract, T] =
		Assoc[RefinedMapping[S, O]#Column, RefinedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose f)




	private[schema] def prefixOption(prefix :String) :Option[String] = prefix.length > 0 ifTrue prefix

}

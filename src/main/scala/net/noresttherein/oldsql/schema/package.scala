package net.noresttherein.oldsql

import net.noresttherein.oldsql.collection.NaturalMap
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Buff.BuffMappingFailureException
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.MappingExtract.ColumnMappingExtract
import net.noresttherein.oldsql.slang._

/**
  * @author Marcin Mościcki
  */
package object schema {

	private[schema] def mapBuffs[S, X](mapping :MappingOf[S])(map :S =?> X) :Seq[Buff[X]] =
		map.requisite match {
			case Some(req) => mapping.buffs.map(_.map(req))
			case _ => mapping.buffs.map(buff => buff.map {
				s => map.get(s) getOrElse {
					throw new BuffMappingFailureException(s"Failed mapping buff $buff of mapping $mapping: no value returned for $s by $map")
				}
			})
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
				case column :ColumnMapping[T @unchecked, O @unchecked] => entry._2 match {
					case extract :ColumnMappingExtract[S @unchecked, T @unchecked, O @unchecked] =>
						Some(Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](column, extract))
					case err =>
						throw new IllegalStateException(s"Extract for column $column of $mapping is not a ColumnExtract: $err.")
				}
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

	@inline def composeExtractAssoc[S, X, T, O](extractor :MappingExtract[S, X, O])
	                                           (entry :Assoc[TypedMapping[X, O]#Component, TypedMapping[X, O]#Extract, T])
			:Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T] =
		Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T](entry._1, entry._2 compose extractor)

	@inline def composeExtractAssoc[S, X, T, O](mapping :MappingFrom[O], f : S => X)
	                                           (entry :Assoc[TypedMapping[X, O]#Component, TypedMapping[X, O]#Extract, T])
			:Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T] =
		Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T](entry._1, entry._2 compose f)


	@inline def composeColumnExtractAssoc[S, X, T, O]
	                                     (extractor :MappingExtract[S, X, O])
	                                     (entry :Assoc[TypedMapping[X, O]#Column, TypedMapping[X, O]#ColumnExtract, T])
			:Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T] =
		Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose extractor)

	@inline def composeColumnExtractAssoc[S, X, T, O]
	                                     (mapping :MappingFrom[O], f : S => X)
	                                     (entry :Assoc[TypedMapping[X, O]#Column, TypedMapping[X, O]#ColumnExtract, T])
	:Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T] =
		Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose f)




	private[schema] def prefixOption(prefix :String) :Option[String] = prefix.length > 0 ifTrue prefix

}

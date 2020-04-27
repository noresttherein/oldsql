package net.noresttherein.oldsql

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Buff.BuffMappingFailureException
import net.noresttherein.oldsql.schema.Mapping.MappingOf
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


	private[schema] def prefixOption(prefix :String) :Option[String] = prefix.length > 0 ifTrue prefix

}

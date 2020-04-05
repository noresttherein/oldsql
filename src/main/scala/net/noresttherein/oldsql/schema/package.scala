package net.noresttherein.oldsql

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Buff.BuffMappingFailureException
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.slang._

/**
  * @author Marcin Mościcki
  */
package object schema {

	private[schema] def mapBuffs[S, X](mapping :TypedMapping[S])(map :S =?> X) :Seq[Buff[X]] =
		map.requisite match {
			case Some(req) => mapping.buffs.map(_.map(req))
			case _ => flatMapBuffs(mapping)(map.optional)
		}

	private[schema] def flatMapBuffs[S, X](mapping :TypedMapping[S])(map :S => Option[X]) :Seq[Buff[X]] =
		mapping.buffs.map { buff => buff.map { s =>
			map(s) getOrElse {
				throw new BuffMappingFailureException(s"Can't flatMap buff $buff of mapping $mapping: no value returned for $s.")
			}
		}}



	private[schema] def prefixOption(prefix :String) :Option[String] = prefix.length > 0 ifTrue prefix

}

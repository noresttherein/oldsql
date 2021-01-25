package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.morsels.ColumnBasedFactory
import net.noresttherein.oldsql.schema.Mapping.{MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter
import net.noresttherein.oldsql.schema.support.MappingProxy.{OpaqueColumnProxy, OpaqueProxy}
import net.noresttherein.oldsql.schema.{cascadeBuffs, Buff, Buffs, ColumnMapping, Mapping}
import net.noresttherein.oldsql.schema.ColumnMapping.{SimpleColumn, StandardColumn}






/** An adapter of a component belonging to a different table than this mapping. The adapted mapping is exposed
  * as property `body`, but has a different origin type to this mapping and must not be used in conjunction
  * with [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] or
  * `Mapping.`[[net.noresttherein.oldsql.schema.Mapping.export export]], as it would result either
  * in a thrown `NoSuchElementException` or read value for columns from a different table.
  * Instead, for every component and column of the adapted mapping, this proxy creates another wrapper,
  * delegating its [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method to the 'covered' component,
  * using additionally aliased `Pieces`.
  * @author Marcin Mościcki
  */
class CoveredMapping[M <: RefinedMapping[S, X], S, X, O]
                    (protected override val backer :M, rename :String => String = identity[String],
                     override val buffs :Seq[Buff[S]] = Nil)
	extends OpaqueProxy[S, O](backer)
{
	//scala can't correctly pick an overload between String and String => String FFS
//	def this(backer :M, columnPrefix :String, buffs :Seq[Buff[S]]) = this(backer, columnPrefix + _, buffs)
//	def this(backer :M, columnPrefix :String) = this(backer, columnPrefix + _)

	val body :M = backer

	protected override def adapt[T](component :backer.Component[T]) :Component[T] =
		new CoveredMapping[RefinedMapping[T, X], T, X, O](component)

	protected override def adapt[T](column :backer.Column[T]) :Column[T] = column match {
		case simple :SimpleColumn[T @unchecked, X @unchecked] =>
			ColumnMapping(rename(column.name), cascadeBuffs(this)(backer(simple)):_*)(simple.form)
		case _ =>
			new OpaqueColumnProxy[T, O](column, rename(column.name), cascadeBuffs(this)(backer(column)))
	}


	def cover[T](component :body.Component[T]) :Component[T] = alias(component)
	def cover[T](column :body.Column[T]) :Column[T] = alias(column)
	def uncover[T](component :Component[T]) :body.Component[T] = dealias(component)
	def uncover[T](column :Column[T]) :body.Column[T] = dealias(column)

	override def mappingName :String = "|" + body.mappingName + "|"

	override def toString :String = "|" + body + "|"
}






object CoveredMapping {

}

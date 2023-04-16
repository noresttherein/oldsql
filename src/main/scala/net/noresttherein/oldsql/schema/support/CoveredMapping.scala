package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.{Buffs, ColumnMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.{SimpleColumn, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.support.MappingProxy.{OpaqueColumnProxy, OpaqueProxy}






/** An adapter of a component belonging to a different table than this mapping. The adapted mapping is exposed
  * as property `body`, but has a different origin type to this mapping and must not be used in conjunction
  * with [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] or
  * `Mapping.`[[net.noresttherein.oldsql.schema.Mapping.export export]], as it would result either
  * in a thrown `NoSuchElementException` or read value for columns from a different table.
  * Instead, for every component and column of the adapted mapping, this proxy creates another wrapper,
  * delegating its [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method to the 'covered' component,
  * using additionally aliased `Pieces`.
  * @author Marcin Mościcki
  */ //consider: renaming to HiddenMapping
class CoveredMapping[M <: TypedMapping[S, X], S, X, O]
                    (protected override val backer :M, rename :String => String = identity[String],
                     override val buffs :Buffs[S] = Buffs.empty[S])
	extends OpaqueProxy[S, O](backer)
{
	//scala can't correctly pick an overload between String and String => String FFS
//	def this(backer :M, columnPrefix :String, buffs :Seq[Buff[S]]) = this(backer, columnPrefix + _, buffs)
//	def this(backer :M, columnPrefix :String) = this(backer, columnPrefix + _)

	val body :M = backer

	protected override def adapt[T](component :backer.Component[T]) :Component[T] =
		new CoveredMapping[TypedMapping[T, X], T, X, O](
			component, rename, buffs.unsafeCascade(backer(component))
		)

	protected override def adapt[T](column :backer.Column[T]) :Column[T] = column match {
		case simple :SimpleColumn[T @unchecked, X @unchecked] =>
			ColumnMapping(rename(column.name), buffs.unsafeCascade(backer(simple)))(simple.form)
		case _ =>
			new OpaqueColumnProxy[T, X, O](column, rename(column.name), buffs.unsafeCascade(backer(column)))
	}


	def cover[T](component :body.Component[T]) :Component[T] = alias(component)
	def cover[T](column :body.Column[T]) :Column[T] = alias(column)
	def uncover[T](component :Component[T]) :body.Component[T] = unexport(component)
	def uncover[T](column :Column[T]) :body.Column[T] = unexport(column)

	override def mappingName :String = "|" + body.mappingName + "|"
	override def toString :String = "|" + body + "|"
}






object CoveredMapping {

}

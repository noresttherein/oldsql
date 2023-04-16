package net.noresttherein.oldsql.schema.support


import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.SpecificExtract
import net.noresttherein.oldsql.schema.Mapping.{MappingTemplate, TypedMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.{SimpleColumn, TypedColumn}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bases.StableMapping.StableMappingTemplate
import net.noresttherein.oldsql.schema.support.MappingAdapter.{ColumnAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter.{ExportColumnAdapter, SimpleColumnAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxyTemplate






/**
  * @author Marcin Mościcki
  */
trait AliasedMapping[S, O] extends BaseMapping[S, O] with StableMappingTemplate[TypedMapping, AliasedColumn] {
	type AliasedExtract[T] = SpecificExtract[AliasedColumn[T, O], S, T, O]
}



object AliasedMapping {

	def apply[S, O](mapping :TypedMapping[S, O], aliases :Map[TypedColumn[_, O], String]) :AliasedMapping[S, O] = {
		val exportAliases =
			if (aliases.keySet.exists(mapping.columns.indexOf(_) < 0)) //non-export (or illegal) columns present
				aliases.map { entry => (mapping.export(entry._1), entry._2) }
			else aliases
		new AliasedMappingAdapter[TypedMapping[S, O], S, O](mapping, exportAliases)
	}

	private class AliasedMappingAdapter[+M <: TypedMapping[S, O], S, O]
	                                   (protected override val backer :M, val aliases :Map[TypedColumn[_, O], String])
		extends DeepProxyTemplate[TypedMapping, AliasedColumn, S, O](backer)
		   with DelegateMapping[M, S, O] with MappingTemplate[TypedMapping, AliasedColumn]
		   with DelegateAdapter[M, S, O] with MappingDecorator[M, S, O] with AliasedMapping[S, O]
		   with StableMappingTemplate[TypedMapping, AliasedColumn]
	{
		if (aliases.keySet.exists(body.columns.indexOf(_) < 0))
			throw new IllegalArgumentException(
				"Columns " + aliases.filter(entry => body.columns.indexOf(entry._1) < 0) +
				" are not (export) columns of mapping " + body + "."
			)
		//not much purpose in validating uniqueness, as they very well may not be unique in the whole select
//		aliases.groupMapReduce(_._2)(_._1::Nil)(_ reverse_::: _).filter(_._2.sizeIs > 1) match {
//			case map if map.nonEmpty =>
//				throw new IllegalArgumentException("Non-unique aliases for columns of " + body + ": " + map + ".")
//			case _ =>
//		}

		protected override def adapt[T](component :backer.Component[T]) :Component[T] = component

		protected override def adapt[T](column :backer.Column[T]) :AliasedColumn[T, O] =
			AliasedColumn(column, aliases.get(column))
	}

}




trait AliasedColumn[S, O] extends ColumnAdapter[TypedColumn[S, O], S, O] {
	def alias :String = aliasOpt getOrElse name
	def aliasOpt :Option[String]
}


object AliasedColumn {
	def apply[S, O](column :TypedColumn[S, O], alias :String) :AliasedColumn[S, O] =
		AliasedColumn(column, Option(alias))

	def apply[S, O](column :TypedColumn[S, O], alias :Option[String]) :AliasedColumn[S, O] =
		column match {
			case simple :SimpleColumn[S @unchecked, O @unchecked] => AliasedColumn(simple, alias)
			case _ =>
				val asOpt = alias
				new ExportColumnAdapter[TypedColumn[S, O], S, O](column, column.name, column.buffs)(column.form)
					with AliasedColumn[S, O]
				{
					override val aliasOpt = asOpt
				}
		}

	def apply[S, O](column :SimpleColumn[S, O], alias :String) :AliasedColumn[S, O] =
		AliasedColumn(column, Option(alias))

	def apply[S, O](column :SimpleColumn[S, O], alias :Option[String]) :AliasedColumn[S, O] = {
		val asOpt = alias
		new SimpleColumnAdapter[SimpleColumn[S, O], S, O](column) with AliasedColumn[S, O] {
			override val aliasOpt = asOpt
		}
	}


	def unapply[S, O](column :TypedColumn[S, O]) :Opt[(TypedColumn[S, O], String)] = column match {
		case aliased :AliasedColumn[S, O] @unchecked if aliased.aliasOpt.isDefined =>
			Got((aliased.body, aliased.alias))
		case _ => Lack
	}

}

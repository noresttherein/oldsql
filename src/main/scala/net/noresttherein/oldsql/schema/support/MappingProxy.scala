package net.noresttherein.oldsql.schema.support

import scala.collection.mutable

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.{Buff, ColumnExtract, ColumnMapping, ComponentValues, GenericExtract, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoUpdate, NoUpdateByDefault}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.schema.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema.bases.LazyMapping




/** A `DelegateMapping` subtype which preserves the subject type of the adapted mapping.
  * @see [[net.noresttherein.oldsql.schema.support.DelegateMapping]]
  */ //consider: require RefinedMapping[S, O] - is there any circumstance when we can't project?
trait MappingProxy[S, O] extends DelegateMapping[MappingOf[S], S, O] {

	override def buffs :Seq[Buff[S]] = backer.buffs

	override def nullValue :NullValue[S] = backer.nullValue

}






object MappingProxy {

	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is. */
	trait ShallowProxy[S, O]
		extends MappingProxy[S, O] with ShallowDelegate[S, O] with DelegateMapping[RefinedMapping[S, O], S, O]
	{
		override def assemble(pieces :Pieces) :Option[S] = backer.optionally(pieces)


		override def apply[T](component :Component[T]) :Extract[T] =
			(if ((component eq backer) | (component eq this))
				 MappingExtract.ident(component)
			 else
				 backer(component)
			).asInstanceOf[Extract[T]]

		override def apply[T](column :Column[T]) :ColumnExtract[T] =
			(if (column eq backer)
				 ColumnExtract.ident(column)
			 else
				 backer(column)
			).asInstanceOf[ColumnExtract[T]]



		override def extracts :NaturalMap[Component, Extract] =
			backer.extracts.updated[Extract, S](backer, MappingExtract.ident(backer))

		override def columnExtracts :NaturalMap[Column, ColumnExtract] = backer match {
			case column :ColumnMapping[S @unchecked, O @unchecked] =>
				NaturalMap.single[Column, ColumnExtract, S](column, ColumnExtract.ident(column))
			case _ =>
				backer.columnExtracts
		}



		override def toString :String = "->" + backer
	}



	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is.
	  * It forwards all methods directly to the backing mapping for efficiency, in turn requiring that no buffs
	  * are applied to this instance (the buffs on the backing mapping will function as normal).
	  */
	trait DirectProxy[S, O] extends ShallowProxy[S, O] {

		override def assemble(pieces :Pieces) :Option[S] = backer.assemble(pieces)

		override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this)


		override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] =
			backer.writtenValues(op, subject)

		override def filterValues(subject :S) :ComponentValues[S, O] = backer.filterValues(subject)
		override def updateValues(subject :S) :ComponentValues[S, O] = backer.updateValues(subject)
		override def insertValues(subject :S) :ComponentValues[S, O] = backer.insertValues(subject)

		override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			backer.writtenValues(op, subject, collector)

		override def filterValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			backer.filterValues(subject, collector)

		override def updateValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			backer.updateValues(subject, collector)

		override def insertValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			backer.insertValues(subject, collector)

		/** Shallow proxies have no buffs, relying instead on the buffs of the backing mapping, as they delegate
		  * all methods to the corresponding methods of the backing mapping. Changing buffs would potentially
		  * require overriding all methods declared here and, in particular, change the nature of components
		  * were they to inherit those buffs.
		  */
		final override def buffs :Seq[Buff[S]] = Nil



		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.contains(backer)) backer.selectForm(backer.selectable) //fixme: default select list unless top level buff
			else backer.selectForm(components)

		override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer)) backer.filterForm(backer.filterable)
			else backer.filterForm(components)

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer)) backer.updateForm(backer.updatable)
			else backer.updateForm(components)

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer)) backer.insertForm(backer.insertable)
			else backer.insertForm(components)

		override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer)) backer.writeForm(op, op.columns(backer))
			else backer.writeForm(op, components)

		override def selectForm :SQLReadForm[S] = backer.selectForm
		override def filterForm :SQLWriteForm[S] = backer.filterForm
		override def updateForm :SQLWriteForm[S] = backer.updateForm
		override def insertForm :SQLWriteForm[S] = backer.insertForm
		override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = backer.writeForm(op)

	}






	/** A skeleton trait for a mapping proxy which needs to adapt every component of the proxied mapping.
	  * The `components`, `subcomponents` and all `columns` lists of this mapping are the corresponding lists
	  * from the adapted mapping, mapped with the `adapt` methods, meaning the components of the proxied mapping
	  * are not in themselves the components of this mapping. `Extract` maps however include both the adapted
	  * and original components as keys.
	  */ //todo: remove this trait, leave only EagerDeepProxy
	trait DeepProxy[S, O] extends MappingProxy[S, O] {

		override def assemble(pieces :Pieces) :Option[S] = //use backer.assemble to bypass buffs on the backer
			backer.assemble(pieces.asInstanceOf[backer.Pieces]) //and use only those on the proxy



		override def apply[T](component :Component[T]) :Extract[T] =
			if (component.isInstanceOf[ColumnMapping[_, _]])
				apply(component.asInstanceOf[Column[T]])
			else if (component eq backer)
				MappingExtract.ident(adaptBacker).asInstanceOf[Extract[T]]
			else
				MappingExtract[S, T, O](export(component))(backer.apply(dealias(component)))

		override def apply[T](column :Column[T]) :ColumnExtract[T] =
			if (column eq backer)
				ColumnExtract.ident(adaptBacker.asInstanceOf[Column[S]]).asInstanceOf[ColumnExtract[T]]
			else
				ColumnExtract[S, T, O](export(column))(backer.apply(dealias(column)))



		override def export[T](component :Component[T]) :Component[T] =
			if (component.isInstanceOf[ColumnMapping[_, _]])
				export(component.asInstanceOf[Column[T]])
			else if (component eq backer)
				adaptBacker.asInstanceOf[Component[T]]
			else
				adapt(backer.export(dealias(component)))

		override def export[T](column :Column[T]) :Column[T] =
			if (column eq backer)
				adaptBacker.asInstanceOf[Column[T]]
			else
				adapt(backer.export(dealias(column)))



		override def extracts :NaturalMap[Component, Extract] = {
			def mapEntry[X](entry :Assoc[backer.Component, backer.Extract, X]) = {
				val export = alias(entry._1)
				val extract = MappingExtract(export)(entry._2)
				Assoc[Component, Extract, X](entry._1.asInstanceOf[Component[X]], extract)::
					Assoc[Component, Extract, X](export, extract)::Nil
			}
			backer.extracts.flatMap(mapEntry(_))
				.updated[Extract, S](backer.asInstanceOf[Component[S]], MappingExtract.ident(adaptBacker))
		}

		override def columnExtracts :NaturalMap[Column, ColumnExtract] = {
			def mapEntry[X](entry :Assoc[backer.Column, backer.ColumnExtract, X]) = {
				val export = alias(entry._1)
				val extract = ColumnExtract(export)(entry._2)
				Assoc[Column, ColumnExtract, X](entry._1.asInstanceOf[Column[X]], extract)::
					Assoc[Column, ColumnExtract, X](export, extract)::Nil
			}
			backer.columnExtracts.flatMap(mapEntry(_))
		}



		private[this] def alias[T](component :backer.Component[T]) :Component[T] = component match {
			case _ if component eq backer => adaptBacker.asInstanceOf[Component[T]]
			case column :ColumnMapping[_, _] =>
				adapt(backer.export(column.asInstanceOf[backer.Column[T]]))
			case _ =>
				adapt(backer.export(component))
		}

		private[this] def alias[T](column :backer.Column[T]) :Column[T] = adapt(backer.export(column))



		/** A hook method left for subclasses to implement adapting of a component of the adapted mapping
		  * to its 'export' form under this mapping. All component lists of this instance contain
		  * components of `backer` (and `backer` itself) mapped with this method.
		  * @param component an 'export', non-column component of `backer`.
		  */
		protected def adapt[T](component :backer.Component[T]) :Component[T]

		/** A hook method left for subclasses to implement adapting of a column of the adapted mapping
		  * to its 'export' form under this mapping. All column lists of this instance contain
		  * columns mapped with this method.
		  * @param column an 'export' column of `backer`.
		  */
		protected def adapt[T](column :backer.Column[T]) :Column[T] //= adapt(component)

		/** The 'export' version of the adapted `backer` itself. Defaults to `this`. */
		protected def adaptBacker :Component[S] = this



		/** A hook method left for subclasses to implement the mapping of the adapted components back to their
		  * originating components of the adapted mapping. Used in implementing `apply(component)` returning
		  * the extract for the component.
		  * @param component a subcomponent of this instance; may be a component adapted from `backer`
		  *                  or a (sub)component of `backer` unchanged, or even `backer` itself.
		  * @return the 'export' version of the given component as exposed by the backing mapping.
		  */
		protected def dealias[T](component :Component[T]) :backer.Component[T]

		/** A hook method left for subclasses to implement the mapping of the adapted columns back to their
		  * originating columns of the adapted mapping. Used in implementing `apply(column)` returning
		  * the extract for the component.
		  * @param column a column of this instance; may be a column adapted from `backer` or a (sub)column
		  *               of `backer` unchanged.
		  * @return the 'export' version of the given column as exposed by the backing mapping.
		  */
		protected def dealias[T](column :Column[T]) :backer.Column[T]



		override def components :Unique[Component[_]] = backer.components.map(alias(_))
		override def subcomponents :Unique[Component[_]] = backer.subcomponents.map(alias(_))

		override def columns :Unique[Column[_]] = backer.columns.map(alias(_))
		override def selectable :Unique[Column[_]] = backer.selectable.map(alias(_))
		override def filterable :Unique[Column[_]] = backer.filterable.map(alias(_))
		override def updatable :Unique[Column[_]] = backer.updatable.map(alias(_))
		override def autoUpdated :Unique[Column[_]] = backer.autoUpdated.map(alias(_))
		override def insertable :Unique[Column[_]] = backer.insertable.map(alias(_))
		override def autoInserted :Unique[Column[_]] = backer.autoInserted.map(alias(_))
		override def selectedByDefault :Unique[Column[_]] = backer.selectedByDefault.map(alias(_))
		override def filteredByDefault :Unique[Column[_]] = backer.filteredByDefault.map(alias(_))
		override def updatedByDefault :Unique[Column[_]] = backer.updatedByDefault.map(alias(_))
		override def insertedByDefault :Unique[Column[_]] = backer.insertedByDefault.map(alias(_))


		override def toString :String = "->>" + backer

	}






	//todo: look into removing Origin and Subject parameters
	/** A `DeepProxy` implementation which eagerly initializes all column and component lists and creates
	  * a fixed mapping between components of the adapted mapping and their adapted counterparts as well as the
	  * reverse. The components of the adapted mapping are not part of any component or column lists of this mapping,
	  * which are formed by adapting each element of the corresponding list from `backer` with `adapt` methods,
	  * left for subclasses to implement. `Extract` maps however include as keys all the keys from the corresponding
	  * extract map of the adapted mapping and the adapted components.
	  */
	abstract class EagerDeepProxy[S, O] private
		                         (protected override val backer :MappingOf[S],
		                          exports :mutable.Map[Mapping, MappingExtract[S, _, O]], //both start empty, are used
		                          originals :mutable.Map[MappingAt[O], Mapping])          //only as tmp in the constructor
		extends DeepProxy[S, O] with LazyMapping[S, O] //inherit the optimized optionally/writtenValues
	{
		//a private constructor with mutable maps ensures that extract entries can be created as method side effects,
		//without the maps becoming instance fields - they are used only in initialization of the 'extracts' properties
		def this(backer :MappingOf[S]) = this(backer, mutable.Map(), mutable.Map())


		preInit()

		private[this] val adaptedBacker = adaptBacker
		exports.put(backer, MappingExtract.ident(adaptedBacker))
		originals.put(adaptedBacker, backer)

		for (Assoc(comp, base) <- backer.extracts) {
			val export = adaptExport(base.export)
			val extract = exports.getOrElse(base.export, MappingExtract(export)(base))
			exports.put(comp, extract)
			originals.put(export, base.export)
		}

		override val columns :Unique[Column[_]] = backer.columns.map(alias(exports, originals, _))
		override val selectable :Unique[Column[_]] = columnsWithout(NoSelect)
		override val filterable :Unique[Column[_]] = columnsWithout(NoFilter)
		override val updatable :Unique[Column[_]] = columnsWithout(NoUpdate)
		override val insertable :Unique[Column[_]] = columnsWithout(NoInsert)
		override val autoUpdated :Unique[Column[_]] = columnsWith(AutoUpdate)
		override val autoInserted :Unique[Column[_]] = columnsWith(AutoInsert)
		override val selectedByDefault :Unique[Column[_]] = columnsWithout(NoInsertByDefault)
		override val filteredByDefault :Unique[Column[_]] = columnsWithout(NoFilterByDefault)
		override val updatedByDefault :Unique[Column[_]] = columnsWithout(NoUpdateByDefault)
		override val insertedByDefault :Unique[Column[_]] = columnsWithout(NoInsertByDefault)

		override val components :Unique[Component[_]] = backer.components.map(alias(exports, originals, _))
		override val subcomponents :Unique[Component[_]] = backer.subcomponents.map(alias(exports, originals, _))

		override val extracts = NaturalMap[Component, Extract](
			exports.map { case (comp, extract) =>
				Assoc[Component, Extract, Any](comp.asInstanceOf[Component[Any]], extract.asInstanceOf[Extract[Any]])
			}.toSeq  :_*
		)

		override val columnExtracts = NaturalMap[Column, ColumnExtract](
			exports.flatMap {
				case (column :ColumnMapping[Any @unchecked, O @unchecked],
				      extract :GenericExtract[ColumnMapping[Any, O] @unchecked, S @unchecked, Any @unchecked, O @unchecked]) =>
					Some(Assoc[Column, ColumnExtract, Any](column, extract))
				case _ =>
					None
			}.toSeq :_*
		)
//
//		oldsql.publishMutable()



		private[this] def adaptExport[T](component :backer.Component[T]) = component match {
			case _ if component eq backer => adaptedBacker.asInstanceOf[Component[T]]
			case column :ColumnMapping[_, _] => adapt(column.asInstanceOf[backer.Column[T]])
			case _ => adapt(component)
		}

		private[this] def alias[T](exports :mutable.Map[Mapping, Extract[_]],
		                           originals :mutable.Map[MappingAt[O], Mapping],
		                           component :backer.Component[T]) :Component[T] =
			exports.getOrElse(component, {
				val base = backer.apply(component)
				val extract = exports.getOrElse(base.export, MappingExtract(adaptExport(base.export))(base))
				exports.put(component, extract)
				exports.put(base.export, extract)
				originals.put(extract.export, base.export)
				extract
			}).export.asInstanceOf[Component[T]]

		private[this] def alias[T](exports :mutable.Map[Mapping, Extract[_]],
		                           originals :mutable.Map[MappingAt[O], Mapping],
		                           column :backer.Column[T]) :Column[T] =
			alias(exports, originals, column :backer.Component[T]).asInstanceOf[Column[T]]


		override def apply[T](component :Component[T]) :Extract[T] = extracts(component)
		override def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)

		//override DeepProxy specialized implementations back to defaults
		override def export[T](component :Component[T]) :Component[T] = extracts(component).export
		override def export[T](column :Column[T]) :Column[T] = columnExtracts(column).export



		/** Method called from the `EagerDeepProxy` constructor before any component lists are initialized. */
		protected def preInit(): Unit = ()

		/** Maps a component of this proxy back to its corresponding export component of `backer`. */
		protected override def dealias[T](component :Component[T]) :backer.Component[T] =
			originals.getOrElse(component, component).asInstanceOf[backer.Component[T]]

		/** Maps a column of this proxy back to its corresponding export column of `backer`. */
		protected override def dealias[T](column :Column[T]) :backer.Column[T] =
			originals.getOrElse(column, column).asInstanceOf[backer.Column[T]]

	}



}


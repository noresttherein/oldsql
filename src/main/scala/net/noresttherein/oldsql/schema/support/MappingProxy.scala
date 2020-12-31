package net.noresttherein.oldsql.schema.support

import scala.collection.mutable

import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.generic.=#>
import net.noresttherein.oldsql.schema.{filterColumnExtracts, Buff, ColumnExtract, ColumnForm, ColumnMapping, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnMapping.StableColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bases.{ExportMapping, StableMapping}
import net.noresttherein.oldsql.schema.support.DelegateMapping.{ShallowDelegate, WrapperDelegate}




/** A `DelegateMapping` subtype which preserves the subject type of the adapted mapping.
  * Proxy classes can be divided into two broad groups: 'shallow' proxies, which use the components of the adapted mapping
  * as their own, without changes, and 'deep' proxies, which create an adapter for some or all components of `backer`.
  * The first group is generally restricted to adapters which do not change the effective buffs, at least those
  * used during assembly, so that there are no changes in their cascading. These classes derive from
  * either [[net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy ShallowProxy]] (treating `backer`
  * as its own ''export'' version) and [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportProxy ExportProxy]]
  * (treating itself as the ''export'' version of `backer`). The second group, for the most part,
  * is derived from [[net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy DeepProxy]].
  * @see [[net.noresttherein.oldsql.schema.support.DelegateMapping]]
  */
trait MappingProxy[S, O] extends DelegateMapping[MappingOf[S], S, O] {

	override def buffs :Seq[Buff[S]] = backer.buffs

	override def nullValue :NullValue[S] = backer.nullValue
}






object MappingProxy {

	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is, treating
	  * the `backer` mapping as just another component, being its own ''export'' version. For efficiency,
	  * it is excluded from all component and column list, but subclasses are free to change this. It is however
	  * still included among the `extracts`, mapped to itself, and a value preset for it in
	  * [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]] will be picked up as the value for this mapping
	  * (assuming there is none preset for this mapping). Select/assembly related buffs on the adapted mapping
	  * are ignored by bypassing its [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
	  * However, unless overriden, the buffs on this instance are exactly the buffs of the adapted mapping.
	  * In the result, the buffs are processed exactly once for the assembled value.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportProxy]]
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.WrapperProxy]]
	  */
	trait ShallowProxy[S, O]
		extends MappingProxy[S, O] with ShallowDelegate[S, O] with DelegateMapping[RefinedMapping[S, O], S, O]
	{
		override def assemble(pieces :Pieces) :Option[S] = {
			val res = pieces.preset(backer)
			if (res.isDefined) res else backer.assemble(pieces)
		}

		override def apply[T](component :Component[T]) :Extract[T] =
			(if (component eq backer)
				 MappingExtract.ident(backer)
			 else
				 backer(component)
			).asInstanceOf[Extract[T]]

		override def apply[T](column :Column[T]) :ColumnExtract[T] =
			(if (column eq backer)
				 MappingExtract.ident(backer)
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



	/** A skeleton of a mapping proxy which has its `backer` mapping as its only direct component
	  * and its components as the subcomponents. All (sub)components of this mapping, including `backer`, are their own
	  * ''export'' versions. It differs from its `ShallowProxy` base trait in that `backer` is a full fledged, public
	  * component, included in the components and subcomponents (and potentially column) lists and doesn't ignore
	  * its buffs, normally (through [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]]) calling
	  * its [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportProxy]]
	  */
	trait WrapperProxy[S, O] extends ShallowProxy[S, O] with WrapperDelegate[S, O] {
		override def assemble(pieces :Pieces) :Option[S] = pieces.get(backer)
	}



	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is.
	  * During assembly, it checks [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]] for preset values
	  * for this instance and, if none is available, it initiates full assembly process of  `backer`,
	  * as if it was the only component of this mapping, from checking for a preset value, through assembly and
	  * ending with handling its buffs. All other methods are forwarded directly to the backing mapping for efficiency,
	  * in turn requiring that no buffs are applied to this instance (the buffs on the backing mapping will function
	  * as normal). In addition to the component-related methods inherited from `ExportMapping`, it overrides
	  * also form- and `ComponentValues`-creating methods, likewise delegating them to `backer`.
	  */
	trait DirectProxy[S, O] extends ShallowProxy[S, O] {

		override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this)
		override def assemble(pieces :Pieces) :Option[S] = pieces.get(backer)

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

		/** Direct proxies have no buffs, relying instead on the buffs of the backing mapping, as they delegate
		  * all methods to the corresponding methods of the backing mapping. Changing buffs would potentially
		  * require overriding all methods declared here and, in particular, change the nature of components
		  * were they to inherit those buffs.
		  */
		final override def buffs :Seq[Buff[S]] = Nil


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.contains(backer))
				backer.selectForm(backer.selectedByDefault :++ components.view.filter(_ != backer))
			else backer.selectForm(components)

		override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer))
				backer.filterForm(backer.filteredByDefault :++ components.view.filter(_ != backer))
			else backer.filterForm(components)

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer))
				backer.updateForm(backer.updatedByDefault :++ components.view.filter(_ != backer))
			else backer.updateForm(components)

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer))
				backer.insertForm(backer.insertedByDefault :++ components.view.filter(_ != backer))
			else backer.insertForm(components)

		override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer))
				backer.writeForm(op, op.defaultColumns(backer) :++ components.view.filter(_ != backer))
			else backer.writeForm(op, components)

		override def selectForm :SQLReadForm[S] = backer.selectForm
		override def filterForm :SQLWriteForm[S] = backer.filterForm
		override def updateForm :SQLWriteForm[S] = backer.updateForm
		override def insertForm :SQLWriteForm[S] = backer.insertForm
		override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = backer.writeForm(op)

	}



	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is, but considers itself
	  * its ''export'' version. It should only be used for non column mappings. Otherwise it behaves similarly
	  * [[net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy ShallowProxy]], in particular
	  * it delegates its [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method to `backer`,
	  * ignoring its [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
	  * However, unless overriden, the buffs on this instance are exactly the buffs of the adapted mapping.
	  * In the result, the buffs are processed exactly once for the assembled value.
	  */
	trait ExportProxy[S, O]
		extends MappingProxy[S, O] with ShallowDelegate[S, O] with DelegateMapping[RefinedMapping[S, O], S, O]
	{
		override def assemble(pieces :Pieces) :Option[S] = backer.assemble(pieces)

		override def export[T](component :Component[T]) :Component[T] =
			if ((component eq backer) | (component eq this)) this.asInstanceOf[Component[T]]
			else backer.export(component)

		override def exportOrNot[T](component :Component[T]) :Component[T] =
			if ((component eq backer) | (component eq this)) this.asInstanceOf[Component[T]]
			else backer.exportOrNot(component)

//		override def export[T](column :Column[T]) :Column[T] = backer.export(column)
//		override def exportOrNot[T](column :Column[T]) :Column[T] = backer.exportOrNot(column)


		override def apply[T](component :Component[T]) :Extract[T] =
			(if (component eq backer)
				MappingExtract.ident(this)
			else
				backer(component)
			).asInstanceOf[Extract[T]]

		override def apply[T](column :Column[T]) :ColumnExtract[T] = backer(column)


		override def extracts :NaturalMap[Component, Extract] =
			backer.extracts.updated[Extract, S](backer, MappingExtract.ident(this))

		override def columnExtracts :NaturalMap[Column, ColumnExtract] = backer.columnExtracts


		override def toString = "->"
	}






	@deprecated("This class is not a part of the public API. It can disappear without notice.", "always")
	abstract class AbstractDeepProxy[S, O] private[MappingProxy]
	                                (protected override val backer :MappingOf[S],
		                            /** Temporary (non-field) constructor parameter which becomes `extracts` property
		                              * in the mapping process. *///both start empty
		                             adapters :mutable.Map[Mapping, MappingExtract[S, _, O]])
		extends MappingProxy[S, O]
	{
		/** Method called from the `DeepProxy` constructor before any component lists are initialized. */
		protected def preInit(): Unit = ()

		preInit()

		/** Mapping from (''export'') components of this instance back to their `export`
		  * components of `backer`; every entry exists in the reversed form in `extracts`. */
		private[this] val originals = mutable.Map.empty[MappingAt[O], Mapping] //mutable, but I'm lazy. Sue me.

		private[this] val adaptedBacker = adaptBacker
		adapters.put(backer, MappingExtract.ident(adaptedBacker))
		originals.put(adaptedBacker, backer)

		for (Assoc(comp, base) <- backer.extracts) {
			val export = adaptExport(base.export)
			val extract = adapters.get(base.export) match {
				case Some(ex) => ex
				case _ =>
					val ex = MappingExtract(export)(base)
					adapters.put(base.export, ex)         //unnecessary, but prevents duplicate extract instances
					originals.put(export, base.export)
					ex
			}
			adapters.put(comp, extract)
		}

		override val columns :Unique[Column[_]] = backer.columns.map(alias(adapters, originals, _))
		override val components :Unique[Component[_]] = backer.components.map(alias(adapters, originals, _))
		override val subcomponents :Unique[Component[_]] = backer.subcomponents.map(alias(adapters, originals, _))
		override val extracts :ExtractMap = initExtractMap
		override val columnExtracts :ColumnExtractMap = filterColumnExtracts(this)(extracts)

		protected[MappingProxy] def initExtractMap :ExtractMap

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


		/** Maps a component of this proxy back to its corresponding export component of `backer`. */
		protected def dealias[T](component :Component[T]) :backer.Component[T] =
			originals.getOrElse(component, component).asInstanceOf[backer.Component[T]]

		/** Maps a column of this proxy back to its corresponding export column of `backer`. */
		protected def dealias[T](column :Column[T]) :backer.Column[T] =
			originals.getOrElse(column, column).asInstanceOf[backer.Column[T]]

		private[this] def adaptExport[T](component :backer.Component[T]) = component match {
			case _ if component eq backer => adaptedBacker.asInstanceOf[Component[T]]
			case column :ColumnMapping[_, _] => adapt(column.asInstanceOf[backer.Column[T]])
			case _ => adapt(component)
		}

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

		override def toString :String = "->>" + backer
	}



	/** A `DeepProxy` implementation which eagerly initializes all column and component lists and creates
	  * a fixed mapping between components of the adapted mapping and their adapted counterparts, as well as the
	  * reverse. The components of the adapted mapping are not part of any component or column lists of this mapping,
	  * which are formed by adapting each element of the corresponding list from `backer` with `adapt` methods,
	  * left for subclasses to implement. They are still recognized by this mapping as its subcomponents though,
	  * and `Extract` maps include as keys all the keys from the corresponding extract map of the adapted mapping
	  * together with the adapted components.
	  */
	abstract class DeepProxy[S, O] private (protected override val backer :MappingOf[S],
	                                        exports :mutable.Map[Mapping, MappingExtract[S, _, O]])
		extends AbstractDeepProxy[S, O](backer, exports) with StableMapping
	{
		//a private constructor with a mutable map ensures that extract entries can be created as method side effects,
		//without the map becoming an instance field - it is used only in initialization of the 'extracts' properties
		def this(backer :MappingOf[S]) = this(backer, mutable.Map.empty)

		override def assemble(pieces :Pieces) :Option[S] = backer.assemble(pieces.asInstanceOf[backer.Pieces])


		protected[MappingProxy] override def initExtractMap :ExtractMap = {
			exports.put(this, MappingExtract.ident(this))
			NaturalMap[Component, Extract](
				exports.view.map { case (comp, extract) =>
					Assoc[Component, Extract, Any](comp.asInstanceOf[Component[Any]], extract.asInstanceOf[Extract[Any]])
				}.toSeq  :_*
			)
		}

		override def apply[T](component :Component[T]) :Extract[T] = extracts(component)
		override def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)

		override def export[T](component :Component[T]) :Component[T] = extracts(component).export
		override def export[T](column :Column[T]) :Column[T] = columnExtracts(column).export

	}






	/** A mapping proxy which does not expose its `backer` mapping or its components, even in its `extracts`.
	  * Similar to [[net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy DeepProxy]],
	  * but the mapping from `backer` components to their adapters in this class's components is done by a separate
	  * `Map`, which is used to perform an extra level of aliasing of
	  * [[ComponentValues ComponentValues]] before passing them to `backer` for assembly.
	  * This is to protected against `backer` instance being used as another component of of the same root mapping,
	  * in particular in foreign key scenarios.
	  */
	abstract class OpaqueProxy[S, O](protected override val backer :MappingOf[S],
	                                 substitutes :mutable.Map[Mapping, MappingExtract[S, _, O]])
		extends AbstractDeepProxy[S, O](backer, substitutes) with StableMapping with ExportMapping
	{
		//a private constructor with a mutable map ensures that extract entries can be created as method side effects,
		//without the map becoming an instance field - it is used only in initialization of the 'extracts' properties
		def this(backer :MappingOf[S]) = this(backer, mutable.Map.empty)

		override def assemble(pieces :Pieces) :Option[S] = //composes aliasing to substitute components of component to this
			backer.asInstanceOf[Component[S]].assemble(pieces.selectivelyAliased(aliases))


		private[this] val aliases = NaturalMap[Component, Extract](
			substitutes.view.map { case (comp, extract) =>
				Assoc(comp.asInstanceOf[Component[Any]], extract.asInstanceOf[Extract[Any]])
			}.toSeq  :_*
		)

		protected def alias[T](component :backer.Component[T]) :Component[T] =
			aliases(component.asInstanceOf[Component[T]]).export

		protected def alias[T](column :backer.Column[T]) :Column[T] =
			aliases(column.asInstanceOf[Column[T]]).export.asInstanceOf[Column[T]]

		protected[MappingProxy] override def initExtractMap :ExtractMap = NaturalMap[Component, Extract](
			substitutes.view.map { entry => //will contain extract for this because substitutions contains backer->this
				def export[T](extract :MappingExtract[S, T, O]) =
					Assoc[Component, Extract, T](extract.export, extract)
				export(entry._2)
			}.toList : _*
		)

		override def apply[T](component :Component[T]) :Extract[T] = extracts(component)
		override def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)
	}



	class OpaqueColumnProxy[S, O] private (protected override val backer :ColumnMapping[S, _], 
	                                       override val name :String, override val buffs :Seq[Buff[S]],
	                                       override val form :ColumnForm[S])
		extends MappingProxy[S, O] with ColumnMapping[S, O] with StableColumn[S, O]
	{
		def this(backer :ColumnMapping[S, _]) = this(backer, backer.name, backer.buffs, backer.form)
		def this(backer :ColumnMapping[S, _], name :String) = this(backer, name, backer.buffs, backer.form)
		def this(backer :ColumnMapping[S, _], buffs :Seq[Buff[S]]) = this(backer, backer.name, buffs, backer.form)
		def this(backer :ColumnMapping[S, _], name :String, buffs :Seq[Buff[S]]) = this(backer, name, buffs, backer.form)
		
		private[this] val aliasing = new (MappingAt[O]#Component =#> MappingAt[O]#Component) {
			override def apply[T](x :Component[T]) =
				if (x eq backer) this.asInstanceOf[Column[T]] else x
		}
		
		override def assemble(pieces :Pieces) :Option[S] = 
			backer.asInstanceOf[Column[S]].assemble(pieces.aliased(aliasing))
	}



	class DirectColumnProxy[S, O] private (protected override val backer :ColumnMapping[S, O], 
	                                       override val name :String, override val form :ColumnForm[S])
		extends MappingProxy[S, O] with ColumnMapping[S, O] with StableColumn[S, O]
	{
		def this(backer :ColumnMapping[S, O]) = this(backer, backer.name, backer.form)
		def this(backer :ColumnMapping[S, O], name :String) = this(backer, name, backer.form)

		override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this)
		override def assemble(pieces :Pieces) :Option[S] = pieces.get(backer)

		override def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)
		override def apply[T](component :Component[T]) :Extract[T] = extracts(component)


		override def export[T](component :Component[T]) :Component[T] =
			if ((component eq this) | (component eq backer)) component
			else backer.export(component)

		override def exportOrNot[T](component :Component[T]) :Component[T] =
			if ((component eq this) | (component eq backer)) component
			else backer.exportOrNot(component)

		override def export[T](column :Column[T]) :Column[T] =
			if ((column eq this) | (column eq backer)) column
			else backer.export(column)

		override def exportOrNot[T](column :Column[T]) :Column[T] =
			if ((column eq this) | (column eq backer)) column
			else backer.exportOrNot(column)

		override val columnExtracts :NaturalMap[Column, ColumnExtract] =
			backer.columnExtracts.updated[ColumnExtract, S](backer, ColumnExtract.ident(backer))
				.updated(this, super.apply(this))

		//this can fail with ClassCastException for custom NaturalMap implementations
		override val extracts :NaturalMap[Component, ColumnExtract] =
			columnExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.isEmpty)  SQLReadForm.empty
			else if (components.contains(this)) selectForm
			else backer.selectForm(components)

		override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.isEmpty) SQLWriteForm.empty
			else if (components.contains(this)) form
			else backer.writeForm(op, components)

		final override def buffs :Seq[Buff[S]] = Nil
	}



	class ExportColumnProxy[S, O] private (protected override val backer :ColumnMapping[S, O],
	                                       override val name :String, override val buffs :Seq[Buff[S]],
	                                       override val form :ColumnForm[S])
		extends MappingProxy[S, O] with ColumnMapping[S, O] with StableColumn[S, O]
	{
		def this(backer :ColumnMapping[S, O]) = this(backer, backer.name, backer.buffs, backer.form)
		def this(backer :ColumnMapping[S, O], name :String) = this(backer, name, backer.buffs, backer.form)
		def this(backer :ColumnMapping[S, O], buffs :Seq[Buff[S]]) = this(backer, backer.name, buffs, backer.form)
		def this(backer :ColumnMapping[S, O], name :String, buffs :Seq[Buff[S]]) =
			this(backer, name, buffs, backer.form)

		override def assemble(pieces :Pieces) :Option[S] = backer.assemble(pieces)

		override def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)
		override def apply[T](component :Component[T]) :Extract[T] = extracts(component)

		override val columnExtracts =
			backer.columnExtracts.map { entry =>
				if (entry._2.export == backer)
					Assoc[Column, ColumnExtract, S](
						entry._1.asInstanceOf[Column[S]], ColumnExtract(this)(entry._2.asInstanceOf[ColumnExtract[S]])
					)
				else
					entry
			}.updated[ColumnExtract, S](backer, super[StableColumn].apply(this))
			 .updated[ColumnExtract, S](this, super[StableColumn].apply(this))

		//this can fail with ClassCastException for custom NaturalMap implementations
		override val extracts = columnExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.isEmpty) SQLReadForm.empty
			else if (components.contains(this) || components.contains(backer)) form
			else backer.selectForm(components)

		override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.isEmpty) SQLWriteForm.empty
			else if (components.contains(this)) form
			else backer.writeForm(op, components)
	}
	
	
}


package net.noresttherein.oldsql.schema.support

import scala.collection.mutable

import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.generic.=#>
import net.noresttherein.oldsql.schema.{filterColumnExtracts, Buff, Buffs, ColumnExtract, ColumnForm, ColumnMapping, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
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

	override def nullValue :NullValue[S] = backer.nullValue

	override def buffs :Buffs[S] = backer.buffs

}






object MappingProxy { //todo: revise writtenValues methods to be consistent with select regarding buff handling

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
		override def assemble(pieces :Pieces) :Opt[S] = {
			val res = pieces.preset(backer)
			if (res.isDefined) res else backer.assemble(pieces)
		}

		override def apply[T](component :Component[T]) :Extract[T] =
			(if ((component eq backer) | (component eq this))
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
				//consider: what if backer has subcolumns?
				NaturalMap.single[Column, ColumnExtract, S](column, ColumnExtract.ident(column))
			case _ =>
				backer.columnExtracts
		}


		override def toString :String = "->" + backer
	}



	/** A skeleton of a mapping proxy which has its `backer` mapping as its only direct component
	  * and its components as the subcomponents. All (sub)components of this mapping, including `backer`, are their own
	  * ''export'' versions. It differs from its `ShallowProxy` base trait in that `backer` is the only exposed, direct
	  * component, and is normally included in the subcomponents (and potentially column) lists. It also doesn't ignore
	  * its buffs, normally (through [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]]) calling
	  * its [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportProxy]]
	  */
	trait WrapperProxy[S, O] extends ShallowProxy[S, O] with WrapperDelegate[S, O] {
		override def assemble(pieces :Pieces) :Opt[S] = pieces.get(backer)
	}



	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is.
	  * During assembly, it checks [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]] for preset values
	  * for this instance and, if none is available, it initiates full assembly process of  `backer`,
	  * as if it was the only component of this mapping, from checking for a preset value, through assembly and
	  * ending with handling its buffs. All other methods are forwarded directly to the backing mapping for efficiency,
	  * in turn requiring that no buffs are applied to this instance (the buffs on the backing mapping will function
	  * as normal). In addition to the component-related methods inherited from `ShallowProxy`, it overrides
	  * also form- and `ComponentValues`-creating methods, likewise delegating them to `backer`.
	  *
	  * The difference between this proxy and
	  * [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportProxy ExportProxy]] is that the latter is
	  * the ''export'' version of the proxied `backer`. This proxy ''cannot'' be the export version of `backer`,
	  * or infinite recursion will occur.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportProxy]]
	  */
	trait DirectProxy[S, O] extends ShallowProxy[S, O] {

		override def optionally(pieces :Pieces) :Opt[S] = pieces.assemble(this)
		override def assemble(pieces :Pieces) :Opt[S] = pieces.get(backer) //infinite recursion potential!

		/** Direct proxies have no buffs, relying instead on the buffs of the backing mapping, as they delegate
		  * all methods to the corresponding methods of the backing mapping. Changing buffs would potentially
		  * require overriding all methods declared here and, in particular, change the nature of components
		  * were they to inherit those buffs.
		  */
		final override def buffs :Buffs[S] = Buffs.empty


		override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] =
			backer.writtenValues(op, subject)

		override def filterValues(subject :S) :ComponentValues[S, O] = backer.filterValues(subject)
		override def insertValues(subject :S) :ComponentValues[S, O] = backer.insertValues(subject)
		override def updateValues(subject :S) :ComponentValues[S, O] = backer.updateValues(subject)

		override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			backer.writtenValues(op, subject, collector)

		override def filterValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			backer.filterValues(subject, collector)

		override def insertValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			backer.insertValues(subject, collector)

		override def updateValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			backer.updateValues(subject, collector)


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.contains(backer))
				backer.selectForm(backer.selectedByDefault :++ components.view.filter(_ != backer))
			else backer.selectForm(components)

		override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer))
				backer.filterForm(backer.filteredByDefault :++ components.view.filter(_ != backer))
			else backer.filterForm(components)

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer))
				backer.insertForm(backer.insertedByDefault :++ components.view.filter(_ != backer))
			else backer.insertForm(components)

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer))
				backer.updateForm(backer.updatedByDefault :++ components.view.filter(_ != backer))
			else backer.updateForm(components)

		override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer))
				backer.writeForm(op, op.defaultColumns(backer) :++ components.view.filter(_ != backer))
			else backer.writeForm(op, components)

		override def selectForm :SQLReadForm[S] = backer.selectForm
		override def filterForm :SQLWriteForm[S] = backer.filterForm
		override def insertForm :SQLWriteForm[S] = backer.insertForm
		override def updateForm :SQLWriteForm[S] = backer.updateForm
		override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = backer.writeForm(op)

	}




	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is, but considers itself
	  * its ''export'' version. It should only be used for non column mappings. Otherwise it behaves similarly
	  * [[net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy ShallowProxy]], in particular
	  * it delegates its [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method to `backer`,
	  * ignoring its [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
	  * However, unless overriden, the buffs on this instance are exactly the buffs of the adapted mapping.
	  * In the result, the buffs are processed exactly once for the assembled value.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy]]
	  */
	trait ExportProxy[S, O]
		extends MappingProxy[S, O] with ShallowDelegate[S, O] with DelegateMapping[RefinedMapping[S, O], S, O]
	{
		override def assemble(pieces :Pieces) :Opt[S] = backer.assemble(pieces)

		override def export[T](component :Component[T]) :Component[T] =
			if ((component eq backer) | (component eq this)) this.asInstanceOf[Component[T]]
			else backer.export(component)

		override def exportOrNot[T](component :Component[T]) :Component[T] =
			if ((component eq backer) | (component eq this)) this.asInstanceOf[Component[T]]
			else backer.exportOrNot(component)


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



	/** A proxy mapping which doesn't consider the adapted mapping (or its subcomponents) as its component(s).
	  * As far as the assembly process is concerned, it is similar
	  * to [[net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy DirectProxy]], returning
	  * the value of `backer` from `assemble` as if it was a component of this mapping. This means that it
	  * must be a component of a larger mapping containing this mapping. Other methods however are inherited
	  * from [[net.noresttherein.oldsql.schema.support.EffectivelyEmptyMapping EffectivelyEmptyMapping]], treating
	  * this proxy as if it didn't have a value or any components.
	  *
	  * Subclasses of this trait are used to include a component already present in a table mapping
	  * under some other component.
	  * @see [[net.noresttherein.oldsql.schema.bits.SymlinkMapping]]
	  */
	trait EmptyProxy[S, O]
		extends MappingProxy[S, O] with DelegateMapping[RefinedMapping[S, O], S, O] with EffectivelyEmptyMapping[S, O]
	{
		override def optionally(pieces :Pieces) :Opt[S] = pieces.assemble(this)
		override def assemble(pieces :Pieces) :Opt[S] = pieces.get(backer) //infinite recursion potential!

		final override def buffs :Buffs[S] = Buffs.empty

		override def columnNamed(name :String) :Column[_] = backer.columnNamed(name)
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
			val extract = adapters.get(base.export) match {
				case Some(ex) => ex
				case _ =>
					val export = adaptExport(base.export)
					val ex = MappingExtract(export)(base)
					adapters.put(base.export, ex)         //unnecessary, but prevents duplicate extract instances
					originals.put(export, base.export)
					ex
			}
			adapters.put(comp, extract)
		}

		override def apply[T](component :Component[T]) :Extract[T] = extracts(component)
		override def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)

		override def export[T](component :Component[T]) :Component[T] = extracts(component).export
		override def export[T](column :Column[T]) :Column[T] = columnExtracts(column).export

		override val columns :Unique[Column[_]] = backer.columns.map(alias(adapters, originals, _))
		override val components :Unique[Component[_]] = backer.components.map(alias(adapters, originals, _))
		override val subcomponents :Unique[Component[_]] = backer.subcomponents.map(alias(adapters, originals, _))
		override val extracts :ExtractMap = initExtractMap //has to be last because it can use adapters map
		override val columnExtracts :ColumnExtractMap = filterColumnExtracts(this)(extracts)

		protected[MappingProxy] def initExtractMap :ExtractMap

		private val columnMap = columns.view.map { c => (c.name, c) }.toMap

		override def columnNamed(name :String) :Column[_] = columnMap.getOrElse(name, null) match {
			case null => throw new NoSuchComponentException("No column '" + name + "' in " + this + ".")
			case res => res
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
	  * together with the adapted components. The buffs of this proxy are exactly the same as those on the adapted
	  * mapping, but the latter are ignored during assembly. This property can be safely overriden.
	  */
	abstract class DeepProxy[S, O] private (protected override val backer :MappingOf[S],
	                                        exports :mutable.Map[Mapping, MappingExtract[S, _, O]])
		extends AbstractDeepProxy[S, O](backer, exports) with StableMapping
	{
		//a private constructor with a mutable map ensures that extract entries can be created as method side effects,
		//without the map becoming an instance field - it is used only in initialization of the 'extracts' properties
		def this(backer :MappingOf[S]) = this(backer, mutable.Map.empty)

		override def assemble(pieces :Pieces) :Opt[S] = backer.assemble(pieces.asInstanceOf[backer.Pieces])

		protected[MappingProxy] override def initExtractMap :ExtractMap = {
			exports.put(this, MappingExtract.ident(this))
			NaturalMap[Component, Extract](
				exports.view.map { case (comp, extract) =>
					Assoc[Component, Extract, Any](comp.asInstanceOf[Component[Any]], extract.asInstanceOf[Extract[Any]])
				}.toSeq  :_*
			)
		}
	}






	/** A mapping proxy which does not expose its `backer` mapping or its components, even in its `extracts`.
	  * Similar to [[net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy DeepProxy]],
	  * but the mapping from `backer` components to their adapters in this class's components is done by a separate
	  * `Map`, which is used to perform an extra level of aliasing of
	  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] before passing them to `backer` for assembly.
	  * Handling of buffs on the proxied component is skipped and buffs on this instance are empty, meaning the
	  * component will receive no special treatment, unless the latter property is overriden by subclasses.
	  * This is to protect against `backer` instance being used as another component of of the same root mapping,
	  * in particular in foreign key scenarios.
	  */
	abstract class OpaqueProxy[S, O] private (protected override val backer :MappingOf[S],
	                                          substitutes :mutable.Map[Mapping, MappingExtract[S, _, O]])
		extends AbstractDeepProxy[S, O](backer, substitutes) with StableMapping with ExportMapping
	{
		//a private constructor with a mutable map ensures that extract entries can be created as method side effects,
		//without the map becoming an instance field - it is used only in initialization of the 'extracts' properties
		def this(backer :MappingOf[S]) = this(backer, mutable.Map.empty)

		override def assemble(pieces :Pieces) :Opt[S] = //composes aliasing to substitute components of component to this
			backer.asInstanceOf[Component[S]].assemble(pieces.selectivelyAliased(aliases))

		override def buffs :Buffs[S] = Buffs.empty[S]

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
			substitutes.view.map { entry => //will contain an extract for this because substitutes contains backer->this
				def export[T](extract :MappingExtract[S, T, O]) =
					Assoc[Component, Extract, T](extract.export, extract)
				export(entry._2)
			}.toList : _*
		)
	}



	/** A `ColumnMapping` implementation which delegates its
	  * [[net.noresttherein.oldsql.schema.support.MappingProxy.OpaqueColumnProxy.assemble assemble]] method to
	  * another column. The proxy is ''opaque'' in that the underlying column is not exposed to the outside
	  * (this proxy column is its own column as per `ColumnMapping` default); additionally,
	  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] are
	  * [[net.noresttherein.oldsql.haul.ComponentValues.aliased aliased]]: the adapted column is substituted with
	  * this proxy when checking for a preset value.
	  */ //consider: why can't we just have it in the extract map, so it's aliased automatically?
	class OpaqueColumnProxy[S, O] private (protected override val backer :ColumnMapping[S, _], 
	                                       override val name :String, override val buffs :Buffs[S],
	                                       override val form :ColumnForm[S])
		extends MappingProxy[S, O] with ColumnMapping[S, O] with StableColumn[S, O]
	{
		def this(backer :ColumnMapping[S, _]) = this(backer, backer.name, backer.buffs, backer.form)
		def this(backer :ColumnMapping[S, _], name :String) = this(backer, name, backer.buffs, backer.form)
		def this(backer :ColumnMapping[S, _], buffs :Buffs[S]) = this(backer, backer.name, buffs, backer.form)
		def this(backer :ColumnMapping[S, _], name :String, buffs :Buffs[S]) = this(backer, name, buffs, backer.form)
		
		private[this] val aliasing = new (MappingAt[O]#Component =#> MappingAt[O]#Component) {
			override def apply[T](x :Component[T]) =
				if (x eq backer) this.asInstanceOf[Column[T]] else null
		}
		
		override def assemble(pieces :Pieces) :Opt[S] =
			backer.asInstanceOf[Column[S]].assemble(pieces.aliased(aliasing))
	}



	/** A `ColumnMapping` which is a proxy to another column. All methods delegate to the corresponding methods
	  * of `backer`, with the exception of component/column lists and extracts map, which retain the default `ColumnMapping`
	  * implementation, declaring no components and this column as its only column.
	  * As with [[net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy DirectProxy]], the buffs of this
	  * proxy are empty, but the buffs of the adapted column are used normally.
	  * This instance must not be used as the export version of the adapted column, or infinite recursion will occur.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportColumnProxy]]
	  */
	class DirectColumnProxy[S, O] private (protected override val backer :ColumnMapping[S, O], 
	                                       override val name :String, override val form :ColumnForm[S])
		extends MappingProxy[S, O] with ColumnMapping[S, O] with StableColumn[S, O]
	{
		def this(backer :ColumnMapping[S, O]) = this(backer, backer.name, backer.form)
		def this(backer :ColumnMapping[S, O], name :String) = this(backer, name, backer.form)

		override def optionally(pieces :Pieces) :Opt[S] = pieces.assemble(this)
		override def assemble(pieces :Pieces) :Opt[S] = pieces.get(backer)

		final override def buffs :Buffs[S] = Buffs.empty

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
	}



	/** A `ColumnMapping` proxy to be used as an export version of another column. This class is required
	  * for columns which do not extend [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn SimpleColumn]]
	  * (as those can be adapted by simply mapping their forms). It allows to override the name, buffs and form
	  * of the adapted column and delegates to it its
	  * [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportColumnProxy.assemble assemble]] method.
	  * As is the standard convention among `ColumnMapping` implementation, the components list are empty,
	  * while the column lists contain this instance or nothing (depending on the buffs). The extracts however
	  * contain all extracts from the adapted column, substituting `this` for `backer` in every extract.
	  * Extracts for this column and `backer` use `this` as the export component.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.DirectColumnProxy]]
	  */
	class ExportColumnProxy[S, O] private (protected override val backer :ColumnMapping[S, O],
	                                       override val name :String, override val buffs :Buffs[S],
	                                       override val form :ColumnForm[S])
		extends MappingProxy[S, O] with ColumnMapping[S, O] with StableColumn[S, O]
	{
		def this(backer :ColumnMapping[S, O]) = this(backer, backer.name, backer.buffs, backer.form)
		def this(backer :ColumnMapping[S, O], name :String) = this(backer, name, backer.buffs, backer.form)
		def this(backer :ColumnMapping[S, O], buffs :Buffs[S]) = this(backer, backer.name, buffs, backer.form)
		def this(backer :ColumnMapping[S, O], name :String, buffs :Buffs[S]) =
			this(backer, name, buffs, backer.form)

		override def assemble(pieces :Pieces) :Opt[S] = backer.assemble(pieces)

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


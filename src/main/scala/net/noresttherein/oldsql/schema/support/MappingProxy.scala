package net.noresttherein.oldsql.schema.support

import java.lang.invoke.VarHandle.releaseFence

import scala.collection.mutable

import net.noresttherein.oldsql
import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.generic.=>:
import net.noresttherein.oldsql.schema.{filterColumnExtracts, Buffs, ColumnExtract, ColumnForm, ColumnMapping, SpecificExtract, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnMapping.{StableColumn, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, MappingTemplate, TypedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bases.{ExportMapping, StableMapping}
import net.noresttherein.oldsql.schema.support.DelegateMapping.{ShallowDelegate, WrapperDelegate}

//implicits:
import net.noresttherein.oldsql.slang._





/** A `DelegateMapping` subtype which preserves the subject type of the adapted mapping.
  * Proxy classes can be divided into two broad groups: ''shallow'' proxies, which use the components
  * of the adapted mapping as their own, without changes, and 'deep' proxies, which create an adapter for some
  * or all components of `backer`. The first group is generally restricted to adapters which do not change
  * the effective buffs, at least those used during assembly, so that there are no changes in their cascading.
  * These classes derive from either [[net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy ShallowProxy]]
  * (treating `backer` as its own ''export'' version)
  * and [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportProxy ExportProxy]]
  * (treating itself as the ''export'' version of `backer`). The second group, for the most part,
  * is derived from [[net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy DeepProxy]].
  * @see [[net.noresttherein.oldsql.schema.support.DelegateMapping]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingDecorator]]
  */
trait MappingProxy[S, O] extends DelegateMapping[MappingOf[S], S, O] {
//	protected override val backer :MappingOf[S] //to grant access
	override def nullValue :NullValue[S] = backer.nullValue

	override def buffs :Buffs[S] = backer.buffs
}






object MappingProxy { //todo: revise writtenValues methods to be consistent with select regarding buff handling

	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is, treating
	  * the `backer` mapping as just another component, being its own ''export'' version. For efficiency,
	  * it is excluded from all component and column list, but subclasses are free to change this. It is however
	  * still included among the `extracts`, mapped to itself, and a value preset for it in
	  * [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]] will be returned by this mapping
	  * (assuming there is none preset for this mapping). Select/assembly related buffs on the adapted mapping
	  * are ignored by bypassing its [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
	  * However, unless overriden, the buffs on this instance are exactly the buffs of the adapted mapping.
	  * As the result, the buffs are processed exactly once for the assembled value, albeit in a default manner,
	  * ignoring potentially overriden implementation of `optionally` in the adapted mapping.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportProxy]]
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy]]
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.WrapperProxy]]
	  */
	trait ShallowProxy[S, O]
		extends MappingProxy[S, O] with ShallowDelegate[S, O] with DelegateMapping[TypedMapping[S, O], S, O]
	{
		protected override val backer :TypedMapping[S, O] //only to grant access to other.bqcker

		override def assemble(pieces :Pieces) :Opt[S] = {
			val res = pieces.preset(backer)
			if (res.isDefined) res else backer.assemble(pieces)
		}

		override def extracts :NaturalMap[Component, Extract] =
			backer.extracts.updated[Extract, S](backer, backer(backer))

		override def columnExtracts :NaturalMap[Column, ColumnExtract] = backer.columnExtracts

		override def apply[T](component :Component[T]) :Extract[T] =
			(if (component eq this)
				 MappingExtract.ident(this)
			 else
				 backer(component)
			).asInstanceOf[Extract[T]]

		override def apply[T](column :Column[T]) :ColumnExtract[T] =
			(if (column eq backer)
				 MappingExtract.ident(backer)
			 else
				 backer(column)
			).asInstanceOf[ColumnExtract[T]]

		override def original :Component[S] = backer.original

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
	  * or infinite recursion will occur. It is more similar to a sister shallow proxy type
	  * [[net.noresttherein.oldsql.schema.support.MappingProxy.WrapperProxy WrapperProxy]], with the same assembly
	  * process, but for three differences:
	  *   1. The direct components of this mapping are the direct components of `backer`, where the latter
	  *      contains `backer` as its only component;
	  *   1. The subcomponents of this proxy do not include `backer`;
	  *   1. Lack of buffs in this instance allows overriding more `Mapping` methods by delegating them
	  *      directly to `backer`, where `WrapperProxy`, allowing subclasses to define buffs, relies on
	  *      the default implementations of [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]]
	  *      and other methods.
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


		override def writtenValues[T](op :WriteOperationView, subject :S) :ComponentValues[S, O] =
			backer.writtenValues(op, subject)

		override def filterValues(subject :S) :ComponentValues[S, O] = backer.filterValues(subject)
		override def insertValues(subject :S) :ComponentValues[S, O] = backer.insertValues(subject)
		override def updateValues(subject :S) :ComponentValues[S, O] = backer.updateValues(subject)

		override def writtenValues[T](op :WriteOperationView, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
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

		protected override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(backer))
				backer.writeForm(op, op.defaultColumns(backer) :++ components.view.filter(_ != backer))
			else backer.writeForm(op, components)

		override def selectForm :SQLReadForm[S] = backer.selectForm
		override def filterForm :SQLWriteForm[S] = backer.filterForm
		override def insertForm :SQLWriteForm[S] = backer.insertForm
		override def updateForm :SQLWriteForm[S] = backer.updateForm
		override def writeForm(op :WriteOperationView) :SQLWriteForm[S] = backer.writeForm(op)
		protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[S] = backer.writeForm(op)

		override def uniIsomorphic(that :Mapping) :Boolean = backer isomorphic that
		override def uniHomomorphic(that :Mapping) :Boolean = backer homomorphic that
	}




	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is, but considers itself
	  * its ''export'' version. It should only be used for non column mappings. Otherwise it behaves similarly
	  * [[net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy ShallowProxy]], in particular
	  * it delegates its [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method to `backer`,
	  * ignoring its [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
	  * However, unless overriden, the buffs on this instance are exactly the buffs of the adapted mapping.
	  * As the result, the buffs are processed exactly once for the assembled value.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy]]
	  */
	trait ExportProxy[S, O]
		extends MappingProxy[S, O] with ShallowDelegate[S, O] with DelegateMapping[TypedMapping[S, O], S, O]
	{
		override def assemble(pieces :Pieces) :Opt[S] = backer.assemble(pieces)

		override def extracts :NaturalMap[Component, Extract] =
			backer.extracts.updated[Extract, S](backer, MappingExtract.ident(this))

		override def columnExtracts :NaturalMap[Column, ColumnExtract] = backer.columnExtracts

		override def original :TypedMapping[S, O] = backer.original

		override def apply[T](component :Component[T]) :Extract[T] =
			(if (component eq backer)
				MappingExtract.ident(this)
			else
				backer(component)
			).asInstanceOf[Extract[T]]

		override def apply[T](column :Column[T]) :ColumnExtract[T] = backer(column)

		override def export[T](component :Component[T]) :Component[T] =
			if ((component eq backer) | (component eq this)) this.asInstanceOf[Component[T]]
			else backer.export(component)

		override def exportOrNot[T](component :Component[T]) :Component[T] =
			if ((component eq backer) | (component eq this)) this.asInstanceOf[Component[T]]
			else backer.exportOrNot(component)

		override def unexport[X](component :Component[X]) :Component[X] =
			if (component eq this) backer.asInstanceOf[Component[X]]
			else component

		override def uniIsomorphic(that :Mapping) :Boolean = backer isomorphic that
		override def uniHomomorphic(that :Mapping) :Boolean = backer homomorphic that

		override def toString :String = "^" + backer + "^"
	}



	/** A proxy mapping which doesn't consider the adapted mapping (or its subcomponents) as its component(s).
	  * As far as the assembly process is concerned, it is similar
	  * to [[net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy DirectProxy]], returning
	  * the value of `backer` from `assemble` as if it was a component of this mapping. This means that it
	  * must be a (unique) component of a larger mapping containing this mapping, or a `NoSuchElementException`
	  * will be thrown during assembly from [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]].
	  * Other methods however are inherited
	  * from [[net.noresttherein.oldsql.schema.support.EffectivelyEmptyMapping EffectivelyEmptyMapping]], treating
	  * this proxy as if it didn't have a value or any components.
	  *
	  * Subclasses of this trait are used to include a component already present in a table mapping
	  * under some other component.
	  * @see [[net.noresttherein.oldsql.schema.bits.SymlinkMapping]]
	  */
	trait EmptyProxy[S, O]
		extends MappingProxy[S, O] with DelegateMapping[TypedMapping[S, O], S, O] with EffectivelyEmptyMapping[S, O]
	{
		override def optionally(pieces :Pieces) :Opt[S] = pieces.assemble(this)
		override def assemble(pieces :Pieces) :Opt[S] = pieces.get(backer) //infinite recursion potential!

		final override def buffs :Buffs[S] = Buffs.empty

		protected override def unexport[X](component :Component[X]) :Component[X] =
			if (component eq this) backer.asInstanceOf[Component[X]] else component

		protected override def unexport[X](column :Column[X]) :Column[X] =
			if (column eq this) backer.asInstanceOf[Column[X]] else column

		override def columnNamed(name :String) :Column[_] = backer.columnNamed(name)
	}






	/** The bulk of implementation
	  * of its main subclass [[net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy DeepProxy]]
	  * shared with [[net.noresttherein.oldsql.schema.support.MappingProxy.OpaqueProxy OpaqueProxy]].
	  * Unlike the former, it doesn't assume that `backer` is of the same origin and that it is actually a component
	  * of the enclosing mapping, restricting itself to simply copying the component structure and relying on
	  * `backer` and its components only for assembly.
	  *
	  * This class is an implementation artifact and should not be used directly by client code.
	  */
	private[MappingProxy] sealed abstract
	class AbstractDeepProxy[+Comp[T, Q] <: TypedMapping[T, Q], +Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], S, O]
	                      (protected override val backer :MappingOf[S],
		                  /** Temporary (non-field) constructor parameter which becomes `extracts`
		                    * property in the mapping process. *///starts empty
		                   adapters :mutable.Map[Mapping, TypedMapping[S, O]#like[Comp]#Extract[_]])
		extends MappingProxy[S, O] with MappingTemplate[Comp, Col]
	{ self :Comp[S, O] =>
		/** Method called from the `DeepProxy` constructor before any component lists are initialized. */
		protected def preInit(): Unit = ()

		preInit()

		/** Mapping from (''export'') components of this instance back to their `export`
		  * components of `backer`; every entry exists in the reversed form in `extracts`. */
		private[this] val originals = mutable.Map.empty[MappingAt[O], Mapping] //mutable, but I'm lazy. Sue me.

		private[this] val adaptedBacker = adaptBacker
		protected override val selfExtract :like[Comp]#Extract[S] = SpecificExtract.ident(adaptedBacker)
		adapters.put(backer, SpecificExtract.ident(adaptedBacker))
		originals.put(adaptedBacker, backer)

		//fixme: this will evaluate lazy vals in foreign keys, leading to potential infinite loops/nulls
		//  This could be fixed by creating a lazily initialized backing mapping
		for (Assoc(comp, base) <- backer.extracts) {
			val extract = adapters.get(base.export) match {
				case Some(ex) => ex
				case _ =>
					val export = adaptExport(base.export)
					val ex = SpecificExtract(export)(base)
					adapters.put(base.export, ex)         //unnecessary, but prevents duplicate extract instances
					originals.put(export, base.export)
					ex
			}
			adapters.put(comp, extract)
		}

		override val columns        :Unique[Col[_, O]]          = backer.columns.map(alias(adapters, originals, _))
		override val components     :Unique[Comp[_, O]]         = backer.components.map(alias(adapters, originals, _))
		override val subcomponents  :Unique[Comp[_, O]]         = backer.subcomponents.map(alias(adapters, originals, _))
		override val extracts       :like[Comp]#ExtractMap      = initExtractMap[like[Comp]#Extract](adapters) //last, so adapters qre initialized
		override val columnExtracts :like[Col]#ColumnExtractMap =
			filterColumnExtracts(this)(extracts).asInstanceOf[like[Col]#ColumnExtractMap]

		protected[MappingProxy] def initExtractMap[E[X] >: like[Comp]#Extract[X] <: Extract[X]]
		                                          (exports :mutable.Map[Mapping, E[_]]) :NaturalMap[Component, E]

		private val columnMap = columns.view.map { c => (c.name, c) }.toMap
		releaseFence()

		override def columnNamed(name :String) :Col[_, O] =
			columnMap.getOrElse(name, null.asInstanceOf[Col[_, O]]) match {
				case null => throw new NoSuchComponentException("No column '" + name + "' in " + this + ".")
				case res => res
			}


		override def apply[T](component :Component[T]) :SpecificExtract[Comp[T, O], S, T, O] = extracts(component)
		override def apply[T](column :Column[T])       :SpecificExtract[Col[T, O], S, T, O]  = columnExtracts(column)

		override def export[T](component :Component[T]) :Comp[T, O] = extracts(component).export
		override def export[T](column :Column[T]) :Col[T, O] = columnExtracts(column).export


		private[this] def alias[T](exports   :mutable.Map[Mapping, like[Comp]#Extract[_]],
		                           originals :mutable.Map[MappingAt[O], Mapping],
		                           component :backer.Component[T]) :Comp[T, O] =
			exports.getOrElse(component, {
				val base = backer.apply(component)
				val extract = exports.getOrElse(base.export, SpecificExtract(adaptExport(base.export))(base))
				exports.put(component, extract)
				exports.put(base.export, extract)
				originals.put(extract.export, base.export)
				extract
			}).export.asInstanceOf[Comp[T, O]]

		private[this] def alias[T](exports   :mutable.Map[Mapping, like[Comp]#Extract[_]],
		                           originals :mutable.Map[MappingAt[O], Mapping],
		                           column    :backer.Column[T]) :Col[T, O] =
			alias(exports, originals, column :backer.Component[T]).asInstanceOf[Col[T, O]]


		/** Maps a component of this proxy back to its corresponding export component of `backer`. */
		protected override def unexport[T](component :Component[T]) :backer.Component[T] = {
			val res = originals.getOrElse(component, null.asInstanceOf[Component[T]]).asInstanceOf[backer.Component[T]]
			if (res != null) res
			else backer.export(component.asInstanceOf[backer.Component[T]])
		}

		/** Maps a column of this proxy back to its corresponding export column of `backer`. */
		protected override def unexport[T](column :Column[T]) :backer.Column[T] = {
			val res = originals.getOrElse(column, null.asInstanceOf[backer.Column[T]]).asInstanceOf[backer.Column[T]]
			if (res != null) res
			else backer.export(column.asInstanceOf[backer.Column[T]])
		}

		private[this] def adaptExport[T](component :backer.Component[T]) :Comp[T, O] = component match {
			case _ if component eq backer => adaptedBacker.asInstanceOf[Comp[T, O]]
			case column :ColumnMapping => adapt(column.asInstanceOf[backer.Column[T]])
			case _ => adapt(component)
		}

		/** A hook method left for subclasses to implement adapting of a component of the adapted mapping
		  * to its 'export' form under this mapping. All component lists of this instance contain
		  * components of `backer` (and `backer` itself) mapped with this method.
		  * @param component an 'export', non-column component of `backer`.
		  */
		protected def adapt[T](component :backer.Component[T]) :Comp[T, O]

		/** A hook method left for subclasses to implement adapting of a column of the adapted mapping
		  * to its 'export' form under this mapping. All column lists of this instance contain
		  * columns mapped with this method.
		  * @param column an 'export' column of `backer`.
		  */
		protected def adapt[T](column :backer.Column[T]) :Col[T, O] //= adapt(component)

		/** The 'export' version of the adapted `backer` itself. Defaults to `this`. */
		protected def adaptBacker :Comp[S, O] = this

		override def uniHomomorphic(that :Mapping) :Boolean = backer.homomorphic(that)

		override def mappingName :String = "->>" + backer.mappingName
	}



	/** The generic version of [[net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy DeepProxy]],
	  * with all components conforming to the type parameter `Comp[_, O]` and all columns to `Col[_, O]`.
	  *///make it a trait in Scala 3
	abstract class DeepProxyTemplate[+Comp[T, Q] <: TypedMapping[T, Q],
	                                 +Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], S, O] private
	                                (protected override val backer :TypedMapping[S, O],
	                                 exports :mutable.Map[Mapping, TypedMapping[S, O]#like[Comp]#Extract[_]])
		extends AbstractDeepProxy[Comp, Col, S, O](backer, exports)
	{ this :Comp[S, O] =>
		//a private constructor with a mutable map ensures that extract entries can be created as method side effects,
		//without the map becoming an instance field - it is used only in initialization of the 'extracts' properties
		def this(backer :TypedMapping[S, O]) = this(backer, mutable.Map.empty)

		override def assemble(pieces :Pieces) :Opt[S] = backer.assemble(pieces)

		override def original :TypedMapping[S, O] = backer.original

		protected[MappingProxy] override def initExtractMap[E[X] >: like[Comp]#Extract[X] <: Extract[X]]
		                                     (exports :mutable.Map[Mapping, E[_]]) :NaturalMap[Component, E] =
		{
			exports.put(this, SpecificExtract.ident(this))
			NaturalMap.from[Component, E](
				exports.view.map { case (comp, extract) =>
					Assoc[Component, E, Any](
						comp.asInstanceOf[Component[Any]], extract.asInstanceOf[E[Any]]
					)
				}
			)
		}
	}

	/** A `DeepProxy` implementation which eagerly initializes all column and component lists and creates
	  * a fixed mapping between components of the adapted mapping and their adapted counterparts, as well as the
	  * reverse. The components of the adapted mapping are not part of any component or column lists of this mapping,
	  * which are formed by adapting each element of the corresponding list from `backer` with `adapt` methods,
	  * left for subclasses to implement. They are still recognized by this mapping as its subcomponents though,
	  * and `Extract` maps include as keys all the keys from the corresponding extract map of the adapted mapping
	  * together with the adapted components. The adapted mapping has a special status: it is, like its components,
	  * a non-export component of this adapter, with the latter considering itself the export version of the former.
	  * The divergence from the treatment of other components is that despite the above, this adapter is not included
	  * in the components and subcomponents lists with other export components. Due to this fact, and because the
	  * order of the components in all property collections is preserved, this mapping is isomorphic with the
	  * underlying mapping: its [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.counterpart counterpart]]
	  * methods will always return a value for any component of `backer`.
	  *
	  * The buffs of this proxy are exactly the same as those on the adapted
	  * mapping, but the latter are ignored during assembly. This property can be safely overriden.
	  * This adapter is not suitable for columns.
	  */
	abstract class DeepProxy[S, O](protected override val backer :TypedMapping[S, O])
		extends DeepProxyTemplate[TypedMapping, TypedColumn, S, O](backer) with StableMapping






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
		extends AbstractDeepProxy[TypedMapping, TypedColumn, S, O](backer, substitutes)
		   with StableMapping with ExportMapping
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

		protected[MappingProxy] override def initExtractMap[E[X] >: Extract[X] <: Extract[X]]
		                                     (exports :mutable.Map[Mapping, E[_]]) :NaturalMap[Component, E] =
			NaturalMap.from[Component, E](
				exports.view.map { entry => //will contain an extract for this because substitutes contains backer->this
					def export[T](extract :MappingExtract[S, T, O]) =
						Assoc[Component, E, T](extract.export, extract) //put only export as entry._1 can't leak
					export(entry._2 :MappingExtract[S, _, O])
				}
			)

		override def toString :String = "//" + backer
	}



	/** A `TypedColumn` implementation which delegates its
	  * [[net.noresttherein.oldsql.schema.support.MappingProxy.OpaqueColumnProxy.assemble assemble]] method to
	  * another column. The proxy is ''opaque'' in that the underlying column is not exposed to the outside
	  * (this proxy column is its own column as per `TypedColumn` default); additionally,
	  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] are
	  * [[net.noresttherein.oldsql.haul.ComponentValues.aliased aliased]]: the adapted column is substituted with
	  * this proxy when checking for a preset value.
	  */ //consider: why can't we just have it in the extract map, so it's aliased automatically?
	class OpaqueColumnProxy[S, A, O] private (protected override val backer :TypedColumn[S, A],
	                                          override val name :String, override val buffs :Buffs[S],
	                                          override val form :ColumnForm[S])
		extends MappingProxy[S, O] with StableColumn[S, O]
	{
		def this(backer :TypedColumn[S, A]) = this(backer, backer.name, backer.buffs, backer.form)
		def this(backer :TypedColumn[S, A], name :String) = this(backer, name, backer.buffs, backer.form)
		def this(backer :TypedColumn[S, A], buffs :Buffs[S]) = this(backer, backer.name, buffs, backer.form)
		def this(backer :TypedColumn[S, A], name :String, buffs :Buffs[S]) = this(backer, name, buffs, backer.form)

		protected override def unexport[X](component :Component[X]) :backer.Component[X] =
			if (component eq this) backer.asInstanceOf[backer.Component[X]]
			else throw new IllegalArgumentException(s"Mapping $component is not a component of column $this.")

		protected override def unexport[X](column :Column[X]) :backer.Column[X] =
			if (column eq this) backer.asInstanceOf[backer.Column[X]]
			else throw new IllegalArgumentException(s"Column $column is not a component of column $this.")

		private[this] val aliasing = new (MappingAt[O]#Component =>: MappingAt[O]#Component) {
			override def apply[T](x :Component[T]) =
				if (x eq backer) this.asInstanceOf[Column[T]] else null
		}
		
		override def assemble(pieces :Pieces) :Opt[S] =
			backer.asInstanceOf[Column[S]].assemble(pieces.aliased(aliasing))

		override def toString :String = "//" + backer
	}



	/** A `TypedColumn` which is a proxy to another column. All methods delegate to the corresponding methods
	  * of `backer`, with the exception of component/column lists and extracts map, which retain the default
	  * `TypedColumn` implementation, declaring no components and this column as its only column.
	  * As with [[net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy DirectProxy]], the buffs of this
	  * proxy are empty, but the buffs of the adapted column are used normally.
	  * This instance must not be used as the export version of the adapted column, or infinite recursion will occur.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportColumnProxy]]
	  */
	class DirectColumnProxy[S, O] private (protected override val backer :TypedColumn[S, O],
	                                       override val name :String, override val form :ColumnForm[S])
		extends MappingProxy[S, O] with StableColumn[S, O]
	{
		def this(backer :TypedColumn[S, O]) = this(backer, backer.name, backer.form)
		def this(backer :TypedColumn[S, O], name :String) = this(backer, name, backer.form)

		override def optionally(pieces :Pieces) :Opt[S] = pieces.assemble(this)
		override def assemble(pieces :Pieces) :Opt[S] = pieces.get(backer)

		final override def buffs :Buffs[S] = Buffs.empty

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.isEmpty)  SQLReadForm.empty
			else if (components.contains(this)) selectForm
			else backer.selectForm(components)

		protected override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.isEmpty) SQLWriteForm.empty
			else if (components.contains(this)) form
			else backer.writeForm(op, components)


		override lazy val columnExtracts :NaturalMap[Column, ColumnExtract] =
			backer.columnExtracts.updated[ColumnExtract, S](backer, backer(backer))
				.updated[ColumnExtract, S](this, super.apply(this))

		//this can fail with ClassCastException for custom NaturalMap implementations
		override def extracts :NaturalMap[Component, ColumnExtract] =
			columnExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]


		override def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)
//		override def apply[T](component :Component[T]) :ColumnExtract[T] = extracts(component)

		override def export[T](component :Component[T]) :Column[T] =
			if ((component eq this) | (component eq backer)) component.asInstanceOf[Column[T]]
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

		override def unexport[X](component :Component[X]) :Component[X] = component match {
			case column :TypedColumn[X, O] @unchecked => unexport(column)
			case _ => throw new IllegalArgumentException(s"Cannot unexport a non-column component $component with $this.")
		}

		override def unexport[X](column :Column[X]) :Column[X] =
			if (column eq this)
				throw new IllegalArgumentException("Cannot un-export the adapter column itself: " + column)
			else
				column

		override def contains[T](component :Component[T]) :Boolean =
			(component eq this) | (component eq backer) || (backer contains component)

		override def toString :String = "->" + backer
	}



	/** A `TypedColumn` proxy to be used as an export version of another column. This class is required
	  * for columns which do not extend [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn SimpleColumn]]
	  * (as those can be adapted by simply mapping their forms). It allows to override the name, buffs and form
	  * of the adapted column and delegates to it its
	  * [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportColumnProxy.assemble assemble]] method.
	  * As per the standard contract of `TypedColumn`, the components lists are empty,
	  * while the column lists contain this instance or nothing (depending on the buffs). The extracts however
	  * contain all extracts from the adapted column, substituting `this` for `backer` in every extract.
	  * Extracts for this column and `backer` use `this` as the export component.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy.DirectColumnProxy]]
	  */
	class ExportColumnProxy[S, O] private (protected override val backer :TypedColumn[S, O],
	                                       override val name :String, override val buffs :Buffs[S],
	                                       override val form :ColumnForm[S])
		extends MappingProxy[S, O] with StableColumn[S, O]
	{
		def this(backer :TypedColumn[S, O]) = this(backer, backer.name, backer.buffs, backer.form)
		def this(backer :TypedColumn[S, O], name :String) = this(backer, name, backer.buffs, backer.form)
		def this(backer :TypedColumn[S, O], buffs :Buffs[S]) = this(backer, backer.name, buffs, backer.form)
		def this(backer :TypedColumn[S, O], name :String, buffs :Buffs[S]) =
			this(backer, name, buffs, backer.form)

		override def assemble(pieces :Pieces) :Opt[S] = backer.assemble(pieces)

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.isEmpty) SQLReadForm.empty
			else if (components.contains(this) || components.contains(backer)) form
			else backer.selectForm(components)

		override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.isEmpty) SQLWriteForm.empty
			else if (components.contains(this)) form
			else backer.writeForm(op, components)

		override def original :TypedColumn[S, O] = backer.original

		//lazy because backer.extracts might be lazy as in ForeignKeyColumnMapping
		override lazy val columnExtracts :NaturalMap[Column, ColumnExtract] =
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
		override def extracts = columnExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]


		override def apply[T](component :Component[T]) :ColumnExtract[T] =
			if (component == this) super[StableColumn].apply(component) //more to avoid lazy init than anything else
			else extracts(component)

		override def contains[T](component :Component[T]) :Boolean =
			(component eq this) | (component eq backer) || (backer contains component)

		protected override def unexport[X](component :Component[X]) :Component[X] =
			if (component == this) backer.asInstanceOf[Component[X]] else component

		protected override def unexport[X](column :Column[X]) :Column[X] =
			if (column == this) backer.asInstanceOf[Column[X]] else column

		override def toString :String = "^" + backer + "^"
	}
	
	
}


package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.{Buff, Buffs}
import net.noresttherein.oldsql.schema.Buffs.BuffsZipper
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy






/** A deep mapping proxy substituting the buffs of `backer` with the given collection.
  * All (sub)components of `backer` are similarly adapted, with all buffs inherited (and adapted) from `backer`
  * being replaced with a cascaded copy of `replacements`. This class makes some assumptions when identifying
  * the inherited buffs suffix to replace in subcomponents.
  *   1. The [[net.noresttherein.oldsql.schema.Buffs.tag tag]] of `backer.buffs` is unique in the whole
  *      component tree;
  *   1. When adapting ('cascading') buffs from `backer` for subcomponents, `tag` of the inherited buffs is preserved;
  *   1. Some or all declarations from a mapping can be dropped by its (sub)component, but the order
  *      of inherited declarations is preserved: it two declarations with the same tags are both present
  *      on two components, then they are present in the same order. The declaration here means a complete
  *      `Buff` collection declared on a single mapping.
  *
  * All above conditions hold within default implementations; however, if buffs were reset by a subcomponent of `backer`
  * (and, in the result, inheriting no buffs from enclosing mappings by it or its components), the operation will
  * have no effect for that subcomponent subtree.
  *
  * The replacement buffs are cascaded for every component, but declarations which were not inherited by the component
  * are removed. Then, all buffs inherited by the component from `backer.buffs` are replaced with the new list.
  * Any hypothetical existing declarations in `component.buffs` which are located after a declaration inherited
  * from `backer.buffs` but are not themselves inherited, are also removed.
  * In this way, the new buffs of every mapping in the component tree of `backer` consist of three segments:
  *   1. old buffs of the mapping, without buffs inherited from `backer`,
  *   1. 'new' declarations in `replacements`: cascaded `replacements` with the longest suffix common with
  *      `backer.buffs` dropped,
  *   1. 'old' declarations in `replacements`: the suffix of `replacements` common with `backer.buffs`,
  *      with any declarations not present in the old buffs of the mapping removed.
  * This behaviour aims to combine the 'delta' buffs of all components with the 'delta' buffs of the replacements.
  * If `replacements` contains declarations from `backer.buffs` as a suffix, it is thus treated as 'delta' buffs
  * rather than flat out replacement, and those preserved buffs are cascaded to the new export subcomponents only
  * if they cascaded in the adapted mapping.
  * In particular, if none of the tags from `backer.buffs` are found in the `component.buffs` stack
  * (possible if some intermediate component declares no buffs at all, for example), then `replacements` are added on
  * the very bottom of the stack, after dropping the whole common suffix with `backer.buffs`.
  * @author Marcin Mościcki
  */
class BuffedMapping[+M <: RefinedMapping[S, O], S, O](protected override val backer :M, replacements :Buffs[S])
	extends DeepProxy[S, O](backer) with DelegateMapping[M, S, O]
{
	override def buffs :Buffs[S] = replacements

	protected def buffsFor[T](component :backer.Component[T]) :Buffs[T] = {
		val suffix = replacements.unsafeCascade(backer(component))
		val inherited = component.buffs.zipper.locate(backer.buffs)   //old buffs split at first inherited from backer
		val combined = inherited.replace(suffix).locate(backer.buffs) //component buffs with inheritance from backer replaced

		//remove declarations inherited from backer from the new replacements if they were not inherited by the component
		def debuff(old :BuffsZipper[T], unzipped :BuffsZipper[T]) :Buffs[T] =
			if (unzipped.isEmpty) unzipped.buffs
			else {
				val carriedOver = old :>> unzipped.source
				if (carriedOver.isEmpty) debuff(old, unzipped.del)
				else debuff(carriedOver, unzipped.>>)
			}
		debuff(inherited, combined)
	}

	protected override def adapt[T](component :backer.Component[T]) :Component[T] =
		new BuffedMapping[Component[T], T, O](component, buffsFor(component))

	protected override def adapt[T](column :backer.Column[T]) :Column[T] =
		column.withBuffs(buffsFor(column))
}






object BuffedMapping {

	def apply[M <: RefinedMapping[S, O], S, O](mapping :M, buffs :Buffs[S]) :Adapted[M] =
		new BuffedMapping[M, S, O](mapping, buffs) with DelegateAdapter[M, S, O]

	def apply[M <: RefinedMapping[S, O], S, O](mapping :M, buffs :Buff[S]*) :Adapted[M] =
		apply[M, S, O](mapping, mapping.buffs.inherited.declare(mapping.buffs.tag, buffs :_*))

	def apply[M <: MappingAt[O], S, O](mapping :MappingAdapter[M, S, O], buffs :Buffs[S]) :MappingAdapter[M, S, O] =
		new BuffedMapping[MappingAdapter[M, S, O], S, O](mapping, buffs) with ComposedAdapter[M, S, S, O]

	def apply[M <: MappingAt[O], S, O](mapping :MappingAdapter[M, S, O], buffs :Buff[S]*) :MappingAdapter[M, S, O] =
		apply[M, S, O](mapping, mapping.buffs.inherited.declare(mapping.buffs.tag, buffs :_*))


	def nonCascading[M <: RefinedMapping[S, O], S, O](mapping :M, buffs :Buffs[S]) :Adapted[M] =
		new NonCascadingBuffedMapping[M, S, O](mapping, buffs) with DelegateAdapter[M, S, O]

	def nonCascading[M <: RefinedMapping[S, O], S, O](mapping :M, buffs :Buff[S]*) :Adapted[M] =
		nonCascading[M, S, O](mapping, mapping.buffs.inherited.declare(mapping.buffs.tag, buffs :_*))

	def nonCascading[M <: MappingAt[O], S, O](mapping :MappingAdapter[M, S, O], buffs :Buffs[S]) :MappingAdapter[M, S, O] =
		new NonCascadingBuffedMapping[mapping.type, S, O](mapping, buffs) with ComposedAdapter[M, S, S, O]

	def nonCascading[M <: MappingAt[O], S, O](mapping :MappingAdapter[M, S, O], buffs :Buff[S]*) :MappingAdapter[M, S, O] =
		nonCascading[M, S, O](mapping, mapping.buffs.inherited.declare(mapping.buffs.tag, buffs :_*))


	private class NonCascadingBuffedMapping[+M <: RefinedMapping[S, O], S, O]
	                                       (protected override val backer :M, override val buffs :Buffs[S])
		extends BuffedMapping[M, S, O](backer, Buffs.empty[S](backer.buffs.tag))

}






trait MappingDeclaredBuffs[S, O] extends BaseMapping[S, O] {

	/** Buffs declared by this instance (rather than inherited). It becomes
	  * the [[net.noresttherein.oldsql.schema.Buffs.front front]] portion of this instance's
	  * [[net.noresttherein.oldsql.schema.support.MappingDeclaredBuffs.buffs buffs]]. This property is for use
	  * in `Buffs` constructor only and it should never be used in place of `buffs.declared`, as extending classes
	  * can override directly `buffs` rather than this property.
	  */
	protected def declaredBuffs :Seq[Buff[S]]

	/** @inheritdoc
	  *
	  * The buffs on this mapping are initialized with those listed by property
	  * [[net.noresttherein.oldsql.schema.support.MappingDeclaredBuffs.declaredBuffs declaredBuffs]].
	  */
	override val buffs :Buffs[S] =
		if (declaredBuffs == null)
			throw new IllegalStateException(
				s"$this.declaredBuffs is null: it must be overriden either with a method or a constructor parameter")
		else
			Buffs(this, declaredBuffs :_*)
}



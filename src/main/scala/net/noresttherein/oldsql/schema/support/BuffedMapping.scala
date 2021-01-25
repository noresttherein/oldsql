package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.{Buff, Buffs}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy






/** A deep mapping proxy substituting the buffs of `backer` with the given collection.
  * All (sub)components of `backer` are similarly adapted, with all buffs inherited (and adapted) from `backer`
  * being replaced with cascaded copy of `replacements`. This class makes some assumptions when identifying
  * the inherited buffs suffix to replace in subcomponents.
  *   1. The [[net.noresttherein.oldsql.schema.Buffs.tag tag]] of `backer.buffs` is unique in the whole
  *      component tree;
  *   1. When adapting ('cascading') buffs from `backer` for subcomponents, `tag` of the inherited buffs is preserved;
  *
  * More formally, the buffs stack bottom for every component of `backer` up to the element with `tag`
  * equal to `backer.buffs.tag` is replaced with `replacements.unsafeCascade(backer(component))`. If the tag
  * of the first declaration of the `backer.buffs` is not preserved, the second tag is searched and so on.
  * The suffix to replace is identified using method [[net.noresttherein.oldsql.schema.Buffs.BuffsZipper.locate locate]]
  * method of buffs [[net.noresttherein.oldsql.schema.Buffs.BuffsZipper zipper]]. In case none of the sources
  * from `backer.buffs` are found in the `component.buffs` stack (possible if some intermediate component declares
  * no buffs at all, for example), then `replacements` are added on the very bottom of the stack, without removing
  * any entry.
  * @author Marcin Mościcki
  */
class BuffedMapping[+M <: RefinedMapping[S, O], S, O](protected override val backer :M, replacements :Buffs[S])
	extends DeepProxy[S, O](backer) with DelegateMapping[M, S, O]
{
	override def buffs :Buffs[S] = replacements

	private def swapBuffs[T](component :backer.Component[T]) :Buffs[T] =
		component.buffs.zipper.locate(backer.buffs).replace(replacements.unsafeCascade(backer(component))).buffs

	protected override def adapt[T](component :backer.Component[T]) :Component[T] =
		new BuffedMapping[Component[T], T, O](component, swapBuffs(component))

	protected override def adapt[T](column :backer.Column[T]) :Column[T] =
		column.withBuffs(swapBuffs(column))
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



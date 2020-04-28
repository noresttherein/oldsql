package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.{-#>, Entry}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, TypedMapping}
import net.noresttherein.oldsql.schema.support.ComponentProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.Buff.{BuffType, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraSelect, FlagBuffType, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate}
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms

import scala.collection.mutable.Builder






/**
  * @author Marcin Mościcki
  */
class CustomizedMapping[+M <: TypedMapping[S, O], S, O] private
                       (override val egg :M, substitutions :NaturalMap[MappingFrom[O]#Component, MappingFrom[O]#Component])
	extends EagerDeepProxy[M, S, O](egg) with MappingAdapter[M, S, O]
{
	protected override def adapt[T](component :egg.Component[T]) :Component[T] =
		substitutions.getOrElse(component, component)

	protected override def adaptColumn[T](column :egg.Column[T]) :Column[T] =
		substitutions.getOrElse(column, column).asInstanceOf[Column[T]]
}





object CustomizedMapping {
	private type Substitution[X, O] = Entry[MappingFrom[O]#Component, MappingFrom[O]#Component, X]
	private type Substitutions[O] = NaturalMap[MappingFrom[O]#Component, MappingFrom[O]#Component]


	def select[M <: TypedMapping[S, O], S, O]
	          (source :M, include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
              (implicit inferS :Conforms[M, M, TypedMapping[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, NoSelect, ExplicitSelect, exclude, OptionalSelect, NoSelectByDefault)

	def query[M <: TypedMapping[S, O], S, O]
	         (source :M, include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
	         (implicit inferS :Conforms[M, M, TypedMapping[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, NoQuery, ExplicitQuery, exclude, OptionalQuery, NoQueryByDefault)

	def update[M <: TypedMapping[S, O], S, O]
	          (source :M, include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
              (implicit inferS :Conforms[M, M, TypedMapping[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, NoUpdate, ExplicitUpdate, exclude, OptionalUpdate, NoUpdateByDefault)

	def insert[M <: TypedMapping[S, O], S, O]
	          (source :M, include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
              (implicit inferS :Conforms[M, M, TypedMapping[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, NoInsert, ExplicitInsert, exclude, OptionalInsert, NoInsertByDefault)



	private def customize[M <: TypedMapping[S, O], S, O]
	                     (source :M, include :Iterable[TypedMapping[_, O]], disabled :BuffType, remove :BuffType,
	                      exclude :Iterable[TypedMapping[_, O]], enabled :BuffType, add :FlagBuffType)
		:CustomizedMapping[M, S, O] =
	{
		val included = withoutBuff(source, include, disabled, remove)
		val excluded = withBuff(source, exclude, enabled, add)
		new CustomizedMapping[M, S, O](source, included ++ excluded)
	}


	/** Removes the `remove` buff from all the components in the `components` collection and their subcomponents.
	  * If the `disabled` buff is present on any of the components, an `IllegalArgumentException` is raised.
	  * If it is present on any of the subcomponents, the presence of `remove` buff is ignored and the subcomponent
	  * remains unchanged.
	  */
	private def withoutBuff[O](mapping :TypedMapping[_, O], components :Iterable[TypedMapping[_, O]],
	                           disabled :BuffType, remove :BuffType)
			:Substitutions[O] =
	{
		val builder = NaturalMap.newBuilder[MappingFrom[O]#Component, MappingFrom[O]#Component]
		for (component <- components)
			withoutBuff(mapping, component, disabled, remove, builder)
		builder.result()
	}

	private def withoutBuff[T, O](mapping :TypedMapping[_, O], component :TypedMapping[T, O],
	                              disabled :BuffType, remove :BuffType,
	                              builder :Builder[Substitution[_, O], Substitutions[O]]) :Unit =
		mapping.export(component) match {
			case comp if disabled.enabled(comp) =>
				throw new IllegalArgumentException(
					s"Can't include component $component off $mapping as it has the $disabled buff."
				)
			case column :ColumnMapping[_, O @unchecked] =>
				if (remove.enabled(column))
					builder += new Substitution(column, column.withBuffs(column.buffs.filter(remove.disabled)))
			case lifted =>
				lifted.subcomponents foreach { sub =>
					sub match {
						case column :ColumnMapping[_, O @unchecked] =>
							val col = mapping.export(column)
							if (disabled.disabled(col) && remove.enabled(col))
								builder += new Substitution(col, col.withBuffs(col.buffs.filter(remove.disabled)))
						case _ =>
							val comp = mapping.export(sub).asInstanceOf[TypedMapping[Any, O]]
							if (disabled.disabled(comp) && remove.enabled(comp))
								builder += new Substitution(comp, BuffedMapping(comp, comp.buffs.filter(remove.disabled) :_*))
					}
				}

		}



	private def withBuff[O](mapping :TypedMapping[_, O], components :Iterable[TypedMapping[_, O]],
	                        enabled :BuffType, add :FlagBuffType)
			:Substitutions[O] =
	{
		val builder = NaturalMap.newBuilder[MappingFrom[O]#Component, MappingFrom[O]#Component]
		for (component <- components)
			withBuff(mapping, component, enabled, add, builder)
		builder.result()
	}

	private def withBuff[T, O](mapping :TypedMapping[_, O], component :TypedMapping[T, O],
	                           enabled :BuffType, add :FlagBuffType,
	                           builder :Builder[Substitution[_, O], Substitutions[O]]) :Unit =
		mapping.export(component) match {
			case column :ColumnMapping[_, O @unchecked] =>
				if (add.disabled(column) && enabled.enabled(column))
					builder += new Substitution(column, column.withBuffs(add[T] +: column.buffs))
			case lifted =>
				lifted.subcomponents foreach { sub =>
					sub match {
						case column :ColumnMapping[_, O @unchecked] =>
							val col = mapping.export(column).asInstanceOf[ColumnMapping[Any, O]]
							if (add.disabled(col) && enabled.enabled(col))
								builder += new Substitution[Any, O](col, col.withBuffs(add[Any] +: col.buffs))
						case _ =>
							val comp = mapping.export(sub).asInstanceOf[TypedMapping[Any, O]]
							if (add.disabled(comp) && enabled.enabled(comp))
								builder += new Substitution[Any, O](comp, BuffedMapping(comp, add[Any] +: comp.buffs :_*))
					}
				}

		}

}

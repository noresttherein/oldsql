package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.{-#>, Entry}
import net.noresttherein.oldsql.morsels.generic
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component}
import net.noresttherein.oldsql.schema.support.ComponentProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.schema.Buff.{BuffType, ExplicitInsert, ExplicitSelect, ExplicitUpdate, ExtraSelect, FlagBuffType, NoInsertByDefault, NoSelectByDefault, NoUpdateByDefault, OptionalInsert, OptionalSelect, OptionalUpdate}
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping}
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth

import scala.collection.mutable.Builder


/**
  * @author Marcin Mościcki
  */
class CustomizedMapping[M <: Component[O, S], O, S] private
                       (override val egg :M, substitutions :NaturalMap[AnyComponent[O]#Component, AnyComponent[O]#Component])
	extends EagerDeepProxy[M, O, S](egg) with Adapted[M]
{
	override protected def adapt[T](component :egg.Component[T]) :Component[T] =
		substitutions.getOrElse(component, component)
}





object CustomizedMapping {
	private type Substitution[O, X] = Entry[AnyComponent[O]#Component, AnyComponent[O]#Component, X]
	private type Substitutions[O] = NaturalMap[AnyComponent[O]#Component, AnyComponent[O]#Component]

	def select[M <: Component[O, S], O, S](source :M, include :Iterable[Component[O, _]], exclude :Iterable[Component[O, _]] = Nil)
	                                      (implicit inferS :IsBoth[M, M, Component[O, S]]) :CustomizedMapping[M, O, S] =
		customize(source, include, ExplicitSelect, exclude, OptionalSelect, NoSelectByDefault)

	def insert[M <: Component[O, S], O, S](source :M, include :Iterable[Component[O, _]], exclude :Iterable[Component[O, _]] = Nil)
	                                      (implicit inferS :IsBoth[M, M, Component[O, S]]) :CustomizedMapping[M, O, S] =
		customize(source, include, ExplicitInsert, exclude, OptionalInsert, NoInsertByDefault)

	def update[M <: Component[O, S], O, S](source :M, include :Iterable[Component[O, _]], exclude :Iterable[Component[O, _]] = Nil)
	                                      (implicit inferS :IsBoth[M, M, Component[O, S]]) :CustomizedMapping[M, O, S] =
		customize(source, include, ExplicitUpdate, exclude, OptionalUpdate, NoUpdateByDefault)



	private def customize[M <: Component[O, S], O, S]
	                     (source :M, include :Iterable[Component[O, _]], remove :BuffType,
	                      exclude :Iterable[Component[O, _]], has :BuffType, add :FlagBuffType)
		:CustomizedMapping[M, O, S] =
	{
		val included = withoutBuff(source, include, remove)
		val excluded = withBuff(source, exclude, has, add)
		new CustomizedMapping[M, O, S](source, included ++ excluded)
	}



	private def withoutBuff[O](mapping :Component[O, _], components :Iterable[Component[O, _]], remove :BuffType)
			:Substitutions[O] =
	{
		val builder = NaturalMap.newBuilder[AnyComponent[O]#Component, AnyComponent[O]#Component]
		for (component <- components)
			withoutBuff(mapping, component, remove, builder)
		builder.result()
	}

	private def withoutBuff[O, T](mapping :Component[O, _], component :Component[O, T], remove :BuffType,
	                              builder :Builder[Substitution[O, _], Substitutions[O]]) :Unit =
		mapping.lift(component) match {
			case column :ColumnMapping[O, _] =>
				if (remove.enabled(column))
					builder += new Substitution(column, column.withBuffs(column.buffs.filter(remove.disabled)))
			case lifted =>
				lifted.subcomponents foreach { sub =>
					val comp = mapping.lift(sub).asInstanceOf[Component[O, Any]]
					if (remove.enabled(comp))
						builder += new Substitution(comp, BuffedMapping(comp, comp.buffs.filter(remove.disabled) :_*))
				}

		}



	private def withBuff[O](mapping :Component[O, _], components :Iterable[Component[O, _]], has :BuffType, add :FlagBuffType)
			:Substitutions[O] =
	{
		val builder = NaturalMap.newBuilder[AnyComponent[O]#Component, AnyComponent[O]#Component]
		for (component <- components)
			withBuff(mapping, component, has, add, builder)
		builder.result()
	}

	private def withBuff[O, T](mapping :Component[O, _], component :Component[O, T], has :BuffType, add :FlagBuffType,
	                           builder :Builder[Substitution[O, _], Substitutions[O]]) :Unit =
		mapping.lift(component) match {
			case column :ColumnMapping[O, _] =>
				if (add.disabled(column) && has.enabled(column))
					builder += new Substitution(column, column.withBuffs(add[T] +: column.buffs))
			case lifted =>
				lifted.subcomponents foreach { sub =>
					val comp = mapping.lift(sub).asInstanceOf[Component[O, Any]]
					if (add.disabled(comp) && has.enabled(comp))
						builder += new Substitution[O, Any](comp, BuffedMapping(comp, add[Any] +: comp.buffs :_*))
				}

		}

}

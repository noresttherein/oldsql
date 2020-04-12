package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.{-#>, Entry}
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component}
import net.noresttherein.oldsql.schema.support.ComponentProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.Buff.{BuffType, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraSelect, FlagBuffType, NoInsertByDefault, NoQueryByDefault, NoSelectByDefault, NoUpdateByDefault, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate}
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth

import scala.collection.mutable.Builder


/**
  * @author Marcin Mościcki
  */
class CustomizedMapping[+M <: Component[S, O], S, O] private
                       (override val egg :M, substitutions :NaturalMap[AnyComponent[O]#Component, AnyComponent[O]#Component])
	extends EagerDeepProxy[M, S, O](egg) with MappingAdapter[M, S, O]
{
	override protected def adapt[T](component :egg.Component[T]) :Component[T] =
		substitutions.getOrElse(component, component)
}





object CustomizedMapping {
	private type Substitution[X, O] = Entry[AnyComponent[O]#Component, AnyComponent[O]#Component, X]
	private type Substitutions[O] = NaturalMap[AnyComponent[O]#Component, AnyComponent[O]#Component]

	def select[M <: Component[S, O], S, O](source :M,
	                                       include :Iterable[Component[_, O]], exclude :Iterable[Component[_, O]] = Nil)
	                                      (implicit inferS :IsBoth[M, M, Component[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, ExplicitSelect, exclude, OptionalSelect, NoSelectByDefault)

	def query[M <: Component[S, O], S, O](source :M,
	                                      include :Iterable[Component[_, O]], exclude :Iterable[Component[_, O]] = Nil)
	                                     (implicit inferS :IsBoth[M, M, Component[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, ExplicitQuery, exclude, OptionalQuery, NoQueryByDefault)

	def update[M <: Component[S, O], S, O](source :M,
	                                       include :Iterable[Component[_, O]], exclude :Iterable[Component[_, O]] = Nil)
	                                      (implicit inferS :IsBoth[M, M, Component[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, ExplicitUpdate, exclude, OptionalUpdate, NoUpdateByDefault)

	def insert[M <: Component[S, O], S, O](source :M,
	                                       include :Iterable[Component[_, O]], exclude :Iterable[Component[_, O]] = Nil)
	                                      (implicit inferS :IsBoth[M, M, Component[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, ExplicitInsert, exclude, OptionalInsert, NoInsertByDefault)



	private def customize[M <: Component[S, O], S, O]
	                     (source :M, include :Iterable[Component[_, O]], remove :BuffType,
	                      exclude :Iterable[Component[_, O]], has :BuffType, add :FlagBuffType)
		:CustomizedMapping[M, S, O] =
	{
		val included = withoutBuff(source, include, remove)
		val excluded = withBuff(source, exclude, has, add)
		new CustomizedMapping[M, S, O](source, included ++ excluded)
	}



	private def withoutBuff[O](mapping :Component[_, O], components :Iterable[Component[_, O]], remove :BuffType)
			:Substitutions[O] =
	{
		val builder = NaturalMap.newBuilder[AnyComponent[O]#Component, AnyComponent[O]#Component]
		for (component <- components)
			withoutBuff(mapping, component, remove, builder)
		builder.result()
	}

	private def withoutBuff[T, O](mapping :Component[_, O], component :Component[T, O], remove :BuffType,
	                              builder :Builder[Substitution[_, O], Substitutions[O]]) :Unit =
		mapping.lift(component) match {
			case column :ColumnMapping[_, O] =>
				if (remove.enabled(column))
					builder += new Substitution(column, column.withBuffs(column.buffs.filter(remove.disabled)))
			case lifted =>
				lifted.subcomponents foreach { sub =>
					val comp = mapping.lift(sub).asInstanceOf[Component[Any, O]]
					if (remove.enabled(comp))
						builder += new Substitution(comp, BuffedMapping(comp, comp.buffs.filter(remove.disabled) :_*))
				}

		}



	private def withBuff[O](mapping :Component[_, O], components :Iterable[Component[_, O]], has :BuffType, add :FlagBuffType)
			:Substitutions[O] =
	{
		val builder = NaturalMap.newBuilder[AnyComponent[O]#Component, AnyComponent[O]#Component]
		for (component <- components)
			withBuff(mapping, component, has, add, builder)
		builder.result()
	}

	private def withBuff[T, O](mapping :Component[_, O], component :Component[T, O], has :BuffType, add :FlagBuffType,
	                           builder :Builder[Substitution[_, O], Substitutions[O]]) :Unit =
		mapping.lift(component) match {
			case column :ColumnMapping[_, O] =>
				if (add.disabled(column) && has.enabled(column))
					builder += new Substitution(column, column.withBuffs(add[T] +: column.buffs))
			case lifted =>
				lifted.subcomponents foreach { sub =>
					val comp = mapping.lift(sub).asInstanceOf[Component[Any, O]]
					if (add.disabled(comp) && has.enabled(comp))
						builder += new Substitution[Any, O](comp, BuffedMapping(comp, add[Any] +: comp.buffs :_*))
				}

		}

}

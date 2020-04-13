package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.{-#>, Entry}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, TypedMapping}
import net.noresttherein.oldsql.schema.support.ComponentProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.Buff.{BuffType, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraSelect, FlagBuffType, NoInsertByDefault, NoQueryByDefault, NoSelectByDefault, NoUpdateByDefault, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate}
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth

import scala.collection.mutable.Builder


/**
  * @author Marcin Mościcki
  */
class CustomizedMapping[+M <: TypedMapping[S, O], S, O] private
                       (override val egg :M, substitutions :NaturalMap[MappingFrom[O]#Component, MappingFrom[O]#Component])
	extends EagerDeepProxy[M, S, O](egg) with MappingAdapter[M, S, O]
{
	override protected def adapt[T](component :egg.Component[T]) :Component[T] =
		substitutions.getOrElse(component, component)
}





object CustomizedMapping {
	private type Substitution[X, O] = Entry[MappingFrom[O]#Component, MappingFrom[O]#Component, X]
	private type Substitutions[O] = NaturalMap[MappingFrom[O]#Component, MappingFrom[O]#Component]

	def select[M <: TypedMapping[S, O], S, O](source :M,
	                                          include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
	                                         (implicit inferS :IsBoth[M, M, TypedMapping[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, ExplicitSelect, exclude, OptionalSelect, NoSelectByDefault)

	def query[M <: TypedMapping[S, O], S, O](source :M,
	                                         include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
	                                        (implicit inferS :IsBoth[M, M, TypedMapping[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, ExplicitQuery, exclude, OptionalQuery, NoQueryByDefault)

	def update[M <: TypedMapping[S, O], S, O](source :M,
	                                          include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
	                                         (implicit inferS :IsBoth[M, M, TypedMapping[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, ExplicitUpdate, exclude, OptionalUpdate, NoUpdateByDefault)

	def insert[M <: TypedMapping[S, O], S, O](source :M,
	                                          include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
	                                         (implicit inferS :IsBoth[M, M, TypedMapping[S, O]]) :CustomizedMapping[M, S, O] =
		customize(source, include, ExplicitInsert, exclude, OptionalInsert, NoInsertByDefault)



	private def customize[M <: TypedMapping[S, O], S, O]
	                     (source :M, include :Iterable[TypedMapping[_, O]], remove :BuffType,
	                      exclude :Iterable[TypedMapping[_, O]], has :BuffType, add :FlagBuffType)
		:CustomizedMapping[M, S, O] =
	{
		val included = withoutBuff(source, include, remove)
		val excluded = withBuff(source, exclude, has, add)
		new CustomizedMapping[M, S, O](source, included ++ excluded)
	}



	private def withoutBuff[O](mapping :TypedMapping[_, O], components :Iterable[TypedMapping[_, O]], remove :BuffType)
			:Substitutions[O] =
	{
		val builder = NaturalMap.newBuilder[MappingFrom[O]#Component, MappingFrom[O]#Component]
		for (component <- components)
			withoutBuff(mapping, component, remove, builder)
		builder.result()
	}

	private def withoutBuff[T, O](mapping :TypedMapping[_, O], component :TypedMapping[T, O], remove :BuffType,
	                              builder :Builder[Substitution[_, O], Substitutions[O]]) :Unit =
		mapping.export(component) match {
			case column :ColumnMapping[_, O] =>
				if (remove.enabled(column))
					builder += new Substitution(column, column.withBuffs(column.buffs.filter(remove.disabled)))
			case lifted =>
				lifted.subcomponents foreach { sub =>
					val comp = mapping.export(sub).asInstanceOf[TypedMapping[Any, O]]
					if (remove.enabled(comp))
						builder += new Substitution(comp, BuffedMapping(comp, comp.buffs.filter(remove.disabled) :_*))
				}

		}



	private def withBuff[O](mapping :TypedMapping[_, O], components :Iterable[TypedMapping[_, O]], has :BuffType, add :FlagBuffType)
			:Substitutions[O] =
	{
		val builder = NaturalMap.newBuilder[MappingFrom[O]#Component, MappingFrom[O]#Component]
		for (component <- components)
			withBuff(mapping, component, has, add, builder)
		builder.result()
	}

	private def withBuff[T, O](mapping :TypedMapping[_, O], component :TypedMapping[T, O], has :BuffType, add :FlagBuffType,
	                           builder :Builder[Substitution[_, O], Substitutions[O]]) :Unit =
		mapping.export(component) match {
			case column :ColumnMapping[_, O] =>
				if (add.disabled(column) && has.enabled(column))
					builder += new Substitution(column, column.withBuffs(add[T] +: column.buffs))
			case lifted =>
				lifted.subcomponents foreach { sub =>
					val comp = mapping.export(sub).asInstanceOf[TypedMapping[Any, O]]
					if (add.disabled(comp) && has.enabled(comp))
						builder += new Substitution[Any, O](comp, BuffedMapping(comp, add[Any] +: comp.buffs :_*))
				}

		}

}

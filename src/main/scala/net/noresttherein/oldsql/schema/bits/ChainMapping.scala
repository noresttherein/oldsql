package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{Chain, Unique}
import net.noresttherein.oldsql.collection.Chain.{~, GenericFun, MapChain, Self}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.{Buff, AbstractMapping}
import net.noresttherein.oldsql.schema.support.LazyMapping
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentSelector}

import scala.annotation.tailrec
import scala.collection.immutable.{Map, Seq}


/**
  * @author Marcin Mościcki
  */
trait ChainMapping[Components <: Chain, C <: Chain, O, S] extends LazyMapping[O, S] {
	mapping =>

	val schema :Components

	@volatile
	private[this] var selectors :Map[Component[_], ComponentSelector[this.type, Owner, S, _]] = _
	private[this] var fastSelect :Map[Component[_], ComponentSelector[this.type, Owner, S, _]] = _



	override val components :Unique[Component[_]] = Unique.Lazy {
		@tailrec def rec(chain :Chain, drop :C => Chain, res :List[Component[_]] = Nil) :Unique[Component[_]] =
			chain match {
				case t ~ (h :Component[_]) =>
					val selector = ComponentSelector(this, h)(Extractor.req {
						s :S => drop(explode(s)).asInstanceOf[Chain ~ h.Subject].head
					})
					fastSelect = fastSelect.updated(h, selector)
					rec(t, drop andThen (_.asInstanceOf[Chain~Any].tail), h::res)

				case _ ~ h =>
					throw new IllegalStateException(s"Non-component on mapping's $introString component list: $h")

				case _ =>
					selectors = fastSelect
					Unique.from(res.reverse)
			}
		fastSelect = Map()
		rec(schema, identity[C])
	}

	override val subcomponents :Unique[Component[_]] = Unique.Lazy(components.flatMap(_.subcomponents))
	override val columns :Unique[Component[_]] = Unique.Lazy(components.flatMap(_.columns))


	override def apply[T](component :Component[T]) :Selector[T] = {
		if (fastSelect == null) {
			val sync = selectors
			if (sync != null)
				fastSelect = sync
			else {
				components.iterator //trigger initialization of the components list and the selectors map at the same time.
				if (fastSelect == null)
					fastSelect = selectors
			}
		}
		fastSelect.getOrElse(component, throw new IllegalArgumentException(
			s"Mapping $component is not on the $introString mapping's component list."
		)).asInstanceOf[Selector[T]]
	}


	protected def implode(values :C) :Option[S]

	protected def explode(whole :S) :C



	protected implicit val componentValues :MapChain[Component, Components, Self, C]

	override def assemble(values :Pieces) :Option[S] =
		implode(
			schema.map(new GenericFun[Component, Self] {
				override def apply[T](x :Component[T]) :T = values(mapping(x))
			})
		)


	override def buffs :Seq[Buff[S]] = Nil

	override def toString :String = schema.toString
}






object ChainMapping {
	type ComponentOf[O] = { type T[S] = Component[O, S] }

	@inline def apply[O] :Factory[O] = new Factory[O] {}


	trait Factory[O] extends Any {
		@inline def apply[C <: Chain, S <: Chain](componentChain :C)
		                                         (implicit values :MapChain[ComponentOf[O]#T, C, Self, S])
				:ChainMapping[C, S, O, S] =
			apply[C, S, S](componentChain, Some(_:S), identity[S])

		@inline def apply[C <: Chain, T <: Chain, S](componentChain :C, fromChain :T => Option[S], toChain :S => T)
		                                            (implicit values :MapChain[ComponentOf[O]#T, C, Self, T])
				:ChainMapping[C, T, O, S] =
			new ChainMapping[C, T, O, S] {
				override protected implicit val componentValues = values

				override val schema = componentChain

				override protected def implode(values :T) :Option[S] = fromChain(values)
				override protected def explode(whole :S) :T = toChain(whole)
			}
	}
}
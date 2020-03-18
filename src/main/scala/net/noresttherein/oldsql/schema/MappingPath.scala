package net.noresttherein.oldsql.schema
/*
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, ComponentFor}
import net.noresttherein.oldsql.schema.MappingPath.\~\






trait MappingPath[X <: ComponentFor[S], Y <: Component[O, T], O, S, T] extends Extractor[S, T] {
	type Subject = S //X#Subjec
	type Target = T //ject
	type Owner = O
	val end :Y

	def \[Z <: Mapping](next :Y \~\ Z) :X \~\ Z = next.requisite match {
		case Some(f) => ???


	}

	def \[Z <: AnyComponent[Owner]](component :Z) :X \~\ Z = ???
//		this \ end(component)

	def :\(component :AnyComponent[Owner]) :X \~\ component.type = \(component :component.type)
}





object MappingPath {
	type \~\[X <: Mapping { type Subject = S } forSome { type S }, Y <: Mapping { type Owner = O; type Subject = S } forSome { type O; type S }] = MappingPath[X, Y, Y#Owner, X#Subject, Y#Subject]
	type \-\[X <: Mapping { type Owner = O; type Subject = S } forSome { type O; type S }, Y <: Component[X#Owner, _]] = ComponentPath[X, Y, Y#Owner,  X#Subject, Y#Subject]

//	type TypedPath[X <: ComponentFor[S], S, Y <: ComponentFor[T], T] = MappingPath[X, Y]




	def apply[X <: Component[_, _], Y <: Component[_, _]](parent :X, component :Y) :X \~\ Y = ???
//		new MappingPath[X, Y] {
//			override val end = component
//			override def toString = parent.toString + " \\~\\ " + end
//		}

	def apply[X <: Component[O, S] forSome { type S }, Y <: Component[O, _], O](parent :X, component :Y) :X \-\ Y = ???
//		new ComponentPath[X, Y] {
//			override val end = component
//			override def toString = parent.toString + " \\-\\ " + end
//		}



	trait ComponentPath[X <: Component[O, S], Y <: Component[O, T], O, S, T] extends MappingPath[X, Y, O, S, T] {
//		type Owner = Y#Owner

		def apply(pieces :ComponentValues[X]) :T = ??? //pieces(this)

		def get(pieces :ComponentValues[X]) :Option[T] = ??? //pieces.get(this)


		def \[Z <: Mapping](next :Y \-\ Z) :X \-\ Z

		override def \[Z <: AnyComponent[Owner]](component :Z) :X \-\ Z = ??? //this \ end(component)

		override def :\(component :AnyComponent[Owner]) :X \-\ component.type = \(component :component.type)

	}



	object ComponentPath {
		def apply[O](parent :AnyComponent[O], component :AnyComponent[O]) :parent.type \-\ component.type = ???
	}



}
*/

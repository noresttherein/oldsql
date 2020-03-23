package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, ComponentFor}
import net.noresttherein.oldsql.schema.MappingPath.ComponentPath






//trait MappingPath[X <: ComponentFor[S], Y <: Component[O, T], O, S, T] extends Extractor[S, T] {
trait MappingPath[X <: Mapping, Y <: AnyComponent[O], O] { self =>
	type Start = X //<: Mapping //<: ComponentFor[Subject] //ComponentFor[S]
	type End = Y //<: Mapping //AnyComponent[Owner] //<: Component[Owner, Target] //ComponentFor[T]
	type Subject = Start#Subject
	type Target = end.Subject //End#Subject //= end.Subject
	type Owner = O //Y#Owner//O //= end.Owner

	val end :End

	def extractor :Extractor[Subject, Target]

	def \[Z <: AnyComponent[P], P](next :MappingPath[Y, Z, P]) :MappingPath[X, Z, P] = ??? //Start \~\ Z = ??? //new ConcatPath[Start, End, Z](this, next)
//		MappingPath[Start, Z](next.end)(extractor andThen next.extractor)

	def \[Z <: AnyComponent[O]](next :ComponentPath[Y, Z, O]) :MappingPath[X, Z, O] //Start \~\ Z = ???

	def \[Z <: Component[O, _]](component :Z) :MappingPath[X, Z, O] //Start \~\ Z //= \(new Mapping.PathComponent(end) \# component)
//		this \ end(component)

	def :\(component :Component[Owner, _]) :MappingPath[X, component.type, O] = \(component :component.type)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[MappingPath[_, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :MappingPath[ _, _, _] if other.canEqual(this)  && canEqual(other) =>
			other.end == end && other.extractor == extractor
		case _ => false
	}

	override def hashCode :Int = end.hashCode * 31 + extractor.hashCode

	override def toString :String = "\\~\\ " + end

}






object MappingPath {
//	type \~\[X <: Mapping, Y <: AnyComponent[_]] = MappingPath[X, Y]
//	type \#[X <: AnyComponent[_], Y <: AnyComponent[X#Owner]] = ComponentPath[X, Y]
//	type \~\[X <: Mapping, Y <: Mapping] = MappingPath { type Start = X; type End = Y }
//	type \#[X <: AnyComponent[Y#Owner], Y <: Mapping] = ComponentPath { type Start = X; type End = Y }
//	type \~\[X <: Mapping, Y <: AnyComponent[_]] = MappingPath[X, Y, Y#Owner]
//	type \#[X <: Mapping, Y <: AnyComponent[X#Owner]] = ComponentPath[X, Y, X#Owner]


//	def apply[X <: Mapping, Y <: Mapping](component :Y)(extract :X#Subject =?> Y#Subject) :X \~\ Y = ???
/*
	def apply[X <: ComponentFor[S], S, Y <: ComponentFor[T], T](component :Y, extract :S =?> T) :X \~\ Y =
		new MappingPath {
			type Start = X
			type End = Y
//			type Subject = S
//			type Target = T
//			type Owner = end.Owner
			val end = component
			val extractor = extract
		}
*/

//	def apply[X <: Mapping, Y <: Mapping](parent :X, component :Y) :X \~\ Y = ???
//		new MappingPath[X, Y] {
//			override val end = component
//			override def toString = parent.toString + " \\~\\ " + end
//		}

//	def apply[X <: AnyComponent[Y#Owner], Y <: Mapping, O](parent :X, component :Y) :X \# Y = ???
//		new ComponentPath[X, Y] {
//			override val end = component
//			override def toString = parent.toString + " \\-\\ " + end
//		}



	trait ComponentPath[X <: AnyComponent[O], Y <: AnyComponent[O], O] extends MappingPath[X, Y, O] {
//		type Start <: AnyComponent[Owner]
//		type End <: AnyComponent[Owner]

/*
		override def \[Z <: Mapping](next :End \~\ Z) :Start \~\ Z = next match {
			case comp :ComponentPath => this \ comp.asInstanceOf[End \# Z]
			case _ => super.\(next)
		}
*/

		override def \[Z <: AnyComponent[Owner]](next :ComponentPath[Y, Z, O]) :ComponentPath[X, Z, O] =
			ComponentPath[X, Z, O](next.end)(extractor andThen next.extractor)
//			ComponentPath[Start, Z, Owner](next.end)(extractor andThen next.extractor)

		override def \[Z <: Component[Owner, _]](component :Z) :ComponentPath[X, Z, O] = //Start \# Z = ??? //=
			???
//			ComponentPath[X, Z, O](component)(extractor andThen end(component))
//			\(new Mapping.PathComponent(end) \# component)
//			this \ (new Mapping.PathComponent(end) \# component)
//			this \ (end \-\ component)

		override def :\(component :Component[Owner, _]) :ComponentPath[X, component.type, O]  = //Start \# component.type = \(component :component.type)
			this \[component.type] (component :component.type)



		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentPath[_, _, _]]

		override def toString :String = "\\-\\ " + end
	}



	object ComponentPath {
		def apply[X <: AnyComponent[O], Y <: AnyComponent[O], O](component :Y)(extract :X#Subject =?> component.Subject) :ComponentPath[X, Y, O] =
			new ComponentPath[X, Y, O] {
//				override type Start = X
//				override type End = Y
//				override type Owner = O

				override val end :component.type = component
				override def extractor = extract

//				override def \[Z <: AnyComponent[Owner]](component :Z) :Start \# Z = ???
//					\(end \-\ component)

			}

//		def apply[O](parent :AnyComponent[O], component :AnyComponent[O]) :parent.type \# component.type = ???
	}



	trait SelfPath[X <: AnyComponent[O], O] extends ComponentPath[X, X, O] {
		override def toString :String = end.toString
	}



	object SelfPath {
		def apply[X <: AnyComponent[O], O](mapping :X) :SelfPath[X, O] = new SelfPath[X, O] {
			override val end = mapping
			override def extractor = ??? //Extractor.ident[X#Subject]
		}
	}
	/*
		private[MappingPath] class ConcatPath[W <: ComponentFor[_], X <: ComponentFor[_], Y <: Component[_, _]]
											 (first :W \~\ X, second :X \~\ Y)
			extends MappingPath
		{
			override type Start = W
			override type End = Y
			override type Owner = Y#Owner

			override val end :Y = second.end
			override val extractor = first.extractor andThen second.extractor :W#Subject =?> Y#Subject

			override def \[Z <: AnyComponent[Owner]](component :Z) :W \~\ Z = second match {
				case _ :ComponentPath => new ConcatPath[W, X, Z](first, second \ component)
				case _ => new ConcatPath[W, Y, Z](this, ComponentPath[Y, Z, Owner](component)(second.end \# component))
			}
		}
	*/
}

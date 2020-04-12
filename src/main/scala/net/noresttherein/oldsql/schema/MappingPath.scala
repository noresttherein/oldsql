package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, TypedMapping}
import net.noresttherein.oldsql.schema.MappingPath.{ComponentPath, ConcatPath, SelfPath}
import net.noresttherein.oldsql.slang.InferTypeParams
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth






trait MappingPath[-X <: TypedMapping[S], +Y <: Component[T, O], S, T, O] { self =>
	val end :Y
	def extractor :Extractor[S, T]

	def \[Z <: Component[U, P], U, P](next :MappingPath[Y, Z, T, U, P]) :MappingPath[X, Z, S, U, P] = next match {
		case _ :SelfPath[_, _, _] =>
			if (end != next.end)
				throw new IllegalArgumentException(s"Can't append self-path $next to $this as refers to a different mapping.")
			this.asInstanceOf[MappingPath[X, Z, S, U, P]]
		case _ => new ConcatPath(this, next)
	}

	def \[Z <: Component[U, O], U](next :ComponentPath[Y, Z, T, U, O]) :MappingPath[X, Z, S, U, O] =
		\(next :MappingPath[Y, Z, T, U, O])

	def \[M <: Mapping, Z <: Component[U, O], U]
	     (component :M)(implicit hint :IsBoth[M, Z, Component[U, O]]) :MappingPath[X, Z, S, U, O] =
		this \ (end \ component)

	def :\[U](component :Component[U, O]) :MappingPath[X, component.type, S, U, O] =
		\[component.type, component.type, U](component :component.type)(IsBoth[component.type])



	def canEqual(that :Any) :Boolean = that.isInstanceOf[MappingPath[_, _, _, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :MappingPath[_, _, _, _, _] if (other canEqual this) && canEqual(other) =>
			end == other.end && extractor == other.extractor
		case _ => false
	}
	
	override def hashCode :Int = end.hashCode * 31 + extractor.hashCode

	override def toString :String = "./*/" + end
	
}






object MappingPath {

//	private def typed[X <: TypedMapping[S], Y <: Component[O, T], O, S, T](component :Y)(extract :S =?> T) :MappingPath[X, Y, O, S, T] =
//		new MappingPath[X, Y, O, S, T] {
//			override val end = component
//			override val extractor = extract
//		}

	
	
	trait ComponentPath[-X <: Component[S, O], +Y <: Component[T, O], S, T, O] extends MappingPath[X, Y, S, T, O] {

		override def \[Z <: Component[U, P], U, P](next :MappingPath[Y, Z, T, U, P]) :MappingPath[X, Z, S, U, P] =
			next match {
				case _ :SelfPath[_, _, _] =>
					if (end != next.end)
						throw new IllegalArgumentException(s"Can't extend $this with self-path $next as it refers to a different mapping")
					this.asInstanceOf[MappingPath[X, Z, S, U, P]]
				case _ :ComponentPath[_, _, _, _, _] =>
					ComponentPath.typed[X, Component[U, O], S, U, O](
						next.end.asInstanceOf[Component[U, O]])(extractor andThen next.extractor
					).asInstanceOf[MappingPath[X, Z, S, U, P]]
				case _ => super.\(next)
			}


		override def \[Z <: Component[U, O], U](next :ComponentPath[Y, Z, T, U, O]) :ComponentPath[X, Z, S, U, O] =
			next match {
				case _ :SelfPath[_, _, _] =>
					if (end != next.end)
						throw new IllegalArgumentException(s"Can't extend $this with self-path $next as it refers to a different mapping")
					this.asInstanceOf[ComponentPath[X, Z, S, U, O]]
				case _ =>
					ComponentPath.typed[X, Z, S, U, O](next.end)(extractor andThen next.extractor)
			}


		override def \[M <: Mapping, Z <: Component[U, O], U]
		             (component :M)(implicit hint :IsBoth[M, Z, Component[U, O]]) :ComponentPath[X, Z, S, U, O] =
		{
			val c = hint(component)
			ComponentPath.typed[X, Z, S, U, O](c)(extractor andThen end(c))
		}

		override def :\[U](component :Component[U, O]) :ComponentPath[X, component.type, S, U, O] =
			\[component.type, component.type, U](component :component.type)(IsBoth[component.type])


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentPath[_, _, _, _, _]]

		override def toString :String = "./" + end

	}

	

	object ComponentPath {

		def apply[P <: Mapping, X <: Component[S, O], C <: Mapping, Y <: Component[T, O], S, T, O]
		         (parent :P, component :C)(implicit parentType :IsBoth[P, X, Component[S, O]], childType :IsBoth[C, Y, Component[O, T]])
			:ComponentPath[X, Y, S, T, O] =
		{ 
			val c = childType(component)
			typed[X, Y, S, T, O](c)(parentType(parent)(c))
		}

//		def apply[M <: Mapping, X <: Component[O, S], Y <: Component[O, T], O, S, T]
//		         (component :M)(extractor :S =?> T)(implicit hint :IsBoth[M, Y, Component[O, T]]) :ComponentPath[X, Y, O, S, T] =
//			typed[X, Y, O, S, T](hint(component).left)(extractor)

		private[MappingPath] def typed[X <: Component[S, O], Y <: Component[T, O], S, T, O]
		                              (component :Y)(extract :S =?> T) :ComponentPath[X, Y, S, T, O] =
			new ComponentPath[X, Y, S, T, O] {
				override val end = component
				override def extractor = extract
			}



		def unapply[X <: TypedMapping[S], Y <: Component[T, O], S, T, O](path :MappingPath[X, Y, S, T, O]) :Option[Y] =
			path match {
				case _ :ComponentPath[_, _, _, _, _] => Some(path.end)
				case _ => None
			}

	}


	trait SelfPath[X <: Component[S, O], S, O] extends ComponentPath[X, X, S, S, O] {

		override def extractor :RequisiteExtractor[S, S] = Extractor.ident[S]


		override def \[Z <: Component[U, P], U, P](next :MappingPath[X, Z, S, U, P]) :MappingPath[X, Z, S, U, P] =
			next

		override def \[Z <: Component[U, O], U](next :ComponentPath[X, Z, S, U, O]) :ComponentPath[X, Z, S, U, O] =
			next

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelfPath[_, _, _]]

		override def toString :String = ".(" + end + ")"
	}




	object SelfPath {
		@inline 
		implicit def apply[M, X <: Component[S, O], S, O]
		                  (mapping :M)(implicit typeHint :IsBoth[M, X, Component[S, O]]) :SelfPath[X, S, O] =
			typed[X, S, O](mapping)

		def typed[X <: Component[S, O], S, O](mapping :X) :SelfPath[X, S, O] = new SelfPath[X, S, O] {
			override val end = mapping
		}


		def unapply[X <: TypedMapping[S], Y <: Component[T, O], S, T, O](path :MappingPath[X, Y, S, T, O]) :Option[X] =
			path match {
				case self :SelfPath[_, _, _] => Some(self.end.asInstanceOf[X])
				case _ => None
			}
	}






	private[MappingPath] class ConcatPath[W <: TypedMapping[R], X <: Component[S, N], Y <: Component[T, O], R, S, T, N, O]
										 (first :MappingPath[W, X, R, S, N], second :MappingPath[X, Y, S, T, O])
		extends MappingPath[W, Y, R, T, O]
	{
		override val end :Y = second.end
		override val extractor : R =?> T = first.extractor andThen second.extractor

		override def \[Z <: Component[U, P], U, P](next :MappingPath[Y, Z, T, U, P]) :MappingPath[W, Z, R, U, P] =
			next match {
				case comp :ComponentPath[_, _, _, _, _] if second.isInstanceOf[ComponentPath[_, _, _, _, _]] =>
					new ConcatPath[W, Component[S, P], Z, R, S, U, P, P](
						first.asInstanceOf[MappingPath[W, Component[S, P], R, S, P]],
						second.asInstanceOf[ComponentPath[Component[S, P], Component[T, P], S, T, P]] \
							comp.asInstanceOf[ComponentPath[Component[T, P], Z, T, U, P]]
					)
				case _ =>
					super.\(next)
			}

		override def toString :String = first.toString + second.toString
	}


}

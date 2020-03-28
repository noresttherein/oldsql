package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, TypedMapping}
import net.noresttherein.oldsql.schema.MappingPath.{ComponentPath, ConcatPath, SelfPath}
import net.noresttherein.oldsql.slang.InferTypeParams
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth






trait MappingPath[-X <: TypedMapping[S], +Y <: Component[O, T], O, S, T] { self =>
	val end :Y
	def extractor :Extractor[S, T]

	def \[Z <: Component[P, U], P, U](next :MappingPath[Y, Z, P, T, U]) :MappingPath[X, Z, P, S, U] = next match {
		case _ :SelfPath[_, _, _] =>
			if (end != next.end)
				throw new IllegalArgumentException(s"Can't append self-path $next to $this as refers to a different mapping.")
			this.asInstanceOf[MappingPath[X, Z, P, S, U]]
		case _ => new ConcatPath(this, next)
	}

	def \[Z <: Component[O, U], U](next :ComponentPath[Y, Z, O, T, U]) :MappingPath[X, Z, O, S, U] =
		\(next :MappingPath[Y, Z, O, T, U])

	def \[M <: Mapping, Z <: Component[O, U], U]
	     (component :M)(implicit hint :IsBoth[M, Z, Component[O, U]]) :MappingPath[X, Z, O, S, U] =
		this \ (end \ component)

	def :\[U](component :Component[O, U]) :MappingPath[X, component.type, O, S, U] =
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

	
	
	trait ComponentPath[-X <: Component[O, S], +Y <: Component[O, T], O, S, T] extends MappingPath[X, Y, O, S, T] {

		override def \[Z <: Component[P, U], P, U](next :MappingPath[Y, Z, P, T, U]) :MappingPath[X, Z, P, S, U] =
			next match {
				case _ :SelfPath[_, _, _] =>
					if (end != next.end)
						throw new IllegalArgumentException(s"Can't extend $this with self-path $next as it refers to a different mapping")
					this.asInstanceOf[MappingPath[X, Z, P, S, U]]
				case _ :ComponentPath[_, _, _, _, _] =>
					ComponentPath.typed[X, Component[O, U], O, S, U](
						next.end.asInstanceOf[Component[O, U]])(extractor andThen next.extractor
					).asInstanceOf[MappingPath[X, Z, P, S, U]]
				case _ => super.\(next)
			}


		override def \[Z <: Component[O, U], U](next :ComponentPath[Y, Z, O, T, U]) :ComponentPath[X, Z, O, S, U] =
			next match {
				case _ :SelfPath[_, _, _] =>
					if (end != next.end)
						throw new IllegalArgumentException(s"Can't extend $this with self-path $next as it refers to a different mapping")
					this.asInstanceOf[ComponentPath[X, Z, O, S, U]]
				case _ =>
					ComponentPath.typed[X, Z, O, S, U](next.end)(extractor andThen next.extractor)
			}


		override def \[M <: Mapping, Z <: Component[O, U], U]
		             (component :M)(implicit hint :IsBoth[M, Z, Component[O, U]]) :ComponentPath[X, Z, O, S, U] =
		{
			val c = hint(component)
			ComponentPath.typed[X, Z, O, S, U](c)(extractor andThen end(c))
		}

		override def :\[U](component :Component[O, U]) :ComponentPath[X, component.type, O, S, U] =
			\[component.type, component.type, U](component :component.type)(IsBoth[component.type])


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentPath[_, _, _, _, _]]

		override def toString :String = "./" + end

	}

	

	object ComponentPath {

		def apply[P <: Mapping, X <: Component[O, S], C <: Mapping, Y <: Component[O, T], O, S, T]
		         (parent :P, component :C)(implicit parentType :IsBoth[P, X, Component[O, S]], childType :IsBoth[C, Y, Component[O, T]])
			:ComponentPath[X, Y, O, S, T] =
		{ 
			val c = childType(component)
			typed[X, Y, O, S, T](c)(parentType(parent)(c))
		}

//		def apply[M <: Mapping, X <: Component[O, S], Y <: Component[O, T], O, S, T]
//		         (component :M)(extractor :S =?> T)(implicit hint :IsBoth[M, Y, Component[O, T]]) :ComponentPath[X, Y, O, S, T] =
//			typed[X, Y, O, S, T](hint(component).left)(extractor)

		private[MappingPath] def typed[X <: Component[O, S], Y <: Component[O, T], O, S, T]
		                              (component :Y)(extract :S =?> T) :ComponentPath[X, Y, O, S, T] =
			new ComponentPath[X, Y, O, S, T] {
				override val end = component
				override def extractor = extract
			}



		def unapply[X <: TypedMapping[S], Y <: Component[O, T], O, S, T](path :MappingPath[X, Y, O, S, T]) :Option[Y] =
			path match {
				case _ :ComponentPath[_, _, _, _, _] => Some(path.end)
				case _ => None
			}

	}



	trait SelfPath[X <: Component[O, S], O, S] extends ComponentPath[X, X, O, S, S] {
		override def extractor :RequisiteExtractor[S, S] = Extractor.ident[S]


		override def \[Z <: Component[P, U], P, U](next :MappingPath[X, Z, P, S, U]) :MappingPath[X, Z, P, S, U] =
			next

		override def \[Z <: Component[O, U], U](next :ComponentPath[X, Z, O, S, U]) :ComponentPath[X, Z, O, S, U] =
			next

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelfPath[_, _, _]]

		override def toString :String = ".(" + end + ")"
	}




	object SelfPath {
		@inline 
		implicit def apply[M, X <: Component[O, S], O, S]
		                  (mapping :M)(implicit typeHint :IsBoth[M, X, Component[O, S]]) :SelfPath[X, O, S] =
			typed[X, O, S](mapping)

		def typed[X <: Component[O, S], O, S](mapping :X) :SelfPath[X, O, S] = new SelfPath[X, O, S] {
			override val end = mapping
		}


		def unapply[X <: TypedMapping[S], Y <: Component[O, T], O, S, T](path :MappingPath[X, Y, O, S, T]) :Option[X] =
			path match {
				case self :SelfPath[_, _, _] => Some(self.end.asInstanceOf[X])
				case _ => None
			}
	}






	private[MappingPath] class ConcatPath[W <: TypedMapping[R], X <: Component[N, S], Y <: Component[O, T], N, O, R, S, T]
										 (first :MappingPath[W, X, N, R, S], second :MappingPath[X, Y, O, S, T])
		extends MappingPath[W, Y, O, R, T]
	{
		override val end :Y = second.end
		override val extractor : R =?> T = first.extractor andThen second.extractor

		override def \[Z <: Component[P, U], P, U](next :MappingPath[Y, Z, P, T, U]) :MappingPath[W, Z, P, R, U] =
			next match {
				case comp :ComponentPath[_, _, _, _, _] if second.isInstanceOf[ComponentPath[_, _, _, _, _]] =>
					new ConcatPath[W, Component[P, S], Z, P, P, R, S, U](
						first.asInstanceOf[MappingPath[W, Component[P, S], P, R, S]],
						second.asInstanceOf[ComponentPath[Component[P, S], Component[P, T], P, S, T]] \ comp.asInstanceOf[ComponentPath[Component[P, T], Z, P, T, U]]
					)
				case _ =>
					super.\(next)
			}

		override def toString :String = first.toString + second.toString
	}


}

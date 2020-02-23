package com.hcore.ogre.mapping

import com.hcore.ogre.mapping.MappingMorphism.ValueMorphism.ValueHomomorphism
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.Mapping.{TypedMapping, CompatibleMapping, ComponentCompatibleMapping}
import com.hcore.ogre.slang.SaferCasts


//implicits

import SaferCasts._


trait MappingMorphism[X<:AnyMapping, Y<:AnyMapping] {
	def source :X
	def target :Y

	@inline
	final def andThen[Z<:AnyMapping](other :MappingMorphism[Y, Z]) :MappingMorphism[X, Z] =
		MappingMorphism[X, Z](source, other.target, value andThen other.value, other.components andThen components)

	@inline
	final def compose[Z<:AnyMapping](other :MappingMorphism[Z, X]) :MappingMorphism[Z, Y] =
		MappingMorphism[Z, Y](other.source, target, other.value andThen value, components andThen other.components)
	
	def value :ValueMorphism[X#ResultType, Y#ResultType]
	def components :ComponentMorphism[Y#Component, X#Component]


	def to[Z<:CompatibleMapping[Y]](end :Z) :MappingMorphism[X, Z] =
		MappingMorphism[X, Z](source, end, value, components.asInstanceOf[ComponentMorphism[Z#Component, X#Component]])
	
	def from[Z<:CompatibleMapping[X]](start :Z) :MappingMorphism[Z, Y] =
		MappingMorphism[Z, Y](start, target, value, components.asInstanceOf[ComponentMorphism[Y#Component, Z#Component]])

	def as[A<:CompatibleMapping[X], B<:CompatibleMapping[Y]](start :A, end :B) :MappingMorphism[A, B] =
		MappingMorphism[A, B](start, end, value, components.asInstanceOf[ComponentMorphism[B#Component, A#Component]])
	
}




object MappingMorphism {

	def apply[X<:AnyMapping, Y<:AnyMapping](start :X, end :Y, result :ValueMorphism[X#ResultType, Y#ResultType], structure :ComponentMorphism[Y#Component, X#Component]) =
		new MappingMorphism[X, Y] {
			def source = start
			def target = end
			val value = result
			val components = structure
		}

	def identity[X<:AnyMapping, Y<:CompatibleMapping[X]](start :X, end :Y) :MappingMorphism[X, Y] =
		new MappingMorphism[X, Y] {
			val source = start
			val target = end
			def value = ValueMorphism.identity[X#ResultType]
			def components = ComponentMorphism.supertype[Y#Component, X#Component]
		}

	def isomorphism[X<:AnyMapping, Y<:ComponentCompatibleMapping[X]](start :X, end :Y, result :ValueMorphism[X#ResultType, Y#ResultType]) :MappingMorphism[X, Y] =
		apply(start, end, result, ComponentMorphism.supertype[Y#Component, X#Component])


	trait ValueMorphism[-X, +Y] {
		def function :X=>Option[Y] = apply
		def apply(x :X) :Option[Y]
		def andThen[Z](morphism :ValueMorphism[Y, Z]) :ValueMorphism[X, Z]
		def compose[Z](morphism :ValueMorphism[Z, X]) :ValueMorphism[Z, Y]
		def isHomomorphism :Boolean = false
		def ifHomomorphism[T](fun :ValueHomomorphism[X, Y]=>T) :Option[T] = None
		def flatMap[T](fun :Y=>T)(x :X) :Option[T] = function(x).map(fun)
	}
	
	
	object ValueMorphism {
		@inline
		def apply[X, Y](fun :X=>Option[Y]) :ValueMorphism[X, Y] = new ValueMorphism[X, Y] {
			override val function = fun

			override def apply(x: X): Option[Y] = function(x)
			
			override def andThen[Z](morphism: ValueMorphism[Y, Z]): ValueMorphism[X, Z] =
				morphism.ifSubclass[ValueHomomorphism[Y, Z]].orElse {
					_ compose this
				} {
					ValueMorphism[X, Z](function(_).flatMap(morphism(_)))
				}

			override def compose[Z](morphism: ValueMorphism[Z, X]): ValueMorphism[Z, Y] =
				morphism.ifSubclass[ValueHomomorphism[Z, X]].orElse {
					_ andThen this
				} {
					ValueMorphism[Z, Y](morphism(_).flatMap(function))
				}
		}
		
		@inline
		def homomorphism[X, Y](fun :X=>Y) :ValueHomomorphism[X, Y] = new ValueHomomorphism[X, Y] {
			val map = fun
		}

		object identity {
			@inline
			def apply[X]: ValueIdentity[X] = id.asInstanceOf[ValueIdentity[X]]

			def unapply(value :ValueMorphism[_, _]) :Boolean = value.isInstanceOf[ValueIdentity[_]]

			def unapply(morphism :MappingMorphism[_, _]) :Boolean = unapply(morphism.value)
		}

		@inline
		def none[X, Y] = nothing.asInstanceOf[ValueMorphism[X, Y]]
		
		abstract class ValueHomomorphism[-X, +Y] extends ValueMorphism[X, Y] {
			def apply(x :X) = Some(map(x))
			val map :X=>Y


			override def isHomomorphism: Boolean = true

			override def ifHomomorphism[T](fun: ValueHomomorphism[X, Y] => T): Option[T] = Some(fun(this))

			override def andThen[Z](morphism: ValueMorphism[Y, Z]): ValueMorphism[X, Z] =
				morphism.ifSubclassOf[ValueIdentity[_]] {
					_ => this.asInstanceOf[ValueMorphism[X, Z]] 
				} orElse morphism.ifSubclass[ValueHomomorphism[Y, Z]] { 
					mono => homomorphism[X, Z](x => mono.map(map(x))) 
				} orElse morphism.ifSubclassOf[NoValue[Y, Z]] {
					_.asInstanceOf[ValueMorphism[X, Z]]
				} getOrElse
					ValueMorphism[X, Z](x => morphism(map(x)))

			override def compose[Z](morphism: ValueMorphism[Z, X]): ValueMorphism[Z, Y] = 
				morphism.ifSubclassOf[ValueIdentity[_]] {
					_ => this.asInstanceOf[ValueMorphism[Z, Y]]
				} orElse morphism.ifSubclass[ValueHomomorphism[Z, X]] {
					mono => homomorphism[Z, Y](z => map(mono.map(z)))
				} orElse morphism.ifSubclassOf[NoValue[Y, Z]] {
					_.asInstanceOf[ValueMorphism[Z, Y]]
				} getOrElse
					ValueMorphism[Z, Y](morphism(_).map(map))
		}
		
		
		class ValueIdentity[X] private[ValueMorphism]() extends ValueHomomorphism[X, X] {
			@inline
			override def apply(x :X) = Some(x)
			val map = Predef.identity[X] _

			override def andThen[Z](morphism: ValueMorphism[X, Z]): ValueMorphism[X, Z] = morphism
			override def compose[Z](morphism: ValueMorphism[Z, X]): ValueMorphism[Z, X] = morphism
		}

		class NoValue[-X, +Y] extends ValueMorphism[X, Y] {
			override def apply(x: X): Option[Y] = None
			override def andThen[Z](morphism: ValueMorphism[Y, Z]): ValueMorphism[X, Z] = this.asInstanceOf[ValueMorphism[X, Z]]
			override def compose[Z](morphism: ValueMorphism[Z, X]): ValueMorphism[Z, Y] = this.asInstanceOf[ValueMorphism[Z, Y]]
		}

		private val id = new ValueIdentity[Any]
		private val nothing = new NoValue[Any, Any]
	}



	
	
	
	

	trait ComponentMorphism[-X[T]<:TypedMapping[T], +Y[T]<:TypedMapping[T]] {
		def function :X[_]=>Option[Y[_]] = apply(_)
		
		def apply[T](x :X[T]) :Option[Y[T]]
		
		def andThen[Z[T]<:TypedMapping[T]](morphism :ComponentMorphism[Y, Z]) :ComponentMorphism[X, Z]
		def compose[Z[T]<:TypedMapping[T]](morphism :ComponentMorphism[Z, X]) :ComponentMorphism[Z, Y]

//		def from[M[T]>:TypedMapping[T]] = this.asInstanceOf[ComponentMorphism[M, Y]]
//		def to[M[T]<:TypedMapping[T]] = this.asInstanceOf
//		def as[A<:ComponentCompatibleMapping[X], B<:ComponentCompatibleMapping[Y]] = this.asInstanceOf[ComponentMorphism[A, B]]

//		def from[Z<:ComponentCompatibleMapping[X]] = this.asInstanceOf[ComponentMorphism[Z, Y]]

//		def to[Z<:ComponentCompatibleMapping[Y]] = this.asInstanceOf[ComponentMorphism[X, Z]]
	}


	object ComponentMorphism { components =>
		@inline
		def apply[X[T]<:TypedMapping[T], Y[T]<:TypedMapping[T]](fun :X[_]=>Option[Y[_]]) :ComponentMorphism[X, Y] =
			new BaseComponentMorphism[X, Y](fun)


		def between[X<:AnyMapping, Y<:AnyMapping](fun :X#Component[_]=>Option[Y#Component[_]]) :ComponentMorphism[X#Component, Y#Component] =
			apply[X#Component, Y#Component](fun)

		@inline
		def homomorphism[X[T]<:TypedMapping[T], Y[T]<:TypedMapping[T]](fun :X[_] => Y[_]) :ComponentHomomorphism[X, Y] =
			new ComponentHomomorphism[X, Y] {
				val homomorphism = fun
			}

		@inline
		def cached[X[T]<:TypedMapping[T], Y[T]<:TypedMapping[T]](components :Iterable[X[_]], adapter :X[_]=>Y[_]) :ComponentHomomorphism[X, Y] =
			new ComponentHomomorphism[X, Y] {
				val homomorphism = components.map(c => c -> adapter(c)).toMap[X[_], Y[_]].withDefault(adapter)
			}

		@inline
		def inverse[X<:AnyMapping, Y<:AnyMapping](components :Iterable[Y#Component[_]], adapter :Y#Component[_]=>X#Component[_]) :ComponentHomomorphism[X#Component, Y#Component] =
			new ComponentHomomorphism[X#Component, Y#Component] {
				val homomorphism = components.map(c=> adapter(c) -> c).toMap[X#Component[_], Y#Component[_]]
			}

		@inline
//		def identity[X<:AnyMapping, Y<:ComponentCompatibleMapping[X]] :ComponentMorphism[X, Y] = id.asInstanceOf[ComponentIdentity[X, Y]]
		def identity[X[T]<:TypedMapping[T]] :ComponentMorphism[X, X] = id.asInstanceOf[ComponentIdentity[X, X]]
		
		@inline
		def supertype[X[T]<:Y[T], Y[T]<:TypedMapping[T]] :ComponentMorphism[X, Y] = id.asInstanceOf[ComponentIdentity[X, Y]]

		@inline
		def empty[X[T]<:TypedMapping[T]] :ComponentMorphism[Mapping, X] = nothing.asInstanceOf[Empty[X]]


		class BaseComponentMorphism[-X[T]<:TypedMapping[T], +Y[T]<:TypedMapping[T]](fun :X[_]=>Option[Y[_]]) extends ComponentMorphism[X, Y] {
			@inline
			override def apply[T](x: X[T]): Option[Y[T]] = function(x).crosstyped[Y[T]]
			
			override val function = fun

			override def andThen[Z[T]<:TypedMapping[T]](morphism: ComponentMorphism[Y, Z]): ComponentMorphism[X, Z] =
				morphism.ifSubclass[ComponentHomomorphism[Y, Z]].orElse {
					_ compose this
				}  {
					ComponentMorphism[X, Z](apply(_).flatMap(morphism(_)))
				}

			override def compose[Z[T]<:TypedMapping[T]](morphism: ComponentMorphism[Z, X]): ComponentMorphism[Z, Y] =
				morphism.ifSubclass[ComponentHomomorphism[Z, X]].orElse {
					_ andThen this
				} {
					ComponentMorphism[Z, Y](morphism(_).flatMap(fun))
				}
		}





		trait ComponentHomomorphism[-X[T]<:TypedMapping[T], +Y[T]<:TypedMapping[T]] extends ComponentMorphism[X, Y] {
			@inline
			def apply[T](x :X[T]) = Some(homomorphism(x).asInstanceOf[Y[T]])

			@inline
			def map[T](x :X[T]) = homomorphism(x).asInstanceOf[Y[T]]
			
			private[ComponentMorphism] val homomorphism :X[_]=>Y[_]
			

			override def andThen[Z[T]<:TypedMapping[T]](other: ComponentMorphism[Y, Z]): ComponentMorphism[X, Z] =
				other.ifSubclassOf[ComponentIdentity[X, Y] forSome { type X[T]; type Y[T] }] {
					_ => this.asInstanceOf[ComponentMorphism[X, Z]]
				} orElse other.ifSubclass[ComponentHomomorphism[Y, Z]] {
					mono => components.homomorphism[X, Z](x => mono.homomorphism(homomorphism(x)))
				} getOrElse
					ComponentMorphism[X, Z](x => other(homomorphism(x)))

			override def compose[Z[T]<:TypedMapping[T]](other: ComponentMorphism[Z, X]): ComponentMorphism[Z, Y] =
				other.ifSubclassOf[ComponentIdentity[X, Y] forSome { type X[T]; type Y[T] }] {
					_ => this.asInstanceOf[ComponentMorphism[Z, Y]]
				} orElse other.ifSubclass[ComponentHomomorphism[Z, X]] {
					mono => components.homomorphism[Z, Y](z => homomorphism(mono.homomorphism(z)))
				} getOrElse
					ComponentMorphism[Z, Y](other(_).map(homomorphism))
		}


		class ComponentIdentity[-X[T]<:TypedMapping[T], +Y[T]<:TypedMapping[T]] private[ComponentMorphism] () extends ComponentHomomorphism[X, Y] {
			@inline
			override def apply[T](x :X[T]) = Some(x.asInstanceOf[Y[T]])

			@inline
			override def map[T](x: X[T]): Y[T] = x.asInstanceOf[Y[T]]

			val homomorphism = (x :X[_]) => x.asInstanceOf[Y[_]]

			override def andThen[Z[T]<:TypedMapping[T]](morphism: ComponentMorphism[Y, Z]): ComponentMorphism[X, Z] =
				morphism.asInstanceOf[ComponentMorphism[X, Z]]

			override def compose[Z[T]<:TypedMapping[T]](morphism: ComponentMorphism[Z, X]): ComponentMorphism[Z, Y] =
				morphism.asInstanceOf[ComponentMorphism[Z, Y]]
		}

		private class Empty[+M[T]<:TypedMapping[T]] extends ComponentMorphism[TypedMapping, M] {
			override def apply[T](x: TypedMapping[T]) = None
			override def compose[Z[T] <: TypedMapping[T]](morphism: ComponentMorphism[Z, TypedMapping]) = this
			override def andThen[Z[T] <: TypedMapping[T]](morphism: ComponentMorphism[M, Z]) = this.asInstanceOf[Empty[Z]]
		}
		private val nothing = new Empty[Mapping]
		
		private val id = new ComponentIdentity[Mapping, Mapping]
	}
	

	
	
	
	
	
	abstract class AbstractMorphism[X<:AnyMapping, Y<:AnyMapping](
			val value :ValueMorphism[X#ResultType, Y#ResultType], val components :ComponentMorphism[Y#Component, X#Component])
		extends MappingMorphism[X, Y]
	


	trait StructuralIdentityMorphism[X<:ComponentCompatibleMapping[Y], Y<:AnyMapping] extends MappingMorphism[X, Y] {
		def components = ComponentMorphism.identity[Y#Component]
	}

	trait ValueIdentityMorphism[X<:TypedMapping[T], Y<:TypedMapping[T], T] extends MappingMorphism[X, Y] {
		def value = ValueMorphism.identity[T]
	}

}


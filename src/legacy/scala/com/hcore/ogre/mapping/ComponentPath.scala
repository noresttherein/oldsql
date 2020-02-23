package com.hcore.ogre.mapping

import com.hcore.ogre.mapping.ComponentPath.MappedComponent.TypedMappedComponent
import com.hcore.ogre.mapping.ComponentPath._
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.Mapping.{ColumnFilter, ComponentCompatibleMapping, CompatibleMapping}
import com.hcore.ogre.mapping.MappingPath._
import com.hcore.ogre.mapping.support.MappedMapping
import com.hcore.ogre.mapping.support.MappedMapping.MappedAs
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.options.extensions


//implicits
import extensions._
import SaferCasts._


/** Represents mapping Y as a part of mapping X. Y may be X, X#Component[_], a component further down the tree,
  * or some otherwise unrelated from the type checker point of view mapping of a type ValueType which in some way is part
  * of the result type of mapping X.  It allows to get the value of a given target component from a value of source type or ComponentValues[X].
  * A path is a component path if the target mapping is an inherent part of assembly process
  * of the source mapping. This doesn't mean that there will always be a value for Y in X, or that X.apply() will be called
  * by mapping Y, as some components may not always be fetched (for example large columns),
  * or their data may not always be present/relevant (such as columns for a particular subclass when mapping a class hierarchy to a table).
  *
  *
  * @tparam X top level mapping type - usually a singleton type for a given mapping instance.
  * @tparam Y target mapping type - usually a singleton type for some component instance
  */
sealed trait ComponentPath[X<:AnyMapping, Y<:AnyMapping] extends MappingPath[X, Y] {

	def pick :X#ResultType => Option[Y#ResultType] //= this.apply
	def surepick :Option[X#ResultType => Y#ResultType]

	def optional = surepick.isEmpty


	def lift[Q](subcomponent :Y#Component[Q]) :Option[X#Component[Q]]

	def lift :Option[X#Component[Y#ResultType]] = direct.map(_.end :X#Component[Y#ResultType])

	def lift[Q](subcomponent :Y=>Y#Component[Q]) :Option[X#Component[Q]] = lift(subcomponent(end))

	def lift(columns :ColumnFilter) :Seq[X#Component[_]] = end.columns.flatMap(col => lift(col) :Option[X#Component[_]])


	def direct :Option[DirectComponent[X, _<:X#Component[Y#ResultType], Y#ResultType]]

	override def asComponentPath = Some(this)

	def apply(value :X#ResultType) :Option[Y#ResultType] = pick(value)

	def unapply(root :X#ResultType) :Option[Y#ResultType] = pick(root)



	abstract class PathMorphism extends MappingMorphism[X, Y] {
		def source = start
		def target = end
	}

	def morphism :MappingMorphism[X, Y]





	def apply()(implicit value :ComponentValues[X]) :Y#ResultType = value(this)

	def get(implicit value :ComponentValues[X]) :Option[Y#ResultType] = value.get(this)



	def walk(values :ComponentValues[X]) :ComponentValues[Y]



	override def prefixesDesc :Seq[ComponentPath[X, _<:AnyMapping]] = prefixes.reverse
	
	def prefixes :Seq[ComponentPath[X, _<:AnyMapping]]

	def suffixes :Seq[ComponentPath[_<:AnyMapping, Y]]

	override def suffixesDesc = suffixes.reverse

	def splits :Seq[(ConcatComponentPath[X, AnyMapping, Y])]

	def splitsReversed :Seq[(ConcatComponentPath[X, AnyMapping, Y])]


	override def drop[M <: AnyMapping](other: MappingPath[_ <: AnyMapping, M]): Option[ComponentPath[M, Y]]

	override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (ComponentPath[X, M], ComponentPath[M, Y]) forSome {type M <: AnyMapping}



	override def apply[C<:Y#AnyComponent](subcomponent :Y=>C) :ComponentPath[X, C] = this \ subcomponent

	def \: (mapping :AnyMapping)(implicit ev : X<:<mapping.Component[_]) :ComponentPath[mapping.type, Y]


	def :\ [Q](subcomponent :Y#Component[Q]) :TypedComponentPath[X, subcomponent.type, Q]


	def \ [C<:Y#AnyComponent](subcomponent :Y=>C) :ComponentPath[X, C]



	override def :+[C <: Y#AnyComponent](subcomponent: C): ComponentPath[X, C] =
		(this :\ subcomponent.asInstanceOf[Y#Component[Any]]).asInstanceOf[X\**\C]





	def ++ [Z<:AnyMapping](path :ComponentPath[Y, Z]) :ComponentPath[X, Z] =
		(this ++ path.asInstanceOf[TypedComponentPath[Y, Mapping[Any], Any]]).asInstanceOf[X \**\ Z]
	
	def ++ [Z<:Mapping[V], V](path :TypedComponentPath[Y, Z, V]) :TypedComponentPath[X, Z, V]


	def ++:[W<:AnyMapping](prefix :ComponentPath[W, X]) :ComponentPath[W, Y]



	def concat[M<:AnyMapping, C<:AnyMapping](suffix :ComponentPath[M, C]) :Option[ComponentPath[X, C]] =
		(this ++ suffix.asInstanceOf[Y \**\ C]).providing(suffix.start==end)

	def map[V](map: (Y#ResultType) => V)(unmap: (V) => Y#ResultType): TypedComponentPath[X, Y MappedAs V, V]

	override def unchecked :ComponentPath[AnyMapping, AnyMapping] = this.asInstanceOf[AnyMapping\**\AnyMapping]

	override def cast[X1<:AnyMapping, Y1<:AnyMapping] :ComponentPath[X1, Y1] = this.asInstanceOf[X1\**\Y1]

	def canEqual(that :Any) :Boolean

	override def tailString: String = "**\\"+end
}








object ComponentPath {
	/** An optional shorthand for writing ComponentPath[X, Y] as X \**\ Y */
	type \**\[X<:AnyMapping, Y<:AnyMapping] = ComponentPath[X, Y]


	implicit def selfPath[T](mapping :Mapping[T]) :TypedComponentPath[mapping.type, mapping.type, T] = SelfPath(mapping)



	def apply[M<:AnyMapping, C<:M#AnyComponent](mapping :M, component :C) :ComponentPath[M, C] =
		direct(mapping, component)

	def direct[M<:AnyMapping, C<:M#AnyComponent](mapping :M, component :C) :ComponentPath[M, C] =
		(mapping \\ component.asInstanceOf[mapping.AnyComponent]).asInstanceOf[M \**\ C]

	def self[M<:AnyMapping](mapping :M) :ComponentPath[M, M] =
		new SelfPath[Mapping[Any], Any](mapping.asInstanceOf[Mapping[Any]]).asInstanceOf[M \**\ M]


	def unapply[X<:AnyMapping, Y<:AnyMapping](path :MappingPath[X, Y]) :Option[ComponentPath[X, Y]] =
		path.asSubclass[X\**\Y]

	



	object SelfPath {
		def apply[T](mapping :Mapping[T]) :TypedComponentPath[mapping.type, mapping.type, T] =
			new SelfPath[mapping.type, T](mapping)

		
		
		def typed[T, M<:Mapping[T]](mapping :M) :TypedComponentPath[M, M, T] =
			new SelfPath[M, T](mapping)

		def unapply[X<:AnyMapping, Y<:AnyMapping](path :X \**\ Y) :Option[Y with X] = path match {
			case s :SelfPath[_, _] => Some(s.end.asInstanceOf[Y with X])
			case _ => None
		}

	}




	object DirectComponent {

		def apply[T, U](mapping :Mapping[T])(component :mapping.Component[U])(
				pick :ValueMorphism[T, U], lift :ComponentMorphism[component.Component, mapping.Component]) :TypedComponentPath[mapping.type, component.type, U] =
			apply[mapping.type, component.type, U](MappingMorphism[mapping.type, component.type](mapping, component, pick, lift))

		def apply[M<:AnyMapping, C<:M#Component[T], T](morph :MappingMorphism[M, C]) :TypedComponentPath[M, C, T] =
			new DirectComponent[M, C, T] with MorphismPath[M, C] {
				val morphism = morph
			}

		def unapply[X<:AnyMapping, Y<:AnyMapping](path :ComponentPath[X, Y]) :Option[X#Component[Y#ResultType]] =
			path match {
				case p :DirectComponent[_, _, _] => Some(p.end.asInstanceOf[X#Component[Y#ResultType]]) //Some(p.asInstanceOf[ComponentPath[X, Y with X#Component[_]]])
				case _ => None
			}



	}

	/** Matcher for component paths slicing off the first component from the path. */
	object NestedComponent {

		/** Matches any non-self component path, splitting it into a prefix of length 1 and a suffix. Be warned that currently due to a not-so-elegant solution,
		  * the first element of the tuple isn't necessarily represent a direct component, because it might be a ComponentLink symbolizing an adapted mapping.
		  * Anyway, DirectComponent is a bit misleading, because it might be a lifted component (a component further down the tree rooted at path.start raised
		  * to a status of component of path.start via an adapter, table columns being good examples).
		  */
		def unapply[X <:AnyMapping, Y<:AnyMapping](path :ComponentPath[X, Y]) :Option[(ComponentPath[X, X#Component[_]], ComponentPath[X#Component[_], Y])] =
			path.ifSubclass[TypedConcatCompPath[X, AnyMapping, _, Y, _]] { concat =>
				(concat.descending.prefix.asInstanceOf[X \**\ X#Component[_]], concat.descending.suffix.asInstanceOf[X#Component[_] \**\ Y])
			}
	}

	/** A bit of a hack, informing that mapping Y is directly used by mapping X, but for implementation reasons is not an actual component of X.
	  * Used to describe a relationship between a mapping adapter/proxy (for example used to rename or otherwise modify some properties of a mapping)
	  * and the adapted mapping.
	  * @tparam X mapping adapter type
	  * @tparam Y adapted mapping type
	  */
	trait MappedComponent[X<:AnyMapping, Y<:AnyMapping] extends MappingLink[X, Y] with ComponentPath[X, Y] with DirectPath[X, Y]

	object MappedComponent {
		def apply[X<:AnyMapping, Y<:AnyMapping](morphism :MappingMorphism[X, Y]) :MappedComponent[X, Y] = {
			val target = morphism.target
			new TypedMappedComponent[X, target.type, target.ResultType](morphism.asInstanceOf[MappingMorphism[X, target.type]]).asInstanceOf[MappedComponent[X, Y]]
		}

		def unapply[X<:AnyMapping, Y<:AnyMapping](path :MappingPath[X, Y]) :Option[ComponentPath[X, Y]] =
			path.asSubclass[MappedComponent[X, Y]]

		class TypedMappedComponent[X<:AnyMapping, Y<:AnyMapping{ type ResultType=V }, V](val morphism :MappingMorphism[X, Y])
			extends MappedComponent[X, Y] with TypedDirectPath[X, Y, V] with TypedComponentPath[X, Y, V] with TypedMappingLink[X, Y, V] with MorphismPath[X, Y]
		{
			override def direct: Option[DirectComponent[X, _ <: X#Component[V], V]] = None

			override def walk(values: ComponentValues[X]): ComponentValues[Y] = values.morph(morphism)

			override def drop[M <: AnyMapping](other: MappingPath[_ <: AnyMapping, M]): Option[TypedComponentPath[M, Y, V]] =
				super.drop(other).asInstanceOf[Option[TypedComponentPath[M, Y, V]]]

			override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (ComponentPath[X, M], TypedComponentPath[M, Y, V]) forSome {type M <: AnyMapping} =
				super.splitWhere(fun).asInstanceOf[(ComponentPath[X, AnyMapping], TypedComponentPath[AnyMapping, Y, V])]



//			override def toString = s"\\$start~$end"
			override def tailString = "~"+end

			override def canEqual(that :Any) = that.isInstanceOf[MappedComponent[_, _]]
		}
	}


	/** A subtype of ComponentPath that should be used wherever possible, as it greatly improves working with type inference and
	  * allows for implicit resolution based on the target mapped type T.
	  */
	trait TypedComponentPath[X<:AnyMapping, Y <:AnyMapping{ type ResultType=T }, T] extends ComponentPath[X, Y] with TypedPath[X, Y, T] {
		override def surepick :Option[X#ResultType =>Y#ResultType]

		override def apply()(implicit value: ComponentValues[X]): T = super.apply()

		override def get(implicit value: ComponentValues[X]): Option[T] = super.get


		override def direct: Option[DirectComponent[X, _ <: X#Component[T], T]]

		override def lift: Option[X#Component[T]] = direct.map(_.end :X#Component[T])

		override def asComponentPath = Some(this)


		override def map[V](map: T => V)(unmap: V => T): TypedComponentPath[X, MappedAs[Y, V], V] = {
			val mapped = MappedMapping(end, map, unmap)
			this ++ new TypedMappedComponent[Y, Y MappedAs V, V](mapped.reverseMorphism.asInstanceOf[MappingMorphism[Y, Y MappedAs V]])
		}


		override def drop[M <: AnyMapping](other: MappingPath[_ <: AnyMapping, M]): Option[TypedComponentPath[M, Y, T]]


		override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (ComponentPath[X, M], TypedComponentPath[M, Y, T]) forSome {type M <: AnyMapping}

		override def \: (mapping :AnyMapping)(implicit ev : X<:<mapping.Component[_]) :TypedComponentPath[mapping.type, Y, T] =
			typedConcat((mapping \\ start).asInstanceOf[mapping.type \**\ X], this)


		override def :\ [Q](subcomponent :Y#Component[Q]) :TypedComponentPath[X, subcomponent.type, Q] = {
			val mapping = end
			val tail = (mapping \\ subcomponent.asInstanceOf[mapping.Component[Q]]).asInstanceOf[TypedComponentPath[Y, subcomponent.type, Q]]
			typedConcat(this, tail)
		}


		override def \ [C<:Y#AnyComponent](subcomponent :Y=>C) :ComponentPath[X, C] = {
			val first = end
			val next = subcomponent(end).asInstanceOf[first.Component[Any]]
			val tail = (first \\ next).asInstanceOf[TypedComponentPath[Y, next.type, Any]]
			typedConcat(this, tail).asInstanceOf[X \**\ C]
		}


		override def ++ [Z<:AnyMapping { type ResultType=V }, V](path :TypedComponentPath[Y, Z, V]) :TypedComponentPath[X, Z, V] = path match {
			case _ if path.start!=end =>
				throw new IllegalArgumentException(s"$this: can't append $path as it starts at a different mapping")
			case SelfPath(_) => this.asInstanceOf[TypedComponentPath[X, Z, V]]
			case _ => typedConcat(this, path)
		}

		override def ++[Z <: AnyMapping](path: MappingPath[Y, Z]): MappingPath[X, Z] = path match {
			case c :ComponentPath[_, _] => super[ComponentPath].++(c.asInstanceOf[ComponentPath[Y, Z]])
			case _ => super[ComponentPath].++(path)
		}

		override def ++[Z <: AnyMapping {type ResultType = V}, V](path: TypedPath[Y, Z, V]): TypedPath[X, Z, V] = super.++(path)

		override def ++:[W<:AnyMapping](prefix :ComponentPath[W, X]) :TypedComponentPath[W, Y, T] = prefix match {
			case _ if prefix.end!=start =>
				throw new IllegalArgumentException(s"can't prepend $prefix to $this as it ends at a different mapping")
			case SelfPath(_) => this.asInstanceOf[TypedComponentPath[W, Y, T]]
			case _ => typedConcat(prefix, this)
		}

		override def ++:[W <: AnyMapping](prefix: MappingPath[W, X]): TypedPath[W, Y, T] = prefix match {
			case c :ComponentPath[_, _] => c.asInstanceOf[ComponentPath[W, X]] ++: this
			case _ => super[TypedPath].++:(prefix)
		}



		override protected def typedConcat[R <: AnyMapping, Q <: AnyMapping {type ResultType = V}, V](
				prefix: R \~\ (_<:AnyMapping), suffix: TypedPath[_<:AnyMapping, Q, V]): TypedConcatPath[R, _, _, Q, V] =
			(prefix, suffix) match {
				case (p:TypedComponentPath[_, _, _], q:TypedComponentPath[_, _, _]) =>
					typedConcat(p.asInstanceOf[R \**\ AnyMapping], q.asInstanceOf[TypedComponentPath[AnyMapping, Q, V]])
				case _ =>
					super.typedConcat(prefix, suffix)
			}

		protected def typedConcat[R<:AnyMapping, Q<:AnyMapping { type ResultType=V }, V](
					prefix :R \**\ (_<:AnyMapping), suffix :TypedComponentPath[_<:AnyMapping, Q, V]) :TypedConcatCompPath[R, _, _, Q, V] =
			new TypedConcatCompPath[R, Mapping[Any], Any, Q, V](
				prefix.asInstanceOf[TypedComponentPath[R, Mapping[Any], Any]],
				suffix.asInstanceOf[TypedComponentPath[Mapping[Any], Q, V]])

		protected override def concat[R <: AnyMapping, S <: AnyMapping, Q <: AnyMapping](prefix: R \~\ S, suffix: S \~\ Q): ConcatPath[R, S, Q] =
			(prefix, suffix) match {
				case (p:ComponentPath[_, _], s:ComponentPath[_, _]) =>
					concat(p.asInstanceOf[R\**\S], s.asInstanceOf[S\**\Q])
				case _ => super.concat(prefix, suffix)
			}

		protected def concat[R<:AnyMapping, S<:AnyMapping, Q<:AnyMapping](prefix : R\**\S, suffix : S\**\Q) :ConcatComponentPath[R, S, Q] =
			new TypedConcatCompPath[R, Mapping[Any], Any, Mapping[Any], Any](
				prefix.asInstanceOf[TypedComponentPath[R, Mapping[Any], Any]],
				suffix.asInstanceOf[TypedComponentPath[Mapping[Any], Mapping[Any], Any]]).asInstanceOf[ConcatComponentPath[R, S, Q]]

	}




	object TypedComponentPath {
		implicit def valueForPath[M<:AnyMapping, T](path :TypedComponentPath[M, _, T])(implicit value :ComponentValues[M]) :T =
			path()
	}





	sealed trait FullyTypedComponentPath[X <: AnyMapping { type ResultType=S }, S, Y <: AnyMapping { type ResultType=T }, T]
		extends TypedComponentPath[X, Y, T] with FullyTypedPath[X, S, Y, T]
	{
		override val pick :S=>Option[T]
		override def surepick :Option[S=>T]
		override def apply(value :S) :Option[T] = pick(value)
		override def unapply(root :S) :Option[T] = pick(root)
	}


	trait MorphismPath[X<:AnyMapping, Y<:AnyMapping] extends ComponentPath[X, Y] {
		override def optional: Boolean = !morphism.value.isHomomorphism

		override def start = morphism.source
		override def end = morphism.target

		override def pick = morphism.value.function
		override def surepick = morphism.value.ifHomomorphism(_.map)

		override def lift[Q](subcomponent: Y#Component[Q]): Option[X#Component[Q]] =
			morphism.components(subcomponent)
	}
	


	class SelfPath[M<:Mapping[T], T] private[ComponentPath] (mapping :M) extends FullyTypedComponentPath[M, T, M, T] {
		def length=0
		def start = mapping
		def end = mapping

		override val pick = Some(_:T)
		override def surepick = Some(identity[T] _)

		override def lift[Q](subcomponent: M#Component[Q]): Option[M#Component[Q]] = Some(subcomponent)

		override def optional: Boolean = false

		object morphism extends PathMorphism {
			val value = ValueMorphism.identity[T]
			val components = ComponentMorphism.identity[M#Component]
		}


		override def mappings = Seq(start)

		override def walk(values: ComponentValues[M]): ComponentValues[M] = values


		
		override def prefixes: Seq[ComponentPath[M, _ <: AnyMapping]] = Seq()
		
		override def prefixesDesc = Seq()

		override def suffixes = Seq()

		override def suffixesDesc = Seq()

		override def splits = Seq()

		override def splitsReversed = Seq()


		override def startsWith(other: MappingPath[_ <: AnyMapping, _ <: AnyMapping]): Boolean = this==other


		override def drop[N <: AnyMapping](other: MappingPath[_ <: AnyMapping, N]): Option[TypedComponentPath[N, M, T]] =
			this.asInstanceOf[TypedComponentPath[N, M, T]].providing(other==this)


		override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (SelfPath[M, T], SelfPath[M, T]) =
			(this, this)

		override def ++[Z <: AnyMapping {type ResultType = V}, V](path: TypedPath[M, Z, V]): TypedPath[M, Z, V] = path

		override def ++[Z <: AnyMapping {type ResultType = V}, V](path: TypedComponentPath[M, Z, V]): TypedComponentPath[M, Z, V] = path


		override def ++:[W <: AnyMapping](prefix: MappingPath[W, M]): TypedPath[W, M, T] = prefix.asInstanceOf[TypedPath[W, M, T]]

		override def ++:[W<:AnyMapping](prefix :ComponentPath[W, M]) :TypedComponentPath[W, M, T] = prefix.asInstanceOf[TypedComponentPath[W, M, T]]


		override def \[C <: M#AnyComponent](subcomponent: (M) => C): ComponentPath[M, C] =
			(mapping \\ subcomponent(mapping).asInstanceOf[mapping.Component[_]]).asInstanceOf[M\**\C]

		override def :\[Q](subcomponent: M#Component[Q]): TypedComponentPath[M, subcomponent.type, Q] =
			(mapping \\ subcomponent.asInstanceOf[mapping.Component[Q]]).asInstanceOf[TypedComponentPath[M, subcomponent.type, Q]]//[M \**\ subcomponent.type]


		override def \: (mapping :AnyMapping)(implicit ev : M<:<mapping.Component[_]) :TypedComponentPath[mapping.type, M, T] =
			(mapping \\ start.asInstanceOf[mapping.Component[T]]).asInstanceOf[TypedComponentPath[mapping.type, M, T]]



		override def direct = None



		override def canEqual(that :Any) = that.isInstanceOf[SelfPath[_, _]]

		override def equals(that :Any) = that match {
			case other :SelfPath[_, _] => (other eq this) || other.end==this.end
			case _ => false
		}

		override def hashCode = end.hashCode


		override def tailString = "" //s"\\$end"
	}






	trait DirectComponent[X<:AnyMapping, Y<:X#Component[T], T] extends TypedComponentPath[X, Y, T] with TypedDirectPath[X, Y, T]{
		def start :X
		def end :Y


		override def direct: Some[DirectComponent[X, Y, T]] = Some(this)

		override def walk(value: ComponentValues[X]): ComponentValues[Y] =
			value.direct(this)


		override def drop[M <: AnyMapping](other: MappingPath[_ <: AnyMapping, M]): Option[TypedComponentPath[M, Y, T]] =
			super.drop(other).asInstanceOf[Option[TypedComponentPath[M, Y, T]]]


		override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (ComponentPath[X, M], TypedComponentPath[M, Y, T]) forSome {type M <: AnyMapping} =
			super.splitWhere(fun).asInstanceOf[(X\**\AnyMapping, TypedComponentPath[AnyMapping, Y, T])]


		override def canEqual(that :Any) = that.isInstanceOf[DirectComponent[_, _, _]]

//		override def equals(that :Any) = that match {
//			case d :DirectComponent[_, _, _] =>
//				(this eq d) || d.canEqual(this) && start==d.start && end==d.end
//			case _ => false
//		}
//
//		override def hashCode = (start, end).hashCode
//
		override def tailString = super[TypedDirectPath].tailString
	}


//	class MappedComponentPath[X<:AnyMapping, Y<:AnyMapping{ type ResultType=V }, V] extends TypedComponentPath[X, Y, V] {
//
//	}
	

	sealed trait ConcatComponentPath[X<:AnyMapping, Y<:AnyMapping, Z<:AnyMapping] 
		extends ConcatPath[X, Y, Z] with ComponentPath[X, Z]

	private[ComponentPath] class TypedConcatCompPath[X <: AnyMapping, Y<:AnyMapping{ type ResultType=W }, W, Z<:AnyMapping{ type ResultType=V}, V] private[mapping] (
            override val prefix :TypedComponentPath[X, Y, W], override val suffix :TypedComponentPath[Y, Z, V]
		) extends TypedConcatPath[X, Y, W, Z, V](prefix, suffix) with ConcatComponentPath[X, Y, Z] with TypedComponentPath[X, Z, V]
	{

		override val pick = morphism.value.function //prefix.pick(_).flatMap(suffix.pick)
		override def surepick = for (f <- prefix.surepick; g <- suffix.surepick) yield (f andThen g)
		override def optional = surepick.isEmpty

		override def lift[T](subcomponent: Z#Component[T]): Option[X#Component[T]] =
			suffix.lift(subcomponent).flatMap(prefix.lift(_))



		override def prefixes: Seq[ComponentPath[X, _ <: AnyMapping]] =
			super[TypedConcatPath].prefixes.asInstanceOf[Seq[ComponentPath[X, _<:AnyMapping]]]


		override def prefixesDesc: Seq[ComponentPath[X, _ <: AnyMapping]] =
			super[TypedConcatPath].prefixesDesc.asInstanceOf[Seq[ComponentPath[X, _<:AnyMapping]]]


		override def suffixes: Seq[ComponentPath[_ <: AnyMapping, Z]] =
			super[TypedConcatPath].suffixes.asInstanceOf[Seq[ComponentPath[_<:AnyMapping, Z]]]


		override def suffixesDesc: Seq[ComponentPath[_ <: AnyMapping, Z]] =
			super[TypedConcatPath].suffixesDesc.asInstanceOf[Seq[ComponentPath[_<:AnyMapping, Z]]]



		override def splits =
			super[TypedConcatPath].splits.asInstanceOf[Stream[(ConcatComponentPath[X, AnyMapping, Z])]]

		override def splitsReversed =
			super[TypedConcatPath].splitsReversed.asInstanceOf[Stream[(ConcatComponentPath[X, AnyMapping, Z])]]


		override def drop[M <: AnyMapping](other: MappingPath[_ <: AnyMapping, M]): Option[TypedComponentPath[M, Z, V]] =
			super.drop(other).asInstanceOf[Option[TypedComponentPath[M, Z, V]]]


		override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (ComponentPath[X, M], TypedComponentPath[M, Z, V]) forSome {type M <: AnyMapping} =
			super.splitWhere(fun).asInstanceOf[(X\**\AnyMapping, TypedComponentPath[AnyMapping, Z, V])]

		object morphism extends PathMorphism {
			override def value = prefix.morphism.value andThen suffix.morphism.value//ValueMorphism(pick)
			override def components = prefix.morphism.components compose suffix.morphism.components//ComponentMorphism.between[Z, X](lift(_))
		}



		override def walk(values: ComponentValues[X]): ComponentValues[Z] =
			suffix.walk(prefix.walk(values))

		
		override lazy val direct: Option[DirectComponent[X, _ <: X#Component[Z#ResultType], Z#ResultType]] = {
			suffix.direct.flatMap { directSecond =>
				prefix.lift(directSecond.end :Y#Component[Z#ResultType]).map { lifted =>
					val root = start
					(root \\ lifted.asInstanceOf[root.Component[Z#ResultType]]).asInstanceOf[DirectComponent[X, X#Component[Z#ResultType], Z#ResultType]]
				}
			}
		}


		override def toString = super[TypedConcatPath].toString

		override def tailString = super[TypedConcatPath].tailString


		override def canEqual(other: Any): Boolean = other.isInstanceOf[TypedConcatCompPath[_, _, _, _, _]]

		override def equals(other: Any): Boolean = other match {
			case that: TypedConcatCompPath[_, _, _, _, _] => super[TypedConcatPath].equals(that)
			case _ => false
		}
	}




/*
	case class SymLinkComponentPath[X<:AnyMapping, Z<:Mapping[T], T, Y<:Mapping[V], V](target :TypedComponentPath[X, Z, T], targetMorphism :MappingMorphism[Z, Y])
		extends TypedMappingLink[X, Y, V] with TypedComponentPath[X, Y, V]  with TypedDirectPath[X, Y, V] with MorphismPath[X, Y]
	{ link =>
//		override def start = target.start

//		override def pick = morphism.value.function //target.morphism andTh
//		override def surepick = morphism.value.ifHomomorphism(_.map)

//		override def lift[Q](subcomponent: Y#Component[Q]): Option[X#Component[Q]] = target.lift(subcomponent)

		val morphism = target.morphism andThen targetMorphism

		override def direct: Option[DirectComponent[X, _ <: X#Component[V], V]] = None

//		override def morphism: ComponentValuesMorphism[X, Y] = new PathMorphism {
//			override val value: ValueMorphism[X#ResultType, V] = link.target.morphism.value andThen valueMorphism
//			override def components: ComponentMorphism[Y#Component, X#Component] = link.target.morphism.components
//		}

		override def drop[M <: AnyMapping](other: MappingPath[_ <: AnyMapping, M]): Option[TypedComponentPath[M, Y, V]] =
			super.drop(other).asInstanceOf[Option[TypedComponentPath[M, Y, V]]]

		override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (ComponentPath[X, M], TypedComponentPath[M, Y, V]) forSome {type M <: AnyMapping} =
			super.splitWhere(fun).asInstanceOf[(ComponentPath[X, AnyMapping], TypedComponentPath[AnyMapping, Y, V])]


		override def walk(values: ComponentValues[X]): ComponentValues[Y] =
			target.walk(values).morph(targetMorphism)


		override def tailString = end.toString + " => " + target.tailString
	}
*/

	
}
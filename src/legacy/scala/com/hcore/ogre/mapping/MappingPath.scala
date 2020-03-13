package com.hcore.ogre.mapping


import com.hcore.ogre.mapping.ComponentPath.MappedComponent.TypedMappedComponent
import com.hcore.ogre.mapping.ComponentPath.{TypedComponentPath, MappedComponent, MorphismPath, SelfPath}
import com.hcore.ogre.mapping.Mapping.ComponentCompatibleMapping
import com.hcore.ogre.mapping.MappingMorphism.ValueMorphism
import com.hcore.ogre.mapping.MappingPath._
import com.hcore.ogre.mapping.support.MappedMapping
import com.hcore.ogre.mapping.support.MappedMapping.MappedAs
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.options.extensions


//implicits
import extensions._
import SaferCasts._


/** Representation of the facto that value of mapping Y can be reached from the value of mapping X. 
  * It might mean that Y#ResultType is physically a part of X#ResultType (for example, a property, 
  * property of a property, etc), or is in some sense associated with with value X#ResultType, like in the case
  * of relationships and possible join definitions. Paths can be concatenated and thus form a sequence of mappings, 
  * or a sequence of direct paths, where the end meapping of each is the start mapping of nest. 
  * The nature of the link between the mappings is defined by concrete subclasses. The main implementation is
  * ComponentPath and derived classes, which state that the end mapping is direct (in case of DirectComponent path) 
  * or indirect (in case of concatenated component paths) component of the start mapping, and thus takes part in its assembly.
  * 
  */
trait MappingPath[X<:AnyMapping, Y<:AnyMapping] {
	type ValueType = Y#ResultType

	def length :Int

	def start :X
	def end :Y


	def pick :X#ResultType => Option[Y#ResultType] //= this.apply
//	def surepick :Option[X#ResultType => Y#ResultType]

	def optional :Boolean

//	def apply(value :X#ResultType) :Option[Y#ResultType] = pick(value)

//	def unapply(root :X#ResultType) :Option[Y#ResultType] = pick(root)

//	def map[V](map :Y#ResultType => V, unmap :V=>Y#ResultType) :MappingPath[X, MappingSubstitute[V, ]] =

	def mappings :Seq[AnyMapping]

	def prefixesDesc :Seq[MappingPath[X, _<:AnyMapping]] = prefixes.reverse

	def prefixes :Seq[MappingPath[X, _<:AnyMapping]]

	def suffixes :Seq[MappingPath[_<:AnyMapping, Y]]

	def suffixesDesc :Seq[MappingPath[_<:AnyMapping, Y]] = suffixes.reverse

	def splits :Seq[(ConcatPath[X, AnyMapping, Y])]

	def splitsReversed :Seq[(ConcatPath[X, AnyMapping, Y])]


	def startsWith(other :MappingPath[_<:AnyMapping, _<:AnyMapping]) :Boolean

	def drop[M<:AnyMapping](other :MappingPath[_<:AnyMapping, M]) :Option[MappingPath[M, Y]]

	def splitWhere(fun :MappingPath[_, _]=>Boolean) :(MappingPath[X, M], MappingPath[M, Y]) forSome { type M<:AnyMapping }


	def apply[C<:Y#AnyComponent](subcomponent :Y=>C) :MappingPath[X, C] = this \ subcomponent


	def \: (mapping :AnyMapping)(implicit ev : X<:<mapping.Component[_]) :MappingPath[mapping.type, Y]


	def :\ [Q](subcomponent :Y#Component[Q]) :TypedPath[X, subcomponent.type, Q]




	def \ [C<:Y#AnyComponent](subcomponent :Y=>C) :MappingPath[X, C]



	def :+ [C<:Y#AnyComponent](subcomponent :C) :MappingPath[X, C] =
		(this :\ subcomponent.asInstanceOf[Y#Component[Any]]).asInstanceOf[X\~\C]

	def +: [M<:AnyMapping](mapping :M)(implicit ev :X<:<M#Component[_]) :MappingPath[M, Y] =
		((mapping :mapping.type) \: this)(ev.asInstanceOf[X<:<mapping.Component[Any]]).asInstanceOf[M\~\Y]



	def ++ [Z<:AnyMapping](path :MappingPath[Y, Z]) :MappingPath[X, Z] =
		(this ++ path.asInstanceOf[TypedPath[Y, Mapping[Any], Any]]).asInstanceOf[X \~\ Z]

	def ++ [Z<:Mapping[V], V](path :TypedPath[Y, Z, V]) :TypedPath[X, Z, V]

	def ++:[W<:AnyMapping](prefix :MappingPath[W, X]) :MappingPath[W, Y]



	def concat[M<:AnyMapping, C<:AnyMapping](suffix :MappingPath[M, C]) :Option[MappingPath[X, C]] =
		(this ++ suffix.asInstanceOf[Y \~\ C]).providing(suffix.start==end)



	protected[MappingPath] def isAscending :Boolean = true
	protected[MappingPath] def isDescending :Boolean = true




	def asComponentPath :Option[ComponentPath[X, Y]] = None

	def map[V](map :Y#ResultType=>V)(unmap :V=>Y#ResultType) :TypedPath[X, Y MappedAs V, V]



	def canEqual(that :Any) :Boolean

	def unchecked :MappingPath[AnyMapping, AnyMapping] = this.asInstanceOf[AnyMapping\~\AnyMapping]

	def cast[X1<:AnyMapping, Y1<:AnyMapping] = this.asInstanceOf[X1\~\Y1]

	override def toString = s"\\$start$tailString"

	def tailString = s"\\~\\$end"
}






object MappingPath {
	/** An alias for MappingPath which adds some sugar allowing to write X \~\ Y. */
	type \~\[X<:AnyMapping, Y<:AnyMapping] = MappingPath[X, Y]

	def unapply[X<:AnyMapping, Y<:AnyMapping](path :MappingPath[X, Y]) :Some[(X, Y)] =
		Some(path.start, path.end)


	object SplitFirst {
		def unapply[X <:AnyMapping, Y<:AnyMapping](path :MappingPath[X, Y]) :Option[(DirectPath[X, AnyMapping], MappingPath[AnyMapping, Y])] = //forSome { type M <:Mapping }] =
			path match {
				case d:DirectPath[_, _] => Some(d.asInstanceOf[DirectPath[X, AnyMapping]], ComponentPath.self(d.end).asInstanceOf[AnyMapping\~\Y])
				case _ => Indirect.unapply(path)
			}
	}

	object Indirect {
		def unapply[X<:AnyMapping, Y<:AnyMapping](path :MappingPath[X, Y]) :Option[(DirectPath[X, AnyMapping], MappingPath[AnyMapping, Y])] = // forSome { type M <:Mapping }] =
			path.ifSubclass[TypedConcatPath[X, AnyMapping, _, Y, _]] { concat =>
				(concat.descending.prefix.asInstanceOf[DirectPath[X, AnyMapping]], concat.descending.suffix.asInstanceOf[AnyMapping \~\ Y])
			}

	}




	/** A subtype of MappingPath that should be used wherever possible, as it greatly improves working with type inference and
	  * allows for implicit resolution based on the target mapped type T.
	  */
	trait TypedPath[X<:AnyMapping, Y <:AnyMapping{ type ResultType=T }, T] extends MappingPath[X, Y] {

		override def pick :X#ResultType => Option[T]
//		override def surepick :Option[X#ResultType =>Y#ResultType]

//		override def apply(value :X#ResultType) :Option[T] = pick(value)

//		override def unapply(root :X#ResultType) :Option[T] = pick(root)


		override def map[V](map: T => V)(unmap: V => T): TypedPath[X, MappedAs[Y, V], V] = {
			val mapped = MappedMapping(end, map, unmap)
			this ++ new TypedMappedComponent[Y, Y MappedAs V, V](mapped.reverseMorphism.asInstanceOf[MappingMorphism[Y, Y MappedAs V]])
		}

		override def drop[M <: AnyMapping](other: MappingPath[_ <: AnyMapping, M]): Option[TypedPath[M, Y, T]]


		override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (MappingPath[X, M], TypedPath[M, Y, T]) forSome { type M<:AnyMapping }

		def \: (mapping :AnyMapping)(implicit ev : X<:<mapping.Component[_]) :TypedPath[mapping.type, Y, T] =
//			new TypedConcatPath[mapping.type, X, Y, T]((mapping \\ start).asInstanceOf[mapping.type \~\ X], this)
			typedConcat((mapping \\ start), this)


		def :\ [Q](subcomponent :Y#Component[Q]) :TypedPath[X, subcomponent.type, Q] = {
			val mapping = end
			val tail = (mapping \\ subcomponent.asInstanceOf[mapping.Component[Q]]).asInstanceOf[TypedPath[Y, subcomponent.type, Q]]
//			new TypedConcatPath[X, Y, subcomponent.type, Q](this, tail)
			typedConcat(this, tail)
		}


		def \ [C<:Y#AnyComponent](subcomponent :Y=>C) :MappingPath[X, C] = {
			val first = end
			val next = subcomponent(end).asInstanceOf[first.Component[Any]]
			val tail = (first \\ next).asInstanceOf[TypedPath[Y, next.type, Any]]
//			new TypedConcatPath[X, Y, next.type, Any](this, tail).asInstanceOf[X \~\ C]
			typedConcat(this, tail).asInstanceOf[X \~\ C]
		}



		def ++ [Z<:AnyMapping { type ResultType=V }, V](path :TypedPath[Y, Z, V]) :TypedPath[X, Z, V] = path match {
			case _ if path.start!=end =>
				throw new IllegalArgumentException(s"$this: can't append $path as it starts at a different mapping")
			case SelfPath(_) => this.asInstanceOf[TypedPath[X, Z, V]]
			case _ => typedConcat(this, path) //new TypedConcatPath[X, Y, Z, V](this, path)
		}



		override def ++:[W<:AnyMapping](prefix :MappingPath[W, X]) :TypedPath[W, Y, T] = prefix match {
			case _ if prefix.end!=start =>
				throw new IllegalArgumentException(s"can't prepend $prefix to $this as it ends at a different mapping")
			case SelfPath(_) => this.asInstanceOf[TypedPath[W, Y, T]]
			case _ => typedConcat(prefix, this) //new TypedConcatPath[W, X, Y, T](prefix, this)
		}

		
		protected def typedConcat[R<:AnyMapping, Q<:AnyMapping { type ResultType=V }, V](
				prefix :R \~\ (_<:AnyMapping), suffix :TypedPath[_<:AnyMapping, Q, V]) :TypedConcatPath[R, _, _, Q, V] =
			new TypedConcatPath(prefix.asInstanceOf[TypedPath[R, Mapping[Any], Any]], suffix.asInstanceOf[TypedPath[Mapping[Any], Q, V]])

		protected def concat[R<:AnyMapping, S<:AnyMapping, Q<:AnyMapping](prefix :R \~\ S, suffix :S \~\ Q) :ConcatPath[R, S, Q] =
			typedConcat(prefix.asInstanceOf[TypedPath[R, Mapping[Any], Any]], suffix.asInstanceOf[TypedPath[Mapping[Any], Mapping[Any], Any]]).asInstanceOf[ConcatPath[R, S, Q]]
		

	}
	
	
	trait FullyTypedPath[X<:AnyMapping{ type ResultType=S }, S, Y<:AnyMapping{ type ResultType=T }, T]
		extends TypedPath[X, Y, T]
	{
		override def pick :S=>Option[T]

//		override def apply(value :S) :Option[T] = pick(value)

//		override def unapply(root :S) :Option[T] = pick(root)
		
	}


	/** The link between mappings X and Y is atomic and cannot be split into smaller steps. Currently all paths
	  * implement either SelfPath (an additively neutral element of the path from some mapping to itself), DirectPath or ConcatPath.
	  */
	trait DirectPath[X<:AnyMapping, Y<:AnyMapping] extends MappingPath[X, Y]

	object DirectPath {
		@inline
		def unapply[X<:AnyMapping, Y<:AnyMapping](path :MappingPath[X, Y]) :Option[DirectPath[X, Y]] =
			path.asSubclass[DirectPath[X, Y]]
	}


	trait TypedDirectPath[X<:AnyMapping, Y<:AnyMapping{ type ResultType=V }, V]
		extends DirectPath[X, Y] with TypedPath[X, Y, V]
	{
		def length = 1

		override def mappings = Seq(start, end)

		override def prefixes :Seq[this.type] = Seq(this)

		override def prefixesDesc :Seq[this.type] = Seq(this)

		override def suffixes :Seq[this.type] = Seq(this)

		override def suffixesDesc :Seq[this.type] = Seq(this)

		override def splits :Seq[Nothing] = Seq()

		override def splitsReversed :Seq[Nothing] = Seq()


		override def startsWith(other: MappingPath[_<:AnyMapping, _ <: AnyMapping]): Boolean = other match {
			case SelfPath(mapping) => mapping==start
			case _ => this == other
		}


		override def drop[M <: AnyMapping](other: MappingPath[_ <: AnyMapping, M]): Option[TypedPath[M, Y, V]] = other match {
			case SelfPath(mapping) if mapping==start => Some(this.asInstanceOf[TypedPath[M, Y, V]])
			case _ if other==this => Some(ComponentPath.self(end).asInstanceOf[TypedPath[M, Y, V]])
			case _ => None
		}


		override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (MappingPath[X, M], TypedPath[M, Y, V]) forSome { type M<:AnyMapping } =
			if (fun(this)) (this.asInstanceOf[X\~\AnyMapping], SelfPath(end.asMapping).asInstanceOf[TypedPath[AnyMapping, Y, V]])
			else (ComponentPath.self(start).asInstanceOf[X\~\AnyMapping], this.asInstanceOf[TypedPath[AnyMapping, Y, V]])


		override def canEqual(that: Any): Boolean = that.isInstanceOf[DirectPath[_, _]]

		override def equals(that :Any) = that match {
			case d :DirectPath[_, _] => (this eq d) || (d.canEqual(this) && canEqual(that) && d.start==start && d.end==end)
			case _ => false
		}

		override def hashCode = (start, end).hashCode

		override def tailString = "\\"+end.toString

	}
	
	/** A path being a concatenation of two (of which any  can be a concatenation itself) paths X\~\Y and Y\~\Z. */
	trait ConcatPath[X<:AnyMapping, Y<:AnyMapping, Z<:AnyMapping] extends MappingPath[X, Z] {
		def prefix :MappingPath[X, Y]
		def suffix :MappingPath[Y, Z]

		private[schema] def ascending :ConcatPath[X, _<:AnyMapping, Z]
		private[schema] def descending :ConcatPath[X, _<:AnyMapping, Z]

		private[MappingPath] def append[M<:AnyMapping](path :Z \~\ M) :ConcatPath[X, _<:AnyMapping, M]
		private[MappingPath] def prepend[M<:AnyMapping](path :M \~\ X) :ConcatPath[M, _<:AnyMapping, Z]
	}



	private[schema] class TypedConcatPath[X <: AnyMapping, Y<:AnyMapping{ type ResultType=W }, W, Z<:AnyMapping{ type ResultType=V }, V] private[schema] (
			 val prefix :TypedPath[X, Y, W], val suffix :TypedPath[Y, Z, V]
		 ) extends ConcatPath[X, Y, Z] with TypedPath[X, Z, V]
	{
		val length = prefix.length + suffix.length
		val start = prefix.start
		val end = suffix.end

		override val pick = prefix.pick(_:X#ResultType).flatMap(suffix.pick)
//		override def surepick = for (f <- prefix.surepick; g <- suffix.surepick) yield (f andThen g)


		override def optional: Boolean = prefix.optional || suffix.optional


		override def mappings = descending.start +: descending.suffix.mappings

		override def prefixes: Seq[MappingPath[X, _ <: AnyMapping]] =
			prefix.prefixes.toStream ++ suffix.prefixes.toStream.map(suffix => prefix ++ suffix)


		override def prefixesDesc: Seq[MappingPath[X, _ <: AnyMapping]] =
			suffix.prefixesDesc.toStream.map(suffix => prefix ++ suffix) ++ prefix.prefixesDesc.toStream


		override def suffixes :Seq[MappingPath[_<:AnyMapping, Z]] =
			suffix.suffixes.toStream ++ prefix.suffixes.toStream.map(prefix => prefix ++ suffix)

		override def suffixesDesc :Seq[MappingPath[_<:AnyMapping, Z]] =
			prefix.suffixesDesc.toStream.map(s => s ++ suffix) ++ suffix.suffixesDesc

		override def splits =
			prefix.splits.toStream.map(split => concat(split.prefix, concat(split.suffix, suffix))) ++
				(this.asInstanceOf[ConcatPath[X, AnyMapping, Z]] +:
					suffix.splits.toStream.map(split => concat(concat(prefix, split.prefix), split.suffix)))

		override def splitsReversed =
			suffix.splitsReversed.toStream.map(split => concat(concat(prefix, split.prefix), split.suffix)) ++
				(this.asInstanceOf[ConcatPath[X, AnyMapping, Z]] +:
					prefix.splitsReversed.toStream.map(split => concat(split.prefix, concat(split.suffix, suffix))))




		def startsWith(other :MappingPath[_<:AnyMapping, _<:AnyMapping]) = other match {
			case SelfPath(mapping) => mapping==descending.start
			case direct :DirectPath[_, _] => descending.prefix==direct
			case concat :ConcatPath[_, _, _] =>
				concat.descending.prefix==descending.prefix && descending.suffix.startsWith(concat.descending.suffix)
		}


		override def drop[M <: AnyMapping](other: MappingPath[_ <: AnyMapping, M]): Option[TypedPath[M, Z, V]] = other match {
			case SelfPath(mapping)  => this.asInstanceOf[TypedPath[M, Z, V]].providing(mapping==descending.start)
			case direct :DirectPath[_, _] => descending.suffix.asInstanceOf[TypedPath[M, Z, V]].providing(descending.prefix==direct)
			case concat :ConcatPath[_, _, _] if concat.descending.prefix==descending.prefix =>
				descending.suffix.drop(concat.descending.suffix).asInstanceOf[Option[TypedPath[M, Z, V]]]
			case _ => None
		}


		override def splitWhere(fun: (MappingPath[_, _]) => Boolean): (MappingPath[X, M], TypedPath[M, Z, V]) forSome {type M <: AnyMapping} =
			if (!fun(descending.prefix))
				(ComponentPath.self(descending.start).asInstanceOf[X\~\AnyMapping], this.asInstanceOf[TypedPath[AnyMapping, Z, V]])
			else {
				val (_1, _2) = descending.suffix.splitWhere(fun)
				(descending.prefix.asInstanceOf[X\~\AnyMapping] ++ _1.asInstanceOf[AnyMapping\~\AnyMapping], _2.asInstanceOf[TypedPath[AnyMapping, Z, V]])
			}

		override val isAscending = prefix.isAscending && (suffix match {
			case c :ConcatPath[_, _, _] => false
			case _ => true
		})
		override val isDescending = suffix.isDescending && (prefix match {
			case c:ConcatPath[_, _, _] => false
			case _ => true
		})



		/** this path as a concatenation tree in which all suffix fields are direct paths (not concatenations) */
		private[schema] lazy val ascending :ConcatPath[X, _<:AnyMapping, Z] =
			if (isAscending) this
			else
				prefix.ifSubclassOf[ConcatPath[X, AnyMapping, Y]] { path =>
					path.ascending.append(suffix)
				} orElse suffix.ifSubclassOf[ConcatPath[Y, AnyMapping, Z]] { path =>
					concat(prefix, path.prefix).ascending.append(path.suffix)
				} getOrElse this

		/** this path as a concatenation tree in which all prefix fields are direct paths (not concatenations) */
		private[schema] lazy val descending :ConcatPath[X, _<:AnyMapping, Z] =
			if (isDescending) this
			else
				suffix.ifSubclassOf[ConcatPath[Y, AnyMapping, Z]] { path =>
					path.descending.prepend(prefix)
				} orElse prefix.ifSubclassOf[ConcatPath[X, AnyMapping, Y]] { path =>
					concat(path.suffix, suffix).prepend(path.prefix)
				} getOrElse this


		private[MappingPath] def append[M<:AnyMapping](path :Z \~\ M) :ConcatPath[X, _<:AnyMapping, M] = path match {
			case ConcatPath(_1, _2) => append(_1).append(_2)
			case _ => concat(this, path)
		}

		private[MappingPath] def prepend[M<:AnyMapping](path :M \~\X) :ConcatPath[M, _<:AnyMapping, Z] = path match {
			case ConcatPath(_1, _2) => prepend(_2).prepend(_1)
			case _ => concat(path, this)
		}




		override def canEqual(other: Any): Boolean = other.isInstanceOf[ConcatPath[_, _, _]]

		override def equals(other: Any): Boolean = other match {
			case that: ConcatPath[_, _, _] =>
				(that canEqual this) &&
					ascending.suffix == that.ascending.suffix &&
					ascending.prefix == that.ascending.prefix
			case _ => false
		}

		override def hashCode: Int = (ascending.prefix, ascending.suffix).hashCode


		override def toString = prefix.toString + suffix.tailString

		override def tailString = prefix.tailString + suffix.tailString

	}




	private object ConcatPath {
		def unapply[X<:AnyMapping, Y<:AnyMapping](path :X \~\ Y) :Option[(X \~\ AnyMapping, AnyMapping \~\ Y)] = path match {
			case c :TypedConcatPath[_, _, _, _, _] =>
				Some(c.prefix.asInstanceOf[X \~\ AnyMapping], c.suffix.asInstanceOf[AnyMapping \~\ Y])
			case _ => None
		}
	}





	trait MappingLink[X<:AnyMapping, Y<:AnyMapping] extends MappingPath[X, Y]

//	object MappingLink {
//		def apply[T, S](from :Mapping[T], to :Mapping[S], getter :T=>Option[S], opt :Boolean=true) :TypedMappingLink[from.type, to.type, S] =
//			new TypedMappingLink[from.type, to.type, S] with TypedDirectPath[from.type, to.type, S] {
//				val start :from.type = from
//				val end :to.type = to
////				override def pick = getter
//				override val optional = opt
//
//				override def canEqual(that: Any): Boolean = that.isInstanceOf[MappingLink[_, _]]
//
//				override def toString = s"\\$start\\~>\\$end"
//			}
//
//		def apply[T](from :Mapping[T], to :Mapping[T]) :TypedMappingLink[from.type, to.type, T] =
//			apply[T, T](from, to, Some(_), false)
//
//		def unapply[X<:Mapping, Y<:Mapping](path :MappingPath[X, Y]) :Option[MappingLink[X, Y]] =
//			path.asSubclass[MappingLink[X, Y]]
//	}


	trait TypedMappingLink[X<:AnyMapping, Y<:AnyMapping{ type ResultType=T }, T] extends TypedPath[X, Y, T] with MappingLink[X, Y]




}
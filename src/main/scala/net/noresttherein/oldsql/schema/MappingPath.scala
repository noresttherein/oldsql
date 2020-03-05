package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.MappingPath.{\~\, ConcatPath, TypedPath}
import net.noresttherein.oldsql.schema.support.MappedMapping
import net.noresttherein.oldsql.schema.support.MappedMapping.MappedAs
import net.noresttherein.oldsql.schema.ComponentPath.{\:\, SelfPath}
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, SingletonComponent, SingletonMapping, TypedMapping, TypedSingleton}
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.SaferCasts._



/** Representation of the fact that value of mapping Y can be reached from the value of mapping X.
  * It might mean that Y#Subject is physically a part of X#Subject (for example, a property,
  * property of a property, etc), or is in some sense associated with with value X#Subject, like in the case
  * of relationships and possible join definitions. Paths can be concatenated and thus form a sequence of mappings,
  * or a sequence of direct paths, where the end mapping of each is the start mapping of nest.
  * The nature of the link between the mappings is defined by concrete subclasses. The main implementation is
  * ComponentPath and derived classes, which state that the end mapping is direct (in case of DirectComponent path)
  * or indirect (in case of concatenated component paths) component of the start mapping, and thus takes part in its assembly.
  */
trait MappingPath[X <: SingletonMapping, Y <: SingletonMapping] {
	type ValueType = Y#Subject

	def length :Int

	def start :X
	def end :Y


	def pick :X#Subject => Option[Y#Subject] //= this.apply
//	def surepick :Option[X#Subject => Y#Subject]

	def optional :Boolean


	def mappings :Seq[SingletonMapping]

	def prefixesDesc :Seq[MappingPath[X, _ <: SingletonMapping]] = prefixes.reverse

	def prefixes :Seq[MappingPath[X, _ <: SingletonMapping]]

	def suffixes :Seq[MappingPath[_ <: SingletonMapping, Y]]

	def suffixesDesc :Seq[MappingPath[_ <: SingletonMapping, Y]] = suffixes.reverse

	def splits :Seq[ConcatPath[X, SingletonMapping, Y]]

	def splitsReversed :Seq[ConcatPath[X, SingletonMapping, Y]]


	def startsWith(other :MappingPath[_ <: SingletonMapping, _ <: SingletonMapping]) :Boolean

	def drop[M <: SingletonMapping](other :MappingPath[_ <: SingletonMapping, M]) :Option[M \~\ Y]

	def splitWhere(fun :MappingPath[_, _] => Boolean) :(X \~\ M, M \~\ Y) forSome { type M <: SingletonMapping }


	def apply[C <: SingletonComponent[Y#Owner, _]](subcomponent :Y=>C) :X \~\ C = this \ subcomponent


	def \: (mapping :AnyMapping)(implicit ev : X <:< mapping.Component[_]) :mapping.type \~\ Y


	def :\ [Q](subcomponent :Component[Y#Owner, Q]) :TypedPath[X, subcomponent.type, Q]




	def \ [C <: SingletonComponent[Y#Owner, _]](subcomponent :Y=>C) :X \~\ C



	def :+ [C <: SingletonComponent[Y#Owner, _]](subcomponent :C) :X \~\ C =
		(this :\ subcomponent.asInstanceOf[SingletonComponent[Y#Owner, Any]]).asInstanceOf[X \~\ C]

	def +: [M <: SingletonMapping](mapping :M)(implicit ev :X <:< AnyComponent[M#Owner]) :M \~\ Y =
		((mapping :mapping.type) \: this)(ev.asInstanceOf[X <:< mapping.Component[Any]]).asInstanceOf[M\~\Y]



	def ++ [Z <: SingletonMapping](path :Y \~\ Z) :X \~\ Z =
		(this ++ path.asInstanceOf[TypedPath[Y, TypedSingleton[Any], Any]]).asInstanceOf[X \~\ Z]

	def ++ [Z <: TypedSingleton[V], V](path :TypedPath[Y, Z, V]) :TypedPath[X, Z, V]

	def ++:[W <: SingletonMapping](prefix :W \~\ X) :W \~\ Y



	def concat[M <: SingletonMapping, C <: SingletonMapping](suffix :M \~\ C) :Option[X \~\ C] =
		(this ++ suffix.asInstanceOf[Y \~\ C]).providing(suffix.start == end)



	protected[MappingPath] def isAscending :Boolean = true
	protected[MappingPath] def isDescending :Boolean = true




	def asComponentPath :Option[X \:\ Y] = None

	def map[V](map :Y#Subject => V)(unmap :V => Y#Subject) :TypedPath[X, (Y MappedAs V) with Singleton, V]



	def canEqual(that :Any) :Boolean

	def unchecked :SingletonMapping \~\ SingletonMapping =
		this.asInstanceOf[SingletonMapping \~\ SingletonMapping]

	def cast[X1 <: SingletonMapping, Y1 <: SingletonMapping] :X1 \~\ Y1 = this.asInstanceOf[X1\~\Y1]



	override def toString = s"\\$start$tailString"

	def tailString = s"\\~\\$end"
}






object MappingPath {
	/** An alias for `MappingPath` which adds some sugar allowing to write `X \~\ Y`. */
	type \~\[X <: SingletonMapping, Y <: SingletonMapping] = MappingPath[X, Y]

	def unapply[X <: SingletonMapping, Y <: SingletonMapping](path :X \~\ Y) :Some[(X, Y)] =
		Some(path.start, path.end)


	object SplitFirst {
		def unapply[X <: SingletonMapping, Y <: SingletonMapping](path :X \~\ Y) :Option[(DirectPath[X, SingletonMapping], SingletonMapping \~\ Y)] =
			path match {
				case d :DirectPath[_, _] =>
					Some(d.asInstanceOf[DirectPath[X, SingletonMapping]], ComponentPath.self(d.end).asInstanceOf[SingletonMapping\~\Y])
				case _ => Indirect.unapply(path)
			}
	}

	object Indirect {
		def unapply[X <: SingletonMapping, Y <: SingletonMapping](path :X \~\ Y) :Option[(DirectPath[X, SingletonMapping], SingletonMapping\~\Y)] = // forSome { type M <:AnyMapping }] =
			path.ifSubclass[TypedConcatPath[X, SingletonMapping, _, Y, _]] { concat =>
				(concat.descending.prefix.asInstanceOf[DirectPath[X, SingletonMapping]],
					concat.descending.suffix.asInstanceOf[SingletonMapping \~\ Y])
			}

	}




	/** A subtype of MappingPath that should be used wherever possible, as it greatly improves working with type inference and
	  * allows for implicit resolution based on the target mapped type T.
	  */
	trait TypedPath[X <: SingletonMapping, Y <: TypedSingleton[T], T] extends MappingPath[X, Y] {

		override def pick :X#Subject => Option[T]


		override def map[V](map: T => V)(unmap: V => T): TypedPath[X, MappedAs[Y, V] with Singleton, V] = ???/* {
			val mapped = MappedMapping(end, map, unmap)
			this ++ new TypedMappedComponent[Y, Y MappedAs V, V](
				mapped.reverseMorphism.asInstanceOf[MappingMorphism[Y, Y MappedAs V]]
			)
		}*/

		override def drop[M <: SingletonMapping](other: MappingPath[_ <: SingletonMapping, M]): Option[TypedPath[M, Y, T]]


		override def splitWhere(fun: MappingPath[_, _] => Boolean): (X \~\ M, TypedPath[M, Y, T]) forSome { type M <: SingletonMapping }

		def \: (mapping :AnyMapping)(implicit ev : X <:< mapping.Component[_]) :TypedPath[mapping.type, Y, T] = ???
//			typedConcat(mapping \\ start, this)


		def :\ [Q](subcomponent :Component[Y#Owner, Q]) :TypedPath[X, subcomponent.type, Q] = ??? /*{
			val mapping = end
			val tail = (mapping \\ subcomponent.asInstanceOf[mapping.Component[Q]]).asInstanceOf[TypedPath[Y, subcomponent.type, Q]]
			//			new TypedConcatPath[X, Y, subcomponent.type, Q](this, tail)
			typedConcat(this, tail)
		}*/


		def ++ [Z <: TypedSingleton[V], V](path :TypedPath[Y, Z, V]) :TypedPath[X, Z, V] = path match {
			case _ if path.start != end =>
				throw new IllegalArgumentException(s"$this: can't append $path as it starts at a different mapping")
			case SelfPath(_) => this.asInstanceOf[TypedPath[X, Z, V]]
			case _ => typedConcat(this, path)
		}



		override def ++:[W <: SingletonMapping](prefix :MappingPath[W, X]) :TypedPath[W, Y, T] = prefix match {
			case _ if prefix.end != start =>
				throw new IllegalArgumentException(s"can't prepend $prefix to $this as it ends at a different mapping")
			case SelfPath(_) => this.asInstanceOf[TypedPath[W, Y, T]]
			case _ => typedConcat(prefix, this) //new TypedConcatPath[W, X, Y, T](prefix, this)
		}



		protected def typedConcat[R <: SingletonMapping, Q <: TypedSingleton[V], V]
				(prefix :R \~\ (_ <: SingletonMapping), suffix :TypedPath[_ <: SingletonMapping, Q, V]) :TypedConcatPath[R, _, _, Q, V] =
			new TypedConcatPath(prefix.asInstanceOf[TypedPath[R, TypedSingleton[Any], Any]], suffix.asInstanceOf[TypedPath[TypedSingleton[Any], Q, V]])


		protected def concat[R <: SingletonMapping, S <: SingletonMapping, Q <: SingletonMapping]
				(prefix :R \~\ S, suffix :S \~\ Q) :ConcatPath[R, S, Q] =
			typedConcat(prefix.asInstanceOf[TypedPath[R, TypedSingleton[Any], Any]],
				suffix.asInstanceOf[TypedPath[SingletonMapping, TypedSingleton[Any], Any]]
			).asInstanceOf[ConcatPath[R, S, Q]]

		def \ [C <: SingletonComponent[Y#Owner, _]](subcomponent :Y=>C) :MappingPath[X, C] = {
			val first = end
			val next = subcomponent(end).asInstanceOf[first.Component[Any]]
			val tail = (first \\ next).asInstanceOf[TypedPath[Y, next.type, Any]]
			//			new TypedConcatPath[X, Y, next.type, Any](this, tail).asInstanceOf[X \~\ C]
			typedConcat(this, tail).asInstanceOf[X \~\ C]
		}


	}


	trait FullyTypedPath[X <: TypedSingleton[S], S, Y <: TypedSingleton[T], T]
		extends TypedPath[X, Y, T]
	{
		override def pick :S=>Option[T]
	}



	/** The link between mappings X and Y is atomic and cannot be split into smaller steps. Currently all paths
	  * implement either SelfPath (an additively neutral element of the path from some mapping to itself), DirectPath or ConcatPath.
	  */
	trait DirectPath[X <: SingletonMapping, Y <: SingletonMapping] extends MappingPath[X, Y]

	object DirectPath {
		@inline
		def unapply[X <: SingletonMapping, Y <: SingletonMapping](path :X \~\ Y) :Option[DirectPath[X, Y]] =
			path.asSubclass[DirectPath[X, Y]]
	}


	trait TypedDirectPath[X <: SingletonMapping, Y <: TypedSingleton[V], V]
		extends DirectPath[X, Y] with TypedPath[X, Y, V]
	{
		def length = 1

		override def mappings :Seq[SingletonMapping] = Seq(start, end)

		override def prefixes :Seq[this.type] = Seq(this)

		override def prefixesDesc :Seq[this.type] = Seq(this)

		override def suffixes :Seq[this.type] = Seq(this)

		override def suffixesDesc :Seq[this.type] = Seq(this)

		override def splits :Seq[Nothing] = Seq()

		override def splitsReversed :Seq[Nothing] = Seq()


		override def startsWith(other: MappingPath[_ <: SingletonMapping, _ <: SingletonMapping]): Boolean = other match {
			case SelfPath(mapping) => mapping == start
			case _ => this == other
		}


		override def drop[M <: SingletonMapping](other: MappingPath[_ <: SingletonMapping, M]): Option[TypedPath[M, Y, V]] = other match {
			case SelfPath(mapping) if mapping == start => Some(this.asInstanceOf[TypedPath[M, Y, V]])
			case _ if other == this => Some(ComponentPath.self(end).asInstanceOf[TypedPath[M, Y, V]])
			case _ => None
		}


		override def splitWhere(fun: MappingPath[_, _] => Boolean): (MappingPath[X, M], TypedPath[M, Y, V]) forSome { type M <: SingletonMapping } =
			if (fun(this))
				(this.asInstanceOf[X\~\SingletonMapping], SelfPath(end.asComponent).asInstanceOf[TypedPath[SingletonMapping, Y, V]])
			else (ComponentPath.self(start).asInstanceOf[X\~\SingletonMapping], this.asInstanceOf[TypedPath[SingletonMapping, Y, V]])


		override def canEqual(that: Any): Boolean = that.isInstanceOf[DirectPath[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case d :DirectPath[_, _] => 
				(this eq d) || (d.canEqual(this) && canEqual(that) && d.start == start && d.end == end)
			case _ => false
		}

		override def hashCode :Int = (start, end).hashCode

		override def tailString :String = "\\"+end.toString

	}

	/** A path being a concatenation of two (of which any  can be a concatenation itself) paths X\~\Y and Y\~\Z. */
	trait ConcatPath[X <: SingletonMapping, Y <: SingletonMapping, Z <: SingletonMapping] extends MappingPath[X, Z] {
		def prefix :X \~\ Y
		def suffix :Y \~\ Z

		private[schema] def ascending :ConcatPath[X, _ <: SingletonMapping, Z]
		private[schema] def descending :ConcatPath[X, _ <: SingletonMapping, Z]

		private[MappingPath] def append[M <: SingletonMapping](path :Z \~\ M) :ConcatPath[X, _ <: SingletonMapping, M]
		private[MappingPath] def prepend[M <: SingletonMapping](path :M \~\ X) :ConcatPath[M, _ <: SingletonMapping, Z]
	}



	private[schema] class TypedConcatPath[X <: SingletonMapping, Y <: TypedSingleton[W], W, Z <: TypedSingleton[V], V] private[schema]
			(val prefix :TypedPath[X, Y, W], val suffix :TypedPath[Y, Z, V])
		extends ConcatPath[X, Y, Z] with TypedPath[X, Z, V]
	{
		val length :Int = prefix.length + suffix.length
		val start :X = prefix.start
		val end :Z = suffix.end

		override val pick :X#Subject => Option[V] = prefix.pick(_ :X#Subject).flatMap(suffix.pick)
		//		override def surepick = for (f <- prefix.surepick; g <- suffix.surepick) yield (f andThen g)


		override def optional: Boolean = prefix.optional || suffix.optional


		override def mappings :Seq[SingletonMapping] = descending.start +: descending.suffix.mappings

		override def prefixes: Seq[MappingPath[X, _ <: SingletonMapping]] =
			prefix.prefixes.toStream ++ suffix.prefixes.toStream.map(suffix => prefix ++ suffix)


		override def prefixesDesc: Seq[MappingPath[X, _ <: SingletonMapping]] =
			suffix.prefixesDesc.toStream.map(suffix => prefix ++ suffix) ++ prefix.prefixesDesc.toStream


		override def suffixes :Seq[MappingPath[_ <: SingletonMapping, Z]] =
			suffix.suffixes.toStream ++ prefix.suffixes.toStream.map(prefix => prefix ++ suffix)

		override def suffixesDesc :Seq[MappingPath[_ <: SingletonMapping, Z]] =
			prefix.suffixesDesc.toStream.map(s => s ++ suffix) ++ suffix.suffixesDesc

		override def splits :Stream[ConcatPath[X, SingletonMapping, Z]] =
			prefix.splits.toStream.map(split => concat(split.prefix, concat(split.suffix, suffix))) ++
				(this.asInstanceOf[ConcatPath[X, SingletonMapping, Z]] +:
					suffix.splits.toStream.map(split => concat(concat(prefix, split.prefix), split.suffix)))

		override def splitsReversed :Stream[ConcatPath[X, SingletonMapping, Z]] =
			suffix.splitsReversed.toStream.map(split => concat(concat(prefix, split.prefix), split.suffix)) ++
				(this.asInstanceOf[ConcatPath[X, SingletonMapping, Z]] +:
					prefix.splitsReversed.toStream.map(split => concat(split.prefix, concat(split.suffix, suffix))))




		def startsWith(other :MappingPath[_ <: SingletonMapping, _ <: SingletonMapping]) :Boolean = other match {
			case SelfPath(mapping) => mapping == descending.start
			case direct :DirectPath[_, _] => descending.prefix == direct
			case concat :ConcatPath[_, _, _] =>
				concat.descending.prefix==descending.prefix && descending.suffix.startsWith(concat.descending.suffix)
		}


		override def drop[M <: SingletonMapping](other: MappingPath[_ <: SingletonMapping, M]): Option[TypedPath[M, Z, V]] = other match {
			case SelfPath(mapping)  => this.asInstanceOf[TypedPath[M, Z, V]].providing(mapping == descending.start)
			case direct :DirectPath[_, _] => descending.suffix.asInstanceOf[TypedPath[M, Z, V]].providing(descending.prefix == direct)
			case concat :ConcatPath[_, _, _] if concat.descending.prefix == descending.prefix =>
				descending.suffix.drop(concat.descending.suffix).asInstanceOf[Option[TypedPath[M, Z, V]]]
			case _ => None
		}


		override def splitWhere(fun: MappingPath[_, _] => Boolean): (MappingPath[X, M], TypedPath[M, Z, V]) forSome { type M <: SingletonMapping } =
			if (!fun(descending.prefix))
				(ComponentPath.self(descending.start).asInstanceOf[X\~\SingletonMapping],
					this.asInstanceOf[TypedPath[SingletonMapping, Z, V]]
				)
			else {
				val (_1, _2) = descending.suffix.splitWhere(fun)
				(descending.prefix.asInstanceOf[X\~\SingletonMapping] ++
					_1.asInstanceOf[SingletonMapping\~\SingletonMapping],
					_2.asInstanceOf[TypedPath[SingletonMapping, Z, V]]
				)
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
		private[schema] lazy val ascending :ConcatPath[X, _ <: SingletonMapping, Z] =
			if (isAscending) this
			else
				prefix.ifSubclassOf[ConcatPath[X, SingletonMapping, Y]] { path =>
					path.ascending.append(suffix)
				} orElse suffix.ifSubclassOf[ConcatPath[Y, SingletonMapping, Z]] { path =>
					concat(prefix, path.prefix).ascending.append(path.suffix)
				} getOrElse this

		/** this path as a concatenation tree in which all prefix fields are direct paths (not concatenations) */
		private[schema] lazy val descending :ConcatPath[X, _ <: SingletonMapping, Z] =
			if (isDescending) this
			else
				suffix.ifSubclassOf[ConcatPath[Y, SingletonMapping, Z]] { path =>
					path.descending.prepend(prefix)
				} orElse prefix.ifSubclassOf[ConcatPath[X, SingletonMapping, Y]] { path =>
					concat(path.suffix, suffix).prepend(path.prefix)
				} getOrElse this


		private[MappingPath] def append[M <: SingletonMapping](path :Z \~\ M) :ConcatPath[X, _ <: SingletonMapping, M] = path match {
			case ConcatPath(_1, _2) => append(_1).append(_2)
			case _ => concat(this, path)
		}

		private[MappingPath] def prepend[M <: SingletonMapping](path :M \~\ X) :ConcatPath[M, _ <: SingletonMapping, Z] = path match {
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


		override def toString :String = prefix.toString + suffix.tailString

		override def tailString :String = prefix.tailString + suffix.tailString

	}




	private object ConcatPath {
		def unapply[X <: SingletonMapping, Y <: SingletonMapping](path :X \~\ Y) :Option[(X \~\ SingletonMapping, SingletonMapping \~\ Y)] =
			path match {
				case c :TypedConcatPath[_, _, _, _, _] =>
					Some(c.prefix.asInstanceOf[X \~\ SingletonMapping], c.suffix.asInstanceOf[SingletonMapping \~\ Y])
				case _ => None
			}
	}





	trait MappingLink[X <: SingletonMapping, Y <: SingletonMapping] extends MappingPath[X, Y]

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
//		def unapply[X<:AnyMapping, Y<:AnyMapping](path :MappingPath[X, Y]) :Option[MappingLink[X, Y]] =
//			path.asSubclass[MappingLink[X, Y]]
//	}


	trait TypedMappingLink[X <: SingletonMapping, Y <: TypedSingleton[T], T] extends TypedPath[X, Y, T] with MappingLink[X, Y]




}


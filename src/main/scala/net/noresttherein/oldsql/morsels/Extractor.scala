package net.noresttherein.oldsql.morsels

import net.noresttherein.oldsql.morsels.Extractor.{ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}



/** A wrapper over an option-returning function `X=>Option[Y]`. It serves three main purposes:
  *   - as a base class for extractor objects declaring `unapply`;
  *   - differentiation between 'requisite' extractors, i.e. functions `X=>Some[Y]`, and true optional ones;
  *   - composition of extractors (by default via flattening) which is aware of requisite and identity extractors
  *     and create a suitable (more efficient) composition with them.
  * It is not a ''SAM'' type or a function itself, but subtypes
  * [[net.noresttherein.oldsql.morsels.Extractor.RequisiteExtractor RequisiteExtractor]] and
  * [[net.noresttherein.oldsql.morsels.Extractor.OptionalExtractor OptionalExtractor]] are,
  * meaning a compatible function expression in a position where they are expected types,
  * such as [[net.noresttherein.oldsql.morsels.Extractor.Requisite.apply Requisite(...)]] and
  * [[net.noresttherein.oldsql.morsels.Extractor.Optional.apply Optional(...)]], will be converted to one by the compiler.
  * The values are typically declared using the infix notation of the shortened type alias `=?>`.
  * @see [[net.noresttherein.oldsql.morsels.Extractor.=?> =?>]]
  * @see [[net.noresttherein.oldsql.morsels.Extractor.RequisiteExtractor RequisiteExtractor]]
  * @author Marcin Mościcki
  */ //consider: implementing PartialFunction. Would be great if Function extended PartialFunction (or vice versa).
trait Extractor[-X, +Y] {
	def optional :X => Option[Y] = get
	def requisite :Option[X => Y] = None

	def apply(x :X) :Y

	def get(x :X) :Option[Y]

	def unapply(x :X) :Option[Y] = get(x)

	def andThen[Z](extractor :Extractor[Y, Z]) :Extractor[X, Z] = extractor match {
		case _ :IdentityExtractor[_] | _ :ConstantExtractor[_, _] | _ :EmptyExtractor[_, _] => extractor compose this
		case sure :RequisiteExtractor[Y, Z] => requisite match {
			case Some(req) => Extractor.req(req andThen sure.getter)
			case _ =>
				val first = optional; val second = sure.getter
				Extractor { x :X => first(x).map(second) }
		}
		case _ => requisite match {
			case Some(req) => Extractor(req andThen extractor.optional)
			case _ =>
				val first = optional; val second = extractor.optional
				Extractor { x :X => first(x).flatMap(second) }
		}
	}

//	def andThen[Z](req :Y => Z) :Extractor[X, Z] = {
//		val first = optional
//		Extractor { x :X => first(x).map(req) }
//	}



	def compose[W](extractor :Extractor[W, X]) :Extractor[W, Y] = extractor andThen this

	def compose[W](req :W => X) :Extractor[W, Y] = Extractor(req andThen optional)

	def composeOpt[W](f :W => Option[X]) :Extractor[W, Y] = {
		val opt = optional; Extractor(f(_) flatMap opt)
	}



	def isIdentity :Boolean = false

	override def toString :String = "Extractor@" + System.identityHashCode(this)

}






sealed abstract class ExtractorImplicits {
	implicit def requisiteExtractor[X, Y](f :X => Y) :RequisiteExtractor[X, Y] = Extractor.req(f)
}



object Extractor extends ExtractorImplicits {

	/** A type alias for [[net.noresttherein.oldsql.morsels.Extractor Extractor]], allowing concise writing it
	  * in the infix function format `X =?> Y`.
	  */ //todo: make Extractor extend PartialFunction
	type =?>[-X, +Y] = Extractor[X, Y]


	def apply[X, Y](extract :X => Option[Y], requisite: Option[X => Y]) :Extractor[X, Y] = requisite match {
		case Some(f) => new RequisiteAdapter(f)
		case _ => new OptionalAdapter(extract)
	}


	/** Wraps the given function in an `Extractor` retrieving an optional value of `Y` from the whole `X`. */
	def apply[X, Y](extract :X => Option[Y]) :Extractor[X, Y] = new OptionalAdapter(extract)

	/** Wraps the given function in an `Extractor` retrieving an optional value of `Y` from the whole `X`. */
	def opt[X, Y](extract :X => Option[Y]) :Extractor[X, Y] = new OptionalAdapter(extract)

	/** Wraps the given function in an `Extractor` retrieving a value of `Y` from the whole `X`. */
	def req[X, Y](extract :X => Y) :RequisiteExtractor[X, Y] = new RequisiteAdapter(extract)

	/** An `Extractor` which always returns the given `result`, regardless of the argument. */
	def const[Y](result :Y) :ConstantExtractor[Any, Y] = new ConstantAdapter[Y](result)

	/** An `Extractor` which never produces a value, i.e. always returns `None`. */
	def none :EmptyExtractor[Any, Nothing] = Empty()

	/** An `Extractor` wrapping an identity function, i.e. always returning `Some(arg)` for the given argument `arg :X`. */
	def ident[X] :IdentityExtractor[X] = id.asInstanceOf[IdentityExtractor[X]]

	/** An extractor which always returns the same result from its `apply` method.
	  * This is equivalent to `Extractor.const` or `Extractor.none`, depending on whether the option contains a value.
	  */
	def maybe[X](value :Option[X]) :Extractor[Any, X] = value match {
		case Some(x) => const(x)
		case _ => none
	}

	private[this] val fail = new EmptyExtractor[Any, Nothing] {}
	private[this] val id = new IdentityExtractor[Any] {}



	def fromOpt[X] :Extractor[Option[X], X] = option.asInstanceOf[Extractor[Option[X], X]]

	private[this] val option = new OptionExtractor[Any]



	def unapply[X, Y](extractor :Extractor[X, Y]) :Some[X => Option[Y]] = Some(extractor.optional)



	implicit def optionalExtractor[X, Y](f :X => Option[Y]) :Extractor[X, Y] = opt(f)






	/** Factory and matcher for extractors of optional values, i.e. `Extractor` instances which might return `None`. */
	object Optional {
		/** Lifts the given function literal for `X => Option[Y]` to an `Extractor[X, Y]` by the SAM conversion.
		  * This is different than `Extractor.opt` in that produced extractor does not wrap a function, but instead
		  * implements its `apply` and `get` methods directly with the body of the argument function literal.
		  * @return f
		  */
		@inline def apply[X, Y](f :OptionalExtractor[X, Y]) :OptionalExtractor[X, Y] = f

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Option[X => Option[Y]] =
			if (extractor.requisite.isEmpty) Some(extractor.optional)
			else None
	}

	/** Factory and matcher for extractors which always produce a value for their argument, i.e.return `Some`. */
	object Requisite {
		/** Lifts the given function literal for `X => Y` to an `Extractor[X, Y]` by the SAM conversion.
		  * This is different than `Extractor.req` in that produced extractor does not wrap a function, but instead
		  * implements its `apply` and `get` methods directly with the body of the argument function literal.
		  * @return f
		  */
		@inline def apply[X, Y](f :RequisiteExtractor[X, Y]) :RequisiteExtractor[X, Y] = f

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Option[X => Y] = extractor.requisite
	}

	/** Factory and matcher for identity extractors, i.e. `Extractor` instances which always return the given argument
	  * from their `get` method.
	  */
	object Identity {
		/** An `Extractor` wrapping an identity function, i.e. always returning `Some(arg)` for the given argument
		  * `arg :X`. This is the same as `Extractor.ident[X]`, provided for consistency.
		  */
		def apply[X]() :IdentityExtractor[X] = id.asInstanceOf[IdentityExtractor[X]]

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Boolean = extractor.isInstanceOf[IdentityExtractor[_]]
	}

	/** Factory and matcher for extractors of constant, i.e. `Extractor` instances which always return the same value. */
	object Constant {
		/** An `Extractor` always returning `value` from its `get` method. This is the same as `Extractor.const`,
		  * provided here for consistency.
		  */
		@inline def apply[Y](value :Y) :ConstantExtractor[Any, Y] = new ConstantAdapter[Y](value)

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Option[Y] = extractor match {
			case const :ConstantExtractor[_, _] => Some(const.constant.asInstanceOf[Y])
			case _ => None
		}
	}

	/** Factory and matcher for extractors of non-existent values, i.e. `Extractor` instances which always return `None`
	  * from their `apply` method.
	  */
	object Empty {
		/** An `Extractor` always returning `None` from its `apply` method. This is the same as `Extractor.none`,
		  * provided here for consistency.
		  */
		def apply() :EmptyExtractor[Any, Nothing] = fail

		@inline def unapply(extractor :Extractor[_, _]) :Boolean = extractor.isInstanceOf[EmptyExtractor[_, _]]
	}






	/** The default `Extractor` implementation which can fail to produce a value for the argument.
	  * It is a 'SAM' type, leaving only the `get` method to be implemented by subclasses.
	  */
	trait OptionalExtractor[-X, +Y] extends Extractor[X, Y] {
		override def apply(x :X) :Y = get(x) getOrElse {
			throw new NoSuchElementException("No value for " + this + " in " + x)
		}

		override def andThen[Z](extractor :Extractor[Y, Z]) :Extractor[X, Z] = extractor match {
			case _ :IdentityExtractor[_] | _ :ConstantExtractor[_, _] | _ :EmptyExtractor[_, _] =>
				extractor compose this
			case sure :RequisiteExtractor[Y, Z] =>
				val first = optional; val second = sure.getter
				Extractor { x :X => first(x).map(second) }
			case _ =>
				val first = optional; val second = extractor.optional
				Extractor { x :X => first(x).flatMap(second) }
		}

	}



	/** An `Extractor` which will always succeed in producing a value for each argument.
	  * It is a 'SAM' type, leaving only the `apply` method to be implemented by subclasses.
	  */
	trait RequisiteExtractor[-X, +Y] extends Extractor[X, Y] {
		def getter :X => Y = apply
		override def optional :X => Some[Y] = (x :X) => Some(apply(x))
		override def requisite :Some[X => Y] = Some(getter)

		def apply(x :X) :Y

		override def get(x :X) :Some[Y] = Some(apply(x))



		override def andThen[Z](extractor :Extractor[Y, Z]) :Extractor[X, Z] = extractor match {
			case _ :IdentityExtractor[_] | _ :ConstantExtractor[_, _] | _ :EmptyExtractor[_, _] =>
				extractor compose this
			case sure :RequisiteExtractor[Y, Z] =>
				Extractor.req(this.getter andThen sure.getter)
			case _ :OptionExtractor[_] =>
				Extractor(this.getter.asInstanceOf[X => Option[Nothing]]).asInstanceOf[Extractor[X, Z]]
			case _ =>
				Extractor(this.getter andThen extractor.optional)
		}

		def andThen[Z](extractor :RequisiteExtractor[Y, Z]) :RequisiteExtractor[X, Z] = extractor match {
			case _ :IdentityExtractor[_] | _ :ConstantExtractor[_, _] | _ :EmptyExtractor[_, _] => extractor compose this
			case _ => Extractor.req(this.getter andThen extractor.getter)
		}




		def compose[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, Y] = extractor.andThen[Y](this)

		override def compose[W](req :W => X) :RequisiteExtractor[W, Y] = Extractor.req(req andThen getter)

		override def composeOpt[W](f :W => Option[X]) :Extractor[W, Y] = {
			val get = getter; Extractor(f(_) map get)
		}

		override def toString :String = "Requisite@" + System.identityHashCode(this)
	}






	/** An identity function as a `RequisiteExtractor`. */
	trait IdentityExtractor[X] extends RequisiteExtractor[X, X] {
		override val getter :X => X = identityGetter.asInstanceOf[X => X]
		override def optional :X => Some[X] = identityOptional.asInstanceOf[X => Some[X]]
		override def requisite :Some[X => X] = identityRequisite.asInstanceOf[Some[X => X]]

		override def apply(x :X) :X = x

		override def get(x :X) :Some[X] = Some(x)

		override def andThen[Z](extractor :Extractor[X, Z]) :Extractor[X, Z] = extractor
		override def compose[W](extractor :Extractor[W, X]) :Extractor[W, X] = extractor
		override def andThen[Z](extractor :RequisiteExtractor[X, Z]) :RequisiteExtractor[X, Z] = extractor
		override def compose[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, X] = extractor
		override def compose[W](req :W => X) :RequisiteExtractor[W, X] = Extractor.req(req)
		override def composeOpt[W](f :W => Option[X]) :Extractor[W, X] = Extractor(f)


		override def isIdentity = true

		override def toString = "Identity"
	}

	private[this] final val identityGetter = identity[Any] _
	private[this] final val identityOptional = (x :Any) => Some(x)
	private[this] final val identityRequisite = Some((x :Any) => x)






	/** An `Extractor` ignoring its arguments and always returning the same value.  */
	trait ConstantExtractor[-X, +Y] extends RequisiteExtractor[X, Y] {
		def constant :Y
		//declarations final to assure both functions and method use erased arguments and hence can be cast to Any =>
		final override val getter = { val c = constant; _ :X => c }
		final override val requisite = Some(getter)
		final override val optional = { val c = Some(constant); _ :X => c}

		final override def apply(x :X) :Y = constant

		final override def get(x :X) :Some[Y] = Some(constant)

		override def andThen[Z](extractor :Y =?> Z) :X =?> Z = extractor match {
			case _ :EmptyExtractor[_, _] => extractor compose this
			case _ => try {
				const(extractor(constant))
			} catch {
				case _ :Exception => Extractor(this.getter andThen extractor.optional)
			}
		}

		override def andThen[Z](extractor :RequisiteExtractor[Y, Z]) :RequisiteExtractor[X, Z] =
			try {
				const(extractor(constant))
			} catch {
				case _ :Exception => Extractor.req(this.getter andThen extractor.getter)
			}


		override def compose[W](extractor :W =?> X) :W =?> Y = extractor match {
			case _ :RequisiteExtractor[_, _] => this.asInstanceOf[ConstantExtractor[W, Y]]
			case _ :EmptyExtractor[_, _] => none
			case _ => composeOpt(extractor.optional)
		}

		override def compose[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, Y] =
			this.asInstanceOf[RequisiteExtractor[W, Y]]

		override def compose[W](req :W => X) :RequisiteExtractor[W, Y] = this.asInstanceOf[RequisiteExtractor[W, Y]]


		override def toString :String = "Const(" + constant + ")"
	}






	/** An `Extractor` which never produces any value, always returning `None` from its `get` method. */
	trait EmptyExtractor[-X, +Y] extends Extractor[X, Y] {
		override val optional :Any => Option[Nothing] = emptyOptional
		override def requisite :Option[Any => Nothing] = None

		override def apply(x :X) :Y = throw new NoSuchElementException(toString + ".apply(" + x + ")")
		override def get(x :X) :Option[Y] = None

		override def andThen[Z](extractor :Y =?> Z) :X =?> Z = none
		override def compose[W](extractor :W =?> X) :W =?> Y = none 
		override def compose[W](req :W => X) :W =?> Y = none
		override def composeOpt[W](f :W => Option[X]) :W =?> Y = none


		override def toString :String = "empty"
	}

	private[this] val emptyOptional = (_ :Any) => None






	private class OptionalAdapter[-X, +Y](override val optional :X => Option[Y]) extends OptionalExtractor[X, Y] {
		override def get(x :X) :Option[Y] = optional(x)
	}



	private class RequisiteAdapter[-X, +Y](override val getter :X=>Y) extends RequisiteExtractor[X, Y] {
		override val optional = { x :X => Some(getter(x)) }
		override val requisite = Some(getter)

		override def apply(x :X) :Y = getter(x)

		override def get(x :X) = Some(getter(x))
	}



	private class ConstantAdapter[+Y](const :Y) extends ConstantExtractor[Any, Y] {
		override def constant :Y = const
	}



	private class OptionExtractor[X] extends OptionalExtractor[Option[X], X] {
		override def optional :Option[X] => Option[X] = identity[Option[X]]

		override def get(x :Option[X]) :Option[X] = x

		override def andThen[Z](extractor :Extractor[X, Z]) :Extractor[Option[X], Z] = extractor match {
			case _ :IdentityExtractor[_] => extractor compose this
			case req :RequisiteExtractor[X, Z] =>
				val f = req.getter
				Extractor { opt :Option[X] => opt.map(f) }
			case _ =>
				val f = extractor.optional
				Extractor { opt :Option[X] => opt.flatMap(f) }
		}

	}


}



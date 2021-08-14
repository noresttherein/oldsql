package net.noresttherein.oldsql.morsels

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.Extractor.{OptionalExtractor, RequisiteExtractor}






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
trait Extractor[-X, +Y] extends Serializable { self =>
	def optional :X => Option[Y] = opt(_).toOption
	def requisite :Opt[X => Y] = Lack
	def force :X => Y = opt(_).get

	def apply(x :X) :Y

	def opt(x :X) :Opt[Y]
	@inline final def ?(x :X) :Opt[Y] = opt(x)

	def unapply(x :X) :Opt[Y] = opt(x)

	def andThen[Z](extractor :Extractor[Y, Z]) :Extractor[X, Z]
	def andThenOpt[Z](extractor :OptionalExtractor[Y, Z]) :Extractor[X, Z]
	def andThenReq[Z](extractor :RequisiteExtractor[Y, Z]) :Extractor[X, Z]
	def andThen[Z](f :Y => Z) :Extractor[X, Z]
	def andThenOpt[Z](f :Y => Option[Z]) :Extractor[X, Z]

	def compose[W](extractor :Extractor[W, X]) :Extractor[W, Y]
	def composeOpt[W](extractor :OptionalExtractor[W, X]) :Extractor[W, Y]
	def composeReq[W](extractor :RequisiteExtractor[W, X]) :Extractor[W, Y]
	def compose[W](f :W => X) :Extractor[W, Y]
	def composeOpt[W](f :W => Option[X]) :Extractor[W, Y]

	def isIdentity :Boolean = false

	override def toString :String = "Extractor@" + System.identityHashCode(this)

}






sealed abstract class ImplicitExtractors {
	implicit def requisiteExtractor[X, Y](f :X => Y) :RequisiteExtractor[X, Y] = Extractor.req(f)
}



object Extractor extends ImplicitExtractors {

	/** A type alias for [[net.noresttherein.oldsql.morsels.Extractor Extractor]], allowing concise writing it
	  * in the infix function format `X =?> Y`.
	  */
	type =?>[-X, +Y] = Extractor[X, Y]
	type =!>[-X, +Y] = RequisiteExtractor[X, Y]


	def apply[X, Y](extract :X => Option[Y], requisite: Opt[X => Y]) :Extractor[X, Y] = requisite match {
		case Got(f) => new RequisiteAdapter(f)
		case _ => new OptionalAdapter(extract)
	}


	/** Wraps the given function in an `Extractor` retrieving an optional value of `Y` from the whole `X`. */
	def apply[X, Y](extract :X => Option[Y]) :Extractor[X, Y] = new OptionalAdapter(extract)

	/** Adapts the given partial function to an `Extractor`. */
	def some[X, Y](extract :PartialFunction[X, Y]) :Extractor[X, Y] =
		opt(extract.andThen(Some(_:Y)).applyOrElse(_:X, (_:X) => None))

	/** Wraps the given function in an `Extractor` retrieving an optional value of `Y` from the whole `X`. */
	def opt[X, Y](extract :X => Option[Y]) :Extractor[X, Y] = new OptionalAdapter(extract)

	/** Wraps the given function in an `Extractor` retrieving a value of `Y` from the whole `X`. */
	def req[X, Y](extract :X => Y) :RequisiteExtractor[X, Y] = new RequisiteAdapter(extract)

	/** An `Extractor` wrapping an identity function, i.e. always returning `Some(arg)` for the given argument `arg :X`. */
	def ident[X] :IdentityExtractor[X] = id.asInstanceOf[IdentityExtractor[X]]

	/** An `Extractor` which always returns the given `result`, regardless of the argument. */
	def const[Y](result :Y) :ConstantExtractor[Any, Y] = new ConstantAdapter[Y](result)

	/** An `Extractor` which never produces a value, i.e. always returns `None`. */
	def none :EmptyExtractor[Any, Nothing] = Empty()


	/** An extractor which always returns the same result from its `apply` method.
	  * This is equivalent to `Extractor.const` or `Extractor.none`, depending on whether the option contains a value.
	  */
	def maybe[X](value :Option[X]) :Extractor[Any, X] = value match {
		case Some(x) => const(x)
		case _ => none
	}

	private[this] val fail = new EmptyExtractor[Any, Nothing] {}
	private[this] val id = new IdentityExtractor[Any] {}
	private[this] val Lack = Opt.Lack



	def fromOption[X] :Extractor[Option[X], X] = option.asInstanceOf[Extractor[Option[X], X]]

	private[this] val option = new OptionExtractor[Any]



	def unapply[X, Y](extractor :Extractor[X, Y]) :Opt[X => Option[Y]] = Got(extractor.optional)



	implicit def optionalExtractor[X, Y](f :X => Option[Y]) :Extractor[X, Y] = opt(f)






	/** Factory and matcher for extractors of optional values, i.e. `Extractor` instances which might return `None`. */
	object Optional {
		/** Lifts the given function literal for `X => Option[Y]` to an `Extractor[X, Y]` by the SAM conversion.
		  * This is different than `Extractor.opt` in that produced extractor does not wrap a function, but instead
		  * implements its `apply` and `opt` methods directly with the body of the argument function literal.
		  * This approach is generally preferable for optional extractors, as it can avoid boxing of the returned values.
		  * @return f
		  */
		@inline def apply[X, Y](f :OptionalExtractor[X, Y]) :OptionalExtractor[X, Y] = f

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Opt[X => Option[Y]] =
			if (extractor.requisite.isEmpty) Got(extractor.optional)
			else Lack
	}

	/** Factory and matcher for extractors which always produce a value for their argument, i.e.return `Some`. */
	object Requisite {
		/** Lifts the given function literal for `X => Y` to an `Extractor[X, Y]` by the SAM conversion.
		  * This is different than `Extractor.req` in that produced extractor does not wrap a function, but instead
		  * implements its `apply` and `opt` methods directly with the body of the argument function literal.
		  * In all situations where the extractor is created from a literal lambda expression, this method
		  * is slightly preferable to `Extractor.req`, as it doesn't involve an additional delegation call.
		  * @return f
		  */
		@inline def apply[X, Y](f :RequisiteExtractorBase[X, Y]) :RequisiteExtractor[X, Y] = f

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Opt[X => Y] = extractor.requisite
	}

	/** Factory and matcher for identity extractors, i.e. `Extractor` instances which always return the given argument
	  * from their `opt` method.
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
		/** An `Extractor` always returning `value` from its `opt` method. This is the same as `Extractor.const`,
		  * provided here for consistency.
		  */
		@inline def apply[Y](value :Y) :ConstantExtractor[Any, Y] = new ConstantAdapter[Y](value)

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Opt[Y] = extractor match {
			case const :ConstantExtractor[_, _] => Got(const.constant.asInstanceOf[Y])
			case _ => Lack
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
	  * It is a 'SAM' type, leaving only the `opt` method to be implemented by subclasses.
	  *///todo: make OptionalExtractor extend PartialFunction
	trait OptionalExtractor[-X, +Y] extends Extractor[X, Y] { self =>
		override def apply(x :X) :Y = opt(x) getOrElse {
			throw new NoSuchElementException("No value for " + this + " in " + x)
		}

		override def andThen[Z](extractor :Y =?> Z) :X =?> Z = extractor composeOpt this
		override def andThenOpt[Z](extractor :OptionalExtractor[Y, Z]) :X =?> Z = andThenCallback(extractor)

		override def andThenReq[Z](extractor :RequisiteExtractor[Y, Z]) :X =?> Z = extractor match {
			case _ :IdentityExtractor[_] | _ :ConstantExtractor[_, _] | _ :EmptyExtractor[_, _] =>
				extractor composeOpt this
			case _ => andThenCallback(extractor)
		}
		protected def andThenCallback[Z](extractor :Y =?> Z) :X =?> Z =
			Optional { opt(_) match {
				case Got(y) => extractor.opt(y)
				case _ => Lack
			}}

		override def andThen[Z](f :Y => Z) :X =?> Z = Optional { opt(_) map f }

		override def andThenOpt[Z](f :Y => Option[Z]) :X =?> Z =
			Extractor.opt { x :X => opt(x) match {
				case Got(y) => f(y)
				case _ => None
			}}

		override def compose[W](extractor :W =?> X) :W =?> Y = extractor andThenOpt this
		override def composeOpt[W](extractor :OptionalExtractor[W, X]) :W =?> Y = extractor andThenOpt[Y] this
		override def composeReq[W](extractor :RequisiteExtractor[W, X]) :W =?> Y = extractor andThenOpt this

		override def compose[W](f :W => X) :W =?> Y =
			new OptionalExtractor[W, Y] {
				override val optional = f andThen self.optional
				override def opt(x :W) = self.opt(f(x))
			}

		override def composeOpt[W](f :W => Option[X]) :W =?> Y =
			new OptionalExtractor[W, Y] {
				private[this] val continue = self.optional
				override val optional = f(_) flatMap continue

				override def opt(w :W) = f(w) match {
					case Some(x) => self.opt(x)
					case _ => Lack
				}
			}

		override def toString :String = "Optional@" + System.identityHashCode(this)
	}


	private[oldsql] class OptionalAdapter[-X, +Y](p :X => Option[Y]) extends OptionalExtractor[X, Y] {
		override def optional = p
		override def opt(x :X) :Opt[Y] = p(x)

		override def andThen[Z](extractor :Y =?> Z) : X =?> Z = extractor composeOpt optional

		protected override def andThenCallback[Z](extractor :Y =?> Z) =
			Optional { p(_) match {
				case Some(y) => extractor.opt(y)
				case _ => Lack
			}}

		override def andThen[Z](f :Y => Z) :X =?> Z = new OptionalExtractor[X, Z] {
			override val optional = p(_).map(f)

			override def opt(x :X) = p(x) match {
				case Some(y) => Got(f(y))
				case _ => Lack
			}
		}

		override def andThenOpt[Z](f :Y => Option[Z]) :X =?> Z = Extractor.opt { p(_).flatMap(f) }

		override def compose[W](extractor :W =?> X) = extractor andThenOpt p
		override def composeOpt[W](extractor :OptionalExtractor[W, X]) = extractor andThenOpt p
		override def composeReq[W](extractor :RequisiteExtractor[W, X]) = extractor andThenOpt p
		override def compose[W](f :W => X) = Extractor.opt(f andThen p)
		override def composeOpt[W](f :W => Option[X]) = Extractor.opt { f(_).flatMap(p) }
	}




	/** An `Extractor` which will always succeed in producing a value for each argument.
	  * It is a 'SAM' type, leaving only the `apply` method to be implemented by subclasses.
	  */
	trait RequisiteExtractor[-X, +Y] extends Extractor[X, Y] { self =>
		def getter :X => Y = apply
		override def optional :X => Some[Y] = (x :X) => Some(apply(x))
		override def requisite :Opt[X => Y] = Got(getter)
		override def force :X => Y = getter

		override def apply(x :X) :Y

		override def opt(x :X) :Opt[Y] = Got(apply(x))

		override def andThen[Z](extractor :Y =?> Z) :X =?> Z = extractor.composeReq[X](this)

		override def andThenOpt[Z](extractor :OptionalExtractor[Y, Z]) :X =?> Z =
			new OptionalExtractor[X, Z] {
				private[this] val first = self.getter
				override val optional = first andThen extractor.optional
				override def opt(x :X) = extractor.opt(first(x))
			}

		override def andThenReq[Z](extractor :RequisiteExtractor[Y, Z]) :RequisiteExtractor[X, Z] =
			andThen(extractor.getter)

		override def andThen[Z](f :Y => Z) :RequisiteExtractor[X, Z] = { //Extractor.req(getter andThen f)
			val first = getter; Requisite { x :X => f(first(x)) }
		}
		override def andThenOpt[Z](f :Y => Option[Z]) :X =?> Z = Extractor.opt(getter andThen f)

		override def compose[W](extractor :W =?> X) :W =?> Y = extractor andThenReq this
		override def composeOpt[W](extractor :OptionalExtractor[W, X]) :W =?> Y = extractor andThenReq this
		override def composeReq[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, Y] =
			extractor andThenReq[Y] this

		override def compose[W](f :W => X) :RequisiteExtractor[W, Y] = { //Extractor.req(req andThen getter)
			val continue = getter; Requisite { w :W => continue(f(w)) }
		}

		override def composeOpt[W](f :W => Option[X]) :Extractor[W, Y] =
			new OptionalExtractor[W, Y] {
				private[this] val continue = self.getter
				override val optional = f(_) map continue

				override def opt(w :W) = f(w) match {
					case Some(x) => Got(continue(x))
					case _ => Lack
				}
			}

		override def toString :String = "Requisite@" + System.identityHashCode(this)
	}


	trait RequisiteExtractorBase[-X, +Y] extends (X => Y) with RequisiteExtractor[X, Y] { self =>
		override def getter :X => Y = this

		override def andThen[Z](f :Y => Z) :RequisiteExtractorBase[X, Z] = { x :X => f(self(x)) }

		override def compose[W](f :W => X) :RequisiteExtractorBase[W, Y] = {  w :W => self(f(w)) }
	}


	private[oldsql] class RequisiteAdapter[-X, +Y](p :X => Y) extends RequisiteExtractor[X, Y] {
		override def getter :X => Y = p
		override val optional = { x :X => Some(p(x)) }
		override val requisite = Got(p)

		override def apply(x :X) :Y = p(x)
		override def opt(x :X) = Got(p(x))

		override def andThen[Z](extractor :Y =?> Z) :X =?> Z = extractor compose p
		override def andThenOpt[Z](extractor :OptionalExtractor[Y, Z]) :X =?> Z = extractor compose p
		override def andThenReq[Z](extractor :RequisiteExtractor[Y, Z]) :RequisiteExtractor[X, Z] = extractor compose p

		override def compose[W](extractor :W =?> X) :W =?> Y = extractor andThen p
		override def composeOpt[W](extractor :OptionalExtractor[W, X]) :W =?> Y = extractor andThen p
		override def composeReq[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, Y] = extractor andThen p
	}




	/** An identity function as a `RequisiteExtractor`. */
	trait IdentityExtractor[X] extends RequisiteExtractor[X, X] {
		override val optional :X => Some[X] = identityOptional.asInstanceOf[X => Some[X]]

		override def apply(x :X) :X = x
		override def opt(x :X) :Opt[X] = Got(x)

		override def andThen[Z](extractor :X =?> Z) :X =?> Z = extractor
		override def andThenOpt[Z](extractor :OptionalExtractor[X, Z]) :X =?> Z = extractor
		override def andThenReq[Z](extractor: RequisiteExtractor[X, Z]) :RequisiteExtractor[X, Z] = extractor
		override def andThen[Z](f :X => Z) :RequisiteExtractor[X, Z] = Extractor.req(f)
		override def andThenOpt[Z](f :X => Option[Z]) :X =?> Z = Extractor.opt(f)

		override def compose[W](extractor :W =?> X) :W =?> X = extractor
		override def composeOpt[W](extractor :OptionalExtractor[W, X]) :W =?> X = extractor
		override def composeReq[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, X] = extractor
		override def compose[W](f :W => X) :RequisiteExtractor[W, X] = Extractor.req(f)
		override def composeOpt[W](f :W => Option[X]) :Extractor[W, X] = Extractor.opt(f)

		override def isIdentity = true
		override def toString = "Identity"
	}

	private[this] final val identityGetter = identity[Any] _
	private[this] final val identityOptional = (x :Any) => Some(x)
	private[this] final val identityRequisite = Got((x :Any) => x)




	/** An `Extractor` ignoring its arguments and always returning the same value.  */
	trait ConstantExtractor[-X, +Y] extends RequisiteExtractor[X, Y] {
		def constant :Y
		//declarations final to assure both functions and method use erased arguments and hence can be cast to Any =>
//		final override val getter = { val c = constant; _ :X => c }
		final override val optional = { val res = Some(constant); _ :X => res }

		final override def apply(x :X) :Y = constant
		final override def opt(x :X) :Opt[Y] = Got(constant)

		override def andThenOpt[Z](extractor :OptionalExtractor[Y, Z]) :X =?> Z = andThenOpt(extractor.optional)

		override def andThen[Z](f :Y => Z) :RequisiteExtractor[X, Z] =
			try { const(f(constant)) }
			catch { case _ :Exception => super.andThen(f) } //Extractor.req(this andThen extractor.getter)

		override def andThenOpt[Z](f :Y => Option[Z]) :X =?> Z =
			try {
				f(constant) match {
					case Some(z) => const(z)
					case _ => none
				}
			} catch {
				case _ :Exception => super.andThenOpt(f)
			}

		override def composeOpt[W](extractor :OptionalExtractor[W, X]) :W =?> Y = composeOpt(extractor.optional)
		override def composeReq[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, Y] =
			this.asInstanceOf[RequisiteExtractor[W, Y]]

		override def compose[W](req :W => X) :RequisiteExtractor[W, Y] = this.asInstanceOf[RequisiteExtractor[W, Y]]


		override def toString :String = "Const(" + constant + ")"
	}


	private[oldsql] class ConstantAdapter[+Y](const :Y) extends ConstantExtractor[Any, Y] {
		override def constant :Y = const
	}




	/** An `Extractor` which never produces any value, always returning `None` from its `opt` method. */
	trait EmptyExtractor[-X, +Y] extends Extractor[X, Y] {
		override def optional :Any => Option[Nothing] = emptyOptional

		override def apply(x :X) :Y = throw new NoSuchElementException(toString + ".apply(" + x + ")")
		override def opt(x :X) :Opt[Y] = Lack

		override def andThen[Z](extractor :Y =?> Z) :X =?> Z = none
		override def andThenOpt[Z](extractor :OptionalExtractor[Y, Z]) :X =?> Z = none
		override def andThenReq[Z](extractor :RequisiteExtractor[Y, Z]) :X =?> Z = none
		override def andThen[Z](f :Y => Z) :X =?> Z = none
		override def andThenOpt[Z](f :Y => Option[Z]) :X =?> Z = none

		override def compose[W](extractor :W =?> X) :W =?> Y = none
		override def composeOpt[W](extractor :OptionalExtractor[W, X]) :W =?> Y = none
		override def composeReq[W](extractor :RequisiteExtractor[W, X]) :W =?> Y = none
		override def compose[W](req :W => X) :W =?> Y = none
		override def composeOpt[W](f :W => Option[X]) :W =?> Y = none

		override def toString :String = "Lack"
	}

	private[this] val emptyOptional = (_ :Any) => None






	private class OptionExtractor[X] extends OptionalExtractor[Option[X], X] {
		override def optional :Option[X] => Option[X] = identity[Option[X]]

		override def apply(x :Option[X]) :X = x.get
		override def opt(x :Option[X]) :Opt[X] = x

		protected override def andThenCallback[Z](extractor :X =?> Z) :Option[X] =?> Z =
			Optional {
				case Some(x) => extractor.opt(x)
				case _ => Lack
			}
		override def andThenOpt[Z](f :X => Option[Z]) :Option[X] =?> Z = Extractor.opt(_.flatMap(f))

		override def composeReq[W](extractor :RequisiteExtractor[W, Option[X]]) :W =?> X =
			Extractor.opt(extractor.getter)

		override def compose[W](f :W => Option[X]) :W =?> X = Extractor.opt(f)

		override def toString = "_.opt"
	}


}



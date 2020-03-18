package net.noresttherein.oldsql.morsels

import net.noresttherein.oldsql.morsels.Extractor.{IdentityExtractor, OptionExtractor, RequisiteExtractor}



/** A wrapper over an option-returning function `X=>Option[Y]`. It serves three main purposes:
  *   - as a base class for extractor objects declaring `unapply`;
  *   - differentiation between 'requisite' extractors, i.e. functions `X=>Some[Y]`;
  *   - composition of extractors (by default via flattening) which is aware of requisite and identity extractors
  *     and create a suitable (more effective) composition with them.
  * Note that this is a ''SAM'' type, meaning a compatible function expression in a position where an `Extractor`
  * is expected will be converted to one by the compiler.
  * @see [[net.noresttherein.oldsql.morsels.Extractor.=?> =?>]]
  * @see [[net.noresttherein.oldsql.morsels.Extractor.RequisiteExtractor RequisiteExtractor]]
  * @author Marcin Mościcki
  */
trait Extractor[-X, +Y] {
	def optional :X => Option[Y] = get
	def requisite :Option[X => Y] = None

	def apply(x :X) :Y

	def get(x :X) :Option[Y]

	def unapply(x :X) :Option[Y] = get(x)

	def andThen[Z](extractor :Extractor[Y, Z]) :Extractor[X, Z] = extractor match {
		case _ :IdentityExtractor[_] => extractor compose this
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



	override def toString :String = "Extractor@" + System.identityHashCode(this)

}






object Extractor {
	type =?>[-X, +Y] = Extractor[X, Y]


	def apply[X, Y](extract :X => Option[Y], requisite: Option[X => Y]) :Extractor[X, Y] = requisite match {
		case Some(f) => new RequisiteAdapter(f)
		case _ => new OptionalAdapter(extract)
	}


	def apply[X, Y](extract :X => Option[Y]) :Extractor[X, Y] = new OptionalAdapter(extract)

	def req[X, Y](extract :X => Y) :RequisiteExtractor[X, Y] = new RequisiteAdapter(extract)

	def ident[X] :IdentityExtractor[X] = id.asInstanceOf[IdentityExtractor[X]]

	private[this] val id = new IdentityExtractor[Any] {}



	def fromOpt[X] :Extractor[Option[X], X] = opt.asInstanceOf[Extractor[Option[X], X]]

	private[this] val opt = new OptionExtractor[Any]



	def unapply[X, Y](extractor :Extractor[X, Y]) :Some[X => Option[Y]] = Some(extractor.optional)



	implicit def functionExtractor[X, Y](f :X => Option[Y]) :Extractor[X, Y] = apply(f)

	implicit def requisiteExtractor[X, Y](f :X => Y) :RequisiteExtractor[X, Y] = req(f)






	object Requisite {
		@inline def apply[X, Y](f :RequisiteExtractor[X, Y]) :RequisiteExtractor[X, Y] = f

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Option[X => Y] = extractor.requisite
	}


	object Optional {
		@inline def apply[X, Y](f :OptionalExtractor[X, Y]) :OptionalExtractor[X, Y] = f

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Option[X => Option[Y]] =
			if (extractor.requisite.isEmpty) Some(extractor.optional)
			else None
	}






	trait OptionalExtractor[-X, +Y] extends Extractor[X, Y] {
		override def apply(x :X) :Y = get(x) getOrElse {
			throw new NoSuchElementException("No value for " + this + " in " + x)
		}

		override def andThen[Z](extractor :Extractor[Y, Z]) :Extractor[X, Z] = extractor match {
			case _ :IdentityExtractor[_] => extractor compose this
			case sure :RequisiteExtractor[Y, Z] =>
				val first = optional; val second = sure.getter
				Extractor { x :X => first(x).map(second) }
			case _ =>
				val first = optional; val second = extractor.optional
				Extractor { x :X => first(x).flatMap(second) }
		}

	}



	trait RequisiteExtractor[-X, +Y] extends Extractor[X, Y] {
		def getter :X => Y = apply
		override def optional :X => Some[Y] = (x :X) => Some(apply(x))
		override def requisite :Some[X=>Y] = Some(getter)

		def apply(x :X) :Y

		override def get(x :X) :Some[Y] = Some(apply(x))



		override def andThen[Z](extractor :Extractor[Y, Z]) :Extractor[X, Z] = extractor match {
			case _ :IdentityExtractor[_] => extractor compose this
			case sure :RequisiteExtractor[Y, Z] =>
				Extractor.req(this.getter andThen sure.getter)
			case _ :OptionExtractor[_] =>
				Extractor(this.getter.asInstanceOf[X => Option[Nothing]]).asInstanceOf[Extractor[X, Z]]
			case _ =>
				Extractor(this.getter andThen extractor.optional)
		}

		def andThen[Z](extractor :RequisiteExtractor[Y, Z]) :RequisiteExtractor[X, Z] = extractor match {
			case _ :IdentityExtractor[_] => extractor compose this
			case _ => Extractor.req(this.getter andThen extractor.getter)
		}




		def compose[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, Y] = extractor.andThen[Y](this)

		override def compose[W](req :W => X) :RequisiteExtractor[W, Y] = Extractor.req(req andThen getter)


		override def toString :String = "Requisite@" + System.identityHashCode(this)
	}






	trait IdentityExtractor[X] extends RequisiteExtractor[X, X] {
		override val getter = identity[X] _
		override val optional = (x :X) => Some(x)
		override val requisite = Some(identity[X] _)

		override def apply(x :X) :X = x

		override def get(x :X) :Some[X] = Some(x)

		override def andThen[Z](extractor :Extractor[X, Z]) :Extractor[X, Z] = extractor
		override def compose[W](extractor :Extractor[W, X]) :Extractor[W, X] = extractor
		override def andThen[Z](extractor :RequisiteExtractor[X, Z]) :RequisiteExtractor[X, Z] = extractor
		override def compose[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, X] = extractor
//		override def andThen[Z](req :X => Z) :RequisiteExtractor[X, Z] = Extractor.requisite(req)
		override def compose[W](req :W => X) :RequisiteExtractor[W, X] = Extractor.req(req)

		override def toString = "Identity"
	}






	private class OptionalAdapter[-X, +Y](override val optional :X => Option[Y]) extends OptionalExtractor[X, Y] {
		override def get(x :X) :Option[Y] = optional(x)
	}



	private class RequisiteAdapter[-X, +Y](override val getter :X=>Y) extends RequisiteExtractor[X, Y] {
		override val optional = { x :X => Some(getter(x)) }
		override val requisite = Some(getter)

		override def apply(x :X) :Y = getter(x)

		override def get(x :X) = Some(getter(x))
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



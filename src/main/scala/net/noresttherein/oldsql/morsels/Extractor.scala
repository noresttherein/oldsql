package net.noresttherein.oldsql.morsels

import net.noresttherein.oldsql.morsels.Extractor.{IdentityExtractor, RequisiteExtractor}


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
	def optional :X => Option[Y] = apply
	def requisite :Option[X => Y] = None

	def apply(x :X) :Option[Y]

	def unapply(x :X) :Option[Y] = apply(x)

	def andThen[Z](extractor :Extractor[Y, Z]) :Extractor[X, Z] = extractor match {
		case _ :IdentityExtractor[_] => extractor compose this
		case sure :RequisiteExtractor[Y, Z] =>
			val first = optional; val second = sure.extractor
			Extractor { x :X => first(x).map(second) }
		case _ =>
			val first = optional; val second = extractor.optional
			Extractor { x :X => first(x).flatMap(second) }
	}

	def compose[W](extractor :Extractor[W, X]) :Extractor[W, Y] = extractor andThen this
}



object Extractor {
	type =?>[-X, +Y] = Extractor[X, Y]


	def apply[X, Y](extract :X => Option[Y]) :Extractor[X, Y] = new Extractor[X, Y] {
		override def optional = extract
		override def apply(x :X) = extract(x)
	}



	def requisite[X, Y](extract :X => Y) :RequisiteExtractor[X, Y] = new RequisiteExtractor[X, Y] {
		override def extractor :X => Y = extract
		override val optional = (x :X) => Some(extract(x))
		override val requisite = Some(extract)

		override def get(x :X) :Y = extract(x)
		override def apply(x :X) :Some[Y] = Some(extract(x))
	}



	def ident[X] :IdentityExtractor[X] = id.asInstanceOf[IdentityExtractor[X]]

	private[this] val id = new IdentityExtractor[Any] {}



	def unapply[X, Y](extractor :Extractor[X, Y]) :Some[X => Option[Y]] = Some(extractor.optional)



	implicit def functionExtractor[X, Y](f :X => Option[Y]) :Extractor[X, Y] = apply(f)

	implicit def requisiteExtractor[X, Y](f :X => Y) :RequisiteExtractor[X, Y] = requisite(f)






	trait RequisiteExtractor[-X, +Y] extends Extractor[X, Y] {
		def extractor :X => Y = get
		override def optional :X => Some[Y] = (x :X) => Some(get(x))
		override def requisite :Some[X=>Y] = Some(extractor)

		def get(x :X) :Y

		def apply(x :X) :Some[Y] = Some(get(x))

		override def andThen[Z](extractor :Extractor[Y, Z]) :Extractor[X, Z] = extractor match {
			case _ :IdentityExtractor[_] => extractor compose this
			case sure :RequisiteExtractor[Y, Z] =>
				val first = this.extractor; val second = sure.extractor
				Extractor.requisite(first andThen second)
			case _ =>
				val first = this.extractor; val second = extractor.optional
				Extractor(first andThen second)
		}

		def andThen[Z](extractor :RequisiteExtractor[Y, Z]) :RequisiteExtractor[X, Z] = extractor match {
			case _ :IdentityExtractor[_] => extractor compose this
			case _ => Extractor.requisite(this.extractor andThen extractor.extractor)
		}

		def compose[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, Y] = extractor andThen this
	}



	object RequisiteExtractor {
		@inline def apply[X, Y](f :X=>Y) :RequisiteExtractor[X, Y] = requisite(f)

		@inline def unapply[X, Y](extractor :Extractor[X, Y]) :Option[X => Y] = extractor.requisite
	}


	trait IdentityExtractor[X] extends RequisiteExtractor[X, X] {
		override val extractor = identity[X] _
		override val optional = (x :X) => Some(x)
		override val requisite = Some(identity[X] _)

		def get(x :X) :X = x

		override def apply(x :X) :Some[X] = Some(x)

		override def andThen[Z](extractor :Extractor[X, Z]) :Extractor[X, Z] = extractor
		override def compose[W](extractor :Extractor[W, X]) :Extractor[W, X] = extractor
		override def andThen[Z](extractor :RequisiteExtractor[X, Z]) :RequisiteExtractor[X, Z] = extractor
		override def compose[W](extractor :RequisiteExtractor[W, X]) :RequisiteExtractor[W, X] = extractor
	}
}


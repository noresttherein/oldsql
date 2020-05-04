package net.noresttherein.oldsql.morsels.witness



import scala.annotation.implicitNotFound



/** An implicit evidence class witnessing that there is an implicit value available for `E1`, `E2` or both.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@implicitNotFound("Can't witness (${E1} Or ${E2}): no implicit value available for either type parameter.")
final class Or[E1, E2] private[witness](
   /** Implicit value available for `E1`, if present in the referencing context. */ val first :Option[E1],
   /** Implicit value available for `E2`, if present in the referencing context. */ val second :Option[E2]
)



sealed abstract class IndividualOrEvidence {
	implicit def witnessFirst[E1, E2](implicit e1 :E1): E1 Or E2 = new Or(Some(e1), None)
	implicit def witnessSecond[E1, E2](implicit e2 :E2): E1 Or E2 = new Or(None, Some(e2))
}



object Or extends IndividualOrEvidence {

	implicit def witnessBoth[E1, E2](implicit e1 :E1, e2 :E2): E1 Or E2 = new Or(Some(e1), Some(e2))


	object First {
		def apply[E1](implicit ev :E1 Or _) :Option[E1] = ev.first

		def unapply[E1, E2](or :E1 Or E2) :Option[E1] = or.first
	}

	object Second {
		def apply[E2](implicit ev :_ Or E2) :Option[E2] = ev.second

		def unapply[E1, E2](or :E1 Or E2) :Option[E2] = or.second
	}

	object Both {
		def unapply[E1, E2](or :E1 Or E2) :Option[(E1, E2)] = (or.first, or.second) match {
			case (Some(first), Some(second)) => Some(first, second)
			case _ => None
		}
	}
}






/** Witnesses the existence of both `X` and `Y` implicit values. Declaring an implicit parameter
  * `(implicit both :X And Y)` is equivalent to declaring `(implicit x :X, y :Y)`, but the conjunction can still
  * be useful for creating more complex boolean formulas with implicit parameters as predicates.
  * Additionally, where one would normally introduce a new evidence type derived from existence of
  * some other implicit values, one can now simply declare a type alias for (possibly nested)
  * `And` values, saving the coding of a class, companion object and implicit values altogether.
  *
  * @author Marcin Mościcki
  */
@implicitNotFound("Can't witness (${X} And ${Y}): missing implicit value for one of these types.")
class And[X, Y] private[witness] (val first :X, val second :Y) {
	def isEmpty = false
	def get :(X, Y) = first -> second
}



object And {

	@inline implicit def both[X, Y](implicit x :X, y :Y) :X And Y = new And(x, y)

	@inline def unaply[X, Y](and :X And Y) :X And Y = and
}






/** An evidence class witnessing that no implicit value of type `P` is present.
  * For any type `P` and any scope, an implicit value of `Not[P]` is available ''iff'' no implicit value for `P` exists
  * in that scope.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@implicitNotFound("Cannot prove that Not[${P}] as evidence for ${P} exists")
final class Not[P] private ()



object Not {
	private[this] val not = new Not[Any]

	@inline implicit def default[P] :Not[P] = not.asInstanceOf[Not[P]]

	@inline implicit def evidenceFound[P](implicit ev :P) :Not[P] = default[P]

}


package com.hcore.ogre.model

import com.hcore.ogre.model.Reference.Satisfying
import org.scalatest.{Matchers, FlatSpec}



class FlattenedReferenceSpec  extends FlatSpec with Matchers {

	case class Drunkard(name :String, pubs: Reference[Seq[Pub]], drinks :Reference[Seq[Beer]])
	case class Pub(id :Long, name :String, serves :Reference[Seq[Beer]])
	case class Beer(pubId :Long, name :String)

	import com.hcore.ogre.model.Restriction.Restrictive

	val beersInPubs = Satisfying(((_:Beer).pubId)==?)
	val pubs = Satisfying(((p:Pub) => p.id) ==?)


}

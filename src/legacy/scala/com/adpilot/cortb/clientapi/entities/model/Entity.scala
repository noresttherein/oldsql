package com.adpilot.cortb.clientapi.entities.model


trait Domain {
	type Source
	
	trait Entity[E] {
		type Relation[X] = Relationship[E, X]
		
		def relations :Seq[Relation[_]]		
	}

	trait Identity[E, I] {
		type Id = I

		def apply(value :E) :Option[Id]
		def fetch(id :I)(implicit source :Source) :Option[E]
	}

	trait PrimaryKeyEntity[E, PK] extends Entity[E] {
		val pk :Identity[E, PK]

	}


	trait EntityLink[L, R] {
		def left :Entity[L]
		def right :Entity[R]
		def inverse :Option[EntityLink[R, L]]

		def values(left :L) :Iterable[R]
	}

	trait EntityCollection[L, R, C<:Iterable[R]] extends EntityLink[L, R] {
		def values(left :L) = apply(left)

		def apply(left :L) :C
	}

	trait ToOne[L, R] extends EntityLink[L, R] {
		def apply(left :L) :R
	}





	sealed trait Relationship[L, R] {

		def left :Entity[L]
		def right :Entity[R]
		def inverse :Relationship[R, L]
		
		def values(left :L) :Option[Iterable[R]]
		def fetch(left :L)(implicit source :Source) :Iterable[R]
	}
	
	trait _ToOne[L, R] extends Relationship[L, R] {
//		def values(left :L) :Option[Option[R]]
//		def fetch(left :L)(implicit source :Source) :Option[R]
	}
	
	trait _ToExactlyOne[L, R] extends _ToOne[L, R] {
//		def values(left :L) :Option[Option[R]] = get(left).map(Some(_))
		def get(left :L) :Option[R]
	}
	
	trait _ToMany[L, R] extends Relationship[L, R]
		
	
	
	trait ManyToOne[M, O] extends Relationship[M, O] with _ToOne[M, O] {
		def many :Entity[M] = left
		def one :Entity[O] = right
		def inverse :OneToMany[O, M]

		def unapply(many :M) :Option[Option[O]]
	}
	
	trait ManyToExactlyOne[M, O] extends ManyToOne[M, O] with _ToExactlyOne[M, O]

	trait OneToMany[O, M] extends Relationship[O, M] {
		def one :Entity[O] = left
		def many :Entity[M] = right

		def inverse :ManyToExactlyOne[M, O]
	}

	trait ManyToMany[L, R] extends Relationship[L, R] {
		def inverse :ManyToMany[R, L]
	}

	trait OneToOne[L, R] extends Relationship[L, R] {
		def inverse :OneToOne[R, L]		
	}

	sealed trait Cascade
	object Delete extends Cascade
	object Create extends Cascade
	object Update extends Cascade
	object Lifecycle extends Cascade
	
}



trait AbstractDomain extends Domain {
	type Source = Nothing
	
	trait NoFetch[L, R] extends Relationship[L, R] {
		def fetch(left :L)(implicit source :Source) = ??? //values(left) getOrElse ???
	}
}




object static extends AbstractDomain
package com.adpilot.cortb.clientapi.prototype.repository

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{IdRef, HasId, Id}
import com.adpilot.cortb.clientapi.prototype.repository.entities._
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference._
import com.adpilot.cortb.clientapi.util.ObjectProperty


import scala.collection.mutable
import scala.reflect.ClassTag

/*
class MockRepository extends Repository { repository =>
	import MockRepository._

	type Session = MockRepository

	def newSession :Session = this



	type Store[E<:HasId] = MockStore[E]

	implicit val Products: Store[Product] = new MockStore[Product](IdProperty[Product]((p, id) => p.copy(id=id)))

	val ProductTags: Store[Tag] = new MockStore[Tag](IdProperty[Tag]((p, id) => p.copy(id=id)))

	implicit val Orders: Store[Order] = new MockStore[Order](
		IdProperty[Order]((p, id) => p.copy(id=id)),
		Order(None, null, null, One(), One(), 0, 0, null, null, null, None, null, null, null, None, None, null, false, None, None, None, None),
		new ManyToOne[Order, Campaign](Orders, Campaigns, _.campaign, (o, c) => o.copy(campaign=c)),
		new ManyToOne[Order, TargetingPolicy](Orders, TargetingPolicies, _.targetingPolicy, (o, p) => o.copy(targetingPolicy=p))
	)

	implicit val TargetingPolicies: Store[TargetingPolicy] = new MockStore[TargetingPolicy](IdProperty[TargetingPolicy]((p, id) => p.copy(id=id)))


	implicit val Creatives: Store[Creative] = new MockStore[Creative](
		IdProperty[Creative]((p, id) => p.copy(id=id)),
		Creative(None, null, null, null, null, null, null, null, One()),
		new ManyToOne[Creative, Product](Creatives, Products, _.product, (c, p) => c.copy(product=p))
	)

	implicit val Landings :Store[Landing] = new MockStore[Landing](
		IdProperty[Landing]((l, id) => l.copy(id=id)),
		Landing(None, null, null, One(), None, null, None, null),
		new ManyToOne[Landing, Campaign](Landings, Campaigns, _.campaign, (l, c) => l.copy(campaign=c))
	)


	implicit val Campaigns: Store[Campaign] = new MockStore[Campaign](
		IdProperty[Campaign]((p, id) => p.copy(id = id)),
		Campaign(None, null, null, One(), One(), 0, 0, null, null, null, None, null, null, None, None, None, None),
		new ManyToOne[Campaign, Product](Campaigns, Products, _.product, (c, p) => c.copy(product=p)),
		new ManyToOne[Campaign, TargetingPolicy](Campaigns, TargetingPolicies, _.targetingPolicy, (c, p) => c.copy(targetingPolicy=p))
	)

	val CampaignTags: Store[Tag] = new MockStore[Tag](IdProperty[Tag]((p, id) => p.copy(id=id)))





	override def apply[E <: HasId](ref: One[E], refs: (E => Reference[Any])*)(implicit store: Store[E], session: Session): Option[E] =
		store.mock.map{ mock =>
			val singles = refs.collect{ case property if property(mock).isInstanceOf[One[_]] => property.asInstanceOf[E => One[Any]] }
			val entity = store(ref, singles:_*)
			entity //todo: Many[_] properties
		}  getOrElse store(ref)


	override def apply[E <: HasId, T <: HasId](ownerOf: (E) => One[T], owner: One[T])(implicit elems: Store[E], owners :Store[T], session: Session): Seq[E] =
		owners(owner).toSeq.flatMap(_.id).flatMap(elems(ownerOf, _))


	override def save[E <: HasId](entity: E)(implicit store: Store[E], session: Session): E =
		store.save(entity) //todo: cascades



	override def delete[E <: HasId](ref: One[E])(implicit store: Store[E], session: Session): Unit =
		store.delete(ref) //todo :cascades








	class MockStore[E<:HasId :ClassTag] private (pk :EntityProperty[E, Option[Id]], private[MockRepository] val mock :Option[E], relations :ManyToOne[E, _<:HasId]*)
		extends EntityStore[E]
	{
		private[MockRepository] val repo = repository

		def this(id :EntityProperty[E, Option[Id]], mock :E, relations :ManyToOne[E, _<:HasId]*) =
			this(id, Some(mock), relations:_*)

		def this(id :EntityProperty[E, Option[Id]]) =
			this(id, None)

		private[this] val byId = mutable.Map[Id, E]()
		private[MockRepository] val byFK = mutable.Map[(ManyToOne[E, _], Id), Seq[E]]().withDefaultValue(Seq())

		private def all = synchronized{ byId }.values 

		override def apply(ref: One[E], refs: (E => One[Any])*)(implicit s: Session): Option[E] = synchronized {
			val matching = ref match {
				case Full(e) if e.id.isDefined => byId.get(e.id.get).toSeq
				case UniquePropertyReference(pk.Property, key :Id) => byId.get(key).toSeq
				case UniquePropertyReference(prop, key) => all.filter(prop(_)==key).toSeq
				case _ => throw new IllegalArgumentException(s"Can't retrieve entity: unsupported reference: $ref")
			}
			matching match {
				case Seq() => None
				case Seq(obj) =>
					relationsRoot().map{ case root =>
						val mocks = refs.map(_(root)).map(_.asInstanceOf[MockRef[_]])
						(obj /: mocks)((o, r) => fetch(o, r.path.reverse))
					} orElse Some(obj)
				case _ => throw new IllegalArgumentException(s"reference $ref doesn't uniquely identify an entity. Matching objects: $matching")
			}
		}


//		override def apply[T <: HasId](owner: (E) => One[T], ref: One[T])(implicit s: Session): Seq[E] = ref match {
//			case IdRef(id) => apply(owner, id)
//			case _ => apply(ref).toSeq.flatMap(obj => apply(owner, obj.id.get))
//		}


		override def apply[T <: HasId](owner: (E) => One[T], ownerId: Id)(implicit s :Session): Seq[E] =
			relationsRoot().flatMap { case r =>
				owner(r) match {
					case MockRef(_, path @ Seq(last, rest @_*)) if relations.contains(last) =>
						synchronized { byFK }.get((last.asInstanceOf[ManyToOne[E, _<:HasId]], ownerId))
					case _ => throw new IllegalArgumentException("MockStore.apply(ref, pk): passed reference is not part of argument entity")
				}
			} getOrElse Seq()

		


		override def save(entity: E)(implicit s: Session): E = {
			val id = this += entity
			pk.set(entity, id)
		}

		override def +=(entity: E)(implicit s :Session) : Option[Id] = synchronized {
			def add(e :E) = {
				val persistent = (e /: relations)((e, r) => r.withEmptyRef(e))
				val id = persistent.id.get
				byId(id) = persistent
				relations.foreach { r =>
					val key = (r, r.foreignKey(e))
					byFK(key) = persistent +: byFK(key)
				}
				Some(id)
			}

			pk.get(entity) match {
				case None =>
					val saved = pk.set(entity, Some(nextId))
					add(saved)
				case Some(id) if byId.contains(id) =>
					this -= entity
					add(entity)
				case id => throw new IllegalArgumentException(s"MockRepository.+=(): entity with id ${id} is not present in this store. Entity: $entity")
			}
		}


		override def delete(ref: One[E])(implicit s: Session): Unit = synchronized {
			apply(ref).foreach { entity =>
				val id = entity.id.get
				for (rel <- relations) {
					val key =  (rel, rel.foreignKey(entity))
					byFK(key) = byFK(key).filterNot(_.id==id)
				}
				byId.remove(id)
			}
		}

//		override def delete(id :Id)(implicit s :Session) : Unit = synchronized {
//			for (entity <- byId.get(id); rel <- relations) {
//				val key =  (rel, rel.foreignKey(entity))
//				byFK(key) = byFK(key).filterNot(_.id==id)
//			}
//			byId.remove(id)
//		}


		override def delete[T <: HasId](owner: (E) => One[T], ref: One[T])(implicit store :Store[T], s: Session): Unit = synchronized {
			val elems = repository.apply(owner :(E=>One[T]), ref :One[T])(this, store, s)
			elems.foreach(e => delete(e.id.get))
		}


		override def delete[T <: HasId](ref: (E) => One[T], id: Id)(implicit s :Session) : Unit = synchronized {
			val elems = apply(ref, id)
			elems.foreach(e => delete(e.id.get))
		}

		protected[MockRepository] def fetch[T<:HasId](root :E, path :Seq[ManyToOne[_, _]]) :E = path match {
			case Seq() => root
			case Seq(next, rest @_*) if relations.contains(next) =>
				next.asInstanceOf[ManyToOne[E, _]].fetch(root, rest)

		}


		protected[MockRepository] def relationsRoot(path :Seq[ManyToOne[_, _]]=Seq()) :Option[E] = mock.map(obj => (obj /: relations)( (e, rel) => rel.navigable(e, rel+:path)))


		private[this] var idGenerator :Long = 0

		private def nextId = synchronized{
			val id = idGenerator
			idGenerator += 1
			Id(id)
		}

	}

}








object MockRepository {
	class EntityProperty[E :ClassTag, P](val get :E=>P, val set :(E, P)=>E) {
		val Property = ObjectProperty(get)

		def apply(obj :E) = get(obj)
//		def update(obj :E, value :P) = set(obj, value)
	}
	
	class ManyToOne[C<:HasId :ClassTag, P <: HasId :ClassTag](child : =>MockRepository#Store[C], parent : =>MockRepository#Store[P], get :C=>One[P], set :(C, One[P])=>C)
		extends EntityProperty[C, One[P]](get, set)
	{

		def one = parent
		def many = child

		def foreignKey(obj :C) :Id = get(obj) match {
			case IdRef(id) => id
			case ref => throw new IllegalArgumentException(s"$ref: no foreign key specified for $Property in $obj")
		}

		def withEmptyRef(obj :C) :C = set(obj, IdRef[P](get(obj)))

		def navigable(obj :C, path :Seq[ManyToOne[_, _]]) :C =
			set(obj, one.relationsRoot(path).map(obj => MockRef(obj, path)) getOrElse MockRef(path))
		
		def fetch[T <: HasId](obj :C, path :Seq[ManyToOne[_, _]]) :C = one.repo.inSession { implicit s =>
			val ref = get(obj)
			set(obj, IdRef[P](one.fetch(ref getOrElse one(ref).get, path)))
		}
	}
	
	def IdProperty[E <:HasId :ClassTag](set :(E, Option[Id])=>E) = new EntityProperty[E, Option[Id]](_.id, set)

	private case class MockRef[T<:HasId](mock :Option[T], path :Seq[ManyToOne[_, _]]) extends One[T] {
		override def toOpt = None
		override def join = mock getOrElse (throw new UnsupportedOperationException(s"can't join ${path.last.one} with anything. JoinLike path: $path"))
	}

	private object MockRef {
		def apply[T<:HasId](route :Seq[ManyToOne[_, _]]) :MockRef[T] =
			new MockRef(None, route)

		def apply[E<:HasId](entity :E, route :Seq[ManyToOne[_, _]]) :MockRef[E] =
			new MockRef(Some(entity), route)

//		def unapply[T](ref :One[T]) = ref match {
//			case m:MockRef[_] => Some(m.path)
//			case _ => None
//		}
	}
}
*/

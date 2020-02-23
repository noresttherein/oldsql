package com.hcore.ogre.repository

import com.hcore.clientapi.entities.Model.{HasId, Id, IdRef}
import com.hcore.ogre.mapping.Mapping.SetParameters
import com.hcore.ogre.mapping.SchemaMapping
import com.hcore.ogre.model.Restriction.{PropertyEquals, PropertyIn}
import com.hcore.ogre.model.{ComposedOf, Reference}
import com.hcore.ogre.model.Reference._
import com.hcore.ogre.slang.matching
import matching.&&
import com.hcore.ogre.morsels.Time
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.morsels.necromancy.PropertyChain.UpdatableProperty

import scala.collection.generic.CanBuildFrom
import scala.reflect.runtime.universe.TypeTag
import scala.slick.jdbc.{GetResult, StaticQuery => Q, StaticQueryInvoker}
import scala.util.Try

trait MappedSchemaRepository extends Repository { repository =>
	protected val schema :SchemaMapping
	import schema._
	
	type Store[E<:HasId] = TableStore[E]
	type Session = schema.Session
	type AnyProperty[X] = PropertyChain[X, Any]

	protected def newSession: Session = schema.newSession

	override def inSession[T](block: (Session) => T): T = {
		val session = newSession
		val res =
			try {
				block(session)
			}catch {
				case e :Exception =>
					Try{ session.close() }
					throw e
			}
		session.close()
		res
	}

	protected def IdProperty[E <:HasId :TypeTag](set :(E, Option[Id])=>E) :UpdatableProperty[E, Option[Id]] = UpdatableProperty[E, Option[Id]](_.id, set)





	def apply[E <:HasId, T](selection :Reference[T], fetch :(E=>Reference[Any])*)(implicit store :Store[E], composition :T ComposedOf E, session :Session) :T = {
		val (ones, rest) = fetch.partition(store.isForeignKeyPath)
		val manys = rest.map(store.cascadeFor).collect {
			case Some(cascade) => cascade
			case None =>
				//				System.err.println(s"requested ${ones.map(ObjectProperty.name)} + ${rest.map(ObjectProperty.name)} -> ${rest.map(store.cascadeFor)}")
				throw new IllegalArgumentException(s"attempted to select $selection from ${store.table} with unknown references")
		}

		val found = Time(s"selecting $selection(${store.table}) with ${ones.map(_(store.table.joinRoot))}"){
			store(selection, ones.asInstanceOf[Seq[E=>Reference[Any]]]:_*)
		}
		val entities = composition(found)

		val res = (entities /: manys)((loaded, many) => Time(s"loading $many for $selection")(many.load(loaded)))
//		val res = Time(s"fetching relations for $selection: $manys"){
//			entities.map(entity => (entity /: manys)((res, many) => many.load(res)))
//		} //todo: n+1 selects! do a subselect query

		composition(res)
	}




	override def save[E <: HasId](entity: E)(implicit store: Store[E], session: Session): E = {
		System.err.println(s"prepare to save $entity")
		val resolved = (entity /: store.foreignKeys){ (e, fk) => fk.resolve(e) }
		val prepared = (resolved /: store.cascades){ (e, c) => System.err.println(s"saving ${store.table}, pre-save cascade $c"); c.preSave(e) }
		System.err.println(s"saving ${store.table}: $prepared")
		val saved = store.save(prepared)
		(saved /: store.cascades){ (e, c) => System.err.println(s"saving ${store.table}, post-save cascade $c"); c.save(e) }
	}



	override def delete[E <: HasId](id: Id)(implicit store: Store[E], session: Session): Unit = {
		for (cascade <- store.cascades)
			cascade.delete(id)
		store.delete(id)
	}

	//	def delete[E<:HasId](ref :One[E])(implicit store :Store[E], session :Session) :Unit = {
	//		for (cascade <- store.cascades)
	//			cascade.delete(ref)
	//		store.delete(ref)
	//	}


	override def delete[E <: HasId, T](selection: Reference[T])(implicit store: Store[E], composition :T ComposedOf E, session: Session): Unit = {
		//todo :cascade delete!
		store.delete(selection)
	}




	class TableStore[E<:HasId :TypeTag](val table :schema.Table[E, Option[Id]],
	                                    val IdProperty :UpdatableProperty[E, Option[Id]],
	                                    private[MappedSchemaRepository] val cascades :Cascade[E, _]*)(
	                                    val foreignKeys :ForeignKey[E, _]*
		)
		extends EntityStore[E]
	{
//		private implicit val Decompose = CompositeReference.DecomposeTo[E]

		override def toString = table.toString

		private[MappedSchemaRepository] def isForeignKeyPath(relation :E=>Reference[Any]) =
			table.ToOne.is(PropertyChain(relation))


		private[MappedSchemaRepository] def cascadeFor(relation :E=>Reference[Any]) = {
			val property = PropertyChain[E](relation)
			cascades.find(_.property == property)
		}

		@deprecated("the whole thing is going down the drain", "now")
		private[MappedSchemaRepository] def ComponentValue(ref :Reference[E]) = ref match {
			case Single(PropertyEquals(table.PropertyComponent(comp), key)) => comp.withValue(key)
			case _ => throw new IllegalArgumentException(s"can't identify $table by $ref")
		}




		def apply[T](select :Reference[T], fetch : (E => Reference[Any])*)(implicit composition :T ComposedOf E, s :Session): T = {
			select match {
//				case Satisfying(composition(PropertyIn(property, keys))) =>
//					val filter = table.joinFilter(property, keys.toSeq)
//					val joins = filter.join ++: fetch
//					val query = table.joinSelect(Some(filter.whereClause), joins:_*)(filter.Setter)
//					val retrieved = Time(s"executing select from $table by property values: $select")(table.joinSelect(Some(filter.whereClause), joins:_*)(filter.Setter)(filter.value).list)
//					composition(retrieved)
				case Single(Full(HasId(id))) =>
					val retrieved = Time(s"executing select one from $table by id: $select")(table.joinSelect(Some(table.wherePKClause), fetch:_*)(table.WherePKParameter)(Some(id)).list)
					composition(retrieved)
//				case Full(composition.items(instances)) => //todo: select where _ in _
				case composition.Full(instances) =>
					if (instances.exists(_.id.isEmpty))
						throw new IllegalArgumentException(s"No id associated with an instance - can't select $select from $table")
					val retrieved = Time(s"executing select by instances from $table :$select")(instances.map(i => apply(IdRef(i), fetch:_*)))
					composition(retrieved)
				//					get(SelectOne(IdRef(id)), fetch:_*)
				case All() =>
					val retrieved = Time(s"executing select all from $table: $select")(table.joinSelect(None, fetch:_*)(SetParameters.Unit)(()).list)
					composition(retrieved)
				case _ =>
					throw new IllegalArgumentException(s"don't know how to select $select from $table")
			}
		}



		override def delete[C](selection: Reference[C])(implicit composition :C ComposedOf E, s: Session): Unit = selection match {
			//			case ByProperty(table.PropertyComponent(comp), key) =>
			//				comp.withValue(key).delete.execute
			case Satisfying(PropertyIn(table.PropertyComponent(comp), keys)) =>
				val filter = comp.filter(keys.toSeq)
				table.deleteWhere(filter.whereClause)(filter.Setter)(keys.toSeq).execute
			case Satisfying(PropertyIn(property, keys)) =>
				throw new IllegalArgumentException(s"can't delete $table by unknown (or foreign) property $property in $keys")
			case composition.Full(instances :Iterable[E]) =>
//			case Decompose.Full(instances :Iterable[E]) =>
				if (instances.exists(_.id.isEmpty))
					throw new IllegalArgumentException(s"can't delete $table; some instances don't have ids: $selection")
			case _ =>
				throw new IllegalArgumentException(s"don't know how to delete $table by $selection")
		}





		def save(entity :E)(implicit s :Session) :E = entity.id match {
			case None =>
				val inserted = table.inserted(entity)
				(inserted /: cascades)((res, c) => c.copy(entity, res))
			case Some(_) =>
				table.update(entity)
				entity
		}

		override def save(entities :Seq[E])(implicit s :Session) :Seq[E] = entities.map(save)

		override def +=(entity: E)(implicit s :Session): Option[Id] =
			entity.id match {
				case None =>
					table.insert(entity).flatten
				case Some(_) =>
					table.update(entity)
					entity.id
			}


		override def ++=(entities: Seq[E])(implicit s :Session): Seq[Option[Id]] =
			entities.map { case entity =>
				entity.id match {
					case None =>
						table.insert(entity).flatten
					case Some(_) =>
						table.update(entity)
						entity.id
				}
			}


	}




	case class ForeignKey[E <:HasId, T <:HasId](get :E=>Reference[T], set :(E, Reference[T])=>E, blockedIds :Seq[Id]=Seq())(source : =>Store[E], target : =>Store[T]) {
		def resolve(entity :E)(implicit s :Session) :E = get(entity) match {
			case IdRef(id) if blockedIds.contains(id) =>
				throw new IllegalArgumentException(s"Cannot create a campaign for entity $target.$id")
			case IdRef(_) => entity
			case ref =>
				val referenced = target(ref)
				if (blockedIds.contains(referenced.id.get))
					throw new IllegalArgumentException(s"Cannot create a campaign for entity $target.$referenced")
				set(entity, IdRef(referenced))
		}
	}


	trait Cascade[O<:HasId, C] {
		def property :AnyProperty[O]

		def preSave(owner :O)(implicit s :Session) :O = owner
		def save(owner :O)(implicit s :Session) :O = owner
		def delete(id :Id)(implicit s :Session) :Unit = { delete(IdRef[O](id)) }
		def delete(ref :Reference[O])(implicit s :Session) :Reference[O] = ref
		def load(owners :Iterable[O])(implicit s :Session) :Iterable[O]
		def load(owner :O)(implicit s :Session) :O

		protected def idOf(owner :O) :Id = owner.id getOrElse (throw new IllegalArgumentException(s"cannot cascade into $property from $owner: no id on parent entity"))

		def copy(from :O, to :O) :O

		def isSingle :Boolean = false
	}

	protected class ForeignKeyCascade[O<:HasId :TypeTag, M<:HasId](val get :O=>Reference[M], val set :(O, Reference[M])=>O)(many: =>Store[O], one : =>Store[M], cascadeDelete :Boolean = false)
		extends Cascade[O, Reference[M]]
	{
		val property = PropertyChain[O](get)


		override def preSave(owner: O)(implicit s: Session): O = {
			get(owner) match {
				//				case Transient(parent) =>
				//					val saved = repository.save(parent)(one, s)
				//					set(owner, IdRef(saved))
				case Full(value) =>
					val saved = repository.save(value)(one, s)
					set(owner, IdRef(saved))
				case IdRef(id) =>
					owner
				case ref =>
					val referenced = one(ref)
					set(owner, IdRef(referenced))
				//					one(ref) match { //we could theoretically just pass a select into the values clause of the insert statement
				//						case Some(found) => set(owner, IdRef(found))
				//						case None => throw new IllegalArgumentException(s"Cannot save $owner: could not find $ref")
				//					}
			}

		}

		override def delete(ref :Reference[O])(implicit s: Session): Reference[O] = {
			if (cascadeDelete) { //todo this should be done by recursive delete statements.
				ref match {
					case Full(o) =>
						repository.delete[M, M](get(o))(one, ComposedOf.itself[M], s)
					//						one.delete(get(o))
					case _ =>
						val cmp = many.ComponentValue(ref)
						val onePKColumn = pkColumn(one.table)
						val fkColumn = many.table.ForeignKeyComponent(get).columns.head.sqlName.get
						val selectFK = cmp.property.select(many.table.ForeignKeyComponent(get)) //s"select $fkColumn from ${many.table.qname} where ${cmp.property.where}"
						val delete = s"${one.table.deleteClause} where $onePKColumn = ($selectFK)"
						cmp.update(delete).execute
				}
			}
			ref
		}

		override def load(owner: O)(implicit s: Session): O =
			set(owner, IdRef(one(get(owner)))) //.map(IdRef(_)) getOrElse (throw new IllegalArgumentException(s"unable to fetch ${get(owner)} for $owner")))


		override def load(owners: Iterable[O])(implicit s: Session): Iterable[O] = {
			val ids = owners.map(get).collect { case Empty() && IdRef(id) => Some(id) }.toSet[Option[Id]]
			if (ids.isEmpty)
				owners
			else {
				val referenced = one(Satisfying(PropertyIn(one.IdProperty)).in[Seq].empty(ids))
				val byId = referenced.map(o => o.id.get -> o).toMap
				owners.map(o => IdRef.unapply(get(o)).flatMap(id => byId.get(id).map(target => set(o, Reference(target)))) getOrElse o)
			}

		}


		override def copy(from: O, to: O): O = set(to, get(from))

		override def isSingle = true

		override def toString = s"ForeignKeyCascade(${many.table}->${one.table}})"
	}






	trait CascadeMany[O<:HasId, C<:Iterable[_]] extends Cascade[O, C]

	protected class ReverseForeignKeyCascade[O<:HasId, M<:HasId, C<:Iterable[M]](get :O=>Reference[C], set :(O, Reference[C])=>O, fk :M=>Reference[O], setfk :(M, Reference[O])=>M, deleteOrphan :Boolean=false)(
			owner : =>Store[O], many : =>Store[M])(implicit cbf :CanBuildFrom[_, M, C], oTag :TypeTag[O], mTag :TypeTag[M]
		) extends CascadeMany[O, C]
	{

		val foreignKeyProperty = PropertyChain(fk)
		val property = PropertyChain[O](get)
		val selector = Satisfying(PropertyEquals(foreignKeyProperty)).as[C]

		override def save(owner :O)(implicit s :Session): O = get(owner) match {
			case PersistedMany(id, values) if id!=idOf(owner) =>
				throw new IllegalArgumentException(s"cannot cascade save into $property from $owner: passed values are associated with another instance: $values. Create copies with reset foreign key")
			case Full(col) =>
				if (deleteOrphan) {
					val m = many
					val comp = m.table.ForeignKeyComponent(fk)
					m.table.deleteWhere[Reference[O]](m.table.where(comp))(m.table.parameter(comp))(IdRef(owner)).execute
				}
				val prepared = prepareForSaving(owner, col)
				val saved = repository.save(prepared)(many, s)
				val ref = new PersistedMany[M, C](idOf(owner), saved)
				set(owner, ref)
			case _ => owner

		}

		override def delete(ref :Reference[O])(implicit s :Session): Reference[O] = {
			many.delete(selector.empty(ref)) //todo: recursive?
			ref
		}

		private def prepareForSaving(owner :O, cascades :C) = {
			cascades.toSeq.map( m => fk(m) match {
				case Unknown() => setfk(m, IdRef(owner))
				case Full(value) if value==owner => setfk(m, IdRef(owner))
				case Single(PropertyEquals(prop, key)) if prop.isApplicableTo[O] && prop.fun.asInstanceOf[O=>Any](owner)==key =>
					setfk(m, IdRef(owner))
//				case UniquePropertyReference(prop, key) if prop(owner)==key => setfk(m, IdRef(owner))
				case ref => throw new IllegalArgumentException(s"attempted to cascade save from $owner to entity referencing another instance: $ref")
			})
		}

		override def load(owner: O)(implicit s: Session): O =
			set(owner, new PersistedMany[M, C](idOf(owner), many(selector.empty(IdRef(owner)))))


		override def load(owners: Iterable[O])(implicit s: Session): Iterable[O] = {
			val ids = owners.map(o=>IdRef[O](o.id.get)).toSet//[Option[Id]]
			if (ids.isEmpty)
				owners
			else {
//				val referenced = many(PropertyReference(foreignKeyProperty).in[Seq].apply(ids))
				val referenced = many(Satisfying(PropertyIn(foreignKeyProperty)).in[Seq].empty(ids))
				val byFK = referenced.groupBy(child => IdRef.unapply(fk(child)))
				owners.map(o => o.id.flatMap(id => byFK.get(Some(id)).map(children => set(o, new PersistedMany[M, C](id, children)))) getOrElse o)
			}

		}


		override def copy(from: O, to: O): O = set(to, get(from))

		override def toString = s"ReverseForeignKeyCascade(${owner.table}->${many.table}})"
	}


	protected abstract class AbstractJoinTableCascade[O<:HasId, M<:HasId, C<:Iterable[M], T<:Table[_, Option[Id]]](
        val get :O=>Reference[C], val set :(O, Reference[C])=>O, protected val table :T)(owner : =>Store[O], many : =>Store[M])(
		implicit cbf :CanBuildFrom[_, M, C], TypeTag :TypeTag[O]
	) extends CascadeMany[O, C]
	{
		def ownerFK :this.table.Component[Reference[O]]
		def manyFK :this.table.Component[Reference[M]]
		def insert(owner :Reference[O], many :Reference[M]) :StaticQueryInvoker[_, _]

		val property = PropertyChain[O](get)

		override def save(owner: O)(implicit s: Session): O = get(owner) match {
			case PersistedMany(id, _) if id==idOf(owner) => owner
			case Full(col) =>
				delete(idOf(owner))
				val saved = repository.save(col.toSeq)(many, s)
				saved.foreach { m => insert(IdRef(owner), IdRef(m)).execute }
				//				val joins = saved.map(m => new JoinRow(None, IdRef(owner), IdRef(m)))
				//				joins.foreach(table.insert(_))
				val ref = new PersistedMany[M, C](idOf(owner), saved)
				set(owner, ref)
			case _ => owner

		}

		override def delete(ref :Reference[O])(implicit s: Session): Reference[O] = ref match {
			case IdRef(id) =>
				table.deleteWhere[Reference[O]](table.where(ownerFK))(table.parameter(ownerFK))(IdRef[O](id)).execute
				ref
			case Full(o) if o.id.isDefined =>
				delete(IdRef[O](o.id.get))
			case _ =>
				val value = owner.ComponentValue(ref)
				value.update(s"${table.deleteClause} where ${ownerFK.sqlName.get} in ${value.property.selectPK}").execute
				ref
		}


		override def load(obj: O)(implicit s: Session): O = obj.id.map { id=>
			val where = s"${many.table.PK.columns(0).sqlName.get} in (select ${manyFK.sqlName.get} from ${table.qname} where ${ownerFK.sqlName.get} = ?)"
			val relations = many.table.selectWhere[Option[Id]](where)(owner.table.WherePKParameter)(Some(id)).list
			set(obj, new PersistedMany[M, C](id, relations))
		} getOrElse (throw new IllegalArgumentException(s"cannot load dependent entity $property: passed object doesn't have an id: $obj"))


		override def load(owners: Iterable[O])(implicit s: Session): Iterable[O] = {

			val ids = owners.toSeq.map(_.id)
			val ownerFKPlaceholders = (0 until ids.size).map(_=>"?").mkString(",")
			val columnNames = many.table.selectable.map(c => s"${many.table.qname}.${c.sqlName.get}")
			val targetPK = many.table.PK.columns.head.sqlName.get
			val select = columnNames.mkString(s"select ${table.qname}.${ownerFK.sqlName.get} as parent_id, ", ", ", " ")
			val from = s"from ${table.qname}, ${many.table.qname} "
			val where = s" where ${many.table.tableName}.$targetPK = ${table.tableName}.${manyFK.sqlName.get} and ${table.tableName}.${ownerFK.sqlName.get} in ($ownerFKPlaceholders)"
			implicit val manyGetter = many.table
//			implicit val idGetter = ownerFK.columnType.Getter
			implicit val idGetter = ownerFK :GetResult[Reference[O]]
			implicit val setter = SetParameters.repeat(owner.table.WherePKParameter)
			val query = Q.query[Seq[Option[Id]], (Reference[O], M)](select + from + where)
			val relations = query(ids).list
			val byId = relations.groupBy(fkToMany => IdRef.unapply(fkToMany._1)).map{ case (Some(id), elems) => id->elems.map(_._2) }
			owners.map(obj => obj.id.map(id => set(obj, new PersistedMany[M, C](id, byId.getOrElse(id, Seq())))) getOrElse obj)
		}

		override def copy(from: O, to: O): O = set(to, get(from))

		override def toString = s"JoinTableCascade(${owner.table}->${many.table}})"
	}






	protected case class PersistedMany[E<:HasId, C<:Iterable[E]](owner: Id, values :C) extends Reference[C] {

		def this(owner :Id, values :Seq[E])(implicit cbf :CanBuildFrom[_, E, C]) =
			this(owner, (cbf() ++= values).result())

		override def toOpt: Option[C] = Some(values)
	}


	protected def pkColumn[T](table :Table[_, T]) = table.PK.columns(0).sqlName.get



}

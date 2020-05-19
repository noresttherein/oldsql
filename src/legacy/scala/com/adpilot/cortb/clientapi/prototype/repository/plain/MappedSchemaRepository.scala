package com.adpilot.cortb.clientapi.prototype.repository.plain

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{IdRef, Id, HasId}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference._
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Selection.{SelectAll, ByInstances, SelectOne, ByPropertyValues}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.{Reference, Selection}
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.Mapping.{SetParameterSeq, SetParameters}
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.SchemaMapping
import com.adpilot.cortb.clientapi.util.ObjectProperty._
import com.adpilot.cortb.clientapi.util.ObjectProperty.apply
import com.adpilot.cortb.clientapi.util.{Time, ObjectProperty, MutableProperty}
import com.adpilot.cortb.clientapi.util.Matching.&&

import scala.collection.generic.CanBuildFrom
import scala.slick.jdbc.{SetParameter, StaticQueryInvoker, StaticQuery=>Q}


import scala.reflect.runtime.universe.TypeTag
import scala.util.Try

trait MappedSchemaRepository extends Repository { repository =>
	protected val schema :SchemaMapping
	import schema._
	
	type Store[E<:HasId] = TableStore[E]
	type Session = schema.Session

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

	protected def IdProperty[E <:HasId :TypeTag](set :(E, Option[Id])=>E) :MutableProperty[E, Option[Id]] = MutableProperty[E, Option[Id]](_.id, set)





	def apply[E <:HasId, T](selection :Selection[T, E], fetch :(E=>Reference[Any])*)(implicit store :Store[E], session :Session) :T = {
		val (ones, rest) = fetch.partition(store.isForeignKeyPath)
		val manys = rest.map(store.cascadeFor).collect {
			case Some(cascade) => cascade
			case None =>
				//				System.err.println(s"requested ${ones.map(ObjectProperty.name)} + ${rest.map(ObjectProperty.name)} -> ${rest.map(store.cascadeFor)}")
				throw new IllegalArgumentException(s"attempted to select $selection from ${store.table} with unknown references")
		}

		val found = Time(s"selecting $selection(${store.table}) with ${ones.map(_(store.table.joinRoot))}"){
			store(selection, ones.asInstanceOf[Seq[E=>One[Any]]]:_*)
		}
		val entities = selection.items(found)

		val res = (entities /: manys)((loaded, many) => Time(s"loading $many for $selection")(many.load(loaded)))
//		val res = Time(s"fetching relations for $selection: $manys"){
//			entities.map(entity => (entity /: manys)((res, many) => many.load(res)))
//		} //todo: n+1 selects! do a subselect query

		selection(res)
	}




	override def save[E <: HasId](entity: E)(implicit store: Store[E], session: Session): E = {
		System.err.println(s"prepare to save $entity")
		val resolved = (entity /: store.foreignKeys){ (e, fk) => fk.resolve(e) }
		val prepared = (resolved /: store.cascades){ (e, c) => System.err.println(s"saving ${store.table}, pre-save cascade $c"); c.preSave(e) }
		System.err.println(s"saving ${store.table}: $prepared")
		val saved = store.save(prepared)
		System.err.println(s"saved ${store.table}: $saved")
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


	override def delete[E <: HasId, T](selection: Selection[T, E])(implicit store: Store[E], session: Session): Unit = {
		System.err.println(s"deleting $selection")
		//todo :cascade delete!
		store.delete(selection)
	}




	class TableStore[E<:HasId :TypeTag](val table :schema.Table[E, Option[Id]],
	                                    val IdProperty :MutableProperty[E, Option[Id]],
	                                    private[MappedSchemaRepository] val cascades :Cascade[E, _]*)(
	                                    val foreignKeys :ForeignKey[E, _]*
		)
		extends EntityStore[E]
	{
		override def toString = table.toString

		private[MappedSchemaRepository] def isForeignKeyPath(relation :E=>Reference[Any]) =
			table.ToOne.is(ObjectProperty(relation)) || table.OptionalComponent.is(ObjectProperty(relation))


		private[MappedSchemaRepository] def cascadeFor(relation :E=>Reference[Any]) = {
			val property = ObjectProperty[E](relation)
			cascades.find(_.property == property)
		}

		@deprecated
		private[MappedSchemaRepository] def ComponentValue(ref :One[E]) = ref match {
			case PropertyReference(table.PropertyComponent(comp), key) => comp.withValue(key)
			case _ => throw new IllegalArgumentException(s"can't identify $table by $ref")
		}




		def apply[T](select :Selection[T, E], fetch : (E => One[Any])*)(implicit s :Session): T = {
			select match {
				//				case ByProperty(IdProperty, Some(key :Id)) =>
				//				case ByProperty(property, key) =>
				case ByPropertyValues(property, keys) =>
					val filter = table.joinFilter(property, keys)
					val joins = filter.join ++: fetch
					val query = table.joinSelect(Some(filter.whereClause), joins:_*)(filter.Setter)
					val retrieved = Time(s"executing select from $table by property values: $select")(table.joinSelect(Some(filter.whereClause), joins:_*)(filter.Setter)(filter.value).list)
					select(retrieved)
				case SelectOne(Full(HasId(id))) =>
					val retrieved = Time(s"executing select one from $table by id: $select")(table.joinSelect(Some(table.wherePKClause), fetch:_*)(table.WherePKParameter)(Some(id)).list)
					select(retrieved)
				case ByInstances(instances) => //todo: select where _ in _
					if (instances.exists(_.id.isEmpty))
						throw new IllegalArgumentException(s"No id associated with an instance - can't select $select from $table")
					val retrieved = Time(s"executing select by instances from $table :$select")(instances.map(i => apply(IdRef(i), fetch:_*)))
					select(retrieved)
				//					get(SelectOne(IdRef(id)), fetch:_*)
				case SelectAll() =>
					val retrieved = Time(s"executing select all from $table: $select")(table.joinSelect(None, fetch:_*)(SetParameters.Unit)(()).list)
					select(retrieved)
				case _ =>
					throw new IllegalArgumentException(s"don't know how to select $select from $table")
			}
		}



		override def delete(selection: Selection[_, E])(implicit s: Session): Unit = selection match {
			//			case ByProperty(last.PropertyComponent(comp), key) =>
			//				comp.withValue(key).delete.execute
			case ByPropertyValues(table.PropertyComponent(comp), keys) =>
				val filter = comp.filter(keys)
				table.deleteWhere(filter.whereClause)(filter.Setter)(keys).execute
			case ByPropertyValues(property, keys) =>
				throw new IllegalArgumentException(s"can't delete $table by unknown (or foreign) property $property in $keys")
			case ByInstances(instances) =>
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




	case class ForeignKey[E <:HasId, T <:HasId](get :E=>One[T], set :(E, One[T])=>E, blockedIds :Seq[Id]=Seq())(source : =>Store[E], target : =>Store[T]) {
		def resolve(entity :E)(implicit s :Session) :E = get(entity) match {
			case IdRef(id) if blockedIds.contains(id) =>
				throw new IllegalArgumentException(s"Cannot create a campaign for entity $target.$id")
			case IdRef(_) => entity
			case ref =>
				val referenced = target(SelectOne(ref))
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
		def delete(ref :One[O])(implicit s :Session) :One[O] = ref
		def load(owners :Iterable[O])(implicit s :Session) :Iterable[O]
		def load(owner :O)(implicit s :Session) :O

		protected def idOf(owner :O) :Id = owner.id getOrElse (throw new IllegalArgumentException(s"cannot cascade into $property from $owner: no id on parent entity"))

		def copy(from :O, to :O) :O

		def isSingle :Boolean = false
	}

	protected class ForeignKeyCascade[O<:HasId :TypeTag, M<:HasId](val get :O=>One[M], val set :(O, One[M])=>O)(many: =>Store[O], one : =>Store[M], cascadeDelete :Boolean = false)
		extends Cascade[O, One[M]]
	{
		val property = ObjectProperty[O](get)


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

		override def delete(ref :One[O])(implicit s: Session): One[O] = {
			if (cascadeDelete) { //todo this should be done by recursive delete statements.
				ref match {
					case Full(o) =>
						repository.delete(get(o))(one, s)
					//						one.delete(get(o))
					case _ =>
						val cmp = many.ComponentValue(ref)
						val onePKColumn = pkColumn(one.table)
						val fkColumn = many.table.ForeignKeyComponent(get).columns(0).name
						val selectFK = cmp.property.select(many.table.ForeignKeyComponent(get)) //s"select $fkColumn from ${many.last.qname} where ${cmp.property.where}"
						val delete = s"${one.table.deleteClause} where $onePKColumn = ($selectFK)"
						cmp.update(delete).execute
				}
			}
			ref
		}

		override def load(owner: O)(implicit s: Session): O =
			set(owner, IdRef(one(get(owner)))) //.map(IdRef(_)) getOrElse (throw new IllegalArgumentException(s"unable to fetch ${get(owner)} for $owner")))


		override def load(owners: Iterable[O])(implicit s: Session): Iterable[O] = {
			val ids = owners.map(get).collect { case Empty() && IdRef(id) => Some(id) }.toSeq
			if (ids.isEmpty)
				owners
			else {
				val referenced = one(PropertyReference(one.IdProperty).in[Seq].apply(ids))
				val byId = referenced.map(o => o.id.get -> o).toMap
				owners.map(o => IdRef.unapply(get(o)).flatMap(id => byId.get(id).map(target => set(o, One(target)))) getOrElse o)
			}

		}


		override def copy(from: O, to: O): O = set(to, get(from))

		override def isSingle = true

		override def toString = s"ForeignKeyCascade(${many.table}->${one.table}})"
	}






	trait CascadeMany[O<:HasId, C<:Iterable[_]] extends Cascade[O, C]

	protected class ReverseForeignKeyCascade[O<:HasId, M<:HasId, C<:Iterable[M]](get :O=>Many[C], set :(O, Many[C])=>O, fk :M=>One[O], setfk :(M, One[O])=>M, deleteOrphan :Boolean=false)(
		owner : =>Store[O], many : =>Store[M])(implicit cbf :CanBuildFrom[_, M, C], oTag :TypeTag[O], mTag :TypeTag[M]
	) extends CascadeMany[O, C]
	{
		val foreignKeyProperty = ObjectProperty(fk)
		val property = ObjectProperty[O](get)
		val selector = PropertyReference(fk).as[C]

		override def save(owner :O)(implicit s :Session): O = get(owner) match {
			case PersistedMany(id, values) if id!=idOf(owner) =>
				throw new IllegalArgumentException(s"cannot cascade save into $property from $owner: passed values are associated with another instance: $values. Create copies with reset foreign key")
			case Transient(col) =>
				if (deleteOrphan) {
					val m = many
					val comp = m.table.ForeignKeyComponent(fk)
					m.table.deleteWhere[One[O]](m.table.where(comp))(m.table.parameter(comp))(IdRef(owner)).execute
				}
				val prepared = prepareForSaving(owner, col)
				val saved = repository.save(prepared)(many, s)
				val ref = new PersistedMany[M, C](idOf(owner), saved)
				set(owner, ref)
			case _ => owner

		}

		override def delete(ref :One[O])(implicit s :Session): One[O] = {
			many.delete(selector(ref)) //todo: recursive?
			ref
		}

		private def prepareForSaving(owner :O, cascades :C) = {
			cascades.toSeq.map( m => fk(m) match {
				case Unknown() => setfk(m, IdRef(owner))
				case Transient(value) if value==owner => setfk(m, IdRef(owner))
				case UniquePropertyReference(prop, key) if prop(owner)==key => setfk(m, IdRef(owner))
				case ref => throw new IllegalArgumentException(s"attempted to cascade save from $owner to entity referencing another instance: $ref")
			})
		}

		override def load(owner: O)(implicit s: Session): O =
			set(owner, new PersistedMany[M, C](idOf(owner), many(selector(IdRef(owner)))))


		override def load(owners: Iterable[O])(implicit s: Session): Iterable[O] = {
			val ids = owners.map(o=>IdRef(o.id.get)).toSeq
			if (ids.isEmpty)
				owners
			else {
				val referenced = many(PropertyReference(foreignKeyProperty).in[Seq].apply(ids))
				val byFK = referenced.groupBy(child => IdRef.unapply(fk(child)))
				owners.map(o => o.id.flatMap(id => byFK.get(Some(id)).map(children => set(o, new PersistedMany[M, C](id, children)))) getOrElse o)
			}

		}


		override def copy(from: O, to: O): O = set(to, get(from))

		override def toString = s"ReverseForeignKeyCascade(${owner.table}->${many.table}})"
	}


	protected abstract class AbstractJoinTableCascade[O<:HasId, M<:HasId, C<:Iterable[M], T<:Table[_, Option[Id]]](
        val get :O=>Many[C], val set :(O, Many[C])=>O, protected val table :T)(owner : =>Store[O], many : =>Store[M])(
		implicit cbf :CanBuildFrom[_, M, C], TypeTag :TypeTag[O]
	) extends CascadeMany[O, C]
	{
		def ownerFK :this.table.Column[One[O]]
		def manyFK :this.table.Column[One[M]]
		def insert(owner :One[O], many :One[M]) :StaticQueryInvoker[_, _]

		val property = ObjectProperty[O](get)

		override def save(owner: O)(implicit s: Session): O = get(owner) match {
			case PersistedMany(id, _) if id==idOf(owner) => owner
			case Full(col) =>
				delete(idOf(owner))
				val saved = repository.save(col.toSeq)(many, s)
				saved.foreach { m => insert(IdRef(owner), IdRef(m)).execute }
				//				val joins = saved.map(m => new JoinRow(None, IdRef(owner), IdRef(m)))
				//				joins.foreach(last.insert(_))
				val ref = new PersistedMany[M, C](idOf(owner), saved)
				set(owner, ref)
			case _ => owner

		}

		override def delete(ref :One[O])(implicit s: Session): One[O] = ref match {
			case IdRef(id) =>
				table.deleteWhere[One[O]](table.where(ownerFK))(table.parameter(ownerFK))(IdRef[O](id)).execute
				ref
			case Full(o) if o.id.isDefined =>
				delete(IdRef[O](o.id.get))
			case _ =>
				val value = owner.ComponentValue(ref)
				value.update(s"${table.deleteClause} where ${ownerFK.name} in ${value.property.selectPK}").execute
				ref
		}


		override def load(obj: O)(implicit s: Session): O = obj.id.map { id=>
			val where = s"${many.table.PK.columns(0).name} in (select ${manyFK.name} from ${table.qname} where ${ownerFK.name} = ?)"
			val relations = many.table.selectWhere[Option[Id]](where)(owner.table.WherePKParameter)(Some(id)).list
			set(obj, new PersistedMany[M, C](id, relations))
		} getOrElse (throw new IllegalArgumentException(s"cannot load dependent entity $property: passed object doesn't have an id: $obj"))


		override def load(owners: Iterable[O])(implicit s: Session): Iterable[O] =
			if (owners.isEmpty) owners
			else {

				val ids = owners.toSeq.map(_.id)
				val ownerFKPlaceholders = (0 until ids.size).map(_=>"?").mkString(",")
				val columnNames = many.table.selectable.map(_.selectHeader(many.table.qname))
				val targetPK = many.table.PK.columns(0).name
				val select = columnNames.mkString(s"select ${table.qname}.${ownerFK.name} as parent_id, ", ", ", " ")
				val from = s"from ${table.qname}, ${many.table.qname} "
				val where = s" where ${many.table.tableName}.$targetPK = ${table.tableName}.${manyFK.name} and ${table.tableName}.${ownerFK.name} in ($ownerFKPlaceholders)"
				implicit val manyGetter = many.table
				implicit val idGetter = ownerFK.columnType.Getter
				implicit val setter = SetParameters.repeat(owner.table.WherePKParameter)
				val query = Q.query[Seq[Option[Id]], (One[O], M)](select + from + where)
				val relations = query(ids).list
				val byId = relations.groupBy(fkToMany => IdRef.unapply(fkToMany._1)).map{ case (Some(id), elems) => id->elems.map(_._2) }
				owners.map(obj => obj.id.map(id => set(obj, new PersistedMany[M, C](id, byId.getOrElse(id, Seq())))) getOrElse obj)
			}

		override def copy(from: O, to: O): O = set(to, get(from))

		override def toString = s"JoinTableCascade(${owner.table}->${many.table}})"
	}






	protected case class PersistedMany[E<:HasId, C<:Iterable[E]](owner: Id, values :C) extends Many[C] {

		def this(owner :Id, values :Seq[E])(implicit cbf :CanBuildFrom[_, E, C]) =
			this(owner, (cbf() ++= values).result())

		override def toOpt: Option[C] = Some(values)
	}


	protected def pkColumn[T](table :Table[_, T]) = table.PK.columns(0).name



}

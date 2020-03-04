package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{UniquePropertyReferenceFactory, One, PropertyReference, Transient}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta._
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnMapping.ColumnOption.OptionalSelect
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnType.{NullValue, MappedType}
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnValues.PositionedResultView
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.EntityMapping.{ColumnKey, JoinResult, JoinKey, JoinByFK}
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.Mapping.{AbstractMappedMapping, MappedMapping}
import com.adpilot.cortb.clientapi.util.Generic.GenericFunction
import com.adpilot.cortb.clientapi.util.Matching.Unapply
import com.adpilot.cortb.clientapi.util.ObjectProperty.AnyProperty
import com.adpilot.cortb.clientapi.util.{Time, OptionOps}

import scala.collection.breakOut
import scala.reflect.runtime.universe.TypeTag
import scala.slick.jdbc.{StaticQuery => Q, PositionedResult, GetResult, SetParameter}

import OptionOps._

import scala.util.Try


trait EntityMapping[E, PK] extends TableMapping[E] with MappingWithPK[E, PK] { mapping =>

	type ForeignKeyComponent[T] = Component[One[T]]

	object ForeignKeyComponent {

		def apply[T](property :E=>One[T]) :mapping.Component[One[T]] = {
			val key = property(joinRoot)
			get(key) getOrElse {
				throw new IllegalArgumentException(s"passed function does not return a foreign key property of $mapping: $key")
			}
		}
		

		def unapply[T](property :E=>T) :Option[mapping.Component[One[_]]] = property(joinRoot) match {
			case o:One[_] => get(o).asInstanceOf[Option[mapping.Component[One[_]]]]
			case _ => None
		}

		def is[T](property :E=>T) = unapply(property).isDefined

		private def get[T](ref :One[T]) :Option[mapping.Component[One[T]]] = ref match {
			case JoinKey(JoinByFK(_, Some((fk :ComponentMapping[_, _], prev)), _)) if prev.table == mapping =>
				Some(fk.asInstanceOf[mapping.Component[One[T]]])
			case _ => None
		}

		private def map[T, X[Q]](ref :One[T], fun :GenericFunction[ForeignKeyComponent, X]) :Option[X[T]] = ref match {
			case JoinKey(JoinByFK(_, Some((fk :ComponentMapping[_, _], prev)), _)) if prev.table == mapping =>
				Some(fun(fk.asInstanceOf[mapping.Component[One[T]]]))
			case _ => None
		}


		def map[T, Y[X]](property :E=>T)(fun :GenericFunction[ForeignKeyComponent, Y]) :Option[Y[_]] =
			property(joinRoot) match {
				case o:One[_] => map(o, fun)
				case _ => None
			}


	}
	
	

	object ReferencedTable {
		def apply[T](property :E=>One[T]) :EntityMapping[T, _] = {
			val key = property(joinRoot)
			get(key) getOrElse {
				throw new IllegalArgumentException(s"passed function does not return a key to a table joinable with $mapping: $key")
			}
		}

		def unapply[T](property :E=>T) :Option[EntityMapping[_, _]] = property(joinRoot) match {
			case o:One[_] => get(o)
			case _ => None
		}

		private def get[T](ref :One[T]) :Option[EntityMapping[T, _]] = ref match {
			case JoinKey(joins) if joins.joinedWith(mapping) => Some(joins.table)
			case _ => None
		}
	}



	def joinRoot = joinMock(JoinByFK(this))

	private def joinMock(implicit path :JoinByFK[E, PK]) :E =
		joinMock() getOrElse apply(ColumnValues(selectable.map(mockValue(_))))

	protected def joinMock() :Option[E] = None


	private def mockValue[T](col :Column[T])(implicit path :JoinByFK[E, PK]) = ColumnValue(col, col.nullValue match {
		case fk:JoinKey[_, _] =>
			val keyComp = col.asInstanceOf[Component[One[fk.ReferencedType]]]
			val table = fk.table.asInstanceOf[EntityMapping[fk.ReferencedType, fk.ReferencedKeyType]]
			val join = JoinByFK(path, keyComp, table)
			JoinKey(join).asInstanceOf[T]
		case ColumnKey(_) =>
			ColumnKey(Some(path.include(col))).asInstanceOf[T]
		case value => value
	})


	final def referenceForeignKey[P, K](component :Component[One[P]])(implicit path :JoinByFK[E, PK], next :EntityMapping[P, K]) :One[P] =
		referenceForeignKey[P, K](path, component, next)

	def referenceForeignKey[P, K](path :JoinByFK[E, PK], component :Component[One[P]], next :EntityMapping[P, K]) :One[P] =
		JoinKey(JoinByFK(path, component, next))



	def joinSelect[P :SetParameter](whereCondition :Option[String], refs :(E=>One[Any])*) :Q[P, E] = {
		val (select, getter) = Time(s"creating select from $this where $whereCondition")(joinSelectStatement(whereCondition, refs:_*))
		Q.query[P, E](select)(implicitly[SetParameter[P]], getter)
	}



	private[schema] def joinSelectStatement(whereCondition :Option[String], refs :(E=>One[Any])*) :(String, GetResult[E]) = {
		val root = JoinByFK(this)
		val mock = joinMock(root)

		type FromItem = (JoinByFK[_, _], Int, Boolean) //(table, unique index for multiple queries against same table, duplicate?)

		//list of joined tables, expanded to include intermediate joins in case of crossing multiple relationships
		val joins :Seq[FromItem] = { //Seq[(join, table occurence, row already joined earlier in the list)]
			val expanded =
				root +: refs.map(_(mock)).flatMap{
					case JoinKey(join) if join.joinedWith(this) => join.expand()
					case ColumnKey(Some(join)) if join.joinedWith(this) => join.expand()
				}

			def indexed(tables :Seq[FromItem] = expanded.map((_, 0, false))) :Seq[FromItem] = tables match {
				case Seq() => Seq()
				case Seq((join, count, false), tail @ _*) =>
					val includes = (join /: tail)(_ combine _._1)
					(includes, count, false) +: indexed(tail.map {
						case (t, i, false) if t==join => (t, i, true)
						case (t, i, false) if t.table==join.table => (t, i+1, false)
						case x => x
					})
				case (Seq(head, tail @_*)) => head +: indexed(tail)
			}

			indexed()
		}

		def alias(table :EntityMapping[_, _], idx :Int) = 
			if (idx==0) table.tableName
			else s"${table.tableName}_$idx"

		val tableAliases = joins.collect { case (j, idx, false) => (j, alias(j.table, idx)) }

		//read & assembly order - referenced tables before referencing
		val ordered = tableAliases.reverse

		val select = {
			val columns =
				for ((join, alias) <- ordered; column <-join.table.selectable) yield
					if (OptionalSelect.enabled(column) && join.columns.contains(column))
						s"$alias.${column.name}"
					else column.selectHeader(alias)

			columns.mkString("select ", ", ", " from ")
		}

		val from = {
			val aliases = tableAliases.map { case (join, alias) => s"${join.table.qname} as $alias" }
			aliases.mkString(select, ", ", "")
		}
		

		val where = {
			def conditions(tables :Seq[FromItem]=joins) :Seq[String] = tables match {
				case Seq(_, (_, _, true), t @_*) => //skip duplicate joins
					conditions(tables.tail)
				case Seq(_, (JoinByFK(_, None, _), _, false), t @_*) => //second table is the root and starts a new join chain
					conditions(tables.tail)
				case Seq((prev, idx1, _), (next, idx2, false), t @_*) if next.prev.isDefined =>
					//let's add condition prev.<fk>=next.<pk>
					val ltable = alias(prev.table, idx1)
					val rtable = alias(next.table, idx2)
					val keysEqual = (next.prev.get._1.querable zip next.table.PK.querable).map {
						case (lcol, rcol) => s"$ltable.${lcol.name} = $rtable.${rcol.name}"
					}.mkString(" and ")
					keysEqual +: conditions(tables.tail)
				case _ => Seq() //one or fewer(?!) tables to join, nothing to do
			}

			val conds = whereCondition ++: conditions()

			if (conds.isEmpty)
				from
			else conds.mkString(from+" where ", " and ", "")
		}

		val getter = GetResult[E](res => apply(JoinResult(res, ordered.dropRight(1).map(_._1.table))))
		System.err.println(s"$where")
		(where, getter)
	}




	def getWithPK(res :PositionedResult) = {
		val entity = apply(res)
		(entity, pk(entity))
	}

	lazy val wherePKClause = where[PK](PK)
}




object EntityMapping {

	class ForeignKeyType[FK :ColumnType, E :TypeTag, PK] private (lazytable: () => EntityMapping[E, PK], toPK :FK=>PK, toFK :PK=>FK)
//		extends MappedType[One[E], FK](
//			fk => One(lazytable().pk _, toPK(fk)),
//			(One(lazytable().pk _).forceKeyOutOf _).andThen(toFK)
//		)
		extends MappedType[One[E], FK](_ => ???, _ => ???)
	{
		private lazy val table = lazytable()
		private val referencing = One[E, PK](table.pk _)

		private val keyType = implicitly[ColumnType[FK]]

		private def emptyValue(key :PK) = referencing.Empty(key)

		//todo: these are required for creating Option[One[E]] out of Option[FK], but will create always only empty references!
		override val map = (fk :FK) => referencing.Empty(toPK(fk)) :One[E]
		override val unmap = (ref :One[E]) => toFK(referencing.forceKeyOutOf(ref))
		
		override lazy val Getter = GetResult[One[E]]{
			case join :JoinResult =>
				val pk = toPK(keyType.Getter(join))
				join.get(table, pk).map(referencing.Full) getOrElse emptyValue(pk)
			case res =>
				emptyValue(toPK(keyType.Getter(res)))
		}


		override lazy val Setter = SetParameter[One[E]]{
			case (key, params) => keyType.Setter(unmap(key), params)
		}

		override def nullValue :One[E] =
			new JoinKey[E, PK](table.joinMock(JoinByFK(table)), JoinByFK(table))
		
	}


	object ForeignKeyType {

		def emptyKey[PK :ColumnType, E :TypeTag](pk :E=>PK) = {
			val factory = One[E, PK](pk)
			ColumnType.mapped[One[E], PK](factory.Empty)(factory.forceKeyOutOf)
		}

//		private def apply[FK, E :TypeTag, PK :ColumnType](mapping : =>EntityMapping[E, PK], toPK :FK=>PK, toFK :PK=>FK) :ForeignKeyType[FK, E, PK] =
//			new ForeignKeyType[FK, E, PK](() => mapping, { lazy val table = mapping; One[E, PK](table.pk _)}, toPK, toFK)


		def apply[PK :ColumnType, E :TypeTag](mapping : =>EntityMapping[E, PK]) :ForeignKeyType[PK, E, PK] =
			new ForeignKeyType[PK, E, PK](()=>mapping, identity, identity)

		def apply[FK :ColumnType, E :TypeTag, PK](mapping : =>EntityMapping[E, PK], pk :FK=>PK, fk :PK=>FK) :ForeignKeyType[FK, E, PK]=
			new ForeignKeyType[FK, E, PK](()=>mapping, pk, fk)

		implicit def implicitForeignKeyType[PK, E](implicit mapping :EntityMapping[E, PK], tpe :ColumnType[PK], tag :TypeTag[E]) :ForeignKeyType[PK, E, PK] =
			new ForeignKeyType[PK, E, PK](()=>mapping, identity, identity)

	}


//	class OptionalColumnType[S :ColumnType.NullValue, T :ColumnType](map :T=>S, unmap :S=>T)
//		extends MappedType[S, T](map, unmap)
//	{
//		override def nullValue
//	}



/*
	trait AbstractForeignKeyMapping[E, PK, FK] extends AbstractMappedMapping[One[E], FK, Mapping[FK]] {
		def parentMapping :EntityMapping[E, PK]
		def keyMapping = adaptedMapping
//		val keyMapping :Mapping[FK]
//		lazy val adaptedMapping = keyMapping
		def foreignKeyType :ColumnType[FK]
		lazy val columnType = ForeignKeyType(parentMapping, toPK, toFK)()

		def toPK(fk :FK) :PK
		def toFK(pk :PK) :FK
		
		
		override def apply(res: PositionedResult): ForeignKey[E, PK] = columnType.Getter(res)

		override def toResultType(value: FK): ForeignKey[E, PK] = columnType.emptyValue(toPK(value))

		override def fromResultType(value: One[E]): FK = columnType.unmap(value) //toFK(value.key)
	}


	class ForeignKeyMapping[E, PK, FK](val adaptedMapping :Mapping[FK], val parentMapping :EntityMapping[E, PK], pk :FK=>PK, fk :PK=>FK)
		extends AbstractForeignKeyMapping[E, PK, FK]
	{
		def this(keyMapping :Mapping[FK], parentMapping :EntityMapping[E, PK])(implicit pk :FK=>PK, fk :PK=>FK) =
			this(keyMapping, parentMapping, pk, fk)

		implicit def foreignKeyType

		def toPK(fk: FK): PK = pk(fk)
		
		def toFK(pk :PK) :FK = fk(pk)
	}

	def ForeignKeyMapping[E, PK, FK](key :Mapping[FK], parent :EntityMapping[E, PK], pk :FK=>PK, fk :PK=>FK) :ForeignKeyMapping[E, PK, FK] =
		new ForeignKeyMapping[E, PK, FK](key, parent, pk, fk)

	def ForeignKeyMapping[E, K](key :Mapping[K], parent :EntityMapping[E, K]) :ForeignKeyMapping[E, K, K] =
		new ForeignKeyMapping[E, K, K](key, parent, identity, identity)
	
*/






	private[EntityMapping] case class JoinByFK[E, PK] private (table :EntityMapping[E, PK], prev :Option[(Mapping[One[E]], JoinByFK[_, _])], columns :Seq[ColumnMapping[E, _]]=Seq())
	{
		def joinMock = table.joinMock(this)
		
		protected[EntityMapping] def expand(acc :Seq[JoinByFK[_,_]]=Seq()) :Seq[JoinByFK[_,_]] = prev match {
			case None => this +: acc
			case Some((_, child)) => child.expand(this +: acc)
		}

		def extend[X, P](key :ComponentMapping[E, One[X]], parent :EntityMapping[X, P]) :JoinByFK[X, P] =
			new JoinByFK(parent, Some(key, this))

		def include(column :EntityMapping[E, PK]#Column[_]) = new JoinByFK(table, prev, column+:columns)

		def combine(join :JoinByFK[_, _]) =
			if (join!=this)
				this
			else
				JoinByFK(table, prev, join.columns.filter(c => columns.forall(_.name != c.name)).asInstanceOf[Seq[ColumnMapping[E, _]]]++:columns)

		def joinedWith(table :EntityMapping[_, _]) :Boolean =
			table == this.table || prev.exists(_._2.joinedWith(table))

		override def equals(that :Any) = that match {
			case JoinByFK(t, p, _) if t==table && p==prev => true
			case _ => false
		}

		override def hashCode = (table, prev).hashCode
	}

	private object JoinByFK {
		def apply[C, P, PK](prev :JoinByFK[C, _], key :ComponentMapping[C, One[P]], table :EntityMapping[P, PK]) :JoinByFK[P, PK] =
			new JoinByFK[P, PK](table, Some(key, prev))

		def apply[E, PK](table :EntityMapping[E, PK]) :JoinByFK[E, PK] = new JoinByFK(table, None)
	}




	private[EntityMapping] class JoinKey[E, PK](override val join :E, val path :JoinByFK[E, PK]) extends One[E] {
//		private def this(value :(FK, E), join :JoinByFK[FK, E, PK]) =
//			this(value._1, value._2, join)
//		override def join = value
		
		def this(join :JoinByFK[E, PK]) =
			this(join.joinMock, join)
		
//		def this(join :JoinByFK[FK, E, PK])(implicit fk :PK=>FK) =
//			this({ val dummy = join.referenceDummy; (fk(join.table.pk(dummy)), dummy)}, join)

		//		type KeyType = FK
		type ReferencedType = E
		type ReferencedKeyType = PK

		def withPath(joins :JoinByFK[E, PK]) = new JoinKey[E, PK](join, joins)

		def table :EntityMapping[ReferencedType, ReferencedKeyType] = path.table


//		override def toOpt: Option[E] = Some(join)
		def toOpt = None

		override def toString = s"JoinKey($path)"

		override def canEqual(that :Any) = that.isInstanceOf[JoinKey[_, _]]

		override def equals(that: Any): Boolean = that match {
			case join :JoinKey[_, _] if join.canEqual(this) => (join, path) == ((join.join, join.path))
			case _ => false
		}

		override def hashCode = (join, path).hashCode


	}
	
	private object JoinKey {
		def apply[E, PK](joins :JoinByFK[E, PK]) :JoinKey[E, PK] =
//			apply(joins, implicitly[NullValue[FK]].value)
			new JoinKey[E, PK](joins)
		
//		def apply[FK, E, PK](joins :JoinByFK[FK, E, PK], fk :FK) :JoinKey[FK, E, PK] = 
//			new JoinKey(fk, joins.table.referenceDummy(joins), joins)

		def apply[K, E](mapping :EntityMapping[E, K]) :JoinKey[E, K] =
			new JoinKey[E, K](JoinByFK[E, K](mapping))//(identity[K])

		def unapply[K, E](key :One[E]) :Option[JoinByFK[E, _]]= key match {
			case k: JoinKey[_, _] => Some(k.path.asInstanceOf[JoinByFK[E, _]])
			case _ => None
		}
	}


	case class ColumnKey[T, E, PK](private[EntityMapping] val owner: Option[JoinByFK[E, PK]]) extends One[T] {
		override def toOpt: Option[T] = None
	}

	object ColumnKey {
		def apply[T]() = ColumnKey[T, Nothing, Nothing](None)
	}



	protected[EntityMapping] class JoinResult private (res :PositionedResult, position :Int, values :Map[(Any, Any), Any])
		extends PositionedResultView(res, position)
	{
		def this(res :PositionedResult) = this(res, res.currentPos, Map())

		def load(mapping :EntityMapping[_, _]) :JoinResult = {
			val (entity, pk) = mapping.getWithPK(this)
			new JoinResult(res, currentPos, values + ((mapping, pk) -> entity))
		}

		def get[E, PK](mapping :EntityMapping[E, PK], pk :PK) = values.get((mapping, pk)).asInstanceOf[Option[E]]

		override def offset(idx: Int): PositionedResultView = new JoinResult(res, pos + idx, values)
	}

	protected[EntityMapping] object JoinResult {
		def apply(res :PositionedResult, mappings :Seq[EntityMapping[_, _]]) =
			(new JoinResult(res) /: mappings)(_.load(_))
	}

}
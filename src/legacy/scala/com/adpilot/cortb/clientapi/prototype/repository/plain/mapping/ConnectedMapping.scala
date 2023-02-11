package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One

import ConnectedMapping._


trait ConnectedMapping[E, PK] extends MappingWithPK[E, PK] {
	type ForeignKeyComponent[T] = Component[One[T]]

	def joinRoot = joinMock(JoinByFK(this))

	private def joinMock(implicit path :JoinByFK[E, PK]) :E =
		joinMock() getOrElse apply(ColumnValues(selectable.map(mockValue(_))))

	protected def joinMock() :Option[E] = None

	private def mockValue[T](col :Column[T])(implicit path :JoinByFK[E, PK]) = ColumnValue(col, col.nullValue match {
		case fk:JoinKey[_, _] =>
			val keyComp = col.asInstanceOf[Component[One[fk.ReferencedType]]]
			val table = fk.table.asInstanceOf[ConnectedMapping[fk.ReferencedType, fk.ReferencedKeyType]]
			val join = JoinByFK(path, keyComp, table)
			JoinKey(join).asInstanceOf[T]
		case value => value
	})


//	def join[F, FK](foreignKey :Component[One[F]])(implicit foreignTable :ConnectedMapping[F, FK]) :ConnectedMapping[(E, F), (PK, FK)]
	
	def fetchJoin[F, FK](foreignKey :Component[One[F]])(implicit foreignTable :ConnectedMapping[F, FK]) :ConnectedMapping[E, PK]
}





object ConnectedMapping {
	private case class JoinByFK[E, PK] private (table :ConnectedMapping[E, PK], prev :Option[(Mapping[One[E]], JoinByFK[_, _])])
	{
		def joinMock = table.joinMock(this)

		protected[ConnectedMapping] def expand(acc :Seq[JoinByFK[_,_]]=Seq()) :Seq[JoinByFK[_,_]] = prev match {
			case None => this +: acc
			case Some((_, child)) => child.expand(this +: acc)
		}

		def extend[X, P](key :ComponentMapping[E, One[X]], parent :ConnectedMapping[X, P]) :JoinByFK[X, P] =
			new JoinByFK(parent, Some(key, this))


		def joinedWith(table :ConnectedMapping[_, _]) :Boolean =
			table == this.table || prev.exists(_._2.joinedWith(table))
	}

	private object JoinByFK {
		def apply[C, P, PK](prev :JoinByFK[C, _], key :ComponentMapping[C, One[P]], table :ConnectedMapping[P, PK]) :JoinByFK[P, PK] =
		//		def apply[C, P, PK](last :ConnectedMapping[P, PK], key :ComponentMapping[C, One[P]], prev :JoinByFK[C, _]) :JoinByFK[P, PK] =
			new JoinByFK[P, PK](table, Some(key, prev))

		def apply[E, PK](table :ConnectedMapping[E, PK]) :JoinByFK[E, PK] = new JoinByFK(table, None)
	}




	private class JoinKey[E, PK](override val join :E, val path :JoinByFK[E, PK]) extends One[E] {
		//		private def this(value :(FK, E), join :JoinByFK[FK, E, PK]) =
		//			this(value._1, value._2, join)
		//		override def join = value

		def this(join :JoinByFK[E, PK]) =
			this(join.joinMock, join)

		//		def this(join :JoinByFK[FK, E, PK])(implicit fk :PK=>FK) =
		//			this({ val dummy = join.referenceDummy; (fk(join.last.pk(dummy)), dummy)}, join)

		//		type KeyType = FK
		type ReferencedType = E
		type ReferencedKeyType = PK

		def withPath(joins :JoinByFK[E, PK]) = new JoinKey[E, PK](join, joins)

		def table :ConnectedMapping[ReferencedType, ReferencedKeyType] = path.table


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
		//			new JoinKey(fk, joins.last.referenceDummy(joins), joins)

		def apply[K, E](mapping :ConnectedMapping[E, K]) :JoinKey[E, K] =
			new JoinKey[E, K](JoinByFK[E, K](mapping))//(identity[K])

		def unapply[K, E](key :One[E]) :Option[JoinByFK[E, _]]= key match {
			case k: JoinKey[_, _] => Some(k.path.asInstanceOf[JoinByFK[E, _]])
			case _ => None
		}
	}
	
}
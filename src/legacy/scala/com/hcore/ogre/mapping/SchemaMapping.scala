package com.hcore.ogre.mapping

import com.hcore.clientapi.repository.DBConfig

import scala.slick.jdbc.{GetResult, StaticQuery => Q}



class SchemaMapping(config :DBConfig, schemaName :Option[String]=None) {
	val driver = config.driver
	import driver.simple._
//	import config.driver.simple._
	private val db = Database.forURL(config.url, config.user.orNull, config.password.orNull, driver = config.driverClass)

//	type Session = db.Session
	type Session = driver.simple.Session

	def newSession :Session = db.createSession()
	def withSession[T](block :Session => T) = db.withSession(block)




	trait Table[E, PK] extends TableMapping[E] with EntityMapping[E, PK] with PropertiesMapping[E, PK] {


		lazy val insertStmt = insertReturning(PK)
		
		lazy val updateByPKStmt = updateWhere[PK](wherePKClause)(WherePKParameter)

		lazy val selectByPKStmt = selectWhere[PK](wherePKClause)(WherePKParameter)

		lazy val deleteByPKStmt = deleteWhere[PK](wherePKClause)(WherePKParameter)

		def inserted(entity :E)(implicit s:Session) :E =
			insertReturning[E](GetResult[E] { res =>
//				val returns = generated.map(col => (col, ColumnValue(col, res))).toMap[Column[_], ColumnValue[_]]
//				val values = ColumnValues(selectable.map(col => returns.getOrElse(col, ColumnValue(this)(entity, col))))
//				this(values)
				val input = ComponentValues(this :this.type)(entity, columns :Seq[Component[_]])
				val returns = ColumnValues(this :this.type)(res, generated)
				apply(returns orElse input)
			})(entity).first
		
		def insert(value :E)(implicit s :Session) :Option[PK] =
			Some(insertStmt(value).first)

		def update(value :E)(implicit s :Session) :Boolean =
			updateByPKStmt((value, pk(value))).first > 0


		def select(id :PK)(implicit s :Session) :Option[E] =
			selectByPKStmt(id).firstOption

		def delete(id :PK)(implicit s :Session) :Boolean =
			deleteByPKStmt(id).first > 0
		
		
	}

//	case class JoinRow[L<:HasId, R<:HasId](id :Option[Id], left :Reference[L], right :Reference[R]) extends HasId
//	trait JoinRow[L, R] {
//		val left :Reference[L]
//		val right :Reference[R]
//	}
//
//	trait JoinTable[L, R](val tableName :String, leftKey :String, rightKey :String)(
//		implicit leftTable :Table[L, Option[Id]], rightTable :Table[R, Option[Id]], leftType :TypeTag[L], rightClass :TypeTag[R]
//		) extends IdTable[JoinRow[L, R]] with MappingSupport[JoinRow[L, R]]
//	{
//		implicit val left :Column[Reference[L]]
//		implicit val right :Column[Reference[R]]
//		implicit val left = column(leftKey, _.left, NoUpdate)
//		implicit val right = column(rightKey, _.right, NoUpdate)
//
//		def side[T](implicit col :Column[Reference[T]]) = col
//
//		override protected def assemble(implicit res: ResultRow): JoinRow[L, R] = JoinRow(id, left, right)
//	}


	
}



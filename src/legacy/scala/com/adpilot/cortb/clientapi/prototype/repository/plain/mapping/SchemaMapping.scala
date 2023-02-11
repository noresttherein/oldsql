package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.DBConfig
import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{Id, HasId}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.TypedColumn.ColumnOption.NoUpdate

import scala.slick.jdbc.{StaticQuery => Q, GetResult, SetParameter}


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
				val returns = generated.map(col => (col, ColumnValue(col, res))).toMap[Column[_], ColumnValue[_]]
				val values = ColumnValues(selectable.map(col => returns.getOrElse(col, ColumnValue(entity, col))))
				this(values)
			})(entity).firstOption.filterNot(_ => generated.isEmpty) getOrElse entity

		def insert(value :E)(implicit s :Session) :Option[PK] =
			Some(insertStmt(value).first)

		def update(value :E)(implicit s :Session) :Boolean =
			updateByPKStmt((value, pk(value))).first > 0


		def select(id :PK)(implicit s :Session) :Option[E] =
			selectByPKStmt(id).firstOption

		def delete(id :PK)(implicit s :Session) :Boolean =
			deleteByPKStmt(id).first > 0
		
		
	}

//	case class JoinRow[L<:HasId, R<:HasId](id :Option[Id], left :One[L], right :One[R]) extends HasId
//	trait JoinRow[L, R] {
//		val left :One[L]
//		val right :One[R]
//	}
//
//	trait JoinTable[L, R](val tableName :String, leftKey :String, rightKey :String)(
//		implicit leftTable :Table[L, Option[Id]], rightTable :Table[R, Option[Id]], leftType :TypeTag[L], rightClass :TypeTag[R]
//		) extends IdTable[JoinRow[L, R]] with MappingSupport[JoinRow[L, R]]
//	{
//		implicit val left :Column[One[L]]
//		implicit val right :Column[One[R]]
//		implicit val left = column(leftKey, _.left, NoUpdate)
//		implicit val right = column(rightKey, _.right, NoUpdate)
//
//		def side[T](implicit col :Column[One[T]]) = col
//
//		override protected def assemble(implicit res: ResultRow): JoinRow[L, R] = JoinRow(id, left, right)
//	}


	
}



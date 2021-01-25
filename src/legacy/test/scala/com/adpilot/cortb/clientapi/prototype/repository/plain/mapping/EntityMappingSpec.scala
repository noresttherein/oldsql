package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.Mapping.SetParameters
import org.scalatest.{Ignore, Matchers, FlatSpec}


@Ignore
class EntityMappingSpec extends FlatSpec with Matchers {
	trait Entity { val id :Long; val name :String }
	case class Religion(id :Long, name :String) extends Entity
	case class God(id :Long, name :String, just :Boolean, religion :One[Religion]) extends Entity
	case class Priest(id :Long, name :String, jolly :Boolean) extends Entity
	case class Church(id :Long, name :String, religion :One[Religion], priest :One[Priest]) extends Entity
	case class Believer(id :Long, name :String, church :One[Church]) extends Entity
	case class Sin(id :Long, name :String, commitedBy :One[Believer]) extends Entity
	
	abstract class T[E<:Entity](val tableName :String) extends EntityMapping[E, Long] with MappingSupport[E] {
		override def schemaName: Option[String] = Some("beliefs")

		val id = column("id", _.id)
		val name = column("name", _.name)

		val PK = id

	}

	import EntityMapping.ForeignKeyType._

	
	implicit object Religions extends T[Religion]("religions") {
		def assemble(implicit res: ResultRow) :Religion = Religion(id, name)
	}

	implicit object Gods extends T[God]("gods") {
		val just = column("just", _.just)
		val religion = column("religion", _.religion)

		def assemble(implicit res: ResultRow): God = God(id, name, just, religion)
	}

	implicit object Priests extends T[Priest]("priests") {
		val jolly = column("jolly", _.jolly)

		def assemble(implicit res: ResultRow): Priest = Priest(id, name, jolly)
	}

	implicit object Churches extends T[Church]("churches") {
		val religion = column("religion", _.religion)
		val priest = column("priest", _.priest)

		def assemble(implicit res: ResultRow): Church = Church(id, name, religion, priest)
	}

	implicit object Believers extends T[Believer]("believers") {
		val church = column("church", _.church)

		def assemble(implicit res: ResultRow): Believer = Believer(id, name, church)

	}

	implicit object Sins extends T[Sin]("sins") {
		val commitedBy = column("commitedBy", _.commitedBy)

		def assemble(implicit res: ResultRow): Sin = Sin(id, name, commitedBy)
	}

	import SetParameters._

	"EntityMapping" should "create join query" in {
		val (select, _) = Sins.joinSelectStatement(None, _.commitedBy.get.church, _.commitedBy.get.church.get.priest, _.commitedBy.get.church.get.religion)
		System.err.println(select)
	}
}

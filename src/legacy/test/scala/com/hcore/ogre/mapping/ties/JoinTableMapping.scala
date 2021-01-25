package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.Mapping
import com.hcore.ogre.mapping.Mapping.SetParameters
import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.model.Reference
import com.hcore.ogre.sql.{SQLReadForm, SQLWriteForm}

import scala.slick.jdbc.SetParameter


trait GenericJoinTableMapping[L, LT<:Mapping[L], LR<:Reference[L], R, RT<:Mapping[R], RR<:Reference[R]]
	extends Mapping[(LR, RR)]
{
	val left :Component[LR]
	val right :Component[RR]

	override def InsertParameters = SetParameter[(LR, RR)] {
		case ((l, r), params) =>
			left.InsertParameters(l, params)
			right.InsertParameters(r, params)
	}

	override def UpdateParameters: SetParameter[(LR, RR)] = SetParameter[(LR, RR)] {
		case ((l, r), params) =>
			left.QueryParameters(l, params)
			right.QueryParameters(r, params)
	}

	override def QueryParameters: SetParameter[(LR, RR)] = SetParameter[(LR, RR)] {
		case ((l, r), params) =>
			left.QueryParameters(l, params)
			right.QueryParameters(r, params)
	}

	override def selectForm: SQLReadForm[(LR, RR)] = left.selectForm * right.selectForm
	override def queryForm: SQLWriteForm[(LR, RR)] = left.queryForm * right.queryForm
	override def updateForm: SQLWriteForm[(LR, RR)] = left.updateForm * right.updateForm
	override def insertForm: SQLWriteForm[(LR, RR)] = left.insertForm * right.insertForm

	override lazy val components = Seq(left, right)

	override def subcomponents: Seq[Component[_]] = ???

	override def columns: Seq[Component[_]] = ???

	override def querable: Seq[Component[_]] = ???
	override def selectable: Seq[Component[_]] = ???
	override def updatable: Seq[Component[_]] = ???
	override def insertable: Seq[Component[_]] = ???
	override def generated: Seq[Component[_]] = ???
}




//class JoinTable[L, (val tableName, val schemaName :Option[String]=None)(val left )

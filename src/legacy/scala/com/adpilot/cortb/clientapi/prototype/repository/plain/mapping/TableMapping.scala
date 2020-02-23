package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnMapping.ColumnOption.OptionalSelect
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.Mapping.SetParameters
import com.adpilot.cortb.clientapi.util.Repeat

import scala.slick.jdbc.{StaticQuery => Q, StaticQueryInvoker, GetResult, SetParameter}


import Repeat._

trait TableMapping[E] extends Mapping[E] { self =>
	val tableName :String
	def schemaName :Option[String]

	type ExecutableQuery = StaticQueryInvoker[_, E]
	
	lazy val qname = schemaName.filter(_.length>0).map(_+"."+tableName) getOrElse tableName

	lazy val selectClause = selectable.map(_.selectHeader("")).mkString("select ", ", ", s" from $qname as $tableName ")

	lazy val insertClause = insertable.map(_.name).mkString(s"insert into $qname(", ", ", ") ")

	lazy val insertValuesStmt = insertable.map(_=>"?").mkString(s"$insertClause values (", ", ", ") ")

	lazy val insertReturningStmt =
		if (generated.isEmpty) insertValuesStmt
		else generated.map(_.name).mkString(s"$insertValuesStmt returning ", ", ", "")

	lazy val updateClause = updatable.map(_.name+"=?").mkString(s"update $qname $tableName set ", ", ", "")

	lazy val deleteClause = s"delete from $qname $tableName "


	def parameter[X](component :Component[X]) :SetParameter[X] = SetParameters[X](component.querable:_*)



	def where[X](component :Component[X]) :String = where(tableName, component)

	def where[X](alias :String, component :Component[X]) :String =
		component.querable.map(col => s"$alias.${col.name} = ?").mkString(" and ")

	def where[X](component :Component[X], value :String) :String =
		where(tableName, component, value)

	def where[X](alias :String, component :Component[X], value :String) :String =
		component.querable match {
			case Seq() => ""
			case Seq(col) => s"$alias.${col.name} = $value"
			case Seq(cols @_*) => cols.map(col => s"$alias.${col.name}").mkString("(", ", ", s") = $value")
		}


	def whereIn[X](component :Component[X], count :Int) :String = whereIn(tableName, component, count)

	def whereIn[X](alias :String, component :Component[X], count :Int) :String = count match {
		case 0 => "1=2"
		case 1 => where(alias, component)
		case n => Stream.continually(where(alias, component)).take(n).mkString("(", " or ", ")")
	}




	def filter[X](component :Component[X], value :X) :TableFilter[X] =
		TableFilter[X](where(component), value)(parameter(component))

	def filter[X](alias :String, component :Component[X], value :X) :TableFilter[X] =
		TableFilter[X](where(alias, component), value)(parameter(component))

	def filter[X](component :Component[X], values :Seq[X]) :TableFilter[Seq[X]] =
		TableFilter[Seq[X]](whereIn(component, values.size), values)(SetParameters.repeat(parameter(component)))

	def filter[X](alias :String, component :Component[X], values :Seq[X]) :TableFilter[Seq[X]] =
		TableFilter[Seq[X]](whereIn(alias, component, values.size), values)(SetParameters.repeat(parameter(component)))




	def whereStatement(selectPrefix :String, condition :String) =
		if (condition.startsWith("where")) selectPrefix + " " + condition
		else selectPrefix + " where " + condition


	private[this] implicit def GetResultEntity :GetResult[E] = this

	def selectWhere[P :SetParameter](condition :String) :Q[P, E] =
		Q.query[P, E](whereStatement(selectClause, condition))

	def selectWhere[P](filter :TableFilter[P]) :ExecutableQuery =
		selectWhere(filter.whereClause)(filter.Setter)(filter.value)


	def updateWhere[P :SetParameter](condition :String) = {
		implicit val set = UpdateParameters
		Q.update[(E, P)](whereStatement(updateClause, condition))
	}


	def deleteWhere[P :SetParameter](condition :String) :Q[P, Int] =
		Q.update[P](whereStatement(deleteClause, condition))

//	def deleteWhere[P](filter :TableFilter[P]) :Q[P, Int] =
//		Q.update[P](filter.whereClause)

	def insertValue =
		Q.update[E](insertValuesStmt)(InsertParameters)

	def insertReturning[R :GetResult] =
		Q.query[E, R](insertReturningStmt)(InsertParameters, implicitly[GetResult[R]])


	override def toString = qname

}



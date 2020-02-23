package com.adpilot.cortb.clientapi.golem.sql

import com.adpilot.cortb.clientapi.golem._

final case class Select[From, Row](selector :From => Row)

trait Query[T<:RowDef] { //[Row[S <: QueryRow.Source] <: QueryRow[S]] {
	import Query._

//	type Source <: QueryRow.Source
//	type Row[S<:RowDef] = QueryRow[S, Source]
	class Row extends RowDef {
		implicit protected val query :Query[T] = Query.this
	}
//	type Value[T] = Source#Value[T]
//	type Column[T] =

	val columns :T

//	type Row = QueryRow[Column]//<:QueryRow[R]

//	def select[SelRow[S <: QueryRow.Source] <: QueryRow[S]](map :Row[Source] => SelRow[Source]) :Query[SelRow]
//	def select[R <:RowDef with Row](map :QueryRow[T, this.type] => R) :Query[R]
	def select[R <: Row](map :this.type => R) :Query[R]

	def from_:[R<:Row] (sel :Select[this.type, R]) :Query[R] = select(sel.selector)
//	def pack[V :RValue.Type](map :this.type => V) :
}

object Query {


//
//	val f = new { val hello = "world" } with C(_)

//	implicit class TableQuery[T<:Table](table :T) extends Query[T] {
//		val selection = table
//	}

//	implicit def queryDefinition[Row](query :Query[Row]) :query.selection.type = query.selection
	implicit def columnsOf[T<:RowDef](q :Query[T]) :T = q.columns

	import JDBCTypes._
//	import RValue._

	object TestTable extends RowDef {
		implicit def ct[T :RValue.Type] :ColumnType[T] = new ColumnType[T]{}

		val first = column[String]("first")
		val second = column[String]("second")

		def from_:[R<:RowDef] (select :Select[Query[TestTable], R]) :Query[R] = ???
	}
	type TestTable = TestTable.type
	val table = TestTable

	def concat[Q<:Query[TestTable]](q :Q) = new q.Row { val name = q.first + q.second }

	val q = Select((t :Query[TestTable]) => new t.Row { val name = t.first + t.second } ) from_: table

//	type xint = { type X[T] = { val x1 :T; val x2 :T }; val x :X[Int];  }#X[Int]

	val tableQuery :Query[TestTable] = ???
	val nameQuery = tableQuery.select(q => new q.Row { val name = q.first + q.second })
	val name2 = tableQuery.select(concat(_))

	val secondQuery :Query[TestTable] = ???
	val secondHeader = new secondQuery.Row { val name = secondQuery.first + secondQuery.second }
//	val clash = tableQuery.select(q => secondHeader)

}


object From {
//	Seq(("hello", 1)).map{ case(s, i) => new { val string = s; val int = i }}.map(x => x.string )
//	def apply[T<:Table](table :T) :Query[]
}

//trait PackedQuery[Entity, Row<:RowPacking[Entity]] extends Query[Row] {
//
//}
//
//
//object PackedQuery {
//	implicit class MappedTableQuery[Entity, T<:MappedTable[Entity]](table :T) extends Query.TableQuery[T](table)
//
//
//}
//
//
//trait ContextQuery[Entity, Ctx, Row<:RowContextPacking[Entity, Ctx]] extends Query[Row]
//
//object ContextQuery {
//	implicit class EntityTableQuery[Entity, PC, UC, T<:EntityTable[Entity, PC, UC]](table :T) extends Query.TableQuery[T](table)
//
//}



class QueryRow[T<:RowDef, S<:QueryRow.Source](val row :T) {
//	type Row = T
	type Row[R<:RowDef] = QueryRow[R, S]
//	val row :Row


}

object QueryRow {
//	class TestCols extends Columns {
//		type Column[T] = ColumnDef[T]
//		def columns = Seq()
//	}
	implicit def asRow[T<:RowDef, S<:QueryRow.Source](qr :QueryRow[T, S]) :T = qr.row

	trait Source {
		type Column[T] <: ColumnDef[T]
		type Value[T] <: RValue[T]

		def column[T](name :String) :Column[T]
	}

//	class BaseTable[S <:Source] extends QueryRow[S] {
//		def column[T](name :String) :S#Column[T] = ??? //new ColumnDef[T](null, name)
//
//	}
//
//	class TestTable[S<:Source] extends BaseTable[S] {
//		val firstName = column[String]("first")
//		val familyName = column[String]("second")
//	}

//	def query[Row[S <: QueryRow.Source] <: QueryRow[S]] :Query[Row] = ???
//	val nameQ :Query[TestTable] = ???

//	nameQ.select(row => new row.Def { val x = row.firstName() }).select(row => row.x )

//	new QueryRow[TestCols#Column] {}
//	implicit def selectionOf[R<:RowDef, Q<:QueryRow[R]](row :Q) :row.selection.type = row.selection
}


trait Columns {
	type Column[T] <: ColumnDef[T]

	def columns :Seq[Column[_]]
}

//case class Join[Left, Right](left :Query[Left], right :Query[Right]) extends
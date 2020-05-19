package com.hcore.ogre.sql

import com.hcore.ogre.mapping.{Mapping, AnyMapping}
import com.hcore.ogre.mapping.ComponentPath.SelfPath
import com.hcore.ogre.mapping.Mapping.ColumnFilter
import com.hcore.ogre.mapping.Mapping.ColumnFilter.{ForSelect, ForQuery}
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.slang.repeat
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.sql.RowSource.{ParamMapping, TableFormula}
import com.hcore.ogre.sql.SQLFormula.SelectFormula.{CaseSelect, SelectAsRow, SelectAsRows}
import com.hcore.ogre.sql.SQLFormula.TupleFormula.CaseTuple
import com.hcore.ogre.sql.SQLFormula._
import com.hcore.ogre.sql.SQLMapper.{SQLReducer, FixedResult=>Res}
import shapeless.HList

import scala.Null

//implicits

import SaferCasts._
import extensions._

class SQLPrinter[S<:RowSource] private (filter :ColumnFilter, aliases :Map[TableFormula[S, _], String])
//	extends ActualSQLVisitor[S, StringBuilder]
	extends SQLReducer[S, StringBuilder] with CaseTuple[S, Res[StringBuilder]#T] with CaseSelect[S, Res[StringBuilder]#T]
{
	private var buffer :StringBuilder = null



	override def apply[X](expression: SQLFormula[S, X]) =
		if (buffer!=null)
			throw new IllegalStateException(s"SQLPrinter($expression) called while printing $buffer")
		else {
			buffer = new StringBuilder
			val res = print(expression)
			buffer = null
			res
		}

	protected def print[X](expression :SQLFormula[S, X]) :StringBuilder = super.apply(expression)

	override def equality[X](e: Equality[S, X]) = e match {
		case Equality(left, Null()) => print(left); buffer ++= " is null"
		case Equality(Null(), right) => print(right); buffer ++= " is null"
		case Equality(left, right) => print(left); buffer ++= "="; print(right)
	}


	override def in[X](e: In[S, X]) = {
		print(e.left) ++= " in "
		print(e.right)
	}


//	override def seq[X](e: SeqFormula[S, X]) = print(e.expressions, ", ", buffer++="()")

//	override def tuple[X <: HList](e: HListFormula[S, X]) = print(e.toSeq, ", ")

	override def tuple[X](f: TupleFormula[S, X]) = print(f.parts, ", ")


	override def or(e: Or[S]) = print(e.conditions, " or ", print(False))

	override def and(e: And[S]) = print(e.conditions, " and ", print(False))

	override def not(e: NotFormula[S]): StringBuilder = {
		buffer ++="not "
		print(e.formula)
	}

	override def sqlNull[X](e: SQLFormula.Null[X]) = buffer ++= "null"

//	override def bool(e: Literal[Boolean]) = buffer ++= e.sqlForm.literal(e.value)
//
//	override def nonbool[X](e: Literal[X]) = buffer ++= e.sqlForm.literal(e.value)


	override def literal[X](f: Literal[X]): StringBuilder = buffer ++= f.sqlForm.literal(f.value)

	override def param[X](e: BoundParameter[X]) = placeholders(e.sqlForm)

//	override def parameter[X](e: UnboundParameter[X]) = placeholders(e.sqlForm)

	override def native[X](e: NativeTerm[X]) = buffer ++= e.value


//	override def option[X](e: OrNull[S, X]) = print(e.expr)

	override def conversion[Z, X](f: AutoConversionFormula[S, Z, X]): StringBuilder = print(f.expr)


	override def component[M <: AnyMapping, C <: AnyMapping](e: ComponentFormula[S, M, C]) = e match {
		case ParamMapping(_, _, form) => placeholders(form)
		case _ =>
			tableAlias(e.table).map { alias =>
				val columns = e.path.end.columnsFor(filter).map{ c => e.path.lift(c) getOrElse {
					throw new IllegalArgumentException(s"Can't lift column $c of ${e.path}")
				} :M#Component[_]}
				val columnNames = columns.map(c => (alias.providing(_.length>0).map(_+".") getOrElse "") + c.sqlName.getOrElse {
					throw new IllegalArgumentException(s"Can't format sql expression $e: sqlName not defined for column $c in ${e.path.end}")
				})
				columnNames match {
					case Seq() if filter==ForSelect => buffer
					case Seq() => buffer ++= "()" //throw new IllegalArgumentException(s"Can't format expression $e: no columns in mapping ${e.component.end}")
					case Seq(col) => buffer++=col
					case _  if filter==ForSelect =>
						columnNames.foreach{ c => buffer ++= c; buffer ++= ", " }
						buffer.delete(buffer.length-2, buffer.length)
					case _ =>
						buffer ++= "("
						columnNames.foreach{ c => buffer ++= c; buffer ++=", " }
						buffer.delete(buffer.length-2, buffer.length)
						buffer ++= ")"
				}
			} getOrElse { throw new IllegalArgumentException(s"Can't format sql expression $e: no alias defined for last ${e.table}") }

	}


	override def path[M <: AnyMapping, C <: AnyMapping](e: PathFormula[S, M, C]): StringBuilder =
		throw new IllegalArgumentException(s"Can't format path expression in sql: $e")





	override def select[H](e: SelectFormula[S, H]) = {
		val widenedAliases = aliases.toMap[Any, String]
		val acc = (Map[TableFormula[e.Source, _], String](), aliases.values.toSet)
		val subaliases = (acc /: e.source.all) {
			case ((aliases, reserved), table) =>
				widenedAliases.get(table).orElse(SQLPrinter.aliasOpt(table, reserved)).map { alias =>
					(aliases  + (table -> alias), reserved + alias)
				} getOrElse (aliases, reserved)
//				val alias = widenedAliases.getOrElse(last, SQLPrinter.alias(last, reserved))
//				(aliases + (last -> alias), reserved + alias)
		}._1

		buffer ++= "select "

		buffer ++= ((e.header, SQLPrinter(e.header, subaliases, ForSelect)) match {
			case (TupleFormula(parts), expr) if parts.size>1 && expr.startsWith("(") && expr.endsWith(")") =>
				expr.substring(1, expr.length-1)
			case (_, expr) => expr
		})
		e.from match { //todo: don't ignore outer joins!
			case Seq() =>
			case tables =>
				buffer ++= " from "
				tables.foreach{ t =>
					buffer ++= t.mapping.sqlName.get ++= " as " ++= subaliases(t) ++=", "
				}
				buffer.delete(buffer.length-2, buffer.length)
		}
		e.filter match {
			case True() => buffer
			case cond =>
				buffer ++= " where " ++= SQLPrinter(e.source.filter, subaliases)
		}

	}

	override def row[H](e: SelectAsRow[S, H]) = {
		buffer ++= "("
		select(e.select)
		buffer ++= ")"
	}

	override def rows[H](e: SelectAsRows[S, H]) = {
		buffer ++= "("
		select(e.select)
		buffer ++= ")"
	}

	override def exists[H](e: ExistsFormula[S, H]) = {
		buffer ++= "exists("
		select(e.select)
		buffer ++=")"
	}


	private def print(expressions :Seq[SQLFormula[S, _]], separator :String, whenEmpty : =>StringBuilder = buffer) :StringBuilder = expressions match {
		case Seq() => whenEmpty
		case Seq(single) => print(single)
		case elems =>
			buffer ++= "("
			elems.foreach{ b => print(b); buffer ++= separator }
			buffer.delete(buffer.length - separator.length, buffer.size)
			buffer ++= ")"
	}

	private def placeholders(form :SQLWriteForm[_]) = form.writtenColumns match {
		case 0 => buffer ++= "()"
		case 1 => buffer ++= "?"
		case n =>
			buffer ++= "("
			(n-1).times { buffer ++= "?, " }
			buffer ++= "?)"
	}


	private def tableAlias(table :TableFormula[S, _<:AnyMapping]) :Option[String] =
		aliases.get(table) orElse SQLPrinter.aliasOpt(table)
}



object SQLPrinter {


	def apply[S<:RowSource, T](expr :SQLFormula[S, T], context :ColumnFilter) :String =
		apply(expr, Map[TableFormula[S, _], String](), context)

	def apply[S<:RowSource, T](expr :SQLFormula[S, T], aliases :Map[TableFormula[S, _], String]=Map[TableFormula[S, _], String](), context :ColumnFilter=ForQuery) :String =
		new SQLPrinter[S](context, aliases).apply(expr).result
	
	//todo: remove
	@deprecated
	def apply[S<:RowSource](source :S) :String = apply(source.filter)
	//todo: remove
	@deprecated
	def apply[S<:RowSource](source :S, aliases :Map[TableFormula[S, _], String]) :String = apply[S, Boolean](source.filter, aliases)


	def unaliased[S<:RowSource](table :TableFormula[S, _<:AnyMapping]) :Map[TableFormula[S, _], String] =
		Map(table -> "")

	def aliased[S<:RowSource](table :TableFormula[S, _<:AnyMapping]) :Map[TableFormula[S, _], String] =
		Map(table -> alias(table, Set()))

	def defaults[S<:RowSource] :Map[TableFormula[S, _], String] = Map()

	def aliases[S<:RowSource](source :S) :Map[TableFormula[S, _], String] =
		((Set[String](), Map[TableFormula[S, _], String]()) /: source.all) {
			case ((reserved, aliases), table) =>
				val alias = this.alias(table, reserved)
				(reserved + alias, aliases + (table -> alias))
		}._2


	def aliasOpt[S<:RowSource](table :TableFormula[S, _<:AnyMapping], reserved :Set[String]=Set[String]()) :Option[String] =
		table.mapping.sqlName.map { name =>
			if (name.startsWith("select")) "select"
			else name.substring(name.lastIndexOf('.') + 1)
		} map { first =>
			first.unless(reserved) getOrElse Stream.iterate(1)(_+1).view.map(first + _).filterNot(reserved).head
		}


	def alias[S<:RowSource](table :TableFormula[S, _<:AnyMapping], reserved :Set[String]=Set[String]()) :String =
		aliasOpt(table, reserved) getOrElse {
			throw new IllegalArgumentException(s"Can't create a table alias for joined table without sqlName $table(${table.mapping.introString})")
		}




}
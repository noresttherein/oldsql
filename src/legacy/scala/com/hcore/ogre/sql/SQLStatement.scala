package com.hcore.ogre.sql

import com.hcore.ogre.mapping.AnyMapping
import com.hcore.ogre.sql.SQLForm.UnitForm
import com.hcore.ogre.sql.SQLStatement.ParameterizedStatement


trait SQLStatementTemplate[X, +Y] {
	def sql :String
	def input :SQLWriteForm[X]
	def output :SQLReadForm[Y]

	def apply(value :X) :SQLStatement[Y] = new ParameterizedStatement(this, value)

	def canEqual(that :Any) = that.isInstanceOf[SQLStatementTemplate[_, _]]

	override def equals(that :Any) = that match {
		case s :SQLStatementTemplate[_, _] =>
			(s eq this) || (s.canEqual(this) && canEqual(s) && sql==s.sql)
		case _ => false
	}

	override def hashCode = sql.hashCode

	override def toString = s"'$sql'($input, $output)"
}



trait SQLStatement[+Y] {
	type Variable
	def template :SQLStatementTemplate[_, Y]
	def param :Variable

	def output :SQLReadForm[Y] //= template.output
	def input :SQLWriteForm[Variable] //= template.input

	def sql :String //= template.sql

	def canEqual(that :Any) = that.isInstanceOf[SQLStatement[_]]

	override def equals(that :Any) = that match {
		case s :SQLStatement[_] =>
			(s eq this) || (s.canEqual(this) && canEqual(that) && param==s.param && template==s.template)
		case _ => false
	}

	override def hashCode = (template, param).hashCode

	override def toString = s"($template)($param)"
}


object SQLStatement {
//	def apply(sql :String) :LiteralStatement[Unit] = LiteralCommand(sql)

	def apply[Y](input :SQLWriteForm[Unit], sql :String, output :SQLReadForm[Y]) :SQLStatement[Y] =
		ComposedStatement(sql, input, output)

	def apply[X, Y](template :SQLStatementTemplate[X, Y], param :X) :SQLStatement[Y] =
		new ParameterizedStatement(template, param)


	trait AutonomousStatement[+Y] extends SQLStatement[Y] with SQLStatementTemplate[Unit, Y] {
		type Variable = Unit

		def template = this

		def param = ()

		override def canEqual(that :Any) = that.isInstanceOf[AutonomousStatement[_]]

		override def hashCode = super[SQLStatementTemplate].hashCode

		override def toString = super[SQLStatementTemplate].toString
	}

	case class ComposedStatement[Y](sql :String, override val input :SQLWriteForm[Unit], output :SQLReadForm[Y])
		extends AutonomousStatement[Y]


	trait LiteralStatement[+Y] extends AutonomousStatement[Y] {
		override def input :SQLWriteForm[Unit] = SQLForm.UnitForm

		override def canEqual(that :Any) = that.isInstanceOf[LiteralStatement[_]]
	}

	trait SQLCommandTemplate[X] extends SQLStatementTemplate[X, Unit] {
		def output :SQLReadForm[Unit] = SQLForm.UnitForm
	}

//	trait SQLCommand extends SQLStatement[Unit] {
//		def output = SQLForm.UnitForm
//	}

	case class LiteralCommand(override val sql :String) extends LiteralStatement[Unit] with SQLCommandTemplate[Unit] {
		override def apply(value: Variable): SQLStatement[Variable] = this
		override def output = super[SQLCommandTemplate].output
	}

	type SQLCommand = SQLStatement[Unit]


//	trait TableStatement[T<:Mapping] {
//		val table :T
//	}
	
	class ParameterizedStatement[X, +Y](val template :SQLStatementTemplate[X, Y], val param :X) extends SQLStatement[Y] {
		type Variable = X

		def output = template.output
		def input = template.input

		def sql = template.sql
	}
}

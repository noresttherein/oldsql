package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, ConstantBuff, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, ForcedQuery, InsertAudit, NoInsert, NoQuery, NoSelect, NoUpdate, Nullable, OptionalSelect, OptionalUpdate, QueryAudit, SelectAudit, UpdateAudit}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelector, ComponentFor}
import net.noresttherein.oldsql.slang._

import scala.util.Try



trait ColumnMapping[O, S] extends SubMapping[O, S] { column =>

	override def sqlName = Some(name)

	def name :String

	final def components :Unique[Nothing] = Unique.empty
	final def subcomponents :Unique[Component[S]] = Unique.empty

	final def columns :Unique[Component[S]] = Unique(this)
//	final def selectable :Seq[Component[S]] = Seq(this)
//	final def queryable :Seq[Component[S]] = Seq(this)
//	final def updatable :Seq[Component[S]] = Seq(this)
//	final def insertable :Seq[Component[S]] = Seq(this)
//	final def generated :Seq[Component[S]] = Seq(this)

	final override def selectable :Unique[Component[S]] = selfUnless(NoSelect)
	final override def queryable :Unique[Component[S]] = selfUnless(NoQuery)
	final override def updatable :Unique[Component[S]] = selfUnless(NoUpdate)
	final override def autoUpdated :Unique[Component[S]] = if (AutoUpdate.enabled(buffs)) Unique(this) else Unique.empty
 	final override def insertable :Unique[Component[S]] = selfUnless(NoInsert)
	final override def autoInserted :Unique[Component[S]] = if (AutoInsert.enabled(buffs)) Unique(this) else Unique.empty

	protected def selfUnless(buff :BuffType) :Unique[Component[S]] =
		if (buff.enabled(buffs)) Unique.empty else Unique(this)



	def form :SQLForm[S]
	
	override def selectForm :SQLReadForm[S] = ExtraSelect.test(buffs) match {
		case Some(ConstantBuff(x)) => SQLReadForm.const(x) 
		case Some(buff) => SQLReadForm.eval(buff.value)
		case _ =>
			val audits = SelectAudit.Audit(buffs)
			val read =
//				if (Nullable.enabled(buffs))
					if (audits.isEmpty) form
					else form.map(audits.reduce(_ andThen _), form.nullValue)
/*
				else {
					val notNull =  { s :S =>
						if (s == null)
							throw new NullPointerException("Read a null value for non-nullable column " + sqlName +
								". Flag the column with Buff.Nullable to explicitly allow nulls.")
						s
					}
					if (audits.isEmpty) form map notNull
					else form.map((audits :\ notNull)(_ andThen _))
				}
*/
			OptionalSelect.test(buffs) match {
				case Some(ConstantBuff(x)) => read orElse SQLReadForm.const(x)
				case Some(buff) => read orElse SQLReadForm.eval(buff.value)
				case _ => read
			}

	}
	
	override def queryForm :SQLWriteForm[S] = ExtraQuery.test(buffs) match {
		case Some(ConstantBuff(x)) => SQLWriteForm.const(x)(form)
		case Some(buff) => SQLWriteForm.eval(buff.value)(form)
		case _ =>
			val audits = QueryAudit.Audit(buffs)
			if (audits.isEmpty) form
			else form.unmap(audits.reduce(_ andThen _))
	}
	
	override def updateForm :SQLWriteForm[S] = ExtraUpdate.test(buffs) match {
		case Some(ConstantBuff(x)) => SQLWriteForm.const(x)(form)
		case Some(buff) => SQLWriteForm.eval(buff.value)(form)
		case _ =>
			val audits = UpdateAudit.Audit(buffs) 
			if (audits.isEmpty) form
			else form.unmap(audits.reduce(_ andThen _))
	}

	override def insertForm :SQLWriteForm[S] = ExtraInsert.test(buffs) match {
		case Some(ConstantBuff(x)) => SQLWriteForm.const(x)(form)
		case Some(buff) => SQLWriteForm.eval(buff.value)(form)
		case _ =>
			val audits = InsertAudit.Audit(buffs)
			if (audits.isEmpty) form
			else form.unmap(audits.reduce(_ andThen _))
	}



	override def lift[T](component :Component[T]) :Component[T] = component

	override def apply[T](component :Component[T]) :Selector[T] =
		if (component == this)
			ComponentSelector.ident[this.type, O, S](this).asInstanceOf[Selector[T]]
		else
			throw new IllegalArgumentException(
				s"Mapping $component is not a subcomponent of column $column. The only subcomponent of a column is the column itself."
			)



	override def assemble(values: Values): Option[S] = None

	override def optionally(values :Values) :Option[S] =
		if (isNullable)
			values.result(this)
		else
			values.result(this) match {
				case Some(null) =>
					throw new NullPointerException("Read a null value for a non-nullable column " + sqlName +
						". Flag the column with Buff.Nullable to explicitly allow nulls.")
				case res => res
			}



	protected def isNullable :Boolean = Nullable.enabled(buffs)



	def withBuffs(opts :Seq[Buff[S]]) :ColumnMapping[O, S] = ColumnMapping(name, opts:_*)(form)

	def renamed(name :String) :ColumnMapping[O, S] = ColumnMapping(name, buffs:_*)(form)

	override def prefixed(prefix :String) :ColumnMapping[O, S] = ColumnMapping(prefix + name, buffs:_*)(form)

	def prefixed(prefix :Option[String]) :ColumnMapping[O, S] = prefix.map(prefixed) getOrElse this

	override def qualified(prefix :String) :ColumnMapping[O, S] =
		prefixOption(prefix).map(p => prefixed(p + ".")) getOrElse this




	override def toString :String = name + "[" + form + "]"

	override def introString :String = buffs.mkString(toString + "(", ", ", ")")

}






object ColumnMapping {

	def apply[O, T :SQLForm](name :String, opts :Buff[T]*) :ColumnMapping[O, T] =
		new StandardColumn(name, opts)

	def adapt[O, T](mapping :ComponentFor[T]) :Option[ColumnMapping[O, T]] = mapping match {
		case c :ColumnMapping[_, _] => Some(c.asInstanceOf[ColumnMapping[O, T]])
		case _ => Try(new ColumnView[O, T](mapping)).toOption
	}



	def apply[O, T](mapping :ComponentFor[T]) :ColumnMapping[O, T] = new ColumnView[O, T](mapping)








	class StandardColumn[O, T](val name :String, override val buffs :Seq[Buff[T]])(implicit val form :SQLForm[T])
		extends ColumnMapping[O, T]
	{
		override val isNullable :Boolean = super.isNullable
	}


	trait ColumnSubstitute[O, S, T] extends ColumnMapping[O, T] {
		val adaptee :ColumnMapping[O, T]

		override def name: String = adaptee.name
	}

	trait ColumnImpostor[O, T] extends ColumnSubstitute[O, T, T] {
		override def form: SQLForm[T] = adaptee.form

		override def buffs :Seq[Buff[T]] = adaptee.buffs

//		override def assemble(values: Values): Option[T] =
//			(values :\ adaptee).result(adaptee)
	}

	class ColumnOverride[O, T]
			(val adaptee :ColumnMapping[O, T], nameOverride :Option[String]=None, buffsOverride :Option[Seq[Buff[T]]]=None)
		extends ColumnImpostor[O, T]
	{
		override val buffs :Seq[Buff[T]] = buffsOverride getOrElse adaptee.buffs
		override val name :String = nameOverride getOrElse adaptee.name
		override val isNullable = super.isNullable
	}




	class ColumnView[O, T](private val adaptee :ComponentFor[T]) extends ColumnMapping[O, T] {

		if (adaptee.columns.size!=1 || adaptee.selectForm.readColumns!=1 || adaptee.insertForm.writtenColumns!=1 || adaptee.updateForm.writtenColumns!=1)
			throw new IllegalArgumentException(s"Expected a column, got a multiple column mapping :$adaptee{${adaptee.columns}}")

		override val name :String = adaptee.sqlName getOrElse {
			throw new IllegalArgumentException(s"Expected a column, got a mapping without an sqlName :$adaptee{${adaptee.columns}}")
		}


		override val form :SQLForm[T] = SQLForm.combine(
			adaptee.selectForm,
			adaptee.insertForm unless(_.writtenColumns == 0) getOrElse adaptee.updateForm
		)

		override def buffs :Seq[Buff[T]] = adaptee.buffs

		override val isNullable :Boolean = super.isNullable


		override def equals(other: Any): Boolean = other match {
			case that: ColumnView[_, _] => adaptee == that.adaptee
			case _ => false
		}

		override def hashCode(): Int = adaptee.hashCode
	}


}


package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, ConstantBuff, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, ForcedQuery, InsertAudit, NoInsert, NoQuery, NoSelect, NoUpdate, Nullable, OptionalSelect, OptionalUpdate, QueryAudit, SelectAudit, UpdateAudit}
import net.noresttherein.oldsql.schema.ColumnMapping.NumberedColumn
import net.noresttherein.oldsql.schema.Mapping.{ComponentFor, ComponentSelector}
import net.noresttherein.oldsql.slang._

import scala.util.Try



trait ColumnMapping[O, S] extends AbstractMapping[O, S] { column =>

	/** Returns `Some(this.name)`. */
	final override def sqlName = Some(name)

	/** The name of this column, as seen from the containing table if it is a table/view column.
	  * If this column represents a column in the SELECT clause of a select statement, it is the column name as
	  * returned by the result set for that statement. The names of columns selected in a subselect query are qualified
	  * with the table/view/subselect name/alias from the FROM clause and any aliases given in the SELECT clause are
	  * applied.
	  */
	def name :String

	/** Returns `Unique.empty`. */
	final def components :Unique[Nothing] = Unique.empty
	/** Returns `Unique.empty`. */
	final def subcomponents :Unique[Component[S]] = Unique.empty

	/** Returns `Unique(this)`. */
	final def columns :Unique[Component[S]] = Unique(this)
//	final def selectable :Seq[Component[S]] = Seq(this)
//	final def queryable :Seq[Component[S]] = Seq(this)
//	final def updatable :Seq[Component[S]] = Seq(this)
//	final def insertable :Seq[Component[S]] = Seq(this)
//	final def generated :Seq[Component[S]] = Seq(this)

	final override def selectable :Unique[Component[S]] = selfUnless(NoSelect)
	final override def queryable :Unique[Component[S]] = selfUnless(NoQuery)
	final override def updatable :Unique[Component[S]] = selfUnless(NoUpdate)
	final override def autoUpdated :Unique[Component[S]] = selfIf(AutoUpdate)
 	final override def insertable :Unique[Component[S]] = selfUnless(NoInsert)
	final override def autoInserted :Unique[Component[S]] = selfIf(AutoInsert)

	/** An empty `Unique` if the given buff is ''disabled'' (not attached), or a singleton `Unique(this)` otherwise. */
	protected def selfUnless(buff :BuffType) :Unique[Component[S]] =
		if (buff.enabled(buffs)) Unique.empty else Unique(this)

	/** An empty `Unique` if the given buff is ''enabled'' (attached), or a singleton `Unique(this)` otherwise. */
	protected def selfIf(buff :BuffType) :Unique[Component[S]] =
		if (buff.disabled(buffs)) Unique.empty else Unique(this)



	/** A read-write form for this column's type which does not take into account any buffs attached to this column.
	  * Serves as the basis for `selectForm`, `queryForm`, `insertForm` and `updateForm` which, in most cases,
	  * should be used instead.
	  */
	def form :ColumnForm[S]
	
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



	override def assemble(pieces: Pieces): Option[S] = None

	override def optionally(pieces :Pieces) :Option[S] =
		if (isNullable)
			pieces.result(this)
		else
			pieces.result(this) match {
				case Some(null) =>
					throw new NullPointerException("Read a null value for a non-nullable column " + name +
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

	def numbered(index :Int) :NumberedColumn[O, S] = new NumberedColumn[O, S](index, name, buffs)(form)




	override def toString :String = name + "[" + form + "]"

	override def introString :String = buffs.mkString(toString + "(", ", ", ")")

}






object ColumnMapping {

	def apply[O, T :ColumnForm](name :String, buffs :Buff[T]*) :ColumnMapping[O, T] =
		new StandardColumn(name, buffs)





	class StandardColumn[O, T](val name :String, override val buffs :Seq[Buff[T]])(implicit val form :ColumnForm[T])
		extends ColumnMapping[O, T]
	{
		override val isNullable :Boolean = super.isNullable
	}


	class NumberedColumn[O, T](val number :Int, val name :String, override val buffs :Seq[Buff[T]])
	                          (implicit val form :ColumnForm[T])
		extends ColumnMapping[O, T]
	{
		override val isNullable :Boolean = super.isNullable
	}
}


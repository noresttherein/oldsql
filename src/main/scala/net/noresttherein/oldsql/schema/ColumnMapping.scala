package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, ConstantBuff, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, FlagBuffType, InsertAudit, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, Nullable, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate, QueryAudit, SelectAudit, UpdateAudit}
import net.noresttherein.oldsql.schema.ColumnMapping.NumberedColumn
import net.noresttherein.oldsql.schema.Mapping.ComponentExtractor
import net.noresttherein.oldsql.schema.support.{EmptyMapping, LabeledMapping}






trait ColumnMapping[S, O] extends GenericMapping[S, O] { column =>

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
	final def components :Unique[Component[_]] = Unique.empty
	/** Returns `Unique.empty`. */
	final def subcomponents :Unique[Component[_]] = Unique.empty

	/** Returns `Unique(this)`. */
	final def columns :Unique[Component[S]] = Unique(this)

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



	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		if (components.size == 1 && components.contains(this)) selectForm
		else if (components.isEmpty) SQLReadForm.none
		else throw new NoSuchElementException("Mappings " + components + " are not components of column " + this)

	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components.size == 1 && components.contains(this)) queryForm
		else if (components.isEmpty) SQLWriteForm.none(form)
		else throw new NoSuchElementException("Mappings " + components + " are not components of column " + this)

	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components.size == 1 && components.contains(this)) updateForm
		else if (components.isEmpty) SQLWriteForm.none(form)
		else throw new NoSuchElementException("Mappings " + components + " are not components of column " + this)

	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components.size == 1 && components.contains(this)) insertForm
		else if (components.isEmpty) SQLWriteForm.none(form)
		else throw new NoSuchElementException("Mappings " + components + " are not components of column " + this)




	protected def customize(optional :BuffType, excluded :FlagBuffType, explicit :BuffType)
	                       (include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		if (include.size > 1)
			throw new IllegalArgumentException("Mappings " + include + " are not components of column " + this)
		else if (exclude.size > 1)
		     throw new IllegalArgumentException("Mappings " + exclude + " are not components of column " + this)
		else if (exclude.headOption.contains(this) && optional.enabled(this))
		     ColumnMapping[S, O](name, excluded[S] +: buffs :_*)(form)
		else if (include.headOption.contains(this) && explicit.enabled(this))
		     ColumnMapping[S, O](name, buffs.filter(explicit.disabled) :_*)(form)
		else
			this

	override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		customize(OptionalSelect, NoSelectByDefault, ExplicitSelect)(include, exclude)

	override def forQuery(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		customize(OptionalQuery, NoQueryByDefault, ExplicitQuery)(include, exclude)

	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		customize(OptionalUpdate, NoUpdateByDefault, ExplicitUpdate)(include, exclude)

	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		customize(OptionalInsert, NoInsertByDefault, ExplicitInsert)(include, exclude)






	override def lift[T](component :Component[T]) :Component[T] = component

	override def apply[T](component :Component[T]) :Selector[T] =
		if (component == this)
			ComponentExtractor.ident[S, O](this).asInstanceOf[Selector[T]]
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



	def withBuffs(opts :Seq[Buff[S]]) :ColumnMapping[S, O] = ColumnMapping(name, opts:_*)(form)

	override def renamed(name :String) :ColumnMapping[S, O] = ColumnMapping(name, buffs:_*)(form)

	override def prefixed(prefix :String) :ColumnMapping[S, O] = ColumnMapping(prefix + name, buffs:_*)(form)

	def prefixed(prefix :Option[String]) :ColumnMapping[S, O] = prefix.map(prefixed) getOrElse this

	override def qualified(prefix :String) :ColumnMapping[S, O] =
		prefixOption(prefix).map(p => prefixed(p + ".")) getOrElse this

	def numbered(index :Int) :NumberedColumn[S, O] = new NumberedColumn[S, O](index, name, buffs)(form)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnMapping[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case column :ColumnMapping[_, _] if canEqual(column) && column.canEqual(this) =>
			column.form == form && column.name == name && column.buffs == buffs
		case _ => false
	}

	override def hashCode :Int = (buffs.hashCode * 31 + name.hashCode) * 31 + form.hashCode



	override def toString :String = name + "[" + form + "]"

	override def debugString :String = buffs.mkString(toString + "(", ", ", ")")

	override def columnString :String = toString


}






object ColumnMapping {

	def apply[S :ColumnForm, O](name :String, buffs :Buff[S]*) :ColumnMapping[S, O] =
		new StandardColumn(name, buffs)

	def apply[N <: String with Singleton :ValueOf, S :ColumnForm, O](buffs :Buff[S]*) :LabeledColumn[N, S, O] =
		new LabeledColumn[N, S, O](buffs)

	def labeled[N <: String with Singleton, S :ColumnForm, O](label :N, buffs :Buff[S]*) :LabeledColumn[N, S, O] =
		new LabeledColumn[N, S, O](buffs)(new ValueOf(label), ColumnForm[S])



	class BaseColumn[S, O](val name :String, override val buffs :Seq[Buff[S]])(implicit val form :ColumnForm[S])
		extends ColumnMapping[S, O]
	{
		override val isNullable :Boolean = super.isNullable
	}



	class StandardColumn[S, O](val name :String, override val buffs :Seq[Buff[S]])(implicit val form :ColumnForm[S])
		extends ColumnMapping[S, O]
	{
		override val isNullable :Boolean = super.isNullable

		override def map[X](there :S => X, back :X => S)(implicit nulls :SQLForm.NullValue[X]) :ColumnMapping[X, O] =
			new StandardColumn[X, O](name, buffs.map(_.map(there)))(
				if (nulls != null) form.bimap(there)(back) else form.bimapNull(there)(back)
			)

		override def flatMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :SQLForm.NullValue[X]) :ColumnMapping[X, O] =
			new StandardColumn[X, O](name, schema.flatMapBuffs(this)(there))(
				if (nulls != null) form.biflatMap(there)(back) else form.biflatMapNull(there)(back)
			)
	}



	class LabeledColumn[N <: String with Singleton, S, O](override val buffs :Seq[Buff[S]] = Nil)
	                                                     (implicit named :ValueOf[N], override val form :ColumnForm[S])
		extends LabeledMapping[N, S, O] with ColumnMapping[S, O]
	{
		override val name :N = named.value
		override val isNullable :Boolean = super.isNullable

		override def withBuffs(buffs :Seq[Buff[S]]) :LabeledColumn[N, S, O] =
			new LabeledColumn[N, S, O](buffs)(new ValueOf(name), form)

		override def map[X](there :S => X, back :X => S)(implicit nulls :SQLForm.NullValue[X]) :LabeledColumn[N, X, O] =
			new LabeledColumn[N, X, O](buffs.map(_.map(there)))(
				implicitly[ValueOf[N]],
				if (nulls != null) form.bimap(there)(back) else form.bimapNull(there)(back)
			)

		override def flatMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :SQLForm.NullValue[X]) :LabeledColumn[N, X, O] =
			new LabeledColumn[N, X, O](schema.flatMapBuffs(this)(there))(
				implicitly[ValueOf[N]],
				if (nulls != null) form.biflatMap(there)(back) else form.biflatMapNull(there)(back)
			)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[LabeledColumn[_, _, _]]

	}



	class NumberedColumn[S, O](val number :Int, val name :String, override val buffs :Seq[Buff[S]])
	                          (implicit val form :ColumnForm[S])
		extends ColumnMapping[S, O]
	{
		override val isNullable :Boolean = super.isNullable

		override def withBuffs(opts :Seq[Buff[S]]) :ColumnMapping[S, O] = new NumberedColumn(number, name, buffs)

		override def map[X](there :S => X, back :X => S)(implicit nulls :SQLForm.NullValue[X]) :NumberedColumn[X, O] =
			new NumberedColumn[X, O](number, name, buffs.map(_.map(there)))(
				if (nulls != null) form.bimap(there)(back) else form.bimapNull(there)(back)
			)

		override def flatMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :SQLForm.NullValue[X]) :NumberedColumn[X, O] =
			new NumberedColumn[X, O](number, name, schema.flatMapBuffs(this)(there))(
				if (nulls != null) form.biflatMap(there)(back) else form.biflatMapNull(there)(back)
			)


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[NumberedColumn[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case col :NumberedColumn[_, _] if canEqual(that) && col.canEqual(this) =>
				number == col.number && form == col.form && name == col.name && buffs == col.buffs
			case _ => false
		}

		override def hashCode :Int = super.hashCode * 31 + number.##
	}

}


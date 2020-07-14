package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, ConstantBuff, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, FlagBuffType, InsertAudit, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, Nullable, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate, QueryAudit, SelectAudit, UpdateAudit}
import net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{Label, LabeledColumn}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{AdapterFactoryMethods, ColumnAdapterFactoryMethods}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause
import net.noresttherein.oldsql.sql.FromClause.{TableCount, TableShift}
import net.noresttherein.oldsql.sql.MappingSQL.{FreeColumn, FreeComponent}






/** A `Mapping` representing a single SQL column of a table or select result.
  * It is special in that it is atomic, the smallest building block in the `Mapping` component hierarchy.
  * Columns are never assembled (their `assemble` method returns `None`) but must be present in `ComponentValues`
  * for the mapping to produce a value. Additionally, it can never have any components (or subcomponents),
  * but it is its own column to maintain the invariant that the `columns` property always returns the complete list
  * of columns comprising any mapping. As follows, the rest of the column lists are either empty or contain
  * this sole instance itself, depending on the presence of specific buffs. Unlike other mapping implementations,
  * which are adaptable by operations such as mapping and prefixing, transformation methods on columns
  * return a new instance, using the form (and, where suitable, the name and buffs) of the original as their basis.
  */
trait ColumnMapping[S, O] extends TypedMapping[S, O]
	with ColumnAdapterFactoryMethods[({ type A[X] = ColumnMapping[X, O] })#A, S, O]
{ column =>

	/** Returns `Some(this.name)`. */
	final override def sqlName = Some(name)

	/** The name of this column, as seen from the containing table if it is a table/view column.
	  * If this column represents a column in the SELECT clause of a select statement, it is the column name as
	  * returned by the result set for that statement. The names of columns selected in a subselect query are qualified
	  * with the table/view/subselect name/alias from the ''from'' clause and any aliases given in the SELECT clause are
	  * applied.
	  */
	def name :String



	override def assemble(pieces: Pieces): Option[S] = None

	override def optionally(pieces :Pieces) :Option[S] =
		if (isNullable)
			pieces.preset(this)
		else
			pieces.preset(this) match {
				case Some(null) =>
					throw new NullPointerException("Read a null value for a non-nullable column " + name + ". " +
					                               "Flag the column with Buff.Nullable to explicitly allow nulls.")
				case res => res //don't check buffs - we do it in the form for speed
			}

	override def nullValue :NullValue[S] = form.nulls

	protected def isNullable :Boolean = Nullable.enabled(buffs)



	override def apply[T](component :Component[T]) :Extract[T] =
		if (component == this)
			ColumnExtract.ident[S, O](this).asInstanceOf[Extract[T]]
		else
			throw new IllegalArgumentException(
				s"Mapping $component is not a subcomponent of column $this. The only subcomponent of a column is the column itself."
			)

	override def apply[T](column :Column[T]) :ColumnExtract[T] =
		if (column == this)
			ColumnExtract.ident[S, O](this).asInstanceOf[ColumnExtract[T]]
		else
			throw new IllegalArgumentException(
				s"Mapping $column is not a column of column $this. The only subcomponent of a column is the column itself."
			)



	override def columnExtracts :NaturalMap[Column, ColumnExtract] =
		NaturalMap.single[Column, ColumnExtract, S](this, ColumnExtract.ident(this))

	override def extracts :NaturalMap[Component, ColumnExtract] =
		NaturalMap.single[Component, ColumnExtract, S](this, ColumnExtract.ident(this))



	override def export[T](component :Component[T]) :Component[T] = component

	override def export[T](column :Column[T]) :Column[T] = column



	/** Returns `Unique.empty`. */
	final override def components :Unique[Component[_]] = Unique.empty
	/** Returns `Unique.empty`. */
	final override def subcomponents :Unique[Component[_]] = Unique.empty

	/** Returns `Unique(this)`. */
	override def columns :Unique[Column[S]] = Unique.single(this)

	override def selectable :Unique[Column[S]] = selfUnless(NoSelect)
	override def queryable :Unique[Column[S]] = selfUnless(NoQuery)
	override def updatable :Unique[Column[S]] = selfUnless(NoUpdate)
	override def autoUpdated :Unique[Column[S]] = selfIf(AutoUpdate)
 	override def insertable :Unique[Column[S]] = selfUnless(NoInsert)
	override def autoInserted :Unique[Column[S]] = selfIf(AutoInsert)

	/** An empty `Unique` if the given buff is ''disabled'' (not attached), or a singleton `Unique(this)` otherwise. */
	@inline protected final def selfUnless(buff :BuffType) :Unique[Column[S]] =
		if (buff.enabled(buffs)) Unique.empty else columns

	/** An empty `Unique` if the given buff is ''enabled'' (attached), or a singleton `Unique(this)` otherwise. */
	@inline protected final def selfIf(buff :BuffType) :Unique[Column[S]] =
		if (buff.disabled(buffs)) Unique.empty else columns



	/** A read-write form for this column's type which does not take into account any buffs attached to this column.
	  * Serves as the basis for `selectForm`, `queryForm`, `insertForm` and `updateForm` which, in most cases,
	  * should be used instead.
	  */
	def form :ColumnForm[S]

	/** `this.form` adapted to a `SQLReadForm` by incorporating behaviour modifications caused by applied buffs.
	  * This includes default values from buffs like `OptionalSelect`, transformations from `AuditBuff`s and similar
	  */
	override def selectForm :SQLReadForm[S] = ExtraSelect.test(buffs) match {
		case Some(ConstantBuff(x)) => SQLReadForm.const(x)
		case Some(buff) => SQLReadForm.eval(buff.value)
		case _ =>
			val audits = SelectAudit.Audit(buffs)
			val read = //we can't enforce not null here because of artificial nulls resulting from outer joins
				if (audits.isEmpty) form
				else form.map(audits.reduce(_ andThen _), form.nullValue)
			OptionalSelect.test(buffs) match {
				case Some(ConstantBuff(x)) => read orElse SQLReadForm.const(x)
				case Some(buff) => read orElse SQLReadForm.eval(buff.value)
				case _ => read
			}

	}

	/** `this.form` adapted to a `SQLWriteForm` by incorporating behaviour modifications caused by applied audit buffs
	  * and `ExtraQuery`. `OptionalQuery` buff is ignored at this step: when omitting this column from a query
	  * its form should not be used at all.
	  */
	override def queryForm :SQLWriteForm[S] = ExtraQuery.test(buffs) match {
		case Some(ConstantBuff(x)) => SQLWriteForm.const(x)(form)
		case Some(buff) => SQLWriteForm.eval(buff.value)(form)
		case _ =>
			val audits = QueryAudit.Audit(buffs)
			if (audits.isEmpty) form
			else form.unmap(audits.reduce(_ andThen _))
	}

	/** `this.form` adapted to a `SQLWriteForm` by incorporating behaviour modifications caused by applied audit buffs
	  * and `ExtraUpdate`. `OptionalUpdate` buff is ignored at this step: when omitting this column from an update
	  * its form should not be used at all.
	  */
	override def updateForm :SQLWriteForm[S] = ExtraUpdate.test(buffs) match {
		case Some(ConstantBuff(x)) => SQLWriteForm.const(x)(form)
		case Some(buff) => SQLWriteForm.eval(buff.value)(form)
		case _ =>
			val audits = UpdateAudit.Audit(buffs)
			if (audits.isEmpty) form
			else form.unmap(audits.reduce(_ andThen _))
	}

	/** `this.form` adapted to a `SQLWriteForm` by incorporating behaviour modifications caused by applied audit buffs
	  * and `ExtraInsert`. `OptionalInsert` buff is ignored at this step: when omitting this column from an insert
	  * its form should not be used at all.
	  */
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



	protected override def thisColumn :ColumnMapping[S, O] = this

	protected override def copy(name :String, buffs :Seq[Buff[S]]) :ColumnMapping[S, O] =
		new StandardColumn(name, buffs)(form)



	/** Attaches a label to this column, transforming it into a `LabeledColumn`.
	  * This does not change the name of the column. Created column uses the same buffs and form as `this`,
	  * but is otherwise an independent instance not retaining a direct reference to `this`.
	  */
	def labeled[N <: Label](label :Label) :LabeledColumn[N, S, O] =
		new StandardColumn[S, O](name, buffs)(form) with LabeledColumn[N, S, O]


	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnMapping[X, O] =
		new StandardColumn[X, O](name, schema.mapBuffs(this)(there, back))(
			schema.mapForm(form)(there, back)
		)



	override def toString :String = name + "[" + form + "]"

	override def debugString :String = buffs.mkString(toString + "(", ", ", ")")

	override def columnString :String = toString

}






object ColumnMapping {

	type ColumnOf[S] = ColumnMapping[S, _]

	type ColumnAt[O] = ColumnMapping[_, O]



	def apply[S :ColumnForm, O](name :String, buffs :Buff[S]*) :ColumnMapping[S, O] =
		new StandardColumn(name, buffs)

	def apply[N <: String with Singleton :ValueOf, S :ColumnForm, O](buffs :Buff[S]*) :LiteralColumn[N, S, O] =
		new LiteralColumn[N, S, O](buffs)

	def labeled[N <: String with Singleton, S :ColumnForm, O](label :N, buffs :Buff[S]*) :LiteralColumn[N, S, O] =
		new LiteralColumn[N, S, O](buffs)(new ValueOf(label), ColumnForm[S])





	implicit def columnSQLFormula[F <: FromClause, C <: ColumnMapping[_, _], M[A] <: ColumnMapping[S, A], S, N <: Numeral]
	             (column :C)(implicit conforms :Conforms[C, M[F], ColumnMapping[S, F]], offset :TableShift[F, M, N])
			:FreeColumn[F, M, S] =
		FreeColumn(column, offset.tables)






	/** Basic (but full) `ColumnMapping` implementation. The difference from
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn StandardColumn]] is that the latter
	  * precomputes many of its properties, storing them in `val`s. Usage of this class should be preferred when
	  * the column (or its containing mapping) is created on demand, rather than at application initialization.
	  */
	class BaseColumn[S, O](override val name :String, override val buffs :Seq[Buff[S]])
	                      (implicit override val form :ColumnForm[S])
		extends ColumnMapping[S, O]
	{
		override val isNullable :Boolean = super.isNullable
	}






	/** Defaults `ColumnMapping` implementation. Many of the properties are overriden as `val`s for efficiency. */
	class StandardColumn[S, O](override val name :String, override val buffs :Seq[Buff[S]])
	                          (implicit override val form :ColumnForm[S])
		extends StableColumn[S, O]






	/** A labeled `ColumnMapping` implementation. As an additional important constraint over
	  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping.LabeledColumn LabeledColumn]] is that the name of the
	  * column is the same as the label.
	  */
	class LiteralColumn[N <: String with Singleton, S, O](override val buffs :Seq[Buff[S]] = Nil)
	                                                     (implicit label :ValueOf[N], override val form :ColumnForm[S])
		extends LabeledColumn[N, S, O] with StableColumn[S, O]
	{
		def this(name :N, buffs :Seq[Buff[S]])(implicit form :ColumnForm[S]) =
			this(buffs)(new ValueOf(name), form)

		override val name :N = label.value


		override def withBuffs(buffs :Seq[Buff[S]]) :LiteralColumn[N, S, O] =
			new LiteralColumn[N, S, O](buffs)(new ValueOf(name), form)


		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :LiteralColumn[N, X, O] =
			new LiteralColumn[N, X, O](schema.mapBuffs(this)(there, back))(
				implicitly[ValueOf[N]], schema.mapForm(form)(there, back)
			)

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :LiteralColumn[N, X, O] =
			as(Extractor.req(there), Extractor.req(back))

		override def optMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X])
				:LiteralColumn[N, X, O] =
			as(Extractor(there), Extractor(back))

	}



	/** A convenience base column class declaring standard fields `name`, `buffs` and `form` ''without'' extending
	  * `ColumnMapping`. This allows the latter to be mixed in later, overriding declarations from any other traits
	  * (and classes) that the extending class might inherit.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.StableColumn]]
	  */
	abstract class ColumnSupport[S, O](val name :String, override val buffs :Seq[Buff[S]] = Nil)
	                                  (implicit val form :ColumnForm[S])
		extends TypedMapping[S, O]
	{ this :ColumnMapping[S, O] => }



	/** A late mix-in trait overriding all benefiting `ColumnMapping` methods with `val`s. Needs to be placed
	  * in the linearization order after declarations of `name`, `buffs` and `form`.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.ColumnSupport]]
	  */
	trait StableColumn[S, O] extends ColumnMapping[S, O] {
		final override val isNullable :Boolean = super.isNullable

		override def apply[T](component :Component[T]) :Extract[T] =
			if (component == this)
				selfExtract.asInstanceOf[Extract[T]]
			else
				throw new IllegalArgumentException(
					s"Mapping $component is not a subcomponent of column $this. The only subcomponent of a column is the column itself."
					)

		override def apply[T](column :Column[T]) :ColumnExtract[T] =
			if (column == this)
				selfExtract.asInstanceOf[ColumnExtract[T]]
			else
				throw new IllegalArgumentException(
					s"Mapping $column is not a column of column $this. The only subcomponent of a column is the column itself."
					)


		private[this] val selfExtract = ColumnExtract.ident(this)

		override val extracts :NaturalMap[Component, ColumnExtract] =
			NaturalMap.single[Component, ColumnExtract, S](this, selfExtract)

		override val columnExtracts :NaturalMap[Column, ColumnExtract] =
			NaturalMap.single[Column, ColumnExtract, S](this, selfExtract)


		final override val columns :Unique[Column[S]] = Unique.single(this)

		final override val selectable :Unique[Column[S]] = selfUnless(NoSelect)
		final override val queryable :Unique[Column[S]] = selfUnless(NoQuery)
		final override val updatable :Unique[Column[S]] = selfUnless(NoUpdate)
		final override val autoUpdated :Unique[Column[S]] = selfIf(AutoUpdate)
		final override val insertable :Unique[Column[S]] = selfUnless(NoInsert)
		final override val autoInserted :Unique[Column[S]] = selfIf(AutoInsert)

		final override val selectForm :SQLReadForm[S] = super.selectForm
		final override val queryForm :SQLWriteForm[S] = super.queryForm
		final override val updateForm :SQLWriteForm[S] = super.updateForm
		final override val insertForm :SQLWriteForm[S] = super.insertForm

	}
}


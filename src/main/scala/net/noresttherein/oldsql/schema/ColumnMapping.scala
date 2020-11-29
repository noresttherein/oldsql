package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, ConstantBuff, ExtraSelect, FilterAudit, InsertAudit, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, Nullable, OptionalSelect, SelectAudit, UpdateAudit}
import net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{Label, LabeledColumn}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.ComponentValues.{ColumnValues, ComponentValuesBuilder}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.support.ColumnMappingFactoryMethods
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct}
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn
import net.noresttherein.oldsql.sql.mechanics.TableCount






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
trait ColumnMapping[S, O] extends BaseMapping[S, O]
	with ColumnMappingFactoryMethods[({ type A[X] = ColumnMapping[X, O] })#A, S, O]
{ column =>

	/** The name of this column, as seen from the containing table if it is a table/view column.
	  * If this column represents a column in the SELECT clause of a select statement, it is the column name as
	  * returned by the result set for that statement. The names of columns selected in a subselect query are qualified
	  * with the table/view/subselect name/alias from the ''from'' clause and any aliases given in the SELECT clause are
	  * applied.
	  */
	def name :String



	override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] =
		if (op.columns(this).nonEmpty)
			ColumnValues.preset(this, (subject /: op.audit.Audit(this)) { (s, f) => f(s) })
		else
	        ColumnValues.preset(this, op.extra.Value(this))

	override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		if (op.columns(this).nonEmpty) { //this check is the fastest with columns caching the individual lists.
			val audited = (subject /: op.audit.Audit(this)) { (s, f) => f(s) }
			collector.add(this, audited)
		} else
			ColumnValues.preset(this, op.extra.Value(this))

	override def filterValues(subject :S) :ComponentValues[S, O] = writtenValues(FILTER, subject)
	override def updateValues(subject :S) :ComponentValues[S, O] = writtenValues(UPDATE, subject)
	override def insertValues(subject :S) :ComponentValues[S, O] = writtenValues(INSERT, subject)



	override def assemble(pieces: Pieces): Option[S] = None

	/** This method is overriden from the default implementation and does not take into account any select-related
	  * buffs, but instead directly asks the `pieces` for a preset value (as none could possibly be assembled, columns
	  * being the bottom components). This is done both for efficiency, by moving the behaviour to the `selectForm`,
	  * and because this method may be called also by `ComponentValues` instances presetting values for update operations,
	  * not only when assembling the result of an SQL select from the row data. For this reason it should not be
	  * overriden; any modification of the values such as validation should happen through `Buff` instances attached
	  * to this instance or, if impossible to implement, by introducing a component class wrapping the column.
	  * @return `pieces.preset(this)`
	  * @throws `NullPointerException` if the preset value is explicitly stated as `null` (`pieces.preset(this)`
	  *                                returned `Some(null)`), but this column does not have the `Nullable` buff.
	  */
	override def optionally(pieces :Pieces) :Option[S] =
		if (isNullable)
			pieces.preset(this)
		else //consider: making the method final, perhaps only calling pieces.preset, so we can make some optimizations
			pieces.preset(this) match {
				case Some(null) =>
					throw new NullPointerException("Read a null value for a non-nullable column " + name + ". " +
					                               "Flag the column with Buff.Nullable to explicitly allow nulls.")
				case res => res //don't check buffs - we do it in the form for speed
			}

	/** @inheritdoc
	  * @return the `NullValue` instance provided by the associated form. */
	override def nullValue :NullValue[S] = form.nulls

	/** True if `null` is a valid value for this column. Unless the `Nullable` buff is attached,
	  * `optionally` will deliberately throw a `NullPointerException` if a `null` value is preset in
	  * the [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]] argument (typically as the result of
	  * this column's form returning `Some(null)`).
	  * @return `Nullable.enabled(buffs)`.
	  */
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
	final override def components :Unique[Nothing] = Unique.empty
	/** Returns `Unique.empty`. */
	final override def subcomponents :Unique[Nothing] = Unique.empty

	/** Returns `Unique(this)`. */
	override def columns :Unique[Column[S]] = Unique.single(this)

	override def selectable :Unique[Column[S]] = selfUnless(NoSelect)
	override def filterable :Unique[Column[S]] = selfUnless(NoFilter)
	override def updatable :Unique[Column[S]] = selfUnless(NoUpdate)
	override def autoUpdated :Unique[Column[S]] = selfIf(AutoUpdate)
 	override def insertable :Unique[Column[S]] = selfUnless(NoInsert)
	override def autoInserted :Unique[Column[S]] = selfIf(AutoInsert)
	override def selectedByDefault :Unique[Column[S]] = selfUnless(NoSelectByDefault)
	override def filteredByDefault :Unique[Column[S]] = selfUnless(NoFilterByDefault)
	override def updatedByDefault :Unique[Column[S]] = selfUnless(NoUpdateByDefault)
	override def insertedByDefault :Unique[Column[S]] = selfUnless(NoInsertByDefault)

	/** An empty `Unique` if the given buff is ''disabled'' (not attached), or a singleton `Unique(this)` otherwise. */
	@inline protected final def selfUnless(buff :BuffType) :Unique[Column[S]] =
		if (buff.enabled(buffs)) Unique.empty else columns

	/** An empty `Unique` if the given buff is ''enabled'' (attached), or a singleton `Unique(this)` otherwise. */
	@inline protected final def selfIf(buff :BuffType) :Unique[Column[S]] =
		if (buff.disabled(buffs)) Unique.empty else columns



	/** A read-write form for this column's type which does not take into account any buffs attached to this column.
	  * Serves as the basis for `selectForm`, `filterForm`, `insertForm` and `updateForm` which, in most cases,
	  * should be used instead.
	  */
	def form :ColumnForm[S]

	/** `this.form` adapted to a `SQLReadForm` by incorporating some of behaviour modifications caused by applied buffs.
	  * This includes default values from buffs like `OptionalSelect` and transformations from `AuditBuff`s and similar.
	  * It doesn't verify if the column is selectable by default or at all, returning always (possibly decorated)
	  * `this.form`, unless [[net.noresttherein.oldsql.schema.Buff.ExtraSelect ExtraSelect]] is present, in which case
	  * a constant `SQLReadForm` is returned, which does not read anything from the `ResultSet`. In all other cases,
	  * the returned form is a [[net.noresttherein.oldsql.schema.ColumnReadForm ColumnReadForm]].
	  * Note that the `optionally`/`apply` and `assembly` methods of this mapping are ''not'' called by the returned
	  * form. They are involved only as a part of the assembly process for owning components, provided
	  * by the created `ComponentValues` containing the value returned by this form instead.
	  */
	override def selectForm :SQLReadForm[S] = ExtraSelect.test(buffs) match {
		//these *could* be column forms, but likely we'd rather have it zero width, as there is no such column in the db.
		case Some(ConstantBuff(x)) => SQLReadForm.const(x, 0, name + "='" + x + "'>")
		case Some(buff) => SQLReadForm.eval(buff.value, 0, name + "=_>")
		case _ =>
			val audits = SelectAudit.Audit(buffs)
			val read = //we can't enforce not null here because of artificial nulls resulting from outer joins
				if (audits.isEmpty) form
				else form.map(audits.reduce(_ andThen _))(form.nulls)
			OptionalSelect.test(buffs) match {
				case Some(ConstantBuff(x)) => read orElse ColumnReadForm.const(read.sqlType, x, name + "='" + x + "'>")
				case Some(buff) => read orElse ColumnReadForm.eval(read.sqlType, buff.value, name + "=_>")
				case _ => read
			}

	}

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		if (components.size == 1 && components.head == this) selectForm
		else if (components.isEmpty) SQLReadForm.empty
		else throw new NoSuchElementException("Mappings " + components + " are not components of column " + this)


	/** Adapts `this.form` by incorporating the behaviour of relevant buffs: the `ExtraXxx` and `XxxAudit`.
	  * Only standard buffs modifying/providing the written value are applied; any buffs determining if the column
	  * can or should be included in the given operation are ignored. The caller should use an explicit column list
	  * rather than rely on this form to handle the case of an excluded column.
	  * Note that `OptionalXxx` and even `NoXxx` buffs are ignored here and columns need to be explicitly
	  * included/excluded in an operation and the burden of validating this information lies with the owning mapping.
	  */
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.extra.test(buffs) match {
		case Some(ConstantBuff(x)) => SQLWriteForm.const(x)(form)
		case Some(buff) => SQLWriteForm.eval(buff.value)(form)
		case _ =>
			val audits = op.audit.Audit(buffs)
			if (audits.isEmpty) form
			else form.unmap(audits.reduce(_ andThen _))
	}

	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components.size == 1 && components.contains(this)) op.form(this)
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
		new StandardColumn[X, O](name, schema.mapBuffs(this)(there, back))(form.as(there)(back))



	override def toString :String = name + "[" + form + "]"

	override def mappingName :String = name

	override def debugString :String = buffs.mkString(toString + "(", ", ", ")")

	override def columnString :String = toString

}






sealed abstract class LowPriorityColumnMappingImplicits {
	//exists for use as the right side of SQLExpression.=== and similar, which will instantiate type F before applying conversion
	implicit def columnSQL[F <: RowProduct, C <: ColumnMapping[_, _], S, O <: RowProduct]
	                      (column :C)
	                      (implicit subject :C <:< ColumnMapping[S, O], origin :F <:< O,
	                                offset :TableCount[O, _ <: Numeral],
	                                projection :OriginProjection[C, S] { type WithOrigin[A] <: ColumnMapping[S, A] })
			:ColumnSQL[F, GlobalScope, S] =
		LooseColumn(projection[F](column), offset.offset)

}






object ColumnMapping extends LowPriorityColumnMappingImplicits {

	type ColumnOf[S] = ColumnMapping[S, _]

	type ColumnAt[O] = ColumnMapping[_, O]



	def apply[S :ColumnForm, O](name :String, buffs :Buff[S]*) :ColumnMapping[S, O] =
		new StandardColumn(name, buffs)

	def apply[N <: String with Singleton :ValueOf, S :ColumnForm, O](buffs :Buff[S]*) :LiteralColumn[N, S, O] =
		new LiteralColumn[N, S, O](buffs)

	def labeled[N <: String with Singleton, S :ColumnForm, O](label :N, buffs :Buff[S]*) :LiteralColumn[N, S, O] =
		new LiteralColumn[N, S, O](buffs)(new ValueOf(label), ColumnForm[S])



	implicit def columnComponentSQL[F <: RowProduct, C <: ColumnMapping[_, _], S]
	                               (column :C)
	                               (implicit subject :C <:< ColumnMapping[S, F], offset :TableCount[F, _ <: Numeral],
	                                projection :OriginProjection[C, S] { type WithOrigin[O] <: ColumnMapping[S, O] })
			:LooseColumn[F, projection.WithOrigin, S] =
		LooseColumn(column)






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
				implicitly[ValueOf[N]], form.as(there)(back)
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
		extends BaseMapping[S, O]
	{ this :ColumnMapping[S, O] => }



	/** A late mix-in trait overriding all benefiting `ColumnMapping` methods with `val`s. Needs to be placed
	  * in the linearization order after declarations of `name`, `buffs` and `form`.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.ColumnSupport]]
	  */
	trait StableColumn[S, O] extends ColumnMapping[S, O] {
		final override val isNullable :Boolean = super.isNullable

		private[this] val isFilterable = NoFilter.disabled(this)
		private[this] val isUpdatable = NoUpdate.disabled(this)
		private[this] val isInsertable = NoInsert.disabled(this)

		private[this] val queryAudit = FilterAudit.fold(this)
		private[this] val updateAudit = UpdateAudit.fold(this)
		private[this] val insertAudit = InsertAudit.fold(this)

		override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] =
			op.writtenValues(this, subject)

		override def filterValues(subject :S) :ComponentValues[S, O] =
			if (isFilterable) ColumnValues.preset(this, queryAudit(subject))
			else ColumnValues.empty

		override def updateValues(subject :S) :ComponentValues[S, O] =
			if (isUpdatable) ColumnValues.preset(this, updateAudit(subject))
			else ColumnValues.empty

		override def insertValues(subject :S) :ComponentValues[S, O] =
			if (isInsertable) ColumnValues.preset(this, insertAudit(subject))
			else ColumnValues.empty


		override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			op.writtenValues(this, subject, collector)

		override def filterValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isFilterable) collector.add(this, queryAudit(subject))

		override def updateValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isUpdatable) collector.add(this, updateAudit(subject))

		override def insertValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isInsertable) collector.add(this, insertAudit(subject))



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
			extracts.asInstanceOf[NaturalMap[Column, ColumnExtract]]


		override val columns :Unique[Column[S]] = Unique.single(this)

		override val selectable :Unique[Column[S]] = super.selectable
		override val filterable :Unique[Column[S]] = super.filterable
		override val updatable :Unique[Column[S]] = super.updatable
		override val autoUpdated :Unique[Column[S]] = super.autoUpdated
		override val insertable :Unique[Column[S]] = super.insertable
		override val autoInserted :Unique[Column[S]] = super.autoInserted
		override val selectedByDefault :Unique[Column[S]] = super.selectedByDefault
		override val filteredByDefault :Unique[Column[S]] = super.filteredByDefault
		override val updatedByDefault :Unique[Column[S]] = super.updatedByDefault
		override val insertedByDefault :Unique[Column[S]] = super.insertedByDefault

		override val selectForm :SQLReadForm[S] = super.selectForm
		override val filterForm :SQLWriteForm[S] = super.filterForm
		override val updateForm :SQLWriteForm[S] = super.updateForm
		override val insertForm :SQLWriteForm[S] = super.insertForm
		override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.form(this)

	}


}


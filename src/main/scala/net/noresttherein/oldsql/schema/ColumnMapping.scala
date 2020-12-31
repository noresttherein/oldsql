package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.haul.ColumnValues
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, ConstantBuff, ExtraFilter, ExtraInsert, ExtraSelect, ExtraUpdate, FilterAudit, InsertAudit, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, Nullable, OptionalSelect, SelectAudit, SelectDefault, UpdateAudit, ValueBuffType}
import net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{Label, LabeledColumn}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.Mapping.OriginProjection
import net.noresttherein.oldsql.schema.bases.{BaseMapping, ExportMapping}
import net.noresttherein.oldsql.schema.support.{ColumnMappingFactoryMethods, MappedMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.ExportColumnProxy
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct}
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn
import net.noresttherein.oldsql.sql.mechanics.TableCount






/** A `Mapping` representing a single SQL column of a table or select result. They behave in principle just like
  * all other [[net.noresttherein.oldsql.schema.Mapping Mapping]] implementations, but are the bottom components
  * in the whole mapping hierarchy, as they typically contain no other subcomponents and can always be used
  * in ''select'' clauses of an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]]. Their value is not in general
  * assembled like the subjects of other mappings, but read by their
  * [[net.noresttherein.oldsql.schema.ColumnMapping.form form]] and preset in
  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] for every `ResultSet` row.
  * If their [[net.noresttherein.oldsql.schema.ColumnMapping.components components]]
  * and [[net.noresttherein.oldsql.schema.ColumnMapping.subcomponents subcomponents]] lists are empty,
  * they must always contain themselves in their [[net.noresttherein.oldsql.schema.ColumnMapping.columns columns]] list
  * (and other column lists, depending on attached buffs). Most columns extend
  * from [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn SimpleColumn]] or its subclass,
  * [[net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn StandardColumn]], having an empty
  * [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn.assemble assemble]] method, rather than this trait
  * directly. Relying solely on their `form`, they are often treated as value types: various transformation methods
  * which for other mappings would create adapters serving as proxies to the original mapping, simply create
  * a new column instance, with possibly modified name/buffs/form. Otherwise, if the column is an adapter
  * to another column, their [[net.noresttherein.oldsql.schema.ColumnMapping.columns columns]] list (and its specific
  * variants) must contain the single export version of the actual bottom column, reported recursively
  * by the adapted column. The presence of that bottom column on `components` and `subcomponents` is optional,
  * but no other column should be included. As always, however,
  * [[net.noresttherein.oldsql.schema.ColumnMapping.columnExtracts columnExtracts]] should contain extracts
  * for all intermediate columns as well.
  */
trait ColumnMapping[S, O]
	extends BaseMapping[S, O] with ColumnMappingFactoryMethods[({ type A[X] = ColumnMapping[X, O] })#A, S, O]
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
		} else op.extra.Value(this) match {
			case res if res.isDefined => collector.add(this, res)
			case _ =>
		}

	override def filterValues(subject :S) :ComponentValues[S, O] = writtenValues(FILTER, subject)
	override def updateValues(subject :S) :ComponentValues[S, O] = writtenValues(UPDATE, subject)
	override def insertValues(subject :S) :ComponentValues[S, O] = writtenValues(INSERT, subject)



	/** Columns are not typically assembled from subcomponents; this method by default returns `None`
	  * and there should be little need to override it.
	  */
	override def assemble(pieces: Pieces): Option[S] = None

	/** Attempts to retrieve the value for the mapped `Subject` from the given `ComponentValues`. The behaviour for
	  * columns is roughly equivalent to that of components, as implemented by default in
	  * [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]. The difference is that columns without
	  * [[net.noresttherein.oldsql.schema.Buff.Nullable Nullable]] buff will never return `Some(null)`,
	  * but throw a `NullPointerException` with an appropriate message. As with other mappings, this behaviour
	  * can however be overriden. As in the overriden implementation,
	  * any [[net.noresttherein.oldsql.schema.Buff.SelectAudit SelectAudit]] buffs are applied, in order,
	  * to the returned value (but only if it comes from `pieces`, not other buffs).  Because preset values typically
	  * come directly from this column's [[net.noresttherein.oldsql.schema.ColumnMapping.selectForm selectForm]],
	  * implementations may opt to move some of the buff handling to that method or the form itself.
	  * @throws `NullPointerException` if the preset value is explicitly stated as `null` (`pieces.preset(this)`
	  *                                returned `Some(null)`), but this column does not have the `Nullable` buff.
	  */
	override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this) match {
		case res @ Some(x) => //a very common case
			if (x == null && !isNullable)
				throw new NullPointerException("Read a null value for a non-nullable column " + name + ". " +
				                               "Flag the column with Buff.Nullable to explicitly allow nulls.")
			else if (buffs.isEmpty)
				res
			else selectAudit match {
				case Some(audit) => Some(audit(x))
				case _ => res
			}
		case _ => defaultValue
	}

	/** Functions from all [[net.noresttherein.oldsql.schema.Buff.SelectAudit SelectAudit]] buffs
	  * composed with each other. It is applied to every value returned by the form or
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.assemble assemble]] method.
	  */
	protected def selectAudit :Option[S => S] = {
		val audits = SelectAudit.Audit(this)
		if (audits.isEmpty) None
		else Some(audits.reduce(_ andThen _))
	}

	/** The value returned by [[net.noresttherein.oldsql.schema.ColumnMapping.optionally optionally]]
	  * if no actual value is present in [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]],
	  * nor can one be assembled from subcolumns.
	  * @return [[net.noresttherein.oldsql.schema.Buff.SelectDefault SelectDefault]]`.`[[net.noresttherein.oldsql.schema.Buff.ValueBuffType.Value Value]]`(this)`
	  *         `orElse` [[net.noresttherein.oldsql.schema.Buff.ExtraSelect ExtraSelect]]`.Value(this)`.
	  */
	protected def defaultValue :Option[S] = {
		val res = SelectDefault.Value(this)
		if (res.isDefined) res else ExtraSelect.Value(this)
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



	override def columnExtracts :NaturalMap[Column, ColumnExtract] =
		NaturalMap.single[Column, ColumnExtract, S](this, ColumnExtract.ident(this))

	override def extracts :NaturalMap[Component, ColumnExtract] =
		NaturalMap.single[Component, ColumnExtract, S](this, ColumnExtract.ident(this))


	/** Returns `Unique.empty`. */
	override def components :Unique[Column[_]] = Unique.empty
	/** Returns `Unique.empty`. */
	override def subcomponents :Unique[Column[_]] = Unique.empty

	/** Returns `Unique(this)`. */
	override def columns :Unique[Column[_]] = Unique.single(this)

	override def selectable :Unique[Column[_]] = selfUnless(NoSelect)
	override def filterable :Unique[Column[_]] = selfUnless(NoFilter)
	override def updatable :Unique[Column[_]] = selfUnless(NoUpdate)
	override def autoUpdated :Unique[Column[_]] = selfIf(AutoUpdate)
 	override def insertable :Unique[Column[_]] = selfUnless(NoInsert)
	override def autoInserted :Unique[Column[_]] = selfIf(AutoInsert)
	override def selectedByDefault :Unique[Column[_]] = selfUnless(NoSelectByDefault)
	override def filteredByDefault :Unique[Column[_]] = selfUnless(NoFilterByDefault)
	override def updatedByDefault :Unique[Column[_]] = selfUnless(NoUpdateByDefault)
	override def insertedByDefault :Unique[Column[_]] = selfUnless(NoInsertByDefault)

	/** An empty `Unique` if the given buff is ''disabled'' (not attached), or a singleton `Unique(this)` otherwise. */
	@inline protected final def selfUnless(buff :BuffType) :Unique[Column[_]] =
		if (buff.enabled(buffs)) Unique.empty else columns

	/** An empty `Unique` if the given buff is ''enabled'' (attached), or a singleton `Unique(this)` otherwise. */
	@inline protected final def selfIf(buff :BuffType) :Unique[Column[_]] =
		if (buff.disabled(buffs)) Unique.empty else columns



	/** A read-write form for this column's type which does not take into account any buffs attached to this column.
	  * Serves as the basis for `selectForm`, `filterForm`, `insertForm` and `updateForm` which, in most cases,
	  * should be used instead.
	  */
	def form :ColumnForm[S]

	/** Default form for this column to use in SQL ''selects''. Unlike other mappings, which implement their
	  * read forms by delegating to their assembly methods, the dependency is in a way reversed for columns.
	  * These forms do not rely on the [[net.noresttherein.oldsql.schema.ColumnMapping.optionally optionally]]
	  * or [[net.noresttherein.oldsql.schema.ColumnMapping.assemble assemble]], as most columns cannot be assembled.
	  * Instead, this form is used by enclosing mappings and framework to read and preset a value for this column
	  * in [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]] used to assemble a `ResultSet` row.
	  * Subclasses are free to move some buff handling from `optionally` to the form or this method in process
	  * of optimisation. This method will always however ignore any buffs controlling if a column should be read
	  * (included in the ''select'' clause) and the returned form must always attempt to read the value
	  * for this column from the `ResultSet` at the given position. It is the enclosing mapping's responsibility
	  * to pick the columns which will be read in a query.
	  * @return `this.`[[net.noresttherein.oldsql.schema.ColumnMapping.form form]].
	  */
	override def selectForm :SQLReadForm[S] = form

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		if (components.size == 1 && components.head == this) selectForm
		else if (components.isEmpty) SQLReadForm.empty
		else throw new NoSuchElementException("Mappings " + components + " are not components of column " + this)


	/** Adapts `this.form` by incorporating the behaviour of relevant buffs: the `ExtraXxx` and `XxxAudit`.
	  * In order to stay consistent in custom `ColumnMapping` implementations, the adapted form first calls
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.writtenValues this.writtenValues]] and,
	  * if created [[ComponentValues ComponentValues]] are not empty,
	  * passes the value for this column to [[net.noresttherein.oldsql.schema.ColumnMapping.form this.form]].
	  */
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = form

	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components.size == 1 && components.contains(this)) op.form(this)
		else if (components.isEmpty) SQLWriteForm.none(form)
		else throw new NoSuchElementException("Mappings " + components + " are not components of column " + this)



	protected override def thisColumn :ColumnMapping[S, O] = this

	protected override def copy(name :String, buffs :Seq[Buff[S]]) :ColumnMapping[S, O] =
		new ExportColumnProxy[S, O](this, name, buffs)


	/** Attaches a label to this column, transforming it into a `LabeledColumn`.
	  * This does not change the name of the column. Created column uses the same buffs and form as `this`,
	  * but is otherwise an independent instance not retaining a direct reference to `this`.
	  */
	def labeled[N <: Label](label :Label) :LabeledColumn[N, S, O] =
		new ExportColumnProxy[S, O](this, name, buffs) with LabeledColumn[N, S, O]


	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnMapping[X, O] =
		MappedMapping.column(this, there, back)



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

	/** A curried definition of [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[S, O]`,
	  * containing a single type constructor `P[O] = ColumnMapping[S, O]`. It allows the use of `ColumnMapping`
	  * as a type parameters to classes/methods which require the definition of a mapping accepting
	  * its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
	  */
	type Of[S] = { type P[O] = ColumnMapping[S, O] }



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
	  * the column needs to override default `optionally`/`apply` definitions,
	  * when inherited/overriden implementations of some methods would conflict with those from
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.StableColumn StableColumn]], or when it is created on demand,
	  * rather than at application initialization,.
	  */
	class BaseColumn[S, O](override val name :String, override val buffs :Seq[Buff[S]])
	                      (implicit override val form :ColumnForm[S])
		extends ColumnMapping[S, O]
	{
		override val isNullable :Boolean = super.isNullable
	}






	/** A column implementation which is completely driven by its
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.form form]]: its
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn.optionally optionally]] and
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn.apply apply]] methods are made final
	  * and simple check if there is a preset value for this column. All logic of handling standard
	  * [[net.noresttherein.oldsql.schema.Buff buffs]] is instead implemented in
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn.selectForm selectForm]] and
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn.writeForm writeForm]] or the returned forms
	  * themselves.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn]]
	  */
	trait SimpleColumn[S, O] extends ColumnMapping[S, O] with ExportMapping {

		final override def apply(pieces: Pieces): S = optionally(pieces) match {
			case Some(res) => res
			case _ =>
				throw new NullPointerException("Read a null value for a non-nullable column " + name + ". " +
				                               "Flag the column with Buff.Nullable to explicitly allow nulls.")
		}

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


		/** `this.form` adapted to an `SQLReadForm` by incorporating some of behaviour modifications caused by applied buffs.
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
				SelectDefault.test(buffs) match {
					case Some(ConstantBuff(x)) => read orElse ColumnReadForm.const(read.sqlType, x, name + "='" + x + "'>")
					case Some(buff) => read orElse ColumnReadForm.eval(read.sqlType, buff.value, name + "=_>")
					case _ => read
				}

		}

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

		/** Adapts `this.form` by incorporating the behaviour of relevant buffs: the `ExtraXxx` and `XxxAudit`.
		  * Only standard buffs modifying/providing the written value are applied; any buffs determining if the column
		  * can or should be included in the given operation are ignored. The caller should use an explicit column list
		  * rather than rely on this form to handle the case of an excluded column.
		  * Note that `OptionalXxx` and even `NoXxx` buffs are ignored here and columns need to be explicitly
		  * included/excluded in an operation. The burden of validating this information lies with the owning mapping.
		  */
		override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.extra.test(buffs) match {
			case Some(ConstantBuff(x)) => SQLWriteForm.const(x)(form)
			case Some(buff) => SQLWriteForm.eval(buff.value)(form)
			case _ =>
				val audits = op.audit.Audit(buffs)
				if (audits.isEmpty) form
				else form.unmap(audits.reduce(_ andThen _))
		}


		protected override def copy(name :String, buffs :Seq[Buff[S]]) :ColumnMapping[S, O] =
			new StandardColumn[S, O](name, buffs)(form)

		override def labeled[N <: Label](label :Label) :LabeledColumn[N, S, O] =
			new StandardColumn[S, O](name, buffs)(form) with LabeledColumn[N, S, O]

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnMapping[X, O] =
			new StandardColumn[X, O](name, mapBuffs(this)(there, back))(form.as(there)(back))
	}



	/** Defaults `ColumnMapping` implementation. Many of the properties are overriden as `val`s for efficiency. */
	class StandardColumn[S, O](override val name :String, override val buffs :Seq[Buff[S]])
	                          (implicit override val form :ColumnForm[S])
		extends SimpleColumn[S, O] with StableColumn[S, O]






	/** A labeled `ColumnMapping` implementation. As an additional important constraint over
	  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping.LabeledColumn LabeledColumn]] is that the name of the
	  * column is the same as the label.
	  */
	class LiteralColumn[N <: String with Singleton, S, O](override val buffs :Seq[Buff[S]] = Nil)
	                                                     (implicit label :ValueOf[N], override val form :ColumnForm[S])
		extends LabeledColumn[N, S, O] with SimpleColumn[S, O] with StableColumn[S, O]
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



	/** A mix-in trait providing optimized `writtenValues` family of methods by caching buff information in fields,
	  * as well as fields for [[net.noresttherein.oldsql.schema.ColumnMapping.isNullable isNullable]]
	  * and all column lists, initialized by calls to `super`.
	  */
	trait OptimizedColumn[S, O] extends ColumnMapping[S, O] {
		final override val isNullable :Boolean = super.isNullable

		protected val isFilterable = NoFilter.disabled(this)
		protected val isUpdatable = NoUpdate.disabled(this)
		protected val isInsertable = NoInsert.disabled(this)

		protected val filterAudit = FilterAudit.fold(this)
		protected val updateAudit = UpdateAudit.fold(this)
		protected val insertAudit = InsertAudit.fold(this)

		protected val extraFilter = ExtraFilter.Value(this)
		protected val extraUpdate = ExtraUpdate.Value(this)
		protected val extraInsert = ExtraInsert.Value(this)

		protected val extraFilterValues = extraWrite(extraFilter)
		protected val extraUpdateValues = extraWrite(extraUpdate)
		protected val extraInsertValues = extraWrite(extraInsert)

		protected def extraWrite(extra :Option[S]) = extra match {
			case Some(value) => ColumnValues.preset[S, S, O](this, value)
			case _ => ColumnValues.empty[S, O]
		}

		protected override val selectAudit = super.selectAudit
		protected override val defaultValue = super.defaultValue

		override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] =
			op.writtenValues(this, subject)

		override def filterValues(subject :S) :ComponentValues[S, O] =
			if (isFilterable) ColumnValues.preset(this, filterAudit(subject))
			else extraFilterValues.asInstanceOf[ColumnValues[S, O]]

		override def updateValues(subject :S) :ComponentValues[S, O] =
			if (isUpdatable) ColumnValues.preset(this, updateAudit(subject))
			else extraUpdateValues.asInstanceOf[ColumnValues[S, O]]

		override def insertValues(subject :S) :ComponentValues[S, O] =
			if (isInsertable) ColumnValues.preset(this, insertAudit(subject))
			else extraInsertValues.asInstanceOf[ColumnValues[S, O]]


		override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			op.writtenValues(this, subject, collector)

		override def filterValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isFilterable) collector.add(this, filterAudit(subject))
			else collector.add(this, extraFilter)

		override def updateValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isUpdatable) collector.add(this, updateAudit(subject))
			else collector.add(this, extraUpdate)

		override def insertValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isInsertable) collector.add(this, insertAudit(subject))
			else collector.add(this, extraInsert)


		protected val defaultColumns = Unique.single(this)
		protected val superSelectable = super.selectable
		protected val superFilterable = super.filterable
		protected val superUpdatable = super.updatable
		protected val superAutoUpdated = super.autoUpdated
		protected val superinsertable = super.insertable
		protected val superAutoInserted = super.autoInserted
		protected val superSelectedByDefault = super.selectedByDefault
		protected val superFilteredByDefault = super.filteredByDefault
		protected val superUpdatedByDefault = super.updatedByDefault
		protected val superInsertedByDefault = super.insertedByDefault

		override def columns :Unique[Column[_]] = defaultColumns
		override def selectable :Unique[Column[_]] = superSelectable
		override def filterable :Unique[Column[_]] = superFilterable
		override def updatable :Unique[Column[_]] = superUpdatable
		override def autoUpdated :Unique[Column[_]] = superAutoUpdated
		override def insertable :Unique[Column[_]] = superinsertable
		override def autoInserted :Unique[Column[_]] = superAutoInserted
		override def selectedByDefault :Unique[Column[_]] = superSelectedByDefault
		override def filteredByDefault :Unique[Column[_]] = superFilteredByDefault
		override def updatedByDefault :Unique[Column[_]] = superUpdatedByDefault
		override def insertedByDefault :Unique[Column[_]] = superInsertedByDefault
	}



	/** A late mix-in trait overriding all benefiting `ColumnMapping` methods with `val`s. Needs to be placed
	  * in the linearization order after declarations of `name`, `buffs` and `form`.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.ColumnSupport]]
	  */
	trait StableColumn[S, O] extends OptimizedColumn[S, O] {

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

		protected val selfExtract = ColumnExtract.ident(this)

		private val defaultExtracts = NaturalMap.single[Column, ColumnExtract, S](this, selfExtract)

		override def extracts :NaturalMap[Component, ColumnExtract] =
			defaultExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]

		override def columnExtracts :NaturalMap[Column, ColumnExtract] = defaultExtracts


		protected val superSelectForm = super.selectForm
		protected val superFilterForm = super.writeForm(FILTER)
		protected val superUpdateForm = super.writeForm(UPDATE)
		protected val superInsertForm = super.writeForm(INSERT)

		override def selectForm :SQLReadForm[S] = superSelectForm
		override def filterForm :SQLWriteForm[S] = superFilterForm
		override def updateForm :SQLWriteForm[S] = superUpdateForm
		override def insertForm :SQLWriteForm[S] = superInsertForm
		override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.form(this)

	}


}


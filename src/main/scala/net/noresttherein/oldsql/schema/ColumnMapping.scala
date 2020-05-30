package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, ConstantBuff, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, FlagBuffType, InsertAudit, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, Nullable, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate, QueryAudit, SelectAudit, UpdateAudit}
import net.noresttherein.oldsql.schema.ColumnMapping.{NumberedColumn, StandardColumn}
import net.noresttherein.oldsql.schema.Mapping.{OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{Label, LabeledColumn}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause
import net.noresttherein.oldsql.sql.FromClause.{TableCount, TableShift}
import net.noresttherein.oldsql.sql.MappingFormula.{FreeColumn, FreeComponent}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula






/** A `Mapping` representing a single SQL column of a table or select result.
  * It is special in that it is atomic, the smallest building block in the `Mapping` component hierarchy.
  * Columns are never assembled (their `assemble` method returns `None`) but must be present in `ComponentValues`
  * for the mapping to produce a value. Additionally, it can never have any components (or subcomponents),
  * but it is its own column to maintain the invariant that the `columns` property always returns the complete list
  * of columns comprising any mapping. As follows, the rest of the column lists are either empty or contain
  * this sole instance itself, depending on the presence of specific buffs. Unlike other mapping implementations,
  * which are adaptable by operations such as mapping and prefixing, transformation methods on columns
  * return a new instance, using the form (and, where suitable, buffs and name) of the original as their basis.
  */
trait ColumnMapping[S, O] extends TypedMapping[S, O] { column =>

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



	override def apply[T](component :Component[T]) :Extract[T] =
		if (component == this)
			MappingExtract.ident[S, O](this).asInstanceOf[Extract[T]]
		else
			throw new IllegalArgumentException(
				s"Mapping $component is not a subcomponent of column $this. The only subcomponent of a column is the column itself."
			)

	override def apply[T](column :Column[T]) :ColumnExtract[T] =
		if (column == this)
			MappingExtract.ident[S, O](this).asInstanceOf[ColumnExtract[T]]
		else
			throw new IllegalArgumentException(
				s"Mapping $column is not a column of column $this. The only subcomponent of a column is the column itself."
			)



	override def columnExtracts :NaturalMap[Column, ColumnExtract] =
		NaturalMap.single[Column, ColumnExtract, S](this, MappingExtract.ident(this))

	override def extracts :NaturalMap[Component, ColumnExtract] =
		NaturalMap.single[Component, ColumnExtract, S](this, MappingExtract.ident(this))



	override def export[T](component :Component[T]) :Component[T] = component

	override def export[T](column :Column[T]) :Column[T] = column



	/** Returns `Unique.empty`. */
	final override def components :Unique[Component[_]] = Unique.empty
	/** Returns `Unique.empty`. */
	final override def subcomponents :Unique[Component[_]] = Unique.empty

	/** Returns `Unique(this)`. */
	final override def columns :Unique[Column[S]] = Unique(this)

	final override def selectable :Unique[Column[S]] = selfUnless(NoSelect)
	final override def queryable :Unique[Column[S]] = selfUnless(NoQuery)
	final override def updatable :Unique[Column[S]] = selfUnless(NoUpdate)
	final override def autoUpdated :Unique[Column[S]] = selfIf(AutoUpdate)
 	final override def insertable :Unique[Column[S]] = selfUnless(NoInsert)
	final override def autoInserted :Unique[Column[S]] = selfIf(AutoInsert)

	/** An empty `Unique` if the given buff is ''disabled'' (not attached), or a singleton `Unique(this)` otherwise. */
	@inline protected final def selfUnless(buff :BuffType) :Unique[Column[S]] =
		if (buff.enabled(buffs)) Unique.empty else Unique.single(this)

	/** An empty `Unique` if the given buff is ''enabled'' (attached), or a singleton `Unique(this)` otherwise. */
	@inline protected final def selfIf(buff :BuffType) :Unique[Column[S]] =
		if (buff.disabled(buffs)) Unique.empty else Unique.single(this)



	/** A read-write form for this column's type which does not take into account any buffs attached to this column.
	  * Serves as the basis for `selectForm`, `queryForm`, `insertForm` and `updateForm` which, in most cases,
	  * should be used instead.
	  */
	def form :ColumnForm[S]

	/*** `this.form` adapted to a `SQLReadForm` by incorporating behaviour modifications caused by applied buffs.
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
	  * and `ExtraQuery`.
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
	  * and `ExtraUpdate`.
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
	  * and `ExtraInsert`.
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





	protected def isNullable :Boolean = Nullable.enabled(buffs)



	/** A new column, with the same name, form (and optionally other properties), but with the buffs replaced by
	  * the provided list.
	  */
	def withBuffs(opts :Seq[Buff[S]]) :ColumnMapping[S, O] = ColumnMapping(name, opts:_*)(form)


	override def renamed(name :String) :ColumnMapping[S, O] = ColumnMapping(name, buffs:_*)(form)

	override def prefixed(prefix :String) :ColumnMapping[S, O] = ColumnMapping(prefix + name, buffs:_*)(form)

	def prefixed(prefix :Option[String]) :ColumnMapping[S, O] = prefix.map(prefixed) getOrElse this

	override def qualified(prefix :String) :ColumnMapping[S, O] =
		prefixOption(prefix).map(p => prefixed(p + ".")) getOrElse this


	def numbered(index :Int) :NumberedColumn[S, O] = new NumberedColumn[S, O](index, name, buffs)(form)



	/** Attaches a label to this column, transforming it into a `LabeledColumn`.
	  * This does not change the name of the column.
	  */
	def @:[N <: Label](label :Label) :LabeledColumn[N, S, O] =
		new StandardColumn[S, O](name, buffs)(form) with LabeledColumn[N, S, O]



	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :Component[X] =
		new StandardColumn[X, O](name, schema.mapBuffs(this)(there, back))(
			schema.mapForm(form)(there, back)
		)

	override def map[X](there :S => X, back :X => S)(implicit nulls :SQLForm.NullValue[X]) :ColumnMapping[X, O] =
		new StandardColumn[X, O](name, buffs.map(_.bimap(there, back)))(
			if (nulls != null) form.bimap(there)(back) else form.bimapNull(there)(back)
		)

	override def flatMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :SQLForm.NullValue[X]) :ColumnMapping[X, O] =
		new StandardColumn[X, O](name, schema.flatMapBuffs(this)(there, back))(
			if (nulls != null) form.biflatMap(there)(back) else form.biflatMapNull(there)(back)
		)



/*
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnMapping[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case column :ColumnMapping[_, _] if canEqual(column) && column.canEqual(this) =>
			column.form == form && column.name == name && column.buffs == buffs
		case _ => false
	}

	override def hashCode :Int = (buffs.hashCode * 31 + name.hashCode) * 31 + form.hashCode
*/



	override def toString :String = name + "[" + form + "]"

	override def debugString :String = buffs.mkString(toString + "(", ", ", ")")

	override def columnString :String = toString


}






object ColumnMapping {

	type ColumnOf[S] = ColumnMapping[S, _]

	type ColumnFrom[O] = ColumnMapping[_, O]

//	type WithSubject[S] = { type M[O] = ColumnMapping[S, O] }
//
//	type WithOrigin[O] = { type M[S] = ColumnMapping[S, O] }



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



	implicit def ColumnProjection[S, A, B] :OriginProjection[ColumnMapping[S, A], A, ColumnMapping[S, B], B] = OriginProjection()

	implicit def LabeledColumnProjection[N <: Label, S, A, B] :OriginProjection[LiteralColumn[N, S, A], A, LiteralColumn[N, S, B], B] =
		OriginProjection()

	implicit def NumberedColumnProjection[S, A, B] :OriginProjection[NumberedColumn[S, A], A, NumberedColumn[S, B], B] =
		OriginProjection()






	class BaseColumn[S, O](val name :String, override val buffs :Seq[Buff[S]])(implicit val form :ColumnForm[S])
		extends ColumnMapping[S, O]
	{
		override val isNullable :Boolean = super.isNullable
	}






	class StandardColumn[S, O](val name :String, override val buffs :Seq[Buff[S]])(implicit val form :ColumnForm[S])
		extends ColumnMapping[S, O]
	{
		override val isNullable :Boolean = super.isNullable

		override val selectForm :SQLReadForm[S] = super.selectForm
		override val queryForm :SQLWriteForm[S] = super.queryForm
		override val updateForm :SQLWriteForm[S] = super.updateForm
		override val insertForm :SQLWriteForm[S] = super.insertForm

		private[this] val selfExtract = MappingExtract.ident(this)

		override val extracts :NaturalMap[Component, ColumnExtract] =
			NaturalMap.single[Component, ColumnExtract, S](this, selfExtract)

		override val columnExtracts :NaturalMap[Column, ColumnExtract] =
			NaturalMap.single[Column, ColumnExtract, S](this, selfExtract)



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

	}






	class LiteralColumn[N <: String with Singleton, S, O](override val buffs :Seq[Buff[S]] = Nil)
	                                                     (implicit named :ValueOf[N], override val form :ColumnForm[S])
		extends LabeledColumn[N, S, O]
	{
		def this(name :N, buffs :Seq[Buff[S]])(implicit form :ColumnForm[S]) =
			this(buffs)(new ValueOf(name), form)

		override val name :N = named.value
		override val isNullable :Boolean = super.isNullable

		override def withBuffs(buffs :Seq[Buff[S]]) :LiteralColumn[N, S, O] =
			new LiteralColumn[N, S, O](buffs)(new ValueOf(name), form)


		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :LiteralColumn[N, X, O] =
			new LiteralColumn[N, X, O](schema.mapBuffs(this)(there, back))(
				implicitly[ValueOf[N]], schema.mapForm(form)(there, back)
			)

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :LiteralColumn[N, X, O] =
			as(Extractor.req(there), Extractor.req(back))

		override def flatMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X]) :LiteralColumn[N, X, O] =
			as(Extractor(there), Extractor(back))

//		override def canEqual(that :Any) :Boolean = that.isInstanceOf[LiteralColumn[_, _, _]]

	}






	class NumberedColumn[S, O](val number :Int, val name :String, override val buffs :Seq[Buff[S]])
	                          (implicit val form :ColumnForm[S])
		extends ColumnMapping[S, O]
	{
		override val isNullable :Boolean = super.isNullable

		override def withBuffs(opts :Seq[Buff[S]]) :ColumnMapping[S, O] = new NumberedColumn(number, name, buffs)

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :NumberedColumn[X, O] =
			new NumberedColumn[X, O](number, name, schema.mapBuffs(this)(there, back))(
				schema.mapForm(form)(there, back)
			)

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :NumberedColumn[X, O] =
			as(Extractor.req(there), Extractor.req(back))

		override def flatMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X]) :NumberedColumn[X, O] =
			as(Extractor.opt(there), Extractor.opt(back))

/*
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[NumberedColumn[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case col :NumberedColumn[_, _] if canEqual(that) && col.canEqual(this) =>
				number == col.number && form == col.form && name == col.name && buffs == col.buffs
			case _ => false
		}

		override def hashCode :Int = super.hashCode * 31 + number.##
*/
	}

}


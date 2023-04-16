package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.OperationView.{FilterView, InsertView, UpdateView, WriteOperationView}
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IncompatibleMappingsException, NoSuchComponentException, NullValueException}
import net.noresttherein.oldsql.haul.ColumnValues
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.{schema, OperationView}
import net.noresttherein.oldsql.schema.Buff.{BuffType, ConstantBuff, FilterAudit, FilterPreset, InsertAudit, InsertPreset, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, Nullable, SelectAudit, SelectDefault, SelectPreset, UpdateAudit, UpdatePreset}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnMappingTemplate, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, MappingAt, MappingTemplate, OriginProjection}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.ExactProjection
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping, ExportMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.LabeledColumn
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.support.{ColumnMappingPrototype, MappedMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.ExportColumnProxy
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct}
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.{BoundColumnParam, ColumnLiteral, LooseColumn}
import net.noresttherein.oldsql.sql.mechanics.RelationCount






/** A `Mapping` representing a single SQL column of a table or select result. They behave in principle just like
  * all other [[net.noresttherein.oldsql.schema.Mapping Mapping]] implementations, but are (normally)
  * the bottom components in the whole mapping hierarchy, as they typically contain no other subcomponents
  * and can always be used in ''select'' clauses of an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]].
  * Their value is not normally assembled like the subjects of other mappings, but externally read by their
  * [[net.noresttherein.oldsql.schema.ColumnMapping.form forms]] and preset in
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
trait ColumnMapping extends Mapping with ColumnMappingTemplate[TypedColumn] {
	column =>

	/** The name of this column, as seen from the containing table if it is a table/view column.
	  * If this column represents a column in the ''select'' clause of a select statement, it is the column name as
	  * returned by the result set for that statement. The names of columns selected in a subselect query are qualified
	  * with the table/view/subselect name/alias from the ''from'' clause and any aliases given in the ''select'' clause
	  * are applied.
	  */
	def name :String

	/** Columns are not typically assembled from subcomponents, but have their values predefined;
	  * this method by default returns `Lack` and there should be little need to override it.
	  */
	override def assemble(pieces: Pieces): Opt[Subject] = Lack

	/** Attempts to retrieve the value for the mapped `Subject` from the given `ComponentValues`. The behaviour for
	  * columns is roughly equivalent to that of components, as implemented by default in
	  * [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]. The difference is that columns without
	  * [[net.noresttherein.oldsql.schema.Buff.Nullable Nullable]] buff will never return `Got(null)`,
	  * but throw a `NullPointerException` with an appropriate message. As with other mappings, this behaviour
	  * can however be overriden. Any [[net.noresttherein.oldsql.schema.Buff.SelectAudit SelectAudit]] buffs are applied,
	  * in order, to the returned value as before (but only if it comes from `pieces`, not other buffs).
	  * Because preset values typically come directly from this column's
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.selectForm selectForm]], implementations may opt to move
	  * some of the buff handling to that method or the form itself.
	  * @throws `NullPointerException` if the preset value is explicitly stated as `null` (`pieces.preset(this)`
	  *                                returned `Got(null)`), but this column does not have the `Nullable` buff.
	  */
	override def optionally(pieces :Pieces) :Opt[Subject] =
		pieces.assemble(this) match {
			case res @ Got(x) => //a very common case
				if (x == null && !isNullable)
					throw new NullValueException("Read a null value for a non-nullable column " + name + ". " +
					                             "Flag the column with Buff.Nullable to explicitly allow nulls.")
				else if (buffs.isEmpty)
					res
				else selectAudit match {
					case Got(audit) => Got(audit(x))
					case _ => res
				}
			case _ => defaultValue
		}

	/** Functions from all [[net.noresttherein.oldsql.schema.Buff.SelectAudit SelectAudit]] buffs
	  * composed with each other. It is applied to every value returned by the form or
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.assemble assemble]] method.
	  */
	protected def selectAudit :Opt[Subject => Subject] = {
		val audits = SelectAudit.Audit(refine)
		if (audits.isEmpty) Lack
		else Got(audits.reduce(_ andThen _))
	}

	/** The value returned by [[net.noresttherein.oldsql.schema.ColumnMapping.optionally optionally]]
	  * if no actual value is present in [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]],
	  * nor can one be assembled from subcolumns.
	  * @return [[net.noresttherein.oldsql.schema.Buff.SelectDefault SelectDefault]]`.`[[net.noresttherein.oldsql.schema.Buff.ValueBuffType.Value Value]]`(this)`
	  *         `orElse` [[net.noresttherein.oldsql.schema.Buff.SelectPreset SelectPreset]]`.Value(this)`.
	  */
	protected def defaultValue :Opt[Subject] = {
		val res = SelectDefault.Value(refine)
		if (res.isDefined) res else SelectPreset.Value(refine)
	}

	/** @inheritdoc
	  * @return the `NullValue` instance provided by the associated form.
	  */
	override def nullValue :NullValue[Subject] = form.nulls

	/** True if `null` is a valid value for this column. Unless the `Nullable` buff is attached,
	  * `optionally` will deliberately throw a `NullPointerException` if a `null` value is preset in
	  * the [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]] argument (typically as the result of
	  * this column's form returning `Got(null)`).
	  * @return `Nullable.active(buffs)`.
	  */
	protected def isNullable :Boolean = Nullable.active(buffs)

//	override def withBuffs(buffs :Buffs[Subject]) :Column[Subject]
//	override def withBuffs(buffs :Seq[Buff[Subject]]) :Column[Subject] = withBuffs(Buffs[Subject](refine, buffs :_*))

	override def writtenValues[T](op :WriteOperationView, subject :Subject) :ComponentValues[Subject, Origin] =
		if (op.applicableColumns(this).nonEmpty)
			ColumnValues.preset(refine, (subject /: op.Audit.Audit(refine)) { (s, f) => f(s) })
		else
			ColumnValues.presetOpt(this, op.Preset.Value(refine))

	override def writtenValues[T](op :WriteOperationView, subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (op.applicableColumns(this).nonEmpty) { //this check is the fastest with columns caching the individual lists.
			val audited = (subject /: op.Audit.Audit(refine)) { (s, f) => f(s) }
			collector.add(refine, audited)
		} else op.Preset.Value(refine) match {
			case Got(res) => collector.add(refine, res)
			case _ =>
		}

	override def filterValues(subject :Subject) :ComponentValues[Subject, Origin] = writtenValues(FilterView, subject)
	override def insertValues(subject :Subject) :ComponentValues[Subject, Origin] = writtenValues(InsertView, subject)
	override def updateValues(subject :Subject) :ComponentValues[Subject, Origin] = writtenValues(UpdateView, subject)



	/** A read-write form for this column's type which does not take into account any buffs attached to this column.
	  * Serves as the basis for `selectForm`, `filterForm`, `insertForm` and `updateForm` which, in most cases,
	  * should be used instead.
	  */
	def form :ColumnForm[Subject]

	/** Default form for this column to use in SQL ''selects''. Unlike other mappings, which implement their
	  * read forms by delegating to their assembly methods, the dependency is, in a way, reversed for columns.
	  * These forms do not rely on the [[net.noresttherein.oldsql.schema.ColumnMapping.optionally optionally]]
	  * or [[net.noresttherein.oldsql.schema.ColumnMapping.assemble assemble]], as most columns cannot be assembled.
	  * Instead, this form is used by enclosing mappings and the framework to read and preset a value for this column
	  * in [[net.noresttherein.oldsql.schema.Mapping.Pieces Pieces]] used to assemble a `ResultSet` row.
	  * Subclasses are free to move some buff handling from `optionally` to the form or this method
	  * as an optimisation. This method will always however ignore any buffs controlling if a column should be read
	  * (included in the ''select'' clause) and the returned form must always attempt to read the value
	  * for this column from the `ResultSet` at the given position. It is the enclosing mapping's responsibility
	  * to pick the columns which will be read in a query.
	  * @return `this.`[[net.noresttherein.oldsql.schema.ColumnMapping.form form]].
	  */ //todo: buffedSelectForm
	override def selectForm :SQLReadForm[Subject] = form

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[Subject] =
		if (components.size == 1 && components.head == this) selectForm
		else if (components.isEmpty) SQLReadForm.empty
		else throw new NoSuchElementException("Mappings " + components + " are not components of column " + this)


	/** Adapts `this.form` by incorporating the behaviour of relevant buffs: the `ExtraXxx` and `XxxAudit`.
	  * In order to stay consistent in custom `TypedColumn` implementations, the adapted form first calls
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.writtenValues this.writtenValues]] and,
	  * if created [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] are not empty,
	  * passes the value for this column to [[net.noresttherein.oldsql.schema.ColumnMapping.form this.form]].
	  */ //consider: ColumnWriteForm? currently it conforms.
	protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[Subject] = form

	protected override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[Subject] =
		if (components.size == 1 && components.contains(this)) op.form(refine)
		else if (components.isEmpty) SQLWriteForm.none(form)
		else throw new NoSuchElementException("Mappings " + components + " are not components of column " + this)



	override def contains[T](component :Component[T]) :Boolean = component == this



	override def original :TypedColumn[Subject, Origin] = this


	override def withBuffs(buffs :Buffs[Subject]) :Column[Subject]
	override def withBuffs(buffs :Seq[Buff[Subject]]) :Column[Subject]


	override def apply(first :ComponentSelection[_, Origin], rest :ComponentSelection[_, Origin]*) :Column[Subject]

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Column[Subject]

	protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:Column[Subject]

	override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Column[Subject]
	override def forFilter(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Column[Subject]
	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Column[Subject]
	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Column[Subject]


	/** Attaches a label to this column, transforming it into a `LabeledColumn`.
	  * This does not change the name of the column. Created column uses the same buffs and form as `this`,
	  * but is otherwise an independent instance not retaining a direct reference to `this`.
	  */
	def labeled[N <: Label](label :Label) :LabeledColumn[N, Subject, Origin] =
		new ExportColumnProxy[Subject, Origin](this, name, buffs) with LabeledColumn[N, Subject, Origin]


	override def qualified(prefix :String) :Column[Subject]

	override def prefixed(prefix :String)  :Column[Subject]

	override def renamed(naming :String => String) :Column[Subject]

	override def reorder(permutation :IndexedSeq[Int]) :Column[Subject]

	override def reorder(precedes :(TypedColumn[_, Origin], TypedColumn[_, Origin]) => Boolean) :Column[Subject]


	override def map[X](there :Subject => X, back :X => Subject)(implicit nulls :NullValue[X]) :Column[X]

	override def optMap[X](there :Subject => Option[X], back :X => Option[Subject])(implicit nulls :NullValue[X]) :Column[X]

	override def as[X](there :Subject =?> X, back :X =?> Subject)(implicit nulls :NullValue[X]) :TypedColumn[X, Origin]

	override def apply(value :Subject) :ColumnLiteral[Subject] = ColumnLiteral(form, value)
	override def ?(value :Subject) :BoundColumnParam[Subject] = BoundColumnParam(form, value)


	/** Two columns are homomorphic ''iff'' their forms are equal.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.homomorphic]]
	  */
	override def uniHomomorphic(that :Mapping) :Boolean = that match {
		case _ if this eq that => true
		case other :ColumnMapping => form == other.form
		case _ => false
	}
	/** Two columns are isomorphic ''iff'' their forms are equal, and they are buffed the same subset of
	  * `{`[[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]`}`.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.isomorphic]]
	  */
	override def uniIsomorphic(that :Mapping) :Boolean = that match {
		case _ if that eq this => true
		case other :ColumnMapping =>
			form == other.form &&
				NoSelectByDefault.active(this) == NoSelectByDefault.active(other) &&
				NoFilterByDefault.active(this) == NoFilterByDefault.active(other) &&
				NoUpdateByDefault.active(this) == NoUpdateByDefault.active(other) &&
				NoInsertByDefault.active(this) == NoInsertByDefault.active(other)
		case _ => false
	}
	/** Two columns are equivalent ''iff'' they are [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]]
	  * (their default include status for ''select'', comparison (''filter''), ''update'', ''insert'' is the same.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.equivalent]]
	  */
	override def equivalent(that :Mapping) :Boolean = that match {
		case other :ColumnMapping => (this eq that) || name == other.name && uniIsomorphic(other)
		case _ => false
	}


	override def mappingName :String = name

	override def buffString :String =
		if (buffs.isEmpty) name + "[" + form + "]" else buffs.mkString(name + "[" + form + "](", ",", ")")

	override def columnString :String = buffString

	override def debugString :String = buffString

	/** A seal method implemented only by [[net.noresttherein.oldsql.schema.bases.BaseColumn BaseColumn]] to enforce
	  * that every concrete implementation extends `BaseColumn`, required by the `sql` package.
	  */
	private[schema] def every_concrete_ColumnMapping_must_extend_BaseColumn(seal :Seal) :Unit

}






private[schema] sealed abstract class Rank1ColumnMappingImplicits {
	//exists for use as the right side of SQLExpression.=== and similar, which will instantiate type F before applying conversion
	implicit def columnSQL[F <: RowProduct, C <: ColumnMapping, S, O <: RowProduct]
	                      (column :C)
	                      (implicit subject :C <:< TypedColumn[S, O], origin :F <:< O,
	                       offset :RelationCount.In[O],
	                       projection :OriginProjection[C, S] { type WithOrigin[A] <: BaseColumn[S, A] })
			:ColumnSQL[F, Single, S] =
		//fixme: migrating from ColumnMapping[_, _] to ColumnMapping broke subtyping between LooseColumn and ColumnSQL
		LooseColumn[F, BaseColumn[S, F], S](projection[F](column), offset.offset).toColumnSQL
}



object ColumnMapping extends Rank1ColumnMappingImplicits {
	type from[O] = ColumnMapping { type Origin = O }

	type ColumnOf[S] = ColumnMapping { type Subject = S }
	type ColumnAt[O] = ColumnMapping { type Origin = O }
	type TypedColumn[S, O] = ColumnMapping { type Subject = S; type Origin = O }

	def apply[S :ColumnForm, O](name :String, buffs :Buffs[S]) :TypedColumn[S, O] =
		new StandardColumn(name, buffs)

	def apply[S :ColumnForm, O](name :String, buffs :Buff[S]*) :TypedColumn[S, O] =
		new StandardColumn(name, buffs)

	def apply[N <: String with Singleton :ValueOf, S :ColumnForm, O](buffs :Buff[S]*) :LiteralColumn[N, S, O] =
		new LiteralColumn[N, S, O](buffs)

	def labeled[N <: String with Singleton, S :ColumnForm, O](label :N, buffs :Buff[S]*) :LiteralColumn[N, S, O] =
		new LiteralColumn[N, S, O](buffs)(new ValueOf(label), ColumnForm[S])

	def labeled[N <: String with Singleton, S :ColumnForm, O](label :N, buffs :Buffs[S]) :LiteralColumn[N, S, O] =
		new LiteralColumn[N, S, O](buffs)(new ValueOf(label), ColumnForm[S])



	implicit def columnComponentSQL[F <: RowProduct, C <: ColumnMapping, S]
	                               (column :C)
	                               (implicit subject :C <:< TypedColumn[S, F], offset :RelationCount.In[F],
	                                projection :OriginProjection[C, S] { type WithOrigin[O] <: BaseColumn[S, O] })
			:LooseColumn[F, projection.WithOrigin, S] =
		LooseColumn[F, C, S](column)


	implicit class ColumnMappingExtension[M <: ColumnMapping](private val self :M) extends AnyVal {
		def toSQL[F <: RowProduct, S](implicit origin :M <:< TypedColumn[S, F], offset :RelationCount[F, _ <: Numeral],
		                              projection :OriginProjection[M, S] { type WithOrigin[O] <: BaseColumn[S, O] })
				:LooseColumn[F, projection.WithOrigin, S] =
			LooseColumn[F, M, S](self)
	}


	/** Extension methods comparing column sets of different mappings. */
	implicit class ColumnCollectionExtension[O](private val self :Unique[TypedColumn[_, O]]) extends AnyVal {

		/** Checks if there is a bijection between the two column lists such that paired columns
		  * are [[net.noresttherein.oldsql.schema.ColumnMapping.homomorphic homomorphic]]
		  * (their types, as defined by their [[net.noresttherein.oldsql.schema.ColumnMapping.form forms]], match).
		  * This is a weaker relation that [[net.noresttherein.oldsql.schema.Mapping.homomorphic homomorphism]]
		  * of owning mappings, as extracts are not compared.
		  */
		def homomorphic(other :Unique[TypedColumn[_, _]]) :Boolean =
			(self eq other) || (self.size == other.size && self.forall(l => other.exists(l homomorphic _)))

		/** Checks if the two column lists are of the same size and that corresponding columns are
		  * [[net.noresttherein.oldsql.schema.ColumnMapping.isomorphic isomorphic]]
		  * (their types, as defined by their [[net.noresttherein.oldsql.schema.ColumnMapping.form forms]], match).
		  * This is a weaker relation that [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphism]]
		  * of owning mappings, as extracts are not compared and the order of corresponding columns must be the same
		  * in both collections.
		  */
		def isomorphic(other :Unique[TypedColumn[_, _]]) :Boolean =
			(self eq other) || (self.size == other.size && self.view.zip(other).forall { case (l, r) => l isomorphic r })

		/** Checks if the two column lists are of the same size and that the types (defined by their
		  * [[net.noresttherein.oldsql.schema.ColumnMapping.form forms]]) of corresponding column pairs match.
		  */ //consider: renaming to compatible
		def equivalent(other :Unique[TypedColumn[_, _]]) :Boolean =
			(self eq other) || (self.size == other.size && self.view.zip(other).forall { case (l, r) => l equivalent r })

		/** Checks if the two column lists are of the same length and that the forms of the corresponding columns
		  * are [[net.noresttherein.oldsql.schema.SQLForm.comparable comparable]]. This check means that
		  * SQL for these columns are compatible on the database level.
		  */
		def comparable(other :Unique[TypedColumn[_, _]]) :Boolean =
			(self eq other) ||
				(self.size == other.size && self.view.zip(other).forall { case (l, r) => l.form comparable r.form })

		/** Checks if the corresponding column pairs in both lists are structurally equal, as defined by
		  * [[net.noresttherein.oldsql.schema.ColumnMapping.identical identical]]. It is the 'proper equality' of
		  * columns, rather than the default referential equality: it involves equality (or structural equality)
		  * of all their matching members, including buffs.
		  */
		def identical(other :Unique[TypedColumn[_, _]]) :Boolean =
			(self eq other) || (self.size == other.size && self.view.zip(other).forall { case (l, r) => l identical r })

		/** Maps all these columns to their counterparts in another mapping.
		  * @param owner a mapping of which these columns are components.
		  * @param other a mapping [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] with `owner`.
		  */
		def counterparts[Q](owner :MappingAt[O], other :MappingAt[Q]) :Unique[TypedColumn[_, Q]] =
			if (!(owner isomorphic other))
				throw new IllegalArgumentException("Mapping " + other + " is not isomorphic with mapping " + owner + ".")
			else if (!self.forall(owner.contains(_)))
				throw new IncompatibleMappingsException(
					"Columns " + self.filterNot(owner.contains(_)) + " do not belong to mapping " + owner + "."
				)
			else
				self.map(other.counterpart(owner, _))

		/** Finds the position of the given column in this collection (assuming it has a fixed iterations order).
		  * Similar to `Seq.indexOf`, but instead of returning `-1` if the component is not found,
		  * it throws a [[net.noresttherein.oldsql.exceptions.NoSuchComponentException NoSuchComponentException]].
		  */
		private[oldsql] def columnIndex(column :MappingAt[O]) :Int = {
			val idx = self.indexOf(column)
			if (idx >= 0) idx
			else throw new NoSuchComponentException(
				"Column " + column + " is not present among " + self + "; is it an export version for the same mapping?"
			)
		}
	}



	//this is here to have a clear precedence hierarchy with Mapping subtypes declaring their own projections
	@inline implicit def columnMappingOriginProjection[S]
			:OriginProjection[ColumnOf[S], S] with ExactProjection[ColumnOf[S]] { type WithOrigin[O] = BaseColumn[S, O] } =
		OriginProjection.projectAs[ColumnOf[S], ColumnOf[S]#TypedColumnProjection]
//	@inline implicit def columnMappingOriginProjection[S]
//			:OriginProjection[ColumnOf[S], S] with ExactProjection[ColumnOf[S]] { type WithOrigin[O] = BaseColumn[S, O] } =
//		OriginProjection.projectAs[ColumnOf[S], ColumnOf[S]#TypedColumnProjection]




	/** A part of [[net.noresttherein.oldsql.schema.ColumnMapping TypedColumn]] interface with properties
	  * and methods providing access to its (sub)columns. As with its supertype `MappingTemplate`,
	  * it is extended by specialist column implementations of kind `C`, shared by any of its subcolumns.
	  * The implementations provided here are the defaults, presenting this column as its only column/component,
	  * and individual column lists and [[net.noresttherein.oldsql.OperationView OperationView]]-specific methods
	  * always return either an empty collection or the singleton `this.columns`, depending on the presence
	  * of appropriate [[net.noresttherein.oldsql.schema.Buff buffs]].
	  */
	trait ColumnMappingTemplate[+Col[T, Q] <: TypedColumn[T, Q]] extends Mapping with MappingTemplate[Col, Col] {
		def name :String

		override type TypedAsFrom[O] <: BaseColumn[Subject, O]

		override def columnNamed(name :String) :Col[_, Origin] =
			if (name == this.name) refine
			else throw new NoSuchComponentException("No column '" + name + "' in column " + this + ".")

		protected override def selfExtract :SpecificExtract[Col[Subject, Origin], Subject, Subject, Origin] =
			SpecificExtract.ident(refine)

		override def columnExtracts :NaturalMap[Column, like[Col]#Extract] =
			NaturalMap.single[Column, like[Col]#Extract, Subject](refine, selfExtract)

		override def extracts :NaturalMap[Component, like[Col]#Extract] =
			NaturalMap.single[Component, like[Col]#Extract, Subject](refine, selfExtract)


		override def components    :Unique[Col[_, Origin]] = Unique.empty
		override def subcomponents :Unique[Col[_, Origin]] = Unique.empty

		/** A singleton collection containing the underlying column. In most cases, it will contain
		  * this column itself, although some rare column implementations such as foreign key columns may return
		  * an adapted other column.
		  */
		override def columns       :Unique[Col[_, Origin]] = Unique.single(refine)

		override def selectedByDefault :Unique[Col[_, Origin]] = selectable
		override def filteredByDefault :Unique[Col[_, Origin]] = filterable
		override def insertedByDefault :Unique[Col[_, Origin]] = insertable
		override def updatedByDefault  :Unique[Col[_, Origin]] = updatable

		override def mandatorySelect   :Unique[Col[_, Origin]] = selectable
		override def mandatoryFilter   :Unique[Col[_, Origin]] = filterable
		override def mandatoryInsert   :Unique[Col[_, Origin]] = insertable
		override def mandatoryUpdate   :Unique[Col[_, Origin]] = updatable

		override def selectedByDefault[T](component :Component[T]) :Unique[Col[_, Origin]] = selectable(component)
		override def filteredByDefault[T](component :Component[T]) :Unique[Col[_, Origin]] = filterable(component)
		override def insertedByDefault[T](component :Component[T]) :Unique[Col[_, Origin]] = insertable(component)
		override def updatedByDefault[T](component :Component[T])  :Unique[Col[_, Origin]] = updatable(component)

		override def columns[T](component :Component[T])           :Unique[Col[_, Origin]] =
			if (component == this) columns else component.columns.map(export(_))

		override def columnsWith(buff :BuffType) :Unique[Col[_, Origin]] =
			if (buff.inactive(buffs)) Unique.empty else columns

		override def columnsWithout(buff :BuffType) :Unique[Col[_, Origin]] =
			if (buff.active(buffs)) Unique.empty else columns

		override def columnsWith(component :Component[_], buff :BuffType) :Unique[Col[_, Origin]] =
			if (component == this) columnsWith(buff)
			else if (buff.active(export(component))) columns(component)
			else Unique.empty

		override def columnsWithout(component :Component[_], buff :BuffType) :Unique[Col[_, Origin]] =
			if (component == this) columnsWithout(buff)
			else if (buff.inactive(export(component))) columns(component)
			else Unique.empty


		override def export[T](component :Component[T]) :Col[T, Origin] =
			if (component == this) this.asInstanceOf[Col[T, Origin]]
			else throwNoSuchComponentException(component)

		override def export[T](column :Column[T]) :Col[T, Origin] = export(column :Component[T])


		protected def throwNoSuchComponentException(component :Component[_]) :Nothing =
			throw new NoSuchElementException("No component " + component + " in column " + this + ".")
	}




	/** Basic (but full) `TypedColumn` implementation. The difference from
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn StandardColumn]] is that the latter
	  * precomputes many of its properties, storing them in `val`s. Usage of this class should be preferred when
	  * the column needs to override default `optionally`/`apply` definitions,
	  * when inherited/overridden implementations of some methods would conflict with those from
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.StableColumn StableColumn]], or when it is created on demand,
	  * rather than at application initialization,.
	  */ //todo: move to bases
	class LightColumn[S, O](override val name :String, override val buffs :Buffs[S])
	                       (implicit override val form :ColumnForm[S])
		extends BaseColumn[S, O]
	{
		def this(name :String, buffs :Seq[Buff[S]])(implicit form :ColumnForm[S]) =
			this(name, Buffs(buffs :_*))

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
	  * themselves. If the `ComponentValues` passed to `optionally` were populated using this instance's `selectForm`,
	  * than the above scheme will have the same result it would for a normal `TypedColumn`.
	  * A difference can be observed if the values are created manually, in which case it would appear as if this column
	  * had no buffs. It doesn't formally break the contract of `TypedColumn` as it leaves it completely
	  * to the mapping implementation how it chooses to handle any preset value, but can nevertheless be confusing
	  * and undesirable. The inconsistency can be eliminated by moving buff handling to the `ComponentValues`
	  * implementation or the code creating it, as long as all [[net.noresttherein.oldsql.schema.Buff.AuditBuff audit]]
	  * buffs are idempotent.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn]]
	  */ //todo: move down next to StableColumn
	trait SimpleColumn[S, O] extends BaseColumn[S, O] with ExportMapping {

		final override def assemble(pieces :Pieces) :Opt[S] = Lack

		final override def apply(pieces: Pieces): S = optionally(pieces) match {
			case Got(res) => res
			case _ =>
				throw new NoSuchElementException(
					"No value for column " + this + ". This may be caused by reading a null value;" +
					"Flag the column with Buff.Nullable to explicitly allow nulls."
				)
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
		  *                                returned `Got(null)`), but this column does not have the `Nullable` buff.
		  */
		final override def optionally(pieces :Pieces) :Opt[S] =
			if (isNullable)
				pieces.preset(this)
			else
				pieces.preset(this) match {
					case Got(null) =>
						throw new NullPointerException("Read a null value for a non-nullable column " + name + ". " +
						                               "Flag the column with Buff.Nullable to explicitly allow nulls.")
					case res => res //don't check buffs - we do it in the form for speed
				}

		/** `this.form` adapted to an `SQLReadForm` by incorporating some of behaviour modifications caused by
		  * applied buffs. This includes default values from buffs like `OptionalSelect` and transformations
		  * from `AuditBuff`s and similar. It doesn't verify if the column is selectable by default or at all,
		  * returning always (possibly decorated) `this.form`,
		  * unless [[net.noresttherein.oldsql.schema.Buff.SelectPreset SelectPreset]] is present, in which case
		  * a constant `SQLReadForm` is returned, which does not read anything from the `ResultSet`. In all other cases,
		  * the returned form is a [[net.noresttherein.oldsql.schema.ColumnReadForm ColumnReadForm]].
		  * Note that the `optionally`/`apply` and `assembly` methods of this mapping are ''not'' called by the returned
		  * form. They are involved only as a part of the assembly process for owning components, provided
		  * by the created `ComponentValues` containing the value returned by this form instead.
		  */
//		final override def selectForm :SQLReadForm[S] = defaultSelectForm
//
		//fixme: this breaks the contract which states that selectForm ignores buffs on this component.
		//  Currently there is no method which could handle this instead and it cannot be referred to the parent component.
		override def selectForm :SQLReadForm[S] = SelectPreset.get(buffs) match {
			//these *could* be column forms, but likely we'd rather have it zero width, as there is no such column in the db.
			case Got(ConstantBuff(x)) => SQLReadForm.const(name + "='" + x + "'>")(0)(x)
			case Got(buff) => SQLReadForm.eval(0, name + "=_>")(buff.value)
			case _ if NoSelect.active(this) => SQLReadForm.empty
			case _ =>
				val audits = SelectAudit.Audit(buffs)
				val audited = //we can't enforce not null here because of artificial nulls resulting from outer joins
					if (audits.isEmpty) form
					else form.map(audits.reduce(_ andThen _))(form.nulls)
				SelectDefault.get(buffs) match {
					case Got(ConstantBuff(x)) =>
						audited orElse ColumnReadForm.const(audited.sqlType, name + "='" + x + "'>")(x)
					case Got(buff) =>
						audited orElse ColumnReadForm.eval(audited.sqlType, name + "=_>")(buff.value)
					case _ => audited
				}
		}

		/** Adapts `this.form` by incorporating the behaviour of relevant buffs: the `ExtraXxx` and `XxxAudit`.
		  * Only standard buffs modifying/providing the written value are applied; any buffs determining if the column
		  * can or should be included in the given operation are ignored. The caller should use an explicit column list
		  * rather than rely on this form to handle the case of an excluded column.
		  * Note that `OptionalXxx` and even `NoXxx` buffs are ignored here and columns need to be explicitly
		  * included/excluded in an operation. The burden of validating this information lies with the owning mapping.
		  */ //todo: make it a ColumnWriteForm. The problem is that it is overriden by StableColumn
		protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[S] = op.Preset.get(buffs) match {
			case Got(ConstantBuff(x)) => ColumnWriteForm.const(x)(form)
			case Got(buff) => ColumnWriteForm.eval(buff.value)(form)
			case _ =>
				val audits = op.Audit.Audit(buffs)
				val audited =
					if (audits.isEmpty) form
					else form.unmap(audits.reduce(_ andThen _))
				op.Default.get(buffs) match {
					case Got(buff) => audited.withNull(buff.toNullValue)
					case _ => audited
				}
		}


		override def apply[T](component :Component[T]) :ColumnExtract[T] =
			if (component == this)
				ColumnExtract.ident[S, O](this).asInstanceOf[ColumnExtract[T]]
			else throwNoSuchComponentException(component)

		override def apply[T](column :Column[T]) :ColumnExtract[T] = apply(column :Component[T])
		//these are unnecessary overrides from ColumnMappingTemplate
		override def export[T](component :Component[T]) :Column[T] =
			if (component == this)
				this.asInstanceOf[Column[T]]
			else
				throwNoSuchComponentException(component)

		override def export[T](column :Column[T]) :Column[T] = export(column :Component[T])

		override def contains[T](component :Component[T]) :Boolean = component == this


		protected override def copy(name :String, buffs :Buffs[S]) :TypedColumn[S, O] =
			new StandardColumn[S, O](name, buffs)(form)

		override def labeled[N <: Label](label :Label) :LabeledColumn[N, S, O] =
			new StandardColumn[S, O](name, buffs)(form) with LabeledColumn[N, S, O]

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :TypedColumn[X, O] =
			new StandardColumn[X, O](name, buffs.unsafeBimap(there, back))(form.as(there)(back))

		/** Two columns are identical if their names, forms and buffs are equal. Column adapters adding
		  * specific functionality may impose additional conditions.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.identical]]
		  */
		override def identical(that :Mapping) :Boolean = that match {
			case _ if that eq this => true
			case column :ColumnMapping if canEqual(that) =>
				name == column.name && form == column.form && buffs == column.buffs
			case _ => false
		}
	}



	/** Defaults `TypedColumn` implementation. Many of the properties are overridden as `val`s for efficiency. */
	class StandardColumn[S, O](override val name :String, override val buffs :Buffs[S])
	                          (implicit override val form :ColumnForm[S]) //consider: moving to bits
		extends SimpleColumn[S, O] with StableColumn[S, O]
	{
		def this(name :String, buffs :Seq[Buff[S]])(implicit form :ColumnForm[S]) =
			this(name, Buffs(buffs :_*))
	}






	/** A labeled `TypedColumn` implementation. As an additional important constraint over
	  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping.LabeledColumn LabeledColumn]] is that the name of the
	  * column is the same as the label.
	  */ //todo: move to bits?
	class LiteralColumn[N <: String with Singleton, S, O](override val buffs :Buffs[S] = Buffs.empty[S])
	                                                     (implicit label :ValueOf[N], override val form :ColumnForm[S])
		extends LabeledColumn[N, S, O] with SimpleColumn[S, O] with StableColumn[S, O]
	{
		def this(name :N, buffs :Buffs[S])(implicit form :ColumnForm[S]) =
			this(buffs)(new ValueOf(name), form)

		def this(buffs :Seq[Buff[S]])(implicit label :ValueOf[N], form :ColumnForm[S]) =
			this(Buffs(buffs :_*))(label, form)

		def this(name :N, buffs :Seq[Buff[S]])(implicit form :ColumnForm[S]) =
			this(Buffs(buffs :_*))(new ValueOf(name), form)

		override val name :N = label.value

		override def withBuffs(buffs :Buffs[S]) :LiteralColumn[N, S, O] =
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
	  * `TypedColumn`. This allows the latter to be mixed in later, overriding declarations from any other traits
	  * (and classes) that the extending class might inherit.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.StableColumn]]
	  */ //todo: move to bases
	private[schema] abstract class ColumnSupport[S, O](val name :String, override val buffs :Buffs[S] = Buffs.empty[S])
	                                                  (implicit val form :ColumnForm[S])
		extends BaseMapping[S, O]
	{ this :TypedColumn[S, O] =>
		def this(name :String, buffs :Seq[Buff[S]])(implicit form :ColumnForm[S]) =
			this(name, Buffs(buffs :_*))
	}



	/** A mix-in trait providing optimized `writtenValues` family of methods by caching buff information in fields,
	  * as well as fields for [[net.noresttherein.oldsql.schema.ColumnMapping.isNullable isNullable]]
	  * and all column lists, initialized by calls to `super`. As these are based on
	  * the [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] of the column, they must be initialized
	  * before this trait in class linearization.
	  */
	trait OptimizedColumnTemplate[+Col[T, Q] <: TypedColumn[T, Q], S, O]
		extends BaseColumn[S, O] with ColumnMappingTemplate[Col]
	{ this :Col[S, O] =>
		if (buffs == null)
			throw new UninitializedFieldError(
				"Buffs of " + getClass + " are uninitialized before OptimizedColumnTemplate trait initialization"
			)

		final override val isNullable :Boolean = super.isNullable

		protected val isFilterable        :Boolean = NoFilter.inactive(this)
		protected val isInsertable        :Boolean = NoInsert.inactive(this)
		protected val isUpdatable         :Boolean = NoUpdate.inactive(this)
//		protected val isSelectedByDefault :Boolean = NoSelectByDefault.inactive(this)
//		protected val isFilteredByDefault :Boolean = NoFilterByDefault.inactive(this)
//		protected val isInsertedByDefault :Boolean = NoFilterByDefault.inactive(this)
//		protected val isUpdatedByDefault  :Boolean = NoFilterByDefault.inactive(this)

		protected val filterAudit :S => S = FilterAudit.fold(this)
		protected val insertAudit :S => S = InsertAudit.fold(this)
		protected val updateAudit :S => S = UpdateAudit.fold(this)

		protected val extraFilter :Opt[S] = FilterPreset.Value(this)
		protected val extraInsert :Opt[S] = InsertPreset.Value(this)
		protected val extraUpdate :Opt[S] = UpdatePreset.Value(this)

		protected val extraFilterValues :ColumnValues[S, O] = extraWrite(extraFilter)
		protected val extraInsertValues :ColumnValues[S, O] = extraWrite(extraInsert)
		protected val extraUpdateValues :ColumnValues[S, O] = extraWrite(extraUpdate)

		protected def extraWrite(extra :Opt[S]) :ColumnValues[S, O] = extra match {
			case Got(value) => ColumnValues.preset[S, S, O](this, value)
			case _ => ColumnValues.empty[S, O]
		}

		protected override val selectAudit  :Opt[S => S] = super.selectAudit
		protected override val defaultValue :Opt[S] = super.defaultValue

		override def writtenValues[T](op :WriteOperationView, subject :S) :ComponentValues[S, O] =
			op.writtenValues(this, subject)

		override def filterValues(subject :S) :ComponentValues[S, O] =
			if (isFilterable) ColumnValues.preset(this, filterAudit(subject))
			else extraFilterValues

		override def insertValues(subject :S) :ComponentValues[S, O] =
			if (isInsertable) ColumnValues.preset(this, insertAudit(subject))
			else extraInsertValues

		override def updateValues(subject :S) :ComponentValues[S, O] =
			if (isUpdatable) ColumnValues.preset(this, updateAudit(subject))
			else extraUpdateValues


		override def writtenValues[T](op :WriteOperationView, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			op.writtenValues(this, subject, collector)

		override def filterValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isFilterable) collector.add(this, filterAudit(subject))
			else collector.addOpt(this, extraFilter)

		override def insertValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isInsertable) collector.add(this, insertAudit(subject))
			else collector.addOpt(this, extraInsert)

		override def updateValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isUpdatable) collector.add(this, updateAudit(subject))
			else collector.addOpt(this, extraUpdate)



		override def extracts :NaturalMap[Component, like[Col]#Extract] =
			defaultExtracts.asInstanceOf[NaturalMap[Component, like[Col]#Extract]]

		override def columnExtracts :NaturalMap[Column, like[Col]#Extract] = defaultExtracts

		override def columns           :Unique[Col[_, O]] = defaultColumns
		override def selectable        :Unique[Col[_, O]] = superSelectable
		override def filterable        :Unique[Col[_, O]] = superFilterable
		override def insertable        :Unique[Col[_, O]] = superInsertable
		override def updatable         :Unique[Col[_, O]] = superUpdatable
		override def autoInserted      :Unique[Col[_, O]] = superAutoInserted
		override def autoUpdated       :Unique[Col[_, O]] = superAutoUpdated
//		override def selectedByDefault :Unique[Col[_, O]] = superSelectedByDefault
//		override def filteredByDefault :Unique[Col[_, O]] = superFilteredByDefault
//		override def insertedByDefault :Unique[Col[_, O]] = superInsertedByDefault
//		override def updatedByDefault  :Unique[Col[_, O]] = superUpdatedByDefault

		protected override val selfExtract :SpecificExtract[Col[S, O], S, S, O] = SpecificExtract.ident(this)
		protected val defaultExtracts = NaturalMap.single[Column, like[Col]#Extract, S](this, selfExtract)

		protected final val defaultColumns         :Unique[Col[_, O]] = Unique.single(this)
		protected final val superSelectable        :Unique[Col[_, O]] = super.selectable
		protected final val superFilterable        :Unique[Col[_, O]] = super.filterable
		protected final val superInsertable        :Unique[Col[_, O]] = super.insertable
		protected final val superUpdatable         :Unique[Col[_, O]] = super.updatable
		protected final val superAutoInserted      :Unique[Col[_, O]] = super.autoInserted
		protected final val superAutoUpdated       :Unique[Col[_, O]] = super.autoUpdated
//		protected final val superSelectedByDefault :Unique[Col[_, O]] = super.selectedByDefault
//		protected final val superFilteredByDefault :Unique[Col[_, O]] = super.filteredByDefault
//		protected final val superInsertedByDefault :Unique[Col[_, O]] = super.insertedByDefault
//		protected final val superUpdatedByDefault  :Unique[Col[_, O]] = super.updatedByDefault


		override def apply[T](component :Component[T]) :like[Col]#Extract[T] =
			if (component eq this)
				selfExtract.asInstanceOf[like[Col]#Extract[T]]
			else
				throw new IllegalArgumentException(
					s"Mapping $component is not a subcomponent of column $this. The only subcomponent of a column is the column itself."
				)

		override def apply[T](column :Column[T]) :like[Col]#Extract[T] = this(column :Component[T])
	}


	type OptimizedColumn[S, O] = OptimizedColumnTemplate[TypedColumn, S, O]
//	trait OptimizedColumn[S, O] extends OptimizedColumnTemplate[TypedColumn, S, O]



	/** A late mix-in trait overriding all benefiting `TypedColumn` methods with `val`s. Needs to be placed
	  * in the linearization order after declarations of `name`, `buffs` and `form`.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.ColumnSupport]]
	  */
	trait StableColumn[S, O] extends OptimizedColumn[S, O] {
		protected val superSelectForm :SQLReadForm[S] = super.selectForm
		protected val superFilterForm :SQLWriteForm[S] = super.newWriteForm(FilterView)
		protected val superInsertForm :SQLWriteForm[S] = super.newWriteForm(InsertView)
		protected val superUpdateForm :SQLWriteForm[S] = super.newWriteForm(UpdateView)

		override def selectForm :SQLReadForm[S]  = superSelectForm
		override def filterForm :SQLWriteForm[S] = superFilterForm
		override def insertForm :SQLWriteForm[S] = superInsertForm
		override def updateForm :SQLWriteForm[S] = superUpdateForm
		protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[S] = op.form(this)


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components == selectedByDefault) selectForm else super.selectForm(components)

		override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components == filteredByDefault) filterForm else super.filterForm(components)

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components == insertedByDefault) insertForm else super.insertForm(components)

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components == updatedByDefault) updateForm else super.updateForm(components)

		protected override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[S] =
			op.form(this)
	}

}



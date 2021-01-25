package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.collection.{Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.{Buff, Buffs, ColumnMapping, ColumnMappingExtract, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{ExtraSelect, OptionalSelect, SelectAudit, SelectDefault}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingReadForm, MappingWriteForm, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.ProjectionDef
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.OptionMapping
import net.noresttherein.oldsql.schema.bits.OptionMapping.Optional
import net.noresttherein.oldsql.schema.bits.MappingPath.ComponentPath
import net.noresttherein.oldsql.schema.support.{AdjustedMapping, AlteredMapping, BuffedMapping, MappedMapping, PrefixedMapping, RenamedMapping}






/** The de facto base trait of all `Mapping` implementations.
  *
  * `Mapping` remains the main outside interface as it allows easy parameterizing with the mapping type
  * without the extra `Origin` and `Subject` type parameters, but there are chosen places where the `BaseMapping`
  * is explicitly required due to type system's limitation. At the same time, it allows to leave some of the methods
  * unimplemented in the `Mapping` trait, allowing traits extending it to narrow down their result types without
  * providing their definitions. See the [[net.noresttherein.oldsql.schema.Mapping Mapping]]'s class documentation
  * for more detailed reasons behind its existence as well as extensive general introduction.
  *
  * @tparam S The subject type, that is the type of objects read and written to a particular table (or a view, query,
  *           or table fragment).
  * @tparam O A marker 'Origin' type, used to distinguish between several instances of the same mapping class,
  *           but coming from different sources (especially different aliases for a table occurring more then once
  *           in a join). At the same time, it adds additional type safety by ensuring that only components of mappings
  *           included in a query can be used in the creation of SQL expressions used by that query.
  *           Consult [[net.noresttherein.oldsql.schema.Mapping.Origin Mapping.Origin]]
  */
trait BaseMapping[S, O] extends Mapping { self =>
	override type Origin = O
	override type Subject = S
	//for nicer compiler output
	override type Extract[T] = MappingExtract[S, T, O]
	override type ColumnExtract[T] = ColumnMappingExtract[S, T, O]
	override type AnyComponent = MappingAt[O]
	override type Component[T] = RefinedMapping[T, O]
	override type Column[T] = ColumnMapping[T, O]



	override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		if (op.Prohibited.inactive(this)) {
			val audited = op.Audit.fold(this)(subject)
			def componentValues[X](comp :Component[X]) :Unit = {
				apply(comp).opt(audited) match {
					case Got(value) => comp.writtenValues(op, value, collector)
					case _ => op.Default.Value(comp) match {
						case Got(value) => comp.writtenValues(op, value, collector)
						case _ =>
					}
				}
			}
			components foreach { c :Component[_] => componentValues(c) }
		}


	override def apply(pieces: Pieces): S =
		optionally(pieces) getOrElse {
			throw new IllegalArgumentException(s"Can't assemble $this from $pieces")
		}

	override def optionally(pieces: Pieces): Opt[S] = pieces.assemble(this) match {
		case res if buffs.isEmpty => res //a very common case
		case Got(res) => Some((res /: SelectAudit.Audit(this)) { (acc, f) => f(acc) })
		case _ =>
			val res = SelectDefault.Value(this)
			if (res.isDefined) res
			else ExtraSelect.Value(this)
	}

	override def assemble(pieces :Pieces) :Opt[S]

	/** @inheritdoc
	  * @return `NullValue.NotNull` by default.
	  */
	override def nullValue :NullValue[S] = NullValue.NotNull

	/** @inheritdoc
	  * @return an empty collection, unless overriden. */
	override def buffs :Buffs[S] = Buffs.empty

	override def withBuffs(buffs :Buffs[S]) :Component[S] =
		BuffedMapping[BaseMapping[S, O], S, O](this, buffs)



	def apply[M >: this.type <: RefinedMapping[S, O], X <: Mapping, C <: RefinedMapping[T, O], T]
	         (component :M => X)(implicit hint :InferTypeParams[X, C, RefinedMapping[T, O]])
			:ComponentPath[M, C, S, T, O] =
		ComponentPath(this :M, component(this))



	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		MappingReadForm.select(this, components)

	/** @inheritdoc
	  * @return `writeForm(FILTER, components)` unless overriden. */
	override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		writeForm(FILTER, components)

	/** @inheritdoc
	  * @return `writeForm(INSERT, components)` unless overriden. */
	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		writeForm(INSERT, components)

	/** @inheritdoc
	  * @return `writeForm(UPDATE, components)` unless overriden. */
	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		writeForm(UPDATE, components)

	/** @inheritdoc
	  * @return `MappingWriteForm(op, this, components)` by default. */
	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
		MappingWriteForm(op, this, components)

	override def selectForm :SQLReadForm[S] = MappingReadForm.defaultSelect(this)

	/** Default write form (included parameters) used for the ''WHERE'' clause filters for this mapping.
	  * @return `writeForm(FILTER)` (or a functionally equivalent instance) unless overriden. */
	override def filterForm :SQLWriteForm[S] = writeForm(FILTER)

	/** Default write form (included columns) of insert statements for this mapping.
	  * @return `writeForm(INSERT)` (or a functionally equivalent instance) unless overriden. */
	override def insertForm :SQLWriteForm[S] = writeForm(INSERT)

	/** Default write form (included columns) of update statements for this mapping.
	  * @return `writeForm(UPDATE)` (or a functionally equivalent instance) unless overriden. */
	override def updateForm :SQLWriteForm[S] = writeForm(UPDATE)

	/** The delegate target of all properties with write forms of various statement types.
	  * @return `MappingWriteForm(op, this)` unless overriden. */
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = MappingWriteForm(op, this)


	override def apply(adjustments :ComponentSelection[_, O]*) :Component[S] =
		apply(
			adjustments.view.collect { case IncludedComponent(c) => c },
			adjustments.view.collect { case ExcludedComponent(c) => c }
		)


	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		AdjustedMapping[BaseMapping[S, O], S, O](this, include, exclude)

	override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		AlteredMapping.select[BaseMapping[S, O], S, O](this, include, exclude)

	override def forFilter(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		AlteredMapping.filter[BaseMapping[S, O], S, O](this, include, exclude)

	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		AlteredMapping.insert[BaseMapping[S, O], S, O](this, include, exclude)

	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		AlteredMapping.update[BaseMapping[S, O], S, O](this, include, exclude)



	override def qualified(prefix :String) :Component[S] =
		if (prefix.length == 0) this else prefixed(prefix + ".")

	override def prefixed(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else PrefixedMapping[BaseMapping[S, O], S, O](prefix, this)

	override def renamed(naming :String => String) :Component[S] =
		RenamedMapping[this.type, S, O](this :this.type, naming)


	override def inOption :Optional[this.type] = OptionMapping.singleton(this)

	override def as[X](there :Subject =?> X, back :X =?> Subject)(implicit nulls :NullValue[X] = null) :Component[X] =
		MappedMapping[BaseMapping[S, O], S, X, O](this, there, back)


	protected[oldsql] override def everyConcreteMappingMustExtendBaseMapping :Nothing =
		throw new UnsupportedOperationException
}






object BaseMapping {

	/** A curried definition of [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[S, O]`,
	  * containing a single type constructor `P[O] = BaseMapping[S, O]`. It allows the use of `BaseMapping`
	  * as a type parameters to classes/methods which require the definition of a mapping accepting
	  * its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
	  */
	type Of[S] = { type P[O] = BaseMapping[S, O] }

	type AnyAt[O] = M[O] forSome { type M[A] <: BaseMapping[_, A] }

	type AnyOf[S] = M[S] forSome { type M[X] <: BaseMapping[X, _] }


	//note for the future: there is some problem with projections from mappings with Nothing as the origin type.
	@inline implicit def baseMappingOriginProjection[M[A] <: BaseMapping[S, A], S, O]
	                                                (implicit types :M[O] <:< BaseMapping[S, O])
			:ProjectionDef[M[O], M, S] =
		OriginProjection.isomorphism[M, S, O]

}



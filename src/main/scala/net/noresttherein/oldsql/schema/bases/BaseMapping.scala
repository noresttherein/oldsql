package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, ColumnMappingExtract, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{ExtraSelect, OptionalSelect, SelectAudit}
import net.noresttherein.oldsql.schema.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingReadForm, MappingWriteForm, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.ProjectionDef
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.OptionMapping
import net.noresttherein.oldsql.schema.bits.OptionMapping.Optional
import net.noresttherein.oldsql.schema.bits.MappingPath.ComponentPath
import net.noresttherein.oldsql.schema.support.{AdjustedMapping, AlteredMapping, MappedMapping, PrefixedMapping}






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
		if (op.prohibited.disabled(this)) {
			val audited = op.audit.fold(this)(subject)
			def componentValues[X](comp :Component[X]) :Unit = {
				apply(comp).get(audited) match {
					case Some(value) => comp.writtenValues(op, value, collector)
					case _ =>
				}
			}
			components foreach { c :Component[_] => componentValues(c) }
		}


	override def apply(pieces: Pieces): S =
		optionally(pieces) getOrElse {
			throw new IllegalArgumentException(s"Can't assemble $this from $pieces")
		}

	override def optionally(pieces: Pieces): Option[S] = pieces.assemble(this) match {
		case res if buffs.isEmpty => res //a very common case
		case Some(res) => Some((res /: SelectAudit.Audit(this)) { (acc, f) => f(acc) })
		case _ =>
			val res = OptionalSelect.Value(this)
			if (res.isDefined) res
			else ExtraSelect.Value(this)
	}

	override def assemble(pieces :Pieces) :Option[S]

	/** @inheritdoc
	  * @return `NullValue.NotNull` by default.
	  */
	override def nullValue :NullValue[S] = NullValue.NotNull



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
	  * @return `writeForm(UPDATE, components)` unless overriden. */
	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		writeForm(UPDATE, components)

	/** @inheritdoc
	  * @return `writeForm(INSERT, components)` unless overriden. */
	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		writeForm(INSERT, components)

	/** @inheritdoc
	  * @return `MappingWriteForm(op, this, components)` by default. */
	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
		MappingWriteForm(op, this, components)

	override def selectForm :SQLReadForm[S] = MappingReadForm.defaultSelect(this)

	/** Default write form (included parameters) used for the ''WHERE'' clause filters for this mapping.
	  * @return `writeForm(FILTER)` (or a functionally equivalent instance) unless overriden. */
	override def filterForm :SQLWriteForm[S] = writeForm(FILTER)

	/** Default write form (included columns) of update statements for this mapping.
	  * @return `writeForm(UPDATE)` (or a functionally equivalent instance) unless overriden. */
	override def updateForm :SQLWriteForm[S] = writeForm(UPDATE)

	/** Default write form (included columns) of insert statements for this mapping.
	  * @return `writeForm(INSERT)` (or a functionally equivalent instance) unless overriden. */
	override def insertForm :SQLWriteForm[S] = writeForm(INSERT)

	/** The delegate target of all properties with write forms of various statement types.
	  * @return `MappingWriteForm(op, this)` unless overriden. */
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = MappingWriteForm(op, this)


	/** @inheritdoc
	  * @return an empty sequence, unless overriden. */
	override def buffs :Seq[Buff[S]] = Nil


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

	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		AlteredMapping.update[BaseMapping[S, O], S, O](this, include, exclude)

	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		AlteredMapping.insert[BaseMapping[S, O], S, O](this, include, exclude)



	override def qualified(prefix :String) :Component[S] =
		if (prefix.length == 0) this else prefixed(prefix + ".")

	override def prefixed(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else PrefixedMapping[BaseMapping[S, O], S, O](prefix, this)



	override def inOption :Optional[this.type] = OptionMapping.singleton(this)

	override def as[X](there :Subject =?> X, back :X =?> Subject)(implicit nulls :NullValue[X] = null) :Component[X] =
		MappedMapping[BaseMapping[S, O], S, X, O](this, there, back)


	protected[oldsql] override def everyConcreteMappingMustExtendBaseMapping :Nothing =
		throw new UnsupportedOperationException
}






object BaseMapping {
	type * = M[O] forSome { type M[A] <: BaseMapping[_, A]; type O }

	type From[O] = BaseMapping[_, O]

	type Of[S] = BaseMapping[S, _]

	type AnyAt[O] = M[O] forSome { type M[A] <: BaseMapping[_, A] }

	type AnyOf[S] = M[S] forSome { type M[X] <: BaseMapping[X, _] }


	//note for the future: there is some problem with projections from mappings with Nothing as the origin type.
	@inline implicit def baseMappingOriginProjection[M[A] <: BaseMapping[S, A], S, O]
	                                                (implicit types :M[O] <:< BaseMapping[S, O])
			:ProjectionDef[M[O], M, S] =
		OriginProjection.isomorphism[M, S, O]

}






/** A `Mapping` subclass which, in its `optionally` (and indirectly `apply`) method, declares aliasing
  * of its components on the passed `Pieces`. Some mappings (and possibly their components) allow
  * declaring a column prefix to be added to all its columns, as well as additional buffs, which should be inherited
  * by all of its subcomponents (including columns), so a component, as defined, can be a different instance from its
  * final representation included on the mapping's component/column lists. As it is the latter version of the component
  * which is used by the framework to create any SQL statements, and thus also by the `Pieces`, but typically
  * the former is used in the assembly process, there is a need to introduce a mapping step in which the `Pieces`
  * implementation substitutes any component passed to it with its export representation before looking for its value.
  *
  */
trait RootMapping[S, O] extends BaseMapping[S, O] {

	override def optionally(pieces :Pieces) :Option[S] =
		super.optionally(pieces.aliased(this))

}




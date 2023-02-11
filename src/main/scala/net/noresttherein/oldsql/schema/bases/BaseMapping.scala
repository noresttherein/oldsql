package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.NaturalMap.WhenNoKey
import net.noresttherein.oldsql.collection.NaturalMap.WhenNoKey.Throw
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buffs, ColumnForm, ColumnMapping, Mapping, Seal}
import net.noresttherein.oldsql.schema.Buff.{SelectAudit, SelectDefault, SelectPreset}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.ProjectionDef
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.OptionMapping
import net.noresttherein.oldsql.schema.bits.OptionMapping.{Optional, OptionColumn}
import net.noresttherein.oldsql.schema.support.{AlteredMapping, BuffedMapping, ColumnMappingPrototype, MappedMapping, MappingPrototype, PatchedMapping, PrefixedMapping, RenamedMapping, ReorderedMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.support.MappingProxy.ExportColumnProxy
import net.noresttherein.oldsql.OperationView






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
trait BaseMapping[S, O] extends Mapping with MappingPrototype[({ type A[s] = TypedMapping[s, O] })#A, S, O] { self =>
	override type Origin = O
	override type Subject = S
	//for nicer compiler output
//	override type Extract[T] = MappingExtract[S, T, O]
//	override type ColumnExtract[T] = ColumnMappingExtract[S, T, O]
//	override type AnyComponent = MappingAt[O]
//	override type Component[T] = TypedMapping[T, O]
//	override type Column[T] = TypedColumn[T, O]


	//fixme: this does not use the export components of the root mapping!
	override def writtenValues[T](op :WriteOperationView, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		if (op.Prohibited.inactive(this)) {
			val audited = op.Audit.fold(this)(subject)
			def componentValues[X](comp :Component[X]) :Unit = {
				apply(comp).opt(audited) match { //fixme: when we accept Opt[S] instead of S move Default.Value check to the tested mapping itself.
					case Got(value) => collector.add(op, comp, value)//comp.writtenValues(op, value, collector)
					case _ => op.Default.Value(comp) match {
						case Got(value) => collector.add(op, comp, value) //comp.writtenValues(op, value, collector)
						case _ =>
					}
				}
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	/** Assures that extract maps created using standard factory methods will be throwing
	  * a [[net.noresttherein.oldsql.exceptions.NoSuchComponentException NoSuchComponentException]] with information
	  * about both the key component and this mapping, instead of the default `NoSuchElementException`.
	  */
	implicit protected def whenNoKeyThrowNoSuchComponentException[C[X] <: Component[X]] :WhenNoKey[C, Throw] =
		WhenNoKey.Throw.aNoSuchComponentException(this)


	override def apply(pieces: Pieces): S =
		optionally(pieces) match {
			case Got(result) => result
			case _ => throw new NoSuchElementException(s"Cannot assemble $this from $pieces.")
		}

	override def optionally(pieces: Pieces): Opt[S] = pieces.assemble(this) match {
		case res if buffs.isEmpty => res //a very common case
		case Got(res) => Got((res /: SelectAudit.Audit(this)) { (acc, f) => f(acc) })
		case _ =>
			val res = SelectDefault.Value(this)
			if (res.isDefined) res
			else SelectPreset.Value(this)
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


//	override def original :BaseMapping[S, O] = this



//	override def apply(first :ComponentSelection[_, O], rest :ComponentSelection[_, O]*) :TypedMapping[S, O] = {
//		val all = rest.view prepended first
//		apply(all collect { case IncludedComponent(c) => c }, all collect { case ExcludedComponent(c) => c })
//	}

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		AlteredMapping[BaseMapping[S, O], S, O](this, include, exclude)

	protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:Component[S] =
		PatchedMapping[BaseMapping[S, O], S, O](op, this, include, exclude)

//	override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
//		PatchedMapping.select[BaseMapping[S, O], S, O](this, include, exclude)
//
//	override def forFilter(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
//		PatchedMapping.filter[BaseMapping[S, O], S, O](this, include, exclude)
//
//	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
//		PatchedMapping.insert[BaseMapping[S, O], S, O](this, include, exclude)
//
//	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
//		PatchedMapping.update[BaseMapping[S, O], S, O](this, include, exclude)



//	override def qualified(prefix :String) :Component[S] =
//		if (prefix.length == 0) this else prefixed(prefix + ".")
//
//	override def prefixed(prefix :String) :Component[S] =
//		if (prefix.length == 0) this
//		else PrefixedMapping[BaseMapping[S, O], S, O](prefix, this)

	override def renamed(naming :String => String) :Component[S] =
		RenamedMapping[this.type, S, O](this :this.type, naming)

	override def reorder(permutation :IndexedSeq[Int]) :Component[S] =
		if (permutation.length != columns.size)
			throw new IllegalArgumentException(
				"Length of permutation " + permutation + " (" + permutation.length +
					") does not match the number of columns " + columns.size + " in " + this + ": " + columns + "."
			)
		else if (permutation == permutation.indices)
			this
		else
			ReorderedMapping[this.type, S, O](this :this.type, permutation)


	override def inOption :Optional[this.type] = OptionMapping.singleton(this)

	override def as[X](there :Subject =?> X, back :X =?> Subject)(implicit nulls :NullValue[X] = null) :Component[X] =
		MappedMapping[BaseMapping[S, O], S, X, O](this, there, back)


	private[schema] override def every_concrete_Mapping_must_extend_BaseMapping(seal :Seal) :Unit = ()
}






object BaseMapping {
	//note for the future: there is some problem with projections from mappings with Nothing as the origin type.
	@inline implicit def baseMappingOriginProjection[M[A] <: BaseMapping[S, A], S, O]
	                                                (implicit types :M[O] <:< BaseMapping[S, O])
			:ProjectionDef[M[O], M, S] =
		OriginProjection.isomorphism[M, S, O]

}






trait BaseColumn[S, O] extends BaseMapping[S, O]
	with ColumnMapping with ColumnMappingPrototype[({ type A[s] = TypedColumn[s, O]})#A, S, O]
{
	protected override def copy(name :String, buffs :Buffs[Subject]) :TypedColumn[Subject, Origin] =
		new ExportColumnProxy[Subject, Origin](this, name, buffs)

	protected override def thisColumn :TypedColumn[S, O] = this

	override def as[X](there :Subject =?> X, back :X =?> Subject)(implicit nulls :NullValue[X]) :TypedColumn[X, Origin] =
		MappedMapping.column[TypedColumn[Subject, Origin], Subject, X, Origin](refine, there, back)

	override def inOption :OptionColumn[this.type, S, O] = OptionMapping.column(this)
	final private[schema] override def every_concrete_ColumnMapping_must_extend_BaseColumn(seal :Seal) :Unit = ()
}


object BaseColumn {
	//note for the future: there is some problem with projections from mappings with Nothing as the origin type.
	@inline implicit def baseColumnOriginProjection[M[A] <: BaseColumn[S, A], S, O]
	                                               (implicit types :M[O] <:< BaseColumn[S, O])
			:ProjectionDef[M[O], M, S] =
		OriginProjection.isomorphism[M, S, O]

}


package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.support.{ConstantMapping, LazyMapping}
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, ComponentExtractor, MappingSchema, SchemaMapping}
import net.noresttherein.oldsql.schema.MappingSchema.{EmptySchema, FlatMappedMappingSchema, FlatMappingSchema, FlatNonEmptySchema, GetLabeledComponent, GetSchemaComponent, MappedFlatMappingSchema, MappedMappingSchema, NonEmptySchema, SchemaFlattening}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatSchemaMapping, LabeledSchemaColumn, MappedSchemaMapping, SchemaColumn}
import net.noresttherein.oldsql.schema.bits.ChainMapping.{BaseChainMapping, ChainPrefixSchema, NonEmptyChainMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy






trait ChainMapping[+C <: Chain, R <: Chain, O] extends BaseChainMapping[C, R, O] {

	override val schema :ChainMapping[C, R, O] = this

	/** Appends a new component to this schema. Full static type of the column will be encoded in the
	  * component chain of the returned schema.
	  */
	protected def append[M <: Subschema[_, _, T], T](component :M) :ChainMapping[C ~ M, R ~ T, O] =
		new NonEmptyChainMapping[C, M, R, T, O](this, component)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  */
	def comp[L <: Chain, V <: Chain, T](component :Subschema[L, V, T]) :ChainMapping[C ~ |*|[L, V, T], R ~ T, O] =
		new NonEmptyChainMapping[C, |*|[L, V, T], R, T, O](this, component)



	/** Appends a new column to this schema with the given name. */
	def col[T :ColumnForm](name :String, buffs :Buff[T]*) :ChainMapping[C ~ ||[T], R ~ T, O] =
		append[||[T], T](SchemaColumn[T, O](name, buffs :_*))



	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def lbl[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :ChainMapping[C ~ (N @|| T), R ~ T, O] =
		append[N @|| T, T](LabeledSchemaColumn[N, T, O](name, buffs:_*))

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def lbl[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*) :ChainMapping[C ~ (N @|| T), R ~ T, O] =
		append[N @|| T, T](LabeledSchemaColumn[N, T, O](label, name, buffs:_*))


	private[schema] def asPrefix[T] :MappingSchema[C, R, R ~ T, O] =
		new ChainPrefixSchema[C, R, R ~ T, O](this)

}






object ChainMapping {

	def apply[O] :FlatChainMapping[@~, @~, O] = empty.asInstanceOf[FlatChainMapping[@~, @~, O]]

	private[this] val empty = new EmptyChainMapping[Any]



	trait BaseChainMapping[+C <: Chain, R <: Chain, O]
		extends SchemaMapping[C, R, R, O] with MappingSchema[C, R, R, O]
	{
		override val schema = this

		override def apply[N <: Label, T]
		                  (label :N)(implicit get :GetLabeledComponent[C, R, LabeledMapping[N, T, O], N, T, O])
				:Selector[T] =
			super.apply(label)

		override def /[M <: LabeledMapping[N, T, O], N <: Label, T]
		              (label :N)(implicit get :GetLabeledComponent[C, R, M, N, T, O]) :M =
			super./[M, N, T](label)


		override def apply[I <: Numeral, T]
		                  (idx :I)(implicit get :GetSchemaComponent[C, R, Component[T], I, T, O])
				:ComponentExtractor[R, T, O] =
			super.apply[I, T](idx)

		override def /[M <: Component[T], I <: Numeral, T]
		              (idx :I)(implicit get :MappingSchema.GetSchemaComponent[C, R, M, I, T, O]) :M =
			super./[M, I, T](idx)




		def map[S](assemble :R => S, disassemble :S => R) :SchemaMapping[C, R, S, O] =
			new MappedMappingSchema[C, R, S, O](this compose disassemble, assemble)

//		def flatMap[S](assemble :R => Option[S])(disassemble :S => Option[R]) :MappingSchema[C, R, S, O] =
//			new FlatMappedMappingSchema[C, R, S, O](this compose disassemble, assemble)
	}






	trait FlatChainMapping[+C <: Chain, R <: Chain, O]
		extends ChainMapping[C, R, O] with FlatSchemaMapping[C, R, R, O] with FlatMappingSchema[C, R, R, O]
	{
		override val schema :FlatChainMapping[C, R, O] = this

		protected def col[M <: Subschema[_, _, T], T](component :M) :FlatChainMapping[C ~ M, R ~ T, O] =
			new NonEmptyFlatChainMapping[C, M, R, T, O](this, component)

		override def col[T :ColumnForm](name :String, buffs :Buff[T]*) :FlatChainMapping[C ~ ||[T], R ~ T, O] =
			col[||[T], T](SchemaColumn[T, O](name, buffs:_*))

		override def lbl[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :ChainMapping[C ~ (N @|| T), R ~ T, O] =
			col[N @|| T, T](LabeledSchemaColumn(name, buffs:_*))



		override def map[S](assemble :R => S, disassemble :S => R) :FlatSchemaMapping[C, R, S, O] =
			new MappedFlatMappingSchema[C, R, S, O](this compose disassemble, assemble)



		private[schema] override def asPrefix[T] :FlatMappingSchema[C, R, R ~ T, O] =
			new FlatChainPrefixSchema[C, R, R ~ T, O](this)
	}






	private def prefix[C <: Chain] :(C ~ Any) => C = init.asInstanceOf[(C ~ Any) => C]
	private[this] val init = (_:Chain ~ Any).init



	private class EmptyChainMapping[O] extends EmptySchema[@~, O] with FlatChainMapping[@~, @~, O]



	private class NonEmptyChainMapping[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, O]
	                                  (prefix :ChainMapping[C, R, O], next :M)
		extends NonEmptySchema[C, M, R, T, R ~ T, O](
				               prefix.asPrefix[T], next, ComponentExtractor.req(next)((row :R ~ T) => row.last))
		   with ChainMapping[C ~ M, R ~ T, O]



	private class NonEmptyFlatChainMapping[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, O]
	                                      (prefix :FlatChainMapping[C, R, O], next :M)
		extends FlatNonEmptySchema[C, M, R, T, R ~ T, O](
		                       prefix.asPrefix[T], next, ComponentExtractor.req(next)((row :R ~ T) => row.last))
		   with FlatChainMapping[C ~ M, R ~ T, O]





	/** Adapts a `ChainMapping` to a `MappingSchema` of a `Chain` longer by one entry. */
	private[schema] class ChainPrefixSchema[+C <: Chain, R <: Chain, S <: R ~ Any, O]
	                                       (protected val egg :MappingSchema[C, R, R, O])
		extends MappingSchema[C, R, S, O] with ShallowProxy[R, O]
	{

		override def members :C = egg.members

		override def extractor[X](component :Component[X]) :ComponentExtractor[S, X, O] =
			egg.extractor(component) compose prefix[R]

		override def unapply(subject :S) :Option[R] = Some(subject.init)

		override def disassemble(subject :S) :R = subject.init

		override def compose[X](extractor :X => S) :MappingSchema[C, R, X, O] =
			egg compose (extractor andThen prefix[R])

		override def compose[X](extractor :X =?> S) :MappingSchema[C, R, X, O] =
			egg compose (extractor andThen prefix[R])

		override protected[schema] def schemaExtractors :List[(Component[_], Selector[_])] = egg.schemaExtractors

		override protected[schema] def ownerExtractors :List[(Component[_], ComponentExtractor[S, _, O])] =
			egg.ownerExtractors map { case (c, ex) =>
				c -> ComponentExtractor[S, Any, O](ex.export.asInstanceOf[Component[Any]])(ex.asInstanceOf[R =?> Any] compose prefix[R])
			}


		override protected[schema] def componentsReversed :List[Component[_]] = egg.componentsReversed

		override protected[schema] def subcomponentsReversed :List[Component[_]] = egg.subcomponentsReversed

		override protected[schema] def columnsReversed :List[Component[_]] = egg.columnsReversed

		override def optionally(pieces :Pieces) :Option[R] = egg.optionally(pieces.compatible(egg))

	}



	private[schema] class FlatChainPrefixSchema[+C <: Chain, R <: Chain, S <: R ~ Any, O]
	                                           (protected override val egg :FlatMappingSchema[C, R, R, O])
		extends ChainPrefixSchema[C, R, S, O](egg) with FlatMappingSchema[C, R, S, O]
	{
		override def compose[X](extractor :X => S) :FlatMappingSchema[C, R, X, O] =
			egg compose (extractor andThen prefix[R])

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[C, R, X, O] =
			egg compose (extractor andThen prefix[R])
	}


}



package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{Chain, LiteralIndex}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication}
import net.noresttherein.oldsql.collection.LiteralIndex.{:~, |~}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.MappingSchema.{BaseNonEmptySchema, EmptySchema, FlatMappingSchema, FlatMappedMappingSchema, MappedMappingSchema}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.ColumnMapping.BaseColumn
import net.noresttherein.oldsql.schema.IndexedMappingSchema.{FlatIndexedMappingSchema, FlatMappedIndexMappingSchema, MappedIndexedMappingSchema, NonEmptyIndexedSchema}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatSchemaMapping, LabeledSchemaColumn}
import net.noresttherein.oldsql.schema.IndexedSchemaMapping.FlatIndexedSchemaMapping




/** A [[net.noresttherein.oldsql.schema.MappingSchema MappingSchema]] variant where all components are indexed
  * with `String` literals for access in
  * @author Marcin Mościcki
  */
trait IndexedMappingSchema[+C <: Chain, R <: LiteralIndex, S, O] extends MappingSchema[C, R, S, O] {


	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value an extractor returning the value of this component for the subject type `S` of an owning mapping.
	  */
	def comp[N <: Label :ValueOf, L <: Chain, V <: Chain, T](component :LabeledSubschema[N, L, V, T], value :Extractor[S, T])
			:IndexedMappingSchema[C ~ @|*|[N, L, V, T], R |~ (N :~ T), S, O] =
		new NonEmptyIndexedSchema[N, C, @|*|[N, L, V, T], R, T, S, O](
			this, component, ComponentExtractor(component, value.optional, value.requisite)
		)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
	  */
	def comp[N <: Label :ValueOf, L <: Chain, V <: Chain, T](value :S => T, component :LabeledSubschema[N, L, V, T])
			:IndexedMappingSchema[C ~ @|*|[N, L, V, T], R |~ (N :~ T), S, O] =
		new NonEmptyIndexedSchema[N, C, @|*|[N, L, V, T], R, T, S, O](
			this, component, ComponentExtractor.req(component)(value)
		)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
	  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
	  *              of the component's columns.
	  */
	def optcomp[N <: Label :ValueOf, L <: Chain, V <: Chain, T](value :S => Option[T], component :LabeledSubschema[N, L, V, T])
			:IndexedMappingSchema[C ~ @|*|[N, L, V, T], R |~ (N :~ T), S, O] =
		new NonEmptyIndexedSchema[N, C, @|*|[N, L, V, T], R, T, S, O](
			this, component, ComponentExtractor.opt(component)(value)
		)



	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value an extractor returning the value of this component for the subject type `S` of an owning mapping.
	  */
	def comp[N <: Label, L <: Chain, V <: Chain, T](label :N, component :Subschema[L, V, T], value :Extractor[S, T])
			:IndexedMappingSchema[C ~ @|*|[N, L, V, T], R |~ (N :~ T), S, O] =
		new NonEmptyIndexedSchema[N, C, @|*|[N, L, V, T], R, T, S, O](
			this, label @: component, ComponentExtractor(component, value.optional, value.requisite)
		)(new ValueOf[N](label))

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
	  */
	def comp[N <: Label, L <: Chain, V <: Chain, T](label :N, value :S => T, component :Subschema[L, V, T])
			:IndexedMappingSchema[C ~ @|*|[N, L, V, T], R |~ (N :~ T), S, O] =
		new NonEmptyIndexedSchema[N, C, @|*|[N, L, V, T], R, T, S, O](
			this, label @: component, ComponentExtractor.req(component)(value)
		)(new ValueOf(label))

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
	  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
	  *              of the component's columns.
	  */
	def optcomp[N <: Label, L <: Chain, V <: Chain, T](label :N, value :S => Option[T], component :Subschema[L, V, T])
			:IndexedMappingSchema[C ~ @|*|[N, L, V, T], R |~ (N :~ T), S, O] =
		new NonEmptyIndexedSchema[N, C, @|*|[N, L, V, T], R, T, S, O](
			this, label @: component, ComponentExtractor.opt(component)(value)
		)(new ValueOf(label))



	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
			:IndexedMappingSchema[C ~ (N @|| T), R |~ (N :~ T), S, O] =
		col(name, name, value, buffs:_*)

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
			:IndexedMappingSchema[C ~ (N @|| T), R |~ (N :~ T), S, O] =
	{
		val column = LabeledSchemaColumn[N, T, O](label, name, buffs:_*)
		new NonEmptyIndexedSchema[N, C, N @|| T, R, T, S, O](
			this, column, ComponentExtractor(column)(Extractor.req(value))
		)(new ValueOf(label))
	}

	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def optcol[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
			:IndexedMappingSchema[C ~ (N @|| T), R |~ (N :~ T), S, O] =
		optcol[N, T](name, name, value, buffs :_*)

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label a string literal used to access the column in the schema.
	  * @param name the name of the column.
	  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def optcol[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
			:IndexedMappingSchema[C ~ (N @|| T), R |~ (N :~ T), S, O] =
	{
		val column = LabeledSchemaColumn[N, T, O](label, name, buffs:_*)
		new NonEmptyIndexedSchema[N, C, N @|| T, R, T, S, O](
			this, column, ComponentExtractor(column)(Extractor(value))
		)(new ValueOf(label))
	}






	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema.
	  * @param constructor a function accepting a chain with the values of all components as they appear in the
	  *                    components chain `C`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.flatMap]]
	  */
	override def map(constructor :R => S) :IndexedSchemaMapping[C, R, S, O] =
		new MappedIndexedMappingSchema[C, R, S, O](this, constructor)

	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema. This will result in slightly more efficient
	  * assembly than the other overloaded `map` method, as no chain with the values of all components will be assembled
	  * as an intermediate step.
	  * @param constructor a function which number of arguments and their types match the subject types of all
	  *                    components as listed by the chain `R`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.flatMap]]
	  */
	override def map[F](constructor :F)(implicit apply :ChainApplication[R, F, S]) :IndexedSchemaMapping[C, R, S, O] =
		map { v :R => v.feedTo(constructor) }



	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may
	  * not produce the subject value for all input rows.
	  * @param constructor a function accepting a chain with the values of all components as they appear in the
	  *                    components chain `C`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.flatMap]]
	  */
	override def flatMap(constructor :R => Option[S]) :IndexedSchemaMapping[C, R, S, O] =
		new FlatMappedIndexMappingSchema[C, R, S, O](this, constructor)

	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may not produce
	  * the subject value for all input rows. This will result in slightly more efficient assembly than the other
	  * overloaded `flatMap` method, as no chain with the values of all components will be assembled as an intermediate step.
	  * @param constructor a function which number of arguments and their types match the subject types of all
	  *                    components as listed by the chain `R`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.flatMap]]
	  */
	override def flatMap[F](constructor :F)(implicit apply :ChainApplication[R, F, Option[S]]) :IndexedSchemaMapping[C, R, S, O] =
		flatMap { row :R => row.feedTo(constructor) }



	override def compose[X](extractor :X => S) :IndexedMappingSchema[C, R, X, O]

	override def compose[X](extractor :X =?> S) :IndexedMappingSchema[C, R, X, O]

}






object IndexedMappingSchema {

	def apply[S, O] :FlatIndexedMappingSchema[@~, @~, S, O] = empty.asInstanceOf[EmptyIndexedSchema[S, O]]

	private[this] val empty = new EmptyIndexedSchema[Any, Any]



	trait FlatIndexedMappingSchema[+C <: Chain, R <: LiteralIndex, S, O]
		extends FlatMappingSchema[C, R, S, O] with IndexedMappingSchema[C, R, S, O]
	{ outer =>

		override def col[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
				:FlatIndexedMappingSchema[C ~ (N @|| T), R |~ (N :~ T), S, O] =
			col[N, T](name, name, value, buffs :_*)

		override def col[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
				:FlatIndexedMappingSchema[C ~ (N @|| T), R |~ (N :~ T), S, O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, name, buffs:_*)
			new FlatNonEmptyIndexedSchema[N, C, N @|| T, R, T, S, O](
				this, column, ComponentExtractor(column)(Extractor.req(value))
			)(new ValueOf(label))
		}



		override def optcol[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
				:FlatIndexedMappingSchema[C ~ (N @|| T), R |~ (N :~ T), S, O] =
			optcol(name, name, value, buffs:_*)

		override def optcol[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
				:FlatIndexedMappingSchema[C ~ (N @|| T), R |~ (N :~ T), S, O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, name, buffs:_*)
			new FlatNonEmptyIndexedSchema[N, C, N @|| T, R, T, S, O](
				this, column, ComponentExtractor(column)(Extractor(value))
			)(new ValueOf(label))
		}



		override def map(constructor :R => S) :FlatIndexedSchemaMapping[C, R, S, O] =
			new MappedIndexedMappingSchema[C, R, S, O](this, constructor) with FlatIndexedSchemaMapping[C, R, S, O] {
				override val schema = outer
			}

		override def map[F](constructor :F)(implicit apply :ChainApplication[R, F, S]) :FlatIndexedSchemaMapping[C, R, S, O] =
			map { row :R => row.feedTo(constructor) }

		override def flatMap(constructor :R => Option[S]) :FlatIndexedSchemaMapping[C, R, S, O] =
			new FlatMappedMappingSchema[C, R, S, O](this, constructor) with FlatIndexedSchemaMapping[C, R, S, O] {
				override val schema = outer
			}

		override def flatMap[F](constructor :F)(implicit apply :ChainApplication[R, F, Option[S]]) :FlatIndexedSchemaMapping[C, R, S, O] =
			flatMap { row :R => row feedTo constructor }


		override def compose[X](extractor :X => S) :FlatIndexedMappingSchema[C, R, X, O]

		override def compose[X](extractor :X =?> S) :FlatIndexedMappingSchema[C, R, X, O]

	}






	private[schema] class EmptyIndexedSchema[S, O] extends EmptySchema[S, O] with FlatIndexedMappingSchema[@~, @~, S, O] {
		override def compose[X](extractor :X => S) :EmptyIndexedSchema[X, O] = this.asInstanceOf[EmptyIndexedSchema[X, O]]

		override def compose[X](extractor :X =?> S) :EmptyIndexedSchema[X, O] = this.asInstanceOf[EmptyIndexedSchema[X, O]]
	}



	private[schema] class NonEmptyIndexedSchema[N <: Label :ValueOf, +C <: Chain, +M <: TypedMapping[T, O],
	                                            R <: LiteralIndex, T, S, O]
	                                           (override val init :IndexedMappingSchema[C, R, S, O],
	                                            component :M, selector :ComponentExtractor[S, T, O])
		extends BaseNonEmptySchema[LiteralIndex, |~, Label :~ Any, C, M, R, T, N :~ T, S, O](
		                           init, component, selector, _.last.value)
		   with IndexedMappingSchema[C ~ M, R |~ (N :~ T), S, O]
	{
		protected val label :N = valueOf[N]

		protected override def link(init :R, last :T) :R |~ (N :~ T) = init |~ label :~ last

		override def compose[X](extractor :X => S) :NonEmptyIndexedSchema[N, C, M, R, T, X, O] =
			new NonEmptyIndexedSchema[N, C, M, R, T, X, O](init compose extractor, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :NonEmptyIndexedSchema[N, C, M, R, T, X, O] =
			new NonEmptyIndexedSchema[N, C, M, R, T, X, O](init compose extractor, last, this.extractor compose extractor)
	}



	private[schema] class FlatNonEmptyIndexedSchema[N <: Label :ValueOf, +C <: Chain, +M <: TypedMapping[T, O],
	                                                R <: LiteralIndex, T, S, O]
	                                               (override val init :FlatIndexedMappingSchema[C, R, S, O],
	                                                last :M, extractor :ComponentExtractor[S, T, O])
		extends NonEmptyIndexedSchema[N, C, M, R, T, S, O](init, last, extractor)
		   with FlatIndexedMappingSchema[C ~ M, R |~ (N :~ T), S, O]
	{
		override def compose[X](extractor :X => S) :FlatNonEmptyIndexedSchema[N, C, M, R, T, X, O] =
			new FlatNonEmptyIndexedSchema[N, C, M, R, T, X, O](init compose extractor, last,
			                                                   this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :FlatNonEmptyIndexedSchema[N, C, M, R, T, X, O] =
			new FlatNonEmptyIndexedSchema[N, C, M, R, T, X, O](init compose extractor, last,
			                                                   this.extractor compose extractor)
	}






	private[schema] class MappedIndexedMappingSchema[+C <: Chain, R <: LiteralIndex, S, O]
	                      (override val schema :IndexedMappingSchema[C, R, S, O], constructor :R => S)
		extends MappedMappingSchema[C, R, S, O](schema, constructor) with IndexedSchemaMapping[C, R, S, O]



	private[schema] class FlatMappedIndexMappingSchema[+C <: Chain, R <: LiteralIndex, S, O]
	                      (override val schema :IndexedMappingSchema[C, R, S, O], constructor :R => Option[S])
		extends FlatMappedMappingSchema[C, R, S, O](schema, constructor) with IndexedSchemaMapping[C, R, S, O]


}






trait IndexedSchemaMapping[+C <: Chain, I <: LiteralIndex, S, O] extends SchemaMapping[C, I, S, O] {
	override val schema :IndexedMappingSchema[C, I, S, O]
}






object IndexedSchemaMapping {
	@inline def apply[S, O] :FlatIndexedMappingSchema[@~, @~, S, O] = IndexedMappingSchema[S, O]


	trait FlatIndexedSchemaMapping[+C <: Chain, I <: LiteralIndex, S, O]
		extends IndexedSchemaMapping[C, I, S, O] with FlatSchemaMapping[C, I, S, O]
	{
		override val schema :FlatIndexedMappingSchema[C, I, S, O]
	}

}






abstract class AbstractIndexedSchemaMapping[+C <: Chain, I <: LiteralIndex, S, O]
                                           (contents :IndexedMappingSchema[C, I, S, O])
	extends AbstractSchemaMapping[C, I, S, O](contents) with IndexedSchemaMapping[C, I, S, O]
{
	implicit override val schema :IndexedMappingSchema[C, I, S, O] = contents
}



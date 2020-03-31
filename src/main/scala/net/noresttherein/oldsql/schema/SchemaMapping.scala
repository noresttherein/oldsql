package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{Chain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication}
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentExtractor}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, AdaptedAs, ShallowAdapter}
import net.noresttherein.oldsql.schema.support.{ConstantMapping, EmptyMapping, LazyMapping, MappedMapping, MappingAdapter}
import net.noresttherein.oldsql.schema.MappingSchema.{Empty, MappingSchemaGuts, MultiComponentSchema}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatMappedSchemaMapping, MappedSchemaMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.support.MappedMapping.FlatMappedMapping
import net.noresttherein.oldsql.schema.ColumnMapping.LabeledColumn

import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag


/** A `Mapping` for type `S` which has the types of all of its components encoded as a type parameter `C`.
  * While this makes the types of larger mappings quite verbose and inconvenient for passing explicitly,
  * it has a couple of advantages over implementations without that feature:
  *   - they can be created in a vey succinct way using the chained call building process:
  *     {{{
  *         case class Gun(make :String, model :String, caliber :Double)
  *         case class Human(gun :Gun, backup :Gun, secondBackup :Gun)
  *
  *         def GunSchema[O] = MappingSchema[O, Gun].col(_.make).col(_.model).col(_.caliber).map(Gun.apply)
  *         def HumanSchema[O] = MappingSchema[O, Human].comp(_.gun, GunSchema).comp(_.backup, GunSchema)
  *                                                     .comp(_.secondBackup, GunSchema).map(Human.apply)
  *     }}}
  *
  *   - they can be used in a type safe way to map rows of arbitrary select statements:
  *     {{{
  *         case class Human(gender :String, height :Int, weight :Int)
  *         def HumanSchema[O] = MappingSchema[O, Human].col(_.gender).col(height).col(_.weight).map(Human.apply)
  *         //can be used for the result of "select gender, avg(height), avg(weight) from humans group by gender"
  *     }}}
  *
  * There are two basic ways of creating a `SchemaMapping`:
  *   - start with building a `MappingSchema` and map the result, as in the examples above;
  *   - extend `AbstractSchemaMapping` and implement `construct`, having access to all components of the schema.
  *
  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order.
  * @tparam O a label type serving to distinguish statically between mappings of the same class but mapping
  *           different fragments of a `ResultSet` when more than one copy is present.
  * @tparam R a `Chain` containing the types of all components in `C` in their exact order, forming a 'row schema'.
  * @tparam S the subject type of this mapping.
  *
  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
  * @see [[net.noresttherein.oldsql.schema.AbstractSchemaMapping]]
  * @author Marcin Mościcki
  */
trait SchemaMapping[+C <:Chain, O, R <: Chain, S] extends GenericMapping[O, S] {
	val schema :MappingSchema[C, O, R, S]

	override def apply[T](component :Component[T]) :Selector[T] =
		if (component eq schema)
			ComponentExtractor.req(schema)(schema.disassemble _).asInstanceOf[Selector[T]]
		else schema.extractor(component)



	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :SchemaMapping[C, O, R, X] =
		new MappedSchemaMapping[C, O, R, S, X](this, there, back)

	override def flatMap[X](there :S => Option[X], back :X => Option[S])
	                       (implicit nulls :NullValue[X]) :SchemaMapping[C, O, R, X] =
		new FlatMappedSchemaMapping[C, O, R, S, X](this, there, back)
}





object SchemaMapping {

	private class MappedSchemaMapping[+C <: Chain, O, R <: Chain, S, T](override val egg :SchemaMapping[C, O, R, S],
	                                                        override val map :S => T, override val unmap :T => S)
	                                                       (implicit override val nulls :NullValue[T])
		extends MappedMapping[SchemaMapping[C, O, R, S], O, S, T] with MappingAdapter[SchemaMapping[C, O, R, S], O, T]
			with SchemaMapping[C, O, R, T]
	{
		override val schema :MappingSchema[C, O, R, T] = egg.schema compose unmap

		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :MappedSchemaMapping[C, O, R, S, X] =
			new MappedSchemaMapping[C, O, R, S, X](egg, map andThen there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedSchemaMapping[C, O, R, S, X] =
			new FlatMappedSchemaMapping[C, O, R, S, X](egg, map andThen there, back(_) map unmap)(flatMapNulls(there))
	}



	private class FlatMappedSchemaMapping[+C <: Chain, O, R <: Chain, S, T]
	                                     (mapping :SchemaMapping[C, O, R, S], assemble :S => Option[T], disassemble :T => Option[S])
	                                     (implicit override val nulls :NullValue[T])
		extends FlatMappedMapping[SchemaMapping[C, O, R, S], O, S, T](mapping, assemble, disassemble, nulls)
		   with MappingAdapter[SchemaMapping[C, O, R, S], O, T] with SchemaMapping[C, O, R, T]
	{
		override val schema :MappingSchema[C, O, R, T] = egg.schema compose Extractor(unmap)

		override def map[X](there :T => X, back :X => T)
		                   (implicit nulls :NullValue[X]) :FlatMappedSchemaMapping[C, O, R, S, X] =
			new FlatMappedSchemaMapping[C, O, R, S, X](egg, map(_) map there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedSchemaMapping[C, O, R, S, X] =
			new FlatMappedSchemaMapping[C, O, R, S, X](egg, map(_) flatMap there, back(_) flatMap unmap)(flatMapNulls(there))
	}


}






/** Base trait for `SchemaMapping` implementations which need individual access to their components during
  * the construction process. This class extends `StaticMapping`, meaning that within its
  * [[net.noresttherein.oldsql.schema.StaticMapping#construct construct(Pieces)]] method components are implicitly
  * converted into their values, allowing their direct use as arguments for the subject's constructor.
  * Note that accepting a `MappingSchema` as the parameter, all type parameters of this class can be usually inferred
  * automatically:
  * {{{
  *     case class Human(favoritePizza :String, agricolaRecord :Int)
  *     class Humans[O] extends AbstractSchemaMapping(
  *             MappingSchema[O, Human].col(_.favoritePizza).col(_.agricolaRecord)
  *     ){
  *         override def construct(implicit pieces :Pieces) :Human =
  *             Human(schema.prev(), schema.last)
  *     }
  * }}}
  *
  *
  * @param schema the schema listing all components of this mapping.
  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order.
  * @tparam O a label type serving to distinguish statically between mappings of the same class but mapping
  *           different fragments of a `ResultSet`, when more than one copy is present.
  * @tparam R a `Chain` containing the types of all components in `C` in their exact order, forming a 'row schema'.
  * @tparam S the subject type of this mapping.
  *
  */
abstract class AbstractSchemaMapping[+C <: Chain, O, R <: Chain, S](override val schema :MappingSchema[C, O, R, S])
	extends SchemaMapping[C, O, R, S] with StaticMapping[O, S] with LazyMapping[O, S]
{
	override val components :Unique[Component[_]] = schema.components
	override val subcomponents :Unique[Component[_]] = schema.subcomponents
	override val columns :Unique[Component[_]] = schema.columns
}






/** A list of components of some `SchemaMapping` for subject type `Ś`, with all their types encoded in this class's type.
  * Each component of type `T` additionally has a `ComponentExtractor[O, S, T]` associated with it by this instance,
  * which can be accessed using the [[net.noresttherein.oldsql.schema.MappingSchema#extractor extractor(component)]]
  * method. A schema itself is a mapping for the chain `R` containing the values of all of its components in order,
  * but is more typically used as the basis of a `SchemaMapping` instance for some entity type `S`.
  * This can happen either by directly mapping the chain of values `R` with its
  * [[net.noresttherein.oldsql.schema.MappingSchema#map map]] method, or indirectly in an enclosing mapping's
  * `assemble` method, where components in this schema can be individually accessed, without constructing
  * an intermediate chain of values. There is an automatically available implicit conversion from non-empty
  * schemas (where `C` and `R` are not empty) which add methods for retrieving its components:
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaComponentAccessor#last last]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaComponentAccessor#apply apply()]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaComponentAccessor#prev prev]].
  *
  * @tparam C a `Chain` listing the types of all components in this schema.
  * @tparam O a label type serving to distinguish statically between mappings of the same class but mapping
  *           different fragments of a `ResultSet`, when more than one copy is present.
  * @tparam R a `Chain` containing the subject types of all components in the chain `C`, which is the subject type
  *           of this mapping.
  * @tparam S the entity type of an owning `SchemaMapping`.
  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
  */
trait MappingSchema[+C <: Chain, O, R <: Chain, S] extends GenericMapping[O, R] {
	outer :MappingSchemaGuts[C, O, R, S] =>

	/** A shorthand alias for `ColumnMapping[O, T]` allowing reduced notation in the component type chain. */
	type ||[T] = ColumnMapping[O, T]

	def schema :C

	def comp[M  <: Component[T], T](value :S => T, component :M) :MappingSchema[C ~ M, O, R ~ T, S] =
		new MultiComponentSchema[C, M, O, R, S, T](this, component, ComponentExtractor.req(component)(value))

	def optcomp[M <: Component[T], T](value :S => Option[T], component :M) :MappingSchema[C ~ M, O, R ~ T, S] =
		new MultiComponentSchema[C, M, O, R, S, T](this, component, ComponentExtractor.opt(component)(value))

	def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*) :MappingSchema[C ~ ||[T], O, R ~ T, S] =
		comp(value, ColumnMapping[O, T](name, buffs:_*))

	def col[T :ColumnForm](value :S => T, buffs :Buff[T]*)(implicit tpe :TypeTag[S])
			:MappingSchema[C ~ ||[T], O, R ~ T, S] =
		col(PropertyPath.nameOf(value), value, buffs :_*)

	def optcol[T :ColumnForm](name :String, value :S => Option[T], buffs :Buff[T]*)
			:MappingSchema[C ~ ||[T], O, R ~ T, S] =
		optcomp(value, ColumnMapping[O, T](name, buffs:_*))

	def optcol[T :ColumnForm](value :S => Option[T], buffs :Buff[T]*)(implicit tpe :TypeTag[S])
			:MappingSchema[C ~ ||[T], O, R ~ T, S] =
		optcol(PropertyPath.nameOf(value), value, buffs :_*)


/*
	def comp[N <: Label, M <: Component[T], T](label :N, value :S => T, component :M)
			:MappingSchema[C ~ (N @: M), O, R ~ T, S] =
		comp(value, label @: component)

	def optcomp[N <: Label, M <: Component[T], T](label :N, value :S => Option[T], component :M)
			:MappingSchema[C ~ (N @: M), O, R ~ T, S] =
		optcomp(value, label @: component)

	def lbl[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
			:MappingSchema[C ~ LabeledColumn[N, O, T], O, R ~ T, S] =
		comp(value, ColumnMapping.labeled[N, O, T](name, buffs:_*))

	def optlbl[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
			:MappingSchema[C ~ LabeledColumn[N, O, T], O, R ~ T, S] =
		optcomp(value, ColumnMapping.labeled[N, O, T](name, buffs:_*))
*/



	def map(constructor :R => S) :SchemaMapping[C, O, R, S] =
		new ShallowAdapter[Component[R], O, R, S] with SchemaMapping[C, O, R, S] {
			override protected val egg = outer
			override val schema = outer
			private[this] val schemaExtractor = ComponentExtractor.opt(schema)(schema.unapply)

			override def apply[T](component :Component[T]) =
				if (component eq schema) schemaExtractor.asInstanceOf[Selector[T]]
				else schema.extractor(component)

			override def assemble(pieces :Pieces) =
				pieces.get(schemaExtractor) map constructor
		}

	def map[F](constructor :F)(implicit apply :ChainApplication[R, F, S]) :SchemaMapping[C, O, R, S] =
		map { v :R => v.feedTo(constructor) }



	def compose[X](extractor :X => S) :MappingSchema[C, O, R, X]

	def compose[X](extractor :X =?> S) :MappingSchema[C, O, R, X]

	def extractor[X](component :Component[X]) :ComponentExtractor[O, S, X]

	def unapply(subject :S) :Option[R]

	def disassemble(subject :S) :R


}






object MappingSchema {


	def apply[O, S] :MappingSchema[@~, O, @~, S] = Empty[O, S]



	/** Methods allowing positional accept of the components listed on `I ~ L` by a `MappingSchema`.  */
	implicit class MappingSchemaComponentAccessor[I <: Chain, L <: Component[O, T], O, R <: Chain, S, T]
	                                             (private val self :MappingSchema[I ~ L, O, R ~ T, S]) extends AnyVal
	{
		/** The last component on the list - same as `last` but more readable in code like `schema.prev.prev()`. */
		def apply() :L = last

		/** The last component on the list. */
		def last :L = self.asInstanceOf[MultiComponentSchema[I, L, O, R, S, T]].last

		/** The schema for the chain `I`, containing all components of this schema except for the last one. */
		def prev :MappingSchema[I, O, R, S] = self.asInstanceOf[MultiComponentSchema[I, L, O, R, S, T]].init
	}






	private[MappingSchema] trait MappingSchemaGuts[+C <: Chain, O, R <: Chain, S] extends MappingSchema[C, O, R, S] {

		def selectorsList :List[(Component[_], Selector[_])]

		def extractorsList :List[(Component[_], ComponentExtractor[O, S, _])]

		override def compose[X](extractor :X => S) :MappingSchemaGuts[C, O, R, X]
		override def compose[X](extractor :X =?> S) :MappingSchemaGuts[C, O, R, X]
	}



	private class EmptyComponent[O, S] extends ConstantMapping[O, @~](@~) with MappingSchemaGuts[@~, O, @~, S] {
		override def schema: @~ = @~

		private[this] val extractor :ComponentExtractor[O, S, @~] = ComponentExtractor.const(this)(@~)

		override def extractor[X](component :Component[X]) :ComponentExtractor[O, S, X] =
			if (component eq this)
				extractor.asInstanceOf[ComponentExtractor[O, S, X]]
			else
				throw new IllegalArgumentException("Component $component is not a part of this empty mapping schema.")

		override def unapply(subject :S): Option[@~] = Some(@~)

		override def disassemble(subject :S): @~ = @~

		override def compose[X](extractor :X => S) :EmptyComponent[O, X] =
			this.asInstanceOf[EmptyComponent[O, X]]

		override def compose[X](extractor :X =?> S) :EmptyComponent[O, X] =
			this.asInstanceOf[EmptyComponent[O, X]]

		override def selectorsList :Nil.type = Nil

		override def extractorsList :Nil.type = Nil
	}



	object Empty {
		private[this] val empty = new EmptyComponent[Any, Any]

		def apply[O, S] :MappingSchema[@~, O, @~, S] = empty.asInstanceOf[EmptyComponent[O, S]]

		def unapply(schema :MappingSchema[_, _, _, _]) :Boolean = schema.isInstanceOf[EmptyComponent[_, _]]
	}






	private class MultiComponentSchema[+C <: Chain, +M <: Component[O, T], O, R <: Chain, S, T]
	                          (val init :MappingSchemaGuts[C, O, R, S], val last :M, val extractor :ComponentExtractor[O, S, T])
		extends MappingSchemaGuts[C ~ M, O, R ~ T, S] with LazyMapping[O, R ~ T]
	{
		override def schema :C ~ M = init.schema ~ last

		override def unapply(subject :S) :Option[R ~ T] =
			for (i <- init.unapply(subject); l <- extractor.get(subject)) yield i ~ l

		override def disassemble(subject :S) :R ~ T = init.disassemble(subject) ~ extractor(subject)

		override def assemble(pieces :Pieces) :Option[R ~ T] =
			for (i <- pieces.get(initSelector); l <- pieces.get(lastSelector))
				yield i ~ l

		override def compose[X](extractor :X => S) :MultiComponentSchema[C, M, O, R, X, T] =
			new MultiComponentSchema[C, M, O, R, X, T](init compose extractor, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :MultiComponentSchema[C, M, O, R, X, T] =
			new MultiComponentSchema[C, M, O, R, X, T](init compose extractor, last, this.extractor compose extractor)



		private[this] val extractors = Lazy(extractorsList.toMap)

		override def extractorsList :List[(init.Component[_], ComponentExtractor[O, S, _])] =
			(last -> extractor) :: (init -> ComponentExtractor.opt(init)(init.unapply)) ::
				last.subcomponents.toList.map { comp => (comp, last(comp) compose extractor) } reverse_:::
				init.extractorsList

		override def extractor[X](component :Component[X]) :ComponentExtractor[O, S, X] =
			extractors.get(component).asInstanceOf[ComponentExtractor[O, S, X]]



		private[this] val selectors = Lazy(selectorsList.toMap)
		private[this] val initSelector = ComponentExtractor.req(init) { vs :(R ~ T) => vs.init }
		private[this] val lastSelector = ComponentExtractor.req(last) { vs :(R ~ T) => vs.last }

		override def selectorsList :List[(Component[_], Selector[_])] =
			(last -> lastSelector) :: (init, initSelector) ::
				last.subcomponents.toList.map { comp => (comp, last(comp) compose lastSelector) } reverse_:::
				init.selectorsList.map { case (comp, sel) => (comp, sel compose initSelector) }

		override def apply[X](component :Component[X]) :Selector[X] =
			selectors.get(component).asInstanceOf[Selector[X]]



		override val components :Unique[Component[_]] = Unique[Component[_]](init, last)

		override val subcomponents :Unique[Component[_]] = {
			@tailrec def rec(schema :MappingSchema[_, O, _, _], subcomponents :List[Component[_]]) :List[Component[_]] =
				schema match {
					case multi :MultiComponentSchema[_, _, O, _, _, _] =>
						rec(multi.init, multi.init :: multi.last.subcomponents.toList ::: multi.last :: subcomponents)
					case _ => subcomponents
				}
			Unique.later(Unique.from(rec(this, Nil)))
		}

		override val columns :Unique[Component[_]] = {
			@tailrec def rec(schema :MappingSchema[_, O, _, _], columns :List[Component[_]]) :List[Component[_]] =
				schema match {
					case multi :MultiComponentSchema[_, _, O, _, _, _] =>
						rec(multi.init, multi.last.columns.toList ::: columns)
					case _ => columns
				}
			Unique.later(Unique.from(rec(this, Nil)))
		}


	}

}



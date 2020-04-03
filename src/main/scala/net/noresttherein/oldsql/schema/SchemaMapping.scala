package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{Chain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication, ChainConcat}
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentExtractor}
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.schema.support.{ConstantMapping, LazyMapping, MappedMapping, MappingAdapter}
import net.noresttherein.oldsql.schema.MappingSchema.{FlatMappedMappingSchema, FlatMappingSchema, MappedMappingSchema, MappingSchemaGuts, MultiComponentSchema, SchemaInlining}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatMappedSchemaMapping, FlatSchemaMapping, MappedSchemaMapping, SchemaColumn}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.MappedMapping.FlatMappedMapping
import net.noresttherein.oldsql.schema.ColumnMapping.BaseColumn
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth

import scala.annotation.{implicitNotFound, tailrec}
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
  * @see [[net.noresttherein.oldsql.schema.MappingSchema.FlatMappingSchema]]
  * @author Marcin Mościcki
  */
trait SchemaMapping[+C <:Chain, R <: Chain, O, S] extends GenericMapping[O, S] { outer =>
	val schema :MappingSchema[C, R, O, S]


	/** Rebases this mapping to the flat version of its schema, where every non-column component is recursively replaced
	  * with the full list of its columns.
	  */
	def inline[U >: C <: Chain, IC <: Chain, IL <: Chain]
	          (implicit inliner :SchemaInlining[U, R, O, S, IC, IL]) :FlatSchemaMapping[IC, IL, O, S] =
		new ShallowProxy[O, S] with FlatSchemaMapping[IC, IL, O, S] {
			override val schema = inliner(outer.schema)
			protected override val egg = outer
		}



	override def apply[T](component :Component[T]) :Selector[T] =
		if (component eq schema)
			ComponentExtractor.req(schema)(schema.disassemble _).asInstanceOf[Selector[T]]
		else
            schema.extractor(component)



	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :SchemaMapping[C, R, O, X] =
		new MappedSchemaMapping[C, R, O, S, X](this, there, back)

	override def flatMap[X](there :S => Option[X], back :X => Option[S])
	                       (implicit nulls :NullValue[X]) :SchemaMapping[C, R, O, X] =
		new FlatMappedSchemaMapping[C, R, O, S, X](this, there, back)
}





object SchemaMapping {

	/** A single-column schema mapping */
	trait SchemaColumn[O, S] extends FlatSchemaMapping[@~ ~ SchemaColumn[O, S], @~ ~ S, O, S] with ColumnMapping[O, S] {
		override val schema :FlatMappingSchema[@~ ~ SchemaColumn[O, S], @~ ~ S, O, S] =
			MappingSchema[O, S].col(this, ComponentExtractor.ident[O, S](this))
	}

	object SchemaColumn {
		def apply[O, S :ColumnForm](name :String, buffs :Buff[S]*) :SchemaColumn[O, S] =
			new BaseColumn[O, S](name, buffs) with SchemaColumn[O, S]
	}






	/** A `SchemaMapping` variant which uses a `FlatSchemaMapping`, that is the component list `C` contains only
	  * `SchemaColumn`s.
	  */
	trait FlatSchemaMapping[+C <: Chain, R <: Chain, O, S] extends SchemaMapping[C, R, O, S] { outer =>
		override val schema :FlatMappingSchema[C, R, O, S]


		override def inline[U >: C <: Chain, IC <: Chain, IL <: Chain]
		                   (implicit inliner :SchemaInlining[U, R, O, S, IC, IL]) :FlatSchemaMapping[IC, IL, O, S] =
			this.asInstanceOf[FlatSchemaMapping[IC, IL, O, S]]



		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :FlatSchemaMapping[C, R, O, X] =
			new MappedFlatSchemaMapping[C, R, O, S, X](this, there, back)

		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :SchemaMapping[C, R, O, X] =
			new FlatMappedSchemaMapping[C, R, O, S, X](this, there, back)

	}






	private[schema] class MappedSchemaMapping[+C <: Chain, R <: Chain, O, S, T]
	                                         (override val egg :SchemaMapping[C, R, O, S],
	                                          override val map :S => T, override val unmap :T => S)
	                                         (implicit override val nulls :NullValue[T])
		extends MappedMapping[SchemaMapping[C, R, O, S], O, S, T] with MappingAdapter[SchemaMapping[C, R, O, S], O, T]
			with SchemaMapping[C, R, O, T]
	{
		override val schema :MappingSchema[C, R, O, T] = egg.schema compose unmap

		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :MappedSchemaMapping[C, R, O, S, X] =
			new MappedSchemaMapping[C, R, O, S, X](egg, map andThen there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedSchemaMapping[C, R, O, S, X] =
			new FlatMappedSchemaMapping[C, R, O, S, X](egg, map andThen there, back(_) map unmap)(flatMapNulls(there))
	}



	private[schema] class FlatMappedSchemaMapping[+C <: Chain, R <: Chain, O, S, T]
	                                             (mapping :SchemaMapping[C, R, O, S],
	                                              assemble :S => Option[T], disassemble :T => Option[S])
	                                             (implicit override val nulls :NullValue[T])
		extends FlatMappedMapping[SchemaMapping[C, R, O, S], O, S, T](mapping, assemble, disassemble, nulls)
		   with MappingAdapter[SchemaMapping[C, R, O, S], O, T] with SchemaMapping[C, R, O, T]
	{
		override val schema :MappingSchema[C, R, O, T] = egg.schema compose Extractor(unmap)

		override def map[X](there :T => X, back :X => T)
		                   (implicit nulls :NullValue[X]) :FlatMappedSchemaMapping[C, R, O, S, X] =
			new FlatMappedSchemaMapping[C, R, O, S, X](egg, map(_) map there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedSchemaMapping[C, R, O, S, X] =
			new FlatMappedSchemaMapping[C, R, O, S, X](egg, map(_) flatMap there, back(_) flatMap unmap)(flatMapNulls(there))
	}






	private[schema] class MappedFlatSchemaMapping[+C <: Chain, R <: Chain, O, S, T]
	                                             (override val egg :FlatSchemaMapping[C, R, O, S],
	                                              override val map :S => T, override val unmap :T => S)
	                                             (implicit override val nulls :NullValue[T])
		extends MappedMapping[FlatSchemaMapping[C, R, O, S], O, S, T] with MappingAdapter[FlatSchemaMapping[C, R, O, S], O, T]
			with FlatSchemaMapping[C, R, O, T]
	{
		override val schema :FlatMappingSchema[C, R, O, T] = egg.schema compose unmap

		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :MappedFlatSchemaMapping[C, R, O, S, X] =
			new MappedFlatSchemaMapping[C, R, O, S, X](egg, map andThen there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedFlatSchemaMapping[C, R, O, S, X] =
			new FlatMappedFlatSchemaMapping[C, R, O, S, X](egg, map andThen there, back(_) map unmap)(flatMapNulls(there))
	}



	private[schema] class FlatMappedFlatSchemaMapping[+C <: Chain, R <: Chain, O, S, T]
	                                                 (override val egg :FlatSchemaMapping[C, R, O, S],
	                                                  assemble :S => Option[T], disassemble :T => Option[S])
	                                                 (implicit override val nulls :NullValue[T])
		extends FlatMappedMapping[FlatSchemaMapping[C, R, O, S], O, S, T](egg, assemble, disassemble, nulls)
			with MappingAdapter[FlatSchemaMapping[C, R, O, S], O, T] with FlatSchemaMapping[C, R, O, T]
	{
		override val schema :FlatMappingSchema[C, R, O, T] = egg.schema compose Extractor(unmap)

		override def map[X](there :T => X, back :X => T)
		                   (implicit nulls :NullValue[X]) :FlatMappedFlatSchemaMapping[C, R, O, S, X] =
			new FlatMappedFlatSchemaMapping[C, R, O, S, X](egg, map(_) map there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedFlatSchemaMapping[C, R, O, S, X] =
			new FlatMappedFlatSchemaMapping[C, R, O, S, X](egg, map(_) flatMap there, back(_) flatMap unmap)(flatMapNulls(there))
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
abstract class AbstractSchemaMapping[+C <: Chain, R <: Chain, O, S](override val schema :MappingSchema[C, R, O, S])
	extends SchemaMapping[C, R, O, S] with StaticMapping[O, S] with LazyMapping[O, S]
{
	override val components :Unique[Component[_]] = schema.components
	override val subcomponents :Unique[Component[_]] = schema.subcomponents
	override val columns :Unique[Component[_]] = schema.columns
}






/** A list of components of some `SchemaMapping` for subject type `S`, with all their types encoded in this class's type.
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
trait MappingSchema[+C <: Chain, R <: Chain, O, S] extends GenericMapping[O, R] { outer :MappingSchemaGuts[C, R, O, S] =>

	/** A shorthand alias for `ColumnMapping[O, T]` allowing reduced notation in the component type chain. */
	type ||[T] = SchemaColumn[O, T]
	type Subschema[+L <: Chain,  V<: Chain, T] = SchemaMapping[L, V, O, T]

	def members :C

	def comp[X <: Mapping, M <: Subschema[L, V, T], L <: Chain, V <: Chain, T]
	        (component :X, value :Extractor[S, T])
	        (implicit types :IsBoth[X, M, Subschema[L, V, T]]) :MappingSchema[C ~ M, R ~ T, O, S] =
		new MultiComponentSchema[C, M, R, T, O, S](this, component,
			ComponentExtractor(component, value.optional, value.requisite)
		)

	def comp[X <: Mapping, M <: Subschema[L, V, T], L <: Chain, V <: Chain, T]
            (value :S => T, component :X)
            (implicit types :IsBoth[X, M, Subschema[L, V, T]]) :MappingSchema[C ~ M, R ~ T, O, S] =
		new MultiComponentSchema[C, M, R, T, O, S](this, types(component), ComponentExtractor.req(types(component))(value))

	def optcomp[X <: Mapping, M <: Subschema[L, V, T], L <: Chain, V <: Chain, T]
	           (value :S => Option[T], component :X)
	           (implicit types :IsBoth[X, M, Subschema[L, V, T]]) :MappingSchema[C ~ M, R ~ T, O, S] =
		new MultiComponentSchema[C, M, R, T, O, S](this, types(component), ComponentExtractor.opt(types(component))(value))



//	def col[T :ColumnForm](name :String, value :S =?> T, buffs :Buff[T]*) :MappingSchema[C ~ ||[T], R ~ T, O, S] =
//		comp(value, SchemaColumn[O, T](name, buffs :_*))

	def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*) :MappingSchema[C ~ ||[T], R ~ T, O, S] =
		comp(value, SchemaColumn[O, T](name, buffs:_*))

//	def col[T :ColumnForm](value :S =?> T, buffs :Buff[T]*)(implicit tpe :TypeTag[S]) :MappingSchema[C ~ ||[T], R ~ T, O, S] =
//		comp(value, SchemaColumn[O, T](PropertyPath.nameOf(value.requisite getOrElse value.optional), buffs:_*))

	def col[T :ColumnForm](value :S => T, buffs :Buff[T]*)(implicit tpe :TypeTag[S])
			:MappingSchema[C ~ ||[T], R ~ T, O, S] =
		col(PropertyPath.nameOf(value), value, buffs :_*)

	def optcol[T :ColumnForm](name :String, value :S => Option[T], buffs :Buff[T]*)
			:MappingSchema[C ~ ||[T], R ~ T, O, S] =
		optcomp(value, SchemaColumn[O, T](name, buffs:_*))

	def optcol[T :ColumnForm](value :S => Option[T], buffs :Buff[T]*)(implicit tpe :TypeTag[S])
			:MappingSchema[C ~ ||[T], R ~ T, O, S] =
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



	def map(constructor :R => S) :SchemaMapping[C, R, O, S] =
		new MappedMappingSchema[C, R, O, S](this, constructor)

	def map[F](constructor :F)(implicit apply :ChainApplication[R, F, S]) :SchemaMapping[C, R, O, S] =
		map { v :R => v.feedTo(constructor) }



	def flatMap(constructor :R => Option[S]) :SchemaMapping[C, R, O, S] =
		new FlatMappedMappingSchema[C, R, O, S](this, constructor)

	def flatMap[F](constructor :F)(implicit apply :ChainApplication[R, F, Option[S]]) :SchemaMapping[C, R, O, S] =
		flatMap { row :R => row.feedTo(constructor) }



	def inline[U >: C <: Chain, IC <: Chain, IR <: Chain]
	          (implicit inliner :SchemaInlining[U, R, O, S, IC, IR]) :FlatMappingSchema[IC, IR, O, S] =
		inliner(this)

	def compose[X](extractor :X => S) :MappingSchema[C, R, O, X]

	def compose[X](extractor :X =?> S) :MappingSchema[C, R, O, X]

	def extractor[X](component :Component[X]) :ComponentExtractor[O, S, X]

	def unapply(subject :S) :Option[R]

	def disassemble(subject :S) :R



	protected[schema] def selectorsList :List[(Component[_], Selector[_])]

	protected[schema] def extractorsList :List[(Component[_], ComponentExtractor[O, S, _])]

}






object MappingSchema {


	def apply[O, S] :FlatMappingSchema[@~, @~, O, S] = EmptySchema[O, S]



	/** Methods allowing positional accept of the components listed on `I ~ L` by a `MappingSchema`.  */
	implicit class MappingSchemaComponentAccessor[I <: Chain, L <: Component[O, T], R <: Chain, T, O, S]
	                                             (private val self :MappingSchema[I ~ L, R ~ T, O, S]) extends AnyVal
	{
		/** The last component on the list - same as `last` but more readable in code like `schema.prev.prev()`. */
		def apply() :L = last

		/** The last component on the list. */
		def last :L = self.asInstanceOf[MultiComponentSchema[I, L, R, T, O, S]].last

		/** The schema for the chain `I`, containing all components of this schema except for the last one. */
		def prev :MappingSchema[I, R, O, S] = self.asInstanceOf[MultiComponentSchema[I, L, R, T, O, S]].init
	}



	/** Methods allowing positional accept of the components listed on `I ~ L` by a `MappingSchema`.  */
	implicit class FlatMappingSchemaComponentAccessor[I <: Chain, L <: Component[O, T], R <: Chain, T, O, S]
	                                                 (private val self :FlatMappingSchema[I ~ L, R ~ T, O, S])
		extends AnyVal
	{
		/** The schema for the chain `I`, containing all components of this schema except for the last one. */
		def prev :FlatMappingSchema[I, R, O, S] = self.asInstanceOf[FlatMultiComponentSchema[I, L, R, T, O, S]].init
	}





	trait FlatMappingSchema[+C <: Chain, R <: Chain, O, S] extends MappingSchema[C, R, O, S] {
		outer :MappingSchemaGuts[C, R, O, S] =>


		def col[X <: Mapping, M <: SchemaColumn[O, T], L <: Chain, V <: Chain, T]
		                (column :X, value :S =?> T)(implicit types :IsBoth[X, M, SchemaColumn[O, T]])
				:FlatMappingSchema[C ~ M, R ~ T, O, S] =
			new FlatMultiComponentSchema[C, M, R, T, O, S](this, types(column),
				                                           ComponentExtractor(column, value.optional, value.requisite))

		override def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*) :FlatMappingSchema[C ~ ||[T], R ~ T, O, S] =
			col(SchemaColumn[O, T](name, buffs:_*), value)

		override def col[T :ColumnForm](value :S => T, buffs :Buff[T]*)(implicit tpe :TypeTag[S]) :FlatMappingSchema[C ~ ||[T], R ~ T, O, S] =
			col(PropertyPath.nameOf(value), value, buffs :_*)



		override def optcol[T :ColumnForm](name :String, value :S => Option[T], buffs :Buff[T]*) :FlatMappingSchema[C ~ ||[T], R ~ T, O, S] =
			col(SchemaColumn[O, T](name, buffs :_*), Extractor(value))

		override def optcol[T :ColumnForm](value :S => Option[T], buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:FlatMappingSchema[C ~ ||[T], R ~ T, O, S] =
			optcol(PropertyPath.nameOf(value), value, buffs :_*)



		override def map(constructor :R => S) :FlatSchemaMapping[C, R, O, S] =
			new MappedMappingSchema[C, R, O, S](this, constructor) with FlatSchemaMapping[C, R, O, S] {
				override val schema = outer
			}

		override def map[F](constructor :F)(implicit apply :ChainApplication[R, F, S]) :FlatSchemaMapping[C, R, O, S] =
			map { row :R => row.feedTo(constructor) }

		override def flatMap(constructor :R => Option[S]) :FlatSchemaMapping[C, R, O, S] =
			new FlatMappedMappingSchema[C, R, O, S](this, constructor) with FlatSchemaMapping[C, R, O, S] {
				override val schema = outer
			}

		override def flatMap[F](constructor :F)(implicit apply :ChainApplication[R, F, Option[S]]) :FlatSchemaMapping[C, R, O, S] =
			flatMap { row :R => row feedTo constructor }



		override def inline[U >: C <: Chain, IC <: Chain, IR <: Chain]
		                   (implicit inliner :SchemaInlining[U, R, O, S, IC, IR]) :FlatMappingSchema[IC, IR, O, S] =
			this.asInstanceOf[FlatMappingSchema[IC, IR, O, S]]

		def compose[X](extractor :X => S) :FlatMappingSchema[C, R, O, X]

		def compose[X](extractor :X =?> S) :FlatMappingSchema[C, R, O, X]

	}






	@implicitNotFound("Cannot concatenate schemas ${PC} + ${SC} (with subject types ${PR} + ${SR}). Possible reasons:\n" +
		"1) any of the schemas contains non-column components,\n" +
		"2) the type of any of the schemas is not fully known,\n" +
		"3) the subject types don't match the subject types of the components," +
		"4) result cannot be unified with provided concatenated type ${C} (${R}).")
	sealed trait ColumnSchemaConcat[PC <: Chain, PR <: Chain, SC <: Chain, SR <: Chain, O, S, C <: Chain, R <: Chain] {
		def apply(prefix :FlatMappingSchema[PC, PR, O, S], suffix :FlatMappingSchema[SC, SR, O, S]) :FlatMappingSchema[C, R, O, S]
	}

	object ColumnSchemaConcat {
		private[this] val emptyCat = new ColumnSchemaConcat[Chain, Chain, @~, @~, Any, Any, Chain, Chain] {
			override def apply(prefix :FlatMappingSchema[Chain, Chain, Any, Any], suffix :FlatMappingSchema[@~, @~, Any, Any]) =
				prefix
		}

		implicit def concatEmpty[C <: Chain, R <: Chain, O, S] :ColumnSchemaConcat[C, R, @~, @~, O, S, C, R] =
			emptyCat.asInstanceOf[ColumnSchemaConcat[C, R, @~, @~, O, S, C, R]]

		implicit def concatColumns[PC <: Chain, PR <: Chain, SC <: Chain, SR <: Chain,
			                       M <: SchemaColumn[O, T], O, T, S, C <: Chain, R <: Chain]
		                          (implicit init :ColumnSchemaConcat[PC, PR, SC, SR, O, S, C, R])
				:ColumnSchemaConcat[PC, PR, SC ~ M, SR ~ T, O, S, C ~ M, R ~ T] =
			new ColumnSchemaConcat[PC, PR, SC ~ M, SR ~ T, O, S, C ~ M, R ~ T] {
				override def apply(prefix :FlatMappingSchema[PC, PR, O, S],
				                   suffix :FlatMappingSchema[SC ~ M, SR ~ T, O, S]) =
					init(prefix, suffix.prev).col(suffix.last, suffix.extractor(suffix.last))

			}
	}



	@implicitNotFound("Cannot inline schema ${C}\n (with subject type ${R}).\n Possible reasons: " +
		"the types are not fully known; subject types don't match the component types; origin types of the components " +
		"on the schema vary; the result cannot be unified with result types ${IC} (${IR}); .")
	abstract class SchemaInlining[C <: Chain, R <: Chain, O, S, IC <: Chain, IR <: Chain] private[MappingSchema] {
		def apply(schema :MappingSchema[C, R, O, S]) :FlatMappingSchema[IC, IR, O, S]
	}

	sealed abstract class ComponentSchemaInlining {

		private[this] final val empty = new SchemaInlining[@~, @~, Any, Any, @~, @~] {
			override def apply(schema :MappingSchema[@~, @~, Any, Any]) = EmptySchema[Any, Any]
		}

		implicit def emptyInlining[O, S] :SchemaInlining[@~, @~, O, S, @~, @~] =
			empty.asInstanceOf[SchemaInlining[@~, @~, O, S, @~, @~]]

		implicit def componentInlining[C <: Chain, R <: Chain, PC <: Chain, PR <: Chain,
			                           M <: SchemaMapping[MC, MR, O, T], MC <: Chain, MR <: Chain, O, T, S,
		                               SC <: Chain,  SR <: Chain, IC <: Chain, IR <: Chain]
		                              (implicit prefix :SchemaInlining[C, R, O, S, PC, PR],
		                               hint :IsBoth[M, M, SchemaMapping[MC, MR, O, T]],
		                               inline :SchemaInlining[MC, MR, O, T, SC, SR],
		                               concat :ColumnSchemaConcat[PC, PR, SC, SR, O, S, IC, IR])
				:SchemaInlining[C ~ M, R ~ T, O, S, IC, IR] =
			new SchemaInlining[C ~ M, R ~ T, O, S, IC, IR] {
				override def apply(schema :MappingSchema[C ~ M, R ~ T, O, S]) =
					concat(prefix(schema.prev), inline(schema.last.schema) compose schema.extractor(schema.last))
			}
	}

	object SchemaInlining extends ComponentSchemaInlining {


		implicit def columnInlining[C <: Chain, R <: Chain, M <: SchemaColumn[O, T], O, T, S, IC <: Chain, IR <: Chain]
		                           (implicit init :SchemaInlining[C, R, O, S, IC, IR])
				:SchemaInlining[C ~ M, R ~ T, O, S, IC ~ M, IR ~ T] =
			new SchemaInlining[C ~ M, R ~ T, O, S, IC ~ M, IR ~ T] {
				override def apply(schema :MappingSchema[C ~ M, R ~ T, O, S]) =
					init(schema.prev).col(schema.last, schema.extractor(schema.last))
			}

	}






	sealed trait MappingSchemaGuts[+C <: Chain, R <: Chain, O, S] extends MappingSchema[C, R, O, S]





	class EmptySchema[O, S] extends ConstantMapping[O, @~](@~) with FlatMappingSchema[@~, @~, O, S]
		with MappingSchemaGuts[@~, @~, O, S]
	{
		override def members: @~ = @~

		private[this] val extractor :ComponentExtractor[O, S, @~] = ComponentExtractor.const(this)(@~)

		override def extractor[X](component :Component[X]) :ComponentExtractor[O, S, X] =
			if (component eq this)
				extractor.asInstanceOf[ComponentExtractor[O, S, X]]
			else
				throw new IllegalArgumentException("Component $component is not a part of this empty mapping schema.")

		override def unapply(subject :S): Option[@~] = Some(@~)

		override def disassemble(subject :S): @~ = @~

		override def compose[X](extractor :X => S) :EmptySchema[O, X] =
			this.asInstanceOf[EmptySchema[O, X]]

		override def compose[X](extractor :X =?> S) :EmptySchema[O, X] =
			this.asInstanceOf[EmptySchema[O, X]]

		protected[schema] override def selectorsList :Nil.type = Nil

		protected[schema] override def extractorsList :Nil.type = Nil
	}



	object EmptySchema {
		private[this] val empty = new EmptySchema[Any, Any]

		def apply[O, S] :FlatMappingSchema[@~, @~, O, S] = empty.asInstanceOf[EmptySchema[O, S]]

		def unapply(schema :MappingSchema[_, _, _, _]) :Boolean = schema.isInstanceOf[EmptySchema[_, _]]
	}






	class MultiComponentSchema[+C <: Chain, +M <: Component[O, T], R <: Chain, T, O, S]
	                          (val init :MappingSchema[C, R, O, S], val last :M, val extractor :ComponentExtractor[O, S, T])
		extends MappingSchemaGuts[C ~ M, R ~ T, O, S] with LazyMapping[O, R ~ T]
	{
		override def members :C ~ M = init.members ~ last

		override def unapply(subject :S) :Option[R ~ T] =
			for (i <- init.unapply(subject); l <- extractor.get(subject)) yield i ~ l

		override def disassemble(subject :S) :R ~ T = init.disassemble(subject) ~ extractor(subject)

		override def assemble(pieces :Pieces) :Option[R ~ T] =
			for (i <- pieces.get(initSelector); l <- pieces.get(lastSelector))
				yield i ~ l

		override def compose[X](extractor :X => S) :MultiComponentSchema[C, M, R, T, O, X] =
			new MultiComponentSchema[C, M, R, T, O, X](init compose extractor, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :MultiComponentSchema[C, M, R, T, O, X] =
			new MultiComponentSchema[C, M, R, T, O, X](init compose extractor, last, this.extractor compose extractor)



		private[this] val extractors = Lazy(extractorsList.toMap)

		protected[schema] override def extractorsList :List[(init.Component[_], ComponentExtractor[O, S, _])] =
			(last -> extractor) :: (init -> ComponentExtractor.opt(init)(init.unapply)) ::
				last.subcomponents.toList.map { comp => (comp, last(comp) compose extractor) } reverse_:::
				init.extractorsList

		override def extractor[X](component :Component[X]) :ComponentExtractor[O, S, X] =
			if (component eq last) extractor.asInstanceOf[ComponentExtractor[O, S, X]]
			else extractors.get(component).asInstanceOf[ComponentExtractor[O, S, X]]



		private[this] val selectors = Lazy(selectorsList.toMap)
		private[this] val initSelector = ComponentExtractor.req(init) { vs :(R ~ T) => vs.init }
		private[this] val lastSelector = ComponentExtractor.req(last) { vs :(R ~ T) => vs.last }

		protected[schema] override def selectorsList :List[(Component[_], Selector[_])] =
			(last -> lastSelector) :: (init, initSelector) ::
				last.subcomponents.toList.map { comp => (comp, last(comp) compose lastSelector) } reverse_:::
				init.selectorsList.map { case (comp, sel) => (comp, sel compose initSelector) }

		override def apply[X](component :Component[X]) :Selector[X] =
			selectors.get(component).asInstanceOf[Selector[X]]



		override val components :Unique[Component[_]] = Unique[Component[_]](init, last)

		override val subcomponents :Unique[Component[_]] = {
			@tailrec def rec(schema :MappingSchema[_, _, O, _], subcomponents :List[Component[_]]) :List[Component[_]] =
				schema match {
					case multi :MultiComponentSchema[_, _, _, _, O, _] =>
						rec(multi.init, multi.init :: multi.last.subcomponents.toList ::: multi.last :: subcomponents)
					case _ => subcomponents
				}
			Unique.later(Unique.from(rec(this, Nil)))
		}

		override val columns :Unique[Component[_]] = {
			@tailrec def rec(schema :MappingSchema[_, _, O, _], columns :List[Component[_]]) :List[Component[_]] =
				schema match {
					case multi :MultiComponentSchema[_, _, _, _, O, _] =>
						rec(multi.init, multi.last.columns.toList ::: columns)
					case _ => columns
				}
			Unique.later(Unique.from(rec(this, Nil)))
		}


	}







	private[MappingSchema] class FlatMultiComponentSchema[+C <: Chain, +M <: Component[O, T], R <: Chain, T, O, S]
	                                                     (override val init :FlatMappingSchema[C, R, O, S], next :M,
	                                                      get :ComponentExtractor[O, S, T])
		extends MultiComponentSchema[C, M, R, T, O, S](init, next, get) with FlatMappingSchema[C ~ M, R ~ T, O, S]
	{
		override def compose[X](extractor :X => S) :FlatMultiComponentSchema[C, M, R, T, O, X] =
			new FlatMultiComponentSchema[C, M, R, T, O, X](init compose extractor, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :FlatMultiComponentSchema[C, M, R, T, O, X] =
			new FlatMultiComponentSchema[C, M, R, T, O, X](init compose extractor, last, this.extractor compose extractor)
	}






	private[MappingSchema] class MappedMappingSchema[C <: Chain, R <: Chain, O, S]
	                                                (override val schema :MappingSchema[C, R, O, S], constructor :R => S)
		extends ShallowAdapter[Component[O, R], O, R, S] with SchemaMapping[C, R, O, S]
	{
		override protected val egg = schema
		private[this] val schemaExtractor = ComponentExtractor.opt(schema)(schema.unapply)

		override def apply[T](component :Component[T]) =
			if (component eq schema) schemaExtractor.asInstanceOf[Selector[T]]
			else schema.extractor(component)

		override def assemble(pieces :Pieces) :Option[S] =
			pieces.get(schemaExtractor) map constructor

	}

	private[MappingSchema] class FlatMappedMappingSchema[C <: Chain, R <: Chain, O, S]
			(override val schema :MappingSchema[C, R, O, S], constructor :R => Option[S])
		extends ShallowAdapter[Component[O, R], O, R, S] with SchemaMapping[C, R, O, S]
	{
		override protected val egg = schema
		private[this] val schemaExtractor = ComponentExtractor.opt(schema)(schema.unapply)

		override def apply[T](component :Component[T]) =
			if (component eq schema) schemaExtractor.asInstanceOf[Selector[T]]
			else schema.extractor(component)

		override def assemble(pieces :Pieces) :Option[S] =
			pieces.get(schemaExtractor) flatMap constructor

	}

}



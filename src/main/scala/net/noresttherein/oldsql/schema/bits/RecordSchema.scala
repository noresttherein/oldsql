package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection.{Chain, Record, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, GenericMapping, StaticMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.{BaseColumn, LabeledColumn}
import net.noresttherein.oldsql.schema.bits.RecordSchema.RecordMapping.{GetComponent, RecordComponents}
import net.noresttherein.oldsql.schema.support.{EmptyMapping, LabeledMapping, LazyMapping, MappedMapping, MappingAdapter}
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentExtractor}
import net.noresttherein.oldsql.schema.bits.RecordSchema.{FlatMappedRecordSchema, MappedRecordSchema, Name, RecordMapping}
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.schema.support.MappedMapping.FlatMappedMapping
import net.noresttherein.oldsql.schema.SQLForm.NullValue

import scala.annotation.implicitNotFound
import scala.collection.mutable.ListBuffer




/**
  * @author Marcin Mościcki
  */
trait RecordSchema[R <: Record, S, O] extends GenericMapping[S, O] { outer =>

	/** The list of components of this instance forming the schema and mapping for the `Record`
	  * containing the their values.
	  */
	def schema :RecordMapping[R, O]

	/** Retrieve the component with the given name from the schema.
	  * @return `schema(name)` (overriden in `RecordComponent`).
	  */
	def apply[N <: Name, V](name :N)(implicit comp :GetComponent[R, N, V, O]) :Component[V] = schema(name)(comp)



	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :RecordSchema[R, X, O] =
		new MappedRecordSchema[R, S, X, O](this, there, back)

	override def flatMap[X](there :S => Option[X], back :X => Option[S])
	                       (implicit nulls :NullValue[X]) :RecordSchema[R, X, O] =
		new FlatMappedRecordSchema[R, S, X, O](this, there, back, nulls)

}






abstract class AbstractRecordSchema[R <: Record, S, O](override val schema :RecordMapping[R, O])
	extends StaticMapping[S, O] with RecordSchema[R, S, O]
{ outer =>


	implicit final class GetComponentNamed[N <: Name](private val name :N) {
		/** Extension method injected into `String` literals retrieving the component of the given name from
		  * the enclosing mapping's schema.
		  * @return `apply(name)`.
		  */
		@inline def c[V](implicit comp :GetComponent[R, N, V, O]) :Component[V] = apply(name)

		/** Extension method injected into `String` literals retrieving as an `Option` the value of the component
		  * with the given name from the implicitly available `ComponentValues`.
		  * @return `pieces.get(apply(apply(name)))`.
		  */
		@inline def ?[V](implicit comp :GetComponent[R, N, V, O], pieces :Pieces) :Option[V] =
			pieces.get(apply(apply(name)))

		/** Extension method injected into `String` literals retrieving the value of the component
		  * with the given name from the implicitly available `ComponentValues`.
		  * @return `pieces(apply(apply(name)))`.
		  */
		@inline def ^[V](implicit comp :GetComponent[R, N, V, O], pieces :Pieces) :V =
			pieces(apply(apply(name)))
	}


}






object RecordSchema {
	type Name = String with Singleton


	/** Base trait extended by both [[net.noresttherein.oldsql.schema.bits.RecordSchema.RecordMapping RecordMapping]]
	  * and its individual member components, providing methods for incremental creation of a `RecordMapping`
	  * mapping.
	  * @tparam O identifier grouping components coming from the same source.
	  * @tparam R the `Record` subject type of the underlying mapping.
	  * @see [[net.noresttherein.oldsql.schema.bits.RecordSchema.RecordMapping]]
	  * @see [[net.noresttherein.oldsql.schema.bits.RecordSchema.RecordComponent]]
	  */
	sealed trait RecordMappingBuilder[R <: Record, O] {
		def +[K <: Name, V](next :RecordComponent[K, V, O]) :RecordMapping[R |# (K, V), O]


		@inline final def column[K <: Name :ValueOf, V :ColumnForm](buffs :Buff[V]*) :RecordMapping[R |# (K, V), O] =
			this + new RecordColumn[K, V, O](buffs)

		@inline final def column[K <: Name, V :ColumnForm](name :K, buffs: Buff[V]*) :RecordMapping[R |# (K, V), O] =
			this + new RecordColumn[K, V, O](buffs)(new ValueOf[K](name), ColumnForm[V])

		@inline final def component[K <: Name, V](name :K, component :Component[V, O]) :RecordMapping[R |# (K, V), O] =
			this + new NamedRecordComponent[K, V, O](name, component)
	}



	/** A list of components identified by unique string literals forming together a mapping of the `Record` formed
	  * from their name and subject pairs.
	  * @tparam R the subject `Record` type.
	  */
	sealed trait RecordMapping[R <: Record, O] extends RecordMappingBuilder[R, O] with RecordSchema[R, R, O] {
		override def schema :RecordMapping[R, O] = this

		def +[K <: Name, V](next :RecordComponent[K, V, O]) :RecordMapping[R |# (K, V), O] =
			new RecordComponents[O, R, K, V](this, next)

		/** The component of this mapping with the given name. */
		override def apply[K <: Name, V](key :K)(implicit get :GetComponent[R, K, V, O]) :RecordComponent[K, V, O] =
			get(this)


		override def map[X](there :R => X, back :X => R)
		                   (implicit nulls :NullValue[X] = null) :RecordSchema[R, X, O] =
			new MappedComponentsRecords[R, X, O](this, there, back)


		override def flatMap[X](there :R => Option[X], back :X => Option[R])
		                       (implicit nulls :NullValue[X] = null) :RecordSchema[R, X, O] =
			new FlatMappedComponentsRecords[R, X, O](this, there, back, nulls)


		protected[RecordSchema] def last[K <: Name, V]
		                                 (implicit ub :R <:< (Record |# (K, V)), sb :(Nothing |# (K, V)) <:< R)
			:RecordComponent[K, V, O]

		protected[RecordSchema] def subcomponentsList :ListBuffer[Component[_]]
		protected[RecordSchema] def selectorsList :List[(Component[_], Selector[_])]
	}






	trait RecordComponent[N <: Name, S, O]
		extends LabeledMapping[N, S, O] with RecordSchema[@~ |# (N, S), S, O] with RecordMappingBuilder[@~ |# (N, S), O]
	{
		def label :N = key
		def key :N
		override val schema :RecordMapping[@~ |# (N, S), O] = RecordMapping[O]() + this

		override def +[K <: Name, V](next :RecordComponent[K, V, O]) :RecordMapping[@~ |# (N, S) |# (K, V), O] =
			new RecordComponents(new RecordComponents(RecordMapping[O](), this), next)

		override def apply[K <: Name, V](name :K)(implicit comp :GetComponent[@~ |# (N, S), K, V, O]) :Component[V] =
			this.asInstanceOf[Component[V]]

		protected[RecordSchema] def subcomponentsList :List[Component[_]] = subcomponents.toList
	}



	class NamedRecordComponent[N <: Name, S, O](name :N, override val egg :Component[S, O])
		extends RecordComponent[N, S, O] with ShallowProxy[S, O]
	{
		override def key :N = name
	}



	class RecordColumn[N <: Name :ValueOf, S :ColumnForm, O](buffs :Seq[Buff[S]] = Nil)
		extends BaseColumn[S, O](valueOf[N], buffs) with RecordComponent[N, S, O]
	{
		override val key :N = valueOf[N]

		protected[RecordSchema] override def subcomponentsList :List[Component[_]] = Nil
	}






	object RecordMapping {

		def apply[O]() :RecordMapping[@~, O] = empty.asInstanceOf[RecordMapping[@~, O]]


		private[this] final val empty :RecordMapping[@~, Any] = new EmptyRecordMapping[Any]

		private class EmptyRecordMapping[O] extends RecordMapping[@~, O] with EmptyMapping[@~, O] {
			private[this] final val res = Some(@~)

			override def last[K <: Name, V](implicit ub: @~ <:< (Record |# (K, V)), sb :(Nothing |# (K, V)) <:< @~) =
				throw new IllegalArgumentException("Don't make a whore out of the type system!")


			override def assemble(values :Pieces) :Option[@~] = res

			protected[RecordSchema] override def subcomponentsList :ListBuffer[Component[_]] = new ListBuffer
			protected[RecordSchema] override def selectorsList :List[(Component[_], Selector[_])] = Nil

		}



		private[RecordSchema] class RecordComponents[O, T <: Record, N <: Name, S]
				                                     (val tail :RecordMapping[T, O], val head :RecordComponent[N, S, O])
			extends RecordMapping[T |# (N, S), O] with LazyMapping[T |# (N, S), O]
		{
			protected[RecordSchema] override def last[K <: Name, V]
			                                         (implicit ub :T |# (N, S) <:< (Record |# (K, V)),
			                                          sb :Nothing |# (K, V) <:< (T |# (N, S))) :RecordComponent[K, V, O] =
				head.asInstanceOf[RecordComponent[K, V, O]]


			override val components :Unique[Component[_]] = Unique(tail, head)

			override val subcomponents :Unique[Component[_]] = Unique.Lazy(subcomponentsList)

			protected[RecordSchema] override def subcomponentsList :ListBuffer[Component[_]] =
				tail.subcomponentsList += tail ++= head.subcomponentsList.reverse += head


			private[this] val headSelector = ComponentExtractor.req(head)((r :T |# (N, S)) => r.last._2)

			private[this] val tailSelector = ComponentExtractor.req(tail)((r :T |# (N, S)) => r.init)

			protected[RecordSchema] override val selectorsList :List[(Component[_], Selector[_])] =
				(head -> headSelector) ::
					head.subcomponentsList.map { c => c -> head(c).compose((r :T |# (N, S)) => r.last._2) } reverse_:::
					(tail -> tailSelector) ::
					tail.selectorsList.map {
						case (component, selector) => component -> selector.compose((r :T |# (N, S)) => r.init)
					}

			private[this] val selectors = selectorsList.toMap

			override def apply[V](component :Component[V]) :Selector[V] =
				selectors(component).asInstanceOf[Selector[V]]



			override def apply(pieces :Pieces) :T |# (N, S) = pieces.predefined(this) match {
				case Some(x) => x
				case _ => pieces(tailSelector) |# head.key -> pieces(headSelector)
			}


			override def optionally(pieces :Pieces) :Option[T |# (N, S)] = pieces.getValue(this)

			override def assemble(pieces :Pieces) :Option[T |# (N, S)] =
				for { t <- pieces.get(tailSelector); h <- pieces.get(headSelector) }
					yield t |# head.key -> h


		}



		@implicitNotFound("There is no component named ${K} in the components record ${R}")
		sealed trait GetComponent[R <: Record, K <: Name, V, O] {
			def apply(components :RecordMapping[R, O]) :RecordComponent[K, V, O]
		}

		object GetComponent {
			private[this] final val last :GetComponent[Record |# (Name, Any), Name, Any, Any] =
				new GetComponent[Record |# (Name, Any), Name, Any, Any] {
					def apply(components :RecordMapping[Record |# (Name, Any), Any]) =
						components.last
				}

			implicit def getLast[O, R <: Record, N <: Name, S] :GetComponent[R |# (N, S), N, S, O] =
				last.asInstanceOf[GetComponent[R |# (N, S), N, S, O]]

			implicit def getPrevious[R <: Record, E <: (Name, Any), K <: Name, V, O]
			                        (implicit get :GetComponent[R, K, V, O]) :GetComponent[R |# E, K, V, O] =
				new GetComponent[R |# E, K, V, O] {
					def apply(components :RecordMapping[R |# E, O]) =
						get(components.asInstanceOf[RecordComponents[O, R, Name, Any]].tail)
				}
		}



		@implicitNotFound("There already is a component named ${K} in the components record ${R}.")
		final class UniqueComponentName[R <: Record, K <: Name, O]

		object UniqueComponentName {
			private[this] final val instance = new UniqueComponentName[Record, Name, Any]

			implicit def componentExists[R <: Record, K <: Name, O](implicit exists :GetComponent[R, K, _, O]) :UniqueComponentName[R, K, O] =
				instance.asInstanceOf[UniqueComponentName[R, K, O]]

			implicit def conflict[R <: Record, K <: Name, O] :UniqueComponentName[R, K, O] =
				instance.asInstanceOf[UniqueComponentName[R, K, O]]
		}

	}






	private class MappedComponentsRecords[R <: Record, S, O](override val egg :RecordMapping[R, O],
	                                                         override val map :R => S, override val unmap :S => R)
	                                                        (implicit val nulls :NullValue[S])
		extends MappedMapping[RecordMapping[R, O], R, S, O] with MappingAdapter[RecordMapping[R, O], S, O]
			with RecordSchema[R, S, O]
	{
		override def schema :RecordMapping[R, O] = egg

		override def map[X](there :S => X, back :X => S)
		                   (implicit nulls :NullValue[X]) :MappedComponentsRecords[R, X, O] =
			new MappedComponentsRecords[R, X, O](schema, map andThen there, back andThen unmap)(mapNulls(there))


		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :FlatMappedComponentsRecords[R, X, O] =
			new FlatMappedComponentsRecords[R, X, O](schema, map andThen there, back(_) map unmap, flatMapNulls(there))

	}



	private class FlatMappedComponentsRecords[R <: Record, S, O](override val schema :RecordMapping[R, O],
	                                                             assemble :R => Option[S], disassemble :S => Option[R],
	                                                             onNone :NullValue[S])
		extends FlatMappedMapping[RecordMapping[R, O], R, S, O](schema, assemble, disassemble, onNone)
	       with RecordSchema[R, S, O]
	{
		override def map[X](there :S => X, back :X => S)
		                   (implicit nulls :NullValue[X]) :FlatMappedComponentsRecords[R, X, O] =
			new FlatMappedComponentsRecords[R, X, O](schema, map(_) map there, back andThen unmap, mapNulls(there))


		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :FlatMappedComponentsRecords[R, X, O] =
			new FlatMappedComponentsRecords[R, X, O](schema, map(_) flatMap there, back(_) flatMap unmap, flatMapNulls(there))

	}







	private class MappedRecordSchema[R <: Record, T, S, O](override val egg :RecordSchema[R, T, O],
	                                                       override val map :T => S, override val unmap :S => T)
	                                                      (implicit val nulls :NullValue[S])
		extends MappedMapping[RecordSchema[R, T, O], T, S, O] with MappingAdapter[RecordSchema[R, T, O], S, O]
			with RecordSchema[R, S, O]
	{
		override def schema :RecordMapping[R, O] = egg.schema

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :MappedRecordSchema[R, T, X, O] =
			new MappedRecordSchema[R, T, X, O](egg, map andThen there, back andThen unmap)(mapNulls(there))


		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :FlatMappedRecordSchema[R, T, X, O] =
			new FlatMappedRecordSchema[R, T, X, O](egg, map andThen there, back(_) map unmap, flatMapNulls(there))

	}



	private class FlatMappedRecordSchema[R <: Record, T, S, O](mapping :RecordSchema[R, T, O],
	                                                           assemble :T => Option[S], disassemble :S => Option[T],
	                                                           onNone :NullValue[S])
		extends FlatMappedMapping[RecordSchema[R, T, O], T, S, O](mapping, assemble, disassemble, onNone)
		   with RecordSchema[R, S, O]
	{
		override def schema :RecordMapping[R, O] = egg.schema

		override def map[X](there :S => X, back :X => S)
		                   (implicit nulls :NullValue[X]) :FlatMappedRecordSchema[R, T, X, O] =
			new FlatMappedRecordSchema[R, T, X, O](egg, map(_) map there, back andThen unmap, mapNulls(there))


		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :FlatMappedRecordSchema[R, T, X, O] =
			new FlatMappedRecordSchema[R, T, X, O](egg, map(_) flatMap there, back(_) flatMap unmap, flatMapNulls(there))
	}

}


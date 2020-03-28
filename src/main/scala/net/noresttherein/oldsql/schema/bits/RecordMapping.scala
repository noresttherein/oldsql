package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{Chain, Record, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, GenericMapping, StaticMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.NamedColumn
import net.noresttherein.oldsql.schema.bits.RecordMapping.ComponentsRecord.{GetComponent, RecordComponents}
import net.noresttherein.oldsql.schema.support.{EmptyMapping, LazyMapping, MappedMapping, MappingAdapter}
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentExtractor}
import net.noresttherein.oldsql.schema.bits.RecordMapping.{ComponentsRecord, FlatMappedRecordMapping, MappedRecordMapping, Name}
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.schema.support.MappedMapping.FlatMappedMapping
import net.noresttherein.oldsql.schema.SQLForm.NullValue

import scala.annotation.implicitNotFound
import scala.collection.mutable.ListBuffer




/**
  * @author Marcin Mościcki
  */
trait RecordMapping[O, R <: Record, S] extends GenericMapping[O, S] { outer =>

	/** The list of components of this instance forming the schema and mapping for the `Record`
	  * containing the their values.
	  */
	def schema :ComponentsRecord[O, R]

	/** Retrieve the component with the given name from the schema.
	  * @return `schema(name)` (overriden in `RecordComponent`).
	  */
	def apply[N <: Name, V](name :N)(implicit comp :GetComponent[O, R, N, V]) :Component[V] = schema(name)(comp)



	override def map[X](there :S => X, back :X => S)(implicit nullValue :NullValue[X]) :RecordMapping[O, R, X] =
		new MappedRecordMapping[O, R, S, X](this, there, back)

	override def flatMap[X](there :S => Option[X], back :X => Option[S])
	                       (implicit nulls :NullValue[X]) :RecordMapping[O, R, X] =
		new FlatMappedRecordMapping[O, R, S, X](this, there, back, nulls)

}






abstract class AbstractRecordMapping[O, R <: Record, S](override val schema :ComponentsRecord[O, R])
	extends StaticMapping[O, S] with RecordMapping[O, R, S]
{ outer =>


	implicit final class GetComponentNamed[N <: Name](private val name :N) {
		/** Extension method injected into `String` literals retrieving the component of the given name from
		  * the enclosing mapping's schema.
		  * @return `apply(name)`.
		  */
		@inline def c[V](implicit comp :GetComponent[O, R, N, V]) :Component[V] = apply(name)

		/** Extension method injected into `String` literals retrieving as an `Option` the value of the component
		  * with the given name from the implicitly available `ComponentValues`.
		  * @return `pieces.get(apply(apply(name)))`.
		  */
		@inline def ?[V](implicit comp :GetComponent[O, R, N, V], pieces :Pieces) :Option[V] =
			pieces.get(apply(apply(name)))

		/** Extension method injected into `String` literals retrieving the value of the component
		  * with the given name from the implicitly available `ComponentValues`.
		  * @return `pieces(apply(apply(name)))`.
		  */
		@inline def ^[V](implicit comp :GetComponent[O, R, N, V], pieces :Pieces) :V =
			pieces(apply(apply(name)))
	}


}






object RecordMapping {
	type Name = String with Singleton


	/** Base trait extended by both [[net.noresttherein.oldsql.schema.bits.RecordMapping.ComponentsRecord ComponentsRecord]]
	  * and its individual member components, providing methods for incremental creation of a `ComponentsRecord`
	  * mapping.
	  * @tparam O identifier grouping components coming from the same source.
	  * @tparam R the `Record` subject type of the underlying mapping.
	  * @see [[net.noresttherein.oldsql.schema.bits.RecordMapping.ComponentsRecord]]
	  * @see [[net.noresttherein.oldsql.schema.bits.RecordMapping.RecordComponent]]
	  */
	sealed trait ComponentsRecordBuilder[O, R <: Record] {
		def +[K <: Name, V](next :RecordComponent[O, K, V]) :ComponentsRecord[O, R |# (K, V)]


		@inline final def column[K <: Name :ValueOf, V :ColumnForm](buffs :Buff[V]*) :ComponentsRecord[O, R |# (K, V)] =
			this + new RecordColumn[O, K, V](buffs)

		@inline final def column[K <: Name, V :ColumnForm](name :K, buffs: Buff[V]*) :ComponentsRecord[O, R |# (K, V)] =
			this + new RecordColumn[O, K, V](buffs)(new ValueOf[K](name), ColumnForm[V])

		@inline final def component[K <: Name, V](name :K, component :Component[O, V]) :ComponentsRecord[O, R |# (K, V)] =
			this + new NamedRecordComponent[O, K, V](name, component)
	}



	/** A list of components identified by unique string literals forming together a mapping of the `Record` formed
	  * from their name and subject pairs.
	  * @tparam R the subject `Record` type.
	  */
	sealed trait ComponentsRecord[O, R <: Record] extends ComponentsRecordBuilder[O, R] with RecordMapping[O, R, R] {
		override def schema :ComponentsRecord[O, R] = this

		def +[K <: Name, V](next :RecordComponent[O, K, V]) :ComponentsRecord[O, R |# (K, V)] =
			new RecordComponents[O, R, K, V](this, next)

		/** The component of this mapping with the given name. */
		override def apply[K <: Name, V](key :K)(implicit get :GetComponent[O, R, K, V]) :RecordComponent[O, K, V] =
			get(this)


		override def map[X](there :R => X, back :X => R)
		                   (implicit nulls :NullValue[X] = null) :RecordMapping[O, R, X] =
			new MappedComponentsRecords[O, R, X](this, there, back)


		override def flatMap[X](there :R => Option[X], back :X => Option[R])
		                       (implicit nulls :NullValue[X] = null) :RecordMapping[O, R, X] =
			new FlatMappedComponentsRecords[O, R, X](this, there, back, nulls)


		protected[RecordMapping] def last[K <: Name, V]
		                                 (implicit ub :R <:< (Record |# (K, V)), sb :(Nothing |# (K, V)) <:< R)
			:RecordComponent[O, K, V]

		protected[RecordMapping] def subcomponentsList :ListBuffer[Component[_]]
		protected[RecordMapping] def selectorsList :List[(Component[_], Selector[_])]
	}






	trait RecordComponent[O, N <: Name, S]
		extends GenericMapping[O, S] with RecordMapping[O, @~ |# (N, S), S] with ComponentsRecordBuilder[O, @~ |# (N, S)]
	{
		def key :N
		override val schema :ComponentsRecord[O, @~ |# (N, S)] = ComponentsRecord[O]() + this

		override def +[K <: Name, V](next :RecordComponent[O, K, V]) :ComponentsRecord[O, @~ |# (N, S) |# (K, V)] =
			new RecordComponents(new RecordComponents(ComponentsRecord[O](), this), next)

		override def apply[K <: Name, V](name :K)(implicit comp :GetComponent[O, @~ |# (N, S), K, V]) :Component[V] =
			this.asInstanceOf[Component[V]]

		protected[RecordMapping] def subcomponentsList :List[Component[_]] = subcomponents.toList
	}



	class NamedRecordComponent[O, N <: Name, S](name :N, override val egg :Component[O, S])
		extends RecordComponent[O, N, S] with ShallowProxy[O, S]
	{
		override def key :N = name
	}



	class RecordColumn[O, N <: Name :ValueOf, S :ColumnForm](buffs :Seq[Buff[S]] = Nil)
		extends NamedColumn[O, N, S](buffs) with RecordComponent[O, N, S]
	{
		override val key :N = name

		protected[RecordMapping] override def subcomponentsList :List[Component[_]] = Nil
	}






	object ComponentsRecord {

		def apply[O]() :ComponentsRecord[O, @~] = empty.asInstanceOf[ComponentsRecord[O, @~]]


		private[this] final val empty :ComponentsRecord[Any, @~] = new EmptyComponentsRecord[Any]

		private class EmptyComponentsRecord[O] extends ComponentsRecord[O, @~] with EmptyMapping[O, @~] {
			private[this] final val res = Some(@~)

			override def last[K <: Name, V](implicit ub: @~ <:< (Record |# (K, V)), sb :(Nothing |# (K, V)) <:< @~) =
				throw new IllegalArgumentException("Don't make a whore out of the type system!")


			override def assemble(values :Pieces) :Option[@~] = res

			protected[RecordMapping] override def subcomponentsList :ListBuffer[Component[_]] = new ListBuffer
			protected[RecordMapping] override def selectorsList :List[(Component[_], Selector[_])] = Nil

		}



		private[RecordMapping] class RecordComponents[O, T <: Record, N <: Name, S]
				                                     (val tail :ComponentsRecord[O, T], val head :RecordComponent[O, N, S])
			extends ComponentsRecord[O, T |# (N, S)] with LazyMapping[O, T |# (N, S)]
		{
			protected[RecordMapping] override def last[K <: Name, V](implicit ub :T |# (N, S) <:< (Record |# (K, V)),
			                                                         sb :Nothing |# (K, V) <:< (T |# (N, S))) :RecordComponent[O, K, V] =
				head.asInstanceOf[RecordComponent[O, K, V]]


			override val components :Unique[Component[_]] = Unique(tail, head)

			override val subcomponents :Unique[Component[_]] = Unique.Lazy(Unique.from(subcomponentsList))

			protected[RecordMapping] override def subcomponentsList :ListBuffer[Component[_]] =
				tail.subcomponentsList += tail ++= head.subcomponentsList.reverse += head


			private[this] val headSelector = ComponentExtractor.req(head)((r :T |# (N, S)) => r.head._2)

			private[this] val tailSelector = ComponentExtractor.req(tail)((r :T |# (N, S)) => r.tail)

			protected[RecordMapping] override val selectorsList :List[(Component[_], Selector[_])] =
				(head -> headSelector) ::
					head.subcomponentsList.map { c => c -> head(c).compose((r :T |# (N, S)) => r.head._2) } reverse_:::
					(tail -> tailSelector) ::
					tail.selectorsList.map {
						case (component, selector) => component -> selector.compose((r :T |# (N, S)) => r.tail)
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
		sealed trait GetComponent[O, R <: Record, K <: Name, V] {
			def apply(components :ComponentsRecord[O, R]) :RecordComponent[O, K, V]
		}

		object GetComponent {
			private[this] final val last :GetComponent[Any, Record |# (Name, Any), Name, Any] =
				new GetComponent[Any, Record |# (Name, Any), Name, Any] {
					def apply(components :ComponentsRecord[Any, Record |# (Name, Any)]) =
						components.last
				}

			implicit def getLast[O, R <: Record, N <: Name, S] :GetComponent[O, R |# (N, S), N, S] =
				last.asInstanceOf[GetComponent[O, R |# (N, S), N, S]]

			implicit def getPrevious[O, R <: Record, E <: (Name, Any), K <: Name, V]
			                        (implicit get :GetComponent[O, R, K, V]) :GetComponent[O, R |# E, K, V] =
				new GetComponent[O, R |# E, K, V] {
					def apply(components :ComponentsRecord[O, R |# E]) =
						get(components.asInstanceOf[RecordComponents[O, R, Name, Any]].tail)
				}
		}



		@implicitNotFound("There already is a component named ${K} in the components record ${R}.")
		final class UniqueComponentName[O, R <: Record, K <: Name]

		object UniqueComponentName {
			private[this] final val instance = new UniqueComponentName[Any, Record, Name]

			implicit def componentExists[O, R <: Record, K <: Name](implicit exists :GetComponent[O, R, K, _]) :UniqueComponentName[O, R, K] =
				instance.asInstanceOf[UniqueComponentName[O, R, K]]

			implicit def conflict[O, R <: Record, K <: Name] :UniqueComponentName[O, R, K] =
				instance.asInstanceOf[UniqueComponentName[O, R, K]]
		}

	}






	private class MappedComponentsRecords[O, R <: Record, S](override val egg :ComponentsRecord[O, R],
	                                                         override val map :R => S, override val unmap :S => R)
	                                                        (implicit val nulls :NullValue[S])
		extends MappedMapping[ComponentsRecord[O, R], O, R, S] with MappingAdapter[ComponentsRecord[O, R], O, S]
			with RecordMapping[O, R, S]
	{
		override def schema :ComponentsRecord[O, R] = egg

		override def map[X](there :S => X, back :X => S)
		                   (implicit nulls :NullValue[X]) :MappedComponentsRecords[O, R, X] =
		{
			val newNull =
				if (nulls != null) nulls
				else if (this.nulls != null) this.nulls.map(there)
				else null
			new MappedComponentsRecords[O, R, X](schema, map andThen there, back andThen unmap)(newNull)
		}


		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :FlatMappedComponentsRecords[O, R, X] =
		{
			val newNull =
				if (nulls != null) nulls
				else if (this.nulls != null) this.nulls.flatMap(there)
				else null
			new FlatMappedComponentsRecords[O, R, X](schema, map andThen there, back(_) map unmap, newNull)
		}

	}



	private class FlatMappedComponentsRecords[O, R <: Record, S](override val schema :ComponentsRecord[O, R],
	                                                             assemble :R => Option[S], disassemble :S => Option[R],
	                                                             onNone :NullValue[S])
		extends FlatMappedMapping[ComponentsRecord[O, R], O, R, S](schema, assemble, disassemble, onNone)
	       with RecordMapping[O, R, S]
	{
		override def map[X](there :S => X, back :X => S)
		                   (implicit nulls :NullValue[X]) :FlatMappedComponentsRecords[O, R, X] =
		{
			val newNull =
				if (nulls != null) nulls
				else this.nulls.map(there)
			new FlatMappedComponentsRecords[O, R, X](schema, map(_) map there, back andThen unmap, newNull)
		}


		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :FlatMappedComponentsRecords[O, R, X] =
		{
			val newNull =
				if (nulls != null) nulls
				else this.nulls.flatMap(there)
			new FlatMappedComponentsRecords[O, R, X](schema, map(_) flatMap there, back(_) flatMap unmap, newNull)
		}
	}







	private class MappedRecordMapping[O, R <: Record, T, S](override val egg :RecordMapping[O, R, T],
	                                                        override val map :T => S, override val unmap :S => T)
	                                                       (implicit val nulls :NullValue[S])
		extends MappedMapping[RecordMapping[O, R, T], O, T, S] with MappingAdapter[RecordMapping[O, R, T], O, S]
			with RecordMapping[O, R, S]
	{
		override def schema :ComponentsRecord[O, R] = egg.schema

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :MappedRecordMapping[O, R, T, X] =
		{
			val newNull =
				if (nulls != null) nulls
				else if (this.nulls != null) this.nulls.map(there)
				else null
			new MappedRecordMapping[O, R, T, X](egg, map andThen there, back andThen unmap)(newNull)
		}


		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :FlatMappedRecordMapping[O, R, T, X] =
		{
			val newNull =
				if (nulls != null) nulls
				else if (this.nulls != null) this.nulls.flatMap(there)
				else null
			new FlatMappedRecordMapping[O, R, T, X](egg, map andThen there, back(_) map unmap, newNull)
		}

	}



	private class FlatMappedRecordMapping[O, R <: Record, T, S](mapping :RecordMapping[O, R, T],
	                                                            assemble :T => Option[S], disassemble :S => Option[T],
	                                                            onNone :NullValue[S])
		extends FlatMappedMapping[RecordMapping[O, R, T], O, T, S](mapping, assemble, disassemble, onNone)
		   with RecordMapping[O, R, S]
	{
		override def schema :ComponentsRecord[O, R] = egg.schema

		override def map[X](there :S => X, back :X => S)
		                   (implicit nulls :NullValue[X]) :FlatMappedRecordMapping[O, R, T, X] =
		{
			val newNull =
				if (nulls != null) nulls
				else this.nulls.map(there)
			new FlatMappedRecordMapping[O, R, T, X](egg, map(_) map there, back andThen unmap, newNull)
		}


		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :FlatMappedRecordMapping[O, R, T, X] =
		{
			val newNull =
				if (nulls != null) nulls
				else this.nulls.flatMap(there)
			new FlatMappedRecordMapping[O, R, T, X](egg, map(_) flatMap there, back(_) flatMap unmap, newNull)
		}
	}

}


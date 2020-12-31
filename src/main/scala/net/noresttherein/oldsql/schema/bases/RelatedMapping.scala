package net.noresttherein.oldsql.schema.bases

import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.model.{Kin, PropertyPath, RelatedEntityFactory, Restraint}
import net.noresttherein.oldsql.model.Kin.SingletonKinFactory
import net.noresttherein.oldsql.schema.bits.{ForeignKeyColumnMapping, ForeignKeyMapping}
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, PrimaryKeyOf}
import net.noresttherein.oldsql.schema.Buff.{Nullable, SelectDefault}
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.PrimaryKeyOf.PrimaryKeyColumnOf
import net.noresttherein.oldsql.schema.Relation.RelVar






/** A mix-in trait providing factory methods for components (and columns) with references to other tables.
  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame]]
  * @see [[net.noresttherein.oldsql.schema.bases.ContainedMapping]]
  * @author Marcin Mościcki
  */ //todo: inverse fk, optional Option[Kin] fk
trait RelatedMapping[S, O] extends BaseMapping[S, O] {

	private def fkKinFactory[M[A] <: RefinedMapping[E, A], E :TypeTag, K]
	                        (table :RelVar[M], pk :M[_] => RefinedMapping[K, _]) :SingletonKinFactory[K, E] =
	{
		Kin.Lazy.factory {
			val key = pk(table.row).withOrigin[()]
			val extract = table.row[()](key)
			extract.requisite match {
				case Some(property) => Kin(Restraint.Property(property))
				case _ => Kin(Restraint.Property(extract.optional).flatten)
			}
		}
	}

	//use a differently named method so definitions in subclasses don't mess with overload rules
	protected def fkimpl[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                    (name :String, property :S => R, buffs :Buff[R]*)
	                    (table :RelVar[M], key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
			:ForeignKeyColumnMapping[K, R, O]



	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
			:ForeignKeyColumnMapping[K, R, O] =
		fkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                (implicit table :RelVar[M]) :ForeignKeyColumnMapping[K, R, O] =
		fkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                (implicit key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[K, R, O] =
		fkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key(_), reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (reference :RelatedEntityFactory[K, E, T, R])
	                (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[K, R, O] =
		fkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key(_), reference)



	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                (implicit entityType :TypeTag[S]) :ForeignKeyColumnMapping[K, R, O] =
		fkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (property :S => R, buffs :Buff[R]*)
	                (key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                (implicit entityType :TypeTag[S], table :RelVar[M]) :ForeignKeyColumnMapping[K, R, O] =
		fkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                (implicit entityType :TypeTag[S],
	                          key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[K, R, O] =
		fkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (property :S => R, buffs :Buff[R]*)
	                (reference :RelatedEntityFactory[K, E, T, R])
	                (implicit entityType :TypeTag[S], table :RelVar[M],
	                          key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[K, R, O] =
		fkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)



	protected def fk[M[A] <: RefinedMapping[E, A], K, E]
	                (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](name, value, buffs :_*)(table, key, fkKinFactory[M, E, K](table, key).required)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E]
	                (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (key :M[_] => ColumnMapping[K, _])
	                (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](name, value, buffs :_*)(table, key, fkKinFactory[M, E, K](table, key).required)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (table :RelVar[M])
	                (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](name, value, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](name, value, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)



	protected def fk[M[A] <: RefinedMapping[E, A], K, E]
	                (value :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E]) :ForeignKeyColumnMapping[K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E]
	                (value :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => ColumnMapping[K, _])
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (value :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
	                 table :RelVar[M], key :PrimaryKeyColumnOf[M]) :ForeignKeyColumnMapping[key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)



	protected def optfkimpl[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                       (name :String, value :S => R, buffs :Buff[R]*)
	                       (table :RelVar[M], pk :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
			:ForeignKeyColumnMapping[K, R, O] =
		fkimpl[M, K, E, T, R](name, value, SelectDefault(reference.nonexistent) +: buffs :_*)(table, pk, reference) //todo: Nullable on the inner column



	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R], table :RelVar[M], key :M[_] => ColumnMapping[K, _])
			:ForeignKeyColumnMapping[K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M]) :ForeignKeyColumnMapping[K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key(_), reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key(_), reference)



	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S]) :ForeignKeyColumnMapping[K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S], table :RelVar[M]) :ForeignKeyColumnMapping[K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S],
	                             key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S], table :RelVar[M],
	                             key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)



	protected def optfk[M[A] <: RefinedMapping[E, A], K, E]
	                   (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                   (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](name, value, buffs :_*)(table, key, fkKinFactory[M, E, K](table, key))

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E]
	                   (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (key :M[_] => ColumnMapping[K, _])
	                   (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](name, value, buffs :_*)(table, key, fkKinFactory[M, E, K](table, key))

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M])
	                   (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](name, value, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_))
		)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](name, value, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_))
		)



	protected def optfk[M[A] <: RefinedMapping[E, A], K, E]
	                   (value :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E]) :ForeignKeyColumnMapping[K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key)
		)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E]
	                   (value :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => ColumnMapping[K, _])
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key)
		)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (value :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_))
		)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
	                    table :RelVar[M], key :PrimaryKeyColumnOf[M]) :ForeignKeyColumnMapping[key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_))
		)






	//use a differently named method so definitions in subclasses don't mess with overload rules
	protected def fkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                    (property :S => R, buffs :Buff[R]*)
	                    (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                    (rename :String => String) :ForeignKeyMapping[C, K, R, O]



	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                (property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                (rename :String => String) :ForeignKeyMapping[C, K, R, O] =
		fkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                (property :S => R, buffs :Buff[R]*)(key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                (rename :String => String)(implicit table :RelVar[M]) :ForeignKeyMapping[C, K, R, O] =
		fkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (property :S => R, buffs :Buff[R]*)(table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                (rename :String => String)
	                (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (property :S => R, buffs :Buff[R]*)(reference :RelatedEntityFactory[K, E, T, R])
	                (rename :String => String)
	                (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K })
			:ForeignKeyMapping[key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(rename)



	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
			:ForeignKeyMapping[C, K, R, O] =
		fkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                (implicit table :RelVar[M]) :ForeignKeyMapping[C, K, R, O] =
		fkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (reference :RelatedEntityFactory[K, E, T, R])
	                (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(columnPrefix + _)



	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
	                (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => C[_])(rename :String => String)
	                (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])(rename :String => String)
	                (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(rename :String => String)
	                (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)(rename)



	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                (table :RelVar[M], key :M[_] => C[_])
	                (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(key :M[_] => C[_])
	                (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)(columnPrefix + _)

	
	
	protected def optfkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                       (property :S => R, buffs :Buff[R]*)
	                       (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                       (rename :String => String) :ForeignKeyMapping[C, K, R, O] =
		fkimpl[M, C, K, E, T, R](property, Nullable[R] +: SelectDefault(reference.nonexistent) +: buffs :_*)(
			table, key, reference //todo: this is lazy, we should give Nullable only to columns
		)(rename)



	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String) :ForeignKeyMapping[C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)(key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String)(implicit table :RelVar[M]) :ForeignKeyMapping[C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)(table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String)
	                   (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)(reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String)
	                   (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K })
			:ForeignKeyMapping[key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(rename)



	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
			:ForeignKeyMapping[C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M]) :ForeignKeyMapping[C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(columnPrefix + _)



	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
	                   (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => C[_])(rename :String => String)
	                   (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])(rename :String => String)
	                   (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(rename :String => String)
	                   (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)(rename)



	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M], key :M[_] => C[_])
	                   (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(key :M[_] => C[_])
	                   (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, fkKinFactory[M, E, K](table, key).required
		)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                   (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                   (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), fkKinFactory[M, E, key.Key](table, key(_)).required
		)(columnPrefix + _)
	
}

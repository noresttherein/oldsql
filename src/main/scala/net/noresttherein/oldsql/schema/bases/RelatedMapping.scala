package net.noresttherein.oldsql.schema.bases

import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.model.{ComposedOf, Kin, KinFactory, PropertyPath, RelatedEntityFactory, Restraint}
import net.noresttherein.oldsql.model.Kin.One
import net.noresttherein.oldsql.model.KinFactory.DerivedKinFactory
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, PrimaryKeyOf}
import net.noresttherein.oldsql.schema.Buff.{Nullable, SelectDefault}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.PrimaryKeyOf.PrimaryKeyColumnOf
import net.noresttherein.oldsql.schema.Relation.RelVar
import net.noresttherein.oldsql.schema.bits.{ForeignKeyColumnMapping, ForeignKeyMapping, TableKin}






/** A mix-in trait providing factory methods for components (and columns) with references to other tables.
  * It leaves a handful of the most generic variants for the subclasses to implement, but provides a large
  * selection of their variants, carefully moving required parameters to the implicit parameter lists in a way
  * which doesn't hinder type inference and insures that appropriate implicit values will be found (if defined).
  * The multitude of overloaded methods, resulting from an exponential explosion of combinations
  * of optional parameters, may seem daunting at first, but the rules are relatively simple.
  *
  * In the following examples, type parameters as well as implicit parameter lists are omitted.
  * Parameters surrounded by `|...|` are optional and any combination of them can be omitted under listed conditions.
  * The order of the remaining parameters remain unchanged. If all parameters from a parameter list are omitted,
  * the whole parameter list is left out (no empty parameter lists are present). Parameters moved to the implicit
  * parameter list retain their relative order.
  *
  * The overloaded methods are:
  *   1. Factory methods for foreign key columns (single-column foreign keys):
  *     {{{
  *     def fk(|name :String,| property :S => R, buffs :Buff[R]*)
  *           (|table :RelVar[M],| |key :M[_] => ColumnMapping[K, _],| |reference :RelatedEntityFactory[K, E, T, R]|)
  *     }}}
  *     1. If `name` is omitted, a `TypeTag[S]` is prepended to the implicit parameter list and the name of the column
  *        is derived from the reflected name of `property`.
  *     1. If `table` is omitted, it is moved to the implicit parameter list, and the mapping type must be inferred
  *        based on its subject type `E`, which is always either a part of `reference` factory's type,
  *        or the property and buffs types' if the default `Kin[E]` reference type is used for the column type.
  *     1. If `key` is omitted, a [[net.noresttherein.oldsql.schema.PrimaryKeyOf PrimaryKeyOf]]`[M]`
  *        is added to the implicit parameter list.
  *     1. If `reference` is omitted, a `TypeTag[E]` is prepended to the implicit parameter list
  *        (but following the `TypeTag[S]` if present) and a default factory for type `R =:= Kin[E]` is used,
  *        provided by method [[net.noresttherein.oldsql.schema.bases.RelatedMapping.kinFactory kinFactory]].
  *   1. Factory methods for foreign key components (multi-column foreign keys):
  *     {{{
  *     def fk(property :S => R, |columnPrefix :String,| buffs :Buff[R]*)
  *           (|table :RelVar[M],| |key :M[_] => C[_],| |reference :RelatedEntityFactory[K, E, T, R]|)
  *           (|rename :String => String|) :ForeignKeyMapping[M, C, K, R, O]
  *     }}}
  *     1. Exactly one parameter between `columnPrefix` and `rename` must be present; if `columnPrefix` is omitted,
  *        then the method has an extra parameter list (preceding the implicit parameter list, if present)
  *        with `rename` parameter.
  *     1. If `table` is omitted, it is moved to the implicit parameter lists following any type tags,
  *        as in the previous case.
  *     1. If `key` is omitted, a `PrimaryKeyOf[M]` is added to the implicit parameter list as above.
  *     1. If `reference` is omitted, a `TypeTag[E]` is prepended to the implicit parameter list and
  *        the reference type `R` is replaced with `Kin[E]` - as above. If all parameters from this parameter list
  *        are omitted, the whole parameter list is dropped.
  *     1. If `rename` is omitted, then the whole parameter list disappears and `columnPrefix` must be present.
  *
  *
  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame]]
  * @see [[net.noresttherein.oldsql.schema.bases.FlatMapping]]
  * @author Marcin Mościcki
  */ //todo: inverse fk, optional Option[Kin] fk
trait RelatedMapping[S, O] extends BaseMapping[S, O] {

	/** Default factory used in all foreign key and inverse foreign key mappings created by this mapping.
	  * The default implementation is [[net.noresttherein.oldsql.schema.bases.RelatedMapping.restrainedKinFactory restrainedKinFactory]],
	  * which creates [[net.noresttherein.oldsql.model.Kin Kin]] carrying a domain-level reflected function expression
	  * expressing the foreign key constraint in terms of the value of the entity property mapped to the key column(s):
	  * [[net.noresttherein.oldsql.model.Kin.Restrained$ Restrained]].
	  * This can be overriden by subclasses and an alternative implementation is provided in the form of method
	  * [[net.noresttherein.oldsql.schema.bases.RelatedMapping.tableKinFactory tableKinFactory]] which carries the exact
	  * columns of the key together with their values, which removes all possible ambiguity, but leaks information about
	  * the database schema outside, which might pose a security risk.
	  * Note that obtaining the referenced key component by applying the function `key` to the `table`'s mapping
	  * will trigger its initialization, which might lead to an infinite initialisation recursion in case of
	  * cycles in mapping references. In order to prevent this issue, the factory should be
	  * [[net.noresttherein.oldsql.model.KinFactory.delay lazy]].
	  */
	protected def kinFactory[M[A] <: RefinedMapping[E, A], K, E, T]
	                        (table :RelVar[M], key :M[_] => RefinedMapping[K, _])
	                        (implicit composition :T ComposedOf E, referencedType :TypeTag[E]) :KinFactory[K, E, T] =
		restrainedKinFactory[M, K, E, T](table, key)

	/** A lazy kin factory producing [[net.noresttherein.oldsql.model.Kin.Restrained$ Restrained]] kin which
	  * restrict the property mapping to the `key` component of the referenced table (which will be typically
	  * the table's primary key when used for creating a foreign key mapping, and the table's foreign key pointing
	  * to this table for inverse foreign key mappings).
	  */
	protected def restrainedKinFactory[M[A] <: RefinedMapping[E, A], K, E, T]
	              (table :RelVar[M], key :M[_] => RefinedMapping[K, _])
	              (implicit composition :T ComposedOf E, referencedType :TypeTag[E]) :KinFactory[K, E, T] =
		KinFactory.delay {
			val comp = key(table.row).withOrigin[()]
			val extract = table.row[()](comp)
			extract.requisite match {
				case Got(property) => Kin(Restraint.Property(property)).as[T]
				case _ => Kin(Restraint.Property(extract.optional).flatten).as[T]
			}
		}

	/** A lazy kin factory producing [[net.noresttherein.oldsql.schema.bits.TableKin TableKin]] carrying
	  * the referenced table and all columns of the referenced `key`, together with their values.
	  */
	protected def tableKinFactory[M[A] <: RefinedMapping[E, A], K, E, T]
	              (table :RelVar[M], key :M[_] => RefinedMapping[K, _])
	              (implicit composition :T ComposedOf E, referencedType :TypeTag[E]) :KinFactory[K, E, T] =
		KinFactory.delay { TableKin(table, key(table.row)).as[T] }



	/** Default factory used in all foreign key mappings created by this mapping.
	  * The default implementation is [[net.noresttherein.oldsql.schema.bases.RelatedMapping.restrainedKinFactory restrainedKinFactory]],
	  * which creates [[net.noresttherein.oldsql.model.Kin Kin]] carrying a domain-level reflected function expression
	  * expressing the foreign key constraint in terms of the value of the entity property mapped to the key column(s):
	  * [[net.noresttherein.oldsql.model.Kin.Restrained$ Restrained]].
	  * This can be overriden by subclasses and an alternative implementation is provided in the form of method
	  * [[net.noresttherein.oldsql.schema.bases.RelatedMapping.tableKinFactory tableKinFactory]] which carries the exact columns
	  * of the key together with their values, which removes all possible ambiguity, but leaks information about
	  * the database schema outside, which might pose a security risk.
	  * Note that obtaining the referenced key component by applying the function `pk` to the `table`'s mapping
	  * will trigger its initialization, which might lead to an infinite initialisation recursion in case of
	  * cycles in mapping references. In order to prevent this issue, the factory should be
	  * [[net.noresttherein.oldsql.model.KinFactory.delay lazy]].
	  */
	protected def requiredKinFactory[M[A] <: RefinedMapping[E, A], K, E, T]
	                                (table :RelVar[M], pk :M[_] => RefinedMapping[K, _]) 
	                                (implicit composition :T ComposedOf E, referenceType :TypeTag[E])
			:DerivedKinFactory[K, E, T] =
		requiredRestrainedKinFactory[M, K, E, T](table, pk)

	protected def requiredRestrainedKinFactory[M[A] <: RefinedMapping[E, A], K, E, T]
	                                          (table :RelVar[M], pk :M[_] => RefinedMapping[K, _])
	                                          (implicit composition :T ComposedOf E, referenceType :TypeTag[E])
			:DerivedKinFactory[K, E, T] =
		KinFactory.delay { () =>
			val key = pk(table.row).withOrigin[()]
			val extract = table.row[()](key)
			extract.requisite match {
				case Got(property) => Kin.Restrained(Restraint.Property(property)).as[T]
				case _ => Kin.Restrained(Restraint.Property(extract.optional).flatten).as[T]
			}
		}

	protected def requiredTableKinFactory[M[A] <: RefinedMapping[E, A], K, E, T]
	                                     (table :RelVar[M], pk :M[_] => RefinedMapping[K, _])
	                                     (implicit composition :T ComposedOf E, referenceType :TypeTag[E])
			:DerivedKinFactory[K, E, T] =
		KinFactory.delay { () => TableKin.one(table, pk(table.row)).as[T] }



	//use a differently named method so definitions in subclasses don't mess with overload rules
	protected def fkimpl[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                    (name :String, property :S => R, buffs :Buff[R]*)
	                    (table :RelVar[M], key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, X, R])
			:ForeignKeyColumnMapping[M, K, R, O]

	/*  Not null single column foreign key using an arbitrary RelatedEntityFactory                            */

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, X, R])
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](name, property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit table :RelVar[M]) :ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](name, property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](name, property, buffs :_*)(table, key(_), reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (reference :RelatedEntityFactory[K, E, X, R])
	                (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](name, property, buffs :_*)(table, key(_), reference)

	/*  Not null single column foreign key with reflected names, using an arbitrary RelatedEntityFactory            */

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit entityType :TypeTag[S]) :ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit entityType :TypeTag[S], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit entityType :TypeTag[S],
	                          key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (reference :RelatedEntityFactory[K, E, X, R])
	                (implicit entityType :TypeTag[S], table :RelVar[M],
	                          key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	/*  Not null single column foreign key as Kin[E]            */

	protected def fk[M[A] <: RefinedMapping[E, A], K, E]
	                (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[M, K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](name, value, buffs :_*)(table, key, kinFactory[M, K, E, E](table, key).required)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E]
	                (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (key :M[_] => ColumnMapping[K, _])
	                (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](name, value, buffs :_*)(table, key, kinFactory[M, K, E, E](table, key).required)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (table :RelVar[M])
	                (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](name, value, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](name, value, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)

	/*  Not null single column foreign key with reflected names, as Kin[E]            */

	protected def fk[M[A] <: RefinedMapping[E, A], K, E]
	                (value :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E])
			:ForeignKeyColumnMapping[M, K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E]
	                (value :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => ColumnMapping[K, _])
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[M, K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (value :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
	                 table :RelVar[M], key :PrimaryKeyColumnOf[M]) :ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)



	//use a differently named method so definitions in subclasses don't mess with overload rules
	protected def fkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
	                    (property :S => R, buffs :Buff[R]*)
	                    (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
	                    (rename :String => String) :ForeignKeyMapping[M, C, K, R, O]

	/*  Not null multi column foreign key, using an arbitrary RelatedEntityFactory            */

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
	                (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, X, R](property, buffs :_*)(table, key, reference)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)(key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
	                (rename :String => String)(implicit table :RelVar[M]) :ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, X, R](property, buffs :_*)(table, key, reference)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)(table :RelVar[M], reference :RelatedEntityFactory[K, E, X, R])
	                (rename :String => String)
	                (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, X, R](property, buffs :_*)(table, key(_), reference)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)(reference :RelatedEntityFactory[K, E, X, R])
	                (rename :String => String)
	                (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K })
			:ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, X, R](property, buffs :_*)(table, key(_), reference)(rename)

	/*  Not null multi column foreign key with a column prefix, using an arbitrary RelatedEntityFactory            */

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
			:ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, X, R](property, buffs :_*)(table, key, reference)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit table :RelVar[M]) :ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, X, R](property, buffs :_*)(table, key, reference)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (table :RelVar[M], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, X, R](property, buffs :_*)(table, key(_), reference)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (reference :RelatedEntityFactory[K, E, X, R])
	                (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, X, R](property, buffs :_*)(table, key(_), reference)(columnPrefix + _)

	/*  Not null multi column foreign key, as Kin[E]            */

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
	                (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => C[_])(rename :String => String)
	                (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])(rename :String => String)
	                (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(rename)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(rename :String => String)
	                (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(rename)

	/*  Not null multi column foreign key with a column prefix, as Kin[E]            */

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                (table :RelVar[M], key :M[_] => C[_])
	                (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(key :M[_] => C[_])
	                (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(columnPrefix + _)

	protected def fk[M[A] <: RefinedMapping[E, A], E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(columnPrefix + _)






	protected def optfkimpl[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                       (name :String, value :S => R, buffs :Buff[R]*)
	                       (table :RelVar[M], pk :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, T, R](name, value, SelectDefault(reference.nonexistent) +: buffs :_*)(table, pk, reference) //todo: Nullable on the inner column

	/*  Nullable single column foreign key, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R], table :RelVar[M], key :M[_] => ColumnMapping[K, _])
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M]) :ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key(_), reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key(_), reference)

	/*  Nullable single column foreign key with reflecated names, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S]) :ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (key :M[_] => ColumnMapping[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S],
	                             key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S], table :RelVar[M],
	                             key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: ColumnMapping[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	/*  Nullable single column foreign key, as Kin[E]            */

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E]
	                   (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                   (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[M, K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](name, value, buffs :_*)(table, key, kinFactory[M, K, E, E](table, key))

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E]
	                   (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (key :M[_] => ColumnMapping[K, _])
	                   (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](name, value, buffs :_*)(table, key, kinFactory[M, K, E, E](table, key))

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M])
	                   (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](name, value, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_))
		)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (name :String, value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](name, value, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_))
		)

	/*  Nullable single column foreign key with reflected names, as Kin[E]            */

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E]
	                   (value :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E])
			:ForeignKeyColumnMapping[M, K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key)
		)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E]
	                   (value :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => ColumnMapping[K, _])
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[M, K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key)
		)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (value :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_))
		)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (value :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
	                    table :RelVar[M], key :PrimaryKeyColumnOf[M]) :ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_))
		)



	protected def optfkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                       (property :S => R, buffs :Buff[R]*)
	                       (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                       (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, T, R](property, Nullable[R] +: SelectDefault(reference.nonexistent) +: buffs :_*)(
			table, key, reference //todo: this is lazy, we should give Nullable only to columns
		)(rename)

	/*  Nullable multi column foreign key, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)(key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String)(implicit table :RelVar[M]) :ForeignKeyMapping[M, C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)(table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String)
	                   (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)(reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String)
	                   (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K })
			:ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(rename)

	/*  Nullable multi column foreign key with a column prefix, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
			:ForeignKeyMapping[M, C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M]) :ForeignKeyMapping[M, C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K })
			:ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(columnPrefix + _)

	/*  Nullable multi column foreign key, as Kin[E]            */

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
	                   (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => C[_])(rename :String => String)
	                   (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])(rename :String => String)
	                   (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(rename)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(rename :String => String)
	                   (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(rename)

	/*  Nullable multi column foreign key with a column prefix, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M], key :M[_] => C[_])
	                   (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(key :M[_] => C[_])
	                   (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                   (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(columnPrefix + _)

	protected def optfk[M[A] <: RefinedMapping[E, A], E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                   (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(columnPrefix + _)




	/*  Not null single column foreign key as One[E]                                                           */

	protected def one[M[A] <: RefinedMapping[E, A], K, E]
	                 (name :String, value :S => One[E], buffs :Buff[One[E]]*)
	                 (table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                 (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[M, K, One[E], O] =
		fkimpl[M, K, E, E, One[E]](name, value, buffs :_*)(table, key, requiredKinFactory[M, K, E, E](table, key).narrow)

	protected def one[M[A] <: RefinedMapping[E, A], K, E]
	                 (name :String, value :S => One[E], buffs :Buff[One[E]]*)
	                 (key :M[_] => ColumnMapping[K, _])
	                 (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, One[E], O] =
		fkimpl[M, K, E, E, One[E]](name, value, buffs :_*)(table, key, requiredKinFactory[M, K, E, E](table, key).narrow)

	protected def one[M[A] <: RefinedMapping[E, A], E]
	                 (name :String, value :S => One[E], buffs :Buff[One[E]]*)
	                 (table :RelVar[M])
	                 (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, One[E], O] =
		fkimpl[M, key.Key, E, E, One[E]](name, value, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)

	protected def one[M[A] <: RefinedMapping[E, A], E]
	                 (name :String, value :S => One[E], buffs :Buff[One[E]]*)
	                 (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, One[E], O] =
		fkimpl[M, key.Key, E, E, One[E]](name, value, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)

	/*  Not null single column foreign key with a reflected name, as One[E]            */

	protected def one[M[A] <: RefinedMapping[E, A], K, E]
	                 (value :S => One[E], buffs :Buff[One[E]]*)(table :RelVar[M], key :M[_] => ColumnMapping[K, _])
	                 (implicit entityType :TypeTag[S], referencedType :TypeTag[E])
			:ForeignKeyColumnMapping[M, K, One[E], O] =
		fkimpl[M, K, E, E, One[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)

	protected def one[M[A] <: RefinedMapping[E, A], K, E]
	                 (value :S => One[E], buffs :Buff[One[E]]*)(key :M[_] => ColumnMapping[K, _])
	                 (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[M, K, One[E], O] =
		fkimpl[M, K, E, E, One[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)

	protected def one[M[A] <: RefinedMapping[E, A], E]
	                 (value :S => One[E], buffs :Buff[One[E]]*)(table :RelVar[M])
	                 (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, One[E], O] =
		fkimpl[M, key.Key, E, E, One[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)

	protected def one[M[A] <: RefinedMapping[E, A], E]
	                 (value :S => One[E], buffs :Buff[One[E]]*)
	                 (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
	                  table :RelVar[M], key :PrimaryKeyColumnOf[M]) :ForeignKeyColumnMapping[M, key.Key, One[E], O] =
		fkimpl[M, key.Key, E, E, One[E]](PropertyPath.nameOf(value), value, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)



	/*  Not null multi column foreign key as One[E]            */

	protected def one[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)
	                 (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
	                 (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, One[E], O] =
		fkimpl[M, C, K, E, E, One[E]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)(rename)

	protected def one[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)(key :M[_] => C[_])(rename :String => String)
	                 (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, One[E], O] =
		fkimpl[M, C, K, E, E, One[E]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)(rename)

	protected def one[M[A] <: RefinedMapping[E, A], E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)(table :RelVar[M])(rename :String => String)
	                 (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, One[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, One[E]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)(rename)

	protected def one[M[A] <: RefinedMapping[E, A], E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)(rename :String => String)
	                 (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, One[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, One[E]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)(rename)

	/*  Not null multi column foreign key with a column prefix, as One[E]            */

	protected def one[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                 (property :S => One[E], columnPrefix :String, buffs :Buff[One[E]]*)
	                 (table :RelVar[M], key :M[_] => C[_])
	                 (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, One[E], O] =
		fkimpl[M, C, K, E, E, One[E]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)(columnPrefix + _)

	protected def one[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
	                 (property :S => One[E], columnPrefix :String, buffs :Buff[One[E]]*)(key :M[_] => C[_])
	                 (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, One[E], O] =
		fkimpl[M, C, K, E, E, One[E]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)(columnPrefix + _)

	protected def one[M[A] <: RefinedMapping[E, A], E]
	                 (property :S => One[E], columnPrefix :String, buffs :Buff[One[E]]*)(table :RelVar[M])
	                 (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, One[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, One[E]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)(columnPrefix + _)

	protected def one[M[A] <: RefinedMapping[E, A], E]
	                 (property :S => One[E], columnPrefix :String, buffs :Buff[One[E]]*)
	                 (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, One[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, One[E]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)(columnPrefix + _)




//	/*  Not null single column foreign key as Supposed[E]                                                                         */
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], K, E]
//	                      (name :String, value :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                      (table :RelVar[M], key :M[_] => ColumnMapping[K, _])
//	                      (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[M, K, Supposed[E], O] =
//		fkimpl[M, K, E, E, Supposed[E]](name, value, buffs :_*)(table, key, requiredKinFactory[M, K, E, E](table, key).narrow)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], K, E]
//	                      (name :String, value :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                      (key :M[_] => ColumnMapping[K, _])
//	                      (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, Supposed[E], O] =
//		fkimpl[M, K, E, E, Supposed[E]](name, value, buffs :_*)(table, key, requiredKinFactory[M, K, E, E](table, key).narrow)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], E]
//	                      (name :String, value :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                      (table :RelVar[M])
//	                      (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
//			:ForeignKeyColumnMapping[M, key.Key, Supposed[E], O] =
//		fkimpl[M, key.Key, E, E, Supposed[E]](name, value, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], E]
//	                      (name :String, value :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                      (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
//			:ForeignKeyColumnMapping[M, key.Key, Supposed[E], O] =
//		fkimpl[M, key.Key, E, E, Supposed[E]](name, value, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)
//
//	/*  Not null single column foreign key with reflected names, as Supposed[E]            */
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], K, E]
//	                      (value :S => Supposed[E], buffs :Buff[Supposed[E]]*)(table :RelVar[M], key :M[_] => ColumnMapping[K, _])
//	                      (implicit entityType :TypeTag[S], referencedType :TypeTag[E])
//			:ForeignKeyColumnMapping[M, K, Supposed[E], O] =
//		fkimpl[M, K, E, E, Supposed[E]](PropertyPath.nameOf(value), value, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], K, E]
//	                      (value :S => Supposed[E], buffs :Buff[Supposed[E]]*)(key :M[_] => ColumnMapping[K, _])
//	                      (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
//			:ForeignKeyColumnMapping[M, K, Supposed[E], O] =
//		fkimpl[M, K, E, E, Supposed[E]](PropertyPath.nameOf(value), value, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], E]
//	                      (value :S => Supposed[E], buffs :Buff[Supposed[E]]*)(table :RelVar[M])
//	                      (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
//			:ForeignKeyColumnMapping[M, key.Key, Supposed[E], O] =
//		fkimpl[M, key.Key, E, E, Supposed[E]](PropertyPath.nameOf(value), value, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], E]
//	                      (value :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                      (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
//	                       table :RelVar[M], key :PrimaryKeyColumnOf[M]) :ForeignKeyColumnMapping[M, key.Key, Supposed[E], O] =
//		fkimpl[M, key.Key, E, E, Supposed[E]](PropertyPath.nameOf(value), value, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)
//
//
//	/*  Not null multi column foreign key as Supposed[E]            */
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
//	                     (property :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                     (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
//	                     (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Supposed[E], O] =
//		fkimpl[M, C, K, E, E, Supposed[E]](property, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)(rename)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
//	                      (property :S => Supposed[E], buffs :Buff[Supposed[E]]*)(key :M[_] => C[_])(rename :String => String)
//	                      (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Supposed[E], O] =
//		fkimpl[M, C, K, E, E, Supposed[E]](property, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)(rename)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], E]
//	                      (property :S => Supposed[E], buffs :Buff[Supposed[E]]*)(table :RelVar[M])(rename :String => String)
//	                      (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
//			:ForeignKeyMapping[M, key.PKMapping, key.Key, Supposed[E], O] =
//		fkimpl[M, key.PKMapping, key.Key, E, E, Supposed[E]](property, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)(rename)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], E]
//	                      (property :S => Supposed[E], buffs :Buff[Supposed[E]]*)(rename :String => String)
//	                      (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
//			:ForeignKeyMapping[M, key.PKMapping, key.Key, Supposed[E], O] =
//		fkimpl[M, key.PKMapping, key.Key, E, E, Supposed[E]](property, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)(rename)
//
//	/*  Not null multi column foreign key with a column prefix, as Supposed[E]            */
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
//	                     (property :S => Supposed[E], columnPrefix :String, buffs :Buff[Supposed[E]]*)
//	                     (table :RelVar[M], key :M[_] => C[_])
//	                     (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Supposed[E], O] =
//		fkimpl[M, C, K, E, E, Supposed[E]](property, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)(columnPrefix + _)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E]
//	                      (property :S => Supposed[E], columnPrefix :String, buffs :Buff[Supposed[E]]*)(key :M[_] => C[_])
//	                      (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Supposed[E], O] =
//		fkimpl[M, C, K, E, E, Supposed[E]](property, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)(columnPrefix + _)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], E]
//	                      (property :S => Supposed[E], columnPrefix :String, buffs :Buff[Supposed[E]]*)(table :RelVar[M])
//	                      (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
//			:ForeignKeyMapping[M, key.PKMapping, key.Key, Supposed[E], O] =
//		fkimpl[M, key.PKMapping, key.Key, E, E, Supposed[E]](property, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)(columnPrefix + _)
//
//	protected def supposed[M[A] <: RefinedMapping[E, A], E]
//	                      (property :S => Supposed[E], columnPrefix :String, buffs :Buff[Supposed[E]]*)
//	                      (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
//			:ForeignKeyMapping[M, key.PKMapping, key.Key, Supposed[E], O] =
//		fkimpl[M, key.PKMapping, key.Key, E, E, Supposed[E]](property, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)(columnPrefix + _)




	/* Target of a foreign key column, as arbitrary RelatedEntityFactory                                       */

/*
	protected def inverseFKImpl[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                           (property :S => R, key :ColumnMapping[K, O], reference :RelatedEntityFactory[K, E, X, R],
	                            buffs :Buff[R]*)
	                           (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[MappingAt, K, _, _])
			:ForeignKeyColumnMapping[M, K, R, O]

	protected def inverseFK[M[A] <: RefinedMapping[E, A], K, E, X, R]
	                       (property :S => R, key :ColumnMapping[K, O], reference :RelatedEntityFactory[K, E, X, R],
	                        buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[MappingAt, K, _, _])
			:ForeignKeyColumnMapping[M, K, R, O] =
		inverseFKImpl[M, K, E, X, R](property, key, reference, buffs :_*)(table, fk)

	protected def inverseFK[T[A] <: RefinedMapping[S, A], M[A] <: RefinedMapping[E, A], K, E, X, R]
	                       (property :S => R, reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[T, K, _, _])
	                       (implicit referencedType :TypeTag[E],
	                        pk :PrimaryKeyOf[T] { type PKMapping[A] <: ColumnMapping[K, A] }, self :this.type <:< T[O])
			:ForeignKeyColumnMapping[M, K, R, O] =
		inverseFKImpl[M, K, E, X, R](property, pk(self(this)), reference, buffs :_*)(table, fk)
*/

	/* Target of a foreign key column, as Kin[X]                                     */

/*
	protected def inverseFK[M[A] <: RefinedMapping[E, A], K, E, X]
	                       (property :S => Kin[X], key :ColumnMapping[K, O], buffs :Buff[Kin[X]]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[MappingAt, K, _, _])
	                       (implicit referencedType :TypeTag[E], composition :X ComposedOf E)
			:ForeignKeyColumnMapping[M, K, Kin[X], O] =
		inverseFKImpl[M, K, E, X, Kin[X]](property, key, kinFactory[M, K, E, X](table, fk(_).key), buffs :_*)(table, fk)

	protected def inverseFK[T[A] <: RefinedMapping[S, A], M[A] <: RefinedMapping[E, A], K, E, X]
	                       (property :S => Kin[X], buffs :Buff[Kin[X]]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[T, K, _, _])
	                       (implicit referencedType :TypeTag[E],
	                        pk :PrimaryKeyOf[T] { type PKMapping[A] <: ColumnMapping[K, A] }, self :this.type <:< T[O],
	                        composition :X ComposedOf E)
			:ForeignKeyColumnMapping[M, K, Kin[X], O] =
		inverseFKImpl[M, K, E, X, Kin[X]](
			property, pk(self(this)), kinFactory[M, K, E, X](table, fk(_).key), buffs :_*
		)(table, fk)
*/

	/* Target of a multi column foreign key, as arbitrary RelatedEntityFactory                                     */

	protected def inverseFKImpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
	                           (property :S => R, key :C[O], reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
	                           (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
			:ForeignKeyMapping[M, C, K, R, O]

	protected def inverseFK[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
	                       (property :S => R, key :C[O], reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
			:ForeignKeyMapping[M, C, K, R, O] =
		inverseFKImpl[M, C, K, E, X, R](property, key, reference, buffs :_*)(table, fk)

	protected def inverseFK[T[A] <: MappingAt[A], M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
	                       (property :S => R, reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[T, C, K, _, _])
	                       (implicit pk :PrimaryKeyOf[T] { type PKMapping[A] = C[A] }, self :this.type <:< T[O])
			:ForeignKeyMapping[M, C, K, R, O] =
		inverseFKImpl[M, C, K, E, X, R](property, pk(self(this)), reference, buffs :_*)(table, fk)

	/* Target of a multi column foreign key, as Kin[X]                                     */

	protected def inverseFK[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X]
	                       (property :S => Kin[X], key :C[O], buffs :Buff[Kin[X]]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
	                       (implicit referencedType :TypeTag[E], composition :X ComposedOf E)
			:ForeignKeyMapping[M, C, K, Kin[X], O] =
		inverseFKImpl[M, C, K, E, X, Kin[X]](
			property, key, kinFactory[M, K, E, X](table, fk(_).key), buffs :_*
		)(table, fk)

	protected def inverseFK[T[A] <: RefinedMapping[S, A], M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X]
	                       (property :S => Kin[X], buffs :Buff[Kin[X]]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[T, C, K, _, _])
	                       (implicit referencedType :TypeTag[E], pk :PrimaryKeyOf[T] { type PKMapping[A] = C[A] },
	                        self :this.type <:< T[O], composition :X ComposedOf E)
			:ForeignKeyMapping[M, C, K, Kin[X], O] =
		inverseFKImpl[M, C, K, E, X, Kin[X]](
			property, pk(self(this)), kinFactory[M, K, E, X](table, fk(_).key), buffs :_*
		)(table, fk)

}

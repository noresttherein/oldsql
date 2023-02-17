package net.noresttherein.oldsql.schema.bases

import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.model.{ComposedOf, Kin, KinFactory, PropertyPath, RelatedEntityFactory, Restraint}
import net.noresttherein.oldsql.model.Kin.{Derived, One}
import net.noresttherein.oldsql.model.KinFactory.DerivedKinFactory
import net.noresttherein.oldsql.schema.{Buff, PrimaryKeyOf}
import net.noresttherein.oldsql.schema.Buff.{Nullable, SelectDefault}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.PrimaryKeyOf.PrimaryKeyColumnOf
import net.noresttherein.oldsql.schema.RelVar
import net.noresttherein.oldsql.schema.bits.{ForeignKeyColumnMapping, ForeignKeyMapping, JoinedEntityColumn, JoinedEntityComponent, JoinTableCollectionMapping, TableKin}






/** A mix-in trait providing factory methods for components (and columns) which reference other tables.
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
  *           (|table :RelVar[M],| |key :M[_] => TypedColumn[K, _],| |reference :RelatedEntityFactory[K, E, T, R]|)
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
  *   1. Factory methods for optional foreign key columns (nullable in the database, and using
  *      [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]] value
  *      of the used reference type) - same as 1), but named `optfk` instead of `fk`.
  *   1. Factory methods for optional foreign key components (foreign keys consisting of multiple nullable columns,
  *      which use the [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]] property
  *      of the used [[net.noresttherein.oldsql.model.RelatedEntityFactory RelatedEntityFactory]]) - same as 2),
  *      but named  `optfk` instead of `fk`.
  *
  *
  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame]]
  * @see [[net.noresttherein.oldsql.schema.bases.SimpleMapping]]
  * @author Marcin Mościcki
  */ //todo: document one, optOne, many, inverseFK
trait RelatedMapping[S, O] extends BaseMapping[S, O] {
	//todo: why so many methods are commented out?
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
	protected def kinFactory[M[A] <: TypedMapping[E, A], K, E, T]
	                        (table :RelVar[M], key :M[_] => TypedMapping[K, _])
	                        (implicit composition :T ComposedOf E, referencedType :TypeTag[E]) :KinFactory[K, E, T] =
		restrainedKinFactory[M, K, E, T](table, key)

	/** A lazy kin factory producing [[net.noresttherein.oldsql.model.Kin.Restrained$ Restrained]] kin which
	  * restrict the property mapping to the `key` component of the referenced table (which will be typically
	  * the table's primary key when used for creating a foreign key mapping, and the table's foreign key pointing
	  * to this table for inverse foreign key mappings).
	  */
	protected def restrainedKinFactory[M[A] <: TypedMapping[E, A], K, E, T]
	              (table :RelVar[M], key :M[_] => TypedMapping[K, _])
	              (implicit composition :T ComposedOf E, referencedType :TypeTag[E]) :KinFactory[K, E, T] =
		KinFactory.delay {
			val comp = key(table.row).withOrigin[Unit]
			val extract = table.row[Unit](comp)
			extract.requisite match {
				case Got(property) => Kin(Restraint.Property(property)).as[T]
				case _ => Kin(Restraint.Property(extract.optional).flatten).as[T]
			}
		}

	/** A lazy kin factory producing [[net.noresttherein.oldsql.schema.bits.TableKin TableKin]] carrying
	  * the referenced table and all columns of the referenced `key`, together with their values.
	  */
	protected def tableKinFactory[M[A] <: TypedMapping[E, A], K, E, T]
	              (table :RelVar[M], key :M[_] => TypedMapping[K, _])
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
	protected def requiredKinFactory[M[A] <: TypedMapping[E, A], K, E, T]
	                                (table :RelVar[M], pk :M[_] => TypedMapping[K, _])
	                                (implicit composition :T ComposedOf E, referenceType :TypeTag[E])
			:DerivedKinFactory[K, E, T] =
		requiredRestrainedKinFactory[M, K, E, T](table, pk)

	protected def requiredRestrainedKinFactory[M[A] <: TypedMapping[E, A], K, E, T]
	                                          (table :RelVar[M], pk :M[_] => TypedMapping[K, _])
	                                          (implicit composition :T ComposedOf E, referenceType :TypeTag[E])
			:DerivedKinFactory[K, E, T] =
		KinFactory.delay { () =>
			val key = pk(table.row).withOrigin[Unit]
			val extract = table.row[Unit](key)
			extract.requisite match {
				case Got(property) => Kin.Restrained.required(Restraint.Property(property)).as[T]
				case _ => Kin.Restrained.required(Restraint.Property(extract.optional).flatten).as[T]
			}
		}

	protected def requiredTableKinFactory[M[A] <: TypedMapping[E, A], K, E, T]
	                                     (table :RelVar[M], pk :M[_] => TypedMapping[K, _])
	                                     (implicit composition :T ComposedOf E, referenceType :TypeTag[E])
			:DerivedKinFactory[K, E, T] =
		KinFactory.delay { () => TableKin.required(table, pk(table.row)).as[T] }



	/** Prefix added to given names of all created instances of this.Column[_]. Defaults to "", subclasses may override.
	  * Overrides with a `val` or `var` (or any used in their implementation) must happen ''before'' any components
	  * of this mapping are initialized - in practice before any column declarations.
	  */ //todo: columnSuffix
	protected def columnPrefix = ""

	//consider: making all factory arguments lazy as in many, instead of creating lazy factories.
	//use a differently named method so definitions in subclasses don't mess with overload rules //todo: rename - capitalize Impl
	protected def fkimpl[M[A] <: TypedMapping[E, A], K, E, X, R]
	                    (name :String, property :S => R, buffs :Buff[R]*)
	                    (table :RelVar[M], key :M[_] => TypedColumn[K, _], reference :RelatedEntityFactory[K, E, X, R])
			:ForeignKeyColumnMapping[M, K, R, O]

	/*  Not null single column foreign key using an arbitrary RelatedEntityFactory                            */

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => TypedColumn[K, _], reference :RelatedEntityFactory[K, E, X, R])
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](name, property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (key :M[_] => TypedColumn[K, _], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit table :RelVar[M]) :ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](name, property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: TypedColumn[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](name, property, buffs :_*)(table, key(_), reference)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (name :String, property :S => R, buffs :Buff[R]*)
	                (reference :RelatedEntityFactory[K, E, X, R])
	                (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: TypedColumn[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](name, property, buffs :_*)(table, key(_), reference)

	/*  Not null single column foreign key with reflected names, using an arbitrary RelatedEntityFactory            */

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => TypedColumn[K, _], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit entityType :TypeTag[S]) :ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (key :M[_] => TypedColumn[K, _], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit entityType :TypeTag[S], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit entityType :TypeTag[S],
	                          key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: TypedColumn[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (reference :RelatedEntityFactory[K, E, X, R])
	                (implicit entityType :TypeTag[S], table :RelVar[M],
	                          key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: TypedColumn[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		fkimpl[M, K, E, X, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	/*  Not null single column foreign key as Kin[E]            */

	protected def fk[M[A] <: TypedMapping[E, A], K, E]
	                (name :String, property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (table :RelVar[M], key :M[_] => TypedColumn[K, _])
	                (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[M, K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](name, property, buffs :_*)(table, key, kinFactory[M, K, E, E](table, key).required)

	protected def fk[M[A] <: TypedMapping[E, A], K, E]
	                (name :String, property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (key :M[_] => TypedColumn[K, _])
	                (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](name, property, buffs :_*)(table, key, kinFactory[M, K, E, E](table, key).required)

	protected def fk[M[A] <: TypedMapping[E, A], E]
	                (name :String, property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (table :RelVar[M])
	                (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](name, property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)

	protected def fk[M[A] <: TypedMapping[E, A], E]
	                (name :String, property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](name, property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)

	/*  Not null single column foreign key with reflected names, as Kin[E]            */

	protected def fk[M[A] <: TypedMapping[E, A], K, E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M], key :M[_] => TypedColumn[K, _])
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E])
			:ForeignKeyColumnMapping[M, K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)

	protected def fk[M[A] <: TypedMapping[E, A], K, E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => TypedColumn[K, _])
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[M, K, Kin[E], O] =
		fkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)

	protected def fk[M[A] <: TypedMapping[E, A], E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)

	protected def fk[M[A] <: TypedMapping[E, A], E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
	                 table :RelVar[M], key :PrimaryKeyColumnOf[M]) :ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		fkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)



	//use a differently named method so definitions in subclasses don't mess with overload rules
	protected def fkimpl[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
	                    (property :S => R, buffs :Buff[R]*)
	                    (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
	                    (rename :String => String) :ForeignKeyMapping[M, C, K, R, O]

	/*  Not null multi column foreign key, using an arbitrary RelatedEntityFactory            */

	protected def fk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
	                (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, X, R](property, buffs :_*)(table, key, reference)(rename)

	protected def fk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)(key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
	                (rename :String => String)(implicit table :RelVar[M]) :ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, X, R](property, buffs :_*)(table, key, reference)(rename)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)(table :RelVar[M], reference :RelatedEntityFactory[K, E, X, R])
	                (rename :String => String)
	                (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, X, R](property, buffs :_*)(table, key(_), reference)(rename)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (property :S => R, buffs :Buff[R]*)(reference :RelatedEntityFactory[K, E, X, R])
	                (rename :String => String)
	                (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K })
			:ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, X, R](property, buffs :_*)(table, key(_), reference)(rename)

	/*  Not null multi column foreign key with a column prefix, using an arbitrary RelatedEntityFactory            */

	protected def fk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
			:ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, X, R](property, buffs :_*)(table, key, reference)(this.columnPrefix + columnPrefix + _)

	protected def fk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (key :M[_] => C[_], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit table :RelVar[M]) :ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, X, R](property, buffs :_*)(table, key, reference)(this.columnPrefix + columnPrefix + _)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (table :RelVar[M], reference :RelatedEntityFactory[K, E, X, R])
	                (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, X, R](property, buffs :_*)(
			table, key(_), reference)(this.columnPrefix + columnPrefix + _)

	protected def fk[M[A] <: TypedMapping[E, A], K, E, X, R]
	                (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                (reference :RelatedEntityFactory[K, E, X, R])
	                (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		fkimpl[M, key.PKMapping, K, E, X, R](property, buffs :_*)(
			table, key(_), reference)(this.columnPrefix + columnPrefix + _)

	/*  Not null multi column foreign key, as Kin[E]            */

	protected def fk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
	                (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(rename)

	protected def fk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => C[_])(rename :String => String)
	                (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(rename)

	protected def fk[M[A] <: TypedMapping[E, A], E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])(rename :String => String)
	                (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(rename)

	protected def fk[M[A] <: TypedMapping[E, A], E]
	                (property :S => Kin[E], buffs :Buff[Kin[E]]*)(rename :String => String)
	                (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(rename)

	/*  Not null multi column foreign key with a column prefix, as Kin[E]            */

	protected def fk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                (table :RelVar[M], key :M[_] => C[_])
	                (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(this.columnPrefix + columnPrefix + _)

	protected def fk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(key :M[_] => C[_])
	                (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		fkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(this.columnPrefix + columnPrefix + _)

	protected def fk[M[A] <: TypedMapping[E, A], E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(this.columnPrefix + columnPrefix + _)

	protected def fk[M[A] <: TypedMapping[E, A], E]
	                (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(this.columnPrefix + columnPrefix + _)






	protected def optfkimpl[M[A] <: TypedMapping[E, A], K, E, T, R]
	                       (name :String, property :S => R, buffs :Buff[R]*)
	                       (table :RelVar[M], pk :M[_] => TypedColumn[K, _], reference :RelatedEntityFactory[K, E, T, R])
			:ForeignKeyColumnMapping[M, K, R, O] =  //todo: Nullable on the inner column
		fkimpl[M, K, E, T, R](name, property, SelectDefault(reference.nonexistent) +: buffs :_*)(table, pk, reference)

	/*  Nullable single column foreign key, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R], table :RelVar[M], key :M[_] => TypedColumn[K, _])
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (key :M[_] => TypedColumn[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M]) :ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: TypedColumn[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key(_), reference)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (name :String, property :S => R, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: TypedColumn[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](name, property, buffs :_*)(table, key(_), reference)

	/*  Nullable single column foreign key with reflected names, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], key :M[_] => TypedColumn[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S]) :ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (key :M[_] => TypedColumn[K, _], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key, reference)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S],
	                             key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: TypedColumn[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit entityType :TypeTag[S], table :RelVar[M],
	                             key :PrimaryKeyOf[M] { type Key = K; type PKMapping[A] <: TypedColumn[K, A] })
			:ForeignKeyColumnMapping[M, K, R, O] =
		optfkimpl[M, K, E, T, R](PropertyPath.nameOf(property), property, buffs :_*)(table, key(_), reference)

	/*  Nullable single column foreign key, as Kin[E]            */

	protected def optfk[M[A] <: TypedMapping[E, A], K, E]
	                   (name :String, property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M], key :M[_] => TypedColumn[K, _])
	                   (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[M, K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](name, property, buffs :_*)(table, key, kinFactory[M, K, E, E](table, key))

	protected def optfk[M[A] <: TypedMapping[E, A], K, E]
	                   (name :String, property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (key :M[_] => TypedColumn[K, _])
	                   (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](name, property, buffs :_*)(table, key, kinFactory[M, K, E, E](table, key))

	protected def optfk[M[A] <: TypedMapping[E, A], E]
	                   (name :String, property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M])
	                   (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](name, property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_))
		)

	protected def optfk[M[A] <: TypedMapping[E, A], E]
	                   (name :String, property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](name, property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_))
		)

	/*  Nullable single column foreign key with reflected names, as Kin[E]            */

	protected def optfk[M[A] <: TypedMapping[E, A], K, E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M], key :M[_] => TypedColumn[K, _])
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E])
			:ForeignKeyColumnMapping[M, K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key)
		)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => TypedColumn[K, _])
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[M, K, Kin[E], O] =
		optfkimpl[M, K, E, E, Kin[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key)
		)

	protected def optfk[M[A] <: TypedMapping[E, A], E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_))
		)

	protected def optfk[M[A] <: TypedMapping[E, A], E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
	                    table :RelVar[M], key :PrimaryKeyColumnOf[M]) :ForeignKeyColumnMapping[M, key.Key, Kin[E], O] =
		optfkimpl[M, key.Key, E, E, Kin[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_))
		)



	protected def optfkimpl[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R]
	                       (property :S => R, buffs :Buff[R]*)
	                       (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                       (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		fkimpl[M, C, K, E, T, R](property, Nullable[R] +: SelectDefault(reference.nonexistent) +: buffs :_*)(
			table, key, reference //todo: this is lazy, we should give Nullable only to columns
		)(rename)

	/*  Nullable multi column foreign key, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)
	                   (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(rename)

	protected def optfk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)(key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String)(implicit table :RelVar[M]) :ForeignKeyMapping[M, C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(rename)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)(table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String)
	                   (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(rename)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (property :S => R, buffs :Buff[R]*)(reference :RelatedEntityFactory[K, E, T, R])
	                   (rename :String => String)
	                   (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K })
			:ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(table, key(_), reference)(rename)

	/*  Nullable multi column foreign key with a column prefix, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (table :RelVar[M], key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
			:ForeignKeyMapping[M, C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(this.columnPrefix + columnPrefix + _)

	protected def optfk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (key :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M]) :ForeignKeyMapping[M, C, K, R, O] =
		optfkimpl[M, C, K, E, T, R](property, buffs :_*)(table, key, reference)(this.columnPrefix + columnPrefix + _)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (table :RelVar[M], reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit key :PrimaryKeyOf[M] { type Key = K }) :ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(
			table, key(_), reference)(this.columnPrefix + columnPrefix + _)

	protected def optfk[M[A] <: TypedMapping[E, A], K, E, T, R]
	                   (property :S => R, columnPrefix :String, buffs :Buff[R]*)
	                   (reference :RelatedEntityFactory[K, E, T, R])
	                   (implicit table :RelVar[M], key :PrimaryKeyOf[M] { type Key = K })
			:ForeignKeyMapping[M, key.PKMapping, K, R, O] =
		optfkimpl[M, key.PKMapping, K, E, T, R](property, buffs :_*)(
			table, key(_), reference)(this.columnPrefix + columnPrefix + _)

	/*  Nullable multi column foreign key, as Kin[E]            */

	protected def optfk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
	                   (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(rename)

	protected def optfk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(key :M[_] => C[_])(rename :String => String)
	                   (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(rename)

	protected def optfk[M[A] <: TypedMapping[E, A], E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(table :RelVar[M])(rename :String => String)
	                   (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(rename)

	protected def optfk[M[A] <: TypedMapping[E, A], E]
	                   (property :S => Kin[E], buffs :Buff[Kin[E]]*)(rename :String => String)
	                   (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(rename)

	/*  Nullable multi column foreign key with a column prefix, using an arbitrary RelatedEntityFactory            */

	protected def optfk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                   (table :RelVar[M], key :M[_] => C[_])
	                   (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(this.columnPrefix + columnPrefix + _)

	protected def optfk[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(key :M[_] => C[_])
	                   (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Kin[E], O] =
		optfkimpl[M, C, K, E, E, Kin[E]](property, buffs :_*)(
			table, key, kinFactory[M, K, E, E](table, key).required
		)(this.columnPrefix + columnPrefix + _)

	protected def optfk[M[A] <: TypedMapping[E, A], E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)(table :RelVar[M])
	                   (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(this.columnPrefix + columnPrefix + _)

	protected def optfk[M[A] <: TypedMapping[E, A], E]
	                   (property :S => Kin[E], columnPrefix :String, buffs :Buff[Kin[E]]*)
	                   (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Kin[E], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Kin[E]](property, buffs :_*)(
			table, key(_), kinFactory[M, key.Key, E, E](table, key(_)).required
		)(this.columnPrefix + columnPrefix + _)




	/*  Not null single column foreign key as One[E]                                                           */

	protected def one[M[A] <: TypedMapping[E, A], K, E]
	                 (name :String, property :S => One[E], buffs :Buff[One[E]]*)
	                 (table :RelVar[M], key :M[_] => TypedColumn[K, _])
	                 (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[M, K, One[E], O] =
		fkimpl[M, K, E, E, One[E]](name, property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)

	protected def one[M[A] <: TypedMapping[E, A], K, E]
	                 (name :String, property :S => One[E], buffs :Buff[One[E]]*)
	                 (key :M[_] => TypedColumn[K, _])
	                 (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, One[E], O] =
		fkimpl[M, K, E, E, One[E]](name, property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)

	protected def one[M[A] <: TypedMapping[E, A], E]
	                 (name :String, property :S => One[E], buffs :Buff[One[E]]*)(table :RelVar[M])
	                 (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, One[E], O] =
		fkimpl[M, key.Key, E, E, One[E]](name, property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)

	protected def one[M[A] <: TypedMapping[E, A], E]
	                 (name :String, property :S => One[E], buffs :Buff[One[E]]*)
	                 (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, One[E], O] =
		fkimpl[M, key.Key, E, E, One[E]](name, property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)

	/*  Not null single column foreign key with a reflected name, as One[E]            */

	protected def one[M[A] <: TypedMapping[E, A], K, E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)(table :RelVar[M], key :M[_] => TypedColumn[K, _])
	                 (implicit entityType :TypeTag[S], referencedType :TypeTag[E])
			:ForeignKeyColumnMapping[M, K, One[E], O] =
		fkimpl[M, K, E, E, One[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)

	protected def one[M[A] <: TypedMapping[E, A], K, E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)(key :M[_] => TypedColumn[K, _])
	                 (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[M, K, One[E], O] =
		fkimpl[M, K, E, E, One[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)

	protected def one[M[A] <: TypedMapping[E, A], E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)(table :RelVar[M])
	                 (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, One[E], O] =
		fkimpl[M, key.Key, E, E, One[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)

	protected def one[M[A] <: TypedMapping[E, A], E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)
	                 (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
	                  table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, One[E], O] =
		fkimpl[M, key.Key, E, E, One[E]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)



	/*  Not null multi column foreign key as One[E]            */

	protected def one[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)
	                 (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
	                 (implicit referencedType :TypeTag[E])
			:ForeignKeyMapping[M, C, K, One[E], O] =
		fkimpl[M, C, K, E, E, One[E]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)(rename)

	protected def one[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)
	                 (key :M[_] => C[_])(rename :String => String)
	                 (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, One[E], O] =
		fkimpl[M, C, K, E, E, One[E]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)(rename)

	protected def one[M[A] <: TypedMapping[E, A], E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)
	                 (table :RelVar[M])(rename :String => String)
	                 (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, One[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, One[E]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)(rename)

	protected def one[M[A] <: TypedMapping[E, A], E]
	                 (property :S => One[E], buffs :Buff[One[E]]*)(rename :String => String)
	                 (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, One[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, One[E]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)(rename)

	/*  Not null multi column foreign key with a column prefix, as One[E]            */

	protected def one[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                 (property :S => One[E], columnPrefix :String, buffs :Buff[One[E]]*)
	                 (table :RelVar[M], key :M[_] => C[_])
	                 (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, One[E], O] =
		fkimpl[M, C, K, E, E, One[E]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)(this.columnPrefix + columnPrefix + _)

	protected def one[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                 (property :S => One[E], columnPrefix :String, buffs :Buff[One[E]]*)(key :M[_] => C[_])
	                 (implicit referenceType :TypeTag[E], table :RelVar[M])
			:ForeignKeyMapping[M, C, K, One[E], O] =
		fkimpl[M, C, K, E, E, One[E]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
		)(this.columnPrefix + columnPrefix + _)

	protected def one[M[A] <: TypedMapping[E, A], E]
	                 (property :S => One[E], columnPrefix :String, buffs :Buff[One[E]]*)(table :RelVar[M])
	                 (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, One[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, One[E]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)(this.columnPrefix + columnPrefix + _)

	protected def one[M[A] <: TypedMapping[E, A], E]
	                 (property :S => One[E], columnPrefix :String, buffs :Buff[One[E]]*)
	                 (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, One[E], O] =
		fkimpl[M, key.PKMapping, key.Key, E, E, One[E]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
		)(this.columnPrefix + columnPrefix + _)




	/*  Not null single column foreign key as Option[One[E]]                                            */
	//consider: renaming to optOne or just opt/option
	protected def optone[M[A] <: TypedMapping[E, A], K, E]
	                    (name :String, property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)
	                    (table :RelVar[M], key :M[_] => TypedColumn[K, _])
	                    (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[M, K, Option[One[E]], O] =
		optfkimpl[M, K, E, E, Option[One[E]]](name, property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow.optional
		)

	protected def optone[M[A] <: TypedMapping[E, A], K, E]
	                    (name :String, property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)
	                    (key :M[_] => TypedColumn[K, _])
	                    (implicit referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[M, K, Option[One[E]], O] =
		optfkimpl[M, K, E, E, Option[One[E]]](name, property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow.optional
		)

	protected def optone[M[A] <: TypedMapping[E, A], E]
                        (name :String, property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)(table :RelVar[M])
	                    (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Option[One[E]], O] =
		optfkimpl[M, key.Key, E, E, Option[One[E]]](name, property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow.optional
		)

	protected def optone[M[A] <: TypedMapping[E, A], E]
	                    (name :String, property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)
	                    (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Option[One[E]], O] =
		optfkimpl[M, key.Key, E, E, Option[One[E]]](name, property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow.optional
		)

	/*  Not null single column foreign key with a reflected name, as Option[One[E]]            */

	protected def optone[M[A] <: TypedMapping[E, A], K, E]
	                    (property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)
	                    (table :RelVar[M], key :M[_] => TypedColumn[K, _])
	                    (implicit entityType :TypeTag[S], referencedType :TypeTag[E])
			:ForeignKeyColumnMapping[M, K, Option[One[E]], O] =
		optfkimpl[M, K, E, E, Option[One[E]]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow.optional
		)

	protected def optone[M[A] <: TypedMapping[E, A], K, E]
	                    (property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)(key :M[_] => TypedColumn[K, _])
	                    (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
			:ForeignKeyColumnMapping[M, K, Option[One[E]], O] =
		optfkimpl[M, K, E, E, Option[One[E]]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow.optional
		)

	protected def optone[M[A] <: TypedMapping[E, A], E]
	                    (property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)(table :RelVar[M])
	                    (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Option[One[E]], O] =
		optfkimpl[M, key.Key, E, E, Option[One[E]]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow.optional
		)

	protected def optone[M[A] <: TypedMapping[E, A], E]
	                    (property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)
	                    (implicit entityType :TypeTag[S], referencedType :TypeTag[E],
	                     table :RelVar[M], key :PrimaryKeyColumnOf[M])
			:ForeignKeyColumnMapping[M, key.Key, Option[One[E]], O] =
		optfkimpl[M, key.Key, E, E, Option[One[E]]](PropertyPath.nameOf(property), property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow.optional
		)



	/*  Not null multi column foreign key as Option[One[E]]            */

	protected def optone[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                    (property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)
	                    (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
	                    (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Option[One[E]], O] =
		optfkimpl[M, C, K, E, E, Option[One[E]]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow.optional
		)(rename)

	protected def optone[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                    (property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)
	                    (key :M[_] => C[_])(rename :String => String)
	                    (implicit referenceType :TypeTag[E], table :RelVar[M])
			:ForeignKeyMapping[M, C, K, Option[One[E]], O] =
		optfkimpl[M, C, K, E, E, Option[One[E]]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow.optional
		)(rename)

	protected def optone[M[A] <: TypedMapping[E, A], E]
	                    (property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)
	                    (table :RelVar[M])(rename :String => String)
	                    (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Option[One[E]], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Option[One[E]]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow.optional
		)(rename)

	protected def optone[M[A] <: TypedMapping[E, A], E]
	                    (property :S => Option[One[E]], buffs :Buff[Option[One[E]]]*)(rename :String => String)
	                    (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Option[One[E]], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Option[One[E]]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow.optional
		)(rename)

	/*  Not null multi column foreign key with a column prefix, as Option[One[E]]            */

	protected def optone[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                    (property :S => Option[One[E]], columnPrefix :String, buffs :Buff[Option[One[E]]]*)
	                    (table :RelVar[M], key :M[_] => C[_])
	                    (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Option[One[E]], O] =
		optfkimpl[M, C, K, E, E, Option[One[E]]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow.optional
		)(this.columnPrefix + columnPrefix + _)

	protected def optone[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
	                    (property :S => Option[One[E]], columnPrefix :String, buffs :Buff[Option[One[E]]]*)
	                    (key :M[_] => C[_])
	                    (implicit referenceType :TypeTag[E], table :RelVar[M])
				:ForeignKeyMapping[M, C, K, Option[One[E]], O] =
		optfkimpl[M, C, K, E, E, Option[One[E]]](property, buffs :_*)(
			table, key, requiredKinFactory[M, K, E, E](table, key).narrow.optional
		)(this.columnPrefix + columnPrefix + _)

	protected def optone[M[A] <: TypedMapping[E, A], E]
	                    (property :S => Option[One[E]], columnPrefix :String, buffs :Buff[Option[One[E]]]*)
	                    (table :RelVar[M])
	                    (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Option[One[E]], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Option[One[E]]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow.optional
		)(this.columnPrefix + columnPrefix + _)

	protected def optone[M[A] <: TypedMapping[E, A], E]
	                    (property :S => Option[One[E]], columnPrefix :String, buffs :Buff[Option[One[E]]]*)
	                    (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
			:ForeignKeyMapping[M, key.PKMapping, key.Key, Option[One[E]], O] =
		optfkimpl[M, key.PKMapping, key.Key, E, E, Option[One[E]]](property, buffs :_*)(
			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow.optional
		)(this.columnPrefix + columnPrefix + _)



//fixme: uncomment and make all relationships work

//	/*  Not null single column foreign key as Supposed[E]                                                                         */
//
//	protected def supposed[M[A] <: TypedMapping[E, A], K, E]
//	                      (name :String, value :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                      (table :RelVar[M], key :M[_] => TypedColumn[K, _])
//	                      (implicit referencedType :TypeTag[E]) :ForeignKeyColumnMapping[M, K, Supposed[E], O] =
//		fkimpl[M, K, E, E, Supposed[E]](name, value, buffs :_*)(table, key, requiredKinFactory[M, K, E, E](table, key).narrow)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], K, E]
//	                      (name :String, value :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                      (key :M[_] => TypedColumn[K, _])
//	                      (implicit referencedType :TypeTag[E], table :RelVar[M]) :ForeignKeyColumnMapping[M, K, Supposed[E], O] =
//		fkimpl[M, K, E, E, Supposed[E]](name, value, buffs :_*)(table, key, requiredKinFactory[M, K, E, E](table, key).narrow)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], E]
//	                      (name :String, value :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                      (table :RelVar[M])
//	                      (implicit referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
//			:ForeignKeyColumnMapping[M, key.Key, Supposed[E], O] =
//		fkimpl[M, key.Key, E, E, Supposed[E]](name, value, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], E]
//	                      (name :String, value :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                      (implicit referencedType :TypeTag[E], table :RelVar[M], key :PrimaryKeyColumnOf[M])
//			:ForeignKeyColumnMapping[M, key.Key, Supposed[E], O] =
//		fkimpl[M, key.Key, E, E, Supposed[E]](name, value, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)
//
//	/*  Not null single column foreign key with reflected names, as Supposed[E]            */
//
//	protected def supposed[M[A] <: TypedMapping[E, A], K, E]
//	                      (value :S => Supposed[E], buffs :Buff[Supposed[E]]*)(table :RelVar[M], key :M[_] => TypedColumn[K, _])
//	                      (implicit entityType :TypeTag[S], referencedType :TypeTag[E])
//			:ForeignKeyColumnMapping[M, K, Supposed[E], O] =
//		fkimpl[M, K, E, E, Supposed[E]](PropertyPath.nameOf(value), value, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], K, E]
//	                      (value :S => Supposed[E], buffs :Buff[Supposed[E]]*)(key :M[_] => TypedColumn[K, _])
//	                      (implicit entityType :TypeTag[S], referencedType :TypeTag[E], table :RelVar[M])
//			:ForeignKeyColumnMapping[M, K, Supposed[E], O] =
//		fkimpl[M, K, E, E, Supposed[E]](PropertyPath.nameOf(value), value, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], E]
//	                      (value :S => Supposed[E], buffs :Buff[Supposed[E]]*)(table :RelVar[M])
//	                      (implicit entityType :TypeTag[S], referencedType :TypeTag[E], key :PrimaryKeyColumnOf[M])
//			:ForeignKeyColumnMapping[M, key.Key, Supposed[E], O] =
//		fkimpl[M, key.Key, E, E, Supposed[E]](PropertyPath.nameOf(value), value, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], E]
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
//	protected def supposed[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
//	                     (property :S => Supposed[E], buffs :Buff[Supposed[E]]*)
//	                     (table :RelVar[M], key :M[_] => C[_])(rename :String => String)
//	                     (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Supposed[E], O] =
//		fkimpl[M, C, K, E, E, Supposed[E]](property, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)(rename)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
//	                      (property :S => Supposed[E], buffs :Buff[Supposed[E]]*)(key :M[_] => C[_])(rename :String => String)
//	                      (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Supposed[E], O] =
//		fkimpl[M, C, K, E, E, Supposed[E]](property, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)(rename)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], E]
//	                      (property :S => Supposed[E], buffs :Buff[Supposed[E]]*)(table :RelVar[M])(rename :String => String)
//	                      (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
//			:ForeignKeyMapping[M, key.PKMapping, key.Key, Supposed[E], O] =
//		fkimpl[M, key.PKMapping, key.Key, E, E, Supposed[E]](property, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)(rename)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], E]
//	                      (property :S => Supposed[E], buffs :Buff[Supposed[E]]*)(rename :String => String)
//	                      (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
//			:ForeignKeyMapping[M, key.PKMapping, key.Key, Supposed[E], O] =
//		fkimpl[M, key.PKMapping, key.Key, E, E, Supposed[E]](property, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)(rename)
//
//	/*  Not null multi column foreign key with a column prefix, as Supposed[E]            */
//
//	protected def supposed[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
//	                     (property :S => Supposed[E], columnPrefix :String, buffs :Buff[Supposed[E]]*)
//	                     (table :RelVar[M], key :M[_] => C[_])
//	                     (implicit referencedType :TypeTag[E]) :ForeignKeyMapping[M, C, K, Supposed[E], O] =
//		fkimpl[M, C, K, E, E, Supposed[E]](property, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)(this.columnPrefix + columnPrefix + _)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E]
//	                      (property :S => Supposed[E], columnPrefix :String, buffs :Buff[Supposed[E]]*)(key :M[_] => C[_])
//	                      (implicit referenceType :TypeTag[E], table :RelVar[M]) :ForeignKeyMapping[M, C, K, Supposed[E], O] =
//		fkimpl[M, C, K, E, E, Supposed[E]](property, buffs :_*)(
//			table, key, requiredKinFactory[M, K, E, E](table, key).narrow
//		)(this.columnPrefix + columnPrefix + _)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], E]
//	                      (property :S => Supposed[E], columnPrefix :String, buffs :Buff[Supposed[E]]*)(table :RelVar[M])
//	                      (implicit referenceType :TypeTag[E], key :PrimaryKeyOf[M])
//			:ForeignKeyMapping[M, key.PKMapping, key.Key, Supposed[E], O] =
//		fkimpl[M, key.PKMapping, key.Key, E, E, Supposed[E]](property, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)(this.columnPrefix + columnPrefix + _)
//
//	protected def supposed[M[A] <: TypedMapping[E, A], E]
//	                      (property :S => Supposed[E], columnPrefix :String, buffs :Buff[Supposed[E]]*)
//	                      (implicit referenceType :TypeTag[E], table :RelVar[M], key :PrimaryKeyOf[M])
//			:ForeignKeyMapping[M, key.PKMapping, key.Key, Supposed[E], O] =
//		fkimpl[M, key.PKMapping, key.Key, E, E, Supposed[E]](property, buffs :_*)(
//			table, key(_), requiredKinFactory[M, key.Key, E, E](table, key(_)).narrow
//		)(this.columnPrefix + columnPrefix + _)




	/* Target of a foreign key column, as an arbitrary RelatedEntityFactory                                       */

/*
	protected def inverseFKImpl[M[A] <: TypedMapping[E, A], K, E, X, R]
	                           (property :S => R, key :TypedColumn[K, O], reference :RelatedEntityFactory[K, E, X, R],
	                            buffs :Buff[R]*)
	                           (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[MappingAt, K, _, _])
			:JoinedEntityColumn[M, K, R, O]

	protected def inverseFK[M[A] <: TypedMapping[E, A], K, E, X, R]
	                       (property :S => R, key :TypedColumn[K, O], reference :RelatedEntityFactory[K, E, X, R],
	                        buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[MappingAt, K, _, _])
			:JoinedEntityColumn[M, K, R, O] =
		inverseFKImpl[M, K, E, X, R](property, key, reference, buffs :_*)(table, fk)

	protected def inverseFK[T[A] <: TypedMapping[S, A], M[A] <: TypedMapping[E, A], K, E, X, R]
	                       (property :S => R, reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[T, K, _, _])
	                       (implicit referencedType :TypeTag[E],
	                        pk :PrimaryKeyOf[T] { type PKMapping[A] <: TypedColumn[K, A] }, self :this.type <:< T[O])
			:JoinedEntityColumn[M, K, R, O] =
		inverseFKImpl[M, K, E, X, R](property, pk(self(this)), reference, buffs :_*)(table, fk)
*/

	/* Target of a foreign key column, as Kin[X]                                     */

/*
	protected def inverseFK[M[A] <: TypedMapping[E, A], K, E, X]
	                       (property :S => Kin[X], key :TypedColumn[K, O], buffs :Buff[Kin[X]]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[MappingAt, K, _, _])
	                       (implicit referencedType :TypeTag[E], composition :X ComposedOf E)
			:ForeignKeyColumnMapping[M, K, Kin[X], O] =
		inverseFKImpl[M, K, E, X, Kin[X]](property, key, kinFactory[M, K, E, X](table, fk(_).key), buffs :_*)(table, fk)

	protected def inverseFK[T[A] <: TypedMapping[S, A], M[A] <: TypedMapping[E, A], K, E, X]
	                       (property :S => Kin[X], buffs :Buff[Kin[X]]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[T, K, _, _])
	                       (implicit referencedType :TypeTag[E],
	                        pk :PrimaryKeyOf[T] { type PKMapping[A] <: TypedColumn[K, A] }, self :this.type <:< T[O],
	                        composition :X ComposedOf E)
			:ForeignKeyColumnMapping[M, K, Kin[X], O] =
		inverseFKImpl[M, K, E, X, Kin[X]](
			property, pk(self(this)), kinFactory[M, K, E, X](table, fk(_).key), buffs :_*
		)(table, fk)
*/

	/* Target of a multi column foreign key, as an arbitrary RelatedEntityFactory                                     */

	protected def inverseFKImpl[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
	                           (property :S => R, key :C[O], reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
	                           (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
			:JoinedEntityComponent[M, C, K, R, O]

	protected def inverseFK[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
	                       (property :S => R, key :C[O], reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
			:JoinedEntityComponent[M, C, K, R, O] =
		inverseFKImpl[M, C, K, E, X, R](property, key, reference, buffs :_*)(table, fk)

	protected def inverseFK[T[A] <: MappingAt[A], M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
	                       (property :S => R, reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[T, C, K, _, _])
	                       (implicit pk :PrimaryKeyOf[T] { type PKMapping[A] = C[A] }, self :this.type <:< T[O])
			:JoinedEntityComponent[M, C, K, R, O] =
		inverseFKImpl[M, C, K, E, X, R](property, pk(self(this)), reference, buffs :_*)(table, fk)

	/* Target of a multi column foreign key, as Kin[X]                                     */

	protected def inverseFK[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X]
	                       (property :S => Kin[X], key :C[O], buffs :Buff[Kin[X]]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
	                       (implicit referencedType :TypeTag[E], composition :X ComposedOf E)
			:JoinedEntityComponent[M, C, K, Kin[X], O] =
		inverseFKImpl[M, C, K, E, X, Kin[X]](
			property, key, kinFactory[M, K, E, X](table, fk(_).key), buffs :_*
		)(table, fk)

	protected def inverseFK[T[A] <: TypedMapping[S, A], M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X]
	                       (property :S => Kin[X], buffs :Buff[Kin[X]]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[T, C, K, _, _])
	                       (implicit referencedType :TypeTag[E], pk :PrimaryKeyOf[T] { type PKMapping[A] = C[A] },
	                        self :this.type <:< T[O], composition :X ComposedOf E)
			:JoinedEntityComponent[M, C, K, Kin[X], O] =
		inverseFKImpl[M, C, K, E, X, Kin[X]](
			property, pk(self(this)), kinFactory[M, K, E, X](table, fk(_).key), buffs :_*
		)(table, fk)




	/* Many to many as Kin[X] */

	protected def kinimpl[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		                  C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                     (property :S => Kin[X], joinTable :RelVar[J],
	                      source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                      target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                      linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	                      buffs :Buff[Kin[X]]*)
	                     (implicit composite :X ComposedOf E, link :TypeTag[JE])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Kin[X], O]

	protected def kin[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		              C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                 (property :S => Kin[X], joinTable :RelVar[J],
	                  source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                  target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                  linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	                  buffs :Buff[Kin[X]]*)
	                 (implicit composite :X ComposedOf E, link :TypeTag[JE])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Kin[X], O] =
		kinimpl(property, joinTable, source, target, linkKin, targetKin, buffs :_*)

	protected def kin[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		              C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                 (property :S => Kin[X], joinTable :RelVar[J],
	                  source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                  target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                  targetKin: => KinFactory[TK, E, E], buffs :Buff[Kin[X]]*)
	                 (implicit composite :X ComposedOf E, link :TypeTag[JE])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Kin[X], O] =
	{
		val linkKin = requiredKinFactory[J, K, JE, Iterable[JE]](joinTable, _ => source(joinTable[JO]).key)
		kinimpl(property, joinTable, source, target, linkKin, targetKin, buffs :_*)
	}

	protected def kin[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		              C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                 (property :S => Kin[X], joinTable :RelVar[J],
	                  source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                  target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO], buffs :Buff[Kin[X]]*)
	                 (implicit composite :X ComposedOf E, linkTag :TypeTag[JE], targetTag :TypeTag[E])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Kin[X], O] =
	{
		val targetFK = target(joinTable[JO])
		val linkKin = requiredKinFactory[J, K, JE, Iterable[JE]](joinTable, _ => source(joinTable[JO]).key)
		val targetKin = requiredKinFactory[T, TK, E, E](targetFK.table, _ => targetFK.target)
		kinimpl(property, joinTable, source, target, linkKin, targetKin, buffs :_*)
	}



	/* Many to many as Derived[E, X] */

	protected def manyimpl[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		                   C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                      (property :S => Derived[E, X], joinTable :RelVar[J],
	                       source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                       target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                       linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	                       buffs :Buff[Derived[E, X]]*)
	                      (implicit composite :X ComposedOf E, link :TypeTag[JE])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Derived[E, X], O]

	protected def many[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		               C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                  (property :S => Derived[E, X], joinTable :RelVar[J],
	                   source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                   target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                   linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	                   buffs :Buff[Derived[E, X]]*)
	                  (implicit composite :X ComposedOf E, link :TypeTag[JE])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Derived[E, X], O] =
		manyimpl(property, joinTable, source, target, linkKin, targetKin, buffs :_*)

	protected def many[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		               C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                  (property :S => Derived[E, X], joinTable :RelVar[J],
	                   source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                   target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                   targetKin: => KinFactory[TK, E, E], buffs :Buff[Derived[E, X]]*)
	                  (implicit composite :X ComposedOf E, link :TypeTag[JE])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Derived[E, X], O] =
	{
		val linkKin = requiredKinFactory[J, K, JE, Iterable[JE]](joinTable, _ => source(joinTable[JO]).key)
		manyimpl(property, joinTable, source, target, linkKin, targetKin, buffs :_*)
	}

	protected def many[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		               C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                  (property :S => Derived[E, X], joinTable :RelVar[J],
	                   source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                   target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO], buffs :Buff[Derived[E, X]]*)
	                  (implicit composite :X ComposedOf E, linkTag :TypeTag[JE], targetTag :TypeTag[E])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Derived[E, X], O] =
	{
		val targetFK = target(joinTable[JO])
		val linkKin = requiredKinFactory[J, K, JE, Iterable[JE]](joinTable, _ => source(joinTable[JO]).key)
		val targetKin = requiredKinFactory[T, TK, E, E](targetFK.table, _ => targetFK.target)
		manyimpl(property, joinTable, source, target, linkKin, targetKin, buffs :_*)
	}


}


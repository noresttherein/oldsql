package net.noresttherein.oldsql.schema

import java.sql

import net.noresttherein.oldsql.schema.Buff.BuffType
import net.noresttherein.oldsql.schema.Mapping.{ColumnFilter, MappingOf}
import net.noresttherein.oldsql.schema.bits.Temporal
import net.noresttherein.oldsql.slang._

import scala.reflect.ClassTag



/** An optional annotation/modifier for component mappings, especially columns.
  * Modifies the way the annotated component is mapped, in particular it is used to include or exclude
  * certain columns from select/insert/update statements. Each `Buff` is associated with a single `BuffType`, which
  * serves as a factory, matcher and a virtual 'class'. These factories can be used to test if a component is
  * annotated with a buff of a particular type and retrieve its information, if present.
  * See [[net.noresttherein.oldsql.schema.Buff.BuffType BuffType]] for more information.
  * @see [[net.noresttherein.oldsql.schema.Buff.FlagBuff FlagBuff]]
  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]
  * @see [[net.noresttherein.oldsql.schema.Buff.AuditBuff AuditBuff]]
  * @tparam T the value type of the annotated component.
  */
trait Buff[T] {
	//todo: cascade flag
	//todo: contradictory buffs
	//todo: dedicated buffs collection handling duplicates and implications

	/** The type of this buff, i.e. the factory that created this instance. */
	def buffType :BuffType

	/** Does this buff belong to the buff group defined by `group`, carrying information defined by the group?
	  * This will be true if this buff was created by this group or if the group which created this buff
	  * has strictly narrower meaning than the passed argument, effectively implying the latter.
	  * This method delegates the test to the associated `BuffType`'s
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implies]].
	  */
	def is(group :BuffType) :Boolean = buffType.implies(group)


	/** Adapts this buff for a new component type. For flag buffs, this is an identity operation; for buffs
	  * with values it creates a new buff of the same type carrying the mapped value. Certain buffs, in particular
	  * audit buffs, do not support this operation and will throw an `UnsupportedOperationException`.
	  */
	def map[X](f :T => X) :Buff[X]

	/** Adapts this buff for a component type derived from this buff's type parameter. This method is used for
	  * conveying attached buffs from a mapping to its components and is responsible for implementing the rules
	  * of which buff types are inherited by 'export' versions of components from their parent mappings.
	  * As such, it can return an empty option even in cases where `map` would return successfully and can
	  * even return a buff of a different type than this instance. By default, flag buffs return themselves and buffs
	  * with constant values create a new buff of the same type carrying the mapped value. Certain buffs, in particular
	  * audit buffs, do not support this operation and will return `None`. This method should not throw exceptions.
	  */
	def cascade[X](f :T => X) :Option[Buff[X]] = Some(map(f))

	/** Adapts this buff for a new component type. For flag buffs, this is an identity operation; for buffs
	  * with values it creates a new buff of the same type carrying the mapped value. This method
	  * differs from `map` in that it also works for `AuditBuff` instances.
	  */
	def bimap[X](there :T => X, back :X => T) :Buff[X] = map(there)



	def canEqual(that :Any) :Boolean = that.getClass == this.getClass

	override def equals(that :Any) :Boolean = that match {
		case o :Buff[_] => (this eq o) || o.canEqual(this) && buffType == o.buffType
		case _ => false
	}

	override def hashCode :Int = getClass.hashCode

	override def toString :String = buffType.toString
}






object Buff {

	/** This column/component is not included in the default select statement and must be specified explicitly,
	  * if at all possible. */
	case object NoSelectByDefault extends AbstractBuffType

	/** This column/component is not included in the default insert statement and must be specified explicitly,
	  * if at all possible. */
	case object NoInsertByDefault extends AbstractBuffType

	/** This column/component is not included in the default update statement and must be specified explicitly,
	  * if at all possible. */
	case object NoUpdateByDefault extends AbstractBuffType

	/** This column/component is not included as the parameter of the default query statement searching for
	  * an equal entity and must be specified explicitly, if at all possible. */
	case object NoQueryByDefault extends AbstractBuffType



	/** This column/component can't be included in a select statement (as part of its header). */
	case object NoSelect extends ComboFlag(NoSelectByDefault)
	/** This column/component can't be included in an insert statement (as the inserted column). */
	case object NoInsert extends ComboFlag(NoInsertByDefault)
	/** This column/component can't be included in an update statement (as the updated column). */
	case object NoUpdate extends ComboFlag(NoUpdateByDefault)
	/** This column/component can't be included as part of the ''where'' clause of a select or update statement. */
	case object NoQuery extends ComboFlag(NoQueryByDefault)

	/** This column/component is never written to the database by the application. */
	case object ReadOnly extends ComboFlag(NoInsert, NoUpdate)

	/** A buff type which marks columns ignored by the enclosing mapping. It is useful when a column is still
	  * used as part of SQL statements. Implies `ReadOnly` and `NoSelect`. */
	case object Ignored extends ComboFlag(ReadOnly, NoSelect)



	/** The value for this column/component is generated by the database on insert and should be returned by the
	  * insert statement. */
	case object AutoInsert extends ComboFlag(NoInsert)

	/** The value for this column/component is updated by the database whenever a mapped row is updated. */
	case object AutoUpdate extends ComboFlag(NoUpdate)

	/** The value for this column/component is generated by the database on insert and updated on each update,
	  * making it read only for the application. */
	case object AutoGen extends ComboFlag(AutoInsert, AutoUpdate, ReadOnly)



	/** A factory for buffs marking that a given column/component can be omitted from the select clause.
	  * It is still included by default and needs to be excluded explicitly. Created values carry a placeholder
	  * value to assign to the annotated component on assembly. */
	case object OptionalSelect extends ComboValueBuffType

	/** A factory for buffs marking that a given column/component is omitted by default from the select clause
	  * and needs to be included explicitly. When not included, the value stored in the buff will be used
	  * as the value for the annotated component. It implies `OptionalSelect` and `NoSelectByDefault`. */
	case object ExplicitSelect extends ComboValueBuffType(OptionalSelect, NoSelectByDefault)

	/** A buff marking a column as non-selectable, and providing the value for the annotated component.
	  * This can be used in particular for 'virtual' columns - components which take part in the mapping, but
	  * aren't present in the database at all.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Virtual$]] */
	case object ExtraSelect extends ComboValueBuffType(NoSelect)

	/** A buff marking a component or column which does not exist in the database and will not be used as part
	  * of any SQL statements under any circumstances. It is still part of the mapping and, during assembly,
	  * the provided expression is used as its value. This can be useful during schema migrations, when a mapping
	  * might need to cover several versions of the schema, or if it is reused for several similar tables. */
	case object Virtual extends ComboValueBuffType(ExtraSelect, ReadOnly, NoQuery)



	/** A buff marking that a given column or component can be omitted from the the parameter list of the ''where'' 
	  * clause of an SQL statement. This covers the case when a comparison in an SQL expression happens between
	  * whole subjects of a multi column mapping, rather than listing the columns individually. The annotated component
	  * is still included by default when comparing the owning mapping's subjects and needs to be excluded explicitly.
	  */
	case object OptionalQuery extends FlagBuffType

	/** A buff marking that a given column or component can be omitted from the parameter list of the ''where'' clause
	  * of an SQL statement and needs to be included explicitly. This applies when the comparison expression
	  * happens on the level of the subject of a multi column mapping enclosing the buffed component without listing
	  * its columns individually. It implies `OptionalQuery` and `NoQueryByDefault`. */
	case object ExplicitQuery extends ComboFlag(OptionalQuery, NoQueryByDefault)

	/** A buff type marking that a given column/component must be included in every query against the table, using
	  * the value provided by the buff. It implies `NoSelect` and `NoQuery` and is used to artificially limit the number
	  * of mapped entities.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Unmapped$]] */
	case object ExtraQuery extends ComboValueBuffType(NoSelect, NoQuery)



	/** A buff marking that a given column/component can be omitted from the insert statement.
	  * It is still included by default and needs to be excluded explicitly. */
	case object OptionalInsert extends FlagBuffType

	/** A buff marking that a given column/component is not inserted by default into the underlying table
	  * and needs to be included explicitly. It implies `OptionalInsert` and `NoInsertByDefault`. */
	case object ExplicitInsert extends ComboFlag(OptionalInsert, NoInsertByDefault)

	/** Marks a column/component as having its value initialized by the expression provided by the buff
	  * rather than the entity. Used particularly for 'created on' or 'created by' type of columns. */
	case object ExtraInsert extends ComboValueBuffType(NoInsert)


	/** A buff marking that a given column/component can be omitted from the update statement.
	  * It is still included by default and needs to be excluded explicitly. */
	case object OptionalUpdate extends FlagBuffType

	/** A buff marking that a given column/component is not included by default in the update statements
	  * and needs to be included explicitly. It implies `OptionalUpdate` and `NoUpdateByDefault`. */
	case object ExplicitUpdate extends ComboFlag(OptionalUpdate, NoUpdateByDefault)

	/** Marks a column/component as being updated with the value of the expression provided by the buff
	  * rather than some property of the mapped entity. Useful for particularly for 'update timestamp' columns. */
	case object ExtraUpdate extends ComboValueBuffType(NoUpdate)



	/** Marks a column/component as not mandatory for insert and update statements. */
	case object OptionalWrite extends ComboFlag(OptionalInsert, OptionalUpdate)

	/** Marks a column/component as not included in the insert/update statements by default and needing to be included explicitly. */
	case object ExplicitWrite extends ComboFlag(ExplicitInsert, ExplicitUpdate, OptionalWrite)

	/** Marks a column/component as having its value set by this buff rather than a property of the entity
	  * at every write to the database. Implies `ReadOnly`, `ExtraInsert` and `ExtraUpdate`. */
	case object ExtraWrite extends ComboValueBuffType(ReadOnly, ExtraInsert, ExtraUpdate)


	/** Signifies that a column/component can be excluded from all types of database operations.
	  * It is a shortcut for marking it with `OptionalSelect`, `OptionalQuery`, `OptionalWrite`. */
	case object Optional extends ComboValueBuffType(OptionalSelect, OptionalQuery, OptionalWrite)

	/** Signifies that a column/component must be listed explicitly in order to be included in any database operation
	  * (it is not included by default).
	  * It is a shortcut for marking it with `Optional`, `ExplicitSelect`, `ExplicitQuery` and `ExplicitWrite`. */
	case object Explicit extends ComboValueBuffType(Optional, ExplicitSelect, ExplicitQuery, ExplicitWrite)


	/** Marks a column or component which is not part of the mapped scala class, but is still part of the mapped
	  * entity from the relational point of view. All rows which are subject to mapping by the application have
	  * the value returned by the buff, essentially partitioning the table and limiting the application to a subset
	  * of its rows. It implies both `ExtraQuery` and `ExtraWrite`, meaning that all queries against the table will
	  * include the annotated column in the filter and all inserts and updates will set its value based on this buff.
	  */
	case object Unmapped extends ComboValueBuffType(ExtraQuery, ExtraWrite)



	/** Any value returned from the select (or assembled from such values) of a column/component annotated
	  * with this buff type must be mapped with the function included in the buff. This buff is independent
	  * from buffs specifying whether and when a component can be included in a select header. */
	case object SelectAudit extends ComboBuffType(Audit) with AuditBuffType

	case object QueryAudit extends ComboBuffType(Audit) with AuditBuffType

	/** All values of columns/components annotated with this buff type must be mapped with the function
	  * included in the buff before inserting the entity declaring it. This does not include update statements
	  * and is independent of any buffs specifying if the column/component can be inserted at all. */
	case object InsertAudit extends ComboBuffType(WriteAudit) with AuditBuffType

	/** All values of columns/components annotated with this buff type must be mapped with the function
	  * included in the buff before updating the entity declaring it. This does not include insert statements
	  * and is independent of any buffs specifying if the column/component can be updated at all. */
	case object UpdateAudit extends AuditBuffType

	/** All values of columns/components annotated with this buff type must be passed through the function
	  * provided by this buff before inserting or updating the entity declaring it. It is thus ideally suited
	  * for implementing validation, both on single columns and constraints spanning several columns (when applied
	  * to a component containing those columns). This is independent of any buffs specifying if the column/component
	  * can be inserted at all.
	  */
	case object WriteAudit extends ComboBuffType(UpdateAudit, InsertAudit) with AuditBuffType

	/** Any read or written value `S` of a column/component annotated with this buff is first passed
	  * through the function `S => S` provided by the buff. This buff type thus makes a good extension point
	  * for consistency validation, both of data already in the database and that being written. If you wish to limit
	  * the check only to insert and update operations, use
	  * the [[net.noresttherein.oldsql.schema.Buff.WriteAudit$ WriteAudit]] buff instead (or those specifically dedicated
	  * to a single database operation type0.
	  */
	case object Audit extends ComboBuffType(SelectAudit, QueryAudit, WriteAudit) with AuditBuffType



	/** A flag signifying that mapped values can be null. */
	case object Nullable extends FlagBuffType






	/** Marks that a column/component ''must'' be included as part of the ''where'' clause of any update statement. */
	case object UpdateMatch extends FlagBuffType

	/** A buff type marking that a column contains an application generated timestamp set once, when the row is inserted.
	  * It is an `ExtraInsert` buff, meaning it will be automatically included in every insert and the value present
	  * in the inserted entity will be ignored.
	  * It should be of a type with a provided [[net.noresttherein.oldsql.schema.bits.Temporal Temporal]] type class.
	  */
	case object CreateTimestamp extends ComboBuffType(ExtraInsert) with GeneratedBuffType {
		def apply[T :Temporal]() :GeneratedBuff[T] = apply(implicitly[Temporal[T]].now())
	}

	/** A buff type marking that a column contains an application generated timestamp set when the row is inserted
	  * and every time it is updated. It implies `ReadOnly`, `CreateTimestamp`, `ExtraInsert` and `ExtraUpdate`, meaning
	  * it will be automatically included in every write of the mapped entity to the database and the value present
	  * in the entity will be ignored.
	  * It should be of a type with a provided [[net.noresttherein.oldsql.schema.bits.Temporal Temporal]] type class.
	  */
	case object UpdateTimestamp extends ComboBuffType(ReadOnly, ExtraUpdate, CreateTimestamp) with GeneratedBuffType {
		def apply[T :Temporal]() :GeneratedBuff[T] = apply(implicitly[Temporal[T]].now())
	}

	/** A buff type marking that a column serves as a timestamp-based optimistic lock.
	  * It is the same as [[net.noresttherein.oldsql.schema.Buff.UpdateTimestamp$ UpdateTimestamp]], but also
	  * implies [[net.noresttherein.oldsql.schema.Buff.UpdateMatch$ UpdateMatch]]. This means the values
	  * carried by entities will be ignored during the update and instead a fresh timestamp will be used as the new
	  * value ''and'' the old value must be included in the ''where'' clause of the update statement to prevent
	  * overwriting a concurrent update.
	  * Note that the value of the column can, strictly speaking, be of any type not related to time, as long as
	  * an implicit instance of the [[net.noresttherein.oldsql.schema.bits.Temporal Temporal]] type class is provided
	  * for it.
	  */
	case object UpdateLock extends ComboBuffType(UpdateTimestamp, UpdateMatch) with GeneratedBuffType {
		def apply[T :Temporal]() :GeneratedBuff[T] = apply(implicitly[Temporal[T]].now())
	}

	/** Marks a column/component as carrying a 'version' stamp used to implement optimistic locking. The value
	  * carried by the entity is automatically increased/modified during the update, but the old value is used
	  * as part of the 'where' clause to prevent overwriting a concurrent update.
	  */
	case object VersionLock extends ComboBuffType(ExtraWrite, UpdateAudit, UpdateMatch) with ManagedBuffType {
		def apply[T]()(implicit int :Integral[T]) :ManagedBuff[T] =
			apply(int.fromInt(0), int.plus(_, int.fromInt(1)))

		def apply[T](init :T)(implicit int :Integral[T]) :ManagedBuff[T] =
			apply(init, int.plus(_, int.fromInt(1)))
	}






	/** A 'class for classes' being subtypes of `Buff`. It implicitly defines the information carried by the buffs of
	  * this type and indicates their purpose or application conditions. It can be used both as a factory and matcher,
	  * testing buff instances, collections, and mappings for the presence of an associated buff. This is an abstract
	  * base class and as such declares no `apply/unapply` methods, leaving it to subclasses which define them
	  * in terms of the information carried by their buffs. Whenever several matching buffs are present on a list
	  * or in a mapping, the buff type will always pick the first one. Similarly, if a subtype extracts ''all''
	  * matching buffs from a list or a component, it will do so preserving their order.
	  *
	  * Note that buffs can be attached also to non-column components. The exact behavior in that case is completely
	  * dependent on the mapping class; by default the subcomponents of the mapping inherit the buffs defined
	  * in the enclosing mappings, with inherited buffs following the buffs declared locally.
	  *
	  * Instances of this class form a subtype relation of sorts, meaning certain buffs created by some factories
	  * are recognized as 'belonging' also to other buff types. This is implemented through the
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implies]] method. If a `BuffType` `A` implies
	  * a `BuffType` `B`, then `B.enabled(a)` and `a is B` will be true for every buff `a` created by the buff type `A`.
	  * If the actual `Buff` class, defining what information is carried by a buff, used by the type `A`
	  * is a subclass of the class of buffs produces by the type `B` (which should be the case for all such pairs),
	  * then also any extractor methods defined by the type `B` will pick up the data from buffs `A`.
	  * This means that, for example, any `BuffType` can safely imply any instance of `FlagBuffType` and any additional
	  * information carried by the buff being tested is ignored for the purpose of the check.
	  * On the other hand, creating a `FlagBuffType` which implies `ValueBuffType` would be an application error:
	  * while the valued type would recognize a buff created by the flag buff type in question in its
	  * `enabled`/`disabled` methods, the `test` and `unapply` methods would yield `None`, as the buff does not conform
	  * to the expected type. For the convenience of implementation, there are `ComboBuffType` and `ComboFlag`
	  * classes which accept a list of implied buff types as constructor arguments.
	  *
	  * Certain buff types are ''abstract'', meaning they can't be used to create new instances, but serve only
	  * as a grouping of more specific buff types which imply it. This is analogous to a common abstract base class.
	  * In particular, certain buff types serve only to indicate ''when'' the buff should be applied, without any
	  * information about its purpose. For example, creating
	  * a [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]],
	  * which implies the [[net.noresttherein.oldsql.schema.Buff.UpdateAudit$ UpdateAudit]], will map all updated values
	  * with the function provided by the buff before they are written to the database.
	  *
	  * See [[net.noresttherein.oldsql.schema.Buff$ Buff]] for the full list of predefined buff types.
	  *
	  * @see [[net.noresttherein.oldsql.schema.Buff.FlagBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.AuditBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ManagedBuffType]]
	  */
	trait BuffType { factory =>

		/** Checks if this buff type ''implies'' another buff type. If it does, tests for presence of `other`
		  * on a mapping will give positive results also if this buff is present. For this to work however,
		  * the implied buff type must use the same `Buff` class as this instance, or a more generic one.
		  * For example, a `FlagBuffType` implying a `ValueBuffType` will not be taken into account, as the latter
		  * specifically checks only for `ValueBuff` subclasses. Implication in the other direction will work
		  * however, as a `FlagBuffType` is satisfied with a `Buff` of any class.
		  */
		def implies(other :BuffType) :Boolean = other == this

		object Enabled extends ColumnFilter.WithBuff(factory) {
			@inline def unapply(buff :Buff[_]) :Boolean = enabled(buff)
			@inline def unapply(buffs :Seq[Buff[_]]) :Boolean = enabled(buffs)
			@inline def unapply(mapping :Mapping) :Boolean = enabled(mapping)
		}
		object Disabled extends ColumnFilter.WithoutBuff(factory) {
			@inline def unapply(buff :Buff[_]) :Boolean = disabled(buff)
			@inline def unapply(buffs :Seq[Buff[_]]) :Boolean = disabled(buffs)
			@inline def unapply(mapping :Mapping) :Boolean = disabled(mapping)
		}

		def enabled(buff :Buff[_]) :Boolean = buff is this
		def enabled(buffs :Seq[Buff[_]]) :Boolean = buffs.exists(enabled)
		def enabled(column :Mapping) :Boolean = enabled(column.buffs)

		def disabled(buff :Buff[_]) :Boolean = !(buff is this)
		def disabled(buffs :Seq[Buff[_]]) :Boolean = buffs.forall(disabled)
		def disabled(column :Mapping) :Boolean = disabled(column.buffs)

		def test[T](buff :Buff[T]) :Option[Buff[T]] = buff is this ifTrue buff
		def test[T](buffs :Seq[Buff[T]]) :Option[Buff[T]] =
			buffs.collectFirst { case buff if buff is this => buff }
		def test[T](column :MappingOf[T]) :Option[Buff[T]] = test(column.buffs)

		def ||(other :BuffType) :BuffType = other match {
			case combo :ComboBuffType => new DisjunctionBuffType(this +: combo.implied)
			case _ => new DisjunctionBuffType(Seq(this, other))
		}

		override val toString :String = this.innerClassName

	}




	/** A scaffolding base trait for buff types which use a `Buff` subclass rather then the `Buff` class itself. */
	sealed trait DedicatedBuffType[+B[T] <: Buff[T]] extends BuffType {
		private[this] val Class :ClassTag[B[Any]] = classTag.asInstanceOf[ClassTag[B[Any]]]
		protected[this] def classTag :ClassTag[_]

		override def test[T](buff :Buff[T]) :Option[B[T]] = buff match {
			case Class(b) if b is this => Some(b.asInstanceOf[B[T]])
			case _ => None
		}

		override def test[T](buffs :Seq[Buff[T]]) :Option[B[T]] =
			buffs collectFirst { case Class(b) if b is this => b.asInstanceOf[B[T]] }

		override def test[T](column :MappingOf[T]) :Option[B[T]] = test(column.buffs)

		def unapply[T](buff :Buff[T]) :Option[B[T]] = test(buff)

		def unapply[T](buffs :Seq[Buff[T]]) :Option[B[T]] = test(buffs)

		def unapply[T](column :MappingOf[T]) :Option[B[T]] = test(column)
	}






	/** A `Buff` type which doesn't have any `Buff` instances, but is instead implied by other buff types. */
	class AbstractBuffType extends FlagBuffType

	/** A `Buff` type which doesn't have any `Buff` instances and won't match any of them.
	  * It is used in place of a `BuffType` when no appropriate concrete implementation exists to render relevant code
	  * inactive, automatically fulfilling a function normally performed by a `Option[BuffType]`.
	  */
	case object AbstractBuff extends AbstractBuffType {
		override def test[T](buff :Buff[T]) :Option[Buff[T]] = None
		override def test[T](buffs :Seq[Buff[T]]) :Option[Buff[T]] = None
	}

	/** A special `Buff` factory producing buffs which are ignored and not recognized by any other buff types. */
	case object NeutralBuff extends FlagBuffType



	/** A `Buff` type which implies other, more general buffs (has strictly more specific implications).
	  * Attaching a `Buff` of this type to a component is roughly equivalent to attaching buffs of the types
	  * listed in the constructor.
	  * Note that this can seem a reversal of the order of class extension: the implied buffs are ''more''
	  * specific than this buff. The difference from inheritance is that every buff instance of this type implies
	  * all the listed buffs, not just one of them. A more proper way of viewing it is multi generalization/inheritance.
	  */
	class ComboBuffType(val implied :BuffType*) extends BuffType {
		override def implies(other :BuffType) :Boolean =
			other == this || implied.exists(_.implies(other))

		override def ||(other :BuffType) :BuffType = other match {
			case combo :ComboBuffType => new DisjunctionBuffType(implied ++: combo.implied)
			case _ => new DisjunctionBuffType(implied :+ other)
		}
	}

	private class DisjunctionBuffType(buffs :Seq[BuffType]) extends ComboBuffType(buffs :_*) {
		override val toString = buffs.mkString(" || ")
	}





	private class FlagBuff[T](val buffType :FlagBuffType) extends Buff[T] {
		override def map[X](there :T => X) :FlagBuff[X] = this.asInstanceOf[FlagBuff[X]]
		override def cascade[X](there :T => X) :Some[Buff[X]] = Some(this.asInstanceOf[FlagBuff[X]])
		override def bimap[X](there :T => X, back :X => T) :FlagBuff[X] = this.asInstanceOf[FlagBuff[X]]
	}



	/** A simple mapping buff which doesn't perform any function in the mapping itself, but serves
	  * instead as a switch checked at certain points to modify the behaviour of the annotated component,
	  * such as including an extra column in the update.
	  * There is an implicit conversion from `FlagBuffType` to `Buff[T]` for any type `T`, so in most cases
	  * it is possible to omit the type parameter of the factory `apply` method.
	  */
	trait FlagBuffType extends BuffType {
		private[this] val buff = new FlagBuff[Nothing](this)

		/** Creates a new instance of this buff type for a column/component with subject type `T`.
		  * All instances created by this `BuffType` are equal.
		  */
		def apply[T] :Buff[T] = buff.asInstanceOf[Buff[T]]

//		/** Creates a new instance of this buff type for a column/component with subject type `T`.
//		  * This is the same as `apply[T]`, but the type parameter in most cases will be inferred and can be omitted.
//		  */
//		@inline def ^[T] :Buff[T] = apply[T]

		@inline final def unapply[T](buff :Buff[T]) :Boolean = enabled(buff)
		@inline final def unapply[T](buffs :Seq[Buff[T]]) :Boolean = enabled(buffs)
		@inline final def unapply(component :Mapping) :Boolean = enabled(component)

	}



	object FlagBuffType {
		/** Creates a new, unique type of component flag using the given name as the identifier. */
		def apply(name :String) :FlagBuffType = new NamedFlag(name)

		implicit def flag[T](buff :FlagBuffType) :Buff[T] = buff[T]

		private class NamedFlag(override val toString :String) extends FlagBuffType {

			override def equals(that :Any) :Boolean = that match {
				case flag :NamedFlag => (flag eq this) || flag.toString == toString
				case _ => false
			}

			override def hashCode :Int = toString.hashCode
		}

	}



	/** A `FlagBuffType` which implies other flags (is equivalent to having them declared alongside it).
	  * Note that this can seem a reversal of the order of class extension: the implied buffs are ''more''
	  * specific than this buff. The difference from inheritance is that every buff instance of this type implies
	  * all the listed buffs, not just one of them. A more proper way of viewing it is multi generalization/inheritance.
	  */
	class ComboFlag(implied :FlagBuffType*) extends ComboBuffType(implied:_*) with FlagBuffType {
		override def implies(other :BuffType) :Boolean = super[ComboBuffType].implies(other)
	}






	/** A `Buff` which carries a value. These values are generally used instead of the the value carried
	  * by an entity or read from the database, but the exact handling depends on the buff type.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  */
	trait ValueBuff[T] extends Buff[T] {
		override def map[X](there :T => X) :ValueBuff[X]

		override def bimap[X](there :T => X, back :X => T) :ValueBuff[X] = map(there)

		def value :T

		def buffType :ValueBuffType
	}



	object ValueBuff {
		def unapply[T](buff :Buff[T]) :Option[T] = buff match {
			case const :ValueBuff[T] => Some(const.value)
			case _ => None
		}

		def unapply[T](buffs :Seq[Buff[T]]) :Option[T] =
			buffs collectFirst { case const :ValueBuff[T] => const.value }

		@inline def unapply[T](column :MappingOf[T]) :Option[T] = unapply(column.buffs)
	}



	/** A `Buff` type which carries a value. These buff types are handled explicitly when creating and executing
	  * individual SQL statements. Which statements are affected (and how the value is used) depends on which
	  * of the `ValueBuffType` instances are implied by the implementing class:
	  *   - implying `ExtraSelect` means the annotated component is never included in the select header and the
	  *     value provided by the buff is used instead;
	  *   - implying `ExtraQuery` means that every select and update statement must include the annotated component
	  *     in the 'where' clause to additionally filter the set of rows mapped by the application;
	  *   - implying `ExtraInsert` means that buff's value is used instead of any value carried by the entity
	  *     when inserting a new row into the database;
	  *   - implying `ExtraUpdate` means that buff's value is used instead of any value carried by the entity
	  *     when updating a row in the database.
	  *  As always, extending classes can imply several of the above at the same time.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraSelect$ ExtraSelect]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraQuery$ ExtraQuery]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraInsert$ ExtraInsert]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraUpdate$ ExtraUpdate]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraWrite$ ExtraWrite]]
	  */
	trait ValueBuffType extends DedicatedBuffType[ValueBuff] {
		protected[this] def classTag :ClassTag[_] = implicitly[ClassTag[ValueBuff[Any]]]

		object Value {
			@inline def unapply[T](buff :Buff[T]) :Option[T] = test(buff).map(_.value)
			@inline def unapply[T](buffs :Seq[Buff[T]]) :Option[T] = test(buffs).map(_.value)
			@inline def unapply[T](mapping :MappingOf[T]) :Option[T] = test(mapping).map(_.value)

			@inline def apply[T](buff :Buff[T]) :Option[T] = unapply(buff)
			@inline def apply[T](buffs :Seq[Buff[T]]) :Option[T] = unapply(buffs)
			@inline def apply[T](mapping :MappingOf[T]) :Option[T] = unapply(mapping)
		}

	}



	/** A `Buff` type without any instances, not applied to any components. It is used in place of a `ValueBuffType`
	  * when no appropriate concrete implementation exists to render relevant code inactive, automatically fulfilling
	  * a function normally performed by a `Option[ValueBuffType]`.
	  */
	case object AbstractValueBuff extends ValueBuffType {
		override def test[T](buff :Buff[T]) :Option[ValueBuff[T]] = None
		override def test[T](buffs :Seq[Buff[T]]) :Option[ValueBuff[T]] = None
	}



	/** A mapping buff which carries a constant value to be used instead of the value present in the entity or
	  * the database, depending on the exact buff type.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType]]
	  */
	class ConstantBuff[T](val buffType :ConstantBuffType, val value :T) extends ValueBuff[T] {

		override def map[X](there: T => X): ConstantBuff[X] = buffType(there(value))

		override def cascade[X](there :T => X) :Option[ConstantBuff[X]] = Some(buffType(there(value)))

		override def bimap[X](there :T => X, back :X => T) :ValueBuff[X] = buffType(there(value))

		override def equals(that: Any): Boolean = that match {
			case self :AnyRef if self eq this => true
			case o :ConstantBuff[_] if o.canEqual(this) => o.value == value && o.buffType == buffType
			case _ => false
		}


		override def toString = s"$buffType: $value"
	}

	object ConstantBuff {
		def unapply[T](buff :Buff[T]) :Option[T] = buff match {
			case const :ConstantBuff[T] => Some(const.value)
			case _ => None
		}

		def unapply[T](buffs :Seq[Buff[T]]) :Option[T] =
			buffs collectFirst { case const :ConstantBuff[T] => const.value }

		@inline def unapply[T](column :MappingOf[T]) :Option[T] = unapply(column.buffs)
	}



	/** A base trait for factories of `Buff[T]` instances wrapping values of `T`.
	  * By implying one of the predefined `ExtraSelect`, `ExtraQuery`, `ExtraInsert`, `ExtraUpdate` buff types,
	  * extending classes specify when (with which statement types) these values should be used.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  */
	trait ConstantBuffType extends ValueBuffType {
		def apply[T](value :T) :ConstantBuff[T] = new ConstantBuff[T](this, value)
	}



	/** A buff which reevaluates encapsulated expression each time its `value` method is called.
	  * This is similar to `ConstantBuff`, but the value is reevaluated with each call to `this.value`.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType]]
	  */
	class GeneratedBuff[T](val buffType :GeneratedBuffType, generator: =>T) extends ValueBuff[T] {
		def value :T = generator

		override def map[X](there :T => X) :GeneratedBuff[X] = buffType(there(generator))

		/** Returns `None` */
		override def cascade[X](there :T => X) :Option[Buff[X]] = None //Some(buffType(there(generator)))

		override def bimap[X](there :T => X, back :X => T) :GeneratedBuff[X] = buffType(there(generator))

		override def equals(that :Any) :Boolean = that match {
			case ref :AnyRef if ref eq this => true
			case _ => false
		}

		override def hashCode :Int = System.identityHashCode(this)

		override def toString :String = "=>" + buffType
	}


	object GeneratedBuff {
		def unapply[T](buff :Buff[T]) :Option[T] = buff match {
			case const :GeneratedBuff[T] => Some(const.value)
			case _ => None
		}

		def unapply[T](buffs :Seq[Buff[T]]) :Option[T] =
			buffs collectFirst { case const :GeneratedBuff[T] => const.value }

		@inline def unapply[T](column :MappingOf[T]) :Option[T] = unapply(column.buffs)
	}



	/** A column/component `Buff` type which carries a by-name value. This value is used instead of the value
	  * present in the entity or the database in SQL statements. Which statements are affected depends on which
	  * of the predefined `ExtraSelect`, `ExtraQuery`, `ExtraInsert`, `ExtraUpdate` buffs is implied by the
	  * extending class. It is similar to `ConstantBuffType`, but the value provided by the buff is re-evaluated
	  * at each access.
	  * @see [[net.noresttherein.oldsql.schema.Buff.GeneratedBuff]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType]]
	  */
	trait GeneratedBuffType extends ValueBuffType {
		/** Create a `Buff` which will reevaluate the given expression each time its `value` method is accessed. */
		def apply[T](value: =>T) :GeneratedBuff[T] = new GeneratedBuff(this, value)
	}



	/** A general purpose base class for buff types carrying values, combining the `ComboBuffType`
	  * with `GeneratedBuffType`, additionally providing a `ConstantBuffType` implying this instance as the
	  * `const` property.
	  */
	class ComboValueBuffType(override val toString :String, implies :BuffType*)
		extends ComboBuffType(implies :_*) with GeneratedBuffType
	{ outer =>
		def this(implies :BuffType*) = this(this.innerClassName, implies :_*)

		val const :ConstantBuffType = new ComboBuffType(this) with ConstantBuffType {
			override val toString :String = outer.toString + ".const"
		}
	}


	/** A column/component `Buff` carrying a function `T=>T` which is used to modify the value read or written
	  * to the database. Which operations are actually affected depends on the buff type.
	  */
	class AuditBuff[T](val buffType :AuditBuffType, val substitute :T => T) extends Buff[T] {

		override def map[X](there :T => X) :AuditBuff[X] =
			throw new UnsupportedOperationException(toString + ".map: AuditBuff can't be mapped unidirectionally.")

		override def cascade[X](f :T => X) :Option[Nothing] = None

		override def bimap[X](there :T => X, back :X => T) :AuditBuff[X] =
			new AuditBuff[X](buffType, back andThen substitute andThen there)


		override def equals(that :Any) :Boolean = that match {
			case sub :AuditBuff[_] => (sub eq this) || sub.buffType == buffType && sub.substitute == substitute
			case _ => false
		}

		override def hashCode :Int = buffType.hashCode * 31 + substitute.hashCode

		override def toString :String = buffType.toString + "(" + substitute + ")"
	}



	/** A buff type which inspects and possibly modifies the value of the annotated column/component during
	  * a database operation. Exactly which operation(s) is/are affected is determined declaratively by
	  * implying one of the 'audit' types: `SelectAudit`, `QueryAudit`, `InsertAudit`, `UpdateAudit`.
	  * @see [[net.noresttherein.oldsql.schema.Buff.SelectAudit$ SelectAudit]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.QueryAudit$ QueryAudit]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.InsertAudit$ InsertAudit]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.UpdateAudit$ UpdateAudit]]
	  */
	trait AuditBuffType extends DedicatedBuffType[AuditBuff] { self =>
		protected[this] override  def classTag :ClassTag[_] = implicitly[ClassTag[AuditBuff[Any]]]


		protected def apply[T](map :T => T) :AuditBuff[T] = new AuditBuff(this, map)

		def fold[T](mapping :MappingOf[T]) :T => T = fold(mapping.buffs)

		def fold[T](buffs :Seq[Buff[T]]) :T => T = {
			val audits = Audit(buffs)
			if (audits.isEmpty) identity[T]
			else audits.reduce(_ andThen _)
		}

		object Audit {
			@inline def apply[T](buff :Buff[T]) :Option[T => T] = unapply(buff)
			@inline def apply[T](buffs :Seq[Buff[T]]) :Seq[T => T] = unapply(buffs)
			@inline def apply[T](mapping :MappingOf[T]) :Seq[T => T] = unapply(mapping)

			def unapply[T](buff :Buff[T]) :Option[T => T] = buff match {
				case sub :AuditBuff[T] if sub is self => Some(sub.substitute)
				case _ => None
			}

			def unapply[T](buffs :Seq[Buff[T]]) :Seq[T => T] =
				buffs collect { case sub :AuditBuff[T] if sub is self => sub.substitute }

			def unapply[T](component :MappingOf[T]) :Seq[T => T] = unapply(component.buffs)
		}
	}





	/** A `ManagedBuff` is a combination of a `ValueBuff` and `AuditBuff`, carrying both a by-name value
	  * and inspection/scanning function. When each of these is used depends, as always, on the associated buff type.
	  * This choice is made by implying one of the predefined 'audit' buff types: `SelectAudit`, `QueryAudit`,
	  * `InsertAudit` and `UpdateAudit`.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ManagedBuffType]]
	  */
	class ManagedBuff[T](override val buffType :ManagedBuffType, init: =>T, update :T => T)
		extends AuditBuff[T](buffType, update) with ValueBuff[T]
	{
		override def value :T = init

		override def map[X](there :T => X) :Nothing =
			throw new UnsupportedOperationException(toString + ".map: ManagedBuff can't be mapped unidirectionally.")

		override def cascade[X](there :T => X) :Option[Nothing] = None

		override def bimap[X](there :T => X, back :X => T) :ManagedBuff[X] =
			new ManagedBuff(buffType, there(init), back andThen substitute andThen there)


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef => this eq self
			case _ => false //won't happen
		}

		override def hashCode :Int = System.identityHashCode(this)

		override def toString :String = buffType.toString + "(?, " + substitute + ")"
	}



	/** A `ManagedBuffType` is a combination of a `ValueBuffType` and `AuditBuffType`, with the intention
	  * of using one for some types of SQL statements and the other for other types. Most typically this means
	  * using a generated value for insert and a modified value for update statements, but any combination
	  * is possible as long as the sets of affected statements are disjoint. This selection is made, as with
	  * the base types, by implying some subset of `ExtraSelect`, `ExtraQuery`, `ExtraInsert`, `ExtraUpdate`,
	  * `SelectAudit`, `QueryAudit`, `InsertAudit`, `UpdateAudit`.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ManagedBuff]]
	  */
	trait ManagedBuffType extends ValueBuffType with AuditBuffType with DedicatedBuffType[ManagedBuff] {
		protected[this] override def classTag :ClassTag[_] = implicitly[ClassTag[ManagedBuff[Any]]]

		override protected def apply[T](map :T => T) :AuditBuff[T] =
			throw new UnsupportedOperationException(
				toString + ".apply(" + map + "): ManagedBuffType requires an initial value; use the two-argument constructor."
			)

		def apply[T](init: =>T, update :T => T) :ManagedBuff[T] = new ManagedBuff(this, init, update)

	}






	/** An exception thrown when the [[net.noresttherein.oldsql.schema.Mapping.optMap Mapping.optMap]] operation
	  * fails due to presence of a buff which cannot be mapped with the given function, either because
	  * the operation is not supported at all, or the function returned `None` for the `Buff`'s value.
	  * This exception can be thrown both from the `optMap` method and at some later point, when the buff's value
	  * is accessed for buffs which generated values.
	  */
	class BuffMappingFailureException(msg :String, cause :Throwable = null) extends RuntimeException(msg, cause)
}

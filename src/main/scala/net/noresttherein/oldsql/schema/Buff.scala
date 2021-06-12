package net.noresttherein.oldsql.schema

import java.sql

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.schema.Buff.{BuffExtractor, BuffType}
import net.noresttherein.oldsql.schema.Mapping.{ColumnFilter, MappingOf}
import net.noresttherein.oldsql.schema.bits.Temporal
import net.noresttherein.oldsql.slang._
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.{Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.OldSQLException
import net.noresttherein.oldsql.morsels.generic.{Fixed, Self}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Buff.BuffType.NamedBuffType
import net.noresttherein.oldsql.schema.Buff.FlagBuffType.NamedFlag






/** An optional annotation/modifier for component mappings, especially columns.
  * Modifies the way the annotated component is mapped, in particular it is used to include or exclude
  * certain columns from select/insert/update statements. Each `Buff` is associated with a single
  * [[net.noresttherein.oldsql.schema.Buff.BuffType BuffType]], which serves as a factory, matcher and a virtual 'class'.
  * These factories can be used to test if a component is annotated with a buff of a particular type and retrieve
  * its information, if present. Unless otherwise noted or implied from the documentation, buffs apply only to automatic
  * translation of application `save/load` requests to SQL ''insert''/''update''/''select'' and value assembly in general,
  * but not to SQL statements created using the SQL DSL. In particular, a manually created SQL ''select'' is allowed
  * to use any columns of the tables in the ''from'' clause, regardless of their flags which control their availability
  * for a given operation type.
  *
  * The term 'buff' can refer in the documentation to both a buff instance (that is, an instance of this class),
  * and to a buff type - an instance of `BuffType`. It should be clear from context and method signatures which
  * is being referred to, and in places where both are mentioned, `BuffType` is explicitly called buff type or
  * the class names are used. Similarly, a buff type can potentially mean 'Scala type of a buff' and `BuffType`;
  * the term is used almost exclusively in the former meaning, and the latter is expanded to 'Scala buff type'
  * to avoid ambiguity. Last but not least, there are various `Buff` subclasses and matching broad `BuffType` categories
  * such as [[net.noresttherein.oldsql.schema.Buff.ValueBuffType value buffs]] and
  * [[net.noresttherein.oldsql.schema.Buff.FlagBuffType flag buffs]] (or flags). To avoid adding to the confusion,
  * they are collectively (both buff and its buff type) are called ''buff kinds''. Buffs are simple objects unlikely
  * to use higher kinds of Scala types, so there should be no ambiguity in this area.
  *
  * See [[net.noresttherein.oldsql.schema.Buff.BuffType BuffType]] for more information.
  * @see [[net.noresttherein.oldsql.schema.Buff.FlagBuff FlagBuff]]
  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]
  * @see [[net.noresttherein.oldsql.schema.Buff.AuditBuff AuditBuff]]
  * @see [[net.noresttherein.oldsql.schema.Buffs Buffs]]
  * @tparam T the subject type of the annotated component.
  */
trait Buff[T] extends Serializable {
	//todo: semantics of use in SQL DSL
	//consider: Buff for column prefix

	/** If `true`, export versions of subcomponents of the annotated component should inherit this buff.
	  * Note that this flag applies only to buffs created by this buff's factory, not any other buffs which
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff. The effect is that this buff's type
	  * will be automatically active on all columns of the component buffed with this instance, with the optional value
	  * obtained from this buff's value (if it is a [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]).
	  */
	def cascades :Boolean = buffType.cascades

	/** The type of this buff, i.e. the factory that created this instance. */
	def buffType :BuffType

	/** Is this buff of the buff type defined by `group`, carrying information defined by the type?
	  * This will be true if this buff was created by `group` or if the type which created this buff
	  * has a strictly narrower meaning than the passed argument, effectively implying the latter.
	  * This method delegates the test to the associated `BuffType`'s
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implies]].
	  */
	def is(group :BuffType) :Boolean = buffType.implies(group)

	/** Applies the given extractor to self if its associated buff type is implied by the one that created this buff.
	  * This is a lower level method called in double dispatch by certain buff types so as to pass the full
	  * responsibility of matching to the buff. In particular it allows to not match a buff type `t` even if
	  * `this is t` is true. This is used to implement guarded value generators which result in lack of a match
	  * instead of an error when the generator throws an exception.
	  */
	def apply[V[_]](extractor :BuffExtractor[V]) :Opt[V[T]] =
		if (buffType implies extractor.buffType) extractor.get(this) else Lack

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
	  */ //consider: a better name (impose? adopt? bestow?)
	def cascade[X](f :T => X) :Option[Buff[X]] =
		if (cascades) Some(map(f)) else None

	/** An unsafe, conditional variant of [[net.noresttherein.oldsql.schema.Buff.cascade cascade]] method for
	  * [[net.noresttherein.oldsql.morsels.Extractor extractors]] which might not produce a value for all possible
	  * arguments. The decision of whether to adapt this buff to a new subject type is implementation specific
	  * and varies between buff [[net.noresttherein.oldsql.schema.Buff.BuffType types]] and kinds.
	  * The default is to delegate to the basic `cascade` only if the extractor is
	  * a [[net.noresttherein.oldsql.morsels.Extractor.RequisiteExtractor RequisiteExtractor]]. This changes however
	  * with all standard buff kinds: [[net.noresttherein.oldsql.schema.Buff.FlagBuffType flags]] always cascade
	  * based only on the buff's [[net.noresttherein.oldsql.schema.Buff.cascades cascades]] property.
	  * [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType Constant buffs]] will by default attempt to eagerly
	  * evaluate the function for their value, catching thrown exception and simply not cascading instead of
	  * propagating the error. [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]s with
	  * a [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType by-name expression]] as their value morph
	  * into a special proxy implementation, which will be ignored by all implied buff types if it were to throw
	  * an exception. All these can however by overriden with custom buff kind implementations.
	  */
	def cascadeGuard[X](extractor :T =?> X) :Option[Buff[X]] =
		if (cascades) extractor.requisite match {
			case Got(f) => cascade(f)
			case _ => None
		} else None


	/** Adapts this buff for a new component type. For flag buffs, this is an identity operation; for buffs
	  * with values it creates a new buff of the same type carrying the mapped value. This method
	  * differs from `map` in that it also works for `AuditBuff` instances.
	  */
	def bimap[X](there :T => X, back :X => T) :Buff[X] = map(there)


	def canEqual(that :Any) :Boolean = that.getClass == this.getClass

	override def toString :String = buffType.toString
}






object Buff {

	/** This column/component is not included in the default select statement and must be specified explicitly,
	  * if at all possible. Implied by [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]] and
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]].
	  */
	case object NoSelectByDefault extends AbstractBuffType

	/** This column/component is not included in the default filter condition when comparing the buffed
	  * component/entity and must be specified explicitly, if at all possible. Implied by
	  * [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]] and
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]].
	  */
	case object NoFilterByDefault extends AbstractBuffType

	/** This column/component is not included in the default insert statement and must be specified explicitly,
	  * if at all possible. Implied by [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]] and
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]].
	  */
	case object NoInsertByDefault extends AbstractBuffType

	/** This column/component is not included in the default update statement and must be specified explicitly,
	  * if at all possible. Implied by [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] and
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]].
	  */
	case object NoUpdateByDefault extends AbstractBuffType


	/** This column/component can't be included in a select statement (as part of its select clause).
	  * Implies [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]], which is the main buff
	  * which should be checked to determine if a column should be selected.
	  */
	case object NoSelect extends ComboFlag(NoSelectByDefault)

	/** This column/component can't be included as part of the ''where'' clause of a select or update statement.
	  * Implies [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]], which is the main buff
	  * which should be checked to determine which columns should be compared.
	  */
	case object NoFilter extends ComboFlag(NoFilterByDefault)

	/** This column/component can't be included in an insert statement (as the inserted column).
	  * Implies [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]], which is the main buff
	  * which should be checked to determine if a column should be inserted.
	  */
	case object NoInsert extends ComboFlag(NoInsertByDefault)

	/** This column/component can't be included in an update statement (as the updated column).
	  * Implies [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]], which is the main buff
	  * which should be checked to determine if a column should be updated.
	  */
	case object NoUpdate extends ComboFlag(NoUpdateByDefault)

	/** This column/component is never written to the database by the application. Note that 'read only' means here
	  * 'no write' and the column can still be non selectable
	  * (see [[net.noresttherein.oldsql.schema.Buff.Virtual Virtual]] and
	  * [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]]).
	  * Implies [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]] and
	  * [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]].
	  */
	case object ReadOnly extends ComboFlag(NoInsert, NoUpdate)

	/** A buff type which marks columns ignored by the enclosing mapping. It is useful when a column is still
	  * used as part of SQL statements. Implies [[net.noresttherein.oldsql.schema.Buff.ReadOnly ReadOnly]],
	  * [[net.noresttherein.oldsql.schema.Buff.ReadOnly ReadOnly]] and
	  * [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.Virtual Virtual]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.Partition Partition]]
	  */
	case object Ignored extends ComboFlag(ReadOnly, NoFilter, NoSelect)



	/** The value for this column/component is generated by the database on insert and should be returned by the
	  * insert statement. */ //this should conflict with NoSelect
	case object AutoInsert extends ComboFlag(NoInsert)

	/** The value for this column/component is updated by the database whenever a mapped row is updated. */
	case object AutoUpdate extends ComboFlag(NoUpdate) //this is superfluous if we don't need the value to be returned

	/** The value for this column/component is generated by the database on insert and updated on each update,
	  * making it read only for the application. */
	case object AutoGen extends ComboFlag(AutoInsert, AutoUpdate, ReadOnly)



	/** A default value which will be returned by annotated component mapping if none is preset and none can
	  * be assembled from subcomponents. This can be used for example with nullable columns, but will also
	  * come into play if the column is missing from the ''select'' clause or in case of 'null' values
	  * coming from outer joins.
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalSelect]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.Default]]
	  */
	case object SelectDefault extends ComboValueBuffType

	/** A factory for buffs marking that a given column/component can be omitted from the select clause.
	  * It is still included by default and needs to be excluded explicitly. Created values carry a placeholder
	  * value to assign to the annotated component on assembly if the component is excluded.
	  * The column can be excluded from a [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] expression by
	  * providing it to the [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.exclude exclude]]
	  * (or [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.exclude exclude]])
	  * and [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.alter alter]]
	  * (or [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.alter alter]]
	  * method of the expression from the ''select'' clause for an owning entity/component,
	  * or on the level of [[net.noresttherein.oldsql.schema.Relation Relation]]:
	  * [[net.noresttherein.oldsql.schema.Relation.exclude exclude]] and
	  * [[net.noresttherein.oldsql.schema.Relation.apply(components* Relation(...)]].
	  */
	case object OptionalSelect extends ContradictoryValueBuffType(SelectDefault)(NoSelect)

	/** A factory for buffs marking that a given column/component is omitted by default from the select clause
	  * and needs to be included explicitly. When not included, the value stored in the buff will be used
	  * as the value for the annotated component. It implies `OptionalSelect` and `NoSelectByDefault`.
	  * The column can be included either by listing it among ''fetch'' components of
	  * [[net.noresttherein.oldsql.hoard.Pile Pile]] methods,
	  * by [[net.noresttherein.oldsql.schema.Relation.apply Relation(...)]],
	  * [[net.noresttherein.oldsql.schema.Relation.include Relation.include]],
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.include include]] & [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.include include]],
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.alter alter]] & [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.alter alter]],
	  * or simply by explicitly including it in a [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] expression
	  * along the containing entity/component.
	  */
	case object ExplicitSelect extends ComboValueBuffType(OptionalSelect, NoSelectByDefault)

	/** A buff marking a column as non-selectable, and providing the value for the annotated component.
	  * This can be used in particular for 'virtual' columns - components which take part in the mapping, but
	  * aren't present in the database at all.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Virtual$]]
	  */
	case object ExtraSelect extends ComboValueBuffType(SelectDefault, NoSelect)



	/** A buff marking that a given column or component can be omitted from the the parameter list of the ''where''
	  * clause of an SQL statement. This covers the case when a comparison in an SQL expression happens between
	  * whole subjects of a multi column mapping, rather than listing the columns individually. The annotated component
	  * is still included by default when comparing the owning mapping's subjects and needs to be excluded explicitly.
	  * The component can be excluded through the 'alter' methods of expressions for any owning component:
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.exclude]]
	  * (and [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.exclude]]),
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.alter]]
	  * (and [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.alter]]), thusly excluding the component
	  * from that particular expression. Alternatively, corresponding methods of
	  * [[net.noresttherein.oldsql.schema.Relation Relation]] can be used, which will exclude it
	  * (unless explicitly included) from all occurrences of any owning component in an SQL ''select''
	  * using the relation: [[net.noresttherein.oldsql.schema.Relation.exclude exclude]] and
	  * [[net.noresttherein.oldsql.schema.Relation.apply(components* Relation(...)]].
	  */
	case object OptionalFilter extends ContradictoryFlag()(NoFilter)

	/** A buff marking that a given column or component can be omitted from the parameter list of the ''where'' clause
	  * of an SQL statement and needs to be included explicitly. This applies when the comparison expression
	  * happens on the level of the subject of a multi column mapping enclosing the buffed component without listing
	  * its columns individually. It implies `OptionalFilter` and `NoFilterByDefault`.
	  * The component can be included through the 'alter' methods of expressions for any owning component:
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.include]]
	  * (and [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.include]]),
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.alter]]
	  * (and [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.alter]]), thusly including the component
	  * as part of that particular expression. Alternatively, corresponding methods of
	  * [[net.noresttherein.oldsql.schema.Relation Relation]] can be used, which will include it
	  * (unless explicitly excluded) in all its occurrences in an SQL ''select'' using the relation:
	  * [[net.noresttherein.oldsql.schema.Relation.include include]] and
	  * [[net.noresttherein.oldsql.schema.Relation.apply(components* Relation(...)]].
	  */
	case object ExplicitFilter extends ComboFlag(OptionalFilter, NoFilterByDefault)

	/** A buff type marking that a given column/component must be included in every query against the table, using
	  * the value provided by the buff. It implies `NoSelect` and `NoFilter` and is used to artificially limit the number
	  * of mapped entities.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Unmapped$]]
	  */ //is it really the best to make it imply NoFilter? It is necessary in ExtraSelect, but here may create confusion
	case object ExtraFilter extends ComboValueBuffType(NoSelect, NoFilter)



	/** Provides a default value which will be inserted into the table
	  * if the [[net.noresttherein.oldsql.schema.MappingExtract extract]] for the annotated component returns no value.
	  * This in particular includes columns not mapped to properties of the entity class.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Default]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.WriteDefault]]
	  */
	case object InsertDefault extends ContradictoryValueBuffType()(NoInsert)

	/** A buff specifying that a given column/component can be omitted from the insert statement.
	  * It is still included by default and needs to be excluded explicitly.
	  */
	case object OptionalInsert extends ContradictoryFlag()(NoInsert)

	/** A buff marking that a given column/component is not inserted by default into the underlying table
	  * and needs to be included explicitly. It implies `OptionalInsert` and `NoInsertByDefault`. */
	case object ExplicitInsert extends ComboFlag(OptionalInsert, NoInsertByDefault)

	/** Marks a column/component as having its value initialized by the expression provided by the buff
	  * rather than the entity. Used particularly for 'created on' or 'created by' type of columns.
	  */ //consider: maybe it should not be cascading?
	case object ExtraInsert extends ComboValueBuffType(InsertDefault, NoInsert) //fixme: currently unused!


	/** Provides a default value which will be included in the update for the mapped table
	  * if the [[net.noresttherein.oldsql.schema.MappingExtract extract]] for the annotated component returns no value.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Default]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.WriteDefault]]
	  */
	case object UpdateDefault extends ContradictoryValueBuffType()(NoUpdate)

	/** A buff marking that a given column/component can be omitted from the update statement.
	  * It is still included by default and needs to be excluded explicitly. */
	case object OptionalUpdate extends ContradictoryFlag()(NoUpdate)

	/** A buff marking that a given column/component is not included by default in the update statements
	  * and needs to be included explicitly. It implies `OptionalUpdate` and `NoUpdateByDefault`. */
	case object ExplicitUpdate extends ComboFlag(OptionalUpdate, NoUpdateByDefault)

	/** Marks a column/component as being updated with the value of the expression provided by the buff
	  * rather than some property of the mapped entity. Useful for particularly for 'update timestamp' columns.
	  */ //consider: maybe it should not be cascading?
	case object ExtraUpdate extends ComboValueBuffType(UpdateDefault, NoUpdate) //fixme: currently unused!



	/** Provides a default value which will be used when inserting or updating the rows of the mapped table
	  * if the [[net.noresttherein.oldsql.schema.MappingExtract extract]] for the annotated component returns no value.
	  * Implies [[net.noresttherein.oldsql.schema.Buff.InsertDefault InsertDefault]] and
	  * [[net.noresttherein.oldsql.schema.Buff.UpdateDefault UpdateDefault]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.Default]]
	  */
	case object WriteDefault extends ComboValueBuffType(InsertDefault, UpdateDefault)

	/** Marks a column/component as not mandatory for insert and update statements. */
	case object OptionalWrite extends ComboFlag(OptionalInsert, OptionalUpdate)

	/** Marks a column/component as not included in the insert/update statements by default and needing to be included explicitly. */
	case object ExplicitWrite extends ComboFlag(ExplicitInsert, ExplicitUpdate, OptionalWrite)

	/** Marks a column/component as having its value set by this buff rather than a property of the entity
	  * at every write to the database. Implies `ReadOnly`, `ExtraInsert` and `ExtraUpdate`.
	  */
	case object ExtraWrite extends ComboValueBuffType(ReadOnly, WriteDefault, ExtraInsert, ExtraUpdate)


	/** Provides a default value which will be returned if the value for the component is not present in the ''select''
	  * as well as when inserting or updating the rows of the mapped table
	  * if the [[net.noresttherein.oldsql.schema.MappingExtract extract]] for the annotated component returns no value.
	  * This has the effect of not only mapping `null` columns to the given value, but also replacing any `null` values
	  * with the value extracted from the provided default.
	  * Implies [[net.noresttherein.oldsql.schema.Buff.SelectDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.InsertDefault InsertDefault]]
	  * and [[net.noresttherein.oldsql.schema.Buff.UpdateDefault UpdateDefault]].
	  */
	case object Default extends ComboValueBuffType(SelectDefault, WriteDefault)

	/** Signifies that a column/component can be excluded from all types of database operations.
	  * It is a shortcut for marking it with `OptionalSelect`, `OptionalFilter`, `OptionalWrite`.
	  */
	case object Optional extends ComboValueBuffType(OptionalSelect, OptionalFilter, OptionalWrite)

	/** Signifies that a column/component must be listed explicitly in order to be included in any database operation
	  * (it is not included by default).
	  * It is a shortcut for marking it with `Optional`, `ExplicitSelect`, `ExplicitFilter` and `ExplicitWrite`.
	  */
	case object Explicit extends ComboValueBuffType(Optional, ExplicitSelect, ExplicitFilter, ExplicitWrite)


	/** Marks a column or component which is not part of the mapped scala class, but is still part of the mapped
	  * entity from the relational point of view. All rows which are subject to mapping by the application have
	  * the value returned by the buff, essentially partitioning the table and limiting the application to a subset
	  * of its rows. It implies both `ExtraFilter` and `ExtraWrite`, meaning that all queries against the table will
	  * include the annotated column in the filter and all inserts and updates will set its value based on this buff.
	  */
	case object Partition extends ComboValueBuffType(NoSelect, ExtraFilter, ExtraInsert, NoUpdate) //SelectDefault?

	/** A buff marking a component or column which does not exist in the database (or at least it not mapped
	  * and required) and will not be used as part of any SQL statements under any circumstances. It is still part
	  * of the mapping and, during assembly, the provided expression is used as its value. This can be useful
	  * during schema migrations, when a mapping might need to cover several versions of the schema, or if it is reused
	  * for several similar tables. Alternatively, it can be used for mapping's which components are already included
	  * as subcomponents of some other component of the same table.
	  */
	case object Virtual extends ComboValueBuffType(ExtraSelect, NoFilter, ReadOnly)



	/** Any value returned from the select (or assembled from such values) of a column/component annotated
	  * with this buff type must be mapped with the function included in the buff. This buff is independent
	  * from buffs specifying whether and when a component can be included in a select clause. */
	case object SelectAudit extends ComboBuffType(Audit) with AuditBuffType

	/** Any value compared in SQL with the value of the annotated column/component is first mapped with the function
	  * included in the buff. This is independent of whether the component can be included in the filter condition
	  * at all.*/
	case object FilterAudit extends ComboBuffType(Audit) with AuditBuffType

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
	  * through a `S => S` function provided by the buff. This buff type thus makes a good extension point
	  * for consistency validation, both of data already in the database and that being written. If you wish to limit
	  * the check only to insert and update operations, use
	  * the [[net.noresttherein.oldsql.schema.Buff.WriteAudit$ WriteAudit]] buff instead (or those specifically dedicated
	  * to a single database operation type0.
	  */
	case object Audit extends ComboBuffType(SelectAudit, FilterAudit, WriteAudit) with AuditBuffType



	/** A flag signifying that mapped values can be null. */
	case object Nullable extends FlagBuffType with NonCascading

	/** A flag signifying that mapped values cannot be null. This is the default behaviour, so this buff will be
	  * superfluous in most circumstances. It may be useful however to annul the `Nullable` buff.
	  */
	case object NotNull extends ComboFlag(false)(Nullable)



	/** Marks that a column/component ''must'' be included as part of the ''where'' clause of any update statement. */
	case object UpdateMatch extends FlagBuffType with Cascading

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






	/** An unspecified matching pattern used to extract values of type `V[T]` from (some subclass of) buffs `Buff[T]`.
	  * This is a low-level callback invoked by buffs from their [[net.noresttherein.oldsql.schema.Buff.apply apply]]
	  * method to return the result of a match as a way of reversing the control over matching.
	  * The buff will check first if its [[net.noresttherein.oldsql.schema.Buff.buffType buffType]] implies
	  * the buff type of this extractor before, leaving only the actual extraction of a value to this trait's
	  * [[net.noresttherein.oldsql.schema.Buff.BuffExtractor.get get]].
	  */
	trait BuffExtractor[+V[_]] {
		def buffType :BuffType
		def get[T](buff :Buff[T]) :Opt[V[T]]
	}



	/** A 'class for classes' being subtypes of `Buff`. It implicitly defines the information carried by the buffs of
	  * this type and indicates their purpose or application conditions. It can be used both as a factory and matcher,
	  * testing buff instances, collections, and mappings for the presence of a corresponding buff. This is an abstract
	  * base class and as such declares no `apply/unapply` methods, leaving it to subclasses which define them
	  * in terms of the information carried by their buffs. Whenever several matching buffs are present on a list
	  * or in a mapping, the buff type will always pick the first one. Similarly, if a subtype extracts ''all''
	  * matching buffs from a list or a component, it will do so preserving their order. Repeated instances
	  * of the same buff type are allowed - it would be impossible to prevent it for a common buff 'supertype'
	  * of two buffs appearing in tandem.
	  *
	  * Note that buffs can be attached also to non-column components. The exact behavior in that case depends both
	  * on the buff type and the mapping class. By default the subcomponents of a mapping inherit those buffs defined
	  * by enclosing mappings which have property [[net.noresttherein.oldsql.schema.Buff.cascades cascades]]
	  * set to true, with inherited buffs following the buffs declared locally. Not all buff kinds can be adopted though:
	  * there is no way for example to map an [[net.noresttherein.oldsql.schema.Buff.AuditBuff AuditBuff]]`[T]` with
	  * a single function `T => S` and other restrictions may also be in play.
	  *
	  * Instances of this class form a subtype relation of sorts, meaning certain buffs created by some factories
	  * are recognized as 'belonging' also to other buff types. This is implemented through the
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implies]] method. If a `BuffType` `A` implies
	  * a `BuffType` `B`, then `B.active(a)` and `a is B` will be true for every buff `a` created by the buff type `A`.
	  * If the actual `Buff` class, defining what information is carried by a buff, used by the type `A`
	  * is a subclass of the class of buffs produces by the type `B` (which should be the case for all such pairs),
	  * then also any extractor methods defined by the type `B` will pick up the data from buffs `A`.
	  * This means that, for example, any `BuffType` can safely imply any instance of `FlagBuffType` and any additional
	  * information carried by the buff being tested is ignored for the purpose of the check.
	  * On the other hand, creating a `FlagBuffType` which implies `ValueBuffType` would be an application error:
	  * because the buff does not conform to the expected type, it would not be matched by any patterns of the
	  * `ValueBuffType`, including those which do not access the actual value like
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.active active]] and
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.inactive inactive]]. In general though it should not be assumed
	  * that belonging to a buff type guarantees being matched by that buff type extractors, as other counterexamples
	  * exist. For convenience of implementation, there are `ComboBuffType` and `ComboFlag` classes which accept
	  * a list of implied buff types as constructor arguments.
	  *
	  * In addition to this implication relation, buffs can have mutually exclusive meanings. This is defined by
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradicts]] method: if a buff of a type
	  * contradicted by this buff type is present on a buff list, it annuls all contradicted buffs which follow it,
	  * including those inherited. While the relation is not transitive, all buff types which contradicted by
	  * a buff ''implied'' by this type are also automatically considered contradicted by this type. There is no formal
	  * definition or requirement as to when two buff types should contradict each other, but should follow
	  * the policy of least surprise. It certainly is not exhaustive in terms of the logical implications modeled
	  * by these buffs: satisfying it is not a guarantee that there is no conflict of meanings between any two buffs
	  * in a collection. In general, however, situations where a buff overrides other buffs should generally be avoided
	  * if possible, as it can lead to confusing situations. No buff should ever test both as contradictory and
	  * belonging to a single buff type and behaviour in that case is unspecified.
	  *
	  * Certain buff types are ''abstract'', meaning they can't be used to create new instances, but serve only
	  * as a grouping of more specific buff types which imply it. This is analogous to a common abstract base class.
	  * In particular, certain buff types serve only to indicate ''when'' the buff should be applied, without any
	  * information about its purpose. For example, creating
	  * a [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]],
	  * which implies the [[net.noresttherein.oldsql.schema.Buff.UpdateAudit$ UpdateAudit]], will map all updated values
	  * with the function provided by the buff before they are written to the database.
	  *
	  * Equality tests are paramount for this classes use case. As most buff types - and all those recognized
	  * by the framework - are static in nature, represented by global constants, it is convenient to declare them
	  * as singleton objects and rely on standard java reference equality for speed. This is not required though,
	  * and there are generic factory methods for various buff type classes, returning value-based instances.
	  *
	  * See [[net.noresttherein.oldsql.schema.Buff$ Buff]] for the full list of predefined buff types.
	  *
	  * @see [[net.noresttherein.oldsql.schema.Buff.FlagBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.AuditBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ManagedBuffType]]
	  */
	trait BuffType extends Serializable { factory =>

		/** If `true`, export versions of subcomponents of the annotated component should inherit this buff.
		  * Note that this flag applies only to buffs created by this instance, not any other buffs which
		  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff. The result is that this buff type
		  * will be automatically active on all columns and subcomponents of a component buffed by this instance,
		  * with the optional value obtained from the buff's value
		  * (if it is a [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]).
		  * Other buff types implying this buff can be cascading, and ultimately it is
		  * a [[net.noresttherein.oldsql.schema.Buff Buff]] property which is in effect.
		  */
		def cascades :Boolean

		/** Checks if this buff type ''implies'' another buff type. If it does, tests for presence of `other`
		  * on a mapping will be positive also if this buff is present. This makes this buff type
		  * a specialization of the more generic `other` in a way similar to class inheritance. For this to work however,
		  * the implied buff type must use the same `Buff` class as this instance, or a more generic one.
		  * For example, a `FlagBuffType` implying a `ValueBuffType` will not be taken into account, as the latter
		  * specifically checks only for `ValueBuff` subclasses. Implication in the other direction will work
		  * however, as a `FlagBuffType` is satisfied with a `Buff` of any class. This relation is transitive,
		  * but not symmetrical and cannot contain cycles. This constraint is unimportant however, as all buffs in an
		  * implication cycle would be equivalent.
		  *
		  * Default implementation calls in double dispatch method
		  * `other `[[net.noresttherein.oldsql.schema.Buff.BuffType.impliedBy impliedBy]]` this`, but buff types which
		  * specifically imply other listed types ([[net.noresttherein.oldsql.schema.Buff.ComboBuffType ComboBuffType]])
		  * will expand this test by checking for implication by transitiveness.
		  * @see [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts]]
		  */
		def implies(other :BuffType) :Boolean = other impliedBy this

		/** Checks if this buff type ''knows'' that it is implied by the given buff type. Negative result here does not
		  * automatically mean that there is no implication - in fact default implementation yields `true` only
		  * if the two buffs are equal. You should use [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implies]]
		  * instead, for which this method is the target of a double dispatch call. Likewise, only some very specific
		  * `BuffType` implementations override this method.
		  */
		def impliedBy(other :BuffType) :Boolean = this == other

		protected[schema] val knownImplied :Unique[BuffType] = Unique.single(this)

		/** Checks if this buff shadows another buff type. If true, presence of this buff on a mapping will
		  * essentially annul the other buff, providing this buff was declared lower in the hierarchy
		  * (i.e it is either defined on that mapping, or the component inclusion 'inheritance' path is shorter.
		  * This relation is not transitive, but holds also if any buff type
		  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implied]] by this buff contradicts the other buff.
		  * It is automatically made symmetrical, as the default implementation performs a symmetrical check
		  * using delegate method [[net.noresttherein.oldsql.schema.Buff.BuffType.impliedContradicts impliedContradicts]],
		  * which should be overriden rather than this method.
		  * @return `(this impliedContradicts other) || (other impliedContradicts this)`.
		  */
		def contradicts(other :BuffType) :Boolean = (this impliedContradicts other) || (other impliedContradicts this)

		/** Checks if buffs [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implied]] by this buff
		  * [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradict]] the given buff.
		  * The check is performed by following the declarations of implication and contradiction,
		  * which must not contain cycles. Default implementation always returns `false`.
		  */
		def impliedContradicts(other :BuffType) :Boolean = false


		/** Match pattern for buffs of this type, checking them simply for existence, without extracting any values.
		  * Note that this pattern matches if and only if
		  * `this.`[[net.noresttherein.oldsql.schema.Buff.BuffType.get get(buff)]] returns a value and does not depend
		  * solely on the [[net.noresttherein.oldsql.schema.Buff.is is]] relation. All extractor methods delegate
		  * eventually to the `get(buff)` method of the enclosing buff type, with a possible intermediate role
		  * of [[net.noresttherein.oldsql.schema.Buffs.apply Buffs.apply]].
		  */
		case object Active {
			val columns = new ColumnFilter.WithBuff(BuffType.this)

			@inline def apply[T](buff :Buff[T]) :Boolean = !get(buff).isEmpty
			@inline def apply[T](buffs :Iterable[Buff[T]]) :Boolean = !get(buffs).isEmpty
			@inline def apply[T](buffs :Buffs[T]) :Boolean = !buffs(BuffType.this).isEmpty
			@inline def apply[T](mapping :MappingOf[T]) :Boolean = !mapping.buffs(BuffType.this).isEmpty

			@inline def unapply[T](buff :Buff[T]) :Boolean = !get(buff).isEmpty
			@inline def unapply[T](buffs :Iterable[Buff[T]]) :Boolean = !get(buffs).isEmpty
			@inline def unapply[T](buffs :Buffs[T]) :Boolean = !buffs(BuffType.this).isEmpty
			@inline def unapply[T](mapping :MappingOf[T]) :Boolean = !mapping.buffs(BuffType.this).isEmpty
		}

		/** Match pattern for buffs ''not'' of this type, checking them simply for existence,
		  * without extracting any values. When matching a mapping or a buff collection, all included buffs must match
		  * (i.e. be not of this type) for the whole match to test positive, unlike the `Active` pattern.
		  * Note that this pattern matches if and only if
		  * `this.`[[net.noresttherein.oldsql.schema.Buff.BuffType.get get(buff)]] des not return a value,
		  * and not being in the [[net.noresttherein.oldsql.schema.Buff.is is]] relation is only a required condition,
		  * not sufficient. All extractor methods delegate eventually to the `get(buff)` method of the enclosing
		  * buff type, with a possible intermediate role of [[net.noresttherein.oldsql.schema.Buffs.apply Buffs.apply]].
		  */
		case object Inactive {
			val columns = new ColumnFilter.WithoutBuff(BuffType.this)

			@inline def apply[T](buff :Buff[T]) :Boolean = get(buff).isEmpty //buff(Active).isEmpty
			@inline def apply[T](buffs :Iterable[Buff[T]]) :Boolean = get(buffs).isEmpty
			@inline def apply[T](buffs :Buffs[T]) :Boolean = buffs(BuffType.this).isEmpty
			@inline def apply[T](mapping :MappingOf[T]) :Boolean = mapping.buffs(BuffType.this).isEmpty

			@inline def unapply[T](buff :Buff[T]) :Boolean = get(buff).isEmpty
			@inline def unapply[T](buffs :Iterable[Buff[T]]) :Boolean = get(buffs).isEmpty
			@inline def unapply[T](buffs :Buffs[T]) :Boolean = buffs(BuffType.this).isEmpty
			@inline def unapply[T](mapping :MappingOf[T]) :Boolean = mapping.buffs(BuffType.this).isEmpty
		}

		private object Get
			extends BuffExtractor[Buff] with PartialFunction[Buff[_], Buff[_]] with Serializable
		{
			override def buffType :BuffType = BuffType.this
			override def get[T](buff :Buff[T]) :Opt[Buff[T]] = Got(buff)

			def apply[T](buffs :Iterable[Buff[T]]) :Option[Buff[T]] = {//preserve the Option to avoid second boxing to Opt
				val res = buffs.collectFirst(this.asInstanceOf[PartialFunction[Buff[T], Buff[T]]])
				if (res.isDefined && res.get == null) None //we found a contradictory buff
				else res
			}

			override def isDefinedAt(buff :Buff[_]) :Boolean =
				(buff.buffType contradicts BuffType.this) || buff(this).isDefined

			override def apply(buff :Buff[_]) :Buff[_] =
				if (buff.buffType contradicts BuffType.this) null
				else buff(this).orFail(buff.toString + " is not a " + BuffType.this + " buff.")

			override def applyOrElse[A1 <: Buff[_], B1 >: Buff[_]](buff :A1, default :A1 => B1) :B1 =
				if (buff.buffType contradicts BuffType.this) null
				else buff(this) getOrElse default(buff)
		}

		/** Checks if the buff [[net.noresttherein.oldsql.schema.Buff.is is]] of this type and is also otherwise
		  * compatible with it. This will return `true` ''iff'' [[net.noresttherein.oldsql.schema.Buff.BuffType.get get]]
		  * method for this buff would return a value.
		  */
		def active[T](buff :Buff[T]) :Boolean = !buff(Get).isEmpty

		/** Checks if the given collection contains a matching buff, as defined by the overloaded `active` method
		  * for a single buff, and that no preceding buff
		  * is [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradictory]] to this type.
		  */
		def active[T](buffs :Iterable[Buff[T]]) :Boolean = !Get(buffs).isEmpty

		/** Checks if the given collection contains a matching buff, as defined by the overloaded `active` method
		  * for a single buff, and that no preceding buff
		  * is [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradictory]] to this type.
		  * The implementation delegates the check to the `buffs` argument, but it will be consistent with the
		  * overloaded method accepting a generic `Iterable[Buff[_]]`.
		  * @see [[net.noresttherein.oldsql.schema.Buffs.apply]]
		  */
		def active[T](buffs :Buffs[T]) :Boolean = !buffs(this).isEmpty

		/** Checks if the given mapping has a matching buff, either directly declared, or inherited from some its
		  * enclosing mapping. This is equivalent to `active(mapping.buffs)`.
		  */
		def active(mapping :Mapping) :Boolean = !mapping.buffs(this).isEmpty

		/** Equivalent to `!active(buff)`. */
		def inactive[T](buff :Buff[T]) :Boolean = buff(Get).isEmpty

		/** Equivalent to `!active(buffs)`. */
		def inactive[T](buffs :Iterable[Buff[T]]) :Boolean = get(buffs).isEmpty

		/** Equivalent to `!active(buffs)`. */
		def inactive[T](buffs :Buffs[T]) :Boolean = buffs(this).isEmpty

		/** Equivalent to `!active(buffs)`. */
		def inactive(mapping :Mapping) :Boolean = mapping.buffs(this).isEmpty

		/** Checks if the given buff [[net.noresttherein.oldsql.schema.Buff.is is]] of this type and is otherwise
		  * compatible with it and, if so, returns it. On the first impression this method does not give additional
		  * information, but subclasses will generally narrow the result type to a buff of the compatible kind
		  * and can even return a different (modified) buff instance than the argument. Additionally, this method
		  * serves as the single final verdict of belonging, used by all other pattern methods associated
		  * with this buff type.
		  */
		def get[T](buff :Buff[T]) :Opt[Buff[T]] = buff(Get)

		/** Checks if the given collection contains a matching buff, as defined by the overloaded `active` method
		  * for a single buff, and that no preceding buff
		  * is [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradictory]] to this type. If so,
		  * it returns the result of invoking `get(buff)` for the first matching buff (which will almost always be
		  * the buff itself).
		  */
		def get[T](buffs :Iterable[Buff[T]]) :Opt[Buff[T]] = Get(buffs)

		/** Checks if the given collection contains a matching buff, as defined by the overloaded `active` method
		  * for a single buff, and that no preceding buff
		  * is [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradictory]] to this type. If so,
		  * it returns the result of invoking `get(buff)` for the first matching buff (which will almost always be
		  * the buff itself). The implementation delegates the check to the `buffs` argument, but it will be consistent
		  * with the more generic `get(buffs :Iterable[Buff[T]])`.
		  */
		@inline def get[T](buffs :Buffs[T]) :Opt[Buff[T]] = buffs(this)

		/** Checks if the given mapping has a matching buff, either directly declared, or inherited from some its
		  * enclosing mapping. This is equivalent to `get(mapping.buffs)`.
		  */
		@inline def get[T](component :MappingOf[T]) :Opt[Buff[T]] = component.buffs(this)



//		/** A buff type which implies both of these buff types. This is different from
//		  * a [[net.noresttherein.oldsql.schema.Buff.ComboBuffType]] in that the latter is considered active only
//		  * by buffs created by itself or buff types implying it. Buff type conjunction, on the other hand,
//		  * ''iff'' all individual buff types are active - the buff could be made by an unrelated factory.
//		  */
//		def &&(other :BuffType) :BuffType = other match {
//			case combo :BuffTypeConjunction => new BuffTypeConjunction(this +: combo.buffs)
//			case _ => new BuffTypeConjunction(ArraySeq(this, other))
//		}

		/** A buff type implied by both of these buffs. Note that this is the reversal of implication direction
		  * from [[net.noresttherein.oldsql.schema.Buff.ComboBuffType ComboBuffType]].
		  */
		def ||(other :BuffType) :BuffType = other match {
			case or :BuffTypeDisjunction => new BuffTypeDisjunction(this +: or.buffs)
			case _ => new BuffTypeDisjunction(ArraySeq(this, other))
		}

		/** A buff implying this buff, but which cascades to subcomponents. */
		def ! :BuffType = if (cascades) this else BuffType(toString + "!")(this)

		def canEqual(that :Any) :Boolean = that.isInstanceOf[BuffType]

		override val toString :String = this.innerClassName

	}


	object BuffType {
		//todo: contradicted
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :BuffType =
			new NamedBuffType(name, cascades)(implied :_*)()

		private[Buff] class NamedBuffType(override val toString :String, override val cascades :Boolean = true)
		                                 (implied :BuffType*)(contradicted :BuffType*)
			extends ContradictoryBuffType(cascades)(implied :_*)(contradicted :_*)
		{
			override def canEqual(that :Any) = that.getClass == getClass

			override def equals(that :Any) = that match {
				case other :NamedBuffType => (other eq this) || (other canEqual this) && other.toString == toString
				case _ => false
			}

			override val hashCode = toString.hashCode
		}

	}




	/** `BuffType` interface parameterized with the buff type produced by this instance. Defines `unapply` methods
	  * which return a buff of this type if one exists.
	  * @see [[net.noresttherein.oldsql.schema.Buff.DedicatedBuffType]]
	  */
	trait TypedBuffType[+B[T] <: Buff[T]] extends BuffType { self =>

		/** Equivalent to [[net.noresttherein.oldsql.schema.Buff.BuffType.get get(buff)]], but narrows down the result
		  * to the compatible type `B[T]`. */
		def unapply[T](buff :Buff[T]) :Opt[B[T]]

		/** Equivalent to [[net.noresttherein.oldsql.schema.Buff.BuffType.get get(buffs)]], but narrows down the result
		  * to the compatible type `B[T]`. */
		def unapply[T](buffs :Iterable[Buff[T]]) :Opt[B[T]]

		/** Equivalent to [[net.noresttherein.oldsql.schema.Buff.BuffType.get get(buffs)]], but narrows down the result
		  * to the compatible type `B[T]`. */
		def unapply[T](buffs :Buffs[T]) :Opt[B[T]] = buffs(this)

		/** Equivalent to [[net.noresttherein.oldsql.schema.Buff.BuffType.get get(mapping)]], but narrows down the result
		  * to the compatible type `B[T]`. */
		def unapply[T](mapping :MappingOf[T]) :Opt[B[T]] = mapping.buffs(this)


		override def ! :TypedBuffType[B] =
			new NamedBuffType(toString + "!")(this)() with TypedBuffType[B] {
				override def unapply[T](buff :Buff[T]) = self.unapply(buff)
				override def unapply[T](buffs :Iterable[Buff[T]]) = self.unapply(buffs)
				override def unapply[T](buffs :Buffs[T]) = self.unapply(buffs)
				override def unapply[T](column :MappingOf[T]) = self.unapply(column)
			}
	}


	/** A scaffolding base trait for buff types which use a `Buff` subclass rather then the `Buff` class itself. */
	sealed trait DedicatedBuffType[+B[T] <: Buff[T]] extends TypedBuffType[B] {
		private object Get
			extends BuffExtractor[B] with PartialFunction[Buff[Any], B[Any]] with Serializable
		{
			private val cls = classTag.runtimeClass //relies on lazy initialization of singleton objects
			override def buffType :BuffType = DedicatedBuffType.this
			override def get[T](buff :Buff[T]) :Opt[B[T]] =
				if (cls.isInstance(buff)) Got(buff.asInstanceOf[B[T]]) else Lack

			def apply[T](buffs :Iterable[Buff[T]]) :Option[B[T]] = {//preserve the Option to avoid second boxing to Opt
				val res = buffs.collectFirst(this.asInstanceOf[PartialFunction[Buff[T], B[T]]])
				if (res.isDefined && res.get == null) None //we found a contradictory buff
				else res
			}

			override def isDefinedAt(buff :Buff[Any]) :Boolean =
				(buff.buffType contradicts DedicatedBuffType.this ) || buff(this).isDefined

			override def apply(buff :Buff[Any]) :B[Any] =
				if (buff.buffType contradicts DedicatedBuffType.this) null.asInstanceOf[B[Any]]
				else buff(this).orFail(buff.toString + " is not a " + DedicatedBuffType.this + " buff.")

			override def applyOrElse[A1 <: Buff[Any], B1 >: B[Any]](buff :A1, default :A1 => B1) :B1 =
				if (buff.buffType contradicts DedicatedBuffType.this) null.asInstanceOf[B[Any]]
				else buff(this) getOrElse default(buff)
		}
		protected[this] def classTag :ClassTag[_]

		override def get[T](buff :Buff[T]) :Opt[B[T]] = buff(Get)
		override def get[T](buffs :Iterable[Buff[T]]) :Opt[B[T]] = Get(buffs)
		override def get[T](buffs :Buffs[T]) :Opt[B[T]] = buffs(this)
		override def get[T](mapping :MappingOf[T]) :Opt[B[T]] = mapping.buffs(this)

		@inline final override def unapply[T](buff :Buff[T]) :Opt[B[T]] = get(buff)
		@inline final override def unapply[T](buffs :Iterable[Buff[T]]) :Opt[B[T]] = get(buffs)
		@inline final override def unapply[T](buffs :Buffs[T]) :Opt[B[T]] = buffs(this)
		@inline final override def unapply[T](mapping :MappingOf[T]) :Opt[B[T]] = mapping.buffs(this)
	}


	/** Mixing trait for [[net.noresttherein.oldsql.schema.Buff.BuffType buff types]] which marks them
	  * as [[net.noresttherein.oldsql.schema.Buff.BuffType.cascades cascading]].
	  */
	trait Cascading extends BuffType {
		override val cascades = true //val, not def, as its mixed in to traits which declare it as a val
	}

	/** Mixing trait for [[net.noresttherein.oldsql.schema.Buff.BuffType buff types]] which marks them
	  * as [[net.noresttherein.oldsql.schema.Buff.BuffType.cascades non cascading]].
	  */
	trait NonCascading extends BuffType {
		override val cascades = false //val, not def, as its mixed in to traits which declare it as a val
	}






	/** A `Buff` type which doesn't have any direct `Buff` instances, but is instead implied by other buff types. */
	trait AbstractBuffType extends BuffType {
		/** Returns `false`, although concrete buff types implying this type (and, by extension, their buffs) may
		  * be cascading. */
		override def cascades :Boolean = false

		/** Equivalent to `active(buff)`. */
		@inline final def unapply[T](buff :Buff[T]) :Boolean = active(buff)

		/** Equivalent to `active(buffs)` */
		@inline final def unapply[T](buffs :Iterable[Buff[T]]) :Boolean = active(buffs)

		/** Equivalent to `active(buffs)`. */
		@inline final def unapply[T](buffs :Buffs[T]) :Boolean = active(buffs)

		/** Equivalent to `active(mapping)`. */
		@inline final def unapply(mapping :Mapping) :Boolean = active(mapping)
	}

	/** A `Buff` type which doesn't have any `Buff` instances and won't match any of them.
	  * It is used in place of a `BuffType` when no appropriate concrete implementation exists to render relevant code
	  * inactive, automatically fulfilling a function normally performed by a `Option[BuffType]`.
	  */
	case object AbstractBuff extends AbstractBuffType {
		override def get[T](buff :Buff[T]) :Opt[Buff[T]] = Lack
		override def get[T](buffs :Iterable[Buff[T]]) :Opt[Buff[T]] = Lack
		override def get[T](buffs :Buffs[T]) :Opt[Buff[T]] = Lack
		override def get[T](mapping :MappingOf[T]) :Opt[Buff[T]] = Lack
	}

	/** A special `Buff` factory producing buffs which are ignored and not recognized by any other buff types. */
	case object NeutralBuff extends FlagBuffType with NonCascading



	/** A `Buff` type which implies other, more general buffs (has strictly more specific implications).
	  * Attaching a `Buff` of this type to a component is roughly equivalent to attaching buffs of the types
	  * listed in the constructor. It is analogous to multi-inheritance with the implied buff types as super types.
	  */
	class ComboBuffType(override val cascades :Boolean)(val implied :BuffType*) extends BuffType {
		def this(implied :BuffType*) = this(true)(implied :_*)

		protected[schema] override val knownImplied :Unique[BuffType] =
			(Unique.single[BuffType](this) /: implied)(_ :++ _.knownImplied)

		override def implies(other :BuffType) :Boolean =
			knownImplied.contains(other) || (other impliedBy this) || implied.exists(_.implies(other))

		override def impliedContradicts(other :BuffType) :Boolean = implied.exists(_.contradicts(other))
	}


	class ContradictoryBuffType(cascades :Boolean)(implied :BuffType*)(val contradicted :BuffType*)
		extends ComboBuffType(cascades)(implied :_*)
	{
		def this(implied :BuffType*)(contradicted :BuffType*) = this(true)(implied :_*)(contradicted :_*)

		override def impliedContradicts(other :BuffType) :Boolean =
			contradicted.contains(other) || implied.exists(_.contradicts(other))
	}


	private class BuffTypeDisjunction(val buffs :Seq[BuffType]) extends AbstractBuffType {
		override def impliedBy(other :BuffType) :Boolean =
			buffs.exists(other.implies)

		override def ||(other :BuffType) :BuffType = other match {
			case or :BuffTypeDisjunction => new BuffTypeDisjunction(buffs ++: or.buffs)
			case _ => new BuffTypeDisjunction(buffs :+ other)
		}

		override val toString = buffs.mkString("(", " || ", ")")
	}

	//this would break unwritten contract of BuffType as it would not match each buff individually, but a whole collection.
//	private class BuffTypeConjunction(val buffs :Seq[BuffType]) extends AbstractBuffType {
//		override def implies(other :BuffType) :Boolean = buffs.forall(_.implies(other))
//
//		override def &&(other :BuffType) :BuffType = other match {
//			case and :BuffTypeConjunction => new BuffTypeConjunction(buffs ++: and.buffs)
//			case _ => new BuffTypeConjunction(buffs :+ other)
//		}
//
//
//		override def active(buffs :Seq[Buff[_]]) = this.buffs.forall(_.active(buffs))
//
//		override def inactive(buffs :Seq[Buff[_]]) = this.buffs.exists(_.inactive(buffs))
//
////		override def get[T](buffs :Seq[Buff[T]]) = super.get(buffs)
//
//		override val toString = buffs.mkString(" && ")
//	}




	private class FlagBuff[T](override val buffType :FlagBuffType) extends Buff[T] {
		override def map[X](there :T => X) :FlagBuff[X] = this.asInstanceOf[FlagBuff[X]]

		override def cascade[X](there :T => X) :Option[FlagBuff[X]] =
			if (cascades) Some(this.asInstanceOf[FlagBuff[X]]) else None

		override def cascadeGuard[X](extractor :T =?> X) :Option[FlagBuff[X]] =
			if (cascades) Some(this.asInstanceOf[FlagBuff[X]]) else None

		override def bimap[X](there :T => X, back :X => T) :FlagBuff[X] = this.asInstanceOf[FlagBuff[X]]

		override def equals(that :Any) :Boolean = that match {
			case flag :FlagBuff[_] => buffType == flag.buffType
			case _ => false
		}
		override def hashCode :Int = buffType.hashCode

		override def toString :String = buffType.toString
	}


	/** A simple mapping buff which doesn't perform any function in the mapping itself, but serves
	  * instead as a switch checked at certain points to modify the behaviour of the annotated component,
	  * such as including an extra column in the update.
	  * There is an implicit conversion from `FlagBuffType` to `Buff[T]` for any type `T`, so in most cases
	  * it is possible to omit the type parameter of the factory `apply` method.
	  */
	trait FlagBuffType extends BuffType {
		/** Creates a new instance of this buff type for a column/component with subject type `T`.
		  * All instances created by this `BuffType` are equal.
		  */
		def apply[T] :Buff[T] = instance.asInstanceOf[Buff[T]]

		/** Creates a new instance of this buff type for a column/component with subject type `T`.
		  * This is the same as `apply[T]`, but the type parameter in most cases will be inferred and can be omitted.
		  */
		@inline def buff[T] :Buff[T] = apply[T]

		def +[T] :Buff[T] = !.buff
		def -[T] :Buff[T] = nonCascading.buff

		@inline final def apply[T](buff :Buff[T]) :Boolean = active(buff)
		@inline final def apply[T](buffs :Iterable[Buff[T]]) :Boolean = active(buffs)
		@inline final def apply[T](buffs :Buffs[T]) :Boolean = active(buffs)
		@inline final def apply(component :Mapping) :Boolean = active(component)

		@inline final def unappl[T](buff :Buff[T]) :Boolean = active(buff)
		@inline final def unapply[T](buffs :Iterable[Buff[T]]) :Boolean = active(buffs)
		@inline final def unapply[T](buffs :Buffs[T]) :Boolean = active(buffs)
		@inline final def unapply(component :Mapping) :Boolean = active(component)

		override val ! :FlagBuffType = FlagBuffType(toString + "!")(this)

		private[this] val instance = new FlagBuff[Nothing](this)
		private[this] val nonCascading = new NamedFlag(toString + "-")(this)() with NonCascading
	}


	object FlagBuffType {
		/** Creates a new, unique type of component flag of the given name. Two instances with the same name
		  * will compare equal regardless of other properties, in particular implied buffs.
		  */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :FlagBuffType =
			new NamedFlag(name, cascades)(implied :_*)() //todo: overrides with contradicted

		implicit def flag[T](buff :FlagBuffType) :Buff[T] = buff[T]

		private[Buff] class NamedFlag(name :String, cascades :Boolean = true)(implied :BuffType*)(contradicted :BuffType*)
			extends NamedBuffType(name, cascades)(implied :_*)(contradicted :_*) with FlagBuffType
	}


	/** A `FlagBuffType` which implies other flags (is equivalent to having them declared alongside it).
	  * Note that this can seem a reversal of the order of class extension: the implied buffs are ''more''
	  * specific than this buff. The difference from inheritance is that every buff instance of this type implies
	  * all the listed buffs, not just one of them. A more proper way of viewing it is multi generalization/inheritance.
	  */
	class ComboFlag(cascades :Boolean)(implied :BuffType*)
		extends ComboBuffType(cascades)(implied :_*) with FlagBuffType
	{
		def this(implied :BuffType*) = this(true)(implied :_*)
	}
	//todo: merge this class with ComboFlag when we can override constructors in Scala 3
	class ContradictoryFlag(cascades :Boolean)(implied :BuffType*)(contradicted :BuffType*)
		extends ContradictoryBuffType(cascades)(implied :_*)(contradicted :_*) with FlagBuffType
	{
		def this(implied :BuffType*)(contradicted :BuffType*) = this(true)(implied :_*)(contradicted :_*)
	}






	/** A `Buff` which carries a value of the subject type of the buffed component. These values are generally used
	  * instead of the the value carried by an entity or read from the database, but the exact handling depends on
	  * the buff type. They are typically created by a [[net.noresttherein.oldsql.schema.Buff.ValueBuffType ValueBuffType]]
	  * factory, but can also be of a different type (for example in case of buff types which support only certain
	  * component types).
	  */
	trait ValueBuff[T] extends Buff[T] {
		override def map[X](there :T => X) :ValueBuff[X]

		override def bimap[X](there :T => X, back :X => T) :ValueBuff[X] = map(there)

		def value :T
	}


	object ValueBuff {
		def unapply[T](buff :Buff[T]) :Opt[T] = buff match {
			case const :ValueBuff[T] => Got(const.value)
			case _ => Lack
		}
	}


	/** A `Buff` type which carries a value. These buff types are handled explicitly when creating and executing
	  * individual SQL statements. Which statements are affected (and how the value is used) depends on which
	  * of the `ValueBuffType` instances are implied by the implementing class:
	  *   - implying `ExtraSelect` means the annotated component is never included in the select clause and the
	  *     value provided by the buff is used instead;
	  *   - implying `ExtraFilter` means that every select and update statement must include the annotated component
	  *     in the 'where' clause to additionally filter the set of rows mapped by the application;
	  *   - implying `ExtraInsert` means that buff's value is used instead of any value carried by the entity
	  *     when inserting a new row into the database;
	  *   - implying `ExtraUpdate` means that buff's value is used instead of any value carried by the entity
	  *     when updating a row in the database.
	  *  As always, extending classes can imply several of the above at the same time.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraSelect$ ExtraSelect]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraFilter$ ExtraFilter]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraInsert$ ExtraInsert]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraUpdate$ ExtraUpdate]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraWrite$ ExtraWrite]]
	  */
	trait ValueBuffType extends DedicatedBuffType[ValueBuff] {
		protected[this] def classTag :ClassTag[_] = implicitly[ClassTag[ValueBuff[Any]]]

		/** Extracts the value carried by buffs belonging to the enclosing
		  * [[net.noresttherein.oldsql.schema.Buff.ValueBuffType ValueBuffType]].
		  */
		object Value extends BuffExtractor[Self] {
			override def buffType :BuffType = ValueBuffType.this
			override def get[T](buff :Buff[T]) :Opt[T] = buff match {
				case value :ValueBuff[T] => Got(value.value)
				case _ => Lack
			}

			@inline def unapply[T](buff :Buff[T]) :Opt[T] = buff(this) //todo: look at the byte code
			@inline def unapply[T](buffs :Iterable[Buff[T]]) :Opt[T] = ValueBuffType.this.get(buffs).map(_.value)
			@inline def unapply[T](buffs :Buffs[T]) :Opt[T] = buffs(ValueBuffType.this).map(_.value)
			@inline def unapply[T](mapping :MappingOf[T]) :Opt[T] = mapping.buffs(ValueBuffType.this).map(_.value)

			@inline def apply[T](buff :Buff[T]) :Opt[T] = buff(this)
			@inline def apply[T](buffs :Iterable[Buff[T]]) :Opt[T] = ValueBuffType.this.get(buffs).map(_.value)
			@inline def apply[T](buffs :Buffs[T]) :Opt[T] = buffs(ValueBuffType.this).map(_.value)
			@inline def apply[T](mapping :MappingOf[T]) :Opt[T] = mapping.buffs(ValueBuffType.this).map(_.value)
		}

		override val ! = ValueBuffType(toString + "!")(this)
	}


	object ValueBuffType {
		/** Creates a new abstract [[net.noresttherein.oldsql.schema.Buff.ValueBuffType ValueBuffType]]
		  * of the given name. The returned object cannot be used to create new `Buff`s, but it can still
		  * test positive for buffs of other `ValueBuffType` which imply it. This implementation is value-based:
		  * two instances of the same name will compare equal, regardless of the `cascades` flag.
		  */ //todo: contradicted
		def apply[T](name :String, cascades :Boolean = true)(implied :BuffType*) :ValueBuffType =
			new NamedValue(name, cascades)(implied :_*)()

		private[Buff] class NamedValue(name :String, cascades :Boolean = true)
		                              (implied :BuffType*)(contradicted :BuffType*)
			extends NamedBuffType(name, cascades)(implied :_*)(contradicted :_*) with ValueBuffType
	}


	/** A `Buff` type without any instances, not applied to any components. It is used in place of a `ValueBuffType`
	  * when no appropriate concrete implementation exists, to render relevant code inactive, automatically fulfilling
	  * a function which would be normally performed by an `Option[ValueBuffType]`.
	  */
	case object AbstractValueBuff extends ValueBuffType with NonCascading {
		override def get[T](buff :Buff[T]) :Opt[ValueBuff[T]] = Lack
		override def get[T](buffs :Iterable[Buff[T]]) :Opt[ValueBuff[T]] = Lack
		override def get[T](buffs :Buffs[T]) :Opt[ValueBuff[T]] = Lack
		override def get[T](mapping :MappingOf[T]) :Opt[ValueBuff[T]] = Lack
	}




	/** A mapping buff which carries a constant value to be used instead of the value present in the entity or
	  * the database, depending on the exact buff type. The default mapping and cascading behaviour is to
	  * eagerly map the carried value and produce a new `ConstantBuff` instance using the factory method
	  * `this.`[[net.noresttherein.oldsql.schema.Buff.ConstantBuffType.apply buffType(this.value)]]. One exception
	  * is the [[net.noresttherein.oldsql.schema.Buff.ConstantBuff.cascadeGuard cascadeGuard]] method when given
	  * a non-requisite [[net.noresttherein.oldsql.morsels.Extractor extractor]]. In that case
	  * the factory's method [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType.guard guard(f(this.value))]]
	  * is called, the default implementation of which catches all `NoSuchElementException`s thrown during value
	  * evaluation and choosing not to cascade instead of propagating the error. This of course can be overriden
	  * by both buff and buff type subclasses.
	  * evaluation of the new value, the buff will not cascade.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType]]
	  */
	class ConstantBuff[T](override val buffType :ConstantBuffType, override val value :T) extends ValueBuff[T] {

		override def map[X](there: T => X): ValueBuff[X] = buffType(there(value))

		override def cascade[X](there :T => X) :Option[ValueBuff[X]] =
			if (cascades) Some(buffType(there(value))) else None

		override def cascadeGuard[X](there :T =?> X) :Option[ValueBuff[X]] =
			if (cascades) there.requisite match {
				case Got(f) => Some(buffType(f(value)))
				case _ => buffType.guard(there(value))
			} else None

		override def equals(that: Any): Boolean = that match {
			case self :AnyRef if self eq this => true
			case o :ConstantBuff[_] if o.canEqual(this) => o.buffType == buffType && o.value == value
			case _ => false
		}
		override def hashCode :Int = value.hashCode

		override def toString :String = buffType.toString + "(" + value + ")"
	}


	object ConstantBuff {
		def unapply[T](buff :Buff[T]) :Opt[T] = buff match {
			case const :ConstantBuff[T] => Got(const.value)
			case _ => Lack
		}
	}


	/** A base trait for factories of `Buff[T]` instances wrapping values of `T`.
	  * By implying one of the predefined `ExtraSelect`, `ExtraFilter`, `ExtraInsert`, `ExtraUpdate` buff types,
	  * extending classes specify when (with which statement types) these values should be used.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  */
	trait ConstantBuffType extends ValueBuffType {
		/** Create a `ValueBuff` of this type, carrying the given value. */
		def apply[T](value :T) :ConstantBuff[T] = new ConstantBuff[T](this, value)

		/** A callback called from method [[net.noresttherein.oldsql.schema.Buff.ConstantBuff.cascadeGuard cascadeGuard]]
		  * invoked when passed a non-requisite extractor. It is responsible for determining if and how to cascade
		  * the mapped [[net.noresttherein.oldsql.schema.Buff.ConstantBuff ConstantBuff]]. Default implementation
		  * evaluates the value eagerly within a try-catch block and, if a [[NoSuchElementException]] is caught,
		  * does not cascade the buff instead of propagating the error. This can however be overriden by subclasses
		  * if desired.
		  */
		def guard[T](value: => T) :Option[ValueBuff[T]] =
			try { Some(this(value)) }
			catch { case _ :NoSuchElementException => None}

		override val ! :ConstantBuffType = ConstantBuffType(toString + "!")(this)
	}

	/** Factory for arbitrary [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType ConstantBuffType]],
	  * with name-based equality.
	  */
	object ConstantBuffType {
		/** Creates a new [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType ConstantBuffType]] of the given name.
		  * Two instances with the same name will be equal to each other.
		  */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :ConstantBuffType =
			new NamedConstant(name, cascades)(implied :_*)() //todo: overrides with contradicted

		private class NamedConstant(name :String, cascades :Boolean = true)
		                           (implied :BuffType*)(contradicted :BuffType*)
			extends NamedBuffType(name, cascades)(implied :_*)(contradicted :_*) with ConstantBuffType
	}



	/** A buff which reevaluates encapsulated expression each time its `value` method is called.
	  * This is similar to `ConstantBuff`, but the value is reevaluated with each call to `this.value`.
	  * One difference is that, because its value may be different for every access, no two separate instances
	  * are ever equal (while al `ConstantBuff`s of the same type and value are equal).
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType]]
	  */
	class GeneratedBuff[T](override val buffType :GeneratedBuffType, generator: => T) extends ValueBuff[T] {
		override def value :T = generator

		override def map[X](there :T => X) :ValueBuff[X] = buffType(there(generator))

		override def cascade[X](there :T => X) :Option[ValueBuff[X]] =
			if (cascades) Some(buffType(there(generator))) else None

		override def cascadeGuard[X](there :T =?> X) :Option[ValueBuff[X]] =
			if (cascades) there.requisite match {
				case Got(f) => cascade(f)
				case _ => buffType.guard(there(value))
			} else None

		override def toString :String = buffType.toString + "()"
	}


	object GeneratedBuff {
		def unapply[T](buff :Buff[T]) :Option[T] = buff match {
			case const :GeneratedBuff[T] => Some(const.value)
			case _ => None
		}
	}



	/** A variant of [[net.noresttherein.oldsql.schema.Buff.GeneratedBuff GeneratedBuff]] which will not be matched
	  * by its own buff type if the generator expression throws a [[NoSuchElementException]]. All mapping and cascading
	  * will produce also guarded buffs, even if the method would result in an exception being thrown by
	  * a standard `GeneratedBuff`.
	  */
	class GuardedBuff[T](factory :GeneratedBuffType, generator: => T)
		extends GeneratedBuff[T](factory, generator)
	{
		override def map[X](there :T => X) :ValueBuff[X] =
			buffType.guard(there(generator)) getOrElse new GuardedBuff(buffType, there(generator))

		override def cascade[X](there :T => X) :Option[ValueBuff[X]] =
			if (cascades) buffType.guard(there(value)) else None

		override def apply[V[_]](extractor :BuffExtractor[V]) :Opt[V[T]] =
			if (buffType implies extractor.buffType)
				try { extractor.get(this) }
				catch { case _ :NoSuchElementException => Lack }
			else Lack
	}



	/** A column/component `Buff` type which carries a by-name value. This value is used instead of the value
	  * present in the entity or the database in SQL statements. Which statements are affected depends on which
	  * of the predefined `ExtraSelect`, `ExtraFilter`, `ExtraInsert`, `ExtraUpdate` buffs is implied by the
	  * extending class. It is similar to `ConstantBuffType`, but the value provided by the buff is re-evaluated
	  * at each access.
	  * @see [[net.noresttherein.oldsql.schema.Buff.GeneratedBuff]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType]]
	  */
	trait GeneratedBuffType extends ValueBuffType {
		/** Create a `Buff` which will reevaluate the given expression each time its `value` method is accessed. */
		def apply[T](value: => T) :GeneratedBuff[T] = new GeneratedBuff(this, value)

		/** A callback from method [[net.noresttherein.oldsql.schema.Buff.GeneratedBuff.cascadeGuard cascadeGuard]]
		  * of [[net.noresttherein.oldsql.schema.Buff.GeneratedBuff GeneratedBuff]] invoked when passed a non-requisite
		  * extractor. Default implementation creates a `ValueBuff` which executes the given expression
		  * within a `try-catch` block when matching against a buff type. If value evaluation throws
		  * a [[NoSuchElementException]], the buff will be treated as if it was not of this type without propagating
		  * the error.
		  */
		def guard[T](value: => T) :Option[ValueBuff[T]] = Some(new GuardedBuff(this, value))


		override val ! :GeneratedBuffType = GeneratedBuffType(toString + "!")(this)
	}


	/** Factory for arbitrary [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType GeneratedBuffType]] instances,
	  * with name-based equality.
	  */
	object GeneratedBuffType {
		/** Create a new, unique [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType GeneratedBuffType]]
		  * of the given name. Two instances of the same name will equal each other.
		  */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :GeneratedBuffType =
			new NamedGenerator(name, cascades)(implied :_*)() //todo: override with contradicted

		private class NamedGenerator(override val toString :String, override val cascades :Boolean)
		                            (implied :BuffType*)(contradicted :BuffType*)
			extends ContradictoryBuffType(cascades)(implied :_*)(contradicted :_*) with GeneratedBuffType
		{
			override def canEqual(that :Any) = that.isInstanceOf[GeneratedBuffType]

			override def equals(that :Any) :Boolean = that match {
				case other :GeneratedBuffType => (other eq this) || (other canEqual this) && other.toString == toString
				case _ => false
			}

			override val hashCode = toString.hashCode
		}
	}




	/** A general purpose base class for buff types carrying values, combining the `ComboBuffType`
	  * with `GeneratedBuffType`, additionally providing a `ConstantBuffType` implying this instance as the
	  * `const` property.
	  */
	class ComboValueBuffType(cascades :Boolean)(implied :BuffType*)
		extends ComboBuffType(cascades)(implied :_*) with GeneratedBuffType
	{ outer =>
		def this(implied :BuffType*) = this(true)(implied :_*)

		protected def readResolve :ComboValueBuffType = this

		/** Factory of buffs wrapping constant values and are recognized as buffs of this buff type.
		  * The [[net.noresttherein.oldsql.schema.Buff.BuffType.cascades cascades]] flag is the same as in this type.
		  */
		val const :ConstantBuffType = new ComboBuffType(cascades)(this) with ConstantBuffType {
			override val toString :String = outer.toString + ".const"
			private def readResolve = outer.readResolve.const
		}

		override val ! :ComboValueBuffType = new ComboValueBuffType(this) { //todo: contradicted
			override val toString = outer.toString + "!" //todo: lazy, it would be better of as a value-based class
			protected override def readResolve = outer.readResolve.!
		}
	}

	//todo: remove these, replacing instead with overloaded constructors on base 'combo' classes
	class ContradictoryValueBuffType(cascades :Boolean)(implied :BuffType*)(val contradicted :BuffType*)
		extends ComboValueBuffType(cascades)(implied :_*)
	{
		def this(implied :BuffType*)(contradicted :BuffType*) =
			this(true)(implied :_*)(contradicted :_*)

		override def impliedContradicts(other :BuffType) :Boolean =
			contradicted.contains(other) || implied.exists(_.contradicts(other))
	}






	/** A column/component `Buff` carrying a function `T=>T` which is used to modify the value read or written
	  * to the database. Which operations are actually affected depends on the buff type. Audit buffs never cascade:
	  * presence of one on a component doesn't make any of its columns or subcomponents test positive.
	  */
	class AuditBuff[T](val buffType :AuditBuffType, val audit :T => T) extends Buff[T] {

		override def map[X](there :T => X) :AuditBuff[X] =
			throw new UnsupportedOperationException(toString + ".map: AuditBuff can't be mapped unidirectionally.")

		override def cascades :Boolean = false
		override def cascade[X](f :T => X) :Option[Nothing] = None
		override def cascadeGuard[X](extractor :T =?> X) :Option[Buff[X]] = None

		override def bimap[X](there :T => X, back :X => T) :AuditBuff[X] =
			new AuditBuff[X](buffType, back andThen audit andThen there)

		override def equals(that :Any) :Boolean = that match {
			case sub :AuditBuff[_] => (sub eq this) || sub.buffType == buffType && sub.audit == audit
			case _ => false
		}
		override def hashCode :Int = buffType.hashCode * 31 + audit.hashCode

		override def toString :String = buffType.toString + "(" + audit + ")"
	}


	/** A buff type which inspects and possibly modifies the value of the annotated column/component during
	  * a database operation. Exactly which operation(s) is/are affected is determined declaratively by
	  * implying one of the 'audit' types: `SelectAudit`, `FilterAudit`, `InsertAudit`, `UpdateAudit`.
	  * @see [[net.noresttherein.oldsql.schema.Buff.SelectAudit$ SelectAudit]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.FilterAudit$ FilterAudit]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.InsertAudit$ InsertAudit]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.UpdateAudit$ UpdateAudit]]
	  */
	trait AuditBuffType extends DedicatedBuffType[AuditBuff] with NonCascading { self =>
		protected[this] override  def classTag :ClassTag[_] = implicitly[ClassTag[AuditBuff[Any]]]

		protected def apply[T](map :T => T) :AuditBuff[T] = new AuditBuff(this, map)

		/** Composes the functions from all matching audit buffs ''declared'' by the given mapping. 
		  * If no matching buffs exist, the returned function will be identity. Note that this method 
		  * ignores any contradictory buffs present in the input.  
		  */
		def fold[T](mapping :MappingOf[T]) :T => T = fold(mapping.buffs)

		/** Composes the functions from all matching audit buffs from the first declaration in the given `Buffs` 
		  * collection. If no matching buffs exist, the returned function will be identity. Note that this method 
		  * ignores any contradictory buffs present in the input.  
		  */
		def fold[T](buffs :Buffs[T]) :T => T = fold(buffs.front)

		/** Composes the functions from all matching audit buffs in the collection. If no matching buffs exist, 
		  * the returned function will be identity. Note that this method ignores any contradictory buffs 
		  * present in the input.  
		  */
		def fold[T](buffs :Iterable[Buff[T]]) :T => T = {
			val audits = Audit(buffs)
			if (audits.isEmpty) identity[T]
			else audits.reduce(_ andThen _)
		}

		object Audit {
			/** If the given buff matches the enclosing 
			  * [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]], return the function it carries.
			  */
			@inline def apply[T](buff :Buff[T]) :Opt[T => T] = unapply(buff)

			/** Find all buffs matching the enclosing [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]]
			  * and return their functions. Note that this method ignores any contradictory buffs present in the input.   
			  */
			@inline def apply[T](buffs :Iterable[Buff[T]]) :Seq[T => T] = unapplySeq(buffs)

			/** Find all buffs matching the enclosing [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]]
			  * in the [[net.noresttherein.oldsql.schema.Buffs.declared declared]] prefix of the given collection
			  * and return their functions. Note that this method ignores any contradictory buffs present in the input.   
			  */
			@inline def apply[T](buffs :Buffs[T]) :Seq[T => T] = unapplySeq(buffs)

			/** Find all buffs declared by the given mapping which match the enclosing 
			  * [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]] and return their functions.
			  * Note that this method ignores any contradictory buffs present in the input.   
			  */
			@inline def apply[T](mapping :MappingOf[T]) :Seq[T => T] = unapplySeq(mapping)

			/** If the given buff matches the enclosing 
			  * [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]], return the function it carries.
			  */
			def unapply[T](buff :Buff[T]) :Opt[T => T] = buff match {
				case sub :AuditBuff[T] if sub is self => Got(sub.audit)
				case _ => Lack
			}

			/** Find all buffs matching the enclosing [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]]
			  * and return their functions. Note that this method ignores any contradictory buffs present in the input.   
			  */
			def unapplySeq[T](buffs :Iterable[Buff[T]]) :Seq[T => T] =
				buffs collect { case sub :AuditBuff[T] if sub is self => sub.audit } to List

			/** Find all buffs matching the enclosing [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]]
			  * in the [[net.noresttherein.oldsql.schema.Buffs.declared declared]] prefix of the given collection
			  * and return their functions. Note that this method ignores any contradictory buffs present in the input.   
			  */
			def unapplySeq[T](buffs :Buffs[T]) :Seq[T => T] = unapplySeq(buffs.front)

			/** Find all buffs declared by the given mapping which match the enclosing 
			  * [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]] and return their functions.
			  * Note that this method ignores any contradictory buffs present in the input.   
			  */
			def unapplySeq[T](component :MappingOf[T]) :Seq[T => T] = unapplySeq(component.buffs)
		}

		override val ! = AuditBuffType(toString + "!")(this)
	}


	object AuditBuffType {
		/** A new, unique [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]] of the given name. */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :AuditBuffType =
			new NamedAudit(name, cascades)(implied :_*)() //todo: contradicts

		private class NamedAudit(name :String, cascades :Boolean = true)(implied :BuffType*)(contradicted :BuffType*)
			extends NamedBuffType(name, cascades)(implied :_*)(contradicted :_*) with AuditBuffType
	}




	/** A `ManagedBuff` is a combination of a `ValueBuff` and `AuditBuff`, carrying both a by-name value
	  * and inspection/scanning function. When each of these is used depends, as always, on the associated buff type.
	  * This choice is made by implying one of the predefined 'audit' buff types: `SelectAudit`, `FilterAudit`,
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
			new ManagedBuff(buffType, there(init), back andThen audit andThen there)


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef => this eq self
			case _ => false //won't happen
		}

		override def hashCode :Int = System.identityHashCode(this)

		override def toString :String = buffType.toString + "(?, " + audit + ")"
	}



	/** A `ManagedBuffType` is a combination of a `ValueBuffType` and `AuditBuffType`, with the intention
	  * of using one for some types of SQL statements and the other for other types. Most typically this means
	  * using a generated value for insert and a modified value for update statements, but any combination
	  * is possible as long as the sets of affected statements are disjoint. This selection is made, as with
	  * the base types, by implying some subset of `ExtraSelect`, `ExtraFilter`, `ExtraInsert`, `ExtraUpdate`,
	  * `SelectAudit`, `FilterAudit`, `InsertAudit`, `UpdateAudit`.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ManagedBuff]]
	  */
	trait ManagedBuffType extends ValueBuffType with AuditBuffType with DedicatedBuffType[ManagedBuff] {
		protected[this] override def classTag :ClassTag[_] = implicitly[ClassTag[ManagedBuff[Any]]]

		protected override def apply[T](map :T => T) :AuditBuff[T] =
			throw new UnsupportedOperationException(
				toString + ".apply(" + map + "): ManagedBuffType requires an initial value; use the two-argument constructor."
			)

		def apply[T](init: => T, update :T => T) :ManagedBuff[T] = new ManagedBuff(this, init, update)

		override val ! = ManagedBuffType(toString + "!")(this)
	}


	object ManagedBuffType {
		/** A new, unique [[net.noresttherein.oldsql.schema.Buff.ManagedBuffType ManagedBuffType]] of the given name. */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :ManagedBuffType =
			new NamedManaged(name, cascades)(implied :_*)() //todo: contradicts

		private class NamedManaged(name :String, cascades :Boolean = true)(implied :BuffType*)(contradicted :BuffType*)
			extends NamedBuffType(name, cascades)(implied :_*)(contradicted :_*) with ManagedBuffType
	}



}

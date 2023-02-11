package net.noresttherein.oldsql.schema

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import net.noresttherein.oldsql.SerialVer
import net.noresttherein.oldsql.collection.{Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.generic.Self
import net.noresttherein.oldsql.schema.Buff.{BuffExtractor, BuffType}
import net.noresttherein.oldsql.schema.Buff.BuffType.NamedBuffType
import net.noresttherein.oldsql.schema.Buff.FlagBuffType.NamedFlag
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.Temporal
import net.noresttherein.oldsql.schema.ColumnMapping.ColumnAt
import net.noresttherein.oldsql.sql.{GroundColumn, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.mechanics.SQLConversion

//here be implicits
import net.noresttherein.oldsql.slang._






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
  * or 'buff class' to avoid ambiguity. Last but not least, there are various `Buff` subclasses and matching
  * broad `BuffType` categories such as [[net.noresttherein.oldsql.schema.Buff.ValueBuffType value buffs]] and
  * [[net.noresttherein.oldsql.schema.Buff.FlagBuffType flag buffs]] (or flags). To avoid adding to the confusion,
  * they are collectively (both buff and its buff type) are called ''buff kinds''. Buffs are simple objects unlikely
  * to use higher kinds of Scala types, so there should be no ambiguity in this area.
  *
  * See [[net.noresttherein.oldsql.schema.Buff.BuffType BuffType]] for more information.
  * @see [[net.noresttherein.oldsql.schema.Buff.FlagBuff FlagBuff]]
  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]
  * @see [[net.noresttherein.oldsql.schema.Buff.AuditBuff AuditBuff]]
  * @see [[net.noresttherein.oldsql.schema.Buff.SQLBuff  SQLBuff]]
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
	  */ //rename to derives
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
	  * with values it creates a new buff of the same type carrying the mapped value. This method differs from
	  * [[net.noresttherein.oldsql.schema.Buff.map map]] in that it works better for some buff types
	  * whose implementation with `map` might be potentially unsafe in principle.
	  * This method will not map an [[net.noresttherein.oldsql.schema.Buff.AuditBuff AuditBuff]],
	  * a [[net.noresttherein.oldsql.schema.Buff.ManagedBuff ManagedBuff]]
	  * or a proper [[net.noresttherein.oldsql.schema.Buff.CustomSQLBuff CustomSQLBuff]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.bimap bimap]]
	  */
	def optMap[X](there :T => X, back :X => Option[T]) :Buff[X] = map(there)

	/** Adapts this buff for a new component type. For flag buffs, this is an identity operation; for buffs
	  * with values it creates a new buff of the same type carrying the mapped value. This method differs from `map`
	  * in that it also works for instances of [[net.noresttherein.oldsql.schema.Buff.AuditBuff AuditBuff]],
	  * [[net.noresttherein.oldsql.schema.Buff.CustomSQLBuff CustomSQLBuff]],
	  * and [[net.noresttherein.oldsql.schema.Buff.SQLBuff SQLBuff]].
	  */
	def bimap[X](there :T => X, back :X => T) :Buff[X] = map(there)

	//todo: override it everywhere, it's better for extensibility
	def canEqual(that :Any) :Boolean = that.getClass == this.getClass

	override def toString :String = buffType.toString
}






/** Standard [[net.noresttherein.oldsql.schema.Buff.BuffType buff type]] definitions, serving as buff factories,
  * as well as a scope with various specific [[net.noresttherein.oldsql.schema.Buff Buff]] and `BuffType` subtypes,
  * both interfaces an factories.
  *
  * Most standard buff types defined here in a form of singleton objects can be grouped based on the context of
  * use of an annotated components. There are four 'primary' groups:
  *   1. `Select` - buffs whose effects apply only if a component is a part of a ''select'' clause of an SQL ''select'';
  *   1. `Insert` - buffs whose effects apply only if a component's value is being set by an ''insert'' DML
  *      (for example, `name` in `insert into hamsters(name) values ('boo')`).
  *   1. `Update` - buffs whose effects apply only if a component's value is being set by an ''update'' DML
  *      (for example, `strength` in `update rangers set strength = 19 where name = 'Minsc'`).
  *   1. `Filter` - buffs whose effects apply if component's value is being compared with another expression.
  *      Depending on the buff instance, it can furthermore require that said comparison is a filter condition
  *      present in a ''where'' clause of an SQL ''select'' against the table owning the component.
  *   1. `Write` - buffs implying corresponding `Insert` and `Update` buffs (for example, attaching to a column an
  *      [[net.noresttherein.oldsql.schema.Buff.OptionalWrite OptionalWrite]] buff is equivalent to attaching to it both
  *      [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]] and
  *      [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]] buffs.
  *
  * Their function is centered primarily on stating in which contexts an annotated column/component
  * should be, or should not be used, and providing default values.
  */
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
	  * [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]] and
	  * [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.Virtual Virtual]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.Partition Partition]]
	  */
	case object Ignored extends ComboFlag(ReadOnly, NoFilter, NoSelect)



	/** States that the value for this column/component is generated by the database on insert and should be returned
	  * by the insert statement.
	  * @see [[net.noresttherein.oldsql.schema.Buff.AutoUpdate]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.AutoGen]]
	  */  //this should conflict with NoSelect
	case object AutoInsert extends ComboFlag(NoInsert)

	/** States that the value for this column/component is updated by the database whenever a mapped row is updated,
	  * and can be retrieved using [[java.sql.Statement.getGeneratedKeys getGeneratedKeys]] method
	  * of a JDBC [[java.sql.Statement Statement]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.AutoInsert]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.AutoGen]]
	  */
	case object AutoUpdate extends ComboFlag(NoUpdate) //this is superfluous if we don't need the value to be returned

	/** States that the value for this column/component is generated by the database on insert and updated on each update,
	  * making it read only for the application. The new values should be possible to retrieve using
	  * [[java.sql.Statement.getGeneratedKeys getGeneratedKeys]] method of a JDBC [[java.sql.Statement Statement]].
	  * This is a convenient way of setting
	  * [[net.noresttherein.oldsql.schema.Buff.ReadOnly ReadOnly]] flag for a column/component and marking it as both
	  * [[net.noresttherein.oldsql.schema.Buff.AutoInsert AutoInsert]]
	  * and [[net.noresttherein.oldsql.schema.Buff.AutoUpdate AutoUpdate]].
	  */
	case object AutoGen extends ComboFlag(AutoInsert, AutoUpdate, ReadOnly)



	//Todo: document that it (currently) will work also for included null columns. Maybe it should be different from OptionalSelect
	/** A default value which will be returned by annotated component mapping if none is preset and none can
	  * be assembled from subcomponents. This can be used for example with nullable columns, but will also
	  * come into play if the column is missing from the ''select'' clause or in case of 'null' values
	  * coming from outer joins. This is a somewhat abstract buff type providing no actual way of creating new buffs;
	  * it is used during the assembly process and components are typically annotated with more concrete buff types
	  * implying it.
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalSelect]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.Default]]
	  */
	case object SelectDefault extends ComboValueBuffType

	//Fixme: the value will be used even if the column is included, but null. It probably shouldn't be so.
	/** A factory for buffs marking that a given column/component can be omitted from the select clause.
	  * It is still included by default and needs to be excluded explicitly. Created values carry a placeholder
	  * value to assign to the annotated component on assembly if the component is excluded.
	  * The column can be excluded from a [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] expression by
	  * providing it to the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.exclude exclude]]
	  * (or [[net.noresttherein.oldsql.sql.ast.LooseComponent.exclude exclude]])
	  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter alter]]
	  * (or [[net.noresttherein.oldsql.sql.ast.LooseComponent.alter alter]]
	  * method of the expression from the ''select'' clause for an owning entity/component,
	  * or on the level of [[net.noresttherein.oldsql.schema.Relation Relation]]:
	  * [[net.noresttherein.oldsql.schema.Relation.exclude exclude]] and
	  * [[net.noresttherein.oldsql.schema.Relation.apply(components* Relation(...)]].
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.SelectDefault SelectDefault]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]].
	  */
	case object OptionalSelect extends ContradictoryValueBuffType(SelectDefault)(NoSelect)

	/** A factory for buffs marking that a given column/component is omitted by default from the select clause
	  * and needs to be included explicitly. When not included, the value stored in the buff will be used
	  * as the value for the annotated component. It implies `OptionalSelect` and `NoSelectByDefault`.
	  * The column can be included either by listing it among ''fetch'' components of
	  * [[net.noresttherein.oldsql.hoard.Pile Pile]] methods,
	  * by [[net.noresttherein.oldsql.schema.Relation.apply Relation(...)]],
	  * [[net.noresttherein.oldsql.schema.Relation.include Relation.include]],
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.include include]] & [[net.noresttherein.oldsql.sql.ast.LooseComponent.include include]],
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter alter]] & [[net.noresttherein.oldsql.sql.ast.LooseComponent.alter alter]],
	  * or simply by explicitly including it in a [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] expression
	  * along the containing entity/component.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]].
	  */
	case object ExplicitSelect extends ComboValueBuffType(OptionalSelect, NoSelectByDefault)

	/** A buff marking a column as non-selectable, and providing the value for the annotated component.
	  * This can be used in particular for 'virtual' columns - components which take part in the mapping, but
	  * aren't present in the database at all. An SQL ''select'' for an owning entity will not use a literal column
	  * of the value in the associated buff; it will simply become available during the assembly
	  * as if it were selected.
	  * `SelectPreset` can coexist with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]]
	  * (and/or [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]),
	  * with the latter buffs taking precedence: if a column is omitted, this buff is ignored.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.SelectDefault SelectDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.ExtraSelect ExtraSelect]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraSelect]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.Virtual$]]
	  */
	case object SelectPreset extends ContradictoryValueBuffType(SelectDefault, NoSelect)(ExtraSelect)

	/** A buff marking an extra column to use in all SQL ''select'' statements for the owning entity,
	  * with the provided SQL expression. Each ''select'' clause containing an owning component, instead of
	  * simply listing the column as `column.`[[net.noresttherein.oldsql.schema.ColumnMapping.name name]],
	  * contains an expression `buff.`[[net.noresttherein.oldsql.schema.Buff.SQLBuff.expr expr]]` as column.name`.
	  * This is used to either simulate a non existing column, override an actual value from the database,
	  * or, for example, to introduce a discriminator column with meta information about the entity type, or mapping
	  * instance which should be used to assemble the values from a `ResultSet`. This buff can be attached
	  * only to columns, not multi column components.
	  * `ExtraSelect` can coexist with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]]/
	  * (and/or [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]),
	  * with the latter buffs taking precedence: if a column is omitted, this buff is ignored.
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.SelectPreset SelectPreset]].
	  */
	case object ExtraSelect extends ConstSQLBuffType with NonCascading with UnrelatedBuffType



	/** A buff marking that a given column or component can be omitted when comparing values of enclosing components
	  * in SQL. A lack of `OptionalFilter` on a column does not imply that it must be used in a ''where'' clause
	  * of a query against a table owning the component, as this buff
	  * does not [[net.noresttherein.oldsql.schema.Buff.BuffType.cascades cascade]] to subcomponents.
	  * This covers the case when a comparison in an SQL expression happens between whole subjects
	  * of a multi column mapping, rather than listing the columns individually. The annotated component
	  * is still included by default when comparing the owning mapping's subjects and needs to be excluded explicitly.
	  *
	  * The component can be excluded through the 'alter' methods of expressions for any owning component:
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.exclude]]
	  * (and [[net.noresttherein.oldsql.sql.ast.LooseComponent.exclude]]),
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter]]
	  * (and [[net.noresttherein.oldsql.sql.ast.LooseComponent.alter]]), thusly excluding the component
	  * from that particular expression. Alternatively, corresponding methods of
	  * [[net.noresttherein.oldsql.schema.Relation Relation]] can be used, which will exclude it
	  * (unless explicitly included) from all occurrences of any owning component in an SQL ''select''
	  * using the relation: [[net.noresttherein.oldsql.schema.Relation.exclude exclude]] and
	  * [[net.noresttherein.oldsql.schema.Relation.apply(components* Relation(...)]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]].
	  */ //todo: rename to OptionalCompare, etc.
	case object OptionalFilter extends ContradictoryFlag(false)()(NoFilter)

	/** A buff marking that a given column or component must be explicitly included when comparing values
	  * of enclosing components in SQL. For example, values of `BLOB/CLOB` and related columns cannot
	  * be compared in SQL. Thus, if a component containing such a component were to be compared, for example
	  * by specifying its value as a parameter used in a ''where'' clause, the column
	  * should receive an `ExplicitFilter` buff. Unlike implied
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]], this buff cascades to subcomponents:
	  * attaching it to a multi column component has the effect of attaching it to all its columns.
	  *
	  * The component can be included through the 'alter' methods of expressions for any owning component:
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.include]]
	  * (and [[net.noresttherein.oldsql.sql.ast.LooseComponent.include]]),
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter]]
	  * (and [[net.noresttherein.oldsql.sql.ast.LooseComponent.alter]]), thusly including the component
	  * as part of that particular expression. Alternatively, corresponding methods of
	  * [[net.noresttherein.oldsql.schema.Relation Relation]] can be used, which will include it
	  * (unless explicitly excluded) in all its occurrences in an SQL ''select'' using the relation:
	  * [[net.noresttherein.oldsql.schema.Relation.include include]] and
	  * [[net.noresttherein.oldsql.schema.Relation.apply(components* Relation(...)]].
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]].
	  */
	case object ExplicitFilter extends ComboFlag(OptionalFilter, NoFilterByDefault)

	/** A buff type stating that the annotated column/component must be included in every query against the table,
	  * using the value provided by the buff. It is used to artificially limit the subset of mapped entities.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.ExtraFilter ExtraFilter]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault, NoFilterByDefault]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraFilter]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.Virtual$]]
	  */ //consider: don't imply NoFilter
	case object FilterPreset extends ContradictoryValueBuffType(NoFilter)(NoFilterByDefault)

	/** A buff type stating that the annotated column must be included in every query against the table,
	  * using the SQL [[net.noresttherein.oldsql.schema.Buff.SQLBuff.expr expression]] provided by the buff.
	  * It is used to artificially limit the subset of mapped entities. This buff can be only used on columns,
	  * not multi-column components.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.FilterPreset FilterPreset]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.FilterPreset]]
	  */ //consider: don't imply NoFilter
	case object ExtraFilter extends ContradictorySQLBuffType(NoFilter)(NoFilterByDefault, FilterPreset)

	val WherePreset :ValueBuffType = FilterPreset
	val ExtraWhere  :SQLBuffType   = ExtraFilter



	/** Provides a default value which will be inserted into the table
	  * if the [[net.noresttherein.oldsql.schema.MappingExtract extract]] for the annotated component returns no value.
	  * This in particular includes columns not mapped to properties of the entity class.
	  *
	  * @see [[net.noresttherein.oldsql.schema.Buff.Default]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.WriteDefault]]
	  */
	case object InsertDefault extends ComboValueBuffType

	/** A buff specifying that a given column/component can be omitted from the insert statement.
	  * It is still included by default and needs to be excluded explicitly.
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]].
	  */
	case object OptionalInsert extends ContradictoryFlag()(NoInsert)

	/** A buff marking that a given column/component is not inserted by default into the underlying table
	  * and needs to be included explicitly.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]].
	  */
	case object ExplicitInsert extends ComboFlag(OptionalInsert, NoInsertByDefault)

	//We could make these three contradict NoUpdateByDefault, but it would work both ways and we don't want to
	//  automatically include an ExplicitInsert buff. What would be nice is canceling mechanics - unidirectional contradiction.
	//todo: we should introduce polymorphic handling for these three
	/** Marks a column/component as having its value initialized by the expression provided by the buff
	  * rather than the entity. Used particularly for 'created on' or 'created by' types of columns.
	  * `InsertPreset` can coexist with [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]]/
	  * (and/or [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]),
	  * with the latter buffs taking precedence: if a column is omitted, this buff is ignored.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.InsertDefault InsertDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.ExtraInsert ExtraInsert]],
	  * [[net.noresttherein.oldsql.schema.Buff.CustomInsert CustomInsert]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraInsert]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.WritePreset]]
	  */ //consider: drop NoInsert; maybe it should not be cascading?
	case object InsertPreset extends ContradictoryValueBuffType(InsertDefault, NoInsert)(ExtraInsert, CustomInsert) //fixme: currently unused!

	/** States that the value of an annotated column in new rows should be initialized using
	  * the SQL [[net.noresttherein.oldsql.schema.Buff.SQLBuff.expr expression]] provided by the associated buff,
	  * rather than a value retrieved from the inserted entity. This buff can be attached only to columns,
	  * not multi column components.
	  * `ExtraInsert` can coexist with [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]]/
	  * (and/or [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]),
	  * with the latter buffs taking precedence: if a column is omitted, this buff is ignored.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]],
	  * [[net.noresttherein.oldsql.schema.Buff.CustomInsert CustomInsert]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.InsertPreset InsertPreset]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.InsertPreset]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraWrite]]
	  */ //consider: drop NoInsert//can't imply CustomInsert as CustomInsert cannot be spelled at normal time, while this buff can.
	case object ExtraInsert extends ComboSQLBuffType(CustomInsert)

	/** A buff type specifying that an ''insert'' statement should not use, as normally, a statement parameter
	  * for the annotated column, of a value received
	  * from the [[net.noresttherein.oldsql.schema.ColumnExtract ColumnExtract]] for that column, but instead
	  * use an inlined SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] created from the extracted value
	  * by the functions included in buff [[net.noresttherein.oldsql.schema.Buff.CustomSQLBuff instances]].
	  * This buff can be attached only to columns, not multi column components.
	  * It can coexist with [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]]/
	  * (and/or [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]),
	  * with the latter buffs taking precedence: if a column is omitted, this buff is ignored.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.InsertPreset InsertPreset]],
	  * [[net.noresttherein.oldsql.schema.Buff.ExtraInsert ExtraInsert]].
	  */ //consider: drop NoInsert
	case object CustomInsert extends ComboCustomSQLBuffType(NoInsert)


	/** Provides a default value which will be included in the update for the mapped table
	  * if the [[net.noresttherein.oldsql.schema.MappingExtract extract]] for the annotated component returns no value.
	  *
	  * @see [[net.noresttherein.oldsql.schema.Buff.Default]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.WriteDefault]]
	  */
	case object UpdateDefault extends ComboValueBuffType

	/** A buff marking that a given column/component can be omitted from the update statement.
	  * It is still included by default and needs to be excluded explicitly.
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]].
	  */
	case object OptionalUpdate extends ContradictoryFlag()(NoUpdate)

	/** A buff marking that a given column/component is not included by default in the update statements
	  * and needs to be included explicitly.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]].
	  */
	case object ExplicitUpdate extends ComboFlag(OptionalUpdate, NoUpdateByDefault)

	//consider: not implying NoUpdate in these three
	/** Marks a column/component as being updated with the value of the expression provided by the buff
	  * rather than some property of the mapped entity. Useful for particularly for 'update timestamp' columns.
	  * `UpdatePreset` can coexist with [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]]/
	  * (and/or [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]]),
	  * with the latter buffs taking precedence: if a column is omitted, this buff is ignored.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.UpdateDefault UpdateDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.ExtraUpdate]],
	  * [[net.noresttherein.oldsql.schema.Buff.CustomUpdate CustomUpdate]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.WritePreset]]
	  */ //consider: maybe it should not be cascading?
	case object UpdatePreset extends ContradictoryValueBuffType(UpdateDefault, NoUpdate)(ExtraUpdate, CustomUpdate) //fixme: currently unused!

	/** States that the value of an annotated column on updated rows should be set
	  * to the SQL [[net.noresttherein.oldsql.schema.Buff.SQLBuff.expr expression]] provided by an associated buff,
	  * rather than a value retrieved from the inserted entity. This buff can be attached only to columns,
	  * not multi column components.
	  * `ExtraUpdate` can coexist with [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]]/
	  * (and/or [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]]),
	  * with the latter buffs taking precedence: if a column is omitted, this buff is ignored.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]],
	  * [[net.noresttherein.oldsql.schema.Buff.CustomUpdate CustomUpdate]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.UpdatePreset UpdatePreset]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraWrite]]
	  */
	//Now that ExtraUpdate implies CustomUpdate we must be wary not to throw an exception when a CustomSQLBuff is found
	// and we do not have a column value, if it happens to be a SQLBuff.
	case object ExtraUpdate extends ComboSQLBuffType(CustomUpdate)

	/** A buff type specifying that an ''update'' statement should not use, as normally, a statement parameter
	  * for the annotated column, of a value received
	  * from the [[net.noresttherein.oldsql.schema.ColumnExtract ColumnExtract]] for that column, but instead
	  * use an inlined SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] created from the extracted value
	  * by the function included in buff [[net.noresttherein.oldsql.schema.Buff.CustomSQLBuff instances]].
	  * This buff can be attached only to columns, not multi column components.
	  * It can coexist with [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]]/
	  * (and/or [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]]),
	  * with the latter buffs taking precedence: if a column is omitted, this buff is ignored.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]].
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.UpdatePreset UpdatePreset]],
	  * [[net.noresttherein.oldsql.schema.Buff.ExtraUpdate ExtraUpdate]].
	  */
	case object CustomUpdate extends ComboCustomSQLBuffType(NoUpdate)



	/** Provides a default value which will be used when inserting or updating the rows of the mapped table
	  * if the [[net.noresttherein.oldsql.schema.MappingExtract extract]] for the annotated component returns no value.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.InsertDefault InsertDefault]] and
	  * [[net.noresttherein.oldsql.schema.Buff.UpdateDefault UpdateDefault]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.Default]]
	  */
	case object WriteDefault extends ComboValueBuffType(InsertDefault, UpdateDefault)

	/** Marks a column/component as not mandatory for insert and update statements.
	  * Implies [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]] and
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]].
	  */
	case object OptionalWrite extends ComboFlag(OptionalInsert, OptionalUpdate)

	/** Marks a column/component as not included in the insert/update statements by default and needing to be included
	  * explicitly.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.ExplicitInsert ExplicitInsert]],
	  * [[net.noresttherein.oldsql.schema.Buff.ExplicitUpdate ExplicitUpdate]],
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalWrite OptionalWrite]].
	  */
	case object ExplicitWrite extends ComboFlag(ExplicitInsert, ExplicitUpdate, OptionalWrite)

	/** Marks a column/component as having its value set by this buff rather than a property of the entity
	  * at every write to the database. Implies [[net.noresttherein.oldsql.schema.Buff.ReadOnly ReadOnly]],
	  * [[net.noresttherein.oldsql.schema.Buff.WriteDefault WriteDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.InsertPreset InsertPreset]] and
	  * [[net.noresttherein.oldsql.schema.Buff.UpdatePreset UpdatePreset]].
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraWrite]]
	  */
	case object WritePreset
		extends ContradictoryValueBuffType(ReadOnly, WriteDefault, InsertPreset, UpdatePreset)(ExtraWrite, CustomWrite)

	/** States that the value of the annotated column must always be set to the SQL expression returned
	  * by an associated [[net.noresttherein.oldsql.schema.Buff.SQLBuff SQLBuff]], rather than extracted from
	  * the saved entity. This is a convenient way of declaring a column as
	  * both [[net.noresttherein.oldsql.schema.Buff.ExtraInsert ExtraInsert]]
	  * and [[net.noresttherein.oldsql.schema.Buff.ExtraUpdate ExtraUpdate]].
	  */
	case object ExtraWrite extends ContradictorySQLBuffType(ExtraInsert, ExtraUpdate)(CustomWrite)

	/** A buff type specifying that ''insert'' and ''update'' statements should use a custom
	  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] created by a function included
	  * in the dedicated [[net.noresttherein.oldsql.schema.Buff.CustomSQLBuff buff]]
	  * from the extracted value for the column in place of a standard statement parameter.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.CustomInsert CustomInsert]],
	  * [[net.noresttherein.oldsql.schema.Buff.CustomUpdate CustomUpdate]]
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.InsertPreset InsertPreset]],
	  * [[net.noresttherein.oldsql.schema.Buff.UpdatePreset UpdatePreset]]
	  */
	case object CustomWrite extends ComboCustomSQLBuffType(CustomInsert, CustomUpdate)


	/** Provides a default value which will be returned if the value for the component is not present in the ''select''
	  * as well as when inserting or updating the rows of the mapped table
	  * if the [[net.noresttherein.oldsql.schema.MappingExtract extract]] for the annotated component returns no value.
	  * This has the effect of not only mapping `null` columns to the given value, but also replacing any `null` values
	  * with the value extracted from the provided default.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.SelectDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.WriteDefault WriteDefault]].
	  */
	case object Default extends ComboValueBuffType(SelectDefault, WriteDefault)

	/** Signifies that a column/component can be excluded from all types of database operations.
	  * It is a shortcut for marking it with
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]],
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]] and
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalWrite OptionalWrite]].
	  */
	case object Optional extends ComboValueBuffType(OptionalSelect, OptionalFilter, OptionalWrite)

	/** Signifies that a column/component must be listed explicitly in order to be included in any database operation
	  * (it is not included by default).
	  * It is a shortcut for marking it with [[net.noresttherein.oldsql.schema.Buff.Optional Optional]],
	  * [[net.noresttherein.oldsql.schema.Buff.ExplicitSelect ExplicitSelect]],
	  * [[net.noresttherein.oldsql.schema.Buff.ExplicitFilter ExplicitFilter]] and
	  * [[net.noresttherein.oldsql.schema.Buff.ExplicitWrite ExplicitWrite]].
	  */
	case object Explicit extends ComboValueBuffType(Optional, ExplicitSelect, ExplicitFilter, ExplicitWrite)


	/** Marks a column or component which is not part of the mapped scala class, but is still part of the mapped
	  * entity from the relational point of view. All rows which are subject to mapping by the application have
	  * the value returned by the buff, essentially partitioning the table and limiting the application to a subset
	  * of its rows. It implies both `FilterPreset` `InsertPreset`, `NoUpdate`, meaning that all queries
	  * against the table will include the annotated column in the filter and all inserts will set its value
	  * based on this buff. The component is ignored during updates.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]],
	  * [[net.noresttherein.oldsql.schema.Buff.FilterPreset FilterPreset]],
	  * [[net.noresttherein.oldsql.schema.Buff.InsertPreset InsertPreset]].
	  */
	case object Partition extends ComboValueBuffType(NoSelect, NoUpdate, FilterPreset, InsertPreset)

	/** A buff marking a component or a column which does not exist in the database (or at least it not mapped
	  * and required) and will not be used as part of any SQL statements under any circumstances. It is still part
	  * of the mapping and, during assembly, the provided expression is used as its value. This can be useful
	  * during schema migrations, when a mapping might need to cover several versions of the schema, or if it is reused
	  * for several similar tables. Alternatively, it can be used for mappings whose components are already included
	  * as subcomponents of some other component of the same table.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.SelectPreset SelectPreset]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]],
	  * [[net.noresttherein.oldsql.schema.Buff.ReadOnly ReadOnly]].
	  */
	case object Virtual extends ComboValueBuffType(SelectPreset, NoFilter, ReadOnly)

//	case object Extra extends ComboSQLBuffType(ExtraSelect, ExtraFilter, ExtraWrite)



	/** Any value returned from the select (or assembled from such values) of a column/component annotated
	  * with this buff type must be mapped with the function included in the buff. This buff is independent
	  * from buffs specifying whether and when a component can be included in a select clause. */
	case object SelectAudit extends AuditBuffType with NonCascading with UnrelatedBuffType

	/** Any value compared in SQL with the value of the annotated column/component is first mapped with the function
	  * included in the buff. This is independent of whether the component can be included in the filter condition
	  * at all.*/
	case object FilterAudit extends AuditBuffType with NonCascading with UnrelatedBuffType

	/** All values of columns/components annotated with this buff type must be mapped with the function
	  * included in the buff before inserting the entity declaring it. This does not include update statements
	  * and is independent of any buffs specifying if the column/component can be inserted at all. */
	case object InsertAudit extends AuditBuffType with NonCascading with UnrelatedBuffType

	/** All values of columns/components annotated with this buff type must be mapped with the function
	  * included in the buff before updating the entity declaring it. This does not include insert statements
	  * and is independent of any buffs specifying if the column/component can be updated at all. */
	case object UpdateAudit extends AuditBuffType with NonCascading with UnrelatedBuffType

	/** All values of columns/components annotated with this buff type must be passed through the function
	  * provided by this buff before inserting or updating the entity declaring it. It is thus ideally suited
	  * for implementing validation, both on single columns and constraints spanning several columns (when applied
	  * to a component containing those columns). This is independent of any buffs specifying if the column/component
	  * can be inserted at all.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.UpdateAudit UpdateAudit]],
	  * [[net.noresttherein.oldsql.schema.Buff.InsertAudit InsertAudit]].
	  */
	case object WriteAudit extends ComboBuffType(false)(UpdateAudit, InsertAudit) with AuditBuffType

	/** Any read or written value `S` of a column/component annotated with this buff is first passed
	  * through a `S => S` function provided by the buff. This buff type thus makes a good extension point
	  * for consistency validation, both of data already in the database and that being written. If you wish to limit
	  * the check only to insert and update operations, use
	  * the [[net.noresttherein.oldsql.schema.Buff.WriteAudit$ WriteAudit]] buff instead (or those specifically dedicated
	  * to a single database operation type.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.SelectAudit SelectAudit]],
	  * [[net.noresttherein.oldsql.schema.Buff.FilterAudit FilterAudit]],
	  * [[net.noresttherein.oldsql.schema.Buff.WriteAudit WriteAudit]].
	  */
	case object Audit extends ComboBuffType(false)(SelectAudit, FilterAudit, WriteAudit) with AuditBuffType



	/** A flag signifying that mapped values can be null. */
	case object Nullable extends FlagBuffType with NonCascading with UnrelatedBuffType

	/** A flag signifying that mapped values cannot be null. This is the default behaviour, so this buff will be
	  * superfluous in most circumstances. It may be useful however to annul the `Nullable` buff.
	  *
	  * Contradicts: [[net.noresttherein.oldsql.schema.Buff.Nullable Nullable]].
	  */
	case object NotNull extends ContradictoryFlag(false)()(Nullable)



	/** Marks that a column/component ''must'' be included as a part of the ''where'' clause of any update statement. */
	case object UpdateMatch extends FlagBuffType with Cascading with UnrelatedBuffType

	/** A buff type marking that a column contains an application generated timestamp set once, when the row is inserted.
	  * It is an `InsertPreset` buff, meaning it will be automatically included in every insert and the value present
	  * in the inserted entity will be ignored.
	  * It should be of a type with a provided [[net.noresttherein.oldsql.schema.bits.Temporal Temporal]] type class.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.InsertPreset InsertPreset]].
	  */
	case object CreateTimestamp extends ComboBuffType(InsertPreset) with GeneratedBuffType {
		def apply[T :Temporal]() :GeneratedBuff[T] = apply(implicitly[Temporal[T]].now())
	}

	/** A buff type marking that a column contains an application generated timestamp set when the row is inserted
	  * and every time it is updated. It implies `ReadOnly`, `CreateTimestamp`, `InsertPreset` and `UpdatePreset`,
	  * meaning it will be automatically included in every write of the mapped entity to the database
	  * and the value present in the entity will be ignored.
	  * It should be of a type with a provided [[net.noresttherein.oldsql.schema.bits.Temporal Temporal]] type class.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.CreateTimestamp CreateTimestamp]],
	  * [[net.noresttherein.oldsql.schema.Buff.ReadOnly ReadOnly]],
	  * [[net.noresttherein.oldsql.schema.Buff.UpdatePreset, UpdatePreset]].
	  */
	case object UpdateTimestamp extends ComboBuffType(CreateTimestamp, UpdatePreset, ReadOnly) with GeneratedBuffType {
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
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.UpdateTimestamp UpdateTimestamp]],
	  * [[net.noresttherein.oldsql.schema.Buff.UpdateMatch UpdateMatch]].
	  */
	case object UpdateLock extends ComboBuffType(UpdateTimestamp, UpdateMatch) with GeneratedBuffType {
		def apply[T :Temporal]() :GeneratedBuff[T] = apply(implicitly[Temporal[T]].now())
	}

	/** Marks a column/component as carrying a 'version' stamp used to implement optimistic locking. The value
	  * carried by the entity is automatically increased/modified during the update, but the old value is used
	  * as part of the 'where' clause to prevent overwriting a concurrent update.
	  *
	  * Implies: [[net.noresttherein.oldsql.schema.Buff.WritePreset WritePreset]],
	  * [[net.noresttherein.oldsql.schema.Buff.UpdateAudit UpdateAudit]],
	  * [[net.noresttherein.oldsql.schema.Buff.UpdateMatch UpdateMatch]]
	  */
	case object VersionLock extends ComboBuffType(WritePreset, UpdateAudit, UpdateMatch) with ManagedBuffType {
		def apply[T]()(implicit int :Integral[T]) :ManagedBuff[T] =
			apply(int.fromInt(0), int.plus(_, int.fromInt(1)))

		def apply[T](init :T)(implicit int :Integral[T]) :ManagedBuff[T] =
			apply(init, int.plus(_, int.fromInt(1)))
	}


	/** A buff defining a parent table, intended for use on the table mapping level. Entities with this buff present
	  * can only be saved together with an entity mapped
	  * to `this.`[[net.noresttherein.oldsql.schema.Buff.Parent.table table]].
	  *
	  * Usually in this case, the dependent entities shouldn't be ever reassigned to another parent entity either,
	  * meaning a [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] buff should be attached
	  * to the foreign key column.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Parent$ Parent]]
	  */ //this buff is experimental
	@SerialVersionUID(SerialVer)
	class Parent[T] private (@volatile private var owner: () => RelVar[MappingAt]) extends Buff[T] {
//		def this(owner: => RelVar[MappingAt]) = this(() => owner)

		/** The parent table of the table with this buff attached. */
		lazy val table :RelVar[MappingAt] = {
			val t = owner()
			if (t == null)
				throw new NullPointerException(
					"Null parent table - likely an initialization error related to a dependency cycle."
				)
			owner = null
			t
		}
		override def buffType :BuffType = Parent

		override def map[X](f :T => X) :Buff[X] = {
			val init = owner
			if (init != null)
				new Parent(init)
			else {
				val t = table
				new Parent(() => t)
			}
		}

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Parent[_]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :Parent[_] if other canEqual this =>
				val (my, their) = (owner, other.owner)
				(my != null && my == their) || table == other.table
			case _ => false
		}
		override def hashCode :Int = table.hashCode

		override def toString :String = if (owner == null) "Parent(?)" else "Parent(" + table + ")"
	}


	/** A buff defining a parent table, intended for use on the table mapping level. Entities with this buff present
	  * can only be saved together with a parent entity.
	  *
	  * Usually in this case, the dependent entities shouldn't be ever reassigned to another parent entity either,
	  * meaning a [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] buff should be attached
	  * to the foreign key column.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Parent! Parent]]
	  */
	object Parent extends ReflectedBuffType[Parent] {
		protected[this] override val classTag :ClassTag[_] = ClassTag(classOf[Parent[Any]])

		def apply[T](owner: => RelVar[MappingAt]) :Parent[T] = new Parent(() => owner)

		/** A pattern matcher extracting the parent table from the first matching buff. */
		object table {
			def apply[T](buff :Buff[T]) :Opt[RelVar[MappingAt]] = buff match {
				case Parent(buff) => Got(buff.table)
				case _ => Lack
			}
			def apply[T](buffs :Buffs[T]) :Opt[RelVar[MappingAt]] = buffs match {
				case Parent(buff) => Got(buff.table)
				case _ => Lack
			}
			def apply[T](buffs :Iterable[Buff[T]]) :Opt[RelVar[MappingAt]] = buffs match {
				case Parent(buff) => Got(buff.table)
				case _ => Lack
			}
			@inline def unapply[T](buff :Buff[T]) :Opt[RelVar[MappingAt]] = apply(buff)
			@inline def unapply[T](buffs :Buffs[T]) :Opt[RelVar[MappingAt]] = apply(buffs)
			@inline def unapply[T](buffs :Iterable[Buff[T]]) :Opt[RelVar[MappingAt]] = apply(buffs)
		}

		override def cascades :Boolean = false
	}




	/** An unspecified matching pattern used to extract values of type `V[T]` from (some subclass of) buffs `Buff[T]`.
	  * This is a low-level callback invoked by buffs from their [[net.noresttherein.oldsql.schema.Buff.apply apply]]
	  * method to return the result of a match as a way of reversing the control over matching.
	  * The buff will check first if its [[net.noresttherein.oldsql.schema.Buff.buffType buffType]] implies
	  * the buff type of this extractor, leaving only the actual extraction of a value to this trait's
	  * [[net.noresttherein.oldsql.schema.Buff.BuffExtractor.get get]].
	  */
	trait BuffExtractor[+V[_]] {
		def buffType :BuffType
		def get[T](buff :Buff[T]) :Opt[V[T]]
	}



	/** A `BuffType` is to a [[net.noresttherein.oldsql.schema.Buff Buff]] what a Scala class is to a scala object:
	  * it implicitly defines both the information carried by the buffs of this type and indicates their purpose
	  * and how it should be interpreted. It can be used both as a factory and matcher,
	  * testing buff instances, collections, and mappings for the presence of matching buffs.
	  *
	  * Instances of this class can be in a transitive subtype relation analogous to that of Scala types: certain buffs
	  * created by some factories are recognized as 'belonging' also to other buff types. This is implemented through
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implies]] method. If a `BuffType` `A` implies
	  * a `BuffType` `B`, then `B.active(a)` and `a is B` will be true for every buff `a` created by the buff type `A`.
	  * If the actual `Buff` class, defining what information is carried by a buff, which is used by the type `A`
	  * is a subclass of the class of buffs produces by the type `B` (which should be the case for all such pairs),
	  * then also any extractor methods defined by the type `B` will pick up the data from buffs `A`.
	  * This means that, for example, any `BuffType` can safely imply any instance of `FlagBuffType` and any additional
	  * information carried by the buff being tested is ignored for the purpose of the check.
	  * On the other hand, creating a `FlagBuffType` which implies a `ValueBuffType` would be an application error:
	  * because created buffs do not conform to the expected type, they would not be matched by any patterns of the
	  * `ValueBuffType`, including those which do not access the actual value like
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.active active]] and
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.inactive inactive]]. In general, belonging to a buff type
	  * does not guarantee being matched by that buff type member extractors, with a notable example being mentioned
	  * subsequently: a property is thus defined by the matching pattern of the `BuffType`, rather than a presence
	  * of a particular `Buff` instance. For convenience of implementation, there are
	  * [[net.noresttherein.oldsql.schema.Buff.ComboBuffType ComboBuffType]],
	  * [[net.noresttherein.oldsql.schema.Buff.ComboFlag ComboFlag]]
	  * base classes which accept a list of implied buff types as constructor arguments, as well as others.
	  *
	  * In addition to this implication relation, buffs can have mutually exclusive meanings. This is defined by
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradicts]] method: if a buff of a type
	  * contradicted by this buff type is present on a buff list, it cancels all contradicted buffs which follow it,
	  * including those inherited from enclosing components. While this relation is not transitive, all buff types
	  * which are contradicted by a buff ''implied'' by this type are also automatically considered contradicted
	  * by this type. There is no formal definition or requirement as to when two buff types should contradict
	  * each other, but they should follow the policy of least surprise. It certainly is not exhaustive in terms
	  * of the logical implications modeled by these buffs: satisfying it is not a guarantee that there is no conflict
	  * of meanings between any two buffs in a collection. In general, however, situations where a buff overrides
	  * other buffs should generally be avoided if possible, as it can lead to confusing situations.
	  * No buff should ever test both as contradictory and belonging to a single buff type and behaviour
	  * of the framework in such cases is unspecified.
	  *
	  * Certain buff types are ''abstract'', meaning they can't be used to create new instances, but serve only
	  * as a grouping of more specific buff types which imply it. This is analogous to a common abstract base class
	  * in a object-oriented programming language. In particular, certain buff types serve only to indicate
	  * ''when'' the buff should be applied, without any information about its purpose. For example, creating
	  * a [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]],
	  * which implies the [[net.noresttherein.oldsql.schema.Buff.UpdateAudit$ UpdateAudit]], will map all updated values
	  * with the function provided by the buff before they are written to the database.
	  *
	  * This is an abstract base class and as such declares no `apply/unapply` methods, leaving it to subclasses
	  * which define them in terms of the information carried by their buffs. Whenever several matching buffs
	  * are present on a list or in a mapping, the buff type will always pick the first one.
	  * Similarly, if a subtype extracts ''all'' matching buffs from a list or a component, it will do so
	  * preserving their order. Repeated buffs of the same buff type on a mapping are allowed - they would be impossible
	  * to prevent for some common 'supertype' buff types of two buffs appearing in tandem.
	  *
	  * Note that buffs can be attached also to non-column components, unless explicitly noted in the documentation.
	  * The exact behavior in that case depends both on the buff type and the mapping class. In mappings extending
	  * provided base classes, such as [[net.noresttherein.oldsql.schema.bases.MappingFrame MappingFrame]]
	  * and [[net.noresttherein.oldsql.schema.bases.SimpleMapping SimpleMapping]], the subcomponents of a mapping inherit
	  * those buffs defined by enclosing mappings which have property
	  * [[net.noresttherein.oldsql.schema.Buff.cascades cascades]] set to true, with inherited buffs following the buffs
	  * declared locally. Not all buff kinds can be adopted though:
	  * there is no way for example to map an [[net.noresttherein.oldsql.schema.Buff.AuditBuff AuditBuff]]`[T]` with
	  * a single function `T => S` and other restrictions may also be in play.
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
	  * @see [[net.noresttherein.oldsql.schema.Buff.ConstSQLBuffType]]
	  */
	trait BuffType extends Serializable { factory =>
		/** If `true`, export versions of subcomponents of the annotated component should inherit this buff.
		  * Note that this flag applies only to buffs created by this instance, not any other buffs which
		  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff. The result is that,
		  * for standard `Mapping` implementations, this buff type will be automatically active on all columns
		  * and subcomponents of a component buffed by this instance, with the optional value obtained
		  * from the buff's value (if it is a [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]).
		  * Note however that a presence of contradicting buffs declared lower in the hierarchy will cancel
		  * an inherited buff, and components are in general allowed to control this process, so this behaviour
		  * cannot be always relied upon; any departure from it however will be documented in case of standard classes.
		  * Other buff types implying this buff can be cascading, and ultimately it is
		  * a [[net.noresttherein.oldsql.schema.Buff Buff]] property which is in effect.
		  */
		def cascades :Boolean

		/** Buffs for which this property is true may be applied only to columns, not multi-column components. */
		def isColumnOnly :Boolean = false //todo: add checks for this in MappingFrame and SimpleMapping at least

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
		  * A natural way of thinking about this relation is to imagine in which way class inheritance would work,
		  * and then reversing the direction.
		  *
		  * The relation must not contain cycles (larger then one element).
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

		/** Checks if this buff cancels another buff type. If true, presence of this buff on a mapping will
		  * essentially annul the other buff, providing this buff was declared lower in the hierarchy
		  * (i.e it is either defined on that mapping, or the component inclusion 'inheritance' path is shorter.
		  * This relation is not transitive, but holds also if any buff type
		  * [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implied]] by this buff contradicts the other buff,
		  * and for all buffs implying a contradicted buff.
		  * The relation is automatically symmetrical, as the default implementation performs a symmetrical check
		  * using delegate method [[net.noresttherein.oldsql.schema.Buff.BuffType.impliedContradicts impliedContradicts]],
		  * which should be overriden rather than this method. It must nevertheless possess a directionality
		  * in its definition such that `!(this impliedContradicts other) || !(other impliedContradicts this)`
		  * in order to prevent initialization errors due to cycles.
		  * @return `(this impliedContradicts other) || (other impliedContradicts this)`.
		  */
		def contradicts(other :BuffType) :Boolean = (this impliedContradicts other) || (other impliedContradicts this)

		/** Checks if buffs [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implied]] by this buff
		  * [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradict]] the given buff.
		  * The check is performed by following the declarations of implication and contradiction,
		  * which must not contain cycles. Default implementation always returns `false`. Other implementations,
		  * such as [[net.noresttherein.oldsql.schema.Buff.ContradictoryBuffType ContradictoryBuffType]], can possess
		  * a list of contradicted buffs and perform a test equivalent to `contradicted.exists(_ implies other)`.
		  * Note that in order to prevent infinite recursion/uninitialized access, only one buff of a pair can
		  * declare the other as contradictory, i.e. `impliedContradicts` between two buffs is always false
		  * in at least one direction. However, `contradicts` performs the check in both directions,
		  * guaranteeing symmetry.
		  */
		def impliedContradicts(other :BuffType) :Boolean = false

		protected[schema] val knownImplied      :Unique[BuffType]  = Unique.single(this)
		protected[schema] val knownContradicted :Unique[BuffType]  = Unique.single(this)
		protected[schema] def knownImpliedIsComplete      :Boolean = false
		protected[schema] def knownContradictedIsComplete :Boolean = false

		/** Match pattern for buffs of this type, checking them simply for existence, without extracting any values.
		  * Note that this pattern matches if and only if
		  * `this.`[[net.noresttherein.oldsql.schema.Buff.BuffType.get get(buff)]] returns a value and does not depend
		  * solely on the [[net.noresttherein.oldsql.schema.Buff.is is]] relation. All extractor methods delegate
		  * eventually to the `get(buff)` method of the enclosing buff type, with a possible intermediate role
		  * of [[net.noresttherein.oldsql.schema.Buffs.apply Buffs.apply]].
		  */
		case object Active {
			def columns(mapping :Mapping) :Unique[ColumnAt[mapping.Origin]] =
				mapping.columnsWith(BuffType.this)

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
			def columns(mapping :Mapping) :Unique[ColumnAt[mapping.Origin]] =
				mapping.columnsWithout(BuffType.this)

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
				else buff(this).orIllegal(buff.toString + " is not a " + BuffType.this + " buff.")

			override def applyOrElse[A1 <: Buff[_], B1 >: Buff[_]](buff :A1, default :A1 => B1) :B1 =
				if (buff.buffType contradicts BuffType.this) null
				else buff(this) getOrElse default(buff)
		}

		/** Checks if the buff [[net.noresttherein.oldsql.schema.Buff.is is]] of this type and is also otherwise
		  * compatible with it. This will return `true` ''iff'' [[net.noresttherein.oldsql.schema.Buff.BuffType.get get]]
		  * method for this buff would return a value.
		  */ //todo: rename to enabled
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

		/** Equivalent to `!active(buff)`. */ //todo: rename to disabled
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

		/** A buff implying this buff, but which cascades to subcomponents. */ //consider: moving it down to FlagBuffType and ValueBuffType
		def > :BuffType = if (cascades) this else BuffType(toString + ">")(this)

		def canEqual(that :Any) :Boolean = that.isInstanceOf[BuffType]

		/** Caches this buff's name. Overrides must happen with a primary constructor field in order
		  * for other properties, in particular [[net.noresttherein.oldsql.schema.Buff.BuffType.> >]],
		  * to use the actual name.
		  */
		override def toString :String = className
		private lazy val className = this.innerClassName
	}


	object BuffType {
		/** Creates a new, unique and abstract type of buffs which implies other buff type(s).
		  * It cannot be used to create new [[net.noresttherein.oldsql.schema.Buff Buff]] instances,
		  * but can be itself in turn implied by other buff types.
		  */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :BuffType =
			new NamedBuffType(name, cascades)(implied :_*)()

		/** Creates a new, unique and abstract type of buffs which 'cancels' other buff type(s) if present
		  * on the component. This buff will contradict any inherited buffs listed among arguments,
		  * even if an enclosing mapping is later altered, resulting in the component inheriting the contradicted buff
		  * retroactively.
		  */
		def contradict(name :String, cascades :Boolean = true)(contradicted :BuffType*) :BuffType =
			new NamedBuffType(name, cascades)()(contradicted :_*)

		@SerialVersionUID(SerialVer)
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
	  * @see [[net.noresttherein.oldsql.schema.Buff.ReflectedBuffType]]
	  */
	trait SpecificBuffType[+B[T] <: Buff[T]] extends BuffType { Self =>

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

		/** Pattern matcher extracting all buffs of the enclosing buff type in a collection. */
		object all {
			def unapply[T](buffs :Iterable[Buff[T]]) :Opt[Seq[B[T]]] =
				Got(buffs.collect { case Self(buff) => buff }.toSeq)
			def unapply[T](buffs :Buffs[T]) :Opt[Seq[B[T]]] = Got(buffs.all(Self))
			def unapply[T](mapping :MappingOf[T]) :Opt[Seq[B[T]]] = Got(mapping.buffs.all(Self))
		}

		override def > :SpecificBuffType[B] =
			new NamedBuffType(toString + ">")(this)() with SpecificBuffType[B] {
				override def unapply[T](buff :Buff[T]) = Self.unapply(buff)
				override def unapply[T](buffs :Iterable[Buff[T]]) = Self.unapply(buffs)
				override def unapply[T](buffs :Buffs[T]) = Self.unapply(buffs)
				override def unapply[T](column :MappingOf[T]) = Self.unapply(column)
			}
	}

	/** A scaffolding base trait for buff types which use a `Buff` subclass rather then the `Buff` class itself. */
	sealed trait ReflectedBuffType[+B[T] <: Buff[T]] extends SpecificBuffType[B] {
		private object Get
			extends BuffExtractor[B] with PartialFunction[Buff[Any], B[Any]] with Serializable
		{
			private val cls = classTag.runtimeClass //relies on lazy initialization of singleton objects
			override def buffType :BuffType = ReflectedBuffType.this
			override def get[T](buff :Buff[T]) :Opt[B[T]] =
				if (cls.isInstance(buff)) Got(buff.asInstanceOf[B[T]]) else Lack

			def apply[T](buffs :Iterable[Buff[T]]) :Option[B[T]] = {//preserve the Option to avoid second boxing to Opt
				val res = buffs.collectFirst(this.asInstanceOf[PartialFunction[Buff[T], B[T]]])
				if (res.isDefined && res.get == null) None //we found a contradictory buff
				else res
			}

			override def isDefinedAt(buff :Buff[Any]) :Boolean =
				(buff.buffType contradicts ReflectedBuffType.this ) || buff(this).isDefined

			override def apply(buff :Buff[Any]) :B[Any] =
				if (buff.buffType contradicts ReflectedBuffType.this) null.asInstanceOf[B[Any]]
				else buff(this).orIllegal(buff.toString + " is not a " + ReflectedBuffType.this + " buff.")

			override def applyOrElse[A1 <: Buff[Any], B1 >: B[Any]](buff :A1, default :A1 => B1) :B1 =
				if (buff.buffType contradicts ReflectedBuffType.this) null.asInstanceOf[B[Any]]
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
	  */ //todo: rename to Branching or StemBuff, or DerivingBuff, or Inheritable
	private[Buff] trait Cascading extends BuffType {
		override val cascades = true //val, not def, as its mixed in to traits which declare it as a val
	}

	/** Mixing trait for [[net.noresttherein.oldsql.schema.Buff.BuffType buff types]] which marks them
	  * as [[net.noresttherein.oldsql.schema.Buff.BuffType.cascades non cascading]].
	  */
	private[Buff] trait NonCascading extends BuffType {
		override val cascades = false //val, not def, as its mixed in to traits which declare it as a val
	}

	/** A Buff type which neither implies other buffs nor contradicts them unidirectionally
	  * (in terms of [[net.noresttherein.oldsql.schema.Buff.BuffType.impliedContradicts impliedContradicts]]).
	  * It can still be implied by and contradicted by other buff types.
	  */
	private[Buff] trait UnrelatedBuffType extends BuffType {
		//made final
		final override def implies(other :BuffType)   :Boolean = other impliedBy this
//		final override def impliedBy(other :BuffType) :Boolean = this == other
		final override def contradicts(other :BuffType) :Boolean = other impliedContradicts this
		final override def impliedContradicts(other :BuffType) :Boolean = this == other

		protected[schema] final override def knownImpliedIsComplete = true
		protected[schema] final override def knownContradictedIsComplete = true
	}




	/** A `Buff` type which doesn't have any dedicated `Buff` instances, but is instead implied by other buff types. */
	trait AbstractBuffType extends BuffType with UnrelatedBuffType {
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

	/** A `Buff` type which has no buff instances, implied or not. It can be used as a mixin for other buff types,
	  * in situations where a specific subtype of `BuffType` is required, but should be ignored.
	  * It is to a certain extent equivalent to an `Option[BuffType]`, but any using code does not need to check
	  * if the option is full/empty, and instead proceed on the path for the default case, relying on the fact
	  * that no buff instances exist and handling of the buff type is automatically skipped.
	  * @see [[net.noresttherein.oldsql.schema.Buff.InactiveBuff]]
	  */
	trait InactiveBuffType extends BuffType {
		override def cascades = false

		final override def get[T](buff :Buff[T]) :Opt[Nothing] = Lack
		final override def get[T](buffs :Iterable[Buff[T]]) :Opt[Nothing] = Lack
		final override def get[T](buffs :Buffs[T]) :Opt[Nothing] = Lack
		final override def get[T](mapping :MappingOf[T]) :Opt[Nothing] = Lack
	}

	/** A `Buff` type which doesn't have any `Buff` instances and won't match any of them.
	  * It is used in place of a `BuffType` when no appropriate concrete implementation exists to render relevant code
	  * inactive, automatically fulfilling a function normally performed by a `Option[BuffType]`.
	  */
	case object InactiveBuff extends AbstractBuffType with InactiveBuffType

//	/** A special `Buff` factory producing buffs which are ignored and not recognized by any other buff types. */
//	case object NeutralBuff extends FlagBuffType with NonCascading




	/** A `Buff` type which implies other, more general buffs (has strictly more specific implications).
	  * Attaching a `Buff` of this type to a component is roughly equivalent to attaching buffs of the types
	  * listed in the constructor. It is analogous to multi-inheritance with the implied buff types as super types.
	  */
	@SerialVersionUID(SerialVer)
	class ComboBuffType(override val cascades :Boolean)(val implied :BuffType*) extends BuffType {
		def this(implied :BuffType*) = this(true)(implied :_*)

		protected[schema] override val knownImplied :Unique[BuffType] =
			(Unique.single[BuffType](this) /: implied)(_ :++ _.knownImplied)

		protected[schema] override val knownContradicted :Unique[BuffType] =
			(Unique.empty[BuffType] /: implied)(_ :++ _.knownContradicted)

		protected[schema] override val knownImpliedIsComplete      :Boolean = implied.forall(_.knownImpliedIsComplete)
		protected[schema] override val knownContradictedIsComplete :Boolean =
			implied.forall(_.knownContradictedIsComplete)

		protected def contradicted :Seq[BuffType] = Nil

		final override def implies(other :BuffType) :Boolean =
			knownImplied.contains(other) || (other impliedBy this) ||
				!knownImpliedIsComplete && !knownContradicted.contains(other) && implied.exists(_.implies(other))

		final override def impliedContradicts(other :BuffType) :Boolean =
			knownContradicted.contains(other) ||
				!knownImplied.contains(other) && !knownContradictedIsComplete && implied.exists(_.contradicts(other))

	}

	/** A [[net.noresttherein.oldsql.schema.Buff.BuffType BuffType]] type which can both imply and contradict
	  * other [[net.noresttherein.oldsql.schema.Buff Buff]] type instances, as described in the documentation
	  * of the former.
	  */
	@SerialVersionUID(SerialVer)
	class ContradictoryBuffType(cascades :Boolean)(override val implied :BuffType*)(override val contradicted :BuffType*)
		extends ComboBuffType(cascades)(implied :_*)
	{
		def this(implied :BuffType*)(contradicted :BuffType*) = this(true)(implied :_*)(contradicted :_*)
	}

	@SerialVersionUID(SerialVer)
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




	trait BuffTemplate[T, +B[X] <: BuffTemplate[X, B]] extends Buff[T] {
//		def value :C[T]

		override def map[X](f :T => X) :B[X]
		override def bimap[X](there :T => X, back :X => T) :B[X] = map(there)
		override def optMap[X](there :T => X, back :X => Option[T]) :B[X] = map(there)

		override def cascade[X](f :T => X) :Option[B[X]] =
			if (cascades) Some(map(f)) else None

		override def cascadeGuard[X](extractor :T =?> X) :Option[B[X]] =
			if (cascades) extractor.requisite match {
				case Got(f) => cascade(f)
				case _ => None
			} else None
	}


	@SerialVersionUID(SerialVer)
	private class FlagBuff[T](override val buffType :FlagBuffType) extends BuffTemplate[T, FlagBuff] {
		override def map[X](there :T => X) :FlagBuff[X] = this.asInstanceOf[FlagBuff[X]]

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
		  * This is the same as `apply[T]`, but more convenient when the type parameter can be inferred and omitted.
		  */
		@inline def buff[T] :Buff[T] = apply[T]

		def +[T] :Buff[T] = >.buff
		def -[T] :Buff[T] = nonCascading.buff

		@inline final def apply[T](buff :Buff[T]) :Boolean = active(buff)
		@inline final def apply[T](buffs :Iterable[Buff[T]]) :Boolean = active(buffs)
		@inline final def apply[T](buffs :Buffs[T]) :Boolean = active(buffs)
		@inline final def apply(component :Mapping) :Boolean = active(component)

		@inline final def unapply[T](buff :Buff[T]) :Boolean = active(buff)
		@inline final def unapply[T](buffs :Iterable[Buff[T]]) :Boolean = active(buffs)
		@inline final def unapply[T](buffs :Buffs[T]) :Boolean = active(buffs)
		@inline final def unapply(component :Mapping) :Boolean = active(component)

		override lazy val > :FlagBuffType =
			if (cascades) this
			else new NamedFlag(toString + "+", true)(this)() {
				override lazy val nonCascading = FlagBuffType.this
			}

		private[Buff] lazy val nonCascading =
			if (!cascades) this
			else new NamedFlag(toString + "-", false)(this)() {
				override lazy val > = FlagBuffType.this
			}

		private[this] val instance = new FlagBuff[Nothing](this)
	}

	object FlagBuffType {
		/** Creates a new, unique type of component flag of the given name. Two instances with the same name
		  * will compare equal regardless of other properties, in particular implied buffs.
		  */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :FlagBuffType =
			new NamedFlag(name, cascades)(implied :_*)() //todo: overrides with contradicted

		/** Creates a new, unique type of component flag which 'switches off' another buff type if present
		  * on the component. This buff will contradict any inherited buffs listed among arguments,
		  * even if an enclosing mapping is later altered, resulting in the component inheriting the contradicted buff
		  * retroactively.
		  */
		def contradict(name :String, cascades :Boolean = true)(contradicted :BuffType*) :FlagBuffType =
			new NamedFlag(name, cascades)()(contradicted :_*)

		implicit def flag[T](buff :FlagBuffType) :Buff[T] = buff[T]

		@SerialVersionUID(SerialVer)
		private[Buff] class NamedFlag(name :String, cascades :Boolean = true)(implied :BuffType*)(contradicted :BuffType*)
			extends NamedBuffType(name, cascades)(implied :_*)(contradicted :_*) with FlagBuffType
	}


	/** A `FlagBuffType` which implies other flags (is equivalent to having them declared alongside it).
	  * Note that this can seem a reversal of the order of class extension: the implied buffs are ''more''
	  * specific than this buff. The difference from inheritance is that every buff instance of this type implies
	  * all the listed buffs, not just one of them. A more proper way of viewing it is multi generalization/inheritance.
	  */
	@SerialVersionUID(SerialVer)
	class ComboFlag(cascades :Boolean)(implied :BuffType*)
		extends ComboBuffType(cascades)(implied :_*) with FlagBuffType
	{
		def this(implied :BuffType*) = this(true)(implied :_*)
	}
	//todo: merge this class with ComboFlag when we can override constructors in Scala 3
	@SerialVersionUID(SerialVer)
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
	trait ValueBuff[T] extends BuffTemplate[T, ValueBuff] {
		/** The value of this buff. */
		def value :T

		/** The value of this buff, wrapped in a `NullValue`. It might be constant, or reevaluated with every access,
		  * as with `this.value`.
		  */
		def toNullValue :NullValue[T]

		protected[oldsql] def reassign(other :ValueBuffType) :ValueBuff[T] = other match {
			case const :ConstantBuffType => const(value)
			case gen :GeneratedBuffType => gen(value)
			case _ =>
				new ValueBuff[T] {
					override def buffType :ValueBuffType = other
					override def map[X](there :T => X) :ValueBuff[X] = ValueBuff.this.map(there).reassign(other)
					override def value :T = ValueBuff.this.value
					override def toNullValue = ValueBuff.this.toNullValue
				}
		}
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
	  *   - implying `SelectPreset` means the annotated component is never included in the select clause and the
	  *     value provided by the buff is used instead;
	  *   - implying `FilterPreset` means that every select and update statement must include the annotated component
	  *     in the 'where' clause to additionally filter the set of rows mapped by the application;
	  *   - implying `InsertPreset` means that buff's value is used instead of any value carried by the entity
	  *     when inserting a new row into the database;
	  *   - implying `UpdatePreset` means that buff's value is used instead of any value carried by the entity
	  *     when updating a row in the database.
	  *  As always, extending classes can imply several of the above at the same time.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraSelect$ SelectPreset]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraFilter$ FilterPreset]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraInsert$ InsertPreset]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraUpdate$ UpdatePreset]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExtraWrite$ WritePreset]]
	  */
	trait ValueBuffType extends ReflectedBuffType[ValueBuff] {
		protected[this] override def classTag :ClassTag[_] = implicitly[ClassTag[ValueBuff[Any]]]

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

		override val > :ValueBuffType = if (cascades) this else ValueBuffType(toString + ">")(this)
	}

	/** A factory of  */
	object ValueBuffType {
		/** Creates a new abstract [[net.noresttherein.oldsql.schema.Buff.ValueBuffType ValueBuffType]]
		  * of the given name. The returned object cannot be used to create new `Buff`s, but it can still
		  * test positive for buffs of other `ValueBuffType` which imply it. This implementation is value-based:
		  * two instances of the same name will compare equal, regardless of the `cascades` flag.
		  */
		def apply[T](name :String, cascades :Boolean = true)(implied :BuffType*) :ValueBuffType =
			new NamedValue(name, cascades)(implied :_*)()

		/** Creates a new abstract [[net.noresttherein.oldsql.schema.Buff.ValueBuffType ValueBuffType]]
		  * of the given name, which cancels another buff type(s) if present
		  * on the component. This buff will contradict any inherited buffs listed among arguments,
		  * even if an enclosing mapping is later altered, resulting in the component inheriting the contradicted buff
		  * retroactively. The returned object cannot be used to create new `Buff`s, but it can still
		  * test positive for buffs of other `ValueBuffType` which imply it. This implementation is value-based:
		  * two instances of the same name will compare equal, regardless of the `cascades` flag.
		  */
		def contradict(name :String, cascades :Boolean = true)(contradicted :BuffType*) :ValueBuffType =
			new NamedValue(name, cascades)()(contradicted :_*)

		@SerialVersionUID(SerialVer)
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
	  * is [[net.noresttherein.oldsql.schema.Buff.ConstantBuff.cascadeGuard cascadeGuard]] method when given
	  * a non-requisite [[net.noresttherein.oldsql.morsels.Extractor extractor]]. In that case
	  * the factory's method [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType.guard guard(f(this.value))]]
	  * is called, the default implementation of which catches all `NoSuchElementException`s thrown during value
	  * evaluation and choosing not to cascade instead of propagating the error. This of course can be overriden
	  * by both buff and buff type subclasses.
	  * evaluation of the new value, the buff will not cascade.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType]]
	  */
	@SerialVersionUID(SerialVer)
	class ConstantBuff[T](override val buffType :ConstantBuffType, override val value :T) extends ValueBuff[T] {
		override val toNullValue :NullValue[T] = NullValue(value)

		override def cascadeGuard[X](there :T =?> X) :Option[ValueBuff[X]] =
			if (cascades) there.requisite match {
				case Got(f) => Some(buffType(f(value)))
				case _ => buffType.guard(there(value))
			} else None

		override def map[X](there: T => X): ValueBuff[X] = buffType(there(value))

		override def equals(that: Any): Boolean = that match {
			case self :AnyRef if self eq this => true
			case o :ConstantBuff[_] if o.canEqual(this) => o.buffType == buffType && o.value == value
			case _ => false
		}
		override def hashCode :Int = buffType.hashCode * 31 + value.hashCode

		override def toString :String = buffType.toString + "(" + value + ")"
	}

	object ConstantBuff {
		def unapply[T](buff :Buff[T]) :Opt[T] = buff match {
			case const :ConstantBuff[T] => Got(const.value)
			case _ => Lack
		}
	}


	/** A base trait for factories of `Buff[T]` instances wrapping values of `T`.
	  * By implying one of the predefined `SelectPreset`, `FilterPreset`, `InsertPreset`, `UpdatePreset` buff types,
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
			catch { case _ :NoSuchElementException => None }

		override val > :ConstantBuffType = if (cascades) this else ConstantBuffType(toString + ">")(this)
	}

	/** Factory for arbitrary [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType ConstantBuffType]],
	  * with name-based equality.
	  */
	object ConstantBuffType {
		/** Creates a new [[net.noresttherein.oldsql.schema.Buff.ConstantBuffType ConstantBuffType]] of the given name.
		  * Two instances with the same name will be equal to each other.
		  */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :ConstantBuffType =
			new NamedConstant(name, cascades)(implied) //todo: overrides with contradicted

		@SerialVersionUID(SerialVer)
		private class NamedConstant(name :String, cascades :Boolean = true)
		                           (implied :Seq[BuffType] = Nil, contradicted :Seq[BuffType] = Nil)
			extends NamedBuffType(name, cascades)(implied :_*)(contradicted :_*) with ConstantBuffType
	}



	/** A buff which reevaluates encapsulated expression each time its `value` method is called.
	  * This is similar to `ConstantBuff`, but the value is reevaluated with each call to `this.value`.
	  * One difference is that, because its value may be different for every access, no two separate instances
	  * are ever equal (while al `ConstantBuff`s of the same type and value are equal).
	  * @see [[net.noresttherein.oldsql.schema.Buff.ValueBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType]]
	  */
	@SerialVersionUID(SerialVer)
	class GeneratedBuff[T](override val buffType :GeneratedBuffType, generator: => T) extends ValueBuff[T] {
		override def value :T = generator

		override val toNullValue :NullValue[T] = NullValue.eval(generator)

		override def cascadeGuard[X](there :T =?> X) :Option[ValueBuff[X]] =
			if (cascades) there.requisite match {
				case Got(f) => cascade(f)
				case _ => buffType.guard(there(value))
			} else None

		override def map[X](there :T => X) :ValueBuff[X] = buffType(there(generator))

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
	  * methods will also produce guarded buffs, even if the method would result in an exception being thrown by
	  * a standard `GeneratedBuff`.
	  */
	@SerialVersionUID(SerialVer)
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
	  * of the predefined `SelectPreset`, `FilterPreset`, `InsertPreset`, `UpdatePreset` buffs is implied by the
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

		override val > :GeneratedBuffType = if (cascades) this else GeneratedBuffType(toString + ">")(this)
	}

	/** Factory for arbitrary [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType GeneratedBuffType]] instances,
	  * with name-based equality.
	  */
	object GeneratedBuffType {
		/** Create a new, unique [[net.noresttherein.oldsql.schema.Buff.GeneratedBuffType GeneratedBuffType]]
		  * of the given name. Two instances of the same name will equal each other.
		  */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :GeneratedBuffType =
			new NamedGenerator(name, cascades)(implied) //todo: override with contradicted

		@SerialVersionUID(SerialVer)
		private class NamedGenerator(override val toString :String, override val cascades :Boolean)
		                            (implied :Seq[BuffType] = Nil, contradicted :Seq[BuffType] = Nil)
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
	@SerialVersionUID(SerialVer)
	class ComboValueBuffType(cascades :Boolean)(implied :BuffType*)
		extends ComboBuffType(cascades)(implied :_*) with GeneratedBuffType
	{ outer =>
		def this(implied :BuffType*) = this(true)(implied :_*)

		private def readResolve :ComboValueBuffType = this

		/** Factory of buffs wrapping constant values and are recognized as buffs of this buff type.
		  * The [[net.noresttherein.oldsql.schema.Buff.BuffType.cascades cascades]] flag is the same as in this type.
		  */
		val const :ConstantBuffType = new ComboBuffType(cascades)(this) with ConstantBuffType {
			override val toString :String = outer.toString + ".const"
			private def readResolve = outer.readResolve.const
		}

		override val > :ComboValueBuffType =
			if (cascades) this
			else new ComboValueBuffType(this) { //todo: contradicted
				override val toString = outer.toString + ">" //todo: lazy, it would be better of as a value-based class
				private def readResolve = outer.readResolve.>
			}
	}

	//todo: remove these, replacing instead with overloaded constructors on base 'combo' classes
	@SerialVersionUID(SerialVer)
	class ContradictoryValueBuffType(cascades :Boolean)(implied :BuffType*)(override val contradicted :BuffType*)
		extends ComboValueBuffType(cascades)(implied :_*)
	{
		def this(implied :BuffType*)(contradicted :BuffType*) =
			this(true)(implied :_*)(contradicted :_*)
	}




	/** A column/component `Buff` carrying a function `T=>T` which is used to modify the value read or written
	  * to the database. Which operations are actually affected depends on the buff type. Audit buffs never cascade:
	  * presence of one on a component doesn't make any of its columns or subcomponents test positive.
	  */
	trait AuditBuff[T] extends BuffTemplate[T, AuditBuff] {
		val audit :T => T
		override def cascades :Boolean = false
		override def cascade[X](f :T => X) :Option[Nothing] = None
		override def cascadeGuard[X](extractor :T =?> X) :Option[Nothing] = None

		override def map[X](there :T => X) :Nothing =
			throw new UnsupportedOperationException(toString + ".map: ManagedBuff can't be mapped unidirectionally.")

		override def optMap[X](there :T => X, back :X => Option[T]) :Nothing = map(there)
	}

	object AuditBuff {
		def apply[T](buffType :AuditBuffType, audit :T => T) :AuditBuff[T] =
			new Impl(buffType, audit)

		def unapply[T](buff :Buff[T]) :Opt[T => T] = buff match {
			case audit :AuditBuff[T] => Got(audit.audit)
			case _ => Lack
		}

		@SerialVersionUID(SerialVer)
		private class Impl[T](val buffType :AuditBuffType, val audit :T => T) extends AuditBuff[T] {

			override def bimap[X](there :T => X, back :X => T) :AuditBuff[X] =
				buffType[X](back andThen audit andThen there)

//			override def optMap[X](there :T => X, back :X => Option[T]) :AuditBuff[X] = map(there)
//			{
//				def get(x :X) = back(x) match {
//					case Some(value) => value
//					case _ =>
//						throw new IllegalArgumentException(
//							"Cannot audit " + x + " with an AuditBuff " + this + " mapped with optMap."
//						)
//				}
//				buffType[X](get _ andThen audit andThen there)
//			}

			override def equals(that :Any) :Boolean = that match {
				case sub :AuditBuff[_] if sub canEqual this => (sub eq this) || sub.buffType == buffType && sub.audit == audit
				case _ => false
			}
			override def hashCode :Int = buffType.hashCode * 31 + audit.hashCode

			override def toString :String = buffType.toString + "(" + audit + ")"
		}

	}


	private[Buff] trait AbstractAuditBuffType extends ReflectedBuffType[AuditBuff] { self =>

		/** Composes the functions from all matching audit buffs ''declared'' by the given mapping.
		  * If no matching buffs exist, the returned function will be identity. This method ignores
		  * any contradictory buffs present in the input.
		  */
		def fold[T](mapping :MappingOf[T]) :T => T = fold(mapping.buffs)

		/** Composes the functions from all matching audit buffs from the first declaration in the given `Buffs`
		  * collection. If no matching buffs exist, the returned function will be identity. This method ignores
		  * any contradictory buffs present in the input.
		  */
		def fold[T](buffs :Buffs[T]) :T => T = fold(buffs.front)

		/** Composes the functions from all matching audit buffs in the collection. If no matching buffs exist,
		  * the returned function will be identity. This method ignores any contradictory buffs present in the input.
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

		/** This buff does not cascade! */
		override def > :Nothing =
			throw new UnsupportedOperationException("Audit buff " + this + " cannot cascade.")
	}

	/** A buff type which inspects and possibly modifies the value of the annotated column/component during
	  * a database operation. Exactly which operation(s) is/are affected is determined declaratively by
	  * implying one of the 'audit' types: `SelectAudit`, `FilterAudit`, `InsertAudit`, `UpdateAudit`.
	  * @see [[net.noresttherein.oldsql.schema.Buff.SelectAudit$ SelectAudit]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.FilterAudit$ FilterAudit]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.InsertAudit$ InsertAudit]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.UpdateAudit$ UpdateAudit]]
	  */
	trait AuditBuffType extends AbstractAuditBuffType {
		protected[this] override def classTag :ClassTag[_] = implicitly[ClassTag[AuditBuff[Any]]]

		def apply[T](map :T => T) :AuditBuff[T] = AuditBuff(this, map)

//		override val > :AuditBuffType =
//			if (cascades) this else AuditBuffType(toString + ">")(this)
	}

	object AuditBuffType {
		/** A new, unique [[net.noresttherein.oldsql.schema.Buff.AuditBuffType AuditBuffType]] of the given name. */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :AuditBuffType =
			new NamedAudit(name, cascades)(implied :_*)() //todo: contradicts

		@SerialVersionUID(SerialVer)
		private class NamedAudit(name :String, cascades :Boolean = true)(implied :BuffType*)(contradicted :BuffType*)
			extends NamedBuffType(name, cascades)(implied :_*)(contradicted :_*) with AuditBuffType
	}




	/** A `ManagedBuff` is a combination of a `ValueBuff` and `AuditBuff`, carrying both a by-name value
	  * and inspection/scanning function. When each of these is used depends, as always, on the associated buff type.
	  * This choice is made by implying one of the predefined 'audit' buff types: `SelectAudit`, `FilterAudit`,
	  * `InsertAudit` and `UpdateAudit`.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ManagedBuffType]]
	  */
	@SerialVersionUID(SerialVer)
	class ManagedBuff[T](override val buffType :ManagedBuffType, generator: => T, override val audit :T => T)
		extends AuditBuff[T] with ValueBuff[T] with BuffTemplate[T, ManagedBuff]
	{
		override def value :T = generator

		override val toNullValue :NullValue[T] = NullValue.eval(generator)

//		override def optMap[X](there :T => X, back :X => Option[T]) :Nothing = map(there)
//		{
//			def get(x :X) = back(x) match {
//				case Some(value) => value
//				case _ => throw new IllegalArgumentException("Cannot map " + x + " with " + this + ".optMap")
//			}
//			buffType(there(generator), get _ andThen audit andThen there)
//		}

		override def bimap[X](there :T => X, back :X => T) :ManagedBuff[X] =
			buffType(there(generator), back andThen audit andThen there)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef => this eq self
			case _ => false //won't happen
		}
		override def hashCode :Int = System.identityHashCode(this)

		override def toString :String = buffType.toString + "(?, " + audit + ")"
	}


	/** A `ManagedBuffType` is a combination of `ValueBuffType` and `AuditBuffType`, with the intention
	  * of using one for some types of SQL statements and the other for other types. Most typically this means
	  * using a generated value for insert and a modified value for update statements, but any combination
	  * is possible as long as the sets of affected statements are disjoint. This selection is made, as with
	  * the base types, by implying some subset of `SelectPreset`, `FilterPreset`, `InsertPreset`, `UpdatePreset`,
	  * `SelectAudit`, `FilterAudit`, `InsertAudit`, `UpdateAudit`.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ManagedBuff]]
	  */
	trait ManagedBuffType extends ValueBuffType with AbstractAuditBuffType with ReflectedBuffType[ManagedBuff] {
		protected[this] override def classTag :ClassTag[_] = implicitly[ClassTag[ManagedBuff[Any]]]

		def apply[T](init: => T, update :T => T) :ManagedBuff[T] = new ManagedBuff(this, init, update)

//		override val > :ManagedBuffType =
//			if (cascades) this else ManagedBuffType(toString + ">")(this)
	}

	object ManagedBuffType {
		/** A new, unique [[net.noresttherein.oldsql.schema.Buff.ManagedBuffType ManagedBuffType]] of the given name. */
		def apply(name :String, cascades :Boolean = true)(implied :BuffType*) :ManagedBuffType =
			new NamedManaged(name, cascades)(implied :_*)() //todo: contradicts

		@SerialVersionUID(SerialVer)
		private class NamedManaged(name :String, cascades :Boolean = true)(implied :BuffType*)(contradicted :BuffType*)
			extends NamedBuffType(name, cascades)(implied :_*)(contradicted :_*) with ManagedBuffType
	}




	//todo: make compatible with CustomSQLBuff. Currently the types
	/** A `Buff` implementation similar to [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]], but carrying
	  * an SQL expression [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[RowProduct, GlobalScope, T]`
	  * instead of a value of T. As with `ValueBuff`, the expression may
	  * be a [[net.noresttherein.oldsql.schema.Buff.ConstSQLBuff constant]],
	  * or [[net.noresttherein.oldsql.schema.Buff.GeneratedSQLBuff evaluated]] each time the expression is accessed.
	  * @see [[net.noresttherein.oldsql.schema.Buff.SQLBuffType]]
	  */
	trait SQLBuff[T] extends BuffTemplate[T, SQLBuff] with CustomSQLBuff[T] {
		def expr :GroundColumn[T]
		override def provide :T => GroundColumn[T] = (_ :T) => expr
	}

	object SQLBuff {
		def unapply[T](buff :Buff[T]) :Opt[GroundColumn[T]] = buff match {
			case sql :SQLBuff[T] => Got(sql.expr)
			case _ => Lack
		}
	}


	/** A buff type which attaches to buffed columns an SQL expression with the same value type as the column.
	  * It cannot be used for non column components. Like [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]],
	  * the expression can be given by-name or by-value, depending on the implementation.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ConstSQLBuffType]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.GeneratedSQLBuffType]]
	  */
	trait SQLBuffType extends ReflectedBuffType[SQLBuff] with NonCascading {
		protected[this] override def classTag :ClassTag[_] = implicitly[ClassTag[SQLBuff[Any]]]

		override def isColumnOnly :Boolean = true

		object Expr extends BuffExtractor[SQLExpression.from[RowProduct]#rows[Single]#Column] {
			override def buffType :BuffType = SQLBuffType.this

			override def get[T](buff :Buff[T]) :Opt[GroundColumn[T]] = buff match {
				case sql :SQLBuff[T] => Got(sql.expr)
				case _ => Lack
			}

			@inline def unapply[T](buff :Buff[T]) :Opt[GroundColumn[T]] = buff(this) //todo: look at the byte code

			@inline def unapply[T](buffs :Iterable[Buff[T]]) :Opt[GroundColumn[T]] =
				SQLBuffType.this.get(buffs).map(_.expr)

			@inline def unapply[T](buffs :Buffs[T]) :Opt[GroundColumn[T]] =
				buffs(SQLBuffType.this).map(_.expr)

			@inline def unapply[T](mapping :MappingOf[T]) :Opt[GroundColumn[T]] =
				mapping.buffs(SQLBuffType.this).map(_.expr)

			@inline def apply[T](buff :Buff[T]) :Opt[GroundColumn[T]] = buff(this) //todo: look at the byte code

			@inline def apply[T](buffs :Iterable[Buff[T]]) :Opt[GroundColumn[T]] =
				SQLBuffType.this.get(buffs).map(_.expr)

			@inline def apply[T](buffs :Buffs[T]) :Opt[GroundColumn[T]] =
				buffs(SQLBuffType.this).map(_.expr)

			@inline def apply[T](mapping :MappingOf[T]) :Opt[GroundColumn[T]] =
				mapping.buffs(SQLBuffType.this).map(_.expr)
		}
	}




	/** A buff carrying a value of [[net.noresttherein.oldsql.sql.GroundColumn GroundColumn]]`[X]`. */
	class ConstSQLBuff[T](override val buffType :ConstSQLBuffType,
	                      override val expr :GroundColumn[T])
		extends SQLBuff[T]
	{
		override val provide :T => GroundColumn[T] = _ => expr

		override def map[X](there :T => X) :ConstSQLBuff[X] = buffType(expr.map(there))

		override def bimap[X](there :T => X, back :X => T) :ConstSQLBuff[X] =
			buffType(expr.to(SQLConversion(".bimap", there, back)))

		override def optMap[X](there :T => X, back :X => Option[T]) :ConstSQLBuff[X] =
			buffType(expr.to(SQLConversion.opt(".optMap", there, back(_ :X))))

		override def equals(that :Any) :Boolean = that match {
			case other :ConstSQLBuff[_] =>
				(this eq other) || (other canEqual this) && buffType == other.buffType && expr == other.expr
			case _ => false
		}
		override def hashCode :Int = buffType.hashCode * 31 + expr.hashCode

		override def toString :String = buffType.toString + "(" + expr + ")"
	}

	object ConstSQLBuff {
		def unapply[T](buff :Buff[T]) :Opt[GroundColumn[T]] = buff match {
			case sql :ConstSQLBuff[T] => Got(sql.expr)
			case _ => Lack
		}
	}



	/** A factory of buffs carrying literal SQL expressions with the value type equal to the column type.
	  * Typically used to replace the column in SQL with that literal, but the exact semantics depend
	  * on particular `BuffType` instance. These buffs have no effect if applied to non column components.
	  */
	trait ConstSQLBuffType extends SQLBuffType with ReflectedBuffType[ConstSQLBuff] {
		def apply[T](expr :GroundColumn[T]) :ConstSQLBuff[T] =
			new ConstSQLBuff(this, expr)
	}

	object ConstSQLBuffType {
		def apply(name :String)(implied :BuffType*) :ConstSQLBuffType =
			new NamedConstSQLBuffType(name)(implied)

		@SerialVersionUID(SerialVer)
		private class NamedConstSQLBuffType(name :String)(implied :Seq[BuffType] = Nil, contradicted :Seq[BuffType] = Nil)
			extends NamedBuffType(name, false)(implied :_*)(contradicted :_*) with ConstSQLBuffType
	}




	/** A buff carrying a by-name SQL expression of a value type equal to annotated column type.
	  * It cannot be used with non column components.
	  */
	class GeneratedSQLBuff[T](override val buffType :GeneratedSQLBuffType, sql: => GroundColumn[T])
		extends SQLBuff[T]
	{
		override def expr :GroundColumn[T] = sql
		override val provide :T => GroundColumn[T] = (_ :T) => sql

		override def map[X](there :T => X) :GeneratedSQLBuff[X] = buffType(expr.map(there))

		override def bimap[X](there :T => X, back :X => T) :GeneratedSQLBuff[X] =
			buffType(expr.to(SQLConversion(".bimap", there, back)))

		override def optMap[X](there :T => X, back :X => Option[T]) :GeneratedSQLBuff[X] =
			buffType(expr.to(SQLConversion.opt(".optMap", there, back(_ :X))))

		override def toString :String = buffType.toString + "()"
	}

	object GeneratedSQLBuff {
		def unapply[T](buff :Buff[T]) :Opt[GroundColumn[T]] = buff match {
			case sql :GeneratedSQLBuff[T] => Got(sql.expr)
			case _ => Lack
		}
	}


	/** An implementation of `SQLBuffType` which uses by-name SQL expressions. */
	trait GeneratedSQLBuffType extends SQLBuffType with ReflectedBuffType[GeneratedSQLBuff] {
		def apply[T](expr: => GroundColumn[T]) :GeneratedSQLBuff[T] =
			new GeneratedSQLBuff(this, expr)
	}

	object GeneratedSQLBuffType {
		def apply(name :String)(implied :BuffType*) :GeneratedSQLBuffType =
			new NamedGeneratedSQLBuffType(name)(implied)

		@SerialVersionUID(SerialVer)
		private class NamedGeneratedSQLBuffType(name :String)(implied :Seq[BuffType] = Nil, contradicted :Seq[BuffType] = Nil)
			extends NamedBuffType(name, false)(implied :_*)(contradicted :_*) with GeneratedSQLBuffType
	}




	@SerialVersionUID(SerialVer)
	class ComboSQLBuffType(implied :BuffType*)
		extends ComboBuffType(false)(implied :_*) with ConstSQLBuffType

	class ContradictorySQLBuffType(implied :BuffType*)(contradicted :BuffType*)
		extends ContradictoryBuffType(false)(implied :_*)(contradicted :_*) with ConstSQLBuffType




	//This would likely be better as a function Option[X] => ColumnSQL. Then SQLBuff could imply this buff.
	// We can't use Option[ColumnSQL] as the value of X is not known at the point of creating the SQL tree
	/** A `Buff` similar to [[net.noresttherein.oldsql.schema.Buff.SQLBuff SQLBuff]], but the SQL expression
	  * is produced based on the value for the annotated column, rather than being a constant or no argument
	  * expression. Like the former, it cannot be attached to non column components.
	  */
	trait CustomSQLBuff[T] extends BuffTemplate[T, CustomSQLBuff] {
		def provide :T => GroundColumn[T]
		def apply(value :T) :GroundColumn[T] = provide(value)
	}

	object CustomSQLBuff {
		def apply[T](buffType :CustomSQLBuffType)(value :T => GroundColumn[T]) :CustomSQLBuff[T] =
			new Impl(buffType, value)

		def unapply[T](buff :Buff[T]) :Opt[T => GroundColumn[T]] = buff match {
			case mapping :CustomSQLBuff[T] => Got(mapping.provide)
			case _ => Lack
		}

		@SerialVersionUID(SerialVer)
		private class Impl[T](override val buffType :CustomSQLBuffType, override val provide :T => GroundColumn[T])
			extends CustomSQLBuff[T]
		{
			override def cascades :Boolean = false
			override def cascade[X](f :T => X) :Option[Nothing] = None
			override def cascadeGuard[X](extractor :T =?> X) :Option[Nothing] = None

			override def map[X](there :T => X) :CustomSQLBuff[X] =
				throw new UnsupportedOperationException(
					"CustomSQLBuff " + this + " cannot be unidirectionally mapped to another type."
				)

			override def optMap[X](there :T => X, back :X => Option[T]) :CustomSQLBuff[X] = map(there)
//			{
//				def get(x :X) = back(x) match {
//					case Some(value) => value
//					case _ => throw new IllegalArgumentException("Cannot map value " + x + " with CustomSQLBuff " + this + ".optMap.")
//				}
//				buffType[X](get _ andThen provide andThen (SQLConversion.opt(".optMap", there, back(_))(_)))
//			}

			override def bimap[X](there :T => X, back :X => T) :CustomSQLBuff[X] =
				buffType[X](back andThen provide andThen (SQLConversion(".map", there, back)(_)))
		}
	}



	/** A buff type which attaches to columns functions `X => GroundColumn[X]`.
	  * It is used in a similar manner to [[net.noresttherein.oldsql.schema.Buff.SQLBuffType SQLBuffType]]s,
	  * but the details depend on the particular instance. Like with the former, buffing a non column component
	  * has no effect.
	  */
	trait CustomSQLBuffType extends ReflectedBuffType[CustomSQLBuff] {
		protected[this] override def classTag :ClassTag[_] = implicitly[ClassTag[CustomSQLBuff[Any]]]

		override def isColumnOnly = true

		def apply[T](value :T => GroundColumn[T]) :CustomSQLBuff[T] = CustomSQLBuff(this)(value)
	}

	object CustomSQLBuffType {
		def apply(name :String)(implied :BuffType*) :CustomSQLBuffType =
			new NamedCustomSQLBuffType(name, implied)

		@SerialVersionUID(SerialVer)
		private class NamedCustomSQLBuffType(name :String, implied :Seq[BuffType] = Nil, contradicted :Seq[BuffType] = Nil)
			extends NamedBuffType(name, false)(implied :_*)(contradicted :_*) with CustomSQLBuffType
	}


	@SerialVersionUID(SerialVer)
	class ComboCustomSQLBuffType(implied :BuffType*)
		extends ComboBuffType(false)(implied :_*) with CustomSQLBuffType

	@SerialVersionUID(SerialVer)
	class ContradictoryCustomSQLBuffType(implied :BuffType*)(contradicted :BuffType*)
		extends ContradictoryBuffType(false)(implied :_*)(contradicted :_*)

}

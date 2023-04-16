package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Opt}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{MismatchedKeyException, MissingKeyException}
import net.noresttherein.oldsql.haul.ColumnValues
import net.noresttherein.oldsql.model.{ComposedOf, Kin, KinFactory, PropertyPath}
import net.noresttherein.oldsql.model.ComposedOf.{ComposableFrom, DecomposableTo}
import net.noresttherein.oldsql.model.Kin.{Derived, Nonexistent, Present}
import net.noresttherein.oldsql.model.KinFactory.{BaseDerivedKinFactory, BaseKinFactory, DerivedKinFactory, HigherKindKinFactory, RequiredKinFactory}
import net.noresttherein.oldsql.morsels.generic.{=>:, Self}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Table
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}
import net.noresttherein.oldsql.slang.&&






/** `Kin` implementation providing a value for a specific mapping component as its 'key'.
  * Unlike the model-level `Kin` implementations, this one identifies the table and columns which should be compared
  * with no room for ambiguity. The drawbacks are:
  *   - possibly leaking schema details if exposed outside;
  *   - large serialized footprint which may potentially suck in a large portion of the schema
  *     together with the referenced table;
  *   - deserialization breaks equality, deserialized columns will not be accepted by the mapping for the original table;
  *   - Unsuitable for kin created in the domain logic layer of the application.
  * @author Marcin Mościcki
  */
trait TableKin[+T] extends Kin[T] {
	type Origin

	/** The referenced table (one containing values used to create a present version of this kin).
	  * Note: equality of tables is not preserved by serialization and name-based aliasing may be required before use.
	  */
	val table :Table[MappingAt]

	/** The value of the key: mapping from all columns which are part of the key to their value.
	  * Note: equality of columns is not preserved by serialization and deserialized columns will make illegal arguments
	  * for the mapping of the original table.
	  */
	val columns :NaturalMap[MappingAt[Origin]#Column, Self] //consider: using ColumnValues/ComponentValues

	/** Mapping from column names to column values. Used in `equals` in order to preserve equality over serialization. */
	def byName :Map[String, Any] = columns.view.map {
		case Assoc(column, value) => (table.export[Origin].export(column).name, value)
	}.toMap

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[TableKin[_]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case key :TableKin[_] if key canEqual this =>
			key.table == table && key.toOption == toOption && key.byName == byName
		case _ => false
	}

	override def hashCode :Int = (table.hashCode * 31 + byName.hashCode) * 31 + toOption.hashCode

	override def toString :String =
		columns.view.map {
			case Assoc(column, value) => column.name + "=" + value
		}.mkString(toOption match {
			case Some(x) => "Present|" + table + "|(" + x + "){"
			case _ if isMissing => "Missing|" + table + "|{"
			case _ if isNonexistent => "Nonexistent|" + table + "|{"
			case _ => "Absent|" + table + "|{"
		}, " && ", "}")

}






object TableKin {

	/** A [[net.noresttherein.oldsql.model.Kin.isNonexistent nonexistent]] kin instance, determined
	  * or known in advance to not identify any rows in the target table. It should really be used only
	  * for kin for individual entities, as ''to-many'' and ''to-at-most-one'' relationships are treated as empty
	  * collections/explicit value specific to missing values such as `None`.
	  * The column set of the returned instance is empty.
	  */
	def nonexistent(target :Table[MappingAt]) :TableKin[Nothing] = new TableKin[Nothing] with Nonexistent[Nothing] {
		override def properties[E, X, C](property :PropertyPath[E, X], as :ComposableFrom[C, X])
		                                (implicit decomposition :Nothing DecomposableTo E) :Kin[Nothing] = this

		override val table = target
		override val columns = NaturalMap.empty[MappingAt[Origin]#Column, Self]
	}

	/** A [[net.noresttherein.oldsql.model.Kin.isMissing missing]] instance, assumed to identify a value of `T`,
	  * composed of entities `E` mapped to rows in the referenced table. This may be because `T` is a collection type
	  * (and matching no rows is treated as an empty collection) or because the key being created is based on a database
	  * foreign key constraint. The given component and key are both decomposed into individual columns
	  * using the table's mapping - those [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]]
	  * - and stored in that form, so will ignore changes to the column set applied when the kin is resolved.
	  * @param target the referenced table.
	  * @param component the component of the mapping of `target` serving as the referenced key.
	  * @param key the value of the key of the referenced rows.
	  */
	def missing[K, E, T, O](target :Table[MappingOf[E]#Projection], component :TypedMapping[K, O], key :K)
	                       (implicit composition :T ComposableFrom E) :TableKin[T] =
		missing(target, columns(target.export[O], component, key))

	/** A [[net.noresttherein.oldsql.model.Kin.isMissing missing]] instance, assumed to identify a value of `T`
	  * in the database. This may be because `T` is a collection type (and matching no rows is treated as an empty
	  * collection) or because the key being created is based on a database foreign key constraint.
	  * @param target the referenced table.
	  * @param columnValues a map from columns of the table's mapping to their values in the referenced rows.
	  */
	def missing[E, T, O](target :Table[MappingOf[E]#Projection], columnValues :NaturalMap[MappingAt[O]#Column, Self])
	                    (implicit composition :T ComposableFrom E) :TableKin[T] =
		new DerivedTableKin[E, T](target) {
			override type Origin = O
			override val columns = columnValues
			override def toOption = None
			override def items = None
			override def isMissing = true
		}

	/** An [[net.noresttherein.oldsql.model.Kin.isAbsent absent]] instance for a value of `T` composed of all entities
	  * `E` mapped to rows with the matching key in the referenced table. The given component and key are both
	  * decomposed into individual columns using the table's mapping - those
	  * [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] - and stored in that form,
	  * so will ignore changes to the column set applied when the kin is resolved.
	  * @param target the referenced table.
	  * @param component the component of the mapping of `target` serving as the referenced key.
	  * @param key the value of the key of the referenced rows.
	  */
	def absent[K, E, T, O](target :Table[MappingOf[E]#Projection], component :TypedMapping[K, O], key :K)
	                      (implicit composition :T ComposableFrom E) :TableKin[T] =
		TableKin.absent(target, columns(target.export[O], component, key))

	/** An [[net.noresttherein.oldsql.model.Kin.isAbsent absent]] instance for a value of `T` composed of all
	  * entities `E` mapped to rows with matching column values in the referenced table.
	  * @param target the referenced table.
	  * @param columnValues a map from columns of the table's mapping to their values in the referenced rows.
	  */
	def absent[E, T, O](target :Table[MappingOf[E]#Projection], columnValues :NaturalMap[MappingAt[O]#Column, Self])
	                   (implicit composition :T ComposableFrom E) :TableKin[T] =
		new DerivedTableKin[E, T](target) {
			override type Origin = O
			override val columns = columnValues
			override def toOption = None
			override def items = None
		}


	/** A present kin with the given value referencing all rows in the specified table with matching column values.
	  * @param target the referenced table.
	  * @param columnValues a map from columns of the table's mapping to their values in the referenced rows, defining
	  *                     the key.
	  * @param value the value of the created kin.
	  */
	def present[E, T, O](target :Table[MappingOf[E]#Projection],
	                     columnValues :NaturalMap[MappingAt[O]#Column, Self], value :T)
	                    (implicit composition :T ComposedOf E) :TableKin[T] =
		TableKin(target, columnValues, Some(value))

	/** A present kin with the given value `T`, either assembled from all entities `E` mapped to rows in the referenced
	  * table with the given `key` value for the key `component`, or one which is intended to be disassembled to
	  * such rows when inserting/updating. The key is defined as
	  * the [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] column set of `component`,
	  * and the values for the columns are extracted from the `key` argument (which should be consistent with `value`).
	  * @param target the referenced table.
	  * @param component the component of the mapping of `target` serving as the referenced key.
	  * @param key the value of the key of the referenced rows.
	  * @param value the value of the created kin.
	  */
	def present[K, E, T, O](target :Table[MappingOf[E]#Projection], component :TypedMapping[K, O], key :K, value :T)
	                       (implicit composition :T ComposedOf E) :TableKin[T] =
		present[E, T, O](target, columns(target.export[O], component, key), value)

	/** A present kin with the given value `T`, either assembled from all entities `E` mapped to rows in the referenced
	  * table with the given `key` value for the key `component`, or one which is intended to be disassembled to
	  * such rows when inserting/updating. The key is defined as
	  * the [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] column set of `component`,
	  * and the values for the columns are extracted from the `values` argument. This is done by disassembling
	  * it into individual entities and using the extracts provided by mapping of the `target` table. The values
	  * returned by these extracts must be equal for all elements; more specifically, if the extract for the whole
	  * key `component` doesn't yield a value for any of the entities, or the key value is not unique, an exception will
	  * be thrown.
	  * @param target the referenced table.
	  * @param component the component of the mapping of `target` serving as the referenced key.
	  * @param value the value of the created kin.
	  */
	@throws[IllegalArgumentException]("If `value` is an empty collection/option.")
	@throws[MismatchedKeyException]("If two elements of `value` have a different value for `component`.")
	@throws[MissingKeyException]("If any of the elements in `value` doesn't have a value for `component`.")
	def present[K, E, T, O](target :Table[MappingOf[E]#Projection], component :TypedMapping[K, O], value :T)
	                       (implicit composition :T ComposedOf E) :TableKin[T] =
	{
		val keyExtract = target.row[O](component) //should be table.export[O] to be pedantic, but it doesn't have the same subject type
		val entities = composition.decomposer(value)
		if (entities.isEmpty)
			throw new IllegalArgumentException(
				s"Cannot create a TableKin($target.$component) for an empty value $value: no key to extract."
			)
		val key = keyExtract.requisite match {
			case Got(f) =>
				val res = f(entities.head)
				if (entities.view.map(f).forall(_ == res)) res
				else throw new MismatchedKeyException(
					s"Cannot create a TableKin($target.$component) for value $value: mismatched keys."
				)
			case _ =>
				val keys = entities.view.map(keyExtract.optional)
				if (keys.exists(_.isEmpty))
					throw new MissingKeyException(
						s"Cannot create a TableKin($target.$component) for value $value: missing keys."
					)
				val hd = keys.head
				if (keys.exists(_ != hd))
					throw new MismatchedKeyException(
						s"Cannot create a TableKin($target.$component) for value $value: mismatched keys."
					)
				hd.get
		}
		present[E, T, O](target, columns(target.export[O], component, key), value)
	}


	/** A present kin for a single entity `value` mapped to table `target`, identified by its value for `component`.
	  * The key is taken as all [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] columns
	  * of `component` for which `value` has a value (as defined by the extracts obtained from `target` table's mapping).
	  * This in particular means that if `value` doesn't have a value for `component`, then the kin will have
	  * an empty column set.
	  */
	def one[E, K, O](target :Table[MappingOf[E]#Projection], component :TypedMapping[K, O], value :E) :TableKin[E] =
		TableKin.present(target,
			(NaturalMap.newBuilder ++= component.filteredByDefault.view.map(target.export[O].export(_)).flatMap { c =>
				def assoc[X](column :TypedColumn[X, O]) =
					target[O].apply(column).opt(value) match {
						case Got(v) => Some(Assoc[MappingAt[O]#Column, Self, X](column, v))
						case _ => None
					}
				assoc(c)
			}).result(), value
		)


	/** A kin with the given value `T`, either assembled from all entities `E` mapped to rows in the referenced
	  * table with the given `key` value for the key `component`, or one which is intended to be disassembled to
	  * such rows when inserting/updating. The key is defined as
	  * the [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] column set of `component`,
	  * and the values for the columns are extracted from the `key` argument (which should be consistent with `value`).
	  * @param target the referenced table.
	  * @param component the component of the mapping of `target` serving as the referenced key.
	  * @param key the value of the key of the referenced rows.
	  * @param value the value of the created kin.
	  */
	def apply[K, E, T, O](target :Table[MappingOf[E]#Projection], component :TypedMapping[K, O], key :K, value :Option[T])
	                     (implicit composition :T ComposedOf E) :TableKin[T] =
		TableKin(target, columns(target.export[O], component, key), value)

	/** A kin with the given value referencing all rows in the specified table with matching column values.
	  * @param target the referenced table.
	  * @param columnValues a map from columns of the table's mapping to their values in the referenced rows, defining
	  *                     the key.
	  * @param value the value of the created kin.
	  */
	def apply[E, T, O](target :Table[MappingOf[E]#Projection],
	                   columnValues :NaturalMap[MappingAt[O]#Column, Self], value :Option[T])
	                  (implicit composition :T ComposedOf E) :TableKin[T] =
		new EagerTableKin(target, columnValues, value)


	/** A lazy kin with the given value `T`, either assembled from all entities `E` mapped to rows in the referenced
	  * table with the given `key` value for the key `component`, or one which is intended to be disassembled to
	  * such rows when inserting/updating. The key is defined as
	  * the [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] column set of `component`,
	  * and the values for the columns are extracted from the `key` argument (which should be consistent with `value`).
	  * @param target the referenced table.
	  * @param component the component of the mapping of `target` serving as the referenced key.
	  * @param key by-name expression initializing the value of the key of the referenced rows.
	  * @param value by-name expression initializing the value of the created kin.
	  */
	def delay[K, E, T, O]
	         (target :Table[MappingOf[E]#Projection], component :TypedMapping[K, O], key: => K, value: => Option[T])
	         (implicit composition :T ComposedOf E) :TableKin[T] =
		delay[E, T, O](target, columns(target.export[O], component, key), value)


	/** A lazy kin with the given value referencing all rows in the specified table with matching column values.
	  * @param target the referenced table.
	  * @param columnValues a by-name expression initializing the key - a map from columns of the table's mapping
	  *                     to their values in the referenced rows.
	  * @param value by-name expression initializing the value of the created kin.
	  */
	def delay[E, T, O](target :Table[MappingOf[E]#Projection],
	                   columnValues: => NaturalMap[MappingAt[O]#Column, Self], value: => Option[T])
	                  (implicit composition :T ComposedOf E) :TableKin[T] =
		new DelayedTableKin(target, columnValues, value)


	private def columns[K, O](mapping :MappingAt[O], component :TypedMapping[K, O], key :K)
			:NaturalMap[MappingAt[O]#Column, Self] =
	{
		(NaturalMap.newBuilder ++= component.filteredByDefault.view.map(mapping.export(_)).flatMap { c =>
			def assoc[X](column :TypedColumn[X, O]) =
				component(column).opt(key) match {
					case Got(v) => Some(Assoc[MappingAt[O]#Column, Self, X](column, v))
					case _ => None
				}
			assoc(c)
		}).result()
	}



	def unapply[T](kin :Kin[T]) :Opt[(Table[MappingAt], NaturalMap[MappingAt[_]#Column, Self])] = kin match {
		case key :TableKin[T @unchecked] => Got((key.table, key.columns))
		case _ => Lack
	}



	/** A factory of references to entities `E` from table `target` with a specific value of the provided `component`.
	  * Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin Kin]]`[E]`,
	  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `E`
	  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. Note that this factory is not simply
	  * a supertype of the 'real' factory [[net.noresttherein.oldsql.schema.bits.TableKin.required required]],
	  * as it is ''not required'' - its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]]
	  * method returns [[net.noresttherein.oldsql.model.Kin.Nonexistent Nonexistent]] kin instead of throwing
	  * a `NonexistentEntityException` as the latter one.
	  * @see [[net.noresttherein.oldsql.schema.bits.TableKin.required[K,E,O]* apply]]`(target, component)`.
	  */
	def factory[K, E, O](target :Table[MappingOf[E]#Projection], component :TypedMapping[K, O]) :KinFactory[K, E, E] =
		new TableKinFactory[K, E, E, O](target, component)

	/** A factory of references to entities `E` from table `target` with a specific value of the provided `component`.
	  * Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin.One One]]`[E]`,
	  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `E`
	  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. As this factory,
	  * and all obtained through it by adapting to other composite types, create only instances
	  * of [[net.noresttherein.oldsql.model.Kin.Derived Derived]] kin, it is automatically
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.isRequired required]]:
	  * its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]] method
	  * throws a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]].
	  * @see [[net.noresttherein.oldsql.schema.bits.TableKin.apply[K,E,O]* apply]]`(target, component)`.
	  */
	def required[K, E, O](target :Table[MappingOf[E]#Projection], component :TypedMapping[K, O])
			:DerivedKinFactory[K, E, E] =
		new DerivedTableKinFactory[K, E, E, O](target, component)




	private abstract class DerivedTableKin[E, +T](override val table :Table[MappingOf[E]#Projection])
	                                             (implicit override val composition :T ComposableFrom E)
		extends TableKin[T] with Derived[E, T]

	private class EagerTableKin[E, +T, O](table :Table[MappingOf[E]#Projection],
	                                      override val columns :NaturalMap[MappingAt[O]#Column, Self],
	                                      override val toOption :Option[T])
	                                     (implicit as :T ComposedOf E)
		extends DerivedTableKin[E, T](table)(as.composer)
	{
		override type Origin = O
		private[this] val decomposer = as.decomposer
		override def items = toOption.map(decomposer(_))
	}

	private class DelayedTableKin[E, +T, O](override val table :Table[MappingOf[E]#Projection],
	                                        columnValues: => NaturalMap[MappingAt[O]#Column, Self], value: => Option[T])
	                                       (implicit as :T ComposedOf E)
		extends DerivedTableKin[E, T](table)(as.composer)
	{
		override type Origin = O
		override lazy val columns = columnValues
		override lazy val toOption = value
		private[this] val decomposer = as.decomposer
		override def items = toOption.map(decomposer(_))
	}



	private abstract class AbstractTableKinFactory[K, E, T, O]
	                       (val table :Table[MappingOf[E]#Projection], val key :TypedMapping[K, O])
	                       (implicit override val result :T ComposedOf E)
		extends BaseKinFactory[K, E, T]
	{
		private val entity = table.export[O]
		private val keyExtract = table.row[O](key)
		private val columnNames = key.columns.map(entity.export(_).name).toSet

		override def delay(key :K, value : => Option[T]) :DerivedTableKin[E, T] =
			new DelayedTableKin(table, columns(entity, this.key, key), value)

		override def apply(key :K, value :Option[T]) :DerivedTableKin[E, T] =
			new EagerTableKin(table, columns(entity, this.key, key), value)

//		override def present(value :T) :DerivedTableKin[E, T] =

		override def missing(key :K) :DerivedTableKin[E, T] =
			new EagerTableKin(table, columns(entity, this.key, key), None)

		override def keyFrom(item :E) :Opt[K] = keyExtract.opt(item)

		override def keyOf(kin :Kin[T]) :Opt[K] = kin match {
			case TableKin(t, _) && Present(vals) => result.decomposer(vals).flatMap(keyExtract.optional).toList match {
				case Nil => Lack
				case _ if t != table => Lack
				case keys @ h::t if t.exists(_ != h) =>
					throw new MismatchedKeyException(s"Differing $this key values in $kin: $keys.")
				case h::_ => Got(h)
			}
			case TableKin(t, columns) if t == table => //or we could cache TableKin.byName in a lazy val
				val byName = columns.view.map { case Assoc(col, value) => (col.name, value) }.toMap
				val values = ColumnValues(entity.refine)(new (MappingAt[O]#Column =>: Self) {
					override def apply[X](x :TypedColumn[X, O]) =
						byName.getOrElse(x.name, null).asInstanceOf[X]
				})
				values.get(key)
			case _ => Lack
		}


		override def notRequired :KinFactory[K, E, T] =
			if (isRequired) new TableKinFactory[K, E, T, O](table, key) else this

		override def equivalencyToken :Any = (table, key)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case keys :AbstractTableKinFactory[_, _, _, _] if keys canEqual this =>
				keys.table == table && keys.columnNames == columnNames && keys.result == result
			case _ => false
		}

		override def hashCode :Int = (table.hashCode * 31 + columnNames.hashCode) * 31 + result.hashCode

		override def toString :String =
			result.toString + (if (isRequired) "" else "?") + "(" + table + "/" + key + ")"

	}



	private class TableKinFactory[K, E, T, O](override val table :Table[MappingOf[E]#Projection],
	                                          override val key :TypedMapping[K, O])
	                                         (implicit override val result :T ComposedOf E)
		extends AbstractTableKinFactory[K, E, T, O](table, key) with HigherKindKinFactory[K, E, T, TableKin]
	{
		private val none = TableKin.nonexistent(table)

		override def present(value :T) :TableKin[T] = TableKin.present(table, key, value)
		override def present(key :K, value :T) :TableKin[T] = TableKin.present(table, this.key, key, value)
		override def absent(key :K) :TableKin[T] = TableKin.absent(table, this.key, key)
		override def nonexistent :TableKin[T] = none
		override def isRequired :Boolean = false

		override def required :HigherKindKinFactory[K, E, T, TableKin] =
			if (isRequired) this
			else new TableKinFactory(table, key) with RequiredKinFactory[K, E, T, TableKin[T]]

		override def as[Y](implicit composition :Y ComposedOf E) = new TableKinFactory(table, key)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TableKinFactory[_, _, _, _]]
	}



	private class DerivedTableKinFactory[K, E, T, O]
	                                    (override val table :Table[MappingOf[E]#Projection],
	                                     override val key :TypedMapping[K, O])
	                                    (implicit override val result :T ComposedOf E)
		extends AbstractTableKinFactory[K, E, T, O](table, key) with BaseDerivedKinFactory[K, E, T]
	{
		override def apply(key :K, value :Option[T]) :DerivedTableKin[E, T] =
			new EagerTableKin(table, columns(table.export[O], this.key, key), value)

		override def present(value :T) :Derived[E, T] = keyFor(value) match {
			case Got(k) => apply(k, Some(value))
			case _ => Derived.present(value)
		}

		override def as[Y](implicit composition :Y ComposedOf E) = new DerivedTableKinFactory(table, key)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[DerivedTableKinFactory[_, _, _, _]]
	}

}

package com.adpilot.cortb.clientapi.golem

import scala.collection.mutable.ListBuffer



/**  A collection of columns definition describing a schema for a last or query.
  *  Allows statically typed access to the database - when a row is defined to be of this RowDef's type,
  *  columns defined under it can be used to access the row data in a type safe manner.
  *  We say that a column belongs to RowDef x, or is under x, if it is either defined by x's class,
  *  or is under a {@link #AggregateSQL group} defined by x's class.
  */
trait RowDef {

	type Column[T] = ColumnDef[T]

	/** A logical grouping of columns, which are part of the owning RowDef's relation, but are accessible from here
	  * for the reasons of readability and reuse. For each column defined in this class, a qualified version is added
	  * to owning RowDef's column list. For example, a subclass PersonName can define columns 'first' and 'last',
	  * which will be translated into prefix+'first' and prefix+'last' columns in the underlying row, but are visible
	  * in the code only as 'x.name.first' and 'x.name.last'
	  *
	  * @param prefix optional prefix which will be added to all columns defined in this group.
	  */
	class Group(protected[golem] val prefix :String="") extends RowDef {

		protected[RowDef] def parent[T](column :Column[T]) :RowDef.this.Column[T] =
			RowDef.this.newColumn(column.name)(column.columnType)

		override protected[golem] def declare[T](col: Column[T]): Column[T] = {
			parent.declare(parent(col))
			super.declare(col)
		}

		private[this] val parent = RowDef.this
	}


	/** Evidence that column of type C belongs to this instance - either directly or through a chain of nested Groups.
	  * Represents a 'path' starting from this RowDef, where each element is a group nested inside the previous one.
	  * Exists if and only if C =:= this.Column or a group G<:AggregateSQL exists such that G#ColumnPath[T, C] exists.
	  */
	class ColumnPath[T, C<:ColumnDef[T]] private[RowDef] (expand :C=>Column[T]) {
		/**  Converts a column under this path into a fully qualified version as seen from this RowDef. */
		protected[golem] def apply(nested :C) :Column[T] = expand(nested)

		/** Same as apply() - converts a column under this path into a fully qualified version as seen from this RowDef. */
		protected[golem] def unapply(nested :C) = Some(expand(nested))
	}

	implicit def ThisColumnPath[T] = new ColumnPath[T, Column[T]](identity[Column[T]])

	implicit def NestedColumnPath[T, G<:Group, C<:ColumnDef[T]](implicit path :G#ColumnPath[T, C]) :ColumnPath[T, C] =
		new ColumnPath[T, C]({ case path(nested) => nested.owner.asInstanceOf[G].parent(nested) })



	/** Returns a qualified version of the given column.
	  *
	  * @param nested a column under this RowDef
	  * @param path evidence showing path to column nested
	  * @return
	  */
	def apply[T, C<:ColumnDef[T]](nested :C)(implicit path :ColumnPath[T, C]) :Column[T] = path(nested)


	/** Represents actual row of this {@link RowDef RowDef}'s  type in a query ResultSet. */
	trait Row {

		/** Value of the specified column stored in this row. Delegates to {@link #valueOf valueOf()}
		  *
		  * @param column a column belonging to this RowDef
		  * @param path evidence that passed column is under this RowDef
		  */
		def apply[T, C<:ColumnDef[T]](column :C)(implicit path :ColumnPath[T, C]) :T =
			valueOf(path(column))

		/** Value of the specified column stored in this row. Delegates to {@link #valueOf valueOf()}
		  *
		  * @param column a function from owning RowDef into a column under it.
		  * @param path evidence that passed column is nested under this RowDef
		  */
		def apply[T, C<:ColumnDef[T], R>:RowDef.this.type](column :R=>C)(implicit path :ColumnPath[T, C]) :T =
			valueOf(path(column(RowDef.this)))

		/** Value of the specified column stored in this row. Implemented in specific subclasses.
		  * @param column column from this RowDef
		  */
		protected def valueOf[T](column :Column[T]) :T
	}

	


	/** Define a column of a given name.
	  * Has to be called in body constructor, like: <code>val name = column[String]("name")</code>
	  */
	protected def column[T :ColumnType](name :String) :Column[T] = declare[T](name)

	/** Defines a column of a given name, scala type X and known column type T */
	protected def column[X, T :ColumnType](name :String, pack :T=>X, unpack :X=>T) :Column[X] =
		declare[X](name)(ColumnType.mapped(pack, unpack))

	/** All columns belonging to this instance. This includes both those defined directly by this class,
	  * or nested under a chain of groups.
	  */
	def columns :Seq[ColumnDef[_]] = myColumns.toList


	protected[golem] def declare[T](col :Column[T]) :Column[T] = {
		myColumns += col
		col
	}
	protected[golem] final def declare[T :ColumnType](name :String) :Column[T] = declare(newColumn(name))

	protected[this] def newColumn[T :ColumnType](name :String) :Column[T] = new ColumnDef[T](this, name)



	private[this] val myColumns = new ListBuffer[ColumnDef[_]]()
}


trait RowPacking[+Entity] extends RowDef {

	def pack(row :Row) :Entity
}

trait RowUnpacking[-Entity] extends RowDef {
	def unpack(entity :Entity) :Row
}


trait RowContextPacking[+Entity, -PackContext] extends RowDef {

	def pack(row :Row, ctx :PackContext) :Entity
}

trait RowContextUnpacking[-Entity, -UnpackContext] extends RowDef {

	def unpack(entity :Entity, ctx :UnpackContext) :Row
}



trait RowMapping[Entity] extends RowPacking[Entity] with RowUnpacking[Entity]

trait RowEntityMapping[Entity, PackContext, UnpackContext]
	extends RowContextPacking[Entity, PackContext] with RowContextUnpacking[Entity, UnpackContext]






trait RowImpl {
	def valueOf[T](column :ColumnDef[T]) :T
}









//class Table[Entity](val name) extends RowMapping[Entity]
//
//object ClientApiSchema {
//	val Clients = new Clients()
//}
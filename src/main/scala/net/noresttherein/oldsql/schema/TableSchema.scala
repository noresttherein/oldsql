package net.noresttherein.oldsql.schema

import scala.collection.mutable.ArrayBuffer

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.Relation.{AlteredRelationOverrides, NamedAlteredRelation, RelationTemplate}




trait TableSchema {
	@inline final implicit def thisSchema :this.type = this

	def name :String
	def tables :Seq[schema.RelVar[MappingAt]]

	trait RelVar[M[O] <: MappingAt[O]] extends schema.RelVar[M] with RelationTemplate[M, RelVar[M]] {
		def schema :TableSchema = thisSchema

		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:RelVar[M] =
			if (includes.isEmpty && excludes.isEmpty)
				this
			else
				new NamedAlteredRelation[M, RelVar[M]](this, includes, excludes)
					with RelVar[M] with AlteredRelationOverrides[M, RelVar[M]]

		override def canEqual(that :Any) :Boolean = that match {
			case other :MutableSchema#RelVar[MappingAt @unchecked] => schema == other.schema
			case _ => false
		}
	}

	trait BaseTable[M[O] <: MappingAt[O]]//consider: renaming to simply Table
		extends RelVar[M] with schema.BaseTable[M] with RelationTemplate[M, BaseTable[M]]
	{
		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:BaseTable[M] =
			if (includes.isEmpty && excludes.isEmpty)
				this
			else
				new NamedAlteredRelation[M, BaseTable[M]](this, includes, excludes)
					with BaseTable[M] with AlteredRelationOverrides[M, BaseTable[M]]

		override def canEqual(that :Any) :Boolean = that match {
			case other :MutableSchema#BaseTable[MappingAt @unchecked] => schema == other.schema
			case _ => false
		}
	}

	trait StaticBaseTable[N <: String with Singleton, M[O] <: MappingAt[O]]
		extends BaseTable[M] with schema.BaseTable.StaticBaseTable[N, M]

	trait View[M[O] <: MappingAt[O]]
		extends RelVar[M] with schema.View[M] with RelationTemplate[M, View[M]]
	{
		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:View[M] =
			if (includes.isEmpty && excludes.isEmpty)
				this
			else
				new NamedAlteredRelation[M, View[M]](this, includes, excludes)
					with View[M] with AlteredRelationOverrides[M, View[M]]

		override def canEqual(that :Any) :Boolean = that match {
			case other :MutableSchema#View[MappingAt @unchecked] => schema == other.schema
			case _ => false
		}
	}

	trait StaticView[N <: String with Singleton, M[O] <: MappingAt[O]]
		extends View[M] with schema.View.StaticView[N, M]


	override def toString :String = name
}


object TableSchema {
	val default :MutableSchema = MutableSchema("")
}




//todo: make BaseTable and View accept an implicit MutableSchema
trait MutableSchema extends TableSchema {
	def createTable[M[O] <: MappingAt[O]](table :schema.BaseTable[M]) :Unit
	def createView[M[O] <: MappingAt[O]](view :schema.View[M]) :Unit
	//todo: factory methods for tables and views
	//todo: methods for procedures and sequences
	def init() :Unit

}


//todo: an implementation with a method initializing all tables.
//Tables should not be lazy vals for this to work; however, lazy row mappings should be enough.
object MutableSchema {
	def apply(name :String) :MutableSchema = new Impl(name)

	private class Impl(override val name :String) extends MutableSchema {
		@volatile private[this] var initialized = false
		override lazy val tables :Seq[schema.RelVar[MappingAt]] = { init(); tableBuffer.toSeq }
		private[this] val tableBuffer = ArrayBuffer.empty[schema.RelVar[MappingAt]]

		private def create[M[O] <: MappingAt[O]](table :schema.RelVar[M]) :Unit = synchronized {
			if (initialized)
				throw new IllegalStateException("Schema " + this + " already initialized.")
			tableBuffer += table
		}
		override def createTable[M[O] <: MappingAt[O]](table :schema.BaseTable[M]) :Unit = create(table)
		override def createView[M[O] <: MappingAt[O]](view :schema.View[M]) :Unit = create(view)

		override def init() :Unit = {
			initialized = true
			//consider: iterating over all tables and calling... what? subcomponents?
		}

	}
}

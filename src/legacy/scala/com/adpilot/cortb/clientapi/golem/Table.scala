package com.adpilot.cortb.clientapi.golem






abstract class Table protected (val schema :Schema, val name :String) extends RowDef {
	protected def this(name :String)(implicit schema :Schema) = this(schema, name)

}

abstract class MappedTable[Entity] protected (schema :Schema, name :String) extends Table(schema, name) with RowMapping[Entity] {
	protected def this(name :String)(implicit schema :Schema) = this(schema, name)


}

abstract class EntityTable[Entity, PackContext, UnpackContext] protected (schema :Schema, name :String)
	extends Table(schema, name) with RowEntityMapping[Entity, PackContext, UnpackContext]

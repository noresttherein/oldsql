package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.{Buffs, ColumnExtract, ColumnForm, ColumnMapping, MappingExtract, composeExtracts}
import net.noresttherein.oldsql.schema.bases.{BaseMapping, LazyMapping}




/**
  * @author Marcin Mościcki
  */
class PairMapping[_1, _2, O](val _1 :TypedMapping[_1, O], val _2 :TypedMapping[_2, O],
                             override val buffs :Buffs[(_1, _2)] = Buffs.empty[(_1, _2)])
	extends BaseMapping[(_1, _2), O] with LazyMapping[(_1, _2), O]
{
	def this()(implicit _1 :String => TypedMapping[_1, O], _2 :String => TypedMapping[_2, O]) =
		this(_1("_1"), _2("_2"))

	def this()(implicit _1 :ColumnForm[_1], _2 :ColumnForm[_2]) =
		this(ColumnMapping[_1, O]("_1")(_1), ColumnMapping[_2, O]("_2")(_2))

	override def assemble(pieces :Pieces) :Opt[(_1, _2)] =
		for (_1 <- pieces.get(this._1); _2 <- pieces.get(this._2)) yield (_1, _2)

	private def extract1 = MappingExtract.req(_1)((_ :(_1, _2))._1)
	private def extract2 = MappingExtract.req(_2)((_ :(_1, _2))._2)

	override lazy val extracts :NaturalMap[Component, Extract] =
		composeExtracts[(_1, _2), _1, O](_1.extracts, extract1) ++
			composeExtracts[(_1, _2), _2, O](_2.extracts, extract2)

	override lazy val components :Unique[TypedMapping[_, O]] = Unique[TypedMapping[_, O]](_1, _2)

	override def mappingName :String = "(" + _1.mappingName + ", " + _2.mappingName + ")"
}



class ColumnPairMapping[_1, _2, O](override val _1 :TypedColumn[_1, O], override val _2 :TypedColumn[_2, O],
                                   override val buffs :Buffs[(_1, _2)] = Buffs.empty[(_1, _2)])
	extends PairMapping[_1, _2, O](_1, _2, buffs)
{
	//can we skip checking if the column does not contian a subcolumn?
	override lazy val extracts :NaturalMap[Component, Extract] =
		NaturalMap[Component, Extract](
			Assoc[Component, Extract, _1](_1, ColumnExtract.req(_1)((_ :(_1, _2))._1)),
			Assoc[Component, Extract, _2](_2, ColumnExtract.req(_2)((_ :(_1, _2))._2)),
		)
	override lazy val columns    :Unique[Column[_]] = Unique[TypedColumn[_, O]](_1, _2)
	override lazy val components :Unique[Component[_]] = columns
}

package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.model.{Kin, KinFactory, RelatedEntityFactory}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.{Buff, Buffs, RelVar}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping




/** A mapping interface for join tables used to implement ''many-to-many'' relationships.
  * It is provided as a ready to use building block, with factory methods in its companion object,
  * but it is not necessary for ''many-to-many'' relationships to use this mapping.
  *
  * @tparam L  the mapping of the referenced table of the 'left' side of this relationship.
  * @tparam CL the mapping for the referenced component of the mapping `L` on the 'left' side of this relationship.
  * @tparam KL the type of the referenced key in the table mapping on the 'left' side of this relationship.
  * @tparam RL the reference type pointing to the left entity, such as [[net.noresttherein.oldsql.model.Kin Kin]]`[EL]`.
  * @tparam R  the mapping of the referenced table of the 'right' side of this relationship.
  * @tparam CR the mapping for the referenced component of the mapping `L` on the 'right' side of this relationship.
  * @tparam KR the type of the referenced key in the table mapping on the 'right' side of this relationship.
  * @tparam RR the reference type pointing to the right entity, such as [[net.noresttherein.oldsql.model.Kin Kin]]`[ER]`.
  * @tparam S  the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type of this mapping,
  *            that is an in-application representation of a row from the mapped join table.
  * @tparam O  the [[net.noresttherein.oldsql.schema.Mapping.Origin origin]] type of this mapping instance,
  *            used to mark all its components.
  * @see [[net.noresttherein.oldsql.schema.JoinTable]]
  */ //consider: renaming to GenericJoinTableMapping or smth, so that the column version can be renamed to JoinTableMapping
trait JoinTableMapping[L[A] <: MappingAt[A], CL[A] <: TypedMapping[KL, A], KL, RL,
                       R[A] <: MappingAt[A], CR[A] <: TypedMapping[KR, A], KR, RR, S, O]
	extends BaseMapping[S, O]
{
	val _1 :ForeignKeyMapping[L, CL, KL, RL, O]
	val _2 :ForeignKeyMapping[R, CR, KR, RR, O]
}


object JoinTableMapping {

	def apply[L[A] <: TypedMapping[EL, A], EL, CL[A] <: TypedMapping[KL, A], RL <: Kin[EL], KL,
	          R[A] <: TypedMapping[ER, A], ER, CR[A] <: TypedMapping[KR, A], RR <: Kin[ER], KR, O]
	         (left :ForeignKeyMapping[L, CL, KL, RL, O], right :ForeignKeyMapping[R, CR, KR, RR, O],
	          buffs :Buffs[(RL, RR)])
			:JoinTableMapping[L, CL, KL, RL, R, CR, KR, RR, (RL, RR), O] =
		new PairMapping[RL, RR, O](left, right, buffs)
		    with JoinTableMapping[L, CL, KL, RL, R, CR, KR, RR, (RL, RR), O]
	    {
			override val _1 = left
			override val _2 = right
		}
	def apply[L[A] <: TypedMapping[EL, A], EL, CL[A] <: TypedMapping[KL, A], RL <: Kin[EL], KL,
	          R[A] <: TypedMapping[ER, A], ER, CR[A] <: TypedMapping[KR, A], RR <: Kin[ER], KR, O]
	         (left :ForeignKeyMapping[L, CL, KL, RL, O], right :ForeignKeyMapping[R, CR, KR, RR, O],
	          buffs :Buff[(RL, RR)]*)
			:JoinTableMapping[L, CL, KL, RL, R, CR, KR, RR, (RL, RR), O] =
		apply[L, EL, CL, RL, KL, R, ER, CR, RR, KR, O](left, right, Buffs(buffs:_*))


	def apply[L[A] <: TypedMapping[EL, A], EL, CL[A] <: TypedMapping[KL, A], RL <: Kin[EL], KL,
	          R[A] <: TypedMapping[ER, A], ER, CR[A] <: TypedMapping[KR, A], RR <: Kin[ER], KR, O]
	         (left :RelVar[L], lpk :L[O] => CL[O], leftFK :RelatedEntityFactory[KL, EL, EL, RL],
	          renameLeftColumns :String => String, leftBuffs :Buffs[RL],
	          right :RelVar[R], rpk :R[O] => CR[O], rightFK :RelatedEntityFactory[KR, ER, ER, RR],
	          renameRightColumns :String => String, rightBuffs :Buffs[RR], buffs :Buffs[(RL, RR)])
			:JoinTableMapping[L, CL, KL, RL, R, CR, KR, RR, (RL, RR), O] =
		apply[L, EL, CL, RL, KL, R, ER, CR, RR, KR, O](
			ForeignKeyMapping[L, CL, KL, EL, EL, RL, O, O](renameLeftColumns, leftFK, leftBuffs)(left, lpk) :ForeignKeyMapping[L, CL, KL, RL, O],
			ForeignKeyMapping[R, CR, KR, ER, ER, RR, O, O](renameRightColumns, rightFK, rightBuffs)(right, rpk) :ForeignKeyMapping[R, CR, KR, RR, O],
			buffs :Buffs[(RL, RR)]
		)

	def apply[L[A] <: TypedMapping[EL, A], EL, CL[A] <: TypedMapping[KL, A], KL,
	          R[A] <: TypedMapping[ER, A], ER, CR[A] <: TypedMapping[KR, A], KR, O]
	         (left :RelVar[L], lpk :L[O] => CL[O], renameLeftColumns :String => String, leftBuffs :Buffs[Kin[EL]],
	          right :RelVar[R], rpk :R[O] => CR[O], renameRightColumns :String => String, rightBuffs :Buffs[Kin[ER]],
	          buffs :Buffs[(Kin[EL], Kin[ER])])
			:JoinTableMapping[L, CL, KL, Kin[EL], R, CR, KR, Kin[ER], (Kin[EL], Kin[ER]), O] =
		apply[L, EL, CL, Kin[EL], KL, R, ER, CR, Kin[ER], KR, O](
			ForeignKeyMapping.kin[L, CL, KL, EL, O, O](renameLeftColumns, true, leftBuffs)(left, lpk),
			ForeignKeyMapping.kin[R, CR, KR, ER, O, O](renameRightColumns, true, rightBuffs)(right, rpk),
			buffs
		)

}






trait JoinTableColumnsMapping[L[A] <: MappingAt[A], KL, RL, R[A] <: MappingAt[A], KR, RR, S, O]
	extends JoinTableMapping
		    [L, MappingOf[KL]#ColumnProjection, KL, RL, R, MappingOf[KR]#ColumnProjection, KR, RR, S, O]
{
    override val _1 :ForeignKeyColumnMapping[L, KL, RL, O]
    override val _2 :ForeignKeyColumnMapping[R, KR, RR, O]
}


object JoinTableColumnsMapping {

	def apply[L[A] <: TypedMapping[EL, A], EL, RL <: Kin[EL], KL,
	          R[A] <: TypedMapping[ER, A], ER, RR <: Kin[ER], KR, O]
	         (left :ForeignKeyColumnMapping[L, KL, RL, O], right :ForeignKeyColumnMapping[R, KR, RR, O],
	          buffs :Buffs[(RL, RR)])
			:JoinTableColumnsMapping[L, KL, RL, R, KR, RR, (RL, RR), O] =
		new ColumnPairMapping[RL, RR, O](left, right, buffs)
		    with JoinTableColumnsMapping[L, KL, RL, R, KR, RR, (RL, RR), O]
	    {
			override val _1 = left
			override val _2 = right
		}
	def apply[L[A] <: TypedMapping[EL, A], EL, RL <: Kin[EL], KL,
	          R[A] <: TypedMapping[ER, A], ER, RR <: Kin[ER], KR, O]
	         (left :ForeignKeyColumnMapping[L, KL, RL, O], right :ForeignKeyColumnMapping[R, KR, RR, O],
	          buffs :Buff[(RL, RR)]*)
			:JoinTableColumnsMapping[L, KL, RL, R, KR, RR, (RL, RR), O] =
		apply[L, EL, RL, KL, R, ER, RR, KR, O](left, right, Buffs(buffs:_*))

	def apply[L[A] <: TypedMapping[EL, A], EL, KL, R[A] <: TypedMapping[ER, A], ER, KR, O]
	         (left :RelVar[L], lpk :L[O] => TypedColumn[KL, O], leftName :String, leftBuffs :Buffs[Kin[EL]],
	          right :RelVar[R], rpk :R[O] => TypedColumn[KR, O], rightName :String, rightBuffs :Buffs[Kin[ER]],
	          buffs :Buffs[(Kin[EL], Kin[ER])])
			:JoinTableColumnsMapping[L, KL, Kin[EL], R, KR, Kin[ER], (Kin[EL], Kin[ER]), O] =
		apply[L, EL, Kin[EL], KL, R, ER, Kin[ER], KR, O](
			ForeignKeyColumnMapping.kin[L, KL, EL, O, O](leftName, true, leftBuffs)(left, lpk),
			ForeignKeyColumnMapping.kin[R, KR, ER, O, O](rightName, true, rightBuffs)(right, rpk),
			buffs
		)
}

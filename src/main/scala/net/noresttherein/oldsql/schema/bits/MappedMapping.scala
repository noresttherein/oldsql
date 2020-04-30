package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.MappingAdapter.{AdaptedAs, ShallowAdapter}
import net.noresttherein.oldsql.schema.Buff.BuffMappingFailureException
import net.noresttherein.oldsql.schema.support.MappingAdapter
import net.noresttherein.oldsql.schema.MappingExtract.ColumnMappingExtract

import scala.util.Try





//todo: combine MappedMapping and FlatMappedMapping by use of extractors.
trait MappedMapping[+M <: Mapping.TypedMapping[T, O], T, S, O] extends ShallowAdapter[M, T, S, O] {
	implicit protected def nulls :NullValue[S]
	protected def map :T => S
	protected def unmap :S => T



/*
	override def apply[X](component :Component[X]) :Extract[X] =
		if (component eq egg)
			eggExtract.asInstanceOf[Extract[X]]
		else
			MappingExtract(component)(egg(component) compose unmap)

	override def apply[X](column :Column[X]) :ColumnExtract[S, X, O] =
		if (column eq egg)
			eggExtract.asInstanceOf[ColumnExtract[S, X, O]]
		else
			MappingExtract(column)(egg(column) compose unmap)
*/



	private[this] val eggExtract :Extract[T] = MappingExtract.req(egg)(unmap)

	override val extracts :NaturalMap[Component, Extract] = {
		def adapt[X](entry :Assoc[Component, egg.Extract, X]) :Assoc[Component, Extract, X] =
			Assoc[Component, Extract, X](entry._1, entry._2 compose unmap)
		egg.extracts.map(adapt(_)).updated(egg, eggExtract)
	}

	override val columnExtracts :NaturalMap[Column, ColumnExtract] = egg match {
		case column :ColumnMapping[T @unchecked, O @unchecked] =>
			NaturalMap.single[Column, ColumnExtract, T](column, eggExtract.asInstanceOf[ColumnExtract[T]])
		case _ =>
			egg.columnExtracts.map(schema.composeColumnExtractAssoc(eggExtract)(_))
	}



	override val components :Unique[Component[_]] = Unique(egg)//egg.components

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = {
		val form = egg.selectForm(if (components.contains(egg)) egg.selectable else components)
		if (nulls == null) form.mapNull(map) else form.map(map)
	}

	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		egg.queryForm(if (components.contains(egg)) egg.queryable else components).unmap(unmap)

	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		egg.updateForm(if (components.contains(egg)) egg.updatable else components).unmap(unmap)

	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		egg.insertForm(if (components.contains(egg)) egg.insertable else components).unmap(unmap)



	override val selectForm :SQLReadForm[S] =
		if (nulls == null) egg.selectForm.mapNull(map)
		else egg.selectForm.map(map)

	override val queryForm :SQLWriteForm[S] = egg.queryForm.unmap(unmap)
	override val updateForm :SQLWriteForm[S] = egg.updateForm.unmap(unmap)
	override val insertForm :SQLWriteForm[S] = egg.insertForm.unmap(unmap)


	override val buffs :Seq[Buff[S]] = egg.buffs.map(_.bimap(map, unmap)) //todo: should buffs use the NullValue?



	override def assemble(values :Pieces) :Option[S] = values.get(eggExtract).map(map)

	override def nullValue :Option[S] =
		if (nulls != null) nulls.toOption
		else Try { egg.nullValue.map(map) }.toOption.flatten

	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
		MappedMapping[M, T, X, O](egg, map andThen there, back andThen unmap)(mapNulls(there))

	override def flatMap[X](there :S => Option[X], back :X => Option[S])
	                       (implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
		MappedMapping.opt[M, T, X, O](egg, map andThen there, back(_) map unmap)(flatMapNulls(there))



	protected def mapNulls[X](there :S => X)(implicit nulls :NullValue[X]) :NullValue[X] =
		if (nulls != null) nulls
		else if (this.nulls != null) this.nulls.map(there)
		else null

	protected def flatMapNulls[X](there :S => Option[X])(implicit nulls :NullValue[X]) :NullValue[X] =
		if (nulls != null) nulls
		else if (this.nulls != null) this.nulls.flatMap(there)
		else null



	override def canEqual(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this

	override def toString :String = "Mapped(" + egg  + ")"

}






object MappedMapping {

	def apply[M <: TypedMapping[S, O], S, T, O](mapping :M, mapped :S => T, unmapped :T => S)
	                                           (implicit nulls :NullValue[T] = null) :M AdaptedAs T =
		new MappingBijection[M, S, T, O](mapping, mapped, unmapped)


	def opt[M <: TypedMapping[S, O], S, T, O](mapping :M, mapped :S => Option[T], unmapped :T => Option[S])
	                                         (implicit nulls :NullValue[T] = null) :M AdaptedAs T =
		new FlatMappedMapping[M, S, T, O](mapping, mapped, unmapped, nulls)






	private class MappingBijection[M <: Mapping.TypedMapping[T, O], T, S, O]
	                              (override val egg :M, override val map :T => S, override val unmap :S => T)
	                              (implicit override val nulls :NullValue[S] = null)
		extends MappedMapping[M, T, S, O] with MappingAdapter[M, S, O] //AdaptedAs[M, T]



	class FlatMappedMapping[+M <: Mapping.TypedMapping[T, O], T, S, O]
	                       (override val egg :M,
	                        protected final val map :T => Option[S],
	                        protected final val unmap :S => Option[T],
	                        onNone :NullValue[S] = null)
		extends ShallowAdapter[M, T, S, O] with MappingAdapter[M, S, O] //AdaptedAs[M, S]
	{
		implicit protected val nulls :NullValue[S] =
			if (onNone != null)
				onNone
			else
	            Try { egg.nullValue.flatMap(map) }.toOption.flatten.map(NullValue.apply[S] _) getOrElse
					NullValue.eval[S] {
						throw new IllegalArgumentException(
							s"No NullValue provided for $this and None after mapping ${egg.nullValue}."
						)
					}

		override def nullValue :Option[S] = nulls.toOption



		override def assemble(values :Pieces) :Option[S] = values.get(eggExtract).flatMap(map)



/*
		override def apply[X](component :Component[X]) :Extract[X] =
			if (component eq egg)
				eggExtract.asInstanceOf[Extract[X]]
			else
				MappingExtract(component)(egg(component) composeOpt unmap)


		override def apply[X](column :Column[X]) :ColumnExtract[S, X, O] =
			if (column eq egg)
				eggExtract.asInstanceOf[ColumnExtract[S, X, O]]
			else
				MappingExtract(column)(egg(column) composeOpt unmap)
*/

		private[this] val eggExtract :Extract[T] = MappingExtract.opt(egg)(unmap)

		override val extracts :NaturalMap[Component, Extract] = {
			def adapt[X](entry :Assoc[Component, egg.Extract, X]) =
				Assoc[Component, Extract, X](entry._1, entry._2 composeOpt unmap)
			egg.extracts.map(adapt(_)).updated(egg, eggExtract)
		}

		override val columnExtracts :NaturalMap[Column, ColumnExtract] = egg match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				NaturalMap.single[Column, ColumnExtract, T](column, eggExtract.asInstanceOf[ColumnExtract[T]])
			case _ =>
				egg.columnExtracts.map(schema.composeColumnExtractAssoc(eggExtract)(_))
		}



		override val components :Unique[Component[_]] = Unique(egg)


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.contains(egg)) egg.selectForm(egg.selectable).flatMap(map)
			else egg.selectForm(components).flatMap(map)

		override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(egg)) egg.queryForm(egg.queryable).flatUnmap(unmap)
			else egg.queryForm(components).flatUnmap(unmap)

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(egg)) egg.updateForm(egg.updatable).flatUnmap(unmap)
			else egg.updateForm(components).flatUnmap(unmap)

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(egg)) egg.insertForm(egg.insertable).flatUnmap(unmap)
			else egg.insertForm(components).flatUnmap(unmap)

		

		override val selectForm :SQLReadForm[S] = egg.selectForm.flatMap(map)
		override val queryForm :SQLWriteForm[S] = egg.queryForm.flatUnmap(unmap)
		override val updateForm :SQLWriteForm[S] = egg.updateForm.flatUnmap(unmap)
		override val insertForm :SQLWriteForm[S] = egg.insertForm.flatUnmap(unmap)


		override val buffs :Seq[Buff[S]] = egg.buffs.map(buff => buff.bimap(
			s => map(s).getOrElse { throw new BuffMappingFailureException(
					s"Failed mapping of $egg: could not derive the value for the buff $buff from $s."
				)},
			(t :S) => unmap(t) getOrElse { throw new BuffMappingFailureException(
					s"Failed mapping of $egg: could not unmap value $t as part of the buff $buff."
				)}
		))



		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
			new FlatMappedMapping[M, T, X, O](egg, map(_) map there, back andThen unmap, mapNulls(there))

		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
			new FlatMappedMapping[M, T, X, O](egg, map(_) flatMap there, back(_) flatMap unmap, flatMapNulls(there))


		protected def mapNulls[X](there :S => X)(implicit nulls :NullValue[X]) :NullValue[X] =
			if (nulls != null) nulls else this.nulls.map(there)

		protected def flatMapNulls[X](there :S => Option[X])(implicit nulls :NullValue[X]) :NullValue[X] =
			if (nulls != null) nulls else this.nulls.flatMap(there)



		override def canEqual(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this

		override def toString :String = "Mapped(" + egg  + ")"

	}






/*
	class MappedColumnMapping[M <: ColumnMapping[O, S], O, S, T]
	                         (override val egg :M, override val map :S => T, override val unmap :T => S)
	                         (implicit override val nulls :NullValue[T] = null)
		extends MappedMapping[M, O, S, T] with MappingAdapter[M, O, T] with ColumnMapping[O, T]
	{
		override def name :String = egg.name

		override val form :ColumnForm[T] =
			if (nulls != null) egg.form.bimap(map)(unmap)
			else egg.form.bimapNull(map)(unmap)

		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :MappedColumnMapping[M, O, S, X] =
			new MappedColumnMapping[M, O, S, X](egg, map andThen there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedColumnMapping[M, O, S, X] =
			new FlatMappedColumnMapping[M, O, S, X](egg, map andThen there, back(_) map unmap)(flatMapNulls(there))
	}



	class FlatMappedColumnMapping[M <: ColumnMapping[O, S], O, S, T]
	                             (col :M, there :S => Option[T], back :T => Option[S])
	                             (implicit nullValue :NullValue[T] = null)
		extends FlatMappedMapping[M, O, S, T](col, there, back) with ColumnMapping[O, T]
	{
		override def name :String = egg.name

		override val form :ColumnForm[T] =
			if (nulls != null) egg.form.biflatMap(map)(unmap)
			else egg.form.biflatMapNull(map)(unmap)

		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :FlatMappedColumnMapping[M, O, S, X] =
			new FlatMappedColumnMapping[M, O, S, X](egg, map(_) map there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedColumnMapping[M, O, S, X] =
			new FlatMappedColumnMapping[M, O, S, X](egg, map(_) flatMap there, back(_) flatMap unmap)(flatMapNulls(there))

	}
*/



}


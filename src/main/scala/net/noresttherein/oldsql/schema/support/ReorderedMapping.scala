package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.morsels.permutation
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.{ColumnExtract, ColumnForm, ColumnMapping, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.{DirectProxy, WrapperProxy}
import net.noresttherein.oldsql.schema.bases.LazyMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, ComposedAdapter, DelegateAdapter}




/** A [[net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy shallow]] proxy to another mapping
  * which changes the order in which the columns appear on
  * [[net.noresttherein.oldsql.schema.support.ReorderedMapping.columns columns]] list according to the given permutation.
  * Operation specific lists ([[net.noresttherein.oldsql.schema.support.ReorderedMapping.selectable selectable]],
  * [[net.noresttherein.oldsql.schema.support.ReorderedMapping.insertable insertable]], etc.), as well as their
  * corresponding forms, are similarly reordered to remain subsequences of `columns`. All methods returning columns
  * of a component however are not reordered.
  * @param backer      The adapted mapping.
  * @param permutation A sequence of values `0..backer.columns.size-1`, each appearing exactly once.
  *                    The number at index `n` specifies the index in this adapter's `columns` to which
  *                    column `backer.columns(n)` is moved.
  * @author Marcin Mościcki
  */
private[oldsql] class ReorderedMapping[+M <: TypedMapping[S, O], S, O]
                                      (protected override val backer :M, val permutation :IndexedSeq[Int])
	extends DirectProxy[S, O] with DelegateMapping[M, S, O] with Mapping
{
	if (permutation.length != backer.columns.size)
		throw new IllegalArgumentException(
			"Permutation table's " + permutation + " length (" + permutation.length +
			") does not match the number of columns in the adapted mapping (" + backer.columns.size + "): " +
			backer.columns + "."
		)
//	private[this] val order = Rearrangement.permutation(permutation)
	private[this] val inverse = {
		val length = permutation.length
		val oldIndex = new Array[Int](length)
		var i = length
		while (i > 0) {
			i -= 1
			oldIndex(i) = -1
		}
		i = length
		while (i > 0) {
			i -= 1
			val newIndex = permutation(i)
			if (newIndex < 0 || newIndex >= length || oldIndex(newIndex) >= 0)
				throw new IllegalArgumentException("Invalid permutation: " + permutation + ".")
			oldIndex(newIndex) = i
		}
		oldIndex
	}

	//lazy vals because components of backer may be lazy (as in ForeignKeyMapping)
	override lazy val columns           :Unique[Column[_]] =
		Unique.tabulate(permutation.length)(i => backer.columns(inverse(i)))

	override lazy val selectable        :Unique[Column[_]] = super[Mapping].selectable
	override lazy val filterable        :Unique[Column[_]] = super[Mapping].filterable
	override lazy val insertable        :Unique[Column[_]] = super[Mapping].insertable
	override lazy val updatable         :Unique[Column[_]] = super[Mapping].updatable
	override lazy val autoInserted      :Unique[Column[_]] = super[Mapping].autoInserted
	override lazy val autoUpdated       :Unique[Column[_]] = super[Mapping].autoUpdated
	override lazy val selectedByDefault :Unique[Column[_]] = super[Mapping].selectedByDefault
	override lazy val filteredByDefault :Unique[Column[_]] = super[Mapping].filteredByDefault
	override lazy val insertedByDefault :Unique[Column[_]] = super[Mapping].insertedByDefault
	override lazy val updatedByDefault  :Unique[Column[_]] = super[Mapping].updatedByDefault
	override lazy val mandatorySelect   :Unique[Column[_]] = super[Mapping].mandatorySelect
	override lazy val mandatoryFilter   :Unique[Column[_]] = super[Mapping].mandatoryFilter
	override lazy val mandatoryInsert   :Unique[Column[_]] = super[Mapping].mandatoryInsert
	override lazy val mandatoryUpdate   :Unique[Column[_]] = super[Mapping].mandatoryUpdate

	override def columns(op :OperationView) :Unique[Column[_]] = op.applicableColumns(this)
	override def defaultColumns(op :OperationView) :Unique[Column[_]] = op.defaultColumns(this)

	override lazy val selectForm :SQLReadForm[S]  =
		backer.selectForm.reorder(permutation(selectedByDefault, backer.selectedByDefault))
	override lazy val filterForm :SQLWriteForm[S] =
		backer.filterForm.reorder(permutation(filteredByDefault, backer.filteredByDefault))
	override lazy val insertForm :SQLWriteForm[S] =
		backer.insertForm.reorder(permutation(insertedByDefault, backer.insertedByDefault))
	override lazy val updateForm :SQLWriteForm[S] =
		backer.updateForm.reorder(permutation(updatedByDefault, backer.updatedByDefault))
	override def writeForm(op :WriteOperationView) :SQLWriteForm[S] = newWriteForm(op)

	protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[S] = {
		val formPermutation = backer.defaultColumns(op).toIndexedSeq.map(defaultColumns(op).columnIndex(_))
		backer.writeForm(op).reorder(formPermutation)
	}

//	private def reorder(columns :Unique[TypedColumn[_, O]]) = this.columns.filter(columns.contains(_))

	private def permutation(columns :Unique[TypedColumn[_, O]], backing :Unique[TypedColumn[_, O]]) =
		backing.toIndexedSeq.map(columns.columnIndex(_))

	override def reorder(permutation :IndexedSeq[Int]) :Component[S] = {
		ReorderedMapping.validatePermutation(this, permutation)
		if (permutation == permutation.indices)
			this
		else if ((0 until permutation.length).forall(i => permutation(this.permutation(i)) == i))
			backer
		else
			backer.reorder(this.permutation.map(permutation(_)))
	}

	override def uniIsomorphic(that :Mapping) :Boolean = super[Mapping].uniIsomorphic(that)

	override def mappingName :String = backer.mappingName + "<=>"
	override def toString :String = permutation.mkString(mappingName + "Permutation(", ",", ")")
}




object ReorderedMapping {
	def apply[M <: TypedMapping[S, O], S, O](mapping :M, permutation :IndexedSeq[Int]) :Adapted[M] =
		new ReorderedMappingAdapter[M, S, O](mapping, permutation)

	def apply[M <: MappingAt[O], S, O]
	         (mapping :MappingAdapter[M, S, O], permutation :IndexedSeq[Int]) :MappingAdapter[M, S, O] =
		new ReorderedMappingComposedAdapter[M, S, O](mapping, permutation)

	def apply[M <: TypedMapping[S, O], T, S, O]
	         (mapping :M, component :TypedMapping[T, O], permutation :IndexedSeq[Int]) :MappingAdapter[M, S, O] =
	{
		val substitute = ReorderedMapping[TypedMapping[T, O], T, O](component, permutation)
		PatchedMapping[M, S, O](mapping, PatchedMapping.Overrides(component, substitute))
	}


	def apply[M <: TypedMapping[S, O], S, O]
	         (mapping :M)(precedes :(TypedColumn[_, O], TypedColumn[_, O]) => Boolean) :Adapted[M] =
		ReorderedMapping[M, S, O](mapping, permutation(mapping.columns.toIndexedSeq)(precedes))

	def apply[M <: MappingAt[O], S, O]
	         (mapping :MappingAdapter[M, S, O])(precedes :(TypedColumn[_, O], TypedColumn[_, O]) => Boolean)
			:MappingAdapter[M, S, O] =
		ReorderedMapping[M, S, O](mapping, permutation(mapping.columns.toIndexedSeq)(precedes))

	def apply[M <: TypedMapping[S, O], T, S, O]
	         (mapping :M, component :TypedMapping[T, O])(precedes :(TypedColumn[_, O], TypedColumn[_, O]) => Boolean)
			:MappingAdapter[M, S, O] =
		ReorderedMapping(mapping, component, permutation(component.columns.toIndexedSeq)(precedes))



	private[oldsql] def validatePermutation(mapping :Mapping, permutation :IndexedSeq[Int]) :Unit =
		if (permutation.length != mapping.columns.size)
			throw new IllegalArgumentException(
				"Length of permutation " + permutation + " (" + permutation.length +
				") does not match the number of columns " + mapping.columns.size + " in " + mapping + ": " +
				mapping.columns + "."
			)
		else if (permutation.sorted != permutation.indices)
			throw new IllegalArgumentException(
				permutation.toString + " is not a valid permutation of columns " + mapping.columns + " in " + mapping + "."
			)


	private[oldsql] class ReorderedMappingAdapter[+M <: TypedMapping[S, O], S, O]
	                                             (override val backer :M, override val permutation :IndexedSeq[Int])
		extends ReorderedMapping[M, S, O](backer, permutation) with MappingDecorator[M, S, O] with DelegateAdapter[M, S, O]
	{
		override def reorder(permutation :IndexedSeq[Int]) :MappingAdapter[M, S, O] = {
			validatePermutation(this, permutation)
			if (permutation == permutation.indices)
				this
			else
				ReorderedMapping[M, S, O](backer, this.permutation.map(permutation(_)))
		}
	}

	private[oldsql] class ReorderedMappingComposedAdapter[+M <: MappingAt[O], S, O]
	                      (override val backer :MappingAdapter[M, S, O], override val permutation :IndexedSeq[Int])
		extends ReorderedMapping[MappingAdapter[M, S, O], S, O](backer, permutation)
		   with ComposedAdapter[M, S, S, O]
	{
		override def reorder(permutation :IndexedSeq[Int]) :MappingAdapter[M, S, O] = {
			validatePermutation(this, permutation)
			if (permutation == permutation.indices)
				this
			else
				ReorderedMapping[M, S, O](backer, this.permutation.map(permutation(_)))
		}
	}

}






private[oldsql] class RearrangedMapping[+M <: TypedMapping[S, O], S, O](backer :M, order :Rearrangement)
	extends LazyMapping[S, O]
{
	if (order.columnCount != backer.columns.size)
		throw new IllegalArgumentException(
			"The number of columns " + backer.columns.size + " of reordered mapping " + backer +
				" does not match the ordering column count " + order.columnCount + ": " + order + "."
		)

	override def assemble(pieces :Pieces) :Opt[S] = pieces.get(backer)

	override lazy val extracts :NaturalMap[Component, Extract] = {
		def extract[T](column :TypedColumn[T, O]) =
			Assoc[Component, Extract, T](column, ColumnExtract.none(column))
		val extraExtracts = columns.view.filterNot(backer.columns.contains).map(extract(_))
		backer.extracts ++ extraExtracts
	}

	override lazy val components        :Unique[Component[_]] = Unique.single(backer)
	override lazy val subcomponents     :Unique[Component[_]] = (backer +: backer.subcomponents) ++ columns

	//lazy vals because components of backer may be lazy (as in ForeignKeyMapping)
	override lazy val columns           :Unique[Column[_]] =
		Unique.tabulate(order.underlyingColumnCount) { i =>
			if (order.isCovered(i)) backer.columns(order.inverse(i)) :Column[_]
			else ColumnMapping[Null, O]("NULL#" + i)(ColumnForm.nulls) :Column[_]
		}
	override def columns(op :OperationView) :Unique[Column[_]] = op.applicableColumns(this)
	override def defaultColumns(op :OperationView) :Unique[Column[_]] = op.defaultColumns(this)

	override lazy val selectForm :SQLReadForm[S]  =
		backer.selectForm.reorder(reorder(selectedByDefault, backer.selectedByDefault))
	override lazy val filterForm :SQLWriteForm[S] =
		backer.filterForm.reorder(reorder(filteredByDefault, backer.filteredByDefault))
	override lazy val insertForm :SQLWriteForm[S] =
		backer.insertForm.reorder(reorder(insertedByDefault, backer.insertedByDefault))
	override lazy val updateForm :SQLWriteForm[S] =
		backer.updateForm.reorder(reorder(updatedByDefault, backer.updatedByDefault))
	override def writeForm(op :WriteOperationView) :SQLWriteForm[S] = newWriteForm(op)

	protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[S] =
		backer.writeForm(op).reorder(reorder(defaultColumns(op), backer.defaultColumns(op)))

	private def reorder(columns :Unique[TypedColumn[_, O]], backing :Unique[TypedColumn[_, O]]) :Rearrangement =
		new Rearrangement {
			override def columnCount            = backing.size
			override def underlyingColumnCount  = columns.size
			override def apply(index :Int)      = columns.columnIndex(backing(index))
			override def underlying(index :Int) = Got(columns.indexOf(backing(index))).filter(_ >= 0)
			override def isMapped(index :Int)   = columns.indexOf(backing(index)) >= 0
			override def inverse(index :Int)    = backing.columnIndex(columns(index))
			override def exposed(index :Int)    = Got(backing.indexOf(columns(index))).filter(_ >= 0)
			override def isCovered(index :Int)  = backing.indexOf(columns(index)) >= 0

			override def toString =
				columns.toSeq.map { col =>
					backing.indexOf(col) match {
						case -1 => "_"
						case n  => n.toString
					}
				}.mkString("Order(", ",", ")")
		}

	override def uniIsomorphic(that :Mapping) :Boolean = super.uniIsomorphic(that)

	override def mappingName :String = backer.mappingName + "<=>"

	override def toString :String = mappingName + order
}





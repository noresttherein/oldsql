package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.{Chain, ChainMap, IndexedChain, LabeledChain, Record}
import net.noresttherein.oldsql.collection.ChainMap.&~
import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.collection.LabeledChain.>~
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.schema.SQLReadForm.{AbstractChainIndexReadForm, BaseChainReadForm}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.ColumnReadForm.OptionColumnReadForm
import net.noresttherein.oldsql.schema.ColumnWriteForm.OptionColumnWriteForm
import net.noresttherein.oldsql.schema.ScalaForms.OptionForm
import net.noresttherein.oldsql.schema.SQLForm.{ChainForm, ChainMapForm, EmptyForm, IndexedChainForm, LabeledChainForm, RecordForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.{ChainWriteForm, GenericChainWriteForm}
import net.noresttherein.oldsql.slang

//implicits
import slang._






trait SQLReadForms

trait SQLWriteForms

trait SQLForms extends SQLReadForms with SQLWriteForms

trait ColumnReadForms extends SQLReadForms

trait ColumnWriteForms extends SQLWriteForms

trait ColumnForms extends SQLForms with ColumnReadForms with ColumnWriteForms

/** Base trait combining companion traits to objects containing implicit form declarations.
  * Extended by both read and write forms, it brings those implicits into the search scope for all form types.
  */
trait BaseSQLForm extends ColumnForms with JDBCTypes with ScalaForms with JavaForms with Serializable  {

	def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def toString :String = this.innerClassName

}






sealed trait SQLReadFormLevel2Implicits {
	/** Provides an implicit form for the heterogeneous list (`Chain`) `I ~ L` as long as implicit forms for both
	  * `I` and `L` are available. */
	implicit def ChainReadForm[I <: Chain, L](implicit i :SQLReadForm[I], l :SQLReadForm[L]) :SQLReadForm[I ~ L] =
		new BaseChainReadForm[I, L](i, l)
}



sealed trait SQLReadFormLevel1Implicits extends SQLReadFormLevel2Implicits {
	/** Provides an implicit form for the heterogeneous map indexed by literal types (`ChainMap`) `I &~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def ChainMapReadForm[I <: ChainMap :SQLReadForm, K <: Singleton :ValueOf, V :SQLReadForm]
			:SQLReadForm[I &~ (K, V)] =
		new AbstractChainIndexReadForm[&~, Tuple2, Singleton, I, K, V] {
			protected[this] override def cons(init :I, value :V) = init &~ (key -> value)
			protected override def symbol = "&~"
		}
}



object SQLReadForms extends SQLReadFormLevel1Implicits {

	/** Provides an implicit form for the heterogeneous map indexed by types (`IndexedChain`) `I |~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def IndexedChainReadForm[I <: IndexedChain :SQLReadForm, K <: IndexedChain.Key :ValueOf, V :SQLReadForm]
			:SQLReadForm[I |~ (K :~ V)] =
		new AbstractChainIndexReadForm[|~, :~, IndexedChain.Key, I, K, V] {
			protected[this] override def cons(init :I, value :V) = init |~ key :~ value
			protected override def symbol = "|~"
		}


	/** Provides an implicit form for the heterogeneous map indexed by string literal types (`LabeledChain`) `I >~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def LabeledChainReadForm[I <: LabeledChain :SQLReadForm, K <: Label :ValueOf, V :SQLReadForm]
			:SQLReadForm[I >~ (K :~ V)] =
		new AbstractChainIndexReadForm[>~, :~, Label, I, K, V] {
			protected[this] override def cons(init :I, value :V) = init >~ key :~ value
			protected override def symbol = ">~"
		}

	/** Provides an implicit form for the heterogeneous map indexed by string literals (`Record`) `I |# L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def RecordReadForm[I <: Record :SQLReadForm, K <: String with Singleton :ValueOf, V :SQLReadForm]
			:SQLReadForm[I |# (K, V)] =
		new AbstractChainIndexReadForm[|#, Tuple2, Label, I, K, V] {
			protected[this] override def cons(init :I, value :V) = init |# (key -> value)
			protected override def symbol = "|#"
		}

}






sealed trait SQLWriteFormLevel2Implicits {
	implicit def ChainWriteForm[I <: Chain, L](implicit t :SQLWriteForm[I], h :SQLWriteForm[L]) :SQLWriteForm[I ~ L] =
		new ChainWriteForm(t, h)
}



sealed trait SQLWriteFormLevel1Implicits extends SQLWriteFormLevel2Implicits {
	implicit def ChainMapWriteForm[I <: ChainMap :SQLWriteForm, K <: Singleton, V :SQLWriteForm] :SQLWriteForm[I &~ (K, V)] =
		new GenericChainWriteForm(SQLWriteForm[I], SQLWriteForm[V].unmap(_._2), SQLWriteForm[V], "&~")
}



object SQLWriteForms extends SQLWriteFormLevel1Implicits {

	implicit def IndexedChainWriteForm[I <: IndexedChain :SQLWriteForm, K <: IndexedChain.Key, V :SQLWriteForm]
			:SQLWriteForm[I |~ (K :~ V)] =
		new GenericChainWriteForm(SQLWriteForm[I], SQLWriteForm[V].unmap(_.value), SQLWriteForm[V], "|~")

	implicit def LabeledChainWriteForm[I <: LabeledChain :SQLWriteForm, K <: Label, V :SQLWriteForm]
			:SQLWriteForm[I >~ (K :~ V)] =
		new GenericChainWriteForm(SQLWriteForm[I], SQLWriteForm[V].unmap(_.value), SQLWriteForm[V], ">~")

	implicit def RecordWriteForm[I <: Record :SQLWriteForm, K <: String with Singleton, V :SQLWriteForm]
			:SQLWriteForm[I |# (K, V)] =
		new GenericChainWriteForm(SQLWriteForm[I], SQLWriteForm[V].unmap(_._2), SQLWriteForm[V], "|#")

}







sealed trait SQLFormLevel2Implicits {
	implicit def ChainForm[I <: Chain, L](implicit i :SQLForm[I], l :SQLForm[L]) :SQLForm[I ~ L] =
		new ChainForm(i, l)
}

sealed trait SQLFormLevel1Implicits extends SQLFormLevel2Implicits {
	implicit def ChainMapForm[I <: ChainMap :SQLForm, K <: Singleton :ValueOf, V :SQLForm] :SQLForm[I &~ (K, V)] =
		new ChainMapForm(SQLForm[I], valueOf[K], SQLForm[V])
}



object SQLForms extends SQLFormLevel1Implicits {
	/** An empty chain form for the `Chain` terminator `@~`, which doesn't write or read any columns. */
	implicit val EmptyChainForm :SQLForm[@~] = new EmptyForm[@~](@~) {
		override def equals(that :Any) :Boolean = that.getClass == getClass
		override def hashCode :Int = getClass.hashCode
		override def toString = "@~"
	}

	implicit def IndexedChainForm[I <: IndexedChain :SQLForm, K :ValueOf, V :SQLForm] :SQLForm[I |~ (K :~ V)] =
		new IndexedChainForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def LabeledChainForm[I <: LabeledChain :SQLForm, K <: Label :ValueOf, V :SQLForm] :SQLForm[I >~ (K :~ V)] =
		new LabeledChainForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def RecordForm[I <: Record :SQLForm, K <: Label :ValueOf, V :SQLForm] :SQLForm[I |# (K, V)] =
		new RecordForm(SQLForm[I], valueOf[K], SQLForm[V])

}






object ColumnReadForms {

	implicit def OptionColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Option[T]] =
		new OptionColumnReadForm[T] { override val form = ColumnReadForm[T] }

	def SomeColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Some[T]] =
		ColumnReadForm[T].nullMap(Some.apply)

}



object ColumnWriteForms {

	implicit def OptionColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Option[T]] =
		new OptionColumnWriteForm[T] {
			val form :ColumnWriteForm[T] = ColumnWriteForm[T]
		}

	def SomeColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Some[T]] =
		ColumnWriteForm[T].unmap(_.get)

}



object ColumnForms {

	/** Lifts the implicit `ColumnForm[T]` implementation to `ColumnForm[Option[T]]`. */
	implicit def OptionColumnForm[T :ColumnForm] :ColumnForm[Option[T]] =
		new OptionForm[T] with OptionColumnWriteForm[T] with OptionColumnReadForm[T] with ColumnForm[Option[T]] {
			override val form = ColumnForm[T]
			override def toString = super[OptionForm].toString
		}

	/** Lifts the implicit `ColumnForm[T]` implementation to `ColumnForm[Some[T]]`. */
	def SomeColumnForm[T :ColumnForm] :ColumnForm[Some[T]] =
		ColumnForm[T].nullBimap(Some.apply)(_.get)

}
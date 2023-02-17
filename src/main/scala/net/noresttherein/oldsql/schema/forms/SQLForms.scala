package net.noresttherein.oldsql.schema.forms

import java.sql.{JDBCType, PreparedStatement, ResultSet}
import java.util.Optional

import net.noresttherein.oldsql.collection.{Chain, ChainMap, LabeledChain, Listing, Opt, PassedArray, Record}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.ChainMap.&~
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.LabeledChain.>~
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.model.:*:
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnForm.{AbstractColumnForm, JDBCForm, NullSafeColumnForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.{DirectColumnWriteForm, SingletonColumnWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.{ReadFormAdapter, ReadFormNullValue}
import net.noresttherein.oldsql.schema.SQLWriteForm.{WriteFormAdapter, WriteFormLiterals, WriteFormSeparateLiterals}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.forms.SQLForms.{ChainFormExtension, ChainMapEntryReadForm, ChainMapEntryWriteForm, ChainMapForm, ChainReadFormExtension, ChainSQLForm, ChainWriteFormExtension, EmptyChainForm, ListingForm, ListingFormExtension, ListingItemReadForm, ListingItemWriteForm}
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.UnspecifiedColumnFormAdapter
import net.noresttherein.oldsql.slang

//implicits
import slang._





/** Brings into implicit search scope standard [[net.noresttherein.oldsql.schema.SQLForm SQLForm]],
  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]
  * and [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]] implicits from the companion object.
  * All column form implicit definitions have precedence over all non-column forms, and bidirectional forms
  * have precedence over solely read/write forms.
  */
trait SQLForms




private[schema] sealed trait Rank2SQLRWFormsImplicits {

	/** Provides an implicit form for the heterogeneous list (`Chain`) `I ~ L` as long as implicit forms for both
	  * `I` and `L` are available. */
	implicit def ChainReadForm[I <: Chain, L](implicit i :SQLReadForm[I], l :SQLReadForm[L]) :SQLReadForm[I ~ L] =
		new UnspecifiedChainForm[SQLReadForm[I], SQLReadForm[L]](i, l) with ChainReadFormImpl[I, L]

	implicit def ChainWriteForm[I <: Chain, L](implicit t :SQLWriteForm[I], h :SQLWriteForm[L]) :SQLWriteForm[I ~ L] =
		new UnspecifiedChainForm[SQLWriteForm[I], SQLWriteForm[L]](t, h) with ChainWriteFormImpl[~, I, L]


	implicit def readFormChainToChainReadForm[P <: Chain, I <: Chain, L]
	             (forms :P ~ SQLReadForm[L])(implicit prefix :P => SQLReadForm[I]) :SQLReadForm[I ~ L] =
		ChainReadForm(prefix(forms.init), forms.last)

	implicit def writeFormChainToChainWriteForm[P <: Chain, I <: Chain, L]
	             (chain :P ~ SQLWriteForm[L])(implicit prefix :P => SQLWriteForm[I]) :SQLWriteForm[I ~ L] =
		ChainWriteForm(prefix(chain.init), chain.last)


	implicit def ChainReadFormExtension[C <: Chain](self :SQLReadForm[C]) = new ChainReadFormExtension[C](self)

	implicit def ChainWriteFormExtension[C <: Chain](self :SQLWriteForm[C]) = new ChainWriteFormExtension[C](self)
}




private[forms] sealed trait Rank1SQLRWFormsImplicits extends Rank2SQLRWFormsImplicits {

	/** Provides an implicit form for the heterogeneous map indexed by literal types (`ChainMap`) `I &~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def ChainMapReadForm[I <: ChainMap :SQLReadForm, K <: ChainMap.Key :ValueOf, V :SQLReadForm]
			:SQLReadForm[I &~ (K, V)] =
		new AbstractListingReadForm[&~, Tuple2, Singleton, I, K, V](ChainMapEntryReadForm[K, V]) {
			protected[this] override def cons(init :I, value :V) = init &~ (key -> value)
			protected override def symbol = "&~"
		}

	implicit def readFormChainMapToChainMapReadForm[P <: ChainMap, I <: ChainMap, K <: ChainMap.Key, V]
	            (forms :P &~ (K, SQLReadForm[V]))(implicit prefix :P => SQLReadForm[I]) :SQLReadForm[I &~ (K, V)] =
		ChainMapReadForm(prefix(forms.init), new ValueOf(forms.last._1), forms.last._2)

	implicit def ChainMapWriteForm[I <: ChainMap :SQLWriteForm, K <: ChainMap.Key, V :SQLWriteForm]
			:SQLWriteForm[I &~ (K, V)] =
		new ChainSQLWriteForm(SQLWriteForm[I], ChainMapEntryWriteForm[K, V], "&~")

	implicit def writeFormChainMapToChainMapWriteForm[P <: ChainMap, I <: ChainMap, K <: ChainMap.Key, V]
	             (chain :P &~ (K, SQLWriteForm[V]))(implicit prefix :P => SQLWriteForm[I]) :SQLWriteForm[I &~ (K, V)] =
		ChainMapWriteForm(prefix(chain.init), chain.last._2)

	implicit class ChainMapReadFormExtension[I <: ChainMap](private val self :SQLReadForm[I]) {
		def &~[K <: ChainMap.Key, V](item :(K, SQLReadForm[V])) :SQLReadForm[I &~ (K, V)] =
			ChainMapReadForm(self, new ValueOf[K](item._1), item._2)

		def &~[K <: ChainMap.Key :ValueOf, V](value :SQLReadForm[V]) :SQLReadForm[I &~ (K, V)] =
			ChainMapReadForm(self, implicitly[ValueOf[K]], value)
	}

	implicit class ChainMapWriteFormExtension[I <: ChainMap](private val self :SQLWriteForm[I]) {
		def &~[K <: ChainMap.Key, V](item :(K, SQLWriteForm[V])) :SQLWriteForm[I &~ (K, V)] =
			ChainMapWriteForm(self, item._2)

		def &~[K <: ChainMap.Key, V](value :SQLWriteForm[V]) :SQLWriteForm[I &~ (K, V)] =
			ChainMapWriteForm(self, value)
	}


	/** Provides an implicit form for the heterogeneous map indexed by types (`Listing`) `I |~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def ListingReadForm[I <: Listing :SQLReadForm, K <: Listing.Key :ValueOf, V :SQLReadForm]
			:SQLReadForm[I |~ (K :~ V)] =
		new AbstractListingReadForm[|~, :~, Listing.Key, I, K, V](ListingItemReadForm[K, V]) {
			protected[this] override def cons(init :I, value :V) = init |~ key :~ value
			protected override def symbol = "|~"
		}

	implicit def readFormListingToListingReadForm[P <: Listing, I <: Listing, K <: Listing.Key, V]
	             (forms :P |~ (K :~ SQLReadForm[V]))(implicit prefix :P => SQLReadForm[I], key :ValueOf[K])
			:SQLReadForm[I |~ (K :~ V)] =
		ListingReadForm(prefix(forms.init), key, forms.last.value)

	implicit def ListingWriteForm[I <: Listing :SQLWriteForm, K <: Listing.Key, V :SQLWriteForm]
			:SQLWriteForm[I |~ (K :~ V)] =
		new ChainSQLWriteForm(SQLWriteForm[I], ListingItemWriteForm[K, V], "|~")

	implicit def writeFormListingToListingWriteForm[P <: Listing, I <: Listing, K <: Listing.Key, V]
	             (chain :P |~ (K :~ SQLWriteForm[V]))(implicit prefix :P => SQLWriteForm[I]) :SQLWriteForm[I |~ (K :~ V)] =
		ListingWriteForm(prefix(chain.init), chain.last.value)

	implicit class ListingReadFormExtension[I <: Listing](private val self :SQLReadForm[I]) {
		def |~[K <: Listing.Key :ValueOf, V](item :K :~ SQLReadForm[V]) :SQLReadForm[I |~ (K :~ V)] =
			ListingReadForm(self, implicitly[ValueOf[K]], item.value)
	}

	implicit class ListingWriteFormExtension[I <: Listing](private val self :SQLWriteForm[I]) {
		def |~[K <: Listing.Key, V](item :K :~ SQLWriteForm[V]) :SQLWriteForm[I |~ (K :~ V)] =
			ListingWriteForm(self, item.value)
	}

}




private[forms] sealed trait SQLRWFormsImplicits extends Rank1SQLRWFormsImplicits {

	implicit def OptionReadForm[T :SQLReadForm] :SQLReadForm[Option[T]] =
		new OptionReadForm[T] { override val form = SQLReadForm[T] }

	implicit def OptionWriteForm[T :SQLWriteForm] :SQLWriteForm[Option[T]] =
		new OptionWriteForm[T] { override val form = SQLWriteForm[T] }


	implicit def OptReadForm[T :SQLReadForm] :SQLReadForm[Opt[T]] =
		new OptReadForm[T] {
			override val form = SQLReadForm[T]
			override def apply(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position)
			override val toString = super.toString
		}

	implicit def OptWriteForm[T :SQLWriteForm] :SQLWriteForm[Opt[T]] =
		new OptWriteForm[T] { //these methods can't be extracted out as there is a clash between them and their inherited
			override val form = SQLWriteForm[T] //erased signatures which for some reason doesn't exist for anonymous classes

			override def set(statement :PreparedStatement, position :Int, value :Opt[T]) :Unit =
				form.setOpt(statement, position, value)

			override def literal(value :Opt[T], inline :Boolean) = value match {
				case Got(x) => form.literal(x, inline)
				case _ => form.nullLiteral(inline)
			}
			override def columnLiterals(value :Opt[T]) = value match {
				case Got(x) => form.columnLiterals(x)
				case _ => form.nullColumnLiterals
			}
		}


	implicit def OptionalReadForm[T :SQLReadForm] :SQLReadForm[Optional[T]] = //takes advantage of NullValue[Optional[T]]
		SQLReadForm.map("Optional>")(Optional.of[T](_))

	implicit def OptionalWriteForm[T :SQLWriteForm] :SQLWriteForm[Optional[T]] =
		SQLWriteForm.optMap("<Optional")(optionalToOption _)

	protected[forms] final def flatMapToOptional[T](t :T) :Option[Optional[T]] =
		Some(Optional.of(t))

	protected[forms] final def optionalToOption[T](opt :Optional[T]) :Option[T] =
		if (opt.isPresent) Some(opt.get) else None


	private[forms] trait OptionReadForm[T] extends ReadFormAdapter[Option[T]] {
		protected override def form :SQLReadForm[T]

		override def apply(res :ResultSet, position :Int) :Option[T] = form.opt(res, position).toOption
		override def opt(res :ResultSet, position :Int) :Opt[Option[T]] = Got(form.opt(res, position).toOption)
		override def nullValue :Option[T] = None

		override def equals(that :Any) :Boolean = that match  {
			case self :AnyRef if self eq this => true
			case opt :OptionReadForm[_] if opt canEqual this => form == opt.form
			case _ => false
		}
		override def hashCode :Int = form.hashCode

		protected lazy val cachedString = "Option[" + form + "]>"
		override def toString :String = cachedString
	}


	private[forms] trait OptionWriteForm[-T] extends WriteFormLiterals[Option[T]] with WriteFormAdapter[Option[T]] {
		protected override def form :SQLWriteForm[T]

		override def set(statement :PreparedStatement, position :Int, value :Option[T]) :Unit =
			if (value.isDefined) form.set(statement, position, value.get)
			else form.setNull(statement, position)

		override def literal(value :Option[T], inline :Boolean) :String = value match {
			case Some(x) => form.literal(x, inline)
			case _ => form.nullLiteral(inline)
		}
		override def columnLiterals(value :Option[T]) :Seq[String] = value match {
			case Some(x) => form.columnLiterals(x)
			case _ => form.nullColumnLiterals
		}
		override def split :Seq[ColumnWriteForm[Option[T]]] = form.split.map(_.toOpt)

		override def equals(that :Any) :Boolean = that match {
			case opt :OptionWriteForm[_] => (this eq opt) || opt.canEqual(this) && form == opt.form
			case _ => false
		}
		override def hashCode :Int = form.hashCode

		protected lazy val cachedString = "<Option[" + form + "]"
		override def toString :String = cachedString
	}


	//apply and nullValue methods not implemented here, as their erased forms clashes with their erased bridges
	private[forms] trait OptReadForm[T] extends ReadFormAdapter[Opt[T]] with ReadFormNullValue[Opt[T]] {
		override val nulls = NullValue.Lack
		protected override def form :SQLReadForm[T]

		override def opt(res :ResultSet, position :Int) :Opt[Opt[T]] = Got(form.opt(res, position))

		override def equals(that :Any) :Boolean = that match  {
			case self :AnyRef if self eq this => true
			case opt :OptReadForm[_] if opt canEqual this => opt.form == form
			case _ => false
		}
		override def hashCode :Int = form.hashCode

		private[forms] lazy val cachedString = "Opt[" + form + "]>"
		override def toString :String = cachedString
	}


	//set and literal methods not implemented here, as their erased forms clashes with their erased bridges
	private[forms] trait OptWriteForm[-T] extends WriteFormAdapter[Opt[T]] {
		protected override def form :SQLWriteForm[T]

		override def nullSafe :SQLWriteForm[Opt[T]] = this

		override def split = form.split.map(SQLForms.OptColumnWriteForm(_))

		override def equals(that :Any) :Boolean = that match {
			case opt :OptWriteForm[_] => (this eq opt) || opt.canEqual(this) && opt.form == form
			case _ => false
		}
		override def hashCode :Int = form.hashCode

		private[forms] lazy val cachedString = "<Opt[" + form + "]"
		override def toString :String = cachedString
	}




	implicit def pairReadForm[A :SQLReadForm, B :SQLReadForm] :SQLReadForm[A :*: B] =
		new PairBaseForm(SQLReadForm[A], SQLReadForm[B]) with PairReadForm[A, B]

	implicit def pairWriteForm[A :SQLWriteForm, B :SQLWriteForm] :SQLWriteForm[A :*: B] =
		new PairBaseForm(SQLWriteForm[A], SQLWriteForm[B]) with PairWriteForm[A, B]


	private[forms] sealed abstract class PairBaseForm[+A <: UnspecifiedForm, +B <: UnspecifiedForm]
	                                                 (protected val first :A, protected val second :B)
		extends UnspecifiedForm
	{
		override lazy val columnTypes = first.columnTypes :++ second.columnTypes

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :PairBaseForm[_, _] if other.getClass == getClass =>
				other.first == first && other.second == second
			case _ => false
		}
		override def hashCode :Int = first.hashCode * 31 + second.hashCode

		override lazy val toString :String = first.toString + ":*:" + second
	}


	private[forms] sealed trait PairReadForm[A, B]
		extends PairBaseForm[SQLReadForm[A], SQLReadForm[B]] with SQLReadForm[A :*: B]
	{
		override val columnCount = first.columnCount + second.columnCount
		override lazy val columnTypes = first.columnTypes :++ second.columnTypes
		override def isUniversal = first.isUniversal && second.isUniversal

		override def apply(res :ResultSet, position :Int) :A :*: B =
			:*:(first(res, position), second(res, position + first.columnCount))

		override def opt(res :ResultSet, position :Int) :Opt[A :*: B] =
			first.opt(res, position) match {
				case Lack => Lack
				case got1 => second.opt(res, position + first.columnCount) match {
					case Lack => Lack
					case got2 => Got(:*:(got1.get, got2.get))
				}
			}

		private val nullPair =
			try { :*:(first.nullValue, second.nullValue) }
			catch { case _ :Exception => null }

		override def nullValue :A :*: B =
			if (nullPair == null) :*:(first.nullValue, second.nullValue) else nullPair

		override def comparable(other :SQLReadForm[_]) :Boolean = other match {
			case self :AnyRef if this eq self => true
			case other :PairReadForm[_, _] =>
				(first comparable other.first) && (second comparable other.second) || super.comparable(other)
			case _ => super.comparable(other)
		}
	}


	private[forms] sealed trait PairWriteForm[A, B]
		extends PairBaseForm[SQLWriteForm[A], SQLWriteForm[B]] with WriteFormSeparateLiterals[A :*: B]
	{
		override val columnCount = first.columnCount + second.columnCount
		override def isUniversal = first.isUniversal && second.isUniversal

		override def set(statement :PreparedStatement, position :Int, value :A :*: B) :Unit =
			if (value == null) setNull(statement, position)
			else {
				first.set(statement, position, value._1)
				second.set(statement, position + first.columnCount, value._2)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			first.setNull(statement, position)
			second.setNull(statement, position + first.columnCount)
		}

		override def literal(value :A :*: B) = "(" + first.inlineLiteral(value._1) + ", " + second.inlineLiteral(value._2) + ")"
		override def inlineLiteral(value :A :*: B) = first.inlineLiteral(value._1) + ", " + second.inlineLiteral(value._2)
		override def columnLiterals(value :A :*: B) = first.columnLiterals(value._1) ++ second.columnLiterals(value._2)
		override lazy val nullLiteral = "(" + first.nullLiteral + ", " + second.nullLiteral + ")"
		override lazy val inlineNullLiteral = first.inlineNullLiteral + ", " + second.inlineNullLiteral
		override lazy val nullColumnLiterals = first.nullColumnLiterals ++ second.nullColumnLiterals

		override def split :Seq[ColumnWriteForm[A :*: B]] =
			(first.split.view.map(_.compose ((_:(A :*: B))._1)) ++
				second.split.view.map(_.compose ((_:(A :*: B))._2))).to(List)

		override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
			case self :AnyRef if this eq self => true
			case other :PairWriteForm[_, _] =>
				(first comparable other.first) && (second comparable other.second) || super.comparable(other)
			case _ => super.comparable(other)
		}
	}



	/** Provides an implicit form for the heterogeneous map indexed by string literal types (`LabeledChain`) `I >~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def LabeledChainReadForm[I <: LabeledChain :SQLReadForm, K <: Label :ValueOf, V :SQLReadForm]
			:SQLReadForm[I >~ (K :~ V)] =
		new AbstractListingReadForm[>~, :~, Label, I, K, V](ListingItemReadForm[K, V]) {
			protected[this] override def cons(init :I, value :V) = init >~ key :~ value
			protected override def symbol = ">~"
		}

	implicit def readFormLabeledChainToLabeledChainReadForm[P <: LabeledChain, I <:LabeledChain, K <: Label, V]
	             (forms :P >~ (K :~ SQLReadForm[V]))(implicit prefix :P => SQLReadForm[I], key :ValueOf[K])
			:SQLReadForm[I >~ (K :~ V)] =
		LabeledChainReadForm(prefix(forms.init), key, forms.last.value)

	implicit def LabeledChainWriteForm[I <: LabeledChain :SQLWriteForm, K <: Label, V :SQLWriteForm]
			:SQLWriteForm[I >~ (K :~ V)] =
		new ChainSQLWriteForm(SQLWriteForm[I], ListingItemWriteForm[K, V], ">~")

	implicit def writeFormLabeledChainToLabeledChainWriteForm[P <: LabeledChain, I <: LabeledChain, K <: Label, V]
	             (chain :P >~ (K :~ SQLWriteForm[V]))(implicit prefix :P => SQLWriteForm[I]) :SQLWriteForm[I >~ (K :~ V)] =
		LabeledChainWriteForm(prefix(chain.init), chain.last.value)

	implicit class LabeledChainReadFormExtension[I <: LabeledChain](private val self :SQLReadForm[I]) {
		def >~[K <: LabeledChain.Key :ValueOf, V](item :K :~ SQLReadForm[V]) :SQLReadForm[I >~ (K :~ V)] =
			LabeledChainReadForm(self, implicitly[ValueOf[K]], item.value)
	}

	implicit class LabeledChainWriteFormExtension[I <: LabeledChain](private val self :SQLWriteForm[I]) {
		def >~[K <: LabeledChain.Key, V](item :(K, SQLWriteForm[V])) :SQLWriteForm[I >~ (K :~ V)] =
			LabeledChainWriteForm(self, item._2)
	}


	/** Provides an implicit form for the heterogeneous map indexed by string literals (`Record`) `I |# L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def RecordReadForm[I <: Record :SQLReadForm, K <: Label :ValueOf, V :SQLReadForm]
			:SQLReadForm[I |# (K, V)] =
		new AbstractListingReadForm[|#, Tuple2, Label, I, K, V](ChainMapEntryReadForm[K, V]) {
			protected[this] override def cons(init :I, value :V) = init |# (key -> value)
			protected override def symbol = "|#"
		}

	implicit def readFormRecordToRecordReadForm[I <: Record, K <: Label, V]
	             (forms :I |# (K, SQLReadForm[V]))(implicit prefix :I => SQLReadForm[I]) :SQLReadForm[I |# (K, V)] =
		RecordReadForm(prefix(forms.init), new ValueOf[K](forms.last._1), forms.last._2)

	implicit def RecordWriteForm[I <: Record :SQLWriteForm, K <: Label, V :SQLWriteForm]
			:SQLWriteForm[I |# (K, V)] =
		new ChainSQLWriteForm(SQLWriteForm[I], ChainMapEntryWriteForm[K, V], "|#")

	implicit def writeFormRecordToRecordWriteForm[I <: Record, K <: Label, V]
	             (chain :I |# (K, SQLWriteForm[V]))(implicit prefix :I => SQLWriteForm[I]) :SQLWriteForm[I |# (K, V)] =
		RecordWriteForm(prefix(chain.init), chain.last._2)

	implicit class RecordReadFormExtension[I <: Record](private val self :SQLReadForm[I]) {
		def |#[K <: Record.Key, V](item :(K, SQLReadForm[V])) :SQLReadForm[I |# (K, V)] =
			RecordReadForm(self, new ValueOf[K](item._1), item._2)

		def |#[K <: Record.Key :ValueOf, V](value :SQLReadForm[V]) :SQLReadForm[I |# (K, V)] =
			RecordReadForm(self, implicitly[ValueOf[K]], value)
	}

	implicit class RecordWriteFormExtension[I <: Record](private val self :SQLWriteForm[I]) {
		def |#[K <: Record.Key, V](item :(K, SQLWriteForm[V])) :SQLWriteForm[I |# (K, V)] =
			RecordWriteForm(self, item._2)

		def |#[K <: Record.Key, V](value :SQLWriteForm[V]) :SQLWriteForm[I |# (K, V)] =
			RecordWriteForm(self, value)
	}



	implicit def ListingItemReadForm[K <: Listing.Key :ValueOf, V :SQLReadForm] :SQLReadForm[K :~ V] =
		SQLReadForm.map(valueOf[K].toString + ":~") { :~[K](_:V) }

	implicit def ListingItemWriteForm[K <: Listing.Key, V :SQLWriteForm] :SQLWriteForm[K :~ V] =
		SQLWriteForm(s":~") { e :(K :~ V) => e.value }

	private[forms] def ChainMapEntryReadForm[K <: ChainMap.Key :ValueOf, V :SQLReadForm] :SQLReadForm[(K, V)] =
		SQLReadForm.map(valueOf[K].toString + "->") { v :V => valueOf[K] -> v }

	private[forms] def ChainMapEntryWriteForm[K <: ChainMap.Key, V :SQLWriteForm] :SQLWriteForm[(K, V)] =
		SQLWriteForm.map("?->") { e :(K, V) => e._2 }

}






private[forms] sealed trait Rank2SQLFormImplicits extends SQLRWFormsImplicits {
	implicit def chainForm[I <: Chain, L](implicit i :SQLForm[I], l :SQLForm[L]) :SQLForm[I ~ L] =
		new ChainSQLForm(i, l)

	implicit def formChainToChainForm[I <: Chain, L](chain :I ~ SQLForm[L])(implicit prefix :I => ChainForm[I])
			:ChainForm[I ~ L] =
		ChainForm(prefix(chain.init), chain.last)

	implicit def ChainFormExtension[C <: Chain](self :SQLForm[C]) = new ChainFormExtension[C](self)

	implicit def ListingFormExtension[I <: Listing](self :SQLForm[I]) = new ListingFormExtension[I](self)

}




private[forms] sealed trait Rank1SQLFormImplicits extends Rank2SQLFormImplicits {
	implicit def ChainMapForm[I <: ChainMap :SQLForm, K <: ChainMap.Key :ValueOf, V :SQLForm] :SQLForm[I &~ (K, V)] =
		new ChainMapForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def formChainMapToChainMapForm[I <: ChainMap, K <: ChainMap.Key, V]
	             (chain :I &~ (K, SQLForm[V]))(implicit prefix :I => SQLForm[I]) :SQLForm[I &~ (K, V)] =
		ChainMapForm(prefix(chain.init), new ValueOf[K](chain.last._1), chain.last._2)

	implicit class ChainMapFormExtension[I <: ChainMap](private val self :SQLForm[I]) {
		def &~[K <: ChainMap.Key, V](item :(K, SQLForm[V])) :SQLForm[I &~ (K, V)] =
			new ChainMapForm(self, item._1, item._2)

		def &~[K <: ChainMap.Key :ValueOf, V](value :SQLForm[V]) :SQLForm[I &~ (K, V)] =
			new ChainMapForm(self, valueOf[K], value)
	}


	implicit def ListingForm[I <: Listing :SQLForm, K <: Listing.Key :ValueOf, V :SQLForm]
			:SQLForm[I |~ (K :~ V)] =
		new ListingForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def formListingToListingForm[I <: Listing, K <: Listing.Key, V]
	             (chain :I |~ (K:~SQLForm[V]))(implicit prefix :I => SQLForm[I], key :ValueOf[K]) :SQLForm[I|~(K:~V)] =
		ListingForm(prefix(chain.init), key, chain.last.value)

}




private[forms] sealed trait SQLFormImplicits extends Rank1SQLFormImplicits {

	implicit def OptionForm[T :SQLForm] :SQLForm[Option[T]] = new OptionForm[T]

	private[forms] class OptionForm[T](implicit protected override val form :SQLForm[T])
		extends OptionWriteForm[T] with OptionReadForm[T] with SQLForm[Option[T]]
	{
		override lazy val cachedString :String = "Option[" + form + "]"
	}


	implicit def OptForm[T :SQLForm] :SQLForm[Opt[T]] =
		new SQLForm[Opt[T]] with OptReadForm[T] with OptWriteForm[T] {
			override val form = SQLForm[T]

			override def apply(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position)

			override def set(statement :PreparedStatement, position :Int, value :Opt[T]) :Unit = value match {
				case Got(x) => form.set(statement, position, x)
				case _ => form.setNull(statement, position)
			}
			override def literal(value :Opt[T], inline :Boolean) :String = value match {
				case Got(t) => form.literal(t, inline)
				case _ => form.nullLiteral(inline)
			}
			override def columnLiterals(value :Opt[T]) = value match {
				case Got(t) => form.columnLiterals(t)
				case _ => form.nullColumnLiterals
			}
			override def nullSafe = this

			override lazy val cachedString = "Opt[" + form + "]"
		}

	implicit def OptionalForm[T :SQLForm] :SQLForm[Optional[T]] =
		SQLForm.optMap("Optional")(SQLForms.flatMapToOptional[T])(SQLForms.optionalToOption)



	implicit def pairForm[A :SQLForm, B :SQLForm] :SQLForm[A :*: B] =
		new PairBaseForm[SQLForm[A], SQLForm[B]](SQLForm[A], SQLForm[B])
			with PairReadForm[A, B] with PairWriteForm[A, B] with SQLForm[A :*: B]



	/** An empty chain form for the `Chain` terminator `@~`, which doesn't write or read any columns. */
	implicit val EmptyChainForm :SQLForm[@~] = ChainForm //SQLForm.empty(@~, "@~")
//	implicit object EmptyChainForm


	implicit def LabeledChainForm[I <: LabeledChain :SQLForm, K <: Label :ValueOf, V :SQLForm] :SQLForm[I >~ (K :~ V)] =
		new LabeledChainForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def formLabeledChainToLabeledChainForm[I <: LabeledChain, K <: Label, V]
	             (chain :I >~ (K:~SQLForm[V]))(implicit prefix :I => SQLForm[I], key :ValueOf[K]) :SQLForm[I>~(K:~V)] =
		LabeledChainForm(prefix(chain.init), key, chain.last.value)

	implicit class LabeledChainFormExtension[I <: LabeledChain](private val self :SQLForm[I]) {
		def >~[K <: LabeledChain.Key :ValueOf, V](item :K :~ SQLForm[V]) :SQLForm[I >~ (K :~ V)] =
			new LabeledChainForm(self, valueOf[K], item.value)
	}


	implicit def RecordForm[I <: Record :SQLForm, K <: Label :ValueOf, V :SQLForm] :SQLForm[I |# (K, V)] =
		new RecordForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def formRecordToRecordForm[I <: Record, K <: Record.Key, V]
	             (chain :I |# (K, SQLForm[V]))(implicit prefix :I => SQLForm[I]) :SQLForm[I |# (K, V)] =
		RecordForm(prefix(chain.init), new ValueOf[K](chain.last._1), chain.last._2)

	implicit class RecordFormExtension[I <: Record](private val self :SQLForm[I]) {
		def |#[K <: Record.Key, V](item :(K, SQLForm[V])) :SQLForm[I |# (K, V)] =
			RecordForm(self, new ValueOf[K](item._1), item._2)

		def |#[K <: Record.Key :ValueOf, V](value :SQLForm[V]) :SQLForm[I |# (K, V)] =
			RecordForm(self, implicitly[ValueOf[K]], value)
	}


	implicit def ListingItemForm[K <: Listing.Key :ValueOf, V :SQLForm] :SQLForm[K :~ V] =
		SQLForm.map(valueOf[K].toString + ":~")(:~[K](_:V))(_.value)

	private[forms] def ChainMapEntryForm[K <: ChainMap.Key :ValueOf, V :SQLForm] :SQLForm[(K, V)] =
		SQLForm.map(valueOf[K].toString + "->")((v :V) => valueOf[K] -> v)(_._2)



	private[forms] class ChainSQLForm[I <: Chain, L](override val init :SQLForm[I], override val last :SQLForm[L])
		extends UnspecifiedChainForm[SQLForm[I], SQLForm[L]](init, last)
		   with ChainWriteFormImpl[~, I, L] with ChainReadFormImpl[I, L] with SQLForm[I ~ L]

	private[forms] class ListingForm[I <: Listing, K <: Listing.Key, V]
	                     (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends UnspecifiedChainForm[SQLForm[I], SQLForm[K:~V]](init, ListingItemForm(new ValueOf[K](key), value))
		   with ChainWriteFormImpl[|~, I, K:~V] with ChainIndexReadFormImpl[|~, :~, Listing.Key, I, K, V]
		   with SQLForm[I |~ (K :~ V)]
	{
		override def symbol = "|~"
		protected[this] override def cons(init :I, value :V) :I |~ (K :~ V) = init |~ (key :~ value)
	}

	private[forms] class LabeledChainForm[I <: LabeledChain, K <: LabeledChain.Key, V]
	                     (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends UnspecifiedChainForm[SQLForm[I], SQLForm[K:~V]](init, ListingItemForm(new ValueOf[K](key), value))
		   with ChainWriteFormImpl[>~, I, K :~ V] with ChainIndexReadFormImpl[>~, :~, LabeledChain.Key, I, K, V]
		   with SQLForm[I >~ (K :~ V)]
	{
		protected override def symbol = ">~"
		protected[this] override def cons(init :I, value :V) :I >~ (K :~ V) = init >~ (key :~ value)
	}


	private[forms] class ChainMapForm[I <: ChainMap, K <: ChainMap.Key, V]
	                     (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends UnspecifiedChainForm[SQLForm[I], SQLForm[(K, V)]](init, ChainMapEntryForm(new ValueOf[K](key), value))
		   with UnspecifiedChainReadForm[SQLForm[I], SQLForm[(K, V)]]
		   with UnspecifiedChainWriteForm[SQLForm[I], SQLForm[(K, V)]]
		   with ChainWriteFormImpl[&~, I, (K, V)] with ChainIndexReadFormImpl[&~, Tuple2, ChainMap.Key, I, K, V]
		   with SQLForm[I &~ (K, V)]
	{
		protected override def symbol = "&~"
		protected[this] override def cons(init :I, value :V) :I &~ (K, V) = init &~ (key -> value)
	}


	private[forms] class RecordForm[I <: Record, K <: Record.Key, V]
	                     (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends UnspecifiedChainForm[SQLForm[I], SQLForm[(K, V)]](init, ChainMapEntryForm(new ValueOf[K](key), value))
		   with UnspecifiedChainReadForm[SQLForm[I], SQLForm[(K, V)]]
		   with UnspecifiedChainWriteForm[SQLForm[I], SQLForm[(K, V)]]
		   with ChainWriteFormImpl[|#, I, (K, V)] with ChainIndexReadFormImpl[|#, Tuple2, Record.Key, I, K, V]
	       with SQLForm[I |# (K, V)]
	{
		protected override def symbol = "|#"
		protected[this] override def cons(init :I, value :V) :I |# (K, V) = init |# key -> value
	}

}






private[forms] sealed trait ColumnRWFormsImplicits extends SQLFormImplicits {

	implicit def OptionColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Option[T]] =
		new OptionReadForm[T] with ColumnReadForm[Option[T]] with UnspecifiedColumnFormAdapter {
			override val form = ColumnReadForm[T]
		}

	implicit def OptionColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Option[T]] =
		new OptionWriteForm[T] with SingletonColumnWriteForm[Option[T]] with UnspecifiedColumnFormAdapter {
			override val form :ColumnWriteForm[T] = ColumnWriteForm[T]
		}


	implicit def OptColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Opt[T]] =
		new OptReadForm[T] with ColumnReadForm[Opt[T]] with UnspecifiedColumnFormAdapter {
			override val form = ColumnReadForm[T]
			override def apply(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position)
			override val toString = super.toString
		}

	implicit def OptColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Opt[T]] =
		new OptWriteForm[T] with SingletonColumnWriteForm[Opt[T]] with UnspecifiedColumnFormAdapter {
			override val form = ColumnWriteForm[T]

			override def set(statement :PreparedStatement, position :Int, value :Opt[T]) :Unit = value match {
				case Got(x) => form.set(statement, position, x)
				case _ => form.setNull(statement, position)
			}

			override def literal(value :Opt[T], inline :Boolean) = value match {
				case Got(x) => form.literal(x, inline)
				case _ => form.nullLiteral(inline)
			}
			override def columnLiterals(value :Opt[T]) = value match {
				case Got(x) => form.columnLiterals(x)
				case _ => form.nullColumnLiterals
			}
		}


	implicit def OptionalColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Optional[T]] =
		ColumnReadForm.map("Optional>")(Optional.of[T])

	implicit def OptionalColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Optional[T]] =
		ColumnWriteForm.optMap("<Optional")(optionalToOption)
}






private[forms] sealed trait ColumnFormImplicits extends ColumnRWFormsImplicits {

	/** Lifts the implicit `ColumnForm[T]` implementation to `ColumnForm[Option[T]]`. */
	implicit def OptionColumnForm[T :ColumnForm] :ColumnForm[Option[T]] =
		new OptionForm[T] with ColumnForm[Option[T]]
			with SingletonColumnWriteForm[Option[T]] with UnspecifiedColumnFormAdapter
		{
			override val form = ColumnForm[T]
		}

	implicit def OptColumnForm[T :ColumnForm] :ColumnForm[Opt[T]] =
		new OptWriteForm[T] with OptReadForm[T] with ColumnForm[Opt[T]]
			with UnspecifiedColumnFormAdapter with SingletonColumnWriteForm[Opt[T]]
		{
			override val form = ColumnForm[T]

			override def apply(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position)

			override def set(statement :PreparedStatement, position :Int, value :Opt[T]) :Unit = value match {
				case Got(x) => form.set(statement, position, x)
				case _ => form.setNull(statement, position)
			}

			override def literal(value :Opt[T], inline :Boolean) = value match {
				case Got(x) => form.literal(x, inline)
				case _ => form.nullLiteral(inline)
			}
			override def columnLiterals(value :Opt[T]) = value match {
				case Got(x) => form.columnLiterals(x)
				case _ => form.nullColumnLiterals
			}

			override lazy val cachedString :String = "Opt[" + form + "]"
		}

	implicit def OptionalColumnForm[T :ColumnForm] :ColumnForm[Optional[T]] =
		ColumnForm.optMap("Optional")(flatMapToOptional[T])(optionalToOption)

}






object SQLForms extends ColumnFormImplicits with BasicForms with ScalaForms {
	implicit final val UnitForm = SQLForm.empty("Unit")(())


	implicit object ClassForm
		extends AbstractColumnForm[Class[_]](JDBCType.VARCHAR)
		   with DirectColumnWriteForm[Class[_]] with NullSafeColumnForm[Class[_]]
	{
		override val columnTypes :Seq[JDBCType] = PassedArray :+ sqlType

		override def set(statement :PreparedStatement, position :Int, value :Class[_]) :Unit =
			if (value == null) setNull(statement, position) else statement.setString(position, value.getName)

		override def opt(res :ResultSet, position :Int) :Opt[Class[_]] = res.getString(position) match {
			case null => Lack
			case name => Got(Class.forName(name))
		}

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			statement.setNull(position, sqlType.getVendorTypeNumber)

		override def literal(value :Class[_]) :String =
			if (value == null) "null" else "'" + value.getName + "'"

		override def toString = "<Class>"
	}



	class ChainReadFormExtension[C <: Chain](private val self :SQLReadForm[C]) extends AnyVal {
		def ~[L](item :SQLReadForm[L]) :SQLReadForm[C ~ L] = ChainReadForm(self, item)
	}

	class ChainWriteFormExtension[C <: Chain](private val self :SQLWriteForm[C]) extends AnyVal {
		def ~[L](item :SQLWriteForm[L]) :SQLWriteForm[C ~ L] = ChainWriteForm(self, item)
	}

	class ChainFormExtension[C <: Chain](private val self :SQLForm[C]) extends AnyVal {
		def ~[L](item :SQLForm[L]) :SQLForm[C ~ L] = chainForm(self, item)
	}
	class ListingFormExtension[I <: Listing](private val self :SQLForm[I]) extends AnyVal {
		def |~[K :ValueOf, V](entry :K :~ SQLForm[V]) :SQLForm[I |~ (K :~ V)] =
			SQLForms.ListingForm(self, implicitly[ValueOf[K]], entry.value)
	}



	/** Base type of anonymous forms for natively supported JDBC types. Implements equality as equality of form classes
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null value]]. For this reason all subclasses should
	  * be dedicated to a single `jdbcType`.
	  */ //consider: using ReflectedJDBCForm
	private[forms] abstract class DedicatedJDBCForm[T :NullValue](jdbcType :JDBCType)
		extends JDBCForm[T](jdbcType) with SingletonColumnWriteForm[T] with NullSafeColumnForm[T]
	{
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :DedicatedJDBCForm[_] => getClass == other.getClass && nulls == other.nulls
			case _ => false
		}

		override def hashCode :Int = getClass.hashCode * 31 + nulls.hashCode
	}

	/** Base type of anonymous forms for those types natively supported by the driver, which are not default
	  * Scala types for the specified SQL type, for example Java variants of classes from Scala's standard library.
	  */
	private[forms] abstract class AlternateJDBCForm[T :NullValue](jdbcType :JDBCType, override val toString :String)
		extends DedicatedJDBCForm[T](jdbcType) with SingletonColumnWriteForm[T] with NullSafeColumnForm[T]
	{
		def this(jdbcType :JDBCType) = this(jdbcType, "J" + jdbcType)
	}

}

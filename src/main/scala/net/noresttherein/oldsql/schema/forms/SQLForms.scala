package net.noresttherein.oldsql.schema.forms

import java.sql.{CallableStatement, JDBCType, PreparedStatement, ResultSet}
import java.util.Optional

import net.noresttherein.oldsql.collection.{Chain, ChainMap, LabeledChain, Listing, Opt, Record}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.ChainMap.&~
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.LabeledChain.>~
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.morsels.{Lazy, Stateless}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.forms.SQLForms.{AbstractListingReadForm, ChainMapEntryReadForm, ChainMapEntryWriteForm, ChainMapForm, ChainReadForm, ChainSQLForm, ChainSQLWriteForm, ChainWriteForm, EmptyChainForm, ListingForm, ListingItemReadForm, ListingItemWriteForm, SuperAdapterColumnForm, SuperChainForm}
import net.noresttherein.oldsql.schema.ColumnForm.{JDBCForm, NullSafeColumnForm}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.{ReadFormAdapter, ReadFormNullValue}
import net.noresttherein.oldsql.schema.SQLWriteForm.{WriteFormAdapter, WriteFormLiterals}
import net.noresttherein.oldsql.slang

//implicits
import slang._





/** Brings into implicit search scope standard `SQLForm` implicits from the companion object.
  * These definitions have higher precedence than read and write forms for the same type.
  */
trait SQLForms






/** Base trait combining companion traits to objects containing implicit form definitions.
  * Extended by both read and write forms, it brings those implicits into the search scope for all form types.
  */
trait SuperSQLForm extends SQLForms with Serializable {

	def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def toString :String = this.innerClassName
}



/** Base trait extended by both read and write column forms. It brings generic implicit definitions for all forms
  * into the search scope and groups common properties of derived types.
  */
trait SuperColumnForm extends SuperSQLForm {
	/** The JDBC code for the underlying column type, as defined by constants in `java.sql.Types`. */
	def sqlType :JDBCType
}






sealed trait SQLRWFormsLevel2Implicits {

	/** Provides an implicit form for the heterogeneous list (`Chain`) `I ~ L` as long as implicit forms for both
	  * `I` and `L` are available. */
	implicit def ChainReadForm[I <: Chain, L](implicit i :SQLReadForm[I], l :SQLReadForm[L]) :SQLReadForm[I ~ L] =
		new SuperChainForm[SQLReadForm[I], SQLReadForm[L]](i, l) with ChainReadForm[I, L]

	implicit def ChainWriteForm[I <: Chain, L](implicit t :SQLWriteForm[I], h :SQLWriteForm[L]) :SQLWriteForm[I ~ L] =
		new SuperChainForm[SQLWriteForm[I], SQLWriteForm[L]](t, h) with ChainWriteForm[~, I, L] {
			override protected def symbol = "~"
		}

	
	implicit def readFormChainToChainReadForm[P <: Chain, I <: Chain, L]
	             (forms :P ~ SQLReadForm[L])(implicit prefix :P => SQLReadForm[I]) :SQLReadForm[I ~ L] =
		ChainReadForm(prefix(forms.init), forms.last)

	implicit def writeFormChainToChainWriteForm[P <: Chain, I <: Chain, L]
	            (chain :P ~ SQLWriteForm[L])(implicit prefix :P => SQLWriteForm[I]) :SQLWriteForm[I ~ L] =
		ChainWriteForm(prefix(chain.init), chain.last)

	
	implicit class ChainReadFormExtension[C <: Chain](private val self :SQLReadForm[C]) { //todo: AnyVal
		def ~[L](item :SQLReadForm[L]) :SQLReadForm[C ~ L] = ChainReadForm(self, item)
	}
	
	implicit class ChainWriteFormExtension[C <: Chain](private val self :SQLWriteForm[C]) {
		def ~[L](item :SQLWriteForm[L]) :SQLWriteForm[C ~ L] = ChainWriteForm(self, item)
	}
}




sealed trait SQLRWFormsLevel1Implicits extends SQLRWFormsLevel2Implicits {

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




sealed trait SQLRWFormsImplicits extends SQLRWFormsLevel1Implicits {

	implicit def OptionReadForm[T :SQLReadForm] :SQLReadForm[Option[T]] =
		new OptionReadForm[T] { override val form = SQLReadForm[T] }

	implicit def OptionWriteForm[T :SQLWriteForm] :SQLWriteForm[Option[T]] =
		new OptionWriteForm[T] { val form :SQLWriteForm[T] = SQLWriteForm[T] }


	implicit def OptReadForm[T :SQLReadForm] :SQLReadForm[Opt[T]] =
		new OptReadForm[T] {
			override val form = SQLReadForm[T]
			override def apply(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position)
			override val toString = super.toString
		}

	implicit def OptWriteForm[T :SQLWriteForm] :SQLWriteForm[Opt[T]] =
		new OptWriteForm[T] { //these methods can't be extracted out as there is a clash between them and their inherited
			override val form = SQLWriteForm[T] //erased signatures which for some reason doesn't exist for anonymous classes

			override def set(statement :PreparedStatement, position :Int, value :Opt[T]) :Unit = value match {
				case Got(x) => form.set(statement, position, x)
				case _ => form.setNull(statement, position)
			}

			override def literal(value :Opt[T], inline :Boolean) = value match {
				case Got(x) => form.literal(x, inline)
				case _ => form.nullLiteral(inline)
			}

			override val toString = super.toString
		}


	implicit def OptionalReadForm[T :SQLReadForm] :SQLReadForm[Optional[T]] = //takes advantage of NullValue[Optional[T]]
		SQLReadForm.map("Optional[" + SQLReadForm[T] + "]>")(Optional.of[T](_))

	implicit def OptionalWriteForm[T :SQLWriteForm] :SQLWriteForm[Optional[T]] =
		SQLWriteForm.flatMap("<Optional[" + SQLWriteForm[T] + "]")(optionalToOption _)

	protected[forms] final def flatMapToOptional[T](t :T) :Option[Optional[T]] =
		Some(Optional.of(t))

	protected[forms] final def optionalToOption[T](opt :Optional[T]) :Option[T] =
		if (opt.isPresent) Some(opt.get) else None


	private[forms] trait OptionReadForm[T] extends ReadFormAdapter[Option[T]] {
		protected override def form :SQLReadForm[T]

		override def apply(res :ResultSet, position :Int) :Option[T] = form.opt(res, position)
		override def opt(res :ResultSet, position :Int) :Option[Option[T]] = Some(form.opt(res, position))
		override def nullValue :Option[T] = None

		override def equals(that :Any) :Boolean = that match  {
			case self :AnyRef if self eq this => true
			case opt :OptionReadForm[_] if opt canEqual this => opt.form == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override val toString :String = "Option[" + form + "]>"
	}




	private[forms] trait OptionWriteForm[-T] extends SQLWriteForm[Option[T]] {
		protected def form :SQLWriteForm[T]

		override def writtenColumns :Int = form.writtenColumns

		override def set(statement :PreparedStatement, position :Int, value :Option[T]) :Unit =
			form.setOpt(statement, position, value)

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.setNull(statement, position)

		override def literal(value :Option[T]) :String = value match {
			case Some(x) => form.literal(x)
			case _ => form.nullLiteral
		}
		override def nullLiteral :String = form.nullLiteral

		override def inlineLiteral(value :Option[T]) :String = value match {
			case Some(x) => form.inlineLiteral(x)
			case _ => form.inlineNullLiteral
		}
		override def inlineNullLiteral :String = form.inlineNullLiteral

		override def equals(that :Any) :Boolean = that match {
			case opt :OptionWriteForm[_] => (this eq opt) || opt.canEqual(this) && opt.form == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override val toString :String = "<Option[" + form + "]"
	}



	//apply and nullValue methods not implemented here, as their erased forms clashes with their erased bridges
	private[forms] trait OptReadForm[T] extends SQLReadForm[Opt[T]] with ReadFormNullValue[Opt[T]] {
		override val nulls = NullValue.Miss
		protected def form :SQLReadForm[T]
		override def readColumns :Int = form.readColumns

		override def opt(res :ResultSet, position :Int) :Option[Opt[T]] = Some(form.opt(res, position))

		override def equals(that :Any) :Boolean = that match  {
			case self :AnyRef if self eq this => true
			case opt :OptReadForm[_] if opt canEqual this => opt.form == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "Opt[" + form + "]>"
	}



	//set and literal methods not implemented here, as their erased forms clashes with their erased bridges
	private[forms] trait OptWriteForm[-T] extends WriteFormLiterals[Opt[T]] with WriteFormAdapter[Opt[T]] {
		protected override def form :SQLWriteForm[T]

		override def equals(that :Any) :Boolean = that match {
			case opt :OptWriteForm[_] => (this eq opt) || opt.canEqual(this) && opt.form == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "<Opt[" + form + "]"
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
		SQLReadForm.map(s"(${valueOf[K]}:~${SQLReadForm[V]}") { :~[K](_:V) }

	implicit def ListingItemWriteForm[K <: Listing.Key, V :SQLWriteForm] :SQLWriteForm[K :~ V] =
		SQLWriteForm(s"(:~${SQLWriteForm[V]})") { e :(K :~ V) => e.value }

	private[forms] def ChainMapEntryReadForm[K <: ChainMap.Key :ValueOf, V :SQLReadForm] :SQLReadForm[(K, V)] =
		SQLReadForm.map(s"(${valueOf[K]}->${SQLReadForm[V]})") { v :V => valueOf[K] -> v }

	private[forms] def ChainMapEntryWriteForm[K <: ChainMap.Key, V :SQLWriteForm] :SQLWriteForm[(K, V)] =
		SQLWriteForm.map(s"(,${SQLWriteForm[V]})") { e :(K, V) => e._2 }






	private[forms] abstract class SuperChainForm[+I <: SuperSQLForm, +L <: SuperSQLForm](val init :I, val last :L)
		extends SuperSQLForm
	{
		override def equals(that :Any) :Boolean = that match {
			case chain :SuperChainForm[_, _] =>
				(chain eq this) || (chain canEqual this) && chain.last == last && chain.init == init
			case _ => false
		}

		override def hashCode :Int = init.hashCode * 31 + last.hashCode


		protected def symbol :String

		private val string = Lazy {
			def rec(chain :SuperChainForm[_, _], res :StringBuilder) :StringBuilder = {
				chain.init match {
					case prefix :SuperChainForm[_, _] => rec(prefix, res)
					case EmptyChainForm => res ++= "@~"
					case other => res ++= other.toString
				}
				last match {
					case chain :SuperChainForm[_, _] => rec(chain, res ++= symbol += '(') += ')'
					case EmptyChainForm => res ++= symbol ++= "(@~)"
					case _ => res ++= symbol ++= last.toString
				}
			}
			rec(this, new StringBuilder).toString
		}

		override def toString = string
	}



	private[forms] trait ChainReadForm[+I <: Chain, +L]
		extends SuperChainForm[SQLReadForm[I], SQLReadForm[L]] with SQLReadForm[I ~ L]
	{   //this is fine because init and last are guaranteed to be initialized as constructor parameters
		override val readColumns :Int = init.readColumns + last.readColumns

		override def opt(res :ResultSet, position :Int) :Option[I ~ L] =
			for (t <- init.opt(res, position); h <- last.opt(res, position + init.readColumns)) yield t ~ h

		private[this] val nullChain :I ~ L =
			try { init.nullValue ~ last.nullValue }
			catch { case _ :Exception => null }

		override def nullValue :I ~ L =
			if (nullChain == null) init.nullValue ~ last.nullValue else nullChain

		override def register(call :CallableStatement, position :Int) :Unit = {
			init.register(call, position)
			last.register(call, position + init.readColumns)
		}


		override protected def symbol = "~"
	}



	private[forms] trait ChainIndexReadForm[C[+A <: I, +B <: E[K, V]] <: A ~ B,
	                                        E[+A <: U, +B], U, I <: Chain, K <: U, V]
		extends SuperChainForm[SQLReadForm[I], SQLReadForm[E[K, V]]] with SQLReadForm[I C E[K, V]]
	{
		protected val value :SQLReadForm[V]
		protected def key :K

		override val readColumns :Int = init.readColumns + last.readColumns

		protected[this] def cons(init :I, value :V) :I C E[K, V]

		override def opt(res :ResultSet, position :Int) :Option[I C E[K, V]] =
			for (i <- init.opt(res, position); v <- value.opt(res, position + init.readColumns))
				yield cons(i, v)

		private[this] val nullChain =
			try { cons(init.nullValue, value.nullValue) }
			catch { case _ :Exception => null.asInstanceOf[I C E[K, V]] }

		override def nullValue =
			if (nullChain == null) cons(init.nullValue, value.nullValue) else nullChain


		override def register(call :CallableStatement, position :Int) :Unit = {
			init.register(call, position)
			last.register(call, position + init.readColumns)
		}

		override def canEqual(that :Any) :Boolean = that match {
			case index :ChainIndexReadForm[_, _, _, _, _, _] => index.key == key
			case _ => false
		}

		override def hashCode :Int = (init.hashCode * 31 + key.hashCode) * 31 + last.hashCode

	}



	private[forms] abstract class AbstractListingReadForm
	                              [C[+A <: I, +B <: E[K, V]] <: A ~ B, E[+A <: U, +B], U,
	                               I <: Chain, K <: U, V]
	                              (last :SQLReadForm[E[K, V]])
	                              (implicit override val init :SQLReadForm[I],
	                               implicit override val value :SQLReadForm[V], keyValue :ValueOf[K])
		extends SuperChainForm[SQLReadForm[I], SQLReadForm[E[K, V]]](init, last)
		   with ChainIndexReadForm[C, E, U, I, K, V]
	{
		final override def key :K = keyValue.value
	}






	private[forms] trait ChainWriteForm[C[+A <: I, +B <: L] <: A ~ B, -I <: Chain, -L]
		extends SuperChainForm[SQLWriteForm[I], SQLWriteForm[L]] with SQLWriteForm[C[I, L]]
	{   //this is fine because init and last are initialized as constructor parameters
		override val writtenColumns :Int = init.writtenColumns + last.writtenColumns

		override def set(statement :PreparedStatement, position :Int, value :C[I, L]) :Unit =
			if (value == null)
				setNull(statement, position)
			else {
				init.set(statement, position, value.init)
				this.last.set(statement, init.writtenColumns + position, value.last)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			init.setNull(statement, position)
			last.setNull(statement, position + init.writtenColumns)
		}


		override def literal(value :I C L, inline :Boolean) :String = {
			def rec(chain :Chain, form :SQLWriteForm[_], res :StringBuilder = new StringBuilder)
					:StringBuilder =
				if (chain == null) form match {
					case f :ChainWriteForm[_, _, _] =>
						if (f.init.writtenColumns > 0)
							if (f.last.writtenColumns > 0)
								rec(null, f.init, res) ++= ", " ++= f.last.inlineNullLiteral
							else rec(null, f.init, res)
						else
	                        if (f.last.writtenColumns > 0)
		                        res ++= f.last.inlineNullLiteral
							else res
					case other => res ++= other.inlineNullLiteral

				} else (form, chain) match {
					case (f :ChainWriteForm[_, _, _], i ~ l) =>
						if (f.init.writtenColumns > 0)
							if (f.last.writtenColumns > 0)
								rec(i, f.init, res) ++= ", " ++= f.last.asInstanceOf[SQLWriteForm[Any]].inlineLiteral(l)
							else rec(i, f.init, res)
						else
	                        if (f.last.writtenColumns > 0)
		                        res ++= f.last.asInstanceOf[SQLWriteForm[Any]].inlineLiteral(l)
	                        else res
					case other => res ++= other.asInstanceOf[SQLWriteForm[Chain]].literal(chain)
				}
			if (inline)
				rec(value, this).toString
			else
				(rec(value, this, new StringBuilder("(")) ++= ")").toString
		}

		override def literal(value :I C L) :String = literal(value, false)
		override def inlineLiteral(value :I C L) :String = literal(value, true)
		override def nullLiteral :String = nullLiteral(false)
		override def inlineNullLiteral :String = nullLiteral(true)
		override def nullLiteral(inline :Boolean) :String = literal(null.asInstanceOf[I C L], false)
	}



	private[forms] class ChainSQLWriteForm[C[+A <: I, +B <: L] >: Null <: A ~ B, -I <: Chain, -L, -V]
	                     (override val init :SQLWriteForm[I], override val last :SQLWriteForm[L],
	                      protected override val symbol :String)
		extends SuperChainForm[SQLWriteForm[I], SQLWriteForm[L]](init, last) with ChainWriteForm[C, I, L]

}






sealed trait SQLFormLevel2Implicits extends SQLRWFormsImplicits {
	implicit def ChainForm[I <: Chain, L](implicit i :SQLForm[I], l :SQLForm[L]) :SQLForm[I ~ L] =
		new ChainSQLForm(i, l)

	implicit def formChainToChainForm[I <: Chain, L](chain :I ~ SQLForm[L])(implicit prefix :I => SQLForm[I])
			:SQLWriteForm[I ~ L] =
		ChainForm(prefix(chain.init), chain.last)

	implicit class ChainFormExtension[C <: Chain](private val self :SQLForm[C]) { //todo: AnyVal
		def ~[L](item :SQLForm[L]) :SQLForm[C ~ L] = ChainForm(self, item)
	}
}




sealed trait SQLFormLevel1Implicits extends SQLFormLevel2Implicits {
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

	implicit class ListingFormExtension[I <: Listing](private val self :SQLForm[I]) {
		def |~[K <: Listing.Key :ValueOf, V](item :K :~ SQLForm[V]) :SQLForm[I |~ (K :~ V)] =
			new ListingForm(self, valueOf[K], item.value)
	}

}




sealed trait SQLFormImplicits extends SQLFormLevel1Implicits {

	implicit def OptionForm[T :SQLForm] :SQLForm[Option[T]] = new OptionForm[T]


	private[forms] class OptionForm[T](implicit protected override val form :SQLForm[T])
		extends OptionWriteForm[T] with OptionReadForm[T] with SQLForm[Option[T]]
	{
		override val toString = "Option[" + form + "]"
	}


	implicit def OptForm[T :SQLForm] :SQLForm[Opt[T]] =
		new SQLForm[Opt[T]] with OptReadForm[T] with OptWriteForm[T] {
			override val form = SQLForm[T]

			override def apply(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position)

			override def set(statement :PreparedStatement, position :Int, value :Opt[T]) :Unit = value match {
				case Got(x) => form.set(statement, position, x)
				case _ => form.setNull(statement, position)
			}

			override def literal(value :Opt[T], inline :Boolean) = value match {
				case Got(x) => form.literal(x, inline)
				case _ => form.nullLiteral(inline)
			}

			override val toString = "Opt[" + form + "]"
		}

	implicit def OptionalForm[T :SQLForm] :SQLForm[Optional[T]] =
		SQLForm.flatMap("Optional[" + SQLForm[T] + "]")(flatMapToOptional[T])(optionalToOption)



	/** An empty chain form for the `Chain` terminator `@~`, which doesn't write or read any columns. */
	implicit val EmptyChainForm :SQLForm[@~] = SQLForm.empty(@~, "@~")


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
		SQLForm.map(s"(${valueOf[K]}:~${SQLForm[V]})")(:~[K](_:V))(_.value)

	private[forms] def ChainMapEntryForm[K <: ChainMap.Key :ValueOf, V :SQLForm] :SQLForm[(K, V)] =
		SQLForm.map(s"${valueOf[K]},${SQLForm[V]})")((v :V) => valueOf[K] -> v)(_._2)



	private[forms] class ChainSQLForm[I <: Chain, L](override val init :SQLForm[I], override val last :SQLForm[L])
		extends SuperChainForm[SQLForm[I], SQLForm[L]](init, last) with ChainWriteForm[~, I, L] with ChainReadForm[I, L]
		   with SQLForm[I ~ L]
	{
		protected override def symbol = "~"
	}


	private[forms] class ListingForm[I <: Listing, K <: Listing.Key, V]
	                     (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends SuperChainForm[SQLForm[I], SQLForm[K:~V]](init, ListingItemForm(new ValueOf[K](key), value))
		   with ChainWriteForm[|~, I, K:~V] with ChainIndexReadForm[|~, :~, Listing.Key, I, K, V]
		   with SQLForm[I |~ (K :~ V)]
	{
		override def symbol = "|~"
		protected[this] override def cons(init :I, value :V) :I |~ (K :~ V) = init |~ (key :~ value)
	}

	private[forms] class LabeledChainForm[I <: LabeledChain, K <: LabeledChain.Key, V]
	                      (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends SuperChainForm[SQLForm[I], SQLForm[K:~V]](init, ListingItemForm(new ValueOf[K](key), value))
		   with ChainWriteForm[>~, I, K :~ V] with ChainIndexReadForm[>~, :~, LabeledChain.Key, I, K, V]
		   with SQLForm[I >~ (K :~ V)]
	{
		protected override def symbol = ">~"
		protected[this] override def cons(init :I, value :V) :I >~ (K :~ V) = init >~ (key :~ value)
	}


	private[forms] class ChainMapForm[I <: ChainMap, K <: ChainMap.Key, V]
	                      (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends SuperChainForm[SQLForm[I], SQLForm[(K, V)]](init, ChainMapEntryForm(new ValueOf[K](key), value))
		   with ChainWriteForm[&~, I, (K, V)] with ChainIndexReadForm[&~, Tuple2, ChainMap.Key, I, K, V]
		   with SQLForm[I &~ (K, V)]
	{
		protected override def symbol = "&~"
		override protected[this] def cons(init :I, value :V) :I &~ (K, V) = init &~ (key -> value)
	}


	private[forms] class RecordForm[I <: Record, K <: Record.Key, V]
	                      (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends SuperChainForm[SQLForm[I], SQLForm[(K, V)]](init, ChainMapEntryForm(new ValueOf[K](key), value))
		   with ChainWriteForm[|#, I, (K, V)] with ChainIndexReadForm[|#, Tuple2, Record.Key, I, K, V]
		   with SQLForm[I |# (K, V)]
	{
		protected override def symbol = "|#"
		override protected[this] def cons(init :I, value :V) :I |# (K, V) = init |# key -> value
	}

}






sealed trait ColumnRWFormsImplicits extends SQLFormImplicits {

	implicit def OptionColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Option[T]] =
		new OptionReadForm[T] with ColumnReadForm[Option[T]] with SuperAdapterColumnForm {
			override val form = ColumnReadForm[T]
		}

	implicit def OptionColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Option[T]] =
		new OptionWriteForm[T] with ColumnWriteForm[Option[T]] with SuperAdapterColumnForm {
			override val form :ColumnWriteForm[T] = ColumnWriteForm[T]
		}


	implicit def OptColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Opt[T]] =
		new OptReadForm[T] with ColumnReadForm[Opt[T]] with SuperAdapterColumnForm {
			override val form = ColumnReadForm[T]
			override def apply(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position)
			override val toString = super.toString
		}

	implicit def OptColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Opt[T]] =
		new OptWriteForm[T] with ColumnWriteForm[Opt[T]] with SuperAdapterColumnForm {
			override val form = ColumnWriteForm[T]

			override def set(statement :PreparedStatement, position :Int, value :Opt[T]) :Unit = value match {
				case Got(x) => form.set(statement, position, x)
				case _ => form.setNull(statement, position)
			}

			override def literal(value :Opt[T], inline :Boolean) = value match {
				case Got(x) => form.literal(x, inline)
				case _ => form.nullLiteral(inline)
			}
			override val toString = super.toString
		}


	implicit def OptionalColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Optional[T]] =
		ColumnReadForm.map("Optional[" + ColumnReadForm[T] + "]>")(Optional.of[T])

	implicit def OptionalColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Optional[T]] =
		ColumnWriteForm.flatMap("<Optional[" + ColumnWriteForm[T] + "]")(optionalToOption)
}






sealed trait ColumnFormImplicits extends ColumnRWFormsImplicits {

	/** Lifts the implicit `ColumnForm[T]` implementation to `ColumnForm[Option[T]]`. */
	implicit def OptionColumnForm[T :ColumnForm] :ColumnForm[Option[T]] =
		new OptionForm[T] with OptionReadForm[T] with ColumnForm[Option[T]] with SuperAdapterColumnForm {
			override val form = ColumnForm[T]
			override val toString :String = "Option[" + form + "]"
		}

	implicit def OptColumnForm[T :ColumnForm] :ColumnForm[Opt[T]] =
		new OptWriteForm[T] with OptReadForm[T] with ColumnForm[Opt[T]] with SuperAdapterColumnForm {
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

			override val toString = "Option[" + form + "]"
		}

	implicit def OptionalColumnForm[T :ColumnForm] :ColumnForm[Optional[T]] =
		ColumnForm.flatMap("Optional[" + ColumnForm[T] + "]")(flatMapToOptional[T])(optionalToOption)

}






object SQLForms extends ColumnFormImplicits with BasicForms with ScalaForms {
	implicit final val UnitForm = SQLForm.empty((), "Unit")

	private[schema] trait SuperAdapterColumnForm extends SuperColumnForm {
		protected def form :SuperColumnForm
		override def sqlType :JDBCType = form.sqlType
	}



	/** Base type for anonymous forms for natively supported JDBC types. Implements equality as class equality
	  * (ignoring the associated `NotNull` and `toString` as `sqlType.toString`.
	  */
	private[forms] abstract class BasicJDBCForm[T :NullValue](jdbcType :JDBCType)
		extends JDBCForm[T](jdbcType) with NullSafeColumnForm[T]
	{
		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :BasicJDBCForm[_] => other.getClass == getClass && other.nulls == nulls
			case _ => false
		}

		override def hashCode :Int = getClass.hashCode * 31 + nulls.hashCode
	}

	private[forms] abstract class AlternateJDBCForm[T :NullValue](jdbcType :JDBCType, override val toString :String)
		extends JDBCForm[T](jdbcType) with NullSafeColumnForm[T]
	{
		def this(jdbcType :JDBCType) = this(jdbcType, "J" + jdbcType)
	}

}

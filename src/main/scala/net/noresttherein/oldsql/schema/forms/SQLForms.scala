package net.noresttherein.oldsql.schema.forms

import java.sql.{JDBCType, PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.{Chain, ChainMap, IndexedChain, LabeledChain, Record}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.ChainMap.&~
import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.collection.LabeledChain.>~
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.forms.SQLForms.{AbstractChainIndexReadForm, ChainMapEntryReadForm, ChainMapEntryWriteForm, ChainMapForm, ChainReadForm, ChainSQLForm, ChainSQLWriteForm, ChainWriteForm, EmptyChainForm, IndexedChainEntryReadForm, IndexedChainEntryWriteForm, IndexedChainForm, SuperAdapterColumnForm, SuperChainForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals
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

	implicit def readFormChainToChainReadForm[P <: Chain, I <: Chain, L]
	             (forms :P ~ SQLReadForm[L])(implicit prefix :P => SQLReadForm[I]) :SQLReadForm[I ~ L] =
		ChainReadForm(prefix(forms.init), forms.last)

	implicit def ChainWriteForm[I <: Chain, L](implicit t :SQLWriteForm[I], h :SQLWriteForm[L]) :SQLWriteForm[I ~ L] =
		new SuperChainForm[SQLWriteForm[I], SQLWriteForm[L]](t, h) with ChainWriteForm[~, I, L] {
			override protected def symbol = "~"
		}

	implicit def writeFormChainToChainWriteForm[P <: Chain, I <: Chain, L]
	            (chain :P ~ SQLWriteForm[L])(implicit prefix :P => SQLWriteForm[I]) :SQLWriteForm[I ~ L] =
		ChainWriteForm(prefix(chain.init), chain.last)
}




sealed trait SQLRWFormsLevel1Implicits extends SQLRWFormsLevel2Implicits {

	/** Provides an implicit form for the heterogeneous map indexed by literal types (`ChainMap`) `I &~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def ChainMapReadForm[I <: ChainMap :SQLReadForm, K <: ChainMap.Key :ValueOf, V :SQLReadForm]
			:SQLReadForm[I &~ (K, V)] =
		new AbstractChainIndexReadForm[&~, Tuple2, Singleton, I, K, V](ChainMapEntryReadForm[K, V]) {
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



	/** Provides an implicit form for the heterogeneous map indexed by types (`IndexedChain`) `I |~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def IndexedChainReadForm[I <: IndexedChain :SQLReadForm, K <: IndexedChain.Key :ValueOf, V :SQLReadForm]
			:SQLReadForm[I |~ (K :~ V)] =
		new AbstractChainIndexReadForm[|~, :~, IndexedChain.Key, I, K, V](IndexedChainEntryReadForm[K, V]) {
			protected[this] override def cons(init :I, value :V) = init |~ key :~ value
			protected override def symbol = "|~"
		}

	implicit def readFormIndexedChainToIndexedChainReadForm[P <: IndexedChain, I <: IndexedChain, K <: IndexedChain.Key, V]
	             (forms :P |~ (K :~ SQLReadForm[V]))(implicit prefix :P => SQLReadForm[I], key :ValueOf[K])
			:SQLReadForm[I |~ (K :~ V)] =
		IndexedChainReadForm(prefix(forms.init), key, forms.last.value)

	implicit def IndexedChainWriteForm[I <: IndexedChain :SQLWriteForm, K <: IndexedChain.Key, V :SQLWriteForm]
			:SQLWriteForm[I |~ (K :~ V)] =
		new ChainSQLWriteForm(SQLWriteForm[I], IndexedChainEntryWriteForm[K, V], "|~")

	implicit def writeFormIndexedChainToIndexedChainWriteForm[P <: IndexedChain, I <: IndexedChain, K <: IndexedChain.Key, V]
	             (chain :P |~ (K :~ SQLWriteForm[V]))(implicit prefix :P => SQLWriteForm[I]) :SQLWriteForm[I |~ (K :~ V)] =
		IndexedChainWriteForm(prefix(chain.init), chain.last.value)

}




sealed trait SQLRWFormsImplicits extends SQLRWFormsLevel1Implicits {

	implicit def OptionReadForm[T :SQLReadForm] :SQLReadForm[Option[T]] =
		new OptionReadForm[T] { override val form = SQLReadForm[T] }

	implicit def OptionWriteForm[T :SQLWriteForm] :SQLWriteForm[Option[T]] =
		new OptionWriteForm[T] { val form :SQLWriteForm[T] = SQLWriteForm[T] }



	private[forms] trait OptionReadForm[T] extends SQLReadForm[Option[T]] {
		protected def form :SQLReadForm[T]
		override def readColumns :Int = form.readColumns

		override def apply(res :ResultSet, position :Int) :Option[T] = form.opt(res, position)
		override def opt(res :ResultSet, position :Int) :Option[Option[T]] = Some(form.opt(res, position))
		override def nullValue :Option[T] = None

		override def equals(that :Any) :Boolean = that match  {
			case self :AnyRef if self eq this => true
			case opt :OptionReadForm[_] if opt canEqual this => opt.form == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "Option[" + form + "]>"
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

		override def toString :String = "<Option[" + form + "]"
	}






	/** Provides an implicit form for the heterogeneous map indexed by string literal types (`LabeledChain`) `I >~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def LabeledChainReadForm[I <: LabeledChain :SQLReadForm, K <: Label :ValueOf, V :SQLReadForm]
			:SQLReadForm[I >~ (K :~ V)] =
		new AbstractChainIndexReadForm[>~, :~, Label, I, K, V](IndexedChainEntryReadForm[K, V]) {
			protected[this] override def cons(init :I, value :V) = init >~ key :~ value
			protected override def symbol = ">~"
		}

	implicit def readFormLabeledChainToLabeledChainReadForm[P <: LabeledChain, I <:LabeledChain, K <: Label, V]
	             (forms :P >~ (K :~ SQLReadForm[V]))(implicit prefix :P => SQLReadForm[I], key :ValueOf[K])
			:SQLReadForm[I >~ (K :~ V)] =
		LabeledChainReadForm(prefix(forms.init), key, forms.last.value)

	implicit def LabeledChainWriteForm[I <: LabeledChain :SQLWriteForm, K <: Label, V :SQLWriteForm]
			:SQLWriteForm[I >~ (K :~ V)] =
		new ChainSQLWriteForm(SQLWriteForm[I], IndexedChainEntryWriteForm[K, V], ">~")

	implicit def writeFormLabeledChainToLabeledChainWriteForm[P <: LabeledChain, I <: LabeledChain, K <: Label, V]
	             (chain :P >~ (K :~ SQLWriteForm[V]))(implicit prefix :P => SQLWriteForm[I]) :SQLWriteForm[I >~ (K :~ V)] =
		LabeledChainWriteForm(prefix(chain.init), chain.last.value)


	/** Provides an implicit form for the heterogeneous map indexed by string literals (`Record`) `I |# L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def RecordReadForm[I <: Record :SQLReadForm, K <: Label :ValueOf, V :SQLReadForm]
			:SQLReadForm[I |# (K, V)] =
		new AbstractChainIndexReadForm[|#, Tuple2, Label, I, K, V](ChainMapEntryReadForm[K, V]) {
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



	implicit def IndexedChainEntryReadForm[K <: IndexedChain.Key :ValueOf, V :SQLReadForm] :SQLReadForm[K :~ V] =
		SQLReadForm.map(s"(${valueOf[K]}:~${SQLReadForm[V]}") { :~[K](_:V) }

	implicit def IndexedChainEntryWriteForm[K <: IndexedChain.Key, V :SQLWriteForm] :SQLWriteForm[K :~ V] =
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


		override def canEqual(that :Any) :Boolean = that match {
			case index :ChainIndexReadForm[_, _, _, _, _, _] => index.key == key
			case _ => false
		}

		override def hashCode :Int = (init.hashCode * 31 + key.hashCode) * 31 + last.hashCode

	}



	private[forms] abstract class AbstractChainIndexReadForm
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






//fixme: I don't think these have precedence over read and write forms :(
sealed trait SQLFormLevel2Implicits extends SQLRWFormsImplicits {
	implicit def ChainForm[I <: Chain, L](implicit i :SQLForm[I], l :SQLForm[L]) :SQLForm[I ~ L] =
		new ChainSQLForm(i, l)

	implicit def formChainToChainForm[I <: Chain, L](chain :I ~ SQLForm[L])(implicit prefix :I => SQLForm[I])
			:SQLWriteForm[I ~ L] =
		ChainForm(prefix(chain.init), chain.last)
}




sealed trait SQLFormLevel1Implicits extends SQLFormLevel2Implicits {
	implicit def ChainMapForm[I <: ChainMap :SQLForm, K <: ChainMap.Key :ValueOf, V :SQLForm] :SQLForm[I &~ (K, V)] =
		new ChainMapForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def formChainMapToChainMapForm[I <: ChainMap, K <: ChainMap.Key, V]
	             (chain :I &~ (K, SQLForm[V]))(implicit prefix :I => SQLForm[I]) :SQLForm[I &~ (K, V)] =
		ChainMapForm(prefix(chain.init), new ValueOf[K](chain.last._1), chain.last._2)


	implicit def IndexedChainForm[I <: IndexedChain :SQLForm, K <: IndexedChain.Key :ValueOf, V :SQLForm]
			:SQLForm[I |~ (K :~ V)] =
		new IndexedChainForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def formIndexedChainToIndexedChainForm[I <: IndexedChain, K <: IndexedChain.Key, V]
	             (chain :I |~ (K:~SQLForm[V]))(implicit prefix :I => SQLForm[I], key :ValueOf[K]) :SQLForm[I|~(K:~V)] =
		IndexedChainForm(prefix(chain.init), key, chain.last.value)
}




sealed trait SQLFormImplicits extends SQLFormLevel1Implicits {

	implicit def OptionForm[T :SQLForm] :SQLForm[Option[T]] = new OptionForm[T]


	/** An empty chain form for the `Chain` terminator `@~`, which doesn't write or read any columns. */
	implicit val EmptyChainForm :SQLForm[@~] = SQLForm.empty(@~, "@~")


	implicit def LabeledChainForm[I <: LabeledChain :SQLForm, K <: Label :ValueOf, V :SQLForm] :SQLForm[I >~ (K :~ V)] =
		new LabeledChainForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def formLabeledChainToLabeledChainForm[I <: LabeledChain, K <: Label, V]
	             (chain :I >~ (K:~SQLForm[V]))(implicit prefix :I => SQLForm[I], key :ValueOf[K]) :SQLForm[I>~(K:~V)] =
		LabeledChainForm(prefix(chain.init), key, chain.last.value)

	implicit def RecordForm[I <: Record :SQLForm, K <: Label :ValueOf, V :SQLForm] :SQLForm[I |# (K, V)] =
		new RecordForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def formRecordToRecordForm[I <: Record, K <: Record.Key, V]
	             (chain :I |# (K, SQLForm[V]))(implicit prefix :I => SQLForm[I]) :SQLForm[I |# (K, V)] =
		RecordForm(prefix(chain.init), new ValueOf[K](chain.last._1), chain.last._2)


	implicit def IndexedChainEntryForm[K <: IndexedChain.Key :ValueOf, V :SQLForm] :SQLForm[K :~ V] =
		SQLForm.map(s"(${valueOf[K]}:~${SQLForm[V]})")(:~[K](_:V))(_.value)

	private[forms] def ChainMapEntryForm[K <: ChainMap.Key :ValueOf, V :SQLForm] :SQLForm[(K, V)] =
		SQLForm.map(s"${valueOf[K]},${SQLForm[V]})")((v :V) => valueOf[K] -> v)(_._2)



	private[forms] class OptionForm[T](implicit protected val form :SQLForm[T])
		extends OptionWriteForm[T] with OptionReadForm[T] with SQLForm[Option[T]]
	{
		override def toString = "Option[" + form + "]"
	}



	private[forms] class ChainSQLForm[I <: Chain, L](override val init :SQLForm[I], override val last :SQLForm[L])
		extends SuperChainForm[SQLForm[I], SQLForm[L]](init, last) with ChainWriteForm[~, I, L] with ChainReadForm[I, L]
		   with SQLForm[I ~ L]
	{
		protected override def symbol = "~"
	}


	private[forms] class IndexedChainForm[I <: IndexedChain, K <: IndexedChain.Key, V]
	                      (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends SuperChainForm[SQLForm[I], SQLForm[K:~V]](init, IndexedChainEntryForm(new ValueOf[K](key), value))
		   with ChainWriteForm[|~, I, K:~V] with ChainIndexReadForm[|~, :~, IndexedChain.Key, I, K, V]
		   with SQLForm[I |~ (K :~ V)]
	{
		override def symbol = "|~"
		protected[this] override def cons(init :I, value :V) :I |~ (K :~ V) = init |~ (key :~ value)
	}

	private[forms] class LabeledChainForm[I <: LabeledChain, K <: LabeledChain.Key, V]
	                      (override val init :SQLForm[I], override val key :K, val value :SQLForm[V])
		extends SuperChainForm[SQLForm[I], SQLForm[K:~V]](init, IndexedChainEntryForm(new ValueOf[K](key), value))
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
}






sealed trait ColumnFormImplicits extends ColumnRWFormsImplicits {

	/** Lifts the implicit `ColumnForm[T]` implementation to `ColumnForm[Option[T]]`. */
	implicit def OptionColumnForm[T](implicit content :ColumnForm[T]) :ColumnForm[Option[T]] =
		new OptionForm[T] with OptionReadForm[T] with ColumnForm[Option[T]] with SuperAdapterColumnForm {
			override val form = ColumnForm[T]
			override val toString :String = super[OptionForm].toString
		}

}






object SQLForms extends ColumnFormImplicits with JDBCTypes /*with ScalaForms with JavaForms*/ {
	implicit final val UnitForm = SQLForm.empty((), "Unit")

	private[schema] trait SuperAdapterColumnForm extends SuperColumnForm {
		protected def form :SuperColumnForm
		override def sqlType :JDBCType = form.sqlType
	}
}
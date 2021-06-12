package net.noresttherein.oldsql.schema.forms

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.EmptyForm
import net.noresttherein.oldsql.schema.forms.ChainForm.NonEmptyChainForm
import net.noresttherein.oldsql.schema.forms.ChainReadForm.NonEmptyChainReadForm
import net.noresttherein.oldsql.schema.forms.ChainWriteForm.NonEmptyChainWriteForm
import net.noresttherein.oldsql.schema.forms.ColumnChainForm.NonEmptyColumnChainForm
import net.noresttherein.oldsql.schema.forms.SQLForms.{ChainReadFormImpl, ChainWriteFormImpl, SuperChainForm}






/** An `SQLForm` for [[net.noresttherein.oldsql.collection.Chain Chain]] subtypes, in which every chain element
  * is handled by its own form. A `ChainReadForm[T]` forms a linked list of `ChainReadForm`s, numbering the size of the chain
  * plus one (for the [[net.noresttherein.oldsql.collection.Chain.@~ @~]] empty chain at the end of the list).
  * Each non-empty instance can be thus decomposed into a `(ChainReadForm[I], SQLForm[L])` pair, where `T =:= I ~ L`.
  * Implicit `SQLForm` values in the search scope are not (in general) instances of this class, as they work
  * with arbitrary `SQLForm[I]` values when providing a form for `SQLForm[I ~ L]`. When there is a need to decompose
  * chain forms, this class must be used directly.
  *
  * The [[net.noresttherein.oldsql.schema.forms.ChainReadForm$ companion]] object to this class is a form for the empty
  * chain; forms for longer chains can be created by appending forms for extra elements with
  * [[net.noresttherein.oldsql.schema.forms.ChainReadForm.~ ~]] method. Additionally, implicit `ChainReadForm`s are available
  * for all statically known chain types if their elements have the `SQLForm` type class.
  * @author Marcin Mościcki
  */ //todo: the rest forms and precedence over SQLForm
sealed trait ChainReadForm[+T <: Chain] extends SQLReadForm[T] {
	/** The number of elements in the chain type parameter of this form. This is unrelated to the number of columns
	  * read/written by this form.
	  */
	def size :Int

	/** Create a form for a chain with an additional element of type `N` appended at the end. */
	@inline def ~[N](form :SQLReadForm[N]) :ChainReadForm[T ~ N] = ChainReadForm(this, form)

	override def canEqual(other :Any) :Boolean = other.isInstanceOf[ChainReadForm[_]]
}



/** An [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] for an empty chain
  * [[net.noresttherein.oldsql.collection.Chain.@~ @~]] as well as a factory and match pattern of forms for longer
  * chains.
  */
object ChainReadForm {
	@inline implicit def empty :ChainReadForm[@~] = ChainForm
	
	@inline implicit def nonEmpty[I <: Chain, L]
	                             (implicit init :ChainReadForm[I], last :SQLReadForm[L]) :ChainReadForm[I ~ L] =
		new NonEmptyChainReadForm(init, last)

	
	implicit class ChainReadFormExtension[I <: Chain, L](private val form :ChainReadForm[I ~ L]) extends AnyVal {
		def init :ChainReadForm[I] = form.asInstanceOf[SuperChainForm[ChainReadForm[I], SQLReadForm[L]]].init
		def last :SQLReadForm[L] = form.asInstanceOf[SuperChainForm[ChainReadForm[I], SQLReadForm[L]]].last
		@inline def split :(ChainReadForm[I], SQLReadForm[L]) = (init, last)
	}
	
	
	/** Summon an implicit `ChainReadForm[T]`. */
	def apply[T <: Chain](implicit form :ChainReadForm[T]) :ChainReadForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ChainReadForm[I], last :SQLReadForm[L]) :ChainReadForm[I ~ L] =
		new NonEmptyChainReadForm(init, last)

	@inline def ~[N](form :SQLReadForm[N]) :ChainReadForm[@~ ~ N] = ChainReadForm(ColumnChainForm, form)

	def unapply(form :SuperSQLForm) :Option[(ChainReadForm[_ <: Chain], SQLReadForm[_])] = form match {
		case chain :NonEmptyChainReadForm[_, _] => Some((chain.init, chain.last))
		case chain :NonEmptyChainForm[_, _] => Some((chain.init, chain.last))
		case _ => None
	}

	def unapply[I <: Chain, L](form :SQLForm[I ~ L]) :Option[(ChainReadForm[I], SQLReadForm[L])] = form match {
		case chain :NonEmptyChainReadForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case chain :NonEmptyChainForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case _ => None
	}


	private[forms] sealed class NonEmptyChainReadForm[I <: Chain, L]
	                            (override val init :ChainReadForm[I], override val last :SQLReadForm[L])
		extends SuperChainForm[ChainReadForm[I], SQLReadForm[L]](init, last)
			with ChainReadFormImpl[I, L] with ChainReadForm[I ~ L]
	{
		override val size = init.size + 1
	}
}





sealed trait ColumnChainReadForm[+T <: Chain] extends ChainReadForm[T] {
	/** Create a form for a chain with an additional element of type `N` appended at the end. */
	@inline def ~[N](form :ColumnReadForm[N]) :ColumnChainReadForm[T ~ N] = ColumnChainReadForm(this, form)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnChainReadForm[_]]
}



object ColumnChainReadForm {
	@inline implicit def empty :ColumnChainReadForm[@~] = ChainForm

	@inline implicit def nonEmpty[I <: Chain, L](implicit init :ColumnChainReadForm[I], last :ColumnReadForm[L])
			:ColumnChainReadForm[I ~ L] =
		new NonEmptyColumnChainReadForm(init, last)


	implicit class ColumnChainReadFormExtension[I <: Chain, L](private val form :ColumnChainReadForm[I ~ L])
		extends AnyVal 
	{
		def init :ColumnChainReadForm[I] =
			form.asInstanceOf[SuperChainForm[ColumnChainReadForm[I], ColumnReadForm[L]]].init
		def last :ColumnReadForm[L] =
			form.asInstanceOf[SuperChainForm[ColumnChainReadForm[I], ColumnReadForm[L]]].last
		@inline def split :(ColumnChainReadForm[I], ColumnReadForm[L]) = (init, last)
	}


	def apply[T <: Chain](implicit form :ColumnChainReadForm[T]) :ColumnChainReadForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ColumnChainReadForm[I], last :ColumnReadForm[L]) :ColumnChainReadForm[I ~ L] =
		new NonEmptyColumnChainReadForm(init, last)

	@inline def ~[N](form :ColumnReadForm[N]) :ColumnChainReadForm[@~ ~ N] = ColumnChainReadForm(ChainForm, form)
	
	
	def unapply(form :SuperSQLForm) :Option[(ColumnChainReadForm[_ <: Chain], ColumnReadForm[_])] = form match {
		case chain :NonEmptyColumnChainReadForm[_, _] => Some((chain.init, chain.last))
		case chain :NonEmptyColumnChainForm[_, _] => Some((chain.init, chain.last))
		case _ => None
	}

	def unapply[I <: Chain, L](form :SQLForm[I ~ L]) :Option[(ColumnChainReadForm[I], ColumnReadForm[L])] = form match {
		case chain :NonEmptyColumnChainReadForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case chain :NonEmptyColumnChainForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case _ => None
	}



	private class NonEmptyColumnChainReadForm[I <: Chain, L]
	              (override val init :ColumnChainReadForm[I], override val last :ColumnReadForm[L])
		extends NonEmptyChainReadForm[I, L](init, last) with ColumnChainReadForm[I ~ L]
}






/** An `SQLForm` for [[net.noresttherein.oldsql.collection.Chain Chain]] subtypes, in which every chain element
  * is handled by its own form. A `ChainWriteForm[T]` forms a linked list of `ChainWriteForm`s, numbering the size of the chain
  * plus one (for the [[net.noresttherein.oldsql.collection.Chain.@~ @~]] empty chain at the end of the list).
  * Each non-empty instance can be thus decomposed into a `(ChainWriteForm[I], SQLForm[L])` pair, where `T =:= I ~ L`.
  * Implicit `SQLForm` values in the search scope are not (in general) instances of this class, as they work
  * with arbitrary `SQLForm[I]` values when providing a form for `SQLForm[I ~ L]`. When there is a need to decompose
  * chain forms, this class must be used directly.
  *
  * The [[net.noresttherein.oldsql.schema.forms.ChainWriteForm$ companion]] object to this class is a form for the empty
  * chain; forms for longer chains can be created by appending forms for extra elements with
  * [[net.noresttherein.oldsql.schema.forms.ChainWriteForm.~ ~]] method. Additionally, implicit `ChainWriteForm`s are available
  * for all statically known chain types if their elements have the `SQLForm` type class.
  * @author Marcin Mościcki
  */ //todo: the rest forms and precedence over SQLForm
sealed trait ChainWriteForm[-T <: Chain] extends SQLWriteForm[T] {
	/** The number of elements in the chain type parameter of this form. This is unrelated to the number of columns
	  * Write/written by this form.
	  */
	def size :Int

	/** Create a form for a chain with an additional element of type `N` appended at the end. */
	@inline def ~[N](form :SQLWriteForm[N]) :ChainWriteForm[T ~ N] = ChainWriteForm(this, form)

	override def canEqual(other :Any) :Boolean = other.isInstanceOf[ChainWriteForm[_]]
}



/** An [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] for an empty chain
  * [[net.noresttherein.oldsql.collection.Chain.@~ @~]] as well as a factory and match pattern of forms for longer
  * chains.
  */
object ChainWriteForm {
	@inline implicit def empty :ChainWriteForm[@~] = ChainForm
	
	@inline implicit def nonEmpty[I <: Chain, L]
	                             (implicit init :ChainWriteForm[I], last :SQLWriteForm[L]) :ChainWriteForm[I ~ L] =
		new NonEmptyChainWriteForm(init, last)


	implicit class ChainWriteFormExtension[I <: Chain, L](private val form :ChainWriteForm[I ~ L]) extends AnyVal {
		def init :ChainWriteForm[I] = form.asInstanceOf[SuperChainForm[ChainWriteForm[I], SQLWriteForm[L]]].init
		def last :SQLWriteForm[L] = form.asInstanceOf[SuperChainForm[ChainWriteForm[I], SQLWriteForm[L]]].last
		@inline def split :(ChainWriteForm[I], SQLWriteForm[L]) = (init, last)
	}


	/** Summon an implicit `ChainWriteForm[T]`. */
	def apply[T <: Chain](implicit form :ChainWriteForm[T]) :ChainWriteForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ChainWriteForm[I], last :SQLWriteForm[L]) :ChainWriteForm[I ~ L] =
		new NonEmptyChainWriteForm(init, last)

	@inline def ~[N](form :SQLWriteForm[N]) :ChainWriteForm[@~ ~ N] = ChainWriteForm(ColumnChainForm, form)

	def unapply(form :SuperSQLForm) :Option[(ChainWriteForm[_ <: Chain], SQLWriteForm[_])] = form match {
		case chain :NonEmptyChainWriteForm[_, _] => Some((chain.init, chain.last))
		case chain :NonEmptyChainForm[_, _] => Some((chain.init, chain.last))
		case _ => None
	}

	def unapply[I <: Chain, L](form :SQLForm[I ~ L]) :Option[(ChainWriteForm[I], SQLWriteForm[L])] = form match {
		case chain :NonEmptyChainWriteForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case chain :NonEmptyChainForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case _ => None
	}

	
	private[forms] sealed class NonEmptyChainWriteForm[I <: Chain, L]
	                            (override val init :ChainWriteForm[I], override val last :SQLWriteForm[L])
		extends SuperChainForm[ChainWriteForm[I], SQLWriteForm[L]](init, last)
			with ChainWriteFormImpl[~, I, L] with ChainWriteForm[I ~ L]
	{
		override val size = init.size + 1
	}
}





sealed trait ColumnChainWriteForm[-T <: Chain] extends ChainWriteForm[T] {
	/** Create a form for a chain with an additional element of type `N` appended at the end. */
	@inline def ~[N](form :ColumnWriteForm[N]) :ColumnChainWriteForm[T ~ N] = ColumnChainWriteForm(this, form)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnChainWriteForm[_]]
}



object ColumnChainWriteForm {
	@inline implicit def empty :ColumnChainWriteForm[@~] = ChainForm

	@inline implicit def nonEmpty[I <: Chain, L](implicit init :ColumnChainWriteForm[I], last :ColumnWriteForm[L])
			:ColumnChainWriteForm[I ~ L] =
		new NonEmptyColumnChainWriteForm(init, last)


	implicit class ColumnChainWriteFormExtension[I <: Chain, L](private val form :ColumnChainWriteForm[I ~ L])
		extends AnyVal 
	{
		def init :ColumnChainWriteForm[I] = form.asInstanceOf[NonEmptyColumnChainWriteForm[I, L]].init
		def last :ColumnWriteForm[L] = form.asInstanceOf[NonEmptyColumnChainWriteForm[I, L]].last
		@inline def split :(ColumnChainWriteForm[I], ColumnWriteForm[L]) = (init, last)
	}


	def apply[T <: Chain](implicit form :ColumnChainWriteForm[T]) :ColumnChainWriteForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ColumnChainWriteForm[I], last :ColumnWriteForm[L]) :ColumnChainWriteForm[I ~ L] =
		new NonEmptyColumnChainWriteForm(init, last)

	@inline def ~[N](form :ColumnWriteForm[N]) :ColumnChainWriteForm[@~ ~ N] = ColumnChainWriteForm(ChainForm, form)
	
	
	def unapply(form :SuperSQLForm) :Option[(ColumnChainWriteForm[_ <: Chain], ColumnWriteForm[_])] = form match {
		case chain :NonEmptyColumnChainWriteForm[_, _] => Some((chain.init, chain.last))
		case chain :NonEmptyColumnChainForm[_, _] => Some((chain.init, chain.last))
		case _ => None
	}

	def unapply[I <: Chain, L](form :SQLForm[I ~ L]) :Option[(ColumnChainWriteForm[I], ColumnWriteForm[L])] = form match {
		case chain :NonEmptyColumnChainWriteForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case chain :NonEmptyColumnChainForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case _ => None
	}


	private class NonEmptyColumnChainWriteForm[I <: Chain, L]
	              (override val init :ColumnChainWriteForm[I], override val last :ColumnWriteForm[L])
		extends NonEmptyChainWriteForm[I, L](init, last) with ColumnChainWriteForm[I ~ L]
}







/** An `SQLForm` for [[net.noresttherein.oldsql.collection.Chain Chain]] subtypes, in which every chain element
  * is handled by its own form. A `ChainForm[T]` forms a linked list of `ChainForm`s, numbering the size of the chain
  * plus one (for the [[net.noresttherein.oldsql.collection.Chain.@~ @~]] empty chain at the end of the list).
  * Each non-empty instance can be thus decomposed into a `(ChainForm[I], SQLForm[L])` pair, where `T =:= I ~ L`.
  * Implicit `SQLForm` values in the search scope are not (in general) instances of this class, as they work
  * with arbitrary `SQLForm[I]` values when providing a form for `SQLForm[I ~ L]`. When there is a need to decompose
  * chain forms, this class must be used directly.
  *
  * The [[net.noresttherein.oldsql.schema.forms.ChainForm$ companion]] object to this class is a form for the empty
  * chain; forms for longer chains can be created by appending forms for extra elements with
  * [[net.noresttherein.oldsql.schema.forms.ChainForm.~ ~]] method. Additionally, implicit `ChainForm`s are available
  * for all statically known chain types if their elements have the `SQLForm` type class.
  * @author Marcin Mościcki
  */ //todo: the rest forms and precedence over SQLForm
sealed trait ChainForm[T <: Chain] extends ChainReadForm[T] with ChainWriteForm[T] with SQLForm[T] {
	/** The number of elements in the chain type parameter of this form. This is unrelated to the number of columns
	  * Write/written by this form.
	  */
	override def size :Int

	/** Create a form for a chain with an additional element of type `N` appended at the end. */
	@inline def ~[N](form :SQLForm[N]) :ChainForm[T ~ N] = ChainForm(this, form)

	override def canEqual(other :Any) :Boolean = other.isInstanceOf[ChainForm[_]]
}



/** An [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] for an empty chain
  * [[net.noresttherein.oldsql.collection.Chain.@~ @~]] as well as a factory and match pattern of forms for longer
  * chains.
  */
object ChainForm extends EmptyForm[@~] with ColumnChainForm[@~] {
	@inline implicit def empty :ChainForm[@~] = this
	@inline implicit def nonEmpty[I <: Chain, L](implicit init :ChainForm[I], last :SQLForm[L]) :ChainForm[I ~ L] =
		new NonEmptyChainForm(init, last)


	implicit class ChainFormExtension[I <: Chain, L](private val form :ChainForm[I ~ L]) extends AnyVal {
		def init :ChainForm[I] = form.asInstanceOf[SuperChainForm[ChainForm[I], SQLForm[L]]].init
		def last :SQLForm[L] = form.asInstanceOf[SuperChainForm[ChainForm[I], SQLForm[L]]].last
		@inline def split :(ChainForm[I], SQLForm[L]) = (init, last)
	}


	/** Summon an implicit `ChainForm[T]`. */
	def apply[T <: Chain](implicit form :ChainForm[T]) :ChainForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ChainForm[I], last :SQLForm[L]) :ChainForm[I ~ L] =
		new NonEmptyChainForm(init, last)

	def unapply(form :SuperSQLForm) :Option[(ChainForm[_ <: Chain], SQLForm[_])] = form match {
		case chain :NonEmptyChainForm[_, _] => Some((chain.init, chain.last))
		case _ => None
	}

	def unapply[I <: Chain, L](form :SQLForm[I ~ L]) :Option[(ChainForm[I], SQLForm[L])] = form match {
		case chain :NonEmptyChainForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case _ => None
	}

	
	private[forms] sealed class NonEmptyChainForm[I <: Chain, L]
	                                             (override val init :ChainForm[I], override val last :SQLForm[L])
		extends SuperChainForm[ChainForm[I], SQLForm[L]](init, last)
		   with ChainWriteFormImpl[~, I, L] with ChainReadFormImpl[I, L] with ChainForm[I ~ L]
	{
		override val size = init.size + 1
	}

	override def size = 0
	override def nullValue: @~ = @~

	override def equals(that :Any) :Boolean = that match {
		case ChainForm | ColumnChainForm => true
		case _ => false
	}
	override def hashCode :Int = ColumnChainForm.hashCode
	override def toString = "@~"
}





sealed trait ColumnChainForm[T <: Chain] extends ColumnChainReadForm[T] with ColumnChainWriteForm[T] with ChainForm[T] {
	/** Create a form for a chain with an additional element of type `N` appended at the end. */
	@inline def ~[N](form :ColumnForm[N]) :ColumnChainForm[T ~ N] = ColumnChainForm(this, form)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnChainForm[_]]
}



object ColumnChainForm extends EmptyForm[@~] with ColumnChainForm[@~] {
	//todo: priority of implicits over ChainForm
	@inline implicit def empty :ColumnChainForm[@~] = this

	@inline implicit def nonEmpty[I <: Chain, L](implicit init :ColumnChainForm[I], last :ColumnForm[L])
			:ColumnChainForm[I ~ L] =
		new NonEmptyColumnChainForm(init, last)


	implicit class ColumnChainFormExtension[I <: Chain, L](private val form :ColumnChainForm[I ~ L]) extends AnyVal {
		def init :ColumnChainForm[I] = form.asInstanceOf[SuperChainForm[ColumnChainForm[I], ColumnForm[L]]].init
		def last :ColumnForm[L] = form.asInstanceOf[SuperChainForm[ColumnChainForm[I], ColumnForm[L]]].last
		@inline def split :(ColumnChainForm[I], ColumnForm[L]) = (init, last)
	}


	def apply[T <: Chain](implicit form :ColumnChainForm[T]) :ColumnChainForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ColumnChainForm[I], last :ColumnForm[L]) :ColumnChainForm[I ~ L] =
		new NonEmptyColumnChainForm(init, last)

	def unapply(form :SuperSQLForm) :Option[(ColumnChainForm[_ <: Chain], ColumnForm[_])] = form match {
		case chain :NonEmptyColumnChainForm[_, _] => Some((chain.init, chain.last))
		case _ => None
	}

	def unapply[I <: Chain, L](form :SQLForm[I ~ L]) :Option[(ColumnChainForm[I], ColumnForm[L])] = form match {
		case chain :NonEmptyColumnChainForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case _ => None
	}


	private[forms] class NonEmptyColumnChainForm[I <: Chain, L]
	                                     (override val init :ColumnChainForm[I], override val last :ColumnForm[L])
		extends NonEmptyChainForm[I, L](init, last) with ColumnChainForm[I ~ L]



	override def size = 0
	override def nullValue: @~ = @~

	override def equals(that :Any) :Boolean = that match {
		case ColumnChainForm | ChainForm => true
		case _ => false
	}
	override def hashCode :Int = 0
	override def toString = "@~"
}




package net.noresttherein.oldsql.schema.forms

import java.lang.{StringBuilder => JStringBuilder}
import java.sql.{CallableStatement, JDBCType, PreparedStatement, ResultSet}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder

import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.EmptyForm
import net.noresttherein.oldsql.schema.forms.ChainForm.NonEmptyChainForm
import net.noresttherein.oldsql.schema.forms.ChainReadForm.NonEmptyChainReadForm
import net.noresttherein.oldsql.schema.forms.ChainWriteForm.NonEmptyChainWriteForm
import net.noresttherein.oldsql.schema.forms.ColumnChainForm.NonEmptyColumnChainForm
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals






/** An `SQLForm` for [[net.noresttherein.oldsql.collection.Chain Chain]] subtypes, in which every chain element
  * is handled by its own form. A `ChainReadForm[T]` forms a linked list of `ChainReadForm`s, numbering the size of
  * the chain plus one (for the [[net.noresttherein.oldsql.collection.Chain.@~ @~]] empty chain at the end of the list).
  * Each non-empty instance can be thus decomposed into a `(ChainReadForm[I], SQLForm[L])` pair, where `T =:= I ~ L`.
  * Implicit `SQLForm` values in the search scope are not (in general) instances of this class, as they work
  * with arbitrary `SQLForm[I]` values when providing a form for `SQLForm[I ~ L]`. When there is a need to decompose
  * chain forms, this class must be used directly.
  *
  * The [[net.noresttherein.oldsql.schema.forms.ChainReadForm$ companion]] object to this class is a form for the empty
  * chain; forms for longer chains can be created by appending forms for extra elements with
  * [[net.noresttherein.oldsql.schema.forms.ChainReadForm.~ ~]] method. Additionally, implicit `ChainReadForm`s
  * are available for all statically known chain types if their elements have the `SQLForm` type class.
  * @see [[net.noresttherein.oldsql.schema.forms.ChainReadForm.ChainReadFormExtension]]
  * @author Marcin Mościcki
  */ //todo: precedence over SQLForm
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
		def init :ChainReadForm[I] = form.asInstanceOf[UnspecifiedChainForm[ChainReadForm[I], SQLReadForm[L]]].init
		def last :SQLReadForm[L] = form.asInstanceOf[UnspecifiedChainForm[ChainReadForm[I], SQLReadForm[L]]].last
		@inline def split :(ChainReadForm[I], SQLReadForm[L]) = (init, last)
	}


	/** Summon an implicit `ChainReadForm[T]`. */
	def apply[T <: Chain](implicit form :ChainReadForm[T]) :ChainReadForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ChainReadForm[I], last :SQLReadForm[L]) :ChainReadForm[I ~ L] =
		new NonEmptyChainReadForm(init, last)

	@inline def ~[N](form :SQLReadForm[N]) :ChainReadForm[@~ ~ N] = ChainReadForm(ColumnChainForm, form)

	def unapply(form :UnspecifiedForm) :Option[(ChainReadForm[_ <: Chain], SQLReadForm[_])] = form match {
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
		extends UnspecifiedChainForm[ChainReadForm[I], SQLReadForm[L]](init, last)
			with ChainReadFormImpl[I, L] with ChainReadForm[I ~ L]
	{
		override val size = init.size + 1
	}
}





/** An [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] for a chain `T` where every element
  * corresponds to a single column. It can be built by appending subsequent
  * [[net.noresttherein.oldsql.schema.ColumnReadForm ColumnReadForm]]s to another `ColumnChainReadForm`
  * with method [[net.noresttherein.oldsql.schema.forms.ColumnChainReadForm.~ ~]], starting with
  * empty chain form [[net.noresttherein.oldsql.schema.forms.ChainForm$ ChainForm]]
  * or [[net.noresttherein.oldsql.schema.forms.ColumnChainForm$ ColumnChainForm]] (equivalent choices)
  * Despite having 'Column' in its name, it is ''not'' a `ColumnReadForm`.
  * @see [[net.noresttherein.oldsql.schema.forms.ColumnChainReadForm.ColumnChainReadFormExtension]]
  */
sealed trait ColumnChainReadForm[+T <: Chain] extends ChainReadForm[T] {
	/** Create a form for a chain with an additional element of type `N` appended at the end. */
	@inline def ~[N](form :ColumnReadForm[N]) :ColumnChainReadForm[T ~ N] = ColumnChainReadForm(this, form)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnChainReadForm[_]]
}



/** A factory, deconstructor and extension method definitions for
  * [[net.noresttherein.oldsql.schema.forms.ColumnChainReadForm ColumnChainReadForm]] chain forms.
  */
object ColumnChainReadForm {
	@inline implicit def empty :ColumnChainReadForm[@~] = ChainForm

	@inline implicit def nonEmpty[I <: Chain, L](implicit init :ColumnChainReadForm[I], last :ColumnReadForm[L])
			:ColumnChainReadForm[I ~ L] =
		new NonEmptyColumnChainReadForm(init, last)


	implicit class ColumnChainReadFormExtension[I <: Chain, L](private val form :ColumnChainReadForm[I ~ L])
		extends AnyVal
	{
		def init :ColumnChainReadForm[I] =
			form.asInstanceOf[UnspecifiedChainForm[ColumnChainReadForm[I], ColumnReadForm[L]]].init
		def last :ColumnReadForm[L] =
			form.asInstanceOf[UnspecifiedChainForm[ColumnChainReadForm[I], ColumnReadForm[L]]].last
		@inline def split :(ColumnChainReadForm[I], ColumnReadForm[L]) = (init, last)
	}


	def apply[T <: Chain](implicit form :ColumnChainReadForm[T]) :ColumnChainReadForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ColumnChainReadForm[I], last :ColumnReadForm[L]) :ColumnChainReadForm[I ~ L] =
		new NonEmptyColumnChainReadForm(init, last)

	@inline def ~[N](form :ColumnReadForm[N]) :ColumnChainReadForm[@~ ~ N] = ColumnChainReadForm(ChainForm, form)


	def unapply(form :UnspecifiedForm) :Option[(ColumnChainReadForm[_ <: Chain], ColumnReadForm[_])] = form match {
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
  * is handled by its own form. A `ChainWriteForm[T]` forms a linked list of `ChainWriteForm`s, numbering the size of
  * the chain plus one (for the [[net.noresttherein.oldsql.collection.Chain.@~ @~]] empty chain at the end of the list).
  * Each non-empty instance can be thus decomposed into a `(ChainWriteForm[I], SQLForm[L])` pair, where `T =:= I ~ L`.
  * Implicit `SQLForm` values in the search scope are not (in general) instances of this class, as they work
  * with arbitrary `SQLForm[I]` values when providing a form for `SQLForm[I ~ L]`. When there is a need to decompose
  * chain forms, this class must be used directly.
  *
  * The [[net.noresttherein.oldsql.schema.forms.ChainWriteForm$ companion]] object to this class is a form for the empty
  * chain; forms for longer chains can be created by appending forms for extra elements with
  * [[net.noresttherein.oldsql.schema.forms.ChainWriteForm.~ ~]] method. Additionally, implicit `ChainWriteForm`s
  * are available for all statically known chain types if their elements have the `SQLForm` type class.
  * @see [[net.noresttherein.oldsql.schema.forms.ChainWriteForm.ChainWriteFormExtension]]
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
		def init :ChainWriteForm[I] = form.asInstanceOf[UnspecifiedChainForm[ChainWriteForm[I], SQLWriteForm[L]]].init
		def last :SQLWriteForm[L] = form.asInstanceOf[UnspecifiedChainForm[ChainWriteForm[I], SQLWriteForm[L]]].last
		@inline def split :(ChainWriteForm[I], SQLWriteForm[L]) = (init, last)
	}


	/** Summon an implicit `ChainWriteForm[T]`. */
	def apply[T <: Chain](implicit form :ChainWriteForm[T]) :ChainWriteForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ChainWriteForm[I], last :SQLWriteForm[L]) :ChainWriteForm[I ~ L] =
		new NonEmptyChainWriteForm(init, last)

	@inline def ~[N](form :SQLWriteForm[N]) :ChainWriteForm[@~ ~ N] = ChainWriteForm(ColumnChainForm, form)

	def unapply(form :UnspecifiedForm) :Option[(ChainWriteForm[_ <: Chain], SQLWriteForm[_])] = form match {
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
		extends UnspecifiedChainForm[ChainWriteForm[I], SQLWriteForm[L]](init, last)
		   with ChainWriteFormImpl[~, I, L] with ChainWriteForm[I ~ L]
	{
		override val size = init.size + 1
	}
}





/** An [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]] for a chain `T` where every element
  * corresponds to a single column. It can be built by appending subsequent
  * [[net.noresttherein.oldsql.schema.ColumnWriteForm ColumnWriteForm]]s to another `ColumnChainWriteForm`
  * with method [[net.noresttherein.oldsql.schema.forms.ColumnChainWriteForm.~ ~]], starting with
  * empty chain form [[net.noresttherein.oldsql.schema.forms.ChainForm$ ChainForm]]
  * or [[net.noresttherein.oldsql.schema.forms.ColumnChainForm$ ColumnChainForm]] (equivalent choices)
  * Despite having 'Column' in its name, it is ''not'' a `ColumnWriteForm`.
  * @see [[net.noresttherein.oldsql.schema.forms.ColumnChainWriteForm.ColumnChainWriteFormExtension]]
  */
sealed trait ColumnChainWriteForm[-T <: Chain] extends ChainWriteForm[T] {
	/** Create a form for a chain with an additional element of type `N` appended at the end. */
	@inline def ~[N](form :ColumnWriteForm[N]) :ColumnChainWriteForm[T ~ N] = ColumnChainWriteForm(this, form)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnChainWriteForm[_]]
}



/** A factory, deconstructor and extension method definitions for
  * [[net.noresttherein.oldsql.schema.forms.ColumnChainWriteForm ColumnChainWriteForm]] chain forms.
  */
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


	def unapply(form :UnspecifiedForm) :Option[(ColumnChainWriteForm[_ <: Chain], ColumnWriteForm[_])] = form match {
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
  * @see [[net.noresttherein.oldsql.schema.forms.ChainForm.ChainFormExtension]]
  * @author Marcin Mościcki
  */ //todo: forms for Chain subtypes and precedence over SQLForm
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
  * chains. Note that it is a [[net.noresttherein.oldsql.schema.forms.ColumnChainForm ColumnChainForm]],
  * so it can be expanded to build instances of the latter type class for longer chains, as well as
  * for [[net.noresttherein.oldsql.schema.forms.ColumnChainReadForm ColumnChainReadForm]]
  * and [[net.noresttherein.oldsql.schema.forms.ColumnChainWriteForm ColumnChainWriteForm]].
  */
object ChainForm extends EmptyForm[@~] with ColumnChainForm[@~] {
	@inline implicit def empty :ChainForm[@~] = this
	@inline implicit def nonEmpty[I <: Chain, L](implicit init :ChainForm[I], last :SQLForm[L]) :ChainForm[I ~ L] =
		new NonEmptyChainForm(init, last)


	implicit class ChainFormExtension[I <: Chain, L](private val form :ChainForm[I ~ L]) extends AnyVal {
		def init :ChainForm[I] = form.asInstanceOf[UnspecifiedChainForm[ChainForm[I], SQLForm[L]]].init
		def last :SQLForm[L] = form.asInstanceOf[UnspecifiedChainForm[ChainForm[I], SQLForm[L]]].last
		@inline def split :(ChainForm[I], SQLForm[L]) = (init, last)
	}


	/** Summon an implicit `ChainForm[T]`. */
	def apply[T <: Chain](implicit form :ChainForm[T]) :ChainForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ChainForm[I], last :SQLForm[L]) :ChainForm[I ~ L] =
		new NonEmptyChainForm(init, last)

	def unapply(form :UnspecifiedForm) :Option[(ChainForm[_ <: Chain], SQLForm[_])] = form match {
		case chain :NonEmptyChainForm[_, _] => Some((chain.init, chain.last))
		case _ => None
	}

	def unapply[I <: Chain, L](form :SQLForm[I ~ L]) :Option[(ChainForm[I], SQLForm[L])] = form match {
		case chain :NonEmptyChainForm[I @unchecked, L @unchecked] => Some((chain.init, chain.last))
		case _ => None
	}


	private[forms] sealed class NonEmptyChainForm[I <: Chain, L]
	                                             (override val init :ChainForm[I], override val last :SQLForm[L])
		extends UnspecifiedChainForm[ChainForm[I], SQLForm[L]](init, last)
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





/** An [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] for a chain `T` where every element
  * corresponds to a single column. It can be built by appending subsequent
  * [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]s to another `ColumnChainForm`
  * with method [[net.noresttherein.oldsql.schema.forms.ColumnChainForm.~ ~]], starting with
  * empty chain form [[net.noresttherein.oldsql.schema.forms.ChainForm$ ChainForm]]
  * or [[net.noresttherein.oldsql.schema.forms.ColumnChainForm$ ColumnChainForm]] (equivalent choices)
  * Despite having 'Column' in its name, it is ''not'' a `ColumnForm`.
  * @see [[net.noresttherein.oldsql.schema.forms.ColumnChainForm.ColumnChainFormExtension]]
  */
sealed trait ColumnChainForm[T <: Chain] extends ColumnChainReadForm[T] with ColumnChainWriteForm[T] with ChainForm[T] {
	/** Create a form for a chain with an additional element of type `N` appended at the end. */
	@inline def ~[N](form :ColumnForm[N]) :ColumnChainForm[T ~ N] = ColumnChainForm(this, form)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnChainForm[_]]
}



/** A factory, deconstructor and extension method definitions for
  * [[net.noresttherein.oldsql.schema.forms.ColumnChainForm ColumnChainForm]] chain forms with a one-to-one mapping
  * between chain elements and column (read/write) forms.
  * This form is ''not'' a [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]], but instead a zero-width form
  * producing an empty chain `@~`. It can be used as a builder/factory of non empty chains using
  * `ChainForm`'s method [[net.noresttherein.oldsql.schema.forms.ChainForm.~ ~]]:
  * {{{
  *     val StringAndIntForm = ColumnChainForm ~ ColumnForm[String] ~ ColumnForm[Int]
  * }}}
  */
object ColumnChainForm extends EmptyForm[@~] with ColumnChainForm[@~] {
	//todo: priority of implicits over ChainForm
	@inline implicit def empty :ColumnChainForm[@~] = this

	@inline implicit def nonEmpty[I <: Chain, L](implicit init :ColumnChainForm[I], last :ColumnForm[L])
			:ColumnChainForm[I ~ L] =
		new NonEmptyColumnChainForm(init, last)


	implicit class ColumnChainFormExtension[I <: Chain, L](private val form :ColumnChainForm[I ~ L]) extends AnyVal {
		def init :ColumnChainForm[I] = form.asInstanceOf[UnspecifiedChainForm[ColumnChainForm[I], ColumnForm[L]]].init
		def last :ColumnForm[L] = form.asInstanceOf[UnspecifiedChainForm[ColumnChainForm[I], ColumnForm[L]]].last
		@inline def split :(ColumnChainForm[I], ColumnForm[L]) = (init, last)
	}


	def apply[T <: Chain](implicit form :ColumnChainForm[T]) :ColumnChainForm[T] = form

	/** A constructor of a non-empty chain form, appending the form `last` to chain `init`.   */
	def apply[I <: Chain, L](init :ColumnChainForm[I], last :ColumnForm[L]) :ColumnChainForm[I ~ L] =
		new NonEmptyColumnChainForm(init, last)

	def unapply(form :UnspecifiedForm) :Option[(ColumnChainForm[_ <: Chain], ColumnForm[_])] = form match {
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









/** Base class used for all non empty chain forms: read and write, forms of `Chain`, `Listing`, and other subclasses of
  * `Chain`, Jproper `ChainReadForm`/`ChainWriteForm` as well as ad hoc compositions of arbitrary two forms
  * for the initial chain and the last element. In itself, it is little more than a pair of forms with common
  * `equals` and `toString` implementations, but defines member fields for the two forms and is especially useful
  * for anonymous implementations.
  */
private[forms] abstract class UnspecifiedChainForm[+I <: UnspecifiedForm, +L <: UnspecifiedForm]
                                                  (val init :I, val last :L)
	extends UnspecifiedForm //init and last must be constructor params because they are accessed by constructors
{
	override lazy val columnTypes :Seq[JDBCType] = init.columnTypes :++ last.columnTypes

	override def equals(that :Any) :Boolean = that match {
		case chain :UnspecifiedChainForm[_, _] =>
			(chain eq this) || (chain canEqual this) && chain.last == last && chain.init == init
		case _ => false
	}
	override def hashCode :Int = init.hashCode * 31 + last.hashCode

	protected def symbol :String

	private lazy val cachedString = {
		def rec(chain :UnspecifiedChainForm[_, _], res :StringBuilder) :StringBuilder = {
			chain.init match {
				case prefix :UnspecifiedChainForm[_, _] => rec(prefix, res)
				case ChainForm => res ++= "@~"
				case other => res ++= other.toString
			}
			chain.last match {
				case chain :UnspecifiedChainForm[_, _] => rec(chain, res ++= symbol += '(') += ')'
				case ChainForm => res ++= symbol ++= "(@~)"
				case _ => res ++= symbol ++= last.toString
			}
		}
		rec(this, new StringBuilder).toString
	}

	override def toString = cachedString
}


/** Base trait for read forms of various `Chain` subtypes, it does not specify its mapped type. */
private[forms] trait UnspecifiedChainReadForm[+I <: SQLReadForm[_], +L <: SQLReadForm[_]]
	extends UnspecifiedChainForm[I, L] with SQLReadForm[Any]
{
	override def comparable(other :SQLReadForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :UnspecifiedChainReadForm[_, _] =>
			(last comparable other.last) && (init comparable other.init) || super.comparable(other)
		case _ => super.comparable(other)
	}
}


/** Base trait for write forms of various `Chain` subtypes, it does not specify its mapped type. */
private[forms] trait UnspecifiedChainWriteForm[+I <: SQLWriteForm[_], +L <: SQLWriteForm[_]]
	extends UnspecifiedChainForm[I, L] with SQLWriteForm[Nothing]
{
	override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
		case _ if this eq other => true
		case other :UnspecifiedChainWriteForm[_, _] =>
			(last comparable other.last) && (init comparable other.init) || super.comparable(other)
		case _ => super.comparable(other)
	}
}






/** Full implementation of a read form for chain `I ~ L`. It remains a trait to allow extension by both pure read
  * forms and read/write forms. Itself, it does not assume anything about the nature of the form for the
  * prefix chain `I`, and has both ad-hoc implementations allowing composition of any forms for `I` and `L`
  * into a chain form, as well as the strict form-per-chain-element `Chain[Read/Write]Form` forms.
  */
private[forms] trait ChainReadFormImpl[+I <: Chain, +L]
	extends UnspecifiedChainForm[SQLReadForm[I], SQLReadForm[L]]
	   with UnspecifiedChainReadForm[SQLReadForm[I], SQLReadForm[L]] with SQLReadForm[I ~ L]
{   //this is fine because init and last are guaranteed to be initialized as constructor parameters
	override val columnCount :Int = init.columnCount + last.columnCount
	override def isUniversal :Boolean = last.isUniversal && init.isUniversal

	override def apply(res :ResultSet, position :Int) :I ~ L =
		init(res, position) ~ last(res, position + init.columnCount)

	override def opt(res :ResultSet, position :Int) :Opt[I ~ L] =
		for (t <- init.opt(res, position); h <- last.opt(res, position + init.columnCount)) yield t ~ h

	private[this] val nullChain :I ~ L =
		try { init.nullValue ~ last.nullValue }
		catch { case _ :Exception => null }

	override def nullValue :I ~ L =
		if (nullChain == null) init.nullValue ~ last.nullValue else nullChain

	override def register(call :CallableStatement, position :Int) :Unit = {
		init.register(call, position)
		last.register(call, position + init.columnCount)
	}

	protected override def symbol = "~"
}



/** Common base trait for `Chain` subtypes `C` in which each entry `E[K, V]` consists of a key `K` and value `V`. */
private[forms] trait ChainIndexReadFormImpl[C[+A <: I, +B <: E[K, V]] <: A ~ B,
                                            E[+A <: U, +B], U, I <: Chain, K <: U, V]
	extends UnspecifiedChainForm[SQLReadForm[I], SQLReadForm[E[K, V]]]
	   with UnspecifiedChainReadForm[SQLReadForm[I], SQLReadForm[E[K, V]]] with SQLReadForm[I C E[K, V]]
{
	protected val value :SQLReadForm[V]
	protected def key :K

	override val columnCount :Int = init.columnCount + last.columnCount
	override def isUniversal :Boolean = last.isUniversal && init.isUniversal

	protected[this] def cons(init :I, value :V) :I C E[K, V]

	override def opt(res :ResultSet, position :Int) :Opt[I C E[K, V]] =
		for (i <- init.opt(res, position); v <- value.opt(res, position + init.columnCount))
			yield cons(i, v)

	private[this] val nullChain =
		try { cons(init.nullValue, value.nullValue) }
		catch { case _ :Exception => null.asInstanceOf[I C E[K, V]] }

	override def nullValue =
		if (nullChain == null) cons(init.nullValue, value.nullValue) else nullChain

	override def register(call :CallableStatement, position :Int) :Unit = {
		init.register(call, position)
		last.register(call, position + init.columnCount)
	}

	override def canEqual(that :Any) :Boolean = that match {
		case index :ChainIndexReadFormImpl[_, _, _, _, _, _] => key == index.key
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
	extends UnspecifiedChainForm[SQLReadForm[I], SQLReadForm[E[K, V]]](init, last)
	   with UnspecifiedChainReadForm[SQLReadForm[I], SQLReadForm[E[K, V]]]
	   with ChainIndexReadFormImpl[C, E, U, I, K, V]
{
	final override def key :K = keyValue.value
}






/** Full implementation of a write form for chain `I ~ L`. It remains a trait to allow extension by both pure write
  * forms and read/write forms. Itself, it does not assume anything about the nature of the form for the
  * prefix chain `I`, and has both ad-hoc implementations allowing composition of any forms for `I` and `L`
  * into a chain form, as well as the strict form-per-chain-element `Chain[Read/Write]Form` forms.
  */
private[forms] trait ChainWriteFormImpl[C[+A <: I, +B <: L] <: A ~ B, -I <: Chain, -L]
	extends UnspecifiedChainForm[SQLWriteForm[I], SQLWriteForm[L]]
	   with UnspecifiedChainWriteForm[SQLWriteForm[I], SQLWriteForm[L]] with SQLWriteForm[C[I, L]]
	   with WriteFormOptLiterals[C[I, L]]
{   //this is fine because init and last are initialized as constructor parameters
	override val columnCount :Int = init.columnCount + last.columnCount
	override def isUniversal :Boolean = last.isUniversal && init.isUniversal

	override def set(statement :PreparedStatement, position :Int, value :C[I, L]) :Unit =
		if (value == null)
			setNull(statement, position)
		else {
			init.set(statement, position, value.init)
			this.last.set(statement, init.columnCount + position, value.last)
		}

	override def setNull(statement :PreparedStatement, position :Int) :Unit = {
		init.setNull(statement, position)
		last.setNull(statement, position + init.columnCount)
	}

	override def optLiteral(value :Opt[I C L], inline :Boolean) :String = {
		def rec(chain :Opt[Chain], form :SQLWriteForm[_], res :JStringBuilder = new JStringBuilder) :JStringBuilder =
			(form, chain) match {
				case (f :ChainWriteFormImpl[_, _, Any @unchecked], Got(i ~ l)) =>
					if (f.init.columnCount > 0)
						if (f.last.columnCount > 0)
							rec(Opt(i), f.init, res) append ", " append f.last.inlineLiteral(l)
						else
							rec(Opt(i), f.init, res)
					else
	                    if (f.last.columnCount > 0)
	                        res append f.last.inlineLiteral(l)
	                    else res
				case (f :ChainWriteFormImpl[_, _, Any @unchecked], _) =>
					if (f.init.columnCount > 0)
						if (f.last.columnCount > 0)
							rec(Lack, f.init, res) append ", " append f.last.inlineNullLiteral
						else
							rec(Lack, f.init, res)
					else
						res
				case (other :SQLWriteForm[Chain] @unchecked, Got(x)) =>
					res append other.literal(x)
				case (other, _) =>
					res append other.inlineNullLiteral
			}
		if (inline)
			rec(value, this).toString
		else
			(rec(value, this, new JStringBuilder("(")) append ")").toString
	}

	override def optColumnLiterals(value :Opt[I C L]) :Seq[String] = {
		def rec(chain :Opt[Chain], form :SQLWriteForm[_], res :Builder[String, Seq[String]]) :Builder[String, Seq[String]] =
			if (chain.isEmpty) form match {
				case f :ChainWriteFormImpl[_, _, _] =>
					if (f.init.columnCount > 0)
						rec(Lack, f.init, res) ++= f.last.nullColumnLiterals
					else
						res ++= f.last.nullColumnLiterals
				case other =>
					res ++= other.nullColumnLiterals
			} else (form, chain) match {
				case (f :ChainWriteFormImpl[_, _, Any @unchecked], Got(i ~ l)) =>
					if (f.init.columnCount > 0)
						rec(Opt(i), f.init, res) ++= f.last.columnLiterals(Opt(l))
					else
						res ++= f.last.columnLiterals(l)
				case (other :SQLWriteForm[Chain @unchecked], Got(x)) =>
					res ++= other.columnLiterals(x)
			}
		rec(value, this, ArraySeq.newBuilder[String]).result()
	}


	override def split =
		(init.split.view.map(_.unmap(Chain.init[I])) ++
			last.split.view.map(_.unmap(Chain.last[L]))
		).to(ArraySeq)

	protected override def symbol :String = "~"
}



/** An ad-hoc implementation of a write form for any subtype of chain `I ~ L`, composed of arbitrary write forms
  * for `I` and `L`.
  * @see [[net.noresttherein.oldsql.schema.forms.ChainWriteForm]]
  */
private[forms] class ChainSQLWriteForm[C[+A <: I, +B <: L] >: Null <: A ~ B, -I <: Chain, -L, -V]
                     (override val init :SQLWriteForm[I], override val last :SQLWriteForm[L],
                      protected override val symbol :String)
	extends UnspecifiedChainForm[SQLWriteForm[I], SQLWriteForm[L]](init, last) with ChainWriteFormImpl[C, I, L]




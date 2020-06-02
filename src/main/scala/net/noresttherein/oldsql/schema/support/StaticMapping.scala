package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Buff.{ExtraSelect, OptionalSelect, SelectAudit}
import net.noresttherein.oldsql.schema.TypedMapping
import net.noresttherein.oldsql.slang._

/**
  * @author Marcin Mościcki
  */
trait StaticMapping[S, O] extends TypedMapping[S, O] {



	override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this) match {
		case res if buffs.isEmpty => res
		case res :Some[S] =>
			if (audits.isEmpty) res else Some((res.get /: audits) { (acc, f) => f(acc) })
		case _ =>
			val res = default.get
			if (res.isDefined) res else explicit
	}

	private val audits = Lazy(SelectAudit.Audit(this))
	private val default = Lazy(OptionalSelect.Value(this))
	private val explicit = Lazy(ExtraSelect.Value(this))



	/** Performs the assembly of this mapping's subject from the components. This method is called in a double-dispatch
	  * from `optionally`/`apply`, which should be used by external mappings, as they are responsible for
	  * introducing default values and any manipulation of the final values. The standard implementation
	  * invokes [[net.noresttherein.oldsql.schema.support.StaticMapping.construct construct(pieces)]] as long as
	  * [[net.noresttherein.oldsql.schema.support.StaticMapping.isDefined isDefined(pieces)]] returns `true`. Additionally,
	  * all `NoSuchElementException` exceptions (thrown by default by components `apply` method when no value can
	  * be assembled or is preset) are caught and result in returning `None`. All other exceptions,
	  * including `NullPointerException` which may result from unavailable columns, are propagated. Subclasses should
	  * override those methods instead of `map`.
	  * @return `Some(construct(pieces))` if `isDefined(pieces)` returns `true` or `None` if it returns `false` or
	  *        a `NoSuchElementException` is caught.
	  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.construct construct]]
	  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.isDefined isDefined]]
	  */
	override def assemble(pieces: Pieces): Option[S] =
		try {
			isDefined(pieces) ifTrue construct(pieces)
		} catch {
			case _ :NoSuchElementException => None
		}

	/** The final target of the assembly process for this mapping invoked directly by `map` (and, indirectly,
	  * `optionally`/`apply`. It is left to implement for subclasses and, in order to make it the simplest possible,
	  * is not responsible for recognizing whether a value can be assembled, but rather this functionality is
	  * shared by `isDefined` method, which can force `map` to return `None` without calling `construct`,
	  * and catching later any `NoSuchElementException`s thrown from  this method and resulting from a failed assembly
	  * of a subcomponent. Another difference is that `pieces` is declared as an implicit parameter, which coupled
	  * with an implicit conversion of `RefinedMapping[O, T]` to `T` in its presence, allows to use the components directly
	  * as arguments to the constructor of the returned subject instance. For example:
	  * {{{
	  *     case class Address(country :String, city :String, zip :String, street :String, no :String)
	  *
	  *     class AddressSchema[O] extends StaticMapping[O, Address] {
	  *         val country = column(_.country)
	  *         val city = column(_.city)
	  *         val zip = column(_.zip)
	  *         val street = column(_.street)
	  *         val no = column(_.no)
	  *
	  *         override def construct(implicit pieces :Pieces) :Address =
	  *             Address(country, city, zip, street, no)
	  *     }
	  * }}}
	  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.isDefined isDefined]]
	  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.assemble map]]
	  */
	protected def construct(implicit pieces :Pieces) :S

	/** Verifies the presence of necessary subcomponents in the input pieces for the assembly to be successful.
	  * This method is called from `map` in order to possibly prevent it from proceeding with the assembly
	  * and  calling `construct`, but return `None` instead. The contract obliges it only detect the situations
	  * where `construct` would certainly fail with an exception, but not necessarily all of them. It is designed
	  * primarily with the thought of outer joins where all columns of a last can carry `null` values.
	  * For this reason, it simply always returns `true`, but entity tables override it with a check of availability
	  * of the primary key. The subclasses are free to implement any condition here.
	  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.construct construct]]
	  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.assemble map]]
	  */
	protected def isDefined(pieces :Pieces) :Boolean = true



	/** Implicitly convert a component of this instance into its subject value by assembling it from implicitly
	  * available `Pieces` for this mapping. This will work for both direct components and indirect subcomponents.
	  */
	implicit def valueOf[T](component :Component[T])(implicit pieces :Pieces) :T =
		pieces(apply(component))



}

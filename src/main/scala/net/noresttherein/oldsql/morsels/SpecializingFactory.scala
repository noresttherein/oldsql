package net.noresttherein.oldsql.morsels

import net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase
import net.noresttherein.oldsql.morsels.generic.Self






/** A simple framework for implicit factories used to narrow down the result of a generic method
  * based on the type of its argument `A[T]` and presence of its type class `E[T]`. Its naming assumes the distinction
  * is between some generic result type `G[T]` and its subtype `S[T] &lt;: G[T]`
  * The main factory method [[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase.apply apply]]
  * takes two arguments: one, explicit, `X &lt;: A[T]`, and an implicit
  * [[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase.DedicatedFactory DedicatedFactory]]`[X, T]`,
  * which defines the return type `Res` of the method. The resolution of the result type depends on the type `X`
  * of the explicit argument and existence of implicit evidence `E[T]/SE[T]`. If the argument type is `X &lt;: SA[T]`,
  * ''and'' an implicit 'specific' type class `SE[T]` exists, then the provided factory will be for values
  * of `S[T]`. Otherwise it will produce the more general `G[T]`, assuming an implicit type class `E[T]` exists.
  * This happens by double dispatch starting from the `apply` method and involving the implicit factory argument;
  * the cases outlined above result in invoking either
  * [[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase.generalResult generalResult]] or
  * [[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase.specificResult specificResult]],
  * depending on the class of the factory argument. Note that, while this trait declares only a single specific
  * subclass of both the result and explicit and implicit arguments, the subclasses can add any additional
  * 'specific' cases by defining additional subclasses of
  * [[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase.DedicatedFactory DedicatedFactory]]
  * and their implicit values.
  *
  * @tparam A  type constructor of the upper bound of the explicit arguments passed to the factory.
  * @tparam SA type constructor of a specialized, 'single-column' version of the argument type `A[T]`.
  * @tparam E  type constructor of the more generic, 'multi-column', implicit type class (for example, `SQLForm`).
  * @tparam SE type constructor of the more specific, 'single column', implicit type class (for example, `ColumnForm`).
  * @tparam G  type constructor of the more generic method result ('the multi-column value').
  * @tparam S  type constructor of the more specific method result ('the single column value').
  * @see [[net.noresttherein.oldsql.schema.SQLForm.FormBasedFactory]]
  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.ReadFormBasedFactory]]
  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormBasedFactory]]
  * @author Marcin Mościcki
  */
trait SpecializingFactory[A[_], SA[T] <: A[T], E[_], SE[T] <: E[T], G[_], S[T] <: G[T]]
	extends SpecializingFactoryBase[A, SA, E, SE, G, S]
{
	implicit def specificFactory[T :SE] :SpecificFactory[T] = new SpecificFactory[T]
	implicit def specificEvidenceFactory[T](ev :SE[T]) :SpecificFactory[T] = new SpecificFactory[T]()(ev)
}



object SpecializingFactory {
	type ArgOf[A] = { type T[_] = A }

	/** A [[net.noresttherein.oldsql.morsels.SpecializingFactory SpecializingFactory]] using a fixed explicit
	  * argument type `A`. The result types `S[T]` and `G[T]` depend only on the implicit argument type `E[T]/SE[T]`.
	  */
	type TypelessFactory[A, E[_], SE[T] <: E[T], G[_], S[T] <: G[T]] =
		SpecializingFactory[ArgOf[A]#T, ArgOf[A]#T, E, SE, G, S]

	/** A [[net.noresttherein.oldsql.morsels.SpecializingFactory SpecializingFactory]] using the type parameter `T`
	  * of the result types `S[T]/G[T]` as the explicit argument.
	  */
	type SimpleFactory[E[_], SE[T] <: E[T], G[_], S[T] <: G[T]] = SpecializingFactory[Self, Self, E, SE, G, S]


	/** Base trait of [[net.noresttherein.oldsql.morsels.SpecializingFactory SpecializingFactory]] extracted
	  * for implicit priority enforcement. Defines an `apply` method accepting an argument of `A[T]`,
	  * an implicit argument of `E[T]`, and returning `G[T]`. However, if both explicit and implicit arguments
	  * are specialized subtypes of the former, `SA[T]` and `SE[T]`, then a specialized subtype `S[T] <: G[T]`
	  * is the method's return type, as defined by
	  * [[net.noresttherein.oldsql.morsels.SpecializingFactory.Factory Factory]].
	  * @tparam A  type constructor of the upper bound of the explicit arguments passed to the factory.
	  * @tparam SA type constructor of a specialized subtype of the argument type `A[T]`.
	  * @tparam E  type constructor of the more generic, implicit type class (for example, `SQLForm`).
	  * @tparam SE type constructor of the more specific, implicit type class (for example, `ColumnForm`).
	  * @tparam G  type constructor of the more generic method result.
	  * @tparam S  type constructor of the more specific method result.
	  */
	sealed trait SpecializingFactoryBase[A[_], SA[T] <: A[T], E[_], SE[T] <: E[T], G[_], S[T] <: G[T]] {

		/** A factory method accepting some subtype of the generic argument type `A[T]` and an implicit
		  * `DedicatedFactory` type class for this type. The latter defines the result type as either the most
		  * generic `G[T]` or its subtype `S[T]`, if the argument is a `SA[T]` and more specific `DedicatedFactory`
		  * is available. The existence of the latter depends on the existence of a type class `E[T]`
		  * or its subtype `SE[T]`. While two default implicit implementations are provided, subclasses can freely
		  * extend this framework to introduce more triplets of type constructors `{argumentType, typeClass, returnType}`.
		  *
		  * The implementation forwards the call in double dispatch to either
		  * [[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase.generalResult generalResult]]
		  * or [[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase.specificResult specificResult]].
		  *
		  * Note that implicit conversions `E[T] => GeneralFactory[X, T]` and `SE[T] => SpecificFactory[X, T]` exist,
		  * meaning you can explicitly provide `E[T]` or `SE[T]` as the implicit argument.
		  */
		def apply[X <: A[T], T](arg :X)(implicit factory :DedicatedFactory[X, T]) :factory.Res = factory(arg)

		/** Default callback method from [[net.noresttherein.oldsql.morsels.SpecializingFactory.apply apply]]
		  * invoked by [[net.noresttherein.oldsql.morsels.SpecializingFactory.GeneralFactory GeneralFactory]].
		  */
		protected def generalResult[T :E](arg :A[T]) :G[T]

		/** Callback method from [[net.noresttherein.oldsql.morsels.SpecializingFactory.apply apply]] invoked by
		  * [[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecificFactory SpecificFactory]] if the argument
		  * is of the more specific type and the specific type class for `T` exists.
		  */
		protected def specificResult[T :SE](arg :SA[T]) :S[T]

		/** An implicit factory of values of `G[T]` or its subtype `S[T]` from values of `A[T]`. The exact result type
		  * is narrowed down in concrete implementations, whose existence depends on the existence of implicit
		  * type class `E[T]` or its more specific version `SE[T]`. Note that in the most generic scenario,
		  * values of this type will ''always'' be the more generic producer of `G[T]`. This type is only really
		  * useful in the common case when the more specific argument `SA[T]` is the same type as the more generic
		  * argument `A[T]`.
		  *
		  * As an example, [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] is one of many
		  * SQL expressions which extend the enclosing trait specifying `A[T] =:= SA[T] =:= T`,
		  * `E[T] =:= `[[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[T]`
		  * and `SE[T] =:= `[[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]`[T]`.
		  * Thus, two implementations exist: one for `BoundParam[T]` itself, and one for its subtype
		  * [[net.noresttherein.oldsql.sql.ast.BoundColumnParam BoundParamColumn]]`[T]`,
		  * which will always have precedence if an implicit `ColumnForm[T]` exists, falling back to the more generic
		  * instance if only an `SQLForm[T]` is available.
		  * @see [[net.noresttherein.oldsql.morsels.SpecializingFactory SpecializingFactory]]
		  */ //todo: rename to ResultConstructor
		type Factory[T] = DedicatedFactory[A[T], T]

		/** A conversion of values of type `X`, being either `A[T]` or `SA[T]`, producing values of `G[T]`
		  * or its subtype `S[T]`. The exact return type is exposed as the `Res` member type and subclasses
		  * can narrow it down - specifically to `S[T]`.
		  * By default, there are two implicit values defined in the same scope as this class:
		  *   1. `DedicatedFactory[A[T], T] { type Res = G[T] }`
		  *   1. `DedicatedFactory[SA[T], T] { type Res = S[T] }`
		  *      Existence of both depends on presence of, respectively, `E[T]` and `SE[T]`, with more specific
		  *      variant having strict precedence. This type is contravariant in the argument type, so
		  *      the more generic factory can be used instead of the more specific one if the latter is unavailable.
		  *      Note that for the common case when `A[T] =:= SA[T]` there is a type alias available in the same scope:
		  *      [[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase.Factory Factory]]`[T]`
		  *      `=:= `[[net.noresttherein.oldsql.morsels.SpecializingFactory.SpecializingFactoryBase.DedicatedFactory]]`[A[T], T]`,
		  *      which, in another common scenario when `A[T] =:= T`, reduces this factory to conversion `T => _ &lt;: G[T]`.
		  *
		  * @tparam X argument type accepted by this factory.
		  * @tparam T the type parameter shared by the return type, the required type class and the argument type.
		  */ //implicitNotFound is not helpful at all, as the type parameters from the enclosing class are inaccessible
		//todo: rename to SuitableResult
		abstract class DedicatedFactory[-X <: A[T], T](implicit val evidence :E[T]) {
			/** The type of objects returned by this factory (and methods accepting it as an implicit parameter). */
			type Res <: G[T]
			def apply(arg :X) :Res
		}
		//todo: rename to GeneralResult
		class GeneralFactory[T :E] extends DedicatedFactory[A[T], T] {
			type Res = G[T]
			def apply(arg :A[T]) :G[T] = generalResult(arg)
		}
		//todo: rename to SpecificResult
		class SpecificFactory[T :SE] extends DedicatedFactory[SA[T], T] {
			type Res = S[T]
			def apply(arg :SA[T]) :S[T] = specificResult(arg)
		}
		implicit def generalFactory[T :E] :GeneralFactory[T] = new GeneralFactory[T]
		implicit def generalEvidenceFactory[T](ev :E[T]) :GeneralFactory[T] = new GeneralFactory[T]()(ev)
	}

}

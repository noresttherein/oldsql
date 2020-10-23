package net.noresttherein.oldsql.morsels

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.morsels.ColumnBasedFactory.MultiColumnFactory
import net.noresttherein.oldsql.morsels.generic.Self






/** A simple framework for implicit factories used to narrow down the result of a generic method 
  * based on the type of its argument and presence of its type class. Its naming assumes the distinction is between 
  * some more generic result type `M[T]`, in some way associated with multiple SQL columns, 
  * and its subtype `S[T] <: M[T]`, which represents specialization of the latter for the case of a single column. 
  * The main factory method [[net.noresttherein.oldsql.morsels.ColumnBasedFactory.MultiColumnFactory.apply apply]]
  * takes two arguments: one, explicit, `X <: A[T]`, and an implicit
  * [[net.noresttherein.oldsql.morsels.ColumnBasedFactory.MultiColumnFactory.SpecificFactory SpecificFactory]]`[X, T]`, which defines
  * the return type `Res` of the method. The resolution of the result type depends on the type `X` of the explicit
  * argument and existence of implicit `F[T]/C[T]`. If the argument type is `X <: B[T]`, ''and'' an implicit
  * 'single column' type class `C[T]` exists, then provided factory will be for values of `S[T]`.
  * Otherwise it will produce the more generic `M[T]`, assuming an implicit 'multi-column' type class `F[T]` exists.
  * The basic use case involves some type of SQL forms: `C[T] >: ColumnForM[T]` and `F[T] >: C[T]` and the original
  * type class `M[T]` is exposed by a `form` property. This class however does not mandate it, as it processes neither
  * the explicit, nor implicit arguments, leaving the implementation of methods
  * [[net.noresttherein.oldsql.morsels.ColumnBasedFactory.MultiColumnFactory.multiColumn multiColumn]] and
  * [[net.noresttherein.oldsql.morsels.ColumnBasedFactory.MultiColumnFactory.singleColumn singleColumn]] for subclasses.
  * @tparam A[T] type constructor of the upper bound of the explicit arguments passed to the factory.
  * @tparam B[T] type constructor of a specialized, 'single-column' version of the argument type `A[T]`.
  * @tparam F[T] type constructor of the more generic, 'multi-column', implicit type class (for example, `SQLForm`).
  * @tparam C[T] type constructor of the more specific, 'single column', implicit type class (for example, `ColumnForm`).
  * @tparam M[T] type constructor of the more generic method result ('the multi-column value').
  * @tparam S[T] type constructor of the more specific method result ('the single column value').
  * @see [[net.noresttherein.oldsql.schema.SQLForm.FormFunction]]
  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.ReadFormFunction]]
  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormFunction]]
  * @author Marcin Mościcki
  */
trait ColumnBasedFactory[A[_], B[T] <: A[T], F[_], C[T] <: F[T], M[_], S[T] <: M[T]]
	extends MultiColumnFactory[A, B, F, C, M, S]
{
	implicit def singleColumnValue[X :C] = new SingleColumn[X]
}



object ColumnBasedFactory {
	type ArgOf[A] = { type T[X] = A }

	type TypelessFactory[A, F[_], C[T] <: F[T], M[_], S[T] <: M[T]] =
		ColumnBasedFactory[ArgOf[A]#T, ArgOf[A]#T, F, C, M, S]

	type SimpleFactory[F[_], C[T] <: F[T], M[_], S[T] <: M[T]] = ColumnBasedFactory[Self, Self, F, C, M, S]


	sealed trait MultiColumnFactory[A[_], B[T] <: A[T], F[_], C[T] <: F[T], M[_], S[T] <: M[T]] {

		/** A factory method accepting some subtype of the generic argument type `A[T]` and an implicit
		  * `SpecificFactory` type class for this type. The latter defines the result type as either the most
		  * generic `M[T]` or its subtype `S[T]`, if the argument is a `B[T]` and more specific `SpecificFactory`
		  * is available. The existence of the latter depends on the existence of a type class `F[T]`
		  * or its subtype `C[T]`. While two default implicit implementations are provided, subclasses can freely
		  * extend this framework to introduce more triplets of type constructors `{argumentType, typeClass, returnType}`.
		  *
		  * The implementation forwards the call in double dispatch to either
		  * [[net.noresttherein.oldsql.morsels.ColumnBasedFactory.MultiColumnFactory.multiColumn multiColumn]]
		  * or [[net.noresttherein.oldsql.morsels.ColumnBasedFactory.MultiColumnFactory.singleColumn singleColumn]].
		  */
		def apply[X <: A[T], T](arg :X)(implicit factory :SpecificFactory[X, T]) :factory.Res = factory(arg)

		protected def multiColumn[T :F](arg :A[T]) :M[T]
		protected def singleColumn[T :C](arg :B[T]) :S[T]

		/** An implicit factory of values of `M[T]` or its subtype `S[T]` from values of `A[T]`. The exact result type
		  * is narrowed down in concrete implementations, whose existence depends on the existence of implicit
		  * type class `F[T]` or its more specific version `C[T]`. Note that in the most generic scenario,
		  * values of this type will ''always'' be the more generic producer of `M[T]`. This type is only really
		  * useful in the common case when the more specific argument `B[T]` is the same type as the more generic
		  * argument `A[T]`.
		  *
		  * As an example, [[net.noresttherein.oldsql.sql.SQLTerm.SQLParameter SQLParameter]] is one of many
		  * SQL expressions which extend the enclosing trait specifying `A[T] =:= B[T] = T`,
		  * `F[T] =:= `[[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[T]`
		  * and `C[T] =:= `[[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]`[T]`.
		  * Thus, two implementations exist: one for `SQLParameter[T]` itself, and one for its subtype
		  * [[net.noresttherein.oldsql.sql.SQLTerm.SQLParameterColumn SQLParameterColumn]]`[T]`,
		  * which will always have precedence if an implicit `ColumnForm[T]` exists, falling back to the more generic
		  * instance if only an `SQLForm[T]` is available.
		  * @see [[net.noresttherein.oldsql.morsels.ColumnBasedFactory ColumnBasedFactory]]
		  */
		type Factory[T] = SpecificFactory[A[T], T]

		/** A conversion of values of type `X`, being either `A[T]` or `B[T]`, producing values of `M[T]`
		  * or its subtype `S[T]`. The exact return type is exposed as the `Res` member type and subclasses
		  * can narrow it down - specifically to `S[T]`.
		  * By default, there are two implicit values defined in the same scope as this class:
		  *   1. `SpecificFactory[A[T], T] { type Res = M[T] }`
		  *   1. `SpecificFactory[B[T], T] { type Res = S[T] }`
		  * Existence of both depends on presence of, respectively, `F[T]` and `C[T]`, with more specific
		  * variant having strict precedence. This type is contravariant in the argument type, so
		  * the more generic factory can be used instead of the more specific one if the latter is unavailable.
		  * Note that for the common case when `A[T] =:= B[T]` there is a type alias available in the same scope:
		  * [[net.noresttherein.oldsql.morsels.ColumnBasedFactory.MultiColumnFactory.Factory Factory]]`[T] =:= SpecificFactory[A[T], T]`,
		  * which, in another common scenario when `A[T] =:= T` reduces this factory to conversion `T => _ <: M[T]`.
		  *
		  * @tparam X argument type accepted by this factory.
		  * @tparam T the type parameter shared by the return type, the required type class and the argument type.
		  */ //implicitNotFound is not helpful at all, as the type parameters from the enclosing class are inaccessible
		abstract class SpecificFactory[-X <: A[T], T](implicit val form :F[T]) {
			type Res <: M[T]
			def apply(arg :X) :Res
		}

		class MultiColumn[T :F] extends SpecificFactory[A[T], T] {
			type Res = M[T]
			def apply(arg :A[T]) :M[T] = multiColumn(arg)
		}

		class SingleColumn[T :C] extends SpecificFactory[B[T], T] {
			type Res = S[T]
			def apply(arg :B[T]) :S[T] = singleColumn(arg)
		}

		implicit def multiColumnValue[X :F] = new MultiColumn[X]
	}

}

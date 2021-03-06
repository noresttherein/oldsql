import net.noresttherein.oldsql.schema.{Mapping, Relation, Table}
import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.sql.{AndFrom, From, InnerJoin, Join, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.RowProduct.RowDecomposition

/**
  * @author Marcin Mościcki
  */
object compilerHangup extends App {{
	class A[O] extends FormMapping[Int, O]
	class B[O] extends FormMapping[Int, O]
	val A = Table("A", new A[Any])
	val B = Table("B", new B[Any])

//	val a :A[RowProduct AndFrom A Join B] = ???
//	val b :B[RowProduct AndFrom B] = ???
	val f = From(A) join B
	val a = f[A]
	val b = f[B]

	//	val from = From(A) join B groupBy a as "a" by b as "b" //this compiles
//	val from = From(A) join B groupBy (_[A]) by (_[B]) //takes forever for some reason, but compiles
//	val from = From(A) join B groupBy (t => a) by (t => b)
	val from = From(A) join B groupBy (_[A]) as "A" by (_[B]) as "B"//StackOverflowError in compile server

	val from2 = f.self groupBy (_[A]) by (_[B])
//
//
//	trait Invariant[B]
//	val from = From(A)
//	(new Invariant[from.Self] {}) :Invariant[From[A]]

//	def decompose[F <: RowProduct, P <: U, U <: RowProduct](f :F)(implicit deco :RowDecomposition[F, P, U]) = ???
//
//	decompose(from.self :from.Self)

//	println(scala.reflect.runtime.universe.reify {
//	}.tree)


}}

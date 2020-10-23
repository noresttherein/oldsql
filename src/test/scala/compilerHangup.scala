import net.noresttherein.oldsql.schema.{BaseMapping, Mapping, Relation}
import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.sql.{AndFrom, From, FromClause, InnerJoin, Join, SQLExpression}
import net.noresttherein.oldsql.sql.FromClause.ClauseDecomposition

/**
  * @author Marcin Mościcki
  */
object compilerHangup extends App {
	class A[O] extends FormMapping[Int, O]
	class B[O] extends FormMapping[Int, O]
	val A = Relation("A", new A[Any])
	val B = Relation("B", new B[Any])

//	val a :A[FromClause AndFrom A Join B] = ???
//	val b :B[FromClause AndFrom B] = ???
	val f = From(A) join B
	val a = f[A]
	val b = f[B]

	//	val from = From(A) join B groupBy a as "a" by b as "b" //this compiles
//	val from = From(A) join B groupBy (_[A]) by (_[B]) //takes forever for some reason, but compiles
//	val from = From(A) join B groupBy (t => a) by (t => b)
//	val from = From(A) join B groupBy (_[A]) as "A" by (_[B])//StackOverflowError in compile server

//	val from = f.self groupBy (_[A]) by (_[B])
//
//
//	trait Invariant[B]
//	val from = From(A)
//	(new Invariant[from.Self] {}) :Invariant[From[A]]

//	def decompose[F <: FromClause, P <: U, U <: FromClause](f :F)(implicit deco :ClauseDecomposition[F, P, U]) = ???
//
//	decompose(from.self :from.Self)

//	println(scala.reflect.runtime.universe.reify {
//	}.tree)


}

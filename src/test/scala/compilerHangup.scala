import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.sql.{AndFrom, From, FromClause, Join}

/**
  * @author Marcin Mościcki
  */
object compilerHangup {
	trait A[O] extends BaseMapping[Int, O];
	trait B[O] extends BaseMapping[Int, O];
	val A :Relation[A] = ???;
	val B :Relation[B] = ???;

//	val a :A[FromClause AndFrom A Join B] = ???
//	val b :B[FromClause AndFrom B] = ???
	val f = From(A) join B
	val a = f[A]
	val b = f[B]
//	val from = From(A) join B groupBy a as "a" by b as "b" //this compiles
//	val from = From(A) join B groupBy (_[A]) by (_[B]) //takes forever for some reason, but compiles
	val from = From(A) join B groupBy (_[A]) as "A" by (_[B])//StackOverflowError in compile server
}

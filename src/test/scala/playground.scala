import net.noresttherein.oldsql.schema.Relation
import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.sql.{AndFrom, From, FromClause, InnerJoin, Join, JoinParam, LeftJoin, RightJoin, Subselect}
import net.noresttherein.oldsql.sql.FromClause.As
import net.noresttherein.oldsql.sql.UnboundParam.FromParam






/**
  * @author Marcin Mościcki
  */
object playground extends App {
	class A[O] extends FormMapping[Int, O]
	class B[O] extends FormMapping[Int, O]
	class C[O] extends FormMapping[Int, O]
	class D[O] extends FormMapping[Int, O]
	class E[O] extends FormMapping[Int, O]
	val A :Relation[A] = Relation("A", new A[Any])
	val B :Relation[B] = Relation("B", new B[Any])
	val C :Relation[C] = Relation("C", new C[Any])
	val D :Relation[D] = Relation("D", new D[Any])
	val E :Relation[E] = Relation("E", new E[Any])

	val f = From(A) as "A" leftJoin B as "B" rightJoin C as "C" subselect D as "D" join E as "E"
//	val a = f[A]; val b = f[B]; val c = f[C]; val d = f[D]; val e = f[E]
//	val a = f(0); val b = f(1); val c = f(2); val d = f(3); val e = f(4)
	val a = f(-5); val b = f(-4); val c = f(-3); val d = f(-2); val e = f(-1)
//	val a = f("A"); val b = f("B"); val c = f("C"); val d = f("D"); val e = f("E")
//	val e = f.of[Int]
	a :A[FromClause AndFrom A Join B Join C Subselect D Join E]
	b :B[FromClause AndFrom B Join C Subselect D Join E]
	c :C[FromClause AndFrom C Subselect D Join E]
	d :D[FromClause AndFrom D Join E]
	e :E[FromClause AndFrom E]

	//todo: make a method which creates a JoinParam _ As _ in one go.
	val params = From(A) as "A" param[Int] "p1" as "P1" join B as "B" param[Long] "p2" as "P2" join C as "C" param[String] "p3" as "P3"

//	val params2 = From(A) ? [Int] "p1"

//	def ?:[X] = ???
//	def ?:[String :ValueOf, Int] = ???
	import net.noresttherein.oldsql.sql.UnboundParam.?:
	?:[Int]
	"param".?:[Int]
	?:["p1", Int]

	val params3 = From(A) param ?:[Int]
//	From(A) param ?:["P2", Int]
	val p1 = params.?[Int]; val p2 = params.?[Long]; val p3 = params.?[String]
//	val p1 = params ? 0; val p2 = params ? 1; val p3 = params ? 2
//	val p1 = params ? -3; val p2 = params ? -2; val p3 = params ? -1

	p1 :FromParam[Int, FromClause AndFrom FromParam.Of[Int]#P Join B JoinParam FromParam.Of[Long]#P Join C JoinParam FromParam.Of[String]#P]
	p2 :FromParam[Long, FromClause AndFrom FromParam.Of[Long]#P Join C JoinParam FromParam.Of[String]#P]
	p3 :FromParam[String, FromClause AndFrom FromParam.Of[String]#P]


	println(scala.reflect.runtime.universe.reify {
		params.of[Int]
	}.tree)
}

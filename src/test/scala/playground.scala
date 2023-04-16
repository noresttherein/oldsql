import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.sql.{Dual, RowProduct}
import net.noresttherein.oldsql.sql.ast.QuerySQL

/**
  * @author Marcin Mościcki
  */
object playground extends App {
	import net.noresttherein.oldsql.sql.*
//	import net.noresttherein.oldsql.sql.With.CTEName
//	"dupa" as (Dual select * :QuerySQL[RowProduct, @~])

	class Arg {
		def apply[T](f :Arg => T) = f(this)
	}
	class This {
		def and(arg :Arg) :Arg = arg
	}
	new This and (new Arg) { arg :Arg => arg }

/*
	/*class Equiv[A, B]

	class Rank2Equiv {
		implicit def liftBoth[X, Y, Z](implicit left :Lift[X, Z], right :Lift[Y, Z]) = new Equiv[X, Y]
	}
	class Rank1Equiv extends Rank2Equiv {
		implicit def liftLeft[X, Y](implicit lift :Lift[X, Y]) = new Equiv[X, Y]
		implicit def liftRight[X, Y](implicit lift :Lift[Y, X]) = new Equiv[X, Y]
	}
	object Equiv extends Rank1Equiv {
		implicit def identity[X] = new Equiv[X, X]
	}

	class Lift[X, Y]

	class Rank1Lift {
	}
	object Lift extends Rank1Lift {
		implicit val int2Long = new Lift[Int, Long]
		implicit val double2Long = new Lift[Double, Long]
		implicit def identity[X] = new Lift[X, X]
	}

	def test[X, Y, Z](x :X, y :Y)(implicit left :Lift[X, Z], right :Lift[Y, Z]) :Unit = ()
	test(1L, 1)
	test(1, 1.1)
	implicitly[Equiv[Int, Long]]
	implicitly[Equiv[Long, Int]]
	implicitly[Equiv[Double, Int]]
*/
	trait Payload {
		type T
	}
	type PayloadOf[X] = Payload { type T = X }
	trait Base {
		type Self
		type T[X] <: PayloadOf[X]
	}
	trait A extends Base { override type Self = A }
	trait B extends Base { override type Self = B }
	type Wildcard = A with B
//	trait Start extends A { type T = Start }

	class Succ[+L <: Base, R[X] <: PayloadOf[X]] extends A { type T[X] = R[X] }
	class AA[R[X] <: PayloadOf[X]] extends Succ[A, R]

	class Count[X <: Base, N <: Int with Singleton]

	class FallbackFirst {
//		implicit def first[L >: Wildcard <: Base, N[+A <: L] <: A And B, R] :First[L J R, R]= new First[L J R, R]
	}
	object Count extends FallbackFirst {
		implicit def aa[R[X] <: PayloadOf[X]] = new Count[AA[R], 0]
		implicit def wildcard[F >: Wildcard <: Base] = new Count[F, 0]
		implicit def next[L <: Base, J[+A <: L, B[X] <: R[X]] <: Succ[A, R], R[X] <: PayloadOf[X], N <: Int with Singleton]
		                 (implicit fist :Count[L, N]) :Count[J[L, R], N] = new Count[J[L, R], N]
	}

	implicit def first[F <: Base, N <: Int with Singleton](in :F)(implicit f :Count[F, N]) :f.type = f

	first(new AA)
*/

//	import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals._
//
//	val proc = ColumnFunction.of5[Byte, Short, Int, Long, String, Double]("proc")
//	val call = proc[Int][Long][String](params => 1.toByte.?.chain ~ 2.toShort.? ~ params.of[Int] ~ params.of[Long] ~ params.of[String])

/*

	type Bound
	trait DoubleBox[X, Y]
	trait Box[X]
	trait High[F[X] <: Box[X]]

	class Base[H[T] <: Box[T], -L <: Bound, X >: L <: Bound]
	          (val high :High[X], val seq :Unique[DoubleBox[_, X]] = Unique.empty[DoubleBox[_, X]])
	{
		lazy val lzy :Box[X] = x

		def copy[Y <: Bound](box :Box[Y]) :Base[Y, Y] = new Base(box)
		def bug[Y <: Bound](box :Box[Y]) :Base[Y, Y] = new Base(box)
	}

	class Wrapper[-L <: Bound, X >: L <: Bound]
	             (val content :Base[L, X], override val x :Box[X], override val seq :Unique[DoubleBox[_, X]] = Unique.empty[DoubleBox[_, X]])
		extends Base[L, X](x, seq)
	{
		override lazy val lzy :Box[X] = x

		override def bug[Y <: Bound](box :Box[Y]) :Base[Y, Y] =
			new Wrapper(content.copy(box), box) {
				override lazy val lzy = Wrapper.this.lzy.asInstanceOf[Box[Y]]
			}
	}
*/

//	call :Nothing
//	call call (params => 1.toByte.? ~ 2.toShort.? ~ params[Int] ~ params[Long] ~ params[String])
//	call :Nothing

//	class A[O] extends FormMapping[Int, O]
//	class B[O] extends FormMapping[Int, O]
//	class C[O] extends FormMapping[Int, O]
//	class D[O] extends FormMapping[Int, O]
//	class E[O] extends FormMapping[Int, O]
//	val A :Relation[A] = Relation("A", new A[Any])
//	val B :Relation[B] = Relation("B", new B[Any])
//	val C :Relation[C] = Relation("C", new C[Any])
//	val D :Relation[D] = Relation("D", new D[Any])
//	val E :Relation[E] = Relation("E", new E[Any])

//	type With[T[O] <: MappingAt[O]] = RowProduct
//	type AndWith[+L <: RowProduct, T[O] <: MappingAt[O]] = RowProduct
//	type SelectFrom[+L <: RowProduct, T[O] <: MappingAt[O]]
//	type TT = With[A] As "A" AndWith B As "B" SelectFrom C
//	type Select = RowProduct
//	type From[+L <: RowProduct, T[O] <: MappingAt[O]] = RowProduct
//	type Def = Select From A
//	type Def2 = Select With A With B With C From A

//	val f = From(A) as "A" param[Int] "int" param[Long] "long" param[String] "string"
//	val f :From[A] As "A" WithParam Int WithParam Long WithParam String = null
//	val res = f("string")(1L)(1)
//	res :From[A]

//	println(scala.reflect.runtime.universe.reify(
//	).tree)

	//	val a = f("A")
//	a :Nothing
/*
	val f = From(A) as "A" leftJoin B as "B" rightJoin C as "C" subselect D as "D" join E as "E"
//	val a = f[A]; val b = f[B]; val c = f[C]; val d = f[D]; val e = f[E]
//	val a = f(0); val b = f(1); val c = f(2); val d = f(3); val e = f(4)
	val a = f(-5); val b = f(-4); val c = f(-3); val d = f(-2); val e = f(-1)
//	val a = f("A"); val b = f("B"); val c = f("C"); val d = f("D"); val e = f("E")
//	val e = f.of[Int]
	a :A[RowProduct AndFrom A Join B Join C Subselect D Join E]
	b :B[RowProduct AndFrom B Join C Subselect D Join E]
	c :C[RowProduct AndFrom C Subselect D Join E]
	d :D[RowProduct AndFrom D Join E]
	e :E[RowProduct AndFrom E]

	val params = From(A) as "A" param[Int] "p1" as "P1" join B as "B" param[Long] "p2" as "P2" join C as "C" param[String] "p3" as "P3"

//	val params2 = From(A) ? [Int] "p1"

//	def ?:[X] = ???
//	def ?:[String :ValueOf, Int] = ???
	import net.noresttherein.oldsql.sql.ParamClause.?:
	?:[Int]
	"param".?:[Int]
	?:["p1", Int]

	val params3 = From(A) param ?:[Int]
//	From(A) param ?:["P2", Int]
	val p1 = params.?[Int]; val p2 = params.?[Long]; val p3 = params.?[String]
//	val p1 = params ? 0; val p2 = params ? 1; val p3 = params ? 2
//	val p1 = params ? -3; val p2 = params ? -2; val p3 = params ? -1

	p1 :UnboundParam[Int, RowProduct AndFrom UnboundParam.Of[Int]#P Join B JoinParam UnboundParam.Of[Long]#P Join C JoinParam UnboundParam.Of[String]#P]
	p2 :UnboundParam[Long, RowProduct AndFrom UnboundParam.Of[Long]#P Join C JoinParam UnboundParam.Of[String]#P]
	p3 :UnboundParam[String, RowProduct AndFrom UnboundParam.Of[String]#P]


	println(scala.reflect.runtime.universe.reify {
		params.of[Int]
	}.tree)
*/
}

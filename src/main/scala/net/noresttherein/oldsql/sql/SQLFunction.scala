package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.FunctionSQL
import net.noresttherein.oldsql.sql.ast.FunctionSQL.FunctionColumn
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** A signature of an SQL function for use in SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]].
  * This is the most generic variant, which accepts arguments as a polymorphic
  * [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuple]]; most implementations will be instances
  * of the single column variant, [[net.noresttherein.oldsql.sql.ColumnFunction ColumnFunction]].
  * @tparam X a [[net.noresttherein.oldsql.collection.Chain Chain]] listing the types of all arguments of this function.
  * @tparam Y the return type of this function.
  * @author Marcin Mościcki
  */
trait SQLFunction[X <: Chain, Y] extends Serializable {
	val readForm :SQLReadForm[Y]

	val name :String

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](args :ChainTuple[F, S, X]) :SQLExpression[F, S, Y] =
		FunctionSQL(this, args)(readForm)


	protected def spell[P, F <: RowProduct](args :ChainTuple[F, LocalScope, X])
	                                       (context :SQLContext, params :Parameterization[P, F])
	                                       (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
		((spelling.function(name) +  "(") +: spelling.inline(args)(context, params)) + ")"

	private[sql] final def spell[P, F <: RowProduct](spelling :SQLSpelling)(args :ChainTuple[F, LocalScope, X])
	                                                (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
		spell(args)(context, params)(spelling)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLFunction[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :SQLFunction[_, _] if other canEqual that => name == other.name && readForm == other.readForm
		case _ => false
	}

	override def hashCode :Int = name.hashCode * 31 + readForm.hashCode

	override def toString :String = name + "[" + readForm + "]"
}



object SQLFunction {
	def apply[X <: Chain, Y :SQLReadForm](name :String) :SQLFunction[X, Y] = new BaseFunction(name)

	class BaseFunction[X <: Chain, Y](override val name :String)(implicit override val readForm :SQLReadForm[Y])
		extends SQLFunction[X, Y]

}






/** A signature of a single column SQL function for use in SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]].
  * This is generic base class, which accepts arguments as a polymorphic
  * [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuple]]; in most cases it is more convenient to use one of
  * its subclasses dedicated to functions of a fixed arity. They can be created using `ofX` methods from
  * the [[net.noresttherein.oldsql.sql.ColumnFunction$ companion]] object to this trait:
  * {{{
  *    val SYSDATE = ColumnFunction.of0[LocalDateTime]("SYSDATE")
  *    val now = SYSDATE()
  *    val TO_DATE = ColumnFunction.of2[String, String, LocalDate]("TO_DATE")
  *    val AprilFool = TO_DATE("01.10.2020".?, "DD.MM.YYYY")
  * }}}
  * @tparam X a [[net.noresttherein.oldsql.collection.Chain Chain]] listing the types of all arguments of this function.
  * @tparam Y the return type of this function.
  * @author Marcin Mościcki
  */
trait ColumnFunction[X <: Chain, Y] extends SQLFunction[X, Y] {
	override val readForm :ColumnReadForm[Y]

	override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](args :ChainTuple[F, S, X]) :ColumnSQL[F, S, Y] =
		FunctionColumn(this, args)(readForm)
}



object ColumnFunction {

	def apply[X <: Chain, Y :ColumnReadForm](name :String) :ColumnFunction[X, Y] = new BaseColumnFunction(name)

	def of0[Y :ColumnReadForm](name :String) :ColumnFunction0[Y] = new ColumnFunction0(name)

	def of1[A, Y :ColumnReadForm](name :String) :ColumnFunction1[A, Y] = new ColumnFunction1(name)

	def of2[A, B, Y :ColumnReadForm](name :String) :ColumnFunction2[A, B, Y] = new ColumnFunction2(name)

	def of3[A, B, C, Y :ColumnReadForm](name :String) :ColumnFunction3[A, B, C, Y] = new ColumnFunction3(name)

	def of4[A, B, C, D, Y :ColumnReadForm](name :String) :ColumnFunction4[A, B, C, D, Y] = new ColumnFunction4(name)

	def of5[A, B, C, D, E, Y :ColumnReadForm](name :String) :ColumnFunction5[A, B, C, D, E, Y] = new ColumnFunction5(name)

	def of6[A, B, C, D, E, F, Y :ColumnReadForm](name :String) :ColumnFunction6[A, B, C, D, E, F, Y] =
		new ColumnFunction6(name)

	def of7[A, B, C, D, E, F, G, Y :ColumnReadForm](name :String) :ColumnFunction7[A, B, C, D, E, F, G, Y] =
		new ColumnFunction7(name)

	def of8[A, B, C, D, E, F, G, H, Y :ColumnReadForm](name :String) :ColumnFunction8[A, B, C, D, E, F, G, H, Y] =
		new ColumnFunction8(name)

	def of9[A, B, C, D, E, F, G, H, I, Y :ColumnReadForm](name :String) :ColumnFunction9[A, B, C, D, E, F, G, H, I, Y] =
		new ColumnFunction9(name)

	def of10[A, B, C, D, E, F, G, H, I, J, Y :ColumnReadForm](name :String)
			:ColumnFunction10[A, B, C, D, E, F, G, H, I, J, Y] =
		new ColumnFunction10(name)



	class BaseColumnFunction[X <: Chain, Y](override val name :String)(implicit override val readForm :ColumnReadForm[Y])
		extends ColumnFunction[X, Y]

	class ColumnFunction0[Y :ColumnReadForm](override val name :String) extends BaseColumnFunction[@~, Y](name) {
		def apply[X <: RowProduct, S >: LocalScope <: GlobalScope]() :ColumnSQL[X, S, Y] =
			apply(ChainTuple())
	}

	class ColumnFunction1[A, Y :ColumnReadForm](override val name :String) extends BaseColumnFunction[@~ ~A, Y](name) {
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope](a :ColumnSQL[X, Sc, A]) :ColumnSQL[X, Sc, Y] =
			apply(ChainTuple(a))
	}

	class ColumnFunction2[A, B, Y :ColumnReadForm](override val name :String)
		extends BaseColumnFunction[@~ ~A~B, Y](name)
	{
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope]
		         (a :ColumnSQL[X, Sc, A], b :ColumnSQL[X, Sc, B]) :ColumnSQL[X, Sc, Y] =
			apply(a ~ b)
	}

	class ColumnFunction3[A, B, C, Y :ColumnReadForm](override val name :String)
		extends BaseColumnFunction[@~ ~A~B~C, Y](name)
	{
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope]
		         (a :ColumnSQL[X, Sc, A], b :ColumnSQL[X, Sc, B], c :ColumnSQL[X, Sc, C]) :ColumnSQL[X, Sc, Y] =
			apply(a~b~c)
	}

	class ColumnFunction4[A, B, C, D, Y :ColumnReadForm](override val name :String)
		extends BaseColumnFunction[@~ ~A~B~C~D, Y](name)
	{
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope]
		         (a :ColumnSQL[X, Sc, A], b :ColumnSQL[X, Sc, B], c :ColumnSQL[X, Sc, C], d :ColumnSQL[X, Sc, D])
				:ColumnSQL[X, Sc, Y] =
			apply(a~b~c~d)
	}

	class ColumnFunction5[A, B, C, D, E, Y :ColumnReadForm](override val name :String)
		extends BaseColumnFunction[@~ ~A~B~C~D~E, Y](name)
	{
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope]
		         (a :ColumnSQL[X, Sc, A], b :ColumnSQL[X, Sc, B], c :ColumnSQL[X, Sc, C], d :ColumnSQL[X, Sc, D],
		          e :ColumnSQL[X, Sc, E])
				:ColumnSQL[X, Sc, Y] =
			apply(a~b~c~d~e)
	}

	class ColumnFunction6[A, B, C, D, E, F, Y :ColumnReadForm](override val name :String)
		extends BaseColumnFunction[@~ ~A~B~C~D~E~F, Y](name)
	{
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope]
		         (a :ColumnSQL[X, Sc, A], b :ColumnSQL[X, Sc, B], c :ColumnSQL[X, Sc, C], d :ColumnSQL[X, Sc, D],
		          e :ColumnSQL[X, Sc, E], f :ColumnSQL[X, Sc, F])
				:ColumnSQL[X, Sc, Y] =
			apply(a~b~c~d~e~f)
	}

	class ColumnFunction7[A, B, C, D, E, F, G, Y :ColumnReadForm](override val name :String)
		extends BaseColumnFunction[@~ ~A~B~C~D~E~F~G, Y](name)
	{
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope]
		         (a :ColumnSQL[X, Sc, A], b :ColumnSQL[X, Sc, B], c :ColumnSQL[X, Sc, C], d :ColumnSQL[X, Sc, D],
		          e :ColumnSQL[X, Sc, E], f :ColumnSQL[X, Sc, F], g :ColumnSQL[X, Sc, G])
				:ColumnSQL[X, Sc, Y] =
			apply(a~b~c~d~e~f~g)
	}

	class ColumnFunction8[A, B, C, D, E, F, G, H, Y :ColumnReadForm](override val name :String)
		extends BaseColumnFunction[@~ ~A~B~C~D~E~F~G~H, Y](name)
	{
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope]
		         (a :ColumnSQL[X, Sc, A], b :ColumnSQL[X, Sc, B], c :ColumnSQL[X, Sc, C], d :ColumnSQL[X, Sc, D],
		          e :ColumnSQL[X, Sc, E], f :ColumnSQL[X, Sc, F], g :ColumnSQL[X, Sc, G], h :ColumnSQL[X, Sc, H])
				:ColumnSQL[X, Sc, Y] =
			apply(a~b~c~d~e~f~g~h)
	}

	class ColumnFunction9[A, B, C, D, E, F, G, H, I, Y :ColumnReadForm](override val name :String)
		extends BaseColumnFunction[@~ ~A~B~C~D~E~F~G~H~I, Y](name)
	{
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope]
		         (a :ColumnSQL[X, Sc, A], b :ColumnSQL[X, Sc, B], c :ColumnSQL[X, Sc, C], d :ColumnSQL[X, Sc, D],
		          e :ColumnSQL[X, Sc, E], f :ColumnSQL[X, Sc, F], g :ColumnSQL[X, Sc, G], h :ColumnSQL[X, Sc, H],
		          i :ColumnSQL[X, Sc, I])
				:ColumnSQL[X, Sc, Y] =
			apply(a~b~c~d~e~f~g~h~i)
	}

	class ColumnFunction10[A, B, C, D, E, F, G, H, I, J, Y :ColumnReadForm](override val name :String)
		extends BaseColumnFunction[@~ ~A~B~C~D~E~F~G~H~I~J, Y](name)
	{
		def apply[X <: RowProduct, Sc >: LocalScope <: GlobalScope]
		         (a :ColumnSQL[X, Sc, A], b :ColumnSQL[X, Sc, B], c :ColumnSQL[X, Sc, C], d :ColumnSQL[X, Sc, D],
		          e :ColumnSQL[X, Sc, E], f :ColumnSQL[X, Sc, F], g :ColumnSQL[X, Sc, G], h :ColumnSQL[X, Sc, H],
		          i :ColumnSQL[X, Sc, I], j :ColumnSQL[X, Sc, J])
				:ColumnSQL[X, Sc, Y] =
			apply(a~b~c~d~e~f~g~h~i~j)
	}


}


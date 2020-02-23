package com.adpilot.cortb.clientapi.golem.sql

import com.adpilot.cortb.clientapi.golem._
import com.adpilot.cortb.clientapi.golem.sql.RValue.Type

//todo: sealed and ColumnDef inheritance
abstract class RValue[+T :Type] {
	import RValue._
	import RValue.Type._



	def ===[O>:T](value :RValue[O])(implicit tp :Type[O], bool :Type[Boolean]) : RValue[Boolean] =
		Equals(this, value)

	def <  [O>:T](value :RValue[O])(implicit tp :Sortable[O], bool :Type[Boolean]) :RValue[Boolean] =
		LessThan(this, value)

	def <= [O>:T](value :RValue[O])(implicit tp :Sortable[O], bool :Type[Boolean]) :RValue[Boolean] =
		LessThanEqual(this, value)

	def >  [O>:T](value :RValue[O])(implicit tp :Sortable[O], bool :Type[Boolean]) :RValue[Boolean] =
		GreaterThan(this, value)

	def >= [O>:T](value :RValue[O])(implicit tp :Sortable[O], bool :Type[Boolean]) :RValue[Boolean] =
		GreaterThanEqual(this, value)


	def in [O>:T](values :RValue[O]*)(implicit tp :In[O], bool :Type[Boolean]) :RValue[Boolean] =
		ValueIn(this, values)

	def in [O>:T](values :RValue[Seq[O]])(implicit tp :In[O], bool :Type[Boolean]) :RValue[Boolean] =
		ValueIn(this, values)

	
	def like [O>:T](value :RValue[String])(implicit tp :Like[O], bool :Type[Boolean]) :RValue[Boolean] =
		ValueLike(this :RValue[O], value)

	
	def && (value :RValue[Boolean])(implicit bool : this.type<:<RValue[Boolean], tpe :Type[Boolean]) :RValue[Boolean] =
		And(bool(this), value)

	def || (value :RValue[Boolean])(implicit bool : this.type<:<RValue[Boolean], tpe :Type[Boolean]) :RValue[Boolean] =
		Or(bool(this), value)

	def + [O>:T :Additive](value :RValue[O]) :RValue[O] = Plus(this, value)
	def - [O>:T :Subtractable](value :RValue[O]) :RValue[O] = Minus(this, value)
	def * [O>:T :Multiplicative](value :RValue[O]) :RValue[O] = Multiplication(this, value)
	def / [O>:T :Divisible](value :RValue[O]) :RValue[O] = Division(this, value)


}




object RValue {


	trait Type[T]
	
	object Type {
		trait Sortable[T] extends Type[T] {

		}

		trait Like[T] extends Type[T] {

		}

		trait In[T] extends Type[T] {

		}



		trait Additive[T] extends Type[T] {
//			def add(left :Expression[T], right :Expression[T]) :Expression[T]
		}

		trait Subtractable[T] extends Type[T] {
//			def subtract(left :Expression[T], right :Expression[T]) :Expression[T]
		}

		trait Multiplicative[T] extends Type[T] {
//			def multiply(left :Expression[T], right :Expression[T]) :Expression[T]
		}

		trait Divisible[T] extends Type[T] {
//			def divide(left :Expression[T], right :Expression[T]) :Expression[T]
		}

		trait Integ[T] extends Additive[T] with Subtractable[T] with Multiplicative[T] with Sortable[T]

		trait Real[T] extends Additive[T] with Subtractable[T] with Multiplicative[T] with Divisible[T] with Sortable[T]
		
		trait Bool extends Sortable[Boolean] with In[Boolean]  

		
		class SeqType[T :Type] extends Type[Seq[T]]

		class OptionType[T :Type] extends Type[Option[T]]

		implicit def SeqType[T :Type] :SeqType[T] = new SeqType[T]
//		implicit def OptionType[T :Type] = new OptionType[T]

	}

	import Type._

	implicit def apply[T :Type](value :T) :RValue[T] = Literal(value)
	implicit def apply[T :Type](column :ColumnDef[T]) :RValue[T] = ColumnValue(column)
	implicit def apply[T :Type](expressions :Seq[RValue[T]]) :RValue[Seq[T]] = SeqValue(expressions)


	protected[golem] sealed case class Literal[T :Type](value: T) extends RValue[T]

	protected[golem] sealed case class ColumnValue[T :Type](column :ColumnDef[T]) extends RValue[T]
	
	protected[golem] sealed case class SeqValue[T :Type](expressions :Seq[RValue[T]]) extends RValue[Seq[T]]

	case class Param[T :Type](value :T) extends RValue[T]

	
	protected[golem] sealed abstract class Comparison(implicit bool :Type[Boolean]) extends RValue[Boolean]

	protected[golem] sealed case class Equals[T] protected[golem] (left :RValue[T], right :RValue[T])(implicit tpe :Type[T], bool :Type[Boolean]) extends Comparison

	protected[golem] sealed case class LessThan[T] protected[golem] (left :RValue[T], right :RValue[T])(implicit tpe :Sortable[T], bool :Type[Boolean]) extends Comparison

	protected[golem] sealed case class LessThanEqual[T] protected[golem] (left :RValue[T], right :RValue[T])(implicit tpe :Sortable[T], bool :Type[Boolean]) extends Comparison

	protected[golem] sealed case class GreaterThan[T] protected[golem] (left :RValue[T], right :RValue[T])(implicit tpe :Sortable[T], bool :Type[Boolean]) extends Comparison

	protected[golem] sealed case class GreaterThanEqual[T] protected[golem] (left :RValue[T], right :RValue[T])(implicit tpe :Sortable[T], bool :Type[Boolean]) extends Comparison

	protected[golem] sealed case class ValueLike[T]  protected[golem] (left :RValue[T], right :RValue[String])(implicit tpe :Like[T], bool :Type[Boolean]) extends Comparison


	protected[golem] sealed case class ValueIn[T] protected[golem] (left :RValue[T], right :RValue[Iterable[T]])(implicit tpe :In[T], bool :Type[Boolean]) extends Comparison




	protected[golem] sealed case class And(left :RValue[Boolean], right :RValue[Boolean])(implicit bool:Type[Boolean]) extends RValue[Boolean]

	protected[golem] sealed case class Or(left :RValue[Boolean], right :RValue[Boolean])(implicit bool :Type[Boolean]) extends RValue[Boolean]



	protected[golem] sealed abstract class ArithmeticOp[T :Type] extends RValue[T]

	protected[golem] sealed case class Plus[T :Additive](left :RValue[T], right :RValue[T]) extends ArithmeticOp[T]

	protected[golem] sealed case class Minus[T :Subtractable](left :RValue[T], right :RValue[T]) extends ArithmeticOp[T]

	protected[golem] sealed case class Multiplication[T :Multiplicative](left :RValue[T], right :RValue[T]) extends ArithmeticOp[T]

	protected[golem] sealed case class Division[T :Divisible](left :RValue[T], right :RValue[T]) extends ArithmeticOp[T]

}




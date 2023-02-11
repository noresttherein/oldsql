package com.hcore.ogre.sql

import java.io.{Reader, InputStream}
import java.lang.{Byte=>JByte, Short=>JShort, Integer=>JInt, Long=>JLong, Boolean=>JBoolean, Double=>JDouble, Float=>JFloat}
import java.net.URL
import java.sql
import java.sql.{Clob, Blob, Types, ResultSet, Ref, NClob, SQLXML}

import com.hcore.ogre.mapping.PositionedResultView
import com.hcore.ogre.morsels.Names
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.SQLForm._
import com.hcore.ogre.sql.SQLReadForm.{HListReadForm, MappedSQLReadForm, AbstractTuple2ReadForm, SeqReadForm}
import com.hcore.ogre.sql.SQLWriteForm._
import shapeless.{HNil, HList}

import scala.slick.jdbc.{GetResult, SetParameter, PositionedParameters, PositionedResult}


//implicits
import extensions._



//trait SuperSQLForm {
////	def columnCount :Int
//}
//
//
//trait BaseCompositeSQLForm extends SuperSQLForm {
//	def forms :Seq[SuperSQLForm]
////	def columnCount = (0 /: forms)(_ + _.columnCount)
//}



trait SQLForm[T] extends SQLReadForm[T] with SQLWriteForm[T] {
	def apply(position :Int)(res :ResultSet) :T
	def apply(res :PositionedResult) :T
	
	def opt(position :Int)(res :ResultSet) :Option[T]
	def opt(res :PositionedResult) :Option[T]

	def apply(params :PositionedParameters, value :T) :Unit
	def nullParam(params :PositionedParameters) :Unit 

	def literal(value :T) :String 
	
	def nullValue :T 
//	def columnCount :Int


	def as[X](map :T=>X)(unmap :X=>T) :SQLForm[X] =
		MappedSQLForm[X, T](t=>Option(map(t)), x=>Option(unmap(x)), map(nullValue))(this)

	def as[X](map :T=>X, nullValue :X)(unmap :X=>T) :SQLForm[X] =
		MappedSQLForm[X, T](t=>Option(map(t)), x=>Option(unmap(x)), nullValue)(this)

	def asOpt[X>:Null](map :T=>Option[X])(unmap :X=>Option[T]) :SQLForm[X] =
		MappedSQLForm[X, T](map, unmap, null)(this)

	def asOpt[X](map :T=>Option[X], nullValue :X)(unmap :X=>Option[T]) :SQLForm[X] =
		MappedSQLForm[X, T](map, unmap, nullValue)(this)

	override def asOpt :SQLForm[Option[T]] = SQLForm.OptionType(this)


	def *[O](other :SQLForm[O]) :SQLForm[(T, O)] = new Tuple2Form()(this, other)

	override def toString = Names.unqualifiedClassName(this)
	
	def compatible(other :SQLForm[_]) = this == other
}





trait BaseAtomicForm /*extends SuperSQLForm*/ {
//	def columnCount = 1
	def sqlType :Int
}


trait AtomicForm[T] extends SQLForm[T] with AtomicReadForm[T] with AtomicWriteForm[T] {

	override def as[X](map :T=>X)(unmap :X=>T) :AtomicForm[X] =
		MappedSQLForm.atom[X, T](t=>Option(map(t)), x=>Option(unmap(x)), map(nullValue))(this)

	override def as[X](map :T=>X, nullValue :X)(unmap :X=>T) :AtomicForm[X] =
		MappedSQLForm.atom[X, T](t=>Option(map(t)), x=>Option(unmap(x)), nullValue)(this)

	override def asOpt[X>:Null](map :T=>Option[X])(unmap :X=>Option[T]) :AtomicForm[X] =
		MappedSQLForm.atom[X, T](map, unmap, null)(this)

	override def asOpt[X](map :T=>Option[X], nullValue :X)(unmap :X=>Option[T]) :AtomicForm[X] =
		MappedSQLForm.atom[X, T](map, unmap, nullValue)(this)
	

	
	override def compatible(other: SQLForm[_]): Boolean = other match {
		case a:AtomicForm[_] => a.sqlType == sqlType
		case _ => false
	}
}




trait RecordForm[T] extends SQLForm[T] {
	
}


object SQLForm extends JDBCTypes {

	def combine[T](read :SQLReadForm[T], write :SQLWriteForm[T]) :SQLForm[T] =
		new CombinedForm[T](read, write)



	class NullValue[+T](val value :T) {
		override def toString = value + ":Null"
	}

	object NullValue {
		def apply[T](value :T) :NullValue[T] = new NullValue[T](value)

		def apply[T :NullValue] :NullValue[T] = implicitly[NullValue[T]]

		def Null[T :NullValue] = implicitly[NullValue[T]].value

		implicit def nullable[T>:Null] :NullValue[T] = new NullValue[T](null)
		implicit val int = new NullValue(0)
		implicit val long = new NullValue(0L)
		implicit val short = new NullValue[Short](0)
		implicit val byte = new NullValue[Byte](0)
		implicit val boolean = new NullValue[Boolean](false)
		implicit val char = new NullValue[Char](0)
		implicit val float = new NullValue[Float](0)
		implicit val double = new NullValue[Double](0)
		implicit val unit = new NullValue(())

	}
	
	
	
	
	
	trait NullableForm[T >:Null] extends SQLForm[T] {
		def nullValue = null
	}
	
	trait NonLiteralForm[T] extends SQLForm[T] {
		override def literal(value: T): String = throw new UnsupportedOperationException(getClass.getName+".literal")
		override def nullLiteral :String = throw new UnsupportedOperationException(getClass.getName+".nullLiteral")
		override def inlineLiteral(value: T): String = throw new UnsupportedOperationException(getClass.getName+".inlineLiteral")
		override def inlineNullLiteral :String = throw new UnsupportedOperationException(getClass.getName+".inlineNullLiteral")

	}

	


	

	




	abstract class AbstractEmptyForm[T] extends SQLForm[T] {
		override def apply(position: Int)(res: ResultSet): T = nullValue
		def apply(column :String)(res :ResultSet) :T = nullValue
		
		override def opt(position :Int)(rs :ResultSet) = None
		override def opt(res: PositionedResult): Option[T] = None

		override def apply(res: PositionedResult): T = nullValue

		override def apply(params: PositionedParameters, value: T): Unit = ()

		override def writtenColumns: Int = 0

		override def readColumns = 0

		override def nullParam(params: PositionedParameters): Unit = ()

		override def toString = "EMPTY"
		
	}
	

	class EmptyForm[T](nullExpr : =>T) extends AbstractEmptyForm[T] with NonLiteralForm[T] {
		def nullValue = nullExpr
//		def nullLiteral = String.valueOf(nullExpr)
	}
	
	object EmptyForm {
		def apply[T](nullExpr : =>T) :EmptyForm[T] = new EmptyForm[T](nullExpr)
		def unapply[T](sqlType :SQLForm[T]) = sqlType.isInstanceOf[EmptyForm[_]]
	}

	
	
	
	implicit case object UnitForm extends EmptyForm[Unit](()) {
		override def toString = "UNIT"
	}

	case object NothingForm extends EmptyForm[Nothing](throw new UnsupportedOperationException("SQLType.NothingType")) {
		override def toString = "NOTHING"
	}

	case object NoneForm extends EmptyForm[Option[Nothing]](None) {
		override def toString = "NONE"
	}

	

	class UnknownForm[T] extends EmptyForm[T](throw new UnsupportedOperationException("SQLType.UnknownType")) {
		override def toString = "UNKNOWN"
	}

	object Unknown {
		def apply[T]() :UnknownForm[T] = unknown.asInstanceOf[UnknownForm[T]]
		def apply[T](tpe :SQLForm[T]) :Boolean = tpe.isInstanceOf[UnknownForm[_]]

		def unapply[T](tpe :SQLForm[T]) :Boolean = apply(tpe)

		private val unknown = new UnknownForm[Any]
	}




	case class CombinedForm[T](read :SQLReadForm[T], write :SQLWriteForm[T]) extends SQLForm[T] {

		override def opt(position: Int)(res: ResultSet): Option[T] = read.opt(position)(res)
		override def opt(res: PositionedResult): Option[T] = read.opt(res)

		override def nullValue: T = read.nullValue

		override def apply(params: PositionedParameters, value: T): Unit = write(params, value)
		override def nullParam(params: PositionedParameters): Unit = write.nullParam(params)

		override def literal(value: T): String = write.literal(value)
		override def nullLiteral: String = write.nullLiteral

		override def inlineLiteral(value: T): String = write.inlineLiteral(value)
		override def inlineNullLiteral: String = write.inlineNullLiteral


		override def writtenColumns: Int = write.writtenColumns

		override def readColumns: Int = read.readColumns

		override def toString = s"($read & $write)"
	}

	
	

	
	
	
	

	object MappedSQLForm {
//		implicit def implicitMapping[T, S](implicit map :S=>T, unmap :T=>S, sqlType :SQLType[S]) :MappedSQLType[T, S] =
//			MappedSQLType(map, unmap)

		def apply[T, S :SQLForm](map :S=>Option[T], unmap :T=>Option[S], nullValue :T) :MappedSQLForm[T, S] = SQLForm[S] match {
			case t:AtomicForm[_] =>
				atom(map, unmap, nullValue)(t.asInstanceOf[AtomicForm[S]])
			case t:RecordForm[_] =>
				new MappedSQLForm[T, S](map, unmap, nullValue) with RecordForm[T]
			case _ =>
				new MappedSQLForm[T, S](map, unmap, nullValue)

		}
		
		def atom[T, S :AtomicForm](map :S=>Option[T], unmap :T=>Option[S], nullValue :T) :MappedAtomicForm[T, S] =
			new MappedAtomicForm(map, unmap, nullValue)
		
		
	}


	class MappedSQLForm[T, S](map :S=>Option[T], val unmap :T=>Option[S], nullValue :T)(implicit override val source :SQLForm[S])
		extends MappedSQLReadForm[T, S](map, nullValue) with MappedSQLWriteForm[T, S] with SQLForm[T]
	{
		override def toString = s"_<=>$source"
	}


	class MappedAtomicForm[T, S](map :S=>Option[T], unmap :T=>Option[S], nullValue :T)(implicit override val source :AtomicForm[S])
		extends MappedSQLForm[T, S](map, unmap, nullValue) with AtomicForm[T]
	{
		override def sqlType: Int = source.sqlType

		override def apply(column: String)(res: ResultSet): T =
			source.opt(column)(res).flatMap(map) getOrElse this.nullValue

		override def opt(position: Int)(res: ResultSet): Option[T] = super[MappedSQLForm].opt(position)(res)
		override def opt(res: PositionedResult): Option[T] = super[MappedSQLForm].opt(res)

		override def writtenColumns = source.writtenColumns
		override def readColumns = source.readColumns

		override def nullParam(params: PositionedParameters): Unit = super[MappedSQLForm].nullParam(params)

	}








	class Tuple2Form[L, R](implicit val _1 :SQLForm[L], implicit val _2 :SQLForm[R])
		extends AbstractTuple2ReadForm[L, R] with AbstractTuple2WriteForm[L, R] with SQLForm[(L, R)]
	{
		override def equals(that :Any) = that match {
			case t :Tuple2Form[_, _] => (t eq this) || t._1==_1 && t._2==_2
			case _ => false
		}

		override def hashCode = (_1, _2).hashCode

		override def toString = s"(${_1},${_2})"
	}










	case class SeqForm[T](forms :Seq[SQLForm[T]]) extends SQLForm[Seq[T]] with SeqWriteForm[T] with SeqReadForm[T] {
		override def toString = forms.mkString("Seq(",",",")")
	}



	import shapeless.::



	implicit case object HNilForm extends AbstractEmptyForm[HNil] with AbstractHListWriteForm[HNil] {
		def ::[X](f :SQLForm[X]) :HListForm[X, HNil] = new HListForm(f, this)

		override def nullValue: HNil = HNil
		override def nullLiteral: String = "()"
		override def literal(value :HNil) = "()"

		override def inlineLiteral(value: HNil): String = ""
		override def inlineNullLiteral: String = ""

		override protected[sql] def elementsLiteral(sb: StringBuilder, value :HNil): StringBuilder = sb
		override def toString = "HNIL"
	}



	case class HListForm[H, T>:Null<:HList](head :SQLForm[H], tail :SQLForm[T] with AbstractHListWriteForm[T])
		extends SQLForm[H::T] with HListReadForm[H, T] with HListWriteForm[H, T]
	{
		override def toString = s"$head::" + tail.toString
	}




}




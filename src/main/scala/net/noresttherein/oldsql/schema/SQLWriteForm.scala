package net.noresttherein.oldsql.schema

import java.sql.PreparedStatement

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.{FlatMappedSQLWriteForm, LazyWriteForm, MappedSQLWriteForm, Tuple2WriteForm}
import net.noresttherein.oldsql.slang._

import scala.collection.immutable.Seq




/** Encapsulates the logic of disassembling values of `T` into values for individual columns and using them
  * to set the `PreparedStatement` parameters. As an additional functionality, it knows how to format the value
  * of `T` as an sql literal for verbatim inclusion as a constant into the SQL (rather than a parameter).
  * Whether or not `null` values are allowed depends on actual implementation. No `NullPointerException` will
  * be thrown directly by forms in this library, but `null` values may be passed over to client's mapping functions
  * used to adapt a form from one type to another.
  * Implementations should provide a pure contract, in particular ''always'' setting the same number of consecutive
  * parameters, defined as `writtenColumns`. This makes it a lower-level counterpart of
  * [[net.noresttherein.oldsql.schema.Mapping]], which can be represented by possibly many forms, depending on the
  * column lists included as both the 'insert/update' and `query/where` portions of a statement.
  * All implementations must be thread safe.
  * @see [[net.noresttherein.oldsql.schema.SQLReadForm]]
  * @see [[net.noresttherein.oldsql.schema.SQLForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  */
trait SQLWriteForm[-T] {

	/** Set the values of parameters `&lt;position..position+writtenColumns)` of the given `PreparedStatement` to
	  * the values obtained from the given value of `T`. This method simply delegates to `set` or `setNull`, depending
	  * on whether the value is defined.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.set]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull]]
	  */
	def setOpt(position  :Int)(statement :PreparedStatement, value :Option[T]) :Unit = value match {
		case Some(x) => set(position)(statement, x)
		case _ => setNull(position)(statement)
	}

	/** Set the values of parameters `&lt;position..position+writtenColumns)` of the given `PreparedStatement` to
	  * the values obtained from the given value of `T`. While forms for reference types can in theory accept
	  * `null` values, client code should not assume that passing `null` to this method will be handled gracefully
	  * by arbitrary forms. This would be impossible to achieve for forms of built-in value types, which will always
	  * throw a `NullPointerException` on a `null` unboxing attempt, but higher-level forms can depend on the internal
	  * structure of the value `T` without checking it for nullity. Instead of calling `set` for a `null`, use
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]] for `None` or explicitly `setNull`.
	  * If a value for a column/parameter cannot be obtained, a `null` value of the appropriate JDBC SQL type should
	  * be set, unless this lack is a result of illegal argument or some other error, in which case an appropriate
	  * exception should be thrown.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull]]
	  */
	def set(position :Int)(statement :PreparedStatement, value :T) :Unit

	/** Set the values of parameters `&lt;position..position+writtenColumns)` of the given `PreparedStatement` to
	  * represent the 'null value' of type `T`, however this form decides to define it. Typically, this means setting
	  * all the columns which would be set with `set` to the appropriate `null` value, but it is not required.
	  */
	def setNull(position :Int)(statement :PreparedStatement) :Unit



//	def literal(value :Option[T]) :String = value match {
//		case Some(x) => literal(x)
//		case _ => nullLiteral
//	}

	/** The string representation of the given value of `T` as an SQL literal, ready to be embedded as a constant part
	  * of an SQL statement. */
	def literal(value :T) :String //todo: define whether it should handle null values and, if so, provide some ease-of-life support.

	/** The string representation of a 'null value' of type `T` (typically an SQL NULL or a tuple of NULL values),
	  * ready to be embedded as a constant part of an SQL statement. */
	def nullLiteral :String


//	def inlineLiteral(value :Option[T]) :String = value match {
//		case Some(x) => inlineLiteral(x)
//		case _ => inlineNullLiteral
//	}

	/** The string representation of of the given value of `T` ready to be embedded as part of a larger SQL tuple literal
	  * or a SELECT clause. For single column forms, this will be the same as `literal(values)`. Multi column forms
	  * omit the surrounding '(' and ')' so that concatenating results of repeated calls to `inlineLiteral` of several
	  * forms yields a flat result.
	  */
	def inlineLiteral(value :T) :String

	/** The string representation of of the 'null value' of `T` ready to be embedded as part of a larger SQL tuple literal
	  * or a SELECT clause. For single column forms, this will be the same as `nullLiteral`. Multi column forms
	  * omit the surrounding '(' and ')' so that concatenating results of repeated calls to `inlineNullLiteral` of
	  * several forms yields a flat result.
	  */
	def inlineNullLiteral :String


//	def literal(value :Option[T], inline :Boolean) :String =
//		if (inline) inlineLiteral(value)
//		else literal(value)

	def literal(value :T, inline :Boolean) :String =
		if (inline) inlineLiteral(value)
		else literal(value)

	def nullLiteral(inline :Boolean) :String =
		if (inline) inlineNullLiteral
		else nullLiteral


	/** Number of parameters set by this form each time its `set` or `setNull` is called. */
	def writtenColumns :Int



	/** Create a write form for `X` which will map received values to `T` and pass them to this form.
	  * The arguments are not tested for `null` values before being passed to `fun`, which should handle `null`s
	  * gracefully if they are considered a valid value for the adapted form's use case.
	  */
	def unmap[X](fun :X => T) :SQLWriteForm[X] = SQLWriteForm.map(fun)(this)

	/** Create a write form for `X` which will try to map received values to `T` and pass them to this form.
	  * If the given function yields `None`, this form's `setNull` method is used instead of `set`.
	  * The arguments are not tested for `null` values before being passed to `fun`, which should handle `null`s
	  * gracefully if they are considered a valid value for the adapted form's use case.
	  */
	def flatUnmap[X](fun :X => Option[T]) :SQLWriteForm[X]  = SQLWriteForm.flatMap(fun)(this)



//	def getOrElse[S](other :)

	/** Lift this form to represent `Option[T]`, where `Some` values are delegated to this instance's `set` method,
	  * while `None` results in calling this form's `setNull` instead.
	  */
	def asOpt :SQLWriteForm[Option[T]] = SQLWriteForm.OptionWriteForm(this)

	/** Combine this form with another form, to create a form for the `(T, O)` pair. The parameters for the second form
	  * are expected to immediately follow this form's statement parameters.
	  */
	def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] = new Tuple2WriteForm()(this, other)



	/** Combine this form with a read form for the same type in order to obtain a read-write form. */
	def &&[O<:T](read :SQLReadForm[O]) :SQLForm[O] = SQLForm.combine(read, this)



	def compatible(other :SQLWriteForm[_]) :Boolean = this == other

	def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def toString :String = this.innerClassName

}






object SQLWriteForm {
	/** Summon an implicitly available `SQLWriteForm[T]`. */
	def apply[T :SQLWriteForm] :SQLWriteForm[T] = implicitly[SQLWriteForm[T]]

	/** Summon an implicitly available `ColumnWriteForm[T]`. */
	def column[T :ColumnWriteForm] :ColumnWriteForm[T] = implicitly[ColumnWriteForm[T]]



	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `SQLWriteForm[T]`.
	  */
	def const[T :SQLWriteForm](value :T) :SQLWriteForm[Any] = 
		if (value == null) new NullWriteForm[T]
		else new ConstWriteForm(value)

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the default `orElse` value will be written instead using the backing forms `set` method.
	  */
	def eval[T :SQLWriteForm](value: =>Option[T], orElse :T) :SQLWriteForm[Any] =
		new EvalWriteForm[T](value)(SQLWriteForm[T], NullValue(orElse))

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the value carried by an implicitly available `NullValue` will be written instead, using the backing
	  * form's `set` method.
	  */
	def eval[T :SQLWriteForm :NullValue](value: =>Option[T]) :SQLWriteForm[Any] =
		new EvalWriteForm[T](value)

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form.
	  */
	def eval[T :SQLWriteForm](value: =>T) :SQLWriteForm[Any] =
		new EvalWriteForm[T](Some(value))(SQLWriteForm[T], NullValue(value))


	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed, but must be thread safe and may be executed more than once
	  * if several threads trigger the initialization at the same time, but the returned form is thread safe.
	  */
	def Lazy[T](delayed: => SQLWriteForm[T]) :SQLWriteForm[T] =
		new LazyWriteForm[T] {
			override protected[this] var init = () => delayed
		}



	def flatMap[S :SQLWriteForm, T](map :T=>Option[S]) :SQLWriteForm[T] =
		implicitly[SQLWriteForm[S]] match {
			case a :ColumnWriteForm[_] =>
				ColumnWriteForm.flatMap(map)(a.asInstanceOf[ColumnWriteForm[S]])
			case f =>
				new FlatMappedSQLWriteForm[S, T] {
					override val source = f
					override val unmap = map
				}
		}

	def map[S :SQLWriteForm, T](map :T => S) :SQLWriteForm[T] = SQLWriteForm[S] match {
		case a :ColumnWriteForm[_] =>
			ColumnWriteForm.map(map)(a.asInstanceOf[ColumnWriteForm[S]])
		case f =>
			new MappedSQLWriteForm[S, T] {
				override val source = f
				override val unmap = map
			}
	}





	/** Create an `SQLWriteForm[T]` which will delegate all `set`/`setNull` calls to ''all'' forms in the given sequence,
	  * in the exact order they appear. The 'position' argument of every form after the last is increased by the sum
	  * of `writtenColumns` of all preceding forms.
	  */
	def chain[T](forms :Seq[SQLWriteForm[T]]) :SQLWriteForm[T] = forms match {
		case Seq() => empty
		case Seq(form) => form
		case _ => new WriteFormChain(forms)
	}

	def seq[T](items :Seq[SQLWriteForm[T]]) :SQLWriteForm[Seq[T]] = new SeqWriteFormImpl[T](items)

	/** An empty form which never writes anything. Its `writtenColumns` property is set to zero. */
	val empty :SQLWriteForm[Any] = new EmptyWriteForm[Any] {}





	implicit def OptionWriteForm[T :SQLWriteForm] :SQLWriteForm[Option[T]] =
		SQLWriteForm[T].flatUnmap(identity[Option[T]])

	implicit def SomeWriteForm[T :SQLWriteForm] :SQLWriteForm[Some[T]] =
		SQLWriteForm[T].unmap(_.get)



	implicit def ChainWriteForm[T <: Chain, H](implicit t :SQLWriteForm[T], h :SQLWriteForm[H]) :SQLWriteForm[T ~ H] =
		new ChainWriteForm(t, h)

	implicit val EmptyChainWriteForm :SQLWriteForm[@~] = empty





	/** A base trait for forms which write nothing. Sets the `writtenColumns` property to zero. */
	trait EmptyWriteForm[-T] extends SQLWriteForm[T] {
		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit = ()
		override def setNull(position :Int)(statement :PreparedStatement) :Unit = ()
		override def inlineLiteral(value: T): String = ""
		override def inlineNullLiteral: String = ""
		override def literal(value: T): String = ""
		override def nullLiteral: String = ""
		final override def writtenColumns: Int = 0
		override def toString = "EMPTY"
	}



	private class NullWriteForm[T](implicit writer :SQLWriteForm[T]) extends ProxyWriteForm[Any] {
		//the cast here is used only for the equals implementation in ProxyWriteForm
		protected override def form :SQLWriteForm[Any] = writer.asInstanceOf[SQLWriteForm[Any]]

		override def writtenColumns :Int = writer.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, value :Any) :Unit =
			writer.setNull(position)(statement)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = 
			writer.setNull(position)(statement)

		override def literal(value :Any) :String = writer.nullLiteral
		override def nullLiteral :String = writer.nullLiteral
		override def inlineLiteral(value :Any) :String = writer.inlineNullLiteral
		override def inlineNullLiteral :String = writer.inlineNullLiteral

	}



	private class ConstWriteForm[T](value :T)(implicit form :SQLWriteForm[T]) extends SQLWriteForm[Any] {
		private def target = form
		private def const = value

		override def writtenColumns: Int = form.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, ignore :Any) :Unit =
			form.set(position)(statement, value)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			form.set(position)(statement, value)

		override def literal(ignored: Any): String = form.literal(value)

		override def nullLiteral: String = form.literal(value)

		override def inlineLiteral(ignored: Any): String =
			form.inlineLiteral(value)

		override def inlineNullLiteral: String =
			form.inlineLiteral(value)



		override def equals(that :Any) :Boolean = that match {
			case const :ConstWriteForm[_] =>
				(const eq this) || (const canEqual this) && const.const == value && const.target == form
			case _ => false
		}

		override def hashCode :Int = value.hashCode * 31 + form.hashCode

		override def toString = s"$form=$value>"
	}



	private class EvalWriteForm[T](value: =>Option[T])(implicit form :SQLWriteForm[T], orElse :NullValue[T])
		extends SQLWriteForm[Any]
	{
		override def writtenColumns: Int = form.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, ignore :Any) :Unit =
			form.setOpt(position)(statement, value)

		@inline final override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			set(position)(statement, null.asInstanceOf[Any]) //safe, because erased and ignored by set

		override def literal(ignored: Any): String = value match {
			case Some(x) => form.literal(x)
			case _ => form.literal(orElse.value) //form.nullLiteral
		}

		@inline final override def nullLiteral: String = literal(null.asInstanceOf[Any])

		override def inlineLiteral(ignored: Any): String = value match {
			case Some(x) => form.inlineLiteral(x)
			case _ => form.inlineNullLiteral
		}

		@inline final override def inlineNullLiteral: String = inlineLiteral(null.asInstanceOf[Any])


		override def toString = s"$form=?>"
	}



	private[schema] trait FlatMappedSQLWriteForm[S, -T] extends SQLWriteForm[T] {
		val source :SQLWriteForm[S]
		val unmap :T => Option[S]

		override def writtenColumns :Int = source.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit =
			source.setOpt(position)(statement, unmap(value))

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = source.setNull(position)(statement)

		override def literal(value: T): String = unmap(value) match {
			case Some(x) => source.literal(x)
			case _ => source.nullLiteral
		}

		override def nullLiteral: String = source.nullLiteral

		override def inlineLiteral(value: T): String = unmap(value) match {
			case Some(x) => source.inlineLiteral(x)
			case _ => source.inlineNullLiteral
		}

		override def inlineNullLiteral: String = source.inlineNullLiteral



		override def unmap[X](fun :X => T) :SQLWriteForm[X] = SQLWriteForm.map(fun)(this)

		override def flatUnmap[X](fun :X => Option[T]) :SQLWriteForm[X] = SQLWriteForm.flatMap(fun)(this)



		override def toString :String = source.toString + "=>"
	}



	private[schema] trait MappedSQLWriteForm[S, -T] extends SQLWriteForm[T] {
		protected val source :SQLWriteForm[S]
		protected val unmap :T => S

		override def writtenColumns :Int = source.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit =
			source.set(position)(statement, unmap(value))

		override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			source.setNull(position)(statement)

		override def literal(value :T) :String = source.literal(unmap(value))
		override def nullLiteral :String = source.nullLiteral
		override def inlineLiteral(value :T) :String = source.inlineLiteral(unmap(value))
		override def inlineNullLiteral :String = source.inlineNullLiteral

		override def toString :String = source.toString + "=>"
	}






	/** Base trait for various forms which delegate the calls to possibly more then one backing form.
	  * It defines the `writtenColumns` as the sum of `writteColumns` of all backing forms and provides a `setNull`
	  * implementation which invokes `setNull` on all backing forms, appropriately increasing the offset of the first
	  * written parameter.
	  */
	trait CompositeWriteForm[-T] extends SQLWriteForm[T] {
		protected def forms :Seq[SQLWriteForm[_]]

		def writtenColumns :Int = (0 /: forms)(_ + _.writtenColumns)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			var i = position
			forms foreach { form => form.setNull(i)(statement); i += form.writtenColumns }
		}

		override def equals(that :Any) :Boolean = that match {
			case composite :CompositeWriteForm[_] =>
				(composite eq this) || (composite canEqual this) && composite.forms == forms
			case _ => false
		}

		override def hashCode :Int = forms.hashCode
	}






	trait ProxyWriteForm[-T] extends SQLWriteForm[T] {
		override def writtenColumns: Int = form.writtenColumns

		protected def form :SQLWriteForm[T]

		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit =
			form.set(position)(statement, value)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			form.setNull(position)(statement)

		override def literal(value: T): String = form.literal(value)
		override def inlineLiteral(value: T): String = form.inlineLiteral(value)
		override def nullLiteral: String = form.nullLiteral


		override def inlineNullLiteral: String = form.inlineNullLiteral

		override def equals(that :Any) :Boolean = that match {
			case proxy :ProxyWriteForm[_] =>
				(proxy eq this) || (proxy canEqual this) && proxy.form == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "~"+form
	}



	private[schema] trait LazyWriteForm[-T] extends ProxyWriteForm[T] {
		protected[this] var init: () => SQLWriteForm[T]
		@volatile
		protected[this] var initialized :SQLWriteForm[T] = _
		protected[this] var fastAccess :SQLWriteForm[T] = _

		def isInitialized :Boolean = fastAccess !=  null || initialized != null

		protected override def form :SQLWriteForm[T] = {
			if (fastAccess == null) {
				val f = initialized
				val cons = init
				if (f != null)
					fastAccess = f
				else if (cons == null)
					fastAccess = initialized
				else {
					fastAccess = cons()
					initialized = fastAccess
					init = null
				}
			}
			fastAccess
		}

		override def unmap[X](fun :X => T) :SQLWriteForm[X] =
			if (fastAccess == null && initialized == null) Lazy(form.unmap(fun))
			else form.unmap(fun)

		override def flatUnmap[X](fun :X => Option[T]) :SQLWriteForm[X] =
			if (fastAccess == null && initialized == null) Lazy(form.flatUnmap(fun))
			else form.flatUnmap(fun)

		override def asOpt :SQLWriteForm[Option[T]] =
			if (fastAccess == null && initialized == null) Lazy(form.asOpt)
			else form.asOpt

		override def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] =
			if (fastAccess == null && initialized == null) Lazy(form * other)
			else form * other

		override def &&[O <: T](read :SQLReadForm[O]) :SQLForm[O] =
			if (fastAccess == null && initialized == null) SQLForm.Lazy(form && read)
			else form && read



		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[ProxyWriteForm[_]] && isInitialized

		override def toString :String =
			if (fastAccess == null && initialized == null) "Lazy>"
			else form.toString

	}





	private class WriteFormChain[-T](val forms :Seq[SQLWriteForm[T]]) extends SQLWriteForm[T] with CompositeWriteForm[T] {
		override val writtenColumns :Int = super.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit = {
			var i = position
			forms foreach { form => form.set(i)(statement, value); i += form.writtenColumns }
		}

		override def literal(value: T): String =
			forms.map(_.literal(value)).mkString("(", ", ", ")")

		override def inlineLiteral(value: T): String =
			forms.map(_.inlineLiteral(value)).mkString("", ", " ,"")

		override def nullLiteral: String = forms.map(_.nullLiteral).mkString("(", ", ", ")")

		override def inlineNullLiteral: String =
			forms.map(_.inlineNullLiteral).mkString("", ", ", "")

		override def toString :String = forms.mkString("(", "&", ")>")
	}






	private[schema] trait SeqWriteForm[-T] extends SQLWriteForm[Seq[T]] with CompositeWriteForm[Seq[T]] {
		protected def forms :Seq[SQLWriteForm[T]]

		override def set(position :Int)(statement :PreparedStatement, value :Seq[T]) :Unit =
			if (value == null)
				setNull(position)(statement)
			else {
				val iter = value.iterator
				var i = position
				forms foreach { form => form.set(i)(statement, iter.next()); i += form.writtenColumns }
			}

		override def literal(value: Seq[T]): String =
			if (value.size!=forms.size)
				throw new IllegalArgumentException(s"can't set parameters $value: expected ${forms.size} values ($forms)")
			else
				(forms zip value).map {
					case (item, v) => item.asInstanceOf[SQLWriteForm[Any]].literal(v)
				}.mkString("(", ",", ")")

		override def inlineLiteral(value: Seq[T]): String =
			if (value.size!=forms.size)
				throw new IllegalArgumentException(s"can't set parameters $value: expected ${forms.size} values ($forms)")
			else
				(forms zip value).map {
					case (item, v) => item.asInstanceOf[SQLWriteForm[Any]].inlineLiteral(v)
				}.mkString("", ", ", "")

		override def nullLiteral: String =
			forms.map(_ => "null").mkString("(", ", ", ")")

		override def inlineNullLiteral: String =
			forms.map(_ => "null").mkString("", ", ", "")



		override def toString :String = forms.mkString("Seq(",",",")>")
	}

	private case class SeqWriteFormImpl[-T](forms :Seq[SQLWriteForm[T]]) extends SeqWriteForm[T] {
		override val writtenColumns :Int = super.writtenColumns
		override def toString :String = super.toString
	}





	private[schema] trait AbstractTuple2WriteForm[-L, -R] extends SQLWriteForm[(L, R)] {
		override def writtenColumns: Int = _1.writtenColumns + _2.writtenColumns

		val _1 :SQLWriteForm[L]
		val _2 :SQLWriteForm[R]

		override def set(position :Int)(statement :PreparedStatement, value :(L, R)) :Unit =
			if (value == null) {
				_1.setNull(position)(statement)
				_2.setNull(position + _1.writtenColumns)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + _1.writtenColumns)(statement, value._2)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + _1.writtenColumns)(statement)
		}


		override def literal(value: (L, R)): String =
			if (value == null) s"(${_1.inlineNullLiteral}, ${_2.inlineNullLiteral})"
			else s"(${_1.inlineLiteral(value._1)}, ${_2.inlineLiteral(value._2)})"

		override def nullLiteral: String = s"(${_1.inlineNullLiteral}, ${_2.inlineNullLiteral})"

		override def inlineLiteral(value: (L, R)): String = _1.inlineLiteral(value._1) + ", " + _2.inlineLiteral(value._2)

		override def inlineNullLiteral: String = _1.inlineNullLiteral + ", " + _2.inlineNullLiteral


		override def equals(that :Any) :Boolean = that match {
			case tuple :AbstractTuple2WriteForm[_, _] =>
				(tuple eq this) || (tuple canEqual this) && tuple._1 == _1 && tuple._2 == _2
			case _ => false
		}

		override def hashCode :Int = _1.hashCode * 31 + _2.hashCode

		override def toString = s"v(${_1},${_2})"
	}



	private[schema] class Tuple2WriteForm[-L, -R](implicit val _1 :SQLWriteForm[L], val _2 :SQLWriteForm[R]) extends AbstractTuple2WriteForm[L, R]



	private[schema] case class ChainWriteForm[-T <: Chain, -H](tail :SQLWriteForm[T], head :SQLWriteForm[H])
		extends SQLWriteForm[T ~ H]
	{
		override val writtenColumns :Int = tail.writtenColumns + head.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, value :T ~ H) :Unit =
			if (value == null)
				setNull(position)(statement)
			else {
				tail.set(position)(statement, value.tail)
				head.set(position + tail.writtenColumns)(statement, value.head)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			tail.setNull(position)(statement)
			head.setNull(position + tail.writtenColumns)(statement)
		}


		override def literal(value :T ~ H, inline :Boolean) :String = {
			def rec(chain :Chain, form :SQLWriteForm[_], res :StringBuilder = new StringBuilder) :StringBuilder =
				(chain, form) match {
					case (t ~ h, f :ChainWriteForm[_, _]) =>
						rec(t, f.tail, res) ++= ", "
						if (f.head.isInstanceOf[ChainWriteForm[_, _]])
							res ++= "(" ++= f.head.asInstanceOf[SQLWriteForm[Any]].literal(h) ++= ")"
						else
							res ++= f.head.asInstanceOf[SQLWriteForm[Any]].literal(h)
					case (null, f :ChainWriteForm[_, _]) =>
						rec(null, f.tail, res) ++= ", "
					case _ =>
						res
				}
			if (inline)
				rec(value, this).toString
			else
	            (rec(value, this, new StringBuilder("(")) ++= ")").toString
		}

		override def nullLiteral(inline :Boolean) :String = literal(null, inline)
		override def literal(value :T ~ H) :String = literal(value, false)
		override def inlineLiteral(value :T ~ H) :String = literal(value, true)
		override def nullLiteral :String = literal(null, false)
		override def inlineNullLiteral :String = literal(null, true)


		override def equals(that :Any) :Boolean = that match {
			case chain :ChainWriteForm[_, _] =>
				(chain eq this) || (chain canEqual this) && chain.head == head && chain.tail == tail
			case _ => false
		}

		override def hashCode :Int = head.hashCode * 31 + tail.hashCode

		override def toString :String =  {
			def rec(form :SQLWriteForm[_], res :StringBuilder = new StringBuilder) :StringBuilder = form match {
				case chain :ChainWriteForm[_, _] =>
					rec(chain.tail, res) ++= "~"
					chain.head match {
						case _ :ChainWriteForm[_, _] => rec(chain.head, res ++= "(") ++= ")"
						case _ => res append chain.head
					}
				case  _ => res ++= "@~"
			}
			rec(this).toString
		}
	}



}




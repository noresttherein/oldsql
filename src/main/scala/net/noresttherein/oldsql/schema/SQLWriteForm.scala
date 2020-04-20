package net.noresttherein.oldsql.schema

import java.sql.PreparedStatement

import net.noresttherein.oldsql.collection.{Chain, ChainMap, LiteralIndex, Record}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.ChainMap.&~
import net.noresttherein.oldsql.collection.LiteralIndex.{:~, |~}
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.Tuple2WriteForm
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


	/** Creates a write form for `X` which will use this form after extracting a value from `X` with the given
	  * extractor. This is equivalent to `unmap` or `flatUnmap`, depending on whether the extractor is
	  * a `RequisiteExtractor` instance.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.unmap]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.flatUnmap]]
	  */
	def compose[X](extractor :X =?> T) :SQLWriteForm[X] = extractor match {
		case _ :IdentityExtractor[_] => this.asInstanceOf[SQLWriteForm[X]]
		case const :ConstantExtractor[_, _] => SQLWriteForm.const(const.constant.asInstanceOf[T])(this)
		case req :RequisiteExtractor[_, _] => unmap(req.getter.asInstanceOf[X => T])
		case _ :EmptyExtractor[_, _] => SQLWriteForm.none(this)
		case _ => flatUnmap(extractor.optional)
	}



	/** Lift this form to represent `Option[T]`, where `Some` values are delegated to this instance's `set` method,
	  * while `None` results in calling this form's `setNull` instead.
	  */
	def asOpt :SQLWriteForm[Option[T]] = SQLWriteForm.OptionWriteForm(this)

	/** Combine this form with another form, to create a form for the `(T, O)` pair. The parameters for the second form
	  * are expected to immediately follow this form's statement parameters.
	  */
	def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] = Tuple2WriteForm(this, other)



	/** Combine this form with a read form for the same type in order to obtain a read-write form. */
	def &&[O<:T](read :SQLReadForm[O]) :SQLForm[O] = SQLForm.combine(read, this)



	def compatible(other :SQLWriteForm[_]) :Boolean = this == other

	def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def toString :String = this.innerClassName

}






object SQLWriteForm extends ScalaWriteForms {

	/** Summon an implicitly available `SQLWriteForm[T]`. */
	def apply[T :SQLWriteForm] :SQLWriteForm[T] = implicitly[SQLWriteForm[T]]



	/** An `SQLWriteForm` which will always write the 'null' value of type `T` using the implicitly available form. */
	def none[T :SQLWriteForm] :SQLWriteForm[Any] = new NullWriteForm[T]()



	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `SQLWriteForm[T]`.
	  */
	def opt[T :SQLWriteForm](value :Option[T]) :SQLWriteForm[Any] =
		if (value.isEmpty) new NullWriteForm[T]
		else new ConstWriteForm(value.get)



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
	def evalopt[T :SQLWriteForm](value: =>Option[T], orElse :T) :SQLWriteForm[Any] =
		new EvalOrNullWriteForm[T](value)(SQLWriteForm[T], NullValue(orElse))

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the 'null' variant of the literal/write method will be called on the backing form.
	  */
	def evalopt[T :SQLWriteForm](value: =>Option[T]) :SQLWriteForm[Any] =
		new EvalWriteForm[T](value)

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form.
	  */
	def eval[T :SQLWriteForm](value: =>T) :SQLWriteForm[Any] =
		new EvalWriteForm[T](Some(value))



	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed, but must be thread safe and may be executed more than once
	  * if several threads trigger the initialization at the same time, but the returned form is thread safe.
	  */
	def Lazy[T](delayed: => SQLWriteForm[T]) :SQLWriteForm[T] =
		new LazyWriteForm[T] {
			override protected[this] var init = () => delayed
		}



	/** Calls [[net.noresttherein.oldsql.schema.SQLWriteForm#flatUnmap flatUnmap]] on the implicit form for `S`. */
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

	/** Calls [[net.noresttherein.oldsql.schema.SQLWriteForm#unmap unmap]] on the implicit form for `S`. */
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
	def combine[T](forms :SQLWriteForm[T]*) :SQLWriteForm[T] = forms match {
		case Seq() => empty
		case Seq(form) => form
		case _ => new CombinedWriteForm(forms)
	}

	def seq[T](items :Seq[SQLWriteForm[T]]) :SQLWriteForm[Seq[T]] = new SeqWriteFormImpl[T](items)

	/** An empty form which never writes anything. Its `writtenColumns` property is set to zero. */
	val empty :SQLWriteForm[Any] = new EmptyWriteForm[Any] {}






	implicit val EmptyChainWriteForm :SQLWriteForm[@~] = empty

	implicit def ChainWriteForm[I <: Chain, L](implicit t :SQLWriteForm[I], h :SQLWriteForm[L]) :SQLWriteForm[I ~ L] =
		new ChainWriteForm(t, h)

	implicit def LiteralIndexWriteForm[I <: LiteralIndex :SQLWriteForm, K <: Singleton, V :SQLWriteForm] :SQLWriteForm[I |~ (K :~ V)] =
		new LiteralIndexWriteForm(SQLWriteForm[I], SQLWriteForm[V])

	implicit def ChainMapWriteForm[I <: ChainMap :SQLWriteForm, K <: Singleton, V :SQLWriteForm] :SQLWriteForm[I &~ (K, V)] =
		new ChainMapWriteForm(SQLWriteForm[I], SQLWriteForm[V])

	implicit def RecordWriteForm[I <: Record :SQLWriteForm, K <: String with Singleton, V :SQLWriteForm] :SQLWriteForm[I |# (K, V)] =
		new RecordWriteForm(SQLWriteForm[I], SQLWriteForm[V])





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



	private[schema] class NullWriteForm[T](implicit writer :SQLWriteForm[T]) extends ProxyWriteForm[Any] {
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

		override def toString = s"NULL:$writer"
	}



	private[schema] class ConstWriteForm[T](value :T)(implicit form :SQLWriteForm[T]) extends SQLWriteForm[Any] {
		protected def target :SQLWriteForm[T] = form
		protected def const :T = value

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



	private[schema] class EvalWriteForm[T](value: =>Option[T])(implicit form :SQLWriteForm[T])
		extends SQLWriteForm[Any]
	{
		override def writtenColumns: Int = form.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, ignore :Any) :Unit =
			form.setOpt(position)(statement, value)

		@inline override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			set(position)(statement, null.asInstanceOf[Any]) //safe, because erased and ignored by set

		override def literal(ignored: Any): String = value match {
			case Some(x) => form.literal(x)
			case _ => form.nullLiteral
		}

		@inline override def nullLiteral: String = literal(null.asInstanceOf[Any])

		override def inlineLiteral(ignored: Any): String = value match {
			case Some(x) => form.inlineLiteral(x)
			case _ => form.inlineNullLiteral
		}

		@inline override def inlineNullLiteral: String = inlineLiteral(null.asInstanceOf[Any])


		override def toString = s"$form=?>"
	}



	private[schema] class EvalOrNullWriteForm[T](value: Option[T])(implicit form :SQLWriteForm[T], orElse :NullValue[T])
		extends SQLWriteForm[Any]
	{
		override def writtenColumns: Int = form.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, ignore :Any) :Unit = value match {
			case Some(x) => form.set(position)(statement, x)
			case _ => form.set(position)(statement, orElse.value)
		}


		@inline override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			set(position)(statement, null.asInstanceOf[Any]) //safe, because erased and ignored by set

		override def literal(ignored: Any): String = value match {
			case Some(x) => form.literal(x)
			case _ => form.literal(orElse.value)
		}

		@inline override def nullLiteral: String = literal(null.asInstanceOf[Any])

		override def inlineLiteral(ignored: Any): String = value match {
			case Some(x) => form.inlineLiteral(x)
			case _ => form.inlineLiteral(orElse.value)
		}

		@inline override def inlineNullLiteral: String = inlineLiteral(null.asInstanceOf[Any])


		override def toString = s"$form=?>"
	}



	trait FlatMappedSQLWriteForm[S, -T] extends SQLWriteForm[T] {
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



	trait MappedSQLWriteForm[S, -T] extends SQLWriteForm[T] {
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



	private class CombinedWriteForm[-T](val forms :Seq[SQLWriteForm[T]]) extends SQLWriteForm[T] with CompositeWriteForm[T] {
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






	private[schema] trait GenericChainWriteForm[C[+A <: I, +B <: L] <: A ~ B, -I <: Chain, -L] extends SQLWriteForm[C[I, L]] {
		protected val init :SQLWriteForm[I]
		protected val last :SQLWriteForm[L]

		override val writtenColumns :Int = init.writtenColumns + last.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, value :C[I, L]) :Unit =
			if (value == null)
				setNull(position)(statement)
			else {
				init.set(position)(statement, value.init)
				last.set(position + init.writtenColumns)(statement, value.last)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			init.setNull(position)(statement)
			last.setNull(position + init.writtenColumns)(statement)
		}



		override def literal(value :I C L, inline :Boolean) :String = {
			def rec(chain :Chain, form :SQLWriteForm[_], res :StringBuilder = new StringBuilder) :StringBuilder =
				(chain, form) match {
					case (t ~ h, f :GenericChainWriteForm[_, _, _]) =>
						rec(t, f.init, res) ++= ", "
						if (f.last.isInstanceOf[ChainWriteForm[_, _]])
							res ++= "(" ++= f.last.asInstanceOf[SQLWriteForm[Any]].literal(h) ++= ")"
						else
							res ++= f.last.asInstanceOf[SQLWriteForm[Any]].literal(h)
					case (null, f :GenericChainWriteForm[_, _, _]) =>
						rec(null, f.init, res) ++= ", "
					case _ =>
						res
				}
			if (inline)
				rec(value, this).toString
			else
				(rec(value, this, new StringBuilder("(")) ++= ")").toString
		}

		override def nullLiteral(inline :Boolean) :String = literal(null.asInstanceOf[I C L], inline)
		override def literal(value :I C L) :String = literal(value, false)
		override def inlineLiteral(value :I C L) :String = literal(value, true)
		override def nullLiteral :String = literal(null.asInstanceOf[I C L], false)
		override def inlineNullLiteral :String = literal(null.asInstanceOf[I C L], true)


		override def equals(that :Any) :Boolean = that match {
			case chain :GenericChainWriteForm[_, _, _] =>
				(chain eq this) || (chain canEqual this) && chain.last == last && chain.init == init
			case _ => false
		}

		override def hashCode :Int = last.hashCode * 31 + init.hashCode

		override def toString :String =  {
			def rec(form :SQLWriteForm[_], res :StringBuilder = new StringBuilder) :StringBuilder = form match {
				case chain :GenericChainWriteForm[_, _, _] =>
					rec(chain.init, res) ++= symbol
					chain.last match {
						case _ :ChainWriteForm[_, _] => rec(chain.last, res ++= "(") ++= ")"
						case _ => res append chain.last
					}
				case  _ => res ++= "@~"
			}
			rec(this).toString
		}

		protected def symbol :String

	}



	private[schema] class ChainWriteForm[-I <: Chain, -L]
	                                    (protected val init :SQLWriteForm[I], protected val last :SQLWriteForm[L])
		extends GenericChainWriteForm[~, I, L]
	{
		override protected def symbol :String = "~"
	}



	private[schema] class LiteralIndexWriteForm[-I <: LiteralIndex, -K <: Singleton, -V]
	                                           (protected val init :SQLWriteForm[I], protected val value :SQLWriteForm[V])
		extends GenericChainWriteForm[|~, I, K :~ V]
	{
		override protected val last :SQLWriteForm[K :~ V] = value.unmap(_.value)

		override def equals(that :Any) :Boolean = that match {
			case other :LiteralIndexWriteForm[_, _, _] =>
				(other eq this) || (other canEqual this) && init == other.init && value == other.value
			case _ => false
		}

		override protected def symbol :String = "|~"
	}



	private[schema] class ChainMapWriteForm[-I <: ChainMap, -K <: Singleton, -V]
	                                       (protected val init :SQLWriteForm[I], protected val value :SQLWriteForm[V])
		extends GenericChainWriteForm[&~, I, (K, V)]
	{
		override protected val last :SQLWriteForm[(K, V)] = value.unmap(_._2)

		override def equals(that :Any) :Boolean = that match {
			case other :ChainMapWriteForm[_, _, _] =>
				(other eq this) || (other canEqual this) && init == other.init && value == other.value
			case _ => false
		}

		override protected def symbol :String = "&~"
	}



	private[schema] class RecordWriteForm[-I <: Record, -K <: String with Singleton, -V]
	                                     (protected val init :SQLWriteForm[I], protected val value :SQLWriteForm[V])
		extends GenericChainWriteForm[|#, I, (K, V)]
	{
		override protected val last :SQLWriteForm[(K, V)] = value.unmap(_._2)

		override def equals(that :Any) :Boolean = that match {
			case other :RecordWriteForm[_, _, _] =>
				(other eq this) || (other canEqual this) && init == other.init && value == other.value
			case _ => false
		}

		override protected def symbol :String = "|#"
	}


}




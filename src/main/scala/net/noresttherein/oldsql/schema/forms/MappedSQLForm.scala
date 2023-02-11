package net.noresttherein.oldsql.schema.forms

import java.sql.{PreparedStatement, ResultSet}

import scala.collection.immutable.Seq

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.SQLForm.{NotNullForm, NullValue}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull
import net.noresttherein.oldsql.schema.SQLReadForm.{AbstractSQLReadForm, DerivedReadForm, ReadFormAdapter, ReadFormNullGuard}
import net.noresttherein.oldsql.schema.SQLWriteForm.{AbstractSQLWriteForm, DerivedWriteForm, NotNullWriteForm, NullSafeWriteForm, WriteFormAdapter, WriteFormLiterals}
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.UnspecifiedColumnFormAdapter






private[oldsql] class MappedSQLReadForm[S, +T](protected final val map :S => T,
                                               protected override val text :Opt[String] = Lack,
                                               protected val suffix :String = "")
                                              (implicit underlying :SQLReadForm[S],
                                               implicit override val nulls :NullValue[T])
	extends AbstractSQLReadForm[T](underlying.columnCount, text) with ReadFormAdapter[T]
{	//map/flatMap not overriden to preserve potentially custom name.
	protected override def form :SQLReadForm[S] = underlying

	override def opt(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position).map(map)

	override def notNull :SQLReadForm[T] =
		new MappedSQLReadForm[S, T](map, text, suffix + ".notNull")(form, NullValue.NotNull)
			with ReadFormNullGuard[T] with ComparableReadForm

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		//if map is a pure function, then the compiler will reuse the same instance even for different instances of this class
		case other :MappedSQLReadForm[_, _] if other canEqual this =>
			form == other.form && map == other.map && nulls == other.nulls
		case _ => false
	}
	override def hashCode :Int = form.hashCode * 31 + nulls.hashCode

	private[schema] override lazy val cachedString =
		if (text.isDefined) text.get + suffix + ">" else "[" + form + "]" + suffix + "=>"
}

private[oldsql] class MappedColumnReadForm[S, +T](f :S => T, protected override val text :Opt[String] = Lack,
                                                  protected override val suffix :String = "")
                                                 (implicit protected override val form :ColumnReadForm[S],
                                                  override val nulls :NullValue[T])
	extends MappedSQLReadForm[S, T](f, text, suffix) with ColumnReadForm[T] with UnspecifiedColumnFormAdapter
{
	override def notNull :ColumnReadForm[T] =
		new MappedColumnReadForm[S, T](map, text, suffix + ".notNull")(form, NotNull)
			with ReadFormNullGuard[T] with ComparableReadForm
}


private[schema] class DerivedMappedSQLReadForm[S :SQLReadForm, +T :NullValue]
                                              (constructorName :String, f :S => T)
	extends MappedSQLReadForm[S, T](f, Got(constructorName)) with DerivedReadForm[T]
{
	override def notNull :SQLReadForm[T] =
		new DerivedMappedSQLReadForm(name.get + ".notNull", map)(form, NullValue.NotNull)
			with ReadFormNullGuard[T] with ComparableReadForm

	private[schema] override lazy val cachedString = name.get + "[" + form + "]>"
}

private[schema] class DerivedMappedColumnReadForm[S, +T](constructorName :String, f :S => T)
                                                        (implicit protected override val form :ColumnReadForm[S],
                                                         override val nulls :NullValue[T])
	extends DerivedMappedSQLReadForm[S, T](constructorName, f) with ColumnReadForm[T] with UnspecifiedColumnFormAdapter
{
	override def notNull :ColumnReadForm[T] =
		new DerivedMappedColumnReadForm[S, T](name.get + ".notNull", map)(form, NotNull)
			with ReadFormNullGuard[T] with ComparableReadForm
}




private[schema] class OptMappedSQLReadForm[S, +T](protected final val map :S => Option[T],
                                                  protected override val text :Opt[String] = Lack,
                                                  protected val suffix :String = "")
                                                 (implicit underlying :SQLReadForm[S],
                                                  implicit override val nulls :NullValue[T])
	extends AbstractSQLReadForm[T](underlying.columnCount, text) with ReadFormAdapter[T]
{	//map/flatMap not overriden to preserve potentially custom name.
	protected override def form :SQLReadForm[S] = underlying

	override def opt(res :ResultSet, position :Int) :Opt[T] = underlying.opt(res, position) match {
		case Got(s) => map(s)
		case _ => Lack
	}

	override def notNull :SQLReadForm[T] =
		new OptMappedSQLReadForm(map, text, suffix + ".notNull")(underlying, NullValue.NotNull)
			with ReadFormNullGuard[T] with ComparableReadForm

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		//if map is a pure function, then the compiler will reuse the same instance even for different instances of this class
		case other :OptMappedSQLReadForm[_, _] if other canEqual this =>
			form == other.form && map == other.map && nulls == other.nulls
		case _ => false
	}
	override def hashCode :Int = form.hashCode * 31 + underlying.hashCode

	private[schema] override lazy val cachedString :String =
		if (text.isDefined) text.get + suffix + ">" else "[" + form + "]" + suffix + "=>"
}

private[schema] class OptMappedColumnReadForm[S, +T](f :S => Option[T], protected override val text :Opt[String] = Lack,
                                                     protected override val suffix :String = "")
                                                    (implicit override val form :ColumnReadForm[S],
                                                     override val nulls :NullValue[T])
	extends OptMappedSQLReadForm[S, T](f, text, suffix) with ColumnReadForm[T] with UnspecifiedColumnFormAdapter
{
	override def notNull :ColumnReadForm[T] =
		new OptMappedColumnReadForm[S, T](map, text, ".notNull")(form, NotNull)
			with ReadFormNullGuard[T] with ComparableReadForm
}


private[schema] class DerivedOptMappedSQLReadForm[S :SQLReadForm, +T :NullValue]
                                                 (constructorName :String, f :S => Option[T])
	extends OptMappedSQLReadForm[S, T](f, Got(constructorName)) with DerivedReadForm[T]
{
	override def notNull :SQLReadForm[T] =
		new DerivedOptMappedSQLReadForm(name.get + ".notNull", map)(form, NullValue.NotNull)
			with ReadFormNullGuard[T] with ComparableReadForm

	private[schema] override lazy val cachedString :String = name.get + "[" + form + "]>"
}

private[schema] class DerivedOptMappedColumnReadForm[S, +T](constructorName :String, f :S => Option[T])
                                                           (implicit override val form :ColumnReadForm[S],
                                                            override val nulls :NullValue[T])
	extends DerivedOptMappedSQLReadForm[S, T](constructorName, f)
		with ColumnReadForm[T] with UnspecifiedColumnFormAdapter
{
	override def notNull :ColumnReadForm[T] =
		new DerivedOptMappedColumnReadForm[S, T](name.get + ".notNull", map)(form, NotNull)
			with ReadFormNullGuard[T] with ComparableReadForm
}






private[schema] trait MappedWriteForm[S, -T] extends WriteFormAdapter[T] {
	protected override val form :SQLWriteForm[S]
	protected val contramap :T => S

	override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
		form.set(statement, position, contramap(value))

	override def literal(value :T, inline :Boolean) :String = form.literal(contramap(value), inline)
	override def columnLiterals(value :T) :Seq[String] = form.columnLiterals(contramap(value))

	override def split :Seq[ColumnWriteForm[T]] = form.split.map(_.compose(contramap))
	//map/flatMap not overriden to preserve a potentially overriden toString.

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :MappedWriteForm[_, _] if other canEqual this => contramap == other.contramap && form == other.form
		case _ => false
	}
	override def hashCode :Int = form.hashCode * 31 + contramap.hashCode
}


private[schema] class MappedSQLWriteForm[S, -T](protected override val contramap :T => S,
                                                protected override val text :Opt[String] = Lack,
                                                protected val suffix :String = "")
                                               (implicit protected override val form :SQLWriteForm[S])
	extends AbstractSQLWriteForm[T](form.columnCount, text) with MappedWriteForm[S, T]
{ outer =>
	override def nullSafe :SQLWriteForm[T] =
		new MappedSQLWriteForm(contramap, text, suffix + ".nullSafe")
			with NullSafeWriteForm[T] with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}

	override def notNull :SQLWriteForm[T] =
		new MappedSQLWriteForm(contramap, text, suffix + ".notNull") with NotNullWriteForm[T] with ComparableWriteForm {
			override def nullSafe = outer.safeNotNull
		}

	private def safeNotNull :SQLWriteForm[T] =
		new MappedSQLWriteForm(contramap, text, suffix + ".notNull.nullSafe")
			with NotNullWriteForm[T] with NullSafeWriteForm[T] with ComparableWriteForm

	private[schema] override lazy val cachedString =
		if (text.isEmpty) "<=[" + form + "]" + suffix else "<" + text.get + suffix
}


private[schema] class MappedColumnWriteForm[S, -T](f :T => S, protected override val text :Opt[String] = Lack,
                                                   protected override val suffix :String = "")
                                                  (implicit override val form :ColumnWriteForm[S])
	extends MappedSQLWriteForm[S, T](f, text, suffix) with SingletonColumnWriteForm[T] with UnspecifiedColumnFormAdapter
{ outer =>
	override def nullSafe :ColumnWriteForm[T] =
		new MappedColumnWriteForm(contramap, text, suffix + ".nullSafe")
			with NullSafeWriteForm[T] with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :ColumnWriteForm[T] =
		new MappedColumnWriteForm(contramap, text, suffix + ".notNull")
			with NotNullWriteForm[T] with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :ColumnWriteForm[T] =
		new MappedColumnWriteForm(contramap, text, ".notNull.nullSafe")
			with NotNullWriteForm[T] with NullSafeWriteForm[T] with ComparableWriteForm
}


private[schema] class DerivedMappedSQLWriteForm[S :SQLWriteForm, -T](constructorName :String, f :T => S)
	extends MappedSQLWriteForm[S, T](f, Got(constructorName)) with DerivedWriteForm[T]
{ outer =>
	override def nullSafe :SQLWriteForm[T] =
		new DerivedMappedSQLWriteForm(name.get + ".nullSafe", contramap)
			with NullSafeWriteForm[T] with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :SQLWriteForm[T] =
		new DerivedMappedSQLWriteForm(name.get + ".notNull", contramap)
			with NotNullWriteForm[T] with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :SQLWriteForm[T] =
		new DerivedMappedSQLWriteForm[S, T](name.get + ".notNull.nullSafe", contramap)
			with NotNullWriteForm[T] with NullSafeWriteForm[T] with ComparableWriteForm

	private[schema] override lazy val cachedString = "<" + name.get + "[" + form + "]"
}


private[schema] class DerivedMappedColumnWriteForm[S, -T](constructorName :String, f :T => S)
                                                         (implicit override val form :ColumnWriteForm[S])
	extends DerivedMappedSQLWriteForm[S, T](constructorName, f)
	   with SingletonColumnWriteForm[T] with UnspecifiedColumnFormAdapter
{ outer =>
	override def nullSafe :ColumnWriteForm[T] =
		new DerivedMappedColumnWriteForm[S, T](name.get + ".nullSafe", contramap)
			with NullSafeWriteForm[T] with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :ColumnWriteForm[T] =
		new DerivedMappedColumnWriteForm[S, T](name.get + ".notNull", contramap)
			with NotNullWriteForm[T] with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :ColumnWriteForm[T] =
		new DerivedMappedColumnWriteForm[S, T](name.get + ".notNull.nullSafe", contramap)
			with NotNullWriteForm[T] with NullSafeWriteForm[T] with ComparableWriteForm
}




private[schema] trait OptMappedWriteForm[S, -T] extends WriteFormAdapter[T] {
	protected override val form :SQLWriteForm[S]
	protected val contramap :T => Option[S]

	override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
		form.setOpt(statement, position, contramap(value))

	override def literal(value: T, inline :Boolean): String = contramap(value) match {
		case Some(x) => form.literal(x, inline)
		case _ => form.nullLiteral(inline)
	}
	override def columnLiterals(value :T) :Seq[String] = contramap(value) match {
		case Some(x) => form.columnLiterals(x)
		case _ => form.nullColumnLiterals
	}
	override def split :Seq[ColumnWriteForm[T]] = form.split.map(_.optUnmap(contramap))
	//map/flatMap not overriden to preserve a potentially overriden toString.

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :OptMappedWriteForm[_, _] if other canEqual this =>
			contramap == other.contramap && form == other.form
		case _ => false
	}
	override def hashCode :Int = form.hashCode * 31 + contramap.hashCode
}


private[schema] class OptMappedSQLWriteForm[S, -T](protected override val contramap :T => Option[S],
                                                   protected override val text :Opt[String] = Lack,
                                                   protected val suffix :String = "")
                                                  (implicit protected override val form :SQLWriteForm[S])
	extends AbstractSQLWriteForm[T](form.columnCount, text) with OptMappedWriteForm[S, T]
{ outer =>
	override def nullSafe :SQLWriteForm[T] =
		new OptMappedSQLWriteForm[S, T](contramap, text, suffix + ".nullSafe")
			with NullSafeWriteForm[T] with ComparableWriteForm
	{
		override def notNull = outer.safeNotNull
	}
	override def notNull :SQLWriteForm[T] =
		new OptMappedSQLWriteForm[S, T](contramap, text, suffix + ".notNull")
			with NotNullWriteForm[T] with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :SQLWriteForm[T] =
		new OptMappedSQLWriteForm[S, T](contramap, text, suffix + ".notNull.nullSafe")
			with NotNullWriteForm[T] with NullSafeWriteForm[T] with ComparableWriteForm

	private[schema] override lazy val cachedString =
		if (text.isEmpty) "<=[" + form + "]" + suffix else "<" + text.get + suffix
}


private[schema] class OptMappedColumnWriteForm[S, -T](protected override val contramap :T => Option[S],
                                                      protected override val text :Opt[String] = Lack,
                                                      protected override val suffix :String = "")
                                                     (implicit protected override val form :ColumnWriteForm[S])
	extends OptMappedSQLWriteForm[S, T](contramap, text, suffix)
	   with SingletonColumnWriteForm[T] with UnspecifiedColumnFormAdapter
{ outer =>
	override def nullSafe :ColumnWriteForm[T] =
		new OptMappedColumnWriteForm[S, T](contramap, text, suffix + ".nullSafe")
			with NullSafeWriteForm[T] with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :ColumnWriteForm[T] =
		new OptMappedColumnWriteForm[S, T](contramap, text, suffix + ".notNull")
			with NotNullWriteForm[T] with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :ColumnWriteForm[T] =
		new OptMappedColumnWriteForm[S, T](contramap, text, suffix + ".notNull.nullSafe")
			with NotNullWriteForm[T] with NullSafeWriteForm[T] with ComparableWriteForm
}


private[schema] class DerivedOptMappedSQLWriteForm[S :SQLWriteForm, -T]
                                                  (constructorName :String, f :T => Option[S])
	extends OptMappedSQLWriteForm[S, T](f, Got(constructorName)) with DerivedWriteForm[T]
{
	override def notNull :SQLWriteForm[T] =
		new DerivedOptMappedSQLWriteForm[S, T](name.get + ".notNull", contramap) with NotNullWriteForm[T]

	private[schema] override lazy val cachedString = "<" + name.get + "[" + form + "]"
}


private[schema] class DerivedOptMappedColumnWriteForm[S, -T](constructorName :String, f :T => Option[S])
                                                            (implicit protected override val form :ColumnWriteForm[S])
	extends DerivedOptMappedSQLWriteForm[S, T](constructorName, f)
	   with SingletonColumnWriteForm[T] with UnspecifiedColumnFormAdapter
{ outer =>
	override def nullSafe :ColumnWriteForm[T] =
		new DerivedOptMappedColumnWriteForm[S, T](name.get + ".nullSafe", contramap)
			with NullSafeWriteForm[T] with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :ColumnWriteForm[T] =
		new DerivedOptMappedColumnWriteForm[S, T](name.get + ".notNull", contramap)
			with NotNullWriteForm[T] with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :ColumnWriteForm[T] =
		new DerivedOptMappedColumnWriteForm[S, T](name.get + ".notNull.nullSafe", contramap)
			with NotNullWriteForm[T] with NullSafeWriteForm[T] with ComparableWriteForm
}






private[oldsql] class MappedSQLForm[S, T](f :S => T, protected override val contramap :T => S,
                                          protected override val text :Opt[String] = Lack,
                                          protected override val suffix :String = "")
                                         (implicit protected override val form :SQLForm[S],
                                          implicit override val nulls :NullValue[T])
	extends MappedSQLReadForm[S, T](f) with MappedWriteForm[S, T] with SQLForm[T]
{ outer =>
	//map/flatMap not overriden to preserve a potentially custom name.
	override def nullSafe :SQLForm[T] =
		new MappedSQLForm[S, T](map, contramap, text, suffix + ".nullSafe")
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :SQLForm[T] =
		new MappedSQLForm[S, T](map, contramap, text, suffix + ".notNull")(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with ComparableReadForm with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :SQLForm[T] =
		new MappedSQLForm[S, T](map, contramap, text, suffix + ".notNull.nullSafe")(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with NullSafeWriteForm[T]
			with ComparableReadForm with ComparableWriteForm

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :MappedSQLForm[_, _] if other canEqual this =>
			map == other.map && contramap == other.contramap && form == other.form && nulls == other.nulls
		case _ => false
	}
	override def hashCode :Int = ((form.hashCode * 31 + map.hashCode) * 31 + contramap.hashCode) * 31 + nulls.hashCode

	private[schema] override lazy val cachedString :String =
		if (text.isEmpty) "<=[" + form + "]" + suffix + "=>" else "<" + text.get + suffix + ">"
}


private[oldsql] class MappedColumnForm[S, T](f :S => T, g :T => S, protected override val text :Opt[String] = Lack,
                                             protected override val suffix :String = "")
                                            (implicit override val form :ColumnForm[S],
                                             override val nulls :NullValue[T])
	extends MappedSQLForm(f, g, text)
		with ColumnForm[T] with SingletonColumnWriteForm[T] with UnspecifiedColumnFormAdapter
{ outer =>
	override def nullSafe :ColumnForm[T] =
		new MappedColumnForm[S, T](map, contramap, text, suffix + ".nullSafe")
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :ColumnForm[T] =
		new MappedColumnForm[S, T](map, contramap, text, suffix + ".notNull")(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with ComparableReadForm with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :ColumnForm[T] =
		new MappedColumnForm[S, T](map, contramap, text, suffix + ".notNull.nullSafe")(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with NullSafeWriteForm[T]
			with ComparableReadForm with ComparableWriteForm
}


private[schema] class DerivedMappedSQLForm[S, T](constructorName :String,
                                                 read :S => T, protected override val contramap :T => S)
                                                (implicit protected override val form :SQLForm[S], nulls :NullValue[T])
	extends DerivedMappedSQLReadForm[S, T](constructorName, read) with MappedWriteForm[S, T] with SQLForm[T]
{ outer =>
	override def nullSafe :SQLForm[T] =
		new DerivedMappedSQLForm[S, T](name.get + ".nullSafe", map, contramap)
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :SQLForm[T] =
		new DerivedMappedSQLForm[S, T](name.get + ".notNull", map, contramap)(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T]
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :SQLForm[T] =
		new DerivedMappedSQLForm[S, T](name.get + ".notNull.nullSafe", map, contramap)(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with NullSafeWriteForm[T]
			with ComparableReadForm with ComparableWriteForm

	private[schema] override lazy val cachedString :String = "<" + name.get + "[" + form +  "]>"
}


private[schema] class DerivedMappedColumnForm[S, T](constructorName :String, f :S => T, g :T => S)
                                                   (implicit override val form :ColumnForm[S],
                                                    override val nulls :NullValue[T])
	extends DerivedMappedSQLForm(constructorName, f, g)
	   with ColumnForm[T] with SingletonColumnWriteForm[T] with UnspecifiedColumnFormAdapter
{ outer =>
	override def nullSafe :ColumnForm[T] =
		new DerivedMappedColumnForm[S, T](name.get + ".nullSafe", map, contramap)
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :ColumnForm[T] =
		new DerivedMappedColumnForm[S, T](name.get + ".notNull", map, contramap)(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with ComparableReadForm with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :ColumnForm[T] =
		new DerivedMappedColumnForm[S, T](name.get + ".notNull.nullSafe", map, contramap)(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with NullSafeWriteForm[T]
			with ComparableReadForm with ComparableWriteForm
}




private[oldsql] class OptMappedSQLForm[S, T](fmap :S => Option[T], protected override val contramap :T => Option[S],
                                             protected override val text :Opt[String] = Lack,
                                             protected override val suffix :String = "")
                                            (implicit protected override val form :SQLForm[S],
                                             override val nulls :NullValue[T])
	extends OptMappedSQLReadForm[S, T](fmap) with OptMappedWriteForm[S, T] with SQLForm[T]
{ outer =>
	//map/flatMap not overriden to preserve potentially custom name.
	override def nullSafe :SQLForm[T] =
		new OptMappedSQLForm[S, T](map, contramap, text, suffix + ".nullSafe")
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :SQLForm[T] =
		new OptMappedSQLForm[S, T](map, contramap, text, suffix + ".notNull")(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with ComparableReadForm with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :SQLForm[T] =
		new OptMappedSQLForm[S, T](map, contramap, text, suffix + ".notNull.nullSafe")(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with NullSafeWriteForm[T]
			with ComparableReadForm with ComparableWriteForm

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :OptMappedSQLForm[_, _] if other canEqual this =>
			map == other.map && contramap == other.contramap && form == other.form && nulls == other.nulls
		case _ => false
	}
	override def hashCode :Int = ((form.hashCode * 31 + map.hashCode) * 31 + contramap.hashCode) * 31 + nulls.hashCode

	private[schema] override lazy val cachedString :String =
		if (text.isEmpty) "<=[" + form + "]" + suffix + "=>" else "<" + text.get + suffix + ">"
}


private[oldsql] class OptMappedColumnForm[S, T](f :S => Option[T], g :T => Option[S],
                                                protected override val text :Opt[String] = Lack,
                                                protected override val suffix :String = "")
                                               (implicit protected override val form :ColumnForm[S],
                                                implicit override val nulls :NullValue[T])
	extends OptMappedSQLForm(f, g, text)
		with ColumnForm[T] with SingletonColumnWriteForm[T] with UnspecifiedColumnFormAdapter
{ outer =>
	override def nullSafe :ColumnForm[T] =
		new OptMappedColumnForm[S, T](map, contramap, text, suffix + ".nullSaffe")
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :ColumnForm[T] =
		new OptMappedColumnForm[S, T](map, contramap, text, suffix + ".notNull")(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with ComparableReadForm with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :ColumnForm[T] =
		new OptMappedColumnForm[S, T](map, contramap, text, suffix + ".notNull.nullSafe")(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with NullSafeWriteForm[T]
			with ComparableReadForm with ComparableWriteForm
}


private[schema] class DerivedOptMappedSQLForm[S, T](constructorName :String,
                                                    fmap :S => Option[T], protected override val contramap :T => Option[S])
                                                   (implicit protected override val form :SQLForm[S],
                                                    override val nulls :NullValue[T])
	extends DerivedOptMappedSQLReadForm[S, T](constructorName, fmap) with OptMappedWriteForm[S, T] with SQLForm[T]
{ outer =>
	override def nullSafe :SQLForm[T] =
		new DerivedOptMappedSQLForm[S, T](name.get + ".nullSafe", map, contramap)
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :SQLForm[T] =
		new DerivedOptMappedSQLForm[S, T](name.get + ".notNull", map, contramap)(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with ComparableReadForm with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :SQLForm[T] =
		new DerivedOptMappedSQLForm[S, T](name.get + ".notNull.nullSafe", map, contramap)(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with NullSafeWriteForm[T]
			with ComparableReadForm with ComparableWriteForm

	private[schema] override lazy val cachedString :String =
		"<" + name.get + "[" + form + "]>"
}


private[schema] class DerivedOptMappedColumnForm[S, T](constructorName :String, f :S => Option[T], g :T => Option[S])
                                                      (implicit protected override val form :ColumnForm[S],
                                                       implicit override val nulls :NullValue[T])
	extends DerivedOptMappedSQLForm(constructorName, f, g)
	   with ColumnForm[T] with SingletonColumnWriteForm[T] with UnspecifiedColumnFormAdapter
{ outer =>
	override def nullSafe :ColumnForm[T] =
		new DerivedOptMappedColumnForm[S, T](name.get + ".nullSafe", map, contramap)
			with NullSafeWriteForm[T] with ComparableReadForm with ComparableWriteForm
		{
			override def notNull = outer.safeNotNull
		}
	override def notNull :ColumnForm[T] =
		new DerivedOptMappedColumnForm[S, T](name.get + ".notNull", map, contramap)(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with ComparableReadForm with ComparableWriteForm
		{
			override def nullSafe = outer.safeNotNull
		}
	private def safeNotNull :ColumnForm[T] =
		new DerivedOptMappedColumnForm[S, T](name.get + ".notNull.nullSafe", map, contramap)(form, NotNull)
			with NotNullForm[T] with ReadFormNullGuard[T] with NullSafeWriteForm[T]
			with ComparableReadForm with ComparableWriteForm
}


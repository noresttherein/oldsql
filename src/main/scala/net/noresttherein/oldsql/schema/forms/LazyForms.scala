package net.noresttherein.oldsql.schema.forms

import scala.collection.immutable.Seq

import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormProxy
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormProxy
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.{UnspecifiedColumnFormAdapter, UnspecifiedFormAdapter}
import net.noresttherein.oldsql.slang.classNameMethods





//todo: use AbstractIdempotent from the slang library instead
private[schema] trait AbstractLazyForm[+F <: UnspecifiedForm] extends UnspecifiedFormAdapter {
	@volatile protected[this] var initializer: () => F
	@volatile private[this] var initialized :F = _

	def isInitialized :Boolean = initialized != null

	protected override def form :F = {
		var f = initialized
		if (f == null) {
			val init = initializer
			if (init == null)
				f = initialized
			else {
				f = init()
				initialized = f
			}
		}
		f
	}
}




private[schema] trait LazyReadForm[+T] extends AbstractLazyForm[SQLReadForm[T]] with ReadFormProxy[T] {
	//better to risk too early evaluation and remove the decorator overhead
	override def map[X :NullValue](f :T => X) :SQLReadForm[X] = form.map(f)
	override def optMap[X :NullValue](f :T => Option[X]) :SQLReadForm[X] = form.optMap(f)

	override def toOpt :SQLReadForm[Option[T]] = form.toOpt
	override def notNull :SQLReadForm[T] = form.notNull
	override def *(repeat :Int) :SQLReadForm[Seq[T]] = form * repeat
	override def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = form * other
	override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] = form orElse fallback
	override def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = form <> write

	override def canEqual(that :Any) :Boolean =
		(that.asInstanceOf[AnyRef] eq this) || that.getClass == getClass && isInitialized

	override def toString :String =
		if (!isInitialized) "Lazy@" + this.shortHashString + ">" else "Lazy(" + form + ")"

}


private[schema] class LazySQLReadForm[+T](init :() => SQLReadForm[T]) extends LazyReadForm[T] {
	@volatile protected[this] override var initializer :() => SQLReadForm[T] = init
}


private[schema] trait LazyColumnReadForm[T]
	extends LazyReadForm[T] with AbstractLazyForm[ColumnReadForm[T]] with UnspecifiedColumnFormAdapter with ColumnReadForm[T]
{
	override def map[X :NullValue](f :T => X) :ColumnReadForm[X] = form.map(f)
	override def optMap[X :NullValue](f :T => Option[X]) :ColumnReadForm[X] = form.optMap(f)

	override def toOpt :ColumnReadForm[Option[T]] = form.toOpt
	override def notNull :ColumnReadForm[T] = form.notNull
	override def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] = form orElse fallback
	override def <>[O >: T](write :ColumnWriteForm[O]) :ColumnForm[O] = form <> write
}






private[schema] trait LazyWriteForm[-T] extends AbstractLazyForm[SQLWriteForm[T]] with WriteFormProxy[T] {
	override def split = form.split

	//better to risk too early evaluation and remove the decorator overhead
	override def unmap[X](f :X => T) :SQLWriteForm[X] = form.unmap(f)
	override def optUnmap[X](f :X => Option[T]) :SQLWriteForm[X] = form.optUnmap(f)

	override def toOpt = form.toOpt
	override def nullSafe = form.nullSafe
	override def notNull = form.notNull
	override def withNull(implicit nulls :NullValue[T]) = form.withNull
	override def withNull(nullValue :T) = form.withNull(nullValue)

	override def *(repeat :Int) = form.*(repeat)

	override def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] = form * other
	override def +[S <: T](next :SQLWriteForm[S]) :SQLWriteForm[S] = form + next
	override def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] = form <> read


	override def toString :String =
		if (!isInitialized) "<Lazy@" + this.shortHashString else "Lazy(" + form + ")"
}


private[schema] trait LazyColumnWriteForm[-T]
	extends LazyWriteForm[T] with AbstractLazyForm[ColumnWriteForm[T]]
	   with SingletonColumnWriteForm[T] with UnspecifiedColumnFormAdapter
{
	override def unmap[X](f :X => T) :ColumnWriteForm[X] = form.unmap(f)
	override def optUnmap[X](f :X => Option[T]) :ColumnWriteForm[X] = form.optUnmap(f)

	override def toOpt = form.toOpt
	override def nullSafe = form.nullSafe
	override def notNull = form.notNull
	override def withNull(implicit nulls :NullValue[T]) = form.withNull
	override def withNull(nullValue :T) = form.withNull(nullValue)

	override def <>[O <: T](read :ColumnReadForm[O]) = read <> this
}






private[schema] trait LazyForm[T]
	extends LazyReadForm[T] with LazyWriteForm[T] with AbstractLazyForm[SQLForm[T]] with SQLForm[T]
{

	override def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
		form.bimap(map)(unmap)

	override def optBimap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		form.optBimap(map)(unmap)

	override def toOpt = form.toOpt
	override def nullSafe = form.nullSafe
	override def notNull = form.notNull
	override def withNull(implicit nulls :NullValue[T]) = form.withNull
	override def withNull(nullValue :T) = form.withNull(nullValue)
	override def *(repeat :Int) = form.*(repeat)

	override def *[O](other :SQLForm[O]) :SQLForm[(T, O)] = form * other

	override def toString :String = if (isInitialized) "Lazy(" + form + ")" else "<Lazy@" + this.shortHashString + ">"
}


private[schema] class LazySQLForm[T](delayed: () => SQLForm[T]) extends LazyForm[T] {
	@volatile protected[this] override var initializer :() => SQLForm[T] = delayed
}



private[schema] class LazyColumnForm[T](target :() => ColumnForm[T])
	extends LazyForm[T] with LazyColumnReadForm[T] with LazyColumnWriteForm[T]
	   with AbstractLazyForm[ColumnForm[T]] with ColumnForm[T]
{
	@volatile protected[this] override var initializer :() => ColumnForm[T] = target

	override def bimap[X :NullValue](map :T => X)(unmap :X => T) =
		form.bimap[X](map)(unmap)

	override def optBimap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) =
		form.optBimap(map)(unmap)

	override def toOpt = form.toOpt
	override def nullSafe = form.nullSafe
	override def notNull = form.notNull
	override def withNull(implicit nulls :NullValue[T]) = form.withNull
	override def withNull(nullValue :T) = form.withNull(nullValue)
	override def *(repeat :Int) = form.*(repeat)
}


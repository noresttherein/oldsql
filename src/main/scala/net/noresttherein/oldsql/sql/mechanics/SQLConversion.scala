package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.{implicitNotFound, tailrec}
import scala.reflect.{classTag, ClassTag}

import net.noresttherein.oldsql.collection.{Chain, Listing, Opt}
import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.IllegalExpressionException
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormAdapter
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.forms.{MappedColumnForm, MappedColumnReadForm, MappedSQLForm, MappedSQLReadForm, OptMappedColumnForm, OptMappedSQLForm}
import net.noresttherein.oldsql.slang.{cast2TypeParams, classMethods, classNameMethods}
import net.noresttherein.oldsql.sql.{ColumnSQL, Query, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ConvertibleColumn
import net.noresttherein.oldsql.sql.Query.QueryTemplate
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, ConvertingTemplate, Grouped, Single}
import net.noresttherein.oldsql.sql.StoredProcedure.Out
import net.noresttherein.oldsql.sql.ast.{denullify, ChainSQL, LabeledSQL, MultiNull, QuerySQL, SQLNull, StandardTransformedSQL}
import net.noresttherein.oldsql.sql.ast.LabeledSQL.LabeledValueSQL
import net.noresttherein.oldsql.sql.ast.QuerySQL.Rows
import net.noresttherein.oldsql.sql.ast.RecordSQL.{RecordProjection, RecordTransformation}
import net.noresttherein.oldsql.sql.ast.RecordSQL.RecordProjection.SubRecordOf
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.mechanics.SQLAdaptation.DefaultExpressionAdaptation
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.{toSelf, ConvertChain, ConvertRecord, ConvertRows, ConvertSeq, ListingToChain, ReorderRecord}
import net.noresttherein.oldsql.sql.mechanics.SQLTransformation.{ArbitraryTransformation, SQLDecoration, TransformChain}




trait SQLValueConversion[X, Y] extends (X => Y) with Serializable {

	def apply(value :X)   :Y
	def unapply(value :Y) :Opt[X]

	@throws[UnsupportedOperationException]("if the conversion is not reversible.")
	@throws[IllegalArgumentException]("if the given value cannot be converted back to the input type.")
	def inverse(value :Y) :X

	/** True if this instance leaves the converted SQL expressions unchanged.
	  * @return `this` == [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation$ SQLTransformation]]`.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.toSelf toSelf]].
	  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.isDecorator isDecorator]]
	  */
	def isIdentity :Boolean = false

	/** True if the conversion always returns its argument,
	  * but may not be [[net.noresttherein.oldsql.sql.mechanics.SQLValueConversion.isReversible reversible]] because
	  * the output type is a proper supertype of the input type.
	  * Being an [[net.noresttherein.oldsql.sql.mechanics.SQLValueConversion.isIdentity identity]] automatically
	  * implies being an upcast conversion.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.Upcast]]
	  */
	def isUpcast :Boolean = isIdentity

	/** True for transformations which do not change input ''values'', but might change input expressions
	  * (in particular, wrapping them in another expression).
	  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.isIdentity isIdentity]]
	  */
	def isDecorator :Boolean = false

	/** True if the conversion is lossless,
	  * that is [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.inverse inverse]]`(apply(x)) == x`
	  * for every `x`.
	  */
	def isLossless :Boolean = false

	/** If true, this instance represents a two-way conversion:
	  * method [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.inverse inverse]] will always produce a value
	  * without throwing an exception. It needs not to be however a true isomorphic, as the inverse conversion
	  * may be lossy.
	  */
	def isReversible :Boolean = false


	def canEqual(that :Any) :Boolean = that.isInstanceOf[FormConversion[_, _]]

	/** Creates `toString` representation of this object by textually adapting `toString` of its arguments
	  * in the form of any preceding conversions. This is used to create `toString` representation
	  * of composed conversions, by recursively applying this method for all forms in the same way
	  * that `apply` calls are composed: a composition of two transformations `second compose first` implements it
	  * as `second.applyString(first.applyString(arg))`. Normally the process starts with a `"_"` placeholder as
	  * the initial argument in a manner similar with lambda expressions and expressions add a suffix to the argument
	  * resembling a method call, although this is not required. For example,
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation$ SQLTransformation]]`.`[[net.noresttherein.oldsql.sql.mechanics.SQLConversion.IntToDouble IntToDouble]]`.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.andThen andThen]]`(`[[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toOption SQLTransformation.toOption]]`)`,
	  * an [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation! SQLTransformation]]`[Int, Option[Double]]`,
	  * is formatted as `"_.toDouble.toOption"`.
	  */
	def applyString(arg :String) :String

	override def toString :String = applyString("_")
}




/** A potentially reversible conversion `X => Y` with factory methods for mapping
  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[X]` and
  * [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[X]` to forms for `Y`.
  * It is a common base type for [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation SQLTransformation]]
  * and [[net.noresttherein.oldsql.sql.mechanics.SpecificTransformation SpecificTransformation]].
  *
  * As this type is used to map read/write forms, not only read forms, and is used to represent the fact that the same
  * database type can be mapped to different Scala types, the conversion is expected to be reversible, possibly
  * with loss of precision, at least for some arguments. In particular, `inverse(apply(x)) == x` should ideally
  * hold for such conversions. This is not a strict requirement however, and methods
  * [[net.noresttherein.oldsql.sql.mechanics.SQLValueConversion.isLossless isLossless]] and
  * [[net.noresttherein.oldsql.sql.mechanics.SQLValueConversion.isReversible isReversible]] can be used to check
  * for these properties, while [[net.noresttherein.oldsql.sql.mechanics.SQLValueConversion.unapply unapply]]
  * can be used instead of [[net.noresttherein.oldsql.sql.mechanics.SQLValueConversion.inverse inverse]] in order to attempt
  * to reverse the conversion without throwing an exception.
  */
trait FormConversion[X, Y] extends Any with SQLValueConversion[X, Y] {

	/* Matching of forms created by our swap is important, because it is common when reforming SQL expressions
	 * (in particular terms) to already have a form of type `Y` from the other expression, but needing to
	 * 'unmap' it to a form of `X` in order for the whole expression to be mapped with this conversion, including,
	 * most most likely, its selectForm. This means that even if a conversion is not reversible, there is a chance
	 * everything will work correctly after we unwrap the original form from the one created by our swap.
	 */
	private trait ReadForm extends ReadFormAdapter[Y] {
		override def form :SQLReadForm[X] //made public
		def conversion = FormConversion.this
		override lazy val toString = FormConversion.this.applyString(form.toString) + "=>"
	}
	private trait ReadWriteForm extends ReadForm {
		override def form :SQLForm[X]
		override lazy val toString = "<=" + FormConversion.this.applyString(form.toString) + "=>"
	}


	def apply(form :SQLReadForm[X]) :SQLReadForm[Y] = form match {
		case swapped :FormConversion[Y, X]#ReadForm @unchecked if swapped.conversion == swap =>
			swapped.form
		case _ =>
			new MappedSQLReadForm[X, Y](apply)(form, form.nulls.map(apply)) with ReadForm
	}

	def apply(form :ColumnReadForm[X]) :ColumnReadForm[Y] = form match {
		case swapped :FormConversion[Y, X]#ReadForm @unchecked if swapped.conversion == swap =>
			swapped.form.asInstanceOf[ColumnReadForm[Y]]
		case _ =>
			new MappedColumnReadForm[X, Y](apply)(form, form.nulls.map(apply)) with ReadForm
	}

	def apply(form :SQLForm[X]) :SQLForm[Y] = form match {
		case swapped :FormConversion[Y, X]#ReadWriteForm @unchecked if swapped.conversion == swap =>
			swapped.form
		case _ if isReversible =>
			new MappedSQLForm[X, Y](apply, inverse)(form, form.nulls.map(apply)) with ReadWriteForm
		case _ =>
			new OptMappedSQLForm[X, Y](x => Some(apply(x)), unapply)(form, form.nulls.map(apply)) with ReadWriteForm
	}

	def apply(form :ColumnForm[X]) :ColumnForm[Y] = form match {
		case swapped :FormConversion[Y, X]#ReadWriteForm @unchecked if swapped.conversion == swap =>
			swapped.form.asInstanceOf[ColumnForm[Y]]
		case _ if isReversible =>
			new MappedColumnForm[X, Y](apply, inverse)(form, form.nulls.map(apply)) with ReadWriteForm
		case _ =>
			new OptMappedColumnForm[X, Y](x => Some(apply(x)), unapply)(form, form.nulls.map(apply)) with ReadWriteForm
	}

	def swap :FormConversion[Y, X]
}


private object FormConversion {
	object ReadForm {
		def unapply[X](form :SQLReadForm[X]) :Opt[FormConversion[_, _ <: X]] = form match {
			case converted :FormConversion[_, X]#ReadForm @unchecked => Got(converted.conversion)
			case _ => Lack
		}
	}
	object Form {
		def unapply[X](form :SQLForm[X]) :Opt[FormConversion[_, X]] = form match {
			case converted :FormConversion[_, X]#ReadWriteForm @unchecked => Got(converted.conversion)
			case _ => Lack
		}
	}
}


//currently unused because conversion forms are created in FormConversion, extended by SQLAdaptation
/*
private trait TransformationForm[X, Y] extends ReadFormAdapter[Y] with Serializable {
	def conversion :SQLTransformation[X, Y]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :TransformationForm[_, _] if getClass == other.getClass =>
			form == other.form && conversion == other.conversion
		case _ => false
	}
	override def hashCode :Int = form.hashCode * 31 + conversion.hashCode
}
*/






/** A function `X => Y` lifted to be applicable to [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, X]`,
  * [[net.noresttherein.oldsql.sql.Query Query]]`[P, X]`, [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[X]`
  * and some other form types. It is used internally primarily as a chainable constructor of specific composite
  * expressions wrapping another expression. Applications normally deal only with its subclasses
  * [[net.noresttherein.oldsql.sql.mechanics.SQLAdaptation SQLAdaptation]] and, in particular,
  * [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]], which create adapters
  * which are rendered as the same SQL as the adapted expressions. Unlike the former, implementations of
  * of `SQLTransformation` are allowed to create completely new expressions of arbitrary types.
  * Furtheremore, an expression transformed by
  */
@implicitNotFound("I do not know how to convert an SQLExpression[F, S, ${X}] to an SQLExpression[F, S, ${Y}]. " +
                  "Missing implicit SQLTransformation[${X}, ${Y}].")
trait SQLTransformation[X, Y] extends FormConversion[X, Y] { self =>
	/* Consider: If we removed the upper bound, we could easily implement LayoutReform, that is a Reform which
	 *  returns SQLExpression[Nothing, Grouped, V] being a more representative combination of left and right.
	 *  However, this would prevent us from doing anything in SQLExpression.reform with the converted expressions, and,
	 *  what's worse, combining converted elements from a tuple expression. I see two ways around it, both ugly:
	 *    1. Introduce in Reform an upper bound (or bounds) on clause types `L` and `R`, as well as scope types
	 *       of accepted expressions and make SQLExpression.reform accept only reforms which can accept both expressions.
	 *       Ugly because all reform methods will have *even more* complex signatures for what is a niche
	 *       internal functionality.
	 *    1. Define in Reform:
	 *           type ResultClause[F] <: RowProduct  // =:= F in all standard implementations
	 *           type ResultScope[S] <: Single // =:= S in all standard implementations
	 *       and, instead of (SQLExpression[L, A, U], SQLExpression[R, A, U])
	 *       or (leftRes.SQLResult[L, A], rightRes.SQLResult[R, B]) return
	 *       (leftRes.SQLResult[ResultClause[L], ResultScope[A]], rightRes.SQLResult[ResultClause[R], ResultScope[B]])
	 *       Likewise, instead of leftRes.ConvertibleResult[L, A, E[U]] or rightRes.ConvertibleResult[R, B, E[U]]
	 *       return leftRes.ConvertibleResult[ResultClause[L], ResultScope[A], E[U]) (or symmetrical).
	 *       We have a variance problem however - ResultClause/ResultScope are invariant in F, S, but are both
	 *       used in contravariant positions in SQLExpression in SQLResult. This perhaps could be fixed
	 *       by declaring bounds ResultClause[+F] <: F and ResultScope[+S] <: S instead of outright
	 *       defining them through as equality, but instead narrowing the *method* return type
	 *       from SQLExpression[_ <: F, _ <: S, V] to SQLExpression[F, S, V] in normal implementations.
	 */
	/** A type alias making it easier to refine this trait with
	  * its [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.SQLResult SQLResult]] type without
	  * having to declare all parameter types of the latter. Use:
	  * {{{
	  *     def m[X, Y, R[F <: RowProduct, S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]]]
	  *             :SQLTransformation[X, Y]#Into[R]
	  * }}}
	  */
	type Into[R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLTransformation[X, Y] {
			type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: R[F, S, E]
		}
//
//	type BoundBy[R[F <: RowProduct, S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
//		SQLTransformation[X, Y] {
//			type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: R[F, S, E]
//		}

	/** Equals `this.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.SQLResult SQLResult]]`[F, S, SQLExpression[F, S, Y]]`
	  * in all concrete implementations. Declared here as double bounded instead in order to be able to impose
	  * an upper bound on it through refinement,
	  * needed by [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.compose compose]].
	  */
//	type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//		SQLResult[F, S, SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]
	type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
		SQLResult[F, S, SQLExpression[F, S, Y]] <: SQLResult[F, S, SQLExpression[F, S, Y]]

	//E must be covariant because we want Expr type parameter to ConvertibleSQL to also be covariant
	/** The type of SQL expressions created by this instance from expressions conforming to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]]`[F, S, X, EC, EC[X]]`,
	  * where type parameter `E` is always specified as `EC[Y]` - the type to which such an argument expression
	  * would [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.convert convert]]
	  * with an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]].
	  * It follows, that the latter defines this type as `EC[Y]`, while most other implementations (typically extending
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.ArbitraryTransformation ArbitraryTransformation]])
	  * define it simply as `SQLExpression[F, S, Y]`.
	  * Specialized implementations may potentially implement any transformation, having any expression type
	  * as the conversion result.
	  */ //todo: modify it to take E[v] <: ConvertibleSQL[F, S, v] as a parameter - the only argument type we accept
	type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]

	/** The supertype of all queries created by this instance through converting rows of a `Query[P, X]`. */
	type QueryResult[P, +Q <: Query[P, Y]] <: Query[P, Y]


	def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	         (expr :ConvertingTemplate[F, S, X, E]) :SQLResult[F, S, E[Y]]

//	def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//	         (expr :ConvertibleSQL[F, S, X, E]) :SQLResult[F, S, E[Y]]
	//an alias because overloading fools the type inferer
	def convert[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	           (expr :ConvertingTemplate[F, S, X, E]) :SQLResult[F, S, E[Y]] =
		apply[F, S, E](expr)

	def default[F <: RowProduct, S >: Grouped <: Single](expr :SQLExpression[F, S, X]) :BaseResult[F, S] =
		apply[F, S, SQLExpression.from[F]#rows[S]#E](expr)

	//todo: rename to rows, because overload resolution would pick this even for SQLTransformation[Rows[X], Y]
	//todo: use QueryResult?
	def apply[F <: RowProduct](query :QuerySQL[F, X]) :QuerySQL[F, Y] = query.transform(this)
	def apply[P](query :Query[P, X]) :Query[P, Y] = query.transform(this)
	def apply(query :TopSelectSQL[X]) :TopSelectSQL[Y] = query.transform(this)

//	/** True if this instance converts SQL expression to instances retaining a reference to the original.
//	  * This is true for the majority of conversions, which delegate to
//	  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.to to]]
//	  * or create an adapter expression such as [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]],
//	  * as well as the identity conversion. It is true for those rare cases where an `SQLTransformation` completely
//	  * recreates an expression, rather simply adapting/enriching it. It is introduced to allow an adapter expressing
//	  * to delegate to `conversion(underlying)` within its `reform` method in order to reform transformed expressions
//	  * without fear of causing an infinite recursion by endlessly re-creating the adapter.
//	  */
//	@deprecated("Infinite recursion is still possible", "0.0")
//	def isDerived :Boolean = false

	//return types have bounds on BaseResult only because using equality would in Scala 2 break the existing bound >: SQLResult
	def compose[W](first :SQLTransformation[W, X])
			:SQLTransformation[W, Y] {
//				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//					SQLResult[F, S, SQLExpression[F, S, Y]] <: self.BaseResult[F, S]
//				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
//					self.BaseResult[F, S]
				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
					SQLResult[F, S, SQLExpression[F, S, Y]] <: SQLResult[F, S, SQLExpression[F, S, Y]]//= self.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
					self.BaseResult[F, S]
			} =
		first match {
			case conversion :SQLConversion[W, X] => compose(conversion)
			case _ => new ArbitraryComposedTransformation[W, X, Y, this.type](first, this)
		}

	def compose[W](first :SQLAdaptation[W, X])
		:SQLTransformation[W, Y] {
//				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//					SQLResult[F, S, SQLExpression[F, S, Y]] <: self.BaseResult[F, S]
//				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
//					self.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
					self.BaseResult[F, S]
			} =
		compose(first :SQLTransformation[W, X])

	def compose[W](first :SQLConversion[W, X])
			:SQLTransformation[W, Y] {
				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = self.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = self.SQLResult[F, S, E]
			} =
		if (first.isIdentity) first andThen this
		else new ConversionComposedTransformation[W, X, Y, this.type](first, this)

	/** Alias for `andThen`, because overload with inherited method from `X => Y` confuses the compiler. */
	def ==>[Z](second :SQLTransformation[Y, Z])
			:SQLTransformation[X, Z] {
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <:
					second.BaseResult[F, S]
			} =
		second compose this

	def andThen[Z](second :SQLTransformation[Y, Z]) //:SQLTransformation[X, Z] = second compose this
			:SQLTransformation[X, Z] {
//				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//					SQLResult[F, S, SQLExpression[F, S, Z]] <: second.BaseResult[F, S]
//				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <:
//					second.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <:
					second.BaseResult[F, S]
			} =
		second compose this

	private[mechanics] def split
			:Opt[(SQLTransformation[X, A], SQLTransformation[A, Y]#Into[SQLResult]) forSome { type A }] = Lack

	/** A factory method used to produce the [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.swap inverse]]
	  * of this conversion; the arguments are used to implement
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.applyString applyString]] by surrounding the argument.
	  */
	protected def swap(namePrefix :String, nameSuffix :String) :SQLTransformation[Y, X] =
		new Swap {
			override def applyString(arg :String) = namePrefix + arg + nameSuffix
		}

	/** An inverse of this conversion, that is a conversion such that `swap.swap == this`.
	  * Method `swap.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.apply(value:X)* apply]] delegates to
	  * `this.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.inverse inverse]] and vice versa.
	  * This means that the conversion may throw an exception for some, or even all of its arguments;
	  * it should be used only as a last resort,
	  * unless `this.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.isReversible isReversible]].
	  */
	override def swap :SQLTransformation[Y, X] = new Swap

	private class Swap extends ArbitraryTransformation[Y, X] with InverseTransformation[Y, X] {
		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingTemplate[F, S, Y, E]) :SQLExpression[F, S, X] =
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, Y, E]) :SQLExpression[F, S, X] =
			if (expr == null) SQLNull[X]()
			else new StandardTransformedSQL[F, S, Y, X](expr.toConvertibleSQL, this)
		override val swap = SQLTransformation.this
	}

//	def generic[F <: RowProduct, S >: Grouped <: Single]
//			:SpecificTransformation[X, Y, SQLExpression.from[F]#rows[S]#E, SQLExpression[F, S, X], SQLExpression[F, S, Y]] =
//		specific[F, S, SQLExpression.from[F]#rows[S]#E]
//
//	def specific[F <: RowProduct, S >: Grouped <: Single,
//	             E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//			:SpecificTransformation[X, Y, E, E[X], SQLResult[F, S, E[Y]]] =
//		SpecificTransformation[F, S, X, Y, E](this)
//
//	def specific[F <: RowProduct, S >: Grouped <: Single,
//	             E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//	            (e :ConvertingTemplate[F, S, X, E, E[X]]) :SpecificTransformation[X, Y, E, E[X], E[Y]] =
//		specific[F, S, E]


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLTransformation[_, _]]
}




object SQLTransformation {
	def summon[X, Y](implicit lift :SQLTransformation[X, Y]) :lift.type = lift

	@inline implicit def implicitSQLAdaptation[X, Y](implicit conversion :SQLAdaptation[X, Y])
			:SQLAdaptation[X, Y]#As[conversion.SQLResult] =
		conversion

//	def const[X, Y](value :Y, expr :SQLExpression[RowProduct, Single, Y]) :SQLTransformation[X, Y] =
//		new ConstantTransformation(value, expr)


	trait SQLDecoration[V] extends SQLTransformation[V, V] with FormConversion[V, V] {
		override def isDecorator = true
		override def apply(value :V) :V = value
		override def unapply(value :V) :Got[V] = Got(value)
		override def inverse(value :V) :V = value

		override def apply(form :SQLReadForm[V]) :SQLReadForm[V] = form
		override def apply(form :ColumnReadForm[V]) :ColumnReadForm[V] = form
		override def apply(form :SQLForm[V]) :SQLForm[V] = form
		override def apply(form :ColumnForm[V]) :ColumnForm[V] = form

		override def isReversible = true
		override def isLossless   = true

		override def swap :SQLTransformation[V, V] = this
	}


	trait ArbitraryTransformation[X, Y] extends SQLTransformation[X, Y] {
		override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = SQLExpression[F, S, Y]
		override type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] =
			SQLExpression[F, S, Y]
//		override type QueryResult[P, +Q <: Query[P, Y]] = Query[P, Y]

		override def unapply(value :Y) :Opt[X] = Lack
		override def inverse(value :Y) :X = unapply(value) match {
			case Got(x) => x
			case _ => throw new IllegalArgumentException(
				s"Cannot convert $value :${value.localClassName} back with $this."
			)
		}

		override lazy val swap :SQLTransformation[Y, X] = super.swap
	}


	object Composition {
		def unapply[X, Z](e :SQLTransformation[X, Z])
				:Opt[(SQLTransformation[X, Y], SQLTransformation[Y, Z]#Into[e.SQLResult]) forSome { type Y }] =
			e.split

		def unapply[X, Z](e :SQLAdaptation[X, Z])
				:Opt[(SQLAdaptation[X, Y], SQLAdaptation[Y, Z]#As[e.SQLResult]) forSome { type Y }] =
			e.split

		def unapply[X, Z](e :SQLConversion[X, Z])
				:Opt[(SQLConversion[X, Y], SQLConversion[Y, Z]) forSome { type Y }] =
			e.split
	}
	type from[X] = { type to[Y] = SQLTransformation[X, Y] }
	type to[Y] = { type from[X] = SQLTransformation[X, Y] }



	/** Recursively performs a 'deep conversion' of a chain expression `SQLExpression[F, S, XI ~ XL]`
	  * to another `SQLExpression[F, S, XI ~ XL]`.
	  */
	case class TransformChain[XI <: Chain, XL, YI <: Chain, YL]
	                         (init :SQLTransformation[XI, YI], last :SQLTransformation[XL, YL])
		extends ArbitraryTransformation[XI ~ XL, YI ~ YL]
	{

		override def apply(value :XI ~ XL) :YI ~ YL = init(value.init) ~ last(value.last)

		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingTemplate[F, S, XI ~ XL, E]) :SQLExpression[F, S, YI ~ YL] =
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, XI ~ XL, E]) :SQLExpression[F, S, YI ~ YL] =
			expr match {
				case null => MultiNull[YI ~ YL]()
				case nullSQL :MultiNull[XI ~ XL] =>
					val form = nullSQL.form.nullOptBimap(
						chain => Some(init(chain.init) ~ last(chain.last)))(
						chain => for { i <- init.unapply(chain.init); l <- last.unapply(chain.last) } yield i ~ l
					)
					MultiNull[YI ~ YL](form)
				case chain :ChainSQL[F, S, XI, XL] =>
					init(chain.init) ~ last(chain.last)
//				case adapted :AdaptedSQL[F, S, x, XI ~ XL] =>
//					adapted.conversion(apply(adapted.value))
				case _ => (init, last) match {
					case (i :SQLConversion[XI, YI], l :SQLConversion[XL, YL]) =>
						ConvertChain(i, l)(expr)
					case _ => throw new IllegalExpressionException(
						"Cannot transform expression " + expr + " with " + this +
							" because it is neither ChainSQL nor ChainTuple, nor AdaptedSQL."
					)
				}
			}
		override def applyString(arg :String) :String = init.applyString(arg) + "~(" + last + ")"
//		override def applyString(arg :String) :String = {
//			def rec(trans :SQLTransformation[_, _], res :StringBuilder = new StringBuilder(arg)) :StringBuilder =
//				trans match {
//					case chain :TransformChain[_, _, _, _] =>
//						rec(chain.init, res) ++= last.toString += ','
//					case ident if ident == SQLConversion.toSelf => res += ".map("
//				}
//			(rec(init) ++= last.toString += ')').result()
//		}
	}

	case class TransformListing[XI <: Listing, XL, YI <: Listing, YL, K <: Label]
	                           (init :SQLTransformation[XI, YI], last :SQLTransformation[XL, YL])
		extends ArbitraryTransformation[XI |~ (K :~ XL), YI |~ (K :~ YL)]
	{

		override def apply(value :XI |~ (K :~ XL)) :YI |~ (K :~ YL) = init(value.init) |~ :~[K](last(value.last))

		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingTemplate[F, S, XI |~ (K :~ XL), E]) :SQLExpression[F, S, YI |~ (K :~ YL)] =
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, XI |~ (K :~ XL), E]) :SQLExpression[F, S, YI |~ (K :~ YL)] =
			expr match {
				case null => null
				case listing :LabeledSQL[F, S, XI |~ (K :~ XL)] => init(listing.init) match {
					case yi :LabeledSQL[F, S, YI] => last(listing.last) match {
						case yl :LabeledValueSQL[F, S, YL] =>
							(yi |~ :~[K](yl))(new ValueOf[K](listing.lastKey))
						case yl =>
							throw new IllegalExpressionException(
								"Cannot transform " + expr + " with " + this + ": transformation " + last +
									" should have created a LabeledValueSQL, but got " + yl + ".")
					}
					case yi =>
						throw new IllegalExpressionException(
							"Cannot transform " + expr + " with " + this + ": transformation " + init +
								" should have created a LabeledSQL, but got " + yi + ".")
				}
				case _ => (init, last) match {
					case (i :SQLConversion[XI, YI], l :SQLConversion[XL, YL]) =>
						SQLConversion.record[XI, XL, YI, YL, K](i, l)(expr)
					case _ => throw new IllegalExpressionException(
						"Cannot transform expression " + expr + " with " + this + " because it is not a LabeledSQL."
					)
				}
			}
	}


	private case class ConstantTransformation[X, Y](value :Y, result :SQLExpression[RowProduct, Single, Y])
		extends ArbitraryTransformation[X, Y]
	{
		override def apply(value :X) :Y = this.value

		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingTemplate[F, S, X, E]) :SQLExpression[F, S, Y] =
			denullify(result).toConvertibleSQL
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, X, E]) :SQLExpression[F, S, Y] =
//			result

		override def applyString(arg :String) :String = arg + ".return(" + value + ")"
	}
}


private trait InverseTransformation[X, Y] extends SQLTransformation[X, Y] {
	override def apply(value :X)   = swap.inverse(value)
	override def inverse(value :Y) = swap(value)
	override def unapply(value :Y) = Got(swap(value))
	override def isReversible = true
	override def applyString(arg :String) = arg + ".inverse(" + swap + ")"

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :InverseTransformation[_, _] if canEqual(other) && other.canEqual(this) => swap == other.swap
		case _ => false
	}
	override def hashCode :Int = Integer.rotateLeft(swap.hashCode, 16)
}


private[sql] trait ComposedTransformation[X, Y, Z] extends SQLTransformation[X, Z] { self =>
//	override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = second.BaseResult[F, S]
//	override type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] >:
//		second.SQLResult[F, S, E] <: second.BaseResult[F, S]
	val first  :SQLTransformation[X, Y]
	val second :SQLTransformation[Y, Z] {
		type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = self.BaseResult[F, S]
		type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <: self.SQLResult[F, S, E]
	}
	override def apply(value: X)   :Z = second(first(value))
	override def unapply(value: Z) :Opt[X] = second.unapply(value).flatMap(first.unapply)
	override def inverse(value :Z) :X = first.inverse(second.inverse(value))

//	override def isDerived    :Boolean = first.isDerived && second.isDerived
	override def isReversible :Boolean = first.isReversible && second.isReversible
	override def isLossless   :Boolean = first.isLossless && second.isLossless

	protected[mechanics] override def split :Opt[(SQLTransformation[X, Y], SQLTransformation[Y, Z]#Into[SQLResult])] =
		Got(first, second)

	override lazy val swap :SQLTransformation[Z, X] = new ArbitraryComposedTransformation(second.swap, first.swap) {
		override def isReversible = true
		override lazy val swap = ComposedTransformation.this
		override def applyString(arg :String) = arg + ".inverse(" + swap + ")"
	}
	//this could be improved by expanding both to sequences, abstracting over composition order
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :ComposedTransformation[_, _, _] if canEqual(that) && other.canEqual(this) =>
			first == other.first && second == other.second
		case _ => false
	}
	override def hashCode :Int = first.hashCode * 31 + second.hashCode

	override def applyString(arg :String) :String = second.applyString(first.applyString(arg))
}


private class ArbitraryComposedTransformation[X, Y, Z, T <: SQLTransformation[Y, Z]]
                                             (override val first :SQLTransformation[X, Y], override val second :T)
	extends ComposedTransformation[X, Y, Z]
{
	override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = second.BaseResult[F, S]
	override type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
		second.BaseResult[F, S]

	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	                  (expr :ConvertingTemplate[F, S, X, E]) :second.BaseResult[F, S] =
		second(first(expr) :ConvertingTemplate[F, S, Y, SQLExpression.from[F]#rows[S]#E])
//	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//	                  (expr :ConvertibleSQL[F, S, X, E]) :second.BaseResult[F, S] =
//		second(first(expr) :ConvertibleSQL[F, S, Y, SQLExpression.from[F]#rows[S]#E])
}


private class ConversionComposedTransformation[X, Y, Z, T <: SQLTransformation[Y, Z]]
                                              (override val first :SQLConversion[X, Y], override val second :T)
	extends ComposedTransformation[X, Y, Z]
{
	override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = second.BaseResult[F, S]
	override type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
		second.SQLResult[F, S, E]

	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	                  (expr :ConvertingTemplate[F, S, X, E]) :SQLResult[F, S, E[Z]] =
		second[F, S, E](first(expr))
//	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//	                  (expr :ConvertibleSQL[F, S, X, E]) :SQLResult[F, S, E[Z]] =
//		second[F, S, E](first(expr))
}






/** A conversion of any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, X]` to
  *  an `SQLExpression[F, S, Y]` which has the same column set as the original. The default implementation delegates to
  * [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.adapt transform]],
  * which, by default, creates an [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]]
  * (or [[net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL AdaptedColumnSQL]]) instance wrapping
  * the argument expression and this conversion, but custom implementations can create directly specific wrappers
  * (usually subclasses of the former two).
  *
  * It serves two purposes:
  *   1. conversions between Scala types which are represented as compatible (or the same) SQL types,
  *      for which a dedicated [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]] subclass is used;
  *   1. constructors of specific expression adapter classes, often preserving the value type,
  *      which introduce additional information about the expression.
  *
  * The difference in handling lies in how conversions are composed: a composition of two or more `SQLConversion`s
  * is treated like every other `SQLConversion` and will create an instance
  * of [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]] (or its more specific subclass) carrying
  * the expression being converted and the composed conversion instance. On other hand,
  * if any of the composed conversions are not an `SQLConversion`, the composition is implemented as chaining
  * `apply` methods: each conversion is applied individually to the expression returned by the previous conversion.
  * Furthermore, it is assumed that an `SQLConversion` limits itself to simply converting the value type,
  * and its result can be sometimes replaced with an externally created `ComposedSQL` instance. This is never the case
  * for arbitrary `SQLAdaptation`s, as it is assumed that the adapted expression carries important information
  * which cannot be discarded.
  *
  * @see [[net.noresttherein.oldsql.sql.mechanics.Interoperable]]
  */
@implicitNotFound("Type ${X} cannot be used in place of type ${Y} in SQL. Missing implicit SQLAdaptation[${X}, ${Y}].")
trait SQLAdaptation[X, Y] extends SQLTransformation[X, Y] with FormConversion[X, Y] { self =>
//	override def isDerived :Boolean = true

	type As[R[F <: RowProduct, S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLAdaptation[X, Y] {
			type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = R[F, S, E]
		}

	//todo: BaseColumn; bound ColumnSQL[F, S, Y] with SQLResult[F, S, ColumnSQL[F, S, Y]]
	type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Y]] <: ColumnSQL[F, S, Y]

	def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
	          (expr :ConvertibleColumn[F, S, X, E]) :ColumnResult[F, S, E[Y]]


	override def compose[W](first :SQLTransformation[W, X])
			:SQLTransformation[W, Y] {
//				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//					SQLResult[F, S, SQLExpression[F, S, Y]] <: self.BaseResult[F, S]
//				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
//					self.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
					self.BaseResult[F, S]
			} =
		first match {
			case conversion :SQLAdaptation[W, X] => compose(conversion)
			case _ => super.compose(first)
		}

	def compose[W](first :SQLAdaptation[W, X])
			:SQLAdaptation[W, Y] { //todo: bounds on ColumnResult
//				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//					SQLResult[F, S, SQLExpression[F, S, Y]] <: self.BaseResult[F, S]
//				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
//					self.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
					self.BaseResult[F, S]
			} =
		first match {
			case conversion :SQLConversion[W, X] => compose(conversion)
			case _ => new ArbitraryComposedAdaptation[W, X, Y, this.type](first, this)
		}

	override def compose[W](first :SQLConversion[W, X])
			:SQLAdaptation[W, Y] {
				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = self.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] =
					self.SQLResult[F, S, E]
				type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, E <: ColumnSQL[F, S, Y]] =
					self.ColumnResult[F, S, E]
			} =
		if (first.isIdentity) first andThen this
		else new ConversionComposedAdaptation[W, X, Y, this.type](first, this)

//	override def andThen[Z](second :SQLTransformation[Y, Z]) :SQLTransformation[X, Z] = second andThen this
	override def andThen[Z](second :SQLTransformation[Y, Z]) //:SQLTransformation[X, Z] = second compose this
			:SQLAdaptation[X, Z] {
//				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//					SQLResult[F, S, SQLExpression[F, S, Z]] <: second.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <:
					second.BaseResult[F, S]
			} =
		second compose this

	def andThen[Z](second :SQLAdaptation[Y, Z])
			:SQLAdaptation[X, Z] {
//				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//					SQLResult[F, S, SQLExpression[F, S, Z]] <: second.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <:
					second.BaseResult[F, S]
			} =
		second compose this

	protected[mechanics] override def split
			:Opt[(SQLAdaptation[X, A], SQLAdaptation[A, Y]#As[SQLResult]) forSome { type A }] = Lack


	protected override def swap(namePrefix :String, nameSuffix :String) :SQLAdaptation[Y, X] =
		new DefaultExpressionAdaptation[Y, X] with InverseTransformation[Y, X] {
			override val swap = SQLAdaptation.this
			override def applyString(arg :String) = namePrefix + arg + nameSuffix
		}

	override def swap :SQLAdaptation[Y, X] =
		new DefaultExpressionAdaptation[Y, X] with InverseTransformation[Y, X] {
			override val swap = SQLAdaptation.this
		}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLAdaptation[_, _]]
}




object SQLAdaptation {
	def summon[X, Y](implicit lift :SQLAdaptation[X, Y]) :lift.type = lift

	@inline implicit def implicitSQLConversion[X, Y](implicit conversion :SQLConversion[X, Y]) //:SQLConversion[X, Y] =
			:SQLAdaptation[X, Y]#As[conversion.SQLResult] =
		conversion

	def apply[X, Y](suffix :String, convert :X => Y) :ArbitraryAdaptation[X, Y] =
		new DefaultExpressionAdaptation[X, Y] {
			override def apply(value :X) :Y = convert(value)
			override def applyString(arg :String) = arg + suffix
		}

	def apply[X, Y](suffix :String, convert :X => Y, inversed :Y => Opt[X]) :ArbitraryAdaptation[X, Y] =
		new DefaultExpressionAdaptation[X, Y] {
			override def apply(value :X) :Y = convert(value)
			override def unapply(value :Y) :Opt[X] = inversed(value)
			override def applyString(arg :String) = arg + suffix
		}


	type from[X] = { type to[Y] = SQLAdaptation[X, Y] }
	type to[Y] = { type from[X] = SQLAdaptation[X, Y] }

	trait ArbitraryAdaptation[X, Y] extends SQLAdaptation[X, Y] with ArbitraryTransformation[X, Y] {
		override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Y]] =
			ColumnSQL[F, S, Y]

		override lazy val swap :SQLAdaptation[Y, X] =
			new DefaultExpressionAdaptation[Y, X] with InverseTransformation[Y, X] {
				override lazy val swap = ArbitraryAdaptation.this
			}
	}

	private trait DefaultExpressionAdaptation[X, Y] extends ArbitraryAdaptation[X, Y] {
		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingTemplate[F, S, X, E]) :SQLExpression[F, S, Y] =
			denullify(expr).`->adapt`(this)
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, X, E]) :SQLExpression[F, S, Y] =
//			expr.`->adapt`(this)

		override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
		                   (expr :ConvertibleColumn[F, S, X, E]) :ColumnSQL[F, S, Y] =
			denullify(expr).`->adapt`(this)
	}


	object Composition {
		def unapply[X, Z](e :SQLAdaptation[X, Z])
				:Opt[(SQLAdaptation[X, Y], SQLAdaptation[Y, Z]#As[e.SQLResult]) forSome { type Y }] =
			e.split

		def unapply[X, Z](e :SQLConversion[X, Z])
				:Opt[(SQLConversion[X, Y], SQLConversion[Y, Z]) forSome { type Y }] =
			SQLConversion.Composition.unapply(e)
	}

	object ReadForm {
		def unapply[X](form :SQLReadForm[X]) :Opt[SQLAdaptation[_, _ <: X]] = form match {
			case FormConversion.ReadForm(conversion :SQLAdaptation[_, X] @unchecked) => Got(conversion)
			case _ => Lack
		}
	}
	object Form {
		def unapply[X](form :SQLForm[X]) :Opt[SQLAdaptation[_, X]] = form match {
			case FormConversion.Form(conversion :SQLAdaptation[_, X] @unchecked) => Got(conversion)
			case _ => Lack
		}
	}

}


private trait ComposedAdaptation[X, Y, Z] extends ComposedTransformation[X, Y, Z] with SQLAdaptation[X, Z] { self =>
	override val first  :SQLAdaptation[X, Y]
	override val second :SQLAdaptation[Y, Z] {
		type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = self.BaseResult[F, S]
		type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <: self.SQLResult[F, S, E]
	}

//	override def apply(form :SQLReadForm[X])    :SQLReadForm[Z] = second(first(form))
//	override def apply(form :ColumnReadForm[X]) :ColumnReadForm[Z] = second(first(form))
//	override def apply(form :SQLForm[X])        :SQLForm[Z] = second(first(form))
//	override def apply(form :ColumnForm[X])     :ColumnForm[Z] = second(first(form))

	protected[mechanics] override def split :Opt[(SQLAdaptation[X, Y], SQLAdaptation[Y, Z]#As[SQLResult])] =
		Got((first, second))

	override lazy val swap :SQLAdaptation[Z, X] = new ArbitraryComposedAdaptation(second.swap, first.swap) {
		override def isReversible = true
		override lazy val swap = ComposedAdaptation.this
		override def applyString(arg :String) = arg + ".inverse(" + swap + ")"
	}
}


private class ArbitraryComposedAdaptation[X, Y, Z, T <: SQLAdaptation[Y, Z]]
                                         (override val first :SQLAdaptation[X, Y], override val second :T)
	extends ArbitraryComposedTransformation[X, Y, Z, T](first, second) with ComposedAdaptation[X, Y, Z]
{
	override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Z]] =
		ColumnSQL[F, S, Z]

	override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
	                   (expr :ConvertibleColumn[F, S, X, E]) :ColumnResult[F, S, E[Z]] =
		second.column[F, S, SQLExpression.from[F]#rows[S]#C](first.column[F, S, E](expr))
}

private class ConversionComposedAdaptation[X, Y, Z, T <: SQLAdaptation[Y, Z]]
                                          (override val first :SQLConversion[X, Y], override val second :T)
	extends ConversionComposedTransformation[X, Y, Z, T](first, second) with ComposedAdaptation[X, Y, Z]
{
	override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Z]] =
		second.ColumnResult[F, S, E]

	override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
	                   (expr :ConvertibleColumn[F, S, X, E]) :ColumnResult[F, S, E[Z]] =
		second.column[F, S, E](first(expr))
}




/** A conversion of any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, X]` to
  * an `SQLExpression[F, S, Y]` which does not change the generated SQL.
  * If two types `X, Y` can be converted througn implicit conversions to a common type `Z`,
  * then an implicit [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]]`[X, Y]` exists,
  * witnessing that two expressions of these types are comparable in SQL.
  * As an `SQLConversion` does not influence the generated SQL itself, they may be freely composed with each other:
  * `expr.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.to to]]`[X].to[Y]` will normally
  * produce a flattened expression, in which `expr` is wrapped in a single
  * [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]]. The framework is free to assume that,
  * in contexts where converting of an actual ''value'' of `X` is not necessary, a conversion exists solely
  * to satisfy stricter Scala's type system, and may perform some optimizations.
  * This is not true for its supertypes, where an instance of (a subclass of)
  * [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]] is never elided.
  * @see [[net.noresttherein.oldsql.sql.mechanics.Interoperable]]
  */
@implicitNotFound("Type ${X} cannot be used in place of type ${Y} in SQL. Missing implicit SQLConversion[${X}, ${Y}].")
trait SQLConversion[X, Y] extends SQLAdaptation[X, Y] {
	override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = SQLExpression[F, S, Y]
	override type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = E
	override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Y]] = E
	override type QueryResult[P, Q <: Query[P, Y]] = Q

	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	                  (expr :ConvertingTemplate[F, S, X, E]) :E[Y] =
		if (expr == null) null else expr.`->convert`(this)

//	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//	                  (expr :ConvertibleSQL[F, S, X, E]) :E[Y] =
//		if (expr == null) null else expr.`->convert`(this)
//
	override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
	                   (expr :ConvertibleColumn[F, S, X, E]) :E[Y] =
		apply(expr :ConvertibleSQL[F, S, X, E])

	override def apply[F <: RowProduct](query :QuerySQL[F, X]) :QuerySQL[F, Y] = query.rowsTo(this)
	override def apply[P](query :Query[P, X]) :Query[P, Y] = query.rowsTo(this)
	override def apply(query :TopSelectSQL[X]) :TopSelectSQL[Y] = query.rowsTo(this)
	def apply[Q[_]](query :Q[X] with QueryTemplate[X, Q]) :Q[Y] = query.rowsTo(this)

	override def compose[W](first :SQLConversion[W, X]) :SQLConversion[W, Y] =
		if (first.isIdentity) first andThen this
		else if (first == swap) SQLConversion.toSelf.castParams[W, Y]
		else new ComposedConversion[W, X, Y](first, this)

	override def andThen[Z](second :SQLTransformation[Y, Z])
			:SQLTransformation[X, Z] {
				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = second.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
					second.SQLResult[F, S, E]
			} =
		second compose this

	override def andThen[Z](second :SQLAdaptation[Y, Z])
			:SQLAdaptation[X, Z] {
				type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = second.BaseResult[F, S]
				type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
					second.SQLResult[F, S, E]
				type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, E <: ColumnSQL[F, S, Z]] =
					second.ColumnResult[F, S, E]
			} =
		second compose this

	def andThen[Z](second :SQLConversion[Y, Z]) :SQLConversion[X, Z] = second compose this

	protected[mechanics] override def split :Opt[(SQLConversion[X, A], SQLConversion[A, Y]) forSome { type A }] = Lack

	protected override def swap(namePrefix :String, nameSuffix :String) :ReversibleConversion[Y, X] =
		new InverseConversion[Y, X](this) {
			override def applyString(arg :String) = namePrefix + arg + nameSuffix
		}
	override def swap :SQLConversion[Y, X] = new InverseConversion[Y, X](this)
//
//	override def generic[F <: RowProduct, S >: Grouped <: Single] :SpecificConversion[X, Y, SQLExpression.from[F]#rows[S]#E] =
//		specific[F, S, SQLExpression.from[F]#rows[S]#E]
//
//	override def specific[F <: RowProduct, S >: Grouped <: Single,
//	                      E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//			:SpecificConversion[X, Y, E] =
//		SpecificConversion[F, S, X, Y, E](this)
//
//	override def specific[F <: RowProduct, S >: Grouped <: Single,
//	                      E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//	                     (e :ConvertingTemplate[F, S, X, E, E[X]]) :SpecificConversion[X, Y, E] =
//		specific[F, S, E]


	/** Combines this instance with another `SQLConversion` to the same type into an evidence value required
	  * by many methods of `SQLExpression`.
	  */
	def vs[A](other :SQLConversion[A, Y]) :Interoperable[X, A]#As[Y] =
		if (other.isIdentity)
			(other vs this).swap
		else
			new StandardInteroperable(this, other) { outer =>
				override lazy val swap = new StandardInteroperable(right, left) {
					override val swap = outer
				}
			}

	/** Creates an evidence value that `X` and `Y` are interoperable in SQL. */
	lazy val asLeft  :Interoperable[X, Y]#As[Y] =
		new StandardInteroperable(this, toSelf[Y]) {
			override lazy val swap = asRight
		}
	/** Creates an evidence value that `Y` and `X` are interoperable in SQL. */
	lazy val asRight :Interoperable[Y, X]#As[Y] =
		new StandardInteroperable(toSelf[Y], this) {
			override lazy val swap = asLeft
		}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLConversion[_, _]]
}




private[mechanics] sealed abstract class LiftedConversions {
	implicit def chain[XI <: Chain, XL, YI <: Chain, YL](implicit init :SQLConversion[XI, YI], last :SQLConversion[XL, YL])
			:SQLConversion[XI ~ XL, YI ~ YL] =
		new ConvertChain(init, last)

	implicit def record[XI <: Listing, XL, YI <: Listing, YL, K <: Label]
	                   (implicit init :SQLConversion[XI, YI], last :SQLConversion[XL, YL])
			:SQLConversion[XI |~ (K :~ XL), YI |~ (K :~ YL)] =
		new ConvertRecord[XI, XL, YI, YL, K](init, last)

	implicit def convertRows[X, Y](implicit item :SQLConversion[X, Y]) :SQLConversion[Rows[X], Rows[Y]] =
		new ConvertRows[X, Y](item)

	implicit def convertSeq[X, Y](implicit item :SQLConversion[X, Y]) :SQLConversion[Seq[X], Seq[Y]] =
		new ConvertSeq[X, Y](item)

	implicit def reversibleRows[X, Y](implicit item :ReversibleConversion[X, Y]) :ReversibleConversion[Rows[X], Rows[Y]] =
		new ConvertRows[X, Y](item) with ReversibleConversion[Rows[X], Rows[Y]]

	implicit def reversibleSeq[X, Y](implicit item :ReversibleConversion[X, Y])  :ReversibleConversion[Seq[X], Seq[Y]] =
		new ConvertSeq[X, Y](item) with ReversibleConversion[Seq[X], Seq[Y]]
}

private[mechanics] sealed abstract class RecordConversions extends LiftedConversions {
	implicit def recordToTuple[LI <: Listing, CI <: Chain, K <: Label, L]
	                          (implicit init :Equivalent[LI, CI]) :Equivalent[LI |~ (K :~ L), CI ~ L] =
		new ListingToChain(init)

	implicit def reorder[X <: Listing, XKs <: Chain, Y <: Listing, YKs <: Chain]
	                    (implicit there :(Y RecordProjection XKs) { type Out = X },
	                     back :(X RecordProjection YKs) { type Out = Y })
			:ReorderRecord[X, Y] { type XKeys = XKs; type YKeys = YKs } =
		ReorderRecord(there, back)
}

private[mechanics] sealed abstract class EqualTypeConversions extends RecordConversions {
	implicit def equal[X, Y](implicit ev :X =:= Y) :Equivalent[X, Y] = SQLConversion.toSelf.castParams[X, Y]
//
//	implicit def identity[X] :Equivalent[X, X] = SQLConversion.toSelf
}

private[mechanics] sealed abstract class ConversionsToDouble extends EqualTypeConversions {
	implicit object ByteToDouble extends ReversibleConversion[Byte, Double] {
		override def apply(value :Byte) :Double = value
		override def inverse(value :Double) :Byte = value.toByte
		override val swap :ReversibleConversion[Double, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Byte.toDouble"
	}
	implicit object ShortToDouble extends ReversibleConversion[Short, Double] {
		override def apply(value :Short) :Double = value
		override def inverse(value :Double) :Short = value.toShort
		override val swap :ReversibleConversion[Double, Short] = swap("", ".toShort")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Short.toDouble"
	}
	implicit object CharToDouble extends ReversibleConversion[Char, Double] {
		override def apply(value :Char) :Double = value
		override def inverse(value :Double) :Char = value.toChar
		override val swap :ReversibleConversion[Double, Char] = swap("", ".toChar")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Char.toDouble"
	}
	implicit object IntToDouble extends ReversibleConversion[Int, Double] {
		override def apply(value :Int) :Double = value
		override def inverse(value :Double) :Int = value.toInt
		override val swap :ReversibleConversion[Double, Int] = swap("", ".toInt")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Int.toDouble"
	}
	implicit object LongToDouble extends ReversibleConversion[Long, Double] {
		override def apply(value :Long) :Double = value
		override def inverse(value :Double) :Long = value.toInt
		override val swap :ReversibleConversion[Double, Long] = swap("", ".toLong")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Long.toDouble"
	}
	implicit object FloatToDouble extends ReversibleConversion[Float, Double] {
		override def apply(value :Float) :Double = value
		override def inverse(value :Double) :Float = value.toFloat
		override val swap :ReversibleConversion[Double, Float] = swap("", ".toFloat")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Float.toDouble"
	}
//	implicit val DoubleToDouble :Equivalent[Double, Double] = SQLConversion.toSelf[Double]
}

private[mechanics] sealed abstract class ConversionsToFloat extends ConversionsToDouble {
	implicit object ByteToFloat extends ReversibleConversion[Byte, Float] {
		override def apply(value :Byte) :Float = value
		override def inverse(value :Float) :Byte = value.toByte
		override val swap :ReversibleConversion[Float, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Byte.toFloat"
	}
	implicit object ShortToFloat extends ReversibleConversion[Short, Float] {
		override def apply(value :Short) :Float = value
		override def inverse(value :Float) :Short = value.toShort
		override val swap :ReversibleConversion[Float, Short] = swap("", ".toShort")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Short.toFloat"
	}
	implicit object CharToFloat extends ReversibleConversion[Char, Float] {
		override def apply(value :Char) :Float = value
		override def inverse(value :Float) :Char = value.toChar
		override val swap :ReversibleConversion[Float, Char] = swap("", ".toChar")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Char.toFloat"
	}
	implicit object IntToFloat extends ReversibleConversion[Int, Float] {
		override def apply(value :Int) :Float = value
		override def inverse(value :Float) :Int = value.toInt
		override val swap :ReversibleConversion[Float, Int] = swap("", ".toInt")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Int.toFloat"
	}
	implicit object LongToFloat extends ReversibleConversion[Long, Float] {
		override def apply(value :Long) :Float = value
		override def inverse(value :Float) :Long = value.toLong
		override val swap :ReversibleConversion[Float, Long] = swap("", ".toLong")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Long.toFloat"
	}
//	implicit val FloatToFloat :Equivalent[Float, Float] = SQLConversion.toSelf[Float]
}

private[mechanics] sealed abstract class ConversionsToLong extends ConversionsToFloat {
	implicit object ByteToLong extends ReversibleConversion[Byte, Long] {
		override def apply(value :Byte) :Long = value
		override def inverse(value :Long) :Byte = value.toByte
		override val swap :ReversibleConversion[Long, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toLong"
		override def toString = "Byte.toLong"
	}
	implicit object ShortToLong extends ReversibleConversion[Short, Long] {
		override def apply(value :Short) :Long = value
		override def inverse(value :Long) :Short = value.toShort
		override val swap :ReversibleConversion[Long, Short] = swap("", ".toShort")
		override def applyString(arg :String) :String = arg + ".toLong"
		override def toString = "Short.toLong"
	}
	implicit object CharToLong extends ReversibleConversion[Char, Long] {
		override def apply(value :Char) :Long = value
		override def inverse(value :Long) :Char = value.toChar
		override val swap :ReversibleConversion[Char, Long] = swap("", ".toChar")
		override def applyString(arg :String) :String = arg + ".toLong"
		override def toString = "Char.toLong"
	}
	implicit object IntToLong extends ReversibleConversion[Int, Long] {
		override def apply(value :Int) :Long = value
		override def inverse(value :Long) :Int = value.toInt
		override val swap :ReversibleConversion[Long, Int] = swap("", ".toInt")
		override def applyString(arg :String) :String = arg + ".toLong"
		override def toString = "Int.toLong"
	}
//	implicit val LongToLong :Equivalent[Long, Long] = SQLConversion.toSelf[Long]
}

private[mechanics] sealed abstract class ConversionsToInt extends ConversionsToLong {
		implicit object ByteToInt extends ReversibleConversion[Byte, Int] {
		override def apply(value :Byte) :Int = value
		override def inverse(value :Int) :Byte = value.toByte
		override val swap :ReversibleConversion[Int, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toInt"
		override def toString = "Byte.toInt"
	}
	implicit object ShortToInt extends ReversibleConversion[Short, Int] {
		override def apply(value :Short) :Int = value
		override def inverse(value :Int) :Short = value.toShort
		override val swap :ReversibleConversion[Int, Short] = swap("", ".toShort")
		override def applyString(arg :String) :String = arg + ".toInt"
		override def toString = "Short.toInt"
	}
	implicit object CharToInt extends ReversibleConversion[Char, Int] {
		override def apply(value :Char) :Int = value
		override def inverse(value :Int) :Char = value.toChar
		override val swap :ReversibleConversion[Int, Char] = swap("", ".toChar")
		override def applyString(arg :String) :String = arg + ".toInt"
		override def toString = "Char.toInt"
	}
//	implicit val IntToInt :Equivalent[Int, Int] = SQLConversion.toSelf[Int]
}

private[mechanics] sealed abstract class ConversionsToShort extends ConversionsToInt {
	implicit object ByteToShort extends ReversibleConversion[Byte, Short] {
		override def apply(value :Byte) :Short = value
		override def inverse(value :Short) :Byte = value.toByte
		override val swap :SQLConversion[Short, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toShort"
	}

	implicit def toOption[T] :SQLConversion[T, Option[T]] = option
	protected def option[T] :SQLConversion[T, Option[T]]
}



object SQLConversion extends ConversionsToShort {

	def apply[X, Y](suffix :String, convert :X => Y) :SQLConversion[X, Y] =
		new Impl[X, Y](suffix, convert)

	def apply[X, Y](suffix :String, convert :X => Y, inversed :Y => X) :ReversibleConversion[X, Y] =
		new Impl[X, Y](suffix, convert) with ReversibleConversion[X, Y] {
			override def unapply(value :Y) = Got(inversed(value)) //Opt.guard(inversed)(value)
			override def inverse(value :Y) = inversed(value)
		}

	def opt[X, Y](suffix :String, convert :X => Y, inversed :Y => Opt[X]) :SQLConversion[X, Y] =
		new Impl[X, Y](suffix, convert) {
			override def unapply(value :Y) = inversed(value)
			override def inverse(value :Y) = unapply(value) match {
				case Got(v) => v
				case _ => throw new IllegalArgumentException("Cannot inverse map " + value + " with " + this + ".")
			}
		}

	def guard[X, Y](suffix :String, convert :X => Y, inversed :Y => X) :SQLConversion[X, Y] =
		opt(suffix, convert, Opt.guard(inversed))

	private class Impl[X, Y](suffix :String, convert :X => Y) extends SQLConversion[X, Y] {
		override def apply(value :X) = convert(value)
		override def inverse(value :Y) :X =
			throw new UnsupportedOperationException("Conversion " + this + " is not reversible.")
		override def unapply(value :Y) :Opt[X] = Lack
		override def applyString(arg :String) = arg + suffix
	}



	object Composition {
		def unapply[X, Z](e :SQLConversion[X, Z])
				:Opt[(SQLConversion[X, Y], SQLConversion[Y, Z]) forSome { type Y }] =
			e.split
	}
	object ReadForm {
		def unapply[X](form :SQLReadForm[X]) :Opt[SQLConversion[_, _ <: X]] = form match {
			case FormConversion.ReadForm(conversion :SQLConversion[_, X] @unchecked) => Got(conversion)
			case _ => Lack
		}
	}
	object Form {
		def unapply[X](form :SQLForm[X]) :Opt[SQLConversion[_, X]] = form match {
			case FormConversion.Form(conversion :SQLConversion[_, X] @unchecked) => Got(conversion)
			case _ => Lack
		}
	}

	implicit def identity[X]   :Equivalent[X, X] = IdentityConversion.asInstanceOf[Equivalent[X, X]]
	implicit def toParam[T]    :Equivalent[Out[T], T] = toOut[T].swap
	implicit def selectRow[T]  :Equivalent[Rows[T], T] = SelectRow.castParams[Rows[T], T] //asInstanceOf[Equiv[Rows[T], T]]
	implicit def selectRows[T] :Equivalent[Rows[T], Seq[T]] = SelectRows.castParams[Rows[T], Seq[T]] //asInstanceOf[Equiv[Rows[T], Seq[T]]]
	implicit def entryValue[K <: Label, V] :Equivalent[K :~ V, V] = ConvertLabel.castParams[K :~ V, V] //asInstanceOf[Equiv[K :~ V, V]]
	private type Equiv[X, Y] = Equivalent[X, Y]

	//inverses can't be implicits or we'd have an implicit conflict
	def toSelf[T] :Equivalent[T, T] = IdentityConversion.castParams[T, T]
	def label[K <: Label, V] :Equivalent[V, K :~ V] = entryValue[K, V].swap
	def toOut[T] :Equivalent[T, Out[T]] = OutParam.castParams[T, Out[T]] //it's better to unpack by default
	def forceListing[X <: Chain, Y <: Listing](implicit listingToChain :Equivalent[Y, X]) :Equivalent[X, Y] =
		listingToChain.swap

	def toRow[T] :SQLTransformation[T, Rows[T]]       = selectRow[T].swap //unlikely to ever see use, but we won't be able
	def toRows[T] :SQLTransformation[Seq[T], Rows[T]] = selectRows[T].swap //  to handle custom implementations when needed

	/** An unsafe [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]] for representing
	  * an SQL expression as one for the supertype of its value type parameter. It is inherently irreversible,
	  * meaning it can't be used to unify two SQL expressions for use within a comparison or assignment.
	  * It is reserved for use cases where the adapted expression is guaranteed to be used only in a covariant manner.
	  */
	def supertype[X, Y](implicit tag :ClassTag[X], subtype :X <:< Y) :SQLConversion[X, Y] =
		new Upcast[X, Y](tag, subtype)

	def subtype[X, Y](implicit tag :ClassTag[Y], subtype :Y <:< X) :SQLConversion[X, Y] =
		supertype[X, Y].swap
//	implicit val ShortToShort :Equivalent[Short, Short] = toSelf[Short]

	type from[X] = { type to[Y] = SQLConversion[X, Y] }
	type to[Y] = { type from[X] = SQLConversion[X, Y] }

	case class Upcast[X, Y](X :ClassTag[X], upcast :X <:< Y) extends SQLConversion[X, Y] {
		override def isUpcast = true
		override def apply(value :X) = upcast(value)
		override def inverse(value :Y) = X.unapply(value) match {
			case Some(x) => x
			case _       =>
				throw new IllegalArgumentException(
					"Cannot downcast " + value + ": " + value.className + " to " + X.runtimeClass.name + "."
				)
		}
		override def unapply(value :Y) = X.unapply(value)

		override def apply(form :SQLReadForm[X]) :SQLReadForm[Y] = upcast.substituteCo(form)
		override def apply(form :ColumnReadForm[X]) :ColumnReadForm[Y] = upcast.substituteCo(form)

		override def compose[W](first :SQLConversion[W, X]) :SQLConversion[W, Y] = first match {
			case _ :Upcast[_, _] => first.castParam2[Y]
			case _ => super.compose(first)
		}

		override def applyString(arg :String) = arg + X.runtimeClass.localName + ".super"
	}

	case class ConvertRows[X, Y](item :SQLConversion[X, Y]) extends SQLConversion[Rows[X], Rows[Y]] {
		override def apply(value :Rows[X]) :Rows[Y] = value.map(item.apply)
		override def inverse(value :Rows[Y]) :Rows[X] = value.map(item.inverse)

		override def unapply(value :Rows[Y]) :Opt[Rows[X]] =
			(Opt(Seq.newBuilder[X]) /: value.seq) {
				(build, y) => item.unapply(y).flatMap { x => if (build.nonEmpty) build.get += x; build }
			}.map(b => Rows(b.result():_*))

		override def isReversible :Boolean = item.isReversible
		override def isLossless   :Boolean = item.isLossless

		override lazy val swap :SQLConversion[Rows[Y], Rows[X]] =
			new ConvertRows(item.swap) with ReversibleConversion[Rows[Y], Rows[X]] {
				override lazy val swap = ConvertRows.this
			}
//		override def equals(that :Any) :Boolean = that match {
//			case other :ConvertRows[_, _] => item == other.item
//			case _ => false
//		}
//		override def hashCode = item.hashCode
		override def applyString(arg :String) :String = item.applyString(arg + ".map(_") + ")"
	}
//
//	object ConvertRows {
//		def unapply[X, Y](conversion :SQLTransformation[X, Y])
//				:Opt[(SQLConversion[A, B], X =:= Rows[A], Y =:= Rows[B]) forSome { type A; type B }] =
//			conversion match {
//				case rows :ConvertRows[a, b] =>
//					val ev = implicitly[X =:= X]
//					Got((rows.item, ev.asInstanceOf[X=:=Rows[a]], ev.asInstanceOf[Y=:=Rows[b]]))
//				case _ => Lack
//			}
//	}


	case class ConvertSeq[X, Y](item :SQLConversion[X, Y]) extends SQLConversion[Seq[X], Seq[Y]] {
		override def apply(value :Seq[X]) :Seq[Y] = value.map(item.apply)
		override def inverse(value :Seq[Y]) :Seq[X] = value.map(item.inverse)
		override def unapply(value :Seq[Y]) :Opt[Seq[X]] =
			(Opt(value.iterableFactory.newBuilder[X]) /: value) {
				(acc, y) => item.unapply(y).flatMap { x => acc foreach { _ += x }; acc }
			}.map(_.result())

		override def isReversible :Boolean = item.isReversible
		override def isLossless   :Boolean = item.isLossless

		override lazy val swap :SQLConversion[Seq[Y], Seq[X]] =
			new ConvertSeq(item.swap) with ReversibleConversion[Seq[Y], Seq[X]] {
				override lazy val swap = ConvertSeq.this
			}

//		override def equals(that :Any) :Boolean = that match {
//			case other :ConvertSeq[_, _] => item == other.item
//			case _ => false
//		}
//		override def hashCode :Int = item.hashCode
		override def applyString(arg :String) :String = item.applyString(arg + ".map{_") + "}"
	}
//
//	object ConvertSeq {
//		def unapply[X, Y](conversion :SQLTransformation[X, Y])
//				:Opt[(SQLConversion[A, B], X =:= Seq[A], Y =:= Seq[B]) forSome { type A; type B }] =
//			conversion match {
//				case seq :ConvertSeq[a, b] =>
//					val ev = implicitly[X =:= X]
//					Got((seq.item, ev.asInstanceOf[X =:= Seq[a]], ev.asInstanceOf[Y =:= Seq[b]]))
//				case _ => Lack
//			}
//	}


	//todo: make this one and other case classes private, with unapply instead
	case class ConvertChain[XI <: Chain, XL, YI <: Chain, YL](init :SQLConversion[XI, YI], last :SQLConversion[XL, YL])
		extends SQLConversion[XI ~ XL, YI ~ YL]
	{
		override def apply(value :XI ~ XL) :YI ~ YL = init(value.init) ~ last(value.last)
		override def inverse(value :YI ~ YL) :XI ~ XL = init.inverse(value.init) ~ last.inverse(value.last)
		override def unapply(value :YI ~ YL) :Opt[XI ~ XL] =
			for (i <- init.unapply(value.init); l <- last.unapply(value.last)) yield i ~ l

//		override def isDerived :Boolean = false
		override def isReversible :Boolean = last.isReversible && init.isReversible
		override def isLossless   :Boolean = last.isLossless && init.isLossless
		override lazy val swap :SQLConversion[YI ~ YL, XI ~ XL] =
			new ConvertChain[YI, YL, XI, XL](init.swap, last.swap) with ReversibleConversion[YI ~ YL, XI ~ XL] {
				override lazy val swap = ConvertChain.this
			}
		def deep :SQLTransformation[XI ~ XL, YI ~ YL] = TransformChain(
			init match {
				case chain :ConvertChain[_, _, _, _] => chain.deep.castParams[XI, YI]
				case _ => init
			}, last
		)
//		override def equals(that :Any) :Boolean = that match {
//			case self :AnyRef if this eq self => true
//			case other :ConvertChain[_, _, _, _] if other canEqual this => last == other.last && init == other.init
//			case _ => false
//		}
//		override def canEqual(that :Any) = that.isInstanceOf[ConvertChain[_, _, _, _]]
//		override def hashCode :Int = init.hashCode * 31 + last.hashCode
		override def applyString(arg :String) :String = init.applyString(arg) + "~(" + last + ")"
	}
//
//	object ConvertChain {
//		def apply[XI <: Chain, XL, YI <: Chain, YL]
//		         (init :SQLConversion[XI, YI], last :SQLConversion[XL, YL]) :SQLConversion[XI ~ XL, YI ~ YL] =
//			new ConvertChain(init, last)
//
//		object Split {
//			def unapply[XI <: Chain, XL, Y](conversion :SQLTransformation[XI ~ XL, Y])
//					:Opt[(SQLConversion[XI, YI], SQLConversion[XL, YL],
//					      SQLAdaptation[YI ~ YL, Y]#As[conversion.SQLResult]) forSome { type YI <: Chain; type YL }
//					] =
//				conversion match {
//					case chain :ConvertChain[XI, XL, yi, yl] =>
//						Got(chain.init, chain.last, toSelf.asInstanceOf[SQLAdaptation[yi ~ yl, Y]#As[conversion.SQLResult]])
//					case _ => Lack
//				}
//
//		}
//	}


	/** An element-by-element conversion of a `Listing`, applying separate `SQLConversion` instances to each value. */
	case class ConvertRecord[XI <: Listing, XL, YI <: Listing, YL, K <: Label]
	                         (init :SQLConversion[XI, YI], last :SQLConversion[XL, YL])
		extends SQLConversion[XI |~ (K :~ XL), YI |~ (K :~ YL)]
	{
		override def apply(value :XI |~ (K :~ XL)) :YI |~ (K :~ YL) =
			init(value.init) |~ :~[K](last(value.last.value))

		override def inverse(value :YI |~ (K :~ YL)) :XI |~ (K :~ XL) =
			init.inverse(value.init) |~ :~[K](last.inverse(value.last.value))

		override def unapply(value :YI |~ (K :~ YL)) :Opt[XI |~ (K :~ XL)] =
			for (i <- init.unapply(value.init); l <- last.unapply(value.last.value)) yield i |~ :~[K](l)

		override def isReversible :Boolean = last.isReversible && init.isReversible
		override def isLossless   :Boolean = last.isLossless && init.isLossless

		override lazy val swap :SQLConversion[YI |~ (K :~ YL), XI |~ (K :~ XL)] =
			new ConvertRecord[YI, YL, XI, XL, K](init.swap, last.swap)
				with ReversibleConversion[YI |~ (K :~ YL), XI |~ (K :~ XL)]
			{
				override lazy val swap = ConvertRecord.this
			}

		override def applyString(arg :String) :String = {
			@tailrec def rec(conv :SQLConversion[_, _]) :String = conv match {
				case listing :ConvertRecord[_, _, _, _, _] => rec(listing.init) + "|~" + listing.last
				case _ if conv == toSelf => arg + ".map(@~"
				case _ => conv.applyString(arg) + ".mapSuffix("
			}
			rec(this) + ")"
		}
	}

/*      //The problem with creating a proper conversion to LabeledSQL is that we can't really tell if a value is a column,
	//  as otherwise (if it isn't a tuple either), no LabeledSQL is possible.
	case class DeepChainToListing[CI <: Chain, LI <: Listing, K <: Label, L] private[Lift] (init :Lift[CI, LI], key :K)
		extends ChainToListing[CI, LI, K, L](init)
	{
		//			override def apply(form :SQLReadForm[CI ~ L]) :SQLReadForm[LI |~ (K :~ L)] =
		//				form.asInstanceOf[SQLReadForm[LI |~ (K :~ L)]]
		override def apply[F <: RowProduct, S >: Grouped <: Single]
		                  (expr :SQLExpression[F, S, CI ~ L]) :SQLExpression[F, S, LI |~ (K :~ L)] =
			expr match {
				case ChainSQL(init, last) => this.init(init) match {
					case EmptyChain => EmptyListing |~
					case listing :LabeledSQL[F, S, LI] =>
				}
			}

		override def isStandard = false

		override def applyString(arg :String) :String = init.applyString(arg) + "|~" + key + ":~_"
	}
*/


	sealed abstract class ReorderRecord[X <: Listing, Y <: Listing]
		extends Equivalent[X, Y] with RecordTransformation[X, Y]
	{ outer =>
		override type Out = Y
		override type OutSQL[-F <: RowProduct, -S >: Grouped <: Single] = LabeledSQL[F, S, Y]
		type XKeys <: Chain
		type YKeys <: Chain
		val there :RecordProjection[X, YKeys] { type Out = Y }
		val back  :RecordProjection[Y, XKeys] { type Out = X }

		override def apply(value :X) :Y = there(value)
		override def inverse(value :Y) :X = back(value)

		override def apply[F <: RowProduct, S >: Grouped <: Single]
		                  (record :LabeledValueSQL[F, S, X]) :LabeledSQL[F, S, Y] =
			there(record)

		override lazy val swap :ReorderRecord[Y, X] { type XKeys = outer.YKeys; type YKeys = outer.XKeys } =
			new ReorderRecord[Y, X] {
				override type XKeys = outer.YKeys
				override type YKeys = outer.XKeys
				override val there  = outer.back
				override val back   = outer.there
				override lazy val swap = outer
			}

		override def andThen[Z](second :SQLConversion[Y, Z]) :SQLConversion[X, Z] = second match {
			case other :ReorderRecord[Y @unchecked, Listing @unchecked] =>
				ReorderRecord(there andThen other.there, other.back andThen back).asInstanceOf[SQLConversion[X, Z]]
			case _ => super.andThen(second)
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :ReorderRecord[_, _] =>
				there.toString == other.there.toString && back.toString == other.back.toString
			case _ => false
		}
		override def hashCode :Int = there.toString.hashCode * 31 + back.toString.hashCode

		override def applyString(arg :String) :String = arg + ".reorder(" + there + ")"
	}

	object ReorderRecord {
		def apply[X <: Listing, XKs <: Chain, Y <: Listing, YKs <: Chain]
		         (map :RecordProjection[X, YKs] { type Out = Y }, unmap :RecordProjection[Y, XKs] { type Out = X })
				:ReorderRecord[X, Y] { type XKeys = XKs; type YKeys = YKs } =
			new ReorderRecord[X, Y] {
				override type XKeys = XKs
				override type YKeys = YKs
				override val there = map
				override val back  = unmap
			}

		def unapply[X <: Listing, Y <: Listing](conversion :SQLConversion[X, Y])
				:Opt[(RecordProjection[X, _] { type Out = Y }, RecordProjection[Y, _] { type Out = X })] =
			conversion match {
				case reorder :ReorderRecord[X, Y] => Got((reorder.there, reorder.back))
				case _ => Lack
			}
	}
//
//	sealed class ReorderRecord[X <: Listing, Y <: Listing](val there :Y SublistingOf X, val back :X SublistingOf Y)
//		extends Equivalent[X, Y]
//	{
//		override def apply(value :X) :Y = there(value)
//		override def inverse(value :Y) :X = back(value)
//
//		override lazy val swap :ReorderRecord[Y, X] =
//			new ReorderRecord(back, there) with ReversibleConversion[Y, X] {
//				override lazy val swap = ReorderRecord.this
//			}
//
//		override def andThen[Z](second :SQLConversion[Y, Z]) :SQLConversion[X, Z] = second match {
//			case other :ReorderRecord[Y @unchecked, Listing @unchecked] =>
//				new ReorderRecord(other.there compose there, back compose other.back).asInstanceOf[SQLConversion[X, Z]]
//			case _ => super.andThen(second)
//		}
//		override def equals(that :Any) :Boolean = that match {
//			case self  :AnyRef if this eq self => true
//			case other :ReorderRecord[_, _] =>
//				there.toString == other.there.toString && back.toString == other.back.toString
//			case _ => false
//		}
//		override def hashCode :Int = there.toString.hashCode * 31 + back.toString.hashCode
//		override def applyString(arg :String) :String = arg + ".reorder(" + there + ")"
//	}


	private[sql] class ChainToListing[CI <: Chain, LI <: Listing, K <: Label, L](init :Equivalent[CI, LI])
		extends Equivalent[CI ~ L, LI |~ (K :~ L)]
	{
		override def apply(value :CI ~ L) :LI |~ (K :~ L) = init(value.init) |~ :~[K](value.last)
		override def inverse(value :LI |~ (K :~ L)) :CI ~ L = value.asInstanceOf[CI ~ L] //works because :~ is a value type

		override def isReversible :Boolean = init.isReversible
		override def isLossless   :Boolean = init.isLossless

		override lazy val swap :Equivalent[LI |~ (K :~ L), CI ~ L] =
			new ListingToChain[LI, CI, K, L](init.swap) {
				override lazy val swap = ChainToListing.this
			}
		override def applyString(arg :String) :String = arg + ".toListing"
	}

	private[sql] class ListingToChain[LI <: Listing, CI <: Chain, K <: Label, L](init :Equivalent[LI, CI])
		extends Equivalent[LI |~ (K :~ L), CI ~ L]
	{
		override def apply(value :LI |~ (K :~ L)) :CI ~ L =
			if (init == toSelf || init.isInstanceOf[ListingToChain[_, _, _, _]] || init.isInstanceOf[Upcast[_, _]])
				value.asInstanceOf[CI ~ L] //works because :~ is a value type
			else
				init(value.init) ~ value.last.value

		override def inverse(value :CI ~ L) :LI |~ (K :~ L) = init.inverse(value.init) |~ :~[K](value.last)
		override lazy val swap :Equivalent[CI ~ L, LI |~ (K :~ L)] =
			new ChainToListing[CI, LI, K, L](init.swap) {
				override lazy val swap = ListingToChain.this
			}
		override def applyString(arg :String) :String = arg + ".toChain"
	}


	protected override def option[X] :SQLConversion[X, Option[X]] =
		ToOption.asInstanceOf[SQLConversion[X, Option[X]]]

	private[this] object ToOption extends SQLConversion[Any, Option[Any]] {
		override def apply(value: Any): Option[Any] = Option(value)
		//should inverse return value.orNull? this will throw an exception for value types.
		override def unapply(value: Option[Any]) : Opt[Any] = value
		override def inverse(value :Option[Any]) :Any = value.get

		override def apply(form :SQLReadForm[Any]) :SQLReadForm[Option[Any]] = form.toOpt
		override def apply(form :ColumnReadForm[Any]) :ColumnReadForm[Option[Any]] = form.toOpt
		override def apply(form :SQLForm[Any]) :SQLForm[Option[Any]] = form.toOpt
		override def apply(form :ColumnForm[Any]) :ColumnForm[Option[Any]] = form.toOpt

//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr: ConvertibleSQL[F, S, Any, E]): E[Option[Any]] =
//			if (expr == null) null else expr.opt
		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr: ConvertingTemplate[F, S, Any, E]): E[Option[Any]] =
			if (expr == null) null else expr.opt

		override lazy val swap = swap("", ".get")
/*
		override object inverse extends Lift[Option[Any], Any] {
			private[this] val SomeNone = Some(None)

			override def apply(value :Option[Any]) :Any = value.orNull
			override def lower(value :Any) :Option[Any] = Option(value)
			override def unapply(value :Any) :Option[Option[Any]] =
				if (value == null) SomeNone else Some(Some(value))

			override def apply(form :SQLReadForm[Option[Any]]) :SQLReadForm[Any] =
				SQLReadForm.optMap(form.toString + ".get")(Predef.identity[Option[Any]])(form, Maybe.none)

			override def apply(form :ColumnReadForm[Option[Any]]) :ColumnReadForm[Any] =
				ColumnReadForm.optMap(form.toString + ".get")(Predef.identity[Option[Any]])(form, Maybe.none)

			override def apply(form :SQLForm[Option[Any]]) :SQLForm[Any] =
				SQLForm.optMap(form.toString + ".get")(Predef.identity[Option[Any]])(unapply)(form, Maybe.none)

			override def apply(form :ColumnForm[Option[Any]]) :ColumnForm[Any] =
				ColumnForm.optMap(form.toString + ".get")(Predef.identity[Option[Any]])(unapply)(form, Maybe.none)

			override def inverse :Lift[Any, Option[Any]] = OptionLift.this
			override def applyString(arg :String) :String = arg + ".orNull"
		}
*/

		override def applyString(arg :String) :String = arg + ".toOption"
	}


	private[this] object ConvertLabel extends Equivalent[Label :~ Any, Any] {
		override def apply(value :Label :~ Any) :Any = value.value
		override def inverse(value :Any) :Label :~ Any = :~[Label](value)
		override val swap = swap("", ".:~")
		override def applyString(arg :String) :String = arg + ".value"
	}

	private[this] object SelectRow extends Equivalent[Rows[Any], Any] {
		override def apply(value: Rows[Any]): Any = value.head
		override def inverse(value: Any): Rows[Any] = Rows(value)
		override def applyString(arg :String) = arg + ".head"
		override val swap = swap("", ".toRow")
	}

	private[this] object SelectRows extends Equivalent[Rows[Any], Seq[Any]] {
		override def apply(value: Rows[Any]): Seq[Any] = value.seq
		override def inverse(value :Seq[Any]) :Rows[Any] = Rows(value :_*)
		override val swap = swap("", ".toRows")
		override def applyString(arg :String) = arg + ".toSeq"
	}

	private[this] object OutParam extends Equivalent[Any, Out[Any]] {
		override def apply(value: Any): Out[Any] = Out(value)
		override def inverse(value :Out[Any]) = value.param
		override val swap = swap("", ".param")
		override def applyString(arg :String) = "" + arg + ".toOut"
	}

	private[this] object IdentityConversion extends Equivalent[Any, Any] with SQLDecoration[Any] {
		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingTemplate[F, S, Any, E]) =
			denullify(expr).toConvertibleSQL
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, Any, E]) = expr

		override def apply[F <: RowProduct](query :QuerySQL[F, Any]) = query
		override def apply[P](query :Query[P, Any]) = query
		override def apply(query :TopSelectSQL[Any]) = query
		override def apply[Q[_]](query :Q[Any] with QueryTemplate[Any, Q]) = query

		override def compose[W](first :SQLTransformation[W, Any]) :first.type = first
		override def compose[W](first :SQLAdaptation[W, Any]) :first.type = first
		override def compose[W](first :SQLConversion[W, Any]) :first.type = first
		override def compose[W](first :ReversibleConversion[W, Any]) :first.type = first
		override def andThen[Z](second :SQLTransformation[Any, Z]) :second.type = second
		override def andThen[Z](second :SQLAdaptation[Any, Z]) :second.type = second
		override def andThen[Z](second :SQLConversion[Any, Z]) :second.type = second
		override def andThen[Z](second :ReversibleConversion[Any, Z]) :second.type = second

		override val swap :Equivalent[Any, Any] = this

		override def vs[A](other :SQLConversion[A, Any]) :Interoperable[Any, A]#As[Any] = other.asRight
		override def =~=[A](other :ReversibleConversion[A, Any]) :(Any =~= A)#Widen[Any] = other.asRight
		override lazy val asLeft  = Interoperable.identity
		override lazy val asRight = Interoperable.identity

		override def isIdentity = true

//		override def specific[F <: RowProduct, S >: Grouped <: Single,
//		                      E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]] =
//			SpecificConversion.identity

		override def applyString(arg :String) = arg
	}

}



private class ComposedConversion[X, Y, Z](override val first  :SQLConversion[X, Y],
                                          override val second :SQLConversion[Y, Z])
	extends ComposedAdaptation[X, Y, Z] with SQLConversion[X, Z]
{
	protected override def split :Opt[(SQLConversion[X, Y], SQLConversion[Y, Z])] = Got(first, second)

	override lazy val swap :ComposedConversion[Z, Y, X] =
		new ComposedConversion(second.swap, first.swap) with ReversibleConversion[Z, X]
	{
		override lazy val swap = ComposedConversion.this
		override def applyString(arg :String) = arg + ".inverse(" + swap + ")"
	}
}

private class InverseConversion[X, Y](override val swap :SQLConversion[Y, X])
	extends InverseTransformation[X, Y] with ReversibleConversion[X, Y]





/** A conversion of type `X` to `Y` which is, possibly with some loss of precision, reversible.
  * It witnesses that the difference between `X` and `Y` exists only in the Scala application,
  * but any two [[net.noresttherein.oldsql.sql.SQLExpression SQL expressions]] of these types can be substituted
  * in the generated SQL because the distinction does not exist in the DBMS.
  * Similarly to `SQLConversion`, two instances `ReversibleConversion[X, Z]` and `ReversibleConversion[Y, Z]`
  * can be combined into evidence `X `[[net.noresttherein.oldsql.sql.mechanics.=~= =~=]]` Y`, which is the most common
  * way in which this conversion is encountered.
  *
  * Conversions of this type are used to unify types of two comparable expressions, in particular when
  * [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.:= assigning]] a value to a column/component, and thus
  * [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.inverse inverse]] should not throw an exception.
  */ //consider: renaming to SQLWidening or SQLPromotion
@implicitNotFound("Type ${X} is not equivalent to ${Y} in SQL. Missing implicit ReversibleConversion[${X}, ${Y}].")
trait ReversibleConversion[X, Y] extends SQLConversion[X, Y] {
	override def unapply(value :Y) :Got[X] = Got(inverse(value))
	override def isReversible = true

	override def compose[W](first :SQLConversion[W, X]) :SQLConversion[W, Y] = first match {
		case reversible :ReversibleConversion[W, X] => compose(reversible)
		case _ => super.compose(first)
	}

	def compose[W](first :ReversibleConversion[W, X]) :ReversibleConversion[W, Y] =
		if (first.isIdentity)
			first andThen this
		else if (first == swap)
			SQLConversion.toSelf.castParams[W, Y]
		else {
			val arg = first
			new ComposedConversion[W, X, Y](first, this) with ReversibleConversion[W, Y] {
				override val first  :ReversibleConversion[W, X] = arg
				override val second :ReversibleConversion[X, Y] = ReversibleConversion.this
				override def split :Opt[(ReversibleConversion[W, X], ReversibleConversion[X, Y])] =
					Got(first, second)
			}
		}

	override def andThen[Z](conversion :ReversibleConversion[Y, Z]) :ReversibleConversion[X, Z] =
		conversion compose this

	protected override def split :Opt[(ReversibleConversion[X, A], ReversibleConversion[A, Y]) forSome { type A }] =
		Lack

	override def vs[A](other :SQLConversion[A, Y]) :Interoperable[X, A]#As[Y] =
		other match {
			case reversible :ReversibleConversion[A, Y] => this =~= reversible
			case _ => super.vs(other)
		}

	/** Combines this instance with another `ReversibleConversion` to the same type into an evidence value required
	  *  by many methods of `SQLExpression`.
	  */
	def =~=[A](other :ReversibleConversion[A, Y]) :(X =~= A)#Widen[Y] =
		if (other.isIdentity)
			(other =~= this).swap
		else
			new Equivalence(this, other) { outer =>
				override lazy val swap = new Equivalence(right, left) {
					override val swap = outer
				}
			}

	override lazy val asLeft :(X =~= Y)#Widen[Y] =
		new Equivalence(this, SQLConversion.toSelf[Y]) {
			override lazy val swap = left.asRight
		}
	override lazy val asRight :(Y =~= X)#Widen[Y] =
		new Equivalence(SQLConversion.toSelf[Y], this) {
			override lazy val swap = right.asLeft
		}
}


object ReversibleConversion {
	@inline implicit def implicitEquivalentConversion[X, Y](implicit conversion :Equivalent[X, Y])
			:ReversibleConversion[X, Y] =
		conversion

	def apply[X, Y](suffix :String, there :X => Y, back :Y => X) :ReversibleConversion[X, Y] =
		SQLConversion(suffix, there, back)
}






/** A fully [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.inverse reversible]] and lossless conversion
  * between types `X` and `Y`. It is often used for types adapting or wrapping another type.
  */
trait Equivalent[X, Y] extends ReversibleConversion[X, Y] {
	override def isLossless = true
	protected override def swap(namePrefix :String, nameSuffix :String) :Equivalent[Y, X] =
		new InverseConversion[Y, X](this) with Equivalent[Y, X] {
			override val swap = Equivalent.this
			override def applyString(arg :String) = namePrefix + arg + nameSuffix
		}
	override def swap :Equivalent[Y, X] =
		new InverseConversion[Y, X](this) with Equivalent[Y, X] {
			override val swap = Equivalent.this
		}
}




object Equivalent {
	def apply[X, Y](implicit ev :Equivalent[X, Y]) :ev.type = ev

	def apply[X, Y](suffix :String, map :X => Y, imap :Y => X) :Equivalent[X, Y] =
		new Equivalent[X, Y] {
			override def apply(value :X) = map(value)
			override def inverse(value :Y) = imap(value)
			override def applyString(arg :String) = arg + suffix
		}
}








///** Dedication of a [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation SQLTransformation]]
//  * to a specific subtype `Arg` of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, X]`,
//  * returning also a specific expression `Res`. Implicit instances exist for any types `X` and `Y` for which
//  * an implicit `SQLTransformation` exists, and an implicit instance of subclass
//  * [[net.noresttherein.oldsql.sql.mechanics.SpecificConversion SpecificConversion]] for expressions in the form of
//  * `E[v] <: SQLExpression[F, S, v] with `[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]]`[F, S, v, E, _ <: E[v]`.
//  * Additionally, any `SQLTransformation` is implicitly convertible to a `SpecificTransformation` for such types.
//  *
//  * This type exists to provide two features impossible to implement on the level of `SQLTransformation` itself:
//  * first, some specialized, largely internal, transformations applicable only to expressions
//  * of a certain type, and second, that a composition of two specific transformations `E1 => E2` and `E2 => E3`
//  * is always possible and results in a conversion `E1 => E3`, as expected. `SQLTransformation` is too generic
//  * to express such two properties. This features prominently in the process of
//  * [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.reform reforming]] expression pairs,
//  * when adapter expressions can add a transformation reapplying the adapter to the stack of conversions
//  * applied to a reformed underlying expression.
//  * @tparam X   The value type of the argument expression.
//  * @tparam Y   The value type of the transformed expression.
//  * @tparam E   A type constructor such that `Arg <: E[X]` and `E[Y] <: Res`. Irrelevant in `SpecificTransformation`
//  *             itself, it comes to play with subclass
//  *             [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]]`[X, Y, E]`, which defines
//  *             `Arg = E[X], Res = E[Y]`. Its inclusion on this level allows polymorphic behaviour between
//  *             `SpecificTransformation`and `SpecificConversion`: without it, expressing former subtyping relations
//  *             between `Arg`, `Res` and `C[X]`, `C[Y]` would be impossible (without implicit `<:<`, evidence,
//  *             which has very limited usefulness in case of higher types). For practical motivation
//  *             see [[net.noresttherein.oldsql.sql.mechanics.SpecificTransformation.result result]] method.
//  * @tparam Arg The argument expression type, `Arg <:< SQLExpression[F, S, X]`. This bound is not enforced on this level
//  *             to reduce syntax noise and allow usage with functions working on arbitrary (unbound) functors.
//  * @tparam Res The type of the transformed expression, `Res <:< SQLExpression[F, S, Y]`. This bound is not enforced
//  *             on this level to reduce syntax noise and allow usage with functions working on arbitrary functors.
//  */
//@implicitNotFound("Cannot convert an SQL expression ${Arg} <: SQLExpression[_, _, ${X}] " +
//                  "to expression ${Res} <: SQLExpression[_, _, ${Y}].")
//trait SpecificTransformation[X, Y, E[_], -Arg, +Res] extends Any with FormConversion[X, Y] {
//	def sql(expr :Arg) :Res
//	def isDerived :Boolean
//
//
//	/** If this transformation is a [[net.noresttherein.oldsql.sql.mechanics.SpecificConversion SpecificConversion]]
//	  * - that is, `Arg =:= E[X]` and `Res =:= E[Y]` - perform an identity conversion of `E[Y]` to `Res`
//	  * and return the result in an `Opt`.
//	  */
//	def result(res: => E[Y]) :Opt[Res] = Lack
//
//	/** If this transformation is a [[net.noresttherein.oldsql.sql.mechanics.SpecificConversion SpecificConversion]]
//	  * - that is, `Arg =:= E[X]` and `Res =:= E[Y]` - return `Got(res(this))`.
//	  */
//	def convert(res :SpecificConversion[X, Y, E] => E[Y]) :Opt[Res] = Lack
//
//	def compose[W, A](first :SpecificTransformation[W, X, E, A, Arg])
//			:SpecificTransformation[W, Y, E, A, Res] =
//		if (first.isIdentity) first andThen this
//		else new ComposedSpecificTransformation(first, this)
//
//	def compose[W](first :SpecificConversion[W, X, E])(implicit ev :E[X] <:< Arg)
//			:SpecificTransformation[W, Y, E, E[W], Res] =
//		compose[W, E[W]](ev.liftCo[({ type T[r] = SpecificTransformation[W, X, E, E[W], r] })#T](first))
//
//	def andThen[Z, A >: Res, R](second :SpecificTransformation[Y, Z, E, A, R])
//			:SpecificTransformation[X, Z, E, Arg, R] =
//		if (second.isIdentity) second compose this
//		else new ComposedSpecificTransformation(this, second)
//
//	def andThen[Z](second :SpecificConversion[Y, Z, E])(implicit ev :Res <:< E[Y])
//			:SpecificTransformation[X, Z, E, Arg, E[Z]] =
//		andThen[Z, Res, E[Z]](ev.liftContra[({ type T[a] = SpecificTransformation[Y, Z, E, a, E[Z]] })#T](second))
//
//	override def applyString(arg :String) :String
//}
//
//
//
//object SpecificTransformation {
//	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y,
//	          E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//             (transformation :SQLTransformation[X, Y])
//			:SpecificTransformation[X, Y, E, E[X], transformation.SQLResult[F, S, E[Y]]] =
//		transformation match {
//			case _ :SQLConversion[X, Y] =>
//				transformation.specific[F, S, E] //SQLConversion overrides specific, so there is no infinite loop
//			case _ =>
//				new SQLTransformationAdapter[X, Y, E, E[X], transformation.SQLResult[F, S, E[Y]]] {
//					override val conversion = transformation
//					override def sql(expr :E[X]) :transformation.SQLResult[F, S, E[Y]] = transformation(expr)
//				}
//		}
//
//	/** The same as the single argument method accepting only a `SQLTransformation[X, Y]`. The additional
//	  * expression argument is present only to help the compiler infer the proper type arguments
//	  * and is otherwise unused.
//	  */
//	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y,
//	          E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//	         (arg :ConvertingTemplate[F, S, X, E, E[X]], transformation :SQLTransformation[X, Y])
//			:SpecificTransformation[X, Y, E, E[X], transformation.SQLResult[F, S, E[Y]]] =
//		apply[F, S, X, Y, E](transformation)
//
//	def unapply[X, Y, E[_], A, R](transformation :SpecificTransformation[X, Y, E, A, R]) :Opt[SQLTransformation[X, Y]] =
//		transformation match {
//			case adapter :SQLTransformationAdapter[X, Y, E, A, R] => Got(adapter.conversion)
//			case composed :ComposedSpecificTransformation[X, z, Y, E, A, b, R] =>
//				(composed.first, composed.second) match {
//					case (SpecificTransformation(first), SpecificTransformation(second)) => Got(first andThen second)
//					case _ => Lack
//				}
//			case _ => Lack
//		}
//
//	//fixme: implicit conversions and values don't work because Scala cannot derive E[V]
//	implicit def dedicate[F <: RowProduct, S >: Grouped <: Single, X, Y,
//	          E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//             (transformation :SQLTransformation[X, Y])
//			:SpecificTransformation[X, Y, E, E[X], transformation.SQLResult[F, S, E[Y]]] =
//		apply[F, S, X, Y, E](transformation)
//
//	object Composition {
//		def unapply[X, Z, E[_], A, C]
//		           (transformation :SpecificTransformation[X, Z, E, A, C])
//				:Opt[(SpecificTransformation[X, Y, E, A, B], SpecificTransformation[Y, Z, E, B, C]) forSome {
//				        type Y; type B
//				 }] =
//			transformation match {
//				case composite :ComposedSpecificTransformation[X, y, Z, E, A, b, C] @unchecked =>
//					Got((composite.first, composite.second))
//				case _ => Lack
//			}
//	}
//}
//
//
//private trait SQLTransformationAdapter[X, Y, E[_], -Arg, +Res]
//	extends SpecificTransformation[X, Y, E, Arg, Res]
//{
//	val conversion :SQLTransformation[X, Y]
//	def sql(expr :Arg) :Res
//	override def apply(value :X) :Y = conversion(value)
//	override def unapply(value :Y) :Opt[X] = conversion.unapply(value)
//	override def inverse(value :Y) :X = conversion.inverse(value)
//
//	override def apply(form :SQLReadForm[X]) :SQLReadForm[Y] = conversion(form)
//	override def apply(form :ColumnReadForm[X]) :ColumnReadForm[Y] = conversion(form)
//	override def apply(form :SQLForm[X]) :SQLForm[Y] = conversion(form)
//	override def apply(form :ColumnForm[X]) :ColumnForm[Y] = conversion(form)
//
//	override def isIdentity   :Boolean = conversion.isIdentity
//	override def isLossless   :Boolean = conversion.isLossless
//	override def isReversible :Boolean = conversion.isReversible
//	override def isDerived    :Boolean = conversion.isDerived
//
//	override def applyString(arg :String) :String = conversion.applyString(arg)
//}
//
//
//private class ComposedSpecificTransformation[X, Y, Z, E[_], -E1, E2, +E3]
//                                            (val first :SpecificTransformation[X, Y, E, E1, E2],
//                                             val second :SpecificTransformation[Y, Z, E, E2, E3])
//	extends SpecificTransformation[X, Z, E, E1, E3]
//{   //overrides in order to not needlessly evaluate the underlying composed conversion.
//	override def apply(value :X) :Z = second(first(value))
//	override def unapply(value :Z) :Opt[X] = second.unapply(value).flatMap(first.unapply)
//	override def inverse(value :Z) :X = first.inverse(second.inverse(value))
//	override def sql(e :E1) = second.sql(first.sql(e))
//
//	override def isDerived = first.isDerived && second.isDerived
//
//	override def applyString(arg :String) :String = second.applyString(first.applyString(arg))
//}
//
//
//
//
//trait SpecificConversion[X, Y, E[_]] extends SpecificTransformation[X, Y, E, E[X], E[Y]] {
//	val conversion :SQLConversion[X, Y]
//
//	override def result(res: => E[Y]) :Opt[E[Y]] = Got(res)
//
//	override def convert(res :SpecificConversion[X, Y, E] => E[Y]) :Opt[E[Y]] = res(this)
//
//	override def compose[W, A](first :SpecificTransformation[W, X, E, A, E[X]])
//			:SpecificTransformation[W, Y, E, A, E[Y]] =
//		first match {
//			case _ :SpecificConversion[W, X, E] => first andThen this
//			case _ => new ComposedSpecificTransformation(first, this)
//		}
//	override def compose[W](first :SpecificConversion[W, X, E])(implicit ev :E[X] <:< E[X])//(implicit equiv :Equivalent[P[X], R])
//			:SpecificConversion[W, Y, E] =
//		if (first.isIdentity) first andThen this
//		else new ComposedSpecificConversion(first, this)
//
//	override def andThen[Z, A >: E[Y], R](second :SpecificTransformation[Y, Z, E, A, R])
//			:SpecificTransformation[X, Z, E, E[X], R] =
//		second match {
//			case _ :SpecificConversion[Y, Z, E] => second compose this
//			case _ => new ComposedSpecificTransformation(this, second)
//		}
//	override def andThen[Z](second :SpecificConversion[Y, Z, E])(implicit ev :E[Y] <:< E[Y])
//			:SpecificConversion[X, Z, E] =
//		if (second.isIdentity) second compose this
//		else new ComposedSpecificConversion(this, second)
//}
//
//
//object SpecificConversion {
//	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y,
//	          E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//             (transformation :SQLConversion[X, Y]) :SpecificConversion[X, Y, E] =
//		new SQLTransformationAdapter[X, Y, E, E[X], E[Y]] with SpecificConversion[X, Y, E] {
//			override val conversion = transformation
//			override def sql(arg :E[X]) = conversion(arg)
//		}
//
//	/** The same as the single argument method accepting only a `SQLTransformation[X, Y]`. The additional
//	  * expression argument is present only to help the compiler infer the proper type arguments
//	  * and is otherwise unused.
//	  */
//	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y,
//	          E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//	         (arg :ConvertingTemplate[F, S, X, E, E[X]], transformation :SQLTransformation[X, Y])
//			:SpecificConversion[X, Y, E] =
//		apply[F, S, X, Y, E](transformation)
//
//	//fixme: implicit conversions and values don't work because Scala cannot derive E[V]
//	implicit def dedicate[F <: RowProduct, S >: Grouped <: Single, X, Y,
//	                      E[v] <: SQLExpression[F, S, v] with ConvertingTemplate[F, S, v, E, E[v]]]
//	                     (transformation :SQLConversion[X, Y]) :SpecificConversion[X, Y, E] =
//		apply[F, S, X, Y, E](transformation)
//
//	object Composition {
//		def unapply[X, Z, E[_], A, C]
//		           (transformation :SpecificTransformation[X, Z, E, A, C])
//				:Opt[(SpecificConversion[X, Y, E], SpecificConversion[Y, Z, E]) forSome { type Y }] =
//			transformation match {
//				case composite :ComposedSpecificConversion[X, y, Z, E] @unchecked =>
//					Got((composite.first, composite.second))
//				case _ => Lack
//			}
////		def unapply[X, Z, E[_]](transformation :SpecificConversion[X, Z, E])
////				:Opt[(SpecificConversion[X, Y, E], SpecificConversion[Y, Z, E]) forSome { type Y }] =
////			transformation match {
////				case composed :ComposedSpecificConversion[X, y, Z, E] =>
////					Got((composed.first, composed.second))
////				case _ => Lack
////			}
//	}
//
//	def identity[V, E[_]] :SpecificConversion[V, V, E] =
//		Identity.asInstanceOf[SpecificConversion[V, V, E]]
//
//	private class Identity[X, E[_]]
//		extends SQLTransformationAdapter[X, X, E, E[X], E[X]] with SpecificConversion[X, X, E]
//	{
//		override val conversion = SQLConversion.toSelf
//
//		override def sql(expr :E[X]) :E[X] = expr
//		override def apply(value :X) = value
//		override def unapply(value :X) = value
//		override def inverse(value :X) = value
//
//		override def compose[W, A](first :SpecificTransformation[W, X, E, A, E[X]]) = first
//		override def compose[W](first :SpecificConversion[W, X, E])(implicit ev :E[X] <:< E[X]) = first
//		override def andThen[Z, A >: E[X], R](second :SpecificTransformation[X, Z, E, A, R]) = second
//		override def andThen[Z](second :SpecificConversion[X, Z, E])(implicit ev :E[X] <:< E[X]) = second
//
//		private def readResolve = identity[X, E]
//	}
//	private val Identity = new Identity[Any, ({ type T[x] = Any })#T]
//
//	private class ComposedSpecificConversion[X, Y, Z, E[_]]
//                                            (override val first  :SpecificConversion[X, Y, E],
//                                             override val second :SpecificConversion[Y, Z, E])
//		extends ComposedSpecificTransformation[X, Y, Z, E, E[X], E[Y], E[Z]](first, second)
//			with SpecificConversion[X, Z, E]
//	{
//		override lazy val conversion :SQLConversion[X, Z] = first.conversion andThen second.conversion
//	}
//
//}

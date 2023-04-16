package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.{implicitNotFound, tailrec}
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.{Chain, Listing, Opt}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.IllegalExpressionException
import net.noresttherein.oldsql.morsels.inversePermutation
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormAdapter
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.forms.{MappedColumnForm, MappedColumnReadForm, MappedSQLForm, MappedSQLReadForm, OptMappedColumnForm, OptMappedSQLForm}
import net.noresttherein.oldsql.slang.{cast2TypeParams, classMethods, classNameMethods, saferCasting}
import net.noresttherein.oldsql.sql.{ColumnSQL, GroundColumn, Query, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnConvertingOps, ConvertibleColumn}
import net.noresttherein.oldsql.sql.Query.QueryTemplate
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, ConvertingOps, Grouped, Single}
import net.noresttherein.oldsql.sql.StoredProcedure.Out
import net.noresttherein.oldsql.sql.ast.{ChainSQL, IdentityTransformedSQL, IndexedSQL, MultiNull, QuerySQL, RecordSQL, SQLNull, denullify}
import net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledValueSQL
import net.noresttherein.oldsql.sql.ast.QuerySQL.Rows
import net.noresttherein.oldsql.sql.ast.RecordSQL.{ProjectRecord, TransformRecord}
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.mechanics.ReversibleConversion.Widening
import net.noresttherein.oldsql.sql.mechanics.SQLAdaptation.DefaultExpressionAdaptation
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.{ChainConversion, ListingToChain, RecordConversion, RecordConversionEntry, RecordReordering, RowsConversion, SeqConversion, toSelf}
import net.noresttherein.oldsql.sql.mechanics.SQLFormConversion.SQLFormIdentity
import net.noresttherein.oldsql.sql.mechanics.SQLTransformation.{ArbitraryTransformation, ChainTransformation, ColumnTransformation, IndependentTransformation}
import net.noresttherein.oldsql.sql.mechanics.SQLValueConversion.SQLValueIdentity




//consider: moving it and SQLFormConversion inside SQLTransformation
trait SQLValueConversion[X, Y] extends (X => Y) with Serializable {
	def apply(value :X)   :Y
	def unapply(value :Y) :Opt[X]

	@throws[UnsupportedOperationException]("if the conversion is not reversible.")
	@throws[IllegalArgumentException]("if the given value cannot be converted back to the input type.")
	def inverse(value :Y) :X

	/** True if this instance leaves the converted SQL expressions unchanged.
	  * @return `this` == [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation$ SQLTransformation]]`.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.toSelf toSelf]].
	  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.isValueIdentity isDecorator]]
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
	def isValueIdentity :Boolean = false

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


	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLFormConversion[_, _]]

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


object SQLValueConversion {
	trait SQLValueConversionProxy[X, Y] extends SQLValueConversion[X, Y] {
		protected def conversion :SQLValueConversion[X, Y]

		override def apply(value :X) :Y = conversion(value)
		override def unapply(value :Y) :Opt[X] = conversion.unapply(value)
		override def inverse(value :Y) :X = conversion.inverse(value)

		override def applyString(arg :String) :String = conversion.applyString(arg)
	}

	trait SQLValueIdentity[X] extends SQLValueConversion[X, X] {
		override def apply(value :X)   :X = value
		override def inverse(value :X) :X = value
		override def unapply(value :X) :Got[X] = Got(value)
		override def isValueIdentity  = true
		override def isReversible     = true
		override def isLossless       = true

		override def applyString(arg :String) :String = arg
	}
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
trait SQLFormConversion[X, Y] extends SQLValueConversion[X, Y] {

	/* Matching of forms created by our swap is important, because it is common when reforming SQL expressions
	 * (in particular terms) to already have a form of type `Y` from the other expression, but needing to
	 * 'unmap' it to a form of `X` in order for the whole expression to be mapped with this conversion, including,
	 * most most likely, its selectForm. This means that even if a conversion is not reversible, there is a chance
	 * everything will work correctly after we unwrap the original form from the one created by our swap.
	 */
	private trait ConversionReadForm extends ReadFormAdapter[Y] {
		override def form :SQLReadForm[X]
		def conversion = SQLFormConversion.this
		override lazy val toString = SQLFormConversion.this.applyString(form.toString) + "=>"
	}
	private trait ConversionForm extends ConversionReadForm {
		override def form :SQLForm[X]
		override lazy val toString = "<=" + SQLFormConversion.this.applyString(form.toString) + "=>"
	}


	def apply(form :SQLReadForm[X]) :SQLReadForm[Y] = form match {
		case column :ColumnReadForm[X] =>
			apply(column)
		case swapped :SQLFormConversion[Y, X]#ConversionReadForm @unchecked if swapped.conversion == swap =>
			swapped.form
		case _ =>
			new MappedSQLReadForm[X, Y](apply)(form, form.nulls.map(apply)) with ConversionReadForm
	}

	def apply(form :ColumnReadForm[X]) :ColumnReadForm[Y] = form match {
		case swapped :SQLFormConversion[Y, X]#ConversionReadForm @unchecked if swapped.conversion == swap =>
			swapped.form.asInstanceOf[ColumnReadForm[Y]]
		case _ =>
			new MappedColumnReadForm[X, Y](apply)(form, form.nulls.map(apply)) with ConversionReadForm
	}

	def apply(form :SQLForm[X]) :SQLForm[Y] = form match {
		case column :ColumnForm[X] =>
			apply(column)
		case swapped :SQLFormConversion[Y, X]#ConversionForm @unchecked if swapped.conversion == swap =>
			swapped.form
		case _ if isReversible =>
			new MappedSQLForm[X, Y](apply, inverse)(form, form.nulls.map(apply)) with ConversionForm
		case _ =>
			new OptMappedSQLForm[X, Y](x => Some(apply(x)), unapply)(form, form.nulls.map(apply)) with ConversionForm
	}

	def apply(form :ColumnForm[X]) :ColumnForm[Y] = form match {
		case swapped :SQLFormConversion[Y, X]#ConversionForm @unchecked if swapped.conversion == swap =>
			swapped.form.asInstanceOf[ColumnForm[Y]]
		case _ if isReversible =>
			new MappedColumnForm[X, Y](apply, inverse)(form, form.nulls.map(apply)) with ConversionForm
		case _ =>
			new OptMappedColumnForm[X, Y](x => Some(apply(x)), unapply)(form, form.nulls.map(apply)) with ConversionForm
	}

	def swap :SQLFormConversion[Y, X]
}


object SQLFormConversion {
	trait SQLFormIdentity[X] extends SQLFormConversion[X, X] with SQLValueIdentity[X] {
		override def apply(form :SQLReadForm[X])    :SQLReadForm[X] = form
		override def apply(form :ColumnReadForm[X]) :ColumnReadForm[X] = form
		override def apply(form :SQLForm[X])    :SQLForm[X] = form
		override def apply(form :ColumnForm[X]) :ColumnForm[X] = form

		override def swap :this.type = this
	}

	object ReadForm {
		def unapply[X](form :SQLReadForm[X]) :Opt[SQLFormConversion[_, _ <: X]] = form match {
			case converted :SQLFormConversion[_, X]#ConversionReadForm @unchecked => Got(converted.conversion)
			case _ => Lack
		}
	}
	object Form {
		def unapply[X](form :SQLForm[X]) :Opt[SQLFormConversion[_, X]] = form match {
			case converted :SQLFormConversion[_, X]#ConversionForm @unchecked => Got(converted.conversion)
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
  * which are rendered as the same SQL, or compatible with the adapted expressions. Unlike the former,
  * implementations of of `SQLTransformation` are allowed to create completely new expressions of arbitrary types.
  * Furthermore, an expression transformed by can alter the [[net.noresttherein.oldsql.sql.RowShape RowShape]]
  * of the transformed expression.
  */
@implicitNotFound("I do not know how to convert an SQLExpression[F, S, ${X}] to an SQLExpression[F, S, ${Y}]. " +
                  "Missing implicit SQLTransformation[${X}, ${Y}].")
//fixme: this should not extend SQLFormConversion because it cannot truly convert column forms.
trait SQLTransformation[X, Y] extends SQLFormConversion[X, Y] { self =>
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
	  * its [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]] type without
	  * having to declare all parameter types of the latter. Use:
	  * {{{
	  *     def m[X, Y, R[F <: RowProduct, S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]]]
	  *             :SQLTransformation[X, Y]#Into[R]
	  * }}}
	  */
	type Into[R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLTransformation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = R[F, S, E]
		}

	type IntoBase[R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLTransformation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] =
				R[F, S, SQLExpression[F, S, Y]]
		}

	type IntoColumn = SQLTransformation[X, Y] {
		type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = ColumnSQL[F, S, Y]
	}

//	type Composed[R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, X]] <: SQLExpression[F, S, ]]


	def refine :SQLTransformation.Into[X, Y, Expression] = this

//
//	type BoundBy[R[F <: RowProduct, S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
//		SQLTransformation[X, Y] {
//			type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: R[F, S, E]
//		}

	/** Equals `this.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]]`[F, S, SQLExpression[F, S, Y]]`
	  * in all concrete implementations. Declared here as double bounded instead in order to be able to impose
	  * an upper bound on it through refinement,
	  * needed by [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.compose compose]].
	  */
	//Todo: try to revive this type, so we can express that
	// (f andThen g).Expression[F, S, SQLExpression[F, S, Z]] =:= g.Expression[F, S, SQLExpression[F, S, Z]]
//	type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//		SQLResult[F, S, SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]
//	type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] >:
//		Expression[F, S, SQLExpression[F, S, Y]] <: Expression[F, S, SQLExpression[F, S, Y]]

	//E must be covariant because we want Expr type parameter to ConvertibleSQL to also be covariant
	/** The type of SQL expressions created by this instance from expressions conforming to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingOps ConvertingOps]]`[F, S, X, EC, EC[X]]`,
	  * where type parameter `E` is always specified as `EC[Y]` - the type to which such an argument expression
	  * would [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingOps.convert convert]]
	  * with an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]].
	  * It follows, that the latter defines this type as `EC[Y]`, while most other implementations (typically extending
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.ArbitraryTransformation ArbitraryTransformation]])
	  * define it simply as `SQLExpression[F, S, Y]`.
	  * Specialized implementations may potentially implement any transformation, having any expression type
	  * as the conversion result.
	  */
	 /* It is tempting to try accepting E[v] instead to be able to express that this type for a composition
	  * of transformations always equals the composition of these types in composed transformation.
	  * For this however the type would also have to accept V as the type parameter, and it would exclude
	  * the possibility to implement transformations with fixed return type.
	  */
	type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]
//
//	type Returns[-F <: RowProduct, -S >: Grouped <: Single, V, +E[v] <: ConvertibleSQL[F, S, v, E]] <:
//		ConvertibleSQL[F, S, V, ({ type E2[v] = Returns[F, S, v, E2] })#E2]

	/** The supertype of all queries created by this instance through converting rows of a `Query[P, X]`. */
	type QueryResult[P, +Q <: Query[P, Y]] <: Query[P, Y]


	def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	         (expr :ConvertingOps[F, S, X, E]) :Expression[F, S, E[Y]]

//	def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//	         (expr :ConvertibleSQL[F, S, X, E]) :SQLResult[F, S, E[Y]]
	//an alias because overloading fools the type inferer
	def convert[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	           (expr :ConvertingOps[F, S, X, E]) :Expression[F, S, E[Y]] =
		apply[F, S, E](expr)

	def default[F <: RowProduct, S >: Grouped <: Single](expr :SQLExpression[F, S, X])
			:Expression[F, S, SQLExpression[F, S, Y]] =
		apply[F, S, SQLExpression.from[F]#rows[S]#E](expr)

	//Consider: shouldn't we care only about isUniversal et all, rather than the output type? We don't need
	//
	/** Specifies if this transformation will convert any expression type.
	  * Transformations which throw an exception based on the type of their arguments should override this method
	  * to return `false`
	  */
	def isUniversal      :Boolean = !isColumnOnly

	/** True if this transformation works only for instances of [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]].
	  * Implies `!this.`[[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.isUniversal isUniversal]].
	  */ //what about those that require a specific subtype of ColumnSQL, like ColumnMappingSQL?
	def isColumnOnly     :Boolean = false

	/** True for [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]] and other implementations
	  * for which [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]] type
	  * actually depends on its `E` type argument, that is, the type of the expressions created by the transformation
	  * statically depend on the type of the argument expression.
	  */
	/* Currently we cannot define Expression type which will return either SQLExpression or ColumnSQL,
	 * so this flag guarantees that it doesn't matter if the argument is a column for type checking.
	 * It will be however possible with match types, and if/when we do it, we must add another flag for this purpose
	 * (or use either isColumn or isColumnOnly).
	 */
	def isTypeDependent  :Boolean = false

	/** True for transformations whose [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]]
	  * type (the type of created expressions) is a proper subtype
	  * of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]. Being specific is taken as a strong indicator
	  * that the expression accepting the converted expression will not accept any `SQLExpression`. If `false`,
	  * some otherwise non-type safe operations may take place.
	  */
	def isSpecific       :Boolean = true

	/** True for transformations whose [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]]
	  * type is a subtype of [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] (including `ColumnSQL` itself).
	  */
	def isColumn         :Boolean = false

	/** True for transformations whose [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]]
	  * type is a proper subtype of [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]. Similarly to
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.isSpecific isSpecific]], it is taken to mean
	  * that the destined use site of the created expressions will not accept any `ColumnSQL`.
	  */
	def isSpecificColumn :Boolean = false


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

	def compose[W](first :SQLTransformation[W, X]) :SQLTransformation.Bound[W, Y, Expression] =
		first match {
			case conversion :SQLConversion[W, X] => compose(conversion)
			case trans :first.type =>
				new ArbitraryComposedTransformation[W, X, Y] {
					override val first  :trans.type = trans
					override val second :SQLTransformation.this.type = SQLTransformation.this
				}
		}

/*
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
*/

	def compose[W](first :SQLConversion[W, X]) :SQLTransformation.Into[W, Y, Expression] =
		if (first.isIdentity) first andThen this
		else new ConversionComposedTransformation[W, X, Y, this.type](first, this)

//	/** Alias for `andThen`, because overload with inherited method from `X => Y` confuses the compiler. */
//	def ==>[Z](second :SQLTransformation[Y, Z]) :SQLTransformation.Composed[X, Z, second.Expression] =
//		second compose this

	def andThen[Z](second :SQLTransformation[Y, Z]) :SQLTransformation.Bound[X, Z, second.Expression] =
		second compose this

	protected[mechanics] def split :Opt[(SQLTransformation[X, A], SQLTransformation[A, Y]) forSome { type A }] = Lack

	protected[mechanics] def splitConversion
			:Opt[(SQLConversion[X, A], SQLTransformation.Into[A, Y, Expression]) forSome { type A }] = Lack


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
	//consider: removing it
	override def swap :SQLTransformation[Y, X] = new Swap

	private class Swap extends ArbitraryTransformation[Y, X] with InverseTransformation[Y, X] {
		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingOps[F, S, Y, E]) :SQLExpression[F, S, X] =
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, Y, E]) :SQLExpression[F, S, X] =
			new IdentityTransformedSQL[F, S, Y, X](denullify(expr), this)
		override lazy val swap = SQLTransformation.this
	}

//	def generic[F <: RowProduct, S >: Grouped <: Single]
//			:SpecificTransformation[X, Y, SQLExpression.from[F]#rows[S]#E, SQLExpression[F, S, X], SQLExpression[F, S, Y]] =
//		specific[F, S, SQLExpression.from[F]#rows[S]#E]
//
//	def specific[F <: RowProduct, S >: Grouped <: Single,
//	             E[v] <: SQLExpression[F, S, v] with ConvertingOps[F, S, v, E, E[v]]]
//			:SpecificTransformation[X, Y, E, E[X], SQLResult[F, S, E[Y]]] =
//		SpecificTransformation[F, S, X, Y, E](this)
//
//	def specific[F <: RowProduct, S >: Grouped <: Single,
//	             E[v] <: SQLExpression[F, S, v] with ConvertingOps[F, S, v, E, E[v]]]
//	            (e :ConvertingOps[F, S, X, E, E[X]]) :SpecificTransformation[X, Y, E, E[X], E[Y]] =
//		specific[F, S, E]


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLTransformation[_, _]]
}




object SQLTransformation {
	def summon[X, Y](implicit lift :SQLTransformation[X, Y]) :lift.type = lift

	@inline implicit def implicitSQLAdaptation[X, Y](implicit conversion :SQLAdaptation[X, Y])
			:SQLTransformation.Into[X, Y, conversion.Expression] =
		conversion

//	def const[X, Y](value :Y, expr :SQLExpression[RowProduct, Single, Y]) :SQLTransformation[X, Y] =
//		new ConstantTransformation(value, expr)



	type Into[X, Y, R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLTransformation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = R[F, S, E]
		}

	/** An `SQLTransformation[X, Y]`
	  * with its [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]] type
	  * bound from above by `R`.
	  */
	type Returning[X, Y, +R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLTransformation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: R[F, S, E]
		}

	/** Result of composition of two transformations, or a transformation and an `SQLAdaptation`.
	  * It uses the second transformation's
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]] type as a bound,
	  * but parameterized with `SQLExpression[F, S, Y]` (instead of type parameter `E`) as the output expression type.
	  * @tparam X transformation's input value type.
	  * @tparam Y transformation's output value type.
	  * @tparam R type `Expression` of the second transformation/adaptation.
	  */
	type Bound[X, Y, +R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLTransformation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
				R[F, S, SQLExpression[F, S, Y]]
		}

	/** A transformation whose returned expressions are of the same type, independent from the type
	  * of the argument expression. This is an alias meant to abstract over concrete interfaces.
	  * Implementations may however choose to extend
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.GenericTransformation]] or
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.IndependentTransformation]].
	  */
	type Wrapping[X, Y, +R[-F <: RowProduct, -S >: Grouped <: Single, V]
	                       <: ConvertibleSQL[F, S, V, ({ type E[v] = R[F, S, v] })#E]] =
		SQLTransformation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: R[F, S, Y]
		}



	trait SQLTransformationTemplate[X, Y, R[-F <: RowProduct, -S >: Grouped <: Single,
	                                        +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]]
		extends SQLTransformation[X, Y]
	{
		override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = R[F, S, E]
	}

	trait ArbitraryTransformation[X, Y] extends SQLTransformation[X, Y] {
//		override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = SQLExpression[F, S, Y]
		override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] =
			SQLExpression[F, S, Y]
//		override type QueryResult[P, +Q <: Query[P, Y]] = Query[P, Y]

		override def isSpecific      = false
		override def isTypeDependent = false

		override def unapply(value :Y) :Opt[X] = Lack
		override def inverse(value :Y) :X = unapply(value) match {
			case Got(x) => x
			case _ => throw new IllegalArgumentException(
				s"Cannot convert $value :${value.localClassName} back with $this."
			)
		}

		override lazy val swap :SQLTransformation[Y, X] = super.swap
	}

	/** An `SQLTransformation` which maps all expressions into a subtype
	  * of [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]. It also assumes that it is applicable only to columns,
	  * and that the return type is a proper subtype of `ColumnSQL`. Subclasses are free to override those properties,
	  * although if their type [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]]
	  * is simply `ColumnSQL`, they might wish to extend
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.ArbitraryColumnTransformation ArbitraryColumnTransformation]]
	  * instead.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.ColumnTransformation.isColumnOnly]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.ColumnTransformation.isSpecificColumn]]
	  */
	trait ColumnTransformation[X, Y] extends IndependentTransformation[X, Y] {
//		override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
//			ColumnSQL[F, S, Y]
		override type Converted[-F <: RowProduct, -S >: Grouped <: Single, V]
		                                                        <: ConvertibleColumn[F, S, V, ({ type E[v] = Converted[F, S, v] })#E]
		override def isTypeDependent  = false
		override def isSpecific       = true
		override def isColumn         = true
		override def isColumnOnly     = true
		override def isSpecificColumn = true

		override def unapply(value :Y) :Opt[X] = Lack
		override def inverse(value :Y) :X = unapply(value) match {
			case Got(x) => x
			case _ => throw new IllegalArgumentException(
				s"Cannot convert $value :${value.localClassName} back with $this."
			)
		}
/*
		override def andThen[Z](second :SQLTransformation[Y, Z])
				:SQLTransformation.Returning[X, Z, ({ type E[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] = second.Expression[F, S, ColumnSQL[F, S, Z]] })#E] =
		{
			val next :second.type = second
			new ComposedTransformation[X, Y, Z] {
				override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
					second.Expression[F, S, ColumnSQL[F, S, Z]]
				override val first  :ColumnTransformation.this.type = ColumnTransformation.this
				override val second :next.type = next

				override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
				                  (expr :ConvertingOps[F, S, X, E]) =
					second(first(expr))
			}
		}*/
	}

	/** An `SQLTransformation` which maps all expressions into a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]. */
	trait ArbitraryColumnTransformation[X, Y] extends ColumnTransformation[X, Y] {
//		override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] =
//			ColumnSQL[F, S, Y]
		override type Converted[-F <: RowProduct, -S >: Grouped <: Single, V] = ColumnSQL[F, S, V]
		override type Wrapped[-F <: RowProduct, -S >: Grouped <: Single] = Converted[F, S, Y]
//		override def isColumn = true
		override def isSpecificColumn = false
	}


//	trait SQLDecoration[X] extends SQLTransformation[X, X] with SQLFormIdentity[X]

	object SQLDecoration {
		def unapply[X, Y](e :SQLTransformation[X, Y])
				:Opt[(SQLConversion[X, Y], SQLTransformation.Into[Y, Y, e.Expression])] =
			if (e.isValueIdentity)
				Got((
					SQLConversion.identity.castParam2[Y], (e :Into[X, Y, e.Expression]).castFrom[Into[X, Y, e.Expression], Into[Y, Y, e.Expression]]
				))
			else
				Lack
	}

	/** An `SQLTransformation` which wraps its argument expressions in an adapter of a specific type. It overrides
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.IndependentTransformation.Expression Expression]]
	  * type in order to reflect it, as well as composition methods in order to preserve the wrapper type in the result.
	  * It is 'independent' in the sense that it is not
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.isTypeDependent type dependent]].
	  */
	trait IndependentTransformation[X, Y] extends SQLTransformation[X, Y] { decoration =>
		/** A supertype of
		  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.IndependentTransformation.Wrapped Wrapper]]
		  * which is preserved by [[net.noresttherein.oldsql.sql.mechanics.SQLConversion conversions]]. It should be
		  * the most specific [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingOps ConvertingOps]]
		  * mixed in into the decorator class.
		  */
		type Converted[-F <: RowProduct, -S >: Grouped <: Single, V] <: ConvertibleSQL[F, S, V, ({ type E[v] = Converted[F, S, v] })#E]

		/** The decorator class implementing this transformation. All argument
		  * [[net.noresttherein.oldsql.sql.SQLExpression expressions]] are wrapped in it using method
		  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.IndependentTransformation.wrap wrap]].
		  */
		type Wrapped[-F <: RowProduct, -S >: Grouped <: Single] <: Converted[F, S, Y]

		type AndThen[Z, T[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, Z]] <: SQLExpression[f, s, Z]] =
			SQLTransformation[X, Z] {
				type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
					T[F, S, Converted[F, S, Z]]
			}

		override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] =
			Wrapped[F, S]

		override def isSpecific      = true
		override def isTypeDependent = false

		/* Todo: see if we can enforce in composition SQLResult = second.SQLResult[f, s, Ex[f, s, Z]].
		 * We need it for reforming, but the problem is that it conflicts with
		 * CompositionTransformation.second.SQLResult[f, s, E] <: CompositionTransformation.SQLResult[f, s, E]
		 * for *all* E, not just for `Ex` - so while we can likely implement a dedicated transformation
		 * which has the proper SQLResult, it will not be a CompositionTransformation, and in other places we match
		 * against it, extracting the two constituents, and relying heavily on the composition property above.
		 */
		override def andThen[Z](next :SQLTransformation[Y, Z]) :AndThen[Z, next.Expression] =
			 if (next.isIdentity)
				this.asInstanceOf[AndThen[Z, next.Expression]]
			else
				new ComposedTransformation[X, Y, Z] {
					override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
						second.Expression[F, S, Converted[F, S, Z]]
					override val first  :SQLTransformation.Into[X, Y, decoration.Expression] = decoration
					override val second :next.type = next

					override def apply[F <: RowProduct, S >: Grouped <: Single, E[V] <: ConvertibleSQL[F, S, V, E]]
					                  (expr :ConvertingOps[F, S, X, E]) :next.Expression[F, S, Converted[F, S, Z]] =
						second(first(denullify(expr)) :Converted[F, S, Y])
				}

		override def compose[W](first :SQLTransformation[W, X]) :SQLTransformation.Into[W, Y, Expression] =
			if (first.isIdentity)
				this.asInstanceOf[SQLTransformation.Into[W, Y, Expression]]
			else {
				val transformation = first
				new ComposedTransformation[W, X, Y] {
					override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] =
						decoration.Wrapped[F, S]
					override val first  = transformation
					override val second :SQLTransformation.Into[X, Y, decoration.Expression] = decoration

					override def apply[F <: RowProduct, S >: Grouped <: Single, E[V] <: ConvertibleSQL[F, S, V, E]]
					                  (expr :ConvertingOps[F, S, W, E]) :decoration.Wrapped[F, S] =
						decoration(first(denullify(expr)))
				}
			}

		override def applyString(arg :String) :String = this.localClassName + "(" + arg + ")"
	}


	/** A 'template' trait for `IndependentTransformation`: a transformation implemented as wrapping any argument
	  * expression in an instance of `E`.
	  */ //Not a best name. Maybe return to IndependentTransformationTemplate? At least it informs that its one.
	trait GenericTransformation[X, Y, E[-F <: RowProduct, -S >: Grouped <: Single, V]
	                                    <: ConvertibleSQL[F, S, V, ({ type Ex[A] = E[F, S, A] })#Ex]]
		extends IndependentTransformation[X, Y]
	{
		override type Converted[-F <: RowProduct, -S >: Grouped <: Single, V] = E[F, S, V]
		override type Wrapped[-F <: RowProduct, -S >: Grouped <: Single] = E[F, S, Y]
	}


	object IndependentTransformation {
		type Convertible[X, Y, +E[-F <: RowProduct, -S >: Grouped <: Single, V] <:
	                              ConvertibleSQL[F, S, V, ({ type Ex[A] = E[F, S, A] })#Ex]] =
			IndependentTransformation[X, Y] {
				type Converted[-F <: RowProduct, -S >: Grouped <: Single, V] <: E[F, S, V]
				type Wrapped[-F <: RowProduct, -S >: Grouped <: Single] = Converted[F, S, Y]
			}
		type Wrap[X, Y, E[-F <: RowProduct, -S >: Grouped <: Single] <: SQLExpression[F, S, Y]] =
			IndependentTransformation[X, Y] {
				type Converted[-F <: RowProduct, -S >: Grouped <: Single, V] = SQLExpression[F, S, V]
				type Wrapped[-F <: RowProduct, -S >: Grouped <: Single] = E[F, S]
			}
//
//		trait IndependentTransformationTemplate[X, Y, E[-F <: RowProduct, -S >: Grouped <: Single, V] <:
//		                                              ConvertibleSQL[F, S, V, ({ type Ex[A] = E[F, S, A] })#Ex]]
//			extends IndependentTransformation[X, Y]
//		{
//			override type Converted[-F <: RowProduct, -S >: Grouped <: Single, V] = E[F, S, V]
//			override type Wrapper[-F <: RowProduct, -S >: Grouped <: Single] = E[F, S, Y]
//		}
	}


	trait IrreversibleTransformation[X, Y] extends SQLTransformation[X, Y] {
		override def isReversible = false
		override def unapply(value :Y) :Opt[X] = Lack
		override def inverse(value :Y) =
			throw new UnsupportedOperationException("Transformation " + this + " is irreversible.")
	}


	object Composition {
		def unapply[X, Z](e :SQLTransformation[X, Z])
				:Opt[(SQLTransformation[X, Y], SQLTransformation[Y, Z]) forSome { type Y }] =
			e.split

		def unapply[X, Z](e :SQLAdaptation[X, Z])
				:Opt[(SQLAdaptation[X, Y], SQLAdaptation[Y, Z]) forSome { type Y }] =
			e.split

		def unapply[X, Z](e :SQLConversion[X, Z])
				:Opt[(SQLConversion[X, Y], SQLConversion[Y, Z]) forSome { type Y }] =
			e.split

		object WithConversion {
			def unapply[X, Z](e :SQLTransformation[X, Z])
					:Opt[(SQLConversion[X, Y], SQLTransformation.Into[Y, Z, e.Expression]) forSome { type Y }] =
			{
				type Split[A, B] = (SQLConversion[A, B], SQLTransformation.Into[B, Z, e.Expression])
				e.splitConversion match {
					case split :Opt[Split[X, y]] if split.isDefined =>
						type Y = y
						val (first, second) = split.get
						unapply(second) match {
							case split2 :Opt[Split[Y, z]] if split2.isDefined =>
								Got((first andThen split2.get._1, split2.get._2))
							case _ =>
								split
						}
					case _ => Lack
				}
			}
		}

		object DecoratedConversion {
			def unapply[X, Y](e :SQLTransformation[X, Y])
					:Opt[(SQLConversion[X, Y], SQLTransformation.Into[Y, Y, e.Expression])] =
				e match {
					case WithConversion(g, f :SQLFormIdentity[_]) =>
						Got((g.castParams[X, Y], f.castFrom[SQLTransformation[_, Y], Into[Y, Y, e.Expression]]))
					case _ =>
						Lack
				}
		}
	}

	type from[X] = { type to[Y] = SQLTransformation[X, Y] }
	type to[Y] = { type from[X] = SQLTransformation[X, Y] }



	/** Recursively performs a 'deep conversion' of a chain expression `SQLExpression[F, S, XI ~ XL]`
	  * to another `SQLExpression[F, S, XI ~ XL]`.
	  */
	case class ChainTransformation[XI <: Chain, XL, YI <: Chain, YL]
	                              (init :SQLTransformation[XI, YI], last :SQLTransformation[XL, YL])
		extends ArbitraryTransformation[XI ~ XL, YI ~ YL]
	{
		//An expression with a Listing type can be a AdaptedSQL
		override def isUniversal  :Boolean = false
		override def isReversible :Boolean = init.isReversible && last.isReversible
		override def isLossless   :Boolean = init.isLossless && last.isLossless

		override def apply(value :XI ~ XL) :YI ~ YL = init(value.init) ~ last(value.last)

		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingOps[F, S, XI ~ XL, E]) :SQLExpression[F, S, YI ~ YL] =
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, XI ~ XL, E]) :SQLExpression[F, S, YI ~ YL] =
			expr match {
				case null => MultiNull[YI ~ YL](1)
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
						ChainConversion(i, l)(expr)
					case _ => throw new IllegalExpressionException(
						"Cannot transform expression " + expr + " with " + this +
							" because it is neither ChainSQL nor ChainTuple, nor AdaptedSQL."
					)
				}
			}

		override def applyString(arg :String) :String = {
			def rec(conv :SQLTransformation[_, _]) :String = conv match {
				case listing :RecordConversion[_, _, _, _, _] => rec(listing.init) + "~" + listing.last
				case _ if conv == toSelf => arg + ".transform(@~"
				case _ => conv.applyString(arg) + ".transformSuffix("
			}
			rec(this) + ")"
		}
	}

	case class TransformRecord[XI <: Listing, XL, YI <: Listing, YL, K <: Label]
	                               (init :SQLTransformation[XI, YI], last :SQLTransformation[XL, YL])
		extends ArbitraryTransformation[XI |~ (K :~ XL), YI |~ (K :~ YL)]
	{
		override def isUniversal  :Boolean = false
		override def isReversible :Boolean = init.isReversible && last.isReversible
		override def isLossless   :Boolean = init.isLossless && last.isLossless

		override def apply(value :XI |~ (K :~ XL)) :YI |~ (K :~ YL) = init(value.init) |~ :~[K](last(value.last.value))

		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingOps[F, S, XI |~ (K :~ XL), E]) :SQLExpression[F, S, YI |~ (K :~ YL)] =
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, XI |~ (K :~ XL), E]) :SQLExpression[F, S, YI |~ (K :~ YL)] =
			expr match {
				case null =>
					MultiNull(1)
				case sqlNull :MultiNull[XI |~ (K :~ XL)] =>
					MultiNull(apply(sqlNull.form))

				case listing :IndexedSQL[F, S, XI |~ (K :~ XL)] => init(listing.init) match {
					case yi :IndexedSQL[F, S, YI] => last(listing.last) match {
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
								" should have created a IndexedSQL, but got " + yi + ".")
				}
				case _ => (init, last) match {
					case (i :SQLConversion[XI, YI], l :SQLConversion[XL, YL]) =>
						SQLConversion.record[XI, XL, YI, YL, K](i, l)(expr)
					case _ => throw new IllegalExpressionException(
						"Cannot transform expression " + expr + " with " + this + " because it is not a IndexedSQL."
					)
				}
			}

		override def applyString(arg :String) :String = {
			def rec(conv :SQLTransformation[_, _]) :String = conv match {
				case listing :RecordConversion[_, _, _, _, _] => rec(listing.init) + "|~" + listing.last
				case _ if conv == toSelf => arg + ".transform(@~"
				case _ => conv.applyString(arg) + ".transformSuffix("
			}
			rec(this) + ")"
		}
	}


	private case class ConstantTransformation[X, Y](value :Y, result :SQLExpression[RowProduct, Single, Y])
		extends ArbitraryTransformation[X, Y]
	{
		override def apply(value :X) :Y = this.value

		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingOps[F, S, X, E]) :SQLExpression[F, S, Y] =
			denullify(result).toConvertibleSQL
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, X, E]) :SQLExpression[F, S, Y] =
//			result

		override def applyString(arg :String) :String = arg + ".return(" + value + ")"
	}
}


private trait InverseTransformation[X, Y] extends SQLTransformation[X, Y] {
	override def isReversible = true
	override def apply(value :X)   = swap.inverse(value)
	override def inverse(value :Y) = swap(value)
	override def unapply(value :Y) = Got(swap(value))
	override def applyString(arg :String) = arg + ".inverse(" + swap + ")"

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :InverseTransformation[_, _] if canEqual(other) && other.canEqual(this) => swap == other.swap
		case _ => false
	}
	override def hashCode :Int = Integer.rotateLeft(swap.hashCode, 16)
}


//consider: making some of these public
private[sql] trait ComposedTransformation[X, Y, Z] extends SQLTransformation[X, Z] { self =>
//	private type Res[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]
//	override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = second.BaseResult[F, S]
	//todo: we should be able to express that self.Expression[F, S, E] <: second.Expression[F, S, SQLExpression[F, S, Z]]
	override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] >:
		//We need the lower bound because in classes like ChainSQL we split composed conversions and need to know
		// that they return a compatible expression.
		second.Expression[F, S, Nothing] <: second.Expression[F, S, SQLExpression[F, S, Z]]
//	override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <:
//		second.Expression[F, S, SQLExpression[F, S, Z]]
//	type Expr[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
//		second.Expr[F, S, first.Expr[F, S, E]]
//	override type Returns[-F <: RowProduct, -S >: Grouped <: Single, +E[v] <: ConvertibleSQL[F, S, v, E]] =
//		second.Returns[F, S, ({ type E[v] = first.Returns[F, S, v, E]

	val first  :SQLTransformation[X, Y]
	val second :SQLTransformation[Y, Z]
//	{
//		type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = self.BaseResult[F, S]
//		type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <: self.SQLResult[F, S, E]
//	}
	override def isReversible     :Boolean = first.isReversible && second.isReversible
	override def isLossless       :Boolean = first.isLossless && second.isLossless
	override def isUpcast         :Boolean = first.isUpcast && second.isUpcast
	override def isValueIdentity  :Boolean = first.isValueIdentity && second.isValueIdentity
	override def isUniversal      :Boolean = first.isUniversal && second.isUniversal
	override def isTypeDependent  :Boolean = first.isTypeDependent && second.isTypeDependent
	override def isSpecific       :Boolean = second.isSpecific
	override def isColumnOnly     :Boolean = first.isColumnOnly
	override def isColumn         :Boolean = second.isColumn
	override def isSpecificColumn :Boolean = second.isSpecificColumn

	override def apply(value: X)   :Z = second(first(value))
	override def unapply(value: Z) :Opt[X] = second.unapply(value).flatMap(first.unapply)
	override def inverse(value :Z) :X = first.inverse(second.inverse(value))

	override def apply(form :SQLReadForm[X]) :SQLReadForm[Z] = second(first(form))
	override def apply(form :SQLForm[X]) :SQLForm[Z] = second(first(form))
	override def apply(form :ColumnReadForm[X]) :ColumnReadForm[Z] = second(first(form))
	override def apply(form :ColumnForm[X]) :ColumnForm[Z] = second(first(form))

//	override def isDerived    :Boolean = first.isDerived && second.isDerived

	protected[mechanics] override def split :Opt[(SQLTransformation[X, Y], SQLTransformation[Y, Z])] =
		Got((first, second))

	override lazy val swap :SQLTransformation[Z, X] = {
		val firstSwap  = first.swap
		val secondSwap = second.swap
		new ArbitraryComposedTransformation[Z, Y, X] {
			override val first  = secondSwap
			override val second = firstSwap
			override def isReversible = true
			override lazy val swap = ComposedTransformation.this
			override def applyString(arg :String) = arg + ".inverse(" + swap + ")"
		}
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


private trait ArbitraryComposedTransformation[X, Y, Z] extends ComposedTransformation[X, Y, Z] {
//	override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = second.BaseResult[F, S]
	override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
		second.Expression[F, S, SQLExpression[F, S, Z]]

	override def isTypeDependent :Boolean = false

	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	                  (expr :ConvertingOps[F, S, X, E]) :Expression[F, S, E[Z]] =
		second(first(denullify(expr)))
}


private class ConversionComposedTransformation[X, Y, Z, T <: SQLTransformation[Y, Z]]
                                              (override val first :SQLConversion[X, Y], override val second :T)
	extends ComposedTransformation[X, Y, Z]
{
//	override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = second.BaseResult[F, S]
	override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
		second.Expression[F, S, E]

	override def isTypeDependent :Boolean = second.isTypeDependent

	protected[mechanics] override def splitConversion
			:Opt[(SQLConversion[X, Y], SQLTransformation.Into[Y, Z, second.Expression])] =
		Got((first, second :SQLTransformation.Into[Y, Z, second.Expression]))


	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	                  (expr :ConvertingOps[F, S, X, E]) :Expression[F, S, E[Z]] =
		second[F, S, E](first(expr))
//	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//	                  (expr :ConvertibleSQL[F, S, X, E]) :SQLResult[F, S, E[Z]] =
//		second[F, S, E](first(expr))
}






/** A conversion of any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, X]` to
  *  an `SQLExpression[F, S, Y]` which has the same column set as the original. The default implementation delegates to
  * [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingOps ConvertingOps]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingOps.adapt transform]],
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
trait SQLAdaptation[X, Y] extends SQLTransformation[X, Y] with SQLFormConversion[X, Y] { self =>
//	override def isDerived :Boolean = true

	type As[R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLAdaptation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = R[F, S, E]
		}
	type AsBase[R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLAdaptation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] =
				R[F, S, SQLExpression[F, S, Y]]
		}

	//do we need it? currently we stopped preserving it in composition.
	type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Y]] <: ColumnSQL[F, S, Y]

	override def refine :SQLAdaptation.Into[X, Y, Expression] = this

	def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
	          (expr :ColumnConvertingOps[F, S, X, E]) :ColumnResult[F, S, E[Y]]


	override def compose[W](first :SQLTransformation[W, X]) :SQLTransformation.Bound[W, Y, Expression] =
		first match {
			case conversion :SQLAdaptation[W, X] => compose(conversion)
			case _ => super.compose(first)
		}

	def compose[W](first :SQLAdaptation[W, X]) :SQLAdaptation.Bound[W, Y, Expression] =
		first match {
			case conversion :SQLConversion[W, X] => compose(conversion)
			case _ => new ArbitraryComposedAdaptation[W, X, Y, this.type](first, this)
		}

	override def compose[W](first :SQLConversion[W, X]) :SQLAdaptation[W, Y]#As[Expression] =
		if (first.isIdentity) first andThen this
		else new ConversionComposedAdaptation[W, X, Y, this.type](first, this)

//	override def andThen[Z](second :SQLTransformation[Y, Z]) :SQLTransformation[X, Z] = second andThen this
	override def andThen[Z](second :SQLTransformation[Y, Z]) :SQLTransformation.Bound[X, Z, second.Expression] =
		second compose this

	def andThen[Z](second :SQLAdaptation[Y, Z]) :SQLAdaptation.Bound[X, Z, second.Expression] =
		second compose this

	protected[mechanics] override def split :Opt[(SQLAdaptation[X, A], SQLAdaptation[A, Y]) forSome { type A }] = Lack

	protected[mechanics] override def splitConversion
			:Opt[(SQLConversion[X, A], SQLAdaptation.Into[A, Y, Expression]) forSome { type A }] = Lack


	protected override def swap(namePrefix :String, nameSuffix :String) :SQLAdaptation[Y, X] =
		new DefaultExpressionAdaptation[Y, X] with InverseTransformation[Y, X] {
			override lazy val swap = SQLAdaptation.this
			override def applyString(arg :String) = namePrefix + arg + nameSuffix
		}

	//This is a bit problematic, because it creates a vanilla AdaptedSQL, which performs nothing but value conversion,
	// but we can't change it to an SQLConversion, even privately, because its swap is not an SQLConversion...
	override def swap :SQLAdaptation[Y, X] =
		new DefaultExpressionAdaptation[Y, X] with InverseTransformation[Y, X] {
			override lazy val swap = SQLAdaptation.this
		}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLAdaptation[_, _]]
}




object SQLAdaptation {
	def summon[X, Y](implicit lift :SQLAdaptation[X, Y]) :lift.type = lift

	@inline implicit def implicitSQLConversion[X, Y](implicit conversion :SQLConversion[X, Y]) //:SQLConversion[X, Y] =
			:SQLAdaptation.Into[X, Y, conversion.Expression] =
		conversion
//
//	def apply[X, Y](suffix :String, convert :X => Y) :ArbitraryAdaptation[X, Y] =
//		new Impl(suffix, convert)
//
//	def apply[X, Y](suffix :String, convert :X => Y, inversed :Y => Opt[X]) :ArbitraryAdaptation[X, Y] =
//		new Impl[X, Y](suffix, convert) {
//			override def unapply(value :Y) :Opt[X] = inversed(value)
//		}
//
//	private class Impl[X, Y](suffix :String, convert :X => Y) extends DefaultExpressionAdaptation[X, Y] {
//		override def apply(value :X) :Y = convert(value)
//		override def applyString(arg :String) = arg + suffix
//	}


	type from[X] = { type to[Y] = SQLAdaptation[X, Y] }
	type to[Y] = { type from[X] = SQLAdaptation[X, Y] }

	type Into[X, Y, R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLAdaptation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = R[F, S, E]
		}

	/** An `SQLAdaptation[X, Y]` with its
	  *  [[net.noresttherein.oldsql.sql.mechanics.SQLAdaptation.Expression Expression]] type bound from above by `R`.
	  */
	type Returning[X, Y, R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLAdaptation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: R[F, S, E]
		}

	/** Result of composition of two adaptations. It uses the second adaptation's
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLAdaptation.Expression Expression]] type as a bound,
	  * but parameterized with `SQLExpression[F, S, Y]` (instead of type parameter `E`) as the output expression type.
	  * @tparam X adaptation's input value type.
	  * @tparam Y adaptation's output value type.
	  * @tparam R type `Expression` of the second adaptation.
	  */
	type Bound[X, Y, +R[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: SQLExpression[F, S, Y]] =
		SQLAdaptation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <:
				R[F, S, SQLExpression[F, S, Y]]
		}

	type Wrapping[X, Y, +R[-F <: RowProduct, -S >: Grouped <: Single, V]
	                       <: ConvertibleSQL[F, S, V, ({ type E[v] = R[F, S, v] })#E]] =
		SQLAdaptation[X, Y] {
			type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] <: R[F, S, Y]
		}

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
		                  (expr :ConvertingOps[F, S, X, E]) :SQLExpression[F, S, Y] =
			denullify(expr).`->adapt`(this)
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, X, E]) :SQLExpression[F, S, Y] =
//			expr.`->adapt`(this)

		override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
		                   (expr :ColumnConvertingOps[F, S, X, E]) :ColumnSQL[F, S, Y] =
			denullify(expr).`->adapt`(this)
	}


	/** An `SQLAdaptation` applicable only to [[net.noresttherein.oldsql.sql.ColumnSQL column]] expressions
	  * and always returning another column.
	  */
	trait ColumnAdaptation[X, Y] extends IndependentAdaptation[X, Y] with ColumnTransformation[X, Y] {
		override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Y]] =
			Expression[F, S, E]

		override def isUniversal  = false
		override def isColumnOnly = true

		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingOps[F, S, X, E]) :Expression[F, S, E[Y]] =
			expr match {
				case null if !isTypeDependent => column(SQLNull[X]())
				case null => null.asInstanceOf[Expression[F, S, E[Y]]]
				case column :ColumnSQL[F, S, X] =>
					this.column(column :ColumnSQL[F, S, X]).castFrom[
						Expression[F, S, ColumnSQL[F, S, Y]], Expression[F, S, E[Y]]
					]
				case MultiNull() if !isTypeDependent =>
					this.column(SQLNull[X]()).castFrom[Expression[F, S, GroundColumn[Y]], Expression[F, S, E[Y]]]
				case _ =>
					throw new IllegalExpressionException(
						"Cannot transform non column expression `" + expr + "` :" + expr.className + " with " + this + "."
					)
			}

/*
		override def andThen[Z](second :SQLAdaptation[Y, Z])
				:SQLAdaptation.Returning[X, Z, ({type E[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] = second.Expression[F, S, ColumnSQL[F, S, Z]]})#E] = {
			val next :second.type = second
			new ComposedAdaptation[X, Y, Z] {
				override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
					second.Expression[F, S, ColumnSQL[F, S, Z]]
				override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Z]] =
					ColumnSQL[F, S, Z]

				override val first :ColumnAdaptation.this.type = ColumnAdaptation.this
				override val second :next.type = next

				override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
				                  (expr :ConvertingOps[F, S, X, E]) =
					second(first(expr))

				override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
				                   (expr :ColumnConvertingOps[F, S, X, E]) =
					second.column(first.column(expr))
			}
		}
*/
	}


	/** An `SQLAdaptation` which wraps its argument expressions in an adapter of a specific type. It overrides
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.IndependentTransformation.Expression Expression]] type
	  * in order to reflect it, as well as composition methods in order to preserve the wrapper type in the result.
	  */
	trait IndependentAdaptation[X, Y] extends SQLAdaptation[X, Y] with IndependentTransformation[X, Y] { decoration =>
		//todo: use it in return type refinements
//		override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Y]] =
//			ColumnSQL[F, S, Y]

		type AndThenAdaptation[Z, T[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, Z]] <: SQLExpression[f, s, Z]] =
			SQLAdaptation[X, Z] {
				type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
					T[F, S, Converted[F, S, Z]]
			}

		override def andThen[Z](next :SQLAdaptation[Y, Z]) :AndThenAdaptation[Z, next.Expression] =
			 if (next.isIdentity)
				this.asInstanceOf[AndThenAdaptation[Z, next.Expression]]
			else
				new ComposedAdaptation[X, Y, Z] {
					override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Z]] =
						ColumnSQL[F, S, Z]
					override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] =
						second.Expression[F, S, Converted[F, S, Z]]
					override val first  :SQLAdaptation.Into[X, Y, decoration.Expression] = decoration
					override val second :next.type = next

					override def apply[F <: RowProduct, S >: Grouped <: Single, E[V] <: ConvertibleSQL[F, S, V, E]]
					                  (expr :ConvertingOps[F, S, X, E]) :next.Expression[F, S, Converted[F, S, Z]] =
						second(first(denullify(expr)) :Converted[F, S, Y])

					override def column[F <: RowProduct, S >: Grouped <: Single, E[V] <: ConvertibleColumn[F, S, V, E]]
					                   (expr :ColumnConvertingOps[F, S, X, E]) =
						second.column(first.column(denullify(expr)))
				}

		override def compose[W](first :SQLAdaptation[W, X]) :SQLAdaptation.Into[W, Y, Expression] =
			if (first.isIdentity)
				this.asInstanceOf[SQLAdaptation.Into[W, Y, Expression]]
			else {
				val transformation = first
				new ComposedAdaptation[W, X, Y] {
					override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Y]] =
						ColumnSQL[F, S, Y]
					override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] =
						Wrapped[F, S]
					override val first  = transformation
					override val second :SQLAdaptation.Into[X, Y, decoration.Expression] = decoration

					override def apply[F <: RowProduct, S >: Grouped <: Single, E[V] <: ConvertibleSQL[F, S, V, E]]
					                  (expr :ConvertingOps[F, S, W, E]) :decoration.Wrapped[F, S] =
						second(first(denullify(expr)))

					override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
					                   (expr :ColumnConvertingOps[F, S, W, E]) =
						second.column(first.column(expr))
				}
			}
	}


	object IndependentAdaptation {
		type Convertible[X, Y, +E[-F <: RowProduct, -S >: Grouped <: Single, V] <:
	                              ConvertibleSQL[F, S, V, ({ type Ex[A] = E[F, S, A] })#Ex]] =
			IndependentAdaptation[X, Y] {
				type Converted[-F <: RowProduct, -S >: Grouped <: Single, V] <: E[F, S, V]
				type Wrapped[-F <: RowProduct, -S >: Grouped <: Single] = Converted[F, S, Y]
			}
		type Wrap[X, Y, E[-F <: RowProduct, -S >: Grouped <: Single] <: SQLExpression[F, S, Y]] =
			IndependentAdaptation[X, Y] {
				type Converted[-F <: RowProduct, -S >: Grouped <: Single, V] = SQLExpression[F, S, V]
				type Wrapped[-F <: RowProduct, -S >: Grouped <: Single] = E[F, S]
			}
	}


	object Composition {
		def unapply[X, Z](e :SQLAdaptation[X, Z])
				:Opt[(SQLAdaptation[X, Y], SQLAdaptation[Y, Z]) forSome { type Y }] =
			e.split

		def unapply[X, Z](e :SQLConversion[X, Z])
				:Opt[(SQLConversion[X, Y], SQLConversion[Y, Z]) forSome { type Y }] =
			SQLConversion.Composition.unapply(e)

		object WithConversion {
			def unapply[X, Z](e :SQLConversion[X, Z])
					:Opt[(SQLConversion[X, Y], Bound[Y, Z, e.Expression]) forSome { type Y }] =
				e.splitConversion
		}
	}

	object ReadForm {
		def unapply[X](form :SQLReadForm[X]) :Opt[SQLAdaptation[_, _ <: X]] = form match {
			case SQLFormConversion.ReadForm(conversion :SQLAdaptation[_, X] @unchecked) => Got(conversion)
			case _ => Lack
		}
	}
	object Form {
		def unapply[X](form :SQLForm[X]) :Opt[SQLAdaptation[_, X]] = form match {
			case SQLFormConversion.Form(conversion :SQLAdaptation[_, X] @unchecked) => Got(conversion)
			case _ => Lack
		}
	}

}


private[sql] trait ComposedAdaptation[X, Y, Z]
	extends ComposedTransformation[X, Y, Z] with SQLAdaptation[X, Z] with SQLFormConversion[X, Z]
{ self =>
	override val first  :SQLAdaptation[X, Y]
	override val second :SQLAdaptation[Y, Z]
//	{
//		type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = self.BaseResult[F, S]
//		type SQLResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Z]] <: self.SQLResult[F, S, E]
//	}

	override def apply(form :SQLReadForm[X])    :SQLReadForm[Z]    = super[SQLFormConversion].apply(form)
	override def apply(form :ColumnReadForm[X]) :ColumnReadForm[Z] = super[SQLFormConversion].apply(form)
	override def apply(form :SQLForm[X])        :SQLForm[Z]        = super[SQLFormConversion].apply(form)
	override def apply(form :ColumnForm[X])     :ColumnForm[Z]     = super[SQLFormConversion].apply(form)

	protected[mechanics] override def split :Opt[(SQLAdaptation[X, Y], SQLAdaptation[Y, Z])] =
		Got((first, second))

	override lazy val swap :SQLAdaptation[Z, X] = {
		val firstSwap = first.swap
		new ArbitraryComposedAdaptation[Z, Y, X, firstSwap.type](second.swap, firstSwap) {
			override def isReversible = true
			override lazy val swap = ComposedAdaptation.this
			override def applyString(arg :String) = arg + ".inverse(" + swap + ")"
		}
	}
}


private class ArbitraryComposedAdaptation[X, Y, Z, T <: SQLAdaptation[Y, Z]]
                                         (override val first :SQLAdaptation[X, Y], override val second :T)
	extends ArbitraryComposedTransformation[X, Y, Z] with ComposedAdaptation[X, Y, Z]
{
	override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Z]] =
		ColumnSQL[F, S, Z]

	override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
	                   (expr :ColumnConvertingOps[F, S, X, E]) :ColumnResult[F, S, E[Z]] =
		second.column[F, S, SQLExpression.from[F]#rows[S]#C](first.column[F, S, E](expr))
}

private class ConversionComposedAdaptation[X, Y, Z, T <: SQLAdaptation[Y, Z]]
                                          (override val first :SQLConversion[X, Y], override val second :T)
	extends ConversionComposedTransformation[X, Y, Z, T](first, second) with ComposedAdaptation[X, Y, Z]
{
	override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Z]] =
		second.ColumnResult[F, S, E]

	override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
	                   (expr :ColumnConvertingOps[F, S, X, E]) :ColumnResult[F, S, E[Z]] =
		second.column[F, S, E](first(expr))


	protected[mechanics] override def splitConversion
			:Opt[(SQLConversion[X, Y], SQLAdaptation.Into[Y, Z, second.Expression])] =
		Got((first, second :SQLAdaptation.Into[Y, Z, second.Expression]))
}




/** A conversion of any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, X]` to
  * an `SQLExpression[F, S, Y]` which does not change the generated SQL.
  * If two types `X, Y` can be converted througn implicit conversions to a common type `Z`,
  * then an implicit [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]]`[X, Y]` exists,
  * witnessing that two expressions of these types are comparable in SQL.
  * As an `SQLConversion` does not influence the generated SQL itself, they may be freely composed with each other:
  * `expr.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingOps.to to]]`[X].to[Y]` will normally
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
//	override type BaseResult[-F <: RowProduct, -S >: Grouped <: Single] = SQLExpression[F, S, Y]
	override type Expression[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, Y]] = E
	override type ColumnResult[-F <: RowProduct, -S >: Grouped <: Single, +E <: ColumnSQL[F, S, Y]] = E
	override type QueryResult[P, +Q <: Query[P, Y]] = Q

	override def isUniversal      = true
	override def isTypeDependent  = true
	override def isSpecific       = false
	override def isSpecificColumn = false
	override def isColumnOnly     = false

	override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
	                  (expr :ConvertingOps[F, S, X, E]) :E[Y] =
		if (expr == null) null.asInstanceOf[E[Y]] else expr.`->convert`(this)

	override def column[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleColumn[F, S, v, E]]
	                   (expr :ColumnConvertingOps[F, S, X, E]) :E[Y] =
		apply(expr)

	override def apply[F <: RowProduct](query :QuerySQL[F, X]) :QuerySQL[F, Y] = query.rowsTo(this)
	override def apply[P](query :Query[P, X]) :Query[P, Y] = query.rowsTo(this)
	override def apply(query :TopSelectSQL[X]) :TopSelectSQL[Y] = query.rowsTo(this)
	def apply[Q[_]](query :QueryTemplate[X, Q]) :Q[Y] = query.rowsTo(this)

	override def compose[W](first :SQLConversion[W, X]) :SQLConversion[W, Y] =
		if (first.isIdentity) first andThen this
		else if (first == swap) SQLConversion.toSelf.castParams[W, Y]
		else new ComposedConversion[W, X, Y](first, this)

	override def andThen[Z](second :SQLTransformation[Y, Z]) :SQLTransformation.Into[X, Z, second.Expression] =
		second compose this

	//we lose ColumnResult type
	override def andThen[Z](second :SQLAdaptation[Y, Z]) :SQLAdaptation.Into[X, Z, second.Expression] =
		second compose this

	def andThen[Z](second :SQLConversion[Y, Z]) :SQLConversion[X, Z] = second compose this

	protected[mechanics] override def split :Opt[(SQLConversion[X, A], SQLConversion[A, Y]) forSome { type A }] = Lack

	protected[mechanics] override def splitConversion :Opt[(SQLConversion[X, A], SQLConversion[A, Y]) forSome { type A }] =
		split

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
//	                      E[v] <: SQLExpression[F, S, v] with ConvertingOps[F, S, v, E, E[v]]]
//			:SpecificConversion[X, Y, E] =
//		SpecificConversion[F, S, X, Y, E](this)
//
//	override def specific[F <: RowProduct, S >: Grouped <: Single,
//	                      E[v] <: SQLExpression[F, S, v] with ConvertingOps[F, S, v, E, E[v]]]
//	                     (e :ConvertingOps[F, S, X, E, E[X]]) :SpecificConversion[X, Y, E] =
//		specific[F, S, E]


	/** Combines this instance with another `SQLConversion` to the same type into an evidence value required
	  * by many methods of `SQLExpression`.
	  */
//	def vs[A](other :SQLConversion[A, Y]) :Interoperable[X, A]#As[Y] =
	def vs[A](other :SQLConversion[A, Y]) :Interoperable[X, A] { type Unified = Y } =
		if (other.isIdentity)
			(other vs this).swap
		else
			new StandardInteroperable(this, other) { outer =>
				override lazy val swap = new StandardInteroperable(right, left) {
					override val swap = outer
				}
			}

	/** Creates an evidence value that `X` and `Y` are interoperable in SQL. */
//	lazy val asLeft  :Interoperable[X, Y]#As[Y] =
	lazy val asLeft  :Interoperable[X, Y]{ type Unified = Y } =
		new StandardInteroperable(this, toSelf[Y]) {
			override lazy val swap = asRight
		}
	/** Creates an evidence value that `Y` and `X` are interoperable in SQL. */
//	lazy val asRight :Interoperable[Y, X]#As[Y] =
	lazy val asRight :Interoperable[Y, X] { type Unified = Y } =
		new StandardInteroperable(toSelf[Y], this) {
			override lazy val swap = asLeft
		}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLConversion[_, _]]
}





private[mechanics] sealed abstract class OptionConversions {
	implicit def toOption[T] :SQLConversion[T, Option[T]] = option
	protected def option[T]  :SQLConversion[T, Option[T]]
}

private[mechanics] sealed abstract class UnwrappingConversions extends OptionConversions {
	implicit def rowsToOne[T] :ReversibleConversion[Rows[T], T] = SQLConversion.selectRow[T]
	implicit def rowsToSeq[T] :ReversibleConversion[Rows[T], Seq[T]] = SQLConversion.selectRows[T]
	implicit def toParam[T]   :ReversibleConversion[Out[T], T] = SQLConversion.toOut[T].swap
	implicit def toEntryValue[K <: Label, V] :ReversibleConversion[K :~ V, V] = entryValue[K, V]

	protected def entryValue[K <: Label, V] :ReversibleConversion[K :~ V, V]
}

private[mechanics] sealed abstract class LiftedConversions extends UnwrappingConversions {
	implicit def chain[XI <: Chain, XL, YI <: Chain, YL](implicit init :SQLConversion[XI, YI], last :SQLConversion[XL, YL])
			:SQLConversion[XI ~ XL, YI ~ YL] =
		if (init.isIdentity && last.isIdentity)
			SQLConversion.toSelf[XI ~ XL].castParam2[YI ~ YL]
		else
			new ChainConversion(init, last)

	implicit def record[XI <: Listing, XL, YI <: Listing, YL, K <: Label]
	                   (implicit init :SQLConversion[XI, YI], last :SQLConversion[XL, YL])
			:SQLConversion[XI |~ (K :~ XL), YI |~ (K :~ YL)] =
		if (init.isIdentity && last.isIdentity)
			SQLConversion.toSelf[XI |~ (K :~ XL)].castParam2[YI |~ (K :~ YL)]
		else
			new RecordConversion[XI, XL, YI, YL, K](init, last)

	implicit def convertedRows[X, Y](implicit item :SQLConversion[X, Y]) :SQLConversion[Rows[X], Rows[Y]] =
		if (item.isIdentity) item.castParams[Rows[X], Rows[Y]]
		else new RowsConversion[X, Y](item)

	implicit def convertedSeq[X, Y](implicit item :SQLConversion[X, Y]) :SQLConversion[Seq[X], Seq[Y]] =
		if (item.isIdentity) SQLConversion.toSelf[Seq[X]].castParam2[Seq[Y]]
		else new SeqConversion[X, Y](item)

	implicit def reversibleRows[X, Y](implicit item :ReversibleConversion[X, Y]) :ReversibleConversion[Rows[X], Rows[Y]] =
		if (item.isIdentity) SQLConversion.toSelf[Rows[X]].castParam2[Rows[Y]]
		else new RowsConversion[X, Y](item) with ReversibleConversion[Rows[X], Rows[Y]]

	implicit def reversibleSeq[X, Y](implicit item :ReversibleConversion[X, Y])  :ReversibleConversion[Seq[X], Seq[Y]] =
		if (item.isIdentity) SQLConversion.toSelf[Seq[X]].castParam2[Seq[Y]]
		else new SeqConversion[X, Y](item) with ReversibleConversion[Seq[X], Seq[Y]]

	implicit def convertedParam[X, Y](implicit value :SQLConversion[X, Y]) :SQLConversion[Out[X], Out[Y]] =
		if (value.isIdentity)
			SQLConversion.toSelf[Out[X]].castParam2[Out[Y]]
		else if (value.isReversible)
			SQLConversion(".map(" + value + ")", (_ :Out[X]).map(value.apply), (_ :Out[Y]).map(value.inverse))
		else
			SQLConversion.opt(".map(" + value + ")",
				(_ :Out[X]).map(value.apply),
				(y :Out[Y]) => value.unapply(y).map(Out.apply)
			)

	implicit def convertedEntry[K <: Label, X, Y](implicit value :SQLConversion[X, Y]) :SQLConversion[K :~ X, K :~ Y] =
		if (value.isIdentity)
			SQLConversion.toSelf[K :~ X].castParam2[K :~ Y]
		else
			new RecordConversionEntry(value)

	implicit def reversibleEntry[K <: Label, X, Y](implicit value :ReversibleConversion[X, Y])
			:ReversibleConversion[K :~ X, K :~ Y] =
		if (value.isIdentity)
			SQLConversion.toSelf[K :~ X].castParam2[K :~ Y]
		else
			new RecordConversionEntry[K, X, Y](value) with ReversibleConversion[K :~ X, K :~ Y]
}

private[mechanics] sealed abstract class RecordConversions extends LiftedConversions {
	implicit def recordToTuple[LI <: Listing, CI <: Chain, K <: Label, L]
	                          (implicit init :Equivalent[LI, CI]) :Equivalent[LI |~ (K :~ L), CI ~ L] =
		new ListingToChain(init)

	implicit def reorder[X <: Listing, XKs <: Chain, Y <: Listing, YKs <: Chain]
	                    (implicit there :(X ProjectRecord YKs) { type Out = Y },
	                     back  :(Y ProjectRecord XKs) { type Out = X })
//			:RecordReordering[X, Y] { type XKeys = XKs; type YKeys = YKs } =
			:Equivalent[X, Y] =
		RecordReordering(there, back)
}

private[mechanics] sealed abstract class IdentityConversions extends RecordConversions {
	//Apparently equiv is not picked up for some of more complex types.
	implicit def identity[X] :Equivalent[X, X] = SQLConversion.toSelf
	//We need this one in ColumnSQL methods which take V=:=Boolean and similar.
	implicit def equiv[X, Y](implicit ev :X =:= Y) :ReversibleConversion[X, Y] =
		SQLConversion.toSelf[X].castParam2[Y]
}


private[mechanics] sealed abstract class ConversionsToDouble extends IdentityConversions {
	implicit object ByteToDouble extends Widening[Byte, Double] {
		override def apply(value :Byte) :Double = value
		override def inverse(value :Double) :Byte = value.toByte
		override val swap :ReversibleConversion[Double, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Byte.toDouble"
	}
	implicit object ShortToDouble extends Widening[Short, Double] {
		override def apply(value :Short) :Double = value
		override def inverse(value :Double) :Short = value.toShort
		override val swap :ReversibleConversion[Double, Short] = swap("", ".toShort")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Short.toDouble"
	}
	implicit object CharToDouble extends Widening[Char, Double] {
		override def apply(value :Char) :Double = value
		override def inverse(value :Double) :Char = value.toChar
		override val swap :ReversibleConversion[Double, Char] = swap("", ".toChar")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Char.toDouble"
	}
	implicit object IntToDouble extends Widening[Int, Double] {
		override def apply(value :Int) :Double = value
		override def inverse(value :Double) :Int = value.toInt
		override val swap :ReversibleConversion[Double, Int] = swap("", ".toInt")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Int.toDouble"
	}
	implicit object LongToDouble extends Widening[Long, Double] {
		override def apply(value :Long) :Double = value.toDouble
		override def inverse(value :Double) :Long = value.toLong
		override val swap :ReversibleConversion[Double, Long] = swap("", ".toLong")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Long.toDouble"
	}
	implicit object FloatToDouble extends Widening[Float, Double] {
		override def apply(value :Float) :Double = value
		override def inverse(value :Double) :Float = value.toFloat
		override val swap :ReversibleConversion[Double, Float] = swap("", ".toFloat")
		override def applyString(arg :String) :String = arg + ".toDouble"
		override def toString = "Float.toDouble"
	}
}

private[mechanics] sealed abstract class ConversionsToFloat extends ConversionsToDouble {
	implicit object ByteToFloat extends Widening[Byte, Float] {
		override def apply(value :Byte) :Float = value
		override def inverse(value :Float) :Byte = value.toByte
		override val swap :ReversibleConversion[Float, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Byte.toFloat"
	}
	implicit object ShortToFloat extends Widening[Short, Float] {
		override def apply(value :Short) :Float = value
		override def inverse(value :Float) :Short = value.toShort
		override val swap :ReversibleConversion[Float, Short] = swap("", ".toShort")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Short.toFloat"
	}
	implicit object CharToFloat extends Widening[Char, Float] {
		override def apply(value :Char) :Float = value
		override def inverse(value :Float) :Char = value.toChar
		override val swap :ReversibleConversion[Float, Char] = swap("", ".toChar")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Char.toFloat"
	}
	implicit object IntToFloat extends Widening[Int, Float] {
		override def apply(value :Int) :Float = value.toFloat
		override def inverse(value :Float) :Int = value.toInt
		override val swap :ReversibleConversion[Float, Int] = swap("", ".toInt")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Int.toFloat"
	}
	implicit object LongToFloat extends Widening[Long, Float] {
		override def apply(value :Long) :Float = value.toFloat
		override def inverse(value :Float) :Long = value.toLong
		override val swap :ReversibleConversion[Float, Long] = swap("", ".toLong")
		override def applyString(arg :String) :String = arg + ".toFloat"
		override def toString = "Long.toFloat"
	}
}

private[mechanics] sealed abstract class ConversionsToLong extends ConversionsToFloat {
	implicit object ByteToLong extends Widening[Byte, Long] {
		override def apply(value :Byte) :Long = value
		override def inverse(value :Long) :Byte = value.toByte
		override val swap :ReversibleConversion[Long, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toLong"
		override def toString = "Byte.toLong"
	}
	implicit object ShortToLong extends Widening[Short, Long] {
		override def apply(value :Short) :Long = value
		override def inverse(value :Long) :Short = value.toShort
		override val swap :ReversibleConversion[Long, Short] = swap("", ".toShort")
		override def applyString(arg :String) :String = arg + ".toLong"
		override def toString = "Short.toLong"
	}
	implicit object CharToLong extends Widening[Char, Long] {
		override def apply(value :Char) :Long = value
		override def inverse(value :Long) :Char = value.toChar
		override val swap :ReversibleConversion[Long, Char] = swap("", ".toChar")
		override def applyString(arg :String) :String = arg + ".toLong"
		override def toString = "Char.toLong"
	}
	implicit object IntToLong extends Widening[Int, Long] {
		override def apply(value :Int) :Long = value
		override def inverse(value :Long) :Int = value.toInt
		override val swap :ReversibleConversion[Long, Int] = swap("", ".toInt")
		override def applyString(arg :String) :String = arg + ".toLong"
		override def toString = "Int.toLong"
	}
}

private[mechanics] sealed abstract class ConversionsToInt extends ConversionsToLong {
		implicit object ByteToInt extends Widening[Byte, Int] {
		override def apply(value :Byte) :Int = value
		override def inverse(value :Int) :Byte = value.toByte
		override val swap :ReversibleConversion[Int, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toInt"
		override def toString = "Byte.toInt"
	}
	implicit object ShortToInt extends Widening[Short, Int] {
		override def apply(value :Short) :Int = value
		override def inverse(value :Int) :Short = value.toShort
		override val swap :ReversibleConversion[Int, Short] = swap("", ".toShort")
		override def applyString(arg :String) :String = arg + ".toInt"
		override def toString = "Short.toInt"
	}
	implicit object CharToInt extends Widening[Char, Int] {
		override def apply(value :Char) :Int = value
		override def inverse(value :Int) :Char = value.toChar
		override val swap :ReversibleConversion[Int, Char] = swap("", ".toChar")
		override def applyString(arg :String) :String = arg + ".toInt"
		override def toString = "Char.toInt"
	}
}

private[mechanics] sealed abstract class ConversionsToShort extends ConversionsToInt {
	implicit object ByteToShort extends Widening[Byte, Short] {
		override def apply(value :Byte) :Short = value
		override def inverse(value :Short) :Byte = value.toByte
		override val swap :SQLConversion[Short, Byte] = swap("", ".toByte")
		override def applyString(arg :String) :String = arg + ".toShort"
	}
}



object SQLConversion extends ConversionsToShort {

	def apply[X, Y](suffix :String, convert :X => Y) :SQLConversion[X, Y] =
		new Impl[X, Y](suffix, convert)

	def apply[X, Y](suffix :String, convert :X => Y, inversed :Y => X) :ReversibleConversion[X, Y] =
		new ReversibleImpl(suffix, convert, inversed)

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
	private class ReversibleImpl[X, Y](suffix :String, convert :X => Y, inversed :Y => X)
		extends Impl[X, Y](suffix, convert) with ReversibleConversion[X, Y]
	{
		override def unapply(value :Y) = Got(inversed(value))
		override def inverse(value :Y) = inversed(value)
	}


//	/** Matches an `SQLConversion`, extracting also an identity transformation which preserves the type of expressions
//	  * created by the original.
//	  */
//	def unapply[X, Y](transformation :SQLTransformation[X, Y])
//			:Opt[(SQLConversion[X, Y], SQLTransformation.Into[Y, Y, transformation.Expression])] =
//		transformation match {
//			case conversion :SQLConversion[X, Y] => Got((
//				conversion, toSelf[Y].castFrom[SQLConversion[Y, Y],
//				SQLTransformation.Into[Y, Y, transformation.Expression]]
//			))
//			case _ => Lack
//		}



	object Composition {
		def unapply[X, Z](e :SQLConversion[X, Z])
				:Opt[(SQLConversion[X, Y], SQLConversion[Y, Z]) forSome { type Y }] =
			e.split
	}
	object ReadForm {
		def unapply[X](form :SQLReadForm[X]) :Opt[SQLConversion[_, _ <: X]] = form match {
			case SQLFormConversion.ReadForm(conversion :SQLConversion[_, X] @unchecked) => Got(conversion)
			case _ => Lack
		}
	}
	object Form {
		def unapply[X](form :SQLForm[X]) :Opt[SQLConversion[_, X]] = form match {
			case SQLFormConversion.Form(conversion :SQLConversion[_, X] @unchecked) => Got(conversion)
			case _ => Lack
		}
	}

//	implicit def toParam[T]    :Equivalent[Out[T], T] = toOut[T].swap

	//inverses can't be implicits or we'd have an implicit conflict
	def toSelf[T] :Equivalent[T, T] = IdentityConversion.castParams[T, T]
//	def equiv[X, Y](implicit ev :X =:= Y)  :Equivalent[X, Y] = toSelf[X].castParam2[Y]
	def label[K <: Label, V] :Equivalent[V, K :~ V] = entryValue[K, V].swap
	def toOut[T] :Equivalent[T, Out[T]] = OutParam.castParams[T, Out[T]] //it's better to unpack by default
	def forceListing[X <: Chain, Y <: Listing](implicit listingToChain :Equivalent[Y, X]) :Equivalent[X, Y] =
		listingToChain.swap

	def selectRow[T]  :Equivalent[Rows[T], T]      = SelectRow.castParams[Rows[T], T]
	def selectRows[T] :Equivalent[Rows[T], Seq[T]] = SelectRows.castParams[Rows[T], Seq[T]]
	def toRow[T]      :SQLTransformation[T, Rows[T]]      = selectRow[T].swap  //unlikely to ever see use, but we won't be able
	def toRows[T]     :SQLTransformation[Seq[T], Rows[T]] = selectRows[T].swap //  to handle custom implementations when needed

	/** An unsafe [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]] for representing
	  * an SQL expression as one for the supertype of its value type parameter. It is inherently irreversible,
	  * meaning it can't be used to unify two SQL expressions for use within a comparison or assignment.
	  * It is reserved for use cases where the adapted expression is guaranteed to be used only in a covariant manner.
	  */
	def supertype[X, Y](implicit tag :ClassTag[X], subtype :X <:< Y) :SQLConversion[X, Y] =
		new Upcast[X, Y](tag, subtype)

	def subtype[X, Y](implicit tag :ClassTag[Y], subtype :Y <:< X) :SQLConversion[X, Y] =
		supertype[Y, X].swap

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

	case class RowsConversion[X, Y](item :SQLConversion[X, Y]) extends SQLConversion[Rows[X], Rows[Y]] {
		override def isReversible :Boolean = item.isReversible
		override def isLossless   :Boolean = item.isLossless
		override def isValueIdentity  :Boolean = item.isValueIdentity

		override def apply(value :Rows[X]) :Rows[Y] = value.map(item.apply)
		override def inverse(value :Rows[Y]) :Rows[X] = value.map(item.inverse)

		override def unapply(value :Rows[Y]) :Opt[Rows[X]] =
			(Opt(Seq.newBuilder[X]) /: value.seq) {
				(build, y) => item.unapply(y).flatMap { x => if (build.nonEmpty) build.get += x; build }
			}.map(b => Rows(b.result():_*))


		override lazy val swap :SQLConversion[Rows[Y], Rows[X]] =
			new RowsConversion(item.swap) with ReversibleConversion[Rows[Y], Rows[X]] {
				override lazy val swap = RowsConversion.this
			}
//		override def equals(that :Any) :Boolean = that match {
//			case other :RowsConversion[_, _] => item == other.item
//			case _ => false
//		}
//		override def hashCode = item.hashCode
		override def applyString(arg :String) :String = item.applyString(arg + ".map(_") + ")"
	}
//
//	object RowsConversion {
//		def unapply[X, Y](conversion :SQLTransformation[X, Y])
//				:Opt[(SQLConversion[A, B], X =:= Rows[A], Y =:= Rows[B]) forSome { type A; type B }] =
//			conversion match {
//				case rows :RowsConversion[a, b] =>
//					val ev = implicitly[X =:= X]
//					Got((rows.item, ev.asInstanceOf[X=:=Rows[a]], ev.asInstanceOf[Y=:=Rows[b]]))
//				case _ => Lack
//			}
//	}

	case class SeqConversion[X, Y](item :SQLConversion[X, Y]) extends SQLConversion[Seq[X], Seq[Y]] {
		override def isReversible :Boolean = item.isReversible
		override def isLossless   :Boolean = item.isLossless
		override def isValueIdentity  :Boolean = item.isValueIdentity

		override def apply(value :Seq[X]) :Seq[Y] = value.map(item.apply)
		override def inverse(value :Seq[Y]) :Seq[X] = value.map(item.inverse)
		override def unapply(value :Seq[Y]) :Opt[Seq[X]] =
			(Opt(value.iterableFactory.newBuilder[X]) /: value) {
				(acc, y) => item.unapply(y).flatMap { x => acc foreach { _ += x }; acc }
			}.map(_.result())


		override lazy val swap :SQLConversion[Seq[Y], Seq[X]] =
			new SeqConversion(item.swap) with ReversibleConversion[Seq[Y], Seq[X]] {
				override lazy val swap = SeqConversion.this
			}

//		override def equals(that :Any) :Boolean = that match {
//			case other :SeqConversion[_, _] => item == other.item
//			case _ => false
//		}
//		override def hashCode :Int = item.hashCode
		override def applyString(arg :String) :String = item.applyString(arg + ".map{_") + "}"
	}
//
//	object SeqConversion {
//		def unapply[X, Y](conversion :SQLTransformation[X, Y])
//				:Opt[(SQLConversion[A, B], X =:= Seq[A], Y =:= Seq[B]) forSome { type A; type B }] =
//			conversion match {
//				case seq :SeqConversion[a, b] =>
//					val ev = implicitly[X =:= X]
//					Got((seq.item, ev.asInstanceOf[X =:= Seq[a]], ev.asInstanceOf[Y =:= Seq[b]]))
//				case _ => Lack
//			}
//	}


	//todo: make this one and other case classes private, with unapply instead
	case class ChainConversion[XI <: Chain, XL, YI <: Chain, YL](init :SQLConversion[XI, YI], last :SQLConversion[XL, YL])
		extends SQLConversion[XI ~ XL, YI ~ YL]
	{
		override def isReversible :Boolean = init.isReversible && last.isReversible
		override def isLossless   :Boolean = init.isLossless && last.isLossless
		override def isValueIdentity  :Boolean = init.isValueIdentity && last.isValueIdentity

		override def apply(value :XI ~ XL) :YI ~ YL = init(value.init) ~ last(value.last)
		override def inverse(value :YI ~ YL) :XI ~ XL = init.inverse(value.init) ~ last.inverse(value.last)
		override def unapply(value :YI ~ YL) :Opt[XI ~ XL] =
			for (i <- init.unapply(value.init); l <- last.unapply(value.last)) yield i ~ l

//		override def isDerived :Boolean = false
//		override def isUpcast     :Boolean = last.isUpcast && init.isUpcast

		override lazy val swap :SQLConversion[YI ~ YL, XI ~ XL] =
			new ChainConversion[YI, YL, XI, XL](init.swap, last.swap) with ReversibleConversion[YI ~ YL, XI ~ XL] {
				override lazy val swap = ChainConversion.this
			}
		def deep :SQLTransformation[XI ~ XL, YI ~ YL] = ChainTransformation(
			init match {
				case chain :ChainConversion[_, _, _, _] => chain.deep.castParams[XI, YI]
				case _ => init
			}, last
		)
//		override def equals(that :Any) :Boolean = that match {
//			case self :AnyRef if this eq self => true
//			case other :ChainConversion[_, _, _, _] if other canEqual this => last == other.last && init == other.init
//			case _ => false
//		}
//		override def canEqual(that :Any) = that.isInstanceOf[ChainConversion[_, _, _, _]]
//		override def hashCode :Int = init.hashCode * 31 + last.hashCode
		override def applyString(arg :String) :String = init.applyString(arg) + "~(" + last + ")"
	}
//
//	object ChainConversion {
//		def apply[XI <: Chain, XL, YI <: Chain, YL]
//		         (init :SQLConversion[XI, YI], last :SQLConversion[XL, YL]) :SQLConversion[XI ~ XL, YI ~ YL] =
//			new ChainConversion(init, last)
//
//		object Split {
//			def unapply[XI <: Chain, XL, Y](conversion :SQLTransformation[XI ~ XL, Y])
//					:Opt[(SQLConversion[XI, YI], SQLConversion[XL, YL],
//					      SQLAdaptation[YI ~ YL, Y]#As[conversion.SQLResult]) forSome { type YI <: Chain; type YL }
//					] =
//				conversion match {
//					case chain :ChainConversion[XI, XL, yi, yl] =>
//						Got(chain.init, chain.last, toSelf.asInstanceOf[SQLAdaptation[yi ~ yl, Y]#As[conversion.SQLResult]])
//					case _ => Lack
//				}
//
//		}
//	}

	private[sql] class ChainReordering[X <: Chain, Y <: Chain]
	                                  (val permutation :IndexedSeq[Int], val inverse :IndexedSeq[Int])
		extends Equivalent[X, Y]
	{
		def this(permutation :IndexedSeq[Int]) = this(permutation, inversePermutation(permutation))

		override def apply(value :X)   :Y = build(@~, 0, inverse.map(value.toIndexedSeq)).castFrom[Chain, Y]
		override def inverse(value :Y) :X = build(@~, 0, permutation.map(value.toIndexedSeq)).castFrom[Chain, X]

		@tailrec private def build(x :Chain, i :Int, values :IndexedSeq[Any]) :Chain =
			if (i >= values.length) x
			else build(x ~ values(i), i + 1, values)

		override lazy val swap :Equivalent[Y, X] = new ChainReordering[Y, X](inverse, permutation) {
			override lazy val swap = ChainReordering.this
		}
		override def applyString(arg :String) = permutation.mkString(arg + ".reorder(", ", ", ")")
	}


	/** An element-by-element conversion of a `Listing`, applying separate `SQLConversion` instances to each value. */
	case class RecordConversion[XI <: Listing, XL, YI <: Listing, YL, K <: Label]
	                           (init :SQLConversion[XI, YI], last :SQLConversion[XL, YL])
		extends SQLConversion[XI |~ (K :~ XL), YI |~ (K :~ YL)]
	{
		override def isReversible :Boolean = init.isReversible && last.isReversible
		override def isLossless   :Boolean = init.isLossless && last.isLossless
		override def isValueIdentity  :Boolean = init.isValueIdentity && last.isValueIdentity

		override def apply(value :XI |~ (K :~ XL)) :YI |~ (K :~ YL) =
			init(value.init) |~ :~[K](last(value.last.value))

		override def inverse(value :YI |~ (K :~ YL)) :XI |~ (K :~ XL) =
			init.inverse(value.init) |~ :~[K](last.inverse(value.last.value))

		override def unapply(value :YI |~ (K :~ YL)) :Opt[XI |~ (K :~ XL)] =
			for (i <- init.unapply(value.init); l <- last.unapply(value.last.value)) yield i |~ :~[K](l)

		override lazy val swap :SQLConversion[YI |~ (K :~ YL), XI |~ (K :~ XL)] =
			new RecordConversion[YI, YL, XI, XL, K](init.swap, last.swap)
				with ReversibleConversion[YI |~ (K :~ YL), XI |~ (K :~ XL)]
			{
				override lazy val swap = RecordConversion.this
			}

		override def applyString(arg :String) :String = {
			def rec(conv :SQLConversion[_, _]) :String = conv match {
				case listing :RecordConversion[_, _, _, _, _] => rec(listing.init) + "|~" + listing.last
				case _ if conv == toSelf => arg + ".map(@~"
				case _ => conv.applyString(arg) + ".mapSuffix("
			}
			rec(this) + ")"
		}
	}

/*      //The problem with creating a proper conversion to IndexedSQL is that we can't really tell if a value is a column,
	//  as otherwise (if it isn't a tuple either), no IndexedSQL is possible.
	case class DeepChainToListing[CI <: Chain, LI <: Listing, K <: Label, L] private[Lift] (init :Lift[CI, LI], key :K)
		extends ChainToListing[CI, LI, K, L](init)
	{
		//			override def apply(form :SQLReadForm[CI ~ L]) :SQLReadForm[LI |~ (K :~ L)] =
		//				form.asInstanceOf[SQLReadForm[LI |~ (K :~ L)]]
		override def apply[F <: RowProduct, S >: Grouped <: Single]
		                  (expr :SQLExpression[F, S, CI ~ L]) :SQLExpression[F, S, LI |~ (K :~ L)] =
			expr match {
				case ChainSQL(init, last) => this.init(init) match {
					case EmptyChain => EmptyIndex |~
					case listing :IndexedSQL[F, S, LI] =>
				}
			}

		override def isStandard = false

		override def applyString(arg :String) :String = init.applyString(arg) + "|~" + key + ":~_"
	}
*/


	private sealed abstract class RecordReordering[X <: Listing, Y <: Listing]
		extends Equivalent[X, Y] with TransformRecord[X, Y]
	{ outer =>
		override type Out = Y
//		override type OutSQL[-F <: RowProduct, -S >: Grouped <: Single] = IndexedSQL[F, S, Y]
		type XKeys <: Chain
		type YKeys <: Chain
		type OfKeys[XKs <: Chain, YKs <: Chain] = RecordReordering[X, Y] { type XKeys = XKs; type YKeys = YKs }
		val there :ProjectRecord[X, YKeys]#To[Y]
		val back  :ProjectRecord[Y, XKeys]#To[X]

		override def apply(value :X) :Y = there(value)
		override def inverse(value :Y) :X = back(value)

//		override def apply[F <: RowProduct, S >: Grouped <: Single]
//		                  (record :LabeledValueSQL[F, S, X]) :IndexedSQL[F, S, Y] =
//			there(record)

		override def apply[F <: RowProduct, S >: Grouped <: Single](record :RecordSQL[F, S, X]) :RecordSQL[F, S, Y] =
			there(record)

		override def apply[F <: RowProduct, S >: Grouped <: Single](record :IndexedSQL[F, S, X]) :IndexedSQL[F, S, Y] =
			there(record)

		override lazy val swap :RecordReordering.FullType[Y, YKeys, X, XKeys] =
			new RecordReordering[Y, X] {
				override type XKeys = outer.YKeys
				override type YKeys = outer.XKeys
				override val there  = outer.back
				override val back   = outer.there
				override lazy val swap :RecordReordering.FullType[X, outer.XKeys, Y, outer.YKeys] = outer
			}

		override def andThen[Z](second :SQLConversion[Y, Z]) :SQLConversion[X, Z] = second match {
			case other :RecordReordering[Y @unchecked, Listing @unchecked] =>
				RecordReordering(there andThen other.there, other.back andThen back).asInstanceOf[SQLConversion[X, Z]]
			case _ => super.andThen(second)
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :RecordReordering[_, _] =>
				there.toString == other.there.toString && back.toString == other.back.toString
			case _ => false
		}
		override def hashCode :Int = there.toString.hashCode * 31 + back.toString.hashCode

		override def applyString(arg :String) :String = arg + ".reorder(" + there + ")"
	}

	object RecordReordering {
		private type FullType[X <: Listing, XKs <: Chain, Y <: Listing, YKs <: Chain] =
			RecordReordering[X, Y] { type XKeys = XKs; type YKeys = YKs }

		def apply[X <: Listing, XKs <: Chain, Y <: Listing, YKs <: Chain]
		         (map :ProjectRecord[X, YKs] { type Out = Y }, unmap :ProjectRecord[Y, XKs] { type Out = X })
//				:RecordReordering[X, Y] { type XKeys = XKs; type YKeys = YKs } =
				:Equivalent[X, Y] =
			new RecordReordering[X, Y] {
				override type XKeys = XKs
				override type YKeys = YKs
				override val there = map
				override val back  = unmap
			}

		def unapply[X <: Listing, Y <: Listing](conversion :SQLConversion[X, Y])
				:Opt[(ProjectRecord[X, _] { type Out = Y }, ProjectRecord[Y, _] { type Out = X })] =
			conversion match {
				case reorder :RecordReordering[X, Y] => Got((reorder.there, reorder.back))
				case _ => Lack
			}
	}
//
//	sealed class RecordReordering[X <: Listing, Y <: Listing](val there :Y SublistingOf X, val back :X SublistingOf Y)
//		extends Equivalent[X, Y]
//	{
//		override def apply(value :X) :Y = there(value)
//		override def inverse(value :Y) :X = back(value)
//
//		override lazy val swap :RecordReordering[Y, X] =
//			new RecordReordering(back, there) with ReversibleConversion[Y, X] {
//				override lazy val swap = RecordReordering.this
//			}
//
//		override def andThen[Z](second :SQLConversion[Y, Z]) :SQLConversion[X, Z] = second match {
//			case other :RecordReordering[Y @unchecked, Listing @unchecked] =>
//				new RecordReordering(other.there compose there, back compose other.back).asInstanceOf[SQLConversion[X, Z]]
//			case _ => super.andThen(second)
//		}
//		override def equals(that :Any) :Boolean = that match {
//			case self  :AnyRef if this eq self => true
//			case other :RecordReordering[_, _] =>
//				there.toString == other.there.toString && back.toString == other.back.toString
//			case _ => false
//		}
//		override def hashCode :Int = there.toString.hashCode * 31 + back.toString.hashCode
//		override def applyString(arg :String) :String = arg + ".reorder(" + there + ")"
//	}

	//A workaround for the 'bridge gnerated for member method clashes with the definition of the member itself'.
	private[sql] abstract class MappedConversion[A, B, X, Y]
	                                            (convert :SQLConversion[A, B], unpack :X => A, pack :B => Y,
	                                             inverseUnpack :Y => B, inversePack :A => X)
		extends SQLConversion[X, Y]
	{
		override def apply(value :X)   :Y      = pack(convert(unpack(value)))
		override def inverse(value :Y) :X      = inversePack(convert.inverse(inverseUnpack(value)))
		override def unapply(value :Y) :Opt[X] = convert.unapply(inverseUnpack(value)).map(inversePack)
	}


	//todo: make private, and expose only factory/match pattern objects
	private[sql] case class RecordConversionEntry[K <: Label, X, Y](value :SQLConversion[X, Y])
		extends MappedConversion[X, Y, K :~ X, K :~ Y](value, (_ :(K :~ X)).value, :~[K](_ :Y),
		                                               (_ :(K :~ Y)).value, :~[K](_ :X))
	{
		override def applyString(arg :String) = ".value(" + value + ")"
	}

	private[sql] case class ChainToListing[CI <: Chain, LI <: Listing, K <: Label, L](init :Equivalent[CI, LI])
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

	private[sql] case class ListingToChain[LI <: Listing, CI <: Chain, K <: Label, L](init :Equivalent[LI, CI])
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
		                  (expr: ConvertingOps[F, S, Any, E]): E[Option[Any]] =
			if (expr == null) null.asInstanceOf[E[Option[Any]]] else expr.opt

		override lazy val swap = swap("", ".get")

		override def applyString(arg :String) :String = arg + ".toOption"
	}

	protected override def entryValue[K <: Label, V] :Equivalent[K :~ V, V] =
		ConvertLabel.castFrom[Equivalent[Label :~ Any, Any], Equivalent[K :~ V, V]] //asInstanceOf[Equiv[K :~ V, V]]

	private[this] object ConvertLabel
		extends ReversibleImpl[Label :~ Any, Any](".value", (_:(Label :~ Any)).value, :~[Label](_:Any))
		   with Equivalent[Label :~ Any, Any]
	{
		override val swap = swap("", ".:~")
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

	private[this] object IdentityConversion extends Equivalent[Any, Any] with SQLFormIdentity[Any] {
		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
		                  (expr :ConvertingOps[F, S, Any, E]) =
			if (expr == null) null.asInstanceOf[E[Any]] else expr.toConvertibleSQL
//		override def apply[F <: RowProduct, S >: Grouped <: Single, E[v] <: ConvertibleSQL[F, S, v, E]]
//		                  (expr :ConvertibleSQL[F, S, Any, E]) = expr

		override def apply[F <: RowProduct](query :QuerySQL[F, Any]) = query
		override def apply[P](query :Query[P, Any]) = query
		override def apply(query :TopSelectSQL[Any]) = query
		override def apply[Q[_]](query :QueryTemplate[Any, Q]) = query.supertyped

		override def compose[W](first :SQLTransformation[W, Any]) :first.type = first
		override def compose[W](first :SQLAdaptation[W, Any]) :first.type = first
		override def compose[W](first :SQLConversion[W, Any]) :first.type = first
		override def compose[W](first :ReversibleConversion[W, Any]) :first.type = first
		override def andThen[Z](second :SQLTransformation[Any, Z]) :second.type = second
		override def andThen[Z](second :SQLAdaptation[Any, Z]) :second.type = second
		override def andThen[Z](second :SQLConversion[Any, Z]) :second.type = second
		override def andThen[Z](second :ReversibleConversion[Any, Z]) :second.type = second

		override val swap :this.type = this

//		override def vs[A](other :SQLConversion[A, Any]) :Interoperable[Any, A]#As[Any] = other.asRight
		override def vs[A](other :SQLConversion[A, Any]) :Interoperable[Any, A] { type Unified = Any } = other.asRight
//		override def =~=[A](other :ReversibleConversion[A, Any]) :(Any =~= A)#Widen[Any] = other.asRight
		override def =~=[A](other :ReversibleConversion[A, Any]) :(Any =~= A) { type Unified = Any } = other.asRight
		override lazy val asLeft  = Interoperable.identity
		override lazy val asRight = Interoperable.identity

		override def isIdentity = true

//		override def specific[F <: RowProduct, S >: Grouped <: Single,
//		                      E[v] <: SQLExpression[F, S, v] with ConvertingOps[F, S, v, E, E[v]]] =
//			SpecificConversion.identity

		override def applyString(arg :String) = arg
	}

}



private class ComposedConversion[X, Y, Z](override val first  :SQLConversion[X, Y],
                                          override val second :SQLConversion[Y, Z])
	extends ComposedAdaptation[X, Y, Z] with SQLConversion[X, Z]
{
	protected[mechanics] override def split :Opt[(SQLConversion[X, Y], SQLConversion[Y, Z])] = Got((first, second))

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
					Got((first, second))
			}
		}

	def andThen[Z](conversion :ReversibleConversion[Y, Z]) :ReversibleConversion[X, Z] =
		conversion compose this
//
//	protected[mechanics] override def split
//			:Opt[(ReversibleConversion[X, A], ReversibleConversion[A, Y]) forSome { type A }] =
//		Lack
//
//	protected[mechanics] override def splitConversion
//			:Opt[(ReversibleConversion[X, A], ReversibleConversion[A, Y]) forSome { type A }] =
//		split

//	override def vs[A](other :SQLConversion[A, Y]) :Interoperable[X, A]#As[Y] =
	override def vs[A](other :SQLConversion[A, Y]) :Interoperable[X, A] { type Unified = Y } =
		other match {
			case reversible :ReversibleConversion[A, Y] => this =~= reversible
			case _ => super.vs(other)
		}

	/** Combines this instance with another `ReversibleConversion` to the same type into an evidence value required
	  *  by many methods of `SQLExpression`.
	  */
//	def =~=[A](other :ReversibleConversion[A, Y]) :(X =~= A)#Widen[Y] =
	def =~=[A](other :ReversibleConversion[A, Y]) :(X =~= A) { type Unified = Y } =
		if (other.isIdentity)
			(other =~= this).swap
		else
			new Equivalence(this, other) { outer =>
				override lazy val swap = new Equivalence(right, left) {
					override val swap = outer
				}
			}

//	override lazy val asLeft :(X =~= Y)#Widen[Y] =
	override lazy val asLeft :(X =~= Y) { type Unified = Y } =
		new Equivalence(this, SQLConversion.toSelf[Y]) {
			override lazy val swap = left.asRight
		}
//	override lazy val asRight :(Y =~= X)#Widen[Y] =
	override lazy val asRight :(Y =~= X) { type Unified = Y } =
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


	/** Base trait for standard type widening/promotions. Exists primarily to influence implicit precedence
	  * by declarations which are not supertypes of [[net.noresttherein.oldsql.sql.mechanics.Equivalent Equivalent]].
	  */
	trait Widening[X, Y] extends ReversibleConversion[X, Y]
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

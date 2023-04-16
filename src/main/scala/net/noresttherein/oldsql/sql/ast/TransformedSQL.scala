package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Opt, PassedArray}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, MisalignedExpressionException, MismatchedExpressionsException, UndefinedShapeException}
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.ColumnAt
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.slang.{classNameMethods, saferCasting}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ColumnConvertingOps, ColumnConvertingTemplate, ColumnGroundingOps, ConvertibleColumn, SpecificColumnVisitor, VariantColumnGroundingTemplate}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, BaseSQLOps, ConvertibleSQL, ConvertingOps, ConvertingTemplate, GroundingOps, Grouped, Single, SpecificExpressionVisitor, SpecificSQL, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL.{AdaptedColumnTemplate, AnyAdaptedColumnVisitor, SpecificAdaptedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.AdaptedSQL.{AdaptedSQLTemplate, AnyAdaptedVisitor, CaseAnyAdapted, CaseSpecificAdapted, SpecificAdaptedVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.ConvertedColumnMappingSQL
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.ConvertedColumnMappingSQL.{AnyConvertedColumnMappingVisitor, CaseAnyConvertedColumnMapping, CaseSpecificConvertedColumnMapping, SpecificConvertedColumnMappingVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.{UnaryCompositeColumn, UnaryCompositeColumnTemplate}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.{UnaryCompositeSQL, UnaryCompositeTemplate}
import net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL.{AnyConvertedColumnVisitor, CaseAnyConvertedColumn, CaseSpecificConvertedColumn, SpecificConvertedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.ConvertedSQL.{AnyConvertedVisitor, CaseAnyConverted, CaseSpecificConverted, ConvertedSQLTemplate, SpecificConvertedSQLTemplate, SpecificConvertedVisitor}
import net.noresttherein.oldsql.sql.ast.DecoratedColumnSQL.{AnyDecoratedColumnVisitor, CaseAnyDecoratedColumn, CaseSpecificDecoratedColumn, SpecificDecoratedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.DecoratedSQL.{AnyDecoratedVisitor, CaseAnyDecorated, CaseSpecificDecorated, SpecificDecoratedVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.ConvertedMappingSQL
import net.noresttherein.oldsql.sql.ast.MappingSQL.ConvertedMappingSQL.{AnyConvertedMappingVisitor, CaseAnyConvertedMapping, CaseSpecificConvertedMapping, SpecificConvertedMappingVisitor}
import net.noresttherein.oldsql.sql.ast.TransformedSQL.{AbstractTransformedSQL, SpecificTransformedSQLTemplate, TransformedSQLConvertingTemplate, WrappedSQLTemplate}
import net.noresttherein.oldsql.sql.mechanics.{AlignableColumns, ComposedTransformation, Reform, SQLAdaptation, SQLConversion, SQLScribe, SQLTransformation, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLAdaptation.{ColumnAdaptation, IndependentAdaptation}
import net.noresttherein.oldsql.sql.mechanics.SQLFormConversion.SQLFormIdentity
import net.noresttherein.oldsql.sql.mechanics.SQLTransformation.{GenericTransformation, IndependentTransformation}




/** A base trait for expressions adapting another `SQLExpression[F, S, Y]`, with implementation centered around
  * an [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation SQLTransformation]]`[X, Y]`,
  * which acts as its constructor. SQL generated from this expression
  * and `this.`[[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeTemplate.value value]] may be different, but must type check
  * on the database level, i.e. must consist of the same number of columns using compatible types.
  * All methods implemented here assume that the column sets
  * of this expression and the adapted [[net.noresttherein.oldsql.sql.ast.TransformedSQL.value value]] are the same,
  * and spelling methods simply amount to spelling of the latter.
  *
  * This covers some edge cases usually not encountered by the application directly - most proper adapters
  * will be satisfied with its subclass [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]],
  * and conversions of only the value of the expression, rather than its SQL form, are implemented
  * by [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]].
  */
//consider: AdaptedSQL docs say that it may change the SQL, providing the column types match. Do we need this class, then?
// We may say that it is allowed to change column types for as long as it is responsible for fixing it itself.
// The issue is of course implementing Aligner: it currently cannot descend down SelectId
trait TransformedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends CompositeSQL[F, S, Y] with TransformedSQLConvertingTemplate[F, S, X, Y, SQLExpression]
{
//	protected override def parts :Seq[SQLExpression[F, S, _]] = PassedArray.one(value)
//	override def value :SQLExpression[F, S, X]
//	def transformation          :SQLTransformation[X, Y]
//	override def transformation :SQLTransformation[X, Y]

//	/** Specifies if the [[net.noresttherein.oldsql.sql.ast.TransformedSQL.transformation transformation]]
//	  * behind this expression accepts any expression as an argument without throwing an exception.
//	  */
//	def isUniversal     :Boolean = false
//
//	/** Specifies if the [[net.noresttherein.oldsql.sql.ast.TransformedSQL.transformation transformation]]
//	  * behind this expression produces expressions whose types depend on the type of the argument expression,
//	  * as in [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]]. Note that it is related to
//	  * [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.isConvertingSpecific isConvertingSpecific]],
//	  * but not the same:
//	  * a [[net.noresttherein.oldsql.sql.ast.TransformedSQL.GenericSQLWrapperTemplate GenericSQLWrapperTemplate]]
//	  * is ''specifically convertible'', but it is not ''type dependent'',
//	  */
//	def isTypeDependent :Boolean = false

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, Y] =
		transformation(mapper(value))

	//just to have a single common ancestor for all implementors
	protected override def reformTransformation[U](leftResult :SQLTransformation[Y, U])
			:SQLTransformation.Bound[X, U, leftResult.Expression] =
		transformation andThen leftResult
//
//	protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
//	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//	                             (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
//	                                       spelling :SQLSpelling)
//			:(leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
//		if (other eq this)
//			(leftResult(this), rightResult(other))
//		else if (passCount.firstTime)
//			passReform(other)(reform, passCount)
//		else
//			reform(value, other)(transformation andThen leftResult, rightResult, spelling)

//	protected def name :String = this.innerClassName
//
//	override def toString :String = name + '(' + value + ')'
}




object TransformedSQL {
	type __ = TransformedSQL[_ <: RowProduct, _ >: Grouped <: Single, _, _]


	//This trait looks like it should be useful, but really isn't. Only GenericSQLWrapperTemplate and ConvertedSQL
	// subtypes can really narrow down the return type, because it involves implementing convert :Same,
	// and we must have ConvertingOps[..., Same] because we want to use passReform

	//todo: rename
	trait TransformedSQLConvertingTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, Y,
	                                       +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                             <: ConvertibleSQL[f, s, v, ({ type E[A] = Same[f, s, A] })#E]]
//		extends UnaryCompositeTemplate[F, S, X, Y, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, Y] })#E]
//		extends UnaryCompositeTemplate[F, S, X, Y, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = SQLExpression[f, s, Y] })#E]
		extends BaseSQLOps[F, S, Y]
		//Does not ConvertingTemplate because we don't want to extend it if Same is SQLExpression,
		// because it will automatically become 'specifically convertible'.
//		   with ConvertingTemplate[F, S, Y, ({ type E[v] = Same[F, S, v] })#E]
//	{ this :Same[F, S, Y] with TransformedSQLConvertingTemplate[F, S, X, Y, Same] =>
	{ self :Same[F, S, Y] => //with GroundingOps[F, S, Y, ({ type T[-f <: RowProduct] = Same[f, S, Y] })#T, Same[F, S, Y]] =>
		def transformation :SQLTransformation[X, Y]
//		protected def value :SQLExpression[F, S, X]
		def value          :SQLExpression[F, S, X]

		override def universalForm       :Opt[SQLForm[Y]] = value.universalForm.map(transformation(_))
		override def selectForm          :SQLReadForm[Y] = transformation(value.selectForm) //consider: NullValue?
		override def groundValue         :Opt[Y] = value.groundValue.map(transformation(_))
		//these we'll inherit from UnaryCompositeTemplate/UnaryColumnCompositeTemplate
//		override def isSingleRow         :Boolean = value.isSingleRow
//		override def isGround            :Boolean = value.isGround
//		override def isAnchored          :Boolean = value.isAnchored
//		override def isAnchored(from :F) :Boolean = value.isAnchored(from)

		protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[Y] =
			transformation(spelling.effectiveForm(value))

		override def asSingleRow :Option[Same[F, Single, Y]] =
			if (isSingleRow) Some(this.castFrom[Same[F, S, Y], Same[F, Single, Y]]) else None

		override def anchor(from :F) :Same[F, S, Y] =
			if (isAnchored(from)) this else selfTransformation(value.anchor(from))

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Same[E, S, Y] =
			selfTransformation(value.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Same[E, S, Y] =
			selfTransformation(value.expand(base))

//		protected type ReformTransformation[U, R[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U]] =
//			SQLTransformation.Returning[X, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: R[f, s, Same[f, s, U]] })#E]

		/** A transformation implementation delegating to outer's
		  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL TransformedSQL]]
		  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeTemplate.reapply reapply]] method.
		  */
		protected trait SelfTransformation[W[-f <: RowProduct, -s >: Grouped <: Single, v]// >: Same[f, s, v]
		                                     <: ConvertibleSQL[f, s, v, ({ type E[A] = W[f, s, A] })#E]]
			extends SQLTransformation[X, Y]
		{
			override def isSpecific :Boolean = self.isConvertingSpecific

			override def unapply(value :Y) :Opt[X] = Lack
			override def inverse(value :Y) :X = unapply(value) match {
				case Got(x) => x
				case _ => throw new IllegalArgumentException("Cannot unmap " + value + " with " + this + ".")
			}
			override type Expression[-f <: RowProduct, -s >: Grouped <: Single, +E <: SQLExpression[f, s, Y]] =
				W[f, s, Y]
//
//			override def apply[f <: RowProduct, s >: Grouped <: Single, E[v] <: ConvertibleSQL[f, s, v, E]]
//			                  (expr :ConvertingOps[f, s, X, E]) :W[f, s, Y] =
//				reapply(denullify(expr))

			override def applyString(arg :String) :String = arg + '.' + name
		}

		@inline private def selfTransformation
				:SQLTransformation.Returning[X, Y, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, Y]] = Same[f, s, Y] })#E] =
			reformTransformation(SQLConversion.toSelf)

		/** Composes the transformation used as the constructor/factory for this class with the argument,
		  * retaining the argument's return type
		  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]]. It is used by
		  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.realign realign]] to adapt its `leftResult` argument for
		  * the use with the underlying [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeTemplate.value value]]
		  * of this expression.
		  */
		protected def reformTransformation[U](leftResult :SQLTransformation[Y, U])
				:SQLTransformation.Returning[X, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, Same[f, s, U]] })#E] //=
/*
			new ComposedTransformation[X, Y, U] {
				override val first  :SQLTransformation[X, Y] = transformation
				override val second :leftResult.type = leftResult

				override type Expression[-f <: RowProduct, -s >: Grouped <: Single, +E <: SQLExpression[f, s, U]] =
					leftResult.Expression[f, s, Same[f, s, U]]

				override def apply[f <: RowProduct, s >: Grouped <: Single, E[v] <: ConvertibleSQL[f, s, v, E]]
				                  (expr :ConvertingOps[f, s, X, E]) :leftResult.Expression[f, s, Same[f, s, U]] =
					second(reapply(expr.toConvertibleSQL))
			}
*/

		/** Delegates the reforming to `reform(value, other)` after composing this instance's
		  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.TransformedSQLConvertingTemplate.transformation transformation]]
		  * with `leftResult` using
		  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.TransformedSQLConvertingTemplate.reformTransformation reformTransformation]].
		  */
		//fixme: the default implementation in this interface should delegate to the reformer.
		// Instead, we should move the simplified delegation to reform(value, other) to AdaptedSQL
		// and remove ReshapedSQLTemplate
		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.Expression[F, S, Same[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
			if (this eq other)
				(leftResult(this :Same[F, S, Y]), rightResult(other))
			else if (passCount.firstTime)
				//catching exceptions should be unnecesssary, because other should catch them and delegate to us again,
				// so there is nothing more that we can do.
				self.passReform(other)(reform, passCount)
			else if (passCount.secondTime)
				try
					reform(value, other)(reformTransformation(leftResult), rightResult, spelling)
				catch {
					case e :Exception => try {
						reformer(other)(reform, passCount).apply(other)
					} catch {
						//e1 is likely a 'no idea mate' exception, so e should be more informative
						case e1 :Exception => e.addSuppressed(e1); throw e
					}
				}
			else
				reformer(other)(reform, passCount).apply(other)

		//Commented out because we can't override it in ConvertedSQLTemplate because the latter doesn't take
		// a Same type with three arguments. As the result, classes extending that trait inherit this definition
		// for SQLExpression rather than what they should. No easy way out of it.
/*
		protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                               (implicit leftResult :SQLTransformation[Y, U],
		                                         rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
				:SpecificExpressionVisitor
				 [F2, S2, V2, (leftResult.Expression[F, S, Same[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])] =
			new BaseReformer[F2, S2, V2, EC2, U, leftResult.Expression, rightResult.Expression](
				other)(reform, passCount)(leftResult, rightResult, spelling)
			{
				override def expression(e :SQLExpression[F2, S2, V2]) =
					try
						reform(value, other)(reformTransformation(leftResult), rightResult, spelling)
					catch {
						case e :Exception => try {
							fallback
						} catch {
							//e1 is likely a 'no idea mate' exception, so e should be more informative
							case e1 :Exception => e.addSuppressed(e1); throw e
						}
					}
			}
*/

		protected def name :String = this.innerClassName

		override def toString :String = name + '(' + value + ')'
	}


	/** A base trait for [[net.noresttherein.oldsql.sql.ast.TransformedSQL TransformedSQL]] implementations
	  * which change the [[net.noresttherein.oldsql.sql.RowShape RowShape]] of the transformed expression.
	  * It is equivalent to `TransformedSQLConvertingTemplate`, except that method
	  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.ReshapedSQLTemplate.reform reform]] performs validation
	  * of the results with `reform.`[[net.noresttherein.oldsql.sql.mechanics.Reform.compatible compatible]]
	  * before returning them.
	  */
	trait ReshapedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, Y,
	                          +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                                      <: ConvertibleSQL[f, s, v, ({ type E[A] = Same[f, s, A] })#E]]
		extends TransformedSQLConvertingTemplate[F, S, X, Y, Same]
	{ self :Same[F, S, Y] =>
		/** Delegates the reforming to `reform(value, other)` after composing this instance's
		  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.TransformedSQLConvertingTemplate.transformation transformation]]
		  * with `leftResult` using
		  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.TransformedSQLConvertingTemplate.reformTransformation reformTransformation]].
		  * As `transformation` is not guaranteed to preserve the shape of the expression, reformed expressions
		  * are checked for [[net.noresttherein.oldsql.sql.mechanics.Reform.compatible compatibility]]
		  * before being returned.
		  */
		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.Expression[F, S, Same[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
			if (this eq other)
				(leftResult(this), rightResult(other))
			else
				reformer(other)(reform, passCount).apply(other)

		protected override def reformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                               (implicit leftResult :SQLTransformation[Y, U],
		                                         rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
				:SpecificExpressionVisitor
				 [F2, S2, V2, (leftResult.Expression[F, S, Same[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])] =
			new BaseReformer[F2, S2, V2, EC2, U, leftResult.Expression, rightResult.Expression](
				other)(reform, passCount)(leftResult, rightResult, spelling)
			{
				override def expression(e :SQLExpression[F2, S2, V2]) =
					if (passCount.firstTime) {
						val res = pass
						if (!reform.compatible(res._1, res._2))
							throw new MismatchedExpressionsException(self, other,
								"expressions reformed with " + reform + " are incompatible: " + res + "."
							)
						res
					} else if (passCount.secondTime) try {
						val transformation = reformTransformation(leftResult)
						val res = reform(value, other)(transformation, rightResult, spelling)
						if (!reform.compatible(res._1, res._2))
							throw new MismatchedExpressionsException(self, other,
								"reformed expressions are incompatible after applying transformation " + transformation +
									" by reform " + reform + ": " + res + "."
							)
						res
					} catch {
						case e :Exception => try
							fallback
						catch {
							//e1 is likely a 'no idea mate' exception, so e should be more informative
							case e1 :Exception => e.addSuppressed(e1); throw e
						}
					} else
						fallback
			}
	}


	trait SpecificTransformedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, Y,
	                                     +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                           <: ConvertibleSQL[f, s, v, ({ type E[A] = Same[f, s, A] })#E]]
		extends TransformedSQLConvertingTemplate[F, S, X, Y, Same]
//		   with VariantGroundingTemplate[F, S, Y, ({ type E[-f <: RowProduct] = Same[f, S, Y] })#E]
	{ this :Same[F, S, Y] =>
		override def transformation :IndependentTransformation.Convertible[X, Y, Same]
//
//		override def asSingleRow :Option[Same[F, Single, Y]] =
//			if (isSingleRow) Some(this.castFrom[Same[F, S, Y], Same[F, Single, Y]]) else None
//
//		override def anchor(from :F) :Same[F, S, Y] =
//			if (isAnchored(from)) this else transformation(value.anchor(from))
//
//		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Same[E, S, Y] =
//			transformation(value.basedOn(base))
//
//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Same[E, S, Y] =
//			transformation(value.expand(base))

		protected override def reformTransformation[U](leftResult :SQLTransformation[Y, U])
				:SQLTransformation.Returning[X, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, Same[f, s, U]] })#E] =
			(transformation :IndependentTransformation.Convertible[X, Y, Same]) andThen leftResult

	}


	/** A template mixin trait for subclasses of
	  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL TransformedSQL]], which can wrap a column expression
	  * of any type (with any type arguments). The type constructor `Same` of the subclass is preserved when converting
	  * its value with an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]]
	  * or grounding it in other clauses. The decorator delegates methods from
	  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]]
	  * and [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]] to the same method
	  * of the underlying expression, wrapping the result in an instance of the same class as this expression.
	  * Note that this means in particular that the result of converting this expression isn't (most likely)
	  * are [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]]. Instead, its underlying expression
	  * is a `ConvertedSQL` wrapping directly `this.value`.
	  * This includes also reforming methods, which, considering a `TransformedSQL` can change the column set
	  * of an expression, may have to be overridden in subclasses. On the other hand, despite preserving the value type
	  * of the wrapped expression,
	  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.TransformedSQLConvertingTemplate.transformation transformation]]
	  * is applied to all forms of the underlying expression.
	  */
	//This is an identity value transformation only because it needs to implement convert :Same, and it does it
	// by pushing the conversion down and wrapping the result.
	//consider: renaming to SpecificWrappedSQL, or making it a pure template trait
	trait WrappedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                         +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                               <: ConvertibleSQL[f, s, v, ({ type E[X] = Same[f, s, X] })#E]]
		extends TransformedSQL[F, S, V, V]
		   with SpecificTransformedSQLTemplate[F, S, V, V, Same]
		   with UnaryCompositeTemplate[F, S, V, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, V] })#E]
//		   with TransformedSQLConvertingTemplate[F, S, V, V, Same]
	{ self :Same[F, S, V] =>
		override def value       :SQLExpression[F, S, V] //override clash
		override def groundValue :Opt[V] = value.groundValue
//		override def transformation :IndependentTransformation.Convertible[V, V, Same]

		protected override def convert[Y](conversion :SQLConversion[V, Y]) :Same[F, S, Y] =
			conversion(value) match {
				case same if same eq value => this.castFrom[Same[F, S, V], Same[F, S, Y]]
				case other => decorate(other)
			}

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, V]) :Same[E, C, V] =
			if(value eq e) this.castFrom[Same[F, S, V], Same[E, C, V]]
			else decorate(e)

		protected def decorate[E <: RowProduct, C >: Grouped <: Single, X](e :SQLExpression[E, C, X]) :Same[E, C, X]

//		protected override def reformTransformation[U](leftResult :SQLTransformation[V, U])
//				:SQLTransformation.Returning[V, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, Same[f, s, U]] })#E] =
//			(transformation :IndependentTransformation.Convertible[V, V, Same]) andThen leftResult
	}


	/** A base trait for `TransformedSQL` implementations secondary
	  * to a [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation SQLTransformation]], delegating methods
	  * to the latter where possible.
	  */
	trait AbstractTransformedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
		extends UnaryCompositeSQL[F, S, X, Y] with TransformedSQL[F, S, X, Y]
	{ self =>
//		override def value :SQLExpression[F, S, X] //override clash between UnaryCompositeSQL and TransformedSQLReformingTemplate

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, X]) =
			if (e eq value) this.castFrom[SQLExpression[F, S, Y], SQLExpression[E, C, Y]]
			else transformation(e)

		override def toString :String = transformation.toString + "(" + value + ")"
	}



	trait SpecificTransformedVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +R]
		extends SpecificAdaptedVisitor[F, S, V, R]
	{
		def transformed[X](e :TransformedSQL[F, S, X, V]) :R
	}
	trait MatchSpecificTransformed[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificTransformedVisitor[F, S, X, Y] with CaseSpecificAdapted[F, S, X, Y]

	trait CaseSpecificTransformed[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends MatchSpecificTransformed[F, S, V, Y]
	{
		override def adapted[X](e :AdaptedSQL[F, S, X, V]) :Y = transformed(e)
	}


	trait AnyTransformedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyAdaptedVisitor[F, R]
	{
		def transformed[S >: Grouped <: Single, X, Y](e :TransformedSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyTransformed[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyTransformedVisitor[F, R] with CaseAnyAdapted[F, R]

	trait CaseAnyTransformed[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends MatchAnyTransformed[F, R] {
		override def adapted[S >: Grouped <: Single, T, U](e :AdaptedSQL[F, S, T, U]) :R[S, U] = transformed(e)
	}

}




/** This class exists primarily as a base class for default `AdaptedSQL`, `AdaptedColumn`, `ConvertedSQL` and
  * `ConvertedColumn` implementation. There is little sense in having a no-op `TransformedSQL` implementation,
  * because it must equal `transformation(value)` anyway, so we might as well call that; an `SQLTransformation`
  * either creates a specific class, or is an `SQLAdaptation`, which will use `AdaptedSQL`.
  */ //this could well be package private if it wasn't for SQLConversion.Swap (which likely can be removed)
private[sql] class IdentityTransformedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
                   (override val value :SQLExpression[F, S, X], conversion :SQLTransformation[X, Y])
	extends AbstractTransformedSQL[F, S, X, Y]
{
	override def transformation :SQLTransformation[X, Y] = conversion //a def, because AdaptedSQL overrides it with a def to adaptation

	protected override def columnCount(implicit spelling :SQLSpelling) :Int = spelling.columnCount(value)
	protected override def shape(implicit spelling :SQLSpelling) :RowShape = spelling.shape(value)
	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] = spelling.split(value)
	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		spelling.potentialColumns(value, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
		spelling.potentialColumnsCount(value)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(value)(from, context, params)

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		spelling.explode(value, independent)(from, context, params)


	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, Y, R]) :R = visitor.transformed(this)

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, R]) :R[S, Y] =
		visitor.transformed(this)

}




/*
trait TransformedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends TransformedSQL[F, S, X, Y] with CompositeColumnSQL[F, S, Y]
	   with TransformedSQLReformingTemplate[F, S, X, Y, ColumnSQL]
{
	override def isUniversal = false
	protected override def reformTransformation[U](leftResult :SQLTransformation[Y, U])
			:SQLTransformation.Returning[X, U,
			                             ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] =
				                             leftResult.Expression[f, s, ColumnSQL[f, s, U]] })#E] =
		new ComposedTransformation[X, Y, U] {
			override type Expression[-f <: RowProduct, -s >: Grouped <: Single, +E <: SQLExpression[f, s, U]] =
				leftResult.Expression[f, s, ColumnSQL[f, s, U]]
			override val first  = transformation
			override val second :leftResult.type = leftResult
			override def isUniversal = false

			override def apply[C <: RowProduct, A >: Grouped <: Single, E[v] <: ConvertibleSQL[C, A, v, E]]
			                  (expr :ConvertingTemplate[C, A, X, E]) =
				first(expr) match {
					case column :ColumnSQL[C, A, Y] =>
						second(column)
					case other =>
						throw new IllegalExpressionException(
							"Cannot return non column `" + other + "` as reformed `" + TransformedColumnSQL.this + "`."
						)
				}
		}
}




object TransformedColumnSQL {

	trait AbstractTransformedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
		extends TransformedColumnSQL[F, S, X, Y]
		   with UnaryCompositeColumn[F, S, X, Y]
	{ self =>
		override def value :ColumnSQL[F, S, X]

		override def transformation :SQLTransformation[X, Y] =
			new WrappingTransformationTemplate[X, Y, SQLExpression] {
				override def isUniversal = false

				override def apply(value :X) :Y = self.convert(value)
				override def unapply(value :Y) :Opt[X] = Lack
				override def inverse(value :Y) =
					throw new UnsupportedOperationException("Transformation " + this + " is irreversible.")

				override protected def wrap[E <: RowProduct, A >: Grouped <: Single](expr :SQLExpression[E, A, X]) =
					expr match {
						case column :ColumnSQL[E, A, X] => reapply(column)
						case _ => throw new IllegalExpressionException(
							"Cannot transform non column `" + expr + "` reformed from `" + expr + "`"
						)
					}

				override def applyString(arg :String) :String = arg + ".wrap(" + self.localClassName + ")"
			}
	}

	trait GenericTransformedColumnTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                       +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                             <: ConvertibleSQL[f, s, v, ({ type E[X] = Same[f, s, X] })#E]]
		extends ColumnConvertingTemplate[F, S, V, ({ type E[v] = Same[F, S, v] })#E]
		   with TransformedSQLReformingTemplate[F, S, V, V, Same]
		   with UnaryCompositeColumnTemplate[F, S, V, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, V] })#E]
		{ self :Same[F, S, V] with TransformedColumnSQL[F, S, V, V] =>
			override def convert(x :V) :V = x

			override def transformation :WrappingTransformation.Convertible[V, V, Same] =
				new WrappingTransformationTemplate[V, V, Same] with SQLFormIdentity[V] {
					override protected def wrap[E <: RowProduct, A >: Grouped <: Single](expr :SQLExpression[E, A, V]) =
						expr match {
							case column :ColumnSQL[E, A, V] =>
								self.decorate(column)
							case _ =>
								throw new IllegalExpressionException(
									"Cannot decorate a non column expression " + expr + ": " + expr.className + " with " +
										this.className + "."
								)
						}
					override def applyString(arg :String) :String = arg + ".wrap(" + self.localClassName + ")"
				}

		protected override def reformTransformation[U](leftResult :SQLTransformation[V, U])
//				:ReformTransformation[U, leftResult.Expression] =
				:SQLTransformation.Returning[V, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, Same[f, s, U]] })#E] =
			transformation andThen leftResult

		override def asSingleRow :Option[Same[F, Single, V]] =
			if (isSingleRow) Some(this.asInstanceOf[Same[F, Single, V]]) else None

		protected override def convert[Y](conversion :SQLConversion[V, Y]) :Same[F, S, Y] =
			reapply(conversion(value))

		protected override def reapply[E <: RowProduct, C >: Grouped <: S](e :ColumnSQL[E, C, V]) :Same[E, C, V] =
			decorate(e)

		protected def decorate[E <: RowProduct, C >: Grouped <: Single, X](e :ColumnSQL[E, C, X]) :Same[E, C, X]
	}


	trait SpecificTransformedColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificAdaptedColumnVisitor[F, S, X, Y]
	{
		def transformedColumn[V](e :TransformedColumnSQL[F, S, V, X]) :Y
	}
	trait MatchSpecificTransformedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificTransformedColumnVisitor[F, S, X, Y] with CaseSpecificAdaptedColumn[F, S, X, Y]

	trait CaseSpecificTransformedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificTransformedColumn[F, S, X, Y]
	{
		override def adaptedColumn[V](e :AdaptedColumnSQL[F, S, V, X]) :Y = transformedColumn(e)
	}


	trait AnyTransformedColumnVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyAdaptedColumnVisitor[F, R]
	{
		def transformedColumn[S >: Grouped <: Single, X, Y](e :TransformedColumnSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyTransformedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyTransformedColumnVisitor[F, Y] with CaseAnyAdaptedColumn[F, Y]

	trait CaseAnyTransformedColumn[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends MatchAnyTransformedColumn[F, R]
	{
		override def adaptedColumn[S >: Grouped <: Single, X, Y]
		                            (e :AdaptedColumnSQL[F, S, X, Y]) :R[S, Y] = transformedColumn(e)
	}
}
*/






/** A [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]] wrapper over another
  * SQL [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL.value expression]]
  * which has the same SQL column set as the latter, despite possibly having different value types.
  * In the default implementation, the generated SQL does not change by wrapping the expression, although
  * it is not strictly required, providing its [[net.noresttherein.oldsql.sql.ast.AdaptedSQL.transformation adaptation]],
  * creates the same specialized adapter type. It encompasses transparent
  * type [[net.noresttherein.oldsql.sql.ast.ConvertedSQL conversions]]
  * as well as [[net.noresttherein.oldsql.sql.ast.DecoratedSQL decorators]] which add some additional
  * information about the expression.
  */ //todo :parameterize this with the type of the adapted expression
trait AdaptedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends TransformedSQL[F, S, X, Y] with SelectableSQL[F, S, Y]
	   with AdaptedSQLTemplate[F, S, X, Y, SQLExpression]
{ self =>

//	override def transformation :SQLAdaptation[X, Y]
	override def isShapeComposite :Boolean = true
	override def isInline         :Boolean = true

	protected override def columnCount(implicit spelling :SQLSpelling) :Int = spelling.columnCount(value)
	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] = spelling.split(value)
	protected override def shape(implicit spelling :SQLSpelling) :RowShape = spelling.shape(value)

	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		spelling.potentialColumns(value, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
		spelling.potentialColumnsCount(value)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(value)(from, context, params)

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		spelling.explode(value, independent)(from, context, params)

//	override def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, S, Y] =
//		if (permutation == permutation.indices) this
//		else reapply(value.reorder(permutation))

	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, Y, R]) :R = visitor.adapted(this)

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor: AnyExpressionVisitor[F, R]): R[S, Y] =
		visitor.adapted(this)

	override def equals(that :Any) :Boolean = that match {
		case other :AdaptedSQL[_, _, _, _] if canEqual(other) && other.canEqual(this) => value == other.value
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.getClass == getClass
	override def hashCode :Int = value.hashCode
}




object AdaptedSQL {
	//todo: a 'generic' AdaptedSQL makes no sense, but we currently use it in SQLTransformation.swap
	// (which likewise not necessarily makes sense).
	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y]
	         (expr :SQLExpression[F, S, X], conversion :SQLAdaptation[X, Y]) :AdaptedSQL[F, S, X, Y] =
		expr match {
			case column :ColumnSQL[F, S, X] => AdaptedColumnSQL(column, conversion)
			case _ => new Impl(expr, conversion)
		}

	def unapply[F <: RowProduct, S >: Grouped <: Single, X]
	           (e :SQLExpression[F, S, X]) :Opt[SQLExpression[F, S, _]] =
		e match {
			case adapter :AdaptedSQL[F, S, _, X] => Got(adapter.value)
			case _ => Lack
		}


	trait AdaptedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, Y,
	                         +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                               <: ConvertibleSQL[f, s, v, ({ type E[A] = Same[f, s, A] })#E]]
		//consider: extending ReorderingTemplate and implementing realign here
		extends UnaryCompositeTemplate[F, S, X, Y, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, Y] })#E]
		   with TransformedSQLConvertingTemplate[F, S, X, Y, Same]
	{ self :Same[F, S, Y] =>
		override def value :SQLExpression[F, S, X] //override clash between TransformedSQLConvertingTemplate and UnaryCompositeTemplate
		override def transformation :SQLAdaptation[X, Y]

		protected def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y]

//		protected override def reapply[E <: RowProduct, C >: Grouped <: Single](value :SQLExpression[E, C, X]) :Same[E, C, Y]

		//consider: moving it down to ConvertedSQL.
//		protected def reapply[E <: RowProduct, C >: Grouped <: Single](value : ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y]

		/** An adaptation implementation delegating to outer's
		  * [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]]
		  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeTemplate.reapply reapply]]
		  * and [[net.noresttherein.oldsql.sql.ast.AdaptedSQL.reapply reapply]]`(column)` methods.
		  */
		protected trait SelfAdaptation[W[-f <: RowProduct, -s >: Grouped <: Single, v] >: Same[f, s, v]
		                                 <: ConvertibleSQL[f, s, v, ({ type E[A] = W[f, s, A] })#E]]
			extends SelfTransformation[W] with SQLAdaptation[X, Y]
		{
			override type ColumnResult[-f <: RowProduct, -s >: Grouped <: Single, +E <: ColumnSQL[f, s, Y]] =
				ColumnSQL[f, s, Y]
//
//			override def apply[F1 <: RowProduct, S1 >: Grouped <: Single, E[v] <: ConvertibleSQL[F1, S1, v, E]]
//			                  (expr :ConvertingOps[F1, S1, X, E]) :W[F1, S1, Y] =
//				expr match {
//					case null => self.reapply(SQLNull[X]())
//					case column :ColumnSQL[F1, S1, X] => reapply(column)
//					case _ => self.reapply(expr.toConvertibleSQL)
//				}

			override def column[F1 <: RowProduct, S1 >: Grouped <: Single, E[v] <: ConvertibleColumn[F1, S1, v, E]]
			                   (expr :ColumnConvertingOps[F1, S1, X, E]) :ColumnSQL[F1, S1, Y] =
				expr match {
					case null => reapply(SQLNull[X]())
					case _ => reapply(expr.toConvertibleSQL)
				}

			override def apply[f <: RowProduct, s >: Grouped <: Single, E[v] <: ConvertibleSQL[f, s, v, E]]
			                  (expr :ConvertingOps[f, s, X, E]) :W[f, s, Y] =
				reapply(denullify(expr))
		}

//		/** Composes the transformation used as the constructor/factory for this class with the argument,
//		  * retaining the argument's return type
//		  * [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation.Expression Expression]]. It is used by
//		  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.reform reform]] to adapt its `leftResult` argument for
//		  * the use with the underlying [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeTemplate.value value]]
//		  * of this expression. This method works analogously to
//		  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.TransformedSQLConvertingTemplate.reformTransformation reformTransformation]];
//		  * this duplication exists due to a conflict between the need to narrow down the type of expressions returned
//		  * by the transformation, and the
//		  */
		protected override def reformTransformation[U](leftResult :SQLTransformation[Y, U])
				:SQLTransformation.Returning[X, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, Same[f, s, U]] })#E] =
			new ComposedTransformation[X, Y, U] {
				override val first  :SQLTransformation[X, Y] = transformation
				override val second :leftResult.type = leftResult

				override type Expression[-f <: RowProduct, -s >: Grouped <: Single, +E <: SQLExpression[f, s, U]] =
					leftResult.Expression[f, s, Same[f, s, U]]

				override def apply[f <: RowProduct, s >: Grouped <: Single, E[v] <: ConvertibleSQL[f, s, v, E]]
				                  (expr :ConvertingOps[f, s, X, E]) :leftResult.Expression[f, s, Same[f, s, U]] =
					second(reapply(expr.toConvertibleSQL))
			}

		protected override def realign(reordering :Rearrangement)
		                              (implicit spelling :SQLSpelling) :Same[F, S, Y] =
			if (reordering.isIdentity) this
			else reapply(spelling.realign(value, reordering))
	}

	/** An `AdaptedSQL` mixin which does not change during
	  * [[net.noresttherein.oldsql.sql.ast.AdaptedSQL.IrreformableAdaptedSQLTemplate.reform reforming]].
	  * It reforms its [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeTemplate.value value]]
	  * with the other expression, allowing changes only to the latter, and then returns itself with the reformed
	  * argument expression.
	  */
	trait IrreformableAdaptedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, Y,
	                                     +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                           <: ConvertibleSQL[f, s, v, ({ type E[A] = Same[f, s, A] })#E]]
		extends TransformedSQLConvertingTemplate[F, S, X, Y, Same]
	{ self :Same[F, S, Y] => //with GroundingOps[F, S, Y, ({ type T[-f <: RowProduct] = Same[f, S, Y] })#T, Same[F, S, Y]] =>

		protected def passLimit :Int = 2

		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.Expression[F, S, Same[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
			if (other eq this)
				(leftResult(this :Same[F, S, Y]), rightResult(other))
			else if (passCount.toInt < passLimit)
				self.passReform(other)(reform, passCount)
			else {
				val transformation = reformTransformation(leftResult)
				val right = reform.prohibitReformLeft(value, other)(transformation, rightResult, spelling)._2
				(leftResult(this :Same[F, S, Y]), right)
			}
	}


	trait SpecificAdaptedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y,
	                         +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                               <: ConvertibleSQL[f, s, v, ({ type E[A] = Same[f, s, A] })#E]]
		extends SpecificTransformedSQLTemplate[F, S, X, Y, Same]
//		   with AdaptedSQL[F, S, X, Y] //mixed in after SpecificTransformedSQLTemplate
//		   with UnaryCompositeTemplate[F, S, X, Y, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, Y] })#E]
		   with ConvertingTemplate[F, S, Y, ({ type E[v] = Same[F, S, v] })#E]
		   with VariantGroundingTemplate[F, S, Y, ({ type E[-f <: RowProduct] = Same[f, S, Y] })#E]
		   with AdaptedSQLTemplate[F, S, X, Y, Same]
	{ this :Same[F, S, Y] =>
		override def transformation :IndependentAdaptation.Convertible[X, Y, Same]
	}

	//	trait AbstractAdaptedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y] extends AdaptedSQL[F, S, X, Y] { self =>
//		override def transformation :SQLAdaptation[X, Y] = new PrivateAdaptation
////		override def isUniversal = true
//	}


	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	                  (override val value :SQLExpression[F, S, X], override val transformation :SQLAdaptation[X, Y])
		extends IdentityTransformedSQL[F, S, X, Y](value, transformation) with AdaptedSQL[F, S, X, Y]
	{
		override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] =
			if (e eq value) this.asInstanceOf[ColumnSQL[E, C, Y]] else transformation.column(e)
	}


	trait SpecificAdaptedVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +R]
		extends SpecificAdaptedColumnVisitor[F, S, V, R]
		   with SpecificConvertedVisitor[F, S, V, R] with SpecificDecoratedVisitor[F, S, V, R]
	{
		def adapted[X](e :AdaptedSQL[F, S, X, V]) :R
	}
	trait MatchSpecificAdapted[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificAdaptedVisitor[F, S, X, Y]
		   with CaseSpecificConverted[F, S, X, Y] with CaseSpecificDecorated[F, S, X, Y]
	{
		override def adaptedColumn[V](e :AdaptedColumnSQL[F, S, V, X]) :Y = adapted(e)
	}
	trait CaseSpecificAdapted[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends MatchSpecificAdapted[F, S, V, Y]
	{
		override def converted[X](e :ConvertedSQL[F, S, X, V]) :Y = adapted(e)
		override def decorated(e :DecoratedSQL[F, S, V]) :Y = adapted(e)
	}


/*
	trait AdaptedVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
		extends AdaptedColumnVisitor[F, Y] with ConvertedVisitor[F, Y] with DecoratedVisitor[F, Y]
	{
		def adapted[S >: Grouped <: Single, X, V](e :AdaptedSQL[F, S, X, V]) :Y[S, V, AdaptedSQL[F, S, X, V]]
	}
	trait MatchAdapted[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
		extends AdaptedVisitor[F, Y] with CaseConverted[F, Y] with CaseDecorated[F, Y]
	{
		override def adaptedColumn[S >: Grouped <: Single, X, V]
		                          (e :AdaptedColumnSQL[F, S, X, V]) :Y[S, V, AdaptedColumnSQL[F, S, X, V]] =
			adapted(e)
	}
	trait CaseAdapted[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
		extends MatchAdapted[F, Y]
	{
		override def converted[S >: Grouped <: Single, X, V]
		                      (e :ConvertedSQL[F, S, X, V]) :Y[S, V, ConvertedSQL[F, S, X, V]] =
			adapted(e)

		override def decorated[S >: Grouped <: Single, V]
		                      (e :DecoratedSQL[F, S, V]) :Y[S, V, DecoratedSQL[F, S, V]] =
			adapted(e)
	}
*/


	trait AnyAdaptedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyAdaptedColumnVisitor[F, R] with AnyConvertedVisitor[F, R] with AnyDecoratedVisitor[F, R]
	{
		def adapted[S >: Grouped <: Single, X, Y](e :AdaptedSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyAdapted[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyAdaptedVisitor[F, R] with CaseAnyConverted[F, R] with CaseAnyDecorated[F, R]
	{
		override def adaptedColumn[S >: Grouped <: Single, X, Y](e :AdaptedColumnSQL[F, S, X, Y]) :R[S, Y] =
			adapted(e)
	}
	trait CaseAnyAdapted[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends MatchAnyAdapted[F, R] {
		override def converted[S >: Grouped <: Single, T, U](e :ConvertedSQL[F, S, T, U]) :R[S, U] =
			adapted(e)

		override def decorated[S >: Grouped <: Single, V](e :DecoratedSQL[F, S, V]) :R[S, V] = adapted(e)
	}

}







/** A [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL CompositeColumnSQL]] wrapper over another
  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn.value column expression]]
  * which translates to the same SQL. It encompasses transparent
  * type [[net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL conversions]]
  * as well as [[net.noresttherein.oldsql.sql.ast.DecoratedColumnSQL decorators]] which add some additional
  * information about the expression.
  */
trait AdaptedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends AdaptedSQL[F, S, X, Y]
	   with UnaryCompositeColumn[F, S, X, Y]
	   with AdaptedColumnTemplate[F, S, X, Y, ColumnSQL]
//	   with TransformedSQLConvertingTemplate[F, S, X, Y, ColumnSQL]
//	   with AdaptedSQLReformingTemplate[F, S, X, Y, ColumnSQL]
{ self =>
	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, Y, R]) :R = visitor.adaptedColumn(this)

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, Y] =
		visitor.adaptedColumn(this)

//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :Y[S_, Y, E] =
//		visitor.adaptedColumn(this)
}




object AdaptedColumnSQL {
	//see comment in AdaptedSQL
	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y]
	         (expr :ColumnSQL[F, S, X], conversion :SQLAdaptation[X, Y]) :AdaptedColumnSQL[F, S, X, Y] =
		new Impl(expr, conversion)

	def unapply[F <: RowProduct, S >: Grouped <: Single, X](e :ColumnSQL[F, S, X]) :Opt[ColumnSQL[F, S, _]] =
		(e :SQLExpression[F, S, X]) match {
			case conv :AdaptedColumnSQL[F, S, _, X] => Got(conv.value)
//			case AdaptedSQL(col :ColumnSQL[F, S, _]) => Got(col)
			case _ => Lack
		}
	//We don't need an AbstractAdaptedColumnSQL because AbstractAdaptedSQL already has reapply(e :ColumnSQL)
	// and AdaptedColumnSQL needs to implement reapply(e :SQLExpression) anyway.

	/* While we do not extend AdaptedSQLTemplate[_, Same], so we can return a non-column expression from
	 * reapply(SQLExpression), unfortunately, this.transformation works only for ColumnSQL arguments.
	 * This is because I see no other way of currently implementing reformTransformation.
	 * This might, however, be possible if we refactor SQLAdaptation to preserve ColumnResult type (or whatever).
	 * If not, we should inherit AdaptedSQLTemplate[_, Same], as it will make things simpler in general.
	 */
	trait AdaptedColumnTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, Y,
	                            +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                  <: ConvertibleColumn[f, s, v, ({ type E[A] = Same[f, s, A] })#E]]
		//Does not extend AdaptedSQLTemplate so as to not force ColumnSQL return type on reapply(SQLExpression)
		// (at least after SQLConversion refactor which preserves all column types).
		//Does not extend ColumnGroundingTemplate so that vanilla AdaptedColumn is not a grounding specific column.
		extends TransformedSQLConvertingTemplate[F, S, X, Y, Same]
		   with AdaptedSQLTemplate[F, S, X, Y, SQLExpression]
		   with UnaryCompositeColumnTemplate[F, S, X, Y, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, Y] })#E]
           with VariantColumnGroundingTemplate[F, S, Y, ({ type E[-f <: RowProduct] = Same[f, S, Y] })#E]
	{ self :Same[F, S, Y] =>

		override def value         :ColumnSQL[F, S, X] //clash between UnaryCompositeTemplate and UnaryColumnCompositeTemplate
		override def selectForm    :ColumnReadForm[Y]  = transformation(value.selectForm)
		override def universalForm :Opt[ColumnForm[Y]] = value.universalForm.map(transformation(_))

		//override clash between AdaptedSQLTemplate and UnaryCompositeColumnTemplate
		protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :Same[E, C, Y]

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                              (e :SQLExpression[E, C, X]) :SQLExpression[E, C, Y] = //:Same[E, C, Y] =
			e match {
				case null => reapply(SQLNull[X]())
				case column :ColumnSQL[E, C, X] => reapply(column)
				case MultiNull() => reapply(SQLNull[X]())
				case _ =>
					throw new IllegalExpressionException(
						"Cannot transform non column expression `" + e + "` :" + e.className + " with " + this + "."
					)
			}

		/** An adaptation implementation delegating to outer's
		  * [[net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL AdaptedColumnSQL]]
		  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumnTemplate.reapply reapply]] method.
		  * Non `ColumnSQL` argument expressions are rejected
		  * with a [[net.noresttherein.oldsql.exceptions.IllegalExpressionException IllegalExpressionException]].
		  *
		  * This adaptation is an
		  * `IndependentAdaptation.`[[net.noresttherein.oldsql.sql.mechanics.SQLAdaptation.IndependentAdaptation.Convertible Convertible]]`[X, Y, W]`,
		  * so its composition with another transformation `next` yields a transformation returning
		  * `next.Expression[f, s, W[f, s, U]]`
		  */
		protected trait SelfColumnAdaptation[W[-f <: RowProduct, -s >: Grouped <: Single, v] >: Same[f, s, v]
		                                        <: ConvertibleColumn[f, s, v, ({ type E[A] = W[f, s, A] })#E]]
			extends SelfTransformation[W] with ColumnAdaptation[X, Y] with GenericTransformation[X, Y, W]
		{
			override def isSpecificColumn :Boolean = self.isColumnConvertingSpecific

			override def column[f <: RowProduct, s >: Grouped <: Single, E[v] <: ConvertibleColumn[f, s, v, E]]
			                   (expr :ColumnConvertingOps[f, s, X, E]) :W[f, s, Y] =
				self.reapply(expr)
		}

		/** Composes this instance's [[net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL.transformation transformation]]
		  * with the argument. This implementation is very similar to the inherited one,
		  * with the subtle difference being that it outright rejects any non column expression as the argument
		  * and delegates to directly to
		  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumnTemplate.reapply reapply]]
		  * otherwise.
		  */
		protected override def reformTransformation[U](leftResult :SQLTransformation[Y, U])
				:SQLTransformation.Returning[X, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, Same[f, s, U]] })#E] =
			if (!leftResult.isTypeDependent)
				(transformation andThen leftResult).castFrom[
					SQLTransformation[X, U],
					SQLTransformation.Returning[X, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, Same[f, s, U]] })#E]
				]
			else
				new ComposedTransformation[X, Y, U] {
					override val first  :SQLTransformation[X, Y] = transformation
					override val second :leftResult.type = leftResult

					override type Expression[-f <: RowProduct, -s >: Grouped <: Single, +E <: SQLExpression[f, s, U]] =
						leftResult.Expression[f, s, Same[f, s, U]]

					override def apply[F1 <: RowProduct, S1 >: Grouped <: Single, E[v] <: ConvertibleSQL[F1, S1, v, E]]
					                  (expr :ConvertingOps[F1, S1, X, E]) :leftResult.Expression[F1, S1, Same[F1, S1, U]] =
						denullify(expr) match {
							case column :ColumnSQL[F1, S1, X] => second(reapply(column))
							case _ => throw new IllegalExpressionException(
								"Cannot adapt a non column expression `" + expr + "` with " + transformation +
									" after reforming `" + self + "`."
							)
						}
				}

//		/** Works the same way as
//		  * [[net.noresttherein.oldsql.sql.ast.TransformedSQL.TransformedSQLConvertingTemplate TransformedSQLConvertingTemplate]]`.`[[net.noresttherein.oldsql.sql.ast.TransformedSQL.TransformedSQLConvertingTemplate.reform reform]],
//		  * except it forwards to [[net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL.columnTransformation columnTransformation]].
//		  */
//		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
//		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//		                             (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
//		                                       spelling :SQLSpelling)
//				:(leftResult.Expression[F, S, ColumnSQL[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
//			if (other eq this)
//				(leftResult(this), rightResult(other))
//			else if (passCount.firstTime)
//				self.passReform(other)(reform, passCount)
//			else
//				reform(value, other)(columnTransformation(leftResult), rightResult, spelling)

		protected override def effectiveForm(implicit spelling :SQLSpelling) :ColumnForm[Y] =
			transformation(spelling.effectiveForm(value))
	}


	trait SpecificAdaptedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y,
	                               +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                     <: ConvertibleColumn[f, s, v, ({ type E[A] = Same[f, s, A] })#E]]
		extends SpecificTransformedSQLTemplate[F, S, X, Y, Same]
		   with ColumnConvertingTemplate[F, S, Y, ({ type E[v] = Same[F, S, v] })#E]
		   with AdaptedColumnTemplate[F, S, X, Y, Same]
		   with VariantColumnGroundingTemplate[F, S, Y, ({ type E[-f <: RowProduct] = Same[f, S, Y] })#E]
	{ this :Same[F, S, Y] =>
		override def transformation :IndependentAdaptation.Convertible[X, Y, Same]
	}


	trait AdaptedColumnOnlySQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
		extends AdaptedColumnSQL[F, S, X, Y]
	{
		protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                              (e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] //stupid overloading

		protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                              (e :SQLExpression[E, C, X]) :ColumnSQL[E, C, Y] =
			e match {
				case column :ColumnSQL[E, C, X] => reapply(column)
				case _ => throw new IllegalExpressionException(
					name + " cannot be applied to a non column expression `" + e + "`."
				)
			}
	}


	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	                  (override val value :ColumnSQL[F, S, X], override val transformation :SQLAdaptation[X, Y])
		extends IdentityTransformedSQL[F, S, X, Y](value, transformation) with AdaptedColumnSQL[F, S, X, Y]
	{
		override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] =
			if (e eq value) this.asInstanceOf[ColumnSQL[E, C, Y]] else transformation.column(e)
	}


	trait SpecificAdaptedColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificConvertedColumnVisitor[F, S, X, Y] with SpecificDecoratedColumnVisitor[F, S, X, Y]
	{
		def adaptedColumn[V](e :AdaptedColumnSQL[F, S, V, X]) :Y
	}
	trait MatchSpecificAdaptedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificAdaptedColumnVisitor[F, S, X, Y]
		   with CaseSpecificConvertedColumn[F, S, X, Y] with CaseSpecificDecoratedColumn[F, S, X, Y]

	trait CaseSpecificAdaptedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificAdaptedColumn[F, S, X, Y]
	{
		override def convertedColumn[V](e :ConvertedColumnSQL[F, S, V, X]) :Y = adaptedColumn(e)
		override def decoratedColumn(e :DecoratedColumnSQL[F, S, X]) :Y = adaptedColumn(e)
	}
//
//
//	trait AdaptedColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedColumnVisitor[F, Y] with DecoratedColumnVisitor[F, Y]
//	{
//		def adaptedColumn[S >: Grouped <: Single, X, V]
//		                 (e :AdaptedColumnSQL[F, S, X, V]) :Y[S, V, AdaptedColumnSQL[F, S, X, V]]
//	}
//	trait MatchAdaptedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends AdaptedColumnVisitor[F, Y] with CaseConvertedColumn[F, Y] with CaseDecoratedColumn[F, Y]
//
//	trait CaseAdaptedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchAdaptedColumn[F, Y]
//	{
//		override def convertedColumn[S >: Grouped <: Single, X, V]
//		                            (e :ConvertedColumnSQL[F, S, X, V]) :Y[S, V, ConvertedColumnSQL[F, S, X, V]] =
//			adaptedColumn(e)
//
//		override def decoratedColumn[S >: Grouped <: Single, V]
//		                            (e :DecoratedColumnSQL[F, S, V]) :Y[S, V, DecoratedColumnSQL[F, S, V]] =
//			adaptedColumn(e)
//
//	}


	trait AnyAdaptedColumnVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyConvertedColumnVisitor[F, R] with AnyDecoratedColumnVisitor[F, R]
	{
		def adaptedColumn[S >: Grouped <: Single, X, Y](e :AdaptedColumnSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyAdaptedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyAdaptedColumnVisitor[F, Y] with CaseAnyConvertedColumn[F, Y] with CaseAnyDecoratedColumn[F, Y]

	trait CaseAnyAdaptedColumn[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends MatchAnyAdaptedColumn[F, R]
	{
		override def convertedColumn[S >: Grouped <: Single, X, Y]
		                            (e :ConvertedColumnSQL[F, S, X, Y]) :R[S, Y] = adaptedColumn(e)

		override def decoratedColumn[S >: Grouped <: Single, V]
		                            (e :DecoratedColumnSQL[F, S, V]) :R[S, V] = adaptedColumn(e)
	}
}






/** An adapter of an `SQLExpression[F, S, X]` to a new value type `Y`.
  * The conversion is assumed to be either between two Scala types which map to the same SQL type,
  * or between types which are implicitly converted between each other in SQL.
  * There is no change whatsoever to the generated SQL, nor any additional information or functionality.
  *
  * The difference from its supertype `AdaptedSQL` is that the expression is only an implementation
  * artifact introduced to satisfy type checking and it limits itself ''only'' to changing the Scala value of an expression
  * - is essentially just a function `X => Y` lift to `SQLExpression` type. Various operations on `SQLExpression`
  * may compose several instances into one, change the exact class used, or change the order in which other adapters
  * are applied with regard to this instance, and are generally not limited in any way other than ensuring the result
  * still type checks and the [[net.noresttherein.oldsql.sql.SQLExpression.selectForm selectForm]] of the expression
  * uses `this.`[[net.noresttherein.oldsql.sql.ast.TransformedSQL.transformation transformation]].
  */ //consider: making it extend TransformedSQL instead to reflect that AdaptedSQL cannot be moved up/down or composed
trait ConvertedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends AdaptedSQL[F, S, X, Y] //with SpecificConvertedSQLTemplate[F, S, X, Y, SQLExpression]
	   with ConvertedSQLTemplate[F, S, X, Y, SQLExpression.from[F]#rows[S]#E]
{
//	override val value          :SQLExpression[F, S, X] //declaration clash
//	override def transformation :SQLConversion[X, Y]
//	override def convert(x :X)  :Y = adaptation(x)
	//overridden to facilitate form equality
//	override def selectForm     :SQLReadForm[Y] = transformation(value.selectForm)

//	protected override def convert[Z](conversion :SQLConversion[Y, Z]) :SQLExpression[F, S, Z] =
//		ConvertedSQL(value, this.transformation andThen conversion)

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (e :SQLExpression[E, C, X]) :SQLExpression[E, C, Y] =
		if (e eq value) this.castFrom[SQLExpression[F, S, Y], SQLExpression[E, C, Y]] else transformation(e)

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] =
		transformation(e)

//	protected override def reform[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
//	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//	                             (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
//	                                       spelling :SQLSpelling)
//			:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
//		if (!passCount.hasPassed)
//			super.reform(other)(reform, passCount)
//		else
//			reform(value, other)(conversion andThen leftResult, rightResult, spelling)

//	protected override def reform[E <: RowProduct, C >: Grouped <: Single, V, U]
//	                             (other :SQLExpression[E, C, V])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[Y, U], rightResult :Lift[V, U], spelling :SQLSpelling)
//			:(SQLExpression[F, S, U], SQLExpression[E, C, U]) =
//		if (passesAllowed >= 3)
//			super.reform(other)(reform, passesAllowed)
//		else {
//			implicit val liftCompat = conversion andThen leftResult vs rightResult
//			reform(value, other)
//		}
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], V, U]
//	                             (other :LValueSQL[E, C, V])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[Y, U], rightResult :Lift[V, U], spelling :SQLSpelling)
//			:(SQLExpression[F, S, U], reform.LValue[E, C, U]) =
//		if (passesAllowed >= 3)
//			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
//		else {
//			implicit val liftCompat = conversion andThen leftResult vs rightResult
//			reform(value, other)
//		}

	protected override def visit[R[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, R]) :R[S, Y] =
		visitor.converted(this)

	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, Y, R]) :R = visitor.converted(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F_, S_, Y] <: SQLExpression[F_, S_, Y],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[RowProduct, R]) :R[S_, Y, E] =
//		visitor.converted(this)

	override def sameAs(that :CompositeSQL.__) :Boolean = that.isInstanceOf[ConvertedSQL[_, _, _, _]]

	override def canEqual(that :Any) :Boolean = that match {
		case promo :ConvertedSQL[_, _, _, _] if promo sameAs this => transformation == promo.transformation
		case _ => false
	}
	override def hashCode :Int = value.hashCode * transformation.hashCode

	override def name :String = transformation.toString
}



object ConvertedSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y]
	         (expr :SQLExpression[F, S, X], conversion :SQLConversion[X, Y]) :ConvertedSQL[F, S, X, Y] =
		expr match {
			case column :ColumnSQL[F, S, X] => ConvertedColumnSQL(column, conversion)
			case _ => new Impl(expr, conversion)
		}

	def unapply[F <: RowProduct, S >: Grouped <: Single, Y](e :SQLExpression[F, S, Y])
			:Opt[(SQLExpression[F, S, X], SQLConversion[X, Y])] forSome { type X } =
		e match {
			case promo :ConvertedSQL[F, S, x, Y] => Got((promo.value, promo.transformation))
			case _ => Lack
		}

	def unapply[F <: RowProduct, S >: Grouped <: Single, Y](e :ColumnSQL[F, S, Y])
			:Opt[(ColumnSQL[F, S, X], SQLConversion[X, Y])] forSome { type X } =
		(e :SQLExpression[F, S, Y]) match {
			case conv :ConvertedColumnSQL[F, S, x, Y] => Got((conv.value, conv.transformation))
			case conv :ConvertedSQL[F, S, x, Y] => conv.value match  {
				case column :ColumnSQL[F, S, x] => Got((column, conv.transformation))
				case _ => Lack
			}
			case _ => Lack
		}

	def OrNull[F <: RowProduct, S >: Grouped <: Single, T]
	          (expr :SQLExpression[F, S, T]) :SQLExpression[F, S, Option[T]] =
		ConvertedSQL(expr, SQLConversion.toOption[T])

	def OrNull[F <: RowProduct, S >: Grouped <: Single, T]
	          (expr :ColumnSQL[F, S, T]) :ColumnSQL[F, S, Option[T]] =
		ConvertedColumnSQL[F, S, T, Option[T]](expr, SQLConversion.toOption[T])


	trait ConvertedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, Y, +Same[v] <: ConvertibleSQL[F, S, v, Same]]
		//does not narrow down the return type because it can't implement reformTransformation
		extends TransformedSQLConvertingTemplate[F, S, X, Y, SQLExpression]
	{ self :Same[Y] =>
		override def value          :Same[X]
		override def transformation :SQLConversion[X, Y]

		protected override def convert[Z](conversion :SQLConversion[Y, Z]) :Same[Z] =
			if (conversion.isIdentity) conversion(this)
			else (transformation andThen conversion)(value)

		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.Expression[F, S, Same[U]], rightResult.Expression[F2, S2, EC2[U]]) =
			if (this eq other)
				(leftResult[F, S, Same](self), rightResult[F2, S2, EC2](other))
			else if (passCount.firstTime)
				self.passReform(other)(reform, passCount)
			else
				reform(value, other)(transformation andThen leftResult, rightResult, spelling)

	}


	trait SpecificConvertedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, Y,
	                                   +Same[-f <: RowProduct, -s >: Grouped <: Single, v] <: SpecificSQL[f, s, v, Same]]
		extends ConvertedSQLTemplate[F, S, X, Y, ({ type E[v] = Same[F, S, v] })#E]
	{ self :Same[F, S, Y] =>
		override def value :Same[F, S, X] //for convenience of being able to override a definition with a non abstract type

		override def asSingleRow :Option[Same[F, Single, Y]] =
			if (isSingleRow) Some(this.castFrom[Same[F, S, Y], Same[F, Single, Y]]) else None

		override def anchor(from :F) :Same[F, S, Y] =
			if (isAnchored(from))
				this
			else value.anchor(from) match {
				case same if same eq value => this
				case other => transformation(other)
			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :Same[E, S, Y] =
			transformation(value.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :Same[E, S, Y] =
			transformation(value.expand(base))

	}


	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	                  (override val value :SQLExpression[F, S, X], override val transformation :SQLConversion[X, Y])
		extends ConvertedSQL[F, S, X, Y]


	trait SpecificConvertedVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificConvertedColumnVisitor[F, S, V, Y] with SpecificConvertedMappingVisitor[F, S, V, Y]
	{
		def converted[X](e :ConvertedSQL[F, S, X, V]) :Y
	}
	trait MatchSpecificConverted[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificConvertedVisitor[F, S, V, Y] with CaseSpecificConvertedMapping[F, S, V, Y]
	{
		override def convertedColumn[X](e :ConvertedColumnSQL[F, S, X, V]) :Y = converted(e)
	}
	trait CaseSpecificConverted[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] extends MatchSpecificConverted[F, S, V, Y] {

		override def convertedMapping[M[A] <: MappingAt[A], X](e :ConvertedMappingSQL[F, S, M, X, V]) :Y =
			converted(e)
	}
//
//
//	trait ConvertedVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedColumnVisitor[F, Y] with ConvertedMappingVisitor[F, Y]
//	{
//		def converted[S >: Grouped <: Single, X, V]
//		             (e :ConvertedSQL[F, S, X, V]) :Y[S, V, ConvertedSQL[F, S, X, V]]
//	}
//	trait MatchConverted[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedVisitor[F, Y] with CaseConvertedMapping[F, Y]
//	{
//		override def convertedColumn[S >: Grouped <: Single, X, V]
//		                            (e :ConvertedColumnSQL[F, S, X, V]) :Y[S, V, ConvertedColumnSQL[F, S, X, V]] =
//			converted(e)
//	}
//	trait CaseConverted[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchConverted[F, Y]
//	{
//		override def convertedMapping[S >: Grouped <: Single, M[A] <: MappingAt[A], X, V]
//		                             (e :ConvertedMappingSQL[F, S, M, X, V]) :Y[S, V, ConvertedMappingSQL[F, S, M, X, V]] =
//			converted(e)
//	}

	trait AnyConvertedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyConvertedColumnVisitor[F, R] with AnyConvertedMappingVisitor[F, R]
	{
		def converted[S >: Grouped <: Single, X, Y](e :ConvertedSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyConverted[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyConvertedVisitor[F, R] with CaseAnyConvertedMapping[F, R]
	{
		override def convertedColumn[S >: Grouped <: Single, X, Y]
		                            (e :ConvertedColumnSQL[F, S, X, Y]) :R[S, Y] = converted(e)
	}
	trait CaseAnyConverted[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends MatchAnyConverted[F, R] {
		override def convertedMapping[S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y]
		                              (e :ConvertedMappingSQL[F, S, M, X, Y]) :R[S, Y] =
			converted(e)
	}
}






trait ConvertedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends ConvertedSQL[F, S, X, Y]
	   with ConvertedSQLTemplate[F, S, X, Y, ColumnSQL.from[F]#rows[S]#E]
	   with AdaptedColumnSQL[F, S, X, Y]
//	   with SpecificConvertedSQLTemplate[F, S, X, Y, ColumnSQL]
{
	//We override these because we mixed in ConvertedSQL after AdaptedColumnSQL. We did it because we want all subclasses
	// of the former to inherit its reform implementation, which would otherwise be overridden by AdaptedColumnSQL.
//	override val value         :ColumnSQL[F, S, X]

//	override def selectForm    :ColumnReadForm[Y] = transformation(value.selectForm)
//	override def universalForm :Opt[ColumnForm[Y]] = value.universalForm.map(transformation(_))
//	protected override def effectiveForm(implicit spelling :SQLSpelling) :ColumnForm[Y] =
//		transformation(spelling.effectiveForm(value))

//	protected override def convert[Z](conversion :SQLConversion[Y, Z]) :ColumnSQL[F, S, Z] =
//		if (conversion.isIdentity) conversion(this)
//		else ConvertedColumnSQL(value, this.transformation andThen conversion)

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] =
		if (e eq value) this.castFrom[ColumnSQL[F, S, Y], ColumnSQL[E, C, Y]] else transformation(e)

/*
	protected override def reformTransformation[U](leftResult :SQLTransformation[Y, U])
			:SQLTransformation.Returning[X, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, ColumnSQL[f, s, U]] })#E] =
		/* This cast doesn't look good, but its safe as long as the transformation is applied only to the value column.
		 * It cannot be composed with a transformation which changes a column into a non column expression
		 * for the exact reason why this cast is necessary.
		 */
		(transformation andThen leftResult).castFrom[
//			SQLTransformation[X, U], ReformTransformation[U, leftResult.Expression]
			SQLTransformation[X, U], SQLTransformation.Returning[X, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, ColumnSQL[f, s, U]] })#E]
		]
*/

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, Y] =
		visitor.convertedColumn[S, X, Y](this)

	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, Y, R]) :R = visitor.convertedColumn(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F_, S_, Y] <: SQLExpression[F_, S_, Y],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Y, E] =
//		visitor.convertedColumn(this)
}



object ConvertedColumnSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y]
	         (expr :ColumnSQL[F, S, X], conversion :SQLConversion[X, Y]) :ConvertedColumnSQL[F, S, X, Y] =
		new Impl(expr, conversion)

	def unapply[F <: RowProduct, S >: Grouped <: Single, Y](e :SQLExpression[F, S, Y])
			:Opt[(ColumnSQL[F, S, X], SQLConversion[X, Y])] forSome { type X } =
		e match {
			case promo :ConvertedColumnSQL[F, S, x, Y] => Got((promo.value, promo.transformation))
			case _ => Lack
		}

	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	                  (override val value :ColumnSQL[F, S, X], override val transformation :SQLConversion[X, Y])
		extends ConvertedColumnSQL[F, S, X, Y]


	trait SpecificConvertedColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificConvertedColumnMappingVisitor[F, S, X, Y]
	{
		def convertedColumn[V](e :ConvertedColumnSQL[F, S, V, X]) :Y
	}
	trait MatchSpecificConvertedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificConvertedColumnVisitor[F, S, X, Y] with CaseSpecificConvertedColumnMapping[F, S, X, Y]

	trait CaseSpecificConvertedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificConvertedColumn[F, S, X, Y]
	{
		override def convertedColumnMapping[M[O] <: ColumnAt[O], V](e :ConvertedColumnMappingSQL[F, S, M, V, X]) :Y =
			convertedColumn(e)
	}
//
//
//	trait ConvertedColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedColumnMappingVisitor[F, Y]
//	{
//		def convertedColumn[S >: Grouped <: Single, X, V]
//		                   (e :ConvertedColumnSQL[F, S, X, V]) :Y[S, V, ConvertedColumnSQL[F, S, X, V]]
//	}
//	trait MatchConvertedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedColumnVisitor[F, Y] with CaseConvertedColumnMapping[F, Y]
//
//	trait CaseConvertedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchConvertedColumn[F, Y]
//	{
//		override def convertedColumnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], X, V]
//		                                   (e :ConvertedColumnMappingSQL[F, S, M, X, V])
//				:Y[S, V, ConvertedColumnMappingSQL[F, S, M, X, V]] =
//			convertedColumn(e)
//	}


	trait AnyConvertedColumnVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyConvertedColumnMappingVisitor[F, R]
	{
		def convertedColumn[S >: Grouped <: Single, X, Y](e :ConvertedColumnSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyConvertedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyConvertedColumnVisitor[F, Y] with CaseAnyConvertedColumnMapping[F, Y]

	trait CaseAnyConvertedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyConvertedColumn[F, Y]
	{
		override def convertedColumnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], X, V]
		                                    (e :ConvertedColumnMappingSQL[F, S, M, X, V]) :Y[S, V] =
			convertedColumn(e)
	}
}







trait DecoratedSQL[-F <: RowProduct, -S >: Grouped <: Single, V] extends AdaptedSQL[F, S, V, V] {
	override def universalForm :Opt[SQLForm[V]] = value.universalForm
	override def selectForm    :SQLReadForm[V] = value.selectForm
	override def groundValue   :Opt[V] = value.groundValue

	override def transformation :SQLAdaptation[V, V] = new SelfDecoration
	protected def isUniversal   :Boolean = true
//	override def convert(x :V) :V = x

	protected class SelfDecoration extends SelfAdaptation[SQLExpression] with SQLFormIdentity[V] {
		override def isUniversal :Boolean = DecoratedSQL.this.isUniversal
		override lazy val swap :this.type = this
	}

	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[V] =
		spelling.effectiveForm(value)

	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, V, R]) :R = visitor.decorated(this)

	protected override def visit[R[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, R]) :R[S, V] = visitor.decorated(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: SQLExpression[F_, S_, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.decorated(this)
}


object DecoratedSQL {

	/** A template mixin trait for subclasses of
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL UnaryCompositeSQL]],
	  * which can wrap a column expression of any type (with any type arguments). While extending
	  * [[net.noresttherein.oldsql.sql.ast.DecoratedSQL DecoratedSQL]] is not obligatory,
	  * this implementation assumes that the forms of the underlying expression are preserved by the transformation.
	  * The type constructor `Same` of the subclass is preserved when converting its value
	  * with an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]] or grounding it in other clauses,
	  * always reemerging to the surface, hence 'floating'. The decorator delegates methods from
	  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]]
	  * and [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingOps ConvertingOps]] to the same
	  * method of the underlying expression, wrapping the result in an instance of the same class as this expression.
	  * Note that this means in particular that the result of converting this expression isn't (most likely)
	  * a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]]. Instead, its underlying expression
	  * is a `ConvertedSQL` wrapping directly `this.value`.
	  */
	trait FloatingDecoratedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                   +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                         <: ConvertibleSQL[f, s, v, ({type E[X] = Same[f, s, X]})#E]]
		extends DecoratedSQL[F, S, V]
		   with AdaptedSQLTemplate[F, S, V, V, Same]
		   with WrappedSQLTemplate[F, S, V, Same]
	{ self :Same[F, S, V] =>

		private class Wrapping[W[-f <: RowProduct, -s >: Grouped <: Single, v] >: Same[f, s, v] <: ConvertibleSQL[f, s, v, ({ type E[X] = W[f, s, X] })#E]]
			extends IndependentAdaptation[V, V]
			   with GenericTransformation[V, V, W]
			   with SelfAdaptation[W]
			   with SQLFormIdentity[V]
		{
			override type ColumnResult[-f <: RowProduct, -s >: Grouped <: Single, +E <: ColumnSQL[f, s, V]] =
				ColumnSQL[f, s, V]
			override def isSpecific  :Boolean = self.isConvertingSpecific
			override def isUniversal :Boolean = self.isUniversal

			override def apply[f <: RowProduct, s >: Grouped <: Single, E[v] <: ConvertibleSQL[f, s, v, E]]
			                  (expr :ConvertingOps[f, s, V, E]) :Same[f, s, V] =
				self.decorate(expr)

			override def column[F1 <: RowProduct, S1 >: Grouped <: Single, E[v] <: ConvertibleColumn[F1, S1, v, E]]
			                   (expr :ColumnConvertingOps[F1, S1, V, E]) :ColumnSQL[F1, S1, V] =
				self.reapply(expr.toConvertibleSQL)

			override def applyString(arg :String) :String = arg + "." + name
		}

		override def transformation :IndependentAdaptation.Convertible[V, V, Same] = new Wrapping
	}


	trait SpecificDecoratedVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificDecoratedColumnVisitor[F, S, X, Y]
	{
		def decorated(e :DecoratedSQL[F, S, X]) :Y
	}
	trait MatchSpecificDecorated[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificDecoratedVisitor[F, S, X, Y]
	{
		override def decoratedColumn(e :DecoratedColumnSQL[F, S, X]) :Y = decorated(e)
	}
	type CaseSpecificDecorated[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
		MatchSpecificDecorated[F, S, V, Y]
//
//
//	trait DecoratedVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends DecoratedColumnVisitor[F, Y]
//	{
//		def decorated[S >: Grouped <: Single, V](e :DecoratedSQL[F, S, V]) :Y[S, V, DecoratedSQL[F, S, V]]
//	}
//	trait MatchDecorated[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends DecoratedVisitor[F, Y]
//	{
//		override def decoratedColumn[S >: Grouped <: Single, V]
//		                            (e :DecoratedColumnSQL[F, S, V]) :Y[S, V, DecoratedColumnSQL[F, S, V]] = decorated(e)
//	}
//	type CaseDecorated[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		MatchDecorated[F, Y]


	trait AnyDecoratedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyDecoratedColumnVisitor[F, R]
	{
		def decorated[S >: Grouped <: Single, V](e :DecoratedSQL[F, S, V]) :R[S, V]
	}
	trait MatchAnyDecorated[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends AnyDecoratedVisitor[F, R] {
		override def decoratedColumn[S >: Grouped <: Single, V](e :DecoratedColumnSQL[F, S, V]) :R[S, V] =
			decorated(e)
	}
	type CaseAnyDecorated[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] = MatchAnyDecorated[F, R]

}




trait DecoratedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
	extends DecoratedSQL[F, S, V] with AdaptedColumnSQL[F, S, V, V]
{
	override def selectForm :ColumnReadForm[V] = value.selectForm

	protected override def effectiveForm(implicit spelling :SQLSpelling) :ColumnForm[V] =
		spelling.effectiveForm(value)

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, V] =
		visitor.decoratedColumn(this)

	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, V, R]) :R = visitor.decoratedColumn(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, Y]) :Y[S_, Boolean, E] =
//		visitor.decoratedColumn(this)
}


object DecoratedColumnSQL {

	/** A template mixin trait for subclasses of
	  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn UnaryCompositeColumn]],
	  * which can wrap a column expression of any type (with any type arguments). While extending
	  * [[net.noresttherein.oldsql.sql.ast.DecoratedColumnSQL DecoratedColumnSQL]] is not obligatory,
	  * this implementation assumes that the forms of the underlying expression are preserved by the transformation.
	  * The type constructor `Same` of the subclass is preserved when converting its value
	  * with an [[net.noresttherein.oldsql.sql.mechanics.SQLConversion SQLConversion]] or grounding it in other clauses,
	  * always reemerging to the surface, hence 'floating'.
	  * The decorator delegates methods from
	  * [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]]
	  * and [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnConvertingTemplate ConvertingColumnTemplate]] to the same
	  * method of the underlying expression, wrapping the result in an instance of the same class as this expression.
	  * Note that this means in particular that the result of converting this expression isn't (most likely)
	  * a [[net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL ConvertedColumnSQL]]. Instead, its underlying expression
	  * is a `ConvertedSQL` wrapping directly `this.value`.
	  *
	  * This trait is almost an exact clone of
	  * [[net.noresttherein.oldsql.sql.ast.DecoratedSQL.FloatingDecoratedSQLTemplate GenericDecoratedSQLTemplate]],
	  * but method [[net.noresttherein.oldsql.sql.ast.DecoratedColumnSQL.FloatingDecoratedColumnTemplate.decorate decorate]]
	  * accepts only [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] instances, rather than any expressions.
	  */
	trait FloatingDecoratedColumnTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                      +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                            <: ConvertibleColumn[f, s, v, ({ type E[X] = Same[f, s, X] })#E]]
		extends DecoratedColumnSQL[F, S, V]
		   with TransformedSQLConvertingTemplate[F, S, V, V, Same]
		   with UnaryCompositeColumnTemplate[F, S, V, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single] = Same[f, s, V] })#E]
		   with ColumnConvertingTemplate[F, S, V, ({ type E[v] = Same[F, S, v] })#E]
		   with AdaptedColumnTemplate[F, S, V, V, Same]
		{ self :Same[F, S, V] =>

			private class Wrapping[W[-f <: RowProduct, -s >: Grouped <: Single, v] >: Same[f, s, v] <: ConvertibleColumn[f, s, v, ({ type E[X] = W[f, s, X] })#E]]
				extends SelfColumnAdaptation[W] with SQLFormIdentity[V]

			override def transformation :IndependentAdaptation.Convertible[V, V, Same] = new Wrapping

			protected override def convert[Y](conversion :SQLConversion[V, Y]) :Same[F, S, Y] =
				decorate(conversion(value))

			protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, V]) :Same[E, C, V] =
				decorate(e)

			protected def decorate[E <: RowProduct, C >: Grouped <: Single, X](e :ColumnSQL[E, C, X]) :Same[E, C, X]

			protected def columnTransformation[U](leftResult :SQLTransformation[V, U])
					:SQLTransformation.Returning[V, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, Same[f, s, U]] })#E] =
				transformation andThen leftResult

//
//			protected override def reformTransformation[U](leftResult :SQLTransformation[V, U])
////					:ReformTransformation[U, leftResult.Expression] =
//					:SQLTransformation.Returning[V, U, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] = leftResult.Expression[f, s, Same[f, s, U]] })#E] =
//				(new Wrapping[Same] andThen leftResult)
		}


	trait SpecificDecoratedColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def decoratedColumn(e :DecoratedColumnSQL[F, S, X]) :Y
	}
	type MatchSpecificDecoratedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificDecoratedColumnVisitor[F, S, X, Y]
	type CaseSpecificDecoratedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificDecoratedColumnVisitor[F, S, X, Y]
//
//	trait DecoratedColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def decoratedColumn[S >: Grouped <: Single, V]
//		                   (e :DecoratedColumnSQL[F, S, V]) :Y[S, V, DecoratedColumnSQL[F, S, V]]
//	}
//	type MatchDecoratedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		DecoratedColumnVisitor[F, Y]
//	type CaseDecoratedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		DecoratedColumnVisitor[F, Y]


	trait AnyDecoratedColumnVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] {
		def decoratedColumn[S >: Grouped <: Single, V](e :DecoratedColumnSQL[F, S, V]) :R[S, V]
	}
	type MatchAnyDecoratedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyDecoratedColumnVisitor[F, Y]
	type CaseAnyDecoratedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyDecoratedColumnVisitor[F, Y]
}


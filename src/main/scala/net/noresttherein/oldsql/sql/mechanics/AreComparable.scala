package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.{implicitNotFound, showAsInfix}

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{ColumnForm, Mapping, SQLForm}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{GroundColumn, GroundSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, ConvertingTemplate, Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{ColumnLiteral, ColumnTerm, LooseComponent, LValueSQL, SQLTerm}
import net.noresttherein.oldsql.sql.ColumnSQL.{ConvertibleColumn, ConvertingColumnTemplate}




/** Attests that expressions of type `L` and `R` are compatible from the SQL point of view and
  * can be directly compared in scala after converting both sides to type
  * [[net.noresttherein.oldsql.sql.mechanics.=~=.Unified Unified]]. This is defined as existence of a pair
  * of SQL [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation conversions]] `SQLTransformation[L, Unified]`
  * and `SQLTransformation[R, Unified]`, included in this evidence. Implicit values exist only if either
  * of these conversions is [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]],
  * but an instance can be created for any compatible conversion pair using the companion object's factory method.
  *
  * @tparam L type of the left side of a comparison.
  * @tparam R type of the right side of a comparison.
  * @see [[net.noresttherein.oldsql.sql.mechanics.=~=]]
  */
@implicitNotFound("Types ${L} and ${R} are not interoperable in SQL. Missing implicit Interoperable[${L}, ${R}] " +
                  "(required SQLAdaptation[${L}, ${R}] or SQLAdaptation[${R}, ${L}]).")
sealed trait Interoperable[L, R] extends Serializable {
	//consider: maybe we should include an SQLForm[Unified]? Would help a lot during reforming, although
	// we'd have to move back to SQLTypeUnification instead of just Lift pairs.
	type As[U] = Interoperable[L, R] { type Unified = U }

	/** The type to which both types are promoted in order to be directly comparable. */
	type Unified

	/** A function lifting both `SQLExpression[_, _, L]` and type `L` itself to a comparable type `U`. */
	val left :SQLConversion[L, Unified]

	/** A function lifting both `SQLExpression[_, _, R]` and type `R` itself to a comparable type `U`. */
	val right :SQLConversion[R, Unified]

	/** Swaps the left and right side of this relation. */
	val swap :Interoperable[R, L] { type Unified = Interoperable.this.Unified }

	/** Converts the value of the left side to the value of the right side, if possible. */
	def l2r(l :L) :Opt[R] = right.unapply(left(l))

	/** Converts the value of the right side to the value of the left side, if possible. */
	def r2l(r :R) :Opt[L] = left.unapply(right(r))

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :Interoperable[_, _] if canEqual(other) && other.canEqual(this) =>
			left == other.left && right == other.right
		case _ => false
	}
	def canEqual(that :Any) :Boolean = that.isInstanceOf[Interoperable[_, _]]
	override def hashCode :Int = left.hashCode * 31 + right.hashCode

	override def toString :String = left.toString + " vs " + right
}




private[mechanics] sealed abstract class InteroperablePriority2 {
	@inline implicit def transformBoth[L, R, U](implicit left :SQLConversion[L, U], right :SQLConversion[R, U])
			:Interoperable[L, R]#As[U] =
		left vs right
}


private[mechanics] sealed abstract class InteroperablePriority1 extends InteroperablePriority2 {
	@inline implicit def transformLeft[L, R](implicit left :SQLConversion[L, R]) :Interoperable[L, R]#As[R] =
		left.asLeft

	@inline implicit def transformRight[L, R](implicit right :SQLConversion[R, L]) :Interoperable[L, R]#As[L] =
		right.asRight
}


/** Factory and implicit values of [[net.noresttherein.oldsql.sql.mechanics.Interoperable Interoperable]]. */
object Interoperable extends InteroperablePriority1 {
	type Unified[L, R, U] = Interoperable[L, R] { type Unified = U }

	@inline implicit def equivalence[L, R](implicit equiv :L =~= R) :Interoperable[L, R]#As[equiv.Unified] =
		equiv

	def apply[L, R, U](left :SQLConversion[L, U], right :SQLConversion[R, U]) :Interoperable[L, R]#As[U] =
		left vs right

	@inline def identity[V] :(V =~= V) { type Unified = V } = =~=.identity[V]
}

private abstract class StandardInteroperable[L, R, U](override val left :SQLConversion[L, U],
                                                      override val right :SQLConversion[R, U])
	extends Interoperable[L, R]
{
	override type Unified = U
}




/** Attests that expressions of type `L` and `R` are compatible from the SQL point of view and
  * can be directly compared in scala after converting both sides to type
  * [[net.noresttherein.oldsql.sql.mechanics.=~=.Unified Unified]]. For example that, for the purpose of generated SQL,
  * we treat `Option[X]` and `X` as equivalent: `(Option[X] =~= X) { type Unified = Option[X] }`.
  * Similarly, various number types can be compared after promoting both to a higher precision:
  * `(Int =~= Long) { type Unified = Long }`.
  *
  * The difference from the extended evidence `Interoperable` is that both of these conversions
  * are [[net.noresttherein.oldsql.sql.mechanics.SQLConversion conversions]] - converted
  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[L]` and `SQLExpression[R]` do not require
  * adapting by specialized [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdapterSQL]] implementations,
  * but are wrapped in one of the standard [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]]
  * @tparam L type of the left side of a comparison.
  * @tparam R type of the right side of a comparison.
  */
@implicitNotFound("Types ${L} and ${R} are not interoperable in SQL. " +
                  "Missing implicit ${L}=~=${R} (required SQLConversion[${L}, ${R}] or SQLConversion[${R}, ${L}]).")
@showAsInfix
sealed trait =~=[L, R] extends Interoperable[L, R] {
	type Widen[U] = (L =~= R) { type Unified = U }

	/** A function lifting both `SQLExpression[_, _, L]` and type `L` itself to a comparable type `U`. */
	override val left :ReversibleConversion[L, Unified]

	/** A function lifting both `SQLExpression[_, _, R]` and type `R` itself to a comparable type `U`. */
	override val right :ReversibleConversion[R, Unified]

	/** Swaps the left and right side of this relation. */
	override val swap :(R =~= L) { type Unified = =~=.this.Unified }

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[=~=[_, _]]
	override def toString :String = left + "=~=" + right
}




private[mechanics] sealed abstract class EquivalencePriority2 {
	@inline implicit def convertBoth[L, R, U](implicit left :ReversibleConversion[L, U], right :ReversibleConversion[R, U])
			:(L =~= R)#Widen[U] =
		left =~= right
}


private[mechanics] sealed abstract class EquivalencePriority1 extends EquivalencePriority2 {
	@inline implicit def convertToLeft[L, R](implicit left :ReversibleConversion[L, R]) :(L =~= R)#Widen[R] =
		left.asLeft

	@inline implicit def convertToRight[L, R](implicit right :ReversibleConversion[R, L]) :(L =~= R)#Widen[L] =
		right.asRight
}


object =~= extends EquivalencePriority1 {
	type Unified[L, R, U] = (L =~= R) { type Unified = U }

	def apply[L, R, U](left :ReversibleConversion[L, U], right :ReversibleConversion[R, U]): (L =~= R)#Widen[U] =
		left =~= right

	implicit def identity[V] :(V =~= V)#Widen[V] =
		Equiv.asInstanceOf[(V =~= V) { type Unified = V }]

	private[this] object Equiv extends (Any =~= Any) {
		override val left  = SQLConversion.toSelf
		override val right = SQLConversion.toSelf
		override type Unified = Any
		override val swap = this
	}
}


private abstract class Equivalence[L, R, U](override val left :ReversibleConversion[L, U],
                                            override val right :ReversibleConversion[R, U])
	extends (L =~= R)
{
	override type Unified = U
}




//sealed class SpecificInteroperable[LV, RV, U, -LE, +LR, -RE, +RR] private
//             (val left :SpecificTransformation[LV, U, LE, LR], val right :SpecificTransformation[RV, U, RE, RR],
//              swapped :SpecificInteroperable[RV, LV, U, RE, RR, LE, LR])
//{
//	def this(left :SpecificTransformation[LV, U, LE, LR], right :SpecificTransformation[RV, U, RE, RR]) =
//		this(left, right, null)
//
//	val swap = if (swapped != null) swapped else new SpecificInteroperable(right, left, this)
//
//	override def equals(that :Any) :Boolean = that match {
//		case other :SpecificInteroperable[_, _, _, _, _, _, _] =>
//			canEqual(other) && other.canEqual(this) && left == other.left && right == other.right
//		case _ => false
//	}
//	def canEqual(that :Any) :Boolean = that.isInstanceOf[SpecificInteroperable[_, _, _, _, _, _, _]]
//	override def hashCode :Int = left.hashCode * 31 + right.hashCode
//}
//
//
//object SpecificInteroperable {
//	/** A curried type constructor of a
//	  * [[net.noresttherein.oldsql.sql.mechanics.SpecificInteroperable SpecificInteroperable]]`[LV, RV, U, LE, LR, RE, RR]`.
//	  * @tparam U SQL expression value type to which both interoperable expressions are converted in order to unify them.
//	 */
//	type to[U]= {
//		/** A curried type constructor of a
//		  * [[net.noresttherein.oldsql.sql.mechanics.SpecificInteroperable SpecificInteroperable]]`[LV, RV, U, LE, LR, RE, RR]`.
//		  * @tparam LR The exact type of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[_, _, U]`
//		  *            to which the left ('this') expression is converted in order to make it interoperable with
//		  *            the right expression.
//		  * @tparam RR The exact type of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[_, _, U]`
//		  *            to which the right (argument) expression is converted in order to make it interoperable with
//		  *            this (left) expression.
//		  */
//		type as[+LR, +RR] = {
//			/** A curried type constructor of a
//			  * [[net.noresttherein.oldsql.sql.mechanics.SpecificInteroperable SpecificInteroperable]]`[LV, RV, U, LE, LR, RE, RR]`.
//			  * @tparam LV the value type of the left ('this') expression before the conversion.
//			  * @tparam LE a subtype of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, LV]`
//			  *            which is a supertype of the left ('this') expression before the conversion;
//			  *            it is the last type parameter given to
//			  *            [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate ConvertingTemplate]].
//			  */
//			type fromLeft[LV, -LE] = {
//				/** A type constructor of kind accepted by
//				  * [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionVisitor ExpressionVisitor]].
//				  * Accepts type arguments related to the right (argument) input expression.
//				  * @tparam RS the scope of the right expression before (and typically after) conversion.
//				  * @tparam RV the value type of the right expression before the conversion.
//				  * @tparam RE a supertype of the right expression.
//				  */
//				type visited[-RS >: Grouped <: Single, RV, -RE] = SpecificInteroperable[LV, RV, U, LE, LR, RE, RR]
//			}
//
//		}
//	}
//}






abstract class MayBeExpression[X] extends Serializable {
	type Expr <: SQLExpression[_, _, Value]
	type Value
	def apply(x :X) :Expr
}





abstract class AreComparable[-L, -R] extends Serializable {
	type Unified
//	type LeftClause <: RowProduct
//	type LeftScope >: Grouped <: Single
//	type RightClause <: RowProduct
//	type RightScope >: Grouped <: Single
//	type Left <: SQLExpression[LeftClause, LeftScope, Unified]
//	type Right <: SQLExpression[RightClause, RightScope, Unified]
	type Left <: SQLExpression[_, _, Unified]
	type Right <: SQLExpression[_, _, Unified]

	def apply(left :L, right :R) :(Left, Right)
//	def left(e :L) :Left
//	def right(e :R) :Right
//	type As[U] = AreComparable[L, R] { type Unified = U }

//	def left(e :L) :
//	def left[F <: RowProduct, S >: Grouped <: Single, EC[v] <: ConvertibleSQL[F, S, v, EC]]
//	        (e :ConvertibleSQL[F, S, L, EC]) :EC[Unified]
//
//	def right[F <: RowProduct, S >: Grouped <: Single, EC[v] <: ConvertibleSQL[F, S, v, EC]]
//	         (e :ConvertibleSQL[F, S, L, EC]) :EC[Unified]
}

object AreComparable {
//	def expressionsAreComparable[LF <: RowProduct, LS >: Grouped <: Single, LV,
//	                             RF <: RowProduct, RS >: Grouped <: Single, RV](implicit compat :LV =~= RV)
//			:AreComparable[SQLExpression[LF, LS, LV], SQLExpression[RF, RS, RV]] {
//				type Left = SQLExpression[LF, LS, compat.Unified]; type Right = SQLExpression[RF, RS, compat.Unified]
//			} =
//		new Impl[SQLExpression[LF, LS, LV], SQLExpression[LF, LS, compat.Unified],
//		         SQLExpression[RF, RS, RV], SQLExpression[RF, RS, compat.Unified], compat.Unified](
//			compat.left(_), compat.right(_)
//		)

	implicit def expressionsAreComparable
	             [LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF,  LS, v, LE],
	              RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE]]
	             (implicit compat :LV =~= RV)
			:AreComparable[ConvertingTemplate[LF, LS, LV, LE], ConvertingTemplate[RF, RS, RV, RE]] {
				type Left = LE[compat.Unified]; type Right = RE[compat.Unified]; type Unified = compat.Unified
			} =
		new Impl[ConvertingTemplate[LF, LS, LV, LE], LE[compat.Unified],
		         ConvertingTemplate[RF, RS, RV, RE], RE[compat.Unified], compat.Unified](
			compat.left(_), compat.right(_)
		)

	implicit def expressionAndLiteralAreComparable
	             [LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE], RV, U]
	             (implicit compat :(LV =~= RV)#Widen[U], form :SQLForm[U])
			:AreComparable[ConvertingTemplate[LF, LS, LV, LE], RV] {
				type Left = LE[compat.Unified]; type Right = GroundSQL[compat.Unified]; type Unified = U
			} =
		new Impl[ConvertingTemplate[LF, LS, LV, LE], LE[compat.Unified], RV, GroundSQL[compat.Unified], compat.Unified](
			compat.left(_), r => SQLTerm(compat.right(r))
		)
	implicit def columnSQLAndLiteralAreComparable
	             [LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleColumn[LF, LS, v, LE], RV, U]
	             (implicit compat :(LV =~= RV)#Widen[U], form :ColumnForm[U])
			:AreComparable[ConvertingColumnTemplate[LF, LS, LV, LE], RV] {
				type Left = LE[compat.Unified]; type Right = GroundColumn[compat.Unified]; type Unified = U
			} =
		new Impl[ConvertingColumnTemplate[LF, LS, LV, LE], LE[compat.Unified], RV, GroundColumn[compat.Unified], compat.Unified](
			compat.left(_), r => ColumnTerm(compat.right(r))
		)
	implicit def expressionAndMappingAreComparable
	             [LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
	              RF <: RowProduct, R <: Mapping, M[O] <: BaseMapping[RV, O], RV]
	             (implicit mappingType :InferTypeParams[R, M[RF], BaseMapping[RV, RF]], compat :(LV =~= RV),
	                       offset :RelationCount[RF, _ <: Numeral])
			:AreComparable[ConvertingTemplate[LF, LS, LV, LE], R] {
				type Left = LE[compat.Unified]; type Right = LValueSQL[RF, M, compat.Unified]; type Unified = compat.Unified
			} =
		new Impl[ConvertingTemplate[LF, LS, LV, LE], LE[compat.Unified], R, LValueSQL[RF, M, compat.Unified], compat.Unified](
			compat.left(_ :ConvertingTemplate[LF, LS, LV, LE]), (r :R) => compat.right(LooseComponent(mappingType(r)))
		)


	private class Impl[L, LE <: SQLExpression[_, _, U], R, RE <: SQLExpression[_, _, U], U](widen :(L, R) => (LE, RE))
		extends AreComparable[L, R]
	{
		def this(widenLeft :L => LE, widenRight :R => RE) = this((l, r) => (widenLeft(l), widenRight(r)))
		override type Unified     = U
		override type Left        = LE
		override type Right       = RE
//		override type LeftClause  = LF
//		override type LeftScope   = LS
//		override type RightClause = RF
//		override type RightScope  = RS

//		override def left(e :L) = widenLeft(e)
//		override def right(e :R) = widenRight(e)
		override def apply(left :L, right :R) :(LE, RE) = widen(left, right)
	}
}

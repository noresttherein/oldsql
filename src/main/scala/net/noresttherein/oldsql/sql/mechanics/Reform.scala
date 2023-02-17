package net.noresttherein.oldsql.sql.mechanics

import scala.collection.mutable.ArrayBuffer

import net.noresttherein.oldsql.collection.{PassedArray, Unique}
import net.noresttherein.oldsql.exceptions.{IncompatibleMappingsException, InvalidSQLException, MisalignedExpressionException, MismatchedExpressionsException, UndefinedShapeException}
import net.noresttherein.oldsql.morsels.Decorable
import net.noresttherein.oldsql.morsels.Decorable.{BaseDecorable, BaseDecorator}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.slang.classNameMethods
import net.noresttherein.oldsql.sql.{CompoundSelect, Query, RowProduct, Select, SQLExpression}
import net.noresttherein.oldsql.sql.Query.SingleQuery
import net.noresttherein.oldsql.sql.Select.SelectOperator
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, ConvertingTemplate, Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{ComponentSQL, CompoundSelectSQL, JoinedRelation, LabeledSQL, LValueSQL, QuerySQL, SelectSQL}
import net.noresttherein.oldsql.sql.ast.QuerySQL.SingleQuerySQL
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.mechanics.Reform.{PassCount, ReformDecorator, SwappedReform}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.{MayAddNullLeft, MayAddNullRight, MayExcludeLeft, MayExcludeRight, MayIncludeLeft, MayIncludeRight, MayReorderLeft, MayReorderRight, Permissions}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.{BinaryReformPermissions, DetailedReformPermissions, ForbiddenReform, ReformPermissionsDecorator}


/** Base trait for [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]]
  * and [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]], extracting all permission related
  * methods. Its companion object contains several abstract classes and traits which can be used as bases
  * for implementations of either of the former.
  *
  * @tparam Self Some extending reform implementation.
  * @define Self `ReformPermissions`
  * @define self reform
  */
trait ReformPermissions[Self <: ReformPermissions[Self]] extends Decorable[Self] { this :Self =>

	/** Can this reform (or an expression asked to reform with this reform) exclude chosen columns from the left
	  * expression to match the column set of the right expression? */
	def mayExcludeLeft  :Boolean
	/** Can this reform (or an expression asked to reform with this reform) include chosen columns from
	  * a component subexpression of the left expression, in order to match the column set of the right expression? */
	def mayIncludeLeft  :Boolean
	/** Can this reform (or an expression asked to reform with this reform) reorder the columns of the left expression
	  * to match the column set of the right expression? */
	def mayReorderLeft  :Boolean
	/** Can this reform (or an expression asked to reform with this reform) add null columns to the left expression
	  * in order to match the column set of the right expression? */
	def mayAddNullLeft  :Boolean
	/** Returns [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayExcludeLeft mayExcludeLeft]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayIncludeLeft mayIncludeLeft]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayReorderLeft mayReorderLeft]]. */
	def mayAlterLeft    :Boolean = mayIncludeLeft && mayExcludeLeft && mayReorderLeft
	/** All permissions for the left side.
	  * @return [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayExcludeLeft mayExcludeLeft]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayIncludeLeft mayIncludeLeft]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayReorderLeft mayReorderLeft]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayAddNullLeft mayAddNullLeft]]. */
	def mayReformLeft  :Boolean = mayIncludeLeft && mayExcludeLeft && mayReorderLeft && mayAddNullLeft

	/** Can this reform (or an expression asked to reform with this reform) exclude chosen columns from the right
	  * expression to match the column set of the left expression? */
	def mayExcludeRight :Boolean
	/** Can this reform (or an expression asked to reform with this reform) include chosen columns from
	  * a component subexpression of the right expression, in order to match the column set of the left expression? */
	def mayIncludeRight :Boolean
	/** Can this reform (or an expression asked to reform with this reform) reorder the columns of the right expression,
	  * to match the column set of the left expression? */
	def mayReorderRight :Boolean
	/** Can this reform (or an expression asked to reform with this reform) add null columns to the right expression
	  * in order to match the column set of the left expression? */
	def mayAddNullRight :Boolean
	/** Returns [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayExcludeRight mayExcludeRight]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayIncludeRight mayIncludeRight]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayReorderRight mayReorderRight]]. */
	def mayAlterRight   :Boolean = mayIncludeRight && mayExcludeRight && mayReorderRight
	/** All permissions for the right side.
	  * @return [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayExcludeRight mayExcludeRight]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayIncludeRight mayIncludeRight]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayReorderRight mayReorderRight]]` || `[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.mayAddNullRight mayAddNullRight]]. */
	def mayReformRight  :Boolean = mayIncludeRight && mayExcludeRight && mayReorderRight && mayAddNullRight

	/** A reform of the same policy as this one, but allowed to additionally exclude columns from the left expression
	  * to match the column set of the right expression. This call may have no effect if an implementation
	  * does not support such fine grained control. */
	def allowExcludeLeft  :Self = prohibit(mayExcludeLeft = true)
	/** A reform of the same policy as this one, but allowed to additionally include columns of the left
	  * component expression, in order to match the column set of the right expression. This call may have no effect
	  * if an implementation does not support such fine grained control. */
	def allowIncludeLeft  :Self = prohibit(mayIncludeLeft = true)
	/** A reform of the same policy as this one, but allowed to additionally reorder the columns of the left
	  *  component expression in order to match the column set of the right expression. This call may have no effect
	  *  if an implementation does not support such fine grained control. */
	def allowReorderLeft  :Self = prohibit(mayReorderLeft = true)
	/** A reform of the same policy as this one, but allowed to additionally add null columns to the left expression
	  * in order to match the column set of the right expression. This call may have no effect if an implementation
	  * does not support such fine grained control. */
	def allowAddNullLeft  :Self = prohibit(mayAddNullLeft = true)
	/** A reform of the same policy as this one, but allowed to additionally include, exclude and reorder columns
	  * of the left expression to match the right expression (but not add null columns). Similar to
	  * [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowExcludeLeft allowExcludeLeft]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowIncludeLeft allowIncludeLeft]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowReorderLeft allowReorderLeft]],
	  * except in some implementations which not support changes to individual reforming actions,
	  * the latter would result in an unchanged reform instance, but allow switching them on in tandem. */
	def allowAlterLeft   :Self =
		prohibit(mayExcludeLeft = true, mayIncludeLeft = true, mayReorderLeft = true)//, mayAddNullLeft = true)
	/** A reform of the same policy as this one, but allowed to additionally include, exclude, reorder and
	  * add null columns in the left expression to match the right expression. Similar to
	  * [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowExcludeLeft allowExcludeLeft]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowIncludeLeft allowIncludeLeft]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowReorderLeft allowReorderLeft]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowAddNullLeft allowAddNullLeft]],
	  * except in some implementations which do not support changes to individual reforming actions,
	  * the latter would result in an unchanged reform instance, but allow switching them on in tandem. */
	def allowReformLeft  :Self =
		prohibit(mayExcludeLeft = true, mayIncludeLeft = true, mayReorderLeft = true, mayAddNullLeft = true)//, mayAddNullLeft = true)

	/** A reform of the same policy as this one, but allowed to additionally exclude columns from the right expression
	  * to match the column set of the left expression. This call may have no effect if an implementation
	  * does not support such fine grained control. */
	def allowExcludeRight :Self = prohibit(mayExcludeRight = true)
	/** A reform of the same policy as this one, but allowed to additionally include columns of the right
	  * component expression, in order to match the column set of the left expression. This call may have no effect
	  * if an implementation does not support such fine grained control. */
	def allowIncludeRight :Self = prohibit(mayIncludeRight = true)
	/** A reform of the same policy as this one, but allowed to additionally reorder the columns of the right
	  *  component expression in order to match the column set of the left expression. This call may have no effect
	  *  if an implementation does not support such fine grained control. */
	def allowReorderRight :Self = prohibit(mayReorderRight = true)
	/** A reform of the same policy as this one, but allowed to additionally add null columns to the right expression
	  * in order to match the column set of the left expression. This call may have no effect if an implementation
	  * does not support such fine grained control. */
	def allowAddNullRight :Self = prohibit(mayAddNullRight = true)
	/** A reform of the same policy as this one, but allowed to additionally include, exclude and reorder columns
	  * of the right expression to match the column set of the left expression (but not add null columns). Similar to
	  * [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowExcludeRight allowExcludeRight]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowIncludeRight allowIncludeRight]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowReorderRight allowReorderRight]],
	  * except in some implementations which not support changes to individual reforming actions,
	  * the latter would result in an unchanged reform instance, but allow switching them on in tandem. */
	def allowAlterRight  :Self =
		prohibit(mayExcludeRight = true, mayIncludeRight = true, mayReorderRight = true)
	/** A reform of the same policy as this one, but allowed to additionally include, exclude, reorder and
	  * add null columns in the right expression to match the left expression. Similar to
	  * [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowExcludeRight allowExcludeRight]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowIncludeRight allowIncludeRight]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowReorderRight allowReorderRight]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.allowAddNullRight allowAddNullRight]],
	  * except in some implementations which do not support changes to individual reforming actions,
	  * the latter would result in an unchanged reform instance, but allow switching them on in tandem. */
	def allowReformRight  :Self =
		prohibit(mayExcludeRight = true, mayIncludeRight = true, mayReorderRight = true, mayAddNullRight = true)

	/** A reform of the same policy as this one, but not allowed to exclude columns from the left expression.
	  * This call may in effect prohibit also other reforming actions for the left expression if an implementation
	  * does not support such fine grained control. */
	def prohibitExcludeLeft  :Self = prohibit(mayExcludeLeft = false)
	/** A reform of the same policy as this one, but not allowed to include additional columns
	  * of component subexpressions of the left expression in order to match the column set of the right expression.
	  * This call may in effect prohibit also other reforming actions for the left expression if an implementation
	  * does not support such fine grained control. */
	def prohibitIncludeLeft  :Self = prohibit(mayIncludeLeft = false)
	/** A reform of the same policy as this one, but not allowed to reorder columns of the left expression
	  * in order to match the column set of the right expression. This call may in effect prohibit also other
	  * reforming actions for the left expression if an implementation does not support such fine grained control. */
	def prohibitReorderLeft  :Self = prohibit(mayReorderLeft = false)
	/** A reform of the same policy as this one, but not allowed to add null columns to the left expression
	  * in order to match the column set of the right expression. This call may in effect prohibit also other
	  * reforming actions for the left expression if an implementation does not support such fine grained control. */
	def prohibitAddNullLeft  :Self = prohibit(mayAddNullLeft = false)
	/** Prohibits any reforming of the left expression to match the column set of the right expression. Equivalent to
	  * [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibitExcludeLeft prohibitExcludeLeft]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibitIncludeLeft prohibitIncludeLeft]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibitReorderLeft prohibitReorderLeft]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibitAddNullLeft prohibitAddNullLeft]],
	  * but in one step. */
	def prohibitReformLeft   :Self =
		prohibit(mayExcludeLeft = false, mayIncludeLeft = false, mayReorderLeft = false, mayAddNullLeft = false)

	/** A reform of the same policy as this one, but not allowed to exclude columns from the right expression.
	  * This call may in effect prohibit also other reforming actions for the right expression if an implementation
	  * does not support such fine grained control. */
	def prohibitExcludeRight :Self = prohibit(mayExcludeRight = false)
	/** A reform of the same policy as this one, but not allowed to include additional columns
	  * of component subexpressions of the right expression in order to match the column set of the left expression.
	  * This call may in effect prohibit also other reforming actions for the right expression if an implementation
	  * does not support such fine grained control. */
	def prohibitIncludeRight :Self = prohibit(mayIncludeRight = false)
	/** A reform of the same policy as this one, but not allowed to reorder columns of the right expression
	  * in order to match the column set of the left expression. This call may in effect prohibit also other
	  * reforming actions for the right expression if an implementation does not support such fine grained control. */
	def prohibitReorderRight :Self = prohibit(mayReorderRight = false)
	/** A reform of the same policy as this one, but not allowed to add null columns to the right expression
	  * in order to match the column set of the left expression. This call may in effect prohibit also other
	  * reforming actions for the right expression if an implementation does not support such fine grained control. */
	def prohibitAddNullRight :Self = prohibit(mayAddNullRight = false)
	/** Prohibits any reforming of the right expression to match the column set of the left expression. Equivalent to
	  * [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibitExcludeRight prohibitExcludeRight]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibitIncludeRight prohibitIncludeRight]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibitReorderRight prohibitReorderRight]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibitAddNullRight prohibitAddNullRight]],
	  * but in one step. */
	def prohibitReformRight  :Self =
		prohibit(mayExcludeRight = false, mayIncludeRight = false, mayReorderRight = false, mayAddNullRight = false)
	/** Prohibits all changes done to unified expressions. The new `Reform` simply validates the input
	  * according to the same policy as this instance and throws
	  * a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
	  * if the argument expressions/queries are incompatible.
	  */
	def prohibitAll          :Self =
		prohibit(mayExcludeLeft = false, mayIncludeLeft = false, mayReorderLeft = false, mayAddNullLeft = false,
		        mayExcludeRight = false, mayIncludeRight = false, mayReorderRight = false, mayAddNullRight = false
		)

	//we can't delegate these methods if they are protected, so we either must make them public or switch back to permit
//	protected def allow(permissions :Permissions) :Self
//	protected def allow(mayExcludeLeft :Boolean = mayExcludeLeft, mayIncludeLeft :Boolean = mayIncludeLeft,
//	                    mayReorderLeft :Boolean = mayReorderLeft, mayAddNullLeft :Boolean = mayAddNullLeft,
//	                    mayExcludeRight :Boolean = mayExcludeRight, mayIncludeRight :Boolean = mayIncludeRight,
//	                    mayReorderRight :Boolean = mayReorderRight, mayAddNullRight :Boolean = mayAddNullRight) :Self =
//		allow(Permissions(
//			mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
//			mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight
//		))

	/** Creates a reform of the same type as this one, but with the given permission changes applied.
	  * Note that this is not necessarily equivalent to allowing/prohibiting these permissions individually
	  * with specific methods, as some implementations do not offer this kind of permission granularity
	  * and granting a single specific permission may have no effect, while granting a complete set at the same time
	  * may in fact result in all the permissions for the given side being changed.
	  */
	def prohibit(mayExcludeLeft :Boolean = mayExcludeLeft, mayIncludeLeft :Boolean = mayIncludeLeft,
	             mayReorderLeft :Boolean = mayReorderLeft, mayAddNullLeft :Boolean = mayAddNullLeft,
	             mayExcludeRight :Boolean = mayExcludeRight, mayIncludeRight :Boolean = mayIncludeRight,
	             mayReorderRight :Boolean = mayReorderRight, mayAddNullRight :Boolean = mayAddNullRight) :Self =
		prohibit(Permissions(
			mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
			mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight
		))

	def prohibit(permissions :Permissions) :Self
//		allow(permissions & this.permissions)
//	= permit(
//		permissions.mayExcludeLeft, permissions.mayIncludeLeft, permissions.mayReorderLeft, permissions.mayAddNullLeft,
//		permissions.mayExcludeRight, permissions.mayIncludeRight, permissions.mayReorderRight, permissions.mayAddNullRight
//	)

	/** Invokes [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibit permit]] passing as the arguments
	  * logical conjunctions of the given arguments with the corresponding permission properties on this reform.
	  */
//	def prohibit(mayExcludeLeft :Boolean = mayExcludeLeft, mayIncludeLeft :Boolean = mayIncludeLeft,
//	             mayReorderLeft :Boolean = mayReorderLeft, mayAddNullLeft :Boolean = mayAddNullLeft,
//	             mayExcludeRight :Boolean = mayExcludeRight, mayIncludeRight :Boolean = mayIncludeRight,
//	             mayReorderRight :Boolean = mayReorderRight, mayAddNullRight :Boolean = mayAddNullRight) :Self =
//		permit(
//			mayExcludeLeft && this.mayExcludeLeft, mayIncludeLeft && this.mayIncludeLeft,
//			mayReorderLeft && this.mayReorderLeft, mayAddNullLeft && this.mayAddNullLeft,
//			mayExcludeRight && this.mayExcludeRight, mayIncludeRight && this.mayIncludeRight,
//			mayReorderRight && this.mayReorderRight, mayAddNullRight && this.mayAddNullRight
//		)
//
//	def prohibit(permissions :Permissions) :Self = prohibit(
//		permissions.mayExcludeLeft || mayExcludeLeft, permissions.mayIncludeLeft || mayIncludeLeft,
//		permissions.mayReorderLeft || mayReorderLeft, permissions.mayAddNullLeft || mayAddNullLeft,
//		permissions.mayExcludeRight || mayExcludeRight, permissions.mayIncludeRight || mayIncludeRight,
//		permissions.mayReorderRight || mayReorderRight, permissions.mayAddNullRight || mayAddNullRight
//	)


	/** All permission properties of this $Self as a single, lightweight object. */
	def permissions :Permissions = new Permissions(
		(if (mayExcludeLeft) MayExcludeLeft.flags else 0) |
		(if (mayIncludeLeft) MayIncludeLeft.flags else 0) |
		(if (mayReorderLeft) MayReorderLeft.flags else 0) |
		(if (mayAddNullLeft) MayAddNullLeft.flags else 0) |
		(if (mayExcludeRight) MayExcludeRight.flags else 0) |
		(if (mayIncludeRight) MayIncludeRight.flags else 0) |
		(if (mayReorderRight) MayReorderRight.flags else 0) |
		(if (mayAddNullRight) MayAddNullRight.flags else 0)
	)

	/** Permissions in which right permission properties of this $Self are substituted for corresponding left permissions. */
	def leftPermissions :Permissions = {
		val permissions =
			(if (mayExcludeLeft) MayExcludeLeft.flags else 0) |
			(if (mayIncludeLeft) MayIncludeLeft.flags else 0) |
			(if (mayReorderLeft) MayReorderLeft.flags else 0) |
			(if (mayAddNullLeft) MayAddNullLeft.flags else 0)
		new Permissions(permissions | permissions << 4)
	}

	/** Permissions in which left permission properties of this $Self are substituted for corresponding right permissions. */
	def rightPermissions :Permissions = {
		val permissions =
			(if (mayExcludeRight) MayExcludeRight.flags else 0) |
			(if (mayIncludeRight) MayIncludeRight.flags else 0) |
			(if (mayReorderRight) MayReorderRight.flags else 0) |
			(if (mayAddNullRight) MayAddNullRight.flags else 0)
		new Permissions(permissions | permissions >>> 4)
	}

	/** A reform is symmetrical if `reform(left, right) == reform(right, left).swap`.
	  * Default implementation checks if left permissions equal right permissions, and is suitable for most reforms.
	  * Subclasses which introduce other asymmetry possibility should override this method to reflect it.
	  */
	def isSymmetrical :Boolean =
		mayExcludeLeft == mayExcludeRight && mayIncludeLeft == mayIncludeRight &&
			mayReorderLeft == mayReorderRight && mayAddNullLeft == mayAddNullRight


	@inline private def thisReform :Self = thisReform

	/** Equality of reforms is defined as equality of their [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.self self]]
	  * versions. The default implementation compares if all permissions on the reforms are equal and defers
	  * to [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.undecoratedEquals undecoratedEquals]] which compares
	  * recursively the whole `Reform` decorator stack down to the underlying reform.
	  */
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :ReformPermissions[Self @unchecked] if canEqual(other) && other.canEqual(this) =>
			permissions == other.permissions && undecoratedEquals(other.thisReform)
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that match {
		case reform :ReformPermissions[Self @unchecked] => this undecoratedCanEqual reform.thisReform
		case _ => false
	}
	override def hashCode :Int = undecoratedHashCode * 31 + permissions.flags.hashCode

	protected def permissionsString :String = permissions.leftRightString

	protected def typeName :String = this.localClassName
	override def toString :String = typeName + "(" + permissionsString + ")"
}




object ReformPermissions {
	/** A light carrier for permissions information
	  * in [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions ReformPermissions]], providing an easy way
	  * for transferring permissions as well as bulk `Boolean` logic.
	  * Each permission is represented by a single bit in the backing `Int`, with lower 4 bits used for left permissions
	  * and higher 4 bits for right permissions. See the constants defined in `ReformedPermissions` object
	  * for each individual permission.
	  *
	  * Instances can be created either through the factory method of the same name,
	  * or by combining the aforementioned `Permissions` constants with logical operators.
	  * Example:
	  * {{{
	  *     //if either of the arguments is false, clear all permissions for said side in `permissions`
	  *     def prohibit(permissions :Permissions)(mayReformLeft :Boolean, mayReformRight :Boolean) =
	  *         permissions & (MayIncludeLeft & mayReformLeft | MayReformLeft & mayReformLeft)
	  * }}}
	  */
	class Permissions private[mechanics] (private[oldsql] val flags :Int) extends AnyVal {
		@inline def unary_! :Permissions = new Permissions(~flags)
		@inline def unary_~ :Permissions = new Permissions(~flags)
		@inline def &(other :Permissions) :Permissions = new Permissions(flags & other.flags)
		@inline def |(other :Permissions) :Permissions = new Permissions(flags | other.flags)
		@inline def +(other :Permissions) :Permissions = new Permissions(flags | other.flags)
		@inline def -(other :Permissions) :Permissions = new Permissions(flags & ~other.flags)
		@inline def ^(other :Permissions) :Permissions = new Permissions(flags ^ other.flags)
		@inline def &(other :Boolean) :Permissions = if (other) this else NoReform
		@inline def ^(other :Boolean) :Permissions = if (other) new Permissions(~flags) else this

		@inline def allowed(permission :Permissions) :Boolean =
			(flags & permission.flags) == permission.flags
		@inline def ?(permission :Permissions) :Boolean =
			(flags & permission.flags) == permission.flags

		@inline def mayIncludeLeft  :Boolean = allowed(MayIncludeLeft)
		@inline def mayExcludeLeft  :Boolean = allowed(MayExcludeLeft)
		@inline def mayReorderLeft  :Boolean = allowed(MayReorderLeft)
		@inline def mayAddNullLeft  :Boolean = allowed(MayAddNullLeft)
		@inline def mayAlterLeft    :Boolean = allowed(MayAlterLeft)
		@inline def mayReformLeft   :Boolean = allowed(MayReformLeft)
		@inline def mayIncludeRight :Boolean = allowed(MayIncludeRight)
		@inline def mayExcludeRight :Boolean = allowed(MayExcludeRight)
		@inline def mayReorderRight :Boolean = allowed(MayReorderRight)
		@inline def mayAddNullRight :Boolean = allowed(MayAddNullRight)
		@inline def mayExclude      :Boolean = allowed(MayExclude)
		@inline def mayInclude      :Boolean = allowed(MayInclude)
		@inline def mayReorder      :Boolean = allowed(MayReorder)
		@inline def mayAddNull      :Boolean = allowed(MayAddNull)
		@inline def mayAlterRight   :Boolean = allowed(MayAlterRight)
		@inline def mayReformRight  :Boolean = allowed(MayReformRight)

		/** Combined left permissions, with all right permissions set to `false`.  */
		@inline def left      :Permissions  = new Permissions(flags & (MayAlterLeft.flags | MayAddNullLeft.flags))
		/** Combined right permissions, with all left permissions set to `false`.  */
		@inline def right     :Permissions  = new Permissions(flags & (MayAlterRight.flags | MayAddNullRight.flags))

		/** Substitutes right permissions for left permissions,
		  * creating [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.isSymmetrical symmetrical]]
		  * permissions. */
		@inline def asInLeft  :Permissions  = new Permissions((flags & 15) << 4 | flags & 15)

		/** Substitutes left permissions for right permissions,
		  * creating [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.isSymmetrical symmetrical]]
		  * permissions. */
		@inline def asInRight :Permissions  = new Permissions(flags & ~15 | flags >>> 4)

		/** Swaps the left permissions with right permissions, creating a mirror version of this object. */
		@inline def swap      :Permissions  = new Permissions(((flags << 4) | (flags >>> 4) & 15) & 0xff)

		/** True ''iff'' left permissions are equal right permissions. */
		@inline def isSymmetrical :Boolean = flags & 0xf == (flags >> 4) & 0xf

		/** A basis for `toString` implementations, both in this class
		  * and in [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions ReformPermissions]] subclasses.
		  * For brevity and clarity, instead of listing all permissions as Boolean values, it lists all allowed
		  * properties by name. Examples: `"left: all; right: none"`, `"left: exclude,include; right: add null"`.
		  *
		  */
		def leftRightString :String = {
			def sidePermissions(side :String, asInLeft :Permissions) =
				if (asInLeft.mayReformLeft)
					side + ": all"
				else if (asInLeft.mayAlterLeft)
					side + ": alter"
				else {
					val list = ArrayBuffer.empty[String]
					if (asInLeft.mayExcludeLeft)
						list += "exclude"
					if (asInLeft.mayIncludeLeft)
						list += "include"
					if (asInLeft.mayReorderLeft)
						list += "reorder"
					if (asInLeft.mayAddNullLeft)
						list += "add null"
					if (list.isEmpty)
						side + ": none"
					else
						list.mkString(side + ": ", ",", "")
				}
			val leftPermissions  = sidePermissions("left", this)
			val rightPermissions = sidePermissions("right", asInRight) //copy right permissions to the left side
			leftPermissions + "; " + rightPermissions
		}

		override def toString :String = "{" + leftRightString + "}"
	}

	@inline private def flag(allow :Boolean, permission :Permissions) :Int = if (allow) permission.flags else 0

	/** A factory method for [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions Permissions]]
	  * allowing to set individual permissions as arguments.
	  */
	def Permissions(mayExcludeLeft :Boolean = false, mayIncludeLeft :Boolean = false,
	                mayReorderLeft :Boolean = false, mayAddNullLeft :Boolean = false,
	                mayExcludeRight :Boolean = false, mayIncludeRight :Boolean = false,
	                mayReorderRight :Boolean = false, mayAddNullRight :Boolean = false) =
		new Permissions(
			flag(mayExcludeLeft, MayExcludeLeft) | flag(mayIncludeLeft, MayIncludeLeft) |
				flag(mayReorderLeft, MayReorderLeft) | flag(mayAddNullLeft, MayAddNullLeft) |
				flag(mayExcludeRight, MayExcludeRight) | flag(mayIncludeRight, MayIncludeRight) |
				flag(mayReorderRight, MayReorderRight) | flag(mayAddNullRight, MayAddNullRight)
		)

	final val MayExcludeLeft  = new Permissions(1)
	final val MayIncludeLeft  = new Permissions(2)
	final val MayReorderLeft  = new Permissions(4)
	final val MayAddNullLeft  = new Permissions(8)
	final val MayAlterLeft    = MayExcludeLeft | MayIncludeLeft | MayReorderLeft
	final val MayReformLeft   = MayAlterLeft | MayAddNullLeft
	final val MayExcludeRight = new Permissions(16)
	final val MayIncludeRight = new Permissions(32)
	final val MayReorderRight = new Permissions(64)
	final val MayAddNullRight = new Permissions(128)
	final val MayAlterRight   = MayExcludeRight | MayIncludeRight | MayAddNullRight
	final val MayReformRight  = MayAlterRight | MayAddNullRight
	final val MayExclude      = MayExcludeLeft | MayExcludeRight
	final val MayInclude      = MayIncludeLeft | MayIncludeRight
	final val MayReorder      = MayReorderLeft | MayReorderRight
	final val MayAddNull      = MayAddNullLeft | MayAddNullRight
	final val MayAlter        = MayAlterLeft | MayAlterRight
	final val MayReform       = MayReformLeft | MayReformRight
	final val NoReformLeft    = MayReformRight
	final val NoReformRight   = MayReformLeft
	final val NoReform        = new Permissions(0)


	abstract class ReformPermissionsDecorator[Self <: ReformPermissions[Self]]
	                                         (constructor :Self => Self)
		extends BaseDecorator[Self](constructor) with ReformPermissions[Self]
	{ this :Self =>
		override def permissions     :Permissions = decorated.permissions
		override def mayExcludeLeft  :Boolean = decorated.mayExcludeLeft
		override def mayIncludeLeft  :Boolean = decorated.mayIncludeLeft
		override def mayReorderLeft  :Boolean = decorated.mayReorderLeft
		override def mayAddNullLeft  :Boolean = decorated.mayAddNullLeft
//		override def mayAlterLeft    :Boolean = decorated.mayAlterLeft
//		override def mayReformLeft   :Boolean = decorated.mayReformLeft
		override def mayExcludeRight :Boolean = decorated.mayExcludeRight
		override def mayIncludeRight :Boolean = decorated.mayIncludeRight
		override def mayReorderRight :Boolean = decorated.mayReorderRight
		override def mayAddNullRight :Boolean = decorated.mayAddNullRight
//		override def mayAlterRight   :Boolean = decorated.mayAlterRight
//		override def mayReformRight  :Boolean = decorated.mayReformRight

		override def allowExcludeLeft  :Self = if (mayExcludeLeft) self else decorated.allowExcludeLeft
		override def allowIncludeLeft  :Self = if (mayIncludeLeft) self else decorated.allowIncludeLeft
		override def allowReorderLeft  :Self = if (mayReorderLeft) self else decorated.allowReorderLeft
		override def allowAddNullLeft  :Self = if (mayAddNullLeft) self else decorated.allowAddNullLeft
		override def allowAlterLeft    :Self = if (mayAlterLeft) self else decorated.allowAlterLeft
		override def allowReformLeft   :Self = if (mayReformLeft) self else decorated.allowReformLeft
		override def allowExcludeRight :Self = if (mayExcludeRight) self else decorated.allowExcludeRight
		override def allowIncludeRight :Self = if (mayIncludeRight) self else decorated.allowIncludeRight
		override def allowReorderRight :Self = if (mayReorderRight) self else decorated.allowReorderRight
		override def allowAddNullRight :Self = if (mayAddNullRight) self else decorated.allowAddNullRight
		override def allowAlterRight   :Self = if (mayAlterRight) self else decorated.allowAlterRight
		override def allowReformRight  :Self = if (mayReformRight) self else decorated.allowReformRight

		override def prohibitExcludeLeft  :Self = if (mayExcludeLeft) decorated.prohibitExcludeLeft else self
		override def prohibitIncludeLeft  :Self = if (mayIncludeLeft) decorated.prohibitIncludeLeft else self
		override def prohibitReorderLeft  :Self = if (mayReorderLeft) decorated.prohibitReorderLeft else self
		override def prohibitAddNullLeft  :Self = if (mayAddNullLeft) decorated.prohibitAddNullLeft else self
		override def prohibitReformLeft   :Self =
			if (mayExcludeLeft || mayIncludeLeft || mayReorderLeft || mayAddNullLeft)
				decorated.prohibitReformLeft
			else self
		override def prohibitExcludeRight :Self = if (mayExcludeRight) decorated.prohibitExcludeRight else self
		override def prohibitIncludeRight :Self = if (mayIncludeRight) decorated.prohibitIncludeRight else self
		override def prohibitReorderRight :Self = if (mayReorderRight) decorated.prohibitReorderRight else self
		override def prohibitAddNullRight :Self = if (mayAddNullRight) decorated.prohibitAddNullRight else self
		override def prohibitReformRight  :Self =
			if (mayExcludeRight || mayIncludeRight || mayReorderRight || mayAddNullRight)
				decorated.prohibitReformRight
			else self
		override def prohibitAll          :Self =
			if (mayExcludeLeft || mayIncludeLeft || mayReorderLeft || mayAddNullLeft
				|| mayExcludeRight || mayIncludeRight || mayReorderRight || mayAddNullRight)
				decorated.prohibitAll
			else self

		override def prohibit(permissions :Permissions) :Self =
			if ((permissions & this.permissions) == this.permissions) self
			else decorated.prohibit(permissions)

//		override def permit(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean,
//		                    mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
//		                    mayExcludeRight :Boolean, mayIncludeRight :Boolean,
//		                    mayReorderRight :Boolean, mayAddNullRight :Boolean) :Self =
//			if (mayExcludeLeft == this.mayExcludeLeft && mayIncludeLeft == this.mayIncludeLeft &&
//				mayReorderLeft == this.mayReorderLeft && mayAddNullLeft == this.mayAddNullLeft &&
//				mayExcludeRight == this.mayExcludeRight && mayIncludeRight == this.mayIncludeRight &&
//				mayReorderRight == this.mayReorderRight && mayAddNullRight == this.mayAddNullRight
//			)
//				self
//			else
//				decorated.permit(mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
//				                 mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight)

		override def isSymmetrical :Boolean = decorated.isSymmetrical

		override def toString :String = typeName + "(" + decorated + ")"
	}


	/** A base trait for reforms which specifies `mayAddNullLeft` and `mayAddNullRight` as false,
	  * and other permissions are derived from primary bulk properties `mayAlterLeft` and `mayAlterRight`,
	  * which should be overridden by subclasses.
	  * @tparam Self Some extending reform implementation.
	  */
	trait BinaryReformPermissions[Self <: ReformPermissions[Self]] extends ReformPermissions[Self] { this :Self =>

		override def isSymmetrical :Boolean = mayAlterLeft == mayAlterRight
		override def permissions      :Permissions = MayAlterLeft & mayAlterLeft | MayAlterRight & mayAlterRight
		override def leftPermissions  :Permissions = MayAlter & mayAlterLeft
		override def rightPermissions :Permissions = MayAlter & mayAlterRight

		override def mayExcludeLeft  :Boolean = mayAlterLeft
		override def mayIncludeLeft  :Boolean = mayAlterLeft
		override def mayReorderLeft  :Boolean = mayAlterLeft
		override def mayAddNullLeft  :Boolean = false
		override def mayReformLeft   :Boolean = false
		override def mayExcludeRight :Boolean = mayAlterRight
		override def mayIncludeRight :Boolean = mayAlterRight
		override def mayReorderRight :Boolean = mayAlterRight
		override def mayAddNullRight :Boolean = false
		override def mayReformRight  :Boolean = false

		override def allowExcludeLeft  :Self = self
		override def allowIncludeLeft  :Self = self
		override def allowReorderLeft  :Self = self
		override def allowAddNullLeft  :Self = self
		override def allowAlterLeft    :Self =
			if (mayAlterLeft) self else allow(true, mayAlterRight).self
		override def allowReformLeft   :Self = allowAlterLeft

		override def allowExcludeRight :Self = self
		override def allowIncludeRight :Self = self
		override def allowReorderRight :Self = self
		override def allowAddNullRight :Self = self
		override def allowAlterRight   :Self =
			if (mayAlterRight) self else allow(mayAlterLeft, true).self
		override def allowReformRight  :Self = allowAlterRight

		override def prohibitExcludeLeft  :Self = prohibitReformLeft
		override def prohibitIncludeLeft  :Self = prohibitReformLeft
		override def prohibitReorderLeft  :Self = prohibitReformLeft
		override def prohibitAddNullLeft  :Self = prohibitReformLeft
		override def prohibitReformLeft   :Self =
			if (mayAlterLeft) allow(false, mayAlterRight).self else self

		override def prohibitExcludeRight :Self = prohibitReformRight
		override def prohibitIncludeRight :Self = prohibitReformRight
		override def prohibitReorderRight :Self = prohibitReformRight
		override def prohibitAddNullRight :Self = prohibitReformRight
		override def prohibitReformRight  :Self =
			if (mayAlterRight) allow(mayAlterLeft, false).self else self
		override def prohibitAll          :Self =
			if (mayAlterLeft || mayAlterRight) allow(false, false).self else self

		override def prohibit(permissions :Permissions) :Self =
			//we ignore mayAddNullLeft/mayAddNullRight arguments because they are hard coded to false
			allow(permissions.allowed(MayAlterLeft) && mayAlterLeft,
			      permissions.allowed(mayAlterRight) && mayAlterRight)
//
//		override def permit(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean,
//		                    mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
//		                    mayExcludeRight :Boolean, mayIncludeRight :Boolean,
//		                    mayReorderRight :Boolean, mayAddNullRight :Boolean) :Self =
//		{
////			val left = !(mayExcludeLeft == mayIncludeLeft && mayIncludeLeft == mayReorderRight ^ mayExcludeLeft)
////			val right = !(mayExcludeRight == mayIncludeRight && mayIncludeRight == mayReorderRight ^ mayExcludeRight)
//			val left = mayExcludeLeft && mayIncludeLeft && mayReorderLeft
//			val right = mayExcludeRight && mayIncludeRight && mayReorderRight
//			if (left == this.mayReformLeft && right == this.mayReformRight) self
//			else permit(left, right).self
//		}
		protected def allow(mayReformLeft :Boolean, mayReformRight :Boolean) :Self

		override def toString :String =
			typeName + "(" (if (mayAlterLeft) "reform, " else "no reform, ") +
				(if (mayAlterRight) "reform)" else "no reform)")
	}


	/** Base class for various reform implementations which derives the values of permission properties
	  * from a [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions Permissions]] instance
	  * passed as the argument (and used as
	  * [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.permissions permissions]] field).
	  * @param permissions all given reforming permissions.
	  * @param wrap        a constructor for [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.self self]]
	  *                    representative of this instance, which will be applied to this instance
	  *                    in order to wrap it in any requested decorators.
	  * @param constructor a constructor for a concrete subclass of this class, accepting `permissions` and `wrap`
	  *                    arguments functioning identically to those in this constructor.
	  *                    It is assumed that the argument function uses de facto itself as the `constructor`
	  *                    argument passed to this super class.
	  */
	abstract class DetailedReformPermissions[Self <: ReformPermissions[Self]]
	               (override val permissions :Permissions)
	               (wrap :Self => Self, constructor :(Permissions, Self => Self) => Self)
		extends BaseDecorable[Self](wrap, constructor(permissions, _))
		   with ReformPermissions[Self]
	{ this :Self =>
		def this(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean, mayReorderLeft :Boolean, mayAddNullLeft :Boolean)
		        (mayExcludeRight :Boolean, mayIncludeRight :Boolean, mayReorderRight :Boolean, mayAddNullRight :Boolean)
		        (wrap :Self => Self,
	             constructor :(Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Self => Self) => Self) =
			this(Permissions(mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
			                 mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight))(
			     wrap,
			     (ps :Permissions, cons :Self => Self) =>
			        constructor(ps.mayExcludeLeft, ps.mayIncludeLeft, ps.mayReorderLeft, ps.mayAddNullLeft,
	                            ps.mayExcludeRight, ps.mayIncludeRight, ps.mayReorderRight, ps.mayAddNullRight, cons)
			)

		override def mayExcludeLeft   :Boolean = permissions.mayExcludeLeft
		override def mayIncludeLeft   :Boolean = permissions.mayIncludeLeft
		override def mayReorderLeft   :Boolean = permissions.mayReorderLeft
		override def mayAddNullLeft   :Boolean = permissions.mayAddNullLeft
		override def mayAlterLeft     :Boolean = permissions.mayAlterLeft
		override def mayReformLeft    :Boolean = permissions.mayReformLeft
		override def mayExcludeRight  :Boolean = permissions.mayExcludeRight
		override def mayIncludeRight  :Boolean = permissions.mayIncludeRight
		override def mayReorderRight  :Boolean = permissions.mayReorderRight
		override def mayAddNullRight  :Boolean = permissions.mayAddNullRight
		override def mayAlterRight    :Boolean = permissions.mayAlterRight
		override def mayReformRight   :Boolean = permissions.mayReformRight
		override def leftPermissions  :Permissions = permissions.asInLeft
		override def rightPermissions :Permissions = permissions.asInRight

		override def prohibit(permissions :Permissions) :Self =
			if ((permissions & this.permissions) == this.permissions) self
			else constructor(permissions & this.permissions, wrap).self
//		override def permit(permissions :Permissions) :Self =
//			if (permissions & this.permissions == this.permissions) self
//			else constructor(permissions & this.permissions, wrap).self
//
//		override def prohibit(permissions  :Permissions) :Self =
//			if (permissions & this.permissions == this.permissions) self
//			else constructor(permissions & this.permissions, wrap).self
//
//		override def permit(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean,
//		                    mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
//		                    mayExcludeRight :Boolean, mayIncludeRight :Boolean,
//		                    mayReorderRight :Boolean, mayAddNullRight :Boolean) :Self =
//			permit(Permissions(
//				mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
//				mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight
//			))
//
//		override def prohibit(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean,
//		                      mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
//		                      mayExcludeRight :Boolean, mayIncludeRight :Boolean,
//		                      mayReorderRight :Boolean, mayAddNullRight :Boolean) :Self =
//			permit(Permissions(
//				mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
//				mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight
//			))
	}


	private[mechanics] trait ForbiddenReform[Self <: ReformPermissions[Self]] extends ReformPermissions[Self] {
		this :Self =>

		override def isSymmetrical   = true
		override def mayExcludeLeft  :Boolean = false
		override def mayIncludeLeft  :Boolean = false
		override def mayReorderLeft  :Boolean = false
		override def mayAddNullLeft  :Boolean = false
		override def mayAlterLeft    :Boolean = false
		override def mayReformLeft   :Boolean = false
		override def mayExcludeRight :Boolean = false
		override def mayIncludeRight :Boolean = false
		override def mayReorderRight :Boolean = false
		override def mayAddNullRight :Boolean = false
		override def mayAlterRight   :Boolean = false
		override def mayReformRight  :Boolean = false

		override def permissions      :Permissions = NoReform
		override def leftPermissions  :Permissions = NoReform
		override def rightPermissions :Permissions = NoReform

		override def prohibit(permissions :Permissions) :Self = self
//		protected override def allow(mayExcludeLeft  :Boolean, mayIncludeLeft  :Boolean,
//		                             mayReorderLeft  :Boolean, mayAddNullLeft  :Boolean,
//		                             mayExcludeRight :Boolean, mayIncludeRight :Boolean,
//		                             mayReorderRight :Boolean, mayAddNullRight :Boolean) :Self =
//			self

		override def toString :String= typeName
	}

}




/** A strategy used to unify the columns between two SQL expressions, for example two sides
  * of an equality comparison or in an assignment to a component as a part of SQL ''insert'' or ''update''.
  * The exact definition of unifications and 'compatible' expressions may differ between the implementations.
  * By default, it is understood as compatibility of their [[net.noresttherein.oldsql.sql.SQLExpression.shape shapes]]:
  * `spelling.shape(left) <:> spelling.shape(right)`; it can however be changed both to something less strict
  * (like having equal [[net.noresttherein.oldsql.sql.SQLExpression.columnCount numbers of columns]],
  * or more strict demanding equal shapes, or [[net.noresttherein.oldsql.sql.SQLExpression.selectForm form]]
  * [[net.noresttherein.oldsql.schema.SQLReadForm.comparable compatibility]], or something else entirely.
  *
  * Standard implementations recursively delegate to
  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.reform reform]],
  * which makes callbacks on this instance for all corresponding subexpressions, with the difference between
  * implementations applying to how the final case of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] vs
  * `ComponentSQL` is handled, but in principle it can also override the main `apply` method for a pair
  * of `SQLExpression` instances and handle individually pairings between arbitrary expression types.
  * It allows comparisons and assignments between expressions which have different default column sets,
  * under certain conditions, defined by implementations.
  *
  * This strategy is employed even if the argument expressions have already an equal number of columns
  * and thus can be used for example to enforce that the types of corresponding columns match.
  *
  * Both expressions must be anchored in the same ''from'' clause and the results should only be used
  * within the context of that clause and in the [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope scope]]
  * defined by `spelling`: returned expressions may not be aligned if used in a different statement type/clause.
  * @define self reform
  * @define Self `Reform`
  */
trait Reform extends ReformPermissions[Reform] { outer =>
//	/** The type to which any [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]]`[F, M, _]`
//	  * is reformed by this instance. Reforms used to align the left and right sides
//	  * of a [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]] define it as `ComponentLValueSQL[F, M, V]`,
//	  * but generic purpose instances, as well as those used to reform ''select'' clauses of constituent SQLl ''selects''
//	  * within a ''compound select'' define it simply as its upper bound `SQLExpression[F, Single, V]`.
//	  */
//	type LValue[-F <: RowProduct, M[O] <: MappingAt[O], V] <: SQLExpression[F, Single, V]

	/** Recursively adjusts the column sets of the `left` and  `right` `SQLExpression`.
	  * This is the main method, which can delegate to any helper method (in particular,
	  * [[net.noresttherein.oldsql.sql.SQLExpression.reform reform]]) of both expressions.
	  *
	  * The 'left' and 'right' names serve only to distinguish between the expressions and allow easy referring by other
	  * parameters and variables, and do not imply that `left` is indeed the left side of an encompassing comparison
	  * expression, as the sides are routinely swapped in a back-and-forth delegation in order to allow 'eager'
	  * and 'reluctant' handling (that is, handling an expression pair at the first opportunity, or letting the other
	  * expression to implement a process of its choosing). This is true only for the initial call for the unified
	  * pair of root expressions. However, each time an `SQLExpression` delegates to other expression's
	  * [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.reform reform]] (including calling
	  * this method with swapped arguments), `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.swap swap]]
	  * is passed as the reform, which, for most implementations, delegate back to this instance with swapped
	  * arguments.
	  *
	  * By default, this method amounts to calling
	  * {{{
	  *     left.reform(right)(this, Reform.PassCount)(leftResult, rightResult, spelling)
	  * }}}
	  * Said method invokes a multiple dispatch chain in which the two expressions discover their own types
	  * and perform the unification. This process is usually possible only if they are of related types,
	  * with the exception of [[net.noresttherein.oldsql.sql.ast.SQLTerm terms]]. If neither of the expressions
	  * is able to achieve unified results, they should delegate back to one of this instance's
	  * [[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] methods. Subclasses can invoke
	  * this method through `super` reference in order to access protected `reform` method of `SQLExpression`.
	  *
	  * Most `Reform` subclasses are unlikely to need overriding of this method.
	  * @param left        the first of the expressions.
	  * @param right       the second of the expressions.
	  * @param leftResult  a conversion (possibly
	  *                    [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]])
	  *                    adapting this expression to a more generic type `U`, common with the other expression.
	  * @param rightResult a conversion (possibly
	  *                    [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]])
	  *                    adapting the other expression to a more generic type `U`, common with this expression.
	  * @param spelling    a strategy defining how an `SQLExpression` is formatted as final, textual SQL.
	  *                    It is used most notably for
	  *                    its [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
	  *                    method, in order to test if the expression's column sets match.
	  * @return `left.reform(right)(this, MaxPasses)` (unless overridden).
	  */
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UndefinedShapeException]("if this expression is not anchored/contains a LooseComponent.")
	@throws[MisalignedExpressionException]("if either of the exceptions doesn't have a definite column set (is not internally reformed).")
	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
	          RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
	         (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
	         (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
			:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
		left.`->reform`(right)(self, new PassCount(0))

	/** A method called from within `reform` family of methods of `SQLExpression` when it gives up on reforming
	  * the expression pair itself. Reforms typically do very little here except for validating if the expressions
	  * align without reforming (which is unlikely, as the check should have been already performed earlier)
	  * and throw an exception if it is not true.
	  */
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UndefinedShapeException]("if this expression is not anchored/contains a LooseComponent.")
	@throws[MisalignedExpressionException]("if either of the exceptions doesn't have a definite column set (is not internally reformed).")
	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
	             RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
	            (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
	            (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
	                      spelling :SQLSpelling)
			:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
	{
		validate(left, right)
		(leftResult(left), rightResult(right))
	}


	/** Checks if the argument expressions already have a unified form.
	  * The default implementation checks if their [[net.noresttherein.oldsql.sql.RowShape shapes]]
	  * are [[net.noresttherein.oldsql.sql.RowShape.<:> compatible]], but this can be overridden.
	  * Note that for complex expression, verifying compatibility will not necessarily be considerably faster
	  * than performing actual reforming. It is recommended to use it either for non reducible expressions
	  * or if expression's [[net.noresttherein.oldsql.sql.SQLExpression.reform reform]] method cannot simply
	  * delegate to `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.fallback fallback]].
	  *
	  * This method is used directly by [[net.noresttherein.oldsql.sql.mechanics.Reform.validate validate]]
	  * and, indirectly, by all [[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] methods.
	  * Note that it swallows all exceptions, returning false.
	  */
	//todo: create a more strict condition: we don't want to compare by default whole rows
	@throws[Nothing]
	def compatible[LF <: RowProduct, LS >: Grouped <: Single, LV, RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	              (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	              (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
	                         spelling :SQLSpelling)
			:Boolean = //ExactReform and ValidatorReform assume these semantics
		try {
			spelling.shape(left) <:> spelling.shape(right)
		} catch {
			case _ :UnsupportedOperationException | _ :InvalidSQLException => false
		}

	/** A convenience method throwing
	  * a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
	  * if check [[net.noresttherein.oldsql.sql.mechanics.Reform.compatible compatible]]`(left, right)` fails.
	  */
	@throws[MismatchedExpressionsException]("if the expression are not compatible in the meaning specific to this Reform")
	@throws[UndefinedShapeException](
		"if the shape of either expression is unknown due to featuring a LooseComponent (or a similar subexpression).")
	protected def validate[LF <: RowProduct, LS >: Grouped <: Single, LV,
	                       RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	                      (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
                          (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
                                    spelling :SQLSpelling) :Unit =
		if (!compatible(left, right))
			throw new MismatchedExpressionsException(left, right)

	/** The terminal callback in the default recursive unification of column sets between two SQL expressions,
	  * called when parallel traversing of the two expression trees encounters two component expressions
	  * in corresponding places. It is the heart of the whole process and, unlike `fallback` method,
	  * `left` does not attempt to perform reforming itself before calling it if the other expression
	  * is also a `ComponentSQL`. For this reason, while it can be implemented by delegating to `fallback(left, right)`,
	  * it remains abstract. Like other methods here, it should not delegate
	  * to method [[net.noresttherein.oldsql.sql.ast.ComponentSQL.reform reform]] of either expression,
	  * or an infinite recursion will occur.
	  *
	  * While, in principle, implementations can alter the column sets between arbitrary expressions,
	  * the main use case for this strategy derives from the possibility
	  * to [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
	  * or [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.exclude exclude]] columns
	  * for any [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]],
	  * or on the [[net.noresttherein.oldsql.schema.Table Table]] level. For this reason, a typical implementation
	  * will reform other expression types only through recursive reforming of their subexpressions, until
	  * at least one of the expressions is a `ComponentSQL`.
	  * @param left        the first expression.
	  * @param right       the second expression.
	  * @param leftResult  a conversion (possibly
	  *                    [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]])
	  *                    adapting this expression to a more generic type `U`, common with the other expression.
	  * @param rightResult a conversion (possibly
	  *                    [[net.noresttherein.oldsql.sql.mechanics.SQLConversion.toSelf identity]])
	  *                    adapting the other expression to a more generic type `U`, common with this expression.
	  * @param spelling    a strategy defining how an `SQLExpression` is formatted as final, textual SQL.
	  *                    It is used most notably for
	  *                    its [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
	  *                    method, in order to test if the expression's column sets match.
	  */
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
	           (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
	           (implicit leftResult :SQLTransformation[left.Subject, U],
	                     rightResult :SQLTransformation[right.Subject, U], spelling :SQLSpelling)
			:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]],
			  rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]])

/*
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def apply[E <: RowProduct, A >: Grouped <: Single, X,
	          L[v] <: SQLExpression[E, A, v] with ConvertingTemplate[E, A, v, L],
	          F <: RowProduct, B >: Grouped <: Single, Y,
	          R[v] <: SQLExpression[F, B, v] with ConvertingTemplate[F, B, v, R], Z]
	         (left :L[X], right :R[Y])
	         (implicit leftResult :SQLTransformation[X, Z], rightResult :SQLTransformation[Y, Z], spelling :SQLSpelling)
			:(leftResult.SQLResult[E, A, L[Z]], rightResult.SQLResult[F, B, R[Z]]) =
		left.`->reform`[F, B, Y, R, Z](right)(self, Reform.MaxPasses)
*/
/*
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV,
	          RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	         (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	         (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
	        :(SQLExpression[LF, LS, LV], SQLExpression[RF, RS, RV]) =
		left.`->reform`(right)(self, Reform.MaxPasses)

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV,
	          RF <: RowProduct, RS >: Grouped <: Single, RV,
	          RE[v] <: SQLExpression[RF, RS, v] with ConvertingTemplate[RF, RS, v, RE], U]
	         (left :SQLExpression[LF, LS, LV], right :RE[RV])
	         (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLConversion[RV, U], spelling :SQLSpelling)
			:(SQLExpression[LF, LS, U], RE[U]) =
		left.`->reform`(right)(self, Reform.MaxPasses)

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV,
	          LE[v] <: SQLExpression[LF, LS, v] with ConvertingTemplate[LF, LS, v, LE],
	          RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	         (left :LE[LV], right :SQLExpression[RF, RS, RV])
	         (implicit leftResult :SQLConversion[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
			:(LE[U], SQLExpression[RF, RS, U]) =
		left.`->convert`(right)(self, Reform.MaxPasses)

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV,
	          LE[v] <: SQLExpression[LF, LS, v] with ConvertingTemplate[LF, LS, v, LE],
	          RF <: RowProduct, RS >: Grouped <: Single, RV,
	          RE[v] <: SQLExpression[RF, RS, v] with ConvertingTemplate[RF, RS, v, RE], U]
	         (left :LE[LV], right :RE[RV])
	         (implicit leftResult :SQLConversion[LV, U], rightResult :SQLConversion[RV, U], spelling :SQLSpelling)
			:(LE[U], RE[U]) =
		left.`->convert`(right)(self, Reform.MaxPasses)


	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV,
	             RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	            (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	            (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
	        :(SQLExpression[LF, LS, LV], SQLExpression[RF, RS, RV]) =
	{
		validate(left, right)
		(leftResult(left), rightResult(right))
	}

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV,
	             RF <: RowProduct, RS >: Grouped <: Single, RV,
	             RE[v] <: SQLExpression[RF, RS, v] with ConvertingTemplate[RF, RS, v, RE], U]
	            (left :SQLExpression[LF, LS, LV], right :RE[RV])
	            (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLConversion[RV, U], spelling :SQLSpelling)
			:(SQLExpression[LF, LS, U], RE[U]) =
	{
		validate(left, right)
		(leftResult(left), rightResult(right))
	}

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV,
	             LE[v] <: SQLExpression[LF, LS, v] with ConvertingTemplate[LF, LS, v, LE],
	             RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	            (left :LE[LV], right :SQLExpression[RF, RS, RV])
	            (implicit leftResult :SQLConversion[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
			:(LE[U], SQLExpression[RF, RS, U]) =
	{
		validate(left, right)
		(leftResult(left), rightResult(right))
	}

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV,
	             LE[v] <: SQLExpression[LF, LS, v] with ConvertingTemplate[LF, LS, v, LE],
	             RF <: RowProduct, RS >: Grouped <: Single, RV,
	             RE[v] <: SQLExpression[RF, RS, v] with ConvertingTemplate[RF, RS, v, RE], U]
	            (left :LE[LV], right :RE[RV])
	            (implicit leftResult :SQLConversion[LV, U], rightResult :SQLConversion[RV, U], spelling :SQLSpelling)
			:(LE[U], RE[U]) =
	{
		validate(left, right)
		(leftResult(left), rightResult(right))
	}
*/
//
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV,
//	          LE[v] <: SQLExpression[LF, LS, v] with ConvertingTemplate[LF, LS, v, LE],
//	          RF <: RowProduct, RS >: Grouped <: Single, RV,
//	          RE[v] <: SQLExpression[RF, RS, v] with ConvertingTemplate[RF, RS, v, RE], U]
//	         (left :ConvertingTemplate[LF, LS, LV, LE], right :ConvertingTemplate[RF, RS, RV, RE])
//	         (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
//			:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
//		apply(left :ConvertingTemplate[LF, LS, LV, LE[LV]], right :ConvertingTemplate[RF, RS, RV, RE[RV]])(
//			leftResult.specific(left), rightResult.specific(right), spelling
//		)
//
//
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, RF <: RowProduct, RS >: Grouped <: Single, RV, U]
//	         (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
//	         (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
//			:(SQLExpression[LF, LS, U], SQLExpression[RF, RS, U]) =
////			:(leftResult.SQLResult[LF, LS, LEC[U]], rightResult.SQLResult[RF, RS, REC[U]]) =
//		???

/*
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LEC[v] <: SQLExpression[LF, LS, v],
	          RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	         (left :ConvertingTemplate[LF, LS, LV, LEC], right :SQLExpression[RF, RS, RV])
	         (implicit leftResult :SQLConversion[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
			:(LEC[U], SQLExpression[RF, RS, U]) =
//			:(leftResult.SQLResult[LF, LS, LEC[U]], rightResult.SQLResult[RF, RS, REC[U]]) =
		???

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV,
	          RF <: RowProduct, RS >: Grouped <: Single, RV, REC[v] <: SQLExpression[RF, RS, v], U]
	         (left :SQLExpression[LF, LS, LV], right :ConvertingTemplate[RF, RS, RV, REC])
	         (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLConversion[RV, U])
			:(SQLExpression[LF, LS, U], REC[U]) =
//			:(leftResult.SQLResult[LF, LS, LEC[U]], rightResult.SQLResult[RF, RS, REC[U]]) =
		???

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LEC[v] <: SQLExpression[LF, LS, v],
	          RF <: RowProduct, RS >: Grouped <: Single, RV, REC[v] <: SQLExpression[RF, RS, v], U]
	         (left :ConvertingTemplate[LF, LS, LV, LEC], right :ConvertingTemplate[RF, RS, RV, REC])
	         (implicit leftResult :SQLConversion[LV, U], rightResult :SQLConversion[RV, U], spelling :SQLSpelling)
			:(SQLExpression[LF, LS, U], SQLExpression[RF, RS, U]) =
//			:(leftResult.SQLResult[LF, LS, LEC[U]], rightResult.SQLResult[RF, RS, REC[U]]) =
		???

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	            (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	            (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
			:(SQLExpression[LF, LS, U], SQLExpression[RF, RS, U]) =
//			:(leftResult.SQLResult[LF, LS, LEC[U]], rightResult.SQLResult[RF, RS, REC[U]]) =
	{
		validate(left, right)
		(leftResult(left.thisExpression), rightResult(right.thisExpression))
	}

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LEC[v] <: SQLExpression[LF, LS, v],
	             RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	            (left :ConvertingTemplate[LF, LS, LV, LEC], right :SQLExpression[RF, RS, RV])
	            (implicit leftResult :SQLConversion[LV, U], rightResult :SQLTransformation[RV, U], spelling :SQLSpelling)
			:(LEC[U], SQLExpression[RF, RS, U]) =
//			:(leftResult.SQLResult[LF, LS, LEC[U]], rightResult.SQLResult[RF, RS, REC[U]]) =
	{
		validate(left.thisExpression, right)
		(leftResult(left.thisExpression), rightResult(right.thisExpression))
	}

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV,
	             RF <: RowProduct, RS >: Grouped <: Single, RV, REC[v] <: SQLExpression[RF, RS, v], U]
	            (left :SQLExpression[LF, LS, LV], right :ConvertingTemplate[RF, RS, RV, REC])
	            (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLConversion[RV, U], spelling :SQLSpelling)
			:(SQLExpression[LF, LS, U], REC[U]) =
//			:(leftResult.SQLResult[LF, LS, LEC[U]], rightResult.SQLResult[RF, RS, REC[U]]) =
	{
		validate(left, right.thisExpression)
		(leftResult(left.thisExpression), rightResult(right.thisExpression))
	}

	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LEC[v] <: SQLExpression[LF, LS, v],
	             RF <: RowProduct, RS >: Grouped <: Single, RV, REC[v] <: SQLExpression[RF, RS, v], U]
	            (left :ConvertingTemplate[LF, LS, LV, LEC], right :ConvertingTemplate[RF, RS, RV, REC])
	            (implicit leftResult :SQLConversion[LV, U], rightResult :SQLConversion[RV, U], spelling :SQLSpelling)
			:(SQLExpression[LF, LS, U], SQLExpression[RF, RS, U]) =
//			:(leftResult.SQLResult[LF, LS, LEC[U]], rightResult.SQLResult[RF, RS, REC[U]]) =
	{
		validate(left.thisExpression, right.thisExpression)
		(leftResult(left.thisExpression), rightResult(right.thisExpression))
	}
*/


//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def apply[LF <: RowProduct, LS >: Grouped <: Single, LV,
//	          LE <: SQLExpression[LF, LS, LV] with ConvertingTemplate[LF, LS, LV, LE],
//	          RF <: RowProduct, RS >: Grouped <: Single, RV,
//	          RE <: SQLExpression[RF, RS, RV] with ConvertingTemplate[RF, RS, RV, RE],
//	          U, LR <: SQLExpression[LF, LS, U], RR <: SQLExpression[RF, RS, U]]
//	         (left :ConvertingTemplate[LF, LS, LV, LE], right :ConvertingTemplate[RF, RS, RV, RE])
//	         (implicit leftResult  :SpecificTransformation[LV, U, LE, LR],
//	                   rightResult :SpecificTransformation[RV, U, RE, RR], spelling :SQLSpelling) :(LR, RR) =
//		left.`->reform`[RF, RS, RV, RE, U, LR, RR](right.thisExpression)(self, Reform.MaxPasses)
//
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV,
//	             LE <: SQLExpression[LF, LS, LV] with ConvertingTemplate[LF, LS, LV, LE],
//	             RF <: RowProduct, RS >: Grouped <: Single, RV,
//	             RE <: SQLExpression[RF, RS, RV] with ConvertingTemplate[RF, RS, RV, RE],
//	             U, LR <: SQLExpression[LF, LS, U], RR <: SQLExpression[RF, RS, U]]
//	            (left :ConvertingTemplate[LF, LS, LV, LE], right :ConvertingTemplate[RF, RS, RV, RE])
//	            (implicit leftResult  :SpecificTransformation[LV, U, LE, LR],
//	                      rightResult :SpecificTransformation[RV, U, RE, RR], spelling :SQLSpelling) :(LR, RR) =
//	{
//		validate(left.thisExpression, right.thisExpression)
//		(leftResult.sql(left.thisExpression), rightResult.sql(right.thisExpression))
//	}
//	def fallback[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
//	            (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
//	            (implicit leftResult  :SQLTransformation[LM[Unit]#Subject, U],
//	                      rightResult :SQLTransformation[RM[Unit]#Subject, U], spelling :SQLSpelling)
//			:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]],
//		      rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
//	{
//		validate(left, right)
//		(leftResult(left), rightResult(right))
//	}

//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	def fallback[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U,
//	             LR <: SQLExpression[LF, Single, U], RR <: SQLExpression[RF, Single, U]]
//	            (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
//	            (implicit leftResult  :SpecificTransformation[LM[Unit]#Subject, U, ComponentSQL[LF, LM], LR],
//	                      rightResult :SpecificTransformation[RM[Unit]#Subject, U, ComponentSQL[RF, RM], RR],
//	                      spelling :SQLSpelling) :(LR, RR) =
//		fallback[LF, Single, LM[Unit]#Subject, ComponentSQL[LF, LM],
//		         RF, Single, RM[Unit]#Subject, ComponentSQL[RF, RM], U, LR, RR](left, right)

/*
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def fallback[E <: RowProduct, A >: Grouped <: Single, X,
	             L[v] <: SQLExpression[E, A, v] with ConvertingTemplate[E, A, v, L],
	             F <: RowProduct, B >: Grouped <: Single, Y,
	             R[v] <: SQLExpression[F, B, v] with ConvertingTemplate[F, B, v, R], Z]
	            (left :L[X], right :R[Y])
	            (implicit leftResult :SQLTransformation[X, Z], rightResult :SQLTransformation[Y, Z],
	                      spelling :SQLSpelling)
			:(leftResult.SQLResult[E, A, L[Z]], rightResult.SQLResult[F, B, R[Z]]) =
	{
		validate(left, right)
		(leftResult(left), rightResult(right))
	}
*/
/*
	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
	def fallback[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
	            (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
	            (implicit leftResult :SQLTransformation[left.Subject, Z], rightResult :SQLTransformation[right.Subject, Z],
	             spelling :SQLSpelling) //todo: try to remove the bounds on -s, simple E[-_, v] should do
			:(leftResult.SQLResult[L, Single, LValueSQL[L, A, Z]],
			  rightResult.SQLResult[R, Single, LValueSQL[R, B, Z]]) =
	{
		validate(left, right)
		(leftResult(left), rightResult(right))
	}
*/


//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def apply[L <: RowProduct, A >: Grouped <: Single, X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
//	         (left :SQLExpression[L, A, X], right :SQLExpression[R, B, Y])
//	         (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//			:(SQLExpression[L, A, Z], SQLExpression[R, B, Z]) =
//		left.`->reform`(right)(self, Reform.MaxPasses)(compat.left, compat.right, spelling)
//
//	/** Adjusts the column sets of the `left` and  `right` `SQLExpression`.
//	  * This is an overloaded variant used, like the main method for `SQLExpression` pair, as an initial call,
//	  * and can also delegate to any helper method (in particular, a corresponding
//	  * [[net.noresttherein.oldsql.sql.SQLExpression.reform reform]]) of both expressions.
//	  * It is separate from the more generic variant in order to preserve the fact of being
//	  * a `ComponentLValueSQL`, that is the special type of expressions referring to table columns and components,
//	  * which can be used as a left side of an assignment in the context of an SQL ''insert'' or ''update''.
//	  * This method's use is not restricted only to the case of
//	  * [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]], but can be also called in the same general
//	  * capacity as the main method whenever one of the expressions is a `ComponentLValueSQL`.
//	  *
//	  * The method can be called both directly, or as a result of a dispatch chain between the most generic `this.apply`
//	  * and `SQLExpression.reform`. By default, it amounts to calling
//	  * {{{
//	  *     left.reform(right)(this, Reform.MaxPasses)(compat.left, compat.right, spelling)
//	  * }}}
//	  * Said method invokes a multiple dispatch chain in which the two expressions discover their own types
//	  * and perform the unification. This process is usually possible only if both expressions
//	  * are `ComponentLValueSQL` instances (sometimes any [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]]),
//	  * or `right` is an [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]]. If neither of the expressions
//	  * is able to achieve unified results, they should delegate back to one of this instance's
//	  * [[net.noresttherein.oldsql.sql.mechanics.Reform.fallback fallback]] methods. Subclasses can invoke
//	  * this method through `super` reference in order to access protected `reform` method of `SQLExpression`.
//	  *
//	  * Most `Reform` subclasses are unlikely to need overriding of this method.
//	  * @param left           the component being assigned to.
//	  * @param right          the assigned value.
//	  * @param compat         A pair of [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] conversions
//	  *                       `X => Z` and `Y => Z`, which change the Scala types used as the value types
//	  *                       of their argument expressions, without affecting the resulting SQL. This pair is used
//	  *                       to unify the types of the two expressions, and implicit instances exist
//	  *                       for basic conversions such as standard value type promotion, lifting of a type `T`
//	  *                       to `Option[T]` and similar. Methods which require two expressions, such as
//	  *                       [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]] or
//	  *                       [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.:= :=]], typically accept it
//	  *                       as an implicit parameter.
//	  *                       The returned, reformed expression pair is converted to their unified type.
//	  * @param spelling       a strategy defining how an `SQLExpression` is formatted as final, textual SQL.
//	  *                       It is used most notably for
//	  *                       its [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
//	  *                       method, in order to test if the expression's column sets match.
//	  * @return `left.reform(right, this, mayReformLeft, mayReformRight, true)` (unless overriden).
//	  */ //consider: catching the exceptions and invoking `fallback`
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
//	         (left :LValueSQL[L, A, X], right :SQLExpression[R, B, Y])
//	         (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//			:(LValue[L, A, Z], SQLExpression[R, B, Z]) =
//		left.`->reform`(right)(self, Reform.MaxPasses)(compat.left, compat.right, spelling)
//
//
//	/** Adjusts the column sets of the `left` and  `right` `SQLExpression`.
//	  * This is an overloaded variant used, like the main method for `SQLExpression` pair, as an initial call,
//	  * and can also delegate to any helper method (in particular, a corresponding
//	  * [[net.noresttherein.oldsql.sql.SQLExpression.reform reform]]) of both expressions.
//	  *
//	  * The method can be called both directly, or as a result of a dispatch chain between the most generic `this.apply`
//	  * and `SQLExpression.reform`. By default, it amounts to calling
//	  * {{{
//	  *     left.reform(right)(this, Reform.MaxPasses)(compat.left, compat.right, spelling)
//	  * }}}
//	  * Said method invokes a multiple dispatch chain in which the two expressions discover their own types
//	  * and perform the unification. This process is usually possible only if both expressions
//	  * are `ComponentLValueSQL` instances (sometimes any [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]]),
//	  * or `left` is an [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]]. If neither of the expressions
//	  * is able to achieve unified results, they should delegate back to one of this instance's
//	  * [[net.noresttherein.oldsql.sql.mechanics.Reform.fallback fallback]] methods. Subclasses can invoke
//	  * this method through `super` reference in order to access protected `reform` method of `SQLExpression`.
//	  *
//	  * Most `Reform` subclasses are unlikely to need overriding of this method.
//	  * @param left           the first expression.
//	  * @param right          the second expression.
//	  * @param compat         A pair of [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] conversions
//	  *                       `X => Z` and `Y => Z`, which change the Scala types used as the value types
//	  *                       of their argument expressions, without affecting the resulting SQL. This pair is used
//	  *                       to unify the types of the two expressions, and implicit instances exist
//	  *                       for basic conversions such as standard value type promotion, lifting of a type `T`
//	  *                       to `Option[T]` and similar. Methods which require two expressions, such as
//	  *                       [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]] or
//	  *                       [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.:= :=]], typically accept it
//	  *                       as an implicit parameter.
//	  *                       The returned, reformed expression pair is converted to their unified type.
//	  * @param spelling       a strategy defining how an `SQLExpression` is formatted as final, textual SQL.
//	  *                       It is used most notably for
//	  *                       its [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
//	  *                       method, in order to test if the expression's column sets match.
//	  * @return `left.reform(right, this, mayReformLeft, mayReformRight, true)` (unless overriden).
//	  * @return by default, and should stay consistent with,
//	  *         `swap(right, left)(mayReformRight, mayReformLeft)(compat.swap, spelling).swap`.
//	  */ //we need a mirror method with swapped arguments to properly implement Reform.swap
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def apply[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
//	         (left :SQLExpression[L, A, X], right :LValueSQL[R, B, Y])
//	         (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//			:(SQLExpression[L, A, Z], LValue[R, B, Z]) =
//		left.`->reform`(right)(self, Reform.MaxPasses)(compat.left, compat.right, spelling)
//
//	/** Adjusts the column sets of the `left` and  `right` `SQLExpression`.
//	  * This is a 'top' method which can, like the main overloaded variant for a pair of generic `SQLExpression`
//	  * instances, but unlike the method for a pair of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
//	  * expressions, delegate to any helper method (in particular, a corresponding
//	  * [[net.noresttherein.oldsql.sql.SQLExpression.reform reform]]) of both expressions.
//	  * It is used over the main method whenever the type of both expressions is known to be `ComponentLValueSQl`,
//	  * and should be preserved in the result as such.
//	  * capacity as the main method whenever one of the expressions is a `ComponentLValueSQL`.
//	  *
//	  * By default, it amounts to calling
//	  * {{{
//	  *     left.reform(right)(this, Reform.MaxPasses)(compat.left, compat.right, spelling)
//	  * }}}
//	  * Said method invokes a multiple dispatch chain in which the two expressions discover their own types
//	  * and perform the unification. If the expressions are unable to achieve unified forms,
//	  * they should delegate back to the corresponding [[net.noresttherein.oldsql.sql.mechanics.Reform.fallback fallback]]
//	  * method.
//	  *
//	  * Subclasses can invoke this method through `super` reference in order to access protected `reform` method
//	  * of `SQLExpression`. Most `Reform` subclasses are unlikely to need overriding of this method.
//	  * @param left           the first expression.
//	  * @param right          the second expression.
//	  * @param compat         A pair of [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] conversions
//	  *                       `X => Z` and `Y => Z`, which change the Scala types used as the value types
//	  *                       of their argument expressions, without affecting the resulting SQL. This pair is used
//	  *                       to unify the types of the two expressions, and implicit instances exist
//	  *                       for basic conversions such as standard value type promotion, lifting of a type `T`
//	  *                       to `Option[T]` and similar. Methods which require two expressions, such as
//	  *                       [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]] or
//	  *                       [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.:= :=]], typically accept it
//	  *                       as an implicit parameter.
//	  *                       The returned, reformed expression pair is converted to their unified type.
//	  * @param spelling       a strategy defining how an `SQLExpression` is formatted as final, textual SQL.
//	  *                       It is used most notably for
//	  *                       its [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
//	  *                       method, in order to test if the expression's column sets match.
//	  * @return `left.reform(right, this, mayReformLeft, mayReformRight, true)` (unless overriden).
//	  */
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
//	         (left :LValueSQL[L, A, X], right :LValueSQL[R, B, Y])
//	         (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//			:(LValue[L, A, Z], LValue[R, B, Z]) =
//		left.`->reform`(right)(self, Reform.MaxPasses)(compat.left, compat.right, spelling)
//
//	/** The terminal callback in the default recursive unification of column sets between two SQL expressions,
//	  * which must be implemented by subclasses. Unlike the other methods here, it should not delegate
//	  * to method [[net.noresttherein.oldsql.sql.ast.ComponentSQL.reform reform]] of either expression,
//	  * or any other method accepting a `Reform` strategy, or an infinite recursion is likely to occur.
//	  *
//	  * While, in principle, implementations can alter the column sets between arbitrary expressions,
//	  * the main use case for this strategy derives from the possibility
//	  * to [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
//	  * or [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.exclude exclude]] columns
//	  * for any [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]],
//	  * or on the [[net.noresttherein.oldsql.schema.Table Table]] level. For this reason, a typical implementation
//	  * will reform other expression types only through recursive reforming of their subexpressions, until
//	  * at least one of the expressions is a `ComponentSQL`.
//	  *
//	  * Every `Reform` subclass must implement this method, as it is the core of the whole unification process -
//	  * the most likely difference between various strategies.
//	  * @param left           the first expression.
//	  * @param right          the second expression.
//	  * @param compat         A pair of [[net.noresttherein.oldsql.sql.SQLExpression.Lift Lift]] conversions
//	  *                       `X => Z` and `Y => Z`, which change the Scala types used as the value types
//	  *                       of their argument expressions, without affecting the resulting SQL. This pair is used
//	  *                       to unify the types of the two expressions, and implicit instances exist
//	  *                       for basic conversions such as standard value type promotion, lifting of a type `T`
//	  *                       to `Option[T]` and similar. Methods which require two expressions, such as
//	  *                       [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]] or
//	  *                       [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.:= :=]], typically accept it
//	  *                       as an implicit parameter.
//	  *                       The returned, reformed expression pair is converted to their unified type.
//	  * @param spelling       a strategy defining how an `SQLExpression` is formatted as final, textual SQL.
//	  *                       It is used most notably for
//	  *                       its [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]
//	  *                       method, in order to test if the expression's column sets match.
//	  */
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def apply[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
//	         (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
//	         (implicit compat :SQLTypeUnification[left.Subject, right.Subject, Z], spelling :SQLSpelling)
//			:(LValue[L, A, Z], LValue[R, B, Z])


//
//	/** A method called from within `reform` method of `ComponentLValueSQL` when it gives up on reforming
//	  * the expression pair itself. Reforms typically do very little here except for validating if the expressions
//	  * align without reforming (which is unlikely, as the check should have been already performed earlier)
//	  * and throw an exception if it is not true.
//	  */
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def fallback[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
//	            (left :LValueSQL[L, A, X], right :SQLExpression[R, B, Y])
//	            (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//			:(LValue[L, A, Z], SQLExpression[R, B, Z]) =
//	{
//		validate(left, right)
//		(lift(left)(compat.left), right.to(compat.right))
//	}
//
//	/** A method called from within `reform` method of `SQLExpression` when the argument is a `ComponentLValueSQL`
//	  * (that is, from a callback called by the latter) and when it gives up on reforming
//	  * the expression pair itself. Reforms typically do very little here except for validating if the expressions
//	  * align without reforming (which is unlikely, as the check should have been already performed earlier)
//	  * and throw an exception if it is not true.
//	  */
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def fallback[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
//	            (left :SQLExpression[L, A, X], right :LValueSQL[R, B, Y])
//	            (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//			:(SQLExpression[L, A, Z], LValue[R, B, Z]) =
//	{
//		validate(left, right)
//		(left.to(compat.left), lift(right)(compat.right))
//	}
//
//	/** A method called from within `reform` method of `ComponentLValueSQL` when the argument
//	  * is also a `ComponentLValueSQL` (that is, from a callback called by the latter) and when it gives up on reforming
//	  * the expression pair itself. Reforms typically do very little here except for validating if the expressions
//	  * align without reforming (which is unlikely, as the check should have been already performed earlier)
//	  * and throw an exception if it is not true.
//	  */
//	@throws[MismatchedExpressionsException]("if neither of the expressions can be reformed to match the other.")
//	@throws[UnsupportedOperationException]("if this expression is not anchored and contains a LooseComponent.")
//	def fallback[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
//	            (left :LValueSQL[L, A, X], right :LValueSQL[R, B, Y])
//	            (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//			:(LValue[L, A, Z], LValue[R, B, Z]) =
//	{
//		validate(left, right)
//		(lift(left)(compat.left), lift(right)(compat.right))
//	}


	/* It is very tempting to make these methods (or at least compatible) public, so that an SQLExpression.reform
	 * implementation can check earlier if the expressions match, and only otherwise perform a much costlier reforming
	 * of the full expression, recreating the whole tree in the process.
	 * However, in most cases, this would be a simple RowShape compatibility check, which does not take into account
	 * the structure of the tree (and, for example, accept two LabeledSQL expressions with different keys,
	 * or even different numbers of elements (as long as the total columns number is equal).
	 * This definitely should be the responsibility of Reform, rather than an expression.
	 * We always have OptimisticReform mixin which always performs shape check as the first thing in apply
	 * and delegates later.
	 */

	/*
	protected def validate[LF <: RowProduct, LS >: Grouped <: Single, LV,
	                       RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	                      (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	                      (implicit compat :SQLTypeUnification[LV, RV, U], spelling :SQLSpelling) :Unit =
		if (!compatible(left, right))
			throw new MismatchedExpressionsException(left, right)
*/
//	protected def validate[LF <: RowProduct, LS >: Grouped <: Single, LV,
//	                       LE <: SQLExpression[LF, LS, LV] with ConvertingTemplate[LF, LS, LV, LE],
//	                       RF <: RowProduct, RS >: Grouped <: Single, RV,
//	                       RE <: SQLExpression[RF, RS, RV] with ConvertingTemplate[RF, RS, RV, RE],
//	                       U, LR <: SQLExpression[LF, LS, U], RR <: SQLExpression[RF, RS, U]]
//	                      (left :ConvertingTemplate[LF, LS, LV, LE], right :ConvertingTemplate[RF, RS, RV, RE])
//	                      (implicit leftResult  :SpecificTransformation[LV, U, LE, LR],
//	                                rightResult :SpecificTransformation[RV, U, RE, RR], spelling :SQLSpelling) :Unit =
//		if (!compatible(left, right))
//			throw new MismatchedExpressionsException(left.thisExpression, right.thisExpression)


	/** Checks if the argument expressions already have a unified form.
	  * The default implementation checks if their [[net.noresttherein.oldsql.sql.RowShape shapes]]
	  * are [[net.noresttherein.oldsql.sql.RowShape.<:> compatible]], but this can be overridden.
	  * This method is used directly by [[net.noresttherein.oldsql.sql.mechanics.Reform.validate validate]]
	  * and, indirectly, by all [[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] methods.
	  */
/*
	protected def compatible[LF <: RowProduct, LS >: Grouped <: Single, LV,
	                         RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	                        (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	                        (implicit compat :SQLTypeUnification[LV, RV, U], spelling :SQLSpelling) :Boolean =
		//consider: this probably should by default pass only for columns
		spelling.shape(left) <:> spelling.shape(right)
*/

//	protected def compatible[LF <: RowProduct, LS >: Grouped <: Single, LV,
//	                         LE <: SQLExpression[LF, LS, LV] with ConvertingTemplate[LF, LS, LV, LE],
//	                         RF <: RowProduct, RS >: Grouped <: Single, RV,
//	                         RE <: SQLExpression[RF, RS, RV] with ConvertingTemplate[RF, RS, RV, RE],
//	                         U, LR <: SQLExpression[LF, LS, U], RR <: SQLExpression[RF, RS, U]]
//	                        (left :ConvertingTemplate[LF, LS, LV, LE], right :ConvertingTemplate[RF, RS, RV, RE])
//	                        (implicit leftResult  :SpecificTransformation[LV, U, LE, LR],
//	                                  rightResult :SpecificTransformation[RV, U, RE, RR], spelling :SQLSpelling) :Boolean =
//		spelling.shape(left.thisExpression) <:> spelling.shape(right.thisExpression)



//	/** Converts the given l-value expression to its reformed type without actually introducing any changes
//	  * to its column set. This is an identity operation for all provided implementations.
//	  */
//	def lift[F <: RowProduct, M[O] <: MappingAt[O], X, Y](lvalue :LValueSQL[F, M, X])
//	                                                     (implicit lift :Lift[X, Y]) :LValue[F, M, Y]
//
//	/** If this reform uses the generic `SQLExpression` type as its `LValue`, it lifts the argument to the expected type:
//	  * `Got(expression.`[[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[Y])`. Otherwise simply `Lack` is returned.
//	  */
//	def lift[F <: RowProduct, M[O] <: MappingAt[O], X, Y]
//	        (expression :SQLExpression[F, Single, X])(implicit lift :Lift[X, Y]) :Opt[LValue[F, M, Y]] =
//		Lack
//
//
//

//	def queryReform[P, V](query :Query[P, V])(implicit spelling :SQLSpelling) :QueryReform = spelling.queryReform
//
//	def queryReform[F <: RowProduct, V](query :QuerySQL[F, V])(implicit spelling :SQLSpelling) :QueryReform = spelling.queryReform

//	def subreform(implicit spelling :SQLSpelling) :Reform = self

	/** The 'external' instance, that is this object wrapped in any chain of decorators.  */
	def self :Reform

	/** If this strategy processes left and right expression differently, return a mirrored version of this instance
	  * for use with swapped arguments. It is called each time
	  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.reform reform]]
	  * delegates to a method on its argument expression, effectively reversing the order within the pair.
	  * This is the outermost, effective implementation with any `Reform` decorators applied, that is it should
	  * equal [[net.noresttherein.oldsql.sql.mechanics.Reform.self self]]`.swap`.
	  */
	def swap :Reform = if ((self eq this) && isSymmetrical) this else SwappedReform(this)

//	/** Create a copy of yourself, with its `self` property set to the result of applying itself
//	  * to the given function. Returned instance is a fully decorated reform, consisting of a copy of this reform,
//	  * with all existing decorators reapplied to a copy of the underlying original reform, plus the new decorator.
//	  * @return the result of applying `decorator` constructor to a copy of this instance.
//	  */
//	def decorate(decorator :Reform => Reform) :Reform

//	override def equals(that :Any) :Boolean = that match {
//		case swap :SwappedReform if swap.undecoratedCanEqual(this) => swap undecoratedEquals this
//		case _ => super.equals(that)[[
//	}
	override def equals(that :Any) :Boolean = that match {
		case same if same eq this => true
		case swap :SwappedReform =>
			(swap swapEquals this) || (this.swap match {
				case _ :SwappedReform => false
				case other => other == swap
			})
		case _ => super.equals(that)
	}
	/** A 'mirrored' equality test called from a [[net.noresttherein.oldsql.sql.mechanics.Reform.swap swapped]]
	  * `Reform`'s `equals` method. It must compare the state of this reform with `that`, swapping
	  * the 'left' side for the 'right' side wherever pertinent. It may assume that
	  * `this.permissions == that.permissions.swap && this.undecoratedEquals(that) && that.undecoratedEquals(this)`.
	  */
	def swapEquals(that :Reform) :Boolean = false
	//a problem with this implementation is that swap is a self value,
	// so we would start comparing things from the top of the stack again.
//	= swap match {
//		case reform :SwappedReform => false
//		case reform => reform.undecoratedEquals(this)
//	}
}




object Reform {
	/** Starts counting the number of times two SQL expressions pass control between each other. */
	def PassCount = new PassCount(0)

	/** A counter specifying the number of times control has been passed between the reformed expression pair,
	  * used as an argument to
	  * `SQLExpression.`[[net.noresttherein.oldsql.sql.SQLExpression.ConvertingTemplate.reform reform]].
	  * Provides parameterless methods checking how many times the control has already passed or can yet pass
	  * for use as named predicates instead of brittle and less clear explicit comparison with `Int` constants.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.MaxPasses]]
	  */
	class PassCount(val toInt :Int) extends AnyVal {
		/** Increases the counter value by one. The increased counter is passed as the argument when
		  * `reform` method is called on an argument expression.
		  */
		@inline def ++ :PassCount = new PassCount(toInt + 1)

		/** An expression can pass the responsibility of reforming to the argument expression by calling its `reform`
		  * method (the counter hasn't yet reached its maximum legal value). */
		@inline def mayPass           :Boolean = toInt < MaxPasses
//
//		/** The argument expression to `SQLExpression.reform` will still be able to pass control back to this expression
//		  * (at least two passes are still allowed). */
//		@inline def mayPassBack       :Boolean = toInt < MaxPasses - 1

		/** This is the last time in the multiple dispatch that this expression's `reform` method is called,
		  * as the argument expression will not be able to call `SQLExpression.reform` on `this`
		  * because the pass limit has been reached. */
		@inline def lastChance        :Boolean = toInt >= MaxPasses - 1

		/** The `reform` method of this expression has been called from `reform` method of the argument expression -
		  * the expression had a chance to inspect this expression at least once and decided to pass control
		  * to this expression. This is in opposition to this expression's `reform` method being called directly
		  * by a `Reform` instance.
		  */ //a better name would be nice
		@inline def otherHasPassed    :Boolean = toInt > 0
//
//		/** This expression's `reform` method is being called at least for the second time,
//		  * the first time having passed control to the other expression's `reform` method.
//		  * @return `toInt > 1`.
//		  */
//		@inline def hasPassed         :Boolean = toInt > 1

		/** The current expression's `reform` method is called for the first time for argument `other` -
		  * `Reform` called either `this.reform`, or `other.reform`, which has delegated to `this`.
		  * @return `toInt <= 1`.
		  */
		@inline def firstTime         :Boolean = toInt <= 1

		/** The current expression's `reform` method is called for the second time (exactly) with `other`
		  * and `other.reform` has been called once or twice.
		  * @return `toInt > 1 & toInt <= 3`.
		  */
		@inline def secondTime        :Boolean = toInt > 1 & toInt <= 3

		/** The current expression's `reform` method is called for the third time (exactly) with `other`
		  * and `other.reform` has been called once or twice.
		  * @return `toInt > 1 & toInt <= 3`.
		  */
		@inline def thirdTime         :Boolean = toInt > 3 <= 5

		/** The other expression's `reform` method has been called exactly once with this expression as the argument.
		  * @return `toInt >= 1 & toInt <= 2`
		  */
		@inline def gaveOneChance     :Boolean = toInt >= 1 & toInt <= 2

		/** The other expression's `reform` method has been called exactly two times with this expression as the argument.
		  * @return `toInt >= 3 & toInt <= 5`
		  */
		@inline def gaveTwoChances    :Boolean = toInt >= 3 & toInt <= 5

		/** This expression's `reform` method is being called at least for the second time,
		  * the first time having passed control to the other expression's `reform` method.
		  * @return `toInt > 1`.
		  */
		@inline def passedOnceOrMore  :Boolean = toInt > 1

		/** This expression's `reform` method is being called at least for the third time,
		  * having previously passed control to the other expression's `reform` method.
		  * @return `toInt > 3`.
		  */
		@inline def passedTwiceOrMore :Boolean = toInt > 3

		@inline override def toString :String = toInt.toString
	}

	/** The initial value of `passesAllowed` argument of `SQLExpression.reform` given by a `Reform`. */
	final val MaxPasses = 5
//	/** Minimal value of `passesAllowed` argument of `SQLExpression.reform` which allows to delegate
//	  * to the other expression and again, from the other expression to this expression. Equals `2`.
//	  */
//	final val MayPassBack = 2
//	/** The value of `passesAllowed` argument of `SQLExpression.reform` indicating that it was invoked by the other
//	  * expression after `Reform` delegated to it. Equals `MaxPasses - 1`.
//	  */
//	final val HasPassedBack = 3

//	/** Refinement of `Reform` defining its [[net.noresttherein.oldsql.sql.mechanics.Reform.LValue LValue]] type as equal
//	  * to the type parameter.
//	  */ //consider: renaming to ReformComponent
//	type ReformTo[L[-F <: RowProduct, M[O] <: MappingAt[O], V] <: SQLExpression[F, Single, V]] =
//		Reform { type LValue[-F <: RowProduct, M[O] <: MappingAt[O], V] = L[F, M, V] }
//
//	/** An alias for [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] implementations
//	  * with [[net.noresttherein.oldsql.sql.mechanics.Reform.LValue LValue]] type
//	  * equal to [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
//	  */
//	type ExpressionReform = ReformTo[ExpressionLValue]
//
//	/** The most generic [[net.noresttherein.oldsql.sql.mechanics.Reform.LValue LValue]] type possible after reforming. */
//	type ExpressionLValue[-F <: RowProduct, M[O] <: MappingAt[O], V] = SQLExpression[F, Single, V]
//
//	/** The universal reform type used to unify arbitrary expressions which can be used to reform an `lvalue`
//	  * in a [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]], as it preserves the property
//	  * of being an ''l-value'' expression. */
//	type LValueReform = ReformTo[LValueSQL]




//	/** Base trait for `Reform`s which reform [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]]
//	  * expressions to arbitrary [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]s.
//	  */
//	trait AbstractExpressionReform extends Reform {
//		override type LValue[-F <: RowProduct, M[O] <: MappingAt[O], V] = SQLExpression[F, Single, V]
//
//		override def lift[F <: RowProduct, M[O] <: MappingAt[O], X, Y]
//		                 (lvalue :LValueSQL[F, M, X])(implicit lift :Lift[X, Y])
//				:SQLExpression[F, Single, Y] =
//			lvalue.to[Y]
//
//		override def lift[F <: RowProduct, M[O] <: MappingAt[O], X, Y]
//		                 (expression :SQLExpression[F, Single, X])(implicit lift :Lift[X, Y])
//				:Opt[SQLExpression[F, Single, Y]] =
//			Got(expression.to[Y])
//	}
//
//
//	/** An almost ubiquitous `Reform` mixin for implementations whose
//	  * [[net.noresttherein.oldsql.sql.mechanics.Reform.LValue LValue]]
//	  * is either [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]] or its supertype.
//	  * Implements `lift` method used to convert a `ComponentLValueSQL` to `LValue`.
//	  */
//	trait AbstractLValueReform extends Reform {
//		override type LValue[-F <: RowProduct, M[O] <: MappingAt[O], V] >: LValueSQL[F, M, V] <: SQLExpression[F, Single, V]
//		override def lift[F <: RowProduct, M[O] <: MappingAt[O], X, Y]
//		(lvalue :LValueSQL[F, M, X])(implicit lift :Lift[X, Y]) :LValue[F, M, Y] =
//			lvalue.to[Y]
//	}



/*
	trait SymmetricalReform extends Reform {
		override def isSymmetrical = true
		override def swap = self

		override def apply[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
		                  (left :SQLExpression[L, A, X], right :ComponentLValueSQL[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(SQLExpression[L, A, Z], LValue[R, B, Z]) =
			apply(right, left)(compat.swap, spelling).swap

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SingleQuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			apply(right, left).swap

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SelectSQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			apply(right, left).swap

		override def apply[X, Y, V](left :Query[X, V], right :SingleQuery[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			apply(right, left).swap

		override def apply[X, Y, V](left :Query[X, V], right :Select[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			apply(right, left).swap

		override def fallback[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
		                     (left :SQLExpression[L, A, X], right :ComponentLValueSQL[R, B, Y])
		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(SQLExpression[L, A, Z], LValue[R, B, Z]) =
			fallback(right, left)(compat.swap, spelling).swap
	}
*/



	/** A `Reform` mixin trait which overrides all reforming methods with paired arguments in order to perform
	  * an eager check if the expressions/queries are already of a unified form. If this check succeeds,
	  * they are returned as-is (after an optional conversion to the unified value type). Otherwise the method
	  * delegates to `super` implementation. The check itself defaults to comparing
	  * the [[net.noresttherein.oldsql.sql.SQLExpression.shape shapes]] of the two expressions, as in `Reform` itself,
	  * but can be changed by overriding [[net.noresttherein.oldsql.sql.mechanics.Reform.compatible compatible]] method.
	  *
	  * By default, the check compares the [[net.noresttherein.oldsql.sql.RowShape shapes]] of the two expressions
	  * for compatibility, but it can be easily overridden. This check is not performed by standard implementations
	  * as in many cases it would amount to a comparable cost to attempting the unification.
	  * An alternative check which does not result in a creation of unified expression
	  * would require doubling the API with a validation method mirroring
	  * [[net.noresttherein.oldsql.sql.SQLExpression.reform reform]].
	  */
	trait OptimisticReform extends Reform {
		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                            spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			if (compatible(left, right))
				(leftResult(left), rightResult(right))
			else
				super.apply(left, right)
	}



	/** Base trait form reforms other than
	  * [[net.noresttherein.oldsql.sql.mechanics.Reform.ReformDecorator ReformDecorator]].
	  * Implements equality assuming that no additional state is kept by extending classes over permission properties.
	  */
	trait BaseReform extends Reform {
		override def undecoratedEquals(that :Reform) :Boolean = that match {
			case _ if that eq this => true
			case other :ReformDecorator => other undecoratedEquals this
			case other :SwappedReform => other undecoratedEquals this
			case _ => undecoratedCanEqual(that) && that.undecoratedCanEqual(this)
		}
		override def swapEquals(that :Reform) :Boolean = true //permissions and 'canEqual' have been checked.
		override def undecoratedCanEqual(that :Reform) :Boolean = getClass == that.getClass
		override def undecoratedHashCode :Int = getClass.hashCode
	}


	/** A base class for `Reform` implementations which do not rely in their implementation on other instances,
	  * other than the [[net.noresttherein.oldsql.sql.mechanics.Reform.self self]] property passed back to
	  * reformed expressions. Implements only the support
	  * for [[net.noresttherein.oldsql.sql.mechanics.Reform.decorate decorating]].
	  * @param wrap        A decorating function, returning the effective `Reform` instance based on this one.
	  *                    Applied to `this`, it is used to initialize `self` property of this instance.
	  *                    Defaults to identity when a concrete subclass instance is first created,
	  *                    but can be a constructor of an arbitrary chain of wrapping
	  *                    [[net.noresttherein.oldsql.sql.mechanics.Reform.ReformDecorator decorators]].
	  * @param constructor The constructor function used to create instances of the concrete subclass of this class
	  *                    which provides it with this very argument. It accepts as an argument the value for
	  *                    the `wrap` argument of this class. It is used by method
	  *                    [[net.noresttherein.oldsql.sql.mechanics.Reform.DecorableReform.decorate decorate]]
	  *                    to create a new instance with its `self` property initialized to the full decorator stack.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.AbstractReform]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.ReformDecorator]]
	  */
//	abstract class DecorableReform[LV[-F <: RowProduct, M[O] <: MappingAt[O], V] <: SQLExpression[F, Single, V]]
//	               (wrap :ReformTo[LV] => ReformTo[LV], constructor :(ReformTo[LV] => ReformTo[LV]) => ReformTo[LV])
//		extends Reform
//	{
/*
	private abstract class DecorableReform(wrap :Reform => Reform, constructor :(Reform => Reform) => Reform)
		extends BaseDecorable[Reform](wrap, constructor) with BaseReform
	{
//		override type LValue[-F <: RowProduct, M[O] <: MappingAt[O], V] = LV[F, M, V]
//		override val self :Reform = wrap(this)
//		override val self :Reform = wrap(this)

//		/** Composes the given decorator constructor with whatever the existing decorating function is (`this.wrap`)
//		  * and creates a copy of this instance with that function as the `wrap` argument.
//		  * As this will create the bottom `Self`, not the external one which should be used, return the `self`
//		  * property of the former, which is the fully swap instance.
//		  */
////		override def decorate(decorator :Reform => Reform) :Reform =
//		override def decorate(decorator :Reform => Reform) :Reform =
//			constructor(wrap andThen decorator).self

		override def undecoratedEquals(that :Reform) :Boolean = that match {
			case _ if that eq this => true
			case other :ReformDecorator => other undecoratedEquals this
			case other :SwappedReform => this swapEquals other.swap
			case _ => undecoratedCanEqual(that) && that.undecoratedCanEqual(this)
		}
		override def undecoratedCanEqual(that :Reform) :Boolean = getClass == that.getClass
		override def undecoratedHashCode :Int = getClass.hashCode
	}
*/


	/** A base class for `Reform` implementations which do not rely in their implementation on other instances,
	  * other than the [[net.noresttherein.oldsql.sql.mechanics.Reform.self self]] property passed back to
	  * reformed expressions. Provides a universal [[net.noresttherein.oldsql.sql.mechanics.Reform.swap swap]]
	  * implementation and support for [[net.noresttherein.oldsql.sql.mechanics.Reform.decorate decorating]].
	  * @param wrap        A decorating function, returning the effective `Reform` instance based on this one.
	  *                    Applied to `this`, it is used to initialize `self` property of this instance.
	  *                    Defaults to identity when a concrete subclass instance is first created,
	  *                    but can be a constructor of an arbitrary chain of wrapping
	  *                    [[net.noresttherein.oldsql.sql.mechanics.Reform.ReformDecorator decorators]].
	  * @param constructor A constructor function used to create instances of the concrete subclass of this class
	  *                    which provides it with this very argument. It accepts as an argument the value for
	  *                    the `wrap` argument of this class. It is used by method
	  *                    [[net.noresttherein.oldsql.morsels.Decorable.decorate decorate]]
	  *                    to create a new instance with its `self` property initialized to the full decorator stack.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.SimpleReform]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.TunedReform]]
	  */
	abstract class AbstractReform(wrap :Reform => Reform, constructor :(Reform => Reform) => Reform)
		extends BaseDecorable[Reform](wrap, constructor) with BaseReform
	{
		override def swap :Reform = _swap
		private[this] val _swap = SwappedReform(self)
	}


	/** A base class for `Reform` implementations which do not allow for a fine grained reforming permission
	  * control and implement an 'all or nothing' approach, in which there is a single permission flag
	  * for each of the sides.
	  * @param mayAlterLeft  Allows including, excluding and reordering columns in the left expression.
	  * @param mayAlterRight Allows including, excluding and reordering columns in the right expression.
	  * @param wrap          A decorating function, returning the effective `Reform` instance based on this one.
	  *                      Applied to `this`, it is used to initialize `self` property of this instance.
	  *                      Defaults to identity when a concrete subclass instance is first created,
	  *                      but can be a constructor of an arbitrary chain of wrapping
	  *                      [[net.noresttherein.oldsql.sql.mechanics.Reform.ReformDecorator decorators]].
	  * @param constructor   The constructor function used to create instances of the concrete subclass of this class
	  *                      which provides it with this very argument. It accepts as an argument the value for
	  *                      the `wrap` argument of this class. It is used by method
	  *                      [[net.noresttherein.oldsql.morsels.Decorable.decorate decorate]]
	  *                      to create a new instance with its `self` property initialized to the full decorator stack.
	  */
	abstract class SimpleReform(override val mayAlterLeft :Boolean, override val mayAlterRight :Boolean)
	                           (wrap :Reform => Reform, constructor :(Boolean, Boolean, Reform => Reform) => Reform)
		extends BaseDecorable[Reform](wrap, constructor(mayAlterLeft, mayAlterRight, _))
		   with BinaryReformPermissions[Reform] with BaseReform
	{
		protected override def allow(mayReformLeft :Boolean, mayReformRight :Boolean) :Reform =
			constructor(mayReformLeft, mayReformRight, wrap).self
/*
		override def isSymmetrical :Boolean = mayReformLeft == mayReformRight
//		override lazy val swap :ReformTo[LV] = SwappedReform[LV](self)
		override def mayExcludeLeft  :Boolean = mayReformLeft
		override def mayIncludeLeft  :Boolean = mayReformLeft
		override def mayReorderLeft  :Boolean = mayReformLeft
		override def mayAddNullLeft  :Boolean = false //mayReformLeft
		override def mayExcludeRight :Boolean = mayReformRight
		override def mayIncludeRight :Boolean = mayReformRight
		override def mayReorderRight :Boolean = mayReformRight
		override def mayAddNullRight :Boolean = false //mayReformRight

		override def allowExcludeLeft  :Reform = self
		override def allowIncludeLeft  :Reform = self
		override def allowReorderLeft  :Reform = self
		override def allowAddNullLeft  :Reform = self
		override def allowReformLeft   :Reform =
			if (mayReformLeft) self else constructor(true, mayReformRight, wrap).self

		override def allowExcludeRight :Reform = self
		override def allowIncludeRight :Reform = self
		override def allowReorderRight :Reform = self
		override def allowAddNullRight :Reform = self
		override def allowReformRight  :Reform =
			if (mayReformRight) self else constructor(mayReformLeft, true, wrap).self

		override def prohibitExcludeLeft  :Reform = prohibitReformLeft
		override def prohibitIncludeLeft  :Reform = prohibitReformLeft
		override def prohibitReorderLeft  :Reform = prohibitReformLeft
		override def prohibitAddNullLeft  :Reform = prohibitReformLeft
		override def prohibitReformLeft   :Reform =
			if (mayReformLeft) constructor(false, mayReformRight, wrap).self else self

		override def prohibitExcludeRight :Reform = prohibitReformRight
		override def prohibitIncludeRight :Reform = prohibitReformRight
		override def prohibitReorderRight :Reform = prohibitReformRight
		override def prohibitAddNullRight :Reform = prohibitReformRight
		override def prohibitReformRight  :Reform =
			if (mayReformRight) constructor(mayReformLeft, false, wrap).self else self
		override def prohibitAll          :Reform =
			if (mayReformLeft || mayReformRight) constructor(false, false, wrap).self else self

		override def permit(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean,
		                    mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
		                    mayExcludeRight :Boolean, mayIncludeRight :Boolean,
		                    mayReorderRight :Boolean, mayAddNullRight :Boolean) :Reform =
		{   //we ignore mayAddNullLeft/mayAddNullRight arguments because they are hard coded to false
//			val left = !(mayExcludeLeft == mayIncludeLeft && mayIncludeLeft == mayReorderRight ^ mayExcludeLeft)
//			val right = !(mayExcludeRight == mayIncludeRight && mayIncludeRight == mayReorderRight ^ mayExcludeRight)
			val left = mayExcludeLeft && mayIncludeLeft && mayReorderLeft
			val right = mayExcludeRight && mayIncludeRight && mayReorderRight
			if (left == this.mayReformLeft && right == this.mayReformRight) self
			else constructor(left, right, wrap).self
		}*/

		override lazy val swap :Reform =
			if (self.isSymmetrical)
				self
			else if (self eq this)
				constructor(mayAlterRight, mayAlterLeft, wrap).self
			else
				SwappedReform(this)
	}



	/** A base class for [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] implementations
	  * other than [[net.noresttherein.oldsql.sql.mechanics.Reform.ReformDecorator decorators]].
	  * It defines separate properties for all reforming permission flags, and assumes that extending classes
	  * do not introduce other directional (left/right specific) state.
	  * @param permissions Specification of the nature of modifications that the reform may apply
	  *                    to the reformed expressions
	  * @param wrap        A decorating function, returning the effective `Reform` instance based on this one.
	  *                    Applied to `this`, it is used to initialize `self` property of this instance.
	  *                    Defaults to identity when a concrete subclass instance is first created,
	  *                    but can be a constructor of an arbitrary chain of wrapping
	  *                    [[net.noresttherein.oldsql.sql.mechanics.Reform.ReformDecorator decorators]].
	  * @param constructor A constructor function used to create instances of the concrete subclass of this class
	  *                    which provides it with this very argument. It accepts as an argument the value for
	  *                    the `wrap` argument of this class. It is used by method
	  *                    [[net.noresttherein.oldsql.morsels.Decorable.decorate decorate]]
	  *                    to create a new instance with its `self` property initialized to the full decorator stack.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.AbstractReform]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.SimpleReform]]
	  */
	abstract class TunedReform(permissions :Permissions)
	                          (wrap :Reform => Reform, constructor :(Permissions, Reform => Reform) => Reform)
		extends DetailedReformPermissions[Reform](permissions)(wrap, constructor)
		   with BaseReform
	{
		override lazy val swap :Reform =
			if (self eq this)
				if (isSymmetrical)
					self
				else
					constructor(permissions.swap, wrap).self
			else
				SwappedReform(this)
	}



	/** Base class for `Reform` implementations which delegate to another `Reform` instance
	  * [[net.noresttherein.oldsql.morsels.Decorable.Decorator.decorated decorated]].
	  */
	abstract class ReformDecorator(constructor :Reform => Reform)
		extends ReformPermissionsDecorator[Reform](constructor) with Reform
	{
		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                            spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			decorated(left, right)

		override def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                      RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                     (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                     (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                               spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			decorated.fallback(left, right)

		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
		                    (implicit leftResult  :SQLTransformation[left.Subject, U],
		                              rightResult :SQLTransformation[right.Subject, U], spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
			decorated.default(left, right)

		override def compatible[LF <: RowProduct, LS >: Grouped <: Single, LV, RF <: RowProduct, RS >: Grouped <: Single, RV, U]
		                       (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
		                       (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                                 spelling :SQLSpelling) :Boolean =
			decorated.compatible(left, right)

/*
		override def apply[E <: RowProduct, A >: Grouped <: Single, X,
		                   L[v] <: SQLExpression[E, A, v] with ConvertingTemplate[E, A, v, L],
		                   F <: RowProduct, B >: Grouped <: Single, Y,
		                   R[v] <: SQLExpression[F, B, v] with ConvertingTemplate[F, B, v, R], Z]
		                  (left :L[X], right :R[Y])
		                  (implicit leftResult :SQLTransformation[X, Z], rightResult :SQLTransformation[Y, Z], spelling :SQLSpelling)
				:(leftResult.SQLResult[E, A, L[Z]], rightResult.SQLResult[F, B, R[Z]]) =
			decorated(left, right)
*/
//		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE <: SQLExpression[LF, LS, LV],
//		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE <: SQLExpression[RF, RS, RV],
//		                   U, LR <: SQLExpression[LF, LS, U], RR <: SQLExpression[RF, RS, U]]
//		                  (left :LE, right :RE)
//		                  (implicit leftResult  :SpecificTransformation[LF, LS, LV, U, LE, LR],
//		                            rightResult :SpecificTransformation[RF, RS, RV, U, RE, RR], spelling :SQLSpelling)
//				:(LR, RR) =
//			decorated(left, right)
/*
		override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
		                  (left :LValueSQL[L, A, X], right :SQLExpression[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(LValue[L, A, Z], SQLExpression[R, B, Z]) =
			decorated(left, right)

		override def apply[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
		                  (left :SQLExpression[L, A, X], right :LValueSQL[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(SQLExpression[L, A, Z], LValue[R, B, Z]) =
			decorated(left, right)

		override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
		                  (left :LValueSQL[L, A, X], right :LValueSQL[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(LValue[L, A, Z], LValue[R, B, Z]) =
			decorated(left, right)
*/
//		override def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LE <: SQLExpression[LF, LS, LV],
//                              RF <: RowProduct, RS >: Grouped <: Single, RV, RE <: SQLExpression[RF, RS, RV],
//                              U, LR <: SQLExpression[LF, LS, U], RR <: SQLExpression[RF, RS, U]]
//                             (left :LE, right :RE)
//                             (implicit leftResult  :SpecificTransformation[LF, LS, LV, U, LE, LR],
//                                       rightResult :SpecificTransformation[RF, RS, RV, U, RE, RR], spelling :SQLSpelling)
//				:(LR, RR) =
//			decorated(left, right)

/*
		override def fallback[E <: RowProduct, A >: Grouped <: Single, X,
		                      L[v] <: SQLExpression[E, A, v] with ConvertingTemplate[E, A, v, L],
		                      F <: RowProduct, B >: Grouped <: Single, Y,
		                      R[v] <: SQLExpression[F, B, v] with ConvertingTemplate[F, B, v, R], Z]
		                     (left :L[X], right :R[Y])
		                     (implicit leftResult :SQLTransformation[X, Z], rightResult :SQLTransformation[Y, Z],
		                               spelling :SQLSpelling)
				:(leftResult.SQLResult[E, A, L[Z]], rightResult.SQLResult[F, B, R[Z]]) =
			decorated.fallback(left, right)

		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
		                     (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
		                     (implicit leftResult :SQLTransformation[left.Subject, Z],
		                               rightResult :SQLTransformation[right.Subject, Z], spelling :SQLSpelling)
				:(leftResult.SQLResult[L, Single, LValueSQL[L, A, Z]], rightResult.SQLResult[R, Single, LValueSQL[R, B, Z]]) =
			decorated(left, right)
*/

/*

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			decorated(left, right)

		override def apply[F <: RowProduct, V](left :SingleQuerySQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			decorated(left, right)

		override def apply[F <: RowProduct, V](left :SelectSQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			decorated(left, right)

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SingleQuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			decorated(left, right)

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SelectSQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			decorated(left, right)


		override def apply[X, Y, V](left :Query[X, V], right :Query[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			decorated(left, right)

		override def apply[X, Y, V](left :SingleQuery[X, V], right :Query[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			decorated(left, right)

		override def apply[X, Y, V](left :Select[X, V], right :Query[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			decorated(left, right)

		override def apply[X, Y, V](left :Query[X, V], right :SingleQuery[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			decorated(left, right)

		override def apply[X, Y, V](left :Query[X, V], right :Select[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			decorated(left, right)

		override def apply[F <: RowProduct, V](query :QuerySQL[F, V])(implicit spelling :SQLSpelling) :QuerySQL[F, V] =
			decorated(query)

		override def apply[P, V](query :Query[P, V])(implicit spelling :SQLSpelling) :Query[P, V] =
			decorated(query)

		override def apply[F <: RowProduct, V]
		                  (query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
			decorated(query)

		override def apply[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
			decorated(query)
*/

//		override def fallback[L <: RowProduct, A >: Grouped <: Single, X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
//		                     (left :SQLExpression[L, A, X], right :SQLExpression[R, B, Y])
//		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//				:(SQLExpression[L, A, Z], SQLExpression[R, B, Z]) =
//			decorated.fallback(left, right)
//
//		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
//		                     (left :LValueSQL[L, A, X], right :SQLExpression[R, B, Y])
//		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//				:(LV[L, A, Z], SQLExpression[R, B, Z]) =
//			decorated.fallback(left, right)
//
//		override def fallback[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
//		                     (left :SQLExpression[L, A, X], right :LValueSQL[R, B, Y])
//		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//				:(SQLExpression[L, A, Z], LV[R, B, Z]) =
//			decorated.fallback(left, right)
//
//		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
//		                     (left :LValueSQL[L, A, X], right :LValueSQL[R, B, Y])
//		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//				:(LV[L, A, Z], LV[R, B, Z]) =
//			decorated.fallback(left, right)
//
//		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
//		                     (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
//		                     (implicit compat :SQLTypeUnification[left.Subject, right.Subject, Z], spelling :SQLSpelling)
//				:(LV[L, A, Z], LV[R, B, Z]) =
//			decorated.fallback(left, right)
//
/*
		override def fallback[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
		                                         (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			decorated.fallback(left, right)

		override def fallback[X, Y, V](left :Query[X, V], right :Query[Y, V])
		                              (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			decorated.fallback(left, right)


		override def default[F <: RowProduct, V](query :SelectSQL[F, V])(implicit spelling :SQLSpelling) :SelectSQL[F, V] =
			decorated.default(query)

		override def default[P, V](query :Select[P, V])(implicit spelling :SQLSpelling) :Select[P, V] =
			decorated.default(query)

		override def default[V](query :TopSelectSQL[V])(implicit spelling :SQLSpelling) :TopSelectSQL[V] =
			decorated.default(query)

		override def default[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
		                                        (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
			decorated.default(query)

		override def default[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
			decorated.default(query)

		override def reformed[F <: RowProduct, V](left :QuerySQL[F, V], operator :SelectOperator, right :QuerySQL[F, V])
		                                         (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
			decorated.reformed(left, operator, right)

		override def reformed[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V])
		                           (implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
			decorated.reformed(left, operator, right)
*/


//		override def lift[F <: RowProduct, M[O] <: MappingAt[O], X, Y]
//		                 (lvalue :LValueSQL[F, M, X])(implicit lift :Lift[X, Y]) :LValue[F, M, Y] =
//			decorated.lift(lvalue)
//
//		override def lift[F <: RowProduct, M[O] <: MappingAt[O], X, Y]
//		                 (expression :SQLExpression[F, Single, X])(implicit lift :Lift[X, Y])
//				:Opt[LV[F, M, Y]] =
//			decorated.lift(expression)
//
//
//		override def subreform[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :Reform =
//			decorated.subreform(query)
//
//		override def subreform[F <: RowProduct, V]
//		                      (query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) :Reform =
//			decorated.subreform(query)
//
//		override def left(implicit spelling :SQLSpelling)  :Reform = decorated.left
//		override def right(implicit spelling :SQLSpelling) :Reform = decorated.right

//
//		override def mayExcludeLeft  :Boolean = decorated.mayExcludeLeft
//		override def mayIncludeLeft  :Boolean = decorated.mayIncludeLeft
//		override def mayReorderLeft  :Boolean = decorated.mayReorderLeft
//		override def mayAddNullLeft  :Boolean = decorated.mayAddNullLeft
//		override def mayReformLeft   :Boolean = decorated.mayReformLeft
//		override def mayExcludeRight :Boolean = decorated.mayExcludeRight
//		override def mayIncludeRight :Boolean = decorated.mayIncludeRight
//		override def mayReorderRight :Boolean = decorated.mayReorderRight
//		override def mayAddNullRight :Boolean = decorated.mayAddNullRight
//		override def mayReformRight  :Boolean = decorated.mayReformRight
//
//		override def allowExcludeLeft  :Reform = if (mayExcludeLeft) self else decorated.allowExcludeLeft
//		override def allowIncludeLeft  :Reform = if (mayIncludeLeft) self else decorated.allowIncludeLeft
//		override def allowReorderLeft  :Reform = if (mayReorderLeft) self else decorated.allowReorderLeft
//		override def allowAddNullLeft  :Reform = if (mayAddNullLeft) self else decorated.allowAddNullLeft
//		override def allowReformLeft   :Reform = if (mayReformLeft) self else decorated.allowReformLeft
//		override def allowExcludeRight :Reform = if (mayExcludeRight) self else decorated.allowExcludeRight
//		override def allowIncludeRight :Reform = if (mayIncludeRight) self else decorated.allowIncludeRight
//		override def allowReorderRight :Reform = if (mayReorderRight) self else decorated.allowReorderRight
//		override def allowAddNullRight :Reform = if (mayAddNullRight) self else decorated.allowAddNullRight
//		override def allowReformRight  :Reform = if (mayReformRight) self else decorated.allowReformRight
//
//		override def prohibitExcludeLeft  :Reform = if (mayExcludeLeft) decorated.prohibitExcludeLeft else self
//		override def prohibitIncludeLeft  :Reform = if (mayIncludeLeft) decorated.prohibitIncludeLeft else self
//		override def prohibitReorderLeft  :Reform = if (mayReorderLeft) decorated.prohibitReorderLeft else self
//		override def prohibitAddNullLeft  :Reform = if (mayAddNullLeft) decorated.prohibitAddNullLeft else self
//		override def prohibitReformLeft   :Reform =
//			if (mayExcludeLeft || mayIncludeLeft || mayReorderLeft || mayAddNullLeft)
//				decorated.prohibitReformLeft
//			else self
//		override def prohibitExcludeRight :Reform = if (mayExcludeRight) decorated.prohibitExcludeRight else self
//		override def prohibitIncludeRight :Reform = if (mayIncludeRight) decorated.prohibitIncludeRight else self
//		override def prohibitReorderRight :Reform = if (mayReorderRight) decorated.prohibitReorderRight else self
//		override def prohibitAddNullRight :Reform = if (mayAddNullRight) decorated.prohibitAddNullRight else self
//		override def prohibitReformRight  :Reform =
//			if (mayExcludeRight || mayIncludeRight || mayReorderRight || mayAddNullRight)
//				decorated.prohibitReformRight
//			else self
//		override def prohibitAll          :Reform =
//			if (mayExcludeLeft || mayIncludeLeft || mayReorderLeft || mayAddNullLeft
//				|| mayExcludeRight || mayIncludeRight || mayReorderRight || mayAddNullRight)
//				decorated.prohibitAll
//			else self
//
//		override def permit(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean,
//		                    mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
//		                    mayExcludeRight :Boolean, mayIncludeRight :Boolean,
//		                    mayReorderRight :Boolean, mayAddNullRight :Boolean) :Reform =
//			if (mayExcludeLeft == this.mayExcludeLeft && mayIncludeLeft == this.mayIncludeLeft &&
//				mayReorderLeft == this.mayReorderLeft && mayAddNullLeft == this.mayAddNullLeft &&
//				mayExcludeRight == this.mayExcludeRight && mayIncludeRight == this.mayIncludeRight &&
//				mayReorderRight == this.mayReorderRight && mayAddNullRight == this.mayAddNullRight
//			)
//				self
//			else
//				decorated.permit(mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
//				                 mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight)
//
//		override def isSymmetrical :Boolean = decorated.isSymmetrical

		override def swap :Reform = decorated.swap
//
//		/** Equals [[net.noresttherein.oldsql.morsels.Decorable.Decorator.decorated decorated]]`.`[[net.noresttherein.oldsql.morsels.Decorable.self self]]. */
//		override val self :Reform = decorated.self
//
//		/** Delegates to [[net.noresttherein.oldsql.morsels.Decorable.Decorator.decorated decorated]]`.`[[net.noresttherein.oldsql.morsels.Decorable.decorate decorate]].  */
//		override def decorate(decorator :Reform => Reform) :Reform =
//			decorated.decorate(decorator)

	}



	/** Decorates the given argument's `self` in a proxy which swaps the order of all expression arguments, unless
	  * `reform` was created by this very method, in which case the original wrapped reform is simply returned.
	  *
	  * Swapped methods include permission methods and all reforming methods, which swap both the arguments
	  * and the reformed expressions in the returned pair. It is special because
	  * it is not present on the decorator stack (included in the constructor function
	  * for [[net.noresttherein.oldsql.sql.mechanics.Reform.self self]]): if the swapped `Reform`
	  * would pass `self` reference somewhere, it would use original sides, obliterating the sense of delegating
	  * all methods to `swap` before swapping the result - we want everything done as the decorated reform would,
	  * but with swapped sides. This changes if a `SwappedReform` is decorated:
	  * in that case we add swapping of the sides before the new decorator ''and after it'', so that `swap.self`
	  * retains correct parameter order. I don't know if it's the best or even a good approach, but it is the only
	  * sensible one I can think of.
	  */
	def SwappedReform(reform :Reform) :Reform =
		reform.self match {
			case swap :SwappedReform => swap.swap
			case self => self.decorate(new SwappedReform(_)) //new SwappedReform(self)
		}



	/** `SwappedReform` is a reform proxy which switches left nad right sides when delegating to the underlying instance.
	  * This includes permission methods and all reforming methods, which swap both the arguments
	  * and the reformed expressions in the returned pair. It is special because
	  * it is neither a [[net.noresttherein.oldsql.sql.mechanics.Reform.BaseReform BaseReform]]
	  * nor a [[net.noresttherein.oldsql.morsels.Decorable.Decorator Decorator]]
	  * and is not present on the decorator stack (included in the constructor function
	  * for [[net.noresttherein.oldsql.sql.mechanics.Reform.self self]]): if the swapped `Reform`
	  * would pass `self` reference somewhere, it would use original sides, obliterating the sense of delegating
	  * all methods to `swap` before swapping the result - we want everything done as the decorated reform would,
	  * but with swapped sides. This changes if a `SwappedReform` is decorated:
	  * in that case we add swapping of the sides before the new decorator ''and after it'', so that `swap.self`
	  * retains correct parameter order. I don't know if it's the best or even a good approach, but it is the only
	  * sensible one I can think of.
	  */
	private class SwappedReform(override val swap :Reform) extends Reform
	{
		override val self = if (swap.self eq swap) this else swap.self.swap
		private def doubleSwap(decorator :Reform => Reform) =
			{ r :Reform => r.swap } andThen decorator andThen (_.swap)

		override def decorate(decorator :Reform => Reform) =
			/* It is important that swap.self takes arguments in the same order as swap:
			 * this is why we add an extra _.swap to the decorator function.
			 * The result of decorate is thus a SwappedReform (unless decorator is identity)
			 * and needs to be 'peeled of' by a call to .swap */
			swap.decorate(doubleSwap(decorator)).swap

		override def redecorate(decorator :Reform => Reform) = swap.redecorate(doubleSwap(decorator)).swap

		override def undecorate(decorator :Reform => Reform) = swap.undecorate(doubleSwap(decorator)).swap


		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                            spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			swap(right, left).swap

		override def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                      RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                     (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                     (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                               spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			swap.fallback(right, left).swap

		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
		                    (implicit leftResult  :SQLTransformation[left.Subject, U],
		                              rightResult :SQLTransformation[right.Subject, U], spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
			swap.default(right, left).swap


/*
		override def apply[F <: RowProduct, V]
		                  (left :QuerySQL[F, V], right :QuerySQL[F, V])(implicit spelling :SQLSpelling) =

		override def apply[F <: RowProduct, V]
		                  (left :SelectSQL[F, V], right :QuerySQL[F, V])(implicit spelling :SQLSpelling) =
			swap(right, left).swap

		override def apply[F <: RowProduct, V](left :SingleQuerySQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) =
			swap(right, left).swap

		override def apply[F <: RowProduct, V]
		                  (left :QuerySQL[F, V], right :SelectSQL[F, V])(implicit spelling :SQLSpelling) =
			swap(right, left).swap

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SingleQuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) =
			swap(right, left).swap


		override def apply[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) =
			swap(right, left).swap

		override def apply[X, Y, V](left :SingleQuery[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) =
			swap(right, left).swap

		override def apply[X, Y, V](left :Select[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) =
			swap(right, left).swap

		override def apply[X, Y, V](left :Query[X, V], right :SingleQuery[Y, V])(implicit spelling :SQLSpelling) =
			swap(right, left).swap

		override def apply[X, Y, V](left :Query[X, V], right :Select[Y, V])(implicit spelling :SQLSpelling) =
			swap(right, left).swap


		override def apply[P, V](query :Query[P, V])(implicit spelling :SQLSpelling) :Query[P, V] = swap(query)
		override def apply[F <: RowProduct, V](query :QuerySQL[F, V])(implicit spelling :SQLSpelling) :QuerySQL[F, V] =
			swap(query)

		override def apply[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) = swap(query)
		override def apply[F <: RowProduct, V](query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) =
			swap(query)
*/


//		override def fallback[L <: RowProduct, A >: Grouped <: Single, X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
//		                     (left :SQLExpression[L, A, X], right :SQLExpression[R, B, Y])
//		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
//			swap.fallback(right, left)(compat.swap, spelling).swap
//
//		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
//		                     (left :LValueSQL[L, A, X], right :SQLExpression[R, B, Y])
//		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
//			swap.fallback(right, left)(compat.swap, spelling).swap
//
//		override def fallback[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
//		                     (left :SQLExpression[L, A, X], right :LValueSQL[R, B, Y])
//		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
//			swap.fallback(right, left)(compat.swap, spelling).swap
//
//		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
//		                     (left :LValueSQL[L, A, X], right :LValueSQL[R, B, Y])
//		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
//		swap.fallback(right, left)(compat.swap, spelling).swap
//
//		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
//		                     (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
//		                     (implicit compat :SQLTypeUnification[left.Subject, right.Subject, Z], spelling :SQLSpelling) =
//			swap.fallback(right, left)(compat.swap, spelling).swap
//
/*
		override def fallback[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
		                                         (implicit spelling :SQLSpelling) =
			swap.fallback(right, left).swap

		override def fallback[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) =
			swap.fallback(right, left).swap


//		override def lift[F <: RowProduct, M[O] <: MappingAt[O], X, Y]
//		(lvalue :LValueSQL[F, M, X])(implicit lift :Lift[X, Y]) :LValue[F, M, Y] =
//			swap.lift(lvalue)
//
//		override def lift[F <: RowProduct, M[O] <: MappingAt[O], X, Y]
//		                 (expression :SQLExpression[F, Single, X])(implicit lift :Lift[X, Y])
//				:Opt[LV[F, M, Y]] =
//			swap.lift(expression)

		override def default[P, V](query :Select[P, V])(implicit spelling :SQLSpelling) = swap.default(query)
		override def default[V](query :TopSelectSQL[V])(implicit spelling :SQLSpelling) = swap.default(query)
		override def default[F <: RowProduct, V](query :SelectSQL[F, V])(implicit spelling :SQLSpelling) =
			swap.default(query)


		override def subreform[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) =
			swap.subreform(query)

		override def subreform[F <: RowProduct, V](query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) =
			swap.subreform(query)

		override def left(implicit spelling :SQLSpelling)  = swap.right
		override def right(implicit spelling :SQLSpelling) = swap.left
*/

		override def permissions      :Permissions = swap.permissions.swap
		override def leftPermissions  :Permissions = swap.rightPermissions
		override def rightPermissions :Permissions = swap.leftPermissions

		override def mayExcludeLeft  = swap.mayExcludeRight
		override def mayIncludeLeft  = swap.mayIncludeRight
		override def mayReorderLeft  = swap.mayReorderRight
		override def mayAddNullLeft  = swap.mayAddNullRight
		override def mayAlterLeft    = swap.mayAlterRight
		override def mayReformLeft   = swap.mayReformRight
		override def mayExcludeRight = swap.mayExcludeLeft
		override def mayIncludeRight = swap.mayIncludeLeft
		override def mayReorderRight = swap.mayReorderLeft
		override def mayAddNullRight = swap.mayAddNullLeft
		override def mayAlterRight   = swap.mayAlterLeft
		override def mayReformRight  = swap.mayReformLeft

		override def allowExcludeLeft  = swap.allowExcludeRight.swap
		override def allowIncludeLeft  = swap.allowIncludeRight.swap
		override def allowReorderLeft  = swap.allowReorderRight.swap
		override def allowAddNullLeft  = swap.allowAddNullRight.swap
		override def allowAlterLeft    = swap.allowAlterRight.swap
		override def allowExcludeRight = swap.allowExcludeLeft.swap
		override def allowIncludeRight = swap.allowIncludeLeft.swap
		override def allowReorderRight = swap.allowReorderLeft.swap
		override def allowAddNullRight = swap.allowAddNullLeft.swap
		override def allowAlterRight   = swap.allowAlterLeft.swap

		override def prohibitExcludeLeft  = swap.prohibitExcludeRight.swap
		override def prohibitIncludeLeft  = swap.prohibitIncludeRight.swap
		override def prohibitReorderLeft  = swap.prohibitReorderRight.swap
		override def prohibitAddNullLeft  = swap.prohibitAddNullRight.swap
		override def prohibitReformLeft   = swap.prohibitReformLeft.swap
		override def prohibitExcludeRight = swap.prohibitExcludeLeft.swap
		override def prohibitIncludeRight = swap.prohibitIncludeLeft.swap
		override def prohibitReorderRight = swap.prohibitReorderLeft.swap
		override def prohibitAddNullRight = swap.prohibitAddNullLeft.swap
		override def prohibitReformRight  = swap.prohibitReformLeft.swap
		override def prohibitAll          = swap.prohibitAll.swap

		override def prohibit(permissions :Permissions) :Reform = swap.prohibit(permissions.swap).swap

		override def isSymmetrical = swap.isSymmetrical

		override def undecoratedEquals(that :Reform) :Boolean = that match {
			case _ if that eq this => true
			case _ if swap.canEqual(that) && that.canEqual(swap) =>
				swap.permissions == that.permissions && (swap swapEquals that)
			case _ => false
		}
		override def swapEquals(that :Reform) :Boolean = swap undecoratedEquals that
		override def undecoratedCanEqual(that :Reform) :Boolean = that.isInstanceOf[Reform]
		override def undecoratedHashCode :Int = swap.hashCode

		override def toString :String = swap.toString + ".swap"
	}



	//todo: either remove it or create also a QueryReformSidesControl
	/** A `Reform` decorator which modifies the arguments `mayReformLeft` and `mayReformRight` before passing them
	  * to the swap `reform`. The new values equal `mayReformLeft & prohibitLeft | allowLeft`
	  * and `mayReformRight & prohibitRight | allowRight`, respectively.
	  * Method [[net.noresttherein.oldsql.sql.mechanics.Reform.swap swap]] is overriden to switch
	  * the `xxxLeft` and `xxxRight` arguments.
	  */ //todo: revise how allowX works and decide if it shouldn't cancel prohibitX
	def ReformSidesControl(prohibitLeft :Boolean, allowLeft :Boolean, prohibitRight :Boolean, allowRight :Boolean)
	                      (reform :Reform) :Reform =
		reform match {
			case control :ReformSidesControl =>
				reform.decorate(
					new ReformSidesControl(_,
						prohibitLeft || control.prohibitLeft && !allowLeft,
						allowLeft || control.allowLeft && !prohibitLeft,
						prohibitRight || control.prohibitRight && !allowRight,
						allowRight || control.allowRight && !prohibitRight
					)
				)
			case _ =>
				reform.decorate(
					new ReformSidesControl(_, prohibitLeft, allowLeft, prohibitRight, allowRight)
				)
		}


	/** A `Reform` which always passes `mayReformRight = false` as an argument
	  * (or `mayReformLeft = false) after swapping)`.
	  */
	def NoRightReform(reform :Reform) :Reform =
		ReformSidesControl(false, false, true, false)(reform)

	/** A `Reform` which always passes `mayReformRight = false` as an argument
	  * (or `mayReformLeft = false) after swapping)`.
	  */
	def NoLeftReform(reform :Reform) :Reform =
		ReformSidesControl(true, false, false, true)(reform)


	/** A `Reform` decorator which modifies the arguments `mayReformLeft` and `mayReformRight` before passing them
	  * to the swap `reform`. The new values equal `mayReformLeft & prohibitLeft | allowLeft`
	  * and `mayReformRight & prohibitRight | allowRight`, respectively.
	  * Method [[net.noresttherein.oldsql.sql.mechanics.Reform.swap swap]] is overriden to switch
	  * the `xxxLeft` and `xxxRight` arguments.
	  */
	private class ReformSidesControl(override val decorated :Reform,
			val prohibitLeft :Boolean, val allowLeft :Boolean, val prohibitRight :Boolean, val allowRight :Boolean
        ) extends ReformDecorator(ReformSidesControl(prohibitLeft, allowLeft, prohibitRight, allowRight))
	{
		override def mayExcludeLeft  = allowLeft || !prohibitLeft && decorated.mayExcludeLeft
		override def mayIncludeLeft  = allowLeft || !prohibitLeft && decorated.mayIncludeLeft
		override def mayReorderLeft  = allowLeft || !prohibitLeft && decorated.mayReorderLeft
		override def mayAddNullLeft  = allowLeft || !prohibitLeft && decorated.mayAddNullLeft
		override def mayAlterLeft    = allowLeft || !prohibitLeft && decorated.mayAlterLeft
		override def mayReformLeft   = allowLeft || !prohibitLeft && decorated.mayReformLeft

		override def mayExcludeRight = allowRight || !prohibitRight && decorated.mayExcludeRight
		override def mayIncludeRight = allowRight || !prohibitRight && decorated.mayIncludeRight
		override def mayReorderRight = allowRight || !prohibitRight && decorated.mayReorderRight
		override def mayAddNullRight = allowRight || !prohibitRight && decorated.mayAddNullRight
		override def mayAlterRight   = allowRight || !prohibitRight && decorated.mayAlterRight
		override def mayReformRight  = allowRight || !prohibitRight && decorated.mayReformRight

		//permission modifications inherited from ReformDecorator work thanks to overriden mayXxx methods
/*
		override def allowExcludeLeft  = if (allowLeft) self else super.allowExcludeLeft
		override def allowIncludeLeft  = if (allowLeft) self else super.allowIncludeLeft
		override def allowReorderLeft  = if (allowLeft) self else super.allowReorderLeft
		override def allowAddNullLeft  = if (allowLeft) self else super.allowAddNullLeft
		override def allowReformLeft   = if (allowLeft) self else super.allowReformLeft
		override def allowExcludeRight = if (allowRight) self else super.allowExcludeRight
		override def allowIncludeRight = if (allowRight) self else super.allowIncludeRight
		override def allowReorderRight = if (allowRight) self else super.allowReorderRight
		override def allowAddNullRight = if (allowRight) self else super.allowAddNullRight
		override def allowReformRight  = if (allowRight) self else super.allowReformRight

		override def prohibitExcludeLeft  = if (prohibitLeft) self else super.prohibitExcludeLeft
		override def prohibitIncludeLeft  = if (prohibitLeft) self else super.prohibitIncludeLeft
		override def prohibitReorderLeft  = if (prohibitLeft) self else super.prohibitReorderLeft
		override def prohibitAddNullLeft  = if (prohibitLeft) self else super.prohibitAddNullLeft
		override def prohibitReformLeft   = if (prohibitLeft) self else super.prohibitReformLeft
		override def prohibitExcludeRight = if (prohibitRight) self else super.prohibitExcludeRight
		override def prohibitIncludeRight = if (prohibitRight) self else super.prohibitIncludeRight
		override def prohibitReorderRight = if (prohibitRight) self else super.prohibitReorderRight
		override def prohibitAddNullRight = if (prohibitRight) self else super.prohibitAddNullRight
		override def prohibitReformRight  = if (prohibitRight) self else super.prohibitReformRight
*/

		override def isSymmetrical :Boolean =
			//we must include decorated.isSymmetrical because it might introduce some new state aside from permissions
			decorated.isSymmetrical && prohibitLeft == prohibitRight && allowLeft == allowRight


		override def undecoratedEquals(that :Reform) :Boolean = that match {
			case _ if that eq this => true
			case other :ReformSidesControl => //no need to check permissions, it is done in equals
				decorated undecoratedEquals other.decorated
			case _ => decorated undecoratedEquals that
		}
		override def undecoratedCanEqual(that :Reform) :Boolean = decorated undecoratedCanEqual that
		override def swapEquals(that :Reform) :Boolean = decorated swapEquals that

		override def toString :String =
			decorated.toString +
				(if (allowLeft) ".allowReformLeft" else if (prohibitLeft) ".prohibitReformLeft" else "") +
				(if (allowRight) ".allowReformRight" else if (prohibitRight) ".prohibitReformRight" else "")
	}



	/** A `Reform` implementation which does not alter the column sets when matching
	  * two [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions.
	  * It will still attempt to alter [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]] instances
	  * and potentially reorder other expressions, given suitable permissions,
	  * so it goes beyond a simple validation of column alignment.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.RowShapeValidator]]
	  */
	val ExactReform :Reform = new ExactReform(true, true, identity)

	/** A `Reform` implementation which does not alter the column sets when matching
	  * two [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions.
	  * It will still attempt to alter [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]] instances
	  * and potentially reorder other expressions, given suitable permissions,
	  * so it goes beyond a simple validation of column alignment.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.RowShapeValidator]]
	  */
	def ExactReform(mayAlterLeft :Boolean, mayAlterRight :Boolean) :Reform =
		new ExactReform(mayAlterLeft, mayAlterRight, identity)

	/** A `Reform` implementation which does not alter the column sets when matching
	  * two [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions.
	  * It will still attempt to alter [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]] instances,
	  * and may reorder columns (given a suitable permission) within expressions,
	  * so it goes beyond a simple validation of column alignment.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.RowShapeValidator]]
	  */
	private[mechanics] class ExactReform(override val mayAlterLeft :Boolean, override val mayAlterRight :Boolean,
	                                     wrap :Reform => Reform,
	                                     constructor :(Boolean, Boolean, Reform => Reform) => Reform)
		extends SimpleReform(mayAlterLeft, mayAlterRight)(wrap, constructor(_, _, _))
	{
		def this(mayReformLeft :Boolean, mayReformRight :Boolean, wrap :Reform => Reform) =
			this(mayReformLeft, mayReformRight, wrap, new ExactReform(_, _, _))

		private[this] val noReform = prohibit(permissions)
		private[this] val noReformLeft = prohibitReformLeft
		private[this] val noReformRight = prohibitReformRight

		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                            spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			(left, right) match {
				case (_ :LValueSQL.__ @unchecked | _ :LabeledSQL.__, _ :LValueSQL.__ @unchecked | _ :LabeledSQL.__) =>
					left.`->reform`(right)(noReform, PassCount)
				case (_ :LValueSQL.__ @unchecked | _ :LabeledSQL.__, _) =>
					left.`->reform`(right)(noReformLeft, PassCount)
				case (_, _ :LValueSQL.__ @unchecked | _ :LabeledSQL.__) =>
					left.`->reform`(right)(noReformRight, PassCount)
				case _ =>
					super.apply(left, right)
			}

		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
		                    (implicit leftResult  :SQLTransformation[left.Subject, U],
		                              rightResult :SQLTransformation[right.Subject, U], spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
			fallback(right, left)

		private def shape(e :SQLExpression.__)(implicit spelling :SQLSpelling) =
			try { spelling.shape(e) } catch {
				case ex :Exception =>
					throw new UndefinedShapeException(
						"Could not determine the shape of `" + e + "`. " + ex.getMessage, ex
					)
			}

		//just a more informative error message
		protected override def validate[LF <: RowProduct, LS >: Grouped <: Single, LV,
		                                RF <: RowProduct, RS >: Grouped <: Single, RV, U]
		                               (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	                                   (implicit leftResult  :SQLTransformation[LV, U],
	                                             rightResult :SQLTransformation[RV, U], spelling :SQLSpelling) :Unit =
			if (!compatible(left, right))
				throw new MismatchedExpressionsException(left, right,
					"different shapes: " + shape(left) + " vs. " + shape(right) + "."
				)

/*

		override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
		                  (left :LValueSQL[L, A, X], right :SQLExpression[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(LValue[L, A, Z], SQLExpression[R, B, Z]) =
			left.`->reform`(right)(prohibitReformLeft, Reform.MaxPasses)(compat.left, compat.right, spelling)

		override def apply[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
		                  (left :SQLExpression[L, A, X], right :LValueSQL[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(SQLExpression[L, A, Z], LValue[R, B, Z]) =
			left.`->reform`(right)(prohibitReformRight, Reform.MaxPasses)(compat.left, compat.right, spelling)

		override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
		                  (left :LValueSQL[L, A, X], right :LValueSQL[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(LValue[L, A, Z], LValue[R, B, Z]) =
		{
			val noReform = permit(false, false, false, false, false, false, false, false)
			left.`->reform`(right)(noReform, Reform.MaxPasses)(compat.left, compat.right, spelling)
		}
*/
/*
		protected override def validate[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) :Unit =
			if (!compatible(left, right))
				throw new MismatchedExpressionsException(left, right,
					"different shapes: " + shape(left) + " vs. " + shape(right) + "."
				)
*/

		override lazy val swap =
			if (self.isSymmetrical)
				self
			else if (self eq this)
				if (getClass == classOf[ExactReform])
					new ExactReform(mayAlterRight, mayAlterLeft, wrap) {
						override lazy val swap = ExactReform.this
					}
				else
					constructor(mayAlterRight, mayAlterLeft, wrap).self
			else
				SwappedReform(this)

//		override def subreform[F <: RowProduct, V]
//		                      (query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) :Reform =
//			query.operator.prevent(this :Reform)
//
//		override def subreform[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :Reform =
//			query.operator.prevent(this :Reform)
//
//		override def left(implicit spelling :SQLSpelling)  :Reform = ExactReform(true, true)
//		override def right(implicit spelling :SQLSpelling) :Reform = ExactReform(true, true)
//
//		protected override def validate[LF <: RowProduct, LS >: Grouped <: Single, LV,
//		                                RF <: RowProduct, RS >: Grouped <: Single, RV, U]
//		                               (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
//		                               (implicit leftResult[LV, RV, U], spelling :SQLSpelling) :Unit =
//			if (!compatible(left, right))
//				throw new MismatchedExpressionsException(left, right,
//					"different shapes: " + spelling.shape(left) + " vs. " + spelling.shape(right) + "."
//				)

		protected override def typeName = "ExactReform"
	}



	/** A `Reform` implementation which does not alter the column sets when matching
	  * two [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions.
	  * It will still attempt to alter [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]] instances
	  * and potentially reorder other expressions, given suitable permissions,
	  * so it goes beyond a simple validation of column alignment.
	  *
	  * The difference from [[net.noresttherein.oldsql.sql.mechanics.Reform.ExactReform ExactReform]] is that
	  * it enforces this instance's `mayReformLeft`/`mayReformRight` permissions for the full process of reforming
	  * left/right subqueries of a ''compound select'', while the former will first try to internally unify
	  * the left/right subquery with no constraints before unifying it with the other subquery.
	  */
//	val TopDownExactReform :Reform = new TopDownExactReform(true, true, identity)

	/** A `Reform` implementation which does not alter the column sets when matching
	  * two [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions.
	  * It will still attempt to alter [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]] instances
	  * and potentially reorder other expressions, given suitable permissions,
	  * so it goes beyond a simple validation of column alignment.
	  *
	  * The difference from [[net.noresttherein.oldsql.sql.mechanics.Reform.ExactReform ExactReform]] is that
	  * it enforces this instance's `mayReformLeft`/`mayReformRight` permissions for the full process of reforming
	  * left/right subqueries of a ''compound select'', while the former will first try to internally unify
	  * the left/right subquery with no constraints before unifying it with the other subquery.
	  */
//	def TopDownExactReform(mayReformLeft :Boolean, mayReformRight :Boolean) :Reform =
//		new TopDownExactReform(mayReformLeft, mayReformRight, identity)

	/** A `Reform` implementation which does not alter the column sets when matching
	  * two [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions.
	  * It will still attempt to alter [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]] instances,
	  * and may reorder columns (given a suitable permission) within expressions,
	  * so it goes beyond a simple validation of column alignment.
	  *
	  * The difference from [[net.noresttherein.oldsql.sql.mechanics.Reform.ExactReform ExactReform]] is that
	  * it enforces this instance's `mayReformLeft`/`mayReformRight` permissions for the full process of reforming
	  * left/right subqueries of a ''compound select'', while the former will first try to internally unify
	  * the left/right subquery with no constraints before unifying it with the other subquery.
	  */
/*
	private[mechanics] class TopDownExactReform(override val mayReformLeft :Boolean, override val mayReformRight :Boolean,
	                                            wrap :Reform => Reform)
		extends ExactReform(mayReformLeft, mayReformRight, wrap, new TopDownExactReform(_, _, _))
	{

		override lazy val swap =
			if (self.isSymmetrical)
				self
			else if ((self eq this) && getClass == classOf[TopDownExactReform])
				new TopDownExactReform(mayReformRight, mayReformLeft, wrap) {
					override lazy val swap = TopDownExactReform.this
				}
			else
				SwappedReform(this)
//
//		override def left(implicit spelling :SQLSpelling) :Reform =
//			if (mayReformRight == mayReformLeft) self
//			else TopDownExactReform(mayReformLeft, mayReformLeft)
//
//		override def right(implicit spelling :SQLSpelling) :Reform =
//			if (mayReformLeft == mayReformRight) self
//			else TopDownExactReform(mayReformRight, mayReformRight)

		override def toString =
			"TopDownExactReform(" + (if (mayReformLeft) "reform, " else "no reform, ") +
				(if (mayReformRight) "reform)" else "no reform)")
	}
*/



//	private[sql]
//	trait AbstractValidatorReform[LV[-F <: RowProduct, M[O] <: MappingAt[O], V] <: SQLExpression[F, Single, V]]
//		extends Reform
//	{
	private[mechanics] trait AbstractValidatorReform extends ForbiddenReform[Reform] with BaseReform {
//		override type LValue[-F <: RowProduct, M[O] <: MappingAt[O], V] = LV[F, M, V]
//
		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
		                    (implicit leftResult  :SQLTransformation[left.Subject, U],
		                              rightResult :SQLTransformation[right.Subject, U], spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
			fallback(left, right)

//		override def subreform[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :Reform = self
//		override def subreform[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
//		                                          (implicit spelling :SQLSpelling) :Reform = self
//
//		override def left(implicit spelling :SQLSpelling)  :Reform = self //:ReformTo[LV] = self
//		override def right(implicit spelling :SQLSpelling) :Reform = self //:ReformTo[LV] = self
/*
		override def mayExcludeLeft  :Boolean = false
		override def mayIncludeLeft  :Boolean = false
		override def mayReorderLeft  :Boolean = false
		override def mayAddNullLeft  :Boolean = false
		override def mayExcludeRight :Boolean = false
		override def mayIncludeRight :Boolean = false
		override def mayReorderRight :Boolean = false
		override def mayAddNullRight :Boolean = false

		override def permit(mayExcludeLeft  :Boolean, mayIncludeLeft  :Boolean,
		                    mayReorderLeft  :Boolean, mayAddNullLeft  :Boolean,
		                    mayExcludeRight :Boolean, mayIncludeRight :Boolean,
		                    mayReorderRight :Boolean, mayAddNullRight :Boolean) :Reform =
			self*/
		override def toString :String = typeName
	}



	/** A `Reform` implementation which structurally compares two expressions in the default manner,
	  * but instead of reforming any expression, it simply
	  * verifies if the column counts and types of both expressions are equal.
	  * It is different from [[net.noresttherein.oldsql.sql.mechanics.Reform.RowShapeValidator RowShapeValidator]]
	  * in that the latter is satisfied if the [[net.noresttherein.oldsql.sql.RowShape RowShape]] of the whole
	  * expressions are compatible and is not inherently recursive, while this validator also requires
	  * that at each recursion step the expressions compared are of the same type/structure/configuration, i.e.
	  * they are equal but for the bottom expressions, which must only be of compatible shapes.
	  */
	val SQLShapeValidator :Reform = new ValidatorReform

	/** Base class for validator reforms: [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] implementations
	  * which do not change the argument expressions, but only validate their compatibility.
	  * They work recursively as normal, performing compatibility check only when the expression does not attempt
	  * to deconstruct itself and continue the process,
	  * but calls [[net.noresttherein.oldsql.sql.mechanics.Reform.fallback fallback]] on this reform.
	  * The exact criteria depend on the subclass implementation; this class compares
	  * [[net.noresttherein.oldsql.sql.SQLExpression.shape shapes]] of the expressions
	  * with [[net.noresttherein.oldsql.sql.RowShape.<:> <:>]].
	  */
	class ValidatorReform(wrap :Reform => Reform, constructor :(Reform => Reform) => Reform)
		extends BaseDecorable[Reform](wrap, constructor) with AbstractValidatorReform
	{
		def this(wrap :Reform => Reform) = this(wrap, new ValidatorReform(_))
		def this() = this(identity[Reform])

		private def shape(e :SQLExpression.__)(implicit spelling :SQLSpelling) =
			try {
				spelling.shape(e)
			} catch {
				case ex :Exception =>
					throw new UndefinedShapeException(
						"Could not determine the shape of `" + e + "`. " + ex.getMessage, ex
					)
			}

		protected override def validate[LF <: RowProduct, LS >: Grouped <: Single, LV,
		                                RF <: RowProduct, RS >: Grouped <: Single, RV, U]
		                               (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	                                   (implicit leftResult  :SQLTransformation[LV, U],
	                                             rightResult :SQLTransformation[RV, U], spelling :SQLSpelling) :Unit =
			if (!compatible(left, right))
				throw new MismatchedExpressionsException(left, right,
					"different shapes: " + shape(left) + " vs. " + shape(right) + "."
				)

//		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
//		                     (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
//		                     (implicit leftResult :SQLTransformation[left.Subject, Z],
//		                               rightResult :SQLTransformation[right.Subject, Z], spelling :SQLSpelling)
//				:(leftResult.SQLResult[L, Single, LValueSQL[L, A, Z]],
//				  rightResult.SQLResult[R, Single, LValueSQL[R, B, Z]]) =
//		{
//			validate(left, right)
//			(left.to(compat.left), right.to(compat.right))
//		}
//
//		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
//		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
//		                    (implicit leftResult  :SQLTransformation[RM[Unit]#Subject, U],
//		                              rightResult :SQLTransformation[LM[Unit]#Subject, U], spelling :SQLSpelling)
//				:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
//			fallback(left, right)


/*
		override def apply[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
		                                      (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
			default(query)

		override def apply[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
			default(query)

		override def default[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
		                                        (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
		{
			val inOperand = spelling.inOperand
			left.apply(query.left)(inOperand)
			right.apply(query.right)(inOperand)
			validate(query.left.selectClause, query.right.selectClause)(
				SQLConversion.toSelf, SQLConversion.toSelf, spelling.inSelect
			)
			query
		}

		override def default[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] = {
			val inOperand = spelling.inOperand
			left.apply(query.left)(inOperand)
			right.apply(query.right)(inOperand)
			validate(query.left.selectClause, query.right.selectClause)(
				SQLConversion.toSelf, SQLConversion.toSelf, spelling.inSelect
			)
			query
		}
*/
		override def swap :Reform =
			if ((self eq this) && isSymmetrical) this else _swap

		private lazy val _swap = SwappedReform(this)
		protected override def typeName :String =
			if (getClass == classOf[ValidatorReform]) "SQLShapeValidator" else this.localClassName
	}



	/** A `Reform` implementation which does not alter the column sets when unifying two expressions, but instead
	  * verifies if the column counts and types of both expressions are equal.
	  * It relies primarily on method `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.shape shape]]
	  * and falls back to `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.split split]].
	  * Note that it is a 'flat' comparison: the reform does not descend down the syntax tree of the expressions
	  * in order to compare them structurally, but instead compares the [[net.noresttherein.oldsql.sql.RowShape shapes]]
	  * of the whole rows from the two arguments.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.SQLShapeValidator]]
	  */
	val RowShapeValidator :Reform = new FlatValidatorReform

	/** A base class for validator reforms which do not descend recursively down the expression using
	  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.reform reform]],
	  * but immediately perform validation on the two expressions as a whole.
	  */
	class FlatValidatorReform(wrap :Reform => Reform, constructor :(Reform => Reform) => Reform)
		extends ValidatorReform(wrap, constructor)
	{
		def this(wrap :Reform => Reform) = this(wrap, new FlatValidatorReform(_))
		def this() = this(identity[Reform])

		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                            spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			fallback(left, right)

/*
		override def apply[E <: RowProduct, A >: Grouped <: Single, X,
		                   L[v] <: SQLExpression[E, A, v] with ConvertingTemplate[E, A, v, L],
		                   F <: RowProduct, B >: Grouped <: Single, Y,
		                   R[v] <: SQLExpression[F, B, v] with ConvertingTemplate[F, B, v, R], Z]
		                  (left :L[X], right :R[Y])
		                  (implicit leftResult :SQLTransformation[X, Z], rightResult :SQLTransformation[Y, Z], spelling :SQLSpelling)
				:(leftResult.SQLResult[E, A, L[Z]], rightResult.SQLResult[F, B, R[Z]]) =
			fallback(left, right)

*/
//		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE <: SQLExpression[LF, LS, LV],
//		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE <: SQLExpression[RF, RS, RV],
//		                   U, LR <: SQLExpression[LF, LS, U], RR <: SQLExpression[RF, RS, U]]
//		                  (left :LE, right :RE)
//		                  (implicit leftResult  :SpecificTransformation[LF, LS, LV, U, LE, LR],
//		                            rightResult :SpecificTransformation[RF, RS, RV, U, RE, RR], spelling :SQLSpelling)
//				:(LR, RR) =
//			fallback(left, right)
/*
		override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
		                  (left :LValueSQL[L, A, X], right :SQLExpression[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(LV[L, A, Z], SQLExpression[R, B, Z]) =
			fallback(left, right)

		override def apply[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
		                  (left :SQLExpression[L, A, X], right :LValueSQL[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(SQLExpression[L, A, Z], LV[R, B, Z]) =
			fallback(left, right)

		override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
		                  (left :LValueSQL[L, A, X], right :LValueSQL[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(LV[L, A, Z], LV[R, B, Z]) =
			fallback(left, right)
*/


/*
		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			fallback(left, right)

		override def apply[F <: RowProduct, V](left :SingleQuerySQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			fallback(left, right)

		override def apply[F <: RowProduct, V](left :SelectSQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			fallback(left, right)

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SingleQuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			fallback(left, right)

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SelectSQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			fallback(left, right)

		override def apply[X, Y, V](left :Query[X, V], right :Query[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			fallback(left, right)

		override def apply[X, Y, V](left :SingleQuery[X, V], right :Query[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			fallback(left, right)

		override def apply[X, Y, V](left :Select[X, V], right :Query[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			fallback(left, right)

		override def apply[X, Y, V](left :Query[X, V], right :SingleQuery[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			fallback(left, right)

		override def apply[X, Y, V](left :Query[X, V], right :Select[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			fallback(left, right)
*/


////		override def apply[F <: RowProduct, V](query :QuerySQL[F, V])(implicit spelling :SQLSpelling) :QuerySQL[F, V] =
////			default(query)
//
//		override def apply[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
//		                                      (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
//			default(query)
//
////		override def apply[P, V](query :Query[P, V])(implicit spelling :SQLSpelling) :Query[P, V] =
////			default(query)
//
//		override def apply[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
//			default(query)
//
//		override def default[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
//		                                        (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
//		{
//			val inOperand = spelling.inOperand
//			left.apply(query.left)(inOperand)
//			right.apply(query.right)(inOperand)
//			validate(query.left.selectClause, query.right.selectClause)(
//				SQLConversion.toSelf, SQLConversion.toSelf, spelling.inSelect
//			)
//			query
//		}
//
//		override def default[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] = {
//			val inOperand = spelling.inOperand
//			left.apply(query.left)(inOperand)
//			right.apply(query.right)(inOperand)
//			validate(query.left.selectClause, query.right.selectClause)(
//				SQLConversion.toSelf, SQLConversion.toSelf, spelling.inSelect
//			)
//			query
//		}
		protected override def typeName :String =
			if (getClass == classOf[FlatValidatorReform]) "RowShapeValidator" else this.localClassName
	}



	/** A `Reform` implementation which does not alter argument expressions, but only verifies
	  * if their column counts are equal
	  * (as per `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]).
	  * The column types are ignored.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.Reform.RowShapeValidator]]
	  */
	val ArityValidator :Reform = new ArityValidator(identity)

	/** A `Reform` implementation which does not alter argument expressions, but only verifies
	  * if their column counts are equal
	  * (as per `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.columnCount columnCount]]).
	  */
	private[mechanics] class ArityValidator(wrap :Reform => Reform)
		extends FlatValidatorReform(wrap, new ArityValidator(_))
	{
		private def columnCount(e :SQLExpression.__)(implicit spelling :SQLSpelling) =
			try { spelling.columnCount(e) } catch {
				case ex :Exception =>
					throw new UndefinedShapeException(
						"Could not determine the number of columns in `" + ex + "`. " + ex.getMessage, ex
					)
			}
		protected override def validate[LF <: RowProduct, LS >: Grouped <: Single, LV,
                                RF <: RowProduct, RS >: Grouped <: Single, RV, U]
                               (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
                               (implicit leftResult  :SQLTransformation[LV, U],
                                         rightResult :SQLTransformation[RV, U], spelling :SQLSpelling) :Unit =
			if (!compatible(left, right))
				throw new MismatchedExpressionsException(left, right,
					"different numbers of columns: " + columnCount(left) + " vs. " + columnCount(right) + "."
				)

		//this is the same implementation as in Reform, we guard ourselfs against a change to the latter
		// without a thought about this class
		override def compatible[LF <: RowProduct, LS >: Grouped <: Single, LV,
		                        RF <: RowProduct, RS >: Grouped <: Single, RV, U]
		                       (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
		                       (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                                 spelling :SQLSpelling)
				:Boolean =
			try {
				spelling.columnCount(left) == spelling.columnCount(right)
			} catch {
				case _ :UnsupportedOperationException | _ :InvalidSQLException => false
			}

		protected override def validate[X, Y, V](left :Query[X, V], right :Query[Y, V])
		                                        (implicit spelling :SQLSpelling) :Unit =
			if (!compatible(left, right))
				throw new MismatchedExpressionsException(left, right,
					"different numbers of columns: " + columnCount(left) + " vs " + columnCount(right) + "."
				)

/*
		override def compatible[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) =
			try {
				spelling.columnCount(left) == spelling.columnCount(right)
			} catch  {
				case _ :UnsupportedOperationException | _ :InvalidSQLException => false
			}
*/
		override def typeName :String = "ArityValidator"
	}



	//This was supposed to be a pseudo reform which returns (left, left) or (right, right), depending on which
	//  has more informative layout: LValueSQL first, then LabeledSQL, then anything other than a term.
	//  Unfortunately it is impossible as the RowProduct and Scope type parameters of returned pair must match arguments.
	//  It doesn't matter really, as the use is only for RowProduct = Nothing and Scope = Grouped,
	//  but it's currently impossible to do it in a type safe way. We can define a GenericReform interface which
	//  defines an upper bound on RowProduct and Scope, which would work.
/*
	val LayoutReform :ExpressionReform = new LayoutReform

	private class LayoutReform(override val mayExcludeLeft :Boolean, override val mayIncludeLeft :Boolean,
	                           override val mayReorderLeft :Boolean, override val mayAddNullLeft :Boolean)
	                          (override val mayExcludeRight :Boolean, override val mayIncludeRight :Boolean,
	                           override val mayReorderRight :Boolean, override val mayAddNullRight :Boolean)
	                          (wrap :ExpressionReform => ExpressionReform)
		extends TunedReform[ExpressionLValue](mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft)(
		                                      mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight)(
		                                      wrap, new LayoutReform(_, _, _, _)(_, _, _, _)(_))
		   with PreprocessorReform
	{
		def this() = this(true, true, true, true)(true, true, true, true)(identity)

		override lazy val swap =
			if ((this eq self) && isSymmetrical)
				self
			else
				new LayoutReform(mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight)(
				                 mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft)(wrap)
				{
					override lazy val swap = LayoutReform.this.self
				}.self

		override def apply[L <: RowProduct, A >: Grouped <: Single, X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
		                  (left :SQLExpression[L, A, X], right :SQLExpression[R, B, Y])
		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(SQLExpression[L, A, Z], SQLExpression[R, B, Z]) =
			(left, right) match {
				case (_ :CompositeSQL[_, _, _], _ :CompositeSQL[_, _, _]) => super.apply(left, right)
				case _ => fallback(left, right) //MappingSQL, SQLTerm, QuerySQL
			}

		override def apply[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
		                  (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
		                  (implicit compat :SQLTypeUnification[left.Subject, right.Subject, Z], spelling :SQLSpelling) =
			fallback(left, right)

//		override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
//		                  (left :ComponentLValueSQL[L, A, X], right :SQLExpression[R, B, Y])
//		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
//			super.apply(left, right)
//
//		override def apply[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
//		                  (left :SQLExpression[L, A, X], right :ComponentLValueSQL[R, B, Y])
//		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
//			super.apply(left, right)
//
//		override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
//		                  (left :ComponentLValueSQL[L, A, X], right :ComponentLValueSQL[R, B, Y])
//		                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
//

		@inline private def pair[X](x :X) = (x, x)

		override def fallback[L <: RowProduct, A >: Grouped <: Single, X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
		                     (left :SQLExpression[L, A, X], right :SQLExpression[R, B, Y])
		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(SQLExpression[L, A, Z], SQLExpression[R, B, Z]) =
			(left, right) match {
				case (_ :ComponentLValueSQL[L, MappingOf[X]#Projection, X] @unchecked, _) => left.to(compat.left)
			}

		override def fallback[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
		                     (left :SQLExpression[L, A, X], right :ComponentLValueSQL[R, B, Y])
		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
			pair(right.to(compat.right))

		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
		                     (left :ComponentLValueSQL[L, A, X], right :SQLExpression[R, B, Y])
		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
			pair(left.to(compat.left))

		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
		                     (left :ComponentLValueSQL[L, A, X], right :ComponentLValueSQL[R, B, Y])
		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) =
			pair(left.to(compat.left))
	}
*/



	/** The default reforming strategy used to unify the columns of a pair of SQL expressions used within another
	  * SQL/DML expression. It recursively adjusts the column sets of the two expressions delegating to their
	  * [[net.noresttherein.oldsql.sql.SQLExpression.reform reform]] methods and handling only the case
	  * of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] vs `ComponentSQL`.
	  *
	  * The aligning of columns between the two component expressions will be successful
	  * only either if the default column sets of the two expressions are already
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.ColumnCollectionExtension.isomorphic isomorphic]],
	  * or the component mappings are [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] and
	  * one expression is legible for being reformed. Either of the expressions will be adjusted only if:
	  *   1. Their argument flag (`mayReformLeft` or `mayReformRight`) is `true`, and
	  *   1. Both its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.relation relation]]
	  *      and its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]] are legible.
	  *      A relation is legible if it is either [[net.noresttherein.oldsql.schema.Relation.isDefault default]]
	  *      or [[net.noresttherein.oldsql.schema.Relation.equivalent equivalent]] to the other relation.
	  *      An origin is legible
	  *      if it is [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isDefault default]]
	  *      or the alterations made to it
	  *      ([[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes included]] and
	  *      [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excluded]] components)
	  *      ''effectively match'' those on the other `origin`. Effectively matching is understood here as resulting
	  *      in the same default column set for the component mapping, as defined by
	  *      the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]] mappings
	  *      of the two expressions. Note that this is generally a weaker requirement than that
	  *      of identical `includes` & `excludes` component lists, as it allows comparing components for the same type,
	  *      but different properties of the root entities. At the same time, having isomorphic `includes`/`excludes`
	  *      does not guarantee legibility, because due to different buffs the effective column sets may still differ.
	  *
	  * This strikes a balance between the convenience of the interoperability of default component expressions
	  * and control/predictability of the outcome. Essentially, only one of the expressions
	  * will be adjusted, and only if does not define conflicting alterations itself to the default column set
	  * of the component. The adjustment may result both in removing and adding columns to an expression.
	  *
	  * This process is a heuristic, and can always fail
	  * with a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
	  * due to buffs affecting the possibility of altering the column sets, or inability to reliably compare
	  * the mappings (in particular due to [[net.noresttherein.oldsql.schema.ColumnMapping column]]
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.form forms]] being unequal).
	  *
	  * Both expressions must be anchored in the same ''from'' clause and the results should only be used
	  * within the context of that clause and in the [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope scope]]
	  * defined by `spelling`: returned expressions may not be aligned if used in a different statement type/clause.
	  */
	val ReformDefaults :Reform      = new ReformDefaults(true, true)(identity)
	private val ReformLeftDefaults  = new ReformDefaults(true, false)(identity)
	private val ReformRightDefaults = new ReformDefaults(false, true)(identity)
	private val ReformNoneDefaults  = new ReformDefaults(false, false)(identity)

	def ReformDefaults(mayAlterLeft :Boolean, mayAlterRight :Boolean) :Reform =
		if (mayAlterLeft)
			if (mayAlterRight) ReformDefaults else ReformLeftDefaults
		else
			if (mayAlterRight) ReformRightDefaults else ReformNoneDefaults

	/** The default reforming strategy used to unify the columns of a pair of SQL expressions used within another
	  * SQL/DML expression. It recursively adjusts the column sets of the two expressions delegating to their
	  * [[net.noresttherein.oldsql.sql.SQLExpression.reform reform]] methods and handling only the case
	  * of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] vs `ComponentSQL`.
	  *
	  * The aligning of columns between the two component expressions will be successful
	  * only either if the default column sets of the two expressions are already
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.ColumnCollectionExtension.isomorphic isomorphic]],
	  * or the component mappings are [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] and
	  * one expression is legible for being reformed. Either of the expressions will be adjusted only if:
	  *   1. Their argument flag (`mayReformLeft` or `mayReformRight`) is `true`, and
	  *   1. Both its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.relation relation]]
	  *      and its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]] are legible.
	  *      A relation is legible if it is either [[net.noresttherein.oldsql.schema.Relation.isDefault default]]
	  *      or [[net.noresttherein.oldsql.schema.Relation.equivalent equivalent]] to the other relation.
	  *      An origin is legible
	  *      if it is [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isDefault default]]
	  *      or the alterations made to it
	  *      ([[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes included]] and
	  *      [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excluded]] components)
	  *      ''effectively match'' those on the other `origin`. Effectively matching is understood here as resulting
	  *      in the same default column set for the component mapping, as defined by
	  *      the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]] mappings
	  *      of the two expressions. Note that this is generally a weaker requirement than that
	  *      of identical `includes` & `excludes` component lists, as it allows comparing components for the same type,
	  *      but different properties of the root entities. At the same time, having isomorphic `includes`/`excludes`
	  *      does not guarantee legibility, because due to different buffs the effective column sets may still differ.
	  *
	  * This strikes a balance between the convenience of the interoperability of default component expressions
	  * and control/predictability of the outcome. Essentially, only one of the expressions
	  * will be adjusted, and only if does not define conflicting alterations itself to the default column set
	  * of the component. The adjustment may result both in removing and adding columns to an expression.
	  *
	  * This process is a heuristic, and can always fail
	  * with a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
	  * due to buffs affecting the possibility of altering the column sets, or inability to reliably compare
	  * the mappings (in particular due to [[net.noresttherein.oldsql.schema.ColumnMapping column]]
	  * [[net.noresttherein.oldsql.schema.ColumnMapping.form forms]] being unequal).
	  *
	  * Both expressions must be anchored in the same ''from'' clause and the results should only be used
	  * within the context of that clause and in the [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope scope]]
	  * defined by `spelling`: returned expressions may not be aligned if used in a different statement type/clause.
	  */
	private class ReformDefaults(override val mayAlterLeft :Boolean, override val mayAlterRight :Boolean)
	                            (wrap :Reform => Reform)
		extends SimpleReform(mayAlterLeft, mayAlterRight)(wrap, new ReformDefaults(_, _)(_))
	{
/*
		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
		                     (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
		                     (implicit leftResult :SQLTransformation[left.Subject, Z], rightResult :SQLTransformation[right.Subject, Z],
		                               spelling :SQLSpelling)
				:(leftResult.SQLResult[L, Single, LValueSQL[L, A, Z]],
				  rightResult.SQLResult[R, Single, LValueSQL[R, B, Z]]) =
*/
		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
		                    (implicit leftResult  :SQLTransformation[left.Subject, U],
		                              rightResult :SQLTransformation[right.Subject, U], spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
			if (left.mapping homomorphic right.mapping)
				(new DefaultComponentReformer[LF, LM, RF, RM, U, leftResult.SQLResult, rightResult.SQLResult](
					left, right)(self)(leftResult, rightResult, spelling
				))()
			else if (spelling.scope.defaultColumns(left.origin.anchored, left.anchored) //fat chance if the above failed
			                        isomorphic
			         spelling.scope.defaultColumns(right.origin.anchored, right.anchored)
			)
				(leftResult(left), rightResult(right))
			else
				throw new MismatchedExpressionsException(left, right,
					"Cannot reform non homomorphic components with different column sets: " +
						spelling.scope.defaultColumns(left.origin.anchored, left.anchored) + " vs " +
						spelling.scope.defaultColumns(right.origin.anchored, right.anchored) + "."
				)

		override lazy val swap =
			if (self.isSymmetrical)
				self
			else if (self eq this)
				ReformDefaults(mayAlterRight, mayAlterLeft)
			else
				SwappedReform(this)

		protected override def typeName :String = "ReformDefaults"
	}



	/** Extracted from [[net.noresttherein.oldsql.sql.mechanics.Reform.ReformDefaults ReformDefaults]]
	  * reforming of two component expressions. A separate class allows storing intermediate computations
	  * as member fields and easier reuse by overriding classes, allowing minor modifications without
	  * reimplementing the whole process.
	  */
	private abstract class HomomorphicComponentReformer
	                       [LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U,
	                        LR[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, U]] <: SQLExpression[F, S, U],
	                        RR[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, U]] <: SQLExpression[F, S, U]]
		                   (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])(reform :Reform)
		                   (implicit leftResult  :SQLTransformation[RM[Unit]#Subject, U]#Into[LR],
		                             rightResult :SQLTransformation[LM[Unit]#Subject, U]#Into[RR], spelling :SQLSpelling)
	{   //different components, we need to compare actual column sets
		if (left.mapping.columns.size != right.mapping.columns.size)
			throw new MismatchedExpressionsException(left, right,
				"different column counts on supposedly homomorphic mappings (" + left.mapping.columns +
					" vs. " + right.mapping.columns + "). This is a bug."
			)

		/** Indices of (export versions of) all columns of `left.mapping` within `left.origin.mapping.columns`.*/
		protected val leftColumnIndices = left.mapping.columns.map {
			col => left.origin.mapping.columns.columnIndex(left.origin.mapping.export(col))
		}
		/** Indices of (export versions of) all columns of `right.mapping` within `right.origin.mapping.columns`. */
		protected val rightColumnIndices = right.mapping.columns.map {
			col => right.origin.mapping.columns.columnIndex(right.origin.mapping.export(col))
		}
		/** Indices of the default columns of `left.anchored` within `left.origin.anchored.columns`. */
		protected val leftDefaultIndices = defaultIndices(left)
		/** The default column set of `left` as export columns of `left.origin.mapping`,
		  * in order consistent with `left.origin.anchored.export(left.mapping)` - the formatting order.
		  */
		protected val leftDefaultColumns = leftDefaultIndices.map(left.origin.mapping.columns(_))
		/** Indices of the default columns of `right.anchored` within `right.origin.anchored.columns`. */
		protected val rightDefaultIndices = defaultIndices(right)
		/** The default column set of `right` as export columns of `right.origin.mapping`. */
		protected val rightDefaultColumns = rightDefaultIndices.map(right.origin.mapping.columns(_))

		protected val (
			/** Counterparts of `right.mapping.columns` in `left.mapping` as export versions for `left.origin.mapping`. */
			leftCounterparts,
			/** Counterparts of `left.mapping.columns` in `right.mapping` as export versions for `right.origin.mapping`. */
			rightCounterparts
		) =
			try { counterparts(left, right) } catch {
				case _ :IncompatibleMappingsException => counterparts(right, left).swap
			}
		/** Counterparts of the default (for `right.origin.anchored`) columns of `right.mapping` in `left.mapping`
		  * as export versions for `left.origin.mapping`. */
		protected val leftDefaultCounterparts =
			defaultCounterparts(leftCounterparts, right, rightDefaultIndices, rightColumnIndices)
		/** Counterparts of the default (for `left.origin.anchored`) columns of `left.mapping` in `right.mapping`
		  * as export versions for `right.origin.mapping`. */
		protected lazy val rightDefaultCounterparts =
			defaultCounterparts(rightCounterparts, left, leftDefaultIndices, leftColumnIndices)

		/** Indices within `component.origin.mapping` of the default column set of `component.mapping`
		  * (as per `component.origin.anchored`) for `spelling.scope`, preserving the order from `component.origin.anchored`.
		  */
		protected def defaultIndices[G <: RowProduct, M[O] <: MappingAt[O]](component :ComponentSQL[G, M])
				:Unique[Int] =
		{
			//the permutation changing the order of columns between origin.mapping and origin.anchored - typically identity
			val anchoredIndices = component.origin.mapping.columns.map(
				col => component.origin.anchored.columns.columnIndex(component.origin.anchored.export(col))
			) //in case the order of columns in anchored changed, for example due to being a ReorderedMapping
			val defaultAnchoredColumns = spelling.scope.defaultColumns(component.origin.anchored, component.anchored)
			defaultAnchoredColumns.map {
				col => anchoredIndices.sureIndexOf(component.origin.anchored.columns.columnIndex(col))
			}
		}
		protected def counterparts[F1 <: RowProduct, M1[O] <: MappingAt[O], F2 <: RowProduct, M2[O] <: MappingAt[O]]
		                          (expr1 :ComponentSQL[F1, M1], expr2 :ComponentSQL[F2, M2]) =
		{
			val counterpartsInExpr1 =
				expr1.mapping.counterpartColumns(expr2.mapping, expr2.mapping.columns)
				     .map(expr1.origin.mapping.export(_))
			val counterpartsInExpr2 = expr1.mapping.columns.map { col =>
				val expr2ComponentIndex = counterpartsInExpr1.columnIndex(expr1.origin.mapping.export(col))
				expr2.origin.mapping.export(expr2.mapping.columns(expr2ComponentIndex))
			}
			(counterpartsInExpr1, counterpartsInExpr2)
		}
		protected def defaultCounterparts[F1 <: RowProduct, F2 <: RowProduct, M2[O] <: MappingAt[O]]
		                                 (expr1Counterparts :Unique[TypedColumn[_, F1]], expr2 :ComponentSQL[F2, M2],
		                                  expr2DefaultIndices :Unique[Int], expr2Indices :Unique[Int]) =
		{
			val expr2DefaultComponentColumns = expr2DefaultIndices.map {
				indexInExpr2Origin => expr2.mapping.columns(expr2Indices.sureIndexOf(indexInExpr2Origin))
			}
			expr2DefaultComponentColumns.map {
				expr2Column => expr1Counterparts(expr2.mapping.columns.columnIndex(expr2Column))
			}
		}

		//the row mapping type is preserved and hence the component mappings have type-compatible counterparts
		protected val sameEntity = left.relation.row compatible right.relation.row
		//origin mappings are isomorphic, columns at the same indices in both represent the same extracts
		protected val isomorphicEntity = sameEntity || (left.relation.row isomorphic right.relation.row)
		//do both left and right describe the same extract / part of the subject?
		protected val sameComponent = isomorphicEntity && leftColumnIndices == rightColumnIndices

		/** Default columns of `left.altered`, that is with applied includes/excludes on origin level,
		  * as export columns of `left.origin.mapping`. */
		protected def leftAlteredColumns =
			left.origin.mapping.counterpartColumns(
				left.origin.altered, spelling.scope.defaultColumns(left.origin.altered, left.altered)
			)
		/** Default columns of `right.altered`, that is with applied includes/excludes on origin level,
		  * as export columns of `right.origin.mapping`. */
		protected def rightAlteredColumns =
			right.origin.mapping.counterpartColumns(
				right.origin.altered, spelling.scope.defaultColumns(right.origin.altered, right.altered)
			)

		/** Counterpart columns in `left.mapping` of the default for `right.origin.altered` columns of `right.mapping`
		  * as export columns of `left.origin.mapping`. */
		protected def leftAlteredCounterparts =
			rightAlteredColumns.map { rightColumn =>
				val rightOriginIndex = right.origin.mapping.columns.columnIndex(rightColumn)
				val rightComponentIndex = rightColumnIndices.sureIndexOf(rightOriginIndex)
				leftCounterparts(rightComponentIndex)
			}

		private def reformFirst[F1 <: RowProduct, M1[O] <: MappingAt[O], F2 <: RowProduct, M2[O] <: MappingAt[O], U,
		                        Res <: SQLExpression[F1, Single, U]]
		                       (expr1 :ComponentSQL[F1, M1], expr2 :ComponentSQL[F2, M2])
		                       (expr1DefaultColumns :Unique[TypedColumn[_, expr1.Origin]],
		                        expr2DefaultColumns :Unique[TypedColumn[_, expr2.Origin]],
		                        expr1Counterparts :Unique[TypedColumn[_, expr1.Origin]],
		                        expr1DefaultCounterparts :Unique[TypedColumn[_, expr1.Origin]])
		                       (implicit conversion :SQLTransformation[M1[Unit]#Subject, U])
			:conversion.SQLResult[F1, Single, LValueSQL[F1, M1, U]] =
		{
			val reformed =
				if (sameComponent && sameEntity) {
					val expr2Origin = expr2.origin.asInstanceOf[
						JoinedRelation[expr2.Origin, expr1.Entity] { type FromLast = expr2.FromLast }
					]
					val expr1Position = RelationOffset.ofRelation[expr1.Origin, expr2.FromLast, expr1.Entity](
						expr1.origin.position.index
					)
					expr1.graft(expr2Origin.moveTo(expr1Position))
				} else {
					val includes = expr1DefaultCounterparts.filterNot(expr1DefaultColumns.contains)
					val excludes = expr1Counterparts.filter {
						col => !expr1DefaultCounterparts.contains(col) && expr1DefaultColumns.contains(col)
					}
					val candidate = expr1.alter(includes, excludes)
					val defaults = defaultIndices(candidate).map(candidate.origin.mapping.columns(_).withOrigin[expr1.Origin])
					if (defaults == expr1DefaultCounterparts)
						candidate
					else if (defaults.toSet == expr1DefaultCounterparts.toSet) {
						val permutation = defaults.toIndexedSeq.map(expr1DefaultCounterparts.columnIndex(_))
						candidate.reorder(permutation)
					} else
						throw new MismatchedExpressionsException(expr1, expr2,
							"attempted reforming of the first expression to match the second one (" +
								includes.mkString("include(", ", ", "); ") + excludes.mkString("exclude(", ", ", ")") +
								"resulted in expression " + candidate + " with default column set " +
								defaults + " not matching " + expr2DefaultColumns + "."
						)
				}
			val res = conversion(reformed)
			if (spelling.columnCount(reformed) != spelling.columnCount(res))
				throw new MismatchedExpressionsException(left, right,
					"conversion " + conversion + " of " + reformed + " to " + res + " changed the column count: "
						+ spelling.columnCount(reformed) + " vs " + spelling.columnCount(res) + "."
				)
			res
		}

		protected def reformLeft =
			reformFirst(left, right)(
				leftDefaultColumns, rightDefaultColumns, leftCounterparts, leftDefaultCounterparts
			)

		protected def reformRight =
			reformFirst(right, left)(
				rightDefaultColumns, leftDefaultColumns, rightCounterparts, rightDefaultCounterparts
			)

		protected def convertLeft = leftResult(left)
		protected def convertRight = rightResult(right)

		protected def fail() =
			throw new MismatchedExpressionsException(left, right, "cannot be unified with " + reform +
				". Column sets are: " + leftDefaultColumns + " and " + rightDefaultColumns + "."
			)

		def apply() :(LR[LF, Single, LValueSQL[LF, LM, U]], RR[RF, Single, LValueSQL[RF, RM, U]])
	}


	/** Reforms a [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isDefault default]]
	  * component expression to match the other component expression.
	  */ //fixme: doesn't support ExtraXxx/CustomXxx buffs which add columns over defaults!
	private class DefaultComponentReformer
	              [LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U,
	               LR[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, U]] <: SQLExpression[F, S, U],
	               RR[-F <: RowProduct, -S >: Grouped <: Single, +E <: SQLExpression[F, S, U]] <: SQLExpression[F, S, U]]
		          (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])(reform :Reform)
		          (implicit leftResult  :SQLTransformation[RM[Unit]#Subject, U]#Into[LR],
		                    rightResult :SQLTransformation[LM[Unit]#Subject, U]#Into[RR],
		                    spelling :SQLSpelling)
		extends HomomorphicComponentReformer[LF, LM, RF, RM, U, LR, RR](left, right)(reform :Reform)(leftResult, rightResult, spelling)
	{
		import reform.{mayAlterLeft, mayAlterRight}

		def apply() :(LR[LF, Single, LValueSQL[LF, LM, U]], RR[RF, Single, LValueSQL[RF, RM, U]]) =
			if (leftDefaultColumns == leftDefaultCounterparts)
				(convertLeft, convertRight) //we don't check isomorphism of columns here in order to avoid a bug of false negatives
			else if (!(mayAlterLeft || mayAlterRight))
				fail()
			else if (left.relation isomorphic right.relation) //same alterations on the relations, only origins differ
				if (mayAlterLeft && left.isDefault)
					(reformLeft, convertRight)
				else if (mayAlterRight && right.isDefault)
					(convertLeft, reformRight)
				else if (leftDefaultColumns.toSet == leftDefaultCounterparts.toSet)
					if (mayAlterLeft)
						(reformLeft, convertRight)
					else
						(convertLeft, reformRight)
				else
					fail()
			else //alterations on left.relation and right.relation differ
				if (mayAlterLeft && left.isDefault && left.relation.isDefault)         //reform left
					(reformLeft, convertRight)
				else if (mayAlterRight && right.isDefault && right.relation.isDefault) //reform right
					(convertLeft, reformRight)
				else if (leftDefaultColumns.toSet == leftDefaultCounterparts.toSet) //effective alterations are the same, just differently ordered
					if (mayAlterLeft)
						(reformLeft, convertRight)
					else
						(convertLeft, reformRight)
				else if (leftAlteredColumns.toSet == leftAlteredCounterparts.toSet)
					//left.origin and right.origin apply equivalent alterations from the point of view of component mappings
					if (mayAlterLeft && left.relation.isDefault)        //reform left
						(reformLeft, convertRight)
					else if (mayAlterRight && right.relation.isDefault) //reform right
						(convertLeft, reformRight)
					else
						fail()
				else
					fail()
	}

}


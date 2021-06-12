package net.noresttherein.oldsql.sql

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{INSERT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnMapping, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.{denullify, ColumnComponentSQL, ColumnLValueSQL, ComponentLValueSQL, ComponentSQL, SQLNull, SQLParameter}






//todo: ComponentUpdates - a collection object joining them
/** Representation of assignment of a value to a table's component as a pair
  * of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]. It is typically created by the expression
  * `lvalue `[[net.noresttherein.oldsql.sql.ast.ComponentSQL.:= :=]]` rvalue`, where
  * `lvalue :`[[net.noresttherein.oldsql.sql.ast.ComponentSQL]]`[L, M]` and
  * `rvalue :SQLExpression[R, GlobalScope, X]`, with the mapping's `M` subject type and `X` being either equal
  * or equivalent in the terms of [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
  * It is used in the creation of [[net.noresttherein.oldsql.sql.Insert Insert]]
  * and [[net.noresttherein.oldsql.sql.Update Update]] SQL statements (including their variants), both as part
  * of the DSL API and internally, in preparation for saving an entity.
  * @tparam L the domain [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] on which the left side of the assignment
  *           is based; it is always [[net.noresttherein.oldsql.sql.From From]]`[M]`
  *           (or `RowProduct `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]` M`), where `M` is the mapping type
  *           of the updated table.
  * @tparam R the domain [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] on which the right side
  *           of the assignment is based, representing legitimate `r-values`. It is either
  *           [[net.noresttherein.oldsql.sql.From From]]`[M]`
  *           (or `RowProduct `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]` M`) for updates without
  *           [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters and
  *           `From[M] `[[net.noresttherein.oldsql.sql.WithParam]]` X` (or
  *           `RowProduct AndFrom `[[net.noresttherein.oldsql.sql.UnboundParam.ParamRelation ParamRelation]]`[X]#Param`)
  *           for parameterized updates. Note that in the former case, the expression can still contain
  *           [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]] parameters.
  * @tparam V the shared type of the left side's mapping's [[net.noresttherein.oldsql.schema.Mapping.Subject subject]]
  *           and of the assigned expression on the right side, possibly after unification.
  * @author Marcin Mościcki
  */ //consider: replacing L with mapping type M
trait ComponentSetter[-L <: RowProduct, -R <: RowProduct, V] extends Serializable {
	/** The type of the set component as a function of the `Origin` type. */
	type Component[O] <: MappingAt[O]

	/** An expression for the component to which a value is being assigned. `ComponentLValueSQL` has only two
	  * (direct) subtypes: [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] - representing
	  * the component directly -
	  * and [[net.noresttherein.oldsql.sql.ast.ComponentLValueSQL ComponentLValueSQL]],
	  * which is a [[net.noresttherein.oldsql.sql.ast.ConversionSQL ConversionSQL]] subtype with a `ComponentSQL` as
	  * the adapted expression type, created by [[net.noresttherein.oldsql.sql.ast.ComponentSQL.:= :=]]
	  * if the types of both sides do not match exactly, but are automatically convertible in SQL.
	  */
	val lvalue :ComponentLValueSQL[L, Component, V]

	/** An expression for the value assigned to the component. */
	val rvalue :SQLExpression[R, GlobalScope, V]

	/** Replaces all occurrences of [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]
	  * in both left and right sides of the assignment
	  * with a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], using the relation at
	  * the given position in `L`/`R` as its parent.
	  */
	def anchor(table :L, params :R) :ComponentSetter[L, R, V] =
		//important to use := as it can be overloaded to establish a common column set
		lvalue.anchor(table) := rvalue.anchor(params)

	def split(operation :OperationType) :Seq[ColumnSetter[L, R, _]] = { ???
		//fixme:
//		val component = lvalue.component
//		import component.Origin
//		import component.Subject
//		val mapping = component.export
//		val columns = operation.defaultColumns(mapping)
//		columns.view.map { column =>
//			def filter[T](column :ColumnMapping[T, Origin]) :ColumnSetter[L, R, _] =
//		}
	}

	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentSetter[_, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :ComponentSetter[_, _, _] if (this canEqual other) && (other canEqual this) =>
			lvalue == other.lvalue && rvalue == other.rvalue
	}

	override def hashCode :Int = lvalue.hashCode * 31 + rvalue.hashCode

	override def toString :String = lvalue.toString + "=" + rvalue.toString
}




object ComponentSetter {
	type MappingUpdate[M[O] <: MappingAt[O], -R <: RowProduct, V] = ComponentSetter[From[M], R, V]

	/** A type alias for [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]] of any expression type,
	  * which in most of its use cases is not needed. When used in an infix manner,
	  * `T := F` (for some `T, F <: RowProduct)`, it represents an assignment
	  * of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, GlobalScope, X]` to
	  * a [[net.noresttherein.oldsql.sql.ast.ComponentLValueSQL ComponentLValueSQL]]`[T, GlobalScope, X]`.
	  * The latter is a supertype of [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions -
	  * representations of column sets of some table in `T` and their SQL-transparent conversions (used to unify
	  * the types of both ''lvalue'' and ''rvalue'' by applying available implicit conversions to the expressions,
	  * in order to bring them both to some common type - for example by number type promotion). The former
	  * is any non-aggregate expression based on ''from'' clause `F`. Currently, `T` is always
	  * [[net.noresttherein.oldsql.sql.From From]]`[M]`, enforcing that `lvalue` of `:=` is a component
	  * of a table with row [[net.noresttherein.oldsql.schema.Mapping mapping]] `M`. `F` can also be `From[M]` -
	  * for [[net.noresttherein.oldsql.sql.Update updates]]
	  * of table [[net.noresttherein.oldsql.schema.Relation.RelVar RelVar]]`[M]` which do not depend on any outside
	  * parameters (which use only to the updated table itself and expressions with concrete values -
	  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] or
	  * [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]] parameters); it can however also take many more
	  * forms, in practice some combination of `From[M]` and [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
	  * pseudo joins introducing expressions for statement parameters with values not known at their creation.
	  * For example, `From[M] := (From[M] WithParam X)` is an assignment usable by ''update'' statements
	  * parameterized with `X`: it assigns to some updated component of `M` a value of an expression depending
	  * on `M` itself and a parameter of type `X`. In [[net.noresttherein.oldsql.sql.Insert Insert]] statements
	  * the inserted values cannot refer to the inserted row, as no preexisting values are present;
	  * thus, `From[M] := RowProduct` is used by parameterless statements (enforcing that the assigned values
	  * are self-contained), or `From[M] := (FromSome WithParam X)` for statements parameterized with type `X`,
	  * as in the previous case. For both of these statement types, additional parameters can be added by subsequent use
	  * of [[net.noresttherein.oldsql.sql.WithParam WithParam]].
	  *
	  * Instances can be created with overloaded [[net.noresttherein.oldsql.sql.ast.ComponentLValueSQL.:= :=]]
	  * method available on [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions,
	  * as well as other, related assignment operators with more traditional forms
	  * such as [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.+= +=]]. Additionally,
	  * most of these operators have a version ending with `?`- for example,
	  * [[net.noresttherein.oldsql.sql.ast.ComponentLValueSQL.:=? :=?]]
	  * or [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.+=? +=?]] - which accept any scala value
	  * of a compatible type with an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] type class,
	  * and use it, wrapping it in a [[net.noresttherein.oldsql.sql.ast.SQLParameter SQLParameter]]
	  * (rather than a [[net.noresttherein.oldsql.sql.ast.SQLLiteral SQLLiteral]] to which it would be otherwise
	  * implicitly converted). This will render the SQL for the expression as the JDBC parameter placeholder `"?"`,
	  * which is important in order to not flood the cache of the driver and the database with countless versions
	  * inlining their value.
	  *
	  * Note that `Mapping` subtypes `M[F]` - with an evident type constructor `M[O] <: Mapping { type Origin = O }`
	  * including an implicit [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] type class -
	  * are implicitly convertible to [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[F, M]`
	  * when used in the ''lvalue'' position of operator `:=` and its kin (just as invoking other operator-methods
	  * of `SQLExpression`, including in particular comparisons
	  * such as [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]]
	  * and arithmetic operators such as [[net.noresttherein.oldsql.sql.ColumnSQL.+ +]], will cause
	  * this implicit conversion).
	  */
	type :=[-L <: RowProduct, -R <: RowProduct] = ComponentSetter[L, R, _]

	object := {
		def unapply[L <: RowProduct, R <: RowProduct, X](setter :ComponentSetter[L, R, X])
				:Opt[(ComponentLValueSQL[L, setter.Component, X], SQLExpression[R, GlobalScope, X])] =
			Got((setter.lvalue, setter.rvalue))
	}


	def apply[L <: RowProduct, M[O] <: RefinedMapping[X, O], X, R <: RowProduct, Y, V]
	         (component :ComponentSQL[L, M], value :SQLExpression[R, GlobalScope, Y])
	         (implicit promote :SQLTypeUnification[X, Y, V]) :ComponentSetter[L, R, V] =
		apply(component.to(promote.left), promote.right(value))

	def apply[L <: RowProduct, M[O] <: MappingAt[O], R <: RowProduct, V]
	         (component :ComponentLValueSQL[L, M, V], value :SQLExpression[R, GlobalScope, V]) :ComponentSetter[L, R, V] =
		new ComponentSetter[L, R, V] {
			override type Component[O] = M[O]
			override val lvalue = component
			override val rvalue = denullify(value)
		}

	def unapply[L <: RowProduct, R <: RowProduct, V](assignment :ComponentSetter[L, R, V])
			:Opt[(ComponentLValueSQL[L, assignment.Component, V], SQLExpression[R, GlobalScope, V])] =
		Got(assignment.lvalue, assignment.rvalue)

}






/** Representation of assignment of a value to a single column of a table as a pair
  * of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]. It is typically created by the expression
  * `lvalue `[[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.:= :=]]` rvalue`, where
  * `lvalue :`[[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL]]`[L, M]` and
  * `rvalue :`[[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[R, GlobalScope, X]`, with the mapping's `M`
  * subject type and `X` being either equal or equivalent in the terms of
  * [[net.noresttherein.oldsql.sql.SQLExpression.SQLTypeUnification SQLTypeUnification]].
  * It is used in the creation of [[net.noresttherein.oldsql.sql.Insert Insert]]
  * and [[net.noresttherein.oldsql.sql.Update Update]] SQL statements, mainly internally.
  * @tparam L the domain [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] on which the left side of the assignment
  *           is based; it is always [[net.noresttherein.oldsql.sql.From From]]`[M]`
  *           (or `RowProduct `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]` M`), where `M` is the mapping type
  *           of the updated table.
  * @tparam R the domain [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] on which the right side
  *           of the assignment is based, representing legitimate `r-values`. It is either
  *           [[net.noresttherein.oldsql.sql.From From]]`[M]`
  *           (or `RowProduct `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]` M`) for updates without
  *           [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters and
  *           `From[M] `[[net.noresttherein.oldsql.sql.WithParam]]` X` (or
  *           `RowProduct AndFrom `[[net.noresttherein.oldsql.sql.UnboundParam.ParamRelation ParamRelation]]`[X]#Param`)
  *           for parameterized updates. Note that in the former case, the expression can still contain
  *           [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]] parameters.
  * @tparam V the shared type of the left side's mapping's [[net.noresttherein.oldsql.schema.Mapping.Subject subject]]
  *           and of the assigned expression on the right side, possibly after unification.
  * @author Marcin Mościcki
  */ //consider: if L is replaced with M in ComponentSetter, than it could be completely omitted here.
trait ColumnSetter[-L <: RowProduct, -R <: RowProduct, V] extends ComponentSetter[L, R, V] {
	override type Component[O] <: ColumnMapping[_, O]
	override val lvalue :ColumnLValueSQL[L, Component, V]
	override val rvalue :ColumnSQL[R, GlobalScope, V]

	override def anchor(table :L, params :R) :ColumnSetter[L, R, V] =
		ColumnSetter(lvalue.anchor(table), rvalue.anchor(params))
}




object ColumnSetter {
	def apply[L <: RowProduct, M[O] <: ColumnMapping[X, O], X, R <: RowProduct, Y, V]
	         (column :ColumnComponentSQL[L, M, X], value :ColumnSQL[R, GlobalScope, Y])
	         (implicit promote :SQLTypeUnification[X, Y, V]) :ColumnSetter[L, R, V] =
		apply(column.to(promote.left), promote.right(value))

	def apply[L <: RowProduct, M[O] <: ColumnMapping[_, O], R <: RowProduct, V]
	         (column :ColumnLValueSQL[L, M, V], value :ColumnSQL[R, GlobalScope, V]) :ColumnSetter[L, R, V] =
		new ColumnSetter[L, R, V] {
			override type Component[O] = M[O]
			override val lvalue = column
			override val rvalue = value
		}

	def unapply[L <: RowProduct, R <: RowProduct, V](assignment :ComponentSetter[L, R, V])
			:Opt[(ColumnLValueSQL[L, m, V], ColumnSQL[R, GlobalScope, V]) forSome { type m[O] <: ColumnMapping[_, O] }] =
		assignment match {
			case set :ColumnSetter[L, R, V]  => Got((set.lvalue, set.rvalue))
			case _ => Lack
		}


	def apply[S, M[O] <: BaseMapping[S, O]](table :Table[M], value :S, operation :WriteOperationType)
			:Seq[ColumnSetter[From[M], RowProduct, _]] =
	{
		val domain = From(table)
		val mapping = table.export.asInstanceOf[RefinedMapping[S, RowProduct AndFrom M]]
		val columns = operation.defaultColumns(mapping)
		columns.view.map { column =>
			def filter[T](column :ColumnMapping[T, RowProduct AndFrom M]) :ColumnSetter[From[M], RowProduct, T] = {
				val extract = mapping(column) //this is somewhat fishy, as we create SQL for an export, not original
				val t = extract.opt(value)
				if (t.isDefined) domain.last \ column :=  SQLParameter(t.get)(column.form)
				else domain.last \ column := SQLNull[T](column.form)
			}
			filter(column)
		}.to(ArraySeq)
	}

	def updates[S, M[O] <: BaseMapping[S, O]](table :Table[M], value :S) :Seq[ColumnSetter[From[M], RowProduct, _]] =
		ColumnSetter(table, value, UPDATE)

	def inserts[S, M[O] <: BaseMapping[S, O]](table :Table[M], value :S) :Seq[ColumnSetter[From[M], RowProduct, _]] =
		ColumnSetter(table, value, INSERT)

	def apply[S, M[O] <: BaseMapping[S, O]](table :Table[M], operation :WriteOperationType)
			:Seq[ColumnSetter[From[M], JoinParam.Last[S], _]] =
		ColumnSetter(From(table).param[S](table.row.selectForm <> operation.form(table.row)), operation)

	def updates[S, M[O] <: BaseMapping[S, O]](domain :From[M] WithParam S)
			:Seq[ColumnSetter[From[M], JoinParam.Last[S], _]] =
		ColumnSetter(domain, UPDATE)

	def inserts[S, M[O] <: BaseMapping[S, O]](domain :From[M] WithParam S)
			:Seq[ColumnSetter[From[M], JoinParam.Last[S], _]] =
		ColumnSetter(domain, INSERT)

	def apply[S, M[O] <: BaseMapping[S, O]](domain :From[M] WithParam S, operation :WriteOperationType)
			:Seq[ColumnSetter[From[M], JoinParam.Last[S], _]] =
	{
		val mapping = domain.left.table.export.asInstanceOf[RefinedMapping[S, RowProduct AndFrom M]]
		val param = domain.last.mapping
		val columns = operation.defaultColumns(mapping)
		columns.view.map { column =>
			def filter[T](column :ColumnMapping[T, RowProduct AndFrom M]) :ColumnSetter[From[M], JoinParam.Last[S], _] =
				domain.left.last \ column :=
					domain.last \ (param.col(mapping(column))(column.form) :ColumnMapping[T, JoinParam.Last[S]])
			filter(column)
		}.to(ArraySeq)
	}
}






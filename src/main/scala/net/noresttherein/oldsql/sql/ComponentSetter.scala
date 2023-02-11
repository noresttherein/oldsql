package net.noresttherein.oldsql.sql

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.OperationView.{InsertView, UpdateView, WriteOperationView}
import net.noresttherein.oldsql.collection.{Opt, PassedArray}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.MismatchedExpressionsException
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Table
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.{denullify, BoundParam, ColumnComponentSQL, ColumnLValueSQL, ComponentSQL, GenericColumnComponentSQL, LValueSQL, SQLNull}
import net.noresttherein.oldsql.sql.mechanics.{=~=, SQLConversion}

//here be implicits
import net.noresttherein.oldsql.slang._





//todo: ComponentUpdates - a collection object joining them
/** Representation of assignment of a value to a table's component as a pair
  * of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]. It is typically created by the expression
  * `lvalue `[[net.noresttherein.oldsql.sql.ast.ComponentSQL.:= :=]]` rvalue`, where
  * `lvalue :`[[net.noresttherein.oldsql.sql.ast.ComponentSQL]]`[L, M]` and
  * `rvalue :SQLExpression[R, GlobalScope, X]`, with the mapping's `M` subject type and `X` being either equal
  * or equivalent in the terms of [[net.noresttherein.oldsql.sql.mechanics.=~= =~=]].
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
  *           `RowProduct AndFrom `[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param`)
  *           for parameterized updates. Note that in the former case, the expression can still contain
  *           [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters.
  * @author Marcin Mościcki
  */
trait ComponentSetter[-L <: RowProduct, -R <: RowProduct] extends Serializable { self =>
	/** The unified value type of left and right side of this assignment. Note that the `Subject` type of the mapping
	  * may be different, as `lvalue` may be a `ComponentLValueSQL`.
	  */
	type Value

	/** The type of the set component as a function of the `Origin` type. */
	type Component[O] <: MappingAt[O]

	/** An expression for the component to which a value is being assigned. `ComponentLValueSQL` has only two
	  * (direct) subtypes: [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] - representing
	  * the component directly - and [[net.noresttherein.oldsql.sql.ast.LValueSQL LValueSQL]],
	  * which is a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]] subtype with a `ComponentSQL` as
	  * the adapted expression type, created by [[net.noresttherein.oldsql.sql.ast.ComponentSQL.:= :=]]
	  * if the types of both sides do not match exactly, but are automatically convertible in SQL.
	  */
	val lvalue :LValueSQL[L, Component, Value]

	/** An expression for the value assigned to the component. */
	val rvalue :SQLExpression[R, Single, Value]

	/** Replaces all occurrences of [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]
	  * in both left and right sides of the assignment
	  * with a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]], using the relation at
	  * the given position in `L`/`R` as its parent.
	  */
	def anchor(table :L, params :R) :ComponentSetter[L, R] = //should we preserve Component and Value types?
		//important to use := as it can be overloaded to establish a common column set
		lvalue.anchor(table) := rvalue.anchor(params)

	/** Splits this assignment into assignments for individual columns of this component.
	  * This will be the default set for `this.value`[[net.noresttherein.oldsql.sql.ComponentSetter.lvalue lvalue]]
	  * for the specified operation type, i.e.
	  * `lvalue.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]]`.`[[net.noresttherein.oldsql.schema.Mapping.columns[T](operation:* columns]]`(operation, lvalue.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]`)`.
	  * Note that the use of `anchored` mapping above allows for its manual adjustment by
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include including]] or
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.exclude excluding]] optional columns.
	  * However, if the column counts for `lvalue` and `rvalue` differ, an attempt is made to reconcile the sets
	  * by `lvalue.`[[net.noresttherein.oldsql.sql.SQLExpression.reform reform]]`(rvalue)`. This is a symmetrical
	  * process, meaning that the final column set may not be the default one declared by the mapping on the left side.
	  * Neither of the sides will be changed if it is already explicitly
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.alter altered]] by the application,
	  * either on the [[net.noresttherein.oldsql.schema.Table table]]
	  * or [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] level - only those component expressions
	  * for which [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isDefault isDefault]] is `true`
	  * can be modified.
	  * @param operation indicator of the DML operation used to set the column values. While technically
	  *                  values other than [[net.noresttherein.oldsql.OperationView.InsertView InsertView]]
	  *                  and [[net.noresttherein.oldsql.OperationView.UpdateView UpdateView]] are valid,
	  *                  they are never used in this capacity.
	  */
	def split(operation :OperationView) :Seq[ColumnSetter[L, R]] = {
		//This is a bit fishy, as we might end up using different spelling than when actually spelling.
		// It will not produce an error, as we separate everything into columns that hopefully nobody tries to render
		// as multiple columns, but the results may be different if someone provides a custom reforming strategy.
		// At some point it might be beneficial to review this and defer the split, if possible.
		implicit val defaultSpelling = StandardSQL.spelling.in(operation.spellingScope)
		//todo: review and decide if we are fine with allowing the left side to be reformed
		val (left, right) = defaultSpelling.setterReform(lvalue, rvalue)
		val lvalues = defaultSpelling.split(left)
		val rvalues = defaultSpelling.split(right) //todo: better split is possible if rvalue is an RWTerm based on lvalue extractors
		if (lvalues.length != rvalues.length)
			throw new MismatchedExpressionsException(
				"Cannot split assignment (" + this + ") into individual column assignments for " + operation +
				"; The numbers of columns differ after reconciliation attempt (" + left + " := " + right + "): " +
				lvalues + " vs. " + rvalues + ". This is most likely a bug."
			)
		lvalues.view.zip(rvalues).map { case (column, expr) =>
			val l = column.castFrom[ColumnLValueSQL.from[L]#__, ColumnComponentSQL[L, Any]]
			val r = expr.castFrom[ColumnSQL[R, Single, _], ColumnSQL[R, Single, Any]]
			ColumnSetter(l, r)
		} to ArraySeq
	}

	/** Alters the left and right side of this assignment so that their column counts are equal in the scope
	  * defined by the implicit `SQLSpelling` argument.
	  * @return `ComponentSetter(spelling.setterReform(lvalue, rvalue))`.
	  */
	def reform(implicit spelling :SQLSpelling)
			:ComponentSetter[L, R] { type Component[O] = self.Component[O]; type Value = self.Value } =
	{
		val (left, right) = spelling.setterReform(lvalue, rvalue)
		ComponentSetter(left, right)
	}


	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentSetter[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :ComponentSetter[_, _] if (this canEqual other) && (other canEqual this) =>
			lvalue == other.lvalue && rvalue == other.rvalue
	}

	override def hashCode :Int = lvalue.hashCode * 31 + rvalue.hashCode

	override def toString :String = lvalue.toString + "=" + rvalue.toString
}




object ComponentSetter {
	type MappingUpdate[M[O] <: MappingAt[O], -R <: RowProduct] = ComponentSetter[From[M], R]

	def apply[L <: RowProduct, M[O] <: TypedMapping[X, O], X, R <: RowProduct, Y]
	         (component :ComponentSQL[L, M], value :SQLExpression[R, Single, Y])(implicit promote :X =~= Y)
			:ComponentSetter[L, R] { type Component[O] = M[O]; type Value = promote.Unified } =
		apply(promote.left(component), promote.right(value))

	def apply[L <: RowProduct, M[O] <: MappingAt[O], R <: RowProduct, V]
	         (component :LValueSQL[L, M, V], value :SQLExpression[R, Single, V])
			:ComponentSetter[L, R] { type Component[O] = M[O]; type Value = V } =
		new ComponentSetter[L, R] {
			override type Component[O] = M[O]
			override type Value = V
			override val lvalue = component
			override val rvalue = denullify(value)
		}

	def unapply[L <: RowProduct, R <: RowProduct](setter :ComponentSetter[L, R])
			:Opt[(LValueSQL[L, setter.Component, setter.Value], SQLExpression[R, Single, setter.Value])] =
		Got(setter.lvalue, setter.rvalue)

}






/** Representation of assignment of a value to a single column of a table as a pair
  * of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]. It is typically created by the expression
  * `lvalue `[[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.:= :=]]` rvalue`, where
  * `lvalue :`[[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL]]`[L, M]` and
  * `rvalue :`[[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[R, GlobalScope, X]`, with the mapping's `M`
  * subject type and `X` being either equal or equivalent in the terms of
  * [[net.noresttherein.oldsql.sql.mechanics.=~= =~=]].
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
  *           `RowProduct AndFrom `[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param`)
  *           for parameterized updates. Note that in the former case, the expression can still contain
  *           [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters.
  * @tparam V the shared type of the left side's mapping's [[net.noresttherein.oldsql.schema.Mapping.Subject subject]]
  *           and of the assigned expression on the right side, possibly after unification.
  * @author Marcin Mościcki
  */ //consider: if L is replaced with M in ComponentSetter, than it could be completely omitted here.
trait ColumnSetter[-L <: RowProduct, -R <: RowProduct] extends ComponentSetter[L, R] { self =>
	/** The value type of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  * on the [[net.noresttherein.oldsql.sql.ComponentSetter.lvalue left]] side, before any conversion.
	  */ //todo: look into extracting this to ComponentSetter
	type Subject

	override type Component[O] <: BaseColumn[Subject, O]

	override val lvalue :ColumnLValueSQL[L, Component, Value]
	override val rvalue :ColumnSQL[R, Single, Value]

	/** Type conversion of the component expression on the [[net.noresttherein.oldsql.sql.ColumnSetter.lvalue left]]
	  * side to a type [[net.noresttherein.oldsql.sql.mechanics.=~= unified]] with the right side.
	  */ //why isn't this up in ComponentSetter?
	implicit def promote :SQLConversion[Subject, Value] = lvalue.conversion

	override def anchor(table :L, params :R) :ColumnSetter[L, R] =
		ColumnSetter[L, Component, Subject, R, Value](lvalue.anchor(table), rvalue.anchor(params))

	override def split(operation :OperationView) :Seq[ColumnSetter[L, R]] = PassedArray :+ this

	override def reform(implicit spelling :SQLSpelling)
			:ColumnSetter[L, R] {
				type Subject = self.Subject; type Value = self.Value; type Component[O] = ColumnSetter.this.Component[O]
			} =
		this
}




object ColumnSetter {
	def apply[L <: RowProduct, M[O] <: BaseColumn[S, O], S, R <: RowProduct, Y, V]
	         (column :GenericColumnComponentSQL[L, M, S], value :ColumnSQL[R, Single, Y])
	         (implicit promote :(S =~= Y) { type Unified = V })
			:ColumnSetter[L, R] { type Component[O] = M[O]; type Subject = S; type Value = V } =
		apply[L, M, S, R, promote.Unified](promote.left(column), promote.right(value))

	def apply[L <: RowProduct, M[O] <: BaseColumn[S, O], S, R <: RowProduct, V]
	         (column :ColumnLValueSQL[L, M, V], value :ColumnSQL[R, Single, V])
			:ColumnSetter[L, R] { type Component[O] = M[O]; type Subject = S; type Value = V } =
		new ColumnSetter[L, R] {
			override type Value   = V
			override type Subject = S
			override type Component[O] = M[O]
			override val lvalue  = column
			override val rvalue  = value
		}

	def unapply[L <: RowProduct, R <: RowProduct, V](setter :ComponentSetter[L, R])
			:Opt[(ColumnLValueSQL[L, m, setter.Value], ColumnSQL[R, Single, setter.Value])
				forSome { type m[O] <: BaseColumn[_, O] }] =
		setter match {
			case set :(ColumnSetter[L, R] { type Value = setter.Value }) @unchecked => Got((set.lvalue, set.rvalue))
			case _ => Lack
		}


	/** Setters for all columns of `table.`[[net.noresttherein.oldsql.schema.Table.export export]] which are included
	  * by default in `operation`, with values obtained from `value` using the extracts for said columns from
	  * the table mapping subject `S`.
	  */
	def all[S, M[O] <: BaseMapping[S, O]](table :Table[M], value :S, operation :WriteOperationView)
			:Seq[ColumnSetter[From[M], RowProduct]] =
	{
		val domain = From(table)
		val mapping = table.export.asInstanceOf[TypedMapping[S, RowProduct AndFrom M]]
		val columns = operation.defaultColumns(mapping)
		columns.view.map { column =>
			def filter[T](column :TypedColumn[T, RowProduct AndFrom M]) :ColumnSetter[From[M], RowProduct] = {
				val extract = mapping(column) //this is somewhat fishy, as we create SQL for an export, not original
				val t = extract.opt(value)
				if (t.isDefined) domain.last \ column := BoundParam(t.get)(column.form)
				else domain.last \ column := SQLNull[T](column.form)
			}
			filter(column)
		}.to(ArraySeq)
	}

	/** Setters for all updatable by default columns of `table.`[[net.noresttherein.oldsql.schema.Table.export export]],
	  * with values obtained from `value` using the extracts for said columns from the table mapping subject `S`.
	  */
	def updates[S, M[O] <: BaseMapping[S, O]](table :Table[M], value :S) :Seq[ColumnSetter[From[M], RowProduct]] =
		ColumnSetter.all(table, value, UpdateView)

	/** Setters for all insertable by default columns of `table.`[[net.noresttherein.oldsql.schema.Table.export export]],
	  * with values obtained from `value` using the extracts for said columns from the table mapping subject `S`.
	  */
	def inserts[S, M[O] <: BaseMapping[S, O]](table :Table[M], value :S) :Seq[ColumnSetter[From[M], RowProduct]] =
		ColumnSetter.all(table, value, InsertView)

	def all[S, M[O] <: BaseMapping[S, O]](table :Table[M], operation :WriteOperationView)
			:Seq[ColumnSetter[From[M], WithParam.Last[S]]] =
		ColumnSetter.all(From(table).param[S](table.row.selectForm <> operation.form(table.row)), operation)

	def updates[S, M[O] <: BaseMapping[S, O]](domain :From[M] WithParam S)
			:Seq[ColumnSetter[From[M], WithParam.Last[S]]] =
		ColumnSetter.all(domain, UpdateView)

	def inserts[S, M[O] <: BaseMapping[S, O]](domain :From[M] WithParam S)
			:Seq[ColumnSetter[From[M], WithParam.Last[S]]] =
		ColumnSetter.all(domain, InsertView)

	def all[S, M[O] <: BaseMapping[S, O]](domain :From[M] WithParam S, operation :WriteOperationView)
			:Seq[ColumnSetter[From[M], WithParam.Last[S]]] =
	{
		val mapping = domain.left.table.export.asInstanceOf[TypedMapping[S, RowProduct AndFrom M]]
		val param = domain.last.mapping
		val columns = operation.defaultColumns(mapping)
		columns.view.map { column =>
			def filter[T](column :TypedColumn[T, RowProduct AndFrom M]) :ColumnSetter[From[M], WithParam.Last[S]] =
				domain.left.last \ column :=
					(domain.last \ (param.col(mapping(column))(column.form) :TypedColumn[T, WithParam.Last[S]])).toColumnSQL
			filter(column)
		}.to(ArraySeq)
	}
}






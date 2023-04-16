package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, InvalidSQLException, MisalignedExpressionException}
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bases.BaseColumn
import net.noresttherein.oldsql.slang.{classNameMethods, saferCasting}
import net.noresttherein.oldsql.sql.{ColumnSQL, CompoundSelect, MappingQuery, RowProduct, RowShape, SQLExpression, StandardSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ColumnReformingDefaults, ColumnShape, SpecificColumnVisitor, VariantColumnGroundingTemplate}
import net.noresttherein.oldsql.sql.CompoundSelect.{CompoundSelectTemplate, ReformedCompoundSelectTemplate}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.Select.SelectOperator
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.TopScope
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, Grouped, ReorderingTemplate, SQLShape, Single, SingleRowSQLTemplate, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnQuery.ColumnSingleQuery
import net.noresttherein.oldsql.sql.ast.CompoundSelectAs.{AnyCompoundSelectAsVisitor, CaseAnyCompoundSelectAs, CaseSpecificCompoundSelectAs, SpecificCompoundSelectAsVisitor}
import net.noresttherein.oldsql.sql.ast.CompoundSelectColumn.{AnyCompoundSelectColumnVisitor, SpecificCompoundSelectColumnVisitor}
import net.noresttherein.oldsql.sql.ast.CompoundSelectColumnAs.{AnyCompoundSelectColumnAsVisitor, SpecificCompoundSelectColumnAsVisitor}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{Rows, SingleQuerySQL}
import net.noresttherein.oldsql.sql.mechanics.{Alignment, AlignableColumns, QueryReform, Reform, ReformPermissions, SQLAdaptation, SQLConversion, SQLScribe, SQLTransformation, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.Reform.{ArityValidator, PassCount}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.Query.QueryReformingTemplate
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.ColumnMappingSQLShape
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.{BinaryCompositeColumn, BinaryCompositeColumnTemplate}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.{BinaryCompositeSQL, BinaryCompositeTemplate}
import net.noresttherein.oldsql.sql.ast.LValueSQL.LValueShape
import net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLShape
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.{MayReformLeft, MayReformRight}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions




/** Implements a compound select combining the result sets of two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). The kind of operation is defined by
  * the [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]]
  * of the [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.operator operator]] member property.
  * The row schemas of both arguments must match or an exception will be thrown when this expression
  * is converted into an executable SQL statement. If the schema of any of the member ''selects'' is flexible
  * (it is defined by a mapping with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect Optional]] columns),
  * the schema of the first member is used for both of the arguments.
  */
trait CompoundSelectSQL[-F <: RowProduct, R]
	extends BinaryCompositeSQL[F, Single, Rows[R], Rows[R], Rows[R]]
	   with QuerySQL[F, R]
	   with BinaryCompositeTemplate[F, Single, Rows[R], Rows[R], Rows[R],
	                                ({ type Q[-f <: RowProduct, -s >: Grouped <: Single] = CompoundSelectSQL[f, R] })#Q]
	   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = CompoundSelectSQL[f, R] })#Q]
	   with ReorderingTemplate[F, Single, Rows[R], CompoundSelectSQL[F, R]]
	   with CompoundSelectTemplate[R, ({ type Q[X] = QuerySQL[F, X] })#Q, CompoundSelectSQL[F, R]]
{
//consider: a Shape type for SQLExpression: column/component/tuple/named tuple. This would allow having some kind of a select clause

//	protected override def parts   :Seq[QuerySQL[F, R]] = left::right::Nil
	override def constituents      :Seq[SingleQuerySQL[F, R]] = left.constituents ++: right.constituents
//	override def rowForm           :SQLReadForm[R] = {
//		val l = left.rowForm
//		val r = right.rowForm
//		if (l comparable r)
//			if (l.isUniversal) l else if (r.isUniversal) r else selectClause.selectForm
//		else
//			selectClause.selectForm
//	}
	override def selectForm        :SQLReadForm[Rows[R]] = {
		val l = left.selectForm
		val r = right.selectForm
		if (l comparable r)
			if (l.isUniversal) l else if (r.isUniversal) r else Rows.readForm(selectClause.selectForm)
		else
			Rows.readForm(selectClause.selectForm)
	}
	override def universalForm     :Opt[SQLForm[Rows[R]]] = {
		roughlyValidate()
		left.universalForm orElse right.universalForm orElse selectClause.universalForm.map(Rows.form(_))
	}

//	override def selectClause :SQLShape[R] = //we don't use this.reformed in order not to cache the result for ArityValidator
//		QueryReform(ArityValidator)(this)(StandardSQL.spelling).selectClause
//
//	protected[this] override def construct[X](left :QuerySQL[F, X], operator :SelectOperator, right :QuerySQL[F, X])
//			:QuerySQL[F, X] =
//		CompoundSelectSQL(left, operator, right)

	override def adaptRows[X](conversion :SQLAdaptation[R, X]) :CompoundSelectSQL[F, X] =
		if (conversion.isIdentity) this.castFrom[CompoundSelectSQL[F, R], CompoundSelectSQL[F, X]]
		else operator(left.adaptRows(conversion), right.adaptRows(conversion))
//
//	override def map[X](f :R => X) :QuerySQL[F, X] = CompoundSelect(left.map(f), operator, right.map(f))

	override def transform[X](transformation :SQLTransformation[R, X]) :CompoundSelectSQL[F, X] =
		operator(left.transform(transformation), right.transform(transformation))

	override def isGround    :Boolean = left.isGround && right.isGround
	override def isSingleRow :Boolean = left.isSingleRow && right.isSingleRow
//	{
//		val leftColumns = left.alignableColumns(permissions)
//		val rightColumns = right.alignableColumns(permissions)
//		val compatible =
//			left.columns.size == right.columns.size && left.columns.corresponds(right.columns) {
//				(l, r) => l.form comparable r.form
//			}
//		if (!compatible)
//			throw new MisalignedExpressionException(
//				left.columns.mkString("Incompatible full (alignable) column lists for left (", ", ", ") and right(") +
//					right.columns.mkString("", ", ", ") subqueries of `" + this + "`")
//			)
//			AlignableColumns(this, leftColumns.permissions & rightColumns.permissions, leftColumns)
//	}
//	override isGround :Boolean = left.isGround && right.isGround
//	override def isGround :Boolean = left.isGround && right.isGround
//	override def isAnchored(from :F) :Boolean = left.isAnchored(from) && right.isAnchored(from)
//
//	override def anchor(from :F) :CompoundSelectSQL[F, R] = {
//		val (l, r) = (left.anchor(from), right.anchor(from))
//		if ((l eq left) && (r eq right)) this else CompoundSelectSQL(l, operator, r)
//	}
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :QuerySQL[E, R] =
//		(left.basedOn(base), right.basedOn(base)) match {
//			case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[QuerySQL[E, R]]
//			case (l, r) => operator(l, r)
//		}
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
//			:CompoundSelectSQL[E, R] =
//		(left.expand(base), right.expand(base)) match {
//			case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[QuerySQL[E, R]]
//			case (l, r) => operator(l, r)
//		}
//
//	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :CompoundSelectSQL[E, R] =
//		(mapper(left), mapper(right)) match {
//			case (l :QuerySQL[E @unchecked, R @unchecked], r :QuerySQL[E @unchecked, R @unchecked]) =>
//				CompoundSelectSQL(l, operator, r)
//			case (l, r) =>
//				throw new IllegalExpressionException(
//					s"Failed $mapper transformation of $this: ($l, $r) is not a valid QuerySQL pair."
//				)
//		}


//	protected override def reform[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                             (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[Rows[R], U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(SQLExpression[F, Single, U], SQLExpression[E, C, U]) =
//		if (passesAllowed > 1)
//			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
//		else {
//			val self = reform.left.apply(this)
//			implicit val compat = leftResult vs rightResult
//			val res = reform.prohibitReformLeft(self.left, other)._2
//			(leftResult(self), res)
//		}
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
//	                             (other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[Rows[R], U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(SQLExpression[F, Single, U], reform.LValue[E, C, U]) =
//		if (passesAllowed > 1)
//			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
//		else {
//			val self = reform.left.apply(this)
//			implicit val compat = leftResult vs rightResult
//			val res = reform.prohibitReformLeft(self.left, other)._2
//			(leftResult(self), res)
//		}


	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLExpression[E, C, Rows[R]], right :SQLExpression[E, C, Rows[R]])
			:CompoundSelectSQL[E, R] =
		(left, right) match {
			case _ if (left eq this.left) & (right eq this.right) => this.asInstanceOf[CompoundSelectSQL[E, R]]
			case (q1 :ColumnQuery[E, R], q2 :ColumnQuery[E, R]) => operator(q1, q2)
			case (q1 :QuerySQL[E, R], q2 :QuerySQL[E, R]) => operator(q1, q2)
			case (_ :QuerySQL[_, _], _) =>
				throw new IllegalExpressionException(
					"Cannot recreate a " + this.localClassName + " `" + this +
						"` because the right expression is not a QuerySQL: `" + right + ": " + right.className + "`."
				)
			case (_, _ :QuerySQL[_, _]) =>
				throw new IllegalExpressionException(
					"Cannot recreate a " + this.localClassName + " `" + this +
						"` because the left expression is not a QuerySQL: `" + left + "`: " + left.className + "."
				)
			case (_, _) =>
				throw new IllegalExpressionException(
					"Cannot recreate a " + this.localClassName + " `" + this +
						"` because left and right side are not QuerySQL expressions: `" + left + "`: " + left.className +
						", `" + right + "`: " + right.className + "`."
				)
		}


	protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :CompoundSelectSQL[F, R] = {
		val l = spelling.realign(left, reordering)
		val r = spelling.realign(right, reordering)
		if ((l eq left) & (r eq right))
			this
		else
			operator(l, r)
	}


	protected override def reform[E <: F](second :QuerySQL[E, R])(reform :QueryReform)
	                                     (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
	{
		//We could enforce no right excludes to induce additive reforms, but this would automatically
		// become !mayReformRight and prevent reforming against terms, which we certainly don't want.
		// unless terms get somehow their own permissions, leave everything to the reform

		val leftReform  = reform.prohibit(operator.allowedReforms + MayReformRight)
		val rightReform = reform.prohibit(operator.allowedReforms.asInRight + MayReformRight)
		val (leftWithSecond, secondWithLeft)   = (left `->reform` second)(leftReform)
		val (rightWithSecond, secondWithRight) = (right `->reform` secondWithLeft)(rightReform)
		val finalLeft = (leftWithSecond `->reform` secondWithRight)(leftReform.prohibitReformRight)._1
		val reformedThis =
			if ((finalLeft eq left) & (rightWithSecond eq right)) this
			else operator(finalLeft, rightWithSecond) //shouldn't we set reformedBy = leftReform here?
		(reformedThis, secondWithRight)
	}

	protected override def reform_:[E <: F](first :SingleQuerySQL[E, R])(reform :QueryReform)
	                                       (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
	{
		val leftReform  = reform.prohibit(operator.allowedReforms.asInLeft + MayReformLeft)
		val rightReform = reform.prohibit(operator.allowedReforms + MayReformLeft)
		val (firstWithLeft, leftWithFirst)   = ((first :QuerySQL[E, R]) `->reform` left)(leftReform)
		val (firstWithRight, rightWithFirst) = ((firstWithLeft :QuerySQL[E, R]) `->reform` right)(rightReform)
		//this is better than reformedThis = leftWithFirst.reform(rightWithFirst)(operator),
		// as it guarantees that, in case of sql terms, regardless of the direction in which the forms are passed
		// in all reform methods, they all share the same form as firstWithRight
		val finalLeft = (firstWithRight `->reform` leftWithFirst)(reform.prohibitReformLeft)._2
		val reformedThis =
			if ((finalLeft eq left) & (rightWithFirst eq right)) this
			else operator(finalLeft, rightWithFirst) //shouldn't we set reformedBy = rightReform here?
		(firstWithRight, reformedThis)
	}
	protected override def reform_:[E <: F](first :SelectSQL[E, R])(reform :QueryReform)
	                                       (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
		reform_:(first :SingleQuerySQL[E, R])(reform)

	protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                             (implicit leftResult  :SQLTransformation[Rows[R], U],
	                                       rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:(leftResult.Expression[F, Single, SQLExpression[F, Single, U]], rightResult.Expression[F2, S2, EC2[U]]) =
		if (this eq other)
			(leftResult(this), rightResult(other))
		else if (passCount.firstTime)
			passReform(other)(reform, passCount)(leftResult, rightResult, spelling)
		else {
			//todo: create a MockSelect class wrapping other and proceed using reform.queryReform
			//consider: if we had access to F2 here, we could just create a select other,
			// and use the algorithm which reforms all select clauses at the same time.
//			val self = reform.queryReform(this).apply(this)
			val self = reformed
			val right = reform.prohibitReformLeft.apply(self.left, other)._2
			(leftResult(self), right)
		}

	@throws[MisalignedExpressionException]("if select clauses of member selects cannot be unified to a common shape.")
	override def reformed(implicit spelling :SQLSpelling) :CompoundSelectSQL[F, R] =
		try {
			spelling.queryReform(this)
		} catch {
			case e :InvalidSQLException =>
				throw new MisalignedExpressionException(
					s"Irreconcilable shapes of member selects in $this: $left vs $right.", e
				)
		}
	protected override def reform(reform :QueryReform)(implicit spelling :SQLSpelling) :CompoundSelectSQL[F, R] =
			reform.default(this)

	protected override def alignment(permissions: Permissions)(implicit spelling: SQLSpelling): Alignment =
		spelling.alignment(left, permissions.asInLeft & operator.allowedReforms.asInLeft) :++
			spelling.alignment(right, permissions.asInRight & operator.allowedReforms.asInRight)

	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		spelling.potentialColumns(selectClause, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
		spelling.potentialColumnsCount(selectClause)

	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[Rows[R]] =
		Rows.form(spelling.inSelect.effectiveForm(selectClause))

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val withClause = if (spelling.scope == TopScope) this.withClause else localWithClause
		val withSQL =
			if (withClause.isEmpty) SpelledSQL(context)
			else spelling.inWith(withClause)(context, Parameterization.paramless) + " "
//		val CompoundSelectSQL(left, _, right) = spelling.queryReform(this)(this)
		val inLeft = spelling.inLeft(this)
		val l = left match {
			case CompoundSelectSQL(_, op, _) if op != operator || left.localWithClause.nonEmpty =>
				"(" +: (inLeft(left)(from, withSQL.context, params) + ")")
			case _ =>
				inLeft(left)(from, withSQL.context, params)
		}
		val inRight = spelling.inRight(this)
		val r = right match {
			case CompoundSelectSQL(_, op, _) //we check textual name as other implementations than Minus are possible
			if op != operator || operator.NAME == "MINUS" || right.localWithClause.nonEmpty =>
				"(" +: (inRight(right)(from, l.context, params) + ")")
			case _ =>
				inRight(right)(from, l.context, params)
		}
		val sql = withSQL.sql + l.sql + (" " + spelling(operator) + " ") + r.sql
		SpelledSQL(sql, withSQL.setter + l.setter + r.setter, r.context)
	}


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, Rows[R]] =
		visitor.compoundSelect(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, Rows[R], Y]) :Y =
		visitor.compoundSelect(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: CompoundSelectSQL[F, R] <: SQLExpression[F_, S_, Rows[R]],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[RowProduct, Y]) :Y[S_, Rows[R], E] =
//		visitor.compoundSelect(this)


	override def sameAs(that :CompositeSQL.__) :Boolean = that match {
		case op :CompoundSelectSQL[_, _] => op.operator == operator
		case _ => false
	}

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :CompoundSelectSQL[_, _] if canEqual(other) && other.canEqual(this) =>
			operator == other.operator && left == other.left && right == other.right
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.getClass == getClass
	override def hashCode :Int = (operator.hashCode * 31 + left.hashCode) * 31 + right.hashCode

//	override def typeString :ChunkedString = left.typeString

	override def toString :String = s"($left) $operator ($right)"
}




object CompoundSelectSQL {
	//todo: guarantee that shape == selectClause.shape
	def apply[F <: RowProduct, V](left :QuerySQL[F, V], operator :SelectOperator, right :QuerySQL[F, V])
			:CompoundSelectSQL[F, V] =
		(left, right) match {
			case (l :ColumnQuery[F, V], r :ColumnQuery[F, V]) =>
				CompoundSelectColumn(l, operator, r)
			case (l :MappingQuerySQL[F, MappingOf[V]#Projection] @unchecked,
			      r :MappingQuerySQL[F, MappingOf[V]#Projection] @unchecked) =>
				CompoundSelectAs[F, MappingOf[V]#Projection](l, operator, r)
			case _ =>
				new Impl(left, operator, right) {
					override lazy val selectClause = super.selectClause
				}
		}

/*
	def apply[F <: RowProduct, V]
	         (left :QuerySQL[F, V], operator :SelectOperator, right :QuerySQL[F, V], selectClause :SQLShape[V])
			:CompoundSelectSQL[F, V] =
		(left, right, selectClause) match {
			case (l :ColumnQuery[F, V], r :ColumnQuery[F, V], s :ColumnSQL[_, _, V]) =>
				CompoundSelectColumn(l, operator, r)
			case _ =>
				val res = new ReformedCompoundSelectSQL(left, operator, right, selectClause)
				CompoundSelect.validate(res)
				res

		}
*/

	//we don't know if they have SelectId - should we check all selects for them?
//	private[sql] def reformed[F <: RowProduct, V](left :QuerySQL[F, V], operator :SelectOperator, right :QuerySQL[F, V])
//	                                             (reform :Reform)(implicit spelling :SQLSpelling)
//			:CompoundSelectSQL[F, V] =
//		apply[F, V](left, operator, right, CompoundSelectIdSQL.selectClause(_))(reform)

	//consider: making them public, but adding validation (reform.validate(left, right))
	private[sql] def reformed[F <: RowProduct, R]
	                 (left :QuerySQL[F, R], operator :SelectOperator, right :QuerySQL[F, R], selectClause :SQLShape[R])
	                 (reform :QueryReform)(implicit spelling :SQLSpelling)
			:CompoundSelectSQL[F, R] =
		(left, right, selectClause) match {
			case (l :ColumnQuery[F, R], r :ColumnQuery[F, R], s :ColumnSQL[_, _, R]) =>
				CompoundSelectColumn.reformed(l, operator, r, s)(reform)
			case (l :MappingQuerySQL[F, MappingOf[R]#Projection] @unchecked,
			      r :MappingQuerySQL[F, MappingOf[R]#Projection] @unchecked,
			      s :MappingSQLShape[MappingOf[R]#Projection, R] @unchecked)
				if (l.mapping identical r.mapping) && (r.mapping identical s.mapping)
			=>
				CompoundSelectAs.reformed[F, MappingOf[R]#Projection](l, operator, r, s)(reform)
			case _ =>
				val res = new ReformedCompoundSelectSQL[F, R](left, operator, right, selectClause)
				res.reformedBy = (reform, spelling)
				res
		}

	private[sql] def reformed[F <: RowProduct, R](left :QuerySQL[F, R], operator :SelectOperator, right :QuerySQL[F, R])
	                                             (selectClause :CompoundSelectSQL[F, R] => SQLShape[R])
	                                             (reform :QueryReform)(implicit spelling :SQLSpelling)
			:CompoundSelectSQL[F, R] =
	{
		val init = selectClause
		val res =
			new Impl[F, R](left, operator, right)
				with ReformedCompoundSelectTemplate[R, ({ type Q[X] = QuerySQL[F, X] })#Q, CompoundSelectSQL[F, R]]
			{
				override lazy val selectClause = init(this)
			}
		res.reformedBy = (reform, spelling)
		res
	}
/*
	def reformed[F <: RowProduct, V](left :QuerySQL[F, V], operator :SelectOperator, right :QuerySQL[F, V],
	                                 rowForm :SQLReadForm[V]) :CompoundSelectSQL[F, V] =
	{
		def returnDefault = {
			CompoundSelect.validate(left, operator, right, rowForm)
			new BaseCompoundSelectSQL(left, operator, right, rowForm)
		}
		(left, right) match {
			case (l :ColumnQuery[F, V], r :ColumnQuery[F, V]) => rowForm match {
				case column :ColumnReadForm[V] => CompoundSelectColumn.reformed(l, operator, r, column)
				case _ if rowForm.columnCount == 1 => try {
					val sqlType = rowForm.columnTypes.head
					val columnForm = ColumnReadForm.opt(sqlType)(rowForm.opt)(rowForm.nulls)
					CompoundSelectColumn.reformed(l, operator, r, columnForm)
				} catch {
					case _ :UnsupportedOperationException => returnDefault
				}
				case _ => returnDefault
			}
			case _ => returnDefault
		}
	}
*/

	@inline def unapply[F <: RowProduct, R](e :SQLExpression[F, Grouped, Rows[R]])
			:Opt[(QuerySQL[F, R], SelectOperator, QuerySQL[F, R])] =
		e match {
			case op :CompoundSelectSQL[F @unchecked, R @unchecked] => Got((op.left, op.operator, op.right))
			case _ => Lack
		}

	private class Impl[-F <: RowProduct, R](override val left  :QuerySQL[F, R], override val operator :SelectOperator,
	                                        override val right :QuerySQL[F, R])
		extends CompoundSelectSQL[F, R] with RowShapeCache
	{
		override type RowMapping[O] = left.RowMapping[O]
		override def mapping[O] = left.mapping[O]
		override def export[O]  = left.export[O] //todo: this should involve some reconciliation
		override def selectForm = Rows.readForm(rowForm)

		override lazy val constituents = super.constituents
	}

	private class ReformedCompoundSelectSQL[-F <: RowProduct, R]
	              (l :QuerySQL[F, R], op :SelectOperator, r :QuerySQL[F, R], override val selectClause :SQLShape[R])
		extends Impl[F, R](l, op, r)
		   with ReformedCompoundSelectTemplate[R, ({ type Q[X] = QuerySQL[F, X] })#Q, CompoundSelectSQL[F, R]]
	{
		override lazy val selectForm    = Rows.readForm(selectClause.selectForm)
		override lazy val universalForm = selectClause.universalForm.map(Rows.form(_))
	}



	trait SpecificCompoundSelectVisitor[+F <: RowProduct, X, +Y]
		extends SpecificCompoundSelectColumnVisitor[F, X, Y] with SpecificCompoundSelectAsVisitor[F, X, Y]
	{
		def compoundSelect[R](e :CompoundSelectSQL[F, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	trait MatchSpecificCompoundSelect[+F <: RowProduct, X, +Y]
		extends SpecificCompoundSelectVisitor[F, X, Y] with CaseSpecificCompoundSelectAs[F, X, Y]
	{
		override def compoundSelectColumn[R](e :CompoundSelectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y =
			compoundSelect(e)
	}
	trait CaseSpecificCompoundSelect[+F <: RowProduct, X, +Y] extends MatchSpecificCompoundSelect[F, X, Y] {
		override def compoundSelectAs[M[O] <: MappingAt[O]]
		                             (e :CompoundSelectAs[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y =
			compoundSelect(e)
	}
//
//
//	trait CompoundSelectVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompoundSelectColumnVisitor[F, Y] with CompoundSelectAsVisitor[F, Y]
//	{
//		def compoundSelect[R](e :CompoundSelectSQL[F, R]) :Y[Single, Rows[R], CompoundSelectSQL[F, R]]
//	}
//	trait MatchCompoundSelect[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompoundSelectVisitor[F, Y] with CaseCompoundSelectAs[F, Y]
//	{
//		override def compoundSelectColumn[R](e :CompoundSelectColumn[F, R])
//				:Y[Single, Rows[R], CompoundSelectColumn[F, R]] =
//			compoundSelect(e)
//	}
//	trait CaseCompoundSelect[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchCompoundSelect[F, Y]
//	{
//		override def compoundSelectAs[M[O] <: MappingAt[O]](e :CompoundSelectAs[F, M])
//				:Y[Single, Rows[M[Unit]#Subject], CompoundSelectAs[F, M]] =
//			compoundSelect(e)
//	}


	trait AnyCompoundSelectVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompoundSelectAsVisitor[F, Y] with AnyCompoundSelectColumnVisitor[F, Y]
	{
		def compoundSelect[V](e :CompoundSelectSQL[F, V]) :Y[Single, Rows[V]]
	}
	trait MatchAnyCompoundSelect[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompoundSelectVisitor[F, Y] with CaseAnyCompoundSelectAs[F, Y]
	{
		override def compoundSelectColumn[V](e :CompoundSelectColumn[F, V]) :Y[Single, Rows[V]] =
			compoundSelect(e :CompoundSelectSQL[F, V])
	}
	trait CaseAnyCompoundSelect[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyCompoundSelect[F, Y]
	{
		override def compoundSelectAs[M[O] <: MappingAt[O]]
		                             (e :CompoundSelectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject]] =
			compoundSelect(e)
	}

}






/** Implements a compound select combining the result sets of two single-column
  * SQL [[net.noresttherein.oldsql.sql.ast.SelectColumn selects]]
  * (or other [[net.noresttherein.oldsql.sql.ast.ColumnQuery queries]]). The kind of operation is defined by
  * the [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]]
  * of the [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.operator operator]] member property.
  */
trait CompoundSelectColumn[-F <: RowProduct, R]
	extends CompositeColumnSQL[F, Single, Rows[R]]
	   with CompoundSelectSQL[F, R]
	   with ColumnQuery[F, R]
	   with VariantColumnGroundingTemplate[F, Single, Rows[R], ({ type E[-f <: RowProduct] = CompoundSelectColumn[f, R] })#E]
	   with BinaryCompositeColumnTemplate[F, Single, Rows[R], Rows[R], Rows[R],
	                                      ({ type Q[-f <: RowProduct, -_ >: Grouped <: Single] = CompoundSelectColumn[f, R] })#Q]
	   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = CompoundSelectColumn[f, R] })#Q]
	   with CompoundSelectTemplate[R, ({ type Q[X] = ColumnQuery[F, X] })#Q, CompoundSelectColumn[F, R]]
	   with ColumnReformingDefaults[F, Single, Rows[R], ({ type Q[v] = ColumnSQL[F, Single, v] })#Q]
{
	override val left              :ColumnQuery[F, R]
	override val right             :ColumnQuery[F, R]
	override def constituents      :Seq[ColumnSingleQuery[F, R]] = left.constituents ++: right.constituents
	override def rowForm           :ColumnReadForm[R]        = {
		val l = left.rowForm
		val r = right.rowForm
		if (l comparable r)
			if (l.isUniversal) l else if (r.isUniversal) r else selectClause.selectForm
		else
			selectClause.selectForm
	}
	override def selectForm        :ColumnReadForm[Rows[R]]  = {
		val l = left.selectForm
		val r = right.selectForm
		if (l comparable r)
			if (l.isUniversal) l else if (r.isUniversal) r else Rows.columnReadForm(selectClause.selectForm)
		else
			Rows.columnReadForm(selectClause.selectForm)
	}
	override def universalForm     :Opt[ColumnForm[Rows[R]]] =
		left.universalForm orElse right.universalForm orElse selectClause.universalForm.map(Rows.columnForm(_))

	override def selectClause :ColumnShape[R] = {
		roughlyValidate()
		left.selectClause
	}



	override def adaptRows[X](conversion :SQLAdaptation[R, X]) :CompoundSelectColumn[F, X] =
		if (conversion.isIdentity) this.asInstanceOf[CompoundSelectColumn[F, X]]
		else operator(left.adaptRows(conversion), right.adaptRows(conversion))
//
//	override def map[X](f :R => X) :ColumnQuery[F, X] = CompoundSelectColumn(left.map(f), operator, right.map(f))
//
//	override def anchor(from :F) :CompoundSelectColumn[F, R] = {
//		val l = left.anchor(from); val r = right.anchor(from)
//		if ((l eq left) && (r eq right)) this else CompoundSelectColumn(l, operator, r)
//	}
////
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :ColumnQuery[E, R] =
//		(left.basedOn(base), right.basedOn(base)) match {
//			case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[ColumnQuery[E, R]]
//			case (l, r) => operator(l, r)
//		}
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
//			:CompoundSelectColumn[E, R] =
//		(left.expand(base), right.expand(base)) match {
//			case (l, r) if (l eq left) && (r eq right) =>
//				this.castFrom[CompoundSelectColumn[F, R], CompoundSelectColumn[E, R]]
//			case (l, r) => operator(l, r)
//		}
//
//	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :CompoundSelectColumn[E, R] =
//		(mapper(left), mapper(right)) match {
//			case (l :ColumnQuery[E @unchecked, R @unchecked], r :ColumnQuery[E @unchecked, R @unchecked]) =>
//				CompoundSelectColumn(l, operator, r)
//			case (l, r) =>
//				throw new IllegalExpressionException(
//					s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
//				)
//		}

//	override def defaultReform :QueryReform = Reform.RowShapeValidator
//	override def topReform     :QueryReform = Reform.RowShapeValidator //don't add SelectId column

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :ColumnSQL[E, C, Rows[R]], right :ColumnSQL[E, C, Rows[R]])
			:CompoundSelectColumn[E, R] =
		(left, right) match {
			case _ if (left eq this.left) & (right eq this.right) =>
				this.castFrom[CompoundSelectColumn[F, R], CompoundSelectColumn[E, R]]
			case (q1 :ColumnQuery[E, R], q2 :ColumnQuery[E, R]) => operator(q1, q2)
			case (_ :ColumnQuery[_, _], _) =>
				throw new IllegalExpressionException(
					"Cannot recreate a CompoundSelectColumn `" + this +
						"` because the right expression is not a ColumnQuery: `" + right + ": " + right.className + "`."
				)
			case (_, _ :ColumnQuery[_, _]) =>
				throw new IllegalExpressionException(
					"Cannot recreate a CompoundSelectColumn `" + this +
						"` because the left expression is not a ColumnQuery: `" + left + "`: " + left.className + "."
				)
			case (_, _) =>
				throw new IllegalExpressionException(
					"Cannot recreate a CompoundSelectColumn `" + this +
						"` because left and right side are not ColumnQuery expressions: `" + left + "`: " + left.className +
						", `" + right + "`: " + right.className + "`."
				)
		}


	protected override def realign(reordering :Rearrangement)
	                              (implicit spelling :SQLSpelling) :CompoundSelectColumn[F, R] =
	{
		val l = spelling.realign(left, reordering)
		val r = spelling.realign(right, reordering)
		if ((l eq left) & (r eq right))
			this
		else
			operator(l, r)
	}
//
//	protected[sql] override def `->reform`(reordering :Rearrangement)(spelling :SQLSpelling) :CompoundSelectColumn[F, R] =
//		reform(reordering)(spelling)


	protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                             (implicit leftResult  :SQLTransformation[Rows[R], U],
	                                       rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:(leftResult.Expression[F, Single, ColumnSQL[F, Single, U]], rightResult.Expression[F2, S2, EC2[U]]) =
		if (this eq other)
			(leftResult(this), rightResult(other))
		else if (passCount.firstTime)
			passReform[F2, S2, V2, EC2, U](other)(reform, passCount)
		else
			other match {
				case column :ColumnSQL[F2, S2, V2] if reform.compatible(this, column) =>
					(leftResult(this), rightResult(other))
				case _ =>
					//consider: if we had access to F2 here, we could just create a select other,
					// and use the algorithm which reforms all select clauses at the same time.
		//			val self = reform.queryReform(this).apply(this)
					val self = reformed
					val right = reform.prohibitReformLeft.apply(self.left, other)._2
					(leftResult(self), right)
		}

	@throws[MisalignedExpressionException]("if select clauses of member selects cannot be unified to a common shape.")
	override def reformed(implicit spelling :SQLSpelling) :CompoundSelectColumn[F, R] =
		try {
			spelling.queryReform(this)
		} catch {
			case e :InvalidSQLException =>
				throw new MisalignedExpressionException(
					s"Irreconcilable shapes of member selects in $this: $left vs $right.", e
				)
		}
	protected override def reform(reform :QueryReform)(implicit spelling :SQLSpelling) :CompoundSelectColumn[F, R] =
		reform.default(this)

	protected override def effectiveForm(implicit spelling :SQLSpelling) :ColumnForm[Rows[R]] =
		Rows.columnForm(spelling.inSelect.effectiveForm(selectClause))

	//don't use the reform which assigns a SelectId, as we must reform to a ColumnQuery.
	override def defaultReform(implicit spelling :SQLSpelling) :QueryReform =
		operator.defaultReform

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[Single, Rows[R]] =
		visitor.compoundSelectColumn(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, Single, Rows[R], Y]) :Y =
		visitor.compoundSelectColumn(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//	                             E >: CompoundSelectColumn[F_, R] <: SQLExpression[F_, S_, Rows[R]],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, Y]) :Y[S_, Rows[R], E] =
//		visitor.compoundSelectColumn(this)

//	override def typeString :ChunkedString = left.typeString
}




object CompoundSelectColumn {
	def apply[F <: RowProduct, V](left :ColumnQuery[F, V], operator :SelectOperator, right :ColumnQuery[F, V])
			:CompoundSelectColumn[F, V] =
		(left, right) match {
			case (l :ColumnMappingQuery[F, MappingOf[V]#TypedColumnProjection, V] @unchecked,
			      r :ColumnMappingQuery[F, MappingOf[V]#TypedColumnProjection, V] @unchecked)
			=>
				CompoundSelectColumnAs(l, operator, r)
			case _ =>
				new Impl(left, operator, right) {
					override lazy val selectClause = super.selectClause
				}
		}

/*
	def apply[F <: RowProduct, V]
	         (left :ColumnQuery[F, V], operator :SelectOperator, right :ColumnQuery[F, V], selectClause :SQLShape[V])
			:CompoundSelectColumn[F, V] =
		selectClause match {
			case _ if !(left.selectForm.shape <:> right.selectForm.shape)
			       || !(left.selectForm.shape <:> selectClause.selectForm.shape)
			       || !(right.selectForm.shape <:> selectClause.selectForm.shape) =>
				throw new IllegalArgumentException(
					"Cannot create a CompoundSelectColumn(" + left + ", " + operator + ", " + right + ") with selectClause "
						+ selectClause + " because the shapes are incompatible: "
						+ left.selectForm.shape + ", " + right.selectForm.shape + ", " + selectClause.selectForm.shape + "."
				)
			case column :ColumnSQL[_, _, V] =>
				new ReformedCompoundSelectColumn(left, operator, right, column)
//			case column if column.selectForm.columnCount == 1 =>
//				new StandardCompoundSelectColumn(left, operator, right, selectClause)
			case _ =>
				throw new IllegalArgumentException(
					"Cannot create a CompoundSelectColumn(" + left + ", " + operator + ", " + right + ") with selectClause "
					+ selectClause + " because its selectForm has more than one column: " + selectClause.selectForm + "."
				)
		}
*/

	private[sql] def reformed[F <: RowProduct, V]
	                         (left :ColumnQuery[F, V], operator :SelectOperator, right :ColumnQuery[F, V],
	                          selectClause :ColumnSQL[Nothing, Grouped, V])
	                         (reform :QueryReform)(implicit spelling :SQLSpelling)
			:CompoundSelectColumn[F, V] =
		(left, right, selectClause) match {
			case (l :ColumnMappingQuery[F, MappingOf[V]#TypedColumnProjection, V] @unchecked,
				  r :ColumnMappingQuery[F, MappingOf[V]#TypedColumnProjection, V] @unchecked,
				  s :ColumnMappingSQL[Nothing, Grouped, MappingOf[V]#TypedColumnProjection, V] @unchecked)
				if (l.mapping identical r.mapping) && (r.mapping identical s.mapping)
			=>
				CompoundSelectColumnAs.reformed(l, operator, r, s)(reform)
			case _ =>
				val res = new ReformedCompoundSelectColumn(left, operator, right, selectClause)
				res.reformedBy = (reform, spelling)
				res
		}


	@inline def unapply[F <: RowProduct, V](e :SQLExpression[F, Grouped, Rows[V]])
			:Opt[(ColumnQuery[F, V], SelectOperator, ColumnQuery[F, V])] =
		e match {
			case op :CompoundSelectColumn[F @unchecked, V @unchecked] => Got((op.left, op.operator, op.right))
			case _ => Lack
		}


	private class Impl[-F <: RowProduct, V](override val left :ColumnQuery[F, V], override val operator :SelectOperator,
	                                        override val right :ColumnQuery[F, V])
		extends CompoundSelectColumn[F, V] with RowShapeCache
	{
		override type RowMapping[O] = TypedColumn[V, O]
		override def mapping[O]     = left.mapping
		override def export[O]      = left.export //todo: this should involve some reconciliation
//		override def selectForm     = Rows.columnReadForm(rowForm)
//		override def universalForm  = selectClause.universalForm.map(Rows.columnForm(_))
		override lazy val selectForm = super.selectForm

		override lazy val constituents = super.constituents
	}

	private class ReformedCompoundSelectColumn[-F <: RowProduct, R]
	                                          (l :ColumnQuery[F, R], op :SelectOperator, r :ColumnQuery[F, R],
	                                           override val selectClause :ColumnSQL[Nothing, Grouped, R])
		extends Impl(l, op, r)
		   with ReformedCompoundSelectTemplate[R, ({ type Q[X] = ColumnQuery[F, X] })#Q, CompoundSelectColumn[F, R]]
	{
		override lazy val rowForm       = selectClause.selectForm
		override lazy val selectForm    = Rows.columnReadForm(selectClause.selectForm)
		override lazy val universalForm = selectClause.universalForm.map(Rows.columnForm(_))
	}



	trait SpecificCompoundSelectColumnVisitor[+F <: RowProduct, X, +Y]
		extends SpecificCompoundSelectColumnAsVisitor[F, X, Y]
	{
		def compoundSelectColumn[R](e :CompoundSelectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	type MatchSpecificCompoundSelectColumn[+F <: RowProduct, X, +Y] = SpecificCompoundSelectColumnVisitor[F, X, Y]

	trait CaseSpecificCompoundSelectColumn[+F <: RowProduct, X, +Y] extends MatchSpecificCompoundSelectColumn[F, X, Y] {
		override def compoundSelectColumnAs[M[O] <: BaseColumn[R, O], R]
		                                   (e :CompoundSelectColumnAs[F, M, R])(implicit isRows :X =:= Rows[R]) :Y =
			compoundSelectColumn(e)
	}
//
//
//	trait CompoundSelectColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompoundSelectColumnAsVisitor[F, Y]
//	{
//		def compoundSelectColumn[R](e :CompoundSelectColumn[F, R]) :Y[Single, Rows[R], CompoundSelectColumn[F, R]]
//	}
//	type MatchCompoundSelectColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		CompoundSelectColumnVisitor[F, Y]
//
//	trait CaseCompoundSelectColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchCompoundSelectColumn[F, Y]
//	{
//		override def compoundSelectColumnAs[M[O] <: BaseColumn[S, O], S](e :CompoundSelectColumnAs[F, M, S])
//				:Y[Single, Rows[S], CompoundSelectColumnAs[F, M, S]] =
//			compoundSelectColumn(e)
//	}


	trait AnyCompoundSelectColumnVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompoundSelectColumnAsVisitor[F, Y]
	{
		def compoundSelectColumn[V](e :CompoundSelectColumn[F, V]) :Y[Single, Rows[V]]
	}
	type MatchAnyCompoundSelectColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyCompoundSelectColumnVisitor[F, Y]

	trait CaseAnyCompoundSelectColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyCompoundSelectColumn[F, Y]
	{
		override def compoundSelectColumnAs[M[O] <: BaseColumn[V, O], V]
	                                       (e :CompoundSelectColumnAs[F, M, V]) :Y[Single, Rows[V]] =
			compoundSelectColumn(e)
	}
}






/** Implements a compound select combining the result sets of two
  * SQL [[net.noresttherein.oldsql.sql.ast.SelectAs selects]]
  * (or other [[net.noresttherein.oldsql.sql.ast.MappingQuerySQL queries]]), sharing the same row schema,
  * as defined by the mapping `M`. The kind of operation is defined by
  * the [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]]
  * of the [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.operator operator]] member property.
  */
//todo: extend ReorderingTemplate[F, Single, Rows[M[Unit]#Subject, CompoundSelectAs[F, Single, Rows[M[Unit]#Subject]]
// with CompoundSelectTemplate[R, ({ type Q[X] = CompoundSelectSQL[F, X] })#Q, CompoundSelectAs[F, M]]
trait CompoundSelectAs[-F <: RowProduct, M[O] <: MappingAt[O]] //to be before SingleRowSQLTemplate in linearisation
	extends BinaryCompositeSQL[F, Single, Rows[M[Unit]#Subject], Rows[M[Unit]#Subject], Rows[M[Unit]#Subject]]
	   with CompoundSelectSQL[F, M[Unit]#Subject]
	   with MappingQuerySQL[F, M]
//	   with BinaryCompositeTemplate[F, Single, Rows[M[Unit]#Subject], Rows[M[Unit]#Subject], Rows[M[Unit]#Subject],
//	                                ({ type Q[-f <: RowProduct, -_ >: Grouped <: Single] = CompoundSelectAs[f, M] })#Q]
//	   with SpecificBinaryCompositeSQL[F, Single,
//	                                   Rows[M[Unit]#Subject], ({ type Q[-f <: RowProduct] = MappingQuerySQL[f, M] })#Q,
//	                                   Rows[M[Unit]#Subject], ({ type Q[-f <: RowProduct] = MappingQuerySQL[f, M] })#Q,
//	                                   Rows[M[Unit]#Subject], ({ type Q[-f <: RowProduct] = CompoundSelectAs[f, M] })#Q]
	   with SingleRowSQLTemplate[F, Rows[M[Unit]#Subject], ({ type Q[-f <: RowProduct] = CompoundSelectAs[f, M] })#Q]
{
	override val left         :MappingQuerySQL[F, M]
	override val right        :MappingQuerySQL[F, M]
//	override def constituents :Seq[SelectAs[F, M]] = left.constituents ++: right.constituents

	override def mapping[O] = left.mapping
	override def export[O]  = left.export //todo: this should involve some reconciliation

	override def anchor(from :F) :CompoundSelectAs[F, M] =
		if (isAnchored(from))
			this
		else {
			val l = left.anchor(from)
			val r = right.anchor(from)
			if ((l eq left) && (r eq right)) this
			else operator(l, r)
		}
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :MappingQuerySQL[E, M] =
//		(left.basedOn(base), right.basedOn(base)) match {
//			case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[MappingQuerySQL[E, M]]
//			case (l, r) => operator(l, r)
//		}
//
	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, isSingle :Single <:< Single) :CompoundSelectAs[E, M] =
		(left.expand(base), right.expand(base)) match {
			case (l, r) if (l eq left) && (r eq right) => this.castFrom[CompoundSelectAs[F, M], CompoundSelectAs[E, M]]
			case (l, r) => operator(l, r)
		}

//	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :CompoundSelectAs[E, M] =
//		(mapper(left), mapper(right)) match {
//			case (l :MappingQuerySQL[E @unchecked, M @unchecked], r :MappingQuerySQL[E @unchecked, M @unchecked]) =>
//				CompoundSelectAs(l, operator, r)
//			case (l, r) =>
//				throw new IllegalExpressionException(
//					s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
//				)
//		}

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, Rows[M[Unit]#Subject]] =
		visitor.compoundSelectAs(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, Rows[M[Unit]#Subject], Y]) :Y =
		visitor.compoundSelectAs(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: CompoundSelectAs[F, M] <: SQLExpression[F_, S_, Rows[M[Unit]#Subject]],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, Rows[M[Unit]#Subject], E] =
//		visitor.compoundSelectAs(this)
}




object CompoundSelectAs {
	def apply[F <: RowProduct, M[O] <: MappingAt[O]]
	         (left :MappingQuerySQL[F, M], operator :SelectOperator, right :MappingQuerySQL[F, M])
			:CompoundSelectAs[F, M] =
		(left, right) match {
			case (l :ColumnMappingQuery[F, MappingOf[M[Unit]#Subject]#TypedColumnProjection, M[Unit]#Subject] @unchecked,
				  r :ColumnMappingQuery[F, MappingOf[M[Unit]#Subject]#TypedColumnProjection, M[Unit]#Subject] @unchecked) =>
//				CompoundSelectColumnAs(l, operator, r).asInstanceOf[CompoundSelectAs[F, M]]
				CompoundSelectColumnAs(l, operator, r).castFrom[
					CompoundSelectColumnAs[F, MappingOf[M[Unit]#Subject]#TypedColumnProjection, M[Unit]#Subject],
					CompoundSelectAs[F, M]
				]
			case _ =>
				new Impl(left, operator, right)
		}

	private[sql] def reformed[F <: RowProduct, M[O] <: MappingAt[O]]
	                         (left :MappingQuerySQL[F, M], operator :SelectOperator, right :MappingQuerySQL[F, M],
	                          selectClause :MappingSQLShape[M, M[Unit]#Subject])
		                     (reform :QueryReform)(implicit spelling :SQLSpelling) :CompoundSelectAs[F, M] =
		(left, right, selectClause :SQLShape[M[Unit]#Subject]) match {
			case (l :ColumnMappingQuery[F, MappingOf[M[Unit]#Subject]#TypedColumnProjection, M[Unit]#Subject] @unchecked,
				  r :ColumnMappingQuery[F, MappingOf[M[Unit]#Subject]#TypedColumnProjection, M[Unit]#Subject] @unchecked,
				  c :ColumnMappingSQL[Nothing, Grouped, MappingOf[M[Unit]#Subject]#TypedColumnProjection, M[Unit]#Subject] @unchecked)
			=>
				CompoundSelectColumnAs.reformed(l, operator, r, c)(reform).castFrom[
					CompoundSelectColumnAs[F, MappingOf[M[Unit]#Subject]#TypedColumnProjection, M[Unit]#Subject],
					CompoundSelectAs[F, M]
				]
			case _ =>
				val res = new ReformedCompoundSelectAs(left, operator, right, selectClause)
				res.reformedBy = (reform, spelling)
				res
		}
/*

	private[sql] def reformed[F <: RowProduct, M[O] <: MappingAt[O]]
	                         (left :MappingQuerySQL[F, M], operator :SelectOperator, right :MappingQuerySQL[F, M],
	                          rowForm :SQLReadForm[M[Unit]#Subject]) :CompoundSelectAs[F, M] =
	{
		def returnDefault = {
			CompoundSelect.validate(left, operator, right, rowForm)
			new Impl(left, operator, right, rowForm)
		}
		(left, right) match {
			case (l :ColumnMappingQuery[F, MappingOf[M[Unit]#Subject]#TypedColumnProjection, M[Unit]#Subject] @unchecked,
			      r :ColumnMappingQuery[F, MappingOf[M[Unit]#Subject]#TypedColumnProjection, M[Unit]#Subject] @unchecked) =>
				rowForm match {
					case column :ColumnReadForm[M[Unit]#Subject] =>
						CompoundSelectColumnAs.reformed(l, operator, r, column).asInstanceOf[CompoundSelectAs[F, M]]
					case _ if rowForm.columnCount == 1 => try {
						val sqlType = rowForm.columnTypes.head
						val form = ColumnReadForm.opt(sqlType)(rowForm.opt)(rowForm.nulls)
						CompoundSelectColumnAs.reformed(l, operator, r, form).asInstanceOf[CompoundSelectAs[F, M]]
					} catch {
						case _ :UnsupportedOperationException => returnDefault
					}
					case _ => returnDefault
				}
			case _ => returnDefault
		}
	}
*/

	@inline def unapply[F <: RowProduct, V](e :SQLExpression[F, Grouped, Rows[V]])
			:Opt[(MappingQuerySQL[F, M], SelectOperator, MappingQuerySQL[F, M]) forSome { type M[O] <: MappingAt[O] }] =
		e match {
			case op :CompoundSelectAs[F @unchecked, MappingAt @unchecked] =>
				Got((op.left, op.operator, op.right))
			case _ => Lack
		}


	private class Impl[-F <: RowProduct, M[O] <: MappingAt[O]]
	                  (override val left :MappingQuerySQL[F, M], override val operator :SelectOperator,
	                   override val right :MappingQuerySQL[F, M])
		extends CompoundSelectAs[F, M] with RowShapeCache
	{
		override lazy val selectForm = super.selectForm
	}

	private class ReformedCompoundSelectAs[-F <: RowProduct, M[O] <: MappingAt[O]]
	              (l :MappingQuerySQL[F, M], op :SelectOperator, r :MappingQuerySQL[F, M],
	               override val selectClause :MappingSQLShape[M, M[Unit]#Subject])
		extends Impl[F, M](l, op, r) //todo:
			with ReformedCompoundSelectTemplate[M[Unit]#Subject, ({type Q[X] = QuerySQL[F, X]})#Q, CompoundSelectSQL[F, M[Unit]#Subject]] //CompoundSelectAs[F, M]]
	{
		override lazy val selectForm    = Rows.readForm(selectClause.selectForm)
		override lazy val universalForm = selectClause.universalForm.map(Rows.form(_))
	}


	trait SpecificCompoundSelectAsVisitor[+F <: RowProduct, X, +Y] extends SpecificCompoundSelectColumnAsVisitor[F, X, Y] {
		def compoundSelectAs[M[O] <: MappingAt[O]]
		                    (e :CompoundSelectAs[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y
	}
	trait MatchSpecificCompoundSelectAs[+F <: RowProduct, X, +Y] extends SpecificCompoundSelectAsVisitor[F, X, Y] {
		override def compoundSelectColumnAs[M[O] <: BaseColumn[R, O], R]
		                                   (e :CompoundSelectColumnAs[F, M, R])(implicit isRows :X =:= Rows[R]) :Y =
			{ val res = compoundSelectAs(e); res }
	}
	type CaseSpecificCompoundSelectAs[+F <: RowProduct, X, +Y] = MatchSpecificCompoundSelectAs[F, X, Y]
//
//
//	trait CompoundSelectAsVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompoundSelectColumnAsVisitor[F, Y]
//	{
//		def compoundSelectAs[M[O] <: MappingAt[O]]
//		                    (e :CompoundSelectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject], CompoundSelectAs[F, M]]
//	}
//	trait MatchCompoundSelectAs[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompoundSelectAsVisitor[F, Y] with CaseCompoundSelectColumnAs[F, Y]
//	{
//		override def compoundSelectColumnAs[M[O] <: BaseColumn[S, O], S](e :CompoundSelectColumnAs[F, M, S])
//				:Y[Single, Rows[S], CompoundSelectColumnAs[F, M, S]] =
//			compoundSelectAs(e)
//	}
//	type CaseCompoundSelectAs[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		MatchCompoundSelectAs[F, Y]


	trait AnyCompoundSelectAsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompoundSelectColumnAsVisitor[F, Y]
	{
		def compoundSelectAs[M[O] <: MappingAt[O]]
		                    (e :CompoundSelectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject]]
	}
	trait MatchAnyCompoundSelectAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompoundSelectAsVisitor[F, Y]
	{
		override def compoundSelectColumnAs[M[O] <: BaseColumn[V, O], V]
		                                   (e :CompoundSelectColumnAs[F, M, V]) :Y[Single, Rows[V]] =
			{ val res = compoundSelectAs(e); res  }
	}
	type CaseAnyCompoundSelectAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		MatchAnyCompoundSelectAs[F, Y]

}






trait CompoundSelectColumnAs[-F <: RowProduct, M[O] <: BaseColumn[R, O], R]
	extends BinaryCompositeSQL[F, Single, Rows[R], Rows[R], Rows[R]] //pull these two before SingleRowSQLTemplate in linearization
	   with BinaryCompositeColumn[F, Single, Rows[R], Rows[R]]
	   with CompoundSelectColumn[F, R]
	   with ColumnMappingQuery[F, M, R]
	   with CompoundSelectAs[F, M]
//	   with BinaryCompositeColumnTemplate[F, Single, Rows[R], Rows[R], Rows[R],
//	                                      ({ type Q[-f <: RowProduct, -_ >: Grouped <: Single] = CompoundSelectColumnAs[f, M, R] })#Q]
	   with SingleRowSQLTemplate[F, Rows[M[Unit]#Subject], ({ type Q[-f <: RowProduct] = CompoundSelectColumnAs[f, M, R] })#Q]
{
	override val left  :ColumnMappingQuery[F, M, R]
	override val right :ColumnMappingQuery[F, M, R]

	override def export[O] :TypedColumn[R, O] = left.export
//	override def constituents :Seq[SelectColumnAs[F, M, V]] = left.constituents ++: right.constituents

	override def anchor(from :F) :CompoundSelectColumnAs[F, M, R] = {
		val l = left.anchor(from)
		val r = right.anchor(from)
		if ((l eq left) && (r eq right)) this
		else operator(l, r)
	}
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :CompoundSelectAs[E, M, V] =
//		(left.basedOn(base), right.basedOn(base)) match {
//			case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[ColumnMappingQuery[E, M, V]]
//			case (l, r) => operator(l, r)
//		}

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
			:CompoundSelectColumnAs[E, M, R] =
		(left.expand(base), right.expand(base)) match {
			case (l, r) if (l eq left) && (r eq right) =>
				this.castFrom[CompoundSelectColumnAs[F, M, R], CompoundSelectColumnAs[E, M, R]]
			case (l, r) => operator(l, r)
		}
//
//	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :CompoundSelectColumnAs[E, M, R] =
//		(mapper(left), mapper(right)) match {
//			case (l :ColumnMappingQuery[E @unchecked, M @unchecked, R @unchecked],
//			      r :ColumnMappingQuery[E @unchecked, M @unchecked, R @unchecked]) =>
//				operator(l, r)
//			case (l, r) =>
//				throw new IllegalExpressionException(
//					s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
//				)
//		}

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[Single, Rows[R]] =
		visitor.compoundSelectColumnAs(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, Single, Rows[R], Y]) :Y =
		visitor.compoundSelectColumnAs(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//	                             E >: CompoundSelectColumnAs[F, M, R] <: SQLExpression[F_, S_, Rows[R]],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, Y]) :Y[S_, Rows[R], E] =
//		visitor.compoundSelectColumnAs(this)
}




object CompoundSelectColumnAs {
	def apply[F <: RowProduct, M[O] <: BaseColumn[V, O], V]
	         (left :ColumnMappingQuery[F, M, V], operator :SelectOperator, right :ColumnMappingQuery[F, M, V])
			:CompoundSelectColumnAs[F, M, V] =
		new Impl(left, operator, right)

/*
	private[sql] def reformed[F <: RowProduct, M[O] <: BaseColumn[V, O], V]
	                 (left :ColumnMappingQuery[F, M, V], operator :SelectOperator, right :ColumnMappingQuery[F, M, V],
	                  rowForm :ColumnReadForm[V]) :CompoundSelectColumnAs[F, M, V] =
	{
		CompoundSelect.validate(left, operator, right, rowForm)
		new Impl(left, operator, right, rowForm)
	}

*/
	private[sql] def reformed[F <: RowProduct, M[O] <: BaseColumn[V, O], V]
	                         (left :ColumnMappingQuery[F, M, V], operator :SelectOperator,
	                          right :ColumnMappingQuery[F, M, V], selectClause :ColumnMappingSQLShape[M, V])
	                         (reform :QueryReform)(implicit spelling :SQLSpelling) :CompoundSelectColumnAs[F, M, V] =
	{
		val res = new ReformedCompoundSelectColumnAs(left, operator, right, selectClause)
		res.reformedBy = (reform, spelling)
		res
	}

	@inline def unapply[F <: RowProduct, V](e :SQLExpression[F, Grouped, Rows[V]])
			:Opt[(ColumnMappingQuery[F, M, V], SelectOperator, ColumnMappingQuery[F, M, V]) forSome {
					type M[O] <: BaseColumn[V, O]
			    }] =
		e match {
			case op :CompoundSelectColumnAs[F @unchecked, MappingOf[V]#TypedColumnProjection @unchecked, V @unchecked] =>
				Got((op.left, op.operator, op.right))
			case _ => Lack
		}

	private class Impl[-F <: RowProduct, M[O] <: BaseColumn[R, O], R]
	                  (override val left :ColumnMappingQuery[F, M, R], override val operator :SelectOperator,
	                   override val right :ColumnMappingQuery[F, M, R])
		extends CompoundSelectColumnAs[F, M, R] with RowShapeCache
	{
		override lazy val selectForm = super.selectForm
	}

	private class ReformedCompoundSelectColumnAs[-F <: RowProduct, M[O] <: BaseColumn[R, O], R]
	              (l :ColumnMappingQuery[F, M, R], op :SelectOperator, r :ColumnMappingQuery[F, M, R],
	               override val selectClause :ColumnMappingSQLShape[M, R])
		extends Impl[F, M, R](l, op, r) //todo: parameterize with CompoundSelectColumnAs[F, M, R]]
			with ReformedCompoundSelectTemplate[R, ({type Q[X] = ColumnQuery[F, X]})#Q, CompoundSelectColumn[F, R]] //CompoundSelectColumnAs[F, M, R]]
	{
		override lazy val rowForm       = selectClause.selectForm
		override lazy val selectForm    = Rows.columnReadForm(selectClause.selectForm)
		override lazy val universalForm = selectClause.universalForm.map(Rows.columnForm(_))
	}



	trait SpecificCompoundSelectColumnAsVisitor[+F <: RowProduct, X, +Y] {
		def compoundSelectColumnAs[M[O] <: BaseColumn[R, O], R]
		                          (e :CompoundSelectColumnAs[F, M, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	type MatchSpecificCompoundSelectColumnAs[+F <: RowProduct, X, +Y] = SpecificCompoundSelectColumnAsVisitor[F, X, Y]
	type CaseSpecificCompoundSelectColumnAs[+F <: RowProduct, X, +Y] = SpecificCompoundSelectColumnAsVisitor[F, X, Y]
//
//
//	trait CompoundSelectColumnAsVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def compoundSelectColumnAs[M[O] <: BaseColumn[S, O], S](e :CompoundSelectColumnAs[F, M, S])
//				:Y[Single, Rows[S], CompoundSelectColumnAs[F, M, S]]
//	}
//	type MatchCompoundSelectColumnAs[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		CompoundSelectColumnAsVisitor[F, Y]
//	type CaseCompoundSelectColumnAs[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		CompoundSelectColumnAsVisitor[F, Y]


	trait AnyCompoundSelectColumnAsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def compoundSelectColumnAs[M[O] <: BaseColumn[V, O], V]
		                          (e :CompoundSelectColumnAs[F, M, V]) :Y[Single, Rows[V]]
	}
	type MatchAnyCompoundSelectColumnAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyCompoundSelectColumnAsVisitor[F, Y]
	type CaseAnyCompoundSelectColumnAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyCompoundSelectColumnAsVisitor[F, Y]
}


package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.ChainApplication
import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{InvalidSQLException, MisalignedExpressionException}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.SQLReadForm
import net.noresttherein.oldsql.slang.saferCasting
import net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate
import net.noresttherein.oldsql.sql.Query.{AbstractQuery, QueryReformingTemplate, QueryTemplate, SingleQuery}
import net.noresttherein.oldsql.sql.Select.SelectOperator
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.TopScope
import net.noresttherein.oldsql.sql.SQLExpression.SQLShape
import net.noresttherein.oldsql.sql.ast.{MappingQuerySQL, QuerySQL, RowShapeCache}
import net.noresttherein.oldsql.sql.mechanics.{Alignment, AlignableColumns, QueryReform, SQLAdaptation, SQLConversion, SQLTransformation, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.Reform.ArityValidator
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.{MayReformLeft, MayReformRight}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}




/** Implements a set operation combining the result sets of two
  * [[net.noresttherein.oldsql.sql.Select parameterized selects]]
  * (or other [[net.noresttherein.oldsql.sql.Query queries]]). The kind of operation is defined by
  * the [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]]
  * of the [[net.noresttherein.oldsql.sql.CompoundSelect.operator operator]] member property.
  * The row schemas of both arguments must match or an exception will be thrown when this expression
  * is converted into an executable SQL statement. If the schema of any of the member ''selects'' is flexible
  * (it is defined by a mapping with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect Optional]] columns),
  * the schema of the first member is used for both of the arguments.
  */
trait CompoundSelect[P, R]
	extends Query[P, R]
	   with CompoundSelectTemplate[R, ({ type Q[X] = Query[P, X] })#Q, CompoundSelect[P, R]]
{
	override def constituents :Seq[SingleQuery[P, R]] = left.constituents ++: right.constituents

	//this easily may end in an infinite recursion, unless the result has externally specified selectClause
//	override def selectClause :SQLShape[R] =
//		QueryReform(ArityValidator)(this)(StandardSQL.spelling).selectClause
//
//	protected def construct[X](left :Query[P, X], operator :SelectOperator, right :Query[P, X]) :CompoundSelect[P, X] =
//		CompoundSelect(left, operator, right)

	override def adaptRows[X](conversion :SQLAdaptation[R, X]) :CompoundSelect[P, X] =
		if (conversion.isIdentity) this.castFrom[CompoundSelect[P, R], CompoundSelect[P, X]]
		else CompoundSelect(left.adaptRows(conversion), operator, right.adaptRows(conversion))

	override def transform[X](transformation :SQLTransformation[R, X]) :CompoundSelect[P, X] =
		operator(left.transform(transformation), right.transform(transformation))


/*
	protected override def crossReform[X](other :Query[X, R])(reform :QueryReform)
	                                     (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R]) =
	{
		val selfReform  = reform.prohibit(reform.leftSubqueryPermissions)
		val otherReform = reform.prohibit(reform.rightSubqueryPermissions)
		val left  = selfReform(this) //will default to calling this.reformed(selfReform) -> selfReform.default(this)
		val right = otherReform(other)
		//in theory, this.left should have been reformed with selfReform.left(this),
		// which should equal leftReform, so the call leftReform(left.left) should return a cached value.
		val leftReform = reform.prohibit(operator.allowedReforms + MayReformRight)
		val rightReform = reform.prohibit(operator.allowedReforms.asInRight + MayReformRight)
		val (leftWithOther, otherWithLeft) = left.left.`->crossReform`(right)(leftReform)
		val (rightWithOther, otherReformed) = left.right`->crossReform`(otherWithLeft)(rightReform)
		(operator(leftWithOther, rightWithOther), otherReformed)
	}

	protected override def crossReform_:[X](other :SingleQuery[X, R])(reform :QueryReform)
	                                       (implicit spelling :SQLSpelling) :(SingleQuery[X, R], CompoundSelect[P, R]) =
	{
		val otherReform = reform.prohibit(reform.leftSubqueryPermissions)
		val selfReform  = reform.prohibit(reform.rightSubqueryPermissions)
		val left  = otherReform(other)
		val right = selfReform(this)
		val leftReform  = reform.prohibit(operator.allowedReforms.asInLeft + MayReformLeft)
		val rightReform = reform.prohibit(operator.allowedReforms + MayReformLeft)
		val (otherWithLeft, leftWithOther)  = left.`->crossReform`(right.left)(leftReform)
		val (otherReformed, rightWithOther) = otherWithLeft.`->crossReform`(right.right)(rightReform)
		(otherReformed, operator(leftWithOther, rightWithOther))
	}
*/
	protected override def reform[X](second :Query[X, R])(reform :QueryReform)
	                                (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R]) =
	{
//		val self  = reform.left.apply(this)
//		val other = reform.right.apply(second)
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

	protected override def reform_:[X](first :SingleQuery[X, R])(reform :QueryReform)
	                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
	{
//		val self = reform.right.apply(this)
		val leftReform  = reform.prohibit(operator.allowedReforms.asInLeft + MayReformLeft)
		val rightReform = reform.prohibit(operator.allowedReforms + MayReformLeft)
		val (firstWithLeft, leftWithFirst)   = ((first :Query[X, R]) `->reform` left)(leftReform)
		val (firstWithRight, rightWithFirst) = ((firstWithLeft :Query[X, R]) `->reform` right)(rightReform)
		//this is better than reformedThis = leftWithFirst.reform(rightWithFirst)(operator),
		// as it guarantees that, in case of sql terms, regardless of the direction in which the forms are passed
		// in all reform methods, they all share the same form as firstWithRight
		val finalLeft = (firstWithRight `->reform` leftWithFirst)(reform.prohibitReformLeft)._2
		val reformedThis =
			if ((finalLeft eq left) & (rightWithFirst eq right)) this
			else operator(finalLeft, rightWithFirst) //shouldn't we set reformedBy = rightReform here?
		(firstWithRight, reformedThis)
	}

	protected override def reform_:[X](first :Select[X, R])(reform :QueryReform)
	                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
		reform_:(first :SingleQuery[X, R])(reform)

	@throws[MisalignedExpressionException]("if select clauses of member selects cannot be unified to a common shape.")
	override def reformed(implicit spelling :SQLSpelling) :CompoundSelect[P, R] =
		try {
			spelling.queryReform(this)
		} catch {
			case e :InvalidSQLException =>
				throw new MisalignedExpressionException(
					s"Irreconcilable shapes of member selects in $this: $left vs $right.", e
				)
		}

	protected override def reform(reform :QueryReform)(implicit spelling :SQLSpelling) :CompoundSelect[P, R] =
		reform.default(this)

	protected override def alignment(permissions: Permissions)(implicit spelling: SQLSpelling): Alignment =
		spelling.alignment(left, permissions.asInLeft & operator.allowedReforms.asInLeft) :++
			spelling.alignment(right, permissions.asInRight & operator.allowedReforms.asInRight)

	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		spelling.potentialColumns(selectClause, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
		spelling.potentialColumnsCount(selectClause)

/*
	protected override def shape(implicit spelling :SQLSpelling) :RowShape = {
		var res = _shape
		if (_shapeSpelling == spelling)
			res
		else {
			val reformed = try { spelling.queryReform(this) } catch {
				case e :InvalidSQLException =>
					throw new MisalignedExpressionException("Non unifiable select clauses in query " + this + ".", e)
			}
			spelling.shape(selectClause)
//			val l = spelling.inLeft(this).shape(reformed.left)
//			val r = spelling.inRight(this).shape(reformed.right)
			try { res = l | r } catch {
				case _ :IllegalArgumentException =>
					throw new MisalignedExpressionException(
						if ((reformed.left eq left) && (reformed.right eq right))
							s"Irreconcilable shapes of member selects in $this: $l vs $r."
						else
							s"Irreconcilable shapes of member selects in $this (unified as $reformed): $l vs $r."
					)
			}
			if (_shapeSpelling == null) synchronized {
				if (_shapeSpelling == null) {
					_shapeSpelling = spelling
					_shape = res
				}
			}
			res
		}
	}

	@volatile @transient private var _shape :RowShape = _
	@volatile @transient private var _shapeSpelling :SQLSpelling = _

	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		if (_shapeSpelling == spelling)
			_shape.size
		else try {
			val reformed = spelling.queryReform(this)
			val leftCount  = spelling.inLeft(this).columnCount(reformed.left)
			val rightCount = spelling.inRight(this).columnCount(reformed.right)
			if (leftCount != rightCount)
				throw new IllegalStateException(
					if ((reformed.left eq left) && (reformed.right eq right))
						s"Differing column counts of member selects of $this: $leftCount vs $rightCount."
					else
						s"Differing column counts of member selects of $this (unified as $reformed): $leftCount vs $rightCount."
				)
			leftCount
		} catch {
			case e :InvalidSQLException =>
				throw new IllegalStateException("Non unifiable select clauses in query " + this + ".", e)
		}
*/
	protected override def defaultSpelling(context :SQLContext[P])(implicit spelling :SQLSpelling) :SpelledSQL[P] = {
		val withClause = if (spelling.scope == TopScope) this.withClause else localWithClause
		val withSQL =
			if (withClause.isEmpty) SpelledSQL(context)
			else spelling.inWith(withClause)(context, Parameterization.paramless) + " "
//		val CompoundSelect(left, _, right) = spelling.queryReform(this)(this)
		val inLeft = spelling.inLeft(this)
		val l = left match {
			case CompoundSelect(_, op, _) if op != operator || left.withClause.local.nonEmpty =>
				"(" +: (inLeft(left)(withSQL.context) + ")")
			case _ =>
				inLeft(left)(withSQL.context)
		}
		val inRight = spelling.inRight(this)
		val r = right match { //use operator name comparison as different implementations than Minus are possible
			case CompoundSelect(_, op, _) if op != operator || operator.NAME == "MINUS" ||
			                                 right.localWithClause.nonEmpty =>
				"(" +: (inRight(right)(l.context) + ")")
			case _ =>
				inRight(right)(l.context)
		}
		val sql = withSQL.sql + l.sql + (" " + spelling(operator) + " ") + r.sql
		SpelledSQL(sql, withSQL.setter + l.setter + r.setter, r.context)
	}


	override def homomorphic(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case other :CompoundSelect[_, _]  =>
			operator == other.operator && (left homomorphic other.left) && (right homomorphic other.right)
		case _ => false
	}
	override def isomorphic(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case other :CompoundSelect[_, _] =>
			operator == other.operator && (left isomorphic other.left) && (right isomorphic other.right)
		case _ => false
	}
	override def equivalent(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case other :CompoundSelect[_, _] =>
			operator == other.operator && (left equivalent other.left) && (right equivalent other.right)
		case _ => false
	}
	override def identical(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case other :CompoundSelect[_, _] if canEqual(other) && other.canEqual(this) =>
			operator == other.operator && (left identical other.left) && (right identical other.right)
		case _ => false
	}
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :CompoundSelect[_, _] if canEqual(other) && other.canEqual(this) =>
			operator == other.operator && left == other.left && right == other.right
		case _ => false
	}
	override def hashCode :Int = (operator.hashCode * 31 + left.hashCode) * 31 + right.hashCode

	override def toString :String = s"($left) $operator ($right)"
}



object CompoundSelect {
	def apply[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V]) :CompoundSelect[P, V] =
		new Impl(left, operator, right) {
			override lazy val selectClause = super.selectClause
		}

	def apply[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V], selectClause :SQLShape[V])
			:CompoundSelect[P, V] =
	{
		val res = new ReformedCompoundSelect(left, operator, right, selectClause)
		validate(res)
		res
	}

	//we don't know if they have a SelectId - should we check all selects for it?
//		private[sql] def reformed[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V])
//		                               (reform :Reform)(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
//			reformed[P, V](left, operator, right, CompoundSelectIdSQL.selectClause(_))(reform)

	private[sql] def reformed[P, V]
	                 (left :Query[P, V], operator :SelectOperator, right :Query[P, V], selectClause :SQLShape[V])
	                 (reform :QueryReform)(implicit spelling :SQLSpelling)
			:CompoundSelect[P, V] =
	{
		val res = new ReformedCompoundSelect(left, operator, right, selectClause)
		res.reformedBy = (reform, spelling)
		res
	}

	private[sql] def reformed[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V])
	                               (selectClause :CompoundSelect[P, V] => SQLShape[V])
	                               (reform :QueryReform)(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
	{
		val init = selectClause
		val res = new Impl[P, V](left, operator, right) {
			override lazy val selectClause = init(this)
		}
		res.reformedBy = (reform, spelling)
		res
	}


	@inline def unapply[P, V](query :Query[P, V]) :Opt[(Query[P, V], SelectOperator, Query[P, V])] =
		query match {
			case op :CompoundSelect[P @unchecked, V @unchecked] => Got((op.left, op.operator, op.right))
			case _ => Lack
		}

	private[sql] def validate[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
	                         (query :CompoundSelectTemplate[R, Q, Self]) :Unit =
	{
		if (!(query.left.rowForm comparable query.right.rowForm))
			throw new IllegalArgumentException(
				"Mismatched select clauses of member selects (" + query.left.rowForm + " vs " + query.right.rowForm
				+ ") in " + query + "."
			)
		if (!(query.left.rowForm comparable query.selectClause.selectForm))
			throw new IllegalArgumentException(
				"Given select clause " + query.selectClause + " is not comparable with form " + query.left.rowForm +
					" of the left side in " + query + "."
			)
		if (!(query.right.rowForm comparable query.selectClause.selectForm))
			throw new IllegalArgumentException(
				"Given select clause " + query.selectClause + " not comparable with form " + query.right.rowForm +
					" of the right side in " + query + "."
			)
	}

	private[sql] def validate[R, Q[_]](left :QueryTemplate[R, Q], operator :SelectOperator,
	                                   right :QueryTemplate[R, Q], rowForm :SQLReadForm[R]) :Unit =
	{
		if (!(left.rowForm comparable right.rowForm))
			throw new IllegalArgumentException(
				"Mismatched select clauses of member selects (" + left.rowForm + " vs " + right.rowForm +
				") in (" + left + ") " + operator + " (" + right + ")."
			)
		if (!(left.rowForm comparable rowForm))
			throw new IllegalArgumentException(
				"Given rowForm " + rowForm + " not comparable with form " + left.rowForm + " of the left side in (" +
				left + ") " + operator + " (" + right + ")."
			)
		if (!(right.rowForm comparable rowForm))
			throw new IllegalArgumentException(
				"Given rowForm " + rowForm + " not comparable with form " + right.rowForm + " of the right side in (" +
				left + ") " + operator + " (" + right + ")."
			)
	}


	type __ = CompoundSelect[_, _]

	type TypedCompoundSelect[P, +M[O] <: MappingAt[O], V] = CompoundSelect[P, V] { type RowMapping[O] <: M[O] }



	/** Common declarations shared by ''compound select'' implementations:
	  * statement [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]
	  * and expression [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]].
	  * @tparam R $R
	  * @tparam Q $Q
	  *
	  * @define Q the general type of member subqueries `left`, `right`, and `constituents`.
	  */
	trait CompoundSelectTemplate[R, +Q[X] <: QueryTemplate[X, Q], +Same <: AbstractQuery[R]]
		extends QueryTemplate[R, Q] with QueryReformingTemplate[R, Same]
	{ this :Q[R] with Same =>
		val left  :Q[R] //must be primary constructor parameters!
		val right :Q[R]
		val operator :SelectOperator
		override val selectCount  :Int = left.selectCount + right.selectCount
//		override def selectClause :SQLLayout[V] =
		//consider: this means that the local with clause of any compound select must be empty; are we fine with it?
		private lazy val ctes     :WithClause = left.withClause.outer ++ right.withClause.outer
		override def withClause   :WithClause = ctes

//		override def rowsTo[X](implicit conversion :SQLConversion[V, X]) :Q[X] =
//			if (conversion.isIdentity) conversion(this)
//			else construct(left.rowsTo[X], operator, right.rowsTo[X])
//
//		override def map[X](f :V => X) :Q[X] = construct(left.map(f), operator, right.map(f))
//
//		protected[this] def construct[X](left :Q[X], operator :SelectOperator, right :Q[X]) :Q[X]
		@inline final private[sql] def asSuperQuery :Same = this

		@throws[MisalignedExpressionException]("if the column types of left and right subqueries are incompatible.")
		override def rowForm :SQLReadForm[R] = {
			val l = left.rowForm
			val r = right.rowForm
			if (l comparable r)
				if (l.isUniversal) l else if (r.isUniversal) r else selectClause.selectForm
			else
				selectClause.selectForm
		}

		@throws[MisalignedExpressionException]("if the column types of left and right subqueries are incompatible.")
		override def columns :Seq[TypedColumn[_, this.type]] = {
			roughlyValidate()
			left.columns.withOrigin[this.type]
		}

		//fixme: selectClause at least of reformed instances should best represent the expression:
		//  we should not return a term when paired with a ComponentSQL, and reforming composite expressions
		//  should be recursive, always picking the expression type most likely to be usable in future reforming
		@throws[MisalignedExpressionException]("if the column types of left and right subqueries are incompatible.")
		override def selectClause :SQLShape[R] = {
			roughlyValidate()
			left.selectClause
		}

		override def rowsTo[X](implicit conversion :SQLConversion[R, X]) :Q[X]

		//Transform is not here because SQLTransformation doesn't always translate a column to a column,
		// so SelectColumn cannot return a SelectColumn
//		def transform[X](conversion :SQLTransformation[R, X]) :Q[X]

		//Todo: rename either   these or SQLExpression.map as, even if they won't have conflicting erasures due to
		// different return types, will break inference of type arguments. Same for overload with the other Query.map
		override def mapRows[X](f :R => X) :Q[X] = rowsTo(SQLConversion(".mapRows", f))

		override def mapRows[Fun, C <: Chain, X]
		                    (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :R <:< C) :Q[X] =
			mapRows(applyFun(f))

		/** Checks if the left and right side of this $this have compatible column types,
		  * as defined by [[net.noresttherein.oldsql.sql.RowShape.<:> <:>]]. This does not imply yet that the $this
		  * is valid SQL, as defined by the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy
		  * in use, or that its shape is final. It is however a good indicator of the likelihood of this fact,
		  * and can be validated relatively swiftly, without depending on any `SQLSpelling`.
		  * If this method returns `true`, then none of the property accessors
		  * will throw a [[net.noresttherein.oldsql.exceptions.InvalidSQLException InvalidSQLException]].
		  * @see [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.isReformed]]
		  */
		override def isRoughlyConsistent :Boolean =
			isReformed || {
				val leftColumns  = left.columns
				val rightColumns = right.columns
				leftColumns.length == rightColumns.length &&
					leftColumns.corresponds(rightColumns)(_.form.shape <:> _.form.shape)
			}

		/** Throws a `MisalignedExpressionException` if this $this is not
		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.isRoughlyConsistent roughly consistent]].
		  */
		@throws[MisalignedExpressionException]("if the column types of left and right subqueries are incompatible.")
		def roughlyValidate() :Unit =
			if (!isRoughlyConsistent)
				throw new MisalignedExpressionException(
					"Columns of `" + this + "` are undefined because the column lists of the left (" + left.columns +
						") and the right (" + right.columns + ") subqueries do not match. The query must be reformed."
				)

		/** Check if the $this was properly
		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.reformed reformed]]
		  * by some [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] instance.
		  * If so, the left and right subqueries were deemed compatible and may be treated as such. The exact level
		  * of integrity depends on the [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]] used,
		  * but by default this means that both row [[net.noresttherein.oldsql.sql.Query.QueryTemplate.shape shapes]]
		  * and expression structure
		  * (the generalized [[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause selectClause]] expression)
		  * are compatible - the columns have types which are considered equivalent from the point of view of a query,
		  * as defined by [[net.noresttherein.oldsql.sql.RowShape.<:> <:>]], and expressions are either of the same
		  * class (including subexpressions), or otherwise the column type match is considered not be an effect
		  * of chance (for example, [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]]'s keys match exactly
		  * the column names of a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] it is aligned with.
		  * Again, the exact meaning depends on spelling and reform used.
		  * @see [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.reformed]]
		  * @see [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.isRoughlyConsistent]]
		  */
		override def isReformed :Boolean = _reformed eq this

		/** The default strategy for unifying the ''select'' clauses of `left` and `right` subqueries
		  * of this ''compound select''. The default implementation delegates to
		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.operator operator]]`.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.defaultReform defaultReform]].
		  */
		def defaultReform(implicit spelling :SQLSpelling) :QueryReform =
			if (spelling.scope == TopScope) operator.topReform else operator.defaultReform

		@throws[MisalignedExpressionException]("if select clauses of member selects cannot be unified to a common shape.")
		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Same =
			try { //consider: using reform.compound(this) instead of reform
				val ctx = (reform, spelling) //consider: maybe instead of spelling use spelling.scope as the key?
				var res = if (_reformedContext == ctx) _reformed else null.asInstanceOf[Same]
				if (res == null) {
					res = this.reform(reform)
					synchronized {
						if (_reformedContext == null) {
							_reformed = res
							_reformedContext = ctx
						}
					}
				}
				res
			} catch {
				case e :InvalidSQLException =>
					throw new MisalignedExpressionException(
						s"Irreconcilable shapes of member selects in $this: $left vs $right.", e
					)
			}
		protected def reform(query :QueryReform)(implicit spelling :SQLSpelling) :Same


		/** A cached reformed version of this instance created using `QueryReform` and `SQLSpelling`
		  *  from `_reformedContext`. */
		@volatile @transient private[this] var _reformed        :Same = _
		/** `QueryReform` and `SQLSpelling` instances used to create `_reformed` instance, if not `null`.  */
		@volatile @transient private[this] var _reformedContext :(QueryReform, SQLSpelling) = _

		private[sql] final def reformedBy :Opt[(QueryReform, SQLSpelling)] = Opt(_reformedContext)
		private[sql] final def reformedBy_=(params :(QueryReform, SQLSpelling)) :Unit = synchronized {
			_reformed = this
			_reformedContext = params
		}

		protected override def allowedReforms(implicit spelling: SQLSpelling): Permissions = operator.allowedReforms

		/** The default reforming strategy for unifying select clauses of SQL ''selects'' within a ''compound select'',
		  * used for top level queries, that is when this query is the whole spelled expression, and not a subquery
		  * of another ''compound select''. The default implementation delegates to
		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.operator operator]]`.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.topReform topReform]].
		  */
//		override def topReform(implicit spelling :SQLSpelling)     :QueryReform = operator.topReform(this)

		//it's tempting to extract the identical reform/reform_:/reformed methods from CompoundSelect and CompoundSelectSQL,
		//  but unfortunately TopSelectSQL inherits both, which would lead to a conflict.

		@throws[MisalignedExpressionException]("if select clauses of member selects cannot be unified to a common shape.")
		protected override def shape(implicit spelling :SQLSpelling) :RowShape = {
			val reformed :Same = this.reformed
			try {
				spelling.inSelect.shape(reformed.selectClause)
			} catch {
				case e :InvalidSQLException =>
					throw new MisalignedExpressionException(
						if (reformed eq this)
							"Irreconcilable shapes of member selects in " + this + "."
						else
							"Irreconcilable shapes of member selects in " + this + " (unified as " + reformed + ").",
						e
					)
			}
		}
		@throws[MisalignedExpressionException]("if select clauses of member selects cannot be unified to a common shape.")
		protected override def columnCount(implicit spelling :SQLSpelling) :Int = {
			val reformed :Same = this.reformed
			try {
				spelling.inSelect.columnCount(reformed.selectClause)
			} catch {
				case e :InvalidSQLException =>
					throw new MisalignedExpressionException(
						if (reformed eq this)
							"Irreconcilable shapes of member selects in " + this + "."
						else
							"Irreconcilable shapes of member selects in " + this + " (unified as " + reformed + ").",
						e
					)
			}
		}
	}


	private[sql] trait ReformedCompoundSelectTemplate[R, +Q[X] <: QueryTemplate[X, Q], +Same <: Q[R]]
		extends CompoundSelectTemplate[R, Q, Same]
	{ this :Same =>
		override lazy val rowForm = selectClause.selectForm
//		override lazy val universalForm = selectClause.universalForm
//		override def isReformed = true
//		override def isRoughlyConsistent = true

//		protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[Rows[R]] =
//			Rows.form(spelling.inSelect.effectiveForm(selectClause))
		protected override def shape(implicit spelling :SQLSpelling) :RowShape =
			spelling.inSelect.shape(selectClause)
	}

	private class Impl[P, R](override val left  :Query[P, R], override val operator :SelectOperator,
	                         override val right :Query[P, R])
		extends CompoundSelect[P, R] with RowShapeCache
	{
		override type RowMapping[O] = left.RowMapping[O]
		override def mapping[O] = left.mapping[O]
		override def export[O] = left.export[O] //todo: this should involve some reconciliation

		override lazy val constituents = super.constituents

		override def bind(params :P) :QuerySQL[RowProduct, R] =
			operator(left.bind(params), right.bind(params))
	}

	private class ReformedCompoundSelect[P, R](l :Query[P, R], op :SelectOperator, r :Query[P, R],
	                                           override val selectClause :SQLShape[R])
		extends Impl(l, op, r)
		   with ReformedCompoundSelectTemplate[R, ({ type Q[X] = Query[P, X] })#Q, CompoundSelect[P, R]]
}




/** Implements a set operation combining the result sets of two parameterized
  * [[net.noresttherein.oldsql.sql.Select.SelectMapping selects]]
  * (or other [[net.noresttherein.oldsql.sql.MappingQuery queries]]), sharing the same row schema,
  * as defined by the shared [[net.noresttherein.oldsql.schema.Mapping mapping]] type `M`. The kind of operation
  * is defined by the [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]]
  * of the [[net.noresttherein.oldsql.sql.CompoundSelect.operator operator]] member property.
  */ //todo: move it to sql or sql.CompoundSelect
trait CompoundSelectMapping[P, M[O] <: MappingAt[O]]
	extends CompoundSelect[P, M[Unit]#Subject] with MappingQuery[P, M]
{
	override val left :MappingQuery[P, M]
	override val right :MappingQuery[P, M]

	override def mapping[O] = left.mapping
	override def export[O]  = left.export //todo: this should involve some reconciliation

	override def bind(params :P) :MappingQuerySQL[RowProduct, M] = operator(left.bind(params), right.bind(params))
}


object CompoundSelectMapping {
	def apply[P, M[O] <: MappingAt[O]]
	         (left :MappingQuery[P, M], operator :SelectOperator, right :MappingQuery[P, M])
			:CompoundSelectMapping[P, M] =
		new Impl(left, operator, right, left.rowForm)

/*
	private[sql] def reformed[P, M[O] <: MappingAt[O]]
	                         (left :MappingQuery[P, M], operator :SelectOperator, right :MappingQuery[P, M],
	                          selectClause :MappingSQLShape[M, M[Unit]#Subject])
	                         (reform :QueryReform)(implicit spelling :SQLSpelling) :CompoundSelectMapping[P, M] =
	{
		val res =
			new Impl(left, operator, right)
				with ReformedCompoundSelectTemplate[M[Unit]#Subject, ({type Q[x] = Query[P, x]})#Q, CompoundSelectMapping[P, M]]
				with RowShapeCache
		res.reformedBy = (reform, spelling)
		res
	}
	private[sql] def reformed[P, M[O] <: MappingAt[O]]
	                         (left :MappingQuery[P, M], operator :SelectOperator, right :MappingQuery[P, M],
	                          rowForm :SQLReadForm[M[Unit]#Subject]) :CompoundSelectMapping[P, M] =
	{
		CompoundSelect.validate(left, operator, right, rowForm)
		new Impl(left, operator, right, rowForm)
			with ReformedCompoundSelectTemplate[M[Unit]#Subject, ({ type Q[x] = Query[P, x] })#Q, CompoundSelectMapping[P, M]]
			with RowShapeCache
	}
*/

	@inline def unapply[P, V](query :Query[P, V])
			:Opt[(MappingQuery[P, M], SelectOperator, MappingQuery[P, M]) forSome { type M[O] <: MappingAt[O] }] =
		query match {
			case op :CompoundSelectMapping[P @unchecked, MappingAt @unchecked] =>
				Got((op.left, op.operator, op.right))
			case _ => Lack
		}


	type __ = CompoundSelectMapping[_, M] forSome { type M[A] <: MappingAt[A] }

	private class Impl[P, M[O] <: MappingAt[O]]
	                  (override val left :MappingQuery[P, M], override val operator :SelectOperator,
	                   override val right :MappingQuery[P, M], override val rowForm :SQLReadForm[M[Unit]#Subject])
		extends CompoundSelectMapping[P, M] with RowShapeCache
}




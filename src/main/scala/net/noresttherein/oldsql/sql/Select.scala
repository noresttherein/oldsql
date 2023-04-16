package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.OperationView.SelectView
import net.noresttherein.oldsql.collection.{Chain, Listing, Opt}
import net.noresttherein.oldsql.collection.Chain.ChainApplication
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{InseparableExpressionException, InvalidSQLException, MisalignedExpressionException, MismatchedExpressionsException, UndefinedShapeException}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.{Relation, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.IndexedMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.CompoundSelect.{CompoundSelectTemplate, ReformedCompoundSelectTemplate}
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.MappingQuery.{ComposedMappingQuery, SingleMappingQuery}
import net.noresttherein.oldsql.sql.Query.{ComposedQuery, ComposedSingleQuery, QueryTemplate, SingleQuery, SingleQueryTemplate}
import net.noresttherein.oldsql.sql.RowProduct.{Complete, GroundRow, TopRow}
import net.noresttherein.oldsql.sql.Select.{ArbitrarySelect, SelectOperator, SelectTemplate}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.TopScope
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, SQLShape}
import net.noresttherein.oldsql.sql.ast.{ColumnMappingQuery, ColumnQuery, ComponentSQL, CompoundSelectAs, CompoundSelectColumn, CompoundSelectColumnAs, CompoundSelectSQL, HasRowShape, IndexedSQL, MappingQuerySQL, MappingSQL, QuerySQL, RelationSQL, RowShapeCache}
import net.noresttherein.oldsql.sql.ast.IndexedSQL.{LabeledColumnSQL, LabeledValueSQL}
import net.noresttherein.oldsql.sql.ast.SelectAs.TopSelectAs
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.mechanics.{Alignment, AlignableColumns, QueryReform, Reform, SQLAdaptation, SQLConversion, SQLScribe, SQLTransformation, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.QueryReform.UnionAllReform
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.Reform.ArityValidator
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.MayReform

//here be implicits
import net.noresttherein.oldsql.slang._





/**
  * @author Marcin Mościcki
  */
trait Select[P, R] extends SingleQuery[P, R] with SelectTemplate[R, ({ type Q[X] = Select[P, X] })#Q, Select[P, R]] {

	/** The from clause of this select. It is in its [[net.noresttherein.oldsql.sql.RowProduct!.Complete Complete]] form:
	  * all clause type constructors are in their [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]]
	  * form, and all relations are listed. This assures that
	  * [[net.noresttherein.oldsql.sql.Select.SelectTemplate.from from]]`.Complete <:< Form` and `Form <:< form.Complete`.
	  * As Scala does not allow recursive type aliases, `Complete` is instead bound from both sides by `From`.
	  */
	override type From <: TopRow { type Complete <: Select.this.From; type Params = P }

	override def rowForm :SQLReadForm[R] = selectClause.selectForm

//	override def constituents :Seq[Select[P, V]] = ReversedList :+ this
	override def transform[X](transformation :SQLTransformation[R, X]) :Select[P, X] =
		selectOther(transformation(selectClause))

	override def adaptRows[X](conversion :SQLAdaptation[R, X]) :Select[P, X] =
		if (conversion.isIdentity) this.castParam2[X]
		else selectOther(conversion(selectClause))
		//ArbitrarySelect[P, From, X](from, selectClause.to[X], isDistinct)

//	override def map[X](f :R => X) :Select[P, X] =
//		selectOther(selectClause.map(f))
//		ArbitrarySelect[P, From, X](from, selectClause.map(f), isDistinct)

	//order by will be problematic here
	def selectOther[X](selectClause :SQLExpression[From, Grouped, X]) :Select[P, X] =
		if (selectClause eq this.selectClause)
			this.castFrom[Select[P, R], Select[P, X]]
		else if (isDistinct)
			selectClause.paramSelectFrom[P, From](from).distinct
		else
			selectClause paramSelectFrom from

	override def bind(params :P) :TopSelectSQL[R]

	//todo: orderBy - should it be a function of the selectClause, or From?

	//todo: take/drop/top/limit


	/** Invokes `this `[[net.noresttherein.oldsql.sql.Query.reform_: reform_:]]` second`. */
	protected override def reform[X](second :Query[X, R])(reform :QueryReform)
	                                (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R]) =
		(this `->reform_:` second)(reform)

	protected override def reform_:[X](first :SingleQuery[X, R])(reform :QueryReform)
	                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
	{
		val reformRight = reform.reform.prohibitReformLeft
		val reformed =
			reformRight[Nothing, Grouped, R, SQLShape, From, Grouped, R, SQLExpression.from[From]#rows[Grouped]#E, R](
				first.selectClause, selectClause)(SQLConversion.toSelf, SQLConversion.toSelf, spelling.inSelect
			)._2
		if (reformed eq selectClause)
			(first, this)
		else
			(first, selectOther(reformed))
	}

	protected override def reform_:[X](first :Select[X, R])(reform :QueryReform)
	                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
	{
		val (left, right) = reform.reform(spelling)(first.selectClause, selectClause)(
			SQLConversion.toSelf, SQLConversion.toSelf, spelling.inSelect
		)
		val leftSelect =
			if (left eq first.selectClause) first
			else first.selectOther(left)
		val rightSelect =
			if (right eq selectClause) this
			else selectOther(right)
		(leftSelect, rightSelect)
	}

//	override def reformed(implicit spelling :SQLSpelling) :Select[P, R] = reformed(spelling.queryReform) //.default(this)

	protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Select[P, R] =
		reform.default(this)

	protected override def potentialColumns(permissions :Permissions)(implicit spelling :SQLSpelling) :AlignableColumns =
		spelling.potentialColumns(selectClause, permissions)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
		spelling.potentialColumnsCount(selectClause)

	protected override def defaultSpelling(context :SQLContext[P])(implicit spelling :SQLSpelling) :SpelledSQL[P] = {
		def completeParams(from :RowProduct) :Parameterization[from.Params, from.Complete] = from.parameterization
		defaultSpelling(from, context)(completeParams(from), from.parameterization)
	}


	override def homomorphic(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case s :Select[_, _] if this.canEqual(that) && s.canEqual(this) =>
			isDistinct == s.isDistinct && (selectClause homomorphic s.selectClause) && (from homomorphic s.from)
		case _ => false
	}
	override def isomorphic(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case s :Select[_, _] if this.canEqual(that) && s.canEqual(this) =>
			isDistinct == s.isDistinct && (selectClause isomorphic s.selectClause) && (from isomorphic s.from)
		case _ => false
	}
	override def equivalent(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case s :Select[_, _] if this.canEqual(that) && s.canEqual(this) => //or from equivalent s.from?
			isDistinct == s.isDistinct && (selectClause equivalent s.selectClause) && (from isomorphic s.from)
		case _ => false
	}
	override def identical(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case s :Select[_, _] if this.canEqual(that) && s.canEqual(this) =>
			isDistinct == s.isDistinct && (selectClause identical s.selectClause) && (from identical s.from)
		case _ => false
	}
	override def equals(that :Any) :Boolean = that match {
		case s :AnyRef if s eq this => true
		case s :Select[_, _] if s canEqual this=>
			isDistinct == s.isDistinct && s.selectClause == selectClause && s.from == from
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Select[_, _]] //that.getClass == getClass
	override def hashCode :Int = (selectClause.hashCode * 31 + from.hashCode) * 31 + isDistinct.hashCode

}




/** A lower level factory of SQL ''select'' statements and expressions. It is used mostly internally,
  * by the [[net.noresttherein.oldsql.sql.SQLExpression.selectFrom selectFrom]] family of methods
  * of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  * and by the [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]] type class which governs
  * what type of SQL ''select'' can be created based on its [[net.noresttherein.oldsql.sql.RowProduct from]]
  * and ''select'' clause.
  *
  * Applications wishing to create a query should first have a look at [[net.noresttherein.oldsql.sql.From From]],
  * a factory of single table [[net.noresttherein.oldsql.sql.FromClause from clauses]], which serves as the starting
  * point of the SQL DSL. Through it, an SQL ''select'' is built in a more natural, reversed fashion,
  * in which its ''from'' clause (including a ''where'' clause, and, optionally
  * a [[net.noresttherein.oldsql.sql.GroupByClause group by]] clause) is created first, and the ''select'' clause,
  * that is an `SQLExpression` based on that ''from'' clause, is provided last,
  * to one of the [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]]
  * extension methods of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]:
  * {{{
  *     From(AnimalCompanions) join Rangers on (_.id) === (_.familiarId) where {
  *         row => row.prev.species ==? "Hamster" && row.prev.name ==? "Boo"
  *     } select _.last.name
  * }}}
  *
  * This object can be however useful in more generic code, where an `SQLExpression` for the ''select'' clause
  * and its domain `RowProduct` are built separately:
  * {{{
  *     Select(selectClause) from fromClause
  *     Select(Dragons.joined) from From(Dragons)
  * }}}
  *
  * Two separate kinds of abstract SQL ''select'' representations exist:
  *   1. [[net.noresttherein.oldsql.sql.Select Select]], the companion class to this object,
  *      is an (optionally) parameterized statement, which,
  *      through [[net.noresttherein.oldsql.sql.Select.spell spelling]],
  *      can be [[net.noresttherein.oldsql.sql.Select.chant converted]]
  *      to an executable [[net.noresttherein.oldsql.sql.Incantation Incantation]].
  *      These serve as stand-alone queries, or parts of a bigger
  *      [[net.noresttherein.oldsql.sql.Query.CompoundSelect CompoundSelect]] forming the final query.
  *   1. [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]
  *      is an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], which can be freely used
  *      within larger SQL expressions, for example as an argument to ''exists''. They are distinguished
  *      from the former in that they can represent ''dependent selects''
  *      ([[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL subselects]]) and capture the namespace of their
  *      outer ''select'' in their ''from'' clause, allowing them to use tables and columns from the outer ''select''
  *      in addition to their own. They can be parameterized by [[net.noresttherein.oldsql.sql.ParamClause unbound]]
  *      parameters only in the outer `RowProduct` - their actual ''from'' clause type becomes lost.
  *
  * The gap between the two is bridged by [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]],
  * which extends both types and represent a ground ''select'' - one without any free variables, that is
  * depending on neither unbound parameters nor any external tables.
  */
object Select {
	//todo: mapping indexed headers

/*
	@inline def apply[E <: SQLExpression[F, Grouped, _], F <: RowProduct]
	                 (expr :E)(implicit result :CanSelect[F, E]) :SelectFactory[E, F] =
		new SelectFactory(expr)

	class SelectFactory[E <: SQLExpression[F, Grouped, _], F <: RowProduct] private[Select]
	                  (private val selectClause :E)
		extends AnyVal
	{
		@inline final def from(clause :F)(implicit result :CanSelect[F, E]) :result.Select =
			result(clause, selectClause)
	}

	@inline def apply(all: *) :SelectAllFactory = new SelectAllFactory {}

	sealed trait SelectAllFactory extends Any {
		@inline final def from[F <: RowProduct]
		                  (clause :F)
		                  (implicit result :CanSelect[F, ChainTuple[clause.Generalized, GlobalScope, clause.Row]])
				:result.Select =
			result(clause, clause.row)
	}
*/

	@inline def apply(from :TopRow) :SelectCurriedFactory[from.Params, from.Complete] =
		new SelectCurriedFactory[from.Params, from.Complete](from.self)

	class SelectCurriedFactory[P, F <: TopRow { type Complete <: F ; type Params = P }] private[Select] (private val from :F)
		extends AnyVal
	{
		//todo: document that not all expressions are accepted!
		def apply[V](selectClause :SQLExpression[F, Grouped, V]) :Select[P, V] =
			ArbitrarySelect[P, F, V](from, selectClause.anchor(from))

//		def apply[V](selectClause :ColumnSQL[from.Self, Grouped, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)
//		def apply[V <: Chain](selectClause :SQLExpression[from.Generalized, Grouped, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)
//
//		def apply[V](selectClause :InlineSQL[from.Generalized, Grouped, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)
//
//		//an unused type parameter due to an overloading resolution bug in scala 2
//		def apply[X, Y, *](selectClause :ConversionSQL[from.Generalized, Grouped, X, Y]) :Select[from.Params, Y] =
//			new ArbitrarySelect[from.Params, from.Self, Y](from.self, selectClause.anchor(from.self))
//
//		def apply[M[A] <: BaseMapping[V, A], V]
//		         (selectClause :EditedLooseComponent[from.Generalized, M, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)
//
//		def apply[M[A] <: BaseMapping[V, A], V]
//		         (selectClause :EditedComponentSQL[from.Generalized, M, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)

		def apply[M[O] <: BaseMapping[V, O], V](selectClause :ComponentSQL[F, M]) :SelectMapping[P, M] =
			new SelectComponent[P, F, M, V](from, selectClause.anchor(from))

		def apply[V <: Listing](selectClause :IndexedSQL[F, Grouped, V])
				:SelectMapping[P, IndexedMapping.of[V]#Mapping] =
			SelectListing[P, F, V](from, selectClause.anchor(from))

		def apply[A <: Label, V](selectClause :LabeledColumnSQL[F, Grouped, A, V])
				:SelectMapping[P, IndexedMapping.of[V]#Column] =
			SelectListingColumn[P, F, A, V](from, selectClause.anchor(from))
	}




	/** An SQL binary operator which can be used to create a query out of two SQL ''selects'', by combining their
	  * result sets. The most notable instance is [[net.noresttherein.oldsql.sql.Select.Union Union]], but other
	  * predefined operators provided by some database engines have also their definition in the enclosing
	  * [[net.noresttherein.oldsql.sql.Select Select]] object.
	  */
	abstract class SelectOperator(val name :String) {
		val NAME :String = name.toUpperCase

		def apply[P, V](left :Query[P, V], right :Query[P, V]) :CompoundSelect[P, V] =
			CompoundSelect(left, this, right)

		def apply[P, M[O] <: MappingAt[O]]
		         (left :MappingQuery[P, M], right :MappingQuery[P, M]) :CompoundSelectMapping[P, M] =
			CompoundSelectMapping(left, this, right)


		def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V]) :CompoundSelectSQL[F, V] =
			CompoundSelectSQL(left, this, right)

		def apply[F <: RowProduct, V](left :ColumnQuery[F, V], right :ColumnQuery[F, V]) :CompoundSelectColumn[F, V] =
			CompoundSelectColumn(left, this, right)

		def apply[F <: RowProduct, M[O] <: MappingAt[O]]
		         (left :MappingQuerySQL[F, M], right :MappingQuerySQL[F, M]) :CompoundSelectAs[F, M] =
			CompoundSelectAs(left, this, right)

		def apply[F <: RowProduct, M[O] <: BaseColumn[V, O], V]
		         (left :ColumnMappingQuery[F, M, V], right :ColumnMappingQuery[F, M, V]) :CompoundSelectColumnAs[F, M, V] =
			CompoundSelectColumnAs(left, this, right)

		/** Default reform strategy used to unify ''select'' clauses of the left and right side
		  * of a [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]/[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
		  * joined with this operator. This strategy is used if the reformed ''compound select'' is actually
		  * a subterm of a larger ''compound select''. This reform is used when the strategy for the whole query
		  * is implemented in a 'bottom-up' manner: the left and right sides of a ''compound select''
		  * are reformed independently before being unified with each other. 'Top-down' reforming strategies
		  * do not rely on this property, instead returning an instance of their own type.
		  *
		  * It is the default implementation of
		  * `CompoundSelect.`[[net.noresttherein.oldsql.sql.CompoundSelect.defaultReform defaultReform]]
		  * and `CompoundSelectSQL.`[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.defaultReform defaultReform]],
		  * and those methods should be used instead.
		  * @see [[net.noresttherein.oldsql.sql.Select.SelectOperator.topReform]]
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform]]
		  */ //consider: adding arguments like in topReform. Inconvenient because it is overriden by vals
		def defaultReform :QueryReform

		/** Default reform strategy used to unify ''select'' clauses of two queries joined with this operator
		  * in a ''compound select'' representing the whole formatted statement/expression.
		  * While the default implementation simply returns
		  * [[net.noresttherein.oldsql.sql.Select.SelectOperator.defaultReform defaultReform]],
		  * this distinction is created with ''union all'' ''compound selects'', which add a synthetic discriminator
		  * column specifying from which of the member ''selects'' any given row comes from, which should not
		  * be duplicated. Additionally, reforming of a top-level
		  * [[net.noresttherein.oldsql.sql.ast.SelectColumn SelectColumn]] is guaranteed to always return a single
		  * column query; this might not be the case if such a query is unified with ''selects'' for multiple columns.
		  *
		  * It is the default implementation of
		  * `CompoundSelect.`[[net.noresttherein.oldsql.sql.CompoundSelect.topReform topReform]]
		  * and `CompoundSelectSQL.`[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.topReform topReform]],
		  * and those methods should be used instead.
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform]]
		  */
		def topReform :QueryReform = defaultReform

		/** Alters the argument `reform` by [[net.noresttherein.oldsql.sql.mechanics.Reform.prohibit prohibiting]]
		  * certain reforming actions. This varies between operators, following the principle of least surprise.
		  */
		def prevent(reform :QueryReform) :QueryReform = reform.prohibit(allowedReforms)

		/** Additional restrictions on column set modification for both left and right sides of a ''compound select''
		  * with this operator, applied during [[net.noresttherein.oldsql.sql.Query.QueryReformingTemplate.reformed reforming]].
		  */
		def allowedReforms :Permissions

		override def toString :String = name
	}


	//consider: We need to know from which select each row comes in order to use a proper form for reading.
	//  However, we can't use discriminators as in UnionAll, because it obviously will no longer be a union.
	//  An exception to the rule would be if we added null columns, as a row with null column will equal no other row.
	//  Of course, we might opt to use an 'application null' in place of nulls in order to enforce union semantics.
	//  Otherwise, the only option we have is to do a UnionAll and then group by the row key
	//  (all columns unless we can use determine that each relation in its from clause has a primary key),
	//  and use min(discriminator) (or whatever) to select a single discriminator for equal rows.
	/** A union operator combining two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). It follows the semantics of set union,
	  * with no duplicate rows in the returned result set.
	  */ //consider: we could add a discriminator, do union all, group by (not supported by clobs/blobs)
	object Union extends SelectOperator("union") { //can we just go with the assembler of the first query?
		//fixme: discriminator? equal null values?
		override val allowedReforms :Permissions = !Permissions.MayAddNull
		override val defaultReform  :QueryReform = QueryReform.bottomUp(allowedReforms)
	}
//	final val Union = new SelectOperator("union")

	//  1. mapping vs mapping:
	//      1. nominal mappings are identical (and effective mappings are homomorphic) - use the first mapping for all rows
	//          1. if a component is default, add missing columns, retaining those excluded from the other;
	//          1. otherwise add a null column for every extra column in the other mapping
	//      1. mappings are homomorphic
	//          1. a mapping is default, add missing columns, but retain those excluded from the other
	//          1. otherwise, add null columns
	//      1. mappings share a column extract subset
	//          - add a discriminator column, if not already present
	//          - for every missing column on either side, add the local counterpart,
	//            if it exists and the mapping is default; otherwise add a null column
	//      1.
	/** A union operator combining two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). This is a multiset variant, with every row
	  * from either of the ''selects'' mapping to a single row in the returned result set; if a row is present
	  * in both ''selects'' (or it has duplicates within either of them), each occurrence will be represented by
	  * a separate row in the result.
	  */ //with ... as select1, ... as select2 select
	object UnionAll extends SelectOperator("union all") {
		override val allowedReforms :Permissions = Permissions.MayReform
		override val defaultReform  :QueryReform = QueryReform.bottomUp
		override val topReform      :QueryReform = UnionAllReform.bottomUp
	}

	//always use the header of the first one; standard ReformDefaults.reform(_, _)(false, true) (for all selects if the second operand is a compound select)
	/** An operator implementing set difference between two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). The query created will return every row
	  * from the first (left) argument which is not present in the second (right) argument.
	  */
	object Minus extends SelectOperator("minus") {
		override val allowedReforms :Permissions = Permissions.MayReformRight
		override val defaultReform  :QueryReform = QueryReform.bottomUp(allowedReforms)
	}

	//Same reform as in SQLExpression
	/** An operator implementing set intersection of two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). The query created will return all rows
	  * which are present in both of its arguments, with every row occurring exactly once, regardless of the number
	  * of duplicates in the input ''selects''.
	  */
	object Intersect extends SelectOperator("intersect") {
		override val allowedReforms :Permissions = !Permissions.MayAddNull
		override val defaultReform  :QueryReform = QueryReform.bottomUp(allowedReforms)
	}




	/** A template for classes representing SQL ''selects'' - both standard SQL
	  * [[net.noresttherein.oldsql.sql.ast.SelectSQL expressions]] and
	  * [[net.noresttherein.oldsql.sql.Select parameterized]] ''selects'', including their compound forms.
	  * @tparam V value type representing the whole ''select'' clause, used as its return type and
	  *           [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] value type.
	  * @tparam Q the self type of this interface, that is the whole public type of the ''select'' parameterized
	  *           with its value type `V`.
	  */ //consider: extending ReorderingTemplate
	trait SelectTemplate[V, +Q[_], +Reformed] extends SingleQueryTemplate[V, Q, Reformed] { this :Q[V] with Reformed =>
//		type Domain >: From <: RowProduct
		/** The from clause of this select.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct]]
		  */
		type From <: RowProduct //{ type Generalized = Domain }

		/** A synthetic mapping type for [[net.noresttherein.oldsql.sql.Select.SelectTemplate.columns columns]]
		  * of this ''select''. These columns are ''not'' necessarily the columns of
		  * `this.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.mapping mapping]]
		  * or `this.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.export export]],
		  * but rather wrappers over [[net.noresttherein.oldsql.sql.ColumnSQL expressions]] for individual
		  * columns in `this.`[[net.noresttherein.oldsql.sql.Select.SelectTemplate.selectClause selectClause]],
		  * as would be returned by `selectClause.`[[net.noresttherein.oldsql.sql.SQLExpression.split split]].
		  */
		type SelectedColumn[X] = TypedColumnSQLMapping[From, Grouped, X, this.type]

		/** The ''from'' and ''where'' clause of this SQL ''select''
		  * (optionally including also ''group by'' and ''having'' clauses).
		  */
		val from :From

		/** An expression serving as the ''select'' clause of this SQL ''select''.
		  * If it is not a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]], all its individual columns are inlined
		  * when formatting the final SQL statement. The expressions for those individual columns are made available
		  * as [[net.noresttherein.oldsql.sql.TypedColumnSQLMapping.expr expr]] property of the columns listed by
		  * `this.`[[net.noresttherein.oldsql.sql.Select.SelectTemplate.columns columns]].
		  */
		override val selectClause :SQLExpression[From, Grouped, V]
		override def columns :Seq[SelectedColumn[_]]

		//consider: allowing select clauses with other value types
//		@deprecated("this method cannot be implemented properly in SelectColumn", "0.0")
//		def selectOther(selectClause :SQLExpression[From, Grouped, V]) :Q[V]

//		override def withClause :WithClause = selectClause.outerWithClause ++ from.withClause
		//caution: in group by queries this returns the elements of the group by clause, not the actual from clause
		def relations :Seq[RelationSQL.from[from.Generalized]#__] = from.tableStack.reverse //todo: remove the explicit type in Scala 3
		def tables    :Seq[Relation.__] =
			(from.fromClause.tableStack :Iterable[RelationSQL.__]).mapReverse(_.relation :Relation[MappingAt]).toSeq

		def isDistinct :Boolean //consider: making this and others return the same From type
		def distinct   :Q[V]// { type RowMapping[O] <: SelectTemplate.this.RowMapping[O] }
		//todo: order by, top, limit


		protected override def allowedReforms(implicit spelling :SQLSpelling) :Permissions = MayReform

		protected override def shape(implicit spelling :SQLSpelling) :RowShape =
			spelling.inSelect.shape(selectClause)

		protected override def columnCount(implicit spelling :SQLSpelling) :Int =
			spelling.inSelect.columnCount(selectClause)

		/** A lower-level variant of standard [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]]
		  * method of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
		  * and [[net.noresttherein.oldsql.sql.Query Query]] expressed in terms of
		  * the ''from'' clause of this select, rather than the base clause of this expression (an outer clause of `from`),
		  * with separate parameter getters for the ''from''/''where'' and ''select'' clauses of this instance.
		  * It is used in implementation of the former by
		  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]],
		  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]],
		  * and [[net.noresttherein.oldsql.sql.Select Select]].
		  */
		protected def defaultSpelling[P](from :From, context :SQLContext[P])
		                                (selectParams :Parameterization[P, From], fromParams :Parameterization[P, from.Self])
		                                (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		{
			val withClause = if (spelling.scope == TopScope) this.withClause else localWithClause
			val withSQL =
				if (withClause.isEmpty) SpelledSQL(context)
				else spelling.inWith(withClause)(context, Parameterization.paramless) + " "
			val fromSQL = from.spell(withSQL.context, fromParams)
			val selectSpelling = spelling.inSelect
			val selectSetter =
				if (selectSpelling.sqlParamCount(selectClause) == 0) SQLWriteForm.empty[P]
				else selectSpelling(selectClause)(from, fromSQL.context, selectParams).setter
			val selectSQL = this.columns.scanLeft(SpelledSQL(fromSQL.context)) {
				(prev, sqlColumn) => selectSpelling.select(sqlColumn)(from, prev.context, selectParams)
			}.tail.reduce(_ + ", " + _)
			val setter = selectSetter + fromSQL.setter
			val resultContext = selectSQL.outContext(context) //allow selectSQL.context to pass back information
			if (fromSQL.sql.isEmpty)
				withSQL + SpelledSQL(spelling.SELECT_ + selectSQL.sql, setter, resultContext)
			else
				withSQL + SpelledSQL(spelling.SELECT_ + selectSQL.sql + " " + fromSQL.sql, setter, resultContext)
		}

		override def toString :String =
			if (isDistinct) s"SELECT DISTINCT $selectClause FROM $from"
			else  s"SELECT $selectClause FROM $from"
	}



	type __ = Select[_, _]

//	type TypedSelect[P, M[O] <: MappingAt[O], V] = Select[P, V] { type RowMapping[O] <: M[O] }
//	type SelectMapping[P, M[O] <: MappingAt[O]] = Select[P, M[Unit]#Subject] { type RowMapping[O] <: M[O] }

	//consider: moving it out, as is SelectAs
	/** A parameterized ''select'' interface exposing the mapping type `H` used for the ''select'' clause. */
	trait SelectMapping[P, H[A] <: MappingAt[A]] extends Select[P, H[Unit]#Subject] with SingleMappingQuery[P, H] {
		override type RowMapping[O] = H[O]

		override def distinct :SelectMapping[P, H]

		override def bind(params :P) :TopSelectAs[H]

//		def selectOther(selectClause :MappingSQL[From, Grouped, H, H[Unit]#Subject])
	}


	object SelectMapping {
		type __ = SelectMapping[_, M] forSome { type M[A] <: MappingAt[A] }
	}




	private class SelectComponent[P, F <: TopRow { type Complete <: F ; type Params = P }, M[A] <: BaseMapping[V, A], V]
	                             (override val from :F, override val selectClause :ComponentSQL[F, M],
	                              override val isDistinct :Boolean = false)
		extends SelectMapping[P, M]
	{
		override type From = F
//		override type RowMapping[O] = M[O]

		override def mapping[O] :RowMapping[O] = selectClause.mapping.withOrigin[O]
		override def export[O] :TypedMapping[V, O] = selectClause.anchored.withOrigin[O]

		override val withClause = from.withClause ++ selectClause.outerWithClause

		override val columns: Seq[TypedColumnSQLMapping[F, Grouped, _, this.type]] =
			selectClause.export.selectedByDefault.toSeq.map(include(_))

		private def include[X](column :TypedColumn[X, selectClause.Origin]) :SelectedColumn[X] =
			TypedColumnSQLMapping(selectClause \ column)

		override def distinct :SelectMapping[P, M] =
			if (isDistinct) this
			else new SelectComponent[P, F, M, V](from, selectClause, true)

		override def bind(params :P) :TopSelectAs[M] = {
			val paramless = from.bind(params).asInstanceOf[Complete[GroundRow]]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
			val select = header.asInstanceOf[ComponentSQL[Complete[GroundRow], M]] topSelectFrom paramless
			if (isDistinct) select.distinct else select
		}
	}




	/** A select expression based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns.
	  */
	private[sql] abstract
	class BaseArbitrarySelect[P, F <: TopRow { type Complete <: F ; type Params = P }, V] protected
	                         (override val from :F, protected val result :TypedSQLMapping[F, Grouped, V, Unit])
		extends Select[P, V]
	{
		def this(from :F, header :SQLExpression[F, Grouped, V]) =
			this(from, TypedSQLMapping[F, Grouped, V, Unit](header, SelectView))

//		override type Domain = G
		override type From = F

		override val selectClause = result.expr
		override val withClause   = from.withClause ++ selectClause.outerWithClause
		override val columns :Seq[TypedColumnSQLMapping[F, Grouped, _, this.type]] =
			result.columns.toSeq.withOrigin[this.type]

		override def bind(params :P) :TopSelectSQL[V] = {
			val paramless = from.bind(params).asInstanceOf[Complete[GroundRow]]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
			val select = header topSelectFrom paramless
			if (isDistinct) select.distinct else select
		}
	}


	private[sql] trait ArbitrarySelectTemplate[P, F <: TopRow { type Complete <: F ; type Params = P },
	                                           M[O] <: BaseMapping[V, O], V]
		extends Select[P, V]
	{ this :BaseArbitrarySelect[P, F, V] =>
		override type RowMapping[O] = M[O]

		protected val result :M[Unit]
		override def mapping[O] :M[O] = (this :ArbitrarySelectTemplate[P, F, M, V]).result.withOrigin[O]
		override def export[O]  :M[O] = mapping[O]
//		override def export[O] :M[O] = mapping[O]
	}




	private[sql] def ArbitrarySelect[P, F <: TopRow { type Complete <: F ; type Params = P }, V]
	                                (from :F, selectClause :SQLExpression[F, Grouped, V], isDistinct :Boolean = false)
			:Select[P, V] =
		new ArbitrarySelect[P, F, V](from, TypedSQLMapping[F, Grouped, V, Unit](selectClause, SelectView), isDistinct)

	private[sql] class ArbitrarySelect[P, F <: TopRow { type Complete <: F ; type Params = P }, V]
	                   (override val from :F, protected override val result :TypedSQLMapping[F, Grouped, V, Unit],
	                    override val isDistinct :Boolean)
		extends BaseArbitrarySelect[P, F, V](from, result)
			with ArbitrarySelectTemplate[P, F, TypedSQLMapping.c[F]#c[Grouped]#c[V]#project, V]
	{
		override def distinct :Select[P, V] =
			if (isDistinct) this else new ArbitrarySelect[P, F, V](from, result, true)
	}


	private[sql] def SelectListing[P, F <: TopRow { type Complete <: F ; type Params = P }, V <: Listing]
	                              (from :F, selectClause :LabeledValueSQL[F, Grouped, V], isDistinct :Boolean = false)
			:SelectMapping[P, IndexedMapping.of[V]#Mapping] =
		new SelectListing[P, F, V](from, selectClause.listingMapping[Unit], isDistinct)

	private[sql] class SelectListing[P, F <: TopRow { type Complete <: F ; type Params = P }, V <: Listing]
	                  (override val from :F, override val result :TypedListingSQLMapping[F, Grouped, V, Unit],
	                   override val isDistinct :Boolean)
		extends BaseArbitrarySelect[P, F, V](from, result)
			with ArbitrarySelectTemplate[P, F, IndexedMapping.of[V]#Mapping, V]
			with SelectMapping[P, IndexedMapping.of[V]#Mapping]
	{
		override val selectClause = result.expr

		override def distinct :SelectMapping[P, IndexedMapping.of[V]#Mapping] =
			if (isDistinct) this else new SelectListing[P, F, V](from, result, true)

		override def bind(params :P) :TopSelectAs[IndexedMapping.of[V]#Mapping] = {
			val paramless = from.bind(params).asInstanceOf[Complete[GroundRow]]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
				.asInstanceOf[IndexedSQL[Complete[GroundRow], Grouped, V]]
			val select = header topSelectFrom paramless
			if (isDistinct) select.distinct else select
		}
	}


	private[sql] def SelectListingColumn[P, F <: TopRow { type Complete <: F ; type Params = P }, A <: Label, V]
	                 (from :F, selectClause :LabeledColumnSQL[F, Grouped, A, V], isDistinct :Boolean = false)
			:SelectMapping[P, IndexedMapping.of[V]#Column] =
		new SelectListingColumn[P, F, A, V](from, selectClause.listingMapping, isDistinct)

	private[sql] class SelectListingColumn[P, F <: TopRow { type Complete <: F ; type Params = P }, A <: Label, V]
	                   (override val from :F, override val result :TypedListingColumnSQLMapping[F, Grouped, A, V, Unit],
	                    override val isDistinct :Boolean)
		extends BaseArbitrarySelect[P, F, V](from, result)
		   with ArbitrarySelectTemplate[P, F, IndexedMapping.of[V]#Column, V]
		   with SelectMapping[P, IndexedMapping.of[V]#Column]
	{
		override val selectClause = result.expr :ColumnSQL[result.Domain, result.Scope, V]

		override def distinct :SelectMapping[P, IndexedMapping.of[V]#Column] =
			if (isDistinct) this
			else new SelectListingColumn[P, F, A, V](from, result, true)

		override def bind(params :P) :TopSelectAs[IndexedMapping.of[V]#Column] = {
			val paramless = from.bind(params).asInstanceOf[Complete[GroundRow]]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
				.asInstanceOf[LabeledColumnSQL[Complete[GroundRow], Grouped, A, V]]
			val select = header topSelectFrom paramless
			if (isDistinct) select.distinct else select
		}
	}

}

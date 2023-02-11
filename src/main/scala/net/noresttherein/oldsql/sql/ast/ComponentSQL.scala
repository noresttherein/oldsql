package net.noresttherein.oldsql.sql.ast

import scala.collection.mutable

import net.noresttherein.oldsql.collection.{Listing, Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{Bug, IncompatibleMappingsException, MismatchedExpressionsException, NoSuchComponentException}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMappingExtract, ColumnReadForm, Mapping, MappingExtract, Relation, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.support.{PatchedMapping, ReorderedMapping}
import net.noresttherein.oldsql.sql.{Adjoin, ColumnSetter, ColumnSQL, Expanded, RowProduct, RowShape, Seal, Select, SQLExpression, WithClause}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, GroundRow, NonEmptyRow, PrefixOf, ProperSubselectOf, TopRow}
import net.noresttherein.oldsql.sql.Sealed.seal
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.ast.LValueSQL.LValueTemplate
import net.noresttherein.oldsql.sql.ast.ComponentSQL.{GenericComponentSQLTemplate, InvariantComponentSQLTemplate, TypedComponentSQL}
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL.{AnyColumnComponentVisitor, CaseAnyColumnComponent, CaseSpecificColumnComponent, SpecificColumnComponentVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate
import net.noresttherein.oldsql.sql.ast.RelationSQL.{AnyRelationVisitor, CaseAnyRelation, CaseSpecificRelation, SpecificRelationVisitor}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.mechanics.{=~=, Reform, RelationCount, RelationOffset, SpelledSQL, SQLConversion, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}

//implicits
import net.noresttherein.oldsql.slang.mappingMethods






/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] representing a whole, potentially multi column,
  * component mapping of a [[net.noresttherein.oldsql.schema.Relation Relation]] from `RowProduct` clause `F`.
  * Note that the 'relation' here might be a [[net.noresttherein.oldsql.schema.Table Table]] from a ''from'' clause,
  * but also other constructs adapted to this interface, such as
  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation expressions]] in a ''group by'' clause
  * or a query [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation parameter]]. The type of relation
  * - and the wrapping relation expression [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] -
  * defines how this component is formatted in the rendered SQL. For tables (including derived tables of
  * inline ''select'' expressions), this is typically a list of `s"$tableAlias.${column.name}"` expressions for
  * all pertinent columns of the component; unbound parameters are translated to JDBC parameter placeholders '?'
  * in a number defined by their [[net.noresttherein.oldsql.schema.SQLForm forms]], while a component
  * of a `GroupingRelation` (representing a usage of a subexpression of an expression used in query's ''group by''
  * clause) will repeat all individual column expressions as they appeared in the ''group by'' clause.
  *
  * The expression normally translates to a list of subexpressions for individual columns separated with ','
  * and optionally surrounded by '(' and ')', depending on the context. In some circumstances however the
  * individual column expressions may be split, for example in column-wise equality comparisons between components.
  *
  * Exact column set used for a component expression for mapping `M` may differ, especially in the case
  * of table components, and depends on three factors:
  *   1. The [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope SpellingScope]], that is the context in which
  *      the expression is used, for example for one of the four
  *      SQL [[net.noresttherein.oldsql.OperationView operation types]] and changes depending on the fragment
  *      of the SQL (in what type of expression and clause this expression is included).
  *   1. Explicit requests to use certain column or subcomponents for this particular expression by calling one of
  *      [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]],
  *      [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.exclude exclude]],
  *      [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.alter alter]],
  *      [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.+- +-]] methods on this expression;
  *   1. Adjustments to the included columns made for the whole source `Relation` (typically before joining it
  *      into ''from'' clause `F` (or added to it by other methods/clauses) - these changes will typically apply
  *      to all component expressions referencing the relation within the whole ''select''.
  *
  * It is defined as [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling spelling]]`.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]]`.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns[o](mapping:MappingAt[O],component:MapppingAt[O])* defaultColumns]]`(this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.export export]]`, this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]`)`.
  * Note that semantically, a component expression is a placeholder referencing a certain component of a relation
  * declared by the statement/query in which the expression is embedded: when rendering SQL for a statement,
  * this expression is formatted in the context of the `RowProduct` used as the ''from'' clause/domain of the whole
  * statement. This means that any information from the `Relation` instance passed during formatting overrides
  * any corresponding information in this component; for tables, the exact column set is the sum
  * of changes applied by said relation and the alterations to this expression stored as
  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]] and
  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] properties of its
  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]. For components
  * of grouping expressions however, the whole expression in the ''group by'' clause is substituted during
  * SQL formatting in case of a discrepancy, which is also reflected in the component's representation,
  * as it is understood as a subset of columns added to the ''group by'' clause by its `origin` grouping expression.
  *
  * Every concrete implementation of this trait is required to extend also
  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]],
  * which lifts several member types of the former to type parameters. This split is motivated by free reasons:
  *   1. A wish for a clean, minimal interface for the majority of client code,
  *   1. The need for mapping `M` to have a well defined
  *      [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.Subject Subject]] type
  *      in order to implement some methods of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], and
  *   1. The need for various modify/copy operations,
  *      such as [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]],
  *      to preserve the type information about the origin relation in order to implement certain operations
  *      working on component expressions.
  *
  * Additionally, if `M[O] <: `[[net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn TypedColumn]]`[_, O]`,
  * then the component expression
  * should also be a [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL GenericColumnComponentSQL]]
  * (and, for the same reasons,
  * [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL TypedColumnComponentSQL]])
  *
  * @tparam F $F
  * @tparam M $M
  * @define M    a type constructor for the nominal [[net.noresttherein.oldsql.schema.Mapping Mapping]] type
  *              of this component, accepting its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type
  *              as the type parameter. Note that this is the interface type of the component as defined,
  *              but the enclosing mapping of the origin relation might use a different mapping
  *              as its ''export''. Furthermore, this expression can represent various views on a component,
  *              including or excluding some of its optional columns. This has the effect of changing
  *              the actual mapping used, which might be any proxy to this mapping (preserving its subject type).
  * @define this component
  * @define Cons `ComponentSQL`
  * @define link [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.JoinedRelation]]
  */
trait ComponentSQL[-F <: RowProduct, M[A] <: MappingAt[A]]
	extends LValueSQL[F, M, M[Unit]#Subject] with SelectableMappingSQL[F, M]
	   with LValueTemplate[F, M, M[Unit]#Subject, ({ type E[f <: RowProduct] = ComponentSQL[f, M] })#E, ComponentSQL[F, M]]
	   with GenericComponentSQLTemplate[F, M, ({ type E[f <: RowProduct] = ComponentSQL[f, M] })#E, ComponentSQL[F, M]]
{ self =>

	/** @inheritdoc
	  * @return [[net.noresttherein.oldsql.sql.ast.ComponentSQL.anchored anchored]]`.`[[net.noresttherein.oldsql.schema.Mapping.selectForm selectForm]],
	  */
	override def selectForm :SQLReadForm[M[Unit]#Subject] = origin.anchored.selectForm(anchored)
	override def conversion :SQLConversion[M[Unit]#Subject, M[Unit]#Subject] = SQLConversion.toSelf

	/** Returns `this`. */
	override def component  :ComponentSQL[F, M] = this

	/** Creates an expression for the given subcomponent of this component.
	  * Passing a component of [[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]] mapping
	  * rather than `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]] will result
	  * in a [[NoSuchElementException NoSuchElementException]] being thrown.
	  */
	@throws[NoSuchComponentException]("if component is not a component of this.mapping.")
	override def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
			:ComponentSQL[F, project.WithOrigin] {
				type Origin = self.Origin; type Entity[A] = self.Entity[A]//; type FromLast = self.FromLast
			}

	/** Creates an expression for the given column of this component.
	  * Passing a column of [[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]] mapping
	  * rather than `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]] will result
	  * in a [[NoSuchElementException NoSuchElementException]] being thrown.
	  */
	@throws[NoSuchComponentException]("if component is not a component of this.mapping.")
	override def \[K <: ColumnAt[Origin], X]
	     (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
			:GenericColumnComponentSQL[F, project.WithOrigin, X] {
				type Origin = self.Origin; type Entity[A] = self.Entity[A]//; type FromLast = self.FromLast
			}

	/** Creates an expression for the given subcomponent of this component. The component is given as a function
	  * accepting the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]] of this instance as its
	  * argument, and returning one of its properties. The result is always a subtype of
	  * `ComponentSQL[F, project.WithOrigin] { type Origin = self.Origin; type Entity[O] = self.Entity[O] }`, where
	  * `project` is an implicit [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]]`[K, _]`.
	  * If `K` is a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]], then the result will
	  * be the specialized subtype [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL ColumnMappingSQL]].
	  *
	  * Passing a component of [[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]] mapping
	  * rather than `this.mapping` will result in a [[NoSuchElementException NoSuchElementException]] being thrown.
	  */
	@throws[NoSuchComponentException]("if component is not a component of this.mapping.")
	def \[K <: MappingAt[Origin]]
	     (component :M[Origin] => K)
	     (implicit factory :ComponentSQL.Factory[K]) :factory.Result[F]

	/** A component expression is always anchored in ''some'' [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
	  * It doesn't however yet allow to determine how it will be represented in SQL (the selected column set,
	  * including their names): for that, an expression must be
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isAnchored(from:F) anchored]]
	  * in the ''from'' clause `F` particular to the formatted query/statement.
	  * @return `true`.
	  */
	override def isAnchored = true

	override def isAnchored(from :F) :Boolean = origin.isAnchored(from)

	override type isSelectable = true

	override def outerWithClause :WithClause = origin.outerWithClause


//
//	protected override def reform[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                             (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[Subject, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(reform.LValue[F, M, U], SQLExpression[E, C, U]) =
//		if (passesAllowed > 1) //pass only if they can pass back, because we can try to reform ourselves.
//			super.reform(other)(reform, passesAllowed)
//		else
//			super.reform(other)(reform, passesAllowed)
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
//	(other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
//	(implicit leftResult :Lift[Subject, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(reform.LValue[F, M, U], reform.LValue[E, C, U]) =
//		if (passesAllowed > 0)
//			super.reform(other)(reform, passesAllowed)
//		else
//			super.reform(other)(reform, passesAllowed)
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], U]
//	                             (other :ComponentSQL[E, C])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[Subject, U], rightResult :Lift[other.Subject, U],
//	                              spelling :SQLSpelling)
//			:(reform.LValue[F, M, U], reform.LValue[E, C, U]) =
//		if (passesAllowed > 0)
//			super.reform(other)(reform, passesAllowed)
//		else
//			(leftResult(this), rightResult(other)) match {
////				case (l, r) if spelling.columnCount(l) == spelling.columnCount(r) =>
////					(reform(l), reform(r))
//				case (l, r) if !leftResult.isDerived || !rightResult.isDerived => //either l or r aren't ConversionSQL
//					reform(l, r)
//				case _ =>
//					implicit val unification = SQLTypeUnification(leftResult, rightResult)
//					reform(this, other)
//			}
//
//	protected override def reformer[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                               (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                               (implicit leftResult :Lift[Subject, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:SpecificExpressionVisitor[E, C, X, (reform.LValue[F, M, U], SQLExpression[E, C, U])] =
//		new ComponentReformer[reform.LValue, E, C, X, U](other)(reform, passesAllowed)

	//fixme: SQLTransformation.SQLResult is invariant, but we return an LValueSQL[F1, M, U] which has Single scope, not S1
	protected override def reformer[F1 <: F, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                   (implicit leftResult  :SQLTransformation[M[Unit]#Subject, U],
                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.SQLResult[F1, S1, LValueSQL[F1, M, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
		new ComponentReformer[F1, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](
		                      other)(reform, passCount)(leftResult, rightResult, spelling)


	protected class ComponentReformer[F1 <: F, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                                  EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U,
	                                  LR[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U],
	                                  RR[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U]]
	                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                                 (implicit leftResult  :SQLTransformation[M[Unit]#Subject, U]#Into[LR],
	                                           rightResult :SQLTransformation[V2, U]#Into[RR], spelling :SQLSpelling)
		extends BaseReformer[F1, Single, F2, S2, V2, EC2, U, LR, RR](
		                     other)(reform, passCount)(leftResult, rightResult, spelling)
	{
		override def component[O >: F2 <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V2, A], L <: RowProduct]
		                      (e :TypedComponentSQL[O, T, R, M, V2, L]) :Result =
			if (!passCount.lastChance) pass
			else reform.default(ComponentSQL.this, e)(leftResult, rightResult, spelling)

		override def term(e :SQLTerm[V2]) :Result =
			if (passCount.secondTime && reform.mayAlterRight && rightResult.isReversible) {
				try {
					val form = spelling.scope.termForm(origin.anchored, anchored)
					//todo: support truncation of column sets/forms with the upcast to U
					(leftResult(ComponentSQL.this), e.reform(leftResult(form), rightResult))
				} catch {
					case ex @ (_ :MismatchedExpressionsException | _ :UnsupportedOperationException) => try {
						fallback
					} catch {
						case e :Exception => ex.addSuppressed(e); throw ex
					}
				}
			} else
				fallback

		//todo: reforming with all mapping terms
		override def mappingTerm[T[A] <: BaseMapping[V2, A]](e :MappingTerm[T, V2]) :Result =
			if (passCount.secondTime)
				if (reform.mayAlterRight && rightResult.isReversible)
					term(e)
				else if (ComponentSQL.this.mapping isomorphic e.mapping)
					if (reform.mayAlterLeft)
						???
					else if (reform.mayAlterRight)
						???
					else
						fallback
				else
					fallback
			else
				fallback

		//todo: tuple with component/column items

		//todo: for IndexedSchemaMapping match the listing keys in the expression with component labels
		//todo: match keys with property names from extracts as an alternative to column names.
		override def labeled[L <: Listing](e :LabeledSQL[F2, S2, L])(implicit isListing :V2 =:= L) :Result =
			if (!passCount.secondTime)
				fallback
			else {
				val rightRes = isListing.substituteCo[({ type T[A] = SQLTransformation[A, U]#Into[RR] })#T](rightResult)
				//todo: compare property names, too
				//todo: support non flat listings
				//todo: support reordering. tough with non flat listings
				val defaultColumns = spelling.scope.defaultColumns(origin.anchored, anchored)
				val items = e.items
				// the shapes of e and this component match, and the keys correspond exactly to default column names
				val labelsMatchDefaultColumns =
					defaultColumns.size == items.length && defaultColumns.zipForAll(items) {
						(column, item) => column.name == item.key && column.form.shape <:> spelling.shape(item.value)
					}
				//labelsMatchDefaultProperties =

				//all items in e are columns with names matching those of default columns of this component,
				// but in a different order
				def labelsMatchReorderedDefaultColumns =
					defaultColumns.size == items.length && defaultColumns.forall { column =>
						e.toMap.get(column.name) match {
							case Some(value) => spelling.shape(value) <:> column.form.shape //consider: using reform.compatible
							case _ => false
						}
					}
				if (labelsMatchDefaultColumns)
					asIs
				else if ((reform.mayAlterRight || reform.mayReorderLeft && !isFinal) && labelsMatchReorderedDefaultColumns) {
					if (reform.mayReorderRight) {
						val reordered = e.reorder(defaultColumns.map(_.name))
						(leftResult(ComponentSQL.this), rightResult(reordered.asInstanceOf[Convertible[F2, S2, V2, EC2]]))
					} else {
						val columnsInKeyOrder = e.keys.view.map(anchored.columnNamed) to Unique
						val mappingColumns = mapping.columns.view.map {
							c => columnsInKeyOrder.indexOf(anchored.export(c)) -> c
						}.filter(_._1 >= 0).sortBy(_._1).map(_._2) to Unique
						(leftResult(alter(mappingColumns)), rightResult(other))
					}
				} else if (!isFinal) try { //todo: fine grained control of include, exclude, reorder, add null
					//include, exclude and reorder columns of this component so that they match e.items (including names)
					val keyOrder = items.view.map(_.key) to Unique
					val columnsByName = spelling.scope.applicableColumns(origin.anchored, anchored).groupBy(_.name).map {
						case (name, Unique(column)) => name -> column
						case (name, columns) =>
							throw Bug("Multiple columns named '" + name + "' in mapping " + anchored + ": " + columns + ".")
					}
					//todo: this is were we'll branch to match keys to property names
					val counterparts = items.view.map { item =>
						val column = columnsByName.getOrElse(item.key,
							throw new MismatchedExpressionsException(ComponentSQL.this, e,
								"no column named " + item.key + " in " + anchored + " allowed in scope " +
									spelling.scope + ": " + columnsByName
							)
						)
						if (!(column.form.shape <:> spelling.shape(item.value)))
							throw new MismatchedExpressionsException(ComponentSQL.this, e,
								"the form " + column.form + " of column " + item.key + " from " + anchored +
									" does not match the form " + item.value.selectForm + " of expression " +
									item.value + " under the same name in the right record."
							)
						column
					} to Unique
					val excludes = defaultColumns.filterNot(counterparts.contains)
					if (excludes.exists(spelling.scope.isMandatory)) {
						val missing = excludes.filter(spelling.scope.isMandatory)
						throw new MismatchedExpressionsException(ComponentSQL.this, e,
							"the right expression misses values for mandatory columns " + missing +
								" of " + anchored + "."
						)
					} else if (excludes.nonEmpty && !reform.mayExcludeLeft)
						throw new MismatchedExpressionsException(ComponentSQL.this, e,
							"the right expression misses values for some default columns of " + anchored + ": "
								+ excludes + "."
						)
					val includes = counterparts.filterNot(defaultColumns.contains)
					if (includes.nonEmpty && !reform.mayIncludeLeft)
						throw new MismatchedExpressionsException(ComponentSQL.this, e,
							"the right expression contains values for some non default columns of " + anchored + ": "
								+ includes + "."
						)
					val reformed = alter(includes, excludes)
					val reformedColumns = spelling.scope.defaultColumns(reformed.origin.anchored, reformed.anchored)
					if (reformedColumns.view.map(_.name) == keyOrder)
						(leftResult(reformed), rightResult(other))
					else {
						if (reformedColumns.size != expectedColumnOrder.size)
							throw new MismatchedExpressionsException(ComponentSQL.this, e,
								"Columns of " + reformed.anchored + " of expression " + reformed +
									" reformed from " + ComponentSQL.this + " by including " + includes +
									" and excluding " + excludes + " do not match the entries in the right expression. " +
									"This is a bug."
							)
						if (reform.mayReorderLeft) {
							val nominalColumnsByName =
								mapping.columns.view.map(c => (origin.anchored.export(c.name), c)).toMap
							val nominalColumns = e.keys.view.map(nominalColumnsByName) to Unique
							val permutation = ReorderedMapping.subseqPermutation(mapping.columns, nominalColumns)
							val reordered = reformed.reorder(permutation)
							(leftResult(reordered), rightResult(other))
						} else if (reform.mayReorderRight) {
							val reordered = e.reorder(reformedColumns.map(_.name))
							(leftResult(reformed), rightResult(reordered.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
						} else
							throw new MismatchedExpressionsException(ComponentSQL.this, e,
								"Reform " + reform + " allows reordering of neither columns of " + reformed.anchored +
									" reformed from the left expression, nor of " + e +
									" in order to match column names with item keys."
							)
					}
				} catch {
					case e :MismatchedExpressionsException => fallbackAfter(e)
					case e :UnsupportedOperationException  => fallbackAfter(e)
				} else
					fallback
//				(leftResult(ComponentSQL.this), rightRes(e)) match {
//					case (l, r) if !leftResult.isDerived || !rightResult.isDerived =>
//						reform(l, r)
//					case (l, r) if unified =>
//						(reform.lift(l), r)
//
//					case (l, _) if reform.mayReorderRight && reorderedColumns =>
//						//substitute e for its reordered version, wrapped in a conversion to the original listing type L
//						val reordered = e.reorder(defaultColumns.map(_.name))
//						(reform.lift(l), reordered.to(rightRes))
//
//					case (l, r) if reform.mayReformLeft => try { //todo: fine grained control of include, exclude, reorder, add null
//						//include, exclude and reorder columns of this component so that they match e.items (including names)
//						val expectedColumnOrder = items.view.map(_.key) to Unique
//						val columnsByName = spelling.scope.applicableColumns(origin.anchored, anchored).groupBy(_.name).map {
//							case (name, Unique(column)) => name -> column
//							case (name, columns) =>
//								throw Bug("Multiple columns named '" + name + "' in mapping " + anchored + ": " + columns + ".")
//						}
//						val counterparts = items.view.map { item =>
//							val column = columnsByName.getOrElse(item.key,
//								throw new MismatchedExpressionsException(ComponentSQL.this, e,
//									"no column named " + item.key + " in " + anchored + " allowed in scope " +
//										spelling.scope + ": " + columnsByName
//								)
//							)
//							if (!(column.form.shape <:> spelling.shape(item.value)))
//								throw new MismatchedExpressionsException(ComponentSQL.this, e,
//									"the form " + column.form + " of column " + item.key + " from " + anchored +
//										" does not match the form " + item.value.selectForm + " of expression " +
//										item.value + " under the same name in the right record."
//								)
//							column
//						} to Unique
//						val excludes = columnsByName.view.values.filterNot(counterparts.contains)
//						if (excludes.exists(spelling.scope.isMandatory)) {
//							val missing = excludes.filter(spelling.scope.isMandatory)
//							throw new MismatchedExpressionsException(ComponentSQL.this, e,
//								"the right expression misses values for mandatory columns " + missing +
//									" of " + anchored + "."
//							)
//						}
//						val includes = counterparts.filterNot(defaultColumns.contains)
//						val reformed = alter(includes, excludes)
//						val reformedColumns = spelling.scope.defaultColumns(reformed.origin.anchored, reformed.anchored)
//						if (reformedColumns.view.map(_.name) == expectedColumnOrder)
//							(reform.lift(reformed), r)
//						else {
//							if (reformedColumns.size != expectedColumnOrder.size)
//								throw new MismatchedExpressionsException(ComponentSQL.this, e,
//									"Columns of " + reformed.anchored + " of expression " + reformed +
//										" reformed from " + ComponentSQL.this + " by including " + includes +
//										" and excluding " + excludes + " do not match the entries in the right expression. " +
//										"This is a bug."
//								)
//							val permutation = defaultColumns.view.map(expectedColumnOrder.sureIndexOf) to IndexedSeq
//							val reordered = reformed.reorder(permutation)
//							(reform.lift(reordered), r)
//						}
//					} catch {
//						case ex :Exception if mayPass || !leftResult.isDerived || !rightResult.isDerived => ex match {
//							case _ :MismatchedExpressionsException | _ :UnsupportedOperationException => try {
//								fallback(isListing.substituteContra[SQLExpression.c[E]#x[C]#E](e))(l, r)
//							} catch {
//								case e :Exception => e.addSuppressed(ex); throw e
//							}
//							case _ => throw ex
//						}
//					}
//	//				case (l, r) if spelling.shape(l) <:> spelling.shape(r) =>
//	//					(reform(l), r)
//					case (l, r) =>
//						fallback(isListing.substituteContra[SQLExpression.c[E]#x[C]#E](e))(l, r)
//				}
			}
	}
//	protected class ComponentReformer[LV[-G <: RowProduct, A[O] <: MappingAt[O], T] <: SQLExpression[G, Single, T],
//	                                  E <: RowProduct, C >: Grouped <: Single, X, U]
//	                                 (other :SQLExpression[E, C, X])(reform :ReformTo[LV], passesAllowed :Int)
//	                                 (implicit leftResult :Lift[Subject, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//		extends SpecificLValueReformer[LV, E, C, X, U](other)(reform, passesAllowed)
//	{
//		//overriden to call the overloaded variant of `->reform`
//		protected override def pass(e :SQLExpression[E, C, X]) :(LV[F, M, U], SQLExpression[E, C, U]) =
//			e.`->reform`(ComponentSQL.this)(reform.swap, passesAllowed - 1).swap
//
//		protected override def pass[N[O] <: MappingAt[O]](e :LValueSQL[E, N, X]) :(LV[F, M, U], LV[E, N, U]) =
//			e.`->reform`(ComponentSQL.this)(reform.swap, passesAllowed - 1).swap
//
//		override def term(e :SQLTerm[V2]) :Result =
//			(leftResult(ComponentSQL.this), rightResult(e)) match {
////				case (l, r) if spelling.columnCount(l) == spelling.columnCount(r) =>
////					(reform(l)(Lift.summon[U, U]), r)
//				//consider: should this have precedence over attempting a reform for a safe mind?
//				case (l, r) if !leftResult.isDerived || !rightResult.isDerived =>
//					reform(l, r)
//				case (l, r) if reform.mayReformRight && leftResult.isReversible =>
//					try {
//						val form = spelling.scope.termForm(origin.anchored, anchored)
//						//todo: support truncation of column sets/forms with the upcast to U
//						(reform.lift(l), e.reform(leftResult(form), rightResult))
//					} catch {
//						case ex @ (_ :MismatchedExpressionsException | _ :UnsupportedOperationException) => try {
//							fallback(e)(l, r)
//						} catch {
//							case e :Exception => ex.addSuppressed(e); throw ex
//						}
//					}
//				case (_, _) if mayPass =>
//					e.`->reform`(ComponentSQL.this)(reform.swap, passesAllowed - 1).swap
//				case (l, r) =>
//					fallback(e)(l, r)
//			}
//
//		//todo: tuple with component/column items
//
//		//todo: for IndexedSchemaMapping match the listing keys in the expression with component labels
//		//todo: use property names from extracts instead of column names - or both.
//		override def labeled[L <: Listing](e :LabeledSQL[E, C, L])(implicit isListing :X =:= L)
//				:(LV[F, M, U], SQLExpression[E, C, U]) =
//		{
//			val rightRes = isListing.substituteCo[({ type T[A] = Lift[A, U] })#T](rightResult)
//			//todo: compare property names, too
//			//todo: support non flat listings
//			//todo: support reordering. tough with non flat listings
//			val defaultColumns = spelling.scope.defaultColumns(origin.anchored, anchored)
//			val items = e.items
//			// the shapes of e and this component match, and the keys correspond exactly to default column names
//			val unified = defaultColumns.zipForAll(items) {
//				(column, item) => column.name == item.key && column.form.shape <:> spelling.shape(item.value)
//			}
//			//all items in e are columns with names matching those of default columns of this component,
//			// but in a different order
//			def reorderedColumns = defaultColumns.size == items.length && defaultColumns.forall { column =>
//				e.toMap.get(column.name) match {
//					case Some(value) => spelling.shape(value) <:> column.form.shape //consider: using reform.compatible
//					case _ => false
//				}
//			}
//			(leftResult(ComponentSQL.this), rightRes(e)) match {
//				case (l, r) if !leftResult.isDerived || !rightResult.isDerived =>
//					reform(l, r)
//				case (l, r) if unified =>
//					(reform.lift(l), r)
//
//				case (l, _) if reform.mayReorderRight && reorderedColumns =>
//					//substitute e for its reordered version, wrapped in a conversion to the original listing type L
//					val reordered = e.reorder(defaultColumns.map(_.name))
//					(reform.lift(l), reordered.to(rightRes))
//
//				case (l, r) if reform.mayReformLeft => try { //todo: fine grained control of include, exclude, reorder, add null
//					//include, exclude and reorder columns of this component so that they match e.items (including names)
//					val expectedColumnOrder = items.view.map(_.key) to Unique
//					val columnsByName = spelling.scope.applicableColumns(origin.anchored, anchored).groupBy(_.name).map {
//						case (name, Unique(column)) => name -> column
//						case (name, columns) =>
//							throw Bug("Multiple columns named '" + name + "' in mapping " + anchored + ": " + columns + ".")
//					}
//					val counterparts = items.view.map { item =>
//						val column = columnsByName.getOrElse(item.key,
//							throw new MismatchedExpressionsException(ComponentSQL.this, e,
//								"no column named " + item.key + " in " + anchored + " allowed in scope " +
//									spelling.scope + ": " + columnsByName
//							)
//						)
//						if (!(column.form.shape <:> spelling.shape(item.value)))
//							throw new MismatchedExpressionsException(ComponentSQL.this, e,
//								"the form " + column.form + " of column " + item.key + " from " + anchored +
//									" does not match the form " + item.value.selectForm + " of expression " +
//									item.value + " under the same name in the right record."
//							)
//						column
//					} to Unique
//					val excludes = columnsByName.view.values.filterNot(counterparts.contains)
//					if (excludes.exists(spelling.scope.isMandatory)) {
//						val missing = excludes.filter(spelling.scope.isMandatory)
//						throw new MismatchedExpressionsException(ComponentSQL.this, e,
//							"the right expression misses values for mandatory columns " + missing +
//								" of " + anchored + "."
//						)
//					}
//					val includes = counterparts.filterNot(defaultColumns.contains)
//					val reformed = alter(includes, excludes)
//					val reformedColumns = spelling.scope.defaultColumns(reformed.origin.anchored, reformed.anchored)
//					if (reformedColumns.view.map(_.name) == expectedColumnOrder)
//						(reform.lift(reformed), r)
//					else {
//						if (reformedColumns.size != expectedColumnOrder.size)
//							throw new MismatchedExpressionsException(ComponentSQL.this, e,
//								"Columns of " + reformed.anchored + " of expression " + reformed +
//									" reformed from " + ComponentSQL.this + " by including " + includes +
//									" and excluding " + excludes + " do not match the entries in the right expression. " +
//									"This is a bug."
//							)
//						val permutation = defaultColumns.view.map(expectedColumnOrder.sureIndexOf) to IndexedSeq
//						val reordered = reformed.reorder(permutation)
//						(reform.lift(reordered), r)
//					}
//				} catch {
//					case ex :Exception if mayPass || !leftResult.isDerived || !rightResult.isDerived => ex match {
//						case _ :MismatchedExpressionsException | _ :UnsupportedOperationException => try {
//							fallback(isListing.substituteContra[SQLExpression.c[E]#x[C]#E](e))(l, r)
//						} catch {
//							case e :Exception => e.addSuppressed(ex); throw e
//						}
//						case _ => throw ex
//					}
//				}
////				case (l, r) if spelling.shape(l) <:> spelling.shape(r) =>
////					(reform(l), r)
//				case (l, r) =>
//					fallback(isListing.substituteContra[SQLExpression.c[E]#x[C]#E](e))(l, r)
//			}
//		}
//	}


	//fixme: use ExtraXxx/CustomXxx columns
	protected override def split(implicit spelling :SQLSpelling) :Seq[GenericColumnComponentSQL.AnyIn[F]] =
		try {
			val columns = spelling.scope.defaultColumns(origin.anchored, anchored)
			origin.mapping.counterpartColumns(origin.anchored, columns).view.map { origin \ _ } to PassedArray
		} catch { //maybe this should be the primary implementation?
			case _ :NoSuchComponentException | _ :IncompatibleMappingsException =>
				origin.mapping.columns.view.collect {
					case column if spelling.scope.isDefault(origin.anchored.export(column)) => column
				}.map(origin \ _) to PassedArray
		}

	protected override def shape(implicit spelling :SQLSpelling) :RowShape = //consider: cache the result
		RowShape(spelling.scope.defaultColumns(origin.anchored, anchored).toSeq.map(_.form.sqlType))

	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		spelling.scope.defaultColumns(origin.anchored, anchored).size //fixme: does not include ExtraXxx columns

	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		origin.sqlParamCountOf(spelling)(this)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		origin.spell(spelling)(this :this.type, spelling.isInline)(from, context, params)

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		origin.spellExploded(spelling, independent)(this)(from, context, params)


	override def isomorphic(expression :SQLExpression.__) :Boolean = expression match {
		case self :AnyRef if this eq self => true
		case other :ComponentSQL.__ =>
			(origin isomorphic other.origin) && (mapping isomorphic other.mapping)
		case _ => false
	}

	private[oldsql] override def equivalent(expression :SQLExpression.__) :Boolean = expression match {
		case self :AnyRef if self eq this => true
		case component :ComponentSQL[_, _] if component canEqual this =>
			relation == component.relation && mapping == component.mapping
		case _ => false
	}


	override def toString :String = origin.toString + "." + mapping.mappingName
}






object ComponentSQL {
	//todo: we must ensure that the nominal component of a expression is always included, or throw an exception
	// if it would violate restrictions. This is tricky as it can happen by basing the component on an already
	// altered relation, including customized relations and even a choice initially valid can result
	// in an illegal instance after the origin relation is substituted with an actual one during spelling.
	// When we are at it, exclude(this.mapping) should work consistently for components and columns, and ideally other methods, too.

	//fixme: again, buggy overload resolution picks this instead of the following one even when given a relation
	//  and the order of method declaration doesn't seem to have any effect in this case.
	def apply[F <: RowProduct, M <: BaseMapping[S, F], S]
	         (from :F, component :M)
	         (implicit offset :RelationCount.In[F], project :OriginProjection[M, S])
			:ComponentSQL[F, project.WithOrigin] =
	{
		val relation = from.fullTableStack(offset.offset).toRelationSQL.asInstanceOf[
			RelationSQL[F, MappingOf[Any]#TypedProjection, Any, RowProduct Adjoin MappingOf[Any]#TypedProjection]
		]
		TypedComponentSQL(relation, component)
	}

	def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, K <: MappingAt[F], V, L <: RowProduct]
	         (origin :RelationSQL[F, T, R, L], component :K)(implicit project :OriginProjection[K, V])
			:ComponentSQL[F, project.WithOrigin] =
		TypedComponentSQL(origin, component)

	def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, C <: ColumnAt[F], V, L <: RowProduct]
	         (origin :RelationSQL[F, T, R, L], column :C)
	         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: BaseColumn[V, A] })
			:GenericColumnComponentSQL[F, project.WithOrigin, V] =
		TypedColumnComponentSQL(origin, column)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, Grouped, X])
			:Opt[(RelationSQL[O, T, R, L], MappingExtract[R, X, O]) forSome {
				type O >: F <: RowProduct; type T[O] <: BaseMapping[R, O]; type R; type L <: RowProduct Adjoin T
			}] =
		e match {
			case component :ComponentSQL.__ @unchecked =>
				Got((component.origin, component.extract).asInstanceOf[(
					RelationSQL[F, MappingOf[Any]#TypedAsFrom, Any, RowProduct Adjoin MappingOf[Any]#TypedAsFrom],
						MappingExtract[Any, X, F]
				)])
			case _ => Lack
		}



	/** A factory of component expressions serving as a type class of mapping types allowed as arguments to
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.\ \]]. Two implicit values exist:
	  *   1. creating a [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL ColumnComponentSQL]]/[[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL TypedColumnComponentSQL]],
	  *      used when the argument is a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]], and
	  *   1. creating a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]/[[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]]
	  *      if the argument is any other mapping.
	  *  Note that if an argument is an instance of `TypedColumn`, then the created expression will always
	  *  be a `TypedColumnComponentSQL`, even if the argument's static type is not a subtype of `TypedColumn`.
	  * @tparam M component mapping type accepted by this factory.
	  */
	trait Factory[M <: Mapping] {
		type Result[F <: RowProduct] <: ComponentSQL[F, M] forSome { type M[O] <: MappingAt[O] }
		type TypedResult[F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct] <: Result[F]

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct]
		         (table :RelationSQL[F, T, R, L], mapping :M) :TypedResult[F, T, R, L]
	}


	private[ast] sealed abstract class Rank1FactoryImplicits {
		implicit def componentFactory[M <: MappingOf[S], S](implicit project :OriginProjection[M, S])
				:Factory[M] {
					type Result[F <: RowProduct] = ComponentSQL[F, project.WithOrigin]
					type TypedResult[F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct] =
						TypedComponentSQL[F, T, R, project.WithOrigin, S, L]
				} =
			new Factory[M] {
				type Result[F <: RowProduct] = ComponentSQL[F, project.WithOrigin]
				type TypedResult[F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct] =
					TypedComponentSQL[F, T, R, project.WithOrigin, S, L]

				override def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct]
				                  (table :RelationSQL[F, T, R, L], mapping :M)
						:TypedComponentSQL[F, T, R, project.WithOrigin, S] =
					(table \ mapping.withOrigin[F])(project.isomorphism)
			}
	}


	object Factory extends Rank1FactoryImplicits {
		implicit def columnFactory[M <: BaseColumn[S, _], S]
		             (implicit project :OriginProjection[M, S] { type WithOrigin[O] <: BaseColumn[S, O] })
				:Factory[M] {
					type Result[F <: RowProduct] = GenericColumnComponentSQL[F, project.WithOrigin, S]
					type TypedResult[F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct] =
						TypedColumnComponentSQL[F, T, R, project.WithOrigin, S, L]
				} =
			new Factory[M] {
				type Result[F <: RowProduct] = GenericColumnComponentSQL[F, project.WithOrigin, S]
				type TypedResult[F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct] =
					TypedColumnComponentSQL[F, T, R, project.WithOrigin, S, L]

				override def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct]
				                  (table :RelationSQL[F, T, R, L], mapping :M)
						:TypedColumnComponentSQL[F, T, R, project.WithOrigin, S, L] =
					(table \ mapping.withOrigin[F])(project.isomorphism)
			}
	}




	/** A supertype of all [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions,
	  * with wildcard types as type arguments.
	  */
	type __ = ComponentSQL[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** A type alias for [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] used in cases
	  * where the actual value of the expression does not matter, but only its type (including the nominal mapping type).
	  * This in particular includes components of different tables.
	  * It is closely related to both [[net.noresttherein.oldsql.sql.RowShape RowShape]]
	  * and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], but extends the level of compatibility of
	  * matching expressions to a level preserved by most of its methods.
	  */
	type ComponentShape[M[O] <: MappingAt[O]] = ComponentSQL[Nothing, M]



	/** The interface of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expression and its subtypes.
	  * @tparam F    a ''from'' clause serving as the ''domain'' of this expression -
	  *              a list of relations/tables which provide columns used in this expression.
	  * @tparam M    a type constructor for the nominal [[net.noresttherein.oldsql.schema.Mapping Mapping]] type
	  *              of this component, accepting its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type
	  *              as the type parameter. Note that this is the interface type of the component as defined,
	  *              but the enclosing mapping of the origin relation might use a different mapping
	  *              as its ''export''. Furthermore, this expression can represent various views on a component,
	  *              including or excluding some of its optional columns. This has the effect of changing
	  *              the actual mapping used, which might be any proxy to this mapping (preserving its subject type).
	  * @tparam Same the self type of this instance - the type of component expressions produced by various copy/modify
	  *              methods of this type, typically also extended by any trait or class extending this trait.
	  */ //alter(Unique[TypedComponent]) requires Same <: ComponentSQLTemplate[F, M, Same] because it is implemented in two steps
	trait ComponentSQLTemplate[-F <: RowProduct, M[O] <: MappingAt[O],
	                           +Same <: ComponentSQL[F, M] with ComponentSQLTemplate[F, M, Same]]
		extends MappingSQLTemplate[F, Single, M, M[Unit]#Subject, Same]
		/* Does not extend GroundingTemplate because JoinedTable.anchor() returns JoinedRelation.
		 * We could merge it with ComponentSQLGroundingTemplate like so:
		 * [-F <: RowProduct, M[O] <: MappingAt[O], +Cons[f <: RowProduct] <: ComponentSQL[f, M],
		 *  +Same <: ComponentSQLTemplate[F, M, ({ type E[f <: RowProduct] = ComponentSQL[f, M] })#E, Same]
		 * However, anchor(Relation[M]) can return only Same, not Cons[F] because of variance, and Same is supposed
		 * to be the exact same type, returned from altering methods and the like.
		 * It currently does so anyway, but I'm not sure if we won't go the route of secretly changing the relation type
		 * based on a given Relation/RowProduct, and always return only JoinedRelation/RelationSQL
		 * from any grounding-like methods.
		 */
	{ self :Same =>
//		protected[sql] def upcast :Same = this

		//declared in LValueGroundingTemplate instead
//		type FromLast <: RowProduct

		override type Origin <: RowProduct //added an upper bound

		/** The mapping type of the `RelationSQL` to which this component belongs. */
		type Entity[O] <: MappingAt[O]

		/** The mapping for the whole row of the table of which this instance is a component expression.
		  * It is the public interface of the mapping provided by [[net.noresttherein.oldsql.schema.Relation Relation]]
		  * used by the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]] of this expression, and a mapping
		  * which recognizes the mapping of this expression as its component - but not necessarily those
		  * of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]].
		  */
//		def entity   :Entity[Origin] = origin.mapping

		/** An extract of this mapping from the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]]
		  * of the underlying [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]. Note that it includes
		  * the export version of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]] in regard
		  * to the 'nominal' mapping of the origin's [[net.noresttherein.oldsql.schema.Relation Relation]]: it ignores
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes]] &
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] component lists modifying the column
		  * set represented by this component and any changes applied by
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relation]]`.`[[net.noresttherein.oldsql.schema.Relation.export export]].
		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.export]]
		  */
		val extract  :MappingExtract[Entity[Unit]#Subject, M[Unit]#Subject, Origin]

		/** The 'nominal' mapping of this component expression: the original instance defining the component's
		  * subcomponents, defining the value type of this expression as its subject type. It might not be
		  * the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]] component
		  * of the mapping for the row
		  * of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]] relation
		  * of this component. For this reason, its column set is not the definitive specification of how this
		  * instance is rendered as SQL, both in their names and their number. While the column/component
		  * [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] are not final, the mapping itself
		  * is [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] with `export` and other of its versions
		  * exposed as properties of this instance.
		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]]
		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
		  */
		override val mapping :M[Origin]

		/** The effective version of this component from the point of view of the table mapping,
		  * that is a proxy to/clone of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]].
		  * It specifies neither the final column set used for this component expression, nor their names, because:
		  *   1. It is the export component only of the nominal table mapping -
		  *      `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relation]]`.`[[net.noresttherein.oldsql.schema.Relation.row row]]/`this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]] -
		  *      and does not take into account any non-standard view on that mapping imposed by
		  *      [[net.noresttherein.oldsql.schema.Relation.export Relation.export]] or per-expression
		  *      [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes included]] and [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excluded]]
		  *      subcomponents.
		  *   1. Any [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] and thus, by transitivity,
		  *      also any component expression, is semantically only a placeholder for the final relation alias included
		  *      in a ''from'' clause `F` of the SQL ''select'' containing the expression, and the final form of
		  *      this component is specified only at spelling time.
		  *   1. All methods of this trait and extending traits which accept subcomponents of this expression,
		  *      accept only components of the nominal mapping of this component `M`, in order to ensure independence
		  *      of the above changes.
		  * It is used as a representative of the class of abstraction of all versions of the nominal component mapping,
		  * in particular during the spelling process.
		  * @return `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]]`.`[[net.noresttherein.oldsql.schema.Mapping.export export]]`(`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]]`)`.
		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]]
		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
		  */
		val export   :TypedMapping[M[Unit]#Subject, Origin] //alternate names: avatar,actual,incarnation

		/** A version of the component [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]
		  * which takes into account any alterations applied to this expression. This includes in particular
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes included]] and
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excluded]] components, listed through
		  * the API provided by this trait, but potentially goes much further. These changes may result in a mapping
		  * sharing no ''export'' components (and columns) with `this.mapping`, but may not alter
		  * the component structure itself (relation of inclusion and associated extracts) or column names:
		  * they should be limited to modifying buffs of the components, or other properties influencing their handling.
		  *
		  * It is [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] with the nominal
		  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]] mappings
		  * of this object: properties of [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]]
		  * and [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.subcomponents subcomponents]] are all
		  * counterparts of each other, that is the extracts for columns/components at the same indices in those
		  * two collections are the same for all these mappings. It will also recognize the components of both
		  * `this.mapping` and `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]],
		  * and is itself a component of
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.altered altered]],
		  * but not of `origin.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]]; it should not
		  * be used to create new component expressions, as they would not be compatible with other expressions.
		  * It is however based on [[net.noresttherein.oldsql.schema.Relation.row nominal]],
		  * not [[net.noresttherein.oldsql.schema.Relation.export export]] mapping for the underlying relation and thus
		  * does not reflect the final column set which will be used in a rendered SQL (and will not recognize
		  * the components of the latter). This makes it of very little utility in client code.
		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]]
		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
		  */ //todo: try to see if we can allow it to have more columns than mapping - would be useful for null columns in unions
		val altered  :TypedMapping[M[Unit]#Subject, Origin]

		/** The definitive mapping used for this component, including alterations applied both by
		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relation]] and
		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]. This includes in particular
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes included]] and
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excluded]] components, listed through
		  * the API provided by this trait, but potentially goes much further. These changes may result in a mapping
		  * sharing no ''export'' components (and columns) with `this.mapping`, but may not alter
		  * the component structure itself (relation of inclusion and associated extracts) or column names:
		  * they should be limited to modifying buffs of the components, or other properties influencing their handling.
		  *
		  * It is [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] with the nominal
		  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]] mappings
		  * of this object: properties of [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]]
		  * and [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.subcomponents subcomponents]] are all
		  * counterparts of each other, that is the extracts for columns/components at the same indices in those
		  * two collections are the same for all these mappings.
		  *
		  * This mapping is a component of `origin.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]]
		  * and recognizes the components
		  * of both `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]] and
		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]], but
		  * not [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]] or
		  * `relation.`[[net.noresttherein.oldsql.schema.Relation.export export]], despite applying the changes
		  * from both. It should not be used to create new expressions, as it is (at least if any alterations
		  * are actually defined by either `origin` or `relation`) a purely local artifact, and the underlying relation
		  * will not recognize it as one of its components: it should be used only for SQL generation and processing
		  * of its results.
		  *
		  * Note that the column set of this mapping can be relied on to be final only if this instance
		  * is [[net.noresttherein.oldsql.sql.ast.ComponentSQL.isAnchored(from:F)* anchored]]
		  * in the [[net.noresttherein.oldsql.schema.Relation Relation]] actually included in the ''from'' clause `F`
		  * by which this expression is used; otherwise the `origin` will be normally replaced
		  * with its counterpart [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] at the same position
		  * in the used ''from'' clause when the expression is formatted as SQL. For this reason, in order to maintain
		  * portability between different enclosing expressions and ''from'' clauses, methods of this class accept only
		  * components of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]] and
		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]], rather than this mapping.
		  */
		val anchored :TypedMapping[M[Unit]#Subject, Origin]

		/** An expression for the whole relation of which this instance is a component. Aside from the mapping
		  * for the whole row specified by the carried [[net.noresttherein.oldsql.schema.Relation Relation]],
		  * it specifies also the position of the relation in ''from'' clause `F`, uniquely identifying a single
		  * entry in the ''from'' clause (or a grouping expression). It also stores lists of components which
		  * should be additionally [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes included]]
		  * or specifically [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excluded]] in regard to the default
		  * column set of the component.
		  */
		val origin   :JoinedRelation[Origin, Entity]

		/** The relation defining the root enclosing mapping of this component. In the most basic case,
		  * it is a [[net.noresttherein.oldsql.schema.Table Table]] used in the ''from'' clause,
		  * but synthetic implementations exist extending the concept
		  * to [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation grouping expressions]]
		  * and [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation query parameters]], allowing their use
		  * in SQL expression in the same manner as table columns.
		  * @return [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]].
		  */ //consider: making it a val already here
		def relation :Relation[Entity] //= origin.relation //we leave it abstract because JoinedRelation redeclares it as a val


		/** A component expression is customized if its column set has been modified in a non-standard way
		  * (other than specifying
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]] and
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]]),
		  * typically through method [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.custom custom]]
		  * of its [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] origin.
		  * Note that this does ''not'' take into account any alterations made directly
		  * to the [[net.noresttherein.oldsql.schema.Relation Relation]], neither current
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relation]] property, nor the actual relation instance
		  * used when the expression is formatted as SQL in the context of the final ''from'' clause.
		  *
 		  * Returning `true` implies `!this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.isDefault isDefault]].
		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.makeCustom]]
		  */
		def isCustom :Boolean = origin.isCustom

		/** Marks this component as [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isCustom custom]]
		  * without making any actual changes to it. This is useful when an otherwise
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isDefault default]] component expression
		  * should not have its column set reformed when aligned with an expression with mismatched columns.
		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.reform]]
		  */
		def makeCustom :Same // <- not in JoinedRelationTemplate

		/** A component expression is ''default'' ''iff'' it does not alter the column set of the represented mapping `M`,
		  * i.e., [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]] and
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] lists of its
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]] relation expression are empty.
		  * It does not specify anything about whether
		  * the underlying [[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relation]] is
		  * [[net.noresttherein.oldsql.schema.Relation.isDefault default]] itself: the exact form of component
		  * expressions is unspecified before their [[net.noresttherein.oldsql.sql.ast.ComponentSQL.anchor anchoring]]
		  * in the final ''from'' clause. This means that:
		  *   1. [[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]] mapping of this instance can be different
		  *      than its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]];
		  *   1. Neither does `export` define a single specific column set of mapping `M`, but it will be determined
		  *      based on the corresponding [[net.noresttherein.oldsql.schema.Relation Relation]] in the ''from'' clause
		  *      of the formatted SQL ''select''. Any alterations applied to the relation stack with
		  *      the `includes` & `excludes` lists from the origin
		  *      [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] associated with this instance,
		  *      with the latter having precedence.
		  *
		  * Being default implies `!origin.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.isCustom isCustom]].
		  */
		override def isDefault :Boolean = origin.isDefault

		/** An expression for the default version of this component, that is one without any alterations
		  * to its column set ''with the exception of those applied directly to ''
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]].[[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]].
		  * The [[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]] property of the component
		  * may be different than `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]],
		  * but `origin.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]]
		  * and `origin.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] are empty.
		  */
		override def default :Same //overridden for docs only

		/** Resets the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]] and
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] properties of
		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]
		  * to the given values. The collections must contain components of
		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]]
		  * or `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]].
		  */ //overridden for docs only
		override def defaultWith(includes : Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]]) :Same

//		/** Includes the given subcomponents in the rendering of this component as SQL.
//		  * This alters the underlying [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]],
//		  * adding the ''export'' version of every component (from the point of view of mapping
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]])
//		  * collection of the relation. If a component is currently present among
//		  * the explicitly [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excluded]]
//		  * to the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes includes]]
//		  * components of the relation, it will be removed from said list.
//		  *
//		  * This represents an alteration of the effective column set on a per-expression basis; when formatting SQL,
//		  * this change will be applied on top of the ''export'' mapping
//		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
//		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
//		  * column of this component will be included depends on the buffs of its effective version after these
//		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
//		  * is a name of a database operation type), this declaration will be ignored, producing no error,
//		  * neither in this method, nor when the expression is formatted.
//		  *
//		  * Note that this method has no effect on columns, as the nominal component is always considered included,
//		  * even it is optional.
//		  * @param components a collection of components of the ''nominal'' or ''export'' mappings of this instance -
//		  *                   components of
//		  *                   the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
//		  *                   mapping will result in throwing an [[IllegalArgumentException]] by this method.
//		  * @return a `ComponentSQL` instance of the same type as this instance, preserving also the type
//		  *         of the underlying relation.
//		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.+]]
//		  */
//		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
//		override def include(components :Iterable[TypedMapping[_, Origin]]) :C =
//			alter(Unique.empty, components)
//
//		/** Includes the given subcomponents in the rendering of this component as SQL.
//		  * This alters the underlying [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]],
//		  * adding the ''export'' version of every component (from the point of view of mapping
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]])
//		  * to the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes includes]]
//		  * collection of the relation. If a component is currently present among
//		  * the explicitly [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excluded]]
//		  * components of the relation, it will be removed from said list.
//		  *
//		  * This represents an alteration of the effective column set on a per-expression basis; when formatting SQL,
//		  * this change will be applied on top of the ''export'' mapping
//		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
//		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
//		  * column of this component will be included depends on the buffs of its effective version after these
//		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
//		  * is a name of a database operation type), this declaration will be ignored, producing no error,
//		  * neither in this method, nor when the expression is formatted.
//		  *
//		  * Note that this method has no effect on columns, as the nominal component is always considered included,
//		  * even it is optional.
//		  * @param components A sequence of functions selecting components of their arguments, given
//		  *                   the nominal [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]]
//		  *                   of this expression. These must be components of `this.mapping`, rather than
//		  *                   the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
//		  *                   mapping, or this method will throw an [[IllegalArgumentException]].
//		  * @return a `ComponentSQL` instance of the same type as this instance, preserving also the type
//		  *         of the underlying relation.
//		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.+]]
//		  */
//		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
//		override def include(components :M[Origin] => TypedMapping[_, Origin]*) :C =
//			include(components.view.map(_(mapping)))
//
//		/** Excludes the given subcomponents from the rendering of this component as SQL.
//		  * This alters the underlying [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]],
//		  * adding the ''export'' version of every component (from the point of view of mapping
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]])
//		  * to the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excludes]]
//		  * collection of the relation. If a component is currently present on the
//		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes list]] of components
//		  * to explicitly include, it will be removed from it.
//		  *
//		  * This represents an alteration of the effective column set on a per-expression basis; when formatting SQL,
//		  * this change will be applied on top of the ''export'' mapping
//		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
//		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
//		  * column of this component will be excluded depends on the buffs of its effective version after these
//		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
//		  * is a name of a database operation type), this declaration will be ignored, producing no error,
//		  * neither in this method, nor when the expression is formatted.
//		  *
//		  * This method will throw an [[UnsupportedOperationException]] when called with a non-empty collection
//		  * on a [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL column]] expression.
//		  * @param components a collection of components of the ''nominal'' or ''export'' mappings of this instance -
//		  *                   components of
//		  *                   the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
//		  *                   mapping will result in throwing an [[IllegalArgumentException]] by this method.
//		  * @return a `ComponentSQL` instance of the same type as this instance, preserving also the type
//		  *         of the underlying relation.
//		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.-]]
//		  */
//		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
//		@throws[UnsupportedOperationException]("if this component is a column and the component list is non empty.")
//		override def exclude(components :Iterable[TypedMapping[_, Origin]]) :C =
//			alter(Unique.empty, components)
//
//		/** Excludes the given subcomponents from the rendering of this component as SQL.
//		  * This alters the underlying [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]],
//		  * adding the ''export'' version of every component (from the point of view of mapping
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]])
//		  * to the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excludes]]
//		  * collection of the relation. If a component is currently present on the
//		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes list]] of components
//		  * to explicitly include, it will be removed from it.
//		  *
//		  * This represents an alteration of the effective column set on a per-expression basis; when formatting SQL,
//		  * this change will be applied on top of the ''export'' mapping
//		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
//		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
//		  * column of this component will be excluded depends on the buffs of its effective version after these
//		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
//		  * is a name of a database operation type), this declaration will be ignored, producing no error,
//		  * neither in this method, nor when the expression is formatted.
//		  *
//		  * This method will throw an [[UnsupportedOperationException]] when called with a non-empty collection
//		  * on a [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL column]] expression.
//		  * @param components A sequence of functions selecting components of their arguments, given
//		  *                   the nominal [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]]
//		  *                   of this expression. These must be components of the `this.mapping`, rather than
//		  *                   the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
//		  *                   mapping, or this method will throw an [[IllegalArgumentException]].
//		  * @return a `ComponentSQL` instance of the same type as this instance, preserving also the type
//		  *         of the underlying relation.
//		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.-]]
//		  */
//		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
//		@throws[UnsupportedOperationException]("if this component is a column and the component list is non empty.")
//		override def exclude(components :M[Origin] => TypedMapping[_, Origin]*) :C =
//			exclude(components.view.map(_(mapping)))
//
//		/** Creates a view of this component with certain columns or subcomponents specifically included or excluded.
//		  * The specified components will be (in their ''export'' versions) included in
//		  * the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes includes]]
//		  * and [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excludes]] lists
//		  * of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]] relation
//		  * of the result. If the underlying relation is already altered (any of its `includes` or `excludes`
//		  * are non empty or, more precisely,
//		  * `origin.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.isDefault isDefault]]
//		  * returns `false`), the components are not simply appended to these collections, but any already listed
//		  * component will override the current choice. Current elements of these lists which are not present
//		  * in any of the arguments remain as they were.
//		  *
//		  * This represents an alteration of the effective column set on a per-expression basis; when formatting SQL,
//		  * this change will be applied on top of the ''export'' mapping
//		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
//		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
//		  * column of this component will be included depends on the buffs of its effective version after these
//		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
//		  * is a name of a database operation type), this declaration will be ignored, producing no error,
//		  * neither in this method, nor when the expression is formatted.
//		  *
//		  * Both argument lists must contain only components of the nominal mapping `M` or its
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]] version; passing any other mapping,
//		  * including components of [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
//		  * will result in throwing an [[IllegalArgumentException]].
//		  * @param includes a collection of subcomponents of this component which should be included when formatting
//		  *                 this object as SQL, if allowed in the specific context in which this expression is used.
//		  *                 These must be components of
//		  *                 `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]]
//		  *                 (or `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]),
//		  *                 i.e. not of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]]
//		  *                 or [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
//		  *                 mappings, or an [[IllegalArgumentException]] will be thrown.
//		  * @param excludes a collection of subcomponents of this component which should be excluded when formatting
//		  *                 this object as SQL, if allowed in the specific context in which this expression is used.
//		  * @return a `ComponentSQL` instance of the same type as this instance, preserving also the type
//		  *         of the underlying relation.
//		  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.+-]]
//		  */
//		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
//		@throws[UnsupportedOperationException]("if this component is a column and the excludes list is non empty.")
//		override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]]) :C
//
//		/** Creates a view of this component with certain columns or subcomponents specifically included or excluded.
//		  * The specified components will be (in their ''export'' versions) included in
//		  * the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes includes]]
//		  * and [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excludes]] lists
//		  * of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]] relation
//		  * of the result. If the underlying relation is already altered (any of its `includes` or `excludes`
//		  * are non empty or, more precisely,
//		  * `origin.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.isDefault isDefault]]
//		  * returns `false`), the components are not simply appended to these collections, but any already listed
//		  * component will override the current choice. Current elements of these lists which are not present
//		  * in any of the arguments remain as they were.
//		  *
//		  * This represents an alteration of the effective column set on a per-expression basis; when formatting SQL,
//		  * this change will be applied on top of the ''export'' mapping
//		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
//		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
//		  * column of this component will be included depends on the buffs of its effective version after these
//		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
//		  * is a name of a database operation type), this declaration will be ignored, producing no error,
//		  * neither in this method, nor when the expression is formatted.
//		  *
//		  * @param components a list of functions accepting the nominal
//		  *                   [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]
//		  *                   of this instance and returning its component, wrapped in either
//		  *                   [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]] or
//		  *                   [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]].
//		  *                   These can be created by methods [[net.noresttherein.oldsql.schema.Mapping.+ +]] and
//		  *                   [[net.noresttherein.oldsql.schema.Mapping.- -]] defined on any mapping. The included
//		  *                   components must be true components of `M` or
//		  *                   its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]]
//		  *                   version, but not those of
//		  *                   `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]],
//		  *                   or `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]],
//		  *                   otherwise an [[IllegalArgumentException]] will be thrown.
//		  * @return a `ComponentSQL` instance of the same type as this instance, preserving also the type
//		  *         of the underlying relation.
//		  */
//		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
//		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
//		override def alter(components :M[Origin] => ComponentSelection[_, Origin]*) :C =
//			this +- components

		/** Creates a view of this component consisting only of the specified columns, in the exact order.
		  * It is similar in effect to
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.alter alter]]`(columns, mapping.columns.filterNot(columns.contains))`,
		  * but the latter will have columns appearing in the same order as in
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored]]`.`[[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]].
		  * Like for the latter, there is no guarantee that the final SQL will actually correspond to the given
		  * column list: the mapping's [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] affect which columns
		  * can - or must - be used in a specified database operation type, so the exact column set is determined
		  * only when the expression is [[net.noresttherein.oldsql.sql.ast.ComponentSQL.defaultSpelling spelled]],
		  * based on the place of use and the actual [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
		  * instance corresponding
		  * to `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]
		  * in the ''from'' clause of the spelled SQL ''select''. As the result, using the returned expression
		  * in different contexts can even result in different SQL being generated. Because the argument cannot
		  * be validated against the allowed column set at this moment, no errors are reported by this method,
		  * or when spelling, if the desired column set cannot be achieved.
		  *
		  * For this reason, it is the caller's responsibility to assure that the column set is legal
		  * for the intended use, for example, because it contains columns required for all operations, and no columns
		  * prohibited from including in any operation. Normally, this method is used only during the spelling
		  * process in order to align this expression with another [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]].
		  * @param columns a list of columns of the nominal
		  *                [[net.noresttherein.oldsql.sql.ast.MappingSQL.mapping mapping]] of this instance.
		  *///consider: we could move it to MappingSQLTemplate, but there is no way to currently implement it in LooseComponent
		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		def alter(columns :Unique[TypedColumn[_, Origin]]) :Same = {
			val exports = columns.map(mapping.export(_))
			val permutation = ReorderedMapping.subseqPermutation(mapping.columns, exports)
			defaultWith(exports, mapping.columns.filterNot(exports.contains)).reorder(permutation)
		}


		/** A component expression representing a view of this component's
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]], applying on top of
		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relation]]
		  * (the [[net.noresttherein.oldsql.schema.Relation.export export]] view of the relation),
		  * in addition to any alterations defined by this instance's
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]], the view defined
		  * by the `template` expression. This includes
		  * specifically [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes included]] and
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excluded]] components of this component
		  * (or its origin). Only alterations defined directly by the argument `JoinedRelation` are taken into account,
		  * ignoring any potential difference between
		  * `template.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]]`.`[[net.noresttherein.oldsql.schema.Relation.row row]]
		  * and `template.relation.`[[net.noresttherein.oldsql.schema.Relation.export export]].
		  *
		  * The [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] property will thus be a composition
		  * of the views defined by `this.relation`, `this.origin`, and `template`, preserving the order where relevant.
		  * Exact semantics of stacking are undefined: in particular, returned relation may not recognize components of
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] mappings and
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]]`.`[[net.noresttherein.oldsql.schema.Relation.export export]]
		  * of `this.origin` and `template`, with guarantees only for the components of the nominal mappings.
		  * This effect does not have to be restricted to the mapping of this component, but may involve global changes
		  * to the whole relation.
		  *
		  * This method is used primarily when an altered component expression must be evaluated in a context of
		  * an already altered corresponding relation in a spelled ''from'' clause, for example
		  * when spelling a subcomponent of a component expression used in a ''group by'' clause.
		  * @param template An expression presenting the mapping of this component as a relation.
		  *                 The 'nominal' component mapping `M` must be the same instance as `this.mapping`.
		  * @return [[net.noresttherein.oldsql.sql.ast.ComponentSQL.GenericComponentSQLTemplate.graft graft]]`(template.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.alterOther alterOther]]`(origin, this))`.
		  */
		def alterLike[C <: RowProduct](template :JoinedRelation[C, M]) :Same

//		/** Creates a view of this component with certain columns or components specifically included or excluded.
//		  * This method is exactly equivalent to [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter alter]].
//		  */ //consider: renaming it apply, and rename the appropriate Mapping apply methods to +-
//		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
//		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
//		override def +-(components :Iterable[M[Origin] => ComponentSelection[_, Origin]]) :C =
//			super.+-(components)
//
//		/** Includes the given subcomponent in the rendering of this component as SQL.
//		  * This alters the underlying [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]],
//		  * adding the ''export'' version of the subcomponent (from the point of view of mapping
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]])
//		  * to the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes includes]]
//		  * collection of the relation. If the component is currently present among
//		  * the explicitly [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excluded]]
//		  * components of the relation, it will be removed from said list.
//		  *
//		  * This represents an alteration of the effective column set on a per-expression basis; when formatting SQL,
//		  * this change will be applied on top of the ''export'' mapping
//		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
//		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
//		  * column of this component will be included depends on the buffs of its effective version after these
//		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
//		  * is a name of a database operation type), this declaration will be ignored, producing no error,
//		  * neither in this method, nor when the expression is formatted.
//		  *
//		  * Note that this method has no effect on columns, as the nominal component is always considered included,
//		  * even it is optional.
//		  * @param component A function selecting a component of
//		  *                  the nominal [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]]
//		  *                  of this expression, given as its argument. The returned mapping must be a component
//		  *                  of `this.mapping`, rather than of
//		  *                  [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]],
//		  *                  [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
//		  *                  or [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]]
//		  *                  mappings, otherwise this method will throw an [[IllegalArgumentException]].
//		  * @return a `ComponentSQL` instance of the same type as this instance, preserving also the type
//		  *         of the underlying relation.
//		  */
//		@throws[IllegalArgumentException]("if the given component is not a component of this mapping.")
//		override def +(component :M[Origin] => TypedMapping[_, Origin]) :C =
//			include(Unique.single(component(mapping)))
//
//		/** Excludes the given subcomponent from the rendering of this component as SQL.
//		  * This alters the underlying [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]],
//		  * adding the ''export'' version of the subcomponent (from the point of view of mapping
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]])
//		  * to the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.excludes excludes]]
//		  * collection of the relation. If the component is currently present on the
//		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes list]] of components
//		  * to explicitly include, it will be removed from it.
//		  *
//		  * This represents an alteration of the effective column set on a per-expression basis; when formatting SQL,
//		  * this change will be applied on top of the ''export'' mapping
//		  * of the [[net.noresttherein.oldsql.schema.Relation Relation]] corresponding to this expression in
//		  * the ''from'' clause `F` of the formatted ''select'' supplied during spelling. Whether or not a particular
//		  * column of this component will be excluded depends on the buffs of its effective version after these
//		  * alterations: if a column is not allowed in a particular context (has a `NoXxx` buff, where `Xxx`
//		  * is a name of a database operation type), this declaration will be ignored, producing no error,
//		  * neither in this method, nor when the expression is formatted.
//		  *
//		  * This method will throw an [[UnsupportedOperationException]] when called with a non-empty collection
//		  * on a [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL column]] expression.
//		  * @param component A function selecting a component of
//		  *                  the nominal [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]]
//		  *                  of this expression, given as its argument. The returned mapping must be a component
//		  *                  of `this.mapping`, rather than of
//		  *                  [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]],
//		  *                  [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
//		  *                  or [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]]
//		  *                  mappings, otherwise this method will throw an [[IllegalArgumentException]].
//		  * @return a `ComponentSQL` instance of the same type as this instance, preserving also the type
//		  *         of the underlying relation.
//		  */
//		@throws[IllegalArgumentException]("if the given component is not a component of this mapping.")
//		@throws[UnsupportedOperationException]("if this component is a column.")
//		override def -(component :M[Origin] => TypedMapping[_, Origin]) :C =
//			exclude(Unique.single(component(mapping)))


		/** Provides aliases for some or all columns of this mapping, to use in an ''as'' clause if this expression
		  * is used inside a ''select'' clause. It is made by creating
		  * a [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.custom custom]]
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] (of the same subtype
		  * as `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]) to use
		  * as a substitute `origin`.
		  * Its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
		  * mapping will be an adapter to
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.relation relation]]`.`[[net.noresttherein.oldsql.schema.Relation.RelationTemplate.row row]]
		  * with the same non-column components as the adapted mapping, but with
		  * [[net.noresttherein.oldsql.schema.support.AliasedColumn AliasedColumn]]s wrapping the columns
		  * of the latter. This is transparent to the application and is only checked during SQL formatting.
		  *
		  * Applications will rarely need this level of fine grained control, as they should prefer
		  * to always use [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] objects rather than
		  * names or aliases to represent columns. It however can see use in the case of creating
		  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL compound]] ''selects'': same column aliases
		  * in ''select'' clauses of constituent ''selects'' allow better matching of the columns and a clearer
		  * final SQL.
		  *
		  * This method relies on a generic mechanism for altering the export mapping of a component expression
		  * and, as such, standard methods from the
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]/[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.exclude exclude]]
		  * family may interfere with its results by wrapping aliased columns in additional adapters. For this reason,
		  * it should be called as late as possible.
		  * @param aliases a map with columns of this component - either in its export versions, or in export versions
		  *                for `this.origin.mapping` - mapping to their desired aliases. It should not include versions
		  *                from [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]]
		  *                or [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
		  *                mapping or any others. It doesn't have to cover all columns of this component;
		  *                any not included in the map will not be aliased.
		  */
		def aliased(aliases :Map[TypedColumn[_, Origin], String]) :Same

		/** Provides aliases for some or all columns of this mapping, to use in an ''as'' clause if this expression
		  * is used inside a ''select'' clause. It is made by creating
		  * a [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.custom custom]]
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] (of the same subtype
		  * as `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]) to use
		  * as a substitute `origin`.
		  * Its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
		  * mapping will be an adapter to
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.relation relation]]`.`[[net.noresttherein.oldsql.schema.Relation.RelationTemplate.row row]]
		  * with the same non-column components as the adapted mapping, but with
		  * [[net.noresttherein.oldsql.schema.support.AliasedColumn AliasedColumn]]s wrapping the columns
		  * of the latter. This is transparent to the application and is only checked during SQL formatting.
		  *
		  * Applications will rarely need this level of fine grained control, as they should prefer
		  * to always use [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] objects rather than
		  * names or aliases to represent columns. It however can see use in the case of creating
		  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL compound]] ''selects'': same column aliases
		  * in ''select'' clauses of constituent ''selects'' allow better matching of the columns and a clearer
		  * final SQL.
		  *
		  * This method relies on the generic mechanism for altering the export mapping of a component expression
		  * and, as such, standard methods from the
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]/[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.exclude exclude]]
		  * family may interfere with its results by wrapping aliased columns in additional adapters. For this reason,
		  * it should be called as late as possible.
		  * @param aliases an exploded list with columns of this component - either in its export versions,
		  *                or in export versions for `this.origin.mapping` - mapped to their desired aliases.
		  *                It should not include versions
		  *                from [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]]
		  *                or [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
		  *                mapping or any others. It doesn't have to cover all columns of this component;
		  *                any not included in the map will not be aliased.
		  * @return `this.aliased(aliases.toMap)`.
		  */
		def aliased(aliases :(TypedColumn[_, Origin], String)*) :Same = aliased(aliases.toMap) //not in JoinedRelationTemplate

		/** A [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isCustom custom]] version
		  * of this expression with columns reordered according to the given permutation. The permutation is applied
		  * to the columns of the nominal [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.mapping mapping]],
		  * discarding any reordering that might already be in place (cals to `reorder` do not stack).
		  * This is a low level method used in the spelling process if a need arises to align columns of two homomorphic
		  * component expressions, but with different ordering of corresponding columns. There should be little need
		  * for it in the application code, as the changes are not binding (unless the expression is made
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.makeFinal final]])
		  * and can have unexpected interactions with other methods and use cases, such as altering
		  * of the included columns or ''group by'' expressions.
		  * @param permutation a permutation of all columns of the nominal mapping `M`: it must contain
		  *                    every value from `[0..mapping.columns.size)` exactly once; the value at position `n`
		  *                    specifies the new position of the `n`-th column of
		  *                    `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]
		  *                    in the new expression's
		  *                    [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]]
		  *                    mapping.
		  */
		def reorder(permutation :IndexedSeq[Int]) :Same

		/** A [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isCustom custom]] version
		  * of this expression with columns reordered according to the ordering defined by the argument function.
		  * The permutation is applied to the columns of the nominal
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate.mapping mapping]],
		  * discarding any reordering that might already be in place (cals to `reorder` do not stack).
		  * This is a low level method used in the spelling process if a need arises to align columns of two homomorphic
		  * component expressions, but with different ordering of corresponding columns. There should be little need
		  * for it in the application code, as the changes are not binding (unless the expression is made
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.makeFinal final]])
		  * and can have unexpected interactions with other methods and use cases, such as altering
		  * of the included columns or ''group by'' expressions.
		  *
		  * This method has no effect on components of [[net.noresttherein.oldsql.sql.ast.JoinedParam parameter relations]].
		  * @param precedes A function defined for export columns of the ''nominal''
		  *                 [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]
		  *                 for this component (export for `this.mapping`, not `origin.mapping`).
		  *                 If `precedes(column1, column2)`, then in
		  *                 [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.altered altered]]
		  *                 mapping of the result, the export version of `column1` will appear before the export version
		  *                 of `column2` in `export.columns` and all other column lists. This defines the order
		  *                 in which the columns will be rendered in the SQL.
		  */
		def reorder(precedes :(TypedColumn[_, Origin], TypedColumn[_, Origin]) => Boolean) :Same = //<- not in JoinedRelationTemplate
			reorder(ReorderedMapping.permutation(mapping.columns.toIndexedSeq)(precedes))

		/** Does this component define the final version used for spelling?
		  * If so, the expression will be treated as an already
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isAnchored anchored]] one when spelling
		  * and its [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]] method
		  * will not substitute it with a corresponding component in the spelled ''from'' clause.
		  * This is very low level behaviour used in limited scope during the spelling process.
		  * Instances visible to the application are not final.
		  * @return `origin.isFinal`
		  */ //todo: rename to isAnchored/isAligned/isReformed when we rename anchor to groundIn
		protected def isFinal :Boolean = origin.isFinal

		/** Create a [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isFinal final]] version
		  * of the component, freezing its column set for the purpose of spelling.
		  * This method should not be made public to the application or used outside of the spelling process.
		  */
		protected def makeFinal :Same //<- effective

		/** Forwarder to [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.makeFinal makeFinal]]. */
		protected[sql] final def `->makeFinal` :Same = makeFinal

		/** Returns `!`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isFinal isFinal]]. */
		protected[sql] final def `->isFinal` :Boolean = isFinal

		/** Substitutes the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.relation relation]]
		  * of this component for the argument. As the row mapping of the given relation will be a different instance
		  * if `relation` and `this.relation` are different instances, an effort is made to find a counterpart
		  * of this component in `relation.`[[net.noresttherein.oldsql.schema.Relation.row row]]; if successful,
		  * it will be used as the nominal
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]] of the returned
		  * expression. If the relation's type doesn't match the type of relation in this component's
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]], or its mapping
		  * is not equivalent to the current origin's mapping, an exception will be thrown.
		  */ //this one cannot just convert to another relation type because type F will be likely incompatible
		@throws[IllegalArgumentException]("If the relation's type is unsuitable for this instance's origin.")
		@throws[IncompatibleMappingsException]("If the component cannot be anchored because no counterpart " +
		                                       "in the argument's mapping can be found.")
		def anchor(relation :Relation[Entity]) :Same

		//commented out because return type in JoinedRelationTemplate is just JoinedRelation (not a specialized subtype)
//		/** Substitutes the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.relation relation]]
//		  * of this component for the argument. As the row mapping of the given relation will be a different instance
//		  * if `relation` and `this.relation` are different instances, an effort is made to find a counterpart
//		  * of this component in `relation.`[[net.noresttherein.oldsql.schema.Relation.row row]]; if successful,
//		  * it will be used as the nominal
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]] of the returned
//		  * expression. If the relation's type doesn't match the type of relation in this component's
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]], or its mapping
//		  * is not equivalent to the current origin's mapping, an exception will be thrown.
//		  */
//		@throws[IllegalArgumentException]("If the relation's type is unsuitable for this instance's origin.")
//		@throws[IncompatibleMappingsException]("If the component cannot be anchored because no counterpart " +
//		                                       "in the argument's mapping can be found.")
//		def anchor(relation :Relation[Entity]) :C //ComponentSQL[F, M]
//
//		/** An expression for the same component, but with the given relation used as its
//		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]. If the `Entity` mapping instance
//		  * of the new relation does not equal the mapping of the current relation, an attempt is made to
//		  * find a corresponding component in the new relation, which becomes used instead of `mapping`.
//		  * The difference from `relation \ this.mapping` is that 1) this method is polymorphic and can preserve
//		  * specialized `ComponentSQL` implementations, and 2) the latter requires `this.mapping` to be a component
//		  * of `relation.mapping`.
//		  */
//		@throws[IncompatibleMappingsException](
//			"If the component cannot be grafted because no counterpart in the argument's mapping can be found.")
//		def graft[P <: RowProduct](rel :JoinedRelation[P, Entity]) :ComponentSQL[P, M]
//
//		/** An expression for the same component, but from the first known
//		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] of ''from'' clause `P`.
//		  * Created expression is equal to this one, except for its `origin`, which is substituted with
//		  * `origin.moveTo(offset)`.
//		  *
//		  * Note that, as a `JoinedRelation[F, T]` is semantically only a placeholder bound to a particular
//		  * [[net.noresttherein.oldsql.schema.Relation Relation]] instance included in the ''from'' clause `F` of
//		  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expression when the latter is rendered as
//		  * the final SQL, information carried by this instance - in particular the exact column set used
//		  * for component `M` - may be no longer representative of the ''from'' clause `P`. In the extreme case,
//		  * instances representing grouping expressions may refer to tables not present in `P`.
//		  * See the documentation of [[net.noresttherein.oldsql.sql.ast.JoinedRelation.moveTo JoinedRelation.moveTo]]
//		  * for more information on how this conflict is resolved.
//		  *
//		  * @param offset a proof that the first relation listed in ''from'' clause `P` is the same as the `origin`
//		  *               relation of this component (or, more precisely, they use the same mapping type `Entity`),
//		  *               carrying the offset of the new origin.
//		  * @return [[net.noresttherein.oldsql.sql.ast.ComponentSQL.anchor graft]]`(`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.moveTo moveTo]]`(offset))`,
//		  *         or a copy of this instance using `offset`
//		  *         as its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.position position]] if this instance
//		  *         is a `JoinedRelation`.
//		  */
//		def moveTo[P <: RowProduct](offset :RelationOffset[P, Entity] { type First = FromLast }) :ComponentSQL[P, M]
//
////		override def asLast :ComponentSQL[FromLast, M] //not implemented here because of linearization issues in RelationSQL subtypes.


		/** A pseudo relation adapting this expression for use in ''group by'' clauses
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and [[net.noresttherein.oldsql.sql.By By]].
		  */ //todo: make it a method of SQLExpression. Of course, the problem here is the lack of Subject type
		def asGrouping :GroupingRelation[F, M]
	}


	 /* This trait can't be mixed in into JoinedRelationTemplate with Same =:= JoinedRelation[F, T] (instead of Rel),
	  * because anchor(Relation) cannot return the exact same type, primarily because JoinedGrouping has the ungrouped
	  * from clause in its type signature in addition to the grouped one, and it depends on the same type parameter
	  * in GroupingRelation.
	  */
	/** Expands the generic interface of
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate ComponentSQLTemplate]] with
	  * several methods adapting the component to another clause `F`, which are ''not'' included
	  * in [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]].
	  * The latter trait - through [[net.noresttherein.oldsql.sql.ast.LValueSQL.LValueTemplate LValueTemplate]] -
	  * needs to be mixed in separately, because for [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
	  * methods declared in the latter need to return a `JoinedRelation[_ >: F, T]` (due to use of variant evidence),
	  * while methods declared here use invariant source of new type `F`, and can return `JoinedRelation[F, T]` normally.
	  * Unlike `ComponentSQLTemplate`, this interface is not mixed in by all subclasses of `ComponentSQL`:
	  * there is no guarantee that adapting a specific relation,
	  * like [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]], to a new clause type will result
	  * in a relation of the same type.
	  */
	trait GenericComponentSQLTemplate[-F <: RowProduct, M[O] <: MappingAt[O],
	                                 +Cons[f <: RowProduct] <: ComponentSQL[f, M],
	                                 +Same <: ComponentSQL[F, M] with ComponentSQLTemplate[F, M, Same]]
		extends ComponentSQLTemplate[F, M, Same]
		/* Does not extend LValueTemplate because in JoinedRelation methods from GroundingTemplate
		 * return EC[f] = JoinedRelation[_ >: f, T], as they depend on variant evidence. Methods declared here return
		 * EC[f] = JoinedRelation[f, T], because they use arguments invariant in F.
		 */
	{ self :Same =>
		/** An expression for the same component, but with the given relation used as its
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]. If the `Entity` mapping instance
		  * of the new relation does not equal the mapping of the current relation, an attempt is made to
		  * find a corresponding component in the new relation, which becomes used instead of `mapping`.
		  * The difference from `relation \ this.mapping` is that 1) this method is polymorphic and can preserve
		  * specialized `ComponentSQL` implementations, and 2) the latter requires `this.mapping` to be a component
		  * of `relation.mapping`.
		  */
		@throws[IncompatibleMappingsException](
			"If the component cannot be grafted because no counterpart in the argument's mapping can be found.")
		def graft[C <: RowProduct](relation :JoinedRelation[C, Entity]) :Cons[C]

		/** An expression for the same component, but from the first known
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] of ''from'' clause `P`.
		  * Created expression is equal to this one, except for its `origin`, which is substituted with
		  * `origin.moveTo(offset)`.
		  *
		  * Note that, as a `JoinedRelation[F, T]` is semantically only a placeholder bound to a particular
		  * [[net.noresttherein.oldsql.schema.Relation Relation]] instance included in the ''from'' clause `F` of
		  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] expression when the latter is rendered as
		  * the final SQL, information carried by this instance - in particular the exact column set used
		  * for component `M` - may be no longer representative of the ''from'' clause `P`. In the extreme case,
		  * instances representing grouping expressions may refer to tables not present in `P`.
		  * See the documentation of [[net.noresttherein.oldsql.sql.ast.JoinedRelation.moveTo JoinedRelation.moveTo]]
		  * for more information on how this conflict is resolved.
		  *
		  * @param offset a proof that the first relation listed in ''from'' clause `P` is the same as the `origin`
		  *               relation of this component (or, more precisely, they use the same mapping type `Entity`),
		  *               carrying the offset of the new origin.
		  * @return [[net.noresttherein.oldsql.sql.ast.ComponentSQL.anchor graft]]`(`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.moveTo moveTo]]`(offset))`,
		  *         or a copy of this instance using `offset`
		  *         as its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.position position]] if this instance
		  *         is a `JoinedRelation`.
		  */ //Can't implement this one because we don't know yet that FromLast = origin.FromLast
		def moveTo[C <: RowProduct](offset :RelationOffset[C, Entity] { type First = FromLast }) :Cons[C]
//			graft(origin.moveTo(offset))

		/** Converts this $this to an expression based on clause `E[F]`, which expands `F` by a single relation. */
		def asIn[E[+L <: F] <: L Expanded M forSome { type M[O] <: MappingAt[O] }] :Cons[E[F]] =
			asIn[E[F]](PrefixOf.expand)

		/** This method is equivalent to
		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.GenericComponentSQLTemplate.expand expand]]`[E]`,
		  * but relies on invariant `PrefixOf` rather than variant
		  * [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]] of the latter, so the result
		  * has an exact clause type parameter `E`, rather than some `_ >: E <: RowProduct`.
		  */
		def asIn[E <: RowProduct](implicit expansion :F PrefixOf E) :Cons[E] =
			graft(origin.asIn[E])

		override def asLast :Cons[FromLast] = //graft(origin.asLast)
			moveTo(RelationOffset.unsafe[FromLast, FromLast, origin.position.Rel, Entity](0))
	}


	/** An implementation mixin narrowing down the return type of methods defined in
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.GenericComponentSQLTemplate GenericComponentSQLTemplate]]
	  * and [[net.noresttherein.oldsql.sql.ast.LValueSQL.LValueTemplate LValueTemplate]]
	  * to `Cons[F]` or `Cons[E]/Cons[_ >: E]`, respectively. It contains only implementation of those methods
	  * and is designed to by mixed in by traits invariant in their clause type parameter `F`.
	  * Because `F` is always specified as a clause including mapping `M` as the first one (following a wildcard prefix),
	  * grounding methods from [[net.noresttherein.oldsql.sql.SQLExpression.GroundingTemplate GroundingTemplate]]
	  * need to return `Cons[_ >: F]`, as they are based on variant evidence (although it is of little practical matter).
	  * This allows this trait to be extended by
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]] and
	  * [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL TypedColumnComponentSQL]].
	  * Despite invariance, this trait is ''not'' extended directly
	  * by [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate JoinedRelationTemplate]]
	  * because some of methods defined here may return a relation of a different type than this instance.
	  * Otherwise, it performs the safe function as its variant cousin.
	  */ //we don't extend this trait from JoinedRelation, so the implementations here will have to be overridden
	trait InvariantComponentSQLTemplate[F <: RowProduct, M[A] <: MappingAt[A],
	                                    +Cons[f <: RowProduct] <: ComponentSQL[f, M] with ComponentSQLTemplate[f, M, Cons[f]]]
		extends LValueTemplate[F, M, M[Unit]#Subject, ({ type E[f <: RowProduct] = Cons[_ >: f <: RowProduct] })#E, Cons[F]]
		   with GenericComponentSQLTemplate[F, M, Cons, Cons[F]]
	{ self :Cons[F] =>
		override type Origin = F

		override def default :Cons[F] =
			if (isDefault) this else graft(origin.default)

		override def defaultWith(includes :Unique[TypedMapping[_, F]], excludes :Unique[TypedMapping[_, F]]) :Cons[F] = {
			validateComponents(includes)
			validateExcludes(excludes)
			graft(origin.defaultWith(includes, excludes))
		}

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		override def include(components :Iterable[TypedMapping[_, F]]) :Cons[F] = {
		    validateComponents(components)
			graft(origin.include(components))
		}

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def exclude(components :Iterable[TypedMapping[_, F]]) :Cons[F] = {
			validateExcludes(components)
			graft(origin.exclude(components))
		}

		override def alter(includes :Iterable[TypedMapping[_, F]], excludes :Iterable[TypedMapping[_, F]]) :Cons[F] = {
			validateComponents(includes)
			validateExcludes(excludes)
			graft(origin.alter(includes, excludes))
		}

		override def alterLike[C <: RowProduct](template :JoinedRelation[C, M]) :Cons[F] = {
			type Rel[f <: RowProduct] = JoinedRelation[f, Entity] { type FromLast = self.FromLast }
			graft(template.alterOther[F, F, Entity, FromLast, Rel](origin, this))
		}

		private def validateComponents(components :Iterable[TypedMapping[_, F]]) :Unit = {
			val nonComponents = components.view.filter {
				c => !mapping.contains(c) || mapping == c
			}.toList
			if (nonComponents.nonEmpty)
				throw new IllegalArgumentException(
					s"Cannot include/exclude not belonging components $nonComponents in/from mapping $mapping of $this."
				)
		}

		private def validateExcludes(components :Iterable[TypedMapping[_, F]]) :Unit = {
			validateComponents(components)
			if (components.exists(origin.mapping.export(_) == export))
				throw new IllegalArgumentException(
					"Cannot exclude mapping " + mapping + " from itself: " + components + "."
				)
		}

		override def aliased(aliases :Map[TypedColumn[_, Origin], String]) :Cons[F] = graft(origin.aliased(aliases))

		override def makeCustom :Cons[F] = graft(origin.makeCustom)

		protected override def makeFinal :Cons[F] = graft(origin.`->makeFinal`)

		override def anchor(from :F) :Cons[F] = graft(origin.anchor(from))

		override def anchor(relation :Relation[Entity]) :Cons[F] = graft(origin.anchor(relation))

		override def moveTo[E <: RowProduct](offset :RelationOffset[E, Entity] { type First = self.FromLast }) :Cons[E] =
			graft(origin.moveTo(offset))

		//these may clash with definition in LValueTemplate because Cons is invariant
		override def expand[U <: F, E <: RowProduct]
                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single) :Cons[_ >: E] =
			{ val res = graft(origin.expand(base).origin); res } //type inference without an expected type

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :Cons[_ >: E] =
			{ val res = graft(origin.expand[E](expansion).origin); res }

//		/** Converts this $this to an expression based on clause `E[F]`, which expands `F` by a single relation. */
//		def asIn[E[+L <: F] <: L Expanded M forSome { type M[O] <: MappingAt[O]}] :Cons[E[F]] =
//			asIn[E[F]](PrefixOf.expand)
//
//		/** This method is equivalent to
//		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.InvariantComponentTemplate.expand expand]]`[E]`,
//		  * but relies on invariant `PrefixOf` rather than variant
//		  * [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]] of the latter, so the result
//		  * has an exact clause type parameter `E`, rather than some `_ >: E <: RowProduct`.
//		  */
//		def asIn[E <: RowProduct](implicit expansion :F PrefixOf E) :Cons[E] =
//			moveTo(position + expansion)
//
//		override def asLast :Cons[FromLast] = //note Cons[FromLast], not as inherited Cons[_ >: FromLast]
//			moveTo(RelationOffset.unsafe[FromLast, FromLast, origin.position.Rel, Entity](0))

//			override type isSelectable = true
	}



	/** Implementation interface of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] which lists
	  * the type arguments of the origin [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
	  * and the subject type of both the component and origin mappings among its type parameters.
	  * This duplication exists because in many contexts, in particular [[net.noresttherein.oldsql.sql.Join Join]]
	  * classes, the mapping type used is a very generic [[net.noresttherein.oldsql.schema.Mapping.MappingAt MappingAt]],
	  * which does not define a [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type, and there is a need
	  * for expression types representing those mappings. On the other hand, implementation of many methods
	  * is impossible without knowledge of the subject type, and `BaseMapping` is a required argument type
	  * for most factory methods of both expressions and ''from'' clauses. This trait provides also a much more
	  * compact type signature than refinement of `ComponentSQL` with all its type parameters would.
	  * The upper bound of both mappings is narrowed down to
	  * [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] because, in Scala 2, using
	  * [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]] instead causes self-type
	  * errors in indirect subclasses due to a compiler bug).
	  * @tparam F $F
	  * @tparam T $T
	  * @tparam R $R
	  * @tparam M $M
	  * @tparam V $V
	  * @tparam L $L
	  * @define F    A ''from'' clause serving as the ''domain'' of this expression -
	  *              a list of relations/tables which provide columns used in this expression.
	  *              It is also [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL!.Origin Origin]]
	  *              type argument for all mapping properties of this expression.
	  * @define T    A type constructor for the `Mapping` type of the underlying
	  *              [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]] relation.
	  *              In the basic use case, it would be a mapping for the whole table row.
	  * @define R    The type to which the complete row of the underlying relation maps; it is the `Subject` type
	  *              of the mapping `T` of the `origin` of this component.
	  * @define V    The type to which this component maps; it is the `Subject` type of the component's mapping `M`
	  *              and the value type of this expression.
	  * @define L    The [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type of the clause resulting
	  *              from dropping all joins and tables following `T` in `F`.
	  * @define Cons `TypedComponentSQL`
	  * @define link [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]]
	  */
	trait TypedComponentSQL[F <: RowProduct, T[A] <: BaseMapping[R, A], R,
	                        M[A] <: BaseMapping[V, A], V, L <: RowProduct]
		extends ComponentSQL[F, M]
		   with InvariantComponentSQLTemplate[F, M, ({ type E[f <: RowProduct] = TypedComponentSQL[f, T, R, M, V, L] })#E]
	{ self =>
		override type Origin = F
		override type Entity[A] = T[A]
		override type FromLast = L

		/** A projection substituting all references to mapping's [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]]
		  * type in `M[O]` with an arbitrary new origin type.
		  */
		def projection :IsomorphicProjection[M, V, F]

//		/** As any [[net.noresttherein.oldsql.sql.ast.RelationSQL RelationSQL]]`[F, T, R, L]`
//		  * was initially created as a `RelationSQL[O, T, R, O]`
//		  * and is a [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]`[O, T]`, its domain clause
//		  * type parameter is a subtype of `O` through contravariance, and it is always safe to upcast it to `O`.
//		  * By transition, the same can be said for any `TypedComponentSQL[F, T, R, M, S, O]`.
//		  */
//		override def upcast :TypedComponentSQL[F, T, R, M, S] { type FromLast = self.FromLast } =
//			this.asInstanceOf[TypedComponentSQL[T, R, M, S] { type FromLast = self.FromLast }]
//
//		//stupid Scala can't otherwise recognize that Origin = O
//		protected[sql] def originAsOrigin :TypedComponentSQL[F, T, R, M, S] { type FromLast = self.FromLast } =
//			this

		override val origin  :RelationSQL[F, T, R, FromLast]
		override val extract :MappingExtract[R, V, Origin]
		override val export  :TypedMapping[V, Origin] //BaseMapping may be actually needed, but it can always be obtained from withOrigin and this interface is rarely used
//		override def anchored :TypedMapping[V, O] = origin.anchored.export(mapping)
//
//		override def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
//				:TypedComponentSQL[F, T, R, project.WithOrigin, X, L]
//
//		override def \[K <: ColumnAt[Origin], X]
//		              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
//				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, X, L]
//
//		override def \[K <: MappingAt[Origin]](component :M[Origin] => K)(implicit factory :ComponentSQL.Factory[K])
//				:factory.TypedResult[F, T, R, L]

		override def \[K <: MappingAt[F], X](component :K)(implicit project :OriginProjection[K, X])
				:TypedComponentSQL[F, T, R, project.WithOrigin, X, L] =
			origin \ component

		override def \[K <: ColumnAt[F], X]
		              (column :K)
		              (implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, X, L] =
			//we don't need to check if column==entity as a column always has itself as its column and among extractors.
			origin \ column

		override def \[K <: MappingAt[F]]
		              (component :M[F] => K)(implicit factory :ComponentSQL.Factory[K])
				:factory.TypedResult[F, T, R, L] =
			factory(origin, component(mapping))


//		override def default :TypedComponentSQL[F, T, R, M, S, L]
//
//		override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
//				:TypedComponentSQL[F, T, R, M, S, L]
//
//		override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
//				:TypedComponentSQL[F, T, R, M, S, L]
//
		//todo: in Scala 3, extract the bulk of these two methods into a single implementation (requires generic functions)
		override def reorder(permutation :IndexedSeq[Int]) :TypedComponentSQL[F, T, R, M, V, L] =
			if (!isCustom && permutation == permutation.indices) //!isCustom checks if we are not already reordered
				this
			else {
				ReorderedMapping.validatePermutation(mapping, permutation)
				val newOrigin = origin.custom { (relation, originCounterpart) =>
					if (relation == originCounterpart && origin == this) //this will also cover relation == mapping
						ReorderedMapping[TypedMapping[Any, Unit], Any, Unit](relation, permutation)
					else {
						val nominal =
							//originCounterpart == mapping also checks for origin.mapping == mapping, which is the important case here
							if (originCounterpart == mapping || relation.contains(mapping.withOrigin[Unit]))
								mapping.withOrigin[Unit]
							else try {
								originCounterpart.counterpart(origin.mapping, mapping).original
							} catch {
								case e :NoSuchComponentException =>
									throw new UnsupportedOperationException(
										"Cannot reorder columns of component " + mapping +
										permutation.mkString(" by permutation(", ",", ")") +
										" because no counterpart can be found in the altered relation mapping " +
										relation + ".", e
									)
							}
						val export = relation.export(nominal)
						val exportColumns = export.counterpartColumns(mapping, mapping.columns)
						//changes the order from export.columns back to that of mapping.columns in case it is also reordered
						val existingPermutation = export.columns.toIndexedSeq.map(exportColumns.indexOf)
						val combinedPermutation = existingPermutation.map(permutation(_))
						val reordered = export.reorder(combinedPermutation)
						val substitution = PatchedMapping.Overrides(export, reordered)
						PatchedMapping[TypedMapping[Any, Unit], Any, Unit](relation, substitution)
					}
				}
				graft(newOrigin)
			}

		//consider: ColumnSetter[From[M], _, _] can be legally created only if origin is a JoinedTable,
		// but there is no type which will work for all relation types and a type class would be even more problematic
		override def substitute[E <: F]
		                       (substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V] =
			if (substitutes.isEmpty)
				this
			else {
				val cols = substitutes.map {
					setter => origin.export.export(setter.lvalue.mapping.refine.withOrigin[F])
				}
				val included = include(cols)
				//1. filter duplicates; 2. anchor each setter in `included`
				val acc = (mutable.Set.empty[TypedColumn[_, F]], List.empty[ColumnSetter[included.FromLast, E]])
				val setters = (substitutes :\ acc) { case (setter, (cols, setters)) =>
					import setter.promote
					val anchored = included \ setter.lvalue.mapping.refine.withOrigin[F]
					if (!cols.add(anchored.export)) (cols, setters)
					else (cols, (anchored.asLast := setter.rvalue)::setters)
				}._2
				EditedComponentSQL[E, T, R, M, V, included.FromLast](included)(setters)
			}

//		override def anchor(from :F) :TypedComponentSQL[F, T, R, M, S, L] =
//			graft(origin.anchor(from))
//
//		override def anchor(relation :Relation[T]) :TypedComponentSQL[F, T, R, M, S, L]

		//todo: improve this once we have comparable extracts
		override def graft[P <: RowProduct](relation :JoinedRelation[P, T])
				:TypedComponentSQL[P, T, R, M, V, relation.FromLast] =
			if (relation eq origin)
				this.asInstanceOf[TypedComponentSQL[P, T, R, M, V, relation.FromLast]]
			else if (relation.mapping == origin.mapping) //can safely use this.mapping
				(relation.typed \ mapping.withOrigin[P])(projection.isomorphism[P])
			//todo: replace this check with something stronger; current MappingReadForm.equals compares referential equality of columns though.
			else if (export == origin.mapping) //shouldn't happen, but better safe than sorry
				relation.asInstanceOf[TypedComponentSQL[P, T, R, M, V, relation.FromLast]]
			else if (!(relation.mapping compatible origin.mapping))
				throw new IncompatibleMappingsException(
					"Cannot graft component expression " + this + " onto relation expression " + relation +
					" because its relation's " + relation.relation + " mapping is not equal to this instance's relation's " +
					this.relation + " mapping and their mappings are not compatible."
				)
			else {
				val counterpart = relation.mapping.counterpart(origin.mapping, export).original
				/* An approximate type check. Can yield false negatives if M is actually a supertype
				 * of the true mapping type. Remember though that the mapping types for origin and relation are equal,
				 * JoinedRelation is invariant in the mapping type and, while component.mapping can be in theory
				 * defined by a subtype of T rather than T, it is very unlikely because its Origin type lists T
				 * as the row mapping type. So, while this can fail in many ways, it should work in practical use cases.
				 */
				if (mapping.getClass != counterpart.getClass)
					throw new IllegalArgumentException(
						"Cannot graft component expression " + this + " onto relation expression " + relation +
						" because the types of the component mapping " + mapping +
						" and its counterpart in the argument relation " + counterpart + " are of different types: " +
						mapping.getClass.getName + " vs. " + counterpart.getClass + "."
					)
				(relation.typed \ counterpart.asInstanceOf[M[P]])(projection.isomorphism)
			}

//		override def moveTo[P <: RowProduct](offset :RelationOffset[P, T] { type First = FromLast })
//				:TypedComponentSQL[P, T, R, M, S, L] =
//			graft(origin.moveTo(offset))
//
//
//		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
//				:TypedComponentSQL[E, T, R, M, S, L] =
//			expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])
//
//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//				:TypedComponentSQL[E, T, R, M, S, L] =
//			{ val res = graft(origin.expand(base).origin); res } //type inference without an expected type
//
//		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E)
//				:TypedComponentSQL[E, T, R, M, S, L] =
//			{ val res = graft(origin.expand[E](expansion).origin); res }
//
//		override def asLast :TypedComponentSQL[FromLast, T, R, M, S, FromLast] =
//			moveTo(RelationOffset.unsafe[FromLast, FromLast, origin.position.Rel, T](0))

		override type isSelectable = true


		override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E) :TopSelectAs[M] =
			SelectSQL[E, M, V](from, this)

		override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectAs[B, M] = //:SubselectAs[B, M] =
			SelectSQL.subselect[B, from.type, M, V](from, this)

		override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
				:SelectMapping[P, M] =
			Select(from)[M, V](this)


		override def asGrouping :GroupingRelation[F, M] = origin.asGrouping(this)


		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, V] =
			visitor.component(this)

		protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, V, Y]) :Y =
			visitor.component(this)


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case component :TypedComponentSQL.__ @unchecked if canEqual(component) && component.canEqual(this) =>
				origin == component.origin && mapping == component.mapping
			case _ => false
		}
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TypedComponentSQL.__ @unchecked]
		override def hashCode :Int = origin.hashCode * 31 + mapping.hashCode

		protected[sql] override
		def all_subclasses_of_ComponentLValueSQL_must_extend_TypedComponentSQL_or_LooseComponent(seal :Seal) :Unit = ()
	}



	object TypedComponentSQL {

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, K <: MappingAt[F], V, L <: RowProduct]
		         (origin :RelationSQL[F, T, R, L], component :K)(implicit project :OriginProjection[K, V])
				:TypedComponentSQL[F, T, R, project.WithOrigin, V, L] =
			component match {
				case column :TypedColumn[V, F] @unchecked =>
					TypedColumnComponentSQL(origin, column)
						.asInstanceOf[TypedComponentSQL[F, T, R, project.WithOrigin, V, L]]
				case _ if component == origin.mapping || component == origin.export ||
				          component == origin.relation.export =>
					origin.asInstanceOf[TypedComponentSQL[F, T, R, project.WithOrigin, V, L]]
				case _ =>
					val projected = project[F](component)
					import project.isomorphism
					new ProperComponent[F, T, R, project.WithOrigin, V, L](origin, projected)
			}

		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[O, T, R, L], MappingExtract[R, X, F]) forSome {
					type O >: F <: RowProduct; type T[A] <: BaseMapping[R, A]; type R; type L <: RowProduct Adjoin T
				 }] =
			e match {
				case component :TypedComponentSQL.__ @unchecked =>
					Got((component.origin, component.extract).asInstanceOf[(
						RelationSQL[F, MappingOf[Any]#TypedAsFrom, Any, RowProduct Adjoin MappingOf[Any]#TypedProjection],
							MappingExtract[Any, X, F]
					)])
				case _ => Lack
			}

		/** A supertype of all [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]]
		  * expressions, with wildcard types as type arguments.
		  */
		type __ = TypedComponentSQL[F, T, R, M, V, L] forSome {
			type F <: RowProduct; type L <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type AnyIn[-F <: RowProduct] = TypedComponentSQL[O, T, R, M, V, L] forSome {
			type O >: F <: RowProduct; type L <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type Typed[-F <: RowProduct, V] = TypedComponentSQL[O, T, R, M, V, L] forSome {
			type O >: F <: RowProduct; type L <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]
		}


		private class ProperComponent[F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[S, A], S,
		                              L <: RowProduct]
		                             (override val origin :RelationSQL[F, T, R, L], override val mapping :M[F])
		                             (implicit override val projection :IsomorphicProjection[M, S, F])
			extends TypedComponentSQL[F, T, R, M, S, L]	with RowShapeCache
		{ self =>
			override def relation      :Relation[T] = origin.relation
			override val extract       :MappingExtract[R, S, Origin] = origin.export(mapping)
			override val export        = extract.export
			override lazy val altered    :TypedMapping[S, F] = origin.altered.export(mapping)
			override lazy val anchored   :TypedMapping[S, F] = origin.anchored.export(mapping)
			override lazy val selectForm :SQLReadForm[S] = super.selectForm
//			override def upcast :TypedComponentSQL[F, T, R, M, S, L] = this
//
//			override def defaultWith(includes :Unique[TypedMapping[_, F]], excludes :Unique[TypedMapping[_, F]])
//					:TypedComponentSQL[F, T, R, M, S, L] =
//			{
//				validateComponents(includes)
//				validateExcludes(excludes)
//				graft(origin.defaultWith(includes, excludes))
//			}
//
//			private def validateComponents(components :Iterable[TypedMapping[_, F]]) :Unit = {
//				val nonComponents = components.view.filter {
//					c => !mapping.contains(c) || mapping == c
//				}.toList
//				if (nonComponents.nonEmpty)
//					throw new IllegalArgumentException(
//						s"Cannot include/exclude not belonging components $nonComponents in/from mapping $mapping of $this."
//					)
//			}
//
//			private def validateExcludes(components :Iterable[TypedMapping[_, F]]) :Unit = {
//				validateComponents(components)
//				if (components.exists(origin.mapping.export(_) == export))
//					throw new IllegalArgumentException(
//						"Cannot exclude mapping " + mapping + " from itself: " + components + "."
//					)
//			}
//
//			@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
//			override def include(components :Iterable[TypedMapping[_, F]]) :TypedComponentSQL[F, T, R, M, S, L] = {
//			    validateComponents(components)
//				graft(origin.include(components))
//			}
//
//			@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
//			@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
//			override def exclude(components :Iterable[TypedMapping[_, F]]) :TypedComponentSQL[F, T, R, M, S, L] = {
//				validateExcludes(components)
//				graft(origin.exclude(components))
//			}
//
//			override def alter(includes :Iterable[TypedMapping[_, F]], excludes :Iterable[TypedMapping[_, F]])
//					:TypedComponentSQL[F, T, R, M, S, L] =
//			{
//				validateComponents(includes)
//				validateExcludes(excludes)
//				graft(origin.alter(includes, excludes))
//			}
//
//			override def alterLike[E <: RowProduct](template :JoinedRelation[E, M])
//					:TypedComponentSQL[F, T, R, M, S, L] =
//			{
//				type Rel[f <: RowProduct] = RelationSQL[f, T, S, L]
//				graft(template.alterOther[F, F, T, L, Rel](origin, this))
//			}

			//			override def aliased(aliases :Map[TypedColumn[_, Origin], String]) :TypedComponentSQL[F, T, R, M, S, L] =
//				graft(origin.aliased(aliases))
//
//			override def makeCustom :TypedComponentSQL[F, T, R, M, S, L] = graft(origin.makeCustom)
//
//			protected override def makeFinal :TypedComponentSQL[F, T, R, M, S, L] = graft(origin.`->makeFinal`)
//
//			override def anchor(relation :Relation[T]) :TypedComponentSQL[F, T, R, M, S, L] =
//				graft(origin.anchor(relation))

//			override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectAs[B, M] =
//				SelectSQL.subselect[B, from.type, M, S](from, this)
//
//			override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E) :TopSelectAs[M] =
//				SelectSQL[E, M, S](from, this)
//
//			override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectAsResult[B] = //:SubselectAs[B, M] =
//				SelectSQL.subselect[B, from.type, M, S](from, this)
//
//			override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
//					:SelectMapping[P, M] =
//				Select(from)[M, S](this)
		}



		trait SpecificComponentVisitor[+F <: RowProduct, X, +Y]
			extends SpecificRelationVisitor[F, X, Y] with SpecificColumnComponentVisitor[F, X, Y]
		{
			def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[X, A], L <: RowProduct]
			             (e :TypedComponentSQL[O, T, R, M, X, L]) :Y
		}
		trait MatchSpecificComponent[+F <: RowProduct, X, +Y]
			extends SpecificComponentVisitor[F, X, Y]
			   with CaseSpecificRelation[F, X, Y] with CaseSpecificColumnComponent[F, X, Y]
		{
			override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[X, A], L <: RowProduct]
			                            (e :TypedColumnComponentSQL[O, T, R, M, X, L]) :Y =
				component(e)
		}
		trait CaseSpecificComponent[+F <: RowProduct, X, +Y] extends MatchSpecificComponent[F, X, Y] {
			override def relation[O >: F <: RowProduct, T[A] <: BaseMapping[X, A], L <: RowProduct]
			                     (e :RelationSQL[O, T, X, L]) :Y =
				component(e)
		}
//
//		trait ComponentVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends RelationVisitor[F, Y] with ColumnComponentVisitor[F, Y]
//		{
//			def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
//			             (e :TypedComponentSQL[O, T, R, M, V, L]) :Y[Single, V, TypedComponentSQL[O, T, R, M, V, L]]
//		}
//		trait MatchComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends ComponentVisitor[F, Y] with CaseRelation[F, Y] with CaseColumnComponent[F, Y]
//		{
//			override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
//			                            (e :TypedColumnComponentSQL[O, T, R, M, V, L])
//			        :Y[Single, V, TypedColumnComponentSQL[O, T, R, M, V, L]] =
//				component(e)
//		}
//		trait CaseComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//			extends MatchComponent[F, Y]
//		{
//			override def relation[O >: F <: RowProduct, T[A] <: BaseMapping[V, A], V, L <: RowProduct]
//			                     (e :RelationSQL[O, T, V, L]) :Y[Single, V, RelationSQL[O, T, V, L]] =
//				component(e)
//		}

		trait AnyComponentVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
			extends AnyRelationVisitor[F, Y] with AnyColumnComponentVisitor[F, Y]
		{
			def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
			             (e :TypedComponentSQL[O, T, R, M, V, L]) :Y[Single, V]
		}
		trait MatchAnyComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
			extends AnyComponentVisitor[F, Y] with CaseAnyRelation[F, Y] with CaseAnyColumnComponent[F, Y]
		{
			override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
			                            (e :TypedColumnComponentSQL[O, T, R, M, V, L]) :Y[Single, V] =
				component(e)
		}
		trait CaseAnyComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyComponent[F, Y] {
			override def relation[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct]
			                     (e :RelationSQL[O, T, R, L]) :Y[Single, R] =
				component(e)
		}
	}

}






/** A specialization of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expression
  * for [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] subtypes, which
  * is a [[net.noresttherein.oldsql.sql.ColumnSQL single column]] expression.
  * The mapping type is accepted as a type parameter, just as in `ComponentSQL`, in order to not limit its usability,
  * but in virtually all cases the type argument will be simply the type constructor of `TypedColumn`.
  * For this reason a type alias [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL ColumnComponentSQL]] exists,
  * which accepts only the column's subject type as its type parameter and is thus much more concise.
  *
  * Every implementation of this trait is required to also extend
  * [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL TypedColumnComponentSQL]],
  * for reasons stated in the documentation of `ComponentSQL`.
  * @tparam F $F
  * @tparam M $M
  * @tparam V $V
  * @define M a type constructor for the nominal [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]
  *           of this component, accepting its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type
  *           as the type parameter. Note that this is the interface type of the component as defined,
  *           but the enclosing mapping of the origin relation might use a different mapping
  *           as its ''export''. Furthermore, this expression can represent various views on a component,
  *           including or excluding some of its optional columns. This has the effect of changing
  *           the actual mapping used, which might be any proxy to this mapping (preserving its subject type).
  *           and the value type of this expression.
  * @define this column
  * @define Cons `GenericColumnComponentSQL`
  * @define link [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL GenericColumnComponent]]
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL]]
  * @see [[net.noresttherein.oldsql.sql.ast.JoinedRelation]]
  */
trait GenericColumnComponentSQL[-F <: RowProduct, M[A] <: BaseColumn[V, A], V]
	extends ComponentSQL[F, M] with ColumnLValueSQL[F, M, V] with SelectableColumnMappingSQL[F, M, V]
	   with LValueTemplate[F, M, V, ({ type E[f <: RowProduct] = GenericColumnComponentSQL[f, M, V] })#E,
	                       GenericColumnComponentSQL[F, M, V]]
	   with GenericComponentSQLTemplate[F, M, ({ type E[f <: RowProduct] = GenericColumnComponentSQL[f, M, V] })#E,
	                                    GenericColumnComponentSQL[F, M, V]]
{ self =>
	override type Subject = V
	override def component :GenericColumnComponentSQL[F, M, V] = this
	//fixme: we should use export.selectForm, but it's not a ColumnReadForm, because SimpleColumn uses an empty SQLReadForm
	// if it is not selectable. This should be possible to work around though as we can ban non-selectable columns
	// or throw an exception here. This requires two selectForm methods in ColumnMapping though and some careful thinking.
	override def selectForm :ColumnReadForm[V] = form
	override def form       :ColumnForm[V] = anchored.form

	override val extract         :ColumnMappingExtract[Entity[Unit]#Subject, V, Origin]
	override val export          :TypedColumn[V, Origin] = extract.export
	override lazy val altered    :TypedColumn[V, Origin] = origin.altered.export(mapping)
	override lazy val anchored   :TypedColumn[V, Origin] = origin.anchored.export(mapping)

	protected override def setter[R <: RowProduct, Y]
	                             (rvalue :ColumnSQL[R, Single, Y])(implicit compat :V =~= Y) :ColumnSetter[F, R] =
		ColumnSetter[F, M, V, R, compat.Unified](to(compat.left), compat.right(denullify(rvalue)))
//
//	protected override def convert[Y](conversion :SQLConversion[S, Y]) :ColumnLValueSQL[F, M, Y] =
//		ConvertedColumnLValue(this, conversion)
//
//	override def anchor(from :F) :GenericColumnComponentSQL[F, M, S] = graft(origin.anchor(from))
//
//	override def anchor(relation :Relation[Entity]) :GenericColumnComponentSQL[F, M, S] = graft(origin.anchor(relation))
//
//	override def graft[P <: RowProduct](rel :JoinedRelation[P, Entity]) :GenericColumnComponentSQL[P, M, S]
//
//	override def moveTo[P <: RowProduct](offset :RelationOffset[P, Entity] { type First = FromLast })
//			:GenericColumnComponentSQL[P, M, S]
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
//			:GenericColumnComponentSQL[E, M, S] =
//		expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//			:GenericColumnComponentSQL[E, M, S] =
//		expand[E]
//
//	override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :GenericColumnComponentSQL[E, M, S]
//
//	override def asLast :GenericColumnComponentSQL[FromLast, M, S]


	protected override def split(implicit spelling :SQLSpelling) :Seq[GenericColumnComponentSQL[F, M, V]] =
		PassedArray :+ this

	protected[sql] override def atomicSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                             (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(toColumnSQL)(from, context, params)
}




object GenericColumnComponentSQL {

	def apply[F <: RowProduct, M <: ColumnAt[F], S]
	         (from :F, column :M)
	         (implicit offset :RelationCount.In[F],
	          project :OriginProjection[M, S] { type WithOrigin[A] <: BaseColumn[S, A] })
			:GenericColumnComponentSQL[F, project.WithOrigin, S] =
	{
		val relation = from.fullTableStack(offset.offset).toRelationSQL.asInstanceOf[
			RelationSQL[F, MappingOf[Any]#TypedProjection, Any, RowProduct Adjoin MappingOf[Any]#TypedProjection]
		]
		TypedColumnComponentSQL(relation, column)
	}

	def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, C <: ColumnAt[F], V, L <: RowProduct]
	         (origin :RelationSQL[F, T, R, L], column :C)
	         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: BaseColumn[V, A] })
			:GenericColumnComponentSQL[F, project.WithOrigin, V] =
		TypedColumnComponentSQL(origin, column)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
			:Opt[(RelationSQL[O, T, R, L], ColumnMappingExtract[R, X, O]) forSome {
				type O >: F <: RowProduct; type T[A] <: BaseMapping[R, A]; type R; type L <: RowProduct Adjoin T
			}] =
		e match {
			case component :TypedColumnComponentSQL.__ @unchecked =>
				Got((component.origin, component.extract).asInstanceOf[(
					RelationSQL[F, MappingOf[Any]#TypedAsFrom, Any, RowProduct Adjoin MappingOf[Any]#TypedProjection],
						ColumnMappingExtract[Any, X, F]
				)])
			case _ => Lack
		}



	/** A supertype of all [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL GenericColumnComponentSQL]]
	  * expressions, with wildcard types as type arguments.
	  */
	type __ = GenericColumnComponentSQL[_ <: RowProduct, M, V] forSome { type V; type M[O] <: BaseColumn[V, O] }

	/** An existential alias for all column expressions sharing the domain clause `F`,
	  * regardless of their mapping types.
	  * @see [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL]]
	  */
	type AnyIn[-F <: RowProduct] = GenericColumnComponentSQL[F, M, V] forSome { type V; type M[O] <: BaseColumn[V, O] }

	/** Implementation interface of
	  * [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL GenericColumnComponentSQL]] which lists
	  * the type arguments of the origin [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
	  * and the subject type of both the column and origin mappings among its type parameters.
	  * @tparam F $F
	  * @tparam T $T
	  * @tparam R $R
	  * @tparam M $M
	  * @tparam V $V
	  * @tparam L $L
	  * @define Cons `TypedColumnComponentSQL`
	  * @define link [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL TypedColumnComponentSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL]]
	  */ //todo: rename to TypedColumnSQL
	trait TypedColumnComponentSQL[F <: RowProduct, T[A] <: BaseMapping[R, A], R,
	                              M[A] <: BaseColumn[V, A], V, L <: RowProduct]
		extends TypedComponentSQL[F, T, R, M, V, L] with GenericColumnComponentSQL[F, M, V]
		   with InvariantComponentSQLTemplate[F, M, ({ type E[f <: RowProduct] = TypedColumnComponentSQL[f, T, R, M, V, L] })#E]
	{ self =>
		/** This method helps with type inference in some situations. */
//		protected[sql] override def upcast :TypedColumnComponentSQL[F, T, R, M, S, L] = this

		//stupid Scala can't otherwise recognize that Origin = O
//		protected[sql] override def originAsOrigin
//				:TypedColumnComponentSQL[F, T, R, M, S, Origin] { type FromLast = self.FromLast } = this

//		override def default :TypedColumnComponentSQL[F, T, R, M, S, L] =
//			graft(origin.default)

		override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
				:TypedColumnComponentSQL[F, T, R, M, V, L] =
			if (excludes.nonEmpty)
				throw new UnsupportedOperationException(
					s"A column expression $this cannot be excluded from itself: $excludes"
				)
			else this

		override def include(components :Iterable[TypedMapping[_, F]]) :TypedColumnComponentSQL[F, T, R, M, V, L] =
			this

		override def include(components :M[F] => TypedMapping[_, F]*) :TypedColumnComponentSQL[F, T, R, M, V, L] =
			this
//
//		@throws[UnsupportedOperationException]("if the exclude list is non empty.")
//		override def exclude(components :Iterable[TypedMapping[_, F]]) :TypedColumnComponentSQL[F, T, R, M, S, L] =
//			if (components.nonEmpty)
//				alter(Nil, components)
//			else this
//
//		@throws[UnsupportedOperationException]("if the component list is non empty.")
//		override def exclude(components :M[F] => TypedMapping[_, F]*) :TypedColumnComponentSQL[F, T, R, M, S, L] =
//			super.exclude(components :_*)

		override def alter(includes :Iterable[TypedMapping[_, F]], excludes :Iterable[TypedMapping[_, F]])
				:TypedColumnComponentSQL[F, T, R, M, V, L] =
			if (excludes.nonEmpty)
				throw new UnsupportedOperationException(
					s"A column expression $this cannot be excluded from itself: $excludes."
				)
			else this

//		override def alter(components :M[F] => ComponentSelection[_, F]*) :TypedColumnComponentSQL[F, T, R, M, V, L] =
//			super.alter(components :_*)

		override def alter(columns :Unique[TypedColumn[_, Origin]]) :TypedColumnComponentSQL[F, T, R, M, V, L] =
			if (columns.size == 1 && columns.head == mapping)
				this
			else
				throw new UnsupportedOperationException(s"A column expression $this cannot have its column set altered.")

//		override def alterLike[E <: RowProduct](template :JoinedRelation[E, M])
//				:TypedColumnComponentSQL[F, T, R, M, V, L] =
//			graft(template.alterOther[F, F, T, L, RelationSQL[F, T, R, L]](origin, this))

		override def +(component :M[F] => TypedMapping[_, F]) :TypedColumnComponentSQL[F, T, R, M, V, L] = this

//		@throws[UnsupportedOperationException]("always.")
//		override def -(component :M[F] => TypedMapping[_, F]) :TypedColumnComponentSQL[F, T, R, M, S, L] =
//			throw new UnsupportedOperationException(
//				s"A column expression $this cannot be excluded from itself: ${component(mapping)}."
//			)


		override def substitute[E <: F]
		                       (substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V] =
		{
			def reject() =
				throw new IllegalArgumentException(
					"Attempted to substitute in " + this + " a different column than " + mapping + ": " + substitutes + "."
				)
			//todo: use EditedColumnSQL; problem: it uses the Subject type, not the rvalue type as the value type
			if (substitutes.isEmpty)
				this
			else if (substitutes.sizeIs == 1)
				if (origin.export.export(substitutes.head.lvalue.mapping.refine.withOrigin[Origin]) == export)
					EditedComponentSQL[E, T, R, M, V](this)(substitutes.toSeq)
				else
					reject()
			else if (substitutes.forall { setter =>
				origin.export.export(setter.lvalue.mapping.refine.withOrigin[Origin]) == export
			}) {
				val last = substitutes.last
				EditedComponentSQL[E, T, R, M, V](this)(last :: Nil)
			} else
				reject()
		}


//		override def aliased(aliases :Map[TypedColumn[_, F], String]) :TypedColumnComponentSQL[F, T, R, M, S, L] =
//			graft(origin.aliased(aliases))
//
		override def reorder(permutation :IndexedSeq[Int]) :TypedColumnComponentSQL[F, T, R, M, V, L] =
			if (permutation.sizeIs != 1 || permutation.head != 0)
				throw new IllegalArgumentException(
					"The only valid permutation of a column is Seq(0): " + permutation + "."
				)
			else
				this

		override def reorder(precedes :(TypedColumn[_, F], TypedColumn[_, F]) => Boolean)
				:TypedColumnComponentSQL[F, T, R, M, V, L] =
			this

//		override def makeCustom :TypedColumnComponentSQL[F, T, R, M, S, L] = graft(origin.makeCustom)
//
//		protected override def makeFinal :TypedColumnComponentSQL[F, T, R, M, S, L] = graft(origin.`->makeFinal`)
//
//		override def anchor(from :F) :TypedColumnComponentSQL[F, T, R, M, S, L] =
//			graft(origin.anchor(from))
//
//		override def anchor(relation :Relation[T]) :TypedColumnComponentSQL[F, T, R, M, S, L] =
//			graft(origin.anchor(relation))

		override def graft[P <: RowProduct](relation :JoinedRelation[P, T])
				:TypedColumnComponentSQL[P, T, R, M, V, L] =
			if (relation eq origin)
				this.asInstanceOf[TypedColumnComponentSQL[P, T, R, M, V, L]]
			else if (relation.mapping == origin.mapping) //can safely use this.mapping
				relation.typed \ mapping.withOrigin[P]
			else if (!(relation.mapping.columns isomorphic origin.mapping.columns))
				throw new IncompatibleMappingsException(
					"Cannot graft column expression " + this + " onto relation expression " + relation +
					" because its relation's " + relation.relation + " mapping is not equal to this instance's relation's " +
					this.relation + " mapping and the column lists do not match."
				)
			else this.relation.row[Origin].columns.indexOf(export) match { //todo: use Mapping.counterpart instead
				case n if n >= 0 =>
					relation.typed \ relation.mapping.columns(n).asInstanceOf[M[P]]
				case _ =>
					throw Bug("The mapping " + mapping + " of column expression " + this +
					          " is not among columns of its origin's mapping " + origin.mapping + "???")
			}
//
//		override def moveTo[E <: RowProduct](offset :RelationOffset[E, T] { type First = FromLast })
//				:TypedColumnComponentSQL[E, T, R, M, S, L] =
//			graft(origin.moveTo(offset))
//
//
//		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
//				:TypedColumnComponentSQL[_ >: E <: RowProduct, T, R, M, S, L] =
//			expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])
//
//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit ext :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//				:TypedColumnComponentSQL[_ >: E <: RowProduct, T, R, M, S, L] =
//			{ val res = graft(origin.expand(base).origin); res } //type inference without expected type
//
//		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E)
//				:TypedColumnComponentSQL[_ >: E <: RowProduct, T, R, M, S, L] =
//			{ val res = graft(origin.expand[E](expansion :E ExpandedBy E).origin); res }
//
//		override def asLast :TypedColumnComponentSQL[FromLast, T, R, M, S, FromLast] =
//			moveTo(RelationOffset.unsafe[FromLast, FromLast, origin.position.Rel, T](0))


		override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E) :TopSelectColumnAs[M, V] =
			SelectSQL(from, this)

		override def subselectFrom[B <: NonEmptyRow]
		                          (from :F ProperSubselectOf B) :SubselectColumnAs[B, M, V] =
			SelectSQL.subselect[B, from.type, M, V](from, this)


		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyColumnVisitor[F, Y]) :Y[Single, V] =
			visitor.columnComponent(this)

		protected override def visit[Y](visitor :SpecificColumnVisitor[F, Single, V, Y]) :Y =
			visitor.columnComponent(this)
//
//		protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//		                             E >: TypedColumnComponentSQL[F_, T, R, M, V, L] <: SQLExpression[F_, S_, V],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//			visitor.grouping(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TypedColumnComponentSQL.__ @unchecked]

		protected[sql] override
		def all_subclasses_of_ColumnLValueSQL_must_extend_TypedColumnSQL_or_LooseColumn(seal :Seal) :Unit = ()
	}




	object TypedColumnComponentSQL {

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, C <: ColumnAt[F], V, L <: RowProduct]
		         (rel :RelationSQL[F, T, R, L], column :C)
		         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: BaseColumn[V, A] })
				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, V, L] =
		{
			val projected = project[F](column)
			import project.isomorphism
			new ProperColumn[F, T, R, project.WithOrigin, V, rel.FromLast](rel, projected)
		}


		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[O, T, R, L], ColumnMappingExtract[R, X, O]) forSome {
					type O >: F <: RowProduct; type T[O] <: BaseMapping[R, O]; type R; type L <: RowProduct Adjoin T
				}] =
			e match {
				case component :TypedColumnComponentSQL.__ @unchecked =>
					Got((component.origin, component.extract).asInstanceOf[(
						RelationSQL[F, MappingOf[Any]#TypedProjection, Any, RowProduct Adjoin MappingOf[Any]#TypedProjection],
							ColumnMappingExtract[Any, X, F]
					)])
				case _ => Lack
			}


		/** A supertype of all
		  * [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL TypedColumnComponentSQL]]
		  * expressions, with wildcard types as type arguments.
		  */
		type __ = TypedColumnComponentSQL[F, T, R, M, V, L] forSome {
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseColumn[V, A]; type V
			type F <: RowProduct; type L <: RowProduct
		}

		type AnyIn[F <: RowProduct] = TypedColumnComponentSQL[O, T, R, M, V, L] forSome {
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseColumn[V, A]; type V
			type O >: F <: RowProduct; type L <: RowProduct
		}

		type Typed[F <: RowProduct, V] = TypedColumnComponentSQL[O, T, R, M, V, L] forSome {
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseColumn[V, A]
			type O >: F <: RowProduct; type L <: RowProduct
		}


		private class ProperColumn[F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V,
		                           L <: RowProduct]
		                          (override val origin :RelationSQL[F, T, R, L], override val mapping :M[F])
		                          (implicit override val projection :IsomorphicProjection[M, V, F])
			extends TypedColumnComponentSQL[F, T, R, M, V, L] with RowShapeCache
		{
			override def relation      :Relation[T] = origin.relation
			override val extract :ColumnMappingExtract[R, V, F] = origin.export(mapping)
			override val export        = extract.export
			override lazy val altered  = origin.altered.export(mapping)
			override lazy val anchored = origin.anchored.export(mapping)

			//fixme: sort out where the buff-related modifications take place to have consistent assembly semantics.
			//  make sure this method won't throw an exception or handle it.
			override val selectForm = super.selectForm
		}



		trait SpecificColumnComponentVisitor[+F <: RowProduct, X, +Y] {
			def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[X, A], L <: RowProduct]
			                   (e :TypedColumnComponentSQL[O, T, R, M, X, L]) :Y
		}
		type MatchSpecificColumnComponent[+F <: RowProduct, X, +Y] = SpecificColumnComponentVisitor[F, X, Y]
		type CaseSpecificColumnComponent[+F <: RowProduct, X, +Y] = SpecificColumnComponentVisitor[F, X, Y]
//
//		trait ColumnComponentVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//			def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
//			                   (e :TypedColumnComponentSQL[O, T, R, M, V, L])
//					:Y[Single, V, TypedColumnComponentSQL[O, T, R, M, V, L]]
//		}
//		type MatchColumnComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//			ColumnComponentVisitor[F, Y]
//		type CaseColumnComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//			ColumnComponentVisitor[F, Y]

		trait AnyColumnComponentVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
			def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
			                   (e :TypedColumnComponentSQL[O, T, R, M, V, L]) :Y[Single, V]
		}
		type MatchAnyColumnComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
			AnyColumnComponentVisitor[F, Y]

		type CaseAnyColumnComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
			AnyColumnComponentVisitor[F, Y]
	}

}

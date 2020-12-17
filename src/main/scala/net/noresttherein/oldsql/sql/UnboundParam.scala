package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, GenericExtract, Mapping, MappingExtract, Relation, SQLForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.{PseudoRelation, StaticRelation, Table}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate
import net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, NonEmptyFrom, PartOf, PrefixOf, TopFrom}
import net.noresttherein.oldsql.sql.Expanded.{AbstractExpanded, ExpandedComposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromSome.TopFromSome
import net.noresttherein.oldsql.sql.GroupBy.AndBy
import net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, NamedParamRelation, ParamAt, ParamRelation}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{RelationSQL, TypedColumnComponentSQL, TypedComponentSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.{RowProductMatcher, SQLScribe}






/** Base trait for ''unbound'' query parameters, that is parameters without a known value, in contrast 
  * to the ''bound'' [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter SQLParameter]] expression.
  * It is represented as a special kind of join between an existing `RowProduct` on the left, and a synthetic
  * `Mapping` subtype [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]].
  * Two concrete subclasses exist: [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] for ''discrete'' clauses
  * (''from'' clauses without a ''group by'' clause) and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
  * for clauses with a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] join to the left.
  */ //consider: make the right side simply X and fix the mapping type to ParamRelation[X]#Param
sealed trait UnboundParam[+F <: NonEmptyFrom, P[O] <: ParamAt[O]] extends NonSubselect[F, P] { thisClause =>
	//consider: make it a WithParam, i.e. require all params to be in the WithClause
	/** A synthetic relation for this parameter. It is never shared with over instances,
	  * with every unbound parameter having its own copy. Using it directly */
	override def right :Relation[P] = last.relation //overriden for docs

	override def lastAsIn[E <: RowProduct](implicit expansion :FromLast PrefixOf E) :Last[E] =
		last.asIn[E]


	override type Last[O <: RowProduct] = JoinedRelation[O, P]

	override type Generalized >: Dealiased <: (left.Generalized UnboundParam P) {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
	}

	type Dealiased >: Self <: (left.Self UnboundParam P) {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: (left.Self UnboundParam P) {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}



	override def lastParamOffset = 0
	override def isParameterized :Boolean = true
	override def isSubselectParameterized :Boolean = true

	/** The type of this parameter, that is the subject type of the joined mapping. */
	type Param = last.Subject //P[FromLast]#Subject

	override type LastParam = Param
	override type Params = left.Params ~ Param
	override type AppliedParam = left.Copy
	override type Paramless = left.Paramless
	override type DecoratedParamless[D <: BoundParamless] = Paramless

	override protected def decoratedBind[D <: BoundParamless]
	                                    (params :Params)(decorate :Paramless => D) :left.Paramless =
		bind(params)

	override type FullRow = left.FullRow

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.fullRow(target)(expansion.expandFront[left.Generalized, P])



	override type JoinedWithSubselect[+S <: NonEmptyFrom] = Nothing

	override def joinedWithSubselect[S <: RowProduct](prefix :S) :Nothing =
		throw new UnsupportedOperationException(
			"JoinParam.joinedWithSubselect: join parameters cannot appear as a part of a subselect clause. " +
				s"$this joinedWithSubselect $prefix"
		)



	override def isValidSubselect = false //either we are a top clause and truly not a subselect, or we are illegal for subselect

	override type Base = Nothing
	override type DefineBase[+I <: RowProduct] = Nothing
	override def base = throw new UnsupportedOperationException(s"JoinParam.base on $this")


	override type Row = left.Row

	override def row[E <: RowProduct]
	             (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, Row] =
		left.row(target)(expansion.expandFront[left.Generalized, P])


	override type AsSubselectOf[+O <: NonEmptyFrom] = Nothing

	override def asSubselectOf[O <: RowProduct](outer :O)(implicit expansion :Implicit ExpandedBy O) :Nothing =
		throw new UnsupportedOperationException(
			"JoinParam.asSubselectOf: join parameters can't appear as a part of a subselect from clause. " +
				s"$this asSubselectOf $outer"
		)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[UnboundParam.* @unchecked]

	override def name = "param"

}






/** Types used by the [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]] 'join' and its subtypes. */
object UnboundParam {

	/** Matches all `UnboundParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: NonEmptyFrom, P[O] <: ParamAt[O]](param :F UnboundParam P) :Option[(F, Relation[P])] =
		Some(param.left -> param.right)

	/** Matches all `UnboundParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: RowProduct, X](from :F Adjoin M forSome { type M[O] <: RefinedMapping[X, O] })
			:Option[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :UnboundParam[_, ParamRelation[X]#Param @unchecked] => Some((from.left, param.right))
			case _ => None
		}

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :RowProduct) :Option[(RowProduct, Relation[ParamRelation[_]#Param])] =
		from match {
			case param :UnboundParam.* @unchecked => Some(param.left -> param.right)
			case _ => None
		}




	/** Type alias for `UnboundParam` with erased type parameters, covering all instances of `UnboundParam`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = UnboundParam[_ <: NonEmptyFrom, ParamRelation[_]#Param]

	/** A curried type constructor for `UnboundParam` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: NonEmptyFrom] = { type F[R[O] <: ParamAt[O]] = L UnboundParam R }

	/** A curried type constructor for `UnboundParam` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: NonEmptyFrom] = L UnboundParam R }






	sealed abstract class GenericParamRelation[X, M[O] <: FromParam[X, O]]
	                                          (val name :String)(implicit val form :SQLForm[X])
		extends PseudoRelation[M]
	{
		type Param[O] = M[O]

		override def sql :String = name + "?:" + form

//		override def canEqual(that :Any) :Boolean

		override def equals(that :Any) :Boolean = that match {
			case other :ParamRelation.* =>
				(other eq this) || other.canEqual(this) && other.name == name && other.form == form
			case _ => false
		}

		override def hashCode :Int = name.hashCode * 31 + form.hashCode
	}



	/** A special, artificial relation implementation dedicated to
	  * the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] mapping class,
	  * representing a query parameter.
	  */
	sealed class ParamRelation[X](val name :String)(implicit val form :SQLForm[X])
		extends PseudoRelation[({ type P[O] = FromParam[X, O] })#P]
	{
		type Param[O] = FromParam[X, O]

		private[this] val param = new FromParam[X, Any](name)

		override def apply[O] :FromParam[X, O] = param.asInstanceOf[FromParam[X, O]]
		override def export[O] :FromParam[X, O] = apply[O]

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:ParamRelation[X] =
			this

		override def sql :String = name + "?:" + form

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamRelation.*]

		override def equals(that :Any) :Boolean = that match {
			case other :ParamRelation.* =>
				(other eq this) || other.canEqual(this) && other.name == name && other.form == form
			case _ => false
		}

		override def hashCode :Int = name.hashCode * 31 + form.hashCode
	}


	object ParamRelation {
		def apply[X :SQLForm](name :String) :ParamRelation[X] = new ParamRelation[X](name)

		def apply[X :SQLForm]() :ParamRelation[X] = ParamRelation("_")

		def unapply(source :Relation.*) :Option[(SQLForm[_], String)] = source match {
			case param :ParamRelation.* => Some(param.form -> param.name)
			case _ => None
		}

		type * = ParamRelation[_]
	}



	/** A special, artificial relation dedicated to
	  * the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] mapping class, representing a query
	  * parameter with the same name `N` as this relation. When using this relation to create
	  * an [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]] instance, this name is taken for the
	  * [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause of the relation.
	  */
	sealed class NamedParamRelation[N <: Label, X :SQLForm](override val name :N)
		extends ParamRelation[X](name) with StaticRelation[N, ({ type P[O] = FromParam[X, O] })#P]
	{
		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:NamedParamRelation[N, X] =
			this

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[NamedParamRelation[_, _]]
	}


	object NamedParamRelation {
		def apply[N <: Label, X :SQLForm](name :N) :NamedParamRelation[N, X] =
			NamedParamRelation[N, X]()(SQLForm[X], new ValueOf(name))

		//the order of implicits reversed to avoid double definition with the one above.
		def apply[N <: Label, X]()(implicit form :SQLForm[X], name :ValueOf[N]) :NamedParamRelation[N, X] =
			new NamedParamRelation[N, X](valueOf[N])

		def unapply(source :Relation.*) :Option[(SQLForm[_], String)] = source match {
			case param :NamedParamRelation[_, _] => Some(param.form -> param.name)
			case _ => None
		}

		type * = NamedParamRelation[_, _]
	}






	type ParamExtract[P, S, O] = GenericExtract[ParamMapping[P, S, O], P, S, O]
	type ParamColumnExtract[P, S, O] = GenericExtract[FromParam[P, O]#ParamColumn[S], P, S, O]

	sealed abstract class ParamMapping[P, S, O] protected (implicit sqlForm :SQLForm[S]) extends FormMapping[S, O] {
		def root :FromParam[P, O]
		def extract :ParamExtract[P, S, O]
		def derivedForm :SQLWriteForm[P] = form compose extract
	}



	/** The generic type supertype of `FromParam` mappings representing SQL statement parameters,
	  * used as the upper bound for the `Mapping`'s used by `JoinParam` to avoid wildcard types of `FromParam[_, O]`,
	  * which break the type compatibility of the subject (parameter) type. The reference to the real downcast type
	  * can be obtained using the `root` method.
	  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
	  */
	sealed trait ParamAt[O] extends Mapping {
		override type Origin = O
		val form :SQLForm[Subject]
		
		def root :FromParam[Subject, O] 
	}



	/** A `Mapping` type representing a query parameter, the value of which is not known.
	  * While the [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter SQLParameter]] expression can be used to represent
	  * a statement parameter, its value must be known when the expression is created. By representing it instead
	  * as a mapping that can be used in the same way as table mappings in `RowProduct` relation lists, we can represent
	  * any value obtainable from `P` by a function `P => T` as a component `FromParam[P, _]#Component[T]`
	  * wrapping that function, which can be used to create component expressions for that function. In particular,
	  * a [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation JoinedRelation]]`[F, ParamRelation[P]#Param]`
	  * is a expression which value will be substituted by a statement parameter `P`.
	  *
	  * This implementation breaks the `Mapping` contract in several ways due to its narrow use case,
	  * and the inherited interface should not be generally used by the application. In particular,
	  * its component and column lists are always empty, even if derived parameters where created as components,
	  * although all created components are recognized correctly as such by the exporting method and `MappingExtract`s
	  * for them are available as normal.
	  * @param name a suggested name of the parameter for debugging purposes, which may, but doesn't have to be used
	  *             for the name of the parameter in the generated SQL.
	  * @tparam P the parameter type needed to prepare statements using this mapping in their ''from'' clauses.
	  * @tparam O a marker type serving as a unique identifier for this mapping within a `RowProduct`.
	  *///consider: think of another name, this can be confusing
	class FromParam[P, O](val name :String)(implicit sqlForm :SQLForm[P])
		extends ParamMapping[P, P, O] with ParamAt[O]
	{ This =>

		def this()(implicit form :SQLForm[P]) = this("?")

		override def root :FromParam[P, O] = this
		override def extract :ParamExtract[P, P, O] = GenericExtract.ident(this)
		override def derivedForm :SQLWriteForm[P] = form


		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[P] =
			SQLWriteForm.combine(
				components.toSeq.map {
					case This(param) => param.derivedForm
					case comp => throw new IllegalArgumentException(s"Mapping $comp is not a component of parameter mapping $this")
				} :_*
			)



		override def apply[T](component :Component[T]) :Extract[T] = component match {
			case self :AnyRef if self eq this =>
				extract.asInstanceOf[MappingExtract[P, T, O]]
			case mapping :FromParam[_, _]#ParamComponent[_] if mapping.root eq this =>
				mapping.asInstanceOf[ParamComponent[T]].extract
			case _ =>
				throw new IllegalArgumentException(s"Mapping $component is not a part of parameter mapping $this")
		}


		override def apply[T](column :Column[T]) :ColumnExtract[T] = column match {
			case mapping :FromParam[_, _]#ParamComponent[_] if mapping.root eq this =>
				mapping.extract.asInstanceOf[ColumnExtract[T]]
			case _ =>
				throw new IllegalArgumentException(s"Column $column is not a part of parameter mapping $this")
		}




		//todo: this almost certainly will fool type inferer as Extractor is a SAM type
		/** Create an artificial component with subject type `T`, extractable from the parameter type.
		  * The component is ''not'' listed on any of the component lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
		def apply[T :SQLForm](pick :P =?> T) :TypedComponent[T] = new ParamComponent[T](pick)

		/** Create an artificial component with subject type `T`, extractable from the parameter type.
		  * The component is ''not'' listed on any of the component lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
		def apply[T :SQLForm](pick :P => T) :TypedComponent[T] = new ParamComponent[T](Extractor.req(pick))

		/** Create an artificial component with subject type `T`, extractable from the parameter type.
		  * The component is ''not'' listed on any of the component lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
		def opt[T :SQLForm](pick :P => Option[T]) :TypedComponent[T] = new ParamComponent[T](Extractor(pick))



		/** Create an artificial column with subject type `T`, extractable from the parameter type.
		  * The column is ''not'' listed on any of the column lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
		def col[T :ColumnForm](pick :P =?> T) :Column[T] = new ParamColumn[T](pick)

		/** Create an artificial column with subject type `T`, extractable from the parameter type.
		  * The column is ''not'' listed on any of the column lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
		def col[T :ColumnForm](pick :P => T) :Column[T] = new ParamColumn[T](Extractor.req(pick))

		/** Create an artificial column with subject type `T`, extractable from the parameter type.
		  * The column is ''not'' listed on any of the column lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
		def optcol[T :ColumnForm](pick :P => Option[T]) :Column[T] = new ParamColumn[T](Extractor(pick))



		/** Represents this parameter mapping as its own column, providing an implicit `ColumnForm[P]` is present.
		  * This method exists because all parameters are created as general mappings, allowing multiple column
		  * forms. It can be used if the parameter is in reality a simple type and there is a need to use it
		  * directly in SQL as an atomic value.
		  */
		def toColumn(implicit form :ColumnForm[P]) :Column[P] = new ParamColumn[P](Extractor.ident[P])



		/** A value derived from the query parameter `P`, represented as a pseudo component of an artificial
		  * mapping in SQL expressions.
		  */
		class ParamComponent[T :SQLForm] private[FromParam] (pick :P =?> T)
			extends ParamMapping[P, T, O]
		{
			override def root :FromParam[P, O] = This
			override def extract :ParamExtract[P, T, O] = GenericExtract(this)(pick)
			override def toString = s"$This[$form]"
		}



		/** A column value derived from the query parameter `P`, represented as a pseudo column of an artificial
		  * mapping in SQL expressions.
		  */
		class ParamColumn[T] private[FromParam] (pick :P =?> T)(implicit override val form :ColumnForm[T])
			extends ParamComponent[T](pick) with ColumnMapping[T, O]
		{
			override def extract :ParamColumnExtract[P, T, O] = GenericExtract(this)(pick)
			override def name :String = This.name
		}


		override def mappingName :String = name
		override def toString :String = name + ":" + form



		def unapply[X](expr :ColumnSQL[_, _, X]) :Option[ParamColumn[X]] = expr match {
			case TypedComponentSQL(_, MappingExtract(_, _, col :FromParam[_, _]#ParamColumn[_])) if col.root == this =>
				Some(col.asInstanceOf[ParamColumn[X]])
			case _ =>
				None
		}

		def unapply[X](expr :SQLExpression[_, _, X]) :Option[ParamMapping[P, X, O]] = expr match {
			case TypedComponentSQL(_, MappingExtract(_, _, comp :ParamMapping[_, _, _])) if comp.root == this =>
				Some(comp.asInstanceOf[ParamMapping[P, X, O]])
			case _ => None
		}

		def unapply[X](column :Column[X]) :Option[ParamColumn[X]] = column match {
			case param :FromParam[_, _]#ParamColumn[_] if param.root == this =>
				Some(param.asInstanceOf[ParamColumn[X]])
			case _ =>
				None
		}

		def unapply[X](component :Component[X]) :Option[ParamMapping[P, X, O]] = component match {
			case param :ParamMapping[_, _, _] if param.root == this =>
				Some(param.asInstanceOf[ParamComponent[X]])
			case _ => None
		}



		/** An extractor matching `ComponentSQL` expressions for components of this mapping,
		  * that is actual sql statement parameters.
		  */
		def ParamForm :Extractor[SQLExpression[_, _, _], SQLWriteForm[P]] = Extractor.Optional(
			(sql :SQLExpression[_, _, _]) => unapply(sql).map(_.derivedForm)
		)

	}






	object FromParam {

		def apply[P :SQLForm, O] :FromParam[P, O] = new FromParam

		def apply[P :SQLForm, O](name :String) :FromParam[P, O] =
			new FromParam(name)

		type Of[S] = { type P[O] = FromParam[S, O] }
	}



	class LabeledFromParam[N <: Label, X :SQLForm, O](override val name :N)
		extends FromParam[X, O](name) with LabeledMapping[N, X, O]


	object LabeledFromParam {

		def apply[P :SQLForm, N <: Label, O](name :N) :FromParam[P, O] = new LabeledFromParam(name)

		type Projection[N <: Label, S] = { type WithOrigin[O] = LabeledFromParam[N, S, O] }
	}



	/** Pattern matching SQL expressions representing unbound parameters and expressions directly derived from them
	  * by application of some function. It extracts both the root `FromParam` and the `MappingExtract` for its component.
	  */
	object UnboundParamSQL {

		def unapply[F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		           (expr :TypedColumnComponentSQL[F, T, E, M, V, O])
				:Option[(FromParam[E, O], ParamColumnExtract[E, V, O], Int)] =
			expr.extract.export match {
				case param :FromParam[E @unchecked, O @unchecked]#ParamColumn[V @unchecked] =>
					Some((param.root, param.extract, expr.origin.offset))
				case _ =>
					None
			}

		def unapply[F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		           (expr :TypedComponentSQL[_, T, E, M, V, O]) :Option[(FromParam[E, O], ParamExtract[E, V, O], Int)] =
			expr.extract.export match {
				case param :ParamMapping[E @unchecked, V @unchecked, O @unchecked] =>
					Some((param.root, param.extract, expr.origin.offset))
				case _ =>
					None
			}

		def unapply[X](expr :SQLExpression[_, _, X])
				:Option[(FromParam[P, O], ParamExtract[P, X, O], Int)] forSome { type P; type O } =
			expr match {
				case TypedComponentSQL(table, extractor) if extractor.export.isInstanceOf[ParamMapping[_, _, _]] =>
					val param = extractor.export.asInstanceOf[ParamMapping[Any, X, Any]]
					Some((param.root, param.extract, table.offset))
				case _ => None
			}

	}
}









/** A special, artificial 'join' type which joins the clause on its left side with a synthetic mapping
  * `P[O] <: FromParam[X, O]`, representing a query parameter `X`, unspecified at this point. To distinguish
  * it from the ''bound'' [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter SQLParameter]] SQL expression, which
  * translates to a statement parameter, but requires a value on creation, it is often referred to as an ''unbound'' 
  * parameter. It allows to filter a given ''from'' clause using values to be provided only at the execution time, 
  * which can be obtained by applying an arbitrary scala function to `X`.
  *
  * This type represents an ungrouped ''from'' clause (i.e., without a ''group by'' clause), and hence it can only
  * be used following another [[net.noresttherein.oldsql.sql.FromSome ungrouped]] clause; a mirror type
  * exists in the form of [[net.noresttherein.oldsql.sql.GroupParam GroupParam]], which offers the same functionality,
  * but for parameters used in the ''group by''/''having'' clauses. The mapping of the synthetic relation in this clause,
  * aside from representing the parameter value itself, can also be used to create additional subcomponents with values
  * derived from the parameter value, which can be used in an `SQLExpression` as any other component. The mappings
  * themselves are however only shills, replaced when creating the select statement, and only their associated
  * `SQLForm`s are being used in the mapping process. The type constructor for a parameter mapping with type `X` is
  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.Of FromParam.Of[X]#P]].
  *
  * This join is typically written in an abbreviated form `FromSome WithParam X` (or `FromSome <=? X`)
  * and `FromSome JoinParam ("name" ?: X)#P` for a parameter of type `X` named with a string literal.
  * A ''from'' clause can have unbound parameters only in its most outer section, before any 
  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins (and, in case of this type, before the
  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause if present) in order to be valid.
  * While it is possible to create instances which violate this property, both due to technical limitations and
  * in order to allow 'placeholder' parameters on temporary, component instances, they will be impossible to
  * use in a [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]] expression. It is so because
  * otherwise such a subselect expression used under its outer select would hide the existence of an unbound parameter
  * from the outside, preventing providing a value for it and making the outer clause appear parameterless
  * (and hence a valid ''free'' select or subselect clause. This is enforced
  * by the [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[F]` type, used to define valid
  * subselect clauses of a clause `F`.
  *
  * @tparam F the actual ''from'' clause of the parameterized select statement, used as the left side of the 'join'.
  * @tparam P a synthetic `FromParam` mapping, the subject of which is the parameter type.
  *           
  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam.WithParam]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */ //lets try to widen the bound to `FromClause` - union types should do it, the problem is the upper bound on WithLeft and ilk.
sealed trait JoinParam[+F <: FromSome, P[O] <: ParamAt[O]]
	extends UnboundParam[F, P] with AndFrom[F, P] with AndFromTemplate[F, P, F JoinParam P]
{ thisClause =>
	//consider: it's tempting to have simply the parameter type as the second parameter, not the mapping,
	// but it would require changes to RowDecomposition, ExpandedBy et al, GetTable...
	override type Generalized = left.Generalized JoinParam P
	override type Dealiased = left.Self JoinParam P
	override type Self <: left.Self JoinParam P

	protected override def narrow :left.type JoinParam P

	override type GeneralizedLeft[+L <: FromSome] = L JoinParam P
	override type DealiasedLeft[+L <: FromSome] = L JoinParam P
	override type WithLeft[+L <: FromSome] <: L JoinParam P


	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :GlobalBoolean[E] =
		left.filter(target)(expansion.expandFront[left.Generalized, P]) && condition.basedOn(target)

	override def bind(param :LastParam) :AppliedParam = {
		val substitute = SQLScribe.applyParam(self, left.generalized, param, 0)
		left.filtered(substitute(condition))
//		left.where(substitute(condition)).asInstanceOf[AppliedParam]
	}

	override def bind(params :Params) :Paramless = {
		val res = left.bind(params.init)
		val substitute = SQLScribe.applyParams(self, res.generalized)(params)
		res.where(substitute(condition)).asInstanceOf[Paramless]
	}

	override def generalizedExpansion[C <: FromSome] :C PrefixOf (C JoinParam P) =
		PrefixOf.itself[C].expand[JoinParam, P]


	override type JoinedWith[+S <: RowProduct, +J[+L <: S, R[O] <: MappingAt[O]] <: L NonParam R] =
		WithLeft[left.JoinedWith[S, J]]

	override def joinedWith[S <: FromSome](prefix :S, firstJoin :Join.*) :JoinedWith[S, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override def appendedTo[S <: FromClause](prefix :S) :JoinedWith[S, NonParam] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit JoinParam P
	override type Inner = left.Inner JoinParam P


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinParam.* @unchecked]

}







object JoinParam {

	/** A template `JoinParam` instance with a dummy mapping, for use as a polymorphic factory of `JoinParam` joins. */
	final val template :JoinParam.* = JoinParam(Relation.Dummy, ParamRelation[Unit]())

	/** An alias for `JoinParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */ //not F <: TopFromSome so it can be used in Generalized types
	type WithParam[+F <: FromSome, X] = JoinParam[F, FromParam.Of[X]#P] //todo: rename, conflicts with param for WithClause

	/** A type alias for `JoinParam` accepting parameter type `X`. As a `RowProduct` containing a `JoinParam` join
	  * in its type is a preliminary from clause which will be translated to a parameterized statement, it uses
	  * an a 'inverse function symbol' as a mnemonic: `From[Users] <=? String`. This is equivalent to `WithParam[F, X]`.
	  */
	type <=?[+F <: FromSome, X] = WithParam[F, X]

	
	
	/** Create an artificial join between the given relation on the left side, and the the `param` special relation 
	  * representing a query parameter of type `X` as the right side. The ''where'' clause can be subsequently specified 
	  * using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[X] ]] DSL instead.
	  * @param from  the first relation of the ''from'' clause, using the `FA[O] <: BaseMapping[A, O]` `Mapping` type.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam[X, _] ]] `Mapping` type.
	  * @param castL implicit witness providing proper type inference for the subject of the left relation
	  *              and conversions of associated classes between instances parameterized with `L` and `LA`.
	  * @tparam F  the type constructor for the mapping of the first relation, accepting the `Origin` type.
	  * @tparam FA the same type as `L`, but with an upper bound of `BaseMapping`, separating the inference of types `L`
	  *            and its subject type `A`.
	  * @tparam X the parameter type - the subject of the synthetic `ParamRelation`.            
	  * @return an unfiltered `F` [[net.noresttherein.oldsql.sql.JoinParam.WithParam WithParam]] `X`.
	  */
	def apply[F[O] <: MappingAt[O], FA[O] <: BaseMapping[A, O], A, X]
	         (from :Table[F], param :ParamRelation[X])
	         (implicit castL :JoinedRelationSubject[From, F, FA, MappingOf[A]#TypedProjection]) :From[F] WithParam X =
		JoinParam(From(from), param, True)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[X] ]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam[X, _] ]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F` [[net.noresttherein.oldsql.sql.JoinParam.WithParam WithParam]] `X`.
	  */
	def apply[F <: TopFromSome, X](from :F, param :ParamRelation[X],
	                               filter :GlobalBoolean[F#Generalized JoinParam ParamRelation[X]#Param] = True)
			:F WithParam X =
		JoinParam[from.type, ParamRelation[X]#Param, X, Nothing](from, LastRelation(param), None)(filter)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[N, X] ]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam[X, _] ]] `Mapping` type.
	  * @return `F JoinParam `[[net.noresttherein.oldsql.sql.UnboundParam.FromParam.Of FromParam.Of[X]#P]]` `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
	  */
	def apply[F <: TopFromSome, N <: Label, X](from :F, param :NamedParamRelation[N, X]) :F WithParam X As N =
		JoinParam[F, FromParam.Of[X]#P, X, N](from, LastRelation(param), Some(param.name))(True)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[N, X] ]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam[X, _] ]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F JoinParam `[[net.noresttherein.oldsql.sql.UnboundParam.FromParam.Of FromParam.Of[X]#P]]` `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
	  */
	def apply[F <: TopFromSome, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :GlobalBoolean[F#Generalized JoinParam ParamRelation[X]#Param]) :F WithParam X As N =
		JoinParam[F, FromParam.Of[X]#P, X, N](from, LastRelation(param), Some(param.name))(filter)


	/** Creates [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: TopFromSome](from :F)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X, Nothing](
				from, LastRelation(ParamRelation[X]()), None)(True)

		def apply[F <: TopFromSome](from :F, name :String)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X, Nothing](
				from, LastRelation(ParamRelation[X](name)), None)(True)
	}



	private[sql] def apply[L <: FromSome, P[O] <: FromParam[X, O], X, A <: Label]
	                      (clause :L, param :LastRelation[P, X], asOpt :Option[A])
	                      (cond :GlobalBoolean[clause.Generalized JoinParam P]) :L JoinParam P As A =
		new JoinParam[clause.type, P] with AbstractExpanded[clause.type, P, X] {
			override val left = clause
			override val last = param
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1
			override def lastRelation = last

			override type Alias = A
			override type DealiasedCopy = left.type JoinParam P
			override type Copy = left.type JoinParam P As A
			override type Self = left.Self JoinParam P As A
			override type WithLeft[+F <: FromSome] = F JoinParam P As A

			override def narrow :left.type JoinParam P As A = this.asInstanceOf[left.type JoinParam P As A]

			override def withCondition(filter :GlobalBoolean[Generalized]) =
				JoinParam[left.type, P, X, A](left, last, aliasOpt)(filter)

			override def withLeft[F <: FromSome](left :F)(filter :GlobalBoolean[left.Generalized JoinParam P]) =
				JoinParam[F, P, X, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				JoinParam[left.type, P, X, N](left, last, Some(alias))(condition)

			override def expansion[C <: FromSome] :C PrefixOf (C JoinParam P As A) =
				PrefixOf.itself[C].expand[JoinParam, P].as[A]


			override def tableStack[E <: RowProduct]
			             (target :E)(implicit stretch :Generalized ExpandedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.expand(target) #:: left.tableStack(target)(stretch.expandFront[left.Generalized, P])



			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.joinParam[L, P, X](this)

		}.asInstanceOf[L JoinParam P As A]






	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: TopFromSome, X](param :F WithParam X) :Option[(F, Relation[ParamRelation[X]#Param])] =
		Some(param.left -> param.right)

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: RowProduct, X](from :F Adjoin M forSome { type M[O] <: RefinedMapping[X, O] })
			:Option[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :JoinParam[_, ParamRelation[X]#Param @unchecked] => Some((from.left, param.right))
			case _ => None
		}

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :RowProduct) :Option[(FromSome, Relation[ParamRelation[_]#Param])] =
		from match {
			case param :JoinParam.* @unchecked => Some(param.left -> param.right)
			case _ => None
		}



	implicit def joinParamComposition[L <: FromSome, R[O] <: ParamAt[O]]
			:ExpandedComposition[L JoinParam R, L, R, JoinParam, FromSome, ParamAt]
				{ type Generalized[+A <: FromSome, B[O] <: ParamAt[O]] = A JoinParam B } =
		composition.asInstanceOf[ExpandedComposition[L JoinParam R, L, R, JoinParam, FromSome, ParamAt] {
			type Generalized[+A <: FromSome, B[O] <: ParamAt[O]] = JoinParam[A, B]
		}]

	private[this] val composition =
		new ExpandedComposition[FromSome JoinParam ParamAt, FromSome, ParamAt, JoinParam, FromSome, ParamAt] {
			override def apply[C <: FromSome](template :FromSome JoinParam ParamAt, clause :C) :C JoinParam ParamAt =
				template.withLeft(clause)(True)
		}



	/** Type alias for `JoinParam` with erased type parameters, covering all instances of `JoinParam`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = JoinParam[_ <: FromSome, ParamRelation[_]#Param]

	/** A curried type constructor for `JoinParam` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: TopFromSome] = {
		type F[R[O] <: ParamAt[O]] = L JoinParam R
		type P[X] = L WithParam X
	}

	/** A curried type constructor for `JoinParam` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: TopFromSome] = L JoinParam R }

}









/** A special, artificial 'join' type which adds a statement parameter of type `X` by expanding a ''group by''
  * clause on its left side with a special mapping `P[O] <: FromParam[X, O]`. The parameter is unspecified at this point
  * and will need to be given to any `SQLStatement` produced from this clause. This type mirrors the functionality
  * of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]], which adds a parameter to the same effect by expanding
  * an [[net.noresttherein.oldsql.sql.FromSome ungrouped]] ''from'' clause. This duplication is required
  * for static type checking to be able to separate the artificial relations for the grouping expressions
  * of the group by clause from the real database relations: just as `JoinParam[_, _] <: FromSome`,
  * `GroupParam[_, _] <: GroupByClause`, both restricting their use and allowing them to be placed in between other
  * relations/grouping expressions. To distinguish it from the ''bound''
  * [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter SQLParameter]] SQL expression, which translates
  * to a statement parameter, but requires a value on creation, it is often referred to as an ''unbound'' parameter
  * (collectively with `JoinParam`). It allows to filter a given ''from'' clause using values to be provided only
  * at the execution time, which can be obtained by applying an arbitrary scala function to `X`. The mapping,
  * aside from representing the parameter value itself, can also be used to create additional subcomponents with values
  * derived from the parameter value, which can be used in an `SQLExpression` as any other component.
  * The mappings themselves are however only shills, replaced when creating the select statement, and only
  * their associated `SQLForm`s are being used in the mapping process. The type constructor for a parameter mapping
  * with type `X` is [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.Of FromParam.Of[X]#P]].
  *
  * This join is typically written in an abbreviated form `GroupByClause ByParam X`
  * and `RowProduct JoinParam ("name" ?: X)#P` for a parameter of type `X` named with a string literal,
  * following the naming scheme of [[net.noresttherein.oldsql.sql.By By]].
  * A ''from'' clause can have unbound grouping parameters only in its most outer section, before any
  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins (and, in case of this type, after the
  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause) in order to be valid.
  * While it is possible to create instances which violate this property, both due to technical limitations and
  * in order to allow 'placeholder' parameters on temporary, component instances, they will be impossible to
  * use in a [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]] expression. It is so because
  * otherwise such a subselect expression used under its outer select would hide the existence of an unbound parameter
  * from the outside, preventing providing a value for it and making the outer clause appear parameterless
  * (and hence a valid ''free'' select or subselect clause. This is enforced
  * by the [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[F]` type, used to define valid
  * subselect clauses of a clause `F`.
  *
  * @tparam F the actual ''group by'' clause of the parameterized select statement, used as the left side of the 'join'.
  * @tparam P a synthetic `FromParam` mapping, the subject of which is the parameter type.
  *
  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam.ByParam]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
sealed trait GroupParam[+F <: GroupByClause, P[O] <: ParamAt[O]]
	extends UnboundParam[F, P] with AndBy[F, P] with GroupByClauseTemplate[F GroupParam P, F GroupParam P]
{ thisClause =>
//consider: making relation protected so it can't be misused
	override type Generalized = left.Generalized GroupParam P
	override type Dealiased = left.Self GroupParam P
	override type Self <: left.Self GroupParam P

	override type GeneralizedLeft[+L <: GroupByClause] = L GroupParam P
	override type DealiasedLeft[+L <: GroupByClause] = L GroupParam P
	override type WithLeft[+L <: GroupByClause] <: L GroupParam P

	override def withLeft[L <: GroupByClause] //overriden to make public
	                     (left :L)(filter :LocalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[L]

	override def bind(param :Param) :AppliedParam = {
		val substitute = SQLScribe.applyParam(self, left.generalized, param, 0)
//		left.having(substitute(condition)).asInstanceOf[AppliedParam]
		left.filtered(substitute(condition))
	}

	override def bind(params :Params) :Paramless = {
		val res = left.bind(params.init)
		val substitute = SQLScribe.applyParams(self, res.generalized)(params)
		res.having(substitute(condition)).asInstanceOf[Paramless]
	}


	override type JoinedWith[+S <: RowProduct, +J[+L <: S, R[O] <: MappingAt[O]] <: L NonParam R] =
		WithLeft[left.JoinedWith[S, J]]

	override def joinedWith[S <: FromSome](prefix :S, firstJoin :Join.*) :JoinedWith[S, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override def appendedTo[S <: FromClause](prefix :S) :JoinedWith[S, NonParam] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit GroupParam P
	override type Inner = left.Inner GroupParam P



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupParam.* @unchecked]

}






object GroupParam {

	/** A template `GroupParam` instance with a dummy mapping, for use as a polymorphic factory of `GroupParam` pseudo joins. */
	final val template :GroupParam.* = GroupParam(GroupBy(From(Relation.Dummy), Relation.Dummy), ParamRelation[Unit]())

	/** An alias for `GroupParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */
	type ByParam[+F <: GroupByClause, X] = GroupParam[F, ParamRelation[X]#Param]



	/** Add a statement parameter to the ''group by'' clause given as the left side, in the form of
	  * the `param` special relation representing a query parameter of type `X` as the right side.
	  * An additional condition for the ''having'' clause can be subsequently specified using the
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] or
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] method.
	  * It is a lower level method; it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.GroupByClause.TopGroupByClauseExtension.param param]]`[X]` DSL instead.
	  * @param from  a ''from''/''group by'' clause containing the non-empty list of relations and grouping expressions
	  *              preceding `param`. It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last pseudo relation of the created ''from'' clause for the introduced parameter,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]`[X, _]` `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F` [[net.noresttherein.oldsql.sql.GroupParam.ByParam ByParam]] `X`.
	  */
	def apply[F <: GroupByClause with TopFrom, X]
	         (from :F, param :ParamRelation[X], filter :LocalBoolean[F#Generalized GroupParam ParamRelation[X]#Param] = True)
			:F ByParam X =
		GroupParam[from.type, ParamRelation[X]#Param, X, Nothing](from, RelationSQL(param, 0), None)(filter)

	/** Add a statement parameter to the ''group by'' clause given as the left side, in the form of
	  * the `param` special relation representing a query parameter of type `X` as the right side.
	  * An additional condition for the ''having'' clause can be subsequently specified using the
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] or
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] method.
	  * It is a lower level method; it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.GroupByClause.TopGroupByClauseExtension.param param]]`[N, X]` DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]`[X, _]` `Mapping` type.
	  * @return `F GroupParam (N `[[net.noresttherein.oldsql.sql.?: ?:]]` X)#P`.
	  */
	def apply[F <: GroupByClause with TopFrom, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X]) :F ByParam X As N =
		GroupParam[F, FromParam.Of[X]#P, X, N](from, RelationSQL(param, 0), Some(param.name))(True)

	/** Add a statement parameter to the ''group by'' clause given as the left side, in the form of
	  * the `param` special relation representing a query parameter of type `X` as the right side.
	  * An additional condition for the ''having'' clause can be subsequently specified using the
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] or
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] method.
	  * It is a lower level method; it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.GroupByClause.TopGroupByClauseExtension.param param]]`[N, X]` DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]`[X, _]` `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F GroupParam (N `[[net.noresttherein.oldsql.sql.?: ?:]]` X)#P`.
	  */
	def apply[F <: GroupByClause with TopFrom, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :LocalBoolean[F#Generalized GroupParam ParamRelation[X]#Param]) :F ByParam X As N =
		GroupParam[F, FromParam.Of[X]#P, X, N](from, RelationSQL(param, 0), Some(param.name))(filter)


	/** Creates [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: GroupByClause with TopFrom](from :F)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X, Nothing](
				from, RelationSQL(ParamRelation[X](), 0), None
			)(True)

		def apply[F <: GroupByClause with TopFrom](from :F, name :String)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X, Nothing](
				from, RelationSQL(ParamRelation[X](name), 0), None
			)(True)
	}



	private[sql] def apply[L <: GroupByClause, P[O] <: FromParam[X, O], X, A <: Label]
	                      (clause :L, param :RelationSQL[GroupByClause AndBy P, P, X, GroupByClause AndBy P],
	                       asOpt :Option[A])
	                      (cond :LocalBoolean[clause.Generalized GroupParam P]) :L GroupParam P As A =
		new GroupParam[clause.type, P] with AbstractExpanded[clause.type, P, X] {
			override val left = clause
			override val last = param
			override val aliasOpt = asOpt
			override val condition = cond
			override val fromClause :Discrete = left.fromClause
			override val outer = left.outer
			override val fullSize = left.fullSize + 1
			override def lastRelation = last

			override type Alias = A
			override type Self = left.Self GroupParam P As A
			override type WithLeft[+F <: GroupByClause] = F GroupParam P As A
			override type DealiasedCopy = left.type GroupParam P
			override type Copy = left.type GroupParam P As A
			override def narrow :left.type GroupParam P As A = this.asInstanceOf[left.type GroupParam P As A]

			override def withCondition(filter :LocalBoolean[Generalized]) =
				GroupParam[left.type, P, X, A](left, last, aliasOpt)(filter)

			override def withLeft[F <: GroupByClause](left :F)(filter :LocalBoolean[left.Generalized GroupParam P]) =
				GroupParam[F, P, X, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				GroupParam[left.type, P, X, N](left, last, Some(alias))(condition)


			override def tableStack[E <: RowProduct]
			             (target :E)(implicit stretch :Generalized ExpandedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.expand(target) #:: left.tableStack(target)(stretch.expandFront[left.Generalized, P])



			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.groupParam[L, P, X](this)

		}.asInstanceOf[L GroupParam P As A]






	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: GroupByClause, X](param :F ByParam X) :Option[(F, Relation[ParamRelation[X]#Param])] =
		Some(param.left -> param.right)

	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: RowProduct, X](from :F Adjoin M forSome { type M[O] <: RefinedMapping[X, O] })
			:Option[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :GroupParam[_, ParamRelation[X]#Param @unchecked] => Some((from.left, param.right))
			case _ => None
		}

	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :RowProduct) :Option[(GroupByClause, Relation[ParamRelation[_]#Param])] =
		from match {
			case param :GroupParam.* @unchecked => Some(param.left -> param.right)
			case _ => None
		}



	implicit def groupParamComposition[L <: GroupByClause, R[O] <: ParamAt[O]]
			:ExpandedComposition[L GroupParam R, L, R, GroupParam, GroupByClause, ParamAt]
				{ type Generalized[+A <: GroupByClause, B[O] <: ParamAt[O]] = A GroupParam B } =
		composition.asInstanceOf[ExpandedComposition[L GroupParam R, L, R, GroupParam, GroupByClause, ParamAt] {
			type Generalized[+A <: GroupByClause, B[O] <: ParamAt[O]] = GroupParam[A, B]
		}]

	private[this] val composition =
		new ExpandedComposition[GroupByClause GroupParam ParamAt, GroupByClause, ParamAt, GroupParam, GroupByClause, ParamAt] {
			override def apply[C <: GroupByClause]
			                  (template :GroupByClause GroupParam ParamAt, clause :C) :C GroupParam ParamAt =
				template.withLeft(clause)(True)
		}


	
	/** Type alias for `GroupParam` with erased type parameters, covering all instances of `GroupParam`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = GroupParam[_ <: GroupByClause, ParamRelation[_]#Param]

	/** A curried type constructor for `GroupParam` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: GroupByClause with TopFrom] = {
		type F[R[O] <: ParamAt[O]] = L GroupParam R
		type P[X] = L ByParam X
	}

	/** A curried type constructor for `GroupParam` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: GroupByClause with TopFrom] = L GroupParam R }
	
}


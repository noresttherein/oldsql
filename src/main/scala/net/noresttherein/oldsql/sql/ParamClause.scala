package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.{Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.collection.Unique.EmptyUnique
import net.noresttherein.oldsql.morsels.{ChunkedString, Extractor}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.generic.FunctionOf
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, GenericExtract, Mapping, MappingExtract, Relation, SQLForm, SQLWriteForm, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.{NamedRelation, PseudoRelation, StaticRelation}
import net.noresttherein.oldsql.schema.SQLForm.FormBasedFactory
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject
import net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate
import net.noresttherein.oldsql.sql.Expanded.{AbstractExpanded, ExpandedComposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromClause.FromClauseTemplate
import net.noresttherein.oldsql.sql.FromSome.TopFromSome
import net.noresttherein.oldsql.sql.GroupByClause.{GroupByClauseTemplate, TopGroupByClause}
import net.noresttherein.oldsql.sql.JoinParam.FromParam.EmptyClause
import net.noresttherein.oldsql.sql.ParamClause.{NamedParamRelation, ParamAt, ParamRelation, UnboundParam}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, NonEmptyFrom, NonEmptyFromTemplate, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.GroupingSpellingContext
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.{JoinedRelation, ParamSQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.ParamSQL.LastParam
import net.noresttherein.oldsql.sql.ast.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






//todo: a type alias being a bound of all RowProduct subtypes consisting solely of JoinParams
/** Base trait for ''unbound'' query parameters, that is parameters without a known value, in contrast
  * to the ''bound'' [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] expression.
  * It is represented as a special kind of join between an existing `RowProduct` on the left, and a synthetic
  * `Mapping` subtype [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]].
  * Two concrete subclasses exist: [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] for ''discrete'' clauses
  * (''from'' clauses without a ''group by'' clause) and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
  * for clauses with a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] join to the left.
  */ //consider: make the right side simply X and fix the mapping type to ParamRelation[X]#Param
sealed trait ParamClause[+F <: NonEmptyFrom, P[O] <: ParamAt[O]] extends NonSubselect[F, P] { thisClause =>
	//todo: make left bound only by RowProduct
	//todo: try to make it accept any table mapping, if the parameter is a mapped entity.
	//consider: make it a WithParam, i.e. require all params to be in the WithClause
	/** A synthetic relation for this parameter. It is never shared with other instances,
	  * with every unbound parameter having its own copy. */
	override def right :Relation[P] = last.relation //overriden for docs

	override def lastAsIn[E <: RowProduct](implicit expansion :FromLast PrefixOf E) :Last[E] =
		last.asIn[E]


	override type Last[O <: RowProduct] = JoinedRelation[O, P]

	override type Generalized >: Dealiased <: (left.Generalized ParamClause P) {
		type FromLast     = thisClause.FromLast
		type Generalized <: thisClause.Generalized
//		type Dealiased   >: Self <: Generalized
		type Explicit    <: thisClause.Explicit
		type Implicit    <: thisClause.Implicit
	}

	type Dealiased >: Self <: (left.Self ParamClause P) {
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
//		type Dealiased  >: Self <: Generalized
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type Base        = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type Self <: (left.Self ParamClause P) {
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
//		type Dealiased  >: Self <: Generalized
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Inner       = thisClause.Inner
		type Implicit    = thisClause.Implicit
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}



	override def paramCount :Int = left.paramCount + 1
	override def lastParamOffset = 0
	override def isParameterized :Boolean = true
	override def isSubselectParameterized :Boolean = true

	/** The type of this parameter, that is the subject type of the joined mapping. */
	type Param = last.Subject

	override type ParamsOnly           = left.ParamsOnly
	override type LastParam            = Param
	override type Params               = left.Params ~ Param
	override type AppliedParam         = left.Copy
	override type GeneralizedParamless = left.GeneralizedParamless
	override type Paramless            = left.Paramless
	override type DecoratedParamless[D <: BoundParamless] = Paramless

	protected override def decoratedBind[D <: BoundParamless]
	                                    (params :Params)(decorate :Paramless => D) :left.Paramless =
		bind(params)

	override type FullRow = left.FullRow

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.fullRow(target)(expansion.expandFront[left.Generalized, P])



	override type SelectedFrom[+S <: NonEmptyFrom] = Nothing

	override def selectedFrom[S <: RowProduct](prefix :S) :Nothing =
		throw new UnsupportedOperationException(
			"ParamClause.selectedFrom: join parameters cannot appear as a part of a subselect clause. " +
				s"$this selectedFrom $prefix"
		)



	override def isValidSubselect = false //either we are a top clause and truly not a subselect, or we are illegal for subselect

	override type Base = Nothing
	override type DefineBase[+I <: RowProduct] = Nothing
	override def base = throw new UnsupportedOperationException(s"ParamClause.base on $this")


	override type Row = left.Row

	override def row[E <: RowProduct]
	             (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, Row] =
		left.row(target)(expansion.expandFront[left.Generalized, P])


	override type AsSubselectOf[+O <: NonEmptyFrom] = Nothing

	override def asSubselectOf[O <: RowProduct](outer :O)(implicit expansion :Implicit ExpandedBy O) :Nothing =
		throw new UnsupportedOperationException(
			"ParamClause.asSubselectOf: join parameters can't appear as a part of a subselect from clause. " +
				s"$this asSubselectOf $outer"
		)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamClause.* @unchecked]

	override def name = "param"

}






/** Types used by the [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] 'join' and its subtypes. */
object ParamClause {

	/** Matches all `ParamClause` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: NonEmptyFrom, P[O] <: ParamAt[O]](param :F ParamClause P) :Opt[(F, Relation[P])] =
		Got(param.left -> param.right)

	/** Matches all `ParamClause` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: RowProduct, X](from :F Adjoin M forSome { type M[O] <: RefinedMapping[X, O] })
			:Opt[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :ParamClause[_, ParamRelation[X]#Param @unchecked] => Got((from.left, param.right))
			case _ => Lack
		}

	/** Matches all `ParamClause` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :RowProduct) :Opt[(RowProduct, Relation[ParamRelation[_]#Param])] =
		from match {
			case param :ParamClause.* @unchecked => Got(param.left -> param.right)
			case _ => Lack
		}




	/** Type alias for `ParamClause` with erased type parameters, covering all instances of `ParamClause`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = ParamClause[_ <: NonEmptyFrom, ParamRelation[_]#Param]

	/** A curried type constructor for `ParamClause` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: NonEmptyFrom] = { type F[R[O] <: ParamAt[O]] = L ParamClause R }

	/** A curried type constructor for `ParamClause` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: NonEmptyFrom] = L ParamClause R }






	/** A special, artificial relation implementation dedicated to
	  * the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] mapping class,
	  * representing am unbound query parameter. Can be joined with any
	  * [[net.noresttherein.oldsql.sql.FromClause FromClause]] with a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]],
	  * and with a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]]
	  * with a [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]. The simplest way of creating a new relation
	  * for a query is with [[net.noresttherein.oldsql.sql.?: ?:]] function from the `sql` package and an extension
	  * method of the same name for `String` literals.
	  * @see [[net.noresttherein.oldsql.sql.ParamClause.NamedParamRelation]]
	  */
	sealed class ParamRelation[X](override val name :String)(implicit val form :SQLForm[X])
		extends PseudoRelation[({ type P[O] = UnboundParam[X, O] })#P] with NamedRelation[({ type P[O] = UnboundParam[X, O] })#P]
	{
		type Param[O] = UnboundParam[X, O]
//		type Last = RowProduct AndFrom ParamRelation[X]#Param

		private[this] val param = new UnboundParam[X, Any](name)
		private[this] val Form = param.formExtractor

		override def apply[O] :UnboundParam[X, O] = param.asInstanceOf[UnboundParam[X, O]]
		override def export[O] :UnboundParam[X, O] = apply[O]

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:ParamRelation[X] =
			this

/*
		protected override def spell[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A], V]
		                            (origin :JoinedRelation[O, T], column :ColumnMapping[V, O])
		                            (context :SQLContext, params :Parameterization[P, F])
		                            (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
			spell(origin, column :MappingAt[O], true)(context, params)

		override def inline[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
		                   (origin :JoinedRelation[O, T], component :MappingAt[O])
		                   (context :SQLContext, params :Parameterization[P, F])
		                   (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, F]] =
			component match {
				case _ if origin.relation != this =>
					throw new IllegalArgumentException(
						s"Cannot spell the expression for component $component of $origin as it is for a different relation than $this."
					)
				case Form(paramForm) =>
					val paramRelation = origin.asInstanceOf[JoinedRelation[O, ParamRelation[X]#Param]]
					val param = params[ParamRelation[X]#Param, X, O](paramRelation)
					val writeForm = paramForm.unmap(param)
					writeForm.writtenColumns match {
						case 0 => Seq.empty
						case 1 => SpelledSQL("?", context, params :+ writeForm)::Nil
						case n => //this is a slight break in contract as all items have the 'complete' setter list
							val resultParams = params :+ writeForm
							Seq.fill(n)(SpelledSQL("?", context, resultParams))
					}
				case _ =>
					throw new IllegalArgumentException(
						s"Mapping $component passed as a component of unbound param $origin is not a ParamMapping for param $param."
					)

			}
*/


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamRelation.*]

		override def equals(that :Any) :Boolean = that match {
			case other :ParamRelation.* =>
				(other eq this) || other.canEqual(this) && other.name == name && other.row == row
			case _ => false
		}
		override def hashCode :Int = name.hashCode * 31 + row.hashCode

		override def refString :String = name
		override def toString :String = name + "?:" + form
	}


	object ParamRelation {
		def apply[X :SQLForm](name :String) :ParamRelation[X] = new ParamRelation[X](name)

		def apply[X :SQLForm]() :ParamRelation[X] = ParamRelation("_")

		def unapply(source :Relation.*) :Opt[(SQLForm[_], String)] = source match {
			case param :ParamRelation.* => Got(param.form -> param.name)
			case _ => Lack
		}

		type * = ParamRelation[_]
	}



	/** A special, artificial relation dedicated to
	  * the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] mapping class, representing a query
	  * parameter with the same name `N` as this relation. It is not different in any functional capacity than
	  * its supertype [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]], but when used to create
	  * a [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] instance, this name is taken for the
	  * [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause of the relation, added automatically to the result.
	  * @tparam N an alias for this parameter relation, used to access it from the larger `RowProduct`.
	  * @tparam X the parameter type, serving as the subject type of the mapping of this relation.
	  */
	sealed class NamedParamRelation[N <: Label, X :SQLForm](override val name :N)
		extends ParamRelation[X](name) with StaticRelation[N, ({ type P[O] = UnboundParam[X, O] })#P]
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

		def unapply(source :Relation.*) :Opt[(SQLForm[_], String)] = source match {
			case param :NamedParamRelation[_, _] => Got(param.form -> param.name)
			case _ => Lack
		}

		type * = NamedParamRelation[_, _]
	}






	type ParamExtract[P, S, O] = GenericExtract[ParamMapping[P, S, O], P, S, O]
	type ParamColumnExtract[P, S, O] = GenericExtract[UnboundParam[P, O]#ParamColumn[S], P, S, O]

	sealed abstract class ParamMapping[P, S :SQLForm, O] protected extends FormMapping[S, O] {
		def root :UnboundParam[P, O]
		def extract :ParamExtract[P, S, O]
		def derivedForm :SQLWriteForm[P] = form compose extract
	}



	/** The generic type supertype of `UnboundParam` mappings representing SQL statement parameters,
	  * used as the upper bound for the `Mapping`'s used by `JoinParam` to avoid wildcard types of `UnboundParam[_, O]`,
	  * which break the type compatibility of the subject (parameter) type. The reference to the real downcast type
	  * can be obtained using the `root` method.
	  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
	  */ //todo: rename to ParamFrom
	sealed trait ParamAt[O] extends Mapping {
		override type Origin = O
		val form :SQLForm[Subject]

		def root :UnboundParam[Subject, O]
	}



	/** A `Mapping` type representing a query parameter, the value of which is not known.
	  * While the [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] expression can be used
	  * to represent a statement parameter, its value must be known when the expression is created. By representing
	  * it instead as a mapping that can be used in the same way as table mappings in `RowProduct` relation lists,
	  * we can represent any value obtainable from `P` by a function `P => T` as a component
	  * `UnboundParam[P, _]#Component[T]` wrapping that function, which can be used to create component expressions
	  * for that function. In particular,
	  * a [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]`[F, ParamRelation[P]#Param]`
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
	class UnboundParam[P, O](val name :String)(implicit sqlForm :SQLForm[P])
		extends ParamMapping[P, P, O] with ParamAt[O]
//		   with FormBasedFactory[FunctionOf[P]#F, MappingAt[O]#TypedComponent, MappingAt[O]#Column]
	{ This =>
		def this()(implicit form :SQLForm[P]) = this("?")

		override def root :UnboundParam[P, O] = this
		override def extract :ParamExtract[P, P, O] = GenericExtract.ident(this)
		override def derivedForm :SQLWriteForm[P] = form

		override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[P] =
			SQLWriteForm.join(
				components.toSeq.map {
					case This(param) => param.derivedForm
					case comp =>
						throw new IllegalArgumentException(s"Mapping $comp is not a component of parameter mapping $this.")
				} :_*
			)

		override def apply[T](component :Component[T]) :Extract[T] = component match {
			case self :AnyRef if self eq this =>
				extract.asInstanceOf[MappingExtract[P, T, O]]
			case mapping :UnboundParam[_, _]#ParamComponent[_] if mapping.root eq this =>
				mapping.asInstanceOf[ParamComponent[T]].extract
			case _ =>
				throw new IllegalArgumentException(s"Mapping $component is not a part of parameter mapping $this.")
		}

		override def apply[T](column :Column[T]) :ColumnExtract[T] = column match {
			case mapping :UnboundParam[_, _]#ParamComponent[_] if mapping.root eq this =>
				mapping.extract.asInstanceOf[ColumnExtract[T]]
			case _ =>
				throw new IllegalArgumentException(s"Column $column is not a part of parameter mapping $this.")
		}


		override object components
			extends EmptyUnique with FormBasedFactory[FunctionOf[P]#F, MappingAt[O]#TypedComponent, MappingAt[O]#Column]
		{
			override def generalResult[T :SQLForm](arg :P => T) = new ParamComponent(arg)
			override def specificResult[T :ColumnForm](arg :P => T) = new ParamColumn(arg)
		}

		//todo: a parameter component compared to or assigned to a mapping component should not require a form
		/** Create an artificial component with subject type `T`, extractable from the parameter type.
		  * The component is ''not'' listed on any of the component lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */ //todo: rename/ in Scala 3 move to macros anc combine with the following
		def comp[T :SQLForm](pick :P =?> T) :TypedComponent[T] = new ParamComponent[T](pick)

		/** Create an artificial component with subject type `T`, extractable from the parameter type.
		  * The component is ''not'' listed on any of the component lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
//		def apply[T :SQLForm](pick :P => T) :TypedComponent[T] = new ParamComponent[T](Extractor.req(pick))

		/** Create an artificial component with subject type `T`, extractable from the parameter type. This component
		  * can be used in SQL expressions the same way as components of table mappings, and is implicitly convertible
		  * to [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[O, GlobalScope, T]`.
		  * This method requires (indirectly) [[net.noresttherein.oldsql.schema.SQLForm]] type class for `T`.
		  * If an implicit [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]`[T]` is present,
		  * the component will be a [[net.noresttherein.oldsql.schema.ColumnMapping column]]; otherwise
		  * it will be of type [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[T, O]`.
		  * The component is ''not'' listed on any of the component lists, but has otherwise the same functionality
		  * as components of mappings for real relations.
		  * @param pick    a function selecting the value of a property of `P` to use in an SQL expression.
		  * @param factory implicit evidence derived from an implicit `SQLForm` which defines
		  *                if the return type will be a `ColumnMapping[O, T]`, or simply `BaseMapping[O, T]`.
		  */
		def apply[T](pick :P => T)(implicit factory :components.DedicatedFactory[P => T, T]) :factory.Res =
			components[P => T, T](pick)

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
		  */ //fixme: overloading causes type inferer to fail
//		def col[T :ColumnForm](pick :P => T) :Column[T] = new ParamColumn[T](Extractor.req(pick))

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
		class ParamComponent[T :SQLForm] private[UnboundParam](pick :P =?> T)
			extends ParamMapping[P, T, O]
		{
			override def root :UnboundParam[P, O] = This
			override def extract :ParamExtract[P, T, O] = GenericExtract(this)(pick)
			override def toString = s"$This[$form]"
		}


		/** A column value derived from the query parameter `P`, represented as a pseudo column of an artificial
		  * mapping in SQL expressions.
		  */
		class ParamColumn[T] private[UnboundParam](pick :P =?> T)(implicit override val form :ColumnForm[T])
			extends ParamComponent[T](pick) with ColumnMapping[T, O]
		{
			override def extract :ParamColumnExtract[P, T, O] = GenericExtract(this)(pick)
			override def name :String = This.name

			override def columnNamed(name :String) :Column[_] =
				if (name == this.name) this else super.columnNamed(name)
		}


		override def mappingName :String = name
		override def toString :String = name + ":" + form


		def unapply[X](expr :ColumnSQL[_, _, X]) :Opt[ParamColumn[X]] = expr match {
			case TypedComponentSQL(_, MappingExtract(_, _, col :UnboundParam[_, _]#ParamColumn[_])) if col.root == this =>
				Got(col.asInstanceOf[ParamColumn[X]])
			case _ => Lack
		}

		def unapply[X](expr :SQLExpression[_, _, X]) :Opt[ParamMapping[P, X, O]] = expr match {
			case TypedComponentSQL(_, MappingExtract(_, _, comp :ParamMapping[_, _, _])) if comp.root == this =>
				Got(comp.asInstanceOf[ParamMapping[P, X, O]])
			case _ => Lack
		}

		def unapply[X](column :Column[X]) :Opt[ParamColumn[X]] = column match {
			case param :UnboundParam[_, _]#ParamColumn[_] if param.root == this =>
				Got(param.asInstanceOf[ParamColumn[X]])
			case _ => Lack
		}

		def unapply[X](component :Component[X]) :Opt[ParamMapping[P, X, O]] = component match {
			case param :ParamMapping[_, _, _] if param.root == this =>
				Got(param.asInstanceOf[ParamComponent[X]])
			case _ => Lack
		}


		/** An extractor matching `ComponentSQL` expressions for components of this mapping,
		  * that is actual sql statement parameters.
		  */
		def formExtractor :Extractor[SQLExpression[_, _, _], SQLWriteForm[P]] = Extractor.Optional(
			(sql :SQLExpression[_, _, _]) => unapply(sql).map(_.derivedForm)
		)

	}




	object UnboundParam {
		def apply[P :SQLForm, O] :UnboundParam[P, O] = new UnboundParam

		def apply[P :SQLForm, O](name :String) :UnboundParam[P, O] =
			new UnboundParam(name)

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, O]`
		  * so it can be used as a type argument for [[net.noresttherein.oldsql.sql.ParamClause ParamClause]].
		  */
		type Of[X] = {
			type P[O] = UnboundParam[X, O]
			type Last = UnboundParam[X, RowProduct AndFrom ParamRelation[X]#Param]
		}

		/** A type alias for [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] synthetic mapping
		  * for parameter type `X` with its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type
		  * characteristic for the last mapping in a sequence of joins. Mappings of this type,
		  * as well as any their components, are implicitly converted
		  * to [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]s representing the parameter.
		  */
		type Last[X] = UnboundParam[X, RowProduct AndFrom ParamRelation[X]#Param]
	}


	class LabeledUnboundParam[N <: Label, X :SQLForm, O](override val name :N)
		extends UnboundParam[X, O](name) with LabeledMapping[N, X, O]

	object LabeledUnboundParam {
		def apply[P :SQLForm, N <: Label, O](name :N) :UnboundParam[P, O] = new LabeledUnboundParam(name)

		type Projection[N <: Label, S] = { type WithOrigin[O] = LabeledUnboundParam[N, S, O] }
	}



	/** Pattern matching SQL expressions representing unbound parameters and expressions directly derived from them
	  * by application of some function. It extracts both the root `UnboundParam` and the `MappingExtract` for its component.
	  */
	object UnboundParamSQL {
		//these do not use Opt only because of a scalac bug regarding existentials and value types.
		def unapply[F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		           (expr :TypedColumnComponentSQL[F, T, E, M, V, O])
				:Option[(UnboundParam[E, O], ParamColumnExtract[E, V, O], Int)] =
			expr.extract.export match {
				case param :UnboundParam[E @unchecked, O @unchecked]#ParamColumn[V @unchecked] =>
					Some((param.root, param.extract, expr.origin.offset))
				case _ => None
			}

		def unapply[F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		           (expr :TypedComponentSQL[_, T, E, M, V, O])
				:Option[(UnboundParam[E, O], ParamExtract[E, V, O], Int)] =
			expr.extract.export match {
				case param :ParamMapping[E @unchecked, V @unchecked, O @unchecked] =>
					Some((param.root, param.extract, expr.origin.offset))
				case _ => None
			}

		def unapply[X](expr :SQLExpression[_, _, X])
				:Option[(UnboundParam[P, O], ParamExtract[P, X, O], Int)] forSome { type P; type O } =
			expr match {
				case TypedComponentSQL(table, extractor) if extractor.export.isInstanceOf[ParamMapping[_, _, _]] =>
					val param = extractor.export.asInstanceOf[ParamMapping[Any, X, Any]]
					Some((param.root, param.extract, table.offset))
				case _ => None
			}

	}

}






/** A special, artificial 'join' type which joins the clause on its left side with a synthetic mapping
  * `P[O] <: UnboundParam[X, O]`, representing a query parameter `X`, unspecified at this point and will become a parameter
  * of any [[net.noresttherein.oldsql.sql.Incantation Incantation]] based on this join. To distinguish
  * it from the ''bound'' [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] SQL expression, which
  * translates to a statement parameter, but requires a value on creation, it is often referred to as an ''unbound''
  * parameter. It allows to filter a given ''from'' clause using values to be provided only at the execution time,
  * which can be obtained by applying an arbitrary scala function to `X`.
  *
  * The parameter value is available to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] based on
  * this or an extending clause just as the values of entities mapped to tables included in the ''from'' clause.
  * It can be used directly in comparisons with other expressions, but, similarly to how expressions for components
  * and columns of a table represent properties of mapped entities, properties of a parameter (in reality, any values
  * derivable from the value of the parameter) can be represented as pseudo 'components' of the synthetic mapping
  * for a parameter. Factory methods for expressions representing such parts of a parameter are available in
  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] mapping class. Any 'components' created this way
  * for use in [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions are virtual:
  * they do not show on any of the component or column lists declared by the mapping. This whole class performs
  * mainly a declarative role, translating to no changes in the generated SQL (beside enabling the use of the declared
  * parameter), unless a [[net.noresttherein.oldsql.sql.Adjoin.condition condition]] is provided with it, in which case
  * it will be included in the ''where'' clause of the created ''select''.
  *
  * This type represents an ungrouped ''from'' clause (i.e., without a ''group by'' clause), and hence it can only
  * be used following another [[net.noresttherein.oldsql.sql.FromSome ungrouped]] clause; a mirror type
  * exists in the form of [[net.noresttherein.oldsql.sql.GroupParam GroupParam]], which offers the same functionality,
  * but for parameters used in the ''group by''/''having'' clauses. The type of the left side must be
  * a [[net.noresttherein.oldsql.sql.FromSome.TopFromSome TopFromSome]], representing a valid ''from'' clause
  * of a top level SQL ''select'' (contain no [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo joins),
  * or this `RowProduct` will be impossible to use as a ''from'' clause of any ''select''. It is so because
  * otherwise such a subselect expression used under its outer select would hide the existence of an unbound parameter
  * from the outside, preventing providing a value for it and making the outer clause appear parameterless
  * (and hence a valid ''free'' select or subselect clause. This is enforced
  * by [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[F]` type, used to define valid
  * subselect clauses of a clause `F`. Before selecting any expressions, it will need to be replaced
  * with wither ''bound'' parameters, or a normal join with a [[net.noresttherein.oldsql.schema.Table table]]
  * using a mapping with the same [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
  * as this unbound parameter.
  *
  * The mapping of the synthetic relation in this clause, aside from representing the parameter value itself,
  * can also be used to create additional subcomponents with values derived from the parameter value, which can be used
  * in an `SQLExpression` as any other component. The mappings themselves are however only shills,
  * replaced when creating the ''select'' statement, and only their associated `SQLForm`s are being used
  * in the mapping process. The type constructor for a parameter mapping with type `X` is
  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.Of UnboundParam.Of[X]#P]].
  *
  * This join is typically written in an abbreviated form `FromSome WithParam X` (or `FromSome <=? X`)
  * and `FromSome JoinParam ("name" ?: X)#P` for a parameter of type `X` named with a string literal.
  *
  * @tparam F the actual ''from'' clause of the parameterized select statement, used as the left side of the 'join'.
  * @tparam P a synthetic `UnboundParam` mapping, the subject of which is the parameter type.
  *
  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
  * @see [[net.noresttherein.oldsql.sql.WithParam]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */ //lets try to widen the bound to `FromClause` - union types should do it, the problem is the upper bound on WithLeft and ilk.
sealed trait JoinParam[+F <: FromSome, P[O] <: ParamAt[O]]
	extends ParamClause[F, P] with AndFrom[F, P] with AndFromTemplate[F, P, F JoinParam P]
{ thisClause => //consider: renaming to ParamJoin
	//consider: it's tempting to have simply the parameter type as the second parameter, not the mapping,
	// but it would require changes to RowDecomposition, ExpandedBy et al, GetTable...
	override type Generalized = left.Generalized JoinParam P
	override type Dealiased   = left.Self JoinParam P
	override type Self       <: left.Self JoinParam P

	override def narrow :left.type JoinParam P

	override type GeneralizedLeft[+L <: FromSome] = L JoinParam P
	override type DealiasedLeft[+L <: FromSome]   = L JoinParam P
	override type WithLeft[+L <: FromSome]       <: L JoinParam P


	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :GlobalBoolean[E] =
		left.filter(target)(expansion.expandFront[left.Generalized, P]) && condition.basedOn(target)

	override def bind(param :LastParam) :AppliedParam = {
		val substitute = SQLScribe.applyParam(self, left.generalized, param)
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
	override type Inner    = left.Inner JoinParam P


	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Any] =
		//even if alias is not unique, it does not matter because no one will try to associate it with param expressions
		left.spellingContext.param(aliasOpt getOrElse paramCount.toString)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinParam.* @unchecked]

}






object JoinParam {

	/** A template `JoinParam` instance with a dummy mapping, for use as a polymorphic factory of `JoinParam` joins. */
	final val template :JoinParam.* = JoinParam(Relation.Dummy, ParamRelation[Unit]())

	/** Create an artificial join between the given relation on the left side, and the the `param` special relation
	  * representing a query parameter of type `X` as the right side. The ''where'' clause can be subsequently specified
	  * using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[X] ]] DSL instead.
	  * @param from  the first relation of the ''from'' clause, using the `FA[O] <: BaseMapping[A, O]` `Mapping` type.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
	  * @param castL implicit witness providing proper type inference for the subject of the left relation
	  *              and conversions of associated classes between instances parameterized with `L` and `LA`.
	  * @tparam F  the type constructor for the mapping of the first relation, accepting the `Origin` type.
	  * @tparam FA the same type as `L`, but with an upper bound of `BaseMapping`, separating the inference of types `L`
	  *            and its subject type `A`.
	  * @tparam X the parameter type - the subject of the synthetic `ParamRelation`.
	  * @return an unfiltered `F` [[net.noresttherein.oldsql.sql.WithParam WithParam]] `X`.
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
	  * @param from a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *             If it is not an [[net.noresttherein.oldsql.sql.FromSome.TopFromSome independent]], 'outer' clause,
	  *             that is it contains a `Subselect` join (or does not conform
	  *             to its [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form, created join
	  *             will be invalid - it will not be possible to create
	  *             a [[net.noresttherein.oldsql.sql.Select select]] with it as its ''from'' clause.
	  *             It may still however be useful for other purposes and the parameter can be ''bound'' at a later
	  *             time, removing the join an replacing its usages with
	  *             [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]], or substituting
	  *             it with a normal [[net.noresttherein.oldsql.schema.Table table]].
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
	  * @return `F` [[net.noresttherein.oldsql.sql.WithParam WithParam]] `X`.
	  */
	def apply[F <: FromSome, X](from :F, param :ParamRelation[X]) :F WithParam X =
		JoinParam[F, X](from, param, True)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[X] ]] DSL instead.
	  * @param from a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *             If it is not an [[net.noresttherein.oldsql.sql.FromSome.TopFromSome independent]], 'outer' clause,
	  *             that is it contains a `Subselect` join (or does not conform
	  *             to its [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form, created join
	  *             will be invalid - it will not be possible to create
	  *             a [[net.noresttherein.oldsql.sql.Select select]] with it as its ''from'' clause.
	  *             It may still however be useful for other purposes and the parameter can be ''bound'' at a later
	  *             time, removing the join an replacing its usages with
	  *             [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]], or substituting
	  *             it with a normal [[net.noresttherein.oldsql.schema.Table table]].
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F` [[net.noresttherein.oldsql.sql.WithParam WithParam]] `X`.
	  */
	def apply[F <: FromSome, X](from :F, param :ParamRelation[X],
	                            filter :GlobalBoolean[F#Generalized JoinParam ParamRelation[X]#Param]) :F WithParam X =
		JoinParam[from.type, ParamRelation[X]#Param, X, Nothing](from, LastParam(param), None)(filter)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[N, X] ]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              If it is not an [[net.noresttherein.oldsql.sql.FromSome.TopFromSome independent]], 'outer' clause,
	  *              that is it contains a `Subselect` join (or does not conform
	  *              to its [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form, created join
	  *              will be invalid - it will not be possible to create
	  *              a [[net.noresttherein.oldsql.sql.Select select]] with it as its ''from'' clause.
	  *              It may still however be useful for other purposes and the parameter can be ''bound'' at a later
	  *              time, removing the join an replacing its usages with
	  *              [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]], or substituting
	  *              it with a normal [[net.noresttherein.oldsql.schema.Table table]].
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
	  * @return `F JoinParam `[[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.Of UnboundParam.Of[X]#P]]` `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
	  */
	def apply[F <: FromSome, N <: Label, X](from :F, param :NamedParamRelation[N, X]) :F WithParam X As N =
		JoinParam[F, UnboundParam.Of[X]#P, X, N](from, LastParam(param), Some(param.name))(True)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[N, X] ]] DSL instead.
	  * @param from   a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *               If it is not an [[net.noresttherein.oldsql.sql.FromSome.TopFromSome independent]], 'outer' clause,
	  *               that is it contains a `Subselect` join (or does not conform
	  *               to its [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form, created join
	  *               will be invalid - it will not be possible to create
	  *               a [[net.noresttherein.oldsql.sql.Select select]] with it as its ''from'' clause.
	  *               It may still however be useful for other purposes and the parameter can be ''bound'' at a later
	  *               time, removing the join an replacing its usages with
	  *               [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]], or substituting
	  *               it with a normal [[net.noresttherein.oldsql.schema.Table table]].
	  * @param param  the last relation of the created ''from'' clause,
	  *               using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F JoinParam `[[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.Of UnboundParam.Of[X]#P]]` `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
	  */
	def apply[F <: FromSome, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :GlobalBoolean[F#Generalized JoinParam ParamRelation[X]#Param]) :F WithParam X As N =
		JoinParam[F, UnboundParam.Of[X]#P, X, N](from, LastParam(param), Some(param.name))(filter)


	/** Creates [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	/** Factory of [[net.noresttherein.oldsql.sql.JoinParam parameter pseudo joins]] for parameter type `X`.
	  * @tparam X the type of the unbound parameter introduced by this factory.
	  */
	trait ParamFactory[X] extends Any {
		/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
		  * and the the `param` special relation representing a query parameter of type `X` as the right side.
		  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
		  * it is generally recommended to use
		  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[X] ]] DSL instead.
		  * @param from a ''from'' clause containing the non-empty list of relations preceding `param`.
		  *             If it is not an [[net.noresttherein.oldsql.sql.FromSome.TopFromSome independent]], 'outer' clause,
		  *             that is it contains a `Subselect` join (or does not conform
		  *             to its [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form, created join
		  *             will be invalid - it will not be possible to create
		  *             a [[net.noresttherein.oldsql.sql.Select select]] with it as its ''from'' clause.
		  *             It may still however be useful for other purposes and the parameter can be ''bound'' at a later
		  *             time, removing the join an replacing its usages with
		  *             [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]], or substituting
		  *             it with a normal [[net.noresttherein.oldsql.schema.Table table]].
		  * @param form the form for the last [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation parameter]]
		  *             relation of the created ''from'' clause,
		  *             using [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @return `F` [[net.noresttherein.oldsql.sql.WithParam WithParam]] `X`.
		  */
		final def apply[F <: FromSome](from :F)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X, Nothing](from, LastParam(ParamRelation[X]()), None)(True)

		/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
		  * and the the `param` special relation representing a query parameter of type `X` as the right side.
		  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
		  * it is generally recommended to use
		  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[X] ]] DSL instead.
		  * @param from a ''from'' clause containing the non-empty list of relations preceding `param`.
		  *             If it is not an [[net.noresttherein.oldsql.sql.FromSome.TopFromSome independent]], 'outer' clause,
		  *             that is it contains a `Subselect` join (or does not conform
		  *             to its [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form, created join
		  *             will be invalid - it will not be possible to create
		  *             a [[net.noresttherein.oldsql.sql.Select select]] with it as its ''from'' clause.
		  *             It may still however be useful for other purposes and the parameter can be ''bound'' at a later
		  *             time, removing the join an replacing its usages with
		  *             [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]], or substituting
		  *             it with a normal [[net.noresttherein.oldsql.schema.Table table]].
		  * @param name
		  * @param form the form for the last [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation parameter]]
		  *             relation of the created ''from'' clause,
		  *             using [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @return `F` [[net.noresttherein.oldsql.sql.WithParam WithParam]] `X`.
		  */
		final def apply[F <: FromSome](from :F, name :String)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X, Nothing](from, LastParam(ParamRelation[X](name)), None)(True)
	}


	private[sql] def apply[L <: FromSome, M[O] <: UnboundParam[X, O], X, A <: Label]
	                      (clause :L, param :LastRelation[M, X], asOpt :Option[A])
	                      (cond :GlobalBoolean[clause.Generalized JoinParam M]) :L JoinParam M As A =
		new JoinParam[clause.type, M] with AbstractExpanded[clause.type, M, X] {
			override val left = clause
			override val last = param
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1
			override val parameterization = left.parameterization.param[Self, left.Self, M, left.Params, X]
			override def lastRelation = last
			override lazy val tableStack = super.tableStack
			override lazy val fullTableStack = super.fullTableStack

			override type Alias = A
			override type DealiasedCopy = left.type JoinParam M
			override type Copy = left.type JoinParam M As A
			override type Self = left.Self JoinParam M As A
			override type WithLeft[+F <: FromSome] = F JoinParam M As A

			override def narrow :left.type JoinParam M As A = this.asInstanceOf[left.type JoinParam M As A]

			override def withCondition(filter :GlobalBoolean[Generalized]) =
				JoinParam[left.type, M, X, A](left, last, aliasOpt)(filter)

			override def withLeft[F <: FromSome](left :F)(filter :GlobalBoolean[left.Generalized JoinParam M]) =
				JoinParam[F, M, X, A](left, last, aliasOpt)(filter)

			//consider: replacing the last relation with NamedParamRelation, but then JoinParam must always use UnboundParam
			override def aliased[N <: Label](alias :N) =
				JoinParam[left.type, M, X, N](left, last, Some(alias))(condition)

			override def expansion[C <: FromSome] :C PrefixOf (C JoinParam M As A) =
				PrefixOf.itself[C].expand[JoinParam, M].as[A]


			protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
			                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			{
				val leftSQL = spelling(left)(context, params.left)
				//the alias *should* never be used, it is here because we need a placeholder value to keep indexing consistent
				val expanded = leftSQL.context.param(aliasOpt getOrElse paramCount.toString)
//				val shiftParams = leftSQL.params.param[Generalized, left.Generalized, P, left.Params, X]
				val res = SpelledSQL(leftSQL.sql, expanded, leftSQL.setter)
				if (condition == True) res
				else res && (spelling.inWhere(condition)(self, _, params))
			}


			override def applyTo[Y](matcher :RowProductVisitor[Y]) = matcher.joinParam[L, M, X](this)

		}.asInstanceOf[L JoinParam M As A]




	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: TopFromSome, X](param :F WithParam X) :Opt[(F, Relation[ParamRelation[X]#Param])] =
		Got(param.left -> param.right)

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: RowProduct, X](from :F Adjoin M forSome { type M[O] <: RefinedMapping[X, O] })
			:Opt[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :JoinParam[_, ParamRelation[X]#Param @unchecked] => Got((from.left, param.right))
			case _ => Lack
		}

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :RowProduct) :Opt[(FromSome, Relation[ParamRelation[_]#Param])] =
		from match {
			case param :JoinParam.* @unchecked => Got(param.left -> param.right)
			case _ => Lack
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

	/** A type alias for [[net.noresttherein.oldsql.sql.JoinParam.FromLast FromLast]] member type of
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
	  * using mapping [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]`.
	  */
	type Last[X] = RowProduct AndFrom ParamRelation[X]#Param

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



/*
	trait FromParam[P[O] <: ParamAt[O]]
		extends JoinParam[EmptyClause, P] with AndFromTemplate[EmptyClause, P, FromParam[P]]
	{
		override type Self <: FromParam[P]
		override type Copy <: FromParam[P]
		override type DealiasedCopy = FromParam[P]
	}
*/
	type FromParam[P[O] <: ParamAt[O]] = EmptyClause JoinParam P

	object FromParam {
		type Of[X] = FromParam[ParamRelation[X]#Param]

		/** Create a [[net.noresttherein.oldsql.sql.RowProduct.PureParamFrom PureParamFrom]] declaring a single
		  * unbound parameter `X` represented by special pseudo relation `param`.
		  * @param param the last relation of the created ''from'' clause, using the
		  *              [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @return [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam]]`[`[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param]`.
		  */
		def apply[X](param :ParamRelation[X]) :Of[X] =
			FromParam[X](param, True)

		/** Create a [[net.noresttherein.oldsql.sql.RowProduct.PureParamFrom PureParamFrom]] declaring a single
		  * unbound parameter `X` represented by special pseudo relation `param`.
		  * @param param the last relation of the created ''from'' clause, using the
		  *              [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @param filter an optional join condition filtering the clause based on the value of `X`.
		  * @return [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam]]`[`[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param]`.
		  */
		def apply[X](param :ParamRelation[X], filter :GlobalBoolean[FromSome JoinParam ParamRelation[X]#Param]) :Of[X] =
			FromParam[ParamRelation[X]#Param, X, Nothing](LastParam(param), None)(filter)

		/** Create a [[net.noresttherein.oldsql.sql.RowProduct.PureParamFrom PureParamFrom]] declaring a single
		  * unbound parameter `X` represented by special pseudo relation `param`.
		  * @param param the last relation of the created ''from'' clause,
		  *              using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @return [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam]]`[`[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param] `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
		  */
		def apply[N <: Label, X](param :NamedParamRelation[N, X]) :Of[X] As N =
			FromParam[UnboundParam.Of[X]#P, X, N](LastParam(param), Some(param.name))(True)

		/** Create a [[net.noresttherein.oldsql.sql.RowProduct.PureParamFrom PureParamFrom]] declaring a single
		  * unbound parameter `X` represented by special pseudo relation `param`.
		  * @param param  the last relation of the created ''from'' clause,
		  *               using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @param filter an optional join condition filtering the clause based on the value of `X`.
		  * @return [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam]]`[`[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param] `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
		  */
		def apply[N <: Label, X]
		         (param :NamedParamRelation[N, X], filter :GlobalBoolean[FromSome JoinParam ParamRelation[X]#Param])
				:Of[X] As N =
			FromParam[UnboundParam.Of[X]#P, X, N](LastParam(param), Some(param.name))(filter)

		private[sql] def apply[M[O] <: UnboundParam[X, O], X, A <: Label]
		                      (param :LastRelation[M, X], asOpt :Option[A])
		                      (cond :GlobalBoolean[FromSome JoinParam M]) : FromParam[M] As A =
			JoinParam[EmptyClause, M, X, A](EmptyClause, param, asOpt)(cond)
/*
			new FromParam[M] with AbstractExpanded[EmptyClause, M, X] {
				override val left = EmptyClause
				override val last = param
				override val aliasOpt = asOpt
				override val condition = cond
				override val outer = Dual
				override val parameterization = left.parameterization.param[Self, left.Self, M, left.Params, X]
				override def lastRelation = last
				override lazy val tableStack = super.tableStack
				override lazy val fullTableStack = super.fullTableStack

				override type Alias = A
				override type Copy = FromParam[M] As A
				override type Self = EmptyClause JoinParam M As A
				override type WithLeft[+F <: FromSome] = F JoinParam M As A

				override def narrow :left.type JoinParam M As A = this.asInstanceOf[left.type JoinParam M As A]

				override def withCondition(filter :GlobalBoolean[Generalized]) =
					FromParam[M, X, A](last, aliasOpt)(filter)

				//consider: replacing the last relation with NamedParamRelation, but then JoinParam must always use UnboundParam
				override def aliased[N <: Label](alias :N) =
					FromParam[M, X, N](last, Some(alias))(condition)

				override def withLeft[F <: FromSome](left :F)(filter :GlobalBoolean[left.Generalized JoinParam M]) =
					JoinParam[F, M, X, A](left, last, aliasOpt)(filter)

				override def expansion[C <: FromSome] :C PrefixOf (C JoinParam M As A) =
					PrefixOf.itself[C].expand[JoinParam, M].as[A]


				protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Self])
				                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				{
					//the alias *should* never be used, it is here because we need a placeholder value to keep indexing consistent
					val expanded = context.param(aliasOpt getOrElse paramCount.toString)
					val res = SpelledSQL(expanded)
					if (condition == True) res
					else res && (spelling.inWhere(condition)(self, _, params))
				}


				override def applyTo[Y](matcher :RowProductVisitor[Y]) = matcher.joinParam[EmptyClause, M, X](this)

				override def chunkedString :ChunkedString = {
					var res = "fromParam " + right.toString
					if (aliasOpt.nonEmpty)
						res += " as " + alias
					if (condition != True)
						res = res + " on " + condition.toString
					res
				}
				override lazy val toString :String = super.toString

			}.asInstanceOf[FromParam[M] As A]
*/


		/** An empty `RowProduct` which extends `FromSome`. Needless to say, it breaks the contract in some places
		  * and expectations in many more. It is used as the start of
		  * [[net.noresttherein.oldsql.sql.RowProduct.PureParamFrom PureParamFrom]] types because
		  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] requires
		  * a [[net.noresttherein.oldsql.sql.FromSome FromSome]] subtype as its left side.
		  */
		class EmptyClause private[FromParam] (override val filter :SQLBoolean[FromSome, GlobalScope])
			extends FromSome with FromClauseTemplate[EmptyClause] with NonEmptyFromTemplate[EmptyClause, EmptyClause]
		{
			override type FromLast      = FromSome
			override type Generalized   = FromSome
			override type Self          = EmptyClause
			override type Dealiased     = EmptyClause
			override type DealiasedCopy = EmptyClause
			override type Copy          = EmptyClause
			override type Alias         = Nothing
			override type Last          = Nothing
			override type LastMapping   = Nothing
			override type JoinFilter    = Nothing

			override def aliasOpt :Option[Nothing] = None

			override def aliased[A <: Label](alias :A) :Nothing =
				throw new UnsupportedOperationException("EmptyClause.aliased(" + alias + ")")

			override def last :Nothing = throw new NoSuchElementException("EmptyClause.last")

			override def lastAsIn[E <: FromSome](implicit expansion :FromSome PrefixOf E) :Nothing = last

			override def filtered(condition :JoinFilter) :Nothing =
				throw new UnsupportedOperationException("EmptyClause.filtered")

			override def filtered[S >: GlobalScope <: GlobalScope](filter :SQLBoolean[FromSome, S]) :EmptyClause =
				new EmptyClause(this.filter && filter)

			override def filter[E <: RowProduct](target :E)(implicit expansion :FromSome PartOf E) :GlobalBoolean[E] =
				True

			override def paramCount = 0
			override def lastParamOffset :Nothing = throw new UnsupportedOperationException("EmptyClause.lastParamOffset")
			override def isSubselectParameterized :Boolean = false

			override type ParamsOnly           = true
			override type LastParam            = Nothing
			override type Params               = @~
			override type AppliedParam         = EmptyClause
			override type GeneralizedParamless = FromSome
			override type Paramless            = EmptyClause
			override type DecoratedParamless[D <: FromSome { type Params = @~ }] = D

			override def bind(param :Nothing) :EmptyClause = this
			override def bind(params: @~) :EmptyClause = this

			protected override def decoratedBind[D <: FromSome { type Params = @~ }]
			                                    (params: @~)(decorate :EmptyClause => D) :D = decorate(this)

			override type FullRow = @~
			override def fullSize :Int = 0

			override def fullRow[E <: RowProduct]
			                    (target :E)(implicit expansion :FromSome ExpandedBy E) :ChainTuple[E, GlobalScope, @~] =
				EmptyChain

			override def fullTableStack[E <: RowProduct]
			                           (target :E)(implicit expansion :FromSome ExpandedBy E) :LazyList[Nothing] =
				LazyList.empty

			override type JoinedWith = Nothing
			override type SelectedFrom[+P <: NonEmptyFrom] = Nothing

			override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) =
				throw new UnsupportedOperationException("EmptyClause.joinedWith(" + prefix + ", " + firstJoin + ")")

			override def selectedFrom[F <: NonEmptyFrom](prefix :F) :Nothing =
				throw new UnsupportedOperationException("EmptyClause.selectedFrom(" + prefix + ")")

			override def appendedTo[P <: FromClause](prefix :P) :Nothing = joinedWith(prefix, InnerJoin.template)


			override type Row      = @~
			override type OuterRow = @~
			override type Explicit = FromSome
			override type Inner    = FromSome
			override type Implicit = RowProduct
			override type Outer    = Dual
			override type Base     = RowProduct
			override type DefineBase[+I <: RowProduct] = I

			override def isValidSubselect :Boolean = false
			override def size  :Int  = 0
			override def base  :Dual = Dual
			override val outer :Dual = Dual

			override def row[E <: RowProduct]
			                (target :E)(implicit expansion :FromSome ExpandedBy E) :ChainTuple[E, GlobalScope, @~] =
				EmptyChain

			override def tableStack[E <: RowProduct]
			                       (target :E)(implicit expansion :FromSome ExpandedBy E) :LazyList[Nothing] =
				LazyList.empty

			override def outerRow[E <: RowProduct]
			                     (target :E)(implicit expansion :RowProduct ExpandedBy E) :ChainTuple[E, GlobalScope, @~] =
				EmptyChain

			override type AsSubselectOf[+F <: NonEmptyFrom] = Nothing

			override def asSubselectOf[F <: NonEmptyFrom]
			                          (newOuter :F)(implicit expansion :RowProduct ExpandedBy F) :Nothing =
				throw new UnsupportedOperationException("EmptyClause.asSubselectOf(" + newOuter + ")")

			override def withClause :WithClause = WithClause.empty


			protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, FromSome])
			                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				SpelledSQL(context)

			override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Any] = SQLContext()

			override def parameterization :Parameterization[@~, EmptyClause] = Parameterization.paramless

			protected override def groupingSpellingContext[P]
			                       (position :Int, context :SQLContext[P], params :Parameterization[P, FromSome])
					:GroupingSpellingContext[P] =
				throw new IndexOutOfBoundsException(
					"Cannot return GroupingSpellingContext for relation #" + position + " in " + this + "."
				)


			override def chunkedString :ChunkedString = "Params"
			override def toString :String = "Params"
		}


		object EmptyClause extends EmptyClause(True)
	}
}






/** A special, artificial 'join' type which adds a statement parameter of type `X` by expanding a ''group by''
  * clause on its left side with a special mapping `P[O] <: UnboundParam[X, O]`. The parameter value is unspecified
  * at this point and will need to be given to any `Incantation` produced from this clause. This type mirrors
  * the functionality of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]], which adds a parameter to the same effect
  * by expanding an [[net.noresttherein.oldsql.sql.FromSome ungrouped]] ''from'' clause. This duplication is required
  * for static type checking to be able to separate the artificial relations for the grouping expressions
  * of the group by clause from the real database relations: just as `JoinParam[_, _] <: FromSome`,
  * `GroupParam[_, _] <: GroupByClause`, both restricting their use and allowing them to be placed in between other
  * relations/grouping expressions. Similarly to `JoinParam`, this pseudo join does not result in adding any columns
  * to the `group by` clause in the generated statement and only its
  * [[net.noresttherein.oldsql.sql.Adjoin.condition condition]] (if non-empty) is added to the ''having'' clause
  * in the generated SQL.
  *
  * To distinguish it from ''bound''
  * [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] expressions,
  * which translates to statement parameters, but require a value on creation, it is often referred to as an ''unbound''
  * parameter (collectively with `JoinParam`). It allows to filter a given ''from'' clause using values to be provided
  * only at the execution time, which can be obtained by applying an arbitrary scala function to `X`. The mapping,
  * aside from representing the parameter value itself, can also be used to create additional subcomponents with values
  * derived from the parameter value, which can be used in an `SQLExpression` as any other component.
  * The mappings themselves are however only shills, replaced when creating the select statement, and only
  * their associated `SQLForm`s are being used in the mapping process. The type constructor for a parameter mapping
  * with type `X` is [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.Of UnboundParam.Of[X]#P]].
  *
  * This join is typically written in an abbreviated form `GroupByClause ByParam X`, following the naming scheme
  * of [[net.noresttherein.oldsql.sql.By By]]. A ''from'' clause can have unbound grouping parameters only
  * in its most outer section, before any [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins
  * (and, in case of this type, after the [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause) in order to be valid.
  * While it is possible to create instances which violate this property, both due to technical limitations and
  * in order to allow 'placeholder' parameters on temporary, component instances, they will be impossible to
  * use in a [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]] expression. It is so because
  * otherwise such a subselect expression used under its outer select would hide the existence of an unbound parameter
  * from the outside, preventing providing a value for it and making the outer clause appear parameterless
  * (and hence a valid ''ground'' select or subselect clause. This is enforced
  * by the [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[F]` type, used to define valid
  * subselect clauses of a clause `F`.
  *
  * @tparam F the actual ''group by'' clause of the parameterized select statement, used as the left side of the 'join'.
  * @tparam P a synthetic `UnboundParam` mapping, the subject of which is the parameter type.
  *
  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
  * @see [[net.noresttherein.oldsql.sql.ByParam]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
sealed trait GroupParam[+F <: GroupByClause, P[O] <: ParamAt[O]]
	extends ParamClause[F, P] with AndBy[F, P] with GroupByClauseTemplate[F GroupParam P, F GroupParam P]
{ thisClause =>
	override type Generalized = left.Generalized GroupParam P
	override type Dealiased   = left.Self GroupParam P
	override type Self       <: left.Self GroupParam P

	override type GeneralizedLeft[+L <: GroupByClause] = L GroupParam P
	override type DealiasedLeft[+L <: GroupByClause]   = L GroupParam P
	override type WithLeft[+L <: GroupByClause]       <: L GroupParam P

	override def withLeft[L <: GroupByClause] //overriden to make public
	                     (left :L)(filter :LocalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[L]

	override def columnCount :Int = left.columnCount

	override def bind(param :Param) :AppliedParam = {
		val substitute = SQLScribe.applyParam(self, left.generalized, param, 0)
		left.filtered(substitute(condition))
	}

	override def bind(params :Params) :Paramless = condition match {
		case True => left.bind(params.init)
		case _ =>
			val res = left.bind(params.init)
			val substitute = SQLScribe.applyParams(self, res.generalized)(params)
			res.filtered(substitute(condition)).asInstanceOf[Paramless]
	}


	override type JoinedWith[+S <: RowProduct, +J[+L <: S, R[O] <: MappingAt[O]] <: L NonParam R] =
		WithLeft[left.JoinedWith[S, J]]

	override def joinedWith[S <: FromSome](prefix :S, firstJoin :Join.*) :JoinedWith[S, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override def appendedTo[S <: FromClause](prefix :S) :JoinedWith[S, NonParam] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit GroupParam P
	override type Inner    = left.Inner GroupParam P


	protected override def defaultSpelling[X](context :SQLContext[X], params :Parameterization[X, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[X] =
	{
		val leftSQL = spelling(left)(context, params.left[left.Self, P])
		SpelledSQL(leftSQL.sql, leftSQL.context.grouped, leftSQL.setter) //condition separately in having
	}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupParam.* @unchecked]

}






object GroupParam {

	/** A template `GroupParam` instance with a dummy mapping, for use as a polymorphic factory of `GroupParam` pseudo joins. */
	final val template :GroupParam.* =
		GroupParam(From(Relation.Dummy).groupBy(Relation.Dummy[RowProduct]), ParamRelation[Unit]())

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
	  *              using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]` `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F` [[net.noresttherein.oldsql.sql.ByParam ByParam]] `X`.
	  */
	def apply[F <: TopGroupByClause, X]
	         (from :F, param :ParamRelation[X], filter :LocalBoolean[F#Generalized GroupParam ParamRelation[X]#Param] = True)
			:F ByParam X =
		GroupParam[from.type, ParamRelation[X]#Param, X, Nothing](from, ParamSQL(param, 0), None)(filter)

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
	  *              using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]` `Mapping` type.
	  * @return `F GroupParam (N `[[net.noresttherein.oldsql.sql.?: ?:]]` X)#P`.
	  */
	def apply[F <: TopGroupByClause, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X]) :F ByParam X As N =
		GroupParam[F, UnboundParam.Of[X]#P, X, N](from, ParamSQL(param, 0), Some(param.name))(True)

	/** Add a statement parameter to the ''group by'' clause given as the left side, in the form of
	  * the `param` special relation representing a query parameter of type `X` as the right side.
	  * An additional condition for the ''having'' clause can be subsequently specified using the
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] or
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] method.
	  * It is a lower level method; it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.GroupByClause.TopGroupByClauseExtension.param param]]`[N, X]` DSL instead.
	  * @param from   a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *               It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param  the last relation of the created ''from'' clause,
	  *               using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]` `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F GroupParam (N `[[net.noresttherein.oldsql.sql.?: ?:]]` X)#P`.
	  */
	def apply[F <: TopGroupByClause, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :LocalBoolean[F#Generalized GroupParam ParamRelation[X]#Param]) :F ByParam X As N =
		GroupParam[F, UnboundParam.Of[X]#P, X, N](from, ParamSQL(param, 0), Some(param.name))(filter)


	/** Creates [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		final def apply[F <: TopGroupByClause](from :F)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X, Nothing](from, ParamSQL(ParamRelation[X](), 0), None)(True)

		final def apply[F <: TopGroupByClause](from :F, name :String)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X, Nothing](from, ParamSQL(ParamRelation[X](name), 0), None)(True)
	}


	private[sql] def apply[L <: GroupByClause, M[O] <: UnboundParam[X, O], X, A <: Label]
	                      (clause :L, param :RelationSQL[GroupByClause AndBy M, M, X, GroupByClause AndBy M],
	                       asOpt :Option[A])
	                      (cond :LocalBoolean[clause.Generalized GroupParam M]) :L GroupParam M As A =
		new GroupParam[clause.type, M] with AbstractExpanded[clause.type, M, X] {
			override val left = clause
			override val last = param
			override val aliasOpt = asOpt
			override val condition = cond
			override val fromClause :Discrete = left.fromClause
			override val outer = left.outer
			override val fullSize = left.fullSize + 1
			override val parameterization = left.parameterization.param[Self, left.Self, M, left.Params, X]
			override def lastRelation = last
			override lazy val tableStack = super.tableStack
			override lazy val fullTableStack = super.fullTableStack

			override type Alias = A
			override type Self = left.Self GroupParam M As A
			override type WithLeft[+F <: GroupByClause] = F GroupParam M As A
			override type DealiasedCopy = left.type GroupParam M
			override type Copy = left.type GroupParam M As A
			override def narrow :left.type GroupParam M As A = this.asInstanceOf[left.type GroupParam M As A]

			override def withCondition(filter :LocalBoolean[Generalized]) =
				GroupParam[left.type, M, X, A](left, last, aliasOpt)(filter)

			override def withLeft[F <: GroupByClause](left :F)(filter :LocalBoolean[left.Generalized GroupParam M]) =
				GroupParam[F, M, X, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				GroupParam[left.type, M, X, N](left, last, Some(alias))(condition)


			override def applyTo[Y](matcher :RowProductVisitor[Y]) = matcher.groupParam[L, M, X](this)

		}.asInstanceOf[L GroupParam M As A]




	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: GroupByClause, X](param :F ByParam X) :Opt[(F, Relation[ParamRelation[X]#Param])] =
		Got(param.left -> param.right)

	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: RowProduct, X](from :F Adjoin M forSome { type M[O] <: RefinedMapping[X, O] })
			:Opt[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :GroupParam[_, ParamRelation[X]#Param @unchecked] => Got((from.left, param.right))
			case _ => Lack
		}

	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :RowProduct) :Opt[(GroupByClause, Relation[ParamRelation[_]#Param])] =
		from match {
			case param :GroupParam.* @unchecked => Got(param.left -> param.right)
			case _ => Lack
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

	/** A type alias for [[net.noresttherein.oldsql.sql.JoinParam.FromLast FromLast]] member type of
	  * [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
	  * using mapping [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]`.
	  */
	type Last[X] = GroupByClause GroupParam ParamRelation[X]#Param

	/** A curried type constructor for `GroupParam` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: TopGroupByClause] = {
		type F[R[O] <: ParamAt[O]] = L GroupParam R
		type P[X] = L ByParam X
	}

	/** A curried type constructor for `GroupParam` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: TopGroupByClause] = L GroupParam R }
	
}


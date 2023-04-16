package net.noresttherein.oldsql.sql

import scala.annotation.showAsInfix

import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.{Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.collection.Unique.EmptyUnique
import net.noresttherein.oldsql.morsels.{ChunkedString, Extractor}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, Requisite}
import net.noresttherein.oldsql.morsels.generic.FunctionOf
import net.noresttherein.oldsql.schema.{ColumnExtract, ColumnForm, Mapping, MappingExtract, Relation, SpecificExtract, SQLForm, SQLWriteForm, Table}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.Relation.{NamedRelation, PseudoRelation, StaticRelation}
import net.noresttherein.oldsql.schema.SQLForm.FormBasedFactory
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate
import net.noresttherein.oldsql.sql.Expanded.{AbstractExpanded, ExpandedComposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromClause.EmptyFromSome
import net.noresttherein.oldsql.sql.FromSome.TopFromSome
import net.noresttherein.oldsql.sql.GroupByClause.{GroupByClauseTemplate, TopGroupByClause}
import net.noresttherein.oldsql.sql.ParamClause.{NamedParamRelation, ParamAt, ParamRelation, UnboundParam}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, NonEmptyRow, ParamsRow, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.GroupingSpellingContext
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.{ChainTuple, JoinedParam, JoinedRelation, ParamSQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.ParamSQL.{LastGroupParam, LastParam}
import net.noresttherein.oldsql.sql.ast.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.BaseMappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






//todo: a type alias being a bound of all RowProduct subtypes consisting solely of JoinParams
/** Base trait for ''unbound'' query parameters, that is parameters without a known value, in contrast
  * to ''bound'' [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] expressions.
  * It is represented as a special kind of join between an existing `RowProduct` on the left, and a synthetic
  * `Mapping` subtype [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]].
  * Two concrete subclasses exist: [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] for ''discrete'' clauses
  * (''from'' clauses without a ''group by'' clause) and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
  * for clauses with a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] join to the left.
  */ //consider: renaming to ClauseParam or AndParam; ParamsRow could then safely become ParamsClause
@showAsInfix //renaming scheme: ParamClause=>ClauseParam,
sealed trait ParamClause[+L <: NonEmptyRow, R[O] <: ParamAt[O]] extends NonSubselect[L, R] { thisClause =>
	/* Todo: we need:
	 *  1. A base ClauseParam[L <: RowProduct, R[O] <: MappingAt[O]] (or, if we can make it work for a WithClause, WithParam
	 *  2. Rename of RowProduct.ParamsRow to ParamsClause.
	 *  3. Possibly a (public) Param[L <: EmptyClause, R[O] <: MappingAt[O]] extends EmptyClause, for Call and Insert
	 *     Ideally, we could just use the above ClauseParam for this purpose.
	 *  4. FromParam[M[O] <: MappingAt[O]] as an alias, hopefully.
	 *  5. Rename of ParamClause to AndParam (?)
	 *  5. Potentially rename GroupParam to AndByParam (not really consistent with other aliases, though)
	 *
	 */
	//todo: make left bound only by RowProduct; the challenge is that Dual ParamClause R would also be an empty clause
	//todo: make a special subclass FromParam[L <: EmptyClause, R[O] <: MappingAt[O]] extends EmptyClause
	//todo: try to make it accept any table mapping, if the parameter is a mapped entity.
	/** A synthetic relation for this parameter. It is never shared with other instances,
	  * with every unbound parameter having its own copy. */
	override def right :Relation[R] = last.relation //overridden for docs

//	override type Last[O <: RowProduct] = JoinedRelation[O, R] //could we narrow it down in subclasses instead?

	override type Generalized >: Complete <: (left.Generalized ParamClause R) {
		type FromLast     = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit    <: thisClause.Explicit
		type Implicit    <: thisClause.Implicit
	}

	override type Complete >: NoAlias <: (left.Complete ParamClause R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type Base        = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type NoAlias >: Self <: (left.Self ParamClause R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type Base        = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type Self <: (left.Self ParamClause R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
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
	override def isExplicitParameterized :Boolean = true

	/** The type of this parameter, that is the subject type of the joined mapping. */
	type Param = last.Subject

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
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, FullRow] =
		left.fullRow(target)(expansion.expandFront[left.Generalized, R])



	override type SelectedFrom[+S <: NonEmptyRow] = Nothing

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
	             (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, Row] =
		left.row(target)(expansion.expandFront[left.Generalized, R])


	override type AsSubselectOf[+O <: NonEmptyRow] = Nothing

	override def asSubselectOf[O <: RowProduct](outer :O)(implicit expansion :Implicit ExpandedBy O) :Nothing =
		throw new UnsupportedOperationException(
			"ParamClause.asSubselectOf: join parameters can't appear as a part of a subselect from clause. " +
				s"$this asSubselectOf $outer"
		)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamClause.__ @unchecked]

	override def name = "param"

}






/** Types used by the [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] 'join' and its subtypes. */
object ParamClause {

	/** Matches all `ParamClause` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: NonEmptyRow, P[O] <: ParamAt[O]](param :F ParamClause P) :Opt[(F, Relation[P])] =
		Got(param.left -> param.right)

	/** Matches all `ParamClause` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: RowProduct, X](from :F Adjoin M forSome { type M[O] <: TypedMapping[X, O] })
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
			case param :ParamClause.__ @unchecked => Got(param.left -> param.right)
			case _ => Lack
		}




	/** Type alias for `ParamClause` with erased type parameters, covering all instances of `ParamClause`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = ParamClause[_ <: NonEmptyRow, ParamRelation[_]#Param]

	/** A curried type constructor for `ParamClause` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: NonEmptyRow] = { type F[R[O] <: ParamAt[O]] = L ParamClause R }

	/** A curried type constructor for `ParamClause` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: NonEmptyRow] = L ParamClause R }

	/** A curried type constructor for [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] mappings,
	  * accepting a parameter type to use as the `Subject` type of the mapping, and exposing a type constructor `M`
	  * accepting its `Origin` type.
	  */
	type Param[X] = { type M[O] = UnboundParam[X, O] }






	/** A special, artificial relation implementation dedicated to
	  * the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] mapping class,
	  * representing am unbound query parameter. Can be joined with any
	  * [[net.noresttherein.oldsql.sql.FromClause FromClause]] with a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]],
	  * and with a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]]
	  * with a [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]. The simplest way of creating a new relation
	  * for a query is with [[net.noresttherein.oldsql.sql.?: ?:]] function from the `sql` package and an extension
	  * method of the same name for `String` literals.
	  * @see [[net.noresttherein.oldsql.sql.ParamClause.NamedParamRelation]]
	  */ //todo: the whole param mapping equality debacle could be solved by moving pattern matching to JoinedParam
	sealed class ParamRelation[X](override val name :String)(implicit val form :SQLForm[X])
		extends PseudoRelation[({ type P[O] = UnboundParam[X, O] })#P] with NamedRelation[({ type P[O] = UnboundParam[X, O] })#P]
	{
		type Param[O] = UnboundParam[X, O]
//		type Last = RowProduct AndFrom ParamRelation[X]#Param

		private[this] val param = new UnboundParam[X, Any](name)
		private[this] val Form = param.formExtractor

		override def apply[O]  :UnboundParam[X, O] = param.withOrigin[O]
		override def export[O] :UnboundParam[X, O] = apply[O]

		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:ParamRelation[X] =
			this


		/** Creates an SQL fragment for the given parameter component, inserting '?' JDBC parameter placeholders
		  * parameters insert the JDBC statement
		  *      parameter placeholder '?' (or multiple, as with regular components) and append a setter form for
		  *      the parameter to the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]]
		  *      returned with the SQL.
		  *   1. [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation Grouping]] relations always copy the whole
		  *      expression as appearing in the ''group by'' clause, together with repetitions of any bound or unbound
		  *      parameters included in the expression.
		  * Default implementation delegates to the overloaded variant for a single column and simply collects the results.
		  * Non standard implementations may resort to other schemes.
		  * @param origin a relation expression which `relation` property must equal this instance.
		  * @param component a component of the [[net.noresttherein.oldsql.schema.Relation.export export]] mapping
		  *                  of this relation.
		  */
		protected def spell[P, F <: O, O <: RowProduct]
		                   (origin :JoinedParam[O, Param], component :MappingAt[O], inline :Boolean = false)
		                   (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                   (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			component match { //the usual problem of equality between ParamRelations and their mappings...
//				case _ if origin.relation != this =>
//					throw new IllegalArgumentException(
//						s"Cannot spell the expression for component $component of $origin as it is for a different relation than $this."
//					)
				case Form(paramForm) => //this currently won't work unless origin.relation == this
					val getter = params[ParamRelation[X]#Param, X, O](origin)
					val writeForm = paramForm.from(getter)
					SpelledSQL(writeForm.param(inline), writeForm, context)
				case _ if origin.relation != this =>
					//the spelling depends only on the form in the component, so we don't mind using a different instance
					origin.toParamSQL.relation.spell(spelling)(origin, component, inline)(from, context, params)
				case _ =>
					throw new IllegalArgumentException(
						s"Mapping $component passed as a component of unbound param $origin is not a ParamMapping derived from $this"
					)
			}

		/** Spells the given parameter `component` as separate columns, returning them as a sequence.
		  * If `component` is a column (or a single column mapping), the list contains a single SQL "?" with the
		  * component's form; otherwise its form is [[net.noresttherein.oldsql.schema.SQLWriteForm.split split]]
		  * into column forms.
		  *
		  * This method is used in circumstances where a custom concatenation strategy is required (for example,
		  * as part of an SQL ''update's'' ''set'' clause).
		  *
		  * There is a `protected` forwarder to this method in `SQLSpelling`.
		  */
		@throws[UnsupportedOperationException]("if the parameter component cannot be split into individual columns " +
		                                       "(for example, because its form adapts a custom function)")
		protected def spellExploded[P, F <: O, O <: RowProduct]
		                           (origin :JoinedParam[O, Param], component :MappingAt[O], independent :Boolean)
		                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                           (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
			component match {
//				case _ if origin.relation != this =>
//					throw new IllegalArgumentException(
//						s"Cannot spell the expression for component $component of $origin as it is for a different relation than $this."
//					)
				case Form(paramForm) => //this currently won't work unless origin.relation == this...
					val getter = params[ParamRelation[X]#Param, X, O](origin)
					val writeForm = paramForm.from(getter)
					form.columnCount match {
						case 0 => Seq.empty
						case 1 => SpelledSQL("?", writeForm, context)::Nil
						case n if independent =>
							val paramSQL = SpelledSQL("?", SQLWriteForm.empty, context)
							PassedArray.fill(n - 1)(paramSQL) :+ SpelledSQL("?", writeForm, context)
						case _ =>
							writeForm.split.map { SpelledSQL(_) }
					}
				case _ if origin.relation != this =>
					//the spelling depends only on the form in the component, so we don't mind using a different instance
					val actual = origin.toParamSQL.relation
					actual.spellExploded[P, F, O](origin, component, independent)(from, context, params)
				case _ =>
					throw new IllegalArgumentException(
						s"Mapping $component passed as a component of unbound param $origin is not a ParamMapping derived from $this."
					)

			}

/*
		protected override def spell[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A], V]
		                            (origin :JoinedRelation[O, T], column :TypedColumn[V, O])
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
					writeForm.columnCount match {
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
		protected[sql] final def spell[P, F <: O, O <: RowProduct]
		                              (spelling :SQLSpelling)
		                              (origin :JoinedParam[O, Param], component :MappingAt[O], inline :Boolean)
		                              (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
			spell(origin, component, inline)(from, context, params)(spelling)

		protected[sql] final def spellExploded[P, F <: O, O <: RowProduct]
		                                      (spelling :SQLSpelling)
		                                      (origin :JoinedParam[O, Param], component :MappingAt[O], independent :Boolean)
		                                      (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:Seq[SpelledSQL[P]] =
			spellExploded(origin, component, independent)(from, context, params)(spelling)


		override def identical(that :Relation[MappingAt]) :Boolean = that match {
			case other :ParamRelation.__ => //should this compare the param name?
				(this eq other) || other.canEqual(this) && name == other.name && (export identical other.export)
			case _ => false
		}
		override def equals(that :Any) :Boolean = that match {
			case other :ParamRelation.__ =>
				(other eq this) || other.canEqual(this) && name == other.name && export == other.export
			case _ => false
		}
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamRelation.__]
		override def hashCode :Int = name.hashCode * 31 + row.hashCode

		override def refString :String = name
		override def toString :String = name + "?:" + form
	}


	/** A factory and match pattern for unbound parameter relations.
	  * Through a pseudo join [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
	  * (or [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] and synthetic mapping
	  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] they introduce a query/statement
	  * parameter whose value is unknown when the query is created.
	  *
	  * An alternative factory method is available in package `sql`: [[net.noresttherein.oldsql.sql.?: ?:]]`[Familiar]`.
	  */
	object ParamRelation {
		def apply[X :SQLForm](name :String) :ParamRelation[X] = new ParamRelation[X](name)

		def apply[X :SQLForm]() :ParamRelation[X] = ParamRelation("_")

		def unapply(source :Relation.__) :Opt[(SQLForm[_], String)] = source match {
			case param :ParamRelation.__ => Got(param.form -> param.name)
			case _ => Lack
		}

		type __ = ParamRelation[_]
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
		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:NamedParamRelation[N, X] =
			this

		override def equals(that :Any) :Boolean = super[ParamRelation].equals(that)
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[NamedParamRelation[_, _]]
	}


	object NamedParamRelation {
		def apply[N <: Label, X :SQLForm](name :N) :NamedParamRelation[N, X] =
			NamedParamRelation[N, X]()(SQLForm[X], new ValueOf(name))

		//the order of implicits reversed to avoid double definition with the one above.
		def apply[N <: Label, X]()(implicit form :SQLForm[X], name :ValueOf[N]) :NamedParamRelation[N, X] =
			new NamedParamRelation[N, X](valueOf[N])

		def unapply(source :Relation.__) :Opt[(SQLForm[_], String)] = source match {
			case param :NamedParamRelation[_, _] => Got(param.form -> param.name)
			case _ => Lack
		}

		type __ = NamedParamRelation[_, _]
	}






	type ParamExtract[P, S, O] = SpecificExtract[ParamMapping[P, S, O], P, S, O]
	type ParamColumnExtract[P, S, O] = SpecificExtract[UnboundParam[P, O]#ParamColumn[S], P, S, O]

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
	  */ //todo: rename to ParamFrom; in Scala 3, we don't need both this and UnboundParam; rename UnboundParam=>ParamFrom, ParamClause=>UnboundParam
	sealed trait ParamAt[O] extends Mapping {
		override type Origin = O
		val form :SQLForm[Subject]

		def root :UnboundParam[Subject, O]
	}



	/** A `Mapping` type representing a query parameter, the value of which is not known at the point of its creation.
	  * While the [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] expression can be used
	  * to represent a statement parameter, its value must be known when the expression is created.
	  * By representing it instead as a mapping, its value becomes available to SQL
	  * [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
	  * through a [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]`[F, ParamRelation[P]#Param]` expression,
	  * provided by a parameter pseudo joins [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
	  * or [[net.noresttherein.oldsql.sql.GroupParam GroupParam]].
	  *
	  * Furthermore, the standard mechanism of [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions
	  * for [[net.noresttherein.oldsql.sql.ast.JoinedTable tables]] listed by a ''from'' clause, can be used
	  * to represent any value obtainable from `P` by a function `P => T` as a component
	  * `UnboundParam[P, _]#Component[T]` wrapping that function, which can be used to create component expressions
	  * for that function. These can be created using factory methods of this class.
	  *
	  * While adding complexity when compared to bound parameters, SQL expressions using unbound expressions
	  * can be reused for different parameter values and do not need to be rewritten each time.
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
	  */ //consider: UnboundParamColumn
	class UnboundParam[P, O](val name :String)(implicit sqlForm :SQLForm[P])
		extends ParamMapping[P, P, O] with ParamAt[O] //todo: make equality form based; decouple ParamComponents
//		   with FormBasedFactory[FunctionOf[P]#F, MappingAt[O]#TypedComponent, MappingAt[O]#Column]
	{
		def this()(implicit form :SQLForm[P]) = this("?")

		override def root :UnboundParam[P, O] = this
		override def extract :ParamExtract[P, P, O] = SpecificExtract.ident(this)
		override def derivedForm :SQLWriteForm[P] = form

		protected override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[P] =
			SQLWriteForm.join(
				components.toSeq.map {
					case this.components(param) => param.derivedForm
					case comp =>
						throw new IllegalArgumentException(s"Mapping $comp is not a component of parameter mapping $this.")
				} :_*
			)

		/** An empty collection of components and a factory of new subcomponents of this parameter mapping.
		  * Defines an `apply` method accepting a getter `P => T` of component value from this parameter's value
		  * and creating either a [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.ParamComponent ParamComponent]]
		  * or a [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.ParamColumn ParamColumn]], depending
		  * on whether an implicit [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]`[T]` exists
		  * (or only [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[T]`).
		  */
		override object components
			extends EmptyUnique with Unique[Nothing] //EmptyUnique is protected
			   with FormBasedFactory[FunctionOf[P]#To, MappingAt[O]#TypedComponent, MappingAt[O]#TypedColumnComponent]
		{
			override def generalResult[T :SQLForm](arg :P => T) = new ParamComponent(arg)
			override def specificResult[T :ColumnForm](arg :P => T) = new ParamColumn(arg)


			def unapply[X](expr :ColumnSQL[_, _, X]) :Opt[ParamColumn[X]] =  expr match {
				case TypedComponentSQL(_, MappingExtract(_, _, col :UnboundParam[_, _]#ParamColumn[_]))
						if col.root == this =>
					Got(col.asInstanceOf[ParamColumn[X]])
				case _ => Lack
			}
			def unapply[X](expr :SQLExpression[_, _, X]) :Opt[ParamComponent[X]] = expr match {
				case TypedComponentSQL(_, MappingExtract(_, _, comp :UnboundParam[_, _]#ParamComponent[_]))
					if comp.root == this
				=>
					Got(comp.asInstanceOf[ParamComponent[X]])
				case _ => Lack
			}
			def unapply[X](column :Column[X]) :Opt[ParamColumn[X]] = column match {
				case param :UnboundParam[_, _]#ParamColumn[_] if param.root == this =>
					Got(param.asInstanceOf[ParamColumn[X]])
				case _ => Lack
			}
			def unapply[X](component :Component[X]) :Opt[ParamComponent[X]] = component match {
				case param :UnboundParam[_, _]#ParamComponent[_] if param.root == this =>
					Got(param.asInstanceOf[ParamComponent[X]])
				case _ => Lack
			}
		}

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


		//todo: a parameter component compared to or assigned to a mapping component should not require a form
		/** Create an artificial component with subject type `T`, extractable from the parameter type.
		  * The component is ''not'' listed on any of the component lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */ //todo: rename/ in Scala 3 move to macros anc combine with the following
		def comp[T :SQLForm](pick :P =?> T) :ParamComponent[T] = new ParamComponent[T](pick)

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
		  *                if the return type will be a `TypedColumn[O, T]`, or simply `BaseMapping[O, T]`.
		  */
		def apply[T](pick :P => T)(implicit factory :components.DedicatedFactory[P => T, T]) :factory.Res =
			components[P => T, T](pick)

		/** Create an artificial component with subject type `T`, extractable from the parameter type.
		  * The component is ''not'' listed on any of the component lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
		def opt[T :SQLForm](pick :P => Option[T]) :ParamComponent[T] = new ParamComponent[T](Extractor(pick))


		/** Create an artificial column with subject type `T`, extractable from the parameter type.
		  * The column is ''not'' listed on any of the column lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
		def col[T :ColumnForm](pick :P =?> T) :ParamColumn[T] = new ParamColumn[T](pick)

		/** Create an artificial column with subject type `T`, extractable from the parameter type.
		  * The column is ''not'' listed on any of the column lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */ //fixme: overloading causes type inferer to fail
//		def col[T :ColumnForm](pick :P => T) :Column[T] = new ParamColumn[T](Extractor.req(pick))

		/** Create an artificial column with subject type `T`, extractable from the parameter type.
		  * The column is ''not'' listed on any of the column lists, but can be used in SQL expressions
		  * in the same way as components of mappings for real relations.
		  */
		def optcol[T :ColumnForm](pick :P => Option[T]) :ParamColumn[T] = new ParamColumn[T](Extractor(pick))



		/** Represents this parameter mapping as its own column, providing an implicit `ColumnForm[P]` is present.
		  * This method exists because all parameters are created as general mappings, allowing multiple column
		  * forms. It can be used if the parameter is in reality a simple type and there is a need to use it
		  * directly in SQL as an atomic value.
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.ifColumn]]
		  */
		def toColumn(implicit form :ColumnForm[P]) :Column[P] = new ParamColumn[P](Extractor.ident[P])

		/** Represents this parameter mapping as its own column,
		  * assuming its [[net.noresttherein.oldsql.schema.bits.FormMapping.form form]]
		  * is a [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]].
		  * This method exists because all parameters are created as general mappings, allowing multiple column
		  * forms. It can be used if the parameter is in reality a simple type and there is a need to use it
		  * directly in SQL as an atomic value.
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.toColumn]]
		  */
		def ifColumn :Option[Column[P]] = form match {
			case column :ColumnForm[P] => Some(new ParamColumn[P](Extractor.ident[P])(column))
			case _ => None
		}

		/** A value derived from the query parameter `P`, represented as a pseudo component of an artificial
		  * mapping in SQL expressions.
		  */
		class ParamComponent[T :SQLForm] private[UnboundParam] (pick :P =?> T)
			extends ParamMapping[P, T, O]
		{
			override def root :UnboundParam[P, O] = UnboundParam.this
			override def extract :ParamExtract[P, T, O] = SpecificExtract(this)(pick)
			override def toString = s"$root.[$form]"
		}


		/** A column value derived from the query parameter `P`, represented as a pseudo column of an artificial
		  * mapping in SQL expressions.
		  */
		class ParamColumn[T] private[UnboundParam] (pick :P =?> T)(implicit override val form :ColumnForm[T])
			extends ParamComponent[T](pick) with BaseColumn[T, O]
		{
			override def extract :ParamColumnExtract[P, T, O] = SpecificExtract(this)(pick)
			override def name :String = UnboundParam.this.name

			override def columnNamed(name :String) :Column[_] = super[BaseColumn].columnNamed(name)

			override def apply[S](component :Component[S]) :ColumnExtract[S] =
				super[BaseColumn].apply(component)

			override def apply[S](column :Column[S]) :ColumnExtract[S] = apply(column :Component[S])

			override def export[S](component :Component[S]) :TypedColumn[S, O] =
				super[BaseColumn].export(component)

			override def export[S](column :Column[S]) :TypedColumn[S, O] = export(column :Component[S])

			override def toString :String = super[ParamComponent].toString
		}

//		override def equivalent(that :Mapping) :Boolean =



		/** An extractor matching `ComponentSQL` expressions for components of this mapping,
		  * that is actual sql statement parameters.
		  */
		def formExtractor :Extractor[SQLExpression[_, _, _], SQLWriteForm[P]] = Extractor.Optional(
			(sql :SQLExpression[_, _, _]) => components.unapply(sql).map(_.derivedForm)
		)


		override def uniHomomorphic(that :Mapping) :Boolean = that match {
			case other :UnboundParam[_, _] => (this eq other) || form == other.form
			case other if other.original != other => homomorphic(other.original)
			case _ => false
		}
		override def identical(that :Mapping) :Boolean = that match {
			case other :UnboundParam[_, _] => (this eq other) || form == other.form
			case _ => false
		}
		override def equivalent(that :Mapping) :Boolean = isomorphic(that)
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[UnboundParam[_, _]]

		override def mappingName :String = name
		override def toString :String = name + ":" + form

	}




	object UnboundParam {
		def apply[P :SQLForm, O] :UnboundParam[P, O] = new UnboundParam

		def apply[P :SQLForm, O](name :String) :UnboundParam[P, O] =
			new UnboundParam(name)

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, O]`
		  * so it can be used as a type argument for [[net.noresttherein.oldsql.sql.ParamClause ParamClause]].
		  */
		type Of[X] = {
			type M[O] = UnboundParam[X, O]
			type Last = UnboundParam[X, RowProduct AndFrom ParamRelation[X]#Param]
		}
		type of[X] = {
			type M[O] = UnboundParam[X, O]
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


	//todo: get rid of the whole LabeledUnboundParam and use the label for an alias
	class LabeledUnboundParam[N <: Label, X :SQLForm, O](override val name :N)
		extends UnboundParam[X, O](name) with LabeledMapping[N, X, O]

	object LabeledUnboundParam {
		def apply[P :SQLForm, N <: Label, O](name :N) :UnboundParam[P, O] = new LabeledUnboundParam(name)

		type Projection[N <: Label, S] = { type WithOrigin[O] = LabeledUnboundParam[N, S, O] }
	}

}






/** A special, artificial 'join' type which joins the clause on its left side with a synthetic mapping
  * `R[O] <: UnboundParam[X, O]`, representing a query parameter `X`, unspecified at this point and will become a parameter
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
  * but for parameters used in the ''group by''/''having'' clauses. Note that while a parameter introduced
  * by `JoinParam`, just as all other tables in the same [[net.noresttherein.oldsql.sql.FromClause FromClause]]
  * is inaccessible to SQL expressions based on a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]]
  * containing it in its [[net.noresttherein.oldsql.sql.GroupByClause.Discrete Discrete]] clause,
  * it can be also used as a ''group by'' expression, just as any component of a grouped table.
  * This would result in an additional [[net.noresttherein.oldsql.sql.ByParam ByParam]]`[R[_]#Subject]` clause element.
  * It has no other effect: as `ParamClause`, it doesn't translate to any fragment of the final SQL and
  * the parameter types (in particular, [[net.noresttherein.oldsql.sql.RowProduct.Params Params]]) remain unchanged.
  *
  * The type of the left side must be a [[net.noresttherein.oldsql.sql.FromSome.TopFromSome TopFromSome]], representing
  * a valid ''from'' clause of a top level SQL ''select'' (contain no [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * pseudo joins), or this `RowProduct` will be impossible to use as a ''from'' clause of any ''select''.
  * It is so because otherwise such a subselect expression used under its outer select would hide the existence
  * of an unbound parameter from the outside, preventing providing a value for it and making the outer clause
  * appear parameterless (and hence a valid ''free'' select or subselect clause. This is enforced
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
  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.Of UnboundParam.Of[X]#M]].
  *
  * This join is typically written in an abbreviated form `FromSome WithParam X`
  * and `FromSome JoinParam ("name" ?: X)#P` for a parameter of type `X` named with a string literal.
  *
  * @tparam L the actual ''from'' clause of the parameterized select statement, used as the left side of the 'join'.
  * @tparam R a synthetic `UnboundParam` mapping, the subject of which is the parameter type.
  *
  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
  * @see [[net.noresttherein.oldsql.sql.WithParam]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */ //lets try to widen the bound to `FromClause` - union types should do it, the problem is the upper bound on WithLeft and the ilk.
@showAsInfix //todo: a common base type for Join and JoinParam (but not Subselect) - the new Generalized supertype
sealed trait JoinParam[+L <: FromSome, R[O] <: ParamAt[O]]
	extends ParamClause[L, R] with AndFrom[L, R] with AndFromTemplate[L, R, L JoinParam R]
{ thisClause => //consider: renaming to ParamJoin
	//consider: it's tempting to have simply the parameter type as the second parameter, not the mapping,
	// but it would require changes to RowDecomposition, ExpandedBy et al, GetTable...
	override type Last[O <: RowProduct] = JoinedRelation[O, R] { type FromLast = RowProduct AndFrom R }
	override type Generalized = left.Generalized JoinParam R
	override type Complete    = left.Complete JoinParam R
	override type NoAlias     = left.Self JoinParam R
	override type Self       <: left.Self JoinParam R

	override def generalizedClass :Class[left.Generalized JoinParam R] = classOf[left.Generalized JoinParam R]
	override def selfClass        :Class[Self] = classOf[left.Self JoinParam R].asInstanceOf[Class[Self]]
	override def narrow           :left.type JoinParam R

	override type LeftBound                       = FromSome
	override type GeneralizedLeft[+F <: FromSome] = F JoinParam R
	override type NoAliasLeft[+F <: FromSome]     = F JoinParam R
	override type WithLeft[+F <: FromSome]       <: F JoinParam R

	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :SingleBoolean[E] =
		left.filter(target)(expansion.expandFront[left.Generalized, R]) && condition.basedOn(target)

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

	override def generalizedExpansion[P <: FromSome] :P PrefixOf (P JoinParam R) =
		PrefixOf.itself[P].expand[JoinParam, R]


	override type JoinedWith[+S <: RowProduct, +J[+F <: S, M[O] <: MappingAt[O]] <: F NonParam M] =
		WithLeft[left.JoinedWith[S, J]]

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) :JoinedWith[F, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override def appendedTo[F <: FromClause](prefix :F) :JoinedWith[F, NonParam] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit JoinParam R
	override type Inner    = left.Inner JoinParam R


	protected override def defaultSpelling[X](context :SQLContext[X], params :Parameterization[X, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[X] =
	{
		val leftSQL = spelling(left)(context, params.left)
		//the alias *should* never be used, it is here because we need a placeholder value to keep indexing consistent
		val expanded = leftSQL.context.param(aliasOpt getOrElse paramCount.toString)
//				val shiftParams = leftSQL.params.param[Generalized, left.Generalized, P, left.Params, X]
		val res = SpelledSQL(leftSQL.sql, leftSQL.setter, expanded)
		if (condition == True) res
		else res && (spelling.inWhere(condition)(self, _, params))
	}

	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Params] =
		//even if alias is not unique, it does not matter because no one will try to associate it with param expressions
		left.spellingContext.param(aliasOpt getOrElse paramCount.toString, Requisite { params :Params => params.init })


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinParam.__ @unchecked]

}






object JoinParam {

	/** A template `JoinParam` instance with a dummy mapping, for use as a polymorphic factory of `JoinParam` joins. */
	final val template :JoinParam.__ = JoinParam(Table.Dummy, ParamRelation[Unit]())

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
	         (implicit castL :BaseMappingSubject[F, FA, A]) :From[F] WithParam X =
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
	  *             to its [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form, created join
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
	  *             to its [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form, created join
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
	                            filter :SingleBoolean[F#Generalized JoinParam ParamRelation[X]#Param]) :F WithParam X =
		JoinParam[from.type, ParamRelation[X]#Param, X, Nothing](from, LastParam(param), None)(filter)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[N, X] ]] DSL instead.
	  * @param from a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *             If it is not an [[net.noresttherein.oldsql.sql.FromSome.TopFromSome independent]], 'outer' clause,
	  *             that is it contains a `Subselect` join (or does not conform
	  *             to its [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form, created join
	  *             will be invalid - it will not be possible to create
	  *             a [[net.noresttherein.oldsql.sql.Select select]] with it as its ''from'' clause.
	  *             It may still however be useful for other purposes and the parameter can be ''bound'' at a later
	  *             time, removing the join an replacing its usages with
	  *             [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]], or substituting
	  *             it with a normal [[net.noresttherein.oldsql.schema.Table table]].
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
	  * @return `F JoinParam `[[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.Of UnboundParam.Of[X]#M]]` `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
	  */
	def apply[F <: FromSome, N <: Label, X](from :F, param :NamedParamRelation[N, X]) :F WithParam X As N =
		JoinParam[F, UnboundParam.Of[X]#M, X, N](from, LastParam(param), Some(param.name))(True)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param[N, X] ]] DSL instead.
	  * @param from a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *             If it is not an [[net.noresttherein.oldsql.sql.FromSome.TopFromSome independent]], 'outer' clause,
	  *             that is it contains a `Subselect` join (or does not conform
	  *             to its [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form, created join
	  *             will be invalid - it will not be possible to create
	  *             a [[net.noresttherein.oldsql.sql.Select select]] with it as its ''from'' clause.
	  *             It may still however be useful for other purposes and the parameter can be ''bound'' at a later
	  *             time, removing the join an replacing its usages with
	  *             [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]], or substituting
	  *             it with a normal [[net.noresttherein.oldsql.schema.Table table]].
	  * @param param  the last relation of the created ''from'' clause,
	  *               using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F JoinParam `[[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.Of UnboundParam.Of[X]#M]]` `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
	  */
	def apply[F <: FromSome, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :SingleBoolean[F#Generalized JoinParam ParamRelation[X]#Param]) :F WithParam X As N =
		JoinParam[F, UnboundParam.Of[X]#M, X, N](from, LastParam(param), Some(param.name))(filter)


	/** Creates [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :JoinParamFactory[X] = new JoinParamFactory[X] {}

	/** Factory of [[net.noresttherein.oldsql.sql.JoinParam parameter pseudo joins]] for parameter type `X`.
	  * @tparam X the type of the unbound parameter introduced by this factory.
	  */
	sealed trait JoinParamFactory[X] extends Any {
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
		  *             to its [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form, created join
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
		  *             to its [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form, created join
		  *             will be invalid - it will not be possible to create
		  *             a [[net.noresttherein.oldsql.sql.Select select]] with it as its ''from'' clause.
		  *             It may still however be useful for other purposes and the parameter can be ''bound'' at a later
		  *             time, removing the join an replacing its usages with
		  *             [[net.noresttherein.oldsql.sql.ast.BoundParam bound parameters]], or substituting
		  *             it with a normal [[net.noresttherein.oldsql.schema.Table table]].
		  * @param name a parameter name, used for debugging and logging.
		  * @param form the form for the last [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation parameter]]
		  *             relation of the created ''from'' clause,
		  *             using [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @return `F` [[net.noresttherein.oldsql.sql.WithParam WithParam]] `X`.
		  */
		final def apply[F <: FromSome](from :F, name :String)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X, Nothing](from, LastParam(ParamRelation[X](name)), None)(True)
	}


	private[sql] def apply[L <: FromSome, R[O] <: UnboundParam[X, O], X, A <: Label]
	                      (clause :L, param :LastRelation[R, X] { type FromLast = RowProduct AndFrom R }, asOpt :Option[A])
	                      (cond :SingleBoolean[clause.Generalized JoinParam R]) :L JoinParam R As A =
		new JoinParam[clause.type, R] with AbstractExpanded[clause.type, R, X] {
			override val left = clause
			override val last :LastRelation[R, X] = param //{ type FromLast = RowProduct AndFrom R } = param
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1
			override val parameterization =
				left.parameterization.param[left.Params, X, left.Self, R, left.Self JoinParam R].as(self)
			override def lastRelation = last
			override lazy val tableStack = super.tableStack
			override lazy val fullTableStack = super.fullTableStack

			override def lastAsIn[E <: RowProduct](implicit expansion :this.FromLast PrefixOf E) :Last[E] = last.asIn[E]

			override type Alias = A
			override type NoAliasCopy = left.type JoinParam R
			override type Copy = left.type JoinParam R As A
			override type Self = left.Self JoinParam R As A
			override type WithLeft[+F <: FromSome] = F JoinParam R As A

			override def narrow :left.type JoinParam R As A = this.asInstanceOf[left.type JoinParam R As A]

			override def withCondition(filter :SingleBoolean[Generalized]) =
				JoinParam[left.type, R, X, A](left, last, aliasOpt)(filter)

			override def withLeft[F <: FromSome](left :F)(filter :SingleBoolean[left.Generalized JoinParam R]) =
				JoinParam[F, R, X, A](left, last, aliasOpt)(filter)

			//consider: replacing the last relation with NamedParamRelation, but then JoinParam must always use UnboundParam
			override def aliased[N <: Label](alias :N) =
				JoinParam[left.type, R, X, N](left, last, Some(alias))(condition)

			override def expansion[C <: FromSome] :C PrefixOf (C JoinParam R As A) =
				PrefixOf.itself[C].expand[JoinParam, R].as[A]

			override def visit[Y](visitor :RowProductVisitor[Y]) = visitor.joinParam[L, R, X](this)

		}.asInstanceOf[L JoinParam R As A]




	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: TopFromSome, X](param :F WithParam X) :Opt[(F, Relation[ParamRelation[X]#Param])] =
		Got(param.left -> param.right)

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: RowProduct, X](from :F Adjoin M forSome { type M[O] <: TypedMapping[X, O] })
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
			case param :JoinParam.__ @unchecked => Got(param.left -> param.right)
			case _ => Lack
		}



	implicit def joinParamComposition[L <: FromSome, R[O] <: ParamAt[O]]
			:ExpandedComposition[L, R, JoinParam, FromSome, ParamAt]
				{ type Generalized[+A <: FromSome, B[O] <: ParamAt[O]] = A JoinParam B } =
		composition.asInstanceOf[ExpandedComposition[L, R, JoinParam, FromSome, ParamAt] {
			type Generalized[+A <: FromSome, B[O] <: ParamAt[O]] = JoinParam[A, B]
		}]

	private[this] val composition =
		new ExpandedComposition[FromSome, ParamAt, JoinParam, FromSome, ParamAt] {
			override def apply[C <: FromSome](template :FromSome JoinParam ParamAt, clause :C) :C JoinParam ParamAt =
				template.withLeft(clause)(True)
		}



	/** Type alias for `JoinParam` with erased type parameters, covering all instances of `JoinParam`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = JoinParam[_ <: FromSome, ParamRelation[_]#Param]

	/** A type alias for [[net.noresttherein.oldsql.sql.JoinParam.FromLast FromLast]] member type of
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] using mapping `M`.
	  */
	type FromLast[M[O] <: ParamAt[O]] = RowProduct AndFrom M

	/** The least upper bound of all ''from'' clauses ending with parameter of mapping `M` */
	type LUB[M[O] <: ParamAt[O]] = FromSome JoinParam M


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
  * clause on its left side with a special mapping `R[O] <: UnboundParam[X, O]`. The parameter value is unspecified
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
  * with type `X` is [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam.Of UnboundParam.Of[X]#M]].
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
  * @tparam L the actual ''group by'' clause of the parameterized select statement, used as the left side of the 'join'.
  * @tparam R a synthetic `UnboundParam` mapping, the subject of which is the parameter type.
  *
  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
  * @see [[net.noresttherein.oldsql.sql.ByParam]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */ //consider: renaming to AndParam //ParamGroup
@showAsInfix
sealed trait GroupParam[+L <: GroupByClause, R[O] <: ParamAt[O]]
	extends ParamClause[L, R] with GroupJoin[L, R] with GroupByClauseTemplate[L GroupParam R, L GroupParam R]
{ thisClause =>
	override def generalizedClass :Class[left.Generalized GroupParam R] = classOf[left.Generalized GroupParam R]
	override def selfClass        :Class[Self] = classOf[left.Generalized GroupParam R].asInstanceOf[Class[Self]]

	override type Last[O <: RowProduct] = JoinedRelation[O, R] { type FromLast = RowProduct AndBy R }
	override type Generalized = left.Generalized GroupParam R
	override type Complete    = left.Complete GroupParam R
	override type NoAlias     = left.Self GroupParam R
	override type Self       <: left.Self GroupParam R

	override type GeneralizedLeft[+F <: GroupByClause] = F GroupParam R
	override type NoAliasLeft[+F <: GroupByClause]     = F GroupParam R
	override type WithLeft[+F <: GroupByClause]       <: F GroupParam R


	/** Expands the given ''group by'' clause `left` by adding to it the parameter relation of this instance.
	  * This is a lower-level 'virtual constructor' method and application code should prefer using the API
	  * provided by [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate NonEmptyRowTemplate]]
	  * (and [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate RowProductTemplate]]).
	  * @param left   the exact instance used as [[net.noresttherein.oldsql.sql.Adjoin.left left]] property
	  *               of the result.
	  * @param filter the exact boolean expression to use as the filter condition of the result.
	  *               It will not be anchored or altered in any way by this method.
	  */
	override def withLeft[F <: GroupByClause] //overriden to widen the upper bound
	                     (left :F)(filter :GroupedBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[F]

	override def columnCount :Int = left.columnCount

	override def bind(param :Param) :AppliedParam = {
		val substitute = SQLScribe.applyParam(self, left.generalized, param)
		left.filtered(substitute(condition))
	}

	override def bind(params :Params) :Paramless = condition match {
		case True => left.bind(params.init)
		case _ =>
			val res = left.bind(params.init)
			val substitute = SQLScribe.applyParams(self, res.generalized)(params)
			res.filtered(substitute(condition)).asInstanceOf[Paramless]
	}


	override type JoinedWith[+P <: RowProduct, +J[+F <: P, M[O] <: MappingAt[O]] <: F NonParam M] =
		WithLeft[left.JoinedWith[P, J]]

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) :JoinedWith[F, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override def appendedTo[F <: FromClause](prefix :F) :JoinedWith[F, NonParam] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit GroupParam R
	override type Inner    = left.Inner GroupParam R


	protected override def defaultSpelling[X](context :SQLContext[X], params :Parameterization[X, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[X] =
	{
		val leftSQL = spelling(left)(context, params.left[left.Generalized, R])
		SpelledSQL(leftSQL.sql, leftSQL.setter, leftSQL.context.grouped) //condition separately in having
	}

	protected override def groupingSpellingContext[Ps]
	                       (context :SQLContext[Ps], params :Parameterization[Ps, Generalized])
			:GroupingSpellingContext[Ps] =
		throw new IllegalArgumentException(
			"Cannot create GroupingSpellingContext for unbound parameter " + right + " of " + (typeString :String) + "."
		)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupParam.__ @unchecked]

}






object GroupParam {

	/** A template `GroupParam` instance with a dummy mapping, for use as a polymorphic factory of `GroupParam` pseudo joins. */
	final val template :GroupParam.__ =
		GroupParam(From(Table.Dummy).groupBy(Table.Dummy[RowProduct]), ParamRelation[Unit]())

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
	         (from :F, param :ParamRelation[X], filter :GroupedBoolean[F#Generalized GroupParam ParamRelation[X]#Param] = True)
			:F ByParam X =
		GroupParam[from.type, ParamRelation[X]#Param, X, Nothing](from, LastGroupParam(param), None)(filter)

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
	  * @return `F GroupParam ParamRelation[X]#Param`.
	  */
	def apply[F <: TopGroupByClause, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X]) :F ByParam X As N =
		GroupParam[F, UnboundParam.Of[X]#M, X, N](from, LastGroupParam(param), Some(param.name))(True)

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
	  * @return `F GroupParam ParamRelation[X]#Param`.
	  */
	def apply[F <: TopGroupByClause, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :GroupedBoolean[F#Generalized GroupParam ParamRelation[X]#Param]) :F ByParam X As N =
		GroupParam[F, UnboundParam.Of[X]#M, X, N](from, LastGroupParam(param), Some(param.name))(filter)


	/** Creates [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ByParamFactory[X] = new ByParamFactory[X] {}

	sealed trait ByParamFactory[X] extends Any {
		final def apply[F <: TopGroupByClause](from :F)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X, Nothing](from, LastGroupParam(ParamRelation[X]()), None)(True)

		final def apply[F <: TopGroupByClause](from :F, name :String)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X, Nothing](from, LastGroupParam(ParamRelation[X](name)), None)(True)
	}


	private[sql] def apply[L <: GroupByClause, M[O] <: UnboundParam[X, O], X, A <: Label]
	                      (clause :L, param :RelationSQL[AndBy.LUB[M], M, X, AndBy.LUB[M]], asOpt :Option[A])
	                      (cond :GroupedBoolean[clause.Generalized GroupParam M]) :L GroupParam M As A =
		new GroupParam[clause.type, M] with AbstractExpanded[clause.type, M, X] {
			override val left = clause
			override val last = param
			override val aliasOpt = asOpt
			override val condition = cond
			override val fromClause :Discrete = left.fromClause
			override val outer = left.outer
			override val fullSize = left.fullSize + 1
			override val parameterization =
				left.parameterization.param[left.Params, X, left.Self, M, left.Self GroupParam M].as(self)
			override def lastRelation = last
			override lazy val tableStack = super.tableStack
			override lazy val fullTableStack = super.fullTableStack

			override def lastAsIn[E <: RowProduct](implicit expansion :FromLast PrefixOf E) :Last[E] = last.asIn[E]

			override type Alias = A
			override type Self = left.Self GroupParam M As A
			override type WithLeft[+F <: GroupByClause] = F GroupParam M As A
			override type NoAliasCopy = left.type GroupParam M
			override type Copy = left.type GroupParam M As A
			override def narrow :left.type GroupParam M As A = this.asInstanceOf[left.type GroupParam M As A]

			override def withCondition(filter :GroupedBoolean[Generalized]) =
				GroupParam[left.type, M, X, A](left, last, aliasOpt)(filter)

			override def withLeft[F <: GroupByClause](left :F)(filter :GroupedBoolean[left.Generalized GroupParam M]) =
				GroupParam[F, M, X, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				GroupParam[left.type, M, X, N](left, last, Some(alias))(condition)


			override def visit[Y](visitor :RowProductVisitor[Y]) = visitor.groupParam[L, M, X](this)

		}.asInstanceOf[L GroupParam M As A]




	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: GroupByClause, X](param :F ByParam X) :Opt[(F, Relation[ParamRelation[X]#Param])] =
		Got(param.left -> param.right)

	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: RowProduct, X](from :F Adjoin M forSome { type M[O] <: TypedMapping[X, O] })
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
			case param :GroupParam.__ @unchecked => Got(param.left -> param.right)
			case _ => Lack
		}



	implicit def groupParamComposition[L <: GroupByClause, R[O] <: ParamAt[O]]
			:ExpandedComposition[L, R, GroupParam, GroupByClause, ParamAt]
				{ type Generalized[+A <: GroupByClause, B[O] <: ParamAt[O]] = A GroupParam B } =
		composition.asInstanceOf[ExpandedComposition[L, R, GroupParam, GroupByClause, ParamAt] {
			type Generalized[+A <: GroupByClause, B[O] <: ParamAt[O]] = GroupParam[A, B]
		}]

	private[this] val composition =
		new ExpandedComposition[GroupByClause, ParamAt, GroupParam, GroupByClause, ParamAt] {
			override def apply[C <: GroupByClause]
			                  (template :GroupByClause GroupParam ParamAt, clause :C) :C GroupParam ParamAt =
				template.withLeft(clause)(True)
		}



	/** Type alias for `GroupParam` with erased type parameters, covering all instances of `GroupParam`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = GroupParam[_ <: GroupByClause, ParamRelation[_]#Param]

	/** A type alias for [[net.noresttherein.oldsql.sql.JoinParam.FromLast FromLast]] member type of
	  * [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] using mapping `M`.
	  */
	type LUB[M[O] <: ParamAt[O]] = GroupByClause GroupParam M

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


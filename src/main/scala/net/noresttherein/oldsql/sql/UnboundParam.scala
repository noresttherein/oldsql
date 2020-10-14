package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnForm, ColumnMapping, GenericMappingExtract, Mapping, MappingExtract, Relation, SQLForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.NamedRelation
import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.AndFrom.AndFromMatrix
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{As, ExtendedBy, NonEmptyFrom, NonEmptyFromMatrix, OuterFrom, OuterFromSome, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.MappingSQL.{ColumnComponentSQL, ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.Extended.{AbstractExtended, ExtendedComposition, NonSubselect}
import net.noresttherein.oldsql.sql.GroupByAll.AndByAll
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.UnboundParam.{?:, FromParam, NamedParamRelation, ParamAt, ParamRelation}
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject
import net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix






/** Base trait for ''unbound'' query parameters, that is parameters without a known value, in contrast 
  * to the ''bound'' [[net.noresttherein.oldsql.sql.SQLTerm.SQLParameter SQLParameter]] expression.
  * It is represented as a special kind of join between an existing `FromClause` on the left, and a synthetic
  * `Mapping` subtype [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]].
  * Two concrete subclasses exist: [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] for ''discrete'' clauses
  * (''from'' clauses without a ''group by'' clause) and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
  * for clauses with a [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] join to the left.
  */
sealed trait UnboundParam[+F <: FromClause, P[O] <: ParamAt[O]] extends NonSubselect[F, P] { thisClause =>

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
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: (left.Self UnboundParam P) {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
	}



	/** The type of this parameter, that is the subject type of the joined mapping. */
	type Param = last.Subject //P[FromLast]#Subject

	override type Params = left.Params ~ Param

	override def isParameterized :Boolean = true


	override type JoinedWithSubselect[+S <: NonEmptyFrom] = Nothing

	override def joinedWithSubselect[S <: FromClause](prefix :S) :Nothing =
		throw new UnsupportedOperationException(
			"JoinParam.joinedWithSubselect: join parameters cannot appear as a part of a subselect clause. " +
				s"$this joinedWithSubselect $prefix"
		)



	override def isValidSubselect = false //either we are an outer clause and truly not a subselect, or we are illegal for subselect

	override type Base = Nothing
	override type DefineBase[+I <: FromClause] = Nothing
	override def base = throw new UnsupportedOperationException(s"JoinParam.base on $this")


	override type AsSubselectOf[+O <: NonEmptyFrom] = Nothing

	override def asSubselectOf[O <: FromClause](outer :O)(implicit extension :Implicit ExtendedBy O) :Nothing =
		throw new UnsupportedOperationException(
			"JoinParam.asSubselectOf: join parameters can't appear as a part of a subselect from clause. " +
				s"$this asSubselectOf $outer"
		)


	/** Provides the value for the joined parameter, removing it from this clause and replacing all references to it
	  * with bound parameters in the form of `SQLParameter` instances.
	  */
	def apply(value :Param) :left.Copy



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[UnboundParam.* @unchecked]

	override def name = "param"

}






/** Types used by the [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]] 'join' and its subtypes. */
object UnboundParam {

	/** A type wrapper for the type constructor of parameter mappings with string literals as names, present in their
	  * type signature. Importing it imports also an implicit conversion adding a `?:[X]` factory method
	  * to string literals.
	  */
	type ?:[N <: Label, X] = { type T[O] = LabeledFromParam[N, X, O] }

	implicit def ?:[N <: Label](name :N) = new method_?:[N](name)

	class method_?:[N <: Label](private val name :N) extends AnyVal {
		/** Creates a synthetic `Relation` of the given name, creating named parameter mappings. */
		def ?:[X :SQLForm] :NamedParamRelation[N, X] = new NamedParamRelation[N, X](name)
	}




	/** Matches all `UnboundParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: FromClause, P[O] <: ParamAt[O]](param :F UnboundParam P) :Option[(F, Relation[P])] =
		Some(param.left -> param.right)

	/** Matches all `UnboundParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: FromClause, X](from :F Compound M forSome { type M[O] <: RefinedMapping[X, O] })
			:Option[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :UnboundParam[_, ParamRelation[X]#Param @unchecked] => Some((from.left, param.right))
			case _ => None
		}

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :FromClause) :Option[(FromClause, Relation[M] forSome { type M[O] <: FromParam[_, O] })] =
		from match {
			case param :UnboundParam.* @unchecked => Some(param.left -> param.right)
			case _ => None
		}




	/** Type alias for `UnboundParam` with erased type parameters, covering all instances of `UnboundParam`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = UnboundParam[_ <: FromClause, M] forSome { type M[O] <: FromParam[_, O] }

	/** A curried type constructor for `UnboundParam` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromClause] = { type F[R[O] <: ParamAt[O]] = L UnboundParam R }

	/** A curried type constructor for `UnboundParam` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: FromClause] = L UnboundParam R }






	//it would be beneficial to make these relations protected in order for the applications to not depend on them
	//  and use only an alias for Relation[({ type P[O] = FromParam[X, O] })#P], but every JoinParam must use
	//  a unique FromParam instance to distinguish their components.
	abstract class GenericParamRelation[X, M[O] <: FromParam[X, O]](val name :String)(implicit val form :SQLForm[X])
		extends Relation[M]
	{
		type Param[O] = M[O]

		override def sql :String = name + "?:" + form

		def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamRelation.*]

		override def equals(that :Any) :Boolean = that match {
			case other :ParamRelation.* =>
				(other eq this) || other.canEqual(this) && other.name == name && other.form == form
			case _ => false
		}

		override def hashCode :Int = name.hashCode * 31 + form.hashCode
	}



	/** A special, artificial `Relation` implementation dedicated to
	  * the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] mapping class, representing a query parameter.
	  */ //consider: making this private to prevent its usage outside of JoinParam/GroupParam; currently we use the Param type
	class ParamRelation[X :SQLForm](name :String)
		extends GenericParamRelation[X, ({ type T[O] = FromParam[X, O] })#T](name)
	{
		override type Param[O] = FromParam[X, O]

		private[this] val param = new FromParam[X, Any](name)

		override def apply[O] :FromParam[X, O] = param.asInstanceOf[FromParam[X, O]]
	}



	object ParamRelation {
		def apply[X :SQLForm](name :String) :ParamRelation[X] = new ParamRelation[X](name)

		def apply[X :SQLForm]() :ParamRelation[X] = new ParamRelation("?")

		def unapply(source :Relation.*) :Option[(SQLForm[_], String)] = source match {
			case param :GenericParamRelation[_, _] => Some(param.form -> param.name)
			case _ => None
		}

		type * = GenericParamRelation[_, M] forSome { type M[O] <: FromParam[_, O] }
	}



	/** A special, artificial `Relation` implementation dedicated to
	  * the [[net.noresttherein.oldsql.sql.UnboundParam.LabeledFromParam LabeledFromParam]] mapping class, representing
	  * a labeled query parameter.
	  */
	class NamedParamRelation[N <: Label, X :SQLForm](override val name :N)
		extends GenericParamRelation[X, (N ?: X)#T](name) with NamedRelation[N, (N ?: X)#T]
	{
		private[this] val param = new LabeledFromParam[N, X, Any](name)

		override def apply[O] :LabeledFromParam[N, X, O] = param.asInstanceOf[LabeledFromParam[N, X, O]]
	}






	type ParamExtract[P, S, O] = GenericMappingExtract[ParamMapping[P, S, O], P, S, O]
	type ParamColumnExtract[P, S, O] = GenericMappingExtract[FromParam[P, O]#ParamColumn[S], P, S, O]

	sealed abstract class ParamMapping[P, S, O] protected (implicit sqlForm :SQLForm[S]) extends FormMapping[S, O] {
		def root :FromParam[P, O]
		def extract :ParamExtract[P, S, O]
		def derivedForm :SQLWriteForm[P] = form compose extract
	}



	/** The generic type supertype of `FromParam` mappings representing SQL statement parameters,
	  * used as the upper bound for the `Mapping`'s used by `JoinParam` to avoid wildcard types of `FromParam[_, O]`,
	  * which break the type compatibility of the subject (parameter) type.
	  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
	  */
	sealed trait ParamAt[O] extends Mapping {
		override type Origin = O
		val form :SQLForm[Subject]
		
		def root :FromParam[Subject, O] 
	}



	/** A `Mapping` type representing a query parameter, the value of which is not known. While the `SQLParameter`
	  * expression can be used to represent a statement parameter, its value must be known when the expression is created.
	  * By representing a statement parameter as a mapping that can be used in the same way as table mappings
	  * in `FromClause` relation lists, we can represent any value obtainable from `P`
	  * by a function `P => T` as a component `(P #? _)#Component[T]` wrapping that function, which can be used
	  * to create component expressions for that function. In particular, a `JoinedRelation[S, ParamRelation[P]#Param]`
	  * is a expression which value will be substituted by a statement parameter `P`.
	  * @param name a suggested name of the parameter for debugging purposes, which may, but doesn't have to be used
	  *             for the name of the parameter in the generated SQL.
	  * @tparam P the parameter type needed to prepare statements using this mapping in their ''from'' clauses.
	  * @tparam O a marker type serving as a unique identifier for this mapping within a `FromClause`.
	  *///consider: think of another name, this can be confusing
	class FromParam[P, O](val name :String)(implicit sqlForm :SQLForm[P])
		extends ParamMapping[P, P, O] with ParamAt[O]
	{ This =>

		def this()(implicit form :SQLForm[P]) = this("?")

		override def root :FromParam[P, O] = this
		override def extract :ParamExtract[P, P, O] = GenericMappingExtract.ident(this)
		override def derivedForm :SQLWriteForm[P] = form


		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[P] =
			SQLWriteForm.combine(
				components.toSeq.map( _ match {
					case This(param) => param.derivedForm
					case comp => throw new IllegalArgumentException(s"Mapping $comp is not a component of parameter mapping $this")
				}) :_*
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
			override def extract :ParamExtract[P, T, O] = GenericMappingExtract(this)(pick)
			override def toString = s"$This[$form]"
		}



		/** A column value derived from the query parameter `P`, represented as a pseudo column of an artificial
		  * mapping in SQL expressions.
		  */
		class ParamColumn[T] private[FromParam] (pick :P =?> T)(implicit override val form :ColumnForm[T])
			extends ParamComponent[T](pick) with ColumnMapping[T, O]
		{
			override def extract :ParamColumnExtract[P, T, O] = GenericMappingExtract(this)(pick)
			override def name :String = This.name
		}


		override def mappingName :String = name
		override def toString :String = name + ":" + form



		def unapply[X](expr :ColumnSQL[_, _, X]) :Option[ParamColumn[X]] = expr match {
			case ComponentSQL(_, MappingExtract(_, _, col :FromParam[_, _]#ParamColumn[_])) if col.root == this =>
				Some(col.asInstanceOf[ParamColumn[X]])
			case _ =>
				None
		}

		def unapply[X](expr :SQLExpression[_, _, X]) :Option[ParamMapping[P, X, O]] = expr match {
			case ComponentSQL(_, MappingExtract(_, _, comp :ParamMapping[_, _, _])) if comp.root == this =>
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

		type Of[S] = { type T[O] = FromParam[S, O] }
	}



	class LabeledFromParam[N <: Label, X :SQLForm, O](override val name :N)
		extends FromParam[X, O](name) with LabeledMapping[N, X, O]


	object LabeledFromParam {

		def apply[P :SQLForm, N <: Label, O](name :N) :FromParam[P, O] = new LabeledFromParam(name)

		type Projection[N <: Label, S] = { type WithOrigin[O] = LabeledFromParam[N, S, O] }
	}



	object UnboundParamSQL {

		def unapply[F <: FromClause, T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		           (expr :ColumnComponentSQL[F, T, E, M, V, O])
				:Option[(FromParam[E, O], ParamColumnExtract[E, V, O], Int)] =
			if (expr.extract.export.isInstanceOf[ParamMapping[_, _, _]]) {
				val param = expr.extract.export.asInstanceOf[FromParam[E, O]#ParamColumn[V]]
				Some((param.root, param.extract, expr.origin.shift))
			} else
				  None

		def unapply[F <: FromClause, T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		           (expr :ComponentSQL[_, T, E, M, V, O]) :Option[(FromParam[E, O], ParamExtract[E, V, O], Int)] =
			if (expr.extract.export.isInstanceOf[ParamMapping[_, _, _]]) {
				val param = expr.extract.export.asInstanceOf[ParamMapping[E, V, O]]
				Some((param.root, param.extract, expr.origin.shift))
			} else
				  None

		def unapply[X](expr :SQLExpression[_, _, X])
				:Option[(FromParam[P, O], ParamExtract[P, X, O], Int)] forSome { type P; type O } =
			expr match {
				case ComponentSQL(table, extractor) if extractor.export.isInstanceOf[ParamMapping[_, _, _]] =>
					val param = extractor.export.asInstanceOf[ParamMapping[Any, X, Any]]
					Some((param.root, param.extract, table.shift))
				case _ => None
			}

	}
}









/** A special, artificial 'join' type which joins the clause on its left side with a synthetic mapping
  * `P[O] <: FromParam[X, O]`, representing a query parameter `X`, unspecified at this point. To distinguish
  * it from the ''bound'' [[net.noresttherein.oldsql.sql.SQLTerm.SQLParameter SQLParameter]] SQL expression, which
  * translates to a statement parameter, but requires a value on creation, it is often referred to as an ''unbound'' 
  * parameter. It allows to filter a given ''from'' clause using values to be provided only at the execution time, 
  * which can be obtained by applying an arbitrary scala function to `X`.
  *
  * This type represents an ungrouped ''from'' clause (i.e., without a ''group by'' clause), and hence it can only
  * be used following another [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome ungrouped]] clause; a mirror type
  * exists in the form of [[net.noresttherein.oldsql.sql.GroupParam GroupParam]], which offers the same functionality,
  * but for parameters used in the ''group by''/''having'' clauses. The mapping of the synthetic relation in this clause,
  * aside from representing the parameter value itself, can also be used to create additional subcomponents with values
  * derived from the parameter value, which can be used in an `SQLExpression` as any other component. The mappings
  * themselves are however only shills, replaced when creating the select statement, and only their associated
  * `SQLForm`s are being used in the mapping process. The type constructor for a parameter mapping with type `X` is
  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.Of FromParam.Of[X]#T]] and
  * [[net.noresttherein.oldsql.sql.UnboundParam.?: (N ?: X)#T)]] for a mapping
  * labeled with a string literal `N <: String with Singleton`.
  *
  * This join is typically written in an abbreviated form `FromSome WithParam X` (or `FromSome <=? X`)
  * and `FromSome JoinParam ("name" ?: X)#T` for a parameter of type `X` named with a string literal.
  * A ''from'' clause can have unbound parameters only in its most outer section, before any 
  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins (and, in case of this type, before the
  * [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] clause if present) in order to be valid.
  * While it is possible to create instances which violate this property, both due to technical limitations and
  * in order to allow 'placeholder' parameters on temporary, component instances, they will be impossible to
  * use in a [[net.noresttherein.oldsql.sql.SelectSQL.SubselectSQL SubselectSQL]] expression. It is so because
  * otherwise such a subselect expression used under its outer select would hide the existence of an unbound parameter
  * from the outside, preventing providing a value for it and making the outer clause appear parameterless
  * (and hence a valid ''free'' select or subselect clause. This is enforced
  * by the [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]`[F]` type, used to define valid
  * subselect clauses of a clause `F`.
  *
  * @tparam F the actual ''from'' clause of the parameterized select statement, used as the left side of the 'join'.
  * @tparam P a synthetic `FromParam` mapping, the subject of which is the parameter type.
  *           
  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam.WithParam]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */ //lets try to widen the bound to `DiscreteFrom`
sealed trait JoinParam[+F <: FromSome, P[O] <: ParamAt[O]]
	extends UnboundParam[F, P] with AndFrom[F, P] with AndFromMatrix[F, P, F JoinParam P]
{ thisClause =>

	override type Generalized = left.Generalized JoinParam P
	override type Dealiased = left.Self JoinParam P
	override type Self <: left.Self JoinParam P

	protected override def narrow :left.type JoinParam P

	override type GeneralizedLeft[+L <: FromSome] = L JoinParam P
	override type DealiasedLeft[+L <: FromSome] = L JoinParam P
	override type WithLeft[+L <: FromSome] <: L JoinParam P


	override def filter[E <: FromClause](target :E)(implicit extension :Generalized PartOf E) :GlobalBoolean[E] =
		left.filter(target)(extension.extendFront[left.Generalized, P]) && condition.basedOn(target)


	override def generalizedExtension[C <: FromSome] :C PrefixOf (C JoinParam P) =
		PrefixOf.itself[C].extend[JoinParam, P]


	override type JoinedWith[+S <: FromClause, +J[+L <: S, R[O] <: MappingAt[O]] <: L AndFrom R] =
		WithLeft[left.JoinedWith[S, J]]

	override def joinedWith[S <: FromSome](prefix :S, firstJoin :Join.*) :JoinedWith[S, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override def appendedTo[S <: DiscreteFrom](prefix :S) :JoinedWith[S, AndFrom] =
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
	  */
	type WithParam[+F <: OuterFromSome, X] = JoinParam[F, FromParam.Of[X]#T]

	/** A type alias for `JoinParam` accepting parameter type `X`. As a `FromClause` containing a `JoinParam` join
	  * in its type is a preliminary from clause which will be translated to a parameterized statement, it uses
	  * an a 'inverse function symbol' as a mnemonic: `From[Users] <=? String`. This is equivalent to `WithParam[F, X]`.
	  */
	type <=?[+F <: OuterFromSome, X] = WithParam[F, X]

	
	
	/** Create an artificial join between the given relation on the left side, and the the `param` special relation 
	  * representing a query parameter of type `X` as the right side. The ''where'' clause can be subsequently specified 
	  * using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.DiscreteFrom.OuterFromSomeExtension.param param[X] ]] DSL instead.
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
	         (from :Relation[F], param :ParamRelation[X])
	         (implicit castL :JoinedRelationSubject[From, F, FA, MappingOf[A]#TypedProjection]) :From[F] WithParam X =
		JoinParam(From(from), param, True)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.DiscreteFrom.OuterFromSomeExtension.param param[X] ]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam[X, _] ]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F` [[net.noresttherein.oldsql.sql.JoinParam.WithParam WithParam]] `X`.
	  */
	def apply[F <: OuterFromSome, X](from :F, param :ParamRelation[X],
	                                 filter :GlobalBoolean[F#Generalized JoinParam ParamRelation[X]#Param] = True)
			:F WithParam X =
		JoinParam[from.type, ParamRelation[X]#Param, X, Nothing](from, LastRelation(param), None)(filter)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.DiscreteFrom.OuterFromSomeExtension.param param[N, X] ]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam[X, _] ]] `Mapping` type.
	  * @return `F JoinParam (N `[[net.noresttherein.oldsql.sql.UnboundParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: OuterFromSome, N <: Label, X](from :F, param :NamedParamRelation[N, X]) :F JoinParam (N ?: X)#T =
		JoinParam[F, (N ?: X)#T, X, Nothing](from, LastRelation(param), None)(True)

	/** Create an artificial join between the ''from'' clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.DiscreteFrom.OuterFromSomeExtension.param param[N, X] ]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam[X, _] ]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F JoinParam (N `[[net.noresttherein.oldsql.sql.UnboundParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: OuterFromSome, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :GlobalBoolean[F#Generalized JoinParam NamedParamRelation[N, X]#Param]) :F JoinParam (N ?: X)#T =
		JoinParam[F, (N ?: X)#T, X, Nothing](from, LastRelation(param), None)(filter)


	/** Creates [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: OuterFromSome](from :F)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X, Nothing](from, LastRelation(ParamRelation[X]()), None)(True)

		def apply[F <: OuterFromSome](from :F, name :String)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X, Nothing](from, LastRelation(ParamRelation[X](name)), None)(True)
	}



	private[sql] def apply[L <: FromSome, P[O] <: FromParam[X, O], X, A <: Label]
	                      (clause :L, param :LastRelation[P, X], asOpt :Option[A])
	                      (cond :GlobalBoolean[clause.Generalized JoinParam P]) :L JoinParam P As A =
		new JoinParam[clause.type, P]
			with AbstractExtended[clause.type, P, X]
			with NonEmptyFromMatrix[clause.type JoinParam P, clause.type JoinParam P As A]
		{
			override val left = clause
			override val last = param
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

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

			override def extension[C <: FromSome] :C PrefixOf (C JoinParam P As A) =
				PrefixOf.itself[C].extend[JoinParam, P].as[A]


			override def innerTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.extend(target) #:: left.innerTableStack(target)(stretch.extendFront[left.Generalized, P])



			override def apply(value :X) :left.Copy = {
				val substitute = SQLScribe.applyParam(generalized, left.generalized, value, 0)
				left filtered substitute(condition)
			}


			override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.joinParam[L, P, X](this)

		}.asInstanceOf[L JoinParam P As A]






	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: OuterFromSome, X](param :F WithParam X) :Option[(F, Relation[ParamRelation[X]#Param])] =
		Some(param.left -> param.right)

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: FromClause, X](from :F Compound M forSome { type M[O] <: RefinedMapping[X, O] })
			:Option[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :JoinParam[_, ParamRelation[X]#Param @unchecked] => Some((from.left, param.right))
			case _ => None
		}

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :FromClause) :Option[(FromSome, Relation[M] forSome { type M[O] <: FromParam[_, O] })] =
		from match {
			case param :JoinParam.* @unchecked => Some(param.left -> param.right)
			case _ => None
		}



	implicit def joinParamComposition[L <: FromSome, R[O] <: ParamAt[O]]
			:ExtendedComposition[L JoinParam R, L, R, JoinParam, FromSome, ParamAt]
				{ type Generalized[+A <: FromSome, B[O] <: ParamAt[O]] = A JoinParam B }=
		composition.asInstanceOf[ExtendedComposition[L JoinParam R, L, R, JoinParam, FromSome, ParamAt] {
			type Generalized[+A <: FromSome, B[O] <: ParamAt[O]] = JoinParam[A, B]
		}]

	private[this] val composition =
		new ExtendedComposition[FromSome JoinParam ParamAt, FromSome, ParamAt, JoinParam, FromSome, ParamAt] {
			override def apply[C <: FromSome](template :FromSome JoinParam ParamAt, clause :C) :C JoinParam ParamAt =
				template.withLeft(clause)(True)
		}



	/** Type alias for `JoinParam` with erased type parameters, covering all instances of `JoinParam`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = JoinParam[_ <: FromSome, M] forSome { type M[O] <: FromParam[_, O] }

	/** A curried type constructor for `JoinParam` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: OuterFromSome] = {
		type F[R[O] <: ParamAt[O]] = L JoinParam R
		type P[X] = L WithParam X
	}

	/** A curried type constructor for `JoinParam` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: OuterFromSome] = L JoinParam R }

}









/** A special, artificial 'join' type which adds a statement parameter of type `X` by extending a ''group by''
  * clause on its left side with a special mapping `P[O] <: FromParam[X, O]`. The parameter is unspecified at this point
  * and will need to be given to any `SQLStatement` produced from this clause. This type mirrors the functionality
  * of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]], which adds a parameter to the same effect by extending
  * an [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome ungrouped]] ''from'' clause. This duplication is required
  * for static type checking to be able to separate the artificial relations for the grouping expressions
  * of the group by clause from the real database relations: just as `JoinParam[_, _] <: FromSome`,
  * `GroupParam[_, _] <: GroupByClause`, both restricting their use and allowing them to be placed in between other
  * relations/grouping expressions. To distinguish it from the ''bound''
  * [[net.noresttherein.oldsql.sql.SQLTerm.SQLParameter SQLParameter]] SQL expression, which translates
  * to a statement parameter, but requires a value on creation, it is often referred to as an ''unbound'' parameter
  * (collectively with `JoinParam`). It allows to filter a given ''from'' clause using values to be provided only
  * at the execution time, which can be obtained by applying an arbitrary scala function to `X`. The mapping,
  * aside from representing the parameter value itself, can also be used to create additional subcomponents with values
  * derived from the parameter value, which can be used in an `SQLExpression` as any other component.
  * The mappings themselves are however only shills, replaced when creating the select statement, and only
  * their associated `SQLForm`s are being used in the mapping process. The type constructor for a parameter mapping
  * with type `X` is [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.Of FromParam.Of[X]#Param]] and
  * [[net.noresttherein.oldsql.sql.UnboundParam.?: (Alias ?: X)#T]] for a mapping labeled with a string literal
  * `A <: String with Singleton`.
  *
  * This join is typically written in an abbreviated form `GroupByClause ByParam X`
  * and `FromClause JoinParam ("name" ?: X)#T` for a parameter of type `X` named with a string literal,
  * following the naming scheme of [[net.noresttherein.oldsql.sql.ByAll ByAll]].
  * A ''from'' clause can have unbound grouping parameters only in its most outer section, before any
  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins (and, in case of this type, after the
  * [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] clause) in order to be valid.
  * While it is possible to create instances which violate this property, both due to technical limitations and
  * in order to allow 'placeholder' parameters on temporary, component instances, they will be impossible to
  * use in a [[net.noresttherein.oldsql.sql.SelectSQL.SubselectSQL SubselectSQL]] expression. It is so because
  * otherwise such a subselect expression used under its outer select would hide the existence of an unbound parameter
  * from the outside, preventing providing a value for it and making the outer clause appear parameterless
  * (and hence a valid ''free'' select or subselect clause. This is enforced
  * by the [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]`[F]` type, used to define valid
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
	extends UnboundParam[F, P] with AndByAll[F, P] with GroupByClauseMatrix[F GroupParam P, F GroupParam P]
{ thisClause =>

	override type Generalized = left.Generalized GroupParam P
	override type Dealiased = left.Self GroupParam P
	override type Self <: left.Self GroupParam P

	override type GeneralizedLeft[+L <: GroupByClause] = L GroupParam P
	override type DealiasedLeft[+L <: GroupByClause] = L GroupParam P
	override type WithLeft[+L <: GroupByClause] <: L GroupParam P

	override def withLeft[L <: GroupByClause] //overriden to make public
	                     (left :L)(filter :LocalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[L]

	override type JoinedWith[+S <: FromClause, +J[+L <: S, R[O] <: MappingAt[O]] <: L AndFrom R] =
		WithLeft[left.JoinedWith[S, J]]

	override def joinedWith[S <: FromSome](prefix :S, firstJoin :Join.*) :JoinedWith[S, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override def appendedTo[S <: DiscreteFrom](prefix :S) :JoinedWith[S, AndFrom] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit GroupParam P
	override type Inner = left.Inner GroupParam P



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupParam.* @unchecked]

}






object GroupParam {

	/** A template `GroupParam` instance with a dummy mapping, for use as a polymorphic factory of `GroupParam` pseudo joins. */
	final val template :GroupParam.* = GroupParam(GroupByAll(From(Relation.Dummy), Relation.Dummy), ParamRelation[Unit]())

	/** An alias for `GroupParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */
	type ByParam[+F <: GroupByClause, X] = GroupParam[F, ParamRelation[X]#Param]



	/** Add a statement parameter to the ''group by'' clause given as the left side, in the form of
	  * the `param` special relation representing a query parameter of type `X` as the right side.
	  * An additional condition for the ''having'' clause can be subsequently specified using the
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix.having having]] or
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix.having having]] method.
	  * It is a lower level method; it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.GroupByClause.OuterGroupingFromExtension.param param]]`[X]` DSL instead.
	  * @param from  a ''from''/''group by'' clause containing the non-empty list of relations and grouping expressions
	  *              preceding `param`. It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last pseudo relation of the created ''from'' clause for the introduced parameter,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]`[X, _]` `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F` [[net.noresttherein.oldsql.sql.GroupParam.ByParam ByParam]] `X`.
	  */
	def apply[F <: GroupByClause with OuterFrom, X]
	         (from :F, param :ParamRelation[X], filter :LocalBoolean[F#Generalized GroupParam ParamRelation[X]#Param] = True)
			:F ByParam X =
		GroupParam[from.type, ParamRelation[X]#Param, X, Nothing](from, RelationSQL(param, 0), None)(filter)

	/** Add a statement parameter to the ''group by'' clause given as the left side, in the form of
	  * the `param` special relation representing a query parameter of type `X` as the right side.
	  * An additional condition for the ''having'' clause can be subsequently specified using the
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix.having having]] or
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix.having having]] method.
	  * It is a lower level method; it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.GroupByClause.OuterGroupingFromExtension.param param]]`[N, X]` DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]`[X, _]` `Mapping` type.
	  * @return `F GroupParam (N `[[net.noresttherein.oldsql.sql.UnboundParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: GroupByClause with OuterFrom, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X]) :F GroupParam (N ?: X)#T =
		GroupParam[F, (N ?: X)#T, X, Nothing](from, RelationSQL(param, 0), None)(True)

	/** Add a statement parameter to the ''group by'' clause given as the left side, in the form of
	  * the `param` special relation representing a query parameter of type `X` as the right side.
	  * An additional condition for the ''having'' clause can be subsequently specified using the
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix.having having]] or
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix.having having]] method.
	  * It is a lower level method; it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.GroupByClause.OuterGroupingFromExtension().param param]]`[N, X]` DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]`[X, _]` `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F GroupParam (N `[[net.noresttherein.oldsql.sql.UnboundParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: GroupByClause with OuterFrom, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :LocalBoolean[F#Generalized GroupParam NamedParamRelation[N, X]#Param]) :F GroupParam (N ?: X)#T =
		GroupParam[F, (N ?: X)#T, X, Nothing](from, RelationSQL(param, 0), None)(filter)


	/** Creates [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: GroupByClause with OuterFrom](from :F)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X, Nothing](from, RelationSQL(ParamRelation[X](), 0), None)(True)

		def apply[F <: GroupByClause with OuterFrom](from :F, name :String)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X, Nothing](from, RelationSQL(ParamRelation[X](name), 0), None)(True)
	}



	private[sql] def apply[L <: GroupByClause, P[O] <: FromParam[X, O], X, A <: Label]
	                      (clause :L, param :RelationSQL[GroupByClause AndByAll P, P, X, GroupByClause AndByAll P],
	                       asOpt :Option[A])
	                      (cond :LocalBoolean[clause.Generalized GroupParam P]) :L GroupParam P As A =
		new GroupParam[clause.type, P]
			with AbstractExtended[clause.type, P, X]
			with GroupByClauseMatrix[clause.type GroupParam P, clause.type GroupParam P As A]
		{
			override val left = clause
			override val last = param
			override val aliasOpt = asOpt
			override val condition = cond
			override val from :Discrete = left.from
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

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


			override def innerTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.extend(target) #:: left.innerTableStack(target)(stretch.extendFront[left.Generalized, P])



			override def apply(value :X) :left.Copy = {
				val substitute = SQLScribe.applyParam(generalized, left.generalized, value, 0)
				left filtered substitute(condition)
			}


			override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.groupParam[L, P, X](this)

		}.asInstanceOf[L GroupParam P As A]






	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: GroupByClause, X](param :F ByParam X) :Option[(F, Relation[ParamRelation[X]#Param])] =
		Some(param.left -> param.right)

	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: FromClause, X](from :F Compound M forSome { type M[O] <: RefinedMapping[X, O] })
			:Option[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :GroupParam[_, ParamRelation[X]#Param @unchecked] => Some((from.left, param.right))
			case _ => None
		}

	/** Matches all `GroupParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :FromClause) :Option[(GroupByClause, Relation[M] forSome { type M[O] <: FromParam[_, O] })] =
		from match {
			case param :GroupParam.* @unchecked => Some(param.left -> param.right)
			case _ => None
		}



	implicit def groupParamComposition[L <: GroupByClause, R[O] <: ParamAt[O]]
			:ExtendedComposition[L GroupParam R, L, R, GroupParam, GroupByClause, ParamAt]
				{ type Generalized[+A <: GroupByClause, B[O] <: ParamAt[O]] = A GroupParam B } =
		composition.asInstanceOf[ExtendedComposition[L GroupParam R, L, R, GroupParam, GroupByClause, ParamAt] {
			type Generalized[+A <: GroupByClause, B[O] <: ParamAt[O]] = GroupParam[A, B]
		}]

	private[this] val composition =
		new ExtendedComposition[GroupByClause GroupParam ParamAt, GroupByClause, ParamAt, GroupParam, GroupByClause, ParamAt] {
			override def apply[C <: GroupByClause]
			                  (template :GroupByClause GroupParam ParamAt, clause :C) :C GroupParam ParamAt =
				template.withLeft(clause)(True)
		}


	
	/** Type alias for `GroupParam` with erased type parameters, covering all instances of `GroupParam`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = GroupParam[_ <: GroupByClause, M] forSome { type M[O] <: FromParam[_, O] }

	/** A curried type constructor for `GroupParam` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: GroupByClause with OuterFrom] = {
		type F[R[O] <: ParamAt[O]] = L GroupParam R
		type P[X] = L ByParam X
	}

	/** A curried type constructor for `GroupParam` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: GroupByClause with OuterFrom] = L GroupParam R }
	
}


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
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.AndFrom.JoinedRelationSubject
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome, NonEmptyClause, OuterClause, OuterFromSome, PrefixOf, UngroupedFrom}
import net.noresttherein.oldsql.sql.MappingSQL.{ColumnComponentSQL, ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.JoinParam.{?:, FromParam, NamedParamRelation, ParamAt, ParamRelation}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLScribe.{RemoveParam, ReplaceParam}
import net.noresttherein.oldsql.sql.Extended.{BaseExtended, NonSubselect}






sealed trait AbstractJoinParam[+F <: FromClause, P[O] <: ParamAt[O]] extends Extended[F, P] with NonSubselect[F, P] {
	thisClause =>

	/** The type of this parameter, that is the subject type of the joined mapping. */
	type Param = P[FromLast]#Subject

	override type Generalized >: Self <: (left.Generalized AbstractJoinParam P) {
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
	}

	override type Self <: (left.Self AbstractJoinParam P) {
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
	}

	override type This >: this.type <: F AbstractJoinParam P



	override def joinedAsSubselect[S <: FromClause](prefix :S) :Nothing =
		throw new UnsupportedOperationException(
			"JoinParam.joinedAsSubselect: join parameters cannot appear as a part of a subselect clause. " +
				s"$this joinedAsSubselect $prefix"
		)

	override type Implicit = Nothing
	override type Outer = Nothing

	override def outer = throw new UnsupportedOperationException(s"JoinParam.outer on $this.")



	override type AsSubselectOf[+O <: FromClause] = Nothing

	override def asSubselectOf[O <: FromClause](outer :O)(implicit extension :Implicit ExtendedBy O) :Nothing =
		throw new UnsupportedOperationException(
			"JoinParam.asSubselectOf: join parameters can't appear as a part of a subselect from clause. " +
				s"$this asSubselectOf $outer"
		)



	type Params = left.Params ~ Param

	/** Provides the value for the joined parameter, removing it from this clause and replacing all references to it
	  * with bound parameters in the form of `SQLParameter` instances.
	  */
	def apply(value :Param) :F#This


//	/** Substitutes the joined parameter mapping for one labeled with `String` literal `name`. */
//	def as[N <: Label](name :N) :F JoinParam (N ?: Param)#T


	override def canEqual(that :Any) :Boolean =
		that.isInstanceOf[AbstractJoinParam[_, T] forSome { type T[O] <: ParamAt[O] }]

	
	override def name = "param"

}






/** A special, artificial 'join' type which joins the clause on its left side with a synthetic mapping
  * `P[O] &lt;: FromParam[X, O]`, representing a query parameter `X`, unspecified at this point. It allows to filter
  * a given ''from'' clause using values to be provided only at the execution time, which can be obtained by applying
  * an arbitrary scala function to `X`. The mapping, aside from representing the parameter value itself,
  * can also be used to create additional subcomponents with values derived from the parameter value, which can be used
  * in an `SQLExpression` as any other component. The mappings themselves are however only shills, replaced
  * when creating the select statement, and only their associated `SQLForm`s are being used in the mapping process.
  * The type constructor for a parameter mapping with type `X` is
  * [[net.noresttherein.oldsql.sql.JoinParam.ParamRelation#Param ParamRelation[X]#Param]] and
  * [[net.noresttherein.oldsql.sql.JoinParam.NamedParamRelation#Param NamedParamRelation[N, X]#Param]] for a mapping
  * labeled with a string literal `N &lt;: String with Singleton`.
  *
  * This join is typically written in an abbreviated form `FromClause WithParam X` (or `FromClause <=? X`)
  * and `FromClause JoinParam ("name" ?: X)#T` for a parameter of type `X` named with a string literal.
  * This class declares its `Implicit` clause as `Nothing` (rather than the `Implicit` of the left side) to ensure that
  * neither it nor any extension clause containing it does not conform to `FromClause.SubselectOf[C]` for any clause `C`,
  * in particular `C =:= left.Implicit`, thus preventing it from being used as a part of a subselect of the outer clause,
  * which would hide the existence of a parameter and make its outer source appear not parameterized.
  * As the result, only top-level select statement from clauses can include unbound parameters.
  *
  * @tparam F the actual from clause of the parameterized select statement.
  * @tparam P synthetic `FromParam` mapping which subject is the parameter type.
  * @see [[net.noresttherein.oldsql.sql.JoinParam.FromParam]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam.WithParam]]
  */ 
sealed trait JoinParam[+F <: FromSome, P[O] <: ParamAt[O]] extends AndFrom[F, P] with AbstractJoinParam[F, P] {
	thisClause =>

	override type GeneralizedLeft[+L <: FromSome] = L JoinParam P
	override type WithLeft[+L <: FromSome] = L JoinParam P
	//overriden to widen the upper bound on left
//	override def withLeft[L <: UngroupedFrom](left :L)(filter :SQLBoolean[left.Generalized JoinParam P]) :L JoinParam P

	override type Generalized = left.Generalized JoinParam P
	override type Self = left.Self JoinParam P
	override type This >: this.type <: F JoinParam P

	override def generalizedExtension[C <: FromSome] :C PrefixOf (C JoinParam P) =
		PrefixOf.itself[C].extend[JoinParam, P]

	override def extension[C <: FromSome] :C PrefixOf (C JoinParam P) =
		PrefixOf.itself[C].extend[JoinParam, P]

	protected override def narrow :left.type JoinParam P



	override type AppendedTo[+S <: UngroupedFrom] = left.AppendedTo[S] JoinParam P

	override def appendedTo[S <: UngroupedFrom](prefix :S) :left.AppendedTo[S] JoinParam P =
		withLeft(left.appendedTo(prefix))(condition)

	override type JoinedWith[+S <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] =
		left.JoinedWith[S, J] JoinParam P

	override def joinedWith[S <: FromSome](prefix :S, firstJoin :TrueJoin.*) :JoinedWith[S, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)



	override type Explicit = left.Explicit JoinParam P
	override type Inner = left.Inner JoinParam P


	/** Substitutes the joined parameter mapping for one labeled with `String` literal `name`. */
	def as[N <: Label](name :N) :F JoinParam (N ?: Param)#T



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinParam.* @unchecked]

}









object JoinParam {

	/** A template `JoinParam` instance with a dummy mapping, for use as a polymorphic factory of `JoinParam` joins. */
	final val template :JoinParam.* = JoinParam(Relation.Dummy, ParamRelation[Unit]())

	/** An alias for `JoinParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */
	type WithParam[+F <: OuterFromSome, X] = JoinParam[F, ParamRelation[X]#Param]

	/** A type alias for `JoinParam` accepting parameter type `X`. As a `FromClause` containing a `JoinParam` join
	  * in its type is a preliminary from clause which will be translated to a parameterized statement, it uses
	  * an a 'inverse function symbol' as a mnemonic: `From[Users] &lt;=? String`. This is equivalent to `WithParam[F, X]`.
	  */
	type <=?[+F <: OuterFromSome, X] = WithParam[F, X]

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



	/** Create an artificial join between the given relation on the left side, and the the `param` special relation 
	  * representing a query parameter of type `X` as the right side. The ''where'' clause can be subsequently specified 
	  * using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[X]]] DSL instead.
	  * @param from  the first relation of the ''from'' clause, using the `FA[O] &lt;: BaseMapping[A, O]` `Mapping` type.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
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
	
	/** Create an artificial join between the `from` clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[X]]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F` [[net.noresttherein.oldsql.sql.JoinParam.WithParam WithParam]] `X`.
	  */
	def apply[F <: OuterFromSome, X](from :F, param :ParamRelation[X],
	                                 filter :SQLBoolean[F#GeneralizedAlias JoinParam ParamRelation[X]#Param] = True)
			:F WithParam X =
		JoinParam[from.type, ParamRelation[X]#Param, X](from, LastRelation(param))(filter)

	/** Create an artificial join between the `from` clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[N, X]]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
	  * @return `F JoinParam (N `[[net.noresttherein.oldsql.sql.JoinParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: OuterFromSome, N <: Label, X](from :F, param :NamedParamRelation[N, X]) :F JoinParam (N ?: X)#T =
		JoinParam[F, (N ?: X)#T, X](from, LastRelation(param))(True)

	/** Create an artificial join between the `from` clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[N, X]]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F JoinParam (N `[[net.noresttherein.oldsql.sql.JoinParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: OuterFromSome, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :SQLBoolean[F#GeneralizedAlias JoinParam NamedParamRelation[N, X]#Param]) :F JoinParam (N ?: X)#T =
		JoinParam[F, (N ?: X)#T, X](from, LastRelation(param))(filter)


	/** Creates [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: OuterFromSome](from :F)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X](from, LastRelation(ParamRelation[X]()))(True)

		def apply[F <: OuterFromSome](from :F, name :String)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X](from, LastRelation(ParamRelation[X](name)))(True)
	}



	private[sql] def apply[L <: FromSome, M[O] <: FromParam[X, O], X]
	                      (clause :L, param :LastRelation[M, X])
	                      (filter :SQLBoolean[clause.Generalized JoinParam M]) :L JoinParam M =
		new JoinParam[clause.type, M] with BaseExtended[clause.type, M, X] {
			override val left = clause
			override val last = param
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type JoinParam M
			override def narrow :left.type JoinParam M = this

			override def withCondition(filter :SQLBoolean[Generalized]) =
				JoinParam[left.type, M, X](left, last)(filter)

			override def withLeft[F <: FromSome]
			                     (left :F)(filter :SQLBoolean[left.Generalized JoinParam M]) :F JoinParam M =
			JoinParam[F, M, X](left, last)(filter)



			override def subselectTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.stretch(target) #:: left.subselectTableStack(target)(stretch.extendFront[left.Generalized, M])



			override def as[N <: Label](name :N) :left.type JoinParam (N ?: X)#T = {
				val replacement = LastRelation[(N ?: X)#T, X](last.mapping.form ?: (name :N))
				val unfiltered = JoinParam[left.type, (N ?: X)#T, X](left, replacement)(True)
				val substitute =
					new ReplaceParam[Generalized, unfiltered.Generalized, FromClause AndFrom (N ?: X)#T, (N ?: X)#T, X, X](
						generalized, unfiltered.generalized)(replacement, Extractor.ident[X]
                    )
				JoinParam[left.type, (N ?: X)#T, X](left, replacement)(substitute(condition))
			}

			override def apply(value :X) :left.This = {
				val substitute = new RemoveParam(generalized, left.generalized, value, 0)
				left where substitute(condition)
			}

		}






	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: OuterFromSome, X](param :F WithParam X) :Option[(F, Relation[ParamRelation[X]#Param])] =
		Some(param.left -> param.right)

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: FromClause, X](from :F Using M forSome { type M[O] <: RefinedMapping[X, O] })
			:Option[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :JoinParam[_, ParamRelation[X]#Param @unchecked] => Some((from.left, param.right))
			case _ => None
		}

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :FromClause)
			:Option[(UngroupedFrom, Relation[M] forSome { type M[A] <: FromParam[_, A] })] =
		from match {
			case param :JoinParam.* @unchecked => Some(param.left -> param.right)
			case _ => None
		}


//for now commented out, would need a custom upper bound on R
//	implicit def joinParamDecomposition[L <: UngroupedFrom, R[O] <: ParamAt[O]]
//			:ExtendedComposition[L JoinParam R, L, R, JoinParam, UngroupedFrom] =
//		decomposition.asInstanceOf[ExtendedComposition[L JoinParam R, L, R, JoinParam, UngroupedFrom]]
//
//	private[this] val decomposition =
//		new ExtendedComposition[UngroupedFrom JoinParam ParamAt, UngroupedFrom, ParamAt, JoinParam, UngroupedFrom]






	protected[sql] trait BaseParam[+F <: FromClause, P[O] <: FromParam[X, O], X] 
		extends BaseExtended[F, P, X] with AbstractJoinParam[F, P]
	{
		
	}
	
	
	
	
	
	
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
	  * the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam]] mapping class, representing a query parameter.
	  */
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
	  * the [[net.noresttherein.oldsql.sql.JoinParam.LabeledFromParam LabeledFromParam]] mapping class, representing
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



	/** Non-parameterized root base type of 'synthetic relation' `Mapping` classes wrapping SQL statement parameters.
	  * @see [[net.noresttherein.oldsql.sql.JoinParam.ParamAt]]
	  * @see [[net.noresttherein.oldsql.sql.JoinParam.FromParam]]
	  */
	sealed trait AnyParam { this :Mapping =>
		val form :SQLForm[Subject]
	}

	/** The generic type supertype of `FromParam` mappings representing SQL statement parameters,
	  * used as the upper bound for the `Mapping`'s used by `JoinParam` to avoid wildcard types of `FromParam[_, O]`,
	  * which break the type compability of the subject (parameter) type.
	  */
	type ParamAt[O] = AnyParam with MappingAt[O]



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
	  * @tparam O a marker type serving as a unique alias for this mapping within a `FromClause`.
	  *///todo: think of another name, this can be confusing
	class FromParam[P, O] private[JoinParam] (val name :String)(implicit sqlForm :SQLForm[P])
		extends ParamMapping[P, P, O] with AnyParam
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
			override def name = This.name
		}



		override def toString :String = name + ":" + form



		def unapply[X](expr :ColumnSQL[_, X]) :Option[ParamColumn[X]] = expr match {
			case ComponentSQL(_, MappingExtract(_, _, col :FromParam[_, _]#ParamColumn[_])) if col.root == this =>
				Some(col.asInstanceOf[ParamColumn[X]])
			case _ =>
				None
		}

		def unapply[X](expr :SQLExpression[_, X]) :Option[ParamMapping[P, X, O]] = expr match {
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
		def ParamForm :Extractor[SQLExpression[_, _], SQLWriteForm[P]] = Extractor.Optional(
			(sql :SQLExpression[_, _]) => unapply(sql).map(_.derivedForm)
		)

	}






	object FromParam {

		def apply[P, N <: String with Singleton](name :N)(implicit form :SQLForm[P]) :FromParam[P, N] =
			new FromParam(name)



		def unapply[F <: FromClause, T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		           (expr :ColumnComponentSQL[F, T, E, M, V, O])
				:Option[(FromParam[E, O], ParamColumnExtract[E, V, O], Int)] =
			if (expr.extract.export.isInstanceOf[ParamMapping[_, _, _]]) {
				val param = expr.extract.export.asInstanceOf[FromParam[E, O]#ParamColumn[V]]
				Some((param.root, param.extract, expr.from.shift))
			} else
				  None

		def unapply[F <: FromClause, T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		           (expr :ComponentSQL[_, T, E, M, V, O]) :Option[(FromParam[E, O], ParamExtract[E, V, O], Int)] =
			if (expr.extract.export.isInstanceOf[ParamMapping[_, _, _]]) {
				val param = expr.extract.export.asInstanceOf[ParamMapping[E, V, O]]
				Some((param.root, param.extract, expr.from.shift))
			} else
				None

		def unapply[X](expr :SQLExpression[_, X])
				:Option[(FromParam[P, O], ParamExtract[P, X, O], Int)] forSome { type P; type O } =
			expr match {
				case ComponentSQL(table, extractor) if extractor.export.isInstanceOf[ParamMapping[_, _, _]] =>
					val param = extractor.export.asInstanceOf[ParamMapping[Any, X, Any]]
					Some((param.root, param.extract, table.shift))
				case _ => None
			}

	}






	class LabeledFromParam[N <: Label, X :SQLForm, O](override val name :N)
		extends FromParam[X, O](name) with LabeledMapping[N, X, O]



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





sealed trait GroupParam[+F <: GroupedFrom, P[O] <: ParamAt[O]]
	extends NonEmptyClause with AbstractJoinParam[F, P] with GroupedFrom
{ thisClause =>

	override type Ungrouped = left.Ungrouped

	override val from :Ungrouped = left.from

	//todo: maybe some common super type with ByAll ?
	override type FromLast = GroupedFrom GroupParam P

	type GeneralizedLeft[+L <: GroupedFrom] = L GroupParam P
	type WithLeft[+L <: GroupedFrom] = L GroupParam P

	def withLeft[L <: GroupedFrom](left :L)(filter :SQLBoolean[left.Generalized GroupParam P]) :L GroupParam P

	override type Generalized = left.Generalized GroupParam P
	override type Self = left.Self GroupParam P
	override type This >: this.type <: F GroupParam P

//	override def generalizedExtension[C <: FromSome] :C PrefixOf (C GroupParam P) =
//		PrefixOf.itself[C].extend[GroupParam, P]
//
//	override def extension[C <: FromSome] :C PrefixOf (C GroupParam P) =
//		PrefixOf.itself[C].extend[GroupParam, P]

	protected override def narrow :left.type GroupParam P


	override type JoinedWith[+S <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] =
		left.JoinedWith[S, J] GroupParam P

	override def joinedWith[S <: FromSome](prefix :S, firstJoin :TrueJoin.*) :JoinedWith[S, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)



	override type Explicit = left.Explicit GroupParam P
	override type Inner = left.Inner GroupParam P
	//fixme;
	override val outer :Nothing = ???



	override type FromSubselect[+E <: FromClause] = Nothing //todo:

	override type SubselectTable[T[O] <: MappingAt[O]] = Nothing //todo:

	override def from[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (first :Relation[R])
	                 (implicit infer :Conforms[Relation[R], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:SubselectTable[T] =
	???

	override def from[S <: OuterFromSome](subselect :S) :FromSubselect[S] = ???

	override def fromSubselect[S <: FromSome](subselect :S)(implicit extension :subselect.Implicit ExtendedBy Generalized)
			:FromSubselect[S] = ???

	/** Substitutes the joined parameter mapping for one labeled with `String` literal `name`. */
	def as[N <: Label](name :N) :F GroupParam (N ?: Param)#T


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupParam.* @unchecked]

}





object GroupParam {

	/** A template `GroupParam` instance with a dummy mapping, for use as a polymorphic factory of `OuterJoin` joins. */
	final val template :GroupParam.* = GroupParam(GroupByAll(From(Relation.Dummy), Relation.Dummy), ParamRelation[Unit]())

	/** An alias for `GroupParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */
	type ByParam[+F <: GroupedFrom, X] = GroupParam[F, ParamRelation[X]#Param]


	
	/** Create an artificial join between the `from` clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[X]]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F` [[net.noresttherein.oldsql.sql.GroupParam.ByParam ByParam]] `X`.
	  */
	def apply[F <: GroupedFrom with OuterClause, X]
	         (from :F, param :ParamRelation[X], filter :SQLBoolean[F#Generalized GroupParam ParamRelation[X]#Param] = True)
			:F ByParam X =
		GroupParam[from.type, ParamRelation[X]#Param, X](from, RelationSQL(param, 0))(filter)

	/** Create an artificial join between the `from` clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[N, X]]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
	  * @return `F GroupParam (N `[[net.noresttherein.oldsql.sql.JoinParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: GroupedFrom with OuterClause, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X]) :F GroupParam (N ?: X)#T =
		GroupParam[F, (N ?: X)#T, X](from, RelationSQL(param, 0))(True)

	/** Create an artificial join between the `from` clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[N, X]]] DSL instead.
	  * @param from  a ''from'' clause containing the non-empty list of relations preceding `param`.
	  *              It must be an independent, 'outer' clause, that is contain no `Subselect` joins.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F GroupParam (N `[[net.noresttherein.oldsql.sql.JoinParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: GroupedFrom with OuterClause, N <: Label, X]
	         (from :F, param :NamedParamRelation[N, X],
	          filter :SQLBoolean[F#Generalized GroupParam NamedParamRelation[N, X]#Param]) :F GroupParam (N ?: X)#T =
		GroupParam[F, (N ?: X)#T, X](from, RelationSQL(param, 0))(filter)


	/** Creates [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: GroupedFrom with OuterClause](from :F)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X](from, RelationSQL(ParamRelation[X](), 0))(True)

		def apply[F <: GroupedFrom with OuterClause](from :F, name :String)(implicit form :SQLForm[X]) :F ByParam X =
			GroupParam[F, ParamRelation[X]#Param, X](from, RelationSQL(ParamRelation[X](name), 0))(True)
	}



	private[sql] def apply[L <: GroupedFrom, M[O] <: FromParam[X, O], X]
	                      (clause :L, param :RelationSQL[GroupedFrom GroupParam M, M, X, GroupedFrom GroupParam M])
	                      (filter :SQLBoolean[clause.Generalized GroupParam M]) :L GroupParam M =
		new GroupParam[clause.type, M] with BaseExtended[clause.type, M, X] {
			override val left = clause
			override val last = param
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type GroupParam M
			override def narrow :left.type GroupParam M = this

			override def withCondition(filter :SQLBoolean[Generalized]) =
				GroupParam[left.type, M, X](left, last)(filter)

			override def withLeft[F <: GroupedFrom]
			                     (left :F)(filter :SQLBoolean[left.Generalized GroupParam M]) :F GroupParam M =
				GroupParam[F, M, X](left, last)(filter)



			override def subselectTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.stretch(target) #:: left.subselectTableStack(target)(stretch.extendFront[left.Generalized, M])



			override def as[N <: Label](name :N) :left.type GroupParam (N ?: X)#T = {
				type Last = GroupedFrom GroupParam (N ?: X)#T
				val replacement = RelationSQL[Last, (N ?: X)#T, X, Last](last.mapping.form ?: (name :N), 0)
				val unfiltered = GroupParam[left.type, (N ?: X)#T, X](left, replacement)(True)
				val substitute =
					new ReplaceParam[Generalized, unfiltered.Generalized, GroupedFrom GroupParam (N ?: X)#T, (N ?: X)#T, X, X](
						generalized, unfiltered.generalized)(replacement, Extractor.ident[X]
					)
				GroupParam[left.type, (N ?: X)#T, X](left, replacement)(substitute(condition))
			}

			override def apply(value :X) :left.This = {
				val substitute = new RemoveParam(generalized, left.generalized, value, 0)
				left where substitute(condition)
			}

		}


	
	/** Type alias for `GroupParam` with erased type parameters, covering all instances of `GroupParam`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = GroupParam[_ <: GroupedFrom, M] forSome { type M[O] <: FromParam[_, O] }

	/** A curried type constructor for `GroupParam` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: GroupedFrom with OuterClause] = {
		type F[R[O] <: ParamAt[O]] = L GroupParam R
		type P[X] = L ByParam X
	}

	/** A curried type constructor for `GroupParam` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: GroupedFrom with OuterClause] = L GroupParam R }
	
}

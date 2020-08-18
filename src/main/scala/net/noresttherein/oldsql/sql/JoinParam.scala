package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnForm, ColumnMapping, GenericMappingExtract, Mapping, MappingExtract, Relation, SQLForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.NamedRelation
import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome, OuterFrom}
import net.noresttherein.oldsql.sql.MappingSQL.{ColumnComponentSQL, ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.JoinParam.{?:, ParamAt}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLScribe.{RemoveParam, ReplaceParam}
import net.noresttherein.oldsql.sql.AndFrom.BaseAndFrom






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
sealed trait JoinParam[+F <: FromClause, P[O] <: ParamAt[O]] extends AndFrom[F, P] { thisClause =>

	/** The type of this parameter, that is the subject type of the joined mapping. */
	type Param = P[FromLast]#Subject

	override type GeneralizedLeft[+L <: FromClause] = L JoinParam P //widened upper bound
	override type WithLeft[+L <: FromClause] = L JoinParam P //widened upper bound
	//overriden to widen the upper bound on left
	override def withLeft[L <: FromClause](left :L)(filter :SQLBoolean[left.Generalized JoinParam P]) :L JoinParam P

	override type Generalized = left.Generalized JoinParam P
	override type Self = left.Self JoinParam P
	override type This >: this.type <: F JoinParam P



	/** Apply a join condition to the previous relation and the parameter expression of this clause.
	  * The condition is combined using `&&` with `this.condition` and becomes a part of `this.filter` representing
	  * the ''where'' clause of the SQL statement. This works exactly like 'where', but instead of a single argument
	  * representing all joined relations, the filter function should take as its arguments the last two relations,
	  * i.e, the last relation defined by the left side of this join, if any, and the right side of this join.
	  * Static type checking enforces that this method can't be called on 'joins' where the left side is empty
	  * (single table sources).
	  * @param condition a function accepting the expressions for the last two relations in this clause and creating
	  *                  an SQL expression for the join condition.
	  * @return a `AndFrom` instance of the same kind as this one, with the same left and right sides,
	  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
	  *         returned by the passed filter function.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#JoinFilter]]
	  */
	def on(condition :(left.LastTable[left.FromLast JoinParam P], LastTable[FromLast])
	                  => SQLBoolean[left.FromLast JoinParam P]) :This =
	{
		val joinFilter = condition(left.lastAsIn[left.FromLast JoinParam P], last)
		val grounded = SQLScribe.groundFreeComponents(generalized, joinFilter)
		where(grounded)
	}



	override type AppendedTo[+S <: FromClause] = left.AppendedTo[S] JoinParam P

	override def appendedTo[S <: FromClause](prefix :S) :left.AppendedTo[S] JoinParam P =
		withLeft(left.appendedTo(prefix))(condition)

	override type JoinedWith[+S <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] =
		left.JoinedWith[S, J] JoinParam P

	override def joinedWith[S <: FromSome](prefix :S, firstJoin :TrueJoin.*) :JoinedWith[S, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override def joinedAsSubselect[S <: FromClause](prefix :S) :Nothing =
		throw new UnsupportedOperationException(
			"JoinParam.joinedAsSubselect: join parameters cannot appear as a part of a subselect clause. " +
			s"$this joinedAsSubselect $prefix"
		)



	override type Explicit = left.Explicit JoinParam P
	override type Inner = left.Inner JoinParam P
	override type Implicit = Nothing
	override type Outer = Nothing

	override def outer = throw new UnsupportedOperationException(s"JoinParam.outer on $this.")



	override def subselectSize = 0

	override type SubselectRow = left.SubselectRow ~ Param

	override def subselectRow[E <: FromSome]
	                         (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow] =
		left.subselectRow(target)(extension.stretchFront[left.Generalized, P]) ~ last.stretch(target)(extension)



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


	/** Substitutes the joined parameter mapping for one labeled with `String` literal `name`. */
	def as[N <: Label](name :N) :F JoinParam (N ?: Param)#T


	override def joinName = "param"

}









object JoinParam {

	/** A template `OuterJoin` instance with a dummy mapping, for use as a polymorphic factory of `OuterJoin` joins. */
	final val template :JoinParam.* = JoinParam(Dual, ParamRelation[Unit]())

	/** An alias for `JoinParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */
	type WithParam[+F <: OuterFrom, X] = JoinParam[F, ParamRelation[X]#Param]

	/** A type alias for `JoinParam` accepting parameter type `X`. As a `FromClause` containing a `JoinParam` join
	  * in its type is a preliminary from clause which will be translated to a parameterized statement, it uses
	  * an a 'inverse function operator' as a mnemonic: `From[Users] &lt;=? String`. This is equivalent to `WithParam[F, X]`.
	  */
	type <=?[+F <: OuterFrom, X] = WithParam[F, X]

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



	/** Create an artificial join between the `from` clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.JoinParam#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[X]]] DSL instead.
	  * @param from  a ''from'' clause containing the list of relations preceding `param`.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F` [[net.noresttherein.oldsql.sql.JoinParam.WithParam WithParam]] `X`.
	  */
	def apply[F <: OuterFrom, X](from :F, param :ParamRelation[X],
	                             filter :SQLBoolean[F#Generalized JoinParam ParamRelation[X]#Param] = True)
			:F WithParam X =
		JoinParam[F, ParamRelation[X]#Param, X](from, LastRelation(param))(filter)

	/** Create an artificial join between the `from` clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.JoinParam#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[N, X]]] DSL instead.
	  * @param from  a ''from'' clause containing the list of relations preceding `param`.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
	  * @return `F JoinParam (N `[[net.noresttherein.oldsql.sql.JoinParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: OuterFrom, N <: Label, X](from :F, param :NamedParamRelation[N, X]) :F JoinParam (N ?: X)#T =
		JoinParam[F, (N ?: X)#T, X](from, LastRelation(param))(True)

	/** Create an artificial join between the `from` clause/list of relations (possibly empty) as the left side,
	  * and the the `param` special relation representing a query parameter of type `X` as the right side.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.JoinParam#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `from` [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param[N, X]]] DSL instead.
	  * @param from  a ''from'' clause containing the list of relations preceding `param`.
	  * @param param the last relation of the created ''from'' clause,
	  *              using the [[net.noresttherein.oldsql.sql.JoinParam.FromParam FromParam[X, _]]] `Mapping` type.
	  * @param filter an optional join condition filtering the clause based on the value of `X`.
	  * @return `F JoinParam (N `[[net.noresttherein.oldsql.sql.JoinParam.?: ?:]]` X)#T`.
	  */
	def apply[F <: OuterFrom, N <: Label, X](from :F, param :NamedParamRelation[N, X],
	                                         filter :SQLBoolean[F#Generalized JoinParam NamedParamRelation[N, X]#Param])
			:F JoinParam (N ?: X)#T =
		JoinParam[F, (N ?: X)#T, X](from, LastRelation(param))(filter)


	/** Creates [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instances for parameters of type `X`. Separates
	  * the constructor into two chained factory methods to allow automatic inference of types of the subsequently
	  * given arguments, as the type `X` will almost always be needed to be specified explicitly.
	  */
	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: OuterFrom](from :F)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X](from, LastRelation(ParamRelation[X]()))(True)

		def apply[F <: OuterFrom](from :F, name :String)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamRelation[X]#Param, X](from, LastRelation(ParamRelation[X](name)))(True)
	}



	private[sql] def apply[L <: FromClause, M[O] <: FromParam[X, O], X]
	                      (clause :L, param :LastRelation[M, X])
	                      (filter :SQLBoolean[clause.Generalized JoinParam M]) :L JoinParam M =
		new JoinParam[clause.type, M] with BaseAndFrom[clause.type, M, X] {
			override val left = clause
			override val last = param
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type JoinParam M
			override def narrow :left.type JoinParam M = this

			override def withCondition(filter :SQLBoolean[Generalized]) =
				JoinParam[left.type, M, X](left, last)(filter)

			override def withLeft[F <: FromClause]
			                     (left :F)(filter :SQLBoolean[left.Generalized JoinParam M]) :F JoinParam M =
			JoinParam[F, M, X](left, last)(filter)



			override def subselectTableStack[E <: FromSome]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.stretch(target) #:: left.subselectTableStack(target)(stretch.stretchFront[left.Generalized, M])



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
	def unapply[F <: OuterFrom, X](param :F WithParam X) :Option[(F, Relation[ParamRelation[X]#Param])] =
		Some(param.left -> param.right)

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply[F <: FromClause, X](from :F AndFrom M forSome { type M[O] <: RefinedMapping[X, O] })
			:Option[(F, Relation[ParamRelation[X]#Param])] =
		from match {
			case param :JoinParam[_, ParamRelation[X]#Param @unchecked] => Some((from.left, param.right))
			case _ => None
		}

	/** Matches all `JoinParam` instances, splitting them into their clause (left side) and the artificial relation
	  * for their parameter (right side).
	  */
	def unapply(from :FromClause)
			:Option[(FromClause, Relation[M] forSome { type M[A] <: FromParam[_, A] })] =
		from match {
			case param :JoinParam.* @unchecked => Some(param.left -> param.right)
			case _ => None
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

	sealed abstract class ParamMapping[P, S, O] protected(implicit sqlForm :SQLForm[S]) extends FormMapping[S, O] {
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
	  */
	class FromParam[P, O] private[JoinParam](val name :String)(implicit sqlForm :SQLForm[P])
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



		def unapply[F <: FromClause, X, T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		           (expr :ColumnComponentSQL[F, T, E, M, V, O])
				:Option[(FromParam[E, O], ParamColumnExtract[E, V, O], Int)] =
			if (expr.extract.export.isInstanceOf[ParamMapping[_, _, _]]) {
				val param = expr.extract.export.asInstanceOf[FromParam[E, O]#ParamColumn[V]]
				Some((param.root, param.extract, expr.from.shift))
			} else
				  None

		def unapply[F <: FromClause, X, T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
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
	type * = JoinParam[_ <: FromClause, M] forSome { type M[O] <: FromParam[_, O] }

	/** A curried type constructor for `JoinParam` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: OuterFrom] = {
		type F[R[O] <: ParamAt[O]] = L JoinParam R
		type P[X] = L WithParam X
	}

	/** A curried type constructor for `JoinParam` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: ParamAt[O]] = { type F[L <: FromClause] = L JoinParam R }

}



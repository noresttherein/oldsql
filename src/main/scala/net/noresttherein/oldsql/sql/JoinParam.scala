package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, GenericMappingExtract, Mapping, MappingExtract, RowSource, SQLForm, SQLWriteForm, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.RowSource.NamedSource
import net.noresttherein.oldsql.schema.bits.FormMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, OuterFrom}
import net.noresttherein.oldsql.sql.MappingSQL.{ColumnComponentSQL, ComponentSQL, JoinedRelation, SQLRelation}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.JoinParam.{?:, ParamAt}
import net.noresttherein.oldsql.sql.MappingSQL.SQLRelation.LastRelation
import net.noresttherein.oldsql.sql.SQLScribe.{RemoveParam, ReplaceParam}
import net.noresttherein.oldsql.sql.With.TypedWith








/** A specialized join class which joins the source on its left side with a synthetic mapping
  * `M[O] &lt;: FromParam[X, O]`, representing a query parameter `X`. It allows to filter a given from clause
  * using values unknown at this time, which can be obtained by applying an arbitrary scala function to `X`.
  * The mapping, aside from representing the parameter value itself, can also be used to create additional
  * subcomponents with values derived from the parameter value, which can be used in an `SQLExpression` as any other
  * component. The mappings themselves are however only shills, replaced when creating the select statement,
  * and only their associated `SQLForm`s are being used in the mapping process.
  * The type constructor for a parameter mapping with type `X` is `ParamSource[X]#Row` and
  * `NamedParamSource[N, X]#Row` for a mapping labeled with a string literal `N &lt;: String with Singleton`.
  * This join is typically written in an abbreviated form `FromClause WithParam X` (or `FromClause <=? X`)
  * and `FromClause JoinParam ("name" ?: X)#T` for a parameter named with a string literal.
  * This class declares its `Implicit` clause as `Nothing` (rather than the `Implicit` of the left side) to ensure that
  * neither it nor any extension clause containing it does not conform to `FromClause.SubselectOf[F#Implicit]`,
  * thus preventing it from being used as a part of a subselect of the outer clause, which would hide the existence
  * of a parameter and make its outer source appear not parameterized. As the result, only top-level select statement
  * from clauses can include unbound parameters.
  *
  * @tparam F the actual from clause of the parameterized select statement.
  * @tparam M synthetic `FromParam` mapping which subject is the parameter type.
  * @see [[net.noresttherein.oldsql.sql.JoinParam.FromParam]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam.WithParam]]
  */
sealed trait JoinParam[+F <: FromClause, M[O] <: ParamAt[O]] extends With[F, M] { thisClause =>

	/** The type of this parameter, that is the subject type of the joined mapping. */
	type Param = M[FromLast]#Subject

	override def subselectSize = 0

	@inline final def from :F = left

	override type This >: this.type <: F JoinParam M

	override type WithLeft[+L <: FromClause] = L JoinParam M

	override type Self = left.Self JoinParam M


	override type JoinedWith[+P <: FromClause, +J[+L <: FromClause, R[O] <: MappingAt[O]] <: L Join R] =
		left.JoinedWith[P, J] JoinParam M

	override def joinedWith[P <: FromClause](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)



	override type Implicit = Nothing

	override def outer = throw new UnsupportedOperationException(s"JoinParam[$this].outer")

	override type Explicit = left.Explicit With M

	override type Outer = Nothing

	override type Inner = left.Inner JoinParam M

	override type AsSubselectOf[O <: FromClause] = Nothing

	override def asSubselectOf[O <: FromClause](outer :O)(implicit extension :Implicit ExtendedBy O) :Nothing =
		throw new UnsupportedOperationException(
			s"JoinParam[$this].asSubselectOf: join parameters can't appear as a part of a subselect from clause."
		)



	override type SubselectRow = left.SubselectRow ~ Param

	override def subselectRow[E <: FromClause]
	                         (target :E)(implicit stretch :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow] =
		left.subselectRow(target)(stretch.stretchFront[left.Generalized, M]) ~ last.stretch(target)(stretch)



	type Params = left.Params ~ Param



	/** Substitutes the joined parameter mapping for one labeled with `String` literal `name`. */
	def as[N <: Label](name :N) :F JoinParam (N ?: Param)#T

	/** Provides the value for the joined parameter, removing it from this clause and replacing all references to it
	  * with bound parameters in the form of `SQLParameter` instances.
	  */
	def apply(value :Param) :F#This



	protected override def joinType = "param"

}









object JoinParam {

	/** A template `OuterJoin` instance with a dummy mapping, for use as a polymorphic factory of `OuterJoin` joins. */
	final val template :JoinParam.* = JoinParam(Dual, ParamSource[Unit]())

	/** An alias for `JoinParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */
	type WithParam[+F <: OuterFrom, X] = JoinParam[F, ParamSource[X]#Row]

	/** A type alias for `JoinParam` accepting parameter type `X`. As a `FromClause` containing a `JoinParam` join
	  * in its type is a preliminary from clause which will be translated to a parameterized statement, it uses
	  * an a 'inverse function operator' as a mnemonic: `From[Users] &lt;=? String`. This is equivalent to `WithParam[F, X]`.
	  */
	type <=?[+F <: OuterFrom, X] = WithParam[F, X]

	/** A type wrapper for the type constructor of parameter mappings with string literals as names, present in their
	  * type signature.
	  */
	type ?:[N <: Label, X] = { type T[O] = LabeledFromParam[N, X, O] }

	implicit def ?:[N <: Label](name :N) = new method_?:[N](name)

	class method_?:[N <: Label](private val name :N) extends AnyVal {
		/** Creates a synthetic `RowSource` of the given name, creating named parameter mappings. */
		def ?:[X :SQLForm] :NamedParamSource[N, X] = new NamedParamSource[N, X](name)
	}



	def apply[F <: OuterFrom, X](from :F, source :ParamSource[X]) :F WithParam X =
		JoinParam[F, ParamSource[X]#Row, X](from, LastRelation(source))(True)

	def apply[F <: OuterFrom, N <: Label, X](from :F, source :NamedParamSource[N, X]) :F JoinParam (N ?: X)#T =
		JoinParam[F, (N ?: X)#T, X](from, LastRelation(source))(True)


	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: OuterFrom](from :F)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamSource[X]#Row, X](from, LastRelation(ParamSource[X]()))(True)

		def apply[F <: OuterFrom](from :F, name :String)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam[F, ParamSource[X]#Row, X](from, LastRelation(ParamSource[X](name)))(True)
	}



	private[sql] def apply[L <: FromClause, M[O] <: FromParam[X, O], X]
	                      (from :L, param :LastRelation[M, X])(
	                       filter :SQLBoolean[from.Generalized With M]) :L JoinParam M =
		new JoinParam[from.type, M] with TypedWith[from.type, M, X] {
			override val left = from
			override val last = param
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type JoinParam M
			override def narrow :left.type JoinParam M = this

			override def withLeft[F <: FromClause]
			                     (left :F)(filter :SQLBoolean[left.Generalized With M]) :F JoinParam M =
				JoinParam[F, M, X](left, last)(filter)

			override def withCondition(filter :SQLBoolean[left.Generalized With M]) :This =
				JoinParam[left.type, M, X](left, last)(condition && filter)


			override def subselectTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[SQLRelation.AnyIn[E]] =
				last.stretch(target) #:: left.subselectTableStack(target)(stretch.stretchFront[left.Generalized, M])



			override def as[N <: Label](name :N) :left.type JoinParam (N ?: X)#T = {
				val replacement = LastRelation[(N ?: X)#T, X](last.mapping.form ?: (name :N))
				val unfiltered = JoinParam[left.type, (N ?: X)#T, X](left, replacement)(True)
				val substitute =
					new ReplaceParam[Generalized, unfiltered.Generalized, FromClause With (N ?: X)#T, (N ?: X)#T, X, X](
						generalized, unfiltered.generalized)(replacement, Extractor.ident[X]
                    )
				JoinParam[left.type, (N ?: X)#T, X](left, replacement)(substitute(condition))
			}

			override def apply(value :X) :left.This = {
				val substitute = new RemoveParam(generalized, left.generalized, value, 0)
				left.where(_ => substitute(condition))
			}

		}






	def unapply[F <: OuterFrom, X](param :F WithParam X)
			:Option[(F, JoinedRelation[FromClause With M, M] forSome { type M[A] <: FromParam[X, A] })] =
		Some(param.left -> param.last)

	def unapply(from :FromClause)
			:Option[(FromClause, JoinedRelation[FromClause With M, M] forSome { type M[A] <: FromParam[_, A] })] =
		from match {
			case param :JoinParam.* @unchecked => Some(param.left -> param.last)
			case _ => None
		}






	abstract class GenericParamSource[X, M[O] <: FromParam[X, O]](val name :String)(implicit val form :SQLForm[X])
		extends RowSource[M]
	{

		override def sql :String = name + "?:" + form


		def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamSource.*]

		override def equals(that :Any) :Boolean = that match {
			case other :ParamSource.* =>
				(other eq this) || other.canEqual(this) && other.name == name && other.form == form
			case _ => false
		}

		override def hashCode :Int = name.hashCode * 31 + form.hashCode
	}



	class ParamSource[X :SQLForm](name :String)
		extends GenericParamSource[X, ({ type T[O] = FromParam[X, O] })#T](name)
	{
		private[this] val param = new FromParam[X, Any](name)

		override def apply[O] :FromParam[X, O] = param.asInstanceOf[FromParam[X, O]]
	}



	object ParamSource {
		def apply[X :SQLForm](name :String) :ParamSource[X] = new ParamSource[X](name)

		def apply[X :SQLForm]() :ParamSource[X] = new ParamSource("?")

		def unapply(source :RowSource.*) :Option[(SQLForm[_], String)] = source match {
			case param :GenericParamSource[_, _] => Some(param.form -> param.name)
			case _ => None
		}

		type * = GenericParamSource[_, M] forSome { type M[O] <: FromParam[_, O] }
	}



	class NamedParamSource[N <: Label, X :SQLForm](override val name :N)
		extends GenericParamSource[X, (N ?: X)#T](name) with NamedSource[N, (N ?: X)#T]
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

	type ParamAt[O] = AnyParam with MappingAt[O]



	/** A `Mapping` type representing a query parameter the value of which is not known. While the `SQLParameter`
	  * expression can be used to represent a statement parameter, its value must be known when the expression is created.
	  * By representing a statement parameter as a mapping that can be used in the same way as table mappings
	  * in `FromClause` table lists, we can represent any value obtainable from `P`
	  * by a function `P => T` as a component `(P #? _)#Component[T]` wrapping that function, which can be used
	  * to create component formulas for that function. In particular, a `TableFormula[S, FromParam[P]]`
	  * is a expression which value will be substituted by a statement parameter `P`.
	  * @param name a suggested name of the parameter for debugging purposes.
	  * @tparam P the parameter type needed to prepare statements using this mapping in their sources.
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



		def toColumn(implicit form :ColumnForm[P]) :Column[P] = new ParamColumn[P](Extractor.ident[P])



		class ParamComponent[T :SQLForm] private[FromParam] (pick :P =?> T)
			extends ParamMapping[P, T, O]
		{
			override def root :FromParam[P, O] = This
			override def extract :ParamExtract[P, T, O] = GenericMappingExtract(this)(pick)
			override def toString = s"$This[$form]"
		}



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




		/** An extractor matching ComponentFormulas for components of this mapping, that is actual sql statement parameters. */
		def ParamForm :Extractor[SQLExpression[_, _], SQLWriteForm[P]] = Extractor.Optional(
			(sql :SQLExpression[_, _]) => unapply(sql).map(_.derivedForm)
		)

	}






	object FromParam {

		def apply[P, N <: String with Singleton](name :N)(implicit form :SQLForm[P]) :FromParam[P, N] =
			new FromParam(name)



		def unapply[F <: FromClause, X, T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		           (expr :ColumnComponentSQL[F, T, E, M, V, O])
				:Option[(FromParam[E, O], ParamColumnExtract[E, V, O], Int)] =
			if (expr.extract.export.isInstanceOf[ParamMapping[_, _, _]]) {
				val param = expr.extract.export.asInstanceOf[FromParam[E, O]#ParamColumn[V]]
				Some((param.root, param.extract, expr.from.shift))
			} else
				  None

		def unapply[F <: FromClause, X, T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O >: F <: FromClause]
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




	type * = JoinParam[_ <: FromClause, M] forSome { type M[O] <: FromParam[_, O] }



}



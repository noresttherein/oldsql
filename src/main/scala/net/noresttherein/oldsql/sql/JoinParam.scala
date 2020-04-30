package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.support.FormMapping
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, MappingExtract, RowSource, SQLForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, OriginProjection}
import net.noresttherein.oldsql.schema.RowSource.{AnyRowSource, NamedSource}
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.MappingExtract.ColumnMappingExtract
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.MappingFormula.ComponentFormula
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.{AnyRelationIn, LastRelation}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple
import net.noresttherein.oldsql.sql.JoinParam.ParamFrom
import net.noresttherein.oldsql.sql.JoinParam.ParamSource.AnyParamSource








/** A specialized join class which joins the source on its left side with a synthetic mapping
  * `M[O] &lt;: FromParam[X, O]`, representing a query parameter `X`. It allows to filter a the given from clause
  * using values unknown at this time, which can be obtained by applying an arbitrary scala function to `X`.
  * The mapping, aside from representing the parameter value itself, can also be used to create additional
  * subcomponents with values derived from the parameter value, which can be used in an `SQLFormula` as any other
  * component. The type constructor for a parameter mapping with type `X` is `ParamSource[X]#Row` and
  * `NamedParamSource[N, X]#Row` for a mapping labeled with a string literal `N &lt;: String with Singleton`.
  * This join is typically written in an abbreviated form `FromClause WithParam X` (or `FromClause <=? X`)
  * and `FromClause JoinParam ("name" ?: X)#T` for the parameter named with a string literal.
  * This class declares its `Outer` clause as `Nothing` (rather than the `Outer` of the left side) to ensure
  * neither it nor any its extension does not conform to `FromClause.SubselectFrom[F#Outer]`, thus preventing
  * it from being used as a part of a subselect of the outer clause, which would hide the existence of a parameter
  * and make its outer source appear not parameterized.
  *
  * @tparam F the actual from clause of the parameterized select statement.
  * @tparam M synthetic `FromParam` mapping which subject is the parameter type.
  * @see [[net.noresttherein.oldsql.sql.JoinParam.FromParam]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam.WithParam]]
  */
sealed trait JoinParam[+F <: FromClause, M[O] <: ParamFrom[O]] extends With[F, M] {
	type Param = M[FromLast]#Subject

	@inline final def from :F = left

	override type This >: this.type <: F JoinParam M
	override type JoinRight[+L <: FromClause] <: L JoinParam M

	override type Outer = Nothing


	override def outer = throw new UnsupportedOperationException(s"JoinParam[$this].outer")



	override type SubselectRow = left.SubselectRow ~ Param

	override def subselectRow[E <: FromClause](stretch :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow] =
		left.subselectRow(stretch.stretchFront[left.Generalized, M]) ~ table.upcast.stretch(stretch)

	override def subselectTableStack[E <: FromClause](stretch :Generalized ExtendedBy E) :LazyList[AnyRelationIn[E]] =
		table.extend(stretch) #:: left.subselectTableStack(stretch.stretchFront[left.Generalized, M])


	protected override def joinType = "param"
}









object JoinParam {

	/** An alias for `JoinParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */
	type WithParam[+F <: FromClause, X] = JoinParam[F, ParamSource[X]#Row]
	type <=?[+F <: FromClause, X] = WithParam[F, X]

	/** A type wrapper for the type constructor of parameter mappings with string literals as names, present in their
	  * type signature.
	  */
	type ?:[N <: Label, X] = { type T[O] = LabeledFromParam[N, X, O] }

	implicit def ?:[N <: Label](name :N) = new method_?:[N](name)

	class method_?:[N <: Label](private val name :N) extends AnyVal {
		/** Creates a synthetic `RowSource` of the given name, creating named parameter mappings. */
		def ?:[X :SQLForm] :NamedParamSource[N, X] = new NamedParamSource[N, X](name)
	}



	def apply[F <: FromClause, X](from :F, source :ParamSource[X]) :F WithParam X =
		JoinParam[F, ParamSource[X]#Row](from, LastRelation(source), True())

	def apply[F <: FromClause, N <: Label, X](from :F, source :NamedParamSource[N, X]) :F JoinParam (N ?: X)#T =
		JoinParam[F, (N ?: X)#T](from, LastRelation(source), True())

	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: FromClause](from :F)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam(from, LastRelation(ParamSource[X]()), True())

		def apply[F <: FromClause](from :F, name :String)(implicit form :SQLForm[X]) :F WithParam X =
			JoinParam(from, LastRelation(ParamSource[X](name)), True())

		def apply[T[O] <: MappingFrom[O]](from :RowSource[T])(implicit form :SQLForm[X]) :From[T] WithParam X =
			JoinParam(From(from), LastRelation(ParamSource[X]()), True())

		def apply[T[O] <: MappingFrom[O]](from :RowSource[T], name :String)(implicit form :SQLForm[X]) :From[T] WithParam X =
			JoinParam(From(from), LastRelation(ParamSource[X](name)), True())
	}



	private[sql] def apply[L <: FromClause, M[O] <: ParamFrom[O]]
	                      (from :L, param :LastRelation[M],
	                       filter :BooleanFormula[L With M]) :L JoinParam M =
		new JoinParam[from.type, M] {
			override val left = from
			override val table = param
			override val joinCondition = filter

			override type This = left.type JoinParam M
			override type JoinRight[+C <: FromClause] = C JoinParam M
			override def self :left.type JoinParam M = this

			override def copy[F <: FromClause](left :F, filter :BooleanFormula[F With M]) :F JoinParam M =
				JoinParam[F, M](left, table, filter)

			override def copy(filter :BooleanFormula[left.type With M]) :This =
				JoinParam[left.type, M](left, table, filter)

		}






	def unapply[F <: FromClause, X](param :F WithParam X) :Option[(F, ParamSource[X])] =
		Some(param.left -> param.right.asInstanceOf[ParamSource[X]])

	def unapply(from :FromClause) :Option[(FromClause, ParamSource[_])] = from match {
		case param :AnyParamJoin => Some(param.left -> param.right.asInstanceOf[ParamSource[_]])
		case _ => None
	}






	abstract class GenericParamSource[X, M[O] <: FromParam[X, O]](val name :String)(implicit val form :SQLForm[X])
		extends RowSource[M]
	{
//		override type Row[O] = FromParam[X, O]

		override def sql :String = name + "?:" + form


		def canEqual(that :Any) :Boolean = that.isInstanceOf[AnyParamSource]

		override def equals(that :Any) :Boolean = that match {
			case other :AnyParamSource =>
				(other eq this) || other.canEqual(this) && other.name == name && other.form == form
			case _ => false
		}

		override def hashCode :Int = name.hashCode * 31 + form.hashCode
	}



	class ParamSource[X :SQLForm](param :String)
		extends GenericParamSource[X, ({ type T[O] = FromParam[X, O] })#T](param)
	{
		override def apply[O] :FromParam[X, O] = new FromParam[X, O](name)
	}



	object ParamSource {
		def apply[X :SQLForm](name :String) :ParamSource[X] = new ParamSource[X](name)

		def apply[X :SQLForm]() :ParamSource[X] = new ParamSource("?")

		def unapply(source :AnyRowSource) :Option[(SQLForm[_], String)] = source match {
			case param :ParamSource[_] => Some(param.form -> param.name)
			case _ => None
		}

		type AnyParamSource = GenericParamSource[_, M] forSome { type M[O] <: FromParam[_, O] }
	}



	class NamedParamSource[N <: Label, X :SQLForm](override val name :N)
		extends GenericParamSource[X, (N ?: X)#T](name) with NamedSource[N, (N ?: X)#T]
	{
		override def apply[O] :LabeledFromParam[N, X, O] = new LabeledFromParam[N, X, O](name)
	}






	sealed abstract class ParamMapping[P, X, O] protected(implicit sqlForm :SQLForm[X]) extends FormMapping[X, O] {
		def root :FromParam[P, O]
		def extractor :MappingExtract[P, X, O]
		def derivedForm :SQLWriteForm[P] = form compose extractor
	}



	type ParamFrom[O] = AnyParam with MappingFrom[O]

	sealed trait AnyParam



	/** A `Mapping` instance representing a query parameter the value of which is not known. While the `BoundParameter`
	  * formula can be used to represent a statement parameter, its value must be known when the formula is created.
	  * By representing a statement parameter as a mapping that can be used in the same way as table mappings
	  * in `FromClause` table lists, we can represent any value obtainable from `P`
	  * by a function `P => T` as a component `(P #? _)#Component[T]` wrapping that function, which can be used
	  * to create component formulas for that function. In particular, a `TableFormula[S, FromParam[P]]`
	  * is a formula which value will be substituted by a statement parameter `P`.
	  * @param name a suggested name of the parameter for debugging purposes.
	  * @tparam P the parameter type needed to prepare statements using this mapping in their sources.
	  * @tparam O a marker type serving as a unique alias for this mapping within a `FromClause`.
	  */
	class FromParam[P, O] private[JoinParam](val name :String)(implicit sqlForm :SQLForm[P])
		extends ParamMapping[P, P, O] with AnyParam
	{ This =>

		def this()(implicit form :SQLForm[P]) = this("?")

		override def root :FromParam[P, O] = this
		override def extractor :MappingExtract[P, P, O] = MappingExtract.ident(this)
		override def derivedForm :SQLWriteForm[P] = form


		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[P] =
			SQLWriteForm.combine(
				components.toSeq.map( _ match {
					case This(param) => param.derivedForm
					case comp => throw new IllegalArgumentException(s"Mapping $comp is not a component of parameter mapping $this")
				}) :_*
			)



		override def apply[T](component :Component[T]) :MappingExtract[P, T, O] = component match {
			case self :AnyRef if self eq this =>
				extractor.asInstanceOf[MappingExtract[P, T, O]]
			case mapping :FromParam[_, _]#ParamComponent[_] if mapping.root eq this =>
				mapping.asInstanceOf[ParamComponent[T]].extractor
			case _ =>
				throw new IllegalArgumentException(s"Mapping $component is not a part of parameter mapping $this")
		}

/*
		override def apply[T](column :Column[T]) :ColumnMappingExtract[P, T, O] = column match {
//			case self :AnyRef if self eq this =>
//				extractor.asInstanceOf[MappingExtract[P, T, O]]
			case mapping :FromParam[_, _]#ParamComponent[_] if mapping.root eq this =>
				mapping.asInstanceOf[ParamComponent[T]].extractor
			case _ =>
				throw new IllegalArgumentException(s"Mapping $column is not a part of parameter mapping $this")
		}
*/



		def apply[T :SQLForm](pick :P => T) :Component[T] = new ParamComponent[T](pick)

		def col[T :ColumnForm](pick :P => T) :Component[T] = apply(pick)



		private class ParamComponent[T :SQLForm](pick :P => T)
			extends ParamMapping[P, T, O]
		{
			override def root :FromParam[P, O] = This
			override def extractor :MappingExtract[P, T, O] = MappingExtract.req(this)(pick)
			override def toString = s"$This[$form]"
		}
/*
		private class ParamColumn[T :ColumnForm](pick :P => T)
			extends ParamComponent[T](pick) with ColumnMapping[T, O]
		{
			override def extractor :ColumnMappingExtract[P, T, O] = MappingExtract.req(this)(pick)

			override def name :String = ???
		}
*/


		override def toString :String = name + ":" + form



		def unapply[X](expr :SQLFormula[_, X]) :Option[ParamMapping[P, X, O]] = expr match {
			case ComponentFormula(_, MappingExtract(_, _, comp :ParamMapping[_, _, _])) if comp.root == this =>
				Some(comp.asInstanceOf[ParamMapping[P, X, O]])
			case _ => None
		}

		/** An extractor matching ComponentFormulas for components of this mapping, that is actual sql statement parameters. */
		def ParamForm :Extractor[SQLFormula[_, _], SQLWriteForm[P]] = Extractor(
			(sql :SQLFormula[_, _]) => unapply(sql).map(_.derivedForm)
		)

	}






	object FromParam {
		def apply[P, N <: String with Singleton](name :N)(implicit form :SQLForm[P]) :FromParam[P, N] =
			new FromParam(name)

		def unapply[X](expr :SQLFormula[_, X]) :Option[(FromParam[P, O], MappingExtract[P, X, O])] forSome { type P; type O } =
			expr match {
				case ComponentFormula(_, extractor) if extractor.export.isInstanceOf[ParamMapping[_, _, _]] =>
					val param = extractor.export.asInstanceOf[ParamMapping[Any, X, Any]]
					Some(param.root -> param.extractor)
				case _ => None
			}



		implicit def FromParamProjection[P, A, B] :OriginProjection[FromParam[P, A], A, FromParam[P, B], B] =
			OriginProjection()
	}






	class LabeledFromParam[N <: Label, X :SQLForm, O](override val name :N)
		extends FromParam[X, O](name) with LabeledMapping[N, X, O]




	type AnyParamJoin = WithParam[_ <: FromClause, _]



}



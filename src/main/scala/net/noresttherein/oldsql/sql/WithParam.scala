package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.support.{EmptyMapping, FormMapping}
import net.noresttherein.oldsql.schema.{ColumnForm, ComponentExtractor, GenericMapping, Mapping, RootMapping, RowSource, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.RowSource.AnyRowSource
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.FromClause.{As, ExtendedBy, SubselectFrom}
import net.noresttherein.oldsql.sql.MappingFormula.{ComponentFormula, JoinedRelation}
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.{AnyRelationIn, LastRelation}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple
import net.noresttherein.oldsql.sql.WithParam.{FromParam, ParamSource}





/** A specialized join class which joins the source on its left side with an artificial mapping `O ?: X`
  * to denote and track usage of a query parameter `X`. This makes it possible to filter on the given from clause
  * using values unknown at this time that can be obtained by applying an arbitrary scala function to `X`.
  * This class declares its `Outer` clause as `Nothing` (rather than the `Outer` of the left side) to ensure
  * neither it nor any its extension does not conform to `FromClause.SubselectFrom[F#Outer]`, thus preventing
  * it from being used as a part of a subselect of the outer clause, which would hide the existence of a parameter
  * and make its outer source appear not parameterized.
  *
  * @tparam F the actual from clause of the parameterized select statement.
  * @tparam X parameter type
  * @see [[net.noresttherein.oldsql.sql.WithParam.FromParam]]
  */
sealed trait WithParam[+F <: FromClause, X] extends With[F, ParamSource[X]#Row] {

	@inline final def from :F = left

	override type This >: this.type <: F WithParam X
	override type JoinRight[+L <: FromClause] = L WithParam X

	override type Outer = Nothing


	override def outer = throw new UnsupportedOperationException(s"WithParam($this).outer")



	override type SubselectRow = left.SubselectRow ~ X

	override def subselectRow[E <: FromClause](stretch :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow] =
		left.subselectRow(stretch.stretchFront[left.Generalized, ParamSource[X]#Row]) ~ table.upcast.stretch(stretch)

	override def subselectTableStack[E <: FromClause](stretch :Generalized ExtendedBy E) :LazyList[AnyRelationIn[E]] =
		table.extend(stretch) #:: left.subselectTableStack(stretch.stretchFront[left.Generalized, ParamSource[X]#Row])



	protected override def joinType :String = "with"

}






object WithParam {

	type <=[L <: FromClause, X] = WithParam[L, X]

	type ?:[N <: Label, X] = { type P[O] = ParamSource[X]#Row[O] }



	def apply[F <: FromClause, X](from :F, source :ParamSource[X]) :F WithParam X =
		WithParam[F, X](from, LastRelation(source), True())

	def apply[X] :ParamFactory[X] = new ParamFactory[X] {}

	trait ParamFactory[X] extends Any {
		def apply[F <: FromClause](from :F)(implicit form :SQLForm[X]) :F WithParam X =
			WithParam(from, LastRelation(ParamSource[X]()), True())

		def apply[F <: FromClause](from :F, name :String)(implicit form :SQLForm[X]) :F WithParam X =
			WithParam(from, LastRelation(ParamSource[X](name)), True())

		def apply[T[O] <: MappingFrom[O]](from :RowSource[T])(implicit form :SQLForm[X]) :From[T] WithParam X =
			WithParam(From(from), LastRelation(ParamSource[X]()), True())

		def apply[T[O] <: MappingFrom[O]](from :RowSource[T], name :String)(implicit form :SQLForm[X]) :From[T] WithParam X =
			WithParam(From(from), LastRelation(ParamSource[X](name)), True())
	}



	private[sql] def apply[L <: FromClause, X]
	                      (from :L, param :LastRelation[ParamSource[X]#Row],
	                       filter :BooleanFormula[L With ParamSource[X]#Row]) :L WithParam X =
		new WithParam[L, X] {
			override val left = from
			override val table = param
			override val joinCondition = filter

			override type This = L WithParam X

			override def copy[F <: FromClause](left :F, filter :BooleanFormula[Super[F, X]]) :F WithParam X =
				WithParam(left, table, filter)

			override def copy(filter :BooleanFormula[L With ParamSource[X]#Row]) :This =
				WithParam[L, X](left, table, filter) //todo:

		}

	private type Super[+F <: FromClause, X] = F With ParamSource[X]#Row



	def unapply[F <: FromClause, X](param :F WithParam X) :Option[(F, ParamSource[X])] =
		Some(param.left -> param.right.asInstanceOf[ParamSource[X]])

	def unapply(from :FromClause) :Option[(FromClause, ParamSource[_])] = from match {
		case param :AnyParamJoin => Some(param.left -> param.right.asInstanceOf[ParamSource[_]])
		case _ => None
	}






	class ParamSource[X](val name :String)(implicit val form :SQLForm[X])
		extends RowSource[({ type T[O] = FromParam[X, O] })#T]
	{
		override type Row[O] = FromParam[X, O]

		override def apply[O] :FromParam[X, O] = new FromParam[X, O](name)

		override def sql :String = name + "?:" + form
	}

	object ParamSource {
		def apply[X :SQLForm](name :String) :ParamSource[X] = new ParamSource[X](name)

		def apply[X :SQLForm]() :ParamSource[X] = new ParamSource("?")

		def unapply(source :AnyRowSource) :Option[(SQLForm[_], String)] = source match {
			case param :ParamSource[_] => Some(param.form -> param.name)
			case _ => None
		}
	}






	sealed abstract class ParamMapping[P, X, O] protected(implicit sqlForm :SQLForm[X]) extends FormMapping[X, O] {
		def root :FromParam[P, O]
		def extractor :ComponentExtractor[P, X, O]
		def derivedForm :SQLWriteForm[P] = form compose extractor
	}



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
	class FromParam[P, O] private[WithParam](val name :String)(implicit sqlForm :SQLForm[P])
		extends ParamMapping[P, P, O] //with FromParam
	{ This =>

		def this()(implicit form :SQLForm[P]) = this("?")

		override def root :FromParam[P, O] = this
		override def extractor :ComponentExtractor[P, P, O] = ComponentExtractor.ident(this)
		override def derivedForm :SQLWriteForm[P] = form


		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[P] =
			SQLWriteForm.combine(
				components.toSeq.map( _ match {
					case This(param) => param.derivedForm
					case comp => throw new IllegalArgumentException(s"Mapping $comp is not a component of parameter mapping $this")
				}) :_*
			)



		override def apply[T](component :Component[T]) :ComponentExtractor[P, T, O] = component match {
			case self :AnyRef if self eq this =>
				extractor.asInstanceOf[ComponentExtractor[P, T, O]]
			case mapping :FromParam[_, _]#ParamComponent[_] if mapping.root eq this =>
				mapping.asInstanceOf[ParamComponent[T]].extractor
			case _ =>
				throw new IllegalArgumentException(s"Mapping $component is not a part of parameter mapping $this")
		}



		def apply[T :SQLForm](pick :P => T) :Component[T] = new ParamComponent[T](pick)

//		def opt[T :SQLForm](pick :P => Option[T]) :TypedMapping[T] = new TypedMapping[T](Extractor(pick))

		def col[T :ColumnForm](pick :P => T) :Component[T] = apply(pick)

//		def optcol[T :ColumnForm](pick :P => Option[T]) :TypedMapping[T] = opt(pick)



		private class ParamComponent[T :SQLForm](pick :P => T)
			extends ParamMapping[P, T, O]
		{
			override def root :FromParam[P, O] = This
			override def extractor :ComponentExtractor[P, T, O] = ComponentExtractor.req(this)(pick)
			override def toString = s"$This[$form]"
		}


		override def toString = s":$name"



		def unapply[X](expr :SQLFormula[_, X]) :Option[ParamMapping[P, X, O]] = expr match {
			case ComponentFormula(_, ComponentExtractor(_, _, comp :ParamMapping[_, _, _])) if comp.root == this =>
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

		def unapply[X](expr :SQLFormula[_, X]) :Option[(FromParam[P, O], ComponentExtractor[P, X, O])] forSome { type P; type O } =
			expr match {
				case ComponentFormula(_, extractor) if extractor.export.isInstanceOf[ParamMapping[_, _, _]] =>
					val param = extractor.export.asInstanceOf[ParamMapping[Any, X, Any]]
					Some(param.root -> param.extractor)
				case _ => None
			}



		implicit def FromParamProjection[P, A, B] :OriginProjection[FromParam[P, A], A, FromParam[P, B], B] =
			OriginProjection()
	}



	type AnyParamJoin = WithParam[_ <: FromClause, _]



}



package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.support.{EmptyMapping, FormMapping}
import net.noresttherein.oldsql.schema.{ColumnForm, GenericMapping, Mapping, RootMapping, RowSource, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, ComponentExtractor}
import net.noresttherein.oldsql.sql.FromClause.SubselectFrom
import net.noresttherein.oldsql.sql.MappingFormula.{ComponentFormula, FromFormula}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.WithParam.{ParamMapping, ParamSource}





/** A specialized join class which joins the source on its left side with an artificial mapping `SourceParam[X]`
  * to denote and track usage of a query parameter `X`. This makes it possible to filter on the given source
  * using values unknown at this time that can be obtained by applying an arbitrary scala function to `X`.
  * This source declares it's `Outer` source as `Nothing` (rather than the outer of the left side) to ensure
  * that it won't be used as a basis for a select embedded in the outer source of the left side, thus 'hiding'
  * the existence of a parameter and making its outer source appear not parameterized.
  *
  * @param from any FromClause containing actual tables providing the data.
  * @param param TableFormula wrapping formal parameter X in a SourceParam[X] mapping
  * @param cond where condition to 'and' with any filter
  * @tparam F the actual source of data
  * @tparam X formal parameter type
  */
sealed trait WithParam[+F <: FromClause, X] extends Join[F, ParamSource[X]#Row] {
	@inline final def from :F = left


	type Outer = Nothing

	def outer = throw new UnsupportedOperationException(s"WithParam($this).outer")



	def mapping :ParamMapping[Any, X] = table.mapping


/*
	override def transplant[O <: FromClause](target: O, rewriter: SQLScribe[Outer, O]): SubselectFrom[O] =
		throw new UnsupportedOperationException(s"WithParam($this).transplant($target, _)")


	override protected def copyJoin(replacement: TableFormula[F Join ParamMapping[X], ParamMapping[X]],
	                                condition :BooleanFormula[F Join ParamMapping[X]] = True()): F WithParam X =
		new WithParam[F, X](from, replacement, condition)


	override def copyJoin[L <: FromClause, M <: Mapping](left: L, right: M): L Join M = right match {
		case p :ParamMapping[_] => new WithParam(left, p.asInstanceOf[ParamMapping[Any]]).asInstanceOf[L Join M]
		case _ => Join(left, right)
	}
*/

	override protected def joinType :String = "with"

}






object WithParam {

	def apply[F <: FromClause, X](from :F, source :ParamSource[X]) :F WithParam X =
		new JoinParam(from, source)

	def apply[F <: FromClause, X :SQLForm](from :F, name :String = "?") :F WithParam X =
		new JoinParam(from, ParamSource[X](name))






	class ParamSource[X :SQLForm](name :String) extends RowSource[({ type T[O] = ParamMapping[O, X] })#T] {
		override type Row[O] = ParamMapping[O, X]

		override def apply[O] :ParamMapping[O, X] = new ParamMapping[O, X](name)

		override def sql :String = ":" + name
	}

	object ParamSource {
		def apply[X :SQLForm](name :String) :ParamSource[X] = new ParamSource[X](name)

		def apply[X :SQLForm]() :ParamSource[X] = new ParamSource("?")
	}






	/** A Mapping instance representing a source parameter the value of which is not known. While the `BoundParameter`
	  *  formula can be used to represent a statement parameter, the value of the parameter must be known when
	  *  the formula using it is created. By representing a statement parameter as a mapping that can be used
	  *  in the same way as table mappings in source table lists, we can represent any value obtainable from `P`
	  *  by a function `P=>T` as a component `ParamMapping[P]#Component[T]` wrapping that function, which can be used
	  *  to create component formulas for that function. In particular, a `TableFormula[S, ParamMapping[P]]`
	  * is a formula which value will be substituted by a statement parameter `P`.
	  * @param name a suggested name of the parameter for debugging purposes.
	  * @tparam P the parameter type needed to prepare statements using this mapping in their sources.
	  */
	class ParamMapping[O, P](name :String)(implicit sqlForm :SQLForm[P]) extends FormMapping[O, P] { This =>

		def this()(implicit form :SQLForm[P]) = this("?")

		def apply[T :SQLForm](pick :P => T) :Component[T] = new ComponentMapping[T](Extractor.req(pick))

//		def opt[T :SQLForm](pick :P => Option[T]) :Component[T] = new Component[T](Extractor(pick))

		def col[T :ColumnForm](pick :P => T) :Component[T] = apply(pick)

//		def optcol[T :ColumnForm](pick :P => Option[T]) :Component[T] = opt(pick)



		override def apply[T](component :Component[T]) :ComponentExtractor[O, P, T] = component match {
			case self :AnyRef if self eq this =>
				ComponentExtractor.ident[O, P](this).asInstanceOf[ComponentExtractor[O, P, T]]
			case mapping :ParamMapping[_, _]#ComponentMapping[_] if mapping.param eq this =>
				ComponentExtractor[O, P, T](component)(mapping.asInstanceOf[ComponentMapping[T]].extractor)
			case _ =>
				throw new IllegalArgumentException(s"Component $component is not a part of this parameter mapping $this")
		}



		private class ComponentMapping[T :SQLForm](private[ParamMapping] val extractor :Extractor[P, T])
			extends FormMapping[O, T]
		{
			def param :ParamMapping[O, P] = This
			override def toString = s"$This[$form]"
		}


		override def toString = s":$name"



		def unapply[X](expr :SQLFormula[_, X]) :Option[ComponentExtractor[_, P, X]] = expr match {
			case ComponentFormula(t, selector) if t.mapping == this =>
				Some(selector.asInstanceOf[ComponentExtractor[_, P, X]])
			case _ => None
		}

		/** An extractor matching ComponentFormulas for components of this mapping, that is actual sql statement parameters. */
		def ParamForm :Extractor[SQLFormula[_, _], SQLWriteForm[P]] = Extractor(
			(sql :SQLFormula[_, _]) => unapply(sql).map {
				extractor => extractor.lifted.queryForm compose extractor
			}
		)

	}



	object ParamMapping {

		def unapply[X](expr :SQLFormula[_, X]) :Option[(ParamMapping[Any, C], ComponentExtractor[Any, C, X]) forSome { type C }] = expr match {
			case ComponentFormula(table, extractor) if table.mapping.isInstanceOf[ParamMapping[_, _]] =>
//				val param = extractor.asInstanceOf[ComponentExtractor[Any, X]]
//				param.surepick.map((table.mapping.asInstanceOf[ParamMapping[Any]], _, param.lifted.queryForm))
				Some(table.mapping.asInstanceOf[ParamMapping[Any, Any]] -> extractor.asInstanceOf[ComponentExtractor[Any, Any, X]])
			case _ => None
		}

	}





	private class JoinParam[F <: FromClause, X] protected
	                       (from :F, param :FromFormula[FromClause Join ParamSource[X]#Row, ParamSource[X]#Row],
	                        cond :BooleanFormula[F Join ParamSource[X]#Row])
		extends Join[F, ParamSource[X]#Row](from, param, cond) with WithParam[F, X]
	{
		def this(from :F, param :ParamSource[X]) =
			this(from, new FromFormula[FromClause, ParamSource[X]#Row](param, from.size), True())

		def this(from :F, name :String = "?")(implicit form :SQLForm[X]) = this(from, ParamSource[X](name))

		override def copy[L <: FromClause](left :L) :L WithParam X = new JoinParam(left, table, True())

		override def copy(filter :BooleanFormula[F Join ParamSource[X]#Row]) :F JoinParam X =
			new JoinParam(from, table, filter)

		type This = F JoinParam X

		def self :F JoinParam X = this

	}
}

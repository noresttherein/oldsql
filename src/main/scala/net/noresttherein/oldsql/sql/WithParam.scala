package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.support.{EmptyMapping, FormMapping}
import net.noresttherein.oldsql.schema.{ColumnForm, ComponentExtractor, GenericMapping, Mapping, RootMapping, RowSource, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{ConcreteMapping, MappingAlias, MappingFrom, TypedMapping}
import net.noresttherein.oldsql.sql.FromClause.SubselectFrom
import net.noresttherein.oldsql.sql.MappingFormula.{ComponentFormula, FromFormula, FromLast}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.WithParam.{ParamMapping, ParamSource}





/** A specialized join class which joins the source on its left side with an artificial mapping `O ?: X`
  * to denote and track usage of a query parameter `X`. This makes it possible to filter on the given from clause
  * using values unknown at this time that can be obtained by applying an arbitrary scala function to `X`.
  * This class declares its `Outer` clause as `Nothing` (rather than the `Outer` of the left side) to ensure
  * neither it nor any its extension does not conform to `FromClause.SubselectFrom[F#Outer]`, thus preventing
  * it from being used as a part of a subselect of the outer clause, which would hide the existence of a parameter
  * and make its outer source appear not parameterized.
  *
  * @tparam F the actual from clause of the parameterized select statement.
  * @tparam P a subtype of `ParamMapping` which carries the information about the parameter type and its origin/name/alias.
  * @see [[net.noresttherein.oldsql.sql.WithParam.?:]]
  * @see [[net.noresttherein.oldsql.sql.WithParam.ParamMapping]]
  */
sealed trait WithParam[+F <: FromClause, P <: Mapping] extends Join[F, P] {
	@inline final def from :F = left

	def mapping :P = table.mapping

	type Outer = Nothing

	def outer = throw new UnsupportedOperationException(s"WithParam($this).outer")


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

//	def apply[F <: FromClause, X, O](from :F, source :ParamSource[X]) :F WithParam (X ?# O) =
//		new JoinParam[F, X, O](from, FromLast(source[O], from.size))

//	def apply[F <: FromClause, X :SQLForm, O](from :F, name :String = "?") :F WithParam (X ?# O) =
//		new JoinParam[F, X, O](from, FromLast(new ?#[X, O](name), from.size))

	def apply[F <: FromClause, X, O](from :F, source :ParamSource[X]) :F WithParam (O ?: X) =
		new JoinParam[F, X, O](from, FromLast(source[O], from.size))

	def apply[F <: FromClause, X, O](from :F, param :O ?: X) :F WithParam (O ?: X) =
		new JoinParam[F, X, O](from, FromLast(param, from.size))

	def apply[X :SQLForm, O] :ArbitraryParamFactory[X, O] = new ArbitraryParamFactory[X, O] {}

	def apply[X :SQLForm] :LiteralParamFactory[X] = new LiteralParamFactory[X](SQLForm[X])



	sealed trait ArbitraryParamFactory[X, O] extends Any {
		def apply[F <: FromClause](from :F, name :String = "?")(implicit form :SQLForm[X]) :F WithParam (O ?: X) =
			new JoinParam[F, X, O](from, FromLast(new ParamMapping[X, O](name), from.size))
	}

	class LiteralParamFactory[X](private val form :SQLForm[X]) extends AnyVal {
		def apply[F <: FromClause, N <: String with Singleton](from :F, name :N) :F WithParam (N ?: X) =
			new JoinParam[F, X, N](from, FromLast(new ParamMapping[X, N](name)(form), from.size))
	}


	class ParamSource[X :SQLForm](name :String) extends RowSource[({ type T[O] = O ?: X })#T] {
		override type Row[O] = O ?: X

		override def apply[O] :O ?: X = new ?:[O, X](name)

		override def sql :String = ":" + name
	}

	object ParamSource {
		def apply[X :SQLForm](name :String) :ParamSource[X] = new ParamSource[X](name)

		def apply[X :SQLForm]() :ParamSource[X] = new ParamSource("?")
	}





//	sealed trait ParamMapping extends Mapping { this :ConcreteMapping => }

	/** A `Mapping` instance representing a query parameter the value of which is not known. While the `BoundParameter`
	  * formula can be used to represent a statement parameter, its value must be known when the formula is created.
	  * By representing a statement parameter as a mapping that can be used in the same way as table mappings
	  * in `FromClause` table lists, we can represent any value obtainable from `P`
	  * by a function `P => T` as a component `(P #? _)#TypedMapping[T]` wrapping that function, which can be used
	  * to create component formulas for that function. In particular, a `TableFormula[S, ParamMapping[P]]`
	  * is a formula which value will be substituted by a statement parameter `P`.
	  * @param name a suggested name of the parameter for debugging purposes.
	  * @tparam P the parameter type needed to prepare statements using this mapping in their sources.
	  * @tparam O a marker type serving as a unique alias for this mapping within a `FromClause`.
	  */
	class ParamMapping[P, O] private[WithParam] (name :String)(implicit sqlForm :SQLForm[P])
		extends FormMapping[P, O] //with ParamMapping
	{ This =>

		def this()(implicit form :SQLForm[P]) = this("?")

		def apply[T :SQLForm](pick :P => T) :Component[T] = new ComponentMapping[T](Extractor.req(pick))

//		def opt[T :SQLForm](pick :P => Option[T]) :TypedMapping[T] = new TypedMapping[T](Extractor(pick))

		def col[T :ColumnForm](pick :P => T) :Component[T] = apply(pick)

//		def optcol[T :ColumnForm](pick :P => Option[T]) :TypedMapping[T] = opt(pick)



		override def apply[T](component :Component[T]) :ComponentExtractor[P, T, O] = component match {
			case self :AnyRef if self eq this =>
				ComponentExtractor.ident[P, O](this).asInstanceOf[ComponentExtractor[P, T, O]]
			case mapping :ParamMapping[_, _]#ComponentMapping[_] if mapping.param eq this =>
				ComponentExtractor[P, T, O](component)(mapping.asInstanceOf[ComponentMapping[T]].extractor)
			case _ =>
				throw new IllegalArgumentException(s"TypedMapping $component is not a part of this parameter mapping $this")
		}



		private class ComponentMapping[T :SQLForm](private[ParamMapping] val extractor :Extractor[P, T])
			extends FormMapping[T, O]
		{
			def param :O ?: P = This
			override def toString = s"$This[$form]"
		}


		override def toString = s":$name"



		def unapply[X](expr :SQLFormula[_, X]) :Option[ComponentExtractor[P, X, _]] = expr match {
			case ComponentFormula(t, selector) if t.mapping == this =>
				Some(selector.asInstanceOf[ComponentExtractor[P, X, _]])
			case _ => None
		}

		/** An extractor matching ComponentFormulas for components of this mapping, that is actual sql statement parameters. */
		def ParamForm :Extractor[SQLFormula[_, _], SQLWriteForm[P]] = Extractor(
			(sql :SQLFormula[_, _]) => unapply(sql).map {
				extractor => extractor.export.queryForm compose extractor
			}
		)

	}



	object ParamMapping {
		def apply[P, N <: String with Singleton](name :N)(implicit form :SQLForm[P]) :N ?: P =
			new ?:(name)

		def unapply[X](expr :SQLFormula[_, X]) :Option[(O ?: P, ComponentExtractor[P, X, O])] forSome { type P; type O } =
			expr match {
				case ComponentFormula(table, extractor) if table.mapping.isInstanceOf[_ ?: _] =>
					Some(table.mapping.asInstanceOf[Any ?: Any] -> extractor.asInstanceOf[ComponentExtractor[Any, X, Any]])
				case _ => None
			}



		implicit def ParamMappingAlias[P, A, B] :MappingAlias[P :? A, A, P :? B, B] =
			MappingAlias()
	}



//	type ?#[X, O] = ParamMapping[X, O]
//
//	implicit def ?#[O <: String with Singleton](name :O) :method_?#[O] = new method_?#[O](name)
//
//	class method_?#[O <: String with Singleton](private val name :O) extends AnyVal {
//		def ?#[X :SQLForm] :O ?# X = ??? //new ParamMapping[X, O](name)
//	}

	type ?:[O, X] = ParamMapping[X, O]
	type :?[X, O] = ParamMapping[X, O]

	implicit def ?:[O <: String with Singleton](name :O) :method_?:[O] = new method_?:[O](name)

	class method_?:[O <: String with Singleton](private val name :O) extends AnyVal {
		def ?:[X :SQLForm] :O ?: X = new ParamMapping[X, O](name)
	}



	private class JoinParam[F <: FromClause, X, O]
	                       (from :F, param :FromFormula[FromClause Join (O ?: X), O ?: X],
	                        cond :BooleanFormula[F Join (O ?: X)] = True())
		extends Join[F, O ?: X](from, param, cond) with WithParam[F, O ?: X]
	{
		override def copy[L <: FromClause](left :L) :L WithParam (O ?: X) = new JoinParam(left, table, True())

		override def copy(filter :BooleanFormula[F Join (O ?: X)]) :This =
			new JoinParam(from, table, filter)

		type This = JoinParam[F, X, O]

//		override def self :This = this

	}
}

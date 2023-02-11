package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.{Component, _}
import net.noresttherein.oldsql.schema.{TypedColumn, Mapping, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{NoQuery, NoSelect}
import net.noresttherein.oldsql.schema.MappingPath.{ComponentPath, SelfPath}
import net.noresttherein.oldsql.schema.bits.MappingPath
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth
import net.noresttherein.oldsql.sql.RowProduct.{RowValues, TableFormula}
import net.noresttherein.oldsql.sql.MappingFormula.ComponentFormula.{CaseComponent, ComponentMatcher}
import net.noresttherein.oldsql.sql.MappingFormula.PathFormula.{CasePath, MatchPath, PathMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.{Formula, FormulaMatcher, SQLTypePromotion}
import net.noresttherein.oldsql.sql.SQLMapper.SQLRewriter
import net.noresttherein.oldsql.sql.SQLTerm.{BoundParameter, SQLLiteral, SQLNull}



/** An `SQLFormula` which SQL form is described by a `Mapping` of type `M`, with its subject being the scala expression
  * type of the formula.
  */
trait MappingFormula[-F <: RowProduct, M <: Mapping] extends SQLFormula[F, M#Subject] {
	def mapping :M

	override def readForm :SQLReadForm[M#Subject] = mapping.selectForm

}



object MappingFormula {


	/** A placeholder expression which value is a value of the end mapping specified by the given path.
	  * This class shouldn't be used directly in sql filters, as the path can represent higher level concepts such as
	  * joins between tables. To refer to a component of a last from the ''from'' clause, use ComponentFormula.
	  * This class exists to facilitate creating filters on a higher level of abstraction, which can be adapted
	  * for different queries, depending on a chosen loading strategy.
	  * @tparam F type of RowProduct this instance is compatible with (i.e, which contains the source last)
	  * @tparam T mapping type of the source last.
	  * @tparam M target mapping type
	  */
	abstract class PathFormula[-F <: RowProduct, T <: Component[R, E], M <: Component[O, V], R, O, E, V] private[sql]
		extends MappingFormula[F, M] with SQLTuple[F, V]
	{
		/** Table in the source at which the path starts. */
		def table :TableFormula[F, T, R, E]

		/** Mapping path specifying the value to be inserted in the final sql in the place of this expression. */
		def path :MappingPath[T, M, O, E, V]

		override def mapping :M = path.end

		override val parts: Seq[SQLFormula[F, _]] =
			if (path.end.subcomponents.isEmpty)
				Nil
			else
                path.end.columns.filterNot { c => NoSelect.enabled(c) && NoQuery.enabled(c) }.map {
					column => PathFormula(table, path \ column)
				}


		override def isGroundedIn(tables: Iterable[TableFormula[_, _, _, _]]): Boolean = false

		override def get(values: RowValues[F]): Option[M#Subject] = values.get(table).flatMap(path.extractor.optional)

		def queryLiteral(value :T#Subject) :SQLFormula[RowProduct, M#Subject] =
			throw new UnsupportedOperationException(s"Can't translate an abstract path expression $this into a literal (of value $value)")

		def queryParameter(value :T#Subject) :SQLFormula[RowProduct, M#Subject] =
			throw new UnsupportedOperationException(s"Can't translate an abstract path expression $this into a parameter (of value $value)")



		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[M#Subject] = matcher.path(this)


		override def map[S <: RowProduct](mapper: SQLRewriter[F, S]): SQLFormula[S, V] = mapper(this)

		override def equals(other :Any) :Boolean = other match {
			case self :AnyRef if self eq this => true
			case p :PathFormula[_, _, _, _, _, _, _] if p canEqual this => table == p.table && path == p.path
			case _ => false
		}

		override def canEqual(other :Any) :Boolean = other.isInstanceOf[PathFormula[_, _, _, _, _, _, _]]

		override def hashCode :Int = table.hashCode * 31 + path.hashCode


		override def isomorphic(expression: Formula[_]): Boolean = expression match {
			case e :PathFormula[_, _, _, _, _, _, _] =>
				(this eq e) || canEqual(e) && e.canEqual(this) && (table sameAs e.table) && path == e.path
			case _ => false
		}

		override def equivalent(expression: Formula[_]): Boolean = expression match {
			case self :AnyRef if self eq this => true
			case p :PathFormula[_ ,_ , _, _, _, _, _] if canEqual(p) && p.canEqual(this) =>
				(table sameAs p.table) && (path.end == p.path.end)
			case _ => false
		}

		override def toString :String = table.toString + path

	}






	/** Factory for path expressions which will always produce a `ComponentFormula` if the passed path is a `ComponentPath`. */
	object PathFormula {

		/** Create an abstract expression denoting a value of the end mapping of a path starting at one of member tables
		  * in the FROM clause. If passed path is a `ComponentPath`, a `ComponentExpression` will be produced which
		  * translates to a single column or a tuple of columns from the argument last, depending on the mapping
		  * subject type. Other `MappingPath` instances will result in generic placeholders which can't be
		  * directly translated into SQL and should be replaced at some later stage with a proper expression.
		  *
		  * @param table source mapping (not necessarily a last - might be a component) from which the path starts
		  * @param path a path pointing to a mapping which value is will be the value of this expression.
		  */
		def apply[F <: RowProduct, T <: Component[R, E], M <: Component[O, V], R, O, E, V]
		         (table :TableFormula[F, T, R, E], path :MappingPath[T, M, O, E, V]) :PathFormula[F, T, M, R, O, E, V] =
			path match {
//				case _  if path.start != last.mapping =>
//					throw new IllegalArgumentException(s"PathFormula($last, $path): path doesn't start at last mapping")
//				case _ :SelfPath[_, _, _] =>
//					last.asInstanceOf[PathFormula[F, T, M, O, E, V]]
				case comp :ComponentPath[_, _, _, _, _] =>
					val t = table.asInstanceOf[TableFormula[F, Component[O, E], O, E]]
					val c = comp.asInstanceOf[ComponentPath[Component[O, E], M, O, E, V]]
					ComponentFormula(t, c).asInstanceOf[PathFormula[F, T, M, R, O, E, V]]
				case _ =>
					new ForeignPathFormula(table, path)
			}



		def unapply[F <: RowProduct, V](expression :SQLFormula[F, V])
				:Option[(TableFormula[F, T, R, E], MappingPath[T, C, O, E, V])] forSome
					{ type T <: Component[R, E]; type C <: Component[O, V]; type R; type O; type E } =
			expression match {
				case path :PathFormula[_, _, _, _, _, _, _] =>
					val cast = path.asInstanceOf[PathFormula[F, Component[Any, Any], Component[Any, V], Any, Any, Any, V]]
					Some(cast.table -> cast.path)
				case _ => None
			}


		trait PathMatcher[+F <: RowProduct, +Y[X]] extends ComponentMatcher[F, Y] {
			def path[T <: Component[R, E], C <: Component[O, V], R, O, E, V](f :PathFormula[F, T, C, R, O, E, V]) :Y[V]
		}

		trait MatchPath[+F <: RowProduct, +Y[X]] extends PathMatcher[F, Y] with CaseComponent[F, Y]


		trait CasePath[+F <: RowProduct, +Y[X]] extends MatchPath[F, Y] {
			def component[T <: Component[O, E], C <: Component[O, V], O, E, V](f :ComponentFormula[F, T, C, O, E, V]) :Y[V] =
				path(f)
		}


		private class ForeignPathFormula[-F <: RowProduct, T <: Component[R, E], M <: Component[O, V], R, O, E, V]
		                                (val table :TableFormula[F, T, R, E], val path :MappingPath[T, M, O, E, V])
			extends PathFormula[F, T, M, R, O, E, V]


	}






	abstract class ComponentFormula[-F <: RowProduct, T <: Component[O, E], C <: Component[O, V], O, E, V] private[sql]
		extends PathFormula[F, T, C, O, O, E, V]
	{
		override def path  :ComponentPath[T, C, O, E, V] //ComponentPath[T, C]

		def component :C = path.end

		override def readForm :SQLReadForm[V] = table.mapping.lift(path.end).selectForm


		implicit private def sqlForm :SQLForm[V] = table.mapping.lift(path.end) match {
			case column :TypedColumn[O, V] => column.form //fixme: this doesn't take into account the buffs from selectForm
			case comp  => comp.selectForm && comp.queryForm
		}


		/** Create an SQL formula for the given component of  this mapping. If the component is not a single column,
		  * it will be treated as a tuple/sequence of columns and produce a literal in a form of `(col1, col2, col3)`
		  * in the resulting SQL.
		  * @param subcomponent a component of the component which this formula's path points to.
		  * @return An SQL expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def \ [M <: Mapping, S <: Component[O, X], X](subcomponent :M)(implicit hint :IsBoth[M, S, Component[O, X]])
				:ComponentFormula[F, T, S, O, E, X] =
			ComponentFormula(table, path \ subcomponent)


		/** Create an SQL formula for the given component of this mapping. If the subcomponent is not a single column, it will be
		  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		  * @param subcomponent a component of the mapping associated with this last.
		  * @return an SQL expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def :\ [X](subcomponent :Component[O, X]) :ComponentFormula[F, T, subcomponent.type, O, E, X] =
			ComponentFormula(table, path :\ subcomponent)

		/** Create an SQL formula for some subcomponent of this mapping, including the whole mapping itself in the case
		  * of a `SelfPath`. If the component is not a single column, it will be treated as a tuple/sequence of columns
		  * and produce a literal in a form of `(col1, col2, col3)` in the resulting SQL.
		  * @param path path starting at the end of this instance's path and pointing to the desired component.
		  * @return an SQL expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def \[S <: Component[O, X], X](path :ComponentPath[C, S, O, V, X]) :ComponentFormula[F, T, S, O, E, X] =
			ComponentFormula(table, this.path \ path)


//		def :=[S <: RowProduct, R, U](expr :SQLFormula[S, R])(implicit lift :SQLTypeUnification[V, R, U]) :SetComponent[S, T, C, R, U] =
//			SetComponent(path, expr)

		override def queryLiteral(value :E) :SQLFormula[RowProduct, V] =
			path.extractor.get(value).map(v => SQLLiteral(v)) getOrElse SQLNull[V]

		override def queryParameter(value :E) :SQLFormula[RowProduct, V] =
			path.extractor.get(value).map(v => BoundParameter(v)) getOrElse SQLNull[V]


		override def isGroundedIn(tables :Iterable[TableFormula[_, _, _, _]]) :Boolean =
			tables.exists(_ == table)

		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[V] = matcher.component(this)


		override def isomorphic(expression: Formula[_]): Boolean = expression match {
			case e :ComponentFormula[_, _, _, _, _, _] =>
				(e eq this) || (e.table sameAs table) && e.path == path
			case _ => false
		}

		override def equivalent(expression: Formula[_]): Boolean = isomorphic(expression)

	}






	object ComponentFormula {

		def apply[F <: RowProduct, T <: Component[O, E], M <: Component[O, V], O, E, V]
		         (table :TableFormula[F, T, O, E], path :ComponentPath[T, M, O, E, V]) :ComponentFormula[F, T, M, O, E, V] =
			path match {
//				case _  if path.start!=last.mapping =>
//					throw new IllegalArgumentException(s"ComponentFormula($last, $path): path doesn't start at last mapping")
				case _ :SelfPath[_, _, _] =>
					table.asInstanceOf[ComponentFormula[F, T, M, O, E, V]]
				case _ =>
					new TableComponent(table, path)
			}

		def unapply[F <: RowProduct, X](expression :SQLFormula[F, X])
				:Option[(TableFormula[F, T, O, E], ComponentPath[T, C, O, E, X])] forSome
					{ type T <: Component[O, E]; type C <: Component[O, X]; type O; type E } =
			expression match {
				case comp :ComponentFormula[_, _, _, _, _, _] =>
					val cast = comp.asInstanceOf[ComponentFormula[F, Component[Any, Any], Component[Any, X], Any, Any, X]]
					Some(cast.table -> cast.path)
				case _  => None
			}



		private class TableComponent[-F <: RowProduct, T <: Component[O, E], C <: Component[O, V], O, E, V]
		                            (val table :TableFormula[F, T, O, E], val path :ComponentPath[T, C, O, E, V])
		extends ComponentFormula[F, T, C, O, E, V]



		trait ComponentMatcher[+F <: RowProduct, +Y[X]] {
			def component[T <: Component[O, W], C <: Component[O, V], O, W, V](f: ComponentFormula[F, T, C, O, W, V]): Y[V]
		}

		type MatchComponent[+F <: RowProduct, +Y[X]] = ComponentMatcher[F, Y]

		type CaseComponent[+F <: RowProduct, +Y[X]] = ComponentMatcher[F, Y]
	}






	type MappingMatcher[+F <: RowProduct, +Y[X]] = PathMatcher[F, Y]

	type MatchMapping[+F <: RowProduct, +Y[X]] = MatchPath[F, Y]

	trait CaseMapping[+F <: RowProduct, +Y[X]] extends CasePath[F, Y] {
		def mapping[M <: Mapping](f :MappingFormula[F, M]) :Y[M#Subject]

		override def path[T <: Component[R, E], M <: Component[O, V], R, O, E, V](f: PathFormula[F, T, M, R, O, E, V]): Y[V] =
			mapping(f)
	}



}

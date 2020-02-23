package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.MappingPath.TypedDirectPath
import com.hcore.ogre.mapping.{AnyMapping, Mapping, MappingPath}
import com.hcore.ogre.model.ComposedOf.Arity
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.sql._


//implicits
import SaferCasts._


/** A path representing a relationship between mappings X and Y from different tables, where value Y is most probably reachable through a reference from X
  * (although this particular class doesn't really care about it). This path represents the directed view of the associated
  * relationship from the side of component X; while in most cases the reverse side would consist of an equivalent join, the order
  * of the mappings is important. Please note that the associated join definition is reversed for convenient access to the first mapping.
  * The query based on this join will usually include the tables (of which there maybe more than just two, as RowSource
  * encompasses any empty or non-empty list of mappings) in the reversed order and, if any of the joins are outer joins,
  * should use it's opposite (left vs. right).
  *
  * As JoinPath will usually be concatenated with other mapping paths, mapping X will generally not correspond to a whole table, but rather
  * a component mapping the value for the mapping Y (such as a Reference[Y#ResultType]. On the other hand. Y should be an actual table,
  * and if a reference for a table component rather than a whole table is required, it should be implemented by concatenating an appropriate
  * ComponentPath to the end of this path.
  * @tparam X mapping type representing the source of the directed relationship with Y; this doesn't have to be a table
  *           and will usually be a component of an actual table corresponding to a Reference[Y#ResultType].
  * @tparam Y target mapping, by default should represent a table.
  */
trait JoinPath[X<:AnyMapping, Y<:AnyMapping] extends MappingPath[X, Y] {

	/** The join underlying this relationship, containing a list of two or more tables, in the reverse order of their joining:
	  * the source of the relationship X, it's target Y, and zero or more other tables (such as dictionaries) which don't participate
	  * in mapping the result value of Y, but rather serve as potential additional filter. Note that if you want to use
	  * outer joins, they will usually be replaced with their opposites if the tables are included in the query in a reverse order; also,
	  * query builders don't have to respect the choice of the join set here.
	  */
	val join :RowSource Join Y Join X

	/** Arity of the target of the relationship which will serve as a hint of how many rows for mapping Y should we expect
	  * for a single row of X. This value will be used to determine if outer joins are necessary or if a join should be replaced
	  * wit ha separate select.
	  */
	val arity :Arity
}


object JoinPath {
	def apply[X<:AnyMapping, Y<:AnyMapping](join :RowSource Join Y Join X, arity :Arity, pick :X#ResultType => Option[Y#ResultType]) :JoinPath[X, Y] =
		new TypedJoinPath[From[Mapping[Any]] Join Mapping[Any], Mapping[Any], Any, Mapping[Any], Any](
			join.asInstanceOf[From[Mapping[Any]] Join Mapping[Any]],
			pick.asInstanceOf[Any=>Option[Any]],
			arity
		).asInstanceOf[JoinPath[X, Y]]

	def apply[X<:AnyMapping, Y<:AnyMapping](join :From[Y] Join X, pick :X#ResultType=>Option[Y#ResultType] = (_:Any) => None, arity :Arity=Arity._0_n) :SingleJoin[X, Y] =
		new TypedSingleJoin(join.asInstanceOf[From[Mapping[Any]] Join Mapping[Any]], pick.asInstanceOf[Any=>Option[Any]], arity).asInstanceOf[SingleJoin[X, Y]]



	def unapply[X<:AnyMapping, Y<:AnyMapping](path :MappingPath[X, Y]) :Option[(RowSource Join Y Join X, Arity)] =
		path.ifSubclass[JoinPath[X, Y]] { p => (p.join, p.arity) }


	case class TypedJoinPath[J<:RowSource Join Y Join X, X<:Mapping[S], S, Y<:Mapping[T], T](join :J, pick :S=>Option[T], arity :Arity)
		extends JoinPath[X, Y] with TypedDirectPath[X, Y, T]
	{
		def start = join.right
		def end = join.left.right

		def optional = true
	}



	trait SingleJoin[X<:AnyMapping, Y<:AnyMapping] extends JoinPath[X, Y] {
		val join :From[Y] Join X

		def inverse(getter :Y#ResultType=>Option[X#ResultType], arity :Arity) :SingleJoin[Y, X]
	}

	case class TypedSingleJoin[X<:AnyMapping{ type ResultType=S }, S, Y<:AnyMapping{ type ResultType=V }, V](join :From[Y] Join X, pick :S=>Option[V], arity :Arity=Arity._0_n)
		extends TypedDirectPath[X, Y, V] with SingleJoin[X, Y]
	{
		def start = join.right //join.left.mapping
		def end = join.left.mapping //join.right

		def optional = true //!AtMostOne(arity)

		def inverse(getter :V=>Option[S], arity :Arity) = {
			val (left, right) = (join.left.right, join.right)
			val inversed = join match {
				case LeftJoin(_, _) => From(right) rightJoin left
				case RightJoin(_, _) => From(right) leftJoin left
				case Join(_, _) => From(right) join left
			}
			val filtered = inversed where SQLScribe.replant(join.filter, join, inversed) {
				case join.lastTable => inversed.prev
				case join.left.lastTable => inversed.last
			}
			TypedSingleJoin[Y, V, X, S](filtered, getter, arity)
		}

		override def tailString = s" join $end"
	}


}
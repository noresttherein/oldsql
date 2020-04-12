package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{Mapping, RowSource, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.AnyComponent
import net.noresttherein.oldsql.sql.FromClause.SubselectFrom
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.sql.MappingFormula.FromFormula
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple




/** A join between an existing `FromClause`, representing a table or a joined list, and another mapping.
  * The given mapping doesn't have to represent a table at this point - it might be for example a table component
  * to be 'planted' in a particular table at a later point.
  *
  * Note that, as with all generic types taking exactly two arguments, it can be written in the infix notation:
  * `val usersGuns :From[Users] Join UserGuns Join Guns`. This class is covariant regarding its left side,
  * so a sequence of joined mappings `X0 J1 X1 J2 X2 .. JN XN &lt;: X0 Join X1 Join X2 ... Join XN`
  * if for all `JN &lt;: Join`.
  *
  * @param left a FromClause constituting a pre-existing join list of tables - may be empty (Dual).
  * @param table the right side of the join - representation of a table alias containing the joined mapping.
  * @param joinCondition the join condition joining the right side to the left side. It is not the complete filter 
  *                      condition, as it doesn't include any join conditions defined in the left side of this join.
  * @tparam L the left side of this join.
  * @tparam R the right side of this join.
  */
abstract class Join[+L <: FromClause, R[O] <: AnyComponent[O]] protected
                   (val left :L, protected[this] val table :FromFormula[FromClause Join R, R],
                    protected[this] val joinCondition :BooleanFormula[L Join R])
	extends FromClause
{ join =>

	private[sql] def this(left :L, right :RowSource[R], condition :BooleanFormula[L Join R] = True()) =
		this(left, new FromFormula[FromClause Join R, R](right, left.size), condition)



	protected def copy(filter :BooleanFormula[L Join R]) :This //= copy(left, table) where joinCondition

	def copy[F <: FromClause](left :F) :F Join R


	type This <: L Join R //{
//		type This = join.This
//	}

	def self :This


//	def as[A <: String with Singleton](alias :A) :This As A =

	type Row = left.Row ~ R[Any]#Subject

	override def row :ChainTuple[this.type, left.Row ~ R[Any]#Subject] = //todo:
		left.row.asInstanceOf[ChainTuple[this.type, left.Row]] ~ table


//	def from :RowSource[R] = table.from

	type LastMapping[O] = R[O] //todo:
	type LastTable[-F <: FromClause] = FromFormula[F, R]

	override def lastTable :FromFormula[FromClause Join R, R] = table

	def right :R[Any] = lastTable.mapping


	def condition :BooleanFormula[this.type] = joinCondition


	override val size :Int = left.size + 1


	protected def filter(filter :BooleanFormula[L Join R]) :This = copy(joinCondition && filter)

	def whereLast(filter :FromFormula[FromClause Join R, R] => BooleanFormula[FromClause Join R]) :This =
		this.filter(filter(table))

//	def on(filter :(L#LastTable[L Join R], FromFormula[L Join R, R]) => BooleanFormula[L Join R]) :Self[L] =
//		this.filter(filter(left.lastTable))
/*
	override def filteredBy :BooleanFormula[this.type] = left.filter.asPartOf(this) && joinCondition

	override def filterBy[S >: this.type <: FromClause](filter: BooleanFormula[S]): S =
		copyJoin(table, joinCondition && filter.downcast).asInstanceOf[S]

	override def row: ChainTuple[this.type, left.Row ~ R#Subject] =
		left.row.asPartOf(this :this.type) ~ //asInstanceOf[HListFormula[this.type, left.Row]]

	override def mappings :Seq[Mapping] = right +: left.mappings

	override def size :Int = lastTable.index + 1

	override def occurrences(mapping: Mapping): Int =
		left.occurrences(mapping) + (if (mapping == right) 1 else 0)


	override def allTables :Seq[FromFormula[Join.this.type, _ <: Mapping]] = toTableStream.reverse

	override protected[sql] def toUntypedTableStream :Seq[FromFormula[FromClause, _ <: Mapping]] =
		asTables[FromClause].last +: left.toUntypedTableStream

	override def subselectTableStream: Seq[FromFormula[this.type, _ <: Mapping]] =
		asTables[FromClause].last +: left.subselectTableStream.asInstanceOf[Seq[FromFormula[this.type, _<:Mapping]]]

	override def headTable :FromFormula[this.type, _ <: Mapping] =
		if (size == 1) lastTable
		else left.head




	override def joinAny(source: FromClause): FromClause = source match {
		case _:Dual => this
		case j:Join[_, _] =>
			val join = j.copyJoin(joinAny(j.left))
			val replanter = new SQLScribe.Replanter[j.type, join.type](j, join) {
				override def map[A <: Mapping](table: FromFormula[j.type, A]): FromFormula[join.type, A] =
					(j.toStream zip join.toStream).find(_._1==table).map(_._2.asInstanceOf[FromFormula[join.type, A]]) getOrElse {
						throw new IllegalArgumentException(s"unrecognized table $table when cross joining $this and $j")
					}
			}
			join filterBy replanter(j.condition)
	}





	/** Perform a join between `S` and `R`, preserving this join's nature and class (inner, outer), but discarding
	  * any filter associated with this instance. 
	  */
	def copyJoin[S <: FromClause](source :S) :S Join R = copyJoin(source, right)

	def copyJoin[S <: FromClause, M <: Mapping](left :S, right :M) :S Join M

	protected def copyJoin(replacement :FromFormula[L Join R, R], condition :BooleanFormula[L Join R] = True()) :L Join R

*/

	/** Apply a join condition to the last two tables in this chain. This works exactly like 'where', but
	  * instead of a single argument representing all joined tables, the filter function should take as its arguments
	  * the last two tables, i.e, the last table defined by the left side of this join, if any, and the right side 
	  * of this join. Static type checking will enforce that this method can't be called on 'joins' where the left side 
	  * is empty (single table sources).
	  * @param condition a function creating a representation of an SQL expression from the passed aliases 
	  *                  for the joined tables.
	  */
/*
	def on(condition :(L#LastTable[this.type], FromFormula[this.type, R]) => BooleanFormula[this.type]) :L Join R =
		filterBy(condition(left.last.asPartOf(this).asInstanceOf[L#LastTable[this.type]], lastTable))



	override def plant(prefix: PartialFunction[FromFormula[this.type, _ <: Mapping],
		ComponentPath[_ <: Mapping, _ <: Mapping]]): FromClause =
	{
		val cast = prefix.asInstanceOf[PartialFunction[FromFormula[left.type, _ <: Mapping], ComponentPath[_ <: Mapping, _ <: Mapping]]]
		val previous = left.plant(cast)
		val planted = copyJoin(previous, prefix.applyOrElse(lastTable, (_:Any) => ComponentPath.self(right)).start)
		val tableMapping = (allTables zip planted.allTables).toMap[FromFormula[this.type, _], FromFormula[planted.type, _]]

		def mapped(t :FromFormula[this.type, _], path :MappingPath[_, _]) =
			PathFormula(tableMapping(t).asInstanceOf[FromFormula[planted.type, Mapping]], path.unchecked)

		planted filterBy {
			SQLScribe(this, planted)(joinCondition) { expr =>
				val concat = prefix andThen ((_: (_ \**\ _)).unchecked ++ expr.path.unchecked)
				val prefixed = concat.applyOrElse(expr.table, (_:Any) => expr.path)
				mapped(expr.table, prefixed)
			}
		}
	}


	override def plantMatching(prefix: ComponentPath[_ <: Mapping, _ <: Mapping]): FromClause =
		plant{ case t :FromFormula[this.type, _] if t.mapping == prefix.start => prefix }


	def plantLast[T <: Mapping](path :ComponentPath[T, R]) :L Join T = path match {
		case SelfPath(m) if m == right =>
			this.asInstanceOf[L Join T]
		case _ =>
			val planted = copyJoin(left, path.start)
			planted where SQLScribe(joinCondition, this, planted) {
				case last if last.table == Join.this.last =>
					PathFormula(planted.last, path ++ last.path.cast[R, Mapping])
				case prev => prev.asInstanceOf[SQLFormula[L Join T, _]]
			}
	}


	override def substitute[T](table: FromFormula[this.type, _ <: Mapping[T]], value: T): FromClause =
		if (table == lastTable) substitute(value.asInstanceOf[R#Subject])
		else {
			val previous = left.substitute(table.asInstanceOf[FromFormula[left.type, Mapping[T]]], value)
			val joined = copyJoin(previous, right)
			val tables = joined.all.toIndexedSeq
			val translated = SQLScribe(this, joined)(joinCondition) {
				case e if e.table == table =>
					e.asInstanceOf[PathFormula[this.type, Mapping[T], Mapping]].queryParameter(value)
				case e if e.table.index < table.index =>
					e.asInstanceOf[PathFormula[joined.type, Mapping, Mapping]]
				case e if e.table.index > table.index =>
					PathFormula(tables(e.table.index-1).asAny, e.path.cast[Mapping, Mapping])
				case e =>
					throw new IllegalArgumentException(s"Cannot translate expression $e while substituting $this ($table -> $value): unknown table.")
			}
			joined filterBy translated
		}
*/


	/** Remove the right table from this join and apply an additional filter on the left side obtained from this instance's
	  * join  condition by replacing any path expressions rooted in the right table with a bound parameter which
	  * value is derived from the value for the right mapping.
	  */
/*
	def substitute(value :R#Subject) :L =
		left where SQLScribe.apply(joinCondition, this, left) {
			case e if e.table == lastTable =>
				e.asInstanceOf[PathFormula[L Join R, R, Mapping]].queryParameter(value)
			case p if p.table != lastTable =>
				p.asInstanceOf[PathFormula[L, Mapping, Mapping]]
//			case p =>
//				throw new IllegalArgumentException(s"Cannot parameterize expression $p in $this: last table is probably an abstract expression")
		}



	def parameterizeLast :L WithParam R#Subject = {
		val parameterized = left.withParam[R#Subject]//.asInstanceOf[L WithParam R#Subject]
		parameterized where (t => SQLScribe(joinCondition, this, parameterized) {
			case ComponentFormula(table, path) if table == lastTable =>
				implicit val form = path.end.selectForm && path.end.queryForm.asInstanceOf[SQLWriteForm[Any]]
				t.?[R#Subject].opt(param => path.cast[R, Mapping[Any]](param))
			case p =>
				p.asInstanceOf[PathFormula[L WithParam R#Subject, Mapping, Mapping]]
		})
	}



*/


//	def selectOne[T<:Mapping, C<:Mapping](mapping :JoinedTables[this.type]=>ComponentFormula[this.type, T, C]) :SelectMapping[this.type, C] =
//		SelectMapping(this :this.type, mapping(this :this.type))





/*

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Join[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case ref :AnyRef => ref eq this
		case join :Join[_, _] if canEqual(join) && join.canEqual(this) =>
			right == join.right && left == join.left && (condition isomorphic join.condition)
		case _ => false
	}

	override def hashCode :Int = left.hashCode * 31 + right.hashCode



	override def toString :String = s"$left join $lastTable" + (if (joinCondition == True) "" else " on " + joinCondition)

*/

	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case join :Join[_, _] =>
			(join eq this) || (join canEqual this) && join.left == left && join.lastTable == lastTable
		case _ => false
	}

	override def hashCode :Int = left.hashCode * 31 + table.hashCode



	protected def joinType :String

	override def toString :String =
		left.toString + " " + joinType + " " + right + (if (condition == True()) "" else " on " + condition)
}






/** a factory for 'FROM' clauses of SQL SELECT statements representing non-empty list of tables joined together. */
object Join {

	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: AnyComponent[O], R[O] <: AnyComponent[O]](left :RowSource[L], right :RowSource[R]) :From[L] InnerJoin R =
		InnerJoin(From(left), right)

	def apply[L <: FromClause, R[O] <: AnyComponent[O]](left :L, right :RowSource[R]) :L InnerJoin R =
		InnerJoin(left, right)



	def unapply[L <: FromClause, R[O] <: AnyComponent[O]](join :L Join R) :Option[(L, R[_])] = join match {
		case _ :InnerJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, AnyComponent[_])] =
		from match {
			case join :InnerJoin[_, r forSome { type r[O] <: AnyComponent[O] }] => Some(join.left -> join.right)
			case _ => None
		}







}






/** Base class for join implementations representing real SQL joins between relations, rather than a synthetic
  * `SubselectJoin` representing a subselect of another select expression.
  */
trait ProperJoin[+L <: FromClause, R[O] <: AnyComponent[O]] extends Join[L, R] { join =>

	def copy[F <: FromClause, T[O] <: AnyComponent[O]](left :F, right :RowSource[T]) :F Kind T

//	override def copy[F <: FromClause](left :F) :Self[F] = copy(left, table.source)


	type Kind[+F <: FromClause, T[O] <: AnyComponent[O]] <: (F ProperJoin T) {
		type Kind[+S <: FromClause, M[O] <: AnyComponent[O]] = join.Kind[S, M]
	}

	type This <: L Kind R// {
//		type This = join.This
//	}

	type Outer = left.Outer


	def outer :Outer = left.outer




/*
	override def transplant[O <: FromClause](target: O, rewriter: SQLScribe[Outer, O]): SubselectFrom[O] = {
		val transplanted = left.transplant(target, rewriter)
		val joined = copyJoin(transplanted)
		val filter = SQLScribe.subselect[Outer, this.type, O, joined.type, Boolean](condition, this, joined, rewriter)
		joined filterBy filter
	}



	override def copyJoin[S <: FromClause](source: S): S ProperJoin R = copyJoin(source, right)

	override def copyJoin[S <: FromClause, M <: Mapping](left: S, right: M): S ProperJoin M


	protected def copyJoin(replacement :FromFormula[L Join R, R], condition :BooleanFormula[L Join R] = True()) :Self[L]
*/



	/** Specify an alias for the last table in the join. This is not necessary and may be overriden in case of conflicts,
	  * but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the table as in 'from users as u'
	  * @return a new join isomorphic with this instance, but with a new last table (not equal to this.last).
	  */
/*
	def as(alias :String) :Self[L] = {
		val join = copyJoin(new FromFormula[FromClause, R](right, left.size, Some(alias))) //new Join[L, R](left, , True())
		val filter = SQLScribe(joinCondition, self, join.self) {
			case PathFormula(t, path) if t == lastTable => PathFormula(join.last, path.cast[R, Mapping])
			case e => e
		}
		join.self where filter
	}



	override def on(condition :(L#LastTable[this.type], FromFormula[this.type, R]) => BooleanFormula[this.type]) :Self[L] =
		super.on(condition).asInstanceOf[Self[L]]

*/


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ProperJoin[_, m forSome { type m[O] <: AnyComponent[O] }]]

}






/** A classic join of two SQL relations. Represents a cross join of all relations from the left side with the relation
  * on the right, narrowed down by the conjunction of this join's condition with the combined filter expression
  * of the left side.
  */
sealed trait InnerJoin[+L <: FromClause, R[O] <: AnyComponent[O]] extends ProperJoin[L, R] { join =>

	type Kind[+F <: FromClause, T[O] <: AnyComponent[O]] = F InnerJoin T

	override def copy[F <: FromClause](left :F) :F InnerJoin R = (left :FromClause) match {
		case Dual => From(table.source).asInstanceOf[F InnerJoin R]
		case _ => InnerJoin(left, table.source)
	}





	override def canEqual(that :Any) :Boolean = that.isInstanceOf[InnerJoin[_, m forSome { type m[O] <: AnyComponent[O] }]]

	protected override def joinType :String = "join"

}






object InnerJoin {

	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: AnyComponent[O], R[O] <: AnyComponent[O]](left :RowSource[L], right :RowSource[R]) :From[L] InnerJoin R =
		new CrossJoin(From(left), right)

	def apply[L <: FromClause, R[O] <: AnyComponent[O]](left :L, right :RowSource[R]) :L InnerJoin R =
		new CrossJoin(left, right)

	def unapply[L <: FromClause, R[O] <: AnyComponent[O]](join :L Join R) :Option[(L, R[_])] = join match {
		case _ :CrossJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, AnyComponent[_])] =
		from match {
			case join :CrossJoin[_, r forSome { type r[O] <: AnyComponent[O] }] => Some(join.left -> join.right)
			case _ => None
		}


	private class CrossJoin[L <: FromClause, R[O] <: AnyComponent[O]]
	                       (source :L, t :FromFormula[FromClause Join R, R], pred :BooleanFormula[L Join R])
		extends Join[L, R](source, t, pred) with InnerJoin[L, R]
	{
		private[sql] def this(left :L, right :RowSource[R], condition :BooleanFormula[L Join R] = True()) =
			this(left, new FromFormula[FromClause Join R, R](right, left.size), condition)

		override def copy(filter :BooleanFormula[L Join R]) :L CrossJoin R = new CrossJoin(left, table, filter)

		override def copy[F <: FromClause, T[O] <: AnyComponent[O]](left :F, right :RowSource[T]) :F InnerJoin T =
			new CrossJoin(left, right)


		type This = L CrossJoin R

		def self :L CrossJoin R = this
	}

}






sealed trait LeftJoin[+L <: FromClause, R[O] <: AnyComponent[O]] extends ProperJoin[L, R] {

	type Kind[+F <: FromClause, T[O] <: AnyComponent[O]] = F LeftJoin T

	override def copy[F <: FromClause](left :F) :F LeftJoin R = LeftJoin(left, table.source)

	override def copy[F <: FromClause, T[O] <: AnyComponent[O]](left :F, right :RowSource[T]) :F LeftJoin T =
		LeftJoin(left, right)




	protected override def joinType = "left join"
/*
	override def copyJoin[S <: FromClause, M <: Mapping](source: S, next :M): S LeftJoin M =
		new LeftJoin(source, next)

	override protected def copyJoin(replacement: FromFormula[L Join R, R],
	                                condition :BooleanFormula[L Join R] = True()): L LeftJoin R =
		new LeftJoin[L, R](left, replacement, condition)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[LeftJoin[_, _]] || that.isInstanceOf[From[_]]

	override def toString :String = left + " left join " + lastTable + (if (condition==True) "" else " on " + condition)
*/
}






object LeftJoin {

	/** Create a left outer join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: AnyComponent[O], R[O] <: AnyComponent[O]](left :RowSource[L], right :RowSource[R]) :From[L] LeftJoin R =
		new LeftOuterJoin(From(left), right)

	def apply[L <: FromClause, R[O] <: AnyComponent[O]](left :L, right :RowSource[R]) :L LeftJoin R =
		new LeftOuterJoin(left, right)

	def unapply[L <: FromClause, R[O] <: AnyComponent[O]](join :L Join R) :Option[(L, R[_])] = join match {
		case _ :LeftJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, AnyComponent[_])] =
		from match {
			case join :LeftJoin[_, r forSome { type r[O] <: AnyComponent[O] }] => Some(join.left -> join.right)
			case _ => None
		}



	private class LeftOuterJoin[L <: FromClause, R[O] <: AnyComponent[O]]
	                           (l :L, r :FromFormula[FromClause Join R, R], cond :BooleanFormula[L Join R] = True())
		extends Join[L, R](l, r, cond) with LeftJoin[L, R]
	{
		def this(left :L, right :RowSource[R]) = this(left, new FromFormula[FromClause Join R, R](right, left.size))

		override protected def copy(filter :BooleanFormula[L Join R]) :L LeftOuterJoin R =
			new LeftOuterJoin(left, table, filter)

		override def self :L LeftOuterJoin R = this

		override type This = L LeftOuterJoin R
	}
}






sealed trait RightJoin[+L <: FromClause, R[O] <: AnyComponent[O]] extends ProperJoin[L, R] {

	type Kind[+F <: FromClause, T[O] <: AnyComponent[O]] = F RightJoin T

	override def copy[F <: FromClause](left :F) :F RightJoin R = RightJoin(left, table.source)

	override def copy[F <: FromClause, T[O] <: AnyComponent[O]](left :F, right :RowSource[T]) :F RightJoin T =
		RightJoin(left, right)



	protected override def joinType = "right join"
/*
	override def copyJoin[S <: FromClause, M<:Mapping](source: S, next :M): S RightJoin M = new RightJoin(source, next)


	override protected def copyJoin(replacement: FromFormula[L Join R, R],
	                                condition :BooleanFormula[L Join R] = True()): L RightJoin R =
		new RightJoin[L, R](left, replacement, condition)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[RightJoin[_, _]] || that.isInstanceOf[From[_]]

	override def toString :String =
		left + " right join " + lastTable + (if (condition == True) "" else " on " + condition)
*/
}






object RightJoin {

	/** Create a right outer join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: AnyComponent[O], R[O] <: AnyComponent[O]](left :RowSource[L], right :RowSource[R]) :From[L] RightJoin R =
		new RightOuterJoin(From(left), right)

	def apply[L <: FromClause, R[O] <: AnyComponent[O]](left :L, right :RowSource[R]) :L RightJoin R =
		new RightOuterJoin(left, right)

	def unapply[L <: FromClause, R[O] <: AnyComponent[O]](join :L Join R) :Option[(L, R[_])] = join match {
		case _ :RightJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, AnyComponent[_])] =
		from match {
			case join :RightJoin[_, r forSome { type r[O] <: AnyComponent[O] }] => Some(join.left -> join.right)
			case _ => None
		}



	private class RightOuterJoin[L <: FromClause, R[O] <: AnyComponent[O]]
	                            (l :L, r :FromFormula[FromClause Join R, R], cond :BooleanFormula[L Join R] = True())
		extends Join[L, R](l, r, cond) with RightJoin[L, R]
	{
		def this(left :L, right :RowSource[R]) =
			this(left, new FromFormula[FromClause Join R, R](right, left.size))

		override protected def copy(filter :BooleanFormula[L Join R]) :L RightOuterJoin R =
			new RightOuterJoin(left, table, filter)

		override def self :L RightOuterJoin R = this

		override type This = L RightOuterJoin R
	}
}






/** A `FromClause` constituting of exactly one table or SQL relation.
  * This is just a bit of sugar for Join[Dual, T], so that we can write the type From[T] instead, especially
  * in larger clauses like `From[Children] Join Daemons`.
  */
class From[T[O] <: AnyComponent[O]] protected (from :FromFormula[FromClause Join T, T], filter :BooleanFormula[Dual Join T])
	extends Join[Dual, T](Dual, from, filter) with InnerJoin[Dual, T]
{
	private[sql] def this(source :RowSource[T], filter :BooleanFormula[FromClause Join T] = True()) =
		this(new FromFormula[FromClause Join T, T](source, 0), filter)


	type This = From[T]

	protected override def copy(filter :BooleanFormula[Dual Join T]) :From[T] = new From(table, filter)


	override def copy[L <: FromClause, R[O] <: AnyComponent[O]](left :L, right :RowSource[R]) :L InnerJoin R =
		left match {
			case Dual => new From(right).asInstanceOf[L InnerJoin R]
			case _ =>InnerJoin(left, right)
		}

	override def self :From[T] = this


	def mapping :T[Any] = right



	protected override def joinType :String = "from"

	override def toString :String =
		if (condition == True()) "from " + mapping
		else "from " + mapping + " where " + condition
/*

	def mapping :T = right



	override def copyJoin[S <: FromClause, M<:Mapping](source: S, right :M): S InnerJoin M =
		source.ifSubclassOf[Dual]{ _ => From(right).asInstanceOf[InnerJoin[S, M]] } getOrElse new InnerJoin(source, right)


	override protected def copyJoin(replacement: FromFormula[Dual Join T, T],
	                                condition :BooleanFormula[Dual Join T] = True()): From[T] =
		new From[T](replacement, condition)



	//	override def as(alias :String) :From[T] = {
	//		val join = new From[T](new JoinedTable[From[T], T](right, left.size, Some(alias)), True())
	//		val filter = SQLScribe[this.type, join.type](condition) {
	//			case PathExpression(t, path) if t==last => PathExpression(join.last, path.cast[T, Mapping])
	//			case e => e
	//		}
	//		join where (_ => filter)
	//	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ProperJoin[_, _]]



	override def toString :String = "from " + lastTable + (if (condition == True) "" else " where " + condition)
*/
}






/** A `FromClause` factory for both single table queries, and starting points for arbitrary join lists.
  * Example (see the `Join` class documentation for explanation of the filters):
  * {{{
  *     val users = From(Users) where (_(_.name)==="Jack")
  *     val guests = From(Hotels) join GuestsBook on (_(_.id)===_(_.hotelId)) join People on (_(_.guestId)===_(_.id))
  * }}}
  */
object From {

//	/** Create a `FromClause` for rows mapped by a single mapping */
//	def apply[M <: Mapping](mapping : M) :From[M] = new From(mapping)
//
//	def instance(mapping :Mapping) :From[mapping.type] = new From[mapping.type](mapping)
	def apply[M[O] <: AnyComponent[O]](source :RowSource[M]) :From[M] = new From(source)


	def unapply[M[O] <: AnyComponent[O]](from :Dual Join M) :Option[M[_]] = from match {
		case _ :From[t forSome { type t[O] <: AnyComponent[O] }] => Some(from.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[AnyComponent[_]] = from match {
		case f :From[t forSome { type t[O] <: AnyComponent[O] }] => Some(f.mapping)
		case _ => None
	}

}






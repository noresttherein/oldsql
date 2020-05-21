package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.schema.{Mapping, SQLWriteForm}
import net.noresttherein.oldsql.sql.FromClause.{SubselectFrom, TableFormula, WithParam}
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True



/** A join between an existing `FromClause`, representing a last or a joined list, and another mapping.
  * The given mapping doesn't have to represent a last at this point - it might be for example a last component
  * to be 'planted' in a particular last at a later point.
  *
  * Note that, as with all generic types taking exactly two arguments, it can be written in the infix notation:
  * `val usersGuns :From[Users] Join UserGuns Join Guns`. This class is covariant regarding its left side,
  * so a sequence of joined mappings `X0 J1 X1 J2 X2 .. JN XN &lt;: X0 Join X1 Join X2 ... Join XN`
  * if for all `JN &lt;: Join`.
  *
  * @param left a FromClause constituting a pre-existing join list of tables - may be empty (Dual).
  * @param table the right side of the join - representation of a last alias containing the joined mapping.
  * @param joinCondition the join condition joining the right side to the left side. It is not the complete filter 
  *                      condition, as it doesn't include any join conditions defined in the left side of this join.
  * @tparam L the left side of this join.
  * @tparam R the right side of this join.
  */
abstract class Join[+L <: FromClause, R <: Mapping] protected(
		val left :L, protected[this] val table :TableFormula[L Join R, R],
		protected[this] val joinCondition :BooleanFormula[L Join R])
	extends FromClause
{ source =>

	private[sql] def this(left :L, right :R, cond :BooleanFormula[L Join R] = True()) =
		this(left, new TableFormula[FromClause Join R, R](right, left.size), cond)

	type Row = left.Row ~ R#Subject
	type LastMapping = R
	type LastTable[F <: FromClause] = TableFormula[F, R]


	override val lastTable :TableFormula[this.type, R] = table

	def right :R = lastTable.mapping


	def condition :BooleanFormula[this.type] = joinCondition

	override def filteredBy :BooleanFormula[this.type] = left.filter.asPartOf(this) && joinCondition

	override def filterBy[S >: this.type <: FromClause](filter: BooleanFormula[S]): S =
		copyJoin(table, joinCondition && filter.downcast).asInstanceOf[S]

	override def row: HListFormula[this.type, R#Subject::left.Row] =
		lastTable :: left.row.asInstanceOf[HListFormula[this.type, left.Row]]

	override def mappings :Seq[Mapping] = right +: left.mappings

	override def size :Int = lastTable.index + 1

	override def occurrences(mapping: Mapping): Int =
		left.occurrences(mapping) + (if (mapping == right) 1 else 0)


	override def allTables :Seq[TableFormula[Join.this.type, _ <: Mapping]] = toTableStream.reverse

	override protected[sql] def toUntypedTableStream :Seq[TableFormula[FromClause, _ <: Mapping]] =
		asTables[FromClause].last +: left.toUntypedTableStream

	override def subselectTableStream: Seq[TableFormula[this.type, _ <: Mapping]] =
		asTables[FromClause].last +: left.subselectTableStream.asInstanceOf[Seq[TableFormula[this.type, _<:Mapping]]]

	override def headTable :TableFormula[this.type, _ <: Mapping] =
		if (size == 1) lastTable
		else left.head




	override def joinAny(source: FromClause): FromClause = source match {
		case _:Dual => this
		case j:Join[_, _] =>
			val join = j.copyJoin(joinAny(j.left))
			val replanter = new SQLScribe.Replanter[j.type, join.type](j, join) {
				override def map[A <: Mapping](table: TableFormula[j.type, A]): TableFormula[join.type, A] =
					(j.toStream zip join.toStream).find(_._1==table).map(_._2.asInstanceOf[TableFormula[join.type, A]]) getOrElse {
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

	protected def copyJoin(replacement :TableFormula[L Join R, R], condition :BooleanFormula[L Join R] = True()) :L Join R


	/** Apply a join condition to the last two tables in this chain. This works exactly like 'where', but
	  * instead of a single argument representing all joined tables, the filter function should take as its arguments
	  * the last two tables, i.e, the last last defined by the left side of this join, if any, and the right side
	  * of this join. Static type checking will enforce that this method can't be called on 'joins' where the left side 
	  * is empty (single last sources).
	  * @param condition a function creating a representation of an SQL expression from the passed aliases 
	  *                  for the joined tables.
	  */
	def on(condition :(L#LastTable[this.type], TableFormula[this.type, R]) => BooleanFormula[this.type]) :L Join R =
		filterBy(condition(left.last.asPartOf(this).asInstanceOf[L#LastTable[this.type]], lastTable))



	override def plant(prefix: PartialFunction[TableFormula[this.type, _ <: Mapping],
	                   ComponentPath[_ <: Mapping, _ <: Mapping]]): FromClause =
	{
		val cast = prefix.asInstanceOf[PartialFunction[TableFormula[left.type, _ <: Mapping], ComponentPath[_ <: Mapping, _ <: Mapping]]]
		val previous = left.plant(cast)
		val planted = copyJoin(previous, prefix.applyOrElse(lastTable, (_:Any) => ComponentPath.self(right)).start)
		val tableMapping = (allTables zip planted.allTables).toMap[TableFormula[this.type, _], TableFormula[planted.type, _]]

		def mapped(t :TableFormula[this.type, _], path :MappingPath[_, _]) =
			PathFormula(tableMapping(t).asInstanceOf[TableFormula[planted.type, Mapping]], path.unchecked)

		planted filterBy {
			SQLScribe(this, planted)(joinCondition) { expr =>
				val concat = prefix andThen ((_: (_ \**\ _)).unchecked ++ expr.path.unchecked)
				val prefixed = concat.applyOrElse(expr.table, (_:Any) => expr.path)
				mapped(expr.table, prefixed)
			}
		}
	}


	override def plantMatching(prefix: ComponentPath[_ <: Mapping, _ <: Mapping]): FromClause =
		plant{ case t :TableFormula[this.type, _] if t.mapping == prefix.start => prefix }


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


	override def substitute[T](table: TableFormula[this.type, _ <: Mapping[T]], value: T): FromClause =
		if (table == lastTable) substitute(value.asInstanceOf[R#Subject])
		else {
			val previous = left.substitute(table.asInstanceOf[TableFormula[left.type, Mapping[T]]], value)
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
					throw new IllegalArgumentException(s"Cannot translate expression $e while substituting $this ($table -> $value): unknown last.")
			}
			joined filterBy translated
		}


	/** Remove the right last from this join and apply an additional filter on the left side obtained from this instance's
	  * join  condition by replacing any path expressions rooted in the right last with a bound parameter which
	  * value is derived from the value for the right mapping.
	  */
	def substitute(value :R#Subject) :L =
		left where SQLScribe.apply(joinCondition, this, left) {
			case e if e.table == lastTable =>
				e.asInstanceOf[PathFormula[L Join R, R, Mapping]].queryParameter(value)
			case p if p.table != lastTable =>
				p.asInstanceOf[PathFormula[L, Mapping, Mapping]]
			//			case p =>
			//				throw new IllegalArgumentException(s"Cannot parameterize expression $p in $this: last last is probably an abstract expression")
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





	//	def selectOne[T<:Mapping, C<:Mapping](mapping :JoinedTables[this.type]=>ComponentFormula[this.type, T, C]) :SelectMapping[this.type, C] =
	//		SelectMapping(this :this.type, mapping(this :this.type))






	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Join[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case ref :AnyRef => ref eq this
		case join :Join[_, _] if canEqual(join) && join.canEqual(this) => 
			right == join.right && left == join.left && (condition isomorphic join.condition)
		case _ => false
	}

	override def hashCode :Int = left.hashCode * 31 + right.hashCode

	
	
	override def toString :String = s"$left join $lastTable" + (if (joinCondition == True) "" else " on " + joinCondition)

}






/** a factory for 'FROM' clauses of SQL SELECT statements representing non-empty list of tables joined together. */
object Join {
	type OuterJoin[+L <: FromClause, R <: Mapping] = LeftJoin[L, R]

	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a last or some last proxy.
	  */
	def apply[L <: FromClause, R <: Mapping](left: L, right: R): L InnerJoin R =
		new InnerJoin(left, right)


	def unapply[L <: FromClause, R <: Mapping](join: L Join R): Some[(L, R)] = Some(join.left, join.right)

	def unapply(source: FromClause): Option[(FromClause, Mapping)] =
		source.ifSubclass[FromClause Join (_ <: Mapping)] { j => (j.left, j.right) }

}






abstract class ProperJoin[+L <: FromClause, R <: Mapping] protected
		(source :L, t :TableFormula[L Join R, R], filter :BooleanFormula[L Join R])
	extends Join[L, R](source, t, filter) //with AsSubselectOf[L#Outer]
{ join =>
	type Outer = left.Outer

	def outer :Outer = left.outer

	type Self[+F <: FromClause] <: (F ProperJoin R) {
		type Self[+S <: FromClause] = join.Self[S]
	}

	def self :Self[L]



	override def transplant[O <: FromClause](target: O, rewriter: SQLScribe[Outer, O]): AsSubselectOf[O] = {
		val transplanted = left.transplant(target, rewriter)
		val joined = copyJoin(transplanted)
		val filter = SQLScribe.subselect[Outer, this.type, O, joined.type, Boolean](condition, this, joined, rewriter)
		joined filterBy filter
	}



	override def copyJoin[S <: FromClause](source: S): S ProperJoin R = copyJoin(source, right)

	override def copyJoin[S <: FromClause, M <: Mapping](left: S, right: M): S ProperJoin M


	protected def copyJoin(replacement :TableFormula[L Join R, R], condition :BooleanFormula[L Join R] = True()) :Self[L]



	/** Specify an alias for the last last in the join. This is not necessary and may be overriden in case of conflicts,
	  * but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the last as in 'from users as u'
	  * @return a new join isomorphic with this instance, but with a new last last (not equal to this.last).
	  */
	def as(alias :String) :Self[L] = {
		val join = copyJoin(new TableFormula[FromClause, R](right, left.size, Some(alias))) //new Join[L, R](left, , True())
		val filter = SQLScribe(joinCondition, self, join.self) {
			case PathFormula(t, path) if t == lastTable => PathFormula(join.last, path.cast[R, Mapping])
			case e => e
		}
		join.self where filter
	}



	override def on(condition :(L#LastTable[this.type], TableFormula[this.type, R]) => BooleanFormula[this.type]) :Self[L] =
		super.on(condition).asInstanceOf[Self[L]]



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ProperJoin[_, _]]

}






class InnerJoin[+L <: FromClause, R <: Mapping] protected
		(source :L, t :TableFormula[L Join R, R], filter :BooleanFormula[L Join R])
	extends ProperJoin[L, R](source, t, filter)
{

	private[sql] def this(left :L, right :R, condition :BooleanFormula[L Join R] = True()) =
		this(left, new TableFormula[FromClause Join R, R](right, left.size), condition)


	type Self[+F <: FromClause] = F InnerJoin R

	override def self :L InnerJoin R = this



	override def copyJoin[S <: FromClause, M <: Mapping](left: S, right: M): S InnerJoin M =
		new InnerJoin(left, right)

	override protected def copyJoin(replacement: TableFormula[L Join R, R], condition: BooleanFormula[L Join R]=True()): L InnerJoin R =
		new InnerJoin[L, R](left, replacement, condition)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[InnerJoin[_, _]]

}



object InnerJoin {
	def apply[L <: Mapping, R <: Mapping](left :L, right :R) :From[L] Join R =
		new InnerJoin(From(left), right)

	def unapply(source: FromClause): Option[(FromClause, Mapping)] =
		source.ifSubclass[FromClause InnerJoin (_ <: Mapping)] { j => (j.left, j.right) }

}






class LeftJoin[+L <: FromClause, R <: Mapping] protected
		(source :L, table :TableFormula[L Join R, R], filter :BooleanFormula[L Join R])
	extends ProperJoin[L, R](source, table, filter)
{

	private[sql] def this(left :L, right :R, condition :BooleanFormula[L Join R]=True) =
		this(left, new TableFormula[FromClause Join R, R](right, left.size), condition)


	type Self[+F <: FromClause] = F LeftJoin R

	override def self :L LeftJoin R = this


	override def copyJoin[S <: FromClause, M <: Mapping](source: S, next :M): S LeftJoin M =
		new LeftJoin(source, next)

	override protected def copyJoin(replacement: TableFormula[L Join R, R],
	                                condition :BooleanFormula[L Join R] = True()): L LeftJoin R =
		new LeftJoin[L, R](left, replacement, condition)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[LeftJoin[_, _]] || that.isInstanceOf[From[_]]

	override def toString :String = left + " left join " + lastTable + (if (condition==True) "" else " on " + condition)
}



object LeftJoin {
	def apply[L <: FromClause, R <: Mapping](left :L, right :R) :L LeftJoin R =
		new LeftJoin(left, right)

	def unapply[L <: FromClause, R <: Mapping](join :L Join R) :Option[(L, R)] =
		join.ifSubclass[L LeftJoin R] { j => (j.left, j.right) }

	def unapply(source :FromClause) :Option[(FromClause, Mapping)] =
		source.ifSubclass[FromClause LeftJoin (_ <: Mapping)] { j => (j.left, j.right) }

}






class RightJoin[+L <: FromClause, R <: Mapping] protected
		(source :L, table :TableFormula[L Join R, R], filter :BooleanFormula[L Join R])
	extends ProperJoin[L, R](source, table, filter)
{

	private[sql] def this(left :L, right :R, condition :BooleanFormula[L Join R]=True) =
		this(left, new TableFormula[FromClause Join R, R](right, left.size), condition)

	type Self[+F <: FromClause] = F RightJoin R

	def self :L RightJoin R = this


	override def copyJoin[S <: FromClause, M<:Mapping](source: S, next :M): S RightJoin M = new RightJoin(source, next)


	override protected def copyJoin(replacement: TableFormula[L Join R, R],
	                                condition :BooleanFormula[L Join R] = True()): L RightJoin R =
		new RightJoin[L, R](left, replacement, condition)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[RightJoin[_, _]] || that.isInstanceOf[From[_]]

	override def toString :String =
		left + " right join " + lastTable + (if (condition == True) "" else " on " + condition)
}



object RightJoin {
	def apply[L <: FromClause, R <: Mapping](left :L, right :R) :L RightJoin R =
		new RightJoin(left, right)

	def unapply[L <: FromClause, R <: Mapping](join :L Join R) :Option[(L, R)] =
		join.ifSubclass[L RightJoin R] { j => (j.left, j.right) }

	def unapply(source :FromClause) :Option[(FromClause, Mapping)] =
		source.ifSubclass[FromClause RightJoin (_ <: Mapping)] { j => (j.left, j.right) }

}






/** A FromClause constituting of exactly one last or last-like object.
  * This is just a bit of sugar for Join[Dual, T], so that we can write the type From[T] instead
  * (and in particular, rather niftly, From[Children] Join Daemons).
  */
class From[T <: Mapping] protected (table :TableFormula[Dual Join T, T], filter :BooleanFormula[Dual Join T])
	extends InnerJoin[Dual, T](Dual, table, filter)
{
	private[sql] def this(mapping :T, filter :BooleanFormula[FromClause Join T]=True) =
		this(new TableFormula[FromClause Join T, T](mapping, 0), filter)


	override def self :From[T] = this

	def mapping :T = right



	override def copyJoin[S <: FromClause, M<:Mapping](source: S, right :M): S InnerJoin M =
		source.ifSubclassOf[Dual]{ _ => From(right).asInstanceOf[InnerJoin[S, M]] } getOrElse new InnerJoin(source, right)


	override protected def copyJoin(replacement: TableFormula[Dual Join T, T],
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

}






/** A `FromClause` factory for both single last queries, and starting points for arbitrary join lists.
  * Example (see the `Join` class documentation for explanation of the filters):
  * {{{
  *     val users = From(Users) where (_(_.name)==="Jack")
  *     val guests = From(Hotels) join GuestsBook on (_(_.id)===_(_.hotelId)) join People on (_(_.guestId)===_(_.id))
  * }}}
  */
object From {
	
	/** Create a `FromClause` for rows mapped by a single mapping */
	def apply[M <: Mapping](mapping : M) :From[M] = new From(mapping)

	def instance(mapping :Mapping) :From[mapping.type] = new From[mapping.type](mapping)

	
	
	def unapply[M<:Mapping](source :Dual Join M) :Option[M] =
		source.ifSubclass[From[M]] { _.mapping }

	def unapply(source :FromClause) :Option[Mapping] =
		source.ifSubclass[From[_<:Mapping]] { _.mapping }

}



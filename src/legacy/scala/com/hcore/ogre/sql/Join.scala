package com.hcore.ogre.sql

import com.hcore.ogre.mapping.ComponentPath._
import com.hcore.ogre.mapping.{Mapping, MappingPath, ComponentPath, AnyMapping}
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.slang.{SubtypeOf, TypeBox, SaferCasts}
import com.hcore.ogre.sql.RowSource._
import com.hcore.ogre.sql.SQLFormula._


//implicits
import SaferCasts._
import extensions._
import shapeless.::





/** A join between an existing row source, representing a table or a joined list, and another mapping.
  * The given mapping doesn't have to represent a table at this point - it might be for example a
  * table component, to be 'planted' in a particular table at a later point.
  *
  * Note that, as with all generic types taking exactly two arguments, it can be written in an infix manner:
  * <code>val usersGuns :From[Users] Join UserGuns Join Guns</code>. This class is covariant regarding its left side,
  * so, a sequence of joined mappings X0 J1 X1 J2 X2 .. JN XN <:< X0 Join X1 Join X2 ... Join XN if for all JN <:< Join.
  *
  * @param left RowSource constituting pre-existing join list of tables - may be empty (Dual)
  * @param table right side of the join - representation of a table alias containing the joined mapping.
  * @param joinCondition join condition joining the right side to the left side. It is not the complete filter condition,
  *             as it doesn't include any join conditions defined in the left side of this join.
  * @tparam L Left side of this join.
  * @tparam R Right side of this join.
  */
abstract class Join[+L<:RowSource, R<:AnyMapping] protected (
		val left :L, protected[this] val table :TableFormula[L Join R, R],
		protected[this] val joinCondition :BooleanFormula[L Join R])
	extends RowSource
{ source =>

	private[sql] def this(left :L, right :R, cond :BooleanFormula[L Join R]=True()) =
		this(left, new TableFormula[RowSource Join R, R](right, left.size), cond)

	type Row = R#ResultType::left.Row
	type LastMapping = R
	type LastTable[S<:RowSource] = TableFormula[S, R]


	override val lastTable :TableFormula[this.type, R] = table

	def right = lastTable.mapping


	def condition :BooleanFormula[this.type] = joinCondition

	override def filteredBy :BooleanFormula[this.type] = left.filter.asPartOf(this) && joinCondition

	override def filterBy[S >: this.type <: RowSource](filter: BooleanFormula[S]): S =
		copyJoin(table, joinCondition && filter.downcast).asInstanceOf[S]

	override def row: HListFormula[this.type, R#ResultType::left.Row] =
		lastTable :: left.row.asInstanceOf[HListFormula[this.type, left.Row]]

	override def mappings = right +: left.mappings

	override def size = lastTable.index +1

	override def occurences(mapping: AnyMapping): Int =
		left.occurences(mapping) + (if (mapping==right) 1 else 0)


	override def allTables = toTableStream.reverse//last +: left.all.map(_.asInstanceOf[JoinedTable[L, Mapping]].joinedWith[R])

	override protected[sql] def toUntypedTableStream = asTables[RowSource].last +: left.toUntypedTableStream

	override def subselectTableStream: Seq[TableFormula[this.type, _ <: AnyMapping]] =
		asTables[RowSource].last +: left.subselectTableStream.asInstanceOf[Seq[TableFormula[this.type, _<:AnyMapping]]]

	override def headTable :TableFormula[this.type, _<:AnyMapping] =
		if (size==1) lastTable
		else left.head




	override def joinAny(source: RowSource): RowSource = source match {
		case _:Dual => this
		case j:Join[_, _] =>
			val join = j.copyJoin(joinAny(j.left))
			val replanter = new SQLScribe.Replanter[j.type, join.type](j, join) {
				override def map[A <: AnyMapping](table: TableFormula[j.type, A]): TableFormula[join.type, A] =
					(j.toStream zip join.toStream).find(_._1==table).map(_._2.asInstanceOf[TableFormula[join.type, A]]) getOrElse {
						throw new IllegalArgumentException(s"unrecognized table $table when cross joining $this and $j")
					}
			}
			join filterBy replanter(j.condition)
	}





	/** Perform a join between S and R, preserving this join's nature and class (inner, outer), but discarding any filter associated with this instance. */
	def copyJoin[S<:RowSource](source :S) :S Join R = copyJoin(source, right)

	def copyJoin[S<:RowSource, M<:AnyMapping](left :S, right :M) :S Join M //= Join(left, right)

	protected def copyJoin(replacement :TableFormula[L Join R, R], condition :BooleanFormula[L Join R]=True()) :L Join R


	/** Apply a join condition between the last two tables in this chain. This works exactly like 'where', but
	  * instead of a single argument representing all joined tables, the filter function should take as its arguments
	  * the last two tables, i.e, the last table defined by the left side of this join, if any, and the right side of this join.
	  * Static type checking will enforce that this method can't be called on 'joins' where left side is empty (single table sources).
	  * @param condition a function creating a representation of a sql expression from the passed aliases for the joined tables.
	  * @return
	  */
	def on(condition :(L#LastTable[this.type], TableFormula[this.type, R]) => BooleanFormula[this.type]) :L Join R =
		filterBy(condition(left.last.asPartOf(this).asInstanceOf[L#LastTable[this.type]], lastTable))



	override def plant(prefix: PartialFunction[TableFormula[this.type, _<:AnyMapping], ComponentPath[_<:AnyMapping, _<:AnyMapping]]): RowSource = {
		val previous = left.plant(prefix.asInstanceOf[PartialFunction[TableFormula[left.type, _<:AnyMapping], ComponentPath[_<:AnyMapping, _<:AnyMapping]]])
		val planted = copyJoin(previous, prefix.applyOrElse(lastTable, (_:Any) => ComponentPath.self(right)).start)
		val tableMapping = (allTables zip planted.allTables).toMap[TableFormula[this.type, _], TableFormula[planted.type, _]]

		def mapped(t :TableFormula[this.type, _], path :MappingPath[_, _]) =
			PathFormula(tableMapping(t).asInstanceOf[TableFormula[planted.type, AnyMapping]], path.unchecked)

		planted filterBy {
			SQLScribe(this, planted)(joinCondition) { expr =>
				val concat = prefix andThen ((_: (_ \**\ _)).unchecked ++ expr.path.unchecked)
				val prefixed = concat.applyOrElse(expr.table, (_:Any) => expr.path)
				mapped(expr.table, prefixed)
			}
		}
	}


	override def plantMatching(prefix: ComponentPath[_<:AnyMapping, _<:AnyMapping]): RowSource =
		plant{ case t :TableFormula[this.type, _] if t.mapping==prefix.start => prefix }


	def plantLast[T<:AnyMapping](path :ComponentPath[T, R]) :L Join T = path match {
		case SelfPath(m) if m==right =>
			this.asInstanceOf[L Join T]
		case _ =>
			val planted = copyJoin(left, path.start)
			planted where SQLScribe(joinCondition, this, planted) {
				case last if last.table==Join.this.last =>
					PathFormula(planted.last, path++last.path.cast[R, AnyMapping])
				case prev => prev.asInstanceOf[SQLFormula[L Join T, _]]
			}
	}


	override def substitute[T](table: TableFormula[this.type, _<:Mapping[T]], value: T): RowSource =
		if (table==lastTable) substitute(value.asInstanceOf[R#ResultType])
		else {
			val previous = left.substitute(table.asInstanceOf[TableFormula[left.type, Mapping[T]]], value)
			val joined = copyJoin(previous, right)
			val tables = joined.all.toIndexedSeq
			val translated = SQLScribe(this, joined)(joinCondition) {
				case e if e.table==table =>
					e.asInstanceOf[PathFormula[this.type, Mapping[T], AnyMapping]].queryParameter(value)
				case e if e.table.index<table.index =>
					e.asInstanceOf[PathFormula[joined.type, AnyMapping, AnyMapping]]
				case e if e.table.index>table.index =>
					PathFormula(tables(e.table.index-1).asAny, e.path.cast[AnyMapping, AnyMapping])
				case e =>
					throw new IllegalArgumentException(s"Cannot translate expression $e while substituting $this ($table -> $value): unknown table.")
			}
			joined filterBy translated
		}


	/** Remove the right table from this join and apply an additional filter on the left side obtained from this instance's
	  * join  condition by replacing any path expressions rooted in the right table with a bound parameter which
	  * value is derived from the value for the right mapping.
	  */
	def substitute(value :R#ResultType) :L =
		left where SQLScribe.apply(joinCondition, this, left) {
			case e if e.table == lastTable =>
				e.asInstanceOf[PathFormula[L Join R, R, AnyMapping]].queryParameter(value)
			case p if p.table != lastTable =>
				p.asInstanceOf[PathFormula[L, AnyMapping, AnyMapping]]
			//			case p =>
			//				throw new IllegalArgumentException(s"Cannot parameterize expression $p in $this: last table is probably an abstract expression")
		}



	def parameterizeLast :L WithParam R#ResultType = {
		val parameterized = left.withParam[R#ResultType]//.asInstanceOf[L WithParam R#ResultType]
		parameterized where (t => SQLScribe(joinCondition, this, parameterized) {
			case ComponentFormula(table, path) if table == lastTable =>
				implicit val form = path.end.selectForm && path.end.queryForm.asInstanceOf[SQLWriteForm[Any]]
				t.?[R#ResultType].opt(param => path.cast[R, Mapping[Any]](param))
			case p =>
				p.asInstanceOf[PathFormula[L WithParam R#ResultType, AnyMapping, AnyMapping]]
		})
	}





//	def selectOne[T<:Mapping, C<:Mapping](mapping :JoinedTables[this.type]=>ComponentFormula[this.type, T, C]) :SelectMapping[this.type, C] =
//		SelectMapping(this :this.type, mapping(this :this.type))




	override def toString = s"$left join $lastTable" + (if (joinCondition==True) "" else s" on $joinCondition")


	override def canEqual(that :Any) = that.isInstanceOf[Join[_, _]]

	override def equals(that :Any) = PartialFunction.cond(that) {
		case join :Join[_, _] => (this eq join) || (join.canEqual(this) && right==join.right && left==join.left && (condition isomorphic join.condition))
	}


	override def hashCode = (left, right).hashCode
}




/** Factory for 'FROM' clauses of SQL SELECT statements representing non-empty list of tables joined together. */
object Join {
	type OuterJoin[+L<:RowSource, R<:AnyMapping] = LeftJoin[L, R]

	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L <: RowSource, R <: AnyMapping](left: L, right: R): L InnerJoin R =
		new InnerJoin(left, right)


	def unapply[L <: RowSource, R <: AnyMapping](join: L Join R): Some[(L, R)] = Some(join.left, join.right)

	def unapply(source: RowSource): Option[(RowSource, AnyMapping)] =
		source.ifSubclass[RowSource Join (_ <: AnyMapping)] { j => (j.left, j.right) }

}




abstract class ExplicitJoin[+L<:RowSource, R<:AnyMapping] protected (
		source :L, t :TableFormula[L Join R, R], filter :BooleanFormula[L Join R])
	extends Join[L, R](source, t, filter) //with SubsourceOf[L#Parent]
{ join =>
	type Parent = left.Parent

	def parent = left.parent

	type Self[+S <:RowSource] <: (S ExplicitJoin R) {
		type Self[+O <:RowSource] = join.Self[O]
	}
	def self :Self[L]

	override def transplant[O <: RowSource](target: O, rewriter: SQLScribe[Parent, O]): SubsourceOf[O] = {
		val transplanted = left.transplant(target, rewriter)
		val joined = copyJoin(transplanted)
		val filter = SQLScribe.subselect[Parent, this.type, O, joined.type, Boolean](condition, this, joined, rewriter)
		joined filterBy filter
	}

	override def copyJoin[S <: RowSource](source: S): S ExplicitJoin R = copyJoin(source, right)

	override def copyJoin[S <: RowSource, M <: AnyMapping](left: S, right: M): S ExplicitJoin M


	protected def copyJoin(replacement :TableFormula[L Join R, R], condition :BooleanFormula[L Join R]=True()) :Self[L] // =
	//		new Join[L, R](left, replacement, True())


	/** Specify an alias for the last table in the join. This is not necessary and may be overriden in case of conflicts,
	  * but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the table as in 'from users as u'
	  * @return a new join isomorphic with this instance, but with a new last table (not equal to this.last).
	  */
	def as(alias :String) :Self[L] = {
		val join = copyJoin(new TableFormula[RowSource, R](right, left.size, Some(alias))) //new Join[L, R](left, , True())
		val filter = SQLScribe(joinCondition, self, join.self) {
				case PathFormula(t, path) if t==lastTable => PathFormula(join.last, path.cast[R, AnyMapping])
				case e => e
			}
		join.self where filter
	}



	/** Apply a join condition between the last two tables in this chain. This works exactly like 'where', but
	  * instead of a single argument representing all joined tables, the filter function should take as its arguments
	  * the last two tables, i.e, the last table defined by the left side of this join, if any, and the right side of this join.
	  * Static type checking will enforce that this method can't be called on 'joins' where left side is empty (single table sources).
	  * @param condition a function creating a representation of a sql expression from the passed aliases for the joined tables.
	  * @return
	  */
	override def on(condition :(L#LastTable[this.type], TableFormula[this.type, R]) => BooleanFormula[this.type]) :Self[L] =
		super.on(condition).asInstanceOf[Self[L]]
//		filterBy(condition(left.last.asPartOf(this).asInstanceOf[L#LastTable[this.type]], last)).self



	override def canEqual(that :Any) = that.isInstanceOf[ExplicitJoin[_, _]]
}





class InnerJoin[+L<:RowSource, R<:AnyMapping] protected (
		source :L, t :TableFormula[L Join R, R], filter :BooleanFormula[L Join R])
	extends ExplicitJoin[L, R](source, t, filter)
{

	type Self[+S <:RowSource] = S InnerJoin R

	def self = this

	private[sql] def this(left :L, right :R, condition :BooleanFormula[L Join R]=True()) =
		this(left, new TableFormula[RowSource Join R, R](right, left.size), condition)


	override def copyJoin[S <: RowSource, M <: AnyMapping](left: S, right: M): S InnerJoin M =
		new InnerJoin(left, right)

	override protected def copyJoin(replacement: TableFormula[L Join R, R], condition: BooleanFormula[L Join R]=True()): L InnerJoin R =
		new InnerJoin[L, R](left, replacement, condition)

	override def canEqual(that :Any) = that.isInstanceOf[InnerJoin[_, _]]
}


object InnerJoin {
	def apply[L<:AnyMapping, R<:AnyMapping](left :L, right :R) :From[L] Join R =
		new InnerJoin(From(left), right)

	def unapply(source: RowSource): Option[(RowSource, AnyMapping)] =
		source.ifSubclass[RowSource InnerJoin (_ <: AnyMapping)] { j => (j.left, j.right) }

}







class LeftJoin[+L<:RowSource, R<:AnyMapping] protected (
		source :L, table :TableFormula[L Join R, R], filter :BooleanFormula[L Join R])
	extends ExplicitJoin[L, R](source, table, filter)
{

	type Self[+S <:RowSource] = S LeftJoin R

	def self = this

	private[sql] def this(left :L, right :R, condition :BooleanFormula[L Join R]=True) =
		this(left, new TableFormula[RowSource Join R, R](right, left.size), condition)


	override def copyJoin[S <: RowSource, M<:AnyMapping](source: S, next :M): S LeftJoin M = LeftJoin(source, next)

	override protected def copyJoin(replacement: TableFormula[L Join R, R], condition :BooleanFormula[L Join R]=True()): L LeftJoin R =
		new LeftJoin[L, R](left, replacement, condition)


	override def canEqual(that :Any) = that.isInstanceOf[LeftJoin[_, _]] || that.isInstanceOf[From[_]]

	override def toString = left + " left join "+lastTable + (if (condition==True) "" else " on "+condition)
}



object LeftJoin {
	def apply[L<:RowSource, R<:AnyMapping](left :L, right :R) :L LeftJoin R =
		new LeftJoin(left, right)

	def unapply[L<:RowSource, R<:AnyMapping](join :L Join R) :Option[(L, R)] =
		join.ifSubclass[L LeftJoin R] { j => (j.left, j.right) }

	def unapply(source :RowSource) :Option[(RowSource, AnyMapping)] =
		source.ifSubclass[RowSource LeftJoin (_<:AnyMapping)] { j=> (j.left, j.right) }

}




class RightJoin[+L<:RowSource, R<:AnyMapping] protected (
		source :L, table :TableFormula[L Join R, R], filter :BooleanFormula[L Join R])
	extends ExplicitJoin[L, R](source, table, filter)
{

	type Self[+S <:RowSource] = RightJoin[S, R]

	def self = this

	private[sql] def this(left :L, right :R, condition :BooleanFormula[L Join R]=True) =
		this(left, new TableFormula[RowSource Join R, R](right, left.size), condition)


	override def copyJoin[S <: RowSource, M<:AnyMapping](source: S, next :M): S RightJoin M = RightJoin(source, next)


	override protected def copyJoin(replacement: TableFormula[L Join R, R], condition :BooleanFormula[L Join R]=True()): L RightJoin R =
		new RightJoin[L, R](left, replacement, condition)


	override def canEqual(that :Any) = that.isInstanceOf[RightJoin[_, _]] || that.isInstanceOf[From[_]]

	override def toString = left + " right join "+lastTable + (if (condition==True) "" else " on "+condition)
}



object RightJoin {
	def apply[L<:RowSource, R<:AnyMapping](left :L, right :R) :L RightJoin R =
		new RightJoin(left, right)

	def unapply[L<:RowSource, R<:AnyMapping](join :L Join R) :Option[(L, R)] =
		join.ifSubclass[L RightJoin R] { j => (j.left, j.right) }

	def unapply(source :RowSource) :Option[(RowSource, AnyMapping)] =
		source.ifSubclass[RowSource RightJoin (_<:AnyMapping)] { j=> (j.left, j.right) }

}



/** A RowSource constituting of exactly one table or table-like object.
  * This is just a bit of sugar for Join[Dual, T], so that we can write the type From[T] instead
  * (and in particular, rather niftly, From[Children] Join Daemons).
  */
class From[T<:AnyMapping] protected (table :TableFormula[Dual Join T, T], filter :BooleanFormula[Dual Join T])
	extends InnerJoin[Dual, T](Dual, table, filter)
{

	override def self = this

	private[sql] def this(mapping :T, filter :BooleanFormula[RowSource Join T]=True) =
		this(new TableFormula[RowSource Join T, T](mapping, 0), filter)

	def mapping = right



	override def copyJoin[S <: RowSource, M<:AnyMapping](source: S, right :M): S InnerJoin M =
		source.ifSubclassOf[Dual]{ _ => From(right).asInstanceOf[InnerJoin[S, M]] } getOrElse new InnerJoin(source, right)


	override protected def copyJoin(replacement: TableFormula[Dual Join T, T], condition :BooleanFormula[Dual Join T]=True()): From[T] =
		new From[T](replacement, condition)



//	override def as(alias :String) :From[T] = {
//		val join = new From[T](new JoinedTable[From[T], T](right, left.size, Some(alias)), True())
//		val filter = SQLScribe[this.type, join.type](condition) {
//			case PathExpression(t, path) if t==last => PathExpression(join.last, path.cast[T, Mapping])
//			case e => e
//		}
//		join where (_ => filter)
//	}



	override def canEqual(that :Any) = that.isInstanceOf[ExplicitJoin[_, _]]
	

	override def toString = s"from $lastTable" + (if (condition==True) "" else s" where $condition")
}


/** Row source factory for both single table-queries, and starting points for arbitrary join lists.
  * Example (see Join class documentation for explanation of the filters):
  * <code>
  *     val users = From(Users) where (_(_.name)==="Jack")
  *     val guests = From(Hotels) join GuestsBook on (_(_.id)===_(_.hotelId)) join People on (_(_.guestId)===_(_.id))
  * </code>
  */
object From {
	/** Create a row source for the rows mapped by a single mapping */
	def apply[M<:AnyMapping](mapping : M) :From[M] = new From(mapping)
	//		def apply[M<:Mapping](source :MappingSource[M]) = new From(source())

	def instance(mapping :AnyMapping) :From[mapping.type] = new From[mapping.type](mapping)

	def unapply[M<:AnyMapping](source :Dual Join M) :Option[M] =
		source.ifSubclass[From[M]] { _.mapping }


	def unapply(source :RowSource) :Option[AnyMapping] =
		source.ifSubclass[From[_<:AnyMapping]] { _.mapping }
}



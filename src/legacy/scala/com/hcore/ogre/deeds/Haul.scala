package com.hcore.ogre.deeds

import com.hcore.ogre.mapping.ComponentPath._
import com.hcore.ogre.mapping.MappingPath.{SplitFirst, \~\}
import com.hcore.ogre.mapping._
import com.hcore.ogre.mapping.ties.JoinPath
import com.hcore.ogre.model.ComposedOf.Arity
import com.hcore.ogre.model.ComposedOf.Arity.AtMostOne
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.RowSource._
import com.hcore.ogre.sql.SQLFormula.{BooleanFormula, ComponentFormula, PathFormula}
import com.hcore.ogre.sql._


//implicits
import extensions._
import com.hcore.ogre.slang.SaferCasts._



/** An extendable collection of sql selects (possibly with a single element) used to fetch data specified
  * by a collection of MappingPath instances sharing the same root. It represents a realisation of requests
  * in the form 'fetch me entities E from mapping (possibly last) H, including in the result following
  * optional column or related entities (including transitive relations).
  *
  * This class provides both the batch to be executed and a way of adding new data to the batch; In that way,
  * the process of creating a Haul will usually be additive, where a first instance is created for the root mapping H,
  * and any subsequent components/tables to be included can be specified by chained calls of 'fetch':
  * <code>
  *     Haul(Armies).fetch(Armies \ (_.command) \ (_.general)).fetch(Armies \ (_.orks)).fetch(Armies \ (_.trolls))
  * </code>
  */
trait Haul[H<:AnyMapping] {

	/** Add a new data source to be included in the haul.
	  * @param path a path to the mapping for the requested data, either an optional component not normally included in selects,
	  *             a foreign key to related last or a combination of thereof.
	  * @tparam M mapping type for requested data.
	  * @return a Haul being the sum of sources contained in this instance and any needed to provide the requested data.
	  */
	def fetch[M<:AnyMapping](path :MappingPath[H, M]) :Haul[H]

	/** Is this path explicitly included in this instance? Note that despite returning false,
	  * the data might be returned by the haul anyway as a result of implicit/transitive inclusion:
	  * for example, when a parent component (prefix of this path) was requested and this path represents its mandatory component,
	  * or when a subcomponent of this path (for which this path is a proper prefix) was requested and all transitive components
	  * are needed in order to include their data in the final result.
	  */
	def included[M<:AnyMapping](path :MappingPath[H, M]) :Boolean

	def filter(src :From[H], condition :BooleanFormula[From[H]]) :Haul[H]

	/** Row Source for the main select, which includes the mapping H itself (usually as the first element of the join). */
	def mainSource :RowSource

	def sources :Seq[RowSource] = Seq(mainSource)
}





object Haul {
	


	/** A Haul instance which is responsible for fetching relationships of some parent Haul using source L.
	  * For example, a parent Haul[Users] for From[Users] might need to fetch pets and guns of the returned users and use
	  * separate queries in order to avoid cross joins. In that case, it would create DerivedHaul[From[Users], Pets]
	  * and DerivedHaul[From[Users], Guns] to delegate managing those queries. Those derived instances would need the
	  * original query to filter their results, probably by some sort of subselect.
	  * @tparam L RowSource for the parent query
	  * @tparam H mapping for values returned by this instance.
	  */
	trait DerivedHaul[L<:RowSource, H<:AnyMapping] extends Haul[H] {
		/** Copy constructor substituting original parent query for a new one, being an extension of the original.
		  * If the parent Haul has further filtered down its result set, it needs to propage that filter to
		  * the derivative hauls.
		  */
		def asPartOf[S<:RowSource](source :S)(implicit ev :L ExtendedBy S) :DerivedHaul[S, H]

		def fetch[M<:AnyMapping](path :MappingPath[H, M]) :DerivedHaul[L, H]
	}


	def apply[H<:Mapping[T], T](mapping :H) :Haul[H] = {
		val from = From(mapping)
		new JoinWithJoinedSubselects[From[H], H](from, TablesIndex(from))
	}






	abstract class AbstractJoinHaul[R<:RowSource, H<:AnyMapping] protected (
			val source :R, 
			protected val tables :TablesIndex[R, H],
			protected val derivatives :DerivedHauls[R, H])	                                                      
		extends Haul[H]
	{ haul =>
		protected def this(source :R, tables :TablesIndex[R, H], derivative :(MappingPath[H, M], DerivedHaul[R, M]) forSome { type M<:AnyMapping }) =
			this(source, tables, new DerivedHauls(derivative))

		protected def this(source :R, tables :TablesIndex[R, H]) =
			this(source, tables, new DerivedHauls[R, H]())
		


		type Self[S<:RowSource] <: AbstractJoinHaul[S, H] {
			type Self[S1<:RowSource] = haul.Self[S1]
		}
		def self :Self[R]


		def mainSource :RowSource = source
		
		override def sources = source +: derivatives.ordered.flatMap(_.sources)

		def rootTable :TableFormula[R, H] = source.head.asInstanceOf[TableFormula[R, H]]
		def rootMapping :H = rootTable.mapping


		def included[M<:AnyMapping](path :MappingPath[H, M]) :Boolean =
			tables.contains(path) || derivatives.contains(path)


		override def filter(src :From[H], condition: BooleanFormula[From[H]]): Self[_] = {
			val paths = condition.collect { case PathFormula(_, path) => path.asInstanceOf[H\~\AnyMapping] }
			val fetched = (self.asInstanceOf[Self[RowSource]] /: paths)(_.fetch(_).asInstanceOf[Self[RowSource]])
			fetched.applyFilter(src, condition)
		}

		protected def applyFilter(src :From[H], condition :BooleanFormula[From[H]]) :Self[R] = {
			val filter = SQLScribe(condition, src, source) {
				case PathFormula(_, path) => resolveTable(path.cast[H, AnyMapping]) match {
					case (_, table, p @ ComponentPath(suffix)) if table.contains(p) =>
						ComponentFormula(table.table, suffix)
					case _ => throw new IllegalArgumentException(s"Can't apply filter $condition on $this. Path $path doesn't resolve to a component of primary join.")
				}
			}
			forSource[R](source.where(filter).asInstanceOf[R])
		}

		

		def fetch[M<:AnyMapping](path :MappingPath[H, M]) :Self[_] =
			if (path.start!=rootMapping)
				throw new IllegalArgumentException(s"Haul.fetch: path $path doesn't start with the root mapping $rootMapping of $this")
			else if (included(path))
				self
			else if (tables.keys.exists(_.startsWith(path)))
				self //todo: is include relation really transitive? Does fetching a component imply fetching all columns in the last?
			else if (derivatives.paths.exists(_.startsWith(path)))
				self
			else derivatives.find(path).map {
				case (prefix, suffix, subselect) =>
					if (subselect.included(suffix)) self
					else withDerived(prefix, subselect.fetch(suffix))
			} getOrElse {
				val (prefix, _, suffix) = resolveTable(path)
				follow(prefix, ComponentPath.self(suffix.start), suffix)
			}


		protected def follow[T<:AnyMapping, X <:AnyMapping, Y<:AnyMapping](table :MappingPath[H, T], component :ComponentPath[T, X], suffix :MappingPath[X, Y]) :Self[_] =
			suffix match {
				case SelfPath(target) =>
					include(table, component)
				case SplitFirst(dir :ComponentPath[_, _], rest) =>
					follow(table, component ++ dir, rest)
				case SplitFirst(next :JoinPath[_, _], rest) if AtMostOne(next.arity) => //@ JoinPath(join, AtMostOne(arity))
					joinOne(table, component, next, rest)
				case SplitFirst(next :JoinPath[_, _], rest) =>
					joinMany(table, component, next, rest)
				case SplitFirst(MappedComponent(link), rest) =>
					follow(table, component ++ link, rest)
				case _ => 
					throw new IllegalArgumentException(s"Can't include ${table++component++suffix} in fetch - unrecognized suffix: $suffix")
			}



		protected def include[Y <: AnyMapping, X <: AnyMapping, T <: AnyMapping](table: MappingPath[H, T], component: ComponentPath[T, X]): Self[R] = {
			val tableHaul = tableFor(table)
			if (tableHaul.contains(component)) self
			else withTable(table, tableHaul.fetch(component))
		}




		protected def joinOne[T<:AnyMapping, FK<:AnyMapping, F<:AnyMapping, Y<:AnyMapping](
			tablePath :MappingPath[H, T], foreignKey :ComponentPath[T, FK], joinPath :JoinPath[FK, F], suffix :MappingPath[F, Y]) :Self[_] =
		{

			val join = joinPath.join.plantLast(foreignKey)
			val outerJoin = joinPath.arity!=Arity._1 || foreignKey.optional // || join.isInstanceOf[RightJoin[_,_]]
			val sourceTable = tableFor(tablePath).table
			implicit val extension = reverseMergeJoin(source, sourceTable, join, outerJoin)
			val targetTable = extension.result.toStream(join.size-2).ensuring(_.mapping==joinPath.end).asInstanceOf[TableFormula[RowSource, F]]
			val joinedPath = tablePath ++ foreignKey ++ joinPath
			val joined = forSource(extension.result)(extension.extension)
			val complete = joined.withTable(joinedPath, new JoinedTableHaul(targetTable))
			complete.follow(joinedPath, ComponentPath.self(suffix.start), suffix)

		}

		
		protected def joinMany[T<:AnyMapping, FK<:AnyMapping, F<:AnyMapping, Y<:AnyMapping](
				tablePath :MappingPath[H, T], foreignKey :ComponentPath[T, FK], joinPath :JoinPath[FK, F], suffix :MappingPath[F, Y]) :Self[R] =
		{
			val join = joinPath.join.plantLast(foreignKey)
			val joinedTable = tableFor(tablePath).table
			val derivative = derived(joinedTable, join)
			withDerived(tablePath++foreignKey++joinPath, derivative)

		}


//		protected def derived[Y <: Mapping, F <: Mapping, FK <: Mapping, T <: Mapping](join: RowSource Join F Join T, joinedTable: JoinedTable[R, T]): DerivedHaul[R, F]
		protected def derived[F <: AnyMapping, T <: AnyMapping](joinedTable: TableFormula[R, T], join: RowSource Join F Join T): DerivedHaul[R, F]



		protected def resolveTable[M<:AnyMapping](path :MappingPath[H, M]) :(MappingPath[H, AnyMapping], JoinedTableHaul[R, AnyMapping], MappingPath[AnyMapping, M]) =
			getTable(path) match {
				case Some(table) => (path, table, ComponentPath.self(path.end)).asInstanceOf[(H\~\AnyMapping, JoinedTableHaul[R, AnyMapping], AnyMapping\~\M)]
				case None =>
					val matching = tables.flatMap {
						case (member, table) => path.drop(member).map(p =>
							(member.asInstanceOf[H\~\AnyMapping], table.asInstanceOf[JoinedTableHaul[R, AnyMapping]], p.asInstanceOf[AnyMapping\~\M])
						)
					}
					matching.maxBy(_._1.length).unless(matching.isEmpty) getOrElse {
						throw new IllegalArgumentException(s"No table for $path in $this. Probably the path doesn't start with the root mapping :$rootMapping.")
					}
			}

		protected def getTable[M<:AnyMapping](path :MappingPath[H, M]) :Option[JoinedTableHaul[R, M]] =
			tables.get(path).asInstanceOf[Option[JoinedTableHaul[R, M]]]

		protected def tableFor[M<:AnyMapping](path :MappingPath[H, M]) :JoinedTableHaul[R, M] = getTable(path) getOrElse {
			throw new NoSuchElementException(s"path $path is not included in $this")
		}



		protected def forSource[S<:RowSource](extended :S)(implicit ext :R ExtendedBy S) :Self[S]

		
		protected def withTable[M<:AnyMapping](path :MappingPath[H, M], table :JoinedTableHaul[R, M]) :Self[R]

		protected def withDerived[M<:AnyMapping](path :MappingPath[H, M], derived :DerivedHaul[R, M]) :Self[R]


		override def toString =
			tables.keys.mkString(s"Haul($source){", ", ", if (derivatives.isEmpty) "}" else "; "+derivatives.toString + "}")
	}




	class JoinWithJoinedSubselects[R<:RowSource, H<:AnyMapping](
			src :R,
			tableHauls :TablesIndex[R, H],
			derivedHauls :DerivedHauls[R, H] = new DerivedHauls[R, H])
		extends AbstractJoinHaul[R, H](src, tableHauls, derivedHauls)
	{
		type Self[S<:RowSource] = JoinWithJoinedSubselects[S, H]
		def self = this

//		protected def derived[Y <: Mapping, F <: Mapping, FK <: Mapping, T <: Mapping](join: Join[From[T], F], subsource: From[F], joinedTable: JoinedTable[R, T]): DerivedHaul[R, F] =
//			new JoinedSubselectHaul[R, T, From[F], F](source, joinedTable, join, subsource, TablesIndex(subsource))
		protected def derived[F <: AnyMapping, T <: AnyMapping](joinedTable: TableFormula[R, T], join: RowSource Join F Join T): DerivedHaul[R, F] =
			JoinedSubselectHaul(source, joinedTable, join)

		override protected def withTable[M <: AnyMapping](path: MappingPath[H, M], table: JoinedTableHaul[R, M]): Self[R] =
			new JoinWithJoinedSubselects(source, tables + (path->table), derivatives)

		override protected def withDerived[M <: AnyMapping](path: MappingPath[H, M], derived: DerivedHaul[R, M]): Self[R] =
			new JoinWithJoinedSubselects(source, tables, derivatives + (path -> derived))

		override protected def forSource[S <: RowSource](extended: S)(implicit ext: ExtendedBy[R, S]): Self[S] =
			new JoinWithJoinedSubselects(extended, tables.mapValues(_.asPartOf(extended)), derivatives.asPartOf(extended))

	}




	class JoinedSubselectHaul[P<:RowSource, T<:AnyMapping, R<:RowSource, H<:AnyMapping](
			parent :P, parentTable :TableFormula[P, T], join :RowSource Join H Join T,
			src :R, tableHauls :TablesIndex[R, H], derivedHauls :DerivedHauls[R, H] = new DerivedHauls[R, H])
		extends AbstractJoinHaul[R, H](src, tableHauls, derivedHauls) with DerivedHaul[P, H]
	{
		type Self[S<:RowSource] = JoinedSubselectHaul[P, T, S, H]
		def self = this


		override def asPartOf[S <: RowSource](source: S)(implicit ev: ExtendedBy[P, S]): DerivedHaul[S, H] = 
			new JoinedSubselectHaul[S, T, R, H](source, parentTable/*.asPartOf(source)*/, join, this.source, tables, derivatives)


		
		override protected def withTable[M <: AnyMapping](path: MappingPath[H, M], table: JoinedTableHaul[R, M]) =
			new JoinedSubselectHaul[P, T, R, H](parent, parentTable, join, source, tables + (path -> table), derivatives)


		override protected def forSource[S <: RowSource](extended: S)(implicit ext: ExtendedBy[R, S]) =
			new JoinedSubselectHaul(parent, parentTable, join, extended, tables.mapValues(_.asPartOf(extended)), derivatives.asPartOf(extended))


		override protected def withDerived[M <: AnyMapping](path: MappingPath[H, M], derived: DerivedHaul[R, M]) =
			new JoinedSubselectHaul(parent, parentTable, join, source, tables, derivatives + (path -> derived))

		protected def derived[F <: AnyMapping, O](joinedTable: TableFormula[R, O], join: RowSource Join F Join O): DerivedHaul[R, F] =
			JoinedSubselectHaul(source, joinedTable, join)
		
//		protected def derived[Y <: Mapping, F <: Mapping, FK <: Mapping, T <: Mapping](join: Join[From[T], F], subsource: From[F], joinedTable: JoinedTable[R, T]): DerivedHaul[R, F] =
//			new JoinedSubselectHaul[R, T, From[F], F](source, joinedTable, join, subsource, TablesIndex(subsource))


		def joinedWithParent(source :RowSource) :RowSource = {
//			val (joined :RowSource, _) = reverseMergeJoin(parent, parentTable, join.left)
			val crossjoin = reverseMergeCrossJoin(parent, join.left).result joinAny source
			val from = crossjoin.toStream.reverse.toIndexedSeq
			val left = from(parentTable.index).ensuring(_.mapping==parentTable.mapping) //parent last
			val right = from(parent.size + join.size-2).ensuring(_.mapping==join.right) //my root last

			
			val condition = SQLScribe.replant(join.filter, join, crossjoin) {
				case join.lastTable => right
				case join.left.lastTable => left
				case TableFormula(m, i) => from(parent.size+i).ensuring(_.mapping==m)
//				case t => throw new IllegalArgumentException(s"Unexpected last $t when replanting $join condition into $crossjoin; should be one of ($left, $right)")
			}
			crossjoin where condition
		}


		override lazy val mainSource = joinedWithParent(source)
		
		override def sources = super.sources.map(joinedWithParent)
	}



	def JoinedSubselectHaul[P<:RowSource, H <: AnyMapping, FK <: AnyMapping, T <: AnyMapping](parent :P, joinedTable: TableFormula[P, T], join: RowSource Join H Join T): JoinedSubselectHaul[P, T, From[H], H] = {
		val subsource = From(join.prev.mapping)
		new JoinedSubselectHaul[P, T, From[H], H](parent, joinedTable, join, subsource, TablesIndex(subsource))
	}







	type TablesIndex[R<:RowSource, H<:AnyMapping] = Map[MappingPath[H, _<:AnyMapping], JoinedTableHaul[R, _]]

	def TablesIndex[H<:AnyMapping](source :From[H]) :TablesIndex[From[H], H] =
		Map(ComponentPath.self(source.mapping) -> JoinedTableHaul[From[H], H](source.last))


	case class JoinedTableHaul[R<:RowSource, T<:AnyMapping](table :TableFormula[R, T], components :Set[ComponentPath[T, _]]=Set[ComponentPath[T, _]]())
	{

		def contains[M<:AnyMapping](path :MappingPath[T, M]) :Boolean =
			path.ifSubclass[ComponentPath[T, M]].orElse {
				cp => components(cp) || components.exists(_.startsWith(cp))
			} { false }


		def fetch[M<:AnyMapping](path :ComponentPath[T, M]) :JoinedTableHaul[R, T] =
			new JoinedTableHaul[R, T](table, components + path)

		def asPartOf[S<:RowSource](source :S)(implicit ext :R ExtendedBy S) =
			new JoinedTableHaul[S, T](table/*.asPartOf(source)*/, components)

		override def toString = s"From[$table]:{${components.mkString(",")}}"
	}





	type DerivedIndex[R<:RowSource, H<:AnyMapping] = Map[MappingPath[H, _<:AnyMapping], DerivedHaul[R, _<:AnyMapping]]

	class DerivedHauls[R<:RowSource, H<:AnyMapping] private (
			order :Seq[MappingPath[H, _<:AnyMapping]], index :DerivedIndex[R, H])
		extends Iterable[(MappingPath[H, M], DerivedHaul[R, M]) forSome { type M<:AnyMapping }]
	{
		def this(haul :(MappingPath[H, M], DerivedHaul[R, M]) forSome { type M<:AnyMapping }) =
			this(Seq(haul._1), Map(haul))

		def this() = this(Seq(), Map())

		def asPartOf[S<:RowSource](extension :S)(implicit ev :R ExtendedBy S) :DerivedHauls[S, H] =
			new DerivedHauls(order, index.mapValues(_.asPartOf(extension)))


		def paths = index.keySet

		def ordered = order.view.map(index).reverse

		def contains[M<:AnyMapping](path :MappingPath[H, M]) :Boolean = index.contains(path)

		def apply[M<:AnyMapping](path :MappingPath[H, M]) = index(path).asInstanceOf[DerivedHaul[R, M]]

		def find[M<:AnyMapping](path :MappingPath[H, M]) :Option[(MappingPath[H, T], MappingPath[T, M], DerivedHaul[R, T]) forSome { type T <:AnyMapping }] =
			index.toStream.flatMap {
				case (key, sub) => path.drop(key).map(p => (key.cast[H, AnyMapping], p.cast[AnyMapping, M], sub.asInstanceOf[DerivedHaul[R, AnyMapping]]))
			}.headOption


		def + [M<:AnyMapping](haul :(MappingPath[H, M], DerivedHaul[R, M])) :DerivedHauls[R, H] =
			if (index.contains(haul._1))
				new DerivedHauls[R, H](order, index + haul)
			else
				new DerivedHauls[R, H](haul._1 +: order, index + haul)




		override def iterator: Iterator[(MappingPath[H, M], DerivedHaul[R, M]) forSome { type M<:AnyMapping }] =
			index.iterator.map{ case (path, haul) => (path.cast[H, AnyMapping], haul.asInstanceOf[DerivedHaul[R, AnyMapping]]) }

		override def toString = index.mkString("Derived{", ", ", "}")
	}





	private def reverseMergeJoin[L<:RowSource, J<:AnyMapping](
			left :L, joinedTable :TableFormula[L, J], right :RowSource Join J, forceFirstOuterJoin :Boolean=false):
		RowSourceExtension[L] =
	{
		val extended = reverseCrossJoin(left, right.left, forceFirstOuterJoin)
		val joined = extended.result where SQLScribe(right.filter, right, extended.result) {
			case ComponentFormula(table, path) =>
				val myTable =
					if (table==right.last) joinedTable.asAny.asPartOf(extended.result)(extended.extension)
					else extended.result.toStream(table.index).asAny.ensuring(_.mapping==table.mapping)
				ComponentFormula(myTable, path.cast[AnyMapping, AnyMapping])
			case e => throw new IllegalArgumentException(s"Couldn't join $left with $right because it contains an abstract expression $e.")
		}
		RowSourceExtension(joined)(extended.extension)
	}



	/** Perform a crossjoin L [outer] join other', where other' is a join with the tables of other in reversed order and no filter. */
	private def reverseCrossJoin[L<:RowSource](left :L, right :RowSource, outer :Boolean=false) :RowSourceExtension[L] =
		right match {
			case Dual => RowSourceExtension(left)(ExtendedBy.itself)
			case l Join r =>
				val first =
					if (outer || right.isInstanceOf[RightJoin[_, _]]) left leftJoin r
					else left join r
				reverseMergeCrossJoin(first, right.asInstanceOf[Join[RowSource, AnyMapping]])
		}


	def reverseMergeCrossJoin[L <:RowSource, S<:RowSource](left :S, right :RowSource Join (_<:AnyMapping))(implicit extension :L ExtendedBy S) :RowSourceExtension[L] =
		right match {
			case Dual Join _ => RowSourceExtension(left)(extension)
			case rest @ (_ Join next) LeftJoin _ =>
				reverseMergeCrossJoin[L, S Join AnyMapping](left rightJoin next, rest)
			case rest @ (_ Join next) RightJoin _ =>
				reverseMergeCrossJoin[L, S Join AnyMapping](left leftJoin next, rest)
			case rest @ (_ Join next) Join _ =>
				reverseMergeCrossJoin[L, S Join AnyMapping](left join next, rest)
			case _ => throw new IllegalArgumentException(s"Can't do a reverse merge cross join between $left and $right: unknown join type")
		}
	


}

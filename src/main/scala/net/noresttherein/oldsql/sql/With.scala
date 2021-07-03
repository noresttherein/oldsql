package net.noresttherein.oldsql.sql

import scala.collection.mutable.Builder
import scala.collection.Factory

import net.noresttherein.oldsql.collection.{Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.schema.{RelVar, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.{AlteredRelation, RelationTemplate}
import net.noresttherein.oldsql.schema.Table.{Aliased, TableExpression, StaticTable}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.CommonTableExpression.CommonTableAlias
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.WithClause.AbstractSingletonWithClause
import net.noresttherein.oldsql.sql.With.CTEName
import net.noresttherein.oldsql.sql.ast.QuerySQL
import net.noresttherein.oldsql.sql.ast.QuerySQL.MappingQuerySQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






trait With {
	implicit def CTEName[N <: Label](name :N) :CTEName[N] = new CTEName(name)

}




object With {
	//todo: ParamTables
	//todo: recursive
	//todo: dependency graph to enforce correct order
	def apply[H](query :QuerySQL[RowProduct, H]) :WithQuerySQL[H] = new WithQuerySQL(query)

	def apply[M[O] <: MappingAt[O]](query :MappingQuerySQL[RowProduct, M]) :WithMappingQuerySQL[M] =
		new WithMappingQuerySQL(query)


	class WithQuerySQL[H] private[With] (private val query :QuerySQL[RowProduct, H]) extends AnyVal {
		def apply[Q](f :Table[MappingOf[H]#TypedProjection] => Q) :Q = f(query)

		def as[A <: Label](alias :A) :Proceed[MappingOf[H]#TypedProjection Aliased A] =
			//because the extension method for query doesn't currently preserve the result type H
			new Proceed((query :Table[MappingOf[H]#TypedProjection]) as alias)
	}

	class WithMappingQuerySQL[M[O] <: MappingAt[O]] private[With] (private val query :MappingQuerySQL[RowProduct, M])
		extends AnyVal
	{
		def apply[Q](f :Table[M] => Q) :Q = f(query)

		def as[A <: Label](alias :A) :Proceed[M Aliased A] =
			new Proceed(query as alias)
	}

	class Proceed[T] private[With](private val table :T) extends AnyVal {
		def apply[Q](f :T => Q) :Q = f(table)
	}

	class CTEName[N <: Label](private val name :N) extends AnyVal {
		def as[M[O] <: MappingAt[O], V](select: => QuerySQL[RowProduct, V] { type ResultMapping[O] = M[O] })
				:CommonTableAlias[N, M] =
			CommonTableExpression(select) as name
	}

}






trait CommonTableExpression[+M[O] <: MappingAt[O]]
	extends TableExpression[M] with RelVar[M] with RelationTemplate[M, CommonTableExpression]
{
	override def default :CommonTableExpression[M] = this

	def as[A <: Label](alias :A) :CommonTableAlias[A, M] =
		CommonTableExpression.as[A, query.ResultMapping, Row](alias, query)

	override def withClause :WithClause = query.withClause + default

	protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
			:CommonTableExpression[M] =
		if (includes.isEmpty && excludes.isEmpty)
			this
		else
			new AlteredRelation[M, CommonTableExpression](this, includes, excludes) with CommonTableExpression[M] {
				override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]]) =
					super[AlteredRelation].alter(includes, excludes)

				override val default = CommonTableExpression.this
				override type Row = default.Row
				override val query = default.query
				override def name = default.name
				override def export[O] = super[AlteredRelation].export[O]
				override lazy val toString :String = alteredString
			}

	def declarationSpelling[P, F <: RowProduct]
	                       (context :SQLContext, params :Parameterization[P, F])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
		super[TableExpression].defaultSpelling(context, params)

	override def refString :String = name
	override def toString  :String = name + " as " + super[TableExpression].toString
}



object CommonTableExpression {
	def apply[M[O] <: MappingAt[O], V]
	         (alias :String, select : => QuerySQL[RowProduct, V] { type ResultMapping[O] = M[O] })
			:CommonTableExpression[M] =
		new AbstractSingletonWithClause with CommonTableExpression[M] {
			override type Row = V
			override val name = alias
			override lazy val query = select
			override def head = this
			override val withClause :WithClause = { val q = query.withClause; if (q.isEmpty) this else q + this }
		}

	def apply[M[O] <: MappingAt[O], V]
	         (select : => QuerySQL[RowProduct, V] { type ResultMapping[O] = M[O] }) :CommonTableExpression[M] =
		apply[M, V]("cte", select)

	def apply[A <: Label](alias :A) :WithTableAsFactory[A] = new WithTableAsFactory(alias)

	class WithTableAsFactory[A <: Label] private[CommonTableExpression](private val alias :A) extends AnyVal {
		def as[M[O] <: MappingAt[O], V]
		      (select : => QuerySQL[RowProduct, V] { type ResultMapping[O] = M[O] }) :CommonTableAlias[A, M] =
			CommonTableExpression.as[A, M, V](alias, select)
	}

	def as[A <: Label, M[O] <: MappingAt[O], V]
	      (alias :A, select : => QuerySQL[RowProduct, V] { type ResultMapping[O] = M[O] })
			:CommonTableAlias[A, M] =
		new AbstractSingletonWithClause with CommonTableAlias[A, M] {
			override type Row = V
			override val name = alias
			override lazy val query = select
			override def head = this
			override val withClause :WithClause = { val q = query.withClause; if (q.isEmpty) this else q + this }
		}


	type * = CommonTableExpression[MappingAt]

	trait CommonTableAlias[A <: Label, +M[O] <: MappingAt[O]] extends CommonTableExpression[M] with StaticTable[A, M]

}






sealed trait WithClause extends Iterable[CommonTableExpression.*] {
	def +(table :CommonTableExpression.*) :WithClause =
		if (isEmpty)
			table.withClause
		else if (table.withClause.size == 1 && contains(table))
			this
		else
			this ++ table.withClause

	def ++(tables :WithClause) :WithClause =
		if (tables.isEmpty)
			this
		else {
			val b = WithClause.newBuilder; b.sizeHint(size + tables.size)
			(b ++= this ++= tables).result()
		}

	def +:(table :CommonTableExpression.*)  :WithClause =
		if (isEmpty)
			table.withClause
		else if (table.withClause.size == 1 && contains(table))
			this
		else
			table.withClause ++: this

	def ++:(tables :WithClause) :WithClause =
		if (tables.isEmpty) this
		else {
			val b = WithClause.newBuilder; b.sizeHint(size + tables.size)
			(b ++= tables ++= this).result()
		}

	def contains(table :CommonTableExpression.*) :Boolean

	protected override def newSpecificBuilder :Builder[CommonTableExpression.*, WithClause] =
		WithClause.newBuilder

	protected override def fromSpecific(coll :IterableOnce[CommonTableExpression.*]) :WithClause =
		WithClause.fromSpecific(coll)

	def toSeq    :Seq[CommonTableExpression.*]
	def toUnique :Unique[CommonTableExpression.*]
//	def toSet :Set[CommonTableExpression.*]
	protected[this] override def className = "With"
}



object WithClause extends Factory[CommonTableExpression.*, WithClause] {
	def apply(tables :CommonTableExpression.* *) :WithClause =
		if (tables.isEmpty) EmptyWithClause
		else if (tables.sizeIs == 1) tables.head.withClause
		else tables.view.flatMap(_.withClause).to(this)

	override def fromSpecific(tables :IterableOnce[CommonTableExpression.*]) :WithClause = tables match {
		case tables :WithClause => tables
		case iter :Iterable[CommonTableExpression.*] if iter.isEmpty => EmptyWithClause
		case iter :Iterable[CommonTableExpression.*] if iter.sizeIs == 1 => iter.head.withClause
		case tables :Unique[CommonTableExpression.*] => tables.view.map(_.withClause).reduce(_ ++ _)
		case iter :Iterator[_] if !iter.hasNext => EmptyWithClause
		case _ => (newBuilder ++= tables).result()
	}

	def single(table :CommonTableExpression.*) :WithClause = table.withClause

	def empty :WithClause = EmptyWithClause

	override def newBuilder :Builder[CommonTableExpression.*, WithClause] =
		new Builder[CommonTableExpression.*, WithClause] {
			val unique = Unique.newBuilder[CommonTableExpression.*]

			override def addOne(elem :CommonTableExpression.*) = { unique addAll elem.withClause; this }

			override def clear() :Unit = unique.clear()
			override def result() = new UniqueWithClause(unique.result())
		}

	def unapplySeq(ctes :WithClause) :Opt[Seq[CommonTableExpression.*]] = Got(ctes.toSeq)


	private object EmptyWithClause extends WithClause {
		override def size = 0
		override def knownSize = 0
		override def iterator = Iterator.empty
		override def foreach[U](f :CommonTableExpression.* => U) :Unit = ()

		override def contains(table :CommonTableExpression.*) = false

		override def +(table :CommonTableExpression.*)   = table.withClause
		override def ++(tables :WithClause)  = tables
		override def +:(table :CommonTableExpression.*)  = table.withClause
		override def ++:(tables :WithClause) = tables

		override def toSeq = Nil
		override def toUnique = Unique.empty
//		override def toSet = Set.empty
		override def toString :String = "With()"
	}

	protected[sql] sealed trait AbstractSingletonWithClause extends WithClause {
		override def size = 1
		override def knownSize = 1
		override def iterator :Iterator[CommonTableExpression.*] = Iterator.single(head)
		override def foreach[U](f :CommonTableExpression.* => U) :Unit = f(head)

		override def contains(table :CommonTableExpression.*) :Boolean = table.default == head

		override def +(table :CommonTableExpression.*)   :WithClause =
			if (table.withClause.size == 1)
				if (head == table.default) this else new UniqueWithClause(Unique(head, table.default))
			else
				this ++: table.withClause

		override def +:(table :CommonTableExpression.*)  :WithClause =
			if (table.withClause.size == 1)
				if (head == table.default) this else new UniqueWithClause(Unique(table.default, head))
			else
				this ++: table.withClause

		override def ++(tables :WithClause)  :WithClause = if (tables.isEmpty) this else head +: tables
		override def ++:(tables :WithClause) :WithClause = if (tables.isEmpty) this else tables + head

		override def toSeq    :Seq[CommonTableExpression.*]    = head::Nil
		override def toUnique :Unique[CommonTableExpression.*] = Unique.single(head)
//		override def toSet :Set[CommonTableExpression.*] = Set(head)
	}

	private class SingletonWithClause(override val head :CommonTableExpression.*) extends AbstractSingletonWithClause {
		override def toString :String = "With(" + head + ")"
	}

	private class UniqueWithClause(override val toUnique :Unique[CommonTableExpression.*]) extends WithClause {
		override def iterator = toUnique.iterator
		override def foreach[U](f :CommonTableExpression.* => U) :Unit = toUnique.foreach(f)

		override def contains(table :CommonTableExpression.*) :Boolean = toUnique.contains(table)

		override def ++(tables :WithClause)  =
			if (tables.isEmpty) this else new UniqueWithClause(toUnique ++ tables)

		override def ++:(tables :WithClause) =
			if (tables.isEmpty) this else new UniqueWithClause((Unique.newBuilder ++= tables ++= toUnique).result())

		override def toSeq = toUnique.toSeq
	}
}




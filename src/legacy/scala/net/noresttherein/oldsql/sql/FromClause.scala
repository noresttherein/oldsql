package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.{Chain, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.{Mapping, RootMapping, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.support.EmptyMapping
import net.noresttherein.oldsql.schema.Mapping.Component
import net.noresttherein.oldsql.sql.FromClause.{RowTables, SubselectFrom, TableFormula}
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, ColumnFormula}
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.sql.MappingFormula.ComponentFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainFormula



/** In its basic use, a representation of the 'FROM' and 'WHERE' clauses in an SQL SELECT statement
  * declaring the schema of a select row.
  * Contains a list of mappings (presumably tables, although it is not enforced),
  * together with an optional filter condition working on those mappings, especially any join conditions.
  * May be used however to represent a more general concept of any input values/types needed by an expression
  * (like a select statement header), where mappings don't have to be tables, by may be components which
  * can be linked to a table at a later state or even single columns representing input parameters.
  * In that case it's best thought as a signature containing declarations of terms (mappings and parameters)
  * over which sql formulas can be defined; hence an SQLFormula is parameterized with a FromClause.
  *
  * An implicit conversion is available to `RowTables[S]` where `S` is a subtype of FromClause, declaring operations
  * for extending this source as well as accessing tables in it. It can be enforced directly be requesting `.tables` for
  * a `FromClause` (essentially wrapping the source in `RowTables` and returning it), or just directly requested
  * from this instance by `asTables` method. This is so the static type of tables/extended joins returned
  * could be based on the static type of this instance, without parameterizing all subclasses with their expected
  * static type. This means that for example if we call s join table, and static types of s and table are `S` and `T`,
  * the result will correctly be `S JoinLike T` rather than `FromClause JoinLike T` (or `this.type JoinLike T`), without the need
  * for re-declaring all operations in individual subclasses. Of course, dynamic types are preserved. An additional
  * benefit worth noting is that we can easily work on partially or fully abstracted types by getting `RowTables`
  * for a supertype of the given source, essentially choosing in the place of usage what level of type checking
  * one would like to have. For example, getting `RowTables[FromClause]` for any particular source instance will allow
  * untyped operations, where all values returned & accepted are compatible with any other source. Be careful
  * when using this and remember that if you get a more generic type than you expected out of
  * an operation, you probably provided an argument of non-compatible (associated with a different source) type.
  */
trait FromClause {

	/** Result types of all mappings in this source concatenated into a heterogeneous list.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `JoinLike` (but unlike `::`), left associative.
	  */
	type Row <: Chain

	/** Type of the outer source if this source represents a subselect source created by `Outer.from()`.
	  * All 'real' `JoinLike` subclasses have this type equal to the `Outer` of their left side, but `SubselectJoin`
	  * defines `Outer` as the type of its left side. Additionally, all 'real' joins implement `AsSubselectOf[L#Outer]`
	  * and `SubselectJoin` implements `AsSubselectOf[L]`. This means that for any concrete class `S <: FromClause`
	  * with fully instantiated parameters (i.e. all tables in `S` and types of joins in it are known)
	  * value `(s :S) from t1 join t2 ... join t3` conforms to `AsSubselectOf[S]`. This way we can statically express
	  * a dependency relationship between sources without resorting to implicit evidence.
	  */
	type Outer <: FromClause

	/** Return the outer source of this instance if it (or, recursively, any source in the left side of the join)
	  * was created by calling `outer.from()`. When viewed as a list of tables, outer constitutes the result of dropping
	  * all tables joined in this instance up until and including a table joined by a `.from()` call. If there's no
	  * `SubselectJoin` in the dynamic type definition of this source, meaning this is not a subselect source
	  * (FROM clause of resulting selects will include all members of this source), `Dual` instance used
	  * as the table list terminator is returned.
	  * @return outer of the left side or just the left side if this instance is a `SubselectJoin`.
	  */
	def outer :Outer


	/** Type of the last mapping in this join (the rightmost one) if not empty. */
	type LastMapping <:Mapping

	/** Table alias proxy for the last table (or table-like expression) in this list as seen from some `FromClause`
	  * type `F` containing this instance in its 'tail'. In other words, this projectsthe type of the last element
	  * of this clause to an extending row source.
	  */
	type LastTable[F <: FromClause] <: TableFormula[F, _]

	/** Last mapping in this source when treated as a list, if any. */
	def lastTable :LastTable[this.type] //:JoinedTable[this.type, _<:Mapping]

	/** First (leftmost) mapping in this source when treated as a list. */
	def headTable :TableFormula[this.type, _ <: Mapping]



	/** All tables or other members (in the most generic cases just aliases for particular mappings) in this source,
	  * listed from left to right (first to last).
	  */
	def allTables :Seq[TableFormula[this.type, _ <: Mapping]] = toTableStream.reverse

	/** All tables or other members (in most generic cases just aliases for particular mappings) in this source, listed
	  * from right to left (last to first).
	  */
	def toTableStream :Seq[TableFormula[this.type, _ <: Mapping]] = toUntypedTableStream

	protected[sql] def toUntypedTableStream :Seq[TableFormula[FromClause, _ <: Mapping]]

	/** All tables or other members in this source occurring after the outer source prefix as defined by `outer`.
	  * These are tables that will occur in the 'FROM' clause of select statements created for this source.
	  * If this instance type is in the form of `From[T1] JoinLike T2 ... SubselectJoin S1 JoinLike S2 ... JoinLike SN`
	  * (and the listed subselect is the rightmost occurrence in this type) it will return `S1 ... SN`.
	  * If there's no `SubselectJoin` in this type's definition, all members will be returned just as in `allTables`.
	  */
	def subselectTables :Seq[TableFormula[this.type, _ <: Mapping]] = subselectTableStream.reverse

	protected[sql] def subselectTableStream :Seq[TableFormula[this.type, _ <: Mapping]] //= toUntypedTableStream

	/** Number of mappings contained in this join (counting all their occurrences separately). */
	def size :Int


	/** All mappings used by this source. */
	def mappings :Seq[Mapping]

	/** Number of mappings equal to the given mapping in the chain */
	def occurrences(mapping :Mapping) :Int


	/** Return a complete filter for this source's rows, being a conjunction of all join conditions and any additional
	  * filters defined in the chain.
	  */
	def filteredBy :BooleanFormula[this.type]

	/** Apply the given filter to this source, returning the source constituting of exactly the same joins,
	  * but with its filter being conjunction of the argument filter and any preexisting filter.
	  */
	def filterBy[S >: this.type <: FromClause](filter :BooleanFormula[S]) :S

	/** Create an sql tuple formula containing `TableFormula`s for all joined tables in their order of appearance.
	  * It will contain entries for all mappings in this source, including parameter mappings and mappings listed in this
	  * source's `Outer` prefix (if this source is a subselect source).
	  */
	def row :ChainFormula[this.type, Row]


	/** Accessor to all tables present in this chain which is passed as an argument to functions creating filters.
	  * This is the less strict version of tables which returns tables as members of a supertype of this, allowing
	  * unchecked operations without casting. In the most generic case, returned tables are just instances of
	  * `JoinedTable[FromClause]` and thus considered a part of any `FromClause`, effectively removing static
	  * type checking of membership. For this reason, use this method sparingly and with care.
	  */
	@inline
	final def asTables[LU >: this.type <: FromClause] :RowTables[LU] = thisAsTables.asInstanceOf[RowTables[LU]]

	private[this] val thisAsTables = new RowTables[FromClause](this)


	/** A cross join between this source and the given source.
	  * If this source is in the form of `From[L1] JoinLike L2 ... JoinLike LN` and the source `S` is in the form of
	  * `From[R1] JoinLike R2 ... JoinLike RM`, the result type `J` of the join will be
	  * `From[L1] JoinLike L2 ... JoinLike LN JoinLike R1 JoinLike R2 ... JoinLike RM` and the filter of the join will be
	  * equivalent to `this.filter && source.filter`. The nature (classes) of the joins is preserved -
	  * if any of the joins in the argument are outer joins, they will remain such in the result.
	  *
	  * The result will be equivalent to `this combine source`, but this method doesn't require the types
	  * of both this and argument sources to be fully instantiated, and the result in turn won't contain any information
	  * about the structure of the join.
	  */
	def joinAny(source :FromClause) :FromClause




	/** Remove the given table from the join, substituting all path expressions rooted in it for bound parameters
	  * retrieved from the given value.
	  */
	def substitute[T](table :TableFormula[this.type, _ <: Mapping[T]], value :T) :FromClause


	/** Create a row source matching the structure of this instance (that is, preserving the number and classes
	  * of the participating joins), where some tables are substituted to the start of the specified prefix path,
	  * and all expressions which use the given table are rewritten to refer to a component of the new table specified
	  * by the new path. In other words, if one of the 'tables' in this join is in fact a component,
	  * such as a foreign key, instead of a real table, by calling this method you can 'embed' or 'plant'
	  * that component in a desired table.
	  * @param prefix partial function, which, if defined for a table in this source, returns a path from a mapping that
	  *               should replace the argument table in returned source to the mapping specified by the argument
	  *               table. Returning a path which doesn't end with the mapping associated with the argument table
	  *               (so that any expression based in this table cannot be rewritten by prefixing a referenced
	  *               component with the given path) will result in an exception.
	  * @return
	  */
	def plant(prefix :PartialFunction[TableFormula[this.type, _ <: Mapping], ComponentPath[_ <: Mapping, _ <: Mapping]]) :FromClause


	/** A shorthand for `plant(PartialFunction)` which will return a `FromClause` where all member tables of this
	  * instance which refer to the end mapping of this path are replaced by the start mapping of this path and
	  * any expressions referring to original table are rewritten by prefixing referenced components with this path.
	  * Please note that if this source contains multiple occurrences of the same mapping, all will be rewritten!
	  */
	def plantMatching(prefix :ComponentPath[_ <: Mapping, _ <: Mapping]) :FromClause


	/** If this source represents a source for a subselect, rewrite it to become a subselect of source `O`.
	  * All joins back to outer are copied, retaining their type and mappings, but changing `TableFormula` instances,
	  * and any filters are rewritten to be grounded in this source, substituting tables from the subselect to the tables
	  * in the newly created source and using the given scribe to rewrite all path expressions grounded in
	  * this instance's outer.
	  * @param target new outer of this subselect source.
	  * @param rewriter substitution for expressions depending on `Outer`, i.e. not grounded in the explicit from list
	  *                 of this subselect.
	  */
	def transplant[O<:FromClause](target :O, rewriter :SQLScribe[Outer, O]) :SubselectFrom[O]


	def canEqual(that :Any) :Boolean = that.isInstanceOf[FromClause]
}






object FromClause {
	import JoinLike._

	/** A `FromClause` representing a subselect source of `S`.
	  * `S <: AsSubselectOf[R]` if and only if `S =:= R SubselectJoin M1 JoinLike M2 ... JoinLike MN` for some mappings
	  * `M1...MN`. Sources conforming to `AsSubselectOf[S]` can use all the mappings/tables which are a part of `S`,
	  * but they are not a part of any select formulas created from that source. This allows the use of nested
	  * select queries which depend on values from the 'FROM' clause of the outer select. Note that subselects
	  * may be nested to an arbitrary depth and only directly nested subselects of `S` conform to this type.
	  */
	type SubselectFrom[-S <: FromClause] = FromClause {
		type Parent >: S <: FromClause
	}


	/** A closed `FromClause`, which doesn't contain any parameters and is not a subsource of another source.
	  * `S <: SelectFrom` if and only if it is a concrete source (without any abstract types or `FromClause`
	  * occurrences in its definition) and doesn't contain any `WithParam` or `SubselectJoin`.
	  * The name stems from the fact that only such sources can be used to create independent select statements.
	  */
	type SelectFrom = FromClause {
		type Parent = FromClause
	}






	/** A container holding values for all members of source `S`. Can be seen as an extension of `ComponentValues[M]`
	  * to a list of mappings, but providing a much simplified interface. Used as input data for evaluation of
	  * SQL formulas grounded in source `S`.
	  */
	trait RowValues[+S <: FromClause] {

		def apply[T <: Component[O, E], O, E](table :TableFormula[S, T, O, E]) :E = get(table) getOrElse {
			throw new NoSuchElementException(s"No value for $table in $this")
		}

		def get[T <: Component[O, E], O, E](table :TableFormula[S, T, O, E]) :Option[E]
	}



	/** Creates containers mapping tables of particular sources into their values and thus allowing to evaluate any
	  * SQL formulas grounded in that source.
	  */
	object RowValues {
		def empty[S <: FromClause] :RowValues[S] = EmptyValues

		def apply[S <: FromClause, X](table :TableFormula[S, _ <: Mapping[X]], value :X) :RowValues[S] =
			new SingleTableValue(table, value)

		/** A value for a single table of source `S`. */
		private class SingleTableValue[S <: FromClause, X](table :TableFormula[S, _ <: Mapping[X]], value :X)
			extends RowValues[S]
		{
			override def get[T <: Mapping](t: TableFormula[S, T]): Option[T#Subject] =
				(table == t ifTrue value).asInstanceOf[Option[T#Subject]]
		}

		/** An empty container, representing an empty row without values for any tables */
		private object EmptyValues extends RowValues[Nothing] {
			override def get[T <: Mapping](table: TableFormula[Nothing, T]): Option[T#Subject] = None
		}

		implicit def extendedValues[S <: FromClause, E <: FromClause](values :RowValues[S])
		                                                             (implicit ext :S ExtendedBy E) :RowValues[E] =
			values.asInstanceOf[RowValues[E]]
	}






	/** A `FromClause` without any tables specified, representing a single statement parameter `X`.
	  * This type encompasses all from clauses ending with a synthetic parameter source, not simply the empty ones.
	  */
	type ParamSource[X] = FromClause WithParam X

	/** A factory of `FromClause` instances representing statement parameters. Used as sources for SQL formulas which
	  * are independent of any tables, but depend on application parameters which values are not known during
	  * their creation.
	  */
	object ParamSource {

		/** Create a source consisting only of a mapping of a statement parameter `X`.
		  * @param param a mapping representing statement parameter of type `X`, where any function `X=>T`
		  *              can be represented as a `Component[T]` of the mapping.
		  * @tparam X the type of the statement/source parameter.
		  * @return a `Dual WithParam X` instance, consisting of the passed `ParamMapping[X]`.
		  */
		def apply[X](param :ParamMapping[X]) :ParamSource[X] = new WithParam(Dual, param)

		/** Create a source consisting only of a mapping for a statement parameter `X`. This mapping serves
		  * as a placeholder for a value to be bound in the future to a value of `X`. Whenever there is a need to use
		  * a value which can be obtained from `X` in an SQL formula grounded in the returned source, a
		  * `WithParam[X]#Component[T]` wrapping a function `X=>T` can be used as its placeholder.
		  * @tparam X the type of the statement/source parameter.
		  * @return a `Dual WithParam X` instance, containing a single `ParamMapping[X]`.
		  */
		def apply[X] :ParamSource[X] = Dual.withParam[X]
	}



	/** A specialized join class which joins the source on its left side with an artificial mapping `SourceParam[X]`
	  * to denote and track usage of a query parameter `X`. This makes it possible to filter on the given source
	  * using values unknown at this time that can be obtained by applying an arbitrary scala function to `X`.
	  * This source declares it's `Outer` source as `Nothing` (rather than the outer of the left side) to ensure
	  * that it won't be used as a basis for a select embedded in the outer source of the left side, thus 'hiding'
	  * the existence of a parameter and making its outer source appear not parameterized.
	  *
	  * @param source any FromClause containing actual tables providing the data.
	  * @param table TableFormula wrapping formal parameter X in a SourceParam[X] mapping
	  * @param filter where condition to 'and' with any filter
	  * @tparam S the actual source of data
	  * @tparam X formal parameter type
	  */
	class WithParam[+S<:FromClause, X] protected(
		                                            val source :S,
		                                            table :TableFormula[S JoinLike ParamMapping[X], ParamMapping[X]],
		                                            filter :BooleanFormula[S JoinLike ParamMapping[X]])
		extends JoinLike[S, ParamMapping[X]](source, table, filter)
	{
		def this(source :S, param :ParamMapping[X]) =
			this(source, new TableFormula[FromClause, ParamMapping[X]](param, source.size), True())

		def this(source :S, name :String, filter :BooleanFormula[S JoinLike ParamMapping[X]]) =
			this(source, new TableFormula[FromClause, ParamMapping[X]](new ParamMapping[X](name), source.size), filter)

		def this(source :S, name :String="?") = this(source, name, True())



		type Outer = Nothing

		def outer = throw new UnsupportedOperationException(s"WithParam($this).outer")

		type Self[+R <: FromClause] = R WithParam X

		def self :WithParam[S, X] = this

//		def param = table.mapping


		override def transplant[O <: FromClause](target: O, rewriter: SQLScribe[Outer, O]): AsSubselectOf[O] =
			throw new UnsupportedOperationException(s"WithParam($this).transplant($target, _)")


		override protected def copyJoin(replacement: TableFormula[S JoinLike ParamMapping[X], ParamMapping[X]],
		                                condition :BooleanFormula[S JoinLike ParamMapping[X]] = True()): S WithParam X =
			new WithParam[S, X](source, replacement, condition)


		override def copyJoin[L <: FromClause, M <: Mapping](left: L, right: M): L JoinLike M = right match {
			case p :ParamMapping[_] => new WithParam(left, p.asInstanceOf[ParamMapping[Any]]).asInstanceOf[L JoinLike M]
			case _ => Join(left, right)
		}



		override def canEqual(that :Any) :Boolean = that.isInstanceOf[WithParam[_, _]]


		override def toString :String = s"$left with $last" +  (if (condition == True) "" else " on " + condition)
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
	class ParamMapping[O, P](name :String) extends RootMapping[O, P] with EmptyMapping[O, P] { This =>

		def this() = this("?")

		def apply[T :SQLForm](pick :P => T) :Component[T] = new Component[T](Extractor.requisite(pick))

		def opt[T :SQLForm](pick :P => Option[T]) :Component[T] = new Component[T](Extractor(pick))

		override def apply[T](component :Component[T]) :ComponentSelector[this.type, Owner, P, T] = component match {
			case mapping :ParamMapping[P]#ComponentMapping[T] if mapping.param eq this =>
				ComponentSelector[this.type, Owner, P, T](component, mapping.value.optional, mapping.value.requisite)
			case _ =>
				throw new IllegalArgumentException(s"Component $component is not a part of this parameter mapping $this")
		}

		private class ComponentMapping[T](private[ParamMapping] val value :Extractor[P, T])(implicit form :SQLForm[T])
			extends SubMapping[This.Owner, T] with EmptyMapping[T]
		{
			def param :ParamMapping[P] = This

			override def selectForm(components :Unique[Component[_]]) :SQLReadForm[T] = form
			override def selectForm :SQLForm[T] = form
			override def insertForm :SQLForm[T] = form
			override def queryForm :SQLForm[T] = form
			override def updateForm :SQLForm[T] = form

			override def toString = s"?[$form]"
		}


		override def toString = s":$name"



		def unapply[X](expr :SQLFormula[_, X]) :Option[(P => Option[X], SQLWriteForm[X])] = expr match {
			case ComponentFormula(t, selector) if t.mapping == this =>
				val s = selector.asInstanceOf[ComponentSelector[this.type, Owner, P, X]]
				Some(s.extractor.optional -> s.lifted.queryForm)
			case _ => None
		}

		/** An extractor matching ComponentFormulas for components of this mapping, that is actual sql statement parameters. */
		def ParamForm :Extractor[SQLFormula[_, _], SQLWriteForm[P]] =
			(sql :SQLFormula[_, _]) => unapply(sql).map { case (pick, form) => form.iflatMap(pick) }
	}



	object ParamMapping {
		def unapply[X](expr :SQLFormula[_, X]) :Option[(ParamMapping[C], C=>X, SQLWriteForm[X]) forSome { type C }] = expr match {
			case ComponentFormula(table, path) if table.mapping.isInstanceOf[ParamMapping[_]] =>
				val param = path.asInstanceOf[GeneralSelector[ParamMapping[Any], X]]
				param.surepick.map((table.mapping.asInstanceOf[ParamMapping[Any]], _, param.lifted.queryForm))
			case _ => None
		}

	}






	/** Implicit conversion of a FromClause instance to an interface providing operations on it parameterized with its
	  * static type. Contains both accessors returning tables (and other members of this source, such as parameters),
	  * and methods for joining this source with other tables/sources. By moving these methods into a separate interface
	  * we can use the static type of 'this' source in the result type, which would be otherwise unknown inside a
	  * FromClause subclass (largely because JoinLike classes are covariant). It also has an added benefit of being able
	  * to specify any supertype of the argument source as the static type parameter, in particular any 'abstract' types,
	  * such as FromClause or FromClause JoinLike T, picking the exact level of type safety we want.
	  * @tparam R expected static type of the given source
	  */
	implicit def tablesForSource[R <: FromClause](source :R) :RowTables[R] =
		new RowTables[R](source)



	/** Accessor methods providing aliases for all tables/mappings present in the row source S, which can be used
	  * to create filter conditions. Below operations are separated into this interface instead of being declared
	  * directly in FromClause/JoinLike for three reasons. First, JoinLike and subclasses are covariant regarding the type
	  * of their left side to ensure uniformity in subtype relation between different source types
	  * (i.e. L LeftJoin R <:< L JoinLike R and L LeftJoin M JoinLike R <:< L JoinLike M JoinLike R), which prevents from declaring
	  * methods  accepting L and types covariant to L. Second, it allows static return types of methods here to depend
	  * on static type of the implicit argument (i.e. the source on which the method being called), ensuring that
	  * filtering/joining methods called for example on L LeftJoin R will return L JoinLike R / extension of L JoinLike R.
	  * Third, it allows to pick the desired level of type checking convenient for the given task;
	  * RowTables[source.type] will ensure that all associated classes (like SQLFormula, etc.) came from
	  * exactly the given instance, while RowTables[FromClause] removes any type checking regarding the source type
	  * (i.e. a JoinedTable[FromClause] or SQLExpression[FromClause] obtained from this instance will statically type check
	  * with any FromClause).
	  *
	  * There is an implicit conversion from S <: FromClause to RowTables[S],
	  * which can be enforced explicitly by calling this instance's table method.
	  *
	  * This class is passed as the argument to sql formula constructor functions instead of
	  * S itself to work around the covariance of the latter.
	  * @param source the source/join for which this instance was created, providing all TableFormula instances.
	  * @tparam S Static type of the join list of tables being the source for aliases returned by this instance.
	  */
	class RowTables[S<:FromClause] private[sql](val source :S) extends AnyVal {
		/** Number of mappings in this source, including any mappings of outer ('prefix') sources */
		@inline
		final def size :Int = source.size

		/** All mappings used by this source. */
		@inline
		final def mappings :Seq[Mapping] = source.mappings

		/** Number of mappings equal to the given mapping in the chain */
		@inline
		final def occurences(mapping :Mapping) :Int = source.occurrences(mapping)




		/** Method forcing an implicit conversion from a source to it's contravariant list of tables.
		  * When s.tables is called for s :S<:FromClause, s is implicitly wrapped in this instance before passing to tables,
		  * which in turn simply returns this.
		  * @return this instance
		  */
		@inline
		final def tables :RowTables[S] = this

		/** Return a complete filter for this source's rows, being a conjunction of all join conditions and any additional filters defined in the chain. */
		@inline
		final def filter :BooleanFormula[S] = source.filteredBy.asInstanceOf[BooleanFormula[S]]


		/** An alias for a mapping in this join, which can be used to create filter expressions. At the time
		  * of this call no static or dynamic check is performed that M is in fact a part of this join,
		  * and it will be performed only when the mapping is used to create an sql expression.
		  * This is so that return value's apply() method can be called in the shorthand form directly
		  * (like t[M](_.id)), translated into RowTables.apply[M].apply(_.id),
		  * which would be impossible if this method took implicit arguments.
		  * @tparam M requested type of the mapping which should uniquely identify a table in this join.
		  * @return
		  */
		@inline
		final def apply[M<:Mapping] :JoinMember[S, M] = new JoinMember[S, M](source)

		/** An alias for a mapping in this join, which can be used to create filter expressions.
		  * Equivalent to apply[M] for the situations when direct parameterless apply call (i.e this[M])
		  * would be ambiguous.
		  */
		@inline
		final def member[M<:Mapping] :JoinMember[S, M] = new JoinMember[S, M](source)

		/** An alias for a mapping in this join, which can be used to create filter expressions.
		  * This takes an argument specifying the position of the table in the join, which will be resolved
		  * implicitly as long as mapping M occurs exactly once in the underlying join.
		  * The alias returned by this instance doesn't need any further information about the represented table,
		  * unlike JoinMember instances returned by apply and member.
		  */
		@inline
		final def table[M<:Mapping](implicit member :M JoinedIn S) :TableFormula[S, M] = member(source)



		/** All members of this source, each representing one occurrence of the given mapping (an alias in a FROM list) */
		@inline
		final def all :Seq[TableFormula[S, _<:Mapping]] = source.allTables.asInstanceOf[Seq[TableFormula[S, _<:Mapping]]]

		/** All members of this source, each representing one occurence of the given mapping, listed from right to left (with last joined table first). */
		final def toStream :Seq[TableFormula[S, _<:Mapping]] = source.toTableStream.asInstanceOf[Stream[TableFormula[S, _<:Mapping]]]





		/** Return the n-th (zero-based) table (mapping) in this join as an existential type.
		  * You'll likely have to cast the result to the expected mapping type do anything useful, but it allows to
		  * operate on this source (such as create filters and selects) without knowing it's actual type and still have
		  * type checking and valid result expressions.
		  */
		def apply(n :Int) :TableFormula[S, _<:Mapping] =
			if (n>=size || n<0) throw new NoSuchElementException(s"Can't return $n-th table of join $this (size: $size)")
			else toStream(size-n)


		/** Find a table in this join backed by the given mapping and return a fully-typed reference to it.
		  * Note that this will throw an exception if the mapping occurs more than once in the join!
		  * @return the unique table in this join using the given mapping in Some or None if none of the members of this join use the given mapping.
		  * @throws IllegalArgumentException if more than one TableFormula[this.type, M] is associated with this instance.
		  */
		def tableFor[M<:Mapping](mapping :M) :Option[TableFormula[S, M]] =
			toStream.filter(_.mapping == mapping) match {
				case Seq() => None
				case Seq(table) => Some(table.asInstanceOf[TableFormula[S, M]])
				case tables => throw new IllegalArgumentException(s"Can't return table for $mapping with multiple occurrences in $this: $tables")
			}

		/** Return an object representing source parameter X which provides methods for creating formulas based on the value
		  * of that parameter in a way similar to table[ParamMapping[X]], but with interface accepting functions X=>T rather than
		  * (equivalent) ParamMapping[X] => ParamMapping[X]#Component[T] for clarity.
		  * @param member proof that there is a source parameter X declared by source S
		  * @tparam X type of source parameter needed being represented in created sql formulas.
		  */
		@inline
		final def param[X](implicit member :ParamMapping[X] JoinedIn S) :RowSourceParam[S, X] =
			RowSourceParam(table[ParamMapping[X]])

		/** Return an object representing source parameter X which provides methods for creating formulas based on the value
		  * of that parameter in way similar to apply[ParamMapping[X]]. This is different from param[X] in that it doesn't
		  * require implicit evidence that parameter X is indeed a part of source S and defers it's binding to an underlying TableFormula
		  * for later, allowing shorthand syntax for apply() calls on the returned object (which would otherwise be interpreted as parameters
		  * for this method's implicit parameter list). It works in the same way as JoinMember returned apply[ParamMapping[X]], but accepts
		  * functions X=>T instead of longer (but equivalent) ParamMapping[X]=>ParamMapping[X]#Component[T].
		  * @tparam X type of source parameter being represented in created sql formulas
		  */
		@inline
		final def ?[X] :JoinParamMember[S, X] = new JoinParamMember[S, X](source)


		/** A fixed alias for the last mapping from the right in the source (that is, its right side). */
		@inline
		final def last :TableFormula[S, S#LastMapping] = source.lastTable.asInstanceOf[TableFormula[S, S#LastMapping]]

		/** A fixed alias for the second-last mapping from the right in source S, if any, or the last mapping in the left side of this join in other words. */
		@inline
		final def prev[T <:Mapping](implicit table :T SecondLastTableOf S) :TableFormula[S, T] =
			table(source)

		/** First (leftmost) mapping in the source when treated as a list.
		  * This method will work only for fully instantiated subtypes of FromClause (i.e. From[X] JoinLike Y JoinLike Z...).
		  * If you are working with abstract types or don't need the table to be parameterized with a concrete mapping type,
		  * use head instead.
		  */
		def first[M<:Mapping](implicit ev :M FirstTableOf S) :TableFormula[S, M] = ev(source)

		/** First (leftmost) mapping in this source when treated as a list. */
		def head :TableFormula[S, _<:Mapping] = source.headTable.asInstanceOf[TableFormula[S, _<:Mapping]]

		/** SQLFormula exposing all tables in this source as a HList of TableFormula instances. */
		def row :SQLFormula[S, S#Row] = source.row.asInstanceOf[SQLFormula[S, S#Row]]

		/** JoinLike these tables with another mapping for some database source of rows.
		  * This results in a cross-join by itself, use additional filtering methods provided by JoinLike class to provide a joining condition.
		  */
		def join[M<:Mapping](mapping :M) :S InnerJoin M = mapping match {
			case Dual => From(mapping).asInstanceOf[S InnerJoin M]
			case _ => new InnerJoin(source, mapping)
		}

		/** Perform an inner join between these tables and another mapping. This specialized, self-typed version of join[mapping.type]. */
		def joinInstance(mapping :Mapping) :S InnerJoin mapping.type = join[mapping.type](mapping)

		/** Perform a left outer join between these tables and the given mapping.
		  * This results in a cross-join by itself, use additional filtering methods provided by JoinLike class to provide a joining condition.
		  */
		def leftJoin[M<:Mapping](mapping :M) :S LeftJoin M = new LeftJoin(source, mapping)

		/** Specialized version of leftJoin working on self-typed mapping. Delegates to leftJoin[mapping.type](mapping) */
		@inline
		final def leftJoinInstance(mapping :Mapping) :S LeftJoin mapping.type = leftJoin[mapping.type](mapping)

		/** Perform a right outer join between these tables and the given mapping.
		  * This results in a cross-join by itself, use additional filtering methods provided by JoinLike class to provide a joining condition.
		  */
		def rightJoin[M<:Mapping](mapping :M) :S RightJoin M = new RightJoin(source, mapping)

		/** Specialized version of rightJoin working on self-typed mapping. Delegates to rightJoin[mapping.type](mapping) */
		@inline
		final def rightJoinInstance(mapping :Mapping) :S LeftJoin mapping.type = leftJoin[mapping.type](mapping)


		/** Perform a left outer join between these tables and the given mapping. An alias for leftJoin. */
		@inline
		final def outerJoin[M<:Mapping](mapping :M) :S LeftJoin M = new LeftJoin(source, mapping)

		/** Specialized version of outerJoin working on self-typed mapping. Delegates to leftJoin[mapping.type](mapping) */
		@inline
		final def outerJoinInstance(mapping :Mapping) :S LeftJoin mapping.type = leftJoin[mapping.type](mapping)



		/** Perform a join between this source and a synthetic mapping symbolizing a query parameter.
		  * Parameter X, as well as any expressions being functions of X can then occur in any SQLExpressions based
		  * on the returned source, such as select headers and filters. The parameter is exposed via regular JoinedTable
		  * for a special mapping SourceParam[X], which can create on request a Component[Y] for any function X=>Y.
		  * This makes it possible to create a full statement definition without knowing actual parameter values;
		  * if the values are known, they can be used in sql expressions directly.
		  * @tparam X a query parameter which value is not known at this time
		  * @return A special version of this join SourceParam[X] exposing value of X as an artificial member of the join.
		  */
		def withParam[X] :S WithParam X = new WithParam[S, X](source)

		/** Perform a join between this source and a synthetic mapping symbolizing a query parameter.
		  * Parameter X, as well as any expressions being functions of X can then occur in any SQLExpressions based
		  * on the returned source, such as select headers and filters. The parameter is exposed via regular JoinedTable
		  * for a special mapping SourceParam[X], which can create on request a Component[Y] for any function X=>Y.
		  * This makes it possible to create a full statement definition without knowing actual parameter values;
		  * if the values are known, they can be used in sql expressions directly via a BoundParameter expression.
		  * @tparam X a query parameter which value is not known at this time
		  * @param name a name  to use for the parameter, useful for debugging.
		  * @return A special version of this join SourceParam[X] exposing value of X as an artificial member of the join.
		  */
		def withParam[X](name :String) :S WithParam X = new WithParam[S, X](source, name)


		/** Create an implicit join between this instance and a new table representing a source for a subselect from that table
		  * (nested select expression which depends on values from this source). Resulting row source will
		  * 'contain' (provide access to via RowTables) all tables contained in this source, but they won't be
		  * part of any resulting subselect's sql FROM clause. All subsequent tables joined with the result will feature as
		  * explicit joins in the subselect as usual. The outer of the returned source
		  * (and any sources obtained by subsequent joins) will be set to this source, and the corresponding Outer type to S
		  * (static type of the concrete subclass of JoinLike).
		  * Additionally, all resulting sources will implement AsSubselectOf[S].
		  * @param mapping first table in the from clause of a subselect being built.
		  */
		def from[M<:Mapping](mapping :M) :S SubselectJoin M = new SubselectJoin(source, mapping)
		//		def from[M<:Mapping](mapping :M) :SubselectJoin[S, S, M] = new SubselectJoin(source, mapping)



		/** A cross join between this source and the given source.
		  * If this source is in the form of <code>From[L1] JoinLike L2 ... JoinLike LN</code> and source R is in the form of
		  * <code>From[R1] JoinLike R2 ... JoinLike RM</code> the result type J of the join will be
		  * <code>From[L1] JoinLike L2 ... JoinLike LN JoinLike R1 JoinLike R2 ... JoinLike RM</code> and the filter of the join will be
		  * equivalent to this.filter && source.filter. The nature(classes) of the joins is preserved -
		  * if any of the joins in the argument are outer joins, they will remain such in the result.
		  * Of course, you can narrow the result as usual.
		  * @param other a source containing mappings to be appended to this join
		  * @param joiner implicit recursive algorithm for performing the join
		  */
		def joinAll[R<:FromClause, J<:FromClause](other :R)(implicit joiner :RowSourceCrossJoin[S, R, J]) :J = joiner(source, other)




		/** Apply a new filter, to be joined with 'and' with any existing filters, as a function of the whole list represented
		  * by this instance (that is, not only the last two tables, but any mappings present in the whole chain).
		  * This method is especially useful when called in a chain after the join constructor, for example:
		  * <code>left join right where (t => t[Users](_.addressId)===t[Addresses](_.id))</code>
		  * @param condition a function which uses its argument to create a boolean expression representation defined for this instance.
		  * @return a new join between the exact same sources as before, where the resulting filter condition will be extended by ' and expr' where
		  *         'expr' will be an sql expression created from the return value of the argument function.
		  */
		def where(condition :RowTables[S]=>BooleanFormula[S]) :S = source.filterBy(condition(this))

		/** Apply a new filter, to be joined with 'and' with any existing filters, as a function of the whole list represented
		  * by this instance (that is, not only the last two tables, but any mappings present in the whole chain).
		  * This method is especially useful when called in a chain after the join constructor, for example:
		  * <code>left join right where (t => t[Users](_.addressId)===t[Addresses](_.id))</code>
		  * @param condition a filter condition to apply to this source
		  * @return a new join between the exact same sources as before, where the resulting filter condition will be extended by ' and cond' where
		  *         'cod' will be an sql expression created from the argument
		  */
		def where(condition :BooleanFormula[S]) :S = source.filterBy(condition)


		/** A shorthand function for applying a filter condition based only on last table, especially useful for queries against a single table.
		  * @param condition a filter condition to apply to this source
		  * @return a new join between the exact same sources as before, where the resulting filter condition will be extended by ' and cond' where
		  *         'cod' will be an sql expression created from the argument
		  */
		def whereLast(condition :TableFormula[S, S#LastMapping]=>BooleanFormula[S]) :S =
			source.filterBy(condition(last))



//		/** Apply a join condition between the last two tables in this chain. This works exactly like 'where', but
//		  * instead of a single argument representing all joined tables, the filter function should take as its arguments
//		  * the last two tables, i.e, the last table defined by the left side of this join, if any, and the right side of this join.
//		  * Static type checking will enforce that this method can't be called on 'joins' where left side is empty (single table sources).
//		  * @param condition a function creating a representation of a sql expression from the passed aliases for the joined tables.
//		  * @return
//		  */
//		def on[L<:Mapping, R<:Mapping](condition :(TableFormula[S, L], TableFormula[S, R]) => BooleanFormula[S])(
//				implicit left :L SecondLastTableOf S, right :R LastTableOf S) :S =
//			where(t => condition(left(source), right(source)))


		/** Create a select based in this source with the select clause specified by the formula returned by the argument function.
		  * This select will use all tables given by the 'from' method, in particular ommiting any tables of the outer source if S
		  * is a subselect source.
		  * As embedding a SelectFormula[R, H] for this select in a formula grounded in the outer source Outer of source S
		  * would require R>:Outer, while the static type of the outer source at embed point might be actually a subtype of R (because
		  * it is used through a RowTables[P] where P>:Outer, for reasons given in this class description), we can't return a universally
		  * usable SelectFormula here and provide any type safety. Instead, we return an intermediate object capturing the static type S of
		  * source used for this subselect and perform type checking at embed point - see methods seq(), single() exists() used to create an
		  * actual formula for the outer source representing this subselect
		  * @param value function creating the header formula (select clause columns) using this instance passed as the argument.
		  * @tparam H select clause type - might be column type for single column selects, a tuple/hlist for multiple columns,
		  *           or M#Subject for some mapping M if a mapped value of M is requested.
		  * @return an object capturing select and from clauses that can be used to create SelectFormulas and Select statments.
		  */
		def subselect[H](value :RowTables[S]=>SQLFormula[S, H]) :FromSelect[S, H] =
			FromSelect(source, value(source))


		/** Create a select based in this source with the select clause specified by the argument formula.
		  * This select will use all tables given by the 'from' method, in particular omitting any tables of the outer source if S
		  * is a subselect source.
		  * As embedding a SelectFormula[R, H] for this select in a formula grounded in the outer source Outer of source S
		  * would require R>:Outer, while the static type of the outer source at embed point might be actually a subtype of R (because
		  * it is used through a RowTables[P] where P>:Outer, for reasons given in this class description), we can't return a universally
		  * usable SelectFormula here and provide any type safety. Instead, we return an intermediate object capturing the static type S of
		  * source used for this subselect and perform type checking at embed point - see methods seq(), single() exists() used to create an
		  * actual formula for the outer source representing this subselect
		  * @param value header formula (select clause columns) using this instance passed as the argument.
		  * @tparam H select clause type - might be column type for single column selects, a tuple/hlist for multiple columns,
		  *           or M#Subject for some mapping M if a mapped value of M is requested.
		  * @return an object capturing select and from clauses that can be used to create SelectFormulas and Select statments.
		  */
		def subselect[H](value :SQLFormula[S, H]) :FromSelect[S, H] =
			FromSelect(source, value)

		//todo: make this return SubselectRow rather than Row
		/** Create a select based in this source with the select clause containing columns of all tables listed in this source,
		  * including in particular synthetic parameter mappings and any tables of outer select (i.e. not just the from list).
		  * This select will use all tables given by the 'from' method, in particular omitting any tables of the outer source if S
		  * is a subselect source.
		  * As embedding a SelectFormula[R, H] for this select in a formula grounded in the outer source Outer of source S
		  * would require R>:Outer, while the static type of the outer source at embed point might be actually a subtype of R (because
		  * it is used through a RowTables[P] where P>:Outer, for reasons given in this class description), we can't return a universally
		  * usable SelectFormula here and provide any type safety. Instead, we return an intermediate object capturing the static type S of
		  * source used for this subselect and perform type checking at embed point - see methods seq(), single() exists() used to create an
		  * actual formula for the outer source representing this subselect
		  * @return an object capturing select and from clauses that can be used to create SelectFormulas and Select statments.
		  */
		def subselectAll :FromSelect[S, S#Row] =
			subselect(source.row.asInstanceOf[SQLFormula[S, S#Row]])

//todo: this would include first column of the whole source, not the first in the from list for subselects
//		def subselectFirst[T<:Mapping](implicit firstTable :T FirstTableOf S) :FromSelect[S, T#Subject] =
//			subselect(firstTable(source))


		/** Create a select based in this source with the select clause containing columns of the last mapping in this source.
		  * Equivalent to subselect(last).
		  * This select will use all tables given by the 'from' method, in particular omitting any tables of the outer source if S
		  * is a subselect source.
		  * As embedding a SelectFormula[R, H] for this select in a formula grounded in the outer source Outer of source S
		  * would require R>:Outer, while the static type of the outer source at embed point might be actually a subtype of R (because
		  * it is used through a RowTables[P] where P>:Outer, for reasons given in this class description), we can't return a universally
		  * usable SelectFormula here and provide any type safety. Instead, we return an intermediate object capturing the static type S of
		  * source used for this subselect and perform type checking at embed point - see methods seq(), single() exists() used to create an
		  * actual formula for the outer source representing this subselect
		  * @return an object capturing select and from clauses that can be used to create SelectFormulas and Select statments.
		  */
		def subselectLast :FromSelect[S, S#LastMapping#Subject] =
			subselect(last)




		def select[H](value :RowTables[S]=>SQLFormula[S, H])(implicit asSubsource :S<:<SelectFrom) :SelectFormula[FromClause, H] =
			select(value(source))

		def select[H](value :SQLFormula[S, H])(implicit asSubsource :S<:<SelectFrom) :SelectFormula[FromClause, H] =
			SelectFormula(asSubsource(source), value.asInstanceOf[SQLFormula[SelectFrom, H]])

		def selectAll(implicit asSubsource :S<:<SelectFrom) :SelectFormula[FromClause, S#Row] =
			select(row)

		def selectFirst[T<:Mapping](implicit firstTable :T FirstTableOf S, asSubsource :S<:<SelectFrom) :SelectFormula[FromClause, T#Subject] =
			select(firstTable(source))

		def selectLast(implicit closedSource :S<:<SelectFrom) :SelectFormula[FromClause, S#LastMapping#Subject] =
			select(last)



		/** Represent a subselect of this source (a select grounded in a subsource of this source) as a formula for sequence of rows
		  * which can be used as argument for 'IN' sql condition.
		  * @param select definition of the subselect to be embedded in a formula for S
		  * @tparam R a subsource of S defined that S#Outer>:R, ensuring that the given select represents a direct subselect of this source.
		  * @tparam H row type returned by the select
		  * @return an SQLFormula[S, Seq[H]]
		  */
		def seq[R<:SubselectFrom[S], H](select :FromSelect[R, H]) :SelectFormula.SelectAsRows[S, H] =
			select.asSubselectOf(source).rows

		/** Convert a subselect of this source (a select grounded in a subsource of source S) as a formula for
		  * a single row. Used whenever we expect a select to return exactly one row to make possible
		  * formulas like x = (select name from users where id=?).
		  * @param select definition of the subselect to be embedded in a formula for S
		  * @tparam R a subsource of S defined that S#Outer>:R, ensuring that the given select represents a direct subselect of this source.
		  * @tparam H row type returned by the select
		  * @return an SQLFormula[S, H]
		  */
		def single[R<:SubselectFrom[S], H](select :FromSelect[R, H]) :SelectFormula.SelectAsRow[S, H] =
			select.asSubselectOf(source).single

		/** Create an sql EXISTS condition for the given select. Equivalent to seq(select).exists and ExistsFormula(seq(select)).
		  * @param select definition of the subselect to be embedded in a formula for S
		  * @tparam R a subsource of S defined that S#Outer>:R, ensuring that the given select represents a direct subselect of this source.
		  * @tparam H row type returned by the select
		  * @return an SQLFormula[S, Boolean] for EXISTS(select ...)
		  */
		def exists[R<:SubselectFrom[S], H](select :FromSelect[R, H]) :SQLFormula[S, Boolean] =
			select.asSubselectOf(source).exists
	}


	/** A stand-in for a table or table-like proxy object in the FROM list, represented by a mapping of type M.
	  * Can be used to create filter conditions and select clause members, as operating on the mapping directly could be ambiguous
	  * in cases where a table is joined with itself. This class is contravariant regarding its source type to reflect the
	  * fact that if a table is declared/present in a join, than it certainly is still present in all its subtypes.
	  *
	  * @tparam F static type of the row source containing the mapping M
	  * @tparam M mapping type identifying a single source in the join.
	  */
	class TableFormula[-F <: FromClause, M <: Component[O, E], O, E] private[sql](override val mapping :M, val index :Int, alias :Option[String]=None)
		extends ComponentFormula[F, M, M, O, E, E]
	{
		def table = this
		def path = ComponentPath.self(mapping)

		override def apply[C<:M#Component[_]](component :M => C) :ComponentFormula[F, M, C] =
			ComponentFormula[F, M, C](this, ComponentPath.direct[M, C](mapping, component(mapping)))


		override def :\ [T](component :M#Component[T]) :ComponentFormula[F, M, component.type] =
			ComponentFormula[F, M, component.type](this, ComponentPath.direct[M, component.type](mapping, component))

		override def \\[C<:Mapping](path :ComponentPath[M, C]) :ComponentFormula[F, M, C] =
			ComponentFormula[F, M, C](this, path)


		//		/** Represent this instance as a part of join with another table T */
		override def asPartOf[U<:F, E<:FromClause](source :E)(implicit ev :U ExtendedBy E) :TableFormula[E, M] = ev(this)

		def sameAs(other :TableFormula[_, _, _, _]) :Boolean = mapping==other.mapping && index==other.index

		def asAny :TableFormula[F, Mapping] = this.asInstanceOf[TableFormula[F, Mapping]]


		override def equals(that :Any) = that match {
			case t :TableFormula[_, _] => this eq t
			case _ => false
		}

		override def canEqual(that :Any) = that.isInstanceOf[TableFormula[_, _]]

		override def hashCode = System.identityHashCode(this)

		override def toString = (alias getOrElse mapping.toString) + s"($index)"

	}


	object TableFormula {

		//		implicit def toComponentExpression[S<:FromClause, M<:Mapping, E<:FromClause](source :JoinedTable[S, M])(implicit extension :S ExtendedBy E) :ComponentFormula[E, M, M] =
		//			source.asInstanceOf[JoinedTable[E, M]].*

		implicit def asMemberOfExtendedJoin[S<:FromClause, M<:Mapping, E<:FromClause](table :TableFormula[S, M])(implicit ev :S ExtendedBy E) :TableFormula[E, M] =
			table.asInstanceOf[TableFormula[E, M]]//.columns
		//			table.asPartOf[E]

		implicit def asMembersOfExtendedJoin[S<:FromClause, E<:FromClause](tables :Seq[TableFormula[S, _<:Mapping]])(implicit ev :S ExtendedBy E) :Seq[TableFormula[E, _<:Mapping]] =
			tables.asInstanceOf[Seq[TableFormula[E, Mapping]]]

		def unapply[S<:FromClause, M<:Mapping](table :TableFormula[S, M]) :Option[(M, Int)] = Some(table.mapping, table.index)
	}



	/** A stand-in for a table or table-like proxy object in the FROM list, represented by a mapping of type M.
	  * Can be used to create filter conditions and select clause members, as operating on the mapping directly could be ambiguous
	  * in cases where a table is joined with itself. It provides the same operations as TableFormula,
	  * but the underlying table is not identified when the instance is created, but when any accessor methods
	  * are invoked.
	  *
	  * To actually produce any useful result from this instance, an implicit
	  * evidence is needed that mapping M indeed uniquely represents a single source among the joined tables.
	  * Be warned, that for this to work correctly, the source type must be fully statically known; for example
	  * mapping M would be accepted at the compile time as a valid alias for FromClause JoinLike M, when in reality
	  * the dynamic type of the left side of the join might contain the given mapping. If that where the case,
	  * the resulting expression would be still correct and refer to the single unique M instance in the join that
	  * could had been identified.
	  *
	  * The cause for this duplication is that an implicit parameter list on a method prevents from implicitly invoking apply
	  * on the result. If the factory for these instances, RowTables[J, R].apply[T<:Mapping] took an implicit evidence
	  * that M uniquely identifies a member of the join, we couldn't just write : t[T](component)
	  * (shorthand for t.apply[T].apply(component) as the given parameter list would be taken as an explicit parameter
	  * for the implicit argument list of the first apply call.
	  *
	  * There is an implicit conversion from this instance to an SQLExpression[S, M#Subject]
	  * which can be used to compare all columns in the mapping.
	  * @param source originating source for this alias.
	  * @tparam S static type of the row source containing the mapping M
	  * @tparam M mapping type identifying a single source in the join.
	  */
	class JoinMember[S<:FromClause, M<:Mapping] private[FromClause](private[FromClause] val source :S) extends AnyVal {

		/** Create an SQL expression for the given component of this mapping. If the component is not a single column, it will be
		  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		  * @param component function returning a component of the mapping associated with this table.
		  * @param member proof that M is indeed a part of the underlying join
		  * @return an sql expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def apply[C<:M#Component[_]](component :M => C)(implicit member :M JoinedIn (_>:S)) :ComponentFormula[S, M, C] =
			t \ component

		/** Create an SQL expression for the given component of  this mapping. If the component is not a single column, it will be
		  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		  *
		  * This is equivalent to apply(component), but may be needed to resolve ambiguity of chained calls to apply methods
		  * by implicit application.
		  * @param component function returning a component of the mapping associated with this table.
		  * @param member proof that M is indeed a part of the underlying join
		  * @return an sql expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def \ [C<:M#Component[_]](component :M => C)(implicit member :M JoinedIn (_>:S)) :ComponentFormula[S, M, C] =
			apply(component)


		/** Create an SQL expression for the given component of this mapping. If the component is not a single column, it will be
		  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		  * @param component a component of the mapping associated with this table.
		  * @param member proof that M is indeed a part of the underlying join
		  * @return an sql expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def :\ [T](component :M#Component[T])(implicit member :M JoinedIn (_>:S)) :ComponentFormula[S, M, component.type] =
			t :\ component

		/** Create an SQL expression for some subcomponent of this mapping, including the whole mapping itself in the case
		  * of SelfPath. If the component is not a single column, it will be
		  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		  * @param path path to the component the value of which we are interested in.
		  * @param member proof that M is indeed a part of the underlying join.
		  * @return an sql expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def \\[C<:Mapping](path :ComponentPath[M, C])(implicit member :M JoinedIn (_>:S)) :ComponentFormula[S, M, C] =
			t \\ path

		//			/** Create an SQL expression for some subcomponent of this mapping, including the whole mapping itself in the case
		//			  * of SelfPath. If the component is not a single column, it will be
		//			  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		//			  * @param path path a function returning the path to the component the value of which we are interested in.
		//			  * @param member proof that M is indeed a part of the underlying join.
		//			  * @return an sql expression which can be used to create search filters and specify columns in the SELECT header.
		//			  */
		//			def \\[C<:Mapping](path :M => ComponentPath[M, C])(implicit member :M JoinedIn (_>:S)) :ComponentExpression[S, M, C] =
		//				t \\ path

		//		/** Create an SQL expression representing the whole underlying mapping. */
		//		def columns(implicit member :M JoinedIn (_>:S)) :ComponentFormula[S, M, M] =
		//			t.*

		/** The underlying mapping for reference */
		def mapping(implicit member :M JoinedIn (_>:S)) :M = t.mapping

		/** Convert this instance to a fully fixed alias by the way of an implicit proof that M uniquely identifies a mapping in S.
		  * If M represents a table mapping which is joined several times in S, it is possible to supply the required membership
		  * directly.
		  * @param member
		  * @return
		  */
		def t(implicit member :M JoinedIn (_>:S)) :TableFormula[S, M] = member(source)

		def asAny :JoinMember[S, Mapping] = this.asInstanceOf[JoinMember[S, Mapping]]
	}



	object JoinMember {

		implicit def toComponentFormula[S<:FromClause, M<:Mapping](source :JoinMember[S, M])(implicit member :M JoinedIn S) :ComponentFormula[S, M, M] =
			source.t

	}


	/** A factory for sql formulas representing abstract concept of source parameters.
	  *
	  * @tparam S
	  * @tparam X
	  */
	abstract class RowSourceParam[-S<:FromClause, X] private[FromClause]() {

		def apply[R<:S, V](param :X)(formula :SQLFormula[R, V]) :V
		def apply[V :SQLForm](property :X=>V) :SQLFormula[S, V]

		def *(implicit form :SQLForm[X]) :SQLFormula[S, X]

		def grounded(formula :SQLFormula[_, _]) :Boolean

		def ParamForm :Unapply[SQLFormula[_, _], SQLWriteForm[X]]
	}


	object RowSourceParam {
		implicit def toComponentFormula[S<:FromClause, X :SQLForm](source :RowSourceParam[S, X]): SQLFormula[S, X] =
			source.*

		/** Create a statement parameter formula factory backed by a ParamMapping[X] given as a table formula */
		def apply[S<:FromClause, X](table :TableFormula[S, ParamMapping[X]]) :JoinedParam[S, X] = new JoinedParam(table)

		/** Create a statement parameter formula factory creating bound paramters with values derived from the given param. */
		def apply[X](param :X) :BoundParams[X] = new BoundParams[X](param)

		/** A factory of formulas with values dependent on source parameter X. It is an adapter for a TableFormula[S, ParamMapping[X]]
		  * which allows creating sql formulas for statement paremeters given by functions X=>T, rather than equivalent, but more verbose
		  * ParamMapping[X]=>ParamMapping[X]#Component[T].
		  * @param table table representing the source parameter.
		  * @tparam S underlying source type
		  * @tparam X scala type of source parameter used to obtain values for actual statement parameters used by formulas grounded in S.
		  */
		class JoinedParam[-S<:FromClause, X] private[RowSourceParam](private val table :TableFormula[S, ParamMapping[X]])
			extends RowSourceParam[S, X]
		{
			override def apply[R <: S, V](param: X)(formula: SQLFormula[R, V]): V =
				formula.evaluate(RowValues(table, param))

			def apply[V :SQLForm](property :X=>V) :SQLFormula[S, V] =
				table(param => param(property))

			def *(implicit form :SQLForm[X]) :SQLFormula[S, X] =
				table(param => param(identity[X] _))

			def grounded(formula: SQLFormula[_, _]): Boolean = formula.isGroundedIn(Set(table))

			def ParamForm :Unapply[SQLFormula[_, _], SQLWriteForm[X]] = table.mapping.ParamForm
		}


		/** A factory of BoundParameter formulas, where the value of bound parameter is obtained by the root source param :X
		  * by given functions.
		  * @param param source parameter containing values for created statement parameters
		  * @tparam X source parameter type
		  */
		class BoundParams[X](param :X) extends RowSourceParam[FromClause, X] {

			override def apply[R <: FromClause, V](param: X)(formula: SQLFormula[R, V]): V =
				formula.evaluate(RowValues.empty)

			override def apply[V: SQLForm](property: X => V): SQLFormula[FromClause, V] =
				BoundParameter(property(param))

			override def *(implicit form: SQLForm[X]): SQLFormula[FromClause, X] =
				BoundParameter(param)

			override def grounded(formula: SQLFormula[_, _]): Boolean =
				formula.isFree

			override def ParamForm: Unapply[SQLFormula[_, _], SQLWriteForm[X]] = Unapply.Never
		}

		/** A dummy instance used when a method requires a RowSourceParam instance but the source doesn't take any parameters
		  * (has no ParamMapping mappings) and thus no statement parameters will be needed.
		  */
		object NoParams extends BoundParams[Any](())


	}



	class JoinParamMember[S<:FromClause, X] private[FromClause](private[FromClause] val source :S) extends AnyVal {

		def apply[V](property :X=>V)(implicit member :(ParamMapping[X]) JoinedIn (_>:S), form :SQLForm[V]) :SQLFormula[S, V] =
			member(source)(param => param(property))

		def opt[V](property :X=>Option[V])(implicit member :ParamMapping[X] JoinedIn (_>:S), form :SQLForm[V]) :SQLFormula[S, V] =
			member(source)(param => param.opt(property))

		def member = new JoinMember[S, ParamMapping[X]](source)

		//		def t(implicit member :SourceParam[X] JoinedIn (_>:S)) :JoinedTable[S, SourceParam[X]] = member(source)

		def columns(implicit member :ParamMapping[X] JoinedIn (_>:S), form :SQLForm[X]) :SQLFormula[S, X] =
			member(source)(param => param(identity[X] _))
	}


	object JoinParamMember {
		implicit def toComponentExpression[S<:FromClause, X](source :JoinParamMember[S, X])(
			implicit member :ParamMapping[X] JoinedIn S, form :SQLForm[X]) :SQLFormula[S, X] =
			source.columns
	}


	/** Implicit evidence that mapping M is joined exactly once in S. Be warned that this will work correctly only
	  * if S is fully instantiated; for example there is always evidence for (M JoinedIn (FromClause JoinLike M)), while
	  * in fact the dynamic type of left side of the join above can contain a mapping of type M.
	  * Note that it will be usually written in the infix form for clarity: M JoinedIn S.
	  */
	class JoinedIn[M<:Mapping, -S<:FromClause] private(private[FromClause] val pick :S=>TableFormula[S, M]) extends AnyVal {
		/** Return the alias for the table described by this instance from the join list. */
		def apply(source :S) :TableFormula[_>:S, M] = pick(source)

		/** Return the underlying mapping */
		def mapping(source :S) :M = pick(source).mapping

	}

	/** Implicit evidence that mapping M is joined exactly once in S. Be warned that this will work correctly only
	  * if S is fully instantiated; for example there is always evidence for (M IncludedIn (FromClause JoinLike M)), while
	  * in fact the dynamic type of left side of the join above can contain a mapping of type M.
	  */
	object JoinedIn {
		/** A shorthand for implicitly[M IncludedIn S] */
		@inline
		def apply[M<:Mapping, S<:FromClause](implicit member :M JoinedIn S) :M JoinedIn S = member

		//			def apply[M<:Mapping, S<:FromClause](pick :S=>M) :M IncludedIn S = new IncludedIn[S, M](pick)

		/** A shorthand for implicitly[M IncludedIn S] */
		@inline
		def proof[M<:Mapping, S<:FromClause](implicit member :M JoinedIn S) :M JoinedIn S = member

		/** Mapping present on the right side of join */
		implicit def right[M<:Mapping, L<:FromClause] :M JoinedIn JoinLike[L, M] =
			new JoinedIn[M, JoinLike[L, M]](_.last)

		/** Mapping present on the left side of join */
		implicit def left[M<:Mapping, L<:FromClause, R<:Mapping](implicit member :JoinedIn[M, L]) :M JoinedIn JoinLike[L, R] =
			new JoinedIn[M, JoinLike[L, R]](s => member(s.left))

	}




	trait RowSourceExtension[S<:FromClause] {
		type Result <:FromClause
		val result :Result
		val extension :S ExtendedBy Result
	}

	object RowSourceExtension {
		def apply[S<:FromClause, E<:FromClause](result :E)(implicit extension :S ExtendedBy E) =
			new ExtendedRowSource[S, E](result)

		def unapply[S<:FromClause](extension :RowSourceExtension[S]) :Some[(extension.Result, S ExtendedBy extension.Result)] =
			Some(extension.result, extension.extension)

		class ExtendedRowSource[S<:FromClause, E<:FromClause](val result :E)(implicit val extension :S ExtendedBy E)
			extends RowSourceExtension[S]
		{
			type Result = E
		}
	}


	/** Proof that source S is an extension of source R / source R is a prefix source of S.
	  * It means that S<: R JoinLike T1 ... JoinLike TN forSome T1 ... TN.
	  */
	class ExtendedBy[R<:FromClause, -S<:FromClause] private() {
		def apply[T <: Component[O, E], O, E](table :TableFormula[R, T, O, E]) :TableFormula[S, T, O, E] =
			table.asInstanceOf[TableFormula[S, T, O, E]]

		def apply[T](expression :SQLFormula[R, T]) :SQLFormula[S, T] =
			expression.asInstanceOf[SQLFormula[S, T]]

		def apply[T](expression :ColumnFormula[R, T]) :ColumnFormula[S, T] =
			expression.asInstanceOf[ColumnFormula[S, T]]
	}

	object ExtendedBy {
		implicit def itself[R<:FromClause] :ExtendedBy[R, R] = new ExtendedBy[R, R]
		implicit def join[S<:FromClause, L<:FromClause, R<:Mapping](implicit ev :S ExtendedBy L) :ExtendedBy[S, L JoinLike R] =
			new ExtendedBy[S, L JoinLike R]
	}


	/** Proof that M is the last mapping in S (which is otherwise given as M#LastMapping). It is often easier for type inferencer
	  * to use an implicit parameter and deduce the concrete type by itself, than a member type.
	  */
	abstract class LastTableOf[M<:Mapping, -S<:FromClause] private() {
		def apply(source :S) :TableFormula[S, M]
	}

	object LastTableOf {
		implicit def lastTableInJoin[M<:Mapping] : M LastTableOf (FromClause JoinLike M) =
			instance.asInstanceOf[M LastTableOf (FromClause JoinLike M)]

		private val instance = new LastTableOf[Mapping, FromClause JoinLike Mapping] {
			final def apply(source: FromClause JoinLike Mapping): TableFormula[FromClause JoinLike Mapping, Mapping] =
				source.last.asInstanceOf[TableFormula[FromClause JoinLike Mapping, Mapping]]
		}
	}

	/** Proof that M is the second last mapping in S, i.e. that S <:< FromClause JoinLike M JoinLike _. */
	abstract class SecondLastTableOf[M<:Mapping, -S<:FromClause] private() {
		def apply(source :S) :TableFormula[S, M]
	}

	object SecondLastTableOf {
		implicit def secondLastTableInJoin[M<:Mapping, R<:Mapping] : M SecondLastTableOf (FromClause JoinLike M JoinLike R) =
			instance.asInstanceOf[M SecondLastTableOf (FromClause JoinLike M JoinLike R)]

		private[this] val instance = new SecondLastTableOf[Mapping, FromClause JoinLike Mapping JoinLike Mapping] {
			final def apply(source: FromClause JoinLike Mapping JoinLike Mapping) =
				source.left.last
		}

	}

	/** Proof that M is the first mapping in source S, i.e. that S ~:~ From[M] .... */
	abstract class FirstTableOf[M<:Mapping, -S<:FromClause] private() {
		def apply(source :S) :TableFormula[S, M]
	}

	object FirstTableOf {
		implicit def singleTable[M<:Mapping] :M FirstTableOf (Dual JoinLike M) = single.asInstanceOf[M FirstTableOf (Dual JoinLike M)]

		implicit def anyJoin[M<:Mapping, L<:FromClause, R<:Mapping](implicit ev :M FirstTableOf L) :M FirstTableOf (L JoinLike R) =
			new FirstTableOf[M, L JoinLike R] {
				override def apply(source: JoinLike[L, R]): TableFormula[JoinLike[L, R], M] = ev(source.left)//.asPartOf(source)
			}

		private[this] val single = new FirstTableOf[Mapping, Dual JoinLike Mapping] {
			final def apply(source: JoinLike[Dual, Mapping]): TableFormula[JoinLike[Dual, Mapping], Mapping] = source.last
		}
	}





	/** An evidence that source M is the result type of cross join between L and R (contains all tables/joins from L joined with all tables/joins in R). */
	abstract class RowSourceCrossJoin[L<:FromClause, -R<:FromClause, M<:FromClause] private[RowSourceCrossJoin]() {
		def apply(left :L, right :R) :M

		def map[A<:Mapping](left :L, right :R, merge :M)(table :TableFormula[right.type, A]) :TableFormula[M, A]

	}


	object RowSourceCrossJoin {
		implicit def crossJoinDual[L<:FromClause] :RowSourceCrossJoin[L, Dual, L] =
			CrossJoinDual.asInstanceOf[RowSourceCrossJoin[L, Dual, L]]

		private object CrossJoinDual extends RowSourceCrossJoin[FromClause, Dual, FromClause] {
			override def apply(left: FromClause, right :Dual): FromClause = left
			override def map[A <: Mapping](left :FromClause, right :Dual, merge :FromClause)(table: TableFormula[right.type, A]): TableFormula[FromClause, A] =
				throw new NoSuchElementException(s"merging table $table for a Dual row from $right into $merge")
		}

		implicit def joinCrossJoin[F<:FromClause, L<:FromClause, R<:Mapping, M<:FromClause](implicit merger :RowSourceCrossJoin[F, L, M]) :RowSourceCrossJoin[F, L JoinLike R, M JoinLike R] =
			new JoinCrossJoin[F, JoinLike, L, R, M](merger)

		//		implicit def rightCrossJoin[F<:FromClause, L<:FromClause, R<:Mapping, M<:FromClause](implicit merger :RowSourceCrossJoin[F, L, M]) :RowSourceCrossJoin[F, L RightJoin R, M RightJoin R] =
		//			new JoinCrossJoin[F, RightJoin, L, R, M](merger)
		//
		//		implicit def leftCrossJoin[F<:FromClause, L<:FromClause, R<:Mapping, M<:FromClause](implicit merger :RowSourceCrossJoin[F, L, M]) :RowSourceCrossJoin[F, L LeftJoin R, M LeftJoin R] =
		//			new JoinCrossJoin[F, LeftJoin, L, R, M](merger)
		//
		//		implicit def innerCrossJoin[F<:FromClause, L<:FromClause, R<:Mapping, M<:FromClause](implicit merger :RowSourceCrossJoin[F, L, M]) :RowSourceCrossJoin[F, L InnerJoin R, M InnerJoin R] =
		//			new JoinCrossJoin[F, InnerJoin, L, R, M](merger)

		class JoinCrossJoin[F<:FromClause, J[A<:FromClause, B<:Mapping]<:JoinLike[A, B], L<:FromClause, R<:Mapping, M<:FromClause] private[RowSourceCrossJoin]
		(merger :RowSourceCrossJoin[F, L, M])
			extends RowSourceCrossJoin[F, L J R, M J R]
		{ self =>

			override def apply(left: F, right :L J R): M J R = {
				val join = right.copyJoin(merger(left, right.left)).asInstanceOf[M J R]
				val replanter = new SQLScribe.Replanter[right.type, join.type](right, join) {
					override def map[A <: Mapping](table: TableFormula[right.type, A]): TableFormula[M J R, A] =
						self.map(left, right, join)(table)
				}
				join filterBy replanter.apply(right.condition)
			}

			override def map[A <: Mapping](left :F, right :L J R, merge :M J R)(table: TableFormula[right.type, A]): TableFormula[M J R, A] =
				if (table==right.last) merge.last.asInstanceOf[TableFormula[M J R, A]]
				else merger.map(left, right.left, merge.left)(table.asInstanceOf[TableFormula[L, A]]).asPartOf(merge :M J R)
		}
	}



}

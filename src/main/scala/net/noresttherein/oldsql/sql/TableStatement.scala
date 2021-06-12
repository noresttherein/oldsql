package net.noresttherein.oldsql.sql

import scala.annotation.nowarn

import net.noresttherein.oldsql.{DeprecatedAlways, OperationType, SharedImplDeprecation}
import net.noresttherein.oldsql.collection.ReversedList
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.Bug
import net.noresttherein.oldsql.morsels.Extractor.Optional
import net.noresttherein.oldsql.schema.{ColumnMapping, RelVar}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.ComponentSetter.:=
import net.noresttherein.oldsql.sql.DML.{BoundDML, ComposedDML, DMLAPI, RepeatedDML}
import net.noresttherein.oldsql.sql.DMLStatement.{AlteredResultStatement, BoundStatement, ComposedStatement, DMLStatementAPI, StatementResult}
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, ParamRelation}
import net.noresttherein.oldsql.sql.ast.SQLParameter
import net.noresttherein.oldsql.sql.ast.SQLLiteral.{False, True}
import net.noresttherein.oldsql.sql.mechanics.SQLScribe

//here be implicits
import net.noresttherein.oldsql.slang._





/** A `DML` instance affecting a single database table (for example, an [[net.noresttherein.oldsql.sql.Insert Insert]]).
  * @tparam Args The types of the argument(s) accepted by this DML and the executable `Incantation` resulting from it.
  *              Multiple arguments are often passed as a [[net.noresttherein.oldsql.collection.Chain Chain]].
  * @tparam M    The type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]] type for the rows
  *              of the associated table, accepting the mapping's
  *              [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
  * @tparam Res  The type of the value returned by the resulting `Incantation`.
  * @see [[net.noresttherein.oldsql.sql.TableStatement]]
  */
trait TableDML[-Args, M[O] <: MappingAt[O], +Res]
	extends DML[Args, Res] with DMLAPI[Args, Res, TableDML.Of[M]#DML]
{
	/** The table which contents is modified by this DML **/
	val table :RelVar[M]

	override def compose[X](f :X => Args) :TableDML[X, M, Res] =
		new ComposedDML.Base[X, Args, Res, TableDML.Of[M]#DML](this, f) with DerivedDML[X, Res]
			with ComposedDML[X, Args, Res] with ComposedDML.Impl[X, Args, Res, TableDML.Of[M]#DML]

	override def bind(args :Args) :TableDML[Unit, M, Res] =
		new BoundDML.Base[Args, Res, TableDML.Of[M]#DML](this, args) with DerivedDML[Any, Res]
			with BoundDML[Args, Res] with BoundDML.Impl[Args, Res, TableDML.Of[M]#DML]

	protected[sql] trait DerivedDML[-X, +Y] extends TableDML[X, M, Y] {
		override val table = TableDML.this.table
	}
}




object TableDML {
	/** Curried type constructor of [[net.noresttherein.oldsql.sql.TableDML TableDML]] and
	  * [[net.noresttherein.oldsql.sql.TableStatement TableStatement]].
	  */
	type Of[M[O] <: MappingAt[O]] = {
		type DML[-X, +Y] = TableDML[X, M, Y]
		type Stmt[-X, +Y] = TableStatement[X, M, Y]
	}

	/** A type alias for a parameterless [[net.noresttherein.oldsql.sql.TableDML TableDML]]. **/
	type GroundTableDML[M[O] <: MappingAt[O], +Res] = TableDML[Unit, M, Res]
}






/** A DML statement affecting a single database table.
  * @tparam Args The types of the argument(s) accepted by this statement and the executable `Incantation` resulting
  *              from it. Multiple arguments are often passed as a [[net.noresttherein.oldsql.collection.Chain Chain]].
  * @tparam M    The type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]] type for the rows
  *              of the associated table, accepting the mapping's
  *              [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
  * @tparam Res  The type of the value returned by the resulting `Incantation`.
  * @author Marcin Mościcki
  */
trait TableStatement[-Args, M[O] <: MappingAt[O], +Res]
	extends TableDML[Args, M, Res] with DMLStatement[Args, Res] with DMLStatementAPI[Args, Res, TableDML.Of[M]#Stmt]
{
	protected override def returns[Y](result :StatementResult[Nothing, Y]) :TableStatement[Args, M, Y] =
		new AlteredResultStatement.Base[Args, Y, TableDML.Of[M]#Stmt](this, result)
			with TableStatement[Args, M, Y] with DerivedDML[Args, Y]
			with AlteredResultStatement[Args, Y] with AlteredResultStatement.Impl[Args, Y, TableDML.Of[M]#Stmt]

	override def compose[X](f :X => Args) :TableStatement[X, M, Res] =
		new ComposedDML.Base[X, Args, Res, TableDML.Of[M]#Stmt](this, f)
			with TableStatement[X, M, Res] with DerivedDML[X, Res]
			with ComposedStatement[X, Args, Res] with ComposedStatement.Impl[X, Args, Res, TableDML.Of[M]#Stmt]

	override def bind(args :Args) :TableStatement[Unit, M, Res] =
		new BoundDML.Base[Args, Res, TableDML.Of[M]#Stmt](this, args)
			with TableStatement[Any, M, Res] with DerivedDML[Any, Res]
			with BoundStatement[Args, Res] with BoundStatement.Impl[Args, Res, TableDML.Of[M]#Stmt]

	override def batch :TableDML[Seq[Args], M, Seq[Res]] =
		new RepeatedDML.Base(this)
			with TableDML[Seq[Args], M, Seq[Res]] with DerivedDML[Seq[Args], Seq[Res]] with RepeatedDML[Args, Res]
}




object TableStatement {
	/** A type alias for a parameterless [[net.noresttherein.oldsql.sql.TableDML TableDML]]. **/
	type GroundTableStatement[M[O] <: MappingAt[O]] = TableStatement[Unit, M, Any]



	trait ReturningClauseFactory[M[O] <: MappingAt[O], +R[_]] {
		def returning[T](key :M[From[M]] => RefinedMapping[T, From[M]]) :R[T]
	}

	trait UpdatingClauseFactory[M[O] <: MappingAt[O], +U] {
		def updating[T](key :M[From[M]] => RefinedMapping[T, From[M]]) :U = //this could benefit from overriding
			updating(key :: Nil)

		def updating(key1 :M[From[M]] => RefinedMapping[_, From[M]],
		             key2 :M[From[M]] => RefinedMapping[_, From[M]],
		             keys :M[From[M]] => RefinedMapping[_, From[M]]*) :U =
			updating(key1 +: key2 +: keys)

		def updating(keys :Seq[M[From[M]] => RefinedMapping[_, From[M]]]) :U

		def updatingKeys(keys :Seq[RefinedMapping[_, From[M]]]) :U
	}

	trait ReturningUpdatingClausesFactory[M[O] <: MappingAt[O], +R[_], +U]
		extends ReturningClauseFactory[M, R] with UpdatingClauseFactory[M, U]


	trait ReturningClauseExpansion[M[O] <: MappingAt[O]] {
		type x[T]
		def x[T](key :M[From[M]] => RefinedMapping[T, From[M]]) :x[T]
	}




	@deprecated(SharedImplDeprecation, DeprecatedAlways)
	sealed trait SetClauseDomain[Arg, M[O] <: MappingAt[O], R >: From[M] WithParam Arg <: RowProduct] extends Any {
		type SetDomain = R
		protected def table :RelVar[M]
		protected def setDomain :From[M] WithParam Arg
		@inline final protected[sql] def setDomainForwarder :From[M] WithParam Arg = setDomain
	}


	@nowarn("cat=deprecation")
	trait SetClauseFactory[Arg, M[O] <: MappingAt[O], R >: From[M] WithParam Arg <: RowProduct, +Res]
		extends Any with SetClauseDomain[Arg, M, R]
	{
		protected[sql] final def setForwarder(setter :From[M] := R) :Res = set(setter)

		protected[sql] final def setAllForwarder(setters :Seq[From[M] := R]) :Res =
			setAll(setters)

		protected def setAll(setters :Seq[From[M] := R]) :Res

//		def setAll(setters :(M[From[M]], FromParam.Last[Arg]) => Seq[From[M] := R]) :Res = {
//			val base = setDomain
//			setAll(setters(table.row, base.last.mapping).map(_.anchor(base.left, base)))
//		}
//
		protected def set(setter :From[M] := R) :Res = setAll(ReversedList :+ setter)

		def set(setters :Seq[(M[From[M]], FromParam.Last[Arg]) => From[M] := R]) :Res = {
			val base = setDomain
			val row = table.row[From[M]]
			val param = base.last.mapping
			setAll(setters.map(_ (row, param).anchor(base.left, base)))
		}

		def set(setter :M[From[M]] => From[M] := RowProduct) :Res = {
			val base = setDomain
			set(setter(table.row).anchor(base.left, base))
		}

		def set(setter :(M[From[M]], FromParam.Last[Arg]) => From[M] := R) :Res = {
			val base = setDomain
			set(setter(table.row, base.last.mapping).anchor(base.left, base))
		}
	}




	@nowarn("cat=deprecation")
	trait SupplantClauseFactory[Arg, M[O] <: MappingAt[O], R >: From[M] WithParam Arg <: RowProduct, +Res]
		extends Any with SetClauseDomain[Arg, M, R]
	{
		protected[sql] final def supplantForwarder(update :From[M] := R) :Res = supplant(update)
		protected[sql] final def supplantAllForwarder(updates :Seq[From[M] := R]) :Res =
			supplantAll(updates)

		protected def supplantAll(setters :Seq[From[M] := R]) :Res

		protected def supplant(setter :From[M] := R) :Res = supplantAll(ReversedList :+ setter)

		def supplant(setters :Seq[(M[From[M]], FromParam.Last[Arg]) => From[M] := R]) :Res = {
			val base = setDomain
			val row = table.row[From[M]]
			val param = base.last.mapping
			supplantAll(setters.map(_ (row, param).anchor(base.left, base)))
		}

		def supplant(setter :M[From[M]] => From[M] := RowProduct) :Res = {
			val base = setDomain
			supplant(setter(table.row).anchor(base.left, base))
		}

		def supplant(setter :(M[From[M]], FromParam.Last[Arg]) => From[M] := R) :Res = {
			val base = setDomain
			supplant(setter(table.row, base.last.mapping).anchor(base.left, base))
		}
	}




	@deprecated(SharedImplDeprecation, DeprecatedAlways)
	trait WhereClauseDomain[Arg, M[O] <: MappingAt[O]] extends Any {
		type WhereDomain = From[M] WithParam Arg
		protected def table       :RelVar[M] //= whereDomain.left.table.castWith[Table[M] => RelVar[M]]
		protected def whereDomain :From[M] WithParam Arg
		protected[sql] def whereDomainForwarder :From[M] WithParam Arg = whereDomain
	}


	/** The API of factories - and complete [[net.noresttherein.oldsql.sql.Delete Delete]]`[Arg, M, Int]`
	  * statements - which define `where` and `whereSelf` methods allowing to replace their ''where'' clause
	  * with another SQL [[net.noresttherein.oldsql.sql.GlobalBoolean Boolean]] expression. $methodsInfo
	  * @tparam Arg the type of the only parameter of the statement, in the sense that its ''where'' condition
	  *             is an expression based on [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
	  *             [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
	  * @tparam M   the type of the mapping used for rows of the table affected by this statement.
	  * @tparam Res the specific type of statements created by (and typically also shared with) this instance,
	  *             most often either a subtype of `Delete[Arg, M, Int]`.
	  * @see [[net.noresttherein.oldsql.sql.TableStatement.WhereAllClauseFactory]]
	  * @see [[net.noresttherein.oldsql.sql.TableStatement.WhereAnyClauseFactory]]
	  * @define Arg                   `Arg`
	  * @define Args                  `Arg`
	  * @define Del                   `Delete[Arg, M, Int]`
	  * @define DelLink               [[net.noresttherein.oldsql.sql.Delete Delete]]`[Arg, M, Int]`
	  * @define delResult             deleting all rows in the table for which `condition` evaluates to `true`,
	  *                               returning their number (mind `null` values in ternary logic).
	  * @define methodsInfo           The arguments of those methods take the form of
	  *                               constructor functions accepting either the two mappings `M` of the table and
	  *                               [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]
	  *                               of the statement parameter `Arg`, or only the table mapping `M`
	  *                               for constant-based filters, and returning an SQL Boolean
	  *                               [[net.noresttherein.oldsql.sql.GlobalBoolean expression]].
	  *                               The former functions are accepted by
	  *                               [[net.noresttherein.oldsql.sql.TableStatement.WhereClauseFactory.where(condition:M[* where]]
	  *                               method, while the latter by overloaded `whereSelf`, which accept a number
	  *                               of arguments varying from 1 to 5, all set to the same `M` instance.
	  *                               This repetition allows repeated reference to the table mapping
	  *                               in the expression using `_` placeholders for subsequent arguments.
	  *                               It worKeys the same way as
	  *                               [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where where]]
	  *                               for parameterless statements.
	  * @define mappingConversionInfo are implicitly convertible to SQL expressions of their
	  *                               [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type,
	  *                               representing all columns of the selected components. The conversion
	  *                               will happen if one of the methods of
	  *                               [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] is called
	  *                               on a mapping, with an equality test
	  *                               [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]] being
	  *                               the most common case, followed by ordering comparisons and arithmetic
	  *                               functions such as [[net.noresttherein.oldsql.sql.SQLExpression.> >]] and
	  *                               [[net.noresttherein.oldsql.sql.ColumnSQL.+ +]]. Consult the API of
	  *                               `SQLExpression` and [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]
	  *                               for more information.  Note that complete components can be compared
	  *                               in this way, not only single column properties, which will translate
	  *                               to pair-wise comparisons of individual columns (those without buff
	  *                               [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]])
	  *                               of the component.
	  * @define whereSelfInfo         The function does not accept
	  *                               a [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] mapping
	  *                               for the parameter type $Arg as an argument, which makes this method
	  *                               more convenient for creating filter conditions based on constants
	  *                               (either [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] or
	  *                               [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]] parameters),
	  *                               as only a single placeholder `_` is expected. Any Scala value of type `X`
	  *                               is implicitly convertible to a literal expression in the presence
	  *                               of an implicit [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[X]`,
	  *                               and the latter can be created using extension method
	  *                               [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL.? ?]]
	  *                               available after importing
	  *                               [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL]]
	  *                               Alternatively, any argument of type `X` given to a comparison operator method
	  *                               ending with `?`, such as [[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]],
	  *                               is automatically promoted to a `SQLParameter[X]` internally,
	  *                               if an implicit form for `X` is present.
	  *
	  *                               The result of this method is not particularly useful itself,
	  *                               as it is a parameterized statement not using its parameter. However,
	  *                               it defines [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere.or or]]
	  *                               and [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll.and and]]
	  *                               with signatures mirroring `where` methods of this instance; they allow
	  *                               to provide additional conditions, combined with the one given to this method,
	  *                               which may make use of the statement parameter.
	  * @define whereSelfOverloadInfo In order to avoid the hassle of providing the argument function
	  *                               in its full literal syntax with an explicit argument, overloaded variants
	  *                               of this method exist which accept the same argument mapping various number
	  *                               of times; this allows to reference columns of the deleted rows multiple times
	  *                               in a single expression.
	  * @define examplesInfo          The following expressions are fully equivalent:
	  * {{{
	  * Delete(Characters) whereSelf (_.isMagicUser && _.level >= 9 && _.magicSchool ==? "conjurer")
	  * Delete(Characters) where { mage => mage.isMagicUser && mage.level >= 9 && mage.magicSchool =? "conjurer" }
	  * }}}
	  */
	@nowarn("cat=deprecation")
	trait WhereClauseFactory[Arg, M[O] <: MappingAt[O], +Res] extends Any with WhereClauseDomain[Arg, M] {
		protected[sql] def whereForwarder(condition :GlobalBoolean[From[M] WithParam Arg]) :Res =
			where(condition)

		/** Creates a `Delete` statement with parameter $Args, using the given expression as its ''where'' clause.
		  * This overloaded method variant is useful in more abstract code, where the expression is created
		  * by some other part of the application. If the filter condition is known at the point
		  * of calling this method, its overloaded siblings accepting constructor functions provide
		  * a more convenient way of creating the expression starting from the expressions for the table row
		  * and statement parameter. The expression may be ''unanchored'', and the method is responsible for
		  * [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchoring]] it to the used domain
		  * `From[M] WithParam Arg` instance. This is a delegation target of other overloaded `where`
		  * and `whereSelf` methods. It is currently protected as all parts of the expression must currently use
		  * the exact same [[net.noresttherein.oldsql.sql.UnboundParam.ParamRelation ParamRelation]] instance.
		  * @return a $DelLink $delResult
		  */ //todo: update docs that it assumes condition is anchored
		protected def where(condition :GlobalBoolean[From[M] WithParam Arg]) :Res

		/** Creates a ''delete'' SQL statement parameterized with $Args and with a new ''where'' clause.
		  * The argument function is a constructor of the ''where'' clause, accepting the mappings `M` of the rows
		  * in the affected table and [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]
		  * exposing the parameter $Arg. Both mappings and their components $mappingConversionInfo
		  *
		  * The parameter mapping [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] for $Arg
		  * can be similarly converted to an `SQLExpression`. It does not define any components or columns
		  * as properties and is treated as a composite type based on a previously provided
		  * [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] type class for $Arg.
		  * If $Arg is a complex type, its properties can be accessed by exposing them as components or columns
		  * of the parameter mapping, using methods
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.apply[X<:P=>T,T](pick:X)* apply]],
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.opt[T:SQLForm](pick:P=>Option[T])* opt]],
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.col[T:ColumnForm](pick:P=>T)* col]] and
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.optcol[T:ColumnForm](pick:P=>Option[T])* optcol]],
		  * which create a synthetic [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]]
		  * expression for the parameter property, the usages of which will translate to additional JDBC parameters
		  * initialized with values derived from $Arg:
		  * {{{
		  *     Delete(Monsters) where (_.race === _(_.race))
		  * }}}
		  * @return a $DelLink $delResult
		  */
		def where(condition :(M[From[M] WithParam Arg], FromParam[Arg, RowProduct AndFrom ParamRelation[Arg]#Param])
		                     => GlobalBoolean[From[M] WithParam Arg]) :Res =
			where(condition(table.row[From[M] WithParam Arg], whereDomain.last.mapping).anchor(whereDomain))

		/** Creates a ''delete'' SQL statement parameterized with $Args and with a new ''where'' clause.
		  * The argument function is a constructor of the ''where'' clause, accepting as the single argument
		  * the mapping `M` of the rows in the affected table. The mapping, and any of its components,
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  * @return a $DelLink delResult
		  */
		def where(condition :M[From[M] WithParam Arg] => GlobalBoolean[From[M] WithParam Arg]) :Res =
			where(condition(table.row).anchor(whereDomain))

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and dependent solely on the table as the body of its ''where'' clause.
		  * The function accepts as an argument the mapping `M` of rows from the affected table;
		  * any of its components and subcomponents $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def whereSelf(condition :M[WhereDomain] => GlobalBoolean[WhereDomain]) :Res =
			where(condition(table.row).anchor(whereDomain))

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and dependent solely on the table as the body of its ''where'' clause.
		  * The function accepts as both its arguments the same mapping `M` of rows from
		  * the affected table; any of its components and subcomponents $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def whereSelf(condition :(M[WhereDomain], M[WhereDomain]) => GlobalBoolean[WhereDomain]) :Res =
			where(condition(table.row, table.row).anchor(whereDomain))

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and dependent solely on the table as the body of its ''where'' clause.
		  * The function accepts as all its arguments the same mapping `M` of rows from
		  * the affected table; any of its components and subcomponents $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def whereSelf(condition :(M[WhereDomain], M[WhereDomain], M[WhereDomain]) => GlobalBoolean[WhereDomain]) :Res = {
			val mapping = table.row[WhereDomain]
			where(condition(mapping, mapping, mapping).anchor(whereDomain))
		}

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and dependent solely on the table as the body of its ''where'' clause.
		  * The function accepts as all its arguments the same mapping `M` of rows from
		  * the affected table; any of its components and subcomponents $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def whereSelf(condition :(M[WhereDomain], M[WhereDomain], M[WhereDomain], M[WhereDomain])
		                         => GlobalBoolean[WhereDomain]) :Res =
		{
			val mapping = table.row[WhereDomain]
			where(condition(mapping, mapping, mapping, mapping).anchor(whereDomain))
		}

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and dependent solely on the table as the body of its ''where'' clause.
		  * The function accepts as all its arguments the same mapping `M` of rows from
		  * the affected table; any of its components and subcomponents $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def whereSelf(condition :(M[WhereDomain], M[WhereDomain], M[WhereDomain], M[WhereDomain], M[WhereDomain])
		                         => GlobalBoolean[WhereDomain]) :Res =
		{
			val mapping = table.row[WhereDomain]
			where(condition(mapping, mapping, mapping, mapping, mapping).anchor(whereDomain))
		}
	}



	/** A template trait for [[net.noresttherein.oldsql.sql.Delete Delete]]`[Arg, M, Int]` statements
	  * whose ''where'' clause consists of several conditions joined in a logical disjunction - typically
	  * of type $Or produced by another instance of $Or or a related $AndLink instance.
	  * It abstracts over the more specific statement type created in the result and contains the shared
	  * implementation of [[net.noresttherein.oldsql.sql.TableStatement.WhereAnyClauseFactory.or or]] methods,
	  * which allow expanding of this instance's ''where'' clause. $methodsInfo
	  *
	  * As a rule, instances of a specific subtype of this class produce other instances of this class,
	  * while are created themselves by more specific $AndLink.
	  * $sharedImplInfo
	  * @tparam Arg the type of the only parameter of the statement, in the sense that its ''where'' condition
	  *             is an expression based on [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
	  *             [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
	  * @tparam M   the type of the mapping used for rows of the table affected by this statement.
	  * @tparam Or  the specific type of statements created by (and typically also shared with) this instance,
	  *             most often a subtype of `Delete[Arg, M, Int]`.
	  * @see [[net.noresttherein.oldsql.sql.TableStatement.WhereAllClauseFactory]]
	  * @see [[net.noresttherein.oldsql.sql.TableStatement.WhereClauseFactory]]
	  * @define Arg                                          `Arg`
	  * @define Args                                         `Arg`
	  * @define Or                                           `Or`
	  * @define OrLink                                       [[net.noresttherein.oldsql.sql.TableStatement.WhereAnyClauseFactory GenericDeleteWhereAny]]
	  * @define AndLink                                      subtypes of [[net.noresttherein.oldsql.sql.TableStatement.WhereAllClauseFactory GenericDeleteWhereAll]]
	  * @define Del                                          `Delete[Arg, M, Int]`
	  * @define DelLink                                      [[net.noresttherein.oldsql.sql.Delete Delete]]`[Arg, M, Int]`
	  * @define delResult                                    deleting all rows in the table for which `condition` evaluates to `true`,
	  *                                                      returning their number (mind `null` values in ternary logic).
	  * @define methodsInfo                                  These methods take as their parameter a constructor function accepting either
	  *                                                      the two mappings `M` of the table and
	  *                                                      [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]
	  *                                                      of the parameter $Arg, or only the table mapping `M`
	  *                                                      for constant-based filters, and returning
	  *                                                      an SQL Boolean [[net.noresttherein.oldsql.sql.GlobalBoolean expression]].
	  *                                                      The former functions are accepted by
	  *                                                      [[net.noresttherein.oldsql.sql.TableStatement.WhereAnyClauseFactory.or(condition:M[* or]]
	  *                                                      method, while the latter by overloaded `orSelf`, which accept a number
	  *                                                      of arguments varying from 1 to 5, all set to the same `M` instance.
	  *                                                      This repetition allows multiple references to the table mapping
	  *                                                      in the expression using `_` placeholders for subsequent arguments.
	  *                                                      The most basic variant of `or`, accepting just a Boolean SQL expression and
	  *                                                      to which all other delegate, is left for subclasses to implement.
	  * @define sharedImplInfo                               This allows any number of chained
	  *                                                      [[net.noresttherein.oldsql.sql.TableStatement.WhereAllClauseFactory.and and]]
	  *                                                      method calls, following by any number of chained
	  *                                                      [[net.noresttherein.oldsql.sql.TableStatement.WhereAnyClauseFactory.or or]]
	  *                                                      calls, without a possibility of changing that order.
	  *
	  *                                                      This trait is an extracted shared interface and is an implementation artefact.
	  *                                                      The syntax of factory methods enabled by it (`or` methods) is a part of public,
	  *                                                      supported API, but itself is not. It should not be referenced directly,
	  *                                                      but rather its methods should be called on its concrete subclasses.
	  *                                                      Referencing any subtypes of `Delete`, other than use of
	  *                                                      [[net.noresttherein.oldsql.sql.Delete.implementation.ParamDelete ParamDelete]] and
	  *                                                      [[net.noresttherein.oldsql.sql.Delete.implementation.GroundDelete GroundDelete]]
	  *                                                      as base classes for custom implementations, is in general discouraged.
	  *                                                      The recommended approach is to use the factory expressions as a whole,
	  *                                                      upcasting their results to
	  *                                                      `Delete`/[[net.noresttherein.oldsql.sql.DeleteDML DeleteDMDL]] when complete.
	  *                                                      In cases where the expression must be created in a dynamic, generic way,
	  *                                                      create the condition [[net.noresttherein.oldsql.sql.GlobalBoolean GlobalBoolean]]
	  *                                                      argument (or condition function argument as declared by
	  *                                                      [[net.noresttherein.oldsql.sql.TableStatement.WhereClauseFactory.where where]],
	  *                                                      [[net.noresttherein.oldsql.sql.TableStatement.WhereAllClauseFactory.and and]]
	  *                                                      and [[net.noresttherein.oldsql.sql.TableStatement.WhereAnyClauseFactory.or or]])
	  *                                                      and pass it as a whole. The syntax for the statements is more stable
	  *                                                      than the methods by which it is achieved.
	  * @define mappingConversionInfo are implicitly convertible to SQL expressions of their
	  *                               [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type,
	  *                               representing all columns of the selected components. The conversion
	  *                               will happen if one of the methods of
	  *                               [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] is called
	  *                               on a mapping, with an equality test
	  *                               [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]] being
	  *                               the most common case, followed by ordering comparisons and arithmetic
	  *                               functions such as [[net.noresttherein.oldsql.sql.SQLExpression.> >]] and
	  *                               [[net.noresttherein.oldsql.sql.ColumnSQL.+ +]]. Consult the API of
	  *                               `SQLExpression` and [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]
	  *                               for more information.  Note that complete components can be compared
	  *                               in this way, not only single column properties, which will translate
	  *                               to pair-wise comparisons of individual columns (those without buff
	  *                               [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]])
	  *                               of the component.
	  * @define whereSelfInfo The function does not accept
	  *                                       a [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] mapping
	  *                                       for the parameter type $Arg as an argument, which makes this method
	  *                                       more convenient for creating filter conditions based on constants
	  *                                       (either [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] or
	  *                                       [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]] parameters),
	  *                                       as only a single placeholder `_` is expected. Any Scala value of type `X`
	  *                                       is implicitly convertible to a literal expression in the presence
	  *                                       of an implicit [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[X]`,
	  *                                       and the latter can be created using extension method
	  *                                       [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL.? ?]]
	  *                                       available after importing
	  *                                       [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL]]
	  *                                       Alternatively, any argument of type `X` given to a comparison operator method
	  *                                       ending with `?`, such as [[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]],
	  *                                       is automatically promoted to a `SQLParameter[X]` internally,
	  *                                       if an implicit form for `X` is present.
	  *
	  *                                       While the result of this method doesn't use the parameter $Arg,
	  *                                       it can be expanded by chaining further
	  *                                       [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere.or or]] and
	  *                                       [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll.and and]] calls;
	  *                                       they allow to provide additional conditions, combined with the one given
	  *                                       to this method and and any preceding since the call of
	  *                                       [[net.noresttherein.oldsql.sql.TableStatement.WhereClauseFactory.where where]].
	  * @define whereSelfOverloadInfo In order to avoid the hassle of providing the argument function
	  *                                                      in its full literal syntax with an explicit argument, overloaded variants
	  *                                                      of this method exist which accept the same argument mapping various number
	  *                                                      of times; this allows to reference columns of the deleted rows multiple times
	  *                                                      in a single expression.
	  * @define examplesInfo                                 The following expressions are fully equivalent:
	  * {{{
	  * Delete(Characters) where (_.characterClass === _(_.characterClass)) orSelf
	  *     (_.isMagicUser && _.level >= 9 && _.magicSchool ==? "conjurer")
	  * Delete(Characters) where { (t, p) =>
	  *     t.characterClass === p(_.characterClass) || t.isMagicUser && t.level >= 9 && t.magicSchool =? "conjurer"
	  * }
	  * }}}
	  */
	@nowarn("cat=deprecation")
	trait WhereAnyClauseFactory[Arg, M[O] <: MappingAt[O], +Or] extends Any with WhereClauseDomain[Arg, M] {

		protected[sql] final def orForwarder(condition :GlobalBoolean[From[M] WithParam Arg]) :Or = or(condition)

		/** Creates a `Delete` statement with parameter $Args, joining the ''where'' clause with `condition`
		  * using ''or'' as its ''where'' clause.
		  * This overloaded method variant is useful in more abstract code, where the expression is created
		  * by some other part of the application. If the filter condition is known at the point
		  * of calling this method, its overloaded siblings accepting constructor functions provide a more convenient
		  * way of creating the expression starting from the expressions for the table row and statement parameter.
		  * The expression may be ''unanchored'', and the method is responsible for
		  * [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchoring]] it to the used domain
		  * `From[M] WithParam Arg` instance. This is a delegation target of other overloaded `or` and `orSelf`
		  * methods. It is currently protected as all parts of the expression must currently used the exact same
		  * [[net.noresttherein.oldsql.sql.UnboundParam.ParamRelation ParamRelation]] instance.
		  * @return a $DelLink $delResult
		  */
		protected def or(condition :GlobalBoolean[From[M] WithParam Arg]) :Or

		/** Creates a ''delete'' SQL statement parameterized with $Args, with a ''where'' clause
		  * combining the ''where'' clause of this instance and the given condition with ''or''.
		  * The argument function is a constructor of an alternative condition for the ''where'' clause,
		  * accepting the mappings `M` of the rows in the affected table and
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] exposing the parameter $Arg.
		  * Both mappings and their components $mappingConversionInfo
		  *
		  * The parameter mapping [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] for $Arg
		  * can be similarly converted to an `SQLExpression`. It does not define any components or columns
		  * as properties and is treated as a composite type based on a previously provided
		  * [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] type class for $Arg. If $Arg is a complex type,
		  * its properties can be accessed by exposing them as components or columns of the parameter mapping,
		  * using methods
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.apply[X<:P=>T,T](pick:X)* apply]],
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.opt[T:SQLForm](pick:P=>Option[T])* opt]],
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.col[T:ColumnForm](pick:P=>T)* col]] and
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.optcol[T:ColumnForm](pick:P=>Option[T])* optcol]],
		  * which create a synthetic [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]]
		  * expression for the parameter property, the usages of which will translate to additional JDBC parameters
		  * initialized with values derived from $Arg:
		  * {{{
		  *     Delete(Monsters) where (_.race === "Red " ++: _(_.race)) or (_.race === "Blue " ++: _(_.race))
		  * }}}
		  * @return a $DelLink $delResult
		  */
		def or(condition :(M[From[M] WithParam Arg], FromParam[Arg, RowProduct AndFrom ParamRelation[Arg]#Param])
		                  => GlobalBoolean[From[M] WithParam Arg]) :Or =
			or(condition(table.row[From[M] WithParam Arg], whereDomain.last.mapping).anchor(whereDomain))

		/** Creates a ''delete'' SQL statement parameterized with $Args and with a new ''where'' clause,
		  * joining the ''where'' clause of this instance with the given condition using logical ''or''.
		  * The argument function is a constructor of the ''where'' clause, accepting as the single argument
		  * the mapping `M` of the rows in the affected table. The mapping, and any of its components,
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  * @return a $DelLink delResult
		  */
		def or(condition :M[From[M] WithParam Arg] => GlobalBoolean[From[M] WithParam Arg]) :Or =
			or(condition(table.row).anchor(whereDomain))

		/** Creates a new ''delete'' statement using a logical alternative of the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and this instance's ''where'' clause as its ''where'' clause.
		  * The function depends solely on the table components and accepts as an argument the mapping `M`
		  * of rows from the affected table; any of its components and subcomponents $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def orSelf(condition :M[WhereDomain] => GlobalBoolean[WhereDomain]) :Or =
			or(condition(table.row).anchor(whereDomain))

		/** Creates a new ''delete'' statement using a logical alternative of the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and this instance's ''where'' clause clause as its ''where'' clause.
		  * The function depends solely on the table components and accepts as both its arguments
		  * the same mapping `M` of rows from the affected table; any of its components and subcomponents
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def orSelf(condition :(M[WhereDomain], M[WhereDomain]) => GlobalBoolean[WhereDomain]) :Or =
			or(condition(table.row, table.row).anchor(whereDomain))

		/** Creates a new ''delete'' statement using a logical alternative of the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and this instance's ''where'' clause as its ''where'' clause.
		  * The function depends solely on the table components and accepts as all its arguments
		  * the same mapping `M` of rows from the affected table; any of its components and subcomponents
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def orSelf(condition :(M[WhereDomain], M[WhereDomain], M[WhereDomain]) => GlobalBoolean[WhereDomain]) :Or = {
			val mapping = table.row[WhereDomain]
			or(condition(mapping, mapping, mapping).anchor(whereDomain))
		}

		/** Creates a new ''delete'' statement using a logical alternative of the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and this instance's ''where'' clause as its ''where'' clause.
		  * The function depends solely on the table components and accepts as all its arguments
		  * the same mapping `M` of rows from the affected table; any of its components and subcomponents
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def orSelf(condition :(M[WhereDomain], M[WhereDomain], M[WhereDomain], M[WhereDomain])
		                      => GlobalBoolean[WhereDomain]) :Or =
		{
			val mapping = table.row[WhereDomain]
			or(condition(mapping, mapping, mapping, mapping).anchor(whereDomain))
		}

		/** Creates a new ''delete'' statement using a logical alternative of the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function
		  * and this instance's ''where'' clause as its ''where'' clause.
		  * The function depends solely on the table components and accepts as all its arguments
		  * the same mapping `M` of rows from the affected table; any of its components and subcomponents
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def orSelf(condition :(M[WhereDomain], M[WhereDomain], M[WhereDomain], M[WhereDomain], M[WhereDomain])
		                      => GlobalBoolean[WhereDomain]) :Or =
		{
			val mapping = table.row[WhereDomain]
			or(condition(mapping, mapping, mapping, mapping, mapping).anchor(whereDomain))
		}
	}



	/** A template trait for [[net.noresttherein.oldsql.sql.Delete Delete]]`[Arg, M, Int]` statements
	  * whose ''where'' clause consists of either a single condition passed to one of
	  * [[net.noresttherein.oldsql.sql.TableStatement.WhereClauseFactory.where where]] factory methods,
	  * or several conditions joined in a logical conjunction - typically of type $And produced by another instance
	  * of $And by calling their [[net.noresttherein.oldsql.sql.TableStatement.WhereAllClauseFactory.and and]]
	  * method.
	  * It abstracts over the more specific statement type created in the result and contains the shared
	  * implementation of [[net.noresttherein.oldsql.sql.TableStatement.WhereAllClauseFactory.and and]] methods,
	  * which allow expanding this instance's ''where'' clause. $methodsInfo
	  *
	  * As a rule, instances of a specific subtype of this class produce other instances of this class,
	  * while are created themselves by their more specific $And.
	  * $sharedImplInfo
	  * @tparam Arg the type of the only parameter of the statement, in the sense that its ''where'' condition
	  *             is an expression based on [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtype
	  *             [[net.noresttherein.oldsql.sql.From From]]`[M] `[[net.noresttherein.oldsql.sql.WithParam WithParam]]` Arg`.
	  * @tparam M   the type of the mapping used for rows of the table affected by this statement.
	  * @tparam Or  the specific type of statements created by (and typically also shared with) this instance,
	  *             most often either a subtype of `Delete[Arg, M, Int]`.
	  * @tparam And the 'informal' self type of this instance and the type of the ''delete'' statements created
	  *             by `and` methods.
	  * @see [[net.noresttherein.oldsql.sql.TableStatement.WhereAnyClauseFactory]]
	  * @see [[net.noresttherein.oldsql.sql.TableStatement.WhereClauseFactory]]
	  * @define And          `And`
	  * @define methodsInfo  These methods take as their parameter a constructor function accepting either
	  *                      the two mappings `M` of the table and
	  *                      [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]
	  *                      of the parameter $Arg, or only the table mapping `M` for constant-based filters,
	  *                      and returning an SQL Boolean [[net.noresttherein.oldsql.sql.GlobalBoolean expression]].
	  *                      The former functions are accepted by
	  *                      [[net.noresttherein.oldsql.sql.TableStatement.WhereAllClauseFactory.and(condition:M[* and]]
	  *                      method, while the latter by overloaded `andSelf`, which accept a number of arguments
	  *                      varying from 1 to 5, all set to the same `M` instance. This repetition allows multiple
	  *                      references to the table mapping in the expression using `_` placeholders for subsequent
	  *                      arguments. The most basic variant of `and`, accepting just a Boolean SQL expression and
	  *                      to which all other delegate, is left for subclasses to implement.
	  * @define examplesInfo The following expressions are fully equivalent:
	  * {{{
	  *     Delete(Characters) where (_.race === _(_.race)) andSelf
	  *         (_.isMagicUser && _.level >= 9 && _.magicSchool ==? "conjurer")
	  *     Delete(Characters) where { (t, p) =>
	  *         t.race === p(_.race) && t.isMagicUser && t.level >= 9 && t.magicSchool =? "conjurer"
	  *     }
	  * }}}
	  */
	trait WhereAllClauseFactory[Arg, M[O] <: MappingAt[O], +Or, +And] extends Any with WhereAnyClauseFactory[Arg, M, Or] {

		protected[sql] final def andForwarder(condition :GlobalBoolean[From[M] WithParam Arg]) :And = and(condition)

		/** Creates a `Delete` statement with parameter $Args, joining the ''where'' clause with `condition`
		  * using ''and'' as its ''where'' clause.
		  * The expression may be ''unanchored'', and the method is responsible for
		  * [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchoring]] it to the used domain
		  * `From[M] WithParam Arg` instance. This is a delegation target of other overloaded `and` and `andSelf`
		  * methods. It is currently protected as all parts of the expression must currently used the exact same
		  * [[net.noresttherein.oldsql.sql.UnboundParam.ParamRelation ParamRelation]] instance.
		  * @return a $DelLink $delResult
		  */
		protected def and(condition :GlobalBoolean[From[M] WithParam Arg]) :And

		/** Creates a ''delete'' SQL statement parameterized with $Args, with a ''where'' clause
		  * combining the ''where'' clause of this instance and the given condition with ''and''.
		  * The argument function is a constructor of an additional condition for the ''where'' clause,
		  * accepting the mappings `M` of the rows in the affected table and
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] exposing the parameter $Arg.
		  * Both mappings and their components $mappingConversionInfo
		  *
		  * The parameter mapping [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] for $Arg
		  * can be similarly converted to an `SQLExpression`. It does not define any components or columns
		  * as properties and is treated as a composite type based on a previously provided
		  * [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] type class for $Arg. If $Arg is a complex type,
		  * its properties can be accessed by exposing them as components or columns of the parameter mapping,
		  * using methods
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.apply[X<:P=>T,T](pick:X)* apply]],
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.opt[T:SQLForm](pick:P=>Option[T])* opt]],
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.col[T:ColumnForm](pick:P=>T)* col]] and
		  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam.optcol[T:ColumnForm](pick:P=>Option[T])* optcol]],
		  * which create a synthetic [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]]
		  * expression for the parameter property, the usages of which will translate to additional JDBC parameters
		  * initialized with values derived from $Arg:
		  * {{{
		  *     Delete(Monsters) where (_.race === "Red " ++: _(_.race)) and (_.race === "Blue " ++: _(_.race))
		  * }}}
		  * @return a $DelLink $delResult
		  */
		def and(condition :(M[From[M] WithParam Arg], FromParam[Arg, RowProduct AndFrom ParamRelation[Arg]#Param])
		                   => GlobalBoolean[From[M] WithParam Arg]) :And =
			and(condition(table.row[From[M] WithParam Arg], whereDomain.last.mapping).anchor(whereDomain))

		/** Creates a ''delete'' SQL statement parameterized with $Args and with a new ''where'' clause,
		  * joining the ''where'' clause of this instance with the given condition using ''and''.
		  * The argument function is a constructor of the ''where'' clause, accepting as the single argument
		  * the mapping `M` of the rows in the affected table. The mapping, and any of its components,
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  * @return a $DelLink delResult
		  */
		def and(condition :M[From[M] WithParam Arg] => GlobalBoolean[From[M] WithParam Arg]) :And =
			and(condition(table.row).anchor(whereDomain))

		/** Creates a new ''delete'' statement with a new ''where'' clause,
		  * joining the ''where'' clause of this instance with the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function using ''and''.
		  * The function depends solely on the table components and accepts as an argument the mapping `M`
		  * of rows from the affected table; any of its components and subcomponents $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def andSelf(condition :M[WhereDomain] => GlobalBoolean[WhereDomain]) :And =
			and(condition(table.row).anchor(whereDomain))

		/** Creates a new ''delete'' statement with a new ''where'' clause,
		  * joining the ''where'' clause of this instance with the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function using ''and''.
		  * The function depends solely on the table components and accepts as both its arguments
		  * the same mapping `M` of rows from the affected table; any of its components and subcomponents
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def andSelf(condition :(M[WhereDomain], M[WhereDomain]) => GlobalBoolean[WhereDomain]) :And =
			and(condition(table.row, table.row).anchor(whereDomain))

		/** Creates a new ''delete'' statement with a new ''where'' clause,
		  * joining the ''where'' clause of this instance with the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function using ''and''.
		  * The function depends solely on the table components and accepts as all its arguments
		  * the same mapping `M` of rows from the affected table; any of its components and subcomponents
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def andSelf(condition :(M[WhereDomain], M[WhereDomain], M[WhereDomain]) => GlobalBoolean[WhereDomain]) :And = {
			val mapping = table.row[WhereDomain]
			and(condition(mapping, mapping, mapping).anchor(whereDomain))
		}

		/** Creates a new ''delete'' statement with a new ''where'' clause,
		  * joining the ''where'' clause of this instance with the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function using ''and''.
		  * The function depends solely on the table components and accepts as all its arguments
		  * the same mapping `M` of rows from the affected table; any of its components and subcomponents
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def andSelf(condition :(M[WhereDomain], M[WhereDomain], M[WhereDomain], M[WhereDomain])
		                       => GlobalBoolean[WhereDomain]) :And =
		{
			val mapping = table.row[WhereDomain]
			and(condition(mapping, mapping, mapping, mapping).anchor(whereDomain))
		}

		/** Creates a new ''delete'' statement with a new ''where'' clause,
		  * joining the ''where'' clause of this instance with the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function using ''and''.
		  * The function depends solely on the table components and accepts as all its arguments
		  * the same mapping `M` of rows from the affected table; any of its components and subcomponents
		  * $mappingConversionInfo
		  *
		  * $whereSelfInfo
		  *
		  * $whereSelfOverloadInfo
		  * $examplesInfo
		  * @return a $DelLink $delResult
		  */
		def andSelf(condition :(M[WhereDomain], M[WhereDomain], M[WhereDomain], M[WhereDomain], M[WhereDomain])
		                       => GlobalBoolean[WhereDomain]) :And =
		{
			val mapping = table.row[WhereDomain]
			and(condition(mapping, mapping, mapping, mapping, mapping).anchor(whereDomain))
		}
	}






	trait GroundSetClauseFactory[M[O] <: MappingAt[O], +R <: RowProduct, +Res] extends Any {
		protected def table     :RelVar[M]
		protected def domain    :From[M]
		protected def setDomain :R

		def setAll(setters :Seq[From[M] := R]) :Res

		def setAll(setters :M[From[M]] => Seq[From[M] := R]) :Res = {
			val left = domain
			val right = setDomain
			setAll(setters(table.row).map(_.anchor(left, right)))
		}

		def set(setter :From[M] := R) :Res = setAll(ReversedList :+ setter)

		def set(setters :Seq[M[From[M]] => (From[M] := R)]) :Res = {
			val left = domain
			val right = setDomain
			setAll(setters.map(_(table.row[From[M]]).anchor(left, right)))
		}

		def set(setter :M[From[M]] => From[M] := R) :Res =
			set(setter(table.row[From[M]]).anchor(domain, setDomain))

		def set(setter :(M[From[M]], M[From[M]]) => From[M] := R) :Res = {
			val row = table.row[From[M]]
			set(setter(row, row).anchor(domain, setDomain))
		}

		def set(setter :(M[From[M]], M[From[M]], M[From[M]]) => From[M] := R) :Res = {
			val row = table.row[From[M]]
			set(setter(row, row, row).anchor(domain, setDomain))
		}

		def set(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => From[M] := R) :Res = {
			val row = table.row[From[M]]
			set(setter(row, row, row, row).anchor(domain, setDomain))
		}

		def set(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => From[M] := R) :Res = {
			val row = table.row[From[M]]
			set(setter(row, row, row, row, row).anchor(domain, setDomain))
		}

		def set(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
			             M[From[M]]) => From[M] := R) :Res =
		{
			val row = table.row[From[M]]
			set(setter(row, row, row, row, row, row).anchor(domain, setDomain))
		}

		def set(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
			             M[From[M]], M[From[M]]) => From[M] := R) :Res =
		{
			val row = table.row[From[M]]
			set(setter(row, row, row, row, row, row, row).anchor(domain, setDomain))
		}

		def set(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
			             M[From[M]], M[From[M]], M[From[M]]) => From[M] := R) :Res =
		{
			val row = table.row[From[M]]
			set(setter(row, row, row, row, row, row, row, row).anchor(domain, setDomain))
		}

		def set(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
			             M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => From[M] := R) :Res =
		{
			val row = table.row[From[M]]
			set(setter(row, row, row, row, row, row, row, row, row).anchor(domain, setDomain))
		}

		def set(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
			             M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => From[M] := R) :Res =
		{
			val row = table.row[From[M]]
			set(setter(row, row, row, row, row, row, row, row, row, row).anchor(domain, setDomain))
		}
	}



	trait GroundSupplantClauseFactory[M[O] <: MappingAt[O], +R <: RowProduct, +Res] extends Any {
		protected def table     :RelVar[M]
		protected def domain    :From[M]
		protected def setDomain :R

		def supplantAll(setters :Seq[From[M] := R]) :Res

		def supplant(setter :From[M] := R) :Res = supplantAll(ReversedList :+ setter)

		def supplant(setters :Seq[M[From[M]] => (From[M] := R)]) :Res =
			supplantAll(setters.map(_(table.row[From[M]]).anchor(domain, setDomain)))

		def supplant(setter :M[From[M]] => From[M] := R) :Res =
			supplant(setter(table.row[From[M]]).anchor(domain, setDomain))

		def supplant(setter :(M[From[M]], M[From[M]]) => (From[M] := R)) :Res = {
			val row = table.row[From[M]]
			supplant(setter(row, row).anchor(domain, setDomain))
		}

		def supplant(setter :(M[From[M]], M[From[M]], M[From[M]]) => (From[M] := R)) :Res = {
			val row = table.row[From[M]]
			supplant(setter(row, row, row).anchor(domain, setDomain))
		}

		def supplant(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => (From[M] := R)) :Res = {
			val row = table.row[From[M]]
			supplant(setter(row, row, row, row).anchor(domain, setDomain))
		}

		def supplant(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => (From[M] := R)) :Res = {
			val row = table.row[From[M]]
			supplant(setter(row, row, row, row, row).anchor(domain, setDomain))
		}

		def supplant(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
		                      M[From[M]]) => (From[M] := R)) :Res =
		{
			val row = table.row[From[M]]
			supplant(setter(row, row, row, row, row, row).anchor(domain, setDomain))
		}

		def supplant(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
		                      M[From[M]], M[From[M]]) => (From[M] := R)) :Res =
		{
			val row = table.row[From[M]]
			supplant(setter(row, row, row, row, row, row, row).anchor(domain, setDomain))
		}

		def supplant(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
		                      M[From[M]], M[From[M]], M[From[M]]) => (From[M] := R)) :Res =
		{
			val row = table.row[From[M]]
			supplant(setter(row, row, row, row, row, row, row, row).anchor(domain, setDomain))
		}

		def supplant(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
		                      M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => (From[M] := R)) :Res =
		{
			val row = table.row[From[M]]
			supplant(setter(row, row, row, row, row, row, row, row, row).anchor(domain, setDomain))
		}

		def supplant(setter :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
		                      M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => (From[M] := R)) :Res =
		{
			val row = table.row[From[M]]
			supplant(setter(row, row, row, row, row, row, row, row, row, row).anchor(domain, setDomain))
		}
	}



	/** Interface with methods for creating parameterless [[net.noresttherein.oldsql.sql.Delete Delete]]`[(), M, Int]`
	  * statements using a provided SQL Boolean [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] as their
	  * ''where'' clause. The condition can be passed either directly as a value, or be a result
	  * of a constructor function accepting the table mapping `M[From[M]]`. These are passed
	  * to overloaded `where` methods, accepting functions of arity varying from 1 to 10,
	  * with identical copies of the table mapping `M[From[M]]` of `this.table` assigned to all arguments,
	  * and returning a [[net.noresttherein.oldsql.sql.GlobalBoolean GlobalBoolean]]`[From[M]]`. The mapping given
	  * as the arguments, as well as any of its components, are implicitly convertible
	  * to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] of the appropriate value type and thus
	  * can be used directly in larger SQL expressions forming the whole condition. Repeated declarations
	  * of the same parameter allows to use placeholder `_` expression for the mapping of the whole row
	  * as many times as needed, without resorting to the full lambda expression syntax with explicit declaration
	  * of parameters. The `_` placeholder here always refers to the table mapping, as created statements
	  * are all parameterless:
	  * {{{
	  *     Delete from where (_.fireResistance > _.coldResistance || _.magicResistance >= 25.?)
	  * }}}
	  * The return type of all `where` methods is generic `Delete[(), M, Int]`, which does not
	  * support following the expression with [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhereAll.and and]]
	  * or [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteWhere.or or]] methods; this is unnecessary
	  * here, as the overloaded `where` allows to the reference the table the required number of times
	  * by placeholder `_` parameters, rather than just once as in parameterized statements, where
	  * `where`, `and` and `or` accept a function of two arguments: the table mapping and the parameter mapping,
	  * which requires named lambda parameters (and thus the longer syntax) to reference any of them
	  * more than once - hence the availability of `and`/`or` methods.
	  * @see [[net.noresttherein.oldsql.sql.Delete.from]] - `Delete from table` creates
	  *      a [[net.noresttherein.oldsql.sql.Delete.syntax.DeleteAll DeleteAll]] statement extending this trait.
	  */
	trait GroundWhereClauseFactory[M[O] <: MappingAt[O], +Res] extends Any {
		protected def table  :RelVar[M]
		protected def domain :From[M] //= From(table)

		//consider: this could take any valid (parameterized) FromClause in theory with a help of a type class
		/** Creates a parameterless ''delete'' statement working on this table
		  * and using `condition` as its ''where'' clause.
		  * This is the basic variant of the overloaded method `where`; other methods allow defining the condition
		  * as a function of one or more references to the mapping `M` instance of the table.
		  */
		def where(condition :GlobalBoolean[From[M]]) :Res

		//From[M] makes including of subselects more difficult
		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as an argument the mapping `M` of rows
		  * from the affected table; any of its components and subcomponents, exposed as its properties
		  * (from columns to the whole row represented by `M` itself), are implicitly convertible
		  * to an SQL expression with the value type equal
		  * to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type of the converted component.
		  * This allows its direct use in comparisons defined as methods
		  * of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], such as
		  *
		  * `component `[[net.noresttherein.oldsql.sql.SQLExpression.=== ===]]` param.?`,
		  *
		  * `component `[[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]]` param`,
		  *
		  * `component `[[net.noresttherein.oldsql.sql.SQLExpression.< <]]` param`,
		  *
		  * and arithmetic expressions for distinct types:
		  *
		  * `column `[[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL.+]]` 42`.
		  *
		  * Any such composite expressions can be used as parts of the returned condition.
		  * As the created expression doesn't declare [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters,
		  * values of all expressions other than those referring to the affected table must be known - either
		  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] or
		  * [[net.noresttherein.oldsql.sql.ast.SQLParameter bound]] parameters. The latter can be created
		  * using extension method [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL.? ?]]
		  * available after importing [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL]]
		  * (it is also present in packages [[net.noresttherein.oldsql.sql.lowercase sql.lowercase]]
		  * and [[net.noresttherein.oldsql.sql.uppercase]]). Alternatively, any argument of type `V`
		  * given to a comparison operator method ending with `?` is automatically promoted to a `SQLParameter[V]`
		  * internally.
		  *
		  * In order to avoid the hassle of providing the argument function in its full literal syntax with
		  * an explicit argument, overloaded variants of this method exist which accept the same argument mapping
		  * two or more times; this allows to reference columns of the deleted rows multiple times in a single
		  * expression. The following expressions are fully equivalent:
		  * {{{
		  *     Delete from Monsters where { monster => monster.strength > monster.intelligence }
		  *     Delete from Monsters where (_.strength > _.intelligence)
		  * }}}
		  * Overloaded variants exist for other argument arities, offering flexibility in how the expression
		  * refers to the table rows.
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :M[From[M]] => GlobalBoolean[From[M]]) :Res =
			where(condition(table.row).anchor(domain))

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as arguments two copies the mapping `M` of rows from
		  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
		  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
		  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  * of the converted component. This allows their use directly as subexpressions in comparison tests,
		  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
		  * as the argument for both of the parameters; this repetition does not provide any additional information
		  * or functionality, but instead allows to refer to the table twice in the same expression using
		  * placeholder `_`: `where (_.fireResistance > _.coldResistance)`. Overloaded variants exist
		  * for other argument arities, offering flexibility in how the expression refers to the table rows.
		  * For additional details, see the documentation of the method variant for a single argument function:
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :(M[From[M]], M[From[M]]) => GlobalBoolean[From[M]]) :Res =
			where(condition(table.row, table.row).anchor(domain))

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as arguments three copies the mapping `M` of rows from
		  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
		  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
		  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  * of the converted component. This allows their use directly as subexpressions in comparison tests,
		  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
		  * as the argument for all of the parameters; this repetition does not provide any additional information
		  * or functionality, but instead allows to refer to the table three times in the same expression
		  * using placeholder `_`:
		  * {{{
		  *     where (_.isMagicUser && _.level >= 9 && _.magicSchool ==? "conjurer")
		  * }}}
		  * Overloaded variants exist for other argument arities, offering flexibility
		  * in how the expression refers to the table rows.
		  * For additional details, see the documentation of the method variant for a single argument function:
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :(M[From[M]], M[From[M]], M[From[M]]) => GlobalBoolean[From[M]]) :Res = {
			val row = table.row[From[M]]
			where(condition(row, row, row).anchor(domain))
		}

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as arguments four copies the mapping `M` of rows from
		  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
		  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
		  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  * of the converted component. This allows their use directly as subexpressions in comparison tests,
		  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
		  * as the argument for all of the parameters; this repetition does not provide any additional information
		  * or functionality, but instead allows to refer to the table four times in the same expression
		  * using placeholder `_`:
		  * {{{
		  *     where (_.meleeWeapon.damage + _.strengthBonus > _.rangedWeapon.damage + _.dexterityBonus)
		  * }}}
		  * Overloaded variants exist for other argument arities, offering flexibility
		  * in how the expression refers to the table rows.
		  * For additional details, see the documentation of the method variant for a single argument function:
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => GlobalBoolean[From[M]]) :Res = {
			val row = table.row[From[M]]
			where(condition(row, row, row, row).anchor(domain))
		}

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as arguments five copies the mapping `M` of rows from
		  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
		  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
		  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  * of the converted component. This allows their use directly as subexpressions in comparison tests,
		  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
		  * as the argument for all of the parameters; this repetition does not provide any additional information
		  * or functionality, but instead allows to refer to the table multiple times in the same expression
		  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
		  * in how the expression refers to the table rows.
		  * For additional details, see the documentation of the method variant for a single argument function:
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]]) => GlobalBoolean[From[M]])
				:Res =
		{
			val row = table.row[From[M]]
			where(condition(row, row, row, row, row).anchor(domain))
		}

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as arguments six copies the mapping `M` of rows from
		  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
		  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
		  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  * of the converted component. This allows their use directly as subexpressions in comparison tests,
		  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
		  * as the argument for all of the parameters; this repetition does not provide any additional information
		  * or functionality, but instead allows to refer to the table multiple times in the same expression
		  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
		  * in how the expression refers to the table rows.
		  * For additional details, see the documentation of the method variant for a single argument function:
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]])
		                     => GlobalBoolean[From[M]]) :Res =
		{
			val row = table.row[From[M]]
			where(condition(row, row, row, row, row, row).anchor(domain))
		}

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as arguments seven copies the mapping `M` of rows from
		  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
		  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
		  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  * of the converted component. This allows their use directly as subexpressions in comparison tests,
		  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
		  * as the argument for all of the parameters; this repetition does not provide any additional information
		  * or functionality, but instead allows to refer to the table multiple times in the same expression
		  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
		  * in how the expression refers to the table rows.
		  * For additional details, see the documentation of the method variant for a single argument function:
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]])
		                     => GlobalBoolean[From[M]]) :Res =
		{
			val row = table.row[From[M]]
			where(condition(row, row, row, row, row, row, row).anchor(domain))
		}

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as arguments eight copies the mapping `M` of rows from
		  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
		  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
		  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  * of the converted component. This allows their use directly as subexpressions in comparison tests,
		  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
		  * as the argument for all of the parameters; this repetition does not provide any additional information
		  * or functionality, but instead allows to refer to the table multiple times in the same expression
		  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
		  * in how the expression refers to the table rows.
		  * For additional details, see the documentation of the method variant for a single argument function:
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
		                      M[From[M]]) => GlobalBoolean[From[M]]) :Res =
		{
			val row = table.row[From[M]]
			where(condition(row, row, row, row, row, row, row, row).anchor(domain))
		}

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as arguments nine copies the mapping `M` of rows from
		  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
		  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
		  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  * of the converted component. This allows their use directly as subexpressions in comparison tests,
		  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
		  * as the argument for all of the parameters; this repetition does not provide any additional information
		  * or functionality, but instead allows to refer to the table multiple times in the same expression
		  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
		  * in how the expression refers to the table rows.
		  * For additional details, see the documentation of the method variant for a single argument function:
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
		                      M[From[M]], M[From[M]]) => GlobalBoolean[From[M]]) :Res =
		{
			val row = table.row[From[M]]
			where(condition(row, row, row, row, row, row, row, row, row).anchor(domain))
		}

		/** Creates a new, parameterless ''delete'' statement using the Boolean SQL
		  * [[net.noresttherein.oldsql.sql.ColumnSQL expression]] returned by the argument function as the body
		  * of its ''where'' clause. The function accepts as arguments ten copies the mapping `M` of rows from
		  * the affected table; any of its components and subcomponents, exposed as its properties (from columns
		  * to the whole row represented by `M` itself), are implicitly convertible to an SQL expression
		  * with the value type equal to the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type
		  * of the converted component. This allows their use directly as subexpressions in comparison tests,
		  * arithmetic operations or as SQL function arguments. The exact same mapping instance is given
		  * as the argument for all of the parameters; this repetition does not provide any additional information
		  * or functionality, but instead allows to refer to the table multiple times in the same expression
		  * using placeholder `_`. Overloaded variants exist for other argument arities, offering flexibility
		  * in how the expression refers to the table rows.
		  * For additional details, see the documentation of the method variant for a single argument function:
		  * [[net.noresttherein.oldsql.sql.Delete.syntax.GroundDeleteWhereFactory.where(condition:M* where]].
		  * @return a statement deleting the rows in `table` for which the boolean expression returned by `condition`
		  *         evaluates to `true` (take note of ternary logic and `null` values).
		  */
		def where(condition :(M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]], M[From[M]],
		                      M[From[M]], M[From[M]], M[From[M]]) => GlobalBoolean[From[M]]) :Res =
		{
			val row = table.row[From[M]]
			where(condition(row, row, row, row, row, row, row, row, row, row))
		}
	}






	private[sql] def supplant[S, M[O] <: BaseMapping[S, O], D <: RowProduct]
	                         (op :OperationType, table :RelVar[M],
	                          defaults :Seq[ColumnSetter[From[M], D, _]], overrides :Seq[From[M] := D])
			:Seq[ColumnSetter[From[M], D, _]] =
	{
		var indexed = defaults.groupBy(_.lvalue.component.mapping) map {
			case (col, seq) =>
				if (seq.sizeIs != 1)
					throw Bug(s"Multiple setters for the same column $col: $seq. " +
					          s"Cannot insert into $table supplant $overrides.")
				(col.castWith[ColumnMapping[_, _] => ColumnMapping[_, From[M]]], seq.head)
		}
		var result :List[ColumnSetter[From[M], D, _]] = Nil
		//todo: split may fail
		var iter = overrides.view.flatMap(_.split(op)).to(ReversedList).reverseIterator
		while (iter.hasNext) {
			val setter = iter.next()
			val column = setter.lvalue.component.mapping.castTo[ColumnMapping[_, _], ColumnMapping[_, From[M]]]
			val left = indexed.size
			indexed = indexed - column
			if (indexed.size != left)
				result = setter :: result
		}
		iter = defaults.reverseIterator
		while (iter.hasNext) {
			val setter = iter.next()
			val column = setter.lvalue.component.mapping.castTo[ColumnMapping[_, _], ColumnMapping[_, From[M]]]
			if (indexed.contains(column))
				result = setter :: result
		}
		result
	}


	private[sql] def whereDomain[S, M[O] <: BaseMapping[S, O]](table :RelVar[M]) :From[M] WithParam S = {
		val mapping = table.export[From[M] WithParam S].asInstanceOf[RefinedMapping[S, From[M] WithParam S]]
		From(table).param[S](mapping.selectForm <> mapping.filterForm)
	}


	private[sql] def whereEntity[S, M[O] <: BaseMapping[S, O]]
	                            (table :RelVar[M], domain :From[M] WithParam S = null)
			:GlobalBoolean[From[M] WithParam S] =
	{
		type F = RowProduct AndFrom M WithParam S
		val mapping = table.export[F].asInstanceOf[RefinedMapping[S, F]]
		val from = if (domain != null) domain else whereDomain[S, M](table)
		val param = from.last.mapping //from.right.apply[WithParam.FromLast[S]]
		val row = from.relations.prev
		val entity = from.last
		//todo: probably should use primary key - or assume entity mappins buff everything else with NoFilterByDefault
		(True[F] /: mapping.filteredByDefault) { (cond, col) =>
			def filter[T](column :ColumnMapping[T, F]) :GlobalBoolean[F] =
				row \ column ===
					entity \ (param.col(mapping(column))(column.form) :ColumnMapping[T, JoinParam.Last[S]])
			cond && filter(col)
		}
	}

	private[sql] def whereEntity[S, M[O] <: BaseMapping[S, O]](domain :From[M], value :S) :GlobalBoolean[From[M]] = {
		type F = RowProduct AndFrom M
//		val mapping = table.export.asInstanceOf[RefinedMapping[S, From[M]]]//.castWith[Mapping => RefinedMapping[S, From[M]]]
		val mapping = domain.last.mapping
		(True[F] /: mapping.filteredByDefault) { (cond, col) =>
			def filter[T](column :ColumnMapping[T, F]) :GlobalBoolean[F] =
				mapping(column).opt(value) match {
					case Got(columnValue) =>
						domain.last \ column === SQLParameter(column.form)(columnValue)
					case _ => True
				}
			cond && filter(col)
		}
	}

	private[sql] def whereEntity[S, M[O] <: BaseMapping[S, O]](table :RelVar[M], value :S) :GlobalBoolean[From[M]] =
		whereEntity(From(table), value)

	private[sql] def repeatedWhere[Arg, S, M[O] <: BaseMapping[S, O]]
	                              (domain :From[M] WithParam Seq[Arg], whereDomain :From[M] WithParam Arg,
	                               condition :GlobalBoolean[From[M] WithParam Arg], max :Int)
	        :GlobalBoolean[From[M] WithParam Seq[Arg]] =
		(0 until max).map { i =>
			SQLScribe.replaceParam(whereDomain, domain, whereDomain.last.toRelationSQL, domain.last.toRelationSQL)(
				Optional { args :Seq[Arg] => if (args.sizeIs > i) Got(args(i)) else Lack }
			)(condition)
		}.foldLeft(False[From[M] WithParam Seq[Arg]])(_ || _)

}



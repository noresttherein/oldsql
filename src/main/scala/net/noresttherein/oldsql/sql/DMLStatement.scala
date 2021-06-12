package net.noresttherein.oldsql.sql

import java.sql.{CallableStatement, PreparedStatement, ResultSet, SQLException}

import scala.collection.{EvidenceIterableFactory, Factory, IterableFactory}
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import com.sun.rowset.CachedRowSetImpl
import net.noresttherein.oldsql.{DeprecatedAlways, OperationType, SharedImplDeprecation}
import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{Bug, IllegalResultArityException, MissingStatementResultBug, TooManyResultsException}
import net.noresttherein.oldsql.morsels.ComparableFactory
import net.noresttherein.oldsql.morsels.Extractor.Optional
import net.noresttherein.oldsql.pixies.{CachedUpdateCountStatement, CallableStatementOutParams}
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.{RelVar, Table}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.Call.{CallVisitor, CaseCall}
import net.noresttherein.oldsql.sql.Delete.implementation.{CaseDelete, DeleteVisitor}
import net.noresttherein.oldsql.sql.DML.{BoundDML, ComposedDML, DMLAdapter, DMLAPI, RepeatedDML}
import net.noresttherein.oldsql.sql.DMLStatement.{AlteredResultStatement, BoundStatement, ComposedStatement, DMLStatementAPI, StatementResult, StatementVisitor}
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult.{BatchResult, LargeUpdateCount, MappedResult, RepeatedResult, UpdateCount}
import net.noresttherein.oldsql.sql.Insert.implementation.{CaseInsert, InsertVisitor}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, ParamRelation}
import net.noresttherein.oldsql.sql.Update.implementation.{CaseUpdate, UpdateVisitor}
import net.noresttherein.oldsql.sql.ast.SQLTerm.{False, SQLParameter, True}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.Returning.implementation.{CaseReturning, ReturningVisitor}

//implicits
import net.noresttherein.oldsql.slang._






/** An abstract representation of a DML statement, such as [[net.noresttherein.oldsql.sql.Insert Insert]] or
  * [[net.noresttherein.oldsql.sql.Update Update]], translating into a [[java.sql.PreparedStatement PreparedStatement]]
  * or a sequence of such statements. The exact textual form of the statement passed to the driver is not specified
  * by this class, but concertised by a [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]] when this instance
  * is converted into an executable [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[Args, Res]` by
  * [[net.noresttherein.oldsql.sql.DML.chant chant]] method. The `Args` type parameter defines the type of arguments
  * accepted by this object and is not directly related to JDBC parameters passed to the `PreparedStatement`.
  * The main subclass of this trait is [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]], which represents
  * a single execution of one DML statement, generally as an abstract syntax tree. Other implementations do not
  * specify the relationship between the type of the value(s) returned by the statement and the return type `Res`
  * of this instance or anything about its nature and implementation. Standard implementations are derived from it
  * are adapters of `DML` and `DMLStatement` instances, but nothing is assumed by the clients of this interface:
  *   1. [[net.noresttherein.oldsql.sql.DML.ComposedDML ComposedDML]] adapts another `DML` instance to a new argument
  *      type.
  *   1. [[net.noresttherein.oldsql.sql.DML.BoundDML BoundDML]] combines another `DML` instance with a single value
  *      of its parameter(s), converting it into a parameterless statement;
  *   1. [[net.noresttherein.oldsql.sql.DML.RepeatedDML RepeatedDML]] executes another `DML` instance multiple times,
  *      converting it from argument type `Args` to `Seq[Args]`. This can result in multiple round trips or a batched
  *      statement.
  *   1. [[net.noresttherein.oldsql.sql.DML.Empty Empty]] a no-op instance.
  * Other implementations are possible, but would likely be unable to benefit from the standard SQL rendering
  * mechanism of [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]], and thus require independent,
  * manual formatting (or at least a custom [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]).
  * @tparam Args The types of the argument(s) accepted by this DML and the executable `Incantation` resulting from it.
  *              Multiple arguments are often passed as a [[net.noresttherein.oldsql.collection.Chain Chain]].
  * @tparam Res  The type of the value returned by the resulting `Incantation`.
  */ //consider: renaming to Statements
trait DML[-Args, +Res] extends DMLAPI[Args, Res, DML] with Serializable {
	override def compose[X](f :X => Args) :DML[X, Res] =
		new ComposedDML.Base[X, Args, Res, DML](this, f)
			with ComposedDML[X, Args, Res] with ComposedDML.Impl[X, Args, Res, DML]

	override def bind(args :Args) :DML[Unit, Res] =
		new BoundDML.Base[Args, Res, DML](this, args)
			with BoundDML[Args, Res] with BoundDML.Impl[Args, Res, DML]

	/** The value of the public `hashCode` method, cached and reused. */
	protected def initHashCode :Int
	@transient @volatile private var hashCodeCached :Boolean = false
	@transient @volatile private var cachedHashCode :Int = _

	override def hashCode :Int = //todo: look up how lazy val is implemented now
		if (hashCodeCached)
			cachedHashCode
		else {
			val hash = initHashCode
			cachedHashCode = hash
			hashCodeCached = true
			hash
		}

	/** The value of the public `toString` method, cached and reused. */
	protected def initToString :String
	@transient @volatile var cachedToString :String = _

	override def toString :String = {
		var s = cachedToString
		if (s == null) {
			s = initToString
			cachedToString = s
		}
		s
	}
}




object DML {
	type * = DML[Nothing, Any]

	/** Public API declaring methods available on [[net.noresttherein.oldsql.sql.DML DML]] and its specialization `S`.
	  * This trait is variant in all its type parameter, requiring the same variance from the type constructor `S`
	  * of the described `DML` subtype to share this variance.
	  * @tparam Args the type of parameters accepted by this statement(s), translating to the arguments of
	  *              [[net.noresttherein.oldsql.sql.Incantation incantations]] created from this instance.
	  * @tparam Res  the return type of the executable `Incantation` created from this instance.
	  * @tparam S    a type constructor of `DML` subclasses of this API; it is the type returned by all the methods
	  *              and also expected self-type of implementors of this trait (although it is not enforced).
	  */
	trait DMLAPI[-Args, +Res, +S[-X, +Y] <: DMLAPI[X, Y, S]] {
		/** Adapt this DML to a different parameter(s) type. */
		def compose[X](f :X => Args) :S[X, Res]

		/** Combine this DML with a single value of its parameters, creating a parameterless instance. */
		def bind(args :Args) :S[Unit, Res]

		/** 'Spell out' this DML into a textual form understandable by the targeted DBMS, converting it into
		  * an executable object.
		  * @param dialect A ''strategy'' translating abstract syntax trees (or even higher-level objects) into
		  *                SQL/DML, hiding any DBMS-specific features.
		  */
		def chant(implicit dialect :SQLDialect = StandardSQL) :Incantation[Args, Res]

		/** By default defined as the equality of classes. */
		def canEqual(that :Any) :Boolean = that.getClass == getClass
	}


	//todo: review all the GroundXxx for consistent use of () vs Any - preferably public () and private Any -
	// and make a clear distinction between implementation and interface types
	/** Type alias for a parameterless [[net.noresttherein.oldsql.sql.DML DML]] (that is, with `Unit`
	  * as its parameter type).
	  */
	type GroundDML[+Res] = DML[Unit, Res]

	object GroundDML {
		/** Implementation mix-in for parameterless [[net.noresttherein.oldsql.sql.DML DML]]. */
		trait Impl[+Res] extends DML[Any, Res] {
			override def compose[X](f :X => Any) :this.type = this
			override def bind(args :Any) :this.type = this
		}
	}


	/** A no-op `DML` implementation translating to no database operations. */
	case object Empty extends DML[Any, Int] {
		override def compose[X](f :X => Any) :this.type = this
		override def bind(args :Any) :this.type = this

		override def chant(implicit dialect :SQLDialect) :Incantation[Any, Int] = Incantation.NoOp

		protected override def initHashCode :Int = 0
		protected override def initToString :String = "DML()"
		override def toString = "DML()"
	}



	/** Mix-in trait for [[net.noresttherein.oldsql.sql.DML DML]] implementations which are adapters to
	  * other `DML` instances, rather than being independent statements of their own.
	  */
	trait DMLAdapter[+S] {
		/** The adapted [[net.noresttherein.oldsql.sql.DML DML]] instance. */
		def dml :S
	}


	/** A `DML` adapter for a different parameter(s) type(s). This is the interface trait which can be used
	  * in pattern matching by client code; concrete extending classes typically also mix in
	  * [[net.noresttherein.oldsql.sql.DML.ComposedDML.Impl ComposedDML.Impl]] which is the template specialized
	  * with a [[net.noresttherein.oldsql.sql.DML DML]] subtype.
	  * This instance will generate the same SQL/DML as the adapted `DML`, only changing the parameter type
	  * of the created [[net.noresttherein.oldsql.sql.Incantation Incantation]].
	  * @tparam Args the type of this instance's parameter(s), with multiple values typically combined
	  *              into a [[net.noresttherein.oldsql.collection.Chain Chain]].
	  * @tparam Org  the type of the parameter(s) of the adapted `DML`.
	  * @tparam Res  the type returned by both this and original `DML` instances.
	  * @see [[net.noresttherein.oldsql.sql.DMLStatement.ComposedStatement]]
	  */
	trait ComposedDML[-Args, Org, +Res] extends DML[Args, Res] with DMLAdapter[DML[Org, Res]] {
		/** The function mapping all arguments of this instance before passing them to the adapted `dml`. */
		def argmap :Args => Org

		override def chant(implicit dialect :SQLDialect) :Incantation[Args, Res] = dml.chant.compose(argmap)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposedDML[_, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :ComposedDML[_, _, _] if (other canEqual this) && this.canEqual(other) =>
				dml == other.dml && argmap == other.argmap
			case _ => false
		}
		protected override def initHashCode :Int = dml.hashCode * 31 + argmap.hashCode
		protected override def initToString :String = "(" + argmap + ")(" + dml + ")"
	}


	object ComposedDML {
		/** Matches [[net.noresttherein.oldsql.sql.DML.ComposedDML ComposedDML]] instances, returning the adapted
		  * dml and the adapting function.
		  */
		def unapply[X, Y](dml :DML[X, Y]) :Option[(DML[A, Y], X => A) forSome { type A }] = dml match {
			case composed :ComposedDML[X, a, Y] => Some((composed.dml, composed.argmap))
			case _ => None
		}

		/** Implementation trait of [[net.noresttherein.oldsql.sql.DML.ComposedDML ComposedDML]]`[Args, Org, Res]`
		  * which defines [[net.noresttherein.oldsql.sql.DML.ComposedDML.Impl.compose compose]]
		  * and [[net.noresttherein.oldsql.sql.DML.ComposedDML.Impl.bind bind]] by delegating to the adapted
		  * `S[Org, Res]` in order to return an instance of properly specialized type `S` (both statically
		  * and dynamically). This trait's methods (just as those of `ComposedDML`) are designed to override any default
		  * methods provided by `S` and for this reason both should be mixed-in to `ComposedDML` subtype `S` last.
		  * @tparam Args the type of this instance's parameter(s), with multiple values typically combined
		  *              into a [[net.noresttherein.oldsql.collection.Chain Chain]].
		  * @tparam Org  the type of the parameter(s) of the adapted `DML`.
		  * @tparam Res  the type returned by both this and original `DML` instances.
		  * @tparam S    the type constructor of a specialized `DML` subtype extended by `dml` and returned
		  *              by all factory methods of this type.
		  */
		private[sql] trait Impl[-Args, Org, +Res, +S[-X, +Y] <: DMLAPI[X, Y, S]] extends DMLAPI[Args, Res, S] {
			def dml :S[Org, Res]
			def argmap :Args => Org
			override def compose[X](f :X => Args) :S[X, Res] = dml.compose(f andThen argmap)
			override def bind(args :Args) :S[Unit, Res] = dml.bind(argmap(args))
		}

		/** Abstract base class for concrete subclasses of [[net.noresttherein.oldsql.sql.DML.ComposedDML ComposedDML]]
		  * which declares its properties as constructor parameters for convenience.
		  */
		private[sql] class Base[-Args, Org, +Res, +S[-X, +Y] <: DMLAPI[X, Y, S]]
		                       (override val dml :S[Org, Res], override val argmap :Args => Org)
			{ this :Impl[Args, Org, Res, S] => }
	}


	/** An association of a parameterized [[net.noresttherein.oldsql.sql.DML DML]]`[Args, Res]]` with a single value
	  * of its parameter(s). It is a `DML[Any, Res]` for flexibility of adapting it to other types than
	  * most common `DML[(), Res]`, but will ignore any arguments passed to it and execute the included DML
	  * for the given parameters instead. This is the public interface that can be used in pattern matching
	  * by the application; extending classes typically also mix-in
	  * [[net.noresttherein.oldsql.sql.DML.BoundDML.Impl BoundDML.Impl]].
	  *
	  * This instance translates to the same SQL/DML as the adapted instance, only providing the parameters used
	  * in its application.
	  * @tparam Args the parameter types of the bound `DML`.
	  * @tparam Res  the shared result type.
	  * @see [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement]]
	  */
	trait BoundDML[Args, +Res] extends DML[Any, Res] with DMLAdapter[DML[Args, Res]] {
		/** The argument for the adapted `dml`. */
		def args :Args

		override def chant(implicit dialect :SQLDialect) :Incantation[Any, Res] =
			dml.chant.bind(args).compose { _ => () } //the composition will be ignored

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[BoundDML[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :BoundDML[_, _] if (other canEqual this) && canEqual(other) =>
				dml == other.dml && args == other.args
			case _ => false
		}
		protected override def initHashCode :Int = dml.hashCode * 31 + args.hashCode
		protected override def initToString :String = "Bound(" + dml + ")(" + args + ")"
	}

	object BoundDML {
		/** Matches any [[net.noresttherein.oldsql.sql.DML.BoundDML BoundDML]] instance returning
		  * the adapted `DML` and its associated argument.
		  */
		def unapply[X, Y](dml :DML[X, Y]) :Option[(DML[A, Y], A) forSome { type A }] = dml match {
			case bound :BoundDML[a, Y] => Some((bound.dml, bound.args))
			case _ => None
		}

		/** Implementation of [[net.noresttherein.oldsql.sql.DML.BoundDML BoundDML]]`[Args, Res]`
		  * which implements [[net.noresttherein.oldsql.sql.DML.BoundDML.Impl.compose compose]]
		  * and [[net.noresttherein.oldsql.sql.DML.BoundDML.Impl.bind bind]] by returning itself.
		  * This trait's (just as those of `BoundDML`) are designed to override any default
		  * methods provided by `S` and for this reason both should be mixed-in to `BoundDML` subtype `S` last.
		  * @tparam Args the type of parameters accepted by the adapted statement.
		  * @tparam Res  the return type of both this and the adapted `DML`.
		  * @tparam S    a type constructor of `DML` subclasses of this API; it is the type returned by all the methods
		  *              and also expected self-type of implementors of this trait.
		  */
		private[sql] trait Impl[Args, +Res, S[-X, +Y] <: DMLAPI[X, Y, S]] extends DMLAPI[Any, Res, S] {
			this :S[Any, Res] =>

			def dml :S[Args, Res]
			def args :Args
			override def compose[X](f :X => Any) :S[Any, Res] = this
			override def bind(args :Any) :S[Any, Res] = this
		}

		/** Abstract base class for concrete subclasses of [[net.noresttherein.oldsql.sql.DML.BoundDML BoundDML]]
		  * which declares its properties as constructor parameters for convenience.
		  */
		private[sql] class Base[Args, +Res, S[-X, +Y] <: DMLAPI[X, Y, S]]
		                       (override val dml :S[Args, Res], override val args :Args)
			{ this :Impl[Args, Res, S] with S[Any, Res] => }
	}


	/** An adaptation of [[net.noresttherein.oldsql.sql.DML DML]] to a collection of arguments.
	  * Creates an [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[Iterable[Args], Seq[Res]]` in place of
	  * `Incantation[Args, Res].` This trait may translate both to a single database request for every element
	  * of the input collection, or one large batch, depending on the adapted `dml`. This is undefined at this stage,
	  * but decided in an object-oriented fashion when this instance is translated into an `Incantation`, a process
	  * in which the `Incantation` for the underlying `dml` is [[net.noresttherein.oldsql.sql.Incantation.batch batched]].
	  * @tparam Args the type of the arguments passed for a single execution of the underlying `dml` and the element
	  *              type of the input collection accepted as the argument by this instance.
	  * @tparam Res the return type of a single execution of the underlying `dml` and the element type of the
	  *             sequence returned by this instance, collecting the results of its individual executions.
	  */
	trait RepeatedDML[-Args, +Res] extends DML[Seq[Args], Seq[Res]] with DMLAdapter[DML[Args, Res]] {
		override def chant(implicit dialect :SQLDialect) :Incantation[Seq[Args], Seq[Res]] =
			dml.chant.batch

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[RepeatedDML[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :RepeatedDML[_, _] if (other canEqual this) && canEqual(other) => dml == other.dml
			case _ => false
		}
		protected override def initHashCode :Int = dml.hashCode
		protected override def initToString :String = "(" + dml + ")*"
	}

	object RepeatedDML {
		def apply[X, Y](dml :DML[X, Y]) :DML[Seq[X], Seq[Y]] = new Base(dml) with RepeatedDML[X, Y]

		/** Matches any [[net.noresttherein.oldsql.sql.DML.RepeatedDML RepeatedDML]],
		  * extracting the batched `DML` instance.
		  */
		def unapply[X, Y](dml :DML[X, Y]) :Option[DML[_, _]] = dml match {
			case batch :RepeatedDML[_, _] => Some(batch.dml)
			case _ => None
		}

		/** Abstract base class for concrete subclasses of [[net.noresttherein.oldsql.sql.DML.RepeatedDML RepeatedDML]]
		  * which declares its properties as constructor parameters for convenience.
		  */
		private[sql] abstract class Base[-Args, +Res, S[-X, +Y] <: DML[X, Y] with DMLAPI[X, Y, S]]
		                                (override val dml :S[Args, Res])
			{ this :RepeatedDML[Args, Res] => }
	}


	/** An additional marker trait for DML which execute a single statement multiple times as a JDBC batch,
	  * requiring only a single database request under most circumstances.
	  */
	trait BatchDML[-Args, +Res] extends RepeatedDML[Args, Res] with DMLAdapter[DMLStatement[Args, Res]]
}






/** A representation of a statement modifying a table or tables such as ''insert'', ''update'' and ''delete''
  * as an abstract syntax tree. It is created as a part of Scala DSL and serves as a basis for creation of
  * an executable [[net.noresttherein.oldsql.sql.Incantation Incantation]]. This is done by
  * [[net.noresttherein.oldsql.sql.DMLStatement.chant chant]] method using
  * an [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]] proper for the chosen DBMS, typically with the help
  * of the corresponding [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] in order to create
  * an intermediate [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] object. For this purpose,
  * each concrete subclass must implement [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling defaultSpelling]]
  * which should return its representation in standard SQL.
  *
  * It is the class of `DML` which translates into a single [[java.sql.PreparedStatement PreparedStatement]],
  * allowing batching of parameter sets. This, in turn, means that instances returned by
  * [[net.noresttherein.oldsql.sql.DMLStatement.batch batch]] should, in principle, result in a single database request,
  * unless the parameter set is large, in which case the driver may divide it into multiple round trips.
  * The parameter types `Args` of this instance need not correspond directly to the JDBC parameter types passed to
  * the `PreparedStatement`; instead, the latter can be any SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
  * based on a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] exposing `Args` as one or more
  * [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter(s).  A lack of parameters on this class doesn't mean
  * that the resulting `PreparedStatement` will be parameterless;
  * if the SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] used contain
  * [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter bound]] parameters, they will be translated to JDBC
  * parameters which will always be assigned the embedded values.
  *
  * A single invocation of this instance is expected to result in a single database round-trip, unless the returned
  * data set is large and the driver decides to split the request. Implementations can be divided
  * into two broad categories:
  *   1. actual, concrete DML statements, most notably [[net.noresttherein.oldsql.sql.Insert Insert]],
  *     [[net.noresttherein.oldsql.sql.Update Update]], [[net.noresttherein.oldsql.sql.Merge Merge]],
  *     [[net.noresttherein.oldsql.sql.Delete Delete]] and [[net.noresttherein.oldsql.sql.Call Call]];
  *   1. Adapter and composition classes, which the applications typically do not interact with directly, but through
  *      their factory methods in `DMLStatement`:
  *      1. parameterless [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement BoundStatement]] returned by
  *         [[net.noresttherein.oldsql.sql.DMLStatement.bind bind]], which associates a parameterized statement with
  *         a concrete value of its parameter(s);
  *      1. [[net.noresttherein.oldsql.sql.DMLStatement.ComposedStatement ComposedStatement]] adapting another
  *         statement to a different argument type through [[net.noresttherein.oldsql.sql.DMLStatement.compose compose]]
  *         method;
  *      1. [[net.noresttherein.oldsql.sql.DMLStatement.AlteredResultStatement AlteredResultStatement]] created by
  *         [[net.noresttherein.oldsql.sql.DMLStatement.result result]] method, which changes the return type
  *         of a statement by substituting its
  *         [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult StatementResult]] strategy.
  *         All standard implementations of this class are listed
  *         by [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] implementing the ''visitor''
  *         pattern for this class hierarchy.
  *
  * @tparam Args the parameter(s) of this statement, represented as [[net.noresttherein.oldsql.sql.UnboundParam unbound]]
  *              parameters of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] within the statement.
  *              Translates to the parameters of created `Incantation`.
  * @tparam Res  the type of the returned value. It can represent a single value, some collection type, or simply
  *              an `Int`/`Long` when returning only the number of modified rows.
  * @see [[net.noresttherein.oldsql.sql.Query]]
  * @see [[net.noresttherein.oldsql.sql.SQLExpression]]
  * @author Marcin Mościcki
  */
trait DMLStatement[-Args, +Res] extends DML[Args, Res] with DMLStatementAPI[Args, Res, DMLStatement] {
	protected override def returns[Y](result :StatementResult[Nothing, Y]) :DMLStatement[Args, Y] =
		new AlteredResultStatement.Base[Args, Y, DMLStatement](this, result)
			with AlteredResultStatement[Args, Y] with AlteredResultStatement.Impl[Args, Y, DMLStatement]

	override def compose[X](f :X => Args) :DMLStatement[X, Res] = //new ComposedImpl(this, f)
		new ComposedDML.Base[X, Args, Res, DMLStatement](this, f)
			with ComposedStatement[X, Args, Res] with ComposedStatement.Impl[X, Args, Res, DMLStatement]

	override def bind(args :Args) :DMLStatement[Unit, Res] =
		new BoundDML.Base[Args, Res, DMLStatement](this, args)
			with BoundStatement[Args, Res] with BoundStatement.Impl[Args, Res, DMLStatement]

	override def batch :DML[Seq[Args], Seq[Res]] = RepeatedDML(this)

	/** The double-dispatch intermediate method of the visitor pattern for the `DMLStatement` class hierarchy.
	  * Implementations should call the method corresponding to the most specific standard `DMLStatement` type they
	  * extend.
	  */
	protected def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res]

	private[sql] final def applyToForwarder[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
		applyTo(visitor)


	/** 'Spell out' this DML into a textual form understandable by the targeted DBMS, converting it into
	  * an executable object. This is a forwarder to the appropriate
	  * [[net.noresttherein.oldsql.sql.SQLDialect.apply[X,Y](statement:DMLStatement[X,Y])* apply]] method
	  * of `SQLDialect`. Standard SQL dialect, in turn, calls by default this instance's
	  * [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling defaultSpelling]] method,
	  * in which every concrete subclass must implement the default SQL formatting.
	  * The result of the first call is cached and returned whenever a future call specifies the same dialect.
	  * There should rarely be need to override this method instead of `defaultSpelling` or providing a specialized
	  * `SQLDialect`. If a subclass chooses to do so, it should honour the SQL spelling returned by the dialect.
	  * @param dialect A ''strategy'' translating abstract syntax trees (or even higher-level objects) into
	  *                SQL/DML, hiding any DBMS-specific features.
	  */
	override def chant(implicit dialect :SQLDialect) :Incantation[Args, Res] = {
		@inline def shazam = if (sqlDialect == dialect) incantation else doChant
		if (incantation == null) synchronized {
			if (incantation == null) {
				sqlDialect = dialect
				incantation = doChant
				incantation
			} else shazam
		} else {
			if (sqlDialect == null) synchronized { shazam }
			else shazam
		}
	}

	/** Creates an [[net.noresttherein.oldsql.sql.Incantation Incantation]] based on this object using the given
	  * dialect. This is the implementation method called by public
	  * [[net.noresttherein.oldsql.sql.DMLStatement.chant chant]] and forwards directly to the dialect
	  * passing this instance as the argument.
	  */
	protected override def doChant(implicit dialect :SQLDialect) :Incantation[Args, Res] = dialect(this)

	private[this] var incantation :Incantation[Args, Res] = _
	private[this] var sqlDialect :SQLDialect = _
}




/** A home of some default implementations of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] and base
  * classes for other implementations, as well as auxiliary classes, most notably
  * [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult StatementResult]] (and its implementations) responsible
  * for handling the result of execution of a statement.
  */
object DMLStatement {

	/** Wildcard type alias matching all [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] instances. */
	type * = DMLStatement[Nothing, Any]

	/** Public API declaring methods available on [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]]
	  * and its specialization `S`. This trait is variant in all its type parameter, requiring the same variance
	  * from the type constructor `S` of the described `DML` subtype to share this variance.
	  * @tparam Args the type of parameters accepted by this statement(s), translating to the arguments of
	  *              [[net.noresttherein.oldsql.sql.Incantation incantations]] created from this instance.
	  * @tparam Res  the return type of the executable `Incantation` created from this instance.
	  * @tparam Stmt a type constructor of `DMLStatement` subclasses of this API; it is the type returned by all
	  *              the methods and also expected self-type of implementors of this trait (although it is not enforced).
	  */
	trait DMLStatementAPI[-Args, +Res, +Stmt[-X, +Y] <: DMLStatementAPI[X, Y, Stmt]] extends DMLAPI[Args, Res, Stmt] {
		/** A strategy handling the reading of the values returned by the database from the execution of the resulting
		  * `PreparedStatement`.
		  */
		def result :StatementResult[Nothing, Res]

		/** A DML statement equivalent in effect to this one, except returning only the number of affected rows
		  * instead of this statement's return type.
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UpdateCount]]
		  */
		def updateCount :Stmt[Args, Int] = returns(UpdateCount)

		/** A DML statement equivalent in effect to this one, except returning only the number of affected rows
		  * instead of this statement's return type. The number is read using
		  * [[java.sql.PreparedStatement.getLargeUpdateCount getLargeUpdateCount]] method of
		  * [[java.sql.PreparedStatement PreparedStatement]].
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.LargeUpdateCount]]
		  */
		def largeUpdateCount :Stmt[Args, Long] = returns(LargeUpdateCount)

		/** A DML statement equivalent in effect to this one, except not returning anything.
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.NoResult]]
		  */
		def noResult :Stmt[Args, Unit] = returns(StatementResult.NoResult)

		/** A DML statement returning the same result as this one, but as a singleton sequence.
		  * Useful when uninfying the types of statements returning individual objects and collections of objects.
		  */
		def inSeq :Stmt[Args, Seq[Res]] = returns(result.map(_::Nil))

		/** Changes the return type of ths statement. [[net.noresttherein.oldsql.sql.Incantation Incantation]] created
		  * by such objects will ignore this instance's [[net.noresttherein.oldsql.sql.DMLStatement.result result]]
		  * and use instead the strategy provided here to read the returned value(s). This swap will work
		  * only in specific situations, hence the method is protected. It is the delegation target of public methods
		  * [[net.noresttherein.oldsql.sql.DMLStatement.DMLStatementAPI.updateCount updateCount]],
		  * [[net.noresttherein.oldsql.sql.DMLStatement.DMLStatementAPI.largeUpdateCount largeUpdateCount]] and
		  * [[net.noresttherein.oldsql.sql.DMLStatement.DMLStatementAPI.noResult noResult]].
		  */
		protected def returns[Y](result :StatementResult[Nothing, Y]) :Stmt[Args, Y]

		private[sql] final def withResult[Y](result :StatementResult[Nothing, Y]) :Stmt[Args, Y] = returns(result)

		/** Create DML which will execute this instance multiple times, for every element of its argument collection,
		  * and collect the results in a single sequence. This should translate
		  * to a [[java.sql.PreparedStatement PreparedStatement]] batch and a single request.
		  */
		def batch :DML[Seq[Args], Seq[Res]]


		/** Callback method invoked by `spelling` when [[net.noresttherein.oldsql.sql.DMLStatement.chant chanting]]
		  * if no specific formatting is needed for this statement type. It should format this statement
		  * in standard SQL.
		  */
		protected def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args, RowProduct]

		private[sql] final def defaultSpellingForwarder(spelling :SQLSpelling) :SpelledSQL[Args, RowProduct] =
			defaultSpelling(spelling)

		protected def doChant(implicit dialect :SQLDialect) :Incantation[Args, Res]
	}


	/** Type alias for parameterless [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]]. */
	type GroundDMLStatement[+Res] = DMLStatement[Unit, Res]


	/** An adapter of another DML statement substituting its return type for one handled by the replacement
	  * [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult StatementResult]]. This is useful particularly
	  * when switching from [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UpdateCount UpdateCount]]
	  * to [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.LargeUpdateCount LargeUpdateCount]] or
	  * when the result should be ignored.
	  * This instance will generate the exact same DML as the one with the unused `StatementResult`.
	  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.NoResult]]
	  */
	trait AlteredResultStatement[-Args, +Res] extends DMLStatement[Args, Res] with DMLAdapter[DMLStatement[Args, Any]] {

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
			visitor.result(this)

		protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args, RowProduct] =
			spelling.spell(dml)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[AlteredResultStatement[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :AlteredResultStatement[_, _] if (other canEqual this) && canEqual(other) =>
				dml == other.dml && result == other.result
			case _ => false
		}
		protected override def initHashCode :Int = dml.hashCode * 31 + result.hashCode
		protected override def initToString :String = dml.toString + " as " + result
	}

	object AlteredResultStatement {
		/** Matches [[net.noresttherein.oldsql.sql.DMLStatement.AlteredResultStatement AlteredResultStatement]] instances,
		  * extracting the adapted statement and the substitute result.
		  */
		def unapply[X, Y](dml :DML[X, Y]) :Option[(DMLStatement[X, Any], StatementResult[Nothing, Any])] =
			dml match {
				case returning :AlteredResultStatement[X, Y] => Some((returning.dml, returning.result))
				case _ => None
			}

		/** Implementation of [[net.noresttherein.oldsql.sql.DMLStatement.AlteredResultStatement AlteredResultStatement]]`[Args, Res]`
		  * which implements [[net.noresttherein.oldsql.sql.DMLStatement.AlteredResultStatement.Impl.result result]]
		  * by delegating to the adapted `dml`.
		  * This trait's methods (just as those of `AlteredResultStatement`) are designed to override any default
		  * methods provided by `Stmt` and for this reason both should be mixed-in to `AlteredResultStatement`
		  * subtype `Stmt` last.
		  * @tparam Args the type of parameters accepted by the adapted statement.
		  * @tparam Res  the new result type.
		  * @tparam Stmt a type constructor of `DML` subclasses of this API; it is the type returned by all the methods
		  *              and also expected self-type of implementors of this trait.
		  */
		trait Impl[-Args, +Res, +Stmt[-X, +Y] <: DMLStatementAPI[X, Y, Stmt]] extends DMLStatementAPI[Args, Res, Stmt] {
			def dml  :Stmt[Args, Any]

			protected override def returns[Y](result :StatementResult[Nothing, Y]) :Stmt[Args, Y] =
				dml.withResult(result)
		}

		/** Abstract base class for concrete subclasses of
		  * [[net.noresttherein.oldsql.sql.DMLStatement.AlteredResultStatement AlteredResultStatement]]
		  * which declares its properties as constructor parameters for convenience.
		  */
		private[sql] class Base[Args, Res, Stmt[-X, +Y] <: DMLStatementAPI[X, Y, Stmt]]
		                       (override val dml :Stmt[Args, Any], override val result :StatementResult[Nothing, Res])
			{ this :Impl[Args, Res, Stmt] => }
	}


	/** Adapts a DML statement to a new argument type by mapping all passed arguments with the given function.
	  * This is the public interface; concrete classes typically also mix-in implementation trait
	  * [[net.noresttherein.oldsql.sql.DMLStatement.ComposedStatement.Impl ComposedStatement.Impl]] specialized
	  * for a particular `DMLStatement` subtype `Stmt`.
	  *
	  * The DML generated does not change from the one of the adapted instance.
	  * @tparam Args the type of this instance's parameter(s), with multiple values typically combined
	  *              into a [[net.noresttherein.oldsql.collection.Chain Chain]].
	  * @tparam Org  the type of the parameter(s) of the adapted `DMLStatement`.
	  * @tparam Res  the type returned by both this and original `DMLStatement` instances.
	  */
	trait ComposedStatement[-Args, Org, +Res]
		extends DMLStatement[Args, Res] with ComposedDML[Args, Org, Res] with DMLAdapter[DMLStatement[Org, Res]]
	{
		override def result :StatementResult[Nothing, Res] = dml.result

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
			visitor.composed(this)

		protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args, RowProduct] =
			spelling.spell(dml).compose(argmap)

		protected override def doChant(implicit dialect :SQLDialect) :Incantation[Args, Res] =
			dml.chant.compose(argmap)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :ComposedStatement[_, _, _] if (other canEqual this) && canEqual(other) =>
				argmap == other.argmap && dml == other.dml && result == other.result
			case _ => false
		}
		protected override def initHashCode :Int = (dml.hashCode * 31 + argmap.hashCode) * 31 + result.hashCode
		protected override def initToString :String = "(" + argmap + ")(" + dml + "):" + result
	}


	object ComposedStatement {
		/** Matches [[net.noresttherein.oldsql.sql.DMLStatement.ComposedStatement ComposedStatement]] instances,
		  * returning the adapted dml and the adapting function.
		  */
		def unapply[X, Y](dml :DML[X, Y]) :Option[(DMLStatement[A, Y], X => A) forSome { type A }] =
			dml match {
				case composed :ComposedStatement[X, a, Y] => Some((composed.dml, composed.argmap))
				case _ => None
			}

		/** Implementation trait of
		  * [[net.noresttherein.oldsql.sql.DMLStatement.ComposedStatement ComposedStatement]]`[Args, Org, Res]`
		  * various adapting methods by delegating to the adapted statement `Stmt[Org, Res]` in order to return
		  * an instance of properly specialized type `Stmt` (both statically and dynamically). This trait's methods
		  * (just as those of `ComposedStatement`) are designed to override any default methods provided by `Stmt` and,
		  * for this reason, both should be mixed-in to `ComposedStatement` subtype `Stmt` last.
		  * @tparam Args the type of this instance's parameter(s), with multiple values typically combined
		  *              into a [[net.noresttherein.oldsql.collection.Chain Chain]].
		  * @tparam Org  the type of the parameter(s) of the adapted `DMLStatement`.
		  * @tparam Res  the type returned by both this and original `DMLStatement` instances.
		  * @tparam Stmt the type constructor of a specialized `DMLStatement` subtype extended by `dml` and returned
		  *              by all factory methods of this type.
		  */
		trait Impl[-Args, Org, +Res, Stmt[-X, +Y] <: DMLStatementAPI[X, Y, Stmt]]
			extends ComposedDML.Impl[Args, Org, Res, Stmt] with DMLStatementAPI[Args, Res, Stmt]
		{
			protected override def returns[Y](result :StatementResult[Nothing, Y]) :Stmt[Args, Y] =
				dml.withResult(result).compose(argmap)
		}
	}


	/** Combines a parameterized DML statement with a single value of its parameter(s), turning it into
	  * a parameterless statement which will always ignore its arguments and use the preset value instead.
	  * This allows late binding of previously created, reusable statements to obtain effectively the same result
	  * as if the statement was defined using SQL [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter bound]]
	  * parameters, reducing the overhead. This is the public interface that can be used in pattern matching
	  * by the application; extending classes typically also mix-in
	  * [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement.Impl BoundStatement.Impl]].
	  */
	trait BoundStatement[Args, +Res]
		extends DMLStatement[Any, Res] with BoundDML[Args, Res] with DMLAdapter[DMLStatement[Args, Res]]
	{
		override def result :StatementResult[Nothing, Res] = dml.result

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Any, Res] =
			visitor.bound(this)

		protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Any, RowProduct] =
			spelling.spell(dml).compose { _ => args }

		protected override def doChant(implicit dialect :SQLDialect) :Incantation[Any, Res] =
			dml.chant.bind(args).compose { _ => () }

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :BoundStatement[_, _] if canEqual(other) && (other canEqual this) =>
				args == other.args && dml == other.dml && result == other.result
			case _ => false
		}
		protected override def initHashCode :Int = (dml.hashCode * 31 + args.hashCode) * 31 + result.hashCode
		protected override def initToString :String = "Bound(" + dml + ")(" + args + "):" + result
	}


	object BoundStatement {
		/** Matches any [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement BoundStatement]] instance returning
		  * the adapted `DML` and its associated argument.
		  */
		def unapply[X, Y](dml :DML[X, Y]) :Option[(DMLStatement[A, Y], A) forSome { type A }] = dml match {
			case bound :BoundStatement[a, Y] => Some((bound.dml, bound.args))
			case _ => None
		}

		/** Implementation of [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement BoundStatement]]`[Args, Res]`
		  * which implements adapting methods by returning itself.
		  * This trait's (just as those of `BoundStatement`) are designed to override any default
		  * methods provided by `Stmt` and for this reason both should be mixed-in to `BoundStatement`
		  * subtype `Stmt` last.
		  * @tparam Args the type of parameters accepted by the adapted statement.
		  * @tparam Res  the return type of both this and the adapted `DMLStatement`.
		  * @tparam Stmt a type constructor of `DMLStatement` subclasses of this adapted by this class; it is the type
		  *              returned by all the methods and also expected self-type of implementors of this trait.
		  */
		trait Impl[Args, +Res, Stmt[-X, +Y] <: DMLStatementAPI[X, Y, Stmt]]
			extends DMLStatementAPI[Args, Res, Stmt] with BoundDML.Impl[Args, Res, Stmt]
		{ this :Stmt[Any, Res] =>
			protected override def returns[Y](result :StatementResult[Nothing, Y]) :Stmt[Any, Y] =
				dml.withResult(result).bind(args).compose(_ => ())
		}
	}



	/** Base trait for implementations of a ''visitor'' pattern implementation matching
	  * standard [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] class hierarchy.
	  * No default implementations are given for any methods, but extending classes can group various tree branches
	  * to handle them as their common base type by mixing-in suitable `CaseXxx` or `MatchXxx` traits provided
	  * in the singleton object grouping the branch. For example, extending
	  * {{{
	  *     StatementVisitor[R] with CaseInsert[R] with CaseUpdate[R] with CaseMerge[R]
	  *                         with CaseDelete[R] with CaseCall[R]
	  * }}}
	  * will leave 9 unimplemented methods:
	  *   - one for each of the listed main statement types;
	  *   - three for existing high-level adapters of other statements;
	  *   - a fallback for custom implementations extending none of the above.
	  *
	  * See [[net.noresttherein.oldsql.sql.DMLStatement.CaseStatement CaseStatement]] for a base type which
	  * instead implements all methods by delegating them to the method for their direct supertype in the hierarchy
	  * (as listed by this visitor). Its supertypes include all mix-in visitor traits for all statement types
	  * and is thus a good starting point for browsing available dispatching implementations for composing
	  * a tailored `StatementVisitor`.
	  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
	  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
	  *           and its return type (the `Res` argument of the visited statement).
	  * @see [[net.noresttherein.oldsql.sql.DMLStatement.MatchStatement]]
	  * @see [[net.noresttherein.oldsql.sql.DMLStatement.CaseStatement]]
	  *///todo: rename to visitor; Merge
	trait StatementVisitor[R[-X, +Y]]
		extends InsertVisitor[R] with UpdateVisitor[R]  with DeleteVisitor[R]
		   with CallVisitor[R] with ReturningVisitor[R]
	{
		def apply[X, Y](stmt :DMLStatement[X, Y]) :R[X, Y] = stmt.applyToForwarder(this)

		def bound[X, Y](stmt :BoundStatement[X, Y])             :R[Any, Y]
		def composed[X, A, Y](stmt :ComposedStatement[X, A, Y]) :R[X, Y]
		def result[X, Y](stmt :AlteredResultStatement[X, Y])    :R[X, Y]

		def statement[X, Y](stmt :DMLStatement[X, Y])           :R[X, Y]
	}

	/** A visitor base trait leaving unimplemented the methods for all main supported statement types,
	  * grouping various implementations under their main type ([[net.noresttherein.oldsql.sql.Insert Insert]],
	  * [[net.noresttherein.oldsql.sql.Update Update]], etc.).
	  */
	trait MatchStatement[R[-X, +Y]]
		extends StatementVisitor[R] with CaseInsert[R] with CaseUpdate[R] with CaseDelete[R]
		   with CaseCall[R] with CaseReturning[R]

	/** A base trait for ''visitor'' pattern implementations over the
	  * [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy which implements all
	  * methods by forwarding them to the methods for the direct supertypes of their statement cases.
	  * All cases are thus eventually delegated to the root
	  * [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor.statement statement]] method, allowing
	  * overriding of cherry-picked cases only.
	  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
	  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
	  *           and its return type (the `Res` argument of the visited statement).
	  */
	trait CaseStatement[R[-X, +Y]] extends MatchStatement[R] {
		override def bound[X, Y](stmt :BoundStatement[X, Y])                         :R[Any, Y] = statement(stmt)
		override def composed[X, A, Y](stmt :ComposedStatement[X, A, Y])             :R[X, Y]   = statement(stmt)
		override def result[X, Y](stmt :AlteredResultStatement[X, Y])                :R[X, Y]   = statement(stmt)

		override def returning[X, M[O] <: MappingAt[O], Y](stmt :Returning[X, M, Y]) :R[X, Y]   = statement(stmt)

		override def insert[X, M[O] <: MappingAt[O], Y](stmt :Insert[X, M, Y])       :R[X, Y]   = statement(stmt)
		override def update[X, M[O] <: MappingAt[O], Y](stmt :Update[X, M, Y])       :R[X, Y]   = statement(stmt)
		override def delete[X, M[O] <: MappingAt[O], Y](stmt :Delete[X, M, Y])       :R[X, Y]   = statement(stmt)
		override def call[X, Y](stmt :Call[X, Y])                                    :R[X, Y]   = statement(stmt)
	}




	/** A facade representing the domain of available identifiers to SQL
	  * [[net.noresttherein.oldsql.sql.SQLExpression expressions]] used
	  * in a [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]]. It is similar to and used in the same manner
	  * as [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]: as an argument type of functions
	  * creating SQL expressions for use in ''where'' and ''set'' clauses of parameterized SQL ''updates''
	  * (and in similar contexts for other DML statements). It exposes the mappings for the updated table
	  * and the statement parameter(s) with proper `Origin` types to allow their implicit conversions to
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]] expressions for the whole table.
	  */
	class DMLScope[X, M[O] <: MappingAt[O]](val domain :From[M] WithParam X) {
		/** The mapping of the modified table, for use on the left side
		  * of [[net.noresttherein.oldsql.sql.ComponentSetter assignments]]. Any of tis components are implicitly
		  * convertible to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] for their columns:
		  * {{{
		  * { scope :DMLScope => scope.table := scope.current + scope.param }
		  * }}}
		  */
		val table = domain.left.table.row[M[From[M]]]

		/** The mapping of the modified table, representing its current value for use on the right side
		  * of [[net.noresttherein.oldsql.sql.ComponentSetter assignments]]. Any of its components are implicitly
		  * convertible to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] for their columns.
		  */
		val current :M[RowProduct AndFrom M WithParam X] = domain.prev

		/** A [[net.noresttherein.oldsql.sql.UnboundParam.FromParam mapping]] for the statement parameter,
		  * implicitly convertible to a placeholder SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
		  * representing the parameter. It can be also used to create pseudo 'components' representing parameter values
		  * derivable from `X`.
		  */
		val param :FromParam[X, RowProduct AndFrom ParamRelation[X]#Param] = domain.last.mapping

		/** Initiates the creation of a dependent ''select'' having access to both the modified table and the statement
		  * parameter.
		  * @return a [[net.noresttherein.oldsql.sql.FromClause ''from'']] clause for the given table which can
		  *         be joined with further tables or used to create an SQL ''select'' using one of its
		  *         `select` extension methods.
		  */
		def from[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		        (table :Table[R])(implicit cast :InferSubject[From[M] WithParam X, Subselect, R, T, S])
				:From[M] WithParam X Subselect R =
			domain subselect table
	}




	/** A strategy of reading the values returned by an [[net.noresttherein.oldsql.sql.Incantation Incantation]]
	  * from a [[java.sql.PreparedStatement PreparedStatement]]. It may be responsible for reading a single result -
	  * either an update count or values returned in a [[java.sql.ResultSet ResultSet]] - or multiple consecutive
	  * results. Default implementations, including implicit values, for most common use cases are provided
	  * in the [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult$ companion]] object to this class.
	  * For the most common cases, a 'result' is a single `ResultSet`/update count and
	  * [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.batch batching]] will apply the same result strategy
	  * repeatedly, expecting multiple results/result sets, but this is dependent on the implementation, as JDBC
	  * handles batching of other statements, for example with auto generated keys, differently.
	  *
	  * As JDBC API is essentially dynamically typed, in an attempt to add some type safety, this trait
	  * defines an 'entity' type parameter `E` in addition to the type of the read values. In the most common case
	  * of reading rows of a table (or joined tables with a single 'master' table), this is given as the application
	  * type to which each read row is mapped, but more complex cases such as implementations handling multiple results
	  * or [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.ProcedureResult OUT]] parameters
	  * of a procedure call will use other types to signal their purpose. For example,
	  *   1. a `StatementResult[Squirrel, Squirrel]` declares that it expects the handled result to be a `ResultSet`
	  *      with a single row mapping to a `Squirrel` entity;
	  *   1. a `StatementResult[Squirrel, Seq[Squirrel]]` declares that it expects the rows of the `ResultSet`
	  *      to have a schema mapping to a `Squirrel`, but is ''also'' shared by an incompatible implementation
	  *      handling batching of a query returning a single squirrel;
	  *   1. a batch statement of the above would conform to `StatementResult[Squirrel, Seq[Seq[Squirrel]]`;
	  *   1. an implementation reading the ''OUT'' parameters of a [[java.sql.CallableStatement CallableStatement]]
	  *      distinguishes itself by extending
	  *      `StatementResult[`[[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]]`[@~ ~ Int ~ Int]]`.
	  *
	  * As there is obvious ambiguity in this scheme, and as it assumes a unique
	  * [[net.noresttherein.oldsql.schema.Mapping Mapping]]
	  * (or [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]) exists for `Squirrel` type which is the one
	  * used internally by that `StatementResult` implementation, the typing happens on a 'best effort' basis only
	  * and is optional.
	  *
	  * Implementations are expected to close all consumed resources (such as `ResultSet`s), but not the
	  * executed statement itself, whose lifecycle is handled
	  * by [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]].
	  */ //todo: we need an argument parameter for InsertReturning
	trait StatementResult[-E, +Res] extends Serializable {
		/** Hot spot method invoked after the statement handled by this instance was created, but before its execution.
		  * This is where various properties of the returned `ResultSet` can be set and where ''OUT'' parameters
		  * are registered.
		  */
		def prepare(statement :PreparedStatement) :Unit = ()

		/** Reads the result from the given `PreparedStatement`. This can consume zero or more actual 'results'
		  * (update counts or result sets) from the statement.
		  */
		@throws[IllegalResultArityException]("if the size of a result set does not match the expected number of rows.")
		@throws[MissingStatementResultBug]("if there are not enough results remaining in the statement for this instance to consume.")
		def apply(statement :PreparedStatement) :Res

		/** Create a `StatementResult` for a batch of the statement handled by this instance.
		  * It will repeatedly apply this instance to the `PreparedStatement`, until no additional results
		  * (either update counts or `ResultSet`s) exist. Note that when called for an already batched instance,
		  * the result will always return a singleton collection containing the results collected by this statement
		  * (empty or not). This is unlikely to be what you want.
		  */
		def batch :StatementResult[E, Seq[Res]] = new BatchResult(this)

		/** Create a `StatementResult` for a batch of a predetermined size of the statement handled by this instance.
		  * This instance is invoked exactly `size` times and the values are collected using the builder provided
		  * by the implicit [[scala.collection.Factory Factory]]`[Res, C]`. Any remaining results are ignored,
		  * which allows nested collections.
		  */
		def batch[C](size :Int)(implicit factory :Factory[Res, C]) :StatementResult[E, C] =
			new RepeatedResult(this, size, factory)

		/** Create a `StatementResult` for a batch of a predetermined size of the statement handled by this instance.
		  * This instance is invoked exactly `size` times and the values are collected using the builder provided
		  * by the given collection factory. Any remaining results are ignored, which allows nested collections.
		  */
		def batch[U >: Res, C[_]](factory :IterableFactory[C], size :Int) :StatementResult[E, C[U]] =
			batch(size)(ComparableFactory(factory))

		/** Create a `StatementResult` for a batch of a predetermined size of the statement handled by this instance.
		  * This instance is invoked exactly `size` times and the values are collected using the builder provided
		  * by the given collection factory. Any remaining results are ignored, which allows nested collections.
		  */
		def batch[U >: Res, C[_], Ev[_]](factory :EvidenceIterableFactory[C, Ev], size :Int)(implicit ev :Ev[U])
				:StatementResult[E, C[U]] =
			batch(size)(ComparableFactory(factory))

		/** Map the result read by this instance with the given function, adapting it to a new result type. */
		def map[Y](result :Res => Y) :StatementResult[E, Y] = new MappedResult(this, result)
	}




	sealed abstract class FactoryResultImplicit {
		/** Consumes a single [[java.sql.ResultSet ResultSet]] of the executed
		  * [[java.sql.PreparedStatement PreparedStatement]], reading every row with the implicitly given
		  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
		  * using the builder provided by the implicitly given factory.
		  */
		implicit def FactoryResult[E, C](implicit form :SQLReadForm[E], factory :Factory[E, C]) :StatementResult[E, C] =
			apply(factory)

		def apply[E :SQLReadForm, C](factory :Factory[E, C]) :StatementResult[E, C]
	}


	/** Default implementations of [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult StatementResult]]
	  * strategy for reading the result(s) of an executed [[java.sql.PreparedStatement PreparedStatement]].
	  */
	object StatementResult extends FactoryResultImplicit {
		/** Returns the next [[java.sql.PreparedStatement.getUpdateCount update count]] of the executed statement. */
		case object UpdateCount extends StatementResult[Any, Int] {
			override def apply(statement :PreparedStatement) :Int = {
				val rows = statement.getUpdateCount
				if (rows < 0)
					throw new MissingStatementResultBug(
						"No (additional) result for " + statement + " or the result is not an update count."
					)
				rows
			}
		}

		/** Returns the next [[java.sql.PreparedStatement.getLargeUpdateCount large update count]]
		  * of the executed statement. */
		case object LargeUpdateCount extends StatementResult[Any, Long] {
			override def apply(statement :PreparedStatement) :Long = try {
				val rows = statement.getLargeUpdateCount
				if (rows < 0)
					throw new MissingStatementResultBug(
						"No (additional) result for " + statement + " or the result is not an update count."
					)
				rows
			} catch {
				case e :SQLException => try {
					statement.getUpdateCount match {
						case -1 => throw new MissingStatementResultBug(
							"No (additional) result for " + statement + " or the result is not an update count.", e
						)
						case Int.MaxValue => throw new TooManyResultsException(
							"largeUpdateCount unavailable and updateCount exceeds Integer limit.", e
						)
						case rows => rows
					}
				} catch {
					case ex :Exception => e.addSuppressed(ex); throw e
				}
			}
		}

		/** Sums the update counts of all results available of the executed statement.
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UpdateCount]] */
		case object TotalUpdateCount extends StatementResult[Any, Int] {
			override def apply(statement :PreparedStatement) :Int = {
				var res = 0
				while(statement.getMoreResults || { val x = statement.getUpdateCount; res += x; x >= 0 })
					{}
				res
			}
		}

		/** Sums the large update counts of all results available of the executed statement.
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.LargeUpdateCount]] */
		case object LargeTotalUpdateCount extends StatementResult[Any, Long] {
			override def apply(statement :PreparedStatement) :Long = {
				var res = 0L
				while(statement.getMoreResults || { val x = statement.getLargeUpdateCount; res += x; x >= 0L })
					{}
				res
			}
		}

		/** Returns `true` if the update count was not zero. */
		case object Modified extends StatementResult[Any, Boolean] {
			override def apply(statement :PreparedStatement) :Boolean = {
				val rows = statement.getUpdateCount
				if (rows < 0)
					throw new MissingStatementResultBug(
						"No (additional) result for " + statement + " or the result is not an update count."
					)
				rows > 0
			}
		}

		/** Returns the predefined value without doing anything. */
		case class NoResult[Res](result :Res) extends StatementResult[Any, Res] {
			override def apply(statement :PreparedStatement) :Res = result
			private[this] lazy val string = result.toString
			override def toString :String = string
		}

		/** Returns `Unit` without doing anything. This is different from
		  * [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.UnitResult UnitResult]] in that the latter
		  * ''consumes'' the result, advancing to the next one (and fails with an exception if there is no result),
		  * while this is a true no-op. This makes it useful for situations where no results are expected
		  * (for example, a procedure call), while the latter can serve as a placeholder or replacement
		  * for other results, which prevents misalignment of results when batching or chaining multiple instances
		  * for consecutive execution.
		  */
		object NoResult extends NoResult[Unit](()) {
			/** Returns [[net.noresttherein.oldsql.collection.Chain.@~ @~]] without doing anything. */
			val chain :StatementResult[Any, @~] = NoResult(@~)

			override def toString = "NoResult"
		}

		/** Always returns `0`, but ''consumes'' the next result from the `PreparedStatement`.
		  * It is an error if no results remain.
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.LargeZeroResult]]
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.NoResult]] */
		val ZeroResult :StatementResult[Any, Int] = ConstResult(0)

		/** Always returns `0`, but ''consumes'' the next result from the `PreparedStatement`.
		  * It is an error if no results remain.
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.ZeroResult]]
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.NoResult]] */
		val LargeZeroResult :StatementResult[Any, Long] = ConstResult(0L)

		/** Always returns the `()` singleton, but ''consumes'' the next result from the `PreparedStatement`.
		  * It is an error if no results remain; use
		  * [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.NoResult NoResult]] if no results are expected
		  * (or should be ignored). Advancing to the next result makes this instance useful as a replacement for
		  * other implementations when used in a batch or during consecutive reading of several results to
		  * prevent misalignment. In other cases `NoResult` will be preferable.
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.NoResult]] */
		val UnitResult :StatementResult[Any, Unit] = ConstResult(())

		/** Always returns the empty [[net.noresttherein.oldsql.collection.Chain chain]] `@~`, but ''consumes''
		  * the next result from the `PreparedStatement`. It is an error if no results remain.
		  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.NoResult]] */
		val EmptyResult :StatementResult[Any, @~] = ConstResult(@~)

		private case class ConstResult[T](result :T) extends StatementResult[Any, T] {
			override def apply(statement :PreparedStatement) :T = {
				if (statement.getUpdateCount < 0) {//advance to the next result in a multi-result statement
					val rs = statement.getResultSet
						if (rs == null)
							throw new MissingStatementResultBug(
								toString + " cannot consume a result from statement '" + statement +
									"' as none are available."
							)
					rs.close()
				}
				result
			}
			override lazy val toString :String = "{" + result + "}"
		}

		private case class MappedResult[-E, Y, +Res](result :StatementResult[E, Y], f :Y => Res)
			extends StatementResult[E, Res]
		{
			override def prepare(statement :PreparedStatement) :Unit = result.prepare(statement)
			override def apply(statement :PreparedStatement) :Res = f(result(statement))
			override lazy val toString :String = f.toString + "(" + result + ")"
		}

		private class BatchResult[-E, +Res](result :StatementResult[E, Res]) extends StatementResult[E, Seq[Res]] {
			override def prepare(statement :PreparedStatement) :Unit = result.prepare(statement)

			override def apply(statement :PreparedStatement) :Seq[Res] = {
				//Connection.getUpdateCount docs specify that it should be called only once per result.
				val proxy = new CachedUpdateCountStatement(statement)
				val builder = ArraySeq.newBuilder[Res](ClassTag[Res](classOf[Any]))
				while ({ builder += result(proxy); statement.getMoreResults || proxy.getUpdateCount >= 0 })
					{}
				builder.result()
			}

			private def elementType = result

			override def equals(that :Any) :Boolean = that match {
				case batch :BatchResult[_, _] => (batch eq this) || batch.elementType == elementType
				case _ => false
			}
			override def hashCode = result.hashCode

			override lazy val toString :String = "(" + result + ")*"
		}

		private class RepeatedResult[-E, Res, +C]
		                            (result :StatementResult[E, Res], arity :Int, factory :Factory[Res, C])
			extends StatementResult[E, C]
		{
			override def prepare(statement :PreparedStatement) :Unit = result.prepare(statement)

			override def apply(statement :PreparedStatement) :C = {
				//Connection.getUpdateCount docs specify that it should be called only once per result.
				val proxy = new CachedUpdateCountStatement(statement)
				var remaining = arity
				val builder = factory.newBuilder
				builder sizeHint remaining
				while (
					remaining > 0 && { builder += result(proxy); statement.getMoreResults || proxy.getUpdateCount >= 0 }
				) {
					remaining -= 1
				}
				if (remaining > 0)
					throw new MissingStatementResultBug(
						"Expected " + arity + " results for " + result + " but got only " + (arity-remaining) +
							" in '" + statement + "'."
					)
				builder.result()
			}

			private def resultType = factory
			private def elementType = result
			private def max = arity

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :RepeatedResult[_, _, _] if other.getClass == getClass =>
					other.max == max && other.resultType == resultType && other.elementType == elementType
				case _ => false
			}
			override def hashCode :Int = (result.hashCode * 31 + factory.hashCode) * 31 + arity

			override lazy val toString :String = factory.toString + "[" + result + "]{" + arity + "}"
		}



		private trait ResultSetResult[E, +Res] extends StatementResult[E, Res] {
			override def apply(statement :PreparedStatement) :Res = {
				val rs = statement.getResultSet
				try {
					read(rs)
				} catch {
					case e :Exception =>
						try { rs.close() }
						catch { case e2 :Exception => e.addSuppressed(e2) }
						throw e
				}
			}

			def read(rs :ResultSet) :Res

			def rowType :SQLReadForm[E]

			override def equals(that :Any) :Boolean = that match {
				case other :ResultSetResult[_, _] =>
					(other eq this) || other.getClass == getClass && other.rowType == rowType
				case _ => false
			}
			override def hashCode :Int = rowType.hashCode
		}

		private class SingleResult[E](implicit form :SQLReadForm[E]) extends ResultSetResult[E, E] {
			final override def read(rs :ResultSet) :E = {
				if (!rs.next())
					throw new IllegalResultArityException(s"Excepted one row in '$rs', got zero.")
				val res = form(rs, 1)
				if (rs.next())
					throw new IllegalResultArityException(s"Expected one row in '$rs', got $res, ${form(rs, 1)}, ...")
				rs.close()
				res
			}

			override def rowType = form

			override lazy val toString :String = "One[" + form + "]"
		}

		private class OptionResult[E](implicit form :SQLReadForm[E]) extends ResultSetResult[E, Option[E]] {
			final override def read(rs :ResultSet) = {
				val res = if (rs.next()) Some(form(rs, 1)) else None
				if (rs.next())
					throw new IllegalResultArityException(
						s"Expected at most one row in '$rs', got: ${res.get}, ${form(rs, 1)}, ..."
					)
				rs.close()
				res
			}

			override def rowType = form

			override lazy val toString :String = "Option[" + form + "]"
		}

		private class CollectionResult[E, C](factory :Factory[E, C])(implicit form :SQLReadForm[E])
			extends ResultSetResult[E, C]
		{
			override def read(rs :ResultSet) = {
				val builder = factory.newBuilder
				while (rs.next()) {
					builder += form(rs, 1)
				}
				rs.close()
				builder.result()
			}

			override def rowType = form
			private def resultType = factory

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :CollectionResult[_, _] =>
					getClass == other.getClass && rowType == other.rowType && resultType == other.resultType
				case _ => false
			}
			override def hashCode :Int = rowType.hashCode * 31 + resultType.hashCode

			override lazy val toString :String = factory.toString + "[" + form + "]"
		}

		private class LazyResult[E](implicit form :SQLReadForm[E]) extends ResultSetResult[E, LazyList[E]] {
			final override def read(rs :ResultSet) :LazyList[E] =
				if (rs.next()) form(rs, 1) #:: read(rs)
				else { rs.close(); LazyList.empty[E] }

			override def rowType = form

			override lazy val toString :String = "Lazy[" + form + "]"
		}

		private class IteratorResult[E](implicit form :SQLReadForm[E]) extends ResultSetResult[E, Iterator[E]] {
			override def read(rs :ResultSet) :Iterator[E] = new Iterator[E] {
				private[this] var hasMore = rs.next()
				override def hasNext = hasMore

				override def next() :E = {
					val res = form(rs, 1)
					hasMore = rs.next()
					if (!hasMore)
						rs.close()
					res
				}
			}

			override def rowType = form

			override lazy val toString :String = "Iterator[" + form + "]"
		}



		private trait GeneratedKeysResult[E, +Res] extends ResultSetResult[E, Res] {
			override def apply(statement :PreparedStatement) :Res = {
				val rs = statement.getGeneratedKeys
				try {
					read(rs)
				} catch {
					case e :Exception =>
						try { rs.close() }
						catch { case e2 :Exception => e.addSuppressed(e2) }
						throw e
				}
			}

			override def batch :StatementResult[E, Seq[Res]] =
				new GeneratedKeysBatchResult[E, Res, Seq](this, ComparableFactory(Seq))

			override def batch[C](size :Int)(implicit factory :Factory[Res, C]) :StatementResult[E, C] =
				new GeneratedKeysRepeatedResult[E, Res, C](this, size, factory)
		}

		private class GeneratedKeyResult[E :SQLReadForm] extends SingleResult[E] with GeneratedKeysResult[E, E] {
			override lazy val toString = "Key[" + rowType + "]"
		}

		private class GeneratedKeyOptionResult[E :SQLReadForm]
			extends OptionResult[E] with GeneratedKeysResult[E, Option[E]]
		{
			override lazy val toString = "KeyOption[" + rowType + "]"
		}

		private class GeneratedKeyCollectionResult[E :SQLReadForm, C](factory :Factory[E, C])
			extends CollectionResult[E, C](factory) with GeneratedKeysResult[E, C]
		{
			override lazy val toString = "Keys[" + factory + "[" + rowType + "]]"
		}

		private class LazyGeneratedKeysResult[E :SQLReadForm]
			extends LazyResult[E] with GeneratedKeysResult[E, LazyList[E]]
		{
			override lazy val toString = "LazyKeys[" + rowType + "]"
		}

		private class GeneratedKeysIteratorResult[E :SQLReadForm]
			extends IteratorResult[E] with GeneratedKeysResult[E, Iterator[E]]
		{
			override lazy val toString = "KeysIterator[" + rowType + "]"
		}

		private class GeneratedKeysBatchResult[E, Res, C[_]]
		                                      (result :ResultSetResult[E, Res], factory :Factory[Res, C[Res]])
			extends GeneratedKeysResult[E, C[Res]]
		{
			override def read(rs :ResultSet) = {
				val res = factory.newBuilder
				while (rs.next())
					result.read(rs)
				res.result()
			}

			override def rowType = result.rowType
			def elementType :ResultSetResult[E, Res] = result
			def resultType :Factory[Res, C[Res]] = factory

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GeneratedKeysBatchResult[_, _, _] if other.getClass == getClass =>
					elementType == other.elementType && resultType == other.resultType
				case _ => false
			}
			override def hashCode :Int = result.hashCode * 31 + factory.hashCode

			override lazy val toString :String = factory.toString + "(" + result + ")*"
		}

		private class GeneratedKeysRepeatedResult[E, Res, C](result :ResultSetResult[E, Res], arity :Int,
		                                                     factory :Factory[Res, C])
			extends GeneratedKeysResult[E, C]
		{
			override def read(rs :ResultSet) = {
				val res = factory.newBuilder
				var remaining = arity
				res sizeHint remaining
				while (rs.next()) {
					remaining -= 1
					result.read(rs)
				}
				if (remaining > 0)
					throw new MissingStatementResultBug(
						"Expected " + arity + " results for " + result + " but got only " + (arity-remaining) +
							" in '" + rs + "'."
					)
				res.result()
			}

			override def rowType = result.rowType
			private def elementType = result
			private def count = arity
			private def resultType = factory

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :GeneratedKeysRepeatedResult[_, _, _] =>
					count == other.count && elementType == other.elementType && resultType == other.resultType
				case _ => false
			}
			override def hashCode :Int = (result.hashCode * 31 + arity.hashCode) * 31 + factory.hashCode
			override lazy val toString :String = factory.toString + "[" + result + "]{" + arity + "}"
		}



		private val emptyResultSet :ResultSet = new CachedRowSetImpl

		private abstract class OutParamsResult[Xs, +Res](private val outParams :Seq[Int], position :Int = 1)
		                                                (implicit form :SQLReadForm[Xs])
			extends StatementResult[Any, Res]
		{
			private[this] val indices = Array.ofDim[Int](outParams.length + 1)
			outParams.copyToArray(indices, 1)

			if (indices.length != form.readColumns + 1)
				throw new IllegalArgumentException(
					"Cannot create StatementResult " + this + " because out parameter form uses " +
						form.readColumns + " columns."
				)

			override def prepare(statement :PreparedStatement) :Unit = statement match {
				case call :CallableStatement => form.register(call, position)
				case _ if form.readColumns == 0 =>
				case _ => throw new IllegalArgumentException(
					"Cannot register out params for " + this + " - not a CallableStatement: '" +
						statement + "':" + statement.getClass.getName + "."
				)

			}

			protected def readParams(statement :PreparedStatement) :Xs = statement match {
				case call :CallableStatement => form(CallableStatementOutParams(call, indices), position)
				case _ if form.readColumns == 0 => form(emptyResultSet, position)
				case _ => throw new IllegalArgumentException(
					"Cannot read out params for " + this + " - not a CallableStatement: '" +
						statement + "' :" + statement.getClass.getName + "."
				)
			}

			private def offset = position

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :OutParamsResult[_, _] if other.getClass == getClass =>
					outParams == other.outParams && offset == other.offset
				case _ => false
			}
			override def hashCode :Int = outParams.hashCode * 31 + offset.hashCode

			protected def toStringImpl = outParams.mkString("Out(", ",", ")[" + form + "]")
			override lazy val toString :String = toStringImpl
		}

		private class ProcedureResult[Out](outParams :Seq[Int])(implicit form :SQLReadForm[Out])
			extends OutParamsResult[Out, Out](outParams)
		{
			override def apply(statement :PreparedStatement) = readParams(statement)
			//fixme: we need custom batch implementations, but how should it work?
		}


		/** Consumes a single [[java.sql.ResultSet ResultSet]] of the executed
		  * [[java.sql.PreparedStatement PreparedStatement]], reading every row with the implicitly given
		  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
		  * using the builder provided by the given factory.
		  * @see [[net.noresttherein.oldsql.morsels.ComparableFactory]]
		  */
		override def apply[E :SQLReadForm, C](factory :Factory[E, C]) :StatementResult[E, C] =
			new CollectionResult(factory)

		/** Consumes a single [[java.sql.ResultSet ResultSet]] of the executed
		  * [[java.sql.PreparedStatement PreparedStatement]], reading every row with the implicitly given
		  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
		  * using the builder provided by the given factory.
		  */
		def apply[E :SQLReadForm, C[_]](factory :IterableFactory[C]) :StatementResult[E, C[E]] =
			new CollectionResult[E, C[E]](ComparableFactory(factory))

		/** Consumes a single [[java.sql.ResultSet ResultSet]] of the executed
		  * [[java.sql.PreparedStatement PreparedStatement]], reading every row with the implicitly given
		  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
		  * using the builder provided by the given factory.
		  */
		def apply[E, C[_], Ev[_]](factory :EvidenceIterableFactory[C, Ev])(implicit ev :Ev[E], form :SQLReadForm[E])
				:StatementResult[E, C[E]] =
			new CollectionResult[E, C[E]](ComparableFactory(factory))

		/** Consumes a single [[java.sql.ResultSet ResultSet]] of the executed
		  * [[java.sql.PreparedStatement PreparedStatement]], expecting a single row of the implicitly given form.
		  */
		implicit def SingleResult[E :SQLReadForm] :StatementResult[E, E] = new SingleResult[E]

		/** Consumes a single [[java.sql.ResultSet ResultSet]] of the executed
		  * [[java.sql.PreparedStatement PreparedStatement]], expecting at most one row of the implicitly given form.
		  */
		implicit def OptionResult[E :SQLReadForm] :StatementResult[E, Option[E]] = new OptionResult[E]

		/** Consumes a single [[java.sql.ResultSet ResultSet]] of the executed
		  * [[java.sql.PreparedStatement PreparedStatement]], reading every row with the implicitly given
		  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in an `Iterable`.
		  */
		implicit def IterableResult[E :SQLReadForm] :StatementResult[E, Iterable[E]] = apply(Iterable)

		/** Lazily reads the contents of the next [[java.sql.ResultSet ResultSet]] with rows of the implicitly given form.
		  * Note that it does not close the result set and the [[java.sql.PreparedStatement PreparedStatement]]
		  * must be likewise open when the list is traversed, which almost certainly will require
		  * specific handling by a dedicated [[net.noresttherein.oldsql.sql.Incantation Incantation]].
		  * This result will automatically close the `ResultSet` when all elements of the list are initialized
		  * (all rows are read).
		  */
		implicit def LazyResult[E :SQLReadForm] :StatementResult[E, LazyList[E]] = new LazyResult[E]

		/** Exposes the contents of the next [[java.sql.ResultSet ResultSet]] with rows of the implicitly given form `E`
		  * as an [[scala.collection.Iterator Iterator]].
		  * Note that it does not close the result set and the [[java.sql.PreparedStatement PreparedStatement]]
		  * must be likewise open when the list is traversed, which almost certainly will require
		  * specific handling by a dedicated [[net.noresttherein.oldsql.sql.Incantation Incantation]].
		  * This result will automatically close the `ResultSet` when the iterator reaches the last row.
		  */
		implicit def IteratorResult[E :SQLReadForm] :StatementResult[E, Iterator[E]] = new IteratorResult[E]


		/** Reads the values of the ''OUT'' parameters of an executed stored procedure.
		  * This implementation requires that the statement used is an instance
		  * of [[java.sql.CallableStatement CallableStatement]]!
		  * @param indices indices of all ''OUT'' parameters read by the implicitly given form of the chain `Xs`
		  *                containing them all. They are one-based and must correspond with
		  *                [[java.sql.PreparedStatement PreparedStatement]] parameter placeholders '?' in the SQL.
		  */
		def ProcedureResult[Xs :SQLReadForm](indices :Seq[Int]) :StatementResult[Call[Xs, Any], Xs] =
			new ProcedureResult[Xs](indices)

		/** Reads the values of the ''OUT'' parameters of an executed stored procedure as well as its return value.
		  * It is essentially the same as
		  * [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult.ProcedureResult ProcedureResult]],
		  * but with the narrowed down 'domain' type parameter.
		  * This implementation requires that the statement used is an instance
		  * of [[java.sql.CallableStatement CallableStatement]]!
		  * @param indices indices of all ''OUT'' parameters read by the implicitly given form of the chain `Xs`
		  *                containing them all. They are one-based and must correspond with
		  *                [[java.sql.PreparedStatement PreparedStatement]] parameter placeholders '?' in the SQL.
		  */
		def FunctionResult[Xs <: Chain :SQLReadForm, Y :SQLReadForm](indices :Seq[Int])
				:StatementResult[Call[Xs, Y], Xs ~ Y] =
			new ProcedureResult[Xs ~ Y](1 +: indices)(SQLReadForm[Xs] ~ SQLReadForm[Y])

		implicit def FunctionResult[Y :ColumnReadForm] :StatementResult[Call[@~, Y], Y] =
			FunctionResult[@~, Y](Nil).map(_.last)

		/** [[net.noresttherein.oldsql.sql.DMLStatement.StatementResult StatementResult]] implementations reading
		  * the values of generated keys returned by the database after ''insert'' statements (or similar).
		  * As the generated keys must be registered when the [[java.sql.PreparedStatement PreparedStatement]]
		  * is created, these results will require a special [[net.noresttherein.oldsql.sql.]]
		  */ //consider: adding a column names seq parameter for validation and error debugging
		object GeneratedKeys {
			/** Reads all rows of the [[java.sql.ResultSet java.sql.ResultSet]] with
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
			  * using the builder provided by the given factory.
			  * @see [[net.noresttherein.oldsql.morsels.ComparableFactory]]
			  */
			def apply[E :SQLReadForm, C](factory :Factory[E, C]) :StatementResult[E, C] =
				new GeneratedKeyCollectionResult(factory)

			/** Reads all rows of the [[java.sql.ResultSet java.sql.ResultSet]] with
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
			  * using the builder provided by the given factory.
			  */
			def apply[E :SQLReadForm, C[_]](factory :IterableFactory[C]) :StatementResult[E, C[E]] =
				new GeneratedKeyCollectionResult[E, C[E]](ComparableFactory(factory))

			/** Reads all rows of the [[java.sql.ResultSet java.sql.ResultSet]] with
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
			  * using the builder provided by the given factory.
			  */
			def apply[E :SQLReadForm, C[_], Ev[_]](factory :EvidenceIterableFactory[C, Ev])(implicit ev :Ev[E])
					:StatementResult[E, C[E]] =
				new GeneratedKeyCollectionResult[E, C[E]](ComparableFactory(factory))

			/** Reads the [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]`, expecting a single row.
			  */
			def Single[E :SQLReadForm] :StatementResult[E, E] = new GeneratedKeyResult[E]

			/** Reads the [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]`, expecting at most a single row.
			  */
			def Option[E :SQLReadForm] :StatementResult[E, Option[E]] = new GeneratedKeyOptionResult[E]

			/** Reads all rows of the [[java.sql.ResultSet java.sql.ResultSet]] with
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
			  * using the builder provided by the implicit factory.
			  * @see [[net.noresttherein.oldsql.morsels.ComparableFactory]]
			  */
			def Factory[E, C](implicit form :SQLReadForm[E], factory :Factory[E, C]) :StatementResult[E, C] =
				apply(factory)

			/** Reads all rows of the [[java.sql.ResultSet java.sql.ResultSet]] with
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a
			  */
			def Iterable[E :SQLReadForm] :StatementResult[E, Iterable[E]] = apply(scala.Iterable)

			/** Lazily reads the rows of the [[java.sql.ResultSet ResultSet]] with the
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given form.
			  * Note that it does not close the result set when the result is returned and
			  * the [[java.sql.PreparedStatement PreparedStatement]] must be likewise open when the list is traversed,
			  * which almost certainly will require specific handling
			  * by a dedicated [[net.noresttherein.oldsql.sql.Incantation Incantation]].
			  * This result closes the result set when the list becomes fully initialized.
			  */
			def Lazy[E :SQLReadForm] :StatementResult[E, LazyList[E]] = new LazyGeneratedKeysResult[E]

			/** Iterates over the rows of the [[java.sql.ResultSet ResultSet]] with the
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] of the implicitly given form.
			  * Note that it does not close the result set when the iterator is returned and
			  * the [[java.sql.PreparedStatement PreparedStatement]] must be likewise open when the list is traversed,
			  * which almost certainly will require specific handling
			  * by a dedicated [[net.noresttherein.oldsql.sql.Incantation Incantation]].
			  * This result closes the result set when the list becomes fully initialized.
			  */
			def Iterator[E :SQLReadForm] :StatementResult[E, Iterator[E]] = new GeneratedKeysIteratorResult[E]
		}

		object UpdatedEntities {
			def Single[E, M[X] <: BaseMapping[E, X], O](mapping :RefinedMapping[E, O], columns :Seq[String])
					:StatementResult[E, E] = ??? //fixme

			def Single[E, M[X] <: BaseMapping[E, X], O](arg :E, mapping :RefinedMapping[E, O], columns :Seq[String])
					:StatementResult[E, E] = ??? //fixme

			def Option[E, M[X] <: BaseMapping[E, X], O](mapping :RefinedMapping[E, O], columns :Seq[String])
					:StatementResult[E, Option[E]] = ??? //fixme

			def Option[E, M[X] <: BaseMapping[E, X], O](arg :E, mapping :RefinedMapping[E, O], columns :Seq[String])
					:StatementResult[E, Option[E]] = ??? //fixme

			/** Reads all rows of the [[java.sql.ResultSet java.sql.ResultSet]] with
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
			  * using the builder provided by the given factory.
			  * @see [[net.noresttherein.oldsql.morsels.ComparableFactory]]
			  */
			def apply[E, C, M[X] <: BaseMapping[E, X], O]
			         (factory :Factory[E, C])(mapping :RefinedMapping[E, O], columns :Seq[String]) :StatementResult[E, C] =
				??? //fixme:

			def apply[E, C, M[X] <: BaseMapping[E, X], O]
			         (factory :Factory[E, C])(args :Seq[E], mapping :RefinedMapping[E, O], columns :Seq[String])
					:StatementResult[E, C] =
				??? //fixme:

			/** Reads all rows of the [[java.sql.ResultSet java.sql.ResultSet]] with
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
			  * using the builder provided by the given factory.
			  */
			def apply[E, C[_], M[X] <: BaseMapping[E, X], O]
			         (mapping :RefinedMapping[E, O], columns :Seq[String], factory :IterableFactory[C])
					:StatementResult[E, C[E]] =
				??? //fixme

			def apply[E, C[_], M[X] <: BaseMapping[E, X], O]
			         (args :Seq[E], mapping :RefinedMapping[E, O], columns :Seq[String], factory :IterableFactory[C])
					:StatementResult[E, C[E]] =
				??? //fixme

			/** Reads all rows of the [[java.sql.ResultSet java.sql.ResultSet]] with
			  * [[java.sql.PreparedStatement.getGeneratedKeys generated keys]] using the implicitly given
			  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[E]` and collecting them in a single result
			  * using the builder provided by the given factory.
			  */
			def apply[E, C[_], M[X] <: BaseMapping[E, X], O, Ev[_]]
			         (mapping :RefinedMapping[E, O], columns :Seq[String], factory :EvidenceIterableFactory[C, Ev])
			         (implicit ev :Ev[E])
					:StatementResult[E, C[E]] =
				??? //fixme

			def apply[E, C[_], M[X] <: BaseMapping[E, X], O, Ev[_]]
			         (args :Seq[E], mapping :RefinedMapping[E, O],
			          columns :Seq[String], factory :EvidenceIterableFactory[C, Ev])(implicit ev :Ev[E])
					:StatementResult[E, C[E]] =
				??? //fixme
		}
	}

}







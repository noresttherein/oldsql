package net.noresttherein.oldsql.sql

import scala.reflect.ClassTag

import net.noresttherein.oldsql.schema.{ColumnForm, Mapping, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, OuterFrom}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.{CaseComposite, CompositeMatcher}
import net.noresttherein.oldsql.sql.ConversionSQL.{CaseConversion, ConversionMatcher, MappedSQL, PromotionConversion}
import net.noresttherein.oldsql.sql.ConditionSQL.{ComparisonSQL, EqualitySQL, InequalitySQL, IsNULL}
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, SQLTypePromotion}
import net.noresttherein.oldsql.sql.SQLExpression.SQLTypePromotion.Lift
import net.noresttherein.oldsql.sql.SQLTerm.{CaseTerm, ColumnLiteral, ColumnTerm, CompositeNULL, False, NULL, SQLLiteral, SQLParameter, SQLParameterColumn, TermMatcher, True}
import net.noresttherein.oldsql.sql.TupleSQL.{CaseTuple, TupleMatcher}
import net.noresttherein.oldsql.sql.MappingSQL.{CaseMapping, MappingMatcher}
import net.noresttherein.oldsql.sql.ColumnSQL.{AliasedColumn, ColumnMatcher}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.{CompositeColumnMatcher, MatchCompositeColumn}
import net.noresttherein.oldsql.sql.SelectSQL.{CaseSelect, FreeSelectSQL, SelectMatcher, SubselectSQL}



//here be implicits
import slang._





/** A representation of an SQL expression as an AST.
  * @tparam F row source - list of tables which provide columns used in this expression
  * @tparam V result type of the expression; may not necessarily be an SQL type, but a result type of some mapping.
  */
trait SQLExpression[-F <: FromClause, V] { //todo: add a type parameter which is Bound || Unbound (flag if it contains any abstract/unbound parts)
	import SQLExpression.boundParameterSQL

	def readForm :SQLReadForm[V]

//	def =:[T <: RefinedMapping[O, E], C <: RefinedMapping[O, L], O, E, L, R >: V, X]
//	      (path :ComponentPath[T, C, O, E, L])(implicit lift :SQLTypePromotion[L, R, X]) :SetComponent[F, T, C, O, E, L, R, X] =
//		SetComponent(path, this :SQLExpression[F, R])

	def isNull :SQLBoolean[F] = IsNULL(this)

	def ==?[X, Y](value :X)(implicit lift :SQLTypePromotion[V, X, Y], form :SQLForm[X]) :SQLBoolean[F] =
		this === value.?

	def ===[S <: F, X, U](that :SQLExpression[S, X])(implicit lift :SQLTypePromotion[V, X, U]) :SQLBoolean[S] =
		EqualitySQL(lift.left(this), lift.right(that))

	def <>[S <: F, X, U](that :SQLExpression[S, X])(implicit lift :SQLTypePromotion[V, X, U]) :SQLBoolean[S] =
		InequalitySQL(lift.left(this), lift.right(that))

	def <=[S <: F, X, U](that :SQLExpression[S, X])
	                    (implicit lift :SQLTypePromotion[V, X, U], ordering :SQLOrdering[U]) :SQLBoolean[S] =
		ComparisonSQL(lift.left(this), ComparisonSQL.LTE, lift.right(that))

	def <[S <: F, X, U](that :SQLExpression[S, X])
	                   (implicit lift :SQLTypePromotion[V, X, U], ordering :SQLOrdering[U]) :SQLBoolean[S] =
		ComparisonSQL(lift.left(this), ComparisonSQL.LT, lift.right(that))

	def >=[S <: F, X, U](that :SQLExpression[S, X])
	                    (implicit lift :SQLTypePromotion[V, X, U], ordering :SQLOrdering[U]) :SQLBoolean[S] =
		ComparisonSQL(lift.left(this), ComparisonSQL.GTE, lift.right(that))

	def >[S <: F, X, U](that :SQLExpression[S, X])
	                   (implicit lift :SQLTypePromotion[V, X, U], ordering :SQLOrdering[U]) :SQLBoolean[S] =
		ComparisonSQL(lift.left(this), ComparisonSQL.GT, lift.right(that))


//todo: arithmetic


	/** Lifts this expression to one of type `X`, without any effect on the actual generated SQL. */
	def to[X](implicit lift :Lift[V, X]) :SQLExpression[F, X] = PromotionConversion(this, lift)

	/** Lift this expression to one typed `Option[V]`, without any effect on the actual generated SQL. */
	def opt :SQLExpression[F, Option[V]] = to[Option[V]]

	/** Maps the read (selected) scala value of this expression, without any effect on the actual generated SQL. */
	def map[X](f :V => X) :SQLExpression[F, X] = new MappedSQL[F, V, X](this)(f)



	/** Creates a `SelectSQL` with this expression as the ''select'' clause and the given `from` clause. */
	def selectFrom[S <: F with OuterFrom, O](from :S) :FreeSelectSQL[V, O] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.unqualifiedClassName} can't be used as a Select header."
		)

	/** Creates a subselect expression selecting this expression from the given `from` clause. */
	def subselectFrom[S <: F, O](from :S) :SubselectSQL[from.Implicit, V, O] = //subtype S needed in SQLRelation
		throw new UnsupportedOperationException(
			s"Expression $this :${this.unqualifiedClassName} can't be used as a Select header."
		)

	/** Upcasts this expression to the base ''from'' clause `E &lt;: F`, using only implicit evidence about the subtype
	  * relation rather than explicit lower type bound (which would be an identity cast in Scala).
	  */
	def basedOn[E <: FromClause](implicit subtype :E <:< F) :SQLExpression[E, V] = this.asInstanceOf[SQLExpression[E, V]]

	/** Treat this expression as an expression of a FROM clause extending (i.e. containing additional tables)
	  * the clause `F` this expression is based on. */
	def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :SQLExpression[S, V]



	def parameters :Seq[SQLParameter[_]] = collect { case x :SQLParameter[_] => x }



	def collect[X](fun :PartialFunction[SQLExpression.*, X]) :Seq[X] = reverseCollect(fun, Nil).reverse

	protected def reverseCollect[X](fun :PartialFunction[SQLExpression.*, X], acc :List[X]) :List[X] =
		fun.lift(this) ++: acc

	protected[this] def reverseCollect[X](e :SQLExpression.*)(fun :PartialFunction[SQLExpression.*, X], acc :List[X]) :List[X] =
		e.reverseCollect(fun, acc)


	def applyTo[Y[_]](matcher :ExpressionMatcher[F, Y]) :Y[V]



	/** If this expression is a free expression - not dependent on `F` or free parameters -
	  * which value can be determined at this point, return its value. */
	def freeValue :Option[V] = None


	/** Is this expression independent of any relations from the FROM clause? */
	def isFree :Boolean = freeValue.isDefined



	/** Tests if this expression is equal to the given one abstracting from possibly different sources.
	  * Basically, if both expressions would produce the same SQL they should be isomorphic.
	  */
	def isomorphic(expression :SQLExpression.*) :Boolean

	/** Tests if this expression would produce the same value as the given expression, abstracting from possibly
	  * different clauses. Similar to isomorphic, but generally disregards order of elements in composite expressions
	  * such as 'and', 'or', seq. Be warned that this method's primary use is for tests, and production code shouldn't
	  * depend on it.
	  */
	private[oldsql] def equivalent(expression :SQLExpression.*) :Boolean = isomorphic(expression)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLExpression.*]

}









sealed abstract class SQLMultiColumnTerms {

	implicit def implicitMultiNull[T :SQLForm](value :Null) :SQLTerm[T] = CompositeNULL[T]

	implicit def implicitLiteral[T :SQLForm](value :T) :SQLTerm[T] = SQLLiteral(value)

}



object SQLExpression extends SQLMultiColumnTerms {

	implicit def implicitNull[T :ColumnForm](value :Null) :ColumnTerm[T] = NULL[T]

	implicit def implicitBoolean(value :Boolean) :SQLBoolean[FromClause] = if (value) True else False

	implicit def implicitColumnLiteral[T :ColumnForm](value :T) :ColumnTerm[T] = ColumnLiteral(value)



	implicit class boundParameterSQL[T, P <: SQLParameter[T]]
	                                (value :T)(implicit factory :BoundParameterExpressions[T, P])
	{
		def ? :P = factory(value)
	}

//	implicit def boundParameterExpressions[T, P](term :T) :boundParameterExpressions[T] =
//		new boundParameterExpressions(term)

	sealed abstract class BoundParameterExpressions[T, E <: SQLParameter[T]] {
		implicit def apply(value :T) :E
	}

	implicit def BoundParameterExpressions[T :SQLForm] :BoundParameterExpressions[T, SQLParameter[T]] =
		new BoundParameterExpressions[T, SQLParameter[T]] {
			implicit override def apply(value :T) = SQLParameter(value)
		}

	implicit def BoundParameterColumnExpressions[T :ColumnForm] :BoundParameterExpressions[T, SQLParameterColumn[T]] =
		new BoundParameterExpressions[T, SQLParameterColumn[T]] {
			override implicit def apply(value :T) = SQLParameterColumn(value)
		}

/*
	implicit def typingCast[J[M[O] <: MappingAt[O]] <: _ Join M, R[O] <: MappingAt[O], T[O] <: BaseMapping[_, O], X]
	                       (e :SQLExpression[J[R], X])
	                       (implicit cast :JoinedRelationSubject[J, R, T, BaseMapping.AnyAt]) :SQLExpression[J[T], X] =
		cast(e)

	implicit def typingColumnCast[J[M[O] <: MappingAt[O]] <: _ Join M, R[O] <: MappingAt[O], T[O] <: BaseMapping[_, O], X]
	                             (e :ColumnSQL[J[R], X])
	                             (implicit cast :JoinedRelationSubject[J, R, T, BaseMapping.AnyAt])
			:ColumnSQL[J[T], X] =
		cast(e)
*/




	/** An upper type bound of all `SQLExpression[_, _]` instances. */
	type * = SQLExpression[_ <: FromClause, _]

	/** A type alias for SQL expressions independent of any relations in the FROM clause, that is applicable
	  * to any `FromClause`. */
	type FreeExpression[T] = SQLExpression[FromClause, T]







	trait CompositeSQL[-F <: FromClause, V] extends SQLExpression[F, V] {
		def inOrder :Seq[SQLExpression[F, _]] = parts

		protected def parts :Seq[SQLExpression[F, _]]


		protected override def reverseCollect[X](fun: PartialFunction[SQLExpression.*, X], acc: List[X]): List[X] =
			(super.reverseCollect(fun, acc) /: inOrder)((collected, member) => member.reverseCollect(fun, collected))



		def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :SQLExpression[S, V]


		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :SQLExpression[S, V] =
			rephrase(SQLScribe.stretcher(base))



		def sameAs(other :CompositeSQL[Nothing, _]) :Boolean = canEqual(other)

		private[oldsql] override def equivalent(expression: SQLExpression.*): Boolean = expression match {
			case e:CompositeSQL[_, _] =>
				(e eq this) || e.sameAs(this) && this.sameAs(e) && contentsEquivalent(e)
			case _ => false
		}

		override def isomorphic(expression :SQLExpression.*) :Boolean = expression match {
			case e:CompositeSQL[_, _] =>
				(this eq e) || sameAs(e) && e.sameAs(this) && contentsIsomorphic(e)
			case _ => false
		}


		private[oldsql] def contentsEquivalent(other :CompositeSQL[_ <: FromClause, _]) :Boolean =
			parts.size == other.parts.size &&
				parts.forall(e => other.parts.exists(_ equivalent e)) &&
				other.parts.forall(e => parts.exists(_ equivalent e))

		def contentsIsomorphic(other :CompositeSQL[_ <: FromClause, _]) :Boolean =
			parts.size == other.parts.size &&
				((parts zip other.parts) forall { case (left, right) => left isomorphic right })



		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case composite :CompositeSQL[_, _] if canEqual(composite) && composite.canEqual(this) =>
				parts == composite.parts
			case _ => false
		}

		override def hashCode :Int = parts.view.map(_.hashCode).reduce(_ * 31 + _)

	}



	object CompositeSQL { //todo: FunctionSQL, ProcedureSQL
		trait CompositeMatcher[+F <: FromClause, +Y[X]]
			extends ConversionMatcher[F, Y] with TupleMatcher[F, Y] with CompositeColumnMatcher[F, Y]

		trait MatchComposite[+F <: FromClause, +Y[X]] extends CompositeMatcher[F, Y]
			with CaseConversion[F, Y] with CaseTuple[F, Y] with MatchCompositeColumn[F, Y]

		trait CaseComposite[+F <: FromClause, +Y[X]] extends MatchComposite[F, Y] {
			def composite[X](e :CompositeSQL[F, X]) :Y[X]

			override def tuple[X](e :TupleSQL[F, X]) :Y[X] = composite(e)
			override def conversion[Z, X](e :ConversionSQL[F, Z, X]) :Y[X] = composite(e)
			override def alias[V](e :AliasedColumn[F, V]) :Y[V] = composite(e)
			override def condition(e :ConditionSQL[F]) :Y[Boolean] = composite(e)
			override def logical(e :LogicalSQL[F]) :Y[Boolean] = composite(e)
		}

	}






	/** Attests that expressions of type `L` and `R` are compatible from the SQL point of view and
	  * can be directly compared in scala after lifting both sides to type `T`.
	  * This may mean for example that, for the purpose of generated SQL, we treat `Option[X]` and `X`
	  * as directly comparable: `SQLTypePromotion[Option[X], X, Option[X]]` or let us promote number types to
	  * a higher precision: `SQLTypePromotion[Int, Long, Long]`.
	  *
	  * @param left a function lifting both `SQLExpression[_, L]` and type `L` itself to a comparable type `T`.
	  * @param right a function lifting both `SQLExpression[_, R]` and type `R` itself to a comparable type `T`.
	  * @tparam L type of the left side of a comparison.
	  * @tparam R type of the right side of a comparison.
	  * @tparam T type to which both types are promoted in order to be directly comparable.
	  */
	class SQLTypePromotion[L, R, T](val left :Lift[L, T], val right :Lift[R, T]) {
		def swapped :SQLTypePromotion[R, L, T] = new SQLTypePromotion(right, left)
		override def toString = s"$left =~= $right"

		/** Converts the value of the left side to the value of the right side, if possible. */
		def l2r(l :L) :Option[R] = right.inverse(left(l))

		/** Converts the value of the right side to the value of the left side, if possible. */
		def r2l(r :R) :Option[L] = left.inverse(right(r))
	}



	sealed abstract class MirroredTypePromotion {
		implicit def mirror[L, R, T](implicit promotion :SQLTypePromotion[L, R, T]) :SQLTypePromotion[R, L, T] =
			promotion.swapped
	}

	object SQLTypePromotion extends MirroredTypePromotion {
		import Lift._

		implicit def directly[T] :SQLTypePromotion[T, T, T] =
			direct.asInstanceOf[SQLTypePromotion[T, T, T]]

		implicit def liftLeft[L, R](implicit lift :Lift[L, R]) :SQLTypePromotion[L, R, R] =
			new SQLTypePromotion(lift, self)

		implicit def liftRight[L, R](implicit lift :Lift[R, L]) :SQLTypePromotion[L, R, L] =
			new SQLTypePromotion(self, lift)


		private[this] val direct = new SQLTypePromotion[Any, Any, Any](Lift.self, Lift.self)



		/** An implicit witness vouching that type `X` is a subtype of `Y` or can be promoted to type `Y`.
		  * It is used to conflate more strict scala types with the same, more loose SQL representations.
		  */ //todo: move this up to SQLExpression
		abstract class Lift[X, Y] {
			def apply(value :X) :Y
			def inverse(value :Y) :Option[X] //todo: we need it for SetComponent, but it may fail or lose precision

			def lower(value :Y) :X = inverse(value) match {
				case Some(x) => x
				case _ => throw new IllegalArgumentException(
					s"Cannot convert $value :${value.unqualifiedClassName} back with $this."
				)
			}

			def apply[F <: FromClause](expr :SQLExpression[F, X]) :SQLExpression[F, Y] = expr.to(this)
			def apply[F <: FromClause](expr :ColumnSQL[F, X]) :ColumnSQL[F, Y] = expr.to(this)
		}



		object Lift { //todo: java type promotion
			implicit def self[T] :Lift[T, T] = ident.asInstanceOf[Lift[T, T]]
			implicit def option[T] :Lift[T, Option[T]] = opt.asInstanceOf[Lift[T, Option[T]]]
//			implicit def some[T] :Lift[Some[T], Option[T]] = new Supertype[Some[T], Option[T]]
			implicit def singleRow[T] :Lift[Rows[T], T] = selectRow.asInstanceOf[Lift[Rows[T], T]]
			implicit def rowSeq[T] :Lift[Rows[T], Seq[T]] = selectRows.asInstanceOf[Lift[Rows[T], Seq[T]]]

			class ComposedLift[X, Y, Z](prev :Lift[X, Y], next :Lift[Y, Z]) extends Lift[X, Z] {

				override def apply(value: X): Z = next(prev(value))

				override def inverse(value: Z): Option[X] = next.inverse(value).flatMap(prev.inverse)

				override def apply[S <: FromClause](expr: SQLExpression[S, X]): SQLExpression[S, Z] =
					next(prev(expr))

				override def apply[F <: FromClause](expr :ColumnSQL[F, X]) :ColumnSQL[F, Z] =
					next(prev(expr))
			}

			private[this] val ident = new Lift[Any, Any] {
				override def apply(value: Any): Any = value
				override def inverse(value: Any): Option[Any] = Some(value)
				override def apply[S <: FromClause](expr: SQLExpression[S, Any]): SQLExpression[S, Any] = expr
				override def apply[S <: FromClause](expr: ColumnSQL[S, Any]) :ColumnSQL[S, Any] = expr
				override def toString = "_"
			}

			private[this] val opt = new Lift[Any, Option[Any]] {

				override def apply(value: Any): Option[Any] = Option(value)

				override def inverse(value: Option[Any]): Option[Any] = value

				override def apply[S <: FromClause](expr: SQLExpression[S, Any]): SQLExpression[S, Option[Any]] = expr.opt
				override def apply[F <: FromClause](expr :ColumnSQL[F, Any]) :ColumnSQL[F, Option[Any]] = expr.opt

				override def toString = "Option[_]"
			}

			private[this] val selectRow = new Lift[Rows[Any], Any] {
				override def apply(value: Rows[Any]): Any = value.head
				override def inverse(value: Any): Option[Rows[Any]] = Some(Rows(value))

				override def toString = "Rows.one"
			}

			private[this] val selectRows = new Lift[Rows[Any], Seq[Any]] {
				override def apply(value: Rows[Any]): Seq[Any] = value.seq
				override def inverse(value: Seq[Any]): Option[Rows[Any]] = value match {
					case Seq(row) => Some(Rows(row))
					case _ => None
				}

				override def toString = "Rows.seq"
			}

		}

	}






	/** A visitor traversing the structure of an SQL expression over tables in `F` and producing a value of `Y[X]`,
	  * where `X` is the value type of the mapped expression. This interface declares a method for every concrete
	  * expression class defined in this package, which is invoked in double dispatch
	  * `apply(expression)`/`expression.applyTo(this)`. This makes for quite a large number of cases, making it
	  * a problem both for maintainers and implementors who may not need to handle every individual type directly.
	  * To alleviate the former, this interface is built by recursively extending partial traits defining methods
	  * for visiting all subclasses of a given expression class. They typically form a one-to-one relationship
	  * with expression classes, with each expression declaring in its companion object (or in the same scope
	  * for inner classes) its own matcher, extending the matchers of its subclasses. To alleviate the latter,
	  * two parallel trait families are introduced: `Case`''Expr'' and `Match`''Expr''. The former implements
	  * all visitor methods for all subclasses of the ''Expr''`SQL`/`SQL`''Expr''/''Expr''`Expression` class
	  * by delegating to the method for the base ''Expr'' class (introduced by the trait if the expression is abstract),
	  * catching all subclasses in a single case. The latter is similar, but doesn't introduce a new method
	  * for abstract expressions and leaves all methods for direct subclasses of ''Expression'' not implemented.
	  * The methods for ''their'' subclasses delegate as in the former example. This in turn has the effect of matching
	  * against a smallest set of classes covering all concrete subclasses of ''Expr''. `ColumnSQL` subtypes
	  * are a bit special, as they have their own hierarchy, parallel to that of plain SQLFormulas.
	  * Wherever an expression type, for example `SQLLiteral`, exists in both non-column and column versions,
	  * the non-column ''Expr''`Matcher` will extend the associated column ''Expr''`Matcher`. The `CaseLiteral` trait
	  * in our example will implement the callback for the column literal by delegating it to the method
	  * for the base literal. In this way, the standard delegation chain for any column expression always starts
	  * by delegating to the non-column method. It is however possible to change by mixing in one or more
	  * of the `Match`''Expr'' or `Case`''Expr'' traits for particular column expression types after the general
	  * matcher traits. For example, `CaseExpression[F, Y] with CaseColumn[F, Y]` will change it
	  * to the opposite: all callbacks for column formulas reach the `column` callback for the root `ColumnSQL`
	  * and, only then, delegate to the `expression` method for `SQLExpression`. An exception to these rules exists
	  * in `MatchComposite`, which includes all methods of `MatchCompositeColumn`. By mixing in a selection
	  * of the above traits, one has a good degree of freedom in selecting which parts of the `SQLExpression` hierarchy
	  * are handled in detail and which are glossed over.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.MatchExpression]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.CaseExpression]]
	  * @see [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher]]
	  */
	trait ExpressionMatcher[+F <: FromClause, +Y[X]]
		extends ColumnMatcher[F, Y] with CompositeMatcher[F, Y] with MappingMatcher[F, Y]
		   with SelectMatcher[F, Y] with TermMatcher[F, Y]
	{
		def apply[X](f: SQLExpression[F, X]): Y[X] = f.applyTo(this)


		def unhandled(e :SQLExpression[F, _]) :Nothing =
			throw new IllegalArgumentException(s"Can't map expression $e :${e.getClass.getName} using $this")

		def unknown[E <: SQLExpression[F, _]](e :E, clazz :Class[E]) :Nothing =
			throw new IllegalArgumentException(s"Can't map expression $e :${e.getClass.getName} using $this - " +
				                               s"unexpected subclass of ${clazz.getName}")

		def unknown[E <: SQLExpression[F, _] :ClassTag](e :E) :Nothing =
			throw new IllegalArgumentException(s"Can't map expression $e :${e.getClass.getName} using $this - " +
			                                   s"unexpected subclass of ${implicitly[ClassTag[E]].runtimeClass.getName}")


		override def toString = this.unqualifiedClassName
	}


	/** A `ExpressionMatcher` delegating all visitor calls to the methods specific to one of the direct `SQLExpression`
	  * subclasses: `term`, `composite`, `select`, `mapping`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionMatcher]]
	  */
	trait MatchExpression[+F <: FromClause, +Y[X]]
		extends CaseComposite[F, Y] with CaseMapping[F, Y] with CaseSelect[F, Y] with CaseTerm[F, Y]

	/** A not particularly useful `ExpressionMatcher` which delegates all the cases to the single `expression` method
	  * invoked for every subexpression (SQL AST node). Used as a base class when only few cases need special handling.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionMatcher]]
	  */
	trait CaseExpression[+F <: FromClause, +Y[X]] extends ExpressionMatcher[F, Y] with MatchExpression[F, Y] {
		def expression[X](e :SQLExpression[F, X]) :Y[X]

		override def composite[X](e: CompositeSQL[F, X]): Y[X] = expression(e)
		override def mapping[M <: Mapping](e :MappingSQL[F, M]) :Y[M#Subject] = expression(e)
		override def select[V, O](e :SelectSQL[F, V, O]) :Y[Rows[V]] = expression(e)
		override def term[X](e: SQLTerm[X]): Y[X] = expression(e)
	}

	trait SelectiveMatcher[+F <: FromClause, +Y[X]] extends CaseExpression[F, Y] {
		override def expression[X](e: SQLExpression[F, X]): Y[X] = unhandled(e)
	}



}

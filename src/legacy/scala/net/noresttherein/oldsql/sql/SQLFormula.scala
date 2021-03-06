package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{ColumnReadForm, Mapping, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.Component
import net.noresttherein.oldsql.schema.MappingPath.ComponentPath
import net.noresttherein.oldsql.sql.RowProduct.{ExtendedBy, RowValues, SelectFrom, SubselectOf, TableFormula}
import net.noresttherein.oldsql.sql.SQLFormula.CompositeFormula.{CaseComposite, CompositeMatcher}
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.sql.AutoConversionFormula.{CaseConversion, ConversionMatcher, OrNull}
import net.noresttherein.oldsql.sql.SQLCondition.{CaseCondition, ConditionMatcher, Equality, In}
import net.noresttherein.oldsql.sql.LogicalFormula.{And, CaseLogical, LogicalMatcher, NotFormula, Or}
import net.noresttherein.oldsql.sql.MappingFormula.{CaseMapping, MappingMatcher}
import net.noresttherein.oldsql.sql.SelectFormula.{CaseRow, CaseRows, CaseSelect, MultipleRowsMatcher, SelectAsRow, SelectAsRows, SelectMatcher, SingleRowMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, Formula, FormulaMatcher, SQLTypePromotion}
import net.noresttherein.oldsql.sql.SQLFormula.SQLTypePromotion.Lift
import net.noresttherein.oldsql.sql.SQLMapper.{FormulaResult, SQLRewriter}
import net.noresttherein.oldsql.sql.SQLTerm.{BoundParameter, CaseTerm, False, SQLLiteral, SQLParameter, TermMatcher, True}
import net.noresttherein.oldsql.sql.SQLTuple.{CaseTuple, SeqFormula, TupleMatcher}
import net.noresttherein.oldsql.slang._
import scala.reflect.ClassTag

import net.noresttherein.oldsql.sql.ast.SQLTerm






/** A representation of an SQL expression as an AST.
  * @tparam F row source - list of tables which provide columns used in this expression
  * @tparam V result type of the expression; may not necessarily be an SQL type, but a result type of some mapping.
  */
trait SQLFormula[-F <: RowProduct, +V] { //todo: add a type parameter which is Bound || Unbound (flag if it contains any abstract/unbound parts)
	import SQLTerm.TermFormulas

	def readForm :SQLReadForm[V]

	def =:[T <: Component[O, E], C <: Component[O, L], O, E, L, R >: V, X]
	      (path :ComponentPath[T, C, O, E, L])(implicit lift :SQLTypePromotion[L, R, X]) :SetComponent[F, T, C, O, E, L, R, X] =
		SetComponent(path, this :SQLFormula[F, R])

	def ==?[U >: V, O, X](value :O)(implicit lift :SQLTypePromotion[U, O, X], form :SQLForm[O]) :BooleanFormula[F] =
		this === value.?

	def ===[S <: F, U >: V, O, X](that :SQLFormula[S, O])(implicit lift :SQLTypePromotion[U, O, X]) :BooleanFormula[S] =
		new Equality(lift.left(this), lift.right(that))



	def in [S <: F, U >: V, O, X](that :SeqFormula[S, O])(implicit lift :SQLTypePromotion[U, O, X]) :BooleanFormula[S] =
		new In[S, X](lift.left(this), SeqFormula(that.parts.map(lift.right.apply[S])))



	def and[S <: F](other :BooleanFormula[S])(implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
		And(this) and other

	def or [S <: F](other :BooleanFormula[S])(implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
		Or(this) or other

	def && [S <: F](other :BooleanFormula[S])(implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
		other match {
			case True() => this
			case False() => other
			case _ => this and other
		}

	def || [S <: F](other :BooleanFormula[S])(implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
		other match {
			case True() => other
			case False() => this
			case _ => this or other
		}

	def unary_![S <: F](implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] = NotFormula(this)



	def boundParameters :Seq[BoundParameter[_]] = collect { case x :BoundParameter[_] => x }

	def parameters :Seq[SQLParameter[_]] = collect { case p :SQLParameter[_] => p }



	def collect[X](fun :PartialFunction[SQLFormula[_ <: RowProduct, _], X]) :Seq[X] = reverseCollect(fun, Nil).reverse

	protected def reverseCollect[X](fun :PartialFunction[SQLFormula[_ <: RowProduct, _], X], acc :List[X]) :List[X] =
		(fun andThen Some.apply).applyOrElse(this, (_:Any) => None) ++: acc




	def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[V] //= matcher.formula(this)



	/** Lift this expression to one typed Option[V], without any effect on the actual generated sql. */
	def opt :SQLFormula[F, Option[V]] = OrNull(this)

	/** Treat this expression as an expression of a source extending (i.e. containing additional tables) the source `F`
	  * this expression is grounded in. */
	def asPartOf[U <: F, S <: RowProduct](implicit ev :U ExtendedBy S) :SQLFormula[S, V] = ev(this)

	/** Treat this expression as an expression of a source extending (i.e. containing additional tables) the source `F`
	  * this expression is grounded in */
	def asPartOf[U <: F, S <: RowProduct](target :S)(implicit ev :U ExtendedBy S) :SQLFormula[S, V] = ev(this)

	/** If this expression is a ground formula - not dependent on `F` or free parameters -
	  * which value can be determined at this point, return its value. */
	def freeValue :Option[V] = None



	def isFree :Boolean = freeValue.isDefined

	def isGroundedIn(tables :Iterable[TableFormula[_, _, _, _]]) :Boolean

	def evaluate(values :RowValues[F]) :V = get(values) getOrElse {
		throw new IllegalArgumentException(s"Couldn't evaluate $this from $values")
	}

	def get(values :RowValues[F]) :Option[V]

//	def as[X >: V] :SQLFormula[F, X] = this.asInstanceOf[SQLFormula[F, X]]



	/** Tests if this expression is equal to the given one abstracting from possibly different sources.
	  * Basically, if both expression would produce the same SQL they should be isomorphic.
	  */
	def isomorphic(expression :Formula[_]) :Boolean

	/** Tests if this expression would produce the same value as the given expression, abstracting from possibly different sources.
	  * Similar to isomorphic, but generally disregards order of elements in composite expressions such as 'and', 'or', seq.
	  * Be warned that this method's primary use is for tests, and production code shouldn't depend on it.
	  */
	private[oldsql] def equivalent(expression :Formula[_]) :Boolean


	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLFormula[_, _]]

}






object SQLFormula {
	/** An upper type bound of all `SQLFormula[_, T]` instances. */
	type Formula[T] = SQLFormula[Nothing, T]

	/** Any SQL expression of `Boolean` value. */
	type BooleanFormula[-F <: RowProduct] = SQLFormula[F, Boolean]






	trait ColumnFormula[-F <: RowProduct, +V] extends SQLFormula[F, V] {
		override def readForm :ColumnReadForm[V]

		override def opt :ColumnFormula[F, Option[V]] = OrNull(this)

		override def asPartOf[U <: F, S <: RowProduct](implicit ev :U ExtendedBy S) :ColumnFormula[S, V] =
			ev(this)

		override def asPartOf[U <: F, S <: RowProduct](target :S)(implicit ev :U ExtendedBy S) :ColumnFormula[S, V] =
			ev(this)
	}




	trait CompositeFormula[-F <: RowProduct, +T] extends SQLFormula[F, T] {
		protected def inOrder :Seq[SQLFormula[F, _]] = parts
		protected def parts :Seq[SQLFormula[F, _]]


		override def isGroundedIn(tables: Iterable[TableFormula[_, _, _, _]]): Boolean =
			parts.forall(_.isGroundedIn(tables))

		override protected def reverseCollect[X](fun: PartialFunction[SQLFormula[_ <: RowProduct, _], X], acc: List[X]): List[X] =
			(super.reverseCollect(fun, acc) /: inOrder)((collected, member) => member.reverseCollect(fun, collected))



		def map[S <: RowProduct](mapper :SQLRewriter[F, S]) :SQLFormula[S, T]


		private[oldsql] override def equivalent(expression: Formula[_]): Boolean = expression match {
			case e:CompositeFormula[_, _] =>
				(e eq this) || e.canEqual(this) && this.canEqual(e) && contentsEquivalent(e)
			case _ => false
		}

		override def isomorphic(expression :Formula[_]) :Boolean = expression match {
			case e:CompositeFormula[_, _] =>
				(this eq e) || canEqual(e) && e.canEqual(this) && contentsIsomorphic(e)
			case _ => false
		}


		def contentsEquivalent(other :CompositeFormula[_ <: RowProduct, _]) :Boolean =
			parts.size == other.parts.size &&
				parts.forall(e => other.parts.exists(_ equivalent e)) &&
				other.parts.forall(e => parts.exists(_ equivalent e))

		def contentsIsomorphic(other :CompositeFormula[_ <: RowProduct, _]) :Boolean =
			parts.size == other.parts.size &&
				((parts zip other.parts) forall { case (left, right) => left isomorphic right })

	}



	object CompositeFormula {
		trait CompositeMatcher[+F <: RowProduct, +Y[X]]
			extends ConversionMatcher[F, Y] with TupleMatcher[F, Y] with ConditionMatcher[F, Y] with LogicalMatcher[F, Y]

		trait MatchComposite[+F <: RowProduct, +Y[X]]
			extends CaseConversion[F, Y] with CaseTuple[F, Y] with CaseCondition[F, Y] with CaseLogical[F, Y]

		trait CaseComposite[+F <: RowProduct, +Y[X]] extends CompositeMatcher[F, Y] with MatchComposite[F, Y] {
			def composite[X](f :CompositeFormula[F, X]) :Y[X]

			def conversion[Z, X](f :AutoConversionFormula[F, Z, X]) :Y[X] = composite(f)

			def tuple[X](f :SQLTuple[F, X]) :Y[X] = composite(f)

			def condition(f :SQLCondition[F]) :Y[Boolean] = composite(f)

			def logical(f :LogicalFormula[F]) :Y[Boolean] = composite(f)
		}

	}






	implicit def implicitTerm[T :SQLForm](value :T) :SQLTerm[T] = SQLLiteral(value)

	implicit def asPartOfExtendingSource[F <: RowProduct, FF <: RowProduct, T]
	(expression :SQLFormula[F, T])(implicit ev :F ExtendedBy FF) :SQLFormula[FF, T] =
		ev(expression)






	/** Attests that expressions of type `L` and `R` are compatible from the SQL point of view and
	  * can be directly compared in scala after lifting both sides to type `T`.
	  * This may mean for example that, for the purpose of generated SQL, we treat `Option[X]` and `X`
	  * as directly comparable: `SQLTypeUnification[Option[X], X, Option[X]]` or let us promote number types to
	  * a higher precision: `SQLTypeUnification[Int, Long, Long]`.
	  *
	  * @param left a function lifting both `SQLFormula[_, L]` and type `L` itself to a comparable type `T`.
	  * @param right a function lifting both `SQLFormula[_, R]` and type `R` itself to a comparable type `T`.
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
		  */
		abstract class Lift[X, Y] {
			def apply(value :X) :Y
			def inverse(value :Y) :Option[X] //todo: we need it for SetComponent, but it may fail or lose precision
			def apply[F <: RowProduct](expr :SQLFormula[F, X]) :SQLFormula[F, Y]
		}



		object Lift { //todo: java type promotion
			implicit def self[T] :Lift[T, T] = ident.asInstanceOf[Lift[T, T]]
			implicit def option[T] :Lift[T, Option[T]] = opt.asInstanceOf[Lift[T, Option[T]]]
//			implicit def some[T] :Lift[Some[T], Option[T]] = new Supertype[Some[T], Option[T]]
			implicit def singleRow[T] :Lift[Rows[T], T] = selectRow.asInstanceOf[Lift[Rows[T], T]]
			implicit def rowSeq[T] :Lift[Rows[T], T] = selectRows.asInstanceOf[Lift[Rows[T], T]]

			class LiftChain[X, Y, Z](prev :Lift[X, Y], next :Lift[Y, Z]) extends Lift[X, Z] {

				override def apply(value: X): Z = next(prev(value))

				override def inverse(value: Z): Option[X] = next.inverse(value).flatMap(prev.inverse)

				override def apply[S <: RowProduct](expr: SQLFormula[S, X]): SQLFormula[S, Z] =
					next(prev(expr))
			}

			private[this] val ident = new Lift[Any, Any] {
				override def apply(value: Any): Any = value
				override def inverse(value: Any): Option[Any] = Some(value)
				override def apply[S <: RowProduct](expr: SQLFormula[S, Any]): SQLFormula[S, Any] = expr
				override def toString = "_"
			}

			private[this] val opt = new Lift[Any, Option[Any]] {

				override def apply(value: Any): Option[Any] = Option(value)

				override def inverse(value: Option[Any]): Option[Any] = value

				override def apply[S <: RowProduct](expr: SQLFormula[S, Any]): SQLFormula[S, Option[Any]] = expr.opt
				override def toString = "Option[_]"
			}

/*
			class Supertype[X <: Y :ClassTag, Y] extends Lift[X, Y] {
				override def apply(value: X): Y = value

				override def inverse(value: Y): Option[X] = value.asSubclassOf[X]

				override def apply[S <: RowProduct](expr: SQLFormula[S, X]): SQLFormula[S, Y] = expr.as[Y]
				override def toString = "_<:X"
			}
*/

			private[this] val selectRow = new Lift[Rows[Any], Any] {
				override def apply(value: Rows[Any]): Any = value.head
				override def inverse(value: Any): Option[Rows[Any]] = Some(Rows(value))

				override def apply[S <: RowProduct](expr: SQLFormula[S, Rows[Any]]): SQLFormula[S, Any] =
					expr.asSubclassOf[SelectFormula[S, _, Any]].map(_.single) getOrElse {
						throw new IllegalArgumentException(s"Can't lift a non-select expression $expr to a single row select formula")
					}

				override def toString = "Rows.one"
			}

			private[this] val selectRows = new Lift[Rows[Any], Seq[Any]] {
				override def apply(value: Rows[Any]): Seq[Any] = Seq(value.seq)
				override def inverse(value: Seq[Any]): Option[Rows[Any]] = value match {
					case Seq(row) => Some(Rows(row))
					case _ => None
				}

				override def apply[S <: RowProduct](expr: SQLFormula[S, Rows[Any]]): SQLFormula[S, Seq[Any]] =
					expr.asSubclassOf[SelectFormula[S, _, Any]].map(_.rows) getOrElse {
						throw new IllegalArgumentException(s"Can't lift a non-select expression $expr to a row seq formula")
					}

				override def toString = "Rows.seq"
			}

		}


	}






	/** A visitor traversing the structure of an SQL formula over tables in `F` and producing a value of `Y[X]`,
	  * where `X` is the value type of the mapped formula. This interface declares a method for every concrete formula
	  * class defined in this package, which is invoked in a double dispatch `apply(formula)`/`formula.applyTo(this)`.
	  * This makes for quite a large number of cases, making it a problem both for maintainers and implementors
	  * who may not need to handle every individual type directly. To alleviate the former, this interface
	  * is built by recursively extending partial traits defining methods for visiting all subclasses of a given
	  * formula class. They typically form a one-to-one relationship with formula classes, with each formula
	  * declaring in its companion object its own matcher, extending the matchers of its subclasses.
	  * To alleviate the latter, two parallel interface families are introduced: `Case''Formula''`
	  * and `Match''Formula''`. The former implements all visitor methods for all subclasses of ''Formula''
	  * by delegating to the method for the ''Formula'' class (introduced by the trait if the formula is abstract),
	  * catching all subclasses in a single case. The latter is similar, but doesn't introduce a new method for
	  * abstract formulas and leaves all methods for direct subclasses of ''Formula'' not implemented. The methods
	  * for ''their'' subclasses delegate as in the former example. This in turn has the effect of matching against
	  * a smallest set of classes covering all concrete subclasses of ''Formula''. By mixing in a selection of above
	  * traits, one has a good degree of freedom in which parts of the `SQLFormula` hierarchy are handled in detail
	  * and which are glossed over.
	  * @see [[net.noresttherein.oldsql.sql.SQLFormula.MatchFormula]]
	  * @see [[net.noresttherein.oldsql.sql.SQLFormula.CaseFormula]]
 	  */
	trait FormulaMatcher[+F <: RowProduct, +Y[X]] extends SQLMapper[F, Y]
		with TermMatcher[F, Y] with CompositeMatcher[F, Y] with SelectMatcher[F, Y] with MappingMatcher[F, Y]
	{
		override def apply[X](f: SQLFormula[F, X]): Y[X] = f.applyTo(this) //f.extract(this) getOrElse unhandled(f)
	}


	/** A `FormulaMatcher` delegating all visitor calls to the methods specific to one of the direct `SQLFormula`
	  * subclasses: `term`, `composite`, `select`, `mapping`.
	  * @see [[net.noresttherein.oldsql.sql.SQLFormula.FormulaMatcher]]
	  */
	trait MatchFormula[+F <: RowProduct, +Y[X]] //extends FormulaMatcher[F, Y]
		extends CaseTerm[F, Y] with CaseComposite[F, Y] with CaseSelect[F, Y] with CaseMapping[F, Y]

	/** A not particularly useful `FormulaMatcher` which delegates all the cases to the single `formula` method
	  * invoked for every subformula (SQL AST node).
	  * @see [[net.noresttherein.oldsql.sql.SQLFormula.FormulaMatcher]]
	  */
	trait CaseFormula[+F <: RowProduct, +Y[X]] extends FormulaMatcher[F, Y] with MatchFormula[F, Y] {
		def formula[X](f :SQLFormula[F, X]) :Y[X]

		override def term[X](f: SQLTerm[X]): Y[X] = formula(f)

		override def composite[X](f: CompositeFormula[F, X]): Y[X] = formula(f)

		override def select[X](f :SelectFormula[F, X]) :Y[Rows[X]] = formula(f)

		override def mapping[M <: Mapping](f :MappingFormula[F, M]) :Y[M#Subject] = formula(f)

	}

	trait MatchSuper[+F <: RowProduct, +Y[X]] extends CaseFormula[F, Y] {
		override def formula[X](f: SQLFormula[F, X]): Y[X] = unhandled(f)
	}



}

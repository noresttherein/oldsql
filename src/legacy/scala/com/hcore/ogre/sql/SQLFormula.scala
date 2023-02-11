package com.hcore.ogre.sql

import com.hcore.ogre.mapping.ComponentPath.{DirectComponent, TypedComponentPath, SelfPath}
import com.hcore.ogre.mapping.Mapping.ColumnFilter.AllColumns
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping._
import com.hcore.ogre.mapping.support.BaseMappings.MappingSupport
import com.hcore.ogre.mapping.support.MappingAdapter.{IsomorphicMappingAdapter, GenericMappingImpostor, MappingImpostor}
import com.hcore.ogre.morsels.{Names, InverseIndexSeq}
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.matching.{Unapply, GenUnapply}
import com.hcore.ogre.sql.RowSource._
import com.hcore.ogre.sql.SQLFormula.And.{AndMatcher, CaseAnd}
import com.hcore.ogre.sql.SQLFormula.AutoConversionFormula.{ConversionMatcher, CaseConversion}
import com.hcore.ogre.sql.SQLFormula.BoundParameter.BoundParameterMatcher
import com.hcore.ogre.sql.SQLFormula.ComparisonFormula.{ComparisonMatcher, CaseComparison}
import com.hcore.ogre.sql.SQLFormula.ComponentFormula.{ComponentMatcher, CaseComponent}
import com.hcore.ogre.sql.SQLFormula.CompositeFormula.{CompositeMatcher, CaseComposite}
import com.hcore.ogre.sql.SQLFormula.ConditionFormula.{ConditionMatcher, CaseCondition}
import com.hcore.ogre.sql.SQLFormula.Equality.{EqualityMatcher, CaseEquality}
import com.hcore.ogre.sql.SQLFormula.ExistsFormula.{ExistsMatcher, CaseExists}
import com.hcore.ogre.sql.SQLFormula.FormulaEqualizer.Lift
import com.hcore.ogre.sql.SQLFormula.HListFormula.{HListMatcher, CaseHList}
import com.hcore.ogre.sql.SQLFormula.In.{InMatcher, CaseIn}
import com.hcore.ogre.sql.SQLFormula.Literal.{LiteralMatcher, CaseLiteral}
import com.hcore.ogre.sql.SQLFormula.LogicalFormula.{LogicalMatcher, CaseLogical}
import com.hcore.ogre.sql.SQLFormula.MappingFormula.{MappingMatcher, CaseMapping}
import com.hcore.ogre.sql.SQLFormula.NativeTerm.{NativeMatcher, CaseNative}
import com.hcore.ogre.sql.SQLFormula.NotFormula.{NotMatcher, CaseNot}
import com.hcore.ogre.sql.SQLFormula.Null.{NullMatcher, CaseNull}
import com.hcore.ogre.sql.SQLFormula.Or.{OrMatcher, CaseOr}
import com.hcore.ogre.sql.SQLFormula.ParameterFormula.{ParameterMatcher, CaseParameter}
import com.hcore.ogre.sql.SQLFormula.PathFormula.{MatchPath, PathMatcher, CasePath}
import com.hcore.ogre.sql.SQLFormula.SelectFormula._
import com.hcore.ogre.sql.SQLFormula.SeqFormula.{SeqMatcher, CaseSeq}
import com.hcore.ogre.sql.SQLFormula.TermFormula.{TermMatcher, CaseTerm}
import com.hcore.ogre.sql.SQLFormula.TupleFormula.{TupleMatcher, CaseTuple}
import com.hcore.ogre.sql.SQLFormula._
import com.hcore.ogre.sql.SQLForm.EmptyForm
import com.hcore.ogre.sql.SQLMapper.{FormulaResult, SQLRewriter}
import com.hcore.ogre.sql.SQLReadForm.HListReadFormImpl
import com.hcore.ogre.sql.SQLScribe.{ConcreteSQLScribe, AbstractSQLScribe}
import shapeless.{::, HList, HNil}

import scala.reflect.ClassTag
import scala.slick.jdbc.{GetResult, SetParameter}
import scala.util.Try


//implicits
import SaferCasts._
import InverseIndexSeq.implicitIndexing



/** A representation of an SQL expression as an AST.
  * @tparam R row source - list of tables which provide columns used in this expression
  * @tparam T result type of the expression; may not necessarily be an sql type, but a result type of some mapping.
  */
trait SQLFormula[-R<:RowSource, T] { //todo: add a type parameter which is Bound || Unbound (flag if it contains any abstract/unbound parts)
	import SQLFormula.TermFormulas

	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLFormula[_, _]]

	def =:[M<:AnyMapping, C<:AnyMapping, X](path :ComponentPath[M, C])(implicit lift :FormulaEqualizer[C#ResultType, T, X]) :SetComponent[R, M, C, T, X] =
		SetComponent[R, M, C, T, X](path, this)

	def ==?[O, X](value :O)(implicit lift :FormulaEqualizer[T, O, X], form :SQLForm[O]) :SQLFormula[R, Boolean] =
		this === value.?

	def ===[S<:R, O, X](that :SQLFormula[S, O])(implicit lift :FormulaEqualizer[T, O, X]) :SQLFormula[S, Boolean] =
		new Equality(lift.left(this), lift.right(that))

	def in [S<:R, O, X](that :SeqFormula[S, O])(implicit lift :FormulaEqualizer[T, O, X]) :SQLFormula[S, Boolean] =
		new In(lift.left(this), SeqFormula(that.parts.map(lift.right(_))))

	def and [S<:R](other :SQLFormula[S, Boolean])(implicit ev :this.type<:<SQLFormula[S, Boolean]) :SQLFormula[S, Boolean] =
		other match {
			case And(conditions) => And(ev(this) +: conditions)
			case _ => And(Seq(ev(this), other))
		}
	
	def or [S<:R](other :SQLFormula[S, Boolean])(implicit ev :this.type<:<SQLFormula[S, Boolean]) :SQLFormula[S, Boolean] =
		other match {
			case Or(conditions) => Or(ev(this) +: conditions)
			case _ => Or(Seq(ev(this), other))
		}

	def && [S<:R](other :SQLFormula[S, Boolean])(implicit ev :this.type<:<SQLFormula[S, Boolean]) :SQLFormula[S, Boolean] =
		other match {
			case True() => ev(this)
			case False() => other
			case _ => ev(this) and other
		}

	def || [S<:R](other :SQLFormula[S, Boolean])(implicit ev :this.type<:<SQLFormula[S, Boolean]) :SQLFormula[S, Boolean] =
		other match {
			case True() => other
			case False() => ev(this)
			case _ => ev(this) or other
		}


	def unary_![S<:R](implicit ev :this.type<:<SQLFormula[S, Boolean]) = NotFormula(ev(this))


	def boundParameters :Seq[BoundParameter[_]] = collect{ case x:BoundParameter[_] => x }


	def parameters :Seq[ParameterFormula[_]] = collect { case p :ParameterFormula[_] => p }

	
	def collect[X](fun :PartialFunction[SQLFormula[_<:RowSource, _], X]) :Seq[X] = reverseCollect(fun, Seq()).reverse
	
	protected def reverseCollect[X](fun :PartialFunction[SQLFormula[_<:RowSource, _], X], acc :Seq[X]) :Seq[X] =
		(fun andThen Some.apply).applyOrElse(this, (_:Any) => None) ++: acc
	
	

	def applyTo[Y[X]](matcher :FormulaMatcher[R, Y]) :Y[T] //= matcher.formula(this)


	/** Tests if this expression is equal to the given one abstracting from possibly different sources.
	  * Basically, if both expression would produce the same SQL they should be isomorphic.
	  */
	def isomorphic(expression :Formula[_]) :Boolean

	/** Tests if this expression would produce the same value as the given expression, abstracting from possibly different sources.
	  * Similar to isomorphic, but generally disregards order of elements in composite expressions such as 'and', 'or', seq.
	  * Be warned that this method's primary use is for tests, and production code shouldn't depend on it.
	  */
	def equivalent(expression :Formula[_]) :Boolean

	/** Lift this expression to one typed Option[T], without any effect on the actual generated sql. */
	def opt :SQLFormula[R, Option[T]] = OrNull(this)

	/** Treat this expression as an expression of a source extending (i.e. containing additional tables) source R this expression is grounded in */
	def asPartOf[U<:R, S<:RowSource](implicit ev :U ExtendedBy S) :SQLFormula[S, T] = ev(this)

	/** Treat this expression as an expression of a source extending (i.e. containing additional tables) source R this expression is grounded in */
	def asPartOf[U<:R, S<:RowSource](target :S)(implicit ev :U ExtendedBy S) :SQLFormula[S, T] = ev(this)
	
	/** If this expression is a ground formula - not dependent on R or free parameters -
	  * which value can be determined at this point, return its value.
	  */
	def groundValue :Option[T] = None



	def isGroundedIn(tables :Iterable[TableFormula[_, _<:AnyMapping]]) :Boolean

	def isGrounded = groundValue.isDefined

	def evaluate(values :RowValues[R]) :T = get(values) getOrElse {
		throw new IllegalArgumentException(s"Couldn't evaluate $this from $values")
	}

	def get(values :RowValues[R]) :Option[T]

	def as[X>:T] = this.asInstanceOf[SQLFormula[R, X]]

	def readForm :SQLReadForm[T]
}




object SQLFormula {

	type Formula[T] = SQLFormula[Nothing, T]
	type BooleanFormula[-R<:RowSource] = SQLFormula[R, Boolean]



	trait FormulaMatcher[+S<:RowSource, +Y[X]] extends SQLMapper[S, Y]
		with TermMatcher[S, Y] with CompositeMatcher[S, Y] with SelectMatcher[S, Y] with MappingMatcher[S, Y]
	{
		override def apply[X](f: SQLFormula[S, X]): Y[X] = f.applyTo(this) //f.extract(this) getOrElse unhandled(f)
	}


	trait MatchFormula[+S<:RowSource, +Y[X]] //extends FormulaMatcher[S, Y]
		extends CaseTerm[S, Y] with CaseComposite[S, Y] with CaseSelect[S, Y] with CaseMapping[S, Y]

	trait CaseFormula[+S<:RowSource, +Y[X]] extends FormulaMatcher[S, Y] with MatchFormula[S, Y] {
		def formula[X](f :SQLFormula[S, X]) :Y[X]

		def term[X](f: TermFormula[X]): Y[X] = formula(f)

		def composite[X](f: CompositeFormula[S, X]): Y[X] = formula(f)

		def select[X](f :SelectFormula[S, X]) :Y[RowCursor[X]] = formula(f)

		def mapping[M<:AnyMapping](f :MappingFormula[S, M]) :Y[M#ResultType] = formula(f)

	}

	trait MatchSuper[+S<:RowSource, +Y[X]] extends CaseFormula[S, Y] {
		override def formula[X](f: SQLFormula[S, X]): Y[X] = unhandled(f)
	}

	/** Assert that MatchSuper implements all methods in FormulaMatcher, and thus all CaseXxx cases are covered by redirecting to a case for superclass */
	private[this] object ExhaustiveImplementationTest extends FormulaMatcher[RowSource, Option] with MatchSuper[RowSource, Option]

//	trait SQLTerm[-R<:RowSource, T] extends SQLFormula[R, T]
//
//	trait SQLSymbol[-R<:RowSource, T] extends SQLTerm[R, T]
//
//	trait SQLConstant[T] extends SQLTerm[RowSource, T]
//
//	trait SQLBoundParam[T] extends SQLConstant[T]
//
//	trait SQLLiteral[T] extends SQLConstant[T]
//
//	//True/False
//
//	trait SQLNull[T] extends SQLLiteral[T]
//
//	trait SQLNativeTerm[T] extends SQLTerm[RowSource, T]
//
//	trait SQLMappedTerm[-R<:RowSource, M<:Mapping] extends SQLTerm[R, M#ResultType]
//
//	trait SQLPath[-R<:RowSource, T<:Mapping, M<:Mapping] extends SQLMappedTerm[R, M]
//
//	trait SQLComponent[-R<:RowSource, T<:Mapping, M<:Mapping] extends SQLPath[R, T, M]
//
//	trait SQLAutoConversion[-R<:RowSource, S, T] extends SQLFormula[R, T]
//
//	trait SQLSelect[-R<:RowSource, T] extends SQLFormula[R, T] with Mapping[T]
//
//	trait SQLSelectAs[-R<:RowSource, M<:Mapping] extends SQLSelect[R, M#ResultType] with IsomorphicMappingAdapter[M#ResultType, M]
//
//	trait GroundSelect[T] extends SQLSelect[RowSource, T]
//
//	trait GroundSelectAs[M<:Mapping] extends GroundSelect[M#ResultType] with SQLSelectAs[RowSource, M]





	/** Attests that expressions of type L and R are compatible from SQL point's of view and 
	  * can be directly compared in scala after lifting both sides to type T.
	  * This may mean for example that, for the purpose of SQL, we treat Option[X] and X is directly comparable:
	  * ExpressionEqualizer[Option[X], X, Option[X]] or let as promote number types to higher precision:
	  * ExpressionEqualizer[Int, Long, Long]. Please not that while the rules for type promoting are symmetrical
	  * there is no implicit definition inversing an existing implict from ExpressionEqualizer[L, R, T]
	  * to FormulaEqualizer[R, L, T], as it would lead to ambigous implicits in static context.
	  * The downside is that if you declare an implicit parameter ExpressionEqualizer[L, R, T]
	  * you can write (e1 :SQLFormula[L]) === (e2 :SQLFormula[R]), but not e2===e1.
	  * For that, import ce.inverse, where ce :ExpressionEqualizer[L, R, T].
	  *
	  * @param left function lifting both SQLFormula[_, L] and type L itself to the comparable type T
	  * @param right function lifting both SQLFormula[_, R] and type R itself to the comparable type T
	  * @tparam L type of the left side of a comparison
	  * @tparam R type of the right side of a comparison
	  * @tparam T type to which both types are promoted in order to be directly comparable
	  */
	class FormulaEqualizer[L, R, T](val left :Lift[L, T], val right :Lift[R, T]) {
		implicit def inverse :FormulaEqualizer[R, L, T] = new FormulaEqualizer(right, left)
		override def toString = s"$left =?= $right"

		/** Converts the value of the left side to the value of the right side, if possible. */
		def l2r(l :L) :Option[R] = right.inverse(left(l))

		/** Converts the value of the right side to the value of the left side, if possible. */
		def r2l(r :R) :Option[L] = left.inverse(right(r))
	}

	object FormulaEqualizer {
		import Lift._

		implicit def directly[T] :FormulaEqualizer[T, T, T] =
			direct.asInstanceOf[FormulaEqualizer[T, T, T]]

		implicit def liftLeft[L, R](implicit lift :Lift[L, R]) :FormulaEqualizer[L, R, R] =
			new FormulaEqualizer(lift, self)

		implicit def liftRight[L, R](implicit lift :Lift[R, L]) :FormulaEqualizer[L, R, L] =
			new FormulaEqualizer(self, lift)


		private[this] val direct = new FormulaEqualizer[Any, Any, Any](Lift.self, Lift.self)

		abstract class Lift[X, Y] {
			def apply(value :X) :Y
			def inverse(value :Y) :Option[X]
			def apply[S<:RowSource](expr :SQLFormula[S, X]) :SQLFormula[S, Y]
		}

		object Lift {
			implicit def self[T] :Lift[T, T] = ident.asInstanceOf[Lift[T, T]]
			implicit def option[T] :Lift[T, Option[T]] = opt.asInstanceOf[Lift[T, Option[T]]]
			implicit def some[T] :Lift[Some[T], Option[T]] = new Supertype[Some[T], Option[T]]
			implicit def singleRow[T] :Lift[RowCursor[T], T] = selectRow.asInstanceOf[Lift[RowCursor[T], T]]
			implicit def rowSeq[T] :Lift[RowCursor[T], T] = selectRows.asInstanceOf[Lift[RowCursor[T], T]]

			class LiftChain[X, Y, Z](prev :Lift[X, Y], next :Lift[Y, Z]) extends Lift[X, Z] {

				override def apply(value: X): Z = next(prev(value))

				override def inverse(value: Z): Option[X] = next.inverse(value).flatMap(prev.inverse)

				override def apply[S <: RowSource](expr: SQLFormula[S, X]): SQLFormula[S, Z] =
					next(prev(expr))
			}

			private[this] val ident = new Lift[Any, Any] {

				override def apply(value: Any): Any = value

				override def inverse(value: Any): Option[Any] = Some(value)

				override def apply[S <: RowSource](expr: SQLFormula[S, Any]): SQLFormula[S, Any] = expr
				override def toString = "X"
			}

			private[this] val opt = new Lift[Any, Option[Any]] {

				override def apply(value: Any): Option[Any] = Option(value)

				override def inverse(value: Option[Any]): Option[Any] = value

				override def apply[S <: RowSource](expr: SQLFormula[S, Any]): SQLFormula[S, Option[Any]] = expr.opt
				override def toString = "Option[X]"
			}

			class Supertype[X<:Y :ClassTag, Y] extends Lift[X, Y] {
				override def apply(value: X): Y = value

				override def inverse(value: Y): Option[X] = value.asSubclassOf[X]

				override def apply[S <: RowSource](expr: SQLFormula[S, X]): SQLFormula[S, Y] = expr.as[Y]
				override def toString = "_<:X"
			}

			private[this] val selectRow = new Lift[RowCursor[Any], Any] {
				override def apply(value: RowCursor[Any]): Any = value.head
				override def inverse(value: Any): Option[RowCursor[Any]] = Some(RowCursor(value))

				override def apply[S <: RowSource](expr: SQLFormula[S, RowCursor[Any]]): SQLFormula[S, Any] =
					expr.asSubclassOf[SelectFormula[S, Any]].map(_.single) getOrElse {
						throw new IllegalArgumentException(s"Can't lift a non-select expression $expr to a one row select formula")
					}
			}

			private[this] val selectRows = new Lift[RowCursor[Any], Seq[Any]] {
				override def apply(value: RowCursor[Any]): Seq[Any] = Seq(value.seq)
				override def inverse(value: Seq[Any]): Option[RowCursor[Any]] = value match {
					case Seq(row) => Some(RowCursor(row))
					case _ => None
				}

				override def apply[S <: RowSource](expr: SQLFormula[S, RowCursor[Any]]): SQLFormula[S, Seq[Any]] =
					expr.asSubclassOf[SelectFormula[S, Any]].map(_.rows) getOrElse {
						throw new IllegalArgumentException(s"Can't lift a non-select expression $expr to a row seq formula")
					}
			}

		}


	}




	implicit def implicitTerm[T :SQLForm](value :T) :TermFormula[T] = Literal(value)

	implicit def asPartOfExtendedSource[S<:RowSource, R<:RowSource, T](expression :SQLFormula[S, T])(implicit ev :S ExtendedBy R) :SQLFormula[R, T] =
		ev(expression)




	implicit class TermFormulas[T](val term :T)(implicit sqlForm :SQLForm[T]) {
		def ? = BoundParameter(term)
	}

	
	abstract class TermFormula[T :SQLForm] extends SQLFormula[RowSource, T] {

		def sqlForm = SQLWriteForm[T]
		def readForm = SQLReadForm[T]


		override def isomorphic(expression: Formula[_]): Boolean = this == expression

		override def equivalent(expression: Formula[_]): Boolean = this == expression

		override def isGroundedIn(tables: Iterable[TableFormula[_, _ <: AnyMapping]]): Boolean = groundValue.isDefined

		override def get(values :RowValues[RowSource]) = groundValue
	}

	object TermFormula {
		trait TermMatcher[+S<:RowSource, +Y[X]]
			extends LiteralMatcher[S, Y] with ParameterMatcher[S, Y] with NullMatcher[S, Y] with NativeMatcher[S, Y]

		trait MatchTerm[+S<:RowSource, +Y[X]] //extends TermMatcher[S, Y]
			extends CaseLiteral[S, Y] with CaseParameter[S, Y] with CaseNull[S, Y] with CaseNative[S, Y]

		trait CaseTerm[+S<:RowSource, +Y[X]] extends TermMatcher[S, Y] with MatchTerm[S, Y] {
			def term[X](f :TermFormula[X]) :Y[X]

			def param[X](f: BoundParameter[X]): Y[X] = term(f)

			def literal[X](f: Literal[X]): Y[X] = term(f)

			def sqlNull[X](f: Null[X]): Y[X] = term(f)

			def native[X](f: NativeTerm[X]): Y[X] = term(f)

		}


	}

	
	class Literal[T :SQLForm](val value :T) extends TermFormula[T] {
		override def &&[S <: RowSource](other: SQLFormula[S, Boolean])(implicit ev: this.type <:< SQLFormula[S, Boolean]): SQLFormula[S, Boolean] =
			if (value.asInstanceOf[Boolean]) other
			else ev(this)

		override def ||[S <: RowSource](other: SQLFormula[S, Boolean])(implicit ev: this.type <:< SQLFormula[S, Boolean]): SQLFormula[S, Boolean] =
			if (value.asInstanceOf[Boolean]) ev(this)
			else ev(this)


		override def opt: SQLFormula[RowSource, Option[T]] = Literal(Option(value))

		override def groundValue = Some(value)


		override def applyTo[Y[X]](matcher: FormulaMatcher[RowSource, Y]): Y[T] = matcher.literal(this)



		override def canEqual(that :Any) = that.isInstanceOf[Literal[_]]

		override def equals(that :Any) = that match {
			case l:Literal[_] if l.canEqual(this) => l.value==value
			case _ => false
		}

		override def toString = String.valueOf(value)
	}


	object Literal {
		def apply[T :SQLForm](value :T) :Literal[T] = new Literal(value)

		def unapply[T](expr :SQLFormula[_, T]) :Option[(T, SQLWriteForm[T])] = expr match {
			case l:Literal[_] => Some(l.value.asInstanceOf[T], l.sqlForm.asInstanceOf[SQLWriteForm[T]])
			case _ => None
		}

		trait LiteralMatcher[+S<:RowSource, +Y[X]] {
			def literal[X](f :Literal[X]) :Y[X]
		}

		trait MatchLiteral[+S<:RowSource, +Y[X]] extends LiteralMatcher[S, Y] {
			def nonbool[X](f :Literal[X]) :Y[X]

			def bool(f :Literal[Boolean]) :Y[Boolean]

			override def literal[X](f: Literal[X]): Y[X] = f.value match {
				case b:Boolean => bool(f.asInstanceOf[Literal[Boolean]]).asInstanceOf[Y[X]]
				case _ => nonbool(f)
			}
		}

		type CaseLiteral[+S<:RowSource, +Y[X]] = LiteralMatcher[S, Y]
	}

	
	
	
	abstract class ParameterFormula[T :SQLForm] extends TermFormula[T]

	object ParameterFormula {

		type ParameterMatcher[+S<:RowSource, +Y[X]] = BoundParameterMatcher[S, Y]

		type MatchParameter[+S<:RowSource, +Y[X]] = ParameterMatcher[S, Y]

		type CaseParameter[+S<:RowSource, +Y[X]] = ParameterMatcher[S, Y]
	}

	class BoundParameter[T :SQLForm](val value :T, val name :Option[String]=None) extends ParameterFormula[T] {
		def writeForm = SQLWriteForm.const[T](value) :SQLWriteForm[Unit]

		override def opt = BoundParameter(Option(value))

		override def groundValue = Some(value)


		override def applyTo[Y[X]](matcher: FormulaMatcher[RowSource, Y]): Y[T] = matcher.param(this)


		override def canEqual(that :Any) = that.isInstanceOf[BoundParameter[_]]

		override def equals(that :Any) = that match {
			case p:BoundParameter[_] =>
				(p eq this) || (p.canEqual(this) && p.value==value)
			case _ => false
		}

		override def hashCode = if (value==null) 0 else value.hashCode

		override def toString = if (value==null) "null" else "?"+value
	}

	object BoundParameter {
		def apply[T :SQLForm](value :T, name :Option[String]=None) :BoundParameter[T] =
			new BoundParameter(value, name)

		def unapply[T](e :SQLFormula[_, T]) :Option[(T, SQLWriteForm[T])] = e match {
			case p :BoundParameter[_] => Some(p.value.asInstanceOf[T], p.sqlForm.asInstanceOf[SQLWriteForm[T]])
			case _ => None
		}

		object WriteForm {
			def unapply(e :SQLFormula[_, _]) :Option[SQLWriteForm[Any]] = e match {
				case BoundParameter(value, form) =>
					Some(SQLWriteForm.const(value)(form.asInstanceOf[SQLWriteForm[Any]]))
				case _ => None
			}
		}


		trait BoundParameterMatcher[+S<:RowSource, +Y[X]] {
			def param[X](f :BoundParameter[X]) :Y[X]
		}

		type MatchBoundParameter[+S<:RowSource, +Y[X]] = BoundParameterMatcher[S, Y]

		type CaseBoundParameter[+S<:RowSource, +Y[X]] = BoundParameterMatcher[S, Y]


	}






	class Null[T :SQLForm] private[SQLFormula] extends TermFormula[T] {
		override val sqlForm = SQLForm[T]

		override def groundValue = Some(sqlForm.nullValue)

		override def opt: SQLFormula[RowSource, Option[T]] = Null[Option[T]]



		override def applyTo[Y[X]](matcher: FormulaMatcher[RowSource, Y]): Y[T] = matcher.sqlNull(this)

		override def canEqual(that :Any) = that.isInstanceOf[Null[_]]

		override def equals(that :Any) = that match {
			case n:Null[_] => groundValue==n.groundValue
			case _ => false
		}

		override def hashCode = groundValue.hashCode

		override def toString = "Null"
	}

	object Null {
		def apply[T :SQLForm] = new Null[T]

		def unapply(expression :SQLFormula[_, _]) :Boolean = expression match {
			case null => true
			case _:Null[_] => true
			case Literal(null, _) => true
			case _ => false
		}


		trait NullMatcher[+S<:RowSource, +Y[X]] {
			def sqlNull[X](f :Null[X]) :Y[X]
		}

		type MatchNull[+S<:RowSource, +Y[X]] = NullMatcher[S, Y]

		type CaseNull[+S<:RowSource, +Y[X]] = NullMatcher[S, Y]

	}


	object True extends Literal[Boolean](true) {
		def apply() = this

		def unapply(expression :SQLFormula[Nothing, _]) :Boolean = expression match {
			case Literal(v :Boolean, _) => v
			case _ => false
		}

	}


	object False extends Literal[Boolean](false) {
		def apply() = this

		def unapply(expression :SQLFormula[Nothing, _]) :Boolean = expression match {
			case Literal(v :Boolean, _) => !v
			case _ => false
		}
	}



	case class NativeTerm[T :SQLForm](value :String) extends TermFormula[T] {

		override def applyTo[Y[X]](matcher: FormulaMatcher[RowSource, Y]): Y[T] =
			matcher.native(this)

		override def toString = value
	}

	object NativeTerm {
		trait NativeMatcher[+S<:RowSource, +Y[X]] {
			def native[X](f :NativeTerm[X]) :Y[X]
		}

		type MatchNative[+S<:RowSource, +Y[X]] = NativeMatcher[S, Y]

		type CaseNative[+S<:RowSource, +Y[X]] = NativeMatcher[S, Y]

	}

	trait CompositeFormula[-R<:RowSource, T] extends SQLFormula[R, T] {
		def parts :Seq[SQLFormula[R, _]]


		def map[S<:RowSource](mapper :SQLRewriter[R, S]) :SQLFormula[S, T]

		override def isGroundedIn(tables: Iterable[TableFormula[_, _ <: AnyMapping]]): Boolean =
			parts.forall(_.isGroundedIn(tables))

		override protected def reverseCollect[X](fun: PartialFunction[SQLFormula[_<:RowSource, _], X], acc: Seq[X]): Seq[X] =
			super.reverseCollect(fun, (acc /: parts)((collected, member) => member.reverseCollect(fun, collected)))


		override def equivalent(expression: Formula[_]): Boolean = expression match {
			case e:CompositeFormula[_, _] =>
				(e eq this) || e.canEqual(this) && this.canEqual(e) && contentsEquivalent(e)
			case _ => false
		}

		override def isomorphic(expression :Formula[_]) :Boolean = expression match {
			case e:CompositeFormula[_, _] =>
				(this eq e) || canEqual(e) && e.canEqual(this) && contentsIsomorphic(e)
			case _ => false
		}


		def contentsEquivalent(other :CompositeFormula[_<:RowSource, _]) :Boolean =
			parts.size==other.parts.size &&
			parts.forall(e => other.parts.exists(_ equivalent e)) &&
			other.parts.forall(e => parts.exists(_ equivalent e))

		def contentsIsomorphic(other :CompositeFormula[_<:RowSource, _]) :Boolean =
			parts.size==other.parts.size &&
				((parts zip other.parts) forall { case (left, right) => left isomorphic right })
	}

	object CompositeFormula {
		trait CompositeMatcher[+S<:RowSource, +Y[X]]
			extends ConversionMatcher[S, Y] with TupleMatcher[S, Y] with ConditionMatcher[S, Y] with LogicalMatcher[S, Y]

		trait MatchComposite[+S<:RowSource, +Y[X]]
			extends CaseConversion[S, Y] with CaseTuple[S, Y] with CaseCondition[S, Y] with CaseLogical[S, Y]

		trait CaseComposite[+S<:RowSource, +Y[X]] extends CompositeMatcher[S, Y] with MatchComposite[S, Y] {
			def composite[X](f :CompositeFormula[S, X]) :Y[X]

			def conversion[Z, X](f :AutoConversionFormula[S, Z, X]) :Y[X] = composite(f)

			def tuple[X](f :TupleFormula[S, X]) :Y[X] = composite(f)

			def condition(f :ConditionFormula[S]) :Y[Boolean] = composite(f)

			def logical(f :LogicalFormula[S]) :Y[Boolean] = composite(f)
		}

	}


	trait AutoConversionFormula[-R<:RowSource, S, T] extends CompositeFormula[R, T] {
		def expr :SQLFormula[R, S]
		
		def lift(s :S) :T
		
		override def parts = Seq(expr)
		
		override def readForm = expr.readForm.map(lift)

		override def groundValue = expr.groundValue.map(lift)

		override def get(values :RowValues[R]) = expr.get(values).map(lift)


		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[T] = matcher.conversion(this)


		def sameAs(that :Formula[_]) :Boolean = canEqual(that)

		override def isomorphic(expression: Formula[_]): Boolean = expression match {
			case auto :AutoConversionFormula[_, _, _] if sameAs(auto) => expr isomorphic auto.expr
			case _ => false
		}

		override def equivalent(expression: Formula[_]): Boolean = expression match {
			case auto :AutoConversionFormula[_, _, _] if sameAs(auto) => expr equivalent auto.expr
			case _ => false
		}



		override def canEqual(that :Any) = that match {
			case auto :AutoConversionFormula[_, _, _] => auto.getClass == getClass
			case _ => false
		}

		override def equals(that :Any) = that match {
			case f :AutoConversionFormula[_, _, _] => (f eq this) || (f sameAs this) && expr == f.expr
			case _ => false
		}

		override def hashCode = expr.hashCode



		protected def name = Names.unqualifiedClassName(this)

		override def toString = s"$name($expr)"
		
		
	}

	object AutoConversionFormula {
		def apply[R<:RowSource, F, T](f :SQLFormula[R, F])(map :F=>T) :AutoConversionFormula[R, F, T] =
			new CustomConversionFormula(f, map)

		trait ConversionMatcher[+S<:RowSource, +Y[X]] extends SingleRowMatcher[S, Y] with MultipleRowsMatcher[S, Y]
		{
			def conversion[Z, X](f :AutoConversionFormula[S, Z, X]) :Y[X]
		}

		trait MatchConversion[+S<:RowSource, +Y[X]] extends ConversionMatcher[S, Y] with CaseRow[S, Y] with CaseRows[S, Y]

		trait CaseConversion[+S<:RowSource, +Y[X]] extends MatchConversion[S, Y]
		{
			def conversion[Z, X](f :AutoConversionFormula[S, Z, X]) :Y[X]

			def row[X](f :SelectAsRow[S, X]) :Y[X] = conversion(f)

			def rows[X](f :SelectAsRows[S, X]) :Y[Seq[X]] = conversion(f)

		}


		case class CustomConversionFormula[-R<:RowSource, F, T](expr :SQLFormula[R, F], map :F=>T)
			extends AutoConversionFormula[R, F, T]
		{
			override def lift(s: F): T = map(s)

			override def map[S <: RowSource](mapper: SQLRewriter[R, S]): SQLFormula[S, T] =
				new CustomConversionFormula(mapper(expr), map)

			override def name = "Custom"
		}
	}

	case class OrNull[-R<:RowSource, T](expr :SQLFormula[R, T]) extends AutoConversionFormula[R, T, Option[T]] {

		override def lift(s: T): Option[T] = Option(s)

		override def name = "Option"

		override def map[S <: RowSource](mapper: SQLRewriter[R, S]) = OrNull(mapper(expr))
	}



	trait TupleFormula[-R<:RowSource, T] extends CompositeFormula[R, T] {


		override def canEqual(that :Any) = that.isInstanceOf[TupleFormula[_, _]]

		override def isomorphic(expression: Formula[_]): Boolean = expression match {
			case t :TupleFormula[_, _] => contentsIsomorphic(t)
			case _ => false
		}

		override def equivalent(expression :Formula[_]) :Boolean = expression match {
			case t :TupleFormula[_, _] => contentsEquivalent(t)
			case _ => false
		}
	}

	object TupleFormula {
		def unapply[S <: RowSource](expr: SQLFormula[S, _]): Option[Seq[SQLFormula[S, _]]] = expr match {
			case t: TupleFormula[_, _] => Some(t.parts.asInstanceOf[Seq[SQLFormula[S, _]]])
			case _ => None
		}

		trait TupleMatcher[+S<:RowSource, +Y[X]] extends HListMatcher[S, Y] with SeqMatcher[S, Y]
//
		trait MatchTuple[+S<:RowSource, +Y[X]] extends CaseHList[S, Y] with CaseSeq[S, Y]


		trait CaseTuple[+S<:RowSource, +Y[X]] extends TupleMatcher[S, Y] with MatchTuple[S, Y] {
			def tuple[X](f: TupleFormula[S, X]): Y[X]

			def hlist[X <: HList](f: HListFormula[S, X]): Y[X] = tuple(f)

			def seq[X](f: SeqFormula[S, X]): Y[Seq[X]] = tuple(f)

		}

	}
	
	
	sealed trait HListFormula[-R<:RowSource, T<:HList] extends TupleFormula[R, T] {
		def size :Int

		def toSeq :Seq[SQLFormula[R, _]]

		override def parts = toSeq

		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[T] = matcher.hlist(this)

		override def map[S <: RowSource](mapper: SQLRewriter[R, S]): HListFormula[S, T]

		def ::[S<:R, H](head :SQLFormula[S, H]) = HListHead(head, this)


		override def toString = parts.mkString("(",", ",")")
	}

	object HListFormula {
		trait HListMatcher[+S<:RowSource, +Y[X]] {
			def hlist[X<:HList](f :HListFormula[S, X]) :Y[X]
		}

		trait MatchHList[+S<:RowSource, +Y[X]] extends HListMatcher[S, Y] {
			def hlistHead[H, T<:HList](head :SQLFormula[S, H], tail :HListFormula[S, T]) :Y[H::T]
			def hNil :Y[HNil]

			override def hlist[X <: HList](f: HListFormula[S, X]): Y[X] = (f match {
				case SQLHNil => hNil
				case HListHead(head, tail) => hlistHead(head, tail)
			}).asInstanceOf[Y[X]]
		}

		type CaseHList[+S<:RowSource, +Y[X]] = HListMatcher[S, Y]
	}

	case class HListHead[-R<:RowSource, H, T<:HList](head :SQLFormula[R, H], tail :HListFormula[R, T])
		extends HListFormula[R, H::T]
	{ expr =>
		def size = 1+tail.size
		def toSeq = head +: tail.toSeq

		override def readForm = new HListReadFormImpl[H, T](expr.head.readForm, expr.tail.readForm)


		override def groundValue = for (h<-head.groundValue; t<-tail.groundValue) yield h::t

		override def get(values: RowValues[R]) = for (h<-head.get(values); t<-tail.get(values)) yield h::t

		override def map[S <: RowSource](mapper: SQLRewriter[R, S]): HListFormula[S, H::T] =
			mapper(head)::tail.map(mapper)
	}

	case object SQLHNil extends TermFormula[HNil]()(EmptyForm(HNil)) with HListFormula[RowSource, HNil] {
		def size = 0
		def toSeq = Seq()


		override def groundValue = Some(HNil)

		override def map[S <: RowSource](mapper: SQLRewriter[RowSource, S])  = this
	}



	case class SeqFormula[-R<:RowSource, T](expressions :Seq[SQLFormula[R, T]]) extends TupleFormula[R, Seq[T]] {
		override def readForm = SQLReadForm.seq(expressions.map(_.readForm))

		override def groundValue = (Option(Seq[T]()) /: expressions) {
				case (acc, e) => for (seq<-acc; v<-e.groundValue) yield v+:seq
			}.map(_.reverse)


		override def parts = expressions


		override def get(values: RowValues[R]): Option[Seq[T]] =
			(Option(Seq[T]()) /: expressions) {
				case (acc, e) => for (seq<-acc; v<-e.get(values)) yield v+:seq
			}.map(_.reverse)



		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[Seq[T]] = matcher.seq(this)

		override def map[S <: RowSource](mapper: SQLRewriter[R, S]) =
			SeqFormula(parts.map(mapper(_)))

		override def toString = expressions.mkString("Seq(", ",", ")")


	}

	object SeqFormula {
		implicit def literalSeq[T :SQLForm](items :Seq[T]) :SeqFormula[RowSource, T] =
			new SeqFormula[RowSource, T](items.map(Literal(_)))

		implicit def expressionSeq[R<:RowSource, T](items :Seq[SQLFormula[R, T]]) :SeqFormula[R, T] =
			new SeqFormula[R, T](items)


		trait SeqMatcher[+S<:RowSource, +Y[X]] {
			def seq[X](f :SeqFormula[S, X]) :Y[Seq[X]]
		}

		type MatchSeq[+S<:RowSource, +Y[X]] = SeqMatcher[S, Y]

		type CaseSeq[+S<:RowSource, +Y[X]] = SeqMatcher[S, Y]
	}


	/** Representation of sql select as an sql formula used in the context of source S. If source is RowSource,
	  * this will be a GroundedSelectFormula instance - a select independent of any external tables or parameters,
	  * in which all formulas (select clause, where clause, etc) can be evaluated based on the values of tables in its from list.
	  * If S is not a RowSource, but contains tables, this is a subselect nested inside a select for source S, either
	  * in its header or from clause. The source for this formula, given by member type Source, is always an extension
	  * of S, and in fact Source <: SubsourceOf[S], where S is the actual source type given at instance creation -
	  * we cannot declare it so because of contravariance.
	  *
	  * Apart from being an sql formula, it is also a Mapping[T], so it can be used as other mappings inside a FROM clause.
	  * @tparam S
	  * @tparam T mapped header type representing a single row.
	  */
	trait SelectFormula[-S<:RowSource, T] extends SQLFormula[S, RowCursor[T]] with Mapping[T] {
		type Source <: RowSource

		trait Column[X] {
			def name :String
			def formula :SQLFormula[Source, X]
		}

		def header :SQLFormula[Source, T]
		def headerColumns :Seq[Column[_]]

		val source :Source

		def from :Seq[TableFormula[Source, _<:AnyMapping]] = source.subselectTables.asInstanceOf[Seq[TableFormula[Source, _<:AnyMapping]]]

		def filter :SQLFormula[Source, Boolean] = source.filteredBy.asInstanceOf[SQLFormula[Source, Boolean]]



		def exists :SQLFormula[S, Boolean] = ExistsFormula[S, T](this)

		def notExists :SQLFormula[S, Boolean] = ! ExistsFormula[S, T](this)


		override def readForm = header.readForm.map(RowCursor(_))

		override def get(values: RowValues[S]) :Option[RowCursor[T]] =
			header.get(values.asInstanceOf[RowValues[Source]]).map(RowCursor(_))

		override def isGroundedIn(tables: Iterable[TableFormula[_, _ <: AnyMapping]]): Boolean =
			header.isGroundedIn(tables)

		override def groundValue = header.groundValue.map(RowCursor(_))


		def single :SelectAsRow[S, T] = new SelectAsRow(this)

		def rows :SelectAsRows[S, T] = new SelectAsRows(this)



		override protected def reverseCollect[X](fun: PartialFunction[SQLFormula[_<:RowSource, _], X], acc: Seq[X]): Seq[X] =
			super.reverseCollect(fun, header.reverseCollect(fun, filter.reverseCollect(fun, acc)))

		override def isomorphic(expression: Formula[_]): Boolean = expression match {
			case s :SelectFormula[_, _] => (s eq this) || (s canEqual this) && (s.header isomorphic header) && (s.source == source)
			case _ => false
		}

		override def equivalent(expression: Formula[_]): Boolean = expression match {
			case s :SelectFormula[_, _] => (s eq this) || (s canEqual this) && (s.header equivalent header) && (s.source == source)
			case _ => false
		}


		override def canEqual(that :Any) = that.isInstanceOf[SelectFormula[_, _]]

		override def equals(that :Any) = that match {
			case s :SelectFormula[_, _] => (s eq this) || (s canEqual this) && s.header==header && s.source==source
			case _ => false
		}

		override def hashCode = (header, source).hashCode

		override def toString = s"select $header from $source"
	}


	object SelectFormula {

		def apply[R<:SelectSource, M<:AnyMapping](from :R, header :ComponentFormula[R, _<:AnyMapping, M]) :SelectMapping[R, M] =
			new SelectComponentFormula[RowSource, R, M](from, header) with SelectMapping[R, M]

		def apply[R<:SelectSource, H](from :R, header :SQLFormula[R, H]) :GroundedSelectFormula[H] =
			header.ifSubclass[ComponentFormula[R, AnyMapping, Mapping[H]]] {
				comp => apply(from, comp) :SelectMapping[R, Mapping[H]]
			} getOrElse
				new ArbitrarySelectFormula[RowSource, R, H](from, header) with GroundedSelectFormula[H]

		def subselect[P<:RowSource, S<:SubsourceOf[P], H](parent :P, from :S, header :SQLFormula[S, H]) :SubselectFormula[P, S, H] =
			subselect[P, S, H](from, header)

		def subselect[P<:RowSource, S<:SubsourceOf[P], H](from :S, header :SQLFormula[S, H]) :SubselectFormula[P, S, H] =
			header.ifSubclass[ComponentFormula[S, AnyMapping, Mapping[H]]] {
				comp => subselect[P, S, Mapping[H]](from, comp)
			} getOrElse
				new ArbitrarySelectFormula[P, S, H](from, header)

		def subselect[P<:RowSource, S<:SubsourceOf[P], H<:AnyMapping](from :S, header :ComponentFormula[S, _<:AnyMapping, H]) :SubselectMapping[P, S, H] =
			new SelectComponentFormula[P, S, H](from, header) with SubselectMapping[P, S, H]

		def subselect[P<:RowSource, S<:SubsourceOf[P], H<:AnyMapping](parent :P, from :S, header :ComponentFormula[S, _<:AnyMapping, H]) :SubselectMapping[P, S, H] =
			subselect[P, S, H](from, header)


		def exists[R<:RowSource, H](select :SelectFormula[R, H]) :BooleanFormula[R] = select.exists



		trait SelectAs[-S<:RowSource, M<:AnyMapping] extends SelectFormula[S, M#ResultType] with GenericMappingImpostor[M] {

			override def canEqual(that :Any) = that.isInstanceOf[SelectAs[_, _]]

			override def equals(that :Any) = super[SelectFormula].equals(that)

			override def hashCode = super[SelectFormula].hashCode

			override def toString = super[SelectFormula].toString
		}

		trait SubselectFormula[S<:RowSource, R <: SubsourceOf[S], H] extends SelectFormula[S, H] {
			type Source = R

			override def applyTo[Y[X]](matcher: FormulaMatcher[S, Y]): Y[RowCursor[H]] = matcher.subselect(this)
		}

		trait GroundedSelectFormula[H] extends SelectFormula[RowSource, H] {
			override def applyTo[Y[X]](matcher: FormulaMatcher[RowSource, Y]): Y[RowCursor[H]] = matcher.select(this)
		}




		trait SubselectMapping[S<:RowSource, R<:SubsourceOf[S], M<:AnyMapping] extends SelectAs[S, M] with SubselectFormula[S, R, M#ResultType]

		trait SelectMapping[S<:SelectSource, M<:AnyMapping] extends SubselectMapping[RowSource, S, M] with GroundedSelectFormula[M#ResultType]



		private class SelectComponentFormula[S<:RowSource, R<:SubsourceOf[S], M<:AnyMapping](val source :R, val header :ComponentFormula[R, _<:AnyMapping, M])
			extends SelectAs[S, M] with SubselectFormula[S, R, M#ResultType]
		{
			protected val adaptee: M = header.path.end


			override val headerColumns: Seq[Column[_]] = header.path.end.columns.map(column(_) :Column[_])

			private def column[T](column :M#Component[T]) :Column[T] = new Column[T] {
				override val name: String = header.path.lift(column).flatMap((_:AnyMapping).sqlName) getOrElse {
					throw new IllegalArgumentException(s"Can't create select $header from $source: column $column has no sqlName")
				}

				override val formula = header :\ column
			}
		}


		/** A select formula selecting an arbitrary expression 'header' based on the given row source. 
		  * This header will be translated by recursively flat mapping header expression to obtain a flat sequence of columns. 
		  * In particular, any sequences/tuples are inlined, and any ComponentFormulas referring to components of tables 
		  * or whole last rows themselves are replaced with their columns.
		  * Column list declared by this mapping is thus created by recursively applying the following rules to header formula:
		  * <verbatim>
		  *     1. If the formula is a component mapping, create a column for every lifted column of the declared mapping;
		  *     2. If the formula is a composite formula such as a tuple, recursively flatMap over it by applying these rules;
		  *     3. In other cases the formula is taken to represent a single column in the resulting select and an appropriate column is created.
		  * </verbatim>
		  * Note that above column list should be considered in the context of this instance as a mapping and represents all
		  * columns that potentially might be part of the select clause. Existance of non-selectable and optional columns means
		  * that resulting select query may not contain all of the above. This distinction is also present when using this
		  * instance to assemble results of created select; as individual columns in the select header may be any formulas,
		  * the source of values for evaluating the header formula are not values of the tables of the underlying source,
		  * but values for whole column formulas. For example, header formula in the form of 
		  * <code>(current_date - birth_date, address, (first_name, family_name)) from users</code>
		  * could translate into a select formula declaring columns :<code>('col1', street, zip, city, country, first_name, family_name)</code>
		  * Such columns would be available for any formulas using this mapping in their RowSource and are considered 'available header columns'.
		  * However, when using this instance as a mapping for assembling the header value, we don't have values for individual
		  * columns of users last, but values for the columns declared by this mapping. This means that we need a bit of creative term rewriting
		  * to assemble the scala value as would be evaluated by the original header formula. In particular, in the above example,
		  * address object would be reassembled based on the values of individual columns included in the final select.
		  * 
		  *
		  * @param source
		  * @param header
		  * @tparam S
		  * @tparam R
		  * @tparam H
		  */
		private class ArbitrarySelectFormula[S<:RowSource, R<:SubsourceOf[S], H](val source :R, val header :SQLFormula[R, H])
			extends SubselectFormula[S, R, H] with MappingSupport[H]
		{ select =>

			/** A column in the header of owning select.
			  * @param formula sql formula providing value for the column
			  * @param name column name (sqlName) and suggested alias for the column in the select clause
			  */
			case class Component[T](formula :SQLFormula[R, T], name :String) extends TypedColumn[T] with Column[T] { component =>
				override type Component[X] = select.Component[X]
				def self = this

				override val columnType = new ColumnType[T] {
					if (formula.readForm.readColumns!=1)
						throw new IllegalArgumentException(s"Can't select $header from $source: $formula is not a column (${formula.readForm})")

					override val sqlForm: SQLForm[T] = formula.readForm && SQLWriteForm.empty(1)

					override def Getter: GetResult[T] = GetResult[T](sqlForm(_))

					override def Setter: SetParameter[T] =
						SetParameter[T]((_, _) => throw new UnsupportedOperationException("ArbitrarySelectFormula.Component.columnType.Setter"))

					override def nullValue: T = sqlForm.nullValue
				}

				private val selectMorphism = new MappingMorphism[select.type, component.type] {
					override def source = select :select.type
					override def target = component :component.type
					override def components = ComponentMorphism.homomorphism[Component, select.Component](_ => component)
					override def value = ValueMorphism((h :H) => throw new UnsupportedOperationException(s"can't select value for $component from $h (part of select $select)"))
				}
				val selectPath = DirectComponent[select.type, this.type, T](selectMorphism)
			}

			/** A source which will be used to provide data for evaluating the header during this select mapping assembly process.
			  * When performing the mapping of rows returned by an arbitrary select, we don't have values for tables listed
			  * in its from clause, but values for column expressions in its header clause. Therefore when evaluating the header,
			  * we won't have values for tables in source, just ComponentValues instance for this mapping.
			  */
			private val sourceForAssembly = ParamSource[Values]


			private type Res[T] = SQLFormula[ParamSource[Values], T]

			private val headerRewriter = new SQLMapper[R, Res] with CaseFormula[R, Res] with CaseTuple[R, Res] {
				var columns :Seq[Component[_]] = Seq()

				override def component[T <: AnyMapping, C <: AnyMapping](f: ComponentFormula[R, T, C]): SQLFormula[ParamSource[Values], C#ResultType] = {
					val substitution = ComponentSubstitution(f)
					columns = substitution.columns.reverse ++: columns
					substitution.substitute
				}

				override def tuple[X](f: TupleFormula[R, X]): SQLFormula[ParamSource[Values], X] = f.map[ParamSource[Values]](this)

				override def formula[X](f: SQLFormula[R, X]): SQLFormula[ParamSource[Values], X] = {
					val column = Component(f, nameFor(f))
					columns = column +: columns
					//todo: defer this form, as we don't know at this point which columns will be actually included
					implicit val form = f.readForm && SQLWriteForm.empty(f.readForm.readColumns): SQLForm[X]
					sourceForAssembly.?[Values].opt { values => values.get(column) }
				}

				private def nameFor(f :SQLFormula[R, _]) :String = s"col_${columns.size}"
			}

			/** Header formula with formulas for individual columns in the select clause substituted by source parameter
			  * of type ComponentValues. It allows to evaluate the header formula using not the values for tables in its
			  * original from clause, but values for column formulas returned by executing the select passed as this mapping's
			  * ComponentValues. Final terms of this formula are 'source parameters' which return the value for a selected column
			  * from passed ComponentValues, either directly (single column) or by assembling a mapped value using mapping
			  * for substituted formula. They are composed to form equivalent header formula by tuple formulas, preserved
			  * as they were in original header formula grounded in source.
			  */
			private val headerForAssembly = headerRewriter(header)


			/** Substitution of a ComponentFormula referring to a last or last component/column into exploded list of its columns.
			  * @param component component formula occurring in header formula (select clause)
			  * @tparam T
			  * @tparam M
			  */
			private case class ComponentSubstitution[T<:AnyMapping, M<:AnyMapping](component :ComponentFormula[R, T, M]) {

				val (substitutions, columns) = {
					val mappedColumns =
						for (column <- component.path.lift(AllColumns)) yield {
							column -> Component(component.table :\ column, column.sqlName getOrElse {
								throw new IllegalArgumentException(s"Can't create select $header from $source: column $column has no name")
							}) :(T#Component[_], Component[_])
						}
					(mappedColumns.toMap[T#Component[_], Component[_]], mappedColumns.map(_._2))
				}

				//todo: defer this form, as we don't know at this point which columns will be actually included
				private implicit val form = component.readForm && SQLWriteForm.empty(component.readForm.readColumns): SQLForm[M#ResultType]

				/** A substitute formula replacing original component formula with a parameter mapping assembling its value
				  * from values of columns of the parent select formula/mapping.
 				  */
				val substitute = sourceForAssembly.?[Values].opt { values =>
					val substitutedValues = ComponentValues.generic(component.table.mapping)(column => substitutions.get(column).flatMap(values.get(_)))
					substitutedValues.get(component.path)
				} :SQLFormula[ParamSource[Values], M#ResultType]

			}

			val columns = headerRewriter.columns.reverse.indexed//columnsFor(header, Seq()).reverse.indexed
			def components = columns
			def subcomponents = columns
			def headerColumns = columns

			override implicit def \\[X](component: Component[X]): TypedComponentPath[this.type, component.type, X] =
				component.selectPath


			override def assemble(values: Values): Option[H] =
				headerForAssembly.get(RowValues(sourceForAssembly.last, values))



		}

		case class SelectAsRow[-R<:RowSource, H](select :SelectFormula[R, H]) extends AutoConversionFormula[R, RowCursor[H], H] {
			def expr = select

			override def lift(s: RowCursor[H]): H = s.head

			override def name = "SingleRow"


			override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[H] = matcher.row(this)

			override def map[S <: RowSource](mapper: SQLRewriter[R, S]): AutoConversionFormula[S, RowCursor[H], H] =
				mapper(select) match {
					case sel :SelectFormula[_, _] => SelectAsRow(sel.asInstanceOf[SelectFormula[S, H]])
					case f => AutoConversionFormula(f)(_.head)
				}
		}

		case class SelectAsRows[-R<:RowSource, H](select :SelectFormula[R, H]) extends AutoConversionFormula[R, RowCursor[H], Seq[H]] {
			def expr = select

			override def lift(s: RowCursor[H]): Seq[H] = s.seq

			override def name = "Rows"


			override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[Seq[H]] = matcher.rows(this)


			override def map[S <: RowSource](mapper: SQLRewriter[R, S]): AutoConversionFormula[S, RowCursor[H], Seq[H]] =
				mapper(select) match {
					case sel :SelectFormula[_, _] => SelectAsRows(sel.asInstanceOf[SelectFormula[S, H]])
					case f => AutoConversionFormula(f)(_.seq)
				}

		}
		
		trait GroundedSelectMatcher[+S<:RowSource, +Y[X]] {
			def select[X](f :GroundedSelectFormula[X]) :Y[RowCursor[X]]
		}

		trait SubselectMatcher[+S<:RowSource, +Y[X]] {
			def subselect[X](f :SelectFormula[S, X]) :Y[RowCursor[X]]
		}

		trait SelectMatcher[+S<:RowSource, +Y[X]] extends GroundedSelectMatcher[S, Y] with SubselectMatcher[S, Y]

		type MatchSelect[+S<:RowSource, +Y[X]] = SelectMatcher[S, Y]

		trait CaseSelect[+S<:RowSource, +Y[X]] extends MatchSelect[S, Y] {
			def select[X](f :SelectFormula[S, X]) :Y[RowCursor[X]]

			def subselect[X](f: SelectFormula[S, X]): Y[RowCursor[X]] = select(f)

			def select[X](f: GroundedSelectFormula[X]): Y[RowCursor[X]] = select(f :SelectFormula[S, X])
		}


		trait SingleRowMatcher[+S<:RowSource, +Y[X]] {
			def row[X](f :SelectAsRow[S, X]) :Y[X]
		}

		trait MultipleRowsMatcher[+S<:RowSource, +Y[X]] {
			def rows[X](f :SelectAsRows[S, X]) :Y[Seq[X]]
		}

		type MatchRow[+S<:RowSource, +Y[X]] = SingleRowMatcher[S, Y]

		type CaseRow[+S<:RowSource, +Y[X]] = SingleRowMatcher[S, Y]

		type MatchRows[+S<:RowSource, +Y[X]] = MultipleRowsMatcher[S, Y]

		type CaseRows[+S<:RowSource, +Y[X]] = MultipleRowsMatcher[S, Y]

	}
	

	case class ExistsFormula[-R<:RowSource, H](select :SelectFormula[R, H]) extends ConditionFormula[R] {
		def parts = Seq(select)

		override def readForm: SQLReadForm[Boolean] = SQLForm[Boolean]

		override def get(values: RowValues[R]): Option[Boolean] = None


		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[Boolean] = matcher.exists(this)

		override def map[S <: RowSource](mapper: SQLRewriter[R, S]): SQLFormula[S, Boolean] = mapper(select) match {
			case sel :SelectFormula[_, _] => ExistsFormula(sel.asInstanceOf[SelectFormula[S, H]])
			case f => throw new IllegalArgumentException(s"Can't rewrite $this using $mapper because argument is not a select: $f")
		}

		override def toString = s"Exists($select)"
	}

	object ExistsFormula {
		trait ExistsMatcher[+S<:RowSource, +Y[X]] {
			def exists[X](f :ExistsFormula[S, X]) :Y[Boolean]
		}

		type MatchExists[+S<:RowSource, +Y[X]] = ExistsMatcher[S, Y]

		type CaseExists[+S<:RowSource, +Y[X]] = ExistsMatcher[S, Y]
	}


	trait MappingFormula[-S<:RowSource, M<:AnyMapping] extends SQLFormula[S, M#ResultType] {
		def mapping :M

		override def readForm = mapping.selectForm


	}

	
	object MappingFormula {
		type MappingMatcher[+S<:RowSource, +Y[X]] = PathMatcher[S, Y]

		type MatchMapping[+S<:RowSource, +Y[X]] = MatchPath[S, Y]

		trait CaseMapping[+S<:RowSource, +Y[X]] extends CasePath[S, Y] {
			def mapping[M<:AnyMapping](f :MappingFormula[S, M]) :Y[M#ResultType]

			def path[T <: AnyMapping, C <: AnyMapping](f: PathFormula[S, T, C]): Y[C#ResultType] = mapping(f)
		}
	}

	/** A placeholder expression which value is a value of the end mapping specified by the given path. Note that this class shouldn't be
	  * used directly in sql filters, as the path can represent higher level concepts such as joins between tables. To refer
	  * to a component of a joined last, use ComponentFormula. This class exists to facilitate creating filters on a higher level
	  * of abstraction, which can be adapted for different queries, depending on a chosen loading strategy.
	  * @tparam R type of RowSource this instance is compatible with (i.e, which contains the source last)
	  * @tparam T mapping type of the source last.
	  * @tparam M target mapping type
	  */
	abstract class PathFormula[-R<:RowSource, T<:AnyMapping, M<:AnyMapping] private[SQLFormula] ()
		extends MappingFormula[R, M] with TupleFormula[R, M#ResultType]
	{
		/** Table in the source at which the path starts. */
		def table :TableFormula[R, T]
		
		/** Mapping path specifying the value to be inserted in the final sql in the place of this expression. */
		def path :MappingPath[T, M]

		override def mapping = path.end

		override def parts: Seq[SQLFormula[R, _]] =
			if (path.end.subcomponents.isEmpty) Seq()
			else path.end.querable.map {
				column => PathFormula(table, path :+ column)
			}


		override def isGroundedIn(tables: Iterable[TableFormula[_, _ <: AnyMapping]]): Boolean =
			path.isInstanceOf[ComponentPath[_,_]] && tables.toSet(table)

		override def get(values: RowValues[R]): Option[M#ResultType] = values.get(table).flatMap(path.pick)

		def queryLiteral(value :T#ResultType) :SQLFormula[RowSource, M#ResultType] =
			throw new UnsupportedOperationException(s"Can't translate abstract path expression $this into a literal (of value $value)")

		def queryParameter(value :T#ResultType) :SQLFormula[RowSource, M#ResultType] =
			throw new UnsupportedOperationException(s"Can't translate abstract path expression $this into a parameter (with value $value)")



		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[M#ResultType] = matcher.path(this)


		override def map[S <: RowSource](mapper: SQLRewriter[R, S]): SQLFormula[S, M#ResultType] = mapper(this)

		override def equals(other :Any) = other match {
			case p:PathFormula[_, _, _] => (this eq p) || (p.canEqual(this) && table==p.table && path==p.path)
			case _ => false
		}
		
		override def canEqual(other :Any) = other.isInstanceOf[PathFormula[_, _, _]]
		
		override def hashCode = (table, path).hashCode
		

		override def isomorphic(expression: Formula[_]): Boolean = expression match {
			case e:PathFormula[_,_,_] =>
				(this eq e) || canEqual(e) && e.canEqual(this) && (table sameAs e.table) && path==e.path
			case _ => false
		}

		override def equivalent(expression: Formula[_]): Boolean = expression match {
			case e:PathFormula[_,_,_] =>
				(this eq e) || canEqual(e) && e.canEqual(this) && (table sameAs e.table) &&
					(path.end==e.path.end)
			case _ => false
		}

		override def toString = s"$table${path.tailString}"
	}

	/** Factory for path expressions which will always produce a ComponentExpression if passed path is a ComponentPath. */
	object PathFormula {

		/** Create an abstract expression denoting a value of the end mapping of a path starting at one of member tables in the row source.
		  * If passed path is a ComponentPath, a ComponentExpression will be produced which translates to a single column
		  * or a tuple of columns from the argument last, depending on the mapping result type. Other MappingPath instances
		  * will result in generic placeholders which can't be  directly translated into sql and should be replaced at some
		  * later stage with a proper expression.
		  *
 		  * @param table source mapping (not necessarly a last - might be a component) from which the path starts
		  * @param path a path pointing to a mapping which value is will be the value of this expression.
		  */
		def apply[S<:RowSource, T<:AnyMapping, M<:AnyMapping](table :TableFormula[S, T], path :MappingPath[T, M]) :PathFormula[S, T, M] =
			path match {
				case _  if path.start!=table.mapping =>
					throw new IllegalArgumentException(s"PathFormula($table, $path): path doesn't start at last mapping")
				case SelfPath(_) => 
					table.asInstanceOf[PathFormula[S, T, M]]
				case ComponentPath(component) =>
					new SubcomponentFormula(table, component)
				case _ =>
					new ForeignPathFormula(table, path)
			}
			


		def unapply[R<:RowSource, X](expression :SQLFormula[R, X]) :Option[(TableFormula[R, T], MappingPath[T, C]) forSome { type T<:AnyMapping; type C<:AnyMapping }] =
			expression.ifSubclassOf[PathFormula[R, AnyMapping, AnyMapping]](e => (e.table, e.path))


		trait PathMatcher[+S<:RowSource, +Y[X]] extends ComponentMatcher[S, Y] {
			def path[T<:AnyMapping, C<:AnyMapping](f :PathFormula[S, T, C]) :Y[C#ResultType]
		}

		trait MatchPath[+S<:RowSource, +Y[X]] extends PathMatcher[S, Y] with CaseComponent[S, Y]


		trait CasePath[+S<:RowSource, +Y[X]] extends MatchPath[S, Y] {
			def component[T<:AnyMapping, C<:AnyMapping](f :ComponentFormula[S, T, C]) :Y[C#ResultType] = path(f)
		}


		private class ForeignPathFormula[-S<:RowSource, T<:AnyMapping, M<:AnyMapping](val table :TableFormula[S, T], val path :MappingPath[T, M])
			extends PathFormula[S, T, M]

		private[SQLFormula] class SubcomponentFormula[-R<:RowSource, T<:AnyMapping, C<:AnyMapping](val table :TableFormula[R, T], val path :ComponentPath[T, C])
			extends ComponentFormula[R, T, C]

	}

	
	abstract class ComponentFormula[-R<:RowSource, T<:AnyMapping, C<:AnyMapping] private[sql] ()
		extends PathFormula[R, T, C]
	{
		override def path  :ComponentPath[T, C]
		
		override def readForm = path.lift.map(_.selectForm) getOrElse path.end.selectForm

		def sqlForm = readForm && (path.lift.map(_.queryForm) getOrElse path.end.queryForm).asInstanceOf[SQLWriteForm[C#ResultType]]


		/** Create an SQL formula for the given component of this mapping. If the component is not a single column, it will be
		  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		  * @param subcomponent function returning a component of the mapping associated with this last.
		  * @return an sql expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def apply[X<:C#Component[_]](subcomponent :C=>X) :ComponentFormula[R, T, X] =
			ComponentFormula(table, path(subcomponent))

		/** Create an SQL formula for the given component of  this mapping. If the component is not a single column, it will be
		  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		  *
		  * This is equivalent to apply(subcomponent), but may be needed to resolve ambiguity of chained calls to apply methods
		  * by implicit application.
		  * @param subcomponent function returning a component of the mapping associated with this last.
		  * @return an sql expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def \ [X<:C#Component[_]](subcomponent :C=>X) :ComponentFormula[R, T, X] = apply(subcomponent)


		/** Create an SQL formula for the given component of this mapping. If the subcomponent is not a single column, it will be
		  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		  * @param subcomponent a component of the mapping associated with this last.
		  * @return an sql expression which can be used to create search filters and specify columns in the SELECT header.
		  */		
		def :\ [X](subcomponent :C#Component[X]) :ComponentFormula[R, T, subcomponent.type] =
			ComponentFormula(table, path :\ subcomponent)

		/** Create an SQL formula for some subcomponent of this mapping, including the whole mapping itself in the case
		  * of SelfPath. If the component is not a single column, it will be
		  * treated as a tuple/sequence of columns and produce a literal in a form of (col1, col2, col3) in the resulting SQL.
		  * @param path path to the component the value of which we are interested in.
		  * @return an sql expression which can be used to create search filters and specify columns in the SELECT header.
		  */
		def \\[S<:AnyMapping](path :ComponentPath[C, S]) :ComponentFormula[R, T, S] =
			ComponentFormula[R, T, S](table, this.path ++ path)


		def :=[S<:RowSource, O, X](expr :SQLFormula[S, O])(implicit lift :FormulaEqualizer[C#ResultType, O, X]) :SetComponent[S, T, C, O, X] =
			SetComponent(path, expr)

		override def queryLiteral(value :T#ResultType) :SQLFormula[RowSource, C#ResultType] =
			path.pick(value).map(v => Literal(v)(sqlForm)) getOrElse Null[C#ResultType](sqlForm)

		override def queryParameter(value :T#ResultType) :SQLFormula[RowSource, C#ResultType] =
			path.pick(value).map(v => BoundParameter(v)(sqlForm)) getOrElse Null[C#ResultType](sqlForm)



		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[C#ResultType] = matcher.component(this)


		override def isomorphic(expression: Formula[_]): Boolean = expression match {
			case e:ComponentFormula[_, _, _] =>
				(e.table sameAs table) && (e.path==path || e.path.lift==path.lift && path.lift.isDefined)
			case _ => false
		}

		override def equivalent(expression: Formula[_]): Boolean = isomorphic(expression)

//		override def toString = s"$last$path"
	}

	object ComponentFormula {
		import PathFormula._
		
		def apply[S<:RowSource, T<:AnyMapping, M<:AnyMapping](table :TableFormula[S, T], path :ComponentPath[T, M]) :ComponentFormula[S, T, M] =
			path match {
				case _  if path.start!=table.mapping =>
					throw new IllegalArgumentException(s"ComponentFormula($table, $path): path doesn't start at last mapping")
				case SelfPath(_) =>
					table.asInstanceOf[ComponentFormula[S, T, M]]
				case _ =>
					new SubcomponentFormula(table, path)
			}
		
		def unapply[R<:RowSource, X](expression :SQLFormula[R, X]) :Option[(TableFormula[R, T], ComponentPath[T, C])] forSome { type T<:AnyMapping; type C<:AnyMapping } =
			expression.ifSubclassOf[ComponentFormula[R, AnyMapping, AnyMapping]](e => (e.table, e.path))


		trait ComponentMatcher[+S<:RowSource, +Y[X]] {
			def component[T <: AnyMapping, C <: AnyMapping](f: ComponentFormula[S, T, C]): Y[C#ResultType]
		}

		type MatchComponent[+S<:RowSource, +Y[X]] = ComponentMatcher[S, Y]

		type CaseComponent[+S<:RowSource, +Y[X]] = ComponentMatcher[S, Y]
	}


	trait ComparisonFormula[-S<:RowSource, T] extends ConditionFormula[S] {
		val left :SQLFormula[S, T]
		val right :SQLFormula[S, T]
		def symbol :String

	}
	

	object ComparisonFormula {
		type ComparisonMatcher[+S<:RowSource, +Y[X]] = EqualityMatcher[S, Y]


		type MatchComparison[+S<:RowSource, +Y[X]] = CaseEquality[S, Y]


		trait CaseComparison[+S<:RowSource, +Y[X]] extends MatchComparison[S, Y] {
			def comparison[X](f :ComparisonFormula[S, X]) :Y[Boolean]
			
			def equality[X](f :Equality[S, X]) :Y[Boolean] = comparison(f)

		}
	}

	case class Equality[-R<:RowSource, T](left :SQLFormula[R, T], right :SQLFormula[R, T])
		extends ComparisonFormula[R, T]
	{
		override def readForm = SQLForm[Boolean]

		override def groundValue = for (l<-left.groundValue; r<-right.groundValue) yield l==r

		override def get(values :RowValues[R]) = for (l<-left.get(values); r<-right.get(values)) yield l==r

		override def parts = Seq(left, right)

		def symbol = "="



		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[Boolean] = matcher.equality(this)


		override def map[S <: RowSource](mapper: SQLRewriter[R, S]) = Equality(mapper(left), mapper(right))

		override def toString = s"$left == $right"
	}
	
	object Equality {
		trait EqualityMatcher[+S<:RowSource, +Y[X]] {
			def equality[X](f :Equality[S, X]) :Y[Boolean]
		}

		type MatchEquality[+S<:RowSource, +Y[X]] = EqualityMatcher[S, Y]

		type CaseEquality[+S<:RowSource, +Y[X]] = EqualityMatcher[S, Y]
	}



	trait ConditionFormula[-S<:RowSource] extends CompositeFormula[S, Boolean]

	object ConditionFormula {
		trait ConditionMatcher[+S<:RowSource, +Y[X]]
			extends InMatcher[S, Y] with ExistsMatcher[S, Y] with ComparisonMatcher[S, Y]

		trait MatchCondition[+S<:RowSource, +Y[X]]
			extends CaseIn[S, Y] with CaseExists[S, Y] with CaseComparison[S, Y]

		trait CaseCondition[+S<:RowSource, +Y[X]] extends ConditionMatcher[S, Y] with MatchCondition[S, Y] {
			def condition(f :ConditionFormula[S]) :Y[Boolean]

			def in[X](f :In[S, X]) :Y[Boolean] = condition(f)

			def exists[X](f :ExistsFormula[S, X]) :Y[Boolean] = condition(f)

			def comparison[X](f: ComparisonFormula[S, X]): Y[Boolean] = condition(f)

		}
	}

	case class In[-R<:RowSource, T](left :SQLFormula[R, T], right :SQLFormula[R, Seq[T]])
		extends ConditionFormula[R]
	{
		override def readForm = SQLForm[Boolean]

		override def get(values: RowValues[R]) = for (l<-left.get(values); r<-right.get(values)) yield r.contains(l)

		override def groundValue = for (l<-left.groundValue; r<-right.groundValue) yield r.contains(l)

		def parts = Seq(left, right)


		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[Boolean] = matcher.in(this)

		override def map[S <: RowSource](mapper: SQLRewriter[R, S]): SQLFormula[S, Boolean] =
			In(mapper(left), mapper(right))

		override def equivalent(expression: Formula[_]): Boolean = expression match {
			case l In r => (l equivalent left) && (r equivalent right)
			case _ => false
		}


		override def isomorphic(expression: Formula[_]): Boolean = expression match {
			case l In r => (l isomorphic left) && (r isomorphic right)
			case _ => false
		}

		override def toString = s"$left in $right"
	}

	object In {
		trait InMatcher[+S<:RowSource, +Y[X]] {
			def in[X](f :In[S, X]) :Y[Boolean]
		}

		type MatchIn[+S<:RowSource, +Y[X]] = InMatcher[S, Y]

		type CaseIn[+S<:RowSource, +Y[X]] = InMatcher[S, Y]
	}


	trait LogicalFormula[-R<:RowSource] extends CompositeFormula[R, Boolean] {
		override def readForm = SQLForm[Boolean]
	}
	
	object LogicalFormula {
		trait LogicalMatcher[+S<:RowSource, +Y[X]] extends NotMatcher[S, Y] with AndMatcher[S, Y] with OrMatcher[S, Y]

		trait MatchLogical[+S<:RowSource, +Y[X]] extends CaseNot[S, Y] with CaseAnd[S, Y] with CaseOr[S, Y]

		trait CaseLogical[+S<:RowSource, +Y[X]] extends LogicalMatcher[S, Y] with MatchLogical[S, Y]
		{
			def logical(f :LogicalFormula[S]) :Y[Boolean]

			def not(f :NotFormula[S]) :Y[Boolean] = logical(f)

			def and(f :And[S]) :Y[Boolean] = logical(f)

			def or(f :Or[S]) :Y[Boolean] = logical(f)

		}
	}

	case class NotFormula[-R<:RowSource](formula :SQLFormula[R, Boolean]) extends LogicalFormula[R] {

		override def get(values: RowValues[R]): Option[Boolean] = formula.get(values).map(!_)

		override def groundValue: Option[Boolean] = formula.groundValue.map(!_)

		override def parts: Seq[SQLFormula[R, _]] = Seq(formula)

		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[Boolean] = matcher.not(this)


		override def map[S <: RowSource](mapper: SQLRewriter[R, S]) = NotFormula(mapper(formula))
	}

	object NotFormula {
		trait NotMatcher[+S<:RowSource, +Y[X]] {
			def not(f :NotFormula[S]) :Y[Boolean]
		}

		type MatchNot[+S<:RowSource, +Y[X]] = NotMatcher[S, Y]

		type CaseNot[+S<:RowSource, +Y[X]] = NotMatcher[S, Y]

	}

	case class And[-R<:RowSource](conditions :Seq[SQLFormula[R, Boolean]]) extends LogicalFormula[R]
	{


		override def get(values: RowValues[R]): Option[Boolean] = (Option(true) /: conditions) {
			case (acc, e) => for (v1<-acc; v2<-e.get(values)) yield v1 && v2
		}

		override def groundValue = (Option(true) /: conditions) {
			case (acc, e) => for (v1<-acc; v2<-e.groundValue) yield v1 && v2
		}

		override def and [S<:R](other: SQLFormula[S, Boolean])(implicit ev: this.type <:< SQLFormula[S, Boolean]): And[S] = other match {
			case And(others) => And(conditions ++: others)
			case _ => And(conditions :+ other)
		}

		override def parts = conditions


		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[Boolean] = matcher.and(this)

		override def map[S <: RowSource](mapper: SQLRewriter[R, S]) = And(conditions.map(mapper(_)))


		override def toString = conditions.mkString("(", " and ", ")")
	}

	object And {
		trait AndMatcher[+S<:RowSource, +Y[X]] {
			def and(f :And[S]) :Y[Boolean]
		}

		type MatchAnd[+S<:RowSource, +Y[X]] = AndMatcher[S, Y]

		type CaseAnd[+S<:RowSource, +Y[X]] = AndMatcher[S, Y]
	}


	case class Or[-R<:RowSource](conditions :Seq[SQLFormula[R, Boolean]]) extends LogicalFormula[R]
	{

		override def get(values: RowValues[R]): Option[Boolean] = (Option(false) /: conditions) {
			case (acc, e) => for (v1<-acc; v2<-e.get(values)) yield v1 || v2
		}

		override def groundValue = (Option(false) /: conditions) {
			case (acc, e) => for (v1<-acc; v2<-e.groundValue) yield v1 || v2
		}


		override def or [S<:R](other: SQLFormula[S, Boolean])(implicit ev: this.type <:< SQLFormula[S, Boolean]): Or[S] = other match {
			case Or(others) => Or(conditions ++: others)
			case _ => Or(conditions :+ other)
		}

		override def parts = conditions

		override def applyTo[Y[X]](matcher: FormulaMatcher[R, Y]): Y[Boolean] = matcher.or(this)

		override def map[S <: RowSource](mapper: SQLRewriter[R, S]) = Or(conditions.map(mapper(_)))

		override def toString = conditions.mkString("(", ") or (", ")")
	}


	object Or {
		trait OrMatcher[+S<:RowSource, +Y[X]] {
			def or(f :Or[S]) :Y[Boolean]
		}

		type MatchOr[+S<:RowSource, +Y[X]] = OrMatcher[S, Y]

		type CaseOr[+S<:RowSource, +Y[X]] = OrMatcher[S, Y]
	}

}

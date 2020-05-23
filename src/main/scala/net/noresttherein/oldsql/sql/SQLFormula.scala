package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnReadForm, Mapping, SQLForm, SQLReadForm, TypedMapping}
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.SQLFormula.CompositeFormula.{CaseComposite, CompositeMatcher}
import net.noresttherein.oldsql.sql.AutoConversionFormula.{CaseConversion, ColumnPromotionConversion, ConversionMatcher, OrNull, PromotionConversion}
import net.noresttherein.oldsql.sql.SQLCondition.{CaseCondition, Comparison, ConditionMatcher, Equality, Inequality}
import net.noresttherein.oldsql.sql.LogicalFormula.{AND, CaseLogical, LogicalMatcher, NOT, OR}
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, Formula, FormulaMatcher, SQLTypePromotion}
import net.noresttherein.oldsql.sql.SQLFormula.SQLTypePromotion.Lift
import net.noresttherein.oldsql.sql.SQLTerm.{BoundParameter, CaseTerm, False, SQLLiteral, TermMatcher, True}
import net.noresttherein.oldsql.sql.SQLTuple.{CaseTuple, TupleMatcher}
import net.noresttherein.oldsql.sql.AutoConversionFormula.ColumnPromotionConversion.{CaseColumnPromotion, ColumnPromotionMatcher}
import net.noresttherein.oldsql.sql.MappingFormula.{CaseMapping, ColumnComponentFormula, FreeColumn, MappingColumnMatcher, MappingMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.{AliasedColumn, ColumnFormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.CompositeColumnFormula.{CaseCompositeColumn, CompositeColumnMatcher, MatchCompositeColumn}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.{CaseColumnTerm, ColumnTermMatcher}
import net.noresttherein.oldsql.sql.MappingFormula.ColumnComponentFormula.CaseColumnComponent
import net.noresttherein.oldsql.sql.MappingFormula.FreeColumn.CaseFreeColumn
import net.noresttherein.oldsql.sql.SelectFormula.{CaseSelect, CaseSelectColumn, SelectColumnMatcher, SelectMatcher, SubselectFormula}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.AliasedColumn.{AliasedColumnMatcher, CaseAliasedColumn}
import slang._





/** A representation of an SQL expression as an AST.
  * @tparam F row source - list of tables which provide columns used in this expression
  * @tparam V result type of the expression; may not necessarily be an SQL type, but a result type of some mapping.
  */
trait SQLFormula[-F <: FromClause, V] { //todo: add a type parameter which is Bound || Unbound (flag if it contains any abstract/unbound parts)
	import SQLTerm.TermFormulas

	def readForm :SQLReadForm[V]

//	def =:[T <: RefinedMapping[O, E], C <: RefinedMapping[O, L], O, E, L, R >: V, X]
//	      (path :ComponentPath[T, C, O, E, L])(implicit lift :SQLTypePromotion[L, R, X]) :SetComponent[F, T, C, O, E, L, R, X] =
//		SetComponent(path, this :SQLFormula[F, R])

	def ==?[O, X](value :O)(implicit lift :SQLTypePromotion[V, O, X], form :SQLForm[O]) :BooleanFormula[F] =
		this === value.?

	def ===[S <: F, O, X](that :SQLFormula[S, O])(implicit lift :SQLTypePromotion[V, O, X]) :BooleanFormula[S] =
		Equality(lift.left(this), lift.right(that))

	def <>[S <: F, O, X](that :SQLFormula[S, O])(implicit lift :SQLTypePromotion[V, O, X]) :BooleanFormula[S] =
		Inequality(lift.left(this), lift.right(that))

	def <=[S <: F, O, X](that :SQLFormula[S, O])
	                    (implicit lift :SQLTypePromotion[V, O, X], ordering :SQLOrdering[X]) :BooleanFormula[S] =
		Comparison(lift.left(this), Comparison.LTE, lift.right(that))

	def <[S <: F, O, X](that :SQLFormula[S, O])
	                   (implicit lift :SQLTypePromotion[V, O, X], ordering :SQLOrdering[X]) :BooleanFormula[S] =
		Comparison(lift.left(this), Comparison.LT, lift.right(that))

	def >=[S <: F, O, X](that :SQLFormula[S, O])
	                    (implicit lift :SQLTypePromotion[V, O, X], ordering :SQLOrdering[X]) :BooleanFormula[S] =
		Comparison(lift.left(this), Comparison.GTE, lift.right(that))

	def >[S <: F, O, X](that :SQLFormula[S, O])
	                   (implicit lift :SQLTypePromotion[V, O, X], ordering :SQLOrdering[X]) :BooleanFormula[S] =
		Comparison(lift.left(this), Comparison.GT, lift.right(that))

//	def in [S <: F, U >: V, O, X](that :SeqTuple[S, O])(implicit lift :SQLTypePromotion[U, O, X]) :BooleanFormula[S] =
//		new In[S, X](lift.left(this), SeqTuple(that.parts.map(lift.right.apply[S])))



	def and[S <: F](other :BooleanFormula[S])(implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
		AND(ev(this)) and other

	def or [S <: F](other :BooleanFormula[S])(implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
		OR(ev(this)) or other

	def && [S <: F](other :BooleanFormula[S])(implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
		other match {
			case True() => ev(this)
			case False() => other
			case _ => this and other
		}

	def || [S <: F](other :BooleanFormula[S])(implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] =
		other match {
			case True() => other
			case False() => ev(this)
			case _ => this or other
		}

	def unary_![S <: F](implicit ev :this.type <:< BooleanFormula[S]) :BooleanFormula[S] = NOT(ev(this))


	/** Lifts this expression to one of type `X`, without any effect on the actual generated sql. */
	def to[X](implicit lift :Lift[V, X]) :SQLFormula[F, X] = PromotionConversion(this, lift)

	/** Lift this expression to one typed `Option[V]`, without any effect on the actual generated sql. */
	def opt :SQLFormula[F, Option[V]] = to[Option[V]] //OrNull(this)


	def subselectFrom(from :F) :SQLFormula[from.Outer, Rows[V]] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.unqualifiedClassName} can't be used as a Select header."
		)

	/** Upcasts this formula to the base ''from'' clause `E &lt;: F`, using only implicit evidence about the subtype
	  * relation rather than explicit lower type bound (which would be an identity cast in Scala).
	  */
	def basedOn[E <: FromClause](implicit subtype :E <:< F) :SQLFormula[E, V] = this.asInstanceOf[SQLFormula[E, V]]

//	/** Treat this expression as an expression of a FROM clause extending the source `F` this expression is based on
//	  * with an additional relation mapping `T`. */
//	def stretch[T[O] <: MappingFrom[O]] :SQLFormula[F With T, V] = stretch[F, F With T]
//
//	/** Treat this expression as an expression of a FROM clause extending (i.e. containing additional tables)
//	  * the clause `F` this expression is based on. */
//	def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :SQLFormula[S, V]

	/** Treat this expression as an expression of a FROM clause extending (i.e. containing additional tables)
	  * the clause `F` this expression is based on. */
	def stretch[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :SQLFormula[S, V] //= stretch[U, S]



	def parameters :Seq[BoundParameter[_]] = collect { case x :BoundParameter[_] => x }



	def collect[X](fun :PartialFunction[Formula[_], X]) :Seq[X] = reverseCollect(fun, Nil).reverse

	protected def reverseCollect[X](fun :PartialFunction[Formula[_], X], acc :List[X]) :List[X] =
		fun.lift(this) ++: acc

	protected[this] def reverseCollect[X](e :Formula[_])(fun :PartialFunction[Formula[_], X], acc :List[X]) :List[X] =
		e.reverseCollect(fun, acc)


	def applyTo[Y[_]](matcher :FormulaMatcher[F, Y]) :Y[V]



	/** If this expression is a free formula - not dependent on `F` or free parameters -
	  * which value can be determined at this point, return its value. */
	def freeValue :Option[V] = None


	/** Is this expression independent of any relations from the FROM clause? */
	def isFree :Boolean = freeValue.isDefined

	/** An SQL formula is said to be ''grounded in `JoinedRelation`s F1,...,FN'' if it can be evaluated based on the
	  * collective column set represented by mappings of sources of those formulas. In other words, it depends
	  * only on those parts of the ''from'' clause.
	  */
//	def isGroundedIn(tables :Iterable[AnyJoinedRelation]) :Boolean

/*
	def evaluate(values :RowValues[F]) :V = get(values) getOrElse {
		throw new IllegalArgumentException(s"Couldn't evaluate $this from $values")
	}

	def get(values :RowValues[F]) :Option[V]
*/



	/** Tests if this expression is equal to the given one abstracting from possibly different sources.
	  * Basically, if both expressions would produce the same SQL they should be isomorphic.
	  */
	def isomorphic(expression :Formula[_]) :Boolean

	/** Tests if this expression would produce the same value as the given expression, abstracting from possibly
	  * different clauses. Similar to isomorphic, but generally disregards order of elements in composite expressions
	  * such as 'and', 'or', seq. Be warned that this method's primary use is for tests, and production code shouldn't
	  * depend on it.
	  */
	private[oldsql] def equivalent(expression :Formula[_]) :Boolean = isomorphic(expression)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLFormula[_, _]]

}









object SQLFormula {

	implicit def implicitTerm[T :SQLForm](value :T) :SQLTerm[T] = SQLLiteral(value)

/*
	implicit def typingCast[J[M[O] <: MappingFrom[O]] <: _ Join M, R[O] <: MappingFrom[O], T[O] <: TypedMapping[_, O], X]
	                       (e :SQLFormula[J[R], X])
	                       (implicit cast :JoinedRelationSubject[J, R, T, TypedMapping.AnyFrom]) :SQLFormula[J[T], X] =
		cast(e)

	implicit def typingColumnCast[J[M[O] <: MappingFrom[O]] <: _ Join M, R[O] <: MappingFrom[O], T[O] <: TypedMapping[_, O], X]
	                             (e :ColumnFormula[J[R], X])
	                             (implicit cast :JoinedRelationSubject[J, R, T, TypedMapping.AnyFrom])
			:ColumnFormula[J[T], X] =
		cast(e)
*/





	type * = SQLFormula[_ <: FromClause, _]
	//todo: replace usages with *
	/** An upper type bound of all `SQLFormula[_, T]` instances. */
	type Formula[T] = SQLFormula[Nothing, T]

	/** A type alias for SQL expressions independent of any relations in the FROM clause, that is applicable
	  * to any `FromClause`. */
	type FreeFormula[T] = SQLFormula[FromClause, T]

	/** Any SQL expression of `Boolean` value. */
	type BooleanFormula[-F <: FromClause] = ColumnFormula[F, Boolean]





	/** An `SQLFormula` which represents an SQL expression of a single, atomic value assignable to a column,
	  * rather than a tuple.
	  */
	trait ColumnFormula[-F <: FromClause, V] extends SQLFormula[F, V] {
		override def readForm :ColumnReadForm[V]

		override def to[X](implicit lift :Lift[V, X]) :ColumnFormula[F, X] =
			ColumnPromotionConversion(this, lift)

		override def opt :ColumnFormula[F, Option[V]] = OrNull(this)


		override def basedOn[E <: FromClause](implicit subtype :E <:< F) :ColumnFormula[E, V] =
			this.asInstanceOf[ColumnFormula[E, V]]

		override def stretch[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :ColumnFormula[S, V]

		override def subselectFrom(from :F) :ColumnFormula[from.Outer, Rows[V]] =
			SelectFormula.subselect[from.Outer, from.type, V, Any](from, this)



		def as(alias :String) :ColumnFormula[F, V] = new AliasedColumn(this, alias)



		override def applyTo[Y[_]](matcher :FormulaMatcher[F, Y]) :Y[V] =
			applyTo(matcher :ColumnFormulaMatcher[F, Y])

		def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[V]
	}



	object ColumnFormula {

		class AliasedColumn[-F <: FromClause, V](val column :ColumnFormula[F, V], val alias :String)
			extends CompositeColumnFormula[F, V]
		{
			protected override val parts = column::Nil
			
			override def readForm :ColumnReadForm[V] = column.readForm
			
			override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnFormula[S, V] =
				new AliasedColumn(mapper(column), alias)
			
			override def applyTo[Y[X]](matcher :ColumnFormulaMatcher[F, Y]) :Y[V] = matcher.alias(this)

		}



		object AliasedColumn {
			trait AliasedColumnMatcher[+F <: FromClause, +Y[X]] {
				def alias[V](e :AliasedColumn[F, V]) :Y[V]
			}

			type MatchAliasedColumn[+F <: FromClause, +Y[X]] = AliasedColumnMatcher[F, Y]

			type CaseAliasedColumn[+F <: FromClause, +Y[X]] = AliasedColumnMatcher[F, Y]
		}



		trait ColumnFormulaMatcher[+F <: FromClause, +Y[X]]
			extends ColumnTermMatcher[F, Y] with CompositeColumnMatcher[F, Y] with MappingColumnMatcher[F, Y]
			with SelectColumnMatcher[F, Y]

		trait MatchColumnFormula[+F <: FromClause, +Y[X]] extends ColumnFormulaMatcher[F, Y]
			with CaseColumnTerm[F, Y] with CaseCompositeColumn[F, Y]
			with CaseColumnComponent[F, Y] with CaseFreeColumn[F, Y] with CaseSelectColumn[F, Y]

		trait CaseColumnFormula[+F <: FromClause, +Y[X]] extends MatchColumnFormula[F, Y] {
			def column[X](e :ColumnFormula[F, X]) :Y[X]

			override def composite[X](e :CompositeColumnFormula[F, X]) :Y[X] = column(e)

			override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
			                      (e :ColumnComponentFormula[F, T, E, M, V, O]) :Y[V] =
				column(e)

			override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
			                          (e :FreeColumn[O, M, V]) :Y[V] =
				column(e)

			override def select[V, O](e :SelectFormula.SelectColumn[F, V, O]) :Y[Rows[V]] = column(e)

			override def term[X](e :SQLTerm.ColumnTerm[X]) :Y[X] = column(e)

		}
	}





	trait CompositeFormula[-F <: FromClause, V] extends SQLFormula[F, V] {
		def inOrder :Seq[SQLFormula[F, _]] = parts
		protected def parts :Seq[SQLFormula[F, _]]


//		override def isGroundedIn(tables: Iterable[AnyJoinedRelation]): Boolean =
//			parts.forall(_.isGroundedIn(tables))

		protected override def reverseCollect[X](fun: PartialFunction[Formula[_], X], acc: List[X]): List[X] =
			(super.reverseCollect(fun, acc) /: inOrder)((collected, member) => member.reverseCollect(fun, collected))



		def map[S <: FromClause](mapper :SQLScribe[F, S]) :SQLFormula[S, V]


		override def stretch[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :SQLFormula[S, V] =
			map(SQLScribe.stretcher(target))



		def sameAs(other :CompositeFormula[Nothing, _]) :Boolean = canEqual(other)

		private[oldsql] override def equivalent(expression: Formula[_]): Boolean = expression match {
			case e:CompositeFormula[_, _] =>
				(e eq this) || e.sameAs(this) && this.sameAs(e) && contentsEquivalent(e)
			case _ => false
		}

		override def isomorphic(expression :Formula[_]) :Boolean = expression match {
			case e:CompositeFormula[_, _] =>
				(this eq e) || sameAs(e) && e.sameAs(this) && contentsIsomorphic(e)
			case _ => false
		}


		private[oldsql] def contentsEquivalent(other :CompositeFormula[_ <: FromClause, _]) :Boolean =
			parts.size == other.parts.size &&
				parts.forall(e => other.parts.exists(_ equivalent e)) &&
				other.parts.forall(e => parts.exists(_ equivalent e))

		def contentsIsomorphic(other :CompositeFormula[_ <: FromClause, _]) :Boolean =
			parts.size == other.parts.size &&
				((parts zip other.parts) forall { case (left, right) => left isomorphic right })



		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case composite :CompositeFormula[_, _] if canEqual(composite) && composite.canEqual(this) =>
				parts == composite.parts
			case _ => false
		}

		override def hashCode :Int = parts.view.map(_.hashCode).reduce(_ * 31 + _)

	}



	object CompositeFormula {
		trait CompositeMatcher[+F <: FromClause, +Y[X]]
			extends ConversionMatcher[F, Y] with TupleMatcher[F, Y] with CompositeColumnMatcher[F, Y]

		trait MatchComposite[+F <: FromClause, +Y[X]] extends CompositeMatcher[F, Y]
			with MatchCompositeColumn[F, Y] with CaseConversion[F, Y] with CaseTuple[F, Y]

		trait CaseComposite[+F <: FromClause, +Y[X]]
			extends MatchComposite[F, Y]
		{
			def composite[X](e :CompositeFormula[F, X]) :Y[X]

//			override def composite[X](e :CompositeColumnFormula[F, X]) :Y[X] = composite(e :CompositeFormula[F, X])

			override def tuple[X](e :SQLTuple[F, X]) :Y[X] = composite(e)

			override def conversion[Z, X](e :AutoConversionFormula[F, Z, X]) :Y[X] = composite(e)
			override def alias[V](e :AliasedColumn[F, V]) :Y[V] = composite(e)
			override def condition(e :SQLCondition[F]) :Y[Boolean] = composite(e)
			override def logical(e :LogicalFormula[F]) :Y[Boolean] = composite(e)
		}

	}



	trait CompositeColumnFormula[-F <: FromClause, X] extends CompositeFormula[F, X] with ColumnFormula[F, X] {
		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ColumnFormula[S, X]

//		override def stretch[M[O] <: MappingFrom[O]] :ColumnFormula[F With M, X] =
//			map(SQLScribe.stretcher[F, F With M])

//		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :ColumnFormula[S, X] =
//			map(SQLScribe.stretcher)

		override def stretch[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :ColumnFormula[S, X] =
			map(SQLScribe.stretcher(target))

	}



	object CompositeColumnFormula {

		trait CompositeColumnMatcher[+F <: FromClause, +Y[X]]
			extends ConditionMatcher[F, Y] with LogicalMatcher[F, Y] with ColumnPromotionMatcher[F, Y]
			   with AliasedColumnMatcher[F, Y]

		trait MatchCompositeColumn[+F <: FromClause, +Y[X]] extends CompositeColumnMatcher[F, Y]
			with CaseCondition[F, Y] with CaseLogical[F, Y] with CaseColumnPromotion[F, Y]
			with CaseAliasedColumn[F, Y]

		trait CaseCompositeColumn[+F <: FromClause, +Y[X]] extends MatchCompositeColumn[F, Y] {
			def composite[X](e :CompositeColumnFormula[F, X]) :Y[X]

			override def logical(e :LogicalFormula[F]) :Y[Boolean] = composite(e)
			override def promotion[T, U](e :ColumnPromotionConversion[F, T, U]) :Y[U] = composite(e)
			override def condition(e :SQLCondition[F]) :Y[Boolean] = composite(e)
			override def alias[V](e :AliasedColumn[F, V]) :Y[V] = composite(e)
		}
	}






	/** Attests that expressions of type `L` and `R` are compatible from the SQL point of view and
	  * can be directly compared in scala after lifting both sides to type `T`.
	  * This may mean for example that, for the purpose of generated SQL, we treat `Option[X]` and `X`
	  * as directly comparable: `SQLTypePromotion[Option[X], X, Option[X]]` or let us promote number types to
	  * a higher precision: `SQLTypePromotion[Int, Long, Long]`.
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
		  */ //todo: move this up to SQLFormula
		abstract class Lift[X, Y] {
			def apply(value :X) :Y
			def inverse(value :Y) :Option[X] //todo: we need it for SetComponent, but it may fail or lose precision

			def lower(value :Y) :X = inverse(value) match {
				case Some(x) => x
				case _ => throw new IllegalArgumentException(
					s"Cannot convert $value :${value.unqualifiedClassName} back with $this."
				)
			}

			def apply[F <: FromClause](expr :SQLFormula[F, X]) :SQLFormula[F, Y] = expr.to(this)
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

				override def apply[S <: FromClause](expr: SQLFormula[S, X]): SQLFormula[S, Z] =
					next(prev(expr))
			}

			private[this] val ident = new Lift[Any, Any] {
				override def apply(value: Any): Any = value
				override def inverse(value: Any): Option[Any] = Some(value)
				override def apply[S <: FromClause](expr: SQLFormula[S, Any]): SQLFormula[S, Any] = expr
				override def toString = "_"
			}

			private[this] val opt = new Lift[Any, Option[Any]] {

				override def apply(value: Any): Option[Any] = Option(value)

				override def inverse(value: Option[Any]): Option[Any] = value

				override def apply[S <: FromClause](expr: SQLFormula[S, Any]): SQLFormula[S, Option[Any]] = expr.opt
				override def toString = "Option[_]"
			}

			private[this] val selectRow = new Lift[Rows[Any], Any] {
				override def apply(value: Rows[Any]): Any = value.head
				override def inverse(value: Any): Option[Rows[Any]] = Some(Rows(value))

				override def toString = "Rows.one"
			}

			private[this] val selectRows = new Lift[Rows[Any], Seq[Any]] {
				override def apply(value: Rows[Any]): Seq[Any] = Seq(value.seq)
				override def inverse(value: Seq[Any]): Option[Rows[Any]] = value match {
					case Seq(row) => Some(Rows(row))
					case _ => None
				}

				override def toString = "Rows.seq"
			}

		}


	}






	/** A visitor traversing the structure of an SQL formula over tables in `F` and producing a value of `Y[X]`,
	  * where `X` is the value type of the mapped formula. This interface declares a method for every concrete formula
	  * class defined in this package, which is invoked in double dispatch `apply(formula)`/`formula.applyTo(this)`.
	  * This makes for quite a large number of cases, making it a problem both for maintainers and implementors
	  * who may not need to handle every individual type directly. To alleviate the former, this interface
	  * is built by recursively extending partial traits defining methods for visiting all subclasses of a given
	  * formula class. They typically form a one-to-one relationship with formula classes, with each formula
	  * declaring in its companion object (or in the same scope for inner classes) its own matcher, extending
	  * the matchers of its subclasses. To alleviate the latter, two parallel interface families are introduced:
	  * `Case`''Formula'' and `Match`''Formula''. The former implements all visitor methods for all subclasses
	  * of ''Formula'' by delegating to the method for the ''Formula'' class (introduced by the trait if the formula
	  * is abstract), catching all subclasses in a single case. The latter is similar, but doesn't introduce
	  * a new method for abstract formulas and leaves all methods for direct subclasses of ''Formula'' not implemented.
	  * The methods for ''their'' subclasses delegate as in the former example. This in turn has the effect of matching
	  * against a smallest set of classes covering all concrete subclasses of ''Formula''. `ColumnFormula` subtypes
	  * are a bit special, as they have their own hierarchy, parallel to that of plain SQLFormulas. Wherever a
	  * formula type, for example `SQLLiteral`, exists in both non-column and column versions, the non-column ''matcher''
	  * will extend the associated column ''matcher''. The `CaseLiteral` trait in our example will implement
	  * the callback for the column literal by delegating it to the method for the base literal. In this way,
	  * the standard delegation chain for any column formula always starts by delegating to the non-column method.
	  * It is however possible to change by mixing in one or more of the `Match` or `Case` traits for particular
	  * column formula types after the general matcher traits. For example,
	  * `CaseFormula[F, Y] with CaseColumnFormula[F, Y]` will change it to the opposite: all callbacks for column
	  * formulas reach the `formula` callback for the root `ColumnFormula` and, only then, delegate to the `formula`
	  * method for `SQLFormula`. By mixing in a selection of the above traits, one has a good degree of freedom
	  * in selecting which parts of the `SQLFormula` hierarchy are handled in detail and which are glossed over.
	  * @see [[net.noresttherein.oldsql.sql.SQLFormula.MatchFormula]]
	  * @see [[net.noresttherein.oldsql.sql.SQLFormula.CaseFormula]]
	  * @see [[net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.ColumnFormulaMatcher]]
	  */
	trait FormulaMatcher[+F <: FromClause, +Y[X]] extends SQLMapper[F, Y]
		with ColumnFormulaMatcher[F, Y] with CompositeMatcher[F, Y] with MappingMatcher[F, Y]
		with SelectMatcher[F, Y] with TermMatcher[F, Y]
	{
		override def apply[X](f: SQLFormula[F, X]): Y[X] = f.applyTo(this) //f.extract(this) getOrElse unhandled(f)
	}


	/** A `FormulaMatcher` delegating all visitor calls to the methods specific to one of the direct `SQLFormula`
	  * subclasses: `term`, `composite`, `select`, `mapping`.
	  * @see [[net.noresttherein.oldsql.sql.SQLFormula.FormulaMatcher]]
	  */
	trait MatchFormula[+F <: FromClause, +Y[X]]
		extends CaseComposite[F, Y] with CaseMapping[F, Y] with CaseSelect[F, Y] with CaseTerm[F, Y]

	/** A not particularly useful `FormulaMatcher` which delegates all the cases to the single `formula` method
	  * invoked for every subformula (SQL AST node). Used as a base class when only few cases need special handling.
	  * @see [[net.noresttherein.oldsql.sql.SQLFormula.FormulaMatcher]]
	  */
	trait CaseFormula[+F <: FromClause, +Y[X]] extends FormulaMatcher[F, Y] with MatchFormula[F, Y] {
		def formula[X](e :SQLFormula[F, X]) :Y[X]

		override def composite[X](e: CompositeFormula[F, X]): Y[X] = formula(e)
		override def mapping[M <: Mapping](e :MappingFormula[F, M]) :Y[M#Subject] = formula(e)
		override def select[V, O](e :SelectFormula[F, V, O]) :Y[Rows[V]] = formula(e)
		override def term[X](e: SQLTerm[X]): Y[X] = formula(e)
	}

	trait SelectiveMatcher[+F <: FromClause, +Y[X]] extends CaseFormula[F, Y] {
		override def formula[X](e: SQLFormula[F, X]): Y[X] = unhandled(e)
	}



}

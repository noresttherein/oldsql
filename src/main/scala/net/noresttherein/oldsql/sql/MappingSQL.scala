package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnMappingExtract, ColumnReadForm, Mapping, MappingExtract, RowSource, SQLReadForm, TypedMapping}
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnExpressionMatcher
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, OuterFrom, PrefixOf, TableShift}
import net.noresttherein.oldsql.sql.MappingSQL.ColumnComponentSQL.{CaseColumnComponent, ColumnComponentMatcher}
import net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL.{CaseComponent, ComponentMatcher, ProperComponent}
import net.noresttherein.oldsql.sql.MappingSQL.FreeColumn.FreeColumnMatcher
import net.noresttherein.oldsql.sql.MappingSQL.FreeComponent.{CaseFreeComponent, FreeComponentMatcher}
import net.noresttherein.oldsql.sql.MappingSQL.SQLRelation.{CaseRelation, RelationMatcher}
import net.noresttherein.oldsql.sql.SelectSQL.{SelectColumnMapping, SelectMapping, SubselectAs, SubselectColumnMapping}
import net.noresttherein.oldsql.sql.SQLExpression.ExpressionMatcher



//here be implicits
import slang._






/**
  * @author Marcin Mościcki
  */
trait MappingSQL[-F <: FromClause, M <: Mapping] extends SQLExpression[F, M#Subject] {
	type Subject = M#Subject

	/** Returns `this` upcast to an `SQLExpression`. This method exists because many expressions,
	  * such as `table \ component` producing some subtype of a `MappingSQL` in a place where
	  * its supertype `SQLExpression[F, Subject]` is expected, will confuse the compiler and make type inference fail.
	  * While simply splitting the above into a `val` assignment and its access would solve the issue, calling
	  * `(table \ component).upcast` is the most concise way of separating the expression creation with the type
	  * inference and the returned value.
	  */
	def upcast :SQLExpression[F, Subject] = this


	def mapping :M

	override def readForm :SQLReadForm[Subject] = mapping.selectForm

}






object MappingSQL {


	/** An expression evaluating to a component mapping of an undetermined at this point relation. It needs to be
	  * resolved by the `Origin` type of the component before the expression can be used.
	  */
	class FreeComponent[F <: FromClause, M[A] <: TypedMapping[V, A], V] private[MappingSQL]
	                   (override val mapping :M[F], val shift :Int)
		extends MappingSQL[F, M[F]]
	{
		type Origin = F

		override def readForm :SQLReadForm[V] = mapping.selectForm

		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :FreeComponent[S, M, V] =
			new FreeComponent[S, M, V](mapping.asInstanceOf[M[S]], shift + ev.length)



		override def applyTo[Y[_]](matcher :ExpressionMatcher[F, Y]) :Y[M[F]#Subject] =
			matcher.freeComponent(this)



		override def isomorphic(expression :SQLExpression.*) :Boolean = ???

		override private[oldsql] def equivalent(expression :SQLExpression.*) = ???

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true

			case free :FreeComponent.* @unchecked if free canEqual this =>
				free.mapping == mapping && free.shift == shift

			case _ => false
		}


		override def toString :String = mapping.toString + "#" + shift
	}






	object FreeComponent {

		def apply[F <: FromClause, C <: Mapping, M[A] <: TypedMapping[V, A], V]
		         (mapping :C, shift :Int)(implicit conforms :Conforms[C, M[F], TypedMapping[V, F]])
				:FreeComponent[F, M, V] =
			conforms(mapping) match {
				case column :ColumnMapping[V @unchecked, F @unchecked] =>
					new FreeColumn[F, MappingOf[V]#ColumnProjection, V](column, shift).asInstanceOf[FreeComponent[F, M, V]]
				case component =>
					new FreeComponent[F, M, V](component, shift)
			}

		def apply[F <: FromClause, C <: Mapping, M[A] <: TypedMapping[V, A], V]
		         (mapping :C)
		         (implicit conforms :Conforms[C, M[F], TypedMapping[V, F]], shift :TableShift[F, M, _ <: Numeral])
				:FreeComponent[F, M, V] =
				apply(mapping, shift.tables)


		def unapply[F <: FromClause, X](formula :SQLExpression[F, X]) :Option[(TypedMapping[X, _ >: F <: FromClause], Int)] =
			formula match {
				case free: FreeComponent.Typed[F, X] @unchecked => Some(free.mapping -> free.shift)
				case _ => None
			}

		def unapply[F <: FromClause, M[A] <: TypedMapping[X, A], X](formula :FreeComponent[F, M, X]) :Option[(M[F], Int)] =
			Some(formula.mapping -> formula.shift)



		type * = FreeComponent[F, M, V] forSome {
			type F <: FromClause; type M[A] <: TypedMapping[V, A]; type V
		}

		private type AnyIn[-F <: FromClause] = FreeComponent[O, M, V] forSome {
			type O >: F <: FromClause; type M[A] <: TypedMapping[V, A]; type V
		}

		private type Typed[-F <: FromClause, V] = FreeComponent[O, M, V] forSome {
			type O >: F <: FromClause; type M[A] <: TypedMapping[V, A]
		}



		trait FreeComponentMatcher[+F <: FromClause, +Y[X]] extends FreeColumnMatcher[F, Y] {
			def freeComponent[O >: F <: FromClause, M[A] <: TypedMapping[X, A], X](e :FreeComponent[O, M, X]) :Y[X]
		}

		type MatchFreeComponent[+F <: FromClause, +Y[X]] = FreeComponentMatcher[F, Y]

		trait CaseFreeComponent[+F <: FromClause, +Y[X]] extends FreeComponentMatcher[F, Y] {
			override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
			                          (e :FreeColumn[O, M, V]) :Y[V] =
				freeComponent(e :FreeComponent[O, M, V])
		}

	}






	class FreeColumn[F <: FromClause, M[A] <: ColumnMapping[V, A], V] private[MappingSQL]
	                (column :M[F], shift :Int)
		extends FreeComponent[F, M, V](column, shift) with ColumnSQL[F, V]
	{
		//this should really use mapping.selectForm, bot it is not a ColumnForm due to possible buffs
		//doesn't really matter though, as this class is a placeholder and the form will never get used.
		override def readForm :ColumnReadForm[V] = mapping.form

		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :FreeColumn[S, M, V] =
			new FreeColumn[S, M, V](column.asInstanceOf[M[S]], shift + ev.length)


		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[V] =
			matcher.freeComponent[F, M, V](this)

	}






	object FreeColumn {

		def apply[F <: FromClause, C <: Mapping, M[A] <: ColumnMapping[V, A], V]
		         (column :C, shift :Int)(implicit conforms :Conforms[C, M[F], ColumnMapping[V, F]])
				:FreeColumn[F, M, V] =
			new FreeColumn[F, M, V](column, shift)


		def apply[F <: FromClause, C <: ColumnMapping[_, _], M[A] <: ColumnMapping[V, A], V]
		         (column :C)
		         (implicit conforms :Conforms[C, M[F], ColumnMapping[V, F]], shift :TableShift[F, M, _ <: Numeral])
				:FreeColumn[F, M, V] =
			apply(column, shift.tables)


		def unapply[F <: FromClause, X](formula :SQLExpression[F, X]) :Option[(ColumnMapping[X, _ >: F <: FromClause], Int)] =
			formula match {
				case free: FreeColumn.Typed[F, X] @unchecked => Some(free.mapping -> free.shift)
				case _ => None
			}

		def unapply[F <: FromClause, M[A] <: TypedMapping[X, A], X](formula :FreeComponent[F, M, X])
				:Option[(M[F], Int)] =
			(formula :FreeComponent.*) match {
				case _ :FreeColumn.* @unchecked => Some(formula.mapping -> formula.shift)
				case _ => None
			}



		type * = FreeColumn[_ <: FromClause, M, V] forSome { type M[A] <: ColumnMapping[V, A]; type V }

		type AnyIn[-F <: FromClause] = FreeColumn[O, M, V]
				forSome { type O >: F <: FromClause; type M[A] <: ColumnMapping[V, A]; type V }

		type Typed[-F <: FromClause, V] = FreeColumn[O, M, V]
				forSome { type O >: F <: FromClause; type M[A] <: ColumnMapping[V, A] }



		trait FreeColumnMatcher[+F <: FromClause, +Y[X]] {
			def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V](e :FreeColumn[O, M, V]) :Y[V]
		}

		type MatchFreeColumn[+F <: FromClause, +Y[X]] = FreeColumnMatcher[F, Y]

		type CaseFreeColumn[+F <: FromClause, +Y[X]] = FreeColumnMatcher[F, Y]

	}






	trait BaseComponentSQL[-F <: FromClause, T[A] <: MappingAt[A], M[A] <: MappingAt[A], O >: F <: FromClause]
		extends MappingSQL[F, M[O]]
	{
		type Origin = O

		def from :JoinedRelation[O, T]
		def table :T[O] = from.mapping
		def source :RowSource[T] = from.source

		def extract :MappingExtract[T[O]#Subject, M[O]#Subject, O]

		override def readForm :SQLReadForm[Subject] = extract.export.selectForm

		def \[K <: Mapping, C[A] <: TypedMapping[X, A], X]
		     (component :K)(implicit inferer :Conforms[K, C[O], TypedMapping[X, O]]) :BaseComponentSQL[F, T, C, O]

		def \[K <: Mapping, C[A] <: TypedMapping[X, A], X]
		     (component :M[O] => K)(implicit inferer :Conforms[K, C[O], TypedMapping[X, O]])
				:BaseComponentSQL[F, T, C, O]

		def \[K <: ColumnMapping[_, O], C[A] <: ColumnMapping[X, A], X]
		     (column :K)(implicit conforms :Conforms[K, C[O], ColumnMapping[X, O]])
				:BaseColumnComponentSQL[F, T, C, X, O]



		override def isomorphic(expression :SQLExpression.*) :Boolean = ???
		override private[oldsql] def equivalent(expression :SQLExpression.*) = ???


		override def toString = from.toString + "." + mapping
	}



	trait ComponentSQL[-F <: FromClause, T[A] <: TypedMapping[E, A], E,
	                       M[A] <: TypedMapping[V, A], V, O >: F <: FromClause]
		extends BaseComponentSQL[F, T, M, O]
	{
		override def from :SQLRelation[F, T, E, O]

		def extract :MappingExtract[E, V, O]

		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S)
				:ComponentSQL[S, T, E, M, V, _ >: S <: FromClause]



		override def selectFrom[S <: F with OuterFrom, A](from :S) :SelectMapping[S, M, V, A] =
			SelectSQL(from, this)

		override def subselectFrom[S <: F, A](from :S) :SubselectAs[from.Implicit, M[A]] =
			SelectSQL.subselect[from.Implicit, from.type, T, E, M, V, O, A](from, this)



		override def \[K <: Mapping, C[A] <: TypedMapping[X, A], X]
		              (component :K)(implicit inferer :Conforms[K, C[O], TypedMapping[X, O]])
				:ComponentSQL[F, T, E, C, X, O] =
			ComponentSQL(from, component)

		override def \[K <: Mapping, C[A] <: TypedMapping[X, A], X]
		              (component :M[O] => K)(implicit inferer :Conforms[K, C[O], TypedMapping[X, O]])
				:ComponentSQL[F, T, E, C, X, O] =
			this \ component(mapping)

		override def \[K <: ColumnMapping[_, O], C[A] <: ColumnMapping[X, A], X]
		              (column :K)(implicit conforms :Conforms[K, C[O], ColumnMapping[X, O]])
				:ColumnComponentSQL[F, T, E, C, X, O] =
			ColumnComponentSQL(from, column)



		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentSQL.* @unchecked]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case component :ComponentSQL.* @unchecked if canEqual(component) && component.canEqual(this) =>
				from == component.from && mapping == component.mapping //consider: should it compare extractor.export?
			case _ => false
		}

		override def hashCode :Int = from.hashCode * 31 + mapping.hashCode
	}






	object ComponentSQL {

		def apply[F <: FromClause, T[A] <: TypedMapping[E, A], E, K <: Mapping,
		          M[A] <: TypedMapping[V, A], V, O >: F <: FromClause]
		         (from :SQLRelation[F, T, E, O], component :K)(implicit inferer :Conforms[K, M[O], TypedMapping[V, O]])
				:ComponentSQL[F, T, E, M, V, O] =
			component match {
				case column :ColumnMapping[Any @unchecked, O @unchecked] =>
					ColumnComponentSQL(from, column).asInstanceOf[ComponentSQL[F, T, E, M, V, O]]
				case _ =>
					new ProperComponent[F, T, E, M, V, O](from, inferer(component))
			}



		def unapply[F <: FromClause, X](e :SQLExpression[F, X])
				:Option[(SQLRelation[F, T, E, O], MappingExtract[E, X, O])
						forSome { type T[O] <: TypedMapping[E, O]; type E; type O >: F <: FromClause }] =
			e match {
				case component :ComponentSQL.* @unchecked =>
					Some((component.from, component.extract).asInstanceOf[
						(SQLRelation[F, MappingOf[Any]#TypedProjection, Any, F], MappingExtract[Any, X, F])
					])
				case _ => None
			}



		type * = ComponentSQL[F, T, E, M, V, O] forSome {
			type F <: FromClause; type O >: F <: FromClause
			type T[A] <: TypedMapping[E, A]; type E
			type M[A] <: TypedMapping[V, A]; type V
		}

		type AnyIn[-F <: FromClause] = ComponentSQL[F, T, E, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: TypedMapping[E, A]; type E
			type M[A] <: TypedMapping[V, A]; type V
		}

		type Typed[-F <: FromClause, V] = ComponentSQL[F, T, E, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: TypedMapping[E, A]; type E
			type M[A] <: TypedMapping[V, A]
		}



		private[MappingSQL] class ProperComponent[-F <: FromClause, T[A] <: TypedMapping[E, A], E,
		                                              M[A] <: TypedMapping[V, A], V, O >: F <: FromClause]
		                                             (override val from :SQLRelation[F, T, E, O],
		                                              override val mapping :M[O])
			extends ComponentSQL[F, T, E, M, V, O]
		{
			override val extract = table(mapping)

			override val readForm :SQLReadForm[V] = extract.export.selectForm

			override def stretch[U <: F, G <: FromClause](base :G)(implicit ev :U ExtendedBy G)
					:ProperComponent[G, T, E, M, V, _ >: G <: FromClause] =
				new ProperComponent[G, T, E, M, V, G](  //type G as O is incorrect, but a correct O exists and we upcast anyway
					from.stretch(base).asInstanceOf[SQLRelation[G, T, E, G]], mapping.asInstanceOf[M[G]]
				)

			override def applyTo[Y[_]](matcher :ExpressionMatcher[F, Y]) :Y[V] =
				matcher.component(this)
		}



		trait ComponentMatcher[+F <: FromClause, +Y[X]] extends RelationMatcher[F, Y] with ColumnComponentMatcher[F, Y] {
			def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O >: F <: FromClause]
			             (e :ComponentSQL[F, T, E, M, V, O]) :Y[V]
		}

		trait MatchComponent[+F <: FromClause, +Y[X]]
			extends ComponentMatcher[F, Y] with CaseRelation[F, Y] with CaseColumnComponent[F, Y]
		{
			override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
			                     (e :ColumnComponentSQL[F, T, E, M, V, O]) :Y[V] =
				component[T, E, M, V, O](e :ComponentSQL[F, T, E, M, V, O])
		}

		trait CaseComponent[+F <: FromClause, +Y[X]] extends MatchComponent[F, Y] {
			override def relation[T[A] <: TypedMapping[E, A], E, O >: F <: FromClause]
			                     (e :SQLRelation[F, T, E, O]) :Y[E] =
				component(e)
		}
	}






	trait BaseColumnComponentSQL[-F <: FromClause, T[A] <: MappingAt[A],
	                                 M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		extends BaseComponentSQL[F, T, M, O] with ColumnSQL[F, V]
	{
		override def extract :ColumnMappingExtract[T[O]#Subject, V, O]

		//fixme: this should really use the selectForm, but it is not a ColumnForm due to buff excludes/const etc
		override def readForm :ColumnReadForm[V] = extract.export.form

		override def upcast :ColumnSQL[F, Subject] = this

		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S)
				:BaseColumnComponentSQL[S, T, M, V, _ >: S <: FromClause]
	}



	trait ColumnComponentSQL[-F <: FromClause, T[A] <: TypedMapping[E, A], E,
	                             M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		extends ComponentSQL[F, T, E, M, V, O] with BaseColumnComponentSQL[F, T, M, V, O]
	{
		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S)
				:ColumnComponentSQL[S, T, E, M, V, _ >: S <: FromClause]

		override def selectFrom[S <: F with OuterFrom, A](from :S) :SelectColumnMapping[S, M, V, A] =
			SelectSQL(from, this)

		override def subselectFrom[S <: F, A](from :S) :SubselectColumnMapping[from.Implicit, from.type, M, V, A] =
			SelectSQL.subselect[from.Implicit, from.type, T, E, M, V, O, A](from, this) //todo: from.type is ugly!

		override def applyTo[Y[_]](matcher :ColumnExpressionMatcher[F, Y]) :Y[V] = matcher.component(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnComponentSQL.* @unchecked]
	}






	object ColumnComponentSQL {

		def apply[F <: FromClause, T[A] <: TypedMapping[E, A], E,
		          C <: ColumnMapping[_, _], M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		         (from :SQLRelation[F, T, E, O], column :C)(implicit conforms :Conforms[C, M[O], ColumnMapping[V, O]])
				:ColumnComponentSQL[F, T, E, M, V, O] =
			new ProperColumn(from, column)



		def unapply[F <: FromClause, X](e :SQLExpression[F, X])
				:Option[(SQLRelation[F, T, E, O], ColumnMappingExtract[E, X, O])
						forSome { type T[O] <: TypedMapping[E, O]; type E; type O >: F <: FromClause }] =
			e match {
				case component :ColumnComponentSQL.* @unchecked =>
					Some((component.from, component.extract).asInstanceOf[
						(SQLRelation[F, MappingOf[Any]#TypedProjection, Any, F], ColumnMappingExtract[Any, X, F])
					])
				case _ => None
			}



		type * = ColumnComponentSQL[F, T, E, M, V, O] forSome {
			type F <: FromClause; type O >: F <: FromClause
			type T[A] <: TypedMapping[E, A]; type E
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type AnyIn[-F <: FromClause] = ColumnComponentSQL[F, T, E, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: TypedMapping[E, A]; type E
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type Typed[-F <: FromClause, V] = ColumnComponentSQL[F, T, E, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: TypedMapping[E, A]; type E
			type M[A] <: ColumnMapping[V, A]
		}



		private class ProperColumn[-F <: FromClause, T[A] <: TypedMapping[E, A], E,
		                           M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                          (relation :SQLRelation[F, T, E, O], column :M[O])
			extends ProperComponent[F, T, E, M, V, O](relation, column)
			   with ColumnComponentSQL[F, T, E, M, V, O]
		{
			override val extract = table(mapping)
			//fixme: sort out where the buff-related modifications take place to have consistent assembly semantics
			override val readForm = extract.export.form

			override def stretch[U <: F, G <: FromClause](base :G)(implicit ev :U ExtendedBy G)
					:ProperColumn[G, T, E, M, V, _ >: G <: FromClause] =
				new ProperColumn[G, T, E, M, V, G](
					from.stretch(base).asInstanceOf[SQLRelation[G, T, E, G]], column.asInstanceOf[M[G]]
				)
		}



		trait ColumnComponentMatcher[+F <: FromClause, +Y[X]] {
			def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
			             (e :ColumnComponentSQL[F, T, E, M, V, O]) :Y[V]
		}

		type MatchColumnComponent[+F <: FromClause, +Y[X]] = ColumnComponentMatcher[F, Y]

		type CaseColumnComponent[+F <: FromClause, +Y[X]] = ColumnComponentMatcher[F, Y]
	}






	sealed trait JoinedRelation[F <: FromClause, T[A] <: MappingAt[A]] extends BaseComponentSQL[F, T, T, F] {
		type Self = SQLRelation[F, M, T[F]#Subject, F] forSome { type M[A] <: TypedMapping[T[F]#Subject, A] with T[A] }
		def shift :Int

		override def from :JoinedRelation[F, T] = this

		def extend[M[A] <: MappingAt[A]] :JoinedRelation[F With M, T]

		/** This method is equivalent to `this.stretch()`, but doesn't require the `G` clause as the parameter
		  * and returns a `JoinedRelation`. The `stretch` method cannot be overriden here to return a `JoinedRelation`
		  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
		  */
		def extend[G <: FromClause](implicit extension :F PrefixOf G) :JoinedRelation[G, T]

		/** Casts down this instance to the more strongly typed `SQLRelation`. The result type is an `Option`
		  * not because the cast may fail, but because the correct existential subtype cannot exist as a value
		  * (returning simply the existentially refined `SQLRelation` produces a compiler error about being
		  * unable to abstract over existential higher type `T`).
		  */
		def toSQLRelation :Option[SQLRelation[F, M, T[F]#Subject, F]
				forSome { type M[A] <: TypedMapping[T[F]#Subject, A] with T[A] }]


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedRelation.*]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case relation :JoinedRelation.* if (this canEqual relation) && (relation canEqual this) =>
				relation.shift == shift && relation.mapping == mapping
			case _ => false
		}

		override def hashCode :Int = shift * 31 + mapping.hashCode

		override def toString :String = mapping.toString + "#" + shift
	}






	object JoinedRelation {

		def unapply[F <: FromClause, X](f :SQLExpression[F, X]) :Option[(RefinedMapping[X, _ >: F <: FromClause], Int)] =
			f match {
				case from :JoinedRelation.Typed[F, X] @unchecked =>
					Some(from.mapping -> from.shift)
				case _ => None
			}



		type * = JoinedRelation[_ <: FromClause, T] forSome { type T[O] <: MappingAt[O] }

		type AnyIn[F <: FromClause] = JoinedRelation[F, T] forSome { type T[O] <: MappingAt[O] }

		type Typed[F <: FromClause, V] = JoinedRelation[F, T] forSome { type T[O] <: RefinedMapping[V, O] }

	}






	class SQLRelation[-F <: FromClause, T[A] <: TypedMapping[E, A], E, O >: F <: FromClause] private[sql]
	                 (override val source :RowSource[T], override val mapping :T[O], override val shift :Int)
		extends JoinedRelation[O, T] with ComponentSQL[F, T, E, T, E, O]
	{
		def this(source :RowSource[T], shift :Int) = this(source, source[O], shift)


		override def from :SQLRelation[O, T, E, O] = actualType

		override val extract = MappingExtract.ident(mapping)

		@inline final def actualType :SQLRelation[O, T, E, O] = this.asInstanceOf[SQLRelation[O, T, E, O]]

		override def toSQLRelation :Some[SQLRelation[O, T, E, O]] = Some(actualType)

		override def upcast :BaseComponentSQL[O, T, T, O] = this



		override def selectFrom[S <: O with OuterFrom, A](from :S) :SelectMapping[S, T, E, A] =
			SelectSQL(from, actualType)

		override def subselectFrom[S <: O, A](from :S) :SubselectAs[from.Implicit, T[A]] =
			SelectSQL.subselect[from.Implicit, from.type, T, E, T, E, O, A](from, actualType)



		override def \[K <: Mapping, C[A] <: TypedMapping[X, A], X]
		              (component :K)(implicit inferer :Conforms[K, C[O], TypedMapping[X, O]])
				:ComponentSQL[O, T, E, C, X, O] =
			ComponentSQL(actualType, component)

		override def \[K <: Mapping, C[A] <: TypedMapping[X, A], X]
		              (component :T[O] => K)(implicit inferer :Conforms[K, C[O], TypedMapping[X, O]])
				:ComponentSQL[O, T, E, C, X, O] =
			ComponentSQL(actualType, component(mapping))

		override def \[K <: ColumnMapping[_, O], C[A] <: ColumnMapping[X, A], X]
		              (column :K)(implicit conforms :Conforms[K, C[O], ColumnMapping[X, O]])
				:ColumnComponentSQL[O, T, E, C, X, O] =
			ColumnComponentSQL(actualType, column)

		//fixme: these must reuse this.mapping or the mapping won't recognize the components!
		override def stretch[U <: O, G <: FromClause]
		                    (base :G)(implicit ev :U ExtendedBy G) :SQLRelation[G, T, E, _ >: G <: FromClause] =
			new SQLRelation[G, T, E, G](source, shift + ev.length) //G is incorrect, but we lose this information anyway


		override def extend[M[A] <: MappingAt[A]] :SQLRelation[F With M, T, E, O With M] =
			new SQLRelation[F With M, T, E, O With M](source, shift)

		override def extend[G <: FromClause](implicit extension :O PrefixOf G) :SQLRelation[G, T, E, G] =
			new SQLRelation(source, shift + extension.suffix)


		override def applyTo[Y[_]](matcher :ExpressionMatcher[O, Y]) :Y[E] = matcher.relation(actualType)
	}






	object SQLRelation {

		def apply[F <: FromClause, M[A] <: MappingAt[A], T[A] <: TypedMapping[S, A], S]
		         (table :M[F])
		         (implicit cast :Conforms[M[F], T[F], TypedMapping[S, F]], shift :TableShift[F, M, _ <: Numeral],
		          alias :OriginProjection[T[F], F, T[Any], Any])
				:SQLRelation[F, T, S, F] =
			new SQLRelation[F, T, S, F](RowSource[T, F](cast(table))(alias), table, shift.tables)

		private[sql] def apply[F <: FromClause, T[A] <: TypedMapping[S, A], S](source :RowSource[T], index :Int)
				:SQLRelation[F, T, T[Any]#Subject, F] =
			new SQLRelation[F, T, T[Any]#Subject, F](source, index)


		def last[M[A] <: MappingAt[A], T[A] <: TypedMapping[S, A], S]
		        (source :RowSource[M])
		        (implicit inference :Conforms[M[Any], T[Any], TypedMapping[S, Any]]) :LastRelation[T, S] =
			new SQLRelation[FromClause With T, T, S, FromClause With T](source.asInstanceOf[RowSource[T]], 0)



		def unapply[F <: FromClause, X](f :SQLExpression[F, X]) :Option[(TypedMapping[X, _ >: F <: FromClause], Int)] =
			f match {
				case from :SQLRelation.Typed[F, X] @unchecked =>
					Some(from.mapping -> from.shift)
				case _ => None
			}



		type * = SQLRelation[F, T, X, O] forSome {
			type F <: FromClause; type O >: F <: FromClause
			type T[A] <: TypedMapping[X, A]; type X
		}

		type AnyIn[-F <: FromClause] = SQLRelation[F, T, E, O] forSome {
			type T[A] <: TypedMapping[E, A]; type E; type O >: F <: FromClause
		}

		type Typed[-F <: FromClause, E] = SQLRelation[F, T, E, O] forSome {
			type T[A] <: TypedMapping[E, O]; type O >: F <: FromClause
		}

		type LastRelation[T[A] <: TypedMapping[S, A], S] = SQLRelation[FromClause With T, T, S, FromClause With T]

		def LastRelation[T[A] <: TypedMapping[S, A], S](from :RowSource[T]) :LastRelation[T, S] =
			new SQLRelation[FromClause With T, T, S, FromClause With T](from, 0)

		private[sql] def LastRelation[T[A] <: TypedMapping[S, A], S](from :RowSource[T], mapping :T[_])
				:LastRelation[T, S] =
			new SQLRelation[FromClause With T, T, S, FromClause With T](from, mapping.withOrigin[FromClause With T], 0)



		trait RelationMatcher[+F <: FromClause, +Y[X]] {
			def relation[T[A] <: TypedMapping[E, A], E, O >: F <: FromClause](e :SQLRelation[F, T, E, O]) :Y[E]
		}

		type MatchRelation[+F <: FromClause, +Y[X]] = RelationMatcher[F, Y]

		type CaseRelation[+F <: FromClause, +Y[X]] = RelationMatcher[F, Y]

	}






	trait MappingColumnMatcher[+F <: FromClause, +Y[X]] extends FreeColumnMatcher[F, Y] with ColumnComponentMatcher[F, Y]

	trait MappingMatcher[+F <: FromClause, +Y[X]]
		extends ComponentMatcher[F, Y] with FreeComponentMatcher[F, Y] with MappingColumnMatcher[F, Y]

	trait MatchMapping[+F <: FromClause, +Y[X]]
		extends MappingMatcher[F, Y] with CaseComponent[F, Y] with CaseFreeComponent[F, Y]

	trait CaseMapping[+F <: FromClause, +Y[X]] extends MatchMapping[F, Y] {
		def mapping[M <: Mapping](e :MappingSQL[F, M]) :Y[M#Subject]

		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, E, M, V, O]) :Y[V] =
			mapping(e)

		override def freeComponent[J >: F <: FromClause, M[A] <: TypedMapping[X, A], X](e :FreeComponent[J, M, X]) :Y[X] =
			mapping(e)
	}

}



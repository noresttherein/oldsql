package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnMappingExtract, ColumnReadForm, Mapping, MappingExtract, RowSource, SQLReadForm, TypedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause.{As, ExtendedBy, TableCount, TableShift}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.MappingFormula.ColumnComponentFormula.{CaseColumnComponent, ColumnComponentMatcher}
import net.noresttherein.oldsql.sql.MappingFormula.ComponentFormula.{CaseComponent, ComponentMatcher, ProperComponent}
import net.noresttherein.oldsql.sql.MappingFormula.FreeColumn.FreeColumnMatcher
import net.noresttherein.oldsql.sql.MappingFormula.FreeComponent.{CaseFreeComponent, FreeComponentMatcher}
import net.noresttherein.oldsql.sql.MappingFormula.SQLRelation.{CaseRelation, RelationMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.{ColumnFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.ColumnFormulaMatcher
import slang._


/**
  * @author Marcin Mościcki
  */
trait MappingFormula[-F <: FromClause, M <: Mapping] extends SQLFormula[F, M#Subject] { //todo:
	type Subject = M#Subject

	/** Returns `this` upcast to an `SQLFormula`. This method exists because many expressions,
	  * such as `table \ component` producing some subtype of a `MappingFormula` in a place where
	  * its supertype `SQLFormula[F, Subject]` is expected, will confuse the compiler and make type inference fail.
	  * While simply splitting the above into a `val` assignment and its access would solve the issue, calling
	  * `(table \ component).upcast` is the most concise way of separating the expression creation with the type
	  * inference and the returned value.
	  */
	def upcast :SQLFormula[F, Subject] = this


	def mapping :M

	override def readForm :SQLReadForm[Subject] = mapping.selectForm
//	def mapping(from :FromClause) :M
}






object MappingFormula {
//todo: subclasses for columns

	/** An expression evaluating to a component mapping of an undetermined at this point relation. It needs to be
	  * resolved by the `Origin` type of the component before the formula can be used.
	  */
	class FreeComponent[F <: FromClause, M[A] <: TypedMapping[V, A], V] private[MappingFormula]
	                   (override val mapping :M[F], val shift :Int)
		extends MappingFormula[F, M[F]]
	{
		type Origin = F

		override def readForm :SQLReadForm[V] = mapping.selectForm


		override def applyTo[Y[_]](matcher :FormulaMatcher[F, Y]) :Y[M[F]#Subject] =
			matcher.freeComponent(this)


//		override def isGroundedIn(tables :Iterable[AnyJoinedRelation]) :Boolean = false

//		override def basedOn[E <: FromClause](implicit subtype :E <:< F) :SQLFormula[E, V] =
//			throw new UnsupportedOperationException("Free")

		override def stretch[T[A] <: MappingFrom[A]] :FreeComponent[F With T, M, V] =
			new FreeComponent[F With T, M, V](mapping.asInstanceOf[M[F With T]], shift + 1)

		override def stretch[U <: F, S <: FromClause]
		                    (implicit ev :U ExtendedBy S) :FreeComponent[S, M, V] =
			new FreeComponent[S, M, V](mapping.asInstanceOf[M[S]], shift + ev.length)

		override def stretch[U <: F, S <: FromClause]
		                    (target :S)(implicit ev :U ExtendedBy S) :FreeComponent[S, M, V] =
			new FreeComponent[S, M, V](mapping.asInstanceOf[M[S]], shift + ev.length)


		override def isomorphic(expression :Formula[_]) :Boolean = ???

		override private[oldsql] def equivalent(expression :Formula[_]) = ???

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case free :FreeComponent.* if free canEqual this => free.mapping == mapping && free.shift == shift
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


		def unapply[F <: FromClause, X](formula :SQLFormula[F, X]) :Option[(TypedMapping[X, _ >: F <: FromClause], Int)] =
			formula match {
				case free: FreeComponent.* => Some(free.mapping.asInstanceOf[TypedMapping[X, F]] -> free.shift)
				case _ => None
			}

		def unapply[F <: FromClause, M[A] <: TypedMapping[X, A], X](formula :FreeComponent[F, M, X]) :Option[(M[F], Int)] =
			Some(formula.mapping -> formula.shift)



		trait FreeComponentMatcher[+F <: FromClause, +Y[X]] extends FreeColumnMatcher[F, Y] {
			def freeComponent[O >: F <: FromClause, M[A] <: TypedMapping[X, A], X](e :FreeComponent[O, M, X]) :Y[X]
		}

		type MatchFreeComponent[+F <: FromClause, +Y[X]] = FreeComponentMatcher[F, Y]

		trait CaseFreeComponent[+F <: FromClause, +Y[X]] extends FreeComponentMatcher[F, Y] {
			override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
			                          (e :FreeColumn[O, M, V]) :Y[V] =
				freeComponent(e :FreeComponent[O, M, V])
		}



		type * = FreeComponent[F, M, X] forSome {
			type F <: FromClause; type M[A] <: RefinedMapping[_, A]; type X
		}

		private type AnyIn[F <: FromClause] = FreeComponent[O, M, X] forSome {
			type O >: F <: FromClause; type M[A] <: RefinedMapping[X, A]; type X
		}

	}






	class FreeColumn[F <: FromClause, M[A] <: ColumnMapping[V, A], V] private[MappingFormula]
	                (column :M[F], shift :Int)
		extends FreeComponent[F, M, V](column, shift) with ColumnFormula[F, V]
	{
		//fixme: this should really use mapping.selectForm, bot it is not a ColumnForm due to possible buffs
		override def readForm :ColumnReadForm[V] = mapping.form

		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[V] =
			matcher.freeComponent[F, M, V](this)


		override def stretch[T[O] <: MappingFrom[O]] :FreeColumn[F With T, M, V] =
			new FreeColumn[F With T, M, V](column.asInstanceOf[M[F With T]], shift + 1)

		override def stretch[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :FreeColumn[S, M, V] =
			new FreeColumn[S, M, V](column.asInstanceOf[M[S]], shift + ev.length)

		override def stretch[U <: FromClause, S <: FromClause](implicit ev :U ExtendedBy S) :FreeColumn[S, M, V] =
			new FreeColumn[S, M, V](column.asInstanceOf[M[S]], shift + ev.length)
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


		def unapply[F <: FromClause, X](formula :SQLFormula[F, X]) :Option[(ColumnMapping[X, _ >: F <: FromClause], Int)] =
			formula match {
				case free: FreeColumn.* => Some(free.mapping.asInstanceOf[ColumnMapping[X, F]] -> free.shift)
				case _ => None
			}

		def unapply[F <: FromClause, M[A] <: TypedMapping[X, A], X](formula :FreeComponent[F, M, X])
				:Option[(M[F], Int)] =
			(formula :FreeComponent.*) match {
				case _ :(FreeColumn.* @unchecked) => Some(formula.mapping -> formula.shift)
				case _ => None
			}



		type * = FreeColumn[_ <: FromClause, M, _] forSome { type M[A] <: ColumnMapping[_, A] }



		trait FreeColumnMatcher[+F <: FromClause, +Y[X]] {
			def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V](e :FreeColumn[O, M, V]) :Y[V]
		}

		type MatchFreeColumn[+F <: FromClause, +Y[X]] = FreeColumnMatcher[F, Y]

		type CaseFreeColumn[+F <: FromClause, +Y[X]] = FreeColumnMatcher[F, Y]

	}






	trait AbstractComponentFormula[-F <: FromClause, T[A] <: MappingFrom[A], M[A] <: MappingFrom[A], O]
		extends MappingFormula[F, M[O]] //with CompositeFormula[F, M[O]#Subject]
	{
		type Origin = O

		def from :AbstractSQLRelation[F, T, O]
		def table :T[O] = from.mapping
		def source :RowSource[T] = from.source

		def extractor :MappingExtract[T[O]#Subject, M[O]#Subject, O] //= table(mapping)

		override def readForm :SQLReadForm[Subject] = extractor.export.selectForm
/*

		def \[C[A] <: MappingFrom[A]](component :C[O]) :ComponentFormula[F, T, C, O] =
			ComponentFormula(from, component)

		def \[C[A] <: MappingFrom[A]](component :M[O] => C[O]) :ComponentFormula[F, T, C, O] =
			ComponentFormula(from, component(mapping))

		def \[X <: ColumnMapping[_, O], C[A] <: ColumnMapping[V, A], V]
		     (column :X)(implicit conforms :Conforms[X, C[O], ColumnMapping[V, O]])
				:ColumnComponentFormula[F, T, C, V, O] =
			ColumnComponentFormula(from, column)
*/


//		override def isGroundedIn(tables :Iterable[JoinedRelation.*]) :Boolean =
//			tables.exists(_ == from)


		override def isomorphic(expression :Formula[_]) :Boolean = ???
		override private[oldsql] def equivalent(expression :Formula[_]) = ???

		override def toString = from.toString + "." + mapping
	}



	trait ComponentFormula[-F <: FromClause, T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O]
		extends AbstractComponentFormula[F, T, M, O]
	{
		override def from :SQLRelation[F, T, E, O]

		def extractor :MappingExtract[E, V, O] //= table(mapping)

		def \[K <: Mapping, C[A] <: TypedMapping[X, A], X]
		     (component :K)(implicit inferer :Conforms[K, C[O], TypedMapping[X, O]])
				:ComponentFormula[F, T, E, C, X, O] =
			ComponentFormula(from, component)

		def \[K <: Mapping, C[A] <: TypedMapping[X, A], X]
		     (component :M[O] => K)(implicit inferer :Conforms[K, C[O], TypedMapping[X, O]])
				:ComponentFormula[F, T, E, C, X, O] =
			this \ component(mapping)

		def \[K <: ColumnMapping[_, O], C[A] <: ColumnMapping[X, A], X]
		     (column :K)(implicit conforms :Conforms[K, C[O], ColumnMapping[X, O]])
				:ColumnComponentFormula[F, T, E, C, X, O] =
			ColumnComponentFormula(from, column)

	}



	object ComponentFormula {

		def apply[F <: FromClause, T[A] <: TypedMapping[E, A], E, K <: Mapping, M[A] <: TypedMapping[V, A], V, O]
		         (from :SQLRelation[F, T, E, O], component :K)(implicit inferer :Conforms[K, M[O], TypedMapping[V, O]])
				:ComponentFormula[F, T, E, M, V, O] =
			component match {
				case column :ColumnMapping[Any @unchecked, O @unchecked] =>
					ColumnComponentFormula(from, column).asInstanceOf[ComponentFormula[F, T, E, M, V, O]]
				case _ =>
					new ProperComponent[F, T, E, M, V, O](from, inferer(component))
			}



		def unapply[F <: FromClause, X](e :SQLFormula[F, X])
				:Option[(SQLRelation.AnyIn[F], MappingExtract[_, X, _])] =
			e match {
				case component :ComponentFormula.* => Some((
					component.from.asInstanceOf[SQLRelation[F, MappingOf[Any]#TypedProjection, Any, Any]],
					component.extractor.asInstanceOf[MappingExtract[_, X, Any]]
				))
				case _ => None
			}



		type * = ComponentFormula[_ <: FromClause, T, _, M, _, _] forSome {
			type T[A] <: TypedMapping[_, A]; type M[A] <: TypedMapping[_, A]
		}

		type AnyIn[-F <: FromClause] = ComponentFormula[F, T, _, M, _, _] forSome {
			type T[A] <: TypedMapping[_, A]; type M[A] <: TypedMapping[_, A]
		}



		private[MappingFormula] class ProperComponent[-F <: FromClause, T[A] <: TypedMapping[E, A], E,
		                                              M[A] <: TypedMapping[V, A], V, O]
		                                             (override val from :SQLRelation[F, T, E, O],
		                                              override val mapping :M[O])
			extends ComponentFormula[F, T, E, M, V, O]
		{
			override val extractor = table(mapping)

			override val readForm :SQLReadForm[V] = extractor.export.selectForm


			override def applyTo[Y[_]](matcher :FormulaMatcher[F, Y]) :Y[V] =
				matcher.component[T, E, M, V, O](this)

			override def stretch[U <: F, G <: FromClause](implicit ev :U ExtendedBy G) :SQLFormula[G, V] =
				new ProperComponent[G, T, E, M, V, G](from.extend[U, G], mapping.asInstanceOf[M[G]])
		}



		trait ComponentMatcher[+F <: FromClause, +Y[X]] extends RelationMatcher[F, Y] with ColumnComponentMatcher[F, Y] {
			def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O]
			             (e :ComponentFormula[F, T, E, M, V, O]) :Y[V]
		}

		trait MatchComponent[+F <: FromClause, +Y[X]]
			extends ComponentMatcher[F, Y] with CaseRelation[F, Y] with CaseColumnComponent[F, Y]
		{
			override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O]
			                     (e :ColumnComponentFormula[F, T, E, M, V, O]) :Y[V] =
				component[T, E, M, V, O](e :ComponentFormula[F, T, E, M, V, O])
		}

		trait CaseComponent[+F <: FromClause, +Y[X]] extends MatchComponent[F, Y] {
			override def relation[T[A] <: TypedMapping[E, A], E, O]
			                     (e :SQLRelation[F, T, E, O]) :Y[E] =
				component(e)
		}
	}






	trait AbstractColumnComponentFormula[-F <: FromClause, T[A] <: MappingFrom[A], M[A] <: ColumnMapping[V, A], V, O]
		extends AbstractComponentFormula[F, T, M, O] with ColumnFormula[F, V]
	{
		override def extractor :ColumnMappingExtract[T[O]#Subject, V, O]

		//fixme: this should really use the selectForm, but it is not a ColumnForm due to buff excludes/const etc
		override def readForm :ColumnReadForm[V] = extractor.export.form

		override def upcast :ColumnFormula[F, Subject] = this
	}



	trait ColumnComponentFormula[-F <: FromClause, T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O]
		extends ComponentFormula[F, T, E, M, V, O] with AbstractColumnComponentFormula[F, T, M, V, O]
	{
		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[V] = matcher.component(this)

	}



	object ColumnComponentFormula {

		def apply[F <: FromClause, T[A] <: TypedMapping[E, A], E,
		          C <: ColumnMapping[_, _], M[A] <: ColumnMapping[V, A], V, O]
		         (from :SQLRelation[F, T, E, O], column :C)(implicit conforms :Conforms[C, M[O], ColumnMapping[V, O]])
				:ColumnComponentFormula[F, T, E, M, V, O] =
			new ProperColumn(from, column)



		def unapply[F <: FromClause, X](e :SQLFormula[F, X])
				:Option[(SQLRelation.AnyIn[F], ColumnMappingExtract[_, X, _])] =
			e match {
				case component :ColumnComponentFormula.* => Some((
					component.from.asInstanceOf[SQLRelation[F, MappingOf[Any]#TypedProjection, Any, Any]],
					component.extractor.asInstanceOf[ColumnMappingExtract[_, X, Any]]
				))
				case _ => None
			}



		type * = ColumnComponentFormula[_ <: FromClause, T, _, M, _, _] forSome {
			type T[A] <: TypedMapping[_, A]; type M[A] <: ColumnMapping[_, A]
		}

		type AnyIn[-F <: FromClause] = ColumnComponentFormula[F, T, E, M, V, _] forSome {
			type T[A] <: TypedMapping[E, A]; type E; type M[A] <: ColumnMapping[V, A]; type V
		}



		private class ProperColumn[-F <: FromClause, T[A] <: TypedMapping[E, A], M[A] <: ColumnMapping[V, A], E, V, O]
		                          (relation :SQLRelation[F, T, E, O], column :M[O])
			extends ProperComponent[F, T, E, M, V, O](relation, column)
			   with ColumnComponentFormula[F, T, E, M, V, O]
		{
			override val extractor = table(mapping)
			//fixme: sort out where the buff-related modifications take place to have consistent assembly semantics
			override val readForm = extractor.export.form

			override def stretch[U <: F, G <: FromClause](implicit ev :U ExtendedBy G) :ColumnFormula[G, V] =
				new ProperColumn[G, T, M, E, V, G](from.extend(ev), column.asInstanceOf[M[G]])
		}



		trait ColumnComponentMatcher[+F <: FromClause, +Y[X]] {
			def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O]
			             (e :ColumnComponentFormula[F, T, E, M, V, O]) :Y[V]
		}

		type MatchColumnComponent[+F <: FromClause, +Y[X]] = ColumnComponentMatcher[F, Y]

		type CaseColumnComponent[+F <: FromClause, +Y[X]] = ColumnComponentMatcher[F, Y]
	}






	sealed trait AbstractSQLRelation[-F <: FromClause, T[A] <: MappingFrom[A], O]
		extends AbstractComponentFormula[F, T, T, O]
	{
		def shift :Int

		override def from :AbstractSQLRelation[F, T, O] = this
		override def table :T[O] = mapping

		override def extractor :MappingExtract[Subject, Subject, O] =
			MappingExtract.ident(mapping.asInstanceOf[RefinedMapping[Subject, O]])

		def extend[S <: F, G <: FromClause](implicit extension :S ExtendedBy G) :AbstractSQLRelation[G, T, G]

//		def aliased[N <: Label](alias :N) :AbstractComponentFormula[F, (T As N)#T, T, O]

		override def toString :String = mapping.toString + "#" + shift

	}



	sealed trait SQLRelation[-F <: FromClause, T[A] <: TypedMapping[E, A], E, O]
		extends ComponentFormula[F, T, E, T, E, O] with AbstractSQLRelation[F, T, O]
	{
		override def from :SQLRelation[F, T, E, O] = this

		override def extend[S <: F, G <: FromClause](implicit extension :S ExtendedBy G) :SQLRelation[G, T, E, G]

//		override def aliased[N <: Label](alias :N) :ComponentFormula[F, (T As N)#T, E, T, E, O]
	}



	object SQLRelation {
//		def apply[F, T[A] <: TypedMapping[E, A], E](source :RowSource[M], shift :Int) :SQLRelation[F, T, E, O]


		type * = SQLRelation[_ <: FromClause, T, E, _] forSome { type T[O] <: TypedMapping[E, O]; type E }

		type AnyIn[-F <: FromClause] = SQLRelation[F, T, E, _] forSome { type T[O] <: TypedMapping[E, O]; type E }




		trait RelationMatcher[+F <: FromClause, +Y[X]] {
			def relation[T[A] <: TypedMapping[E, A], E, O](e :SQLRelation[F, T, E, O]) :Y[E]
		}

		type MatchRelation[+F <: FromClause, +Y[X]] = RelationMatcher[F, Y]

		type CaseRelation[+F <: FromClause, +Y[X]] = RelationMatcher[F, Y]
	}






	sealed trait JoinedRelation[F <: FromClause, T[A] <: MappingFrom[A]] extends AbstractSQLRelation[F, T, F] {

		def extend[M[A] <: MappingFrom[A]] :JoinedRelation[F With M, T]

		def extend[S <: F, E <: FromClause](implicit extension :S ExtendedBy E) :JoinedRelation[E, T]

//		private[sql] def aliased[N <: Label](alias :N)
//				:AbstractComponentFormula[FromClause With (T As N)#T, (T As N)#T, T, FromClause With (T As N)#T]
	}






	object JoinedRelation {

		private[sql] def apply[F <: FromClause, M[A] <: MappingFrom[A]](source :RowSource[M], index :Int)
				:JoinedRelation[F, M] =
			new TypedJoinedRelation[F, MappingOf[Any]#TypedProjection, Any](
				source.asInstanceOf[RowSource[MappingOf[Any]#TypedProjection]], index
			).asInstanceOf[JoinedRelation[F, M]]

//		private[sql] def apply[F <: FromClause, M[O] <: MappingFrom[O]](mapping :M[Any], index :Int) :JoinedRelation[F, M] =
//			new JoinedRelation(mapping, index)

		def unapply[F <: FromClause, X](f :SQLFormula[F, X]) :Option[(TypedMapping[X, _], Int)] = f match {
			case from :TypedJoinedRelation[_, _, _] =>
				Some(from.mapping.asInstanceOf[TypedMapping[X, _]] -> from.shift)
			case _ => None
		}



		type * = JoinedRelation[_ <: FromClause, T] forSome { type T[O] <: MappingFrom[O] }
		type AnyIn[F <: FromClause] = JoinedRelation[F, T] forSome { type T[O] <: MappingFrom[O] }

	}



	class TypedJoinedRelation[F <: FromClause, T[A] <: TypedMapping[E, A], E] private[sql]
	                         (override val source :RowSource[T], override val mapping :T[F], override val shift :Int)
		extends JoinedRelation[F, T] with SQLRelation[F, T, E, F]
	{
		def this(source :RowSource[T], shift :Int) = this(source, source[F], shift)

//		override def isGroundedIn(tables :Iterable[JoinedRelation.*]) :Boolean =
//			tables.exists(_ == this)

		override def applyTo[Y[_]](matcher :FormulaMatcher[F, Y]) :Y[E] =
			matcher.relation(this)


		//fixme: these must reuse this.mapping or the mapping won't recognize the components!
		override def stretch[M[A] <: MappingFrom[A]] :TypedJoinedRelation[F With M, T, E] =
			stretch[F, F With M]

		override def stretch[U <: F, G <: FromClause](implicit ev :U ExtendedBy G) :TypedJoinedRelation[G, T, E] =
			new TypedJoinedRelation[G, T, E](source, shift + ev.length)

		override def stretch[U <: F, G <: FromClause]
		                    (target :G)(implicit ev :U ExtendedBy G) :TypedJoinedRelation[G, T, E] =
			stretch[U, G]

		//todo: why did we need both stretch and extend?
		override def extend[M[A] <: MappingFrom[A]] :TypedJoinedRelation[F With M, T, E] =
			new TypedJoinedRelation[F With M, T, E](source, shift)

		override def extend[S <: F, G <: FromClause](implicit extension :ExtendedBy[S, G])
				:TypedJoinedRelation[G, T, E] =
			new TypedJoinedRelation(source, shift)

//		override private[sql] def aliased[N <: Label](alias :N)
//				:ComponentFormula[FromClause With (T As N)#T, (T As N)#T, E, T, E, FromClause With (T As N)#T] =
//		{
//			val source = FromClause.AliasedSource(this.source, alias)
//			val substitute = new TypedJoinedRelation[FromClause With (T As N)#T, (T As N)#T, E](source, 0)
//			ComponentFormula(substitute, mapping)
//		}

	}



	object TypedJoinedRelation {

		def apply[F <: FromClause, M[A] <: MappingFrom[A], T[A] <: TypedMapping[S, A], S]
		         (table :M[F])
		         (implicit cast :Conforms[M[F], T[F], TypedMapping[S, F]], shift :TableShift[F, M, _ <: Numeral],
		          alias :OriginProjection[T[F], F, T[Any], Any])
				:TypedJoinedRelation[F, T, S] =
			new TypedJoinedRelation[F, T, S](RowSource[T, F](cast(table))(alias), table, shift.tables)

		private[sql] def apply[F <: FromClause, T[A] <: TypedMapping[S, A], S](source :RowSource[T], index :Int)
				:TypedJoinedRelation[F, T, T[Any]#Subject] =
			new TypedJoinedRelation[F, T, T[Any]#Subject](source, index)

		private[sql] def apply[F <: FromClause, M[A] <: MappingFrom[A], T[A] <: TypedMapping[S, A], S]
		                      (from :F, source :RowSource[M], index :Int)
		                      (implicit inference :Conforms[M[Any], T[Any], TypedMapping[S, Any]],
		                       cast :RowSource[M] <:< RowSource[T]) :TypedJoinedRelation[F, T, S] =
			new TypedJoinedRelation[F, T, S](source, index)

		def last[M[A] <: MappingFrom[A], T[A] <: TypedMapping[S, A], S]
		        (source :RowSource[M])
		        (implicit inference :Conforms[M[Any], T[Any], TypedMapping[S, Any]]) :LastRelation[T, S] =
			new TypedJoinedRelation[FromClause With T, T, S](source.asInstanceOf[RowSource[T]], 0)

//		private[sql] def apply[F <: FromClause, M[O] <: MappingFrom[O]](mapping :M[Any], index :Int) :JoinedRelation[F, M] =
//			new JoinedRelation(mapping, index)



		def unapply[F <: FromClause, X](f :SQLFormula[F, X]) :Option[(TypedMapping[X, _], Int)] = f match {
			case from :TypedJoinedRelation[_, _, _] =>
				Some(from.mapping.asInstanceOf[TypedMapping[X, _]] -> from.shift)
			case _ => None
		}



		type * = TypedJoinedRelation[_ <: FromClause, T, E] forSome { type T[A] <: TypedMapping[E, A]; type E }

		type LastRelation[T[A] <: TypedMapping[S, A], S] = TypedJoinedRelation[FromClause With T, T, S]

		def LastRelation[T[A] <: TypedMapping[S, A], S](from :RowSource[T]) :LastRelation[T, S] =
			new TypedJoinedRelation[FromClause With T, T, S](from, 0)
	}






	trait MappingMatcher[+F <: FromClause, +Y[X]]
		extends ComponentMatcher[F, Y] with FreeComponentMatcher[F, Y] with MappingColumnMatcher[F, Y]

	trait MatchMapping[+F <: FromClause, +Y[X]]
		extends MappingMatcher[F, Y] with CaseComponent[F, Y] with CaseFreeComponent[F, Y]

	trait CaseMapping[+F <: FromClause, +Y[X]] extends MatchMapping[F, Y] {
		def mapping[M <: Mapping](e :MappingFormula[F, M]) :Y[M#Subject]

//		override def relation[S >: F <: FromClause, M[A] <: MappingFrom[A]](e :JoinedRelation[S, M]) :Y[M[S]#Subject] =
//			mapping(e)

		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O]
		                      (e :ComponentFormula[F, T, E, M, V, O]) :Y[V] =
			mapping(e)

		override def freeComponent[J >: F <: FromClause, M[A] <: TypedMapping[X, A], X](e :FreeComponent[J, M, X]) :Y[X] =
			mapping(e)
	}



	trait MappingColumnMatcher[+F <: FromClause, +Y[X]] extends FreeColumnMatcher[F, Y] with ColumnComponentMatcher[F, Y]

}



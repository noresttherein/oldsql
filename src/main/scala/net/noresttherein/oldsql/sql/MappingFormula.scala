package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf, MappingSubject, TypedMapping}
import net.noresttherein.oldsql.schema.{MappingExtract, Mapping, RowSource, SQLReadForm}
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.MappingFormula.ComponentFormula.ComponentMatcher
import net.noresttherein.oldsql.sql.MappingFormula.FreeComponent.FreeComponentMatcher
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.{AnyJoinedRelation, MatchRelation, RelationMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.{CompositeFormula, Formula, FormulaMatcher}




/**
  * @author Marcin Mościcki
  */
trait MappingFormula[-F <: FromClause, M <: Mapping] extends SQLFormula[F, M#Subject] { //todo:
	type Subject = M#Subject
	def upcast :SQLFormula[F, Subject] = this
	def mapping :M

//	def mapping(from :FromClause) :M
}






object MappingFormula {
//todo: subclasses for columns

	/** An expression evaluating to a component mapping of an undetermined at this point relation. It needs to be
	  * resolved by the `Origin` type of the component before the formula can be used.
	  */
	class FreeComponent[F <: FromClause, M[A] <: TypedMapping[X, A], X](val mapping :M[F], val shift :Int)
		extends MappingFormula[F, M[F]] with SQLFormula[F, M[F]#Subject]
	{
		type Origin = F

		override def readForm :SQLReadForm[Subject] = mapping.selectForm


		override def applyTo[Y[V]](matcher :FormulaMatcher[F, Y]) :Y[M[F]#Subject] =
			matcher.freeComponent(this)


		override def isGroundedIn(tables :Iterable[AnyJoinedRelation]) :Boolean = false

		override def isomorphic(expression :Formula[_]) :Boolean = ???

		override private[oldsql] def equivalent(expression :Formula[_]) = ???

		override def stretch[T[A] <: MappingFrom[A]] :FreeComponent[F With T, M, X] =
			new FreeComponent[F With T, M, X](mapping.asInstanceOf[M[F With T]], shift + 1)

		override def stretch[U <: FromClause, S <: FromClause]
		                    (implicit ev :U ExtendedBy S) :FreeComponent[S, M, X] =
			new FreeComponent[S, M, X](mapping.asInstanceOf[M[S]], shift + ev.length)

		override def stretch[U <: FromClause, S <: FromClause]
		                    (target :S)(implicit ev :U ExtendedBy S) :FreeComponent[S, M, X] =
			new FreeComponent[S, M, X](mapping.asInstanceOf[M[S]], shift + ev.length)

	}



	object FreeComponent {
		trait FreeComponentMatcher[+F <: FromClause, +Y[X]] {
			def freeComponent[O >: F <: FromClause, M[A] <: TypedMapping[X, A], X](e :FreeComponent[O, M, X]) :Y[X]
		}

		type MatchFreeComponent[+F <: FromClause, +Y[X]] = FreeComponentMatcher[F, Y]

		type CaseFreeComponent[+F <: FromClause, +Y[X]] = FreeComponentMatcher[F, Y]
	}






	trait ComponentFormula[-F <: FromClause, T[A] <: MappingFrom[A], M[A] <: MappingFrom[A], O]
		extends MappingFormula[F, M[O]] //with CompositeFormula[F, M[O]#Subject]
	{
		type Origin = O

		def from :JoinedRelation[_ >: F <: FromClause, T]
		def table :T[O] = from.mapping.asInstanceOf[T[O]]
		def source :RowSource[T] = from.source

		def mapping :M[O]
		def extractor :MappingExtract[T[O]#Subject, M[O]#Subject, O] //= table(mapping)

//		override def parts = from::Nil

		override def isGroundedIn(tables :Iterable[AnyJoinedRelation]) :Boolean =
			tables.exists(_ == from)


		override def isomorphic(expression :Formula[_]) :Boolean = ???
		override private[oldsql] def equivalent(expression :Formula[_]) = ???


//		override def stretch[C[A] <: MappingFrom[A]] :ComponentFormula[F With C, T, M, O] =
//			stretch[F, F With C]
//
//		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :ComponentFormula[S, T, M, O]
//
//		override def stretch[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :ComponentFormula[S, T, M, O] =
//			stretch[U, S]
	}



	object ComponentFormula {
		def apply[F <: FromClause, T[A] <: MappingFrom[A], M[A] <: MappingFrom[A]]
		         (from :JoinedRelation[F, T], component :M[F]) :ComponentFormula[F, T, M, F] =
			new ComponentFrom[MappingSubject[Any]#M, MappingSubject[Any]#M, Any, Any, F](
				from.asInstanceOf[JoinedRelation[F, MappingSubject[Any]#M]],
				component.asInstanceOf[MappingSubject[Any]#M[F]]
			).asInstanceOf[ComponentFormula[F, T, M, F]]



		def unapply[F <: FromClause, X](e :SQLFormula[F, X])
				:Option[(AnyJoinedRelation, MappingExtract[_, X, _])] =
			e match {
				case component :ComponentFormula[_, _, _, _] => Some((
					component.from.asInstanceOf[JoinedRelation[FromClause, MappingSubject[Any]#M]],
					component.extractor.asInstanceOf[MappingExtract[_, X, _]]
				))
				case _ => None
			}


		private class ComponentFrom[T[A] <: TypedMapping[E, A], M[A] <: TypedMapping[V, A], E, V, O <: FromClause]
		                           (relation :JoinedRelation[O, T], override val mapping :M[O])
			extends ComponentFormula[O, T, M, O]
		{

			override def from :JoinedRelation[O, T] = relation

			val extractor = table(mapping)

			override val readForm :SQLReadForm[V] = extractor.export.selectForm


			override def applyTo[Y[X]](matcher :FormulaMatcher[O, Y]) :Y[V] = matcher.component(this)

			override def stretch[U <: O, G <: FromClause](implicit ev :U ExtendedBy G) :SQLFormula[G, V] =
				new ComponentFrom[T, M, E, V, G](from.extend[U, G], mapping.asInstanceOf[M[G]])
		}



		trait ComponentMatcher[+F <: FromClause, +Y[X]] {
			def component[T[A] <: MappingFrom[A], M[A] <: MappingFrom[A], O]
			             (e :ComponentFormula[F, T, M, O]) :Y[M[O]#Subject]
		}

		type MatchComponent[+F <: FromClause, +Y[X]] = ComponentMatcher[F, Y]

		type CaseComponent[+F <: FromClause, +Y[X]] = ComponentMatcher[F, Y]
	}






	class JoinedRelation[F <: FromClause, T[A] <: MappingFrom[A]] private[sql]
                        (override val source :RowSource[T], override val mapping :T[F], val shift :Int)
		extends ComponentFormula[F, T, T, F]
	{
		private[sql] def this(source :RowSource[T], index :Int) = this(source, source[F], index)

		override def from :JoinedRelation[F, T] = this
		override def table :T[F] = mapping
		override val extractor = MappingExtract.ident(table.asInstanceOf[TypedMapping[T[F]#Subject, F]])

		override def readForm :SQLReadForm[Subject] = mapping.selectForm

		def /[M[A] <: MappingFrom[A]](component :T[F] => M[F]) :ComponentFormula[F, T, M, F] =
			ComponentFormula(this, component(mapping))

		override def applyTo[Y[X]](matcher :FormulaMatcher[F, Y]) :Y[Subject] = matcher.relation(this)

//		override def isGroundedIn(tables :Iterable[JoinedRelation[_, m forSome { type m[W] <: MappingFrom[W] }]]) :Boolean =
//			tables.exists(_ == this)


//		override def stretch[M[A] <: MappingFrom[A]] :JoinedRelation[F With M, T] =
//			stretch[F, F With M]
//
//		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :JoinedRelation[S, T] =
//			new JoinedRelation[S, T](source, shift + ev.length)
//
//		override def stretch[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :JoinedRelation[S, T] =
//			stretch[U, S]

//
//		override def isomorphic(expression :Formula[_]) :Boolean = ???
//
//		override private[oldsql] def equivalent(expression :Formula[_]) = ???
		//fixme: we *must* reuse the table mapping instance here, as otherwise it will not recognize the components!
		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :SQLFormula[S, T[F]#Subject] =
			new JoinedRelation[S, T](source, shift + ev.length).asInstanceOf[SQLFormula[S, T[F]#Subject]]



		def extend[M[O] <: MappingFrom[O]] :JoinedRelation[F With M, T] =
			new JoinedRelation(source, shift + 1)

		def extend[S <: F, E <: FromClause](implicit extension :S ExtendedBy E) :JoinedRelation[E, T] =
			new JoinedRelation(source, shift + extension.length)

		override def toString :String = "FROM " + mapping + " #" + shift
	}



	object JoinedRelation {
		private[sql] def apply[F <: FromClause, M[A] <: MappingFrom[A]](source :RowSource[M], index :Int)
				:JoinedRelation[F, M] =
			new JoinedRelation(source, index)

//		private[sql] def apply[F <: FromClause, M[O] <: MappingFrom[O]](mapping :M[Any], index :Int) :JoinedRelation[F, M] =
//			new JoinedRelation(mapping, index)

		def unapply[F <: FromClause, X](f :SQLFormula[F, X]) :Option[(MappingOf[X], Int)] = f match {
			case from :JoinedRelation[_, _] => Some(from.mapping.asInstanceOf[MappingOf[X]] -> from.shift)
			case _ => None
		}



		type LastRelation[M[A] <: MappingFrom[A]] = JoinedRelation[FromClause With M, M]

		def LastRelation[M[A] <: MappingFrom[A]](from :RowSource[M]) :LastRelation[M] =
			new JoinedRelation[FromClause With M, M](from, 0)

		private[sql] def LastRelation[M[A] <: MappingFrom[A]]
		                             (from :RowSource[M], mapping :M[FromClause With M]) =
			new LastRelation[M](from, mapping, 0)


		type AnyJoinedRelation = JoinedRelation[_ <: FromClause, T] forSome { type T[O] <: MappingFrom[O] }
		type AnyRelationIn[F <: FromClause] = JoinedRelation[F, T] forSome { type T[O] <: MappingFrom[O] }

		trait RelationMatcher[+F <: FromClause, +Y[X]] {
			def relation[O >: F <: FromClause, M[A] <: MappingFrom[A]](e :JoinedRelation[O, M]) :Y[M[O]#Subject]
		}

		type MatchRelation[+F <: FromClause, +Y[X]] = RelationMatcher[F, Y]

		type CaseRelation[+F <: FromClause, +Y[X]] = RelationMatcher[F, Y]
	}







	trait MappingMatcher[+F <: FromClause, +Y[X]]
		extends RelationMatcher[F, Y] with ComponentMatcher[F, Y] with FreeComponentMatcher[F, Y]

	type MatchMapping[+F <: FromClause, +Y[X]] = MappingMatcher[F, Y]

	trait CaseMapping[+F <: FromClause, +Y[X]] extends MatchMapping[F, Y] {
		def mapping[M <: Mapping](e :MappingFormula[F, M]) :Y[M#Subject]

		override def relation[S >: F <: FromClause, M[A] <: MappingFrom[A]](e :JoinedRelation[S, M]) :Y[M[S]#Subject] =
			mapping(e)

		override def component[T[A] <: MappingFrom[A], M[A] <: MappingFrom[A], O]
		                      (e :ComponentFormula[F, T, M, O]) :Y[M[O]#Subject] =
			mapping(e)

		override def freeComponent[J >: F <: FromClause, M[A] <: TypedMapping[X, A], X](e :FreeComponent[J, M, X]) :Y[X] =
			mapping(e)
	}

}



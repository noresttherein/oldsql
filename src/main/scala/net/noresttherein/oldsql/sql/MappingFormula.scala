package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.{ComponentExtractor, Mapping, RowSource, SQLReadForm}
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.MappingFormula.ComponentFormula.ComponentMatcher
import net.noresttherein.oldsql.sql.MappingFormula.FreeComponent.FreeComponentMatcher
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.{AnyJoinedRelation, MatchRelation, RelationMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.{Formula, FormulaMatcher}


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


	/** An expression evaluating to a component mapping of an undetermined at this point relation. It needs to be
	  * resolved by the `Origin` type of the component before the formula can be used.
	  */
	class FreeComponent[M[A] <: MappingFrom[A], O](val mapping :M[O], val offset :Int)
		extends MappingFormula[FromClause, M[O]]
	{
		type Origin = O

		override def readForm :SQLReadForm[Subject] = mapping.selectForm


		override def applyTo[Y[+X]](matcher :FormulaMatcher[FromClause, Y]) :Y[M[O]#Subject] =
			matcher.freeComponent(this)


		override def isGroundedIn(tables :Iterable[AnyJoinedRelation]) :Boolean = false

		override def isomorphic(expression :Formula[_]) :Boolean = ???

		override private[oldsql] def equivalent(expression :Formula[_]) = ???
	}



	object FreeComponent {
		trait FreeComponentMatcher[+F <: FromClause, +Y[X]] {
			def freeComponent[M[A] <: MappingFrom[A], O](component :FreeComponent[M, O]) :Y[M[O]#Subject]
		}

		type MatchFreeComponent[+F <: FromClause, +Y[X]] = FreeComponentMatcher[F, Y]

		type CaseFreeComponent[+F <: FromClause, +Y[X]] = FreeComponentMatcher[F, Y]
	}






	trait ComponentFormula[-F <: FromClause, T[A] <: MappingFrom[A], M[A] <: MappingFrom[A], O]
		extends MappingFormula[F, M[O]]
	{
		type Origin = O

		def from :JoinedRelation[F, T, O]
		def table :T[O] = from.mapping.asInstanceOf[T[O]]
		def source :RowSource[T] = from.source

		def mapping :M[O]
		def extractor :ComponentExtractor[T[O]#Subject, M[O]#Subject, O] //= table(mapping)

		override def isGroundedIn(tables :Iterable[AnyJoinedRelation]) :Boolean =
			tables.exists(_ == from)


		override def isomorphic(expression :Formula[_]) :Boolean = ???
		override private[oldsql] def equivalent(expression :Formula[_]) = ???

	}



	object ComponentFormula {
		def apply[F <: FromClause, T[A] <: MappingFrom[A], M[A] <: MappingFrom[A], O]
		         (from :JoinedRelation[F, T, O], component :M[O]) :ComponentFormula[F, T, M, O] = ???
//			new ComponentFrom[F, ]



		def unapply[F <: FromClause, X](f :SQLFormula[F, X])
				:Option[(JoinedRelation[F, M, O], ComponentExtractor[E, X, O])]
					forSome { type M[A] <: TypedMapping[E, A]; type E; type O } =
			???


		private class ComponentFrom[-F <: FromClause, T[A] <: TypedMapping[E, A], M[A] <: TypedMapping[S, A], E, S, O]
		                           (override val from :JoinedRelation[F, T, O], override val mapping :M[O])
			extends ComponentFormula[F, T, M, O]
		{
			val extractor = table(mapping)

			override val readForm :SQLReadForm[S] = extractor.export.selectForm

			override def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[S] = matcher.component(this)

		}



		trait ComponentMatcher[+F <: FromClause, +Y[X]] {
			def component[T[A] <: MappingFrom[A], M[A] <: MappingFrom[A], O]
			             (f :ComponentFormula[F, T, M, O]) :Y[M[O]#Subject]
		}

		type MatchComponent[+F <: FromClause, +Y[X]] = ComponentMatcher[F, Y]

		type CaseComponent[+F <: FromClause, +Y[X]] = ComponentMatcher[F, Y]
	}






	class JoinedRelation[-F <: FromClause, T[A] <: MappingFrom[A], O] private[sql]
                        (override val source :RowSource[T], override val mapping :T[O], val index :Int)
		extends ComponentFormula[F, T, T, O]
	{
		private[sql] def this(source :RowSource[T], index :Int) = this(source, source[O], index)

		override def from :JoinedRelation[F, T, O] = this
		override def table :T[O] = mapping
		override val extractor = ComponentExtractor.ident(table.asInstanceOf[TypedMapping[T[O]#Subject, O]])

		override def readForm :SQLReadForm[Subject] = mapping.selectForm



		override def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[Subject] = matcher.relation(this)

//		override def isGroundedIn(tables :Iterable[JoinedRelation[_, m forSome { type m[W] <: MappingFrom[W] }]]) :Boolean =
//			tables.exists(_ == this)

		override def asPartOf[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :JoinedRelation[S, T, O] =
			ev(this)
//			new JoinedRelation[S, M](from, )

		override def asPartOf[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :JoinedRelation[S, T, O] =
			ev(this)
//
//		override def isomorphic(expression :Formula[_]) :Boolean = ???
//
//		override private[oldsql] def equivalent(expression :Formula[_]) = ???

		override def toString :String = "FROM " + mapping + " #" + index
	}



	object JoinedRelation {
		private[sql] def apply[F <: FromClause, M[A] <: MappingFrom[A], O](source :RowSource[M], index :Int)
				:JoinedRelation[F, M, O] =
			new JoinedRelation(source, index)

//		private[sql] def apply[F <: FromClause, M[O] <: MappingFrom[O]](mapping :M[Any], index :Int) :JoinedRelation[F, M] =
//			new JoinedRelation(mapping, index)

		def unapply[F <: FromClause, X](f :SQLFormula[F, X]) :Option[(MappingOf[X], Int)] = f match {
			case from :JoinedRelation[_, _, _] => Some(from.mapping.asInstanceOf[MappingOf[X]] -> from.index)
			case _ => None
		}



		type LastRelation[M[A] <: MappingFrom[A], O] = JoinedRelation[FromClause With M, M, O]

		def LastRelation[M[A] <: MappingFrom[A], O](from :RowSource[M], index :Int) :LastRelation[M, O] =
			new JoinedRelation[FromClause With M, M, O](from, index)

		private[sql] def LastRelation[M[A] <: MappingFrom[A], O](from :RowSource[M], mapping :M[O], index :Int) =
			new LastRelation[M, O](from, mapping, index)


		type AnyJoinedRelation = JoinedRelation[_ <: FromClause, T, _] forSome { type T[O] <: MappingFrom[O] }
		type AnyRelationIn[-F <: FromClause] = JoinedRelation[F, T, _] forSome { type T[O] <: MappingFrom[O] }

		trait RelationMatcher[+F <: FromClause, +Y[X]] {
			def relation[M[A] <: MappingFrom[A], O](f :JoinedRelation[F, M, O]) :Y[M[O]#Subject]
		}

		type MatchRelation[+F <: FromClause, +Y[X]] = RelationMatcher[F, Y]

		type CaseRelation[+F <: FromClause, +Y[X]] = RelationMatcher[F, Y]
	}







	trait MappingMatcher[+F <: FromClause, +Y[X]]
		extends RelationMatcher[F, Y] with ComponentMatcher[F, Y] with FreeComponentMatcher[F, Y]

	type MatchMapping[+F <: FromClause, +Y[X]] = MappingMatcher[F, Y]

	trait CaseMapping[+F <: FromClause, +Y[X]] extends MatchMapping[F, Y] {
		def mapping[M <: Mapping](f :MappingFormula[F, M]) :Y[M#Subject]

		override def relation[M[A] <: MappingFrom[A], O](f :JoinedRelation[F, M, O]) :Y[M[O]#Subject] =
			mapping(f)

		override def component[T[A] <: MappingFrom[A], M[A] <: MappingFrom[A], O]
		                      (f :ComponentFormula[F, T, M, O]) :Y[M[O]#Subject] =
			mapping(f)

		override def freeComponent[M[A] <: MappingFrom[A], O](f :FreeComponent[M, O]) :Y[M[O]#Subject] =
			mapping(f)
	}

}



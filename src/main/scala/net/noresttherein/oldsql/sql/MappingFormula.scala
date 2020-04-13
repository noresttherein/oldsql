package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, TypedMapping, ComponentExtractor, MappingOf}
import net.noresttherein.oldsql.schema.{Mapping, RowSource, SQLReadForm}
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.MappingFormula.ComponentFormula.ComponentMatcher
import net.noresttherein.oldsql.sql.MappingFormula.FromFormula.{FromMatcher, MatchFrom}
import net.noresttherein.oldsql.sql.SQLFormula.{Formula, FormulaMatcher}


/**
  * @author Marcin Mościcki
  */
trait MappingFormula[-F <: FromClause, M <: Mapping] extends SQLFormula[F, M#Subject] { //todo:
	type Subject = M#Subject
	private[sql] def self :SQLFormula[F, M#Subject] = this
	def mapping :M
}






object MappingFormula {


	trait ComponentFormula[-F <: FromClause, T <: Mapping, M <: Mapping] extends MappingFormula[F, M] {
		def from :FromFormula[F, T]
		def table :T = from.mapping
		def mapping :M
		def extractor :ComponentExtractor[T#Subject, M#Subject, _] //= table(mapping)

		override def isGroundedIn(tables :Iterable[FromFormula[_, _]]) :Boolean =
			tables.exists(_ == table)


		override def isomorphic(expression :Formula[_]) :Boolean = ???
		override private[oldsql] def equivalent(expression :Formula[_]) = ???

	}



	object ComponentFormula {
		def apply[F <: FromClause, T <: Mapping, M <: Mapping]
		         (from :FromFormula[F, T], component :M) :ComponentFormula[F, T, M] = ???
//			new ComponentFrom[F, ]



		def unapply[F <: FromClause, T](f :SQLFormula[F, T])
				:Option[(FromFormula[F, M], ComponentExtractor[E, T, _])] forSome { type M <: MappingOf[E]; type E } =
			???


		private class ComponentFrom[-F <: FromClause, T <: TypedMapping[E, O], M <: TypedMapping[S, O], E, S, O]
		                           (override val from :FromFormula[F, T], val mapping :M)
			extends ComponentFormula[F, T, M]
		{
			val extractor = table(mapping)

			override val readForm :SQLReadForm[S] = extractor.lifted.selectForm

			override def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[S] = matcher.component[T, M](this)

		}



		trait ComponentMatcher[+F <: FromClause, +Y[X]] {
			def component[T <: Mapping, M <: Mapping](f :ComponentFormula[F, T, M]) :Y[M#Subject]
		}

		type MatchComponent[+F <: FromClause, +Y[X]] = ComponentMatcher[F, Y]

		type CaseComponent[+F <: FromClause, +Y[X]] = ComponentMatcher[F, Y]
	}






//	class FromFormula[-F <: FromClause, M[O] <: MappingFrom[O]] private[sql] (val mapping :M[Any], val index :Int)
	class FromFormula[-F <: FromClause, M <: Mapping] private[sql] (val mapping :M, val index :Int)
		extends ComponentFormula[F, M, M]
	{
//		type Origin
//		type Kind[O] <: MappingFrom[O]
//		def this(source :RowSource[M], index :Int) = this(source[Any], index)
		override def from :FromFormula[F, M] = this
//		override val mapping :M = source[Any]
		override def table :M = mapping
		override val extractor = ComponentExtractor.ident(table.asInstanceOf[TypedMapping[M#Subject, Any]])

		override def readForm :SQLReadForm[Subject] = mapping.selectForm

		override def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[Subject] = matcher.from[M](this)

//		override def isGroundedIn(tables :Iterable[FromFormula[_, m forSome { type m[O] <: MappingFrom[O] }]]) :Boolean =
//			tables.exists(_ == this)

		override def asPartOf[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :FromFormula[S, M] =
			ev(this)
//			new FromFormula[S, M](from, )

		override def asPartOf[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :FromFormula[S, M] =
			ev(this)
//
//		override def isomorphic(expression :Formula[_]) :Boolean = ???
//
//		override private[oldsql] def equivalent(expression :Formula[_]) = ???

		override def toString :String = "FROM " + mapping + " #" + index
	}



	object FromFormula {
		private[sql] def apply[F <: FromClause, M[A] <: MappingFrom[A], O](source :RowSource[M], index :Int) :FromFormula[F, M[O]] =
			new FromFormula(source[O], index)

//		private[sql] def apply[F <: FromClause, M[O] <: MappingFrom[O]](mapping :M[Any], index :Int) :FromFormula[F, M] =
//			new FromFormula(mapping, index)

		def unapply[F <: FromClause, X](f :SQLFormula[F, X]) :Option[(Mapping, Int)] = f match {
			case from :FromFormula[_, _] => Some(from.mapping -> from.index)
			case _ => None
		}



		trait FromMatcher[+F <: FromClause, +Y[X]] {
			def from[M <: Mapping](f :FromFormula[F, M]) :Y[M#Subject]
		}

		type MatchFrom[+F <: FromClause, +Y[X]] = FromMatcher[F, Y]

		type CaseFrom[+F <: FromClause, +Y[X]] = FromMatcher[F, Y]
	}



	type FromLast[M <: Mapping] = FromFormula[FromClause Join M, M]

	def FromLast[M[A] <: MappingFrom[A], O](from :RowSource[M], index :Int) = new FromLast[M[O]](from[O], index)

	def FromLast[M <: Mapping](from :M, index :Int) = new FromLast(from, index)




	trait MappingMatcher[+F <: FromClause, +Y[X]] extends FromMatcher[F, Y] with ComponentMatcher[F, Y]

	type MatchMapping[+F <: FromClause, +Y[X]] = MappingMatcher[F, Y]

	trait CaseMapping[+F <: FromClause, +Y[X]] extends MatchMapping[F, Y] {
		def mapping[M <: Mapping](f :MappingFormula[F, M]) :Y[M#Subject]

		override def from[M <: Mapping](f :FromFormula[F, M]) :Y[M#Subject] =
			mapping(f)

		override def component[T <: Mapping, M <: Mapping](f :ComponentFormula[F, T, M])  :Y[M#Subject] =
			mapping(f)
	}

}
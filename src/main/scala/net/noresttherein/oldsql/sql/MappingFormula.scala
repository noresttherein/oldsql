package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, ComponentExtractor}
import net.noresttherein.oldsql.schema.{RowSource, SQLReadForm}
import net.noresttherein.oldsql.sql.MappingFormula.ComponentFormula.ComponentMatcher
import net.noresttherein.oldsql.sql.MappingFormula.FromFormula.{FromMatcher, MatchFrom}
import net.noresttherein.oldsql.sql.SQLFormula.{Formula, FormulaMatcher}


/**
  * @author Marcin Mościcki
  */
trait MappingFormula[-F <: FromClause, M[O] <: AnyComponent[O]] extends SQLFormula[F, M[Any]#Subject] { //todo:
	type Subject = M[Any]#Subject
	private[sql] def self :SQLFormula[F, M[Any]#Subject] = this
}






object MappingFormula {


	trait ComponentFormula[-F <: FromClause, T[O] <: AnyComponent[O], M[O] <: AnyComponent[O]] extends MappingFormula[F, M] {
//		type Origin
//		val from :RowSource[T]
		def from :FromFormula[F, T]
		def table :T[Any] = from.mapping
		def mapping :M[Any]
		def extractor :ComponentExtractor[T[Any]#Subject, M[Any]#Subject, Any] //= table(mapping)

		override def isGroundedIn(tables :Iterable[FromFormula[_, (m) forSome {type m[O] <: AnyComponent[O]}]]) :Boolean =
			tables.exists(_ == table)


		override def isomorphic(expression :Formula[_]) :Boolean = ???
		override private[oldsql] def equivalent(expression :Formula[_]) = ???

	}



	object ComponentFormula {
		def apply[F <: FromClause, T[O] <: AnyComponent[O], M[O] <: AnyComponent[O]]
		         (from :FromFormula[F, T], component :M[_]) :ComponentFormula[F, T, M] = ???
//			new ComponentFrom[F, ]



		def unapply[F <: FromClause, T](f :SQLFormula[F, T])
				:Option[(FromFormula[F, M], ComponentExtractor[E, T, _])] forSome { type M[O] <: Component[E, O]; type E } =
			???


		private class ComponentFrom[-F <: FromClause, T[O] <: Component[E, O], M[O] <: Component[S, O], E, S]
		                           (override val from :FromFormula[F, T], val mapping :M[Any])
			extends ComponentFormula[F, T, M]
		{
			val extractor = table(mapping)

			override val readForm :SQLReadForm[S] = extractor.lifted.selectForm

			override def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[S] = matcher.component[T, M](this)

		}



		trait ComponentMatcher[+F <: FromClause, +Y[X]] {
			def component[T[O] <: AnyComponent[O], M[O] <: AnyComponent[O]](f :ComponentFormula[F, T, M]) :Y[M[Any]#Subject]
		}

		type MatchComponent[+F <: FromClause, +Y[X]] = ComponentMatcher[F, Y]

		type CaseComponent[+F <: FromClause, +Y[X]] = ComponentMatcher[F, Y]
	}






//	class FromFormula[-F <: FromClause, M[O] <: AnyComponent[O]] private[sql] (val mapping :M[Any], val index :Int)
	class FromFormula[-F <: FromClause, M[O] <: AnyComponent[O]] private[sql] (val source :RowSource[M], val index :Int)
		extends ComponentFormula[F, M, M]
	{
//		def this(source :RowSource[M], index :Int) = this(source[Any], index)
		override def from :FromFormula[F, M] = this
		override val mapping :M[Any] = source[Any]
		override def table :M[Any] = mapping
		override val extractor = ComponentExtractor.ident(table.asInstanceOf[Component[M[Any]#Subject, Any]])

		override def readForm :SQLReadForm[Subject] = mapping.selectForm

		override def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[Subject] = matcher.from(this)

//		override def isGroundedIn(tables :Iterable[FromFormula[_, m forSome { type m[O] <: AnyComponent[O] }]]) :Boolean =
//			tables.exists(_ == this)

//		override def asPartOf[U <: F, S <: FromClause](implicit ev :FromClause.ExtendedBy[U, S]) :FromFormula[S, M] =
//			new FromFormula[S, M](from, )

//		override def asPartOf[U <: F, S <: FromClause](target :S)(implicit ev :FromClause.ExtendedBy[U, S]) :SQLFormula[S, M[Any]#Subject] =
//
//		override def isomorphic(expression :Formula[_]) :Boolean = ???
//
//		override private[oldsql] def equivalent(expression :Formula[_]) = ???

		override def toString :String = "FROM " + mapping + " #" + index
	}



	object FromFormula {
		private[sql] def apply[F <: FromClause, M[O] <: AnyComponent[O]](source :RowSource[M], index :Int) :FromFormula[F, M] =
			new FromFormula(source, index)

//		private[sql] def apply[F <: FromClause, M[O] <: AnyComponent[O]](mapping :M[Any], index :Int) :FromFormula[F, M] =
//			new FromFormula(mapping, index)

		def unapply[F <: FromClause, X](f :SQLFormula[F, X]) :Option[(AnyComponent[Any], Int)] = f match {
			case from :FromFormula[_, AnyComponent] => Some(from.mapping -> from.index)
			case _ => None
		}



		trait FromMatcher[+F <: FromClause, +Y[X]] {
			def from[M[O] <: AnyComponent[O]](f :FromFormula[F, M]) :Y[M[Any]#Subject]
		}

		type MatchFrom[+F <: FromClause, +Y[X]] = FromMatcher[F, Y]

		type CaseFrom[+F <: FromClause, +Y[X]] = FromMatcher[F, Y]
	}



	type FromTable[M[O] <: AnyComponent[O]] = FromFormula[FromClause Join M, M]

	def FromTable[M[O] <: AnyComponent[O]](from :RowSource[M], index :Int) = new FromTable[M](from, index)





	trait MappingMatcher[+F <: FromClause, +Y[X]] extends FromMatcher[F, Y] with ComponentMatcher[F, Y]

	type MatchMapping[+F <: FromClause, +Y[X]] = MappingMatcher[F, Y]

	trait CaseMapping[+F <: FromClause, +Y[X]] extends MatchMapping[F, Y] {
		def mapping[M[O] <: AnyComponent[O]](f :MappingFormula[F, M]) :Y[M[Any]#Subject]

		override def from[M[O] <: AnyComponent[O]](f :FromFormula[F, M]) :Y[M[Any]#Subject] =
			mapping(f)

		override def component[T[O] <: AnyComponent[O], M[O] <: AnyComponent[O]](f :ComponentFormula[F, T, M])  :Y[M[Any]#Subject] =
			mapping(f)
	}

}
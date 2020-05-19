package com.hcore.ogre.hoard

/*


import com.hcore.ogre.mapping.{Mapping, Mapping}
import com.hcore.ogre.model.Restriction
import com.hcore.ogre.model.Restriction.{Restrictive, Restrainer}
import com.hcore.ogre.morsels.necromancy.PropertyPath
import com.hcore.ogre.morsels.necromancy.PropertyPath.===>
import com.hcore.ogre.sql.RowSource.TableFormula
import com.hcore.ogre.sql.SQLFormula.SelectFormula.{SelectAsRow, SelectAsRows}
import com.hcore.ogre.sql.SQLFormula._
import com.hcore.ogre.sql.SQLVisitor.Fixed
import com.hcore.ogre.sql._
import shapeless.HList

import scala.collection.Set


class JoinOpressor[S<:RowSource Join FK, FK<:Mapping[K], E, K](
		join :S, references :TableFormula[S, _]=>PropertyPath[E, _])
	extends Restrainer[K, E]
{
	override def apply(key: K): Restriction[E] =
		RestrictionBuilder(join.substitute(key).filter) match {
			case RestrictionExpression(r) => r
			case r => throw new IllegalArgumentException(s"Failed to create restriction for key $key based on $join. Got $r.")
		}

	override def from(value: E): Option[K] = ???

	override def from(restriction: Restriction[_ <: E]): Option[K] = ???

	override def in: Restrainer[Set[K], E] = ???

	override def like(value: E): Restriction[E] = ???



	

	private trait RestrictionElement[T]


	private case class RestrictionExpression(restriction :Restriction[E]) extends RestrictionElement[Boolean]

	private case class RestrictiveExpression[T](restrictive :Restrictive[E, T]) extends RestrictionElement[T]
	
	private case class RestrictiveSeq[T](elements :Seq[Restrictive[E, T]]) extends RestrictionElement[Seq[T]]

	private object RestrictionBuilder extends GenericSQLTermVisitor[S, RestrictionElement] {

		override def option[X](e: OrNull[S, X]): RestrictionElement[Option[X]] = apply(e.expr) match {
			case RestrictiveExpression(Restrictive.Literal(v)) =>
				literalResult(Some(v))
			case x => error(e, x)
		}

		override def equality[X](e: Equality[S, X]): RestrictionElement[Boolean] = 
			(apply(e.left), apply(e.right)) match {
				case (RestrictiveExpression(l), RestrictiveExpression(r)) => 
					result(Restriction.Equality(l, r))
				case (l, r) => error(e, (l, r))
			}

		override def seq[X](e: SeqFormula[S, X]): RestrictionElement[Seq[X]] =
			RestrictiveSeq(e.parts.map(apply).map {
				case r :Restrictive[_, _] => r.asInstanceOf[Restrictive[E, X]]
				case x => error(e, x)
			})

		override def sqlNull[X](e: Null[X]): RestrictionElement[X] = error(e, None)

		override def or(e: Or[S]): RestrictionElement[Boolean] = 
			RestrictionExpression(Restriction.Disjunction(e.conditions.map(apply).map {
				case RestrictionExpression(r) => r
				case x => error(e, x)
			}))

		override def native[X](e: NativeTerm[X]): RestrictionElement[X] = error(e, None)

		override def nonbool[X](e: Literal[X]): RestrictionElement[X] = literalResult(e.value)
		
		override def and(e: And[S]): RestrictionElement[Boolean] = 
		    result(Restriction.Conjunction(e.conditions.map(apply).map {
			    case RestrictionExpression(r) => r
			    case x => error(e, x)
		    }))


		override def not(e: NotFormula[S]): RestrictionElement[Boolean] =
			error(e, e)

		override def parameter[X](e: BoundParameter[X]): RestrictionElement[X] = literalResult(e.value)


//		override def parameter[X](e: UnboundParameter[X]): RestrictionElement[X] =
//			error(e, None)

		override def in[X](e: In[S, X]): RestrictionElement[Boolean] = (apply(e.left), apply(e.right)) match {
			case (RestrictiveExpression(l), RestrictiveExpression(Restrictive.Literal(r))) =>
				RestrictionExpression(l in r.map(Restrictive.Literal(_)))
			case (RestrictiveExpression(l), RestrictiveSeq(r)) =>
				RestrictionExpression(l in r)
			case x => error(e, x)
		} 

		override def tuple[X <: HList](e: HListFormula[S, X]): RestrictionElement[X] =
			RestrictiveSeq(e.parts.map(apply(_)).map {
				case r :Restrictive[_, _] => r.asInstanceOf[Restrictive[E, X]]
				case x => error(e, x)
			}).asInstanceOf[RestrictionElement[X]] //I don't care!


		override def bool(e: Literal[Boolean]): RestrictionElement[Boolean] = literalResult(e.value)

		override def path[M <: Mapping, C <: Mapping](e: PathFormula[S, M, C]): RestrictionElement[C#ResultType] =
			e match {
				case ComponentFormula(last, component) if component.surepick.isDefined =>
					propertyResult(references(last).andThen(component.surepick.get.asInstanceOf[Any=>C#ResultType]))
				case _ => error(e, None)
			}

		override def select[H](e: SelectFormula[S, H]) = error(e, e)

		override def exists[H](e: ExistsFormula[S, H]): RestrictionElement[Boolean] = error(e, e)

		override def row[H](e: SelectAsRow[S, H]) = error(e, e)

		override def rows[H](e: SelectAsRows[S, H]) = error(e, e)
	}

	private def propertyResult[X](property :PropertyPath[E, X]) = RestrictiveExpression(Restrictive.Property(property))
	private def literalResult[X](value :X) = RestrictiveExpression(Restrictive.Literal(value))
	private def result(restriction :Restriction[E]) = RestrictionExpression(restriction)
	
//	private def property[X]()
	
	private def error(expr :SQLFormula[join.type, _], value :Any) :Nothing =
		throw new IllegalArgumentException(s"Can't translate $expr($value) into a reference restriction")

}




object JoinOpressor {
	def Restriction[S<:RowSource Join FK, FK<:Mapping[K], E, K](join :S, references :TableFormula[S, _]=>PropertyPath[E, _]) :Restriction[E] =
		???
	

}
*/

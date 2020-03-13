package com.hcore.ogre.sql

import com.hcore.ogre.mapping.{ComponentPath, Mapping, AnyMapping}
import com.hcore.ogre.model.Reference.{RestrictedReferenceFactory, Full, HigherKindReferenceFactory, GenericReferenceFactory}
import com.hcore.ogre.model.Restriction.Opressor
import com.hcore.ogre.model.{Restriction, Reference, ComposedOf}
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.RowSource.TableFormula
import com.hcore.ogre.sql.SQLFormula.ComponentFormula

import scala.collection.Set


//implicits
import extensions._
import SaferCasts._

/*


trait JoinReference[X, E, K] extends SQLReference[X, E] {
	type ForeignKey <: Mapping[K]
	type JoinSource = Source Join ForeignKey
//	type TargetTable <: Mapping

	val joinSource :JoinSource
	val target :ComponentExpression[Source, _<:Mapping, TargetMapping]
//	val foreignKey :JoinedTable[Source, ForeignKey]
	val key :K

	override def select: Select[Source, TargetMapping] = Select(joinSource.parameterize(key), target)
}






object JoinReference {

	def apply[S<:RowSource, M<:Mapping[E], FK<:Mapping[K], X, E, K](
			join :S Join FK, target :ComponentExpression[S, _<:Mapping, M], key :K, toOpt :Option[X]=None)
			(implicit composedOf :X ComposedOf E)
	:JoinReference[X, E, K] =
		new TypedJoinReference[S, M, FK, X, E, K](join, target, key, toOpt)


//	def restriction[S <:RowSource Join FK, FK<:Mapping[K]]


	class TypedJoinReference[S<:RowSource, M<:Mapping[E], FK<:Mapping[K], X, E, K](
			val joinSource :S Join FK, val target :ComponentExpression[S, _<:Mapping, M], val key :K, value : =>Option[X]=None)
			(implicit val items :X ComposedOf E)
		extends JoinReference[X, E, K]
	{
		type Source = S
		type ForeignKey = FK
		type TargetMapping = M

		def toOpt = value

		override def canEqual(that :Any) =  that.isInstanceOf[JoinReference[_, _, _]]

		override def equals(that :Any) = that match {
			case j :JoinReference[_,_,_] =>
				(j eq this) || (j.canEqual(this) && j.key==key && j.target==target && j.joinSource==joinSource)
			case _ => false
		}

		override def toString = s"Join[$target=$key]" + toOpt.map(v => s"($v)").getOrElse("")
	}

	object TypedJoinReference {
		def unapply[X](reference :Reference[X]) =
			reference.ifSubclass[TypedJoinReference[RowSource, Mapping[Any], Mapping[Any], X, Any, Any]] {
				r => (r.joinSource, r.target, r.key, r.toOpt)
			}

	}





	class JoinReferenceFactory[S<:RowSource, T<:Mapping[O], M<:Mapping[E], FK<:Mapping[K], O, X, E, K](
			join :S Join FK, target :ComponentExpression[S, T, M], key :E=>Option[K])(implicit val items :X ComposedOf E)
		extends HigherKindReferenceFactory[K, E, X, Reference]
	{
		override def delayed(key: K, value: => X): Reference[X] =
			new TypedJoinReference[S, M, FK, X, E, K](join, target, key, Some(value))

		override def apply(key :K, value :Option[X]) :Reference[X] =
			new TypedJoinReference[S, M, FK, X, E, K](join, target, key, value)


		override def full(value: X): Reference[X] = {
			val elems = items.decomposition(value)
			val keys = elems.flatMap(key(_))
			if (keys.nonEmpty && keys.size==elems.size)
				apply(keys.head, Some(value))
			else Full(value)
		}

		override def empty(key: K): Reference[X] = apply(key, None)

		override def keyFor(item: E): Option[K] = key(item)

		override def keyOf[F >: Reference[X] <: Reference[X]](ref: F): Option[K] = ref match {
			case TypedJoinReference(j, t, key, _) if j==join && t==target =>
				Some(key.asInstanceOf[K])
			case items.decomposition(items)  =>
				items.headOption.flatMap(keyFor).filter(k => items.forall(keyFor(_)==k))
			case _ => None
		}

		override def as[Y](implicit composition: ComposedOf[Y, E]): HigherKindReferenceFactory[K, E, Y, Reference] =
			new JoinReferenceFactory[S, T, M, FK, O, Y, E, K](join, target, key)

		override def equivalencyToken: Any = (join, target)

		override def toString = s"$items($target from $join)"
	}



	trait GenericJoinReferenceIndustry[R[X]<:Reference[X]] {
		def apply[S<:RowSource, T<:Mapping[O], M<:Mapping[E], FK<:Mapping[K], O, E, K](
				source :S Join FK, target :ComponentExpression[S, T, M], key :E=>Option[K])
			:GenericReferenceFactory[K, E, E, R[E]]

		def apply[S<:RowSource, T<:Mapping[E], FK<:Mapping[K], E, K](
				source :S Join FK, target :JoinedTable[S, T], key :E=>Option[K]) :GenericReferenceFactory[K, E, E, R[E]] =
			apply[S, T, T, FK, E, E, K](source, target.columns, key)

		def apply[S <: RowSource, T <: Mapping[E], FK <: Mapping[K], E, K](source: S Join T Join FK, key: E => Option[K])
				: GenericReferenceFactory[K, E, E, R[E]] =
			apply[S Join T, T, T, FK, E, E, K](source, source.left.last.columns, key)


	}


	class JoinReferenceIndustry extends GenericJoinReferenceIndustry[Reference] {
		override def apply[S <: RowSource, T <: Mapping[O], M <: Mapping[E], FK <: Mapping[K], O, E, K](
				source: Join[S, FK], target: ComponentExpression[S, T, M], key: (E) => Option[K])
		: GenericReferenceFactory[K, E, E, Reference[E]] =
			new JoinReferenceFactory[S, T, M, FK, O, E, E, K](source, target, key)

	}


//
//	class JoinRestrictedReferenceIndustry extends GenericJoinReferenceIndustry[Reference] {
//		override def apply[S <: RowSource, T <: Mapping[O], M <: Mapping[E], FK <: Mapping[K], O, E, K](
//				source: Join[S, FK], target: ComponentExpression[S, T, M], key: (E) => Option[K])
//		: GenericReferenceFactory[K, E, E, Reference[E]] =
//			new RestrictedReferenceFactory()
//	}

}

*/

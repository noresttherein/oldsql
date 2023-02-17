package com.hcore.ogre.mapping.bits

import com.hcore.ogre.mapping.ComponentPath.TypedComponentPath
import com.hcore.ogre.mapping.Mapping.TypedMapping
import com.hcore.ogre.mapping.MappingMorphism.ValueMorphism
import com.hcore.ogre.mapping.{ComponentPath, AnyMapping, TypedColumn, Mapping}
import com.hcore.ogre.slang.matching.Unapply
import com.hcore.ogre.slang.matching.Unapply.UnapplyAdapter


object MappingMatchers {

	object AsColumn {
		def apply[T](mapping :Mapping[T]) :Option[TypedColumn[T]] = //unapply(mapping)
			TypedColumn.adapt(mapping)

//		def unapply[T](mapping :Mapping[T]) :Option[TypedColumn[T]] =
//			TypedColumn.adapt(mapping)

//		def unapply[M<:Mapping, C](path)
//		def unapply[T](path :TypedComponentPath[_<:Mapping, _<:ComponentFor[T], T]) :Option[TypedColumn[T]] =
//			TypedColumn.adapt(path.end)

		def unapply[M<:AnyMapping, C<:AnyMapping](path :ComponentPath[M, C]) :Option[(TypedColumn[C#ResultType], ValueMorphism[M#ResultType, C#ResultType])] =
			for (lifted <- path.lift; column <- TypedColumn.adapt(lifted.asInstanceOf[TypedColumn[C#ResultType]]))
				yield (column, path.morphism.value)
	}

	object SingleColumn {
//		def unapply[T](mapping :Mapping[T]) :Option[(TypedColumn[X], ValueMorphism[T, X]) forSome { type X }] = mapping.columns match {
//			case Seq(column) =>
//				AsColumn(column.asInstanceOf[Mapping[Any]]).map((_, (mapping \\ column).morphism.value))
//			case _ => None
//		}

//		def unapply[T](path :TypedComponentPath[_<:Mapping, _<:ComponentFor[T], T]) :Option[(TypedColumn[T], ValueMorphism)]

		def unapply[M<:AnyMapping, C<:AnyMapping](path :ComponentPath[M, C]) :Option[(TypedColumn[X], ValueMorphism[M#ResultType, X]) forSome { type X }] =
			path.end.columns match {
				case Seq(single) => AsColumn.unapply(path)
//					for (lifted <- path.lift(single); column <- TypedColumn.adapt(lifted.asInstanceOf[TypedColumn[C#ResultType]]))
//						yield (column, (path :+ single).morphism.value)
				case _ => None
			}
	}

	object EmptyColumns {
//		def unapply(mapping :Mapping) :Boolean = mapping.columns.isEmpty

		def unapply[M<:AnyMapping, C<:AnyMapping](path :ComponentPath[M, C]) :Boolean = path.end.columns.map(path.lift(_) :Option[M#Component[_]]).isEmpty
	}

//	object LiftedColumns {
//		def unapply[M<:Mapping, C<:Mapping](path :ComponentPath[M, C]) :Option[(TypedColumn[C#ResultType], ValueMorphism[M#ResultType, C#ResultType])] =
//
//	}

//	val EmptyMapping = Unapply.Check((_:Mapping).columns.isEmpty)
}

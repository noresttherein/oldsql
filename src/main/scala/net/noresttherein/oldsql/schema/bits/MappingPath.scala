package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.morsels.{Extractor, InferTypeParams}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.{MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.bits.MappingPath.{ComponentPath, ConcatPath, SelfPath}






trait MappingPath[-X <: MappingOf[S], +Y <: TypedMapping[T, O], S, T, O] { self =>
	val end :Y
	def extractor :Extractor[S, T]

	def components :Seq[Mapping] = listComponents(Nil)

	private[schema] def listComponents(acc :List[Mapping]) :List[Mapping] = end::acc



	def \[Z <: TypedMapping[U, P], U, P](next :MappingPath[Y, Z, T, U, P]) :MappingPath[X, Z, S, U, P] = next match {
		case _ :SelfPath[_, _, _] =>
			if (end != next.end)
				throw new IllegalArgumentException(
					s"Can't append self-path $next to $this as refers to a different mapping."
				)
			this.asInstanceOf[MappingPath[X, Z, S, U, P]]
		case _ => new ConcatPath(this, next)
	}

	def \[Z <: TypedMapping[U, O], U](next :ComponentPath[Y, Z, T, U, O]) :MappingPath[X, Z, S, U, O] =
		\(next :MappingPath[Y, Z, T, U, O])

	def \[M <: Mapping, Z <: TypedMapping[U, O], U]
	     (component :Y => M)(implicit hint :InferTypeParams[M, Z, TypedMapping[U, O]]) :MappingPath[X, Z, S, U, O] =
		\(component(end))

	def \[M <: Mapping, Z <: TypedMapping[U, O], U]
	     (component :M)(implicit hint :InferTypeParams[M, Z, TypedMapping[U, O]]) :MappingPath[X, Z, S, U, O] =
		this \ (end \ component)

	def :\[U](component :TypedMapping[U, O]) :MappingPath[X, component.type, S, U, O] =
		\[component.type, component.type, U](component :component.type)(InferTypeParams[component.type])



	def apply[M <: Mapping, Z <: TypedMapping[U, O], U]
	         (component :Y => M)(implicit hint :InferTypeParams[M, Z, TypedMapping[U, O]]) :MappingPath[X, Z, S, U, O] =
		\(component(end))


	def canEqual(that :Any) :Boolean = that.isInstanceOf[MappingPath[_, _, _, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :MappingPath[_, _, _, _, _] if (other canEqual this) && canEqual(other) =>
			end == other.end && extractor == other.extractor
		case _ => false
	}
	
	override def hashCode :Int = end.hashCode * 31 + extractor.hashCode

	override def toString :String = ".\\*\\" + end
	
}






object MappingPath {


	
	trait ComponentPath[-X <: TypedMapping[S, O], +Y <: TypedMapping[T, O], S, T, O]
		extends MappingPath[X, Y, S, T, O]
	{

		def carry(values :ComponentValues[S, O]) :ComponentValues[T, O]

		def carry(values :ColumnValues[S, O]) :ColumnValues[T, O]

		override def \[Z <: TypedMapping[U, P], U, P](next :MappingPath[Y, Z, T, U, P]) :MappingPath[X, Z, S, U, P] =
			next match {
				case comp :ComponentPath[X, Y, S, T, O] @unchecked => this \ comp
				case _ => super.\(next)
			}


		override def \[Z <: TypedMapping[U, O], U](next :ComponentPath[Y, Z, T, U, O]) :ComponentPath[X, Z, S, U, O] =
			next match {
				case self :SelfPath[_, _, _] =>
					if (self.end != end)
						throw new IllegalArgumentException(
							s"Can't append self-path $next to $this as refers to a different mapping."
						)
                    this.asInstanceOf[ComponentPath[X, Z, S, U, O]]
				case _ =>
					new ConcatComponentPath[X, Y, Z, S, T, U, O](this, next)
			}



		override def \[M <: Mapping, Z <: TypedMapping[U, O], U]
		              (component :Y => M)(implicit hint :InferTypeParams[M, Z, TypedMapping[U, O]])
				:ComponentPath[X, Z, S, U, O] =
			\(component(end))


		override def \[M <: Mapping, Z <: TypedMapping[U, O], U]
		             (component :M)(implicit hint :InferTypeParams[M, Z, TypedMapping[U, O]])
				:ComponentPath[X, Z, S, U, O] =
		{
			val c = hint(component)
			ComponentPath.typed[X, Z, S, U, O](c)(extractor andThen end(c))
		}

		override def :\[U](component :TypedMapping[U, O]) :ComponentPath[X, component.type, S, U, O] =
			\[component.type, component.type, U](component :component.type)(InferTypeParams[component.type])



		override def apply[M <: Mapping, Z <: TypedMapping[U, O], U]
		                  (component :Y => M)(implicit hint :InferTypeParams[M, Z, TypedMapping[U, O]])
				:ComponentPath[X, Z, S, U, O] =
			\(component(end))

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentPath[_, _, _, _, _]]

		override def toString :String = "\\" + end

	}

	

	object ComponentPath {

		def apply[P <: Mapping, X <: TypedMapping[S, O], C <: Mapping, Y <: TypedMapping[T, O], S, T, O]
		         (parent :P, component :C)
		         (implicit parentType :InferTypeParams[P, X, TypedMapping[S, O]],
		                   childType :InferTypeParams[C, Y, TypedMapping[T, O]])
			:ComponentPath[X, Y, S, T, O] =
		{ 
			val c = childType(component)
			typed[X, Y, S, T, O](c)(parentType(parent)(c))
		}

		private[MappingPath] def typed[X <: TypedMapping[S, O], Y <: TypedMapping[T, O], S, T, O]
		                              (component :Y)(extract :S =?> T) :ComponentPath[X, Y, S, T, O] =
			new ComponentPath[X, Y, S, T, O] {
				override val end = component
				override def extractor = extract

				override def carry(values :ComponentValues[S, O]) = values / end

				override def carry(values :ColumnValues[S, O]) = values / end
			}



		def unapply[X <: MappingOf[S], Y <: TypedMapping[T, O], S, T, O](path :MappingPath[X, Y, S, T, O]) :Opt[Y] =
			path match {
				case _ :ComponentPath[_, _, _, _, _] => Got(path.end)
				case _ => Lack
			}

	}






	trait SelfPath[X <: TypedMapping[S, O], S, O] extends ComponentPath[X, X, S, S, O] {

		override def extractor :RequisiteExtractor[S, S] = Extractor.ident[S]

		override def listComponents(acc :List[Mapping]) :List[Mapping] = acc


		override def carry(values :ComponentValues[S, O]) :ComponentValues[S, O] = values

		override def carry(values :ColumnValues[S, O]) :ColumnValues[S, O] = values


		override def \[Z <: TypedMapping[U, P], U, P](next :MappingPath[X, Z, S, U, P]) :MappingPath[X, Z, S, U, P] =
			next

		override def \[Z <: TypedMapping[U, O], U](next :ComponentPath[X, Z, S, U, O]) :ComponentPath[X, Z, S, U, O] =
			next

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelfPath[_, _, _]]

		override def toString :String = ".(" + end + ")"
	}




	object SelfPath {
		@inline 
		implicit def apply[M, X <: TypedMapping[S, O], S, O]
		                  (mapping :M)(implicit typeHint :InferTypeParams[M, X, TypedMapping[S, O]]) :SelfPath[X, S, O] =
			typed[X, S, O](mapping)

		def typed[X <: TypedMapping[S, O], S, O](mapping :X) :SelfPath[X, S, O] = new SelfPath[X, S, O] {
			override val end = mapping
		}


		def unapply[X <: MappingOf[S], Y <: TypedMapping[T, O], S, T, O](path :MappingPath[X, Y, S, T, O]) :Opt[X] =
			path match {
				case self :SelfPath[_, _, _] => Got(self.end.asInstanceOf[X])
				case _ => Lack
			}
	}






	private[MappingPath] class ConcatPath[W <: MappingOf[R], X <: TypedMapping[S, N],
	                                      Y <: TypedMapping[T, O], R, S, T, N, O]
										 (val first :MappingPath[W, X, R, S, N], val second :MappingPath[X, Y, S, T, O])
		extends MappingPath[W, Y, R, T, O]
	{
		private[MappingPath] type First = MappingPath[W, X, R, S, N]
		private[MappingPath] type Second = MappingPath[X, Y, S, T, O]

		private[MappingPath] def _1 :First = first
		private[MappingPath] def _2 :Second = second

		override val end :Y = second.end
		override val extractor : R =?> T = first.extractor andThen second.extractor

		override def listComponents(acc :List[Mapping]) :List[Mapping] =
			first.listComponents(second.end::acc)

		override def \[Z <: TypedMapping[U, P], U, P](next :MappingPath[Y, Z, T, U, P]) :MappingPath[W, Z, R, U, P] =
			next match {
				case concat :ConcatPath[_, _, _, _, _, _, _, _] =>
					this \ concat.first.asInstanceOf[MappingPath[Y, TypedMapping[Any, P], T, Any, P]] \
						concat.second.asInstanceOf[MappingPath[TypedMapping[Any, P], Z, Any, U, P]]
				case _ =>
					super.\(next)
			}

		override def toString :String = first.toString + second.toString
	}

	
	
	private[MappingPath] class ConcatComponentPath[W <: TypedMapping[R, O], X <: TypedMapping[S, O],
	                                               Y <: TypedMapping[T, O], R, S, T, O]
	                                              (override val first :ComponentPath[W, X, R, S, O],
	                                               override val second :ComponentPath[X, Y, S, T, O])
		extends ConcatPath[W, X, Y, R, S, T, O, O](first, second) with ComponentPath[W, Y, R, T, O]
	{
		override def carry(values :ComponentValues[R, O]) = second.carry(first.carry(values))

		override def carry(values :ColumnValues[R, O]) = second.carry(first.carry(values))

		override def \[Z <: TypedMapping[U, P], U, P](next :MappingPath[Y, Z, T, U, P]) :MappingPath[W, Z, R, U, P] =
			next match {
				case comp :ComponentPath[Y, Z, T, U, P] => this \ comp
				case concat :ConcatPath[_, _, _, _, _, _, _, _] =>
					this \ concat.first.asInstanceOf[MappingPath[Y, TypedMapping[Any, P], T, Any, P]] \
						concat.second.asInstanceOf[MappingPath[TypedMapping[Any, P], Z, Any, U, P]]
				case _ =>
					super.\(next)
			}

		override def \[Z <: TypedMapping[U, O], U](next :ComponentPath[Y, Z, T, U, O]) :ComponentPath[W, Z, R, U, O] =
			next match {
				case concat :ConcatComponentPath[_, _, _, _, _, _, _] =>
					this \ concat.first.asInstanceOf[ComponentPath[Y, TypedMapping[Any, O], T, Any, O]] \
						concat.second.asInstanceOf[ComponentPath[TypedMapping[Any, O], Z, Any, U, O]]
				case _ =>
					super.\(next)
			}

	}




	object \~\ {

		def unapply[X <: MappingOf[S], Y <: TypedMapping[T, O], S, T, O](path :MappingPath[X, Y, S, T, O])
				:Opt[(MappingPath[X, W, S, Q, _], MappingPath[W, Y, Q, T, O])] forSome
					{ type W <: MappingOf[Q]; type Q } =
			path match {
				case concat :ConcatPath[X, _, Y, S, _, T, _, O] => Got(concat.first -> concat.second)
				case _ => Lack
			}
	}

	object \\ {
		def unapply[X <: TypedMapping[S, O], Y <: TypedMapping[T, O], S, T, O](path :MappingPath[X, Y, S, T, O])
				:Opt[(ComponentPath[X, W, S, Q, O], ComponentPath[W, Y, Q, T, O])] forSome
					{ type W <: TypedMapping[Q, O]; type Q } =
			path match {
				case concat :ConcatComponentPath[X, _, Y, S, _, T, O] => Got(concat.first -> concat.second)
				case _ => Lack
			}

	}


}


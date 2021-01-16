package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.morsels.{Extractor, InferTypeParams}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalAdapter, OptionalExtractor, RequisiteAdapter, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.GenericExtract.{ConstantExtract, EmptyExtract, IdentityExtract, OptionalExtract, RequisiteExtract}






object ColumnExtract { //todo: unify the name with ColumnMappingExtract


	def apply[S, T, O](column :ColumnMapping[T, O], pick :S => Option[T], surepick :Option[S => T])
			:ColumnMappingExtract[S, T, O] =
		surepick match {
			case Some(sure) => req(column)(sure)
			case _ => opt(column)(pick)
		}


	def apply[S, T, O](column :ColumnMapping[T, O])(extractor :Extractor[S, T]) :ColumnMappingExtract[S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => ident(column).asInstanceOf[ColumnMappingExtract[S, T, O]]
			case c :ConstantExtractor[_, T @unchecked] => const(column)(c.constant)
			case requisite :RequisiteExtractor[S @unchecked, T @unchecked] => req(column)(requisite.getter)
			case _ :EmptyExtractor[_, _] => none(column)
			case _ => opt(column)(extractor.optional) //todo: FromOptionExtractor
		}




	def opt[S, T, O](column :ColumnMapping[T, O])(extractor :S => Option[T]) :ColumnMappingExtract[S, T, O] =
		new OptionalExtract[ColumnMapping[T, O], S, T, O](column, extractor)

	def req[S, T, O](column :ColumnMapping[T, O])(requisite :S => T) :ColumnMappingExtract[S, T, O] =
		new RequisiteExtract[ColumnMapping[T, O], S, T, O](column, requisite)

	def ident[T, O](column :ColumnMapping[T, O]) :ColumnMappingExtract[T, T, O] =
		new IdentityExtract[ColumnMapping[T, O], T, O](column)

	def const[T, O](column :ColumnMapping[T, O])(value :T) :ColumnMappingExtract[Any, T, O] =
		new ConstantExtract[ColumnMapping[T, O], T, O](column, value)

	def none[T, O](column :ColumnMapping[T, O]) :ColumnMappingExtract[Any, T, O] =
		new EmptyExtract(column)



	def unapply[S, T](extractor :Extractor[S, T]) :Opt[(S => Option[T], Opt[S => T], ColumnMapping[_ <: T, _])] =
		extractor match {
			case c :GenericExtract[_, S @unchecked, T @unchecked, _] if c.export.isInstanceOf[ColumnMapping[_, _]] =>
				Got((extractor.optional, extractor.requisite, c.export.asInstanceOf[ColumnMapping[_ <: T, _]]))
			case _ =>
				Lack
		}

}






object MappingExtract {

	def apply[S, T, O](component :RefinedMapping[T, O], pick :S => Option[T], surepick :Option[S => T])
			:MappingExtract[S, T, O] =
		surepick match {
			case Some(sure) => req(component)(sure)
			case _ => opt(component)(pick)
		}

	def apply[S, T, O](component :RefinedMapping[T, O])(extractor :Extractor[S, T]) :MappingExtract[S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => ident(component).asInstanceOf[MappingExtract[S, T, O]]
			case c :ConstantExtractor[_, T @unchecked] => const(component)(c.constant)
			case requisite :RequisiteExtractor[S @unchecked, T @unchecked] => req(component)(requisite.getter)
			case _ :EmptyExtractor[_, _] => none(component)
			case _ => opt(component)(extractor.optional) //todo: FromOptionExtractor
		}



	def opt[S, T, O](component :RefinedMapping[T, O])(extractor :S => Option[T]) :MappingExtract[S, T, O] =
		new OptionalExtract[RefinedMapping[T, O], S, T, O](component, extractor)

	def req[S, T, O](component :RefinedMapping[T, O])(requisite :S => T) :MappingExtract[S, T, O] =
		new RequisiteExtract[RefinedMapping[T, O], S, T, O](component, requisite)

	def ident[T, O](component :RefinedMapping[T, O]) :MappingExtract[T, T, O] =
		new IdentityExtract[RefinedMapping[T, O], T, O](component)

	def const[T, O](component :RefinedMapping[T, O])(value :T) :MappingExtract[Any, T, O] =
		new ConstantExtract[RefinedMapping[T, O], T, O](component, value)

	def none[T, O](component :RefinedMapping[T, O]) :MappingExtract[Any, T, O] =
		new EmptyExtract(component)



	def unapply[S, T](extractor :Extractor[S, T]) :Opt[(S => Option[T], Opt[S => T], RefinedMapping[_ <: T, _])] =
		extractor match {
			case c :GenericExtract[_, S @unchecked, T @unchecked, _] =>
				Got((extractor.optional, extractor.requisite, c.export.asInstanceOf[RefinedMapping[_ <: T, _]]))
			case _ =>
				Lack
		}

}






/** Generic interface of [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]]
  * and [[net.noresttherein.oldsql.schema.ColumnMappingExtract ColumnMappingExtract]], parameterized
  * with the type of the ''export'' mapping. Both of the above types are type aliases to this trait, so as long as
  * the export mapping is a column, a `MappingExtract` can be cast down to a `ColumnMappingExtract`, even if it was
  * created as a `MappingExtract` (using the more generic `MappingExtract` factory). Several places in the codebase
  * take advantage of this type.
  */
trait GenericExtract[+M <: RefinedMapping[T, O], -S, T, O] extends Extractor[S, T] {

	val export :M

	@inline final def apply[L <: S](pieces :ComponentValues[L, O]) :T = pieces(this)

	@inline final def get[L <: S](pieces :ComponentValues[L, O]) :Option[T] = pieces.get(this)


	def andThen[C <: RefinedMapping[Y, O], Y](extract :GenericExtract[C, T, Y, O]) :GenericExtract[C, S, Y, O]

	override def compose[X](extractor :X =?> S) :GenericExtract[M, X, T, O]
	override def composeOpt[X](extractor :OptionalExtractor[X, S]) :GenericExtract[M, X, T, O]
	override def composeReq[X](extractor :RequisiteExtractor[X, S]) :GenericExtract[M, X, T, O]
	override def compose[X](f :X => S) :GenericExtract[M, X, T, O]
	override def composeOpt[X](f :X => Option[S]) :GenericExtract[M, X, T, O]

	override def toString :String = "Extract(" + export + ")"
}






object GenericExtract {

	def apply[X <: Mapping, M <: RefinedMapping[T, O], S, T, O]
	         (component :X)(extractor :Extractor[S, T])
	         (implicit InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]]) :GenericExtract[M, S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => identity(component).asInstanceOf[GenericExtract[M, S, T, O]]
			case c :ConstantExtractor[_, T @unchecked] => const(component)(c.constant)
			case r :RequisiteExtractor[S @unchecked, T @unchecked] => req(component)(r.getter)
			case _ :EmptyExtractor[_, _] => none(component)
			case _ => opt(component)(extractor.optional) //todo: FromOptionExtractor
		}



	def opt[X <: Mapping, M <: RefinedMapping[T, O], S, T, O]
	       (component :X)(extractor :S => Option[T])
	       (implicit InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]]) :GenericExtract[M, S, T, O] =
		new OptionalExtract[M, S, T, O](component, extractor)

	def req[X <: Mapping, M <: RefinedMapping[T, O], S, T, O]
	       (component :X)(extractor :S => T)(implicit InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericExtract[M, S, T, O] =
		new RequisiteExtract(component, extractor)

	def ident[X <: Mapping, M <: RefinedMapping[T, O], T, O]
	         (component :X)(implicit InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericExtract[M, T, T, O] =
		new IdentityExtract(component)

	def const[X <: Mapping, M <: RefinedMapping[T, O], T, O]
	         (component :X)(constant :T)(implicit InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericExtract[M, Any, T, O] =
		new ConstantExtract[M, T, O](component, constant)

	def none[X <: Mapping, M <: RefinedMapping[T, O], T, O]
	        (component :X)(implicit InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericExtract[M, Any, T, O] =
		new EmptyExtract[M, T, O](component)



	def unapply[M <: RefinedMapping[T, O], S, T, O](extract :GenericExtract[M, S, T, O])
			:Opt[(S => Option[T], Opt[S => T], M)] =
		Got(extract.optional, extract.requisite, extract.export)






	private[schema] class OptionalExtract[+M <: RefinedMapping[T, O], -S, T, O]
	                                     (override val export :M, extract :S => Option[T])
		extends OptionalAdapter[S, T](extract) with GenericExtract[M, S, T, O]
	{
		override def andThen[C <: RefinedMapping[Y, O], Y](extract :GenericExtract[C, T, Y, O]) =
			extract composeOpt optional

		override def compose[W](extractor :W =?> S) =
			GenericExtract(export)(extractor andThenOpt optional)

		override def composeOpt[W](extractor :OptionalExtractor[W, S]) =
			GenericExtract(export)(extractor andThenOpt optional)

		override def composeReq[W](extractor :RequisiteExtractor[W, S]) =
			GenericExtract(export)(extractor andThenOpt optional)

		override def compose[W](f :W => S) =
			GenericExtract.opt(export)(f andThen optional)

		override def composeOpt[W](f :W => Option[S]) =
			GenericExtract.opt(export)(f(_).flatMap(optional))

		override def toString :String = "Optional(" + export + ")"
	}




	private[schema] class RequisiteExtract[+M <: RefinedMapping[T, O], -S, T, O]
	                                      (override val export :M, extract :S => T)
		extends RequisiteAdapter[S, T](extract) with GenericExtract[M, S, T, O]
	{
		override def andThen[C <: RefinedMapping[Y, O], Y](extract :GenericExtract[C, T, Y, O]) =
			extract compose getter

		override def compose[X](extractor :X =?> S) :GenericExtract[M, X, T, O] =
			GenericExtract(export)(extractor andThen getter)

		override def composeOpt[X](extractor :OptionalExtractor[X, S]) :GenericExtract[M, X, T, O] =
			composeOpt(extractor.optional) //this method isn't really called, at least by GenericExtract

		override def composeReq[X](extractor :RequisiteExtractor[X, S]) :RequisiteExtract[M, X, T, O] =
			compose(extractor.getter) //this method isn't really called, at least by GenericExtract

		override def compose[X](f :X => S) :RequisiteExtract[M, X, T, O] =
			new RequisiteExtract[M, X, T, O](export, f andThen getter)

		override def composeOpt[X](f :X => Option[S]) :GenericExtract[M, X, T, O] = {
			val cont = getter
			new OptionalExtract[M, X, T, O](export, f(_) map cont) {
				override def get(x :X) :Opt[T] = f(x) match {
					case Some(s) => Got(cont(s))
					case _ => None
				}
			}
		}

		override def toString :String = "Requisite(" + export + ")"
	}




	private[schema] class IdentityExtract[+M <: RefinedMapping[T, O], T, O](component :M)
		extends RequisiteExtract[M, T, T, O](component, Predef.identity[T]) with IdentityExtractor[T]
	{
		override def andThen[C <: RefinedMapping[Y, O], Y](extract :GenericExtract[C, T, Y, O]) =
			extract

		override def compose[X](extractor :X =?> T) :GenericExtract[M, X, T, O] =
			GenericExtract(export)(extractor)

		override def composeOpt[X](extractor :OptionalExtractor[X, T]) :GenericExtract[M, X, T, O] =
			GenericExtract.opt(export)(extractor.optional)

		override def composeReq[X](extractor :RequisiteExtractor[X, T]) :RequisiteExtract[M, X, T, O] =
			new RequisiteExtract[M, X, T, O](export, extractor.getter)
//			GenericExtract(export)(extractor).asInstanceOf[RequisiteExtract[M, X, T, O]]

		override def compose[X](r :X => T) = new RequisiteExtract[M, X, T, O](export, r)

		override def composeOpt[X](f :X => Option[T]) :GenericExtract[M, X, T, O] =
			new OptionalExtract[M, X, T, O](export, f)

		override def toString :String = "Identity(" + export + ")"
	}




	private[schema] class ConstantExtract[+M <: RefinedMapping[T, O], T, O](component :M, override val constant :T)
		extends RequisiteExtract[M, Any, T, O](component, (_ :Any) => constant) with ConstantExtractor[Any, T]
	{
		override def andThen[C <: RefinedMapping[Y, O], Y](extract :GenericExtract[C, T, Y, O]) =
			GenericExtract(extract.export)(this andThen (extract :T =?> Y))

		override def compose[X](extractor :X =?> Any) :GenericExtract[M, X, T, O] =
			GenericExtract(export)(super[ConstantExtractor].compose(extractor))

		override def composeOpt[X](extractor :OptionalExtractor[X, Any]) :GenericExtract[M, X, T, O] =
			composeOpt(extractor.optional)

		override def composeReq[X](extractor :RequisiteExtractor[X, Any]) :RequisiteExtract[M, X, T, O] = this

		override def compose[X](req :X => Any) :RequisiteExtract[M, X, T, O] = this

		override def toString :String = "Const(" + export + "=" + constant + ")"
	}




	private[schema] class EmptyExtract[+M <: RefinedMapping[T, O], T, O](component :M)
		extends OptionalExtract[M, Any, T, O](component, Extractor.none.optional) with EmptyExtractor[Any, T]
	{
		override def andThen[C <: RefinedMapping[Y, O], Y](extract :GenericExtract[C, T, Y, O]) =
			GenericExtract.none(extract.export)

		override def compose[X](extractor :X =?> Any) :GenericExtract[M, X, T, O] = this
		override def composeOpt[X](extractor :OptionalExtractor[X, Any]) :GenericExtract[M, X, T, O] = this
		override def composeReq[X](extractor :RequisiteExtractor[X, Any]) :GenericExtract[M, X, T, O] = this
		override def compose[X](req :X => Any) :GenericExtract[M, X, T, O] = this
		override def composeOpt[X](f :X => Option[Any]) :GenericExtract[M, X, T, O] = this

		override def toString :String = "Empty(" + export + ")"
	}


}


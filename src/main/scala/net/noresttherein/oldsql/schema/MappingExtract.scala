package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.morsels.{Extractor, InferTypeParams}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.GenericExtract.{ConstantExtract, EmptyExtract, IdentityExtract, OptionalExtract, RequisiteExtract}






object ColumnExtract {


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



	def unapply[S, T](extractor :Extractor[S, T]) :Option[(S => Option[T], Option[S => T], ColumnMapping[_ <: T, _])] =
		extractor match {
			case c :GenericExtract[_, S @unchecked, T @unchecked, _] if c.export.isInstanceOf[ColumnMapping[_, _]] =>
				Some((extractor.optional, extractor.requisite, c.export.asInstanceOf[ColumnMapping[_ <: T, _]]))
			case _ =>
				None
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



	def unapply[S, T](extractor :Extractor[S, T]) :Option[(S => Option[T], Option[S => T], RefinedMapping[_ <: T, _])] =
		extractor match {
			case c :GenericExtract[_, S @unchecked, T @unchecked, _] =>
				Some((extractor.optional, extractor.requisite, c.export.asInstanceOf[RefinedMapping[_ <: T, _]]))
			case _ =>
				None
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


	def andThen[C <: RefinedMapping[Y, O], Y](extract :GenericExtract[C, T, Y, O])
			:GenericExtract[C, S, Y, O] =
		extract compose this



	override def compose[X](extractor :Extractor[X, S]) :GenericExtract[M, X, T, O] = extractor match {
		case _ :OptionalExtractor[_, _] => composeOpt(extractor.optional)

		case _ :IdentityExtractor[_] => this.asInstanceOf[GenericExtract[M, X, T, O]]

		case c :ConstantExtractor[X @unchecked, S @unchecked] => try {
			new ConstantExtract[M, T, O](export, apply(c.constant))
		} catch {
			case _ :Exception =>
				new OptionalExtract[M, X, T, O](export, c.getter andThen optional)
		}

		case req :RequisiteExtractor[X, S] =>
			new OptionalExtract[M, X, T, O](export, req.getter andThen optional)

		case _ :EmptyExtractor[_, _] =>
			new EmptyExtract[M, T, O](export)

		case _ => extractor.requisite match {
			case Some(req) => this compose req
			case _ => this composeOpt extractor.optional
		}

	}

	override def compose[X](first :X => S) :GenericExtract[M, X, T, O] =
		new OptionalExtract[M, X, T, O](export, first andThen optional)

	override def composeOpt[X](first :X => Option[S]) :GenericExtract[M, X, T, O] = {
		val pick = optional
		new OptionalExtract[M, X, T, O](export, first(_) flatMap pick)
	}


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
			:Some[(S => Option[T], Option[S => T], M)] =
		Some(extract.optional, extract.requisite, extract.export)






	private[schema] class OptionalExtract[+M <: RefinedMapping[T, O], -S, T, O]
	                                     (val export :M, override val optional :S => Option[T])
		extends GenericExtract[M, S, T, O] with OptionalExtractor[S, T]
	{
		override def get(x :S) :Option[T] = optional(x)

		override def toString :String = "Optional(" + export + ")"
	}






	private[schema] class RequisiteExtract[+M <: RefinedMapping[T, O], -S, T, O]
	                                      (override val export :M, override val getter :S => T)
		extends GenericExtract[M, S, T, O] with RequisiteExtractor[S, T]
	{
		override def apply(x :S) :T = getter(x)

		override def compose[X](extractor :X =?> S) :GenericExtract[M, X, T, O] = extractor match {
			case r :RequisiteExtractor[X, S] => this compose r

			case _ :OptionalExtractor[X ,S] => this composeOpt extractor.optional

			case _ => extractor.requisite match {
				case Some(req) => this compose req
				case _ => this composeOpt extractor.optional
			}
		}

		override def compose[X](extractor :RequisiteExtractor[X, S]) :RequisiteExtract[M, X, T, O] = extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[RequisiteExtract[M, X, T, O]]

			case c :ConstantExtractor[_, S] => try {
				new ConstantExtract[M, T, O](export, getter(c.constant))
			} catch {
				case _ :Exception =>
					new RequisiteExtract[M, X, T, O](export, c.getter andThen getter)
			}
			case _ =>
				new RequisiteExtract[M, X, T, O](export, extractor.getter andThen getter)
		}

		override def compose[X](f :X => S) :RequisiteExtract[M, X, T, O] =
			new RequisiteExtract[M, X, T, O](export, f andThen getter)

		override def composeOpt[X](f :X => Option[S]) :GenericExtract[M, X, T, O] = {
			val get = getter; new OptionalExtract[M, X, T, O](export, f(_) map get)
		}



		override def toString :String = "Requisite(" + export + ")"

	}






	private[schema] class IdentityExtract[+M <: RefinedMapping[T, O], T, O](component :M)
		extends RequisiteExtract[M, T, T, O](component, Predef.identity[T]) with IdentityExtractor[T]
	{

		override def compose[X](extractor :X =?> T) :GenericExtract[M, X, T, O] = extractor match {
			case e :RequisiteExtractor[_, _] => this compose e

			case comp :GenericExtract[_, _, _, _] if comp.export == export =>
				comp.asInstanceOf[GenericExtract[M, X, T, O]]

			case _ :OptionalExtractor[_, _] => new OptionalExtract[M, X, T, O](export, extractor.optional)

			case _ => extractor.requisite match {
				case Some(req) => this compose req
				case _ => new OptionalExtract[M, X, T, O](export, extractor.optional)
			}
		}

		override def compose[X](extractor :RequisiteExtractor[X, T]) :RequisiteExtract[M, X, T, O] = extractor match {
			case comp :RequisiteExtract[_, _, _, _] => comp.asInstanceOf[RequisiteExtract[M, X, T, O]]

			case _ :IdentityExtractor[_] => this.asInstanceOf[RequisiteExtract[M, X, T, O]]

			case _ => new RequisiteExtract[M, X, T, O](export, extractor.getter)
		}

		override def compose[X](r :X => T) = new RequisiteExtract[M, X, T, O](export, r)

		override def composeOpt[X](f :X => Option[T]) :GenericExtract[M, X, T, O] =
			new OptionalExtract[M, X, T, O](export, f)



		override def toString :String = "Identity(" + export + ")"

	}






	private[schema] class ConstantExtract[+M <: RefinedMapping[T, O], T, O](component :M, override val constant :T)
		extends RequisiteExtract[M, Any, T, O](component, (_ :Any) => constant) with ConstantExtractor[Any, T]
	{

		override def compose[X](extractor :X =?> Any) :GenericExtract[M, X, T, O] = extractor match {
			case _ :RequisiteExtractor[_, _] => this
			case _ :OptionalExtractor[_, _] => composeOpt(extractor.optional)
			case _ :EmptyExtractor[_, _] => new EmptyExtract[M, T, O](export)
			case _ => extractor.requisite match {
				case Some(req) => compose(req)
				case _ => composeOpt(extractor.optional)
			}
		}

		override def compose[X](extractor :RequisiteExtractor[X, Any]) :RequisiteExtract[M, X, T, O] = this

		override def compose[X](req :X => Any) :RequisiteExtract[M, X, T, O] = this

		override def composeOpt[X](f :X => Option[Any]) = new OptionalExtract(export, f(_) map getter)

		override def toString :String = "Const(" + export + "=" + constant + ")"

	}






	private[schema] class EmptyExtract[+M <: RefinedMapping[T, O], T, O](component :M)
		extends OptionalExtract[M, Any, T, O](component, Extractor.none.optional) with EmptyExtractor[Any, T]
	{
		override def compose[X](extractor :X =?> Any) :GenericExtract[M, X, T, O] = this

		override def compose[X](req :X => Any) :GenericExtract[M, X, T, O] = this

		override def composeOpt[X](f :X => Option[Any]) :GenericExtract[M, X, T, O] = this


		override def toString :String = "Empty(" + export + ")"

	}


}


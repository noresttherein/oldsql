package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.slang.OptionGuardExtension





object MappingExtract {

	def apply[S, T, O](component :RefinedMapping[T, O], pick :S => Option[T], surepick :Option[S => T])
			:MappingExtract[S, T, O] =
		surepick match {
			case Some(sure) => req(component)(sure)
			case _ => opt(component)(pick)
		}

	def apply[S, T, O](column :ColumnMapping[T, O], pick :S => Option[T], surepick :Option[S => T])
			:ColumnMappingExtract[S, T, O] =
		surepick match {
			case Some(sure) => req(column)(sure)
			case _ => opt(column)(pick)
		}

	def apply[S, T, O](component :RefinedMapping[T, O])(extractor :Extractor[S, T]) :MappingExtract[S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => ident(component).asInstanceOf[MappingExtract[S, T, O]]
			case c :ConstantExtractor[_, T @unchecked] => const(component)(c.constant)
			case requisite :RequisiteExtractor[S @unchecked, T @unchecked] => req(component)(requisite.getter)
			case _ :EmptyExtractor[_, _] => none(component)
			case _ => opt(component)(extractor.optional) //todo: FromOptionExtractor
		}

	def apply[S, T, O](column :ColumnMapping[T, O])(extractor :Extractor[S, T]) :ColumnMappingExtract[S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => ident(column).asInstanceOf[ColumnMappingExtract[S, T, O]]
			case c :ConstantExtractor[_, T @unchecked] => const(column)(c.constant)
			case requisite :RequisiteExtractor[S @unchecked, T @unchecked] => req(column)(requisite.getter)
			case _ :EmptyExtractor[_, _] => none(column)
			case _ => opt(column)(extractor.optional) //todo: FromOptionExtractor
		}

	def like[X <: Mapping, M <: RefinedMapping[T, O], S, T, O]
	        (component :X)(extractor :Extractor[S, T])(implicit conforms :Conforms[X, M, RefinedMapping[T, O]])
			:GenericMappingExtract[M, S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => identity(component).asInstanceOf[GenericMappingExtract[M, S, T, O]]
			case c :ConstantExtractor[_, T @unchecked] => constant(component)(c.constant)
			case r :RequisiteExtractor[S @unchecked, T @unchecked] => requisite(component)(r.getter)
			case _ :EmptyExtractor[_, _] => empty(component)
			case _ => optional(component)(extractor.optional) //todo: FromOptionExtractor
		}



	def optional[X <: Mapping, M <: RefinedMapping[T, O], S, T, O]
	            (component :X)(extractor :S => Option[T])(implicit conforms :Conforms[X, M, RefinedMapping[T, O]])
			:GenericMappingExtract[M, S, T, O] =
		new OptionalExtract[M, S, T, O](component, extractor)

	def opt[S, T, O](component :RefinedMapping[T, O])(extractor :S => Option[T]) :MappingExtract[S, T, O] =
		new OptionalExtract[RefinedMapping[T, O], S, T, O](component, extractor)

	def opt[S, T, O](column :ColumnMapping[T, O])(extractor :S => Option[T]) :ColumnMappingExtract[S, T, O] =
		new OptionalExtract[ColumnMapping[T, O], S, T, O](column, extractor)



	def requisite[X <: Mapping, M <: RefinedMapping[T, O], S, T, O]
	             (component :X)(extractor :S => T)(implicit conforms :Conforms[X, M, RefinedMapping[T, O]])
			:GenericMappingExtract[M, S, T, O] =
		new RequisiteExtract(component, extractor)

	def req[S, T, O](component :RefinedMapping[T, O])(requisite :S => T) :MappingExtract[S, T, O] =
		new RequisiteExtract[RefinedMapping[T, O], S, T, O](component, requisite)

	def req[S, T, O](column :ColumnMapping[T, O])(requisite :S => T) :ColumnMappingExtract[S, T, O] =
		new RequisiteExtract[ColumnMapping[T, O], S, T, O](column, requisite)



	def identity[X <: Mapping, M <: RefinedMapping[T, O], T, O]
	            (component :X)(implicit conforms :Conforms[X, M, RefinedMapping[T, O]])
			:GenericMappingExtract[M, T, T, O] =
		new IdentityExtract(component)

	def ident[T, O](component :RefinedMapping[T, O]) :MappingExtract[T, T, O] =
		new IdentityExtract[RefinedMapping[T, O], T, O](component)

	def ident[T, O](column :ColumnMapping[T, O]) :ColumnMappingExtract[T, T, O] =
		new IdentityExtract[ColumnMapping[T, O], T, O](column)



	def constant[X <: Mapping, M <: RefinedMapping[T, O], T, O]
	            (component :X)(constant :T)(implicit conforms :Conforms[X, M, RefinedMapping[T, O]])
			:GenericMappingExtract[M, Any, T, O] =
		new ConstantExtract[M, T, O](component, constant)

	def const[T, O](component :RefinedMapping[T, O])(value :T) :MappingExtract[Any, T, O] =
		new ConstantExtract[RefinedMapping[T, O], T, O](component, value)

	def const[T, O](column :ColumnMapping[T, O])(value :T) :ColumnMappingExtract[Any, T, O] =
		new ConstantExtract[ColumnMapping[T, O], T, O](column, value)



	def empty[X <: Mapping, M <: RefinedMapping[T, O], T, O]
	         (component :X)(implicit conforms :Conforms[X, M, RefinedMapping[T, O]])
			:GenericMappingExtract[M, Any, T, O] =
		new EmptyExtract[M, T, O](component)

	def none[T, O](component :RefinedMapping[T, O]) :MappingExtract[Any, T, O] =
		new EmptyExtract(component)

	def none[T, O](column :ColumnMapping[T, O]) :ColumnMappingExtract[Any, T, O] =
		new EmptyExtract(column)



	def unapply[S, T, O](extract :MappingExtract[S, T, O]) :Some[(S => Option[T], Option[S => T], RefinedMapping[T, O])] =
		Some(extract.optional, extract.requisite, extract.export)

	def unapply[S, T](extractor :Extractor[S, T]) :Option[(S => Option[T], Option[S => T], RefinedMapping[_ <: T, _])] =
		extractor match {
			case c :GenericMappingExtract[_, S @unchecked, T @unchecked, _] =>
				Some((extractor.optional, extractor.requisite, c.export.asInstanceOf[RefinedMapping[_ <: T, _]]))
			case _ =>
				None
		}






	trait GenericMappingExtract[+M <: RefinedMapping[T, O], -S, T, O] extends Extractor[S, T] {

		val export :M

		@inline final def apply[L <: S](pieces :ComponentValues[L, O]) :T = pieces(this)

		@inline final def get[L <: S](pieces :ComponentValues[L, O]) :Option[T] = pieces.get(this)


		def andThen[C <: RefinedMapping[Y, O], Y](extract :GenericMappingExtract[C, T, Y, O])
				:GenericMappingExtract[C, S, Y, O] =
			extract compose this



		override def compose[X](extractor :Extractor[X, S]) :GenericMappingExtract[M, X, T, O] = extractor match {
			case _ :OptionalExtractor[_, _] => composeOpt(extractor.optional)

			case _ :IdentityExtractor[_] => this.asInstanceOf[GenericMappingExtract[M, X, T, O]]

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

		override def compose[X](first :X => S) :GenericMappingExtract[M, X, T, O] =
			new OptionalExtract[M, X, T, O](export, first andThen optional)

		override def composeOpt[X](first :X => Option[S]) :GenericMappingExtract[M, X, T, O] = {
			val pick = optional
			new OptionalExtract[M, X, T, O](export, first(_) flatMap pick)
		}


		override def toString :String = "Extract(" + export + ")"
	}






	private[schema] class OptionalExtract[+M <: RefinedMapping[T, O], -S, T, O]
	                                     (val export :M, override val optional :S => Option[T])
		extends GenericMappingExtract[M, S, T, O] with OptionalExtractor[S, T]
	{
		override def get(x :S) :Option[T] = optional(x)

		override def toString :String = "Optional(" + export + ")"
	}






	private[schema] class RequisiteExtract[+M <: RefinedMapping[T, O], -S, T, O]
	                                      (override val export :M, override val getter :S => T)
		extends GenericMappingExtract[M, S, T, O] with RequisiteExtractor[S, T]
	{
		override def apply(x :S) :T = getter(x)

		override def compose[X](extractor :X =?> S) :GenericMappingExtract[M, X, T, O] = extractor match {
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

		override def composeOpt[X](f :X => Option[S]) :GenericMappingExtract[M, X, T, O] = {
			val get = getter; new OptionalExtract[M, X, T, O](export, f(_) map get)
		}



		override def toString :String = "Requisite(" + export + ")"

	}






	private[schema] class IdentityExtract[+M <: RefinedMapping[T, O], T, O](component :M)
		extends RequisiteExtract[M, T, T, O](component, Predef.identity[T]) with IdentityExtractor[T]
	{

		override def compose[X](extractor :X =?> T) :GenericMappingExtract[M, X, T, O] = extractor match {
			case e :RequisiteExtractor[_, _] => this compose e

			case comp :GenericMappingExtract[_, _, _, _] if comp.export == export =>
				comp.asInstanceOf[GenericMappingExtract[M, X, T, O]]

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

		override def composeOpt[X](f :X => Option[T]) :GenericMappingExtract[M, X, T, O] =
			new OptionalExtract[M, X, T, O](export, f)



		override def toString :String = "Identity(" + export + ")"

	}






	private[schema] class ConstantExtract[+M <: RefinedMapping[T, O], T, O](component :M, override val constant :T)
		extends RequisiteExtract[M, Any, T, O](component, (_ :Any) => constant) with ConstantExtractor[Any, T]
	{

		override def compose[X](extractor :X =?> Any) :GenericMappingExtract[M, X, T, O] = extractor match {
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
		override def compose[X](extractor :X =?> Any) :GenericMappingExtract[M, X, T, O] = this

		override def compose[X](req :X => Any) :GenericMappingExtract[M, X, T, O] = this

		override def composeOpt[X](f :X => Option[Any]) :GenericMappingExtract[M, X, T, O] = this


		override def toString :String = "Empty(" + export + ")"

	}


}



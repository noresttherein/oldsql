package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.model.PropertyPath.ReflectedProperty
import net.noresttherein.oldsql.morsels.{Extractor, InferTypeParams}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.{ColumnMapping, GenericExtract, Mapping}
import net.noresttherein.oldsql.schema.GenericExtract.{ConstantExtract, EmptyExtract, IdentityExtract, OptionalExtract, RequisiteExtract}
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import scala.reflect.runtime.universe.{typeOf, typeTag, Type, TypeTag}

import net.noresttherein.oldsql.collection.Opt.Got






object ComponentProperty {

	private def prop[S :TypeTag, T](extractor :S => Option[T]) :PropertyPath[S, T] =
		PropertyPath.property(extractor andThen (_.get))



	def apply[S :TypeTag, T, O](component :RefinedMapping[T, O])(extractor :Extractor[S, T]) :ComponentProperty[S, T, O] =
		extractor match {
			case _ :OptionalExtractor[S @unchecked, T @unchecked] =>
				opt(component)(extractor.optional)
			case _ :IdentityExtractor[_] =>
				ident(component.asInstanceOf[RefinedMapping[S, O]]).asInstanceOf[ComponentProperty[S, T, O]]
			case constant :ConstantExtractor[S @unchecked, T @unchecked] =>
				const(component)(constant.constant)
			case requisite :RequisiteExtractor[S @unchecked, T @unchecked] =>
				req(component)(requisite.getter)
			case _ :EmptyExtractor[_, _] =>
				none(component)
			case _ =>
				opt(component)(extractor.optional)

		}

	def apply[S :TypeTag, T, O](column :ColumnMapping[T, O])(extractor :Extractor[S, T]) :ColumnProperty[S, T, O] =
		extractor match {
			case _ :OptionalExtractor[S @unchecked, T @unchecked] =>
				opt(column)(extractor.optional)
			case _ :IdentityExtractor[_] =>
				ident(column.asInstanceOf[ColumnMapping[S, O]]).asInstanceOf[ColumnProperty[S, T, O]]
			case constant :ConstantExtractor[S @unchecked, T @unchecked] =>
				const(column)(constant.constant)
			case requisite :RequisiteExtractor[S @unchecked, T @unchecked] =>
				req(column)(requisite.getter)
			case _ :EmptyExtractor[_, _] =>
				none(column)
			case _ =>
				opt(column)(extractor.optional)
		}

	def like[X <: Mapping, M <: RefinedMapping[T, O], S, T, O]
	        (component :X)(extractor :Extractor[S, T])
	        (implicit tag :TypeTag[S], InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericComponentProperty[M, S, T, O] =
		extractor match {
			case _ :OptionalExtractor[S @unchecked, T @unchecked] =>
				optional(component)(extractor.optional)
			case _ :IdentityExtractor[_] =>
				new IdentityProperty[M, T, O](component)(tag.asInstanceOf[TypeTag[T]])
					.asInstanceOf[GenericComponentProperty[M, S, T, O]]
			case const :ConstantExtractor[S @unchecked, T @unchecked] =>
				constant(component)(const.constant)
			case req :RequisiteExtractor[S @unchecked, T @unchecked] =>
				requisite(component)(req.getter)
			case _ :EmptyExtractor[_, _] =>
				empty(component)
			case _ =>
				optional(component)(extractor.optional)

		}



	def opt[S :TypeTag, T, O](component :RefinedMapping[T, O])(extract :S => Option[T]) :ComponentProperty[S, T, O] =
		new OptionalProperty(component, extract)

	def opt[S :TypeTag, T, O](column :ColumnMapping[T, O])(extract :S => Option[T]) :ColumnProperty[S, T, O] =
		new OptionalProperty(column, extract)

	def optional[X <: Mapping, M <: RefinedMapping[T, O], S, T, O]
	            (component :X)(extract :S => Option[T])
	            (implicit tag :TypeTag[S], InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericComponentProperty[M, S, T, O] =
		new OptionalProperty(component, extract)



	def req[S :TypeTag, T, O](component :RefinedMapping[T, O])(extract :S => T) :ComponentProperty[S, T, O] =
		new RequisiteProperty(component, extract)

	def req[S :TypeTag, T, O](column :ColumnMapping[T, O])(extract :S => T) :ColumnProperty[S, T, O] =
		new RequisiteProperty(column, extract)

	def requisite[X <: Mapping, M <: RefinedMapping[T, O], S, T, O]
	             (component :X)(extract :S => T)
	             (implicit tag :TypeTag[S], InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericComponentProperty[M, S, T, O] =
		new RequisiteProperty(component, extract)



	def ident[T :TypeTag, O](component :RefinedMapping[T, O]) :ComponentProperty[T, T, O] =
		new IdentityProperty(component)

	def ident[T :TypeTag, O](column :ColumnMapping[T, O]) :ColumnProperty[T, T, O] =
		new IdentityProperty(column)

	def identity[X <: Mapping, M <: RefinedMapping[T, O], T, O]
	            (component :X)(implicit tag :TypeTag[T], InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericComponentProperty[M, T, T, O] =
		new IdentityProperty(component)



	def const[T, O](component :RefinedMapping[T, O])(constant :T) :ComponentProperty[Any, T, O] =
		new ConstantProperty[RefinedMapping[T, O], T, O](component, constant)

	def const[T, O](column :ColumnMapping[T, O])(constant :T) :ColumnProperty[Any, T, O] =
		new ConstantProperty[ColumnMapping[T, O], T, O](column, constant)

	def constant[X <: Mapping, M <: RefinedMapping[T, O], T, O]
	            (component :X)(constant :T)(implicit InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericComponentProperty[M, Any, T, O] =
		new ConstantProperty[M, T, O](component, constant)



	def none[T, O](component :RefinedMapping[T, O]) :ComponentProperty[Any, T, O] =
		new EmptyProperty(component)

	def none[T, O](column :ColumnMapping[T, O]) :ColumnProperty[Any, T, O] =
		new EmptyProperty(column)

	def empty[X <: Mapping, M <: RefinedMapping[T, O], T, O]
	         (component :X)(implicit InferTypeParams :InferTypeParams[X, M, RefinedMapping[T, O]])
			:GenericComponentProperty[M, Any, T, O] =
		new EmptyProperty(component)






	trait GenericComponentProperty[+M <: RefinedMapping[T, O], -S, T, O] extends GenericExtract[M, S, T, O] {

		val property :PropertyPath[S, T]
		def argumentType :Type
		protected[this] implicit def tag :TypeTag[S]


		override def andThen[C <: RefinedMapping[Y, O], Y](extractor :GenericExtract[C, T, Y, O])
				:GenericComponentProperty[C, S, Y, O]
//		override def andThen[C <: RefinedMapping[Y, O], Y](extractor :GenericExtract[C, T, Y, O])
//				:GenericComponentProperty[C, S, Y, O] = extractor match {
//			case _ :IdentityExtractor[_] =>
//				if (extractor.export == export) this.asInstanceOf[GenericComponentProperty[C, S, Y, O]]
//				else requisite match {
//					case Got(req) =>
//						new RequisiteProperty[C, S, Y, O](extractor.export, req.asInstanceOf[S => Y])
//					case _ =>
//						new OptionalProperty[C, S, Y, O](extractor.export, optional.asInstanceOf[S => Option[Y]])
//				}
//			case const :ConstantProperty[C @unchecked, Y @unchecked, O @unchecked] => const
//
//			case const :ConstantExtractor[T @unchecked, Y @unchecked] =>
//				if (requisite.isDefined)
//					new ConstantProperty[C, Y, O](extractor.export, const.constant)
//				else {
//					val first = optional
//					new OptionalProperty[C, S, Y, O](extractor.export, first(_) map const.getter)
//				}
//
//			case none :EmptyProperty[C @unchecked, Y @unchecked, O @unchecked] => none
//
//			case none :EmptyExtract[C @unchecked, Y @unchecked, O @unchecked] =>
//				new EmptyProperty[C, Y, O](extractor.export)
//
//			case sure :RequisiteExtractor[T @unchecked, Y @unchecked] => requisite match {
//				case Got(req) => new RequisiteProperty[C, S, Y, O](extractor.export, req andThen sure.getter)
//				case _ =>
//					val first = optional; val second = sure.getter
//					new OptionalProperty[C, S, Y, O](extractor.export, first(_).map(second))
//			}
//			case _ => requisite match {
//				case Got(req) => new OptionalProperty[C, S, Y, O](extractor.export, req andThen extractor.optional)
//				case _ =>
//					val first = optional; val second = extractor.optional
//					new OptionalProperty[C, S, Y, O](extractor.export, first(_).flatMap(second))
//			}
//		}



		//todo: maybe we should allow composing extractors with different origins - why not?
		def compose[C <: RefinedMapping[LS, O], X, LS <: S](extractor :GenericComponentProperty[C, X, LS, O])
				:GenericComponentProperty[M, X, T, O] =
			extractor andThen this

		abstract override def compose[X](extractor :X =?> S) :GenericExtract[M, X, T, O] = extractor match {
			case prop :GenericComponentProperty[_, X @unchecked, S @unchecked, _] => compose(prop)
			case _ => super.compose(extractor)
		}


		override def toString :String = "Extractor(" + property + ":" + export + ")"

	}






	private class OptionalProperty[+M <: RefinedMapping[T, O], -S, T, O](component :M, f :S => Option[T])
	                                                                    (implicit protected[this] val tag :TypeTag[S])
		extends OptionalExtract[M, S, T, O](component, f) with GenericComponentProperty[M, S, T, O]
	{
		override val property = prop(f)
		override val argumentType = typeOf[S]

		override def andThen[C <: RefinedMapping[Y, O], Y](extractor :GenericExtract[C, T, Y, O])
				:GenericComponentProperty[C, S, Y, O] =
			extractor match {
				case _ :IdentityExtractor[Y @unchecked] =>
					if (extractor.export == export) this.asInstanceOf[GenericComponentProperty[C, S, Y, O]]
					else new OptionalProperty[C, S, Y, O](extractor.export, optional.asInstanceOf[S => Option[Y]])

				case requisite :RequisiteExtractor[T @unchecked, Y @unchecked] =>
					val first = optional; val second = requisite.getter
					new OptionalProperty(extractor.export, first(_) map second)

				case _ :EmptyExtractor[_, _] => new EmptyProperty[C, Y, O](extractor.export)

				case _ =>
					val first = optional; val second = extractor.optional
					new OptionalProperty(extractor.export, first(_) flatMap second)
			}

		override def toString :String = "Optional(" + property + ":" + export + ")"

	}






	private class RequisiteProperty[+M <: RefinedMapping[T, O], -S, T, O](component :M, f :S => T)
	                                                                     (implicit protected[this] val tag :TypeTag[S])
		extends RequisiteExtract[M, S, T, O](component, f) with GenericComponentProperty[M, S, T, O]
	{

		override val property :ReflectedProperty[S, T] = PropertyPath.property(getter)
		override val argumentType :Type = typeOf[S]

		override def andThen[C <: RefinedMapping[Y, O], Y](extractor :GenericExtract[C, T, Y, O])
				:GenericComponentProperty[C, S, Y, O] =
			extractor match {
				case requisite :RequisiteExtractor[T @unchecked, Y @unchecked] => requisite match {

					case _ :IdentityExtractor[_] =>
						if (extractor.export == export) this.asInstanceOf[GenericComponentProperty[C, S, Y, O]]
						else new RequisiteProperty(extractor.export, getter.asInstanceOf[S => Y])

					case constant :ConstantExtractor[T @unchecked, Y @unchecked] => constant match {
						case property :ConstantProperty[C @unchecked, Y @unchecked, O @unchecked] => property
						case _ => new ConstantProperty(constant.export, constant.constant)
					}
					case _ => new RequisiteProperty(extractor.export, getter andThen requisite.getter)
				}
				case empty :EmptyExtractor[Y @unchecked, O @unchecked] => empty match {
					case property :EmptyProperty[C @unchecked, Y @unchecked, O @unchecked] => property
					case _ => new EmptyProperty(empty.export)
				}
				case _ => new OptionalProperty(extractor.export, getter andThen extractor.optional)
			}



		override def toString :String = "Requisite(" + property + ":" + export + ")"

	}






	private class IdentityProperty[+M <: RefinedMapping[T, O], T, O](component :M)
	                                                                (implicit protected[this] val tag :TypeTag[T])
		extends IdentityExtract[M, T, O](component) with GenericComponentProperty[M, T, T, O]
	{
		override val property :ReflectedProperty[T, T] = PropertyPath.identity[T]
		override val argumentType :Type = typeOf[T]

		override def andThen[C <: RefinedMapping[Y, O], Y](extractor :GenericExtract[C, T, Y, O])
				:GenericComponentProperty[C, T, Y, O] =
			extractor match {
				case prop :GenericComponentProperty[C@unchecked, T@unchecked, Y@unchecked, O@unchecked] =>
					prop
				case _ => like(extractor.export)(extractor)
			}



		override def toString :String = "Identity(" + property + ":" + export + ")"

	}






	private class ConstantProperty[+M <: RefinedMapping[T, O], T, O](component :M, const :T)
		extends ConstantExtract[M, T, O](component, const) with GenericComponentProperty[M, Any, T, O]
	{
		protected[this] implicit override def tag :TypeTag[Any] = typeTag[Any]
		//fixme: this will throw an exception
		override val property :ReflectedProperty[Any, T] = PropertyPath.property(getter)
		override val argumentType :Type = typeOf[Any]


		override def andThen[C <: RefinedMapping[Y, O], Y](extractor :GenericExtract[C, T, Y, O])
				:GenericComponentProperty[C, Any, Y, O] =
			extractor match {
				case _ :IdentityExtractor[_] =>
					if (extractor.export == export) this.asInstanceOf[GenericComponentProperty[C, Any, Y, O]]
					else new ConstantProperty[C, Y, O](extractor.export, constant.asInstanceOf[Y])

				case const :ConstantExtractor[_, Y @unchecked] => extractor match {
					case const :ConstantProperty[C @unchecked, Y @unchecked, O @unchecked] => const
					case _ => new ConstantProperty(extractor.export, const.constant)
				}
				case req :RequisiteExtractor[T @unchecked, Y @unchecked] =>
					new ConstantProperty(extractor.export, req(constant))
				case _ => extractor.opt(constant) match {
					case Got(const) => new ConstantProperty(extractor.export, const)
					case _ => new EmptyProperty(extractor.export)
				}

			}

		override def toString :String = "Constant(" + property + ":" + export + "=" + constant + ")"
	}




	private class EmptyProperty[+M <: RefinedMapping[T, O], T, O](component :M)
		extends EmptyExtract[M, T, O](component) with GenericComponentProperty[M, Any, T, O]
	{
		override val property = prop(optional) //fixme: PropertyPath will throw up
		override val argumentType = typeOf[Any]
		protected[this] override val tag :TypeTag[Any] = typeTag[Any]


		override def andThen[C <: RefinedMapping[Y, O], Y](extractor :GenericExtract[C, T, Y, O])
				:GenericComponentProperty[C, Any, Y, O] =
			new EmptyProperty[C, Y, O](extractor.export)

		override def toString :String = "Empty(" + export + "=<None>)"

	}



}

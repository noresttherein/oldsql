package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.model.PropertyPath.{==>, ReflectedPropertyPath}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.{ColumnMapping, MappingExtract}
import net.noresttherein.oldsql.schema.MappingExtract.{ColumnMappingExtract, MappingExtractTemplate, ConstantColumn, ConstantExtract, ConstantExtractTemplate, EmptyColumn, EmptyExtract, EmptyExtractTemplate, IdentityColumn, IdentityExtract, IdentityExtractTemplate, OptionalExtract, RequisiteColumn, RequisiteExtract, RequisiteExtractTemplate}
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.bits.ComponentProperty.{ColumnProperty, ConstantProperty, EmptyProperty, OptionalProperty, RequisiteProperty}

import scala.reflect.runtime.universe.{typeOf, typeTag, Type, TypeTag}
import scala.reflect.runtime.universe






/** A `MappingExtract` carrying the reflected form of its function as a `PropertyPath`.
  * @see [[net.noresttherein.oldsql.schema.bits.ComponentProperty.ColumnProperty]]
  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
  * @see [[net.noresttherein.oldsql.model.PropertyPath]]
  * @author Marcin Mościcki
  */
trait ComponentProperty[-S, T, O] extends MappingExtract[S, T, O] {
	val property :PropertyPath[S, T]
	def argumentType :Type
	protected[this] implicit def tag :TypeTag[S]



	override def andThen[Y](extractor :MappingExtract[T, Y, O]) :ComponentProperty[S, Y, O] = extractor match {
		case _ :IdentityExtractor[_] =>
			if (extractor.export == export) this.asInstanceOf[ComponentProperty[S, Y, O]]
			else requisite match {
				case Some(req) =>
					ComponentProperty.req(extractor.export)(req.asInstanceOf[S => Y])
				case _ =>
					ComponentProperty.opt(extractor.export)(optional.asInstanceOf[S => Option[Y]])
			}
		case const :ConstantProperty[Y @unchecked, O @unchecked] => const

		case const :ConstantExtractor[T @unchecked, Y @unchecked] => requisite match {
			case Some(_) => ComponentProperty.const(extractor.export)(const.constant)
			case _ =>
				val first = optional
				ComponentProperty.opt(extractor.export)(first(_) map const.getter)
		}

		case none :EmptyProperty[Y @unchecked, O @unchecked] => none

		case none :EmptyExtract[Y @unchecked, O @unchecked] => ComponentProperty.none(none.export)

		case sure :RequisiteExtractor[T @unchecked, Y @unchecked] => requisite match {
			case Some(req) => ComponentProperty.req(extractor.export)(req andThen sure.getter)
			case _ =>
				val first = optional; val second = sure.getter
				ComponentProperty.opt(extractor.export) { first(_).map(second) }
		}
		case _ => requisite match {
			case Some(req) => ComponentProperty.opt(extractor.export)(req andThen extractor.optional)
			case _ =>
				val first = optional; val second = extractor.optional
				ComponentProperty.opt(extractor.export) { first(_).flatMap(second) }
		}
	}



	override def andThen[Y](extractor :ColumnMappingExtract[T, Y, O]) :ColumnProperty[S, Y, O] =
		andThen(extractor :MappingExtract[T, Y, O]).asInstanceOf[ColumnProperty[S, Y, O]]



	//todo: maybe we should allow composing extractors with different origins - why not?
	def compose[X, LS <: S](extractor :ComponentProperty[X, LS, O]) :ComponentProperty[X, T, O] =
		extractor andThen this

	override def compose[X](extractor :X =?> S) :MappingExtract[X, T, O] = extractor match {
		case prop :ComponentProperty[X @unchecked, S @unchecked, _] => compose(prop)
		case _ => super.compose(extractor)
	}


	override def toString :String = "Extractor(" + export + "=" + property + ")"
}






object ComponentProperty {

	private def prop[S :TypeTag, T](extractor :S =?> T) :PropertyPath[S, T] =
		extractor.requisite.map(PropertyPath.property(_)) getOrElse
			PropertyPath.property(extractor.optional.andThen(_.get))

	private def prop[S :TypeTag, T](extractor :S => Option[T]) :PropertyPath[S, T] =
		PropertyPath.property(extractor andThen (_.get))



	def apply[S :TypeTag, T, O](component :TypedMapping[T, O])(extractor :Extractor[S, T]) :ComponentProperty[S, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				apply(column)(extractor)
			case _ => extractor match {
				case _ :OptionalExtractor[S @unchecked, T @unchecked] =>
					opt(component)(extractor.optional)
				case _ :IdentityExtractor[_] =>
					ident(component.asInstanceOf[TypedMapping[S, O]]).asInstanceOf[ComponentProperty[S, T, O]]
				case constant :ConstantExtractor[S @unchecked, T @unchecked] =>
					const(component)(constant.constant)
				case requisite :RequisiteExtractor[S @unchecked, T @unchecked] =>
					req(component)(requisite.getter)
				case _ :EmptyExtractor[_, _] =>
					none(component)
				case _ =>
					opt(component)(extractor.optional)

			}
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



	def opt[S :TypeTag, T, O](component :TypedMapping[T, O])(extract :S => Option[T]) :ComponentProperty[S, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new OptionalColumnProperty(column, extract)
			case _ =>
				new OptionalProperty(component, extract)
		}

	def opt[S :TypeTag, T, O](column :ColumnMapping[T, O])(extract :S => Option[T]) :ColumnProperty[S, T, O] =
		new OptionalColumnProperty(column, extract)



	def req[S :TypeTag, T, O](component :TypedMapping[T, O])(extract :S => T) :ComponentProperty[S, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new RequisiteColumnProperty[S, T, O](column, extract)
			case _ =>
				new RequisiteProperty(component, extract)
		}

	def req[S :TypeTag, T, O](column :ColumnMapping[T, O])(extract :S => T) :ColumnProperty[S, T, O] =
		new RequisiteColumnProperty(column, extract)



	def ident[T :TypeTag, O](component :TypedMapping[T, O]) :ComponentProperty[T, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new IdentityColumnProperty(column)
			case _ =>
				new IdentityProperty(component)
		}

	def ident[T :TypeTag, O](column :ColumnMapping[T, O]) :ColumnProperty[T, T, O] =
		new IdentityColumnProperty(column)



	def const[T, O](component :TypedMapping[T, O])(constant :T) :ComponentProperty[Any, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new ConstantColumnProperty(column, constant)
			case _ =>
				new ConstantProperty(component, constant)
		}

	def const[T, O](column :ColumnMapping[T, O])(constant :T) :ColumnProperty[Any, T, O] =
		new ConstantColumnProperty(column, constant)



	def none[T, O](component :TypedMapping[T, O]) :ComponentProperty[Any, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new EmptyColumnProperty(column)
			case _ =>
				new EmptyProperty(component)
		}

	def none[T, O](column :ColumnMapping[T, O]) :ColumnProperty[Any, T, O] =
		new EmptyColumnProperty(column)






	trait ColumnProperty[-S, T, O] extends ColumnMappingExtract[S, T, O] with ComponentProperty[S, T, O] {

		override def compose[X, LS <: S](extractor :ComponentProperty[X, LS, O]) :ColumnProperty[X, T, O] =
			extractor andThen this

		override def compose[X](extractor :X =?> S) :ColumnMappingExtract[X, T, O] = extractor match {
			case property :ComponentProperty[X @unchecked, S @unchecked, O @unchecked] =>
				property andThen this
			case _ =>
				super[ColumnMappingExtract].compose(extractor)
		}
	}






	private class OptionalProperty[-S, T, O](component :TypedMapping[T, O], f :S => Option[T])
	                                (implicit protected[this] val tag :TypeTag[S])
		extends OptionalExtract[S, T, O](component, f) with ComponentProperty[S, T, O]
	{
		override val property = prop(f)
		override val argumentType = typeOf[S]

		override def andThen[Y](extractor :MappingExtract[T, Y, O]) :ComponentProperty[S, Y, O] = extractor match {
			case _ :IdentityExtractor[Y @unchecked] =>
				if (extractor.export == export) this.asInstanceOf[ComponentProperty[S, Y, O]]
				else ComponentProperty.opt(extractor.export)(optional.asInstanceOf[S => Option[Y]])

			case requisite :RequisiteExtractor[T @unchecked, Y @unchecked] =>
				val first = optional; val second = requisite.getter
				ComponentProperty.opt(extractor.export)(first(_) map second)

			case _ :EmptyExtractor[_, _] => ComponentProperty.none(extractor.export)

			case _ =>
				val first = optional; val second = extractor.optional
				ComponentProperty.opt(extractor.export)(first(_) flatMap second)
		}

		override def toString :String = "Optional(" + export + "=" + property + ")"

	}



	private class OptionalColumnProperty[-S :TypeTag, T, O](override val export :ColumnMapping[T, O], f :S => Option[T])
		extends OptionalProperty[S, T, O](export, f) with ColumnProperty[S, T, O]






	private trait RequisitePropertyTemplate[-S, T, O] extends RequisiteExtract[S, T, O] with ComponentProperty[S, T, O] {

		override val property :ReflectedPropertyPath[S, T] = PropertyPath.property(getter)
		override val argumentType :Type = typeOf[S]

		override def andThen[Y](extractor :MappingExtract[T, Y, O]) :ComponentProperty[S, Y, O] = extractor match {
			case requisite :RequisiteExtractor[T @unchecked, Y @unchecked] => requisite match {

				case _ :IdentityExtractor[_] =>
					if (extractor.export == export) this.asInstanceOf[ComponentProperty[S, Y, O]]
					else ComponentProperty.req(extractor.export)(getter.asInstanceOf[S => Y])

				case constant :ConstantExtractor[T @unchecked, Y @unchecked] => constant match {
					case property :ConstantProperty[Y @unchecked, O @unchecked] => property
					case _ => ComponentProperty.const(constant.export)(constant.constant)
				}
				case _ => ComponentProperty.req(extractor.export)(getter andThen requisite.getter)
			}
			case empty :EmptyExtractor[Y @unchecked, O @unchecked] => empty match {
				case property :EmptyProperty[Y @unchecked, O @unchecked] => property
				case _ => ComponentProperty.none(empty.export)
			}
			case _ => ComponentProperty.opt(extractor.export)(getter andThen extractor.optional)
		}

		override def toString :String = "Requisite(" + export + "=" + property + ")"
	}



	private class RequisiteProperty[-S, T, O](component :TypedMapping[T, O], f :S => T)
	                                 (implicit protected[this] val tag :TypeTag[S])
		extends RequisiteExtract[S, T, O](component, f) with RequisitePropertyTemplate[S, T, O]



	private class RequisiteColumnProperty[-S :TypeTag, T, O](export :ColumnMapping[T, O], f :S => T)
	                                                        (implicit protected[this] val tag :TypeTag[S])
		extends RequisiteColumn[S, T, O](export, f) with ColumnProperty[S, T, O]
		   with RequisitePropertyTemplate[S, T, O]






	private class IdentityProperty[T, O](component :TypedMapping[T, O])
	                                    (implicit protected[this] val tag :TypeTag[T])
		extends IdentityExtract[T, O](component) with ComponentProperty[T, T, O]
		   with RequisitePropertyTemplate[T, T, O]
	{
		override def toString :String = "Identity(" + export + "=" + property + ")"
	}



	private class IdentityColumnProperty[T :TypeTag, O](export :ColumnMapping[T, O])
	                                                   (implicit protected[this] val tag :TypeTag[T])
		extends IdentityColumn[T, O](export) with ColumnProperty[T, T, O]
		   with RequisitePropertyTemplate[T, T, O]
	{
		override def toString :String = "Identity(" + export + "=" + property + ")"
	}







	private class ConstantProperty[T, O](component :TypedMapping[T, O], constant :T)
		extends ConstantExtract[T, O](component, constant) with ComponentProperty[Any, T, O]
		   with RequisitePropertyTemplate[Any, T, O]
	{
		protected[this] implicit override def tag :TypeTag[Any] = typeTag[Any]

		override def toString :String = "Constant(" + export + "=" + constant + ")"
	}



	private class ConstantColumnProperty[T, O](override val export :ColumnMapping[T, O], const :T)
		extends ConstantColumn[T, O](export, const) with ColumnProperty[Any, T, O]
		   with RequisitePropertyTemplate[Any, T, O]
	{
		protected[this] implicit override def tag :TypeTag[Any] = typeTag[Any]

		override def toString :String = "Constant(" + export + "=" + constant + ")"
	}





	private class EmptyProperty[T, O](component :TypedMapping[T, O])
		extends EmptyExtract[T, O](component) with ComponentProperty[Any, T, O]
	{
		override val property = prop(optional) //fixme: PropertyPath will throw up
		override val argumentType = typeOf[Any]
		protected[this] override val tag :TypeTag[Any] = typeTag[Any]

		override def toString :String = "Empty(" + export + "=<None>)"
	}



	private class EmptyColumnProperty[T, O](override val export :ColumnMapping[T, O])
		extends EmptyColumn[T, O](export) with ColumnProperty[Any, T, O]
	{
		override val property = prop(optional) //fixme: PropertyPath will throw up
		override val argumentType = typeOf[Any]
		protected[this] override val tag :TypeTag[Any] = typeTag[Any]

		override def toString :String = "Empty(" + export + "=<None>)"
	}



}

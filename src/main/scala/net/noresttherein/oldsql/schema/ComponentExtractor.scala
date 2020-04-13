package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.TypedMapping



/** A `ComponentExtractor` describes the parent-child relationship between a mapping and its component.
  * It serves three functions:
  *   - provides a means of extracting the value of the component from the value of the parent;
  *   - retrieves the value of a component from `ComponentValues`;
  *   - provides the canonical, 'export' version of the component, that is the version with any wholesale
  *     modifications declared in the parent mapping (or some other mapping on the path to the subcomponent),
  *     applied to the original version of the component. This includes buffs and column prefix declarations
  *     defined for all subcomponents of a mapping.
  * @see [[net.noresttherein.oldsql.schema.Mapping.apply[T](TypedMapping[T] ]]
  * @see [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]]
  */
trait ComponentExtractor[-S, T, O] extends Extractor[S, T] {
	val export :TypedMapping[T, O]

	override def compose[X](extractor :Extractor[X, S]) :ComponentExtractor[X, T, O] = extractor match {
		case _ :IdentityExtractor[_] => this.asInstanceOf[ComponentExtractor[X, T, O]]

		case const :ConstantExtractor[_, S] => try {
			ComponentExtractor.const(export)(apply(const.constant))
		} catch {
			case _ :Exception => ComponentExtractor.opt(export)(const.getter andThen optional)
		}

		case req :RequisiteExtractor[X, S] =>
			ComponentExtractor.opt(export)(req.getter andThen optional)

		case _ :EmptyExtractor[_, _] =>
			ComponentExtractor.none[T, O](export)

		case _ =>
			val first = extractor.optional; val second = optional
			ComponentExtractor.opt(export)(first(_) flatMap second)
	}

	override def compose[X](first :X => S) :ComponentExtractor[X, T, O] =
		ComponentExtractor.opt(export)(first andThen optional)

	def andThen[Y](selector :ComponentExtractor[T, Y, O]) :ComponentExtractor[S, Y, O] =
		selector compose this

	override def toString :String = "Extractor(" + export + ")"
}






object ComponentExtractor {
	def apply[S, T, O](component :TypedMapping[T, O], pick :S => Option[T], surepick :Option[S => T]) :ComponentExtractor[S, T, O] =
		surepick match {
			case Some(sure) => req(component)(sure)
			case _ => opt(component)(pick)
		}

	def apply[S, T, O](component :TypedMapping[T, O])(extractor :Extractor[S, T]) :ComponentExtractor[S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => ident(component).asInstanceOf[ComponentExtractor[S, T, O]]
			case c :ConstantExtractor[_, T] => const(component)(c.constant)
			case requisite :RequisiteExtractor[S, T] => new RequisiteComponent(component, requisite.getter)
			case _ :EmptyExtractor[_, _] => none(component)
			case _ => new OptionalComponent(component, extractor.optional) //todo: FromOptionExtractor
		}



	def req[S, T, O](component :TypedMapping[T, O])(requisite :S => T) :ComponentExtractor[S, T, O] =
		new RequisiteComponent[S, T, O](component, requisite)

	def opt[S, T, O](component :TypedMapping[T, O])(selector :S => Option[T]) :ComponentExtractor[S, T, O] =
		new OptionalComponent[S, T, O](component, selector)

	def ident[T, O](component :TypedMapping[T, O]) :ComponentExtractor[T, T, O] =
		new IdentityComponent[T, O](component)

	def const[T, O](component :TypedMapping[T, O])(value :T) :ComponentExtractor[Any, T, O] =
		new ConstantComponent[T, O](component, value)

	def none[T, O](component :TypedMapping[T, O]) :ComponentExtractor[Any, T, O] =
		new EmptyComponent(component)



	def unapply[S, T, O](selector :ComponentExtractor[S, T, O]) :Option[(S => Option[T], Option[S => T], TypedMapping[T, O])] =
		Some(selector.optional, selector.requisite, selector.export)



	class OptionalComponent[S, T, O](val export :TypedMapping[T, O], override val optional :S => Option[T])
		extends ComponentExtractor[S, T, O] with OptionalExtractor[S, T]
	{
		override def get(x :S) :Option[T] = optional(x)

		override def toString :String = "Optional(" + export + ")"
	}



	class RequisiteComponent[-S, T, O](final val export :TypedMapping[T, O], override val getter :S => T)
		extends ComponentExtractor[S, T, O] with RequisiteExtractor[S, T]
	{
		override def apply(x :S) :T = getter(x)

		override def compose[X](extractor :X =?> S) :ComponentExtractor[X, T, O] = extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[ComponentExtractor[X, T, O]]
			case _ :ConstantExtractor[_, _] => super.compose(extractor)
			case req :RequisiteExtractor[X, S] =>
				new RequisiteComponent[X, T, O](export, req.getter andThen getter)
			case _ =>
				val first = extractor.optional
				new OptionalComponent[X, T, O](export, first(_).map(getter))
		}

		override def compose[W](extractor :RequisiteExtractor[W, S]) :RequisiteComponent[W, T, O] = extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[RequisiteComponent[W, T, O]]
			case const :ConstantExtractor[_, S] => try {
				new ConstantComponent[T, O](export, getter(const.constant))
			} catch {
				case _ :Exception => new RequisiteComponent[W, T, O](export, extractor.getter andThen getter)
			}
			case _ =>
				new RequisiteComponent[W, T, O](export, extractor.getter andThen getter)
		}

		override def compose[W](req :W => S) :RequisiteComponent[W, T, O] =
			new RequisiteComponent[W, T, O](export, req andThen getter)

		override def toString :String = "Requisite(" + export + ")"
	}



	class IdentityComponent[S, O](component :TypedMapping[S, O])
		extends RequisiteComponent[S, S, O](component, identity[S]) with IdentityExtractor[S]
	{
		override def compose[W](extractor :W =?> S) :ComponentExtractor[W, S, O] = extractor match {
			case comp :ComponentExtractor[_, _, _] if comp.export == export =>
				comp.asInstanceOf[ComponentExtractor[W, S, O]]

			case _ :IdentityExtractor[_] => this.asInstanceOf[RequisiteComponent[W, S, O]]

			case c :ConstantExtractor[_, _] => const(export)(c.constant.asInstanceOf[S])

			case e :RequisiteExtractor[_, _] => req(export)(e.getter.asInstanceOf[W => S])

			case _ => opt(export)(extractor.optional)
		}

		override def compose[W](extractor :RequisiteExtractor[W, S]) :RequisiteComponent[W, S, O] = extractor match {
			case comp :RequisiteComponent[_, _, _] => comp.asInstanceOf[RequisiteComponent[W, S, O]]
			case _ :IdentityExtractor[_] => this.asInstanceOf[RequisiteComponent[W, S, O]]
			case _ => new RequisiteComponent[W, S, O](export, extractor.getter)
		}

		override def compose[W](req :W => S) = new RequisiteComponent[W, S, O](export, req)

		override def toString :String = "Identity(" + export + ")"
	}



	class ConstantComponent[S, O](component :TypedMapping[S, O], const :S)
		extends RequisiteComponent[Any, S, O](component, (_ :Any) => const) with ConstantExtractor[Any, S]
	{
		override def constant :S = const

		override def compose[W](extractor :W =?> Any) :ComponentExtractor[W, S, O] = this

		override def compose[W](extractor :RequisiteExtractor[W, Any]) :RequisiteComponent[W, S, O] = this

		override def compose[W](req :W => Any) :RequisiteComponent[W, S, O] = this

		override def toString :String = "Const(" + component + "=" + constant + ")"
	}



	class EmptyComponent[T, O](component :TypedMapping[T, O])
		extends OptionalComponent[Any, T, O](component, Extractor.none.optional) with EmptyExtractor[Any, T]
	{
		override def compose[W](extractor :W =?> Any) :EmptyComponent[T, O] = this

		override def compose[W](req :W => Any) :EmptyComponent[T, O] = this

		override def toString :String = "Empty(" + component + ")"
	}


}

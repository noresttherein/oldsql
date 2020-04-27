package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ComponentExtractor.{ColumnExtractor, ComponentExtractorTemplate, ConstantComponent, RequisiteComponent}
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
trait ComponentExtractor[-S, T, O]
	extends ComponentExtractorTemplate[S, T, O, ({ type E[-X] = ComponentExtractor[X, T, O] })#E,
	                                            ({ type R[-X] = RequisiteComponent[X, T, O] })#R]
{
	val export :TypedMapping[T, O]

	def andThen[Y](selector :ComponentExtractor[T, Y, O]) :ComponentExtractor[S, Y, O] =
		selector compose this

	def andThen[Y](selector :ColumnExtractor[T, Y, O]) :ColumnExtractor[S, Y, O] =
		selector compose this



	protected override def opt[X](f :X => Option[T]) :ComponentExtractor[X, T, O] =
		ComponentExtractor.opt(export)(f)

	protected override def req[X](f :X => T) :RequisiteComponent[X, T, O] =
		new RequisiteComponent[X, T, O](export, f)

	protected override def const[X](constant :T) :RequisiteComponent[X, T, O] =
		new ConstantComponent[T, O](export, constant)

	protected override def none[X] :ComponentExtractor[X, T, O] = ComponentExtractor.none[T, O](export)



	override def toString :String = "Extractor(" + export + ")"
}






object ComponentExtractor {

	def apply[S, T, O](component :TypedMapping[T, O], pick :S => Option[T], surepick :Option[S => T]) :ComponentExtractor[S, T, O] =
		surepick match {
			case Some(sure) => req(component)(sure)
			case _ => opt(component)(pick)
		}

	def apply[S, T, O](column :ColumnMapping[T, O], pick :S => Option[T], surepick :Option[S => T]) :ColumnExtractor[S, T, O] =
		surepick match {
			case Some(sure) => req(column)(sure)
			case _ => opt(column)(pick)
		}

	def apply[S, T, O](component :TypedMapping[T, O])(extractor :Extractor[S, T]) :ComponentExtractor[S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => ident(component).asInstanceOf[ComponentExtractor[S, T, O]]
			case c :ConstantExtractor[_, T] => const(component)(c.constant)
			case requisite :RequisiteExtractor[S, T] => req(component)(requisite.getter)
			case _ :EmptyExtractor[_, _] => none(component)
			case _ => opt(component)(extractor.optional) //todo: FromOptionExtractor
		}

	def apply[S, T, O](column :ColumnMapping[T, O])(extractor :Extractor[S, T]) :ColumnExtractor[S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => ident(column).asInstanceOf[ColumnExtractor[S, T, O]]
			case c :ConstantExtractor[_, T] => const(column)(c.constant)
			case requisite :RequisiteExtractor[S, T] => req(column)(requisite.getter)
			case _ :EmptyExtractor[_, _] => none(column)
			case _ => opt(column)(extractor.optional) //todo: FromOptionExtractor
		}



	def opt[S, T, O](component :TypedMapping[T, O])(selector :S => Option[T]) :ComponentExtractor[S, T, O] =
		component match {
			case col :ColumnMapping[T @unchecked , O @unchecked] =>
				new OptionalColumn[S, T, O](col, selector)
			case _ =>
				new OptionalComponent[S, T, O](component, selector)
		}

	def opt[S, T, O](column :ColumnMapping[T, O])(selector :S => Option[T]) :ColumnExtractor[S, T, O] =
		new OptionalColumn[S, T, O](column, selector)


	def req[S, T, O](component :TypedMapping[T, O])(requisite :S => T) :ComponentExtractor[S, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new RequisiteColumn[S, T, O](column, requisite)
			case _ =>
				new RequisiteComponent[S, T, O](component, requisite)
		}

	def req[S, T, O](column :ColumnMapping[T, O])(requisite :S => T) :ColumnExtractor[S, T, O] =
		new RequisiteColumn[S, T, O](column, requisite)


	def ident[T, O](component :TypedMapping[T, O]) :ComponentExtractor[T, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new IdentityColumn[T, O](column)
			case _ =>
				new IdentityComponent[T, O](component)
		}

	def ident[T, O](column :ColumnMapping[T, O]) :ColumnExtractor[T, T, O] =
		new IdentityColumn[T, O](column)


	def const[T, O](component :TypedMapping[T, O])(value :T) :ComponentExtractor[Any, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new ConstantColumn[T, O](column, value)
			case _ =>
				new ConstantComponent[T, O](component, value)
		}

	def const[T, O](column :ColumnMapping[T, O])(value :T) :ColumnExtractor[Any, T, O] =
		new ConstantColumn[T, O](column, value)


	def none[T, O](component :TypedMapping[T, O]) :ComponentExtractor[Any, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new EmptyColumn(column)
			case _ =>
				new EmptyComponent(component)
		}

	def none[T, O](column :ColumnMapping[T, O]) :ColumnExtractor[Any, T, O] =
		new EmptyColumn(column)



	def unapply[S, T, O](selector :ComponentExtractor[S, T, O]) :Option[(S => Option[T], Option[S => T], TypedMapping[T, O])] =
		Some(selector.optional, selector.requisite, selector.export)

	def unapply[S, T](selector :Extractor[S, T]) :Option[(S => Option[T], Option[S => T], TypedMapping[T, _])] =
		selector match {
			case c :ComponentExtractor[_, _, _] =>
				Some((selector.optional, selector.requisite, c.export.asInstanceOf[TypedMapping[T, _]]))
			case _ =>
				None
		}






	trait ComponentExtractorTemplate[-S, T, O, +E[-X] <: ComponentExtractor[X, T, O],
	                                           +R[-X] <: E[X] with RequisiteExtractor[X, T]]
		extends Extractor[S, T]
	{ this :E[S] with ComponentExtractorTemplate[S, T, O, E, R] =>

		override def compose[X](extractor :Extractor[X, S]) :E[X] = extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[E[X]]

			case c :ConstantExtractor[_, S] => try {
				(this :ComponentExtractorTemplate[S, T, O, E, R]).const(apply(c.constant))
			} catch {
				case _ :Exception =>
					(this :ComponentExtractorTemplate[S, T, O, E, R]).opt(c.getter andThen optional)
			}

			case req :RequisiteExtractor[X, S] =>
				(this :ComponentExtractorTemplate[S, T, O, E, R]).opt(req.getter andThen optional)

			case _ :EmptyExtractor[_, _] =>
				(this :ComponentExtractorTemplate[S, T, O, E, R]).none

			case _ => composeOpt(extractor.optional)
		}

		override def compose[X](first :X => S) :E[X] =
			(this :ComponentExtractorTemplate[S, T, O, E, R]).opt(first andThen optional)

		override def composeOpt[X](first :X => Option[S]) :E[X] = {
			val pick = optional
			(this :ComponentExtractorTemplate[S, T, O, E, R]).opt(first(_) flatMap pick)
		}


		protected def opt[X](f :X => Option[T]) :E[X]

		protected def req[X](f :X => T) :R[X]

		protected def const[X](constant :T) :R[X]

		protected def none[X] :E[X]


		override def toString :String = "Optional(" + export + ")"
	}



	trait RequisiteComponentTemplate[-S, T, O, +E[-X] <: ComponentExtractor[X, T, O],
	                                           +R[-X] <: E[X] with RequisiteExtractor[X, T]]
		extends ComponentExtractorTemplate[S, T, O, E, R] with RequisiteExtractor[S, T]
	{ this :R[S] with RequisiteComponentTemplate[S, T, O, E, R] =>

		override def compose[X](extractor :X =?> S) :E[X] = extractor match {
			case r :RequisiteExtractor[X, S] =>
				(this :RequisiteComponentTemplate[S, T, O, E, R]).req(r.getter andThen getter)
			case _ =>
				val first = extractor.optional; val second = getter
				(this :RequisiteComponentTemplate[S, T, O, E, R]).opt[X](first(_).map(second))
		}

		override def compose[X](extractor :RequisiteExtractor[X, S]) :R[X] = extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[R[X]]

			case c :ConstantExtractor[_, S] => try {
				(this :RequisiteComponentTemplate[S, T, O, E, R]).const(getter(c.constant))
			} catch {
				case _ :Exception =>
					(this :RequisiteComponentTemplate[S, T, O, E, R]).req(extractor.getter andThen getter)
			}
			case _ =>
				(this :RequisiteComponentTemplate[S, T, O, E, R]).req(extractor.getter andThen getter)
		}

		override def compose[X](f :X => S) :R[X] =
			(this :RequisiteComponentTemplate[S, T, O, E, R]).req[X](f andThen getter)

		override def composeOpt[X](f :X => Option[S]) :E[X] = {
			val get = getter; (this :RequisiteComponentTemplate[S, T, O, E, R]).opt(f(_) map get)
		}

		override def toString :String = "Requisite(" + export + ")"

	}



	trait IdentityComponentTemplate[T, O, +E[-X] <: ComponentExtractor[X, T, O],
	                                      +R[-X] <: E[X] with RequisiteExtractor[X, T]]
		extends RequisiteComponentTemplate[T, T, O, E, R] with IdentityExtractor[T]
	{ this :R[T] with IdentityComponentTemplate[T, O, E, R] =>

		override def compose[X](extractor :X =?> T) :E[X] = extractor match {
			case e :RequisiteExtractor[_, _] => this compose e

			case comp :ComponentExtractor[_, _, _] if comp.export == export =>
				comp.asInstanceOf[E[X]]

			case _ => (this :IdentityComponentTemplate[T, O, E, R]).opt(extractor.optional)
		}

		override def compose[X](extractor :RequisiteExtractor[X, T]) :R[X] = extractor match {
			case comp :RequisiteComponent[_, _, _] => comp.asInstanceOf[R[X]]
			case _ :IdentityExtractor[_] => this.asInstanceOf[R[X]]
			case _ => (this :IdentityComponentTemplate[T, O, E, R]).req[X](extractor.getter)
		}

		override def compose[X](r :X => T) = (this :IdentityComponentTemplate[T, O, E, R]).req[X](r)

		override def composeOpt[X](f :X => Option[T]) :E[X] =
			(this :IdentityComponentTemplate[T, O, E, R]).opt(f)

		override def toString :String = "Identity(" + export + ")"
	}



	trait ConstantComponentTemplate[T, O, +E[-X] <: ComponentExtractor[X, T, O],
	                                      +R[-X] <: E[X] with RequisiteExtractor[X, T]]
		extends RequisiteComponentTemplate[Any, T, O, E, R] with ConstantExtractor[Any, T]
	{ this :R[Any] with ConstantComponentTemplate[T, O, E, R] =>

		override def compose[X](extractor :X =?> Any) :E[X] = extractor match {
			case _ :RequisiteExtractor[_, _] => this
			case _ :EmptyExtractor[_, _] => (this :ConstantComponentTemplate[T, O, E, R]).none
			case _ => (this :ConstantComponentTemplate[T, O, E, R]).opt(extractor.optional(_) map getter)
		}

		override def compose[X](extractor :RequisiteExtractor[X, Any]) :R[X] = this

		override def compose[X](req :X => Any) :R[X] = this

		override def composeOpt[X](f :X => Option[Any]) :E[X] = super[RequisiteComponentTemplate].composeOpt(f)

		override def toString :String = "Const(" + export + "=" + constant + ")"

	}



	trait EmptyComponentTemplate[T, O, +E[-X] <: ComponentExtractor[X, T, O],
	                                   +R[-X] <: E[X] with RequisiteExtractor[X, T]]
		extends ComponentExtractorTemplate[T, T, O, E, R] with EmptyExtractor[Any, T]
	{ this :E[Any] with EmptyComponentTemplate[T, O, E, R] =>

		override def compose[X](extractor :X =?> Any) :E[X] =
			(this :EmptyComponentTemplate[T, O, E, R]).none

		override def compose[X](req :X => Any) :E[X] =
			(this :EmptyComponentTemplate[T, O, E, R]).none

		override def composeOpt[X](f :X => Option[Any]) :E[X] =
			(this :EmptyComponentTemplate[T, O, E, R]).none

		override def toString :String = "Empty(" + export + ")"
	}






	trait ColumnExtractor[-S, T, O] extends ComponentExtractor[S, T, O]
		with ComponentExtractorTemplate[S, T, O, ({ type E[-X] = ColumnExtractor[X, T, O] })#E,
		                                         ({ type R[-X] = RequisiteColumn[X, T, O] })#R]
	{
		override val export :ColumnMapping[T, O]

		protected override def opt[X](f :X => Option[T]) :ColumnExtractor[X, T, O] =
			ComponentExtractor.opt(export)(f)

		protected override def req[X](f :X => T) :RequisiteColumn[X, T, O] =
			new RequisiteColumn[X, T, O](export, f)

		protected override def const[X](constant :T) :RequisiteColumn[X, T, O] =
			new ConstantColumn[T, O](export, constant)

		protected override def none[X] :ColumnExtractor[X, T, O] =
			ComponentExtractor.none(export)
	}






	class OptionalComponent[-S, T, O](val export :TypedMapping[T, O], override val optional :S => Option[T])
		extends ComponentExtractor[S, T, O] with OptionalExtractor[S, T]
	{
		override def get(x :S) :Option[T] = optional(x)

//		override def toString :String = "Optional(" + export + ")"
	}



	class OptionalColumn[-S, T, O](override val export :ColumnMapping[T, O], optional :S => Option[T])
		extends OptionalComponent[S, T, O](export, optional) with ColumnExtractor[S, T, O]



	class RequisiteComponent[-S, T, O](override val export :TypedMapping[T, O], override val getter :S => T)
		extends ComponentExtractor[S, T, O]
		   with RequisiteComponentTemplate[S, T, O, ({ type E[-X] = ComponentExtractor[X, T, O] })#E,
			                                        ({ type R[-X] = RequisiteComponent[X, T, O] })#R]
	{
		override def apply(x :S) :T = getter(x)

//		override def toString :String = "Requisite(" + export + ")"
	}



	class RequisiteColumn[-S, T, O](override val export :ColumnMapping[T, O], pick :S => T)
		extends RequisiteComponent[S, T, O](export, pick) with ColumnExtractor[S, T, O]
		   with RequisiteComponentTemplate[S, T, O, ({ type E[-X] = ColumnExtractor[X, T, O] })#E,
			                                        ({ type R[-X] = RequisiteColumn[X, T, O] })#R]



	class IdentityComponent[T, O](component :TypedMapping[T, O])
		extends RequisiteComponent[T, T, O](component, identity[T])
		   with IdentityComponentTemplate[T, O, ({ type E[-X] = ComponentExtractor[X, T, O] })#E,
		                                        ({ type R[-X] = RequisiteComponent[X, T, O] })#R]
//	{
//		override def toString :String = "Identity(" + export + ")"
//	}



	class IdentityColumn[T, O](column :ColumnMapping[T, O])
		extends RequisiteColumn[T, T, O](column, identity[T])
		   with IdentityComponentTemplate[T, O, ({ type E[-X] = ColumnExtractor[X, T, O] })#E,
			                                    ({ type R[-X] = RequisiteColumn[X, T, O] })#R]
//	{
//		override def toString :String = "Identity(" + export + ")"
//	}






	class ConstantComponent[T, O](component :TypedMapping[T, O], override val constant :T)
		extends RequisiteComponent[Any, T, O](component, (_ :Any) => constant)
		   with ConstantComponentTemplate[T, O, ({ type E[-X] = ComponentExtractor[X, T, O] })#E,
		                                        ({ type R[-X] = RequisiteComponent[X, T, O] })#R]
//	{
//		override def toString :String = "Const(" + export + "=" + constant + ")"
//	}



	class ConstantColumn[T, O](column :ColumnMapping[T, O], override val constant :T)
		extends RequisiteColumn[Any, T, O](column, (_ :Any) => constant)
		   with ConstantComponentTemplate[T, O, ({ type E[-X] = ColumnExtractor[X, T, O] })#E,
		                                        ({ type R[-X] = RequisiteColumn[X, T, O] })#R]
//	{
//		override def toString :String = "Const(" + export + "=" + constant + ")"
//	}



	class EmptyComponent[T, O](component :TypedMapping[T, O])
		extends OptionalComponent[Any, T, O](component, Extractor.none.optional)
		   with EmptyComponentTemplate[T, O, ({ type E[-X] = ComponentExtractor[X, T, O] })#E,
		                                     ({ type R[-X] = RequisiteComponent[X, T, O] })#R]


	class EmptyColumn[T, O](column :ColumnMapping[T, O])
		extends OptionalColumn[Any, T, O](column, Extractor.none.optional)
		   with EmptyComponentTemplate[T, O, ({ type E[-X] = ColumnExtractor[X, T, O] })#E,
		                                     ({ type R[-X] = RequisiteColumn[X, T, O] })#R]


}

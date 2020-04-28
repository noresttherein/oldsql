package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.MappingExtract.{ColumnExtract, MappingExtractTemplate, ConstantExtract, RequisiteExtract}
import net.noresttherein.oldsql.schema.Mapping.TypedMapping



/** A `MappingExtract` describes the parent-child relationship between a mapping and its component.
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
trait MappingExtract[-S, T, O]
	extends MappingExtractTemplate[S, T, O, ({ type E[-X] = MappingExtract[X, T, O] })#E]
{
	val export :TypedMapping[T, O]

	def andThen[Y](selector :MappingExtract[T, Y, O]) :MappingExtract[S, Y, O] =
		selector compose this

	def andThen[Y](selector :ColumnExtract[T, Y, O]) :ColumnExtract[S, Y, O] =
		selector compose this



	protected[schema] override def opt[X](f :X => Option[T]) :MappingExtract[X, T, O] =
		MappingExtract.opt(export)(f)

	protected[schema] override def req[X](f :X => T) :RequisiteExtract[X, T, O] =
		new RequisiteExtract[X, T, O](export, f)

	protected[schema] override def const[X](constant :T) :RequisiteExtract[X, T, O] =
		new ConstantExtract[T, O](export, constant)

	protected[schema] override def none[X] :MappingExtract[X, T, O] =
		MappingExtract.none[T, O](export)



	override def toString :String = "Extractor(" + export + ")"
}






object MappingExtract {

	def apply[S, T, O](component :TypedMapping[T, O], pick :S => Option[T], surepick :Option[S => T])
			:MappingExtract[S, T, O] =
		surepick match {
			case Some(sure) => req(component)(sure)
			case _ => opt(component)(pick)
		}

	def apply[S, T, O](column :ColumnMapping[T, O], pick :S => Option[T], surepick :Option[S => T])
			:ColumnExtract[S, T, O] =
		surepick match {
			case Some(sure) => req(column)(sure)
			case _ => opt(column)(pick)
		}

	def apply[S, T, O](component :TypedMapping[T, O])(extractor :Extractor[S, T]) :MappingExtract[S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => ident(component).asInstanceOf[MappingExtract[S, T, O]]
			case c :ConstantExtractor[_, T @unchecked] => const(component)(c.constant)
			case requisite :RequisiteExtractor[S @unchecked, T @unchecked] => req(component)(requisite.getter)
			case _ :EmptyExtractor[_, _] => none(component)
			case _ => opt(component)(extractor.optional) //todo: FromOptionExtractor
		}

	def apply[S, T, O](column :ColumnMapping[T, O])(extractor :Extractor[S, T]) :ColumnExtract[S, T, O] =
		extractor match {
			case _ :IdentityExtractor[_] => ident(column).asInstanceOf[ColumnExtract[S, T, O]]
			case c :ConstantExtractor[_, T @unchecked] => const(column)(c.constant)
			case requisite :RequisiteExtractor[S @unchecked, T @unchecked] => req(column)(requisite.getter)
			case _ :EmptyExtractor[_, _] => none(column)
			case _ => opt(column)(extractor.optional) //todo: FromOptionExtractor
		}



	def opt[S, T, O](component :TypedMapping[T, O])(selector :S => Option[T]) :MappingExtract[S, T, O] =
		component match {
			case col :ColumnMapping[T @unchecked , O @unchecked] =>
				new OptionalColumn[S, T, O](col, selector)
			case _ =>
				new OptionalExtract[S, T, O](component, selector)
		}

	def opt[S, T, O](column :ColumnMapping[T, O])(selector :S => Option[T]) :ColumnExtract[S, T, O] =
		new OptionalColumn[S, T, O](column, selector)


	def req[S, T, O](component :TypedMapping[T, O])(requisite :S => T) :MappingExtract[S, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new RequisiteColumn[S, T, O](column, requisite)
			case _ =>
				new RequisiteExtract[S, T, O](component, requisite)
		}

	def req[S, T, O](column :ColumnMapping[T, O])(requisite :S => T) :ColumnExtract[S, T, O] =
		new RequisiteColumn[S, T, O](column, requisite)


	def ident[T, O](component :TypedMapping[T, O]) :MappingExtract[T, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new IdentityColumn[T, O](column)
			case _ =>
				new IdentityExtract[T, O](component)
		}

	def ident[T, O](column :ColumnMapping[T, O]) :ColumnExtract[T, T, O] =
		new IdentityColumn[T, O](column)


	def const[T, O](component :TypedMapping[T, O])(value :T) :MappingExtract[Any, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new ConstantColumn[T, O](column, value)
			case _ =>
				new ConstantExtract[T, O](component, value)
		}

	def const[T, O](column :ColumnMapping[T, O])(value :T) :ColumnExtract[Any, T, O] =
		new ConstantColumn[T, O](column, value)


	def none[T, O](component :TypedMapping[T, O]) :MappingExtract[Any, T, O] =
		component match {
			case column :ColumnMapping[T @unchecked, O @unchecked] =>
				new EmptyColumn(column)
			case _ =>
				new EmptyExtract(component)
		}

	def none[T, O](column :ColumnMapping[T, O]) :ColumnExtract[Any, T, O] =
		new EmptyColumn(column)



	def unapply[S, T, O](selector :MappingExtract[S, T, O]) :Option[(S => Option[T], Option[S => T], TypedMapping[T, O])] =
		Some(selector.optional, selector.requisite, selector.export)

	def unapply[S, T](selector :Extractor[S, T]) :Option[(S => Option[T], Option[S => T], TypedMapping[_ <: T, _])] =
		selector match {
			case c :MappingExtract[S @unchecked, T @unchecked, _] =>
				Some((selector.optional, selector.requisite, c.export))
			case _ =>
				None
		}






	trait MappingExtractTemplate[-S, T, O, +E[-X] <: MappingExtract[X, T, O]]
		extends Extractor[S, T]
	{ this :E[S] with MappingExtractTemplate[S, T, O, E] =>

		override def compose[X](extractor :Extractor[X, S]) :E[X] = extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[E[X]]

			case c :ConstantExtractor[_, S] => try {
				(this :MappingExtractTemplate[S, T, O, E]).const(apply(c.constant))
			} catch {
				case _ :Exception =>
					(this :MappingExtractTemplate[S, T, O, E]).opt(c.getter andThen optional)
			}

			case req :RequisiteExtractor[X, S] =>
				(this :MappingExtractTemplate[S, T, O, E]).opt(req.getter andThen optional)

			case _ :EmptyExtractor[_, _] =>
				(this :MappingExtractTemplate[S, T, O, E]).none

			case _ => composeOpt(extractor.optional)
		}

		override def compose[X](first :X => S) :E[X] =
			(this :MappingExtractTemplate[S, T, O, E]).opt(first andThen optional)

		override def composeOpt[X](first :X => Option[S]) :E[X] = {
			val pick = optional
			(this :MappingExtractTemplate[S, T, O, E]).opt(first(_) flatMap pick)
		}


		protected[schema] def opt[X](f :X => Option[T]) :E[X]

		protected[schema] def req[X](f :X => T) :E[X]

		protected[schema] def const[X](constant :T) :E[X]

		protected[schema] def none[X] :E[X]


		override def toString :String = "Optional(" + export + ")"
	}



	private[schema] trait RequisiteExtractTemplate[-S, T, O, +E[-X] <: MappingExtract[X, T, O],
	                                                         +R[-X] <: E[X] with RequisiteExtractor[X, T]]
		extends MappingExtractTemplate[S, T, O, E] with RequisiteExtractor[S, T]
	{ this :R[S] with RequisiteExtractTemplate[S, T, O, E, R] =>

		override def compose[X](extractor :X =?> S) :E[X] = extractor match {
			case r :RequisiteExtractor[X, S] =>
				(this :RequisiteExtractTemplate[S, T, O, E, R]).req(r.getter andThen getter)
			case _ =>
				val first = extractor.optional; val second = getter
				(this :RequisiteExtractTemplate[S, T, O, E, R]).opt[X](first(_).map(second))
		}

		override def compose[X](extractor :RequisiteExtractor[X, S]) :R[X] = extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[R[X]]

			case c :ConstantExtractor[_, S] => try {
				(this :RequisiteExtractTemplate[S, T, O, E, R]).const(getter(c.constant))
			} catch {
				case _ :Exception =>
					(this :RequisiteExtractTemplate[S, T, O, E, R]).req(extractor.getter andThen getter)
			}
			case _ =>
				(this :RequisiteExtractTemplate[S, T, O, E, R]).req(extractor.getter andThen getter)
		}

		override def compose[X](f :X => S) :R[X] =
			(this :RequisiteExtractTemplate[S, T, O, E, R]).req[X](f andThen getter)

		override def composeOpt[X](f :X => Option[S]) :E[X] = {
			val get = getter; (this :RequisiteExtractTemplate[S, T, O, E, R]).opt(f(_) map get)
		}


		protected[schema] override def req[X](f :X => T) :R[X]

		protected[schema] override def const[X](constant :T) :R[X]


		override def toString :String = "Requisite(" + export + ")"

	}



	private[schema] trait IdentityExtractTemplate[T, O, +E[-X] <: MappingExtract[X, T, O],
	                                                    +R[-X] <: E[X] with RequisiteExtractor[X, T]]
		extends RequisiteExtractTemplate[T, T, O, E, R] with IdentityExtractor[T]
	{ this :R[T] with IdentityExtractTemplate[T, O, E, R] =>

		override def compose[X](extractor :X =?> T) :E[X] = extractor match {
			case e :RequisiteExtractor[_, _] => this compose e

			case comp :MappingExtract[_, _, _] if comp.export == export =>
				comp.asInstanceOf[E[X]]

			case _ => (this :IdentityExtractTemplate[T, O, E, R]).opt(extractor.optional)
		}

		override def compose[X](extractor :RequisiteExtractor[X, T]) :R[X] = extractor match {
			case comp :RequisiteExtract[_, _, _] => comp.asInstanceOf[R[X]]
			case _ :IdentityExtractor[_] => this.asInstanceOf[R[X]]
			case _ => (this :IdentityExtractTemplate[T, O, E, R]).req[X](extractor.getter)
		}

		override def compose[X](r :X => T) = (this :IdentityExtractTemplate[T, O, E, R]).req[X](r)

		override def composeOpt[X](f :X => Option[T]) :E[X] =
			(this :IdentityExtractTemplate[T, O, E, R]).opt(f)

		override def toString :String = "Identity(" + export + ")"
	}



	private[schema] trait ConstantExtractTemplate[T, O, +E[-X] <: MappingExtract[X, T, O],
	                                                    +R[-X] <: E[X] with RequisiteExtractor[X, T]]
		extends RequisiteExtractTemplate[Any, T, O, E, R] with ConstantExtractor[Any, T]
	{ this :R[Any] with ConstantExtractTemplate[T, O, E, R] =>

		override def compose[X](extractor :X =?> Any) :E[X] = extractor match {
			case _ :RequisiteExtractor[_, _] => this
			case _ :EmptyExtractor[_, _] => (this :ConstantExtractTemplate[T, O, E, R]).none
			case _ => (this :ConstantExtractTemplate[T, O, E, R]).opt(extractor.optional(_) map getter)
		}

		override def compose[X](extractor :RequisiteExtractor[X, Any]) :R[X] = this

		override def compose[X](req :X => Any) :R[X] = this

		override def composeOpt[X](f :X => Option[Any]) :E[X] = super[RequisiteExtractTemplate].composeOpt(f)

		override def toString :String = "Const(" + export + "=" + constant + ")"

	}



	private[schema] trait EmptyExtractTemplate[T, O, +E[-X] <: MappingExtract[X, T, O]]
		extends MappingExtractTemplate[T, T, O, E] with EmptyExtractor[Any, T]
	{ this :E[Any] with EmptyExtractTemplate[T, O, E] =>

		override def compose[X](extractor :X =?> Any) :E[X] =
			(this :EmptyExtractTemplate[T, O, E]).none

		override def compose[X](req :X => Any) :E[X] =
			(this :EmptyExtractTemplate[T, O, E]).none

		override def composeOpt[X](f :X => Option[Any]) :E[X] =
			(this :EmptyExtractTemplate[T, O, E]).none

		override def toString :String = "Empty(" + export + ")"
	}






	trait ColumnExtract[-S, T, O] extends MappingExtract[S, T, O]
		with MappingExtractTemplate[S, T, O, ({ type E[-X] = ColumnExtract[X, T, O] })#E]
	{
		override val export :ColumnMapping[T, O]

		protected[schema] override def opt[X](f :X => Option[T]) :ColumnExtract[X, T, O] =
			MappingExtract.opt(export)(f)

		protected[schema] override def req[X](f :X => T) :RequisiteColumn[X, T, O] =
			new RequisiteColumn[X, T, O](export, f)

		protected[schema] override def const[X](constant :T) :RequisiteColumn[X, T, O] =
			new ConstantColumn[T, O](export, constant)

		protected[schema] override def none[X] :ColumnExtract[X, T, O] =
			MappingExtract.none(export)
	}






	private[schema] class OptionalExtract[-S, T, O]
	                                     (val export :TypedMapping[T, O], override val optional :S => Option[T])
		extends MappingExtract[S, T, O] with OptionalExtractor[S, T]
	{
		override def get(x :S) :Option[T] = optional(x)
	}



	private[schema] class OptionalColumn[-S, T, O](override val export :ColumnMapping[T, O], optional :S => Option[T])
		extends OptionalExtract[S, T, O](export, optional) with ColumnExtract[S, T, O]



	private[schema] class RequisiteExtract[-S, T, O]
	                                      (override val export :TypedMapping[T, O], override val getter :S => T)
		extends MappingExtract[S, T, O]
		   with RequisiteExtractTemplate[S, T, O, ({ type E[-X] = MappingExtract[X, T, O] })#E,
                                                  ({ type R[-X] = RequisiteExtract[X, T, O] })#R]
	{
		override def apply(x :S) :T = getter(x)
	}



	private[schema] class RequisiteColumn[-S, T, O](override val export :ColumnMapping[T, O], pick :S => T)
		extends RequisiteExtract[S, T, O](export, pick) with ColumnExtract[S, T, O]
		   with RequisiteExtractTemplate[S, T, O, ({ type E[-X] = ColumnExtract[X, T, O] })#E,
			                                      ({ type R[-X] = RequisiteColumn[X, T, O] })#R]



	private[schema] class IdentityExtract[T, O](component :TypedMapping[T, O])
		extends RequisiteExtract[T, T, O](component, identity[T])
		   with IdentityExtractTemplate[T, O, ({ type E[-X] = MappingExtract[X, T, O] })#E,
		                                      ({ type R[-X] = RequisiteExtract[X, T, O] })#R]



	private[schema] class IdentityColumn[T, O](column :ColumnMapping[T, O])
		extends RequisiteColumn[T, T, O](column, identity[T])
		   with IdentityExtractTemplate[T, O, ({ type E[-X] = ColumnExtract[X, T, O] })#E,
			                                  ({ type R[-X] = RequisiteColumn[X, T, O] })#R]



	private[schema] class ConstantExtract[T, O](component :TypedMapping[T, O], override val constant :T)
		extends RequisiteExtract[Any, T, O](component, (_ :Any) => constant)
		   with ConstantExtractTemplate[T, O, ({ type E[-X] = MappingExtract[X, T, O] })#E,
                                              ({ type R[-X] = RequisiteExtract[X, T, O] })#R]



	private[schema] class ConstantColumn[T, O](column :ColumnMapping[T, O], override val constant :T)
		extends RequisiteColumn[Any, T, O](column, (_ :Any) => constant)
		   with ConstantExtractTemplate[T, O, ({ type E[-X] = ColumnExtract[X, T, O] })#E,
		                                      ({ type R[-X] = RequisiteColumn[X, T, O] })#R]



	private[schema] class EmptyExtract[T, O](component :TypedMapping[T, O])
		extends OptionalExtract[Any, T, O](component, Extractor.none.optional)
		   with EmptyExtractTemplate[T, O, ({ type E[-X] = MappingExtract[X, T, O] })#E]



	private[schema] class EmptyColumn[T, O](column :ColumnMapping[T, O])
		extends OptionalColumn[Any, T, O](column, Extractor.none.optional)
		   with EmptyExtractTemplate[T, O, ({ type E[-X] = ColumnExtract[X, T, O] })#E]


}

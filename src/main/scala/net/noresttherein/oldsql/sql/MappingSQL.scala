package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, ColumnMappingExtract, ColumnReadForm, Mapping, MappingExtract, Relation, SQLReadForm}
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FreeFrom, PartOf, PrefixOf, TableShift}
import net.noresttherein.oldsql.sql.MappingSQL.ColumnComponentSQL.{CaseColumnComponent, ColumnComponentMatcher}
import net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL.{CaseComponent, ComponentMatcher, ProperComponent}
import net.noresttherein.oldsql.sql.MappingSQL.FreeColumn.FreeColumnMatcher
import net.noresttherein.oldsql.sql.MappingSQL.FreeComponent.{CaseFreeComponent, FreeComponentMatcher}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.{CaseRelation, RelationMatcher}
import net.noresttherein.oldsql.sql.SelectSQL.{SelectColumnMapping, SelectMapping, SubselectAs, SubselectColumnMapping}
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, GlobalSQL, LocalScope}



//here be implicits
import slang._






/**
  * @author Marcin Mościcki
  */
trait MappingSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, M <: Mapping] extends SQLExpression[F, S, M#Subject] {

	type Subject = M#Subject

	override def readForm :SQLReadForm[Subject] = mapping.selectForm

	def mapping :M

	/** Returns `this` upcast to an `SQLExpression`. This method exists because many expressions,
	  * such as `table \ component`, producing some subtype of a `MappingSQL` in a place where
	  * its supertype `SQLExpression[F, S, Subject]` is expected, will confuse the compiler and make type inference fail.
	  * While simply splitting the above into a `val` assignment and its access would solve the issue, calling
	  * `(table \ component).upcast` is the most concise way of separating the expression creation with the type
	  * inference and the returned value.
	  */
	def upcast :SQLExpression[F, S, Subject] = this

}






object MappingSQL {

	//All these classes would benefit with being parameterized with RefinedMapping and kin instead of BaseMapping,
	//but a bug in the compiler makes subclasses of a subclass of MappingSQL not conform to MappingSQL
	/** An expression evaluating to a component mapping of an undetermined at this point relation. It needs to be
	  * resolved by the `Origin` type of the component before the expression can be used.
	  */
	class FreeComponent[F <: FromClause, M[A] <: BaseMapping[V, A], V] private[MappingSQL]
	                   (override val mapping :M[F], val shift :Int)
		extends MappingSQL[F, GlobalScope, M[F]]
	{
		type Origin = F

		override def readForm :SQLReadForm[V] = mapping.selectForm

		override def isGlobal = true
		override def asGlobal :Option[GlobalSQL[F, V]] = Some(this)

		override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :FreeComponent[E, M, V] =
			new FreeComponent[E, M, V](mapping.asInstanceOf[M[E]], shift + ext.diff)

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:FreeComponent[E, M, V] =
			new FreeComponent[E, M, V](mapping.asInstanceOf[M[E]], shift + ev.length)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, M[F]#Subject] =
			matcher.freeComponent(this)


		override def isomorphic(expression :SQLExpression.*) :Boolean = equals(expression)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true

			case free :FreeComponent.* @unchecked if free canEqual this =>
				free.mapping == mapping && free.shift == shift

			case _ => false
		}

		override def hashCode :Int = mapping.hashCode * 31 + shift.hashCode


		override def toString :String = mapping.toString + "#" + shift
	}






	object FreeComponent {

		def apply[F <: FromClause, C <: Mapping, M[A] <: BaseMapping[V, A], V]
		         (mapping :C, shift :Int)(implicit conforms :Conforms[C, M[F], BaseMapping[V, F]])
				:FreeComponent[F, M, V] =
			conforms(mapping) match {
				case column :ColumnMapping[V @unchecked, F @unchecked] =>
					new FreeColumn[F, MappingOf[V]#ColumnProjection, V](column, shift).asInstanceOf[FreeComponent[F, M, V]]
				case component =>
					new FreeComponent[F, M, V](component, shift)
			}

		def apply[F <: FromClause, C <: Mapping, M[A] <: BaseMapping[V, A], V]
		         (mapping :C)
		         (implicit conforms :Conforms[C, M[F], BaseMapping[V, F]], shift :TableShift[F, M, _ <: Numeral])
				:FreeComponent[F, M, V] =
				apply(mapping, shift.tables)


		def unapply[F <: FromClause, X](expr :SQLExpression[F, _, X])
				:Option[(BaseMapping[X, _ >: F <: FromClause], Int)] =
			expr match {
				case free: FreeComponent.Typed[F, X] @unchecked => Some(free.mapping -> free.shift)
				case _ => None
			}

		def unapply[F <: FromClause, M[A] <: BaseMapping[X, A], X](expr :FreeComponent[F, M, X]) :Option[(M[F], Int)] =
			Some(expr.mapping -> expr.shift)



		type * = FreeComponent[F, M, V] forSome {
			type F <: FromClause; type M[A] <: BaseMapping[V, A]; type V
		}

		private type AnyIn[-F <: FromClause] = FreeComponent[O, M, V] forSome {
			type O >: F <: FromClause; type M[A] <: BaseMapping[V, A]; type V
		}

		private type Typed[-F <: FromClause, V] = FreeComponent[O, M, V] forSome {
			type O >: F <: FromClause; type M[A] <: BaseMapping[V, A]
		}



		trait FreeComponentMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends FreeColumnMatcher[F, Y] {
			def freeComponent[O >: F <: FromClause, M[A] <: BaseMapping[X, A], X]
			                 (e :FreeComponent[O, M, X]) :Y[GlobalScope, X]
		}

		type MatchFreeComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = FreeComponentMatcher[F, Y]

		trait CaseFreeComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends FreeComponentMatcher[F, Y] {
			override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
			                          (e :FreeColumn[O, M, V]) :Y[GlobalScope, V] =
				freeComponent(e :FreeComponent[O, M, V])
		}

	}






	class FreeColumn[F <: FromClause, M[A] <: ColumnMapping[V, A], V] private[MappingSQL]
	                (column :M[F], shift :Int)
		extends FreeComponent[F, M, V](column, shift) with ColumnSQL[F, GlobalScope, V]
	{
		//this should really use mapping.selectForm, bot it is not a ColumnForm due to possible buffs
		//doesn't really matter though, as this class is a placeholder and the form will never get used.
		override def readForm :ColumnReadForm[V] = mapping.form

		override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)

		override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :FreeColumn[E, M, V] =
			new FreeColumn[E, M, V](column.asInstanceOf[M[E]], shift + ext.diff)

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :FreeColumn[E, M, V] =
			new FreeColumn[E, M, V](column.asInstanceOf[M[E]], shift + ev.length)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, V] =
			matcher.freeComponent[F, M, V](this)

	}






	object FreeColumn {

		def apply[F <: FromClause, C <: Mapping, M[A] <: ColumnMapping[V, A], V]
		         (column :C, shift :Int)(implicit conforms :Conforms[C, M[F], ColumnMapping[V, F]])
				:FreeColumn[F, M, V] =
			new FreeColumn[F, M, V](column, shift)


		def apply[F <: FromClause, C <: ColumnMapping[_, _], M[A] <: ColumnMapping[V, A], V]
		         (column :C)
		         (implicit conforms :Conforms[C, M[F], ColumnMapping[V, F]], shift :TableShift[F, M, _ <: Numeral])
				:FreeColumn[F, M, V] =
			apply(column, shift.tables)


		def unapply[F <: FromClause, X](expr :SQLExpression[F, _, X])
				:Option[(ColumnMapping[X, _ >: F <: FromClause], Int)] =
			expr match {
				case free: FreeColumn.Typed[F, X] @unchecked => Some(free.mapping -> free.shift)
				case _ => None
			}

		def unapply[F <: FromClause, M[A] <: BaseMapping[X, A], X](expr :FreeComponent[F, M, X]) :Option[(M[F], Int)] =
			(expr :FreeComponent.*) match {
				case _ :FreeColumn.* @unchecked => Some(expr.mapping -> expr.shift)
				case _ => None
			}



		type * = FreeColumn[_ <: FromClause, M, V] forSome { type M[A] <: ColumnMapping[V, A]; type V }

		type AnyIn[-F <: FromClause] = FreeColumn[O, M, V]
				forSome { type O >: F <: FromClause; type M[A] <: ColumnMapping[V, A]; type V }

		type Typed[-F <: FromClause, V] = FreeColumn[O, M, V]
				forSome { type O >: F <: FromClause; type M[A] <: ColumnMapping[V, A] }



		trait FreeColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
			                 (e :FreeColumn[O, M, V]) :Y[GlobalScope, V]
		}

		type MatchFreeColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = FreeColumnMatcher[F, Y]

		type CaseFreeColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = FreeColumnMatcher[F, Y]

	}






	trait BaseComponentSQL[-F <: FromClause, M[A] <: MappingAt[A], O >: F <: FromClause]
		extends MappingSQL[F, GlobalScope, M[O]]
	{
		type Origin = O

		/** The mapping type of the `SQLRelation` to which this component belongs. */
		type Entity[A] <: MappingAt[A]

		override def isGlobal = true
		override def asGlobal :Option[GlobalSQL[F, Subject]] = Some(this)

		def entity :Entity[O] = origin.mapping
		def relation :Relation[Entity] = origin.relation
		def origin :JoinedRelation[O, Entity]

		def extract :MappingExtract[Entity[O]#Subject, M[O]#Subject, O]

		override def readForm :SQLReadForm[Subject] = extract.export.selectForm

		def \[K <: Mapping, C[A] <: BaseMapping[X, A], X]
		     (component :K)(implicit inferer :Conforms[K, C[O], BaseMapping[X, O]]) :BaseComponentSQL[F, C, O]

		def \[K <: Mapping, C[A] <: BaseMapping[X, A], X]
		     (component :M[O] => K)(implicit inferer :Conforms[K, C[O], BaseMapping[X, O]])
				:BaseComponentSQL[F, C, O]

		def \[K <: ColumnMapping[_, O], C[A] <: ColumnMapping[X, A], X]
		     (column :K)(implicit conforms :Conforms[K, C[O], ColumnMapping[X, O]])
				:BaseColumnComponentSQL[F, C, X, O]



		override def isomorphic(expression :SQLExpression.*) :Boolean = this == expression

		private[oldsql] override def equivalent(expression :SQLExpression.*) :Boolean = expression match {
			case self :AnyRef if self eq this => true
			case component :BaseComponentSQL[_, _, _] if component canEqual this =>
				relation == component.relation && mapping == component.mapping
			case _ => false
		}


		override def toString :String = origin.toString + "." + mapping
	}



	trait ComponentSQL[-F <: FromClause, T[A] <: BaseMapping[R, A], R,
	                   M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		extends BaseComponentSQL[F, M, O]
	{
		override type Entity[A] = T[A]

		override def origin :RelationSQL[F, T, R, O]

		def extract :MappingExtract[R, V, O]


		override def \[K <: Mapping, C[A] <: BaseMapping[X, A], X]
		              (component :K)(implicit inferer :Conforms[K, C[O], BaseMapping[X, O]])
				:ComponentSQL[F, T, R, C, X, O] =
			if (component == entity)
				origin.asInstanceOf[ComponentSQL[F, T, R, C, X, O]]
			else
				ComponentSQL(origin, component)

		override def \[K <: Mapping, C[A] <: BaseMapping[X, A], X]
		              (component :M[O] => K)(implicit inferer :Conforms[K, C[O], BaseMapping[X, O]])
				:ComponentSQL[F, T, R, C, X, O] =
			this \ component(mapping)

		override def \[K <: ColumnMapping[_, O], C[A] <: ColumnMapping[X, A], X]
		              (column :K)(implicit conforms :Conforms[K, C[O], ColumnMapping[X, O]])
				:ColumnComponentSQL[F, T, R, C, X, O] =
			ColumnComponentSQL(origin, column)


		override def basedOn[U <: F, E <: FromClause](base :E)(implicit extension :U PartOf E)
				:ComponentSQL[E, T, R, M, V, _ >: E <: FromClause] =
			extend(base)(extension.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:ComponentSQL[E, T, R, M, V, _ >: E <: FromClause]



		override def selectFrom[E <: F with FreeFrom, A](from :E) :SelectMapping[E, M, V, A] =
			SelectSQL(from, this)

		override def subselectFrom[E <: F, A](from :E) :SubselectAs[from.Base, M[A]] =
			SelectSQL.subselect[from.Base, from.type, T, R, M, V, O, A](from, this)


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentSQL.* @unchecked]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case component :ComponentSQL.* @unchecked if canEqual(component) && component.canEqual(this) =>
				origin == component.origin && mapping == component.mapping //consider: should it compare extractor.export?
			case _ => false
		}

		override def hashCode :Int = origin.hashCode * 31 + mapping.hashCode
	}






	object ComponentSQL {

		def apply[F <: FromClause, T[A] <: BaseMapping[R, A], R, K <: Mapping,
		          M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		         (from :RelationSQL[F, T, R, O], component :K)(implicit inferer :Conforms[K, M[O], BaseMapping[V, O]])
				:ComponentSQL[F, T, R, M, V, O] =
			component match {
				case column :ColumnMapping[Any @unchecked, O @unchecked] =>
					ColumnComponentSQL(from, column).asInstanceOf[ComponentSQL[F, T, R, M, V, O]]
				case _ =>
					new ProperComponent[F, T, R, M, V, O](from, inferer(component))
			}



		def unapply[F <: FromClause, X](e :SQLExpression[F, _, X])
				:Option[(RelationSQL[F, T, R, O], MappingExtract[R, X, O])
						forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: FromClause }] =
			e match {
				case component :ComponentSQL.* @unchecked =>
					Some((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], MappingExtract[Any, X, F])
					])
				case _ => None
			}



		type * = ComponentSQL[F, T, R, M, V, O] forSome {
			type F <: FromClause; type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type AnyIn[-F <: FromClause] = ComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type Typed[-F <: FromClause, V] = ComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]
		}



		private[MappingSQL] class ProperComponent[-F <: FromClause, T[A] <: BaseMapping[R, A], R,
		                                          M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                                         (override val origin :RelationSQL[F, T, R, O],
		                                          override val mapping :M[O])
			extends ComponentSQL[F, T, R, M, V, O]
		{
			override val extract = entity(mapping)

			override val readForm :SQLReadForm[V] = extract.export.selectForm

			override def extend[U <: F, G <: FromClause]
			                   (base :G)(implicit ev :U ExtendedBy G, global :GlobalScope <:< GlobalScope)
					:ProperComponent[G, T, R, M, V, _ >: G <: FromClause] =
				new ProperComponent[G, T, R, M, V, G](  //type G as O is incorrect, but a correct O exists and we upcast anyway
                    origin.extend(base).asInstanceOf[RelationSQL[G, T, R, G]], mapping.asInstanceOf[M[G]]
                )

			override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
			                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, V] =
				matcher.component(this)
		}



		trait ComponentMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends RelationMatcher[F, Y] with ColumnComponentMatcher[F, Y]
		{
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
			             (e :ComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		trait MatchComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ComponentMatcher[F, Y] with CaseRelation[F, Y] with CaseColumnComponent[F, Y]
		{
			override def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
			                      (e :ColumnComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V] =
				component[T, R, M, V, O](e :ComponentSQL[F, T, R, M, V, O])
		}

		trait CaseComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchComponent[F, Y] {
			override def relation[T[A] <: BaseMapping[R, A], R, O >: F <: FromClause]
			                     (e :RelationSQL[F, T, R, O]) :Y[GlobalScope, R] =
				component(e)
		}
	}






	trait BaseColumnComponentSQL[-F <: FromClause, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		extends BaseComponentSQL[F, M, O] with ColumnSQL[F, GlobalScope, V]
	{
		override def extract :ColumnMappingExtract[Entity[O]#Subject, V, O]

		//fixme: this should really use the selectForm, but it is not a ColumnForm due to buff excludes/const etc
		override def readForm :ColumnReadForm[V] = extract.export.form

		override def upcast :ColumnSQL[F, GlobalScope, Subject] = this

		override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)

		override def basedOn[U <: F, E <: FromClause]
		                    (base :E)(implicit extension :U PartOf E)
				:BaseColumnComponentSQL[E, M, V, _ >: E <: FromClause] =
			extend(base)(extension.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit extension :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:BaseColumnComponentSQL[E, M, V, _ >: E <: FromClause]
	}



	trait ColumnComponentSQL[-F <: FromClause, T[A] <: BaseMapping[R, A], R,
	                         M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		extends ComponentSQL[F, T, R, M, V, O] with BaseColumnComponentSQL[F, M, V, O]
	{
		override def basedOn[U <: F, E <: FromClause]
		                    (base :E)(implicit extension :U PartOf E)
				:ColumnComponentSQL[E, T, R, M, V, _ >: E <: FromClause] =
			extend(base)(extension.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:ColumnComponentSQL[E, T, R, M, V, _ >: E <: FromClause]


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, V] =
			matcher.component(this)


		override def selectFrom[E <: F with FreeFrom, A](from :E) :SelectColumnMapping[E, M, V, A] =
			SelectSQL(from, this)

		override def subselectFrom[E <: F, A](from :E) :SubselectColumnMapping[from.Base, from.type, M, V, A] =
			SelectSQL.subselect[from.Base, from.type, T, R, M, V, O, A](from, this) //todo: source.type is ugly!


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnComponentSQL.* @unchecked]
	}






	object ColumnComponentSQL {

		def apply[F <: FromClause, T[A] <: BaseMapping[R, A], R,
		          C <: ColumnMapping[_, _], M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		         (from :RelationSQL[F, T, R, O], column :C)(implicit conforms :Conforms[C, M[O], ColumnMapping[V, O]])
				:ColumnComponentSQL[F, T, R, M, V, O] =
			new ProperColumn(from, column)



		def unapply[F <: FromClause, X](e :SQLExpression[F, _, X])
				:Option[(RelationSQL[F, T, R, O], ColumnMappingExtract[R, X, O])
						forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: FromClause }] =
			e match {
				case component :ColumnComponentSQL.* @unchecked =>
					Some((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], ColumnMappingExtract[Any, X, F])
					])
				case _ => None
			}



		type * = ColumnComponentSQL[F, T, R, M, V, O] forSome {
			type F <: FromClause; type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type AnyIn[-F <: FromClause] = ColumnComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type Typed[-F <: FromClause, V] = ColumnComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]
		}



		private class ProperColumn[-F <: FromClause, T[A] <: BaseMapping[R, A], R,
		                           M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                          (relation :RelationSQL[F, T, R, O], column :M[O])
			extends ProperComponent[F, T, R, M, V, O](relation, column)
			   with ColumnComponentSQL[F, T, R, M, V, O]
		{
			override val extract = entity(mapping)
			//fixme: sort out where the buff-related modifications take place to have consistent assembly semantics
			override val readForm = super.readForm

			override def extend[U <: F, E <: FromClause]
			                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
					:ProperColumn[E, T, R, M, V, _ >: E <: FromClause] =
				new ProperColumn[E, T, R, M, V, E](
					origin.extend(base).asInstanceOf[RelationSQL[E, T, R, E]], column.asInstanceOf[M[E]]
				)
		}



		trait ColumnComponentMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
			             (e :ColumnComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		type MatchColumnComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentMatcher[F, Y]

		type CaseColumnComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentMatcher[F, Y]
	}






	sealed trait JoinedRelation[F <: FromClause, T[A] <: MappingAt[A]] extends BaseComponentSQL[F, T, F] {
		override type Entity[A] = T[A]

		type Self = RelationSQL[F, M, T[F]#Subject, F] forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

		/** Offset of this relation in the clause `F`, counting ''from right to left'' and starting with 0. */
		def shift :Int

		override def origin :JoinedRelation[F, T] = this

		def asRelationSQL :RelationSQL[F, M, T[F]#Subject, F]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

		/** Casts down this instance to the more strongly typed `RelationSQL`. The result type is an `Option`
		  * not because the cast may fail, but because the correct existential subtype cannot exist as a value
		  * (returning simply the existentially refined `RelationSQL` produces a compiler error about being
		  * unable to abstract over existential higher type `T`).
		  */
		def toRelationSQL :Option[RelationSQL[F, M, T[F]#Subject, F]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }]


		/** Converts this relation to an expression based on the clause `F[F]`, which extends `F` by a single relation. */
		def asIn[E[+L <: F] <: L Extended T forSome { type T[O] <: MappingAt[O] }] :JoinedRelation[E[F], T]

		/** This method is equivalent to `this.extend()`, but doesn't require the `G` clause as the parameter
		  * and returns a `JoinedRelation`. The `extend` method cannot be overriden here to return a `JoinedRelation`
		  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
		  */
		def asIn[G <: FromClause](implicit extension :F PrefixOf G) :JoinedRelation[G, T]


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedRelation.*]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case relation :JoinedRelation.* if (this canEqual relation) && (relation canEqual this) =>
				relation.shift == shift && relation.mapping == mapping
			case _ => false
		}

		override def hashCode :Int = shift * 31 + mapping.hashCode

		override def toString :String = mapping.toString + "#" + shift
	}






	object JoinedRelation {

		def unapply[F <: FromClause, X](e :SQLExpression[F, _, X])
				:Option[(RefinedMapping[X, _ >: F <: FromClause], Int)] =
			e match {
				case from :JoinedRelation.Typed[F, X] @unchecked =>
					Some(from.mapping -> from.shift)
				case _ => None
			}



		type * = JoinedRelation[_ <: FromClause, T] forSome { type T[O] <: MappingAt[O] }

		type AnyIn[F <: FromClause] = JoinedRelation[F, T] forSome { type T[O] <: MappingAt[O] }

		type Typed[F <: FromClause, V] = JoinedRelation[F, T] forSome { type T[O] <: RefinedMapping[V, O] }

	}






	class RelationSQL[-F <: FromClause, T[A] <: BaseMapping[R, A], R, O >: F <: FromClause] private[sql]
	                 (override val relation :Relation[T], override val mapping :T[O], override val shift :Int)
		extends JoinedRelation[O, T] with ComponentSQL[F, T, R, T, R, O]
	{
		private[sql] def this(relation :Relation[T], shift :Int) = this(relation, relation[O], shift)


		override def origin :RelationSQL[O, T, R, O] = asRelationSQL

		override val extract = MappingExtract.ident(mapping)

		@inline final override def asRelationSQL :RelationSQL[O, T, R, O] = this.asInstanceOf[RelationSQL[O, T, R, O]]

		override def toRelationSQL :Some[RelationSQL[O, T, R, O]] = Some(asRelationSQL)

		override def upcast :BaseComponentSQL[O, T, O] = this


		//these need to be overriden due to JoinedRelation's having wider bounds than the inherited implementation
		override def \[K <: Mapping, C[A] <: BaseMapping[X, A], X]
		              (component :K)(implicit inferer :Conforms[K, C[O], BaseMapping[X, O]])
				:ComponentSQL[O, T, R, C, X, O] =
			if (component == entity)
				origin.asInstanceOf[ComponentSQL[O, T, R, C, X, O]]
			else
				ComponentSQL(origin, component)

		override def \[K <: Mapping, C[A] <: BaseMapping[X, A], X] //todo: narrow down type to ColumnSQL if C is a ColumnMapping
		              (component :T[O] => K)(implicit inferer :Conforms[K, C[O], BaseMapping[X, O]])
				:ComponentSQL[O, T, R, C, X, O] =
			ComponentSQL(asRelationSQL, component(mapping))

		override def \[K <: ColumnMapping[_, O], C[A] <: ColumnMapping[X, A], X]
		              (column :K)(implicit conforms :Conforms[K, C[O], ColumnMapping[X, O]])
				:ColumnComponentSQL[O, T, R, C, X, O] =
			ColumnComponentSQL(asRelationSQL, column)


		override def basedOn[U <: O, E <: FromClause]
		                    (base :E)(implicit extension :U PartOf E) :RelationSQL[E, T, R, _ >: E <: FromClause] =
			new RelationSQL[E, T, R, E](relation, shift + extension.diff) //E is incorrect, but we lose this information anyway

		override def extend[U <: O, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:RelationSQL[E, T, R, _ >: E <: FromClause] =
			new RelationSQL[E, T, R, E](relation, shift + ev.length) //E is incorrect, but we lose this information anyway

		override def asIn[J[+L <: O] <: L Extended T forSome { type T[A] <: MappingAt[A] }] :RelationSQL[J[F], T, R, J[O]] =
			new RelationSQL[J[F], T, R, J[O]](relation, shift + 1)

		override def asIn[E <: FromClause](implicit extension :O PrefixOf E) :RelationSQL[E, T, R, E] =
			new RelationSQL(relation, shift + extension.diff)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[O, Y]) :Y[GlobalScope, R] =
			matcher.relation(asRelationSQL)



		override def selectFrom[E <: O with FreeFrom, A](from :E) :SelectMapping[E, T, R, A] =
			SelectSQL(from, asRelationSQL)

		override def subselectFrom[E <: O, A](from :E) :SubselectAs[from.Base, T[A]] =
			SelectSQL.subselect[from.Base, from.type, T, R, T, R, O, A](from, asRelationSQL)


	}






	object RelationSQL {

//		def apply[F <: FromClause, T[A] <: BaseMapping[S, A], S]
//		         (table :Relation[T])(implicit shift :TableShift[F, T, _ <: Numeral]) :RelationSQL[F, T, S, F] =
//			new RelationSQL[F, T, S, F](table, table[F], shift.tables)

		def apply[F <: FromClause] :RelationSQLFactory[F] = new RelationSQLFactory[F] {}

		trait RelationSQLFactory[F <: FromClause] extends Any {
			def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			         (relation :Relation[M])
			         (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]],
			                   shift :TableShift[F, T, _ <: Numeral]) :RelationSQL[F, T, S, F] =
			{
				val table = cast(relation)
				new RelationSQL[F, T, S, F](table, table[F], shift.tables)
			}
		}

		private[sql] def apply[F <: FromClause, T[A] <: BaseMapping[S, A], S, O >: F <: FromClause]
		                      (table :Relation[T], index :Int) :RelationSQL[F, T, S, O] =
			new RelationSQL[F, T, S, O](table, index)


		def last[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		        (table :Relation[M])
		        (implicit inference :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
				:LastRelation[T, S] =
			new RelationSQL[FromClause AndFrom T, T, S, FromClause AndFrom T](table, 0)



		def unapply[F <: FromClause, X](e :SQLExpression[F, _, X])
				:Option[(BaseMapping[X, _ >: F <: FromClause], Int)] =
			e match {
				case from :RelationSQL.Typed[F, X] @unchecked =>
					Some(from.mapping -> from.shift)
				case _ => None
			}



		type * = RelationSQL[F, T, X, O] forSome {
			type F <: FromClause; type O >: F <: FromClause
			type T[A] <: BaseMapping[X, A]; type X
		}

		type AnyIn[-F <: FromClause] = RelationSQL[F, T, R, O] forSome {
			type T[A] <: BaseMapping[R, A]; type R; type O >: F <: FromClause
		}

		type Typed[-F <: FromClause, R] = RelationSQL[F, T, R, O] forSome {
			type T[A] <: BaseMapping[R, O]; type O >: F <: FromClause
		}

		type LastRelation[T[A] <: BaseMapping[S, A], S] = RelationSQL[FromClause AndFrom T, T, S, FromClause AndFrom T]

		def LastRelation[T[A] <: BaseMapping[S, A], S](from :Relation[T]) :LastRelation[T, S] =
			new RelationSQL[FromClause AndFrom T, T, S, FromClause AndFrom T](from, 0)



		trait RelationMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def relation[T[A] <: BaseMapping[R, A], R, O >: F <: FromClause]
			            (e :RelationSQL[F, T, R, O]) :Y[GlobalScope, R]
		}

		type MatchRelation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = RelationMatcher[F, Y]

		type CaseRelation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = RelationMatcher[F, Y]

	}






	trait MappingColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends FreeColumnMatcher[F, Y] with ColumnComponentMatcher[F, Y]

	trait MappingMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ComponentMatcher[F, Y] with FreeComponentMatcher[F, Y] with MappingColumnMatcher[F, Y]

	trait MatchMapping[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MappingMatcher[F, Y] with CaseComponent[F, Y] with CaseFreeComponent[F, Y]

	trait CaseMapping[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchMapping[F, Y] {
		def mapping[S >: LocalScope <: GlobalScope, M <: Mapping](e :MappingSQL[F, S, M]) :Y[S, M#Subject]

		override def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V] =
			mapping(e)

		override def freeComponent[J >: F <: FromClause, M[A] <: BaseMapping[X, A], X]
		                          (e :FreeComponent[J, M, X]) :Y[GlobalScope, X] =
			mapping(e)
	}

}



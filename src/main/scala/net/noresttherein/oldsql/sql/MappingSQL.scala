package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, ColumnMappingExtract, ColumnReadForm, Mapping, MappingExtract, Relation, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.SchemaMapping.|-|
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.FromClause.{ExactSubselectOf, ExtendedBy, FreeFrom, NonEmptyFrom, PartOf, PrefixOf, SubselectFrom, TableCount, TableShift}
import net.noresttherein.oldsql.sql.MappingSQL.TypedColumnComponentSQL.{CaseColumnComponent, ColumnComponentMatcher}
import net.noresttherein.oldsql.sql.MappingSQL.TypedComponentSQL.{CaseComponent, ComponentMatcher, ProperComponent}
import net.noresttherein.oldsql.sql.MappingSQL.LooseColumnComponent.LooseColumnMatcher
import net.noresttherein.oldsql.sql.MappingSQL.LooseComponent.{CaseLooseComponent, LooseComponentMatcher}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.{CaseRelation, RelationMatcher}
import net.noresttherein.oldsql.sql.SelectSQL.{FreeSelectAs, SelectAs, SelectColumnAs, SelectColumnMapping, SelectMapping, SubselectAs, SubselectColumnMapping, SubselectMapping}
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.TupleSQL.IndexedChainTuple.IndexedSQLExpression



//here be implicits
import slang._






/** An SQL expression AST node represented by a mapping `M`. While `M` might be a subtype of
  * [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]], typically it is a component of some relation
  * from the ''from'' clause of an SQL select containing this expression. The value type of this expression is defined
  * as the mapped [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type, as seen by the application,
  * but will be represented in actual generated SQL as a tuple containing all columns of the mapping `M`.
  * If the expression is used literally as part of a ''select'' clause (either directly, or inside
  * a [[net.noresttherein.oldsql.sql.TupleSQL tuple]]), default
  * [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns (those without a
  * [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]] buff) will be inlined in its place.
  * If used as part of a comparison in a ''where'' or ''having'' clause, the columns of the two expressions will
  * be compared ordinally.
  * @author Marcin Mościcki
  */
trait MappingSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]]
	extends SQLExpression[F, S, M[Any]#Subject]
{
	type Origin <: FromClause
	type Subject = M[Any]#Subject

//	override def readForm :SQLReadForm[Subject] //= mapping.withOrigin[Any].selectForm

	def mapping :M[Origin]

	/** Returns `this` upcast to an `SQLExpression`. This method exists because many expressions,
	  * such as `table \ component`, producing some subtype of a `MappingSQL` in a place where
	  * its supertype `SQLExpression[F, S, Subject]` is expected, will confuse the compiler and make type inference fail.
	  * While simply splitting the above into a `val` assignment and its access would solve the issue, calling
	  * `(table \ component).upcast` is the most concise way of separating the expression creation with the type
	  * inference and the returned value.
	  */
	def upcast :SQLExpression[F, S, Subject] = this


	override def selectFrom(from :F) :SelectAs[from.Base, M] =
		if (from.isParameterized)
			throw new IllegalArgumentException(
				s"Cannot use a parameterized clause as a basis for select $this: $from."
			)
		else if (from.isSubselect) {
			subselectFrom(from.asInstanceOf[ExactSubselectOf[from.type, NonEmptyFrom]])
				.asInstanceOf[SelectAs[from.Base, M]]
		} else
			freeSelectFrom(from.asInstanceOf[F with FreeFrom])

	override def freeSelectFrom[E <: F with FreeFrom](from :E) :FreeSelectAs[M] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.unqualifiedClassName} can't be used as a select clause."
		)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectAs[B, M] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.unqualifiedClassName} can't be used as a select clause."
		)

}






object MappingSQL {

	//All these classes would benefit with being parameterized with RefinedMapping and kin instead of BaseMapping,
	//but a bug in the compiler makes subclasses of a subclass of MappingSQL not conform to MappingSQL
	/** An expression evaluating to a component mapping of an undetermined at this point relation. It needs to be
	  * resolved by the `Origin` type of the component before the expression can be used.
	  */
	class LooseComponent[F <: FromClause, M[A] <: BaseMapping[V, A], V] private[MappingSQL]
	                    (override val mapping :M[F], val shift :Int)(implicit val projection :IsomorphicProjection[M, V, F])
		extends MappingSQL[F, GlobalScope, M]
	{
		type Origin = F

		override def readForm :SQLReadForm[V] = mapping.selectForm

		override def isGlobal = true
		override def asGlobal :Option[GlobalSQL[F, V]] = Some(this)

		override def isAnchored = false

		override def anchor(from :F) :ComponentSQL[F, M] = {
			val relation = from.fullTableStack(shift).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
			relation \ mapping
		}


		override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :LooseComponent[E, M, V] =
			new LooseComponent[E, M, V](mapping.asInstanceOf[M[E]], shift + ext.diff)(projection.isomorphism[E])

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:LooseComponent[E, M, V] =
			new LooseComponent[E, M, V](mapping.asInstanceOf[M[E]], shift + ev.length)(projection.isomorphism[E])


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, M[F]#Subject] =
			matcher.looseComponent(this)


		override def freeSelectFrom[E <: F with FreeFrom](from :E) :SelectMapping[E, M, V] =
			SelectSQL(from, anchor(from))

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, anchor(from))


		override def isomorphic(expression :SQLExpression.*) :Boolean = equals(expression)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true

			case free :LooseComponent.* @unchecked if free canEqual this =>
				free.mapping == mapping && free.shift == shift

			case _ => false
		}

		override def hashCode :Int = mapping.hashCode * 31 + shift.hashCode


		override def toString :String = mapping.toString + "#" + shift
	}






	object LooseComponent {

		private[oldsql] def apply[F <: FromClause, C <: Mapping, V]
		                         (mapping :C, shift :Int)
		                         (implicit cast :C <:< MappingAt[F], project :OriginProjection[C, V])
				:LooseComponent[F, project.WithOrigin, V] =
			project[F](mapping) match {
				case column :ColumnMapping[V @unchecked, F @unchecked] =>
					implicit val projection = OriginProjection.isomorphism[MappingOf[V]#ColumnProjection, V, F]
					new LooseColumnComponent[F, MappingOf[V]#ColumnProjection, V](column, shift)
						.asInstanceOf[LooseComponent[F, project.WithOrigin, V]]
				case component =>
					new LooseComponent[F, project.WithOrigin, V](component, shift)(project.isomorphism)
			}

		def apply[F <: FromClause, C <: Mapping, V]
		         (mapping :C) //TableCount, not TableShift, because M is likely a component, not a table
		         (implicit cast :C <:< MappingAt[F], shift :TableCount[F, _ <: Numeral],
		                   project :OriginProjection[C, V])
				:LooseComponent[F, project.WithOrigin, V] =
				apply(mapping, shift.offset)


		def unapply[F <: FromClause, X](expr :SQLExpression[F, _, X])
				:Option[(BaseMapping[X, _ >: F <: FromClause], Int)] =
			expr match {
				case free: LooseComponent.Typed[F, X] @unchecked => Some(free.mapping -> free.shift)
				case _ => None
			}

		def unapply[F <: FromClause, M[A] <: BaseMapping[X, A], X](expr :LooseComponent[F, M, X]) :Option[(M[F], Int)] =
			Some(expr.mapping -> expr.shift)



		type * = LooseComponent[F, M, V] forSome {
			type F <: FromClause; type M[A] <: BaseMapping[V, A]; type V
		}

		private type AnyIn[-F <: FromClause] = LooseComponent[O, M, V] forSome {
			type O >: F <: FromClause; type M[A] <: BaseMapping[V, A]; type V
		}

		private type Typed[-F <: FromClause, V] = LooseComponent[O, M, V] forSome {
			type O >: F <: FromClause; type M[A] <: BaseMapping[V, A]
		}



		trait LooseComponentMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends LooseColumnMatcher[F, Y] {
			def looseComponent[O >: F <: FromClause, M[A] <: BaseMapping[X, A], X]
			                  (e :LooseComponent[O, M, X]) :Y[GlobalScope, X]
		}

		trait MatchLooseComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends LooseComponentMatcher[F, Y] {
			override def looseComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
			                          (e :LooseColumnComponent[O, M, V]) :Y[GlobalScope, V] =
				looseComponent(e :LooseComponent[O, M, V])
		}

		type CaseLooseComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = MatchLooseComponent[F, Y]

	}






	class LooseColumnComponent[F <: FromClause, M[A] <: ColumnMapping[V, A], V] private[MappingSQL]
	                          (column :M[F], shift :Int)(implicit project :IsomorphicProjection[M, V, F])
		extends LooseComponent[F, M, V](column, shift) with ColumnSQL[F, GlobalScope, V]
	{
		//this should really use mapping.selectForm, bot it is not a ColumnForm due to possible buffs
		//doesn't really matter though, as this class is a placeholder and the form will never get used.
		override def readForm :ColumnReadForm[V] = mapping.form

		override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)

		override def anchor(from :F) :ColumnComponentSQL[F, M, V] = {
			val relation = from.fullTableStack(shift).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
			relation \ mapping
		}

		override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :LooseColumnComponent[E, M, V] =
			new LooseColumnComponent[E, M, V](column.asInstanceOf[M[E]], shift + ext.diff)(projection.isomorphism)

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:LooseColumnComponent[E, M, V] =
			new LooseColumnComponent[E, M, V](column.asInstanceOf[M[E]], shift + ev.length)(projection.isomorphism)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, V] =
			matcher.looseComponent[F, M, V](this)

		override def selectFrom(from :F) :SelectColumnAs[from.Base, M, V] =
			if (from.isParameterized)
				throw new IllegalArgumentException(
					s"Cannot use a parameterized clause as a basis for select $this: $from."
				)
			else if (from.isSubselect) {
				subselectFrom(from.asInstanceOf[ExactSubselectOf[from.type, NonEmptyFrom]])
					.asInstanceOf[SelectColumnAs[from.Base, M, V]]
			} else
				freeSelectFrom(from.asInstanceOf[F with FreeFrom])

		override def freeSelectFrom[E <: F with FreeFrom](from :E) :SelectColumnMapping[E, M, V] =
			SelectSQL(from, anchor(from))

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectColumnMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, anchor(from))

	}






	object LooseColumnComponent {

		private[oldsql] def apply[F <: FromClause, C <: ColumnMapping[_, _], V]
		                         (column :C, shift :Int)
		                         (implicit cast :C <:< ColumnMapping[V, F],
		                                   project :OriginProjection[C, V] { type WithOrigin[O] <: ColumnMapping[V, O] })
				:LooseColumnComponent[F, project.WithOrigin, V] =
			new LooseColumnComponent[F, project.WithOrigin, V](project[F](column), shift)(project.isomorphism)


		def apply[F <: FromClause, C <: ColumnMapping[_, _], V]
		         (column :C) //TableCount, not TableShift, because M is likely a component, not a table
		         (implicit cast :C <:< ColumnMapping[V, F], shift :TableCount[F, _ <: Numeral],
		                   project :OriginProjection[C, V] { type WithOrigin[O] <: ColumnMapping[V, O] })
				:LooseColumnComponent[F, project.WithOrigin, V] =
			apply(column, shift.offset)


		def unapply[F <: FromClause, X](expr :SQLExpression[F, _, X])
				:Option[(ColumnMapping[X, _ >: F <: FromClause], Int)] =
			expr match {
				case free: LooseColumnComponent.Typed[F, X] @unchecked => Some(free.mapping -> free.shift)
				case _ => None
			}

		def unapply[F <: FromClause, M[A] <: BaseMapping[X, A], X](expr :LooseComponent[F, M, X]) :Option[(M[F], Int)] =
			(expr :LooseComponent.*) match {
				case _ :LooseColumnComponent.* @unchecked => Some(expr.mapping -> expr.shift)
				case _ => None
			}



		type * = LooseColumnComponent[_ <: FromClause, M, V] forSome { type M[A] <: ColumnMapping[V, A]; type V }

		type AnyIn[-F <: FromClause] = LooseColumnComponent[O, M, V]
				forSome { type O >: F <: FromClause; type M[A] <: ColumnMapping[V, A]; type V }

		type Typed[-F <: FromClause, V] = LooseColumnComponent[O, M, V]
				forSome { type O >: F <: FromClause; type M[A] <: ColumnMapping[V, A] }



		trait LooseColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def looseComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
			                  (e :LooseColumnComponent[O, M, V]) :Y[GlobalScope, V]
		}

		type MatchLooseColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = LooseColumnMatcher[F, Y]

		type CaseLooseColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = LooseColumnMatcher[F, Y]

	}






	trait ComponentSQL[-F <: FromClause, M[A] <: MappingAt[A]]
		extends MappingSQL[F, GlobalScope, M]
	{
		/** The mapping type of the `SQLRelation` to which this component belongs. */
		type Entity[A] <: MappingAt[A]

		/** A pseudo relation adapting this expression for use in a ''group by'' clauses
		  * [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] and [[net.noresttherein.oldsql.sql.ByAll ByAll]].
		  */
		def groupingRelation :Relation[M]

		override def isGlobal = true
		override def asGlobal :Option[GlobalSQL[F, Subject]] = Some(this)

		override def isAnchored = true
		override def anchor(from :F) :ComponentSQL[F, M] = this

		def entity :Entity[Origin] = origin.mapping
		def relation :Relation[Entity] = origin.relation
		def origin :JoinedRelation[Origin, Entity]

		def extract :MappingExtract[Entity[Origin]#Subject, M[Origin]#Subject, Origin]


		def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
				:ComponentSQL[F, project.WithOrigin]

		def \[K <: MappingAt[Origin]]
		     (component :M[Origin] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.Result[F]

		def \[K <: ColumnMapping[_, Origin], X]
		     (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
				:ColumnComponentSQL[F, project.WithOrigin, X]


		override def isomorphic(expression :SQLExpression.*) :Boolean = this == expression

		private[oldsql] override def equivalent(expression :SQLExpression.*) :Boolean = expression match {
			case self :AnyRef if self eq this => true
			case component :ComponentSQL[_, _] if component canEqual this =>
				relation == component.relation && mapping == component.mapping
			case _ => false
		}


		override def toString :String = origin.toString + "." + mapping
	}



	object ComponentSQL {

		def apply[F <: FromClause, M <: BaseMapping[S, O], S, O >: F <: FromClause]
		         (from :F, component :M)
		         (implicit offset :TableCount[O, _ <: Numeral], project :OriginProjection[M, S])
				:ComponentSQL[F, project.WithOrigin] =
		{
			val relation = from.fullTableStack(offset.offset).toRelationSQL
			                   .asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, O]]
			TypedComponentSQL(relation, component)
		}

		def apply[F <: FromClause, T[A] <: BaseMapping[R, A], R, K <: Mapping, V, O >: F <: FromClause]
		         (from :RelationSQL[F, T, R, O], component :K)(implicit project :OriginProjection[K, V])
				:ComponentSQL[F, project.WithOrigin] =
			TypedComponentSQL(from, component)

		def apply[F <: FromClause, T[A] <: BaseMapping[R, A], R, C <: ColumnMapping[_, _], V, O >: F <: FromClause]
		         (from :RelationSQL[F, T, R, O], column :C)
		         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: ColumnMapping[V, A] })
				:ColumnComponentSQL[F, project.WithOrigin, V] =
			TypedColumnComponentSQL(from, column)



		type * = ComponentSQL[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }



		trait Factory[M <: Mapping] {
			type Result[-F <: FromClause] <: ComponentSQL[F, M] forSome { type M[O] <: MappingAt[O] }
			type TypedResult[-F <: FromClause, T[A] <: BaseMapping[R, A], R, O >: F <: FromClause] <: Result[F]

			def apply[F <: FromClause, T[A] <: BaseMapping[R, A], R, O >: F <: FromClause]
			         (table :RelationSQL[F, T, R, O], mapping :M) :TypedResult[F, T, R, O]
		}

		sealed abstract class ComponentAbstractFactory {
			implicit def componentFactory[M <: Mapping, S](implicit project :OriginProjection[M, S])
					:Factory[M] {
						type Result[-F <: FromClause] = ComponentSQL[F, project.WithOrigin]
						type TypedResult[-F <: FromClause, T[A] <: BaseMapping[R, A], R, O >: F <: FromClause] =
							TypedComponentSQL[F, T, R, project.WithOrigin, S, O]
					} =
				new Factory[M] {
					type Result[-F <: FromClause] = ComponentSQL[F, project.WithOrigin]
					type TypedResult[-F <: FromClause, T[A] <: BaseMapping[R, A], R, O >: F <: FromClause] =
						TypedComponentSQL[F, T, R, project.WithOrigin, S, O]

					override def apply[F <: FromClause, T[A] <: BaseMapping[R, A], R, O >: F <: FromClause]
					                  (table :RelationSQL[F, T, R, O], mapping :M) =
						TypedComponentSQL(table, mapping)
				}
		}

		object Factory extends ComponentAbstractFactory {
			implicit def columnFactory[M <: ColumnMapping[S, _], S]
			             (implicit project :OriginProjection[M, S] { type WithOrigin[O] <: ColumnMapping[S, O] })
					:Factory[M] {
						type Result[-F <: FromClause] = ColumnComponentSQL[F, project.WithOrigin, S]
						type TypedResult[-F <: FromClause, T[A] <: BaseMapping[R, A], R, O >: F <: FromClause] =
							TypedColumnComponentSQL[F, T, R, project.WithOrigin, S, O]
					} =
				new Factory[M] {
					type Result[-F <: FromClause] = ColumnComponentSQL[F, project.WithOrigin, S]
					type TypedResult[-F <: FromClause, T[A] <: BaseMapping[R, A], R, O >: F <: FromClause] =
						TypedColumnComponentSQL[F, T, R, project.WithOrigin, S, O]

					override def apply[F <: FromClause, T[A] <: BaseMapping[R, A], R, O >: F <: FromClause]
					                  (table :RelationSQL[F, T, R, O], mapping :M) =
						TypedColumnComponentSQL(table, mapping)
				}
		}

	}



	trait TypedComponentSQL[-F <: FromClause, T[A] <: BaseMapping[R, A], R,
	                        M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		extends ComponentSQL[F, M]
	{
		override type Origin = O
		override type Entity[A] = T[A]

		def projection :IsomorphicProjection[M, V, O]

		override def groupingRelation :Relation[M] = GroupingRelation[F, M, V, O](this)(projection)

		override def origin :RelationSQL[F, T, R, O]

		def extract :MappingExtract[R, V, O]

		override def readForm :SQLReadForm[V] = extract.export.selectForm


		override def \[K <: MappingAt[O], X](component :K)(implicit project :OriginProjection[K, X])
				:TypedComponentSQL[F, T, R, project.WithOrigin, X, O] =
			if (component == entity)
				origin.asInstanceOf[TypedComponentSQL[F, T, R, project.WithOrigin, X, O]]
			else
				TypedComponentSQL(origin, component)

		override def \[K <: MappingAt[O]]
		              (component :M[O] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.TypedResult[F, T, R, O] =
			factory(origin, component(mapping))

		override def \[K <: ColumnMapping[_, O], X]
		              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, X, O] =
			//we don't need to check if column==entity as a column always has itself as its column and among extractors.
			TypedColumnComponentSQL(origin, column)


		override def basedOn[U <: F, E <: FromClause](base :E)(implicit extension :U PartOf E)
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: FromClause] =
			extend(base)(extension.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: FromClause]



		override def freeSelectFrom[E <: F with FreeFrom](from :E) :SelectMapping[E, M, V] =
			SelectSQL(from, this)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, this)


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TypedComponentSQL.* @unchecked]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case component :TypedComponentSQL.* @unchecked if canEqual(component) && component.canEqual(this) =>
				origin == component.origin && mapping == component.mapping //consider: should it compare extractor.export?
			case _ => false
		}

		override def hashCode :Int = origin.hashCode * 31 + mapping.hashCode
	}






	object TypedComponentSQL {

		def apply[F <: FromClause, T[A] <: BaseMapping[R, A], R, K <: Mapping, V, O >: F <: FromClause]
		         (from :RelationSQL[F, T, R, O], component :K)(implicit project :OriginProjection[K, V])
				:TypedComponentSQL[F, T, R, project.WithOrigin, V, O] =
			component match {
				case column :ColumnMapping[V @unchecked, O @unchecked] =>
					TypedColumnComponentSQL(from, column).asInstanceOf[TypedComponentSQL[F, T, R, project.WithOrigin, V, O]]
				case from.mapping =>
					from.asInstanceOf[TypedComponentSQL[F, T, R, project.WithOrigin, V, O]]
				case _ =>
					new ProperComponent[F, T, R, project.WithOrigin, V, O](from, project(component))(project.isomorphism)
			}



		def unapply[F <: FromClause, X](e :SQLExpression[F, _, X])
				:Option[(RelationSQL[F, T, R, O], MappingExtract[R, X, O])
						forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: FromClause }] =
			e match {
				case component :TypedComponentSQL.* @unchecked =>
					Some((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], MappingExtract[Any, X, F])
					])
				case _ => None
			}



		type * = TypedComponentSQL[F, T, R, M, V, O] forSome {
			type F <: FromClause; type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type AnyIn[-F <: FromClause] = TypedComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type Typed[-F <: FromClause, V] = TypedComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]
		}



		private[MappingSQL] class ProperComponent[-F <: FromClause, T[A] <: BaseMapping[R, A], R,
		                                          M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                                         (override val origin :RelationSQL[F, T, R, O],
		                                          override val mapping :M[O])
		                                         (implicit override val projection :IsomorphicProjection[M, V, O])
			extends TypedComponentSQL[F, T, R, M, V, O]
		{
			override val extract = entity(mapping)

			override val readForm :SQLReadForm[V] = super.readForm

			override def extend[U <: F, G <: FromClause]
			                   (base :G)(implicit ev :U ExtendedBy G, global :GlobalScope <:< GlobalScope)
					:ProperComponent[G, T, R, M, V, _ >: G <: FromClause] =
				new ProperComponent[G, T, R, M, V, G](  //type G as O is incorrect, but a correct O exists and we upcast anyway
                    origin.extend(base).asInstanceOf[RelationSQL[G, T, R, G]], mapping.asInstanceOf[M[G]]
                )(projection.isomorphism)

			override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
			                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, V] =
				matcher.component(this)
		}



		trait ComponentMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends RelationMatcher[F, Y] with ColumnComponentMatcher[F, Y]
		{
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
			             (e :TypedComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		trait MatchComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ComponentMatcher[F, Y] with CaseRelation[F, Y] with CaseColumnComponent[F, Y]
		{
			override def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
			                      (e :TypedColumnComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V] =
				component[T, R, M, V, O](e :TypedComponentSQL[F, T, R, M, V, O])
		}

		trait CaseComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchComponent[F, Y] {
			override def relation[T[A] <: BaseMapping[R, A], R, O >: F <: FromClause]
			                     (e :RelationSQL[F, T, R, O]) :Y[GlobalScope, R] =
				component(e)
		}
	}






	trait ColumnComponentSQL[-F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		extends ComponentSQL[F, M] with ColumnSQL[F, GlobalScope, V]
	{
		override def extract :ColumnMappingExtract[Entity[Origin]#Subject, V, Origin]

		//fixme: this should really use the selectForm, but it is not a ColumnForm due to buff excludes/const etc
		override def readForm :ColumnReadForm[V] = extract.export.form

		override def upcast :ColumnSQL[F, GlobalScope, Subject] = this

		override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)

		override def anchor(from :F) :ColumnComponentSQL[F, M, V] = this

		override def basedOn[U <: F, E <: FromClause]
		                    (base :E)(implicit extension :U PartOf E)
				:ColumnComponentSQL[E, M, V] =
			extend(base)(extension.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit extension :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:ColumnComponentSQL[E, M, V]

		override def selectFrom(from :F) :SelectColumnAs[from.Base, M, V] =
			if (from.isParameterized)
				throw new IllegalArgumentException(
					s"Cannot use a parameterized clause as a basis for select $this: $from."
				)
			else if (from.isSubselect) {
				subselectFrom(from.asInstanceOf[ExactSubselectOf[from.type, NonEmptyFrom]])
					.asInstanceOf[SelectColumnAs[from.Base, M, V]]
			} else
				freeSelectFrom(from.asInstanceOf[F with FreeFrom])

		override def freeSelectFrom[E <: F with FreeFrom](from :E) :SelectColumnMapping[E, M, V] =
			throw new UnsupportedOperationException(
				s"Expression $this :${this.unqualifiedClassName} can't be used as a select clause."
			)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectColumnMapping[B, from.type, M, V] =
			throw new UnsupportedOperationException(
				s"Expression $this :${this.unqualifiedClassName} can't be used as a select clause."
			)

	}



	trait TypedColumnComponentSQL[-F <: FromClause, T[A] <: BaseMapping[R, A], R,
	                              M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		extends TypedComponentSQL[F, T, R, M, V, O] with ColumnComponentSQL[F, M, V]
	{
		override def basedOn[U <: F, E <: FromClause]
		                    (base :E)(implicit extension :U PartOf E)
				:TypedColumnComponentSQL[E, T, R, M, V, _ >: E <: FromClause] =
			extend(base)(extension.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:TypedColumnComponentSQL[E, T, R, M, V, _ >: E <: FromClause]


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, V] =
			matcher.component(this)


		override def freeSelectFrom[E <: F with FreeFrom](from :E) :SelectColumnMapping[E, M, V] =
			SelectSQL(from, this)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectColumnMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, this) //todo: from.type is ugly!


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TypedColumnComponentSQL.* @unchecked]
	}






	object TypedColumnComponentSQL {

		def apply[F <: FromClause, T[A] <: BaseMapping[R, A], R, C <: ColumnMapping[_, _], V, O >: F <: FromClause]
		         (from :RelationSQL[F, T, R, O], column :C)
		         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: ColumnMapping[V, A] })
				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, V, O] =
			new ProperColumn[F, T, R, project.WithOrigin, V, O](from, project(column))(project.isomorphism)



		def unapply[F <: FromClause, X](e :SQLExpression[F, _, X])
				:Option[(RelationSQL[F, T, R, O], ColumnMappingExtract[R, X, O])
						forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: FromClause }] =
			e match {
				case component :TypedColumnComponentSQL.* @unchecked =>
					Some((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], ColumnMappingExtract[Any, X, F])
					])
				case _ => None
			}



		type * = TypedColumnComponentSQL[F, T, R, M, V, O] forSome {
			type F <: FromClause; type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type AnyIn[-F <: FromClause] = TypedColumnComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type Typed[-F <: FromClause, V] = TypedColumnComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: FromClause
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]
		}



		private class ProperColumn[-F <: FromClause, T[A] <: BaseMapping[R, A], R,
		                           M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                          (relation :RelationSQL[F, T, R, O], column :M[O])
		                          (implicit project :IsomorphicProjection[M, V, O])
			extends ProperComponent[F, T, R, M, V, O](relation, column)
			   with TypedColumnComponentSQL[F, T, R, M, V, O]
		{
			override val extract = entity(mapping)
			//fixme: sort out where the buff-related modifications take place to have consistent assembly semantics
			override val readForm = super.readForm

			override def extend[U <: F, E <: FromClause]
			                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
					:ProperColumn[E, T, R, M, V, _ >: E <: FromClause] =
				new ProperColumn[E, T, R, M, V, E](
					origin.extend(base).asInstanceOf[RelationSQL[E, T, R, E]], column.asInstanceOf[M[E]]
				)(project.isomorphism)
		}



		trait ColumnComponentMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
			             (e :TypedColumnComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		type MatchColumnComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentMatcher[F, Y]

		type CaseColumnComponent[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentMatcher[F, Y]
	}






	sealed trait JoinedRelation[F <: FromClause, T[A] <: MappingAt[A]] extends ComponentSQL[F, T] {
		override type Origin = F
		override type Entity[A] = T[A]

		type Self = RelationSQL[F, M, T[F]#Subject, F] forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

		/** Offset of this relation in the clause `F`, counting ''from right to left'' and starting with 0. */
		def shift :Int

		override def origin :JoinedRelation[F, T] = this

		def toRelationSQL :RelationSQL[F, M, T[F]#Subject, F]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

		/** Casts down this instance to the more strongly typed `RelationSQL`. The result type is an `Option`
		  * not because the cast may fail, but because the correct existential subtype cannot exist as a value
		  * (returning simply the existentially refined `RelationSQL` produces a compiler error about being
		  * unable to abstract over existential higher type `T`).
		  */
		def asRelationSQL :Option[RelationSQL[F, M, T[F]#Subject, F]
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
		extends JoinedRelation[O, T] with TypedComponentSQL[F, T, R, T, R, O]
	{
		private[sql] def this(relation :Relation[T], shift :Int) = this(relation, relation[O], shift)


		override def projection :IsomorphicProjection[T, R, O] = OriginProjection.isomorphism

		override def groupingRelation :Relation[T] = relation

		override def origin :RelationSQL[O, T, R, O] = toRelationSQL

		override val extract = MappingExtract.ident(mapping)

		@inline final override def toRelationSQL :RelationSQL[O, T, R, O] = this.asInstanceOf[RelationSQL[O, T, R, O]]

		override def asRelationSQL :Some[RelationSQL[O, T, R, O]] = Some(toRelationSQL)

		override def upcast :ComponentSQL[O, T] = this


		//these need to be overriden due to JoinedRelation's having wider bounds than the inherited implementations
		override def \[K <: MappingAt[O], X](component :K)(implicit project :OriginProjection[K, X])
				:TypedComponentSQL[O, T, R, project.WithOrigin, X, O] =
			if (component == entity)
				origin.asInstanceOf[TypedComponentSQL[O, T, R, project.WithOrigin, X, O]]
			else
				TypedComponentSQL(origin, component)

		override def \[K <: MappingAt[O]]
		              (component :T[O] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.TypedResult[O, T, R, O] =
			factory(toRelationSQL, component(mapping))

		override def \[K <: ColumnMapping[_, O], X]
		              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
				:TypedColumnComponentSQL[O, T, R, project.WithOrigin, X, O] =
			TypedColumnComponentSQL(toRelationSQL, column)


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
			matcher.relation(toRelationSQL)



		override def freeSelectFrom[E <: O with FreeFrom](from :E) :SelectMapping[E, T, R] =
			SelectSQL(from, toRelationSQL)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectMapping[B, from.type, T, R] =
			SelectSQL.subselect(from, toRelationSQL)


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
		        (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
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
		extends ColumnComponentMatcher[F, Y] with LooseColumnMatcher[F, Y]

	trait MappingMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MappingColumnMatcher[F, Y] with ComponentMatcher[F, Y] with LooseComponentMatcher[F, Y]
	{
		def mapping[S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]](e :MappingSQL[F, S, M]) :Y[S, M[Any]#Subject]
	}

	trait MatchMapping[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MappingMatcher[F, Y]
		with CaseComponent[F, Y] with CaseLooseComponent[F, Y]

	trait CaseMapping[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchMapping[F, Y] {

		override def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                      (e :TypedComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V] =
			mapping(e)

		override def looseComponent[J >: F <: FromClause, M[A] <: BaseMapping[X, A], X]
		                          (e :LooseComponent[J, M, X]) :Y[GlobalScope, X] =
			mapping(e)
	}

}



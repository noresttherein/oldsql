package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Chain, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnMappingExtract, ColumnReadForm, Mapping, MappingExtract, Relation, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.Relation.Table
import net.noresttherein.oldsql.sql.{AndFrom, ColumnSQL, Expanded, ParamSelect, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, PartOf, PrefixOf, TopFrom}
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn.LooseColumnMatcher
import net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.{CaseLooseComponent, LooseComponentMatcher}
import net.noresttherein.oldsql.sql.ast.MappingSQL.RelationSQL.{CaseRelation, RelationMatcher}
import net.noresttherein.oldsql.sql.ast.MappingSQL.TypedColumnComponentSQL.{CaseColumnComponent, ColumnComponentMatcher}
import net.noresttherein.oldsql.sql.ast.MappingSQL.TypedComponentSQL.{CaseComponent, ComponentMatcher, ProperComponent}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SelectAs, SelectColumnAs, SelectColumnMapping, SelectMapping, SubselectAs, SubselectColumnAs, SubselectColumnMapping, SubselectMapping, TopSelectAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.mechanics.{TableCount, TableOffset}
import net.noresttherein.oldsql.sql.ParamSelect.ParamSelectAs


//here be implicits
import net.noresttherein.oldsql.slang._






/** An SQL expression AST node represented by a mapping `M`. While `M` might be a subtype of
  * [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]], typically it is a component of some relation
  * from the ''from'' clause of an SQL select containing this expression. The value type of this expression is defined
  * as the mapped [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type, as seen by the application,
  * but will be represented in actual generated SQL as a tuple containing all columns of the mapping `M`.
  * If the expression is used literally as part of a ''select'' clause (either directly, or inside
  * a [[net.noresttherein.oldsql.sql.ast.TupleSQL tuple]]), default
  * [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns (those without a
  * [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]] buff) will be inlined in its place.
  * If used as part of a comparison in a ''where'' or ''having'' clause, the columns of the two expressions will
  * be compared ordinally.
  * @author Marcin Mościcki
  */
trait MappingSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]]
	extends SQLExpression[F, S, M[()]#Subject]
{
	type Origin <: RowProduct
	type Subject = M[()]#Subject

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
			topSelectFrom(from.asInstanceOf[F with GroundFrom])

	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectAs[M] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectAs[B, M] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

	override def paramSelectFrom[E <: F with TopFrom { type Params = P }, P <: Chain](from :E) :ParamSelectAs[P, M] =
		throw new UnsupportedOperationException(
			s"Expression $this :${this.localClassName} can't be used as a select clause."
		)

}






object MappingSQL {

	//All these classes would benefit with being parameterized with RefinedMapping and kin instead of BaseMapping,
	//but a bug in the compiler makes subclasses of a subclass of MappingSQL not conform to MappingSQL
	/** An expression evaluating to a component mapping of an undetermined at this point relation.
	  * It can be used as a part of other expressions, but any expression containing loose components must
	  * be [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] before actual SQL is generated.
	  * This is done automatically for any expression used as a ''select'' clause in any of the factory methods in
	  * [[net.noresttherein.oldsql.sql.ast.SelectSQL$ SelectSQL]], as well as for all expressions created/passed
	  * to [[net.noresttherein.oldsql.sql.RowProduct.where where]] method of
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as well as its relatives.
	  *
	  * There is an implicit conversion from `M[F] for some M[O] <: BaseMapping[S, O]` to `LooseComponent[F, M, S]`,
	  * providing that `F` is a `RowProduct` with an existing
	  * [[net.noresttherein.oldsql.sql.mechanics.TableCount TableCount]]; it assumes that mapping `M[F]` is
	  * a component of the first known relation in `F`. As `Origin` type is inherited from the parent mapping
	  * by all its components, and all accessor methods of
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] always return `T[F]`,
	  * where `F` is a super type of the source `RowProduct` and `T` is the first known relation in `F`,
	  * this holds true in the intended use case of lifting a component path to an `SQLExpression`.
	  * Note however that `Origin` type can be set explicitly to any type, which would lead to
	  * `NoSuchElementException` thrown when a `LooseComponent` is created with non-conforming `F` type.
	  * Type safety is therefore guaranteed only for mappings obtained from official API, without manual
	  * changing of the `Origin` type.
	  *
	  * This conversion can be triggered explicitly by invoking
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.alter alter]],
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.include include]], or
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.exclude exclude]] method of this class
	  * on a `Mapping`, which create a ''view'' of the mapping with certain subcomponents included/excluded from
	  * the database operation the resulting expression will be used in.
	  */
	class LooseComponent[F <: RowProduct, M[A] <: BaseMapping[V, A], V] private[MappingSQL]
	                    (override val mapping :M[F], val offset :Int,
	                     val includes :Unique[RefinedMapping[_, F]], val excludes :Unique[RefinedMapping[_, F]])
	                    (implicit val projection :IsomorphicProjection[M, V, F])
		extends MappingSQL[F, GlobalScope, M]
	{
		def this(mapping :M[F], offset :Int)(implicit projection :IsomorphicProjection[M, V, F]) =
			this(mapping, offset, Unique.empty, Unique.empty)

		includes.find(!mapping.contains(_)) match {
			case Some(c) => throw new IllegalArgumentException(
				s"Included mapping $c is not a component of $mapping: " +
					s"cannot create LooseComponent($mapping, $offset, $includes, $excludes)."
			)
			case _ =>
		}
		excludes.find(!mapping.contains(_)) match {
			case Some(c) => throw new IllegalArgumentException(
				s"Excluded mapping $c is not a component of $mapping: " +
					s"cannot create LooseComponent($mapping, $offset, $includes, $excludes)."
			)
			case _ =>
		}

		type Origin = F

		/** Applies all given functions to this mapping and creates an SQL expression representing this mapping
		  * with the components wrapped in
		  * [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]] included
		  * and those wrapped in [[net.noresttherein.oldsql.schema.Mapping.Mapping.ExcludedComponent ExcludedComponent]]
		  * excluded. This is very similar to [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]
		  * method on any mapping, but takes a sequence of functions, instead of ready components. This makes it
		  * more convenient to call on return values of functions and methods, as the mapping `M` doesn't need
		  * to be assigned to an intermediate `val`. Example:
		  * {{{
		  *     Mages select _.alter(_.spellbook.+, _.familiar.-)
		  * }}}
		  *
		  * This method and [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.+- +-]] method are equivalent.
		  * @throws IllegalArgumentException if any of the given components is not a component of this mapping.
		  */
		def alter(adjustments :M[F] => ComponentSelection[_, F]*) :LooseComponent[F, M, V] = {
			val newExcludes = adjustments.view.map(_(mapping)).collect {
				case ExcludedComponent(c) => mapping.export(c)
			}.to(Unique)
			val newIncludes = adjustments.view.map(_(mapping)).collect {
				case IncludedComponent(c) => mapping.export(c)
			}.to(Unique)
			new LooseComponent[F, M, V](mapping, offset,
				(includes.view ++ newIncludes).filterNot(newExcludes.toSet).to(Unique),
				(excludes.view.filterNot(newIncludes.toSet) ++ newExcludes).to(Unique)
			)(projection)
		}

		/** Applies all given functions to this mapping and creates an SQL expression representing this mapping
		  * with the components wrapped in
		  * [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]] included
		  * and those wrapped in [[net.noresttherein.oldsql.schema.Mapping.Mapping.ExcludedComponent ExcludedComponent]]
		  * excluded. This is very similar to [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]
		  * method on any mapping, but takes a sequence of functions, instead of ready components. This makes it
		  * more convenient to call on return values of functions and methods, as the mapping `M` doesn't need
		  * to be assigned to an intermediate `val`. Example:
		  * {{{
		  *     mage.alter(_.spellbook.+, _.familiar.-)
		  * }}}
		  *
		  * This method and [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.alter alter]] are equivalent.
		  * @throws IllegalArgumentException if any of the given components is not a component of this mapping.
		  */
		def +-(adjustments :M[F] => ComponentSelection[_, F]*) :LooseComponent[F, M, V] =
			alter(adjustments:_*)

		/** Applies all given functions to this mapping and creates an SQL expression representing this mapping
		  * with returned components marked for being included in whatever database operation this expression
		  * is going to be used in. The [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] of the components
		  * are not validated for allowing them to be used in such an operation - as the operation type is not known
		  * at this point - which will result in the include directive being ignored.
		  *
		  * If any of the listed components is present on
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.excludes excludes]] list of this expression,
		  * it is removed.
		  * @throws IllegalArgumentException if any of the given components is not a component of this mapping.
		  */
		def include(components :M[F] => RefinedMapping[_, F]*) :LooseComponent[F, M, V] = {
			val newIncludes = components.view.map(_(mapping)).map(mapping.export(_)).to(Unique)
			new LooseComponent(mapping, offset, includes ++ newIncludes, excludes.filterNot(newIncludes.toSet))
		}

		/** Applies all given functions to this mapping and creates an SQL expression representing this mapping
		  * with returned components marked for being excluded from whatever database operation this expression
		  * is going to be used in. The [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] of the components
		  * are not validated for allowing them to be excluded from such an operation - as the operation type
		  * is not known at this point - which will result in the exclude directive being ignored.
		  *
		  * If any of the listed components is present on
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.includes includes]] list of this expression,
		  * it is removed.
		  * @throws IllegalArgumentException if any of the given components is not a component of this mapping.
		  */
		def exclude(components :M[F] => RefinedMapping[_, F]*) :LooseComponent[F, M, V] = {
			val newExcludes = components.view.map(_(mapping)).map(mapping.export(_)).to(Unique)
			new LooseComponent(mapping, offset, includes.filterNot(newExcludes.toSet), excludes ++ newExcludes)
		}

		/** Applies the given function to this mapping and creates an SQL expression representing this mapping
		  * with the returned component included by default in all permitted database operation types.
		  * This is essentially the same as 
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.include include]]`(component)`,
		  * but provides syntax for adding components one by one, which is often preferable when only few components
		  * are include/excluded.
		  */
		def +(component :M[F] => RefinedMapping[_, F]) :LooseComponent[F, M, V] = {
			val cmp = mapping.export(component(mapping))
			new LooseComponent[F, M, V](mapping, offset, includes :+ cmp, excludes - cmp)
		}

		/** Applies the given function to this mapping and creates an SQL expression representing this mapping
		  * with the returned component excluded by default from all database operation types for which it is optional.
		  * This is essentially the same as 
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.exclude exclude]]`(component)`,
		  * but provides syntax for adding components one by one, which is often preferable when only few components
		  * are include/excluded.
		  */
		def -(component :M[F] => RefinedMapping[_, F]) :LooseComponent[F, M, V] = {
			val cmp = mapping.export(component(mapping))
			new LooseComponent[F, M, V](mapping, offset, includes - cmp, excludes :+ cmp)
		}
		
		/** This component with `includes` and `excludes` list cleared, making the export version of this mapping
		  * the mapping itself.
		  */
		def default :LooseComponent[F, M, V] =
			if (excludes.isEmpty && includes.isEmpty) this
			else new LooseComponent(mapping, offset)


		override def readForm :SQLReadForm[V] = mapping.selectForm
		override def isGlobal = true
		override def asGlobal :Option[GlobalSQL[F, V]] = Some(this)
		override def isAnchored = false

		override def anchor(from :F) :ComponentSQL[F, M] = {
			val relation = from.fullTableStack(offset).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
			relation.include(includes).exclude(excludes) \ mapping
		}


		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :LooseComponent[E, M, V] =
			new LooseComponent[E, M, V](
				mapping.asInstanceOf[M[E]], offset + ext.diff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)(projection.isomorphism[E])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:LooseComponent[E, M, V] =
			new LooseComponent[E, M, V](mapping.asInstanceOf[M[E]], offset + ev.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(projection.isomorphism[E])


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, M[F]#Subject] =
			matcher.looseComponent(this)


		override def topSelectFrom[E <: F with GroundFrom](from :E) :SelectMapping[E, M, V] =
			SelectSQL(from, anchor(from))

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, anchor(from))

		override def paramSelectFrom[E <: F with TopFrom { type Params = P }, P <: Chain](from :E) :ParamSelectAs[P, M] =
			ParamSelect[E, M, V](from, anchor(from))


		override def isomorphic(expression :SQLExpression.*) :Boolean = equals(expression)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[LooseComponent.* @unchecked]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true

			case free :LooseComponent.* @unchecked if free canEqual this =>
				free.mapping == mapping && free.offset == offset &&
					includes.toSet == free.includes.toSet && excludes.toSet == free.excludes.toSet

			case _ => false
		}

		override def hashCode :Int = mapping.hashCode * 31 + offset.hashCode


		override lazy val toString :String =
			if (includes.isEmpty && excludes.isEmpty)
				"_#" + offset + "." + mapping
			else
				(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(s"_#$offset.$mapping(", ", ", ")")
	}



	object LooseComponent {

		private[oldsql] def apply[F <: RowProduct, C <: Mapping, V]
		                         (mapping :C, offset :Int)
		                         (implicit cast :C <:< MappingAt[F], project :OriginProjection[C, V])
				:LooseComponent[F, project.WithOrigin, V] =
			project[F](mapping) match {
				case column :ColumnMapping[V @unchecked, F @unchecked] =>
					implicit val projection = OriginProjection.isomorphism[MappingOf[V]#ColumnProjection, V, F]
					new LooseColumn[F, MappingOf[V]#ColumnProjection, V](column, offset)
						.asInstanceOf[LooseComponent[F, project.WithOrigin, V]]
				case component =>
					new LooseComponent[F, project.WithOrigin, V](component, offset)(project.isomorphism)
			}

		def apply[F <: RowProduct, C <: Mapping, V]
		         (mapping :C) //TableCount, not TableOffset, because M is likely a component, not a table
		         (implicit cast :C <:< MappingAt[F], offset :TableCount[F, _ <: Numeral],
		                   project :OriginProjection[C, V])
				:LooseComponent[F, project.WithOrigin, V] =
				apply(mapping, offset.offset)


		def unapply[F <: RowProduct, X](expr :SQLExpression[F, _, X])
				:Opt[(BaseMapping[X, _ >: F <: RowProduct], Int)] =
			expr match {
				case free: LooseComponent.Typed[F, X] @unchecked => Got(free.mapping -> free.offset)
				case _ => Lack
			}

		def unapply[F <: RowProduct, M[A] <: BaseMapping[X, A], X](expr :LooseComponent[F, M, X]) :Opt[(M[F], Int)] =
			Got(expr.mapping -> expr.offset)



		type * = LooseComponent[F, M, V] forSome {
			type F <: RowProduct; type M[A] <: BaseMapping[V, A]; type V
		}

		private type AnyIn[-F <: RowProduct] = LooseComponent[O, M, V] forSome {
			type O >: F <: RowProduct; type M[A] <: BaseMapping[V, A]; type V
		}

		private type Typed[-F <: RowProduct, V] = LooseComponent[O, M, V] forSome {
			type O >: F <: RowProduct; type M[A] <: BaseMapping[V, A]
		}



		trait LooseComponentMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends LooseColumnMatcher[F, Y] {
			def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[X, A], X]
			                  (e :LooseComponent[O, M, X]) :Y[GlobalScope, X]
		}

		trait MatchLooseComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends LooseComponentMatcher[F, Y] {
			override def looseComponent[O >: F <: RowProduct, M[A] <: ColumnMapping[V, A], V]
			                           (e :LooseColumn[O, M, V]) :Y[GlobalScope, V] =
				looseComponent(e :LooseComponent[O, M, V])
		}

		type CaseLooseComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = MatchLooseComponent[F, Y]

	}






	/** An expression evaluating to a column mapping of an undetermined at this point relation.
	  * It can be used as a part of other expressions, but any expression containing loose components must
	  * be [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] before actual SQL is generated.
	  * This is done automatically for any expression used as a ''select'' clause in any of the factory methods in
	  * [[net.noresttherein.oldsql.sql.ast.SelectSQL$ SelectSQL]], as well as for all expressions created/passed
	  * to [[net.noresttherein.oldsql.sql.RowProduct.where where]] method of
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] and as its relatives.
	  *
	  * There is an implicit conversion from `C[O] <: `[[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[S, O]`
	  * to `LooseColumn[O, C, S]`, providing `O <: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
	  * See more about its details and guarantees in the documentation of the
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent base class]] of this class.
	  */
	class LooseColumn[F <: RowProduct, M[A] <: ColumnMapping[V, A], V] private[MappingSQL]
	                 (column :M[F], shift :Int,
	                  incl :Unique[RefinedMapping[_, F]], excl :Unique[RefinedMapping[_, F]])
	                 (implicit project :IsomorphicProjection[M, V, F])
		extends LooseComponent[F, M, V](column, shift, incl, excl) with ColumnSQL[F, GlobalScope, V]
	{
		def this(column :M[F], shift :Int)(implicit project :IsomorphicProjection[M, V, F]) =
			this(column, shift, Unique.empty, Unique.empty)

		//this should really use mapping.selectForm, bot it is not a ColumnForm due to possible buffs
		//doesn't really matter though, as this class is a placeholder and the form will never get used.
		override def readForm :ColumnReadForm[V] = mapping.form

		override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)


		private def include(components :Iterable[RefinedMapping[_, F]]) :LooseColumn[F, M, V] =
			if (components.isEmpty) this
			else {
				val newIncludes = components.to(Unique)
				if (newIncludes.size == 1)
					if (newIncludes.head == mapping)
						if (includes.nonEmpty) this
						else new LooseColumn[F, M, V](mapping, offset, newIncludes, Unique.empty)
					else
						throw new IllegalArgumentException(
							s"Cannot include ${newIncludes.head} in column $mapping: different component."
						)
			    else
					throw new IllegalArgumentException(
						s"Cannot include $newIncludes in column $mapping: the only component of a column is the column itself."
					)
			}

		private def exclude(components :Iterable[RefinedMapping[_, F]]) :LooseColumn[F, M, V] =
			if (components.isEmpty) this
			else {
				val newExcludes = components.to(Unique)
				if (newExcludes.size == 1)
					if (newExcludes.head == mapping)
						if (excludes.nonEmpty) this
						else new LooseColumn[F, M, V](mapping, offset, Unique.empty, newExcludes)
					else
						throw new IllegalArgumentException(
							s"Cannot exclude ${newExcludes.head} from column $mapping: different component."
						)
				else
					throw new IllegalArgumentException(
						s"Cannot exclude $newExcludes from column $mapping: the only component of a column is the column itself."
					)
			}

		override def include(components :M[F] => RefinedMapping[_, F]*) :LooseColumn[F, M, V] =
			include(components.view.map(_(mapping)))
		
		override def exclude(components :M[F] => RefinedMapping[_, F]*) :LooseColumn[F, M, V] =
			exclude(components.view.map(_(mapping)))

		/** Applies the given function to this column, which must return a column with the same ''export'' version
		  * as this column, and creates an SQL expression representing this column with the returned column included 
		  * by default in all permitted database operation types. This is essentially the same as 
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn.include include]]`(component)`,
		  * but provides syntax for adding components one by one, which is often preferable when only few components
		  * are include/excluded.
		  */
		override def +(component :M[F] => RefinedMapping[_, F]) :LooseColumn[F, M, V] = {
			val cmp = mapping.export(component(mapping))
			new LooseColumn[F, M, V](mapping, offset, includes :+ cmp, excludes - cmp)
		}

		/** Applies the given function to this column, which must return a column with the same ''export'' version
		  * as this column, and creates an SQL expression representing this mapping with the returned column excluded 
		  * by default from all database operation types for which it is optional. This is essentially the same as 
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn.exclude exclude]]`(component)`,
		  * but provides syntax for adding components one by one, which is often preferable when only few components
		  * are include/excluded.
		  */
		override def -(component :M[F] => RefinedMapping[_, F]) :LooseColumn[F, M, V] = {
			val cmp = mapping.export(component(mapping))
			new LooseColumn[F, M, V](mapping, offset, includes - cmp, excludes :+ cmp)
		}
		
		override def default :LooseColumn[F, M, V] =
			if (includes.isEmpty && excludes.isEmpty) this
			else new LooseColumn(mapping, offset)


		override def anchor(from :F) :ColumnComponentSQL[F, M, V] = {
			val relation = from.fullTableStack(offset).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
			relation.include(includes).exclude(excludes) \ mapping
		}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :LooseColumn[E, M, V] =
			new LooseColumn[E, M, V](column.asInstanceOf[M[E]], offset + ext.diff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(
				projection.isomorphism
			)

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:LooseColumn[E, M, V] =
			new LooseColumn[E, M, V](column.asInstanceOf[M[E]], offset + ev.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(
				projection.isomorphism
			)


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
				topSelectFrom(from.asInstanceOf[F with GroundFrom])

		override def topSelectFrom[E <: F with GroundFrom](from :E) :SelectColumnMapping[E, M, V] =
			SelectSQL(from, anchor(from))

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectColumnMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, anchor(from))

		override def paramSelectFrom[E <: F with TopFrom {type Params = P }, P <: Chain](from :E) :ParamSelectAs[P, M] =
			ParamSelect[E, M, V](from, anchor(from))
	}



	object LooseColumn {

		private[oldsql] def apply[F <: RowProduct, C <: ColumnMapping[_, _], V]
		                         (column :C, shift :Int)
		                         (implicit cast :C <:< ColumnMapping[V, F],
		                                   project :OriginProjection[C, V] { type WithOrigin[O] <: ColumnMapping[V, O] })
				:LooseColumn[F, project.WithOrigin, V] =
			new LooseColumn[F, project.WithOrigin, V](project[F](column), shift)(project.isomorphism)


		def apply[F <: RowProduct, C <: ColumnMapping[_, _], V]
		         (column :C) //TableCount, not TableOffset, because M is likely a component, not a table
		         (implicit cast :C <:< ColumnMapping[V, F], shift :TableCount[F, _ <: Numeral],
		                   project :OriginProjection[C, V] { type WithOrigin[O] <: ColumnMapping[V, O] })
				:LooseColumn[F, project.WithOrigin, V] =
			apply(column, shift.offset)


		def unapply[F <: RowProduct, X](expr :SQLExpression[F, _, X])
				:Opt[(ColumnMapping[X, _ >: F <: RowProduct], Int)] =
			expr match {
				case free: LooseColumn.Typed[F, X] @unchecked => Got(free.mapping -> free.offset)
				case _ => Lack
			}

		def unapply[F <: RowProduct, M[A] <: BaseMapping[X, A], X](expr :LooseComponent[F, M, X]) :Opt[(M[F], Int)] =
			(expr :LooseComponent.*) match {
				case _ :LooseColumn.* @unchecked => Got(expr.mapping -> expr.offset)
				case _ => Lack
			}



		type * = LooseColumn[_ <: RowProduct, M, V] forSome { type M[A] <: ColumnMapping[V, A]; type V }

		type AnyIn[-F <: RowProduct] = LooseColumn[O, M, V]
				forSome { type O >: F <: RowProduct; type M[A] <: ColumnMapping[V, A]; type V }

		type Typed[-F <: RowProduct, V] = LooseColumn[O, M, V]
				forSome { type O >: F <: RowProduct; type M[A] <: ColumnMapping[V, A] }



		trait LooseColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def looseComponent[O >: F <: RowProduct, M[A] <: ColumnMapping[V, A], V]
			                  (e :LooseColumn[O, M, V]) :Y[GlobalScope, V]
		}

		type MatchLooseColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = LooseColumnMatcher[F, Y]

		type CaseLooseColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = LooseColumnMatcher[F, Y]

	}






	trait ComponentSQL[-F <: RowProduct, M[A] <: MappingAt[A]] extends MappingSQL[F, GlobalScope, M] {
		//consider: in quite a few places we create 'copies' of the expression using `table \ this.mapping`
		// this is not very OO as it will convert any custom subclass of this trait with default implementation
		/** The mapping type of the `SQLRelation` to which this component belongs. */
		type Entity[A] <: MappingAt[A]

		override def readForm :SQLReadForm[M[Unit]#Subject] = export.selectForm

		/** A pseudo relation adapting this expression for use in a ''group by'' clauses
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and [[net.noresttherein.oldsql.sql.By By]].
		  */
		def groupingRelation :Relation[M]

		def entity :Entity[Origin] = origin.mapping
		def relation :Relation[Entity] = origin.relation
		def origin :JoinedRelation[Origin, Entity]

		def extract :MappingExtract[Entity[Origin]#Subject, M[Origin]#Subject, Origin]
		def export :RefinedMapping[M[()]#Subject, Origin]

		def alter(components :M[Origin] => ComponentSelection[_, Origin]*) :ComponentSQL[F, M]
		def +-(components :M[Origin] => ComponentSelection[_, Origin]*) :ComponentSQL[F, M]

		def include(components :Iterable[RefinedMapping[_, Origin]]) :ComponentSQL[F, M]
		def exclude(components :Iterable[RefinedMapping[_, Origin]]) :ComponentSQL[F, M]

		def include(components :M[Origin] => RefinedMapping[_, Origin]*) :ComponentSQL[F, M]
		def exclude(components :M[Origin] => RefinedMapping[_, Origin]*) :ComponentSQL[F, M]

		def +(component :M[Origin] => RefinedMapping[_, Origin]) :ComponentSQL[F, M]
		def -(component :M[Origin] => RefinedMapping[_, Origin]) :ComponentSQL[F, M]
		
		def default :ComponentSQL[F, M]

		def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
				:ComponentSQL[F, project.WithOrigin]

		def \[K <: MappingAt[Origin]]
		     (component :M[Origin] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.Result[F]

		def \[K <: ColumnMapping[_, Origin], X]
		     (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
				:ColumnComponentSQL[F, project.WithOrigin, X]


		/** An expression for the same component, but from the first
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation JoinedRelation]] of ''from'' clause `P`.
		  * Created expression is equal to this one, except for its `origin`, which is substituted with
		  * `origin.moveTo(offset)`.
		  * @param offset a proof that the first relation in ''from'' clause `P` is the same as the `origin` relation
		  *               of this component (or, more precisely, they use the same mapping type `Entity`),
		  *               carrying the offset of the new origin.
		  */
		def moveTo[P <: RowProduct](offset :TableOffset[P, Entity]) :ComponentSQL[P, M]

		/** An expression for the same component, but with the given relation used as its
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.origin origin]].
		  */
		def graft[P <: RowProduct](relation :JoinedRelation[P, Entity]) :ComponentSQL[P, M]

		override def isGlobal = true
		override def asGlobal :Option[GlobalSQL[F, Subject]] = Some(this)
		override def isAnchored = true
		override def anchor(from :F) :ComponentSQL[F, M] = this


		override def isomorphic(expression :SQLExpression.*) :Boolean = this == expression

		private[oldsql] override def equivalent(expression :SQLExpression.*) :Boolean = expression match {
			case self :AnyRef if self eq this => true
			case component :ComponentSQL[_, _] if component canEqual this =>
				relation == component.relation && mapping == component.mapping
			case _ => false
		}


		override def toString :String = origin.toString + "." + mapping
	}



	object ComponentSQL { //support for AlteredMapping
		//fixme: again, buggy overload resolution picks this instead of the following one even when given a relation
		//  and the order of method declaration doesn't seem to have any effect in this case.
		def apply[F <: RowProduct, M <: BaseMapping[S, O], S, O >: F <: RowProduct]
		         (from :F, component :M)
		         (implicit offset :TableCount[O, _ <: Numeral], project :OriginProjection[M, S])
				:ComponentSQL[F, project.WithOrigin] =
		{
			val relation = from.fullTableStack(offset.offset).toRelationSQL
			                   .asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, O]]
			TypedComponentSQL(relation, component)
		}

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, K <: Mapping, V, O >: F <: RowProduct]
		         (from :RelationSQL[F, T, R, O], component :K)(implicit project :OriginProjection[K, V])
				:ComponentSQL[F, project.WithOrigin] =
			TypedComponentSQL(from, component)

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, C <: ColumnMapping[_, _], V, O >: F <: RowProduct]
		         (from :RelationSQL[F, T, R, O], column :C)
		         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: ColumnMapping[V, A] })
				:ColumnComponentSQL[F, project.WithOrigin, V] =
			TypedColumnComponentSQL(from, column)



		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[F, T, R, O], MappingExtract[R, X, O])
						forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: RowProduct }] =
			e match {
				case component :ComponentSQL.* @unchecked =>
					Got((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], MappingExtract[Any, X, F])
					])
				case _ => Lack
			}



		type * = ComponentSQL[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }



		trait Factory[M <: Mapping] {
			type Result[-F <: RowProduct] <: ComponentSQL[F, M] forSome { type M[O] <: MappingAt[O] }
			type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] <: Result[F]

			def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
			         (table :RelationSQL[F, T, R, O], mapping :M) :TypedResult[F, T, R, O]
		}


		sealed abstract class ComponentAbstractFactory {
			implicit def componentFactory[M <: MappingOf[S], S](implicit project :OriginProjection[M, S])
					:Factory[M] {
						type Result[-F <: RowProduct] = ComponentSQL[F, project.WithOrigin]
						type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] =
							TypedComponentSQL[F, T, R, project.WithOrigin, S, O]
					} =
				new Factory[M] {
					type Result[-F <: RowProduct] = ComponentSQL[F, project.WithOrigin]
					type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] =
						TypedComponentSQL[F, T, R, project.WithOrigin, S, O]

					override def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
					                  (table :RelationSQL[F, T, R, O], mapping :M) =
						(table \ mapping.withOrigin[O])(project.isomorphism)
				}
		}


		object Factory extends ComponentAbstractFactory {
			implicit def columnFactory[M <: ColumnMapping[S, _], S]
			             (implicit project :OriginProjection[M, S] { type WithOrigin[O] <: ColumnMapping[S, O] })
					:Factory[M] {
						type Result[-F <: RowProduct] = ColumnComponentSQL[F, project.WithOrigin, S]
						type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] =
							TypedColumnComponentSQL[F, T, R, project.WithOrigin, S, O]
					} =
				new Factory[M] {
					type Result[-F <: RowProduct] = ColumnComponentSQL[F, project.WithOrigin, S]
					type TypedResult[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] =
						TypedColumnComponentSQL[F, T, R, project.WithOrigin, S, O]

					override def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
					                  (table :RelationSQL[F, T, R, O], mapping :M) =
						(table \ mapping.withOrigin[O])(project.isomorphism)
				}
		}

	}






	trait TypedComponentSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R,
	                        M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		extends ComponentSQL[F, M]
	{
		override type Origin = O
		override type Entity[A] = T[A]

		def projection :IsomorphicProjection[M, V, O]

		override def groupingRelation :Relation[M] = GroupingRelation[F, M, V, O](this)(projection)

		override def origin :RelationSQL[F, T, R, O]
		override def extract :MappingExtract[R, V, O]
		override def export :RefinedMapping[V, O] = extract.export


		override def alter(components :M[O] => ComponentSelection[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] = {
			val newExcludes = components.view.map(_(mapping)).collect { case ExcludedComponent(c) => c }
			val newIncludes = components.view.map(_(mapping)).collect { case IncludedComponent(c) => c }
			include(newIncludes).exclude(newExcludes)
		}

		override def +-(components :M[O] => ComponentSelection[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] =
			alter(components :_*)

		override def include(components :Iterable[RefinedMapping[_, O]]) :TypedComponentSQL[F, T, R, M, V, O] =
		 	components.view.collect { case c if !export.contains(c) => c }.toList match {
			    case Nil => origin.include(components) \ mapping
				case comps => throw new IllegalArgumentException(
					s"Cannot include not belonging components $comps with mapping $mapping."
				)
			}

		override def exclude(components :Iterable[RefinedMapping[_, O]]) :TypedComponentSQL[F, T, R, M, V, O] =
			components.view.collect { case c if !export.contains(c) => c }.toList match {
				case Nil => origin.exclude(components) \ mapping
				case comps => throw new IllegalArgumentException(
					s"Cannot exclude not belonging components $comps from mapping $mapping."
				)
			}

		override def include(components :M[O] => RefinedMapping[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] =
			include(components.map(_(mapping)))

		override def exclude(components :M[O] => RefinedMapping[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] =
			exclude(components.map(_(mapping)))

		override def +(component :M[O] => RefinedMapping[_, O]) :TypedComponentSQL[F, T, R, M, V, O] =
			include(Unique.single(component(mapping)))

		override def -(component :M[O] => RefinedMapping[_, O]) :TypedComponentSQL[F, T, R, M, V, O] =
			exclude(Unique.single(component(mapping)))

		override def default :TypedComponentSQL[F, T, R, M, V, O] = graft(origin.default)


		override def \[K <: MappingAt[O], X](component :K)(implicit project :OriginProjection[K, X])
				:TypedComponentSQL[F, T, R, project.WithOrigin, X, O] =
			origin \ component

		override def \[K <: MappingAt[O]]
		              (component :M[O] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.TypedResult[F, T, R, O] =
			factory(origin, component(mapping))

		override def \[K <: ColumnMapping[_, O], X]
		              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, X, O] =
			//we don't need to check if column==entity as a column always has itself as its column and among extractors.
			origin \ column



		override def moveTo[P <: RowProduct](offset :TableOffset[P, T]) :TypedComponentSQL[P, T, R, M, V, P] =
			graft(origin.moveTo(offset))

		override def graft[P <: RowProduct]
		                       (relation :JoinedRelation[P, T]) :TypedComponentSQL[P, T, R, M, V, P] =
			relation.asInstanceOf[RelationSQL[P, T, R, P]] \ mapping.withOrigin[P]

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			graft(origin.expand(base).asInstanceOf[RelationSQL[E, T, R, E]])

		override def topSelectFrom[E <: F with GroundFrom](from :E) :SelectMapping[E, M, V] =
			SelectSQL(from, this)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, this)

		override def paramSelectFrom[E <: F with TopFrom { type Params = P }, P <: Chain](from :E) :ParamSelectAs[P, M] =
			ParamSelect[E, M, V](from, this)


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TypedComponentSQL.* @unchecked]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true

			case component :TypedComponentSQL.* @unchecked if canEqual(component) && component.canEqual(this) =>
				origin == component.origin && mapping == component.mapping
			case _ => false
		}

		override def hashCode :Int = origin.hashCode * 31 + mapping.hashCode
	}



	object TypedComponentSQL {

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, K <: Mapping, V, O >: F <: RowProduct]
		         (from :RelationSQL[F, T, R, O], component :K)(implicit project :OriginProjection[K, V])
				:TypedComponentSQL[F, T, R, project.WithOrigin, V, O] =
			component match {
				case column :ColumnMapping[V @unchecked, O @unchecked] =>
					TypedColumnComponentSQL(from, column)
						.asInstanceOf[TypedComponentSQL[F, T, R, project.WithOrigin, V, O]]
				case _ if component == from.mapping || component == from.export =>
					from.asInstanceOf[TypedComponentSQL[F, T, R, project.WithOrigin, V, O]]
				case _ =>
					new ProperComponent[F, T, R, project.WithOrigin, V, O](from, project(component))(project.isomorphism)
			}



		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[F, T, R, O], MappingExtract[R, X, O])
						forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: RowProduct }] =
			e match {
				case component :TypedComponentSQL.* @unchecked =>
					Got((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], MappingExtract[Any, X, F])
					])
				case _ => Lack
			}



		type * = TypedComponentSQL[F, T, R, M, V, O] forSome {
			type F <: RowProduct; type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type AnyIn[-F <: RowProduct] = TypedComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]; type V
		}

		type Typed[-F <: RowProduct, V] = TypedComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: BaseMapping[V, A]
		}


		private[MappingSQL] class ProperComponent[-F <: RowProduct, T[A] <: BaseMapping[R, A], R,
		                                          M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		                                         (override val origin :RelationSQL[F, T, R, O],
		                                          override val mapping :M[O])
		                                         (implicit override val projection :IsomorphicProjection[M, V, O])
			extends TypedComponentSQL[F, T, R, M, V, O]
		{
			override val extract :MappingExtract[R, V, O] = origin.export(mapping)

			override val readForm :SQLReadForm[V] = super.readForm

			//must be here because RelationSQL couldn't override a concrete definition due to extending invariant
			// JoinedRelation for some super type of F.
			override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
			                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, V] =
				matcher.component(this)
		}


		trait ComponentMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends RelationMatcher[F, Y] with ColumnComponentMatcher[F, Y]
		{
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
			             (e :TypedComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		trait MatchComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ComponentMatcher[F, Y] with CaseRelation[F, Y] with CaseColumnComponent[F, Y]
		{
			override def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
			                      (e :TypedColumnComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V] =
				component[T, R, M, V, O](e :TypedComponentSQL[F, T, R, M, V, O])
		}

		trait CaseComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchComponent[F, Y] {
			override def relation[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
			                     (e :RelationSQL[F, T, R, O]) :Y[GlobalScope, R] =
				component(e)
		}
	}






	trait ColumnComponentSQL[-F <: RowProduct, M[A] <: ColumnMapping[V, A], V]
		extends ComponentSQL[F, M] with ColumnSQL[F, GlobalScope, V]
	{ //consider: a fishy thing is that this column may be excluded from an operation, breaking(?) ColumnSQL contract
		override def upcast :ColumnSQL[F, GlobalScope, Subject] = this

		override def extract :ColumnMappingExtract[Entity[Origin]#Subject, V, Origin]
		override def export :ColumnMapping[V, Origin]

		override def readForm :ColumnReadForm[V] = export.selectForm match { //an alternative would be for the
			case select :ColumnReadForm[V @unchecked] => select //always the case unless the column has ExtraSelect
			case _ => throw new UnsupportedOperationException(  //being excluded doesn't affect being a column form
				s"No (Column)ReadForm for selecting column $this: most likely the column $export is not selectable."
			)
		}

		override def alter(components :M[Origin] => ComponentSelection[_, Origin]*) :ColumnComponentSQL[F, M, V]
		override def +-(components :M[Origin] => ComponentSelection[_, Origin]*) :ColumnComponentSQL[F, M, V]

		override def include(components :Iterable[RefinedMapping[_, Origin]]) :ColumnComponentSQL[F, M, V]
		override def exclude(components :Iterable[RefinedMapping[_, Origin]]) :ColumnComponentSQL[F, M, V]
		override def include(components :M[Origin] => RefinedMapping[_, Origin]*) :ColumnComponentSQL[F, M, V]
		override def exclude(components :M[Origin] => RefinedMapping[_, Origin]*) :ColumnComponentSQL[F, M, V]
		override def +(component :M[Origin] => RefinedMapping[_, Origin]) :ColumnComponentSQL[F, M, V]
		override def -(component :M[Origin] => RefinedMapping[_, Origin]) :ColumnComponentSQL[F, M, V]
		override def default :ColumnComponentSQL[F, M, V]


		override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)

		override def anchor(from :F) :ColumnComponentSQL[F, M, V] = this

		override def moveTo[P <: RowProduct](offset :TableOffset[P, Entity]) :ColumnComponentSQL[P, M, V]

		override def graft[P <: RowProduct](relation :JoinedRelation[P, Entity]) :ColumnComponentSQL[P, M, V]

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
				:ColumnComponentSQL[E, M, V] =
			expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
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
				topSelectFrom(from.asInstanceOf[F with GroundFrom])

		override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectColumnAs[M, V] =
			throw new UnsupportedOperationException(
				s"Expression $this :${this.localClassName} can't be used as a select clause."
			)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectColumnAs[B, M, V] =
			throw new UnsupportedOperationException(
				s"Expression $this :${this.localClassName} can't be used as a select clause."
			)

		override def paramSelectFrom[E <: F with TopFrom { type Params = P }, P <: Chain](from :E) :ParamSelectAs[P, M] =
			ParamSelect[E, M, V](from, this)
	}



	object ColumnComponentSQL {

		def apply[F <: RowProduct, M <: ColumnMapping[S, O], S, O >: F <: RowProduct]
		         (from :F, component :M)
		         (implicit offset :TableCount[O, _ <: Numeral],
		                   project :OriginProjection[M, S] { type WithOrigin[A] <: ColumnMapping[S, A] })
				:ColumnComponentSQL[F, project.WithOrigin, S] =
		{
			val relation = from.fullTableStack(offset.offset).toRelationSQL
				.asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, O]]
			TypedColumnComponentSQL(relation, component)
		}

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, C <: ColumnMapping[_, _], V, O >: F <: RowProduct]
		         (from :RelationSQL[F, T, R, O], column :C)
		         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: ColumnMapping[V, A] })
				:ColumnComponentSQL[F, project.WithOrigin, V] =
			ColumnComponentSQL(from, column)



		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[F, T, R, O], ColumnMappingExtract[R, X, O])
					forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: RowProduct }] =
			e match {
				case component :TypedColumnComponentSQL.* @unchecked =>
					Got((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], ColumnMappingExtract[Any, X, F])
					])
				case _ => Lack
			}

	}






	trait TypedColumnComponentSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R,
	                              M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		extends TypedComponentSQL[F, T, R, M, V, O] with ColumnComponentSQL[F, M, V]
	{
		override def export :ColumnMapping[V, O] = extract.export

		override def alter(components :M[O] => ComponentSelection[_, O]*) :TypedColumnComponentSQL[F, T, R, M, V, O] = {
			val excludes = components.view.map(_(mapping)).collect { case ExcludedComponent(c) => c }
			val includes = components.view.map(_(mapping)).collect { case IncludedComponent(c) => c }
			include(includes).exclude(excludes)
		}

		override def +-(components :M[O] => ComponentSelection[_, O]*) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			alter(components :_*)

		override def include(components :Iterable[RefinedMapping[_, O]]) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			if (components.isEmpty) this
			else {
				val newIncludes = components.view.map(origin.export.export(_)).to(Unique)
				if (newIncludes.size == 1)
					if (newIncludes.head == mapping || newIncludes.head == export)
						if (origin.includes.contains(mapping) || origin.includes.contains(export)) this
						else graft(origin.include(newIncludes))
					else
						throw new IllegalArgumentException(
							s"Cannot include ${newIncludes.head} in column $mapping: different component."
						)
				else
					throw new IllegalArgumentException(
						s"Cannot include $newIncludes in column $mapping: the only component of a column is the column itself."
					)
			}

		override def exclude(components :Iterable[RefinedMapping[_, O]]) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			if (components.isEmpty) this
			else {
				val newExcludes = components.view.map(origin.export.export(_)).to(Unique)
				if (newExcludes.size == 1)
					if (newExcludes.head == mapping || newExcludes.head == export)
						if (origin.excludes.contains(mapping) || origin.excludes.contains(export)) this
						else graft(origin.exclude(newExcludes))
					else
						throw new IllegalArgumentException(
							s"Cannot exclude ${newExcludes.head} from column $mapping: different component."
						)
				else
					throw new IllegalArgumentException(
						s"Cannot exclude $newExcludes from column $mapping: the only component of a column is the column itself."
					)
			}

		override def include(components :M[O] => RefinedMapping[_, O]*) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			include(components.view.map(_(mapping)))

		override def exclude(components :M[O] => RefinedMapping[_, O]*) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			exclude(components.view.map(_(mapping)))

		override def +(component :M[O] => RefinedMapping[_, O]) :TypedColumnComponentSQL[F, T, R, M, V, O] = {
			val cmp = component(mapping)
			val ex = origin.export.export(cmp)
			if (ex == export)
				if (origin.includes.contains(ex)) this else graft(origin + { t => ex })
			else
				throw new IllegalArgumentException(
					s"Cannot include $cmp in column $mapping: the only component of a column is the column itself."
				)
		}

		override def -(component :M[O] => RefinedMapping[_, O]) :TypedColumnComponentSQL[F, T, R, M, V, O] = {
			val cmp = component(mapping)
			val ex = origin.export.export(cmp)
			if (ex == export)
				if (origin.excludes.contains(ex)) this else graft(origin - { t => ex })
			else
				throw new IllegalArgumentException(
					s"Cannot exclude $cmp from column $mapping: the only component of a column is the column itself."
				)
		}


		override def default :TypedColumnComponentSQL[F, T, R, M, V, O] = graft(origin.default)



		override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :TypedColumnComponentSQL[E, T, R, M, V, E] =
			graft(origin.moveTo(offset))

		override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :TypedColumnComponentSQL[P, T, R, M, V, P] =
			relation.asInstanceOf[RelationSQL[P, T, R, P]] \ mapping.withOrigin[P]

		override def basedOn[U <: F, E <: RowProduct]
		                    (base :E)(implicit expansion :U PartOf E)
				:TypedColumnComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ext :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:TypedColumnComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			graft(origin.expand(base))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, V] =
			matcher.component(this)


		override def topSelectFrom[E <: F with GroundFrom](from :E) :SelectColumnMapping[E, M, V] =
			SelectSQL(from, this)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectColumnMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, this) //todo: from.type is ugly!


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TypedColumnComponentSQL.* @unchecked]
	}






	object TypedColumnComponentSQL {

		def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, C <: ColumnMapping[_, _], V, O >: F <: RowProduct]
		         (from :RelationSQL[F, T, R, O], column :C)
		         (implicit project :OriginProjection[C, V] { type WithOrigin[A] <: ColumnMapping[V, A] })
				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, V, O] =
			new ProperColumn[F, T, R, project.WithOrigin, V, O](from, project(column))(project.isomorphism)



		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X])
				:Opt[(RelationSQL[F, T, R, O], ColumnMappingExtract[R, X, O])
						forSome { type T[O] <: BaseMapping[R, O]; type R; type O >: F <: RowProduct }] =
			e match {
				case component :TypedColumnComponentSQL.* @unchecked =>
					Got((component.origin, component.extract).asInstanceOf[
						(RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F], ColumnMappingExtract[Any, X, F])
					])
				case _ => Lack
			}



		type * = TypedColumnComponentSQL[F, T, R, M, V, O] forSome {
			type F <: RowProduct; type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type AnyIn[-F <: RowProduct] = TypedColumnComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]; type V
		}

		type Typed[-F <: RowProduct, V] = TypedColumnComponentSQL[F, T, R, M, V, O] forSome {
			type O >: F <: RowProduct
			type T[A] <: BaseMapping[R, A]; type R
			type M[A] <: ColumnMapping[V, A]
		}



		private[MappingSQL] class ProperColumn[-F <: RowProduct, T[A] <: BaseMapping[R, A], R,
		                                       M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                                      (relation :RelationSQL[F, T, R, O], column :M[O])
		                                      (implicit project :IsomorphicProjection[M, V, O])
			extends ProperComponent[F, T, R, M, V, O](relation, column)
			   with TypedColumnComponentSQL[F, T, R, M, V, O]
		{
			override val extract :ColumnMappingExtract[R, V, O] = origin.export(mapping)

			override def export = extract.export

			//todo: sort out where the buff-related modifications take place to have consistent assembly semantics
			override val readForm = super.readForm
		}



		trait ColumnComponentMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
			             (e :TypedColumnComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		type MatchColumnComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentMatcher[F, Y]

		type CaseColumnComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentMatcher[F, Y]
	}






	//consider: unseal and 'seal' with a package private abstract method
	trait JoinedRelation[F <: RowProduct, T[A] <: MappingAt[A]] extends ComponentSQL[F, T] {
		override type Origin = F
		override type Entity[A] = T[A]

		type Self = RelationSQL[F, M, T[F]#Subject, F] forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

		/** Offset of this relation in the clause `F`, counting ''from right to left'' and starting with 0. */
		def offset :Int
		def safeOffset :TableOffset[F, T] = new TableOffset(offset)

		override def origin :JoinedRelation[F, T] = this

		def includes :Unique[RefinedMapping[_, F]]
		def excludes :Unique[RefinedMapping[_, F]]

		override def alter(components :T[F] => ComponentSelection[_, F]*) :JoinedRelation[F, T]
		override def +-(components :T[F] => ComponentSelection[_, F]*) :JoinedRelation[F, T]

		override def include(components :Iterable[RefinedMapping[_, F]]) :JoinedRelation[F, T]
		override def exclude(components :Iterable[RefinedMapping[_, F]]) :JoinedRelation[F, T]
		override def include(components :T[F] => RefinedMapping[_, F]*) :JoinedRelation[F, T]
		override def exclude(components :T[F] => RefinedMapping[_, F]*) :JoinedRelation[F, T]
		override def +(component :T[F] => RefinedMapping[_, F]) :JoinedRelation[F, T]
		override def -(component :T[F] => RefinedMapping[_, F]) :JoinedRelation[F, T]
		override def default :JoinedRelation[F, T]

		def toRelationSQL :RelationSQL[F, M, T[F]#Subject, F]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

		/** Casts down this instance to the more strongly typed `RelationSQL`. The result type is an `Option`
		  * not because the cast may fail, but because the correct existential subtype cannot exist as a value
		  * (returning simply the existentially refined `RelationSQL` produces a compiler error about being
		  * unable to abstract over existential higher type `T`).
		  */
		def asRelationSQL :Option[RelationSQL[F, M, T[F]#Subject, F]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }]

		/** Casts this relation down to the more strongly typed `TableSQL` if it is a table (persistent or derived). */
		def asTableSQL :Option[TableSQL[F, M, T[F]#Subject, F]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }] = None


		/** A new `JoinedRelation` identical to this one, but in ''from'' clause `E` at offset `offset`. */
		override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :JoinedRelation[E, T]

		/** Simply returns the given relation. */
		override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :JoinedRelation[P, T] = relation

		/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
		def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedRelation[E[F], T]

		/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
		  * and returns a `JoinedRelation`. The `expand` method cannot be overriden here to return a `JoinedRelation`
		  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
		  */
		def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedRelation[G, T]


		/** Checks if this instance and the argument use the same [[net.noresttherein.oldsql.schema.Relation Relation]]
		  * and have the same offset. When comparing relations from the same
		  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], this attests that the two relations refer to
		  * the same alias in the ''from'' clause (or a grouping expression in the ''group by'' clause),
		  * ignoring `includes` and `excludes` lists which alter the column set for all components of this relation.
		  */
		def same(that :JoinedRelation.*) :Boolean = relation == that.relation && offset == that.offset

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedRelation.*]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case relation :JoinedRelation.* if (this canEqual relation) && (relation canEqual this) =>
				relation.offset == offset && relation.mapping == mapping &&
					relation.includes.toSet == includes.toSet && relation.excludes.toSet == excludes.toSet
			case _ => false
		}

		override def hashCode :Int = offset * 31 + mapping.hashCode

		override lazy val toString :String =
			if (includes.isEmpty && excludes.isEmpty)
				mapping.toString + "#" + offset
			else
				(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(
					mapping.toString + "#" + offset + "(", ",", ")"
				)


		private[sql] def concrete_JoinedRelation_subclass_must_extend_RelationSQL :Nothing
	}



	object JoinedRelation {

		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Relation[MappingAt], Int)] =
			e match {
				case from :JoinedRelation.Typed[F, X] @unchecked =>
					Got(from.relation -> from.offset)
				case _ => Lack
			}


		type * = JoinedRelation[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

		type AnyIn[F <: RowProduct] = JoinedRelation[F, T] forSome { type T[O] <: MappingAt[O] }

		type Typed[F <: RowProduct, V] = JoinedRelation[F, T] forSome { type T[O] <: RefinedMapping[V, O] }

		type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedRelation[O, M] }
	}






	trait JoinedTable[F <: RowProduct, T[A] <: MappingAt[A]] extends JoinedRelation[F, T] {
		override def relation :Table[T] = toTableSQL.table //overriden in TableSQL, as this will cause a StackOverflowException
		def table :Table[T] = relation

		override def alter(components :T[F] => ComponentSelection[_, F]*) :JoinedTable[F, T]
		override def +-(components :T[F] => ComponentSelection[_, F]*) :JoinedTable[F, T]

		override def include(components :Iterable[RefinedMapping[_, F]]) :JoinedTable[F, T]
		override def exclude(components :Iterable[RefinedMapping[_, F]]) :JoinedTable[F, T]
		override def include(components :T[F] => RefinedMapping[_, F]*) :JoinedTable[F, T]
		override def exclude(components :T[F] => RefinedMapping[_, F]*) :JoinedTable[F, T]
		override def +(component :T[F] => RefinedMapping[_, F]) :JoinedTable[F, T]
		override def -(component :T[F] => RefinedMapping[_, F]) :JoinedTable[F, T]
		override def default :JoinedTable[F, T]

		def toTableSQL :TableSQL[F, M, T[F]#Subject, F]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

		/** Casts down this instance to the more strongly typed `RelationSQL`. The result type is an `Option`
		  * not because the cast may fail, but because the correct existential subtype cannot exist as a value
		  * (returning simply the existentially refined `RelationSQL` produces a compiler error about being
		  * unable to abstract over existential higher type `T`).
		  */
		def asTableSQL :Option[TableSQL[F, M, T[F]#Subject, F]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }]


		/** A new `JoinedTable` identical to this one, but in ''from'' clause `E` at offset `offset`. */
		override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :JoinedTable[E, T]

		/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
		def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedTable[E[F], T]

		/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
		  * and returns a `JoinedTable`. The `expand` method cannot be overriden here to return a `JoinedTable`
		  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
		  */
		def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedTable[G, T]

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedTable.*]

		private[sql] def concrete_JoinedTable_subclass_must_extend_TableSQL :Nothing
	}



	object JoinedTable {

		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Table[MappingAt], Int)] =
			e match {
				case from :JoinedTable.Typed[F, X] @unchecked =>
					Got(from.relation -> from.offset)
				case _ => Lack
			}


		type * = JoinedTable[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

		type AnyIn[F <: RowProduct] = JoinedTable[F, T] forSome { type T[O] <: MappingAt[O] }

		type Typed[F <: RowProduct, V] = JoinedTable[F, T] forSome { type T[O] <: RefinedMapping[V, O] }

		type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedTable[O, M] }
	}






	class RelationSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] protected
	                 (override val relation :Relation[T], override val offset :Int,
	                  override val includes :Unique[RefinedMapping[_, O]],
	                  override val excludes :Unique[RefinedMapping[_, O]])
		extends JoinedRelation[O, T] with TypedComponentSQL[F, T, R, T, R, O]
	{
		override def mapping :T[O] = relation[O]

		override val export :RefinedMapping[R, O] = {
			if (includes.isEmpty && excludes.isEmpty) relation.`export`[O]
			else relation.`export`[O].apply(includes, excludes)
		}.asInstanceOf[RefinedMapping[R, O]]

		override val extract = MappingExtract.ident(export)

		override def projection :IsomorphicProjection[T, R, O] = OriginProjection.isomorphism

		override def groupingRelation :Relation[T] = relation

		override def origin :RelationSQL[O, T, R, O] = toRelationSQL

		@inline final override def toRelationSQL :RelationSQL[O, T, R, O] = this.asInstanceOf[RelationSQL[O, T, R, O]]

		override def asRelationSQL :Some[RelationSQL[O, T, R, O]] = Some(toRelationSQL)

		override def upcast :ComponentSQL[O, T] = this


		override def alter(components :T[O] => ComponentSelection[_, O]*) :RelationSQL[F, T, R, O] = {
			val newExcludes = components.view.map(_(mapping)).collect {
				case ExcludedComponent(c) => export.export(c)
			}.to(Unique)
			val newIncludes = components.view.map(_(mapping)).collect {
				case IncludedComponent(c) => export.export(c)
			}.to(Unique)
			new RelationSQL[F, T, R, O](relation, offset,
				(includes.view ++ newIncludes).filterNot(newExcludes.toSet).to(Unique),
				(excludes.view.filterNot(newIncludes.toSet) ++ newExcludes).to(Unique)
			)
		}

		override def +-(components :T[O] => ComponentSelection[_, O]*) :RelationSQL[F, T, R, O] =
			alter(components :_*)

		override def include(components :Iterable[RefinedMapping[_, O]]) :RelationSQL[O, T, R, O] =
			if (components.isEmpty) toRelationSQL
			else {
				val newIncludes = components.view.map(export.export(_)).to(Unique)
				new RelationSQL[O, T, R, O](
					relation, offset, includes ++ newIncludes, excludes.filterNot(newIncludes.toSet)
				)
			}

		override def exclude(components :Iterable[RefinedMapping[_, O]]) :RelationSQL[O, T, R, O] =
			if (components.isEmpty) toRelationSQL
			else {
				val newExcludes = components.view.map(export.export(_)).to(Unique)
				new RelationSQL[O, T, R, O](
					relation, offset, includes.filterNot(newExcludes.toSet), excludes ++ newExcludes
				)
			}

		override def include(components :T[O] => RefinedMapping[_, O]*) :RelationSQL[O, T, R, O] =
			include(components.view.map(_(mapping)))

		override def exclude(components :T[O] => RefinedMapping[_, O]*) :RelationSQL[O, T, R, O] =
			exclude(components.view.map(_(mapping)))

		override def +(component :T[O] => RefinedMapping[_, O]) :RelationSQL[O, T, R, O] =
			include(Unique.single(component(mapping)))

		override def -(component :T[O] => RefinedMapping[_, O]) :RelationSQL[O, T, R, O] =
			exclude(Unique.single(component(mapping)))

		override def default :RelationSQL[O, T, R, O] =
			if (includes.isEmpty && excludes.isEmpty) toRelationSQL
			else new RelationSQL[O, T, R, O](relation, offset, Unique.empty, Unique.empty)


		//these need to be overriden due to JoinedRelation's having wider bounds than the inherited implementations
		override def \[K <: MappingAt[O], X](component :K)(implicit project :OriginProjection[K, X])
				:TypedComponentSQL[O, T, R, project.WithOrigin, X, O] =
			TypedComponentSQL(origin, component)

		override def \[K <: MappingAt[O]]
		              (component :T[O] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.TypedResult[O, T, R, O] =
			factory(toRelationSQL, component(mapping))

		override def \[K <: ColumnMapping[_, O], X]
		              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
				:TypedColumnComponentSQL[O, T, R, project.WithOrigin, X, O] =
			TypedColumnComponentSQL(toRelationSQL, column)



		/** A new `RelationSQL` instance representing the same relation, but in ''from'' clause `E` at `offset`.
		  * The class of the created instance and all its fields except for `offset` are the same as in this instance.
		  */
		override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :RelationSQL[E, T, R, E] =
			new RelationSQL[E, T, R, E](relation, offset.tables,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)

		/** Simply returns `relation.`[[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation.toRelationSQL toRelationSQL]]. */
		override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :RelationSQL[P, T, R, P] =
			relation.toRelationSQL.asInstanceOf[RelationSQL[P, T, R, P]]

		override def basedOn[U <: O, E <: RowProduct]
		                    (base :E)(implicit expansion :U PartOf E) :RelationSQL[E, T, R, _ >: E <: RowProduct] =
			new RelationSQL[E, T, R, E](relation, offset + expansion.diff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			) //E is incorrect, but we lose this information anyway

		override def expand[U <: O, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:RelationSQL[E, T, R, _ >: E <: RowProduct] =
			new RelationSQL[E, T, R, E](relation, offset + ev.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			) //E is incorrect, but we lose this information anyway

		override def asIn[J[+L <: O] <: L Expanded T forSome { type T[A] <: MappingAt[A] }] :RelationSQL[J[F], T, R, J[O]] =
			new RelationSQL[J[F], T, R, J[O]](relation, offset + 1,
				includes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]]
			)

		override def asIn[E <: RowProduct](implicit expansion :O PrefixOf E) :RelationSQL[E, T, R, E] =
			new RelationSQL(relation, offset + expansion.diff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[O, Y]) :Y[GlobalScope, R] =
			matcher.relation(toRelationSQL)

		
		override def topSelectFrom[E <: O with GroundFrom](from :E) :SelectMapping[E, T, R] =
			SelectSQL(from, toRelationSQL)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectMapping[B, from.type, T, R] =
			SelectSQL.subselect(from, toRelationSQL)

		override def paramSelectFrom[E <: O with TopFrom { type Params = P }, P <: Chain](from :E) :ParamSelectAs[P, T] =
			ParamSelect[E, T, R](from, toRelationSQL)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case rel :RelationSQL.* @unchecked if rel canEqual this =>
				relation == rel.relation && includes.toSet == rel.includes.toSet && excludes.toSet == rel.excludes.toSet
			case _ => false
		}

		override def hashCode :Int = relation.hashCode

		override private[sql] def concrete_JoinedRelation_subclass_must_extend_RelationSQL :Nothing =
			throw new UnsupportedOperationException
	}


	
	object RelationSQL {

//		def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S]
//		         (table :Relation[T])(implicit offset :TableOffset[F, T, _ <: Numeral]) :RelationSQL[F, T, S, F] =
//			new RelationSQL[F, T, S, F](table, table[F], offset.tables)

		def apply[F <: RowProduct] :RelationSQLFactory[F] = new RelationSQLFactory[F] {}

		trait RelationSQLFactory[F <: RowProduct] extends Any {
			final def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			          (relation :Relation[M])
			          (implicit cast :InferTypeParams[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]],
			                    shift :TableOffset[F, T]) :RelationSQL[F, T, S, F] =
				new RelationSQL[F, T, S, F](cast(relation), shift.tables, Unique.empty, Unique.empty)
		}

		private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
		                      (table :Relation[T], index :Int) :RelationSQL[F, T, S, O] =
			new RelationSQL[F, T, S, O](table, index, Unique.empty, Unique.empty)

		private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
		                      (table :Relation[T], index :Int,
		                       includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]])
				:RelationSQL[F, T, S, O] =
			new RelationSQL[F, T, S, O](table, index, includes, excludes)


		def last[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		        (table :Relation[M])
		        (implicit cast :InferTypeParams[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
				:LastRelation[T, S] =
			new RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
				table, 0, Unique.empty, Unique.empty
			)



		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Relation[MappingAt], Int)] =
			e match {
				case from :RelationSQL.Typed[F, X] @unchecked =>
					Got(from.relation.asInstanceOf[Relation[MappingOf[X]#TypedProjection]] -> from.offset)
				case _ => Lack
			}



		type * = RelationSQL[F, T, X, O] forSome {
			type F <: RowProduct; type O >: F <: RowProduct
			type T[A] <: BaseMapping[X, A]; type X
		}

		type AnyIn[-F <: RowProduct] = RelationSQL[F, T, R, O] forSome {
			type T[A] <: BaseMapping[R, A]; type R; type O >: F <: RowProduct
		}

		type Typed[-F <: RowProduct, R] = RelationSQL[F, T, R, O] forSome {
			type T[A] <: BaseMapping[R, O]; type O >: F <: RowProduct
		}

		type LastRelation[T[A] <: BaseMapping[S, A], S] = RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T]

		def LastRelation[T[A] <: BaseMapping[S, A], S](from :Relation[T]) :LastRelation[T, S] =
			new RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
				from, 0, Unique.empty, Unique.empty
			)



		trait RelationMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def relation[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
			            (e :RelationSQL[F, T, R, O]) :Y[GlobalScope, R]
		}

		type MatchRelation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = RelationMatcher[F, Y]

		type CaseRelation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = RelationMatcher[F, Y]

	}






	class TableSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
	              (override val relation :Table[T], override val offset :Int,
	               override val includes :Unique[RefinedMapping[_, O]],
	               override val excludes :Unique[RefinedMapping[_, O]])
		extends RelationSQL[F, T, R, O](relation, offset, includes, excludes)
		   with JoinedTable[O, T]
	{
		@inline final override def toTableSQL :TableSQL[O, T, R, O] = this.asInstanceOf[TableSQL[O, T, R, O]]

		override def asTableSQL :Some[TableSQL[O, T, R, O]] = Some(toTableSQL)


		override def alter(components :T[O] => ComponentSelection[_, O]*) :TableSQL[F, T, R, O] = {
			val newExcludes = components.view.map(_(mapping)).collect {
				case ExcludedComponent(c) => export.export(c)
			}.to(Unique)
			val newIncludes = components.view.map(_(mapping)).collect {
				case IncludedComponent(c) => export.export(c)
			}.to(Unique)
			new TableSQL[F, T, R, O](relation, offset,
				(includes.view ++ newIncludes).filterNot(newExcludes.toSet).to(Unique),
				(excludes.view.filterNot(newIncludes.toSet) ++ newExcludes).to(Unique)
			)
		}

		override def +-(components :T[O] => ComponentSelection[_, O]*) :TableSQL[F, T, R, O] =
			alter(components :_*)

		override def include(components :Iterable[RefinedMapping[_, O]]) :TableSQL[O, T, R, O] =
			if (components.isEmpty) toTableSQL
			else {
				val newIncludes = components.view.map(export.export(_)).to(Unique)
				new TableSQL[O, T, R, O](
					relation, offset, includes ++ newIncludes, excludes.filterNot(newIncludes.toSet)
				)
			}

		override def exclude(components :Iterable[RefinedMapping[_, O]]) :TableSQL[O, T, R, O] =
			if (components.isEmpty) toTableSQL
			else {
				val newExcludes = components.view.map(export.export(_)).to(Unique)
				new TableSQL[O, T, R, O](
					relation, offset, includes.filterNot(newExcludes.toSet), excludes ++ newExcludes
				)
			}

		override def include(components :T[O] => RefinedMapping[_, O]*) :TableSQL[O, T, R, O] =
			include(components.view.map(_(mapping)))

		override def exclude(components :T[O] => RefinedMapping[_, O]*) :TableSQL[O, T, R, O] =
			exclude(components.view.map(_(mapping)))

		override def +(component :T[O] => RefinedMapping[_, O]) :TableSQL[O, T, R, O] =
			include(Unique.single(component(mapping)))

		override def -(component :T[O] => RefinedMapping[_, O]) :TableSQL[O, T, R, O] =
			exclude(Unique.single(component(mapping)))

		override def default :TableSQL[O, T, R, O] =
			if (includes.isEmpty && excludes.isEmpty) toTableSQL
			else new TableSQL[O, T, R, O](relation, offset, Unique.empty, Unique.empty)


		
		/** A new `TableSQL` instance representing the same relation, but in ''from'' clause `E` at `offset`.
		  * The class of the created instance and all its fields except for `offset` are the same as in this instance.
		  */
		override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :TableSQL[E, T, R, E] =
			new TableSQL[E, T, R, E](relation, offset.tables,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)

		override def basedOn[U <: O, E <: RowProduct]
		                    (base :E)(implicit expansion :U PartOf E) :TableSQL[E, T, R, _ >: E <: RowProduct] =
			new TableSQL[E, T, R, E](relation, offset + expansion.diff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			) //E is incorrect, but we lose this information anyway

		override def expand[U <: O, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:TableSQL[E, T, R, _ >: E <: RowProduct] =
			new TableSQL[E, T, R, E](relation, offset + ev.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			) //E is incorrect, but we lose this information anyway

		override def asIn[J[+L <: O] <: L Expanded T forSome { type T[A] <: MappingAt[A] }] :TableSQL[J[F], T, R, J[O]] =
			new TableSQL[J[F], T, R, J[O]](relation, offset + 1,
				includes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]]
			)

		override def asIn[E <: RowProduct](implicit expansion :O PrefixOf E) :TableSQL[E, T, R, E] =
			new TableSQL(relation, offset + expansion.diff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[O, Y]) :Y[GlobalScope, R] =
			matcher.relation(toTableSQL)

		override private[sql] def concrete_JoinedTable_subclass_must_extend_TableSQL :Nothing =
			throw new UnsupportedOperationException
	}
	
	
	
	object TableSQL {

		def apply[F <: RowProduct] :TableSQLFactory[F] = new TableSQLFactory[F] {}

		trait TableSQLFactory[F <: RowProduct] extends Any {
			final def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
			               (table :Table[M])
			               (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]],
			                         offset :TableOffset[F, T]) :RelationSQL[F, T, S, F] =
				new TableSQL[F, T, S, F](cast(table), offset.tables, Unique.empty, Unique.empty)
		}

		private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
		                      (table :Table[T], index :Int) :RelationSQL[F, T, S, O] =
			new TableSQL[F, T, S, O](table, index, Unique.empty, Unique.empty)

		private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
		                      (table :Table[T], index :Int,
		                       includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]])
				:TableSQL[F, T, S, O] =
			new TableSQL[F, T, S, O](table, index, includes, excludes)


		def last[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		        (table :Table[M])
		        (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
				:LastTable[T, S] =
			new TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
				table, 0, Unique.empty, Unique.empty
			)



		def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Table[MappingAt], Int)] =
			e match {
				case from :TableSQL.Typed[F, X] @unchecked =>
					Got(from.relation.asInstanceOf[Table[MappingOf[X]#TypedProjection]] -> from.offset)
				case _ => Lack
			}



		type * = TableSQL[F, T, X, O] forSome {
			type F <: RowProduct; type O >: F <: RowProduct
			type T[A] <: BaseMapping[X, A]; type X
		}

		type AnyIn[-F <: RowProduct] = TableSQL[F, T, R, O] forSome {
			type T[A] <: BaseMapping[R, A]; type R; type O >: F <: RowProduct
		}

		type Typed[-F <: RowProduct, R] = TableSQL[F, T, R, O] forSome {
			type T[A] <: BaseMapping[R, O]; type O >: F <: RowProduct
		}

		type LastTable[T[A] <: BaseMapping[S, A], S] = TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T]

		def LastTable[T[A] <: BaseMapping[S, A], S](from :Table[T]) :LastTable[T, S] =
			new TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
				from, 0, Unique.empty, Unique.empty
			)



		trait TableMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def table[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct](e :TableSQL[F, T, R, O]) :Y[GlobalScope, R]
		}

		type MatchTable[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = TableMatcher[F, Y]

		type CaseTable[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = TableMatcher[F, Y]

	}

	
	
	
	
	
	trait MappingColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnComponentMatcher[F, Y] with LooseColumnMatcher[F, Y]

	trait MappingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MappingColumnMatcher[F, Y] with ComponentMatcher[F, Y] with LooseComponentMatcher[F, Y]
	{
		def mapping[S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]](e :MappingSQL[F, S, M]) :Y[S, M[()]#Subject]
	}

	trait MatchMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MappingMatcher[F, Y]
		with CaseComponent[F, Y] with CaseLooseComponent[F, Y]

	trait CaseMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchMapping[F, Y] {

		override def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V] =
			mapping(e)

		override def looseComponent[J >: F <: RowProduct, M[A] <: BaseMapping[X, A], X]
		                          (e :LooseComponent[J, M, X]) :Y[GlobalScope, X] =
			mapping(e)
	}

}



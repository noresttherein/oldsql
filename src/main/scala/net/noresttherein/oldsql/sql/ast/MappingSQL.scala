package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{Chain, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.Extractor.{Optional, Requisite}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, ColumnMappingExtract, ColumnReadForm, Mapping, MappingExtract, Relation, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.Relation.Table
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{AndFrom, ColumnSetter, ColumnSQL, ComponentSetter, Expanded, RowProduct, Select, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, PartOf, PrefixOf, TopFrom}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, GlobalSQL, Lift, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.ConversionSQL.{ColumnConversionSQL, ColumnPromotionConversion, PromotionConversion}
import net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnComponentSQL.ColumnComponentConversion
import net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnLValueSQL.BaseColumnComponentConversion
import net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentLValueSQL.BaseComponentConversion
import net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.ComponentConversion
import net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn.{LooseColumnConversion, LooseColumnVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.{CaseLooseComponent, LooseComponentConversion, LooseComponentVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.RelationSQL.{CaseRelation, RelationVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.TableSQL.TableVisitor
import net.noresttherein.oldsql.sql.ast.MappingSQL.TypedColumnComponentSQL.{CaseColumnComponent, ColumnComponentVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.TypedComponentSQL.{CaseComponent, ComponentVisitor, ProperComponent}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SelectAs, SelectColumnAs, SubselectAs, SubselectColumnAs, SubselectColumnMapping, SubselectMapping, TopSelectAs, TopSelectColumnAs, TopSelectColumnMapping, TopSelectMapping}
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLNumber, TableCount, TableOffset}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}


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
	extends SQLExpression[F, S, M[Unit]#Subject]
{
	type Origin >: F <: RowProduct
	type Subject = M[Unit]#Subject

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

	override def groundValue :Opt[Subject] = Lack

	override def anchor(from :F) :MappingSQL[F, GlobalScope, M]

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

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:SelectMapping[P, M] =
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
		extends MappingSQL[F, GlobalScope, M] with ComponentLValueSQL[F, M, V]
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

		override def readForm :SQLReadForm[V] = mapping.selectForm

		override def component :Nothing =
			throw new UnsupportedOperationException(
				s"An unanchored LooseComponent $this cannot be updated as its export version is unknown."
			)

		override type Origin = F

		/** Applies all given functions to this mapping and creates an SQL expression representing this mapping
		  * with the components wrapped in
		  * [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]] included
		  * and those wrapped in [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]]
		  * excluded. This is very similar to [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]
		  * method on any mapping, but takes a sequence of functions, instead of ready components. This makes it
		  * more convenient to call on return values of functions and methods, as the mapping `M` doesn't need
		  * to be assigned to an intermediate `val`. Example:
		  * {{{
		  *     Mages select _.alter(_.spellbook.+, _.familiar.-)
		  * }}}
		  *
		  * This method and [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.+- +-]] method are equivalent.
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
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
		  * and those wrapped in [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]]
		  * excluded. This is very similar to [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]
		  * method on any mapping, but takes a sequence of functions, instead of ready components. This makes it
		  * more convenient to call on return values of functions and methods, as the mapping `M` doesn't need
		  * to be assigned to an intermediate `val`. Example:
		  * {{{
		  *     mage.alter(_.spellbook.+, _.familiar.-)
		  * }}}
		  *
		  * This method and [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.alter alter]] are equivalent.
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
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
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
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
		  */
		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
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
		@throws[IllegalArgumentException]("if the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column.")
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

		override def to[Y](implicit lift :Lift[V, Y]) :ComponentLValueSQL[F, M, Y] =
			if (lift == Lift.self[V]) this.asInstanceOf[ComponentLValueSQL[F, M, Y]]
			else LooseComponentConversion(this, lift)

		override def isAnchored = false

		override def anchor(from :F) :ComponentSQL[F, M] = {
			val relation = from.fullTableStack(offset).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
			relation.include(includes).exclude(excludes) \ mapping
		}


		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :LooseComponent[E, M, V] =
			new LooseComponent[E, M, V](
				mapping.asInstanceOf[M[E]], offset + ext.lengthDiff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)(projection.isomorphism[E])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:LooseComponent[E, M, V] =
			new LooseComponent[E, M, V](mapping.asInstanceOf[M[E]], offset + ev.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(projection.isomorphism[E])

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :LooseComponent[E, M, V] =
			new LooseComponent[E, M, V](mapping.asInstanceOf[M[E]], offset + expansion.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(projection.isomorphism[E])
		

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ExpressionVisitor[F, Y]) :Y[GlobalScope, M[F]#Subject] =
			visitor.looseComponent(this)


		override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectMapping[E, M, V] =
			SelectSQL(from, anchor(from))

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, anchor(from))

		override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
				:SelectMapping[P, M] =
			Select(from)[M, V](anchor(from.generalized))


		@throws[UnsupportedOperationException]("if this expression is not a ColumnSQL.")
		override def split(implicit scope :OperationType) :Seq[ColumnSQL[F, GlobalScope, _]] =
			throw new UnsupportedOperationException(
				"Not anchored component " + this + " has an undetermined number of effective columns."
			)

		override def columnCount(implicit spelling :SQLSpelling) :Int =
			throw new UnsupportedOperationException(
				"Not anchored component " + this + " has an undetermined number of effective columns."
			)

		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			throw new UnsupportedOperationException(
				"Not anchored component " + this + " cannot be spelled. This is likely a bug."
			)

		protected override def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
			defaultSpelling(context, params)::Nil


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


		override def toString :String = //not a lazy val as most likely we will never print this (outside of debugging)
			if (includes.isEmpty && excludes.isEmpty)
				"_#" + offset + "." + mapping.mappingName
			else {
				val alterations = includes.view.map("+" + _) ++ excludes.view.map("-" + _)
				alterations.mkString(s"_#$offset.${mapping.mappingName}(", ", ", ")")
			}
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



		/** A conversion applying type promotion to a
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent LooseComponent]]. Together with
		  * the wrapped type itself, anchored [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]]
		  * and its conversion
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.ComponentConversion ComponentConversion]]
		  * they form the close set of direct subtypes of (sealed)
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentLValueSQL ComponentLValueSQL]]
		  * used as the ''l-value'' of of component assignment
		  * [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]], guaranteeing that the underlying
		  * expression is a component, one way or another. This class does not extend
		  * [[net.noresttherein.oldsql.sql.ast.ConversionSQL.PromotionConversion PromotionConversion]] only because
		  * the latter is a class, preventing suitable multiple inheritance by
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn.LooseColumnConversion LooseColumnConversion]].
		  */
		sealed class LooseComponentConversion[F <: RowProduct, M[O] <: BaseMapping[S, O], S, V]
		                                     (override val value :LooseComponent[F, M, S], override val lift :Lift[S, V])
			extends BaseComponentConversion[F, M, V](value, lift)
		{
			override def component :ComponentSQL[F, M] =
				throw new UnsupportedOperationException(
					s"A not anchored LooseComponent $this cannot be updated as its export version is unknown."
				)

			override def to[Y](implicit lift :Lift[V, Y]) :ComponentLValueSQL[F, M, Y] =
				LooseComponentConversion(value, this.lift andThen lift)
		}


		object LooseComponentConversion {
			def apply[F <: RowProduct, M[O] <: BaseMapping[S, O], S, V]
			         (component :LooseComponent[F, M, S], lift :Lift[S, V]) :LooseComponentConversion[F, M, S, V] =
				new LooseComponentConversion(component, lift)

			def unapply[F <: RowProduct, M[O] <: BaseMapping[S, O], S, V](lvalue :ComponentLValueSQL[F, M, V])
					:Opt[(LooseComponent[_ >: F <: RowProduct, M, S], Lift[S, V])] =
				lvalue match {
					case set :LooseComponentConversion[_, M, S, V] @unchecked =>
						Got((set.value, set.lift))
					case _ => Lack
				}

			def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, V])
					:Opt[(LooseComponent[F, m, s], Lift[s, V]) forSome { type m[O] <: BaseMapping[s, O]; type s }] =
				e match {
					case set :LooseComponentConversion[F, MappingOf[Any]#ColumnProjection, Any, V] @unchecked =>
						Got((set.value, set.lift))
					case _ => Lack
				}


			type * = ComponentConversion[_ <: RowProduct, M, _] forSome { type M[O] <: MappingAt[O] }
		}



		trait LooseComponentVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends LooseColumnVisitor[F, Y] {
			def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[X, A], X]
			                  (e :LooseComponent[O, M, X]) :Y[GlobalScope, X]
		}

		trait MatchLooseComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends LooseComponentVisitor[F, Y] {
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
		extends LooseComponent[F, M, V](column, shift, incl, excl) with ColumnLValueSQL[F, M, V]
	{
		def this(column :M[F], shift :Int)(implicit project :IsomorphicProjection[M, V, F]) =
			this(column, shift, Unique.single(column), Unique.empty)

		//this should really use mapping.selectForm, bot it is not a ColumnForm due to possible buffs
		//doesn't really matter though, as this class is a placeholder and the form will never get used.
		override def readForm :ColumnReadForm[V] = mapping.form
		override def form :ColumnForm[V] = mapping.form

		//fixme: should return LooseComponent as it can be empty
		//fixme: Adapter column with subcolumns support
		private def include(components :Iterable[RefinedMapping[_, F]]) :LooseColumn[F, M, V] = this

		private def exclude(components :Iterable[RefinedMapping[_, F]]) :LooseColumn[F, M, V] =
			if (components.isEmpty) this
			else throw new UnsupportedOperationException(
				s"A column expression $this cannot be excluded from itself: $components."
			)

		override def include(components :M[F] => RefinedMapping[_, F]*) :LooseColumn[F, M, V] = this

		override def exclude(components :M[F] => RefinedMapping[_, F]*) :LooseColumn[F, M, V] =
			if (components.isEmpty) this
			else exclude(components.view.map(_(mapping)))

		/** Applies the given function to this column, which must return a column with the same ''export'' version
		  * as this column, and creates an SQL expression representing this column with the returned column included
		  * by default in all permitted database operation types. This is essentially the same as
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn.include include]]`(component)`,
		  * but provides syntax for adding components one by one, which is often preferable when only few components
		  * are include/excluded.
		  */
		override def +(component :M[F] => RefinedMapping[_, F]) :LooseColumn[F, M, V] = this

		/** Throws an [[UnsupportedOperationException]] - a column cannot be excluded from itself. */
		@throws[UnsupportedOperationException]("Always - column expressions cannot exclude their columns.")
		override def -(component :M[F] => RefinedMapping[_, F]) :LooseColumn[F, M, V] =
			throw new UnsupportedOperationException(
				s"A column expression $this cannot be excluded from itself: ${component(mapping)}."
			)

		override def default :LooseColumn[F, M, V] =
			if (includes.isEmpty && excludes.isEmpty) this
			else new LooseColumn(mapping, offset)

		override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
			if (lift == Lift.self[V]) this.asInstanceOf[ColumnLValueSQL[F, M, Y]]
			else LooseColumnConversion(this, lift)

		override def anchor(from :F) :ColumnComponentSQL[F, M, V] = {
			val relation = from.fullTableStack(offset).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
			relation.include(includes) \ mapping
		}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :LooseColumn[E, M, V] =
			new LooseColumn[E, M, V](column.asInstanceOf[M[E]], offset + ext.lengthDiff,
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
		
		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :LooseColumn[E, M, V] =
			new LooseColumn[E, M, V](column.asInstanceOf[M[E]], offset + expansion.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, E]]])(projection.isomorphism
			)


		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ColumnVisitor[F, Y]) :Y[GlobalScope, V] =
			visitor.looseComponent[F, M, V](this)

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

		override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectColumnMapping[E, M, V] =
			SelectSQL(from, anchor(from))

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectColumnMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, anchor(from))

		override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
				:SelectMapping[P, M] =
			Select(from)[M, V](anchor(from.generalized))

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


		/** A conversion applying a type promotion
		  * to a [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn LooseColumn]] instance
		  * in the process of the type unification with another SQL expression of a type automatically converted
		  * by the DBMS to/from the column type. Used in [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]]
		  * to represent setting of the value of a table column.
		  */
		class LooseColumnConversion[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
		                                    (override val value :LooseColumn[F, M, S], lift :Lift[S, V])
			extends LooseComponentConversion[F, M, S, V](value, lift) with BaseColumnComponentConversion[F, M, S, V]
		{
			override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
				LooseColumnConversion(value, this.lift andThen lift)
		}


		object LooseColumnConversion {
			def apply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
			         (column :LooseColumn[F, M, S], lift :Lift[S, V]) :LooseColumnConversion[F, M, S, V] =
				new LooseColumnConversion(column, lift)


			def unapply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V](lvalue :ComponentLValueSQL[F, M, V])
					:Opt[(LooseColumn[F, M, S], Lift[S, V])] =
				lvalue match {
					case set :LooseColumnConversion[F, M, S, V] @unchecked => Got((set.value, set.lift))
					case _ => Lack
				}

			def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, V])
					:Opt[(ColumnComponentSQL[F, m, s], Lift[s, V]) forSome { type m[O] <: ColumnMapping[s, O]; type s }] =
				e match {
					case set :ColumnComponentConversion[F, MappingOf[Any]#ColumnProjection, Any, V] @unchecked =>
						Got((set.value, set.lift))
					case _ => Lack
				}


			type * = ColumnComponentConversion[_ <: RowProduct, M, S, _] forSome { type M[O] <: ColumnMapping[S, O]; type S }
		}



		trait LooseColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def looseComponent[O >: F <: RowProduct, M[A] <: ColumnMapping[V, A], V](e :LooseColumn[O, M, V])
			       :Y[GlobalScope, V]
		}

		type MatchLooseColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = LooseColumnVisitor[F, Y]

		type CaseLooseColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = LooseColumnVisitor[F, Y]
	}






	trait ComponentSQL[-F <: RowProduct, M[A] <: MappingAt[A]]
		extends MappingSQL[F, GlobalScope, M] with ComponentLValueSQL[F, M, M[Unit]#Subject]
	{
		//consider: in quite a few places we create 'copies' of the expression using `table \ this.mapping`
		// this is not very OO as it will convert any custom subclass of this trait with default implementation
		/** The mapping type of the `SQLRelation` to which this component belongs. */
		type Entity[A] <: MappingAt[A]

		override def readForm :SQLReadForm[M[Unit]#Subject] = export.selectForm
		override def component :ComponentSQL[F, M] = this

		/** A pseudo relation adapting this expression for use in ''group by'' clauses
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and [[net.noresttherein.oldsql.sql.By By]].
		  */
		def groupingRelation :Relation[M]

		def entity :Entity[Origin] = origin.mapping
		def relation :Relation[Entity] = origin.relation
		def origin :JoinedRelation[Origin, Entity]

		def extract :MappingExtract[Entity[Origin]#Subject, M[Origin]#Subject, Origin]
		//fixme: currently export can be not a component of table.export if include/exclude is non empty;
		//  exposing it outside may in turn mean someone creates a ComponentSQL using *it* is the non-exported mapping.
		def export :RefinedMapping[M[Unit]#Subject, Origin]

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		def alter(components :M[Origin] => ComponentSelection[_, Origin]*) :ComponentSQL[F, M]

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		def +-(components :M[Origin] => ComponentSelection[_, Origin]*) :ComponentSQL[F, M]

		def include(components :Iterable[RefinedMapping[_, Origin]]) :ComponentSQL[F, M]

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		def exclude(components :Iterable[RefinedMapping[_, Origin]]) :ComponentSQL[F, M]

		def include(components :M[Origin] => RefinedMapping[_, Origin]*) :ComponentSQL[F, M]

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		def exclude(components :M[Origin] => RefinedMapping[_, Origin]*) :ComponentSQL[F, M]

		def +(component :M[Origin] => RefinedMapping[_, Origin]) :ComponentSQL[F, M]

		@throws[IllegalArgumentException]("if the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column.")
		def -(component :M[Origin] => RefinedMapping[_, Origin]) :ComponentSQL[F, M]

		def default :ComponentSQL[F, M]

		def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
				:ComponentSQL[F, project.WithOrigin]

		def \[K <: MappingAt[Origin]]
		     (component :M[Origin] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.Result[F]

		def \[K <: ColumnMapping[_, Origin], X]
		     (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
				:ColumnComponentSQL[F, project.WithOrigin, X]

		override def to[Y](implicit lift :Lift[M[Unit]#Subject, Y]) :ComponentLValueSQL[F, M, Y] =
			if (lift == Lift.self[Y]) this.asInstanceOf[ComponentLValueSQL[F, M, Y]]
			else ComponentConversion(this, lift)

		override def isAnchored = true
		override def anchor(from :F) :ComponentSQL[F, M] = this
//		override def component :ComponentSQL[F, M] = this
			

		/** An expression for the same component, but from the first known
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation JoinedRelation]] of ''from'' clause `P`.
		  * Created expression is equal to this one, except for its `origin`, which is substituted with
		  * `origin.moveTo(offset)`.
		  * @param offset a proof that the first relation listed in ''from'' clause `P` is the same as the `origin`
		  *               relation of this component (or, more precisely, they use the same mapping type `Entity`),
		  *               carrying the offset of the new origin.
		  */
		def moveTo[P <: RowProduct](offset :TableOffset[P, Entity]) :ComponentSQL[P, M]

		/** An expression for the same component, but with the given relation used as its
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.origin origin]].
		  */
		def graft[P <: RowProduct](relation :JoinedRelation[P, Entity]) :ComponentSQL[P, M]


		override def split(implicit op :OperationType) :Seq[ColumnSQL[F, GlobalScope, _]] =
			op.defaultColumns(export).map(origin \ _).toSeq

		override def columnCount(implicit spelling :SQLSpelling) :Int =
			spelling.scope.defaultColumns(export).size

		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			origin.relation.spell(origin, export)(context, params)

		protected override def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
			origin.relation.inline(origin, export)(context, params)


		override def isomorphic(expression :SQLExpression.*) :Boolean = this == expression

		private[oldsql] override def equivalent(expression :SQLExpression.*) :Boolean = expression match {
			case self :AnyRef if self eq this => true
			case component :ComponentSQL[_, _] if component canEqual this =>
				relation == component.relation && mapping == component.mapping
			case _ => false
		}


		override def toString :String = origin.toString + "." + mapping.mappingName
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


		/** A conversion applying type promotion to a
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSLQ]]. Together with the component
		  * expression type itself and its [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent 'loose']]
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.LooseComponentConversion counterparts]],
		  * they form the close set of direct subtypes of
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentLValueSQL ComponentLValueSQL]]
		  * used as the ''l-value'' of of component assignment
		  * [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]], guaranteeing that the underlying
		  * expression is a component, one way or another. This class does not extend
		  * [[net.noresttherein.oldsql.sql.ast.ConversionSQL.PromotionConversion PromotionConversion]] only because
		  * the latter is a class, preventing suitable multiple inheritance by
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnComponentSQL.ColumnComponentConversion ColumnComponentConversion]].
		  */
		sealed class ComponentConversion[-F <: RowProduct, M[O] <: MappingAt[O], V]
		                                (override val value :ComponentSQL[F, M], override val lift :Lift[M[Unit]#Subject, V])
			extends BaseComponentConversion[F, M, V](value, lift)
		{
			override def anchor(from :F) :ComponentConversion[F, M, V] = this

			override def to[Y](implicit lift :Lift[V, Y]) :ComponentLValueSQL[F, M, Y] =
				ComponentConversion(value, this.lift andThen lift)
		}


		object ComponentConversion {
			def apply[F <: RowProduct, M[O] <: MappingAt[O], V]
			         (component :ComponentSQL[F, M], lift :Lift[M[Unit]#Subject, V]) :ComponentConversion[F, M, V] =
				new ComponentConversion(component, lift)

			def unapply[F <: RowProduct, M[O] <: MappingAt[O], V](lvalue :ComponentLValueSQL[F, M, V])
					:Opt[(ComponentSQL[F, M], Lift[M[Unit]#Subject, V]) forSome { type s }] =
				lvalue match {
					case set :ComponentConversion[F, M, V] => Got(set.value, set.lift)
					case _ => Lack
				}

			type * = ComponentConversion[_ <: RowProduct, M, _] forSome { type M[O] <: MappingAt[O] }
		}



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


		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def alter(components :M[O] => ComponentSelection[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] = {
			val newExcludes = components.view.map(_(mapping)).collect { case ExcludedComponent(c) => c }
			val newIncludes = components.view.map(_(mapping)).collect { case IncludedComponent(c) => c }
			include(newIncludes).exclude(newExcludes)
		}

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def +-(components :M[O] => ComponentSelection[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] =
			alter(components :_*)

		override def include(components :Iterable[RefinedMapping[_, O]]) :TypedComponentSQL[F, T, R, M, V, O] =
		 	components.view.collect { case c if !export.contains(c) => c }.toList match {
			    case Nil => origin.include(components) \ mapping
				case comps => throw new IllegalArgumentException(
					s"Cannot include not belonging components $comps with mapping $mapping."
				)
			}

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def exclude(components :Iterable[RefinedMapping[_, O]]) :TypedComponentSQL[F, T, R, M, V, O] =
			components.view.collect { case c if !export.contains(c) => c }.toList match {
				case Nil => origin.exclude(components) \ mapping
				case comps => throw new IllegalArgumentException(
					s"Cannot exclude not belonging components $comps from mapping $mapping."
				)
			}

		override def include(components :M[O] => RefinedMapping[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] =
			include(components.map(_(mapping)))

		@throws[IllegalArgumentException]("if any of the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def exclude(components :M[O] => RefinedMapping[_, O]*) :TypedComponentSQL[F, T, R, M, V, O] =
			exclude(components.map(_(mapping)))

		override def +(component :M[O] => RefinedMapping[_, O]) :TypedComponentSQL[F, T, R, M, V, O] =
			include(Unique.single(component(mapping)))

		@throws[IllegalArgumentException]("if the given components is not a component of this mapping.")
		@throws[UnsupportedOperationException]("if this component is a column.")
		override def -(component :M[O] => RefinedMapping[_, O]) :TypedComponentSQL[F, T, R, M, V, O] =
			exclude(Unique.single(component(mapping)))

		override def default :TypedComponentSQL[F, T, R, M, V, O] = graft(origin.default)


		override def \[K <: MappingAt[O], X](component :K)(implicit project :OriginProjection[K, X])
				:TypedComponentSQL[F, T, R, project.WithOrigin, X, O] =
			origin \ component

		override def \[K <: MappingAt[O]]
		              (component :M[O] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.TypedResult[F, T, R, O] =
			factory(origin, component(mapping))

		override def \[K <: ColumnMapping[_, Origin], X]
		              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
				:TypedColumnComponentSQL[F, T, R, project.WithOrigin, X, O] =
			//we don't need to check if column==entity as a column always has itself as its column and among extractors.
			origin \ column



		override def moveTo[P <: RowProduct](offset :TableOffset[P, T]) :TypedComponentSQL[P, T, R, M, V, P] =
			graft(origin.moveTo(offset))

		override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :TypedComponentSQL[P, T, R, M, V, P] =
			relation.asInstanceOf[RelationSQL[P, T, R, P]] \ mapping.withOrigin[P]

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			graft(origin.expand(base).asInstanceOf[RelationSQL[E, T, R, E]])

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) 
				:TypedComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			graft(origin.expand[E].asInstanceOf[RelationSQL[E, T, R, E]])
		
		override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectMapping[E, M, V] =
			SelectSQL(from, this)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectMapping[B, from.type, M, V] =
			SelectSQL.subselect(from, this)

		override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
				:SelectMapping[P, M] =
			Select(from)[M, V](this)


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
			protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
			                              (visitor :ExpressionVisitor[F, Y]) :Y[GlobalScope, V] =
				visitor.component(this)
		}


		trait ComponentVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends RelationVisitor[F, Y] with ColumnComponentVisitor[F, Y]
		{
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
			             (e :TypedComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		trait MatchComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ComponentVisitor[F, Y] with CaseRelation[F, Y] with CaseColumnComponent[F, Y]
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
		extends ComponentSQL[F, M] with ColumnSQL[F, GlobalScope, V] with ColumnLValueSQL[F, M, V]
	{ //consider: a fishy thing is that this column may be excluded from an operation, breaking(?) ColumnSQL contract
		override def upcast :ColumnSQL[F, GlobalScope, Subject] = this
		override def component :ColumnComponentSQL[F, M, V] = this

		override def extract :ColumnMappingExtract[Entity[Origin]#Subject, V, Origin]
		override def export :ColumnMapping[V, Origin]

		override def readForm :ColumnReadForm[V] = export.selectForm match { //an alternative would be for the
			case select :ColumnReadForm[V @unchecked] => select //always the case unless the column has ExtraSelect
			case _ => throw new UnsupportedOperationException(  //being excluded doesn't affect being a column form
				s"No (Column)ReadForm for selecting column $this: most likely the column $export is not selectable."
			)
		}
		override def form :ColumnForm[V] = export.form

		//fixme: should return ComponentSQL as it might be empty
		override def include(components :Iterable[RefinedMapping[_, Origin]]) :ColumnComponentSQL[F, M, V]
		override def include(components :M[Origin] => RefinedMapping[_, Origin]*) :ColumnComponentSQL[F, M, V]
		override def +(component :M[Origin] => RefinedMapping[_, Origin]) :ColumnComponentSQL[F, M, V]
		override def default :ColumnComponentSQL[F, M, V]


		override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
			if (lift == Lift.self[V]) this.asInstanceOf[ColumnLValueSQL[F, M, Y]]
			else ColumnComponentConversion(this, lift)

		override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)

		override def anchor(from :F) :ColumnComponentSQL[F, M, V] = this

		override def moveTo[P <: RowProduct](offset :TableOffset[P, Entity]) :ColumnComponentSQL[P, M, V]

		override def graft[P <: RowProduct](relation :JoinedRelation[P, Entity]) :ColumnComponentSQL[P, M, V]

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
				:ColumnComponentSQL[E, M, V] =
			expand(base)(expansion.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:ColumnComponentSQL[E, M, V] =
			expand[E]

		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ColumnComponentSQL[E, M, V]

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

		override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
				:SelectMapping[P, M] =
			Select(from.self)[M, V](this)


		override def inParens[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			spelling(this :ColumnSQL[E, LocalScope, V])(context, params)

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


		type * = ColumnComponentSQL[_ <: RowProduct, M, V] forSome { type V; type M[O] <: ColumnMapping[V, O] }


		/** A conversion applying a type promotion
		  * to a [[net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnComponentSQL ColumnComponentSQL]] instance
		  * in the process of the type unification with another SQL expression of a type automatically converted
		  * by the DBMS to/from the column type. Used in [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]]
		  * to represent setting of the value of a table column.
		  */
		class ColumnComponentConversion[-F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
		                               (override val value :ColumnComponentSQL[F, M, S], override val lift :Lift[S, V])
			extends ComponentConversion[F, M, V](value, lift) with BaseColumnComponentConversion[F, M, S, V]
		{
			override def component :ColumnComponentSQL[F, M, S] = value

			override def anchor(from :F) :ColumnComponentConversion[F, M, S, V] = this

			override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
				ColumnComponentConversion(value, this.lift andThen lift)
		}


		object ColumnComponentConversion {
			def apply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
			         (column :ColumnComponentSQL[F, M, S], lift :Lift[S, V]) :ColumnComponentConversion[F, M, S, V] =
				new ColumnComponentConversion(column, lift)


			def unapply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V](lvalue :ComponentLValueSQL[F, M, V])
					:Opt[(ColumnComponentSQL[F, M, S], Lift[S, V])] =
				lvalue match {
					case set :ColumnComponentConversion[F, M, S, V] @unchecked => Got((set.value, set.lift))
					case _ => Lack
				}

			def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, V])
					:Opt[(ColumnComponentSQL[F, m, s], Lift[s, V]) forSome { type m[O] <: ColumnMapping[s, O]; type s }] =
				e match {
					case set :ColumnComponentConversion[F, MappingOf[Any]#ColumnProjection, Any, V] @unchecked =>
						Got((set.value, set.lift))
					case _ => Lack
				}


			type * = ColumnComponentConversion[_ <: RowProduct, M, S, _] forSome { type M[O] <: ColumnMapping[S, O]; type S }
		}
	}






	trait TypedColumnComponentSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R,
	                              M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		extends TypedComponentSQL[F, T, R, M, V, O] with ColumnComponentSQL[F, M, V]
	{
		override def export :ColumnMapping[V, O] = extract.export

		override def include(components :Iterable[RefinedMapping[_, O]]) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			this

		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def exclude(components :Iterable[RefinedMapping[_, O]]) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			if (components.isEmpty) this
			else
				throw new UnsupportedOperationException(
					s"A column expression $this cannot be excluded from itself: $components."
				)

		override def include(components :M[O] => RefinedMapping[_, O]*) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			this

		@throws[UnsupportedOperationException]("if this component is a column and the exclude list is non empty.")
		override def exclude(components :M[O] => RefinedMapping[_, O]*) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			if (components.isEmpty) this
			else exclude(components.view.map(_(mapping)))

		override def +(component :M[O] => RefinedMapping[_, O]) :TypedColumnComponentSQL[F, T, R, M, V, O] = this

		@throws[UnsupportedOperationException]("if this component is a column.")
		override def -(component :M[O] => RefinedMapping[_, O]) :TypedColumnComponentSQL[F, T, R, M, V, O] =
			throw new UnsupportedOperationException(
				s"A column expression $this cannot be excluded from itself: ${component(mapping)}."
			)


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
		
		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) 
				:TypedColumnComponentSQL[E, T, R, M, V, _ >: E <: RowProduct] =
			graft(origin.expand[E])

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, Y]) :Y[GlobalScope, V] =
			visitor.component(this)


		override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectColumnMapping[E, M, V] =
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
		{
			val cast = project[O](column)
			val export = from.export(cast).export
			val relation = if (from.includes.contains(export)) from else from + { _ => export }
			new ProperColumn[F, T, R, project.WithOrigin, V, O](relation, cast)(project.isomorphism)
		}


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



		trait ColumnComponentVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
			             (e :TypedColumnComponentSQL[F, T, R, M, V, O]) :Y[GlobalScope, V]
		}

		type MatchColumnComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentVisitor[F, Y]

		type CaseColumnComponent[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ColumnComponentVisitor[F, Y]
	}






	//consider: unseal and 'seal' with a package private abstract method
	trait JoinedRelation[F <: RowProduct, T[A] <: MappingAt[A]] extends ComponentSQL[F, T] {
		override type Origin = F
		override type Entity[A] = T[A]

		type Self = RelationSQL[F, M, T[F]#Subject, F] forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

		/** Offset of this relation in the clause `F`, counting ''from right to left'' and starting with 0. */
		def offset :Int
		def position :TableOffset[F, T] = new TableOffset(offset)

		override val mapping :T[F]
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
				relation.refString + "#" + offset
			else
				(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(
					relation.refString + "#" + offset + "(", ",", ")"
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

		type Last[M[O] <: MappingAt[O]] = JoinedRelation[RowProduct AndFrom M, M]

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

		type Last[M[O] <: MappingAt[O]] = JoinedTable[RowProduct AndFrom M, M]

		type AnyIn[F <: RowProduct] = JoinedTable[F, T] forSome { type T[O] <: MappingAt[O] }

		type Typed[F <: RowProduct, V] = JoinedTable[F, T] forSome { type T[O] <: RefinedMapping[V, O] }

		type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedTable[O, M] }
	}





	//consider: why do we have includes and excludes here? Some problem with relation equality?
	class RelationSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] protected
	                 (override val relation :Relation[T], override val offset :Int,
	                  override val includes :Unique[RefinedMapping[_, O]],
	                  override val excludes :Unique[RefinedMapping[_, O]])
		extends JoinedRelation[O, T] with TypedComponentSQL[F, T, R, T, R, O]
	{
		override val mapping :T[O] = relation[O]
		/** The export mapping used for the whole relation; note that it might not be `relation.export` due
		  * to includes and excludes on the particular instance of a component.
		  */
		override val export :RefinedMapping[R, O] = {
			if (includes.isEmpty && excludes.isEmpty) relation.export[O]
			else relation.export[O].apply(includes, excludes)
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
			new RelationSQL[E, T, R, E](relation, offset + expansion.lengthDiff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			) //E is incorrect, but we lose this information anyway

		override def expand[U <: O, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:RelationSQL[E, T, R, _ >: E <: RowProduct] =
			new RelationSQL[E, T, R, E](relation, offset + ev.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			) //E is incorrect, but we lose this information anyway

		override def expand[E <: RowProduct](implicit expansion :O ExpandedBy E)
				:RelationSQL[E, T, R, _ >: E <: RowProduct] =
			new RelationSQL[E, T, R, E](relation, offset + expansion.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)

		override def asIn[J[+L <: O] <: L Expanded T forSome { type T[A] <: MappingAt[A] }] :RelationSQL[J[F], T, R, J[O]] =
			new RelationSQL[J[F], T, R, J[O]](relation, offset + 1,
				includes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]]
			)

		override def asIn[E <: RowProduct](implicit expansion :O PrefixOf E) :RelationSQL[E, T, R, E] =
			new RelationSQL(relation, offset + expansion.lengthDiff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)


		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ExpressionVisitor[O, Y]) :Y[GlobalScope, R] =
			visitor.relation(toRelationSQL)


		//the following methods are overriden due to a winder bound O inherited from invariant JoinedTable

		override def topSelectFrom[E <: O with GroundFrom](from :E) :TopSelectMapping[E, T, R] =
			SelectSQL(from, toRelationSQL)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectMapping[B, from.type, T, R] =
			SelectSQL.subselect(from, toRelationSQL)

		override def paramSelectFrom[P <: Chain, G <: O](from :TopFrom { type Generalized <: G; type Params = P })
				:SelectMapping[P, T] =
			Select(from)[T, R](toRelationSQL)


		override def :=[P <: RowProduct, Y, U](rvalue :SQLExpression[P, GlobalScope, Y])
		                                      (implicit promote :SQLTypeUnification[R, Y, U])
				:ComponentSetter[O, P, U] =
			ComponentSetter[O, T, R, P, Y, U](this, rvalue)(promote)

		override def :=[C <: MappingOf[R], E <: RowProduct, P <: RowProduct]
		               (component :C)(implicit cast :C <:< RefinedMapping[R, P],
		               subtype :SQLExpression[P, GlobalScope, R] <:< SQLExpression[E, GlobalScope, R],
		               project :OriginProjection[C, R], offset :TableCount[P, _ <: Numeral]) :ComponentSetter[O, E, R] =
			this := subtype(LooseComponent(component))

		override def :=[X, U](that :X)(implicit promote :SQLTypeUnification[R, X, U], form :SQLForm[X])
				:ComponentSetter[O, RowProduct, U] =
			this := SQLTerm(that)

		override def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[R, Y, U], form :SQLForm[Y])
				:ComponentSetter[O, RowProduct, U] =
			this := SQLParameter(rvalue)


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



		trait RelationVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends TableVisitor[F, Y] {
			def relation[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
			            (e :RelationSQL[F, T, R, O]) :Y[GlobalScope, R]
		}

		type MatchRelation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = RelationVisitor[F, Y]

		trait CaseRelation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends RelationVisitor[F, Y] {
			override def table[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
			                  (e :TableSQL[F, T, R, O]) :Y[GlobalScope, R] =
				relation(e)
		}
	}






	class TableSQL[-F <: RowProduct, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct] protected
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
			new TableSQL[E, T, R, E](relation, offset + expansion.lengthDiff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			) //E is incorrect, but we lose this information anyway

		override def expand[U <: O, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:TableSQL[E, T, R, _ >: E <: RowProduct] =
			new TableSQL[E, T, R, E](relation, offset + ev.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			) //E is incorrect, but we lose this information anyway

		override def expand[E <: RowProduct](implicit expansion :O ExpandedBy E) :TableSQL[E, T, R, _ >: E <: RowProduct] =
			new TableSQL[E, T, R, E](relation, offset + expansion.length,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)
		
		override def asIn[J[+L <: O] <: L Expanded T forSome { type T[A] <: MappingAt[A] }] :TableSQL[J[F], T, R, J[O]] =
			new TableSQL[J[F], T, R, J[O]](relation, offset + 1,
				includes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, J[O]]]]
			)

		override def asIn[E <: RowProduct](implicit expansion :O PrefixOf E) :TableSQL[E, T, R, E] =
			new TableSQL(relation, offset + expansion.lengthDiff,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)


		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ExpressionVisitor[O, Y]) :Y[GlobalScope, R] =
			visitor.relation(toTableSQL)

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



		trait TableVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def table[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct](e :TableSQL[F, T, R, O]) :Y[GlobalScope, R]
		}

		type MatchTable[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = TableVisitor[F, Y]

		type CaseTable[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = TableVisitor[F, Y]
	}






	/** An expression which can occur on the left side in the DSL for a ''set'' clause of an SQL ''update'',
	  * representing a table component which value is set by an SQL ''insert'' or ''update'' statement.
	  * It is a sealed trait with four distinct direct 'case' subtypes:
	  *   1. [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]], representing
	  *      a direct use of a component, when the type of the ''r-value'' is the same as the subject type
	  *      of the component;
	  *   1. [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL.ComponentConversion ComponentConversion]],
	  *      which is a [[net.noresttherein.oldsql.sql.ast.ConversionSQL ConversionSQL]] subtype
	  *      [[net.noresttherein.oldsql.sql.SQLExpression.Lift promoting]] the subject type of a converted
	  *      `ComponentSQL` instance as a part of type unification with a ''r-value'' expression, originally
	  *      of some different type;
	  *   1. [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent LooseComponent]], an unanchored
	  *      (not linked to the owning table) component expression;
	  *   1. [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent.LooseComponentConversion LooseComponentConversion]],
	  *      which promotes the type of the latter in the same manner as former `ComponentConversion`.
	  *
	  * Additionally, there is a subtype for columns
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnLValueSQL ColumnLValueSQL]],
	  * with four direct subtypes mirroring the above.
	  *
	  * This type is used as part of [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]],
	  * when creating DML ''update'' and ''insert'' statements as well as internally in preparation of an entity
	  * before its saving. Note that, while representing a component, it is not
	  * a [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]] subtype, as its value type may be different
	  * than the mapping's `Subject`.
	  * @tparam F a ''from'' clause - list of relations/tables which provide columns used in this expression.
	  * @tparam M the mapping type of the updated component, accepting the `Origin` type as a type argument.
	  *           It is the type parameter of the underlying `ComponentSQL`.
	  * @tparam V the subject type of mapping `M` or a type to which it is promoted when unifying it
	  *           with another expression.
	  */ //just rename it to LValueSQL
	sealed trait ComponentLValueSQL[-F <: RowProduct, M[O] <: MappingAt[O], V]
		extends SQLExpression[F, GlobalScope, V]
	{
		type Origin >: F <: RowProduct

		/** The underlying component expression (possibly this instance). */
		@throws[UnsupportedOperationException]("if the lvalue is not anchored (it is a LooseComponent).")
		def component :ComponentSQL[F, M]
//		def component :ComponentSQL[F, M]
		/** The mapping of the set component. */
		def mapping :M[Origin]


		override def isGlobal = true
		override def asGlobal :Option[GlobalSQL[F, V]] = Some(this)

		override def anchor(from :F) :ComponentLValueSQL[F, M, V]

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ComponentLValueSQL[E, M, V] =
			expand[E]

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) 
				:ComponentLValueSQL[E, M, V] =
			expand[E]
		
		def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ComponentLValueSQL[E, M, V]
		
		
		/** Creates an assignment object [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]]
		  * with the given expression as its ''r-value'' and this instance as its ''l-value'', used to set the value
		  * of this component as a part of an SQL ''insert'' or ''update''.
		  */
		def :=[R <: RowProduct, Y, U](rvalue :SQLExpression[R, GlobalScope, Y])
		                             (implicit promote :SQLTypeUnification[V, Y, U]) :ComponentSetter[F, R, U] =
			ComponentSetter(to(promote.left), promote.right(denullify(rvalue)))

		def :=[C <: MappingOf[V], R <: RowProduct, O <: RowProduct] //R and O are separate as R may be instantiated early from the expected type
		      (component :C)(implicit cast :C <:< RefinedMapping[V, O], //todo: SQLTypeUnification
		                     subtype :SQLExpression[O, GlobalScope, V] <:< SQLExpression[R, GlobalScope, V],
		                     project :OriginProjection[C, V], offset :TableCount[O, _ <: Numeral])
				:ComponentSetter[F, R, V] =
			this := subtype(LooseComponent(component))

		def :=[X, U](that :X)(implicit promote :SQLTypeUnification[V, X, U], form :SQLForm[X])
				:ComponentSetter[F, RowProduct, U] =
			this := SQLTerm(that)

		//todo: we *could* get rid of the form param, as we have one from the mapping. But we don't know if insert or update
		def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[V, Y, U], form :SQLForm[Y])
				:ComponentSetter[F, RowProduct, U] =
			this := SQLParameter(rvalue)

		override def to[Y](implicit lift :Lift[V, Y]) :ComponentLValueSQL[F, M, Y] =
			throw new UnsupportedOperationException("This method should have been overriden by the subclass. This is a bug.")

	}


	object ComponentLValueSQL {
		def apply[F <: RowProduct, M[O] <: MappingAt[O], V]
		         (component :ComponentSQL[F, M], lift :Lift[M[Unit]#Subject, V]) :ComponentConversion[F, M, V] =
			new ComponentConversion(component, lift)

		def apply[F <: RowProduct, M[O] <: BaseMapping[S, O], S, V]
		         (component :LooseComponent[F, M, S], lift :Lift[S, V]) :LooseComponentConversion[F, M, S, V] =
			new LooseComponentConversion(component, lift)


		type * = ComponentLValueSQL[_ <: RowProduct, M, _] forSome { type M[O] <: MappingAt[O] }


		sealed class BaseComponentConversion[-F <: RowProduct, M[O] <: MappingAt[O], V]
		             (override val value :ComponentLValueSQL[F, M, M[Unit]#Subject], val lift :Lift[M[Unit]#Subject, V])
			extends ConversionSQL[F, GlobalScope, M[Unit]#Subject, V] with ComponentLValueSQL[F, M, V]
		{
			override type Origin = value.Origin
			override def mapping :M[Origin] = value.mapping
			override def component :ComponentSQL[F, M] = value.component

			override def convert(x :M[Unit]#Subject) :V = lift(x)
			override def groundValue :Opt[V] = Lack

			override def anchor(from :F) :ComponentLValueSQL[F, M, V] = value.anchor(from) match {
				case same if same eq value => this
				case other => other.to(lift)
			}

			override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ComponentLValueSQL[E, M, V] =
				value.expand[E].to(lift)	

			protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope]
			                              (e :SQLExpression[E, C, M[Unit]#Subject]) :SQLExpression[E, C, V] =
				e match {
					case c :LooseComponent[_, MappingOf[M[Unit]#Subject]#TypedProjection, M[Unit]#Subject] @unchecked =>
						new LooseComponentConversion(c, lift)
					case c :ComponentSQL[F, MappingOf[M[Unit]#Subject]#Projection @unchecked] =>
						new ComponentConversion[E, MappingOf[M[Unit]#Subject]#Projection, V](c, lift)
					case e =>
						PromotionConversion(e, lift)
				}

		}
	}





	/** An expression which can occur on the left side in the DSL for a ''set'' clause of an SQL ''update'',
	  * representing an updated column of a table. It is a sealed trait with four distinct direct 'case' subtypes:
	  *   1. [[net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnComponentSQL ColumnComponentSQL]], representing
	  *      a direct use of a column, when the type of the ''r-value'' is the same as the subject type
	  *      of the column;
	  *   1. [[net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnComponentSQL.ColumnComponentConversion ColumnComponentConversion]],
	  *      which is a [[net.noresttherein.oldsql.sql.ast.ConversionSQL.ColumnConversionSQL ColumnConversionSQL]]
	  *      subtype [[net.noresttherein.oldsql.sql.SQLExpression.Lift promoting]] the subject type of a converted
	  *      `ColumnComponentSQL` instance as a part of type unification with a ''r-value'' expression, originally
	  *      of some different type;
	  *   1. [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn LooseColumn]], an unanchored (not linked
	  *      to the owning table) table column expression;
	  *   1. [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseColumn.LooseColumnConversion LooseColumnConversion]],
	  *      which promotes the type of the latter expression in the same manner as former `ColumnComponentConversion`.
	  *
	  * This type is used as part of [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]],
	  * when creating DML ''update'' and ''insert'' statements as well as internally in preparation of an entity
	  * before its saving.
	  * @tparam F a ''from'' clause - list of relations/tables which provide columns used in this expression.
	  * @tparam M the mapping type of the updated component, accepting the `Origin` type as a type argument.
	  *           It is the type parameter of the underlying `ColumnComponentSQL`.
	  * @tparam V the subject type of column `M` or a type to which it is promoted when unifying it
	  *           with another expression.
	  */
	sealed trait ColumnLValueSQL[-F <: RowProduct, M[O] <: ColumnMapping[_, O], V]
		extends ColumnSQL[F, GlobalScope, V] with ComponentLValueSQL[F, M, V]
	{
		override def component :ColumnComponentSQL[F, M, _]
		def form :ColumnForm[V]
		override def asGlobal :Option[ColumnSQL[F, GlobalScope, V]] = Some(this)

		override def anchor(from :F) :ColumnLValueSQL[F, M, V]


		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :PartOf[U, E]) :ColumnLValueSQL[E, M, V] =
			expand[E]	

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:ColumnLValueSQL[E, M, V] =
			expand[E]

		def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ColumnLValueSQL[E, M, V]
			
//		override def :=[R <: RowProduct, Y, U](rvalue :SQLExpression[R, GlobalScope, Y]) //overriden for correct overloading
//		                                      (implicit promote :SQLTypeUnification[V, Y, U]) :ComponentSetter[F, R, U] =
//			ComponentSetter[F, M, R, U](to(promote.left), promote.right(rvalue))

		/** Creates an assignment object [[net.noresttherein.oldsql.sql.ColumnSetter ColumnSetter]]
		  * with the given expression as its ''r-value'' and this instance as its ''l-value'', used to set the value
		  * of this component as a part of an SQL ''insert'' or ''update''.
		  */
		def :=[R <: RowProduct, Y, U](rvalue :ColumnSQL[R, GlobalScope, Y])
		                             (implicit promote :SQLTypeUnification[V, Y, U]) :ColumnSetter[F, R, U] =
			ColumnSetter[F, M, R, U](to(promote.left), promote.right(denullify(rvalue)))

		def :=[E <: F, O <: RowProduct] //E and O are separate as E may be instantiated early from the expected type
		      (component :ColumnMapping[V, O]) //todo: variants for columns and literals for other assignment methods
		      (implicit subtype :ColumnSQL[O, GlobalScope, V] <:< ColumnSQL[E, GlobalScope, V],
		                offset :TableCount[O, _ <: Numeral])
				:ColumnSetter[F, E, V] =
			this := subtype(LooseColumn(component))

		override def :=[X, U](that :X)(implicit promote :SQLTypeUnification[V, X, U], form :SQLForm[X])
				:ComponentSetter[F, RowProduct, U] =
			to(promote.left) := SQLTerm(promote.right(that))(promote.left(this.form))

		def +=[R <: RowProduct, Y, U]
		      (rvalue :ColumnSQL[R, GlobalScope, Y])
		      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
				:ColumnSetter[F, R, U] =
			to(promote.left) := expand[R] + denullify(rvalue)

		def -=[R <: RowProduct, Y, U]
		      (rvalue :ColumnSQL[R, GlobalScope, Y])
		      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
				:ColumnSetter[F, R, U] =
			to(promote.left) := expand[R] - denullify(rvalue)

		def *=[R <: RowProduct, Y, U]
		      (rvalue :ColumnSQL[R, GlobalScope, Y])
		      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
				:ColumnSetter[F, R, U] =
			to(promote.left) := expand[R] * denullify(rvalue)

		def /=[R <: RowProduct, Y, U]
		      (rvalue :ColumnSQL[R, GlobalScope, Y])
		      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
				:ColumnSetter[F, R, U] =
			to(promote.left) := expand[R] / denullify(rvalue)

		def %=[R <: RowProduct, Y, U]
		      (rvalue :ColumnSQL[R, GlobalScope, Y])
		      (implicit promote :SQLTypeUnification[V, Y, U], math :SQLNumber[U], expansion :F ExpandedBy R)
				:ColumnSetter[F, R, U] =
			to(promote.left) := expand[R] % denullify(rvalue)


		def &&=[R <: RowProduct, Y](rvalue :ColumnSQL[R, GlobalScope, Y])
		                           (implicit promote :SQLTypeUnification[V, Y, Boolean], expansion :F ExpandedBy R)
				:ColumnSetter[F, R, Boolean] =
			to(promote.left) := expand[R].to(promote.left) && promote.right(rvalue)

		def ||=[R <: RowProduct, Y](rvalue :ColumnSQL[R, GlobalScope, Y])
		                           (implicit promote :SQLTypeUnification[V, Y, Boolean], expansion :F ExpandedBy R)
				:ColumnSetter[F, R, Boolean] =
			to(promote.left) := expand[R].to(promote.left) || promote.right(rvalue)

//		def ^= [R <: RowProduct, Y](rvalue :ColumnSQL[R, GlobalScope, Y])
//		                  (implicit promote :SQLTypeUnification[V, Y, Boolean]) :ColumnSetter[F, R, Boolean] =
//			to(promote.left) := to(promote.left) ^ promote.right(rvalue)


		def ++=[R <: RowProduct, Y](rvalue :ColumnSQL[R, GlobalScope, Y])
		                           (implicit promote :SQLTypeUnification[V, Y, String], expansion :F ExpandedBy R)
				:ColumnSetter[F, R, String] =
			to(promote.left) := expand[R].to(promote.left) ++ promote.right(denullify(rvalue))

//		override def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[V, Y, U], form :SQLForm[Y])
//				:ComponentSetter[F, RowProduct, U] =  //overriden for correct overloading
//			this := SQLParameter(rvalue)

		def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[V, Y, U], form :ColumnForm[Y])
				:ColumnSetter[F, RowProduct, U] =
			this := SQLParameter(rvalue)


		override def to[Y](implicit lift :Lift[V, Y]) :ColumnLValueSQL[F, M, Y] =
			throw new UnsupportedOperationException("This method should have been overriden by the subclass. This is a bug.")

	}


	object ColumnLValueSQL {
		def apply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
		         (column :ColumnComponentSQL[F, M, S], lift :Lift[S, V]) :ColumnComponentConversion[F, M, S, V] =
			new ColumnComponentConversion(column, lift)

		def apply[F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
		         (column :LooseColumn[F, M, S], lift :Lift[S, V]) :LooseColumnConversion[F, M, S, V] =
			new LooseColumnConversion(column, lift)


		type * = ColumnLValueSQL[_ <: RowProduct, M, _] forSome { type M[O] <: ColumnMapping[_, O] }


		sealed trait BaseColumnComponentConversion[-F <: RowProduct, M[O] <: ColumnMapping[S, O], S, V]
			extends BaseComponentConversion[F, M, V] with ColumnConversionSQL[F, GlobalScope, S, V]
			   with ColumnLValueSQL[F, M, V]
		{
			override val value :ColumnLValueSQL[F, M, S] = null
			override def component :ColumnComponentSQL[F, M, _] = value.component
			override def form :ColumnForm[V] = lift(value.form)

			override def anchor(from :F) :ColumnLValueSQL[F, M, V] = value.anchor(from) match {
				case same if same eq value => this
				case other => other.to(lift)
			}

			override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ColumnLValueSQL[E, M, V] =
				value.expand[E].to(lift)	

			protected override def reapply[E <: RowProduct, C >: LocalScope <: GlobalScope](e :ColumnSQL[E, C, S])
					:ColumnSQL[E, C, V] =
				e match {
					case c :LooseColumn[E, M, S] @unchecked =>
						new LooseColumnConversion[E, M, S, V](c, lift)
					case c :ColumnComponentSQL[F, M, S]  @unchecked =>
						new ColumnComponentConversion[F, M, S, V](c, lift)
					case e =>
						ColumnPromotionConversion(e, lift)
				}
		}
	}






	trait MappingColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnComponentVisitor[F, Y] with LooseColumnVisitor[F, Y]

	trait MappingVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MappingColumnVisitor[F, Y] with ComponentVisitor[F, Y] with LooseComponentVisitor[F, Y]
	{
		def mapping[S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]](e :MappingSQL[F, S, M]) :Y[S, M[Unit]#Subject]
	}

	trait MatchMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MappingVisitor[F, Y]
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



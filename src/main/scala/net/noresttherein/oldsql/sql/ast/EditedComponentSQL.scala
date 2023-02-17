package net.noresttherein.oldsql.sql.ast

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.exceptions.{Bug, IllegalExpressionException}
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.slang.castTypeParam
import net.noresttherein.oldsql.sql.{:=, ColumnSetter, ColumnSQL, RowProduct, RowShape, SQLExpression, WithClause}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, CaseAnyColumn, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor.Result
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, CaseAnyExpression, ConvertibleSQL, ConvertingTemplate, Grouped, Single, SingleRowSQLTemplate, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.AdaptedSQL.AbstractAdaptedSQL
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.EditedComponentSQL.{AnyEditedComponentVisitor, SpecificEditedComponentVisitor}
import net.noresttherein.oldsql.sql.ast.EditedLooseComponent.{AnyEditedLooseComponentVisitor, SpecificEditedLooseComponentVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.ConvertedMappingSQL
import net.noresttherein.oldsql.sql.mechanics.{Reform, SpelledSQL, SQLConversion, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLScribe.AbstractSQLScribe






/** A component expression (more specifically, some subtype of
  * [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]]) in which certain columns
  * are replaced with arbitrary column expressions of compatible types, grounded in the same clause `F`.
  * This allows overriding values of table columns with presets which should be used in the assembly instead.
  * Two independent implementations exist, one for [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]
  * and one for [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]:
  * [[net.noresttherein.oldsql.sql.ast.EditedLooseComponent EditedLooseComponent]] and
  * [[net.noresttherein.oldsql.sql.ast.EditedComponentSQL EditedComponentSQL]].
  *
  * The substituted columns are treated the same way as all other columns of
  * `this.`[[net.noresttherein.oldsql.sql.ast.EditedLValueSQL.component component]]: in particular,
  * they are not automatically included if they are not default for a particular
  * [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope SpellingScope]]. Reforming of this expression
  * can also result in a substituted column becoming excluded, unless `component` is not susceptible to reforming.
  *
  * Method [[net.noresttherein.oldsql.sql.ast.LValueSQL.substitute substitute]]
  * of [[net.noresttherein.oldsql.sql.ast.LValueSQL LValueSQL]] however automatically
  * adds all substituted columns
  * to the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.includes include]] list
  * of the edited component.
  */ //technically, this doesn't have to be Single
trait EditedLValueSQL[-F <: RowProduct, M[A] <: BaseMapping[V, A], V]
	extends MappingSQL[F, Single, M, V] with CompositeSQL[F, Single, V] with SelectableSQL[F, Single, V]
{ self =>
	override type Origin >: F <: RowProduct

	/** This component's [[net.noresttherein.oldsql.sql.ast.MappingSQL.Origin Origin]] type truncated
	  * to its first relation (from the right).
	  */
	type FromLast <: RowProduct

	/** The modified component expression. */
	val component :LValueSQL[F, M, V] { type Origin = self.Origin; type FromLast = self.FromLast }
	//requires that component is a parameter!
	override val mapping       :M[Origin] = component.mapping
	override val export        :TypedMapping[V, Origin] = component.export
	override lazy val anchored :TypedMapping[V, Origin] = component.anchored

	/** Column - expression pairs defining the replacement expressions,
	  * where the [[net.noresttherein.oldsql.sql.ColumnSetter.lvalue lvalue]] of each setter is a
	  * [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL ColumnComponentSQL]]
	  * (or a [[net.noresttherein.oldsql.sql.ast.LooseColumn LooseColumn]])
	  * with a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] for a column of
	  * [[net.noresttherein.oldsql.sql.ast.EditedComponentSQL.component component]]`.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]],
	  * and the [[net.noresttherein.oldsql.sql.ColumnSetter.rvalue rvalue]] is the expression which should be formatted
	  * in its place.
	  */
	val substitutes :Seq[ColumnSetter[FromLast, Origin]]

	/** Maps export columns of [[net.noresttherein.oldsql.sql.ast.EditedLValueSQL.component component]]`.mapping`
	  * to their substitutes, if present.
	  * @return (pseudocode):
	  *         `substitutes.map { setter => Assoc(mapping.export(setter.lvalue.mapping), setter) } to NaturalMap`.
	  */
	lazy val nominalSubstitutes
			:NaturalMap[MappingAt[Origin]#Column, ({ type T[X] = ColumnSetter[FromLast, F] { type Subject = X } })#T] =
		substituteMap(component.mapping)

	private[ast] def substituteMap(root :MappingAt[Origin])
			:NaturalMap[MappingAt[Origin]#Column, ({ type T[X] = ColumnSetter[FromLast, F] { type Subject = X } })#T] =
		substitutes.map { setter =>
			type Setter[A] = ColumnSetter[FromLast, F] { type Subject = A }
			val lvalue = root.export(setter.lvalue.mapping.withOrigin[Origin])
			Assoc[MappingAt[Origin]#Column, Setter, setter.Subject](lvalue, setter)
		} to NaturalMap

	protected override lazy val parts :Seq[SQLExpression[F, Single, _]] =
		substitutes.view.map(_.rvalue).prepended(component) to Seq

	override def selectForm :SQLReadForm[V] = component.selectForm

	override def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
			:MappingSQL[F, Single, project.WithOrigin, X] =
	{
		val unedited = this.component \ component
		val last = unedited.asLast
		unedited.substitute(substitutes.collect {
			case setter if unedited.export.contains(setter.lvalue.export.withOrigin[Origin]) =>
				setter.promote(last \ setter.lvalue.mapping.withOrigin[last.Origin]) := setter.rvalue
		})
	}

	override def \[K <: ColumnAt[Origin], X]
	              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
			:ColumnMappingSQL[F, Single, project.WithOrigin, X] =
	{
		val unedited = this.component \ column
		substitutes.find(setter => unedited.export == setter.lvalue.export) match {
			case Some(setter :(ColumnSetter[FromLast, F]
			                   { type Component[O] = project.WithOrigin[O]; type Subject = X }) @unchecked
			) =>
				EditedColumnSQL(setter)
			case _ => unedited
		}
	}

	/** Creates an SQL expression which substitutes certain columns of this component for arbitrary SQL expressions
	  * of compatible type. The [[net.noresttherein.oldsql.sql.ColumnSetter.lvalue l-values]] of the pairs must
	  * be instances of [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL ColumnLValueSQL]] for column mappings
	  * of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]] (including
	  * columns of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]),
	  * while the [[net.noresttherein.oldsql.sql.ColumnSetter.rvalue r-values]] must be expressions based on
	  * the same clause as this expression (or its subtype). The columns will be automatically included in the result
	  * using this instance's [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
	  * method.
	  *
	  * The arguments of this method are split into the first substitution and the rest in order to avoid erasure
	  * conflicts. All arguments are functions accepting the nominal mapping of this expression and pairing
	  * one of its columns with an arbitrary [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] expression.
	  * They work in a similar fashion to [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter alter]] family
	  * of methods of `ComponentSQL`, but instead of just selecting a column/component, they assign to it another
	  * expression. This can be done using method [[net.noresttherein.oldsql.sql.ast.LValueSQL.:= :=]]
	  * available on the argument through an implicit conversion:
	  * {{{
	  *     familiar.substitute(_.name := "Boo", _.species := "Hamster")
	  * }}}
	  */ //MappingSQL as the result type is not ideal as we don't have a CanSelect for it.
	def substitute[E <: F](column :M[FromLast] => ColumnSetter[FromLast, E],
	                       columns :M[FromLast] => ColumnSetter[FromLast, E]*) :MappingSQL[E, Single, M, V] =
	{
		val arg = mapping.castParam[FromLast]
		substitute(columns.view.map(_(arg)).prepended(column(arg)) to ArraySeq)
	}

	/** Creates an SQL expression which substitutes certain columns of this component for arbitrary SQL expressions
	  * of compatible type. The [[net.noresttherein.oldsql.sql.ColumnSetter.lvalue l-values]] of the pairs must
	  * be instances of [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL ColumnLValueSQL]] for column mappings
	  * of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.export export]] (including
	  * columns of `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]),
	  * while the [[net.noresttherein.oldsql.sql.ColumnSetter.rvalue r-values]] must be expressions based on
	  * the same clause as this expression (or its subtype). The columns will be automatically included in the result
	  * using this instance's [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.include include]]
	  * method.
	  */
	def substitute[E <: F](columns :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V] =
		component.substitute(substitutes ++ columns)

	def apply[E <: F](substitute :M[FromLast] => ColumnSetter[FromLast, E],
	                  substitutes :M[FromLast] => ColumnSetter[FromLast, E]*) :MappingSQL[E, Single, M, V] =
	{
		val arg = mapping.castParam[FromLast]
		this.substitute(substitutes.view.map(_(arg)).prepended(substitute(arg)) to ArraySeq)
	}

	def apply[E <: F](substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, M, V] =
		substitute(substitutes)

	override def isDefault :Boolean = component.isDefault

//	protected def reapply(comp :TypedComponent)

//	override def default :MappingSQL[F, GlobalScope, M, S] = component.default substitute
//
//	override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]]) :MappingSQL[F, GlobalScope, M, S] = ???
//
//	override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]]) :MappingSQL[F, GlobalScope, M, S] = ???
	protected override def convert[Y](conversion :SQLConversion[V, Y]) :MappingSQL[F, Single, M, Y] =
		ConvertedMappingSQL(this, conversion)

	override def anchor(from :F) :EditedLValueSQL[F, M, V] =
		if (isAnchored(from)) this else rephrased(SQLScribe.anchor(from))

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :EditedLValueSQL[E, M, V] =
		rephrased(SQLScribe.rebase(base))

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
			:EditedLValueSQL[E, M, V] =
		rephrased(SQLScribe.expand(base))

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, Single, V] = {
		mapper(component) match {
			case lvalue :LValueSQL[E, M, V] @unchecked if lvalue.mapping isomorphic component.mapping =>
				lvalue.substitute(rephraseSubstitutes(lvalue, mapper))
			case edited :EditedLValueSQL[E, M, V] @unchecked if edited.component.mapping isomorphic component.mapping =>
				edited.substitute(rephraseSubstitutes(edited.component, mapper))
			case other => other
//				throw new IllegalExpressionException(
//					"Cannot transcribe " + this + " with " + mapper + ": returned expression " + other +
//						" is not a ComponentSQL."
//				)
		}
	}

	private def rephrased[E <: RowProduct](mapper :SQLScribe[F, E]) :EditedLValueSQL[E, M, V] =
		rephrase(mapper) match { //we require EditedLValueSQL because it has the CanSelect type class
			case mapping :EditedLValueSQL[E, M, V] @unchecked if mapping.mapping identical this.mapping =>
				mapping
			case other =>
				throw new IllegalExpressionException(
					"Cannot rephrase " + this + " with " + mapper + ": the transcribed component " + other +
					" (" + other.getClass + ") is not a MappingSQL for an identical mapping to " + this.mapping + "."
				)
		}

	private[ast] def rephraseSubstitutes[E <: RowProduct](lvalue :LValueSQL[E, M, V], rvalue :SQLScribe[F, E])
			:Seq[ColumnSetter[lvalue.FromLast, E]] =
	{
		val rephraseLValue = new SetterLValueScribe[E, M, V, lvalue.FromLast](lvalue)
		substitutes.map { setter =>
			try { rephraseLValue(setter.lvalue) := rvalue(setter.rvalue) } catch {
				case ex :IllegalExpressionException =>
					throw ex.addInfo(
						"Cannot rephrase " + this + " with " + rvalue +
							"; could not convert one of substitutions: " + ex.getMessage
					)
			}
		}
	}

	/** Converts the lvalues of individual setters from `component.FromLast` to `comp.FromLast`, where `comp`
	  * is result of some rephrasing/rebasing of `this.component` to clause `E`.
	  */
	private[ast] class SetterLValueScribe[E <: RowProduct, C[A] <: BaseMapping[X, A], X, L <: RowProduct]
	                                     (comp :LValueSQL[E, C, X] { type FromLast = L })
		extends CaseAnyExpression[FromLast, Result[L]#E] with CaseAnyColumn[FromLast, Result[L]#C]
		   with AbstractSQLScribe[FromLast, L]
	{
		private[this] val oldLeft = self.component.asLast
		private[this] val newLeft = comp.asLast

		def apply[A[Z] <: BaseColumn[_, Z], V](e :ColumnLValueSQL[FromLast, A, V]) :ColumnLValueSQL[L, A, V] =
			apply(e :ColumnSQL[FromLast, Single, V]) match {
				case lvalue :ColumnLValueSQL[L @unchecked, A @unchecked, V @unchecked]
					if lvalue.mapping identical oldLeft.mapping =>
					lvalue
				case lvalue =>
					throw new IllegalExpressionException(
						"Column l-value " + e + " of " + self.component +
							" after rephrasing the component to " + comp +
							" is no longer a ColumnLValueSQL for an identical mapping: " + lvalue
							+ " (" + lvalue.getClass + "). This is a bug."
					)
			}

		override def columnComponent[O1 >: FromLast <: RowProduct, T1[Z] <: BaseMapping[R1, Z], R1,
		                             C1[Z] <: BaseColumn[X1, Z], X1, FL <: RowProduct]
		                            (e :TypedColumnComponentSQL[O1, T1, R1, C1, X1, FL])
				:ColumnLValueSQL[L, MappingOf[X1]#TypedColumnProjection, X1] =
		{
			val counterpart = newLeft.mapping.counterpart(oldLeft.mapping, e.mapping.withOrigin[oldLeft.Origin])
			newLeft \ counterpart
		}

		override def looseColumn[O1 >: FromLast <: RowProduct, C1[A] <: BaseColumn[X1, A], X1]
		                        (e :LooseColumn[O1, C1, X1]) :ColumnLValueSQL[L, MappingOf[X1]#TypedColumnProjection, X1] =
		{
			val counterpart = newLeft.mapping.counterpart(oldLeft.mapping, e.mapping.withOrigin[oldLeft.Origin])
			newLeft \ counterpart
		}

		override def expression[A >: Grouped <: Single, V](e :SQLExpression[FromLast, A, V]) =
			unhandled(e) //doesn't happen. as we call this only for ColumnLValueSQL

		override def column[A >: Grouped <: Single, V](e :ColumnSQL[FromLast, A, V]) =
			unhandled(e) //doesn't happen. as we call this only for ColumnLValueSQL
	}



	override def outerWithClause :WithClause = (component.outerWithClause /: substitutes)(_ ++ _.rvalue.outerWithClause)

	protected override def shape(implicit spelling :SQLSpelling) :RowShape = spelling.shape(component)
	protected override def columnCount(implicit spelling :SQLSpelling) :Int = spelling.columnCount(component)

	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		(spelling.sqlParamCount(component) /: substitutes)((acc, setter) => acc + spelling.sqlParamCount(setter.rvalue))

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val columns = explodedSpelling(true)(from, context, params).filter(_.nonEmpty)
		if (columns.isEmpty)
			SpelledSQL(context)
		else if (columns.sizeIs <= 1)
			columns.head
		else if (spelling.isInline)
			columns.reduce(_ +: ", " +: _)
		else
			("(" +: columns.reduce(_ +: ", " +: _)) + ")"
	}

	override def toString :String = substitutes.mkString(component.toString + "{", ", ", "}")

}




object EditedLValueSQL {

	type __ = EditedLValueSQL[_ <: RowProduct, M, S] forSome { type S; type M[O] <: BaseMapping[S, O] }


	trait SpecificEditedLValueVisitor[+F <: RowProduct, X, +Y]
		extends SpecificEditedComponentVisitor[F, X, Y] with SpecificEditedLooseComponentVisitor[F, X, Y]
	{
		def editedLValue[M[A] <: BaseMapping[X, A]](e :EditedLValueSQL[F, M, X]) :Y
	}
	type MatchSpecificEditedLValue[+F <: RowProduct, X, +Y] = SpecificEditedLValueVisitor[F, X, Y]

	trait CaseSpecificEditedLValue[+F <: RowProduct, X, +Y] extends MatchSpecificEditedLValue[F, X, Y] {
		override def editedComponent[M[A] <: BaseMapping[X, A]](e :EditedComponentSQL[F, M, X]) :Y =
			editedLValue(e)

		override def editedLooseComponent[M[A] <: BaseMapping[X, A]](e :EditedLooseComponent[F, M, X]) :Y =
			editedLValue(e)
	}
//
//
//	trait EditedLValueVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends EditedComponentVisitor[F, Y] with EditedLooseComponentVisitor[F, Y]
//	{
//		def editedLValue[M[A] <: BaseMapping[V, A], V]
//		                (e :EditedLValueSQL[F, M, V]) :Y[Single, V, EditedLValueSQL[F, M, V]]
//	}
//	type MatchEditedLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		EditedLValueVisitor[F, Y]
//
//	trait CaseEditedLValue[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchEditedLValue[F, Y]
//	{
//		override def editedComponent[M[A] <: BaseMapping[V, A], V](e :EditedComponentSQL[F, M, V])
//				:Y[Single, V, EditedComponentSQL[F, M, V]] =
//			editedLValue(e)
//
//		override def editedLooseComponent[M[A] <: BaseMapping[V, A], V](e :EditedLooseComponent[F, M, V])
//				:Y[Single, V, EditedLooseComponent[F, M, V]] =
//			editedLValue(e)
//	}

	trait AnyEditedLValueVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyEditedComponentVisitor[F, Y] with AnyEditedLooseComponentVisitor[F, Y]
	{
		def editedLValue[M[A] <: BaseMapping[V, A], V](e :EditedLValueSQL[F, M, V]) :Y[Single, V]
	}
	type MatchAnyEditedLValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyEditedLValueVisitor[F, Y]

	trait CaseAnyEditedLValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyEditedLValue[F, Y]
	{
		override def editedComponent[M[A] <: BaseMapping[S, A], S](e :EditedComponentSQL[F, M, S]) :Y[Single, S] =
			editedLValue(e)

		override def editedLooseComponent[M[A] <: BaseMapping[S, A], S]
		                                 (e :EditedLooseComponent[F, M, S]) :Y[Single, S] =
			editedLValue(e)
	}

}






/** A [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expression in which certain columns
  * have been replaced by arbitrary [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] expressions based
  * on the same ''from'' clause.
  */
trait EditedComponentSQL[-F <: RowProduct, M[A] <: BaseMapping[V, A], V] extends EditedLValueSQL[F, M, V] { self =>
	/** The type constructor for the mapping of the underlying
	  * [[net.noresttherein.oldsql.sql.ast.RelationSQL relation]]/[[net.noresttherein.oldsql.sql.ast.TableSQL TableSQL]].
	  */
	type Entity[O] <: BaseMapping[EntitySubject, O]

	/** The subject type of this expression's origin relation mapping, that is the Scala type to which whole rows
	  * from the table (or other relation) of which this is a component are mapped.
	  */
	type EntitySubject


	/** The modified component expression. */
	override val component :TypedComponentSQL[Origin, Entity, EntitySubject, M, V, FromLast]

	/** Maps export columns
	  * of [[net.noresttherein.oldsql.sql.ast.EditedLValueSQL.component component]]`.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.mapping`
	  * to their substitute expressions
	  * in the [[net.noresttherein.oldsql.sql.ast.EditedLValueSQL.substitutes substitutes]] list, if present.
	  * @return (pseudocode):
	  *         {{{
	  *             substitutes.map {
	  *                 setter => Assoc(component.origin.export.export(setter.lvalue.mapping), setter)
      *             } to NaturalMap
	  *         }}}
	  */
	lazy val exportSubstitutes
			:NaturalMap[MappingAt[Origin]#Column, ({ type T[X] = ColumnSetter[FromLast, F] { type Subject = X } })#T] =
		substituteMap(component.origin.export)

	/** Maps export columns
	  * of [[net.noresttherein.oldsql.sql.ast.EditedLValueSQL.component component]]`.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]]`.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]]
	  * to their substitute expressions
	  * in the [[net.noresttherein.oldsql.sql.ast.EditedLValueSQL.substitutes substitutes]] list, if present.
	  * @return (pseudocode):
	  *         {{{
	  *             substitutes.map {
	  *                 setter => Assoc(component.origin.anchored.export(setter.lvalue.mapping), setter)
	  *             } to NaturalMap
	  *         }}}
	  */
	lazy val anchoredSubstitutes
			:NaturalMap[MappingAt[Origin]#Column, ({ type T[X] = ColumnSetter[FromLast, F] { type Subject = X } })#T] =
		substituteMap(component.origin.anchored)


	override def default :MappingSQL[F, Single, M, V] = component.default.substitute(substitutes)

	override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
			:MappingSQL[F, Single, M, V] =
		component.defaultWith(includes, excludes).substitute(substitutes)

	override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
			:MappingSQL[F, Single, M, V] =
		component.alter(includes, excludes).substitute(substitutes)

	override def isAnchored :Boolean = substitutes.forall(_.rvalue.isAnchored)
	override def isAnchored(from :F) :Boolean =
		component.isAnchored(from) && substitutes.forall(_.rvalue.isAnchored(from))

	def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, Single, V] =
		if (permutation == permutation.indices)
			this
		else
			EditedComponentSQL(component.reorder(permutation))(substitutes)

//	protected override def reform[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                             (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(SQLExpression[F, Single, U], SQLExpression[E, C, U]) =
//		if (passesAllowed > 1)
//			super.reform(other)(reform, passesAllowed)
//		else {
//			implicit val compat = leftResult vs rightResult
//			val (left, right) = reform(component, other)
//			(new SubstituteReformed(other, Got(leftResult)).apply(left), right)
//		}
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
//	(other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
//	(implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(SQLExpression[F, Single, U], reform.LValue[E, C, U]) =
//		if (passesAllowed > 1)
//			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
//		else {
//			implicit val compat = leftResult vs rightResult
//			val (left, right) = reform(component, other)
//			(new SubstituteReformed(other, Got(leftResult)).apply(left), right)
//		}
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], U]
//	                             (other :ComponentSQL[E, C])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[V, U], rightResult :Lift[other.Subject, U],
//	                              spelling :SQLSpelling)
//			:(SQLExpression[F, Single, U], reform.LValue[E, C, U]) =
//		if (passesAllowed > 1)
//			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
//		else {
//			implicit val compat = leftResult vs rightResult
//			val (left, right) = reform(component, other)
//			(new SubstituteReformed(other, Got(leftResult)).apply(left), right)
//		}

	protected override def reform[F1 <: F, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                             (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
	                                       spelling :SQLSpelling)
			:(leftResult.SQLResult[F1, S1, MappingSQL[F1, S1, M, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
		if (this eq other)
			(leftResult(this), rightResult(other))
		else if (passCount.firstTime)
			super.reform[F1, S1, F2, S2, V2, EC2, U](other)(reform, passCount)
		else if (passCount.secondTime) {
			val substituteLeft = new SQLTransformation[V, U] {
				override type BaseResult[f <: RowProduct, s >: Grouped <: Single] = leftResult.BaseResult[f, s]
				override type SQLResult[f <: RowProduct, s >: Grouped <: Single, +E <: SQLExpression[f, s, U]] =
					leftResult.SQLResult[f, s, MappingSQL[f, s, M, U]]
//				override type ColumnResult[f <: RowProduct, s >: Grouped <: Single, +E <: ColumnSQL[f, s, U]] = ColumnMappingSQL[f, s, ]

				override def apply[f <: RowProduct, s >: Grouped <: Single, E[v] <: ConvertibleSQL[f, s, v, E]]
				                  (expr :ConvertingTemplate[f, s, V, E]) =
//				override def apply[f <: RowProduct, s >: Grouped <: Single, E[v] <: ConvertibleSQL[f, s, v, E]]
//				                  (expr :ConvertibleSQL[f, s, V, E]) =
					expr match {
						//We don't handle nulls because this transformation is applied to a reformed version
						// of self.value, which is not null, so it's unlikely to be converted to null.
						case self.component =>
							leftResult(EditedComponentSQL.this.asInstanceOf[MappingSQL[f, s, M, V]])
						case lvalue :(LValueSQL[F, M, V] { type FromLast = self.FromLast }) @unchecked
								if lvalue.mapping == self.component.mapping
									&& lvalue.component.origin.mapping == self.component.origin.mapping =>
							val setters = substitutes.filter { setter =>
								val left = component.origin.anchored.export(setter.lvalue.mapping.withOrigin[Origin])
								spelling.scope.isDefault(left)
							}
							val edited = lvalue.substitute[F](setters).asInstanceOf[MappingSQL[f, s, M, V]]
							leftResult(edited)
						case _ =>
							throw new IllegalExpressionException(
								"Cannot substitute expressions for columns: reforming of (" + component + ", " +
									other + ") with " + reform + " did not result in a LValueSQL: " + expr + "."
							)
					}

				override def apply(value :V) :U = leftResult(value)
				override def unapply(value :U) :Opt[V] = leftResult.unapply(value)
				override def inverse(value :U) :V = leftResult.inverse(value)

				override def applyString(arg :String) :String =
					leftResult.applyString(
						substitutes.view.map(_.lvalue.anchored.name).mkString(arg + ".substitute(", ",", ")")
					)
			}
			reform(component, other)(substituteLeft, rightResult, spelling)
		} else
			super.reform[F1, S1, F2, S2, V2, EC2, U](other)(reform, passCount)


	/** A visitor which attempts to reapply the substitutions from this `EditedComponentSQL`
	  * to a result of reforming `this.`[[net.noresttherein.oldsql.sql.ast.EditedComponentSQL.component component]].
	  * The only really applicable expression types are [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  * and its conversion [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]], but a visitor avoids
	  * pattern matching a [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]]
	  * with problematic type constructors as type parameters.
	  * If the component expression after reforming is neither of the above, it is used as-is
	  * (although this is likely an error).
	  * @param other the expression which must be aligned with `this`/`this.component`; used only in exception messages.
	  * @param lift  the `leftResult` argument of the `reform` method using this visitor. It is set to `Lack`
	  *              when this class is used to recursively align `component` when the reform result
	  *              is a `ComponentConversionSQL`.
	  */
//	private[this] class SubstituteReformed[U](other :SQLExpression.__, lift :Opt[Lift[V, U]])
//	                                         (implicit spelling :SQLSpelling)
//		extends CaseSpecificExpression[F, Single, U, SQLExpression[F, Single, U]]
//		   with CaseSpecificConvertedMapping[F, Single, U, SQLExpression[F, Single, U]]
//	{
//		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, N[A] <: BaseMapping[U, A], L <: RowProduct]
//		                      (e :TypedComponentSQL[O, T, R, N, U, L]) =
//			if (self.component == e && lift.isDefined)
//				self.to[U](lift.get)
//			else if (self.component.origin.mapping == e.origin.mapping && self.component.mapping == e.mapping) {
//				val setters = substitutes.filter { setter =>
//					val lvalue = e.origin.anchored.export(setter.lvalue.mapping.withOrigin[O])
//					spelling.scope.isDefault(lvalue)
//				}.castParam[ColumnSetter[e.FromLast, F]]
//				EditedComponentSQL(e)(setters)
//			} else if (self.mapping homomorphic e.mapping) {
//				try {
//					val rephraseLValue = new SetterLValueScribe[O, N, U, e.FromLast](e.upcast)
//					val setters = substitutes.flatMap { setter =>
//						val lvalue = rephraseLValue(setter.lvalue)
//						if (spelling.scope.isDefault(lvalue.component.anchored))
//							Some(lvalue := setter.rvalue)
//						else None
//					}
//					EditedComponentSQL(e)(setters)
//				} catch {
//					case ex :IllegalExpressionException =>
//						throw new MismatchedExpressionsException(
//							"Cannot align expressions (" + self + ") and (" + other
//								+ "); failed to convert a substitution to the reformed component " + e + "; "
//								+ ex.getMessage,
//							ex
//						)
//				}
//			} else
//				throw new MismatchedExpressionsException(self, other,
//				"reforming of a modified component " + self.component +
//					" produced a mapping not homomorphic with the original."
//				)
//
//		override def editedComponent[N[A] <: BaseMapping[U, A]](e :EditedComponentSQL[F, N, U]) =
//			e.component.substitute(substitutes.castParam[ColumnSetter[e.FromLast, F]] ++ e.substitutes)
//
//		override def convertedMapping[N[O] <: MappingAt[O], X](e :ConvertedMappingSQL[F, Single, N, X, U]) =
//			new SubstituteReformed[X](other, Lack).apply(e.value).to(e.conversion)
//
//		override def expression(e :SQLExpression[F, Single, U]) :SQLExpression[F, Single, U] = e
////			throw new MismatchedExpressionsException(EditedComponentSQL.this, other,
////				"reforming of the modified component " + self.component + " produced an unsupported expression: "
////					+ e + " (" + e.getClass + ")."
////			)
//	}


	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[F]#rows[Single]#__] = {
		val edits = substitutes.view.map[(ColumnMapping, ColumnMappingSQL.from[F]#rows[Single]#__)] { setter =>
			(setter.lvalue.export, EditedColumnSQL(setter))
		}.toMap
		val normal = spelling.split(component)
		def result[K, E](unedited :Seq[E], edited :Map[K, E])(export :E => K) :Seq[E] =
			unedited.map { column => edited.getOrElse(export(column), column) }
		val res = result(normal, edits)(_.export :ColumnMapping)
		res
	}


	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		if (!isAnchored(from))
			anchor(from).explodedSpelling(spelling, independent)(from, context, params)
		else {
			val tableMapping = component.origin.anchored
			val defaultColumns = spelling.scope.defaultColumns(tableMapping, component.mapping)
			val substituteIndices = substitutes.view.map {
				setter => defaultColumns.columnIndex(tableMapping.export(setter.lvalue.mapping.refine.withOrigin[Origin]))
			} to Unique
			val substituteSpellings = substitutes.scanLeft(SpelledSQL(context)) {
				(last, setter) => spelling(setter.rvalue)(from, last.context, params)
			}.tail to Array
			val ctx = if (substituteSpellings.isEmpty) context else substituteSpellings.last.context
			val normal = spelling.explode(component)(from, ctx, params)
			if (normal.size != defaultColumns.size)
				throw Bug(
					"Spelling of " + component + " produced " + normal + " with " + normal.size + " columns, while " +
					"there are " + defaultColumns.size + " default columns: " + defaultColumns + "."
				)

			normal.view.zipWithIndex.map { case (sql, i) =>
				substituteIndices.indexOf(i) match {
					case n if n >= 0 => substituteSpellings(n)
					case _ => sql
				}
			} to Seq
		}


	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, V, Y]) :Y =
		visitor.editedComponent(this)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, V] =
		visitor.editedComponent(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: EditedComponentSQL[F_, M, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.editedComponent(this)

	override def isomorphic(that :SQLExpression.__) :Boolean = that match {
		case _ if this eq that => true
		case other :EditedComponentSQL[F @unchecked, M @unchecked, V @unchecked] if other canEqual this =>
			(component isomorphic other.component) && substitutes.size == other.substitutes.size &&
				(substitutes.view zip other.substitutes).forall { case (my, their) =>
					(my.lvalue isomorphic their.lvalue) && (my.rvalue isomorphic their.rvalue)
				}
		case _ => false
	}
}




object EditedComponentSQL {
	def apply[F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[S, A], S, L <: RowProduct]
	         (component :TypedComponentSQL[F, T, R, M, S, L])(setters :Seq[ColumnSetter[L, F]])
			:EditedComponentSQL[F, M, S] =
	{
		val substitutedColumns = setters.groupBy(
			setter => component.origin.export.export(setter.lvalue.mapping.withOrigin[component.Origin])
		)
		if (substitutedColumns.exists(_._2.size > 1))
			throw new IllegalArgumentException("Cannot substitute columns " + setters + " in " + component +
				": the list of substitutions contains duplicates."
			)
		new Impl[F, T, R, M, S, L](component, setters)
	}


	type __ = EditedComponentSQL[_ <: RowProduct, M, S] forSome { type M[A] <: BaseMapping[S, A]; type S }

	private class Impl[F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[S, A], S, L <: RowProduct]
	                  (override val component :TypedComponentSQL[F, T, R, M, S, L],
	                   override val substitutes :Seq[ColumnSetter[L, F]])
		extends EditedComponentSQL[F, M, S]
	{
		override type Entity[A] = T[A]
		override type EntitySubject = R
		override type Origin   = F
		override type FromLast = L
	}


	trait SpecificEditedComponentVisitor[+F <: RowProduct, X, +Y] {
		def editedComponent[M[A] <: BaseMapping[X, A]](e :EditedComponentSQL[F, M, X]) :Y
	}
	type MatchSpecificEditedComponent[+F <: RowProduct, X, +Y] = SpecificEditedComponentVisitor[F, X, Y]
	type CaseSpecificEditedComponent[+F <: RowProduct, X, +Y] = SpecificEditedComponentVisitor[F, X, Y]
//
//	trait EditedComponentVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def editedComponent[M[A] <: BaseMapping[V, A], V]
//		                   (e :EditedComponentSQL[F, M, V]) :Y[Single, V, EditedComponentSQL[F, M, V]]
//	}
//	type MatchEditedComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		EditedComponentVisitor[F, Y]
//	type CaseEditedComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		EditedComponentVisitor[F, Y]


	trait AnyEditedComponentVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def editedComponent[M[A] <: BaseMapping[V, A], V](e :EditedComponentSQL[F, M, V]) :Y[Single, V]
	}
	type MatchAnyEditedComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyEditedComponentVisitor[F, Y]
	type CaseAnyEditedComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyEditedComponentVisitor[F, Y]
}






trait EditedLooseComponent[-F <: RowProduct, M[O] <: BaseMapping[V, O], V] extends EditedLValueSQL[F, M, V] { self =>

	/** The modified component expression. */
	override val component :LooseComponent[Origin, M, V]

	private def substituteAltered(component :LooseComponent[Origin, M, V]) :MappingSQL[F, Single, M, V] =
		component.substitute(substitutes.castParam[ColumnSetter[component.FromLast, Origin]])

	override def default :MappingSQL[F, Single, M, V] = substituteAltered(component.default)

	override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
			:MappingSQL[F, Single, M, V] =
		substituteAltered(component.defaultWith(includes, excludes))

	override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
			:MappingSQL[F, Single, M, V] =
		substituteAltered(component.alter(includes, excludes))

	override def isAnchored :Boolean = false
	override def isAnchored(from :F) :Boolean = false


	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[F]#rows[Single]#__] =
		spelling.split(component) //throws


	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		anchor(from).explodedSpelling(spelling, independent)(from, context, params)


	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, V, Y]) :Y =
		visitor.editedLooseComponent(this)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, V] =
		visitor.editedLooseComponent(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: EditedLooseComponent[F_, M, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.editedLooseComponent(this)

	override def isomorphic(that :SQLExpression.__) :Boolean = that match {
		case _ if this eq that => true
		case other :EditedLooseComponent[F @unchecked, M @unchecked, V @unchecked] if other canEqual this =>
			(component isomorphic other.component) && substitutes.size == other.substitutes.size &&
				(substitutes.view zip other.substitutes).forall { case (my, their) =>
					(my.lvalue isomorphic their.lvalue) && (my.rvalue isomorphic their.rvalue)
				}
		case _ => false
	}

}




object EditedLooseComponent {
	def apply[O <: RowProduct, F <: O, M[A] <: BaseMapping[S, A], S]
	         (component :LooseComponent[O, M, S])(setters :Seq[ColumnSetter[component.FromLast, F]])
			:EditedLooseComponent[F, M, S] =
		new Impl[O, F, M, S, component.FromLast](component, setters)


	type __ = EditedLooseComponent[_ <: RowProduct, M, S] forSome { type M[A] <: BaseMapping[S, A]; type S }

	private class Impl[O <: RowProduct, F <: O, M[A] <: BaseMapping[S, A], S, L <: RowProduct]
	                  (override val component :LooseComponent[O, M, S] { type FromLast = L },
	                   override val substitutes :Seq[ColumnSetter[L, F]])
		extends EditedLooseComponent[F, M, S]
	{
		override type Origin   = O
		override type FromLast = L
	}


	trait SpecificEditedLooseComponentVisitor[+F <: RowProduct, X, +Y] {
		def editedLooseComponent[M[A] <: BaseMapping[X, A]](e :EditedLooseComponent[F, M, X]) :Y
	}
	type MatchSpecificEditedLooseComponent[+F <: RowProduct, X, +Y] = SpecificEditedLooseComponentVisitor[F, X, Y]
	type CaseSpecificEditedLooseComponent[+F <: RowProduct, X, +Y] = SpecificEditedLooseComponentVisitor[F, X, Y]
//
//	trait EditedLooseComponentVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def editedLooseComponent[M[A] <: BaseMapping[V, A], V]
//		                        (e :EditedLooseComponent[F, M, V]) :Y[Single, V, EditedLooseComponent[F, M, V]]
//	}
//	type MatchEditedLooseComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		EditedLooseComponentVisitor[F, Y]
//	type CaseEditedLooseComponent[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		EditedLooseComponentVisitor[F, Y]

	trait AnyEditedLooseComponentVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def editedLooseComponent[M[A] <: BaseMapping[V, A], V](e :EditedLooseComponent[F, M, V]) :Y[Single, V]
	}
	type MatchAnyEditedLooseComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyEditedLooseComponentVisitor[F, Y]
	type CaseAnyEditedLooseComponent[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyEditedLooseComponentVisitor[F, Y]

}






/** An expression substituting a relation column. It is used as the result of
  * [[net.noresttherein.oldsql.sql.ast.EditedLValueSQL EditedLValueSQL]]`.`[[net.noresttherein.oldsql.sql.ast.EditedLValueSQL.\ \]].
  */
//Uses value type V, not X, because \ TypedColumn[T, Origin] returns ColumnMappingSQL[_,_,_,T].
//  A potential problem is with selectForm, which now uses lvalue.selectForm, not rvalue.selectForm.
//  Not a huge deal, because \ is only really used in reforming, unlikely ever to create actually used expressions
trait EditedColumnSQL[-F <: RowProduct, M[A] <: BaseColumn[V, A], V, X]
	extends AdaptedColumnSQL[F, Single, X, V] with AbstractAdaptedSQL[F, Single, X, V]
	   with ColumnMappingSQL[F, Single, M, V] //ColumnSQL must be mixed in after TransformedSQL
	   with SingleRowSQLTemplate[F, V, ({ type E[-f <: RowProduct] = EditedColumnSQL[f, M, V, X] })#E]
{ //consider: this could potentially be of any scope; however, ColumnSetter requires Single for rvalue
	val column :ColumnLValueSQL[FromLast, M, X]// { type FromLast = EditedColumnSQL.this.FromLast }
	val value  :ColumnSQL[F, Single, X]
	val setter :ColumnSetter[FromLast, F] { type Component[O] = M[O]; type Subject = V; type Value = X }
	override val mapping       :M[Origin]              = column.mapping
	override val export        :TypedColumn[V, Origin] = column.export
	override lazy val anchored :TypedColumn[V, Origin] = column.anchored

	override def selectForm :ColumnReadForm[V] = column.export.form

	type FromLast <: RowProduct //= column.FromLast
	override type Origin    = column.Origin

	override def convert(value :X) :V = column.conversion.inverse(value)

	override def \[K <: MappingAt[Origin], S](component :K)(implicit project :OriginProjection[K, S])
			:MappingSQL[F, Single, project.WithOrigin, X]  =
		if (component == column.mapping || component == column.export)
			this.asInstanceOf[MappingSQL[F, Single, project.WithOrigin, S]]
		else
			throw new IllegalArgumentException(
				"Mapping " + component + " is not a component of edited column " + column + " of " + this + "."
			)

	override def \[K <: ColumnAt[Origin], S]
	              (column :K)(implicit project :OriginProjection[K, S] { type WithOrigin[A] <: BaseColumn[S, A] })
			:ColumnMappingSQL[F, Single, project.WithOrigin, S] =
		if (column == this.column.mapping || column == this.column.export)
			this.asInstanceOf[EditedColumnSQL[F, project.WithOrigin, S, S]]
		else
			throw new IllegalArgumentException(
				"Mapping " + column + " is not a component of edited column " + this.column + " of " + this + "."
			)


	override def isDefault :Boolean = true
	override def default   :ColumnMappingSQL[F, Single, M, V] = this

	override def defaultWith(includes :Unique[TypedMapping[_, Origin]], excludes :Unique[TypedMapping[_, Origin]])
			:EditedColumnSQL[F, M, V, X] =
		alter(includes, excludes)

	override def alter(includes :Iterable[TypedMapping[_, Origin]], excludes :Iterable[TypedMapping[_, Origin]])
			:EditedColumnSQL[F, M, V, X] =
		if (excludes.nonEmpty)
			throw new UnsupportedOperationException(
				s"A column expression $this cannot be excluded from itself: $excludes"
			)
		else this

//	protected override def map[Y](lift :Lift[S, Y]) :ColumnMappingSQL[F, GlobalScope, M, Y] =
//		ConvertedColumnMappingSQL(this, lift)

//	override def asGlobal :Option[ColumnSQL[F, GlobalScope, S]] = Some(this)
//	override def isAnchored :Boolean = value.isAnchored
//	override def isAnchored(from :F) :Boolean = value.isAnchored(from)
//
//	protected def convert[X](conversion :SQLConversion[V, X]) :ColumnSQL[F, S, X] =
//		ConvertedColumnMappingSQL(this, conversion)

	override def anchor(from :F) :EditedColumnSQL[F, M, V, X] =
		EditedColumnSQL(column, value.anchor(from))

//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :EditedColumnSQL[E, M, V, X] =
//		EditedColumnSQL(column, value.basedOn(base))
//
	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
			:EditedColumnSQL[E, M, V, X] =
		EditedColumnSQL(column, value.expand(base))


	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, X]) :SQLExpression[E, C, V] =
		throw new IllegalExpressionException(
			"Cannot recreate an EditedColumnSQL like " + this + " from multi column expression " + e + "."
		)

	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, V] =
		e.asSingleRow match {
			case Some(global) => EditedColumnSQL(column, global)
			case _ =>
				throw new IllegalExpressionException(
					"Cannot create an EditedColumnSQL for column " + column + " and a non global expression " + e + "."
				)
		}

	protected override def reform[F1 <: F, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                 (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
                                           spelling :SQLSpelling)
			:(leftResult.SQLResult[F1, S1, ColumnMappingSQL[F1, S1, M, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
		if (this eq other)
			(leftResult(this), rightResult(other))
		else if (passCount.firstTime)
			super.reform[F1, S1, F2, S2, V2, EC2, U](other)(reform, passCount)
		else {
			val right = reform.prohibitReformLeft((column :LValueSQL[FromLast, M, X]).component, other)._2
			(leftResult(this), right)
		}


	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(value)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(value)(from, context, params)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, Single, V, Y]) :Y = visitor.editedColumn(this)
	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[Single, V] = visitor.editedColumn(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: ColumnMappingSQL[F_, S_, M, X] <: SQLExpression[F_, S_, X],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, X, E] =
//		visitor.editedColumn(this)

	override def isomorphic(that :SQLExpression.__) :Boolean = ???

	override def toString :String = "(" + setter + ")"
}




object EditedColumnSQL {
	def apply[L <: RowProduct, R <: RowProduct, M[O] <: BaseColumn[S, O], S, V]
	         (lvalue :ColumnLValueSQL[L, M, V], rvalue :ColumnSQL[R, Single, V])
			:EditedColumnSQL[R, M, S, V] =
		new Impl[L, R, M, S, V](lvalue, rvalue, ColumnSetter(lvalue, rvalue))

	def apply[L <: RowProduct, R <: RowProduct](setter :ColumnSetter[L, R])
			:EditedColumnSQL[R, setter.Component, setter.Subject, setter.Value] =
		new Impl[L, R, setter.Component, setter.Subject, setter.Value](setter.lvalue, setter.rvalue, setter)

	private class Impl[L <: RowProduct, R <: RowProduct, M[A] <: BaseColumn[S, A], S, V]
	              (override val column :ColumnLValueSQL[L, M, V],
	               override val value :ColumnSQL[R, Single, V],
	               override val setter :ColumnSetter[L, R] { type Component[O] = M[O]; type Subject = S; type Value = V })
		extends EditedColumnSQL[R, M, S, V]
	{
		override type FromLast = L
	}


	trait SpecificEditedColumnVisitor[+F <: RowProduct, X, +Y] {
		def editedColumn[M[A] <: BaseColumn[X, A], V](e :EditedColumnSQL[F, M, X, V]) :Y
	}
	type MatchSpecificEditedColumn[+F <: RowProduct, X, +Y] = SpecificEditedColumnVisitor[F, X, Y]
	type CaseSpecificEditedColumn[+F <: RowProduct, X, +Y] = SpecificEditedColumnVisitor[F, X, Y]
//
//
//	trait EditedColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def editedColumn[M[A] <: BaseColumn[V, A], V, X]
//		                (e :EditedColumnSQL[F, M, V, X]) :Y[Single, V, EditedColumnSQL[F, M, V, X]]
//	}
//	type MatchEditedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		EditedColumnVisitor[F, Y]
//	type CaseEditedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		EditedColumnVisitor[F, Y]

	trait AnyEditedColumnVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] {
		def editedColumn[M[A] <: BaseColumn[S, A], S, V](e :EditedColumnSQL[F, M, S, V]) :R[Single, S]
	}
	type MatchAnyEditedColumn[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] = AnyEditedColumnVisitor[F, R]
	type CaseAnyEditedColumn[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] = AnyEditedColumnVisitor[F, R]

}

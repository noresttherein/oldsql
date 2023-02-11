package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{ConstSeq, Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{Bug, InseparableExpressionException}
import net.noresttherein.oldsql.morsels.generic.{Fixed, Self}
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, ColumnReadForm, ColumnWriteForm, Mapping, Relation, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.SQLForm.FormBasedFactory
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormBasedFactory
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.sql.{ColumnSQL, GroundColumn, GroundSQL, RowProduct, RowShape, SingleBoolean, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.SQLBoolean.{False, True}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingTemplate, GroundSQLTemplate, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.BoundColumnMappingParam.{AnyBoundColumnMappingParamVisitor, CaseAnyBoundColumnMappingParam, CaseSpecificBoundColumnMappingParam, SpecificBoundColumnMappingParamVisitor}
import net.noresttherein.oldsql.sql.ast.BoundMappingParam.{AnyBoundMappingParamVisitor, CaseAnyBoundMappingParam, CaseSpecificBoundMappingParam, SpecificBoundMappingParamVisitor}
import net.noresttherein.oldsql.sql.ast.BoundParam.{AnyBoundParamVisitor, CaseAnyBoundParam, CaseSpecificBoundParam, Impl, SpecificBoundParamVisitor}
import net.noresttherein.oldsql.sql.ast.BoundColumnParam.{AnyBoundColumnParamVisitor, CaseAnyBoundColumnParam, CaseSpecificBoundColumnParam, SpecificBoundColumnParamVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnLiteral.{AnyColumnLiteralVisitor, CaseAnyColumnLiteral, CaseSpecificColumnLiteral, SpecificColumnLiteralVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingLiteral.{AnyColumnMappingLiteralVisitor, CaseAnyColumnMappingLiteral, CaseSpecificColumnMappingLiteral, SpecificColumnMappingLiteralVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingTerm.{AnyColumnMappingTermVisitor, ColumnMappingTermTemplate, SpecificColumnMappingTermVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnTerm.{AnyColumnTermVisitor, ErrorColumnTerm, RWColumnTerm, SpecificColumnTermVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnTerm.ErrorColumnTerm.{AnyErrorColumnTermVisitor, CaseAnyErrorColumnTerm, CaseSpecificErrorColumnTerm, SpecificErrorColumnTermVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnTerm.RWColumnTerm.{AnyRWColumnTermVisitor, CaseAnyRWColumnTerm, CaseSpecificRWColumnTerm, SpecificRWColumnTermVisitor}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.EmptySQL.{AnyEmptyVisitor, SpecificEmptyVisitor}
import net.noresttherein.oldsql.sql.ast.LValueSQL.CaseSpecificLValue
import net.noresttherein.oldsql.sql.ast.MappingLiteral.{AnyMappingLiteralVisitor, CaseAnyMappingLiteral, CaseSpecificMappingLiteral, SpecificMappingLiteralVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLTemplate
import net.noresttherein.oldsql.sql.ast.MappingTerm.{AnyMappingTermVisitor, MappingInstance, MappingTermTemplate, SpecificMappingTermVisitor}
import net.noresttherein.oldsql.sql.ast.MultiNull.{AnyMultiNullVisitor, CaseAnyMultiNull, CaseSpecificMultiNull, SpecificMultiNullVisitor}
import net.noresttherein.oldsql.sql.ast.NativeColumnTerm.{AnyNativeColumnTermVisitor, CaseAnyNativeColumnTerm, CaseSpecificNativeColumnTerm, SpecificNativeColumnTermVisitor}
import net.noresttherein.oldsql.sql.ast.NativeTerm.{AnyNativeTermVisitor, CaseAnyNativeTerm, CaseSpecificNativeTerm, SpecificNativeTermVisitor}
import net.noresttherein.oldsql.sql.ast.SQLLiteral.{AnyLiteralVisitor, CaseAnyLiteral, CaseSpecificLiteral, SpecificLiteralVisitor}
import net.noresttherein.oldsql.sql.ast.SQLNull.{AnyNullVisitor, CaseAnyNull, CaseSpecificNull, SpecificNullVisitor}
import net.noresttherein.oldsql.sql.ast.SQLTerm.{ErrorTerm, RWTerm, SQLTermTemplate}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ErrorTerm.{AnyErrorTermVisitor, CaseAnyErrorTerm, CaseSpecificErrorTerm, SpecificErrorTermVisitor}
import net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.{AnyRWTermVisitor, CaseAnyRWTerm, CaseSpecificRWTerm, ConvertingRWTermTemplate, SpecificRWTermVisitor}
import net.noresttherein.oldsql.sql.mechanics.{Interoperable, Reform, SpelledSQL, SQLConversion, SQLOrdering, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}

//implicits
import net.noresttherein.oldsql.slang._





/** Base type for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] implementations representing various
  * atomic terms. The atomicity is in regard to the SQL AST defined in this package, but not necessarily the generated
  * SQL, as instances for types `T` represented by multi column [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]s
  * are possible. Terms are [[net.noresttherein.oldsql.sql.GroundSQL ground]] expressions,
  * meaning they depend on no tables and can be used wherever an expression of a matching value type is used.
  * In most, but not all, cases the value associated with the expression is known and mapped to a `Scala` object.
  * @see [[net.noresttherein.oldsql.sql.ast.ColumnTerm]]
  */ //todo: rename to SQLAtom; make it a SelectableSQL
trait SQLTerm[V] //not a ConvertingTemplate[_, _, _, SQLTerm] because MappingTerm is a ConvertingTemplate[_,_,_,MappingSQL]
	extends SQLExpression[RowProduct, Single, V]
	   with SQLTermTemplate[V, GroundSQL, SQLTerm, ColumnTerm]
{
//	protected def groundValue :Option[T]
//	override def isGlobal   = true
//	override def isGround   = true
//	override def isAnchored = true
//	override def isAnchored(from :RowProduct) = true
//	override def anchor(from :RowProduct) :SQLTerm[V] = this

//	protected override def reform[E <: RowProduct, C[A] <: MappingAt[A], U]
//	                             (other :ComponentSQL[E, C])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[V, U], rightResult :Lift[other.Subject, U],
//	                              spelling :SQLSpelling)
//			:(SQLExpression[RowProduct, Single, U], reform.LValue[E, C, U]) =
//		if (passesAllowed >= MayPassBack)
//			super.reform(other)(reform, passesAllowed)
//		else
//			(leftResult(this), rightResult(other)) match {
//				case (l, r) if !leftResult.isDerived || !rightResult.isDerived =>
//					reform(l, r)
//				case (l, r) if spelling.shape(l) <:> spelling.shape(r) =>
//					(l, reform.lift(r))
//				case (l, r) if reform.mayReformLeft =>
//					try {
//						val form = spelling.scope.termForm(other.origin.anchored, other.anchored)
//						(this.reform(rightResult(form), leftResult), reform.lift(r))
//					} catch {
//						case _ :Exception if spelling.shape(l) <:> spelling.shape(r) =>
//							(l, reform.lift(r))
//						case e :Exception => try {
//							super.reform(other)(reform, passesAllowed)
//						} catch {
//							case e1 :Exception => e.addSuppressed(e1); throw e
//						}
//					}
//				case (_, _) =>
//					super.reform(other)(reform, passesAllowed)
//			}


//	protected class TermReformer[F1 <: RowProduct, S1 >: Grouped <: Single,
//	                             EC1[v] >: SQLExpression[RowProduct, Single, v] <: SQLExpression[F1, S1, v],
//		                         F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//		                         EC2[v] <: SQLExpression[F2, S2, v], E2 <: EC2[V2], U,
//		                         LeftRes <: SQLExpression[F1, S1, U], RightRes <: SQLExpression[F2, S2, U]]
//	                            (form :SQLForm[V])
//		                        (other :ConvertingTemplate[F2, S2, V2, EC2, E2])(reform :Reform, passCount :PassCount)
//		                        (implicit leftResult  :SpecificTransformation[V, U, EC1, SQLTerm[V], LeftRes],
//		                                  rightResult :SpecificTransformation[V2, U, EC2, E2, RightRes],
//		                                  spelling :SQLSpelling)
//		extends BaseReformer[F1, S1, EC1, F2, S2, V2, EC2, E2, U, LeftRes, RightRes](other)(reform, passCount)
//	{
//		protected override def fallback :(LeftRes, RightRes) =
//			if (reform.compatible(SQLTerm.this, other))
//				(leftResult(SQLTerm.this), rightResult(other))
//			else
//				super.fallback
//
//		override def term[X](e :SQLTerm[X])
//				:SpecificTransformation[X, U, SQLTerm, SQLTerm[X], RightRes] => (LeftRes, RightRes) =
//			trans =>
//				if (passCount.mayPassBack)
//					pass
//				else if (reform.mayReformRight && form.isUniversal && leftResult.isReversible)
//					//todo: lose the restriction on `leftResult` being reversible. We can't however
//					// just do it in general, as it involves mapping a write form with leftResult.unapply.
//					// One idea is to introduce a 'downcast' method, which would in a MappingForm remove columns
//					// for properties not existing in the supertype.
//					trans.convert(e.reform(leftResult(form), _)) match {
//						case Got(res) => (leftResult.sql(SQLTerm.this), res)
//						case _        => fallback
//					}
//				else
//					fallback
//
//		override def rwTerm[X](e :RWTerm[X])
//				:SpecificTransformation[X, U, RWTerm, RWTerm[X], RightRes] => (LeftRes, RightRes) =
//			trans =>
//				if (reform.mayReformRight && form.isUniversal && leftResult.isReversible)
//					(leftResult(SQLTerm.this), e.reform(leftResult(form), trans))
//				else if (reform.mayReformLeft && e.form.isUniversal && rightResult.isReversible)
//					(SQLTerm.this.reform(trans(e.form), leftResult), trans.sql(e))
//				else
//					fallback
//	}

	protected override def shape(implicit spelling :SQLSpelling)         :RowShape = selectForm.shape
	protected override def columnCount(implicit spelling :SQLSpelling)   :Int = selectForm.columnCount
	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = 0

//	override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//	                    (matcher :AnyExpressionVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
//		matcher.term(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             C[v] >: SQLExpression[RowProduct, Single, v] <: SQLExpression[F_, S_, v], T >: SQLTerm[V] <: C[V],
//		                         Y[-s >: Grouped <: Single, v, c[vv] <: SQLExpression[F_, s, vv], -e <: c[v]]]
//		                        (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, C, T] =
//		visitor.term(this)


	def sameAs(that :Any) :Boolean = canEqual(that)

	override def isomorphic(expression: SQLExpression.__): Boolean = expression match { //this == expression
		case self :AnyRef if self eq this => true
		case term :SQLTerm[_] if term sameAs this => term.groundValue == groundValue
		case _ => false
	}

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case term :SQLTerm[_] if term.canEqual(this) =>
			term.groundValue == groundValue && term.selectForm == selectForm
		case _ => false
	}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLTerm[_]]
	override def hashCode :Int = groundValue.hashCode * 31 + selectForm.hashCode

}






object SQLTerm {
	def apply[V](literal :V)(implicit factory :TermFactory.Factory[V]) :factory.Res = TermFactory(literal)

	type Factory[V] = TermFactory.Factory[V]

	object TermFactory extends FormBasedFactory[Self, SQLTerm, ColumnTerm] {
		implicit def fromImplicitRelation[M[A] <: BaseMapping[V, A], V](implicit table :Relation[M])
				:Factory[V] { type Res = MappingLiteral[M, V] } =
			new Factory[V]()(table.row.selectForm <> table.row[Unit].writeForm(table.row[Unit].selectedByDefault)) {
				override def apply(arg :V) = MappingLiteral(arg)(MappingTerm.implicitRelationMapping)
				override type Res = MappingLiteral[M, V]
			}

		implicit def fromImplicitMapping[M[A] <: BaseMapping[V, A], V](implicit mapping :M[_])
				:Factory[V] { type Res = MappingLiteral[M, V] } =
		{
			val m = mapping.withOrigin[Any]
			new Factory[V]()(m.selectForm <> m.writeForm(m.selectedByDefault)) {
				override type Res = MappingLiteral[M, V]
				override def apply(arg :V) = MappingLiteral(arg)
			}
		}

		protected override def generalResult[V :SQLForm](arg :V) :SQLTerm[V] = arg match {
			case null => new MultiNull[V]
			case @~   => ChainTuple.EmptyChain
			case _ => SQLLiteral(arg)
		}

		protected override def specificResult[V :ColumnForm](arg :V) :ColumnTerm[V] = arg match {
			case null => new SQLNull[V]
			case _ => ColumnLiteral(arg)
		}
	}


	implicit def nullColumn[T :ColumnForm](self :SQLNull.type) :SQLNull[T] = SQLNull[T]
	implicit def nullComposite[T :SQLForm](self :SQLNull.type) :MultiNull[T] = MultiNull[T]



	trait SQLTermTemplate[V, +E[v] <: ConvertibleSQL[RowProduct, Single, v, E], +T[v] <: E[v] with SQLTerm[v], +C[v] <: T[v]]
		extends ConvertingTemplate[RowProduct, Single, V, E]
		   with GroundSQLTemplate[V, T[V]]
	{ this :T[V] =>
		/** Creates a new term of the same value and kind as this one, but using the specified form to format it
		  * as SQL/set JDBC parameters.
		  */
		def reform(form :SQLForm[V]) :T[V] = (this :SQLTermTemplate[V, E, T, C]).reform(form, SQLConversion.toSelf)

		/** Creates a new term of the same value and kind as this one, but using the specified form to format it
		  * as SQL/set JDBC parameters.
		  */
		def reform(form :ColumnForm[V]) :C[V] = (this :SQLTermTemplate[V, E, T, C]).reform(form, SQLConversion.toSelf)

		/** Converts this term to an equivalent or auto convertible, from the point of view the DBMS, type `U`.
		  * This is similar to [[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[U]`, but this method requires
		  * a fresh form for `U` and creates a new term expression around that form, rather than wrapping this one
		  * in a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL conversion]] expression.
		  */
		//this method is very problematic, because form is usually obtained by mapping a SQLForm[_ <: U] through SQLTransformation.unapply
		def reform[U](form :SQLForm[U], conversion :SQLConversion[V, U]) :T[U]

		/** Converts this term to an equivalent or auto convertible, from the point of view the DBMS, column `U`.
		  * This is similar to [[net.noresttherein.oldsql.sql.SQLExpression.to to]]`[U]`, but this method requires
		  * a fresh column form for `U` and creates a new column term expression around that form,
		  * rather than wrapping this one in a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL conversion]] expression.
		  */
		def reform[U](form :ColumnForm[U], conversion :SQLConversion[V, U]) :C[U]


		protected override def reformer[F1 <: RowProduct, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                                   (implicit leftResult  :SQLTransformation[V, U],
	                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
				:SpecificExpressionVisitor
				 [F2, S2, V2, (leftResult.SQLResult[F1, S1, E[U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
			new TermReformer[F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](other)(reform, passCount)

		protected class TermReformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
			                         EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U,
			                         LR[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U],
			                         RR[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U]]
		                            (form :SQLForm[V])
		                            (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
			                        (implicit leftResult  :SQLTransformation[V, U]#Into[LR],
			                                  rightResult :SQLTransformation[V2, U]#Into[RR], spelling :SQLSpelling)
			extends BaseReformer[RowProduct, Single, F2, S2, V2, EC2, U, LR, RR](other)(reform, passCount)
			   with CaseSpecificLValue[F2, V2, (LR[E[U]], RR[EC2[U]])]
		{
			protected override def mapping[M[O] <: MappingAt[O]](e :MappingSQL[F2, S2, M, V2]) =
				if (reform.mayAlterLeft && !passCount.firstTime)
					reformLeft(e.conversion(spelling.scope.termForm(e.anchored)))
				else
					fallback

			protected override def component[O >: F2 <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V2, A], L <: RowProduct]
			                                (e :TypedComponentSQL[O, T, R, M, V2, L]) =
				if (reform.mayAlterLeft && !passCount.firstTime)
					reformLeft(spelling.scope.termForm(e.origin.anchored, e.anchored))
				else
					fallback

			override def rwTerm(e :RWTerm[V2]) =
				if (reform.mayAlterLeft && e.form.isUniversal && !passCount.firstTime)
					reformLeft(e.form)
				else
					fallback

			protected def reformLeft(form :SQLForm[V2]) :Result =
				try {
					if (passCount.thirdTime || passCount.secondTime && rightResult.isReversible)
						leftResult match {
							//important to know that leftResult doesn't create expressions of some incompatible type
							case conversion :SQLConversion[V, U] =>
								val reformed = SQLTermTemplate.this.reform(rightResult(form), conversion)
								(reformed.asInstanceOf[Left], rightResult(other))
							case _ if leftResult.isReversible =>
								val mappedForm = form.bimap(v2 => leftResult.inverse(rightResult(v2)))(
									v => rightResult.inverse(leftResult(v))
								)(selectForm.nulls)
								(leftResult(SQLTermTemplate.this.reform(mappedForm)), right)
							case _ =>
								val mappedForm = form.optBimap(v2 => leftResult.unapply(rightResult(v2)))(
									v => rightResult.unapply(leftResult(v))
								)(selectForm.nulls)
								(leftResult(SQLTermTemplate.this.reform(mappedForm)), right)
						}
					else
						fallback
				} catch {
					case e :Exception =>
						try { fallback } catch {
							case e2 :Exception => e.addSuppressed(e2); throw e
						}
				}
		}

		protected class RWTermReformer[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
			                           EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U,
			                           LR[f <: RowProduct, s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U],
			                           RR[f <: RowProduct, s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U]]
		                              (form :SQLForm[V])
		                              (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
			                          (implicit leftResult  :SQLTransformation[V, U]#Into[LR],
			                                    rightResult :SQLTransformation[V2, U]#Into[RR], spelling :SQLSpelling)
			extends TermReformer[F2, S2, V2, EC2, U, LR, RR](other)(reform, passCount)
		{
			override def rwTerm(e :RWTerm[V2]) =
				if (reform.mayAlterRight && passCount.thirdTime && form.isUniversal && leftResult.isReversible)
					reformRight(e)
				else super.rwTerm(e)

			override def term(e :SQLTerm[V2]) =
				if (!passCount.lastChance)
					pass
				else if (reform.mayAlterRight && form.isUniversal && leftResult.isReversible)
					//todo: lose the restriction on `leftResult` being reversible. We can't however
					// just do it in general, as it involves mapping a write form with leftResult.unapply.
					// One idea is to introduce a 'downcast' method, which would in a MappingForm remove columns
					// for properties not existing in the supertype.
					reformRight(e)
				else
					fallback

			protected def reformRight(e :SQLTerm[V2]) :Result =
				try {
					if (passCount.thirdTime || passCount.secondTime && leftResult.isReversible)
						rightResult match {
							case conversion :SQLConversion[V2, U] =>
								(left, e.reform(leftResult(form), conversion).asInstanceOf[Right])
							case _ if rightResult.isReversible =>
								val mappedForm = form.bimap(v => rightResult.inverse(leftResult(v)))(
									v2 => leftResult.inverse(rightResult(v2))
								)(form.nulls)
								(left, rightResult(e.reform(mappedForm)).asInstanceOf[Right])
							case _ =>
								val mappedForm = form.optBimap(v => rightResult.unapply(leftResult(v)))(
									v2 => leftResult.unapply(rightResult(v2))
								)(form.nulls)
								(left, rightResult(e.reform(mappedForm)))
						}
					else
						fallback
				} catch {
					case e :Exception =>
						try { fallback } catch {
							case e2 :Exception => e.addSuppressed(e2); throw e
						}
				}
		}
	}



	/** An SQL expression for a specific Scala [[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.value value]]
	  * of type `T`. Additionally, in order to represent this value properly as SQL, it defines a read/write
	  * [[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.form form]],
	  * which can be potentially used by other terms. This is a common supertype of
	  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral SQLLiteral]] and
	  * [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] allowing their uniform handling
	  * through a shared interface, in particular for [[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.reform reforming]]
	  * purposes.
	  */ //not preserved by conversions because MappingTerm must return a MappingSQL, so we'd need a special class
	trait RWTerm[V] extends SQLTerm[V] with SQLTermTemplate[V, GroundSQL, RWTerm, RWColumnTerm] {
		/** The value of this term. */
		def value :V

		/** An SQL form which can read and write `this.`[[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.value value]]. */
		val form  :SQLForm[V]

		/** If `true`, [[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.form form]] can be safely used to read
		  * and write values other than [[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.value value]].
		  * This is usually the case for user-created instances, but some operations can create literals/parameters
		  * with forms valid only for their particular values. This is the case for example for terms for individual
		  * columns of this term, as returned by [[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.split split]].
		  * @return `form.`[[net.noresttherein.oldsql.schema.SQLWriteForm.isUniversal isUniversal]].
		  */
		def isFormUniversal :Boolean = form.isUniversal

		override def groundValue :Opt[V] = Got(value)
		override def selectForm  :SQLReadForm[V] = form.reader

		/** Cached result of method [[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm.split split]]. */
		//this implementation is likely more efficient and robust than the commented out one, but the latter
		// attempts to create expressions actually mapping to the represented column, rather than simply translating
		// to the expected SQL - this initialization defines all columns as having type `T`.
		private lazy val columns :Seq[ColumnTerm[_]] = form match {
			case columnForm :ColumnForm[V] =>
				PassedArray :+ column(value, 0)(columnForm)
			case _ =>
				SQLWriteForm.const(value)(form).split.mapWithIndex { (write, i) =>
					val read = ColumnReadForm.const(write.sqlType)(value)
					column(value, i)(read <> write)
				}
		}
//		{
//			val columnValues = new RecordingPreparedStatement(form.columnCount)
//			form.set(columnValues, 1, value)
//			form.split.mapWithIndex { (write, i) =>
//				implicit val columnForm = { //can't check if form is ColumnForm because of contravariance issues
//					val readForm = ColumnReadForm.const(columnValues.parameterType(i + 1))(columnValues(i + 1))
//					write.castParam[Any] <> readForm
//				}
//				column(columnValues(i + 1), i)
//			}
//		}
		protected def column[X](value :X, index :Int)(implicit form :ColumnForm[X]) :ColumnTerm[X]

//		protected override def transform[X](conversion :SQLTransformation[V, X]) :SQLExpression[RowProduct, GlobalScope, X] =
//			if (conversion.isIdentity) conversion(this)
//			else reform(conversion(form), conversion)

		protected override def convert[X](conversion :SQLConversion[V, X]) :GroundSQL[X] =
			if (conversion.isIdentity)
				conversion(this)
			else if (conversion.isReversible && conversion.isLossless)
				reform(conversion(form), conversion)
			else
				super.convert(conversion)

		protected override def reformer[F1 <: RowProduct, S1 >: Grouped <: Single,
		                                F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                                   (implicit leftResult  :SQLTransformation[V, U],
	                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
				:SpecificExpressionVisitor
				 [F2, S2, V2, (leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
			new RWTermReformer[F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](form)(other)(reform, passCount)

//
//		override def reform(form :SQLForm[V]) :RWTerm[V]
//		override def reform[U](implicit form :SQLForm[U], conversion :SQLTransformation[V, U]) :RWTerm[U]
//		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//		                              Expr2 <: SQLExpression[F2, S2, V2] with ConvertingTemplate[F2, S2, V2, Expr2],
//		                              U, LeftRes <: SQLExpression[_ <: RowProduct, _ >: Grouped <: Single, U],
//		                              RightRes <: SQLExpression[F2, S2, U]]
//		                             (other :ConvertingTemplate[F2, S2, V2, Expr2])(reform :Reform, passCount :PassCount)
//		                             (implicit leftResult  :SpecificTransformation[V, U, GroundSQL[V], LeftRes],
//		                                       rightResult :SpecificTransformation[V2, U, Expr2, RightRes],
//		                                       spelling :SQLSpelling)
//				:(LeftRes, RightRes) =
//			if (passCount.mayPass)
//				super.reform(other)(reform, passCount)
//			else
//				reformer(other)(reform, passCount).apply(other)(rightResult)
//
//		protected override def reform[E <: RowProduct, C >: Grouped <: Single, X, U]
//		                             (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//		                             (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//				:(SQLExpression[RowProduct, Single, U], SQLExpression[E, C, U]) =
//			if (passesAllowed >= MayPassBack)
//				super.reform(other)(reform, passesAllowed)
//			else
//				reformer(other)(reform, passesAllowed).apply(other)
//
//		protected override def reformer[E <: RowProduct, C >: Grouped <: Single, X, U]
//		                               (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//		                               (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//				:SpecificExpressionVisitor[E, C, X, (SQLExpression[RowProduct, Single, U], SQLExpression[E, C, U])] =
//			new TermReformer[E, C, X, U](form)(other)(reform, passesAllowed)

		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[RowProduct, Single, _]] =
			try { columns } catch {
				case e :Exception => throw new InseparableExpressionException(
					this, "Failed to split multi column term " + this + ": " + e.getMessage + ".", e
				)
			}
	}


	object RWTerm {
		/** A mixin trait for implementations of [[net.noresttherein.oldsql.sql.ast.SQLTerm.RWTerm RWTerm]]
		  * which preserve their type constructor during conversion. Note that `RWTerm` itself
		  * does ''not'' extend this trait, as [[net.noresttherein.oldsql.sql.ast.MappingTerm MappingTerm]]
		  * creates a [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]] on conversion instead.
		  */ //consider: moving it out, it does not require being an RWTerm
		trait ConvertingRWTermTemplate[V, +T[X] <: SQLTerm[X] with SQLTermTemplate[V, T, T, C], +C[X] <: T[X]]
			extends ConvertingTemplate[RowProduct, Single, V, T]
			   with GroundSQLTemplate[V, T[V]]
			   with SQLTermTemplate[V, T, T, C]
		{ this :T[V] =>
			val form :SQLForm[V]

			protected override def convert[X](conversion :SQLConversion[V, X]) :T[X] =
				(this :ConvertingRWTermTemplate[V, T, C]).reform(conversion(form), conversion)

			protected override def reformer[F1 <: RowProduct, S1 >: Grouped <: Single,
			                                F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                                   (implicit leftResult  :SQLTransformation[V, U],
		                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
					:SpecificExpressionVisitor
					 [F2, S2, V2, (leftResult.SQLResult[F1, S1, T[U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
				new RWTermReformer[F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](form)(other)(reform, passCount)
		}


		trait SpecificRWTermVisitor[X, +Y] extends SpecificRWColumnTermVisitor[X, Y]
			with SpecificLiteralVisitor[X, Y] with SpecificBoundParamVisitor[X, Y] with SpecificMappingTermVisitor[X, Y]
		{
			def rwTerm(e :RWTerm[X]) :Y
		}
		trait MatchSpecificRWTerm[X, +Y] extends SpecificRWTermVisitor[X, Y] with CaseSpecificLiteral[X, Y] with CaseSpecificBoundParam[X, Y] {
			//we don't extend CaseRWTerm because we want MappingLiteral and BoundMappingParam to be handled as literals and params
			override def columnMappingTerm[M[A] <: BaseColumn[X, A]](e :ColumnMappingTerm[M, X]) :Y = mappingTerm(e)
			override def rwColumnTerm(e :RWColumnTerm[X]) :Y = rwTerm(e)
		}
		trait CaseSpecificRWTerm[X, +Y] extends MatchSpecificRWTerm[X, Y] {
			override def literal(e :SQLLiteral[X]) :Y = rwTerm(e)
			override def param(e :BoundParam[X]) :Y = rwTerm(e)
			override def mappingTerm[M[A] <: BaseMapping[X, A]](e :MappingTerm[M, X]) :Y = rwTerm(e)
		}
//
//
//		trait RWTermVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//			extends RWColumnTermVisitor[Y] with LiteralVisitor[Y] with BoundParamVisitor[Y] with MappingTermVisitor[Y]
//		{
//			def rwTerm[V](e :RWTerm[V]) :Y[Single, V, RWTerm[V]]
//		}
//		trait MatchRWTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//			extends RWTermVisitor[Y] with CaseLiteral[Y] with CaseBoundParam[Y]
//		{
//			override def rwColumnTerm[V](e :RWColumnTerm[V]) :Y[Single, V, RWColumnTerm[V]] = rwTerm(e)
//			override def columnMappingTerm[M[A] <: BaseColumn[V, A], V]
//			                              (e :ColumnMappingTerm[M, V]) :Y[Single, V, ColumnMappingTerm[M, V]] =
//				mappingTerm(e)
//		}
//		trait CaseRWTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//			extends MatchRWTerm[Y]
//		{
//			override def literal[V](e :SQLLiteral[V]) :Y[Single, V, SQLLiteral[V]] = rwTerm(e)
//			override def param[V](e :BoundParam[V]) :Y[Single, V, BoundParam[V]] = rwTerm(e)
//			override def mappingTerm[M[A] <: BaseMapping[V, A], V]
//			                        (e :MappingTerm[M, V]) :Y[Single, V, MappingTerm[M, V]] = rwTerm(e)
//		}


		trait AnyRWTermVisitor[+Y[-_ >: Grouped <: Single, _]] extends AnyRWColumnTermVisitor[Y]
			with AnyLiteralVisitor[Y] with AnyBoundParamVisitor[Y] with AnyMappingTermVisitor[Y]
		{
			def rwTerm[X](e :RWTerm[X]) :Y[Single, X]
		}

		trait MatchAnyRWTerm[+Y[-_ >: Grouped <: Single, _]] extends AnyRWTermVisitor[Y]
			with CaseAnyLiteral[Y] with CaseAnyBoundParam[Y]
		{
			//we don't extend CaseAnyRWTerm because we want MappingLiteral and BoundMappingParam to be handled as literals and params
			override def columnMappingTerm[M[A] <: BaseColumn[X, A], X](e :ColumnMappingTerm[M, X]) :Y[Single, X] =
				mappingTerm(e)
			override def rwColumnTerm[X](e :RWColumnTerm[X]) :Y[Single, X] = rwTerm(e)
		}

		trait CaseAnyRWTerm[+Y[- _ >: Grouped <: Single, _]] extends MatchAnyRWTerm[Y] {
			override def literal[X](e :SQLLiteral[X]) :Y[Single, X] = rwTerm(e)
			override def param[X](e :BoundParam[X]) :Y[Single, X] = rwTerm(e)
			override def mappingTerm[M[A] <: BaseMapping[X, A], X](e :MappingTerm[M, X]) :Y[Single, X] =
				rwTerm(e)
		}
	}




	/** A placeholder expression intended to be either replaced or never used.
	  * Most methods throw a [[UnsupportedOperationException]] or [[net.noresttherein.oldsql.exceptions.Bug Bug]].
	  *///currently unused
	class ErrorTerm[V](val msg :String) extends SQLTerm[V] {
		override def selectForm :SQLReadForm[V] =
			throw new UnsupportedOperationException("No form for error expression " + this + ".")

		override def groundValue :Opt[V] = Lack

		override def reform[U](form :SQLForm[U], conversion :SQLConversion[V, U]) :SQLTerm[U] = form match {
			case _ :ColumnForm[_] => ErrorColumnTerm(msg)
			case _ => ErrorTerm(msg)
		}
		override def reform[U](form :ColumnForm[U], conversion :SQLConversion[V, U]) :ColumnTerm[U] =
			ErrorColumnTerm(msg)


		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[RowProduct, Single, _]] = Nil

		protected override def defaultSpelling[P](from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			throw Bug("Cannot spell an error expression: " + this + ".")

		protected override def explodedSpelling[P](independent :Boolean)
		                                          (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
		                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
			PassedArray.empty

		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyExpressionVisitor[RowProduct, Y]) :Y[Single, V] =
			visitor.error(this)

		protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, V, Y]) :Y =
			visitor.error(this)
//
//		protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//		                             E >: SQLTerm[V] <: SQLExpression[F_, S_, V],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ExpressionVisitor[RowProduct, Y]) :Y[S_, V, E] =
//			visitor.error(this)

		override def toString :String = "ERROR(" + msg + ")"
	}


	object ErrorTerm {
		def apply[V](msg :String) :ErrorTerm[V] = new ErrorTerm(msg)

		def unapply(e :SQLExpression.__) :Opt[String] = e match {
			case term :ErrorTerm[_] => Got(term.msg)
			case _ => Lack
		}

		trait SpecificErrorTermVisitor[X, +Y] extends SpecificErrorColumnTermVisitor[X, Y] {
			def error(e :ErrorTerm[X]) :Y
		}
		trait MatchSpecificErrorTerm[X, +Y] extends SpecificErrorTermVisitor[X, Y] with CaseSpecificErrorColumnTerm[X, Y] {
			override def errorColumn(e :ErrorColumnTerm[X]) :Y = error(e)
		}
		type CaseSpecificErrorTerm[X, +Y] = MatchSpecificErrorTerm[X, Y]
//
//
//		trait ErrorTermVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//			extends ErrorColumnTermVisitor[Y]
//		{
//			def error[V](e :ErrorTerm[V]) :Y[Single, V, ErrorTerm[V]]
//		}
//		trait MatchErrorTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//			extends ErrorTermVisitor[Y]
//		{
//			override def errorColumn[V](e :ErrorColumnTerm[V]) :Y[Single, V, ErrorColumnTerm[V]] = error(e)
//		}
//		type CaseErrorTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//			MatchErrorTerm[Y]


		trait AnyErrorTermVisitor[+Y[-_ >: Grouped <: Single, _]]
			extends AnyErrorColumnTermVisitor[Y]
		{
			def error[X](e :ErrorTerm[X]) :Y[Single, X]
		}
		trait MatchAnyErrorTerm[+Y[-_ >: Grouped <: Single, _]]
			extends AnyErrorTermVisitor[Y] with CaseAnyErrorColumnTerm[Y]
		{
			override def errorColumn[X](e :ErrorColumnTerm[X]) :Y[Single, X] = error(e)
		}
		type CaseAnyErrorTerm[+Y[-_ >: Grouped <: Single, _]] = MatchAnyErrorTerm[Y]
	}


	trait SpecificTermVisitor[X, +Y] extends SpecificColumnTermVisitor[X, Y]
		with SpecificRWTermVisitor[X, Y] with SpecificMappingTermVisitor[X, Y]
		with SpecificMultiNullVisitor[X, Y] with SpecificNativeTermVisitor[X, Y]
		with SpecificErrorTermVisitor[X, Y] with SpecificEmptyVisitor[X, Y]
	{
		def term(e :SQLTerm[X]) :Y
	}
	trait MatchSpecificTerm[X, +Y]
		extends SpecificTermVisitor[X, Y] with CaseSpecificRWTerm[X, Y] with CaseSpecificMultiNull[X, Y]
		   with CaseSpecificNativeTerm[X, Y] with CaseSpecificErrorTerm[X, Y]
	{
		override def columnTerm(e :ColumnTerm[X]) :Y = term(e)
	}
	trait CaseSpecificTerm[X, +Y] extends MatchSpecificTerm[X, Y] {
		override def rwTerm(e :RWTerm[X]) :Y = term(e)
		override def multiNull(e :MultiNull[X]) :Y = term(e)
		override def native(e :NativeTerm[X]) :Y = term(e)
		override def empty(implicit isEmpty :X =:= @~) :Y = term(isEmpty.substituteContra(EmptySQL :SQLTerm[@~]))
		override def error(e :ErrorTerm[X]) :Y = term(e)
		override def mappingTerm[M[A] <: BaseMapping[X, A]](e :MappingTerm[M, X]) :Y = term(e)
	}
//
//
//	trait TermVisitor[+Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[RowProduct, S, v], -E <: EC[V]]]
//		extends ColumnTermVisitor[Y] with RWTermVisitor[Y] with MappingTermVisitor[Y]
//		   with MultiNullVisitor[Y] with NativeTermVisitor[Y] with ErrorTermVisitor[Y]
//	{
//		def term[V](e :SQLTerm[V]) :Y[Single, V, SQLTerm, SQLTerm[V]]
//	}
//	trait MatchTerm[+Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[RowProduct, S, v], -E <: EC[V]]]
//		extends TermVisitor[Y] with CaseRWTerm[Y] with CaseMultiNull[Y] with CaseNativeTerm[Y] with CaseErrorTerm[Y]
//	{
//		override def columnTerm[V](e :ColumnTerm[V]) :Y[Single, V, ColumnTerm[V]] = term(e)
//	}
//	trait CaseTerm[+Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[RowProduct, S, v], -E <: EC[V]]]
//		extends MatchTerm[Y]
//	{
//		override def rwTerm[V](e :RWTerm[V]) :Y[Single, V, RWTerm[V]] = term(e)
//		override def multiNull[V](e :MultiNull[V]) :Y[Single, V, MultiNull[V]] = term(e)
//		override def native[V](e :NativeTerm[V]) :Y[Single, V, NativeTerm[V]] = term(e)
//		override def error[V](e :ErrorTerm[V]) :Y[Single, V, ErrorTerm[V]] = term(e)
//		override def mappingTerm[M[A] <: BaseMapping[V, A], V]
//		                        (e :MappingTerm[M, V]) :Y[Single, V, MappingTerm[M, V]] = term(e)
//	}


	trait AnyTermVisitor[+Y[-_ >: Grouped <: Single, _]] extends AnyColumnTermVisitor[Y]
		with AnyRWTermVisitor[Y] with AnyMappingTermVisitor[Y] with AnyMultiNullVisitor[Y] with AnyNativeTermVisitor[Y]
		with AnyErrorTermVisitor[Y] with AnyEmptyVisitor[Y]
	{
		def term[X](e :SQLTerm[X]) :Y[Single, X]
	}

	trait MatchAnyTerm[+Y[-_ >: Grouped <: Single, _]] extends AnyTermVisitor[Y]
		with CaseAnyRWTerm[Y]with CaseAnyMultiNull[Y] with CaseAnyNativeTerm[Y] with CaseAnyErrorTerm[Y]
	{
		override def columnTerm[X](e :ColumnTerm[X]) :Y[Single, X] = term(e :SQLTerm[X])
	}

	trait CaseAnyTerm[+Y[-_ >: Grouped <: Single, _]] extends MatchAnyTerm[Y] {
		override def rwTerm[X](e :RWTerm[X]) :Y[Single, X] = term(e)
		override def multiNull[X](e: MultiNull[X]): Y[Single, X] = term(e)
		override def native[X](e: NativeTerm[X]): Y[Single, X] = term(e)
		override def empty = term(EmptySQL)
		override def error[X](e :ErrorTerm[X]) :Y[Single, X] = term(e)
		override def mappingTerm[M[A] <: BaseMapping[X, A], X](e :MappingTerm[M, X]) :Y[Single, X] = term(e)
	}

}





trait ColumnTerm[V]
	extends SQLTerm[V] with ColumnSQL[RowProduct, Single, V]
//	   with ConvertingTemplate[RowProduct, GlobalScope, V, ColumnTerm]
	   with SQLTermTemplate[V, GroundSQL, SQLTerm, ColumnTerm] with GroundSQLTemplate[V, ColumnTerm[V]]
{
//	override def reform[U](implicit form :SQLForm[U], conversion :SQLTransformation[V, U]) :SQLTerm[U] =
//		form match {
//			case column :ColumnForm[U] = reform(column, conversion)
//			case _ => super.reform[U]
//		}
//	def reform[U](implicit form :ColumnForm[U], conversion :SQLTransformation[V, U]) :ColumnTerm[U]
//
//	override def anchor(from :RowProduct) :ColumnTerm[V] = this
//
//	override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnTerm[V] =
//		this
//
//	override def expand[U <: RowProduct, E <: RowProduct]
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :ColumnTerm[V] =
//		this

//		override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//		                    (matcher :AnyColumnVisitor[RowProduct, Y]) :Y[GlobalScope, T] =
//			matcher.term(this)

	protected[sql] override def atomicSpelling[P]
	                            (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                            (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(this)(from, context, params)
}


object ColumnTerm {
//
//	trait ColumnTermTemplate[V, T[v] <: SQLTerm[v] with SQLTermTemplate[v, T, C],
//	                         C[v] <: ColumnTerm[v] with T[v] with ColumnTermTemplate[v, T, C]]
//		extends SQLTermTemplate[V, T, C]
////		   with ConvertingColumnTemplate[RowProduct, Single, V, C]
//		   with GroundSQLTemplate[V, C[V]]
//	{ this :C[V] =>
//		abstract override def reform[U](form :SQLForm[U], conversion :SQLConversion[V, U]) :T[U] =
//			form match {
//				case column :ColumnForm[U] => (this :ColumnTermTemplate[V, T, C]).reform(column, conversion)
//				case _ => super.reform(form, conversion)
//			}
////
////		def reform[U](implicit form :ColumnForm[U], conversion :SQLConversion[V, U]) :C[U]
//	}

	trait RWColumnTerm[V]
		extends RWTerm[V] with ColumnTerm[V]
		   with SQLTermTemplate[V, GroundSQL, RWTerm, RWColumnTerm] with GroundSQLTemplate[V, RWColumnTerm[V]]
	{
		override val form       :ColumnForm[V]
		override def selectForm :ColumnReadForm[V] = form.reader
//
//		protected override def transform[X](conversion :SQLTransformation[V, X]) :ColumnSQL[RowProduct, GlobalScope, X] =
//			if (conversion.isIdentity) conversion(this)
//			else reform(conversion(form), conversion)

		protected override def convert[X](conversion :SQLConversion[V, X]) :GroundColumn[X] =
			if (conversion.isIdentity)
				conversion(this)
			else if (conversion.isReversible && conversion.isLossless)
				reform(conversion(form), conversion)
			else
				super[ColumnTerm].convert(conversion)

//		override def reform[U](implicit form :ColumnForm[U], conversion :SQLConversion[V, U]) :RWColumnTerm[U]
	}

	object RWColumnTerm {
		/** A mixin trait for implementations of [[net.noresttherein.oldsql.sql.ast.ColumnTerm.RWColumnTerm RWColumnTerm]]
		  * which preserve their type constructor during conversion. Note that `RWTerm` itself
		  * does ''not'' extend this trait, as [[net.noresttherein.oldsql.sql.ast.ColumnMappingTerm ColumnMappingTerm]]
		  * creates a [[net.noresttherein.oldsql.sql.ast.ColumnMappingSQL ColumnMappingSQL]] on conversion instead.
		  */
		trait ConvertingRWColumnTermTemplate[V, +T[X] <: SQLTerm[X] with SQLTermTemplate[V, T, T, C],
		                                     +C[X] <: T[X] with ConvertingTemplate[RowProduct, Single, V, C]]
			extends ConvertingTemplate[RowProduct, Single, V, C]
			   with GroundSQLTemplate[V, C[V]]
			   with ConvertingRWTermTemplate[V, T, C]
		{ this :C[V] =>
			override val form :ColumnForm[V]
			//looks the same as ConvertingRWTermTemplate, but form here is a ColumnForm and a different method is called
			protected override def convert[X](conversion :SQLConversion[V, X]) :C[X] =
				(this :ConvertingRWColumnTermTemplate[V, T, C]).reform(conversion(form), conversion)
		}

		trait SpecificRWColumnTermVisitor[X, +Y] extends SpecificColumnMappingTermVisitor[X, Y]
			with SpecificColumnLiteralVisitor[X, Y] with SpecificBoundColumnParamVisitor[X, Y]
		{
			def rwColumnTerm(e :RWColumnTerm[X]) :Y
		}
		trait MatchSpecificRWColumnTerm[X, +Y] extends SpecificRWColumnTermVisitor[X, Y]
			with CaseSpecificColumnLiteral[X, Y] with CaseSpecificBoundColumnParam[X, Y]

		trait CaseSpecificRWColumnTerm[X, +Y] extends MatchSpecificRWColumnTerm[X, Y] {
			override def columnMappingTerm[M[A] <: BaseColumn[X, A]](e :ColumnMappingTerm[M, X]) :Y = rwColumnTerm(e)
			override def paramColumn(e :BoundColumnParam[X]) :Y = rwColumnTerm(e)
			override def columnLiteral(e :ColumnLiteral[X]) :Y = rwColumnTerm(e)
		}
//
//
//		trait RWColumnTermVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//			extends ColumnMappingTermVisitor[Y] with ColumnLiteralVisitor[Y] with BoundColumnParamVisitor[Y]
//		{
//			def rwColumnTerm[V](e :RWColumnTerm[V]) :Y[Single, V, RWColumnTerm[V]]
//		}
//		trait MatchRWColumnTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//			extends RWColumnTermVisitor[Y] with CaseColumnLiteral[Y] with CaseBoundColumnParam[Y]
//
//		trait CaseRWColumnTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//			extends MatchRWColumnTerm[Y]
//		{
//			override def columnLiteral[V](e :ColumnLiteral[V]) :Y[Single, V, ColumnLiteral[V]] = rwColumnTerm(e)
//			override def paramColumn[V](e :BoundColumnParam[V]) :Y[Single, V, BoundColumnParam[V]] = rwColumnTerm(e)
//			override def columnMappingTerm[M[A] <: BaseColumn[V, A], V]
//			                              (e :ColumnMappingTerm[M, V]) :Y[Single, V, ColumnMappingTerm[M, V]]
//		}


		trait AnyRWColumnTermVisitor[+Y[-_ >: Grouped <: Single, _]]
			extends AnyColumnMappingTermVisitor[Y] with AnyColumnLiteralVisitor[Y] with AnyBoundColumnParamVisitor[Y]
		{
			def rwColumnTerm[X](e :RWColumnTerm[X]) :Y[Single, X]
		}
		trait MatchAnyRWColumnTerm[+Y[-_ >: Grouped <: Single, _]] extends AnyRWColumnTermVisitor[Y]
			with CaseAnyColumnLiteral[Y] with CaseAnyBoundColumnParam[Y]

		trait CaseAnyRWColumnTerm[+Y[-_ >: Grouped <: Single, _]] extends MatchAnyRWColumnTerm[Y] {
			override def columnMappingTerm[M[A] <: BaseColumn[X, A], X](e :ColumnMappingTerm[M, X]) :Y[Single, X] =
				rwColumnTerm(e)
			override def columnLiteral[X](e :ColumnLiteral[X])  :Y[Single, X] = rwColumnTerm(e)
			override def paramColumn[X](e :BoundColumnParam[X]) :Y[Single, X] = rwColumnTerm(e)
		}
	}



	//consider: moving to ast
	class ErrorColumnTerm[V](override val msg :String) extends ErrorTerm[V](msg) with ColumnTerm[V] {
		override def selectForm :ColumnReadForm[V] =
			throw new UnsupportedOperationException("No form for error expression " + this + ".")

		override def reform[U](implicit form :ColumnForm[U], conversion :SQLConversion[V, U]) :ColumnTerm[U] =
			this.castParam[U]

		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyColumnVisitor[RowProduct, Y]) :Y[Single, V] =
			visitor.errorColumn(this)

		protected override def visit[Y](visitor :SpecificColumnVisitor[RowProduct, Single, V, Y]) :Y =
			visitor.errorColumn(this)
//
//		protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//		                             E >: ColumnTerm[V] <: SQLExpression[F_, S_, V],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, E] =
//			visitor.errorColumn(this)
	}


	object ErrorColumnTerm {
		def apply[X](msg :String) :ErrorColumnTerm[X] = new ErrorColumnTerm(msg)
		def unapply(e :SQLExpression.__) :Opt[String] = e match {
			case term :ErrorColumnTerm[_] => Got(term.msg)
			case _ => Lack
		}

		trait SpecificErrorColumnTermVisitor[X, +Y] {
			def errorColumn(e :ErrorColumnTerm[X]) :Y
		}
		type MatchSpecificErrorColumnTerm[X, +Y] = SpecificErrorColumnTermVisitor[X, Y]
		type CaseSpecificErrorColumnTerm[X, +Y] = SpecificErrorColumnTermVisitor[X, Y]
//
//		trait ErrorColumnTermVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] {
//			def errorColumn[V](e :ErrorColumnTerm[V]) :Y[Single, V, ErrorColumnTerm[V]]
//		}
//		type MatchErrorColumnTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//			ErrorColumnTermVisitor[Y]
//		type CaseErrorColumnTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//			ErrorColumnTermVisitor[Y]

		trait AnyErrorColumnTermVisitor[+Y[-_ >: Grouped <: Single, _]] {
			def errorColumn[X](e :ErrorColumnTerm[X]) :Y[Single, X]
		}
		type MatchAnyErrorColumnTerm[+Y[-_ >: Grouped <: Single, _]] = AnyErrorColumnTermVisitor[Y]
		type CaseAnyErrorColumnTerm[+Y[-_ >: Grouped <: Single, _]] = AnyErrorColumnTermVisitor[Y]
	}



	trait SpecificColumnTermVisitor[X, +Y]
		extends SpecificRWColumnTermVisitor[X, Y] with SpecificNullVisitor[X, Y]
		   with SpecificNativeColumnTermVisitor[X, Y] with SpecificErrorColumnTermVisitor[X, Y]
	{
		def columnTerm(e :ColumnTerm[X]) :Y
	}
	trait MatchSpecificColumnTerm[X, +Y] extends SpecificColumnTermVisitor[X, Y]
		with CaseSpecificRWColumnTerm[X, Y] with CaseSpecificNull[X, Y]
		with CaseSpecificNativeColumnTerm[X, Y] with CaseSpecificErrorColumnTerm[X, Y]

	trait CaseSpecificColumnTerm[X, +Y] extends MatchSpecificColumnTerm[X, Y] {
		override def rwColumnTerm(e :RWColumnTerm[X]) :Y = columnTerm(e)
		override def nullColumn(e :SQLNull[X]) :Y = columnTerm(e)
		override def nativeColumn(e :NativeColumnTerm[X]) :Y = columnTerm(e)
		override def errorColumn(e :ErrorColumnTerm[X]) :Y = columnTerm(e)
	}
//
//
//	trait ColumnTermVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends RWColumnTermVisitor[Y] with NullVisitor[Y] with NativeColumnTermVisitor[Y] with ErrorColumnTermVisitor[Y]
//	{
//		def columnTerm[V](e :ColumnTerm[V]) :Y[Single, V, ColumnTerm[V]]
//	}
//	trait MatchColumnTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends ColumnTermVisitor[Y]
//		   with CaseRWColumnTerm[Y] with CaseNull[Y] with CaseNativeColumnTerm[Y] with CaseErrorColumnTerm[Y]
//
//	trait CaseColumnTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MatchColumnTerm[Y]
//	{
//		override def rwColumnTerm[V](e :RWColumnTerm[V]) :Y[Single, V, RWColumnTerm[V]] = columnTerm(e)
//		override def nullColumn[V](e :SQLNull[V]) :Y[Single, V, SQLNull[V]] = columnTerm(e)
//		override def nativeColumn[V](e :NativeColumnTerm[V]) :Y[Single, V, NativeColumnTerm[V]] = columnTerm(e)
//		override def errorColumn[V](e :ErrorColumnTerm[V]) :Y[Single, V, ErrorColumnTerm[V]] = columnTerm(e)
//		override def columnMappingTerm[M[A] <: BaseColumn[V, A], V]
//		                              (e :ColumnMappingTerm[M, V]) :Y[Single, V, ColumnMappingTerm[M, V]] =
//			columnTerm(e)
//	}

	trait AnyColumnTermVisitor[+Y[-_ >: Grouped <: Single, _]]
		extends AnyRWColumnTermVisitor[Y] with AnyNullVisitor[Y]
		   with AnyNativeColumnTermVisitor[Y] with AnyErrorColumnTermVisitor[Y]
	{
		def columnTerm[X](e :ColumnTerm[X]) :Y[Single, X]
	}
	trait MatchAnyColumnTerm[+Y[-_ >: Grouped <: Single, _]] extends AnyColumnTermVisitor[Y]
		with CaseAnyRWColumnTerm[Y] with CaseAnyNull[Y]
		with CaseAnyNativeColumnTerm[Y] with CaseAnyErrorColumnTerm[Y]

	trait CaseAnyColumnTerm[+Y[-_ >: Grouped <: Single, _]] extends MatchAnyColumnTerm[Y] {
		override def rwColumnTerm[X](e :RWColumnTerm[X]) :Y[Single, X] = columnTerm(e)
		override def nullColumn[X](e :SQLNull[X]) :Y[Single, X] = columnTerm(e)
		override def nativeColumn[X](e :NativeColumnTerm[X]) :Y[Single, X] = columnTerm(e)
		override def errorColumn[X](e :ErrorColumnTerm[X]) :Y[Single, X] = columnTerm(e)
	}
}






/** An SQL term of shape specified not by a fixed [[net.noresttherein.oldsql.schema.SQLForm SQLForm]],
  * but by a [[net.noresttherein.oldsql.schema.Mapping Mapping]] for its value type. Unlike in its base trait,
  * the effective column set (and form) is [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope spelling scope]]
  * specific.
  */ //todo: make it a SelectableMappingSQL
trait MappingTerm[M[A] <: BaseMapping[V, A], V]
	extends RWTerm[V] with SQLTerm[V] with MappingSQL[RowProduct, Single, M, V]
	   with MappingTermTemplate[M, V, MappingTerm[M, V]]
{
//	@deprecated val project :IsomorphicProjection[M, V, Origin]

	override val form       :SQLForm[V] = export.selectForm <> export.writeForm(export.selectedByDefault)
	override def selectForm :SQLReadForm[V] = export.selectForm
	override def conversion :SQLConversion[V, V] = SQLConversion.toSelf

	def scopedForm(implicit spelling :SQLSpelling) :SQLForm[V] = spelling.scope.termForm(export)

//	override type ComponentLike[C[A] <: BaseMapping[X, A], X] <:
//		MappingTerm[C, X] { type Origin = MappingTerm.this.Origin }
//
//	override type ColumnLike[C[A] <: BaseColumn[X, A], X] <: ColumnMappingTerm[C, X] with ComponentLike[C, X]
//
	override def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
			:MappingTerm[project.WithOrigin, X] { type Origin = MappingTerm.this.Origin }

	override def \[K <: ColumnAt[Origin], X]
	              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[O] <: BaseColumn[X, O] })
			:ColumnMappingTerm[project.WithOrigin, X] { type Origin = MappingTerm.this.Origin }


	override def isDefault :Boolean = mapping == export

//	protected override def convert[X](conversion :SQLConversion[V, X]) :MappingSQL[RowProduct, Single, M, X] =
//		ConvertedMappingSQL(this, conversion)
//	protected override def map[Y](lift :Lift[V, Y]) :MappingSQL[RowProduct, GlobalScope, M, Y] =
//		ConvertedMappingSQL(this, lift)
//
//	protected override def reform[E <: RowProduct, C[A] <: MappingAt[A], U]
//	                             (other :ComponentSQL[E, C])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[V, U], rightResult :Lift[other.Subject, U], spelling :SQLSpelling)
//			:(SQLExpression[RowProduct, Single, U], reform.LValue[E, C, U]) =
//		if (passesAllowed >= MayPassBack)
//			super[SQLTerm].reform(other)(reform, passesAllowed)
//		else if (other.anchored isomorphic export)
//			(leftResult(this), reform.lift(rightResult(other)))
//		else if (other.mapping isomorphic mapping)
//			//We could eliminate a cast by applying rightResult to a new MappingTerm[C, other.Subject]
//			//  but this would cause bugs if we introduced Lifts which create non standard conversions.
//			(leftResult(alter(other.anchored.castParams[V, Origin])), reform.lift(rightResult(other)))
//		else
//			super[SQLTerm].reform(other)(reform, passesAllowed)

//	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[RowProduct, GlobalScope, _]] =
//		spelling.scope.defaultColumns(export).toSeq.map(this \  _)

	protected override def reformer[F1 <: RowProduct, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                   (implicit leftResult  :SQLTransformation[V, U],
                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.SQLResult[F1, S1, MappingSQL[F1, S1, M, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
		new TermReformer[F1, S1, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](form)(other)(reform, passCount) {
			override def mapping[M[O] <: MappingAt[O]](e :MappingSQL[F2, S2, M, V2]) =
				if (e.anchored isomorphic mapping)
					asIs
				else if (reform.mayAlterLeft && (e.mapping isomorphic mapping))
					(leftResult(e.conversion(alter(e.anchored.castParams[V, Origin]))), rightResult(other))
				else
					super.mapping(e)
		}

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[RowProduct]#rows[Single]#__] = {
		def columnTerm[X](column :TypedColumn[X, Origin]) = this \ column
		spelling.scope.defaultColumns(export).toSeq.map(columnTerm(_))
	}

	protected override def shape(implicit spelling :SQLSpelling) :RowShape =
		spelling.scope.termForm(export).shape

	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		spelling.scope.defaultColumns(export).size

	protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.mappingTerm(this)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.mappingTerm(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: MappingTerm[M, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.mappingTerm(this)

	override def equals(that :Any) :Boolean = that match {
		case self  :AnyRef if self eq this => true
		case other :MappingTerm[M @unchecked, _] if canEqual(other) && (other canEqual this) =>
			value == other.value && (mapping isomorphic other.mapping)
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[MappingTerm[M @unchecked, _]]
	override def hashCode :Int = value.hashCode * 31 + mapping.hashCode
}


private[ast] sealed abstract class Rank1MappingTermImplicits {
	implicit def implicitMappingMapping[K <: Mapping, V](implicit mapping :K, project :OriginProjection[K, V])
			:MappingInstance[project.WithOrigin, V] =
		new MappingInstance(mapping.withOrigin[Any])
}


object MappingTerm extends Rank1MappingTermImplicits {
	def unapply[V](e :SQLExpression[Nothing, Grouped, V]) :Opt[(V, MappingOf[V])] =
		e match {
			case literal :MappingTerm[MappingOf[V]#TypedProjection @unchecked, V @unchecked] =>
				Got(literal.value, literal.mapping)
			case _ => Lack
		}

	/** A mixin trait for [[net.noresttherein.oldsql.sql.ast.MappingTerm MappingTerm]] subclasses introducing
	  * methods returning another `MappingTerm` of the same type.
	  */
	trait MappingTermTemplate[M[A] <: BaseMapping[V, A], V, +E <: MappingTerm[M, V]]
		extends SQLTerm[V]
		   with GroundSQLTemplate[V, E] with MappingSQLTemplate[RowProduct, Single, M, V, E]
	{ this :E =>
//		override type Origin = O
		val export  :TypedMapping[V, Origin]

		override def default :E = alter(mapping)

		override def defaultWith(includes :Unique[TypedMapping[_, Origin]],
		                         excludes :Unique[TypedMapping[_, Origin]]) :E =
			alter(mapping(includes, excludes))

		override def alter(includes :Iterable[TypedMapping[_, Origin]],
		                   excludes :Iterable[TypedMapping[_, Origin]]) :E =
			alter(export(includes, excludes))

		protected def alter(export :TypedMapping[V, Origin]) :E
	}



	class MappingInstance[M[A] <: BaseMapping[V, A], V] private[ast] (val mapping :M[_]) extends AnyVal {
		@inline def withOrigin[O] :M[O] = mapping.asInstanceOf[M[O]]
	}

	implicit def implicitRelationMapping[M[A] <: BaseMapping[V, A], V](implicit table :Relation[M])
			:MappingInstance[M, V] =
		new MappingInstance(table.row)


	trait SpecificMappingTermVisitor[X, +Y] extends SpecificColumnMappingTermVisitor[X, Y]
		with SpecificMappingLiteralVisitor[X, Y] with SpecificBoundMappingParamVisitor[X, Y]
	{
		def mappingTerm[M[A] <: BaseMapping[X, A]](e :MappingTerm[M, X]) :Y
	}
	trait MatchSpecificMappingTerm[X, +Y] extends SpecificMappingTermVisitor[X, Y]
		with CaseSpecificMappingLiteral[X, Y] with CaseSpecificBoundMappingParam[X, Y]
	{
		override def columnMappingTerm[M[A] <: BaseColumn[X, A]](e :ColumnMappingTerm[M, X]) :Y = mappingTerm(e)
	}
	trait CaseSpecificMappingTerm[X, +Y] extends MatchSpecificMappingTerm[X, Y] {
		override def mappingLiteral[M[A] <: BaseMapping[X, A]](e :MappingLiteral[M, X]) :Y = mappingTerm(e)
		override def mappingParam[M[A] <: BaseMapping[X, A]](e :BoundMappingParam[M, X]) :Y = mappingTerm(e)
	}
//
//
//	trait MappingTermVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends ColumnMappingTermVisitor[Y] with MappingLiteralVisitor[Y] with BoundMappingParamVisitor[Y]
//	{
//		def mappingTerm[M[A] <: BaseMapping[V, A], V](e :MappingTerm[M, V]) :Y[Single, V, MappingTerm[M, V]]
//	}
//	trait MatchMappingTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MappingTermVisitor[Y] with CaseMappingLiteral[Y] with CaseBoundMappingParam[Y]
//	{
//		override def columnMappingTerm[M[A] <: BaseColumn[V, A], V](e :ColumnMappingTerm[M, V])
//				:Y[Single, V, ColumnMappingTerm[M, V]] =
//			mappingTerm(e)
//	}
//	trait CaseMappingTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MatchMappingTerm[Y]
//	{
//		override def mappingLiteral[M[A] <: BaseMapping[V, A], V]
//		                           (e :MappingLiteral[M, V]) :Y[Single, V, MappingLiteral[M, V]] = mappingTerm(e)
//
//		override def mappingParam[M[A] <: BaseMapping[V, A], V]
//		                         (e :BoundMappingParam[M, V]) :Y[Single, V, BoundMappingParam[M, V]] =
//			mappingTerm(e)
//	}

	trait AnyMappingTermVisitor[+Y[-_ >: Grouped <: Single, _]] extends AnyColumnMappingTermVisitor[Y]
		with AnyMappingLiteralVisitor[Y] with AnyBoundMappingParamVisitor[Y]
	{
		def mappingTerm[M[A] <: BaseMapping[X, A], X](e :MappingTerm[M, X]) :Y[Single, X]
	}
	trait MatchAnyMappingTerm[+Y[-_ >: Grouped <: Single, _]]
		extends AnyMappingTermVisitor[Y] with CaseAnyMappingLiteral[Y] with CaseAnyBoundMappingParam[Y]
	{
		override def columnMappingTerm[M[A] <: BaseColumn[V, A], V](e :ColumnMappingTerm[M, V]) :Y[Single, V] =
			mappingTerm(e)
	}
	trait CaseAnyMappingTerm[+Y[-_ >: Grouped <: Single, _]] extends MatchAnyMappingTerm[Y] {
		override def mappingLiteral[M[A] <: BaseMapping[X, A], X](e :MappingLiteral[M, X]) :Y[Single, X] =
			mappingTerm(e)

		override def mappingParam[M[A] <: BaseMapping[X, A], X](e :BoundMappingParam[M, X]) :Y[Single, X] =
			mappingTerm(e)
	}
}



trait ColumnMappingTerm[M[A] <: BaseColumn[V, A], V]
	extends MappingTerm[M, V] with RWColumnTerm[V] with ColumnMappingSQL[RowProduct, Single, M, V]
	   with ColumnMappingTermTemplate[M, V, ColumnMappingTerm[M, V]]
{
	override val form :ColumnForm[V] = export.form
//
//	protected override def convert[X](conversion :SQLConversion[V, X]) :ColumnMappingSQL[RowProduct, Single, M, X] =
//		ConvertedColumnMappingSQL(this, conversion)

	override def scopedForm(implicit spelling :SQLSpelling) :ColumnForm[V] = export.form
}


object ColumnMappingTerm {
	trait ColumnMappingTermTemplate[M[A] <: BaseColumn[V, A], V, +C <: ColumnMappingTerm[M, V]]
		extends ColumnTerm[V] with MappingTermTemplate[M, V, C]
	{ this :C =>
		override def defaultWith(includes :Unique[TypedMapping[_, Origin]],
		                         excludes :Unique[TypedMapping[_, Origin]]) :C =
			alter(mapping(includes, excludes)) //looks the same as in overridden method, but mapping(...) returns a column

		override def alter(includes :Iterable[TypedMapping[_, Origin]],
		                   excludes :Iterable[TypedMapping[_, Origin]]) :C =
			alter(export(includes, excludes))
	}


	trait SpecificColumnMappingTermVisitor[V, +Y]
		extends SpecificColumnMappingLiteralVisitor[V, Y] with SpecificBoundColumnMappingParamVisitor[V, Y]
	{
		def columnMappingTerm[M[A] <: BaseColumn[V, A]](e :ColumnMappingTerm[M, V]) :Y
	}
	trait MatchSpecificColumnMappingTerm[V, +Y] extends SpecificColumnMappingTermVisitor[V, Y]
		with CaseSpecificColumnMappingLiteral[V, Y] with CaseSpecificBoundColumnMappingParam[V, Y]

	trait CaseSpecificColumnMappingTerm[V, +Y] extends MatchSpecificColumnMappingTerm[V, Y] {
		override def columnMappingLiteral[M[A] <: BaseColumn[V, A]](e :ColumnMappingLiteral[M, V]) :Y =
			columnMappingTerm(e)

		override def columnMappingParam[M[A] <: BaseColumn[V, A]](e :BoundColumnMappingParam[M, V]) :Y =
			columnMappingTerm(e)
	}
//
//
//	trait ColumnMappingTermVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends ColumnMappingLiteralVisitor[Y] with BoundColumnMappingParamVisitor[Y]
//	{
//		def columnMappingTerm[M[A] <: BaseColumn[V, A], V]
//		                     (e :ColumnMappingTerm[M, V]) :Y[Single, V, ColumnMappingTerm[M, V]]
//	}
//	trait MatchColumnMappingTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends ColumnMappingTermVisitor[Y] with CaseColumnMappingLiteral[Y] with CaseBoundColumnMappingParam[Y]
//
//	trait CaseColumnMappingTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MatchColumnMappingTerm[Y]
//	{
//		override def columnMappingLiteral[M[A] <: BaseColumn[V, A], V](e :ColumnMappingLiteral[M, V])
//		        :Y[Single, V, ColumnMappingLiteral[M, V]] =
//			columnMappingTerm(e)
//
//		override def columnMappingParam[M[A] <: BaseColumn[V, A], V](e :BoundColumnMappingParam[M, V])
//				:Y[Single, V, BoundColumnMappingParam[M, V]] =
//			columnMappingTerm(e)
//	}


	trait AnyColumnMappingTermVisitor[+R[-_ >: Grouped <: Single, _]]
		extends AnyColumnMappingLiteralVisitor[R] with AnyBoundColumnMappingParamVisitor[R]
	{
		def columnMappingTerm[M[A] <: BaseColumn[V, A], V](e :ColumnMappingTerm[M, V]) :R[Single, V]
	}
	trait MatchAnyColumnMappingTerm[+R[-_ >: Grouped <: Single, _]] extends AnyColumnMappingTermVisitor[R]
		with CaseAnyColumnMappingLiteral[R] with CaseAnyBoundColumnMappingParam[R]

	trait CaseAnyColumnMappingTerm[+R[-_ >: Grouped <: Single, _]] extends MatchAnyColumnMappingTerm[R] {
		override def columnMappingParam[M[A] <: BaseColumn[V, A], V](e :BoundColumnMappingParam[M, V]) :R[Single, V] =
			columnMappingTerm(e)

		override def columnMappingLiteral[M[A] <: BaseColumn[V, A], V](e :ColumnMappingLiteral[M, V]) :R[Single, V] =
			columnMappingTerm(e)
	}
}






/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] representing an SQL constant mapping
  * to a predefined Scala value of type `T`. Literal expressions do not translate to statement expressions,
  * but rather are embedded in the complete SQL.
  */
trait SQLLiteral[V]
	extends RWTerm[V] with SQLTermTemplate[V, GroundSQL, SQLLiteral, ColumnLiteral]
	   with SelectableSQL[RowProduct, Single, V]
{
	//type of Unit rather than Any to prevent accidental use for other values
	val writeForm :SQLWriteForm[Unit] = SQLWriteForm.const(value)(form.writer)

//	override def downcast[X](implicit lift :Lift[X, T]) :SQLExpression[RowProduct, GlobalScope, X] =
//		try { to(lift.inverse) } catch {
//			//the exception may not be a problem, for example if we try to convert any type to an SQL NULL
//			case _ :Exception => super.to(lift.inverse)
//		}

	protected override def column[X](value :X, index :Int)(implicit form :ColumnForm[X]) :ColumnTerm[X] =
		ColumnLiteral(value)

	override def reform[U](form :SQLForm[U], conversion :SQLConversion[V, U]) :SQLLiteral[U] =
		SQLLiteral(conversion(value))(form)

	override def reform[U](form :ColumnForm[U], conversion :SQLConversion[V, U]) :ColumnLiteral[U] =
		ColumnLiteral(conversion(value))(form)


	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		SpelledSQL(form.literal(value, spelling.isInline), context)

	protected override def explodedSpelling[P]
	                       (independent :Boolean)
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		if (form.columnCount == 1)
			PassedArray :+ SpelledSQL(form.literal(value), context)
		else
			form.columnLiterals(value).map(SpelledSQL(_, context))


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor: AnyExpressionVisitor[RowProduct, Y]): Y[Single, V] =
		visitor.literal(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.literal(this)

//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: SQLLiteral[V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.literal(this)
//
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLLiteral[_]]
//		override def sameAs(that :Any) :Boolean = that.isInstanceOf[SQLLiteral[_]]

	override def toString :String = String.valueOf(value)
}



object SQLLiteral extends FormBasedFactory[Self, SQLLiteral, ColumnLiteral] {
	//consider: making SQLNull an SQLLiteral and handle null values gracefully

	def apply[V :SQLForm](value :V) :SQLLiteral[V] = SQLForm[V] match {
		case column :ColumnForm[V] => ColumnLiteral[V](value)(column)
		case _ => new Impl(value)
	}

	def unapply[V](e :SQLExpression[Nothing, Grouped, V]) :Opt[V] = e match {
		case literal :SQLLiteral[V @unchecked] => Got(literal.value)
		case _ => Lack
	}

	private class Impl[V](override val value :V)(implicit override val form :SQLForm[V])
		extends SQLLiteral[V]

	protected override def generalResult[T :SQLForm](arg :T) :SQLLiteral[T] = SQLLiteral(arg)
	protected override def specificResult[T :ColumnForm](arg :T) :ColumnLiteral[T] = ColumnLiteral(arg)


	trait SpecificLiteralVisitor[X, +Y] extends SpecificColumnLiteralVisitor[X, Y] with SpecificMappingLiteralVisitor[X, Y] {
		def literal(e :SQLLiteral[X]) :Y
	}
	trait MatchSpecificLiteral[X, +Y] extends SpecificLiteralVisitor[X, Y] with CaseSpecificMappingLiteral[X, Y] {
		override def columnLiteral(e :ColumnLiteral[X])   :Y = literal(e)
	}
	trait CaseSpecificLiteral[X, +Y] extends MatchSpecificLiteral[X, Y] {
		override def mappingLiteral[M[A] <: BaseMapping[X, A]](e :MappingLiteral[M, X]) :Y = literal(e)
	}
//
//
//	trait LiteralVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends ColumnLiteralVisitor[Y] with MappingLiteralVisitor[Y]
//	{
//		def literal[V](e :SQLLiteral[V]) :Y[Single, V, SQLLiteral[V]]
//	}
//	trait MatchLiteral[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends LiteralVisitor[Y] with CaseMappingLiteral[Y]
//	{
//		override def columnLiteral[V](e :ColumnLiteral[V]) :Y[Single, V, ColumnLiteral[V]] = literal(e)
//	}
//	trait CaseLiteral[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MatchLiteral[Y]
//	{
//		override def mappingLiteral[M[A] <: BaseMapping[V, A], V]
//		                           (e :MappingLiteral[M, V]) :Y[Single, V, MappingLiteral[M, V]] =
//			literal(e)
//	}


	trait AnyLiteralVisitor[+Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnLiteralVisitor[Y] with AnyMappingLiteralVisitor[Y]
	{
		def literal[X](e :SQLLiteral[X]) :Y[Single, X]
	}
	trait MatchAnyLiteral[+Y[-_ >: Grouped <: Single, _]]
		extends AnyLiteralVisitor[Y] with CaseAnyMappingLiteral[Y]
	{
		override def columnLiteral[X](e :ColumnLiteral[X])   :Y[Single, X] = literal(e)
	}
	trait CaseAnyLiteral[+Y[-_ >: Grouped <: Single, _]] extends MatchAnyLiteral[Y] {
		override def mappingLiteral[M[A] <: BaseMapping[X, A], X](e :MappingLiteral[M, X]) :Y[Single, X] =
			literal(e)
	}
}




trait ColumnLiteral[V] extends SQLLiteral[V] with RWColumnTerm[V] with GroundSQLTemplate[V, ColumnLiteral[V]] {
	override val form :ColumnForm[V]
	override val writeForm  :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)(form.writer)

	override def &&[E <: RowProduct, O >: Grouped <: Single]
	               (other: ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		if (value) other else False

	override def ||[E <: RowProduct, O >: Grouped <: Single]
	               (other: ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		if (value) True else other

	override def unary_![E <: RowProduct, O >: Grouped <: Single]
	                    (implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		if (value) False else True


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.columnLiteral(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.columnLiteral(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: ColumnLiteral[V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.columnLiteral(this)
}


object ColumnLiteral {
	def apply[V :ColumnForm](value :V) :ColumnLiteral[V] = new Impl[V](value)

	def apply[V](form :ColumnForm[V])(value :V) :ColumnLiteral[V] = new Impl[V](value)(form)

	def unapply[V](e :SQLExpression[Nothing, Grouped, V]) :Opt[V] = e match {
		case literal :ColumnLiteral[V @unchecked] => Got(literal.value)
		case _ => Lack
	}

	private class Impl[V](override val value :V)(implicit override val form :ColumnForm[V]) extends ColumnLiteral[V]

	trait SpecificColumnLiteralVisitor[X, +Y] extends SpecificColumnMappingLiteralVisitor[X, Y] {
		def columnLiteral(e :ColumnLiteral[X]) :Y
	}
	trait CaseSpecificColumnLiteral[X, +Y]
		extends SpecificColumnLiteralVisitor[X, Y] with CaseSpecificColumnMappingLiteral[X, Y]
	{
		override def columnMappingLiteral[M[A] <: BaseColumn[X, A]](e :ColumnMappingLiteral[M, X]) :Y =
			columnLiteral(e)
	}
	type MatchSpecificColumnLiteral[X, +Y] = SpecificColumnLiteralVisitor[X, Y]
//
//
//	trait ColumnLiteralVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends ColumnMappingLiteralVisitor[Y]
//	{
//		def columnLiteral[V](e :ColumnLiteral[V]) :Y[Single, V, ColumnLiteral[V]]
//	}
//	type MatchColumnLiteral[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		ColumnLiteralVisitor[Y]
//
//	trait CaseColumnLiteral[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends ColumnLiteralVisitor[Y]
//	{
//		override def columnMappingLiteral[M[A] <: BaseColumn[V, A], V]
//		                                 (e :ColumnMappingLiteral[M, V]) :Y[Single, V, ColumnMappingLiteral[M, V]] =
//			columnLiteral(e)
//	}


	trait AnyColumnLiteralVisitor[+Y[-_ >: Grouped <: Single, _]] extends AnyColumnMappingLiteralVisitor[Y] {
		def columnLiteral[X](e :ColumnLiteral[X]) :Y[Single, X]
	}

	trait MatchAnyColumnLiteral[+Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnLiteralVisitor[Y]
	{
		def nonbool[X](e :ColumnLiteral[X]) :Y[Single, X]

		def bool(e :ColumnLiteral[Boolean]) :Y[Single, Boolean]

		override def columnLiteral[X](e: ColumnLiteral[X]): Y[Single, X] = e.value match {
			case _ :Boolean => bool(e.asInstanceOf[ColumnLiteral[Boolean]]).asInstanceOf[Y[Single, X]]
			case _ => nonbool(e)
		}
	}

	trait CaseAnyColumnLiteral[+Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnLiteralVisitor[Y] with CaseAnyColumnMappingLiteral[Y]
	{
		override def columnMappingLiteral[M[A] <: BaseColumn[V, A], V]
		                                 (e :ColumnMappingLiteral[M, V]) :Y[Single, V] =
			columnLiteral(e)
	}
}




/** A literal expression of shape specified not by a fixed [[net.noresttherein.oldsql.schema.SQLForm SQLForm]],
  * but by a [[net.noresttherein.oldsql.schema.Mapping Mapping]] for its value type. Unlike in its base class,
  * the effective column set (and form) is [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope spelling scope]]
  * specific. It works as if it was replaced during spelling with an `SQLLiteral[T]` using form
  * `spelling.scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.termForm termForm]]`(mapping)`.
  * If reformed, it behaves as a regular literal.
  */ //todo: make it a SelectableMappingSQL
sealed trait MappingLiteral[M[A] <: BaseMapping[V, A], V]
	extends SQLLiteral[V] with MappingTerm[M, V] with MappingTermTemplate[M, V, MappingLiteral[M, V]]
{
//	override type ComponentLike[C[A] <: BaseMapping[X, A], X] =
//		MappingLiteral[C, X] { type Origin = MappingLiteral.this.Origin }
//
//	override type ColumnLike[C[A] <: BaseColumn[X, A], X] =
//		ColumnMappingLiteral[C, X] { type Origin = MappingLiteral.this.Origin }

	override def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
			:MappingLiteral[project.WithOrigin, X] { type Origin = MappingLiteral.this.Origin } =
	{
		val projected = project[Origin](component) //give us the subject type
		MappingLiteral(mapping(projected)(value), projected, export.export(projected))
	}
	override def \[K <: ColumnAt[Origin], X]
	              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[O] <: BaseColumn[X, O] })
			:ColumnMappingLiteral[project.WithOrigin, X] { type Origin = MappingLiteral.this.Origin } =
	{
		val projected = project[Origin](column)
		ColumnMappingLiteral(projected, export.export(projected), mapping(projected)(value))
	}
//	override def \[X](column :TypedColumn[X, Origin]) :ColumnMappingLiteral[MappingOf[X]#ColumnProjection, X] = {
//		val extract = export(column)
//		ColumnMappingLiteral(column, extract.export, extract(value))
//	}

	def \[K <: MappingAt[Origin], X, R]
	     (component :M[Origin] => K)
	     (implicit project :OriginProjection[K, X], factory :MappingLiteral.AptResult[K, X, Origin, R]) :R =
	{
		val subcomponent = component(mapping)
		val extract = mapping(project[Origin](subcomponent))
		factory(extract(value), subcomponent)
	}

	def apply[K <: MappingAt[Origin], X, R]
	         (component :M[Origin] => K)
	         (implicit project :OriginProjection[K, X], factory :MappingLiteral.AptResult[K, X, Origin, R]) :R =
		this \ component

	protected override def alter(export :TypedMapping[V, Origin]) :MappingLiteral[M, V] =
		MappingLiteral(value, mapping, export)

	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		SpelledSQL(spelling.scope.termForm(mapping).literal(value, spelling.isInline), context)

	protected override def explodedSpelling[P]
	                       (independent :Boolean)
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
	{
		val form = spelling.scope.termForm(export)
		if (form.columnCount == 1)
			PassedArray :+ SpelledSQL(form.literal(value), context)
		else
			form.columnLiterals(value).map(SpelledSQL(_, context))
	}

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[RowProduct]#rows[Single]#__] = {
		def columnLiteral[X](column :TypedColumn[X, Origin]) = this \ column
		spelling.scope.defaultColumns(export).toSeq.map(columnLiteral(_))
	}

	protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.mappingLiteral(this)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.mappingLiteral(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: MappingLiteral[M, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.mappingLiteral(this)

	override def canEqual(that :Any) :Boolean =
		that.isInstanceOf[MappingLiteral[MappingOf[Any]#TypedProjection @unchecked, _]]
}


private[ast] sealed abstract class Rank1MappingLiteralImplicits {
	implicit def mappingLiteralResult[K <: MappingAt[O], M[A] <: BaseMapping[V, A], V, O]
	                                 (implicit project :OriginProjection[K, V] { type WithOrigin[A] = M[A] }) =
		new MappingLiteral.AptResult[K, V, O, MappingLiteral[M, V]](
			(value :V, component :K) => MappingLiteral[M, V, O](value, project[O](component), project[O](component))
		)
}


object MappingLiteral extends Rank1MappingLiteralImplicits {
	def apply[M[A] <: BaseMapping[V, A], V](value :V)(implicit summon :MappingInstance[M, V])
			:MappingLiteral[M, V] =
		MappingLiteral[M, V, Any](value, summon.withOrigin, summon.withOrigin)

	def apply[K <: Mapping, V](mapping :K, value :V)(implicit project :OriginProjection[K, V])
			:MappingLiteral[project.WithOrigin, V] =
		mapping match {
			case column :TypedColumn[V, Any] @unchecked =>
				ColumnMappingLiteral(column.withOrigin[Any], column, value)
					.asInstanceOf[MappingLiteral[project.WithOrigin, V]]
			case _ =>
				new Impl(value, project[Any](mapping), project[Any](mapping))
		}

	def apply[K <: MappingAt[O], V, O](mapping :K, export :TypedMapping[V, O], value :V)
	                                  (implicit project :OriginProjection[K, V])
			:MappingLiteral[project.WithOrigin, V] { type Origin = O } =
		(mapping, export) match {
			case (c :TypedColumn[V, O] @unchecked, e :TypedColumn[V, O] @unchecked) =>
				ColumnMappingLiteral(c, e, value)
					.asInstanceOf[MappingLiteral[project.WithOrigin, V] { type Origin = O }]
			case _ if !(mapping isomorphic export) =>
				throw new IllegalArgumentException(
					"Cannot create a MappingLiteral(" + value + "): nominal mapping " + mapping +
					" is not isomorphic with export mapping " + export + "."
				)
			case _ =>
				new Impl(value, project[O](mapping), export)
		}

	private[ast] def apply[M[A] <: BaseMapping[V, A], V, O](value :V, mapping :M[O], export :TypedMapping[V, O])
			:MappingLiteral[M, V] { type Origin = O } =
		(mapping, export) match {
			case (c :TypedColumn[V, O] @unchecked, e :TypedColumn[V, O] @unchecked) =>
				ColumnMappingLiteral(c, e, value)
					.asInstanceOf[MappingLiteral[M, V] { type Origin = O }]
			case _ =>
				new Impl[M, V, O](value, mapping, export)
		}


	def unapply[T](e :SQLExpression[Nothing, Grouped, T]) :Opt[(T, MappingOf[T])] =
		e match {
			case literal :MappingLiteral[MappingOf[T]#TypedProjection @unchecked, T @unchecked] =>
				Got(literal.value, literal.mapping)
			case _ => Lack
		}


	private class Impl[M[A] <: BaseMapping[V, A], V, O]
	                  (override val value :V, override val mapping :M[O], override val export :TypedMapping[V, O])
		extends SQLLiteral[V] with MappingLiteral[M, V]
	{
		override type Origin = O
		override val anchored = export
	}


	//type O included only for inference
	class AptResult[K <: Mapping, V, O, Res] private[ast](private val result :(V, K) => Res) extends AnyVal {
		@inline def apply(value :V, component :K) :Res = result(value, component)
	}
	implicit def columnMappingResult[K <: MappingAt[O], M[A] <: BaseColumn[V, A], V, O]
	                                (implicit project :OriginProjection[K, V] { type WithOrigin[A] = M[A] }) =
		new AptResult[K, V, O, ColumnMappingLiteral[M, V]](
			(value :V, component :K) => ColumnMappingLiteral(project[O](component), project[O](component), value)
		)


	trait SpecificMappingLiteralVisitor[X, +Y] extends SpecificColumnMappingLiteralVisitor[X, Y] {
		def mappingLiteral[M[A] <: BaseMapping[X, A]](e :MappingLiteral[M, X]) :Y
	}
	trait MatchSpecificMappingLiteral[X, +Y]
		extends SpecificMappingLiteralVisitor[X, Y] with CaseSpecificColumnMappingLiteral[X, Y]
	{
		override def columnMappingLiteral[M[A] <: BaseColumn[X, A]](e :ColumnMappingLiteral[M, X]) :Y =
			mappingLiteral(e)
	}
	type CaseSpecificMappingLiteral[X, +Y] = MatchSpecificMappingLiteral[X, Y]
//
//
//	trait MappingLiteralVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends ColumnMappingLiteralVisitor[Y]
//	{
//		def mappingLiteral[M[A] <: BaseMapping[V, A], V](e :MappingLiteral[M, V]) :Y[Single, V, MappingLiteral[M, V]]
//	}
//	trait MatchMappingLiteral[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MappingLiteralVisitor[Y]
//	{
//		override def columnMappingLiteral[M[A] <: BaseColumn[V, A], V]
//		                                 (e :ColumnMappingLiteral[M, V]) :Y[Single, V, ColumnMappingLiteral[M, V]] =
//			mappingLiteral(e)
//	}
//	type CaseMappingLiteral[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		MatchMappingLiteral[Y]


	trait AnyMappingLiteralVisitor[+Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnMappingLiteralVisitor[Y]
	{
		def mappingLiteral[M[A] <: BaseMapping[X, A], X](e :MappingLiteral[M, X]) :Y[Single, X]
	}
	trait MatchAnyMappingLiteral[+Y[-_ >: Grouped <: Single, _]]
		extends AnyMappingLiteralVisitor[Y] with CaseAnyColumnMappingLiteral[Y]
	{
		override def columnMappingLiteral[M[A] <: BaseColumn[V, A], V]
		                                 (e :ColumnMappingLiteral[M, V]) :Y[Single, V] = mappingLiteral(e)
	}
	type CaseAnyMappingLiteral[+Y[-_ >: Grouped <: Single, _]] = MatchAnyMappingLiteral[Y]
}




trait ColumnMappingLiteral[M[A] <: BaseColumn[V, A], V]
	extends MappingLiteral[M, V] with ColumnLiteral[V] with ColumnMappingTerm[M, V]
	   with ColumnMappingTermTemplate[M, V, ColumnMappingLiteral[M, V]]
{
	protected override def alter(export :TypedMapping[V, Origin]) :ColumnMappingLiteral[M, V] =
		export match {
			case column :TypedColumn[V, Origin] @unchecked =>
				ColumnMappingLiteral(mapping, column, value)
			case _ =>
				throw new IllegalArgumentException(
					"Cannot alter a ColumnMappingLiteral " + this  + " to a non column mapping " + export + "."
				)
		}

	protected override def visit[Y](visitor :SpecificColumnVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.columnMappingLiteral(this)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.columnMappingLiteral(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: ColumnMappingLiteral[M, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.columnMappingLiteral(this)
}


object ColumnMappingLiteral {

	def apply[K <: ColumnMapping, M[A] <: BaseColumn[V, A], V]
	         (column :K, value :V)(implicit project :OriginProjection[K, V] { type WithOrigin[A] = M[A] })
			:ColumnMappingLiteral[M, V] =
		new Impl(value, project[Any](column), project[Any](column))

	def apply[K <: ColumnMapping, M[A] <: BaseColumn[V, A], V, O]
	         (column :K, export :TypedColumn[V, O], value :V)
	         (implicit project :OriginProjection[K, V] { type WithOrigin[A] = M[A] })
			:ColumnMappingLiteral[M, V] { type Origin = O } =
		new Impl(value, project(column), export)

	private class Impl[M[A] <: BaseColumn[V, A], V, O]
	                  (override val value :V, override val mapping :M[O], override val export :TypedColumn[V, O])
		extends ColumnMappingLiteral[M, V]
	{
		override type Origin = O
		override val anchored = export
	}

	trait SpecificColumnMappingLiteralVisitor[X, +Y] {
		def columnMappingLiteral[M[A] <: BaseColumn[X, A]](e :ColumnMappingLiteral[M, X]) :Y
	}
	type MatchSpecificColumnMappingLiteral[X, +Y] = SpecificColumnMappingLiteralVisitor[X, Y]
	type CaseSpecificColumnMappingLiteral[X, +Y]  = SpecificColumnMappingLiteralVisitor[X, Y]
//
//	trait ColumnMappingLiteralVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] {
//		def columnMappingLiteral[M[A] <: BaseColumn[V, A], V]
//		                        (e :ColumnMappingLiteral[M, V]) :Y[Single, V, ColumnMappingLiteral[M, V]]
//	}
//	type MatchColumnMappingLiteral[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		ColumnMappingLiteralVisitor[Y]
//	type CaseColumnMappingLiteral[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		ColumnMappingLiteralVisitor[Y]

	trait AnyColumnMappingLiteralVisitor[+Y[-_ >: Grouped <: Single, _]] {
		def columnMappingLiteral[M[A] <: BaseColumn[X, A], X](e :ColumnMappingLiteral[M, X]) :Y[Single, X]
	}
	type MatchAnyColumnMappingLiteral[+Y[-_ >: Grouped <: Single, _]] = AnyColumnMappingLiteralVisitor[Y]
	type CaseAnyColumnMappingLiteral[+Y[-_ >: Grouped <: Single, _]]  = AnyColumnMappingLiteralVisitor[Y]
}





//todo: EvalParam
trait BoundParam[V]
	extends RWTerm[V] with SQLTermTemplate[V, GroundSQL, BoundParam, BoundColumnParam]
//	   with ConvertingTemplate[RowProduct, GlobalScope, V, BoundParam]
	   with SelectableSQL[RowProduct, Single, V]
{
	val name :Option[String]
	private[this] val constForm = SQLWriteForm.const(value)(form.writer)
	def writeForm :SQLWriteForm[Unit] = constForm

	protected override def column[X](value :X, index :Int)(implicit form :ColumnForm[X]) :ColumnTerm[X] =
		BoundColumnParam(value, name.map(_ + "_" + index))

	override def reform[U](form :SQLForm[U], conversion :SQLConversion[V, U]) :BoundParam[U] =
		BoundParam(conversion(value), name)(form)

	override def reform[U](form :ColumnForm[U], conversion :SQLConversion[V, U]) :BoundColumnParam[U] =
		BoundColumnParam(conversion(value), name)(form)
//
//	override def reform[U, EC[x] >: BoundParam[x], E](form :SQLForm[U], conversion :SpecificTransformation[V, U, EC, BoundParam[V], E]) :E =
//		conversion.sql(this)

//	override def reform[U](form :SQLReadForm[U], lift :Lift[T, U]) :SQLExpression[RowProduct, GlobalScope, U] =

//	override def reform(form :SQLForm[V]) :BoundParam[V] = form match {
//		case column :ColumnForm[V] => BoundColumnParam(value, name)(column)
//		case _ => new Impl(value, name)(form)
//	}
//	override def reform(form :ColumnForm[V]) :BoundColumnParam[V] = BoundColumnParam[V](value, name)(form)

	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = columnCount

	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		SpelledSQL(form.param(spelling.isInline), constForm, context)

	protected override def explodedSpelling[P]
	                       (independent :Boolean)
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		try {
			constForm.split.map { SpelledSQL("?", _, context) }
		} catch {
			case e :UnsupportedOperationException => form.columnCount match {
				case 0 => Nil
				case n if !independent =>
					val p = SpelledSQL("?", context)
					PassedArray.fill(n - 1)(p) :+ SpelledSQL("?", constForm, context)
				case _ => throw e
			}
		}

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor: AnyExpressionVisitor[RowProduct, Y]): Y[Single, V] =
		visitor.param(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.param(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: BoundParam[V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.native(this)

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case p :BoundParam[_] if canEqual(p) && p.canEqual(this) =>
			name == p.name && value == p.value && form == p.form
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[BoundParam[_]]
	override def hashCode :Int =
		((if (value == null) 0 else value.hashCode) * 31 + name.hashCode) * 31 + form.hashCode

	override def toString :String = "?" + value
}


object BoundParam extends FormBasedFactory[Self, BoundParam, BoundColumnParam] {

	def apply[T :SQLForm](name :String, value :T) :BoundParam[T] = SQLForm[T] match {
		case column :ColumnForm[T] => BoundColumnParam(name, value)(column)
		case _ => new Impl(value, Some(name))
	}
	def apply[T](form :SQLForm[T])(value :T, name :String = null) :BoundParam[T] = form match {
		case column :ColumnForm[T] => BoundColumnParam(value)(column)
		case _ => new Impl(value)(form)
	}

	def unapply[T](e :SQLExpression[Nothing, Grouped, T]) :Opt[(T, Option[String])] = e match {
		case param :BoundParam[T @unchecked] => Got((param.value, param.name))
		case _ => Lack
	}

	private class Impl[V](override val value :V, val name :Option[String] = None)(implicit override val form :SQLForm[V])
		extends BoundParam[V]
//		/** Provider of implicit converters `ParameterFactory[X]` from any Scala literal `X`
//		  * with an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]/[[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
//		  * to an [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]]`[X]`/[[net.noresttherein.oldsql.sql.ast.BoundParamColumn BoundParamColumn]]`[X]`
//		  */

	protected override def specificResult[X :ColumnForm](value :X) :BoundColumnParam[X] = BoundColumnParam(value)
	protected override def generalResult[X :SQLForm](value :X) :BoundParam[X] = BoundParam(SQLForm[X])(value)

	/** A factory lifting any literal `X` into an [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]]`[X]`
	  * or its subclass [[net.noresttherein.oldsql.sql.ast.BoundColumnParam BoundParamColumn]]`[X]`,
	  * depending on whether an implicit [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
	  * or [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists.
	  */
	type BoundParamFactory[X, P <: BoundParam[X]] = DedicatedFactory[X, X] { type Res = P }


	trait SpecificBoundParamVisitor[X, +Y]
		extends SpecificBoundColumnParamVisitor[X, Y] with SpecificBoundMappingParamVisitor[X, Y]
	{
		def param(e :BoundParam[X]) :Y
	}
	trait MatchSpecificBoundParam[X, +Y]
		extends SpecificBoundParamVisitor[X, Y] with CaseSpecificBoundMappingParam[X, Y]
	{
		override def paramColumn(e :BoundColumnParam[X]) :Y = param(e)
	}
	trait CaseSpecificBoundParam[X, +Y] extends MatchSpecificBoundParam[X, Y] {
		override def mappingParam[M[A] <: BaseMapping[X, A]](e :BoundMappingParam[M, X]) :Y = param(e)
	}
//
//
//	trait BoundParamVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends BoundColumnParamVisitor[Y] with BoundMappingParamVisitor[Y]
//	{
//		def param[V](e :BoundParam[V]) :Y[Single, V, BoundParam[V]]
//	}
//	trait MatchBoundParam[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends BoundParamVisitor[Y] with CaseBoundMappingParam[Y]
//	{
//		override def paramColumn[V](e :BoundColumnParam[V]) :Y[Single, V, BoundColumnParam[V]] = param(e)
//	}
//	trait CaseBoundParam[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MatchBoundParam[Y]
//	{
//		override def mappingParam[M[A] <: BaseMapping[V, A], V]
//		                         (e :BoundMappingParam[M, V]) :Y[Single, V, BoundMappingParam[M, V]] = param(e)
//	}


	trait AnyBoundParamVisitor[+Y[-_ >: Grouped <: Single, _]]
		extends AnyBoundColumnParamVisitor[Y] with AnyBoundMappingParamVisitor[Y]
	{
		def param[X](e :BoundParam[X]) :Y[Single, X]
	}
	trait MatchAnyBoundParam[+Y[-_ >: Grouped <: Single, _]]
		extends AnyBoundParamVisitor[Y] with CaseAnyBoundMappingParam[Y]
	{
		override def paramColumn[X](e :BoundColumnParam[X]) :Y[Single, X] = param(e)
	}
	trait CaseAnyBoundParam[+Y[-_ >: Grouped <: Single, _]] extends MatchAnyBoundParam[Y] {
		override def mappingParam[M[A] <: BaseMapping[X, A], X](e :BoundMappingParam[M, X]) :Y[Single, X] =
			param(e)
	}
}




trait BoundColumnParam[V]
	extends BoundParam[V] with RWColumnTerm[V]
	   with GroundSQLTemplate[V, BoundColumnParam[V]]
{
	override val writeForm  :ColumnWriteForm[Unit] = ColumnWriteForm.const(value)(form.writer)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.paramColumn(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.paramColumn(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: BoundColumnParam[V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.paramColumn(this)
}


object BoundColumnParam {
	def apply[V :ColumnForm](name :String, param :V) :BoundColumnParam[V] =
		new Impl(param, Some(name))

	def apply[V :ColumnForm](param :V, name :Option[String] = None) :BoundColumnParam[V] =
		new Impl[V](param, name)

	def apply[V](form :ColumnForm[V])(value :V) :BoundColumnParam[V] =
		new Impl[V](value)(form)

	def unapply[V](e :SQLExpression[Nothing, Grouped, V]) :Opt[(V, Option[String])] = e match {
		case param :BoundColumnParam[V] => Got((param.value, param.name))
		case _ => Lack
	}

	private class Impl[V](override val value :V, override val name :Option[String] = None)
	                     (implicit override val form :ColumnForm[V])
		extends BoundColumnParam[V]

	trait SpecificBoundColumnParamVisitor[X, +Y] extends SpecificBoundColumnMappingParamVisitor[X, Y] {
		def paramColumn(e :BoundColumnParam[X]) :Y
	}
	type MatchSpecificBoundColumnParam[X, +Y] = SpecificBoundColumnParamVisitor[X, Y]

	trait CaseSpecificBoundColumnParam[X, +Y] extends MatchSpecificBoundColumnParam[X, Y] {
		override def columnMappingParam[M[A] <: BaseColumn[X, A]](e :BoundColumnMappingParam[M, X]) :Y =
			paramColumn(e)
	}
//
//
//	trait BoundColumnParamVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends BoundColumnMappingParamVisitor[Y]
//	{
//		def paramColumn[V](e :BoundColumnParam[V]) :Y[Single, V, BoundColumnParam[V]]
//	}
//	type MatchBoundColumnParam[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		BoundColumnParamVisitor[Y]
//
//	trait CaseBoundColumnParam[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MatchBoundColumnParam[Y]
//	{
//		override def columnMappingParam[M[A] <: BaseColumn[V, A], V](e :BoundColumnMappingParam[M, V])
//				:Y[Single, V, BoundColumnMappingParam[M, V]] =
//			paramColumn(e)
//	}

	trait AnyBoundColumnParamVisitor[+Y[-_ >: Grouped <: Single, _]]
		extends AnyBoundColumnMappingParamVisitor[Y]
	{
		def paramColumn[X](e :BoundColumnParam[X]) :Y[Single, X]
	}
	trait CaseAnyBoundColumnParam[+Y[-_ >: Grouped <: Single, _]] extends MatchAnyBoundColumnParam[Y] {
		override def columnMappingParam[M[A] <: BaseColumn[V, A], V]
		                               (e :BoundColumnMappingParam[M, V]) :Y[Single, V] = paramColumn(e)
	}
	type MatchAnyBoundColumnParam[+Y[-_ >: Grouped <: Single, _]] = AnyBoundColumnParamVisitor[Y]
}




/** A parameter expression of shape specified not by a fixed [[net.noresttherein.oldsql.schema.SQLForm SQLForm]],
  * but by a [[net.noresttherein.oldsql.schema.Mapping Mapping]] for its value type. Unlike in its base class,
  * the effective column set (and form) is [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope spelling scope]]
  * specific. It works as if it was replaced during spelling with an `BoundParam[T]` using form
  * `spelling.scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.termForm termForm]]`(mapping)`.
  * Reforming produces a regular `BoundParam`.
  */
sealed trait BoundMappingParam[M[A] <: BaseMapping[V, A], V]
	extends BoundParam[V] with MappingTerm[M, V] with MappingTermTemplate[M, V, BoundMappingParam[M, V]]
{
	override def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
			:BoundMappingParam[project.WithOrigin, X] { type Origin = BoundMappingParam.this.Origin } =
	{
		val projected = project[Origin](component) //give us the subject type
		BoundMappingParam(mapping(projected)(value), projected, export.export(projected))
	}
	override def \[K <: ColumnAt[Origin], X]
	              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
			:BoundColumnMappingParam[project.WithOrigin, X] { type Origin = BoundMappingParam.this.Origin } =
	{
		val projected = project[Origin](column)
		BoundColumnMappingParam(projected, export.export(projected), mapping(projected)(value))
	}

//	override def \[X](column :TypedColumn[X, Origin]) :BoundColumnMappingParam[MappingOf[X]#ColumnProjection, X] = {
//		val extract = export(column)
//		BoundColumnMappingParam(column, extract.export, extract(value))
//	}

	def \[K <: MappingAt[Origin], X, R]
	     (component :M[Origin] => K)
	     (implicit project :OriginProjection[K, X], factory :BoundMappingParam.AptResult[K, X, Origin, R]) :R =
	{
		val subcomponent = component(mapping)
		val extract = mapping(project[Origin](subcomponent))
		factory(extract(value), subcomponent)
	}

	def apply[K <: MappingAt[Origin], X, R]
	         (component :M[Origin] => K)
	         (implicit project :OriginProjection[K, X], factory :BoundMappingParam.AptResult[K, X, Origin, R]) :R =
		this \ component

	protected override def alter(export :TypedMapping[V, Origin]) :BoundMappingParam[M, V] =
		BoundMappingParam(value, mapping, export)

	//todo: too lazy to implement it now, we don't really know if we need it anyway
//		override protected def map[Y](lift :Lift[V, Y]) :MappingSQL[RowProduct, GlobalScope, M, Y] = ???

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnMappingSQL.from[RowProduct]#rows[Single]#__] = {
		def columnParam[X](column :TypedColumn[X, Origin]) = this \ column
		spelling.scope.defaultColumns(export).toSeq.map(columnParam(_))
	}

	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val form = spelling.scope.termForm(export)
		SpelledSQL(form.param(spelling.isInline), SQLWriteForm.const(value)(form.writer), context)
	}

	protected override def explodedSpelling[P]
	                       (independent :Boolean)
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
	{
		implicit val form = spelling.scope.termForm(export).writer
		try {
			form.columnCount match {
				case 0 =>
					PassedArray.empty
				case 1 =>
					PassedArray :+ SpelledSQL(form.param, SQLWriteForm.const(value), context)
				case _ =>
					form.split.map { implicit columnForm => SpelledSQL("?", ColumnWriteForm.const(value), context) }
			}
		} catch {
			case _ :UnsupportedOperationException =>
				if (!independent) {
					val p = SpelledSQL("?", context)
					PassedArray.fill(form.columnCount - 1)(p) :+ SpelledSQL("?", SQLWriteForm.const(value), context)
				} else {
					spelling.scope.defaultColumns(export).toSeq map { column =>
						def writeForm[X](column :TypedColumn[X, Origin]) =
							ColumnWriteForm.constOpt(mapping(column).opt(value))(column.form)
						SpelledSQL("?", writeForm(column), context)
					}
				}
		}
	}

	protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.mappingParam(this)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.mappingParam(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: BoundMappingParam[M, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.native(this)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[BoundMappingParam[M @unchecked, V @unchecked]]
}


private[ast] sealed abstract class Rank1BoundMappingParamImplicits {
	implicit def mappingParamResult[K <: MappingAt[O], M[A] <: BaseMapping[V, A], V, O]
	                               (implicit project :OriginProjection[K, V] { type WithOrigin[A] = M[A] }) =
		new BoundMappingParam.AptResult((value :V, component :K) =>
			BoundMappingParam[M, V, O](value, project[O](component), project[O](component))
		)
}


object BoundMappingParam {
	def apply[M[A] <: BaseMapping[V, A], V](value :V)(implicit summon :MappingInstance[M, V]) :BoundMappingParam[M, V] =
		BoundMappingParam[M, V, Any](value, summon.withOrigin, summon.withOrigin)

	def apply[K <: Mapping, V](mapping :K, value :V)(implicit project :OriginProjection[K, V])
			:BoundMappingParam[project.WithOrigin, V] =
		mapping match {
			case column :TypedColumn[V, Any] @unchecked =>
				BoundColumnMappingParam(column, value).asInstanceOf[BoundMappingParam[project.WithOrigin, V]]
			case _ =>
				new Impl(value, project[Any](mapping), project[Any](mapping))
		}

	def apply[K <: MappingAt[O], V, O](mapping :K, export :TypedMapping[V, O], value :V)
	                                  (implicit project :OriginProjection[K, V])
			:BoundMappingParam[project.WithOrigin, V] { type Origin = O } =
		(mapping, export) match {
			case (c :TypedColumn[V, O] @unchecked, e :TypedColumn[V, O] @unchecked) =>
				BoundColumnMappingParam(c, e, value)
					.asInstanceOf[BoundMappingParam[project.WithOrigin, V] { type Origin = O }]
			case _ if !(mapping isomorphic export) =>
				throw new IllegalArgumentException(
					"Cannot create a BoundMappingParam(" + value + "): nominal mapping " + mapping +
					" is not isomorphic with export mapping " + export + "."
				)
			case _ =>
				new Impl(value, project[O](mapping), export)

		}

	private[ast] def apply[M[A] <: BaseMapping[V, A], V, O](value :V, mapping :M[O], export :TypedMapping[V, O])
			:BoundMappingParam[M, V] { type Origin = O } =
		(mapping, export) match {
			case (c :TypedColumn[V, O] @unchecked, e :TypedColumn[V, O] @unchecked)  =>
				BoundColumnMappingParam(c, e, value)
					.asInstanceOf[BoundMappingParam[M, V] { type Origin = O }]
			case _ =>
				new Impl(value, mapping, export)
		}


	def unapply[T](e :SQLExpression[Nothing, Grouped, T]) :Opt[(T, MappingOf[T])] =
		e match {
			case literal :BoundMappingParam[MappingOf[T]#TypedProjection @unchecked, T @unchecked] =>
				Got(literal.value, literal.mapping)
			case _ => Lack
		}


	private class Impl[M[A] <: BaseMapping[V, A], V, O]
	                  (override val value :V, override val mapping :M[O], override val export :TypedMapping[V, O])
		extends BoundMappingParam[M, V]
	{
		override type Origin = O
		override val name = Some(mapping.mappingName)
		override val anchored = export
	}


	//type O included only for inference
	class AptResult[K <: Mapping, V, O, Res] private[ast](private val result :(V, K) => Res) extends AnyVal {
		@inline def apply(value :V, component :K) :Res = result(value, component)
	}
	implicit def columnMappingParamResult[K <: MappingAt[O], M[A] <: BaseColumn[V, A], V, O]
	                                     (implicit project :OriginProjection[K, V] { type WithOrigin[A] = M[A] }) =
		new AptResult((value :V, component :K) =>
			BoundColumnMappingParam(project[O](component), project[O](component), value)
		)

	trait SpecificBoundMappingParamVisitor[X, +Y] extends SpecificBoundColumnMappingParamVisitor[X, Y] {
		def mappingParam[M[A] <: BaseMapping[X, A]](e :BoundMappingParam[M, X]) :Y
	}
	trait MatchSpecificBoundMappingParam[X, +Y]
		extends SpecificBoundMappingParamVisitor[X, Y] with CaseSpecificBoundColumnMappingParam[X, Y]
	{
		override def columnMappingParam[M[A] <: BaseColumn[X, A]](e :BoundColumnMappingParam[M, X]) :Y =
			mappingParam(e)
	}
	type CaseSpecificBoundMappingParam[X, +Y] = MatchSpecificBoundMappingParam[X, Y]
//
//
//	trait BoundMappingParamVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends BoundColumnMappingParamVisitor[Y]
//	{
//		def mappingParam[M[A] <: BaseMapping[V, A], V]
//		                (e :BoundMappingParam[M, V]) :Y[Single, V, BoundMappingParam[M, V]]
//	}
//	trait MatchBoundMappingParam[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends BoundMappingParamVisitor[Y]
//	{
//		override def columnMappingParam[M[A] <: BaseColumn[V, A], V](e :BoundColumnMappingParam[M, V])
//				:Y[Single, V, BoundColumnMappingParam[M, V]] =
//			mappingParam(e)
//	}
//	type CaseBoundMappingParam[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		MatchBoundMappingParam[Y]


	trait AnyBoundMappingParamVisitor[+Y[-_ >: Grouped <: Single, _]]
		extends AnyBoundColumnMappingParamVisitor[Y]
	{
		def mappingParam[M[A] <: BaseMapping[X, A], X](e :BoundMappingParam[M, X]) :Y[Single, X]
	}
	trait MatchAnyBoundMappingParam[+Y[-_ >: Grouped <: Single, _]]
		extends AnyBoundMappingParamVisitor[Y] with CaseAnyBoundColumnMappingParam[Y]
	{
		override def columnMappingParam[M[A] <: BaseColumn[V, A], V]
		                               (e :BoundColumnMappingParam[M, V]) :Y[Single, V] = mappingParam(e)
	}
	type CaseAnyBoundMappingParam[+Y[-_ >: Grouped <: Single, _]] = MatchAnyBoundMappingParam[Y]
}




trait BoundColumnMappingParam[M[A] <: BaseColumn[V, A], V]
	extends BoundMappingParam[M, V] with BoundColumnParam[V] with ColumnMappingTerm[M, V]
	   with ColumnMappingTermTemplate[M, V, BoundColumnMappingParam[M, V]]
{
	protected override def alter(export :TypedMapping[V, Origin]) :BoundColumnMappingParam[M, V] =
		export match {
			case column :TypedColumn[V, Origin] @unchecked =>
				BoundColumnMappingParam(mapping, column, value)
			case _ =>
				throw new IllegalArgumentException(
					"Cannot alter a BoundColumnMappingParam " + this  + " to a non column mapping " + export + "."
				)
		}

	protected override def visit[Y](visitor :SpecificColumnVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.columnMappingParam(this)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.columnMappingParam(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: BoundColumnMappingParam[M, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.columnMappingParam(this)
}


object BoundColumnMappingParam {
	def apply[K <: ColumnMapping, M[A] <: BaseColumn[V, A], V]
	         (column :K, value :V)(implicit project :OriginProjection[K, V] { type WithOrigin[A] = M[A] })
			:BoundColumnMappingParam[M, V] =
		new Impl[project.WithOrigin, V, Unit](value, project(column), project(column))

	def apply[K <: ColumnMapping, M[A] <: BaseColumn[V, A], V, O]
	         (column :K, export :TypedColumn[V, O], value :V)
	         (implicit project :OriginProjection[K, V] { type WithOrigin[A] = M[A] })
			:BoundColumnMappingParam[M, V] { type Origin = O } =
		new Impl(value, project(column), export)

	private class Impl[M[A] <: BaseColumn[V, A], V, O]
	                  (override val value :V, override val mapping :M[O], override val export :TypedColumn[V, O])
		extends BoundColumnMappingParam[M, V]
	{
		override type Origin = O
		override val name = Some(mapping.mappingName)
		override val anchored = export
	}

	trait SpecificBoundColumnMappingParamVisitor[X, +Y] {
		def columnMappingParam[M[A] <: BaseColumn[X, A]](e :BoundColumnMappingParam[M, X]) :Y
	}
	type MatchSpecificBoundColumnMappingParam[X, +Y] = SpecificBoundColumnMappingParamVisitor[X, Y]
	type CaseSpecificBoundColumnMappingParam[X, +Y]  = SpecificBoundColumnMappingParamVisitor[X, Y]
//
//	trait BoundColumnMappingParamVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def columnMappingParam[M[A] <: BaseColumn[X, A], X]
//		                      (e :BoundColumnMappingParam[M, X]) :Y[Single, X, BoundColumnMappingParam[M, X]]
//	}
//	type MatchBoundColumnMappingParam[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		BoundColumnMappingParamVisitor[Y]
//	type CaseBoundColumnMappingParam[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		BoundColumnMappingParamVisitor[Y]

	trait AnyBoundColumnMappingParamVisitor[+Y[-_ >: Grouped <: Single, _]] {
		def columnMappingParam[M[A] <: BaseColumn[X, A], X](e :BoundColumnMappingParam[M, X]) :Y[Single, X]
	}
	type MatchAnyBoundColumnMappingParam[+Y[-_ >: Grouped <: Single, _]] = AnyBoundColumnMappingParamVisitor[Y]
	type CaseAnyBoundColumnMappingParam[+Y[-_ >: Grouped <: Single, _]]  = AnyBoundColumnMappingParamVisitor[Y]

}






/** A generalization of SQL NULL literal to a representation of a null value for type `T`, as defined by the parameter
  * [[net.noresttherein.oldsql.sql.ast.MultiNull.form form]].
  * It is rendered as `form.`[[net.noresttherein.oldsql.schema.SQLWriteForm.nullLiteral nullLiteral]]
  * or `form.`[[net.noresttherein.oldsql.schema.SQLWriteForm.inlineNullLiteral inlineNullLiteral]],
  * which, unless specifically overridden, are simply lists of `NULL` columns of a fixed length.
  * It is a superclass of [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]], which is a `NULL` column literal.
  */
sealed class MultiNull[V] private[ast](implicit val form :SQLForm[V])
	extends SQLTerm[V] with SQLTermTemplate[V, GroundSQL, MultiNull, SQLNull]
	   with SelectableSQL[RowProduct, Single, V]
{
	def this(columnCount :Int) =
		this()(SQLReadForm.none[V](columnCount) <> SQLWriteForm.nulls(columnCount))

	val writeForm            :SQLWriteForm[Unit] = SQLWriteForm.none[V](form.writer)
	override def selectForm  :SQLReadForm[V] = form.reader
	override def isNull      :SingleBoolean[RowProduct] = True
	override def groundValue :Opt[V] = form.nulls.option

	private lazy val isNullLiteralSeq = try {
		form.nullColumnLiterals.forall(_ equalsIgnoreCase "null")
	} catch {
		case _ :Exception => false
	}

	private lazy val columns :Seq[GroundColumn[_]] = form match {
		case col :ColumnForm[V] => PassedArray :+ SQLNull(col)
		case _ if form.columnCount == 0 => PassedArray.empty
		case _ =>
			form.nullColumnLiterals.zipMap(form.split) { (literal, writeForm) =>
				val columnForm = writeForm <> ColumnReadForm.none[V]
				if (literal equalsIgnoreCase "null") SQLNull(columnForm)
				else NativeColumnTerm(literal)(columnForm)
			}
	}

//	protected override def transform[X](conversion :SQLTransformation[V, X]) :SQLExpression[RowProduct, GlobalScope, X] =

	protected override def convert[X](conversion :SQLConversion[V, X]) :GroundSQL[X] =
		if (conversion.isIdentity)
			conversion(this)
		else if (conversion.isReversible && conversion.isLossless)
			MultiNull(conversion(form))
		else
			super.convert(conversion)
//		if (conversion.isIdentity)
//			conversion(this)
//		else if (conversion.isReversible)
//			new MultiNull[X]()(form.nullBimap(conversion.apply)(conversion.inverse))
//		else
//			new MultiNull[X]()(form.nullOptBimap((t :V) => Some(conversion(t)))(conversion.unapply))
//
//	override def opt: GlobalSQL[RowProduct, Option[V]] = MultiNull[Option[V]]


	override def reform[U](form :SQLForm[U], conversion :SQLConversion[V, U]) :MultiNull[U] =
		MultiNull[U](form)

	override def reform[U](form :ColumnForm[U], conversion :SQLConversion[V, U]) :SQLNull[U] =
		SQLNull[U](form)

	protected override def reform[F1 <: RowProduct, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                 (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
                                           spelling :SQLSpelling)
			:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
		if (!passCount.lastChance)
			passReform(other)(reform, passCount)
		else
			reformer(other)(reform, passCount).apply(other)

	protected override def reformer[F1 <: RowProduct, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                   (implicit leftResult  :SQLTransformation[V, U],
                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
		new RWTermReformer[F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](other)(reform, passCount) {
			override def expression(e :SQLExpression[F2, S2, V2]) =
				if (!passCount.lastChance)
					pass
				else if (reform.mayAlterLeft) {
					val r = rightResult(e)
					r.selectForm match {
						case columnForm :ColumnReadForm[U] =>
							val form = ColumnWriteForm.nulls(columnForm.sqlType) <> r.selectForm
							(SQLNull[U](form), r)
						case readForm =>
							val form = SQLWriteForm.nulls(readForm.columnCount) <> readForm
							(MultiNull[U](form), r)
					}
				} else
					fallback

		}

//	override def reform[U, EC[x] >: MultiNull[x], E]
//	                   (form :SQLForm[U], conversion :SpecificTransformation[V, U, EC, MultiNull[V], E]) :E =
//		conversion.result(MultiNull[U]) getOrElse ???
//
//	override def reform(form :SQLForm[V]) :MultiNull[V] = SQLNull[V](form)
//	override def reform(form :ColumnForm[V]) :SQLNull[V] = new SQLNull[V]()(form)
//
//	protected override def reform[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                             (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(SQLExpression[RowProduct, Single, U], SQLExpression[E, C, U]) =
//		if (passesAllowed >= MayPassBack)
//			super.reform(other)(reform, passesAllowed)
//		else
//			reformer(other)(reform, passesAllowed).apply(other)
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
//	                             (other :ComponentLValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[T, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(SQLExpression[RowProduct, GlobalScope, U], reform.LValue[E, C, U]) =
//		if (passesAllowed > 1)
//			super.reform(other)(reform, passesAllowed)
//		else
//
//	protected override def reformer[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                               (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                               (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:SpecificExpressionVisitor[E, C, X, (SQLExpression[RowProduct, Single, U], SQLExpression[E, C, U])] =
//		new TermReformer[E, C, X, U](form)(other)(reform, passesAllowed) {
//			override def expression(e :SQLExpression[E, C, X])
//					:(SQLExpression[RowProduct, Single, U], SQLExpression[E, C, U]) =
//				if (passesAllowed >= MayPassBack)
//					pass(e)
//				else if (!leftResult.isDerived || !rightResult.isDerived)
//					reform(leftResult(MultiNull.this), rightResult(e))
//				else if (reform.mayReformLeft) {
//					val r = rightResult(e)
//					r.selectForm match {
//						case columnForm :ColumnReadForm[U] =>
//							val form = ColumnWriteForm.nulls(columnForm.sqlType) <> r.selectForm
//							(SQLNull[U](form), r)
//						case readForm =>
//							val form = SQLWriteForm.nulls(readForm.columnCount) <> readForm
//							(MultiNull[U](form), r)
//					}
//				} else
//					fallback
//		}


	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[RowProduct, Single, _]] =
		try { columns } catch {
			case e :InseparableExpressionException => throw e
			case e :Exception =>
				throw new InseparableExpressionException(
					this, s"Cannot split composite null expression into separate columns: form $form cannot be split.", e
				)
		}

	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		SpelledSQL(form.nullLiteral(spelling.isInline), context)

	protected override def explodedSpelling[P]
	                       (independent :Boolean)
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		form.columnCount match {
			case 0 => Nil
			case 1 => SpelledSQL(form.nullLiteral, context)::Nil
			case _ =>
				val nullSQL = SpelledSQL(spelling.NULL, context)
				if (isNullLiteralSeq)
					ConstSeq(nullSQL, form.columnCount)
				else
					form.nullColumnLiterals.map { literal =>
						if (literal equalsIgnoreCase "null") nullSQL
						else SpelledSQL(spelling.literal(literal), context)
					}
		}


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.multiNull(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.multiNull(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: SQLTerm[V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.multiNull(this)


	override def sameAs(that :Any) :Boolean = that.isInstanceOf[MultiNull[_]]

	override def toString :String =
		groundValue.filter(_ != null).map(v => v.toString + ":SQLNull") getOrElse form.nullLiteral
}


object MultiNull {
	def apply[T :SQLForm] :MultiNull[T] = SQLForm[T] match {
		case form :ColumnForm[T] => SQLNull[T](form)
		case _ => new MultiNull[T]
	}

	def apply[T](columnCount :Int) :MultiNull[T] =
		if (columnCount == 1) SQLNull[T]()
		else new MultiNull[T](columnCount)

	def unapply(expression :SQLExpression[Nothing, Grouped, _]) :Boolean = expression match {
		case null => true
		case _ :MultiNull[_] => true
		case SQLLiteral(null) => true
		case _ => false
	}


	trait SpecificMultiNullVisitor[X, +Y] extends SpecificNullVisitor[X, Y] {
		def multiNull(e :MultiNull[X]) :Y
	}
	type MatchSpecificMultiNull[X, +Y] = CaseSpecificMultiNull[X, Y]

	trait CaseSpecificMultiNull[X, +Y] extends SpecificMultiNullVisitor[X, Y] with CaseSpecificNull[X, Y] {
		override def nullColumn(e :SQLNull[X]) :Y = multiNull(e)
	}
//
//
//	trait MultiNullVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends NullVisitor[Y]
//	{
//		def multiNull[V](e :MultiNull[V]) :Y[Single, V, MultiNull[V]]
//	}
//	trait MatchMultiNull[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MultiNullVisitor[Y] with CaseNull[Y]
//	{
//		override def nullColumn[V](e :SQLNull[V]) :Y[Single, V, SQLNull[V]] = multiNull(e)
//	}
//	type CaseMultiNull[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		MatchMultiNull[Y]


	trait AnyMultiNullVisitor[+Y[-_ >: Grouped <: Single, _]] extends AnyNullVisitor[Y] {
		def multiNull[X](e :MultiNull[X]) :Y[Single, X]
	}
	type MatchAnyMultiNull[+Y[-_ >: Grouped <: Single, _]] = CaseAnyMultiNull[Y]

	trait CaseAnyMultiNull[+Y[-_ >: Grouped <: Single, _]]
		extends AnyMultiNullVisitor[Y] with CaseAnyNull[Y]
	{
		override def nullColumn[X](e :SQLNull[X]) :Y[Single, X] = multiNull(e)
	}

}




final class SQLNull[V] private[ast]
                   (implicit override val form :ColumnForm[V] = ColumnReadForm.none[V] <> ColumnWriteForm.nulls[V])
	extends MultiNull[V] with ColumnTerm[V]
	   with GroundSQLTemplate[V, SQLNull[V]]
{
	override val writeForm  :ColumnWriteForm[Unit] = ColumnWriteForm.none[V](form.writer)
	override def selectForm :ColumnReadForm[V] = form.reader

	override def <>[E <: RowProduct, O >: Grouped <: Single, X]
	               (that :SQLExpression[E, O, X])(implicit compat :Interoperable[V, X]) :ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]

	override def <=[E <: RowProduct, O >: Grouped <: Single, X, U]
	               (that :SQLExpression[E, O, X])
	               (implicit compat :Interoperable[V, X] { type Unified = U }, ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]

	override def < [E <: RowProduct, O >: Grouped <: Single, X, U]
	              (that :SQLExpression[E, O, X])
	              (implicit compat :Interoperable[V, X] { type Unified = U }, ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]

	override def >=[E <: RowProduct, O >: Grouped <: Single, X, U]
	               (that :SQLExpression[E, O, X])
	               (implicit compat :Interoperable[V, X] { type Unified = U }, ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]

	override def > [E <: RowProduct, O >: Grouped <: Single, X, U]
	               (that :SQLExpression[E, O, X])
	               (implicit compat :Interoperable[V, X] { type Unified = U }, ordering :SQLOrdering[U])
			:ColumnSQL[E, O, Boolean] =
		SQLNull[Boolean]

	override def &&[E <: RowProduct, O >: Grouped <: Single]
	               (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		to[Boolean]

	override def ||[E <: RowProduct, O >: Grouped <: Single]
	               (other :ColumnSQL[E, O, Boolean])(implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		to[Boolean]

	override def unary_![E <: RowProduct, O >: Grouped <: Single]
	                    (implicit ev :V =:= Boolean) :ColumnSQL[E, O, Boolean] =
		to[Boolean]

//	protected override def transform[X](conversion :SQLTransformation[V, X]) :ColumnSQL[RowProduct, GlobalScope, X] =
//		if (conversion.isIdentity)
//			conversion(this)
//		else if (conversion.isReversible)
//			new SQLNull[X]()(form.nullBimap(conversion.apply)(conversion.inverse))
//		else
//			new SQLNull[X]()(form.nullOptBimap((t :V) => Some(conversion(t)))(conversion.unapply))

	protected override def convert[X](conversion :SQLConversion[V, X]) :SQLNull[X] =
		SQLNull[X](conversion(form))
//		if (conversion.isIdentity)
//			conversion(this)
//		else if (conversion.isReversible)
//			new SQLNull[X]()(form.nullBimap(conversion.apply)(conversion.inverse))
//		else
//			new SQLNull[X]()(form.nullOptBimap((t :V) => Some(conversion(t)))(conversion.unapply))
//
//	override def opt: ColumnSQL[RowProduct, GlobalScope, Option[V]] = SQLNull()

	override def reform[U](implicit form :ColumnForm[U], conversion :SQLConversion[V, U]) :SQLNull[U] =
		SQLNull[U]

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.nullColumn(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.nullColumn(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: SQLNull[V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.nullColumn(this)

	override def toString :String =
		groundValue.filter(_ != null).map(_.toString + ":SQLNull") getOrElse "SQLNull"
}


/** Brings into the implicit search scope a conversion from [[net.noresttherein.oldsql.sql.ast.SQLNull$ SQLNull]]
  * singleton object into a
  * [[net.noresttherein.oldsql.sql.ast.SQLNull! SQLNull]]/[[net.noresttherein.oldsql.sql.ast.MultiNull MultiNull]]
  * of any type for which an implicit [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]
  * (or [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]] exists.
  */
private[ast] sealed abstract class SQLNullFactory extends FormBasedFactory[Fixed[Unit]#T, MultiNull, SQLNull]

/** Adds an implicit conversion from the [[net.noresttherein.oldsql.sql.ast.SQLNull$ SQLNull]] singleton object itself
  * into an `SQLNull`/`MultiNull` of any type with an implicit `SQLForm`.
  */
object SQLNullFactory {
	@inline implicit def anySQLNull[T](singleton :SQLNull.type)(implicit factory :SQLNull.Factory[T]) :factory.Res =
		SQLNull[Unit, T](())
}


object SQLNull extends SQLNullFactory { //consider: extend SQLNull[Null]

	def apply[T](implicit factory :Factory[T]) :factory.Res = SQLNull[Unit, T](())

	def apply[T](form :ColumnForm[T] = ColumnReadForm.none[T] <> ColumnWriteForm.nulls[T]) :SQLNull[T] =
		new SQLNull[T]()(form)

	def apply[T](form :ColumnReadForm[T]) :SQLNull[T] = SQLNull(form <> ColumnWriteForm.nulls[T])



	def unapply(expression :SQLExpression[Nothing, Grouped, _]) :Boolean = expression match {
		case null => true
		case _ :SQLNull[_] => true
		case SQLLiteral(null) => true
		case _ => false
	}

	protected override def generalResult[T :SQLForm](arg :Unit) :MultiNull[T] = MultiNull[T]
	protected override def specificResult[T :ColumnForm](arg :Unit) :SQLNull[T] = new SQLNull[T]

	private[oldsql] val boolean = SQLNull[Boolean]
	private[oldsql] val int     = SQLNull[Int]
	private[oldsql] val long    = SQLNull[Long]
	private[oldsql] val double  = SQLNull[Double]
	private[oldsql] val string  = SQLNull[String]

	trait SpecificNullVisitor[X, +Y] {
		def nullColumn(e :SQLNull[X]) :Y
	}
	type MatchSpecificNull[X, +Y] = SpecificNullVisitor[X, Y]
	type CaseSpecificNull[X, +Y] = SpecificNullVisitor[X, Y]
//
//	trait NullVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] {
//		def nullColumn[V](e :SQLNull[V]) :Y[Single, V, SQLNull[V]]
//	}
//	type MatchNull[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] = NullVisitor[Y]
//	type CaseNull[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]  = NullVisitor[Y]

	trait AnyNullVisitor[+Y[-_ >: Grouped <: Single, _]] {
		def nullColumn[X](e :SQLNull[X]) :Y[Single, X]
	}
	type MatchAnyNull[+Y[-_ >: Grouped <: Single, _]] = AnyNullVisitor[Y]
	type CaseAnyNull[+Y[-_ >: Grouped <: Single, _]] = AnyNullVisitor[Y]

}






/** An expression injecting an arbitrary SQL `String`. Used for ad hoc implementations of some DBMS specific
  * features not covered by the bundled classes. This expression cannot be reformed or split
  * (unless it is already a [[net.noresttherein.oldsql.sql.ast.NativeColumnTerm NativeColumnTerm]]).
  */
class NativeTerm[V] protected (val sql :String, override val groundValue :Opt[V] = Lack)
                              (implicit override val selectForm :SQLReadForm[V])
	extends SQLTerm[V] with SQLTermTemplate[V, GroundSQL, NativeTerm, NativeColumnTerm]
{
	protected override def convert[X](conversion :SQLConversion[V, X]) :GroundSQL[X] =
		NativeTerm(sql, groundValue.map(conversion.apply))(conversion(selectForm))

	override def reform[U](form :SQLForm[U], conversion :SQLConversion[V, U]) :NativeTerm[U] =
		if (form.shape <:> selectForm.shape)
			NativeTerm(sql, groundValue.map(conversion.apply))(form)
		else
			throw new UnsupportedOperationException(
				"NativeTerm <" + this + "> cannot be reformed with a new form " + form + "."
			)

	override def reform[U](form :ColumnForm[U], conversion :SQLConversion[V, U]) :NativeColumnTerm[U] =
		if (form.shape <:> selectForm.shape)
			NativeColumnTerm(sql, groundValue.map(conversion.apply))(form)
		else
			throw new UnsupportedOperationException(
				"NativeColumnTerm <" + this + "> cannot be reformed with a new form " + form + "."
			)

	//prevent other from calling our reform(form) by clearing mayReformThis
	protected override def reform[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                 (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
                                           spelling :SQLSpelling)
			:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
		super.reform(other)(reform.prohibitReformLeft, passCount)

//	protected override def reform[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                             (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(SQLExpression[RowProduct, Single, U], SQLExpression[E, C, U]) =
//		super.reform(other)(reform.prohibitReformLeft, passesAllowed)
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
//	(other :LValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
//	(implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:(SQLExpression[RowProduct, Single, U], reform.LValue[E, C, U]) =
//		super.reform(other)(reform.prohibitReformLeft, passesAllowed)

	//reform(ComponentSQL) already delegates to reform(ComponentLValueSQL) so it's fine

   protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[RowProduct, Single, _]] =
		throw new InseparableExpressionException(this, "Native term " + this + " cannot be split into columns.")

	protected override def defaultSpelling[P]
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		SpelledSQL(sql, context)

	protected override def explodedSpelling[P]
	                       (independent :Boolean)
	                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		selectForm.columnCount match {
			case 0 => Nil
			case 1 => PassedArray.one(defaultSpelling(from, context, params))
			case _ => throw new InseparableExpressionException(this)
		}

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor: AnyExpressionVisitor[RowProduct, Y]): Y[Single, V] =
		visitor.native(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.native(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: NativeTerm[V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.native(this)

	override def isomorphic(expression :SQLExpression.__) :Boolean = expression match {
		case self :AnyRef if self eq this => true
		case native :NativeTerm[_] if sameAs(native) && (native sameAs this) => sql == native.sql
		case _ => false
	}

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case native :NativeTerm[_] if native canEqual this => sql == native.sql && selectForm == native.selectForm
		case _ => false
	}
	override def hashCode :Int = sql.hashCode * 31 + selectForm.hashCode

	override def toString :String = sql
}


object NativeTerm extends ReadFormBasedFactory[Fixed[String]#T, NativeTerm, NativeColumnTerm] {

	def apply[V :SQLReadForm](sql :String, value :Opt[V] = Lack) :NativeTerm[V] = SQLReadForm[V] match {
		case column :ColumnReadForm[V] => NativeColumnTerm[V](sql, value)(column)
		case _ => new NativeTerm[V](sql, value)
	}

	def unapply(e :SQLExpression[Nothing, Grouped, _]) :Opt[String] = e match {
		case native :NativeTerm[_] => Got(native.sql)
		case _ => Lack
	}

	protected override def generalResult[T :SQLReadForm](arg :String) :NativeTerm[T] = NativeTerm[T](arg)
	protected override def specificResult[T :ColumnReadForm](arg :String) :NativeColumnTerm[T] = NativeColumnTerm(arg)

	trait SpecificNativeTermVisitor[X, +Y] extends SpecificNativeColumnTermVisitor[X, Y] {
		def native(e :NativeTerm[X]) :Y
	}
	trait MatchSpecificNativeTerm[X, +Y] extends SpecificNativeTermVisitor[X, Y] with CaseSpecificNativeColumnTerm[X, Y] {
		override def nativeColumn(e :NativeColumnTerm[X]) :Y = native(e)
	}
	type CaseSpecificNativeTerm[X, +Y] = MatchSpecificNativeTerm[X, Y]
//
//
//	trait NativeTermVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends NativeColumnTermVisitor[Y]
//	{
//		def native[V](e :NativeTerm[V]) :Y[Single, V, NativeTerm[V]]
//	}
//	trait MatchNativeTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends NativeTermVisitor[Y]
//	{
//		override def nativeColumn[V](e :NativeColumnTerm[V]) :Y[Single, V, NativeColumnTerm[V]] = native(e)
//	}
//	type CaseNativeTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		MatchNativeTerm[Y]

	trait AnyNativeTermVisitor[+Y[-_ >: Grouped <: Single, _]]
		extends AnyNativeColumnTermVisitor[Y]
	{
		def native[X](e :NativeTerm[X]) :Y[Single, X]
	}
	trait MatchAnyNativeTerm[+Y[-_ >: Grouped <: Single, _]] extends AnyNativeTermVisitor[Y] {
		override def nativeColumn[X](e :NativeColumnTerm[X]) :Y[Single, X] = native(e)
	}
	type CaseAnyNativeTerm[+Y[-_ >: Grouped <: Single, _]] = MatchAnyNativeTerm[Y]
}




//consider: moving to ColumnTerm - or completely to ast
class NativeColumnTerm[V] protected (sql :String, override val groundValue :Opt[V] = Lack)
                                    (implicit override val selectForm :ColumnReadForm[V])
	extends NativeTerm[V](sql) with ColumnTerm[V]
	   with GroundSQLTemplate[V, NativeColumnTerm[V]]
{
	protected override def convert[X](conversion :SQLConversion[V, X]) :NativeColumnTerm[X] =
		NativeColumnTerm(sql, groundValue.map(conversion.apply))(conversion(selectForm))

	override def reform[U](implicit form :ColumnForm[U], conversion :SQLConversion[V, U]) :NativeColumnTerm[U] =
		NativeColumnTerm(sql, groundValue.map(conversion.apply))(form)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[RowProduct, Y]) :Y[Single, V] =
		visitor.nativeColumn(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[RowProduct, Single, V, Y]) :Y =
		visitor.nativeColumn(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: NativeColumnTerm[V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.nativeColumn(this)
}


object NativeColumnTerm {
	def apply[V :ColumnReadForm](sql :String, value :Opt[V] = Lack) :NativeColumnTerm[V] =
		new NativeColumnTerm[V](sql, value)

	def unapply(e :SQLExpression[Nothing, Grouped, _]) :Opt[String] = e match {
		case native :NativeColumnTerm[_] => Got(native.sql)
		case _ => Lack
	}

	trait SpecificNativeColumnTermVisitor[X, +Y] {
		def nativeColumn(e :NativeColumnTerm[X]) :Y
	}
	type MatchSpecificNativeColumnTerm[X, +Y] = SpecificNativeColumnTermVisitor[X, Y]
	type CaseSpecificNativeColumnTerm[X, +Y] = SpecificNativeColumnTermVisitor[X, Y]
//
//	trait NativeColumnTermVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] {
//		def nativeColumn[V](e :NativeColumnTerm[V]) :Y[Single, V, NativeColumnTerm[V]]
//	}
//	type MatchNativeColumnTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		NativeColumnTermVisitor[Y]
//	type CaseNativeColumnTerm[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		NativeColumnTermVisitor[Y]

	trait AnyNativeColumnTermVisitor[+Y[-_ >: Grouped <: Single, _]] {
		def nativeColumn[X](e :NativeColumnTerm[X]) :Y[Single, X]
	}
	type MatchAnyNativeColumnTerm[+Y[-_ >: Grouped <: Single, _]] = AnyNativeColumnTermVisitor[Y]
	type CaseAnyNativeColumnTerm[+Y[-_ >: Grouped <: Single, _]] = AnyNativeColumnTermVisitor[Y]
}



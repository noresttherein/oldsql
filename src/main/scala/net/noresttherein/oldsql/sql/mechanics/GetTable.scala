package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.ChainLength
import net.noresttherein.oldsql.morsels.abacus.{Inc, Negative, NegativeInc, Numeral, Positive}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.{Adjoin, Aggregated, AndBy, AndFrom, DecoratedFrom, Expanded, FromSome, GroupBy, GroupByClause, GroupParam, JoinParam, RowProduct, Subselect, ParamClause}
import net.noresttherein.oldsql.sql.DecoratedFrom.{DecoratorDecomposition, ExpandingDecorator}
import net.noresttherein.oldsql.sql.Expanded.{ExpandedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.ParamClause.{UnboundParam, LabeledUnboundParam, ParamAt}
import net.noresttherein.oldsql.sql.RowProduct.{As, NonEmptyFrom, PrefixOf, RowDecomposition}
import net.noresttherein.oldsql.sql.ast.JoinedRelation
import net.noresttherein.oldsql.sql.mechanics.GetTable.{Delegate, EvidenceTemplate, RelationEvidence}






/** Implicit evidence providing the `LastMapping`, `Last` and `FromLast` types of the mapping `F`.
  * An implicit value of this class exists for every type `F` which defines the eponymous type. While in many contexts
  * this information could be obtained by simply refining the type of the accepted ''from'' clause,
  * if an alternative in the form of a `FromLast` type parameter of a generic method and a free variable,
  * it will be likely instantiated based on its bounds and failing the typing. An implicit type which defines these
  * as member types instead eliminates the possibility of them being instantiated prematurely.
  * Apart from simply accessing the last relation, it also handles the mechanics of
  * ''expanding'' the relation, as defined by clause `F`, to clauses containing `F` as their
  * [[net.noresttherein.oldsql.sql.RowProduct.PrefixOf prefix]]. For this reason, type `Last` is, potentially,
  * not reported as defined in clause `F`, but as its supertype. In practice, default implicit values exist for
  * all `F#Last` and bundled types `F <: RowProduct`. Introducing a new `RowProduct` using a relation expression
  * type different than [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] and
  * [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]], will require declaring custom implicit instances
  * in companion objects to the new clause types.
  */
@implicitNotFound("I cannot determine the last relation of ${F}. This typically means the type is too abstract " +
                  "and doesn't define types LastMapping and FromLast or that F =:= Dual.\n" +
                  "Missing implicit LastTableOf[${F}].")
abstract class LastTableOf[-F <: NonEmptyFrom] {

	/** The [[net.noresttherein.oldsql.sql.RowProduct.LastMapping LastMapping]] type of clause `F`. */
	type LastMapping[O] <: MappingAt[O]

	/** A supertype of type [[net.noresttherein.oldsql.sql.RowProduct.Last Last]] of clause `F`. */
	type Last[O <: RowProduct] <: JoinedRelation[O, LastMapping]

	/** The [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type of clause `F`. */
	type FromLast >: F <: NonEmptyFrom

	/** The last relation in `F`, based on `F#FromLast`. */
	def apply(from :F) :Last[FromLast] = expand[FromLast](from)(PrefixOf.itself)

	/** The last relation in `from` based on ''from'' clause `E` expanding `F#FromLast`. */
	def expand[E <: RowProduct](from :F)(implicit prefix :FromLast PrefixOf E) :Last[E]
}



object LastTableOf {

	/** A refinement of a `RowProduct` lifting its `FromLast` and `LastMapping` types to type parameters
	  * of this type alias in the 'Aux' pattern.
	  */
	type LastBound[+T[O <: RowProduct] <: JoinedRelation[O, M], U <: NonEmptyFrom, M[O] <: MappingAt[O]] = U {
		type FromLast = U
		type LastMapping[O] = M[O]
		type Last[O <: RowProduct] <: T[O]
	}

	private[sql] val LastBound = new LastTableOf[NonEmptyFrom] {
		override type LastMapping[O] = MappingAt[O]
		override type Last[O <: RowProduct] = JoinedRelation[O, LastMapping]
//		override type FromLast = RowProduct AndFrom LastMapping

		override def apply(from :NonEmptyFrom) :JoinedRelation[FromLast, LastMapping] =
			from.last.asInstanceOf[JoinedRelation[FromLast, LastMapping]]

		override def expand[E <: RowProduct](from :NonEmptyFrom)(implicit prefix :FromLast PrefixOf E) =
			from.last.asInstanceOf[JoinedRelation[FromLast, LastMapping]].asIn(prefix)
	}

}






/** A framework base class for companion objects of implicit evidence classes extending
  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.RelationEvidence RelationEvidence]]. Each instance
  * implements type safe access to relations/mappings of any `RowProduct` based on some specific criteria, such as
  * the alias given to the relation or its relative position. The returned
  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
  * (or [[net.noresttherein.oldsql.schema.Mapping Mapping]]) will not only be of the correct type,
  * but also use an `Origin` type uniquely identifying the relation within the clause, which distinguishes
  * between its potential multiple occurrences and provides its positional index.
  *
  * It defines the [[net.noresttherein.oldsql.sql.mechanics.GetTable.Found Found]] intermediate implicit class
  * and implicit conversions of least precedence: Those converting an implicit of the (final) companion class
  * into `Found` as well as definitions propagating a `Found` instance through the joins under a ''group by''
  * clause (and thus not considered in the implicit search). Most implementations extend
  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.GetTableByPredicate GetTableByPredicate]] rather than
  * directly this class, which provides whole implementation of propagating the `Found` evidence through
  * all available `RowProduct` subtypes.
  */
abstract class GetTable {

	/** The upper type of the key type against which the relations are matched. */
	type Key

	/** The companion evidence class to this object. It is unnecessary for the subclasses
	  * to provide a definition, but doing so introduces an implicit conversion `Get => Found`
	  * of the lowest precedence, which allows adapting an existing implicit evidence `Get[F, X]`
	  * for an abstract type `F` into `Get[E, X]`, where `F ExpandedBy E`.
	  */
	type Get[-F <: RowProduct, G <: RowProduct, X <: Key] <: RelationEvidence[F, G, X]

	/** Summon an implicit instance of [[net.noresttherein.oldsql.sql.mechanics.GetTable.Get Get]] -
	  * the evidence companion class to this object.
	  */
	def apply[F <: RowProduct, G <: RowProduct, X <: Key](implicit get :Get[F, G, X]) :get.type = get



	/** Implicit resolution of search for a mapping `M` in `F <: RowProduct` which matches key type `X`.
	  * The nature of the match depends on the instance, but can take into account the mapping type,
	  * the prefix clause of `F` `P <: RowProduct { type LastMapping[O] = M[O] }`, such that
	  * `P `[[net.noresttherein.oldsql.sql.RowProduct.PrefixOf PrefixOf]]` F`, which contains the mapping as well
	  * as position (index) in the clause. Recursion framework provided here is predicate based;
	  * if a clause `P` satisfies
	  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.GetTableByPredicate.Predicate Predicate]]`[P, X]`
	  * (that is, such an implicit value exists), than its last relation `P#Last`/`P#LastMapping` is returned.
	  * Other implementations are also possible, but may require dedicated implicit recursion.
	  * The type of the mapping of the found relation is returned as the member type `M[O]` and
	  * the ([[net.noresttherein.oldsql.sql.ast.JoinedRelation relation type]] as member type `T[O]`. The type
	  * of the key used to retrieve the relation is implementation dependent. In other words, an implicit value
	  * `found :Found[F, G, X] { type I = N }` witnesses that `found.M` is the mapping of the last relation (rightmost)
	  * in the clause `F` for which an implicit `Predicate[F, X]` exists, with `N` being the ''negative'' index
	  * of the mapping (starting with `-1` for the last mapping and decreasing).
	  *
	  * This class is not the actual companion evidence class reported by this object for two reasons: first,
	  * various implementations introduce additional refinements over the standard
	  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.RelationEvidence RelationEvidence]] interface and, second,
	  * because after erasure all `GetTable#Found` classes are equal and methods of the same signature accepting
	  * evidence from different `GetTable` instances would clash with each other. The typical procedure is thus
	  * to implement the evidence resolution in means of `Found` and convert the final `Found` evidence into a `Get`.
	  * Leaving things at that would however not allow to perform the search based on an existing implicit `Get`
	  * (for example, to convert `Get[F, X] { type I = -2 }` into `Get[F Join T, X] { type I = -3 }`.
	  * For this reason, `GetTableByPredicate` subclasses typically introduce also a fallback feedback conversion
	  * in the other direction - from `Get` to `Found`. As this would lead to infinite loops when the evidence
	  * cannot be found (and reporting a 'diverging implicit expansion' error instead of 'implicit not found'
	  * with the customized message), `Found` instances obtained through scanning of the ''from'' clause `F`,
	  * rather than from an implicit `Get`, are always returned as its subclass
	  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.GetTableTemplate.Return Return]], and only values
	  * of that class are legible for conversion into the final `Get` evidence.
	  */
	@implicitNotFound("Cannot find a mapping for key type ${X} in the clause ${F}:\n " +
	                  "missing implicit Found[${F}, ${G}, ${X}].")
	trait Found[-F <: RowProduct, G <: RowProduct, X] extends RelationEvidence[F, G, X] {
		def how :GetTable = GetTable.this
	}

	/** Summon an implicit instance of [[net.noresttherein.oldsql.sql.mechanics.GetTable.Found! Found]]. */
	def Found[F <: RowProduct, G <: RowProduct, X](implicit get :Found[F, G, X]) :get.type = get



	/** The working subtype of `Found` returned by all recursive implicit methods, but not the one
	  * converting an implicit `Get` into a `Found`. Only values of this type can be adapted as the final,
	  * 'public' implicit value `Get[F, X]`. This distinction is introduced to break the implicit resolution
	  * cycle `Found => Get => Found` which would result in a 'diverging implicit expansion' error.
	  */
	@implicitNotFound("Cannot find a mapping for key type ${X} in the clause ${F}:\n " +
	                  "Missing implicit Return[${F}, ${G}, ${X}].")
	trait Return[-F <: RowProduct, G <: RowProduct, X] extends Found[F, G, X]



	/** Helper implementation method providing a [[net.noresttherein.oldsql.sql.mechanics.GetTable.Return Return]]
	  * instance returning the last relation of the specified
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFrom NonEmptyFrom]] clause.
	  */
	protected def found[F <: NonEmptyFrom, G <: NonEmptyFrom, X](implicit last :LastTableOf[G])
			:Return[F, G, X] {
				type T[O <: RowProduct] = last.Last[O]; type M[O] = last.LastMapping[O]
				type O = last.FromLast; type I = -1
			} =
		new EvidenceTemplate[F, G, X, last.FromLast, last.FromLast, -1, last.LastMapping, last.Last](PrefixOf.itself)
			with Return[F, G, X]
		{
			override def table[E <: RowProduct](from :G)(implicit stretch :S PrefixOf E) :T[E] = last.expand(from)
		}

	/** Helper implementation method providing a [[net.noresttherein.oldsql.sql.mechanics.GetTable.Return Return]]
	  * instance returning a relation from the prefix subclause to which the clause `F` decomposes.
	  */
	protected def forward[F <: RowProduct, G <: RowProduct, L <: U, U <: RowProduct, X, N <: Numeral]
	                     (get :RelationEvidence[_, L, _] { type O >: L <: U },
	                      general :RowDecomposition[G, L, U])
			:Return[F, G, X]
				{ type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = general.S[get.O]; type I = N } =
		new EvidenceTemplate[F, G, X, get.S, general.S[get.O], N, get.M, get.T](
		                     get.stretch + general.prefix[get.O])
			with Return[F, G, X]
		{
			override def table[E <: RowProduct](from :G)(implicit stretch :get.S PrefixOf E) =
				get.table(general.unapply(from))
		}

	/** Helper implementation method providing a [[net.noresttherein.oldsql.sql.mechanics.GetTable.Return Return]]
	  * instance returning a relation from the left side of a [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] clause.
	  */
	protected def left[F <: RowProduct, G <: L J R, L <: U, R[O] <: MappingAt[O],
	                   J[+A >: L <: U, B[O] <: R[O]] <: A Adjoin B, U <: RowProduct, X, N <: Numeral]
	                  (get :RelationEvidence[_, L, _] { type O >: L <: U })(prefix :get.S PrefixOf (get.O J R))
			:Return[F, G, X]
				{ type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = get.O J R; type I = N } =
		new EvidenceTemplate[F, G, X, get.S, get.O J R, N, get.M, get.T](prefix) with Return[F, G, X] {
			override def table[E <: RowProduct](from :G)(implicit stretch :get.S PrefixOf E) =
				get.table(from.left)
		}

	/** Helper implementation method providing a [[net.noresttherein.oldsql.sql.mechanics.GetTable.Return Return]]
	  * instance returning a relation from the decorated clause of
	  * a [[net.noresttherein.oldsql.sql.DecoratedFrom DecoratedFrom]] clause.
	  */
	protected def body[F <: RowProduct, G <: D[B], B <: U, D[+C >: B <: U] <: DecoratedFrom[C], U <: RowProduct,
	                   X, N <: Numeral]
	                  (get :RelationEvidence[_, B, _] { type O >: B <: U })(prefix :get.S PrefixOf D[get.O])
			:Return[F, G, X]
				{ type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = D[get.O]; type I = N } =
		new EvidenceTemplate[F, G, X, get.S, D[get.O], N, get.M, get.T](prefix) with Return[F, G, X] {
			override def table[E <: RowProduct](from :G)(implicit stretch :get.S PrefixOf E) =
				get.table(from.clause)
		}


	/** Feedback conversion from 'public' evidence to 'implementation' evidence. */
	implicit def continue[F <: RowProduct, G <: RowProduct, X <: Key](implicit get :Get[F, G, X])
			:Found[F, G, X]
				{ type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = get.O; type I = get.I } =
		get match {
			case found :GetTable#Found[_, _, _] if found.how == this =>
				found.asInstanceOf[Found[F, G, X] {
					type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = get.O; type I = get.I
				}]
			case _ =>
				new Delegate[F, G, X, get.O, get.I, get.M, get.T](get) with Found[F, G, X]
		}



	/** A carrier of actual evidence `Found` over a subselect fragment of relations in the grouped portion
	  * of a `GroupBy` clause, that is mappings that are not available for non-aggregate SQL expressions
	  * and can't be returned as the result of this search. It allows the search to continue backwards
	  * to the outer segment of the original input ''from'' clause.
	  */
	@implicitNotFound("Cannot find a mapping for key type ${X} in clause ${F}.\n" +
	                  "Missing implicit GroupedTunnel[${F}, ${G}, ${X}].")
	trait GroupedTunnel[-F <: RowProduct, G <: RowProduct, X] extends RelationEvidence[F, G, X] {
		/** The [[net.noresttherein.oldsql.sql.RowProduct.Outer outer]] clause of `O`,
		  * that is its prefix ending before the last `Subselect` of `F`.
		  */
		type B <: RowProduct

		/** The proof that the clause `S` ending with the found relation is a prefix of the outer clause `B` and,
		  * by extension using `this.outer`, also `O`. It represents the (negative) offset of the relation in clause `B`.
		  */
		def suffix :S PrefixOf B

		/** A proof that `B` is the [[net.noresttherein.oldsql.sql.RowProduct.Outer outer]] clause of `O`,
		  * the 'origin' clause for the relation.
		  */
		def outer :B OuterClauseOf O
	}


	implicit def outer[F <: FromSome, G <: FromSome, R[O] <: MappingAt[O], X]
	                  (implicit get :Found[F, G, X] { type O >: G <: FromSome })
			:GroupedTunnel[F Subselect R, G Subselect R, X] {
				type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
				type O = get.O Subselect R; type B = get.O; type I = get.I
			} =
		new EvidenceTemplate[F Subselect R, G Subselect R, X, get.S, get.O Subselect R, get.I, get.M, get.T](
			get.stretch.expand[get.O Subselect R])
			with GroupedTunnel[F Subselect R, G Subselect R, X]
		{
			override type B = get.O
			override def outer :B OuterClauseOf O = OuterClauseOf.subselect[get.O, R]
			override def suffix = get.stretch

			override def table[E <: RowProduct](from :G Subselect R)(implicit stretch :get.S PrefixOf E) =
				get.table(from.left)
		}


	implicit def tunnelJoin[F <: RowProduct, C <: RowProduct, G <: L J R, L <: U, R[O] <: MappingAt[O],
	                        J[+A <: U, B[O] <: R[O]] <: A NonSubselect B, U <: RowProduct, X]
	                       (implicit specific :RowDecomposition[F, C, _],
	                        general :ExpandedDecomposition[G, L, R, J, U],
	                        get :GroupedTunnel[C, L, X] { type O >: L <: U })
			:GroupedTunnel[F, G, X] {
				type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
				type O = general.S[get.O]; type B = get.B; type I = get.I
			} =
		new EvidenceTemplate[F, G, X, get.S, general.S[get.O], get.I, get.M, get.T](get.stretch.expand[J, R])
			with GroupedTunnel[F, G, X]
		{
			override type B = get.B
			override val suffix = get.suffix
			override val outer = OuterClauseOf.expanded[get.B, get.O J R, get.O, R, J, U](general.upcast[get.O], get.outer)

			override def table[E <: RowProduct](from :G)(implicit stretch :get.S PrefixOf E) =
				get.table(from.left)
		}

	implicit def tunnelDecorator[F <: D[B], B <: U, G <: D[C], C <: U,
	                             D[+A <: U] <: ExpandingDecorator[A], U <: RowProduct, X]
	                            (implicit specific :RowDecomposition[F, B, _],
	                             general :DecoratorDecomposition[G, C, D, U],
	                             get :GroupedTunnel[B, C, X] { type O >: C <: U })
			:GroupedTunnel[F, G, X] {
				type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
				type O = D[get.O]; type B = get.B; type I = get.I
			} =
		new EvidenceTemplate[F, G, X, get.S, D[get.O], get.I, get.M, get.T](
			get.stretch.expand(general.expansion[get.O]))
			with GroupedTunnel[F, G, X]
		{
			override type B = get.B
			override val suffix = get.suffix
			override val outer = OuterClauseOf.decorated[get.B, D[get.O], D, get.O, U](general.upcast[get.O], get.outer)

			override def table[E <: RowProduct](from :G)(implicit stretch :get.S PrefixOf E) =
				get.table(from.clause)
		}

}







/** Namespace containing implicit witnesses for the presence of a certain relation in a given `RowProduct`.
  * They are used to access joined relations by subject, index, label, etc.
  */
object GetTable {

	/** Shorthand alias for `RowProduct` subtypes with their
	  * [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form conforming to the type parameter `G`.
	  */
	type SpecificOf[G <: RowProduct] = RowProduct { type Generalized <: G }

	/** Base trait for various implicit evidence classes used to find a particular relation based on some key type `X`.
	  * It carries the mapping type `M` associated with the found relation, `S` - the `FromLast` type
	  * of the link with the found relation and `O`- a suffix clause of `G` which starts with `S` followed by all
	  * joins and relations exactly as they appear in the type `G`. The adaptation of the accessed relation
	  * from type `S` to `O` is done based on the [[net.noresttherein.oldsql.sql.RowProduct.PrefixOf PrefixOf]]
	  * instance representing the expansion.
	  * @tparam F The input clause type which is used to match the relation. In the
	  *           [[net.noresttherein.oldsql.sql.mechanics.GetTable.GetTableByPredicate GetTableByPredicate]]
	  *           class, it is the type for which the
	  *           [[net.noresttherein.oldsql.sql.mechanics.GetTable.GetTableByPredicate.Predicate predicate]]
	  *           must exist in order for the relation to be returned.
	  * @tparam G The [[net.noresttherein.oldsql.sql.RowProduct.Generalized Generalized]] type of `F`.
	  *           It must be a supertype of `F` on the suffix starting with the accessed relation.
	  *           It is provided here in order to statically bound `O` type used as `Origin` type of returned mappings
	  *           from below, hence making [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]`[O, M]`
	  *           a subtype of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[G, M[()]#Subject]`,
	  *           and thus usable in ''where'', ''select'', ''group by'' and ''having'' clauses of SQL ''selects''
	  *           based on type `F/G`.
	  * @tparam X The type given as the argument which determines some, implementation dependent,
	  *           relation characteristic of the accessed relation.
	  */
	trait RelationEvidence[-F <: RowProduct, -G <: RowProduct, X] {

		/** The type of the table expression specific to the returned relation. It is returned as defined
		  * by [[net.noresttherein.oldsql.sql.RowProduct.Last Last]] of the row prefix which ends with
		  * the returned relation. In practice, it is [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]]
		  * for 'true' tables from the ''from'' clause
		  * and [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] for others.
		  */
		type T[O <: RowProduct] <: JoinedRelation[O, M]

		/** The accessed `Mapping` type, matching the key `X`. */
		type M[O] <: MappingAt[O]

		/** The supertype of the generalized supertype `G` of the input ''from'' clause `F`, in which the search
		  * takes place, resulting from replacing the [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] link
		  * having the found relation as its right side with `S` - the `FromLast` type of that type.
		  * The returned relation is thus the first relation in this type, and the number of relations listed
		  * defines its right-based index in `F`. This type is used as
		  * the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the returned mapping
		  * and the (invariant) base type for the returned
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]], which allows to trace back
		  * any component of their components back to their source.
		  */
		type O >: G <: RowProduct

		/** The clause type that the original `JoinedRelation` at index `N` is  parameterized with,
		  * i.e. `FromLast` type of the accessed join.
		  */
		type S <: RowProduct

		/** The ''negative'' index of the accessed relation, starting with `-1` for the rightmost relation in `F`.*/
		type I <: Numeral

		/** Extension of the initial clause `S` with the found relation as the last one to the final clause `O`. */
		def stretch :S PrefixOf O

		/** The offset of the found table as an evidence that it is the first mapping listed by returned type
		  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.RelationEvidence.O O]].
		  */
		def offset :TableOffset[O, M] { type N = I } =
			new TableOffset[O, M](stretch.lengthDiff).asInstanceOf[TableOffset[O, M]{ type N = I }]

		/** Getter for the matching relation.
		  * @return `this.table(from)(this.stretch)`.
		  */
		def apply(from :G) :T[O] = table(from)(stretch)

		def table[E <: RowProduct](from :G)(implicit stretch :S PrefixOf E) :T[E]
	}


	protected[GetTable] abstract class EvidenceTemplate[-F <: RowProduct, G <: RowProduct, X,
	                                                    C <: RowProduct, U >: G <: RowProduct, N <: Numeral,
	                                                    E[A] <: MappingAt[A], R[O <: RowProduct] <: JoinedRelation[O, E]]
	                                                   (override val stretch :C PrefixOf U)
		extends RelationEvidence[F, G, X]
	{
		override type T[O <: RowProduct] = R[O]
		override type M[O] = E[O]
		override type O = U
		override type S = C
		override type I = N
	}


	/** An implicit result of a search for a relation matching the key type `X` in the ''from'' clause `F`.
	  * The meaning of `X` and the matching rules are specified by the subclasses of the enclosing
	  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.GetTableByPredicate GetTableByPredicate]].
	  * This is an implementation class which acts solely as a wrapper over another evidence instance -
	  * typically a `Found[F, X, N]` - and is used as a base class for the final evidence class dedicated
	  * to the particular `GetTableByPredicate` subclass, such as `ByIndex`, `ByLabel`, etc. Its role
	  * is simply to lift the need to define delegates for all member types and methods with each
	  * such 'public' implicit definition.
	  * @tparam F the input `RowProduct` from which the relation is taken.
	  * @tparam G the `Generalized` type of `F`.
	  * @tparam X the 'key' type used to match the `Mapping` types of all relations in search for an implicit
	  *           `Predicate[F, M[Any], X]`.
	  * @tparam U the `Origin` type for the mapping, which is a supertype of `F` resulting from replacing
	  *           the prefix clause which contains the accessed relation with its `FromLast` type.
	  * @tparam E the mapping of the accessed relation.
	  * @tparam N the negative index of the accessed relation in the clause `F` - it starts with -1
	  *           for the rightmost relation and decreases with each relation going to the left.
	  */
	private[mechanics] abstract class Delegate[-F <: RowProduct, G <: RowProduct, X, U >: G <: RowProduct, N <: Numeral,
	                                           E[A] <: MappingAt[A], R[A <: RowProduct] <: JoinedRelation[A, E]]
	                            (val evidence :RelationEvidence[F, G, X]
	                                { type T[O <: RowProduct] = R[O]; type M[O] = E[O]; type O = U; type I = N })
		extends RelationEvidence[F, G, X]
	{
		override type T[O <: RowProduct] = R[O]
		override type M[O] = E[O]
		override type O = U
		override type S = evidence.S
		override type I = N

		override def stretch :S PrefixOf O = evidence.stretch

		override def table[C <: RowProduct](from :G)(implicit stretch :S PrefixOf C) :T[C] = evidence.table(from)
	}



	/** Provides the bulk of implicit definitions for its
	  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.GetTableByPredicate GetTableByPredicate]] subclass,
	  * except for the actual reporting of initial [[net.noresttherein.oldsql.sql.mechanics.GetTable.Found Found]]
	  * evidence - which should always have precedence, in case several subclauses of the accessed
	  * `RowProduct` match the predicate.
	  */
	abstract class GetTableTemplate extends GetTable {

		/** Witnesses that mapping `M[Any]` (where `Any` is the `Origin` type), being the last mapping
		  * of the ''from'' clause `F` satisfies the search predicate and should be returned.
		  * Note that the predicate is contravariant with regard to the clause type `F`, meaning that
		  * if it holds for some clause `F`, it holds also for all its subtypes, in particular those
		  * with a longer instantiated prefix and the join types being a subtype of the matched join,
		  * as well as joins aliased with the [[net.noresttherein.oldsql.sql.RowProduct.As As]] type.
		  * Instances of `GetTableByPredicate` typically report this implicit using the
		  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.GetTableByPredicate.report GetTableByPredicate.report]]
		  * method for the most generic clause containing a matching mapping in its last relation, and which
		  * retains its upper bound of
		  * [[net.noresttherein.oldsql.sql.FromSome FromSome]]/[[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]].
		  * @tparam F a ''from'' clause with `M` as the last mapping, satisfying this predicate.
		  *           It is matched with the first type argument of
		  *           [[net.noresttherein.oldsql.sql.mechanics.GetTable.Found Found]] implicit evidence returning
		  *           the last relation in `F`.
		  * @tparam X the input key of the search: the type provided by the accessor, such as a label for a `LabeledMapping`.
		  */
		@implicitNotFound("The last mapping of FROM clause ${F} does not match the key type ${X}.")
		final class Predicate[-F <: RowProduct, X]

		private[this] final val predicate = new Predicate[RowProduct, Any]

		/** Subclasses can use this method as the implicit definition for `Predicate` witnessing that
		  * the clause `F`/its last relation match some, specific to implementation, key `X`.
		  */
		protected def report[F <: RowProduct, X] :Predicate[F, X] = predicate.asInstanceOf[Predicate[F, X]]


		//this includes Subselect, but we don't mind, because in FromClause it isn't treated differently,
		//and in a GroupByClause we search for GroupedTunnel, not Return/Found
		implicit def expanded[F <: RowProduct, P <: RowProduct, G <: L J R, L <: U, R[O] <: MappingAt[O],
		                      J[+A <: U, B[O] <: R[O]] <: A Expanded B, U <: RowProduct,
		                      X, V <: Numeral, W <: Numeral]
		                     (implicit specific :RowDecomposition[F, P, _],
		                      general :ExpandedDecomposition[G, L, R, J, U],
		                      get :Found[P, L, X] { type O >: L <: U; type I = W }, dec :Inc[V, W])
				:Return[F, G, X] {
					type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
					type O = general.S[get.O]; type I = V
				} =
			forward[F, G, L, U, X, V](get, general)


		implicit def decorated[F <: RowProduct, B <: RowProduct, G <: D[C], C <: U,
		                       D[+A <: U] <: ExpandingDecorator[A], U <: RowProduct, X]
		                      (implicit specific :RowDecomposition[F, B, _],
		                       general :DecoratorDecomposition[G, C, D, U],
		                       get :Found[B, C, X] { type O >: C <: U })
				:Return[F, G, X] {
					type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
					type O = general.S[get.O]; type I = get.I
				} =
			forward[F, G, C, U, X, get.I](get, general)


		implicit def grouped[P <: FromSome, L <: FromSome, R[O] <: MappingAt[O], X, V <: Numeral, W <: Numeral]
		                    (implicit get :GroupedTunnel[P, L, X] { type O >: L <: FromSome; type I = W }, dec :Inc[V, W])
				:Return[P GroupBy R, L GroupBy R, X]
					{ type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = get.O GroupBy R; type I = V } =
			left[P GroupBy R, L GroupBy R, L, R, GroupBy, FromSome, X, V](get)(get.suffix.group(get.outer))


		implicit def aggregated[F <: FromSome, G <: FromSome, X]
		                       (implicit get :GroupedTunnel[F, G, X] { type O >: G <: FromSome })
				:Return[Aggregated[F], Aggregated[G], X] {
					type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
					type O = Aggregated[get.O]; type I = get.I
				} =
			body[Aggregated[F], Aggregated[G], G, Aggregated, FromSome, X, get.I](get)(
				get.suffix.aggregate(get.outer)
			)

	}



	/** Implicit resolution of retrieving a relation from a ''from'' clause where the `Mapping` type satisfies
	  * some predicate. The search starts with the last (rightmost) relation in the clause and goes backward,
	  * looking for the first relation with an implicit `Predicate[M, X]` by recursively reducing the input
	  * ''from'' clause to its prefix.
	  */
	abstract class GetTableByPredicate extends GetTableTemplate {

		//has to have priority as there is ambiguity with As and several mappings matching in general
		implicit def last[F <: NonEmptyFrom, G <: NonEmptyFrom, M[O] <: MappingAt[O], X]
		                 (implicit last :LastTableOf[G] { type LastMapping[O] = M[O] },
		                  check :LastTableOf[F] { type LastMapping[O] = M[O] },
		                  pred :Predicate[F, X])
				:Return[F, G, X] {
					type T[O <: RowProduct] = last.Last[O]; type M[O] = last.LastMapping[O]
					type O = last.FromLast; type I = -1
				} =
			found
	}






	/** Implicit resolution of the `N`-th relation in the ''from'' clause `F`. This works both for positive numbers,
	  * indexed from zero and going from left to right (in which case `F` must be complete), and negative -
	  * indexed from `-1` and going from right to left (which is available always, but whose index changes with
	  * joining new tables). If `0 <= N` and an implicit `ByIndex[A, B, M]` for some `0 <= M` is present
	  * in the ''lexical'' scope (where `A` is a prefix type of `F` and `B` is its generalized form),
	  * it will be used as the initial evidence for any `N >= M`, without backtracking
	  * all the way to `Dual/From`, providing that all joins and their generalized forms since `A`/`B`
	  * are statically known. Similarly, if `X < 0`, than an implicit `ByIndex[A, B, X]` can be converted under
	  * these circumstances to `ByIndex[F, G, N]`. In both cases, `abs(N) - abs(X)` must equal the number
	  * of relations joined in `F` since `A`, i.e.
	  * (`A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F).length`.
	  * Note that all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * and a following [[net.noresttherein.oldsql.sql.GroupBy]] (including its type aliases) are ignored:
	  * the positive/negative indices of B and G in `From[A] Join B Subselect C Join D GroupBy G By E`
	  * are `1/-3` and `2/-2`, respectively, with no indices associated with relations `C` and `D`.
	  * @tparam G the [[net.noresttherein.oldsql.sql.RowProduct.Generalized Generalized]] type of `F`.
	  * @tparam N index of the desired relation as a literal `Int` type.
	  */
	@implicitNotFound("Cannot get ${N}-th relation of the FROM clause ${F}.\n" +
	                  "Either ${N} >= size (where size is the number of relations in the whole FROM clause),\n" +
	                  "or -(${N}) is greater the number of relations in the instantiated suffix of the generalized clause,\n" +
	                  "or ${N} >= 0 and the size is not known (the clause starts with a wildcard or abstract type)\n" +
	                  "or its generalized type ${G} is not known/is too abstract on the required span.\n" +
	                  "Missing implicit GetTable.ByIndex[F, G, N].")
	sealed trait ByIndex[-F <: RowProduct, G <: RowProduct, N <: Numeral] extends RelationEvidence[F, G, N]


	/** Provides implicit values of [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByIndex! ByIndex]],
	  * used as accessor objects/lenses returning the relation at the given position in the input clause.
	  * They are converted from implicit
	  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByIndex.ByPositiveIndex ByPositiveIndex]]`.`[[net.noresttherein.oldsql.sql.mechanics.GetTable.Return Return]]
	  * and [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByIndex.ByNegativeIndex ByNegativeIndex]]`.Return`,
	  * based on whether `N` is positive or negative.
	  */
	object ByIndex {

		/** Summon an implicit instance of [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByIndex! ByIndex]]
		  * for the given type arguments.
		  */
		def apply[F <: RowProduct, G <: RowProduct, N <: Numeral](implicit get :ByIndex[F, G, N]) :get.type = get


		implicit def byPositiveIndex[F <: RowProduct, G <: RowProduct, N <: Numeral]
		             (implicit left2right :Positive[N], found :ByPositiveIndex.Return[F, G, N])
				:ByIndex[F, G, N] {
					type T[O <: RowProduct] = found.T[O]; type M[O] = found.M[O]
					type O = found.O; type I = found.I
				} =
			new Delegate[F, G, N, found.O, found.I, found.M, found.T](found)
				with ByIndex[F, G, N] with ByPositiveIndex.Found[F, G, N]

		implicit def byNegativeIndex[F <: RowProduct, G <: RowProduct, N <: Numeral]
		             (implicit right2left :Negative[N], found :ByNegativeIndex.Return[F, G, N] { type I = N })
				:ByIndex[F, G, N] { type T[O <: RowProduct] = found.T[O]; type M[O] = found.M[O]; type O = found.O } =
			new Delegate[F, G, N, found.O, N, found.M, found.T](found)
				with ByIndex[F, G, N] with ByNegativeIndex.Found[F, G, N]


		object ByPositiveIndex extends GetTableByPredicate {
			override type Key = Numeral
			override type Get[-F <: RowProduct, G <: RowProduct, X <: Numeral] = ByIndex[F, G, X]

			implicit def satisfies[L <: RowProduct, R[O] <: MappingAt[O], N <: Numeral]
			                      (implicit size :RowProductSize[L, N]) :Predicate[L Expanded R, N] =
				report

			implicit def groupSatisfies[O <: RowProduct, L <: FromSome, R[A] <: MappingAt[A], N <: Numeral]
			                           (implicit outer :O OuterClauseOf L, size :RowProductSize[O, N])
//			                           (implicit outer :GetOuter[L, L, O], size :RowProductSize[O, N])
					:Predicate[L GroupBy R, N] =
				report
		}


		//ByNegativeIndex is special, because it must decrease the index (key) with every step
		//and the order of implicits.
		sealed abstract class ByNegativeIndexFeedback extends GetTable {

			/** Adapts an existing implicit value of `ByIndex[F, N]` into a `ByNegativeIndex.Found[F, X] { type I = N }`
			  * to use, especially when `F` is incomplete. This process allows
			  *///can't use the inherited one from GetTable as we must check if N is negative; NegativeInc will exist for 0
			implicit def byIndexFeedback[F <: RowProduct, G <: RowProduct, X <: Numeral]
			                            (implicit negative :Negative[X], get :ByIndex[F, G, X])
					:Found[F, G, X] {
						type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
						type O = get.O; type I = X
					} =
				get match {
					case pos :GetTable#Found[_, _, _] if pos.how == ByNegativeIndex =>
						pos.asInstanceOf[Found[F, G, X] {
							type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = get.O; type I = X
						}]
					case _ =>
						new EvidenceTemplate[F, G, X, get.S, get.O, X, get.M, get.T](get.stretch) with Found[F, G, X] {
							override def table[E <: RowProduct](from :G)(implicit stretch :get.S PrefixOf E) =
								get.table(from)
						}
				}
		}


		sealed abstract class ByNegativeIndexRecursion extends ByNegativeIndexFeedback {

			implicit def expanded[F <: RowProduct, P <: RowProduct, G <: L J R, L <: U, R[O] <: MappingAt[O],
			                      J[+A <: U, B[O] <: R[O]] <: A Expanded B, U <: RowProduct,
			                      V <: Numeral, W <: Numeral]
			                     (implicit specific :RowDecomposition[F, P, _],
			                      general :ExpandedDecomposition[G, L, R, J, U],
			                      dec :NegativeInc[V, W], get :Found[P, L, W] { type O >: L <: U })
					:Return[F, G, V] {
						type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
						type O = general.S[get.O]; type I = V
					} =
				forward[F, G, L, U, V, V](get, general)

			implicit def decorated[F <: RowProduct, P <: RowProduct, G <: D[C], C <: U,
			                       D[+A <: U] <: ExpandingDecorator[A], U <: RowProduct, X <: Numeral]
			                      (implicit specific :RowDecomposition[F, P, _],
			                       general :DecoratorDecomposition[G, C, D, U],
			                       get :Found[P, C, X] { type O >: C <: U })
					:Return[F, G, X] {
						type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
						type O = general.S[get.O]; type I = X
					} =
				forward[F, G, C, U, X, X](get, general)

			implicit def grouped[P <: FromSome, L <: FromSome, R[O] <: MappingAt[O], V <: Numeral, W <: Numeral]
			                    (implicit dec :NegativeInc[V, W],
			                              get :GroupedTunnel[P, L, W] { type O >: L <: FromSome })
					:Return[P GroupBy R, L GroupBy R, V] {
						type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
						type O = get.O GroupBy R; type I = V
					} =
				left[P GroupBy R, L GroupBy R, L, R, GroupBy, FromSome, V, V](get)(
					get.suffix.group(get.outer)
				)

			implicit def aggregated[F <: FromSome, G <: FromSome, X <: Numeral]
			                       (implicit get :GroupedTunnel[F, G, X] { type O >: G <: FromSome })
					:Return[Aggregated[F], Aggregated[G], X] {
						type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
						type O = Aggregated[get.O]; type I = X
					} =
				body[Aggregated[F], Aggregated[G], G, Aggregated, FromSome, X, X](get)(
					get.suffix.aggregate(get.outer)
				)

		}


		object ByNegativeIndex extends ByNegativeIndexRecursion {

			implicit def last[F <: NonEmptyFrom, G <: NonEmptyFrom, E[O] <: MappingAt[O]]
			                 (implicit last :LastTableOf[G] { type LastMapping[O] = E[O] },
			                           check :LastTableOf[F] { type LastMapping[O] = E[O] })
					:Return[F, G, -1] {
						type T[O <: RowProduct] = last.Last[O]; type M[O] = E[O]
						type O = last.FromLast; type I = -1
					} =
				found
		}

	}




	/** Accessor for the right-most relation in `F` with mapping conforming to
	  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]`[N, _, _]`.
	  * For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * and a following [[net.noresttherein.oldsql.sql.GroupBy]] (including its type aliases) are ignored.
	  * Note that an implicit `ByLabel[A, B, N]` can be converted to a `ByLabel[F, G, N]`
	  * if `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * and the join types in `F` and `G` since `A` and `B` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	@implicitNotFound("No relation with Mapping labeled ${N} in the FROM clause ${F}:\n" +
	                  "no implicit value for GetTable.ByLabel[${F}, ${G}, ${N}].")
	sealed trait ByLabel[-F <: RowProduct, G <: RowProduct, N <: Label] extends RelationEvidence[F, G, N] {
		override type M[O] <: LabeledMapping[N, _, O]
	}


	/** Provides implicit values of [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByLabel! ByLabel]] -
	  * accessors/lenses returning a [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]
	  * with the specified `Label` type parameter. An implicit `ByLabel[F, G, N]` returns the last relation
	  * with label `N` in `F` (and `G`).
	  */
	object ByLabel extends GetTableByPredicate {
		override type Key = Label
		override type Get[-F <: RowProduct, G <: RowProduct, A <: Label] = ByLabel[F, G, A]

		implicit def byLabel[F <: RowProduct, G <: RowProduct, A <: Label]
		                    (implicit found :Return[F, G, A] { type M[O] <: LabeledMapping[A, _, O] })
				:ByLabel[F, G, A]
					{ type T[O <: RowProduct] = found.T[O]; type M[O] = found.M[O]; type O = found.O; type I = found.I } =
			new Delegate[F, G, A, found.O, found.I, found.M, found.T](found) with ByLabel[F, G, A]

		implicit def satisfies[M[O] <: LabeledMapping[L, _, O], L <: Label] :Predicate[RowProduct Adjoin M, L] =
			report
	}




    /** Implicit witness accessing the last relation in the ''from'' clause `F` with alias `A`.
	  * It is defined as the last relation of the clause `L`, such that
	  * `L` [[net.noresttherein.oldsql.sql.RowProduct.As As]] `A` appears as a part of type `F` and
	  * is the right-most such occurrence. As the generalized supertype of `P As A` is the same as `P`
	  * (the alias information is lost for interoperability of SQL expressions), the check is performed
	  * only against type `F`, and not its generalization `G`.
	  * For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * and a following [[net.noresttherein.oldsql.sql.GroupBy]] (including its type aliases) are ignored.
	  * Note that an implicit `ByAlias[K, L, A]` can be converted to a `ByAlias[F, G, A]`
	  * if `K `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `L ExpandedBy G`,
	  * and the join types in `F` and `G` since `K` and `L` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `K, L, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	@implicitNotFound("No relation with alias ${A} appears in the FROM clause ${F}:\n" +
	                  "no implicit value for GetTable.ByAlias[${F}, ${G}, ${A}].")
	sealed trait ByAlias[-F <: RowProduct, G <: RowProduct, A <: Label] extends RelationEvidence[F, G, A]


	/** Provides implicit values of [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByAlias! ByAlias]] -
	  * accessors/lenses returning relations directly followed by
	  * [[net.noresttherein.oldsql.sql.RowProduct.As As]] subclause for a specified String literal.
	  * An implicit `ByAlias[F, G, A]` returns the last relation with alias `A` in `F` (and `G`).
	  */
	object ByAlias extends GetTableByPredicate {
		override type Key = Label
		override type Get[-F <: RowProduct, G <: RowProduct, X <: Label] = ByAlias[F, G, X]

		implicit def byAlias[F <: RowProduct, G <: RowProduct, A <: Label](implicit found :Return[F, G, A])
				:ByAlias[F, G, A]
					{ type T[O <: RowProduct] = found.T[O]; type M[O] = found.M[O]; type O = found.O; type I = found.I } =
			new Delegate[F, G, A, found.O, found.I, found.M, found.T](found) with ByAlias[F, G, A]

		implicit def satisfies[M[O] <: MappingAt[O], A <: Label] :Predicate[NonEmptyFrom As A, A] = report
	}




	/** An implicit accessor object for the last relation in `F` with `Subject` type `S`.
	  * The type and index of the relation are returned as members `M[O]` and `I`/ `shift :I`.
	  * For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * and a following [[net.noresttherein.oldsql.sql.GroupBy]] (including its type aliases) are ignored.
	  * Note that an implicit `BySubject[A, B, S]` can be converted to a `BySubject[F, G, S]`
	  * if `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * and the join types in `F` and `G` since `A` and `B` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	@implicitNotFound("No relation with Subject type ${X} appears in the FROM clause ${F}:\n" +
	                  "no implicit value for GetTable.BySubject[${F}, ${G}, ${X}].")
	sealed trait BySubject[-F <: RowProduct, G <: RowProduct, X] extends RelationEvidence[F, G, X] {
		override type M[O] <: RefinedMapping[X, O]
	}


	/** Provides implicit values of [[net.noresttherein.oldsql.sql.mechanics.GetTable.BySubject! BySubject]] -
	  * accessors/lenses returning relations based on their specified
	  * [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type. An implicit `BySubject[F, G, S]` returns
	  * the last relation conforming to [[net.noresttherein.oldsql.schema.Mapping.MappingOf MappingOf]]`[S]`
	  * in `F` (and `G`).
	  */
	object BySubject extends GetTableByPredicate {
		override type Key = Any
		override type Get[-F <: RowProduct, G <: RowProduct, S] = BySubject[F, G, S]

		implicit def bySubject[F <: RowProduct, G <: RowProduct, X] //type name S confuses the compiler which mixes it with RelationEvidence.S
		                      (implicit found :Return[F, G, X] { type M[O] <: RefinedMapping[X, O] })
				:BySubject[F, G, X]
					{ type T[O <: RowProduct] = found.T[O]; type M[O] = found.M[O]; type O = found.O; type I = found.I } =
			new Delegate[F, G, X, found.O, found.I, found.M, found.T](found) with BySubject[F, G, X]


		implicit def satisfies[M[O] <: RefinedMapping[S, O], S] :Predicate[RowProduct Adjoin M, S] =
			report[RowProduct Adjoin M, S]

	}



	/** An implicit accessor object for the last relation in `F`
	  * for a [[net.noresttherein.oldsql.schema.Mapping Mapping]] with type constructor `M`.
	  * The argument type of the type constructor must be the mapping's `Origin` type.
	  * The type and index of the relation are returned as members `M[O]` and `I`/ `shift :I`.
	  * For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * and a following [[net.noresttherein.oldsql.sql.GroupBy]] (including its type aliases) are ignored.
	  * Note that an implicit `ByType[A, B, N]` can be converted to a `ByType[F, G, N]`
	  * if `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * and the join types in `F` and `G` since `A` and `B` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	@implicitNotFound("No relation with type constructor ${C} in the FROM clause ${F}:\n" +
	                  "no implicit value for GetTable.ByType[${F}, ${G}, ${C}].")
	sealed trait ByType[-F <: RowProduct, G <: RowProduct, C[O] <: MappingAt[O]]
		extends RelationEvidence[F, G, C[Unit]]
	{
		override type M[O] = C[O]
	}


	/** Provides implicit values of [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByType ByType]]
	  * accessing relations in a `RowProduct` based on a type constructor for a `Mapping` subtype accepting
	  * the `Origin` type. While the input to match accepts only a single parameter, it is possible to match mappings
	  * with multiple type parameters as long as all of them are fixed by using a ''type lambda'' as the argument:
	  * `({ type M[O] = SomeMapping[X1, X2, ..., O] })#M`.
	  * Note that an implicit `ByType[A, B, M]` can be converted to a `ByType[F, G, M]`
	  * if `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * and the join types in `F` and `G` since `A` and `B` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  *  - `ExpandedBy` from a lexical scope is not sufficient.
	  */
	object ByType extends GetTableByPredicate {

		implicit def byTypeConstructor[F <: RowProduct, G <: RowProduct, C[O] <: MappingAt[O]]
		                              (implicit found :Return[F, G, C[Unit]] { type M[O] = C[O] })
				:ByType[F, G, C]
					{ type T[O <: RowProduct] = found.T[O]; type M[O] = found.M[O]; type O = found.O; type I = found.I } =
			new Delegate[F, G, C[Unit], found.O, found.I, found.M, found.T](found) with ByType[F, G, C]

		implicit def satisfies[M[O] <: MappingAt[O]] :Predicate[RowProduct Adjoin M, M[Unit]] = report

	}




	/** Implicit resolution of the `N`-th [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameter
	  * in the ''from'' clause `F`. It works both for positive numbers, indexed from zero and going
	  * from left to right (in which case `F` must be complete or start
	  * with a [[net.noresttherein.oldsql.sql.RowProduct.ParamlessFrom ParamlessFrom]]), and negative -
	  * indexed from `-1` and going from right to left (which is available always, but whose index changes
	  * with joining of new tables). Note that the index is relative only to other parameters, not all joined
	  * relations, which simplifies access and has two consequences: it is the index of the corresponding parameter
	  * in the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] list of the clause and, unlike
	  * with [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByIndex ByIndex]], all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * and a following [[net.noresttherein.oldsql.sql.GroupBy]] are still included in the counting
	  * (but remain inaccessible due to the lack of an implicit value of this evidence for the particular index).
	  *
	  * While the mapping could be also returned based on any other criteria (for example, its absolute index),
	  * its type as provided by this class is statically narrowed down to
	  * [[net.noresttherein.oldsql.sql.ParamClause.ParamAt ParamAt]].
	  * An implicit `ByParamIndex[A, B, N]` can be converted to a `ByParamIndex[F, G, N]`
	  * if `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * and the join types in `F` and `G` since `A` and `B` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  * @tparam F the input `RowProduct`.
	  * @tparam N index of the desired parameter as a literal `Int` type.
	  */
	@implicitNotFound("Cannot get the ${N}-th unbound parameter of the FROM clause ${F}.\n" +
	                  "Either ${N} is greater or equal to the total number of parameters,\n" +
		              "or -(${N}) is greater than the number of parameters in the instantiated suffix,\n" +
		              "or ${N} >= 0 and the clause is incomplete (starts with a wildcard or abstract type).\n" +
	                  "Missing implicit GetTable.ByParamIndex[${F}, ${G}, ${N}].")
	sealed trait ByParamIndex[-F <: RowProduct, G <: RowProduct, N <: Numeral] extends RelationEvidence[F, G, N] {
		type M[O] <: ParamAt[O]
	}


	/** Provides implicit values of [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByParamIndex ByParamIndex]],
	  * accessing [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters of a `RowProduct` based on
	  * their position in the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] list of the clause.
	  * For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * If an implicit `ByParamIndex[A, B, M]` is present in the lexical scope for some `M >= 0`,
	  * and `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * as well as the join types in `F` and `G` since `A` and `B` are known at least to their generalized
	  * super type, it can be converted into a `ByParamIndex[F, G, N]` such that `N - M = (A ExpandedBy F).length`.
	  * The situation for negative indices is analogous.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	object ByParamIndex {

		implicit def byPositiveParamIndex[F <: RowProduct, G <: RowProduct, N <: Numeral]
		                                 (implicit positive :Positive[N],
		                                  get :ByPositiveParamIndex.Return[F, G, N] { type M[O] <: ParamAt[O] })
				:ByParamIndex[F, G, N]
					{ type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = get.O; type I = get.I } =
			new Delegate[F, G, N, get.O, get.I, get.M, get.T](get)
				with ByParamIndex[F, G, N] with ByPositiveParamIndex.Found[F, G, N]

		implicit def byNegativeParamIndex[F <: RowProduct, G <: RowProduct, N <: Numeral]
		                                 (implicit negative :Negative[N],
		                                  get :ByNegativeParamIndex.Return[F, G, N] { type M[O] <: ParamAt[O] })
				:ByParamIndex[F, G, N]
					{ type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = get.O; type I = get.I } =
			new Delegate[F, G, N, get.O, get.I, get.M, get.T](get)
				with ByParamIndex[F, G, N] with ByNegativeParamIndex.Found[F, G, N]


		object ByPositiveParamIndex extends GetTableByPredicate {

			implicit def satisfies[F <: NonEmptyFrom, P[O] <: ParamAt[O], N <: Numeral]
			                      (implicit preceding :ChainLength[F#Params, N])
					:Predicate[F ParamClause P, N] =
				report
		}


		sealed abstract class ByNegativeParamIndexFeedback extends GetTableTemplate {
			//can't use the 'free' inherited feedback implicit because we must assert that N is negative first.
			implicit def feedback[F <: RowProduct, G <: RowProduct, N <: Numeral]
			                     (implicit negative :Negative[N], get :ByParamIndex[F, G, N])
					:Found[F, G, N] { type M[O] = get.M[O]; type O = get.O; type I = get.I } =
				get match {
					case pos :GetTableByPredicate#Found[_, _, _] if pos.how == this =>
						pos.asInstanceOf[Found[F, G, N] {
							type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]; type O = get.O; type I = get.I
						}]
					case _ =>
						new Delegate[F, G, N, get.O, get.I, get.M, get.T](get) with Found[F, G, N]
				}
		}


		sealed abstract class ByNegativeParamIndexParams extends ByNegativeParamIndexFeedback {

			/** Decrease the absolute value of the index type parameter and search in the left side
			  * of the unbound parameter. This implicit has precedence over all standard inherited definitions,
			  * but not over `joinParam` and `groupParam` for `X =:= -1`.
			  */
			implicit def param[F <: RowProduct, P <: RowProduct, G <: L J R, L <: U, R[O] <: ParamAt[O],
			                   J[+A <: U, B[O] <: ParamAt[O]] <: A ParamClause B, U <: NonEmptyFrom,
			                   X <: Numeral, Y <: Numeral, V <: Numeral, W <: Numeral]
			                  (implicit specific :RowDecomposition[F, P, _],
			                   general :ExpandedDecomposition[G, L, R, J, U],
			                   key :NegativeInc[X, Y], get :Found[P, L, Y] { type O >: L <: U; type I = W },
			                   idx :NegativeInc[V, W])
					:Return[F, G, X] {
						type T[O <: RowProduct] = get.T[O]; type M[O] = get.M[O]
						type O = general.S[get.O]; type I = V
					} =
				forward[F, G, L, U, X, V](get, general)
		}


		object ByNegativeParamIndex extends ByNegativeParamIndexParams {

			implicit def joinParam[P[O] <: ParamAt[O], L <: FromSome]
					:Return[FromSome JoinParam P, L JoinParam P, -1] {
						type T[O <: RowProduct] = JoinedRelation[O, P]; type M[O] = P[O]
						type O = RowProduct AndFrom P; type I = -1
					} =
				found

			implicit def groupParam[P[O] <: ParamAt[O], L <: GroupByClause]
					:Return[GroupByClause GroupParam P, L GroupParam P, -1] {
						type T[O <: RowProduct] = JoinedRelation[O, P]; type M[O] = P[O]
						type O = GroupByClause AndBy P; type I = -1
					} =
				found
		}

	}




	/** Accessor for the right-most relation in `F` with a mapping conforming to
	  * [[net.noresttherein.oldsql.sql.ParamClause.LabeledUnboundParam LabeledUnboundParam]]`[N, _, _]`, providing
	  * it is joined using either [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] or
	  * [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]. The parameter name in the sense of this class
	  * is thus not the same as any [[net.noresttherein.oldsql.sql.RowProduct.As alias]] given to this relation.
	  * Note that such a parameter could be accessed also using
	  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByLabel ByLabel]], however this evidence
	  * is more restrictive and has statically narrowed return type to the dedicated parameter `Mapping`.
	  * For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * and a following [[net.noresttherein.oldsql.sql.GroupBy]] (including its type aliases) are ignored.
	  * An implicit `ByParamName[A, B, N]` can be converted to a `ByParamName[F, G, N]`
	  * if `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * and the join types in `F` and `G` since `A` and `B` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	@implicitNotFound("No parameter with name type ${N} in the FROM clause ${F}:\n" +
	                  "no implicit value for GetTable.ByParamName[${F}, ${G}, ${N}].\n" +
	                  "Note that a parameter name is a label of a Mapping, not a relation alias.")
	sealed trait ByParamName[-F <: RowProduct, G <: RowProduct, N <: Label] extends ByParamName.Found[F, G, N] {
		type M[O] <: LabeledUnboundParam[N, _, O]
	}


	/** Provides implicit values of [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByParamName ByParamName]],
	  * accessing [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters of a `RowProduct` based on
	  * a given parameter name used as their label. Note that this matches parameter mappings derived from
	  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]], and not a relation alias introduced
	  * by [[net.noresttherein.oldsql.sql.RowProduct.As As]].
	  * For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * An implicit `ByParamName[A, B, N]` can be converted to a `ByParamName[F, G, N]`
	  * if `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * and the join types in `F` and `G` since `A` and `B` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	object ByParamName extends GetTableByPredicate {
		type Key = Label
		override type Get[-F <: RowProduct, G <: RowProduct, N <: Label] = ByParamName[F, G, N]

		implicit def byParamName[F <: RowProduct, G <: RowProduct, A <: Label]
		                        (implicit found :Return[F, G, A] { type M[O] <: LabeledUnboundParam[A, _, O] })
				:ByParamName[F, G, A]
					{ type T[O <: RowProduct] = found.T[O]; type M[O] = found.M[O]; type O = found.O; type I = found.I } =
			new Delegate[F, G, A, found.O, found.I, found.M, found.T](found) with ByParamName[F, G, A]

		implicit def satisfies[M[O] <: LabeledUnboundParam[N, _, O], N <: Label]
				:Predicate[NonEmptyFrom ParamClause M, N] =
			report[NonEmptyFrom ParamClause M, N]

	}




    /** Implicit witness accessing the last parameter in the ''from'' clause `F` with alias `A`.
	  * It is defined as the pseudo relation for mapping `M` from the last occurrence of
      * a pseudo join in the form of `L `[[net.noresttherein.oldsql.sql.ParamClause ParamClause]]` M As A`
      * (or its subclass). As the generalized supertype of `P As A` is the same as `P`
	  * (the alias information is lost for interoperability of SQL expressions), the check is performed only against
      * type `F`, and not its generalization `G`. For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * and a following [[net.noresttherein.oldsql.sql.GroupBy]] (including its type aliases) are ignored.
	  * Note that an implicit `ByParamAlias[K, L, A]` can be converted to a `ByParamAlias[F, G, A]`
	  * if `K `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `L ExpandedBy G`,
	  * and the join types in `F` and `G` since `K` and `L` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `K, L, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	@implicitNotFound("No parameter with alias ${A} appears in the FROM clause ${F}:\n" +
	                  "no implicit value for GetTable.ByParamAlias[${F}, ${G}, ${A}].")
	sealed trait ByParamAlias[-F <: RowProduct, G <: RowProduct, A <: Label] extends RelationEvidence[F, G, A]


	/** Provides implicit values of [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByParamAlias! ByParamAlias]] -
	  * accessors/lenses returning parameter mappings directly followed by
	  * [[net.noresttherein.oldsql.sql.RowProduct.As As]] subclause for a specified `String` literal.
	  * An implicit `ByParamAlias[F, G, A]` returns the last relation joined with a
	  * [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] with alias `A` in `F` (and `G`).
	  */
	object ByParamAlias extends GetTableByPredicate {
		override type Key = Label
		override type Get[-F <: RowProduct, G <: RowProduct, X <: Label] = ByParamAlias[F, G, X]

		implicit def byParamAlias[F <: RowProduct, G <: RowProduct, A <: Label](implicit found :Return[F, G, A])
				:ByParamAlias[F, G, A]
					{ type T[O <: RowProduct] = found.T[O]; type M[O] = found.M[O]; type O = found.O; type I = found.I } =
			new Delegate[F, G, A, found.O, found.I, found.M, found.T](found) with ByParamAlias[F, G, A]

		implicit def satisfies[M[O] <: ParamAt[O], A <: Label] :Predicate[NonEmptyFrom ParamClause M As A, A] =
			report
	}




	/** Accessor for the right-most relation in `F` with a mapping conforming to
	  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]`, providing
	  * it is joined using either [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] or
	  * [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]. This is similar to
	  * [[net.noresttherein.oldsql.sql.mechanics.GetTable.BySubject BySubject]], but this evidence
	  * takes into account only synthetic parameter relations and has a statically narrowed return type
	  * to the dedicated parameter `Mapping` subtype [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]].
	  * For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * and a following [[net.noresttherein.oldsql.sql.GroupBy]] (including its type aliases) are ignored.
	  * An implicit `ByParamType[A, B, X]` can be converted to a `ByParamType[F, G, X]`
	  * if `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * and the join types in `F` and `G` since `A` and `B` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	@implicitNotFound("No parameter with type ${X} in the FROM clause ${F}:\n"+
	                  "no implicit GetTable.ByParamType[${F}, ${G}, ${X}]")
	sealed trait ByParamType[-F <: RowProduct, G <: RowProduct, X] extends RelationEvidence[F, G, X] {
		type M[O] <: UnboundParam[X, O]
	}


	/** Provides implicit values of [[net.noresttherein.oldsql.sql.mechanics.GetTable.ByParamType! ByParamType]],
	  * accessing [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters of a `RowProduct` based on
	  * a given parameter type represented as the subject type of the synthetic parameter mapping
	  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, O]`.
	  * For the purpose of this implicit, all relations joined between
	  * [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * An implicit `ByParamType[A, B, X]` can be converted to a `ByParamType[F, G, X]`
	  * if `A `[[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]` F`, `B ExpandedBy G`,
	  * and the join types in `F` and `G` since `A` and `B` are known at least to their generalized super type.
	  * This applies only when such implicits can be computed based on the static types `A, B, F, G`
	  * - an implicit `ExpandedBy` from the lexical scope is not sufficient.
	  */
	object ByParamType extends GetTableByPredicate {
		type Key = Any
		override type Get[-F <: RowProduct, G <: RowProduct, X] = ByParamType[F, G, X]

		implicit def byParamType[F <: RowProduct, G <: RowProduct, X]
		                        (implicit found :Return[F, G, X] { type M[O] <: UnboundParam[X, O] })
				:ByParamType[F, G, X]
					{ type T[O <: RowProduct] = found.T[O]; type M[O] = found.M[O]; type O = found.O; type I = found.I } =
			new Delegate[F, G, X, found.O, found.I, found.M, found.T](found) with ByParamType[F, G, X]

		implicit def satisfies[M[O] <: UnboundParam[X, O], X] :Predicate[NonEmptyFrom ParamClause M, X] =
			report
	}

}





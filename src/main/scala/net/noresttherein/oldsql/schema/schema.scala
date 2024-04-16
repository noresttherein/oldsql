package net.noresttherein.oldsql

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{NaturalMap, PassedArray}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.exceptions.BuffMappingFailureException
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.topologicalSort
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}


//here be implicits
import net.noresttherein.oldsql.slang._




/**
  * @author Marcin Mościcki
  */
package object schema {

	/** A `MappingExtract[S, T, O]` describes the parent-child relationship between a mapping
	  * [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]`[S, O]`
	  * and its component `TypedMapping[T, O]`.
	  * It serves three functions:
	  *   - provides a means of extracting the value of the component from the value of the parent;
	  *   - retrieves the value of a component from `ComponentValues`;
	  *   - provides the operative, 'export' version of the component, that is the version with any modifications,
	  *     declared in the parent mapping (or some other mapping on the path to the subcomponent),
	  *     applied to the original version of the component. This includes additional buffs and column prefix
	  *     declarations defined for all subcomponents of a mapping.
	  * As with its [[net.noresttherein.oldsql.morsels.Extractor Extractor]] super type, the primary extractor function
	  * returns an [[net.noresttherein.oldsql.collection.Opt Opt]], thus allowing situations where a value
	  * for a component does not exist in a given subject value `S` (and forcing using code to take this into account).
	  * This is conceptually different from a property as an `Option` (or similar), which should still be generally
	  * declared as a [[net.noresttherein.oldsql.schema.MappingExtract$.req requisite]] (mandatory) property
	  * of type `Option[T]`. The optional nature is reserved primarily for situations where a value is not present
	  * for technical reasons, but might in theory exist in the logical model. The primary example of this is data which
	  * was not loaded with the main subject, for example a BLOB property. It might still be declared as an `Option`
	  * in the entity class, but (assuming it is declared as ''not null'' in the database) in this case it signifies
	  * the value not being present only for this particular instance, rather than the modeled logical entity.
	  * An extract returning [[net.noresttherein.oldsql.collection.Opt.Lack Lack]] in this case
	  * (rather than [[net.noresttherein.oldsql.collection.Opt.Got Got]]`(Some[_])`) can be taken to mean
	  * that the column should not be included in the update for example.
	  *
	  * This is a type alias for a template class parameterized with the component mapping type, specified as
	  * the most generic [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping[T, O] ]].
	  * Note that while it is generally assumed that the component included as an extract
	  * is an export component of some [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]`[S, O]`,
	  * ensuring that it is indeed the case lies on the code creating a new instance.
	  * @see [[net.noresttherein.oldsql.schema.SpecificExtract]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.apply[T](TypedMapping[T]) ]]
	  * @see [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]]
	  * @tparam S the subject type of the parent mapping.
	  * @tparam T the subject type of the component mapping.
	  * @tparam O the origin type of the parent and child mappings.
	  */
	type MappingExtract[-S, T, O] = SpecificExtract[TypedMapping[T, O], S, T, O]

	type GenericExtract[+M[s, o] <: TypedMapping[s, o], S, T, O] = SpecificExtract[M[T, O], S, T, O]

	/** A `MappingExtract` for a column with subject type `T` of a parent mapping with subject type `S` and origin
	  * type `O`.
	  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
	  * @see [[net.noresttherein.oldsql.schema.SpecificExtract]]
	  */
	type ColumnMappingExtract[S, T, O] = SpecificExtract[TypedColumn[T, O], S, T, O]


	type EntityMapping[PK, S, O] = GenericEntityMapping[MappingOf[PK]#Projection, PK, S, O]


	/** An implicit argument used to 'seal' methods to package `schema`. A method `private[scope]` or `protected[scope]`
	  * cannot be used outside package/class `scope`, but, unless final, it can be overriden by extending classes
	  * with a matching `public` definition. In some cases we can't declare the method `final` in all open classes,
	  * but we don't want it to leak in any form, including overriding, to client code. Accepting an implicit parameter
	  * of `Seal` (a value of which is always available) ensures that the method cannot be overriden, as `Seal`
	  * cannot be referenced outside this package. Note that this should not be dependent upon in the context 
	  * of security, as all such declarations are public in the bytecode and can thus be easily accessed from `Java`. 
	  */
	private[schema] final class Seal

	private[schema] object Seal {
		@inline def apply() :Seal = instance
		implicit final val instance :Seal = new Seal
	}

	/** A value wrapper with visibility restricted to package `schema`, ensuring that any definition including it
	  * can neither be used nor overriden by extending classes from outside this package. A declaration
	  * of `private[schema] val x :Int` is usable only within the specified scope, but an extending class from any
	  * package can always override it with a `val x :Int`. Adding a `final` modifier solves this source
	  * of interface leak but is not always possible if other classes from withing the package override/implement
	  * the field. Declaring it as `protected[schema] val x :Sealed[Int]` makes overriding impossible, as class `Sealed`
	  * can be neither extended, nor referenced from outside the package. Inlined implicit boxing and unboxing reduces 
	  * the syntax cost of this pattern. Note that this should not be dependent upon in the context of security, 
	  * as all such declarations are public in the bytecode and can thus be easily accessed from `Java`. 
	  */
	private[schema] class Sealed[+T](val value :T) extends AnyVal

	private[schema] object Sealed {
		@inline def apply[T](value :T) :Sealed[T] = new Sealed(value)

		@inline implicit def seal[T](value :T) :Sealed[T] = new Sealed(value)
		@inline implicit def unseal[T](value :Sealed[T]) :T = value.value
	}



	//todo: move the lot to package support
	private[schema] def mapBuffs[S, X](mapping :MappingOf[S])(map :S =?> X, unmap :X =?> S) :Buffs[X] =
		mapping.buffs.map { buff =>
			buff.bimap(
				map.requisite getOrElse {
					val flatMap = map.optional
					s :S => flatMap(s) getOrElse {
						throw new BuffMappingFailureException(
							s"Failed mapping of $mapping: could not derive the value for buff $buff from $s."
						)
					}
				},
				unmap.requisite getOrElse {
					val flatUnmap = unmap.optional
					x :X => flatUnmap(x) getOrElse {
						throw new BuffMappingFailureException(
							s"Failed mapping of $mapping: could not unmap value $x as part of the buff $buff."
						)
					}
				}
			)
		}



	private[schema] def flatMapBuffs[S, X](mapping :MappingOf[S])
	                                      (map :S => Option[X], unmap :X => Option[S]) :Buffs[X] =
		mapping.buffs.map { buff => buff.bimap[X](
			s => map(s) getOrElse {
				throw new BuffMappingFailureException(
					s"Failed mapping buff $buff of mapping $mapping: no value returned for $s by $map."
				)
			},
			x => unmap(x) getOrElse {
				throw new BuffMappingFailureException(
					s"Failed reverse mapping buff $buff of mapping $mapping: no value returned for $x by $unmap"
				)
			}
		)}


	private[schema] def cascadeBuffs[S, X](mapping :MappingOf[S])(map :S =?> X) :Buffs[X] =
		cascadeBuffs[S, X](mapping.buffs, mapping.toString)(map)

	private[schema] def cascadeBuffs[S, X](buffs :Buffs[S], owner: => String)(map :S =?> X) :Buffs[X] =
		buffs.flatMap(buff =>
			buff.cascade(map.requisite getOrElse {
				s :S => map.opt(s) getOrElse {
					throw new BuffMappingFailureException(
						s"Failed cascading buff $buff from $owner: no value returned for $s by $map"
					)
				}
			})
		)






	private[oldsql] def filterColumnExtracts[S, O]
	                    (mapping :TypedMapping[S, O])
	                    (extracts :NaturalMap[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract])
			:NaturalMap[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract] =
		filterColumnExtracts(extracts)


	private[oldsql] def filterColumnExtracts[S, O]
	                    (extracts :NaturalMap[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract])
			:NaturalMap[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract] =
	{
		def castToColumn[T](entry :Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T]) =
			entry._1 match {
				case column :TypedColumn[T, O] @unchecked =>
					Some(Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](
						column, entry._2.asInstanceOf[ColumnMappingExtract[S, T, O]]
					))
				case _ => None
			}
		
		extracts.flatMap(castToColumn(_))
	}



	private[oldsql] def composeExtracts[S, X, O](extract :MappingExtract[S, X, O])
			:TypedMapping[S, O]#ExtractMap =
		composeExtracts(extract.export, extract)

	private[oldsql] def composeExtracts[S, X, O]
	                    (extracts :TypedMapping[X, O]#ExtractMap, extract :MappingExtract[S, X, O])
			:TypedMapping[S, O]#ExtractMap =
		extracts.map(composeExtractAssoc(extract)(_))

	private[oldsql] def composeExtracts[S, X, O](mapping :TypedMapping[X, O], extract :S =?> X)
			:TypedMapping[S, O]#ExtractMap =
		mapping.extracts.map(composeExtractAssoc(mapping, extract)(_))



	private[oldsql] def composeColumnExtracts[S, X, O](extract :MappingExtract[S, X, O])
			:TypedMapping[S, O]#ColumnExtractMap =
		composeColumnExtracts(extract.export, extract)

	private[oldsql] def composeColumnExtracts[S, X, O]
	                    (extracts :TypedMapping[X, O]#ColumnExtractMap, extractor :MappingExtract[S, X, O])
			:TypedMapping[S, O]#ColumnExtractMap =
		extracts.map(composeColumnExtractAssoc(extractor)(_))

	private[oldsql] def composeColumnExtracts[S, X, O](mapping :TypedMapping[X, O], extractor :S =?> X)
			:TypedMapping[S, O]#ColumnExtractMap =
		mapping.columnExtracts.map(composeColumnExtractAssoc(mapping, extractor)(_))



	@inline private[oldsql] def composeExtractAssoc[S, X, T, O]
	                            (extractor :MappingExtract[S, X, O])
	                            (entry :Assoc[TypedMapping[X, O]#Component, TypedMapping[X, O]#Extract, T])
			:Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T] =
		Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T](entry._1, entry._2 compose extractor)

	@inline private[oldsql] def composeExtractAssoc[S, X, T, O](mapping :TypedMapping[X, O], extractor :S =?> X)
	                            (entry :Assoc[TypedMapping[X, O]#Component, TypedMapping[X, O]#Extract, T])
			:Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T] =
		Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T](entry._1, entry._2 compose extractor)

	@inline private[oldsql] def composeExtractAssoc[S, X, T, O](mapping :MappingAt[O], f : S => X)
	                            (entry :Assoc[TypedMapping[X, O]#Component, TypedMapping[X, O]#Extract, T])
			:Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T] =
		Assoc[TypedMapping[S, O]#Component, TypedMapping[S, O]#Extract, T](entry._1, entry._2 compose f)



	@inline private[oldsql] def composeColumnExtractAssoc[S, X, T, O]
	                            (extractor :MappingExtract[S, X, O])
	                            (entry :Assoc[TypedMapping[X, O]#Column, TypedMapping[X, O]#ColumnExtract, T])
			:Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T] =
		Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose extractor)

	@inline private[oldsql] def composeColumnExtractAssoc[S, X, T, O]
	                            (mapping :MappingAt[O], extractor : S =?> X)
	                            (entry :Assoc[TypedMapping[X, O]#Column, TypedMapping[X, O]#ColumnExtract, T])
			:Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T] =
		Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose extractor)

	@inline private[oldsql] def composeColumnExtractAssoc[S, X, T, O]
	                            (mapping :MappingAt[O], f : S => X)
	                            (entry :Assoc[TypedMapping[X, O]#Column, TypedMapping[X, O]#ColumnExtract, T])
			:Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T] =
		Assoc[TypedMapping[S, O]#Column, TypedMapping[S, O]#ColumnExtract, T](entry._1, entry._2 compose f)




	private[schema] def prefixOption(prefix :String) :Option[String] = prefix.length > 0 ifTrue prefix


	/** Sorts the given component list so that if component `x` contains component `y` then x is located in the returned
	  * sequence before `y`. All components should be ideally in their 'nominal' versions (as declared)
	  * or implement [[net.noresttherein.oldsql.schema.Mapping.original original]] to return such instance in order
	  * for the [[net.noresttherein.oldsql.schema.Mapping.contains contains]] check to not yield false negatives.
	  */
/*
	private[oldsql] def sortTopologically[O](components :Seq[TypedMapping[_, O]]) :Seq[TypedMapping[_, O]] =
		components.size match {
			case 0 => Nil
			case 1 => components
			case count =>
				//graph(i * count + j) == 1 for i != j <==> components(i) contains components(j)
				//graph(i * i) == 1: vertex entered; graph(i * i) == 2: vertex processed
				val graph = new Array[Byte](count * count)
				val indexed = components match {
					case ok :IndexedSeq[TypedMapping[_, O]] => ok
					case _ => components to ArraySeq
				}
				var i = 0
				while (i < count) {
					val c = indexed(i)
					var j = 0
					while (j < count) {
						val next = indexed(j)
						if (i != j && ((c contains next) || (c contains next.original)))
							graph(i * count + j) = 1
						j += 1
					}
					i += 1
				}
				var sorted :List[TypedMapping[_, O]] = Nil

				def dfs(comp :Int, tail :List[TypedMapping[_, O]]) :List[TypedMapping[_, O]] =
					graph(comp * comp) match {
						case 0 =>
							var res = tail
							graph(comp * comp) = 1
							var j = count
							while (j > 0) {
								j -= 1
								if (comp != j && graph(comp * count + j) == 1)
									res = dfs(j, res)
							}
							graph(comp * comp) = 2
							indexed(comp)::res
						case 1 =>
							throw new IllegalArgumentException(
								"A cycle involving " + components(comp) + " detected among " + components + "."
							)
						case _ => tail
					}

				while (i > 0) {
					i -= 1
					sorted = dfs(i, sorted)
				}
				sorted
		}
*/


	/** Sorts the given component list so that if component `x` contains component `y` then x is located in the returned
	  * sequence before `y`. All components should be ideally in their 'nominal' versions (as declared)
	  * or implement [[net.noresttherein.oldsql.schema.Mapping.original original]] to return such instance in order
	  * for the [[net.noresttherein.oldsql.schema.Mapping.contains contains]] check to not yield false negatives.
	  */
	private[oldsql] def babushkaSort[O](components :Seq[TypedMapping[_, O]]) :Seq[TypedMapping[_, O]] =
		topologicalSort(components) { (c1, c2) => c1.contains(c2) || c1.contains(c2.original) }

	/** Sorts the given mapping list so that
	  * if `x `[[net.noresttherein.oldsql.schema.Mapping.submappingOf submappingOf]]` y` then x is located
	  * in the returned sequence before `y`. Elements of cycles in the input (representing homomorphic mappings)
	  * may appear in any order. All components should be ideally in their 'nominal' versions (as declared)
	  * or implement [[net.noresttherein.oldsql.schema.Mapping.original original]] to return such instance in order
	  * for the check to not yield false negatives.
	  */
	private[oldsql] def sortByExtensions(components :Seq[Mapping]) :Seq[Mapping] =
		topologicalSort(components, false) { (c1, c2) => c2 submappingOf c1 }
}


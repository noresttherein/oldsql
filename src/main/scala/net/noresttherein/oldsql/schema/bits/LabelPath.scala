package net.noresttherein.oldsql.schema.bits

import scala.annotation.tailrec

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.bits.LabelPath.{/, :/, ConcatLabelPath, Label, SplitLabelPath}
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping






/** A type class marking the type `P` as a ''label path'' - a sequence of one or more `Label`s (string literal types)
  * separated with a `/` (a composite `LabelPath` class). Aside from its implicit witness nature, it actually
  * references the path instance it is parameterized with.
  * It is used in places where a (mapping) identifier is needed and must be composed of label identifiers
  * of individual mappings. There are two subtypes of this type:
  *   1. [[net.noresttherein.oldsql.schema.bits.LabelPath.:/ #/]], the type class for any
  *      `String` literal type representing an atomic label;
  *      2. [[net.noresttherein.oldsql.schema.bits.LabelPath./ /]], the type of compound paths composed of several labels.
  *      `A / B / C` is a type class for itself, `A / B / C`.
  *      This class/type class duality comes from the dual use cases of path types: indexing by values and indexing
  *      purely on type level and the desire for uniform interface of single labels and compound paths. The polymorphism
  *      provided by the type class interface is unfortunately limited to cases where the label type is specified explicitly,
  *      as the compiler will never infer a literal type in place of `String` unless a singleton type is expected,
  *      introducing the need for explicit overloads of methods accepting a `Label` and a `LabelPath`.
  *      Importing the [[net.noresttherein.oldsql.schema.bits.LabelPath.Label Label]] type imports also
  *      an implicit conversion from any string literal to `#/`, allowing their use directly as `LabelPath` values.
  * @tparam P a path type - either a `Label`, or one or more `/` instances appending new labels to the first one.
  * @see [[net.noresttherein.oldsql.schema.bits.LabelPath.Label]]
  * @see [[net.noresttherein.oldsql.schema.bits.LabelPath./]]
  */
sealed trait LabelPath[P] extends Any with Serializable {

	/** The last label in this path. */
	def label :Label

	/** The instance of the path type `P` */
	def path :P

	/** Creates a new path consisting of this path followed by the given label. */
	def /[N <: Label](next :N) :P / N = new /(path, next)

	/** Creates a new path being an inlined concatenation of this path and the given suffix path `next`. */
	@inline final def /[A, B <: Label](next :A / B)(implicit concat :ConcatLabelPath[P, A / B]) :concat.Path =
		concat.result

	/** Creates a new path being an inlined concatenation of this path and the suffix path `S`
	  * provided as an implicit parameter. */
	@inline final def /[S](implicit suffix :LabelPath[S], concat :ConcatLabelPath[P, S]) :concat.Path = concat.result

	@inline final def split(implicit split :SplitLabelPath[P]) :(split.First, split.Suffix) = (split.first, split.suffix)

	def toSeq :Seq[String] = {
		@tailrec def rec(path :LabelPath[_], acc :List[String]) :List[String] = path match {
			case :/(first) => first::acc
			case prefix / last => rec(prefix, last::acc)
		}
		rec(this, Nil)
	}
}






object LabelPath {

	/** A type of string literals used to label mappings on the type level for ease of access.
	  * Importing a Label
	  */
	type Label = String with Singleton

	/** Wraps a `Label` - a string literal - in its `LabelPath` type class. */
	@inline implicit def Label[L <: Label](label :L): :/[L] = new :/[L](label)


	/** Pattern matching extractor for ''label paths'' consisting of a single `Label`.
	  * @see [[net.noresttherein.oldsql.schema.bits.LabelPath.:/]]
	  */
	object Label {
		@inline def unapply[P](path :LabelPath[P]) :Opt[P with Label] = path match {
			case label: :/[_] => Got(label.label.asInstanceOf[P with Label])
			case _ => Lack
		}

		@inline def unapply[P](path :P) :Opt[P with String with path.type] = path match {
			case label :String => Got(label.asInstanceOf[P with String with path.type])
			case _ => Lack
		}
	}



	/** Summons an implicit type class `LabelPath` for the path type `P`. */
	@inline def apply[P](implicit path :LabelPath[P]) :LabelPath[P] = path

	/** Retrieves an instance of the ''label path'' type `P` from the implicit type class `LabelPath`. */
	@inline def pathOf[P](implicit path :LabelPath[P]) :P = path.path

	/** Provides the type class instance for a presumed ''label path'' type `P`.
	  * @return `path` itself if it is an instance of `LabelPath`, a `#/` if it is an instance of `String`,
	  *         or `None` otherwise.
	  */
	def fromPath[P](path :P) :Option[LabelPath[P]] = path match {
		case path :LabelPath[P @unchecked] => Some(path)
		case label :String => Some(new :/[label.type](label).asInstanceOf[LabelPath[P]])
		case _ => None
	}


	@inline implicit def atomicLabelPath[L <: Label :ValueOf]: :/[L] = new :/[L](valueOf[L])

	@inline implicit def compositeLabelPath[A :LabelPath, B <: Label :ValueOf] :A / B =
		new /[A, B](implicitly[LabelPath[A]].path, valueOf[B])

	@inline implicit def pathSingleton[P, L <: Label](implicit path :LabelPath[P/L]) :ValueOf[P/L] =
		new ValueOf(path.path)



	/** A factory of single-element [[net.noresttherein.oldsql.schema.bits.LabelPath LabelPath]] instances.
	  * `$this / label` is a `LabelPath` type class for type `L <: String with Singleton` such that `label :L`.
	  */
	object $this {
		/** A single-element path of the given label. */
		def /[L <: Label](label :L): :/[L] = new :/(label)
	}



	/** The `LabelPath` type class for a single label `L`. There is an implicit conversion wrapping any
	  * `String` literal type `L` in this class.
	  */
	class :/[L <: Label](val label :L) extends AnyVal with LabelPath[L] {
		def path :L = label

		/** Creates a new path consisting of this label followed by the given label. */
		@inline override def /[N <: Label](next :N)  :L / N = new /(label, next)

		override def toString :String = "/" + label
	}

	object :/ {
		def apply[L <: Label](label :L): :/[L] = new :/(label)

		def unapply[P](path :LabelPath[P]) :Opt[P with Label] = path match {
			case atom: :/[_] => Got(atom.label.asInstanceOf[P with Label])
			case _ => Lack
		}
	}



	/** A composite label path type consisting of a prefix label path type `P` (either a label or another composite path)
	  * and the last label `L`. It is also its own type class `LabelPath[P/L]`.
	  * @tparam P the prefix path type of this path (one for which an implicit type class `LabelPath[P]` exists).
	  * @tparam L the label being the last element of this path.
	  */
	class /[P, L <: Label] private[LabelPath] (val prefix :P, val label :L) extends LabelPath[P / L] {
		def path :P / L = this

		@inline final override def /[N <: Label](next :N) : P / L / N = new /(this, next)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case path: /[_, _] => path.label == label && path.prefix == prefix
			case _ => false
		}

		override def toString :String = prefix.toString + "/" + label
	}



	object / {
		def apply[L <: Label](label :L): :/[L] = new :/(label)

		def unapply[P, L <: Label](path :P / L) :Opt[(LabelPath[P], L)] = path.prefix match {
			case label :String => Got((Label[label.type](label).asInstanceOf[LabelPath[P]], path.label))
			case prefix :LabelPath[P @unchecked] => Got((prefix, path.label))
			case _ => Lack //should never happen, unless someone messes with new implementations
		}

		def unapply(path :LabelPath[_]) :Opt[(LabelPath[_], Label)] = path match {
			case split: /[p, l] => unapply[p, l](split)
			case _ => Lack
		}
	}



	/** Implicit value concatenating to path types `A` and `B` (types for which `LabelPath` type classes exist).
	  * The result of the concatenation is another path type (an instance of `/`) specified by the `Path` member type.
	  * In order to avoid creating to stacks of objects - one of these implicit witnesses and the path being
	  * the result of this concatenation - instances of this trait are actually also instances
	  * of the concatenated `Path`.
	  */
	sealed trait ConcatLabelPath[A, B] {
		/** The result of concatenating label path types `A` and `B`. */
		type Path >: this.type
		@inline final def result :Path = this
		def typeClass :LabelPath[Path]
	}

	@inline implicit def concatWithLabel[A :LabelPath, B <: Label :ValueOf] :ConcatLabelPath[A, B] { type Path = A / B } =
		new /[A, B](pathOf[A], valueOf[B]) with ConcatLabelPath[A, B] {
			override type Path = A / B
			@inline override def typeClass :A / B = this
		}

	@inline implicit def concatWithPath[A, B, C <: Label :ValueOf](implicit concat :ConcatLabelPath[A, B])
			:ConcatLabelPath[A, B / C] { type Path = concat.Path / C } =
		new /[concat.Path, C](concat.result, valueOf[C]) with ConcatLabelPath[A, B / C] {
			override type Path = concat.Path / C
			@inline override def typeClass :concat.Path / C = this
		}



	sealed abstract class SplitLabelPath[P] {
		type First <: Label
		type Suffix// <: LabelPath[_]

		def first :First
		def suffix :Suffix
	}


	@inline implicit def splitIntoTwoLabels[A <: Label, B <: Label](implicit a :ValueOf[A], b :ValueOf[B])
			:SplitLabelPath[A / B] { type First = A; type Suffix = :/[B] } =
		new SplitLabelPath[A/B] {
			override type First = A
			override type Suffix = :/[B]
			val first = a.value
			val suffix = Label(b.value)
		}

	@inline implicit def splitIntoLabelAndPath[P, L <: Label](implicit split :SplitLabelPath[P], last :ValueOf[L])
			:SplitLabelPath[P / L] { type First = split.First; type Suffix = split.Suffix / L } =
		new SplitLabelPath[P / L] {
			override type First = split.First
			override type Suffix = split.Suffix / L
			val first = split.first
			val suffix = new /(split.suffix, last.value)
		}

}

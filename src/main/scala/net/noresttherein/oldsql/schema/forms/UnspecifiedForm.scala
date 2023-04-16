package net.noresttherein.oldsql.schema.forms

import java.sql.JDBCType

import net.noresttherein.oldsql.collection.{Opt, PassedArray}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.slang.classNameMethods
import net.noresttherein.oldsql.sql.RowShape





//todo: move all forms to package forms, and package forms to top level
/** Base trait extended both by [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]
  * and [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]],
  * it introduces declarations shared by both of them.
  * Additionally, by extending `SQLForm`, it brings into implicit search scope all implicit form declarations,
  * regardless of their kind.
  */
trait UnspecifiedForm extends SQLForms with Serializable {
	/** An `SQLForm` is not universal if it ignores a part o entirety of its input when reading or writing,
	  * and its output at least in part does not depend on its input. Form for an entity which has a transient
	  * property, not present in the database, is still universal; a form which ignores a persistent property
	  * of its entity, replacing it with a new value is not - at least unless that property could be otherwise
	  * updated. A form setting a versioning field to the current timestamp rather than the old one present
	  * in the updated entity could still call itself universal if the column is not updatable by normal means.
	  */ //not super happy with the name, but have no better idea
	def isUniversal :Boolean

	/** The number of read consecutive columns/written parameters by this form.  */
	def columnCount :Int

	/** A sequence listing the SQL types of columns read/set parameters by this form.
	  * It has exactly [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount columnCount]] elements,
	  * and are either the exact `JDBCType` argument given to the JDBC API or, if it is not directly expressed,
	  * an `JDBCType` most closely representing the [[java.sql.ResultSet ResultSet]] read method
	  * or [[java.sql.PreparedStatement PreparedStatement]] set method. It is one of the main factors involved
	  * in implementation of
	  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`.`[[net.noresttherein.oldsql.schema.SQLReadForm.comparable comparable]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]`.`[[net.noresttherein.oldsql.schema.SQLWriteForm.comparable comparable]]
	  * methods used to determine if two forms are 'compatible' on the SQL level - they can be used for separate member
	  * selects forming an SQL ''union select'' and their columns can be directly compared.
	  *
	  * Note that this comparison is for equality only, and hence will not match closely related types such
	  * as different number types. Also, some JDBC types like `JAVA_OBJECT` or `NULL` are very vague and could
	  * in practice represent many different SQL types in the database, but will nevertheless match only the same type,
	  * leading to false negatives. Furthermore, it is defined solely in terms of the underlying SQL types
	  * and say nothing about their Scala types, which may be completely unrelated objects which happen
	  * to be assembled from columns of the same types.
	  *
	  * Some special forms, mainly those which ignore the data and the result set and return a predetermined value,
	  * or custom forms defined by a single function of `ResultSet`/`PreparedStatement`, do not support this method
	  * and will throw an [[UnsupportedOperationException]]. Comparing this property on different forms is thus
	  * a more strict check than `comparable` methods, and gives feedback through the thrown exception
	  * when such a comparison cannot be made, unlike `comparable`, which swallows errors and returns `false`.
	  */
	@throws[UnsupportedOperationException]("if the column types of this form are unknown.")
	def columnTypes :Seq[JDBCType] //todo JDBCType.comparable extension method equaling types like TINYINT and INTEGER

	/** SQL column types read/written by this form,
	  * or [[net.noresttherein.oldsql.sql.RowShape RowShape]]`.`[[net.noresttherein.oldsql.sql.RowShape.Indefinite Indefinite]]
	  * if they are undefined.
	  */
	def shape :RowShape = RowShape(this)

	/** This form, but throwing a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]]
	  * if it were to read or write a `null` value. Declared on this level to have a common method overriden
	  * by both [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]] and not cause conflicts
	  * in [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]s mixing in both.
	  */
	def notNull :UnspecifiedForm

	/** Default implementation requires equality of runtime classes of both forms. */
	def canEqual(that :Any) :Boolean = that.getClass == getClass

	/** An optional textual representation of this form provided at its creation, used in its default
	  * [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.toString toString]] implementation if defined.
	  * Typically, this property is a textual representation of the Scala type to which the form is dedicated;
	  * in this way, read, write and read/write forms for the same type may share the same `text` value.
	  * The property is introduced primarily to facilitate cleaner `toString` methods by reducing redundant noise.
	  * For example, [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]`[Int].toString == "<Int"`
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]`[Int].toString == "Int>"`.
	  * When the forms are combined `write vs read`, they produce an `SQLForm[Int]` with `toString == "Int"`.
	  * Additionally, forms for types of higher kinds which adapt other forms, can use directly the `text` property
	  * of the underlying form to include both in their `text` and `toString` properties. Write, read and read/write
	  * forms for `Option[Int]` are thus printed as `"<Option[Int]"`, `"Option[Int]>` and `"Option[Int]"` - omitting
	  * redundant `<` and `>` as well as avoiding printing the combined read+write form
	  * as `"(<Option[<Int] vs Option[Int>]>)"`.
	  *
	  * By focusing on the type used rather than exact "dumping" of form's class and properties/adapted forms,
	  * this scheme introduces some ambiguity in textual representation, but this allows to include forms to be used
	  * in [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]'s `toString`, leading for example to
	  * a very legible format of `s"${column.name}[${column.form}]"`. It is however still possible for `toString`
	  * and `text` to be completely distinct in a form, allowing the user a choice between a more concise output
	  * of `text` for use when printing larger objects for logging purposes, and a detailed `toString` catering
	  * for an ease in debugging.
	  *
	  * If a form has a unique [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.name name]],
	  * then its value should be used also for `text` property. This is however not implemented on this level,
	  * as it is more practical to delegate `def name = text` instead in the forms which use `name` property
	  * as an identifier, in order to make use of, likely already defined, `text` property.
	  */ //todo: make it public
	protected def text :Opt[String] = Lack
	/** Returns `other.`[[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.text text]]. */
	protected final def textOf(other :UnspecifiedForm) :Opt[String] = other.text
	private[oldsql] final def `->text` :Opt[String] = text

	/** An optional name used to help identify the form. This is meant both as textual, visual representation
	  * in `toString` as well as an actual identifier serving as a sort of 'virtual class'.
	  * Depending on implementation, two forms may compare
	  * equal solely based on their name equality, regardless of other properties, or if their names
	  * and some subset of properties are equal. Form equality plays a part in determining equality and other
	  * equivalence relations of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] and a form lacking
	  * a defined equality may cause restriction to some features.
	  * In particular, it is expected that two [[net.noresttherein.oldsql.schema.ColumnForm column forms]]
	  * created by the same factory method with the same arguments will compare equal.
	  *
	  * If a form uses `name` in this capacity, its [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.text text]]
	  * property will equal `name`; this `text` -> `name` delegation is not however implemented here for practical
	  * reasons: name-based equality is typically a feature added on top of an existing form implementation,
	  * which may already have non empty `text` property.
	  */
	def name :Opt[String] = Lack

//	/** Grants access to `name` property to all other forms. */
//	@inline private[schema] final def `->name` :Opt[String] = name

	/** A compact textual representation of this form, with focus on brevity rather then being exhaustive and WYSIWYG.
	  * In general, for differentiation, [[net.noresttherein.oldsql.schema.SQLReadForm read]] forms tend to end
	  * with a '>' character and [[net.noresttherein.oldsql.schema.SQLWriteForm write]] forms start
	  * with a '&lt;' character. [[net.noresttherein.oldsql.schema.SQLForm Read/write]] forms either include
	  * neither or both. This is not a strict rule and, for example, adapters often end with the method name used
	  * to create them, for example "VARCHAR>.notnull". Due to heavy use o special characters and a philosophy
	  * on reflecting the (intended) function rather than the exact identity of a form, different implementations
	  * performing the same role may have the same `toString` representation despite having unrelated types
	  * and being unequal, possibly even not [[net.noresttherein.oldsql.schema.SQLForm.comparable comparable]].
	  * This can make determining the exact class and composition in complex cases may be difficult or impossible.
	  *
	  * The following symbols are used to represent specific behaviours:
	  *   - `s"=`$expr`"` indicates a form which ignores its argument in favour of a value obtained by other means
	  *     (typically a constant or a by-name parameter);
	  *   - `"{}"` represents a by-name expression, so, for example, `"={}>"` represents a read form returning the result
	  *     of evaluation of such an expression instead of data in the result set;
	  *   - `'#'` character is used to represent the number of columns, if cannot be otherwise inferred;
	  *   - `s"@${form.hashCode.toHex}"` encodes referential identity for forms which can't, or a are unlikely to
	  *   , equal another instance, in a similar way to default `Object.toString`;
	  *   - `'?'` indicates a non-evaluated lazy value.
	  *   - `"=>"` suffix and `"<="` prefix mark a [[net.noresttherein.oldsql.schema.SQLReadForm.map mapped]] read form
	  *     and an [[net.noresttherein.oldsql.schema.SQLWriteForm.unmap unmapped]] write form, regardless of the exact
	  *     mapping used.
	  *   - `"/Null(...)"` provides the information about
	  *     the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class instance used to handle
	  *     null values.
	  *
	  * The default implementation prints the form's
	  * [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.text text]], if defined, (followed by a '>'
	  * for read forms and preceded with a '<' for write forms), defaulting to
	  * [[net.noresttherein.oldsql.slang.innerNameOf inner class name]] followed by its hash code
	  * [[net.noresttherein.oldsql.slang.classNameMethods.shortHashString compacted]] to 4 hexadecimal digits.
	  */
	override def toString :String = //don't pattern match in case we migrate name to Option[String]
		if (text.isDefined) text.get else this.innerClassName + "@" + this.shortHashString
}


/** Base trait extended by both read and write column forms. It brings generic implicit definitions for all forms
  * into the search scope and groups common properties of derived types.
  */
trait UnspecifiedColumnForm extends UnspecifiedForm {
	/** The type of the column as a wrapper over an `Int` code understood by the database JDBC driver.
	  * @see [[java.sql.SQLType]]
	  */
	def sqlType :JDBCType

	final override def columnCount = 1
	override def columnTypes :Seq[JDBCType] = PassedArray :+ sqlType
}



object UnspecifiedForm {
	/** A trait extended directly exclusively by
	  * by [[net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm SingletonColumnWriteForm]],
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.JoinedSQLWriteForm JoinedSQLWriteForm]] in order
	  * to restore implementations of standard methods to whatever implementations they had before mixing the former
	  * traits. It declares forwarders for methods `toString`, `equals`, `hashCode` and `canEqual`, so they
	  * can be overriden in order to bypass the implementations inherited from `Seq`.
	  */
	private[schema] trait UnspecifiedFormDefaults extends UnspecifiedForm {
		private[schema] def superCanEqual(that :Any) :Boolean = super.canEqual(that)
		private[schema] def superEquals(that :Any) :Boolean = super.equals(that)
		private[schema] def superHashCode :Int = super.hashCode
		private[schema] def superToString :String = super.toString
	}

	/** A form which implements `equals` solely in terms
	  * of form [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.name names]] (which must not be empty).
	  */
	private[schema] trait UnspecifiedNamedForm extends UnspecifiedForm {
		assert(name.isDefined && name.get != null, "Empty name in a " + getClass + " instance.")

		override def name :Opt[String] = text

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :UnspecifiedNamedForm if other canEqual this => name == other.name
			case _ => false
		}
		override def hashCode :Int = name.hashCode
	}

	trait UnspecifiedFormAdapter extends UnspecifiedForm {
		override def columnCount :Int = form.columnCount
		override def columnTypes :Seq[JDBCType] = form.columnTypes
		override def isUniversal :Boolean = form.isUniversal
		protected def form :UnspecifiedForm
	}

	//consider: a different name, more consistent with other abstract forms
	private[schema] abstract class BaseFormAdapter[+F <: UnspecifiedForm](protected override val form :F,
	                                                                      protected override val text :Opt[String] = Lack)
		extends UnspecifiedFormAdapter
	{
		private[schema] lazy val cachedString = super.toString
		override def toString :String = cachedString
	}

	private[schema] trait UnspecifiedColumnFormAdapter extends UnspecifiedFormAdapter with UnspecifiedColumnForm {
		protected def form :UnspecifiedColumnForm
		override def sqlType :JDBCType = form.sqlType
	}

	private[schema] trait ColumnTypeEquality extends UnspecifiedColumnForm {
		override def canEqual(that :Any) :Boolean = super.canEqual(that) && that.isInstanceOf[UnspecifiedColumnForm]
		override def equals(that :Any) :Boolean = that match {
			case other :UnspecifiedColumnForm => (this eq other) || other.sqlType == sqlType && super.equals(other)
			case _ => false
		}
		override def hashCode :Int = super.hashCode * 31 + sqlType.hashCode
	}

}

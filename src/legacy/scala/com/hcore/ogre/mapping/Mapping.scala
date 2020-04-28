package com.hcore.ogre.mapping


import java.sql.ResultSet

import com.hcore.ogre.mapping.ComponentPath.{DirectComponent, SelfPath, TypedComponentPath}
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.Mapping.MappingExtension._
import com.hcore.ogre.mapping.Mapping._
import com.hcore.ogre.mapping.bits.{PrefixedMapping, QualifiedMapping}
import com.hcore.ogre.mapping.support.MappedMapping
import com.hcore.ogre.mapping.support.MappedMapping.MappedAs
import com.hcore.ogre.morsels._
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.SQLForm.EmptyForm
import com.hcore.ogre.sql.{SQLWriteForm, SQLReadForm, SQLForm}

import scala.slick.jdbc.{PositionedParameters, GetResult, PositionedResult, SetParameter}
import scala.util.Try

//implicits
import com.hcore.ogre.morsels.InverseIndexSeq.implicitIndexing
import com.hcore.ogre.morsels.Names._
import extensions._
import com.hcore.ogre.slang.repeat
import SaferCasts._


/** Root interface of the  mapping class hierarchy to be used when the mapped type is of little importance (for example in collections
  * of mappings for various types). All implementations are required to extend Mapping[T] instead, which is parameterized
  * with mapped type. The main reason for this duplicity is the limitation of scala type inference: a generic method
  * <code>m[M<:Mapping[T], T](m :M)</code> will infer type parameters M, Nothing when given an argument of type M<:Mapping[T].
  * On the other hand, when a type parameter is declared M<:Mapping[_] the ResultType is instantiated early as 'Any' and it is not
  * equal to m.ResultType, leading to other problems later.
  */
sealed trait AnyMapping { mapping => //this :Mapping[_] =>
	//todo: rename to Ego
	type ResultType
	type Values = ComponentValues[this.type]
	type Component[_] = Component[_]
	type Component[T] <: Mapping[T] //with Component[_]


	def modifiers :Seq[MappingExtension[ResultType]]


	def enabled(opt :MappingExtensionType) :Boolean = opt.enabled(this)
	def disabled(opt :MappingExtensionType) :Boolean = opt.disabled(this)


	def asMapping :Mapping[ResultType] with this.type

	/** Direct component mappings of this mapping, including any top-level columns. */
	def components :Seq[Component[_]]

	/** All transitive componenents of this mapping (i.e. components/columns declared by it's components or other subcomponents),
	  *  or all that this mapping cares to expose, as instances of this.Component[_]. This list should include all selectable columns. */
	def subcomponents :Seq[Component[_]]

	def columnsFor(filter :ColumnFilter) :Seq[Component[_]]

	/** All direct and transitive columns declared within this mapping. This will include columns which are read-only, write-only and so on. */
	def columns :Seq[Component[_]]
	/** All columns which can be part of an sql filter (don't have NoQuery flag set) */
	def querable :Seq[Component[_]]
	/** All columns which can be listed in the select clause of an sql */
	def selectable :Seq[Component[_]]
	/** All columns which can be updated on existing database records */
	def updatable :Seq[Component[_]]
	/** All columns which can occur in an insert statement */
	def insertable :Seq[Component[_]]
	/** Columns autogenerated by the database; this implies being non-insertable. */
	def generated :Seq[Component[_]]

	/** All columns with a given option provided */
	protected def columnsWith(opt :MappingExtensionType) = columns.filter(opt.enabled).indexed
	/** All columns without the given option */
	protected def columnsWithout(opt :MappingExtensionType) = columns.filter(opt.disabled).indexed


	/** Slick positional parameter setter for all insertable columns, in that order */
	def InsertParameters :SetParameter[ResultType]
	/** Slick positional parameter setter for all updatable columns, in that order */
	def UpdateParameters :SetParameter[ResultType]
	/** Slick positional parameter setter for all queryable columns, in that order */
	def QueryParameters :SetParameter[ResultType]

	def selectForm(components :Seq[Component[_]]) :SQLReadForm[ResultType]
	def selectForm :SQLReadForm[ResultType]
	def queryForm :SQLWriteForm[ResultType]
	def updateForm :SQLWriteForm[ResultType]
	def insertForm :SQLWriteForm[ResultType]


	def readForm(filter :ColumnFilter) :SQLReadForm[ResultType]
	def writeForm(filter :ColumnFilter) :SQLWriteForm[ResultType]

//	def columnDefinition :Option[ColumnDefinition[ResultType]]

//	def ComponentParameters[T](component :Component[T]) :SetParameter[ResultType] =
//		component



	def apply(values :Values) :ResultType

	def assemble(values :Values) :Option[ResultType]

	def optionally(values :Values) :Option[ResultType]



	def apply(res :PositionedResult): ResultType



	def sqlName :Option[String] = None

	def scoutValue :Option[ResultType]

	def scoutValue(ctx :ReferenceContext[this.type]) :Option[ResultType]

	def nullValue :Option[ResultType]

//	@inline
//	final def apply[T](component :Component[T]) :ComponentView[this.type, component.type, T] =
//		new ComponentView[this.type, component.type, T](this \\ component)

	def apply[T](component :Component[T], value :ResultType) = {
		val path = this \\ component
		path.surepick.map(_(value)) orElse path.pick(value) getOrElse {
			throw new IllegalArgumentException(s"Can't get value for $component from $value of $this")
		}
	}

//	def apply[C<:Component[_]](component :this.type=>C) :ComponentPath[this.type, C]

	implicit def \\ [X](component :Component[X]) :TypedComponentPath[this.type, component.type, X]




//	def \\ [C<:Component[_]](component : this.type=>C) :ComponentPath[this.type, C] = {
//		val comp = component(this).asInstanceOf[Component[Any]]
//		(this \\ comp).asInstanceOf[ComponentPath[this.type, C]]
//	}


//	implicit def nestedPath[C<:Component[_], X<:Mapping](path :ComponentPath[C, X]) :ComponentPath[this.type, X] =
//		(this :this.type) \: path



	def map[X](there :ResultType=>X, back :X=>ResultType) :this.type MappedAs X =
		MappedMapping[X, ResultType, this.type](this, there, back)


	override def toString = sqlName getOrElse this.unqualifiedClassName

	def introString = columns.mkString(this+"{", ",", "}")
}



trait Mapping[E] extends GetResult[E] with AnyMapping { self =>

	type ResultType = E
	type Component[T] <: Mapping[T]
//	type Column[T] <: ColumnMapping[T] with Component[T]

	final override def asMapping :this.type = this

	def modifiers :Seq[MappingExtension[E]] = Seq()

	override def columnsFor(filter :ColumnFilter) :Seq[Component[_]] = filter(this :this.type)

	def InsertParameters :SetParameter[E]
	def UpdateParameters :SetParameter[E]
	def QueryParameters :SetParameter[E]

	def selectForm(components :Seq[Component[_]]) :SQLReadForm[E] = MappingReadForm[E](this :this.type)(this.selectable, _.selectForm)
	def selectForm :SQLReadForm[E]
	def queryForm :SQLWriteForm[E]
	def updateForm :SQLWriteForm[E]
	def insertForm :SQLWriteForm[E]

	def writeForm(filter :ColumnFilter) :SQLWriteForm[E] = filter.write(this)
	def readForm(filter :ColumnFilter) :SQLReadForm[E] = filter.read(this)


//	def columnDefinition :Option[ColumnDefinition[E]] = None



	def apply(res :PositionedResult) :E = {
		val value = apply(ComponentValues[this.type](this :this.type, res))
		selectable.size.times { res.skip } //fast forward default amount of columns
		value
	}


	override def apply(values: Values): ResultType =
		optionally(values) getOrElse {
			throw new IllegalArgumentException(s"Can't assemble $this from $values")
		}

	override def optionally(values: Values): Option[ResultType] =
		values.result(this) orElse ExplicitSelect.Value(this)

	def assemble(values :Values) :Option[E]

	final def scoutValue :Option[E] = scoutValue(new ReferenceContext[this.type](this))

	def scoutValue(ctx :ReferenceContext[this.type]) :Option[E] =
		assembledScoutValue(ctx) orElse ExplicitSelect.Value(this) orElse nullValue



	def nullValue :Option[E] = None

	def assembledScoutValue(ctx :ReferenceContext[this.type]) :Option[E] =
		Try{ 
			assemble(ComponentValues(this :this.type)(cmp => cmp.scoutValue(ctx :\ cmp)))
		}.toOption.flatten 


	def isSymLink[T](component :Component[T]) = symLinkTarget(component).isDefined

	def symLinkTarget[T](component :Component[T]) :Option[ComponentPath[this.type, _<:AnyMapping]] =
		SymLink.startsWith[this.type, T](this :this.type)(component).map(_.target)



	def qualified(prefix :String) :Mapping[E] =
		Mapping.prefixOption(prefix).map(new QualifiedMapping[E](this, _)) getOrElse this

	def prefixed(prefix :String) :Mapping[E] =
		Mapping.prefixOption(prefix).map(new PrefixedMapping[E, this.type](this, _)) getOrElse this

//	override def map[X](there: E => X, back: X => E): this.type MappedAs X =
//		MappedMapping(this, there, back)


//	def symLink[T](component :Component[T]) :Component[T] =
//		symLink[component.type, T]((this :this.type) \\ (component :component.type))
//
//	def symLink[C<:Mapping[X], X](path :TypedComponentPath[this.type, C, X]) :Component[X]



}








object Mapping {
	class ReferenceContext[M<:AnyMapping](val forger :Forger, val path :MappingPath[_<:AnyMapping, M]) {
		def this(mapping :M) = this(new Forger, ComponentPath.self(mapping))

		def :\ (comp :M#Component[_]) :ReferenceContext[comp.type] =
			new ReferenceContext(forger, path :\ comp)

		def \\[X<:AnyMapping] (path :MappingPath[M, X]) = new ReferenceContext[X](forger, this.path++path)
	}

	type TypedMapping[X] = AnyMapping { type ResultType=X }

	type ComponentCompatibleMapping[M <: AnyMapping] = AnyMapping {
		type Component[X] >: M#Component[X] <:M#Component[X]
//		type Column[X] = M#Column[X]
	}
	
	type CompatibleMapping[M<:AnyMapping] = AnyMapping {
		type ResultType = M#ResultType
		type Component[X] = M#Component[X]
//		type Column[X] = M#Column[X]
	}

	type TypeCompatibleMapping[M<:AnyMapping] = AnyMapping {
		type ResultType = M#ResultType
	}


//	class ComponentView[M<:Mapping, C<:M#Component[T], T](val path :TypedPath[M, C, T]) extends AnyVal {
//		def apply(value :M#ResultType) :Option[T] = path.pick(value)
//		def apply[X](subcomponent :C#Component[X]) :Option[M#Component[X]] = path.lift(subcomponent)
//	}

	

//	def apply[E](columns :Seq[ColumnMapping[_]], assemble :Seq[_]=>E, deassamble :E=>Seq[_]) :Mapping[E] =
//		new ColumnSeqMapping[E](columns, assemble, deassamble)

//	trait MappingWalker[M<:Mapping]


	trait ColumnFilter {
		def apply[E](mapping :Mapping[E]) :Seq[mapping.Component[_]] =
			mapping.columns.filter(c => apply(mapping :mapping.type, c))

		def apply[M<:AnyMapping, T](mapping :M, column :M#Component[T]) :Boolean

		def read[E](mapping :Mapping[E]) :SQLReadForm[E] =
			MappingReadForm(mapping :mapping.type)(apply(mapping), read(_))

		def write[E](mapping :Mapping[E]) :SQLWriteForm[E] =
			MappingWriteForm(mapping :mapping.type)(apply(mapping), write(_))
	}

	object ColumnFilter {
		class WithModifier(modifier :MappingExtensionType) extends ColumnFilter {
			def apply[M<:AnyMapping, T](mapping :M, column :M#Component[T]): Boolean =
				modifier.enabled(column)
		}
		class WithoutModifier(modifier :MappingExtensionType) extends ColumnFilter {
			def apply[M<:AnyMapping, T](mapping :M, column :M#Component[T]): Boolean =
				modifier.disabled(column)
		}
		class WriteFilter(modifier :MappingExtensionType) extends WithoutModifier(modifier) {
			override def read[E](mapping: Mapping[E]) = EmptyForm(throw new UnsupportedOperationException(s"$this: readForm for $mapping"))
		}

		case object ForSelect extends WithoutModifier(NoSelect) {
			override def apply[E](mapping: Mapping[E]) :Seq[mapping.Component[_]] = mapping.selectable
			override def read[E](mapping: Mapping[E]) = mapping.selectForm
			override def write[E](mapping: Mapping[E]) = SQLWriteForm.empty
		}

		case object ForQuery extends WriteFilter(NoQuery) {
			override def apply[E](mapping :Mapping[E]) :Seq[mapping.Component[_]] = mapping.querable
			override def write[E](mapping :Mapping[E]) = mapping.queryForm
		}

		case object ForInsert extends WriteFilter(NoInsert) {
			override def apply[E](mapping: Mapping[E]) :Seq[mapping.Component[_]] = mapping.insertable
			override def write[E](mapping: Mapping[E]) = mapping.insertForm
		}

		case object ForUpdate extends WriteFilter(NoUpdate) {
			override def apply[E](mapping :Mapping[E]) :Seq[mapping.Component[_]] = mapping.updatable
			override def write[E](mapping :Mapping[E]) = mapping.updateForm
		}

		case object AllColumns extends ColumnFilter {
			override def apply[E](mapping :Mapping[E]) :Seq[mapping.Component[_]] = mapping.columns
			override def apply[M<:AnyMapping, T](mapping :M, column :M#Component[T]) = true
		}
	}


	/** Todo: rename to boost or buff */
	trait MappingExtension[+T] {

		def is(opt :MappingExtensionType) :Boolean = factory.implies(opt)
		
		def map[X](there :T=>X) :MappingExtension[X]

		protected[MappingExtension] def factory :MappingExtensionType



		def canEqual(that :Any) = that.getClass == this.getClass

		override def equals(that :Any) = that match {
			case o:MappingExtension[_] => (this eq o) || o.canEqual(this) && factory == o.factory
			case _ => false
		}

		override def hashCode = getClass.hashCode


//		override lazy val toString = {
//			val name = getClass.getName
//			val offset = name.lastIndexOf('.') //skip package prefix
//			val end = if (name.last!='$') name.length else name.length-1 //skip '$' at the end of name for singleton classes
//			val inner = name.lastIndexOf('$', end-1) //skip outer class prefix
//			name.substring((inner max offset) +1, end)
//		}

	}
	
	

	object MappingExtension {
		case object NoSelectByDefault extends AbstractMappingExtensionType
		case object NoInsertByDefault extends AbstractMappingExtensionType
		case object NoUpdateByDefault extends AbstractMappingExtensionType
		case object NoQueryByDefault extends AbstractMappingExtensionType
	
		case object NoSelect extends ComboFlag(NoSelectByDefault)
		case object NoInsert extends ComboFlag(NoInsertByDefault)
		case object NoUpdate extends ComboFlag(NoUpdateByDefault)
		case object NoQuery extends ComboFlag(NoQueryByDefault)
		
		case object ReadOnly extends ComboFlag(NoInsert, NoUpdate)
		case object AutoGen extends ComboFlag(ReadOnly)

		case object OptionalSelect extends ValuedExtensionType

		case object ExplicitSelect extends ComboExtensionType(OptionalSelect, NoSelectByDefault) with ValuedExtensionType

		case object OptionalInsert extends MappingFlag

		case object ExplicitInsert extends ComboFlag(OptionalInsert, NoInsertByDefault)

		case object OptionalUpdate extends MappingFlag

		case object ExplicitUpdate extends ComboFlag(OptionalUpdate, NoUpdateByDefault)

		case object OptionalQuery extends MappingFlag

		case object ExplicitQuery extends ComboFlag(OptionalQuery, NoQueryByDefault)

		


		case class SymLink[X<:AnyMapping, Y<:Mapping[T], T, V] private (target :TypedComponentPath[X, Y, T], value :T=>V)
			extends MappingExtension[V]
		{
			override def map[O](there: V => O): MappingExtension[O] = new SymLink(target, value andThen there)

			def startingWith[M<:AnyMapping](mapping :M) :Option[SymLink[M, Y, T, V]] =
				this.asInstanceOf[SymLink[M, Y, T, V]].providing(target.start==mapping)


			override def factory: MappingExtensionType = SymLink
		}

		object SymLink extends MappingExtensionType {
			def apply[X<:AnyMapping, Y<:Mapping[T], T](path :TypedComponentPath[X, Y, T]) :SymLink[X, Y, T, T] =
				new SymLink(path, identity[T] _)

			def apply[T](mapping :AnyMapping)(target :mapping.Component[T]) :SymLink[mapping.type, target.type, T, T] =
				new SymLink[mapping.type, target.type, T, T](mapping \\ target, identity[T] _)

			override def test[T](modifier: MappingExtension[T]): Option[SymLink[_<:AnyMapping, _<:AnyMapping, _, T]] =
				modifier.asSubclass[SymLink[_<:AnyMapping, _<:AnyMapping, _, T]]

			def startsWith[M<:AnyMapping, T](mapping :M)(component :mapping.Component[T]) :Option[SymLink[M, _<:Mapping[X], X, T] forSome { type X }]  =
				component.modifiers.toStream.flatMap(startsWith(mapping, _)).headOption //.asInstanceOf[SymLink[M, Mapping[Any], Any, T]]

			def startsWith[M<:AnyMapping, T](mapping :M, modifier :MappingExtension[T]) :Option[SymLink[M, _<:Mapping[X], X, T] forSome { type X }] =
				 modifier match {
					case sl :SymLink[_,_,_,_] if sl.target.start==mapping =>
						Some(sl.asInstanceOf[SymLink[M, Mapping[Any], Any, T]])
					case _ => None
				}

		}
		

		trait MappingExtensionType {
			def implies(other :MappingExtensionType) :Boolean = other==this

			object Enabled {
				def unapply(option :MappingExtension[_]) :Boolean = enabled(option)
				def unapply(options :Seq[MappingExtension[_]]) :Boolean = enabled(options)
				def unapply(mapping :AnyMapping) :Boolean = enabled(mapping)
			}
			object Disabled {
				def unapply(option :MappingExtension[_]) :Boolean = disabled(option)
				def unapply(options :Seq[MappingExtension[_]]) :Boolean = disabled(options)
				def unapply(mapping :AnyMapping) :Boolean = disabled(mapping)
			}
			
			def enabled(option :MappingExtension[_]) :Boolean = test(option).isDefined
			def enabled(options :Seq[MappingExtension[_]]) :Boolean = options.exists(enabled)
			def enabled(column :AnyMapping) :Boolean = enabled(column.modifiers)

			def disabled(option :MappingExtension[_]) = test(option).isEmpty
			def disabled(options :Seq[MappingExtension[_]]) :Boolean = options.forall(disabled)
			def disabled(column :AnyMapping) :Boolean = disabled(column.modifiers)

			def test[T](option :MappingExtension[T]) :Option[MappingExtension[T]] //= option.providing(option.is(this))
			def test[T](options :Seq[MappingExtension[T]]) :Option[MappingExtension[T]] = options.map(test(_)).collectFirst{ case Some(x) => x }
			def test[T](column :Mapping[T]) :Option[MappingExtension[T]] = test(column.modifiers)


			override lazy val toString = {
				val name = getClass.getName
				val offset = name.lastIndexOf('.') //skip package prefix
				val end = if (name.last!='$') name.length else name.length-1 //skip '$' at the end of name for singleton classes
				val inner = name.lastIndexOf('$', end-1) //skip outer class prefix
				name.substring((inner max offset) +1, end)
			}

		}
		
		class AbstractMappingExtensionType extends MappingExtensionType {
			override def test[T](extension: MappingExtension[T]): Option[MappingExtension[T]] =
				extension.providing(extension.is(this))
		}
		
//		trait MappingModifierInspector[O[T]<:Buff[T]] extends MappingModifierType {
//			abstract override def test[T](option :Buff[T]) :Option[O[T]] //= //option.is(this)
//
//			override def test[T](options :Seq[Buff[T]]) :Option[O[T]] =
//				options.map(test(_ :Buff[T]) :Option[O[T]]).collectFirst{ case Some(x) => x }
//
//			override def test[T](mapping :Mapping[T]) :Option[O[T]] = test(mapping.modifiers)
//			
//		}

	


		trait MappingFlag extends MappingExtension[Nothing] with MappingExtensionType {
			override def test[T](option: MappingExtension[T]): Option[MappingExtension[T]] = 
				option.providing(option.is(this))

			override def factory = this

			override def map[X](there: Nothing => X): MappingFlag = this

			override def is(opt: MappingExtensionType): Boolean = implies(opt)


//			override def implies(other: MappingModifierType): Boolean = other==this

		}



		class ValuedExtension[+T] private[MappingExtension] (val factory :ValuedExtensionType, val value :T) extends MappingExtension[T] {

			override def map[X](there: (T) => X): MappingExtension[X] = factory(there(value))

			override def equals(that: Any): Boolean = that match {
				case o :ValuedExtension[_] => (o eq this) || o.canEqual(this) && o.value==value && o.factory == factory
				case _ => false
			}


			override def toString = s"$factory: $value"

		}



		trait ValuedExtensionType extends MappingExtensionType {
			def apply[T](value :T) :ValuedExtension[T] = new ValuedExtension[T](this, value)

			override def test[T](option: MappingExtension[T]): Option[ValuedExtension[T]] = option match {
				case v :ValuedExtension[_] if v.is(this) => Some(v.asInstanceOf[ValuedExtension[T]])
				case _ => None
			}

			override def test[T](options: Seq[MappingExtension[T]]): Option[ValuedExtension[T]] =
				options.map(test(_)).collectFirst{ case Some(x) => x }

			override def test[T](mapping: Mapping[T]): Option[ValuedExtension[T]] =
				test(mapping.modifiers)

			object Value {
				def unapply[T](option :MappingExtension[T]) :Option[T] = test(option).map(_.value)
				def unapply[T](options :Seq[MappingExtension[T]]) :Option[T] = test(options).map(_.value)
				def unapply[T](mapping :Mapping[T]) :Option[T] = test(mapping).map(_.value)

				def apply[T](option :MappingExtension[T]) :Option[T] = unapply(option)
				def apply[T](options :Seq[MappingExtension[T]]) :Option[T] = unapply(options)
				def apply[T](mapping :Mapping[T]) :Option[T] = unapply(mapping)

			}
		}



		class ComboExtensionType(val implied :MappingExtensionType*) extends MappingExtensionType {
			override def implies(other: MappingExtensionType): Boolean =
				other==this || implied.exists(_.implies(other))

			override def test[T](option: MappingExtension[T]): Option[MappingExtension[T]] =
				option.providing(option.is(this))
		}
		
		class ComboFlag(implied :MappingExtensionType*) extends ComboExtensionType(implied:_*) with MappingFlag {
			override def implies(other :MappingExtensionType) :Boolean = super[ComboExtensionType].implies(other)
		}



	}
	
	
	
	
	
	
	def prefixOption(prefix :String) = Option(prefix).filter(_.length>0)




	object SetParameters {
		def apply[E](setters :Seq[SetParameter[E]]) = SetParameter[E](
			(entity, params) => setters.foreach(_(entity, params))
		)

		def apply[E](mapping :Mapping[E])(setter :mapping.Component[_]=>SetParameter[_], components :Seq[mapping.Component[_]]) =
			SetParameter[E](
				(entity, params) => components.foreach { c =>
					type T = c.ResultType
					val comp = c.asInstanceOf[mapping.Component[T]]
					val value :c.ResultType = (mapping \\ comp).pick(entity) orElse comp.nullValue getOrElse {
						throw new IllegalArgumentException(s"cannot set parameter for component $comp of $mapping: no value in $entity and no null value defined")
					}
					setter(c).asInstanceOf[SetParameter[T]](value, params)
				}
			)

		def repeat[T](setter :SetParameter[T]) = SetParameter[Seq[T]](
			(seq, params) => seq.foreach(setter(_, params))
		)


		implicit val Empty :SetParameter[Any] = SetParameter[Any]((_, _) => ())
		implicit val Unit :SetParameter[Unit] = SetParameter[Unit]((_, _) => ())

	}


	def GetColumns[E](columns :ColumnMapping[_]*) = GetResult[Seq[Any]](
		params => columns.map(_(params))
	)



	trait MappingReadForm[M<:AnyMapping] extends SQLReadForm[M#ResultType] {
		val mapping :M

		def columns :Seq[M#Component[_]]
		
//		def include(components :M#Component[_]*) :MappingReadForm[M]

		def apply(values :ComponentValues[M]) :M#ResultType

		def opt(values :ComponentValues[M]) :Option[M#ResultType]

		override def readColumns = columns.size
	}


	class ColumnsReadForm[M<:Mapping[E], E](val mapping :M, read :M#Component[_]=>SQLReadForm[_], val columns :Seq[M#Component[_]])
		extends MappingReadForm[M]
	{
		override def apply(values: ComponentValues[M]): E = values.value(mapping)

		override def opt(values: ComponentValues[M]): Option[E] = values.getValue(mapping)

		override def opt(position: Int)(res: ResultSet): Option[E] =
			opt(PositionedResultView(res, position))

		override def opt(res: PositionedResult): Option[E] = {
			val values = columns.map(read(_).opt(res))
			mapping.optionally(ComponentValues(mapping :mapping.type)(comp => columns.indexOf(comp).providing(_>=0).flatMap(values(_))))
		}

//		override def include(components: M#Component[_]*): MappingReadForm[M] =

		override val readColumns: Int = (0 /: columns)(_ + read(_).readColumns)

		override def nullValue: E = mapping.nullValue getOrElse {
			mapping(ComponentValues(mapping)(comp => columns.indexOf(comp).providing(_>=0).map(i => read(columns(i)).nullValue)))
		}

		private val mappingString = mapping.sqlName getOrElse mapping.unqualifiedClassName
		override def toString = columns.map(read).mkString(s"^$mappingString{", ",", "}")
	}


	object MappingReadForm {

		def apply[E](mapping :Mapping[E])(components :Seq[mapping.Component[_]], form :mapping.Component[_]=>SQLReadForm[_] = _.selectForm) :SQLReadForm[E] =
			new ColumnsReadForm[mapping.type, E](mapping, form, components)

		def selectForm[E](mapping :Mapping[E]) :SQLReadForm[E] =
			apply(mapping :mapping.type)(mapping.selectable, _.selectForm)


	}



	class MappingWriteForm[M<:AnyMapping](mapping :M, write :M#Component[_]=>SQLWriteForm[_], components :Seq[M#Component[_]])
		extends SQLWriteForm[M#ResultType]
	{
		override def apply(params: PositionedParameters, value: M#ResultType): Unit =
			components.foreach{ c =>
				val comp = c.asInstanceOf[mapping.Component[Any]]
				write(c).asInstanceOf[SQLWriteForm[Any]](params, (mapping \\ comp)(value.asInstanceOf[mapping.ResultType]))
			}

		override def literal(value: M#ResultType, inline :Boolean): String = {
			val literals = components.map { c =>
				val comp = c.asInstanceOf[mapping.Component[Any]]
				write(c).asInstanceOf[SQLWriteForm[Any]].literal((mapping \\ comp)(value.asInstanceOf[mapping.ResultType]), inline)
			}
			if (inline) literals.mkString("", ", ", "")
			else literals.mkString("(", ", ", ")")
		}

		override def nullLiteral(inline :Boolean): String = {
			val literals = components.map(write(_).nullLiteral)
			if (inline) literals.mkString("", ", ", "")
			else literals.mkString("(", ", ", ")")
		}


		override def literal(value: M#ResultType): String = literal(value, false)

		override def inlineLiteral(value: M#ResultType): String = literal(value, true)

		override def nullLiteral: String = nullLiteral(false)

		override def inlineNullLiteral: String = nullLiteral(true)


		override val writtenColumns: Int = (0 /: components)(_ + write(_).writtenColumns)

		override def nullParam(params: PositionedParameters): Unit =
			components.foreach(write(_).nullParam(params))

		override def toString = components.map(write).mkString(s"v$mapping{", ",", "}")
	}


	object MappingWriteForm {
		def apply[E](mapping :Mapping[E])(components :Seq[mapping.Component[_]], write :mapping.Component[_]=>SQLWriteForm[_]) :SQLWriteForm[E] =
			new MappingWriteForm[mapping.type](mapping, write, components)


		def insertForm[E](mapping :Mapping[E]) :SQLWriteForm[E] = apply(mapping :mapping.type)(mapping.insertable, _.insertForm)
		def updateForm[E](mapping :Mapping[E]) :SQLWriteForm[E] = apply(mapping :mapping.type)(mapping.updatable, _.updateForm)
		def queryForm[E](mapping :Mapping[E]) :SQLWriteForm[E] = apply(mapping :mapping.type)(mapping.querable, _.queryForm)

	}

}

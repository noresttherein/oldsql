package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping


import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnMapping.ColumnOption.ColumnFlagType
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnType.NullValue
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ComponentMapping.{DirectComponentMappingAdapter, EmbeddedMappingComponent}
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.Mapping.{SetParameters}
import com.adpilot.cortb.clientapi.util.ObjectProperty
import com.adpilot.cortb.clientapi.util.Repeat._

import scala.collection.mutable.ListBuffer
import scala.slick.jdbc.{PositionedParameters, PositionedResult}

import ColumnMapping._
import ColumnOption._


trait MappingSupport[E] extends Mapping[E] { self =>

	trait Component[T]  extends ComponentMapping[E, T] {

		private[MappingSupport] def belongsTo(mapping :MappingSupport[_]) = mapping eq self

		private[MappingSupport] final lazy val valuesForMe = init()

		protected[MappingSupport] final def include() :this.type = { valuesForMe; this }

		private[MappingSupport] def init() :ColumnValues => ColumnValues = {
			def include[X](c :Column[X]) =
				new EmbeddedCol[Column[X], T, X](c, value, None, c.options).include()
			//todo: what if columns are reused between components/parent mapping?
			val map = columns.map(c => c -> include(c)).toMap[ColumnMapping[_, _], ColumnMapping[_, _]]
			ColumnSelection.Adapted(map)
		}

//		private[MappingSupport] val valuesForMe :ColumnSelection = ColumnSelection.Inverse(columns, {
//			case c:EmbeddedCol[_, _, _] if c.belongsTo(self) => c.adaptedMapping
//		})

//		private[MappingSupport] def valuesForMe(values :ColumnValues) :ColumnValues =
//			values.slice(index, index + selectable.size)

		private[MappingSupport] def liftNestedComponent[X](child :Component[X]) :self.Component[_] =
			new self.NestedComponent[T, X](this, child)

		delayInit(this)

	}

	object Component {
		implicit def valueOf[T](comp :self.Component[T])(implicit res :MappingResult[self.type, E]) :T =
			res.get(comp)

		implicit def resultFor[T](comp :self.Component[T])(implicit res :MappingResult[self.type, E]) :MappingResult[comp.type, T] =
			res :\ comp


	}


	trait Column[T] extends ColumnMapping[E, T] with Component[T] {
		final override private[MappingSupport] def init() = {
//			System.err.println(s"including $this in $self")
			if (exported)
				throw new IllegalStateException(s"cannot initialize component $this of $self: components have already been exported. Create components eagerly!")
//			if (all.exists(_.name==name))
//				throw new IllegalArgumentException(s"duplicate column '$name' in $self")

			comps += this
			all += this
			if (!enabled[NoQuery]) queries += this
			if (!enabled[NoSelect]) selects += this
			if (!enabled[NoInsert]) inserts += this
			if (!enabled[NoUpdate]) updates += this
			if (enabled[AutoGen]) auto += this

			identity
//			ColumnSelection.Single(this)
		}
		
	}

	object Column {
		implicit def valueOf[T](comp :self.Column[T])(implicit res :MappingResult[self.type, E]) :T = try {
			res.get(comp)
			} catch {
				case e :Exception =>
					System.err.println(s"failed to read value for $comp :${comp.columnType}")
					throw e
			}

	}


	abstract class ComponentSupport[T](pick :E=>T, override final val columnPrefix :String="") extends MappingSupport[T] with Component[T] {
		final override def value(entity: E): T = pick(entity)
	}





	class Embedded[M<:Mapping[T], T] protected[MappingSupport] (comp :M, pick :E=>T, prefix :Option[String])
		extends EmbeddedMappingComponent[E, T, M](comp, pick, Some(prefix.map(columnPrefix + _) getOrElse columnPrefix)) with Component[T]


	object Embedded {
		implicit def husk[M<:Embedded[C, _], C<:Mapping[_]](husk :M) :C = husk.adaptedMapping
	}

	protected class EmbeddedCol[M<:ColumnMapping[C, T], C, T](column :M, pick :E=>C, name :Option[String], options :Seq[ColumnOption[T]])
		extends EmbeddedColumn(column, pick andThen column.value, Some(columnPrefix + (name getOrElse column.name)), options) with Column[T]

	private class NestedComponent[S, T](parent :Component[S], child :Component[S]#Component[T])
		extends Component[T] with DirectComponentMappingAdapter[E, T, Component[S]#Component[T]]
	{
		override val adaptedMapping = child

		override private[MappingSupport] def init()  =
			parent.valuesForMe andThen parent.valuesFor(child.asInstanceOf[parent.Component[_]])

		override def value(entity: E): T = adaptedMapping.value(parent.value(entity))

//			
	}


	type ResultRow = MappingResult[this.type, E]

	protected def assemble(implicit res :ResultRow) :E


	final override def apply(values: ColumnValues): E = {
		val result = MappingResult[this.type, E](this :this.type, values)
		assemble(result)
	}


	override def valuesFor(component: Component[_]) =
		component.valuesForMe



	lazy val InsertParameters = SetParameters(insertable:_*)
	lazy val UpdateParameters = SetParameters(updatable:_*)

	final def components = afterInit(comps.toList)
	final def columns = afterInit(all.toList)
	final def selectable = afterInit(selects.toList)
	final def querable = afterInit(queries.toList)
	final def updatable = afterInit(updates.toList)
	final def insertable = afterInit(inserts.toList)
	final def generated = afterInit(auto.toList)

	final lazy val nestedComponents = components.flatMap(c => c +: c.nestedComponents.map(c.liftNestedComponent(_)))


	private[this] val comps = new ListBuffer[Component[_]]
	private[this] val all = new ListBuffer[Column[_]]()
	private[this] val selects = new ListBuffer[Column[_]]()
	private[this] val queries = new ListBuffer[Column[_]]()
	private[this] val updates = new ListBuffer[Column[_]]()
	private[this] val inserts = new ListBuffer[Column[_]]()
	private[this] val auto = new ListBuffer[Column[_]]()
	@volatile
	private[this] var exported = false


	private def delayInit(c :Component[_]) :Unit =
		if (exported)
			throw new IllegalStateException(s"cannot initialize mapping component $c on $this: components have already been exported")
		else
			delayedInits += c

	private[this] val delayedInits = new ListBuffer[Component[_]]()

	protected final def initialize() = synchronized {
		delayedInits.foreach(_.include())
		delayedInits.clear()
	}

	protected final def initAfter[T](value :T) = { initialize(); value }

	protected final def afterInit[T](block : =>T) = { initialize(); block }

	private def export[T](list :ListBuffer[T]) :List[T] = afterInit {
		exported = true
		list.toList
	}



	import Mapping.prefixOption

	protected def columnPrefix :String = ""


	protected def embed[C<:Mapping[T], T](component :C, pick :E=>T, prefix :String="") :Embedded[C, T] =
		initAfter(new Embedded[C, T](component, pick, prefixOption(prefix)))



	protected def embed[C<:ComponentMapping[E, T], T](component :C, prefix :Option[String]) :Embedded[C, T] =
		embed(component, component.value(_), prefix.orNull)

	protected def embed[C<:ComponentMapping[E, T], T](component :C, prefix :String) :Embedded[C, T] =
		embed(component, prefixOption(prefix))

	protected def embed[C<:ComponentMapping[E, T], T](component :C) :Embedded[C, T] =
		embed(component, None)



	class ColumnEmbedder[C, T](source :ColumnMapping[C, T], name :Option[String], options :Seq[ColumnOption[T]]) {
		def this(source :ColumnMapping[C, T]) = this(source, None, source.options)

		def apply()(implicit ev:E=:=C) :Column[T] = apply(ev(_))

		def apply(pick :E=>C) :Column[T] =
			source match {
				case c:MappingSupport[_]#Column[_] if c.belongsTo(self) => c.asInstanceOf[Column[T]]
				case _ => initAfter(new EmbeddedCol(source, pick, name, options))
			}

		def prefixed(prefix :Option[String]) :ColumnEmbedder[C, T] =
			new ColumnEmbedder(source, prefix.map(_ + (name getOrElse source.name)), options)

		def prefixed(prefix :String) :ColumnEmbedder[C, T] = prefixed(Some(prefix))

		def named(name :String) :ColumnEmbedder[C, T] =
			new ColumnEmbedder(source, Some(name), options)

		def set(options :ColumnOption[T]*) :ColumnEmbedder[C, T] =
			new ColumnEmbedder(source, name, options)

		def add(options :ColumnOption[T]*) :ColumnEmbedder[C, T] =
			new ColumnEmbedder(source, name, options ++: this.options)

		def remove(options :ColumnOption[T]*) :ColumnEmbedder[C, T] =
			new ColumnEmbedder(source, name, this.options.filterNot(options.contains) )
	}

//	class ColumnBuilder[T](pick :E=>T, name :Option[String]=None, options :Seq[ColumnOption[T]]=Seq()) {
//
//		def apply()(implicit tpe :ColumnType[T]) :Column[T] = column(name getOrElse guessName(pick), pick, options:_*)
//
//		def apply(name :String, options :ColumnOption[T]*)(implicit tpe :ColumnType[T]) :Column[T] = named(name).set(options:_*)()
//
//
//		def named(name :String) :ColumnBuilder[T] = new ColumnBuilder(pick, Some(name), options)
//
//		def set(options :ColumnOption[T]*) :ColumnBuilder[T] = new ColumnBuilder(pick, name, options)
//
//		def add(options :ColumnOption[T]*) :ColumnBuilder[T] = new ColumnBuilder[T](pick, name, options ++: this.options)
//
//		def remove(options :ColumnOption[T]*) :ColumnBuilder[T] = new ColumnBuilder(pick, name, this.options.filterNot(options.contains))
//
//
//		private def guessName(pick :E=>T) :String = ObjectProperty(pick).name.replace('.', '_')
//	}


	protected def column[C, T](col :ColumnMapping[C, T]) :ColumnEmbedder[C, T] =
		new ColumnEmbedder(col)

	protected def column[C, T](col :ColumnMapping[C, T], name :String) :ColumnEmbedder[C, T] =
		new ColumnEmbedder(col, Some(name), col.options)

	protected def column[C, T](col :ColumnMapping[C, T], options :Seq[ColumnOption[T]]) :ColumnEmbedder[C, T] =
		new ColumnEmbedder(col, None, options)

	protected def column[C, T](col :ColumnMapping[C, T], name :String, options :Seq[ColumnOption[T]]) :ColumnEmbedder[C, T] =
		new ColumnEmbedder(col, Some(name), options)




	protected def column[T](col :ColumnMapping[E, T], prefix :Option[String], options :Seq[ColumnOption[T]]) :Column[T] =
		new ColumnEmbedder(col).prefixed(prefix).set(options:_*)()

	protected def column[T](col :ColumnMapping[E, T], prefix :Option[String]) :Column[T] =
		column(col, prefix, col.options)

	protected def column[T](col :ColumnMapping[E, T], prefix :String, options :Seq[ColumnOption[T]]) :Column[T] =
		column(col, prefixOption(prefix), options)

	protected def column[T](col :ColumnMapping[E, T], prefix :String) :Column[T] =
		column(col, prefixOption(prefix), col.options)

	protected def column[T](col :ColumnMapping[E, T], options :Seq[ColumnOption[T]]) :Column[T] =
		column(col, None, options)

	protected def column[T](col :ColumnMapping[E, T]) :Column[T] =
		column(col, None, col.options)



	protected def column[T :ColumnType](name :String, pick :E=>T, options :ColumnOption[T]*) :Column[T] =
		initAfter(new ColumnComponent[E, T](columnPrefix+name, pick, options) with Column[T])

//	protected def column[T :ColumnType](name :String, pick :E=>T, options :ColumnFlagType[ColumnOption]) :Column[T] =
//		column(name, pick, options.map(_[T]() :ColumnOption[T]):_*)


	protected def column[T :ColumnType](pick :E=>T)(name :String, options :ColumnOption[T]*) :Column[T] =
		column[T](name, pick, options:_*)




//	protected def autoins[T :NullValue](col :ColumnMapping[E, Option[T]]) :Column[T] = {
//		val options = AutoGen[T] +: col.options.map(_.map(_ getOrElse NullValue.Null[T])(Some(_)))
//		column[T](col.name, col.value(_ :E).get, options :_*)(ColumnType[T](
//			(t :T, params :PositionedParameters) => col(Some(t), params), col(_ :PositionedResult).get
//		))
//	}



	protected def autoins[T :ColumnType](name :String, pick :E=>Option[T], options :ColumnOption[T]*) :Column[T] =
		this.column[T](name, pick(_:E).get, AutoGen +: options:_*)

}

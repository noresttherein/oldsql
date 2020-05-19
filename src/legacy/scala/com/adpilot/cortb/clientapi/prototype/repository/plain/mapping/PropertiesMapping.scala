package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{Full, UniquePropertyReference, One}
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnMapping.ColumnOption.OptionalSelect
import com.adpilot.cortb.clientapi.util.Generic.GenericFunction
import com.adpilot.cortb.clientapi.util.{OptionOps, PropertyChain, Matching, ObjectProperty}
import com.adpilot.cortb.clientapi.util.ObjectProperty.AnyProperty

import scala.reflect.runtime.universe.TypeTag

import scala.slick.jdbc.{StaticQuery => Q, SetParameter, GetResult}

import ForeignTableFilter.JoinProperty
import OptionOps._


trait PropertiesMapping[E, PK] extends Mapping[E] {
	table :EntityMapping[E, PK] =>

	protected implicit val typeTag :TypeTag[E]

	import Matching._

	class PropertyComponent[T] private[PropertiesMapping] (val component :Component[T], val property :ObjectProperty[E, T]) {
		def owner :table.type = table

		val columns =
			if (component.querable.size==1) component.querable(0).name
			else component.querable.mkString("(", ", ", ")")


		val where = table.where(component)
		val setter = table.parameter(component)

		def filter(values :Seq[T]) :TableFilter[Seq[T]] = table.filter(component, values)

		def filter(value :T) :TableFilter[T] = table.filter(component, value)

		def filter(alias :String, value :T) :TableFilter[T] = table.filter(alias, component, value)
		

		val selectPK = select(table.PK.selectable)

//		def select(component :Component[_]) :String = select(component.selectable)
		def select(component :ComponentMapping[E, _]) :String = select(component.selectable)

		def select(columns :Seq[ColumnMapping[_, _]]) :String = {
			val header = columns.map(_.selectHeader("")).mkString(",")
			s"select $header from ${table.qname} where $where"
		}

		def withValue(value :Any) = new ComponentValue(this, value.asInstanceOf[T])

		protected[PropertiesMapping] def joinFilter[S, X](whole :PropertyChain[S, X], path :Option[S=>One[E]], rest :PropertyChain[E, X], values :Seq[X]) :ForeignTableFilter[S, X] =
			if (rest.chain==property.chain)
				ForeignTableFilter[S, X](filter(values.asInstanceOf[Seq[T]]).asInstanceOf[TableFilter[Seq[X]]], path)
			else
				throw new IllegalArgumentException(s"can't create a filter for $whole: property $rest is not a part of $table (and $property is not a foreign key). Known components: ${table.propertyComponents.map(_.property)}")

		protected[PropertiesMapping] def isToOne(property :PropertyChain[E, _]) :Boolean = false

	}



	class ForeignKeyPropertyComponent[T] private[PropertiesMapping](_c :Component[One[T]], _p :ObjectProperty[E, One[T]], val foreignTable :EntityMapping[T, _])
		extends PropertyComponent[One[T]](_c, _p)
	{

		override protected[PropertiesMapping] def joinFilter[S, X](whole: PropertyChain[S, X], path: Option[(S) => One[E]], rest: PropertyChain[E, X], values: Seq[X]): ForeignTableFilter[S, X] =
			if (rest.chain==property.chain)
				ForeignTableFilter[S, X](filter(values.asInstanceOf[Seq[One[T]]]).asInstanceOf[TableFilter[Seq[X]]], path) //TODO: if value is not a PK this might requrie a subselect
			else {
				val next = rest.drop(property).flatMap(_.drop(JoinProperty[T])) getOrElse (
					throw new IllegalArgumentException(s"can't filter on $whole: property $property is not a prefix of $rest (or is not followed by join property) - most likely a bug")
				)
				val mapping = foreignMapping getOrElse (throw new IllegalArgumentException(s"can't filter on $whole: last $foreignTable is not a PropertiesMapping (following $rest)"))
				val extended = path match {
					case Some(p) => p andThen (_.join) andThen property.fun
					case None if whole==rest => property.fun.asInstanceOf[S=>One[T]]
					case _ => throw new IllegalArgumentException(s"can't filter on $whole: got None as path to $table (remainder: $rest). This is a bug.")
				}
				mapping.joinFilter(whole, Some(extended), next, values)
			}


		override protected[PropertiesMapping] def isToOne(rest: PropertyChain[E, _]): Boolean =
			if (rest.chain==this.property.chain) true
			else {
				val next = rest.drop(property).flatMap(_.drop(JoinProperty[T])) getOrElse (
					throw new IllegalArgumentException(s"$property is not a prefix of $rest (or is not followed by join property) - most likely a bug")
				)
				foreignMapping.exists(_.ToOne.is(next))
			}
			

		private def foreignMapping = foreignTable match {
			case m :PropertiesMapping[_, _] => Some(m.asInstanceOf[PropertiesMapping[T, _]])
			case _ => None
		}
	}


	@deprecated
	class ComponentValue[T](val property :PropertyComponent[T], value :T) {
		def query[R :GetResult](select :String) =
			Q.query[T, R](select)(property.setter, implicitly[GetResult[R]])(value)

		def select(properties :Seq[E => One[Any]]) =
			table.joinSelect[T](Some(property.where), properties:_*)(property.setter)(value)

		def delete = table.deleteWhere[T](property.where)(property.setter)(value)

		def update(statement :String) =
			Q.update[T](statement)(property.setter)(value)
	}



	object PropertyComponent {


		def apply[T](property :PropertyChain[E, T]) :Option[PropertyComponent[T]] =
			componentsByProperty.get(property.chain).asInstanceOf[Option[PropertyComponent[T]]]

		def apply[T](property :E=>T) :Option[PropertyComponent[T]] =
			apply(ObjectProperty(property))

		def unapply[T](property :PropertyChain[E, T]) :Option[PropertyComponent[T]] = apply(property)


		private[PropertiesMapping] def create[T](component :Component[T]) :Option[PropertyComponent[_]] =
			ForeignKeyComponent.map(component.value)(FKCompBuilder) match {
				case Some(res) => res
				case None => ObjectProperty.optional(component.value).map(new PropertyComponent[T](component, _))
			}

		private type MaybeForeignKeyProperty[T] = Option[ForeignKeyPropertyComponent[T]]

		private object FKCompBuilder extends GenericFunction[ForeignKeyComponent, MaybeForeignKeyProperty] {
			override def apply[T](x: ForeignKeyComponent[T]): MaybeForeignKeyProperty[T] =
				ObjectProperty.optional(x.value).map(fk => new ForeignKeyPropertyComponent[T](x, fk, ReferencedTable(x.value)))
		}

	}



	private lazy val componentsByProperty = nestedComponents.flatMap(PropertyComponent.create(_)).map(
		pc => (pc.property.chain, pc)).toMap[PropertyChain[E, _], PropertyComponent[_]]


	def propertyComponents = componentsByProperty.values




	 
	object ForeignKeyProperty {
//		def unapply[T](property :PropertyPath[E, T]) :Option[PropertyComponent[T with One[_]]] =
//			PropertyComponent(property).flatMap(check)

		def unapply[T](property :PropertyComponent[T]) :Option[PropertyComponent[One[_]]] =
			check(property)

		def check[T](cmp :PropertyComponent[T]) :Option[PropertyComponent[One[_]]] =
			cmp.property.fun match {
				case table.ForeignKeyComponent(_) => Some(cmp.asInstanceOf[PropertyComponent[One[_]]])
				case _ => None
			}
		
		def check[T](property :PropertyChain[E, T]) :Option[PropertyComponent[One[_]]] = PropertyComponent(property).flatMap(check(_))

	}

	object PrefixComponent {

		def unapply(property :PropertyChain[E, _]) :Option[PropertyComponent[_]] =
			PropertyComponent(property) match {
				case Some(c) => Some(c)
				case None =>
					val prefixes = propertyComponents.filter(_.property.prefixOf(property))
					if (prefixes.isEmpty)
						None
					else
						Some(prefixes.maxBy(_.property.name.length))
			}

		def unapply(property :E=>Any) :Option[PropertyComponent[_]] =
			unapply(ObjectProperty(property))

		def apply(property :PropertyChain[E, _]) :Option[PropertyComponent[_]] = unapply(property)
	}
	

	object ToOne {
		def unapply(property :ObjectProperty[E, _]) :Option[ObjectProperty[E, One[_]]] = 
			is(property).ifTrue(property.asInstanceOf[ObjectProperty[E, One[_]]])
		
		def is(property :PropertyChain[E, _]) :Boolean =
			PrefixComponent(property.chain).exists(_.isToOne(property))
	}

	object OptionalComponent {
		def is(property :PropertyChain[E, _]) :Boolean =
			PrefixComponent(property.chain).map(_.component).collect{ case col :ColumnMapping[_, _] if OptionalSelect.enabled(col) => col }.nonEmpty
	}


	def joinFilter[T](property :PropertyChain[E, T], values :Seq[T]) :ForeignTableFilter[E, T] =
		joinFilter(property, None, property, values)

	private def joinFilter[S, T](whole :PropertyChain[S, T], path :Option[S=>One[E]], rest :PropertyChain[E, T], values :Seq[T]) :ForeignTableFilter[S, T] = rest match {
		case PrefixComponent(cmp) => cmp.joinFilter(whole, path, rest, values)
		case _ => throw new IllegalArgumentException(s"can't create filter for $whole: $rest is not a part of $table")
	}


}



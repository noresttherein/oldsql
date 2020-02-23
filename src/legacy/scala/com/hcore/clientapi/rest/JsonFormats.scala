package com.hcore.clientapi.rest

import java.text.SimpleDateFormat
import java.util.Date
import javax.xml.bind.DatatypeConverter

import com.hcore.clientapi.entities.Model.Id
import com.hcore.clientapi.rest.items.ItemMeta.PropertyType.TransportValue
import com.hcore.clientapi.rest.items._
import com.hcore.clientapi.rest.mappings.ItemRepresentation
import com.hcore.ogre.slang.options.extensions
import play.api.libs.json._

//implicits
import extensions._

object JsonFormats {
	import Fields._

	def reads[T :Reads] = implicitly[Reads[T]]
	def writes[T :Writes] = implicitly[Writes[T]]
	def format[T :Format] = implicitly[Format[T]]
	

	def map[T :Format, X](read :T=>X, write :X=>T) :Format[X] = //new Format[X] {
		Format(format[T].map(read), Writes(write andThen format[T].writes))
//		override def writes(o: X): JsValue = format[T].writes(write(o))
//
//		override def reads(json: JsValue): JsResult[X] = Try(format[T].reads(json).map())
//	}
	
	implicit val DateFormat = {
		def format = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
		//don't change these to method values! We need def format to be evaluated each time passed functions are called as it's not thread safe
		map[String, Date](format.parse(_), format.format(_))
	}


	//java 8
	//implicit val ByteArrayFormat = map[String, Array[Byte]](Base64.getDecoder.decode, Base64.getEncoder.encodeToString)
	val Base64BinaryFormat = map[String, Array[Byte]](DatatypeConverter.parseBase64Binary, DatatypeConverter.printBase64Binary)
	implicit val ByteArrayFormat = Base64BinaryFormat





	implicit val IdFormat = map[Long, Id](Id, _.value)
	

	implicit object IdItemFormat extends Format[IdItem[Any]] {

		override def reads(json: JsValue): JsResult[IdItem[Any]] = unapply(json) match {
			case Some(x) => JsSuccess(x)
			case None => JsError(s"""invalid id reference. Expected { "$ItemId" : <number> }""")
		}

		override def writes(o: IdItem[Any]): JsValue = apply(o.id)

		def unapply(js :JsValue) :Option[IdItem[Nothing]] = js match {
			case JsObject(Seq((ItemId, JsNumber(id)))) => Some(IdItem(Id(id.toLong)))
			case _ => None
		}

		def apply(id :Id): JsValue =
			Json.obj(ItemId -> id.value)
	}

	private object PropertyItemJson {
		def unapply(js :JsValue) :Option[(String, TransportValue)] = js match {
			case o @ JsObject(fields) if fields.map(_._1).toSet == ItemPropertyFields =>
				val name = (o \ ItemPropertyName).as[String]
				val value = fromJsValue(o \ ItemPropertyValue)
				value.map((name, _))
			case _ => None
		}

		def apply(property :String, value :TransportValue): JsValue =
			Json.obj(ItemPropertyName -> property, ItemPropertyValue -> toJsValue(value))

	}


	
	
	private class ItemFormat[X :Format :ItemMeta] extends Format[Item[X]] {

		override def reads(json: JsValue): JsResult[Item[X]] = json match {
			case IdItemFormat(item) => JsSuccess(item)
			case PropertyItemJson(prop, value) => JsSuccess(ItemMeta[X].item(prop, value))
			case JsNull => JsSuccess(Item())
			case JsUndefined() => JsSuccess(Item())
			case _ => format[X].map(Item.meta[X].item).reads(json)
		}

		override def writes(o: Item[X]): JsValue = o match {
			case IdItem(id) => IdItemFormat(id)
			case PropertyItem(property, value) => PropertyItemJson(property.name, property.tpe.write(value))
			case ValueItem(value) => format[X].writes(value)
			case AbsentItem => JsNull
		}
	}
	
	implicit def ItemFormat[X :Format :ItemMeta] :Format[Item[X]] = new ItemFormat[X]





	private object PropertyItemsJson {
		def unapply(js :JsValue) :Option[(String, Seq[TransportValue])] = js match {
			case PropertyItemJson(property, value) =>
				Some(property, Seq(value))
			case o @ JsObject(fields) if fields.map(_._1).toSet == ItemsPropertyFields =>
				val name = (o \ ItemPropertyName).as[String]
				(o \ ItemPropertyValues) match {
					case JsArray(values) =>
						values.flatMap(fromJsValue).providing(_.size==values.size).map((name, _))
					case _ => None
				}
			case _ => None
		}

		def apply(property :String, values :Seq[TransportValue]): JsValue =
			if (values.size==1)
				PropertyItemJson(property, values.head)
			else
				Json.obj(ItemPropertyName -> property, ItemPropertyValues -> Json.arr(values.map(toJsValue)))

	}


	implicit def ItemsFormat[X :Format :ItemMeta] :Format[Items[X]] = new Format[Items[X]] {

		override def reads(json: JsValue): JsResult[Items[X]] = json match {
			case JsUndefined() | JsNull | JsObject(Seq()) | JsArray(Seq()) =>
				JsSuccess(ItemsAbsent)
			case JsArray(items) =>
				val propertyValues = items.flatMap(PropertyItemJson.unapply)
				if (propertyValues.size==items.size)
					if (propertyValues.forall(_._1 == propertyValues(0)._1))
						JsSuccess(ItemMeta[X].items(propertyValues(0)._1, propertyValues.map(_._2)))
					else
						JsError("different property references in one collection are not supported")
				else
					jsResult(items.map(format[X].reads))(parsed => JsSuccess(Items(parsed.map(_.get))))
			case JsString(ItemsAllName) =>
				JsSuccess(ItemsAll)
			case PropertyItemJson(property, value) =>
				JsSuccess(ItemMeta[X].items(property, value))
			case PropertyItemsJson(property, values) =>
				JsSuccess(ItemMeta[X].items(property, values))
			case JsObject(_) =>
				format[X].reads(json).map(x => Items(Seq(x)))
		}

		override def writes(o: Items[X]): JsValue = o match {
			case ItemsValues(items) => JsArray(items.map(format[X].writes))
			case ItemsByProperty(property, value) => PropertyItemJson(property.name, property.tpe.write(value))
			case ItemsByPropertyValues(property, values) => PropertyItemsJson(property.name, values.map(property.tpe.write))
			case ItemsAbsent => JsNull
			case ItemsAll => JsString(ItemsAllName)
		}
	}

	private class LazyFormat[T](format : =>Format[T]) extends Format[T] {
		private lazy val internal = format
		override def writes(o: T): JsValue = internal.writes(o)
		override def reads(json: JsValue): JsResult[T] = internal.reads(json)
	}
	def LazyFormat[T](format : =>Format[T]) :Format[T] = new LazyFormat(format)

	import ItemRepresentation._
	import TargetingPolicyItem._

	implicit lazy val targetingPolicyGeoFormat :Format[Geography] = LazyFormat(Json.format[Geography])
	implicit lazy val targetingPolicyTopicsFormat :Format[Topics] = LazyFormat(Json.format[Topics])
	implicit lazy val clientFormat :Format[ClientItem] = LazyFormat(Json.format[ClientItem])
	implicit lazy val productFormat :Format[ProductItem] = LazyFormat(Json.format[ProductItem])
	implicit lazy val targetingPolicyFormat :Format[TargetingPolicyItem] = LazyFormat(Json.format[TargetingPolicyItem])
	implicit lazy val orderFormat :Format[OrderItem] = LazyFormat(Json.format[OrderItem])
	implicit lazy val landingFormat :Format[LandingItem] = LazyFormat(Json.format[LandingItem])
	implicit lazy val creativeFormat :Format[CreativeItem] = LazyFormat(Json.format[CreativeItem])
	implicit lazy val campaignFormat :Format[CampaignItem] = LazyFormat(Json.format[CampaignItem])
	implicit lazy val pixelFormat :Format[PixelItem] = LazyFormat(Json.format[PixelItem])
	implicit lazy val tagFormat :Format[TagItem] = LazyFormat(Json.format[TagItem])

	implicit lazy val biddingFormat :Format[BiddingItem] = LazyFormat(Json.format[BiddingItem])




	implicit def seqFormat[E :Format] :Format[Seq[E]] = new Format[Seq[E]] {
		val elemFormat = format[E]

		override def writes(o: Seq[E]): JsValue = JsArray(o.map(elemFormat.writes(_)))

		override def reads(json: JsValue): JsResult[Seq[E]] = json match {
			case JsArray(elems) =>
				jsResult(elems.map(elemFormat.reads))(parsed => JsSuccess(parsed.map(_.get)))
			case _ => JsError("expected []")
		}
	}

	private def jsResult[X, Y](x :Seq[JsResult[X]])(fun :Seq[JsResult[X]]=>JsResult[Y]) :JsResult[Y] = {
		val errors = x.view.zipWithIndex.collect {
			case (error :JsError, idx) => error.errors.map {
				case (path, e) => (JsPath(idx) ++ path, e)
			}
		}.force.flatten
		if (errors.nonEmpty) JsError(errors)
		else fun(x)
	}


	private def toJsValue(value :TransportValue) :JsValue = value.value match {
		case null => JsNull
		case x:String => JsString(x)
		case x:BigDecimal => JsNumber(x)
		case x:Boolean => JsBoolean(x)
		case x => throw new IllegalArgumentException(s"Unrecognized transport property value type :$x")
	}
	private def fromJsValue(json :JsValue) :Option[TransportValue] = json match {
		case JsNull => Some(TransportValue.Null)
		case JsString(x) => Some(TransportValue(x))
		case JsNumber(x) => Some(TransportValue(x))
		case JsBoolean(x) => Some(TransportValue(x))
		case _ => None
	}

	object Fields {
		val ItemId = "id"
		val ItemPropertyName = "property"
		val ItemPropertyValue = "value"
		val ItemPropertyValues = "values"
		val ItemPropertyFields = Set(ItemPropertyName, ItemPropertyValue)
		val ItemsPropertyFields = Set(ItemPropertyName, ItemPropertyValues)

		val ItemsAllName = "All"
	}


}

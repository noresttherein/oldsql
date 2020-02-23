package com.adpilot.cortb.clientapi.rest.items

import java.util.Date

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}
import com.adpilot.cortb.clientapi.prototype.repository.entities.{Campaign, Product}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference._


case class CreativeItem(
	id :Option[Id],
	hash :Option[String],
	format :String,
	fileName :String,
	fileFormat :String,
	status :String,
	creativeType :String,
	image :Item[CreativeImageItem],
	product :Item[ProductItem],
	campaigns :Items[CampaignItem]
)

/*
case class Creative(id :Option[Id],
                    hash :String,
                    format :Creative.Format,
                    file :Creative.File,
                    addedAt :Date,
                    deletedAt :Option[Date],
                    status :Creative.Status,
                    ctype :Creative.Type,
                    product :One[Product],
                    campaigns :Many[Set[Campaign]] = Many()
	                   ) extends HasId //ManyToMany Campaign


object Creative {
	case class File(name :String, format :FileFormat, data :Array[Byte]) {

		override def toString= s"[$name ($format), ${data.length} bytes]"

		def canEqual(other: Any): Boolean = other.isInstanceOf[File]

		override def equals(other: Any): Boolean = other match {
			case that: File if that canEqual this =>
				name == that.name && format == that.format && data.toSeq == that.data.toSeq
			case _ => false
		}

		override def hashCode(): Int = {
			val state = Seq(name, format, data)
			state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
		}
	}
	case class Format(format :String) //widthxheight
	case class FileFormat(name :String)
	case class Status(name :String)
	case class Type(name :String)
}
*/

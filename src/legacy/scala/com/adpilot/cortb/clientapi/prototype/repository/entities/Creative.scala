package com.adpilot.cortb.clientapi.prototype.repository.entities

import java.util.Date

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{Many, One}


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
	case class File(name :String, format :FileFormat, image :One[Image]) {

		override def toString= s"[$name ($format), $image]"

		def canEqual(other: Any): Boolean = other.isInstanceOf[File]

		override def equals(other: Any): Boolean = other match {
			case that: File if that canEqual this =>
				name == that.name && format == that.format && image == that.image
			case _ => false
		}

		override def hashCode(): Int = {
			val state = Seq(name, format, image)
			state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
		}
	}

	case class Format(format :String) //widthxheight
	case class FileFormat(name :String)
	case class Status(name :String)
	case class Type(name :String)
	
	case class Image(data :Array[Byte]) {
		override def equals(that :Any) = that match {
			case Image(data) => this.data.toSeq == data.toSeq
			case _ => false
		}

		override def hashCode =  data.toSeq.hashCode

		override def toString = s"${data.length} bytes"
	}
}

package com.hcore.clientapi.repository

import java.sql.Timestamp
import java.util.Date

import com.hcore.clientapi.entities.Components._
import com.hcore.clientapi.entities.Model.{HasId, Id, IdRef, One}
import com.hcore.clientapi.entities._
import com.hcore.ogre.mapping.ColumnType._
import com.hcore.ogre.mapping.EntityMapping.ForeignKeyType
import com.hcore.ogre.mapping.{ColumnType, EntityMapping}
import com.hcore.ogre.model.Restriction.PropertyEquals
import com.hcore.ogre.model.Reference.Single

import scala.reflect.runtime.universe.TypeTag
import scala.slick.jdbc.{GetResult, SetParameter}
import scala.util.Try

trait ColumnTypes {

	import ColumnType.{mapped => Boxed, safe}
//	import EntityMapping.ForeignKeyType._
	
//	def Nullable[S >:Null, T >:Null :ColumnType](from :T=>S)(to :S=>T) :ColumnType[S] =
//		Boxed[S, T](Option(_).map(from).orNull)(Option(_).map(to).orNull)
	
	protected val CSVSep = ","

	implicit def SeqType[S, T](implicit mapping :MappedType[S, T], seqType :ColumnType[Seq[T]]) :MappedType[Seq[S], Seq[T]] =
//		Boxed[Seq[S], Seq[T]](_.map(mapping.map))(_.map(mapping.unmap))
		Boxed[Seq[S], Seq[T]](_.flatMap(e => Try(mapping.map(e)).toOption.filter(_!=null)))(_.map(mapping.unmap))

	implicit def MappedOptionType[S, T](implicit mapping :MappedType[S, T], optType :ColumnType[Option[T]]) :MappedType[Option[S], Option[T]] =
		Boxed[Option[S], Option[T]](_.flatMap(t=>Option(mapping.map(t))))(_.flatMap(s => Option(mapping.unmap(s))))


	implicit def StringOptionType :MappedType[Option[String], String] =
		Boxed[Option[String], String](Option(_).filter(_.trim.length>0))(_.orNull)

//	implicit val byteArrayType = ColumnType[Array[Byte]](
//		SetParameter[Array[Byte]]{ case (bytes, params) => params.setBytes(bytes) }, GetResult[Array[Byte]](res => res.nextBytes())
//	)

	implicit val csvType = Boxed[Seq[String], String](
		Option(_).toSeq.flatMap(_.split(CSVSep)).map(_.trim).filter(_!=""))(_.mkString(CSVSep))

	implicit val dateType = Boxed[Date, Timestamp](d => new Date(d.getTime))(d => new Timestamp(d.getTime))
	implicit val urlType = safe[URL, String](URL(_))(_.name)
//	implicit val urlType = Boxed((s:String) => Try{new URL(s)}.toOption.orNull)(u => if (u==null) null else u.toString)


	implicit val clientStatusType = safe(Client.Status)(_.name)
	implicit val productTypeType = safe(Product.Type)(_.name)
	implicit val productStatusType = safe(Product.Status)(_.name)
	implicit val campaignTypeType = safe(Campaign.Type)(_.name)
	implicit val campaignStatusType = safe(Campaign.Status)( _.name)
	implicit val orderTypeType = safe(Order.Type)(_.name)
	implicit val orderStatusType = safe(Order.Status)(_.name)
	implicit val pixelTypeType = safe(Pixel.Type)(_.name)
	implicit val pixelStatusType = safe(Pixel.Status)(_.name)
	implicit val landingStatusType = safe(Landing.Status)(_.name)
	implicit val creativeTypeType = safe(Creative.Type)(_.name)
	implicit val creativeStatusType = safe(Creative.Status)(_.name)
	implicit val creativeFormat = safe(Creative.Format)(_.format)
	implicit val creativeFileFormat = safe(Creative.FileFormat)(_.name)


	implicit val countryType = safe(Country)(_.code)
	implicit val biddingStrategyType = safe(BiddingStrategy)(_.name)

	implicit val networkType = safe(Network)(_.name)
	implicit val hourType = safe(new Hour(_:String))(_.toString)
	implicit val geoType = safe(Geo)(_.location)
	implicit val languageType = safe(Language)(_.code)
	implicit val ipRangeType = safe(IPRange)(_.range)
	implicit val sspBucketType = safe(BucketSSP)(_.buckets)

	implicit val bucketType = safe(Bucket)(_.name)


	implicit val idType = Boxed(Id)(_.value)

	implicit def foreignKeyType[T <:HasId :TypeTag](implicit target :EntityMapping[T, Option[Id]]) :MappedType[One[T], Id] =
		ForeignKeyType(target, Some(_:Id), (pk:Option[Id]) => pk.getOrElse(throw new IllegalArgumentException(s"can't convert empty primary key of ${target} to a foreign key")))


	//	implicit def foreignKeyType[T :ColumnType, E] = Boxed[ForeignKey[T, E], T](Empty.apply)(_.key)
	//	import EntityMapping.{ForeignKeyType, EmptyForeignKeyType}

	//	implicit def IdRefType[T <:HasId :TypeTag :IdTable] :ForeignKeyType[Id, T, Option[Id]] =
	//		ForeignKeyType[Id, T, Option[Id]](implicitly[IdTable[T]], Some(_), _.getOrElse(throw new IllegalArgumentException(s"can't convert empty primary key of ${implicitly[IdTable[T]]} to a foreign key")))
//	implicit def IdRefType[T <:HasId] :MappedType[One[T], Id] =
//		Boxed[One[T], Id](IdRef[T](_))(ref => IdRef.unapply(ref) getOrElse (throw new IllegalArgumentException(s"can't get foreign key out of $ref")))


	//	private case class NothingKey[T](key :T) extends One[Nothing] {
//		override def toOpt: Option[Nothing] = None
//	}
	implicit val NothingKeyType =
		ColumnType.mapped[One[Nothing], Id](IdRef(_))(_ match {
//			case NothingKey(key :Id) => key
			case Single(PropertyEquals(_, Some(key :Id))) => key
//			case PropertyReference(_, key :Id) => key
//			case PropertyReference(_, Some(key :Id)) => key
			case one => throw new IllegalArgumentException(s"expected a a foreign key to Nothing, got $one :${one.getClass}")
		})



}

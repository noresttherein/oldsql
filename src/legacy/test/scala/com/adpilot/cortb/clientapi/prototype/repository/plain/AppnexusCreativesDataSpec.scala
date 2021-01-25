package com.adpilot.cortb.clientapi.prototype.repository.plain

import com.adpilot.cortb.clientapi.prototype.repository.DBConfig
import com.adpilot.cortb.clientapi.prototype.repository.entities.AppnexusCreativeData.Category
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{One, PropertyReference}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Selection.{SelectOne, SelectMany}
import com.adpilot.cortb.clientapi.prototype.repository.entities.{Creative, AppnexusCreativeData}
import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{Id, IdRef}
import org.scalatest._


class AppnexusCreativesDataSpec extends FlatSpec with Matchers {

  // get zakomentowany - nie ma bazy do ktorej moglby uderzac
  "AppnexusCreativesData store" should "save and read appnexus creative data" in {
    //    val dbConfig = new DBConfig("jdbc:postgresql://localhost:5431/adpilot", "adpilot", "adpilot")
    //    val repo = new PlainAPIRepository(dbConfig, Some("rtb"), Seq())
    //    import repo._
    //    repo.inSession { implicit s =>
    //
    //      val id = 239
    //      val ids = Seq(Some(Id(id)))
    //      val creativesRef = PropertyReference((c: Creative) => c.id).in[Seq](ids)
    //      val creatives = repo.Creatives(SelectMany[Seq[Creative], Creative](creativesRef))
    //      val creative = creatives.head
    //
    //      val creativeRef = IdRef[Creative](creative)
    //      val selection = {
    //        val reference = PropertyReference((_: AppnexusCreativeData).creative).in[Seq](creativeRef)
    //        SelectMany[Seq[AppnexusCreativeData],AppnexusCreativeData](reference)
    //      }
    //
    //      // zapisac appnexusData dla bannera
    //      val appnexusCreativeData = AppnexusCreativeData(Some(Id(id)), creativeRef, Seq())
    //      repo.save(appnexusCreativeData)
    //
    //      // pociagnac appnexusData dla bannera
    //      val seq = repo.AppnexusCreativesData.apply(selection, (_:AppnexusCreativeData).creative)
    //      val appCrData = seq.head
    //      assert(appCrData.categories.toSet == appnexusCreativeData.categories.toSet)
    //      println(appCrData)
    //
    //      // nadpisac appnexusData dla bannera
    //      val appnexusData2 = AppnexusCreativeData(Some(Id(id)), creativeRef, Seq(Category(1)))
    //      repo.save(appnexusData2)
    //
    //      // odczyt appnexusData
    //      val seq2 = repo.AppnexusCreativesData.apply(selection, (_:AppnexusCreativeData).creative)
    //      val appCrData2 = seq2.head
    //
    //      // usunac appnexusData
    //      repo.delete(appnexusCreativeData.copy(id=creative.id))
    //
    //      // odczyt - powinno nie byc danych appnexusowych
    //      val seq3 = repo.AppnexusCreativesData.apply(selection, (_:AppnexusCreativeData).creative)
    //      assert(seq3.fullSize == 0)
    //    }
  }

}

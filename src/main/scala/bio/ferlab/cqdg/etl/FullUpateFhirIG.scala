package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.conf.Conf
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{bundleDelete, updateIG}
import bio.ferlab.cqdg.etl.models.TBundle
import ca.uhn.fhir.rest.api.SummaryEnum
import ca.uhn.fhir.rest.client.api.IGenericClient
import org.hl7.fhir.r4.model._

import scala.jdk.CollectionConverters._

object FullUpateFhirIG extends App {

  val conf = Conf.readConf().map(e => {
    implicit val fhirClient: IGenericClient = buildFhirClient(e.fhir, e.keycloak)


    IG_RESOURCES.foreach(r => {
      deleteIGByResource(r)
    })

    updateIG()
  })

  def deleteIGByResource(resource: String)(implicit fhirClient: IGenericClient): Unit = {
    val total = fhirClient.search().byUrl(s"$resource?_total=accurate")
      .returnBundle(classOf[Bundle])
      .summaryMode(SummaryEnum.TRUE)
      .execute()
      .getTotal

    var se = Seq.empty[Bundle.BundleEntryComponent]
    (0 to total by 20).foreach(e => {
      val bundle = fhirClient.search().byUrl(s"$resource")
        .returnBundle(classOf[Bundle])
        .summaryMode(SummaryEnum.TRUE)
        .offset(e)
        .count(20)
        .execute()
      se = se ++ bundleDelete(bundle.getEntry.asScala.map(_.getResource).toList)
    })
    TBundle(se.toList).delete()
  }

}


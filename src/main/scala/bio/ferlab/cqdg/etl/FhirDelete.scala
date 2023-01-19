package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.conf.Conf
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.fhir.FhirUtils.bundleDelete
import bio.ferlab.cqdg.etl.models.TBundle
import ca.uhn.fhir.rest.api.SummaryEnum
import ca.uhn.fhir.rest.client.api.IGenericClient
import org.hl7.fhir.r4.model.Bundle

import scala.jdk.CollectionConverters._


object FhirDelete extends App {
  val BATCH_SIZE = 20
  val MAX_NUMBER_RESOURCES = 20000

  val Array(version, studyId) = args

  val conf = Conf.readConf().map(e => {
    implicit val fhirClient: IGenericClient = buildFhirClient(e.fhir, e.keycloak)

    val resources = Seq("Patient", "ResearchStudy", "Observation", "Group", "Condition", "Specimen", "Task", "DocumentReference", "Organization")

    val bundleComponentList = resources.flatMap(resourceType => {

      val allEntries = getAllEntries(s"$resourceType?_tag:not=study_version:$version&_tag:exact=study:$studyId", BATCH_SIZE, MAX_NUMBER_RESOURCES)
      LOGGER.info(s"${allEntries.size} $resourceType for study: $studyId, not it version: $version")
      allEntries
    })

    val deleteBundle = bundleDelete(bundleComponentList.map(b => b.getResource)).toList
    val response = TBundle(deleteBundle).delete()

    response
  })

  def getQueryTotal(query: String)(implicit fhirClient: IGenericClient): Int = {
    fhirClient.search().byUrl(s"$query&_total=accurate")
      .returnBundle(classOf[Bundle])
      .summaryMode(SummaryEnum.TRUE)
      .execute()
      .getTotal
  }

  def getEntries(query: String, offset: Int)(implicit fhirClient: IGenericClient) = {
    fhirClient.search().byUrl(s"$query")
      .returnBundle(classOf[Bundle])
      .summaryMode(SummaryEnum.TRUE)
      .offset(offset)
      .count(20)
      .execute()
      .getEntry.asScala.toList
  }

  def getAllEntries(query: String, batchSize: Int, maxSize: Int)(implicit fhirClient: IGenericClient) = {

    var entriesNumber = 0
    var i = 0
    var bundleList = List[Bundle.BundleEntryComponent]()
    do {
      val entries = getEntries(query, i * batchSize)
      entriesNumber = entries.size
      bundleList = bundleList ++ entries
      i += 1
    } while (entriesNumber == batchSize && i * batchSize <= maxSize)
    bundleList
  }
}

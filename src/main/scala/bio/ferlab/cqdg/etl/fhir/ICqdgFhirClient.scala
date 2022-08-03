package bio.ferlab.cqdg.etl.fhir

import ca.uhn.fhir.rest.annotation.{IdParam, Read}
import ca.uhn.fhir.rest.client.api.{IBasicClient, IGenericClient}
import ca.uhn.fhir.rest.param.StringParam
import ca.uhn.fhir.rest.server.exceptions.{PreconditionFailedException, ResourceNotFoundException}
import org.hl7.fhir.r4.model._

import scala.util.Try

//https://github.com/jamesagnew/hapi-fhir/blob/master/hapi-fhir-structures-r4/src/test/java/ca/uhn/fhir/rest/client/ITestClient.java
trait ICqdgFhirClient extends IBasicClient {
//  @Read(`type` = classOf[Patient])
//  def getPatientById(@IdParam id: IdType): Patient
//
//  @Read(`type` = classOf[Practitioner])
//  def getPractitionerById(@IdParam id: IdType): Practitioner
//
//  @Read(`type` = classOf[ServiceRequest])
//  def getServiceRequestById(@IdParam id: IdType): ServiceRequest
//
//  import ca.uhn.fhir.rest.annotation.{RequiredParam, Search}
//  import ca.uhn.fhir.rest.param.TokenParam
//
//  @Search(`type` = classOf[Specimen])
//  def findSpecimenByAccession(@RequiredParam(name = Specimen.SP_ACCESSION) theId: TokenParam): Specimen
//
//  @Search(`type` = classOf[Person])
//  def findPersonByRamq(@RequiredParam(name = Person.SP_IDENTIFIER) theId: TokenParam): Person

}

object ICqdgFhirClient {
  def opt[T](f: => T): Option[T] = {
    Try(Option(f)).recover {
      case _: ResourceNotFoundException => None
    }.get

  }

}

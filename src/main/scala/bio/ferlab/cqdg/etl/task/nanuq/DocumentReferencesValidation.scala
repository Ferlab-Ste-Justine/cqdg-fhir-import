package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.ValidationResult
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.models.nanuq.TDocumentReference.validate
import bio.ferlab.cqdg.etl.models.nanuq._
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.implicits._

object DocumentReferencesValidation {

  def validateFiles(files: Map[String, FileEntry], a: Analysis, studyId: String, release: String)(implicit client: IGenericClient, ferloadConf: FerloadConf): ValidationResult[TDocumentReferences] = {
    (
      validate[SequencingAlignment](files, a, studyId, release),
      validate[VariantCalling](files, a, studyId, release),
      a.files.cnv.map(_ => validate[CopyNumberVariant](files, a, studyId, release)).traverse(identity),
      a.files.sv.map(_ => validate[StructuralVariant](files, a, studyId, release)).traverse(identity),
      a.files.supplement.map(_ => validate[SupplementDocument](files, a, studyId, release)).traverse(identity)
    ).mapN(TDocumentReferences)

  }

}

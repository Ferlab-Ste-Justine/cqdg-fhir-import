package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.ValidationResult
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.models.nanuq.TDocumentReference.validate
import bio.ferlab.cqdg.etl.models.nanuq.{Analysis, CopyNumberVariant, FileEntry, SequencingAlignment, StructuralVariant, SupplementDocument, TDocumentReferences, VariantCalling}
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.implicits._

object DocumentReferencesValidation {

  def validateFiles(files: Map[String, FileEntry], a: Analysis, studyId: String, release: String, dataset: String)(implicit client: IGenericClient, ferloadConf: FerloadConf): ValidationResult[TDocumentReferences] = {

    (
      validate[SequencingAlignment](files, a, studyId, release, dataset),
      validate[VariantCalling](files, a, studyId, release, dataset),
      validate[CopyNumberVariant](files, a, studyId, release, dataset),
      validate[StructuralVariant](files, a, studyId, release, dataset),
      validate[SupplementDocument](files, a, studyId, release, dataset)
      ).mapN(TDocumentReferences)

  }

}

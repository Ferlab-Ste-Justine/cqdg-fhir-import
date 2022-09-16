package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.ValidationResult
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.models.nanuq.TDocumentReference.validate
import bio.ferlab.cqdg.etl.models.nanuq.{Analysis, CopyNumberVariant, FileEntry, SequencingAlignment, StructuralVariant, SupplementDocument, TDocumentReferences, VariantCalling}
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.implicits._

object DocumentReferencesValidation {

  def validateFiles(files: Map[String, FileEntry], a: Analysis)(implicit client: IGenericClient, ferloadConf: FerloadConf): ValidationResult[TDocumentReferences] = {

    (
      validate[SequencingAlignment](files, a),
      validate[VariantCalling](files, a),
      validate[CopyNumberVariant](files, a),
      validate[StructuralVariant](files, a),
      validate[SupplementDocument](files, a)
      ).mapN(TDocumentReferences)

  }

}
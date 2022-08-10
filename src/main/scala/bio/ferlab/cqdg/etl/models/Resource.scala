package bio.ferlab.cqdg.etl.models
 trait Resource {
   val study_id: String

   def getHash: String
 }

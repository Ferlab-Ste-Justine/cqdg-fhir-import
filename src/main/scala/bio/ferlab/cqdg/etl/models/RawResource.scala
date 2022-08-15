package bio.ferlab.cqdg.etl.models
 trait RawResource {
   val study_id: String

   def getHash: String
 }

lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.13.8",
  scalacOptions += "-Ypartial-unification",
  name := "cqdg-fhir-import"
)

val awssdkVersion = "2.16.66"

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    libraryDependencies += "software.amazon.awssdk" % "s3" % awssdkVersion,
    libraryDependencies += "software.amazon.awssdk" % "apache-client" % awssdkVersion,
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.30",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.30",
    libraryDependencies += "ca.uhn.hapi.fhir" % "hapi-fhir-client" % "5.4.2",
    libraryDependencies += "ca.uhn.hapi.fhir" % "hapi-fhir-structures-r4" % "5.4.2",
    libraryDependencies += "ca.uhn.hapi.fhir" % "org.hl7.fhir.r4" % "5.0.0",
  )

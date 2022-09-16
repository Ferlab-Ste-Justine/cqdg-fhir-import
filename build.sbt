lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.13.8",
  name := "cqdg-fhir-import"
)

val awssdkVersion = "2.16.66"

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    libraryDependencies += "software.amazon.awssdk" % "s3" % awssdkVersion,
    libraryDependencies += "software.amazon.awssdk" % "apache-client" % awssdkVersion,
    libraryDependencies += "org.keycloak" % "keycloak-authz-client" % "12.0.3",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    libraryDependencies += "org.testcontainers" % "localstack" % "1.15.2" % Test,
    libraryDependencies += "com.dimafeng" %% "testcontainers-scala-scalatest" % "0.38.8" % Test,
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.30",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.30",
    libraryDependencies += "ca.uhn.hapi.fhir" % "hapi-fhir-client" % "5.4.2",
    libraryDependencies += "ca.uhn.hapi.fhir" % "hapi-fhir-structures-r4" % "5.4.2",
    libraryDependencies += "ca.uhn.hapi.fhir" % "org.hl7.fhir.r4" % "5.0.0",
    libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.17.1",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.1",
    libraryDependencies +=
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  )

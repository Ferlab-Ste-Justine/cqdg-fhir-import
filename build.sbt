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
    libraryDependencies += "ca.uhn.hapi.fhir" % "hapi-fhir-server" % "5.4.2",
    libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.17.1",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.1",
    libraryDependencies += "com.decodified" %% "scala-ssh" % "0.11.1",
    libraryDependencies +=
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  )

excludeDependencies ++= Seq(
  ExclusionRule("commons-logging", "commons-logging"),
  ExclusionRule("org.slf4j", "jcl-over-slf4j"),
)

Test / fork := true
Test / testForkedParallel := false

assembly / assemblyMergeStrategy:= {
  case PathList("META-INF", "mailcap") => MergeStrategy.first
  case PathList("META-INF", xs@_*) => MergeStrategy.discard
  case PathList("module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}



assembly / test := {}
parallelExecution / test := false
assembly / assemblyJarName:= "cqdg-fhir-import.jar"

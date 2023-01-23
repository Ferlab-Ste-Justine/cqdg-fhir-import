FROM openjdk:11

COPY target/scala-2.13/cqdg-fhir-import.jar .

ENTRYPOINT ["java", "-jar", "fhavro-export.jar"]

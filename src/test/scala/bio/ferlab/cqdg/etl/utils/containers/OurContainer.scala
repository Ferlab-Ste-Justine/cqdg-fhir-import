package bio.ferlab.cqdg.etl.utils.containers

import scala.jdk.CollectionConverters._
import com.dimafeng.testcontainers.GenericContainer

trait OurContainer {
  def container: GenericContainer

  private var isStarted = false

  def name: String

  def port: Int

  private var publicPort: Int = -1

  def startIfNotRunning(): (Int, Boolean) = {
    if (isStarted) {
      (publicPort, false)
    } else {
      val runningContainer = container.dockerClient.listContainersCmd().withLabelFilter(Map("name" -> name).asJava).exec().asScala

      val isNew = runningContainer.toList match {
        case Nil =>
          container.start()
          publicPort = container.mappedPort(port)
          true
        case List(c) =>
          publicPort = c.ports.collectFirst { case p if p.getPrivatePort == port => p.getPublicPort }.get
          false
      }
      isStarted = true
      (publicPort, isNew)
    }
  }


}
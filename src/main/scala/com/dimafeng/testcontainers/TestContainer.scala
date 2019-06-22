package com.dimafeng.testcontainers

import java.util.function.Consumer

import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.{CreateContainerCmd, InspectContainerResponse}
import com.github.dockerjava.api.model.{Bind, Info, VolumesFrom}
import org.junit.runner.Description
import org.scalatest._
import org.testcontainers.containers.output.OutputFrame
import org.testcontainers.containers.startupcheck.StartupCheckStrategy
import org.testcontainers.containers.traits.LinkableContainer
import org.testcontainers.containers.{FailureDetectingExternalResource, Network, TestContainerAccessor, GenericContainer => OTCGenericContainer}
import org.testcontainers.containers.{GenericContainer => JavaGenericContainer}
import org.testcontainers.lifecycle.Startable

import scala.collection.JavaConverters._
import scala.concurrent.{Future, blocking}

trait ForEachTestContainer extends SuiteMixin {
  self: Suite =>

  val container: Container

  implicit private val suiteDescription = Description.createSuiteDescription(self.getClass)

  abstract protected override def runTest(testName: String, args: Args): Status = {
    container.starting()
    try {
      afterStart()
      val status = super.runTest(testName, args)
      status match {
        case FailedStatus => container.failed(new RuntimeException(status.toString))
        case _ => container.succeeded()
      }
      status
    }
    catch {
      case e: Throwable =>
        container.failed(e)
        throw e
    }
    finally {
      try {
        beforeStop()
      }
      finally {
        container.finished()
      }
    }
  }

  def afterStart(): Unit = {}

  def beforeStop(): Unit = {}
}

trait ForAllTestContainer extends SuiteMixin {
  self: Suite =>

  val container: Container

  implicit private val suiteDescription = Description.createSuiteDescription(self.getClass)

  abstract override def run(testName: Option[String], args: Args): Status = {
    if (expectedTestCount(args.filter) == 0) {
      new CompositeStatus(Set.empty)
    } else {
      container.starting()
      try {
        afterStart()
        super.run(testName, args)
      } finally {
        try {
          beforeStop()
        }
        finally {
          container.finished()
        }
      }
    }
  }

  def afterStart(): Unit = {}

  def beforeStop(): Unit = {}
}

sealed trait ContainerList {

  def stop(): Unit

  def foreach(f: Container => Unit): Unit = {
    // TODO: test it
    this match {
      case and(head, tail) =>
        head.foreach(f)
        tail.foreach(f)

      case container: Container =>
        f(container)
    }
  }

}
final case class and[C1 <: ContainerList, C2 <: ContainerList](head : C1, tail : C2) extends ContainerList {
  override def stop(): Unit = {
    // TODO: test stopping order
    head.stop()
    tail.stop()
  }
}

object ContainerList {
  implicit class ContainerListOps[T <: ContainerList](val self: T) extends AnyVal {
    def and[T2 <: ContainerList](that: T2): T and T2 = com.dimafeng.testcontainers.and(self, that)
  }
}

trait Container extends ContainerList {
  @deprecated def finished()(implicit description: Description): Unit = stop()

  @deprecated def failed(e: Throwable)(implicit description: Description): Unit = {}

  @deprecated def starting()(implicit description: Description): Unit = start()

  @deprecated def succeeded()(implicit description: Description): Unit = {}

  @deprecated("Please use reflective methods from the wrapper and `configure` method for creation")
  implicit def container: JavaContainer

  type JavaContainer <: Startable

  def underlyingUnsafeContainer: JavaContainer = container

  def stop(): Unit = underlyingUnsafeContainer.stop()

  def start(): Unit = underlyingUnsafeContainer.start()
}

abstract class SingleContainer[T <: OTCGenericContainer[_]] extends Container {

  type JavaContainer = T

  def binds: Seq[Bind] = container.getBinds.asScala

  def command: Seq[String] = container.getCommandParts

  def containerId: String = container.getContainerId

  def containerInfo: InspectContainerResponse = container.getContainerInfo

  def containerIpAddress: String = container.getContainerIpAddress

  def containerName: String = container.getContainerName

  def env: Seq[String] = container.getEnv.asScala

  def exposedPorts: Seq[Int] = container.getExposedPorts.asScala.map(_.intValue())

  def extraHosts: Seq[String] = container.getExtraHosts.asScala

  import scala.concurrent.ExecutionContext.Implicits.global

  def image: Future[String] = Future {
    blocking {
      container.getImage.get()
    }
  }

  @deprecated("See org.testcontainers.containers.Network")
  def linkedContainers: Map[String, LinkableContainer] = container.getLinkedContainers.asScala.toMap

  def mappedPort(port: Int): Int = container.getMappedPort(port)

  def portBindings: Seq[String] = container.getPortBindings.asScala

  def networkMode: String = container.getNetworkMode

  def network: Network = container.getNetwork

  def networkAliases: Seq[String] = container.getNetworkAliases.asScala

  def privilegedMode: Boolean = container.isPrivilegedMode

  def volumesFroms: Seq[VolumesFrom] = container.getVolumesFroms.asScala

  def startupCheckStrategy: StartupCheckStrategy = container.getStartupCheckStrategy

  def startupAttempts: Int = container.getStartupAttempts

  def workingDirectory: String = container.getWorkingDirectory

  def dockerClient: DockerClient = container.getDockerClient

  def dockerDaemonInfo: Info = container.getDockerDaemonInfo

  def logConsumers: Seq[Consumer[OutputFrame]] = container.getLogConsumers.asScala

  def createContainerCmdModifiers: Set[Consumer[CreateContainerCmd]] = container.getCreateContainerCmdModifiers.asScala.toSet

  def configure(configProvider: T => Unit): this.type = {
    configProvider(container)
    this
  }
}

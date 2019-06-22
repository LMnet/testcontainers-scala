package com.dimafeng.testcontainers

import org.testcontainers.containers.{GenericContainer => JavaGenericContainer}

import scala.language.implicitConversions

class MultipleContainers private(containers: Seq[LazyContainer[_]]) extends Container {
  override type JavaContainer = JavaGenericContainer[_]

  override def container: JavaContainer = null

  override def start(): Unit = containers.foreach(_.start())

  override def stop(): Unit = containers.foreach(_.stop())
}

object MultipleContainers {

  /**
    * Creates a `MultipleContainers` instance with nested containers (support 2+ nested containers)
    *  {{{
    *  val pgContainer = PostgreSQLContainer()
    *  val mySqlContainer = MySQLContainer()
    *  val seleniumContainer = SeleniumContainer()
    *
    *  val containers = MultipleContainers(pgContainer, mySqlContainer, seleniumContainer)
    *  }}}
    *
    * In case of dependent containers you need to define this containers explicitly with `lazy val`,
    * and after that pass them to the `MultipleContainers`:
    *  {{{
    *  lazy val pgContainer = PostgreSQLContainer()
    *  lazy val appContainer = AppContainer(pgContainer.jdbcUrl, pgContainer.username, pgContainer.password)
    *
    *  val containers = MultipleContainers(pgContainer, appContainer)
    *  }}}
    */
  def apply(containers: LazyContainer[_]*): MultipleContainers = new MultipleContainers(containers)
}

/**
  * Lazy container wrapper aims to solve the problem of cross-container dependencies in `MultipleContainers` when a second container
  * requires some after start data from a first one (e.g. an application container needs JDBC url of a container with a database - in that case
  * the url becomes available after the database container has started)
  *
  * You don't need to wrap your containers into the `LazyContainer` manually
  * when you pass your containers in the `MultipleContainers`- there is implicit conversion for that.
  */
class LazyContainer[T <: Container](factory: => T) extends Container {
  override type JavaContainer = T#JavaContainer

  override lazy val container: JavaContainer = factory.container
}

object LazyContainer {

  implicit def containerToLazyContainer[T <: Container](container: => T): LazyContainer[T] = LazyContainer(container)

  def apply[T <: Container](factory: => T): LazyContainer[T] = new LazyContainer(factory)
}

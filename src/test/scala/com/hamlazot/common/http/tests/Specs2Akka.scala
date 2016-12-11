package com.hamlazot.common.http.tests

/**
 * @author yoav @since 11/30/16.
 */

import akka.http.scaladsl.server.RoutingLog
import akka.http.scaladsl.settings.RoutingSettings
import akka.http.scaladsl.testkit.{TestFrameworkInterface, RouteTest, RouteTestTimeout}
import org.specs2.execute.{Failure, FailureException}
import org.specs2.mutable.Specification

import scala.concurrent.duration._

trait Specs2Akka
  extends Specification
  with RouteTest
  with Specs2Interface{

  implicit def routeTestTimeout: RouteTestTimeout = RouteTestTimeout(FiniteDuration(
    60, "seconds"))

  protected implicit lazy val routingSettings = RoutingSettings.apply(system)
  protected implicit lazy val routingLog = RoutingLog.fromActorSystem(system)

}

trait Specs2Interface extends TestFrameworkInterface {

  def failTest(msg: String): Nothing = {
    val trace = new Exception().getStackTrace.toList
    val fixedTrace = trace.drop(trace.indexWhere(_.getClassName.startsWith("org.specs2")) - 1)
    throw new FailureException(Failure(msg, stackTrace = fixedTrace))
  }
}
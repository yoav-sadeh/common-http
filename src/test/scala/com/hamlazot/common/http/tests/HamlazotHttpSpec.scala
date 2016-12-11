package com.hamlazot.common.http.tests

import akka.http.scaladsl.server.RouteConcatenation
import akka.http.scaladsl.server.directives.{MethodDirectives, MiscDirectives, PathDirectives, RouteDirectives}

/**
 * @author yoav @since 12/11/16.
 */
trait HamlazotHttpSpec
  extends Specs2Akka
  with RouteDirectives
  with MethodDirectives
  with PathDirectives
  with MiscDirectives
  with RouteConcatenation

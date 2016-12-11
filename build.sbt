name := "common-http"

organization := "com.hamlazot"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

val akkaVersion = "2.4.10"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-experimental" % akkaVersion,
  "com.hamlazot" %% "common-macros" % "0.1.0-SNAPSHOT",
  "org.specs2" %% "specs2-core" % "3.8.6" % Test,
  "com.typesafe.akka" % "akka-http-testkit_2.11" % "2.4.10" % Test
)
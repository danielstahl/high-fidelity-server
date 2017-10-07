name := "high-fidelity-server"

version := "1.0"

scalaVersion := "2.12.2"

lazy val akkaVersion = "2.4.17"
lazy val akkaHttpVersion = "10.0.6"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "ch.megard" %% "akka-http-cors" % "0.2.1",
  "com.pauldijou" %% "jwt-core" % "0.12.1",
  "com.google.firebase" % "firebase-admin" % "5.2.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "net.ruippeixotog" %% "scala-scraper" % "2.0.0"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)
dockerBaseImage := "java:openjdk-8-jre"
dockerExposedPorts := Seq(8080)

import com.typesafe.sbt.packager.docker.Cmd

dockerCommands := dockerCommands.value.flatMap{
  case cmd@Cmd("FROM",_) => List(cmd,Cmd("RUN", "apt-get -y update && apt-get -y install docker.io"))
  case other => List(other)
}

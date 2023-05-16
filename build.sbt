import java.util.Calendar

ThisBuild / scalaVersion := "3.2.2"
ThisBuild / version := "1.3.0"
ThisBuild / organization := "com.stulsoft"
ThisBuild / organizationName := "stulsoft"

lazy val root = (project in file("."))
  .settings(
    name := "life",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.6",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",

    libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.15" % Test
  )
  .enablePlugins(JavaAppPackaging)

Compile / packageBin / packageOptions += Package.ManifestAttributes("Build-Date" -> Calendar.getInstance().getTime.toString)

Compile / mainClass := Some("com.stulsoft.life.Main")
name := """ch02"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies += "net.sf.barcode4j" % "barcode4j" % "2.0"

// play.Project.playScalaSettings

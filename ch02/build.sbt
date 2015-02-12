name := """ch02"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"
)

libraryDependencies += "net.sf.barcode4j" % "barcode4j" % "2.0"

libraryDependencies += "com.versant" % "versantjpa" % "2.0.15.2878"

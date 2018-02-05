import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.sagebear",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "BigRussianBoss",
    libraryDependencies += "com.github.javafaker" % "javafaker" % "0.14",
    libraryDependencies += "com.typesafe" % "config" % "1.3.2",
    libraryDependencies += scalaTest % Test
  )

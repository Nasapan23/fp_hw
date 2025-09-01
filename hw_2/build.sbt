ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test

lazy val root = (project in file("."))
  .settings(
    name := "h2-word-tree"
  )

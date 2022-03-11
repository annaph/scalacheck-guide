ThisBuild / organization := "org.scalacheck.guide"

ThisBuild / description := "ScalaCheck The Definitive guide"

ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := "2.13.8"


ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-deprecation",
  "-feature",
  "-unchecked"
)

ThisBuild / fork := true

lazy val root = project.in(file("."))
  .settings(name := "scalacheck-guide")

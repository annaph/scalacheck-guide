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

ThisBuild / libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.15.4"
)

lazy val root = project.in(file("."))
  .settings(name := "scalacheck-guide")
  .aggregate(
    foundation,
    fundamentals,
    designingProperties,
    properties,
    generators
  )

lazy val foundation = project.in(file("foundation"))
  .settings(
    libraryDependencies += "org.junit.vintage" % "junit-vintage-engine" % "5.8.2" % Test
  )

lazy val fundamentals = project.in(file("fundamentals"))

lazy val designingProperties = project.in(file("designing-properties"))

lazy val properties = project.in(file("properties"))

lazy val generators = project.in(file("generators"))

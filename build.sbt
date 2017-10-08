import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "fi.kapsi.kosmik",
      scalaVersion := "2.12.1",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "Scala for the Impatient Exercises",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % Test
    )
)

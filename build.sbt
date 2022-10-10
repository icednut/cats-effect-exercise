ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "cats-effect-exercise",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.typelevel" %% "cats-effect" % "3.3.14",

      "org.scalatest" %% "scalatest" % "3.2.14" % Test,
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.4.0" % Test
    )
  )

scalaVersion := "2.12.7"
name := "minesweeper"
version := "1.0"

lazy val minesweeper = (project in file("."))
  .settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
  )
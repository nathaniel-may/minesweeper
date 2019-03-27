scalaVersion := "2.12.8"
name := "minesweeper"
version := "1.0"

lazy val minesweeper = (project in file("."))
  .settings(
    resolvers += "jitpack" at "https://jitpack.io",

    libraryDependencies += "org.scalaz"               %% "scalaz-core"        % "7.2.26",
    libraryDependencies += "com.github.nathaniel-may" %  "functional-shuffle" % "1.0.0",
    libraryDependencies += "org.scalatest"            %% "scalatest"          % "3.0.5"  % "test",
    libraryDependencies += "org.scalacheck"           %% "scalacheck"         % "1.14.0" % "test",
  )
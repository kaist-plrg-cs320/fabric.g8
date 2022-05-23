name := "fabric"

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation", "-Xlint:unused")

lazy val macros = (project in file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val core = (project in file("core")).settings(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % Test,
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  libraryDependencies += "org.jline" % "jline" % "3.21.0",
  libraryDependencies += "org.jline" % "jline-terminal-jna" % "3.21.0"
).dependsOn(macros)

run := (core / Compile / run).evaluated
test := (core / Test / test).value

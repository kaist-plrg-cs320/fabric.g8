name := "fabric"

ThisBuild / scalaVersion := "2.13.16"
ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation", "-Xlint:unused")

lazy val macros = (project in file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val core = (project in file("core")).settings(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % Test,
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  libraryDependencies += "org.jline" % "jline" % "3.21.0",
  libraryDependencies += "org.jline" % "jline-terminal-jna" % "3.21.0",

  wartremoverClasspaths += "file://" + baseDirectory.value + "/../lib/cs320warts.jar",
  wartremoverErrors ++= Seq(
    Wart.AsInstanceOf, Wart.IsInstanceOf, Wart.Null, Wart.Return, Wart.Throw, Wart.Var, Wart.While,
    Wart.custom("cs320warts.MutableDataStructures"), Wart.custom("cs320warts.TryCatch")
  ),
  wartremoverExcluded ++= {
    lazy val dir = baseDirectory.value / "src" / "main" / "scala"
    lazy val sdir = dir / "cs320"
    Seq(
      dir / "package.scala", sdir / "Fuzzer.scala", sdir / "Main.scala", sdir / "Typed.scala",
      baseDirectory.value / "src" / "test" / "scala" / "cs320" / "Spec.scala"
    )
  }
).dependsOn(macros)

run := (core / Compile / run).evaluated
test := (core / Test / test).value

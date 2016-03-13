import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

val globalSettings = scalariformSettings ++ Seq(
  organization := "lingua",
  scalaVersion := "2.11.8",
  licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalacOptions ++= Seq("-feature", "-deprecation"),
  ScalariformKeys.preferences := {
    ScalariformKeys.preferences.value
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  })

lazy val root = project.in(file(".")).aggregate(core, fst, lexikon)

lazy val fst = project.in(file("fst"))
  .settings(globalSettings: _*)
  .settings(
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scodec" %% "scodec-core" % "1.7.1",
    name := "lingua-fst")

lazy val core = project.in(file("core"))
  .settings(globalSettings: _*)
  .settings(
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.5",
    name := "lingua-core")

lazy val lexikon = project.in(file("lexikon"))
  .settings(globalSettings: _*)
  .settings(
    version := "0.1.0-SNAPSHOT",
    name := "lingua-lexikon")
  .dependsOn(core, fst)

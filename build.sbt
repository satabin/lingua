import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

val globalSettings = Seq(
  organization := "lingua",
  scalaVersion := "2.11.6",
  licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalacOptions ++= Seq("-feature", "-deprecation"))

lazy val root = project.in(file("."))
  .settings(globalSettings: _*)
  .settings(scalariformSettings: _*)
  .settings(ScalariformKeys.preferences := {
    ScalariformKeys.preferences.value
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  },
  version := "0.1.0-SNAPSHOT",
  libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.5",
  libraryDependencies += "org.scodec" %% "scodec-core" % "1.7.1",
  name := "lingua")

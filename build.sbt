import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

val globalSettings = Seq(
  organization := "org.gnieh",
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
  libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0",
  libraryDependencies += "org.scodec" %% "scodec-core" % "1.7.1",
  name := "korrekt")

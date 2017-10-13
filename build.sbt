import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

val globalSettings = scalariformSettings ++ Seq(
  organization := "lingua",
  scalaVersion := "2.12.3",
  resolvers +=
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked"),
  scalacOptions in (Compile, doc) ++= Seq("-groups"),
  ScalariformKeys.preferences := {
    ScalariformKeys.preferences.value
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  })

lazy val root = project.in(file("."))
  .settings(globalSettings: _*)
  .settings(unidocSettings: _*)
  .aggregate(core, fst, lexikon)

lazy val fst = project.in(file("fst"))
  .settings(globalSettings: _*)
  .settings(
    version := "0.1.0-SNAPSHOT",
    name := "lingua-fst")

lazy val core = project.in(file("core"))
  .settings(globalSettings: _*)
  .settings(
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.4",
    name := "lingua-core")

lazy val lexikonDependencies = Seq(
  "com.github.scopt" %% "scopt" % "3.7.0",
  "com.github.pathikrit" %% "better-files" % "3.1.0",
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.gnieh" %% "tekstlib" % "0.2.0-SNAPSHOT")

lazy val lexikon = project.in(file("lexikon"))
  .enablePlugins(BuildInfoPlugin)
  .settings(globalSettings: _*)
  .settings(
    version := "0.1.0-SNAPSHOT",
    name := "lingua-lexikon",
    libraryDependencies ++= lexikonDependencies,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "lingua.lexikon")
  .dependsOn(core, fst)

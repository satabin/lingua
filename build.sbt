import scalariform.formatter.preferences._

enablePlugins(PublishingPlugin)

val globalSettings = Seq(
  organization := "org.gnieh",
  scalaVersion := "2.12.4",
  version := "0.2.0-SNAPSHOT",
  resolvers +=
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("https://gnieh.org/satabin/lingua")),
  scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Ypartial-unification"),
  scalacOptions in (Compile, doc) ++= Seq("-groups"),
  scalariformAutoformat := true,
  scalariformPreferences := {
    scalariformPreferences.value
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentConstructorArguments, true)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  }) ++ publishSettings

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  // The Nexus repo we're publishing to.
  publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
  ),
  pomIncludeRepository := { x => false },
  pomExtra := (
    <scm>
      <url>https://github.com/satabin/lingua</url>
      <connection>scm:git:git://github.com/satabin/lingua.git</connection>
      <developerConnection>scm:git:git@github.com:satabin/lingua.git</developerConnection>
      <tag>HEAD</tag>
    </scm>
    <developers>
      <developer>
        <id>satabin</id>
        <name>Lucas Satabin</name>
        <email>lucas.satabin@gnieh.org</email>
      </developer>
    </developers>
    <ciManagement>
      <system>travis</system>
      <url>https://travis-ci.org/#!/satabin/lingua</url>
    </ciManagement>
    <issueManagement>
      <system>github</system>
      <url>https://github.com/satabin/lingua/issues</url>
    </issueManagement>
  )
)
lazy val root = project.in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .settings(globalSettings: _*)
  .settings(
    publishArtifact := false)
  .aggregate(core, cascade, fst, lexikon)

lazy val fst = project.in(file("fst"))
  .settings(globalSettings: _*)
  .settings(
    name := "lingua-fst",
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.4" cross CrossVersion.binary),
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1")
  .dependsOn(cascade)

lazy val cascade = project.in(file("cascade"))
  .settings(globalSettings)
  .settings(
    name := "lingua-cascade")

lazy val core = project.in(file("core"))
  .settings(globalSettings: _*)
  .settings(
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0",
    publishArtifact := false,
    name := "lingua-core")

lazy val lexikonDependencies = Seq(
  "com.github.scopt" %% "scopt" % "3.7.0",
  "com.github.pathikrit" %% "better-files" % "3.4.0",
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.gnieh" %% "tekstlib" % "0.2.0-SNAPSHOT")

lazy val lexikon = project.in(file("lexikon"))
  .enablePlugins(BuildInfoPlugin)
  .settings(globalSettings: _*)
  .settings(
    name := "lingua-lexikon",
    publishArtifact := false,
    libraryDependencies ++= lexikonDependencies,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "lingua.lexikon")
  .dependsOn(core, fst)

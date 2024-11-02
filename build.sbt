lazy val sv      = "3.4.3"
lazy val svMinor = sv.split('.').drop(1).head

ThisBuild / organization         := "org.felher"
ThisBuild / organizationName     := "Felix Herrmann"
ThisBuild / version              := "0.0.2"
ThisBuild / publish / skip       := true
ThisBuild / organizationHomepage := Some(url("https://felher.org"))
ThisBuild / scalaVersion         := sv

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/felher/s3te/"),
    "scm:git@github.com:felher/s3te.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id = "felher",
    name = "Felix Herrmann",
    email = "felix@herrmann-koenigsberg.de",
    url = url("https://felher.org")
  )
)

ThisBuild / description       := "Scala 3 Tree Explorer - lets you explore the Scala 3 reflect module AST."
ThisBuild / licenses          := List(sbt.librarymanagement.License.MIT)
ThisBuild / homepage          := Some(url("https://github.com/felher/s3te/"))
ThisBuild / publishTo         := sonatypePublishToBundle.value
ThisBuild / publishMavenStyle := true

val compilerSettings = scalacOptions ++= Seq(
  "-language:strictEquality",
  "-source:future",
  "-feature",
  "-deprecation",
  "-Xkind-projector:underscores",
  "-Xmax-inlines:256",
  "-Wall"
)

lazy val s3teCompile = project
  .in(file("s3te-compile"))
  .settings(
    name           := "s3te-compile",
    publish / skip := false,
    usePgpKeyHex("DE132E3B66E5239F490F52AB3DA07E9E7CFDB415"),
    crossVersion   := CrossVersion.binaryWith("", "." + svMinor),
    compilerSettings,
    Compile / unmanagedSourceDirectories += baseDirectory.value / ".." / "s3te-shared" / "src" / "main" / "scala"
  )

lazy val s3teUi = project
  .in(file("s3te-ui"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name                            := "s3te-ui",
    compilerSettings,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom"   % "2.8.0",
      "com.raquo"    %%% "laminar"       % "17.1.0",
      "org.felher"   %%% "beminar"       % "1.1.0",
      "io.circe"     %%% "circe-core"    % "0.14.10",
      "io.circe"     %%% "circe-generic" % "0.14.10",
      "io.circe"     %%% "circe-parser"  % "0.14.10"
    ),
    scalaJSUseMainModuleInitializer := true,
    Compile / unmanagedSourceDirectories += baseDirectory.value / ".." / "s3te-shared" / "src" / "main" / "scala"
  )

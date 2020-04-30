import Dependencies._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val supportedScalaVersions = List("2.12.11", "2.13.1")

ThisBuild / organization := "dev.scarisey"
ThisBuild / homepage := None
ThisBuild / licenses += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")
ThisBuild / developers := List(
  Developer("scarisey", "Sylvain Carisey", "sylvain@carisey.dev", url("https://github.com/scarisey"))
)
ThisBuild / startYear := Some(2020)
ThisBuild / dynverSeparator := "-"
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/scarisey/bastion" + name.value),
    "scm:git:git@github.com/scarisey/bastion" + name.value + ".git"
  )
)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

lazy val createProductHelper = taskKey[Unit]("Generate code for ProductHelper.scala")

createProductHelper := {
  val productBuilderFile =
    (sourceDirectory in core).value / "main" / "scala" / "dev" / "scarisey" / "bastion" / "ProductHelper.scala"
  val resource = (resourceManaged in Compile).value / "scalaFmt" / "temporary"
  val scalaFmt = baseDirectory.value / ".scalafmt.conf"

  CodeGen.replaceFileSection(
    productBuilderFile,
    "producthelper",
    ProductHelperCodeGen.generateProductHelpers,
    resource,
    scalaFmt
  )
}

lazy val createResultHelper = taskKey[Unit]("Generate code for ResultHelper.scala")

createResultHelper := {
  val file =
    (sourceDirectory in core).value / "main" / "scala" / "dev" / "scarisey" / "bastion" / "ResultHelper.scala"
  val resource = (resourceManaged in Compile).value / "scalaFmt" / "temporary"
  val scalaFmt = baseDirectory.value / ".scalafmt.conf"

  CodeGen.replaceFileSection(
    file,
    "resulthelper",
    ResultHelperCodeGen.generateResultHelpers,
    resource,
    scalaFmt
  )
}

lazy val core = (project in file("core"))
  .settings(moduleName := "bastion-core")
  .settings(buildSettings)
  .settings(
    libraryDependencies ++= Seq(
      magnolia,
      mercator,
      scalaTest % Test
    )
  )

lazy val examples = (project in file("examples"))
  .settings(moduleName := "bastion-examples")
  .settings(buildSettings)
  .settings(publish / skip := true)
  .dependsOn(core)

lazy val buildSettings = Seq(
  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds,implicitConversions",
    "-deprecation",
    "-unchecked",
    "-opt:l:inline",
    "-opt-inline-from:dev.carisey.converter.**",
    "-Ywarn-unused",
    "-Yrangepos"
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 => Seq("-Ypartial-unification")
      case _                       => Nil
    }
  },
  addCompilerPlugin(scalafixSemanticdb),
  crossScalaVersions := supportedScalaVersions
)

lazy val root = (project in file("."))
  .aggregate(core,examples)
  .settings(crossScalaVersions := Nil, publish / skip := true)

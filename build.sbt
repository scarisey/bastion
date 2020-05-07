import Dependencies._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val supportedScalaVersions = List("2.12.11", "2.13.1")

ThisBuild / organization := "dev.scarisey"
ThisBuild / homepage := Some(url("https://bastion.scarisey.dev"))
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

lazy val createDynamicReprTuples = taskKey[Unit]("Generate code for DynamicReprTuples.scala")

createDynamicReprTuples := {
  val dynamicReprTuplesFile =
    (sourceDirectory in core).value / "main" / "scala" / "bastion" / "DynamicReprTuples.scala"
  val resource = (resourceManaged in Compile).value / "scalaFmt" / "temporary"
  val scalaFmt = baseDirectory.value / ".scalafmt.conf"

  CodeGen.replaceFileSection(
    dynamicReprTuplesFile,
    "producthelper",
    DynamicReprTuplesCodeGen.generate,
    resource,
    scalaFmt
  )
}

lazy val createResultProducts = taskKey[Unit]("Generate code for ResultProducts.scala")

createResultProducts := {
  val file =
    (sourceDirectory in core).value / "main" / "scala" / "bastion" / "ResultProducts.scala"
  val resource = (resourceManaged in Compile).value / "scalaFmt" / "temporary"
  val scalaFmt = baseDirectory.value / ".scalafmt.conf"

  CodeGen.replaceFileSection(
    file,
    "resulthelper",
    ResultProductsCodeGen.generate,
    resource,
    scalaFmt
  )
}

lazy val tryBuild = taskKey[Unit]("Build like it should on CI")

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
  .settings(tryBuild := {
    (headerCheckAll in Compile).value
    scalafmtCheckAll.value
    (compile in Compile).value
    unusedCompileDependenciesTest.value
    (test in Test).value
  })

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

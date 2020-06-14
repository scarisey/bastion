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
    url("https://github.com/scarisey/bastion"),
    "scm:git:git@github.com/scarisey/bastion.git"
  )
)

lazy val createDecodingStateTuples = taskKey[Unit]("Generate code for DecodingStateTuples.scala")

createDecodingStateTuples := {
  val decodingStateTuplesFile =
    (sourceDirectory in core).value / "main" / "scala" / "bastion" / "DecodingStateTuples.scala"
  val resource = (resourceManaged in Compile).value / "scalaFmt" / "temporary"
  val scalaFmt = baseDirectory.value / ".scalafmt.conf"

  CodeGen.replaceFileSection(
    decodingStateTuplesFile,
    "producthelper",
    DecodingStateTuplesCodeGen.generate,
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
lazy val tryBuildImpl = Def.task {
  (headerCheckAll in Compile).value
  scalafmtCheckAll.value
  (compile in Compile).value
  unusedCompileDependenciesTest.value
  (test in Test).value
}

lazy val core = (project in file("core"))
  .settings(moduleName := "bastion-core")
  .settings(buildSettings)
  .settings(tryBuild := tryBuildImpl.value)
  .settings(
    libraryDependencies ++= Seq(
      magnolia,
      mercator,
      scalaTest % Test
    )
  )

lazy val ujsonModule = (project in file("ujson"))
  .settings(moduleName := "bastion-ujson")
  .settings(buildSettings)
  .settings(tryBuild := tryBuildImpl.value)
  .settings(
    libraryDependencies ++= Seq(
      ujson,
      scalaTest % Test
    )
  )
  .dependsOn(core)

lazy val examples = (project in file("examples"))
  .settings(moduleName := "bastion-examples")
  .settings(buildSettings)
  .settings(tryBuild := tryBuildImpl.value)
  .settings(publish / skip := true)
  .dependsOn(core, ujsonModule)

lazy val benchmark = (project in file("benchmark"))
  .settings(moduleName := "bastion-benchmark")
  .settings(buildSettings)
  .settings(libraryDependencies ++= circe :+ circeMagnolia)
  .settings(publish / skip := true)
  .dependsOn(core, ujsonModule)
  .enablePlugins(JmhPlugin)

lazy val buildSettings = Seq(
  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds,implicitConversions",
    "-deprecation",
    "-unchecked",
    "-opt:l:inline",
    "-opt-inline-from:bastion.**",
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
  .aggregate(core, examples, ujsonModule)
  .settings(crossScalaVersions := Nil, publish / skip := true)

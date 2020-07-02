import sbt._

object Dependencies {
  /* core */
  lazy val scalaTest = "org.scalatest"  %% "scalatest" % "3.1.1"
  lazy val magnolia  = "com.propensive" %% "magnolia"  % "0.15.0"

  /* transitive deps explicitly declared */
  lazy val mercator = "com.propensive" %% "mercator" % "0.2.1"

  /* ujson */
  lazy val ujson = "com.lihaoyi" %% "ujson" % "1.1.0"

  //BENCHMARK DEPENDENCIES

  /* circe */
  lazy val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser",
    "io.circe" %% "circe-optics"
  ).map(_ % "0.13.0")

  lazy val circeMagnolia = "io.circe" %% "circe-magnolia-derivation" % "0.6.1"

  /* upickle */

  lazy val upickle = "com.lihaoyi" %% "upickle" % "1.1.0"

  /* Jackson */

  lazy val jackson = "com.fasterxml.jackson.core" % "jackson-databind" % "2.11.1"
}

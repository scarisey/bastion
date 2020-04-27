import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest"  %% "scalatest" % "3.1.1"
  lazy val magnolia  = "com.propensive" %% "magnolia"  % "0.15.0"

  /* transitive deps explicitly declared */
  lazy val mercator     = "com.propensive" %% "mercator"     % "0.2.1"
}

import sbt._
import Keys._

object Deps {
  val scalatags = "com.lihaoyi"       %% "scalatags"    % "0.12.0"
  val tinfour   = "org.tinfour"        % "TinfourCore"  % "2.1.7"
  val orTools   = "com.google.ortools" % "ortools-java" % "9.5.2237"

  val circe = Seq("circe-core", "circe-parser").map("io.circe" %% _ % "0.14.4")
  val cats  = "org.typelevel" %% "cats-core" % "2.9.0"

  val scalatest = Seq("scalatest", "scalatest-flatspec").map("org.scalatest" %% _ % "3.2.15" % "test")

  val jackson = "com.fasterxml.jackson.core" % "jackson-databind" % "2.12.0"
  val batik   = "batik"                      % "batik-svggen"     % "1.6-1"
}

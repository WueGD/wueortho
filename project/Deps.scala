import sbt._
import Keys._

object Deps {
  val scalatags = "com.lihaoyi"       %% "scalatags"    % "0.12.0"
  val tinfour   = "org.tinfour"        % "TinfourCore"  % "2.1.7"
  val orTools   = "com.google.ortools" % "ortools-java" % "9.5.2237"

  val circe = Seq("circe-core", "circe-parser").map("io.circe" %% _ % "0.14.4")
}
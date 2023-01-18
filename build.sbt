scalaVersion := "3.2.1"

libraryDependencies ++= Seq(
  "com.lihaoyi"       %% "scalatags"    % "0.12.0",
  "org.tinfour"        % "TinfourCore"  % "2.1.7",
  "com.google.ortools" % "ortools-java" % "9.5.2237",
)

scalacOptions ++= Seq("-Yexplicit-nulls", "-language:strict-equality", "-deprecation", "-Wconf:any:verbose")

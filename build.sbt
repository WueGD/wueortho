scalaVersion := "3.2.1"

libraryDependencies ++= Seq(
  "com.lihaoyi"       %% "scalatags"            % "0.12.0",
  "org.tinfour"        % "TinfourCore"          % "2.1.7",
  "com.google.ortools" % "ortools-java"         % "9.5.2237",
  "com.breinify"       % "brein-time-utilities" % "1.8.0",
)

scalacOptions ++= Seq("-Yexplicit-nulls", "-language:strict-equality", "-deprecation", "-Wconf:any:verbose")

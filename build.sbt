scalaVersion := "3.2.0"

libraryDependencies ++= Seq(
  "com.lihaoyi"       %% "scalatags"            % "0.11.1",
  "org.tinfour"        % "TinfourCore"          % "2.1.7",
  "com.google.ortools" % "ortools-java"         % "9.4.1874",
  "com.breinify"       % "brein-time-utilities" % "1.8.0",
)

scalacOptions ++= Seq("-Yexplicit-nulls", "-language:strict-equality", "-deprecation", "-Wconf:any:verbose")

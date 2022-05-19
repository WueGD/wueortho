scalaVersion := "3.1.2"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "scalatags" % "0.11.1",
  "org.tinfour" % "TinfourCore" % "2.1.7")

scalacOptions ++= Seq("-Yexplicit-nulls", "-language:strict-equality", "-deprecation")

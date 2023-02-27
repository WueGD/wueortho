import Deps._

ThisBuild / scalaVersion := "3.2.2"
ThisBuild / organization := "de.uniwue.info1"
ThisBuild / version      := "0.1.0"
ThisBuild / scalacOptions ++= compilerOptions

lazy val core = project
  .settings(
    name := "wueortho-core",
  )

lazy val io = project
  .settings(
    name := "wueortho-io",
    libraryDependencies ++= (scalatags +: circe),
  )
  .dependsOn(core)

lazy val layout = project
  .settings(
    name := "wueortho-layout",
    libraryDependencies ++= Seq(tinfour, orTools),
  )
  .dependsOn(core)

lazy val pipeline = project
  .settings(
    name := "wueortho-pipeline",
    libraryDependencies ++= circe,
  )
  .dependsOn(core, io, layout)

lazy val root = (project in file(".")).dependsOn(core, io, layout, pipeline)

lazy val compilerOptions = Seq(
  "-source:future",
  "-release:17",
  "-Yexplicit-nulls",
  "-language:strictEquality",
  "-deprecation",
  "-feature",
  "-Wconf:any:verbose",
)

enablePlugins(JavaAppPackaging)

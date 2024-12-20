// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

import Deps._

ThisBuild / scalaVersion := "3.5.2"
ThisBuild / organization := "de.wueortho"
ThisBuild / version      := "0.1.2"
ThisBuild / scalacOptions ++= compilerOptions

lazy val core = project.settings(
  name := "wueortho-core",
)

lazy val io = project.settings(
  name := "wueortho-io",
  libraryDependencies ++= (scalatags +: cats +: circe),
).dependsOn(core)

lazy val layout = project.settings(
  name := "wueortho-layout",
  libraryDependencies ++= tinfour +: orTools +: scalatest,
).dependsOn(core)

lazy val pipeline = project.settings(
  name := "wueortho-pipeline",
  libraryDependencies ++= circe ++ scalatest,
).dependsOn(core, io, layout)

lazy val praline = project.settings(
  name := "wueortho-praline",
  libraryDependencies ++= jackson +: batik +: scalatest,
  javacOptions ++= Seq("-source", "17"),
).dependsOn(pipeline)

lazy val root = (project in file(".")).settings(publish / skip := true).aggregate(core, io, layout, pipeline, praline)
  .dependsOn(pipeline, praline)

lazy val compilerOptions = Seq(
  "-source:future",
  "-release:17",
  "-Xmax-inlines:256",
  "-Yexplicit-nulls",
  "-Wsafe-init",
  "-language:strictEquality",
  "-deprecation",
  "-feature",
  "-Wconf:any:verbose",
  "-Wunused:all",
  "-Wvalue-discard",
)

enablePlugins(JavaAppPackaging)

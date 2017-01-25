
name := "weblvcscala"

version := (version in ThisBuild).value

organization := "com.github.workingDog"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8", "2.12.1")

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.5.11",
  "com.typesafe.play.extras" %% "play-geojson" % "1.4.0")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

homepage := Some(url("https://github.com/workingDog/weblvcscala"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

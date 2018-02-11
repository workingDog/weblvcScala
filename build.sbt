
name := "weblvcscala"

version := (version in ThisBuild).value

organization := "com.github.workingDog"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.6.8",
  "au.id.jazzy" %% "play-geojson" % "1.5.0")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

homepage := Some(url("https://github.com/workingDog/weblvcscala"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

assemblyMergeStrategy in assembly := {
  case PathList(xs@_*) if xs.last.toLowerCase endsWith ".dsa" => MergeStrategy.discard
  case PathList(xs@_*) if xs.last.toLowerCase endsWith ".sf" => MergeStrategy.discard
  case PathList(xs@_*) if xs.last.toLowerCase endsWith ".des" => MergeStrategy.discard
  case PathList(xs@_*) if xs.last endsWith "LICENSES.txt" => MergeStrategy.discard
  case PathList(xs@_*) if xs.last endsWith "LICENSE.txt" => MergeStrategy.discard
  case PathList("META-INF", xs@_*) => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

homepage := Some(url("https://github.com/workingDog/weblvcScala"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

//mainClass in(Compile, run) := Some("com.kodekutters.weblvcScala")

//mainClass in assembly := Some("com.kodekutters.weblvcScala")

assemblyJarName in assembly := "weblvcScala-" + version.value + ".jar"


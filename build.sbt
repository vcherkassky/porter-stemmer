scalaVersion := "2.10.3"

EclipseKeys.withSource := true

mainClass in (Compile, run) := Some("linguistica.stemmer.porter.Test")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.13"


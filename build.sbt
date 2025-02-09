name := "learn-fp"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalacOptions ++= Seq("-language:_")

// debug implicits helper
// scalacOptions in ThisBuild += "-Xlog-implicits"


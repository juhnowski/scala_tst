name := "lms-lite"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.1"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.10.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

// tests are not thread safe
parallelExecution in Test := false

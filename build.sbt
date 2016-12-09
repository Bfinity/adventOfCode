name := "hello"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"

scalacOptions ++= Seq("-deprecation")
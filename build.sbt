//import play.Project._

name := "scala-x86-gen"

version := "0.1-SNAPSHOT"

organization := "com.scalaAsm"

scalaVersion := "2.11.5"

//playScalaSettings

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"

lazy val scalax86 = project.in(file("."))

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil
//import play.Project._

name := "scala-x86-gen"

version := "0.1-SNAPSHOT"

organization := "com.scalaAsm"

scalaVersion := "2.11.2"

//playScalaSettings

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11+"

lazy val scalax86 = project.in(file("."))
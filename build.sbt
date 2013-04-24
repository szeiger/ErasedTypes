organization := "com.novocode"

name := "erased-types"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

//scalacOptions ++= Seq("-deprecation", "-unchecked", "-uniqid", "-Xprint:typer")

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M4" % "test"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "compile")

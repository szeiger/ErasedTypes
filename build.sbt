organization := "com.novocode"

name := "erased-types"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-Ydependent-method-types", "-unchecked")

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test"

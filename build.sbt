name := "Type_Directed"

ThisBuild / scalaVersion := "2.13.1"

ThisBuild / organization := "com.td"

val scalaTest = "org.scalatest" %% "scalatest" % "3.1.0"

libraryDependencies ++= Seq(
  scalaTest % Test,
  "org.typelevel" %% "cats-core" % "2.1.1"
  )


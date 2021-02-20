name := "Type_Directed"

ThisBuild / scalaVersion := "2.13.1"

ThisBuild / organization := "com.td"

val scalaTest = "org.scalatest" %% "scalatest" % "3.2.2"

libraryDependencies ++= Seq(
  scalaTest % Test,
  "org.typelevel" %% "cats-core" % "2.1.1"
  )


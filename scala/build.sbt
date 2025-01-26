name := "heavybool"

version := "0.1"

scalaVersion := "2.13.14"

libraryDependencies += "junit" % "junit" % "4.13.2" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation"
)

Test / parallelExecution := false



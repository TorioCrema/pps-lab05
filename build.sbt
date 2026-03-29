val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "pps-lab05",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test
  )

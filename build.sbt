val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "pfds-dotty",
    version := "0.1.0",

    scalaVersion := dottyVersion,
  )

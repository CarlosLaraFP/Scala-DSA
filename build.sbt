ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "3.3.0",
  "org.apache.spark" %% "spark-sql" % "3.3.0",
  "org.scalatest" % "scalatest_2.13" % "3.2.14" % Test
)

lazy val root = (project in file("."))
  .settings(
    name := "DSA"
  )


val sharedSettings = Seq(
  organization := "com.gigapayroll",
  version := "0.1.0",

  scalaVersion := "2.12.8",

  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "utf-8",
    "-explaintypes",
    "-feature",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-unchecked",
    "-Xfatal-warnings",
    "-Ypartial-unification",
    "-Ywarn-dead-code",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused:implicits",
    "-Ywarn-unused:imports",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:params",
    "-Ywarn-unused:patvars",
    "-Ywarn-unused:privates",
    "-Ywarn-value-discard"
  )
)

val testSettings = Seq(
  scalacOptions in Test ++= Seq("-Yrangepos"),

  libraryDependencies ++= Seq(
    "org.scalatest"  %% "scalatest"  % "3.0.5"  % "test",

    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "crony"
  )
  .settings(sharedSettings)
  .settings(testSettings)

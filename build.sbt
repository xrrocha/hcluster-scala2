name := "playground"

version := "0.1"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-spellchecker" % "3.6.2",
  //"com.rockymadden.stringmetric" %% "stringmetric-core" % "0.27.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.6.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.jfree" % "jfreechart" % "1.0.19" % "test",
  "org.scalatest" % "scalatest_2.12" % "3.2.0-SNAP8" % "test",
  "com.lihaoyi" % "ammonite" % "1.0.0-RC9" % "test" cross CrossVersion.full
)

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

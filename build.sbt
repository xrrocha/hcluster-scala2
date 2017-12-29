name := "playground"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-spellchecker" % "3.6.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.jfree" % "jfreechart" % "1.5.0" % "test",
  "org.scalatest" % "scalatest_2.12" % "3.2.0-SNAP9" % "test",
  "com.lihaoyi" % "ammonite" % "1.0.3" % "test" cross CrossVersion.full
)

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

import sbt._

class ScardfProject(info: ProjectInfo) extends DefaultProject(info)
{
  val jena = "com.hp.hpl.jena" % "jena" % "2.6.3"
  val arq = "com.hp.hpl.jena" % "arq" % "2.8.7"
  val jodatime = "joda-time" % "joda-time" % "1.6"
  val commonslogging = "commons-logging" % "commons-logging" % "1.1.1"

  val junit = "junit" % "junit" % "4.7" % "test"
  val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5" % "test"
}

// vim: set ts=2 sw=2 et:

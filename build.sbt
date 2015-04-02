name := "scardf"

organization := "com.github.scardf" //adding this so published artifacts won't have groupID = 'default'

version := "0.6-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers++= Seq("Maven2 Central Repository" at "http://repo1.maven.org/maven2",
                 "Scala-tools Maven2 Repository" at "http://scala-tools.org/repo-releases",
                 "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
                 "wp5.e-taxonomy.eu" at "http://wp5.e-taxonomy.eu/cdmlib/mavenrepo/")

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.apache.jena" % "apache-jena-libs" % "2.13.0" excludeAll(ExclusionRule("log4j","log4j"),ExclusionRule("org.slf4j","slf4j-log4j12")),
  "joda-time" % "joda-time" % "2.7",
  "org.joda" % "joda-convert" % "1.7",
  "org.slf4j" % "jcl-over-slf4j" % "1.7.10",
  "org.slf4j" % "jul-to-slf4j" % "1.7.10",
  "org.slf4j" % "log4j-over-slf4j" % "1.7.10",
  "org.slf4j" % "slf4j-api" % "1.7.10",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "org.codehaus.groovy" % "groovy" % "2.4.2" % "test", //used to let tests use test/resources/logback.groovy
  "junit" % "junit" % "4.11",
  "org.specs2" %% "specs2" % "3.2" % "test",
  "org.scalatest" %% "scalatest" % "2.2.3" % "test"
)

libraryDependencies <++= (scalaVersion){v =>
  if(v.startsWith("2.11")) Seq(
    "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3")
  else Seq()
}

net.virtualvoid.sbt.graph.Plugin.graphSettings

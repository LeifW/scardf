name := "scardf"

version := "0.8.1-SNAPSHOT"

scalaVersion := "2.10.4"

resolvers ++= Seq("Maven2 Central Repository" at "http://repo1.maven.org/maven2",
 	"Scala-tools Maven2 Repository" at "http://scala-tools.org/repo-releases")


libraryDependencies ++= Seq(
  "org.apache.jena" % "apache-jena-libs" % "2.11.2",
	"joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.6",
  "org.slf4j" % "jcl-over-slf4j" % "1.7.7",
  "org.slf4j" % "jul-to-slf4j" % "1.7.7",
  "org.slf4j" % "log4j-over-slf4j" % "1.7.7",
  "org.slf4j" % "slf4j-api" % "1.7.7",
	"junit" % "junit" % "4.8.2",
	"org.specs2" %% "specs2" % "2.3.12" % "test")
name := "scardf"

version := "0.6-SNAPSHOT"

scalaVersion := "2.11.2"

resolvers++= Seq("Maven2 Central Repository" at "http://repo1.maven.org/maven2",
 	"Scala-tools Maven2 Repository" at "http://scala-tools.org/repo-releases",
 	"wp5.e-taxonomy.eu" at "http://wp5.e-taxonomy.eu/cdmlib/mavenrepo/")


libraryDependencies ++= Seq("com.hp.hpl.jena" % "jena" % "2.6.3",
	 "com.hp.hpl.jena" % "arq" % "2.8.4",
	 "joda-time" % "joda-time" % "1.6",
	 "commons-logging" % "commons-logging" % "1.1.1",
	 "junit" % "junit" % "4.7",
	 "org.specs2" %% "specs2" % "2.3.11" % "test")


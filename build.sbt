organization := "net.noresttherein"

name := "oldsql"

version := "pleistocen"

scalaVersion := "2.11.12"

fork in Compile := true

javaOptions in Compile ++= Seq("-Xmx4G")


testOptions in Test ++= Seq(Tests.Filter(s => !s.endsWith("Props")))


resolvers ++= Seq(
	"Spray repository" at "http://repo.spray.io",
	"Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
	"Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
)



libraryDependencies ++= Seq(
	"org.postgresql" % "postgresql" % "9.2-1004-jdbc41",
	"joda-time" % "joda-time" % "2.8.1",

	"com.typesafe.play" %% "play-json" % "2.3.7",	
	"com.typesafe.slick" %% "slick" % "2.1.0",
	"com.typesafe.akka" %% "akka-actor" % "2.3.6",
	"com.typesafe.akka" %% "akka-slf4j" % "2.3.6",
	"io.spray" %% "spray-can" % "1.3.1",
	"io.spray" %% "spray-http" % "1.3.1",
	"io.spray" %% "spray-httpx" % "1.3.1",
	"io.spray" %% "spray-json" % "1.3.1",
	"io.spray" %% "spray-routing" % "1.3.1",
	"org.scala-lang" % "scala-library" % "2.11.12",
	"org.scala-lang" % "scala-reflect" % "2.11.12",
	"org.scalatest" %% "scalatest" % "2.2.4" % "test",
	"org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
	//"org.mockito" % "mockito-all" 
)


scalacOptions ++= Seq(
	//	"-Ylog-classpath",
	//	"-Xlog-implicits",
	"-Xexperimental",
	"-feature",
	"-deprecation",
	"-language:postfixOps",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



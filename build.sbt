organization := "net.noresttherein"

name := "oldsql"

version := "pleistocen"

scalaVersion := "2.13.1"

fork in Compile := true

javaOptions in Compile ++= Seq("-Xmx4G")


testOptions in Test ++= Seq(Tests.Filter(s => !s.endsWith("Props")))


resolvers ++= Seq(
	"Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
	"Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"
)



libraryDependencies ++= Seq(
	"org.postgresql" % "postgresql" % "9.2-1004-jdbc41",
	"net.bytebuddy" % "byte-buddy" % "1.10.8",
//	"cglib" % "cglib-nodep" % "3.3.0",
	"org.scala-lang" % "scala-library" % "2.13.1",
	"org.scala-lang" % "scala-reflect" % "2.13.1",
	"org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % "test",
	"org.scalacheck" %% "scalacheck" % "1.14.3" % "test"
	//"org.mockito" % "mockito-all" 
)


scalacOptions ++= Seq(
//	"-Ylog-classpath",
//	"-Xlog-implicits",
	"-feature",
	"-deprecation",
//	"-language:postfixOps",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



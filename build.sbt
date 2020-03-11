organization := "net.noresttherein"

name := "oldsql"

version := "pleistocen"

scalaVersion := "2.11.12"

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
//	"-language:postfixOps",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



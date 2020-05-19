organization := "net.noresttherein"

name := "oldsql"

version := "pleistocen"

scalaVersion := "2.13.2"

fork in Compile := true

javaOptions in Compile ++= Seq("-Xmx4G")


testOptions in Test ++= Seq(Tests.Filter(s => !s.endsWith("Props")))


resolvers ++= Seq(
	"Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
	"Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"
)



libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-library" % "2.13.2",
	"org.scala-lang" % "scala-reflect" % "2.13.2",
	"net.bytebuddy" % "byte-buddy" % "1.10.8",
	"org.postgresql" % "postgresql" % "9.2-1004-jdbc41",
	"org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % "test",
	"org.scalacheck" %% "scalacheck" % "1.14.3" % "test"
)


scalacOptions ++= Seq(
//	"-Ylog-classpath",
//	"-Xlog-implicits",
	"-Wconf:cat=deprecation&msg=\\(foldRight instead\\|foldLeft instead\\):silent",
	"-feature",
	"-deprecation",
//	"-language:postfixOps",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



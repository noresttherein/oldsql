organization := "net.noresttherein"

name := "oldsql"

version := "Imoen"

scalaVersion := "2.13.4"

fork in Compile := true

javaOptions in Compile ++= Seq("-Xmx4G")


testOptions in Test ++= Seq(Tests.Filter(s => !s.endsWith("Props")))


resolvers ++= Seq(
	"Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/",
	"Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"
)



libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-library" % "2.13.4",
	"org.scala-lang" % "scala-reflect" % "2.13.4",
	"net.bytebuddy" % "byte-buddy" % "1.10.8",
//	"org.postgresql" % "postgresql" % "9.2-1004-jdbc41",
//	"org.hibernate" % "hibernate-core" % "5.4.24.Final",
	"org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % "test",
	"org.scalacheck" %% "scalacheck" % "1.14.3" % "test"
)


scalacOptions ++= Seq(
//	"-Ylog-classpath",
	"-Xlog-implicits",
	"-Wconf:cat=deprecation&msg=not a part of the public API:silent," +
		"cat=deprecation&msg=foldLeft:silent,cat=deprecation&msg=foldRight:silent," +
		"cat=deprecation:w,cat=feature:w",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



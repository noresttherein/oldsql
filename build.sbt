organization := "net.noresttherein"

name := "oldsql"

version := "Imoen"

scalaVersion := "2.13.10-patch"

Compile / fork := true

Compile / javaOptions ++= Seq("-Xmx4G")


Test / testOptions ++= Seq(Tests.Filter(s => !s.endsWith("Props")))



libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-compiler" % "2.13.10-patch",
	"org.scala-lang" % "scala-library" % "2.13.10-patch",
	"org.scala-lang" % "scala-reflect" % "2.13.10-patch",
	"net.bytebuddy" % "byte-buddy" % "1.13.0",
//	"org.postgresql" % "postgresql" % "9.2-1004-jdbc41",
//	"org.hibernate" % "hibernate-core" % "5.4.24.Final",
	"org.scalatest" %% "scalatest" % "3.2.15" % "test",
	"org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
)

//javaOptions ++= Seq(
//	"-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=127.0.0.1:2501"
////	"-Xbootclasspath/a:/home/turin/scala/patches/lib/ConcurrentModificationExceptionPatch.jar"
//)

scalacOptions ++= Seq(
//	"-J-Xss8m",
//	"-verbose",
//	"-Ydebug", //<- always throws a NullPointerException
//	"-Ylog:0-100",
//	"-Ytyper-debug", //<- reports an error in a place which compiles without it
//	"-Ylog:extmethods",
//	"-Ycheck:extmethods",
//	"-Ylog-classpath",
	"-Vimplicits",
	"-Vimplicits-verbose-tree",
//	"-Xprint:0-100",
//	"-Xprint:extmethods",
//	"-Xdev",
//	"-Vtype-diffs",
//	"-Vprint-types",
	"-Xlog-reflective-calls",
	"-Xlog-implicits",
//	"-Jagentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=127.0.0.1:2501",
//	"-W",
//	"-JXbootclasspath/a:/home/turin/scala/patches/lib/ConcurrentModificationExceptionPatch.jar",
//	"-bootclasspath:/home/turin/scala/patches/lib/ConcurrentModificationExceptionPatch.jar",
	"-Wunused:patvars,privates,locals",
	"""-Wconf:msg=export:silent,cat=other-match-analysis&msg=Singleton():silent,cat=deprecation&msg=foldLeft:silent,cat=deprecation&msg=foldRight:silent,cat=deprecation:w,cat=feature:w""",
	"-Xlint:delayedinit-select,implicit-not-found,option-implicit,poly-implicit-overload,nullary-unit",
	"-feature",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



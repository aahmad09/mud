name := "MUD"
version := "1.0"
scalaVersion := "2.12.12"

run / fork := true
run / connectInput := true

mainClass in (Compile, packageBin) := Some("mud.Main")

mainClass in (Compile, run) := Some("mud.Main")

libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor" % "2.6.8",
	"com.typesafe.akka" %% "akka-testkit" % "2.6.8" % Test,
	"org.scala-lang.modules" %% "scala-xml" % "1.3.0",
	"com.novocode" % "junit-interface" % "0.11" % Test,
	"org.scalactic" %% "scalactic" % "3.2.2",
	"org.scalatest" %% "scalatest" % "3.2.2" % "test"
)


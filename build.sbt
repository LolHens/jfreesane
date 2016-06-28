name := "jfreesane"

lazy val settings = Seq(
  version := "0.0.0",

  scalaVersion := "2.11.8",

  resolvers ++= Seq(
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
    Resolver.jcenterRepo
  ),

  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % "2.11.8",
    "org.scala-lang" % "scala-reflect" % "2.11.8",
    "com.typesafe.akka" %% "akka-actor" % "2.4.3",
    "com.typesafe.akka" %% "akka-remote" % "2.4.3",
    "com.chuusai" %% "shapeless" % "2.3.0",
    "com.google.guava" % "guava" % "18.0",
    "junit" % "junit" % "4.8.2",
    "com.google.truth" % "truth" % "0.24"
  ),

  mainClass in Compile := Some(""),

  scalacOptions ++= Seq("-Xmax-classfile-name", "254")
)

lazy val root = Project("jfreesane", file("."))
  .enablePlugins(
    JavaAppPackaging,
    UniversalPlugin)
  .settings(settings: _*)
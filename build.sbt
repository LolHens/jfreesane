name := "jfreesane"

lazy val settings = Seq(
  version := "0.0.0",

  scalaVersion := "2.11.8",

  resolvers ++= Seq(
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
    Resolver.jcenterRepo
  ),

  classpathTypes += "maven-plugin",

  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.2",
    "com.github.mpilquist" %% "simulacrum" % "0.10.0",
    "com.google.guava" % "guava" % "21.0",
    "junit" % "junit" % "4.12",
    "com.google.truth" % "truth" % "0.31"
  ),

  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  //addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0"),

  fork in run := true,

  mainClass in Compile := Some(""),

  dependencyUpdatesExclusions := moduleFilter(organization = "org.scala-lang"),

  scalacOptions ++= Seq("-Xmax-classfile-name", "254")
)

lazy val root = Project("jfreesane", file("."))
  .enablePlugins(
    JavaAppPackaging,
    UniversalPlugin)
  .settings(settings: _*)
import sbt._
import Keys._
import Process._

object SpecializeBuild extends Build {

  val scala = "2.10.1-SNAPSHOT"

  val defaults = Defaults.defaultSettings ++ Seq(
    // scala version + resolver
    scalaVersion := scala,
    scalaBinaryVersion := "2.10",
    resolvers in ThisBuild += ScalaToolsSnapshots,
    // paths
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "test"),
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),
    // sbteclipse needs some info on source directories:
    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_)),
    // add the library, reflect and the compiler as libraries
    libraryDependencies <<= scalaVersion(ver => Seq(
      "org.scala-lang" % "scala-library" % ver,
      "org.scala-lang" % "scala-reflect" % ver, 
      "org.scala-lang" % "scala-compiler" % ver,
      "com.github.axel22" %% "scalameter" % "0.2"
    )),
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false,
    //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
    com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,
    // debugging
    scalacOptions ++= Seq("-uniqid", "-Yshow-syms")
  )

  // we might need this later
  // val testSettings = Seq(libraryDependencies ++= sMeter, testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"))

  lazy val _specialize  = Project(id = "specialize",       base = file(".")) aggregate (_spec_base, _spec_test, _spec_bench)
  lazy val _spec_base   = Project(id = "specialize-base",  base = file("components/base"), settings = defaults)
  lazy val _spec_test   = Project(id = "specialize-test",  base = file("components/test"), settings = defaults) dependsOn(_spec_base)
  lazy val _spec_bench  = Project(id = "specialize-bench", base = file("components/bench"), settings = defaults) dependsOn(_spec_base)
}

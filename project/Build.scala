import sbt._
import Keys._
import Process._
import sbtassembly.Plugin._
import AssemblyKeys._

object SpecializeBuild extends Build {

  val scala = "2.10.2-SNAPSHOT"

  // http://stackoverflow.com/questions/6506377/how-to-get-list-of-dependency-jars-from-an-sbt-0-10-0-project
  val getJars = TaskKey[Unit]("get-jars")
  val getJarsTask = getJars <<= (target, fullClasspath in Runtime) map { (target, cp) =>
    println("Target path is: "+target)
    println("Full classpath is: "+cp.map(_.data).mkString(":"))
  }

  val defaults = Defaults.defaultSettings ++ assemblySettings ++ Seq(
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
      //"org.scala-lang" % "scala-reflect" % ver, 
      "org.scala-lang" % "scala-compiler" % ver,
      "org.scala-lang" % "scala-partest" % ver,
      "com.novocode" % "junit-interface" % "0.10-M2" % "test"
    )),
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
    //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
    com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,
    // debugging
    // scalacOptions ++= Seq("-uniqid")
    // don't run tests before assembly:
    test in assembly := {}
  )

  val benchDeps = Seq(
    libraryDependencies += "com.github.axel22" %% "scalameter" % "0.3",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false
  )

  // Running tests requires that we put scala-library in the bootclasspath, 
  // else the compiler mirror won't pick up the dependencies. So we'll need
  // to fork compile-run and pass the scala-library in the bootclasspath 
  // instead of the normal classlpath. See this thread for a discussion:
  // https://groups.google.com/forum/#!msg/simple-build-tool/9OTd1DPNFqk/3RM-AXWhxosJ
  def isScalaLib(file: java.io.File) = (file.getName() startsWith "scala-") && (file.getName() endsWith ".jar")
  val compilerDeps = Seq(
    getJarsTask,
    fork in Test := true,
    javaOptions in Test <+= (dependencyClasspath in Runtime) map { path =>
      val cp = "-Xbootclasspath/a:"+path.map(_.data).filter(isScalaLib).mkString(":")
      println(cp)
      cp
    },
    fullClasspath in Runtime ~= { path => println("classpath: " + path); path }
    //fullClasspath in Runtime ~= { path => path.filterNot(x => isScalaLib(x.data)) }
  )

  // we might need this later
  // val testSettings = Seq(libraryDependencies ++= sMeter, testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"))

  lazy val _specialize  = Project(id = "specialize",       base = file(".")) aggregate (_spec_base, _spec_test, _spec_bench)
  lazy val _spec_base   = Project(id = "specialize-base",  base = file("components/base"), settings = defaults)
  lazy val _spec_test   = Project(id = "specialize-test",  base = file("components/test"), settings = defaults ++ compilerDeps) dependsOn(_spec_base)
  lazy val _spec_bench  = Project(id = "specialize-bench", base = file("components/bench"), settings = defaults ++ benchDeps) dependsOn(_spec_base)
}

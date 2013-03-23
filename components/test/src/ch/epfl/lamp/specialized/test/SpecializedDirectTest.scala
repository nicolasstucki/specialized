package ch.epfl.lamp.specialized.test

import scala.tools.partest._
import scala.tools.nsc._
import nest.FileUtil._
import scala.reflect.io._
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.io.{ File => JFile }
import java.io.PrintWriter

case class SpecializedDirectTest(val code: String, val flags: String) extends DirectTest {
   lazy val cwd = sys.props.get("user.dir").getOrElse(".")

   override lazy val testPath = File(new JFile(f"${cwd}/componets/test")) // File(new JFile(f"${cwd}/components/base/target/scala-2.10/classes")) //File(new JFile(System.getProperty("java.io.tmpdir")))
   override lazy val testOutput = Directory(new JFile(f"${cwd}/components/test/testOutput")) //Directory(new JFile(System.getProperty("java.io.tmpdir")))

   override def extraSettings = "-classpath 'components/base/target/scala-2.10/classes/' " + flags

   // http://stackoverflow.com/questions/8708342/redirect-console-output-to-string-in-java
   private[this] lazy val ba = new ByteArrayOutputStream();

   def show() = {
      // this is the interesting part :)
      val pa = new PrintStream(ba)
      val pOut = Console.out
      val pErr = Console.err
      Console.setOut(pa)
      Console.setErr(pa)
      compile()
      pa.flush()
      Console.setOut(pOut)
      Console.setErr(pErr)
   }

   def output = ba.toString()
}


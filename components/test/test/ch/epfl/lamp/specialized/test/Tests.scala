package ch.epfl.lamp.specialized.test

import org.junit.Test
import java.io.{ File => JFile }
import scala.tools.nsc.io._
import scala.tools.partest.nest.FileUtil._

class Tests {

   @Test
   def test = {
      // for each file in resources/ do:
      val cwd = sys.props.get("user.dir").getOrElse(".")
      val res = File(new JFile(cwd)) / "components" / "test" / "res"
      System.err.println("res: " + res)

      def new_extension(source: JFile, ext: String) = source.toString.replaceAll("\\.scala", "." + ext)

      var crash = false

      def sources(dir: JFile): Seq[JFile] = {
         val sourcesInDir = for (source <- dir.listFiles() if source.getName().endsWith(".scala")) yield source

         val sourcesInSubDir = for (source <- dir.listFiles() if source.isDirectory) yield sources(source)

         sourcesInDir.toSeq ++ sourcesInSubDir.toList.flatten
      }

      for (source <- sources(res.jfile)) {
         // source code:
         val code = File(source).slurp
         val flags = File(new_extension(source, "flags")).slurp
         val check_output = File(new_extension(source, "check")).slurp
         val test = SpecializedDirectTest(code, flags)
         test.show()
         val output = test.output
         val diff = compareContents(output.split("\n"), check_output.split("\n"))
         if (!diff.isEmpty()) {
            System.err.println("\n\n\nDifference in test for: " + source)
            System.err.println("\nCompiler output:\n" + output)
            System.err.println("\nExpected output:\n" + check_output)
            System.err.println("\nDiff:\n" + diff)
            crash = true
         }
      }

      assert(!crash, "Tests failed")
   }
}

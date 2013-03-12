package ch.epfl.specialize.benchmark
import org.scalameter.api._
import ch.epfl.specialize.banchmark.tests._
import org.scalameter.CurveData
import org.scalameter.Parameters

object RangeBenchmark
      extends PerformanceTest {

   // PerformanceTest defs
   // TODO: Not sure this is supposed to be transient, but it complains class is not serializable due to it
   @transient lazy val executor = SeparateJvmsExecutor(
      Executor.Warmer.Default(),
      Aggregator.complete(Aggregator.average),
      new Executor.Measurer.Default)
   def persistor = Persistor.None
   def reporter = new LoggingReporter {
      var count = 0
      var lastParams = ""
      override def report(result: CurveData, persistor: Persistor) {
         var output = ""

         if (lastParams != result.measurements.head.params.toString) {
            output += "\n"
            count = 0
            lastParams = result.measurements.head.params.toString
         }

         if (count == 0) {
            val t1 = "test"
            val t2 = "testUnroled"
            val t3 = "testSpecialized"
            output += f"${result.measurements.head.params.axisData.head}%25s"
            output += f"${t1}%20s"
            output += f"${t2}%20s"
            output += f"${t3}%20s\n"
         }

         if (count % 3 == 0) {
            val n = result.context.scope.length
            output += f"${result.context.scope.substring(n - 3, n)}%24s:"
         }

         output += f"${result.measurements.head.time}% 20f"

         print(output)

         if (count % 3 == 2) {
            println
         }
         count += 1
      }
   }

   for (n <- Seq(200000, 500000, 1000000)) {
      bench("Test1", "Int", Gen.range("Test1[Int]")(n, n, 1).map(new Test1[Int](_)))
      bench("Test1", "Double", Gen.range("Test1[Double]")(n, n, 1).map(new Test1[Double](_)))
      bench("Test1", "Boolean", Gen.range("Test1[Boolean]")(n, n, 1).map(new Test1[Boolean](_)))
      bench("Test1", "Any", Gen.range("Test1[Any]")(n, n, 1).map(new Test1[Any](_)))

      bench("Test2", "Int", Gen.range("Test2[Int]")(n, n, 1).map(new Test2[Int](_)))
      bench("Test2", "Double", Gen.range("Test2[Double]")(n, n, 1).map(new Test2[Double](_)))
      bench("Test2", "Boolean", Gen.range("Test2[Boolean]")(n, n, 1).map(new Test2[Boolean](_)))
      bench("Test2", "Any", Gen.range("Test2[Any]")(n, n, 1).map(new Test2[Any](_)))

      bench("Test3", "Int", Gen.range("Test3[Int]")(n, n, 1).map(new Test3[Int](_)))
      bench("Test3", "Double", Gen.range("Test3[Double]")(n, n, 1).map(new Test3[Double](_)))
      bench("Test3", "Boolean", Gen.range("Test3[Boolean]")(n, n, 1).map(new Test3[Boolean](_)))
      bench("Test3", "Any", Gen.range("Test3[Any]")(n, n, 1).map(new Test3[Any](_)))

      bench("Test4", "Int", Gen.range("Test4[Int]")(n, n, 1).map(new Test4[Int](_)(7, (x: Int) => x + 1)))
      bench("Test4", "Double", Gen.range("Test4[Double]")(n, n, 1).map(new Test4[Double](_)(2.3, (x: Double) => x + 1.0d)))
      bench("Test4", "Boolean", Gen.range("Test4[Boolean]")(n, n, 1).map(new Test4[Boolean](_)(false, (x: Boolean) => !x)))
      bench("Test4", "Any", Gen.range("Test4[Any]")(n, n, 1).map(new Test4[Any](_)("f", (x: Any) => x)))

      bench("Test5", "Int", Gen.range("Test5[Int]")(n, n, 1).map(new Test5[Int](_)((x: Int) => x)))
      bench("Test5", "Double", Gen.range("Test5[Double]")(n, n, 1).map(new Test5[Double](_)((x: Double) => x)))
      bench("Test5", "Boolean", Gen.range("Test5[Boolean]")(n, n, 1).map(new Test5[Boolean](_)((x: Boolean) => x)))
      bench("Test5", "Any", Gen.range("Test5[Any]")(n, n, 1).map(new Test5[Any](_)((x: Any) => x)))
   }

   def bench(name: String, tpe: String, test: Gen[TestApi]): Unit = {
      val interpFlags = ("int", "-Xint")
      val c1Flags = ("c1", "-XX:-TieredCompilation -XX:CompileThreshold=1 -client") // -XX:+PrintCompilation")
      val c2Flags = ("c2", "-XX:-TieredCompilation -XX:CompileThreshold=1 -server") // -XX:+PrintCompilation")
      val samples = 1
      val minWarmupRuns = 1000

      for ((flagtype, flags) <- List(interpFlags, c1Flags, c2Flags)) {

         // The three measures are needed for the formating of the reporter
         measure method "%s[%s] %s".format(name, tpe, flagtype) in {
            using(test) curve ("Range") config (exec.jvmflags -> flags, exec.minWarmupRuns -> minWarmupRuns, exec.independentSamples -> samples) in {
               _.test
            }
         }

         measure method "%s[%s] %s".format(name, tpe, flagtype) in {
            using(test) curve ("Range") config (exec.jvmflags -> flags, exec.minWarmupRuns -> minWarmupRuns, exec.independentSamples -> samples) in {
               _.testUnrolled
            }
         }

         measure method "%s[%s] %s".format(name, tpe, flagtype) in {
            using(test) curve ("Range") config (exec.jvmflags -> flags, exec.minWarmupRuns -> minWarmupRuns, exec.independentSamples -> samples) in {
               _.testSpecialized
            }
         }
      }
   }
}

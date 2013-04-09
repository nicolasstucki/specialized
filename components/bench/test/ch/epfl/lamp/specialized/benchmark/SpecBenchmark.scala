package ch.epfl.lamp.specialized.benchmark
import org.scalameter.api._
import ch.epfl.lamp.specialized.benchmark.tests._
import org.scalameter.CurveData
import org.scalameter.Parameters

object RangeBenchmark
      extends PerformanceTest {

   // PerformanceTest defs
   // TODO: Not sure this is supposed to be transient, but it complains class is not serializable due to it
//   @transient lazy val executor = SeparateJvmsExecutor(
//      Executor.Warmer.Default(),
//      Aggregator.complete(Aggregator.average),
//      new Executor.Measurer.Default)
   def executor = new org.scalameter.execution.LocalExecutor(
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
            val t3 = "test@Specialized"
            val t4 = "testSpecialized[T]"
            output += f"  ${result.measurements.head.params.axisData.head._1} -> ${result.measurements.head.params.axisData.head._2}\n"
            output += f"${""}%6s"
            output += f"${t1}%20s"
            output += f"${t2}%20s"
            output += f"${t3}%20s"
            output += f"${t4}%20s\n"
         }

         if (count % 4 == 0) {
            val n = result.context.scope.length
            output += f"${result.context.scope}%5s:"
         }

         output += f"${result.measurements.head.time}% 20f"

         print(output)

         if (count % 4 == 3) {
            println
         }
         count += 1
      }
   }

   for (n <- Seq(500000 /*200000, 500000, 1000000, 2000000*/ )) {
      bench(Gen.single("TestArrayReverse[Int]")(n).map(new BenchmarkArrayReverse[Int](_)))
      bench(Gen.single("BenchmarkArrayReverse[Double]")(n).map(new BenchmarkArrayReverse[Double](_)))
      bench(Gen.single("TestArrayReverse[Boolean]")(n).map(new BenchmarkArrayReverse[Boolean](_)))
      bench(Gen.single("TestArrayReverse[Any]")(n).map(new BenchmarkArrayReverse[Any](_)))

      bench(Gen.single("TestArrayDuplicate[Int]")(n).map(new TestArrayDuplicate[Int](_)))
      bench(Gen.single("TestArrayDuplicate[Double]")(n).map(new TestArrayDuplicate[Double](_)))
      bench(Gen.single("TestArrayDuplicate[Boolean]")(n).map(new TestArrayDuplicate[Boolean](_)))
      bench(Gen.single("TestArrayDuplicate[Any]")(n).map(new TestArrayDuplicate[Any](_)))

      bench(Gen.single("TestFunctionApplyNTimesRec[Int]")(n).map(new TestFunctionApplyNTimesRec[Int](_)(7, (x: Int) => x * 5)))
      bench(Gen.single("TestFunctionApplyNTimesRec[Double]")(n).map(new TestFunctionApplyNTimesRec[Double](_)(2.3, (x: Double) => x * 2.3d)))
      bench(Gen.single("TestFunctionApplyNTimesRec[Boolean]")(n).map(new TestFunctionApplyNTimesRec[Boolean](_)(false, (x: Boolean) => !x)))
      bench(Gen.single("TestFunctionApplyNTimesRec[Any]")(n).map(new TestFunctionApplyNTimesRec[Any](_)("f", (x: Any) => x)))

      bench(Gen.single("TestFunction2ApplyNTimesRec[Int]")(n).map(new TestFunction2ApplyNTimesRec[Int](_)(0, 1, (x: Int, y: Int) => x + y)))
      bench(Gen.single("TestFunction2ApplyNTimesRec[Double]")(n).map(new TestFunction2ApplyNTimesRec[Double](_)(0.0, 1.0, (x: Double, y: Double) => x + y)))
      bench(Gen.single("TestFunction2ApplyNTimesRec[Boolean]")(n).map(new TestFunction2ApplyNTimesRec[Boolean](_)(false, true, (x: Boolean, y: Boolean) => x && y)))
      bench(Gen.single("TestFunction2ApplyNTimesRec[Any]")(n).map(new TestFunction2ApplyNTimesRec[Any](_)("f", "g", (x: Any, y: Any) => x)))

      bench(Gen.single("TestArrayMapOverFunction[Int]")(n).map(new TestArrayMapOverFunction[Int](_)((x: Int) => x)))
      bench(Gen.single("TestArrayMapOverFunction[Double]")(n).map(new TestArrayMapOverFunction[Double](_)((x: Double) => x)))
      bench(Gen.single("TestArrayMapOverFunction[Boolean]")(n).map(new TestArrayMapOverFunction[Boolean](_)((x: Boolean) => x)))
      bench(Gen.single("TestArrayMapOverFunction[Any]")(n).map(new TestArrayMapOverFunction[Any](_)((x: Any) => x)))

      bench(Gen.single("TestFunctionApplyNTimesRecOverTuples[Int]")(n).map(new TestFunctionApplyNTimesRecOverTuples[Int](_)((7, 4), (x: Int) => x * 5)))
      bench(Gen.single("TestFunctionApplyNTimesRecOverTuples[Double]")(n).map(new TestFunctionApplyNTimesRecOverTuples[Double](_)((2.3, 3.5), (x: Double) => x * 2.3d)))
      bench(Gen.single("TestFunctionApplyNTimesRecOverTuples[Boolean]")(n).map(new TestFunctionApplyNTimesRecOverTuples[Boolean](_)((false, true), (x: Boolean) => !x)))
      bench(Gen.single("TestFunctionApplyNTimesRecOverTuples[Any]")(n).map(new TestFunctionApplyNTimesRecOverTuples[Any](_)(("f", BigInt("3")), (x: Any) => x.hashCode)))

      bench(Gen.single("TestArrayOfTuplesSwap[Int]")(n).map(new TestArrayOfTuplesSwap[Int](_)))
      bench(Gen.single("TestArrayOfTuplesSwap[Double]")(n).map(new TestArrayOfTuplesSwap[Double](_)))
      bench(Gen.single("TestArrayOfTuplesSwap[Boolean]")(n).map(new TestArrayOfTuplesSwap[Boolean](_)))
      bench(Gen.single("TestArrayOfTuplesSwap[Any]")(n).map(new TestArrayOfTuplesSwap[Any](_)))
   }

   def bench(test: Gen[TestApi]): Unit = {
      val debugFlags = "-XX:+PrintInlining -XX:-TieredCompilation"
      val interpFlags = ("int", "-Xint")
      val c1Flags = ("c1 ", "-XX:-TieredCompilation -XX:CompileThreshold=1 -client") // -XX:+PrintCompilation")
      val c2Flags = ("c2 ", "-XX:-TieredCompilation -XX:CompileThreshold=1 -server") // -XX:+PrintCompilation")
      val samples = 1
      val minWarmupRuns = 1000

      for ((flagtype, flags) <- Seq(interpFlags, c1Flags, c2Flags)) {

         // The four measures are needed for the formating of the reporter
         measure method "%s".format(flagtype) in {
            using(test) curve ("Range") config (exec.jvmflags -> flags, exec.minWarmupRuns -> minWarmupRuns, exec.independentSamples -> samples) in {
               _.test
            }
         }

         measure method "%s".format(flagtype) in {
            using(test) curve ("Range") config (exec.jvmflags -> flags, exec.minWarmupRuns -> minWarmupRuns, exec.independentSamples -> samples) in {
               _.testUnrolled
            }
         }

         measure method "%s".format(flagtype) in {
            using(test) curve ("Range") config (exec.jvmflags -> flags, exec.minWarmupRuns -> minWarmupRuns, exec.independentSamples -> samples) in {
               _.testSpecialized
            }
         }

         measure method "%s".format(flagtype) in {
            using(test) curve ("Range") config (exec.jvmflags -> flags, exec.minWarmupRuns -> minWarmupRuns, exec.independentSamples -> samples) in {
               _.testSpecializedBlock
            }
         }
      }
   }
}

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
            output += f"  ${result.measurements.head.params.axisData.head._1} -> ${result.measurements.head.params.axisData.head._2}\n"
            output += f"${""}%6s"
            output += f"${t1}%20s"
            output += f"${t2}%20s"
            output += f"${t3}%20s\n"
         }

         if (count % 3 == 0) {
            val n = result.context.scope.length
            output += f"${result.context.scope}%5s:"
         }

         output += f"${result.measurements.head.time}% 20f"

         print(output)

         if (count % 3 == 2) {
            println
         }
         count += 1
      }
   }

   for (n <- Seq(1000000) /* Seq(500000, 1000000, 2000000)*/) {
//      bench(Gen.single("TestArrayReverse[Int]")(n).map(new TestArrayReverse[Int](_)))
//      bench(Gen.single("TestArrayReverse[Double]")(n).map(new TestArrayReverse[Double](_)))
//      bench(Gen.single("TestArrayReverse[Boolean]")(n).map(new TestArrayReverse[Boolean](_)))
//      bench(Gen.single("TestArrayReverse[Any]")(n).map(new TestArrayReverse[Any](_)))

//      bench(Gen.single("TestArrayDuplicate[Int]")(n).map(new TestArrayDuplicate[Int](_)))
      bench(Gen.single("TestArrayDuplicate[Double]")(n).map(new TestArrayDuplicate[Double](_)))
//      bench(Gen.single("TestArrayDuplicate[Boolean]")(n).map(new TestArrayDuplicate[Boolean](_)))
//      bench(Gen.single("TestArrayDuplicate[Any]")(n).map(new TestArrayDuplicate[Any](_)))

//      bench(Gen.single("TestFunctionApplyNTimesRec[Int]")(n).map(new TestFunctionApplyNTimesRec[Int](_)(7, (x: Int) => x * 5)))
//      bench(Gen.single("TestFunctionApplyNTimesRec[Double]")(n).map(new TestFunctionApplyNTimesRec[Double](_)(2.3, (x: Double) => x * 2.3d)))
//      bench(Gen.single("TestFunctionApplyNTimesRec[Boolean]")(n).map(new TestFunctionApplyNTimesRec[Boolean](_)(false, (x: Boolean) => !x)))
//      bench(Gen.single("TestFunctionApplyNTimesRec[Any]")(n).map(new TestFunctionApplyNTimesRec[Any](_)("f", (x: Any) => x)))
//
//      bench(Gen.single("TestArrayMapOverFunction[Int]")(n).map(new TestArrayMapOverFunction[Int](_)((x: Int) => x)))
//      bench(Gen.single("TestArrayMapOverFunction[Double]")(n).map(new TestArrayMapOverFunction[Double](_)((x: Double) => x)))
//      bench(Gen.single("TestArrayMapOverFunction[Boolean]")(n).map(new TestArrayMapOverFunction[Boolean](_)((x: Boolean) => x)))
//      bench(Gen.single("TestArrayMapOverFunction[Any]")(n).map(new TestArrayMapOverFunction[Any](_)((x: Any) => x)))
//      //
//      bench(Gen.single("TestFunctionApplyNTimesRecOverTuples[Int]")(n).map(new TestFunctionApplyNTimesRecOverTuples[Int](_)((7, 4), (x: Int) => x * 5)))
//      bench(Gen.single("TestFunctionApplyNTimesRecOverTuples[Double]")(n).map(new TestFunctionApplyNTimesRecOverTuples[Double](_)((2.3, 3.5), (x: Double) => x * 2.3d)))
//      bench(Gen.single("TestFunctionApplyNTimesRecOverTuples[Boolean]")(n).map(new TestFunctionApplyNTimesRecOverTuples[Boolean](_)((false, true), (x: Boolean) => !x)))
//      bench(Gen.single("TestFunctionApplyNTimesRecOverTuples[Any]")(n).map(new TestFunctionApplyNTimesRecOverTuples[Any](_)(("f", BigInt("3")), (x: Any) => x.hashCode)))
//
//      bench(Gen.single("TestArrayCount[Int]")(n).map(new TestArrayCount[Int](_)(1)))
//      bench(Gen.single("TestArrayCount[Double]")(n).map(new TestArrayCount[Double](_)(2.0)))
//      bench(Gen.single("TestArrayCount[Boolean]")(n).map(new TestArrayCount[Boolean](_)(false)))
//      bench(Gen.single("TestArrayCount[Any]")(n).map(new TestArrayCount[Any](_)("2")))
//
//      bench(Gen.single("TestArrayOfTuplesSwap[Int]")(n).map(new TestArrayOfTuplesSwap[Int](_)))
//      bench(Gen.single("TestArrayOfTuplesSwap[Double]")(n).map(new TestArrayOfTuplesSwap[Double](_)))
//      bench(Gen.single("TestArrayOfTuplesSwap[Boolean]")(n).map(new TestArrayOfTuplesSwap[Boolean](_)))
//      bench(Gen.single("TestArrayOfTuplesSwap[Any]")(n).map(new TestArrayOfTuplesSwap[Any](_)))
   }

   def bench(test: Gen[TestApi]): Unit = {
//      val interpFlags = ("int", "-Xint")
//      val c1Flags = ("c1 ", "-XX:+PrintInlining -XX:+PrintCompilation -XX:-TieredCompilation -XX:CompileThreshold=1 -client") // -XX:+PrintCompilation")
      val c2Flags = ("c2 ", " -XX:+PrintInlining -XX:+PrintCompilation -XX:+TraceDeoptimization " + " -XX:-TieredCompilation -XX:CompileThreshold=1 -server") // -XX:+PrintCompilation")
      val samples = 1
      val minWarmupRuns = 10

      for ((flagtype, flags) <- Seq(/*interpFlags, c1Flags,*/ c2Flags)) {

         // The three measures are needed for the formating of the reporter

        measure method "%s".format(flagtype) in {
            using(test) curve ("Range") config (exec.jvmflags -> flags /*, exec.minWarmupRuns -> minWarmupRuns*/, exec.independentSamples -> samples) in {
               _.test
            }
         }


         measure method "%s".format(flagtype) in {
            using(test) curve ("Range") config (exec.jvmflags -> flags /*, exec.minWarmupRuns -> minWarmupRuns*/, exec.independentSamples -> samples) in {
               _.testUnrolled
            }
         }

         measure method "%s".format(flagtype) in {
           using(test) curve ("Range") config (exec.jvmflags -> flags /*, exec.minWarmupRuns -> minWarmupRuns*/, exec.independentSamples -> samples) in {
             _.testSpecialized
           }
         }
      }
   }
}

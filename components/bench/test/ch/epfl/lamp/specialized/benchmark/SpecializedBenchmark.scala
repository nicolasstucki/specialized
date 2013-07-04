package ch.epfl.lamp.specialized.benchmark
import org.scalameter.api._
import ch.epfl.lamp.specialized.benchmark.cases._
import org.scalameter.CurveData
import org.scalameter.Parameters

object SpecializedBenchmark extends PerformanceTest {

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

  for (n <- Seq(1000000, 2000000, 5000000)) {
    bench(Gen.single("BenchmarkArrayReverse[Int]")(n).map(new BenchmarkArrayReverse[Int](_)))
    bench(Gen.single("BenchmarkArrayReverse[Double]")(n).map(new BenchmarkArrayReverse[Double](_)))
    bench(Gen.single("BenchmarkArrayReverse[Boolean]")(n).map(new BenchmarkArrayReverse[Boolean](_)))
    bench(Gen.single("BenchmarkArrayReverse[Any]")(n).map(new BenchmarkArrayReverse[Any](_)))

    // Need bigger n to warm up correctly
    bench(Gen.single("BenchmarkArrayReverseWhile[Int]")(n).map(new BenchmarkArrayReverseWhile[Int](_)))
    bench(Gen.single("BenchmarkArrayReverseWhile[Double]")(n).map(new BenchmarkArrayReverseWhile[Double](_)))
    bench(Gen.single("BenchmarkArrayReverseWhile[Boolean]")(n).map(new BenchmarkArrayReverseWhile[Boolean](_)))
    bench(Gen.single("BenchmarkArrayReverseWhile[Any]")(n).map(new BenchmarkArrayReverseWhile[Any](_)))

    bench(Gen.single("BenchmarkArrayDuplicate[Int]")(n).map(new BenchmarkArrayDuplicate[Int](_)))
    bench(Gen.single("BenchmarkArrayDuplicate[Double]")(n).map(new BenchmarkArrayDuplicate[Double](_)))
    bench(Gen.single("BenchmarkArrayDuplicate[Boolean]")(n).map(new BenchmarkArrayDuplicate[Boolean](_)))
    bench(Gen.single("BenchmarkArrayDuplicate[Any]")(n).map(new BenchmarkArrayDuplicate[Any](_)))

    bench(Gen.single("BenchmarkFunctionApplyNTimesRec[Int]")(n).map(new BenchmarkFunctionApplyNTimesRec[Int](_)(7, (x: Int) => x * 5)))
    bench(Gen.single("BenchmarkFunctionApplyNTimesRec[Double]")(n).map(new BenchmarkFunctionApplyNTimesRec[Double](_)(2.3, (x: Double) => x * 2.3d)))
    bench(Gen.single("BenchmarkFunctionApplyNTimesRec[Boolean]")(n).map(new BenchmarkFunctionApplyNTimesRec[Boolean](_)(false, (x: Boolean) => !x)))
    bench(Gen.single("BenchmarkFunctionApplyNTimesRec[Any]")(n).map(new BenchmarkFunctionApplyNTimesRec[Any](_)("f", (x: Any) => x)))

    bench(Gen.single("BenchmarkFunction2ApplyNTimesRec[Int]")(n).map(new BenchmarkFunction2ApplyNTimesRec[Int](_)(0, 1, (x: Int, y: Int) => x + y)))
    bench(Gen.single("BenchmarkFunction2ApplyNTimesRec[Double]")(n).map(new BenchmarkFunction2ApplyNTimesRec[Double](_)(0.0, 1.0, (x: Double, y: Double) => x + y)))
    bench(Gen.single("BenchmarkFunction2ApplyNTimesRec[Boolean]")(n).map(new BenchmarkFunction2ApplyNTimesRec[Boolean](_)(false, true, (x: Boolean, y: Boolean) => x && y)))
    bench(Gen.single("BenchmarkFunction2ApplyNTimesRec[Any]")(n).map(new BenchmarkFunction2ApplyNTimesRec[Any](_)("f", "g", (x: Any, y: Any) => x)))

    bench(Gen.single("BenchmarkArrayMapOverFunction[Int]")(n).map(new BenchmarkArrayMapOverFunction[Int](_)((x: Int) => x)))
    bench(Gen.single("BenchmarkArrayMapOverFunction[Double]")(n).map(new BenchmarkArrayMapOverFunction[Double](_)((x: Double) => x)))
    bench(Gen.single("BenchmarkArrayMapOverFunction[Boolean]")(n).map(new BenchmarkArrayMapOverFunction[Boolean](_)((x: Boolean) => x)))

    bench(Gen.single("BenchmarkArrayTabulate[Int]")(n).map(new BenchmarkArrayTabulate[Int](_)((x: Int) => 4 * x)))
    bench(Gen.single("BenchmarkArrayTabulate[Double]")(n).map(new BenchmarkArrayTabulate[Double](_)((x: Int) => 4 * x)))
    bench(Gen.single("BenchmarkArrayTabulate[Boolean]")(n).map(new BenchmarkArrayTabulate[Boolean](_)((x: Int) => x % 2 == 0)))
    bench(Gen.single("BenchmarkArrayTabulate[Any]")(n).map(new BenchmarkArrayTabulate[Any](_)((x: Int) => 4 * x)))

    bench(Gen.single("BenchmarkTuplesSwap[Int]")(n).map(new BenchmarkTuplesSwap[Int](_)((1, 3))))
    bench(Gen.single("BenchmarkTuplesSwap[Double]")(n).map(new BenchmarkTuplesSwap[Double](_)(1L, 3L)))
    bench(Gen.single("BenchmarkTuplesSwap[Boolean]")(n).map(new BenchmarkTuplesSwap[Boolean](_)((true, false))))
    bench(Gen.single("BenchmarkTuplesSwap[Any]")(n).map(new BenchmarkTuplesSwap[Any](_)((1, "f"))))
  }

  def bench(test: Gen[BenchmarkApi]): Unit = {
    val interpFlags = ("int", "-Xint")
    val c1Flags = ("c1 ", "-XX:-TieredCompilation -XX:CompileThreshold=1 -client")
    val c2Flags = ("c2 ", "-XX:-TieredCompilation -XX:CompileThreshold=1 -server")
    val sameVMFlags = ("same", "-Xint")

    val samples = 1
    val minWarmupRuns = 30

    for ((flagtype, flags) <- Seq( /*interpFlags,*/ c1Flags, c2Flags /*, sameVMFlags*/ )) {

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

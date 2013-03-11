package ch.epfl.specialize.benchmark
import org.scalameter.api._
import ch.epfl.specialize.banchmark.tests._

object RangeBenchmark
      extends PerformanceTest {

  // PerformanceTest defs
  // TODO: Not sure this is supposed to be transient, but it complains class is not serializable due to it
  @transient lazy val executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.complete(Aggregator.average),
    new Executor.Measurer.Default
  )
  def persistor = Persistor.None
  def reporter = new LoggingReporter

   val start = 200000;
   val end = 300000;
   val step = 10000;

   bench("Test1", "Any", Gen.range("Test1[Any]")(start, end, step).map(new Test1[Any](_)))
   bench("Test1", "Int", Gen.range("Test1[Int]")(start, end, step).map(new Test1[Int](_)))
   bench("Test1", "Double", Gen.range("Test1[Double]")(start, end, step).map(new Test1[Double](_)))
   bench("Test1", "Boolean", Gen.range("Test1[Boolean]")(start, end, step).map(new Test1[Boolean](_)))

   //      bench("Test2", "Any", Gen.range("Test2[Any]")(start, end, step).map(new Test2[Any](_)))
   //      bench("Test2", "Int", Gen.range("Test2[Int]")(start, end, step).map(new Test2[Int](_)))
   //      bench("Test2", "Double", Gen.range("Test2[Double]")(start, end, step).map(new Test2[Double](_)))
   //      bench("Test2", "Boolean", Gen.range("Test2[Boolean]")(start, end, step).map(new Test2[Boolean](_)))

//      bench("Test3", "Any", Gen.range("Test3[Any]")(start, end, step).map(new Test3[Any](_)))
//      bench("Test3", "Int", Gen.range("Test3[Int]")(start, end, step).map(new Test3[Int](_)))
//      bench("Test3", "Double", Gen.range("Test3[Double]")(start, end, step).map(new Test3[Double](_)))
//      bench("Test3", "Boolean", Gen.range("Test3[Boolean]")(start, end, step).map(new Test3[Boolean](_)))

   //   bench("Test4", "Any", Gen.range("Test4[Any]")(start, end, step).map(new Test4[Any](_)))
   //   bench("Test4", "Int", Gen.range("Test4[Int]")(start, end, step).map(new Test4[Int } ](_)))
   //   bench("Test4", "Double", Gen.range("Test4[Double]")(start, end, step).map(new Test4[Double](_)))
   //   bench("Test4", "Boolean", Gen.range("Test4[Boolean]")(start, end, step).map(new Test4[Boolean](_)))

   //   bench("Test5", "Any", Gen.range("Test5[Any]")(start, end, step).map(new Test5[Any](_)( (x: Any) => x )))
   //   bench("Test5", "Int", Gen.range("Test5[Int]")(start, end, step).map(new Test5[Int](_)( (x: Int) => x )))
   //   bench("Test5", "Double", Gen.range("Test5[Double]")(start, end, step).map(new Test5[Double](_)( (x: Double) => x )))
   //   bench("Test5", "Boolean", Gen.range("Test5[Boolean]")(start, end, step).map(new Test5[Boolean](_)( (x: Boolean) => x )))

   //      bench("Test6", "Any", Gen.range("Test6[Any]")(start, end, step).map(new Test6[Any](_)("f",  (x: Any) => x )))
   //      bench("Test6", "Int", Gen.range("Test6[Int]")(start, end, step).map(new Test6[Int](_)(7, (x: Int) => x+1 )))
   //      bench("Test6", "Double", Gen.range("Test6[Double]")(start, end, step).map(new Test6[Double](_)(2.3, (x: Double) => x+1.0d )))
   //      bench("Test6", "Boolean", Gen.range("Test6[Boolean]")(start, end, step).map(new Test6[Boolean](_)( false, (x: Boolean) => !x )))

   def bench(name: String, tpe: String, test: Gen[TestApi]): Unit = {
     val interpFlags = ""
     val c1Flags = ""
     val c2Flags = ""
     val samples = 1

     for (flags <- List(interpFlags, c1Flags, c2Flags)) {

       measure method "%s[%s].test".format(name, tpe) in {
           using(test) curve ("Range") config (exec.jvmflags -> flags, exec.independentSamples -> samples) in {
              _.test
           }
        }

        measure method "%s[%s].testUnrolled".format(name, tpe) in {
           using(test) curve ("Range") config (exec.jvmflags -> flags, exec.independentSamples -> samples) in {
              _.testUnrolled
           }
        }

        measure method "%s[%s].testSpecialized".format(name, tpe) in {
           using(test) curve ("Range") config (exec.jvmflags -> flags, exec.independentSamples -> samples) in {
              _.testSpecialized
           }
        }
     }
   }
}

package ch.epfl.specialize.banchmark
import org.scalameter.api._
import ch.epfl.specialize.banchmark.tests._

object RangeBenchmark
      extends PerformanceTest.Microbenchmark {

   val start = 10000;
   val end = 20000;
   val step = 5000;

//      bench("Test1", "Any", Gen.range("Test1[Any]")(start, end, step).map(new Test1[Any](_)))
      bench("Test1", "Boolean", Gen.range("Test1[Boolean]")(start, end, step).map(new Test1[Boolean](_)))
//      bench("Test1", "Char", Gen.range("Test1[Char]")(start, end, step).map(new Test1[Char](_)))
//      bench("Test1", "Byte", Gen.range("Test1[Byte]")(start, end, step).map(new Test1[Byte](_)))
      bench("Test1", "Double", Gen.range("Test1[Double]")(start, end, step).map(new Test1[Double](_)))
//      bench("Test1", "Float", Gen.range("Test1[Float]")(start, end, step).map(new Test1[Float](_)))
      bench("Test1", "Int", Gen.range("Test1[Int]")(start, end, step).map(new Test1[Int](_)))
//      bench("Test1", "Long", Gen.range("Test1[Long]")(start, end, step).map(new Test1[Long](_)))
//      bench("Test1", "Short", Gen.range("Test1[Short]")(start, end, step).map(new Test1[Short](_)))

   //   bench("Test2", "Any", Gen.range("Test2[Any]")(start, end, step).map(new Test2[Any](_)))
   //   bench("Test2", "Boolean", Gen.range("Test2[Boolean]")(start, end, step).map(new Test2[Boolean](_)))
   //   bench("Test2", "Char", Gen.range("Test2[Char]")(start, end, step).map(new Test2[Char](_)))
   //   bench("Test2", "Byte", Gen.range("Test2[Byte]")(start, end, step).map(new Test2[Byte](_)))
   //   bench("Test2", "Double", Gen.range("Test2[Double]")(start, end, step).map(new Test2[Double](_)))
   //   bench("Test2", "Float", Gen.range("Test2[Float]")(start, end, step).map(new Test2[Float](_)))
   //   bench("Test2", "Int", Gen.range("Test2[Int]")(start, end, step).map(new Test2[Int](_)))
   //   bench("Test2", "Long", Gen.range("Test2[Long]")(start, end, step).map(new Test2[Long](_)))
   //   bench("Test2", "Short", Gen.range("Test2[Short]")(start, end, step).map(new Test2[Short](_)))

   //   bench("Test3", "Any", Gen.range("Test3[Any]")(start, end, step).map(new Test3[Any](_)))
   //   bench("Test3", "Boolean", Gen.range("Test3[Boolean]")(start, end, step).map(new Test3[Boolean](_)))
   //   bench("Test3", "Char", Gen.range("Test3[Char]")(start, end, step).map(new Test3[Char](_)))
   //   bench("Test3", "Byte", Gen.range("Test3[Byte]")(start, end, step).map(new Test3[Byte](_)))
   //   bench("Test3", "Double", Gen.range("Test3[Double]")(start, end, step).map(new Test3[Double](_)))
   //   bench("Test3", "Float", Gen.range("Test3[Float]")(start, end, step).map(new Test3[Float](_)))
   //   bench("Test3", "Int", Gen.range("Test3[Int]")(start, end, step).map(new Test3[Int](_)))
   //   bench("Test3", "Long", Gen.range("Test3[Long]")(start, end, step).map(new Test3[Long](_)))
   //   bench("Test3", "Short", Gen.range("Test3[Short]")(start, end, step).map(new Test3[Short](_)))

   //   bench("Test4", "Any", Gen.range("Test4[Any]")(start, end, step).map(new Test4[Any](_)))
   //   bench("Test4", "Boolean", Gen.range("Test4[Boolean]")(start, end, step).map(new Test4[Boolean](_)))
   //   bench("Test4", "Char", Gen.range("Test4[Char]")(start, end, step).map(new Test4[Char](_)))
   //   bench("Test4", "Byte", Gen.range("Test4[Byte]")(start, end, step).map(new Test4[Byte](_)))
   //   bench("Test4", "Double", Gen.range("Test4[Double]")(start, end, step).map(new Test4[Double](_)))
   //   bench("Test4", "Float", Gen.range("Test4[Float]")(start, end, step).map(new Test4[Float](_)))
   //   bench("Test4", "Int", Gen.range("Test4[Int]")(start, end, step).map(new Test4[Int } ](_)))
   //   bench("Test4", "Long", Gen.range("Test4[Long]")(start, end, step).map(new Test4[Long](_)))
   //   bench("Test4", "Short", Gen.range("Test4[Short]")(start, end, step).map(new Test4[Short](_)))

   //   bench("Test5", "Any", Gen.range("Test5[Any]")(start, end, step).map(new Test5[Any](_)( (x: Any) => x )))
   //   bench("Test5", "Boolean", Gen.range("Test5[Boolean]")(start, end, step).map(new Test5[Boolean](_)( (x: Boolean) => x )))
   //   bench("Test5", "Char", Gen.range("Test5[Char]")(start, end, step).map(new Test5[Char](_)( (x: Char) => x )))
   //   bench("Test5", "Byte", Gen.range("Test5[Byte]")(start, end, step).map(new Test5[Byte](_)( (x: Byte) => x )))
   //   bench("Test5", "Double", Gen.range("Test5[Double]")(start, end, step).map(new Test5[Double](_)( (x: Double) => x )))
   //   bench("Test5", "Float", Gen.range("Test5[Float]")(start, end, step).map(new Test5[Float](_)( (x: Float) => x )))
   //   bench("Test5", "Int", Gen.range("Test5[Int]")(start, end, step).map(new Test5[Int](_)( (x: Int) => x )))
   //   bench("Test5", "Long", Gen.range("Test5[Long]")(start, end, step).map(new Test5[Long](_)( (x: Long) => x )))
   //   bench("Test5", "Short", Gen.range("Test5[Short]")(start, end, step).map(new Test5[Short](_)( (x: Short) => x )))

//      bench("Test6", "Any", Gen.range("Test6[Any]")(start, end, step).map(new Test6[Any](_)("f",  (x: Any) => x )))
//      bench("Test6", "Boolean", Gen.range("Test6[Boolean]")(start, end, step).map(new Test6[Boolean](_)( false, (x: Boolean) => !x )))
//      bench("Test6", "Char", Gen.range("Test6[Char]")(start, end, step).map(new Test6[Char](_)(1, (x: Char) => x )))
//      bench("Test6", "Byte", Gen.range("Test6[Byte]")(start, end, step).map(new Test6[Byte](_)(2, (x: Byte) => x )))
//      bench("Test6", "Double", Gen.range("Test6[Double]")(start, end, step).map(new Test6[Double](_)(2.3, (x: Double) => x+1.0d )))
//      bench("Test6", "Float", Gen.range("Test6[Float]")(start, end, step).map(new Test6[Float](_)(4.3f, (x: Float) => x+1.0f )))
//      bench("Test6", "Int", Gen.range("Test6[Int]")(start, end, step).map(new Test6[Int](_)(7, (x: Int) => x+1 )))
//      bench("Test6", "Long", Gen.range("Test6[Long]")(start, end, step).map(new Test6[Long](_)(423, (x: Long) => x+1L )))
//      bench("Test6", "Short", Gen.range("Test6[Short]")(start, end, step).map(new Test6[Short](_)(42, (x: Short) => (x+1).toShort )))

   def bench(name: String, tpe: String, test: Gen[TestApi]): Unit = {
      measure method "%s[%s].test".format(name, tpe) in {
         using(test) curve ("Range") in {
            _.test
         }
      }

      measure method "%s[%s].testUnrolled".format(name, tpe) in {
         using(test) curve ("Range") in {
            _.testUnrolled
         }
      }

      measure method "%s[%s].testSpecialized".format(name, tpe) in {
         using(test) curve ("Range") in {
            _.testSpecialized
         }
      }
   }
}
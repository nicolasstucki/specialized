package ch.epfl.specialize.banchmark
import org.scalameter.api._
import ch.epfl.specialize.banchmark.tests._

object RangeBenchmark
      extends PerformanceTest.Microbenchmark {

   val start = 1000;
   val end = 3000;
   val step = 1000;

//   bench("Test1", "Any", Gen.range("Test1[Any]")(start, end, step).map(new Test1[Any](_)))
//   bench("Test1", "Boolean", Gen.range("Test1[Boolean]")(start, end, step).map(new Test1[Boolean](_)))
//   bench("Test1", "Char", Gen.range("Test1[Char]")(start, end, step).map(new Test1[Char](_)))
//   bench("Test1", "Byte", Gen.range("Test1[Byte]")(start, end, step).map(new Test1[Byte](_)))
//   bench("Test1", "Double", Gen.range("Test1[Double]")(start, end, step).map(new Test1[Double](_)))
//   bench("Test1", "Float", Gen.range("Test1[Float]")(start, end, step).map(new Test1[Float](_)))
//   bench("Test1", "Int", Gen.range("Test1[Int]")(start, end, step).map(new Test1[Int](_)))
//   bench("Test1", "Long", Gen.range("Test1[Long]")(start, end, step).map(new Test1[Long](_)))
//   bench("Test1", "Short", Gen.range("Test1[Short]")(start, end, step).map(new Test1[Short](_)))

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
package ch.epfl.specialize.banchmark
import org.scalameter.api._
import ch.epfl.specialize.banchmark.tests._

object RangeBenchmark
      extends PerformanceTest.Microbenchmark {

   val start = 1000;
   val end = 3000;
   val step = 1000;

   val testList = List(
      //("Test1", "Any", new Object, Gen.range("Test1[Any]")(start, end, step).map(new Test1[Any](_))) 
      //("Test1", "Boolean", true, Gen.range("Test1[Boolean]")(start, end, step).map(new Test1[Boolean](_)))
      //("Test1", "Char", Gen.range("Test1[Char]")(start, end, step).map(new Test1[Char](_)))
      //("Test1", "Byte", Gen.range("Test1[Byte]")(start, end, step).map(new Test1[Byte](_))) 
      //("Test1", "Double",42.0, Gen.range("Test1[Double]")(start, end, step).map(new Test1[Double](_))) 
      //("Test1", "Float",42.0, Gen.range("Test1[Float]")(start, end, step).map(new Test1[Float](_)))
      //("Test1", "Int", 42, Gen.range("Test1[Int]")(start, end, step).map(new Test1[Int](_))) 
      //("Test1", "Long",42L, Gen.range("Test1[Long]")(start, end, step).map(new Test1[Long](_)))
      //("Test1", "Short", Gen.range("Test1[Short]")(start, end, step).map(new Test1[Short](_)))
      // ("Test2", "Any", Gen.range("Test2[Any]")(start, end, step).map(new Test2[Any](_)))
       //("Test2", "Int", Gen.range("Test2[Int]")(start, end, step).map(new Test2[Int](_)))
      //("Test3", "Any", Gen.range("Test3[Any]")(start, end, step).map(new Test3[Any](_))) 
         ("Test3", "Int", Gen.range("Test3[Int]")(start, end, step).map(new Test3[Int](_)))
      //("Test3", "Boolean", Gen.range("Test3[Boolean]")(start, end, step).map(new Test3[Boolean](_)))
      )

   for ((name, tpe, test) <- testList) {
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
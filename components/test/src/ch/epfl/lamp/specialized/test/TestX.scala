package ch.epfl.lamp.specialized.benchmark.tests

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

// Class used for quick testing of new changes
class TestX[T: ClassTag, U: ClassTag](val tup: (T, T), val v1: T) {

   def matchTest(v2: T) = {
      specialized[T](Specializable.Primitives) {
         v1
      }
      //      specialized {
      //         tup match {
      //            case (`v1`, `v2`) if v1!=v2 => tup._1
      //            case (`v2`, `v1`) => tup._2
      //         }
      //      }
   }
}
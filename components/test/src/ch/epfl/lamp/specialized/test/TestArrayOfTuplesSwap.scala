package ch.epfl.lamp.specialized.benchmark.tests

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestTuplesSwap[T: ClassTag](val tup: (T, T)) {

   def swap1 = {
      specialized {
         tup.swap
      }
   }
}
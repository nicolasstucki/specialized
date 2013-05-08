package ch.epfl.lamp.specialized.benchmark.tests

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

// Class used for quick testing of new changes
class TestX[T: ClassTag, U: ClassTag]() {

   val arr = new Array[T](2)
   val arrB = new Array[T](arr.size)

   def reverse = {
      specialized[T](Int, Double, Boolean) {
         val temp = arr(0)
         arrB(0) = arr(1)
         arrB(1) = temp
         
      }
   }
}
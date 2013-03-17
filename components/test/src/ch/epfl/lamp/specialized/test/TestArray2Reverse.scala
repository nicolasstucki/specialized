package ch.epfl.lamp.specialized.test

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray2Reverse[T](implicit classTag: ClassTag[T]) {
   val arr = new Array[T](2)

   def reverse = {
      specialized[T] {
         val temp = arr(1)
         arr(0) = arr(1)
         arr(1) = arr(0)
      }
   }
}
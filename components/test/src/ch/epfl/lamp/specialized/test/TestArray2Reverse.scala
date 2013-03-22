package ch.epfl.lamp.specialized.test

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray2Reverse[T](implicit classTag: ClassTag[T]) {
   val arr = new Array[T](2)
   val arrB = new Array[T](arr.size)

   def reverse = {
      specialized[T] {
         val temp = arr(0)
         arrB(0) = arr(1)
         arrB(1) = temp
      }
   }

   def copy = {
      specialized[T] {
         arrB(0) = arr(0)
         arrB(1) = arr(1)
      }
   }

   def size = {
      specialized[T] {
         arr.size
      }
   }

   def copy2 = {
      val arrB = this.arrB
      specialized[T] {
         arr(0) = arrB(0)
         arr(1) = arrB(1)
      }
   }
}
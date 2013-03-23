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
}
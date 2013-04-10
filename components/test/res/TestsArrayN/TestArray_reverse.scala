import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray[T](arrA: Array[T])(implicit classTag: ClassTag[T]) {
   val arrB = new Array[T](arrA.length)
   val arrC = new Array[T](arrA.length)

   def reverse = {
      specialized[T](Int, Double, Boolean) {
         for (i <- 0 to arrA.length / 2) {
            val j = arrA.length - i - 1
            val temp = arrA(j)
            arrA(j) = arrA(i)
            arrA(i) = temp
         }
      }
   }
}
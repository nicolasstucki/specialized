import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray[T](arrA: Array[T])(implicit classTag: ClassTag[T]) {

   def copy = {
      val arrB = new Array[T](arrA.length)
      specialized[T](Int, Double, Boolean) {
         for (i <- 0 until arrA.length) {
            arrB(i) = arrA(i)
         }
      }
   }
}
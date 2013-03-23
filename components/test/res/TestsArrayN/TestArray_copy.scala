import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray[T](arrA: Array[T])(implicit classTag: ClassTag[T]) {
   val arrB = new Array[T](arrA.length)
   val arrC = new Array[T](arrA.length)

   def copy = {
      val arrB = this.arrB
      val arrD = new Array[T](arrA.length)
      val arrE = new Array[T](arrA.length)
      specialized[T] {
         for (i <- 0 until arrA.length) {
            arrB(i) = arrA(i)
            arrD(i) = arrA(i)
         }
      }
   }
}
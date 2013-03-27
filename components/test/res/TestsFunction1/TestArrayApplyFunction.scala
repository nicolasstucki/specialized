import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArrayApplyFunction[T: ClassTag](arr: Array[T], func: T => T) {

   def applyFunc: Array[T] = {
      val arrB = new Array[T](arr.length)
      specialized[T] {
         for (i <- 1 until arr.length) {
            arr(i) = func(arr(i))
         }
         arrB
      }
   }
}
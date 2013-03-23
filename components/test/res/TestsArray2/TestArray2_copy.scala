import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray2Reverse[T: ClassTag] {
   val arr = new Array[T](2)
   val arrB = new Array[T](arr.size)

   def copy = {
      val arrB = this.arrB
      specialized[T] {
         arr(0) = arrB(0)
         arr(1) = arrB(1)
      }
   }
}
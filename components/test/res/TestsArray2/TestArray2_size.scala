import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray2Reverse[T](implicit classTag: ClassTag[T]) {
   val arr = new Array[T](2)
   val arrB = new Array[T](arr.size)

   def size = {
      specialized[T](Int, Double, Boolean) {
         arr.size
      }
   }
}
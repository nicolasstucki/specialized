
import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag] {

   def mIntLongShort() = {
      specialized[T](Int, Long, Short) {
         println
      }
   }

   def mDoubleFloat() = {
      specialized[T](Double, Float) {
         println
      }
   }
   
   def mNumbers() = {
      specialized[T](Int, Long, Short, Double, Float) {
         println
      }
   }
   
   def mEverything() = {
      specialized[T](Int, Long, Short, Double, Float, Boolean, Char, Unit) {
         println
      }
   }
}
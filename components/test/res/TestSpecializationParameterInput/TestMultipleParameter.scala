
import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag](val dummy: T) {
	
   def mIntLongShort() = {
      specialized[T](Int, Long, Short) {
         dummy
      }
   }

   def mDoubleFloat() = {
      specialized[T](Double, Float) {
         dummy
      }
   }
   
   def mNumbers() = {
      specialized[T](Int, Long, Short, Double, Float) {
         dummy
      }
   }
   
   def mEverything() = {
      specialized[T](Int, Long, Short, Double, Float, Boolean, Char, Unit) {
         dummy
      }
   }
}

import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag](val dummy: T) {

   def m1() = {
      specialized[T]() {
         dummy
      }
   }
   
   def m2() = {
      specialized[T] {
         dummy
      }
   }
}
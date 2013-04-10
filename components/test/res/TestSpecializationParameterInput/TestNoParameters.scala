
import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag] {

   def m1() = {
      specialized[T]() {
         println
      }
   }
   
   def m2() = {
      specialized[T] {
         println
      }
   }
}
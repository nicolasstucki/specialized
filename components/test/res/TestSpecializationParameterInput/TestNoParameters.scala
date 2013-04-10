
import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag] {

   def m() = {
      specialized[T]() {
         println
      }
   }
}
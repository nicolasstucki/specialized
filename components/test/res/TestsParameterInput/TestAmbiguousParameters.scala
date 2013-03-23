
import ch.epfl.lamp.specialized._
import scala.reflect.ClassTag

class C[T: ClassTag, U: ClassTag] {

   def m() = {
      specialized {
         Unit
      }
   }
}

import ch.epfl.lamp.specialized._

class C[T,U] {
   
   def m() = {
      specialized[Any](Int, Double, Boolean) {
         0
      }
   }
}
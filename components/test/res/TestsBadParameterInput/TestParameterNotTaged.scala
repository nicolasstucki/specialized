
import ch.epfl.lamp.specialized._

class C[T, U] {

   def m() = {
      specialized[T](Int, Double, Boolean) {
         0
      }
   }
}
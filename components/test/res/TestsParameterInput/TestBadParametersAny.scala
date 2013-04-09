
import ch.epfl.lamp.specialized._

class C[T,U] {
   
   def m() = {
      specialized[Any] {
         0
      }
   }
}
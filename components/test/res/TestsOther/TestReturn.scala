import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestReturn[T: ClassTag](dummy: T) {
   def return1 = {
      specialized[T](Int, Double, Boolean) {
         return dummy
      }
   }
}
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestTuplesSwap[T: ClassTag](val tup: (T, T)) {

   def swapTuple = {
      specialized(Int, Double, Boolean) {
         tup.swap
      }
   }
}
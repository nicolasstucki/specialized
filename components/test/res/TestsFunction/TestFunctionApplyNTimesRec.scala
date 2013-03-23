import scala.reflect.ClassTag
import scala.annotation.tailrec
import ch.epfl.lamp.specialized._

class TestFunctionApplyNTimesRec[T: ClassTag](times: Int, init: T, func: T => T) {

   def applyFunction = {
      specialized[T] {
         @tailrec def rec(n: Int, last: T): T = {
            if (n == 0) last
            else rec(n - 1, func(last))
         }
         rec(times, init)
      }
   }
}
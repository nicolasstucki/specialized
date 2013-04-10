import scala.reflect.ClassTag
import scala.annotation.tailrec
import ch.epfl.lamp.specialized._

class TestFunction2ApplyNTimesRec[T: ClassTag](val times: Int)(val init0: T, val init1: T, val func: (T, T) => T) {

   def applyFunction = {
      specialized[T](Int, Double, Boolean) {
         @tailrec def rec(n: Int, beforelast: T, last: T): T = {
            if (n == 0) last
            else rec(n - 1, last, func(beforelast, last))
         }
         rec(times, init0, init1)
      }
   }
}
package ch.epfl.lamp.specialized.benchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import scala.annotation.tailrec
import ch.epfl.lamp.specialized._

class TestFunctionApplyNTimesRecOverTuples[T: ClassTag](times: Int, init: (T, T), func: T => T) {

   def applyFunction = {
      specialized[T](Int, Double, Boolean) {
         @tailrec def rec(n: Int, last: (T, T)): (T, T) = {
            if (n == 0) last
            else rec(n - 1, (func(last._1), func(last._2)))
         }
         rec(times, init)
      }
   }
}
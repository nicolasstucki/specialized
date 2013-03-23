package ch.epfl.lamp.specialized.benchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import scala.annotation.tailrec
import ch.epfl.lamp.specialized._

class TestFunctionApplyNTimesRecOverTuples[T: ClassTag](times: Int, init: (T, T), func: T => T) {

   def testSpecializedBlock = {
      // TODO: Solve problem with functions
//      specialized[T] {
//         @tailrec def rec(n: Int, last: (T, T)): (T, T) = {
//            if (n == 0) last
//            else rec(n - 1, (func(last._1), func(last._2)))
//         }
//         rec(times, init)
//      }
   }
   
//     def testSpecializedBlock_spec_Int1(TestFunctionApplyNTimesRecOverTuples_this_func1: Int => Int, 
//	 TestFunctionApplyNTimesRecOverTuples_this_init1: (Int, Int), last__11: Int, last__21: Int) = { 
//      
//      def rec(n: Int, last: (Int, Int)): (Int, Int) = 
//         if (n.$eq$eq(0)) last 
//         else rec(n.$minus(1), scala.Tuple2.apply[Int, Int](
//               TestFunctionApplyNTimesRecOverTuples_this_func1.apply(last__11), 
//               TestFunctionApplyNTimesRecOverTuples_this_func1.apply(last__21))); 
//      rec(TestFunctionApplyNTimesRecOverTuples.this.times, TestFunctionApplyNTimesRecOverTuples_this_init1) 
//      
//   }; 
}
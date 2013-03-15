package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import scala.annotation.tailrec

class TestFunctionApplyNTimesRecOverTuples[T](val times: Int)(val init: (T, T), val func: T => T)(implicit mf: ClassTag[T]) extends TestApi {

   def test = {
      //specialized[T] {
      @tailrec def rec(n: Int, last: (T, T)): (T, T) = {
         if (n == 0) last
         else rec(n - 1, (func(last._1), func(last._2)))
      }
      rec(times, init)
      // } 
   }

   def testUnrolled = {
      (if (mf == manifest[Boolean]) {
         val spec_init = init.asInstanceOf[(Boolean, Boolean)]
         val spec_func = func.asInstanceOf[Function1[Boolean, Boolean]]
         @tailrec def rec(n: Int, last: (Boolean, Boolean)): (Boolean, Boolean) = {
            if (n == 0) last
            else rec(n - 1, (spec_func(last._1), spec_func(last._2)))
         }
         rec(times, spec_init)
      } else if (mf == manifest[Double]) {
         val spec_init = init.asInstanceOf[(Double, Double)]
         val spec_func = func.asInstanceOf[Function1[Double, Double]]
         @tailrec def rec(n: Int, last: (Double, Double)): (Double, Double) = {
            if (n == 0) last
            else rec(n - 1, (spec_func(last._1), spec_func(last._2)))
         }
         rec(times, spec_init)
      } else if (mf == manifest[Int]) {
         val spec_init = init.asInstanceOf[(Int, Int)]
         val spec_func = func.asInstanceOf[Function1[Int, Int]]
         @tailrec def rec(n: Int, last: (Int, Int)): (Int, Int) = {
            if (n == 0) last
            else rec(n - 1, (spec_func(last._1), spec_func(last._2)))
         }
         rec(times, spec_init)
      } else {
         @tailrec def rec(n: Int, last: (T, T)): (T, T) = {
            if (n == 0) last
            else rec(n - 1, (func(last._1), func(last._2)))
         }
         rec(times, init)
      }).asInstanceOf[(T, T)]
   }

   def testSpecialized = {
      (if (mf == manifest[Boolean]) {
         spec[Boolean](init.asInstanceOf[(Boolean, Boolean)], func.asInstanceOf[Function1[Boolean, Boolean]])
      } else if (mf == manifest[Double]) {
         spec[Double](init.asInstanceOf[(Double, Double)], func.asInstanceOf[Function1[Double, Double]])
      } else if (mf == manifest[Int]) {
         spec[Int](init.asInstanceOf[(Int, Int)], func.asInstanceOf[Function1[Int, Int]])
      } else {
         spec[T](init, func)
      }).asInstanceOf[(T, T)]
   }

   private def spec[@specialized U](init: (U, U), func: U => U) = {
      @tailrec def rec(n: Int, last: (U, U)): (U, U) = {
         if (n == 0) last
         else rec(n - 1, (func(last._1), func(last._2)))
      }
      rec(times, init)
   }

}
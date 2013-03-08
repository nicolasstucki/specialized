package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import scala.annotation.tailrec

class Test6[T](val times: Int)(val init: T, val func: T => T)(implicit mf: ClassTag[T]) extends TestApi {

   def test = {
      //specialized[T] {
      @tailrec def rec(n: Int, last: T): T = {
         if (n == 0) last
         else rec(n - 1, func(last))
      }
      rec(times, init)
      // } 
   }

   def testUnrolled = {
      (if (mf == manifest[Boolean]) {
         val spec_init = init.asInstanceOf[Boolean]
         val spec_func = func.asInstanceOf[Function1[Boolean, Boolean]]
         @tailrec def rec(n: Int, last: Boolean): Boolean = {
            if (n == 0) last
            else rec(n - 1, spec_func(last))
         }
         rec(times, spec_init)
      } else if (mf == manifest[Double]) {
         val spec_init = init.asInstanceOf[Double]
         val spec_func = func.asInstanceOf[Function1[Double, Double]]
         @tailrec def rec(n: Int, last: Double): Double = {
            if (n == 0) last
            else rec(n - 1, spec_func(last))
         }
         rec(times, spec_init)
      } else if (mf == manifest[Int]) {
         val spec_init = init.asInstanceOf[Int]
         val spec_func = func.asInstanceOf[Function1[Int, Int]]
         @tailrec def rec(n: Int, last: Int): Int = {
            if (n == 0) last
            else rec(n - 1, spec_func(last))
         }
         rec(times, spec_init)
      } else {
         @tailrec def rec(n: Int, last: T): T = {
            if (n == 0) last
            else rec(n - 1, func(last))
         }
         rec(times, init)
      }).asInstanceOf[T]
   }

   def testSpecialized = {
      def spec[@specialized U](init: U, func: U => U) = {
         @tailrec def rec(n: Int, last: U): U = { // Here there is some conversion to do
            if (n == 0) last
            else rec(n - 1, func(last))
         }
         rec(times, init)
      }
      (if (mf == manifest[Boolean]) {
         spec[Boolean](init.asInstanceOf[Boolean], func.asInstanceOf[Boolean => Boolean])
      } else if (mf == manifest[Double]) {
         spec[Double](init.asInstanceOf[Double], func.asInstanceOf[Double => Double])
      } else if (mf == manifest[Int]) {
         spec[Int](init.asInstanceOf[Int], func.asInstanceOf[Int => Int])
      } else {
         spec[T](init, func)
      }).asInstanceOf[T]
   }

}
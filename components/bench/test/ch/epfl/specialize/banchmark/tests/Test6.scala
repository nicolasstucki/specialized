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
      if (mf == manifest[Boolean]) {
         @tailrec def rec(n: Int, last: Boolean): Boolean = {
            if (n == 0) last
            else rec(n - 1, func.asInstanceOf[Function1[Boolean, Boolean]](last))
         }
         rec(times, init.asInstanceOf[Boolean])
      } else if (mf == manifest[Char]) {
         @tailrec def rec(n: Int, last: Char): Char = {
            if (n == 0) last
            else rec(n - 1, func.asInstanceOf[Function1[Char, Char]](last))
         }
         rec(times, init.asInstanceOf[Char])
      } else if (mf == manifest[Byte]) {
         @tailrec def rec(n: Int, last: Byte): Byte = {
            if (n == 0) last
            else rec(n - 1, func.asInstanceOf[Function1[Byte, Byte]](last))
         }
         rec(times, init.asInstanceOf[Byte])
      } else if (mf == manifest[Double]) {
         @tailrec def rec(n: Int, last: Double): Double = {
            if (n == 0) last
            else rec(n - 1, func.asInstanceOf[Function1[Double, Double]](last))
         }
         rec(times, init.asInstanceOf[Double])
      } else if (mf == manifest[Float]) {
         @tailrec def rec(n: Int, last: Float): Float = {
            if (n == 0) last
            else rec(n - 1, func.asInstanceOf[Function1[Float, Float]](last))
         }
         rec(times, init.asInstanceOf[Float])
      } else if (mf == manifest[Int]) {
         @tailrec def rec(n: Int, last: Int): Int = {
            if (n == 0) last
            else rec(n - 1, func.asInstanceOf[Function1[Int, Int]](last))
         }
         rec(times, init.asInstanceOf[Int])
      } else if (mf == manifest[Long]) {
         @tailrec def rec(n: Int, last: Long): Long = {
            if (n == 0) last
            else rec(n - 1, func.asInstanceOf[Function1[Long, Long]](last))
         }
         rec(times, init.asInstanceOf[Long])
      } else if (mf == manifest[Short]) {
         @tailrec def rec(n: Int, last: Short): Short = {
            if (n == 0) last
            else rec(n - 1, func.asInstanceOf[Function1[Short, Short]](last))
         }
         rec(times, init.asInstanceOf[Short])
      } else {
         @tailrec def rec(n: Int, last: T): T = {
            if (n == 0) last
            else rec(n - 1, func(last))
         }
         rec(times, init)
      }
   }

   def testSpecialized = {
      def spec[@specialized U](init: U, func: U => U) = {
         @tailrec def rec(n: Int, last: U): U = { // Here there is some conversion to do
            if (n == 0) last
            else rec(n - 1, func(last))
         }
         rec(times, init)
      }
      spec(init, func)
   }

}
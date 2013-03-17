package ch.epfl.lamp.specialize.benchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag

class TestArrayMapOverFunction[T](val size: Int)(val func: T => T)(implicit mf: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)

   for (i <- 0 until size) arr(i) = if (mf == manifest[Boolean]) {
      (true).asInstanceOf[T]
   } else if (mf == manifest[Double]) {
      (54.6d).asInstanceOf[T]
   } else if (mf == manifest[Int]) {
      (53).asInstanceOf[T]
   } else { (new { def g = "h" }).asInstanceOf[T] }

   def test = {
      //specialized[T] {
      for (i <- 1 until arr.length) {
         arr(i) = func(arr(i))
      }
      // } 
   }

   def testUnrolled = {
      if (mf == manifest[Boolean]) {
         val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
         val spec_func = func.asInstanceOf[Function1[Boolean, Boolean]]
         for (i <- 1 until spec_arr.length) {
            spec_arr(i) = spec_func(spec_arr(i))
         }
      } else if (mf == manifest[Double]) {
         val spec_arr = arr.asInstanceOf[Array[Double]]
         val spec_func = func.asInstanceOf[Function1[Double, Double]]
         for (i <- 1 until spec_arr.length) {
            spec_arr(i) = spec_func(spec_arr(i))
         }
      } else if (mf == manifest[Int]) {
         val spec_arr = arr.asInstanceOf[Array[Int]]
         val spec_func = func.asInstanceOf[Function1[Int, Int]]
         for (i <- 1 until spec_arr.length) {
            spec_arr(i) = spec_func(spec_arr(i))
         }
      } else {
         for (i <- 1 until arr.length) {
            arr(i) = func(arr(i))
         }
      }
   }

   def testSpecialized = {
      (if (mf == manifest[Boolean]) {
         spec[Boolean](arr.asInstanceOf[Array[Boolean]], func.asInstanceOf[Boolean => Boolean])
      } else if (mf == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[Double]], func.asInstanceOf[Double => Double])
      } else if (mf == manifest[Int]) {
         spec[Int](arr.asInstanceOf[Array[Int]], func.asInstanceOf[Int => Int])
      } else {
         spec[T](arr, func)
      }).asInstanceOf[Unit]
   }

   private def spec[@specialized U](arr: Array[U], func: U => U) = {
      for (i <- 1 until arr.length) {
         arr(i) = func(arr(i))
      }
   }
}
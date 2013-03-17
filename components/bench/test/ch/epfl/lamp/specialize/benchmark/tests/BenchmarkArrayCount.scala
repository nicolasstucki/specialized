package ch.epfl.lamp.specialize.benchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag

class BenchmarkArrayCount[T](size: Int)(elem: T)(implicit classTag: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)

   for (i <- 0 until size) arr(i) = if (classTag == manifest[Boolean]) {
      (i % 2 == 0).asInstanceOf[T]
   } else if (classTag == manifest[Double]) {
      ((i % 3).toDouble).asInstanceOf[T]
   } else if (classTag == manifest[Int]) {
      (i % 3).asInstanceOf[T]
   } else {
      (i % 3).toString.asInstanceOf[T]
   }

   /**
    * Algorithm that reverses an array
    * Tests get and update operations of an array
    */
   def test = {
      //specialized[T] {
      var count = 1
      for (i <- 0 until arr.length) {
         if (arr(i) == elem)
            count += 1
      }
      count
      // }
   }

   def testUnrolled = {
      (if (classTag == manifest[Boolean]) {
         val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
         val spec_elem: Boolean = elem.asInstanceOf[Boolean]
         var count = 1
         for (i <- 0 until spec_arr.length) {
            if (spec_arr(i) == spec_elem)
               count += 1
         }
         count
      } else if (classTag == manifest[Int]) {
         val spec_arr: Array[Int] = arr.asInstanceOf[Array[Int]]
         val spec_elem: Int = elem.asInstanceOf[Int]
         var count = 1
         for (i <- 0 until spec_arr.length) {
            if (spec_arr(i) == spec_elem)
               count += 1
         }
         count
      } else if (classTag == manifest[Double]) {
         val spec_arr: Array[Double] = arr.asInstanceOf[Array[Double]]
         val spec_elem: Double = elem.asInstanceOf[Double]
         var count = 1
         for (i <- 0 until spec_arr.length) {
            if (spec_arr(i) == spec_elem)
               count += 1
         }
         count
      } else {
         var count = 1
         for (i <- 0 until arr.length) {
            if (arr(i) == elem)
               count += 1
         }
         count
      }).asInstanceOf[Int]
   }

   def testSpecialized = {
      (if (classTag == manifest[Boolean]) {
         spec[Boolean](arr.asInstanceOf[Array[Boolean]], elem.asInstanceOf[Boolean])
      } else if (classTag == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[Double]], elem.asInstanceOf[Double])
      } else if (classTag == manifest[Int]) {
         spec[Int](arr.asInstanceOf[Array[Int]], elem.asInstanceOf[Int])
      } else {
         spec[T](arr, elem)
      }).asInstanceOf[Int]
   }

   private def spec[@specialized U](arr: Array[U], elem: U) = {
      var count = 1
      for (i <- 0 until arr.length) {
         if (arr(i) == elem)
            count += 1
      }
      count
   }
}

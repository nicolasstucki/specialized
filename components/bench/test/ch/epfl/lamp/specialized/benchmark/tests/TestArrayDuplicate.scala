package ch.epfl.lamp.specialized.benchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArrayDuplicate[T](val size: Int)(implicit classTag: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)
   for (i <- 0 until size) arr(i) = if (classTag == manifest[Boolean]) {
      (true).asInstanceOf[T]
   } else if (classTag == manifest[Double]) {
      (54.3d).asInstanceOf[T]
   } else if (classTag == manifest[Int]) {
      (53).asInstanceOf[T]
   } else { (new { def g = "h" }).asInstanceOf[T] }

   def test: Array[T] = {
      val arr2 = new Array[T](size)
      for (i <- 0 until size) {
         arr2(i) = arr(i)
      }
      return arr2
   }

   def testSpecializedBlock: Array[T] = {
      specialized[T] {
         val arr2 = new Array[T](size)
         for (i <- 0 until size) {
            arr2(i) = arr(i)
         }
         arr2
      }
   }

   def testUnrolled: Array[T] = {
      (if (classTag == manifest[Boolean]) {
         val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
         val arr2 = new Array[Boolean](size)
         for (i <- 0 until size) {
            arr2(i) = spec_arr(i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (classTag == manifest[Double]) {
         val spec_arr: Array[Double] = arr.asInstanceOf[Array[Double]]
         val arr2 = new Array[Double](size)
         //         for (i <- 0 until size) {
         //            arr2(i) = spec_arr(i)
         //         }
         var i = 0
         while (i < size) {
            arr2(i) = spec_arr(i)
            i += 1
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (classTag == manifest[Int]) {
         val spec_arr: Array[Int] = arr.asInstanceOf[Array[Int]]
         val arr2 = new Array[Int](size)
         for (i <- 0 until size) {
            arr2(i) = spec_arr.asInstanceOf[Array[Int]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else {
         val arr2 = new Array[T](size)
         for (i <- 0 until arr.length) {
            arr2(i) = arr(i)
         }
         return arr2
      }).asInstanceOf[Array[T]]
   }

   def testSpecialized = {

      (if (classTag == manifest[Boolean]) {
         spec[Boolean](arr.asInstanceOf[Array[Boolean]])
      } else if (classTag == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[Double]])
      } else if (classTag == manifest[Int]) {
         spec[Int](arr.asInstanceOf[Array[Int]])
      } else {
         spec[T](arr)
      }).asInstanceOf[Array[T]]
   }

   def spec[@specialized U](arr: Array[U])(implicit ct: ClassTag[U]): Array[U] = {
      val arr2 = new Array[U](size)
      //      for (i <- 0 until size) {
      //         arr2(i) = arr(i)
      //      }
      var i = 0
      while (i < size) {
         arr2(i) = arr(i)
         i += 1
      }
      return arr2
   }
}
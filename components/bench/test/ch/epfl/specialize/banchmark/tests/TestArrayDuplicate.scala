package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag

class TestArrayDuplicate[T](val size: Int)(implicit mf: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)
   for (i <- 0 until size) arr(i) = if (mf == manifest[Boolean]) {
      (true).asInstanceOf[T]
   } else if (mf == manifest[Double]) {
      (54.3d).asInstanceOf[T]
   } else if (mf == manifest[Int]) {
      (53).asInstanceOf[T]
   } else { (new { def g = "h" }).asInstanceOf[T] }

   def test: Array[T] = {
      //specialized[T] {
      val arr2 = new Array[T](size)
      for (i <- 0 until size) {
         arr2(i) = arr(i)
      }
      return arr2
      // } 
   }

   def testUnrolled: Array[T] = {
      (if (mf == manifest[Boolean]) {
         val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
         val arr2 = new Array[Boolean](size)
         for (i <- 0 until size) {
            arr2(i) = spec_arr(i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (mf == manifest[Double]) {
         val spec_arr: Array[Double] = arr.asInstanceOf[Array[Double]]
         val arr2 = new Array[Double](size)
//         for (i <- 0 until size) {
//            arr2(i) = spec_arr(i)
//         }
         var i=0
         while (i < size) {
         arr2(i) = spec_arr(i)
         i += 1
      }
         return arr2.asInstanceOf[Array[T]]
      } else if (mf == manifest[Int]) {
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

      (if (mf == manifest[Boolean]) {
         spec[Boolean](arr.asInstanceOf[Array[Boolean]])
      } else if (mf == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[Double]])
      } else if (mf == manifest[Int]) {
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
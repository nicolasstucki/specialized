package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag

class Test1[T](val size: Int)(implicit mf: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)

   def test = {
      //specialized[T] { 
      for (i <- 0 until arr.length / 2) {
         val temp = arr(arr.length - i - 1)
         arr(arr.length - i - 1) = arr(i)
         arr(i) = temp
      }
      // } 
   }

   def testUnrolled = {
      if (mf == manifest[Boolean]) {
         val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
         for (i <- 0 until spec_arr.length / 2) {
            val temp: Boolean = spec_arr(spec_arr.length - i - 1)
            spec_arr(spec_arr.length - i - 1) = spec_arr(i)
            spec_arr(i) = temp
         }
      } else if (mf == manifest[Int]) {
         val spec_arr: Array[Int] = arr.asInstanceOf[Array[Int]]
         for (i <- 0 until spec_arr.length / 2) {
            val temp: Int = spec_arr(spec_arr.length - i - 1)
            spec_arr(spec_arr.length - i - 1) = spec_arr(i)
            spec_arr(i) = temp
         }
      } else if (mf == manifest[Double]) {
         val spec_arr: Array[Double] = arr.asInstanceOf[Array[Double]]
         for (i <- 0 until spec_arr.length / 2) {
            val temp: Double = spec_arr(spec_arr.length - i - 1)
            spec_arr(spec_arr.length - i - 1) = spec_arr(i)
            spec_arr(i) = temp
         }
      } else {
         for (i <- 0 until arr.length / 2) {
            val temp = arr(arr.length - i - 1)
            arr(arr.length - i - 1) = arr(i)
            arr(i) = temp
         }
      }
   }

   def testSpecialized = {
      def spec[@specialized U](arr: Array[U]) = {
         for (i <- 0 until arr.length / 2) {
            val temp = arr(arr.length - i - 1)
            arr(arr.length - i - 1) = arr(i)
            arr(i) = temp
         }
      }
      (if (mf == manifest[Boolean]) {
         spec[Boolean](arr.asInstanceOf[Array[Boolean]])
      } else if (mf == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[Double]])
      } else if (mf == manifest[Int]) {
         spec[Int](arr.asInstanceOf[Array[Int]])
      } else {
         spec[T](arr)
      }).asInstanceOf[Unit]
   }

}
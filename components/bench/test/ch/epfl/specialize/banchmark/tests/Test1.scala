package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag

class Test1[T](val size: Int)(implicit classTag: ClassTag[T]) extends TestApi {
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
      if (classTag == manifest[Boolean]) {
         val arr: Array[Boolean] = this.arr.asInstanceOf[Array[Boolean]]
         for (i <- 0 until arr.length / 2) {
            val temp: Boolean = arr(arr.length - i - 1)
            arr(arr.length - i - 1) = arr(i)
            arr(i) = temp
         }
      } else if (classTag == manifest[Int]) {
         val arr: Array[Int] = this.arr.asInstanceOf[Array[Int]]
         for (i <- 0 until arr.length / 2) {
            val temp: Int = arr(arr.length - i - 1)
            arr(arr.length - i - 1) = arr(i)
            arr(i) = temp
         }
      } else if (classTag == manifest[Double]) {
         val arr: Array[Double] = this.arr.asInstanceOf[Array[Double]]
         for (i <- 0 until arr.length / 2) {
            val temp: Double = arr(arr.length - i - 1)
            arr(arr.length - i - 1) = arr(i)
            arr(i) = temp
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
      (if (classTag == manifest[Boolean]) {
         spec[Boolean](arr.asInstanceOf[Array[Boolean]])
      } else if (classTag == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[Double]])
      } else if (classTag == manifest[Int]) {
         spec[Int](arr.asInstanceOf[Array[Int]])
      } else {
         spec[T](arr)
      }).asInstanceOf[Unit]
   }

}
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
         val arr2: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
         for (i <- 0 until arr2.length / 2) {
            val temp: Boolean = arr2(arr2.length - i - 1)
            arr2(arr2.length - i - 1) = arr2(i)
            arr2(i) = temp
         }
      } else if (mf == manifest[Char]) {
         ???
      } else if (mf == manifest[Byte]) {
         ???
      } else if (mf == manifest[Double]) {
         val arr2: Array[Double] = arr.asInstanceOf[Array[Double]]
         for (i <- 0 until arr2.length / 2) {
            val temp: Double = arr2(arr2.length - i - 1)
            arr2(arr2.length - i - 1) = arr2(i)
            arr2(i) = temp
         }
      } else if (mf == manifest[Float]) {
         ???
      } else if (mf == manifest[Int]) {
         val arr2: Array[Int] = arr.asInstanceOf[Array[Int]]
         for (i <- 0 until arr2.length / 2) {
            val temp: Int = arr2(arr2.length - i - 1)
            arr2(arr.length - i - 1) = arr2(i)
            arr2(i) = temp
         }
      } else if (mf == manifest[Long]) {
         ???
      } else if (mf == manifest[Short]) {
         ???
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
      } else if (mf == manifest[Char]) {
         spec[Char](arr.asInstanceOf[Array[Char]])
      } else if (mf == manifest[Byte]) {
         spec[Byte](arr.asInstanceOf[Array[Byte]])
      } else if (mf == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[Double]])
      } else if (mf == manifest[Float]) {
         spec[Float](arr.asInstanceOf[Array[Float]])
      } else if (mf == manifest[Int]) {
         spec[Int](arr.asInstanceOf[Array[Int]])
      } else if (mf == manifest[Long]) {
         spec[Long](arr.asInstanceOf[Array[Long]])
      } else if (mf == manifest[Short]) {
         spec[Short](arr.asInstanceOf[Array[Short]])
      } else {
         spec[T](arr)
      }).asInstanceOf[Unit]
   }

}
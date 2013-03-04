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
         for (i <- 0 until arr.asInstanceOf[Array[Boolean]].length / 2) {
            val temp: Boolean = arr.asInstanceOf[Array[Boolean]](arr.length - i - 1)
            arr.asInstanceOf[Array[Boolean]](arr.asInstanceOf[Array[Boolean]].length - i - 1) = arr.asInstanceOf[Array[Boolean]](i)
            arr.asInstanceOf[Array[Boolean]](i) = temp
         }
      } else if (mf == manifest[Char]) {
         for (i <- 0 until arr.asInstanceOf[Array[Char]].length / 2) {
            val temp: Char = arr.asInstanceOf[Array[Char]](arr.length - i - 1)
            arr.asInstanceOf[Array[Char]](arr.asInstanceOf[Array[Char]].length - i - 1) = arr.asInstanceOf[Array[Char]](i)
            arr.asInstanceOf[Array[Char]](i) = temp
         }
      } else if (mf == manifest[Byte]) {
         for (i <- 0 until arr.asInstanceOf[Array[Byte]].length / 2) {
            val temp: Byte = arr.asInstanceOf[Array[Byte]](arr.length - i - 1)
            arr.asInstanceOf[Array[Byte]](arr.asInstanceOf[Array[Byte]].length - i - 1) = arr.asInstanceOf[Array[Byte]](i)
            arr.asInstanceOf[Array[Byte]](i) = temp
         }
      } else if (mf == manifest[Double]) {
         for (i <- 0 until arr.asInstanceOf[Array[Double]].length / 2) {
            val temp: Double = arr.asInstanceOf[Array[Double]](arr.length - i - 1)
            arr.asInstanceOf[Array[Double]](arr.asInstanceOf[Array[Double]].length - i - 1) = arr.asInstanceOf[Array[Double]](i)
            arr.asInstanceOf[Array[Double]](i) = temp
         }
      } else if (mf == manifest[Float]) {
         for (i <- 0 until arr.asInstanceOf[Array[Float]].length / 2) {
            val temp: Float = arr.asInstanceOf[Array[Float]](arr.length - i - 1)
            arr.asInstanceOf[Array[Float]](arr.asInstanceOf[Array[Float]].length - i - 1) = arr.asInstanceOf[Array[Float]](i)
            arr.asInstanceOf[Array[Float]](i) = temp
         }
      } else if (mf == manifest[Int]) {
         for (i <- 0 until arr.asInstanceOf[Array[Int]].length / 2) {
            val temp: Int = arr.asInstanceOf[Array[Int]](arr.length - i - 1)
            arr.asInstanceOf[Array[Int]](arr.asInstanceOf[Array[Int]].length - i - 1) = arr.asInstanceOf[Array[Int]](i)
            arr.asInstanceOf[Array[Int]](i) = temp
         }
      } else if (mf == manifest[Long]) {
         for (i <- 0 until arr.asInstanceOf[Array[Long]].length / 2) {
            val temp: Long = arr.asInstanceOf[Array[Long]](arr.length - i - 1)
            arr.asInstanceOf[Array[Long]](arr.asInstanceOf[Array[Long]].length - i - 1) = arr.asInstanceOf[Array[Long]](i)
            arr.asInstanceOf[Array[Long]](i) = temp
         }
      } else if (mf == manifest[Short]) {
         for (i <- 0 until arr.asInstanceOf[Array[Short]].length / 2) {
            val temp: Short = arr.asInstanceOf[Array[Short]](arr.length - i - 1)
            arr.asInstanceOf[Array[Short]](arr.asInstanceOf[Array[Short]].length - i - 1) = arr.asInstanceOf[Array[Short]](i)
            arr.asInstanceOf[Array[Short]](i) = temp
         }
      } else {
         for (i <- 0 until arr.length / 2) {
            val temp = arr.asInstanceOf[Array[Any]](arr.asInstanceOf[Array[Any]].length - i - 1)
            arr(arr.asInstanceOf[Array[Any]].length - i - 1) = arr(i)
            arr.asInstanceOf[Array[Any]](i) = temp
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
      spec(arr)
   }

}
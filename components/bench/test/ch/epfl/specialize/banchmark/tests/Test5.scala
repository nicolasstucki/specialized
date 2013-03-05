package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag

class Test5[T](val size: Int)(val func: T => T)(implicit mf: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)

   for (i <- 0 until size) arr(i) = if (mf == manifest[Boolean]) {
      (true).asInstanceOf[T]
   } else if (mf == manifest[Char]) {
      (46).asInstanceOf[T]
   } else if (mf == manifest[Byte]) {
      (5).asInstanceOf[T]
   } else if (mf == manifest[Double]) {
      (54.6).asInstanceOf[T]
   } else if (mf == manifest[Float]) {
      (63.3).asInstanceOf[T]
   } else if (mf == manifest[Int]) {
      (53).asInstanceOf[T]
   } else if (mf == manifest[Long]) {
      (35).asInstanceOf[T]
   } else if (mf == manifest[Short]) {
      (12).asInstanceOf[T]
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
         for (i <- 1 until arr.asInstanceOf[Array[Boolean]].length) {
            arr.asInstanceOf[Array[Boolean]](i) = func.asInstanceOf[Function1[Boolean, Boolean]](arr.asInstanceOf[Array[Boolean]](i))
         }
      } else if (mf == manifest[Char]) {
         for (
            i <- 1 until arr.asInstanceOf[Array[Char]].length
         ) {
            arr.asInstanceOf[Array[Char]](i) = func.asInstanceOf[Function1[Char, Char]](arr.asInstanceOf[Array[Char]](i))
         }
      } else if (mf == manifest[Byte]) {
         for (i <- 1 until arr.asInstanceOf[Array[Byte]].length) {
            arr.asInstanceOf[Array[Byte]](i) = func.asInstanceOf[Function1[Byte, Byte]](arr.asInstanceOf[Array[Byte]](i))
         }
      } else if (mf == manifest[Double]) {
         for (i <- 1 until arr.asInstanceOf[Array[Double]].length) {
            arr.asInstanceOf[Array[Double]](i) = func.asInstanceOf[Function1[Double, Double]](arr.asInstanceOf[Array[Double]](i))
         }
      } else if (mf == manifest[Float]) {
         for (i <- 1 until arr.asInstanceOf[Array[Float]].length) {
            arr.asInstanceOf[Array[Float]](i) = func.asInstanceOf[Function1[Float, Float]](arr.asInstanceOf[Array[Float]](i))
         }
      } else if (mf == manifest[Int]) {
         for (i <- 1 until arr.asInstanceOf[Array[Int]].length) {
            arr.asInstanceOf[Array[Int]](i) = func.asInstanceOf[Function1[Int, Int]](arr.asInstanceOf[Array[Int]](i))
         }
      } else if (mf == manifest[Long]) {
         for (i <- 1 until arr.asInstanceOf[Array[Long]].length) {
            arr.asInstanceOf[Array[Long]](i) = func.asInstanceOf[Function1[Long, Long]](arr.asInstanceOf[Array[Long]](i))
         }
      } else if (mf == manifest[Short]) {
         for (i <- 1 until arr.asInstanceOf[Array[Short]].length) {
            arr.asInstanceOf[Array[Short]](i) = func.asInstanceOf[Function1[Short, Short]](arr.asInstanceOf[Array[Short]](i))
         }
      } else {
         for (i <- 1 until arr.length) {
            arr(i) = func(arr(i))
         }
      }
   }

   def testSpecialized = {
      def spec[@specialized U](arr: Array[U], func: U => U) = {
         for (i <- 1 until arr.length) {
            arr(i) = func(arr(i))
         }
      }
      spec(arr, func)
   }

}
package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag

class Test4[T <: Ordered[T]](val size: Int)(implicit mf: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)
   for (i <- 0 until size) arr(i) = if (mf == manifest[Boolean]) {
      (true).asInstanceOf[T]
   } else if (mf == manifest[Char]) {
      (46).asInstanceOf[T]
   } else if (mf == manifest[Byte]) {
      (5).asInstanceOf[T]
   } else if (mf == manifest[Double]) {
      (54).asInstanceOf[T]
   } else if (mf == manifest[Float]) {
      (63).asInstanceOf[T]
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
         for (j <- (i - 1) to 0 by -1) {
            if (arr(j) > arr(j + 1)) {
               val temp = arr(j + 1)
               arr(j + 1) = arr(j)
               arr(j) = temp
            }
         }
      }
      // } 
   }

   def testUnrolled = {
      if (mf == manifest[Boolean]) {

      } else if (mf == manifest[Char]) {

      } else if (mf == manifest[Byte]) {

      } else if (mf == manifest[Double]) {

      } else if (mf == manifest[Float]) {

      } else if (mf == manifest[Int]) {

      } else if (mf == manifest[Long]) {

      } else if (mf == manifest[Short]) {

      } else {

      }
   }

   def testSpecialized = {
      def spec[@specialized U <: Ordered[U]](arr: Array[U]) = {
         for (i <- 1 until arr.length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr(j) > arr(j + 1)) {
                  val temp = arr(j + 1)
                  arr(j + 1) = arr(j)
                  arr(j) = temp
               }
            }
         }
      }
      spec(arr)
   }

}
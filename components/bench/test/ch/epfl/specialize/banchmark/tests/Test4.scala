package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import scala.runtime.RichBoolean

class Test4[T <: Ordered[T]](val size: Int)(implicit mf: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)
   for (i <- 0 until size) arr(i) = if (mf == manifest[Boolean]) {
      (true).asInstanceOf[T]
   } else if (mf == manifest[Double]) {
      (54.7d).asInstanceOf[T]
   } else if (mf == manifest[Int]) {
      (53).asInstanceOf[T]
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
      (if (mf == manifest[Boolean]) {
         val arr = this.arr.asInstanceOf[Array[Boolean]]
         for (i <- 1 until arr.length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr(j) > arr(j + 1)) {
                  val temp: Boolean = arr(j + 1)
                  arr(j + 1) = arr(j)
                  arr(j) = temp
               }
            }
         }
      } else if (mf == manifest[Double]) {
         val arr = this.arr.asInstanceOf[Array[Double]]
         for (i <- 1 until arr.length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr(j) > arr(j + 1)) {
                  val temp: Double = arr(j + 1)
                  arr(j + 1) = arr(j)
                  arr(j) = temp
               }
            }
         }
      } else if (mf == manifest[Int]) {
         val arr = this.arr.asInstanceOf[Array[Int]]
         for (i <- 1 until arr.length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr(j) > arr(j + 1)) {
                  val temp: Int = arr(j + 1)
                  arr(j + 1) = arr(j)
                  arr(j) = temp
               }
            }
         }
      } else {
         for (i <- 1 until arr.length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr(j) > arr(j+1)) {
                  val temp = arr(j + 1)
                  arr(j + 1) = arr(j)
                  arr(j) = temp
               }
            }
         }
      }).asInstanceOf[Unit]
   }

   
   def testSpecialized = {
//      def spec[@specialized U <: Ordered[U]](arr: Array[U]) = {
//         for (i <- 1 until arr.length) {
//            for (j <- (i - 1) to 0 by -1) {
//               if (arr(j) > arr(j + 1)) {
//                  val temp = arr(j + 1)
//                  arr(j + 1) = arr(j)
//                  arr(j) = temp
//               }
//            }
//         }
//      }
//      (if (mf == manifest[Boolean]) {
//         spec[Boolean](arr.asInstanceOf[Array[Boolean]])
//      } else if (mf == manifest[Double]) {
//         spec[Double](arr.asInstanceOf[Array[Double]])
//      } else if (mf == manifest[Int]) {
//         spec[Int](arr.asInstanceOf[Array[Int]])
//      } else {
//         spec[T](arr)
//      }).asInstanceOf[Unit]
   }

}
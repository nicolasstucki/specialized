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
         for (i <- 1 until arr.asInstanceOf[Array[Boolean]].length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr.asInstanceOf[Array[Boolean]](j) > arr.asInstanceOf[Array[Boolean]](j + 1)) {
                  val temp: Boolean = arr.asInstanceOf[Array[Boolean]](j + 1)
                  arr.asInstanceOf[Array[Boolean]](j + 1) = arr.asInstanceOf[Array[Boolean]](j)
                  arr.asInstanceOf[Array[Boolean]](j) = temp
               }
            }
         }
      } else if (mf == manifest[Char]) {
         for (i <- 1 until arr.asInstanceOf[Array[Char]].length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr.asInstanceOf[Array[Char]](j) > arr.asInstanceOf[Array[Char]](j + 1)) {
                  val temp: Char = arr.asInstanceOf[Array[Char]](j + 1)
                  arr.asInstanceOf[Array[Char]](j + 1) = arr.asInstanceOf[Array[Char]](j)
                  arr.asInstanceOf[Array[Char]](j) = temp
               }
            }
         }
      } else if (mf == manifest[Byte]) {
         for (i <- 1 until arr.asInstanceOf[Array[Byte]].length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr.asInstanceOf[Array[Byte]](j) > arr.asInstanceOf[Array[Byte]](j + 1)) {
                  val temp: Byte = arr.asInstanceOf[Array[Byte]](j + 1)
                  arr.asInstanceOf[Array[Byte]](j + 1) = arr.asInstanceOf[Array[Byte]](j)
                  arr.asInstanceOf[Array[Byte]](j) = temp
               }
            }
         }
      } else if (mf == manifest[Double]) {
         for (i <- 1 until arr.asInstanceOf[Array[Double]].length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr.asInstanceOf[Array[Double]](j) > arr.asInstanceOf[Array[Double]](j + 1)) {
                  val temp: Double = arr.asInstanceOf[Array[Double]](j + 1)
                  arr.asInstanceOf[Array[Double]](j + 1) = arr.asInstanceOf[Array[Double]](j)
                  arr.asInstanceOf[Array[Double]](j) = temp
               }
            }
         }
      } else if (mf == manifest[Float]) {
         for (i <- 1 until arr.asInstanceOf[Array[Float]].length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr.asInstanceOf[Array[Float]](j) > arr.asInstanceOf[Array[Float]](j + 1)) {
                  val temp: Float = arr.asInstanceOf[Array[Float]](j + 1)
                  arr.asInstanceOf[Array[Float]](j + 1) = arr.asInstanceOf[Array[Float]](j)
                  arr.asInstanceOf[Array[Float]](j) = temp
               }
            }
         }
      } else if (mf == manifest[Int]) {
         for (i <- 1 until arr.asInstanceOf[Array[Int]].length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr.asInstanceOf[Array[Int]](j) > arr.asInstanceOf[Array[Int]](j + 1)) {
                  val temp: Int = arr.asInstanceOf[Array[Int]](j + 1)
                  arr.asInstanceOf[Array[Int]](j + 1) = arr.asInstanceOf[Array[Int]](j)
                  arr.asInstanceOf[Array[Int]](j) = temp
               }
            }
         }
      } else if (mf == manifest[Long]) {
         for (i <- 1 until arr.asInstanceOf[Array[Long]].length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr.asInstanceOf[Array[Long]](j) > arr.asInstanceOf[Array[Long]](j + 1)) {
                  val temp: Long = arr.asInstanceOf[Array[Long]](j + 1)
                  arr.asInstanceOf[Array[Long]](j + 1) = arr.asInstanceOf[Array[Long]](j)
                  arr.asInstanceOf[Array[Long]](j) = temp
               }
            }
         }
      } else if (mf == manifest[Short]) {
         for (i <- 1 until arr.asInstanceOf[Array[Short]].length) {
            for (j <- (i - 1) to 0 by -1) {
               if (arr.asInstanceOf[Array[Short]](j) > arr.asInstanceOf[Array[Short]](j + 1)) {
                  val temp: Short = arr.asInstanceOf[Array[Short]](j + 1)
                  arr.asInstanceOf[Array[Short]](j + 1) = arr.asInstanceOf[Array[Short]](j)
                  arr.asInstanceOf[Array[Short]](j) = temp
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
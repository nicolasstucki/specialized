package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag

class Test3[T](val size: Int)(implicit mf: ClassTag[T]) extends TestApi {
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
      if (mf == manifest[Boolean]) {
         val arr2 = new Array[Boolean](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Boolean]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (mf == manifest[Char]) {
         val arr2 = new Array[Char](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Char]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (mf == manifest[Byte]) {
         val arr2 = new Array[Byte](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Byte]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (mf == manifest[Double]) {
         val arr2 = new Array[Byte](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Byte]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (mf == manifest[Float]) {
         val arr2 = new Array[Float](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Float]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (mf == manifest[Int]) {
         val arr2 = new Array[Int](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Int]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (mf == manifest[Long]) {
         val arr2 = new Array[Long](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Long]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else if (mf == manifest[Short]) {
         val arr2 = new Array[Short](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Short]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      } else {
         val arr2: Array[Any] = new Array[T](size).asInstanceOf[Array[Any]]
         for (i <- 0 until arr.asInstanceOf[Array[Any]].length) {
            arr2(i) = arr.asInstanceOf[Array[Any]](i)
         }
         return arr2.asInstanceOf[Array[T]]
      }
   }

   def testSpecialized = {
      def spec[@specialized U](arr: Array[U])(implicit ct: ClassTag[U]): Array[U] = {
         val arr2 = new Array[U](size)
         for (i <- 0 until size) {
            arr2(i) = arr(i)
         }
         return arr2
      }
      spec(arr)
   }

}
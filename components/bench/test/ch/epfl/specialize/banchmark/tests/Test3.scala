package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception

class Test3[T](val size: Int)(implicit mf: Manifest[T]) {
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
   } else { (new {def g="h"}).asInstanceOf[T] }

   def test = {
      //specialized[T] {
      val arr2 = new Array[T](size)
      for (i <- 0 until size) {
         arr(i) = arr(i)
      }
      arr2
      // } 
   }

   def testUnrolled = {
      if (mf == manifest[Boolean]) {
         val arr2 = new Array[Boolean](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Boolean]](i)
         }
         arr2
      } else if (mf == manifest[Char]) {
         val arr2 = new Array[Char](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Char]](i)
         }
         arr2
      } else if (mf == manifest[Byte]) {
         val arr2 = new Array[Byte](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Byte]](i)
         }
         arr2
      } else if (mf == manifest[Double]) {
         val arr2 = new Array[Byte](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Byte]](i)
         }
         arr2
      } else if (mf == manifest[Float]) {
         val arr2 = new Array[Float](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Float]](i)
         }
         arr2
      } else if (mf == manifest[Int]) {
        val arr2 = new Array[Int](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Int]](i)
         }
         arr2
      } else if (mf == manifest[Long]) {
         val arr2 = new Array[Long](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Long]](i)
         }
         arr2
      } else if (mf == manifest[Short]) {
         val arr2 = new Array[Short](size)
         for (i <- 0 until size) {
            arr2(i) = arr.asInstanceOf[Array[Short]](i)
         }
         arr2
      } else {
         val arr2 = new Array[T](size)
         for (i <- 0 until arr.length) {
            arr(i) = arr(i)
         }
         arr2
      }.asInstanceOf[Array[T]]
   }

   def testSpecialized = {
      spec(arr)
   }
   def spec[@specialized U: Manifest](arr: Array[U]) = {
      val arr2 = new Array[U](size)
      for (i <- 0 until size) {
         arr(i) = arr(i)
      }
      arr2
   }

}
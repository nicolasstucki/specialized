package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception

class Test2[T](val size: Int)(implicit mf: Manifest[T]) extends TestApi {
   val arr = new Array[(T, T)](size)
   // Fill array with non null values
   for (i <- 0 until size) arr(i) = if (mf == manifest[Boolean]) {
      (true, false).asInstanceOf[(T, T)]
   } else if (mf == manifest[Char]) {
      (45, 46).asInstanceOf[(T, T)]
   } else if (mf == manifest[Byte]) {
      (5, 7).asInstanceOf[(T, T)]
   } else if (mf == manifest[Double]) {
      (53, 4).asInstanceOf[(T, T)]
   } else if (mf == manifest[Float]) {
      (6, 3).asInstanceOf[(T, T)]
   } else if (mf == manifest[Int]) {
      (5, 3).asInstanceOf[(T, T)]
   } else if (mf == manifest[Long]) {
      (3, 5).asInstanceOf[(T, T)]
   } else if (mf == manifest[Short]) {
      (1, 2).asInstanceOf[(T, T)]
   } else { (new {}, new {}).asInstanceOf[(T, T)] }

   def test = {
      //specialized[T] { 
      for (i <- 0 until arr.length) {
         val (a, b) = arr(i)
         arr(i) = (b, a)
      }
      // } 
   }

   def testUnrolled = {
      if (mf == manifest[Boolean]) {
         for (i <- 0 until arr.length) {
            val (a, b): (Boolean, Boolean) = arr.asInstanceOf[Array[(Boolean, Boolean)]](i)
            arr.asInstanceOf[Array[(Boolean, Boolean)]](i) = (b, a).asInstanceOf[(Boolean, Boolean)]
         }
      } else if (mf == manifest[Char]) {
         for (i <- 0 until arr.length) {
            val (a, b): (Char, Char) = arr.asInstanceOf[Array[(Char, Char)]](i)
            arr.asInstanceOf[Array[(Char, Char)]](i) = (b, a).asInstanceOf[(Char, Char)]
         }
      } else if (mf == manifest[Byte]) {
         for (i <- 0 until arr.length) {
            val (a, b): (Byte, Byte) = arr.asInstanceOf[Array[(Byte, Byte)]](i)
            arr.asInstanceOf[Array[(Byte, Byte)]](i) = (b, a).asInstanceOf[(Byte, Byte)]
         }
      } else if (mf == manifest[Double]) {
         for (i <- 0 until arr.length) {
            val (a, b): (Double, Double) = arr.asInstanceOf[Array[(Double, Double)]](i)
            arr.asInstanceOf[Array[(Double, Double)]](i) = (b, a).asInstanceOf[(Double, Double)]
         }
      } else if (mf == manifest[Float]) {
         for (i <- 0 until arr.length) {
            val (a, b): (Float, Float) = arr.asInstanceOf[Array[(Float, Float)]](i)
            arr.asInstanceOf[Array[(Float, Float)]](i) = (b, a).asInstanceOf[(Float, Float)]
         }
      } else if (mf == manifest[Int]) {
         for (i <- 0 until arr.length) {
            val (a, b): (Int, Int) = arr.asInstanceOf[Array[(Int, Int)]](i)
            arr.asInstanceOf[Array[(Int, Int)]](i) = (b, a).asInstanceOf[(Int, Int)]
         }
      } else if (mf == manifest[Long]) {
         for (i <- 0 until arr.length) {
            val (a, b): (Long, Long) = arr.asInstanceOf[Array[(Long, Long)]](i)
            arr.asInstanceOf[Array[(Long, Long)]](i) = (b, a).asInstanceOf[(Long, Long)]
         }
      } else if (mf == manifest[Short]) {
         for (i <- 0 until arr.length) {
            val (a, b): (Short, Short) = arr.asInstanceOf[Array[(Short, Short)]](i)
            arr.asInstanceOf[Array[(Short, Short)]](i) = (b, a).asInstanceOf[(Short, Short)]
         }
      } else {
         for (i <- 0 until arr.asInstanceOf[Array[(Short, Short)]].length) {
            val tup: (Any, Any) = arr.asInstanceOf[Array[(Any, Any)]](i)
            arr.asInstanceOf[Array[(Any, Any)]](i) = (tup._2, tup._1)
         }
      }
   }

   def testSpecialized = {
      def spec[@specialized U](arr: Array[(U, U)]) = {
         for (i <- 0 until arr.length) {
            val (a, b) = arr(i)
            arr(i) = (b, a)
         }
      }
      spec(arr)
   }

}
package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception

class Test2[T](val size: Int)(implicit mf: Manifest[T]) extends TestApi {
   val arr = new Array[(T, T)](size)
   // Fill array with non null values
   for (i <- 0 until size) arr(i) = if (mf == manifest[Boolean]) {
      (true, false).asInstanceOf[(T, T)]
   } else if (mf == manifest[Double]) {
      (53.0d, 4.4d).asInstanceOf[(T, T)]
   } else if (mf == manifest[Int]) {
      (5, 3).asInstanceOf[(T, T)]
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
      (if (mf == manifest[Boolean]) {
         val spec_arr: Array[(Boolean, Boolean)] = arr.asInstanceOf[Array[(Boolean, Boolean)]]
         for (i <- 0 until spec_arr.length) {
            val (a, b): (Boolean, Boolean) = spec_arr(i)
            spec_arr(i) = (b, a)
         }
      } else if (mf == manifest[Double]) {
         val spec_arr: Array[(Double, Double)] = arr.asInstanceOf[Array[(Double, Double)]]
         for (i <- 0 until spec_arr.length) {
            val (a, b): (Double, Double) = spec_arr(i)
            spec_arr(i) = (b, a)
         }
      } else if (mf == manifest[Int]) {
         val spec_arr: Array[(Int, Int)] = arr.asInstanceOf[Array[(Int, Int)]]
         for (i <- 0 until spec_arr.length) {
            val (a, b): (Int, Int) = spec_arr(i)
            spec_arr(i) = (b, a)
         }
      } else {
         for (i <- 0 until arr.length) {
            val tup = arr(i)
            arr(i) = (tup._2, tup._1)
         }
      }).asInstanceOf[Unit]
   }

   def testSpecialized = {
      def spec[@specialized U](arr: Array[(U, U)]) = {
         for (i <- 0 until arr.length) {
            val (a, b) = arr(i)
            arr(i) = (b, a)
         }
      }
      (if (mf == manifest[Boolean]) {
         spec[Boolean](arr.asInstanceOf[Array[(Boolean, Boolean)]])
      } else if (mf == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[(Double, Double)]])
      } else if (mf == manifest[Int]) {
         spec[Int](arr.asInstanceOf[Array[(Int, Int)]])
      } else {
         spec[T](arr)
      }).asInstanceOf[Unit]
   }

}
package ch.epfl.lamp.specialized.benchmark.tests

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArrayOfTuplesSwap[T](val size: Int)(implicit mf: ClassTag[T]) extends TestApi {
   val arr = new Array[(T, T)](size)
   // Fill array with non null values
   for (i <- 0 until size) arr(i) = if (mf == manifest[Boolean]) {
      (true, false).asInstanceOf[(T, T)]
   } else if (mf == manifest[Double]) {
      (53.0d, 4.4d).asInstanceOf[(T, T)]
   } else if (mf == manifest[Int]) {
      (5, 3).asInstanceOf[(T, T)]
   } else { (new {}, new {}).asInstanceOf[(T, T)] }

   /**
    * Algorithm that inverses the order of all tuples in an array of tuples
    * Tests get and update operations of an array
    * and tests swap operation of tuples
    */
   def test = {
      for (i <- 0 until arr.length) {
         arr(i) = arr(i).swap
      }
   }

   def testSpecializedBlock = {
      specialized[T] {
         for (i <- 0 until arr.length) {
            arr(i) = arr(i).swap
         }
      }
   }

   def testUnrolled = {
      (if (mf == manifest[Boolean]) {
         val spec_arr: Array[(Boolean, Boolean)] = arr.asInstanceOf[Array[(Boolean, Boolean)]]
         for (i <- 0 until spec_arr.length) {
            spec_arr(i) = spec_arr(i).swap
         }
      } else if (mf == manifest[Double]) {
         val spec_arr: Array[(Double, Double)] = arr.asInstanceOf[Array[(Double, Double)]]
         for (i <- 0 until spec_arr.length) {
            spec_arr(i) = spec_arr(i).swap
         }
      } else if (mf == manifest[Int]) {
         val spec_arr: Array[(Int, Int)] = arr.asInstanceOf[Array[(Int, Int)]]
         for (i <- 0 until spec_arr.length) {
            spec_arr(i) = spec_arr(i).swap
         }
      } else {
         for (i <- 0 until arr.length) {
            arr(i) = arr(i).swap
         }
      }).asInstanceOf[Unit]
   }

   def testSpecialized = {

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

   private def spec[@specialized U](arr: Array[(U, U)]) = {
      for (i <- 0 until arr.length) {
         arr(i) = arr(i).swap
      }
   }
}
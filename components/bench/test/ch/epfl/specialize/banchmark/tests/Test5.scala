package ch.epfl.specialize.banchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag

class Test5[T](val size: Int)(val func: T => T)(implicit mf: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)

   for (i <- 0 until size) arr(i) = if (mf == manifest[Boolean]) {
      (true).asInstanceOf[T]
   } else if (mf == manifest[Double]) {
      (54.6d).asInstanceOf[T]
   } else if (mf == manifest[Int]) {
      (53).asInstanceOf[T]
   } else { (new { def g = "h" }).asInstanceOf[T] }

   def test = {
      //specialized[T] {
      for (i <- 1 until arr.length) {
         arr(i) = func(arr(i))
      }
      // } 
   }

   def testUnrolled = {
//      if (mf == manifest[Boolean]) {
//         val spec_arr: Array[Boolean] = arr.isInstanceOf[Array[Boolean]]
//         val spec_func = func.isInstanceOf[Function1[Boolean,Boolean]]
//         for (i <- 1 until spec_arr.length ) {
//            spec_arr(i) = spec_func(spec_arr(i))
//         }
//      } else if (mf == manifest[Double]) {
//         val spec_arr = arr.isInstanceOf[Array[Double]]
//         val spec_func = func.isInstanceOf[Function1[Double,Double]]
//         for (i <- 1 until spec_arr.length ) {
//            spec_arr(i) = spec_func(spec_arr(i))
//         }
//      } else if (mf == manifest[Int]) {
//         val spec_arr = arr.isInstanceOf[Array[Int]]
//         val spec_func = func.isInstanceOf[Function1[Int,Int]]
//         for (i <- 1 until spec_arr.length ) {
//            spec_arr(i) = spec_func(spec_arr(i))
//         }
//      } else {
//         for (i <- 1 until arr.length) {
//            arr(i) = func(arr(i))
//         }
//      }
   }

   def testSpecialized = {
      def spec[@specialized U](arr: Array[U], func: U => U) = {
         for (i <- 1 until arr.length) {
            arr(i) = func(arr(i))
         }
      }
      (if (mf == manifest[Boolean]) {
         spec[Boolean](arr.asInstanceOf[Array[Boolean]], func.asInstanceOf[Boolean=>Boolean])
      } else if (mf == manifest[Char]) {
         spec[Char](arr.asInstanceOf[Array[Char]], func.asInstanceOf[Char=>Char])
      } else if (mf == manifest[Byte]) {
         spec[Byte](arr.asInstanceOf[Array[Byte]], func.asInstanceOf[Byte=>Byte])
      } else if (mf == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[Double]], func.asInstanceOf[Double=>Double])
      } else if (mf == manifest[Float]) {
         spec[Float](arr.asInstanceOf[Array[Float]], func.asInstanceOf[Float=>Float])
      } else if (mf == manifest[Int]) {
         spec[Int](arr.asInstanceOf[Array[Int]], func.asInstanceOf[Int=>Int])
      } else if (mf == manifest[Long]) {
         spec[Long](arr.asInstanceOf[Array[Long]], func.asInstanceOf[Long=>Long])
      } else if (mf == manifest[Short]) {
         spec[Short](arr.asInstanceOf[Array[Short]], func.asInstanceOf[Short=>Short])
      } else {
         spec[T](arr,func)
      }).asInstanceOf[Unit]
   }

}
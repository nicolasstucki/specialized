package ch.epfl.lamp.specialized.benchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class BenchmarkQuicksort[T](val size: Int)(implicit order: Ordering[T], classTag: ClassTag[T]) extends TestApi {
   val arr = new Array[T](size)
   for (i <- 0 until size) arr(i) = if (classTag == manifest[Boolean]) {
      (true).asInstanceOf[T]
   } else if (classTag == manifest[Double]) {
      (54.3d).asInstanceOf[T]
   } else if (classTag == manifest[Int]) {
      (53).asInstanceOf[T]
   } else { (new { def g = "h" }).asInstanceOf[T] }
   
   def test: Array[T] = {
      scala.util.Sorting.quickSort(arr)
      arr
   }

   def testSpecializedBlock: Array[T] = {
      specialized[T] {
         scala.util.Sorting.quickSort(arr)
      }
      arr
   }

   def testUnrolled: Array[T] = {
      arr
   }

   def testSpecialized = {
      (if (classTag == manifest[Boolean]) {
         spec[Boolean](arr.asInstanceOf[Array[Boolean]])
      } else if (classTag == manifest[Double]) {
         spec[Double](arr.asInstanceOf[Array[Double]])
      } else if (classTag == manifest[Int]) {
         spec[Int](arr.asInstanceOf[Array[Int]])
      } else {
         spec[T](arr)
      }).asInstanceOf[Array[T]]
   }

   def spec[@specialized U](arr: Array[U])(implicit ct: ClassTag[U]): Array[U] = {
      arr
   }
}
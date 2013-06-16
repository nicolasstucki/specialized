package ch.epfl.lamp.specialized.benchmark.cases

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class BenchmarkArrayOfTuplesSwap[T](val size: Int)(implicit classTag: ClassTag[T]) extends BenchmarkApi {
  val arr = new Array[(T, T)](size)

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
    if (classTag == ManifestFactory.Boolean) {
      val spec_arr: Array[(Boolean, Boolean)] = arr.asInstanceOf[Array[(Boolean, Boolean)]]
      for (i <- 0 until spec_arr.length) {
        spec_arr(i) = spec_arr(i).swap
      }
    } else if (classTag == ManifestFactory.Double) {
      val spec_arr: Array[(Double, Double)] = arr.asInstanceOf[Array[(Double, Double)]]
      for (i <- 0 until spec_arr.length) {
        spec_arr(i) = spec_arr(i).swap
      }
    } else if (classTag == ManifestFactory.Int) {
      val spec_arr: Array[(Int, Int)] = arr.asInstanceOf[Array[(Int, Int)]]
      for (i <- 0 until spec_arr.length) {
        spec_arr(i) = spec_arr(i).swap
      }
    } else {
      for (i <- 0 until arr.length) {
        arr(i) = arr(i).swap
      }
    }
  }

  def testSpecialized = {
    if (classTag == manifest[Boolean]) {
      spec[Boolean](arr.asInstanceOf[Array[(Boolean, Boolean)]])
    } else if (classTag == manifest[Double]) {
      spec[Double](arr.asInstanceOf[Array[(Double, Double)]])
    } else if (classTag == manifest[Int]) {
      spec[Int](arr.asInstanceOf[Array[(Int, Int)]])
    } else {
      spec[T](arr)
    }
  }

  private def spec[@specialized U](arr: Array[(U, U)]) = {
    for (i <- 0 until arr.length) {
      arr(i) = arr(i).swap
    }
  }
}
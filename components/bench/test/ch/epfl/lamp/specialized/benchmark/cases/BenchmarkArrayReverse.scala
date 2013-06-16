package ch.epfl.lamp.specialized.benchmark.cases

import scala.util.control.Exception
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class BenchmarkArrayReverse[T](val size: Int)(implicit classTag: ClassTag[T]) extends BenchmarkApi {
  val arr = new Array[T](size)

  /**
   * Algorithm that reverses an array
   * Tests get and update operations of an array
   */
  def test = {
    for (i <- 0 until arr.length / 2) {
      val temp = arr(arr.length - i - 1)
      arr(arr.length - i - 1) = arr(i)
      arr(i) = temp
    }
  }

  def testSpecializedBlock = {
    specialized[T] {
      for (i <- 0 until arr.length / 2) {
        val temp = arr(arr.length - i - 1)
        arr(arr.length - i - 1) = arr(i)
        arr(i) = temp
      }
    }
  }

  def testUnrolled = {
    if (classTag == ManifestFactory.Boolean) {
      val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
      for (i <- 0 until spec_arr.length / 2) {
        val temp: Boolean = spec_arr(spec_arr.length - i - 1)
        spec_arr(spec_arr.length - i - 1) = spec_arr(i)
        spec_arr(i) = temp
      }
    } else if (classTag == ManifestFactory.Int) {
      val spec_arr: Array[Int] = arr.asInstanceOf[Array[Int]]
      for (i <- 0 until spec_arr.length / 2) {
        val temp: Int = spec_arr(spec_arr.length - i - 1)
        spec_arr(spec_arr.length - i - 1) = spec_arr(i)
        spec_arr(i) = temp
      }
    } else if (classTag == ManifestFactory.Double) {
      val spec_arr: Array[Double] = arr.asInstanceOf[Array[Double]]
      for (i <- 0 until spec_arr.length / 2) {
        val temp: Double = spec_arr(spec_arr.length - i - 1)
        spec_arr(spec_arr.length - i - 1) = spec_arr(i)
        spec_arr(i) = temp
      }
    } else {
      val spec_arr: Array[T] = arr
      for (i <- 0 until spec_arr.length / 2) {
        val temp: T = spec_arr(spec_arr.length - i - 1)
        spec_arr(spec_arr.length - i - 1) = spec_arr(i)
        spec_arr(i) = temp
      }
    }
  }

  def testSpecialized = {
    if (classTag == ManifestFactory.Boolean) {
      spec[Boolean](arr.asInstanceOf[Array[Boolean]])
    } else if (classTag == ManifestFactory.Int) {
      spec[Int](arr.asInstanceOf[Array[Int]])
    } else if (classTag == ManifestFactory.Double) {
      spec[Double](arr.asInstanceOf[Array[Double]])
    } else {
      spec[T](arr)
    }
  }

  private def spec[@specialized U](arr: Array[U]) = {
    for (i <- 0 until arr.length / 2) {
      val temp = arr(arr.length - i - 1)
      arr(arr.length - i - 1) = arr(i)
      arr(i) = temp
    }
  }
}

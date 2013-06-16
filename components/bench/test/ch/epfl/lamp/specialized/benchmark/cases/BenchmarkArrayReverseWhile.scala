package ch.epfl.lamp.specialized.benchmark.cases

import scala.util.control.Exception
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class BenchmarkArrayReverseWhile[T](val size: Int)(implicit classTag: ClassTag[T]) extends BenchmarkApi {
  val arr = new Array[T](size)

  /**
   * Algorithm that reverses an array
   * Tests get and update operations of an array
   */
  def test = {
    var i = 0
    while (i < arr.length / 2) {
      val temp = arr(arr.length - i - 1)
      arr(arr.length - i - 1) = arr(i)
      arr(i) = temp
      i += 1
    }
  }

  def testSpecializedBlock = {
    specialized[T] {
      var i = 0
      while (i < arr.length / 2) {
        val temp = arr(arr.length - i - 1)
        arr(arr.length - i - 1) = arr(i)
        arr(i) = temp
        i += 1
      }
    }
  }

  def testUnrolled = {
    if (classTag == ManifestFactory.Boolean) {
      val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
      var i = 0
      while (i < spec_arr.length / 2) {
        val temp = spec_arr(spec_arr.length - i - 1)
        spec_arr(spec_arr.length - i - 1) = spec_arr(i)
        spec_arr(i) = temp
        i += 1
      }
    } else if (classTag == ManifestFactory.Int) {
      val spec_arr: Array[Int] = arr.asInstanceOf[Array[Int]]
      var i = 0
      while (i < spec_arr.length / 2) {
        val temp = spec_arr(spec_arr.length - i - 1)
        spec_arr(spec_arr.length - i - 1) = spec_arr(i)
        spec_arr(i) = temp
        i += 1
      }
    } else if (classTag == ManifestFactory.Double) {
      val spec_arr: Array[Double] = arr.asInstanceOf[Array[Double]]
      var i = 0
      while (i < spec_arr.length / 2) {
        val temp = spec_arr(spec_arr.length - i - 1)
        spec_arr(spec_arr.length - i - 1) = spec_arr(i)
        spec_arr(i) = temp
        i += 1
      }
    } else {
      val spec_arr: Array[T] = arr
      var i = 0
      while (i < spec_arr.length / 2) {
        val temp = spec_arr(spec_arr.length - i - 1)
        spec_arr(spec_arr.length - i - 1) = spec_arr(i)
        spec_arr(i) = temp
        i += 1
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
    var i = 0
    while (i < arr.length / 2) {
      val temp = arr(arr.length - i - 1)
      arr(arr.length - i - 1) = arr(i)
      arr(i) = temp
      i += 1
    }
  }
}

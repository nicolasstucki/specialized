package ch.epfl.lamp.specialized.benchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class TestArrayDuplicate[T](val size: Int)(implicit classTag: ClassTag[T]) extends TestApi {
  val arr = new Array[T](size)

  def test: Array[T] = {
    val arr2 = new Array[T](size)
    for (i <- 0 until size) {
      arr2(i) = arr(i)
    }
    return arr2
  }

  def testSpecializedBlock: Array[T] = {
    specialized[T] {
      val arr2 = new Array[T](size)
      for (i <- 0 until size) {
        arr2(i) = arr(i)
      }
      arr2
    }
  }

  def testUnrolled: Array[T] = {
    {
      if (classTag == ManifestFactory.Boolean) {
        val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
        val arr2 = new Array[Boolean](size)
        for (i <- 0 until size) {
          arr2(i) = spec_arr(i)
        }
        arr2
      } else if (classTag == ManifestFactory.Double) {
        val spec_arr: Array[Double] = arr.asInstanceOf[Array[Double]]
        val arr2 = new Array[Double](size)
        for (i <- 0 until size) {
          arr2(i) = spec_arr(i)
        }
        arr2
      } else if (classTag == ManifestFactory.Int) {
        val spec_arr: Array[Int] = arr.asInstanceOf[Array[Int]]
        val arr2 = new Array[Int](size)
        for (i <- 0 until size) {
          arr2(i) = spec_arr.asInstanceOf[Array[Int]](i)
        }
        arr2
      } else {
        val arr2 = new Array[T](size)
        for (i <- 0 until arr.length) {
          arr2(i) = arr(i)
        }
        arr2
      }
    }.asInstanceOf[Array[T]]
  }

  def testSpecialized = {
    {
      if (classTag == ManifestFactory.Boolean) {
        spec[Boolean](arr.asInstanceOf[Array[Boolean]])
      } else if (classTag == ManifestFactory.Double) {
        spec[Double](arr.asInstanceOf[Array[Double]])
      } else if (classTag == ManifestFactory.Int) {
        spec[Int](arr.asInstanceOf[Array[Int]])
      } else {
        spec[T](arr)
      }
    }.asInstanceOf[Array[T]]
  }

  private[this] def spec[@specialized U](arr: Array[U])(implicit ct: ClassTag[U]): Array[U] = {
    val arr2 = new Array[U](size)
    for (i <- 0 until size) {
      arr2(i) = arr(i)
    }
    arr2
  }
}
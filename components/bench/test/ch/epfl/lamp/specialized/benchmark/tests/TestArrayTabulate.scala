package ch.epfl.lamp.specialized.benchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class TestArrayTabulate[T](val size: Int)(val func: Int => T)(implicit classTag: ClassTag[T]) extends TestApi {
  val arr = new Array[T](size)

  def test = {
    for (i <- 0 until arr.length) {
      arr(i) = func(i)
    }
  }

  def testSpecializedBlock = {
    specialized[T] {
      for (i <- 0 until arr.length) {
        arr(i) = func(i)
      }
    }
  }

  def testUnrolled = {
    if (classTag == ManifestFactory.Boolean) {
      val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
      val spec_func = func.asInstanceOf[Function1[Int, Boolean]]
      for (i <- 0 until spec_arr.length) {
        spec_arr(i) = spec_func(i)
      }
    } else if (classTag == ManifestFactory.Double) {
      val spec_arr = arr.asInstanceOf[Array[Double]]
      val spec_func = func.asInstanceOf[Function1[Int, Double]]
      for (i <- 0 until spec_arr.length) {
        spec_arr(i) = spec_func(i)
      }
    } else if (classTag == ManifestFactory.Int) {
      val spec_arr = arr.asInstanceOf[Array[Int]]
      val spec_func = func.asInstanceOf[Function1[Int, Int]]
      for (i <- 0 until spec_arr.length) {
        spec_arr(i) = spec_func(i)
      }
    } else {
      for (i <- 0 until arr.length) {
        arr(i) = func(i)
      }
    }
  }

  def testSpecialized = {
    if (classTag == ManifestFactory.Boolean) {
      spec[Boolean](arr.asInstanceOf[Array[Boolean]], func.asInstanceOf[Int => Boolean])
    } else if (classTag == ManifestFactory.Double) {
      spec[Double](arr.asInstanceOf[Array[Double]], func.asInstanceOf[Int => Double])
    } else if (classTag == ManifestFactory.Int) {
      spec[Int](arr.asInstanceOf[Array[Int]], func.asInstanceOf[Int => Int])
    } else {
      spec[T](arr, func)
    }
  }

  private def spec[@specialized U](arr: Array[U], func: Int => U) = {
    for (i <- 0 until arr.length) {
      arr(i) = func(i)
    }
  }
}
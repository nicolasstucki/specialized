package ch.epfl.lamp.specialized.benchmark.tests

import scala.util.control.Exception
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class TestArrayMapOverFunction[T](val size: Int)(val func: T => T)(implicit classTag: ClassTag[T]) extends TestApi {
  val arr = new Array[T](size)

  def test = {
    for (i <- 1 until arr.length) {
      arr(i) = func(arr(i))
    }
  }

  def testSpecializedBlock = {
    specialized[T] {
      for (i <- 1 until arr.length) {
        arr(i) = func(arr(i))
      }
    }
  }

  def testUnrolled = {
    if (classTag == ManifestFactory.Boolean) {
      val spec_arr: Array[Boolean] = arr.asInstanceOf[Array[Boolean]]
      val spec_func = func.asInstanceOf[Function1[Boolean, Boolean]]
      for (i <- 1 until spec_arr.length) {
        spec_arr(i) = spec_func(spec_arr(i))
      }
    } else if (classTag == ManifestFactory.Double) {
      val spec_arr = arr.asInstanceOf[Array[Double]]
      val spec_func = func.asInstanceOf[Function1[Double, Double]]
      for (i <- 1 until spec_arr.length) {
        spec_arr(i) = spec_func(spec_arr(i))
      }
    } else if (classTag == ManifestFactory.Int) {
      val spec_arr = arr.asInstanceOf[Array[Int]]
      val spec_func = func.asInstanceOf[Function1[Int, Int]]
      for (i <- 1 until spec_arr.length) {
        spec_arr(i) = spec_func(spec_arr(i))
      }
    } else {
      for (i <- 1 until arr.length) {
        arr(i) = func(arr(i))
      }
    }
  }

  def testSpecialized = {
    if (classTag == ManifestFactory.Boolean) {
      spec[Boolean](arr.asInstanceOf[Array[Boolean]], func.asInstanceOf[Boolean => Boolean])
    } else if (classTag == ManifestFactory.Double) {
      spec[Double](arr.asInstanceOf[Array[Double]], func.asInstanceOf[Double => Double])
    } else if (classTag == ManifestFactory.Int) {
      spec[Int](arr.asInstanceOf[Array[Int]], func.asInstanceOf[Int => Int])
    } else {
      spec[T](arr, func)
    }
  }

  private def spec[@specialized U](arr: Array[U], func: U => U) = {
    for (i <- 1 until arr.length) {
      arr(i) = func(arr(i))
    }
  }
}
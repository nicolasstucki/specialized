package ch.epfl.lamp.specialized.benchmark.cases

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class BenchmarkTuplesSwap[T](val times: Int)(tup: (T, T))(implicit classTag: ClassTag[T]) extends BenchmarkApi {

  /** Algorithm that inverses the order of all tuples in an array of tuples
    * Tests get and update operations of an array
    * and tests swap operation of tuples
    */
  def test = {
    for (i <- 0 until times) {
      val a = tup.swap._1
    }
  }

  def testSpecializedBlock = {
    specialized[T] {
      for (i <- 0 until times) {
        val a = tup.swap._1
      }
    }
  }

  def testUnrolled = {
    if (classTag == ManifestFactory.Boolean) {
      val tup_spec: (Boolean, Boolean) = tup.asInstanceOf[(Boolean, Boolean)]
      for (i <- 0 until times) {
        val a = tup_spec.swap._1
      }
    } else if (classTag == ManifestFactory.Double) {
      val tup_spec: (Double, Double) = tup.asInstanceOf[(Double, Double)]
      for (i <- 0 until times) {
        val a = tup_spec.swap._1
      }
    } else if (classTag == ManifestFactory.Int) {
      val tup_spec: (Int, Int) = tup.asInstanceOf[(Int, Int)]
      for (i <- 0 until times) {
        val a = tup_spec.swap._1
      }
    } else {
      val tup_spec = tup
      for (i <- 0 until times) {
        val a = tup_spec.swap._1
      }
    }
  }

  def testSpecialized = {
    if (classTag == manifest[Boolean]) {
      spec[Boolean](tup.asInstanceOf[(Boolean, Boolean)])
    } else if (classTag == manifest[Double]) {
      spec[Double](tup.asInstanceOf[(Double, Double)])
    } else if (classTag == manifest[Int]) {
      spec[Int](tup.asInstanceOf[(Int, Int)])
    } else {
      spec[T](tup)
    }
  }

  private def spec[@specialized U](tup: (U, U)) = {
    for (i <- 0 until times) {
      val a = tup.swap._1
    }
  }
}
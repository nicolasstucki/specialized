package ch.epfl.lamp.specialized.benchmark.cases

import scala.util.control.Exception
import scala.reflect.ClassTag
import scala.annotation.tailrec
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class BenchmarkFunctionApplyNTimesRec[T](val times: Int)(val init: T, val func: T => T)(implicit classTag: ClassTag[T]) extends BenchmarkApi {

  def test = {
    @tailrec def rec(n: Int, last: T): T = {
      if (n == 0) last
      else rec(n - 1, func(last))
    }
    rec(times, init)
  }

  def testSpecializedBlock = {
    specialized[T] {
      @tailrec def rec(n: Int, last: T): T = {
        if (n == 0) last
        else rec(n - 1, func(last))
      }
      rec(times, init)
    }
  }

  def testUnrolled = {
    {
      if (classTag == ManifestFactory.Boolean) {
        val spec_init = init.asInstanceOf[Boolean]
        val spec_func = func.asInstanceOf[Function1[Boolean, Boolean]]
        @tailrec def rec(n: Int, last: Boolean): Boolean = {
          if (n == 0) last
          else rec(n - 1, spec_func(last))
        }
        rec(times, spec_init)
      } else if (classTag == ManifestFactory.Double) {
        val spec_init = init.asInstanceOf[Double]
        val spec_func = func.asInstanceOf[Function1[Double, Double]]
        @tailrec def rec(n: Int, last: Double): Double = {
          if (n == 0) last
          else rec(n - 1, spec_func(last))
        }
        rec(times, spec_init)
      } else if (classTag == ManifestFactory.Int) {
        val spec_init = init.asInstanceOf[Int]
        val spec_func = func.asInstanceOf[Function1[Int, Int]]
        @tailrec def rec(n: Int, last: Int): Int = {
          if (n == 0) last
          else rec(n - 1, spec_func(last))
        }
        rec(times, spec_init)
      } else {
        @tailrec def rec(n: Int, last: T): T = {
          if (n == 0) last
          else rec(n - 1, func(last))
        }
        rec(times, init)
      }
    }.asInstanceOf[T]
  }

  def testSpecialized = {
    {
      if (classTag == ManifestFactory.Boolean) {
        spec[Boolean](init.asInstanceOf[Boolean], func.asInstanceOf[Function1[Boolean, Boolean]])
      } else if (classTag == ManifestFactory.Double) {
        spec[Double](init.asInstanceOf[Double], func.asInstanceOf[Function1[Double, Double]])
      } else if (classTag == ManifestFactory.Int) {
        spec[Int](init.asInstanceOf[Int], func.asInstanceOf[Function1[Int, Int]])
      } else {
        spec[T](init, func)
      }
    }.asInstanceOf[T]
  }

  private def spec[@specialized U](init: U, func: U => U) = {
    @tailrec def rec(n: Int, last: U): U = {
      if (n == 0) last
      else rec(n - 1, func(last))
    }
    rec(times, init)
  }

}
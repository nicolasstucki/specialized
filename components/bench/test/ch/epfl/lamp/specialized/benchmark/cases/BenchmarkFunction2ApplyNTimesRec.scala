package ch.epfl.lamp.specialized.benchmark.cases

import scala.util.control.Exception
import scala.reflect.ClassTag
import scala.annotation.tailrec
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class BenchmarkFunction2ApplyNTimesRec[T](val times: Int)(val init0: T, val init1: T, val func: (T, T) => T)(implicit classTag: ClassTag[T]) extends BenchmarkApi {

  def test = {
    @tailrec def rec(n: Int, beforelast: T, last: T): T = {
      if (n == 0) last
      else rec(n - 1, last, func(beforelast, last))
    }
    rec(times, init0, init1)
  }

  def testSpecializedBlock = {
    specialized[T] {
      @tailrec def rec(n: Int, beforelast: T, last: T): T = {
        if (n == 0) last
        else rec(n - 1, last, func(beforelast, last))
      }
      rec(times, init0, init1)
    }
  }

  def testUnrolled = {
    {
      if (classTag == ManifestFactory.Boolean) {
        val spec_init0 = init0.asInstanceOf[Boolean]
        val spec_init1 = init1.asInstanceOf[Boolean]
        val spec_func = func.asInstanceOf[Function2[Boolean, Boolean, Boolean]]
        @tailrec def rec(n: Int, beforelast: Boolean, last: Boolean): Boolean = {
          if (n == 0) last
          else rec(n - 1, last, spec_func(beforelast, last))
        }
        rec(times, spec_init0, spec_init1)
      } else if (classTag == ManifestFactory.Double) {
        val spec_init0 = init0.asInstanceOf[Double]
        val spec_init1 = init1.asInstanceOf[Double]
        val spec_func = func.asInstanceOf[Function2[Double, Double, Double]]
        @tailrec def rec(n: Int, beforelast: Double, last: Double): Double = {
          if (n == 0) last
          else rec(n - 1, last, spec_func(beforelast, last))
        }
        rec(times, spec_init0, spec_init1)
      } else if (classTag == ManifestFactory.Int) {
        val spec_init0 = init0.asInstanceOf[Int]
        val spec_init1 = init1.asInstanceOf[Int]
        val spec_func = func.asInstanceOf[Function2[Int, Int, Int]]
        @tailrec def rec(n: Int, beforelast: Int, last: Int): Int = {
          if (n == 0) last
          else rec(n - 1, last, spec_func(beforelast, last))
        }
        rec(times, spec_init0, spec_init1)
      } else {
        @tailrec def rec(n: Int, beforelast: T, last: T): T = {
          if (n == 0) last
          else rec(n - 1, last, func(beforelast, last))
        }
        rec(times, init0, init1)
      }
    }.asInstanceOf[T]
  }

  def testSpecialized = {
    {
      if (classTag == ManifestFactory.Boolean) {
        spec[Boolean](init0.asInstanceOf[Boolean], init1.asInstanceOf[Boolean], func.asInstanceOf[Function2[Boolean, Boolean, Boolean]])
      } else if (classTag == ManifestFactory.Double) {
        spec[Double](init0.asInstanceOf[Double], init1.asInstanceOf[Double], func.asInstanceOf[Function2[Double, Double, Double]])
      } else if (classTag == ManifestFactory.Int) {
        spec[Int](init0.asInstanceOf[Int], init1.asInstanceOf[Int], func.asInstanceOf[Function2[Int, Int, Int]])
      } else {
        spec[T](init0, init1, func)
      }
    }.asInstanceOf[T]
  }

  private[this] def spec[@specialized U](init0: U, init1: U, func: (U, U) => U) = {
    @tailrec def rec(n: Int, beforelast: U, last: U): U = {
      if (n == 0) last
      else rec(n - 1, last, func(beforelast, last))
    }
    rec(times, init0, init1)
  }

}
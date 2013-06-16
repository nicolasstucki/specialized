package ch.epfl.lamp.specialized.benchmark.cases

import scala.util.control.Exception
import scala.reflect.ClassTag
import scala.annotation.tailrec
import ch.epfl.lamp.specialized._
import scala.reflect.ManifestFactory

class BenchmarkFunctionApplyNTimesRecOverTuples[T](val times: Int)(val init: (T, T), val func: T => T)(implicit classTag: ClassTag[T]) extends BenchmarkApi {

  def test = {
    @tailrec def rec(n: Int, last: (T, T)): (T, T) = {
      if (n == 0) last
      else rec(n - 1, (func(last._1), func(last._2)))
    }
    rec(times, init)
  }

  def testSpecializedBlock = {
//    specialized[T] {
//      @tailrec def rec(n: Int, last: (T, T)): (T, T) = {
//        if (n == 0) last
//        else rec(n - 1, (func(last._1), func(last._2)))
//      }
//      rec(times, init)
//    }
  }

  def testUnrolled = {
    {
      if (classTag == ManifestFactory.Boolean) {
        val spec_init = init.asInstanceOf[(Boolean, Boolean)]
        val spec_func = func.asInstanceOf[Boolean => Boolean]
        @tailrec def rec(n: Int, last: (Boolean, Boolean)): (Boolean, Boolean) = {
          if (n == 0) last
          else rec(n - 1, (spec_func(last._1), spec_func(last._2)))
        }
        rec(times, spec_init)
      } else if (classTag == ManifestFactory.Double) {
        val spec_init = init.asInstanceOf[(Double, Double)]
        val spec_func = func.asInstanceOf[Double => Double]
        @tailrec def rec(n: Int, last: (Double, Double)): (Double, Double) = {
          if (n == 0) last
          else rec(n - 1, (spec_func(last._1), spec_func(last._2)))
        }
        rec(times, spec_init)
      } else if (classTag == ManifestFactory.Int) {
        val spec_init = init.asInstanceOf[(Int, Int)]
        val spec_func = func.asInstanceOf[Int => Int]
        @tailrec def rec(n: Int, last: (Int, Int)): (Int, Int) = {
          if (n == 0) last
          else rec(n - 1, (spec_func(last._1), spec_func(last._2)))
        }
        rec(times, spec_init)
      } else {
        @tailrec def rec(n: Int, last: (T, T)): (T, T) = {
          if (n == 0) last
          else rec(n - 1, (func(last._1), func(last._2)))
        }
        rec(times, init)
      }
    }.asInstanceOf[(T, T)]
  }

  def testSpecialized = {
    {
      if (classTag == ManifestFactory.Boolean) {
        spec[Boolean](init.asInstanceOf[(Boolean, Boolean)], func.asInstanceOf[Boolean => Boolean])
      } else if (classTag == ManifestFactory.Double) {
        spec[Double](init.asInstanceOf[(Double, Double)], func.asInstanceOf[Double => Double])
      } else if (classTag == ManifestFactory.Int) {
        spec[Int](init.asInstanceOf[(Int, Int)], func.asInstanceOf[Int => Int])
      } else {
        spec[T](init, func)
      }
    }.asInstanceOf[(T, T)]
  }

  private def spec[@specialized U](init: (U, U), func: U => U) = {
    @tailrec def rec(n: Int, last: (U, U)): (U, U) = {
      if (n == 0) last
      else rec(n - 1, (func(last._1), func(last._2)))
    }
    rec(times, init)
  }

}
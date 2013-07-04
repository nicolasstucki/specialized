package ch.epfl.lamp.specialized.benchmark.tests

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

// Class used for quick testing of new changes
class QuickTest[T, U](p: T)(implicit classTag: ClassTag[T]) {

  val times = 10
  val c = p
  var d = p
  def testMethod = {
//        val tup = (p, p)
//        specialized[T] {
//          val (a, b) = tup.swap
//        }
    val a = p
    specialized[T] {
      var b = a
      println(a)
      println(b)
      println(c)
      println(d)
    }
  }

}

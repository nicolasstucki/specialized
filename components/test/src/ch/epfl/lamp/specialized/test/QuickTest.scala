package ch.epfl.lamp.specialized.benchmark.tests

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

// Class used for quick testing of new changes
class QuickTest[T, U](tup: (T, T))(implicit classTag: ClassTag[T]) {

  val times = 10

  def testMethod = {
//    specialized[T] {
//      val (a, b) = tup.swap
//    }
  }

}

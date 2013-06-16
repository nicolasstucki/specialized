package ch.epfl.lamp.specialized.benchmark.tests

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

// Class used for quick testing of new changes
class QuickTest[T](array: Array[T])(implicit classTag: ClassTag[T]) {

  def testMethod = {
    specialized[T](Int, Double) {
      var i = 0
      while (i < array.length / 2) {
        val temp = array(array.length - i - 1)
        array(array.length - i - 1) = array(i)
        array(i) = temp
        i += 1
      }
    }
  }
}
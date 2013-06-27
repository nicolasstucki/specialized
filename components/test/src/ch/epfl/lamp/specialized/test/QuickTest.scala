package ch.epfl.lamp.specialized.benchmark.tests

import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

// Class used for quick testing of new changes
class QuickTest[T, U](list: List[T])(implicit classTag: ClassTag[T]) {

  def testMethod = {
    //    specialized[T] {
    list.headOption match {
      case Some(head) => head
      case None => null
    }
    //    }
  }

}

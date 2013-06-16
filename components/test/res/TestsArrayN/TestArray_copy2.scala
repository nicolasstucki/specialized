import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray[T](arrA: Array[T])(implicit classTag: ClassTag[T]) {
  val arrB = new Array[T](arrA.length)

  def copy2 = {
    specialized[T](Int, Double, Boolean) {
      var i = 0
      while (i < arrA.length) {
        arrB(i) = arrA(i)
        i += 1
      }
    }
  }
}
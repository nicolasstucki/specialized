import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray[T](arr: Array[T])(implicit classTag: ClassTag[T]) {

  def reverse2 = {
    specialized[T](Int, Double, Boolean) {
      var i = 0
      while (i < arr.length / 2) {
        val j = arr.length - i - 1
        val temp = arr(j)
        arr(j) = arr(i)
        arr(i) = temp
        i += 1
      }
    }
  }
}
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

class TestArray[T](arr: Array[T])(implicit classTag: ClassTag[T]) {

  def reverse = {
    specialized[T](Int, Double, Boolean) {
      for (i <- 0 to arr.length / 2) {
        val j = arr.length - i - 1
        val temp = arr(j)
        arr(j) = arr(i)
        arr(i) = temp
      }
    }
  }
}
import scala.reflect.ClassTag
import ch.epfl.lamp.specialized._

object MatrixUtil {
  type Matrix[K] = Array[Array[K]]

  def power[T: ClassTag](A: Matrix[T], n: Int)(implicit numeric: Numeric[T]): Matrix[T] = {
    if (n % 2 == 0) {
      val B = power(A, n / 2)
      square(B)
    } else {
      matrixMult(power(A, n - 1), A)
    }
  }

  def square[T: ClassTag](A: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] = {
    matrixMult(A, A)
  }

  def matrixMult[T: ClassTag](A: Matrix[T], B: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] = {
    val C = Array.ofDim[T](A.length, B(0).length)
    specialized[T] {
      _matrixMult(A, B, C)
    }
    C
  }

  private def _matrixMult[@specialized T](A: Matrix[T], B: Matrix[T], C: Matrix[T])(implicit numeric: Numeric[T]) {
    import numeric._
    val I = A.length; val J = B.length; val K = C(0).length
    for (i <- 0 until I) {
      for (k <- 0 until K) {
        C(i)(k) = zero
        for (j <- 0 until J) {
          C(i)(k) = plus(C(i)(k), times(C(i)(j), B(j)(k)))
        }
      }
    }
  }
}